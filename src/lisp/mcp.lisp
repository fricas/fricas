(in-package :boot)

;; Ensure yason is loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (asdf:load-system :yason)
  #+sbcl (require :sb-bsd-sockets))

(defpackage :fricas-mcp
  (:use :cl :boot)
  (:export #:start-mcp-server #:stop-mcp-server #:check-mcp-args #:start-socket-mcp-server #:start-socket-mcp-client))

(in-package :fricas-mcp)

(defvar *mcp-thread* nil)
(defvar *mcp-running* nil)
(defvar *mcp-json-stream* nil)
(defvar *mcp-output-stream* nil)
(defvar *mcp-socket* nil)
(defvar *mcp-use-content-length* nil
  "When T, use LSP-style Content-Length headers (socket/LSP mode).
   When NIL, use newline-delimited JSON (MCP stdio mode).")

(defvar *mcp-plots* nil
  "List of plots sent to the client. Each item is (timestamp kind data).")

(defvar *fricas-eval-lock*
  #+openmcl (ccl:make-lock "FriCAS Evaluation Lock")
  #+sbcl (sb-thread:make-mutex :name "FriCAS Evaluation Lock")
  #-(or openmcl sbcl) nil)

(defvar *fricas-db-lock*
  #+openmcl (ccl:make-lock "FriCAS Database Lock")
  #+sbcl (sb-thread:make-mutex :name "FriCAS Database Lock")
  #-(or openmcl sbcl) nil)

(defun patch-fricas-database-read ()
  "Patch FriCAS database read function to be thread-safe."
  (let ((get-data-sym (find-symbol "|get_data_from_file|" "BOOT"))
        (db-lock *fricas-db-lock*))
    (when (and get-data-sym (fboundp get-data-sym) (not (get get-data-sym :patched-by-mcp)))
      (let ((old-get-data (symbol-function get-data-sym)))
        (setf (symbol-function get-data-sym)
              (lambda (con key data stream)
                #+openmcl
                (ccl:with-lock-grabbed (db-lock)
                  (funcall old-get-data con key data stream))
                #+sbcl
                (sb-thread:with-mutex (db-lock)
                  (funcall old-get-data con key data stream))
                #-(or openmcl sbcl)
                (funcall old-get-data con key data stream)))
        (setf (get get-data-sym :patched-by-mcp) t)))))

(defvar *mcp-output-lock*
  #+openmcl (ccl:make-lock "MCP Output Lock")
  #+sbcl (sb-thread:make-mutex :name "MCP Output Lock")
  #-(or openmcl sbcl) nil)

(defvar *mcp-input-lock*
  #+openmcl (ccl:make-lock "MCP Input Lock")
  #+sbcl (sb-thread:make-mutex :name "MCP Input Lock")
  #-(or openmcl sbcl) nil)

(defvar *fricas-databases-shared* nil)

(defun send-json (data &optional stream)
  "Encode data as JSON and send.
    When *mcp-use-content-length* is T, uses LSP-style Content-Length headers.
    When NIL, uses newline-delimited JSON (MCP stdio transport).
    Supports both binary (unsigned-byte 8) and text output streams.
    NOTE: CCL (Clozure CL) bivalent socket streams report 'character' as their
    element-type. For those streams we use character-mode Content-Length framing
    with the length reported as char count (equals byte count for ASCII-only JSON)."
  (let* ((out (or stream *mcp-output-stream* *mcp-json-stream* *standard-output*))
         (json-str (with-output-to-string (s)
                     (yason:encode data s)))
         (octets
           #+sbcl (sb-ext:string-to-octets json-str :external-format :utf-8)
           #+openmcl (ccl:encode-string-to-octets json-str :external-format :utf-8)
           #-(or sbcl openmcl) (map '(vector (unsigned-byte 8)) #'char-code json-str))
         (octet-len (length octets))
         (char-len (length json-str))
         (is-binary (and (streamp out)
                         (subtypep (stream-element-type out) '(unsigned-byte 8))))
         ;; Content-Length: byte count for SBCL binary streams,
         ;; char count for CCL bivalent/text streams (chars == bytes for ASCII JSON).
         (len (if is-binary octet-len char-len)))
    (flet ((do-send ()
             (handler-case
                 (if *mcp-use-content-length*
                     ;; LSP-style Content-Length framing (socket mode)
                     (if is-binary
                         (let ((header-octets
                                 #+sbcl (sb-ext:string-to-octets
                                         (format nil "Content-Length: ~A~C~C~C~C" len #\Return #\Newline #\Return #\Newline)
                                         :external-format :ascii)
                                 #+openmcl (ccl:encode-string-to-octets
                                            (format nil "Content-Length: ~A~C~C~C~C" len #\Return #\Newline #\Return #\Newline)
                                            :external-format :ascii)
                                 #-(or sbcl openmcl)
                                 (map '(vector (unsigned-byte 8)) #'char-code
                                      (format nil "Content-Length: ~A~C~C~C~C" len #\Return #\Newline #\Return #\Newline))))
                           (write-sequence header-octets out)
                           (write-sequence octets out)
                           (force-output out))
                         (progn
                           (format out "Content-Length: ~A~C~C~C~C" len #\Return #\Newline #\Return #\Newline)
                           (write-string json-str out)
                           (force-output out)))
                     ;; Newline-delimited JSON (MCP stdio transport)
                     (if is-binary
                         (let ((nl-octets
                                 #+sbcl (sb-ext:string-to-octets
                                         (format nil "~A~%" json-str)
                                         :external-format :utf-8)
                                 #+openmcl (ccl:encode-string-to-octets
                                            (format nil "~A~%" json-str)
                                            :external-format :utf-8)
                                 #-(or sbcl openmcl)
                                 (map '(vector (unsigned-byte 8)) #'char-code
                                      (format nil "~A~%" json-str))))
                           (write-sequence nl-octets out)
                           (force-output out))
                         (progn
                           (write-string json-str out)
                           (terpri out)
                           (force-output out))))
               (error (c)
                 (format *error-output* "Failed to send JSON: ~A~%" c)))))
      #+openmcl
      (if (and *mcp-output-lock* (not (eq out *standard-output*)))
          (ccl:with-lock-grabbed (*mcp-output-lock*)
            (do-send))
          (do-send))
      #+sbcl
      (if (and *mcp-output-lock* (not (eq out *standard-output*)))
          (sb-thread:with-mutex (*mcp-output-lock*)
            (do-send))
          (do-send))
      #-(or openmcl sbcl)
      (do-send))))

(defun send-notification (method params)
  (let ((msg (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" msg) "2.0")
    (setf (gethash "method" msg) method)
    (setf (gethash "params" msg) params)
    (send-json msg)))

(defun send-error (id code message &optional data)
  (let ((resp (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" resp) "2.0")
    (setf (gethash "id" resp) id)
    (let ((err (make-hash-table :test 'equal)))
      (setf (gethash "code" err) code)
      (setf (gethash "message" err) message)
      (when data (setf (gethash "data" err) data))
      (setf (gethash "error" resp) err))
    (send-json resp)))

(defun send-result (id result)
  (let ((resp (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" resp) "2.0")
    (setf (gethash "id" resp) id)
    (setf (gethash "result" resp) result)
    (send-json resp)))

(defun |sendDisplay| (kind data)
  "Send a 'display' notification to the MCP client (VS Code).
   Recognized kinds include 'image/svg+xml', 'image/png', etc."
  (push (list (get-universal-time) kind data) *mcp-plots*)
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "kind" params) kind)
    (setf (gethash "data" params) data)
    (send-notification "display" params)))

(defun handle-initialize (id params)
  (declare (ignore params))
  (let ((result (make-hash-table :test 'equal)))
    (setf (gethash "protocolVersion" result) "2024-11-05")
    (let ((caps (make-hash-table :test 'equal)))
      (setf (gethash "tools" caps) (make-hash-table))
      (setf (gethash "resources" caps) (make-hash-table))
      (setf (gethash "capabilities" result) caps))
    (let ((info (make-hash-table :test 'equal)))
      (setf (gethash "name" info) "jlFriCAS MCP Server")
      (setf (gethash "version" info) "0.1.0")
      (setf (gethash "serverInfo" result) info))
    (send-result id result)))

(defun handle-list-resources (id)
  (let ((result (make-hash-table :test 'equal))
        (resources nil)
        (index (length *mcp-plots*)))
    (dolist (plot *mcp-plots*)
      (let ((res (make-hash-table :test 'equal))
            (time (decode-universal-time (car plot))))
        (setf (gethash "uri" res) (format nil "plot://~A" index))
        (setf (gethash "name" res) (format nil "Plot ~A (~2,'0D:~2,'0D:~2,'0D)"
                                           index (nth 2 (list time)) (nth 1 (list time)) (nth 0 (list time)))) ;; wait, decode returns multiple values
        ;; Let's simplify naming
        (multiple-value-bind (sec min hour) (decode-universal-time (car plot))
           (setf (gethash "name" res) (format nil "Plot ~A (~2,'0D:~2,'0D:~2,'0D)" index hour min sec)))
        (setf (gethash "mimeType" res) (cadr plot))
        (push res resources)
        (decf index)))
    (setf (gethash "resources" result) resources)
    (send-result id result)))

(defun handle-read-resource (id params)
  (let ((uri (gethash "uri" params))
        (result (make-hash-table :test 'equal)))
    (if (not (and uri (string= (subseq uri 0 7) "plot://")))
        (send-error id -32602 "Invalid or missing URI")
        (let* ((index-str (subseq uri 7))
               (index (ignore-errors (parse-integer index-str)))
               (plot-count (length *mcp-plots*)))
          (if (or (not index) (< index 1) (> index plot-count))
              (send-error id -32000 "Plot not found")
              (let* ((plot (nth (- plot-count index) *mcp-plots*))
                     (kind (cadr plot))
                     (data (caddr plot))
                     (content (make-hash-table :test 'equal)))
                (setf (gethash "uri" content) uri)
                (setf (gethash "mimeType" content) kind)
                (if (string= kind "image/svg+xml")
                    (setf (gethash "text" content) data)
                    (setf (gethash "blob" content) data))
                (setf (gethash "contents" result) (list content))
                (send-result id result)))))))

(defun handle-list-tools (id)
  (let ((result (make-hash-table :test 'equal))
        (tool (make-hash-table :test 'equal))
        (schema (make-hash-table :test 'equal))
        (props (make-hash-table :test 'equal))
        (expr-prop (make-hash-table :test 'equal))
        (format-prop (make-hash-table :test 'equal)))
    (setf (gethash "name" tool) "evaluate")
    (setf (gethash "description" tool) "Evaluate a mathematical expression.")
    (setf (gethash "type" expr-prop) "string")
    (setf (gethash "description" expr-prop) "The mathematical expression to evaluate")
    (setf (gethash "expression" props) expr-prop)
    (setf (gethash "type" format-prop) "string")
    (setf (gethash "description" format-prop) "Output format: 'text' (default) or 'markdown'")
    (setf (gethash "enum" format-prop) '("text" "markdown"))
    (setf (gethash "format" props) format-prop)
    (setf (gethash "type" schema) "object")
    (setf (gethash "properties" schema) props)
    (setf (gethash "required" schema) '("expression"))
    (setf (gethash "inputSchema" tool) schema)

    (let ((doc-tool (make-hash-table :test 'equal))
          (doc-schema (make-hash-table :test 'equal))
          (doc-props (make-hash-table :test 'equal))
          (name-prop (make-hash-table :test 'equal))
          (type-prop (make-hash-table :test 'equal)))
      (setf (gethash "name" doc-tool) "get-documentation")
      (setf (gethash "description" doc-tool) "Get documentation for a FriCAS constructor (Domain, Category, Package) or operation.")
      (setf (gethash "type" name-prop) "string")
      (setf (gethash "description" name-prop) "The name of the constructor or operation")
      (setf (gethash "name" doc-props) name-prop)
      (setf (gethash "type" type-prop) "string")
      (setf (gethash "description" type-prop) "The type of the entity ('constructor' or 'operation')")
      (setf (gethash "enum" type-prop) '("constructor" "operation"))
      (setf (gethash "type" doc-props) type-prop)
      (setf (gethash "type" doc-schema) "object")
      (setf (gethash "properties" doc-schema) doc-props)
      (setf (gethash "required" doc-schema) '("name" "type"))
      (setf (gethash "inputSchema" doc-tool) doc-schema)

      (let ((list-tool (make-hash-table :test 'equal))
            (list-schema (make-hash-table :test 'equal))
            (list-props (make-hash-table :test 'equal))
            (pattern-prop (make-hash-table :test 'equal)))
        (setf (gethash "name" list-tool) "list-constructors")
        (setf (gethash "description" list-tool) "List FriCAS constructors (categories, domains, packages) matching a pattern.")
        (setf (gethash "type" pattern-prop) "string")
        (setf (gethash "description" pattern-prop) "The pattern to search for (e.g., 'AbelianMonoid' or 'Int*')")
        (setf (gethash "pattern" list-props) pattern-prop)
        (setf (gethash "type" list-schema) "object")
        (setf (gethash "properties" list-schema) list-props)
        (setf (gethash "required" list-schema) '("pattern"))
        (setf (gethash "inputSchema" list-tool) list-schema)
        (setf (gethash "tools" result) (list tool doc-tool list-tool)))
      (send-result id result))))

(defun handle-call-tool (id name params)
  (cond
    ((string= name "evaluate")
     (let ((expr (gethash "expression" params))
           (format (gethash "format" params)))
       (if (not expr)
           (send-error id -32602 "Missing expression parameter")
           (progn
             (send-notification "repl/starteval" nil)
             ;; Echo code to terminal REPL
             (format *error-output* "~A~%" expr)
             (finish-output *error-output*)
             (let ((output (evaluate-expr expr t)))
               ;; Echo result to terminal REPL
               (when (and output (not (string= output "")))
                 (format *error-output* "~A~%" output)
                 (finish-output *error-output*))
               (send-notification "repl/finisheval" nil)
               (let ((result (make-hash-table :test 'equal))
                     (content (make-hash-table :test 'equal)))
                 (if (string= format "markdown")
                     (progn
                       (setf (gethash "type" content) "text")
                       (setf (gethash "text" content) (format-fricas-output-as-markdown output)))
                     (progn
                       (setf (gethash "type" content) "text")
                       (setf (gethash "text" content) output)))
                 (setf (gethash "content" result) (list content))
                 (send-result id result)))))))
    ((string= name "get-documentation")
     (let ((entity-name (gethash "name" params))
           (entity-type (gethash "type" params)))
       (if (or (not entity-name) (not entity-type))
           (send-error id -32602 "Missing name or type parameter")
           (if (suppressed-number-p entity-name)
               (let ((result (make-hash-table :test 'equal))
                     (content (make-hash-table :test 'equal)))
                 (setf (gethash "type" content) "text")
                 (setf (gethash "text" content) "")
                 (setf (gethash "content" result) (list content))
                 (send-result id result))
               (let* ((fricas-func (if (string= entity-type "constructor")
                                       "constructorDocumentation"
                                       "operationDocumentation"))
                      (cmd (format nil "~A(\"~A\")$SpadDoc" fricas-func entity-name))
                      (output (capture-fricas-output cmd)))
                 (let ((result (make-hash-table :test 'equal))
                       (content (make-hash-table :test 'equal)))
                   (setf (gethash "type" content) "text")
                   (setf (gethash "text" content) (clean-fricas-doc-output output))
                   (setf (gethash "content" result) (list content))
                   (send-result id result)))))))
    ((string= name "list-constructors")
     (let ((pattern (gethash "pattern" params)))
       (if (not pattern)
           (send-error id -32602 "Missing pattern parameter")
           (let* ((cmd (format nil "listConstructors(\"~A\")$SpadDoc" pattern))
                  (output (capture-fricas-output cmd)))
             (let ((result (make-hash-table :test 'equal))
                   (content (make-hash-table :test 'equal)))
               (setf (gethash "type" content) "text")
               (setf (gethash "text" content) (clean-fricas-doc-output output))
               (setf (gethash "content" result) (list content))
               (send-result id result))))))
    (t (send-error id -32601 (format nil "Tool not found: ~A" name)))))

(defun handle-repl-runcode (id params)
  (let ((code (gethash "code" params))
        (result (make-hash-table :test 'equal)))
    (if (not code)
        (send-error id -32602 "Missing code parameter")
        (progn
          (send-notification "repl/starteval" nil)
          ;; Always echo code to terminal REPL
          (format *error-output* "~A~%" code)
          (finish-output *error-output*)
          (let ((output (evaluate-expr code t)))
            ;; Always echo result to terminal REPL
            (when (and output (not (string= output "")))
              (format *error-output* "~A~%" output)
              (finish-output *error-output*))
            (send-notification "repl/finisheval" nil)
            (setf (gethash "inline" result) (or output ""))
            (setf (gethash "all" result) (or output ""))
            (setf (gethash "stackframe" result) nil)
            (send-result id result))))))

(defun handle-repl-get-doc-from-word (id params)
  (let ((word (gethash "word" params))
        (entity-type (gethash "type" params))
        (result (make-hash-table :test 'equal)))
    (if (not word)
        (send-error id -32602 "Missing word parameter")
        (if (suppressed-number-p word)
            (progn
              (setf (gethash "result" result) "")
              (send-result id ""))
            (let* ((fricas-func (if (equal entity-type "constructor")
                                    "constructorDocumentation"
                                    "operationDocumentation"))
                   (cmd (format nil "~A(\"~A\")$SpadDoc" fricas-func word))
                   (output (capture-fricas-output cmd)))
              (setf (gethash "result" result) (clean-fricas-doc-output output))
              (send-result id (gethash "result" result)))))))

(defun handle-repl-get-doc-at (id params)
  (let ((word (gethash "word" params)))
    (if (not word)
        (send-error id -32602 "Missing word parameter")
        (handle-repl-get-doc-from-word id params))))

(defun suppressed-number-p (s)
  "Return T if S is a FriCAS numeric literal that should not have documentation.
   Excludes '0' and '1' which are allowed."
  (let* ((len (length s))
         (start 0))
    (when (zerop len) (return-from suppressed-number-p nil))
    ;; Handle optional minus
    (when (char= (char s 0) #\-)
      (setf start 1)
      (when (= len 1) (return-from suppressed-number-p nil)))
    (let ((body (subseq s start)))
      ;; Allow "0" and "1" exactly (as decimal integers without minus)
      (when (and (= start 0) (or (string= body "0") (string= body "1")))
        (return-from suppressed-number-p nil))
      ;; Decimal integer
      (when (every #'digit-char-p body) (return-from suppressed-number-p t))
      ;; Radix: \d+r[0-9a-zA-Z]+
      (let ((r-pos (position #\r body)))
        (when (and r-pos (> r-pos 0) (< r-pos (1- (length body))))
          (when (and (every #'digit-char-p (subseq body 0 r-pos))
                     (every (lambda (c) (or (digit-char-p c) (alpha-char-p c))) (subseq body (1+ r-pos))))
            (return-from suppressed-number-p t))))
      ;; Float: contains dot or exponent
      (let ((has-dot (find #\. body))
            (has-exp (or (find #\e body) (find #\E body))))
        (when (or has-dot has-exp)
          (let ((first (char body 0)))
            (when (or (digit-char-p first) (char= first #\.))
              (return-from suppressed-number-p t))))))
    nil))

(defun fricas-split-lines (string)
  (let ((lines nil)
        (start 0))
    (loop for pos = (position #\Newline string :start start)
          while pos
          do (push (subseq string start pos) lines)
             (setf start (1+ pos))
          finally (push (subseq string start) lines))
    (nreverse lines)))

(defun clean-fricas-doc-output (text)
  "Clean up FriCAS output for documentation tools."
  (if (not (stringp text)) (return-from clean-fricas-doc-output ""))
  (let* ((lines (fricas-split-lines text))
         (cleaned-lines (mapcar (lambda (line)
                                  (let ((p (search ")  " line)))
                                    (if (and p (< p 10))
                                        (subseq line (+ p 3))
                                        line)))
                                lines))
         (joined (string-trim '(#\Space #\Newline #\Return)
                              (format nil "~{~A~^~%~}" cleaned-lines))))
    ;; Strip trailing FriCAS types
    (dolist (marker '("Type: String" "Type: Void" "Type: OutputForm"))
      (let ((t-len (length joined))
            (m-len (length marker)))
        (when (and (>= t-len m-len)
                   (string= joined marker :start1 (- t-len m-len)))
          (setf joined (string-trim '(#\Space #\Newline #\Return)
                                    (subseq joined 0 (- t-len m-len)))))))
    joined))

(defun format-fricas-output-as-markdown (output)
  "Format FriCAS output string as Markdown."
  (if (or (null output) (string= output ""))
      ""
      (format nil "```fricas~%~A~%```" output)))


(defun ensure-fricas-databases-shared ()
  "Ensure FriCAS database streams are shared across threads in Clozure CL."
  #+openmcl
  (let ((open-file-streams-sym (find-symbol "OPEN-FILE-STREAMS" "CCL"))
        (stream-ioblock-sym (find-symbol "STREAM-IOBLOCK" "CCL"))
        (ioblock-sharing-sym (find-symbol "IOBLOCK-SHARING" "CCL"))
        (ioblock-owner-sym (find-symbol "IOBLOCK-OWNER" "CCL")))
    (when (and open-file-streams-sym stream-ioblock-sym
               ioblock-sharing-sym ioblock-owner-sym)
      (dolist (s (funcall open-file-streams-sym))
        (when (typep s 'file-stream)
          (let ((pn (ignore-errors (pathname s))))
            (when (and pn
                       (let ((pt (pathname-type pn)))
                         (member pt '("daase" "it" "data" "etc" "lib")
                                 :test #'string-equal)))
              (ignore-errors
                (let ((ib (funcall stream-ioblock-sym s t)))
                  (when ib
                    ;; Set sharing to :lock and owner to nil to allow cross-thread access
                    (funcall (fdefinition `(setf ,ioblock-sharing-sym)) :lock ib)
                    (funcall (fdefinition `(setf ,ioblock-owner-sym)) nil ib))))))))
      (setf *fricas-databases-shared* t)))
  t)

(defun fricas-statement-line-p (line)
  "Return T if LINE is a non-empty, non-comment FriCAS statement (or continuation)."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (and (> (length trimmed) 0)
         (not (and (>= (length trimmed) 2)
                   (string= (subseq trimmed 0 2) "--"))))))

(defun split-fricas-statements (expr)
  "Split a multi-line FriCAS input string into individual statement strings.
   Lines that begin with whitespace are treated as continuations of the
   preceding statement. Blank lines and -- comments are skipped.
   Returns a list of trimmed statement strings."
  (let ((lines (fricas-split-lines expr))
        (statements nil)
        (current nil))
    (dolist (line lines)
      (let ((content (string-right-trim '(#\Space #\Tab #\Return) line)))
        (cond
          ;; Blank line: flush current statement
          ((string= (string-trim '(#\Space #\Tab #\Return) content) "")
           (when current
             (push (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (format nil "~{~A~^~%~}" (nreverse current)))
                   statements)
             (setf current nil)))
          ;; Comment line: skip
          ((let ((tr (string-left-trim '(#\Space #\Tab) content)))
             (and (>= (length tr) 2) (string= (subseq tr 0 2) "--")))
           nil)
          ;; Continuation line (starts with whitespace) -- append to current
          ((and current
                (> (length content) 0)
                (or (char= (char content 0) #\Space)
                    (char= (char content 0) #\Tab)))
           (push content current))
          ;; New statement: flush old, start new
          (t
           (when current
             (push (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (format nil "~{~A~^~%~}" (nreverse current)))
                   statements))
           (setf current (list content))))))
    ;; Flush last statement
    (when current
      (push (string-trim '(#\Space #\Tab #\Newline #\Return)
                         (format nil "~{~A~^~%~}" (nreverse current)))
            statements))
    (nreverse statements)))

(defun evaluate-expr (expr &optional eq-num-p)
  "Evaluate FriCAS input, handling multi-line code blocks correctly.
   Single-line or system-command input is passed directly to capture-fricas-output.
   Multi-line input is split into individual statements; each is evaluated
   in sequence and the results are concatenated."
  (let ((clean (string-trim '(#\Space #\Tab #\Newline #\Return) expr)))
    (if (find #\Newline clean)
        ;; Multi-line: split and evaluate each statement
        (let* ((stmts (split-fricas-statements clean))
               (parts
                 (loop for stmt in stmts
                       for result = (capture-fricas-output stmt eq-num-p)
                       when (and result (not (string= result "")))
                         collect result)))
          (format nil "~{~A~^~%~}" parts))
        ;; Single line: fast path
        (capture-fricas-output clean eq-num-p))))

(defun capture-fricas-output (expr &optional eq-num-p)
  (let ((out-str (make-string-output-stream))
        (boot-pkg (find-package "BOOT")))
    (flet ((do-eval ()
             (ensure-fricas-databases-shared)
             (handler-case
                 (let* ((symbols '("|$sayBrightlyStream|" "|$errorStream|" "|$InteractiveMode|" "|$QuietCommand|"))
                        (found-syms (mapcar (lambda (s) (find-symbol s boot-pkg)) symbols))
                        (int-frame-sym (find-symbol "|$InteractiveFrame|" boot-pkg))
                        (int-frame (if (and int-frame-sym (boundp int-frame-sym))
                                       (symbol-value int-frame-sym)
                                       nil)))

                   ;; Ensure we have an InteractiveFrame if we're evaluating
                   (when (and (not int-frame) int-frame-sym)
                     (let ((add-frame (find-symbol "addNewInterpreterFrame" boot-pkg)))
                       (when (and add-frame (fboundp add-frame))
                         (setf int-frame (funcall add-frame "Master"))
                         (set int-frame-sym int-frame))))

                   (let ((sym-list nil) (val-list nil))
                     (loop for sym in found-syms
                           for val in (list out-str out-str t t)
                           do (when sym
                                (push sym sym-list)
                                (push val val-list)))

                     (progv sym-list val-list
                       (let ((*standard-output* out-str)
                             (*package* boot-pkg))
                         (let ((res (catch 'boot::|top_level|
                                      (catch 'boot::SPAD_READER
                                        (if eq-num-p
                                            (boot::|parseAndEvalToStringEqNum| expr)
                                            (boot::|parseAndEvalToString| expr))))))
                           (let ((extra (get-output-stream-string out-str))
                                 (res-str (cond
                                            ((null res) "")
                                            ((consp res) (format nil "~{~A~^~%~}" res))
                                            (t (format nil "~A" res)))))
                             (if (string= extra "")
                                 res-str
                                 (format nil "~A~%~A" extra res-str))))))))
               (error (c)
                 (format nil "Evaluation Error: ~A" c)))))
      #+openmcl
      (ccl:with-lock-grabbed (*fricas-eval-lock*)
        (do-eval))
      #+sbcl
      (sb-thread:with-mutex (*fricas-eval-lock*)
        (do-eval))
      #-(or openmcl sbcl)
      (do-eval))))

(defun read-binary-line (stream)
  "Read a CRLF/LF-terminated line from a binary stream, returning as string.
   Strips CR characters. Returns :eof on end of stream."
  (let ((bytes (make-array 64 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil :eof)
          do (cond
               ((eq byte :eof)
                (return (if (zerop (length bytes))
                            :eof
                            #+sbcl (sb-ext:octets-to-string bytes :external-format :utf-8)
                            #+openmcl (ccl:decode-string-from-octets bytes :external-format :utf-8)
                            #-(or sbcl openmcl) (map 'string #'code-char bytes))))
               ((= byte 10) ; LF - end of line
                (return #+sbcl (sb-ext:octets-to-string bytes :external-format :utf-8)
                        #+openmcl (ccl:decode-string-from-octets bytes :external-format :utf-8)
                        #-(or sbcl openmcl) (map 'string #'code-char bytes)))
               ((= byte 13) nil) ; CR - skip
               (t (vector-push-extend byte bytes))))))

(defun mcp-read-chars (stream n)
  "Read exactly N characters from STREAM into a string. For CCL bivalent sockets
   which cannot use read-sequence with a byte array."
  (let ((buf (make-string n)))
    (loop for i from 0 below n
          for ch = (read-char stream nil nil)
          while ch
          do (setf (char buf i) ch))
    buf))

(defun mcp-loop (&optional (input *standard-input*) (output *standard-output*))
  "Main MCP message loop. Auto-detects binary vs text input streams.
   Binary streams (SBCL): reads Content-Length as byte count (correct per LSP spec).
   CCL bivalent socket streams: uses character I/O with Content-Length as char count.
   Plain text streams: falls back to reading Content-Length as character count.
   NOTE: CCL (Clozure CL) bivalent sockets always report 'character' as their
   stream-element-type even though they support binary I/O. We use
   *mcp-use-content-length* as the authoritative signal for LSP/socket framing."
  (let* ((*mcp-output-stream* output)
         (connection-running t)
         ;; True only for SBCL actual binary streams (unsigned-byte 8 element-type).
         ;; CCL bivalent sockets report 'character' so we cannot rely on subtypep.
         (is-binary-sbcl (and (streamp input)
                              (subtypep (stream-element-type input) '(unsigned-byte 8))))
         ;; In socket/LSP mode (*mcp-use-content-length* = T) on CCL we use
         ;; character-mode Content-Length framing (bivalent streams support
         ;; read-char just fine, and we write text headers in send-json too).
         (use-content-length *mcp-use-content-length*)
         (is-binary is-binary-sbcl))
    (handler-case
        (loop while (and connection-running *mcp-running*)
              do (let ((line (if is-binary
                                (read-binary-line input)
                                (read-line input nil :eof))))
                   (cond
                     ((eq line :eof) (return))
                     ((and (stringp line) (> (length line) 14)
                           (string= (subseq line 0 15) "Content-Length:"))
                      (let* ((len-str (string-trim '(#\Return #\Newline #\Space)
                                                   (subseq line 15)))
                             (len (ignore-errors (parse-integer len-str))))
                        (when len
                          ;; Skip remaining headers until empty line
                          (if is-binary
                              (loop for l = (read-binary-line input)
                                    while (and (not (eq l :eof))
                                               (> (length l) 0)))
                              (loop for l = (read-line input nil :eof)
                                    while (and (not (eq l :eof))
                                               (> (length l) 0)
                                               (not (string= l (string #\Return))))))

                          ;; Read the message body
                          (let* ((json-str
                                  (if is-binary
                                      ;; SBCL binary: read exactly len bytes, decode UTF-8
                                      (let ((octets (make-array len
                                                      :element-type '(unsigned-byte 8))))
                                        (read-sequence octets input)
                                        #+sbcl (sb-ext:octets-to-string octets
                                                 :external-format :utf-8)
                                        #+openmcl (ccl:decode-string-from-octets octets
                                                    :external-format :utf-8)
                                        #-(or sbcl openmcl) (map 'string #'code-char octets))
                                      ;; Text/CCL bivalent: read len characters
                                      (if use-content-length
                                          ;; Socket/LSP mode: Content-Length is in bytes
                                          ;; but for ASCII-dominant JSON, chars == bytes.
                                          ;; Use mcp-read-chars for reliable CCL compat.
                                          (mcp-read-chars input len)
                                          ;; Stdlib stdio mode: simple read-sequence
                                          (let ((buf (make-string len)))
                                            (read-sequence buf input)
                                            buf))))
                                 (json (ignore-errors (yason:parse json-str))))
                            (if (not json)
                                (send-error nil -32700 "Parse error")
                                (process-mcp-request json))))))
                     ((and use-content-length
                           (stringp line) (> (length line) 0)
                           ;; In socket mode, ignore non-Content-Length lines
                           ;; (they might be partial reads or protocol noise).
                           nil))
                     ((and (stringp line) (> (length line) 0))
                      ;; Fallback for line-based JSON (no Content-Length header, stdio mode)
                      (let ((json (ignore-errors (yason:parse line))))
                        (when json (process-mcp-request json)))))))
      (error (c)
        (format *error-output* "MCP Loop aborted: ~A~%" c)))))

(defun process-mcp-request (json)
  (let ((id (gethash "id" json))
        (method (gethash "method" json))
        (params (gethash "params" json)))
    (cond
      ((string= method "initialize") (handle-initialize id params))
      ((string= method "tools/list") (handle-list-tools id))
      ((string= method "tools/call") (handle-call-tool id (gethash "name" params) (gethash "arguments" params)))
      ((string= method "resources/list") (handle-list-resources id))
      ((string= method "resources/read") (handle-read-resource id params))
      ((string= method "repl/runcode") (handle-repl-runcode id params))
      ((string= method "repl/getDocFromWord") (handle-repl-get-doc-from-word id params))
      ((string= method "repl/getDocAt") (handle-repl-get-doc-at id params))
      ((string= method "repl/interrupt") nil)
      ((string= method "repl/cd") nil)
      ((string= method "repl/activateProject") nil)
      ((string= method "repl/togglePlotPane") nil)
      ((string= method "repl/toggleProgress") nil)
      ((string= method "repl/toggleDiagnostics") nil)
      ((string= method "notifications/initialized") nil)
      ((string= method "exit") (setf *mcp-running* nil))
      (t (if id (send-error id -32601 (format nil "Method not found: ~A" method)))))))

(defvar *fricas-mcp-initialized* nil)

(defun initialize-fricas-for-mcp ()
  "Initialize FriCAS interpreter for use by MCP tools."
  (when *fricas-mcp-initialized* (return-from initialize-fricas-for-mcp t))
  (let ((boot-pkg (find-package "BOOT")))
    (when boot-pkg
      ;; Redirect standard streams to stderr immediately to avoid polluting stdout (fd 1/3)
      (setf *standard-output* *error-output*)
      (setf *trace-output* *error-output*)
      (setf *debug-io* *error-output*)

      (handler-case
          (let ((msgs-sym (find-symbol "|$displayStartMsgs|" boot-pkg))
                (quiet-sym (find-symbol "|$QuietCommand|" boot-pkg))
                (init-sym (find-symbol "fricas_init" boot-pkg))
                (curout-sym (find-symbol "CUROUTSTREAM" boot-pkg))
                (int-frame-sym (find-symbol "|$InteractiveFrame|" boot-pkg))
                (jl-init-sym (find-symbol "init_julia_env" boot-pkg)))

            (when msgs-sym (set msgs-sym nil))
            (when quiet-sym (set quiet-sym t))

            ;; Ensure CUROUTSTREAM is redirected to STDERR
            (when curout-sym (set curout-sym *error-output*))

            ;; Initialize FriCAS only if not already running
            ;; (when started via -eval, FriCAS is already initialized)
            (when (and init-sym
                      (or (not int-frame-sym)
                          (not (boundp int-frame-sym))
                          (null (symbol-value int-frame-sym))))
              (let ((*standard-output* (make-broadcast-stream)))
                (funcall init-sym)))

            ;; Patch database read for thread-safety before opening them
            (patch-fricas-database-read)

            ;; Force open all databases if they are not already open
            ;; This ensures they are available in the current process
            (let ((interp-db-sym (find-symbol "open_interp_db" boot-pkg))
                  (op-db-sym (find-symbol "open_operation_db" boot-pkg))
                  (cat-db-sym (find-symbol "open_category_db" boot-pkg))
                  (browse-db-sym (find-symbol "open_browse_db" boot-pkg)))
              (when interp-db-sym (funcall interp-db-sym nil))
              (when op-db-sym (funcall op-db-sym nil))
              (when cat-db-sym (funcall cat-db-sym nil))
              (when browse-db-sym (funcall browse-db-sym nil)))

            ;; Initialize Julia if available
            (when jl-init-sym (funcall jl-init-sym))

            (setf *fricas-mcp-initialized* t)
            (ensure-fricas-databases-shared)
            t)
        (error (c)
          (format *error-output* "FriCAS initialization failed: ~A~%" c)
          nil)))))

(defun start-socket-mcp-server (port)
  #+sbcl
  (if *mcp-socket*
      (format *error-output* "MCP socket server already running.~%")
      (let ((is-local (stringp port)))
        (format *error-output* "Starting jlFriCAS MCP server on ~A ~A...~%" (if is-local "Unix socket" "port") port)
        (initialize-fricas-for-mcp)
        (setf *mcp-running* t)
        (setf *mcp-use-content-length* t)
        (let ((socket (if is-local
                          (make-instance 'sb-bsd-sockets:local-socket :type :stream)
                          (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))))
          (unless is-local
            (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
          (handler-case
              (progn
                (if is-local
                    (progn
                      (ignore-errors (delete-file port))
                      (sb-bsd-sockets:socket-bind socket port))
                    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port))
                (sb-bsd-sockets:socket-listen socket 5)
                (setf *mcp-socket* socket)
                (setf *mcp-thread*
                      (sb-thread:make-thread
                       (lambda ()
                         (unwind-protect
                              (loop while *mcp-running*
                                    do (handler-case
                                            (let* ((client-socket (sb-bsd-sockets:socket-accept socket))
                                                   (stream (sb-bsd-sockets:socket-make-stream client-socket
                                                                                              :input t
                                                                                              :output t
                                                                                              :buffering :full
                                                                                              :element-type '(unsigned-byte 8))))
                                              (sb-thread:make-thread
                                               (lambda ()
                                                 (unwind-protect
                                                      (mcp-loop stream stream)
                                                   (close stream)))
                                               :name (format nil "MCP Client on ~A" port)))
                                          (error (c)
                                            (when *mcp-running*
                                              (format *error-output* "Socket accept error: ~A~%" c)))))
                           (sb-bsd-sockets:socket-close socket)
                           (when (and is-local (probe-file port)) (ignore-errors (delete-file port)))
                           (setf *mcp-socket* nil)))
                       :name "MCP Socket Listener")))
            (error (c)
              (sb-bsd-sockets:socket-close socket)
              (format *error-output* "Failed to start socket server: ~A~%" c))))))
  #+openmcl
  (if *mcp-socket*
      (format *error-output* "MCP socket server already running.~%")
      (let ((is-local (stringp port)))
        (format *error-output* "Starting jlFriCAS MCP server on ~A ~A...~%"
                (if is-local "Unix socket" "port") port)
        (initialize-fricas-for-mcp)
        (setf *mcp-running* t)
        (setf *mcp-use-content-length* t)
        (handler-case
            (let ((listener (if is-local
                                (progn
                                  (ignore-errors (delete-file port))
                                  (ccl:make-socket :address-family :file
                                                   :connect :passive
                                                   :local-filename port
                                                   :format :bivalent))
                                (ccl:make-socket :connect :passive
                                                 :local-host "127.0.0.1"
                                                 :local-port port
                                                 :reuse-address t
                                                 :format :bivalent))))
              (setf *mcp-socket* listener)
              (setf *mcp-thread*
                    (ccl:process-run-function "MCP Socket Listener"
                      (lambda ()
                        (unwind-protect
                             (loop while *mcp-running*
                                   do (handler-case
                                           (let ((client (ccl:accept-connection listener)))
                                             (ccl:process-run-function
                                               (format nil "MCP Client on ~A" port)
                                               (lambda ()
                                                 (unwind-protect
                                                      (mcp-loop client client)
                                                   (close client)))))
                                         (error (c)
                                           (when *mcp-running*
                                             (format *error-output* "Socket accept error: ~A~%" c)))))
                          (close listener)
                          (when (and is-local (probe-file port))
                            (ignore-errors (delete-file port)))
                          (setf *mcp-socket* nil))))))
            (error (c)
              (format *error-output* "Failed to start socket server: ~A~%" c))))))

(defun start-socket-mcp-client (port)
  #+sbcl
  (let ((is-local (stringp port)))
    (format *error-output* "Connecting to MCP server on ~A ~A...~%" (if is-local "Unix socket" "port") port)
    (finish-output *error-output*)
    (initialize-fricas-for-mcp)
    (setf *mcp-use-content-length* t)
    (let ((socket (if is-local
                      (make-instance 'sb-bsd-sockets:local-socket :type :stream)
                      (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))))
      (handler-case
          (progn
            (if is-local
                (sb-bsd-sockets:socket-connect socket port)
                (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port))
            (let ((stream (sb-bsd-sockets:socket-make-stream socket
                                                           :input t
                                                           :output t
                                                           :buffering :full
                                                           :element-type '(unsigned-byte 8))))
              (setf *mcp-running* t)
              (setf *mcp-thread*
                    (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect
                            (mcp-loop stream stream)
                         (close stream)
                         (sb-bsd-sockets:socket-close socket)
                         (setf *mcp-thread* nil)))
                     :name "MCP Client Thread"))))
        (error (c)
          (sb-bsd-sockets:socket-close socket)
          (format *error-output* "Failed to connect to socket server: ~A~%" c)))))
  #+openmcl
  (let ((is-local (stringp port)))
    (format *error-output* "Connecting to MCP server on ~A ~A...~%"
            (if is-local "Unix socket" "port") port)
    (finish-output *error-output*)
    (initialize-fricas-for-mcp)
    (setf *mcp-use-content-length* t)
    (handler-case
        (let ((stream (if is-local
                          (ccl:make-socket :address-family :file
                                           :remote-filename port
                                           :format :bivalent)
                          (ccl:make-socket :remote-host "127.0.0.1"
                                           :remote-port port
                                           :format :bivalent))))
          (setf *mcp-running* t)
          (setf *mcp-thread*
                (ccl:process-run-function "MCP Client Thread"
                  (lambda ()
                    (unwind-protect
                         (mcp-loop stream stream)
                      (close stream)
                      (setf *mcp-thread* nil))))))
      (error (c)
        (format *error-output* "Failed to connect to socket server: ~A~%" c)))))

(defun start-mcp-server ()
  (if (or *mcp-thread* *mcp-socket*)
      (format *error-output* "MCP server already running.~%")
      (progn
        (format *error-output* "Starting jlFriCAS MCP server on stdio...~%")
        (initialize-fricas-for-mcp)
        (setf *mcp-running* t)
        (setf *mcp-use-content-length* nil) ;; stdio mode uses newline-delimited JSON
        (setf *mcp-thread*
              #+sbcl (sb-thread:make-thread
                      (lambda () (mcp-loop))
                      :name "MCP Server")
              #+openmcl (ccl:process-run-function "MCP Server"
                          (lambda () (mcp-loop)))))))

(defun stop-mcp-server ()
  (setf *mcp-running* nil)
  (when *mcp-socket*
    #+sbcl (sb-bsd-sockets:socket-close *mcp-socket*)
    #+openmcl (close *mcp-socket*))
  (setf *mcp-thread* nil))

(defun check-mcp-args ()
  "Check command line arguments for --mcp or --mcp-port flags and start server."
  (let* ((args (fricas-lisp:|getCLArgs|))
         (port-pos (position "--mcp-port" args :test #'string=))
         (raw-port-arg (when (and port-pos (< (1+ port-pos) (length args)))
                          (nth (1+ port-pos) args)))
         (port (when raw-port-arg
                 (handler-case (parse-integer raw-port-arg)
                    (error () raw-port-arg)))))
    (cond
      (port
       (format *error-output* "MCP socket mode detected on ~A ~A. Initializing FriCAS...~%" (if (stringp port) "Unix socket" "port") port)
       (start-socket-mcp-server port)
       ;; Keep main thread alive
       (loop while *mcp-running* do (sleep 1))
       (fricas-lisp:quit))
      ((member "--mcp" args :test #'string=)
       (let ((is-ls (or (boot::|getEnv| "FRICAS_LANGUAGESERVER")
                        (member "--lsp" args :test #'string=))))
         (setf *mcp-use-content-length* (if is-ls t nil))
         (format *error-output* "MCP stdio flag detected (~A mode). Initializing FriCAS...~%"
                 (if is-ls "LSP" "MCP")))

       ;; Use fd 3 for output (original stdout), fd 0 for input
       #+sbcl
       (let* ((bin-out (sb-sys:make-fd-stream 3
                                               :output t
                                               :buffering :full
                                               :element-type '(unsigned-byte 8)))
              (bin-in (sb-sys:make-fd-stream 0
                                              :input t
                                              :element-type '(unsigned-byte 8))))
         (setf *mcp-json-stream* bin-out)
         ;; Redirect fd 1 to stderr (fd 2) at the C level,
         ;; so any C-level output (e.g. "Checking for foreign routines")
         ;; goes to stderr instead of leaking to the MCP client via fd 3.
         ;; The fd 3 stream (bin-out) is already open and unaffected.
         (sb-posix:dup2 2 1)
         (unwind-protect
              (progn
                (initialize-fricas-for-mcp)
                (setf *mcp-running* t)
                (mcp-loop bin-in bin-out))
           (fricas-lisp:quit)))
       #+openmcl
       (let* ((bin-out (handler-case
                           (ccl::make-fd-stream 3
                                                :direction :output
                                                :character-p t)
                         (error ()
                           ;; fd 3 doesn't exist (e.g., -nosman mode)
                           ;; Fall back to stdout (fd 1)
                           (format *error-output* "fd 3 doesn't exist")
                           (ccl::make-fd-stream 1
                                                :direction :output
                                                :character-p t))))
              (bin-in (ccl::make-fd-stream 0
                                            :direction :input
                                            :character-p t)))
         (setf *mcp-json-stream* bin-out)
         ;; Redirect fd 1 to stderr (fd 2) at the C level,
         ;; so any C-level output doesn't leak to the MCP client.
         (ccl:external-call "dup2" :int 2 :int 1 :int)
         (unwind-protect
              (progn
                (initialize-fricas-for-mcp)
                (setf *mcp-running* t)
                (format *error-output* "STARTING MCP SERVER LOOP...~%")
                (finish-output *error-output*)
                (mcp-loop bin-in bin-out))
           (fricas-lisp:quit)))))))

(in-package :boot)

(defun |startMCPServer| ()
  (fricas-mcp:start-mcp-server))

(defun |stopMCPServer| ()
  (fricas-mcp:stop-mcp-server))

(defun |sendDisplay| (kind data)
  (fricas-mcp::|sendDisplay| kind data))

(defun |mcp| (args)
  "Main entry point for )lisp |mcp| command."
  (let ((cmd (if (consp args) (car args) args)))
    (cond
      ((or (null cmd) (eq cmd '|start|))
       (|startMCPServer|))
      ((eq cmd '|stop|)
       (|stopMCPServer|))
      (t (format t "Unknown MCP command: ~A (use 'start' or 'stop')~%" cmd)))))

(fricas-mcp:check-mcp-args)
