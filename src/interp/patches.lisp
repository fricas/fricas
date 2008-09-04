;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     - Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     - Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;;     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package "BOOT")
;;patches for now

(defun |stripSpaces| (str)
    (string-trim '(#\Space) str))

;; browser stuff:
(defvar |$standard| 't)
(defvar |$saturn| 'nil)

(defun CATCHALL (a &rest b) a) ;; not correct but ok for now
(defvar |$demoFlag| nil)

(define-function '|construct| #'list) ;; NEEDED , SPAD-COMPILER generated Lisp code
(define-function '|COMP,TRAN| #'comp-tran) ;called by |compWithMappingMode|

(defvar |Undef| (function |Undef|)) ;needed in NewbFVectorCopy
(define-function '|spadHash| #'sxhash)

(defun |mkAutoLoad| (fn cname)
   (function (lambda (&rest args)
                 (|autoLoad| fn cname)
                 (apply cname args))))

(setq |$printTimeIfTrue| nil)


(defmacro dribinit (streamvar)
  `(if (is-console ,streamvar)
       (setq ,streamvar *terminal-io*)))

(defun clear-highlight ()
  (let ((|$displaySetValue| nil))
    (declare (special |$displaySetValue| |$saveHighlight| |$saveSpecialchars|))
    (setq |$saveHighlight| |$highlightAllowed|
          |$highlightAllowed| nil)
    (setq |$saveSpecialchars| |$specialCharacters|)
    (|setOutputCharacters| '(|plain|))))

(defun reset-highlight ()
  (setq |$highlightAllowed| |$saveHighlight|)
  (setq |$specialCharacters| |$saveSpecialchars|))

(defun |spool| (filename)
  (if (and filename (symbolp (car filename)))
      (setf (car filename) (symbol-name (car filename))))
  (cond ((null filename)
         (dribble) (TERPRI)
         (reset-highlight))
        ((probe-file (car filename))
         (error (format nil "file ~a already exists" (car filename))))
        (t (dribble (car filename))
           (TERPRI)
           (clear-highlight))
    ))

(defun |cd| (args)
    (let ((dname (if (null args)
                     (trim-directory-name (namestring (user-homedir-pathname)))
                     (car args))))
         (if (symbolp dname)
             (setf dname (symbol-name dname)))
         (chdir dname))
    (|sayKeyedMsg| 'S2IZ0070 (list (get-current-directory))))

(defun toplevel (&rest foo) (throw '|top_level| '|restart|))
;;(defun toplevel (&rest foo) (lisp::unwind))

(define-function 'top-level #'toplevel)
(define-function 'unwind #'|spadThrow|)
(define-function 'resume #'|spadThrow|)

(DEFUN BUMPCOMPERRORCOUNT () ())

(setq |nullstream| '|nullstream|)
(setq |nonnullstream| '|nonnullstream|)
(setq *print-escape* nil) ;; so stringimage doesn't escape idents?

;;; FIXME: do we need this?
#+(and :GCL :IEEE-FLOATING-POINT)
 (setq system:*print-nans* T)

(defun /RF (&rest foo &aux (Echo-Meta 'T))
  (declare (special Echo-Meta))
  (/RF-1 nil))
(defun /RQ (&rest foo &aux (Echo-Meta nil))
  (declare (special Echo-Meta))
  (/RF-1 nil))
(defun |/RQ,LIB| (&rest foo &aux (Echo-Meta nil) ($LISPLIB T))
  (declare (special Echo-Meta $LISPLIB))
  (/RF-1 nil))

(defun /RF-1 (ignore)
 (declare (ignore ignore))
  (let* ((input-file (vmlisp::make-input-filename /EDITFILE))
     (lfile ())
     (type (pathname-type input-file)))
    (cond
     ((string= type "boot")
#-:CCL
      (boot input-file
         (setq lfile (make-pathname :type "lisp"
                           :defaults input-file)))
#+:CCL
      (boot input-file
         (setq lfile (make-pathname :name (pathname-name input-file)
                                :type "lisp")))
      (load lfile))
     ((string= type "lisp") (load input-file))
     ((string= type "bbin") (load input-file))
     ((and (string= type "input")
           |$useNewParser|)
      (|ncINTERPFILE| input-file Echo-Meta))
     (t (spad input-file)))))

(defun /EF (&rest foo)
  (obey (concat "vi " (namestring (vmlisp::make-input-filename /EDITFILE))))
#|
#-:CCL
  (defun user::start () (in-package "BOOT") (boot::|start|))
#+:CCL
  (defun user::start () (setq *package* (find-package "BOOT")) (boot::|start|)
|#
 )

(setq |$algebraOutputStream|
   (setq |$fortranOutputStream|
      (setq |$texOutputStream|
          (setq |$formulaOutputStream|
             (setq |conOutStream| (make-synonym-stream '*terminal-io*))))))

;; non-interactive restarts...
(defun restart0 ()
#+(and :NAG :ccl) (lisp::init-lm 0)
  (compressopen);; set up the compression tables
  (interpopen);; open up the interpreter database
  (operationopen);; all of the operations known to the system
  (categoryopen);; answer hasCategory question
  (browseopen)
  (|makeConstructorsAutoLoad|)
  (let ((asharprootlib (strconc (|getEnv| "AXIOM") "/aldor/lib/")))
    (set-file-getter (strconc asharprootlib "runtime"))
    (set-file-getter (strconc asharprootlib "lang"))
    (set-file-getter (strconc asharprootlib "attrib"))
    (set-file-getter (strconc asharprootlib "axlit"))
    (set-file-getter (strconc asharprootlib "minimach"))
    (set-file-getter (strconc asharprootlib "axextend")))
)

(defun whocalled (n) nil) ;; no way to look n frames up the stack
(defun setletprintflag (x) x)

(define-function '|eval| #'eval)

;;--------------------> NEW DEFINITION (see cattable.boot.pamphlet)
(defun |compressHashTable| (ht) ht)
(defun GETZEROVEC (n) (MAKE-ARRAY n :initial-element 0))

(setq |$localVars| ())  ;checked by isType

(setq |$specialCharacters| |$plainRTspecialCharacters|)

;; following in defined in word.boot
(defun |bootFind| (word) ())
;; following 3 are replacements for g-util.boot
(define-function '|isLowerCaseLetter| #'LOWER-CASE-P)
(define-function '|isUpperCaseLetter| #'UPPER-CASE-P)
(define-function '|isLetter| #'ALPHA-CHAR-P)




(defvar *msghash* nil "hash table keyed by msg number")

(defun cacheKeyedMsg (file)
  (let ((line "") (msg "") key)
   (with-open-file (in file)
    (catch 'done
     (loop
      (setq line (read-line in nil nil))
      (cond
       ((null line)
         (when key
          (setf (gethash key *msghash*) msg))
          (throw 'done nil))
       ((= (length line) 0))
       ((char= (schar line 0) #\S)
         (when key
          (setf (gethash key *msghash*) msg))
         (setq key (intern line "BOOT"))
         (setq msg ""))
       ('else
        (setq msg (concatenate 'string msg line)))))))))

(defun |fetchKeyedMsg| (key ignore)
 (declare (ignore ignore))
 (setq key (|object2Identifier| key))
 (unless *msghash*
  (setq *msghash* (make-hash-table))
  (cacheKeyedMsg |$defaultMsgDatabaseName|))
 (gethash key *msghash*))

(setq identity #'identity) ;to make LispVM code for handling constants to work

(|initializeTimedNames| |$interpreterTimedNames| |$interpreterTimedClasses|)

;;; Accesed from HyperDoc
(defun |setViewportProcess| ()
  (setq |$ViewportProcessToWatch|
     (stringimage (CDR
         (|processInteractive|  '(|key| (|%%| -2)) NIL) ))))

;;; Accesed from HyperDoc
(defun |waitForViewport| ()
  (progn
   (do ()
       ((not (zerop (obey
        (concat
         "ps "
         |$ViewportProcessToWatch|
         " > /dev/null")))))
       ())
   (|sockSendInt| |$MenuServer| 1)
   (|setIOindex| (- |$IOindex| 3))
  )
)


(defun |makeVector| (els type)
 (make-array (length els) :element-type (or type t) :initial-contents els))


(defun |makeList| (size el) (make-list size :initial-element el) )

#+:akcl
(defun print-xdr-stream (x y z) (format y "XDR:~A" (xdr-stream-name x)))
#+:akcl
(defstruct (xdr-stream
                (:print-function  print-xdr-stream))
           "A structure to hold XDR streams. The stream is printed out."
           (handle ) ;; this is what is used for xdr-open xdr-read xdr-write
           (name ))  ;; this is used for printing
#+(and :gcl (not (or :dos :win32)))
(defun |xdrOpen| (str dir) (make-xdr-stream :handle (system:xdr-open str) :name str))
#+:CCL
(defun |xdrOpen| (str dir) (xdr-open str dir) )
#+(and :gcl (or :dos :win32))
(defun |xdrOpen| (str dir) (format t "xdrOpen called"))

#+(and :akcl (not (or :dos :win32)))
(defun |xdrRead| (xstr r) (system:xdr-read (xdr-stream-handle xstr) r) )
#+:CCL
(defun |xdrRead| (xstr r) (xdr-read xstr r) )
#+(and :gcl (or :dos :win32))
(defun |xdrRead| (str) (format t "xdrRead called"))

#+(and :akcl (not (or :dos :win32)))
(defun |xdrWrite| (xstr d) (system:xdr-write (xdr-stream-handle xstr) d) )
#+:CCL
(defun |xdrWrite| (xstr d) (xdr-write xstr d) )
#+(and :gcl (or :dos :win32))
(defun |xdrWrite| (str) (format t "xdrWrite called"))

;; here is a test for XDR
;; (setq *print-array* T)
;; (setq foo (open "xdrtest" :direction :output))
;; (setq xfoo (|xdrOpen| foo))
;; (|xdrWrite| xfoo "hello: This contains an integer, a float and a float array")
;; (|xdrWrite| xfoo 42)
;; (|xdrWrite| xfoo 3.14159)
;; (|xdrWrite| xfoo (make-array 10 :element-type 'double-float :initial-element 2.78111D12))
;; (close foo)
;; (setq foo (open "xdrtest" :direction :input))
;; (setq xfoo (|xdrOpen| foo))
;; (|xdrRead| xfoo "")
;; (|xdrRead| xfoo 0)
;; (|xdrRead| xfoo 0.0)
;; (|xdrRead| xfoo (make-array 10 :element-type 'double-float ))
;; (setq *print-array* NIL)

;; clearParserMacro has problems as boot code (package notation)
;; defined here in Lisp

(setq /MAJOR-VERSION 2)
(setq echo-meta nil)
(defun /versioncheck (n) (unless (= n /MAJOR-VERSION) (throw 'versioncheck -1)))

