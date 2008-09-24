;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; )lisp (setf |$ioHook| (lambda (x &optional args) (format t "<~S>~%" x)))
;;

(require 'comint)

(defvar axiom-beg-marker-regexp "<|start[a-zA-Z]*|>\n")
(defvar axiom-end-marker-regexp "<|endOf[a-zA-Z]*|>\n")
;; we set the marker format string such that it always terminates with >
;; followed by newline
(defvar axiom-max-marker-length 40) ;; maximal length of a marker
(defvar axiom-marker-format-function 
  (concat
   "(lambda (x &optional args)                   "
   "  (cond ((and (eq x '|startKeyedMsg|)        "
   "              (or (eq (car args) 'S2GL0012)  "
   "                  (eq (car args) 'S2GL0013)  "
   "                  (eq (car args) 'S2GL0014)))"
   "         (format t \"<|startTypeTime|>~%\")) "
   "        ((and (eq x '|endOfKeyedMsg|)        "
   "              (or (eq (car args) 'S2GL0012)  "
   "                  (eq (car args) 'S2GL0013)  "
   "                  (eq (car args) 'S2GL0014)))"
   "         (format t \"<|endOfTypeTime|>~%\")) "
   "        (t (format t \"<~S>~%\" x))))        "))


(defface axiom-algebra 
  '((t (:background "#ffffa0")))
  "Face used for algebra output."
  :group 'axiom)

(defface axiom-typeTime 
  '((t (:background "#ffffa0" :foreground "darkgreen")))
  "Face used for type and time output."
  :group 'axiom)

(defface axiom-message 
  '((t (:background "#ffffa0" :foreground "palevioletred")))
  "Face used for type and time output."
  :group 'axiom)

(defface axiom-undefined
  '((t (:background "#ffffa0" :foreground "lightblue")))
  "Face used for other output."
  :group 'axiom)

(defface axiom-input
  nil
  "Face for user input."
  :group 'axiom)

(defface axiom-prompt
  nil
  "Face used for the prompt."
  :group 'axiom)


(defvar axiom-mode-map (make-sparse-keymap)
  "local key map for Axiom terminal mode")
(define-key axiom-mode-map [(meta k)] 'axiom-copy-to-clipboard)
(define-key axiom-mode-map [(ctrl return)] 'axiom-yank)
(define-key axiom-mode-map [(meta up)] 'axiom-previous-input)
(define-key axiom-mode-map (kbd "ESC <up>") 'axiom-previous-input)
(define-key axiom-mode-map [(meta down)] 'axiom-next-input)
(define-key axiom-mode-map (kbd "ESC <down>") 'axiom-next-input)
;(define-key axiom-mode-map [left] 'axiom-backward-char)
;(define-key axiom-mode-map [right] 'axiom-forward-char)
(define-key axiom-mode-map [(shift up)] 'axiom-paint-previous-line)
(define-key axiom-mode-map [(shift down)] 'axiom-paint-next-line)
(define-key axiom-mode-map [(shift left)] 'axiom-paint-previous-char)
(define-key axiom-mode-map [(shift right)] 'axiom-paint-next-char)
;; doesn't work in terminal
;; (define-key axiom-mode-map [return] 'axiom-eval)
(define-key axiom-mode-map "\C-m" 'axiom-eval)
(define-key axiom-mode-map [(shift return)] 'axiom-underscore-newline)
;; doesn't work in terminal
;;(define-key axiom-mode-map [(meta return)] 'axiom-eval-append)
(define-key axiom-mode-map "\M-\C-m" 'axiom-eval-append)
(define-key axiom-mode-map "\t" 'axiom-dynamic-complete)
(define-key axiom-mode-map "\C-c\C-c" 'axiom-interrupt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some helpers returning region types and positions of nearby regions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-prompt? (pos)
  "Returns t if pos is in a prompt region."
  (eq (get-text-property pos 'type) 'axiom-prompt))

(defun axiom-input? (pos)
  "Returns t if pos is in an input region."
  (eq (get-text-property pos 'type) nil))

(defun axiom-paintable? (pos)
  "Returns t if pos is in a region where we allow painting."
  (not (or (axiom-prompt? pos)
	   (axiom-input? pos))))

(defun axiom-next-prompt-pos (pos)
  "Returns the beginning position of the first prompt after pos, otherwise nil."
  (when (next-single-property-change pos 'type)
    (text-property-any pos (point-max) 'type 'axiom-prompt)))

(defun axiom-previous-prompt-pos (pos)
  "Returns the beginning position of the first prompt before pos, otherwise nil."
  (while (and (setq pos (previous-single-property-change pos 'type))
	      (not (axiom-prompt? pos))))
  pos)

(defun axiom-beginning-of-region-pos (pos)
  "Returns the beginning position of the current region, even if
it consists only of a single character. Should consider no
property as input, but doesn't yet."
  (cond ((= pos (point-min))
	 pos)
	((eq (get-text-property pos 'type)
	     (get-text-property (1- pos) 'type))
	 (or (previous-single-property-change pos 'type)
	     (point-min)))
	(t pos)))

(defun axiom-end-of-region-pos (pos)
  "Returns the end position of the current region."
  (1- (or (next-single-property-change pos 'type)
	  (1+ (point-max)))))

(defun axiom-can-receive-commands? ()
 "Returns true only if axiom is not working and not awaiting an
answer.  Prints a message otherwise."
 (cond ((eq axiom-state 'working)
	(message "Axiom is working")
	nil)
       (axiom-query-user 
	(message "Axiom expects an answer")
	nil)
       (t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interrupt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-interrupt ()
  (interactive)
  (process-send-string axiom-process "\003"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; painting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface axiom-paint-lightblue '((t (:background "lightblue")))
  "Lightblue face to use for painting."
  :group 'axiom)

(defface axiom-paint-red '((t (:background "red")))
  "Red face to use for painting."
  :group 'axiom)

(defface axiom-paint-custom '((t nil))
  "Custom face to use for painting."
  :group 'axiom)

(defvar axiom-paint-face-alist
  '(("lightblue" axiom-paint-lightblue)
    ("red"  axiom-paint-red)
    ("custom"  axiom-paint-custom)
    ("output"  axiom-output)))

(defvar axiom-paint-face 'axiom-paint-lightblue)

(defun axiom-paint-face ()
  (interactive)
  (let ((newpaint (completing-read "New paint face: "
                                   axiom-paint-face-alist
                                   nil t)))
    (setq axiom-paint-face (cadr (assoc newpaint axiom-paint-face-alist)))))

(defun axiom-make-space-if-necessary-and-paint ()
  "Make sure that a line does not end with a painted character."
;;; This would have the unwanted effect, that spaces appended by either
;;; axiom-paint-previous-line or axiom-paint-next-line inherit the face of the
;;; last character.
  (when (eolp)
    (insert-char 32 2 t)
    (backward-char 2))
  (forward-char 1)
  (when (eolp)
    (insert-char 32 1 t)
    (backward-char 1))
  (backward-char 1)
  (let ((pos (point)))
    (if (equal (get-text-property pos 'face)
	       axiom-paint-face)
	(put-text-property pos (1+ pos) 
			   'face (get-text-property pos 'type))
      (put-text-property pos (1+ pos) 'face axiom-paint-face))))

(defun axiom-paint-previous-line ()
  (interactive)
  (when (axiom-paintable? (point))
    (let ((inhibit-read-only t)
          (old-column (current-column))
          (old-pos    (point)))
      (axiom-make-space-if-necessary-and-paint)
      (previous-line 1)
      (if (axiom-paintable? (point))
          (let ((difference (- old-column (current-column))))
            (when (> difference 0)
              (insert-char 32 difference t)))
        (goto-char old-pos)))))

(defun axiom-paint-next-line ()
  (interactive)
  (when (axiom-paintable? (point))
    (let ((inhibit-read-only t)
          (old-column (current-column))
          (old-pos    (point)))
      (axiom-make-space-if-necessary-and-paint)
      (next-line 1)
      (if (axiom-paintable? (point))
          (let ((difference (- old-column (current-column))))
            (when (> difference 0)
              (insert-char 32 difference t)))
        (goto-char old-pos)))))

(defun axiom-paint-previous-char ()
  (interactive)
  (when (axiom-paintable? (point))
    (let ((inhibit-read-only t))
      (axiom-make-space-if-necessary-and-paint)
      (when (axiom-paintable? (1- (point)))
        (backward-char 1)))))

(defun axiom-paint-next-char ()
  (interactive)
  (when (axiom-paintable? (point))
    (let ((inhibit-read-only t))
      (axiom-make-space-if-necessary-and-paint)
      (forward-char 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resync directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-resync-directory ()
  "Go to the process mark, send )sys pwd to the axiom process and
 cd to the result."
;;; most of the work is actually done by axiom-resync-directory-post
  (interactive)
  (when (axiom-can-receive-commands?)
    (setq axiom-resync-directory?
	  (marker-position (process-mark axiom-process)))
    (process-send-string axiom-process ")sys pwd\n")))

(defun axiom-resync-directory-post ()
  "parse output from )sys pwd and clean up."
  (let ((inhibit-read-only t)
	(begin axiom-resync-directory?)
	(end (marker-position (process-mark axiom-process))))
    (setq axiom-resync-directory? nil)
    (cd (buffer-substring-no-properties 
	 begin 
	 (1- (previous-single-property-change end 'type))))
    (delete-region begin end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yanking input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; from emacs 22
(defun make-temp-file (prefix &optional dir-flag suffix)
  "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name."
  (let ((umask (default-file-modes))
        file)
    (unwind-protect
        (progn
          ;; Create temp files with strict access rights.  It's easy to
          ;; loosen them later, whereas it's impossible to close the
          ;; time-window of loose permissions otherwise.
          (set-default-file-modes ?\700)
          (while (condition-case ()
                     (progn
                       (setq file
                             (make-temp-name
                              (expand-file-name prefix temporary-file-directory)))
                       (if suffix
                           (setq file (concat file suffix)))
                       (if dir-flag
                           (make-directory file)
                         (write-region "" nil file nil 'silent nil 'excl))
                       nil)
                   (file-already-exists t))
            ;; the file was somehow created by someone else between
            ;; `make-temp-name' and `write-region', let's try again.
            nil)
          file)
      ;; Reset the umask.
      (set-default-file-modes umask))))

(defun axiom-yank (&optional quiet)
  "Puts the front item of the kill ring into a temporary file and
makes axiom )read it."
  (interactive "P")
  (when (axiom-can-receive-commands?)
    (setq axiom-yank-file? (make-temp-file "axiom" nil ".input"))
    (write-region (car kill-ring-yank-pointer) nil axiom-yank-file?)
    (goto-char (process-mark axiom-process))
    (while (not (file-exists-p axiom-yank-file?))
      (sit-for 0))
    (axiom-send-input (concat ")read " axiom-yank-file?
			      (if quiet " )quiet" "")))))

(defun axiom-yank-post ()
  "Deletes the temporary file created by axiom-yank."
  (delete-file axiom-yank-file?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; moving around
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-next-input-pos (pos)
  "Returns position just after next prompt after pos, or nil."
  (let ((pos (axiom-next-prompt-pos pos)))
    (when pos (or (next-single-property-change pos 'type)
		  (point-max)))))

(defun axiom-next-input ()
  "Puts point just after the next prompt.  If there is no next prompt, point
stays where it is."
  (interactive)
  (let ((pos (axiom-next-input-pos (point))))
    (when pos (goto-char pos))))

(defun axiom-previous-input ()
  "If not in input, or at the first input line, puts point just after the
previous prompt.  If in input, puts point just after the prompt before the
previous prompt.  Otherwise, point stays where it is."
  (interactive)
  (let ((pos (point)))
    (when (axiom-input? pos)
      (setq pos (1- (axiom-beginning-of-region-pos pos)))
      (when (axiom-prompt? pos)
	(setq pos (1- (axiom-previous-prompt-pos pos)))))
    (setq pos (axiom-previous-prompt-pos pos))
    (when pos
      (goto-char (axiom-next-input-pos pos)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; another input method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-eval-append ()
  "Evaluate the current input and append output."
  (interactive)
  (when (axiom-can-receive-commands?)
    (if (not (axiom-input? (point)))
	(message "Not in input region")
      (let ((beg-of-input-pos (axiom-beginning-of-region-pos (point)))
	    (end-of-input-pos (1- (or (next-single-property-change (point) 
								   'type)
				      (1+ (point-max)))))
	    input)
	(setq input (buffer-substring beg-of-input-pos
				      end-of-input-pos))
	(goto-char (process-mark axiom-process))
	(delete-region (point) (point-max))
	(axiom-send-input input)))))

(defun axiom-underscore-newline ()
  "If in input, append an underscore and a newline."
  (interactive)
  (if (not (axiom-input? (point)))
      (message "Not in input region")
    (end-of-line)
    (axiom-insert-ascii "_\n" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copying
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-copy-to-clipboard (&optional arg)
   "Copy the arg previous input-output combinations into the kill-ring."
   (interactive "p")
   (when (> arg 0)
     (let ((n arg)
	   (end (or (axiom-next-prompt-pos (point)) (point-max)))
	   (begin (point)))
       (while (and (> n 0)
		   (not (= begin (point-min))))
	 (setq begin (or (axiom-previous-prompt-pos (1- begin))
			 (point-min)))
	 (setq n (1- n)))
       (clipboard-kill-ring-save begin end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluating input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun axiom-set-idle ()
  "Emergency function in case emacs thinks that FriCAS is working, but this is not the case."
  (interactive)
  (setq axiom-state 'waiting))

(defun axiom-eval-overwrite (beg-of-input-pos end-of-input-pos)
  "Prepares overwrite, set's process-mark and point, send's input"

;;; if there is a prompt further down, we delete old prompt and output, and
;;; write the prompt from the very end of the buffer (which is always the last
;;; one) instead, and the new input instead.  axiom-repair-prompt is checked
;;; also in the output filter, so that the new prompt will be copied to the
;;; bottom of the buffer:

;;; (3) -> input               becomes (10) -> input
;;;
;;; (4) ->                             (4) ->
;;;
;;;
;;; (10) ->                            (11) ->

  (let ((inhibit-read-only t)
	current-prompt-beg
	final-prompt-beg
        final-prompt
	input)
;;; is there a prompt further down?
    (setq axiom-repair-prompt (axiom-next-prompt-pos end-of-input-pos))
    (if (not axiom-repair-prompt)
	(progn (goto-char beg-of-input-pos)
	       (comint-set-process-mark)
	       (goto-char end-of-input-pos))

;; find beginning position of prompt
      (setq current-prompt-beg 
	    (previous-single-property-change beg-of-input-pos 'type))

;;; else: delete the old output
      (delete-region (1+ end-of-input-pos) axiom-repair-prompt)
;;;       delete input, because otherwise the new prompt will inherit some of
;;;       its text-properties
      (setq input (delete-and-extract-region beg-of-input-pos
					     end-of-input-pos))
;;;       delete the old prompt before the input
      (delete-region current-prompt-beg beg-of-input-pos)
;;;       insert the new prompt from the bottom of the buffer - delete any
;;;       input that may be left there.
      (setq final-prompt-beg (axiom-beginning-of-region-pos (point-max)))
      (delete-region final-prompt-beg (point-max))
      (setq final-prompt (delete-and-extract-region 
			  (previous-single-property-change (point-max) 'type)
			  (point-max)))
	;; maybe I should do some error checking here - did I really copy a
	;; prompt?
	
      (goto-char current-prompt-beg)
      (axiom-insert-bypass-filter final-prompt 'axiom-prompt)
      (comint-set-process-mark)
      (insert input))

    (setq axiom-output-pos (1+ (point)))
    (comint-send-input)))

(defun axiom-eval () 
  (interactive)
  (if (eq axiom-state 'working)
      (message "Axiom is working")
    (let ((pos (point))
	  beg-of-input-pos
	  end-of-input-pos)
      (if (not (axiom-input? pos))
	  (message "Not in input region")
;;; now we know that we are either after a prompt of after a user query.
;;; thus, there should be a previous text property
	(setq beg-of-input-pos (axiom-beginning-of-region-pos pos))
	(if axiom-query-user
;;; we still need to check, whether we are in the right input-region
;;; for user's convenience, we move there if we aren't.
	    (if (not (= beg-of-input-pos (process-mark axiom-process)))
		(goto-char (process-mark axiom-process))
;;; we are in the right place, get the input
	      (setq end-of-input-pos (axiom-end-of-region-pos pos))
	      (comint-send-string axiom-process
				  (concat (delete-and-extract-region 
					   beg-of-input-pos 
					   end-of-input-pos)
					  "\n")))
;;; not user query
	  (if (not (axiom-prompt? (1- beg-of-input-pos)))
	      (message "Not after a prompt")
;;; now we know that beg-of-input-pos is truly the first pos after a prompt

	    (setq end-of-input-pos (axiom-end-of-region-pos pos))
;;; now end-of-input-pos is the end of the input, possibly multi-line
	    (axiom-eval-overwrite beg-of-input-pos end-of-input-pos)))))))
	    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dealing with output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun axiom-get-next-output (str ind)
  "str is output to be processed, ind the position of unprocessed output in
str. Returns: 
  nil, if all output from str has been processed,
  (nil    end-pos) if buffer can be inserted up to end-pos,
  (marker end-pos) if there is a marker ending at end-pos."

  (let* ((axMarkBeg (string-match axiom-beg-marker-regexp str ind))
	 (axMarkBegEnd (when axMarkBeg (match-end 0)))
	 (axMarkEnd (string-match axiom-end-marker-regexp str ind))
	 (axMarkEndEnd (when axMarkEnd (match-end 0)))
	 (output-length (length str))
	 (end-pos (or (and (not (eq (aref str (1- output-length)) 
				    ?\n)) ;; last char is a newline
			   (string-match "<" str 
					 (max ind (- output-length 
						     axiom-max-marker-length))))
		      output-length)))
    (cond ((eq axMarkBeg ind)                          ;; begin marker at beginning
	   (list (substring str axMarkBeg axMarkBegEnd) axMarkBegEnd))
	      
	  ((eq axMarkEnd ind)                          ;; end marker   at beginning
	   (list (substring str axMarkEnd axMarkEndEnd) axMarkEndEnd))
	      
	  ((and axMarkBeg 
		(or (not axMarkEnd)                    ;; begin marker before end marker
		    (< axMarkBeg axMarkEnd)))
	   (list nil axMarkBeg))
	      
	  ((and axMarkEnd 
		(or (not axMarkBeg)                    ;; end marker before begin marker
		    (< axMarkEnd axMarkBeg)))
	   (list nil axMarkEnd))

	  ((and (not axMarkBeg) (not axMarkEnd))       ;; no complete marker
	   (unless (= end-pos ind)
	     (list nil end-pos)))

	  (t (error "Cannot happen")))))

(defun axiom-update-output-properties (new-type)
  "Update output properties, reset axiom-output-length, set axiom-output-type
to new-type."
;;; prepend (length type) to axiom-output-properties
  (setq axiom-output-properties 
	(append axiom-output-properties
		(list (cons axiom-output-length
			    axiom-output-type))))
  (setq	axiom-output-length 0)
  (setq	axiom-output-type new-type))  

(defun axiom-preoutput-filter (str)
  "feeds axiom-output-properties."
  (let ((output-index 0) 
	;; maintains position in axiom-output-buffer yet to be inserted
	output-str-type
	result
	(new-str ""))

    (setq axiom-output-buffer (concat axiom-output-buffer str))
    (while (setq output-str-type (axiom-get-next-output axiom-output-buffer 
							output-index))

      (cond ((null (car output-str-type))                         ;; no markup
	     (setq new-str (substring axiom-output-buffer 
				      output-index
				      (cadr output-str-type)))
	     (if axiom-redirect
		 (save-excursion
		   (goto-char (point-max))
		   (axiom-insert-bypass-filter new-str 'axiom-prompt))
	       (setq result (cons new-str result))
	       (setq axiom-output-length (+ axiom-output-length 
					    (- (cadr output-str-type)
					       output-index)))))
	      
	    ((equal (car output-str-type) "<|startPrompt|>\n")
	     (if (not axiom-repair-prompt)
		 (axiom-update-output-properties 'axiom-prompt)
	       (setq axiom-redirect t)
	       (axiom-update-output-properties 'axiom-undefined)))
	     
	    ((equal (car output-str-type) "<|endOfPrompt|>\n")
	     (if (not axiom-repair-prompt)
		 (axiom-update-output-properties 'axiom-undefined)))
;;;	       (setq axiom-repair-prompt nil)
;;;	       (setq axiom-redirect nil)))

	    ((equal (car output-str-type) "<|startReadLine|>\n")  ;; expect input after prompt
	     (axiom-update-output-properties nil)
	     (setq axiom-state 'waiting))
	      
	    ((equal (car output-str-type) "<|endOfReadLine|>\n")
	     (setq axiom-state 'working)
	     (axiom-update-output-properties 'axiom-undefined))

	    ((equal (car output-str-type) "<|startAlgebraOutput|>\n") 
	     (axiom-update-output-properties 'axiom-algebra))

	    ((equal (car output-str-type) "<|endOfAlgebraOutput|>\n") 
	     (axiom-update-output-properties 'axiom-undefined))
	      
	    ((equal (car output-str-type) "<|startTypeTime|>\n") 
	     (axiom-update-output-properties 'axiom-typeTime))

	    ((equal (car output-str-type) "<|endOfTypeTime|>\n") 
	     (axiom-update-output-properties 'axiom-undefined))
	      
	    ((equal (car output-str-type) "<|startKeyedMsg|>\n") 
	     (axiom-update-output-properties 'axiom-message))

	    ((equal (car output-str-type) "<|endOfKeyedMsg|>\n") 
	     (axiom-update-output-properties 'axiom-undefined))

	    ((equal (car output-str-type) "<|startQueryUser|>\n") ;; expect input after system command
	     (axiom-update-output-properties nil)
	     (setq axiom-state 'waiting)
	     (setq axiom-query-user t))

	    ((equal (car output-str-type) "<|endOfQueryUser|>\n") ;; expect input after system command
	     (setq axiom-state 'working)
	     (axiom-update-output-properties 'axiom-undefined)
	     (setq axiom-query-user nil))

	    (t (message (concat "Possibly unrecognised marker: " (car output-str-type)))
	       (setq axiom-output-length (+ axiom-output-length 
					    (- (cadr output-str-type)
					       output-index)))))
      (setq output-index (cadr output-str-type)))

    ;; delete processed output from buffer
    (setq axiom-output-buffer (substring axiom-output-buffer output-index))
    ;; return str to be inserted into buffer
    (apply 'concat (reverse result))))

(defun axiom-output-filter (str)
  "eats axiom-output-properties."
  (let (new-output-pos)
    (while axiom-output-properties
      (setq new-output-pos (+ axiom-output-pos 
			      (caar axiom-output-properties)))

      (while (< (point-max) new-output-pos) (sit-for 0))

      (axiom-set-properties axiom-output-pos
			    new-output-pos
			    (cdar axiom-output-properties))

      (setq axiom-output-pos new-output-pos)
      (setq axiom-output-properties (cdr axiom-output-properties)))
    (when (and axiom-repair-prompt
	       axiom-redirect
	       (eq axiom-state 'waiting))
      (delete-char 1) 
      (axiom-next-input)
      (comint-set-process-mark)
      (setq axiom-repair-prompt nil)
      (setq axiom-redirect nil))))


(defun axiom-set-properties (beg end type)
  "Set the properties depending on type."
  (when type
    (put-text-property beg end 'type type)
    (put-text-property beg end 'face type)
    (put-text-property beg end 'rear-nonsticky t)
    (put-text-property beg end 'front-sticky t)
    (put-text-property beg end 'read-only t)))

(defun axiom-insert-bypass-filter (str type)
  "Insert str with properties given type at point. For input just
use insert."
  (let ((pos (point)))
    (insert str)
    (axiom-set-properties  pos (+ pos (length str)) type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; connect with comint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar axiom-process nil)

(defvar axiom-output-buffer "")             ;; contains output yet to be processed
(defvar axiom-output-properties nil)        ;; contains properties yet to be processed
(defvar axiom-output-length 0)              ;; contains length of output since last markup
(defvar axiom-output-pos 1)                 ;; contains position of first output to be markup'd
                                            ;; should maybe be a marker!
(defvar axiom-output-type 'axiom-undefined) ;; contains type of output since last markup
(defvar axiom-redirect nil)                 ;; non-nil, when we redirect output, bypassing the filter


;; (put 'axiom-mode 'mode-class 'special)
(define-derived-mode axiom-mode comint-mode "AXIOM"
  ""
  (setq axiom-process (get-buffer-process (current-buffer)))

  (setq axiom-output-buffer "")             
  (setq axiom-output-properties nil)        
  (setq axiom-output-length 0)                 
  (setq axiom-output-pos 1)                 
  (setq axiom-output-type 'axiom-undefined) 
  (setq axiom-redirect nil)
  
  (setq axiom-state 'working)       ;; 'working or 'waiting
  
  (setq axiom-resync-directory? nil);; are we resyncing the directory?
  (setq axiom-repair-prompt nil)    ;; did we overwrite old output?
  (setq axiom-query-user nil)       ;; are we expecting a response to a query?
  (setq axiom-cd nil)               ;; are we changing the directory?
  (setq axiom-yank-file? nil)       ;; did we yank a file?

  (make-variable-buffer-local 'comint-eol-on-send)
  (setq comint-eol-on-send nil)

  ;; we want this hook to be local lest we conflict with other buffers in
  ;; comint or shell mode
  (make-variable-buffer-local 'comint-preoutput-filter-functions)
  (add-hook 'comint-preoutput-filter-functions (function axiom-preoutput-filter t t))
  (add-hook 'comint-output-filter-functions (function axiom-output-filter) nil t)

  (setq font-lock-defaults nil)

;;; should I use process send-string here?
  (comint-send-string axiom-process 
		      (concat ")lisp (setf |$ioHook| " 
			      axiom-marker-format-function 
			      ")\n"))
  

;;  (setq buffer-offer-save t)
;;  (add-hook 'after-save-hook 'axiom-save-history nil t)
;;  (add-hook 'kill-buffer-query-functions 'axiom-query-kill)

    ;; Next, we turn on some key bindings for our new mode:
  (use-local-map axiom-mode-map)
;;  (substitute-key-definition 'comint-previous-input 
;;			     'axiom-scroll-previous-input axiom-mode-map)
;;  (substitute-key-definition 'comint-next-input 
;;			     'axiom-scroll-next-input axiom-mode-map)
;;
;;  (setq font-lock-defaults nil)
;;
;;  (unless axiom-process (insert (concat ")history )restore " (buffer-file-name))))
;;  (set-buffer-modified-p nil))
)

(defun axiom-run()
  "Run Axiom in a buffer."
  (switch-to-buffer (make-comint "Axiom" "/tmp/bin/fricas" nil "-noclef" "-noht")))


(defun axiom ()
   "Run axiom in a terminal environment"
  (interactive)
  (if (not (comint-check-proc "*Axiom*"))
      (progn (axiom-run)
             (axiom-mode))
    (pop-to-buffer "*Axiom*")))


(provide 'axiom)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab completion -- currently only of filenames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-dynamic-complete ()
  "Dynamically perform completion at point, if after a prompt."
  (interactive)
  (let (beg-of-input)
    (when (and (axiom-input? (point))
	       (axiom-prompt? (1- (setq beg-of-input 
					(axiom-beginning-of-region-pos (point)))))
	       (save-excursion
		 (goto-char beg-of-input)
		 (looking-at " *)")))
      (axiom-dynamic-complete-filename))))

(defun axiom-file-name-all-completions (pathnondir directory)
  "Returns all filenames relevant to axiom"
  (save-match-data
    (remove-if-not 
     (function (lambda (f) 
                 (or (and (string-match "\\.[^.]*\\'" f)
                          (member (match-string 0 f)
                                  (list ".input" ".spad" ".as")))
                     (string= (file-name-directory f) f))))
     (file-name-all-completions pathnondir directory))))

(defun axiom-file-name-completion (file directory)
  "Returns the longest string common to all file names relevant to axiom in
DIRECTORY that start with FILE.  If there is only one and FILE matches it
exactly, returns t.  Returns nil if DIR contains no name starting with FILE."
  (let* ((completions (axiom-file-name-all-completions file directory))
	 (frst (first completions))
	 (len  (length frst))
	 (start      0)
	 (not-done   t))
    (cond ((consp (rest completions))
	   (while (and not-done
		       (> len start))
	     (let ((char (substring frst start (1+ start)))
		   (rst  (rest completions)))
	       (while (and not-done 
			   (consp rst))
		 (if (and (> (length (first rst)) start)
			  (string= (substring (first rst) 
					      start (1+ start))
				   char))
		     (setq rst (rest rst))
		   (setq not-done nil))))
	     (when not-done 
	       (setq start (1+ start))))
	   (substring frst 0 start))
	  ((string= frst file)
	   t)
	  (t
	   frst))))

(defun axiom-dynamic-list-filename-completions ()
  "List in help buffer possible completions of the filename at point."
  (interactive)
  (let* ((completion-ignore-case (memq system-type '(ms-dos windows-nt)))
	 ;; If we bind this, it breaks remote directory tracking in rlogin.el.
	 ;; I think it was originally bound to solve file completion problems,
	 ;; but subsequent changes may have made this unnecessary.  sm.
	 ;;(file-name-handler-alist nil)
	 (filename (or (comint-match-partial-filename) ""))
	 (pathdir (file-name-directory filename))
	 (pathnondir (file-name-nondirectory filename))
	 (directory (if pathdir (comint-directory pathdir) default-directory))
	 (completions (axiom-file-name-all-completions pathnondir directory)))
    (if (not completions)
	(message "No completions of %s" filename)
      (comint-dynamic-list-completions
       (mapcar 'comint-quote-filename completions)))))

(defun axiom-dynamic-complete-filename ()
  "Dynamically complete at point as a filename.
See `comint-dynamic-complete-filename'.  Returns t if successful."
  (interactive)
  (let* ((completion-ignore-case (memq system-type '(ms-dos windows-nt)))
	 (completion-ignored-extensions comint-completion-fignore)
	 ;; If we bind this, it breaks remote directory tracking in rlogin.el.
	 ;; I think it was originally bound to solve file completion problems,
	 ;; but subsequent changes may have made this unnecessary.  sm.
	 ;;(file-name-handler-alist nil)
	 (minibuffer-p (window-minibuffer-p (selected-window)))
	 (success t)
	 (dirsuffix (cond ((not comint-completion-addsuffix)
			   "")
			  ((not (consp comint-completion-addsuffix))
			   (char-to-string directory-sep-char))
			  (t
			   (car comint-completion-addsuffix))))
	 (filesuffix (cond ((not comint-completion-addsuffix)
			    "")
			   ((not (consp comint-completion-addsuffix))
			    " ")
			   (t
			    (cdr comint-completion-addsuffix))))
	 (filename (or (comint-match-partial-filename) ""))
	 (pathdir (file-name-directory filename))
	 (pathnondir (file-name-nondirectory filename))
	 (directory (if pathdir (comint-directory pathdir) default-directory))
	 (completion (axiom-file-name-completion pathnondir directory)))
    (cond ((null completion)
	   (message "No completions of %s" filename)
	   (setq success nil))
	  ((eq completion t)            ; Means already completed "file".
	   (insert filesuffix)
	   (unless minibuffer-p
	     (message "Sole completion")))
	  ((string-equal completion "") ; Means completion on "directory/".
	   (axiom-dynamic-list-filename-completions))
	  (t                            ; Completion string returned.
	   (let ((file (concat (file-name-as-directory directory) completion)))
	     (insert (comint-quote-filename
		      (substring (directory-file-name completion)
				 (length pathnondir))))
	     (cond ((symbolp (axiom-file-name-completion completion directory))
		    ;; We inserted a unique completion.
		    (insert (if (file-directory-p file) dirsuffix filesuffix))
		    (unless minibuffer-p
		      (message "Completed")))
		   ((and comint-completion-recexact comint-completion-addsuffix
			 (string-equal pathnondir completion)
			 (file-exists-p file))
		    ;; It's not unique, but user wants shortest match.
		    (insert (if (file-directory-p file) dirsuffix filesuffix))
		    (unless minibuffer-p
		      (message "Completed shortest")))
		   ((or comint-completion-autolist
			(string-equal pathnondir completion))
		    ;; It's not unique, list possible completions.
		    (axiom-dynamic-list-filename-completions))
		   (t
		    (unless minibuffer-p
		      (message "Partially completed")))))))
    success))

