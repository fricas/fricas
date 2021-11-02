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


;; this files contains basic routines for error handling
(in-package "BOOT")

#+:GCL
(defun error-format (message args)
  (let ((|$BreakMode| '|break|))
    (declare (special |$BreakMode|))
   (if (stringp message) (apply #'format nil message args) nil)))

#-:GCL
(defun error-format (c)
  (let ((|$BreakMode| '|break|))
    (declare (special |$BreakMode|))
    (format nil "~a" c)))

;;(defmacro |trappedSpadEval| (form) form) ;;nop for now

#+:GCL
(defun |resetStackLimits| () (system:reset-stack-limits))
#-:GCL
(defun |resetStackLimits| () nil)

;; failed union branch --  value returned for numeric failure
(setq |$numericFailure| (cons 1 "failed"))

(defvar |$oldBreakMode|)

;; following macro evaluates form returning Union(type-of form, "failed")

#+:GCL
(defmacro |trapNumericErrors| (form)
  `(let ((|$oldBreakMode| |$BreakMode|)
         (|$BreakMode| '|trapNumerics|)
         (val))
     (setq val (catch '|trapNumerics| ,form))
     (if (eq val |$numericFailure|) val
       (cons 0 val))))

#-:GCL
(defmacro |trapNumericErrors| (form)
    `(handler-case (cons 0 ,form)
         (arithmetic-error () |$numericFailure|)))

(defmacro |trappedSpadEval| (form)
  `(let ((|$BreakMode| '|trapSpadErrors|))
       (catch '|trapSpadErrors| (cons 0 ,form))))

(defmacro |trappedSpadEvalUnion| (form)
  `(let ((|$BreakMode| '|trapSpadErrors|))
       (catch '|trapSpadErrors| ,form)))

#+:sbcl
(progn
(defun |do_timeout| (f ti)
   (handler-case
          (sb-ext:with-timeout ti (SPADCALL f))
       (sb-ext:timeout (e)
          (THROW '|trapSpadErrors| |$numericFailure|))
   )
)

(defun |eval_with_timeout| (f ti)
    (CATCH '|trapSpadErrors| (cons 0 (|do_timeout| f ti))))
)

#-:sbcl
(defun |eval_with_timeout| (f ti) (|error| "unimplemented for this Lisp"))

(defparameter |$inLispVM| nil)

#-:GCL
(defun spad-system-error-handler (c)
  (block nil
    (setq |$NeedToSignalSessionManager| T)
    (if (and (boundp '|$inLispVM|) (boundp '|$BreakMode|))
        (cond ((eq |$BreakMode| '|validate|)
                   (|systemError| (error-format c)))
               ((and (null |$inLispVM|)
                     (memq |$BreakMode| '(|nobreak| |query| |resume| |quit|)))
                   (let ((|$inLispVM| T)) ;; turn off handler
                        (return (|systemError| (error-format c)))))
               ((eq |$BreakMode| '|letPrint2|)
                   (setq |$BreakMode| nil)
                   (throw '|letPrint2| nil))))))



;; the following form embeds around the akcl error handler
#+:GCL
(eval-when
 (load eval)
 (UNEMBED 'system:universal-error-handler)
 (EMBED 'system:universal-error-handler
            '(lambda (type correctable? op
                           continue-string error-string &rest args)
               (block
                nil
                (setq |$NeedToSignalSessionManager| T)
                (if (and (boundp '|$inLispVM|) (boundp '|$BreakMode|))
                    (cond ((eq |$BreakMode| '|validate|)
                           (|systemError| (error-format error-string args)))
                          ((and (eq |$BreakMode| '|trapNumerics|)
                                (eq type :ERROR))
                           (setq |$BreakMode| nil)                         (throw '|trapNumerics| |$numericFailure|))
                          ((and (eq |$BreakMode| '|trapNumerics|)
                                (boundp '|$oldBreakMode|)
                                (setq |$BreakMode| |$oldBreakMode|)
                                nil)) ;; resets error handler
                          ((and (null |$inLispVM|)
                                (memq |$BreakMode| '(|nobreak| |query| |resume| |quit|)))
                           (let ((|$inLispVM| T)) ;; turn off handler
                             (return
                              (|systemError| (error-format error-string args)))))
                          ((eq |$BreakMode| '|letPrint2|)
                           (setq |$BreakMode| nil)
                           (throw '|letPrint2| nil))))
                (apply system:universal-error-handler type correctable? op
                       continue-string error-string args )))))
