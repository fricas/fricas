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

#|
This file is a collection of utility functions that are useful
for system level work.  {\bf build-interpsys} interfaces to the
src/interp/Makefile.

A fifth group of related functions are some translated boot
functions we need to define here so they work and are available
at load time.
|#

(in-package "BOOT")

#+:GCL
(defun |resetStackLimits| () (system:reset-stack-limits))
#-:GCL
(defun |resetStackLimits| () nil)

#|
The {\bf build-interpsys} function takes a list of files to load
into the image ({\bf load-files}).

This function calls {\bf initroot} to set up pathnames we need. Next
it sets up the lisp system memory (at present only for GCL). Next
it loads all of the named files, resets a few global state variables,
loads the databases and clears out hash tables.
After this function is called the image is clean and can be saved.
|#
(defun build-interpsys (load-files)
  #-:ecl
  (progn
      (mapcar #'load load-files)
      ;; for CMUCL, do not load libspad.so before dumping image,
      ;; the dumped image will load libspad.so instead.
      (let (#+:cmu(*fricas-load-libspad* nil)
            #+:cmu($openServerIfTrue nil))
          (interpsys-image-init t)))
  (if (and (boundp 'FRICAS-LISP::*building-fricassys*)
                FRICAS-LISP::*building-fricassys*)
       (progn
           #+:GCL(setf compiler::*default-system-p* nil)
           #+:GCL(compiler::emit-fn nil)
           (setq *load-verbose* nil)
           #+:clisp(setf custom:*suppress-check-redefinition* t)
       )
  )
  #+:ecl
  (progn
      (setf FRICAS-LISP::*fricas-initial-lisp-objects*
           (append FRICAS-LISP::*fricas-initial-lisp-objects*
                   '("util.o")
                   load-files))
      (let ((initforms nil))
          (dolist (el '(|$build_date| |$build_version|
                        |$lisp_id_string| |$createLocalLibDb|))
              (if (boundp el)
                  (push (list 'defparameter el (symbol-value el))
                        initforms)))
          (setf FRICAS-LISP::*fricas-initial-lisp-forms*
                `(progn
                   ,@initforms
                   (interpsys-ecl-image-init)
                   (|fricas_restart|)
                   (si:top-level)))
      )
  )
)

(defun interpsys-ecl-image-init ()
     (format *standard-output* "Starting interpsys~%")
     #+:ecl (let ((sym (or (find-symbol "TRAP-FPE" "EXT")
                           (find-symbol "TRAP-FPE" "SI"))))
                 (if (and sym (fboundp sym))
                     (funcall sym T T)))
     #+:ecl (let ((sym (find-symbol "*BREAK-ENABLE*" "SI")))
                (if (and sym (boundp sym))
                    (setf (symbol-value sym) t)))
     (interpsys-image-init nil)
     (format *standard-output* "spad = ~s~%" |$spadroot|)
     (force-output  *standard-output*)
     (format *standard-output* "before fricas_restart~%")
     (force-output  *standard-output*)
)

(defun interpsys-image-init (display_messages)
  (setf *package* (find-package "BOOT"))
  (|initroot|)
  #+:GCL
  (setq compiler::*suppress-compiler-notes* t)
    (|interpsysInitialization| display_messages)
    (setq *load-verbose* nil)
    ; the databases into core, then close the streams
    (|reset_hash_tables| display_messages)
)

;; the following are for conditional reading
(setq |$opSysName| '"shell")

(defvar $openServerIfTrue t "t means try starting an open server")
(defparameter $SpadServerName "/tmp/.d" "the name of the spad server socket")
(defvar |$SpadServer| nil "t means Scratchpad acts as a remote server")


(defun |loadExposureGroupData| ()
 (cond
  ((load "./exposed" :verbose nil :if-does-not-exist nil)
    '|done|)
  ((load (|make_absolute_filename| "/algebra/exposed")
     :verbose nil :if-does-not-exist nil)
   '|done|)
  (t '|failed|) ))

(defvar *fricas-load-libspad* t)

(defun |fricas_init| ()
#+:GCL (setq compiler::*compile-verbose* nil)
#+:GCL (setq compiler::*suppress-compiler-warnings* t)
#+:GCL (setq compiler::*suppress-compiler-notes* t)
#+:GCL (when (fboundp 'si::readline-off) (si::readline-off))  ;; Disable GCL readline to avoid double-prompt issue
  (in-package "BOOT")
  (|initroot|)
#+:poplog (setf POPLOG:*READ-PROMPT* "") ;; Turn off Poplog read prompts
#+:GCL (system:gbc-time 0)
    #+(or :sbcl :clisp :openmcl :lispworks :cmu)
    (if *fricas-load-libspad*
        (let ((spad-lib (|make_absolute_filename| "/lib/libspad.so")))
            (format t "Checking for foreign routines~%")
            (format t "FRICAS=~S~%" |$spadroot|)
            (format t "spad-lib=~S~%" spad-lib)
            (if (|fricas_probe_file| spad-lib)
                (progn
                    (setf *fricas-load-libspad* nil)
                    (format t "foreign routines found~%")
                    #+(or :sbcl :openmcl :lispworks :cmu)
                    (|quiet_load_alien| spad-lib)
                    #+(or :sbcl :openmcl)
                    (fricas-lisp::init-gmp
                        (|make_absolute_filename| "/lib/gmp_wrap.so"))
                    #+(and :clisp :ffi)
                    (progn
                        (eval `(FFI:DEFAULT-FOREIGN-LIBRARY ,spad-lib))
                        (FRICAS-LISP::clisp-init-foreign-calls))
                    #+:cmu
                    (FRICAS-LISP::cmu-init-foreign-calls)
                )
                (setf $openServerIfTrue nil))))
    #+(or :GCL (and :clisp :ffi) :sbcl :cmu :openmcl :ecl :lispworks)
    (if $openServerIfTrue
        (let ((os (|openServer| $SpadServerName)))
             (format t "openServer result ~S~%" os)
             (if (zerop os)
                 (progn
                      (setf $openServerIfTrue nil)
                      #+:GCL
                      (if (fboundp 'si::readline-off)
                          (si::readline-off))
                      (setq |$SpadServer| t)))))
  (setq *GENSYM-COUNTER* 0)
  (|interpsys_restart|)
)

(defun |fricas_restart2| ()
  #+:poplog
  (|spad|)
  #-:poplog
  (let ((*debugger-hook*
            (lambda (condition previous-handler)
                (|spad_system_error_handler| condition))
       ))
     (handler-bind ((error #'|spad_system_error_handler|))
       (|spad|)))
)


(defun spad-save (save-file do-restart save-exec)
  ;; if the parameter "save-exec" is t, FriCAS will be saved as a standalone
  ;; executable; if nil, FriCAS will be saved as a Lisp core file.
  (setq |$SpadServer| nil)
  (setq $openServerIfTrue t)
  (FRICAS-LISP::save-core-restart
         (if save-exec save-file (strconc save-file ".core"))
         (if do-restart #'boot::|fricas_restart| nil) save-exec)
)

(defun |mkAutoLoad| (cname)
   (function (lambda (&rest args)
                 #+:sbcl
                 (handler-bind ((style-warning #'muffle-warning))
                     (|autoLoad| cname))
                 #-:sbcl
                 (|autoLoad| cname)
                 (apply cname args))))

(defun |eval|(x)
    (handler-bind ((warning #'muffle-warning)
                   #+:sbcl (sb-ext::compiler-note #'muffle-warning))
            (eval  x))
)

;;; For evaluating categories we need to bind %.
(defun |c_eval|(u) (let ((% '%)) (declare (special %)) (|eval| u)))

;;; Accesed from HyperDoc
(defun |setViewportProcess| ()
  (setq |$ViewportProcessToWatch|
     (stringimage (CDR
         (|processInteractive|  '(|key| (|%%| -2)) NIL) ))))

;;; Accesed from HyperDoc
(defun |waitForViewport| ()
  (progn
   (do ()
       ((not (zerop (|run_shell_command|
        (concat
         "ps "
         |$ViewportProcessToWatch|
         " > /dev/null && sleep 0.1")))))
       ())
   (|sockSendInt| |$MenuServer| 1)
   (|setIOindex| (- |$IOindex| 3))
  )
)
