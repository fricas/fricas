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
(export '($spadroot $directory-list reroot
          make-absolute-filename |$defaultMsgDatabaseName|))

;;; Various lisps use different ``extensions'' on the filename to indicate
;;; that a file has been compiled. We set this variable correctly depending
;;; on the system we are using.
(defvar |$lisp_bin_filetype|
  #+:GCL "o"
  #+lucid "bbin"
  #+symbolics "bin"
  #+:cmu (c:backend-fasl-file-type c:*target-backend*)
  #+:sbcl "fasl"
  #+:clisp "fas"
  #+:openmcl (subseq (namestring CCL:*.FASL-PATHNAME*) 1)
  #+:ecl "fas"
  #+:lispworks (pathname-type (compile-file-pathname "foo.lisp"))
  #+:poplog "lsp"
  )

;;; The relative directory list specifies a search path for files
;;; for the current directory structure.
(defvar $relative-directory-list
  '("/share/msgs/"
    "/share/spadhelp/" ))

;;; The relative directory list specifies how to find the algebra
;;; directory from the current {\bf FRICAS} shell variable.
(defvar $relative-library-directory-list '("/algebra/"))

;;; This is the system-wide list of directories to search.
;;; It is set up in the {\bf reroot} function.
(defvar $directory-list ())

;;; This is the system-wide search path for library files.
;;; It is set up in the {\bf reroot} function.
(defvar $library-directory-list ())

;;; Prefix a filename with the {\bf FRICAS} shell variable.
(defun make-absolute-filename (name)
 (concatenate 'string $spadroot name))

#|
The reroot function is used to reset the important variables used by
the system. In particular, these variables are sensitive to the
{\bf FRICAS} shell variable. That variable is renamed internally to
be {\bf \$spadroot}. The {\bf reroot} function will change the
system to use a new root directory and will have the same effect
as changing the {\bf FRICAS} shell variable and rerunning the system
from scratch.  A correct call looks like:
\begin{verbatim}
(in-package "BOOT")
(reroot "${FRICAS}")
\end{verbatim}
where the [[${FRICAS}]] variable points to installed tree.
|#
(defun reroot (dir)
  (setq $spadroot dir)
  (setq $directory-list
   (mapcar #'make-absolute-filename $relative-directory-list))
  (setq $library-directory-list
   (mapcar #'make-absolute-filename $relative-library-directory-list))
  (setq |$defaultMsgDatabaseName|
        (pathname (make-absolute-filename "/share/msgs/s2-us.msgs")))
  )

;;; Sets up the system to use the {\bf FRICAS} shell variable if we can
;;; and default to the {\bf \$spadroot} variable (which was the value
;;; of the {\bf FRICAS} shell variable at build time) if we can't.
;;; Use the parent directory of FRICASsys binary as fallback.
(defun initroot (&optional (newroot nil))
  (reroot (or (|getEnv| "FRICAS") newroot
              (if (|fricas_probe_file| $spadroot) $spadroot)
              (let ((bin-parent-dir
                     (concatenate 'string
                                  (directory-namestring (car (|getCLArgs|)))
                                  "/../")))
                (if (|fricas_probe_file| (concatenate 'string bin-parent-dir
                                                      "algebra/interp.daase"))
                    bin-parent-dir))
              (error "setenv FRICAS or (setq $spadroot)"))))

;;; Gnu Common Lisp (GCL) (at least 2.6.[78]) requires some changes
;;; to the default memory setup to run FriCAS efficiently.
;;; This function performs those setup commands.
#+:GCL
(defun init-memory-config (&key
                           (cons 500)
                           (fixnum 200)
                           (symbol 500)
                           (package 8)
                           (array 400)
                           (string 500)
                           (cfun 100)
                           (cpages 3000)
                           (rpages 1000)
                           (hole 2000) )
  ;; initialize GCL memory allocation parameters
  (progn
    (system:allocate 'cons cons)
    (system:allocate 'fixnum fixnum)
    (system:allocate 'symbol symbol)
    (system:allocate 'package package)
    (system:allocate 'array array)
    (system:allocate 'string string)
    (system:allocate 'cfun cfun)
    (system:allocate-contiguous-pages cpages)
    (system:allocate-relocatable-pages rpages)
    (system:set-hole-size hole))
  nil)

#|
;############################################################################
;# autoload dependencies
;#
;# if you are adding a file which is to be autoloaded the following step
;# information is useful:
;#  there are 2 cases:
;#   1) adding files to currently autoloaded parts
;#      (as of 2/92: browser old parser and old compiler)
;#   2) adding new files
;#   case 1:
;#     a) you have to add the file to the list of files currently there
;#        (e.g. see BROBJS above)
;#     b) add an autolaod rule
;#        (e.g. ${AUTO}/parsing.${O}: ${OUT}/parsing.${O})
;#     c) edit util.lisp to add the 'external' function (those that
;#        should trigger the autoload
;#   case 2:
;#     build-interpsys (in util.lisp) needs an extra argument for the
;#     new autoload things and several functions in util.lisp need hacking.
;############################################################################

The {\bf build-interpsys} function takes a list of files to load
into the image ({\bf load-files}). It also takes several lists of files,
one for each subsystem which will be autoloaded. Autoloading is explained
below. Next it takes a set of shell variables, the most important of
which is the {\bf spad} variable. This is normally set to be the same
as the final build location. This function is called in the
src/interp/Makefile.

This function calls {\bf initroot} to set up pathnames we need. Next
it sets up the lisp system memory (at present only for GCL). Next
it loads all of the named files, resets a few global state variables,
loads the databases, sets up autoload triggers and clears out hash tables.
After this function is called the image is clean and can be saved.
|#
(defun build-interpsys (load-files spad)
  #-:ecl
  (progn
      (mapcar #'load load-files)
      (interpsys-image-init spad))
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
          (dolist (el '(|$build_date| |$build_version| |$createLocalLibDb|))
              (if (boundp el)
                  (push (list 'defparameter el (symbol-value el))
                        initforms)))
          (push `(interpsys-ecl-image-init ,spad) initforms)
          (push `(fricas-restart) initforms)
          (setf initforms (reverse initforms))
          (push `progn initforms)
          (setf FRICAS-LISP::*fricas-initial-lisp-forms* initforms)
      )
  )
)

(defun interpsys-ecl-image-init (spad)
     (format *standard-output* "Starting interpsys~%")
     #+:ecl (let ((sym (or (find-symbol "TRAP-FPE" "EXT")
                           (find-symbol "TRAP-FPE" "SI"))))
                 (if (and sym (fboundp sym))
                     (funcall sym T T)))
     #+:ecl (let ((sym (find-symbol "*BREAK-ENABLE*" "SI")))
                (if (and sym (boundp sym))
                    (setf (symbol-value sym) t)))
     (initroot spad)
     (setf spad $spadroot)
     (format *standard-output* "spad = ~s~%" spad)
     (force-output  *standard-output*)
     (interpsys-image-init spad)
     (format *standard-output* "before fricas-restart~%")
     (force-output  *standard-output*)
)

(defun interpsys-image-init (spad)
  (setf *package* (find-package "BOOT"))
  (initroot spad)
  #+:GCL
  (init-memory-config :cons 500 :fixnum 200 :symbol 500 :package 8
                      :array 400 :string 500 :cfun 100 :cpages 1000
                      :rpages 1000 :hole 2000)
  #+:GCL
  (setq compiler::*suppress-compiler-notes* t)
  (|interpsysInitialization|)
  (setq *load-verbose* nil)
  (resethashtables) ; the databases into core, then close the streams
 )

;; the following are for conditional reading
(setq |$opSysName| '"shell")

;;; moved from bookvol5

(defvar |$HiFiAccess| t               "t means turn on history mechanism")

(defvar |$reportUndo| nil "t means we report the steps undo takes")
(defvar $openServerIfTrue t "t means try starting an open server")
(defparameter $SpadServerName "/tmp/.d" "the name of the spad server socket")
(defvar |$SpadServer| nil "t means Scratchpad acts as a remote server")


(defun |loadExposureGroupData| ()
 (cond
  ((load "./exposed" :verbose nil :if-does-not-exist nil)
    '|done|)
  ((load (concat (|getEnv| "FRICAS") "/algebra/exposed")
     :verbose nil :if-does-not-exist nil)
   '|done|)
  (t '|failed|) ))

(defvar *fricas-load-libspad* t)

(defun fricas-init ()
#+:GCL
  (init-memory-config :cons 500 :fixnum 200 :symbol 500 :package 8
    :array 400 :string 500 :cfun 100 :cpages 3000 :rpages 1000 :hole 2000)
#+:GCL (setq compiler::*compile-verbose* nil)
#+:GCL (setq compiler::*suppress-compiler-warnings* t)
#+:GCL (setq compiler::*suppress-compiler-notes* t)
  (in-package "BOOT")
  (initroot)
#+:poplog (setf POPLOG:*READ-PROMPT* "") ;; Turn off Poplog read prompts
#+:GCL (system:gbc-time 0)
    #+(or :sbcl :clisp :openmcl :lispworks)
    (if *fricas-load-libspad*
        (let* ((ax-dir (|getEnv| "FRICAS"))
               (spad-lib (concatenate 'string ax-dir "/lib/libspad.so")))
            (format t "Checking for foreign routines~%")
            (format t "FRICAS=~S~%" ax-dir)
            (format t "spad-lib=~S~%" spad-lib)
            (if (|fricas_probe_file| spad-lib)
                (progn
                    (setf *fricas-load-libspad* nil)
                    (format t "foreign routines found~%")
                    #+(or :sbcl :openmcl :lispworks)
                    (|quiet_load_alien| spad-lib)
                    #+(or :sbcl :openmcl)
                    (fricas-lisp::init-gmp
                        (concatenate 'string ax-dir "/lib/gmp_wrap.so"))
                    #+(and :clisp :ffi)
                    (progn
                        (eval `(FFI:DEFAULT-FOREIGN-LIBRARY ,spad-lib))
                        (FRICAS-LISP::clisp-init-foreign-calls))
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
  (|interpsys_restart|)
)

(DEFVAR |$trace_stream| *standard-output*)
(DEFVAR CUROUTSTREAM *standard-output*)

(defun fricas-restart ()
  ;;; Need to reinitialize various streams because
  ;;; CLISP closes them when dumping executable
  (setf CUROUTSTREAM *standard-output*)
  (setf |$trace_stream| *standard-output*)
  (setq |$algebraOutputStream| (|mkOutputConsoleStream|))
  (setq |$fortranOutputStream| (|mkOutputConsoleStream|))
  (setq |$mathmlOutputStream| (|mkOutputConsoleStream|))
  (setq |$texmacsOutputStream| (|mkOutputConsoleStream|))
  (setq |$htmlOutputStream| (|mkOutputConsoleStream|))
  (setq |$openMathOutputStream| (|mkOutputConsoleStream|))
  (setq |$texOutputStream| (|mkOutputConsoleStream|))
  (setq |$formattedOutputStream| (|mkOutputConsoleStream|))
  (fricas-init)
  #+(or :GCL :poplog)
  (|spad|)
  #-(or :GCL :poplog)
  (let ((*debugger-hook*
            (lambda (condition previous-handler)
                (spad-system-error-handler condition))
       ))
     (handler-bind ((error #'spad-system-error-handler))
       (|spad|)))
)


(defun spad-save (save-file do-restart)
  (setq |$SpadServer| nil)
  (setq $openServerIfTrue t)
  (FRICAS-LISP::save-core-restart save-file
         (if do-restart #'boot::fricas-restart nil))
)

(defun |statisticsInitialization| ()
 "initialize the garbage collection timer"
 #+:GCL (system:gbc-time 0)
 nil)

(defun |mkAutoLoad| (fn cname)
   (function (lambda (&rest args)
                 #+:sbcl
                 (handler-bind ((style-warning #'muffle-warning))
                     (|autoLoad| fn cname))
                 #-:sbcl
                 (|autoLoad| fn cname)
                 (apply cname args))))

(defun |eval|(x)
    #-:GCL
    (handler-bind ((warning #'muffle-warning)
                   #+:sbcl (sb-ext::compiler-note #'muffle-warning))
            (eval  x))
    #+:GCL
    (eval  x)
)

;;; For evaluating categories we need to bind $.
(defun |c_eval|(u) (let (($ '$)) (declare (special $)) (|eval| u)))

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
