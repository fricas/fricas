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
for system level work. A couple of the functions, {\bf build-depsys}
and {\bf build-interpsys} interface to the src/interp/Makefile.

A third group of related functions are used to set up the
{\bf autoload} mechanism. These enable whole subsystems to
be kept out of memory until they are used.

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
(defvar *lisp-bin-filetype*
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
  '("/../../src/input/"
    "/doc/msgs/"
    "/../../src/algebra/"
    "/../../src/interp/"  ; for boot and lisp  files (helps fd)
    "/doc/spadhelp/" ))

;;; The relative directory list specifies how to find the algebra
;;; directory from the current {\bf AXIOM} shell variable.
(defvar $relative-library-directory-list '("/algebra/"))

;;; This is the system-wide list of directories to search.
;;; It is set up in the {\bf reroot} function.
(defvar $directory-list ())

;;; This is the system-wide search path for library files.
;;; It is set up in the {\bf reroot} function.
(defvar $library-directory-list ())

;;; When we are building a {\bf depsys} image for GCL we need
;;; need to initialize some optimization routines. Each time a file is
;;; compiled in GCL we collect some function information and write it
;;; out to a {\bf .fn} file. If this {\bf .fn} file exists at compile
;;; time then GCL will perform function call optimizations. These can
;;; be significant in terms of performance.
(defun make-depsys (build-interp-dir)
  ;; perform system initializations for building a starter system
  (init-memory-config)
  #+:GCL
  (let ()
   (mapcar
     #'load
     (directory (concatenate 'string build-interp-dir "/*.fn")))
   (with-open-file
    (out (concatenate 'string build-interp-dir "/proclaims.lisp" )
      :direction :output)
     (compiler::make-proclaims out))
   (load (concatenate 'string build-interp-dir "/proclaims.lisp")))
  )

;;; This is the {\bf boot parser} subsystem. It is only needed by
;;; algebra developers and developers who translate boot code to
;;; Common Lisp.
(setq parse-functions
      '(
;;      loadparser
        |oldParserAutoloadOnceTrigger|
        |parse_Expression|
        |spadCompile|
        init-boot/spad-reader))

;;; This is the {\bf spad compiler} subsystem. It is only needed by
;;; developers who write or modify algebra code.
(setq comp-functions
      '(
;;      loadcompiler
        |oldCompilerAutoloadOnceTrigger|
        |compileSpad2Cmd|
        |compilerDoit|
        |compilerDoitWithScreenedLisplib|
        |mkCategory|
        |cons5|
        |isCategoryForm|
        |sublisV|))

;;; This is the {\bf browser} subsystem. It will get autoloaded only
;;; if you use the browse function of the {\bf hypertex} system.
(setq browse-functions
      '(
;;      loadbrowse
        |browserAutoloadOnceTrigger|
        |parentsOf|           ;interop.boot
        |getParentsFor|       ;old compiler
        |folks|               ;for astran
        |extendLocalLibdb|    ;)lib needs this
        |oSearch|
        |aokSearch|
        |kSearch|
        |aSearch|
        |genSearch|
        |docSearch|
        |abSearch|
        |detailedSearch|
        |ancestorsOf|
        |aPage|
        |dbGetOrigin|
        |dbGetParams|
        |dbGetKindString|
        |dbGetOrigin|
        |dbComments|
        |grepConstruct|
        |buildLibdb|
        |bcDefiniteIntegrate|
        |bcDifferentiate|
        |bcDraw|
        |bcExpand|
        |bcIndefiniteIntegrate|
        |bcLimit|
        |bcMatrix|
        |bcProduct|
        |bcSeries|
        |bcSolve|
        |bcSum|
        |cSearch|
        |conPage|
        |dbName|
        |dbPart|
        |extendLocalLibdb|
        |form2HtString|
        |htGloss|
        |htGreekSearch|
        |htHistory|
        |htSystemCommands|
        |htSystemVariables|
        |htTextSearch|
        |htTutorialSearch|
        |htUserVariables|
        |htsv|
        |oPage|
        |oPageFrom|
        |spadSys|
        |spadType|
        |syscomPage|
        |unescapeStringsInForm|))

;;; This is part of the {\bf ALDOR subsystem}. These will be loaded
;;; if you compile a {\bf .as} file rather than a {\bf .spad} file.
;;; {\bf ALDOR} is an external compiler that gets automatically called
;;; if the file extension is {\bf .as}.
(setq asauto-functions '(
        loadas
;;      |as|                         ;; now in as.boot
;;      |astran|                     ;; now in as.boot
        |spad2AxTranslatorAutoloadOnceTrigger|
        |sourceFilesToAxcliqueAxFile|
        |sourceFilesToAxFile|
        |setExtendedDomains|
        |makeAxFile|
        |makeAxcliqueAxFile|
        |nrlibsToAxFile|
        |attributesToAxFile| ))

;;; These are some old {\bf debugging} functions.  I can't imagine
;;; why you might autoload them but they don't need to be in a running
;;; system.
(setq debug-functions '(
        loaddebug
        |showSummary|
        |showPredicates|
        |showAttributes|
        |showFrom|
        |showImp|))

#|
There are several subsystems within {\bf AXIOM} that are not normally
loaded into a running system. They will be loaded only if you invoke
one of the functions listed here. Each of these listed functions will
have their definitions replaced by a special ``autoloader'' function.
The first time a function named here is called it will trigger a
load of the associated subsystem, the autoloader functions will get
overwritten, the function call is retried and now succeeds. Files
containing functions listed here are assumed to exist in the
{\bf autoload} subdirectory. The list of files to load is defined
in the src/interp/Makefile.
|#

#|
This function is called by {\bf build-interpsys}. It takes two lists.
The first is a list of functions that need to be used as
``autoload triggers''. The second is a list of files to load if one
of the trigger functions is called. At system build time each of the
functions in the first list is set up to load every file in the second
list. In this way we will automatically load a whole subsystem if we
touch any function in that subsystem. We call a helper function
called {\bf setBootAutoLoadProperty} to set up the autoload trigger.
This helper function is listed below.
|#
(defun |setBootAutloadProperties| (fun-list file-list)
  (mapc #'(lambda (fun) (|setBootAutoLoadProperty| fun file-list)) fun-list)
)

;;; This function knows where the {\bf autoload} subdirectory lives.
;;; It is called by {\bf mkBootAutoLoad} above to find the necessary
;;; files.
(defun boot-load (file)
  (let ((name (concat $SPADROOT "/autoload/" (pathname-name file))))
    (if (eq |$printLoadMsgs| '|on|)
	(|sayKeyedMsg| 'S2IL0030 (list name)))
    (load name)))

;;; This is a helper function to set up the autoload trigger. It sets
;;; the function cell of each symbol to {\bf mkBootAutoLoad} which is
;;; listed below.
(defun |setBootAutoLoadProperty| (func file-list)
  (setf (symbol-function func) (|mkBootAutoLoad| func file-list)) )

#|
This is how the autoload magic happens. Every function named in the
autoload lists is actually just another name for this function. When
the named function is called we call {\bf boot-load} on all of the
files in the subsystem. This overwrites all of the autoload triggers.
We then look up the new (real) function definition and call it again
with the real arguments. Thus the subsystem loads and the original
call succeeds.
|#
(defun |mkBootAutoLoad| (fn file-list)
   (function (lambda (&rest args)
                 #+:sbcl
                 (handler-bind ((style-warning #'muffle-warning))
                     (mapc #'boot-load file-list))
                 #-:sbcl
                 (mapc #'boot-load file-list)
                 (unless (string= (subseq (string fn) 0 4) "LOAD")
                  (apply (symbol-function fn) args)))))

;;; Prefix a filename with the {\bf AXIOM} shell variable.
(defun make-absolute-filename (name)
 (concatenate 'string $spadroot name))

#|
The reroot function is used to reset the important variables used by
the system. In particular, these variables are sensitive to the
{\bf AXIOM} shell variable. That variable is renamed internally to
be {\bf \$spadroot}. The {\bf reroot} function will change the
system to use a new root directory and will have the same effect
as changing the {\bf AXIOM} shell variable and rerunning the system
from scratch.  A correct call looks like:
\begin{verbatim}
(in-package "BOOT")
(reroot "${AXIOM}")
\end{verbatim}
where the [[${AXIOM}]] variable points to installed tree.
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

;;; Sets up the system to use the {\bf AXIOM} shell variable if we can
;;; and default to the {\bf \$spadroot} variable (which was the value
;;; of the {\bf AXIOM} shell variable at build time) if we can't.
(defun initroot (&optional (newroot nil))
  (reroot (or (BOOT::|getEnv| "AXIOM") newroot $spadroot
              (error "setenv AXIOM or (setq $spadroot)"))))

;;; Gnu Common Lisp (GCL) (at least 2.6.[78]) requires some changes
;;; to the default memory setup to run Axiom efficently.
;;; This function performs those setup commands.
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
  #+:GCL
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
  #-:GCL
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
(defun build-interpsys (load-files parse-files comp-files browse-files
             asauto-files spad)
  (declare (ignore nagbr-files))
  #-:ecl
  (progn
      (mapcar #'load load-files)
      (interpsys-image-init parse-files comp-files browse-files
             asauto-files spad))
  (if (and (boundp 'FRICAS-LISP::*building-axiomsys*)
                FRICAS-LISP::*building-axiomsys*)
       (progn
           #+:GCL(setf compiler::*default-system-p* nil)
           #+:GCL(compiler::emit-fn nil)
           (setq *load-verbose* nil)
           #+:clisp(setf custom:*suppress-check-redefinition* t)
           (setf |$createLocalLibDb| t)
       )
  )
  #+:ecl
  (progn
      (setf FRICAS-LISP::*fricas-initial-lisp-objects*
           (append FRICAS-LISP::*fricas-initial-lisp-objects*
                   '("sys-pkg.o" "util.o")
                   load-files))
      (dolist (el `(
                    ("comp-files" ,comp-files)
                    ("browse-files" ,browse-files)
                    ("asauto-files" ,asauto-files)))
          (c:build-fasl (concatenate 'string spad "/autoload/" (car el))
                        :lisp-files (nth 1 el)))
      (let ((initforms nil))
          (dolist (el '(*YEARWEEK* *BUILD-VERSION* timestamp
                    |$createLocalLibDb|))
              (if (boundp el)
                  (push (list 'defparameter el (symbol-value el))
                        initforms)))
          (push `(interpsys-ecl-image-init ,spad) initforms)
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
                    (setf (symbol-values sym) t)))
     (initroot spad)
     (setf spad $spadroot)
     (format *standard-output* "spad = ~s~%" spad)
     (force-output  *standard-output*)
     ;;; (load (concatenate 'string spad "/autoload/"  "parini.lsp"))
     (interpsys-image-init
           (list (concatenate 'string spad "/autoload/" "parse-files"))
           (list (concatenate 'string spad "/autoload/" "comp-files"))
           (list (concatenate 'string spad "/autoload/" "browse-files"))
           (list (concatenate 'string spad "/autoload/" "asauto-files"))
           spad)
      (format *standard-output* "before fricas-restart~%")
      (force-output  *standard-output*)
      (fricas-restart))

(defun interpsys-image-init (parse-files comp-files browse-files
             asauto-files spad)
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
  (|setBootAutloadProperties| comp-functions comp-files)
  (|setBootAutloadProperties| browse-functions browse-files)
  (|setBootAutloadProperties| asauto-functions asauto-files)
  (resethashtables) ; the databases into core, then close the streams
 )

;;; The {\bf depsys} image is one of the two images we build from
;;; the src/interp subdirectory (the other is {\bf interpsys}). We
;;; use {\bf depsys} as a compile-time image as it contains all of
;;; the necessary functions and macros to compile any file. The
;;; {\bf depsys} image is almost the same as an {\bf interpsys}
;;; image but it does not have any autoload triggers or databases
;;; loaded.
(defun build-depsys (load-files spad build-interp-dir)
#+:GCL
  (in-package "BOOT")
  (mapcar #'load load-files)
  (make-depsys build-interp-dir)
  (initroot spad)
  #+:GCL
  (init-memory-config :cons 1000 :fixnum 400 :symbol 1000 :package 16
                      :array 800 :string 1000 :cfun 200 :cpages 2000
                      :rpages 2000 :hole 4000) )
;;  (init-memory-config :cons 500 :fixnum 200 :symbol 500 :package 8
;;                    :array 400 :string 500 :cfun 100 :cpages 1000
;;                    :rpages 1000 :hole 2000) )

(DEFUN |string2BootTree| (S) (boottran::STTOSEX S))

(defun |processSynonyms| () nil) ;;dummy def for depsys, redefined later


;; the following are for conditional reading
(setq |$opSysName| '"shell")

#|
We need a way of distinguishing different versions of the system.
There used to be a way to touch the src/timestamp file whenever
you checked in a change to the change control subsystem.
During make PART=interp (the default for make) we set timestamp
to the filename of this timestamp file. This function converts it
to a luser readable string and sets the *yearweek* variable.

The result of this function is a string that is printed as a banner
when Axiom starts. The actual printing is done by the function
[[spadStartUpMsgs]] in [[src/interp/msgdb.boot]]. It uses a
format string from the file [[src/doc/msgs/s2-us.msgs]].
|#
(defun yearweek ()
 "set *yearweek* to the current time string for the version banner"
  (declare (special timestamp) (special *yearweek*))
  (if (and (boundp 'timestamp) (probe-file timestamp))
      (let (sec min hour date month year day dayvec monvec)
        (setq dayvec '("Monday" "Tuesday" "Wednesday" "Thursday"
                       "Friday" "Saturday" "Sunday"))
        (setq monvec '("January" "February" "March" "April" "May" "June"
                       "July" "August" "September" "October" "November"
                       "December"))
        (multiple-value-setq (sec min hour date month year day)
                             (decode-universal-time
                              (file-write-date timestamp)))
        (setq *yearweek*
          (copy-seq
              (format nil "~a ~a ~d, ~d at ~2,'0d:~2,'0d:~2,'0d "
                      (elt dayvec day)
                      (elt monvec (1- month)) date year hour min sec))))
      (setq *yearweek* "no timestamp")))

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
  ((load (concat (BOOT::|getEnv| "AXIOM") "/algebra/exposed")
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
        (let* ((ax-dir (|getEnv| "AXIOM"))
               (spad-lib (concatenate 'string ax-dir "/lib/libspad.so")))
            (format t "Checking for foreign routines~%")
            (format t "AXIOM=~S~%" ax-dir)
            (format t "spad-lib=~S~%" spad-lib)
            (if (fricas-probe-file spad-lib)
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

(defun fricas-restart ()
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
