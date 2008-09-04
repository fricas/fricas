;;; We put this in separate file to avoid problems with compilation.
(eval-when (:execute :compile-toplevel :load-toplevel)
(if (not (find-package "FRICAS-LISP"))
   (make-package "FRICAS-LISP" 
     :use (list (or (find-package "COMMON-LISP")
                    "LISP"))))) 
#+:sbcl
(eval-when (:execute :compile-toplevel :load-toplevel)
    (setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :LATIN-1))

(in-package "FRICAS-LISP")
;;; Aldor 1.1.0 and before produces IN-PACKAGE statements with :use options.
;;; These are not allowed in ANSI Common Lisp, so we have to provide our own
;;; IN-PACKAGE.  The :use options can actually be ignored, so we do not use
;;; (defpackage package options)
;;; (in-package package)
#-gcl
(shadow "IN-PACKAGE" "FRICAS-LISP")
#-gcl
(defmacro IN-PACKAGE (package &rest options) 
  `(COMMON-LISP:IN-PACKAGE ,package))

(do-symbols (x "FRICAS-LISP") (export (list x)))

(export '(quit chdir |getEnv| |load_quietly| get-current-directory
          fricas-probe-file trim-directory-name pad-directory-name
          file-kind makedir fricas-compile-file load-maybe-compiling
          maybe-compile
          |writeablep| |openServer| |sockGetInt|
          |sockSendInt| |sockSendString| |sockGetFloat| |sockSendFloat|
          |serverSwitch| |sockSendSignal| |sockGetStringFrom|))

#+:GCL
(progn
    (import '(LISP::LAMBDA-CLOSURE))
    (export '(LISP::LAMBDA-CLOSURE))
)
#+:ecl 
(progn
    (require 'cmp)
    (eval-when (:execute :compile-toplevel :load-toplevel)
         (proclaim '(optimize (safety 1))))
)
#+:openmcl
(eval-when (:execute :compile-toplevel :load-toplevel)
      (setf *features* (delete :CCL *features*)))
