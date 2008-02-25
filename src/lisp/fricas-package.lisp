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
