;;; We put this in separate file to avoid problems with compilation.
(eval-when (:execute :compile-toplevel :load-toplevel)
(if (not (find-package "AXIOM-LISP"))
   (make-package "AXIOM-LISP" 
     :use (list (or (find-package "COMMON-LISP")
                    "LISP"))))) 
#+:sbcl
(eval-when (:execute :compile-toplevel :load-toplevel)
    (setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :LATIN-1))

(in-package "AXIOM-LISP")
(do-symbols (x "AXIOM-LISP") (export (list x)))

(export '(quit chdir |getEnv| |load_quietly| get-current-directory
          axiom-probe-file trim-directory-name pad-directory-name
          file-kind makedir axiom-compile-file load-maybe-compiling))

#+:GCL
(progn
    (import '(LISP::LAMBDA-CLOSURE))
    (export '(LISP::LAMBDA-CLOSURE))
    (import '(DEFPACKAGE::DEFPACKAGE))
    (export '(DEFPACKAGE::DEFPACKAGE))
)
#+:ecl 
(progn
    (require 'cmp)
    ;;; Currently build using ecl fails at safety 0
    (eval-when (:execute :compile-toplevel :load-toplevel)
         (proclaim '(optimize (safety 1))))
)
