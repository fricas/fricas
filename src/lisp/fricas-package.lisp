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
          |writeablep| |openServer| sock_get_int sock_send_int
          sock_get_float sock_send_float sock_send_string
          sock_send_string_len |serverSwitch| sock_send_signal
          |sockGetStringFrom|))

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
