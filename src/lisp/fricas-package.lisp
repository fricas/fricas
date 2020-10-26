;;; We put this in separate file to avoid problems with compilation.
(make-package "FRICAS-LISP"
     :use (list (or (find-package "COMMON-LISP")
                    "LISP")))
#+:sbcl
(eval-when (:execute :compile-toplevel :load-toplevel)
    (ignore-errors (require "SB-SPROF")))

#+:cmu
(eval-when (:execute :compile-toplevel :load-toplevel)
    (setf STREAM::*DEFAULT-EXTERNAL-FORMAT* :ISO8859-1))

#+:poplog
(eval-when (:compile-toplevel :execute :load-toplevel)
      (set-syntax-from-char #\@ #\@))

(in-package "FRICAS-LISP")
;;; Aldor 1.1.0 and before produces IN-PACKAGE statements with :use options.
;;; These are not allowed in ANSI Common Lisp, so we have to provide our own
;;; IN-PACKAGE.  The :use options can actually be ignored, so we do not use
;;; (defpackage package options)
;;; (in-package package)
#+:cmu
(shadow "UNION" "FRICAS-LISP")
#+:cmu
(shadow "NUNION" "FRICAS-LISP")

#-gcl
(shadow "IN-PACKAGE" "FRICAS-LISP")
#-gcl
(defmacro IN-PACKAGE (package &rest options)
  `(COMMON-LISP:IN-PACKAGE ,package))

#+gcl
(shadow "QUIT")

(do-symbols (x "FRICAS-LISP") (export (list x)))

(export '(quit chdir |getEnv| |getCLArgs| |load_quietly| get-current-directory
          trim-directory-name pad-directory-name
          file-kind makedir fricas_compile_file fricas_compile_fasl
          |fricas_probe_file|
          DEFCONST exit-with-status MEMQ |quiet_load_alien|
          |handle_input_file| |handle_output_file| |maybe_delete_file|
          |remove_directory| |writeablep| |openServer| |sockGetInt|
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
         (proclaim '(optimize (safety 0))))
)
#+:openmcl
(eval-when (:execute :compile-toplevel :load-toplevel)
      (setf *features* (delete :CCL *features*)))

;;; Package containing Shoe to Lisp translator
(make-package "BOOTTRAN" :use '("FRICAS-LISP"))

;;; Main FriCAS package.  The interpreter and the algebra are run
;;; after switching to the boot package (in-package "BOOT") so any
;;; symbol that the interpreter or algebra uses has to appear here.
(make-package "BOOT" :use '("FRICAS-LISP"))

(in-package "BOOT")

(import '(BOOTTRAN::BOOTTOCLC BOOTTRAN::STTOSEX
          BOOTTRAN::|shoeRemovebootIfNec|))

(in-package "BOOTTRAN")

(import '(BOOT::QSETVELT BOOT::SETELT_BVEC BOOT::STR_ELT))

;;; Package containing support routines for code generated
;;; by Aldor compiler.
(make-package "FOAM" :use '("FRICAS-LISP"))

;;; Package for code output by Aldor.
(make-package "FOAM-USER" :use '("FRICAS-LISP" "FOAM"))
