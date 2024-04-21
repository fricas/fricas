;;; Lisp file which creates shared library containing FriCAS.
;;; Works only with ECL.
;;; 
;;; To use this you need to correctly set FRICAS environment
;;; variable.  During cration of the library FRICAS should point to
;;; target/platform subdirectory of the build tree (where 'platform'
;;; is name of your platform, for example 'x86_64-linux-gnu').
;;;
;;; After creating the shared library you can load it (this
;;; time FRICAS should point to target/platform subdirectory
;;; of place where you installed FriCAS files):
#|
(load "src/interp/fricas_lib.fas")
(in-package "BOOT")
(fricas-init)
|#
;;; and use (for use example see 'load-fricas.lisp').

;;; load compiler
(defun foo () nil)
(compile 'foo)

(let ((*default-pathname-defaults*
       #P"/full_path_to_FriCAS_build_directory/src/interp/"))
     (load "../lisp/fricas-package.lisp")
)
(let ((*default-pathname-defaults*
       #P"/full_path_to_FriCAS_build_directory/src/interp/"))
     (load "../lisp/fricas-config.lisp")
     (load "../lisp/fricas-lisp")
     (load "../lisp/primitives.lisp")
     (load "../lisp/fricas-ecl.lisp")
     (load "makeint.lisp")
     (let ((initforms (reverse FRICAS-LISP::*fricas-initial-lisp-forms*)))
          (setf initforms (reverse (cdr initforms)))
          (c:build-fasl "fricas_lib"
                   :lisp-files FRICAS-LISP::*fricas-initial-lisp-objects*
                   :ld-flags FRICAS-LISP::*fricas-extra-c-files*
                   :epilogue-code initforms)
       )
 )
