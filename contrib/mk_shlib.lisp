;;; Lisp file which creates shared library contaning FriCAS.
;;; Works with ECL.
;;;
;;; After creating the shared library you can load it:
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
