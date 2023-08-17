(defun get-version-number ()
    (let* ((ver (lisp-implementation-version))
           (pos (position-if #'digit-char-p ver)))
          ;;; First strip leading and trailing garbage
          (if pos (setf ver (subseq ver pos))
                  (return-from get-version-number 'bad-version-string))
          (setf pos (position #\Space ver))
          (if pos (setf ver (subseq ver 0 pos)))
          ver))

(with-open-file (out "config_cl.out"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (format out "fricas_fasl_type=~a~&"
        (pathname-type (compile-file-pathname "foo.lisp")))

    (format out "fricas_lisp_flavor=~a~&"
        #+:lispworks "lispworks"
        #+:poplog "poplog"
        #+(or :gcl :ecl :openmcl :sbcl :clisp :cmu :abcl) (lisp-implementation-type)
        #-(or :gcl :ecl :openmcl :sbcl :clisp :cmu :abcl :lispworks :poplog) "unknown"
    )
    #+:clisp
    (if (not (find-package "FFI"))
        (format out "fricas_clisp_no_ffi"))
    (let ((ver (get-version-number)))
         (if (eq ver 'bad-version-string)
             (format t "Unable to determine version")
             (format out "fricas_lisp_version=~a~&" ver)))
)

#+:poplog
(pop11::sysexit)
#-:poplog
(quit)
