;;; This file contains portablity and support routines which abstract away
;;; differences between Lisp dialects.

(in-package "FRICAS-LISP")
#+:sbcl
(progn
     (defvar *saved-terminal-io* *terminal-io*)
     (setf *terminal-io* (make-two-way-stream *standard-input*
                                             *standard-output*))
     (setf sb-ext:*evaluator-mode* :interpret)
 )

(defun set-initial-parameters()
    (setf *read-default-float-format* 'double-float))

#-:sbcl
(eval-when (:execute :load-toplevel)
    (set-initial-parameters))

#+:clisp
(eval-when (:execute :compile-toplevel :load-toplevel)
    ;;; clisp wants to search whole "~/lisp" subtree to find a source
    ;;; file, which is insane.  Below we disable this behaviour.
    (setf custom:*load-paths* '(#P"./"))
    ;;; We want ANSI compliance
    (setf custom:*ansi* t)
    ;;; We have our own loading messages
    (setf *LOAD-VERBOSE* nil))

;;
#+:openmcl
(progn

(defclass fricas-application (ccl::application) ())

(defvar *my-toplevel-function* nil)

(defvar *ccl-default-directory* nil)

(defmethod ccl::toplevel-function ((app fricas-application) init-file)
    (declare (ignore init-file))
        (call-next-method) ; this is critical, but shouldn't be.
        (funcall *my-toplevel-function*)
        (let ((ap (make-instance 'ccl::lisp-development-system)))
            (ccl::toplevel-function ap init-file)))

)

;; Save current image on disk as executable and quit.
(defun save-core-restart (core-image restart)
#+:GCL
  (progn
     (if restart
          (setq system::*top-level-hook* restart))
     (system::save-system core-image))
#+:allegro
  (if restart
   (excl::dumplisp :name core-image :restart-function restart)
   (excl::dumplisp :name core-image))
#+Lucid
  (if restart
   (sys::disksave core-image :restart-function restart)
   (sys::disksave core-image))
#+:sbcl
  (let* ((restart-fun 
               (if restart
                   restart
                   #'(lambda () nil)))
         (top-fun #'(lambda ()
                       (set-initial-parameters)
                       (funcall restart-fun)
                       (sb-impl::toplevel-repl nil))))
        (sb-ext::save-lisp-and-die core-image :toplevel top-fun
            :executable t))
#+:clisp
  (if restart
     (ext::saveinitmem core-image :INIT-FUNCTION restart :QUIET t
         :NORC t :executable t)
     (ext::saveinitmem core-image :executable t :NORC t :QUIET t))
#+:openmcl
  (let* ((ccl-dir (or *ccl-default-directory*
                 (|getEnv| "CCL_DEFAULT_DIRECTORY")))
         (kname (concatenate 'string ccl-dir "/"
                             (ccl::standard-kernel-name))))
        (setf *ccl-default-directory* ccl-dir)
        (if restart
            (progn
                (setf *my-toplevel-function* restart)
                (CCL::save-application core-image
                                       :PREPEND-KERNEL kname
                                       :application-class 'fricas-application))
            (CCL::save-application core-image :PREPEND-KERNEL kname))
        (quit))
#|
  (let ((ccl-dir (|getEnv| "CCL_DEFAULT_DIRECTORY"))
        (core-fname (concatenate 'string core-image ".image"))
        (eval-arg (if restart 
                      (format nil " --eval '(~A)'" restart)
                      ""))
        core-path exe-path)
        ;;; truename works only on existing files, so we
        ;;; create one just to get absolute path
        (with-open-file (ims core-fname 
                          :direction :output :if-exists :supersede)
            (declare (ignore ims))
            (setf core-path (namestring (truename core-fname))))
        (delete-file core-path)
        (with-open-file (ims core-image 
                        :direction :output :if-exists :supersede)
                (setf exe-path (namestring (truename core-image)))
                (format ims "#!/bin/sh~2%")
                (format ims "CCL_DEFAULT_DIRECTORY=~A~%" ccl-dir)
                (format ims "export CCL_DEFAULT_DIRECTORY~%")
                (format ims "exec ~A/~A -I ~A~A~%"
                            ccl-dir (ccl::standard-kernel-name)
                            core-path eval-arg))
        (ccl::run-program "chmod" (list "a+x" exe-path))
        #|
        ;;; We would prefer this version, but due to openmcl bug
        ;;; it does not work
        (if restart
          (ccl::save-application core-path :toplevel-function restart)
          (ccl::save-application core-path))
        |#
        (ccl::save-application core-path)
        )
  |#
)

(defun save-core (core-image)
     (save-core-restart core-image nil))

;; Load Lisp files (any LOADable file), given as a list of file names.
;; The file names are strings, as approrpriate for LOAD.
(defun load-lisp-files (files)
  (mapcar #'(lambda (f) (load f)) files))

;;; How to exit Lisp process
#+(and :GCL :common-lisp)
(defun quit() (lisp::quit))

#+:sbcl
(defun quit()
    (setf *terminal-io* *saved-terminal-io*)
    (sb-ext::quit))

#+:clisp
(defun quit() (ext::quit))

#+:openmcl
(defun quit() (ccl::quit))

#+:ecl
(defun quit ()
    (SI:quit))

#+:poplog
(defun quit() (poplog::bye))

;;; -----------------------------------------------------------------

;;; Making (linking) program

#-:ecl
(defun make-program (core-image lisp-files)
    (load-lisp-files lisp-files)
    (save-core core-image))

#+:ecl
(defun make-program (core-image lisp-files)
    (if *fricas-initial-lisp-forms*
        (c:build-program core-image
             :lisp-files (append *fricas-initial-lisp-objects* lisp-files)
             :ld-flags *fricas-extra-c-files*
             :epilogue-code *fricas-initial-lisp-forms*)
        (c:build-program core-image
             :lisp-files (append *fricas-initial-lisp-objects* lisp-files)
             :ld-flags *fricas-extra-c-files*))
    (quit))

;;; -----------------------------------------------------------------
;;; For ECL assume :unix, when :netbsd or :darwin
#+(and :ecl (or :darwin :netbsd)) (push :unix *features*)

;;; -----------------------------------------------------------------

;;; Chdir function

#+:GCL
(defun chdir (dir)
 (system::chdir dir))

#+:sbcl
(eval-when (:execute :compile-toplevel :load-toplevel)
    (require :sb-posix))
#+:sbcl
(defun chdir (dir)
 (let ((tdir (probe-file dir)))
  (cond
    (tdir
       #-:win32 (sb-posix::chdir tdir) 
       (setq *default-pathname-defaults* tdir))
     (t nil))))

#+(and :clisp (or :unix :win32))
(defun chdir (dir)
 (ext::cd dir))

#+:openmcl
(defun chdir (dir)
  (ccl::%chdir dir))

#+:ecl
(defun chdir (dir)
   (SI:CHDIR (pad-directory-name dir) t))

;;; Environment access

(defun |getEnv| (var-name)
  #+:GCL (system::getenv var-name)
  #+:sbcl (sb-ext::posix-getenv var-name)
  #+:clisp (ext::getenv var-name)
  #+:openmcl (ccl::getenv var-name)
  #+:ecl (si::getenv var-name)
  )

;;; Silent loading of files

(defun |load_quietly| (f)
    ;;; (format *error-output* "entred load_quietly ~&") 
    #-:GCL
    (handler-bind ((warning #'muffle-warning))
                  (load f))
    #+:GCL
    (load f)
    ;;; (format *error-output* "finished load_quietly ~&") 
)

;;; -------------------------------------------------------
;;;
;;; FriCAS FFI macros
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *c-type-to-ffi*)
(defun c-type-to-ffi (c-type)
   (let ((pp (assoc c-type *c-type-to-ffi*)))
        (if pp (nth 1 pp) (break))))

)

#+:GCL
(eval-when (:compile-toplevel :load-toplevel :execute)
(setf *c-type-to-ffi* '(
    (int LISP::int)
    (c-string LISP::string)
    (double LISP::double)
))               

(defun c-args-to-gcl (arguments)
   (declare (safety 3))
   (mapcar (lambda (x) (c-type-to-ffi (nth 1 x))) arguments))

(defun gcl-foreign-call (name c-name return-type arguments)
    (let ((gcl-args (c-args-to-gcl arguments))
          (gcl-ret (c-type-to-ffi return-type)))
    `(LISP::defentry ,name ,gcl-args (,gcl-ret ,c-name))
  ))

(defmacro fricas-foreign-call (name c-name return-type &rest arguments)
    (gcl-foreign-call name c-name return-type arguments))

)

#+(and :clisp :ffi)
(eval-when (:compile-toplevel :load-toplevel :execute)

(setf *c-type-to-ffi* '(
    (int ffi:int)
    (c-string  ffi:c-string)
    (double ffi:double-float)
))

(defun c-args-to-clisp (arguments)
   (mapcar (lambda (x) (list (nth 0 x) (c-type-to-ffi (nth 1 x)))) arguments))

(defun clisp-foreign-call (name c-name return-type arguments)
    (let ((clisp-args (c-args-to-clisp arguments))
          (clisp-ret (c-type-to-ffi return-type)))
     `(eval (quote (ffi:def-call-out ,name
          ;;; (:library "./libspad.so")
          (:name ,c-name) 
          (:arguments ,@clisp-args)
          (:return-type ,clisp-ret)
          (:language :stdc))))
     ))

(defmacro fricas-foreign-call (name c-name return-type &rest arguments)
     (clisp-foreign-call name c-name return-type arguments))

)

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)

(setf *c-type-to-ffi* '(
    (int SB-ALIEN::int)
    (c-string SB-ALIEN::c-string)
    (double SB-ALIEN::double)
))

(defun c-args-to-sbcl (arguments)
       (mapcar (lambda (x) (list (nth 0 x) (c-type-to-ffi (nth 1 x)) :in))
               arguments))

(defun sbcl-foreign-call (name c-name return-type arguments)
    (let ((sbcl-args (c-args-to-sbcl arguments))
          (sbcl-ret (c-type-to-ffi return-type)))
       `(SB-ALIEN::define-alien-routine (,c-name ,name) ,sbcl-ret
           ,@sbcl-args)))

(defmacro fricas-foreign-call (name c-name return-type &rest arguments)
       (sbcl-foreign-call name c-name return-type arguments))

)

#+:openmcl
(eval-when (:compile-toplevel :load-toplevel :execute)

(setf *c-type-to-ffi* '(
    (int :int)
    (c-string :address)
    (double :double-float)
))

(defun c-args-to-openmcl (arguments)
   (let ((strs nil) (fargs nil))
        (mapcar (lambda (x)
                         (if (eq (nth 1 x) 'c-string)
                             (let ((sym (gensym)))
                                   (push (list sym (nth 0 x)) strs)
                                   (push :address fargs)
                                   (push sym fargs))
                             (progn
                                  (push (c-type-to-ffi (nth 1 x)) fargs)
                                  (push (nth 0 x) fargs))))
                     arguments)
         (values (nreverse fargs) strs 
                 (mapcar #'car arguments))))

(defun openmcl-foreign-call (name c-name return-type arguments)
    (multiple-value-bind (fargs strs largs) (c-args-to-openmcl arguments)
        (let* ((l-ret (c-type-to-ffi return-type))
               (call-body
                 `(ccl::external-call ,c-name ,@fargs ,l-ret))
               (fun-body
                  (if strs
                     `(ccl::with-cstrs ,strs ,call-body)
                      call-body)))
               `(defun ,name ,largs ,fun-body))))
            
(defmacro fricas-foreign-call (name c-name return-type &rest arguments)
     (openmcl-foreign-call name c-name return-type arguments))

)

#+:ecl
(eval-when (:compile-toplevel :load-toplevel :execute)

(setf *c-type-to-ffi* '(
                 (int :int)
                 (c-string  :cstring )
                 (double :double)
                 ))

(defun c-args-to-ecl (arguments)
    (let ((strs nil) (fargs nil))
        (mapcar (lambda (x)
                        (if (eq (nth 1 x) 'c-string)
                            (let ((sym (gensym)))
                                (push (list sym (nth 0 x)) strs)
                                (push (list sym :cstring) fargs))
                            (push (list (nth 0 x) (c-type-to-ffi (nth 1 x)))
                                  fargs)))
                arguments)
        (values (nreverse fargs) strs)))

(defun ecl-foreign-call (name c-name return-type arguments)
    (multiple-value-bind (fargs strs) (c-args-to-ecl arguments)
        (let ((l-ret (c-type-to-ffi return-type))
               wrapper)
            (if strs
                (let* ((sym (gensym))
                       (wargs (mapcar #'car fargs))
                       (largs (mapcar #'car arguments))
                       (wrapper `(,sym ,@wargs)))
                    (dolist (el strs)
                        (setf wrapper `(FFI:WITH-CSTRING ,el ,wrapper)))
                    (setf wrapper `(defun ,name ,largs ,wrapper))
                    `(progn (uffi:def-function (,c-name ,sym)
                                ,fargs :returning ,l-ret)
                            ,wrapper))
                `(uffi:def-function (,c-name ,name)
                     ,fargs :returning ,l-ret)))))

(defmacro fricas-foreign-call (name c-name return-type &rest arguments)
    (ecl-foreign-call name c-name return-type arguments))

)

;;;
;;; Foreign routines
;;;

(defmacro foreign-defs (&rest arguments)
    #-:clisp `(progn ,@arguments)
    #+(and :clisp :ffi) `(defun clisp-init-foreign-calls () ,@arguments)
)

(foreign-defs

(fricas-foreign-call |writeablep| "writeablep" int
        (filename c-string))

(fricas-foreign-call |openServer| "open_server" int
        (server_name c-string))

(fricas-foreign-call |sockGetInt| "sock_get_int" int
        (purpose int))

(fricas-foreign-call |sockSendInt| "sock_send_int" int
        (purpose int)
        (val int))

#+:GCL
(LISP::clines "extern double sock_get_float();")

(fricas-foreign-call |sockGetFloat| "sock_get_float" double
        (purpose int))

(fricas-foreign-call |sockSendFloat| "sock_send_float" int
       (purpose int)
       (num double))

;;; (fricas-foreign-call |sockSendString| "sock_send_string" int
;;;       (purpose int)
;;;       (str c-string))

(fricas-foreign-call sock_send_string_len "sock_send_string_len" int
       (purpose int)
       (str c-string)
       (len int))

(defun |sockSendString| (purpose str)
     (sock_send_string_len purpose str (length str)))

(fricas-foreign-call |serverSwitch| "server_switch" int)

(fricas-foreign-call |sockSendSignal| "sock_send_signal" int
       (purpose int)
       (sig int))

#+:GCL
(progn

(LISP::defentry sock_get_string_buf (LISP::int LISP::object LISP::int)
    (LISP::int "sock_get_string_buf_wrapper"))

;; GCL may pass strings by value.  'sock_get_string_buf' should fill
;; string with data read from connection, therefore needs address of
;; actual string buffer. We use 'sock_get_string_buf_wrapper' to
;; resolve the problem
(LISP::clines "int sock_get_string_buf_wrapper(int i, object x, int j)"
    "{ if (type_of(x)!=t_string) FEwrong_type_argument(sLstring,x);"
    "  if (x->st.st_fillp<j)"
    "    FEerror(\"string too small in sock_get_string_buf_wrapper\",0);"
    "  return sock_get_string_buf(i, x->st.st_self, j); }")

(LISP::defentry sock_get_string_buf (LISP::int LISP::object LISP::int)
    (LISP::int "sock_get_string_buf_wrapper"))

(defun |sockGetStringFrom| (type)
    (let ((buf (MAKE-STRING 10000)))
        (sock_get_string_buf type buf 10000)
            buf))

)
#+(and :clisp :ffi)
(eval '(FFI:DEF-CALL-OUT sock_get_string_buf
    (:NAME "sock_get_string_buf")
    (:arguments (purpose ffi:int)
    (buf (FFI:C-POINTER (FFI:C-ARRAY FFI::char 10000)))
    (len ffi:int))
    (:return-type ffi:int)
    (:language :stdc)))

)

#+(and :clisp :ffi)
(defun |sockGetStringFrom| (purpose)
    (let ((buf nil))
        (FFI:WITH-C-VAR (tmp-buf '(FFI:C-ARRAY
                                   FFI::char 10000))
            (sock_get_string_buf purpose (FFI:C-VAR-ADDRESS tmp-buf) 10000)
            (prog ((len2 10000))
                (dotimes (i 10000)
                    (if (eql 0 (FFI:ELEMENT tmp-buf i))
                        (progn
                            (setf len2 i)
                            (go nn1))))
              nn1
                (setf buf (make-string len2))
                (dotimes (i len2)
                    (setf (aref buf i)
                          (code-char (FFI:ELEMENT tmp-buf i)))))
        )
        buf
    )
)

#+:openmcl
(defun |sockGetStringFrom| (purpose)
    (ccl::%stack-block ((tmp-buf 10000))
        (ccl::external-call "sock_get_string_buf"
            :int purpose :address tmp-buf :int 10000)
        (ccl::%get-cstring tmp-buf)))

#+:sbcl
(defun |sockGetStringFrom| (purpose)
    (let ((buf nil))
        (SB-ALIEN::with-alien ((tmp-buf (SB-ALIEN::array
                                         SB-ALIEN::char 10000)))
            (SB-ALIEN::alien-funcall
                (SB-ALIEN::extern-alien
                    "sock_get_string_buf"
                        (SB-ALIEN::function SB-ALIEN::void
                            SB-ALIEN::int
                            (SB-ALIEN::* SB-ALIEN::char)
                            SB-ALIEN::int))
                purpose
                (SB-ALIEN::addr (SB-ALIEN::deref tmp-buf 0))
                10000)
            (prog ((len2 10000))
                (dotimes (i 10000)
                    (if (eql 0 (SB-ALIEN::deref tmp-buf i))
                        (progn
                            (setf len2 i)
                            (go nn1))))
              nn1
                (setf buf (make-string len2))
                (dotimes (i len2)
                    (setf (aref buf i)
                        (code-char (SB-ALIEN::deref tmp-buf i))))
            )
        )
        buf
    )
)

#+:ecl
(progn

(uffi:def-function ("sock_get_string_buf" sock_get_string_buf_wrapper)
                   ((purpose :int) (buf (:array :unsigned-char 10000)) (len :int))
                   :returning :void)

(defun |sockGetStringFrom| (purpose)
    (uffi:with-foreign-object (buf '(:array :unsigned-char 10000))
        (sock_get_string_buf_wrapper purpose buf 10000)
        (uffi:convert-from-foreign-string buf)))

)
      
;;; -------------------------------------------------------
;;; File and directory support
;;; First version contributed by Juergen Weiss.

#+:GCL
(progn
  (LISP::defentry file_kind (LISP::string)      (LISP::int "directoryp"))
  (LISP::defentry |makedir| (LISP::string)         (LISP::int "makedir")))

#+:ecl
(uffi:def-function ("directoryp" raw_file_kind)
                   ((arg :cstring))
                   :returning :int)
#+:ecl
(defun file_kind (name)
      (FFI:WITH-CSTRING (cname name)
           (raw_file_kind cname)))

#+:ecl
(uffi:def-function ("makedir" raw_makedir)
                   ((arg :cstring))
                   :returning :int)

#+:ecl
(defun |makedir| (name)
      (FFI:WITH-CSTRING (cname name)
          (raw_makedir cname)))

(defun trim-directory-name (name)
    #+(or :unix :win32)
    (if (char= (char name (1- (length name))) #\/)
        (setf name (subseq name 0 (1- (length name)))))
    name)

(defun pad-directory-name (name)
   #+(or :unix :win32)
   (if (char= (char name (1- (length name))) #\/)
       name
       (concatenate 'string name "/"))
   #-(or :unix :win32)
       (error "Not Unix and not Windows, what system it is?")
    )

;;; Make directory

#+(or :GCL :ecl)
(defun makedir (fname) (|makedir| fname))

#+:sbcl
(defun makedir (fname)
    (sb-ext::run-program "mkdir" (list fname) :search t))

#+:openmcl
(defun makedir (fname)
    (ccl::run-program "mkdir" (list fname)))

#+:clisp
(defun makedir (fname)
    (ext:make-dir (pad-directory-name (namestring fname))))

;;;

(defun file-kind (filename)
   #+(or :GCL :ecl) (file_kind filename)
   #+:sbcl (case (sb-unix::unix-file-kind filename)
                 (:directory 1)
                 ((nil) -1)
               (t 0))
   #+:openmcl (if (ccl::directoryp filename)
                  1
                  (if (probe-file filename)
                      0
                     -1))
   #+:clisp (let* ((fname (trim-directory-name (namestring filename)))
                   (dname (pad-directory-name fname)))
             (if (ignore-errors (truename dname))
                 1
                 (if (ignore-errors (truename fname))
                     0
                     -1)))
   )
 
#+:cmu
(defun get-current-directory ()
  (namestring (extensions::default-directory)))

#+(or :ecl :GCL :sbcl :clisp :openmcl)
(defun get-current-directory ()
    (trim-directory-name (namestring (truename ""))))

#+:poplog
(defun get-current-directory ()
   (let ((name (namestring (truename "."))))
        (trim-directory-name (subseq name 0 (1- (length name))))))

(defun fricas-probe-file (file)
#|
#+:GCL (if (fboundp 'system::stat)
           ;;; gcl-2.6.8
           (and (system::stat file) (truename file))
           ;;; gcl-2.6.7
           (probe-file file))
|#
#+:GCL (let* ((fk (file-kind (namestring file)))
              (fname (trim-directory-name (namestring file)))
              (dname (pad-directory-name fname)))
           (cond
             ((equal fk 1)
                (truename dname))
             ((equal fk 0)
               (truename fname))
             (t nil)))
#+:sbcl (if (sb-unix::unix-file-kind file) (truename file))
#+(or :openmcl :ecl) (probe-file file)
#+:clisp(let* ((fname (trim-directory-name (namestring file)))
               (dname (pad-directory-name fname)))
                 (or (ignore-errors (truename dname))
                     (ignore-errors (truename fname))))
         )

(defun relative-to-absolute (name)
    (let ((ns (namestring name)))
         (if (and (consp (pathname-directory name))
                  (eq (car (pathname-directory name))
                      #-:GCL :absolute #+:GCL :root))
             ns
             (concatenate 'string (get-current-directory)  "/" ns))))

;;; Saner version of compile-file
#+:ecl
(defun fricas-compile-file (f &key output-file)
    (if output-file
        (compile-file f :output-file (relative-to-absolute output-file)
                        :system-p t)
        (compile-file f :system-p t)))
#-:ecl
(defun fricas-compile-file (f &key output-file)
    (if output-file
        (compile-file f :output-file (relative-to-absolute output-file))
        (compile-file f)))

(defun maybe-compile (f cf)
    (if (or (not (probe-file cf))
            (< (file-write-date cf) (file-write-date f)))
        (fricas-compile-file f :output-file cf)))

(defun load-maybe-compiling (f cf)
         (maybe-compile f cf)
         (load #-:ecl cf #+:ecl f))

(defmacro DEFCONST (name value)
   `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)))

