;;; This file contains portablity and support routines which abstract away
;;; differences between Lisp dialects.

#+gcl
(si::clines "
#define GCL_SOURCE
#include \"bsdsignal.c\"
#include \"cfuns-c.c\"
#include \"sockio-c.c\"
")

(in-package "FRICAS-LISP")

#+:cmu
(progn
     (defvar *saved-terminal-io* *terminal-io*)
     (setf *terminal-io* (make-two-way-stream *standard-input*
                                             *standard-output*))
 )

#+:sbcl
(progn
  (defvar *saved-terminal-io* *terminal-io*)
  (setf *terminal-io* (make-two-way-stream *standard-input* *standard-output*))
  (setf sb-ext:*invoke-debugger-hook*
        (lambda (sb-cond sb-hook)
          (setf *terminal-io* *saved-terminal-io*)))
  (setf sb-ext:*evaluator-mode* :interpret))

#-:cmu
(defun set-initial-parameters()
    (setf *print-circle* t)
    (setf *compile-print* nil)
    (setf *read-default-float-format* 'double-float))

#+:cmu
(defun set-initial-parameters()
  (setq debug:*debug-print-length* 1024)
  (setq debug:*debug-print-level* 1024)
  ;; prevent error when computing small exponentials, but also prevent
  ;; computing very large exponentials during compiler optimization
  ;; (see function "safe-expt" in "cmucl/src/compiler/float-tran.lisp")
  (setq extensions:*intexp-maximum-exponent* (- most-positive-fixnum 64))
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
    (setf *LOAD-VERBOSE* nil)
    (let ((reopen-sym (find-symbol "*REOPEN-OPEN-FILE*" "CUSTOM")))
         (if reopen-sym (setf (symbol-value reopen-sym) nil))))

#|
GCL keeps noting the fact that the compiler is performing tail-recursion.
Bill Schelter added this as a debugging tool for Axiom and it was never
removed. Patching the lisp code in the GCL build fails as the system
is actually built from the pre-compiled C code. Thus, we can only step
on this message after the fact. The cmpnote function is used nowhere
else in GCL so stepping on the function call seems best. We're unhappy
with this hack and will try to convince the GCL crowd to fix this.
|#
#+gcl(setq compiler::*suppress-compiler-notes* t)

;;
#+:openmcl
(progn
(defvar *ccl-default-directory* nil)
;;; Disable default argument processing
(defmethod ccl::process-application-arguments
    ((app ccl::application) error-flag opts args) nil)
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
#+:cmu
  (let* ((restart-fun
               (if restart
                   restart
                   #'(lambda () nil)))
         (top-fun #'(lambda ()
                       (set-initial-parameters)
                       (funcall restart-fun)
                       (lisp::%top-level))))
        (ext::save-lisp
         (unix::unix-maybe-prepend-current-directory core-image)
         :init-function top-fun :executable t :print-herald nil))
#+:sbcl
  (let* ((restart-fun
               (if restart
                   restart
                   #'(lambda () nil)))
         (top-fun #'(lambda ()
                       (set-initial-parameters)
                       (funcall restart-fun)
                       (sb-impl::toplevel-repl nil)))
         (save-options-keyword (find-symbol "SAVE-RUNTIME-OPTIONS" "KEYWORD"))
         (save-options-arg
             (if save-options-keyword (list save-options-keyword t) nil))
        )
        (uninstall-gmp-multiplication)
        (apply #'sb-ext::save-lisp-and-die
              (append `(,core-image :toplevel ,top-fun :executable t)
                      save-options-arg))
  )
#+:clisp
  (if restart
     (ext::saveinitmem core-image :INIT-FUNCTION restart :QUIET t
         :NORC t :executable t)
     (ext::saveinitmem core-image :executable t :NORC t :QUIET t))
#+:openmcl
  (let* ((ccl-dir (or *ccl-default-directory*
                 (|getEnv| "CCL_DEFAULT_DIRECTORY")))
         (restart-fun
               (if restart
                   restart
                   #'(lambda () nil)))
         (top-fun #'(lambda ()
                       (set-initial-parameters)
                       (funcall restart-fun)
                       (ccl::toplevel-loop))))
        (setf *ccl-default-directory* ccl-dir)
        (CCL::save-application core-image :toplevel-function top-fun
                                       :PREPEND-KERNEL t)
        (QUIT))
#+:lispworks
  (progn
    ; LispWorks by default loads a siteinit and an init file.
    ; It complains when saving an image, if init files are already loaded.
    ; LispWorks can be started with  lispworks -siteinit - -init -  and
    ; then doesn't load these init files. That would mean changing
    ; the FriCAS scripts. Here we let LispWorks load the init files,
    ; but use the undocumented system variable preventing LispWorks
    ; from presenting an error.
    (setf SYSTEM::*COMPLAIN-ABOUT-INIT-FILE-LOADED* nil)
    (if restart
        (hcl:save-image core-image :restart-function restart)
      (hcl:save-image core-image)))
)

(defun save-core (core-image)
     (save-core-restart core-image nil))

;; Load Lisp files (any LOADable file), given as a list of file names.
;; The file names are strings, as appropriate for LOAD.
(defun load-lisp-files (files)
  (mapcar #'(lambda (f) (load f)) files))

;;; How to exit Lisp process
#+:GCL
(defun |exit_with_status| (s) (SI::quit s))

#+:cmu
(defun |exit_with_status| (s)
    (setf *terminal-io* *saved-terminal-io*)
    (unix:unix-exit s))

#+:sbcl
(defun |exit_with_status| (s)
    (setf *terminal-io* *saved-terminal-io*)
    (sb-ext::quit :UNIX-STATUS s))

#+:clisp
(defun |exit_with_status| (s) (ext::quit s))

#+:openmcl
(defun |exit_with_status| (s) (ccl::quit s))

#+:ecl
(defun |exit_with_status| (s)
    (SI:quit s))

#+:lispworks
(defun |exit_with_status| (s)
  (lispworks:quit :status s))

#+:abcl
(defun |exit_with_status| (s)
  (ext:quit :status s))

#+:poplog
(defun |exit_with_status| (s)
    (pop11::sysexit1 s))

(defun QUIT() (|exit_with_status| 0))

;;; -----------------------------------------------------------------

;;; Making (linking) program

#-:ecl
(defun make-program (core-image lisp-files)
  #+gcl(setq si::*optimize-maximum-pages* nil)
  (load-lisp-files lisp-files)
  #+:gcl(progn (setq si::*code-block-reserve* "")(si::gbc t)(setq si::*code-block-reserve* (make-array 10000000 :element-type (quote character) :static t) si::*optimize-maximum-pages* t))
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
    (QUIT))

;;; -----------------------------------------------------------------
;;; For ECL assume :unix, when :netbsd or :darwin
#+(and :ecl (or :darwin :netbsd)) (push :unix *features*)

;;; -----------------------------------------------------------------

;;; Deleting files ignoring errors

(defun |maybe_delete_file| (file)
  (ignore-errors (delete-file file))
)

;;; Chdir function

#+:GCL
(defun CHDIR (dir)
 (system::chdir dir))

#+:cmu
(defun CHDIR (dir)
 (let ((tdir (probe-file dir)))
  (cond
    (tdir
       (unix::unix-chdir dir)
       (setq *default-pathname-defaults* tdir))
     (t nil))))

#+:sbcl
(eval-when (:execute :compile-toplevel :load-toplevel)
    (require :sb-posix))
#+:sbcl
(defun CHDIR (dir)
 (let ((tdir (probe-file dir)))
  (cond
    (tdir
       #-:win32 (sb-posix::chdir tdir)
       (setq *default-pathname-defaults* tdir))
     (t nil))))

#+(and :clisp (or :unix :win32))
(defun CHDIR (dir)
 (ext::cd dir))

#+:openmcl
(defun CHDIR (dir)
  (ccl::%chdir dir))

#+:ecl
(defun CHDIR (dir)
   (SI:CHDIR (|pad_directory_name| dir) t))

#+:lispworks
(defun CHDIR (dir)
  (hcl:change-directory dir))

;;; Environment access

(defun |getEnv| (var-name)
  #+:GCL (system::getenv var-name)
  #+:cmu (cdr (ext::assq (intern var-name "KEYWORD" )  ext:*environment-list*))
  #+:sbcl (sb-ext::posix-getenv var-name)
  #+:clisp (ext::getenv var-name)
  #+:openmcl (ccl::getenv var-name)
  #+:ecl (si::getenv var-name)
  #+:poplog (let ((pres (POP11::systranslate var-name)))
                (if (stringp pres) pres))
  #+:lispworks (lispworks:environment-variable var-name)
  #+:abcl (ext:getenv var-name)
  )

;;; Command-line arguments

(defun |getCLArgs| ()
  #+:GCL si::*command-args*
  #+:cmu extensions:*command-line-strings*
  #+:sbcl sb-ext::*posix-argv*
  #+:clisp (coerce (ext:argv) 'list)
  #+:openmcl ccl::*COMMAND-LINE-ARGUMENT-LIST*
  #+:ecl
    (let ((n (SI:ARGC)) (res nil))
        (dotimes (i n) (push (SI:ARGV (- n (+ i 1))) res))
        res)
  #+:poplog '()
  #+:lispworks system:*line-arguments-list*
  #+:abcl ext:*command-line-argument-list*
  )

;;; Silent loading of files

(defun |load_quietly| (f)
    ;;; (format *error-output* "entered load_quietly ~&")
    (handler-bind ((warning #'muffle-warning))
                  (load f))
    ;;; (format *error-output* "finished load_quietly ~&")
)

(defun |quiet_load_alien|(s)
    #+:sbcl
    (handler-bind ((style-warning #'muffle-warning))
          (sb-alien::load-shared-object s))
    #+:openmcl
    (ccl::open-shared-library s)
    #+:lispworks
    (fli:register-module s)
    #+:cmu
    (ext:load-foreign s)
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

(defvar *c_type_as_string* '(
    (int "int")
    (c-string "char *")
    (double "double")
    (char-* "char *")
))

(defun c_type_as_string(c_type) (nth 1 (assoc c_type *c_type_as_string*)))

(defun c_args_as_string  (arguments)
    (cond
        ((null arguments) "")
        (t (let ((res (c_type_as_string (nth 1 (car arguments)))))
              (dolist (el (rest arguments))
                  (setf res (concatenate 'string res ", "
                             (c_type_as_string (nth 1 el)))))
              res)))
)

(defun make_extern (return-type c-name arguments)
    (concatenate 'string "extern " (c_type_as_string return-type) " "
                 c-name "(" (c_args_as_string arguments) ");"))

#+:GCL
(eval-when (:compile-toplevel :load-toplevel :execute)
(setf *c-type-to-ffi* '(
    (int SI::int)
    (c-string SI::string)
    (double SI::double)
))

(defun c-args-to-gcl (arguments)
   (mapcar (lambda (x) (c-type-to-ffi (nth 1 x))) arguments))

(defun gcl-foreign-call (name c-name return-type arguments)
    (let ((gcl-args (c-args-to-gcl arguments))
          (gcl-ret (c-type-to-ffi return-type)))
    `(SI::defentry ,name ,gcl-args (,gcl-ret ,c-name))
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
    (char-* ffi:c-pointer)
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

#+:cmu
(eval-when (:compile-toplevel :load-toplevel :execute)

(setf *c-type-to-ffi* '(
    (int c-call:int)
    (c-string c-call:c-string)
    (double c-call:double)
    (char-* (alien:* c-call:char))
))

(defun c-args-to-cmucl (arguments)
  (mapcar (lambda (x) (list (nth 0 x) (c-type-to-ffi (nth 1 x))))
          arguments))

(defun cmucl-foreign-call (name c-name return-type arguments)
    (let ((cmucl-args (c-args-to-cmucl arguments))
          (cmucl-ret (c-type-to-ffi return-type)))
       `(alien:def-alien-routine (,c-name ,name) ,cmucl-ret
           ,@cmucl-args)))

(defmacro fricas-foreign-call (name c-name return-type &rest arguments)
       (cmucl-foreign-call name c-name return-type arguments))

)

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)

(setf *c-type-to-ffi* '(
    (int SB-ALIEN::int)
    (c-string SB-ALIEN::c-string)
    (double SB-ALIEN::double)
    (char-* (sb-alien:* sb-alien:char))
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
    (char-* :address)
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
                 (c-string  :cstring)
                 (double :double)
                 (char-* :pointer-void)
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
        (let ((l-ret (c-type-to-ffi return-type)))
          `(progn
            (ext:with-backend :c/c++
               (FFI:clines ,(make_extern return-type c-name arguments)))
            ,(if strs
                (let ((sym (gensym))
                      (wargs (mapcar #'car fargs))
                      (largs (mapcar #'car arguments)))
                    `(progn (ffi:def-function (,c-name ,sym)
                                ,fargs :returning ,l-ret)
                            (defun ,name ,largs
                                (ffi:with-cstrings ,strs
                                    (,sym ,@wargs)))))
                `(ffi:def-function (,c-name ,name)
                     ,fargs :returning ,l-ret))))))

(defmacro fricas-foreign-call (name c-name return-type &rest arguments)
    (ecl-foreign-call name c-name return-type arguments))

)

#+(or :poplog :abcl)
(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro fricas-foreign-call (name c-name return-type &rest arguments)
    nil)

)
;; LispWorks FFI interface

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)

(setf *c-type-to-ffi*
      '((int      :int)
        (c-string (:reference-pass :ef-mb-string))
        (double   :double)
        (char-*   :pointer)
        ))

(defun c-args-to-lispworks (arguments)
  (mapcar (lambda (x) (list (nth 0 x) (c-type-to-ffi (nth 1 x))))
          arguments))

(defun lispworks-foreign-call (name c-name return-type arguments)
  (let ((lispworks-args (c-args-to-lispworks arguments))
        (lispworks-ret (c-type-to-ffi return-type)))
    `(fli::define-foreign-function (,name ,c-name)
         ,lispworks-args
       :result-type ,lispworks-ret)))

(defmacro fricas-foreign-call (name c-name return-type &rest arguments)
  (lispworks-foreign-call name c-name return-type arguments))

)

;;;
;;; Foreign routines
;;;

(defmacro foreign-defs (&rest arguments)
    #-(or :clisp :cmu) `(progn ,@arguments)
    #+(and :clisp :ffi)
    `(defun clisp-init-foreign-calls () ,@arguments)
    #+:cmu
    `(defun cmu-init-foreign-calls ()
        (eval (quote (progn ,@arguments))))
)

(foreign-defs

(fricas-foreign-call |writeablep| "writeablep" int
        (filename c-string))

#+:fricas_has_remove_directory
(fricas-foreign-call |remove_directory| "remove_directory" int
        (dir_name c-string))

(fricas-foreign-call |openServer| "open_server" int
        (server_name c-string))

(fricas-foreign-call |sockGetInt| "sock_get_int" int
        (purpose int))

(fricas-foreign-call |sockSendInt| "sock_send_int" int
        (purpose int)
        (val int))

(fricas-foreign-call |sockGetFloat| "sock_get_float" double
        (purpose int))

(fricas-foreign-call |sockSendFloat| "sock_send_float" int
       (purpose int)
       (num double))

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

#-:gcl
(fricas-foreign-call sock_get_string_buf "sock_get_string_buf" int
       (purpose int)
       (buf char-*)
       (len int))

)

#+:GCL
(progn

;; GCL may pass strings by value.  'sock_get_string_buf' should fill
;; string with data read from connection, therefore needs address of
;; actual string buffer. We use 'sock_get_string_buf_wrapper' to
;; resolve the problem
(SI::clines "fixnum sock_get_string_buf_wrapper(fixnum i, object x, fixnum j)"
    "{ if (!vectorp(x)) FEerror(\"not a string ->~s<-\",1,x);/*FIXME no stringp, and 2.7.0 only has simple_string*/"
    "  if (length(x)<j)"
    "    FEerror(\"string too small in sock_get_string_buf_wrapper ~s\",1,list(3,x,make_fixnum((fixnum)x),make_fixnum(j)));"
    "  return (fixnum)sock_get_string_buf(i, x->st.st_self, j); }")

(SI::defentry sock_get_string_buf (SI::fixnum SI::object SI::fixnum)
    (SI::fixnum "sock_get_string_buf_wrapper"))

(defun |sockGetStringFrom| (type)
    (let ((buf (MAKE-STRING 10000)))
        (sock_get_string_buf type buf 10000)
            buf))

)

#+(and :clisp :ffi)
(defun |sockGetStringFrom| (purpose)
    (ffi:with-foreign-object (buf '(ffi:c-array-max ffi:character 10000))
        (sock_get_string_buf purpose buf 10000)
        (ffi:foreign-value buf)))

#+:openmcl
(defun |sockGetStringFrom| (purpose)
    (ccl:%stack-block ((buf 10000))
        (sock_get_string_buf purpose buf 10000)
        (ccl:%get-cstring buf)))

#+:cmu
(defun |sockGetStringFrom| (purpose)
    (alien:with-alien ((buf (alien:array c-call:char 10000)))
        (sock_get_string_buf purpose (alien:addr (alien:deref buf 0)) 10000)
        (alien:cast buf c-call:c-string)))

#+:sbcl
(defun |sockGetStringFrom| (purpose)
  (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 10000)))
    (sock_get_string_buf purpose (sb-alien:addr (sb-alien:deref buf 0)) 10000)
    (sb-alien:cast buf sb-alien:c-string)))

#+:ecl
(defun |sockGetStringFrom| (purpose)
    (ffi:with-foreign-object (buf '(:array :unsigned-char 10000))
        (sock_get_string_buf purpose buf 10000)
        (ffi:convert-from-foreign-string buf)))

#+:lispworks
(defun |sockGetStringFrom| (purpose)
    (fli:with-dynamic-foreign-objects ((buf (:ef-mb-string :limit 10000)))
        (sock_get_string_buf purpose buf 10000)
        (fli:convert-from-foreign-string buf)))


;;; -------------------------------------------------------
;;; File and directory support
;;; First version contributed by Juergen Weiss.

#+ecl
(progn

  (fricas-foreign-call file_kind "directoryp" int
                   (arg c-string))

  (fricas-foreign-call |makedir| "makedir" int
                   (arg c-string))
)

(defun |append_directory_name| (dir name)
  (concatenate 'string (|trim_directory_name| dir) "/"
	       (if (char= #\/ (char name 0)) (subseq name 1) name)))

(defun |trim_directory_name| (name)
    #+(or :unix :win32)
    (if (when (> (length name) 0) (char= (char name (1- (length name))) #\/))
        (subseq name 0 (1- (length name)))
        name)
    #-(or :unix :win32)
    (error "Not Unix and not Windows, what system it is?"))

(defun |pad_directory_name| (name)
   #+(or :unix :win32)
   (if (when (> (length name) 0) (char= (char name (1- (length name))) #\/))
       name
       (concatenate 'string name "/"))
   #-(or :unix :win32)
       (error "Not Unix and not Windows, what system it is?")
    )

;;; Make directory

#+gcl
(defun |makedir| (fname) (si::mkdir fname))

#+(or :abcl :cmu :lispworks :openmcl)
(defun |makedir| (fname)
    (|run_program| "mkdir" (list fname)))

#+:sbcl
(defun |makedir| (fname)
    (sb-unix:unix-mkdir fname #o777))

#+:clisp
(defun |makedir| (fname)
  ;; ext:make-dir was deprecated in clisp-2.44-2008-02-02
  ;; and removed in clisp-2.49.90-2018-02-11
  (let ((sym (or (find-symbol "MAKE-DIRECTORY" "EXT")
                 (find-symbol "MAKE-DIR" "EXT"))))
    (funcall sym (|pad_directory_name| (namestring fname)))))

;;;

#+:sbcl
(defmacro sbcl-file-kind(x)
    (let ((file-kind-fun
            (or (find-symbol "NATIVE-FILE-KIND" :sb-impl)
                (find-symbol "UNIX-FILE-KIND" :sb-unix))))
         `(,file-kind-fun ,x)))

#+gcl
(defun file_kind (fname) (case (si::stat fname) (:directory 1) ((nil) -1) (otherwise 0)))

(defun |file_kind| (filename)
   #+(or :GCL :ecl) (file_kind filename)
   #+:cmu
           (case (unix:unix-file-kind filename)
                (:directory 1)
                ((nil) -1)
                (t 0))
   #+:sbcl
           (case (sbcl-file-kind filename)
                (:directory 1)
                ((nil) -1)
                (t 0))
   #+:openmcl (if (ccl::directoryp filename)
                  1
                  (if (probe-file filename)
                      0
                     -1))
   #+:clisp (let* ((fname (|trim_directory_name| (namestring filename)))
                   (dname (|pad_directory_name| fname)))
             (if (ignore-errors (ext:probe-directory dname))
                 1
                 (if (ignore-errors (probe-file fname))
                     0
                     -1)))
   #+:abcl
       (if (ext:file-directory-p filename)
           1
         (if (probe-file filename) 0 -1))
   #+:lispworks
       (if (lispworks:file-directory-p filename)
           1
         (if (probe-file filename) 0 -1))
)

#+:cmu
(defun |get_current_directory| ()
  (multiple-value-bind (win dir) (unix::unix-current-directory)
                       (declare (ignore win))  dir))

#+(or :ecl :GCL :sbcl :clisp :openmcl :abcl)
(defun |get_current_directory| ()
    (|trim_directory_name| (namestring (truename ""))))

#+:poplog
(defun |get_current_directory| ()
   (let ((name (namestring (truename "."))))
        (|trim_directory_name| (subseq name 0 (1- (length name))))))

#+lispworks
(defun |get_current_directory| ()
  (let ((directory (namestring (system:current-directory))))
    (|trim_directory_name| directory)))


(defun |fricas_probe_file0| (file)
#+(or :GCL :clisp)
       (let* ((fk (|file_kind| (namestring file)))
              (fname (|trim_directory_name| (namestring file)))
              (dname (|pad_directory_name| fname)))
           (cond
             ((equal fk 1)
                (truename dname))
             ((equal fk 0)
               (truename fname))
             (t nil)))
#+:cmu (if (unix:unix-file-kind file) (truename file))
#+:sbcl (if (sbcl-file-kind file) (truename file))
#+(or :abcl :ecl :lispworks :openmcl :poplog) (probe-file file)
         )

(defun |fricas_probe_file| (file)
     (let ((path (|fricas_probe_file0| file)))
          (if path (namestring path)
              nil)))

#-:cmu
(defun relative-to-absolute (name)
    (let ((ns (namestring name)))
         (if (and (consp (pathname-directory name))
                  (eq (car (pathname-directory name))
                      :absolute))
             ns
             (concatenate 'string (|get_current_directory|)  "/" ns))))
#+:cmu
(defun relative-to-absolute (name)
  (unix::unix-maybe-prepend-current-directory name))

;;; Saner version of compile-file
#+:ecl
(defun |fricas_compile_file| (f output-file)
    (compile-file f :output-file (relative-to-absolute output-file)
                    :system-p t))

#+:poplog
(defun |fricas_compile_file| (f output-file)
    (|run_program| "cp" (list f output-file)))

#-(or :ecl :poplog)
(defun |fricas_compile_file| (f output-file)
    (compile-file f :output-file (relative-to-absolute output-file)))

(defun |fricas_compile_fasl| (f output-file)
#-:ecl
    (|fricas_compile_file| f output-file)
#+:ecl
    (compile-file f :output-file (relative-to-absolute output-file))
)

;;; |run_program| and |run_shell_command|

(defun |run_program| (command arguments)
  ;; Execute "command" with a list of "arguments" synchronously.
  ;; Output to the standard output stream.
  ;; The return value is the exit code of "command".
  #+:abcl
  (sys:process-exit-code (sys:run-program command arguments :output t))
  #+:clisp
  (let ((exit-code (ext:run-program command :arguments arguments)))
    (if exit-code exit-code 0))
  #+:cmu
  (ext:process-exit-code (ext:run-program command arguments :output t))
  #+:ecl
  (cadr (multiple-value-list (ext:run-program command arguments :output t)))
  ;; #+:gcl ;; run-process is asynchronous
  ;; (si:run-process command arguments)
  #+:lispworks ;; call-system requires absolute path for "command"
  (system:call-system-showing-output `("/usr/bin/env" ,command ,@arguments))
  #+:openmcl
  (cadr (multiple-value-list (ccl:external-process-status
                              (ccl:run-program command arguments :output t))))
  #+:poplog
  (pop11:sysobey "/usr/bin/env" (cons command arguments))
  #+:sbcl
  (sb-ext:process-exit-code
    (sb-ext:run-program command arguments :search t :output *standard-output*))
  #+:gcl
  (si:system (format nil "~{~a~^ ~}" (cons command arguments)))
)

(defun |run_shell_command| (s)
  #+:gcl
  (si:system s)
  #-:gcl
  (|run_program| "sh" (list "-c" s)))

(defmacro DEFCONST (name value)
    (if (not (boundp name))
        `(DEFCONSTANT ,name ,value)))

#+:cmu
(defconstant +list-based-union-limit+ 80)

#+:cmu
(defun union (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Return the union of LIST1 and LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  ;; We have two possibilities here: for shortish lists we pick up the
  ;; shorter one as the result, and add the other one to it. For long
  ;; lists we use a hash-table when possible.
  (let ((n1 (length list1))
        (n2 (length list2))
        (key (and key (coerce key 'function)))
        (test (if notp
                  (let ((test-not-fun (coerce test-not 'function)))
                    (lambda (x y) (not (funcall test-not-fun x y))))
                  (coerce test 'function))))
    (multiple-value-bind (short long n-short)
        (if (< n1 n2)
            (values list1 list2 n1)
            (values list2 list1 n2))
      (if (or (< n-short +list-based-union-limit+)
              (not (member test (list #'eq #'eql #'equal #'equalp))))
          (let ((orig short))
            (dolist (elt long)
              (unless (member
                       (lisp::apply-key key elt) orig :key key :test test)
                (push elt short)))
            short)
          (let ((table (make-hash-table :test test :size (+ n1 n2)))
                (union nil))
            (dolist (elt long)
              (setf (gethash (lisp::apply-key key elt) table) elt))
            (dolist (elt short)
              (setf (gethash (lisp::apply-key key elt) table) elt))
            (maphash (lambda (k v)
                       (declare (ignore k))
                       (push v union))
                     table)
            union)))))

#+:cmu
(defun nunion (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Destructively return the union of LIST1 and LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  ;; We have two possibilities here: for shortish lists we pick up the
  ;; shorter one as the result, and add the other one to it. For long
  ;; lists we use a hash-table when possible.
  (let ((n1 (length list1))
        (n2 (length list2))
        (key (and key (coerce key 'function)))
        (test (if notp
                  (let ((test-not-fun (coerce test-not 'function)))
                    (lambda (x y) (not (funcall test-not-fun x y))))
                  (coerce test 'function))))
    (multiple-value-bind (short long n-short)
        (if (< n1 n2)
            (values list1 list2 n1)
            (values list2 list1 n2))
      (if (or (< n-short +list-based-union-limit+)
              (not (member test (list #'eq #'eql #'equal #'equalp))))
          (let ((orig short))
            (do ((elt (car long) (car long)))
                ((endp long))
              (if (not (member
                        (lisp::apply-key key elt) orig :key key :test test))
                  (lisp::steve-splice long short)
                  (setf long (cdr long))))
            short)
          (let ((table (make-hash-table :test test :size (+ n1 n2))))
            (dolist (elt long)
              (setf (gethash (lisp::apply-key key elt) table) elt))
            (dolist (elt short)
              (setf (gethash (lisp::apply-key key elt) table) elt))
            (let ((union long)
                  (head long))
              (maphash (lambda (k v)
                         (declare (ignore k))
                         (if head
                             (setf (car head) v
                                   head (cdr head))
                             (push v union)))
                      table)
              union))))))

(defmacro MEMQ (a b) `(member ,a ,b :test #'eq))

(defun |handle_input_file|(fn fun args)
    (with-open-file (stream fn :direction :input
       :if-does-not-exist nil)
       (APPLY fun (cons stream args))))

(defun |handle_output_file|(fn fun args)
    (with-open-file (stream fn :direction :output
       :if-exists :supersede)
       (APPLY fun (cons stream args))))

(in-package "BOOT")

;;; Various lisps use different ``extensions'' on the filename to indicate
;;; that a file has been compiled. We set this variable correctly depending
;;; on the system we are using.
(defvar |$lisp_bin_filetype|
  #+:GCL "o"
  #+:cmu (c:backend-fasl-file-type c:*target-backend*)
  #+:sbcl "fasl"
  #+:clisp "fas"
  #+:openmcl (subseq (namestring CCL:*.FASL-PATHNAME*) 1)
  #+:ecl "fas"
  #+:lispworks (pathname-type (compile-file-pathname "foo.lisp"))
  #+:poplog "lsp"
  #+:abcl "abcl"
)

;;; Macros used in Boot code

(defmacro IFCAR (x)
  (if (atom x)
      `(and (consp ,x) (qcar ,x))
    (let ((xx (gensym)))
      `(let ((,xx ,x))
         (and (consp ,xx) (qcar ,xx))))))

(defmacro IFCDR (x)
  (if (atom x)
      `(and (consp ,x) (qcdr ,x))
    (let ((xx (gensym)))
      `(let ((,xx ,x))
         (and (consp ,xx) (qcdr ,xx))))))

(defmacro |function| (name) `(FUNCTION ,name))

(defmacro |replaceString| (result part start)
    `(replace ,result ,part :start1 ,start))

(defun |elapsedGcTime| ()
  #+:clisp
  (multiple-value-bind (used room static gc-count gc-space gc-time) (sys::%room)
    gc-time)
  #+:cmu ext:*gc-run-time*
  #+:gcl (system:gbc-time)
  #+:openmcl (ccl:gctime)
  #+:sbcl sb-ext:*gc-run-time*
  #+:lispworks
  (progn
    (hcl:start-gc-timing :initialize nil)
    (* (getf (hcl:get-gc-timing) :total) |$timerTicksPerSecond|))
  #-(or :clisp :cmu :gcl :openmcl :sbcl :lispworks)
  0)

(defmacro |char| (arg)
  (cond ((stringp arg) (character arg))
        ((integerp arg) (code-char arg))
        ((and (consp arg) (eq (car arg) 'quote)) (character (cadr arg)))
        (t `(character ,arg))))

(defmacro add1 (x) `(1+ ,x))

(defmacro assq (a b)
 `(assoc ,a ,b :test #'eq))

(defmacro fetchchar (x i)
 `(char ,x ,i))

(defmacro fixp (x)
 `(integerp ,x))

(defmacro identp (x)
 (if (atom x)
  `(and ,x (symbolp ,x))
   (let ((xx (gensym)))
    `(let ((,xx ,x))
      (and ,xx (symbolp ,xx))))))

(defmacro LASTNODE (l)
 `(last ,l))

(defmacro makestring (a) a)

(defmacro maxindex (x)
 `(the fixnum (1- (the fixnum (length ,x)))))

(defmacro refvecp (v) `(simple-vector-p ,v))

(defmacro sintp (n)
 `(typep ,n 'fixnum))

(defmacro stringlength (x)
 `(length (the string ,x)))

(defmacro subrp (x)
 `(compiled-function-p ,x))

(defmacro vecp (v) `(simple-vector-p ,v))

;;; The following defines syntax of Spad identifiers

(defmacro |startsId?| (x)
    `(or (alpha-char-p ,x) (member ,x '(#\? #\% #\!) :test #'char=)))

(defmacro |idChar?| (x)
    `(or (alphanumericp ,x) (member ,x '(#\? #\% #\' #\!) :test #'char=)))

(defun |write_to_string_radix| (int radix)
    (write-to-string int :base radix))

(in-package "BOOTTRAN")

(defmacro |doInBoottranPackage| (expr)
    `(let ((*PACKAGE* (find-package "BOOTTRAN")))
         ,expr))

(defun |shoeEVALANDFILEACTQ| (expr)
    `(eval-when (:execute :load-toplevel)
         ,expr))

#+gcl
(in-package "BOOT")
#+gcl
(shadow "LIST")
#+gcl
(defmacro list (&rest r &aux (l (length r)))
  (let ((x (nthcdr (1- call-arguments-limit) r)))
    (if x `(nconc (cl::list ,@(ldiff r x)) (list ,@x)) `(cl::list ,@r))))
#+gcl
(deftype list nil 'cl::list)
