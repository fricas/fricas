;(locally
;  (declare (optimize (speed 3) (safety 0) (debug 0))))

(in-package "FRICAS-LISP")

#+(or :sbcl :openmcl)
(progn

(foreign-defs
(fricas-foreign-call boot::|jl_gc_collect| "jl_gc_collect" void)
(fricas-foreign-call boot::|jl_eval_string| "jl_eval_str" void
        (command c-string))
(fricas-foreign-call boot::|jl_int64_eval_string| "jl_int64_eval_string" long
        (command c-string))
(fricas-foreign-call boot::|jl_dbl_eval_string| "jl_dbl_eval_string" double
        (command c-string))
(fricas-foreign-call boot::|jl_call1_wrapped_index| "jl_call1_wrapped_index" long
        (func c-string) (index long) (ind long))
(fricas-foreign-call boot::|jl_call2_wrapped_index| "jl_call2_wrapped_index" long
        (func c-string) (index long) (ind1 long) (ind2 long))
(fricas-foreign-call boot::|jl_call3_wrapped_index| "jl_call3_wrapped_index" long
        (func c-string) (index long) (ind1 long) (ind2 long) (ind3 long))
(fricas-foreign-call boot::|jl_call4_wrapped_index| "jl_call4_wrapped_index" long
        (func c-string) (index long) (ind1 long) (ind2 long) (ind3 long)
            (ind4 long))
(fricas-foreign-call boot::|jl_call5_wrapped_index| "jl_call5_wrapped_index" long
        (func c-string) (index long) (ind1 long) (ind2 long) (ind3 long)
            (ind4 long) (ind5 long))
(fricas-foreign-call boot::|jl_setindex_mwrap_eval_string| "jl_setindex_mwrap_eval_string"
    long (index long) (command c-string))
(fricas-foreign-call boot::|jl_setindex_imwrap_eval_string| "jl_setindex_imwrap_eval_string"
    long (index long) (command c-string))

; 64 bits floating point numbers
(fricas-foreign-call boot::|jl_function_dbl| "jl_call_function_dbl" void
        (func c-string) (arg double))
(fricas-foreign-call boot::|jl_function_dbl_dbl| "jl_call_function_dbl_dbl" void
        (func c-string) (arg1 double) (arg2 double))
(fricas-foreign-call boot::|jl_function_dbl_dbl_dbl| "jl_call_function_dbl_dbl_dbl" void
        (func c-string) (arg1 double) (arg2 double) (arg3 double))
(fricas-foreign-call boot::|jl_int64_function_dbl| "jl_call_int64_function_dbl" long
        (func c-string) (arg double))
(fricas-foreign-call boot::|jl_dbl_function_dbl| "jl_call_dbl_function_dbl" double
        (func c-string) (arg double))
(fricas-foreign-call boot::|jl_dbl_function_dbl_dbl| "jl_call_dbl_function_dbl_dbl" double
        (func c-string) (arg1 double) (arg2 double))
(fricas-foreign-call boot::|jl_dbl_function_dbl_dbl_dbl| "jl_call_dbl_function_dbl_dbl_dbl" double
        (func c-string) (arg1 double) (arg2 double) (arg3 double))
(fricas-foreign-call boot::|jl_dbl_function_dbl_int64| "jl_call_dbl_function_dbl_int64" double
        (func c-string) (arg1 double) (arg2 long))
(fricas-foreign-call boot::|jl_dbl_function_int64_dbl| "jl_call_dbl_function_int64_dbl" double
        (func c-string) (arg1 long) (arg2 double))
(fricas-foreign-call boot::|jl_delete_wrapped_index| "jl_delete_wrapped_index"
        void (idx long))

; 32 bits floating point numbers
(fricas-foreign-call boot::|jl_flt_eval_string| "jl_flt_eval_string" float
        (command c-string))
(fricas-foreign-call boot::|jl_function_flt| "jl_call_function_flt" void
        (func c-string) (arg float))
(fricas-foreign-call boot::|jl_function_flt_flt| "jl_call_function_flt_flt" void
        (func c-string) (arg1 float) (arg2 float))
(fricas-foreign-call boot::|jl_function_flt_flt_flt| "jl_call_function_flt_flt_flt" void
        (func c-string) (arg1 float) (arg2 float) (arg3 float))
(fricas-foreign-call boot::|jl_int64_function_flt| "jl_call_int64_function_flt" long
        (func c-string) (arg float))
(fricas-foreign-call boot::|jl_flt_function_flt| "jl_call_flt_function_flt" float
        (func c-string) (arg float))
(fricas-foreign-call boot::|jl_flt_function_flt_flt| "jl_call_flt_function_flt_flt" float
        (func c-string) (arg1 float) (arg2 float))
(fricas-foreign-call boot::|jl_flt_function_flt_flt_flt| "jl_call_flt_function_flt_flt_flt" float
        (func c-string) (arg1 float) (arg2 float) (arg3 float))
(fricas-foreign-call boot::|jl_flt_function_flt_int64| "jl_call_flt_function_flt_int64" float
        (func c-string) (arg1 float) (arg2 long))
(fricas-foreign-call boot::|jl_flt_function_int64_flt| "jl_call_flt_function_int64_flt" float
        (func c-string) (arg1 long) (arg2 float))

#+:sbcl
(progn
(fricas-foreign-call jl_bool_eval_string "jl_bool_eval_string" jbool
        (command c-string))
(fricas-foreign-call jl_bool_function_flt_flt "jl_call_bool_function_flt_flt"
    jbool (func c-string) (arg1 float) (arg2 float))
(fricas-foreign-call jl_bool_function_dbl_dbl "jl_call_bool_function_dbl_dbl"
    jbool (func c-string) (arg1 double) (arg2 double))
(fricas-foreign-call jl_call1_bool_wrapped_index "jl_call1_bool_wrapped_index"
    jbool (func c-string) (arg1 long))
(fricas-foreign-call jl_call2_bool_wrapped_index "jl_call2_bool_wrapped_index"
    jbool (func c-string) (arg1 long) (arg2 long))
(fricas-foreign-call boot::|jl_string_eval_string| "jl_string_eval_string"
    c-string (command c-string))
(fricas-foreign-call boot::|jl_string_function_flt| "jl_call_string_function_flt"
    c-string (func c-string) (arg float))
(fricas-foreign-call boot::|jl_string_function_dbl| "jl_call_string_function_dbl"
    c-string (func c-string) (arg double))
(fricas-foreign-call boot::|jl_string_getindex| "jl_stringify_wrapped_index"
    c-string (index long))
)

#+:openmcl
(progn
(fricas-foreign-call jl_bool_eval_string "jl_bool_eval_string" jbool
        (command c-string))
(fricas-foreign-call jl_string_eval_string "jl_string_eval_string" c-string
        (command c-string))
(fricas-foreign-call jl_bool_function_dbl_dbl "jl_call_bool_function_dbl_dbl" jbool
        (func c-string) (arg1 double) (arg2 double))
(fricas-foreign-call jl_string_function_flt "jl_call_string_function_flt" c-string
        (func c-string) (arg float))
(fricas-foreign-call jl_string_function_dbl "jl_call_string_function_dbl" c-string
        (func c-string) (arg double))
(fricas-foreign-call jl_stringify_wrapped_index "jl_stringify_wrapped_index" c-string
    (index long))
(fricas-foreign-call jl_bool_function_flt_flt "jl_call_bool_function_flt_flt" jbool
        (func c-string) (arg1 float) (arg2 float))
(fricas-foreign-call jl_call1_bool_wrapped_index "jl_call1_bool_wrapped_index" jbool
        (func c-string) (arg1 long))
(fricas-foreign-call jl_call2_bool_wrapped_index "jl_call2_bool_wrapped_index" jbool
        (func c-string) (arg1 long) (arg2 long))
)
)

#+sbcl
(progn
(defmacro boot::|jl_bool_eval_string| (str)
    `(if (eq (jl_bool_eval_string ,str)  1) t nil))
(defmacro boot::|jl_bool_function_flt_flt| (func arg1 arg2)
    `(if (eq (jl_bool_function_flt_flt ,func ,arg1 ,arg2)  1) t nil))
(defmacro boot::|jl_bool_function_dbl_dbl| (func arg1 arg2)
    `(if (eq (jl_bool_function_dbl_dbl ,func ,arg1 ,arg2)  1) t nil))
(defmacro boot::|jl_call1_bool_wrapped_index| (func index)
    `(if (eq (jl_call1_bool_wrapped_index ,func ,index) 1) t nil))
(defmacro boot::|jl_call2_bool_wrapped_index| (func index1 index2)
    `(if (eq (jl_call2_bool_wrapped_index ,func ,index1 ,index2) 1) t nil))
)

#+:openmcl
(progn
(defmacro boot::|jl_bool_eval_string| (str)
    `(if (eq (jl_bool_eval_string ,str)  1) t nil))
(defmacro boot::|jl_bool_function_flt_flt| (func arg1 arg2)
    `(if (eq (jl_bool_function_flt_flt ,func ,arg1 ,arg2)  1) t nil))
(defmacro boot::|jl_bool_function_dbl_dbl| (func arg1 arg2)
    `(if (eq (jl_bool_function_dbl_dbl ,func ,arg1 ,arg2)  1) t nil))
(defmacro boot::|jl_call1_bool_wrapped_index| (func index)
    `(if (eq (jl_call1_bool_wrapped_index ,func ,index) 1) t nil))
(defmacro boot::|jl_call2_bool_wrapped_index| (func index1 index2)
    `(if (eq (jl_call2_bool_wrapped_index ,func ,index1 ,index2) 1) t nil))
(defmacro boot::|jl_string_eval_string| (str)
    `(ccl::%get-utf-8-cstring (jl_string_eval_string ,str)))
(defmacro boot::|jl_string_function_flt| (func  flt)
    `(ccl::%get-utf-8-cstring (jl_string_function_flt ,func ,flt)))
(defmacro boot::|jl_string_function_dbl| (func dbl)
    `(ccl::%get-utf-8-cstring (jl_string_function_dbl ,func ,dbl)))
(defmacro boot::|jl_string_getindex| (index)
    `(ccl::%get-utf-8-cstring (jl_stringify_wrapped_index ,index)))
)
)

(in-package "BOOT")

(defun make-jlindex()
    (loop for n from 1 upto most-positive-fixnum
        when (not (|jl_bool_eval_string|
            (concatenate 'string "haskey(refs," (princ-to-string n) ")")))
            return n))

(defclass jlref ()
    ((id  :reader jlrefId   :initarg :id))
    (:default-initargs :id nil))

(defmethod print-object((obj jlref) stream)
    (print-unreadable-object (obj stream :type t :identity t)
        (princ (jlrefId obj) stream)))

(defun |make_jlref| (str)
    (let* ((index (or (make-jlindex)
        (error "Amount of Julia references excedeed")))
            (id (|jl_setindex_mwrap_eval_string| index str)))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid command given to Julia"))))

(defun |make_jlimref| (str)
    (let* ((index (or (make-jlindex)
        (error "Amount of Julia references excedeed")))
            (id (|jl_setindex_imwrap_eval_string| index str)))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid command given to Julia"))))

(defun |make_jlref_wcall1| (func var1)
    (let* ((index (or (make-jlindex)
        (error "Amount of Julia references excedeed")))
            (id (|jl_call1_wrapped_index| func index (jlrefId var1))))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid command given to Julia"))))

(defun |make_jlref_wcall2| (func var1 var2)
    (let* ((index (or (make-jlindex)
        (error "Amount of Julia references excedeed")))
            (id (|jl_call2_wrapped_index| func index (jlrefId var1) (jlrefId var2))))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid command given to Julia"))))

(defun |make_jlref_wcall3| (func var1 var2 var3)
    (let* ((index (or (make-jlindex)
        (error "Amount of Julia references excedeed")))
            (id (|jl_call3_wrapped_index| func index
                (jlrefId var1) (jlrefId var2) (jlrefId var3))))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid command given to Julia"))))

(defun |make_jlref_wcall4| (func var1 var2 var3 var4)
    (let* ((index (or (make-jlindex)
        (error "Amount of Julia references excedeed")))
            (id (|jl_call4_wrapped_index| func index
                (jlrefId var1) (jlrefId var2) (jlrefId var3) (jlrefId var4))))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid command given to Julia"))))

(defun |make_jlref_wcall5| (func var1 var2 var3 var4 var5)
    (let* ((index (or (make-jlindex)
        (error "Amount of Julia references excedeed"))))
            (id (|jl_call5_wrapped_index| func index
                (jlrefId var1) (jlrefId var2) (jlrefId var3)
                (jlrefId var4) (jlrefId var5))))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid command given to Julia")))

(defun |make_jlref_from_fvec| (cplx vec)
    (let* ((index (or (make-jlindex)
                (error "Amount of Julia references excedeed")))
            (id (|jl_wrap_1dfarray| cplx index vec))))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid vector given to Julia")))

(defun |make_jlref_from_vec| (cplx vec)
    (let* ((index (or (make-jlindex)
                (error "Amount of Julia references excedeed")))
            (id (|jl_wrap_1darray| cplx index vec)))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid vector given to Julia"))))

(defun |make_jlref_from_fmat| (cplx mat m)
    (let* ((index (or (make-jlindex)
                (error "Amount of Julia references excedeed")))
            (id (|jl_wrap_2dfarray| cplx index mat m)))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid matrix given to Julia"))))

(defun |make_jlref_from_mat| (cplx mat m)
    (let* ((index (or (make-jlindex)
                (error "Amount of Julia references excedeed")))
        (id (|jl_wrap_2darray| cplx index mat m)))
        (if (not (zerop id))
            (let ((ret (make-instance 'jlref :id id)))
                    #+:sbcl (sb-ext:finalize ret (lambda ()
                        (sb-concurrency:enqueue index *jqueue*)))
                ret)
            (error "Invalid matrix given to Julia"))))

;;; Floating point macros

(defmacro |convertDFToSF|(x)
    `(coerce (the double-float ,x) 'single-float))
(defmacro |convertSFToDF|(x) ; Horror!!!
    `(coerce (the single-float ,x) 'double-float))
(defun |makeF32| (mantissa exponent)
  (FLOAT (/ mantissa (expt 2 (- exponent))) 0.0f0)) 

;; Before version 1.8 Clozure CL had buggy floating point optimizer, so
;; for it we need to omit type declarations to disable optimization
;; ???

#-(and :openmcl (not :CCL-1.8))
(defmacro DEF_SF_BINOP (name op)
   `(defmacro ,name (x y) `(the single-float (,',op (the single-float ,x)
                                                    (the single-float ,y)))))
#+(and :openmcl (not :CCL-1.8))
(defmacro DEF_SF_BINOP (name op) `(defmacro ,name (x y) `(,',op ,x ,y)))

(DEF_SF_BINOP |add_SF| +)
(DEF_SF_BINOP |mul_SF| *)
(DEF_SF_BINOP |max_SF| MAX)
(DEF_SF_BINOP |min_SF| MIN)
(DEF_SF_BINOP |sub_SF| -)
(DEF_SF_BINOP |div_SF| /)

(defmacro |minus_SF| (x) `(- (the single-float ,x)))
(defmacro |abs_SF| (x) `(FLOAT-SIGN (the (single-float 1.0f0 1.0f0) 1.0f0)
                                    (the single-float ,x)))
(defmacro |less_SF| (x y) `(< (the single-float ,x)
                              (the single-float ,y)))
(defmacro |eql_SF| (x y) `(= (the single-float ,x)
                             (the single-float ,y)))
(defmacro |expt_SF_I| (x y) `(EXPT (the single-float ,x)
                                   (the integer ,y)))
(defmacro |expt_SF| (x y) `(EXPT (the single-float ,x)
                                 (the single-float ,y)))
(defmacro |mul_SF_I| (x y) `(* (the single-float ,x)
                                (the integer ,y)))
(defmacro |div_SF_I| (x y) `(/ (the single-float ,x)
                               (the integer ,y)))
(defmacro |zero?_SF| (x) `(ZEROP (the single-float ,x)))
(defmacro |negative?_SF| (x) `(MINUSP (the single-float ,x)))

(defun split-str (string &optional (separator " "))
  (split_str string separator))

(defun split_str (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	  (split_str (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
        (cons string r))))

#+(or :sbcl :openmcl)
(defun |jl_stringify_wrapped_index| (func mime index)
    (split-str
        (|jl_string_eval_string| (concatenate 'string
            "io = IOBuffer();" func "(io," mime ", getindex(refs,"
                (princ-to-string index) "));String(take!(io))"))
                    (string #\newline)))

(defun |jl_using_package|(pack)
    (|jl_bool_eval_string| (concatenate 'string "try using " pack
        " catch;  return false else return true end")))

(defun |jl_import_symbol|(sym)
    (|jl_bool_eval_string| (concatenate 'string "try import " sym
        " catch; return false else return true end")))

(defun |jl_include_file|(file)
    (|jl_bool_eval_string| (concatenate 'string "try include(\"" file "\")"
        " catch; return false else return true end")))

(defmacro get_ncols (cplx a nrows)
    `(if (zerop ,cplx)
        (/ (array-dimension ,a 0) ,nrows)
        (/ (/ (array-dimension ,a 0) 2) ,nrows)))

(defmacro get_vnrows (cplx v)
    `(if (zerop ,cplx)
        (array-dimension ,v 0)
        (/ (array-dimension ,v 0) 2)))

; double-float array creation
; 1D
(defmacro |make_df_array1| (size)
    `(make-array ,size :element-type 'double-float))
(defmacro |make_df_iarray1| (size val)
    `(make-array ,size :element-type 'double-float :initial-element ,val))

; 32 bits
(defmacro |make_sf_array1| (size)
    `(make-array ,size :element-type 'single-float))

(defmacro |make_sf_iarray1| (size val)
    `(make-array ,size :element-type 'single-float :initial-element ,val))

(defmacro make_cf_array1 (n)
   `(make-array (list (* 2 ,n)) :element-type 'single-float))

; Int 64 array creation
; 1D
(defmacro |make_int64_array1| (size)
    `(make-array ,size :element-type '(signed-byte 64)))

(defmacro |make_int64_iarray1| (size val)
    `(make-array ,size :element-type '(signed-byte 64) :initial-element ,val))

; double-float array creation
; 2D
 (defmacro |make_df_array2| (sizea sizeb)
     `(make-array (* (the fixnum ,sizea) (the fixnum ,sizeb)) :element-type 'double-float))
;    (make-array (list sizeb sizea) :element-type 'double-float))

(defmacro |make_df_iarray2| (sizea sizeb val)
    `(make-array (* (the fixnum ,sizea) (the fixnum ,sizeb))
        :element-type 'double-float :initial-element ,val))

; 32 bits
(defmacro |make_sf_array2| (sizea sizeb)
     `(make-array (* (the fixnum ,sizea) (the fixnum ,sizeb)) :element-type 'single-float))
;    (make-array (list sizeb sizea) :element-type 'single-float))

(defmacro |make_sf_iarray2| (sizea sizeb val)
    `(make-array (* (the fixnum ,sizea) (the fixnum ,sizeb))
        :element-type 'single-float :initial-element ,val))

; Double-float 1D array access - 1-based index

(defmacro dfvsize (x)
 `(the fixnum (length (the (simple-array double-float (*)) ,x))))

(defmacro dfaref11(v i)
`(aref (the (simple-array double-float (*)) ,v) (1- (the fixnum ,i))))

(defmacro dfsetaref11(v i s)
    `(setf (aref (the (simple-array double-float (*)) ,v)
        (1- (the fixnum ,i))) (the double-float ,s)))

; 32 bits

(defmacro sfvsize (x)
 `(the fixnum (length (the (simple-array single-float (*)) ,x))))

(defmacro sfaref11(v i)
`(aref (the (simple-array single-float (*)) ,v) (1- (the fixnum ,i))))

(defmacro sfsetaref11(v i s)
    `(setf (aref (the (simple-array single-float (*)) ,v)
        (1- (the fixnum ,i))) (the single-float ,s)))

; Int64 array access

(defmacro int64vsize (x)
 `(the fixnum (length (the (simple-array (signed-byte 64) (*)) ,x))))

(defmacro int64aref11(v i)
`(aref (the (simple-array (signed-byte 64) (*)) ,v) (1- (the (signed-byte 64) ,i))))

(defmacro int64setaref11(v i s)
    `(setf (aref (the (simple-array (signed-byte 64) (*)) ,v)
        (1- (the (signed-byte 64) ,i))) (the (signed-byte 64) ,s)))


; 2D array emulation - 1-based index - row major ordering access
; column major order done in Spad

; sizea: row length
; sizeb: col length
; i: rown
; j: coln
(defmacro dfaref21(v sizea sizeb i j)
    (declare (ignore sizeb))
    `(aref (the (simple-array double-float (*)) ,v)
        (1- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)))
                (the fixnum ,j)))))

(defmacro dfsetaref21(v sizea sizeb i j s)
    (declare (ignore sizeb))
    `(setf
        (aref (the (simple-array double-float (*)) ,v)
            (1- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)))
                    (the fixnum ,j))))
        (the double-float ,s)))

; 32 bits
(defmacro sfaref21(v sizea sizeb i j)
    (declare (ignore sizeb))
    `(aref (the (simple-array single-float (*)) ,v)
        (1- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)))
                (the fixnum ,j)))))

(defmacro sfsetaref21(v sizea sizeb i j s)
    (declare (ignore sizeb))
    `(setf
        (aref (the (simple-array single-float (*)) ,v)
            (1- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)))
                    (the fixnum ,j))))
        (the single-float ,s)))

; Complex double float vectors
; 1-based index

(defmacro jcdelt(ov oi)
   (let ((v (gensym))
         (i (gensym)))
   `(let ((,v ,ov)
          (,i ,oi))
      (cons
          (aref (the (simple-array double-float (*)) ,v) (- (* 2 ,i) 2))
          (aref (the (simple-array double-float (*)) ,v) (1- (* 2 ,i)))))))

(defmacro jcdsetelt(ov oi os)
   (let ((v (gensym))
         (i (gensym))
         (s (gensym)))
   `(let ((,v ,ov)
          (,i ,oi)
          (,s ,os))
        (setf (aref (the (simple-array double-float (*)) ,v) (- (* 2 ,i) 2))
           (car ,s))
        (setf (aref (the (simple-array double-float (*)) ,v) (1- (* 2 ,i)))
           (cdr ,s))
        ,s)))

; 32 bits
(defmacro JCFLEN(v)
    `(truncate (length (the (simple-array single-float (*)) ,v)) 2))

(defmacro jcfelt(ov oi)
   (let ((v (gensym))
         (i (gensym)))
   `(let ((,v ,ov)
          (,i ,oi))
      (cons
          (aref (the (simple-array single-float (*)) ,v) (- (* 2 ,i) 2))
          (aref (the (simple-array single-float (*)) ,v) (1- (* 2 ,i)))))))

(defmacro jcfsetelt(ov oi os)
   (let ((v (gensym))
         (i (gensym))
         (s (gensym)))
   `(let ((,v ,ov)
          (,i ,oi)
          (,s ,os))
        (setf (aref (the (simple-array single-float (*)) ,v) (- (* 2 ,i) 2))
           (car ,s))
        (setf (aref (the (simple-array single-float (*)) ,v) (1- (* 2 ,i)))
           (cdr ,s))
        ,s)))

; Complex double float matrices
; Row major access - 1-based index

(defmacro jcdelt2(ov osizea oi oj)
   (let ((v (gensym))
         (sizea (gensym))
         (i (gensym))
         (j (gensym)))
   `(let ((,v ,ov)
          (,sizea ,osizea)
          (,i ,oi)
          (,j ,oj))
        (cons
            (aref (the (simple-array double-float (*)) ,v)
                (- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)) 2)
                    (* (the fixnum ,j) 2)) 2))
            (aref (the (simple-array double-float (*)) ,v)
                (1- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)) 2)
                    (* (the fixnum ,j) 2))))))))

(defmacro jcdsetelt2(ov osizea oi oj os)
   (let ((v (gensym))
         (sizea (gensym))
         (i (gensym))
         (j (gensym))
         (s (gensym)))
   `(let ((,v ,ov)
          (,sizea ,osizea)
          (,i ,oi)
          (,j ,oj)
          (,s ,os))
        (setf (aref (the (simple-array double-float (*)) ,v)
            (- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)) 2)
                (* (the fixnum ,j) 2)) 2))
            (car ,s))
        (setf (aref (the (simple-array double-float (*)) ,v)
            (1- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)) 2)
                (* (the fixnum ,j) 2))))
            (cdr ,s))
        ,s)))

; 32 bits
(defmacro jcfelt2(ov osizea oi oj)
   (let ((v (gensym))
         (sizea (gensym))
         (i (gensym))
         (j (gensym)))
   `(let ((,v ,ov)
          (,sizea ,osizea)
          (,i ,oi)
          (,j ,oj))
        (cons
            (aref (the (simple-array single-float (*)) ,v)
                (- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)) 2)
                    (* (the fixnum ,j) 2)) 2))
            (aref (the (simple-array single-float (*)) ,v)
                (1- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)) 2)
                    (* (the fixnum ,j) 2))))))))

(defmacro jcfsetelt2(ov osizea oi oj os)
   (let ((v (gensym))
         (sizea (gensym))
         (i (gensym))
         (j (gensym))
         (s (gensym)))
   `(let ((,v ,ov)
          (,sizea ,osizea)
          (,i ,oi)
          (,j ,oj)
          (,s ,os))
        (setf (aref (the (simple-array single-float (*)) ,v)
            (- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)) 2)
                (* (the fixnum ,j) 2)) 2))
            (car ,s))
        (setf (aref (the (simple-array single-float (*)) ,v)
            (1- (+ (* (the fixnum ,sizea) (1- (the fixnum ,i)) 2)
                (* (the fixnum ,j) 2))))
            (cdr ,s))
        ,s)))
