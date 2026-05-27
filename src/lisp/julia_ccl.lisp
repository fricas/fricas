(in-package "BOOT")
(export (import (find-symbol "FIXNUMP" 'ccl)) 'boot)

(defconstant single-positive-infinity (coerce ccl::double-float-positive-infinity 'single-float))
(defconstant single-negative-infinity (coerce ccl::double-float-positive-infinity 'single-float))
(defconstant double-positive-infinity ccl::double-float-positive-infinity)
(defconstant double-negative-infinity ccl::double-float-negative-infinity)
(defconstant fnan (coerce ccl::double-float-nan 'single-float))
(defconstant dnan ccl::double-float-nan)

(defmethod initialize-instance :after ((obj jlref) &rest initargs)
    (declare (ignore initargs))
    (ccl:terminate-when-unreachable obj))

(defmethod ccl:terminate ((obj jlref))
    (when (jlrefId obj) (queues:qpush *jqueue* (jlrefId obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun |jl_stringify_1difunction| (func mime array)
    (let ((size (array-dimension array 0)))
        ;(concatenate 'string (string #\newline)
        (split-str (ccl::%get-utf-8-cstring
                (ccl::with-encoded-cstrs :iso-8859-1 ((sa func) (sb mime))
                    (ccl:with-pointer-to-ivector (ptr array)
                        (ccl::external-call "jl_call_stringify_1difunction"
                            :address sa :address sb
                            :address ptr :signed-doubleword size :address))))
                                (string #\newline))))

(defun |jl_stringify_1dffunction| (cplx func mime array)
    (let ((size (get_vnrows cplx array)))
        (split-str (ccl::%get-utf-8-cstring
            (ccl::with-encoded-cstrs :iso-8859-1 ((sa func) (sb mime))
                (ccl:with-pointer-to-ivector (ptr array)
                    (ccl::external-call "jl_call_stringify_1dffunction"
                        :signed-doubleword cplx :address sa :address sb
                        :address ptr :signed-doubleword size :address))))
                            (string #\newline))))

(defun |jl_stringify_1dfunction| (cplx func mime array)
    (let ((size (get_vnrows cplx array)))
        (split-str (ccl::%get-utf-8-cstring
            (ccl::with-encoded-cstrs :iso-8859-1 ((sa func) (sb mime))
                (ccl:with-pointer-to-ivector (ptr array)
                    (ccl::external-call "jl_call_stringify_1dfunction"
                        :signed-doubleword cplx :address sa :address sb
                        :address ptr :signed-doubleword size :address))))
                            (string #\newline))))

(defun |jl_1difunction| (func array)
    (let ((size (array-dimension array 0)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_1difunction"
                    :address s :address ptr :signed-doubleword size :void)))))

(defun |jl_1dffunction| (cplx func array)
    (let ((size (get_vnrows cplx array)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_1dffunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword size :void)))))

(defun |jl_1dfunction| (cplx func array)
    (let ((size (get_vnrows cplx array)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_1dfunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword size :void)))))

(defun |jl_wrap_1dfarray| (cplx index array)
    (let ((size (get_vnrows cplx array))
            (index (or (make-jlindex)
                (error "Amount of Julia references excedeed"))))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_wrap_1dfarray"
                    :signed-doubleword cplx :signed-doubleword index
                        :address ptr :signed-doubleword size
                            :signed-doubleword))))

(defun |jl_wrap_1darray| (cplx index array)
    (let ((size (get_vnrows cplx array))
            (index (or (make-jlindex)
                (error "Amount of Julia references excedeed"))))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_wrap_1darray"
                    :signed-doubleword cplx :signed-doubleword index 
                        :address ptr :signed-doubleword size
                            :signed-doubleword))))

(defun |jl_flt_1dffunction| (cplx func array)
    (let ((size (get_vnrows cplx array)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_flt_1dffunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword size
                        :single-float)))))

(defun |jl_dbl_1dfunction| (cplx func array)
    (let ((size (get_vnrows cplx array)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_dbl_1dfunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword size
                        :double-float)))))

(defun |jl_flt_1dffunction_flt| (cplx func array val)
    (let ((size (get_vnrows cplx array)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_flt_1dffunction_flt"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword size
                        :single-float val :single-float)))))

(defun |jl_dbl_1dfunction_dbl| (cplx func array val)
    (let ((size (get_vnrows cplx array)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_dbl_1dfunction_dbl"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword size
                        :double-float val :double-float)))))

(defun |jl_1d2ffunction| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2))) 
        (ccl::with-encoded-cstrs :utf-8 ((s func))
          (ccl::with-pointer-to-ivector (ptr1 array1)
              (ccl::with-pointer-to-ivector (ptr2 array2)
                (ccl::external-call "jl_call_1d2ffunction"
                    :signed-doubleword cplx :address s :address ptr1 :signed-doubleword size1 
                    :address ptr2 :signed-doubleword size2 :void))))))

(defun |jl_1d2function| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2))) 
        (ccl::with-encoded-cstrs :utf-8 ((s func))
          (ccl::with-pointer-to-ivector (ptr1 array1)
              (ccl::with-pointer-to-ivector (ptr2 array2)
                (ccl::external-call "jl_call_1d2function"
                    :signed-doubleword cplx :address s :address ptr1 :signed-doubleword size1 
                    :address ptr2 :signed-doubleword size2 :void))))))

 (defun |jl_bool_1d2ffunction| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2)))
        (if (eq
            (ccl::with-encoded-cstrs :utf-8 ((s func))
                (ccl::with-pointer-to-ivector (ptr1 array1)
                    (ccl::with-pointer-to-ivector (ptr2 array2)
                        (ccl::external-call "jl_call_bool_1d2ffunction"
                            :signed-doubleword cplx :address s :address ptr1 :signed-doubleword size1 
                            :address ptr2 :signed-doubleword size2 :signed-byte)))) 0)
                            nil t)))

 (defun |jl_bool_1d2function| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2)))
        (if (eq
            (ccl::with-encoded-cstrs :utf-8 ((s func))
                (ccl::with-pointer-to-ivector (ptr1 array1)
                    (ccl::with-pointer-to-ivector (ptr2 array2)
                        (ccl::external-call "jl_call_bool_1d2function"
                            :signed-doubleword cplx :address s :address ptr1 :signed-doubleword size1 
                            :address ptr2 :signed-doubleword size2 :signed-byte)))) 0)
                            nil t)))

 (defun |jl_flt_1d2ffunction| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2))) 
        (ccl::with-encoded-cstrs :utf-8 ((s func))
          (ccl::with-pointer-to-ivector (ptr1 array1)
              (ccl::with-pointer-to-ivector (ptr2 array2)
                (ccl::external-call "jl_call_flt_1d2ffunction"
                    :signed-doubleword cplx :address s :address ptr1 :signed-doubleword size1
                    :address ptr2 :signed-doubleword size2 :single-float))))))

 (defun |jl_dbl_1d2function| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2))) 
        (ccl::with-encoded-cstrs :utf-8 ((s func))
          (ccl::with-pointer-to-ivector (ptr1 array1)
              (ccl::with-pointer-to-ivector (ptr2 array2)
                (ccl::external-call "jl_call_dbl_1d2function"
                    :signed-doubleword cplx :address s :address ptr1 :signed-doubleword size1
                    :address ptr2 :signed-doubleword size2 :double-float))))))

(defun |jl_1d3ffunction| (func array1 array2 array3)
    (let ((size1 (array-dimension array1 0))
          (size2 (array-dimension array2 0))
          (size3 (array-dimension array3 0)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl::with-pointer-to-ivector (ptr1 array1)
              (ccl::with-pointer-to-ivector (ptr2 array2)
                (ccl::with-pointer-to-ivector (ptr3 array3)
                  (ccl::external-call "jl_call_1d3ffunction"
                    :address s :address ptr1 :signed-doubleword size1 
                    :address ptr2 :signed-doubleword size2
                    :address ptr3 :signed-doubleword size3 :void)))))))

(defun |jl_1d3function| (func array1 array2 array3)
    (let ((size1 (array-dimension array1 0))
          (size2 (array-dimension array2 0))
          (size3 (array-dimension array3 0)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl::with-pointer-to-ivector (ptr1 array1)
              (ccl::with-pointer-to-ivector (ptr2 array2)
                (ccl::with-pointer-to-ivector (ptr3 array3)
                  (ccl::external-call "jl_call_1d3function"
                    :address s :address ptr1 :signed-doubleword size1 
                    :address ptr2 :signed-doubleword size2
                    :address ptr3 :signed-doubleword size3 :void)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun |jl_stringify_2dffunction| (cplx func mime array m)
    (let ((n (get_ncols cplx array m)))
        (split-str 
            (ccl::%get-utf-8-cstring 
                (ccl::with-encoded-cstrs :iso-8859-1 ((sa func) (sb mime))
                    (ccl:with-pointer-to-ivector (ptr array)
                        (ccl::external-call "jl_call_stringify_2dffunction"
                            :signed-doubleword cplx :address sa :address sb
                            :address ptr :signed-doubleword m :signed-doubleword n :address))))
                            (string #\newline))))

(defun |jl_stringify_2dfunction| (cplx func mime array m)
    (let ((n (get_ncols cplx array m)))
        (split-str 
            (ccl::%get-utf-8-cstring 
                (ccl::with-encoded-cstrs :iso-8859-1 ((sa func) (sb mime))
                    (ccl:with-pointer-to-ivector (ptr array)
                        (ccl::external-call "jl_call_stringify_2dfunction"
                            :signed-doubleword cplx :address sa :address sb
                            :address ptr :signed-doubleword m :signed-doubleword n :address))))
                            (string #\newline))))

(defun |jl_wrap_2dfarray| (cplx index array m)
    (let ((n (get_ncols cplx array m))
            (index (or (make-jlindex)
                (error "Amount of Julia references excedeed"))))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_wrap_2dfarray"
                    :signed-doubleword cplx :signed-doubleword index 
                        :address ptr :signed-doubleword m :signed-doubleword n
                            :signed-doubleword))))

(defun |jl_wrap_2darray| (cplx index array m)
    (let ((n (get_ncols cplx array m))
            (index (or (make-jlindex)
                (error "Amount of Julia references excedeed"))))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_wrap_2darray"
                    :signed-doubleword cplx :signed-doubleword index :address ptr
                        :signed-doubleword m :signed-doubleword n
                            :signed-doubleword))))

(defun |jl_2dffunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_2dffunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                        :void)))))

(defun |jl_2dfunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_2dfunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                        :void)))))

(defun |jl_2d2ffunction| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
              (ccl:with-pointer-to-ivector (ptr1 array1)
                (ccl::external-call "jl_call_2d2ffunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                               :address ptr1 :signed-doubleword o :signed-doubleword p
                        :void))))))

(defun |jl_2d2function| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
              (ccl:with-pointer-to-ivector (ptr1 array1)
                (ccl::external-call "jl_call_2d2function"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                               :address ptr1 :signed-doubleword o :signed-doubleword p
                        :void))))))

(defun |jl_bool_2dffunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
    (if (eq
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_bool_2dffunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                        :signed-byte))) 0) nil t)))

(defun |jl_bool_2dfunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
    (if (eq
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_bool_2dfunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                        :signed-byte))) 0) nil t)))

(defun |jl_bool_2d2ffunction| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
    (if (eq
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptr1 array1)
                (ccl::external-call "jl_call_bool_2d2ffunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                    :address ptr1 :signed-doubleword o :signed-doubleword p :signed-byte)))) 0) nil t)))

(defun |jl_bool_2d2function| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
    (if (eq
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptr1 array1)
                (ccl::external-call "jl_call_bool_2d2function"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                    :address ptr1 :signed-doubleword o :signed-doubleword p :signed-byte)))) 0) nil t)))

(defun |jl_2d3ffunction| (cplx func array m array1 o array2 q)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o))
          (r (get_ncols cplx array2 q)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptr1 array1)
            (ccl:with-pointer-to-ivector (ptr2 array2)
                (ccl::external-call "jl_call_2d3ffunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                               :address ptr1 :signed-doubleword o :signed-doubleword p
                               :address ptr2 :signed-doubleword q :signed-doubleword r
                        :void)))))))

(defun |jl_2d3function| (cplx func array m array1 o array2 q)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o))
          (r (get_ncols cplx array2 q)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptr1 array1)
            (ccl:with-pointer-to-ivector (ptr2 array2)
                (ccl::external-call "jl_call_2d3function"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                               :address ptr1 :signed-doubleword o :signed-doubleword p
                               :address ptr2 :signed-doubleword q :signed-doubleword r
                        :void)))))))

(defun |jl_flt_2dffunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_flt_2dffunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                        :single-float)))))

(defun |jl_dbl_2dfunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_dbl_2dfunction"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                        :double-float)))))

(defun |jl_flt_2dffunction_flt| (cplx func array m val)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_flt_2dffunction_dbl"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                        :double-float val :single-float)))))

(defun |jl_dbl_2dfunction_dbl| (cplx func array m val)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
                (ccl::external-call "jl_call_dbl_2dfunction_dbl"
                    :signed-doubleword cplx :address s :address ptr :signed-doubleword m :signed-doubleword n
                        :double-float val :double-float)))))

(defun |jl_iarray_2dffunction| (cplx func ipiv array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
              (ccl:with-pointer-to-ivector (ptri ipiv)
                (ccl::external-call "jl_call_iarray_2dffunction"
                    :signed-doubleword cplx :address s :address ptri 
                    :address ptr :signed-doubleword m :signed-doubleword n
                        :void))))))

(defun |jl_iarray_2dfunction| (cplx func ipiv array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
              (ccl:with-pointer-to-ivector (ptri ipiv)
                (ccl::external-call "jl_call_iarray_2dfunction"
                    :signed-doubleword cplx :address s :address ptri 
                    :address ptr :signed-doubleword m :signed-doubleword n
                        :void))))))

(defun |jl_array_2dffunction| (acplx cplx func vec array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptra array)
                (ccl:with-pointer-to-ivector (pvec vec)
                    (ccl::external-call "jl_call_array_2dffunction"
                        :signed-doubleword acplx :signed-doubleword cplx
                        :address s :address pvec :address ptra
                        :signed-doubleword m :signed-doubleword n :void))))))

(defun |jl_array_2dfunction| (acplx cplx func vec array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptra array)
                (ccl:with-pointer-to-ivector (pvec vec)
                    (ccl::external-call "jl_call_array_2dfunction"
                        :signed-doubleword acplx :signed-doubleword cplx
                        :address s :address pvec :address ptra
                        :signed-doubleword m :signed-doubleword n :void))))))

(defun |jl_svd_ffunction| (cplx func u s v array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((str func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptru u)
            (ccl:with-pointer-to-ivector (ptrs s)
            (ccl:with-pointer-to-ivector (ptrv v)
                (ccl::external-call "jl_call_fsvd"  :signed-doubleword cplx :address str
                    :address ptru :address ptrs :address ptrv
                    :address ptr :signed-doubleword m :signed-doubleword n
                        :void))))))))

(defun |jl_svd_function| (cplx func u s v array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((str func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptru u)
            (ccl:with-pointer-to-ivector (ptrs s)
            (ccl:with-pointer-to-ivector (ptrv v)
                (ccl::external-call "jl_call_svd"  :signed-doubleword cplx :address str
                    :address ptru :address ptrs :address ptrv
                    :address ptr :signed-doubleword m :signed-doubleword n
                        :void))))))))

(defun |jl_eigen_ffunction| (cplx func val vec array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptrval val)
            (ccl:with-pointer-to-ivector (ptrvec vec)
                (ccl::external-call "jl_call_feigen" :signed-doubleword cplx :address s
                    :address ptrval :address ptrvec
                    :address ptr :signed-doubleword m :signed-doubleword n
                        :void)))))))

(defun |jl_eigen_function| (cplx func val vec array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptrval val)
            (ccl:with-pointer-to-ivector (ptrvec vec)
                (ccl::external-call "jl_call_eigen" :signed-doubleword cplx :address s
                    :address ptrval :address ptrvec
                    :address ptr :signed-doubleword m :signed-doubleword n
                        :void)))))))

(defun |jl_eigen_system_ffunction| (cplx func val lvec rvec array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptrval val)
            (ccl:with-pointer-to-ivector (ptrlvec lvec)
            (ccl:with-pointer-to-ivector (ptrrvec rvec)
                (ccl::external-call "jl_call_feigen_system" :signed-doubleword cplx :address s
                    :address ptrval :address ptrlvec :address ptrrvec
                    :address ptr :signed-doubleword m :signed-doubleword n
                        :void))))))))

(defun |jl_eigen_system_function| (cplx func val lvec rvec array m)
    (let ((n (get_ncols cplx array m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptr array)
            (ccl:with-pointer-to-ivector (ptrval val)
            (ccl:with-pointer-to-ivector (ptrlvec lvec)
            (ccl:with-pointer-to-ivector (ptrrvec rvec)
                (ccl::external-call "jl_call_eigen_system" :signed-doubleword cplx :address s
                    :address ptrval :address ptrlvec :address ptrrvec
                    :address ptr :signed-doubleword m :signed-doubleword n
                        :void))))))))

(defun |jl_array_1dffunction| (acplx cplx func vec array)
    (let ((size (get_vnrows cplx array)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptra array)
                (ccl:with-pointer-to-ivector (pvec vec)
                    (ccl::external-call "jl_call_array_1dffunction"
                        :signed-doubleword acplx :signed-doubleword cplx
                        :address s :address pvec :address ptra
                        :signed-doubleword size :void))))))

(defun |jl_array_1dfunction| (acplx cplx func vec array)
    (let ((size (get_vnrows cplx array)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl:with-pointer-to-ivector (ptra array)
                (ccl:with-pointer-to-ivector (pvec vec)
                    (ccl::external-call "jl_call_array_1dfunction"
                        :signed-doubleword acplx :signed-doubleword cplx
                        :address s :address pvec :address ptra
                        :signed-doubleword size :void))))))

(defun |jl_2v2dffunction| (func vec1 vec2 array m)
    (let ((size (array-dimension vec1 0))
          (n (/ (array-dimension array 0) m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl::with-pointer-to-ivector (ptr1 vec1)
              (ccl::with-pointer-to-ivector (ptr2 vec2)
                (ccl::with-pointer-to-ivector (ptr3 array)
                  (ccl::external-call "jl_call_2v2dffunction"
                    :address s :signed-doubleword size :address ptr1
                        :address ptr2 :address ptr3
                        :signed-doubleword m :signed-doubleword n :void)))))))

(defun |jl_2v2dfunction| (func vec1 vec2 array m)
    (let ((size (array-dimension vec1 0))
          (n (/ (array-dimension array 0) m)))
        (ccl::with-encoded-cstrs :utf-8 ((s func))
            (ccl::with-pointer-to-ivector (ptr1 vec1)
              (ccl::with-pointer-to-ivector (ptr2 vec2)
                (ccl::with-pointer-to-ivector (ptr3 array)
                  (ccl::external-call "jl_call_2v2dfunction"
                    :address s :signed-doubleword size :address ptr1
                        :address ptr2 :address ptr3
                        :signed-doubleword m :signed-doubleword n :void)))))))

(defun |init_julia_env| ()
    (if (not *julia-initialized*)
        (progn
            (ccl::external-call "jl_init_env" :void)
            (setf *julia-initialized* t))
        *julia-initialized*))

(defun |clear_julia_env| ()
    (if *julia-initialized*
        (progn
            (ccl::external-call "jl_clear_env" :void)
            (setf  *julia-initialized* nil))
        *julia-initialized*))
