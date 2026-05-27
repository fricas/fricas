(in-package "BOOT")
(export (import (find-symbol "FIXNUMP" 'sb-ext)) 'boot)

(eval-when (:compile-toplevel :load-toplevel :execute)
    (sb-int:set-floating-point-modes :traps nil ; :overflow
         :rounding-mode :nearest :current-exceptions nil
         :accrued-exceptions nil :fast-mode nil)
    (unwind-protect
        (defconstant fnan (/ 0f0 0f0))
        (defconstant dnan (/ 0d0 0d0))))

(defconstant single-positive-infinity sb-ext::single-float-positive-infinity)
(defconstant single-negative-infinity sb-ext::single-float-negative-infinity)
(defconstant double-positive-infinity sb-ext::double-float-positive-infinity)
(defconstant double-negative-infinity sb-ext::double-float-negative-infinity)

;(defun |run_shell_command| (s)
;    (boot::|jl_eval_string| (concatenate 'string "run(\`" s "\`)")))

(defmacro fpointer (array) `(sb-alien:sap-alien
    (sb-sys:vector-sap (sb-ext:array-storage-vector ,array)) (* single-float)))

(defmacro dpointer (array) `(sb-alien:sap-alien
    (sb-sys:vector-sap (sb-ext:array-storage-vector ,array)) (* double-float)))

(defmacro ipointer (array) `(sb-alien:sap-alien
    (sb-sys:vector-sap (sb-ext:array-storage-vector ,array)) (* (sb-alien::signed 64))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun |jl_stringify_1difunction| (func mime array)
    (let ((size (array-dimension array 0)))
        ;(concatenate 'string #\newline
        (split-str
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_stringify_1difunction"
                        (sb-alien::function (sb-alien::c-string)
                            (sb-alien::c-string)
                            (sb-alien::c-string)
                            (* integer)
                            (sb-alien::integer)))
                        func mime (ipointer array) size))
                                (string #\newline))))

(defun |jl_stringify_1dffunction| (cplx func mime array)
    (let ((size (get_vnrows cplx array)))
        (split-str
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_stringify_1dffunction"
                        (sb-alien::function (sb-alien::c-string)
                            (sb-alien::integer)
                            (sb-alien::c-string)
                            (sb-alien::c-string)
                            (* single-float)
                            (sb-alien::integer)))
                    cplx func mime (fpointer array) size))
                        (string #\newline))))

(defun |jl_stringify_1dfunction| (cplx func mime array)
    (let ((size (get_vnrows cplx array)))
        (split-str
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_stringify_1dfunction"
                        (sb-alien::function (sb-alien::c-string)
                            (sb-alien::integer)
                            (sb-alien::c-string)
                            (sb-alien::c-string)
                            (* double-float)
                            (sb-alien::integer)))
                    cplx func mime (dpointer array) size))
                        (string #\newline))))

(defun |jl_1dffunction| (cplx func array)
    (let ((size (get_vnrows cplx array)))
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_1dffunction"
                        (sb-alien::function sb-alien::void
                            (sb-alien::integer)
                            (sb-alien::c-string)
                            (* single-float)
                            (sb-alien::integer)))
                    cplx func (fpointer array) size))))

(defun |jl_1dfunction| (cplx func array)
    (let ((size (get_vnrows cplx array)))
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_1dfunction"
                        (sb-alien::function sb-alien::void
                            (sb-alien::integer)
                            (sb-alien::c-string)
                            (* double-float)
                            (sb-alien::integer)))
                    cplx func (dpointer array) size))))

(defun |jl_1difunction| (func array)
    (let ((size (array-dimension array 0)))
        (sb-sys:with-pinned-objects (array)
            (sb-alien:alien-funcall
                (sb-alien::extern-alien "jl_call_1difunction"
                    (sb-alien::function sb-alien::void
                            (sb-alien::c-string)
                            (* (sb-alien::signed 64))
                            (sb-alien::integer)))
                    func (ipointer array) size))))

(defun |jl_wrap_1dfarray| (cplx index array)
    (let ((size (get_vnrows cplx array))
            (index (or (make-jlindex)
                (error "Amount of Julia references excedeed"))))
        (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_wrap_1dfarray"
                        (sb-alien::function (sb-alien::integer)
                             (sb-alien::integer)
                             (sb-alien::integer)
                            (* single-float)
                            (sb-alien::integer)))
                    cplx index (fpointer array) size))))

(defun |jl_wrap_1darray| (cplx index array)
    (let ((size (get_vnrows cplx array))
            (index (or (make-jlindex)
                (error "Amount of Julia references excedeed"))))
        (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_wrap_1darray"
                        (sb-alien::function sb-alien::integer
                             (sb-alien::integer)
                             (sb-alien::integer)
                            (* double-float)
                            (sb-alien::integer)))
                    cplx index (dpointer array) size))))

(defun |jl_flt_1dffunction| (cplx func array)
    (let ((size (get_vnrows cplx array)))
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_flt_1dffunction"
                        (sb-alien::function sb-alien::single-float
                             (sb-alien::integer)
                            (sb-alien::c-string)
                            (* single-float)
                            (sb-alien::integer)))
                    cplx func (fpointer array) size))))

(defun |jl_dbl_1dfunction| (cplx func array)
    (let ((size (get_vnrows cplx array)))
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_dbl_1dfunction"
                        (sb-alien::function sb-alien::double-float
                             (sb-alien::integer)
                            (sb-alien::c-string)
                            (* double-float)
                            (sb-alien::integer)))
                    cplx func (dpointer array) size))))

(defun |jl_flt_1dffunction_flt| (cplx func array val)
    (let ((size (get_vnrows cplx array)))
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_flt_1dffunction_flt"
                        (sb-alien::function sb-alien::single-float
                            (sb-alien::integer)
                            (sb-alien::c-string)
                            (* single-float)
                            (sb-alien::integer)
                            sb-alien::single-float))
                    cplx func (fpointer array) size val))))

(defun |jl_dbl_1dfunction_dbl| (cplx func array val)
    (let ((size (get_vnrows cplx array)))
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_dbl_1dfunction_dbl"
                        (sb-alien::function sb-alien::double-float
                            (sb-alien::integer)
                            (sb-alien::c-string)
                            (* double-float)
                            (sb-alien::integer)
                            sb-alien::double-float))
                    cplx func (dpointer array) size val))))

(defun |jl_1d2ffunction| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2)))
            (sb-sys:with-pinned-objects (array1 array2)
              (sb-alien:alien-funcall
                (sb-alien::extern-alien "jl_call_1d2ffunction"
                      (sb-alien::function sb-alien::void
                          (sb-alien::integer)
                          (sb-alien::c-string)
                          (* single-float)
                          (sb-alien::integer)
                          (* single-float)
                          (sb-alien::integer)))
                  cplx func (fpointer array1) size1
                            (fpointer array2) size2))))

(defun |jl_1d2function| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2)))
            (sb-sys:with-pinned-objects (array1 array2)
              (sb-alien:alien-funcall
                (sb-alien::extern-alien "jl_call_1d2function"
                      (sb-alien::function sb-alien::void
                          (sb-alien::integer)
                          (sb-alien::c-string)
                          (* double-float)
                          (sb-alien::integer)
                          (* double-float)
                          (sb-alien::integer)))
                  cplx func (dpointer array1) size1
                            (dpointer array2) size2))))

 (defun |jl_bool_1d2ffunction| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2)))
              (sb-sys:with-pinned-objects (array1 array2)
                (sb-alien:alien-funcall
                  (sb-alien::extern-alien "jl_call_bool_1d2ffunction"
                      (sb-alien::function (sb-alien::boolean 8)
                          (sb-alien::integer)
                          (sb-alien::c-string)
                          (* single-float)
                          (sb-alien::integer)
                          (* single-float)
                          (sb-alien::integer)))
                  cplx func (fpointer array1) size1
                            (fpointer array2) size2))))

 (defun |jl_bool_1d2function| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2)))
              (sb-sys:with-pinned-objects (array1 array2)
                (sb-alien:alien-funcall
                  (sb-alien::extern-alien "jl_call_bool_1d2function"
                      (sb-alien::function (sb-alien::boolean 8)
                          (sb-alien::integer)
                          (sb-alien::c-string)
                          (* double-float)
                          (sb-alien::integer)
                          (* double-float)
                          (sb-alien::integer)))
                  cplx func (dpointer array1) size1
                            (dpointer array2) size2))))

(defun |jl_flt_1d2ffunction| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2)))
            (sb-sys:with-pinned-objects (array1 array2)
              (sb-alien:alien-funcall
                (sb-alien::extern-alien "jl_call_flt_1d2ffunction"
                      (sb-alien::function sb-alien::single-float
                          (sb-alien::integer)
                          (sb-alien:c-string)
                          (* single-float)
                          (sb-alien::integer)
                          (* single-float)
                          (sb-alien::integer)))
                    cplx func (fpointer array1) size1
                              (fpointer array2) size2))))

(defun |jl_dbl_1d2function| (cplx func array1 array2)
    (let ((size1 (get_vnrows cplx array1))
          (size2 (get_vnrows cplx array2)))
            (sb-sys:with-pinned-objects (array1 array2)
              (sb-alien:alien-funcall
                (sb-alien::extern-alien "jl_call_dbl_1d2function"
                      (sb-alien::function sb-alien::double-float
                          (sb-alien::integer)
                          (sb-alien:c-string)
                          (* double-float)
                          (sb-alien::integer)
                          (* double-float)
                          (sb-alien::integer)))
                    cplx func (dpointer array1) size1
                              (dpointer array2) size2))))

(defun |jl_1d3ffunction| (func array1 array2 array3)
    (let ((size1 (array-dimension array1 0))
          (size2 (array-dimension array2 0))
          (size3 (array-dimension array3 0)))
          (sb-sys:with-pinned-objects (array1 array2 array3)
              (sb-alien:alien-funcall
                  (sb-alien::extern-alien "jl_call_1d3ffunction"
                      (sb-alien::function sb-alien::void
                          (sb-alien::c-string)
                          (* single-float)
                          (sb-alien::integer)
                          (* single-float)
                          (sb-alien::integer)
                          (* single-float)
                          (sb-alien::integer)))
                  func (fpointer array1) size1
                        (fpointer array2) size2
                        (fpointer array3) size3))))

(defun |jl_1d3function| (func array1 array2 array3)
    (let ((size1 (array-dimension array1 0))
          (size2 (array-dimension array2 0))
          (size3 (array-dimension array3 0)))
          (sb-sys:with-pinned-objects (array1 array2 array3)
              (sb-alien:alien-funcall
                  (sb-alien::extern-alien "jl_call_1d3function"
                      (sb-alien::function sb-alien::void
                          (sb-alien::c-string)
                          (* double-float)
                          (sb-alien::integer)
                          (* double-float)
                          (sb-alien::integer)
                          (* double-float)
                          (sb-alien::integer)))
                  func (dpointer array1) size1
                        (dpointer array2) size2
                        (dpointer array3) size3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun |jl_stringify_2dffunction| (cplx func mime array m)
    (let ((n (get_ncols cplx array m)))
        (split-str
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_stringify_2dffunction"
                        (sb-alien::function (sb-alien::c-string)
                            (sb-alien::integer)
                            (sb-alien::c-string)
                            (sb-alien::c-string)
                            (* single-float)
                            (sb-alien::integer)
                            (sb-alien::integer)))
                            cplx func mime (fpointer array) m n))
                                (string #\newline))))

(defun |jl_stringify_2dfunction| (cplx func mime array m)
    (let ((n (get_ncols cplx array m)))
        (split-str
            (sb-sys:with-pinned-objects (array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_stringify_2dfunction"
                        (sb-alien::function (sb-alien::c-string)
                            (sb-alien::integer)
                            (sb-alien::c-string)
                            (sb-alien::c-string)
                            (* double-float)
                            (sb-alien::integer)
                            (sb-alien::integer)))
                            cplx func mime (dpointer array) m n))
                                (string #\newline))))

(defun |jl_wrap_2dfarray| (cplx index array m)
    (let ((n (get_ncols cplx array m))
            (index (or (make-jlindex)
                (error "Amount of Julia references excedeed"))))
        (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_wrap_2dfarray"
                   (sb-alien::function (sb-alien::integer)
                       (sb-alien::integer)
                       (sb-alien::integer)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx index (fpointer array) m n))))

(defun |jl_wrap_2darray| (cplx index array m)
    (let ((n (get_ncols cplx array m))
            (index (or (make-jlindex)
                (error "Amount of Julia references excedeed"))))
        (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_wrap_2darray"
                   (sb-alien::function (sb-alien::integer)
                       (sb-alien::integer)
                       (sb-alien::integer)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx index (dpointer array) m n))))

(defun |jl_2dffunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
       (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_2dffunction"
                   (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (fpointer array) m n))))

(defun |jl_2dfunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
       (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_2dfunction"
                   (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (dpointer array) m n))))

(defun |jl_bool_2dffunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_bool_2dffunction"
                   (sb-alien::function (sb-alien::boolean 8)
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (fpointer array) m n))))

(defun |jl_bool_2dfunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_bool_2dfunction"
                   (sb-alien::function (sb-alien::boolean 8)
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (dpointer array) m n))))

(defun |jl_flt_2dffunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
       (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_flt_2dffunction"
                   (sb-alien::function sb-alien::single-float
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (fpointer array) m n))))

(defun |jl_dbl_2dfunction| (cplx func array m)
    (let ((n (get_ncols cplx array m)))
       (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_dbl_2dfunction"
                   (sb-alien::function sb-alien::double-float
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (dpointer array) m n))))

(defun |jl_2d2ffunction| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
       (sb-sys:with-pinned-objects (array array1)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_2d2ffunction"
                   (sb-alien::function sb-alien::void
                        (sb-alien::integer)
                        (sb-alien:c-string)
                        (* single-float)
                        (sb-alien::integer)
                        (sb-alien::integer)
                        (* single-float)
                        (sb-alien::integer)
                        (sb-alien::integer)))
                cplx func (fpointer array) m n (fpointer array1) o p))))

(defun |jl_2d2function| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
       (sb-sys:with-pinned-objects (array array1)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_2d2function"
                   (sb-alien::function sb-alien::void
                        (sb-alien::integer)
                        (sb-alien:c-string)
                        (* double-float)
                        (sb-alien::integer)
                        (sb-alien::integer)
                        (* double-float)
                        (sb-alien::integer)
                        (sb-alien::integer)))
                cplx func (dpointer array) m n (dpointer array1) o p))))

(defun |jl_flt_2dffunction_flt| (cplx func array m val)
    (let ((n (get_ncols cplx array m)))
      (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_flt_2dffunction_flt"
                   (sb-alien::function sb-alien::single-float
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)
                       sb-alien::single-float
                       ))
               cplx func (fpointer array) m n val))))

(defun |jl_dbl_2dfunction_dbl| (cplx func array m val)
    (let ((n (get_ncols cplx array m)))
      (sb-sys:with-pinned-objects (array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_dbl_2dfunction_dbl"
                   (sb-alien::function sb-alien::double-float
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)
                       sb-alien::double-float
                       ))
               cplx func (dpointer array) m n val))))

(defun |jl_bool_2d2ffunction| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
        (sb-sys:with-pinned-objects (array array1)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_bool_2d2ffunction"
                   (sb-alien::function (sb-alien::boolean 8)
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (fpointer array) m n
                    (fpointer array1) o p))))

(defun |jl_bool_2d2function| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
        (sb-sys:with-pinned-objects (array array1)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_bool_2d2function"
                   (sb-alien::function (sb-alien::boolean 8)
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (dpointer array) m n
                    (dpointer array1) o p))))

(defun |jl_flt_2d2ffunction| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
       (sb-sys:with-pinned-objects (array array1)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_flt_2d2ffunction"
                   (sb-alien::function sb-alien::single-float
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
               cplx func (fpointer array) m n (fpointer array1) o p))))

(defun |jl_dbl_2d2function| (cplx func array m array1 o)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o)))
       (sb-sys:with-pinned-objects (array array1)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_dbl_2d2function"
                   (sb-alien::function sb-alien::double-float
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
               cplx func (dpointer array) m n (dpointer array1) o p))))

(defun |jl_2d3ffunction| (cplx func array m array1 o array2 q)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o))
          (r (get_ncols cplx array2 q)))
       (sb-sys:with-pinned-objects (array array1 array2)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_2d3ffunction"
                   (sb-alien::function sb-alien::void
                        (sb-alien::integer)
                        (sb-alien:c-string)
                        (* single-float)
                        (sb-alien::integer)
                        (sb-alien::integer)
                        (* single-float)
                        (sb-alien::integer)
                        (sb-alien::integer)
                        (* single-float)
                        (sb-alien::integer)
                        (sb-alien::integer)))
                cplx func (fpointer array) m n
                    (fpointer array1) o p (fpointer array2) q r))))

(defun |jl_2d3function| (cplx func array m array1 o array2 q)
    (let ((n (get_ncols cplx array m))
          (p (get_ncols cplx array1 o))
          (r (get_ncols cplx array2 q)))
       (sb-sys:with-pinned-objects (array array1 array2)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_2d3function"
                   (sb-alien::function sb-alien::void
                        (sb-alien::integer)
                        (sb-alien:c-string)
                        (* double-float)
                        (sb-alien::integer)
                        (sb-alien::integer)
                        (* double-float)
                        (sb-alien::integer)
                        (sb-alien::integer)
                        (* double-float)
                        (sb-alien::integer)
                        (sb-alien::integer)))
                cplx func (dpointer array) m n
                    (dpointer array1) o p (dpointer array2) q r))))

(defun |jl_iarray_2dffunction| (cplx func ipiv array m)
    (let ((n (get_ncols cplx array m)))
       (sb-sys:with-pinned-objects (ipiv array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_iarray_2dffunction"
                   (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* (sb-alien::signed 64))
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (ipointer ipiv) (fpointer array) m n))))

(defun |jl_iarray_2dfunction| (cplx func ipiv array m)
    (let ((n (get_ncols cplx array m)))
       (sb-sys:with-pinned-objects (ipiv array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_iarray_2dfunction"
                   (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* (sb-alien::signed 64))
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (ipointer ipiv) (dpointer array) m n))))

(defun |jl_array_1dffunction| (acplx cplx func vec array)
    (let ((size (get_vnrows cplx array)))
        (sb-sys:with-pinned-objects (vec array)
            (sb-alien:alien-funcall
                (sb-alien::extern-alien "jl_call_array_1dffunction"
                    (sb-alien::function sb-alien::void
                        (sb-alien::integer)
                        (sb-alien::integer)
                        (sb-alien:c-string)
                        (* single-float)
                        (* single-float)
                        (sb-alien::integer)))
                    acplx cplx func (fpointer vec) (fpointer array) size))))

(defun |jl_array_1dfunction| (acplx cplx func vec array)
    (let ((size (get_vnrows cplx array)))
        (sb-sys:with-pinned-objects (vec array)
            (sb-alien:alien-funcall
                (sb-alien::extern-alien "jl_call_array_1dfunction"
                    (sb-alien::function sb-alien::void
                        (sb-alien::integer)
                        (sb-alien::integer)
                        (sb-alien:c-string)
                        (* double-float)
                        (* double-float)
                        (sb-alien::integer)))
                    acplx cplx func (dpointer vec) (dpointer array) size))))

(defun |jl_array_2dffunction| (acplx cplx func vec array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (vec array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_array_2dffunction"
                   (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
               acplx cplx func (fpointer vec) (fpointer array) m n))))

(defun |jl_array_2dfunction| (acplx cplx func vec array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (vec array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_array_2dfunction"
                   (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
               acplx cplx func (dpointer vec) (dpointer array) m n))))

(defun |jl_svd_ffunction| (cplx func u s v array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (u s v array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_fsvd"
                    (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (* single-float)
                       (* single-float)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (fpointer u) (fpointer s)
                    (fpointer v) (fpointer array) m n))))

(defun |jl_svd_function| (cplx func u s v array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (u s v array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_svd"
                    (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (* double-float)
                       (* double-float)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (dpointer u) (dpointer s)
                    (dpointer v) (dpointer array) m n))))

(defun |jl_eigen_ffunction| (cplx func val vec array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (val vec array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_feigen"
                    (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (* single-float)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (fpointer val) (fpointer vec) (fpointer array) m n))))

(defun |jl_eigen_function| (cplx func val vec array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (val vec array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_eigen"
                    (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (* double-float)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (dpointer val) (dpointer vec) (dpointer array) m n))))

(defun |jl_eigen_system_ffunction| (cplx func val lvec rvec array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (val lvec rvec array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_eigen_system"
                    (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* single-float)
                       (* single-float)
                       (* single-float)
                       (* single-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (fpointer val) (fpointer lvec) (fpointer rvec)
                    (fpointer array) m n))))

(defun |jl_eigen_system_function| (cplx func val lvec rvec array m)
    (let ((n (get_ncols cplx array m)))
        (sb-sys:with-pinned-objects (val lvec rvec array)
           (sb-alien:alien-funcall
               (sb-alien::extern-alien "jl_call_eigen_system"
                    (sb-alien::function sb-alien::void
                       (sb-alien::integer)
                       (sb-alien:c-string)
                       (* double-float)
                       (* double-float)
                       (* double-float)
                       (* double-float)
                       (sb-alien::integer)
                       (sb-alien::integer)))
                cplx func (dpointer val) (dpointer lvec) (dpointer rvec)
                    (dpointer array) m n))))

(defun |jl_2v2dffunction| (func vec1 vec2 array m)
    (let ((size (array-dimension vec1 0))
          (n (/ (array-dimension array 0) m)))
            (sb-sys:with-pinned-objects (vec1 vec2 array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_2v2dffunction"
                        (sb-alien::function sb-alien::void
                            (sb-alien:c-string)
                            (sb-alien::integer)
                            (* single-float)
                            (* single-float)
                            (* single-float)
                            (sb-alien::integer)
                            (sb-alien::integer)))
                    func size (fpointer vec1)
                        (fpointer vec2)
                        (fpointer array) m n))))

(defun |jl_2v2dfunction| (func vec1 vec2 array m)
    (let ((size (array-dimension vec1 0))
          (n (/ (array-dimension array 0) m)))
            (sb-sys:with-pinned-objects (vec1 vec2 array)
                (sb-alien:alien-funcall
                    (sb-alien::extern-alien "jl_call_2v2dfunction"
                        (sb-alien::function sb-alien::void
                            (sb-alien:c-string)
                            (sb-alien::integer)
                            (* double-float)
                            (* double-float)
                            (* double-float)
                            (sb-alien::integer)
                            (sb-alien::integer)))
                    func size (dpointer vec1)
                        (dpointer vec2)
                        (dpointer array) m n))))

(defun |init_julia_env| ()
    (if (not *julia-initialized*)
        (progn
            (sb-alien:alien-funcall
                (sb-alien:extern-alien "jl_init_env"
                    (sb-alien:function sb-alien:void)))
            (setf *julia-initialized* t))
        *julia-initialized*))

(defun |clear_julia_env| ()
    (if *julia-initialized*
      (progn
          (sb-alien:alien-funcall
              (sb-alien:extern-alien "jl_clear_env"
                (sb-alien:function sb-alien:void)))
	  (setf *julia-initialized* nil))
      *julia-initialized*))
