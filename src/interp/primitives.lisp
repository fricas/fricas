(in-package "BOOT")

;;; Fast array accessors

(defmacro QAREF1(v i)
`(aref (the (simple-array T (*)) ,v) ,i))

(defmacro QSETAREF1(v i s)
    `(setf (aref (the (simple-array T (*)) ,v) ,i) ,s))

;;; arrays of arbitrary offset

(defmacro QAREF1O(v i o)
    `(aref (the (simple-array T (*)) ,v) (qsdifference ,i ,o)))

(defmacro QSETAREF1O (v i s o)
    `(setf (aref (the (simple-array T (*)) ,v)
                 (qsdifference ,i ,o))
           ,s))

(defmacro QAREF2O(m i j oi oj)
    `(QAREF1O (QAREF1O ,m ,i ,oi) ,j ,oj))

(defmacro QSETAREF2O (m i j r oi oj)
    `(QSETAREF1O (QAREF1O ,m ,i ,oi) ,j ,r ,oj))


;;; array creation

(defun MAKEARR1 (size init)
    (make-array size :initial-element init))

;;; Vectors of 32-bit numbers

(defmacro ELT32(v i)
    `(aref (the (simple-array (unsigned-byte 32) (*)) ,v) ,i))

(defmacro SETELT32(v i s)
    `(setf (aref (the (simple-array (unsigned-byte 32) (*)) ,v) ,i)
           ,s))
#+:sbcl
(progn

(defmacro sbcl-make-u32-vector(n)
    (multiple-value-bind (typetag n-bits)
        (SB-IMPL::%VECTOR-WIDETAG-AND-N-BITS '(unsigned-byte 32))
        `(SB-KERNEL:ALLOCATE-VECTOR ,typetag ,n
                       (ceiling (* ,n ,n-bits) sb-vm:n-word-bits))))

(defun GETREFV32(n x)
    (let ((vec (sbcl-make-u32-vector n)))
        (fill vec x)
        vec))

)

#-:sbcl
(defun GETREFV32(n x) (make-array n :initial-element x
                                     :element-type '(unsigned-byte 32)))

(defmacro QV32LEN(v)
    `(length (the (simple-array (unsigned-byte 32) (*)) ,v)))

;;; Matrix operations

(defmacro MAKE-U32-MATRIX (n m)
   `(make-array (list ,n ,m) :element-type '(unsigned-byte 32)))

(defmacro MAKE-U32-MATRIX1 (n m s)
   `(make-array (list ,n ,m) :element-type '(unsigned-byte 32)
           :initial-element ,s))

(defmacro U32AREF2(v i j)
   `(aref (the (simple-array (unsigned-byte 32) (* *)) ,v) ,i ,j))

(defmacro U32SETAREF2(v i j s)
   `(setf (aref (the (simple-array (unsigned-byte 32) (* *)) ,v) ,i ,j)
          ,s))

(defmacro U32ANROWS(v)
    `(array-dimension (the (simple-array (unsigned-byte 32) (* *)) ,v) 0))

(defmacro U32ANCOLS(v)
    `(array-dimension (the (simple-array (unsigned-byte 32) (* *)) ,v) 1))

;;; Modular arithmetic

(deftype machine-int () '(unsigned-byte 64))

;;; (x*y + z) using 32-bit x and y and 64-bit z and assuming that
;;; intermediate results fits into 64 bits
(defmacro QSMULADD64-32 (x y z)
    `(the machine-int
         (+ (the machine-int
               (* (the (unsigned-byte 32) ,x)
                  (the (unsigned-byte 32) ,y)))
            (the machine-int ,z))))

(defmacro QSMUL64-32 (x y)
    `(the machine-int
         (* (the (unsigned-byte 32) ,x)
            (the (unsigned-byte 32) ,y))))


(defmacro QSMOD64-32 (x p)
    `(the (unsigned-byte 32)
         (rem (the machine-int ,x) (the (unsigned-byte 32) ,p))))

(defmacro QSMULADDMOD64-32 (x y z p)
    `(QSMOD64-32 (QSMULADD64-32 ,x ,y ,z) ,p))

(defmacro QSDOT2-64-32 (a1 b1 a2 b2)
    `(QSMULADD64-32 ,a1 ,b1 (QSMUL64-32 ,a2 ,b2)))

(defmacro QSDOT2MOD64-32 (a1 b1 a2 b2 p)
    `(QSMOD64-32 (QSDOT2-64-32 ,a1 ,b1 ,a2 ,b2) , p))

(defmacro QSMULMOD32 (x y p)
    `(QSMOD64-32 (QSMUL64-32 ,x ,y) ,p))

;;; Modular scalar product

(defmacro QMODDOT0 (eltfun varg1 varg2 ind1 ind2 kk s0 p)
    `(let ((s ,s0)
           (v1 ,varg1)
           (v2 ,varg2)
           (i1 ,ind1)
           (i2 ,ind2)
           (k0 ,kk)
           (k 0))
          (declare (type machine-int s)
                   (type fixnum i1 i2 k k0))
          (prog ()
             l1
              (if (>= k k0) (return (QSMOD64-32 s ,p)))
              (setf s (QSMULADD64-32 (,eltfun v1 (QSPLUS i1 k))
                                     (,eltfun v2 (QSPLUS i2 k))
                                     s))
              (setf k (QSPLUS k 1))
              (go l1))))

(defmacro QMODDOT32 (v1 v2 ind1 ind2 kk s0 p)
     `(QMODDOT0 ELT32 ,v1 ,v2 ,ind1 ,ind2 ,kk ,s0 ,p))

;;; Floating point macros

;; Closure CL has buggy floating point optimizer, so for it we need
;; to omit type declarations to disable optimization
#-:openmcl
(defmacro DEF-DF-BINOP (name op)
   `(defmacro ,name (x y) `(the double-float (,',op (the double-float ,x)
                                                    (the double-float ,y)))))
#+:openmcl
(defmacro DEF-DF-BINOP (name op) `(defmacro ,name (x y) `(,',op ,x ,y)))

(DEF-DF-BINOP ADD-DF +)
(DEF-DF-BINOP MUL-DF *)
(DEF-DF-BINOP MAX-DF MAX)
(DEF-DF-BINOP MIN-DF MIN)
(DEF-DF-BINOP SUB-DF -)
(DEF-DF-BINOP DIV-DF /)

#-:openmcl
(progn
(defmacro LESS-DF (x y) `(< (the double-float ,x)
                                             (the double-float ,y)))
(defmacro EQL-DF (x y) `(EQL (the double-float ,x)
                                             (the double-float ,y)))
(defmacro EXPT-DF-I (x y) `(EXPT (the double-float ,x)
                                 (the integer ,y)))
(defmacro EXPT-DF-DF (x y) `(EXPT (the double-float ,x)
                                  (the double-float ,y)))
(defmacro MUL-DF-I (x y) `(* (the double-float ,x)
                                  (the integer ,y)))
(defmacro DIV-DF-I (x y) `(/ (the double-float ,x)
                                  (the integer ,y)))
(defmacro ZEROP-DF (x) `(ZEROP (the double-float ,x)))
(defmacro MINUSP-DF (x) `(MINUSP (the double-float ,x)))
(defmacro SQRT-DF(x) `(SQRT (the double-float ,x)))
(defmacro LOG-DF (x) `(LOG (the double-float ,x)))

(defmacro DEF-DF-UNOP (name op)
    `(defmacro ,name (x) `(the double-float (,',op (the double-float ,x)))))
)

#+:openmcl
(progn
(defmacro LESS-DF (x y) `(<  ,x ,y))
(defmacro EQL-DF (x y) `(EQL ,x ,y))
(defmacro EXPT-DF-I (x y) `(EXPT ,x ,y))
(defmacro EXPT-DF-DF (x y) `(EXPT ,x ,y))
(defmacro MUL-DF-I (x y) `(* ,x ,y))
(defmacro DIV-DF-I (x y) `(/ ,x ,y))
(defmacro ZEROP-DF (x) `(ZEROP ,x))
(defmacro MINUSP-DF (x) `(MINUSP ,x))
(defmacro SQRT-DF(x) `(SQRT ,x))
(defmacro LOG-DF (x) `(LOG ,x))

(defmacro DEF-DF-UNOP (name op)
    `(defmacro ,name (x) `(,',op ,x)))
)


(DEF-DF-UNOP EXP-DF EXP)
(DEF-DF-UNOP MINUS-DF -)
(DEF-DF-UNOP SIN-DF SIN)
(DEF-DF-UNOP COS-DF COS)
(DEF-DF-UNOP TAN-DF TAN)
(DEF-DF-UNOP ATAN-DF ATAN)
(DEF-DF-UNOP SINH-DF SINH)
(DEF-DF-UNOP COSH-DF COSH)
(DEF-DF-UNOP TANH-DF TANH)
(DEF-DF-UNOP QSQRT-DF SQRT)
(DEF-DF-UNOP QLOG-DF LOG)

;;; Double precision arrays and matrices

(defmacro MAKE-DOUBLE-VECTOR (n)
   `(make-array (list ,n) :element-type 'double-float))

(defmacro MAKE-DOUBLE-VECTOR1 (n s)
   `(make-array (list ,n) :element-type 'double-float :initial-element ,s))

(defmacro DELT(v i)
   `(aref (the (simple-array double-float (*)) ,v) ,i))

(defmacro DSETELT(v i s)
   `(setf (aref (the (simple-array double-float (*)) ,v) ,i)
           ,s))

(defmacro DLEN(v)
    `(length (the (simple-array double-float (*)) ,v)))

(defmacro MAKE-DOUBLE-MATRIX (n m)
   `(make-array (list ,n ,m) :element-type 'double-float))

(defmacro MAKE-DOUBLE-MATRIX1 (n m s)
   `(make-array (list ,n ,m) :element-type 'double-float
           :initial-element ,s))

(defmacro DAREF2(v i j)
   `(aref (the (simple-array double-float (* *)) ,v) ,i ,j))

(defmacro DSETAREF2(v i j s)
   `(setf (aref (the (simple-array double-float (* *)) ,v) ,i ,j)
          ,s))

(defmacro DANROWS(v)
    `(array-dimension (the (simple-array double-float (* *)) ,v) 0))

(defmacro DANCOLS(v)
    `(array-dimension (the (simple-array double-float (* *)) ,v) 1))

;;; We implement complex array as arrays of doubles -- each
;;; complex number occupies two positions in the real
;;; array.

(defmacro MAKE-CDOUBLE-VECTOR (n)
   `(make-array (list (* 2 ,n)) :element-type 'double-float))

(defmacro CDELT(ov oi)
   (let ((v (gensym))
         (i (gensym)))
   `(let ((,v ,ov)
          (,i ,oi))
      (cons
          (aref (the (simple-array double-float (*)) ,v) (* 2 ,i))
          (aref (the (simple-array double-float (*)) ,v) (+ (* 2 ,i) 1))))))

(defmacro CDSETELT(ov oi os)
   (let ((v (gensym))
         (i (gensym))
         (s (gensym)))
   `(let ((,v ,ov)
          (,i ,oi)
          (,s ,os))
        (setf (aref (the (simple-array double-float (*)) ,v) (* 2 ,i))
           (car ,s))
        (setf (aref (the (simple-array double-float (*)) ,v) (+ (* 2 ,i) 1))
           (cdr ,s))
        ,s)))

(defmacro CDLEN(v)
    `(truncate (length (the (simple-array double-float (*)) ,v)) 2))

(defmacro MAKE-CDOUBLE-MATRIX (n m)
   `(make-array (list ,n (* 2 ,m)) :element-type 'double-float))

(defmacro CDAREF2(ov oi oj)
   (let ((v (gensym))
         (i (gensym))
         (j (gensym)))
   `(let ((,v ,ov)
          (,i ,oi)
          (,j ,oj))
        (cons
            (aref (the (simple-array double-float (* *)) ,v) ,i (* 2 ,j))
            (aref (the (simple-array double-float (* *)) ,v)
                  ,i (+ (* 2 ,j) 1))))))

(defmacro CDSETAREF2(ov oi oj os)
   (let ((v (gensym))
         (i (gensym))
         (j (gensym))
         (s (gensym)))
   `(let ((,v ,ov)
          (,i ,oi)
          (,j ,oj)
          (,s ,os))
         (setf (aref (the (simple-array double-float (* *)) ,v) ,i (* 2 ,j))
               (car ,s))
         (setf (aref (the (simple-array double-float (* *)) ,v)
                     ,i (+ (* 2 ,j) 1))
               (cdr ,s))
         ,s)))

(defmacro CDANROWS(v)
    `(array-dimension (the (simple-array double-float (* *)) ,v) 0))

(defmacro CDANCOLS(v)
    `(truncate
         (array-dimension (the (simple-array double-float (* *)) ,v) 1) 2))


(defstruct (SPAD-KERNEL 
          (:print-function
               (lambda (p s k)
                   (format s "#S~S" (list 
                        'SPAD-KERNEL
                         :OP (SPAD-KERNEL-OP p) 
                         :ARG (SPAD-KERNEL-ARG p)
                         :NEST (SPAD-KERNEL-NEST p))))))
           OP ARG NEST (POSIT 0))

(defmacro SET-SPAD-KERNEL-POSIT(s p) `(setf (SPAD-KERNEL-POSIT ,s) ,p))

(defun |makeSpadKernel|(o a n) (MAKE-SPAD-KERNEL :OP o :ARG a :NEST n))
