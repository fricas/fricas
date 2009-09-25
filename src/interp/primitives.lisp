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

(defun GETREFV32(n x) (make-array n :initial-element x
                                     :element-type '(unsigned-byte 32)))

(defmacro QV32LEN(v)
    `(length (the (simple-array (unsigned-byte 32) (*)) ,v)))

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
