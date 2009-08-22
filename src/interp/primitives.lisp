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
