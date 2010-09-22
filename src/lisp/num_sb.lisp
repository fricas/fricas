(in-package "FRICAS-LISP")

(locally
  (declare (optimize (speed 3) (safety 0)))

;;; Tests
;;; (my-bignum-isqrt (expt 10 50))
;;; (my-bignum-isqrt (expt 2 127))
(defun gmp-bignum-isqrt (x)
  (let* ((len-x (sb-bignum::%bignum-length x))
         (len-res (ceiling len-x 2))
         (res (sb-bignum::%allocate-bignum len-res)))
        (declare (type fixnum len-x len-res))
        (sb-sys:with-pinned-objects (x res)
          (let* ((addrx (sb-kernel:get-lisp-obj-address x))
                 (sapx (sb-sys:int-sap addrx))
                 (addr-res (sb-kernel:get-lisp-obj-address res))
                 (sapr (sb-sys:int-sap addr-res)))
         (sb-alien:alien-funcall
             (sb-alien:extern-alien "gmp_sb_isqrt"
                 (sb-alien:function sb-alien:void (* t) (* t)))
             sapx sapr)))
        (sb-bignum::%normalize-bignum res len-res)))


(defun gmp-isqrt (n)
    (check-type n unsigned-byte)
    (if (not (sb-int:fixnump n)) (return-from gmp-isqrt (gmp-bignum-isqrt n)))
    (locally
        (declare (type fixnum n))
        (if (<= n 24)
            (cond ((> n 15) 4)
                  ((> n  8) 3)
                  ((> n  3) 2)
                  ((> n  0) 1)
                  (t 0))
        (let* ((n-len-quarter (ash (integer-length n) -2))
               (n-half (ash n (- (ash n-len-quarter 1))))
               (n-half-isqrt (isqrt n-half))
               (init-value (ash (1+ n-half-isqrt) n-len-quarter)))
            (declare (type fixnum n-len-quarter n-half 
                              n-half-isqrt init-value))
            (loop
                (let ((iterated-value
                        (ash (+ init-value (truncate n init-value)) -1)))
                    (unless (< iterated-value init-value)
                         (return init-value))
                    (setq init-value iterated-value)))))))


(if (not (fboundp 'orig-multiply-bignums))
    (setf (symbol-function 'orig-multiply-bignums)
          (symbol-function 'sb-bignum::multiply-bignums)))

(if (not (fboundp 'orig-bignum-gcd))
    (setf (symbol-function 'orig-bignum-gcd)
          (symbol-function 'sb-bignum::bignum-gcd)))

(if (not (fboundp 'orig-bignum-truncate))
    (setf (symbol-function 'orig-bignum-truncate)
          (symbol-function 'sb-bignum::bignum-truncate)))

(if (not (fboundp 'orig-isqrt))
    (setf (symbol-function 'orig-isqrt)
          (symbol-function 'common-lisp:isqrt)))

(defun gmp-multiply-bignums0 (a b)
  ;;; (declare (type bignum-type a b))
  (let* ((a-plusp (sb-bignum::%bignum-0-or-plusp a
                  (sb-bignum::%bignum-length a)))
         (b-plusp (sb-bignum::%bignum-0-or-plusp b 
                  (sb-bignum::%bignum-length b)))
         (a (if a-plusp a (sb-bignum::negate-bignum a)))
         (b (if b-plusp b (sb-bignum::negate-bignum b)))
         (len-a (sb-bignum::%bignum-length a))
         (len-b (sb-bignum::%bignum-length b))
         (len-res (+ len-a len-b))
         (res (sb-bignum::%allocate-bignum len-res))
         (negate-res (not (eql a-plusp b-plusp))))
    ;;; (declare (type bignum-index len-a len-b len-res))
        (if (< len-a len-b)
            (let ((tmp a))
                (setf a b)
                (setf b tmp)))
        (sb-sys:with-pinned-objects (a b res)
          (let* ((addra (sb-kernel:get-lisp-obj-address a))
                 (sapa (sb-sys:int-sap addra))
                 (addrb (sb-kernel:get-lisp-obj-address b))
                 (sapb (sb-sys:int-sap addrb))
                 (addr-res (sb-kernel:get-lisp-obj-address res))
                 (sap-res (sb-sys:int-sap addr-res)))
            (sb-alien:alien-funcall
                (sb-alien:extern-alien "gmp_sb_mul"
                   (sb-alien:function sb-alien:void (* t) (* t) (* t)))
                sapa sapb sap-res)))
        (when negate-res (sb-bignum::negate-bignum-in-place res))
    (sb-bignum::%normalize-bignum res len-res)

  )
)

(defun gmp-multiply-bignums(x y)
  (let ((xl0 (sb-bignum::%bignum-length x))
        (yl0 (sb-bignum::%bignum-length y)))
       (declare (type fixnum xl0 yl0))
  (if (< xl0 10)
      (return-from gmp-multiply-bignums (orig-multiply-bignums x y)))
  (if (< yl0 10)
      (return-from gmp-multiply-bignums (orig-multiply-bignums y x)))
  (if (< (+ xl0 yl0) 40)
      (return-from gmp-multiply-bignums (orig-multiply-bignums x y)))
  (gmp-multiply-bignums0 x y)))

(defun gmp-bignum-gcd(x y)
  (let* (
    (nx (if (sb-bignum::%bignum-0-or-plusp x (sb-bignum::%bignum-length x))
            (sb-bignum::copy-bignum x)
            (sb-bignum::negate-bignum x nil)))
    (ny (if (sb-bignum::%bignum-0-or-plusp y (sb-bignum::%bignum-length y))
            (sb-bignum::copy-bignum y)
            (sb-bignum::negate-bignum y nil)))
    (xl (sb-bignum::%bignum-length nx))
    (yl (sb-bignum::%bignum-length ny))
    (rl (if (< xl yl) xl yl))
    (res (sb-bignum::%allocate-bignum rl)))
        (sb-sys:with-pinned-objects (nx ny res)
          (let* ((addrx (sb-kernel:get-lisp-obj-address nx))
                 (sapx (sb-sys:int-sap addrx))
                 (addry (sb-kernel:get-lisp-obj-address ny))
                 (sapy (sb-sys:int-sap addry))
                 (addr-res (sb-kernel:get-lisp-obj-address res))
                 (sap-res (sb-sys:int-sap addr-res)))
            (sb-alien:alien-funcall
                (sb-alien:extern-alien "gmp_sb_gcd"
                   (sb-alien:function sb-alien:void (* t) (* t) (* t)))
                sapx sapy sap-res)))
    (sb-bignum::%normalize-bignum res rl)
  )
)

(defun test-bignum-gcd(x y)
   (let ((res1 (orig-bignum-gcd x y))
         (res2 (gmp-bignum-gcd x y)))
        (if (not (equal res1 res2))
            (format t 
               "Different results from gcd ~S ~S, orig ~S, gmp ~S ~%"
               x y res1 res2))
        res2))


(defun gmp-bignum-truncate(x y)
  (let* ( 
    (x-plusp (sb-bignum::%bignum-0-or-plusp x (sb-bignum::%bignum-length x)))
    (y-plusp (sb-bignum::%bignum-0-or-plusp y (sb-bignum::%bignum-length y)))
    (nx (if x-plusp x
           (sb-bignum::negate-bignum x nil)))
    (ny (if y-plusp y
           (sb-bignum::negate-bignum y nil)))
    (len-x (sb-bignum::%bignum-length nx))
    (len-y (sb-bignum::%bignum-length ny))
    (q nil)
    (r nil)
    )
    (if (plusp (sb-bignum::bignum-compare ny nx))
        (progn 
            (setf q 0)
            (setf r (if y-plusp (sb-bignum::copy-bignum nx) nx))
        )
        (let* (
            (len-r len-y)
            (len-y (if (eql 0 (sb-bignum::%bignum-ref ny (- len-y 1)))
                       (- len-y 1)
                       len-y))
            (len-q (+ 1 (- len-x len-y)))
            (nq (sb-bignum::%allocate-bignum len-q))
            (nr (sb-bignum::%allocate-bignum len-r)))
          (sb-sys:with-pinned-objects (nx ny nq nr)
            (let* (
                 (addrx (sb-kernel:get-lisp-obj-address nx))
                 (sapx (sb-sys:int-sap addrx))
                 (addry (sb-kernel:get-lisp-obj-address ny))
                 (sapy (sb-sys:int-sap addry))
                 (addr-quo (sb-kernel:get-lisp-obj-address nq))
                 (sapq (sb-sys:int-sap addr-quo))
                 (addr-rem (sb-kernel:get-lisp-obj-address nr))
                 (sapr (sb-sys:int-sap addr-rem)))
            (sb-alien:alien-funcall
                (sb-alien:extern-alien "gmp_sb_div_rem"
                   (sb-alien:function sb-alien:void (* t) (* t) (* t) (* t)))
                sapx sapy sapq sapr)))
          (setf q (sb-bignum::%normalize-bignum nq len-q))
          (setf r (sb-bignum::%normalize-bignum nr len-r))))
    (let ((quotient (cond ((eql x-plusp y-plusp) q)
                            ((typep q 'fixnum) (the fixnum (- q)))
                            (t (sb-bignum::negate-bignum-in-place q))))
          (rem (cond (x-plusp r)
                     ((typep r 'fixnum) (the fixnum (- r)))
                     (t (sb-bignum::negate-bignum-in-place r)))))
          (values (if (typep quotient 'fixnum)
                      quotient
                      (sb-bignum::%normalize-bignum quotient
                          (sb-bignum::%bignum-length quotient)))
                  (if (typep rem 'fixnum)
                      rem
                      (sb-bignum::%normalize-bignum rem
                          (sb-bignum::%bignum-length rem)))))))

 

#|
;;; Tests
;;;   (truncate -51520943106947801344 17339521378867071488)
;;;   (truncate 23215968175662844254 12149601698671348626)
;;;   (truncate 1666974137583209287393566720 -2023369608)
|#

(defparameter *gmp-multiplication-initialized* nil)

(defun init-gmp(wrapper-lib)
    (if (not *gmp-multiplication-initialized*)
        (if (ignore-errors (sb-alien::load-shared-object "libgmp.so") t)
            (if (ignore-errors
                    (sb-alien::load-shared-object wrapper-lib) t)
                 (install-gmp-multiplication)
                 (setf *gmp-multiplication-initialized* t)))))

(defun install-gmp-multiplication()
    (sb-ext:unlock-package "SB-BIGNUM")
    (setf (symbol-function 'sb-bignum::multiply-bignums)
          (symbol-function 'gmp-multiply-bignums))
    (setf (symbol-function 'sb-bignum::bignum-truncate)
          (symbol-function 'gmp-bignum-truncate))
    (setf (symbol-function 'sb-bignum::bignum-gcd)
          (symbol-function 'gmp-bignum-gcd))
    (sb-ext:lock-package "SB-BIGNUM")
    (sb-ext:unlock-package "COMMON-LISP")
    (setf (symbol-function 'common-lisp:isqrt)
          (symbol-function 'gmp-isqrt))
    (sb-ext:lock-package "COMMON-LISP")
)

(defun unistall-gmp-multiplication()
    (sb-ext:unlock-package "SB-BIGNUM")
    (setf (symbol-function 'sb-bignum::multiply-bignums)
          (symbol-function 'orig-multiply-bignums))
    (setf (symbol-function 'sb-bignum::bignum-truncate)
          (symbol-function 'orig-bignum-truncate))
    (setf (symbol-function 'sb-bignum::bignum-gcd)
          (symbol-function 'orig-bignum-gcd))
    (sb-ext:lock-package "SB-BIGNUM"))
    (sb-ext:unlock-package "COMMON-LISP")
    (setf (symbol-function 'common-lisp:isqrt)
          (symbol-function 'orig-isqrt))
    (sb-ext:lock-package "COMMON-LISP")

)

