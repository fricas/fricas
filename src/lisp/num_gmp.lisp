(in-package "FRICAS-LISP")

(locally
  (declare (optimize (speed 3) (safety 0)))

#+:openmcl
(progn

(CCL::defx86lapfunction gmp-bignum-copy-from-lisp
    ((x arg_x) (y arg_y) (l arg_z))
    (movq ($ 0) (% imm1))
    (movq (@ x8664::misc-data-offset (% y)) (% imm2))
   @loop
    (movq (@ x8664::misc-data-offset (% x) (% imm1)) (% imm0))
    (movq (% imm0) (@ (% imm2) (% imm1)))
    (addq ($ 8) (% imm1))
    (cmpq (% imm1) (% l))
    (jnz @loop)
    (single-value-return))

(CCL::defx86lapfunction gmp-bignum-copy-negate-from-lisp
    ((x arg_x) (y arg_y) (l arg_z))
    (movq ($ 8) (% temp0))
    (andq (% l) (% temp0))
    (xorq (% temp0) (% l))
    (shrq ($ 1) (% l))
    (xorq (% imm1) (% imm1))
    (movq (@ x8664::misc-data-offset (% y)) (% imm2))
    (clc)
   @loop1
    (movq (@ x8664::misc-data-offset (% x) (% imm1)) (% imm0))
    (cmpq ($ 0) (% imm0))
    (jnz @negate)
    (movq ($ 0) (@ (% imm2) (% imm1)))
    (addq ($ 8) (% imm1))
    (cmpq (% imm1) (% l))
    (jnz @loop1)
    (cmpq ($ 0) (% temp0))
    (jz @return)
    (movslq (@ x8664::misc-data-offset (% x) (% imm1)) (% imm0))
    (xorq (% temp0) (% temp0))
   @negate
    (neg (% imm0))
    (movq (% imm0) (@ (% imm2) (% imm1)))
    (addq ($ 8) (% imm1))
    (cmpq (% imm1) (% l))
    (jle @finish)
   @loop2
    (movq (@ x8664::misc-data-offset (% x) (% imm1)) (% imm0))
    (notq (% imm0))
    (movq (% imm0) (@ (% imm2) (% imm1)))
    (addq ($ 8) (% imm1))
    (cmpq (% imm1) (% l))
    (jnz @loop2)
   @finish
    (cmpq ($ 0) (% temp0))
    (jz @return)
    (movslq (@ x8664::misc-data-offset (% x) (% imm1)) (% imm0))
    (notq (% imm0))
    (movq (% imm0) (@ (% imm2) (% imm1)))
   @return
    (single-value-return))

(CCL::defx86lapfunction gmp-bignum-copy-to-lisp
    ((x arg_x) (y arg_y) (l arg_z))
    (movq ($ 0) (% imm1))
    (movq (@ x8664::misc-data-offset (% x)) (% imm2))
   @loop
    (movq (@ (% imm2) (% imm1)) (% imm0))
    (movq (% imm0) (@ x8664::misc-data-offset (% y) (% imm1)))
    (addq ($ 8) (% imm1))
    (cmpq (% imm1) (% l))
    (jnz @loop)
    (single-value-return))

(CCL::defx86lapfunction gmp-bignum-copy-negate-to-lisp
    ((x arg_x) (y arg_y) (l arg_z))
    (xorq (% imm1) (% imm1))
    (movq (@ x8664::misc-data-offset (% x)) (% imm2))
   @loop1
    (movq (@ (% imm2) (% imm1)) (% imm0))
    (cmpq ($ 0) (% imm0))
    (jnz @negate)
    (movq ($ 0) (@ x8664::misc-data-offset (% y) (% imm1)))
    (addq ($ 8) (% imm1))
    (cmpq (% imm1) (% l))
    (jnz @loop1)
    (jmp @return)
   @negate
    (neg (% imm0))
    (movq (% imm0) (@ x8664::misc-data-offset (% y) (% imm1)))
    (addq ($ 8) (% imm1))
    (cmpq (% imm1) (% l))
    (jz @return)
   @loop2
    (movq (@ (% imm2) (% imm1)) (% imm0))
    (notq (% imm0))
    (movq (% imm0) (@ x8664::misc-data-offset (% y) (% imm1)))
    (addq ($ 8) (% imm1))
    (cmpq (% imm1) (% l))
    (jnz @loop2)
   @return
    (single-value-return))

(CCL::defx86lapfunction gmp-bignum-copy
    ((x arg_x) (y arg_y) (l arg_z))
    (movq ($ 0) (% imm1))
   @loop
    (movq (@ x8664::misc-data-offset (% x) (% imm1)) (% imm0))
    (movq (% imm0) (@ x8664::misc-data-offset (% y) (% imm1)))
    (addq ($ 8) (% imm1))
    (cmpq (% imm1) (% l))
    (jnz @loop)
    (single-value-return))

(defun gmp-bignum-isqrt (x)
  (let* ((xl (ceiling (ccl::%bignum-length x) 2))
         (rl (ceiling xl 2))
         (xlb (ash xl 3))
         (rlb (ash rl 3))
         (rl2 (+ rl rl))
         (res (ccl::%alloc-misc rl2 X8664::SUBTAG-BIGNUM)))
        (declare (type fixnum xl rl xlb rlb rl2))
      (ccl::%stack-block ((tx xlb)
                          (tr rlb))
         (gmp-bignum-copy-from-lisp x tx xl)
         (ccl::external-call "gmp_wrap_isqrt"
             :address tr :long rl
             :address tx :long xl)
         (gmp-bignum-copy-to-lisp tr res rl)
         (ccl::%normalize-bignum-2 t res))))

(if (not (fboundp 'orig-multiply-bignums))
    (setf (symbol-function 'orig-multiply-bignums)
          (symbol-function 'ccl::multiply-bignums)))

(if (not (fboundp 'orig-bignum-gcd))
    (setf (symbol-function 'orig-positive-bignum-gcd)
          (symbol-function 'ccl::%positive-bignum-bignum-gcd)))

(if (not (fboundp 'orig-bignum-truncate))
    (setf (symbol-function 'orig-bignum-truncate)
          (symbol-function 'ccl::bignum-truncate)))

(if (not (fboundp 'orig-isqrt))
    (setf (symbol-function 'orig-isqrt)
          (symbol-function 'common-lisp:isqrt)))

(defun gmp-multiply-bignums(x y)
  (let ((xl0 (ccl::%bignum-length x))
        (yl0 (ccl::%bignum-length y)))
       (declare (type fixnum xl0 yl0))
  (if (< xl0 30)
      (return-from gmp-multiply-bignums (orig-multiply-bignums x y)))
  (if (< yl0 30)
      (return-from gmp-multiply-bignums (orig-multiply-bignums y x)))
  (if (< (+ xl0 yl0) 120)
      (return-from gmp-multiply-bignums (orig-multiply-bignums x y)))
  (let* ((xl (ceiling xl0 2))
         (yl (ceiling yl0 2))
         (x-plusp nil)
         (y-plusp nil)
         (rl (+ xl yl))
         (rl2 (+ rl rl))
         (xlb 0)
         (ylb 0)
         (itmp 0)
         (tmp nil)
         (rlb 0)
         (res (ccl::%alloc-misc rl2 X8664::SUBTAG-BIGNUM)))
        (declare (type fixnum xl yl rl rl2 xlb ylb rlb itmp))
        ;;; XXX Does not work
        ;;; (declare (dynamic-extent res))
      (if (< xl yl)
          (progn
              (setf itmp xl)
              (setf xl yl)
              (setf yl itmp)
              (setf itmp xl0)
              (setf xl0 yl0)
              (setf yl0 itmp)
              (setf tmp x)
              (setf x y)
              (setf y tmp)))
      (setf xlb (ash xl 3))
      (setf ylb (ash yl 3))
      (setf rlb (+ xlb ylb))
      (setf x-plusp (ccl::%bignum-0-or-plusp x xl0))
      (setf y-plusp (ccl::%bignum-0-or-plusp y yl0))
      (ccl::%stack-block ((tx xlb)
                          (ty ylb)
                          (tr rlb))
         (if x-plusp
             (gmp-bignum-copy-from-lisp x tx xl)
             (gmp-bignum-copy-negate-from-lisp x tx xl0))
         (if y-plusp
             (gmp-bignum-copy-from-lisp y ty yl)
             (gmp-bignum-copy-negate-from-lisp y ty yl0))
         (ccl::external-call "__gmpn_mul"
                  :address tr
                  :address tx :long xl
                  :address ty :long yl)
         (if (eq x-plusp y-plusp)
             (gmp-bignum-copy-to-lisp tr res rl)
             (gmp-bignum-copy-negate-to-lisp tr res rl))
         (ccl::%normalize-bignum-2 t res)))))


(defun gmp-positive-bignum-gcd(x y)
  (let* ((xl (ceiling (ccl::%bignum-length x) 2))
         (yl (ceiling (ccl::%bignum-length y) 2))
         (rl (if (< xl yl) xl yl))
         (xlb (ash xl 3))
         (ylb (ash yl 3))
         (rlb (+ xlb ylb))
         (res nil))
        (declare (type fixnum xl yl rl xlb ylb rlb))
        ;;; XXX Does not work
        ;;; (declare (dynamic-extent res))
      (ccl::%stack-block ((tx xlb)
                          (ty ylb)
                          (tr rlb))
         (gmp-bignum-copy-from-lisp x tx xl)
         (gmp-bignum-copy-from-lisp y ty yl)
         (setf rl (ccl::external-call "gmp_wrap_gcd"
                     :address tr
                     :address tx :long xl
                     :address ty :long yl
                     :long))
         (setf res (ccl::%alloc-misc (+ rl rl) X8664::SUBTAG-BIGNUM))
         (gmp-bignum-copy-to-lisp tr res rl)
         (ccl::%normalize-bignum-2 t res))))
;;; Tests
;;;   (truncate -51520943106947801344 17339521378867071488)
;;;
(defun gmp-bignum-truncate (x y &optional norem)
    (declare (ignore norem))
    (if (and (eql (ccl::%bignum-length y) 1)
             (eql (ccl::%typed-miscref :bignum y 0) 0))
        (error (make-condition 'division-by-zero
                               :operation 'gmp-bignum-truncate
                               :operands (list x 0))))
    (let* ((x-plusp (ccl::%bignum-0-or-plusp x (ccl::%bignum-length x)))
           (y-plusp (ccl::%bignum-0-or-plusp y (ccl::%bignum-length y)))
           (x (if x-plusp x (ccl::negate-bignum x nil)))
           (y (if y-plusp y (ccl::negate-bignum y nil)))
           (yl0 (ccl::%bignum-length y))
           (xl (ceiling (ccl::%bignum-length x) 2))
           (yl1 (ceiling yl0 2))
           (yl (if (eq 0 (ccl::%typed-miscref :bignum y (- yl0 1)))
                   (ash yl0 -1)
                   yl1))
           (ql (max (+ 1 (- xl yl)) 1))
           (q nil)
           (r nil)
           (xlb (ash xl 3))
           (ylb (ash yl 3))
           (qlb (ash ql 3)))
      (declare (type fixnum xl yl yl0 yl1 ql xlb ylb qlb))
      (if (plusp (ccl::bignum-compare y x))
        (progn
               (setf r (ccl::%alloc-misc (+ xl xl) X8664::SUBTAG-BIGNUM))
               (gmp-bignum-copy x r xl)
               (setf q 0))
        (ccl::%stack-block ((tx xlb)
                          (ty ylb)
                          (tq qlb)
                          (tr ylb))
         (gmp-bignum-copy-from-lisp x tx xl)
         (gmp-bignum-copy-from-lisp y ty yl)
         (ccl::external-call "__gmpn_tdiv_qr"
                     :address tq :address tr :long 0
                     :address tx :long xl
                     :address ty :long yl)
         (setf q (ccl::%alloc-misc (+ ql ql) X8664::SUBTAG-BIGNUM))
         (setf r (ccl::%alloc-misc (+ yl1 yl) X8664::SUBTAG-BIGNUM))
         (gmp-bignum-copy-to-lisp tq q ql)
         (gmp-bignum-copy-to-lisp tr r yl)
         (if (> yl1 yl)
             (setf (ccl::%typed-miscref :bignum r (- yl0 1)) 0))
         (setf q (ccl::%normalize-bignum-2 t q))
         (setf r (ccl::%normalize-bignum-2 t r))))
      (let ((quotient (cond ((eq x-plusp y-plusp) q)
                            ((typep q 'fixnum) (the fixnum (- q)))
                            (t (ccl::negate-bignum-in-place q))))
            (rem (cond (x-plusp r)
                       ((typep r 'fixnum) (the fixnum (- r)))
                       (t (ccl::negate-bignum-in-place r)))))
            (values (if (typep quotient 'fixnum)
                        quotient
                        (ccl::%normalize-bignum-2 t quotient))
                    (if (typep rem 'fixnum)
                        rem
                        (ccl::%normalize-bignum-2 t rem))))))

)

;;; Tests
;;; (gmp-bignum-isqrt (expt 10 50))
;;; (gmp-bignum-isqrt (expt 2 127))
#+:sbcl
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
             (sb-alien:extern-alien "gmp_wrap_sb_isqrt"
                 (sb-alien:function sb-alien:void (* t) (* t)))
             sapx sapr)))
        (sb-bignum::%normalize-bignum res len-res)))

(defun gmp-isqrt (n)
    (check-type n unsigned-byte)
    (if (not #+:sbcl(sb-int:fixnump n)
             #+:openmcl(ccl:fixnump n))
        (return-from gmp-isqrt (gmp-bignum-isqrt n)))
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

(defparameter *gmp-multiplication-initialized* nil)

#+:openmcl
(progn

(defun install-gmp-multiplication()
    (let ((package *PACKAGE*))
    (ccl:SET-DEVELOPMENT-ENVIRONMENT T)
    (setf (symbol-function 'ccl::multiply-bignums)
          (symbol-function 'gmp-multiply-bignums))
    (setf (symbol-function 'ccl::bignum-truncate)
          (symbol-function 'gmp-bignum-truncate))
    (setf (symbol-function 'ccl::%positive-bignum-bignum-gcd)
          (symbol-function 'gmp-positive-bignum-gcd))
    (setf (symbol-function 'common-lisp:isqrt)
          (symbol-function 'gmp-isqrt))
    (ccl:SET-USER-ENVIRONMENT T)
    (setf *PACKAGE* package))
)

(defun unistall-gmp-multiplication()
   (let ((package *PACKAGE*))
    (ccl:SET-DEVELOPMENT-ENVIRONMENT T)
   (setf (symbol-function 'ccl::multiply-bignums)
          (symbol-function 'orig-multiply-bignums))
    (setf (symbol-function 'ccl::bignum-truncate)
          (symbol-function 'orig-bignum-truncate))
    (setf (symbol-function 'ccl::%positive-bignum-bignum-gcd)
          (symbol-function 'orig-positive-bignum-gcd))
    (setf (symbol-function 'common-lisp:isqrt)
          (symbol-function 'orig-isqrt))
    (ccl:SET-USER-ENVIRONMENT T)
    (setf *PACKAGE* package))
)

(defun init-gmp(wrapper-lib)
    (if (not *gmp-multiplication-initialized*)
        (if (ignore-errors (ccl::open-shared-library "libgmp.so") t)
            (if (ignore-errors
                    (ccl::open-shared-library wrapper-lib) t)
                 (install-gmp-multiplication)
                 (setf *gmp-multiplication-initialized* t)))))
)

#+:sbcl
(progn
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
                (sb-alien:extern-alien "gmp_wrap_sb_mul"
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
                (sb-alien:extern-alien "gmp_wrap_sb_gcd"
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
                (sb-alien:extern-alien "gmp_wrap_sb_div_rem"
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
)
