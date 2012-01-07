;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     - Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     - Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;;     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;      VM LISP EMULATION PACKAGE
;      Lars Ericson, Barry Trager, Martial Schor, tim daly, LVMCL, et al
;      IBM Thomas J. Watson Research Center
;      Summer, 1986
;  see /spad/daly.changes

; This emulation package version is written for Symbolics Common Lisp.
; Emulation commentary refers to LISP/VM, IBM Program Number 5798-DQZ,
; as described in the LISP/VM User's Guide, document SH20-6477-1.
; Main comment section headings refer to sections in the User's Guide.

; If you are using this, you are probably in Common Lisp, yes?

(in-package "BOOT")

;; DEFVARS

(defvar curoutstream (make-synonym-stream '*standard-output*))

(defvar *fileactq-apply* nil "function to apply in fileactq")

;; DEFMACROS


(defmacro add1 (x)
 `(1+ ,x))

(defmacro assq (a b)
 `(assoc ,a ,b :test #'eq))

(defmacro difference (&rest args)
 `(- ,@args))

(defmacro eqcar (x y)
  (if (atom x)
    `(and (consp ,x) (eql (qcar ,x) ,y))
    (let ((xx (gensym)))
     `(let ((,xx ,x))
       (and (consp ,xx) (eql (qcar ,xx) ,y))))))

(defmacro exit (&rest value)
 `(return-from seq ,@value))

(defmacro fetchchar (x i)
 `(char ,x ,i))

(defmacro fixp (x)
 `(integerp ,x))

(defmacro greaterp (&rest args)
 `(> ,@args))

(defmacro |idChar?| (x)
 `(or (alphanumericp ,x) (member ,x '(#\? #\% #\' #\!) :test #'char=)))

(defmacro identp (x)
 (if (atom x)
  `(and ,x (symbolp ,x))
   (let ((xx (gensym)))
    `(let ((,xx ,x))
      (and ,xx (symbolp ,xx))))))

(defmacro ifcar (x)
  (if (atom x)
      `(and (consp ,x) (qcar ,x))
    (let ((xx (gensym)))
      `(let ((,xx ,x))
         (and (consp ,xx) (qcar ,xx))))))

(defmacro ifcdr (x)
  (if (atom x)
      `(and (consp ,x) (qcdr ,x))
    (let ((xx (gensym)))
      `(let ((,xx ,x))
         (and (consp ,xx) (qcdr ,xx))))))

(defmacro LASTNODE (l)
 `(last ,l))

(defmacro lessp (&rest args)
 `(< ,@args))

(defmacro makestring (a) a)

(defmacro maxindex (x)
 `(the fixnum (1- (the fixnum (length ,x)))))

(defmacro minus (x)
 `(- ,x))

(defmacro ne (a b) `(not (equal ,a ,b)))

(defmacro neq (a b) `(not (eq ,a ,b)))

(defmacro pairp (x)
 `(consp ,x))

(defmacro plus (&rest args)
 `(+ ,@ args))

(defmacro qcar (x)
 `(car (the cons ,x)))

(defmacro qcdr (x)
 `(cdr (the cons ,x)))

(defmacro qcaar (x)
 `(car (the cons (car (the cons ,x)))))

(defmacro qcadr (x)
 `(car (the cons (cdr (the cons ,x)))))

(defmacro qcdar (x)
 `(cdr (the cons (car (the cons ,x)))))

(defmacro qcddr (x)
 `(cdr (the cons (cdr (the cons ,x)))))

(defmacro qcsize (x)
 `(the fixnum (length (the simple-string ,x))))

(defmacro qlength (a)
 `(length ,a))

(defmacro qrefelt (vec ind)
 `(svref ,vec ,ind))

(defmacro qrplaca (a b)
 `(rplaca (the cons ,a) ,b))

(defmacro qrplacd (a b)
 `(rplacd (the cons ,a) ,b))

(defmacro qsadd1 (x)
 `(the fixnum (1+ (the fixnum ,x))))

(defmacro qsdifference (x y)
 `(the fixnum (- (the fixnum ,x) (the fixnum ,y))))

(defmacro qsetrefv (vec ind val)
 `(setf (svref ,vec (the fixnum ,ind)) ,val))

(defmacro qsetvelt (vec ind val)
 `(setf (svref ,vec (the fixnum ,ind)) ,val))

(defmacro qsgreaterp (a b)
 `(> (the fixnum ,a) (the fixnum ,b)))

(defmacro qsinc1 (x)
 `(the fixnum (1+ (the fixnum ,x))))

(defmacro qsleftshift (a b)
 `(the fixnum (ash (the fixnum ,a) (the fixnum ,b))))

(defmacro qslessp (a b)
 `(< (the fixnum ,a) (the fixnum ,b)))

(defmacro qsmax (x y)
 `(the fixnum (max (the fixnum ,x) (the fixnum ,y))))

(defmacro qsmin (x y)
 `(the fixnum (min (the fixnum ,x) (the fixnum ,y))))

(defmacro qsminus (x)
 `(the fixnum (minus (the fixnum ,x))))

(defmacro qsminusp (x)
 `(minusp (the fixnum ,x)))

(defmacro qsoddp (x)
 `(oddp (the fixnum ,x)))

(defmacro qsabsval (x)
  `(the fixnum (abs (the fixnum ,x))))

(defmacro qsplus (x y)
 `(the fixnum (+ (the fixnum ,x) (the fixnum ,y))))

(defmacro qssub1 (x)
 `(the fixnum (1- (the fixnum ,x))))

(defmacro qstimes (x y)
 `(the fixnum (* (the fixnum ,x) (the fixnum ,y))))

(defmacro qszerop (x)
 `(zerop (the fixnum ,x)))

(defmacro qvelt (vec ind)
 `(svref ,vec (the fixnum ,ind)))

(defmacro qvmaxindex (x)
 `(the fixnum (1- (the fixnum (length (the simple-vector ,x))))))

(defmacro qvsize (x)
 `(the fixnum (length (the simple-vector ,x))))

(defmacro qlessp(x y) `(< ,x ,y))

(defmacro refvecp (v) `(simple-vector-p ,v))

(defmacro resetq (a b)
 `(prog1 ,a (setq ,a ,b)))

(defmacro setandfileq (id item)
 `(eval-when (:execute :load-toplevel)
   (defparameter ,id ,item)))

(defmacro setelt (vec ind val)
 `(setf (elt ,vec ,ind) ,val))

(defmacro seq (&rest form)
  (let* ((body (reverse form))
         (val `(return-from seq ,(pop body))))
    (nsubstitute '(progn) nil body) ;don't treat NIL as a label
    `(block seq (tagbody ,@(nreverse body) ,val))))

(defmacro sintp (n)
 `(typep ,n 'fixnum))

(defmacro |startsId?| (x)
 `(or (alpha-char-p ,x) (member ,x '(#\? #\% #\!) :test #'char=)))

(defmacro stringlength (x)
 `(length (the string ,x)))

(defmacro subrp (x)
 `(compiled-function-p ,x))

(defmacro sub1 (x)
 `(1- ,x))

(defmacro times (&rest args)
 `(* ,@args))

(defmacro vec-setelt (vec ind val)
 `(setf (svref ,vec ,ind) ,val))

(defmacro vecp (v) `(simple-vector-p ,v))

;; defuns

(defun define-function (f v)
 (setf (symbol-function f) v))

(define-function 'tempus-fugit #'get-internal-run-time)

; 9.4 Vectors and Bpis

(defun FBPIP (item) (or (compiled-function-p item)
                        (and (symbolp item) (fboundp item)
                             (not (macro-function item))
                             (compiled-function-p (symbol-function item)))))

; 9.5 Identifiers

(defun gensymp (x) (and (symbolp x) (null (symbol-package x))))

(defun digitp (x)
  (or (and (symbolp x) (digitp (symbol-name x)))
      (and (characterp x) (digit-char-p x))
      (and (stringp x) (= (length x) 1) (digit-char-p (char x 0)))))

(defun dig2fix (x)
  (if (symbolp x)
    (digit-char-p (char (symbol-name x) 0))
    (digit-char-p x)))

(defun LOG2 (x) (LOG x 2.0))

; 9.13 Streams

#+GCL
(defun IS-CONSOLE (stream)
  (and (streamp stream) (output-stream-p stream)
       (eq (system:fp-output-stream stream)
           (system:fp-output-stream *terminal-io*))))

#-:GCL
(defun IS-CONSOLE (stream)
     (or (eq stream *standard-output*)
         (and (typep stream 'synonym-stream)
              (eq (SYNONYM-STREAM-SYMBOL stream) '*standard-output*))))

; 11.0 Operations on Identifiers

; 11.1 Creation

(defun upcase (l)
  (cond ((stringp l) (string-upcase l))
        ((identp l) (intern (string-upcase (symbol-name l))))
        ((characterp l) (char-upcase l))
        ((atom l) l)
        (t (mapcar #'upcase l))))

(defun downcase (l)
  (cond ((stringp l) (string-downcase l))
        ((identp l) (intern (string-downcase (symbol-name l))))
        ((characterp l) (char-downcase L))
        ((atom l) l)
        (t (mapcar #'downcase l))))

; 11.2 Accessing

;; note it is important that PNAME returns nil not an error for non-symbols
(defun pname (x)
  (cond ((symbolp x) (symbol-name x))
        ((characterp x) (string x))
        (t nil)))

(defun put (sym ind val) (setf (get sym ind) val))

(define-function 'MAKEPROP #'put)

; 12.0 Operations on Numbers

; 12.1 Conversion

; 12.2 Predicates

; 12.3 Computation


(defun QUOTIENT (x y)
  (cond ((or (floatp x) (floatp y)) (BREAK))
        (t (truncate x y))))

(defun REMAINDER (x y)
  (if (and (integerp x) (integerp y))
      (rem x y)
      (BREAK)))

(defun DIVIDE (x y)
  (if (and (integerp x) (integerp y))
      (multiple-value-list (truncate x y))
      (BREAK)))

(defun QSQUOTIENT (a b) (the fixnum (truncate (the fixnum a) (the fixnum b))))

(defun QSREMAINDER (a b) (the fixnum (rem (the fixnum a) (the fixnum b))))

; 13.3 Updating


(defun RPLPAIR (pair1 pair2)
  (RPLACA pair1 (CAR pair2))
  (RPLACD pair1 (CDR pair2)) pair1)

(defun RPLNODE (pair1 ca2 cd2)
 (RPLACA pair1 ca2)
 (RPLACD pair1 cd2) pair1)

; 14.0 Operations on Lists

; 14.1 Creation

(defun VEC2LIST (vec) (coerce vec 'list))

; note default test for union, intersection and set-difference is eql
(defun UNIONQ (l1 l2) (union l1 l2 :test #'eq))
(defun INTERSECTIONQ (l1 l2) (intersection l1 l2 :test #'eq))
(defun |member| (item sequence)
   (cond ((symbolp item) (member item sequence :test #'eq))
         ((stringp item) (member item sequence :test #'equal))
         ((and (atom item) (not (arrayp item))) (member item sequence))
         (T (member item sequence :test #'equalp))))

(defun |remove| (list item &optional (count 1))
  (if (integerp count)
      (remove item list :count count :test #'equalp)
      (remove item list :test #'equalp)))

;;; moved from union.lisp

(defmacro RESETQ(a b)
 `(prog1 ,a (setq ,a ,b)))

(DEFUN |intersection|  (LIST-OF-ITEMS-1 LIST-OF-ITEMS-2)
    (PROG (I H V)
      (SETQ V (SETQ H (CONS NIL NIL)))
      (COND
        ( (NOT (LISTP LIST-OF-ITEMS-1))
          (SETQ LIST-OF-ITEMS-1 (LIST LIST-OF-ITEMS-1)) ) )
      (COND
        ( (NOT (LISTP LIST-OF-ITEMS-2))
          (SETQ LIST-OF-ITEMS-2 (LIST LIST-OF-ITEMS-2)) ) )
  LP  (COND
        ( (NOT (PAIRP LIST-OF-ITEMS-1))
          (RETURN (QCDR H)) )
        ( (|member|
            (SETQ I (QCAR (RESETQ LIST-OF-ITEMS-1 (QCDR LIST-OF-ITEMS-1))))
            (QCDR H)) )
        ( (|member| I LIST-OF-ITEMS-2)
          (QRPLACD V (SETQ V (CONS I NIL))) ) )
      (GO LP) ) )

(DEFUN |union| (LIST-OF-ITEMS-1 LIST-OF-ITEMS-2)
    (PROG (I H V)
      (SETQ H (SETQ V (CONS NIL NIL)))
      (COND
        ( (NOT (LISTP LIST-OF-ITEMS-1))
          (SETQ LIST-OF-ITEMS-1 (LIST LIST-OF-ITEMS-1)) ) )
      (COND
        ( (NOT (LISTP LIST-OF-ITEMS-2))
          (SETQ LIST-OF-ITEMS-2 (LIST LIST-OF-ITEMS-2)) ) )
  LP1 (COND
        ( (NOT (PAIRP LIST-OF-ITEMS-1))
          (COND
            ( (PAIRP LIST-OF-ITEMS-2)
              (SETF LIST-OF-ITEMS-1 LIST-OF-ITEMS-2)
              (SETF LIST-OF-ITEMS-2 NIL) )
            ( 'T
              (RETURN (QCDR H)) ) ) )
        ( (NOT
            (|member|
              (SETQ I (QCAR (RESETQ LIST-OF-ITEMS-1 (QCDR LIST-OF-ITEMS-1))))
              (QCDR H)))
          (QRPLACD V (SETQ V (CONS I NIL))) ) )
      (GO LP1) ) )

(DEFUN SETDIFFERENCE (LIST-OF-ITEMS-1 LIST-OF-ITEMS-2)
    (PROG (I H V)
      (SETQ H (SETQ V (CONS NIL NIL)))
      (COND
        ( (NOT (LISTP LIST-OF-ITEMS-1))
          (SETQ LIST-OF-ITEMS-1 (LIST LIST-OF-ITEMS-1)) ) )
      (COND
        ( (NOT (LISTP LIST-OF-ITEMS-2))
          (SETQ LIST-OF-ITEMS-2 (LIST LIST-OF-ITEMS-2)) ) )
  LP1 (COND
        ( (NOT (PAIRP LIST-OF-ITEMS-1))
          (RETURN (QCDR H)) )
        ( (|member|
            (SETQ I (QCAR (RESETQ LIST-OF-ITEMS-1 (QCDR LIST-OF-ITEMS-1))))
            (QCDR H)) )
        ( (NOT (|member| I LIST-OF-ITEMS-2))
          (QRPLACD V (SETQ V (CONS I NIL))) ) )
      (GO LP1) ) )

;;; end of moved fragment

; 14.2 Accessing

(defun |last| (x) (car (LASTNODE x)))

; 14.3 Searching

(DEFUN |assoc| (X Y)
  "Return the pair associated with key X in association list Y."
  ; ignores non-nil list terminators
  ; ignores non-pair a-list entries
  (cond ((symbolp X)
         (PROG ()
               A  (COND ((ATOM Y) (RETURN NIL))
                        ((NOT (consp (CAR Y))) )
                        ((EQ (CAAR Y) X) (RETURN (CAR Y))) )
               (SETQ Y (CDR Y))
               (GO A)))
        ((or (numberp x) (characterp x))
         (PROG ()
               A  (COND ((ATOM Y) (RETURN NIL))
                        ((NOT (consp (CAR Y))) )
                        ((EQL (CAAR Y) X) (RETURN (CAR Y))) )
               (SETQ Y (CDR Y))
               (GO A)))
        (t
         (PROG ()
               A  (COND ((ATOM Y) (RETURN NIL))
                        ((NOT (consp (CAR Y))) )
                        ((EQUAL (CAAR Y) X) (RETURN (CAR Y))) )
               (SETQ Y (CDR Y))
               (GO A)))))
; 14.5 Updating

(defun NREMOVE (list item &optional (count 1))
  (if (integerp count)
      (delete item list :count count :test #'equal)
      (delete item list :test #'equal)))

(defun EFFACE (item list) (delete item list :count 1 :test #'equal))

(defun NCONC2 (x y) (NCONC x y)) ;NCONC with exactly two arguments

; 14.6 Miscellaneous

(defun QSORT (l)
 (declare (special sortgreaterp))
  (NREVERSE (sort (copy-seq l) SORTGREATERP)))

(defun SORTBY (keyfn l)
 (declare (special sortgreaterp))
  (nreverse (sort (copy-seq l) SORTGREATERP :key keyfn)))

; 16.0 Operations on Vectors

; 16.1 Creation

(defun MAKE-VEC (n) (make-array n :initial-element nil))

(defun GETREFV (n) (make-array n :initial-element nil))

#-:GCL
(defun LIST2VEC (list) (coerce list 'vector))

;;; At least in gcl 2.6.8 coerce is slow, so we roll our own version
#+:GCL
(defun LIST2VEC (list)
    (if (consp list)
        (let* ((len (length list))
               (vec (make-array len)))
             (dotimes (i len)
                  (setf (aref vec i) (pop list)))
             vec)
        (coerce list 'vector)))


(define-function 'LIST2REFVEC #'LIST2VEC)

; 16.2 Accessing


(defun size (l)
  (cond ((vectorp l) (length l))
        ((consp l)   (list-length l))
        (t           0)))

(define-function 'MOVEVEC #'replace)

; 17.0 Operations on Character and Bit Vectors

(defun charp (a) (or (characterp a)
                     (and (identp a) (= (length (symbol-name a)) 1))))

(defun NUM2CHAR (n) (code-char n))

(defun CHAR2NUM (c) (char-code (character c)))

#+(or :UNICODE :SB-UNICODE :OPENMCL-UNICODE-STRINGS)
(defun NUM2USTR (n) 
    (make-string 1 :initial-element (NUM2CHAR n)))
#-(or :UNICODE :SB-UNICODE :OPENMCL-UNICODE-STRINGS)
(defun NUM2USTR (n)
   (let (k n1 n2 n3 n4 (l nil))
       (cond
           ((< n 128)
               (setf k 1)
               (setf l (list n)))
           ((< n (ash 1 11))
               (setf k 2)
               (setf n1 (logior 128 (logand 63 n)))
               (setf n2 (logior 192 (logand 31 (ash n -6))))
               (setf l (list n2 n1)))
           ((< n (ash 1 16))
               (setf k 3)
               (setf n1 (logior 128 (logand 63 n)))
               (setf n2 (logior 128 (logand 63 (ash n -6))))
               (setf n3 (logior 224 (logand 15 (ash n -12))))
               (setf l (list n3 n2 n1)))
           ((< n (ash 1 21))
               (setf k 4)
               (setf n1 (logior 128 (logand 63 n)))
               (setf n2 (logior 128 (logand 63 (ash n -6))))
               (setf n3 (logior 128 (logand 63 (ash n -12))))
               (setf n4 (logior 240 (logand 7 (ash n -18))))
               (setf l (list n4 n3 n2 n1)))
           (t
               (|error| "Too large character code"))
       )
       (make-array k :element-type 'character
                  :initial-contents (mapcar #'code-char l))))

(defun CGREATERP (s1 s2) (string> (string s1) (string s2)))

; 17.1 Creation


#-AKCL
(defun concat (a b &rest l)
   (let ((type (cond ((bit-vector-p a) 'bit-vector) (t 'string))))
      (cond ((eq type 'string)
             (setq a (string a) b (string b))
             (if l (setq l (mapcar #'string l)))))
      (if l (apply #'concatenate type a b l)
        (concatenate type a b))) )
#+AKCL
(defun concat (a b &rest l)
  (if (bit-vector-p a)
      (if l (apply #'concatenate 'bit-vector a b l)
        (concatenate 'bit-vector a b))
    (if l (apply #'system:string-concatenate a b l)
      (system:string-concatenate a b))))

(define-function 'strconc #'concat)

(defun make-cvec (sint) (make-array sint :fill-pointer 0 :element-type 'character))

(define-function 'getstr #'make-cvec)

(defun make-full-cvec (sint &optional (char #\space))
  (make-string sint :initial-element (if (integerp char)
                                       (code-char char)
                                       (character char))))

(define-function 'getfullstr #'make-full-cvec)

; 17.2 Accessing

(defun QENUM (cvec ind) (char-code (char cvec ind)))

(defun QESET (cvec ind charnum)
  (setf (char cvec ind) (code-char charnum)))

(defun string2id-n (cvec sint)
  (if (< sint 1)
      nil
      (let ((start (position-if-not #'(lambda (x) (char= x #\Space)) cvec)))
        (if start
            (let ((end (or (position #\Space cvec :start start) (length cvec))))
              (if (= sint 1)
                  (intern (subseq cvec start end))
                  (string2id-n (subseq cvec end) (1- sint))))
            0))))

(defun substring (cvec start length)
  (setq cvec (string cvec))
  (if length (subseq cvec start (+ start length)) (subseq cvec start)))

; 17.3 Searching

;;- (defun strpos (what in start dontcare)
;;-    (setq what (string what) in (string in))
;;-    (if dontcare (progn (setq dontcare (character dontcare))
;;-                    (search what in :start2 start
;;-                            :test #'(lambda (x y) (or (eql x dontcare)
;;-                                                      (eql x y)))))
;;-                 (search what in :start2 start)))

(defun strpos (what in start dontcare)
   (setq what (string what) in (string in))
   (if dontcare (progn (setq dontcare (character dontcare))
                       (search what in :start2 start
                               :test #'(lambda (x y) (or (eql x dontcare)
                                                         (eql x y)))))
                (if (= start 0)
                   (search what in)
                   (search what in :start2 start))
   ))

; In the following, table should be a string:

(defun strposl (table cvec sint item)
  (setq cvec (string cvec))
  (if (not item)
      (position table cvec :test #'(lambda (x y) (position y x)) :start sint)
      (position table cvec :test-not #'(lambda (x y) (position y x)) :start sint)))

; 17.4 Updating operators

(defun suffix (id cvec)
  "Suffixes the first char of the symbol or char ID to the string CVEC,
    changing CVEC."
  (unless (characterp id) (setq id (elt (string id) 0)))
  (cond ((array-has-fill-pointer-p cvec)
         (vector-push-extend id cvec)
         cvec)
        ((adjustable-array-p cvec)
         (let ((l (length cvec)))
           (adjust-array cvec (1+ l))
           (setf (elt cvec l) id)
           cvec))
        (t (concat cvec id))))

;;-- (defun rplacstr (cvec1 start1 length1 cvec2
;;--                        &optional (start2 0) (length2 nil)
;;--                        &aux end1 end2)
;;--   (setq cvec2 (string cvec2))
;;--   (if (null start1) (setq start1 0))
;;--   (if (null start2) (setq start2 0))
;;--   (if (null length1) (setq length1 (- (length cvec1) start1)))
;;--   (if (null length2) (setq length2 (- (length cvec2) start2)))
;;--   (if (numberp length1) (setq end1 (+ start1 length1)))
;;--   (if (numberp length2) (setq end2 (+ start2 length2)))
;;--   (if (/= length1 length2)
;;--       (concatenate 'string (subseq cvec1 0 start1)
;;--                            (subseq cvec2 start2 end2)
;;--                            (subseq cvec1 end1))
;;--       (replace cvec1 cvec2 :start1 start1 :end1 end1
;;--              :start2 start2 :end2 end2)))

; The following version has been provided to avoid reliance on the
; Common Lisp concatenate and replace functions. These built-in Lisp
; functions would probably end up doing the character-by-character
; copying shown here, but would also need to cope with generic sorts
; of sequences and unwarranted keyword generality

(defun rplacstr (cvec1 start1 length1 cvec2
                       &optional start2 length2
                       &aux end1 end2)
  (setq cvec2 (string cvec2))
  (if (null start1) (setq start1 0))
  (if (null start2) (setq start2 0))
  (if (null length1) (setq length1 (- (length cvec1) start1)))
  (if (null length2) (setq length2 (- (length cvec2) start2)))
  (setq end1 (+ start1 length1))
  (setq end2 (+ start2 length2))
  (if (= length1 length2)
      (do ()
          ((= start1 end1) cvec1)
          (setf (aref cvec1 start1) (aref cvec2 start2))
          (setq start1 (1+ start1))
          (setq start2 (1+ start2)))
      (let* ((l1 (length cvec1))
             (r (make-string (- (+ l1 length2) length1)))
             (i 0))
         (do ((j 0 (1+ j)))
             ((= j start1))
             (setf (aref r i) (aref cvec1 j))
             (setq i (1+ i)))
         (do ((j start2 (1+ j)))
             ((= j end2))
             (setf (aref r i) (aref cvec2 j))
             (setq i (1+ i)))
         (do ((j end1 (1+ j)))
             ((= j l1))
             (setf (aref r i) (aref cvec1 j))
             (setq i (1+ i)))
         r)
  ))

; 19.0 Operations on Arbitrary Objects

; 19.1 Creating

(defun |substitute| (new old tree) (subst new old tree :test #'equal))

(define-function 'MSUBSTQ #'subst) ;default test is eql

(defun copy (x) (copy-tree x)) ; not right since should descend vectors

(defun eqsubstlist (new old list) (sublis (mapcar #'cons old new) list))


; 24.0 Printing

;(define-function 'prin2cvec #'write-to-string)
(define-function 'prin2cvec #'princ-to-string)
;(define-function 'stringimage #'write-to-string)
(define-function 'stringimage #'princ-to-string)

(define-function 'printexp #'princ)
(define-function 'prin0  #'prin1)

(defun |F,PRINT-ONE| (form &optional (stream *standard-output*))
 (declare (ignore stream))
    (let ((*print-level* 4) (*print-length* 4))
       (prin1 form) (terpri)))

(defun prettyprint (x &optional (stream *standard-output*))
  (prettyprin0 x stream) (terpri stream))

(defun prettyprin0 (x &optional (stream *standard-output*))
  (let ((*print-pretty* t) (*print-array* t))
    (prin1 x stream)))

(defun tab (sint &optional (stream t))
  (format stream "~vT" sint))

; 27.0 Stream I/O


; 27.1 Creation

(defun MAKE-INSTREAM (filespec)
   (cond ((numberp filespec) (make-synonym-stream '*standard-input*))
         ((null filespec) (error "not handled yet"))
         (t (open (make-input-filename filespec)
                  :direction :input :if-does-not-exist nil))))

(defun MAKE-OUTSTREAM (filespec)
   (cond ((numberp filespec) (make-synonym-stream '*standard-output*))
         ((null filespec) (error "not handled yet"))
         (t (open (make-filename filespec) :direction :output
               :if-exists :supersede))))

(defun MAKE-APPENDSTREAM (filespec)
 "fortran support"
 (cond
  ((numberp filespec) (make-synonym-stream '*standard-output*))
  ((null filespec) (error "make-appendstream: not handled yet"))
  ('else (open (make-filename filespec) :direction :output
          :if-exists :append :if-does-not-exist :create))))

(defun |mkOutputConsoleStream| ()
     (make-synonym-stream '*standard-output*))

(defun shut (st) (if #+:GCL(is-console st)
                     #-:GCL(typep st 'synonym-stream)
                     st
                   (if (streamp st) (close st) -1)))

(defun EOFP (stream) (null (peek-char nil stream nil nil)))

; 28.0 Key addressed I/O


; 46.0 Call tracing

(defvar *embedded-functions* nil)

(defun EMBEDDED () (mapcar #'car *embedded-functions*))

(defun EMBED (CURRENT-BINDING NEW-DEFINITION)
  (PROG
      (OP BV BODY OLD-DEF)
      (COND
        ( (NOT (IDENTP CURRENT-BINDING))
            (error (format nil "invalid argument ~s to EMBED"
                      CURRENT-BINDING))) )
      (SETQ OLD-DEF (symbol-function CURRENT-BINDING))
      (SETQ NEW-DEFINITION
          (COND
            ( (NOT (consp NEW-DEFINITION))
              NEW-DEFINITION )
            ( (AND
		(setf BODY (ifcdr (ifcdr NEW-DEFINITION)))
		(or (progn (setf OP (car NEW-DEFINITION))
		           (setf BV (car (cdr NEW-DEFINITION))))
		    T)
                (OR (EQ OP 'LAMBDA) (EQ OP 'MLAMBDA)))
              (COND
                ( (NOT (MEMQ CURRENT-BINDING (FLAT-BV-LIST BV)))
                 `(,OP ,BV ((LAMBDA (,CURRENT-BINDING) . ,BODY) ',OLD-DEF))
                   )
                ( 'T
                  NEW-DEFINITION ) ) )
            ( 'T
              `((LAMBDA (,CURRENT-BINDING) ,NEW-DEFINITION) ',OLD-DEF)))
            )
      (SETF NEW-DEFINITION (COERCE NEW-DEFINITION 'FUNCTION))
      (SETF (symbol-function CURRENT-BINDING) NEW-DEFINITION)
      (push (LIST CURRENT-BINDING NEW-DEFINITION OLD-DEF) *embedded-functions*)
      (RETURN CURRENT-BINDING) ) )

(defun UNEMBED (CURRENT-BINDING)
    (let
      (TMP E-LIST E-HEAD CUR-DEF)
      (SETQ E-LIST *embedded-functions*)
      (SETQ CUR-DEF (symbol-function CURRENT-BINDING))
      (tagbody
       LP (if (NOT (consp E-LIST))
              (return-from UNEMBED NIL))
          (setf E-HEAD (car E-LIST))
          (if (or (NOT (consp E-HEAD))
                  (NOT (consp (cdr E-HEAD)))
                  (NOT (EQ CURRENT-BINDING (car E-HEAD)))
                  (NOT (EQ CUR-DEF (nth 1 E-HEAD))))
              (progn
                  (setf TMP E-LIST)
                  (setf E-LIST (cdr E-LIST))
                  (GO LP) ))
          (setf (symbol-function CURRENT-BINDING) (nth 2 E-HEAD))
          (if TMP
              (setf (cdr TMP) (QCDR E-LIST))
              (setf *embedded-functions* (QCDR E-LIST)))
              (return-from UNEMBED CURRENT-BINDING))))

(defun FLAT-BV-LIST (BV-LIST)
  (PROG (TMP1)
      (RETURN
        (COND
          ( (VARP BV-LIST)
            (LIST BV-LIST) )
          ( (REFVECP BV-LIST)
	    (BREAK))
          ( (NOT (consp BV-LIST))
            NIL )
          ( (EQ '= (SETQ TMP1 (QCAR BV-LIST)))
            (FLAT-BV-LIST (QCDR BV-LIST)) )
          ( (VARP TMP1)
            (CONS TMP1 (FLAT-BV-LIST (QCDR BV-LIST))) )
          ( (AND (NOT (consp TMP1)) (NOT (REFVECP TMP1)))
            (FLAT-BV-LIST (QCDR BV-LIST)) )
          ( 'T
            (NCONC (FLAT-BV-LIST TMP1) (FLAT-BV-LIST (QCDR BV-LIST))) ) )) ))

(defun VARP (TEST-ITEM)
    (COND
      ( (IDENTP TEST-ITEM)
        TEST-ITEM )
      ( (AND
          (consp TEST-ITEM)
          (OR (EQ (QCAR TEST-ITEM) 'FLUID) (EQ (QCAR TEST-ITEM) 'LEX))
          (consp (QCDR TEST-ITEM))
          (IDENTP (QCADR TEST-ITEM)))
        TEST-ITEM )
      ( 'T
        NIL ) ) )

; 48.0 Miscellaneous CMS Interactions

(defun CurrentTime ()
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format nil "~2,'0D/~2,'0D/~2,'0D~2,'0D:~2,'0D:~2,'0D"
            month day (rem year 100) hour min sec)))

; 97.0 Stuff In The Manual But Wierdly Documented

(defun EBCDIC (x) (int-char x))

; 98.0 Stuff Not In The VMLisp Manual That We Like

; A version of GET that works with lists

; (defun getl (sym key )
;   (cond ((consp sym) (cdr (assoc key sym :test #'eq)))
;         ((symbolp sym) (get sym key))))
;
;; assq fails if list sym contains symbols
;; and not only conses

(defun getl (sym key )
  (cond ((consp sym)
         (do ((sym sym (cdr sym)))
             ((null sym) nil)
             (cond ((and (consp (car sym))
                          (eq (caar sym) key))
                     (return (cdar sym))))))
        ((symbolp sym) (get sym key))))

; 99.0 Ancient Stuff We Decided To Keep

(defvar *read-place-holder* (make-symbol "%.EOF")
   "default value returned by read and read-line at end-of-file")

(defun PLACEP (item) (eq item *read-place-holder*))
(defun VMREAD (st &optional (eofval *read-place-holder*))
  (read st nil eofval))
(defun |read-line| (st &optional (eofval *read-place-holder*))
  (read-line st nil eofval))

#+(OR IBCL KCL)
(defun gcmsg (x)
   (prog1 system:*gbc-message* (setq system:*gbc-message* x)))
#+:cmu
(defun gcmsg (x)
   (prog1 ext:*gc-verbose* (setq ext:*gc-verbose* x)))
#+:allegro
(defun gcmsg (x))
#+:sbcl
(defun gcmsg (x))
#+:openmcl
(defun gcmsg (x))
#+:clisp
(defun gcmsg (x))
#+:ecl
(defun gcmsg (x))
#+:poplog
(defun gcmsg (x))
#+:lispworks
(defun gcmsg (x))

#+abcl
(defun reclaim () (ext::gc))
#+:allegro
(defun reclaim () (excl::gc t))
#+clisp
(defun reclaim () (#+lisp=cl ext::gc #-lisp=cl lisp::gc))
#+:cmu
(defun reclaim () (ext:gc))
#+cormanlisp
(defun reclaim () (cl::gc))
#+:GCL
(defun reclaim () (si::gbc t))
#+lispworks
(defun reclaim () )
#+sbcl
(defun reclaim () (sb-ext::gc))
#+openmcl
(defun reclaim () (ccl::gc))
#+:ecl
(defun reclaim () (si::gc t))
#+:poplog
(defun reclaim () nil)


#+(OR IBCL KCL)
(defun BPINAME (func)
  (if (functionp func)
      (cond ((symbolp func) func)
            ((and (consp func) (eq (car func) 'LAMBDA-BLOCK))
              (cadr func))
            ((compiled-function-p func)
             (system:compiled-function-name func))
            ('t func))))
#+:cmu
(defun BPINAME (func)
 (when (functionp func)
  (cond
    ((symbolp func) func)
    ((and (consp func) (eq (car func) 'lambda)) (second (third func)))
    ((compiled-function-p func)
     (kernel::%function-name func))
    ('t func))))

#+(or :sbcl :clisp :openmcl :ecl :lispworks :poplog)
(defun BPINAME (func)
  (cond
      ((functionp func)
         (let (d1 d2 res)
             (setf (values d1 d2 res) (function-lambda-expression func))
             (if (and res (symbolp res) (fboundp res))
                 res
                 func)))
      ((symbolp func) func)))

#+:cmu
(defun OBEY (S)
  (ext::process-exit-code
   (ext::run-program "sh" (list "-c" S) :input t :output t)))

#+:GCL
(defun OBEY (S) (LISP::SYSTEM S))

#+:allegro
(defun OBEY (S) (excl::run-shell-command s))

#+:sbcl
(defun OBEY (S)
   #-:win32 (sb-ext::process-exit-code
             (sb-ext::run-program "/bin/sh"
                    (list "-c" S) :input t :output t))
   #+:win32 (sb-ext::process-exit-code
             (sb-ext::run-program (make-absolute-filename "/lib/obey.bat")
                    (list S) :input t :output t))
)

#+:openmcl
(defun OBEY (S)
  (ccl::run-program "sh" (list "-c" S)))

#+(and :clisp (or :win32 :unix))
(defun OBEY (S)
   (ext:run-shell-command S))

#+:ecl
(defun OBEY (S)
   (ext:system S))

#+:poplog
(defun OBEY (S)
   (POP11:sysobey S))


#+:lispworks
(defun OBEY (S)
   (system:call-system S))

(defun MAKE-BVEC (n)
 (make-array (list n) :element-type 'bit :initial-element 0))

;;; moved from hash.lisp

;17.0 Operations on Hashtables

;17.1 Creation

(defun MAKE-HASHTABLE (id1)
   (let ((test (case id1
                     ((EQ ID) #'eq)
                     (CVEC #'equal)
                     (EQL #'eql)
                     #+Lucid ((UEQUAL EQUALP) #'EQUALP)
                     #-Lucid ((UEQUAL EQUAL) #'equal)
                     (otherwise (error "bad arg to make-hashtable")))))
      (make-hash-table :test test)))

;17.2 Accessing

(defmacro HGET (table key &rest default)
   `(gethash ,key ,table ,@default))

(defun HKEYS (table)
   (let (keys)
      (maphash
        #'(lambda (key val) (declare (ignore val)) (push key keys)) table)
        keys))

(define-function 'HASHTABLE-CLASS #'hash-table-test)

(define-function 'HCOUNT #'hash-table-count)

;17.4 Searching and Updating

(defun HPUT (table key value) (setf (gethash key table) value))

(defmacro HREM (table key) `(remhash ,key ,table))

(defun HREMPROP (table key property)
  (let ((plist (gethash key table)))
    (if plist (setf (gethash key table)
                    (delete property plist :test #'equal :key #'car)))))

;17.6 Miscellaneous

(define-function 'HASHTABLEP #'hash-table-p)

(define-function 'HASHEQ #'sxhash)

;;; end of moved fragment

;;; ---------------------------------------------

;; Contributed by Juergen Weiss from a suggestion by Arthur Norman.
;; This is a Mantissa and Exponent function.
(defun manexp (u)
  (multiple-value-bind (f e s)
    (decode-float u)
    (cons (* s f) e)))

;;; Contributed by Juergen Weiss from Arthur Norman's CCL.
(defun acot (a)
  (if (> a 0.0)
    (if (> a 1.0)
       (atan (/ 1.0 a))
       (- (/ pi 2.0) (atan a)))
    (if (< a -1.0)
       (- pi (atan (/ -1.0 a)))
       (+ (/ pi 2.0) (atan (- a))))))

;;; Contributed by Juergen Weiss from Arthur Norman's CCL.
(defun cot (a)
  (if (or (> a 1000.0) (< a -1000.0))
    (/ (cos a) (sin a))
    (/ 1.0 (tan a))))

;;; These functions should be defined for DoubleFloat inputs but are not.
;;; These are cheap and easy definitions that work but should be rewritten.
(defun sec (x) (/ 1 (cos x)))
(defun csc (x) (/ 1 (sin x)))
(defun acsc (x) (asin (/ 1 x)))
(defun asec (x) (acos (/ 1 x)))
(defun csch (x) (/ 1 (sinh x)))
(defun coth (x) (* (cosh x) (csch x)))
(defun sech (x) (/ 1 (cosh x)))
(defun acsch (x) (asinh (/ 1 x)))
(defun acoth (x) (atanh (/ 1 x)))
(defun asech (x) (acosh (/ 1 x)))

;;; moved from unlisp.lisp.pamphlet
(defun |AlistAssocQ| (key l)
  (assoc key l :test #'eq) )

(defun |AlistRemoveQ| (key l)
   (let ((pr (assoc key l :test #'eq)))
       (if pr
           (remove pr l :test #'eq)
           l) ))

(defun log10 (u) (log u 10.0d0))

(defun |make_spaces| (len)
    (make-string len :initial-element #\ ))

;;; end of moved fragment

;;; moved from bits.lisp

;;; The types "bit" and "bit vector" are implemented differently
;;; in different variants of lisp.
;;; These lisp macros/functions will have different implementations
;;; on different lisp systems.

;;; The operations which traverse entire vectors are given as functions
;;; since the function calling overhead will be relatively small.
;;; The operations which extract or set a single part of the vector are
;;; provided as macros.

;;; SMW Nov 88: Created

(defmacro truth-to-bit (x) `(cond (,x 1) ('else 0)))
(defmacro bit-to-truth (b) `(eq ,b 1))

(defun    bvec-make-full (n x)
    (make-array (list n) :element-type 'bit :initial-element x))

(defmacro bvec-elt       (bv i)    `(sbit ,bv ,i))
(defmacro bvec-setelt    (bv i x)  `(setf (sbit ,bv ,i) ,x))
(defmacro bvec-size      (bv)      `(size ,bv))

(defun    bvec-copy      (bv)      (copy-seq bv))
(defun    bvec-concat    (bv1 bv2) (concatenate '(vector bit) bv1 bv2))
(defun    bvec-equal     (bv1 bv2) (equal    bv1 bv2))
(defun    bvec-greater   (bv1 bv2)
  (let ((pos (mismatch bv1 bv2)))
    (cond ((or (null pos) (>= pos (length bv1))) nil)
          ((< pos (length bv2)) (> (bit bv1 pos) (bit bv2 pos)))
          ((find 1 bv1 :start pos) t)
          (t nil))))
(defun    bvec-and       (bv1 bv2) (bit-and  bv1 bv2))
(defun    bvec-or        (bv1 bv2) (bit-ior  bv1 bv2))
(defun    bvec-xor       (bv1 bv2) (bit-xor  bv1 bv2))
(defun    bvec-nand      (bv1 bv2) (bit-nand bv1 bv2))
(defun    bvec-nor       (bv1 bv2) (bit-nor  bv1 bv2))
(defun    bvec-not       (bv)      (bit-not  bv))

;;; end of moved fragment
