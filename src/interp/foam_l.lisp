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

;;;
;;; FOAM Operations for Common Lisp
;;;

;;
;; Client files should begin with
;;   (in-package "FOAM-USER" :use '("FOAM" "LISP"))
;;
;;
;; To Do:
;;    Test cases.
;;    Scan and format functions need to be rewritten to handle complete syntax.
;;    Deftypes for each Foam type?
;;

(in-package "FOAM")

(export '(
 compile-as-file cases

 |Clos| |Char| |Bool| |Byte| |HInt| |SInt| |BInt| |SFlo| |DFlo| |Ptr| 
 |Word| |Arb| |Env| |Level| |Arr| |Record| |Nil|

 |ClosInit| |CharInit| |BoolInit| |ByteInit| |HIntInit| |SIntInit| 
 |BIntInit| |SFloInit| |DFloInit| |PtrInit| |WordInit| |ArbInit| |EnvInit|
 |ArrInit| |RecordInit| |LevelInit| 

 |BoolFalse| |BoolTrue| |BoolNot| |BoolAnd| |BoolOr| |BoolEQ| |BoolNE|

 |CharSpace| |CharNewline| |CharMin| |CharMax| |CharIsDigit|
 |CharIsLetter| |CharEQ| |CharNE| |CharLT| |CharLE|
 |CharLower| |CharUpper| |CharOrd| |CharNum| |CharCode0|

 |SFlo0| |SFlo1| |SFloMin| |SFloMax| |SFloEpsilon| |SFloIsZero|
 |SFloIsNeg| |SFloIsPos| |SFloEQ| |SFloNE| |SFloLT|
 |SFloLE| |SFloNegate| |SFloPrev| |SFloNext| |SFloPlus|
 |SFloMinus| |SFloTimes| |SFloTimesPlus| |SFloDivide|
 |SFloRPlus| |SFloRMinus| |SFloRTimes| |SFloRTimesPlus|
 |SFloRDivide| |SFloDissemble| |SFloAssemble|

 |DFlo0| |DFlo1| |DFloMin| |DFloMax| |DFloEpsilon|
 |DFloIsZero| |DFloIsNeg| |DFloIsPos| |DFloEQ| |DFloNE|
 |DFloLT| |DFloLE| |DFloNegate| |DFloPrev| |DFloNext|
 |DFloPlus| |DFloMinus| |DFloTimes| |DFloTimesPlus|
 |DFloDivide| |DFloRPlus| |DFloRMinus| |DFloRTimes|
 |DFloRTimesPlus| |DFloRDivide| |DFloDissemble|
 |DFloAssemble| |Byte0| |Byte1| |ByteMin| |ByteMax|

 |HInt0| |HInt1| |HIntMin| |HIntMax|

 |SInt0| |SInt1| |SIntMin| |SIntMax| |SIntIsZero| |SIntIsNeg|
 |SIntIsPos| |SIntIsEven| |SIntIsOdd| |SIntEQ| |SIntNE|
 |SIntLT| |SIntLE| |SIntNegate| |SIntPrev| |SIntNext|
 |SIntPlus| |SIntMinus| |SIntTimes| |SIntTimesPlus|
 |SIntMod| |SIntQuo| |SIntRem| |SIntDivide| |SIntGcd|
 |SIntPlusMod| |SIntMinusMod| |SIntTimesMod|
 |SIntTimesModInv| |SIntLength| |SIntShiftUp|
 |SIntShiftDn| |SIntBit| |SIntNot| |SIntAnd| |SIntOr|

 |WordTimesDouble| |WordDivideDouble| |WordPlusStep| |WordTimesStep|

 |BInt0| |BInt1| |BIntIsZero| |BIntIsNeg| |BIntIsPos| |BIntIsEven|
 |BIntIsOdd| |BIntIsSingle| |BIntEQ| |BIntNE| |BIntLT|
 |BIntLE| |BIntNegate| |BIntPrev| |BIntNext| |BIntPlus|
 |BIntMinus| |BIntTimes| |BIntTimesPlus| |BIntMod|
 |BIntQuo| |BIntRem| |BIntDivide| |BIntGcd|
 |BIntSIPower| |BIntBIPower| |BIntLength| |BIntShiftUp|
 |BIntShiftDn| |BIntBit|

 |PtrNil| |PtrIsNil| |PtrMagicEQ| |PtrEQ| |PtrNE|

 |FormatSFlo| |FormatDFlo| |FormatSInt| |FormatBInt|
 |fgetss| |fputss|

 |ScanSFlo| |ScanDFlo| |ScanSInt| |ScanBInt|

 |SFloToDFlo| |DFloToSFlo| |ByteToSInt| |SIntToByte| |HIntToSInt|
 |SIntToHInt| |SIntToBInt| |BIntToSInt| |SIntToSFlo|
 |SIntToDFlo| |BIntToSFlo| |BIntToDFlo| |PtrToSInt|
 |SIntToPtr| |BoolToSInt|

 |ArrToSFlo| |ArrToDFlo| |ArrToSInt| |ArrToBInt|

 |PlatformRTE| |PlatformOS| |Halt|

 |Clos| |CCall| |ClosEnv| |ClosFun| |SetClosEnv| |SetClosFun|
 |DDecl| |RNew| |ANew| |RElt| |EElt| |AElt| |Lex|
 |SetLex| |SetRElt| |SetAElt| |SetEElt|
 |FoamFree|

 declare-prog declare-type
 defprog ignore-var block-return
 defspecials file-exports file-imports
 typed-let foamfn |FoamProg| |alloc-prog-info|

 |MakeEnv| |EnvLevel| |EnvNext| |EnvInfo| |SetEnvInfo| |FoamEnvEnsure|
 |MakeLit| |MakeLevel|
 |printNewLine| |printChar| |printString| |printSInt| |printBInt| |printSFloat|
 |printDFloat| 
 |strLength| |formatSInt| |formatBInt| |formatSFloat| |formatDFloat|
 
 |ProgHashCode| |SetProgHashCode| |ProgFun|
 |G-mainArgc| |G-mainArgv|
 |stdinFile| |stdoutFile| |stderrFile|
 |fputc| |fputs| |foamfun|


 ;; trancendental functions
 |sqrt| |pow| |log| |exp| |sin| |cos| |tan| |sinh| |cosh| |tanh| 
 |asin| |acos| |atan| |atan2|

 ;; debuging
 |fiSetDebugVar| |fiGetDebugVar| |fiSetDebugger| |fiGetDebugger|
 ;; Blatent hacks..
 |G-stdoutVar| |G-stdinVar| |G-stderrVar|
 |fiStrHash|

 axiomxl-file-init-name
 axiomxl-global-name
))


;; type defs for Foam types
(deftype |Char| () 'character)
(deftype |Clos| () 'list)
(deftype |Bool| () '(member t nil))
(deftype |Byte| () 'unsigned-byte)
(deftype |HInt| () '(integer #.(- (expt 2 15)) #.(1- (expt 2 15))))
(deftype |SInt| () '(integer #.(- (expt 2 31)) #.(1- (expt 2 31))))

#+:AKCL
(deftype |BInt| () t)
#-:AKCL
(deftype |BInt| () 'integer)

(deftype |SFlo| () 'short-float)

#+:AKCL
(deftype |DFlo| () t)
#-:AKCL
(deftype |DFlo| () 'double-float)

(deftype |Level| () t) ;; structure??

(deftype |Nil|  () t)
(deftype |Ptr|  () t)
(deftype |Word| () t)
(deftype |Arr|  () t)
(deftype |Record| () t)
(deftype |Arb|  () t)
(deftype |Env|  () t)  ; (or cons nil)

;; default values for types.  Used as initializers in lets.
(defconstant |CharInit| (the |Char| '#\Space))
(defconstant |ClosInit| (the |Clos| nil))
(defconstant |BoolInit| (the |Bool| nil))
(defconstant |ByteInit| (the |Byte| 0))
(defconstant |HIntInit| (the |HInt| 0))
(defconstant |SIntInit| (the |SInt| 0))
(defconstant |BIntInit| (the |BInt|  0))
(defconstant |SFloInit| (the |SFlo| 0.0s0))
(defconstant |DFloInit| (the |DFlo| 0.0d0))
(defconstant |PtrInit|  (the |Ptr|  nil))
(defconstant |ArrInit|  (the |Arr|  nil))
(defconstant |RecordInit|  (the |Record|  nil))
(defconstant |WordInit| (the |Word| nil))
(defconstant |ArbInit|  (the |Arb|  nil))
(defconstant |EnvInit|  (the |Env|  nil))
(defconstant |LevelInit|  (the |Level|  nil))

;; Bool values are assumed to be either 'T or NIL.
;; Thus non-nil values are canonically represented.
(defmacro |BoolFalse|     () NIL)
(defmacro |BoolTrue|      () 'T)
(defmacro |BoolNot|      (x) `(NOT ,x))
(defmacro |BoolAnd|    (x y)
  `(let ((xx ,x) (yy ,y)) (AND xx yy))) ;; force evaluation of both args
(defmacro |BoolOr|     (x y)
  `(let ((xx ,x) (yy ,y)) (OR  xx yy))) ;; force evaluation of both args
(defmacro |BoolEQ|     (x y) `(EQ ,x ,y))
(defmacro |BoolNE|     (x y) `(NOT (|BoolEQ| ,x ,y)))

(defconstant |CharCode0| (code-char 0))

(defmacro |CharSpace|    () '#\Space)
(defmacro |CharNewline|  () '#\Newline)
(defmacro |CharMin|      ()  |CharCode0|)
(defmacro |CharMax|      ()  #.(code-char (1- char-code-limit)))
(defmacro |CharIsDigit| (x) `(if (DIGIT-CHAR-P (the |Char| ,x)) 't nil))
(defmacro |CharIsLetter|(x) `(ALPHA-CHAR-P (the |Char| ,x)))
(defmacro |CharLT|    (x y) `(CHAR< (the |Char| ,x) (the |Char| ,y)))
(defmacro |CharLE|    (x y) `(CHAR<= (the |Char| ,x) (the |Char| ,y)))
(defmacro |CharEQ|    (x y) `(CHAR= (the |Char| ,x) (the |Char| ,y)))
(defmacro |CharNE|    (x y) `(CHAR/= (the |Char| ,x) (the |Char| ,y)))
(defmacro |CharLower|   (x) `(the |Char| (CHAR-DOWNCASE (the |Char| ,x))))
(defmacro |CharUpper|   (x) `(the |Char| (CHAR-UPCASE (the |Char| ,x))))
(defmacro |CharOrd|     (x) `(CHAR-INT (the |Char| ,x)))
(defmacro |CharNum|     (x) `(INT-CHAR (the |SInt| ,x)))

(defmacro |SFlo0|        () 0.0s0)
(defmacro |SFlo1|        () 1.0s0)
(defmacro |SFloMin|      () most-negative-short-float)
(defmacro |SFloMax|      () most-positive-short-float)
(defmacro |SFloEpsilon|  () short-float-epsilon)
(defmacro |SFloIsZero|  (x) `(zerop (the |SFlo| ,x)))
(defmacro |SFloIsNeg|   (x) `(minusp (the |SFlo| ,x)))
(defmacro |SFloIsPos|   (x) `(plusp (the |SFlo| ,x)))
(defmacro |SFloLT|    (x y) `(< (the |SFlo| ,x) (the |SFlo| ,y)))
(defmacro |SFloLE|    (x y) `(<= (the |SFlo| ,x) (the |SFlo| ,y)))
(defmacro |SFloEQ|    (x y) `(= (the |SFlo| ,x) (the |SFlo| ,y)))
(defmacro |SFloNE|    (x y) `(/= (the |SFlo| ,x) (the |SFlo| ,y)))
(defmacro |SFloNegate|  (x) `(the |SFlo| (- (the |SFlo| ,x))))
(defmacro |SFloNext|    (x) `(the |SFlo| (+ (the |SFlo| ,x) 1.0s0)))
(defmacro |SFloPrev|    (x) `(the |SFlo| (- (the |SFlo| ,x) 1.0s0)))
(defmacro |SFloMinus| (x y) `(the |SFlo| (- (the |SFlo| ,x) (the |SFlo| ,y))))
(defmacro |SFloTimes| (x y) `(the |SFlo| (* (the |SFlo| ,x) (the |SFlo| ,y))))
(defmacro |SFloTimesPlus| (x y z)
  `(the |SFlo| (+ (* (the |SFlo| ,x) (the |SFlo| ,y)) (the |SFlo| ,z))))
(defmacro |SFloDivide|  (x y) `(the |SFlo| (/ (the |SFlo| ,x) (the |SFlo| ,y))))
(defmacro |SFloRPlus|  (x y r) `(error "unimplemented operation -- SFloRPlus"))
(defmacro |SFloRMinus| (x y r) `(error "unimplemented operation -- SFloRTimes"))
(defmacro |SFloRTimes| (x y r) `(error "unimplemented operation -- SFloRTimes"))
(defmacro |SFloRTimesPlus| (x y z r) `(error "unimplemented operation -- SFloTimesPlus"))
(defmacro |SFloRDivide|(x y r) `(error "unimplemented operation -- SFloDivide"))
(defmacro |SFloDissemble| (x) `(error "unimplemented operation -- SFloDissemble"))
(defmacro |SFloAssemble| (w x y) `(error "unimplemented operation -- SFloAssemble"))

;; These are no longer foam builtins
;;(defmacro |SFloRound|    (x) `(the |BInt| (round (the |SFlo| ,x))))
;;(defmacro |SFloTruncate| (x) `(the |BInt| (truncate (the |SFlo| ,x))))
;;(defmacro |SFloFloor|    (x) `(the |BInt| (floor (the |SFlo| ,x))))
;;(defmacro |SFloCeiling|  (x) `(the |BInt| (ceiling (the |SFlo| ,x))))

(defmacro |DFlo0|         () 0.0d0)
(defmacro |DFlo1|         () 1.0d0)
(defmacro |DFloMin|       () most-negative-double-float)
(defmacro |DFloMax|       () most-positive-double-float)
(defmacro |DFloEpsilon|   () double-float-epsilon)
(defmacro |DFloIsZero|   (x) `(zerop (the |DFlo| ,x)))
(defmacro |DFloIsNeg|    (x) `(minusp (the |DFlo| ,x)))
(defmacro |DFloIsPos|    (x) `(plusp (the |DFlo| ,x)))
(defmacro |DFloLE|     (x y) `(<= (the |DFlo| ,x) (the |DFlo| ,y)))
(defmacro |DFloEQ|     (x y) `(= (the |DFlo| ,x) (the |DFlo| ,y)))
(defmacro |DFloLT|     (x y) `(< (the |DFlo| ,x) (the |DFlo| ,y)))
(defmacro |DFloNE|     (x y) `(/= (the |DFlo| ,x) (the |DFlo| ,y)))
(defmacro |DFloNegate|   (x) `(the |DFlo| (- (the |DFlo| ,x))))
(defmacro |DFloNext|     (x) `(the |DFlo| (+ (the |DFlo| ,x) 1.0d0)))
(defmacro |DFloPrev|     (x) `(the |DFlo| (- (the |DFlo| ,x) 1.0d0)))
(defmacro |DFloPlus|   (x y) `(the |DFlo| (+ (the |DFlo| ,x) (the |DFlo| ,y))))
(defmacro |DFloMinus|  (x y) `(the |DFlo| (- (the |DFlo| ,x) (the |DFlo| ,y))))
(defmacro |DFloTimes|  (x y) `(the |DFlo| (* (the |DFlo| ,x) (the |DFlo| ,y))))
(defmacro |DFloDivide| (x y) `(the |DFlo| (/ (the |DFlo| ,x) (the |DFlo| ,y))))
(defmacro |DFloTimesPlus| (x y z)
  `(the |DFlo| (+ (* (the |DFlo| ,x) (the |DFlo| ,y)) (the |DFlo| ,z))))

(defmacro |DFloRPlus|  (x y r) `(error "unimplemented operation -- DFloRPlus"))
(defmacro |DFloRMinus| (x y r) `(error "unimplemented operation -- DFloRTimes"))
(defmacro |DFloRTimes| (x y r) `(error "unimplemented operation -- DFloRTimes"))
(defmacro |DFloRTimesPlus| (x y z r) `(error "unimplemented operation -- DFloTimesPlus"))
(defmacro |DFloRDivide|(x y r) `(error "unimplemented operation -- DFloDivide"))

(defmacro |DFloDissemble| (x) `(error "unimplemented operation -- DFloDissemble"))
(defmacro |DFloAssemble| (w x y z) `(error "unimplemented operation -- DFloAssemble"))

;; Not builtins anymore
;;(defmacro |DFloRound|    (x) `(the |BInt| (round (the |DFlo| ,x))))
;;(defmacro |DFloTruncate| (x) `(the |BInt| (truncate (the |DFlo| ,x))))
;;(defmacro |DFloFloor|    (x) `(the |BInt| (floor (the |DFlo| ,x))))
;;(defmacro |DFloCeiling|  (x) `(the |BInt| (ceiling (the |DFlo| ,x))))

(defmacro |Byte0|         () 0)
(defmacro |Byte1|         () 1)
(defmacro |ByteMin|       () 0)
(defmacro |ByteMax|       () 255)

(defmacro |HInt0|         () 0)
(defmacro |HInt1|         () 1)
(defmacro |HIntMin|       () #.(- (expt 2 15)))
(defmacro |HIntMax|       () #.(1- (expt 2 15)))

(defmacro |SInt0|         () 0)
(defmacro |SInt1|         () 1)
(defmacro |SIntMin|       () `(the |SInt| #.(- (expt 2 31))))
(defmacro |SIntMax|       () `(the |SInt| #.(1- (expt 2 31))))
(defmacro |SIntIsZero|   (x) `(zerop (the |SInt| ,x)))
(defmacro |SIntIsNeg|    (x) `(minusp (the |SInt| ,x)))
(defmacro |SIntIsPos|    (x) `(plusp (the |SInt| ,x)))
(defmacro |SIntIsEven|   (x) `(evenp (the |SInt| ,x)))
(defmacro |SIntIsOdd|    (x) `(oddp (the |SInt| ,x)))
(defmacro |SIntLE|     (x y) `(<= (the |SInt| ,x) (the |SInt| ,y)))
(defmacro |SIntEQ|     (x y) `(= (the |SInt| ,x) (the |SInt| ,y)))
(defmacro |SIntLT|     (x y) `(< (the |SInt| ,x) (the |SInt| ,y)))
(defmacro |SIntNE|     (x y) `(/= (the |SInt| ,x) (the |SInt| ,y)))
(defmacro |SIntNegate|   (x) `(the |SInt| (- (the |SInt| ,x))))
(defmacro |SIntPrev|      (x) `(the |SInt| (1- (the |SInt| ,x))))
(defmacro |SIntNext|      (x) `(the |SInt| (1+ (the |SInt| ,x))))
(defmacro |SIntPlus|   (x y) `(the |SInt| (+ (the |SInt| ,x) (the |SInt| ,y))))
(defmacro |SIntMinus|  (x y) `(the |SInt| (- (the |SInt| ,x) (the |SInt| ,y))))
(defmacro |SIntTimes|  (x y) `(the |SInt| (* (the |SInt| ,x) (the |SInt| ,y))))
(defmacro |SIntTimesPlus| (x y z)
  `(the |SInt| (+ (* (the |SInt| ,x) (the |SInt| ,y)) (the |SInt| ,z))))
(defmacro |SIntMod|    (x y) `(the |SInt| (mod(the |SInt| ,x)(the |SInt| ,y))))
(defmacro |SIntQuo|    (x y)
  `(the |SInt| (values (truncate (the |SInt| ,x) (the |SInt| ,y)))))
(defmacro |SIntRem|    (x y) `(the |SInt| (rem(the |SInt| ,x)(the |SInt| ,y))))
;;! declare all let variables
(defmacro |SIntDivide| (x y) `(truncate (the |SInt| ,x) (the |SInt| ,y)))
(defmacro |SIntGcd|  (x y) `(the |SInt| (gcd (the |SInt| ,x) (the |SInt| ,y))))

(defmacro |SIntPlusMod|  (a b c)
  `(the |SInt| (mod (+ (the |SInt| ,a) (the |SInt| ,b)) (the |SInt| ,c))))
(defmacro |SIntMinusMod| (a b c)
  `(the |SInt| (mod (- (the |SInt| ,a) (the |SInt| ,b)) (the |SInt| ,c))))
(defmacro |SIntTimesMod| (a b c)
  `(the |SInt| (mod (* (the |SInt| ,a) (the |SInt| ,b)) (the |SInt| ,c))))
;; |SIntTimesModInv|
(defmacro |SIntLength|  (x) `(the |SInt| (integer-length (the |SInt| ,x))))
(defmacro |SIntShiftUp| (x y) `(the |SInt| (ash (the |SInt| ,x) (the |SInt| ,y))))
(defmacro |SIntShiftDn| (x y) `(the |SInt| (ash (the |SInt| ,x) (the |SInt| (- (the |SInt| ,y))))))

(defmacro |SIntBit|   (x i)
  `(let ((xx ,x) (ii ,i)) (declare (type |SInt| xx ii)) (logbitp ii xx)))
(defmacro |SIntNot|     (a) `(the |SInt| (lognot (the |SInt| ,a))))
(defmacro |SIntAnd|   (a b)
  `(the |SInt| (logand (the |SInt| ,a) (the |SInt| ,b))))
(defmacro |SIntOr|    (a b)
  `(the |SInt| (logior (the |SInt| ,a) (the |SInt| ,b))))

;; WordTimesDouble
;; WordDivideDouble
;; WordPlusStep
;; WordTimesStep

(defmacro |SIntSIPower| (x y)
  `(let ((xx ,x) (yy ,y))
     (declare (type |SInt| xx yy))
     (if (minusp yy) (error "cannot raise integers to negative powers")
       (the |SInt| (expt xx yy)))))
(defmacro |SIntBIPower| (x y)
  `(let ((xx ,x) (yy ,y))
     (declare (type |SInt| xx))
     (declare (type |BInt| yy))
     (if (minusp yy) (error "cannot raise integers to negative powers")
       (the |SInt| (expt xx yy)))))

(defmacro |BInt0|        () 0)
(defmacro |BInt1|        () 1)
(defmacro |BIntIsZero|  (x) `(zerop (the |BInt| ,x)))
(defmacro |BIntIsNeg|   (x) `(minusp(the |BInt| ,x)))
(defmacro |BIntIsPos|   (x) `(plusp (the |BInt| ,x)))
(defmacro |BIntIsEven|  (x) `(evenp (the |BInt| ,x)))
(defmacro |BIntIsOdd|   (x) `(oddp  (the |BInt| ,x)))
(defmacro |BIntIsSingle| (x) `(typep ,x '|SInt|))
(defmacro |BIntLE|    (x y) `(<= (the |BInt| ,x) (the |BInt| ,y)))
(defmacro |BIntEQ|    (x y) `(=  (the |BInt| ,x) (the |BInt| ,y)))
(defmacro |BIntLT|    (x y) `(<  (the |BInt| ,x) (the |BInt| ,y)))
(defmacro |BIntNE|    (x y) `(/= (the |BInt| ,x) (the |BInt| ,y)))
(defmacro |BIntNegate|  (x) `(the |BInt| (-   (the |BInt| ,x))))
(defmacro |BIntPrev|     (x) `(the |BInt| (1-  (the |BInt| ,x))))
(defmacro |BIntNext|     (x) `(the |BInt| (1+  (the |BInt| ,x))))
(defmacro |BIntPlus|  (x y) `(the |BInt| (+ (the |BInt| ,x) (the |BInt| ,y))))
(defmacro |BIntMinus| (x y) `(the |BInt| (- (the |BInt| ,x) (the |BInt| ,y))))
(defmacro |BIntTimes| (x y) `(the |BInt| (* (the |BInt| ,x) (the |BInt| ,y))))
(defmacro |BIntTimesPlus| (x y z)
  `(the |BInt| (+ (* (the |BInt| ,x) (the |BInt| ,y)) (the |BInt| ,z))))
(defmacro |BIntMod|   (x y) `(the |BInt| (mod(the |BInt| ,x)(the |BInt| ,y))))
(defmacro |BIntQuo|   (x y)
  `(the |BInt| (values (truncate (the |BInt| ,x) (the |BInt| ,y)))))
(defmacro |BIntRem|   (x y)
  `(the |BInt| (rem (the |BInt| ,x) (the |BInt| ,y))))
(defmacro |BIntDivide| (x y) `(truncate (the |BInt| ,x) (the |BInt| ,y)))
(defmacro |BIntGcd|   (x y)
  `(the |BInt| (gcd (the |BInt| ,x) (the |BInt| ,y))))
(defmacro |BIntSIPower| (x y)
  `(let ((xx ,x) (yy ,y))
     (declare (type |BInt| xx))
     (declare (type |SInt| yy))
     (if (minusp yy) (error "cannot raise integers to negative powers")
       (the |BInt| (expt xx yy)))))
(defmacro |BIntBIPower| (x y)
  `(let ((xx ,x) (yy ,y))
     (declare (type |BInt| xx))
     (declare (type |BInt| yy))
     (if (minusp yy) (error "cannot raise integers to negative powers")
       (the |BInt| (expt xx yy)))))
(defmacro |BIntLength|  (x) `(the |SInt| (integer-length (the |BInt| ,x))))
(defmacro |BIntShiftUp| (x y) `(the |BInt| (ash (the |BInt| ,x)(the |SInt| ,y))))
(defmacro |BIntShiftDn| (x y) `(the |BInt| (ash (the |BInt| ,x) (the |SInt| (- (the |SInt| ,y))))))

(defmacro |BIntBit|   (x i)
  `(let ((xx ,x) (ii ,i)) (declare (type |BInt| xx) (type |SInt| ii))
        (logbitp ii xx)))
;;(defmacro |BIntAbs|     (x) `(the |BInt| (abs (the |BInt| ,x))))

(defmacro |PtrNil|      () ())
(defmacro |PtrIsNil|   (x) `(NULL ,x))
(defmacro |PtrEQ|    (x y) `(eq ,x ,y))
(defmacro |PtrNE|    (x y) `(not (eq ,x ,y)))

;; |WordTimesDouble| |WordDivideDouble| |WordPlusStep| |WordTimesStep|


;;(defvar |FoamOutputString|
;;  (make-array 80 :element-type 'string-char :adjustable t :fill-pointer 0))
(defun |FormatNumber| (c arr i)
  (setq str (format nil "~a" c))
  (replace arr str :start1 i)
;;  (incf i (fill-pointer |FoamOutputString|))
;;  (if (> i (length arr)) (error "not enough space"))
;;  (setf (fill-pointer |FoamOutputString|) 0)
  (+ i (length str)))

(defmacro |FormatSFlo| (c arr i) `(|FormatNumber| ,c ,arr ,i))
(defmacro |FormatDFlo| (c arr i) `(|FormatNumber| ,c ,arr ,i))
(defmacro |FormatSInt| (c arr i) `(|FormatNumber| ,c ,arr ,i))
(defmacro |FormatBInt| (c arr i) `(|FormatNumber| ,c ,arr ,i))

(set-syntax-from-char (code-char 0) #\space) ;;makes null char act like white space

(defmacro |ScanSFlo| (arr i)
  `(read-from-string ,arr nil (|SFlo0|)
                     :start ,i :preserve-whitespace t))
(defmacro |ScanDFlo| (arr i)
  `(read-from-string ,arr nil (|DFlo0|)
                     :start ,i :preserve-whitespace t))
(defmacro |ScanSInt| (arr i)
  `(parse-integer ,arr :start ,i :junk-allowed t))
(defmacro |ScanBInt| (arr i)
  `(parse-integer ,arr :start ,i :junk-allowed t))

;; 18/8/93: Evil bug in genfoam---nil generated.
(defmacro hacked-the (type x)
  (if x `(the ,type ,x) `(the ,type 0)))

(defmacro |ByteToSInt| (x) `(coerce (hacked-the |Byte| ,x) '|SInt|))
(defmacro |BoolToSInt|  (x) `(if ,x 1 0))
(defmacro |BIntToSInt| (x) `(hacked-the |SInt| ,x))
(defmacro |SIntToBInt| (x) `(hacked-the |BInt| ,x))
(defmacro |SIntToSFlo| (x) `(coerce (hacked-the |SInt| ,x) '|SFlo|))
(defmacro |SIntToByte| (x) `(coerce (hacked-the |SInt| ,x) '|Byte|))
(defmacro |SIntToHInt| (x) `(coerce (hacked-the |SInt| ,x) '|HInt|))
(defmacro |SIntToDFlo| (x) `(coerce (hacked-the |SInt| ,x) '|DFlo|))
(defmacro |BIntToSFlo| (x) `(coerce (hacked-the |BInt| ,x) '|SFlo|))
(defmacro |BIntToDFlo| (x) `(coerce (hacked-the |BInt| ,x) '|DFlo|))
(defmacro |ArrToSFlo|  (x) `(read-from-string ,x nil (|SFlo0|)))
(defmacro |ArrToDFlo|  (x) `(read-from-string ,x nil (|DFlo0|)))
(defmacro |ArrToSInt|  (x) `(read-from-string ,x nil (|SInt0|)))
(defmacro |ArrToBInt|  (x) `(read-from-string ,x nil (|BInt0|)))

(defmacro |Clos| (x y) `(let ((xx ,x) (yy #',y)) (cons yy xx)))
(defmacro |ClosFun| (x) `(car ,x))
(defmacro |ClosEnv| (x) `(cdr ,x))
(defmacro |SetClosFun| (x y) `(rplaca ,x ,y))
(defmacro |SetClosEnv| (x y) `(rplacd ,x ,y))

(defmacro |MakeEnv|     (x y) 
  `(let ((xx ,x) (yy ,y)) (cons yy (cons xx nil))))

(defmacro |EnvLevel|    (x)   `(car ,x))
(defmacro |EnvNext|     (x)   `(cadr ,x))
(defmacro |EnvInfo|     (x)   `(if (and (consp ,x) (consp (cdr ,x)))
                                   (cddr ,x) nil))
(defmacro |SetEnvInfo|  (x val)   `(rplacd (cdr  ,x) ,val))

#+:CCL
(defmacro |FoamEnvEnsure| (e) 
  `(let ((einf (|EnvInfo| ,e)))
     (if einf (|CCall| einf) nil)))
#-:CCL
(defmacro |FoamEnvEnsure| (e) 
  `(if (|EnvInfo| ,e) (|CCall| (|EnvInfo| ,e)) nil))

(defparameter null-char-string (string (code-char 0)))
(defmacro |MakeLit| (s) `(concatenate 'string ,s null-char-string))

;; functions are represented by symbols, with the symbol-value being some
;; information, and the symbol-function is the function itself.
;; 1-valued lisp should represent progs as either a pair or defstruct.

(defmacro |FunProg| (x) x)

(defstruct FoamProgInfoStruct
  (funcall #'(lambda () (error "FoamProgInfoStruct: funcall not assigned")) :type function)
  (hashval 0   :type |SInt|))

(defun |ProgHashCode| (x)
  (let ((aa (foam-function-info x)))
    (if (null aa) 0
      (FoamProgInfoStruct-hashval aa))))

(defun |SetProgHashCode| (x y)
  (let ((aa (foam-function-info x)))
    (if (null aa) 0
      (setf (FoamProgInfoStruct-hashval aa) y))))

;; In a hurry -> O(n) lookup..
(defvar foam-function-list ())

(defun alloc-prog-info (fun val)
  (setq foam-function-list (cons (cons fun val) foam-function-list)))

(defun foam-function-info (fun)
  (let ((xx (assoc fun foam-function-list)))
    (if (null xx) nil
      (cdr xx))))

;; Accessors and constructors
(defmacro |DDecl| (name &rest args)
  (setf (get name 'struct-args) args)
  `(defstruct ,name ,@(insert-types args)))

(defun insert-types (slots)
  (mapcar #'(lambda (slot)
              `(,(car slot) ,(type2init (cadr slot))
                :type ,(cadr slot)))
          slots))

(defmacro |RNew| (name)
  (let* ((struct-args (get name 'struct-args))
         (init-args (mapcar #'(lambda (x) (type2init (cadr x)))
                            struct-args))
         (count (length struct-args)))
    (cond ((> count 2) `(vector ,@init-args))
          ((= count 2) `(cons ,@init-args))
          (t `(list ,@init-args)))))

(defmacro |RElt| (name field index rec)
  (let ((count (length (get name 'struct-args))))
    (cond ((> count 2) `(svref ,rec ,index))
          ((= count 2)
           (if (zerop index) `(car ,rec) `(cdr ,rec)))
          (t `(car ,rec)))))

(defmacro |SetRElt| (name field index rec val)
  (let ((count (length (get name 'struct-args))))
    (cond ((> count 2) `(setf (svref ,rec ,index) ,val))
          ((= count 2)
           (if (zerop index) `(rplaca ,rec ,val) `(rplacd ,rec ,val)))
          (t `(rplaca ,rec ,val)))))

(defmacro |AElt| (name index)
  `(aref ,name ,index))

(defmacro |SetAElt| (name index val)
  `(setf (aref ,name ,index) ,val))

(defmacro |MakeLevel| (builder struct)
  (if (get struct 'struct-args)
      `(,builder)
    'nil))


(defmacro |EElt| (accessor n var)
  `(,accessor ,var))

(defmacro |SetEElt| (accessor n var val)
  `(setf (,accessor ,var) ,val))

(defmacro |Lex| (accessor n var)
  `(,accessor ,var))

(defmacro |SetLex| (accessor n var val)
  `(progn ;; (print ',accessor)
          (setf (,accessor ,var) ,val)))

;; Atomic arguments for fun don't need a let to hold the fun.
;; CCall's with arguments need a let to hold the prog and the env.
(defmacro |CCall| (fun &rest args)
  (cond ((and (atom fun) (null args))
         `(funcall (|FunProg| (|ClosFun| ,fun)) (|ClosEnv| ,fun)))
        ((null args)
         `(let ((c ,fun))
            (funcall (|FunProg| (|ClosFun| c)) (|ClosEnv| c))))
        ((atom fun)
         `(let ((fun (|FunProg| (|ClosFun| ,fun)))
                (env (|ClosEnv| ,fun)))
            (funcall fun ,@args env)))
        (t
         `(let ((c ,fun))
            (let ((fun (|FunProg| (|ClosFun| c)))
                  (env (|ClosEnv| c)))
              (funcall fun ,@args env))))))

(defmacro |FoamFree| (o) '())

;; macros for defining things

;; name-result is a list, the car is the name of the function to be declared,
;; the cdr is the list of return values
;; params is a list of pairs, the car of each is the name of the argument, the
;; cdr is its type.

;; in the ANSI Common Lisp ftype function declaration, the names of the
;; arguments do not appear, actually.  In GCL, they did.  

;; Example:
;; (declare-prog
;;  (|C25-csspecies-generBaseFn| |Clos| |Clos| |Clos| |Clos|)
;;  ((|e1| |Env|)))
(defmacro declare-prog (name-result params)
  `(proclaim '(ftype (function
		      ,(mapcar #'cadr params)
		      (values ,@(cdr name-result)))
		     ,(car name-result))))

(defmacro declare-type (name type)
  `(proclaim '(type ,name ,type)))

(defmacro defprog (type temps &rest body)
  `(progn (defun ,(caar type) ,(mapcar #'car (cadr type))
            (typed-let ,temps ,@body))
          (alloc-prog-info #',(caar type) (make-FoamProgInfoStruct))))

(defmacro defspecials (&rest lst)
  `(proclaim '(special ,@lst)))

(defmacro top-level-define (&rest junk)
  `(setq ,@junk))

;; Runtime macros

;; control transfer
(defmacro block-return (obj val)
  `(return-from ,obj ,val))

#-:CCL
(defmacro typed-let (letvars &rest forms)
  `(let ,(mapcar #'(lambda (var)
                     (list (car var) (type2init (cadr var))))
                 letvars )
     (declare ,@(mapcar #'(lambda (var)
                            (list 'type (cadr var) (car var)))
                        letvars))
     ,@forms))

#+:CCL
(defmacro typed-let (letvars &rest forms)
  `(let ,(mapcar #'(lambda (var) (car var))
                 letvars )
     ,@forms))

(defmacro cases (&rest junk)
  `(case ,@junk))


;;; Boot macros
(defmacro file-exports (lst)
  `(eval-when (load eval)
              (when (fboundp 'process-export-entry)
                    (mapcar #'process-export-entry ,lst))
        nil))

(defmacro file-imports (lst)
  `(eval-when (load eval)
              (when (fboundp 'process-import-entry)
                    (mapcar #'process-import-entry ,lst))
        nil))

(defmacro ignore-var (var)
  `(declare (ignore ,var)))

(defmacro |ANew| (type size)
  (if (eq type '|Char|)
      `(make-string ,size)
      `(make-array ,size
               :element-type ',type
               :initial-element ,(type2init type))))

#-:CCL
(defun type2init (x)
  (cond
   ((eq x '|Char|) '|CharInit|)
   ((eq x '|Clos|) '|ClosInit|)
   ((eq x '|Bool|) '|BoolInit|)
   ((eq x '|Byte|) '|ByteInit|)
   ((eq x '|HInt|) '|HIntInit|)
   ((eq x '|SInt|) '|SIntInit|)
   ((eq x '|BInt|) '|BIntInit|)
   ((eq x '|SFlo|) '|SFloInit|)
   ((eq x '|DFlo|) '|DFloInit|)
   ((eq x '|Ptr|) '|PtrInit|)
   ((eq x '|Word|) '|WordInit|)
   ((eq x '|Arr|) '|ArrInit|)
   ((eq x '|Record|) '|RecordInit|)
   ((eq x '|Arb|) '|ArbInit|)
   ((eq x '|Env|) '|EnvInit|)
   ((eq x '|Level|) '|LevelInit|)
   ((eq x '|Nil|) nil)
   (t nil)))

#+:CCL
(defun type2init (x) nil)

;; opsys interface
(defvar |G-mainArgc| 0)
(defvar |G-mainArgv| (vector))
(defmacro |stdinFile| () '*standard-input*)
(defmacro |stdoutFile| () '*standard-output*)
(defmacro |stderrFile| () '*error-output*)

;; Format functions
;needs to stop when it gets a null character
(defun |strLength| (s)
  (dotimes (i (length s))
           (let ((c (schar s i)))
             (if (char= c |CharCode0|)
                 (return i))))
  (length s))

(defun |formatSInt| (n) (format nil "~D" n))
(defun |formatBInt| (n) (format nil "~D" n))
(defun |formatSFloat| (x) (format nil "~G" x))
(defun |formatDFloat| (x) (format nil "~G" x))


;; Printing functions
(defun |printNewLine| (cs) (terpri cs))
(defun |printChar|  (cs c) (princ c cs))

;needs to stop when it gets a null character
(defun |printString| (cs s)
  (dotimes (i (length s))
           (let ((c (schar s i)))
             (if (char= c |CharCode0|)
                 (return i)
               (princ c cs)))))

(defun |printSInt| (cs n) (format cs "~D" n))
(defun |printBInt| (cs n) (format cs "~D" n))
(defun |printSFloat| (cs x) (format cs "~G" x))
(defun |printDFloat| (cs x) (format cs "~G" x))

(defun |fputc| (si cs)
  (|printChar| cs (code-char si))
  si)

(defun |fputs| (s cs)
  (|printString| cs s))

;; read a string into s starting at pos i1, ending at i2
;; we should probably macro-out cases where args are constant

;; fill s[i1..i2] with a null terminated string read from
;; the given input stream
(defun |fgetss| (s i1 i2 f)
  (labels ((aux (n)
                (if (= n i2)
                    (progn (setf (schar s n) (code-char 0))
                           (- n i1))
                  (let ((c (read-char f)))
                    (setf (schar s n) c)
                    (if (equal c #\newline)
                        (progn (setf (char s (+ n 1)) (code-char 0))
                               (- n i1))
                      (aux (+ n 1)))))))
          (aux i1)))
                
;; write s[i1..i2) to the output stream f
;; stop on any null characters

(defun |fputss| (s i1 i2 f)
  (labels ((aux (n)
                (if (= n i2) (- n i1)
                  (let ((c (schar s n)))
                    (if  (equal (code-char 0) c)
                         (- n i1)
                      (progn (princ c f)        
                             (aux (+ n 1))))))))
          (setq i2 (if (minusp i2) (|strLength| s)
                     (min i2 (|strLength| s))))
          (aux i1)))

;; function for compiling and loading from lisp

(defun compile-as-file (file &optional (opts nil))
  (let* ((path (pathname file))
         (name (pathname-name path))
         (dir (pathname-directory path))
         (type (pathname-type path))
         (lpath (make-pathname :name name :type "l"))
         (cpath (make-pathname :name name :type "o")))
    (if (null type)
        (setq path (make-pathname :directory dir :name name :type "as")))
    (if opts
        (OBEY (format nil "axiomxl ~A -Flsp ~A" opts (namestring path)))
        (OBEY (format nil "axiomxl -Flsp ~A" (namestring path))))
    (compile-file (namestring lpath))
    (load (namestring cpath))))


;; given the name of a file (a string), return the name of the AXIOM-XL function
;; that initialises the file.
(defun axiomxl-file-init-name (filename)
  (intern (format nil "G-~a" (string-downcase filename)) 'foam-user))

;; given the name of the file, id name, and hashcode, return the
;; AXIOM-XL identifier for that object

(defun axiomxl-global-name (file id hashcode)
  (intern (format nil "G-~a_~a_~9,'0d" (string-downcase file) id hashcode) 'foam-user))

;; double float elementary functions
(defmacro |sqrt| (x) `(sqrt ,x))
(defmacro |pow| (a b) `(expt ,a ,b))
(defmacro |log| (a)  `(log ,a))
(defmacro |exp| (a) `(exp ,a))

(defmacro |sin| (a) `(sin ,a))
(defmacro |cos| (a) `(cos ,a))
(defmacro |tan| (a) `(tan ,a))

(defmacro |sinh| (a) `(sinh ,a))
(defmacro |cosh| (a) `(cosh ,a))
(defmacro |tanh| (a) `(tanh ,a))

(defmacro |asin| (a) `(asin ,a))
(defmacro |acos| (a) `(acos ,a))
(defmacro |atan| (a) `(atan ,a))
(defmacro |atan2| (a b) `(atan ,a ,b))

(defun |Halt| (n) 
  (error (cond ((= n 101) "System Error: Unfortunate use of dependant type")
               ((= n 102) "User error: Reached a 'never'")
               ((= n 103) "User error: Bad union branch")
               ((= n 104) "User error: Assertion failed")
               (t (format nil "Unknown halt condition ~a" n)))))
;; debuging
(defvar *foam-debug-var* nil)
(defun |fiGetDebugVar| () *foam-debug-var*)

(defun |fiSetDebugVar| (x) (setq *foam-debug-var* x))
(defun |fiSetDebugger| (x y) ())
(defun |fiGetDebugger| (x) ())

;; Output ports
(setq |G-stdoutVar| t)
(setq |G-stdinVar| t)
(setq |G-stderrVar| t)

;; !! Not portable !!
(defun foam::|fiStrHash| (x) (boot::|hashString| (subseq x 0 (- (length x) 1))))

;; These three functions check that two cons's contain identical entries.
;; We use EQL to test numbers and EQ everywhere else.  If the structure 
;; of the two items is different, or any elements are different, we
;; return false.
(defmacro |politicallySound| (u v) 
 `(or (eql ,u ,v) (eq ,u ,v)))

(defun |PtrMagicEQ| (u v) 
;; I find (as-eg4) that these buggers can be numbers 
 (cond ( (or (NULL u) (NULL v)) nil)
       ( (and (ATOM u) (ATOM v)) (eql u v))
       ( (or (ATOM u) (ATOM v)) nil)
;; removed for Aldor integration
;;       ( (equal (length u) (length v)) (|magicEq1| u v)) 
       (t (eq u v) )))

(defun |magicEq1| (u v)
 (cond ( (and (atom u) (atom v)) (|politicallySound| u v))
       ( (or (atom u) (atom v)) nil)
       ( (|politicallySound| (car u) (car v)) (|magicEq1| (cdr u) (cdr v)))
       (t nil) ))

