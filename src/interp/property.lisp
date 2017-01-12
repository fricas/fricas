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


#|
This file contains most of the code that puts properties on
identifiers in the Scratchpad II system.  If it was not possible
to actually put the code here, we have pointers to where such
property list manipulation is being done.

Pointers:
o  coerceIntCommute puts the "commute" property on constructors.
o  coerceRetract puts the "retract" property on constructors.
o  there is some code at the end of SPECEVAL BOOT that puts "up"
   properties on some special handlers.
|#

(in-package "BOOT")

;; following was in OUTINIT LISP

(DOLIST (X '(
   (LET " := ")
   (|/| "/")
   (* "*")
   (TENSOR " # ")
   (** "**")
   (^ "^")
   (|:| ":")
   (|::| "::")
   (|@| "@")
   (SEL ".")
   (|exquo| " exquo ")
   (|div| " div ")
   (|quo| " quo ")
   (|rem| " rem ")
   (|case| " case ")
   (|and| " and ")
   (|or| " or ")
   (TAG " -> ")
   (|+->| " +-> ")
   (SEGMENT "..")
   (in " in ")
   (|~=|  "~=")
   (JOIN " JOIN ")
   (EQUATNUM "  ")
   (IQUOTIENT "//")
   (= " = ")
   (|>=| " >= ")
   (|>| " > ")
   (|<=| " <= ")
   (|<| " < ")
   (\| " \| ")
;;   (+ "+")
   (+ " + ")
   (- " - ")
   (WHERE " WHERE ")
   (AT " AT ")
   (MAX " MAX ")
   (MIN " MIN ")
       )) (MAKEPROP (CAR X) 'INFIXOP (CADR X)))

(DOLIST (X '(
  (= "=")
  (|:| ":")
  (|not| "~ ")
  (\| " \| ")
  (SEGMENT "..")  ;" 0.. is represented by (SEGMENT 0)"
 )) (MAKEPROP (CAR X) 'PREFIXOP (CADR X)))

(DOLIST (X '(
  (- APP |appneg|)
  (- WIDTH |minusWidth|)
  (/ APP |appfrac|)
  (/ SUBSPAN |fracsub|)
  (/ SUPERSPAN |fracsuper|)
  (/ WIDTH |fracwidth|)
  (AGGSET APP |argsapp|)
  (AGGSET SUBSPAN |agggsub|)
  (AGGSET SUPERSPAN |agggsuper|)
  (AGGSET WIDTH |agggwidth|)
  (|binom| APP |binomApp|)
  (|binom| SUBSPAN |binomSub|)
  (|binom| SUPERSPAN |binomSuper|)
  (|binom| WIDTH |binomWidth|)
  (ALTSUPERSUB APP       |altSuperSubApp|)
  (ALTSUPERSUB SUBSPAN   |altSuperSubSub|)
  (ALTSUPERSUB SUPERSPAN |altSuperSubSuper|)
  (ALTSUPERSUB WIDTH     |altSuperSubWidth|)
  (BOX APP |boxApp|)
  (BOX SUBSPAN |boxSub|)
  (BOX SUPERSPAN |boxSuper|)
  (BOX WIDTH |boxWidth|)
  (BRACKET SUBSPAN |qTSub|)
  (BRACKET SUPERSPAN |qTSuper|)
  (BRACKET WIDTH |qTWidth|)
  (CENTER APP |centerApp|)
  (EXT APP |appext|)
  (EXT SUBSPAN |extsub|)
  (EXT SUPERSPAN |extsuper|)
  (EXT WIDTH |extwidth|)
  (MATRIX APP |appmat|)
  (MATRIX SUBSPAN |matSub|)
  (MATRIX SUPERSPAN |matSuper|)
  (MATRIX WIDTH |matWidth|)
  (NOTHING APP       |nothingApp|)
  (NOTHING SUPERSPAN |nothingSuper|)
  (NOTHING SUBSPAN   |nothingSub|)
  (NOTHING WIDTH     |nothingWidth|)
  (OVER APP |appfrac|)
  (OVER SUBSPAN |fracsub|)
  (OVER SUPERSPAN |fracsuper|)
  (OVER WIDTH |fracwidth|)
  (OVERLABEL APP |overlabelApp|)
  (OVERLABEL SUPERSPAN |overlabelSuper|)
  (OVERLABEL WIDTH |overlabelWidth|)
  (OVERBAR APP |overbarApp|)
  (OVERBAR SUPERSPAN |overbarSuper|)
  (OVERBAR WIDTH |overbarWidth|)
  (PAREN APP |appparu1|)
  (PAREN SUBSPAN |qTSub|)
  (PAREN SUPERSPAN |qTSuper|)
  (PAREN WIDTH |qTWidth|)
  (PRIME APP        |primeApp|)
  (PRIME SUBSPAN    |primeSub|)
  (PRIME SUPERSPAN  |primeSuper|)
  (PRIME WIDTH      |primeWidth|)
  (ROOT APP       |rootApp|)
  (ROOT SUBSPAN   |rootSub|)
  (ROOT SUPERSPAN |rootSuper|)
  (ROOT WIDTH     |rootWidth|)
  (ROW WIDTH |eq0|)
  (SC APP |appsc|)
  (SC SUBSPAN |agggsub|)
  (SC SUPERSPAN |agggsuper|)
  (SC WIDTH |widthSC|)
  (SETQ APP |appsetq|)
  (SETQ WIDTH |letWidth|)
  (SLASH APP       |slashApp|)
  (SLASH SUBSPAN   |slashSub|)
  (SLASH SUPERSPAN |slashSuper|)
  (SLASH WIDTH     |slashWidth|)
  (SUB APP |appsub|)
  (SUB SUBSPAN |subSub|)
  (SUB SUPERSPAN |subSuper|)
  (SUB WIDTH |suScWidth|)
  (SUPERSUB APP |superSubApp|)
  (SUPERSUB SUBSPAN |superSubSub|)
  (SUPERSUB SUPERSPAN |superSubSuper|)
  (SUPERSUB WIDTH |superSubWidth|)
  (VCONCAT APP |vconcatapp|)
  (VCONCAT SUBSPAN |vConcatSub|)
  (VCONCAT SUPERSPAN |vConcatSuper|)
  (VCONCAT WIDTH |vConcatWidth|)
  (BINOMIAL APP |binomialApp|)
  (BINOMIAL SUBSPAN |binomialSub|)
  (BINOMIAL SUPERSPAN |binomialSuper|)
  (BINOMIAL WIDTH |binomialWidth|)
  (ZAG APP |zagApp|)
  (ZAG SUBSPAN |zagSub|)
  (ZAG SUPERSPAN |zagSuper|)
  (ZAG WIDTH |zagWidth|)
)) (PROGN (MAKEPROP (CAR X) (CADR X) (CADDR X)))
)

(DOLIST (X '(
  (+ APP |plusApp|)
  (+ WIDTH |sumWidth|)
  (* APP |timesApp|)
  (* WIDTH |timesWidth|)
  (TENSOR APP |tensorApp|)
  (TENSOR WIDTH |tensorWidth|)
  (** APP |exptApp|)
  (** WIDTH |exptWidth|)
  (** SUBSPAN |exptSub|)
  (** SUPERSPAN |exptSuper|)
  (^  APP |exptApp|)
  (^  WIDTH |exptWidth|)
  (^  SUBSPAN |exptSub|)
  (^  SUPERSPAN |exptSuper|)
  (STEP APP |stepApp|)
  (STEP WIDTH |stepWidth|)
  (STEP SUBSPAN |stepSub|)
  (STEP SUPERSPAN |stepSuper|)
  (IN APP |inApp|)
  (IN WIDTH |inWidth|)
  (IN SUBSPAN |inSub|)
  (IN SUPERSPAN |inSuper|)
  (AGGLST APP |aggApp|)
  (AGGLST SUBSPAN |aggSub|)
  (AGGLST SUPERSPAN |aggSuper|)
  (CONCATB APP |concatbApp|)
  (CONCATB SUBSPAN |concatSub|)
  (CONCATB SUPERSPAN |concatSuper|)
  (CONCATB WIDTH |concatbWidth|)
  (CONCAT APP |concatApp|)
  (CONCAT  SUBSPAN |concatSub|)
  (CONCAT SUPERSPAN |concatSuper|)
  (CONCAT WIDTH |concatWidth|)
  (QUOTE APP |quoteApp|)
  (QUOTE SUBSPAN |quoteSub|)
  (QUOTE SUPERSPAN |quoteSuper|)
  (QUOTE WIDTH |quoteWidth|)
  (STRING APP |stringApp|)
  (STRING SUBSPAN |eq0|)
  (STRING SUPERSPAN |eq0|)
  (STRING WIDTH |stringWidth|)
  (SIGMA APP |sigmaApp|)
  (SIGMA SUBSPAN |sigmaSub|)
  (SIGMA SUPERSPAN |sigmaSup|)
  (SIGMA WIDTH |sigmaWidth|)
  (SIGMA2 APP |sigma2App|)
  (SIGMA2 SUBSPAN |sigma2Sub|)
  (SIGMA2 SUPERSPAN |sigma2Sup|)
  (SIGMA2 WIDTH |sigma2Width|)
  (INTSIGN APP |intApp|)
  (INTSIGN SUBSPAN |intSub|)
  (INTSIGN SUPERSPAN |intSup|)
  (INTSIGN WIDTH |intWidth|)
  (PI APP |piApp|)
  (PI SUBSPAN |piSub|)
  (PI SUPERSPAN |piSup|)
  (PI WIDTH |piWidth|)
  (PI2 APP |pi2App|)
  (PI2 SUBSPAN |pi2Sub|)
  (PI2 SUPERSPAN |pi2Sup|)
  (PI2 WIDTH |pi2Width|)
  (AGGLST WIDTH |aggWidth|)
  (BRACKET APP |bracketApp|)
  (BRACE APP |braceApp|)
  (BRACE WIDTH |qTWidth|)
)) (PROGN (MAKEPROP (CAR X) (CADR X) (CADDR X)))
)

;; following was in INIT LISP

(MAKEPROP '|Integer| '|Subsets|
  '((|PositiveInteger| . (|>| * 0))
    (|NonNegativeInteger| . (|>=| * 0))
    (|NegativeInteger| . (|<| * 0))
    (|NonPositiveInteger| . (|<=| * 0))
    (|NonZeroInteger| . (~= * 0))
    (|SingleInteger| . (SINTP *))
    ))

(MAKEPROP '|NonNegativeInteger| '|Subsets| '(
  (|PositiveInteger| . (|>| * 0))
  ))

(MAKEPROP '|NonPositiveInteger| '|Subsets| '(
  (|NegativeInteger| . (|<| * 0))
  ))

(DOLIST (X '(TENSOR * + AND OR PROGN)) (MAKEPROP X 'NARY T))

(DOLIST (X '(
  (|Record| |mkRecordFunList|)
  (|Union| |mkUnionFunList|)
  (|Mapping| |mkMappingFunList|)
  (|Enumeration| |mkEnumerationFunList|)
)) (MAKEPROP (CAR X) '|makeFunctionList| (CADR X)))

(DOLIST (X '(

  (|and| |parseAnd|)
  (CATEGORY |parseCategory|)
  (DEF |parseDEF|)
  (|exit| |parseExit|)
  (|has| |parseHas|)
  (IF |parseIf|)
  (|Join| |parseJoin|)
  (|leave| |parseLeave|)
  (MDEF |parseMDEF|)
  (|not| |parseNot|)
  (|or| |parseOr|)
  (SEGMENT |parseSegment|)
  (SEQ |parseSeq|)
)) (MAKEPROP (CAR X) '|parseTran| (CADR X)))

(DOLIST (X '(
  (|with| |postWith|)
  (/ |postSlash|)
  (|construct| |postConstruct|)
  (QUOTE |postQUOTE|)
  (COLLECT |postCollect|)
  (|:BF:| |postBigFloat|)
  (|in| |postin|)  ;" the infix operator version of in"
  (IN |postIn|)  ;" the iterator form of in"
  (REPEAT |postRepeat|)
  (|add| |postAdd|)
  (|Reduce| |postReduce|)
  (\, |postComma|)
  (\; |postSemiColon|)
  (|where| |postWhere|)
  (|::| |postColonColon|)
  (\: |postColon|)
  (@ |postAtSign|)
  (|pretend| |postPretend|)
  (|if| |postIf|)
  (|Join| |postJoin|)
  (|Signature| |postSignature|)
  (CATEGORY |postCategory|)
  (== |postDef|)
  (|==>| |postMDef|)
  (|->| |postMapping|)
  (|=>| |postExit|)
  (|@Tuple| |postTuple|)
)) (MAKEPROP (CAR X) '|postTran| (CADR X)))

(DOLIST (X '(
  (\| |compSuchthat|)
  (\@ |compAtSign|)
  (|:| |compColon|)
  (\:\: |compCoerce|)
  (|+->| |compLambda|)
  (QUOTE |compQuote|)

  (|add| |compAdd|)
  (CAPSULE |compCapsule|)
  (|case| |compCase|)
  (CATEGORY |compCategory|)
  (COLLECT |compRepeatOrCollect|)
  (COLLECTV |compCollectV|)
  (|construct| |compConstruct|)
  (DEF |compDefine|)
  (|exit| |compExit|)
  (|has| |compHas|)
  (IF |compIf|)
  (|import| |compImport|)
  (|is| |compIs|)
  (|Join| |compJoin|)
  (|leave| |compLeave|)
  (LET |compSetq|)
  (MDEF |compMacro|)
  (|pretend| |compPretend|)
  (|Record| |compCat|)
  (REDUCE |compReduce|)
  (REPEAT |compRepeatOrCollect|)
  (|return| |compReturn|)
  (|Sel| |compSel|)
  (SEQ |compSeq|)
  (|SubDomain| |compSubDomain|)
  (|try| |comp_try|)
  (|Union| |compCat|)
  (|Mapping| |compCat|)
  (|where| |compWhere|)
)) (MAKEPROP (CAR X) 'SPECIAL  (CADR X)))
