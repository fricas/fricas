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


;;; This is the package that originally contained the VMLisp macros
;;; but in fact contains macros to support several other lisps. It
;;; is essentially the place where most of the macros to support
;;; idioms from prior ports (like rdefiostream and fileactq)
(make-package "VMLISP" :use '("FRICAS-LISP"))

;;; This is the boot to lisp compiler package which contains the
;;; src/boot files. Tt is the boot translator package.
(or (find-package "BOOTTRAN") (make-package "BOOTTRAN" :use '("FRICAS-LISP")))

;;; Everything in axiom that the user references eventually shows
;;; up here. The interpreter and the algebra are run after switching
;;; to the boot package (in-package "BOOT") so any symbol that the
;;; interpreter or algebra uses has to (cough, cough) appear here.
(make-package "BOOT" :use '("VMLISP" "FRICAS-LISP"))

;;; FOAM is the intermediate language for the aldor compiler. FOAM
;;; means "first order abstract machine" and functions similar to
;;; RTL for the GCC compiler. It is a "machine" that is used as the
;;; target for meta-assembler level statments. These are eventually
;;; expanded for the real target machine (or interpreted directly)
(make-package "FOAM" :use '("FRICAS-LISP"))

;;; FOAM-USER is the package containing foam statements and macros
;;; that get inserted into user code versus the foam package which
;;; provides support for compiler code.
(make-package "FOAM-USER" :use '("FRICAS-LISP" "FOAM"))

(in-package "BOOT")

;;; Definitions for package VMLISP of type EXPORT
(in-package "VMLISP")

(export
    '(SINTP $FCOPY PUT EQCAR RDEFIOSTREAM MLAMBDA
         QSLESSP QSDIFFERENCE QSQUOTIENT
         EQSUBSTLIST QCAAAR $TOTAL-ELAPSED-TIME
         QUOTIENT SORTGREATERP QSETREFV QSTRINGLENGTH EVALFUN
         QCDAR TEMPUS-FUGIT QSPLUS QSABSVAL QSZEROP QSMIN QSLEFTSHIFT
         SETDIFFERENCE RPLQ CATCHALL RECOMPILE-DIRECTORY MDEF
         NILFN TAB QCDDR IOSTATE SFP NE STRGREATERP
         USE-VMLISP-SYNTAX RCLASS RSETCLASS SEQ FIXP MAKE-CVEC
         |F,PRINT-ONE| HASHUEQUAL $OUTFILEP TIMES DIFFERENCE MSUBST DIVIDE
         |remove| GETL QCADAR QCAAAAR RECLAIM ORADDTEMPDEFS NAMEDERRSET
         TRIMSTRING CURRINDEX EVALANDFILEACTQ LISPLIB FLUID MDEFX COMP370
         NEQ GETREFV |log| QVSIZE MBPIP RPLNODE QSORT PLACEP RREAD BINTP
         QSODDP RVECP CHAR2NUM POPP QCDAADR HKEYS HASHCVEC HASHID
         REMOVEQ LISTOFFUNCTIONS QCADAAR ABSVAL VMPRINT
         MAKE-APPENDSTREAM MAKE-INSTREAM HASHTABLEP UPCASE
         LOADCOND STRPOSL STATEP QCDADR HREMPROP LAM FBPIP NCONC2
         GETFULLSTR HREM *LISP-BIN-FILETYPE* INT2RNUM EBCDIC
         $INFILEP BFP NUMP UNEMBED PAIRP BOOLEANP FIX REMAINDER
         QCAADDR QCDDADR $LISTFILE IVECP LIST2VEC $ERASE QSDEC1
         QSSUB1 QCAR EVA1FUN IS-CONSOLE MAKESTRING CUROUTSTREAM QCDDDR
         QCDADAR MAKE-ABSOLUTE-FILENAME SUFFIX QRPLACA GGREATERP
         CGREATERP RNUMP RESETQ QRPLACD SORTBY CVECP SETELT HGET
         $DIRECTORY-LIST LN |member| AXIOM-MEMBER $LIBRARY-DIRECTORY-LIST
         QCSIZE QCADDDR RWRITE SUBLOAD STRINGIMAGE $CLEAR |read-line|
         INTP OUTPUT CONSOLE QCDDDAR ADDOPTIONS $FILETYPE-TABLE
         QSMINUSP |assoc| SETSIZE QCDR EFFACE COPY DOWNCASE LC2UC
         EMBED SETANDFILEQ QSMAX LIST2REFVEC MACRO-INVALIDARGS EMBEDDED
         REFVECP CLOSEDFN MAKE-HASHTABLE MAKE-FILENAME
         |$defaultMsgDatabaseName| LEXGREATERP IDENTP QSINC1 QESET MRP
         LESSP RPLPAIR QVELT QRPLQ MACERR *FILEACTQ-APPLY*
         $FILEP MAKE-FULL-CVEC HCLEAR HPUTPROP STRING2ID-N CALLBELOW BPINAME
         CHANGELENGTH OBEY QASSQ DCQ SHUT FILE HPUT MAKEPROP GREATERP
         REROOT DIG2FIX L-CASE TEREAD QSREMAINDER $FINDFILE
         PRETTYPRINT HASHEQ LOG2 U-CASE NREMOVE QREFELT SIZE
         EOFP QCDAAR RSHUT ADD1 QMEMQ SUBSTRING LOADVOL
         QSTIMES STRINGLENGTH NEXT DEVICE MAPELT LENGTHOFBPI
         DIGITP QLENGTH QCAAADR CVEC VEC2LIST MODE MAKE-VEC GCMSG
         CONCAT $SHOWLINE QCAADR QCDDAR QCDAAAR RDROPITEMS VECP
         |union| ONE-OF NULLOUTSTREAM QSGREATERP MINUS MAXINDEX
         GETSTR QCADADR PRIN2CVEC CURRENTTIME $REPLACE UNIONQ
         NREMOVEQ CURINSTREAM MAKE-OUTSTREAM APPLX LASTNODE SUBSTQ TRUEFN
         |last| RPLACSTR SETQP QCADDR QCAADAR QCDDAAR |intersection|
         HASHTABLE-CLASS *COMP370-APPLY* QSETVELT MOVEVEC
         ID DEFINE-FUNCTION MSUBSTQ |nsubst| SUB1 NUMBEROFARGS
         VMREAD SMINTP $SCREENSIZE QCDADDR COMPRREAD GENSYMP IFCAR QSETQ
         QCADDAR *LISP-SOURCE-FILETYPE* KOMPILE INPUT UEQUAL COMPRWRITE
         SUBRP ASSEMBLE IFCDR QVMAXINDEX $SPADROOT PRIN0 PRETTYPRIN0
         STACKLIFO ASSQ PRINTEXP QCDDDDR QSADD1 QLESSP
         SETDIFFERENCEQ STRPOS CONSTANT QCAAR HCOUNT RCOPYITEMS
         QSMINUS EVA1 OPTIONLIST NUM2CHAR QENUM $TOTAL-GC-TIME CHARP QCADR
         INTERSECTIONQ DSETQ FETCHCHAR STRCONC MACRO-MISSINGARGS RPACKFILE
         EXIT PLUS RKEYIDS COMPILE-LIB-FILE RECOMPILE-LIB-FILE-IF-NECESSARY
         GET-CURRENT-DIRECTORY TRIM-DIRECTORY-NAME
         |substitute|))

;;; Definitions for package BOOT of type SHADOW
(in-package "BOOT")
(shadow '(BOOT::MAP))

(import '(
   vmlisp::libstream-dirname
   vmlisp::make-input-filename
   vmlisp::is-console
   vmlisp::qsdifference
   vmlisp::qsminusp
   vmlisp::qsplus
   vmlisp::absval
   vmlisp::cgreaterp
   vmlisp::char2num
   vmlisp::charp
   vmlisp::concat
   vmlisp::copy
   vmlisp::difference
   vmlisp::digitp
   vmlisp::divide
   vmlisp::eqcar
   vmlisp::fixp
   vmlisp::greaterp
   vmlisp::hasheq
   vmlisp::hput
   vmlisp::hrem
   vmlisp::identp
   vmlisp::lessp
   vmlisp::ln
   vmlisp::make-cvec
   vmlisp::make-full-cvec
   vmlisp::make-vec
   vmlisp::memq
   vmlisp::movevec
   vmlisp::pname
   vmlisp::prettyprin0
   vmlisp::prettyprint
   vmlisp::printexp
   vmlisp::qassq
   vmlisp::qcar
   vmlisp::qcdr
   vmlisp::qcaar
   vmlisp::qcadr
   vmlisp::qcdar
   vmlisp::qcddr
   vmlisp::qcaaar
   vmlisp::qcaadr
   vmlisp::qcadar
   vmlisp::qcaddr
   vmlisp::qcdaar
   vmlisp::qcdadr
   vmlisp::qcddar
   vmlisp::qcddddr
   vmlisp::qcsize
   vmlisp::qenum
   vmlisp::qeset
   vmlisp::qlength
   vmlisp::qmemq
   vmlisp::qsadd1
   vmlisp::qslessp
   vmlisp::qsdec1
   vmlisp::qsetvelt
   vmlisp::qsgreaterp
   vmlisp::qsinc1
   vmlisp::qsmax
   vmlisp::qsmin
   vmlisp::qsoddp
   vmlisp::qsquotient
   vmlisp::qsremainder
   vmlisp::qssub1
   vmlisp::qstimes
   vmlisp::qszerop
   vmlisp::qvelt
   vmlisp::qvsize
   vmlisp::setandfileq
   vmlisp::sintp
   vmlisp::size
   vmlisp::stringimage
   vmlisp::strpos
   vmlisp::strposl
   vmlisp::substring
   vmlisp::ivecp
   vmlisp::rvecp
   vmlisp::dig2fix
   vmlisp::rnump
   vmlisp::fix
   vmlisp::sortgreaterp
   vmlisp::qsort
   vmlisp::sortby
   vmlisp::make-instream
   vmlisp::list2vec
   vmlisp::vec2list
   vmlisp::sub1
   vmlisp::add1
   vmlisp::neq
   vmlisp::hashtable-class
   vmlisp::maxindex
   vmlisp::upcase
   vmlisp::downcase
   vmlisp::vecp
   vmlisp::strconc
   vmlisp::shut
   vmlisp::prin2cvec
   vmlisp::ncons
   vmlisp::rplpair
   vmlisp::nump
   vmlisp::intp
   vmlisp::makeprop
   vmlisp::ifcar
   vmlisp::ifcdr
   vmlisp::quotient
   vmlisp::remainder
   vmlisp::make-hashtable
   vmlisp::hget
   vmlisp::hkeys
   vmlisp::$infilep
   vmlisp::$findfile
   vmlisp::pairp
   vmlisp::cvec
   vmlisp::uequal
   vmlisp::id
   vmlisp::vec-setelt
   vmlisp::make-bvec
   vmlisp::|startsId?|
   vmlisp::|idChar?|
   vmlisp::|mkOutputConsoleStream|
   vmlisp::|rMkIstream|
   vmlisp::|rMkOstream|
   vmlisp::NUM2USTR
))

(in-package "FOAM")

(export '(
|printDFloat| |printSFloat| |printBInt| |printSInt| |printString|
|printChar| |printNewLine| |MakeLit| |EnvNext| |EnvLevel| |MakeEnv|
|RElt| |RNew| |DDecl| |ClosFun| |ClosEnv| |CCall| |ArrToBInt|
|ArrToSInt| |ArrToDFlo| |ArrToSFlo| |BIntToDFlo| |BIntToSFlo|
|SIntToDFlo| |SIntToSFlo| |BIntToSInt| |SIntToBInt| |BitToSInt|
|ScanBInt| |ScanSInt| |ScanDFlo| |ScanSFlo| |FormatBInt| |FormatSInt|
|FormatDFlo| |FormatSFlo| |PtrEQ| |PtrIsNil| |PtrNil| |BIntBit|
|BIntShift| |BIntLength| |BIntPower| |BIntGcd| |BIntDivide| |BIntRem|
|BIntQuo| |BIntMod| |BIntTimes| |BIntMinus| |BIntPlus| |BIntInc|
|BIntDec| |BIntAbs| |BIntNegate| |BIntNE| |BIntGT| |BIntEQ| |BIntLE|
|BIntIsSmall| |BIntIsOdd| |BIntIsEven| |BIntIsPos| |BIntIsNeg|
|BIntIsZero| |BInt1| |BInt0| |SIntOr| |SIntAnd| |SIntNot| |SIntBit|
|SIntShift| |SIntLength| |SIntTimesMod| |SIntMinusMod| |SIntPlusMod|
|SIntPower| |SIntGcd| |SIntDivide| |SIntRem| |SIntQuo| |SIntMod|
|SIntTimes| |SIntMinus| |SIntPlus| |SIntInc| |SIntDec| |SIntNegate|
|SIntNE| |SIntGT| |SIntEQ| |SIntLE| |SIntIsOdd| |SIntIsEven|
|SIntIsPos| |SIntIsNeg| |SIntIsZero| |SIntMax| |SIntMin| |SInt1|
|SInt0| |HIntMax| |HIntMin| |HInt1| |HInt0| |ByteMax| |ByteMin|
|Byte1| |Byte0| |DFloCeiling| |DFloFloor| |DFloTruncate| |DFloRound|
|DFloIPower| |DFloPower| |DFloDivide| |DFloTimes| |DFloMinus|
|DFloPlus| |DFloNegate| OTHER-FORM |DFloNE| |DFloGT| |DFloEQ| |DFloLE|
|DFloIsPos| |DFloIsNeg| |DFloIsZero| |DFloEpsilon| |DFloMax| |DFloMin|
|DFlo1| |DFlo0| |SFloCeiling| |SFloFloor| |SFloTruncate| |SFloRound|
|SFloIPower| |SFloPower| |SFloDivide| |SFloTimes| |SFloMinus| |SFloPlus|
|SFloNegate| |SFloNE| |SFloGT| |SFloEQ| |SFloLE| |SFloIsPos| |SFloIsNeg|
|SFloIsZero| |SFloEpsilon| |SFloMax| |SFloMin| |SFlo1| |SFlo0|
|CharNum| |CharOrd| |CharUpper| |CharLower| |CharNE| |CharGT| |CharEQ|
|CharLE| |CharIsLetter| |CharIsDigit| |CharMax| |CharMin| |CharNewline|
|CharSpace| |BitNE| |BitEQ| |BitOr| |BitAnd| |BitNot| |BitTrue| |BitFalse|
|Clos| COMPILE-AS-FILE |FormatNumber| AXIOMXL-GLOBAL-NAME
AXIOMXL-FILE-INIT-NAME |BIntPrev| |BIntLT| |BIntIsSingle| |SIntShiftDn|
|SIntShiftUp| |SIntTimesPlus| |SIntNext| |SIntPrev| |SIntLT| |DFloAssemble|
|DFloDissemble| |DFloRDivide| |DFloRTimesPlus| |DFloRTimes| |DFloRMinus|
|DFloRPlus| |DFloTimesPlus| |DFloNext| |DFloPrev| |DFloLT| |SFloAssemble|
|fiStrHash| |SFloDissemble| |SFloRDivide| |SFloRTimesPlus| |SFloRTimes|
|fiGetDebugger| |SFloRMinus| |fiSetDebugger| |SFloRPlus| |SFloTimesPlus|
|fiGetDebugVar| |SFloNext| |fiSetDebugVar| |SFloPrev| |atan2| |SFloLT|
|atan| |acos| |CharLT| |asin| |BoolNE| |tanh| |BoolEQ| |cosh| |BoolOr|
|sinh| |BoolAnd| |tan| |BoolNot| |cos| |sin| |BoolTrue| |exp| |BoolFalse|
|log| |pow| |sqrt| |fputs| |fputc| |stderrFile| |stdoutFile| |stdinFile|
|SetProgHashCode| |ProgHashCode| |formatDFloat| |formatSFloat|
|formatBInt| |formatSInt| |strLength| |MakeLevel| |FoamEnvEnsure|
|SetEnvInfo| |EnvInfo| |SInt| TYPED-LET FILE-IMPORTS FILE-EXPORTS
DEFSPECIALS BLOCK-RETURN CASES IGNORE-VAR DEFPROG DECLARE-TYPE
DECLARE-PROG |FoamFree| |SetEElt| |SetAElt| |SetRElt| |SetLex| |Lex|
|AElt| |EElt| |ANew| |SetClosFun| |SetClosEnv| |Halt| |BoolToSInt|
|SIntToHInt| |SIntToByte| |ByteToSInt| |fputss| |fgetss| |PtrNE|
|PtrMagicEQ| |BIntShiftDn| |BIntShiftUp| |BIntBIPower| |BIntSIPower|
|BIntTimesPlus| |BIntNext|
))
