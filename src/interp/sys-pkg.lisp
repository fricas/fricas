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
(make-package "BOOTTRAN" :use '("FRICAS-LISP"))

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

(import
    '(VMLISP::NE VMLISP::FLUID
         VMLISP::OBEY VMLISP::|union|
         VMLISP::OPTIONLIST VMLISP::EXIT
         VMLISP::*INDEX-FILENAME*))

;;; Definitions for package VMLISP of type EXPORT
(in-package "VMLISP")
(import '(
          BOOT::|directoryp| BOOT::|makedir|))

(export
    '(VMLISP::SINTP VMLISP::$FCOPY 
         VMLISP::PUT
         VMLISP::QVELT-1 VMLISP::QSETVELT-1 vmlisp::throw-protect
         VMLISP::|directoryp| VMLISP::|makedir| VMLISP::EQCAR
         VMLISP::DEFIOSTREAM VMLISP::RDEFIOSTREAM VMLISP::MLAMBDA
         VMLISP::QSLESSP VMLISP::QSDIFFERENCE VMLISP::QSQUOTIENT
         VMLISP::CREATE-SBC VMLISP::LASTPAIR
         VMLISP::EQSUBSTLIST VMLISP::QCAAAR VMLISP::$TOTAL-ELAPSED-TIME
         VMLISP::QUOTIENT VMLISP::SORTGREATERP
         VMLISP::QSETREFV VMLISP::QSTRINGLENGTH VMLISP::EVALFUN
         VMLISP::QCDAR VMLISP::TEMPUS-FUGIT VMLISP::QSPLUS VMLISP::QSABSVAL
         VMLISP::QSZEROP VMLISP::QSMIN VMLISP::QSLEFTSHIFT
         VMLISP::SETDIFFERENCE VMLISP::RPLQ VMLISP::CATCHALL
         VMLISP::RECOMPILE-DIRECTORY VMLISP::MDEF VMLISP::LINTP
         VMLISP::NILFN VMLISP::TAB VMLISP::QCDDR VMLISP::IOSTATE
         VMLISP::SFP VMLISP::NE VMLISP::STRGREATERP
         VMLISP::USE-VMLISP-SYNTAX VMLISP::RCLASS VMLISP::RSETCLASS
         VMLISP::SEQ VMLISP::FIXP VMLISP::MAKE-CVEC
         VMLISP::|F,PRINT-ONE| VMLISP::HASHUEQUAL VMLISP::$OUTFILEP
         VMLISP::TIMES VMLISP::DIFFERENCE VMLISP::MSUBST VMLISP::DIVIDE
         VMLISP::|remove| VMLISP::GETL VMLISP::QCADAR VMLISP::QCAAAAR
         VMLISP::RECLAIM VMLISP::ORADDTEMPDEFS VMLISP::NAMEDERRSET
         VMLISP::TRIMSTRING VMLISP::CURRINDEX VMLISP::EVALANDFILEACTQ
         VMLISP::LISPLIB VMLISP::FLUID VMLISP::MDEFX VMLISP::COMP370
         VMLISP::NEQ VMLISP::GETREFV VMLISP::|log| VMLISP::QVSIZE
         VMLISP::MBPIP VMLISP::RPLNODE VMLISP::QSORT
         VMLISP::PLACEP VMLISP::RREAD VMLISP::BINTP VMLISP::QSODDP
         VMLISP::O VMLISP::RVECP VMLISP::CHAR2NUM VMLISP::POPP
         VMLISP::QCDAADR VMLISP::HKEYS VMLISP::HASHCVEC VMLISP::HASHID
         VMLISP::REMOVEQ VMLISP::LISTOFFUNCTIONS
         VMLISP::QCADAAR VMLISP::ABSVAL VMLISP::VMPRINT
         VMLISP::MAKE-APPENDSTREAM
         VMLISP::MAKE-INSTREAM VMLISP::HASHTABLEP VMLISP::UPCASE
         VMLISP::LOADCOND VMLISP::STRPOSL VMLISP::STATEP VMLISP::QCDADR
         VMLISP::HREMPROP VMLISP::LAM VMLISP::FBPIP VMLISP::NCONC2
         VMLISP::GETFULLSTR VMLISP::I VMLISP::HREM
         VMLISP::*LISP-BIN-FILETYPE* VMLISP::INT2RNUM VMLISP::EBCDIC
         VMLISP::$INFILEP VMLISP::BFP VMLISP::NUMP VMLISP::UNEMBED
         VMLISP::PAIRP VMLISP::BOOLEANP VMLISP::FIX VMLISP::REMAINDER
         VMLISP::RE-ENABLE-INT VMLISP::QCAADDR VMLISP::QCDDADR
         VMLISP::$LISTFILE VMLISP::IVECP VMLISP::LIST2VEC
         VMLISP::|LAM,FILEACTQ| VMLISP::LISTOFQUOTES
         VMLISP::$ERASE VMLISP::QSDEC1
         VMLISP::QSSUB1 VMLISP::QCAR VMLISP::EVA1FUN VMLISP::IS-CONSOLE
         VMLISP::MAKESTRING VMLISP::CUROUTSTREAM VMLISP::QCDDDR
         VMLISP::QCDADAR VMLISP::MAKE-ABSOLUTE-FILENAME VMLISP::SUFFIX
         VMLISP::FUNARGP VMLISP::VM/ VMLISP::QRPLACA VMLISP::GGREATERP
         VMLISP::CGREATERP VMLISP::RNUMP VMLISP::RESETQ VMLISP::QRPLACD
         VMLISP::SORTBY VMLISP::CVECP VMLISP::SETELT VMLISP::HGET
         VMLISP::$DIRECTORY-LIST VMLISP::LN
         VMLISP::|member| VMLISP::AXIOM-MEMBER
         VMLISP::$LIBRARY-DIRECTORY-LIST
         VMLISP::QCSIZE VMLISP::QCADDDR VMLISP::RWRITE VMLISP::SUBLOAD
         VMLISP::STRINGIMAGE VMLISP::$CLEAR VMLISP::|read-line|
         VMLISP::PROPLIST VMLISP::INTP VMLISP::OUTPUT VMLISP::CONSOLE
         VMLISP::QCDDDAR VMLISP::ADDOPTIONS VMLISP::$FILETYPE-TABLE
         VMLISP::QSMINUSP VMLISP::|assoc| VMLISP::SETSIZE VMLISP::QCDR
         VMLISP::EFFACE VMLISP::COPY VMLISP::DOWNCASE VMLISP::LC2UC
         VMLISP::EMBED VMLISP::SETANDFILEQ VMLISP::QSMAX
         VMLISP::LIST2REFVEC VMLISP::MACRO-INVALIDARGS VMLISP::EMBEDDED
         VMLISP::REFVECP VMLISP::CLOSEDFN VMLISP::MAKE-HASHTABLE
         VMLISP::MAKE-FILENAME VMLISP::|$defaultMsgDatabaseName|
         VMLISP::LEXGREATERP
         VMLISP::IDENTP VMLISP::QSINC1 VMLISP::QESET VMLISP::MRP
         VMLISP::LESSP VMLISP::RPLPAIR VMLISP::QVELT VMLISP::QRPLQ
         VMLISP::MACERR VMLISP::*FILEACTQ-APPLY* VMLISP::HPUT*
         VMLISP::$FILEP VMLISP::MAKE-FULL-CVEC VMLISP::HCLEAR
         VMLISP::HPUTPROP 
         VMLISP::STRING2ID-N VMLISP::CALLBELOW VMLISP::BPINAME
         VMLISP::CHANGELENGTH VMLISP::ECQ VMLISP::OBEY VMLISP::QASSQ
         VMLISP::DCQ VMLISP::SHUT VMLISP::FILE VMLISP::HPUT
         VMLISP::MAKEPROP VMLISP::GREATERP
         VMLISP::REROOT VMLISP::DIG2FIX VMLISP::L-CASE
         VMLISP::TEREAD VMLISP::QSREMAINDER VMLISP::$FINDFILE
         VMLISP::EQQ VMLISP::PRETTYPRINT VMLISP::HASHEQ VMLISP::LOG2
         VMLISP::U-CASE VMLISP::NREMOVE VMLISP::QREFELT VMLISP::SIZE
         VMLISP::EOFP VMLISP::QCDAAR VMLISP::RSHUT VMLISP::ADD1
         VMLISP::QMEMQ VMLISP::SUBSTRING VMLISP::LOADVOL
         VMLISP::QSTIMES VMLISP::STRINGLENGTH VMLISP::NEXT
         VMLISP::DEVICE VMLISP::MAPELT VMLISP::LENGTHOFBPI
         VMLISP::DIGITP VMLISP::QLENGTH VMLISP::QCAAADR VMLISP::CVEC
         VMLISP::VEC2LIST VMLISP::MODE VMLISP::MAKE-VEC VMLISP::GCMSG
         VMLISP::CONCAT VMLISP::$SHOWLINE VMLISP::QCAADR VMLISP::QCDDAR
         VMLISP::QCDAAAR VMLISP::RDROPITEMS VMLISP::VECP
         VMLISP::|union| VMLISP::ONE-OF VMLISP::NULLOUTSTREAM
         VMLISP::QSGREATERP VMLISP::MINUS VMLISP::MAXINDEX
         VMLISP::GETSTR VMLISP::QCADADR VMLISP::PRIN2CVEC
         VMLISP::CURRENTTIME VMLISP::$REPLACE VMLISP::UNIONQ
         VMLISP::NREMOVEQ VMLISP::CURINSTREAM VMLISP::MAKE-OUTSTREAM
         VMLISP::APPLX VMLISP::LASTNODE VMLISP::SUBSTQ VMLISP::TRUEFN
         VMLISP::|last| VMLISP::RPLACSTR VMLISP::SETQP VMLISP::QCADDR
         VMLISP::QCAADAR VMLISP::QCDDAAR VMLISP::|intersection|
         VMLISP::HASHTABLE-CLASS
         VMLISP::*COMP370-APPLY* VMLISP::QSETVELT VMLISP::MOVEVEC
         VMLISP::ID VMLISP::DEFINE-FUNCTION VMLISP::MSUBSTQ VMLISP::|nsubst|
         VMLISP::LISTOFFLUIDS VMLISP::SUB1 VMLISP::NUMBEROFARGS
         VMLISP::VMREAD VMLISP::SMINTP VMLISP::$SCREENSIZE
         VMLISP::LISTOFFREES VMLISP::QCDADDR VMLISP::COMPRREAD
         VMLISP::GENSYMP VMLISP::IFCAR VMLISP::QSETQ
         VMLISP::QCADDAR VMLISP::*LISP-SOURCE-FILETYPE* VMLISP::KOMPILE
         VMLISP::INPUT VMLISP::PAPPP VMLISP::UEQUAL VMLISP::COMPRWRITE
         VMLISP::SUBRP VMLISP::ASSEMBLE VMLISP::|LAM,EVALANDFILEACTQ|
         VMLISP::|$msgDatabaseName| VMLISP::IFCDR VMLISP::QVMAXINDEX
         VMLISP::$SPADROOT VMLISP::PRIN0 VMLISP::PRETTYPRIN0
         VMLISP::STACKLIFO VMLISP::ASSQ VMLISP::PRINTEXP
         VMLISP::QCDDDDR VMLISP::QSADD1
         VMLISP::SETDIFFERENCEQ VMLISP::STRPOS VMLISP::CONSTANT
         VMLISP::QCAAR VMLISP::HCOUNT VMLISP::RCOPYITEMS
         VMLISP::QSMINUS VMLISP::EVA1 VMLISP::OPTIONLIST
         VMLISP::NUM2CHAR VMLISP::QENUM VMLISP::QEQQ
         VMLISP::$TOTAL-GC-TIME VMLISP::CHARP VMLISP::QCADR
         VMLISP::INTERSECTIONQ VMLISP::DSETQ VMLISP::FETCHCHAR
         VMLISP::STRCONC VMLISP::MACRO-MISSINGARGS VMLISP::RPACKFILE
         VMLISP::EXIT VMLISP::PLUS VMLISP::RKEYIDS
         VMLISP::COMPILE-LIB-FILE VMLISP::RECOMPILE-LIB-FILE-IF-NECESSARY
         VMLISP::GET-CURRENT-DIRECTORY VMLISP::TRIM-DIRECTORY-NAME))

;;; Definitions for package BOOT of type SHADOW
(in-package "BOOT")
(shadow '(BOOT::MAP))
(import
    '(VMLISP::NE VMLISP::FLUID
         VMLISP::OBEY VMLISP::|union|
         VMLISP::OPTIONLIST VMLISP::EXIT VMLISP::LEXGREATERP))
(import '(vmlisp::make-input-filename))
(import '(vmlisp::libstream-dirname))
(import '(vmlisp::eqcar))

;;; Definitions for package VMLISP of type SHADOW

(in-package "BOOT") ;; Used to be "UNCOMMON"

(in-package "BOOT")
(import '(
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
   vmlisp::qcdddr
   vmlisp::qcaaaar
   vmlisp::qcaaadr
   vmlisp::qcaadar
   vmlisp::qcaaddr
   vmlisp::qcadaar
   vmlisp::qcadadr
   vmlisp::qcaddar
   vmlisp::qcadddr
   vmlisp::qcdaaar
   vmlisp::qcdaadr
   vmlisp::qcdadar
   vmlisp::qcdaddr
   vmlisp::qcddaar
   vmlisp::qcddadr
   vmlisp::qcdddar
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
   vmlisp::defiostream
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
   vmlisp::|shoeread-line|
   vmlisp::|shoeInputFile|
   vmlisp::|shoeConsole|
   vmlisp::|startsId?|
   vmlisp::|idChar?|
   vmlisp::|npPC|
   vmlisp::|npPP|
))

(in-package "FOAM")

(export '(
FOAM::|printDFloat|
FOAM::|printSFloat|
FOAM::|printBInt|
FOAM::|printSInt|
FOAM::|printString|
FOAM::|printChar|
FOAM::|printNewLine|
FOAM::|MakeLit|
FOAM::|EnvNext|
FOAM::|EnvLevel|
FOAM::|MakeEnv|
FOAM::|RElt|
FOAM::|RNew|
FOAM::|DDecl|
FOAM::|ClosFun|
FOAM::|ClosEnv|
FOAM::|CCall|
FOAM::|ArrToBInt|
FOAM::|ArrToSInt|
FOAM::|ArrToDFlo|
FOAM::|ArrToSFlo|
FOAM::|BIntToDFlo|
FOAM::|BIntToSFlo|
FOAM::|SIntToDFlo|
FOAM::|SIntToSFlo|
FOAM::|BIntToSInt|
FOAM::|SIntToBInt|
FOAM::|BitToSInt|
FOAM::|ScanBInt|
FOAM::|ScanSInt|
FOAM::|ScanDFlo|
FOAM::|ScanSFlo|
FOAM::|FormatBInt|
FOAM::|FormatSInt|
FOAM::|FormatDFlo|
FOAM::|FormatSFlo|
FOAM::|PtrEQ|
FOAM::|PtrIsNil|
FOAM::|PtrNil|
FOAM::|BIntBit|
FOAM::|BIntShift|
FOAM::|BIntLength|
FOAM::|BIntPower|
FOAM::|BIntGcd|
FOAM::|BIntDivide|
FOAM::|BIntRem|
FOAM::|BIntQuo|
FOAM::|BIntMod|
FOAM::|BIntTimes|
FOAM::|BIntMinus|
FOAM::|BIntPlus|
FOAM::|BIntInc|
FOAM::|BIntDec|
FOAM::|BIntAbs|
FOAM::|BIntNegate|
FOAM::|BIntNE|
FOAM::|BIntGT|
FOAM::|BIntEQ|
FOAM::|BIntLE|
FOAM::|BIntIsSmall|
FOAM::|BIntIsOdd|
FOAM::|BIntIsEven|
FOAM::|BIntIsPos|
FOAM::|BIntIsNeg|
FOAM::|BIntIsZero|
FOAM::|BInt1|
FOAM::|BInt0|
FOAM::|SIntOr|
FOAM::|SIntAnd|
FOAM::|SIntNot|
FOAM::|SIntBit|
FOAM::|SIntShift|
FOAM::|SIntLength|
FOAM::|SIntTimesMod|
FOAM::|SIntMinusMod|
FOAM::|SIntPlusMod|
FOAM::|SIntPower|
FOAM::|SIntGcd|
FOAM::|SIntDivide|
FOAM::|SIntRem|
FOAM::|SIntQuo|
FOAM::|SIntMod|
FOAM::|SIntTimes|
FOAM::|SIntMinus|
FOAM::|SIntPlus|
FOAM::|SIntInc|
FOAM::|SIntDec|
FOAM::|SIntNegate|
FOAM::|SIntNE|
FOAM::|SIntGT|
FOAM::|SIntEQ|
FOAM::|SIntLE|
FOAM::|SIntIsOdd|
FOAM::|SIntIsEven|
FOAM::|SIntIsPos|
FOAM::|SIntIsNeg|
FOAM::|SIntIsZero|
FOAM::|SIntMax|
FOAM::|SIntMin|
FOAM::|SInt1|
FOAM::|SInt0|
FOAM::|HIntMax|
FOAM::|HIntMin|
FOAM::|HInt1|
FOAM::|HInt0|
FOAM::|ByteMax|
FOAM::|ByteMin|
FOAM::|Byte1|
FOAM::|Byte0|
FOAM::|DFloCeiling|
FOAM::|DFloFloor|
FOAM::|DFloTruncate|
FOAM::|DFloRound|
FOAM::|DFloIPower|
FOAM::|DFloPower|
FOAM::|DFloDivide|
FOAM::|DFloTimes|
FOAM::|DFloMinus|
FOAM::|DFloPlus|
FOAM::|DFloNegate|
FOAM::OTHER-FORM
FOAM::|DFloNE|
FOAM::|DFloGT|
FOAM::|DFloEQ|
FOAM::|DFloLE|
FOAM::|DFloIsPos|
FOAM::|DFloIsNeg|
FOAM::|DFloIsZero|
FOAM::|DFloEpsilon|
FOAM::|DFloMax|
FOAM::|DFloMin|
FOAM::|DFlo1|
FOAM::|DFlo0|
FOAM::|SFloCeiling|
FOAM::|SFloFloor|
FOAM::|SFloTruncate|
FOAM::|SFloRound|
FOAM::|SFloIPower|
FOAM::|SFloPower|
FOAM::|SFloDivide|
FOAM::|SFloTimes|
FOAM::|SFloMinus|
FOAM::|SFloPlus|
FOAM::|SFloNegate|
FOAM::|SFloNE|
FOAM::|SFloGT|
FOAM::|SFloEQ|
FOAM::|SFloLE|
FOAM::|SFloIsPos|
FOAM::|SFloIsNeg|
FOAM::|SFloIsZero|
FOAM::|SFloEpsilon|
FOAM::|SFloMax|
FOAM::|SFloMin|
FOAM::|SFlo1|
FOAM::|SFlo0|
FOAM::|CharNum|
FOAM::|CharOrd|
FOAM::|CharUpper|
FOAM::|CharLower|
FOAM::|CharNE|
FOAM::|CharGT|
FOAM::|CharEQ|
FOAM::|CharLE|
FOAM::|CharIsLetter|
FOAM::|CharIsDigit|
FOAM::|CharMax|
FOAM::|CharMin|
FOAM::|CharNewline|
FOAM::|CharSpace|
FOAM::|BitNE|
FOAM::|BitEQ|
FOAM::|BitOr|
FOAM::|BitAnd|
FOAM::|BitNot|
FOAM::|BitTrue|
FOAM::|BitFalse|
FOAM::|Clos|
FOAM::COMPILE-AS-FILE
FOAM::|FormatNumber|
FOAM::AXIOMXL-GLOBAL-NAME
FOAM::AXIOMXL-FILE-INIT-NAME
FOAM::|BIntPrev|
FOAM::|BIntLT|
FOAM::|BIntIsSingle|
FOAM::|SIntShiftDn|
FOAM::|SIntShiftUp|
FOAM::|SIntTimesPlus|
FOAM::|SIntNext|
FOAM::|SIntPrev|
FOAM::|SIntLT|
FOAM::|DFloAssemble|
FOAM::|DFloDissemble|
FOAM::|DFloRDivide|
FOAM::|DFloRTimesPlus|
FOAM::|DFloRTimes|
FOAM::|DFloRMinus|
FOAM::|DFloRPlus|
FOAM::|DFloTimesPlus|
FOAM::|DFloNext|
FOAM::|DFloPrev|
FOAM::|DFloLT|
FOAM::|SFloAssemble|
FOAM::|fiStrHash|
FOAM::|SFloDissemble|
FOAM::|SFloRDivide|
FOAM::|SFloRTimesPlus|
FOAM::|SFloRTimes|
FOAM::|fiGetDebugger|
FOAM::|SFloRMinus|
FOAM::|fiSetDebugger|
FOAM::|SFloRPlus|
FOAM::|SFloTimesPlus|
FOAM::|fiGetDebugVar|
FOAM::|SFloNext|
FOAM::|fiSetDebugVar|
FOAM::|SFloPrev|
FOAM::|atan2|
FOAM::|SFloLT|
FOAM::|atan|
FOAM::|acos|
FOAM::|CharLT|
FOAM::|asin|
FOAM::|BoolNE|
FOAM::|tanh|
FOAM::|BoolEQ|
FOAM::|cosh|
FOAM::|BoolOr|
FOAM::|sinh|
FOAM::|BoolAnd|
FOAM::|tan|
FOAM::|BoolNot|
FOAM::|cos|
FOAM::|sin|
FOAM::|BoolTrue|
FOAM::|exp|
FOAM::|BoolFalse|
FOAM::|log|
FOAM::|pow|
FOAM::|sqrt|
FOAM::|fputs|
FOAM::|fputc|
FOAM::|stderrFile|
FOAM::|stdoutFile|
FOAM::|stdinFile|
FOAM::|SetProgHashCode|
FOAM::|ProgHashCode|
FOAM::|formatDFloat|
FOAM::|formatSFloat|
FOAM::|formatBInt|
FOAM::|formatSInt|
FOAM::|strLength|
FOAM::|MakeLevel|
FOAM::|FoamEnvEnsure|
FOAM::|SetEnvInfo|
FOAM::|EnvInfo|
FOAM::|SInt|
FOAM::TYPED-LET
FOAM::FILE-IMPORTS
FOAM::FILE-EXPORTS
FOAM::DEFSPECIALS
FOAM::BLOCK-RETURN
FOAM::CASES
FOAM::IGNORE-VAR
FOAM::DEFPROG
FOAM::DECLARE-TYPE
FOAM::DECLARE-PROG
FOAM::|FoamFree|
FOAM::|SetEElt|
FOAM::|SetAElt|
FOAM::|SetRElt|
FOAM::|SetLex|
FOAM::|Lex|
FOAM::|AElt|
FOAM::|EElt|
FOAM::|ANew|
FOAM::|SetClosFun|
FOAM::|SetClosEnv|
FOAM::|Halt|
FOAM::|BoolToSInt|
FOAM::|SIntToHInt|
FOAM::|SIntToByte|
FOAM::|ByteToSInt|
FOAM::|fputss|
FOAM::|fgetss|
FOAM::|PtrNE|
FOAM::|PtrMagicEQ|
FOAM::|BIntShiftDn|
FOAM::|BIntShiftUp|
FOAM::|BIntBIPower|
FOAM::|BIntSIPower|
FOAM::|BIntTimesPlus|
FOAM::|BIntNext|
))
