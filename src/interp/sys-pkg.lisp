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
