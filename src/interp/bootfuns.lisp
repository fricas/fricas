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


; NAME:    Boot Package
; PURPOSE: Provide forward references to Boot Code for functions to be at
;          defined at the boot level, but which must be accessible
;          not defined at lower levels.

(in-package "BOOT")

(defmacro def-boot-fun (f args where)
   `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,f ,args ,where (print (list ',f . ,args)))
     (export '(,f) "BOOT")))

(defmacro def-boot-var (p where)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,p nil ,where)
     (export '(,p) "BOOT")))

(defmacro def-boot-val (p val where)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,p ,val ,where)
     (export '(,p) "BOOT")))

(def-boot-val |$timerTicksPerSecond| INTERNAL-TIME-UNITS-PER-SECOND
    "for TEMPUS-FUGIT and $TOTAL-ELAPSED-TIME")
(def-boot-val $boxString
  (concatenate 'string (list (code-char #x1d) (code-char #xe2)))
  "this string of 2 chars displays as a box")
(def-boot-val |$quadSymbol| $boxString "displays an APL quad")
(def-boot-val $escapeString  (string (code-char 27))
   "string for single escape character")
(def-boot-val |$boldString| (concatenate 'string $escapeString "[12m")
  "switch into bold font")
(def-boot-val |$normalString| (concatenate 'string $escapeString "[0;10m")
  "switch back into normal font")
(def-boot-val $COMPILE t  "checked in COMP-2 to skip compilation")
(def-boot-fun |break| (msg)                         "Interpreter>Trace.boot")
(def-boot-fun |breaklet| (fn vars)                  "Interpreter>Trace.boot")
(def-boot-val |$BreakMode| '|query|                 "error.boot")


(def-boot-var |$compUniquelyIfTrue|                 "Compiler>Compiler.boot")
(def-boot-val |$currentLine|    ""          "current input line for history")

(def-boot-var |$exitMode|                           "???")
(def-boot-var |$exitModeStack|                      "???")

(def-boot-var |$fromSpadTrace|                      "Interpreter>Trace.boot")

(def-boot-val |$genSDVar| 0         "counter for genSomeVariable" )

(def-boot-var |$insideCapsuleFunctionIfTrue|        "???")
(def-boot-var |$insideCategoryIfTrue|               "???")
(def-boot-val |$insideCompTypeOf| NIL  "checked in comp3")
(def-boot-var |$insideExpressionIfTrue|             "???")
(def-boot-var |$insideFunctorIfTrue|                "???")
(def-boot-var |$insideWhereIfTrue|                  "???")

(def-boot-var |$leaveLevelStack|                    "???")
(def-boot-var |$libFile|                            "Compiler>LispLib.boot")
(def-boot-val $LISPLIB nil                  "whether to produce a lisplib or not")
(def-boot-var |$lisplibForm|                        "Compiler>LispLib.boot")
(def-boot-var |$lisplibKind|                        "Compiler>LispLib.boot")
(def-boot-var |$lisplibModemapAlist|                "Compiler>LispLib.boot")
(def-boot-var |$lisplibModemap|                     "Compiler>LispLib.boot")
(def-boot-var |$lisplibOperationAlist|              "Compiler>LispLib.boot")
(def-boot-var |$lisplibSignatureAlist|              "Compiler>LispLib.boot")

(def-boot-var |$mapSubNameAlist|                    "Interpreter>Trace.boot")
(def-boot-var |$mathTrace|                          "Interpreter>Trace.boot")
(def-boot-var |$mathTraceList|              "Controls mathprint output for )trace.")

(def-boot-val $num_of_meta_errors 0                 "Number of errors seen so far")
(def-boot-val |$oldTime| 0                          "???")

(def-boot-var |$postStack|                          "???")
(def-boot-val |$PrettyPrint| nil "if t generated code is prettyprinted")
(def-boot-var |$previousTime|                       "???")
(def-boot-val |$optimizableDomainNames|
      '(|FactoredForm| |List| |Vector|
        |Integer| |NonNegativeInteger| |PositiveInteger|
        |SmallInteger| |String| |Boolean| |Symbol| |BooleanFunctions|)
   "used in optCall to decide which domains can be optimized")
(def-boot-val |$printLoadMsgs|  '|off|          "Interpreter>SetVarT.boot")
(def-boot-var |$PrintOnly|                          "Compiler>LispLib.boot")
(def-boot-var |$reportBottomUpFlag|                 "Interpreter>SetVarT.boot")
(def-boot-var |$reportFlag|                         "Interpreter>SetVars.boot")
(def-boot-var |$returnMode|                         "???")
(def-boot-var |$semanticErrorStack|                 "???")
(def-boot-val |$SetFunctions| nil  "checked in SetFunctionSlots")

(def-boot-val $SPAD nil                             "Is this Spad code?")

(def-boot-val |$timerOn| t                          "???")
(def-boot-var |$topOp|                             "See displayPreCompilationErrors")
(def-boot-val |$traceDomains| t                      "enables domain tracing")
(def-boot-val |$TraceFlag| t                        "???")
(def-boot-var |$tracedSpadModemap|                  "Interpreter>Trace.boot")
(def-boot-var |$traceletFunctions|                  "???")
(def-boot-var |$traceNoisely|                       "Interpreter>Trace.boot")
(def-boot-var |$TranslateOnly|                      "???")

(def-boot-fun BUMPCOMPERRORCOUNT ()                 "errorSupervisor1")
(def-boot-var |$warningStack|                       "???")
(def-boot-val |$whereList| () "referenced in format boot formDecl2String")

(def-boot-val |$inputPromptType| '|step|  "checked in MKPROMPT")
(def-boot-val |$IOindex| 0                 "step counter")

