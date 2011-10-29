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

;;;  PURPOSE:
;;;    Provide generally useful macros and functions for MetaLanguage
;;;    and Boot code.  Contents are organized along Common Lisp datatype
;;;    lines, with sections numbered to match the section headings of the
;;;    Common Lisp Reference Manual, by Guy Steele, Digital Press, 1984,
;;;    Digital Press Order Number EY-00031-DP.  This way you can
;;;    look up the corresponding section in the manual and see if
;;;    there isn't a cleaner and non-VM-specific way of doing things.

(provide 'Boot)

(in-package "BOOT")

; moved from bootfuns.lisp

;          Provide forward references to Boot Code for functions to be at
;          defined at the boot level, but which must be accessible
;          not defined at lower levels.

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
(def-boot-val |$BreakMode| '|query|                 "error.boot")


(def-boot-var |$compUniquelyIfTrue|                 "Compiler>Compiler.boot")
(def-boot-val |$currentLine|    ""          "current input line for history")

(def-boot-var |$exitMode|                           "???")
(def-boot-var |$exitModeStack|                      "???")

(def-boot-var |$fromSpadTrace|                      "Interpreter>Trace.boot")

(def-boot-val |$genSDVar| 0         "counter for genSomeVariable" )

(def-boot-var |$insideCapsuleFunctionIfTrue|        "???")
(def-boot-var |$insideCategoryIfTrue|               "???")
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

(def-boot-var |$mapSubNameAlist|                    "Interpreter>Trace.boot")
(def-boot-var |$mathTrace|                          "Interpreter>Trace.boot")
(def-boot-var |$mathTraceList|              "Controls mathprint output for )trace.")

(def-boot-val |$oldTime| 0                          "???")

(def-boot-var |$postStack|                          "???")
(def-boot-var |$previousTime|                       "???")
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

(def-boot-var |$warningStack|                       "???")
(def-boot-val |$whereList| () "referenced in format boot formDecl2String")

(def-boot-val |$inputPromptType| '|step|  "checked in MKPROMPT")
(def-boot-val |$IOindex| 0                 "step counter")

; End of moved fragment


(defvar |$compilingMap| ())
(defvar |$definingMap| nil)

(defmacro KAR (ARG) `(ifcar ,arg))
(defmacro KDR (ARG) `(ifcdr ,arg))
(defmacro KADR (ARG) `(ifcar (ifcdr ,arg)))
(defmacro KADDR (ARG) `(ifcar (ifcdr (ifcdr ,arg))))

; 5 PROGRAM STRUCTURE

; 5.3 Top-Level Forms


(defun SETANDFILE (x y) (LAM\,EVALANDFILEACTQ `(defparameter ,x ',y)))

(defun LAM\,EVALANDFILEACTQ (name &optional (form name))
    (|outputLispForm| name form) (eval form))

(defun |outputLispForm| (name form)
       (if *FILEACTQ-APPLY* (FUNCALL *FILEACTQ-APPLY* name form)))

; 5.3.2 Declaring Global Variables and Named Constants

(defmacro |function| (name) `(FUNCTION ,name))
(defmacro |dispatchFunction| (name) `(FUNCTION ,name))

(defun |functionp| (fn)
   (if (identp fn) (and (fboundp fn) (not (macro-function fn))) (functionp fn)))
(defun |macrop| (fn) (and (identp fn) (macro-function fn)))

; 6 PREDICATES

; Ordering

(DEFUN ?ORDER (U V)
  "Multiple-type ordering relation."
;;; Result negated compared to LEXGREATERP and GGREATERP
;;;   Order of types: nil number symbol string vector cons"
  (COND ((NULL U))
        ((NULL V) NIL)
        ((ATOM U)
         (if (ATOM V)
             (COND ((NUMBERP U) (if (NUMBERP V) (> V U) T))
                   ((NUMBERP V) NIL)
                   ((IDENTP U) (AND (IDENTP V) (string> (SYMBOL-NAME V) (SYMBOL-NAME U))))
                   ((IDENTP V) NIL)
                   ((STRINGP U) (AND (STRINGP V) (string> V U)))
                   ((STRINGP V) NIL)
                   ((AND (VECP U) (VECP V))
                    (AND (> (SIZE V) (SIZE U))
                         (DO ((I 0 (1+ I)))
                             ((> I (MAXINDEX U)) 'T)
                           (COND ((NOT (EQUAL (ELT U I) (ELT V I)))
                                  (RETURN (?ORDER (ELT U I) (ELT V I))))))))
                   ((croak "Do not understand")))
               T))
        ((ATOM V) NIL)
        ((EQUAL U V))
        ((EQUAL (CAR U) (CAR V))
           (?ORDER (CDR U) (CDR V)))
        ((?ORDER (CAR U) (CAR V)))
))

(DEFUN LEXGREATERP (COMPERAND-1 COMPERAND-2)
    ;;  "Order of types: pair NIL vec string symbol num fbpi other"
    (COND
      ((EQ COMPERAND-1 COMPERAND-2) NIL)
      ((consp COMPERAND-1)
        (COND
          ( (consp COMPERAND-2)
            (COND
              ( (EQUAL (qcar COMPERAND-1) (qcar COMPERAND-2))
                (LEXGREATERP (qcdr COMPERAND-1) (qcdr COMPERAND-2)) )
              ( (LEXGREATERP (qcar COMPERAND-1) (qcar COMPERAND-2)) ) ) )
          ('else t)))
      ((consp COMPERAND-2) NIL)
      ((NULL COMPERAND-1) 'T )
      ((NULL COMPERAND-2) NIL)
      ((VECP COMPERAND-1)
        (COND
          ((VECP COMPERAND-2) (LEXVGREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((VECP COMPERAND-2) NIL)
      ((stringp COMPERAND-1)
        (COND
          ((stringp COMPERAND-2)
            (STRING-GREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((stringp COMPERAND-2) NIL)
      ((symbolp COMPERAND-1)
        (COND
          ((symbolp COMPERAND-2)
            (STRING-GREATERP (symbol-name COMPERAND-1) (symbol-name COMPERAND-2)) )
          ('else t)))
      ((symbolp COMPERAND-2) NIL )
      ((numberp COMPERAND-1)
        (COND
          ( (numberp COMPERAND-2)
            (> COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((numberp COMPERAND-2) NIL)
      ((CHARACTERP COMPERAND-1)
        (COND
          ((CHARACTERP COMPERAND-2)
            (CHAR-GREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((CHARACTERP COMPERAND-2) NIL )
      ((FBPIP COMPERAND-1)
        (COND
          ((FBPIP COMPERAND-2)
            (LEXGREATERP (BPINAME COMPERAND-1) (BPINAME COMPERAND-2)) )
          ('else t)))
      ((FBPIP COMPERAND-2) NIL)
      ((> (SXHASH COMPERAND-1) (SXHASH COMPERAND-2)))))

(DEFUN LEXVGREATERP (VECTOR-COMPERAND-1 VECTOR-COMPERAND-2)
  (declare (simple-vector vector-comperand-1 vector-comperand-2))
    (PROG ((L1 (length VECTOR-COMPERAND-1))
           (L2 (length VECTOR-COMPERAND-2))
           (I -1)
           T1 T2)
     (declare (fixnum i l1 l2) )
  LP  (setq i (1+ i))
      (COND
        ((EQL L1 I) (RETURN NIL))
        ((EQL L2 I) (RETURN 'T)))
      (COND
        ((EQUAL
            (SETQ T1 (svref VECTOR-COMPERAND-1 I))
            (SETQ T2 (svref VECTOR-COMPERAND-2 I)))
          (GO LP)))
      (RETURN (LEXGREATERP T1 T2)) ) )


(DEFUN GGREATERP (COMPERAND-1 COMPERAND-2)
    ;;  "Order of types: symbol pair NIL vec string num fbpi other"
    (COND
      ((EQ COMPERAND-1 COMPERAND-2) NIL)
      ((symbolp COMPERAND-1)
        (COND
          ((symbolp COMPERAND-2)
            (CGREATERP (symbol-name COMPERAND-1) (symbol-name COMPERAND-2)) )
          ('else t)))
      ((symbolp COMPERAND-2) NIL )
      ((consp COMPERAND-1)
        (COND
          ( (consp COMPERAND-2)
            (COND
              ( (EQUAL (qcar COMPERAND-1) (qcar COMPERAND-2))
                (GGREATERP (qcdr COMPERAND-1) (qcdr COMPERAND-2)) )
              ( (GGREATERP (qcar COMPERAND-1) (qcar COMPERAND-2)) ) ) )
          ('else t)))
      ((consp COMPERAND-2) NIL)
      ((NULL COMPERAND-1) 'T )
      ((NULL COMPERAND-2) NIL)
      ((VECP COMPERAND-1)
        (COND
          ((VECP COMPERAND-2) (VGREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((VECP COMPERAND-2) NIL)
      ((stringp COMPERAND-1)
        (COND
          ((stringp COMPERAND-2)
            (CGREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((stringp COMPERAND-2) NIL)
      ((numberp COMPERAND-1)
        (COND
          ( (numberp COMPERAND-2)
            (> COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((numberp COMPERAND-2) NIL)
      ((CHARACTERP COMPERAND-1)
        (COND
          ((CHARACTERP COMPERAND-2)
            (CHAR> COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((CHARACTERP COMPERAND-2) NIL )
      ((FBPIP COMPERAND-1)
        (COND
          ((FBPIP COMPERAND-2)
            (GGREATERP (BPINAME COMPERAND-1) (BPINAME COMPERAND-2)) )
          ('else t)))
      ((FBPIP COMPERAND-2) NIL)
      ((> (SXHASH COMPERAND-1) (SXHASH COMPERAND-2)))))

(DEFUN VGREATERP (VECTOR-COMPERAND-1 VECTOR-COMPERAND-2)
  (declare (simple-vector vector-comperand-1 vector-comperand-2))
    (PROG ((L1 (length VECTOR-COMPERAND-1))
           (L2 (length VECTOR-COMPERAND-2))
           (I -1)
           T1 T2)
     (declare (fixnum i l1 l2) )
  LP  (setq i (1+ i))
      (COND
        ((EQL L1 I) (RETURN NIL))
        ((EQL L2 I) (RETURN 'T)))
      (COND
        ((EQUAL
            (SETQ T1 (svref VECTOR-COMPERAND-1 I))
            (SETQ T2 (svref VECTOR-COMPERAND-2 I)))
          (GO LP)))
      (RETURN (GGREATERP T1 T2)) ) )

(defvar SORTGREATERP #'GGREATERP "default sorting predicate")

(defun GLESSEQP (X Y) (NOT (GGREATERP X Y)))

(defun LEXLESSEQP (X Y) (NOT (LEXGREATERP X Y)))

; 6.3 Equality Predicates

;;; -----------------------

;; qeqcar should be used when you know the first arg is a pair
;; the second arg should either be a literal fixnum or a symbol
;; the car of the first arg is always of the same type as the second
;; use eql unless we are sure fixnums are represented canonically

(defmacro qeqcar (x y)
    (if (integerp y) `(eql (the fixnum (qcar ,x)) (the fixnum ,y))
         `(eq (qcar ,x) ,y)))

; 7 CONTROL STRUCTURE

; 7.1 Constants and Variables

; 7.1.1 Reference

(DEFUN MKQ (X)
  "Evaluates an object and returns it with QUOTE wrapped around it."
  (if (NUMBERP X) X (LIST 'QUOTE X)))

; 7.2 Generalized Variables

(defmacro LETT (var val &rest L)
  (COND
    (|$QuickLet| `(SETQ ,var ,val))
    (|$compilingMap|
   ;; map tracing
     `(PROGN
        (SETQ ,var ,val)
        (COND (|$letAssoc|
               (|mapLetPrint| ,(MKQ var)
                              ,var
                              (QUOTE ,(KAR L))))
              ('T ,var))))
     ;; used for LETs in SPAD code --- see devious trick in COMP-TRAN-1
     ((ATOM var)
      `(PROGN
         (SETQ ,var ,val)
         (IF |$letAssoc|
             ,(cond ((null (cdr l))
                     `(|letPrint| ,(MKQ var) ,var (QUOTE ,(KAR L))))
                    ((and (eqcar (car l) 'SPADCALL) (= (length (car l)) 3))
                     `(|letPrint3| ,(MKQ var) ,var ,(third (car l)) (QUOTE ,(KADR L))))
                    (t `(|letPrint2| ,(MKQ var) ,(car l) (QUOTE ,(KADR L))))))
         ,var))
     ('T (ERROR "Cannot compileLET construct"))))

(defmacro SPADLET (A B)
  (if (ATOM A) `(SETQ ,A ,B)
      (BREAK)))

(defmacro RPLAC (&rest L)
  (if (EQCAR (CAR L) 'ELT)
      (LIST 'SETELT (CADAR L) (CADDR (CAR L)) (CADR L))
      (let ((A (CARCDREXPAND (CAR L) NIL)) (B (CADR L)))
        (COND ((CDDR L) (ERROR 'RPLAC))
              ((EQCAR A 'CAR) (LIST 'RPLACA (CADR A) B))
              ((EQCAR A 'CDR) (LIST 'RPLACD (CADR A) B))
              ((ERROR 'RPLAC))))))

(MAPC #'(LAMBDA (J) (MAKEPROP (CAR J) 'SELCODE (CADR J)))
      '((CAR 2) (CDR 3) (CAAR 4) (CADR 5) (CDAR 6) (CDDR 7)
        (CAAAR 8) (CAADR 9) (CADAR 10) (CADDR 11) (CDAAR 12)
        (CDADR 13) (CDDAR 14) (CDDDR 15) (CAAAAR 16) (CAAADR 17)
        (CAADAR 18) (CAADDR 19) (CADAAR 20) (CADADR 21) (CADDAR 22)
        (CADDDR 23) (CDAAAR 24) (CDAADR 25) (CDADAR 26) (CDADDR 27)
        (CDDAAR 28) (CDDADR 29) (CDDDAR 30) (CDDDDR 31)))

(eval-when (compile eval load)
(defun CARCDREXPAND (X FG)    ; FG = TRUE FOR CAR AND CDR
    (let (n hx)
      (COND ((ATOM X) X)
            ((SETQ N (GET (SETQ HX (CARCDREXPAND (CAR X) FG)) 'SELCODE))
             (CARCDRX1 (CARCDREXPAND (CADR X) FG) N FG))
            ((CONS HX (MAPCAR #'(LAMBDA (Y) (CARCDREXPAND Y FG)) (CDR X)))))))

(defun CARCDRX1 (X N FG)      ; FG = TRUE FOR CAR AND CDR
    (COND ((< N 1) (fail))
          ((EQL N 1) X)
          ((let ((D (DIVIDE N 2)))
             (CARCDRX1 (LIST (if (EQL (CADR D) 0) (if FG 'CAR 'CAR) (if FG 'CDR 'CDR)) X)
                       (CAR D)
                       FG))))))


; 7.3 Function Invocation

; 7.8 Iteration

; 7.8.2 General Iteration

(defmacro REPEAT (&rest L) (|expandREPEAT| L))

(DEFUN MKPF (L OP)
  (if (FLAGP OP 'NARY) (SETQ L (MKPFFLATTEN-1 L OP NIL)))
  (MKPF1 L OP))

(DEFUN MKPFFLATTEN (X OP)
  (COND ((ATOM X) X)
        ((EQL (CAR X) OP) (CONS OP (MKPFFLATTEN-1 (CDR X) OP NIL)))
        ((CONS (MKPFFLATTEN (CAR X) OP) (MKPFFLATTEN (CDR X) OP)))))

(DEFUN MKPFFLATTEN-1 (L OP R)
  (let (X)
    (if (NULL L)
        R
        (MKPFFLATTEN-1 (CDR L) OP
           (APPEND R (if (EQCAR (SETQ X
                                      (MKPFFLATTEN (CAR L) OP)) OP)
                         (CDR X) (LIST X)))))))

(DEFUN MKPF1 (L OP)
  (let (X) (case OP (PLUS (COND ((EQL 0 (SETQ X (LENGTH
                                                 (SETQ L (S- L '(0 (ZERO))))))) 0)
                                ((EQL 1 X) (CAR L))
                                ((CONS 'PLUS L)) ))
                 (TIMES (COND ((S* L '(0 (ZERO))) 0)
                              ((EQL 0 (SETQ X (LENGTH
                                               (SETQ L (S- L '(1 (ONE))))))) 1)
                              ((EQL 1 X) (CAR L))
                              ((CONS 'TIMES L)) ))
                 (QUOTIENT (COND ((GREATERP (LENGTH L) 2) (fail))
                                 ((EQL 0 (CAR L)) 0)
                                 ((EQL (CADR L) 1) (CAR L))
                                 ((CONS 'QUOTIENT L)) ))
                 (MINUS (COND ((CDR L) (FAIL))
                              ((NUMBERP (SETQ X (CAR L))) (MINUS X))
                              ((EQCAR X 'MINUS) (CADR X))
                              ((CONS 'MINUS L))  ))
                 (DIFFERENCE (COND ((GREATERP (LENGTH L) 2) (FAIL))
                                   ((EQUAL (CAR L) (CADR L)) '(ZERO))
                                   ((|member| (CAR L) '(0 (ZERO))) (MKPF (CDR L) 'MINUS))
                                   ((|member| (CADR L) '(0 (ZERO))) (CAR L))
                                   ((EQCAR (CADR L) 'MINUS)
                                    (MKPF (LIST (CAR L) (CADADR L)) 'PLUS))
                                   ((CONS 'DIFFERENCE L)) ))
                 (EXPT (COND ((GREATERP (LENGTH L) 2) (FAIL))
                             ((EQL 0 (CADR L)) 1)
                             ((EQL 1 (CADR L)) (CAR L))
                             ((|member| (CAR L) '(0 1 (ZERO) (ONE))) (CAR L))
                             ((CONS 'EXPT L)) ))
                 (OR (COND ((MEMBER 'T L) ''T)
                           ((EQL 0 (SETQ X (LENGTH (SETQ L (REMOVE NIL L))))) NIL)
                           ((EQL 1 X) (CAR L))
                           ((CONS 'OR L)) ))
                 (|or| (COND ((MEMBER 'T L) 'T)
                             ((EQL 0 (SETQ X (LENGTH (SETQ L (REMOVE NIL L))))) NIL)
                             ((EQL 1 X) (CAR L))
                             ((CONS 'or L)) ))
                 (NULL (COND ((CDR L) (FAIL))
                             ((EQCAR (CAR L) 'NULL) (CADAR L))
                             ((EQL (CAR L) 'T) NIL)
                             ((NULL (CAR L)) ''T)
                             ((CONS 'NULL L)) ))
                 (|and| (COND ((EQL 0 (SETQ X (LENGTH
                                               (SETQ L (REMOVE T (REMOVE '|true| L)))))) T)
                              ((EQL 1 X) (CAR L))
                              ((CONS '|and| L)) ))
                 (AND (COND ((EQL 0 (SETQ X (LENGTH
                                             (SETQ L (REMOVE T (REMOVE '|true| L)))))) ''T)
                            ((EQL 1 X) (CAR L))
                            ((CONS 'AND L)) ))
                 (PROGN (COND ((AND (NOT (ATOM L)) (NULL (LAST L)))
                               (if (CDR L) `(PROGN . ,L) (CAR L)))
                              ((NULL (SETQ L (REMOVE NIL L))) NIL)
                              ((CDR L) (CONS 'PROGN L))
                              ((CAR L))))
                 (SEQ (COND ((EQCAR (CAR L) 'EXIT) (CADAR L))
                            ((CDR L) (CONS 'SEQ L))
                            ((CAR L))))
                 (LIST (if L (cons 'LIST L)))
                 (CONS (if (cdr L) (cons 'CONS L) (car L)))
                 (t (CONS OP L) ))))

(defvar $TRACELETFLAG NIL "Also referred to in Comp.Lisp")

(defmacro |Zero| (&rest L)
 (declare (ignore l))
 "Needed by spadCompileOrSetq" 0)

(defmacro |One| (&rest L)
 (declare (ignore l))
 "Needed by spadCompileOrSetq" 1)

(defvar $BOOT NIL)

; 7.8.4 Mapping

(defmacro COLLECT (&rest L) (|expandCOLLECT| L))

(defun MKQSADD1 (X)
  (COND ((ATOM X) `(QSADD1 ,X))
        ((AND (member (CAR X) '(DIFFERENCE QSDIFFERENCE -) :test #'eq)
              (EQL 1 (CADDR X)))
         (CADR X))
        (`(QSADD1 ,X))))

; 10.1 The Property List

(DEFUN FLAG (L KEY)
  "Set the KEY property of every item in list L to T."
  (mapc #'(lambda (item) (makeprop item KEY T)) L))

(FLAG '(* + AND OR PROGN) 'NARY)                ; flag for MKPF

(DEFUN FLAGP (X KEY)
  "If X has a KEY property, then FLAGP is true."
  (GET X KEY))

(defun PROPERTY (X IND N)
  "Returns the Nth element of X's IND property, if it exists."
  (let (Y) (if (AND (INTEGERP N) (SETQ Y (GET X IND)) (>= (LENGTH Y) N)) (ELEM Y N))))

; 10.3 Creating Symbols

(defmacro INTERNL (a &rest b) (if (not b) `(intern ,a) `(intern (strconc ,a . ,b))))

(defvar $GENNO 0)

(DEFUN GENVAR () (INTERNL "$" (STRINGIMAGE (SETQ $GENNO (1+ $GENNO)))))

(DEFUN IS_GENVAR (X)
  (AND (IDENTP X)
       (let ((y (symbol-name x)))
         (and (char= #\$ (elt y 0)) (> (size y) 1) (digitp (elt y 1))))))

; 12 NUMBERS

; 12.4 Arithmetic Operations

(defmacro SPADDIFFERENCE (&rest x) `(- . ,x))

; 12.6 Small Finite Field ops with vector trimming

;; following macros assume 0 <= x,y < z
;; qsaddmod additionally assumes that rsum has correct value even
;; when (x + y) exceeds range of a fixnum.  This is true if
;; fixnums use modular arithmetic with no overflow checking,
;; but according to ANSI Lisp the result is undefined in
;; such case.

(defmacro qsaddmod (x y z)
  `(let* ((sum (qsplus ,x ,y))
          (rsum (qsdifference sum ,z)))
     (if (qsminusp rsum) sum rsum)))

(defmacro qsdifmod (x y z)
  `(let ((dif (qsdifference ,x ,y)))
     (if (qsminusp dif) (qsplus dif ,z) dif)))

(defmacro qsmultmod (x y z)
 `(rem (* ,x ,y) ,z))

; 14 SEQUENCES

; 14.1 Simple Sequence Functions

(define-function 'getchar #'elt)

; 14.2 Concatenating, Mapping, and Reducing Sequences

(MAPC #'(LAMBDA (X) (MAKEPROP (CAR X) 'THETA (CDR X)))
      '((PLUS 0) (+ (|Zero|)) (|lcm| (|One|)) (STRCONC "") (|strconc| "")
        (MAX -999999) (MIN 999999) (TIMES 1) (* (|One|)) (CONS NIL)
        (APPEND NIL) (|append| NIL) (UNION NIL) (UNIONQ NIL) (|gcd| (|Zero|))
        (|union| NIL) (NCONC NIL) (|and| |true|) (|or| |false|) (AND 'T)
        (OR NIL)))

(define-function '|append| #'APPEND)

(defun |delete| (item sequence)
   (cond ((symbolp item) (remove item sequence :test #'eq))
         ((and (atom item) (not (arrayp item))) (remove item sequence))
         (T (remove item sequence :test #'equalp))))

; 15 LISTS

; 15.1 Conses


(defmacro |SPADfirst| (l)
  (let ((tem (gensym)))
    `(let ((,tem ,l)) (if ,tem (car ,tem) (first-error)))))

(defun first-error () (error "Cannot take first of an empty list"))

; 15.2 Lists


(defmacro ELEM (val &rest indices)
   (if (null indices) val `(ELEM (nth (1- ,(car indices)) ,val) ,@(cdr indices))))

(defun ELEMN (X N DEFAULT)
  (COND ((NULL X) DEFAULT)
        ((EQL N 1) (CAR X))
        ((ELEMN (CDR X) (SUB1 N) DEFAULT))))

(defmacro SPADCONST (&rest L) (cons 'qrefelt L))

(defmacro SPADCALL (&rest L)
  (let ((args (butlast l))
	(fn (car (last l)))
	(gi (gensym)))
     ;; (values t) indicates a single return value
    `(let ((,gi ,fn))
       (the (values t)
	 (funcall
          (the #-(or :genera :lispworks)
                   (function ,(make-list (length l) :initial-element t) t)
               #+(or :genera :lispworks)function
	    (car ,gi))
	  ,@args
	  (cdr ,gi))))))

(defun LISTOFATOMS (X)
  (COND ((NULL X) NIL)
        ((ATOM X) (LIST X))
        ((NCONC (LISTOFATOMS (CAR X)) (LISTOFATOMS (CDR X))))))

(DEFUN LASTATOM (L) (if (ATOM L) L (LASTATOM (CDR L))))

(define-function 'LASTTAIL #'last)

(defun DROP (N X &aux m)
  "Return a pointer to the Nth cons of X, counting 0 as the first cons."
  (COND ((EQL N 0) X)
        ((> N 0) (DROP (1- N) (CDR X)))
        ((>= (setq m (+ (length x) N)) 0) (take m x))
        ((CROAK (list "Bad args to DROP" N X)))))

(DEFUN TAKE (N X &aux m)
  "Returns a list of the first N elements of list X."
  (COND ((EQL N 0) NIL)
        ((> N 0) (CONS (CAR X) (TAKE (1- N) (CDR X))))
        ((>= (setq m (+ (length x) N)) 0) (DROP m x))
        ((CROAK (list "Bad args to DROP" N X)))))

(DEFUN NUMOFNODES (X) (if (ATOM X) 0 (+ 1 (NUMOFNODES (CAR X)) (NUMOFNODES (CDR X)))))

(DEFUN TRUNCLIST (L TL) "Truncate list L at the point marked by TL."
  (let ((U L)) (TRUNCLIST-1 L TL) U))

(DEFUN TRUNCLIST-1 (L TL)
  (COND ((ATOM L) L)
        ((EQL (CDR L) TL) (RPLACD L NIL))
        ((TRUNCLIST-1 (CDR L) TL))))

; 15.4 Substitution of Expressions

(DEFUN SUBSTEQ (NEW OLD FORM)
  "Version of SUBST that uses EQ rather than EQUAL on the world."
  (PROG (NFORM HNFORM ITEM)
        (SETQ HNFORM (SETQ NFORM (CONS () ())))
     LP    (RPLACD NFORM
                   (COND ((EQ FORM OLD) (SETQ FORM ()) NEW )
                         ((NOT (PAIRP FORM)) FORM )
                         ((EQ (SETQ ITEM (CAR FORM)) OLD) (CONS NEW ()) )
                         ((PAIRP ITEM) (CONS (SUBSTEQ NEW OLD ITEM) ()) )
                         ((CONS ITEM ()))))
        (if (NOT (PAIRP FORM)) (RETURN (CDR HNFORM)))
        (SETQ NFORM (CDR NFORM))
        (SETQ FORM (CDR FORM))
        (GO LP)))

;; needed for substNames (always copy)
(define-function 'SUBSTQ #'SUBSTEQ)


(DEFUN SUBLISNQ (KEY E) (declare (special KEY)) (if (NULL KEY) E (SUBANQ E)))

(DEFUN SUBANQ (E)
  (declare (special key))
  (COND ((ATOM E) (SUBB KEY E))
        ((EQCAR E (QUOTE QUOTE)) E)
        ((MAPCAR #'(LAMBDA (J) (SUBANQ J)) E))))

(DEFUN SUBB (X E)
  (COND ((ATOM X) E)
        ((EQ (CAAR X) E) (CDAR X))
        ((SUBB (CDR X) E))))

(defun SUBLISLIS (newl oldl form)
   (sublis (mapcar #'cons oldl newl) form))

; 15.5 Using Lists as Sets

;;; The [[CONTAINED]] predicate is used to walk internal structures
;;; such as modemaps to see if the $X$ object occurs within $Y$. One
;;; particular use is in a function called [[isPartialMode]] (see
;;; i-funsel.boot) to decide
;;; if a modemap is only partially complete. If this is true then the
;;; modemap will contain the constant [[$EmptyMode]]. So the call
;;; ends up being [[CONTAINED |$EmptyMode| Y]].
(DEFUN CONTAINED (X Y)
  (if (symbolp x)
      (contained\,eq X Y)
      (contained\,equal X Y)))

(defun contained\,eq (x y)
       (if (atom y) (eq x y)
           (or (contained\,eq x (car y)) (contained\,eq x (cdr y)))))

(defun contained\,equal (x y)
   (cond ((atom y) (equal x y))
         ((equal x y) 't)
         ('t (or (contained\,equal x (car y)) (contained\,equal x (cdr y))))))

(DEFUN S+ (X Y)
  (COND ((ATOM Y) X)
        ((ATOM X) Y)
        ((MEMBER (CAR X) Y :test #'equal) (S+ (CDR X) Y))
        ((S+ (CDR X) (CONS (CAR X) Y)))))

(defun S* (l1 l2) (INTERSECTION l1 l2 :test #'equal))
(defun S- (l1 l2) (set-difference l1 l2 :test #'equal))

(DEFUN PREDECESSOR (TL L)
  "Returns the sublist of L whose CDR is EQ to TL."
  (COND ((ATOM L) NIL)
        ((EQ TL (CDR L)) L)
        ((PREDECESSOR TL (CDR L)))))

(defun remdup (l) (remove-duplicates l :test #'equalp))

; 15.6 Association Lists

(defun DELASC (u v) "Returns a copy of a-list V in which any pair with key U is deleted."
   (cond ((atom v) nil)
         ((or (atom (car v))(not (equal u (caar v))))
          (cons (car v) (DELASC u (cdr v))))
         ((cdr v))))

(DEFUN ADDASSOC (X Y L)
  "Put the association list pair (X . Y) into L, erasing any previous association for X"
  (COND ((ATOM L) (CONS (CONS X Y) L))
        ((EQUAL X (CAAR L)) (CONS (CONS X Y) (CDR L)))
        ((CONS (CAR L) (ADDASSOC X Y (CDR L))))))

(DEFUN DELLASOS (U V)
  "Remove any assocation pair (U . X) from list V."
  (COND ((ATOM V) NIL)
        ((EQUAL U (CAAR V)) (CDR V))
        ((CONS (CAR V) (DELLASOS U (CDR V))))))

(DEFUN ASSOCLEFT (X)
  "Returns all the keys of association list X."
  (if (ATOM X) X (mapcar #'car x)))

(DEFUN ASSOCRIGHT (X)
  "Returns all the datums of association list X."
  (if (ATOM X) X (mapcar #'cdr x)))

(DEFUN LASSOC (X Y)
  "Return the datum associated with key X in association list Y."
  (PROG ()
     A  (COND ((ATOM Y) (RETURN NIL))
              ((EQUAL (CAAR Y) X) (RETURN (CDAR Y))) )
        (SETQ Y (CDR Y))
        (GO A)))

(DEFUN |rassoc| (X Y)
  "Return the datum associated with key X in association list Y."
  (PROG ()
     A  (COND ((ATOM Y) (RETURN NIL))
              ((EQUAL (CDAR Y) X) (RETURN (CAAR Y))) )
        (SETQ Y (CDR Y))
        (GO A)))

; (defun QLASSQ (p a-list) (let ((y (assoc p a-list :test #'eq))) (if y (cdr y))))
(defun QLASSQ (p a-list) (cdr (assq p a-list)))

(define-function 'LASSQ #'QLASSQ)

(defun pair (x y) (mapcar #'cons x y))

;;; Operations on Association Sets (AS)

(defun AS-INSERT (A B L)
    (let ((pp (assoc A L :test #'equal)))
        (if pp
            (progn
                 (setf (cdr pp) B)
                 L))
         (cons (cons A B) L)))

; 17 ARRAYS

; 17.6 Changing the Dimensions of an Array

(defmacro |replaceString| (result part start)
    `(replace ,result ,part :start1 ,start))

;;; The function below continually adds one element to a vector.
;;; We use vectors with fill pointers to limit numbers of
;;; copies made (otherwise this function would use too much time).
(defun lengthenvec (v n)
  (if
    (and (array-has-fill-pointer-p v) (adjustable-array-p v))
    (if
      (>= n (array-total-size v))
        (adjust-array v (* n 2) :fill-pointer n)
        (progn
          (setf (fill-pointer v) n)
          v))
    (replace (make-array n :fill-pointer t) v)))


; 22 INPUT/OUTPUT

; 22.2 Input Functions

; 22.2.1 Input from Character Streams

(defvar *EOF* NIL)


; 22.3 Output Functions

; 22.3.1 Output to Character Streams

(defvar |$sayBrightlyStream| nil "if not nil, gives stream for sayBrightly output")

(defun |sayBrightly| (x) (|sayBrightly2| x *standard-output*))

(defun |sayBrightly2| (x out-stream)
  (COND ((NULL X) NIL)
        (|$sayBrightlyStream| (|sayBrightly1| X |$sayBrightlyStream|))
        ((IS-CONSOLE out-stream) (|sayBrightly1| X out-stream))
        ((|sayBrightly1| X out-stream) (|sayBrightly1| X *error-output*))))

(defun |sayBrightlyI| (x)
 (let ((S *error-output*))
    "Prints at console or output stream."
  (if (NULL X) NIL (|sayBrightly1| X S))))

(defun |sayBrightlyNT| (x) (|sayBrightlyNT2| x *standard-output*))

(defun |sayBrightlyNT2| (x S)
  (COND ((NULL X) NIL)
        (|$sayBrightlyStream| (|sayBrightlyNT1| X |$sayBrightlyStream|))
        ((IS-CONSOLE S) (|sayBrightlyNT1| X S))
        ((|sayBrightly1| X S) (|sayBrightlyNT1| X *error-output*))))

(defparameter |$fricasOutput| (make-synonym-stream '*standard-output*))

(defun |sayMSG2File| (msg)
  (PROG (file str)
        (SETQ file (|makePathname| '|spadmsg| '|listing|))
        (SETQ str (MAKE-OUTSTREAM file))
        (|sayBrightly1| msg str)
        (SHUT str) ) )

(defvar |$fortranOutputStream|)

(defvar |$formulaOutputStream|)

(defvar |$highlightAllowed| nil "Used in BRIGHTPRINT and is a )set variable.")

(defvar |$highlightFontOn| (concat " " |$boldString|)
                     "switch to highlight font")
(defvar |$highlightFontOff| (concat |$normalString| " ")
                     "return to normal font")

(defun SAY (&rest x) (progn (MESSAGEPRINT X) (TERPRI)))

(DEFUN MESSAGEPRINT (X) (mapc #'messageprint-1 X))

(DEFUN MESSAGEPRINT-1 (X)
  (COND ((OR (EQ X '|%l|) (EQUAL X "%l")) (TERPRI))
        ((STRINGP X) (PRINC X))
        ((IDENTP X) (PRINC X))
        ((ATOM X) (PRINC X))
        ((PRINC "(") (MESSAGEPRINT-1 (CAR X))
         (MESSAGEPRINT-2 (CDR X)) (PRINC ")"))))

(DEFUN MESSAGEPRINT-2 (X)
  (if (ATOM X)
      (if (NULL X) NIL (progn (PRINC " . ") (MESSAGEPRINT-1 X)))
      (progn (PRINC " ") (MESSAGEPRINT-1 (CAR X)) (MESSAGEPRINT-2 (CDR X)))))

(DEFUN BLANKS (N &optional (stream *standard-output*)) "Print N blanks."
    (do ((i 1 (the fixnum(1+ i))))
        ((> i N))(declare (fixnum i n)) (princ " " stream)))

; 24 ERRORS

; 24.2 Specialized Error-Signalling Forms and Macros

(defun MOAN (&rest x) (|sayBrightly| `(|%l| "===> " ,@X |%l|)))

(DEFUN FAIL () (|systemError| '"Antique error (FAIL ENTERED)"))

(defun CROAK (&rest x) (|systemError| x))

; 25 MISCELLANEOUS FEATURES

;; range tests and assertions

(defmacro |assert| (x y) `(IF (NULL ,x) (|error| ,y)))

(defun coerce-failure-msg (val mode)
   (STRCONC (MAKE-REASONABLE (STRINGIMAGE val))
            " cannot be coerced to mode "
            (STRINGIMAGE (|devaluate| mode))))

(defmacro |check-subtype| (pred submode val)
   `(|assert| ,pred (coerce-failure-msg ,val ,submode)))

(defmacro |check-union| (pred branch val)
   `(|assert| ,pred (coerce-failure-msg ,val ,branch )))

(defun MAKE-REASONABLE (Z)
   (if (> (length Z) 30) (CONCAT "expression beginning " (subseq Z 0 20)) Z))


(defmacro |elapsedUserTime| () '(get-internal-run-time))

#+:GCL
(defmacro |elapsedGcTime| () '(system:gbc-time))
#-:GCL
(defmacro |elapsedGcTime| () '0)

(defmacro |do| (&rest args) (CONS 'PROGN args))

(defmacro |char| (arg)
  (cond ((stringp arg) (character arg))
        ((integerp arg) (code-char arg))
        ((and (consp arg) (eq (car arg) 'quote)) (character (cadr arg)))
        (t `(character ,arg))))

(defun DROPTRAILINGBLANKS  (LINE)
     (let ((l (length LINE)))
         (if (and (> l 0)
                  (char= (char LINE (1- l)) #\ ))
             (string-right-trim " " LINE)
             LINE)))

; # Gives the number of elements of a list, 0 for atoms.
; If we quote it, then an interpreter trip is necessary every time
; we call #, and this costs us - 4% in the RATINT DEMO."

(define-function '\# #'SIZE)

(defun print-and-eval-defun (name body)
   (eval body)
   (print-defun name body)
   )

(defun eval-defun (name body) (eval (macroexpandall body)))

; This function was modified by Greg Vanuxem on March 31, 2005
; to handle the special case of #'(lambda ..... which expands
; into (function (lambda .....
;
; The extra if clause fixes bugs #196 and #114
;
; an example that used to cause the failure was:
; )set func comp off
; f(xl:LIST FRAC INT): LIST FRAC INT == map(x +-> x, xl)
; f [1,2,3]
;
; which expanded into
;
; (defun |xl;f;1;initial| (|#1| |envArg|)
;  (prog (#:G1420)
;   (return
;    (progn
;     (lett #:G1420 'uninitialized_variable |f| |#1;f;1:initial|)
;      (spadcall
;       (cons (|function| (lambda (#:G1420 |envArg|) #:G1420)) (vector))
;       |#1|
;       (qrefelt |*1;f;1;initial;MV| 0))))))
;
; the (|function| (lambda form used to cause an infinite expansion loop
;
(defun macroexpandall (sexpr)
 (cond
  ((atom sexpr) sexpr)
  ((eq (car sexpr) 'quote) sexpr)
  ((eq (car sexpr) 'defun)
   (cons (car sexpr) (cons (cadr sexpr)
       (mapcar #'macroexpandall (cddr sexpr)))))
  ((and (symbolp (car sexpr)) (macro-function (car sexpr)))
   (do ()
       ((not (and (consp sexpr) (symbolp (car sexpr))
                  (macro-function (car sexpr)))))
     (setq sexpr (macroexpand sexpr)))
   (if (consp sexpr)
      (let ((a (car sexpr)) (b (cadr sexpr)))
         (if (and (eq a 'function) (consp b) (eq (car b) 'lambda))
            (cons a (list (cons (car b)
                                (mapcar #'macroexpandall (cdadr sexpr)))))
            (mapcar #'macroexpandall sexpr)))
      sexpr))
  ('else
    (mapcar #'macroexpandall sexpr))))


(defun compile-defun (name body) (eval body) (compile name))

(defmacro |Record| (&rest args)
    (list '|Record0|
          (cons 'LIST
                (mapcar #'(lambda (x) (list 'CONS (MKQ (CADR x)) (CADDR x)))
                        args))))

(defmacro |Enumeration| (&rest args)
      (cons '|Enumeration0|
	            (mapcar #'(lambda (x) (list 'QUOTE x)) args)))

(defmacro |:| (tag expr) `(LIST '|:| ,(MKQ tag) ,expr))

(DEFUN |leftBindingPowerOf| (X IND &AUX (Y (GETL X IND)))
   (IF Y (ELEMN Y 3 0) 0))

(DEFUN |rightBindingPowerOf| (X IND &AUX (Y (GETL X IND)))
   (IF Y (ELEMN Y 4 105) 105))

(defmacro MAKE-BF (MT EP) `(LIST |$BFtag| ,MT ,EP))

(defun MAKE-FLOAT (int frac fraclen exp)
    (if (AND $SPAD |$useBFasDefault|)
        (if (= frac 0)
          (MAKE-BF int exp)
          (MAKE-BF (+ (* int (expt 10 fraclen)) frac) (- exp fraclen)) )
        (read-from-string
          (format nil "~D.~v,'0De~D" int fraclen frac exp))) )

(defun print-full (expr &optional (stream *standard-output*))
   (let ((*print-circle* t) (*print-array* t) *print-level* *print-length*)
     (print expr stream)
     (terpri stream)
     (finish-output stream)))

(defmacro |float| (x &optional (y 0.0d0)) `(float ,x ,y))

;; moved here from spad.lisp

(defmacro |rplac| (&rest L)
  (let (a b s)
    (cond
      ((EQCAR (SETQ A (CAR L)) 'ELT)
       (COND ((AND (INTEGERP (SETQ B (CADDR A))) (>= B 0))
              (SETQ S "CA")
              (do ((i 1 (1+ i))) ((> i b)) (SETQ S (STRCONC S "D")))
              (LIST 'RPLAC (LIST (INTERN (STRCONC S "R")) (CADR A)) (CADR L)))
             ((ERROR "rplac"))))
      ((PROGN
         (SETQ A (CARCDREXPAND (CAR L) NIL))
         (SETQ B (CADR L))
         (COND
           ((CDDR L) (ERROR 'RPLAC))
           ((EQCAR A 'CAR) (LIST 'RPLACA (CADR A) B))
           ((EQCAR A 'CDR) (LIST 'RPLACD (CADR A) B))
           ((ERROR 'RPLAC))))))))

;; moved here from preparse.lisp

(defun NEXT-TAB-LOC (i) (* (1+ (truncate i 8)) 8))

(defun INDENT-POS (STR)
  (do ((i 0 (1+ i))
       (pos 0))
      ((>= i (length str)) nil)
      (case (char str i)
            (#\space (incf pos))
            (#\tab (setq pos (next-tab-loc pos)))
            (otherwise (return pos)))))

;;(defun expand-tabs (str)
;;  (let ((bpos (nonblankloc str))
;;      (tpos (indent-pos str)))
;;    (if (eql bpos tpos) str
;;      (concatenate 'string (make-string tpos :initial-element #\space)
;;                 (subseq str bpos)))))
(defun expand-tabs (str)
   (if (and (stringp str) (> (length str) 0))
      (let ((bpos (nonblankloc str))
            (tpos (indent-pos str)))
        (setq str
              (if (eql bpos tpos)
                  str
                  (concatenate 'string
                               (make-string tpos :initial-element #\space)
                               (subseq str bpos))))
         ;; remove dos CR
        (let ((lpos (maxindex str)))
          (if (eq (char str lpos) #\Return) (subseq str 0 lpos) str)))
    str))

(defun blankp (char) (or (eq char #\Space) (eq char #\tab)))

(defun nonblankloc (str) (position-if-not #'blankp str))

;; Temporary parser macros

(defmacro |symbol_is?| (x)
   `(eq (current-symbol) ,x))

(defmacro |match_symbol| (x)
   `(if (|symbol_is?| ,x)
          (progn
             (advance-token)
             t)))

;; stream handling for paste-in generation

(defun |applyWithOutputToString| (func args)
  ;; returns the cons of applying func to args and a string produced
  ;; from standard-output while executing.
  (let* ((*standard-output* (make-string-output-stream))
         (curoutstream *standard-output*)
         (*error-output* *standard-output*)
         (|$algebraOutputStream| *standard-output*)
        val)
    (declare (special curoutstream
                      |$algebraOutputStream|))
    (setq val (catch 'spad_reader
                (catch 'TOP_LEVEL
                  (apply (symbol-function func) args))))
    (cons val (get-output-stream-string *standard-output*))))

(defun |breakIntoLines| (str)
  (let ((bol 0) (eol) (line-list nil))
    (loop
     (setq eol (position #\Newline str :start bol))
     (if (null eol) (return))
     (if (> eol bol)
         (setq line-list (cons (subseq str bol eol) line-list)))
     (setq bol (+ eol 1)))
    (nreverse line-list)))

; moved from comp.lisp

(export '(Comp SLAM SPADSLAM FLUID))

;;; Common Block section

(defun |compAndDefine| (L)
  (let ((|$comp370_apply| (function print-and-eval-defun)))
    (declare (special |$comp370_apply|))
    (COMP L)))

(defun comp_quietly_using_driver (driver fn)
  (let ((|$comp370_apply|
         (if |$InteractiveMode|
             (if |$compileDontDefineFunctions| #'compile-defun #'eval-defun)
           #'print-defun))
     ;; following creates a null outputstream if $InteractiveMode
        (*standard-output*
         (if |$InteractiveMode| (make-broadcast-stream)
           *standard-output*))
        (*compile-verbose* nil))
    (declare (special |$comp370_apply|))
    #-:GCL
    (handler-bind ((warning #'muffle-warning)
                   #+:sbcl (sb-ext::compiler-note #'muffle-warning))
      (funcall driver fn)
      )
    #+:GCL
      (funcall driver fn)
))

(defun |compQuietly| (fn)
    (comp_quietly_using_driver #'COMP fn))

(defun |compileFileQuietly| (fn)
    (comp_quietly_using_driver #'COMPILE-FILE fn))

(defun |compileQuietly| (fn)
    (comp_quietly_using_driver #'COMP370 fn))

;; used to be called POSN - but that interfered with a CCL function
(DEFUN POSN1 (X L) (position x l :test #'equal))

; end of moved fragment
