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


;;;  @(#)debug.lisp     2.5      90/02/15  10:27:33

; NAME:    Debugging Package
; PURPOSE: Debugging hooks for Boot code

(in-package "BOOT")

(DEFPARAMETER /COUNTLIST NIL)
(DEFPARAMETER /TIMERLIST NIL)
(DEFPARAMETER /TRACESIZE NIL "sets limit on size of object to be mathprinted")
(DEFPARAMETER /DEPTH 0)
(DEFVAR CURSTRM *TERMINAL-IO*)
(DEFVAR /PRETTY () "controls pretty printing of trace output")
(SETANDFILEQ /ECHO NIL) ;;"prevents echo of SPAD or BOOT code with /c"

(defun enable-backtrace (&rest arg))

(defun heapelapsed () 0)

(DEFUN DEFSTREAM (file MODE)
       (if (member mode '(i input))
           (MAKE-INSTREAM file)
         (MAKE-OUTSTREAM file)))

(defmacro /TRACE (&rest L) `',(/TRACE-0 L))

(DEFUN /TRACE-0 (L)
    (let* ((options (/OPTIONS L)) (FNL (TRUNCLIST L OPTIONS)))
        (/TRACE-1 FNL OPTIONS)))

(defmacro /TRACEANDCOUNT (&rest L) `',
  (let* ((OPTIONS (/OPTIONS L))
         (FNL (TRUNCLIST L OPTIONS)))
    (/TRACE-1 FNL (CONS '(DEPTH) OPTIONS))))

(DEFUN /TRACE-1 (FNLIST OPTIONS)
   (mapcar #'(lambda (X) (/TRACE-2 X OPTIONS)) FNLIST)
   (/TRACEREPLY))

(DEFUN /TRACE-2 (FN OPTIONS)
  (PROG (U FNVAL COUNTNAM TRACECODE BEFORE AFTER CONDITION
         TRACENAME CALLER VARS BREAK FROM_CONDITION VARBREAK TIMERNAM
         ONLYS G WITHIN_CONDITION  DEPTH_CONDITION COUNT_CONDITION
         LETFUNCODE MATHTRACE )
        (if (member FN /TRACENAMES :test #'eq) (/UNTRACE-2 FN NIL))
        (SETQ OPTIONS (OPTIONS2UC OPTIONS))
        (if (AND |$traceDomains| (|isFunctor| FN) (ATOM FN))
            (RETURN (|traceDomainConstructor| FN OPTIONS)))
        (SETQ MATHTRACE (/GETTRACEOPTIONS OPTIONS 'MATHPRINT))
        (if (AND MATHTRACE (NOT (EQL (ELT (PNAME FN) 0) #\$)) (NOT (GENSYMP FN)))
            (if (RASSOC FN |$mapSubNameAlist|)
                (SETQ |$mathTraceList| (CONS FN |$mathTraceList|))
                (|spadThrowBrightly|
                  (format nil "mathprint not available for ~A" FN))))
        (SETQ VARS (/GETTRACEOPTIONS OPTIONS 'VARS))
        (if VARS
            (progn (if (NOT (CDR VARS)) (SETQ VARS 'all) (SETQ VARS (CDR VARS)))
                   (|tracelet| FN VARS)))
        (SETQ BREAK (/GETTRACEOPTIONS OPTIONS 'BREAK))
        (SETQ VARBREAK (/GETTRACEOPTIONS OPTIONS 'VARBREAK))
        (if VARBREAK
            (progn   (if (NOT (CDR VARBREAK)) (SETQ VARS 'all)
                         (SETQ VARS (CDR VARBREAK)))
                     (|breaklet| FN VARS)))
        (if (and (symbolp fn) (not (boundp FN)) (not (fboundp FN)))
            (progn
              (COND ((|isUncompiledMap| FN)
                     (|sayBrightly|
                       (format nil
           "~A must be compiled before it may be traced -- invoke ~A to compile"
                                            FN FN)))
                    ((|isInterpOnlyMap| FN)
                     (|sayBrightly| (format nil
            "~A cannot be traced because it is an interpret-only function" FN)))
                    (T (|sayBrightly| (format nil "~A is not a function" FN))))
              (RETURN NIL)))
        (if (and (symbolp fn) (boundp FN)
                 (|isDomainOrPackage| (SETQ FNVAL (EVAL FN))))
            (RETURN (|spadTrace| FNVAL OPTIONS)))
        (if (SETQ U (/GETTRACEOPTIONS OPTIONS 'MASK=))
            (MAKEPROP FN '/TRANSFORM (CADR U)))
        (SETQ /TRACENAMES
              (COND ((/GETTRACEOPTIONS OPTIONS 'ALIAS) /TRACENAMES)
                    ((ATOM /TRACENAMES) (LIST FN))
                    ((CONS FN /TRACENAMES))))
        (SETQ TRACENAME
              (COND ((SETQ U (/GETTRACEOPTIONS OPTIONS 'ALIAS))
                     (STRINGIMAGE (CADR U)))
                    (T
                     (COND ((AND |$traceNoisely| (NOT VARS)
                                 (NOT (|isSubForRedundantMapName| FN)))
                            (|sayBrightly|
                             (LIST '|%b| (|rassocSub| FN |$mapSubNameAlist|)
                                   '|%d| "traced"))))
                     (STRINGIMAGE FN))))
        (COND (|$fromSpadTrace|
               (if MATHTRACE (push (INTERN TRACENAME) |$mathTraceList|))
               (SETQ LETFUNCODE `(EQ nil nil)) ;; No-op
               (SETQ BEFORE
                     (if (SETQ U (/GETTRACEOPTIONS OPTIONS 'BEFORE))
                         `(progn ,(CADR U) ,LETFUNCODE)
                         LETFUNCODE)))
              (T (SETQ BEFORE
                       (if (SETQ U (/GETTRACEOPTIONS OPTIONS 'BEFORE))
                           (CADR U)))))
        (SETQ AFTER (if (SETQ U (/GETTRACEOPTIONS OPTIONS 'AFTER)) (CADR U)))
        (SETQ CALLER (/GETTRACEOPTIONS OPTIONS 'CALLER))
        (SETQ FROM_CONDITION
              (if (SETQ U (/GETTRACEOPTIONS OPTIONS 'FROM))
                  (LIST 'EQ '|#9| (LIST 'QUOTE (CADR U)))
                  T))
        (SETQ CONDITION
              (if (SETQ U (/GETTRACEOPTIONS OPTIONS 'WHEN)) (CADR U) T))
        (SETQ WITHIN_CONDITION T)
        (COND ((SETQ U (/GETTRACEOPTIONS OPTIONS 'WITHIN))
               (SETQ G (INTERN (STRCONC (PNAME FN) "/" (PNAME (CADR U)))))
               (SET G 0)
               (/TRACE-1
                 (LIST (CADR U))
                 `((WHEN NIL)
                   (BEFORE (SETQ ,G (1+ ,G)))
                   (AFTER (SETQ ,G (1- ,G)))))
               (SETQ WITHIN_CONDITION `(> ,G 0))))
        (SETQ COUNTNAM
              (AND (/GETTRACEOPTIONS OPTIONS 'COUNT)
                   (INTERN (STRCONC TRACENAME ",COUNT"))) )
        (SETQ COUNT_CONDITION
              (COND ((SETQ U (/GETTRACEOPTIONS OPTIONS 'COUNT))
                     (SETQ /COUNTLIST (adjoin TRACENAME /COUNTLIST
                                              :test 'equal))
                     (if (AND (CDR U) (integerp (CADR U)))
                         `(cond ((<= ,COUNTNAM ,(CADR U)) t)
                                (t (/UNTRACE-2 ,(MKQ FN) NIL) NIL))
                         t))
                    (T T)))
        (AND (/GETTRACEOPTIONS OPTIONS 'TIMER)
             (SETQ TIMERNAM (INTERN (STRCONC TRACENAME ",TIMER")))
             (SETQ /TIMERLIST (adjoin TRACENAME /TIMERLIST :test 'equal)))
        (SETQ DEPTH_CONDITION
              (if (SETQ U (/GETTRACEOPTIONS OPTIONS 'DEPTH))
                  (if (AND (CDR U) (integerp (CADR U)))
                      (LIST 'LE 'FUNDEPTH (CADR U))
                    (TRACE_OPTION_ERROR 'DEPTH))
                  T))
        (SETQ CONDITION
              (MKPF
                (LIST CONDITION WITHIN_CONDITION FROM_CONDITION COUNT_CONDITION
                      DEPTH_CONDITION )
                'AND))
        (SETQ ONLYS (/GETTRACEOPTIONS OPTIONS 'ONLY))

        ;TRACECODE meaning:
        ; 0:        Caller (0,1)           print caller if 1
        ; 1:        Value (0,1)            print value if 1
        ; 2...:     Arguments (0,...,9)    stop if 0; print ith if i; all if 9
        (SETQ TRACECODE
              (if (/GETTRACEOPTIONS OPTIONS 'NT) "000"
                  (PROG (F A V C NL BUF)
                        (SETQ ONLYS (MAPCAR #'COND-UCASE ONLYS))
                        (SETQ F (OR (member 'F ONLYS :test #'eq)
                                    (member 'FULL ONLYS :test #'eq)))
                        (SETQ A (OR F (member 'A ONLYS :test #'eq)
                                    (member 'ARGS ONLYS :test #'eq)))
                        (SETQ V (OR F (member 'V ONLYS :test #'eq)
                                    (member 'VALUE ONLYS :test #'eq)))
                        (SETQ C (OR F (member 'C ONLYS :test #'eq)
                                    (member 'CALLER ONLYS :test #'eq)))
                        (SETQ NL
                              (if A '(#\9)
                                  (mapcan #'(lambda (X)
                                              (if (AND (INTEGERP X)
                                                       (> X 0)
                                                       (< X 9))
                                                  (LIST (FETCHCHAR (STRINGIMAGE X) 0))))
                                          onlys)))
                        (if (NOT (OR A V C NL))
                            (if Caller (return "119") (return "019")))
                        (SETQ NL (APPEND NL '(\0)))
                        (SETQ BUF (GETSTR 12))
                        (SUFFIX (if (or C Caller) #\1 #\0) BUF)
                        (SUFFIX (if V #\1 #\0) BUF)
                        (if A (suffix #\9 BUF)
                            (mapcar #'(lambda (x) (suffix x BUF)) NL))
                        (RETURN BUF))))
        (/MONITOR FN TRACECODE BEFORE AFTER CONDITION TIMERNAM
                  COUNTNAM TRACENAME BREAK )))

(DEFUN OPTIONS2UC (L)
  (COND ((NOT L) NIL)
        ((ATOM (CAR L))
         (|spadThrowBrightly|
           (format nil "~A has wrong format for an option" (car L))))
        ((CONS (CONS (UPCASE (CAAR L)) (CDAR L)) (OPTIONS2UC (CDR L))))))

(DEFUN COND-UCASE (X) (COND ((INTEGERP X) X) ((UPCASE X))))

(DEFUN TRACEOPTIONS (X)
  (COND ((NOT X) NIL)
        ((EQ (CAR X) '/) X)
        ((TRACEOPTIONS (CDR X)))))

(defmacro |/untrace| (&rest L) `', (/UNTRACE-0 L))

(defmacro /UNTRACE (&rest L) `', (/UNTRACE-0 L))

(DEFUN /UNTRACE-0 (L)
    (PROG (OPTIONL OPTIONS FNL)
      (SETQ OPTIONL (/OPTIONS L))
      (SETQ FNL (TRUNCLIST L OPTIONL))
      (SETQ OPTIONS (if OPTIONL (CAR OPTIONL)))
      (RETURN (/UNTRACE-1 FNL OPTIONS))))

(defun /UNTRACE-1 (L OPTIONS)
  (cond
    ((NOT L)
     (if (ATOM /TRACENAMES)
         NIL
         (mapcar #'(lambda (u) (/UNTRACE-2 (/UNTRACE-REDUCE U) OPTIONS))
                 (APPEND /TRACENAMES NIL))))
    ((mapcar #'(lambda (x) (/UNTRACE-2 X OPTIONS)) L)))
  (/TRACEREPLY))

(DEFUN /UNTRACE-REDUCE (X) (if (ATOM X) X (first X))) ; (CAR X) is now a domain

(DEFUN /UNTRACE-2 (X OPTIONS)
 (let (u y)
  (COND ((AND (|isFunctor| X) (ATOM X))
         (|untraceDomainConstructor| X))
        ((OR (|isDomainOrPackage| (SETQ U X))
             (and (symbolp X) (boundp X)
                  (|isDomain| (SETQ U (EVAL X)))))
         (|spadUntrace| U OPTIONS))
        ((EQCAR OPTIONS 'ALIAS)
           (if |$traceNoisely|
               (|sayBrightly| (LIST '|%b| (CADR OPTIONS) '|%d| '**untraced)))
           (SETQ /TIMERLIST
                 (REMOVE (STRINGIMAGE (CADR OPTIONS)) /TIMERLIST :test 'equal))
           (SETQ /COUNTLIST
                 (REMOVE (STRINGIMAGE (CADR OPTIONS)) /COUNTLIST :test 'equal))
           (SETQ |$mathTraceList|
                 (REMOVE (CADR OPTIONS) |$mathTraceList| :test 'equal))
           (UNEMBED X))
        ((AND (NOT (MEMBER X /TRACENAMES))
              (NOT (|isSubForRedundantMapName| X)))
         (|sayBrightly|
           (LIST
             '|%b|
             (|rassocSub| X |$mapSubNameAlist|)
             '|%d|
             "not traced")))
        (T (SETQ /TRACENAMES (REMOVE X /TRACENAMES :test 'equal))
           (SETQ |$mathTraceList|
                 (REMOVE (if (STRINGP X) (INTERN X) X) |$mathTraceList|))
           (SETQ |$letAssoc| (DELASC X |$letAssoc|))
           (setq Y (if (IS_GENVAR X) (|devaluate| (EVAL X)) X))
           (SETQ /TIMERLIST (REMOVE (STRINGIMAGE Y) /TIMERLIST :test 'equal))
           (SET (INTERN (STRCONC Y ",TIMER")) 0)
           (SETQ /COUNTLIST (REMOVE (STRINGIMAGE Y) /COUNTLIST :test 'equal))
           (SET (INTERN (STRCONC Y ",COUNT")) 0)
           (COND ((AND |$traceNoisely| (NOT (|isSubForRedundantMapName| Y)))
                  (|sayBrightly|
                    (LIST '|%b| (|rassocSub| Y |$mapSubNameAlist|)
                  '|%d| "untraced"))))
           (UNEMBED X)))))

(DEFUN MONITOR-PRINVALUE (VAL NAME)
  (let (u)
    (COND ((setq U (GET NAME '/TRANSFORM))
           (COND
             ((EQCAR U '&)
              (PRINC "//" CURSTRM) (PRIN1 VAL CURSTRM) (TERPRI CURSTRM))
             (T (PRINC "! " CURSTRM)
                (PRIN1 (EVAL (SUBST (MKQ VAL) '* (CAR U))) CURSTRM)
                (TERPRI CURSTRM)) ))
          (T
           (PRINC ": " CURSTRM)
           (COND ((NOT (SMALL-ENOUGH VAL)) (|F,PRINT-ONE| VAL CURSTRM))
                 (/PRETTY (PRETTYPRINT VAL CURSTRM))
                 (T (COND (|$mathTrace| (TERPRI)))
                    (PRINMATHOR0 VAL CURSTRM)))))))

(DEFUN MONITOR-BLANKS (N) (PRINC (MAKE-FULL-CVEC N " ") CURSTRM))

(DEFUN MONITOR-EVALBEFORE (X) (EVALFUN (MONITOR-EVALTRAN X NIL)) X)

(DEFUN MONITOR-EVALAFTER (X) (EVALFUN (MONITOR-EVALTRAN X 'T)))

(DEFUN MONITOR-EVALTRAN (X FG)
  (if (HAS_SHARP_VAR X) (MONITOR-EVALTRAN1 X FG) X))

(define-function 'MONITOR\,EVALTRAN #'MONITOR-EVALTRAN)

(DEFUN MONITOR-EVALTRAN1 (X FG)
  (let (n)
    (COND
      ((SETQ N (|isSharpVarWithNum| X)) (MONITOR-GETVALUE N FG))
      ((ATOM X) X)
      ((CONS (MONITOR-EVALTRAN1 (CAR X) FG)
             (MONITOR-EVALTRAN1 (CDR X) FG))))))

(DEFUN HAS_SHARP_VAR (X)
  (COND ((AND (ATOM X) (IS_SHARP_VAR X)) 'T)
        ((ATOM X) NIL)
        ((OR (HAS_SHARP_VAR (CAR X)) (HAS_SHARP_VAR (CDR X))))))

(DEFUN IS_SHARP_VAR (X)
  (AND (IDENTP X)
       (EQL (ELT (PNAME X) 0) #\#)
       (INTEGERP (parse-integer (symbol-name X) :start 1))))

(DEFUN MONITOR-GETVALUE (N FG)
  (COND ((= N 0)
         (if FG
             (MKQ /VALUE)
             (|spadThrowBrightly| "cannot ask for value before execution")))
        ((= N 9) (MKQ /CALLER))
        ((<= N (SIZE /ARGS)) (MKQ (ELT /ARGS (1- N))))
        ((|spadThrowBrightly| (LIST 'function '|%b| /NAME '|%d|
                              "does not have" '|%b| N '|%d| "arguments")))))

(DEFUN MONITOR-PRINARGS (L CODE /TRANSFORM)
  (let (N)
    (cond
      ((= (digit-char-p (elt CODE 2)) 0) NIL)
      ((= (digit-char-p (elt CODE 2)) 9)
       (cond
         (/TRANSFORM
           (mapcar
             #'(lambda (x y)
                 (COND ((EQ Y '*)
                        (PRINC "\\ " CURSTRM)
                        (MONITOR-PRINT X CURSTRM))
                       ((EQ Y '&)
                        (PRINC "\\\\" CURSTRM)
                        (TERPRI CURSTRM)
                        (PRINT X CURSTRM))
                       ((NOT Y) (PRINC "! " CURSTRM))
                       (T
                        (PRINC "! " CURSTRM)
                        (MONITOR-PRINT
                          (EVAL (SUBST (MKQ X) '* Y)) CURSTRM))))
            L (cdr /transform)))
         (T (PRINC ": " CURSTRM)
            (COND ((NOT (ATOM L))
                   (if |$mathTrace| (TERPRI CURSTRM))
                   (MONITOR-PRINT (CAR L) CURSTRM) (SETQ L (CDR L))))
            (mapcar #'monitor-printrest L))))
      ((do ((istep 2 (+ istep 1))
            (k (maxindex code)))
           ((> istep k) nil)
        (when (not (= 0 (SETQ N (digit-char-p (elt CODE ISTEP)))))
         (PRINC "\\" CURSTRM)
         (PRINMATHOR0 N CURSTRM)
         (PRINC ": " CURSTRM)
         (MONITOR-PRINARGS-1 L N)))))))

(DEFUN MONITOR-PRINTREST (X)
  (COND ((NOT (SMALL-ENOUGH X))
         (PROGN (TERPRI)
                (MONITOR-BLANKS (1+ /DEPTH))
                (PRINC "\\" CURSTRM)
                (PRINT X CURSTRM)))
        ((PROGN (if (NOT |$mathTrace|) (PRINC "\\" CURSTRM))
                (COND (/PRETTY (PRETTYPRINT X CURSTRM))
                      ((PRINMATHOR0 X CURSTRM)))))))

(DEFUN MONITOR-PRINARGS-1 (L N)
  (COND ((OR (ATOM L) (LESSP N 1)) NIL)
        ((EQ N 1) (MONITOR-PRINT (CAR L) CURSTRM))
        ((MONITOR-PRINARGS-1 (CDR L) (1- N)))))

(DEFUN MONITOR-PRINT (X CURSTRM)
  (COND ((NOT (SMALL-ENOUGH X)) (|F,PRINT-ONE| X CURSTRM))
        (/PRETTY (PRETTYPRINT X CURSTRM))
        ((PRINMATHOR0 X CURSTRM))))

(DEFUN PRINMATHOR0 (X CURSTRM)
  (if |$mathTrace| (|maprinSpecial| (|outputTran| X) /DEPTH 80)
      (PRIN0 X CURSTRM)))

(DEFUN SMALL-ENOUGH (X) (if /TRACESIZE (SMALL-ENOUGH-COUNT X 0 /TRACESIZE) t))

(DEFUN SMALL-ENOUGH-COUNT (X N M)
  "Returns number if number of nodes < M otherwise nil."
  (COND ((< M N) NIL)
        ((VECP X)
         (do ((i 0 (1+ i)) (k (maxindex x)))
             ((> i k) n)
           (if (NOT (SETQ N (SMALL-ENOUGH-COUNT (ELT X I) (1+ N) M)))
               (RETURN NIL))))
        ((ATOM X) N)
        ((AND (SETQ N (SMALL-ENOUGH-COUNT (CAR X) (1+ N) M))
              (SMALL-ENOUGH-COUNT (CDR X) N M)))))

(DEFUN /OPTIONS (X)
  (COND ((ATOM X) NIL)
        ((OR (ATOM (CAR X)) (|isFunctor| (CAAR X))) (/OPTIONS (CDR X)))
        (X)))

(DEFUN /GETOPTION (L OPT) (KDR (/GETTRACEOPTIONS L OPT)))

(DEFUN /GETTRACEOPTIONS (L OPT)
  (COND ((ATOM L) NIL)
        ((EQ (KAR (CAR L)) OPT) (CAR L))
        ((/GETTRACEOPTIONS (CDR L) OPT))))

(defmacro /TRACE-LET (A B)
  `(PROG1 (SPADLET ,A ,B)
          . ,(mapcar #'(lambda (x) `(/tracelet-print ',x ,x))
                     (if (ATOM A) (LIST A) A))))

(defun /TRACELET-PRINT (X Y &AUX (/PRETTY 'T))
  (PRINC (STRCONC (PNAME X) ": ") *terminal-io*)
  (MONITOR-PRINT Y *terminal-io*))

(defun /MONITOR ;;(&rest G5)
  (G1 TRACECODE BEFORE AFTER CONDITION TIMERNAM COUNTNAM TRACENAME BREAK)
  (PROG () ;; (G1 G4 TRACECODE BEFORE AFTER CONDITION
         ;; TIMERNAM COUNTNAM TRACENAME BREAK)
        ;; (dcq (G1 TRACECODE BEFORE AFTER CONDITION TIMERNAM COUNTNAM TRACENAME BREAK) G5)
        (SETQ G4 (macro-function G1))
        (SETQ TRACECODE (OR TRACECODE "119"))
        (if COUNTNAM (SET COUNTNAM 0))
        (if TIMERNAM (SET TIMERNAM 0))
        (EMBED
          G1
          (LIST
            (if G4 'MLAMBDA 'LAMBDA)
            '(&rest G6)
            (LIST
              '/MONITORX
              (QUOTE G6)
              G1
              (LIST
                'QUOTE
                (LIST
                  TRACENAME (if G4 'MACRO) TRACECODE
                  COUNTNAM TIMERNAM BEFORE AFTER
                  CONDITION BREAK |$tracedModemap| ''T)))))
        (RETURN G1)))

(defun /MONITORX (/ARGS FUNCT OPTS &AUX NAME TYPE TRACECODE COUNTNAM TIMERNAM
                        BEFORE AFTER CONDITION BREAK TRACEDMODEMAP
                        BREAKCONDITION)
            (declare (special /ARGS))
  (let ((x opts))
       (setf NAME (ifcar x)              x (ifcdr x))
       (setf TYPE (ifcar x)              x (ifcdr x))
       (setf TRACECODE (ifcar x)         x (ifcdr x))
       (setf COUNTNAM (ifcar x)          x (ifcdr x))
       (setf TIMERNAM (ifcar x)          x (ifcdr x))
       (setf BEFORE (ifcar x)            x (ifcdr x))
       (setf AFTER (ifcar x)             x (ifcdr x))
       (setf CONDITION (ifcar x)         x (ifcdr x))
       (setf BREAK (ifcar x)             x (ifcdr x))
       (setf TRACEDMODEMAP (ifcar x)     x (ifcdr x))
       (setf BREAKCONDITION (ifcar x)    x (ifcdr x)))

  (|stopTimer|)
  (PROG (C V A NAME1 CURSTRM EVAL_TIME INIT_TIME NOT_TOP_LEVEL
         (/DEPTH (if (and (BOUNDP '/DEPTH) (numberp /depth)) (1+ /DEPTH) 1))
         (|depthAlist| (if (BOUNDP '|depthAlist|) (COPY-TREE |depthAlist|) NIL))
         FUNDEPTH NAMEID YES (|$tracedSpadModemap| TRACEDMODEMAP) (|$mathTrace| NIL)
         /caller /name /value /breakcondition curdepth)
    (declare (special curstrm /depth fundepth |$tracedSpadModemap| |$mathTrace|
                      /caller /name /value /breakcondition |depthAlist|))
        (SETQ /NAME NAME)
        (SETQ NAME1 (PNAME (|rassocSub| (INTERN NAME) |$mapSubNameAlist|)))
        (SETQ /BREAKCONDITION BREAKCONDITION)
        (SETQ /CALLER (|rassocSub| (WHOCALLED 6) |$mapSubNameAlist|))
        (if (NOT (STRINGP TRACECODE))
            (MOAN "set TRACECODE to \'1911\' and restart"))
        (SETQ C (digit-char-p (elt TRACECODE 0))
              V (digit-char-p (elt TRACECODE 1))
              A (digit-char-p (elt TRACECODE 2)))
        (if COUNTNAM (SET COUNTNAM (1+ (EVAL COUNTNAM))))
        (SETQ NAMEID (INTERN NAME))
        (SETQ NOT_TOP_LEVEL (ASSOC NAMEID |depthAlist| :test #'eq))
        (if (NOT NOT_TOP_LEVEL)
            (SETQ |depthAlist| (CONS (CONS NAMEID 1) |depthAlist|))
            (RPLACD NOT_TOP_LEVEL (1+ (CDR NOT_TOP_LEVEL))))
        (SETQ FUNDEPTH (CDR (ASSOC NAMEID |depthAlist| :test #'eq)))
        (SETQ CONDITION (MONITOR-EVALTRAN CONDITION NIL))
        (SETQ YES (EVALFUN CONDITION))
        (if (member NAMEID |$mathTraceList| :test #'eq)
            (SETQ |$mathTrace| T))
        (if (AND YES |$TraceFlag|)
            (PROG (|$TraceFlag|)
                  (SETQ CURSTRM *TERMINAL-IO*)
                  (if (EQUAL TRACECODE "000") (RETURN NIL))
                  (TAB 0 CURSTRM)
                  (MONITOR-BLANKS (1- /DEPTH))
                  (PRIN0 FUNDEPTH CURSTRM)
                  (|sayBrightlyNT| (LIST "<enter" '|%b|
                                         NAME1 '|%d|) CURSTRM)
                  (COND ((EQ 0 C) NIL)
                        ((EQ TYPE 'MACRO)
                         (PRINT " expanded" CURSTRM))
                        (T (PRINT " from " CURSTRM)
                           (PRIN0 /CALLER CURSTRM)))
                  (MONITOR-PRINARGS
                    (if (SPADSYSNAMEP NAME)
                        (NREVERSE (REVERSE  (|coerceTraceArgs2E|
                                              (INTERN NAME1)
                                              (INTERN NAME)
                                              /ARGS)))
                        (|coerceTraceArgs2E| (INTERN NAME1)
                                             (INTERN NAME) /ARGS))
                    TRACECODE
                    (GET (INTERN NAME) '/TRANSFORM))
                  (if (NOT |$mathTrace|) (TERPRI CURSTRM))))
        (if before (MONITOR-EVALBEFORE BEFORE))
        (if (member '|before| BREAK :test #'eq)
            (|break| (LIST "Break on entering" '|%b| NAME1 '|%d| ":")))
        (if TIMERNAM (SETQ INIT_TIME (|startTimer|)))
        (SETQ /VALUE (if (EQ TYPE 'MACRO) (macroexpand FUNCT /ARGS)
                         (APPLY FUNCT /ARGS)))
        (|stopTimer|)
        (if TIMERNAM (SETQ EVAL_TIME (- (|clock|) INIT_TIME)) )
        (if (AND TIMERNAM (NOT NOT_TOP_LEVEL))
            (SET TIMERNAM (+ (EVAL TIMERNAM) EVAL_TIME)))
        (if AFTER (MONITOR-EVALAFTER AFTER))
        (if (AND YES |$TraceFlag|)
            (PROG (|$TraceFlag|)
                  (if (EQUAL TRACECODE "000") (GO SKIP))
                  (TAB 0 CURSTRM)
                  (MONITOR-BLANKS (1- /DEPTH))
                  (PRIN0 FUNDEPTH CURSTRM)
                  (|sayBrightlyNT| (LIST ">exit " '|%b| NAME1 '|%d|) CURSTRM)
                  (COND (TIMERNAM
                         (|sayBrightlyNT| '\( CURSTRM)
                         (|sayBrightlyNT| (/ EVAL_TIME 60.0) CURSTRM)
                         (|sayBrightlyNT| '\ sec\) CURSTRM) ))
                  (if (EQ 1 V)
                      (MONITOR-PRINVALUE
                        (|coerceTraceFunValue2E|
                          (INTERN NAME1) (INTERN NAME) /VALUE)
                        (INTERN NAME1)))
                  (if (NOT |$mathTrace|) (TERPRI CURSTRM))
               SKIP))
        (if (member '|after| BREAK :test #'eq)
            (|break| (LIST "Break on exiting" '|%b| NAME1 '|%d| ":")))
        (|startTimer|)
        (RETURN /VALUE)))

; Functions to run a timer for tracing
; It avoids timing the tracing function itself by turning the timer
; on and off

(defparameter $delay 0)

(defun |startTimer| ()
    (SETQ $delay (PLUS $delay (DIFFERENCE (TEMPUS-FUGIT) |$oldTime|)))
    (SETQ |$timerOn| 'T)
    (|clock|))

(defun |stopTimer| () (SETQ |$oldTime| (TEMPUS-FUGIT) |$timerOn| NIL) (|clock|))

(defun |clock| ()
  (if |$timerOn| (- (TEMPUS-FUGIT) $delay) (- |$oldTime| $delay)))

; Functions to trace/untrace a BPI; use as follows:
; To trace a BPI-value <bpi>, evaluate (SETQ <name> (BPITRACE <bpi>))
; To later untrace <bpi>, evaluate (BPITRACE <name>)

(defun PAIRTRACE (PAIR ALIAS)
   (RPLACA PAIR (BPITRACE (CAR PAIR) ALIAS )) NIL)

(defun BPITRACE (BPI ALIAS &optional OPTIONS)
  (SETQ NEWNAME (GENSYM))
  (IF (identp bpi) (setq bpi (symbol-function bpi)))
  (SET NEWNAME BPI)
  (SETF (symbol-function NEWNAME) BPI)
  (/TRACE-0 (APPEND (LIST NEWNAME (LIST 'ALIAS ALIAS)) OPTIONS))
  NEWNAME)

(defun BPIUNTRACE (X ALIAS) (/UNTRACE-0 (LIST X (LIST 'ALIAS ALIAS))))

(defun SPADSYSNAMEP (STR)
  (let (n i j)
    (AND (SETQ N (MAXINDEX STR))
         (SETQ I (position #\. STR :start 1))
         (SETQ J (position #\, STR :start (1+ I)))
         (do ((k (1+ j) (1+ k)))
             ((> k n) t)
           (if (not (digitp (elt str k))) (return nil))))))

; **********************************************************************
;            Utility functions for Tracing Package
; **********************************************************************

(MAKEPROP '|coerce| '/TRANSFORM '(& & *))
(MAKEPROP '|comp| '/TRANSFORM '(& * * &))
(MAKEPROP '|compIf| '/TRANSFORM '(& * * &))

;  by having no transform for the 3rd argument, it is simply not printed

(MAKEPROP '|compFormWithModemap| '/TRANSFORM '(& * * & *))

(defun UNVEC (X)
  (COND ((REFVECP X) (CONS '$ (VEC_TO_TREE X)))
        ((ATOM X) X)
        ((CONS (UNVEC (CAR X)) (UNVEC (CDR X))))))

(defun DROPENV (X) (AND X (LIST (CAR X) (CADR X))))

(defun SHOWBIND (E)
  (do ((v e (cdr v))
       (llev 1 (1+ llev)))
      ((not v))
    (PRINT (LIST "LAMBDA LEVEL" LLEV))
    (do ((w (car v) (cdr w))
         (clev 1 (1+ clev)))
        ((not w))
      (PRINT (LIST "CONTOUR LEVEL" CLEV))
      (PRINT (mapcar #'car (car W))))))


;;; A "resumable" break loop for use in trace etc. Unfortunately this
;;; only worked for CCL. We need to define a Common Lisp version. For
;;; now the function is defined but does nothing.
(defun interrupt (&rest ignore))
