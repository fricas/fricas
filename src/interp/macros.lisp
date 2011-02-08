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
                             ((GT I (MAXINDEX U)) 'T)
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
      ((OR (IVECP COMPERAND-1) (RVECP COMPERAND-1)) (BREAK))
      ((OR (IVECP COMPERAND-2) (RVECP COMPERAND-2)) (BREAK))
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
      ((MBPIP COMPERAND-1) (BREAK))
      ((MBPIP COMPERAND-2) (BREAK))
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
      ((OR (IVECP COMPERAND-1) (RVECP COMPERAND-1)) (BREAK))
      ((OR (IVECP COMPERAND-2) (RVECP COMPERAND-2)) (BREAK))
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
      ((MBPIP COMPERAND-1) (BREAK))
      ((MBPIP COMPERAND-2 (BREAK)))
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

(defmacro boot-equal (a b)
   (cond ((ident-char-lit a)
           `(or (eql ,a ,b) (eql (character ,a) ,b)))
         ((ident-char-lit b)
           `(or (eql ,a ,b) (eql ,a (character ,b))))
         (t `(eqqual ,a ,b))))

(defun ident-char-lit (x)
   (and (eqcar x 'quote) (identp (cadr x)) (= (length (pname (cadr x))) 1)))

(defmacro EQQUAL (a b)
  (cond ((OR (EQUABLE a) (EQUABLE b)) `(eq ,a ,b))
        ((OR (numberp a) (numberp b)) `(eql ,a ,b))
        (t  `(equal ,a ,b))))

(defmacro NEQUAL (a b) `(not (BOOT-EQUAL ,a ,b)))

(defun EQUABLE (X)
  (OR (NULL X) (AND (EQCAR X 'QUOTE) (symbolp (CADR X)))))

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

(defmacro REPEAT (&rest L)
  (let ((U (REPEAT-TRAN L NIL))) (-REPEAT (CDR U) (CAR U))))

(defun REPEAT-TRAN (L LP)
  (COND ((ATOM L) (ERROR "REPEAT FORMAT ERROR"))
        ((MEMBER (KAR (KAR L))
                 '(EXIT RESET IN ON GSTEP ISTEP STEP GENERAL UNTIL WHILE SUCHTHAT EXIT))
         (REPEAT-TRAN (CDR L) (CONS (CAR L) LP)))
        ((CONS (NREVERSE LP) (MKPF L 'PROGN)))))

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

(defun -REPEAT (BD SPL)
  (let (u g g1 inc final xcl xv il rsl tll funPLUS funGT fun? funIdent
        funPLUSform funGTform)
    (DO ((X SPL (CDR X)))
        ((ATOM X)
         (|expandDO| (LIST (NREVERSE IL) (LIST (MKPF (NREVERSE XCL) 'OR) XV)
               (SEQOPT (CONS 'SEQ (NCONC (NREVERSE RSL) (LIST (LIST 'EXIT BD))))))))
      (COND ((ATOM (CAR X)) (FAIL)))
      (COND ((AND (EQ (CAAR X) 'STEP)
                  (|member| (CADDAR X) '(2 1 0 (|One|) (|Zero|)))
                  (|member| (CADR (CDDAR X)) '(1 (|One|))))
             (SETQ X (CONS (CONS 'ISTEP (CDAR X)) (CDR X))) ))
                        ; A hack to increase the likelihood of small integers
      (SETQ U (CDAR X))
      (case (CAAR X)
        (GENERAL (AND (CDDDR U) (PUSH (CADDDR U) XCL))
                 (PUSH (LIST (CAR U) (CADR U) (CADDR U)) IL) )
        (GSTEP
          (SETQ tll (CDDDDR U))  ;tll is (+fun >fun type? ident)
          (SETQ funPLUSform (CAR tll))
          (SETQ funGTform   (CAR (SETQ tll (QCDR tll))))
          (PUSH (LIST (SETQ funPLUS (GENSYM)) funPLUSform) IL)
          (PUSH (LIST (SETQ funGT   (GENSYM)) funGTform) IL)
          (COND ((SETQ tll (CDR tll))
            (SETQ fun?     (CAR tll))
            (SETQ funIdent (CAR (SETQ tll (QCDR tll))))))
          (IF (NOT (ATOM (SETQ inc (CADDR U)) ))
              (PUSH (LIST (SETQ inc (GENSYM)) (CADDR U)) IL))
          (SETQ final (CADDDR U))
          (COND (final
             (COND ((ATOM final))
                   ((PUSH (LIST (SETQ final (GENSYM)) (CADDDR U)) IL)))
                 ; If CADDDR U is not an atom, only compute the value once
             (PUSH
                (if fun?
                      (if (FUNCALL fun? INC)
                          (if  (FUNCALL (EVAL funGTform) INC funIdent)
                               (LIST 'FUNCALL funGT (CAR U) FINAL)
                               (LIST 'FUNCALL funGT FINAL (CAR U)))
                           (LIST 'IF (LIST 'FUNCALL funGT INC funIdent)
                                     (LIST 'FUNCALL funGT (CAR U) FINAL)
                                     (LIST 'FUNCALL funGT FINAL  (CAR U))))
                       (LIST 'FUNCALL funGT (CAR U) final))
                     XCL)))
          (PUSH (LIST (CAR U) (CADR U) (LIST 'FUNCALL funPLUS (CAR U) INC)) IL))
        (STEP
          (IF (NOT (ATOM (SETQ inc (CADDR U)) ))
              (PUSH (LIST (SETQ inc (GENSYM)) (CADDR U)) IL))
          (COND ((CDDDR U)
                 (COND ((ATOM (SETQ final (CADDDR U)) ))
                       ((PUSH (LIST (SETQ final (GENSYM)) (CADDDR U)) IL)))
                 ; If CADDDR U is not an atom, only compute the value once
                 (PUSH
                   (if (INTEGERP INC)
                       (LIST (if  (MINUSP INC) '< '>) (CAR U) FINAL)
                     `(if (MINUSP ,INC)
                          (< ,(CAR U) ,FINAL)
                        (> ,(CAR U) ,FINAL)))
                       XCL)))
          (PUSH (LIST (CAR U) (CADR U) (LIST '+ (CAR U) INC)) IL))
        (ISTEP
          (IF (NOT (ATOM (SETQ inc (CADDR U)) ))
              (PUSH (LIST (SETQ inc (GENSYM)) (CADDR U)) IL))
          (COND ((CDDDR U)
                 (COND ((ATOM (SETQ final (CADDDR U)) ))
                       ((PUSH (LIST (SETQ final (GENSYM)) (CADDDR U)) IL)))
                     ; If CADDDR U is not an atom, only compute the value once
                 (PUSH
                   (if (INTEGERP INC)
                       (LIST (if  (QSMINUSP INC) 'QSLESSP 'QSGREATERP)
                             (CAR U) FINAL)
                     `(if (QSMINUSP ,INC)
                          (QSLESSP ,(CAR U) ,FINAL)
                        (QSGREATERP ,(CAR U) ,FINAL)))
                       XCL)))
          (PUSH (LIST (CAR U) (CADR U)
                      (COND ((|member| INC '(1 (|One|)))
                             (MKQSADD1 (CAR U)))
                            ((LIST 'QSPLUS (CAR U) INC)) ))
                IL))
        (ON (PUSH (LIST 'ATOM (CAR U)) XCL)
            (PUSH (LIST (CAR U) (CADR U) (LIST 'CDR (CAR U))) IL))
        (RESET (PUSH (LIST 'PROGN (CAR U) NIL) XCL))
        (IN
          (PUSH (LIST 'OR
                      (LIST 'ATOM (SETQ G (GENSYM)))
                      (CONS 'PROGN
                            (CONS
                              (LIST 'SETQ (CAR U) (LIST 'CAR G))
                              (APPEND
                                (COND ((AND (symbolp (car U))
                                            (symbol-package (car U))
                                             $TRACELETFLAG)
                                       (LIST (LIST '/TRACELET-PRINT (CAR U)
                                                   (CAR U))))
                                      (NIL))
                                (LIST NIL))))  ) XCL)
          (PUSH (LIST G (CADR U) (LIST 'CDR G)) IL)
          (PUSH (LIST (CAR U) NIL) IL))
        (INDOM (SETQ G (GENSYM))
               (SETQ G1 (GENSYM))
               (PUSH (LIST 'ATOM G) XCL)
               (PUSH (LIST G (LIST 'INDOM-FIRST (CADR U))
                           (LIST 'INDOM-NEXT G1)) IL)
               (PUSH (LIST (CAR U) NIL) IL)
               (PUSH (LIST G1 NIL) IL)
               (PUSH (LIST 'SETQ G1 (LIST 'CDR G)) RSL)
               (PUSH (LIST 'SETQ (CAR U) (LIST 'CAR G)) RSL))
        (UNTIL (SETQ G (GENSYM)) (PUSH (LIST G NIL (CAR U)) IL) (PUSH G XCL))
        (WHILE (PUSH (LIST 'NULL (CAR U)) XCL))
        (SUCHTHAT (SETQ BD (LIST 'COND (LIST (CAR U) BD))))
        (EXIT (SETQ XV (CAR U))) (FAIL)))))


(defun SEQOPT (U)
  (if (AND (EQCAR U 'SEQ) (EQCAR (CADR U) 'EXIT) (EQCAR (CADADR U) 'SEQ))
      (CADADR U)
      U))

(defvar $BOOT NIL)

(defun |expandDO| (OL)
    (PROG (VARS L VL V U INITS U-VARS U-VALS ENDTEST EXITFORMS BODYFORMS)
         (if $BOOT (return (CONS 'DO OL)))
         (SETQ L  (copy-list OL))
         (if (OR (ATOM L) (ATOM (CDR L))) (GO BADO))
         (setq vl (POP L))
         (COND ((IDENTP VL)
                (SETQ VARS (LIST VL))
                (AND (OR (ATOM L)
                         (ATOM (progn (setq inits (POP L)) L))
                         (ATOM (progn (setq u-vals (pop L)) L)))
                     (GO BADO))
                (SETQ INITS (LIST INITS) U-VARS (LIST (CAR VARS)) U-VALS (LIST U-VALS))
                (setq endtest (POP L)))
               ((prog ()
                        (COND ((NULL VL) (GO TG5)) ((ATOM VL) (GO BADO)))
                 G180   (AND (NOT (PAIRP (SETQ V (CAR VL)))) (SETQ V (LIST V)))
                        (AND (NOT (IDENTP (CAR V))) (GO BADO))
                        (PUSH (CAR V) VARS)
                        (PUSH (COND ((PAIRP (CDR V)) (CADR V))) INITS)
                        (AND (PAIRP (CDR V))
                             (PAIRP (CDDR V))
                             (SEQ (PUSH (CAR V) U-VARS)
                                  (PUSH (CADDR V) U-VALS)))
                        (AND (PAIRP (progn (POP VL) VL)) (GO G180))
                    TG5 (setq exitforms (POP L))
                        (and (PAIRP EXITFORMS)
                             (progn (setq endtest (POP EXITFORMS)) exitforms)))))
         (AND L
           (COND ((CDR L) (SETQ BODYFORMS (CONS 'SEQ L)))
                 ((NULL (EQCAR (CAR L) 'SEQ)) (SETQ BODYFORMS (CONS 'SEQ L)))
                 ((SETQ BODYFORMS (CAR L)))))
         (SETQ EXITFORMS `(EXIT ,(MKPF EXITFORMS 'PROGN)))
         (AND ENDTEST (SETQ ENDTEST (LIST 'COND (LIST ENDTEST '(GO G191)))))
         (COND ((NULL U-VARS) (GO XT) )
               ((NULL (CDR U-VARS))
                (SEQ (SETQ U-VARS (LIST 'SETQ (CAR U-VARS) (CAR U-VALS)))
                     (GO XT)) ))
         (SETQ VL (LIST 'SETQ (CAR U-VARS) (CAR U-VALS)))
         (SEQ (SETQ V (CDR U-VARS)) (SETQ U (CDR U-VALS)))
     TG  (SETQ VL (LIST 'SETQ (CAR V) (LIST 'PROG1 (CAR U) VL)))
         (POP U)
         (AND (progn (POP V) V)  (GO TG))
         (SETQ U-VARS VL)
     XT  (RETURN (COND
           ((NULL $BOOT)
             (CONS 'SEQ (NCONC (DO_LET VARS INITS)
               (LIST 'G190 ENDTEST BODYFORMS U-VARS '(GO G190)
                'G191 EXITFORMS))))
           ((CONS `(LAMBDA ,(NRECONC VARS NIL)
                     (SEQ G190 ,ENDTEST ,BODYFORMS ,U-VARS (GO G190) G191 ,EXITFORMS))
                  (NRECONC INITS NIL)))))
   BADO  (ERROR (FORMAT NIL "BAD DO FORMAT~%~A" OL))))

(defun DO_LET (VARS INITS)
  (if (OR (NULL VARS) (NULL INITS)) NIL
      (CONS (LIST 'SPADLET (CAR VARS) (CAR INITS))
           (DO_LET (CDR VARS) (CDR INITS)))))

(defun NREVERSE0 (X)
  "Returns LST, reversed. The argument is modified.
This version is needed so that (COLLECT (IN X Y) ... (RETURN 'JUNK))=>JUNK."
 (if (ATOM X) X (NREVERSE X)))

; 7.8.4 Mapping

(defmacro COLLECT (&rest L)
    (let ((U (REPEAT-TRAN L NIL)))
        (-REDUCE 'CONS NIL (CDR U) (CAR U))))

(defmacro COLLECTVEC (&rest L)
   `(COLLECTV ,@L))

(defmacro COLLECTV (&rest L)
  (PROG (CONDS BODY ANS COUNTER X Y)
         ;If we can work out how often we will go round
         ;allocate a vector first
    (SETQ CONDS NIL)
    (SETQ BODY (REVERSE L))
    (SETQ ANS (GENSYM))
    (SETQ COUNTER NIL)
    (SETQ X (CDR BODY))
    (SETQ BODY (CAR BODY))
LP  (COND ((NULL X)
            (COND ((NULL COUNTER)
                    (SETQ COUNTER (GENSYM))
                    (SETQ L (CONS (LIST 'ISTEP COUNTER 0 1) L)) ))
            (RETURN (LIST 'PROGN
                          (LIST 'SPADLET ANS
                                     (LIST 'GETREFV
                                           (COND ((NULL CONDS) (fail))
                                                 ((NULL (CDR CONDS))
                                                   (CAR CONDS))
                                                   ((CONS 'MIN CONDS)) ) ))
                          (CONS 'REPEAT (NCONC (CDR (REVERSE L))
                                        (LIST (LIST 'SETELT ANS COUNTER BODY))))
                          ANS)) ))
    (SETQ Y (CAR X))
    (SETQ X (CDR X))
    (COND ((MEMQ (CAR Y) '(SUCHTHAT WHILE UNTIL))
                (RETURN (LIST 'LIST2VEC (CONS 'COLLECT L)) ))
          ((member (CAR Y) '(IN ON) :test #'eq)
            (SETQ CONDS (CONS (LIST 'SIZE (CADDR Y)) CONDS))
            (GO LP))
          ((member (CAR Y) '(STEP ISTEP) :test #'eq)
            (if (AND (EQL (CADDR Y) 0) (EQL (CADDDR Y) 1))
                (SETQ COUNTER (CADR Y)) )
            (COND ((CDDDDR Y)    ; there may not be a limit
                   (SETQ CONDS (CONS
                                 (COND ((EQL 1 (CADDDR Y))
                                        (COND ((EQL 1 (CADDR Y)) (CAR (CDDDDR Y)))
                                              ((EQL 0 (CADDR Y)) (MKQSADD1 (CAR (CDDDDR Y))))
                                              ((MKQSADD1 `(- ,(CAR (CDDDDR Y)) ,(CADDR Y))))))
                                       ((EQL 1 (CADDR Y)) `(/ ,(CAR (CDDDDR Y)) ,(CADDR Y)))
                                       ((EQL 0 (CADDR Y))
                                        `(/ ,(MKQSADD1 (CAR (CDDDDR Y))) ,(CADDR Y)))
                                       (`(/ (- ,(MKQSADD1 (CAR (CDDDDR Y))) ,(CADDR Y))
                                            ,(CADDR Y))))
                                 CONDS))))
            (GO LP)))
  (ERROR "Cannot handle macro expansion")))

(defun MKQSADD1 (X)
  (COND ((ATOM X) `(QSADD1 ,X))
        ((AND (member (CAR X) '(-DIFFERENCE QSDIFFERENCE -) :test #'eq)
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

; 12.3 Comparisons on Numbers

(defmacro IEQUAL (&rest L) `(eql . ,L))
(defmacro GE (&rest L) `(>= . ,L))
(defmacro GT (&rest L) `(> . ,L))
(defmacro LE (&rest L) `(<= . ,L))
(defmacro LT (&rest L) `(< . ,L))

; 12.4 Arithmetic Operations

(defmacro SPADDIFFERENCE (&rest x) `(- . ,x))

; 12.6 Small Finite Field ops with vector trimming

;; following macros assume 0 <= x,y < z

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

(defun |expandSPADREDUCE| (op bod)
    (if (not $BOOT) (BREAK))
    (REDUCE-1 op bod))

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

(MAKEPROP 'CONS 'RIGHT-ASSOCIATIVE T)

(defun REDUCE-1 (OP BOD)
  (let (u op1 tran iden)
    (SEQ (SETQ OP1 (cond ((EQ OP '\,) (BREAK) 'CONS)
                         ((EQCAR OP 'QUOTE) (CADR OP))
                         (OP)))
         (SETQ IDEN (if (SETQ U (GET OP1 'THETA)) (CAR U) 'NO_THETA_PROPERTY))
         (SETQ TRAN (if (EQCAR BOD 'COLLECT)
                        (PROG (L BOD1 ITL)
                              (SETQ L (REVERSE (CDR BOD)))
                              (SETQ BOD1 (CAR L))
                              (SETQ ITL (NREVERSE (CDR L)))
                              (RETURN (-REDUCE OP1 IDEN BOD1 ITL)) )
                        (BREAK)
                               ))
         TRAN)))

(defun -REDUCE (OP Y BODY SPL)
  (PROG (X G AUX EXIT VALUE PRESET CONSCODE RESETCODE)
   (SETQ G (GENSYM))
   ; create preset of accumulate
   (SETQ PRESET (COND
      ((EQ Y 'NO_THETA_PROPERTY) (LIST 'SPADLET G (MKQ G)))
      ((LIST 'SPADLET G Y)) ))
   (SETQ EXIT (COND
      ((SETQ X (ASSOC 'EXIT SPL))(SETQ SPL (DELASC 'EXIT SPL)) (COND
         ((MEMBER OP '(AND OR)) (LIST 'AND G (CADR X))) ((CADR X)) ))
      ((EQ Y 'NO_THETA_PROPERTY) (LIST 'THETACHECK G (MKQ G)(MKQ OP)))
      (G) ))
   (COND ((EQ OP 'CONS) (SETQ EXIT (LIST 'NREVERSE0 EXIT))))
   ; CONSCODE= code which conses a member onto the list
   (SETQ VALUE (COND ((EQ Y 'NO_THETA_PROPERTY) (GENSYM))
                     (BODY)))
   (SETQ CONSCODE (CONS OP (COND
      ((FLAGP OP 'RIGHT-ASSOCIATIVE) (LIST VALUE G))
      ((LIST G VALUE) ) ) ) )
   ; next reset code which varies if THETA property is|/is not given
   (SETQ RESETCODE (LIST 'SETQ G (COND
      ((EQ Y 'NO_THETA_PROPERTY)
         (LIST 'COND (LIST (LIST 'EQ G (MKQ G)) VALUE)
                     (LIST ''T CONSCODE)) )
      (CONSCODE) )))
   ; create body
   (SETQ BODY (COND ((EQ VALUE BODY) RESETCODE)
                    ((LIST 'PROGN (LIST 'SPADLET VALUE BODY) RESETCODE)) ))
   (SETQ AUX (CONS (LIST 'EXIT EXIT) (COND
      ((EQ OP 'AND) (LIST (LIST 'UNTIL (LIST 'NULL G))))
      ((EQ OP 'OR) (LIST (LIST 'UNTIL G)))
      (NIL) )))
   (RETURN
      (LIST 'PROGN PRESET
         (CONS 'REPEAT (APPEND AUX (APPEND SPL (LIST BODY))) ))
      )))

(defun THETACHECK (VAL VAR OP) (if (EQL VAL VAR) (THETA_ERROR OP) val))

(defun THETA_ERROR (OP)
  (Boot::|userError|
        (LIST "Sorry, do not know the identity element for " OP)))

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

(defmacro |Record| (&rest x)
  `(|Record0| (LIST ,@(COLLECT (IN Y X)
                         (list 'CONS (MKQ (CADR Y)) (CADDR Y))))))

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

(defmacro |symbolis?| (x)
   `(eq (current-symbol) ,x))

(defmacro |matchsymbol| (x)
   `(if (|symbolis?| ,x)
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

; part of the old spad to new spad translator
; these are here because they need to be in depsys
; they were in nspadaux.lisp

(defmacro wi (a b) b)

(defmacro |try| (X)
  `(LET ((|$autoLine|))
        (declare (special |$autoLine|))
        (|tryToFit| (|saveState|) ,X)))

(defmacro |embrace| (X) `(|wrapBraces| (|saveC|) ,X (|restoreC|)))
(defmacro |indentNB| (X) `(|wrapBraces| (|saveD|) ,X (|restoreD|)))

(defmacro |tryBreak| (a b c d)
; Try to format <a b> by:
; (1) with no line breaking ($autoLine = nil)
; (2) with possible line breaks within a;
; (3) otherwise use a brace
  `(LET
    ((state))
    (setq state (|saveState| 't))
    (or
      (LET ((|$autoLine|))
         (declare (special |$autoLine|))
         (and ,a (|formatRight| '|formatPreferPile| ,b ,c ,d)))
      (|restoreState| state)
      (and (eqcar ,b (quote seq))
               (|embrace| (and
                  ,a
                  (|formatLB|)
                  (|formatRight| '|formatPreferPile| ,b ,c ,d))))
      (|restoreState| state)
      (|embrace| (and ,a
                  (|formatLB|)
                  (|formatRight| '|formatPreferPile| ,b ,c ,d))))))

(defmacro |tryBreakNB| (a b c d)
; Try to format <a b> by:
; (1) with no line breaking ($autoLine = nil)
; (2) with possible line breaks within a;
; (3) otherwise display without a brace
  `(LET
    ((state))
    (setq state (|saveState| 't))
    (or
      (markhash ,b 0)
      (LET ((|$autoLine|))
         (declare (special |$autoLine|))
         (and ,a (|formatRight| '|formatPreferPile| ,b ,c ,d)))
      (|restoreState| state)
      (markhash ,b 1)
      (and (eqcar ,b (quote seq))
               (|embrace| (and
                  ,a
                  (|formatLB|)
                  (|formatRight| '|formatPreferPile| ,b ,c ,d))))
      (markhash ,b 2)
      (|restoreState| state)
      (|indentNB| (and ,a
                  (|formatRight| '|formatPreferPile| ,b ,c ,d)))
      (markhash ,b 3)

)))

(defun markhash (key n) (progn (cond
  ((equal n 3) (remhash key ht))
  ('t (hput ht key n)) ) nil))
