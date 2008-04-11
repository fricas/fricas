;;; -*- Mode:Lisp; Package:Boot  -*-


(IN-PACKAGE "BOOT" )


(DEFPARAMETER |tmptok| NIL)
(DEFPARAMETER TOK NIL)
(DEFPARAMETER |ParseMode| NIL)
(DEFPARAMETER DEFINITION_NAME NIL)
(DEFPARAMETER LABLASOC NIL)


(DEFUN |PARSE-NewExpr| ()
  (AND (ACTION (SETQ DEFINITION_NAME (CURRENT-SYMBOL)))
       (|PARSE-Statement|))) 


(DEFUN |PARSE-Statement| ()
  (AND (|PARSE-Expr| 0)
       (OPTIONAL
           (AND (STAR REPEATOR
                      (AND (MATCH-ADVANCE-STRING ",")
                           (MUST (|PARSE-Expr| 0))))
                (PUSH-REDUCTION '|PARSE-Statement|
                    (CONS '|Series|
                          (CONS (POP-STACK-2)
                                (APPEND (POP-STACK-1) NIL)))))))) 


(DEFUN |PARSE-InfixWith| ()
  (AND (|PARSE-With|)
       (PUSH-REDUCTION '|PARSE-InfixWith|
           (CONS '|Join| (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-With| ()
  (AND (MATCH-ADVANCE-STRING "with") (MUST (|PARSE-Category|))
       (PUSH-REDUCTION '|PARSE-With|
           (CONS '|with| (CONS (POP-STACK-1) NIL))))) 


(DEFUN |PARSE-Category| ()
  (PROG (G1)
    (RETURN
      (OR (AND (MATCH-ADVANCE-STRING "if") (MUST (|PARSE-Expression|))
               (MUST (MATCH-ADVANCE-STRING "then"))
               (MUST (|PARSE-Category|))
               (BANG FIL_TEST
                     (OPTIONAL
                         (AND (MATCH-ADVANCE-STRING "else")
                              (MUST (|PARSE-Category|)))))
               (PUSH-REDUCTION '|PARSE-Category|
                   (CONS '|if|
                         (CONS (POP-STACK-3)
                               (CONS (POP-STACK-2)
                                     (CONS (POP-STACK-1) NIL))))))
          (AND (MATCH-ADVANCE-STRING "(") (MUST (|PARSE-Category|))
               (BANG FIL_TEST
                     (OPTIONAL
                         (STAR REPEATOR
                               (AND (MATCH-ADVANCE-STRING ";")
                                    (MUST (|PARSE-Category|))))))
               (MUST (MATCH-ADVANCE-STRING ")"))
               (PUSH-REDUCTION '|PARSE-Category|
                   (CONS 'CATEGORY
                         (CONS (POP-STACK-2)
                               (APPEND (POP-STACK-1) NIL)))))
          (AND (ACTION (SETQ G1 (LINE-NUMBER CURRENT-LINE)))
               (|PARSE-Application|)
               (MUST (OR (AND (MATCH-ADVANCE-STRING ":")
                              (MUST (|PARSE-Expression|))
                              (PUSH-REDUCTION '|PARSE-Category|
                                  (CONS '|Signature|
                                        (CONS (POP-STACK-2)
                                         (CONS (POP-STACK-1) NIL))))
                              (ACTION (|recordSignatureDocumentation|
                                       (NTH-STACK 1) G1)))
                         (AND (PUSH-REDUCTION '|PARSE-Category|
                                  (CONS '|Attribute|
                                        (CONS (POP-STACK-1) NIL)))
                              (ACTION (|recordAttributeDocumentation|
                                       (NTH-STACK 1) G1)))))))))) 


(DEFUN |PARSE-Expression| ()
  (AND (|PARSE-Expr|
           (|PARSE-rightBindingPowerOf| (MAKE-SYMBOL-OF PRIOR-TOKEN)
               |ParseMode|))
       (PUSH-REDUCTION '|PARSE-Expression| (POP-STACK-1)))) 


(DEFUN |PARSE-Import| ()
  (AND (MATCH-ADVANCE-STRING "import") (MUST (|PARSE-Expr| 1000))
       (BANG FIL_TEST
             (OPTIONAL
                 (STAR REPEATOR
                       (AND (MATCH-ADVANCE-STRING ",")
                            (MUST (|PARSE-Expr| 1000))))))
       (PUSH-REDUCTION '|PARSE-Import|
           (CONS '|import|
                 (CONS (POP-STACK-2) (APPEND (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Infix| ()
  (AND (PUSH-REDUCTION '|PARSE-Infix| (CURRENT-SYMBOL))
       (ACTION (ADVANCE-TOKEN)) (OPTIONAL (|PARSE-TokTail|))
       (MUST (|PARSE-Expression|))
       (PUSH-REDUCTION '|PARSE-Infix|
           (CONS (POP-STACK-2)
                 (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Prefix| ()
  (AND (PUSH-REDUCTION '|PARSE-Prefix| (CURRENT-SYMBOL))
       (ACTION (ADVANCE-TOKEN)) (OPTIONAL (|PARSE-TokTail|))
       (MUST (|PARSE-Expression|))
       (PUSH-REDUCTION '|PARSE-Prefix|
           (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))) 


(DEFUN |PARSE-Suffix| ()
  (AND (PUSH-REDUCTION '|PARSE-Suffix| (CURRENT-SYMBOL))
       (ACTION (ADVANCE-TOKEN)) (OPTIONAL (|PARSE-TokTail|))
       (PUSH-REDUCTION '|PARSE-Suffix|
           (CONS (POP-STACK-1) (CONS (POP-STACK-1) NIL))))) 


(DEFUN |PARSE-TokTail| ()
  (PROG (G1)
    (RETURN
      (AND (NULL $BOOT) (EQ (CURRENT-SYMBOL) '$)
           (OR (ALPHA-CHAR-P (CURRENT-CHAR))
               (CHAR-EQ (CURRENT-CHAR) "$")
               (CHAR-EQ (CURRENT-CHAR) "%")
               (CHAR-EQ (CURRENT-CHAR) "("))
           (ACTION (SETQ G1 (COPY-TOKEN PRIOR-TOKEN)))
           (|PARSE-Qualification|) (ACTION (SETQ PRIOR-TOKEN G1)))))) 


(DEFUN |PARSE-Qualification| ()
  (AND (MATCH-ADVANCE-STRING "$") (MUST (|PARSE-Primary1|))
       (PUSH-REDUCTION '|PARSE-Qualification|
           (|dollarTran| (POP-STACK-1) (POP-STACK-1))))) 


(DEFUN |PARSE-SemiColon| ()
  (AND (MATCH-ADVANCE-STRING ";")
       (MUST (OR (|PARSE-Expr| 82)
                 (PUSH-REDUCTION '|PARSE-SemiColon| '|/throwAway|)))
       (PUSH-REDUCTION '|PARSE-SemiColon|
           (CONS '|;| (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Return| ()
  (AND (MATCH-ADVANCE-STRING "return") (MUST (|PARSE-Expression|))
       (PUSH-REDUCTION '|PARSE-Return|
           (CONS '|return| (CONS (POP-STACK-1) NIL))))) 


(DEFUN |PARSE-Exit| ()
  (AND (MATCH-ADVANCE-STRING "exit")
       (MUST (OR (|PARSE-Expression|)
                 (PUSH-REDUCTION '|PARSE-Exit| '|$NoValue|)))
       (PUSH-REDUCTION '|PARSE-Exit|
           (CONS '|exit| (CONS (POP-STACK-1) NIL))))) 


(DEFUN |PARSE-Leave| ()
  (AND (MATCH-ADVANCE-STRING "leave")
       (MUST (OR (|PARSE-Expression|)
                 (PUSH-REDUCTION '|PARSE-Leave| '|$NoValue|)))
       (MUST (OR (AND (MATCH-ADVANCE-STRING "from")
                      (MUST (|PARSE-Label|))
                      (PUSH-REDUCTION '|PARSE-Leave|
                          (CONS '|leaveFrom|
                                (CONS (POP-STACK-1)
                                      (CONS (POP-STACK-1) NIL)))))
                 (PUSH-REDUCTION '|PARSE-Leave|
                     (CONS '|leave| (CONS (POP-STACK-1) NIL))))))) 


(DEFUN |PARSE-Seg| ()
  (AND (|PARSE-GliphTok| '|..|)
       (BANG FIL_TEST (OPTIONAL (|PARSE-Expression|)))
       (PUSH-REDUCTION '|PARSE-Seg|
           (CONS 'SEGMENT
                 (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Conditional| ()
  (AND (MATCH-ADVANCE-STRING "if") (MUST (|PARSE-Expression|))
       (MUST (MATCH-ADVANCE-STRING "then")) (MUST (|PARSE-Expression|))
       (BANG FIL_TEST
             (OPTIONAL
                 (AND (MATCH-ADVANCE-STRING "else")
                      (MUST (|PARSE-ElseClause|)))))
       (PUSH-REDUCTION '|PARSE-Conditional|
           (CONS '|if|
                 (CONS (POP-STACK-3)
                       (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))))) 


(DEFUN |PARSE-ElseClause| ()
  (OR (AND (EQ (CURRENT-SYMBOL) '|if|) (|PARSE-Conditional|))
      (|PARSE-Expression|))) 


(DEFUN |PARSE-Loop| ()
  (OR (AND (STAR REPEATOR (|PARSE-Iterator|))
           (MUST (MATCH-ADVANCE-STRING "repeat"))
           (MUST (|PARSE-Expr| 110))
           (PUSH-REDUCTION '|PARSE-Loop|
               (CONS 'REPEAT
                     (APPEND (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))
      (AND (MATCH-ADVANCE-STRING "repeat") (MUST (|PARSE-Expr| 110))
           (PUSH-REDUCTION '|PARSE-Loop|
               (CONS 'REPEAT (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Iterator| ()
  (OR (AND (MATCH-ADVANCE-STRING "for") (MUST (|PARSE-Primary|))
           (MUST (MATCH-ADVANCE-STRING "in"))
           (MUST (|PARSE-Expression|))
           (MUST (OR (AND (MATCH-ADVANCE-STRING "by")
                          (MUST (|PARSE-Expr| 200))
                          (PUSH-REDUCTION '|PARSE-Iterator|
                              (CONS 'INBY
                                    (CONS (POP-STACK-3)
                                     (CONS (POP-STACK-2)
                                      (CONS (POP-STACK-1) NIL))))))
                     (PUSH-REDUCTION '|PARSE-Iterator|
                         (CONS 'IN
                               (CONS (POP-STACK-2)
                                     (CONS (POP-STACK-1) NIL))))))
           (OPTIONAL
               (AND (MATCH-ADVANCE-STRING "|")
                    (MUST (|PARSE-Expr| 111))
                    (PUSH-REDUCTION '|PARSE-Iterator|
                        (CONS '|\|| (CONS (POP-STACK-1) NIL))))))
      (AND (MATCH-ADVANCE-STRING "while") (MUST (|PARSE-Expr| 190))
           (PUSH-REDUCTION '|PARSE-Iterator|
               (CONS 'WHILE (CONS (POP-STACK-1) NIL))))
      (AND (MATCH-ADVANCE-STRING "until") (MUST (|PARSE-Expr| 190))
           (PUSH-REDUCTION '|PARSE-Iterator|
               (CONS 'UNTIL (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Expr| (RBP)
  (DECLARE (SPECIAL RBP))
  (AND (|PARSE-NudPart| RBP)
       (OPTIONAL (STAR OPT_EXPR (|PARSE-LedPart| RBP)))
       (PUSH-REDUCTION '|PARSE-Expr| (POP-STACK-1)))) 


(DEFUN |PARSE-LabelExpr| ()
  (AND (|PARSE-Label|) (MUST (|PARSE-Expr| 120))
       (PUSH-REDUCTION '|PARSE-LabelExpr|
           (CONS 'LABEL (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Label| ()
  (AND (MATCH-ADVANCE-STRING "<<") (MUST (|PARSE-Name|))
       (MUST (MATCH-ADVANCE-STRING ">>")))) 


(DEFUN |PARSE-LedPart| (RBP)
  (DECLARE (SPECIAL RBP))
  (AND (|PARSE-Operation| '|Led| RBP)
       (PUSH-REDUCTION '|PARSE-LedPart| (POP-STACK-1)))) 


(DEFUN |PARSE-NudPart| (RBP)
  (DECLARE (SPECIAL RBP))
  (AND (OR (|PARSE-Operation| '|Nud| RBP) (|PARSE-Reduction|)
           (|PARSE-Form|))
       (PUSH-REDUCTION '|PARSE-NudPart| (POP-STACK-1)))) 


(DEFUN |PARSE-Operation| (|ParseMode| RBP)
  (DECLARE (SPECIAL |ParseMode| RBP))
  (AND (NOT (MATCH-CURRENT-TOKEN 'IDENTIFIER))
       (GETL (SETQ |tmptok| (CURRENT-SYMBOL)) |ParseMode|)
       (LT RBP (|PARSE-leftBindingPowerOf| |tmptok| |ParseMode|))
       (ACTION (SETQ RBP
                     (|PARSE-rightBindingPowerOf| |tmptok| |ParseMode|)))
       (|PARSE-getSemanticForm| |tmptok| |ParseMode|
           (ELEMN (GETL |tmptok| |ParseMode|) 5 NIL)))) 


(DEFUN |PARSE-leftBindingPowerOf| (X IND)
  (DECLARE (SPECIAL X IND))
  (LET ((Y (GETL X IND))) (IF Y (ELEMN Y 3 0) 0))) 


(DEFUN |PARSE-rightBindingPowerOf| (X IND)
  (DECLARE (SPECIAL X IND))
  (LET ((Y (GETL X IND))) (IF Y (ELEMN Y 4 105) 105))) 


(DEFUN |PARSE-getSemanticForm| (X IND Y)
  (DECLARE (SPECIAL X IND Y))
  (OR (AND Y (FUNCALL (CAR Y))) (AND (EQ IND '|Nud|) (|PARSE-Prefix|))
      (AND (EQ IND '|Led|) (|PARSE-Infix|)))) 


(DEFUN |PARSE-Reduction| ()
  (AND (|PARSE-ReductionOp|) (MUST (|PARSE-Expr| 1000))
       (PUSH-REDUCTION '|PARSE-Reduction|
           (CONS '|Reduce|
                 (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-ReductionOp| ()
  (AND (GETL (CURRENT-SYMBOL) '|Led|)
       (MATCH-NEXT-TOKEN 'SPECIAL-CHAR (CODE-CHAR 47))
       (PUSH-REDUCTION '|PARSE-ReductionOp| (CURRENT-SYMBOL))
       (ACTION (ADVANCE-TOKEN)) (ACTION (ADVANCE-TOKEN)))) 


(DEFUN |PARSE-Form| ()
  (OR (AND (MATCH-ADVANCE-STRING "iterate")
           (BANG FIL_TEST
                 (OPTIONAL
                     (AND (MATCH-ADVANCE-STRING "from")
                          (MUST (|PARSE-Label|))
                          (PUSH-REDUCTION '|PARSE-Form|
                              (CONS (POP-STACK-1) NIL)))))
           (PUSH-REDUCTION '|PARSE-Form|
               (CONS '|iterate| (APPEND (POP-STACK-1) NIL))))
      (AND (MATCH-ADVANCE-STRING "yield") (MUST (|PARSE-Application|))
           (PUSH-REDUCTION '|PARSE-Form|
               (CONS '|yield| (CONS (POP-STACK-1) NIL))))
      (|PARSE-Application|))) 


(DEFUN |PARSE-Application| ()
  (AND (|PARSE-Primary|) (OPTIONAL (STAR OPT_EXPR (|PARSE-Selector|)))
       (OPTIONAL
           (AND (|PARSE-Application|)
                (PUSH-REDUCTION '|PARSE-Application|
                    (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))))) 


(DEFUN |PARSE-Selector| ()
  (OR (AND NONBLANK (EQ (CURRENT-SYMBOL) '|.|)
           (CHAR-NE (CURRENT-CHAR) '| |) (MATCH-ADVANCE-STRING ".")
           (MUST (|PARSE-PrimaryNoFloat|))
           (MUST (OR (AND $BOOT
                          (PUSH-REDUCTION '|PARSE-Selector|
                              (CONS 'ELT
                                    (CONS (POP-STACK-2)
                                     (CONS (POP-STACK-1) NIL)))))
                     (PUSH-REDUCTION '|PARSE-Selector|
                         (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))))
      (AND (OR (|PARSE-Float|)
               (AND (MATCH-ADVANCE-STRING ".")
                    (MUST (|PARSE-Primary|))))
           (MUST (OR (AND $BOOT
                          (PUSH-REDUCTION '|PARSE-Selector|
                              (CONS 'ELT
                                    (CONS (POP-STACK-2)
                                     (CONS (POP-STACK-1) NIL)))))
                     (PUSH-REDUCTION '|PARSE-Selector|
                         (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))))) 


(DEFUN |PARSE-PrimaryNoFloat| ()
  (AND (|PARSE-Primary1|) (OPTIONAL (|PARSE-TokTail|)))) 


(DEFUN |PARSE-Primary| ()
  (OR (|PARSE-Float|) (|PARSE-PrimaryNoFloat|))) 


(DEFUN |PARSE-Primary1| ()
  (OR (AND (|PARSE-VarForm|)
           (OPTIONAL
               (AND NONBLANK (EQ (CURRENT-SYMBOL) '|(|)
                    (MUST (|PARSE-Enclosure|))
                    (PUSH-REDUCTION '|PARSE-Primary1|
                        (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))))
      (|PARSE-Quad|) (|PARSE-String|) (|PARSE-IntegerTok|)
      (|PARSE-FormalParameter|)
      (AND (MATCH-STRING "'")
           (MUST (OR (AND $BOOT (|PARSE-Data|))
                     (AND (MATCH-ADVANCE-STRING "'")
                          (MUST (|PARSE-Expr| 999))
                          (PUSH-REDUCTION '|PARSE-Primary1|
                              (CONS 'QUOTE (CONS (POP-STACK-1) NIL)))))))
      (|PARSE-Sequence|) (|PARSE-Enclosure|))) 


(DEFUN |PARSE-Float| ()
  (AND (|PARSE-FloatBase|)
       (MUST (OR (AND NONBLANK (|PARSE-FloatExponent|))
                 (PUSH-REDUCTION '|PARSE-Float| 0)))
       (PUSH-REDUCTION '|PARSE-Float|
           (MAKE-FLOAT (POP-STACK-4) (POP-STACK-2) (POP-STACK-2)
               (POP-STACK-1))))) 


(DEFUN |PARSE-FloatBase| ()
  (OR (AND (FIXP (CURRENT-SYMBOL)) (CHAR-EQ (CURRENT-CHAR) ".")
           (CHAR-NE (NEXT-CHAR) ".") (|PARSE-IntegerTok|)
           (MUST (|PARSE-FloatBasePart|)))
      (AND (FIXP (CURRENT-SYMBOL))
           (CHAR-EQ (CHAR-UPCASE (CURRENT-CHAR)) 'E)
           (|PARSE-IntegerTok|) (PUSH-REDUCTION '|PARSE-FloatBase| 0)
           (PUSH-REDUCTION '|PARSE-FloatBase| 0))
      (AND (DIGITP (CURRENT-CHAR)) (EQ (CURRENT-SYMBOL) '|.|)
           (PUSH-REDUCTION '|PARSE-FloatBase| 0)
           (|PARSE-FloatBasePart|)))) 


(DEFUN |PARSE-FloatBasePart| ()
  (AND (MATCH-ADVANCE-STRING ".")
       (MUST (OR (AND (DIGITP (CURRENT-CHAR))
                      (PUSH-REDUCTION '|PARSE-FloatBasePart|
                          (TOKEN-NONBLANK (CURRENT-TOKEN)))
                      (|PARSE-IntegerTok|))
                 (AND (PUSH-REDUCTION '|PARSE-FloatBasePart| 0)
                      (PUSH-REDUCTION '|PARSE-FloatBasePart| 0)))))) 


(DEFUN |PARSE-FloatExponent| ()
  (PROG (G1)
    (RETURN
      (OR (AND (MEMBER (CURRENT-SYMBOL) '(E |e|))
               (FIND (CURRENT-CHAR) "+-") (ACTION (ADVANCE-TOKEN))
               (MUST (OR (|PARSE-IntegerTok|)
                         (AND (MATCH-ADVANCE-STRING "+")
                              (MUST (|PARSE-IntegerTok|)))
                         (AND (MATCH-ADVANCE-STRING "-")
                              (MUST (|PARSE-IntegerTok|))
                              (PUSH-REDUCTION '|PARSE-FloatExponent|
                                  (MINUS (POP-STACK-1))))
                         (PUSH-REDUCTION '|PARSE-FloatExponent| 0))))
          (AND (IDENTP (CURRENT-SYMBOL))
               (SETQ G1 (FLOATEXPID (CURRENT-SYMBOL)))
               (ACTION (ADVANCE-TOKEN))
               (PUSH-REDUCTION '|PARSE-FloatExponent| G1)))))) 


(DEFUN |PARSE-Enclosure| ()
  (OR (AND (MATCH-ADVANCE-STRING "(")
           (MUST (OR (AND (|PARSE-Expr| 6)
                          (MUST (MATCH-ADVANCE-STRING ")")))
                     (AND (MATCH-ADVANCE-STRING ")")
                          (PUSH-REDUCTION '|PARSE-Enclosure|
                              (CONS '|Tuple| NIL))))))
      (AND (MATCH-ADVANCE-STRING "{")
           (MUST (OR (AND (|PARSE-Expr| 6)
                          (MUST (MATCH-ADVANCE-STRING "}"))
                          (PUSH-REDUCTION '|PARSE-Enclosure|
                              (CONS '|brace|
                                    (CONS
                                     (CONS '|construct|
                                      (CONS (POP-STACK-1) NIL))
                                     NIL))))
                     (AND (MATCH-ADVANCE-STRING "}")
                          (PUSH-REDUCTION '|PARSE-Enclosure|
                              (CONS '|brace| NIL)))))))) 


(DEFUN |PARSE-IntegerTok| () (PARSE-NUMBER)) 


(DEFUN |PARSE-FloatTok| ()
  (AND (PARSE-NUMBER)
       (PUSH-REDUCTION '|PARSE-FloatTok|
           (IF $BOOT (POP-STACK-1) (BFP- (POP-STACK-1)))))) 


(DEFUN |PARSE-FormalParameter| () (|PARSE-FormalParameterTok|)) 


(DEFUN |PARSE-FormalParameterTok| () (PARSE-ARGUMENT-DESIGNATOR)) 


(DEFUN |PARSE-Quad| ()
  (OR (AND (MATCH-ADVANCE-STRING "$")
           (PUSH-REDUCTION '|PARSE-Quad| '$))
      (AND $BOOT (|PARSE-GliphTok| '|.|)
           (PUSH-REDUCTION '|PARSE-Quad| '|.|)))) 


(DEFUN |PARSE-String| () (PARSE-SPADSTRING)) 


(DEFUN |PARSE-VarForm| ()
  (AND (|PARSE-Name|) (PUSH-REDUCTION '|PARSE-VarForm| (POP-STACK-1)))) 


(DEFUN |PARSE-Name| ()
  (AND (PARSE-IDENTIFIER) (PUSH-REDUCTION '|PARSE-Name| (POP-STACK-1)))) 


(DEFUN |PARSE-Data| ()
  (AND (ACTION (SETQ LABLASOC NIL)) (|PARSE-Sexpr|)
       (PUSH-REDUCTION '|PARSE-Data|
           (CONS 'QUOTE (CONS (TRANSLABEL (POP-STACK-1) LABLASOC) NIL))))) 


(DEFUN |PARSE-Sexpr| ()
  (AND (ACTION (ADVANCE-TOKEN)) (|PARSE-Sexpr1|))) 


(DEFUN |PARSE-Sexpr1| ()
  (OR (AND (|PARSE-AnyId|)
           (OPTIONAL
               (AND (|PARSE-NBGliphTok| '=) (MUST (|PARSE-Sexpr1|))
                    (ACTION (SETQ LABLASOC
                                  (CONS (CONS (POP-STACK-2)
                                         (NTH-STACK 1))
                                        LABLASOC))))))
      (AND (MATCH-ADVANCE-STRING "'") (MUST (|PARSE-Sexpr1|))
           (PUSH-REDUCTION '|PARSE-Sexpr1|
               (CONS 'QUOTE (CONS (POP-STACK-1) NIL))))
      (|PARSE-IntegerTok|)
      (AND (MATCH-ADVANCE-STRING "-") (MUST (|PARSE-IntegerTok|))
           (PUSH-REDUCTION '|PARSE-Sexpr1| (MINUS (POP-STACK-1))))
      (|PARSE-String|)
      (AND (MATCH-ADVANCE-STRING "<")
           (BANG FIL_TEST (OPTIONAL (STAR REPEATOR (|PARSE-Sexpr1|))))
           (MUST (MATCH-ADVANCE-STRING ">"))
           (PUSH-REDUCTION '|PARSE-Sexpr1| (LIST2VEC (POP-STACK-1))))
      (AND (MATCH-ADVANCE-STRING "(")
           (BANG FIL_TEST
                 (OPTIONAL
                     (AND (STAR REPEATOR (|PARSE-Sexpr1|))
                          (OPTIONAL
                              (AND (|PARSE-GliphTok| '|.|)
                                   (MUST (|PARSE-Sexpr1|))
                                   (PUSH-REDUCTION '|PARSE-Sexpr1|
                                    (NCONC (POP-STACK-2) (POP-STACK-1))))))))
           (MUST (MATCH-ADVANCE-STRING ")"))))) 


(DEFUN |PARSE-NBGliphTok| (|tok|)
  (DECLARE (SPECIAL |tok|))
  (AND (MATCH-CURRENT-TOKEN 'GLIPH |tok|) NONBLANK
       (ACTION (ADVANCE-TOKEN)))) 


(DEFUN |PARSE-AnyId| ()
  (OR (PARSE-IDENTIFIER)
      (OR (AND (MATCH-STRING "$")
               (PUSH-REDUCTION '|PARSE-AnyId| (CURRENT-SYMBOL))
               (ACTION (ADVANCE-TOKEN)))
          (PARSE-KEYWORD)))) 


(DEFUN |PARSE-GliphTok| (|tok|)
  (DECLARE (SPECIAL |tok|))
  (AND (MATCH-CURRENT-TOKEN 'GLIPH |tok|) (ACTION (ADVANCE-TOKEN)))) 


(DEFUN |PARSE-Sequence| ()
  (OR (AND (|PARSE-OpenBracket|) (MUST (|PARSE-Sequence1|))
           (MUST (MATCH-ADVANCE-STRING "]")))
      (AND (|PARSE-OpenBrace|) (MUST (|PARSE-Sequence1|))
           (MUST (MATCH-ADVANCE-STRING "}"))
           (PUSH-REDUCTION '|PARSE-Sequence|
               (CONS '|brace| (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Sequence1| ()
  (AND (OR (AND (|PARSE-Expression|)
                (PUSH-REDUCTION '|PARSE-Sequence1|
                    (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))
           (PUSH-REDUCTION '|PARSE-Sequence1| (CONS (POP-STACK-1) NIL)))
       (OPTIONAL
           (AND (|PARSE-IteratorTail|)
                (PUSH-REDUCTION '|PARSE-Sequence1|
                    (CONS 'COLLECT
                          (APPEND (POP-STACK-1)
                                  (CONS (POP-STACK-1) NIL)))))))) 


(DEFUN |PARSE-OpenBracket| ()
  (PROG (G1)
    (RETURN
      (AND (EQ (|getToken| (SETQ G1 (CURRENT-SYMBOL))) '[)
           (MUST (OR (AND (EQCAR G1 '|elt|)
                          (PUSH-REDUCTION '|PARSE-OpenBracket|
                              (CONS '|elt|
                                    (CONS (CADR G1)
                                     (CONS '|construct| NIL)))))
                     (PUSH-REDUCTION '|PARSE-OpenBracket| '|construct|)))
           (ACTION (ADVANCE-TOKEN)))))) 


(DEFUN |PARSE-OpenBrace| ()
  (PROG (G1)
    (RETURN
      (AND (EQ (|getToken| (SETQ G1 (CURRENT-SYMBOL))) '{)
           (MUST (OR (AND (EQCAR G1 '|elt|)
                          (PUSH-REDUCTION '|PARSE-OpenBrace|
                              (CONS '|elt|
                                    (CONS (CADR G1)
                                     (CONS '|brace| NIL)))))
                     (PUSH-REDUCTION '|PARSE-OpenBrace| '|construct|)))
           (ACTION (ADVANCE-TOKEN)))))) 


(DEFUN |PARSE-IteratorTail| ()
  (OR (AND (MATCH-ADVANCE-STRING "repeat")
           (BANG FIL_TEST
                 (OPTIONAL (STAR REPEATOR (|PARSE-Iterator|)))))
      (STAR REPEATOR (|PARSE-Iterator|)))) 

