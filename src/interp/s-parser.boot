)package "BOOT"

DEFPARAMETER($ParseMode, NIL)
DEFPARAMETER($LABLASOC, NIL)


-- PURPOSE: This file sets up properties which are used by the Boot lexical
--          analyzer for bottom-up recognition of operators.  Also certain
--          other character-class definitions are included, as well as
--          table accessing functions.
--
-- 1. Led and Nud Tables
--
-- TABLE PURPOSE

-- Led and Nud have to do with operators. An operator with a Led property takes
-- an operand on its left (infix/suffix operator).

-- An operator with a Nud takes no operand on its left (prefix/nilfix).
-- Some have both (e.g. - ).  This terminology is from the Pratt parser.
-- The translator for Scratchpad II is a modification of the Pratt parser which
-- branches to special handlers when it is most convenient and practical to
-- do so (Pratt's scheme cannot handle local contexts very easily).

-- Both LEDs and NUDs have right and left binding powers.  This is meaningful
-- for prefix and infix operators.  These powers are stored as the values of
-- the LED and NUD properties of an atom, if the atom has such a property.
-- The format is:

--     <Operator Left-Binding-Power  Right-Binding-Power <Special-Handler>>

-- where the Special-Handler is the name of a function to be evaluated when
-- that keyword is encountered.

-- The default values of Left and Right Binding-Power are NIL.  NIL is a
-- legitimate value signifying no precedence.  If the Special-Handler is NIL,
-- this is just an ordinary operator (as opposed to a surfix operator like
-- if-then-else).

-- ** TABLE CREATION

MAKEOP(X, Y) ==
    if OR(NOT (CDR X), NUMBERP (SECOND X)) then
        X := CONS(FIRST X, X)
    MAKEPROP(FIRST X, Y, X)

init_parser_properties() ==
    for j in _
         [["*", 800, 801],   ["rem", 800, 801],   ["mod", 800, 801], _
          ["quo", 800, 801],   ["div", 800, 801], _
          ["/", 800, 801],    ["**", 900, 901],  ["^", 900, 901], _
          ["exquo", 800, 801], ["+", 700, 701], _
          ["-", 700, 701],    ["->", 1001, 1002],  ["<-", 1001, 1002], _
          [":", 996, 997],    ["::", 996, 997], _
          ["@", 996, 997],    ["pretend", 995, 996], _
          ["."],            ["!", 1002, 1001], _
          [",", 110, 111], _
          [";", 81, 82, ["parse__SemiColon"]], _
          ["<", 400, 400],    [">", 400, 400], _
          ["<<", 400, 400],  [">>", 400, 400], _
          ["<=", 400, 400],   [">=", 400, 400], _
          ["=", 400, 400],     ["^=", 400, 400], _
          ["~=", 400, 400], _
          ["in", 400, 400],    ["case", 400, 400], _
          ["add", 400, 120],   ["with", 2000, 400, ["parse__InfixWith"]], _
          ["has", 400, 400], _
          ["where", 121, 104], _
          ["when", 112, 190], _
          ["otherwise", 119, 190, ["parse__Suffix"]], _
          ["is", 400, 400],    ["isnt", 400, 400], _
          ["and", 250, 251],   ["or", 200, 201], _
          ["/\", 250, 251],   ["\/", 200, 201], _
          ["..", "SEGMENT", 401, 699, ["parse__Seg"]], _
          ["=>", 123, 103], _
          ["+->", 995, 112], _
          ["==", "DEF", 122, 121], _
          ["==>", "MDEF", 122, 121], _
          ["|", 108, 111], _
          [":=", "LET", 125, 124]] repeat
        MAKEOP(j, "Led")

    for j in _
         [["for", 130, 350, ["parse__Loop"]], _
          ["while", 130, 190, ["parse__Loop"]], _
          ["until", 130, 190, ["parse__Loop"]], _
          ["repeat", 130, 190, ["parse__Loop"]], _
          ["import", 120, 0, ["parse__Import"]], _
          ["unless"], _
          ["add", 900, 120], _
          ["with", 1000, 300, ["parse__With"]], _
          ["has", 400, 400], _
          ["-", 701, 700], _
          ["#", 999, 998], _
          ["!", 1002, 1001], _
          ["'", 999, 999, ["parse__Data"]], _
          ["<<", 122, 120, ["parse__LabelExpr"]], _
          [">>"], _
          ["->", 1001, 1002], _
          [":", 194, 195], _
          ["not", 260, 259, NIL], _
          ["~", 260, 259, nil], _
          ["=", 400, 700], _
          ["return", 202, 201, ["parse__Return"]], _
          ["leave", 202, 201, ["parse__Leave"]], _
          ["exit", 202, 201, ["parse__Exit"]], _
          ["from"], _
          ["iterate"], _
          ["yield"], _
          ["if", 130, 0, ["parse__Conditional"]], _
          ["|", 0, 190], _
          ["suchthat"], _
          ["then", 0, 114], _
          ["else", 0, 114]] repeat
        MAKEOP(j, "Nud")

init_parser_properties()

push_lform0(tag) ==
    push_reduction("dummy", tag)

push_form0(tag) ==
    push_reduction("dummy", [tag])

push_lform1(tag, arg1) ==
    push_reduction("dummy", [tag, :arg1])

push_form1(tag, arg1) ==
    push_reduction("dummy", [tag, arg1])

push_lform2(tag, arg1, arg2) ==
    push_reduction("dummy", [tag, arg1, :arg2])

push_form2(tag, arg1, arg2) ==
    push_reduction("dummy", [tag, arg1, arg2])

push_form3(tag, arg1, arg2, arg3) ==
    push_reduction("dummy", [tag, arg1, arg2, arg3])

parse__new__expr() == parse__Expr 0

parse__InfixWith() ==
    not(parse__With()) => nil
    push_form2("Join", pop_stack_2(), pop_stack_1())

parse__With() ==
    not(match_symbol "with") => nil
    MUST parse__Category()
    push_form1("with", pop_stack_1())

repetition(delimiter, fn) ==
    val := nil
    repeat
        if delimiter then
            if not(match_symbol(delimiter)) then return nil -- break loop
            MUST(FUNCALL fn)
        else
            if not(FUNCALL fn) then return nil -- break loop
        val := [pop_stack_1(), :val]
    val => push_lform0(nreverse(val))
    nil

-- category : if expression then category [else category]
--          | '(' category* ')'
--          | application [':' expression]
--          ;
parse__Category() ==
    match_symbol "if" =>
        MUST parse__Expression()
        cond := pop_stack_1()
        MUST match_symbol "then"
        MUST parse__Category()
        else_val :=
            match_symbol "else" =>
                MUST parse__Category()
                pop_stack_1()
            nil
        push_form3("if", cond, pop_stack_1(), else_val)
    match_symbol "(" =>
        MUST
            match_symbol ")" => push_form0("CATEGORY")
            MUST(parse__Category())
            tail_val :=
                repetition(";", FUNCTION parse__Category) => pop_stack_1()
                nil
            MUST match_symbol ")"
            push_lform2("CATEGORY", pop_stack_1(), tail_val)
    G1 := LINE_-NUMBER CURRENT_-LINE
    not(parse__Application()) => nil
    MUST
        OR(
              AND(match_symbol ":", MUST parse__Expression(),
                  push_form2("Signature", pop_stack_2(), pop_stack_1()),
                  ACTION recordSignatureDocumentation(NTH_-STACK 1, G1)),
              AND(push_form1("Attribute", pop_stack_1()),
                  ACTION recordAttributeDocumentation(NTH_-STACK 1, G1)))

parse__Expression() ==
    parse__Expr
     parse__rightBindingPowerOf(MAKE_-SYMBOL_-OF PRIOR_-TOKEN, $ParseMode)

parse__Expr1000() == parse__Expr 1000

-- import : 'import' expr_1000 [',' expr_1000]*
parse__Import() ==
    not(match_symbol "import") => nil
    MUST parse__Expr 1000
    tail_val :=
        repetition(",", FUNCTION parse__Expr1000) => pop_stack_1()
        nil
    push_lform2("import", pop_stack_1(), tail_val)

parse__Infix() ==
    push_reduction("parse__Infix", current_symbol())
    advance_token()
    parse__TokTail()
    MUST parse__Expression()
    push_reduction("parse__Infix",
                   [pop_stack_2(), pop_stack_2(), pop_stack_1()])

parse__Prefix() ==
    push_reduction("parse__Prefix", current_symbol())
    advance_token()
    parse__TokTail()
    MUST parse__Expression()
    push_reduction("parse__Prefix", [pop_stack_2(), pop_stack_1()])

parse__Suffix() ==
    push_reduction("parse__Suffix", current_symbol())
    advance_token()
    parse__TokTail()
    push_reduction("parse__Suffix", [pop_stack_1(), pop_stack_1()])

parse__TokTail() ==
    $BOOT or current_symbol() ~= "$" => nil
    not(OR(MATCH_-NEXT_-TOKEN("IDENTIFIER", NIL), next_symbol() = "%",
           next_symbol() = "(")) => nil                     -- )
    G1 := COPY_-TOKEN PRIOR_-TOKEN
    not(parse__Qualification()) => nil
    SETF(PRIOR_-TOKEN, G1)

parse__Qualification() ==
    not(match_symbol "$") => nil
    MUST parse__Primary1()
    push_reduction("parse__Qualification",
                   dollarTran(pop_stack_1(), pop_stack_1()))

parse__SemiColon() ==
    not(match_symbol ";") => nil
    OR(parse__Expr 82,
       push_reduction("parse__SemiColon", "/throwAway"))
    push_form2(";", pop_stack_2(), pop_stack_1())

parse__Return() ==
    not(match_symbol "return") => nil
    MUST parse__Expression()
    push_form1("return", pop_stack_1())

parse__Exit() ==
    not(match_symbol "exit") => nil
    OR(parse__Expression(), push_reduction("parse__Exit", "$NoValue"))
    push_form1("exit", pop_stack_1())

parse__Leave() ==
    not(match_symbol "leave") => nil
    OR(parse__Expression(), push_reduction("parse__Leave", "$NoValue"))
    match_symbol "from" =>
        MUST parse__Label()
        push_form2("leaveFrom", pop_stack_1(), pop_stack_1())
    push_form1("leave", pop_stack_1())

parse__Seg() ==
    not(parse__GliphTok "..") => nil
    right_val :=
        parse__Expression() => pop_stack_1()
        nil
    push_form2("SEGMENT", pop_stack_1(), right_val)

parse__Conditional() ==
    not(match_symbol "if") => nil
    MUST parse__Expression()
    MUST match_symbol "then"
    MUST parse__Expression()
    else_val :=
        match_symbol "else" =>
            MUST parse__ElseClause()
            pop_stack_1()
        nil
    push_form3("if", pop_stack_2(), pop_stack_1(), else_val)

parse__ElseClause() ==
    current_symbol() = "if" => parse__Conditional()
    parse__Expression()

parse__Loop() ==
    OR(AND(repetition(nil, FUNCTION parse__Iterator),
           MUST match_symbol "repeat", MUST parse__Expr 110,
           push_lform1("REPEAT", [:pop_stack_2(), pop_stack_1()])),
       AND(match_symbol "repeat", MUST parse__Expr 110,
           push_form1("REPEAT", pop_stack_1())))

parse__Iterator() ==
    match_symbol "for" =>
        MUST parse__Primary()
        MUST match_symbol "in"
        MUST parse__Expression()
        by_val :=
              AND(match_symbol "by", MUST parse__Expr 200) => pop_stack_1()
              nil
        bar_val :=
            AND(match_symbol "|", MUST parse__Expr 111) => pop_stack_1()
            nil
        in_val := pop_stack_1()
        if bar_val then
            in_val := ["|", in_val, bar_val]
        if by_val then
            push_form3("INBY", pop_stack_1(), in_val, by_val)
        else
            push_form2("IN", pop_stack_1(), in_val)
    match_symbol "while" =>
        MUST parse__Expr 190
        push_form1("WHILE", pop_stack_1())
    match_symbol "until" =>
        MUST parse__Expr 190
        push_form1("UNTIL", pop_stack_1())
    nil

parse__Expr($RBP) ==
    not(parse__NudPart($RBP)) => nil
    while parse__LedPart($RBP) repeat nil
    push_reduction("parse__Expr", pop_stack_1())

parse__LabelExpr() ==
    not(parse__Label()) => nil
    MUST parse__Expr(120)
    push_form2("LABEL", pop_stack_2(), pop_stack_1())

parse__Label() ==
    not(match_symbol "<<") => nil
    MUST parse__Name()
    MUST match_symbol ">>"

parse__LedPart($RBP) ==
    not(parse__Operation("Led", $RBP)) => nil
    push_reduction("parse__LedPart", pop_stack_1())

parse__NudPart($RBP) ==
    AND(OR(parse__Operation("Nud", $RBP), parse__Reduction(), parse__Form()),
        push_reduction("parse__NudPart", pop_stack_1()))

parse__Operation($ParseMode, $RBP) ==
    match_current_token("IDENTIFIER", NIL) => nil
    GETL(tmptok := current_symbol(), $ParseMode) and
      $RBP < parse__leftBindingPowerOf(tmptok, $ParseMode) =>
        $RBP := parse__rightBindingPowerOf(tmptok, $ParseMode)
        parse__getSemanticForm($ParseMode,
                               ELEMN(GETL(tmptok, $ParseMode), 5, NIL))

parse__leftBindingPowerOf(x, ind) ==
    (y := GETL(x, ind)) => ELEMN(y, 3, 0)
    0

parse__rightBindingPowerOf(x, ind) ==
    (y := GETL(x, ind)) => ELEMN(y, 4, 105)
    105

parse__getSemanticForm(ind, y) ==
    AND(y, FUNCALL(CAR y)) => true
    ind = "Nud" => parse__Prefix()
    ind = "Led" => parse__Infix()
    nil

parse__Reduction() ==
    parse__ReductionOp() =>
        MUST parse__Expr 1000
        push_form2("Reduce", pop_stack_2(), pop_stack_1())
    nil

parse__ReductionOp() ==
    AND(GETL(current_symbol(), "Led"), MATCH_-NEXT_-TOKEN("KEYWORD", "/"),
        push_reduction("parse__ReductionOp", current_symbol()),
        ACTION advance_token(), ACTION advance_token())

parse__Form() ==
    match_symbol "iterate" =>
        from_val :=
            match_symbol "from" =>
                MUST parse__Label()
                [pop_stack_1()]
            nil
        push_lform1("iterate", from_val)
    match_symbol "yield" =>
        MUST parse__Application()
        push_form1("yield", pop_stack_1())
    parse__Application()

parse__Application() ==
    not(parse__Primary()) => nil
    while parse__Selector() repeat nil
    parse__Application() =>
        push_reduction("parse__Application", [pop_stack_2(), pop_stack_1()])
    true

parse__Selector() ==
    not(match_symbol ".") => nil
    MUST parse__Primary()
    $BOOT => push_form2("ELT", pop_stack_2(), pop_stack_1())
    push_reduction("parse__Selector",
                         [pop_stack_2(), pop_stack_1()])

parse__PrimaryNoFloat() ==
    AND(parse__Primary1(), OPTIONAL(parse__TokTail()))

parse__Primary() == OR(parse__Float(), parse__PrimaryNoFloat())

parse__Primary1() ==
    OR(
       AND(parse__VarForm(),
           OPTIONAL AND(
              NONBLANK, current_symbol() = "(", MUST parse__Enclosure(),
              push_reduction("parse__Primary1",
                             [pop_stack_2(), pop_stack_1()]))),
       parse__Quad(), parse__String(), parse__IntegerTok(),
       parse__FormalParameter(),
       AND(symbol_is? "'",
          MUST OR(
             AND($BOOT, parse__Data()),
             AND(match_symbol "'", MUST parse__Expr 999,
                 push_form1("QUOTE", pop_stack_1())))),
       parse__Sequence(), parse__Enclosure())

parse__Float() == parse__SPADFLOAT()

parse__Enclosure() ==
    match_symbol "(" =>
        MUST OR(  -- (
               AND(parse__Expr 6, MUST match_symbol ")"), -- (
               AND(match_symbol ")",
                   push_form0("@Tuple")))
    match_symbol "{" =>
           BREAK()
           MUST OR(
              AND(parse__Expr 6, MUST match_symbol "}",
                  push_form1("brace", ["construct", pop_stack_1()])),
              AND(match_symbol "}",
                  push_form0("brace")))
    nil

parse__IntegerTok() == parse__NUMBER()

parse__FormalParameter() == parse__FormalParameterTok()

parse__FormalParameterTok() == parse__ARGUMENT_-DESIGNATOR()

parse__Quad() ==
    OR(AND($BOOT, match_symbol "$", push_lform0("$")),
       AND($BOOT, parse__GliphTok("."), push_lform0(".")))

parse__String() == parse__SPADSTRING()

parse__VarForm() == parse__IDENTIFIER()

parse__Name() == parse__IDENTIFIER()

parse__Data() ==
    AND(ACTION ($LABLASOC := NIL), parse__Sexpr(),
        push_form1("QUOTE", TRANSLABEL(pop_stack_1(), $LABLASOC)))

parse__Sexpr() == AND(ACTION(advance_token()), parse__Sexpr1())

parse__Sexpr1() ==
    parse__AnyId() =>
        if parse__NBGliphTok "=" then
            MUST parse__Sexpr1()
            $LABLASOC := [[pop_stack_2(), :NTH_-STACK 1], :$LABLASOC]
        true
    match_symbol "'" =>
        MUST parse__Sexpr1()
        push_form1("QUOTE", pop_stack_1())
    parse__IntegerTok() => true
    match_symbol "-" =>
        MUST parse__IntegerTok()
        push_reduction("parse__Sexpr1", MINUS pop_stack_1())
    parse__String() => true
    match_symbol "<" =>
        seq_val :=
            repetition(nil, FUNCTION parse__Sexpr1) => pop_stack_1()
            nil
        MUST match_symbol ">"
        push_reduction("parse__Sexpr1", LIST2VEC(seq_val))
    match_symbol "(" =>
        if repetition(nil, FUNCTION parse__Sexpr1) then
            OPTIONAL AND(
                        parse__GliphTok ".", MUST parse__Sexpr1(),
                        push_reduction("parse__Sexpr1",
                                       NCONC(pop_stack_2(),
                                             pop_stack_1())))
        else
            push_reduction("parse__Sexpr1", nil)
        MUST match_symbol ")"
    nil

parse__NBGliphTok(tok) ==
   AND(match_current_token("KEYWORD", tok),
       NONBLANK,
       ACTION(advance_token()))

parse__AnyId() ==
    OR(parse__IDENTIFIER(),
       OR(AND(symbol_is? "$",
              push_reduction("parse__AnyId", current_symbol()),
              ACTION advance_token()),
        parse__AKEYWORD()))

parse__GliphTok(tok) ==
  AND(match_current_token('KEYWORD, tok), ACTION(advance_token()))

parse__Sequence() ==
    match_symbol "[" =>
        MUST(parse__Sequence1())
        MUST(match_symbol "]")
    match_symbol "{" =>
        MUST(parse__Sequence1())
        MUST(match_symbol "}")
        push_form1("brace", pop_stack_1())
    nil

parse__Sequence1() ==
    val :=
        parse__Expression() => [pop_stack_1()]
        nil
    push_reduction("parse__Sequence1", ["construct", :val])
    OPTIONAL
      AND(parse__IteratorTail(),
          push_lform1("COLLECT", [:pop_stack_1(),
                                       pop_stack_1()]))

-- IteratorTail : "repeat" [Iterator*] | [Iterator*]
parse__IteratorTail() ==
    match_symbol("repeat") =>
        repetition(nil, FUNCTION parse__Iterator) => true
        push_reduction("null__Tail", nil)
    repetition(nil, FUNCTION parse__Iterator)
