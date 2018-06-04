)package "BOOT"

DEFPARAMETER($ParseMode, NIL)
DEFPARAMETER($LABLASOC, NIL)
DEFVAR($NONBLANK, nil)


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
        X := CONS(first X, X)
    MAKEPROP(first X, Y, X)

init_parser_properties() ==
    for j in _
         [["*", 800, 801],   ["rem", 800, 801], _
          ["quo", 800, 801], _
          ["/", 800, 801],    ["**", 901, 900],  ["^", 901, 900], _
          ["exquo", 800, 801], ["+", 700, 701], _
          ["-", 700, 701],    ["->", 1002, 1001],  ["<-", 1001, 1002], _
          [":", 996, 997],    ["::", 996, 997], _
          ["@", 996, 997],    ["pretend", 995, 996], _
          ["."],            ["!", 1002, 1001], _
          [",", 110, 111], _
          [";", 81, 82, ["parse_SemiColon"]], _
          ["<", 400, 400],    [">", 400, 400], _
          ["<<", 400, 400],  [">>", 400, 400], _
          ["<=", 400, 400],   [">=", 400, 400], _
          ["=", 400, 400],     ["^=", 400, 400], _
          ["~=", 400, 400], _
          ["in", 400, 400],    ["case", 400, 400], _
          ["add", 400, 120],   ["with", 2000, 400, ["parse_InfixWith"]], _
          ["has", 400, 400], _
          ["where", 121, 104], _
          ["is", 400, 400],    ["isnt", 400, 400], _
          ["and", 250, 251],   ["or", 200, 201], _
          ["/\", 250, 251],   ["\/", 200, 201], _
          ["..", "SEGMENT", 401, 699, ["parse_Seg"]], _
          ["=>", 123, 103], _
          ["+->", 995, 112], _
          ["==", "DEF", 122, 121], _
          ["==>", "MDEF", 122, 121], _
          ["|", 108, 111], _
          [":=", "LET", 125, 124]] repeat
        MAKEOP(j, "Led")

    for j in _
         [["for", 130, 350, ["parse_Loop"]], _
          ["while", 130, 190, ["parse_Loop"]], _
          ["until", 130, 190, ["parse_Loop"]], _
          ["repeat", 130, 190, ["parse_Loop"]], _
          ["import", 120, 0, ["parse_Import"]], _
          ["add", 900, 120], _
          ["with", 1000, 300, ["parse_With"]], _
          ["has", 400, 400], _
          ["-", 701, 700], _
          ["#", 999, 998], _
          ["'", 999, 999, ["parse_Data"]], _
          ["->", 1001, 1002], _
          [":", 194, 195], _
          ["not", 260, 259, NIL], _
          ["~", 260, 259, nil], _
          ["=", 400, 700], _
          ["return", 202, 201, ["parse_Return"]], _
          ["from"], _
          ["iterate"], _
          ["yield"], _
          ["if", 130, 0, ["parse_Conditional"]], _
          ["try", 130, 0, ["parse_Try"]], _
          ["catch", 0, 114], _
          ["finally", 0, 114], _
          ["|", 0, 190], _
          ["then", 0, 114], _
          ["else", 0, 114]] repeat
        MAKEOP(j, "Nud")

init_parser_properties()

-- Parsing functions return true if successful or false if not.
-- If successful the result is left on the reduction stack.

-- Signal error if not successful.  Used for mandatory elements
-- in the grammar.
MUST(x) ==
    x => true
    spad_syntax_error(nil, nil)

-- Return successfuly regardless of status of x.  Used for
-- optional elements in the grammar.  Code matching 'x' must
-- preserve number of elements on the eduction stack.
OPTIONAL(x) == true

-- The same as OPTIONAL, but used for actions.
ACTION(x) == true

symbol_is?(x) == EQ(current_symbol(), x)

match_symbol(x) ==
    match_current_token("KEYWORD", x) => (advance_token(); true)
    false

expect_symbol(x) ==
    match_symbol(x) => true
    spad_syntax_error(x, nil)

DEFPARAMETER($reduction_stack, nil)

push_reduction(x, y) ==
    PUSH(y, $reduction_stack)
    true

pop_stack_1() == POP($reduction_stack)

pop_stack_2() ==
    el1 := POP($reduction_stack)
    el2 := POP($reduction_stack)
    PUSH(el1, $reduction_stack)
    el2

pop_stack_3() ==
    el1 := POP($reduction_stack)
    el2 := POP($reduction_stack)
    el3 := POP($reduction_stack)
    PUSH(el2, $reduction_stack)
    PUSH(el1, $reduction_stack)
    el3

top_of_stack() == first($reduction_stack)

parse_token(token) ==
    tok := match_current_token(token, nil)
    not(tok) => nil
    symbol := TOKEN_-SYMBOL(tok)
    push_reduction(token, COPY_-TREE(symbol))
    advance_token()
    true

parse_SPADSTRING() == parse_token("SPADSTRING")
parse_KEYWORD() == parse_token("KEYWORD")
parse_ARGUMENT_DESIGNATOR() == parse_token("ARGUMENT-DESIGNATOR")
parse_SPADFLOAT() == parse_token("SPADFLOAT")
parse_IDENTIFIER() == parse_token("IDENTIFIER")
parse_NUMBER() == parse_token("NUMBER")

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

dollarTran(dom, expr) ==
    expr is [fun, :args] =>
        [["Sel", dom, fun], :args]
    ["Sel", dom, expr]

parse_new_expr() ==
    $reduction_stack := nil
    parse_Expr 0

parse_InfixWith() ==
    not(parse_With()) => nil
    push_form2("Join", pop_stack_2(), pop_stack_1())

parse_With() ==
    not(match_symbol "with") => nil
    MUST parse_Category()
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

getSignatureDocumentation2(n1, n2) ==
    val1 := getSignatureDocumentation(n1) => val1
    not(n2) =>
        $COMBLOCKLIST is [[n, :val], :rr] and n1 <= n =>
            $COMBLOCKLIST := rr
            val
        nil
    nr := n2 + 1
    for pp in $COMBLOCKLIST repeat
        if pp is [n, :val] and n1 <= n and n <= n2 then
            nr := n
    nr <= n2 => getSignatureDocumentation(nr)
    nil

-- category : if expression then category [else category]
--          | '(' category* ')'
--          | application [':' expression]
--          ;

parse_category_list(closer) ==
    MUST
        match_symbol(closer) => push_form0("CATEGORY")
        MUST(parse_Category())
        tail_val :=
            repetition(";", FUNCTION parse_Category) => pop_stack_1()
            nil
        expect_symbol(closer)
        val1 := pop_stack_1()
        IFCAR(val1) = "if" and tail_val = nil => push_lform0(val1)
        push_lform2("CATEGORY", val1, tail_val)

parse_Category() ==
    match_symbol("if") =>
        MUST parse_Expression()
        cond := pop_stack_1()
        expect_symbol "then"
        MUST parse_Category()
        else_val :=
            match_symbol "else" =>
                MUST parse_Category()
                pop_stack_1()
            nil
        push_form3("if", cond, pop_stack_1(), else_val)
    match_symbol("(") => parse_category_list(")")
    match_symbol("{") => parse_category_list("}")
    match_symbol("SETTAB") => parse_category_list("BACKTAB")
    G1 := current_line_number()
    not(parse_Application()) => nil
    MUST
        OR(
              AND(match_symbol ":", MUST parse_Expression(),
                  push_form3("Signature", pop_stack_2(), pop_stack_1(),
                      getSignatureDocumentation2(G1, current_line_number()))),
              AND(push_form1("ATTRIBUTE", pop_stack_1()),
                  ACTION recordAttributeDocumentation(top_of_stack(), G1)))

parse_Expression() ==
    prior_sym := MAKE_-SYMBOL_-OF PRIOR_-TOKEN
    prior_sym :=
        SYMBOLP(prior_sym) => prior_sym
        nil
    parse_Expr
     parse_rightBindingPowerOf(prior_sym, $ParseMode)

parse_Expr1000() == parse_Expr 1000

-- import : 'import' expr_1000 [',' expr_1000]*
parse_Import() ==
    not(match_symbol "import") => nil
    match_symbol "from" or true
    MUST parse_Expr 1000
    tail_val :=
        repetition(",", FUNCTION parse_Expr1000) => pop_stack_1()
        nil
    push_lform2("import", pop_stack_1(), tail_val)

parse_Infix() ==
    push_reduction("parse_Infix", current_symbol())
    advance_token()
    parse_TokTail()
    MUST parse_Expression()
    push_reduction("parse_Infix",
                   [pop_stack_2(), pop_stack_2(), pop_stack_1()])

parse_Prefix() ==
    push_reduction("parse_Prefix", current_symbol())
    advance_token()
    parse_TokTail()
    MUST parse_Expression()
    push_reduction("parse_Prefix", [pop_stack_2(), pop_stack_1()])

parse_Suffix() ==
    push_reduction("parse_Suffix", current_symbol())
    advance_token()
    parse_TokTail()
    push_reduction("parse_Suffix", [pop_stack_1(), pop_stack_1()])

parse_TokTail() ==
    current_symbol() ~= "$" => nil
    not(OR(match_next_token("IDENTIFIER", NIL), next_symbol() = "%",
           next_symbol() = "(")) => nil                     -- )
    G1 := COPY_-TOKEN PRIOR_-TOKEN
    not(parse_Qualification()) => nil
    SETF(PRIOR_-TOKEN, G1)

parse_Qualification() ==
    not(match_symbol "$") => nil
    MUST parse_Primary1()
    push_reduction("parse_Qualification",
                   dollarTran(pop_stack_1(), pop_stack_1()))

parse_SemiColon() ==
    not(match_symbol ";") => nil
    parse_Expr 82 =>
        push_form2(";", pop_stack_2(), pop_stack_1())
    true

parse_Return() ==
    not(match_symbol "return") => nil
    MUST parse_Expression()
    push_form1("return", pop_stack_1())

parse_Seg() ==
    not(parse_GliphTok "..") => nil
    right_val :=
        parse_Expression() => pop_stack_1()
        nil
    push_form2("SEGMENT", pop_stack_1(), right_val)

parse_Conditional() ==
    not(match_symbol "if") => nil
    MUST parse_Expression()
    expect_symbol "then"
    MUST parse_Expression()
    else_val :=
        match_symbol "else" =>
            MUST parse_ElseClause()
            pop_stack_1()
        nil
    push_form3("if", pop_stack_2(), pop_stack_1(), else_val)

parse_ElseClause() ==
    current_symbol() = "if" => parse_Conditional()
    parse_Expression()

parse_Try() ==
    not(match_symbol "try") => nil
    MUST parse_Expression()
    expr := pop_stack_1()
    expr :=
        expr is [";", expr1, "/throwAway"] => expr1
        expr
    catcher := nil
    if match_symbol "catch" then
        MUST parse_Expression()
        catcher := pop_stack_1()
        MUST(catcher is ["in", var, expr])
    finalizer := nil
    if match_symbol "finally" then
        MUST parse_Expression()
        finalizer := pop_stack_1()
    MUST(catcher or finalizer)
    push_form3("try", expr, catcher, finalizer)

parse_Loop() ==
    OR(AND(repetition(nil, FUNCTION parse_Iterator),
           expect_symbol "repeat", MUST parse_Expr 110,
           push_lform1("REPEAT", [:pop_stack_2(), pop_stack_1()])),
       AND(expect_symbol "repeat", MUST parse_Expr 110,
           push_form1("REPEAT", pop_stack_1())))

parse_Iterator() ==
    match_symbol "for" =>
        MUST parse_Primary()
        expect_symbol "in"
        MUST parse_Expression()
        by_val :=
              AND(match_symbol "by", MUST parse_Expr 200) => pop_stack_1()
              nil
        bar_val :=
            AND(match_symbol "|", MUST parse_Expr 111) => pop_stack_1()
            nil
        in_val := pop_stack_1()
        if bar_val then
            in_val := ["|", in_val, bar_val]
        if by_val then
            push_form3("INBY", pop_stack_1(), in_val, by_val)
        else
            push_form2("IN", pop_stack_1(), in_val)
    match_symbol "while" =>
        MUST parse_Expr 190
        push_form1("WHILE", pop_stack_1())
    match_symbol "until" =>
        MUST parse_Expr 190
        push_form1("UNTIL", pop_stack_1())
    nil

parse_Expr($RBP) ==
    not(parse_NudPart($RBP)) => nil
    while parse_LedPart($RBP) repeat nil
    push_reduction("parse_Expr", pop_stack_1())

parse_LabelExpr() ==
    not(parse_Label()) => nil
    MUST parse_Expr(120)
    push_form2("LABEL", pop_stack_2(), pop_stack_1())

parse_Label() ==
    not(match_symbol "<<") => nil
    MUST parse_Name()
    MUST match_symbol ">>"

parse_LedPart($RBP) ==
    not(parse_Operation("Led", $RBP)) => nil
    push_reduction("parse_LedPart", pop_stack_1())

parse_NudPart($RBP) ==
    AND(OR(parse_Operation("Nud", $RBP), parse_Reduction(), parse_Form()),
        push_reduction("parse_NudPart", pop_stack_1()))

parse_Operation($ParseMode, $RBP) ==
    match_current_token("IDENTIFIER", NIL) => nil
    tmptok := current_symbol()
    SYMBOLP(tmptok) and GET(tmptok, $ParseMode) and
      $RBP < parse_leftBindingPowerOf(tmptok, $ParseMode) =>
        $RBP := parse_rightBindingPowerOf(tmptok, $ParseMode)
        parse_getSemanticForm($ParseMode,
                               ELEMN(GET(tmptok, $ParseMode), 5, NIL))

parse_leftBindingPowerOf(x, ind) ==
    (y := GET(x, ind)) => ELEMN(y, 3, 0)
    0

parse_rightBindingPowerOf(x, ind) ==
    (y := GET(x, ind)) => ELEMN(y, 4, 105)
    105

parse_getSemanticForm(ind, y) ==
    AND(y, FUNCALL(first y)) => true
    ind = "Nud" => parse_Prefix()
    ind = "Led" => parse_Infix()
    nil

parse_Reduction() ==
    parse_ReductionOp() =>
        MUST parse_Expr 1000
        push_form2("Reduce", pop_stack_2(), pop_stack_1())
    nil

parse_ReductionOp() ==
    cur_sym := current_symbol()
    AND(SYMBOLP(cur_sym), GET(cur_sym, "Led"),
        match_next_token("KEYWORD", "/"),
        push_reduction("parse_ReductionOp", cur_sym),
        ACTION advance_token(), ACTION advance_token())

parse_Form() ==
    match_symbol "iterate" =>
        from_val :=
            match_symbol "from" =>
                MUST parse_Label()
                [pop_stack_1()]
            nil
        push_lform1("iterate", from_val)
    match_symbol "yield" =>
        MUST parse_Application()
        push_form1("yield", pop_stack_1())
    parse_Application()

parse_Application() ==
    not(parse_Primary()) => nil
    while parse_Selector() repeat nil
    parse_Application() =>
        push_reduction("parse_Application", [pop_stack_2(), pop_stack_1()])
    true

parse_Selector() ==
    not(match_symbol ".") => nil
    MUST parse_Primary()
    push_reduction("parse_Selector",
                         [pop_stack_2(), pop_stack_1()])

parse_PrimaryNoFloat() ==
    AND(parse_Primary1(), OPTIONAL(parse_TokTail()))

parse_Primary() == OR(parse_Float(), parse_PrimaryNoFloat())

parse_Primary1() ==
    OR(
       AND(parse_VarForm(),
           OPTIONAL AND(current_token_is_nonblank(),
              current_symbol() = "(", MUST parse_Enclosure(),
              push_reduction("parse_Primary1",
                             [pop_stack_2(), pop_stack_1()]))),
       parse_String(), parse_IntegerTok(),
       parse_FormalParameter(),
       AND(symbol_is? "'",
          MUST AND(match_symbol "'", MUST parse_Expr 999,
                   push_form1("QUOTE", pop_stack_1()))),
       parse_Sequence(), parse_Enclosure())

parse_Float() == parse_SPADFLOAT()

parse_Enclosure1(closer) ==
    MUST OR(
            AND(parse_Expr 6, expect_symbol(closer)),
            AND(expect_symbol(closer), push_form0("@Tuple")))

parse_Enclosure() ==
    match_symbol "(" => parse_Enclosure1(")")
    match_symbol "{" => parse_Enclosure1("}")
    match_symbol "SETTAB" => parse_Enclosure1("BACKTAB")
    nil

parse_IntegerTok() == parse_NUMBER()

parse_FormalParameter() == parse_ARGUMENT_DESIGNATOR()

parse_String() == parse_SPADSTRING()

parse_VarForm() == parse_IDENTIFIER()

parse_Name() == parse_IDENTIFIER()

parse_Data() == AND(ACTION(advance_token()),
                    OR(parse_IDENTIFIER(), parse_KEYWORD()),
                    push_form1("QUOTE", pop_stack_1()))

parse_GliphTok(tok) ==
  AND(match_current_token('KEYWORD, tok), ACTION(advance_token()))

parse_Sequence() ==
    match_symbol "[" =>
        MUST(parse_Sequence1())
        expect_symbol "]"
    nil

parse_Sequence1() ==
    val :=
        parse_Expression() => [pop_stack_1()]
        nil
    push_reduction("parse_Sequence1", ["construct", :val])
    OPTIONAL
      AND(parse_IteratorTail(),
          push_lform1("COLLECT", [:pop_stack_1(),
                                       pop_stack_1()]))

-- IteratorTail : [Iterator*]
parse_IteratorTail() ==
    repetition(nil, FUNCTION parse_Iterator)
