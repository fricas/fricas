)package "BOOT"

$use_old_value_print := false

any_to_string(u) == WRITE_-TO_-STRING(u)

$outform_to_string_fun := [0, nil]

outform_to_string(o) ==
    fun := SpadFun($outform_to_string_fun,
              getFunctionFromDomain1("outform_to_string",
                '(HyperdocUtilities), '(String), '((OutputForm))))
    SPADCALL(o, fun)

value_to_form(arg, t, kind) ==
    form :=
         kind = 'in_form => ["InputForm"]
         $OutputForm
    isValidType(t) and PAIRP(t) and
            (get_database(first(t), 'CONSTRUCTORKIND) = 'domain) =>
        (val := coerceInteractive(objNewWrap(arg, t), form)) =>
            res := objValUnwrap(val)
            kind = 'str_form => outform_to_string(res)
            res
        -- Wrong, but we try to produce something
        kind = 'in_form => ["error", '"Bad value"]
        any_to_string(arg)
    -- Wrong, but we try to produce something
    kind = 'in_form => ["error", '"Bad value"]
    any_to_string(arg)

arg_to_form(arg, t, c, kind) ==
    c => constructor_to_form(arg, kind)
    value_to_form(arg, t, kind)

prefix_to_string(con) ==
    u := prefix2String(con)
    atom(u) => u
    concatenateStringList([object2String(x) for x in u])

-- fake, to catch possible use
mkCategory_to_OutputForm(argl) ==
    throwMessage('"mkCategory_to_OutputForm called")

-- fake, to catch possible use
Join_to_OutputForm(argl) ==
    throwMessage('"Join_to_OutputForm called")

record_arg_to_form(at, kind) ==
    [., name, type] := at
    kind = 'in_form => [":", name, constructor_to_form(type, kind)]
    kind = 'str_form =>
        CONCAT(PNAME(name), '": ", constructor_to_form(type, kind))
    ['CONCAT, name, '":", constructor_to_form(type, kind)]

record_args_to_form(argl, kind) ==
    res := []
    for at in argl repeat
        res := cons(record_arg_to_form(at, kind), res)
    nreverse(res)

comma_separate(sl) ==
    first := true
    res := []
    for s in sl repeat
        if not(first) then
            res := cons('",", res)
        first := false
        res := cons(s, res)
    nreverse(res)

op_with_args(op, sl) ==
    concatenateStringList([PNAME(op), '"(", :comma_separate(sl), '")"])

RU_to_form(op, argl, kind) ==
    rl := record_args_to_form(argl, kind)
    kind = 'str_form => op_with_args(op, rl)
    cons(op, rl)

Record_to_form(argl, kind) == RU_to_form('Record, argl, kind)

Union_to_form(argl, kind) ==
    not(null(argl)) and (first(argl) is [":", name, type]) =>
        -- FIXME
        kind = 'str_form and #argl = 2 and argl.1 = '"..." =>
            rl := [record_arg_to_form(argl.0, kind), '"..."]
            op_with_args('Union, rl)
        -- new style Union
        RU_to_form('Union, argl, kind)
    -- old style
    ul := [constructor_to_form(arg, kind) for arg in argl]
    kind = 'str_form => op_with_args('Union, ul)
    cons('Union, ul)

Mapping_to_form(argl, kind) ==
    -- should we allow this ???
    null(argl) =>
        kind = 'in_form => ["error", '"Bad mapping"]
        kind = 'str_form => '"() -> ()"
        ['PAREN, ["->", '"()", '"()"]]
    rt := constructor_to_form(first(argl), kind)
    nargs := [constructor_to_form(arg, kind) for arg in rest(argl)]
    kind = 'in_form => ["Mapping", rt, :nargs]
    if #nargs > 1 then
        nargs :=
            kind = 'str_form =>
                concatenateStringList(['"(", :comma_separate(nargs), '")"])
            ['PAREN, ['AGGLST, :nargs]]
    else if null(nargs) then
        nargs := '"()"
    else
        nargs := first(nargs)
    kind = 'str_form => CONCAT('"(", nargs, " -> ", rt , '")")
    ['PAREN, ["->", nargs, rt]]

constructor_to_form(con, kind) ==
    if VECTORP(con) then
        con := devaluate(con)
    STRINGP(con) =>
        kind = 'in_form => con
        -- FIXME: Interpreter mangles Unions, this should be handled better.
        kind = 'str_form and con = '"..." => con
        CONCAT("_"", con, "_"")
    ATOM(con) =>
        con = $EmptyMode => '"?"
        -- Wrong, but we try to produce something
        kind = 'in_form => ["error", '"Bad type"]
        any_to_string(con)
    op := first(con)
    argl := rest(con)

    op = 'Join => Join_to_OutputForm(argl)
    op = 'mkCategory => mkCategory_to_OutputForm(argl)
    op = 'Enumeration =>
        kind = 'in_form => con
        prefix_to_string(con)
    op = 'Record => Record_to_form(argl, kind)
    op = 'Union => Union_to_form(argl, kind)
    op = 'Mapping => Mapping_to_form(argl, kind)
    (abb := constructor?(op)) =>
        opn :=
            $abbreviateTypes => abb
            op
        null(argl) =>
            kind = 'in_form => con
            kind = 'str_form => PNAME(opn)
            opn
        con_sig := getConstructorSignature(op)
        cosig := get_database(op, 'COSIG)
        null(con_sig) or null(cosig) =>
            -- Wrong, but we try to produce something
            kind = 'in_form => ["error", '"Bad type"]
            prefix_to_string(con)
        con_sig := rest(con_sig)
        cosig := rest(cosig)
        if not freeOfSharpVars(con_sig) then
            con_sig := SUBLIS([[s_var, :val]
                               for s_var in $FormalMapVariableList
                               for val in argl], con_sig)
        n_argl := [arg_to_form(arg, t, c, kind) for arg in argl
                   for t in con_sig for c in cosig]
        kind = 'in_form => cons(op, n_argl)
        kind = 'str_form =>
            op_with_args(opn, n_argl)
        [opn, :n_argl]
    -- Wrong, but we try to produce something
    kind = 'in_form => ["error", '"Bad type"]
    prefix_to_string(con)

constructor_to_String(con) == constructor_to_form(con, 'str_form)

constructor_to_OutputForm(con) == constructor_to_form(con, 'out_form)

constructor_to_InputForm(con) == constructor_to_form(con, 'in_form)
