)package "BOOT"

any_to_string(u) == WRITE_-TO_-STRING(u)

value_to_form(arg, t, in_form?) ==
    form :=
         in_form? => ["InputForm"]
         $OutputForm
    isValidType(t) and PAIRP(t) and
            (get_database(first(t), 'CONSTRUCTORKIND) = 'domain) =>
        (val := coerceInteractive(objNewWrap(arg, t), form)) =>
            objValUnwrap(val)
        -- Wrong, but we try to produce something
        in_form? => ["error", '"Bad value"]
        any_to_string(arg)
    -- Wrong, but we try to produce something
    in_form? => ["error", '"Bad value"]
    any_to_string(arg)

arg_to_form(arg, t, c, in_form?) ==
    c => constructor_to_form(arg, in_form?)
    value_to_form(arg, t, in_form?)

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

record_arg_to_form(at, in_form?) ==
    [., name, type] := at
    in_form? => [":", name, constructor_to_form(type, in_form?)]
    ['CONCAT, name, '":", constructor_to_form(type, in_form?)]

record_args_to_form(argl, in_form?) ==
    res := []
    for at in argl repeat
        res := cons(record_arg_to_form(at, in_form?), res)
    nreverse(res)

Record_to_form(argl, in_form?) ==
    cons('Record, record_args_to_form(argl, in_form?))

Union_to_form(argl, in_form?) ==
    not(null(argl)) and (first(argl) is [":", name, type]) =>
        -- new style Union
        cons('Union, record_args_to_form(argl, in_form?))
    -- old style
    cons('Union, [constructor_to_form(arg, in_form?) for arg in argl])

Mapping_to_form(argl, in_form?) ==
    -- should we allow this ???
    null(argl) =>
        in_form? => ["error", '"Bad mapping"]
        ['PAREN, ["->", '"()", '"()"]]
    rt := constructor_to_form(first(argl), in_form?)
    nargs := [constructor_to_form(arg, in_form?) for arg in rest(argl)]
    in_form? => ["Mapping", rt, :nargs]
    if #nargs > 1 then
        nargs := ['PAREN, ['AGGLST, :nargs]]
    else if null(nargs) then
        nargs := '"()"
    else
        nargs := first(nargs)
    ['PAREN, ["->", nargs, rt]]

constructor_to_form(con, in_form?) ==
    if VECTORP(con) then
        con := devaluate(con)
    STRINGP(con) =>
        in_form? => con
        CONCAT("_"", con, "_"")
    ATOM(con) =>
        con = $EmptyMode => '"?"
        -- Wrong, but we try to produce something
        in_form? => ["error", '"Bad type"]
        any_to_string(con)
    op := first(con)
    argl := rest(con)

    op = 'Join => Join_to_OutputForm(argl)
    op = 'mkCategory => mkCategory_to_OutputForm(argl)
    op = 'Enumeration =>
        in_form? => con
        prefix_to_string(con)
    op = 'Record => Record_to_form(argl, in_form?)
    op = 'Union => Union_to_form(argl, in_form?)
    op = 'Mapping => Mapping_to_form(argl, in_form?)
    (abb := constructor?(op)) =>
        null(argl) =>
            in_form? => con
            constructorName(op)
        con_sig := getConstructorSignature(op)
        cosig := get_database(op, 'COSIG)
        null(con_sig) or null(cosig) =>
            -- Wrong, but we try to produce something
            in_form? => ["error", '"Bad type"]
            prefix_to_string(con)
        con_sig := rest(con_sig)
        cosig := rest(cosig)
        if not freeOfSharpVars(con_sig) then
            con_sig := SUBLIS([[s_var, :val]
                               for s_var in $FormalMapVariableList
                               for val in argl], con_sig)
        n_argl := [arg_to_form(arg, t, c, in_form?) for arg in argl
                   for t in con_sig for c in cosig]
        in_form? => cons(op, n_argl)
        [constructorName(op), :n_argl]
    -- Wrong, but we try to produce something
    in_form? => ["error", '"Bad type"]
    prefix_to_string(con)

constructor_to_OutputForm(con) == constructor_to_form(con, false)

constructor_to_InputForm(con) == constructor_to_form(con, true)
