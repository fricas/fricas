)package "BOOT"

any_to_string(u) == WRITE_-TO_-STRING(u)

arg_to_OutputForm(arg, t, c) ==
    c => constructor_to_OutputForm(arg)
    isValidType(t) and PAIRP(t) and
            (get_database(first(t), 'CONSTRUCTORKIND) = 'domain) =>
        (val := coerceInteractive(objNewWrap(arg, t), $OutputForm)) =>
            objValUnwrap(val)
        -- Wrong, but we try to produce something
        any_to_string(arg)
    -- Wrong, but we try to produce something
    any_to_string(arg)

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

Record_to_OutputForm(argl) ==
    rres := []
    for [":", name, type] in argl repeat
        r1 := ['CONCAT, name, '":", constructor_to_OutputForm(type)]
        rres := cons(r1, rres)
    cons('Record, reverse(rres))

Union_to_OutputForm(argl) ==
    not(null(argl)) and (first(argl) is [":", name, type]) =>
        -- new style Union
        nargs := [['CONCAT, name, '":", constructor_to_OutputForm(type)]
                  for [":", name, type] in argl]
        ['Union, :nargs]
    -- old style
    nargs := [constructor_to_OutputForm(arg) for arg in argl]
    ['Union, :nargs]

Mapping_to_OutputForm(argl) ==
    -- should we allow this ???
    null(argl) => ['PAREN, ["->", '"()", '"()"]]
    rt := constructor_to_OutputForm(first(argl))
    nargs := [constructor_to_OutputForm(arg) for arg in rest(argl)]
    if #nargs > 1 then
        nargs := ['PAREN, ['AGGLST, :nargs]]
    else if null(nargs) then
        nargs := '"()"
    else
        nargs := first(nargs)
    ['PAREN, ["->", nargs, rt]]

constructor_to_OutputForm(con) ==
    if VECTORP(con) then
        con := devaluate(con)
    STRINGP(con) => CONCAT("_"", con, "_"")
    ATOM(con) =>
        con = $EmptyMode => '"?"
        -- Wrong, but we try to produce something printable
        any_to_string(con)
    op := first(con)
    argl := rest(con)

    op = 'Join => Join_to_OutputForm(argl)
    op = 'mkCategory => mkCategory_to_OutputForm(argl)
    op = 'Record => Record_to_OutputForm(argl)
    op = 'Union => Union_to_OutputForm(argl)
    op = 'Mapping => Mapping_to_OutputForm(argl)
    (abb := constructor?(op)) =>
        null(argl) => constructorName(op)
        con_sig := getConstructorSignature(op)
        cosig := get_database(op, 'COSIG)
        null(con_sig) or null(cosig) =>
            -- Wrong, but we try to produce something
            prefix_to_string(con)
        con_sig := rest(con_sig)
        cosig := rest(cosig)
        if not freeOfSharpVars(con_sig) then
            con_sig := SUBLIS([[s_var, :val]
                               for s_var in $FormalMapVariableList
                               for val in argl], con_sig)
        n_argl := [arg_to_OutputForm(arg, t, c) for arg in argl
                   for t in con_sig for c in cosig]
        [constructorName(op), :n_argl]
    -- Wrong, but we try to produce something
    prefix_to_string(con)
