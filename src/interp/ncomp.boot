)package "BOOT"

flattenSemi(tree) ==
    not(CONSP(tree)) => tree
    tree is [";", t1, t2] =>
        t1 := flattenSemi(t1)
        t2 := flattenSemi(t2)
        t1 :=
            t1 is [";",:rest] => rest
            [t1]
        t2 :=
            t2 is [";",:rest] => rest
            [t2]
        [";", :t1, :t2]
    tree is [";", :.] => BREAK()
    [flattenSemi(el) for el in tree]

--
-- Expansion of macros and removal of macrodefinitions
--

expandMacros(tree) ==
    ATOM tree =>
        repval := HGET($MacroTable, tree)
        repval => expandMacros(repval)
        tree
    -- floating point numbers
    EQ(CAR(tree), ":BF") => tree
    [expandMacros(x) for x in tree]

--
--  Handling of extra definitions
--

replaceArgDef1(args, edef) ==
    SYMBOLP args =>
        edef is [":", args, .] => edef
        BREAK()
    args is  [",", args1, args2] =>
        EQ(args2, NTH(1, edef)) => [",", args1, edef]
        [",", replaceArgDef1(args1, edef), args2]
    BREAK()

replaceArgDef(h1, edef) ==
   h1 is [name, args] => [name, replaceArgDef1(args, edef)]
   BREAK()

replaceArgDefs1(h1, edefs) ==
    for edef in edefs repeat
        h1 := replaceArgDef(h1, edef)
    h1

replaceArgDefs(header, edefs) ==
    header is [":", h1, type] => [":", replaceArgDefs1(h1, edefs), type]
    replaceArgDefs1(header, edefs)

----------------------------------------------------------------------
--
-- Collect definitions from where list.  Returns list of definitions
-- which can not be converted to macros
--

walkWhereList(tree) ==
    lastIteration := false
    ress := nil
    while not(lastIteration) repeat
        if tree is [";", tree1, el] then
            tree := tree1
        else
            el := tree
            lastIteration := true
        el is ["==>", name, def] =>
            HPUT($MacroTable, name, def)
        el is ["==", name, def] =>
            HPUT($MacroTable, name, def)
        el is [":", ., .] =>
            ress := [el, :ress]
        el is [",", pel, item] =>
            item is [":", sym, type] =>
                sl := [sym]
                while pel is [",", pel1, sym] repeat
                    sl := [sym, :sl]
                    pel := pel1
                if not(SYMBOLP pel) then
                    FORMAT(true, '"strange where |,| item2")
                    BREAK()
                sl := [pel, :sl]
                for sym in sl repeat
                    ress := [[":", sym, type], :ress]
            FORMAT(true, '"strange where |,| item1")
            BREAK()
        FORMAT(true, '"strange where item: ~S~&", el)
        BREAK()
    ress

------------------------------------------------------------------
--
-- Remove macros and where parts from global definitions
--

walkForm(tree) ==
    tree is ["==>", name, def] =>
        HPUT($MacroTable, name, def)
        nil
    tree is ["==", head, def] =>
        ress := expandMacros(tree)
        $MacroTable := MAKE_-HASH_-TABLE()
        ress
    tree is ["where", ["==", name, def], env] =>
        edefs := walkWhereList env
        ress := expandMacros(["==", replaceArgDefs(name, edefs), def])
        $MacroTable := MAKE_-HASH_-TABLE()
        ress
    BREAK()

--------------------------------------------------------------------

isNiladic(head1) ==
    SYMBOLP head1 => true
    head1 is [., ["Tuple"]]

getCon(head1) ==
    SYMBOLP head1 => head1
    CAR head1

processGlobals1() ==
    for form in $globalDefs repeat
        [., head, :.] := form
        head1 :=
            head is [":", a, .] => a
            head
        con := getCon head1
        -- at this stage distinction between domain and package does
        -- not matter, so we treat packages as domains
        if head is [":", ., "Category"] then
            SETDATABASE(con, 'CONSTRUCTORKIND, "category")
        else
            SETDATABASE(con, 'CONSTRUCTORKIND, "domain")
        SETDATABASE(con, 'NILADIC, isNiladic head1)

processGlobals () ==
    $InteractiveMode : local := nil
    $globalDefs := REVERSE $globalDefs
    processGlobals1()
    $globalDefs := [parseTransform postTransform x for x in $globalDefs]
    untypedDefs := []
    for def in $globalDefs repeat
        ["DEF", form, sig, sc, body] := def
        -- PRETTYPRINT([CAR form, sig])
        cosig := CONS(nil, [categoryForm? ty for ty in CDR(sig)])
        if not(GETDATABASE(CAR form, 'COSIG) = cosig) then
            PRETTYPRINT([CAR form, sig])
        if null CAR(sig) then
            untypedDefs := [def, :untypedDefs]
        else
            handleKind(def)

    for def in untypedDefs repeat
        ["DEF", form, sig, sc, body] := def
        nt := computeTargetMode(form, body)
        if nt then
            handleKind(["DEF", form, [nt, :rest sig], sc, body])


handleKind(df is ['DEF,form,sig,sc,body]) ==
    [op,:argl] := form

    null CAR(sig) => nil
    if sig is [["Category"], :.] then
        if body is ['add,cat,capsule] then
            body := cat
        sargl:= TAKE(# argl, $TriangleVariableList)
        aList:= [[a,:sa] for a in argl for sa in sargl]
        formalBody:= SUBLIS(aList,body)
        if (not(opOf(formalBody) = "Join")) and _
           (not(opOf(formalBody) = "mkCategory")) then
           formalBody := ['Join, formalBody]
        signature' := SUBLIS(aList,sig)
        constructorCategory := formalBody
    else
        signature' := sig

    pairlis:= [[a,:v] for a in argl for v in $FormalMapVariableList]
    parSignature:= SUBLIS(pairlis,signature')
    parForm:= SUBLIS(pairlis,form)
    constructorModemap := removeZeroOne [[parForm,:parSignature],[true,op]]
    constructorCategory := constructorCategory or constructorModemap.mmTarget
    if not(GETDATABASE(op, 'CONSTRUCTORMODEMAP) = constructorModemap) then
        PRETTYPRINT([CAR form])
        PRETTYPRINT(constructorModemap)
        PRETTYPRINT(GETDATABASE(op, 'CONSTRUCTORMODEMAP))
    if not(GETDATABASE(op, 'CONSTRUCTORCATEGORY) = constructorCategory) then
        PRETTYPRINT([CAR form])
        PRETTYPRINT(constructorCategory)
        PRETTYPRINT(GETDATABASE(op, 'CONSTRUCTORCATEGORY))


-- for domains
--   $lisplibCategory := modemap.mmTarget
-- for categories
--   $lisplibCategory:= formalBody

computeTargetMode(lhs, rhs) ==
    rhs is ['CAPSULE,:.] => MOAN(['"target category of ", lhs,_
          '" cannot be determined from definition"],nil)
    rhs is ['SubDomain,D,:.] => computeTargetMode(lhs,D)
    rhs is ['add,D,['CAPSULE,:.]] => computeTargetMode(lhs,D)
    rhs is ['Record,:l] => ['RecordCategory,:l]
    rhs is ['Union,:l] => ['UnionCategory,:l]
    rhs is ['List,:l] => ['ListCategory,:l]
    rhs is ['Vector,:l] => ['VectorCategory,:l]

    rhs is [op, :argl] =>
        modemap := GETDATABASE(op, 'CONSTRUCTORMODEMAP)
        modemap is [[form, sig], [=true,.]] =>
            pairlis:= [[v,:a] for a in argl for v in $FormalMapVariableList]
            -- substitue
            SUBLIS(pailis, CAR(sig))
        nil
    BREAK()

)if false

 abbreviation               ; +-
 ancestors                  ; interp.
 constructor                ; unused
 constructorcategory        ; +
 constructorkind            ; +
 constructormodemap         ; +- (need to handle untyped definitions)
 cosig                      ; +
 defaultdomain              ; + used only in interpreter, values is
                              computed in daase.lisp, but is unused
                              (getdatabase returns value from hardcoded list)
 modemaps                   ; almost unused in the compiler -- used to
                              invalidate old modemaps when updating
                              *operation-hash* (which in turn is used
                              only in intepreter).
 niladic                    ; +
 object                     ; +-
 operationalist             ; interp.

)endif
