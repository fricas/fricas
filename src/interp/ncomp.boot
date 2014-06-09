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
        mdef := HGET($MacroTable, tree)
        mdef =>
            repval := first(mdef)
            null(rest(mdef)) => expandMacros(repval)
            userError("macro call needs arguments")
        tree
    -- floating point numbers
    [op, :args] := tree
    EQ(op, ":BF") => tree
    ATOM(op) =>
        mdef := HGET($MacroTable, op)
        mdef =>
            repval := first(mdef)
            margs := rest(mdef)
            null(margs) =>
                [expandMacros(repval), :[expandMacros(x) for x in args]]
            args :=
                args is [[",", :args1]] => postFlatten(first(args), ",")
                args
            #args = #margs =>
                expandMacros(SUBLISLIS(args, margs, repval))
            userError("invalid macro call, #args ~= #margs")
        [op, :[expandMacros(x) for x in args]]
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

DEFPARAMETER($restore_list, nil)

define_macro(name, def) ==
    if SYMBOLP(name) then
        def := [def]
    else if name is [op, :args] and SYMBOLP(op) then
        args :=
            args is [[",", :args1]] => postFlatten(first(args), ",")
            args
        name := op
        def := [def, :args]
    else
        SAY([name, def])
        userError("Invalid macro definition")
    prev_def := HGET($MacroTable, name)
    PUSH([name, :prev_def], $restore_list)
    HPUT($MacroTable, name, def)

do_walk_where_list(tree) ==
    lastIteration := false
    ress := nil
    while not(lastIteration) repeat
        if tree is [";", tree1, el] then
            tree := tree1
        else
            el := tree
            lastIteration := true
        el is ["==>", name, def] => define_macro(name, def)
        el is ["==", name, def] =>
            define_macro(name, def)
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

walkWhereList(name, def, env) ==
    $restore_list : local := nil
    edefs := do_walk_where_list env
    ress := expandMacros(["==", replaceArgDefs(name, edefs), def])
    for it in $restore_list repeat
        [op, :def] := it
        HPUT($MacroTable, op, def)
    ress

walkForm(tree) ==
    tree is ["==>", name, def] =>
        define_macro(name, def)
        nil
    tree is ["==", head, def] => expandMacros(tree)
    tree is ["where", ["==", name, def], env] =>
        walkWhereList(name, def, env)
    userError("Parsing error: illegal toplevel form")
    nil

--------------------------------------------------------------------

isNiladic(head1) ==
    SYMBOLP head1 => true
    head1 is [., ["@Tuple"]]

getCon(head1) ==
    SYMBOLP head1 => head1
    first head1

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
        cosig := CONS(nil, [categoryForm? ty for ty in rest(sig)])
        SETDATABASE(first form, 'COSIG, cosig)
        if null first(sig) then
            untypedDefs := [def, :untypedDefs]
        else
            handleKind(def)

    for def in untypedDefs repeat
        ["DEF", form, sig, sc, body] := def
        nt := computeTargetMode(form, body)
        if nt then
            handleKind(["DEF", form, [nt, :rest sig], sc, body])
        else
            SAY(["unhandled target", form])
    boo_comp_cats()


handleKind(df is ['DEF,form,sig,sc,body]) ==
    [op,:argl] := form

    null first(sig) => nil
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
    SETDATABASE(op, 'CONSTRUCTORMODEMAP, constructorModemap)
    SETDATABASE(op, 'CONSTRUCTORCATEGORY, constructorCategory)

boo_comp_cats() ==
    $compiler_output_stream := MAKE_-BROADCAST_-STREAM()
    $bootStrapMode : local := true
    SAY(["boo_comp_cats"])
    hcats := []
    for def in $globalDefs repeat
        ["DEF", form, sig, sc, body] := def
        if sig is [["Category"], :.] then
            SAY(["doing", form, sig])
            not("and"/[categoryForm? ty for ty in rest(sig)]) =>
                hcats := cons(def, hcats)
            boo_comp1(def)
    for def in hcats repeat boo_comp1(def)

boo_comp1(x) ==
    $Index : local := 0
    $MACROASSOC : local := []
    $compUniquelyIfTrue : local := nil
    $postStack : local := nil
    $topOp : local := nil
    $semanticErrorStack : local := []
    $warningStack : local := []
    $exitMode : local := $EmptyMode
    $exitModeStack : local := []
    $returnMode : local := $EmptyMode
    $leaveLevelStack : local := []
    $CategoryFrame : local := [[[]]]
    $insideFunctorIfTrue : local := false
    $insideExpressionIfTrue : local := false
    $insideWhereIfTrue : local := false
    $insideCategoryIfTrue : local := false
    $insideCapsuleFunctionIfTrue : local := false
    $form : local := nil
    $e : local := $EmptyEnvironment
    $genSDVar : local :=  0
    $previousTime : local := get_run_time()
    compTopLevel(x, $EmptyMode,  [[[]]])
    if $semanticErrorStack then displaySemanticErrors()

-- for domains
--   $lisplibCategory := modemap.mmTarget
-- for categories
--   $lisplibCategory:= formalBody

computeTargetMode(lhs, rhs) ==
    PRETTYPRINT(["computeTargetMode", lhs])
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
        modemap is [[form, sig, :.], [=true,.]] =>
            pairlis:= [[v,:a] for a in argl for v in $FormalMapVariableList]
            -- substitue
            SUBLIS(pairlis, sig)
        PRETTYPRINT("strange untyped def")
        PRETTYPRINT([lhs, rhs, modemap])
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

DEFVAR($PrintOnly, false)
DEFVAR($RawParseOnly, false)
DEFVAR($PostTranOnly, false)
DEFVAR($FlatParseOnly, false)
DEFVAR($TranslateOnly, false)
DEFVAR($noEarlyMacroexpand, false)
DEFVAR($SaveParseOnly, false)
DEFVAR($globalDefs, nil)
DEFVAR($MacroTable)

S_process(x) ==
    $Index : local := 0
    $MACROASSOC : local := nil
    $compUniquelyIfTrue : local := false
    $postStack : local := nil
    $topOp : local := nil
    $semanticErrorStack : local := nil
    $warningStack : local := nil
    $exitMode : local := $EmptyMode
    $exitModeStack : local := nil
    $returnMode : local := $EmptyMode
    $leaveLevelStack : local := nil
    $CategoryFrame : local := [[[]]]
    $insideFunctorIfTrue : local := false
    $insideExpressionIfTrue : local := false
    $insideWhereIfTrue : local := false
    $insideCategoryIfTrue : local := false
    $insideCapsuleFunctionIfTrue : local := false
    $form : local := nil
    $e : local := $EmptyEnvironment
    $genSDVar : local := 0
    $previousTime : local := get_run_time()
    $s : local := nil
    $x : local := nil
    $m : local := nil
    null(x) => nil
    $SaveParseOnly =>
        x := walkForm(x)
        if x then PUSH(x, $globalDefs)
    $RawParseOnly => PRETTYPRINT(x)
    $FlatParseOnly => PRETTYPRINT(flattenSemi x)
    $PostTranOnly => PRETTYPRINT(postTransform x)
    nform :=
        $noEarlyMacroexpand => x
        walkForm x
    null(nform) => nil
    x := parseTransform(postTransform(nform))
    $TranslateOnly => $Translation := x
    $postStack =>
        displayPreCompilationErrors()
        userError "precompilation failed"
    $PrintOnly =>
        FORMAT(true, '"~S   =====>~%", $currentLine)
        PRETTYPRINT(x)
    u := compTopLevel(x, $EmptyMode, $InteractiveFrame)
    if u then $InteractiveFrame := THIRD(u)
    if $semanticErrorStack then displaySemanticErrors()
    TERPRI()
