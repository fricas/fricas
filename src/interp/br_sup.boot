)package "BOOT"

get_op_implementation(op, sig, pred, predicates, dom, domname) ==
    $returnNowhereFromGoGet: local := true
    predValue := evalDomainOpPred(dom, pred, predicates)
    null predValue =>
        'unexported
    slot := compiledLookup(op, sig, dom) =>
        [f, :r] := slot
        f = 'nowhere => 'nowhere           --see replaceGoGetSlot
        f = function makeSpadConstant => 'constant
        f = function IDENTITY => 'constant
        f = function newGoGet =>
            substitute('_%, domname, devaluate(first(r)))
        null VECP r => systemError devaluateList r
        substitute('_%, domname, devaluate(r))
    'nowhere

ht_add_strings(page, strings) ==
    for str in strings repeat
        ht_add_string(page, str)

mkEvalable2(form) ==
    [op, :args] := form
    co_sig := get_database(op, 'COSIG)
    nargs := []
    for arg in args for k? in rest(co_sig) repeat
        narg :=
            k? => mkEvalable(arg)
            MKQ(arg)
        nargs := cons(narg, nargs)
    nargs := NREVERSE(nargs)
    [op, :nargs]

make_domain_form(kind, name, argString) ==
    typeForm := CATCH('SPAD_READER, CATCH('top_level,
                       unabbrev mkConform(kind, name, argString)))
    ATOM(typeForm) => $spad_failure
    null(evaluatedTypeForm := kisValidType typeForm) =>
        $spad_failure
    cons(0, mkEvalable2(evaluatedTypeForm))

quiet_valid_type?(t) ==
    $ncMsgList : local := []
    kisValidType(t)

--% Call to Spad code generating pages ---

clear_hyperdoc_funs() ==
    $add_description_string_fun := [0, nil]
    $add_to_noproces_fun := [0, nil]
    $add_to_page_fun := [0, nil]
    $destroy_page_fun := [0, nil]
    $do_gen_fun1_fun := [0, nil]
    $do_page_fun0_fun := [0, nil]
    $do_page_fun1_fun := [0, nil]
    $do_page_fun2_fun := [0, nil]
    $do_search_fun := [0, nil]
    $make_page_fun := [0, nil]
    $set_input_string_fun := [0, nil]
    $show_page_fun := [0, nil]

clear_hyperdoc_funs()

ht_add_string(page, s) ==
    not(STRINGP(s)) => BREAK()
    fun := SpadFun($add_description_string_fun,
              getFunctionFromDomain1("add_to_description",
                '(HyperdocPage), $Void, '((HyperdocPage) (String))))
    SPADCALL(page, s, fun)

ht_add_item(page, line) ==
    STRINGP line => ht_add_string(page, line)
    text :=
      PAIRP line => [['text, :line]]
      [['text, line]]
    fun := SpadFun($add_to_noproces_fun,
              getFunctionFromDomain1("add_to_description",
            '(HyperdocPage), $Void, '((HyperdocPage) (List (SExpression)))))
    SPADCALL(page, text, fun)

ht_show_page(page) ==
    fun := SpadFun($show_page_fun,
               getFunctionFromDomain1("show", '(HyperdocPage),
                                      $Void, '((HyperdocPage))))
    SPADCALL(page, fun)

ht_add_to_page(page, itemList) ==
    fun := SpadFun($add_to_page_fun,
              getFunctionFromDomain1("add_to", '(HyperdocPage),
                $Void, '((HyperdocPage) (List (SExpression)))))
    SPADCALL(page, itemList, fun)

ht_new_page(propList) ==
    fun := SpadFun($make_page_fun,
              getFunctionFromDomain1("new", '(HyperdocPage),
                '(HyperdocPage), '(HyperdocAssociationList)))
    page := SPADCALL(propList, fun)
    page

htpDestroyPage(name) ==
    fun := SpadFun($destroy_page_fun,
              getFunctionFromDomain1("destroy", '(HyperdocPage),
                $Void, '(Symbol)))
    SPADCALL(name, fun)

htpSetLabelInputString(page, key, val) ==
    fun := SpadFun($set_input_string_fun,
              getFunctionFromDomain1("set_input_string", '(HyperdocPage),
                $Void, [["HyperdocPage"], ["Symbol"], ["String"]]))
    SPADCALL(page, key, val, fun)

do_search(pat, tag, desc) ==
    fun := SpadFun($do_search_fun,
             getFunctionFromDomain1("do_search", ["HyperdocTopPage"],
                $Void, [["String"], ["Symbol"], ["String"]]))
    SPADCALL(pat, tag, desc, fun)

do_page_fun0(page, tag) ==
    fun := SpadFun($do_page_fun0_fun,
             getFunctionFromDomain1("do_page_fun0", ["HyperdocTopPage"],
                $Void, [["HyperdocPage"], ["Symbol"]]))
    SPADCALL(page, tag, fun)

do_page_fun1(page, tag, data) ==
    fun := SpadFun($do_page_fun1_fun,
             getFunctionFromDomain1("do_page_fun1", ["HyperdocTopPage"],
                $Void, [["HyperdocPage"], ["Symbol"], ["SExpression"]]))
    SPADCALL(page, tag, data, fun)

do_page_fun2(page, tag, data1, data2) ==
    fun := SpadFun($do_page_fun2_fun,
             getFunctionFromDomain1("do_page_fun2", ["HyperdocTopPage"],
                $Void, [["HyperdocPage"], ["Symbol"],
                    ["SExpression"], ["SExpression"]]))
    SPADCALL(page, tag, data1, data2, fun)

do_gen_fun1(tag, data) ==
    fun := SpadFun($do_gen_fun1_fun,
             getFunctionFromDomain1("do_gen_fun1", ["HyperdocTopPage"],
                $Void, [["Symbol"], ["SExpression"]]))
    SPADCALL(tag, data, fun)

-- functions called from HD (man0.ht):
aokSearch(filter) == do_search(filter, 'aok, '"aokSearch")

cSearch(filter) == do_search(filter, 'c, '"category")

detailedSearch(filter) == do_search(filter, 'det, '"detailedSearch")

docSearch(filter) == do_search(filter, 'doc, '"docSearch")

dSearch(filter) == do_search(filter, 'd, '"domain")

genSearch(filter) == do_search(filter, 'gen, '"genSearch")

oSearch(filter) == do_search(filter, 'o, '"operation")

pSearch filter == do_search(filter, 'p, '"package")

kSearch filter == do_search(filter, 'k, '"constructor")

ySearch filter == do_search(filter, 'y, '"constructor")

cons_opts(page, data) == do_page_fun1(page, 'cons_opts, data)

dbChooseDomainOp(page, data) == do_page_fun1(page, 'dbChooseDomainOp, data)
dbSelectCon(page, data) == do_page_fun1(page, 'dbSelectCon, data)
dbShowCons(page, data) == do_page_fun1(page, 'dbShowCons, data)
dbShowConsKindsFilter(page, data) ==
    do_page_fun1(page, 'dbShowConsKindsFilter, data)
dbShowOps(page, data) == do_page_fun1(page, 'dbShowOps, data)
generalSearchDo(page, data) == do_page_fun1(page, 'generalSearchDo, data)
genSearchSayJump(page, data) == do_page_fun1(page, 'genSearchSayJump, data)
grepSearchJump(page, data) == do_page_fun1(page, 'grepSearchJump, data)
htFilterPage(page, data) == do_page_fun1(page, 'htFilterPage, data)
kArgPage(page, data) == do_page_fun1(page, 'kArgPage, data)
kxPage(page, data) == do_page_fun1(page, 'kxPage, data)

showDoc(page, count) == do_page_fun1(page, 'showDoc, count)
showConstruct(page, count) == do_page_fun1(page, 'showConstruct, count)

conPage(c) == do_gen_fun1('conPage, c)

conPageChoose(c) == do_gen_fun1('conPageChoose, c)

dbSpecialOperations(c) == do_gen_fun1('dbSpecialOperations, c)

spadType(s) == do_gen_fun1('spadType, s)

oPageFrom(opname,conname) == do_search(opname, 'oPageFrom, conname)

repeatSearch(page, newValue) ==
    $exposedOnlyIfTrue := newValue
    do_page_fun1(page, 'repeatSearch, newValue)

dbShowCons1(page, data) == do_page_fun1(page, 'dbShowCons1, data)

htGloss(pattern) == do_gen_fun1('htGloss, pattern)

htGlossSearch(page, data) == do_page_fun1(page, 'htGlossSearch, data)
