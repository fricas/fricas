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

