-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     - Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     - Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in
--       the documentation and/or other materials provided with the
--       distribution.
--
--     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

)package "BOOT"

--% Code for tracing functions

-- This code supports the )trace system command and allows the
-- tracing of LISP, BOOT and SPAD functions and interpreter maps.

DEFPARAMETER($traceNoisely, NIL)  -- give trace and untrace messages

DEFVAR($traceDomains, true)

DEFPARAMETER($reportSpadTrace, NIL)  -- reports traced funs

DEFPARAMETER($optionAlist, NIL)

DEFPARAMETER($tracedMapSignatures, NIL)

DEFPARAMETER($traceOptionList, '(
    after _
    before _
    break_
    cond_
    count_
    depth_
    local_
    mathprint _
    nonquietly_
    nt_
    of_
    only_
    ops_
    restore_
    timer_
    varbreak _
    vars_
    within _
    ))

DEFPARAMETER($domainTraceNameAssoc, NIL)

-- used for )restore option of )trace.
DEFPARAMETER($lastUntraced, NIL)

DEFVAR($trace_names, [])
DEFVAR($count_list, [])
DEFVAR($timer_list, [])

trace l == traceSpad2Cmd l

traceSpad2Cmd l ==
  if l is ['Tuple, l1] then l := l1
  $mapSubNameAlist:= getMapSubNames(l)
  trace1(augmentTraceNames(l, $mapSubNameAlist), $options)
  traceReply()

trace1(l, options) ==
  $traceNoisely: local := NIL
  if hasOption(options, 'nonquietly) then $traceNoisely := true
  hasOption(options, 'off) =>
    (ops := hasOption(options, 'ops)) or
      (lops := hasOption(options, 'local)) =>
        null l => throwKeyedMsg("S2IT0019",NIL)
        constructor := unabbrev
          atom l => l
          null rest l =>
            atom first l => first l
            first first l
          NIL
        not(isFunctor constructor) => throwKeyedMsg("S2IT0020",NIL)
        if ops then
          ops := getTraceOption ops
          NIL
        if lops then
          lops := rest getTraceOption lops
          untraceDomainLocalOps(constructor,lops)
    (1 < #options) and not hasOption(options, 'nonquietly) =>
      throwKeyedMsg("S2IT0021",NIL)
    untrace l
    clearConstructorCaches()
  hasOption(options, 'stats) =>
    (1 < #options) =>
      throwKeyedMsg("S2IT0001",['")trace ... )stats"])
    [., :opt] := first options
    -- look for )trace )stats       to list the statistics
    --          )trace )stats reset to reset them
    null opt =>      -- list the statistics
      centerAndHighlight('"Traced function execution times",78,"-")
      ptimers ()
      SAY '" "
      centerAndHighlight('"Traced function execution counts",78,"-")
      pcounters ()
    selectOptionLC(first opt,'(reset),'optionError)
    resetSpacers()
    resetTimers()
    resetCounters()
    throwKeyedMsg("S2IT0002",NIL)
  a:= hasOption(options, 'restore) =>
    null(oldL:= $lastUntraced) => nil
    newOptions := delete(a, options)
    null l => trace1(oldL, options)
    for x in l repeat
      x is [domain,:opList] and VECP domain =>
        sayKeyedMsg("S2IT0003",[devaluate domain])
      options := [:newOptions, :LASSOC(x, $optionAlist)]
      trace1(LIST x, options)
  null l => nil
  l is ["?"] => _?t()
  traceList:= [transTraceItem x for x in l] or return nil
  for x in traceList repeat $optionAlist:=
    ADDASSOC(x, options, $optionAlist)
  optionList:= getTraceOptions(options)
  if (domainList := LASSOC("of", optionList)) then
      LASSOC("ops", optionList) =>
        throwKeyedMsg("S2IT0004", NIL)
      opList:=
        traceList => [["ops", :traceList]]
        nil
      varList:=
        y:= LASSOC("vars", optionList) => [["vars", :y]]
        nil
      optionList := [:opList, :varList]
      traceList := domainList
  for funName in traceList repeat
      trace2(funName, [], optionList)
  saveMapSig(traceList)

get_trace_option(l, opt) ==
    res := nil
    for el in l repeat
        if EQ(IFCAR(el), opt) then
            res := el
    res

COND_UCASE(X) ==
    INTEGERP(X) => X
    UPCASE(X)

-- Call tracing

VARP(test_item) ==
    IDENTP(test_item) => test_item
    CONSP(test_item) and
        (EQ(first(test_item), 'FLUID) or EQ(first(test_item), 'LEX)) and
      CONSP(rest(test_item)) and IDENTP(first(rest(test_item))) => test_item
    false

flat_bv_list(bv_list) ==
    VARP(bv_list) => [bv_list]
    REFVECP(bv_list) => BREAK()
    NOT(CONSP(bv_list)) => []
    EQ("=", (tmp1 := first(bv_list))) => flat_bv_list(rest(bv_list))
    VARP(tmp1) => [tmp1, :flat_bv_list(rest(bv_list))]
    NOT(CONSP(tmp1)) and NOT(REFVECP(tmp1)) =>
        flat_bv_list(rest(bv_list))
    NCONC(flat_bv_list(tmp1), flat_bv_list(rest(bv_list)))

DEFVAR($embedded_functions, [])

embeded_function(name, new_def, old_def) ==
    new_def :=
        NOT(CONSP(new_def)) => new_def
        body := IFCDR(IFCDR(new_def))
        op := first(new_def)
        body and (bv := first(rest(new_def)))
          and (EQ(op, 'LAMBDA) or EQ(op, 'MLAMBDA)) =>
            NOT(MEMQ(name, flat_bv_list(bv))) =>
                [op, bv, [['LAMBDA, [name], :body],
                          ['QUOTE, old_def]]]
            new_def
        BREAK()
    COERCE(new_def, 'FUNCTION)

embed2(name, new_def, old_def) ==
    setSf(name, new_def)
    PUSH([name, new_def, old_def], $embedded_functions)
    name

EMBED(name, new_def) ==
    if NOT(IDENTP(name)) then
        ERROR(FORMAT(nil, '"invalid argument ~s to EMBED", name))
    old_def := SYMBOL_-FUNCTION(name)
    new_def := embeded_function(name, new_def, old_def)
    embed2(name, new_def, old_def)

UNEMBED(name) ==
    tmp := []
    e_list := $embedded_functions
    cur_def := SYMBOL_-FUNCTION(name)
    flag := true
    e_head := []
    while flag repeat
        NULL(e_list) => flag := false
        e_head := first(e_list)
        CONSP(e_head) and CONSP(rest(e_head)) and
          EQ(name, first(e_head)) and EQ(cur_def, e_head.1) =>
            flag := false
        tmp := e_list
        e_list := rest(e_list)
    e_head =>
        setSf(name, e_head.2)
        if NOT(NULL(tmp)) then
            RPLACD(tmp, rest(e_list))
        else
            $embedded_functions := rest(e_list)
        name
    false

trace3(fn, modemap, options, bin_def) ==
    if MEMQ(fn, $trace_names) then untrace2(fn, [])
    options := OPTIONS2UC(options)
    $traceDomains and isFunctor(fn) and ATOM(fn) =>
        traceDomainConstructor(fn, options)
    math_trace := get_trace_option(options, 'MATHPRINT)
    if math_trace and NOT(EQL(ELT(PNAME(fn), 0), char('_$)))
               and NOT(GENSYMP(fn)) then
        if RASSOC(fn, $mapSubNameAlist) then
            $mathTraceList := [fn, :$mathTraceList]
        else
            spadThrowBrightly(
              FORMAT(nil, '"mathprint not available for ~A", fn))
    vars := get_trace_option(options, 'VARS)
    if vars then
        vars :=
            NOT(rest(vars)) => 'ALL
            rest(vars)
        tracelet(fn, bin_def, vars)
    BREAK := get_trace_option(options, 'BREAK)
    VARBREAK := get_trace_option(options, 'VARBREAK)
    if VARBREAK then
        vars :=
            NOT(rest(VARBREAK)) => 'ALL
            rest(VARBREAK)
        breaklet(fn, bin_def, vars)
    not(bin_def) and SYMBOLP(fn) and NOT(BOUNDP(fn)) and NOT(FBOUNDP(fn)) =>
      isUncompiledMap(fn) =>
          sayBrightly(FORMAT(nil,
        '"~A must be compiled before it may be traced -- invoke ~A to compile",
                                            fn, fn))
      isInterpOnlyMap(fn) =>
          sayBrightly(FORMAT(nil,
        '"~A cannot be traced because it is an interpret-only function", fn))
      sayBrightly(FORMAT(nil, '"~A is not a function", fn))
    not(bin_def) and SYMBOLP(fn) and BOUNDP(fn)
      and isDomainOrPackage(FNVAL := EVAL(fn)) =>
        spadTrace(FNVAL, options)
    if (U := get_trace_option(options, "MASK=")) then
        MAKEPROP(fn, '_/TRANSFORM, U.1)
    $trace_names :=
        get_trace_option(options, 'ALIAS) => $trace_names
        [fn, :$trace_names]
    TRACENAME :=
        (U := get_trace_option(options, 'ALIAS)) => STRINGIMAGE(U.1)
        if $traceNoisely and NOT(vars) and
           NOT(isSubForRedundantMapName(fn)) then
            sayBrightly(["%b", rassocSub(fn, $mapSubNameAlist),
                         "%d", '"traced"])
        STRINGIMAGE(fn)
    if $fromSpadTrace then
        if math_trace then PUSH(INTERN(TRACENAME), $mathTraceList)
        LETFUNCODE := ["EQ", nil, nil] --  No-op
        BEFORE :=
            (U := get_trace_option(options, 'BEFORE)) =>
                ["PROGN", U.1, LETFUNCODE]
            LETFUNCODE
    else
        BEFORE :=
            (U := get_trace_option(options, 'BEFORE)) => U.1
            []
    AFTER :=
        (U := get_trace_option(options, 'AFTER)) => U.1
        []
    caller := get_trace_option(options, 'CALLER)
    FROM_CONDITION :=
        (U := get_trace_option(options, 'FROM)) =>
            ["EQ", "#9", ["QUOTE", U.1]]
        true
    CONDITION :=
        (U := get_trace_option(options, 'WHEN)) => U.1
        true
    WITHIN_CONDITION := true
    if (U := get_trace_option(options, 'WITHIN)) then
        G := INTERN(STRCONC(PNAME(fn), '"/", PNAME(U.1)))
        SET(G, 0)
        trace2(U.1, [], [['WHEN, false],
                   ['BEFORE, ["SETQ", G, ["1+", G]]],
                    ['AFTER, ["SETQ", G, ["1-", G]]]])
        WITHIN_CONDITION := [">", G, 0]
    if get_trace_option(options, 'COUNT) then
        COUNTNAM := INTERN(STRCONC(TRACENAME, ",COUNT"))
    COUNT_CONDITION :=
        (U := get_trace_option(options, 'COUNT)) =>
            $count_list := adjoin_equal(TRACENAME, $count_list)
            rest(U) and INTEGERP(U.1) =>
                ["COND", [["<=", COUNTNAM, U.1], true],
                         [true, ["untrace2", MKQ(fn), []], false]]
            true
        true
    if get_trace_option(options, 'TIMER) then
        TIMERNAM := INTERN(STRCONC(TRACENAME, '",TIMER"))
        $timer_list := adjoin_equal(TRACENAME, $timer_list)
    DEPTH_CONDITION :=
        (U := get_trace_option(options, 'DEPTH)) =>
            rest(U) and INTEGERP(U.1) => ["<=", 'FUNDEPTH, U.1]
            TRACE_OPTION_ERROR('DEPTH)
        true
    CONDITION := MKPF([CONDITION, WITHIN_CONDITION, FROM_CONDITION,
                       COUNT_CONDITION, DEPTH_CONDITION], 'AND)
    ONLYS := get_trace_option(options, 'ONLY)
    -- TRACECODE meaning:
    --  0:        Caller (0,1)           print caller if 1
    --  1:        Value (0,1)            print value if 1
    --  2...:     Arguments (0,...,9)    stop if 0; print ith if i; all if 9
    TRACECODE :=
        get_trace_option(options, 'NT) => '"000"
        ONLYS := MAPCAR(FUNCTION COND_UCASE, ONLYS)
        F := member('F, ONLYS) or member('FULL, ONLYS)
        A := F or member('A, ONLYS) or member('ARGS, ONLYS)
        V := F or member('V, ONLYS) or member('VALUE, ONLYS)
        C := F or member('C, ONLYS) or member('CALLER, ONLYS)
        NL :=
            A => [char('_9)]
            [FETCHCHAR(STRINGIMAGE(X), 0) for X in ONLYS |
                INTEGERP(X) and 0 < X and X < 9]
        NOT(A or V or C or NL) =>
            caller =>  '"119"
            '"019"
        NL := APPEND(NL, [char('_0)])
        buf := make_spaces(A => 3; 2 + #NL)
        buf.0 :=
            (C or caller) => char('_1)
            char('_0)
        buf.1 := (V => char('_1); char('_0))
        A =>
            buf.2 := char('_9)
            buf
        for x in NL for i in 2.. repeat
            buf.i := x
        buf
    G4 := MACRO_-FUNCTION(fn)
    if COUNTNAM then SET(COUNTNAM, 0)
    if TIMERNAM then SET(TIMERNAM, 0)
    ll := ['QUOTE, [TRACENAME, if G4 then 'MACRO else false, TRACECODE,
                    COUNTNAM, TIMERNAM, BEFORE, AFTER, CONDITION,
                       BREAK, modemap, ['QUOTE, true]]]
    NEW_DEF := [if G4 then 'MLAMBDA else 'LAMBDA, ["&REST", 'G6],
                ["/MONITORX", 'G6, fn, ll]]
    bin_def => embeded_function(fn, NEW_DEF, bin_def)
    OLD_DEF := SYMBOL_-FUNCTION(fn)
    NEW_DEF := embeded_function(fn, NEW_DEF, OLD_DEF)
    embed2(fn, NEW_DEF, OLD_DEF)
    fn

trace2(fn, modemap, options) ==
    trace3(fn, modemap, options, false)

untrace2(X, options) ==
    isFunctor(X) and ATOM(X) => untraceDomainConstructor(X)
    isDomainOrPackage(U := X) or (SYMBOLP(X) and BOUNDP(X)
                                  and isDomain(U := EVAL(X))) =>
        spadUntrace(U, options)
    EQCAR(options, 'ALIAS) =>
        if $traceNoisely then
            sayBrightly(["%b", options.1, "%d", '"**untraced"])
        $timer_list := remove_equal(STRINGIMAGE(options.1), $timer_list)
        $count_list := remove_equal(STRINGIMAGE(options.1), $count_list)
        $mathTraceList := remove_equal(options.1, $mathTraceList)
        UNEMBED(X)
    NOT(MEMBER(X, $trace_names)) and NOT(isSubForRedundantMapName(X)) =>
        sayBrightly(["%b", rassocSub(X, $mapSubNameAlist),
                     "%d", '"not traced"])
    $trace_names := remove_equal(X, $trace_names)
    $mathTraceList := REMOVE((STRINGP(X) => INTERN(X); X), $mathTraceList)
    $letAssoc := DELASC(X, $letAssoc)
    Y := (IS_GENVAR(X) => devaluate(EVAL(X)); X)
    $timer_list := remove_equal(STRINGIMAGE(Y), $timer_list)
    SET(INTERN(STRCONC(Y, ",TIMER")), 0)
    $count_list := remove_equal(STRINGIMAGE(Y), $count_list)
    SET(INTERN(STRCONC(Y, ",COUNT")), 0)
    if $traceNoisely and NOT(isSubForRedundantMapName(Y)) then
        sayBrightly(["%b", rassocSub(Y, $mapSubNameAlist),
                     "%d", '"untraced"])
    UNEMBED(X)

BPITRACE(bin_def, alias, modemap, options) ==
    trace3(GENSYM(), modemap, [["ALIAS", alias], :options], bin_def)

getTraceOptions options ==
  $traceErrorStack: local := nil
  optionList:= [getTraceOption x for x in options]
  $traceErrorStack =>
    null rest $traceErrorStack =>
      [key,parms] := first $traceErrorStack
      throwKeyedMsg(key,['"",:parms])
    throwListOfKeyedMsgs("S2IT0017",[# $traceErrorStack],
      NREVERSE $traceErrorStack)
  optionList

saveMapSig(funNames) ==
  for name in funNames repeat
    map:= rassoc(name,$mapSubNameAlist) =>
      $tracedMapSignatures:= ADDASSOC(name,getMapSig(map,name),
        $tracedMapSignatures)

getMapSig(mapName,subName) ==
  lmms:= get(mapName,'localModemap,$InteractiveFrame) =>
    for mm in lmms until sig repeat
      CADR mm = subName => sig:= CDAR mm
    sig

getTraceOption (x is [key,:l]) ==
  key:= selectOptionLC(key,$traceOptionList,'traceOptionError)
  x := [key,:l]
  MEMQ(key,'(nonquietly timer nt)) => x
  key='break =>
    null l => ['break,'before]
    opts := [selectOptionLC(y,'(before after),NIL) for y in l]
    and/[IDENTP y for y in opts] => ['break,:opts]
    stackTraceOptionError ["S2IT0008",NIL]
  key='restore =>
    null l => x
    stackTraceOptionError ["S2IT0009",[STRCONC('")",object2String key)]]
  key='only => ['only,:transOnlyOption l]
  key='within =>
    l is [a] and IDENTP a => x
    stackTraceOptionError ["S2IT0010",['")within"]]
  MEMQ(key,'(cond before after)) =>
    key:=
      key="cond" => "when"
      key
    l is [a] => [key,:l]
    stackTraceOptionError ["S2IT0011",[STRCONC('")",object2String key)]]
  key='depth =>
    l is [n] and FIXP n => x
    stackTraceOptionError ["S2IT0012",['")depth"]]
  key='count =>
    (null l) or (l is [n] and FIXP n) => x
    stackTraceOptionError ["S2IT0012",['")count"]]
  key="of" =>
    ["of",:[hn y for y in l]] where
      hn x ==
        atom x and not UPPER_-CASE_-P (STRINGIMAGE x).(0) =>
          isDomainOrPackage EVAL x => x
          stackTraceOptionError ["S2IT0013",[x]]
        g:= domainToGenvar x => g
        stackTraceOptionError ["S2IT0013",[x]]
  MEMQ(key,'(local ops vars)) =>
    null l or l is ["all"] => [key,:"all"]
    isListOfIdentifiersOrStrings l => x
    stackTraceOptionError ["S2IT0015",[STRCONC('")",object2String key)]]
  key='varbreak =>
    null l or l is ["all"] => ["varbreak",:"all"]
    isListOfIdentifiers l => x
    stackTraceOptionError ["S2IT0016",[STRCONC('")",object2String key)]]
  key='mathprint =>
    null l => x
    stackTraceOptionError ["S2IT0009",[STRCONC('")",object2String key)]]
  key => throwKeyedMsg("S2IT0005",[key])

traceOptionError(opt,keys) ==
  null keys => stackTraceOptionError ["S2IT0007",[opt]]
  commandAmbiguityError("trace option",opt,keys)

resetTimers () ==
  for timer in $timer_list repeat
    SET(INTERN STRCONC(timer,'"_,TIMER"),0)

resetSpacers () ==
  for spacer in _/SPACELIST repeat
    SET(INTERN STRCONC(spacer,'"_,SPACE"),0)

resetCounters () ==
  for k in $count_list repeat
    SET(INTERN STRCONC(k,'"_,COUNT"),0)

ptimers() ==
  null $timer_list => sayBrightly '"   no functions are timed"
  for timer in $timer_list repeat
    sayBrightly ["  ",:bright timer,'_:,'" ",
      EVAL(INTERN STRCONC(timer, '"_,TIMER")) /
        FLOAT($timerTicksPerSecond, 0.0), '" sec."]

pspacers() ==
  null _/SPACELIST => sayBrightly '"   no functions have space monitored"
  for spacer in _/SPACELIST repeat
    sayBrightly ["  ",:bright spacer,'_:,'" ",
      EVAL INTERN STRCONC(spacer,'"_,SPACE"),'" bytes"]

pcounters() ==
  null $count_list => sayBrightly '"   no functions are being counted"
  for k in $count_list repeat
    sayBrightly ["  ",:bright k,'_:,'" ",
      EVAL INTERN STRCONC(k,'"_,COUNT"),'" times"]

transOnlyOption l ==
  l is [n,:y] =>
    FIXP n => [n,:transOnlyOption y]
    MEMQ(n:= UPCASE n,'(V A C)) => [n,:transOnlyOption y]
    stackTraceOptionError ["S2IT0006",[n]]
    transOnlyOption y
  nil

stackTraceOptionError x ==
  $traceErrorStack:= [x,:$traceErrorStack]
  nil

removeOption(op,options) ==
  [optEntry for (optEntry:=[opt,:.]) in options | opt ~= op]

domainToGenvar x ==
  $doNotAddEmptyModeIfTrue: local:= true
  (y:= unabbrevAndLoad x) and GETDATABASE(opOf y,'CONSTRUCTORKIND) = 'domain =>
    g:= genDomainTraceName y
    SET(g,evalDomain y)
    g

genDomainTraceName y ==
  u:= LASSOC(y,$domainTraceNameAssoc) => u
  g:= GENVAR()
  $domainTraceNameAssoc:= [[y,:g],:$domainTraceNameAssoc]
  g

--this is now called from trace with the )off option
untrace l ==
  $lastUntraced:=
    null l => COPY $trace_names
    l
  untraceList:= [transTraceItem x for x in l]
  for funName in untraceList repeat
      untrace2(lassocSub(funName, $mapSubNameAlist), [])
  removeTracedMapSigs untraceList

transTraceItem x ==
  $doNotAddEmptyModeIfTrue: local:=true
  atom x =>
    (value:=get(x,"value",$InteractiveFrame)) and
      (objMode value in '((Mode) (Type) (Category))) =>
        x := objVal value
        (y:= domainToGenvar x) => y
        x
    UPPER_-CASE_-P (STRINGIMAGE x).(0) =>
      y := unabbrev x
      constructor?(y) => y
      PAIRP(y) and constructor?(first y) => first y
      (y:= domainToGenvar x) => y
      x
    x
  VECP first x => transTraceItem devaluate first x
  y:= domainToGenvar x => y
  throwKeyedMsg("S2IT0018",[x])

removeTracedMapSigs untraceList ==
  for name in untraceList repeat
    REMPROP(name,$tracedMapSignatures)

coerceTraceArgs2E(traceName,subName,args) ==
  MEMQ(name:= subName,$mathTraceList) =>
    SPADSYSNAMEP PNAME name => coerceSpadArgs2E(reverse rest reverse args)
    [["=",name,objValUnwrap coerceInteractive(objNewWrap(arg,type),$OutputForm)]
      for name in '(arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 )
       for arg in args for type in rest LASSOC(subName,
        $tracedMapSignatures)]
  SPADSYSNAMEP PNAME name => reverse rest reverse args
  args

coerceSpadArgs2E(args) ==
  -- following binding is to prevent forcing calculation of stream elements
  $streamCount:local := 0
  [["=",name,objValUnwrap coerceInteractive(objNewWrap(arg,type),$OutputForm)]
      for name in '(arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 )
        for arg in args for type in rest $tracedSpadModemap]

subTypes(mm,sublist) ==
  ATOM mm =>
    (s:= LASSOC(mm,sublist)) => s
    mm
  [subTypes(m,sublist) for m in mm]

coerceTraceFunValue2E(traceName,subName,value) ==
  MEMQ(name:= subName,$mathTraceList) =>
    SPADSYSNAMEP PNAME traceName => coerceSpadFunValue2E(value)
    (u:=LASSOC(subName,$tracedMapSignatures)) =>
      objValUnwrap coerceInteractive(objNewWrap(value, first u), $OutputForm)
    value
  value

coerceSpadFunValue2E(value) ==
  -- following binding is to prevent forcing calculation of stream elements
  $streamCount:local := 0
  objValUnwrap coerceInteractive(objNewWrap(value, first $tracedSpadModemap),
    $OutputForm)

isListOfIdentifiers l == and/[IDENTP x for x in l]

isListOfIdentifiersOrStrings l == and/[IDENTP x or STRINGP x for x in l]

getMapSubNames(l) ==
  subs:= nil
  for mapName in l repeat
    lmm:= get(mapName,'localModemap,$InteractiveFrame) =>
      subs:= APPEND([[mapName,:CADR mm] for mm in lmm],subs)
  union(subs, getPreviousMapSubNames UNIONQ($trace_names,
    $lastUntraced))

getPreviousMapSubNames(traceNames) ==
  subs:= nil
  for mapName in ASSOCLEFT CAAR $InteractiveFrame repeat
    lmm:= get(mapName,'localModemap,$InteractiveFrame) =>
      MEMQ(CADAR lmm,traceNames) =>
        for mm in lmm repeat
          subs:= [[mapName,:CADR mm],:subs]
  subs

lassocSub(x,subs)  ==
  y := QLASSQ(x, subs) => y
  x

rassocSub(x,subs) ==
  y:= rassoc(x,subs) => y
  x

isUncompiledMap(x) ==
  y:= get(x,'value,$InteractiveFrame) =>
    (CAAR y) = 'SPADMAP and null get(x,'localModemap,$InteractiveFrame)

isInterpOnlyMap(map) ==
  x:= get(map,'localModemap,$InteractiveFrame) =>
    (CAAAR x) = 'interpOnly

augmentTraceNames(l,mapSubNames) ==
  res:= nil
  for traceName in l repeat
    mml:= get(traceName,'localModemap,$InteractiveFrame) =>
      res:= APPEND([CADR mm for mm in mml],res)
    res:= [traceName,:res]
  res

isSubForRedundantMapName(subName) ==
  mapName:= rassocSub(subName,$mapSubNameAlist) =>
    tail:=member([mapName,:subName],$mapSubNameAlist) =>
      MEMQ(mapName, rest ASSOCLEFT tail)

untraceMapSubNames traceNames ==
  null($mapSubNameAlist:local:= getPreviousMapSubNames traceNames) => nil
  for name in (subs:= ASSOCRIGHT $mapSubNameAlist)
    | MEMQ(name, $trace_names) repeat
      untrace2(name, [])
      $lastUntraced:= SETDIFFERENCE($lastUntraced,subs)

funfind(functor, opname) ==
  ops:= isFunctor functor
  [u for u in ops | u is [[ =opname,:.],:.]]

isDomainOrPackage dom ==
  REFVECP dom and #dom>0 and isFunctor opOf dom.(0)

isTraceGensym x == GENSYMP x

spadTrace(domain,options) ==
  $fromSpadTrace:= true
  PAIRP domain and REFVECP first domain and (first domain).0 = 0 =>
      aldorTrace(domain,options)
  not isDomainOrPackage domain => userError '"bad argument to trace"
  listOfOperations:=
    [g x for x in getOption("OPS",options)] where
      g x ==
        STRINGP x => INTERN x
        x
  if listOfVariables := getOption("VARS",options) then
    options := removeOption("VARS",options)
  if listOfBreakVars := getOption("VARBREAK",options) then
    options := removeOption("VARBREAK",options)
  anyifTrue:= null listOfOperations
  domainId:= opOf domain.(0)
  currentEntry := assoc(domain, $trace_names)
  currentAlist:= IFCDR currentEntry
  opStructureList:= flattenOperationAlist getOperationAlistFromLisplib domainId
  sigSlotNumberAlist:=
    [triple
      --new form is (<op> <signature> <slotNumber> <condition> <kind>)
      for [op,sig,n,.,kind] in opStructureList | kind = 'ELT
        and (anyifTrue or MEMQ(op,listOfOperations)) and
         FIXP n and
          isTraceable(triple:= [op,sig,n],domain)] where
            isTraceable(x is [.,.,n,:.],domain) ==
              atom domain.n => nil
              functionSlot:= first domain.n
              GENSYMP functionSlot =>
                (reportSpadTrace("Already Traced",x); nil)
              null (BPINAME functionSlot) =>
                (reportSpadTrace("No function for",x); nil)
              true
  if listOfVariables then
    for [.,.,n] in sigSlotNumberAlist repeat
      fn := first domain.n
      $letAssoc := AS_-INSERT(BPINAME fn,
        listOfVariables,$letAssoc)
  if listOfBreakVars then
    for [.,.,n] in sigSlotNumberAlist repeat
      fn := first domain.n
      $letAssoc := AS_-INSERT(BPINAME fn,
        [["BREAK",:listOfBreakVars]],$letAssoc)
  for (pair:= [op,mm,n]) in sigSlotNumberAlist repeat
    alias:= spadTraceAlias(domainId,op,n)
    tracedModemap := subTypes(mm, constructSubst(domain.0))
    dn1 := first domain.n
    fgg := FUNCTION newGoGet
    tf :=
        dn1 = fgg =>
            goGetTracerHelper(domain.n, fgg, pair, alias,
                                options, tracedModemap)
        BPITRACE(dn1, alias, tracedModemap, options)
    NCONC(pair, [listOfVariables, first domain.n])
    rplac(first domain.n, tf)
  sigSlotNumberAlist:= [x for x in sigSlotNumberAlist | CDDDR x]
  if $reportSpadTrace then
    if $traceNoisely then printDashedLine()
    for x in orderBySlotNumber sigSlotNumberAlist repeat
      reportSpadTrace("TRACING",x)
  currentEntry =>
    rplac(rest currentEntry,[:sigSlotNumberAlist,:currentAlist])
  $trace_names := [[domain, :sigSlotNumberAlist], :$trace_names]
  spadReply()

goGetTracer(l, dn, f, tlst, alias, options, modemap) ==
    rplac(first dn, f)
    [:arglist, env] := l
    slot := replaceGoGetSlot env
    tlst.4 := first slot
    nf := BPITRACE(first slot, alias, modemap, options)
    rplac(first slot, nf)
    APPLY(first slot, [:arglist, rest slot])  --SPADCALL it!

traceDomainLocalOps(dom,lops,options) ==
 sayMSG ['"  ",'"The )local option has been withdrawn"]
 sayMSG ['"  ",'"Use )ltr to trace local functions."]
 NIL
--  abb := abbreviate dom
--  loadLibIfNotLoaded abb
--  actualLops := getLocalOpsFromLisplib abb
--  null actualLops =>
--    sayMSG ['"  ",:bright abb,'"has no local functions to trace."]
--  lops = 'all => _/TRACE_,1(actualLops,options)
--  l := NIL
--  for lop in lops repeat
--    internalName := INTERN STRCONC(PNAME abb,'";",PNAME lop)
--    not MEMQ(internalName,actualLops) =>
--      sayMSG ['"  ",:bright abb,'"does not have a local",
--        '" function called",:bright lop]
--    l := cons(internalName,l)
--  l => _/TRACE_,1(l,options)
--  nil

untraceDomainLocalOps(dom,lops) ==
 sayMSG ['"  ",:bright abb,'"has no local functions to untrace."]
 NIL
--  lops = "all" => untraceAllDomainLocalOps(dom)
--  abb := abbreviate dom
--  loadLibIfNotLoaded abb
--  actualLops := getLocalOpsFromLisplib abb
--  null actualLops =>
--    sayMSG ['"  ",:bright abb,'"has no local functions to untrace."]
--  l := NIL
--  for lop in lops repeat
--    internalName := INTERN STRCONC(PNAME abb,'";",PNAME lop)
--    not MEMQ(internalName,actualLops) =>
--      sayMSG ['"  ",:bright abb,'"does not have a local",
--        '" function called",:bright lop]
--    l := cons(internalName,l)
--  l => untrace l
--  nil

untraceAllDomainLocalOps(dom) == NIL
--  abb := abbreviate dom
--  actualLops := getLocalOpsFromLisplib abb
--  null (l := intersection(actualLops, $trace_names)) => NIL
--  _/UNTRACE_,1(l,NIL)
--  NIL

traceDomainConstructor(domainConstructor,options) ==
  -- Trace all domains built with the given domain constructor,
  -- including all presently instantiated domains, and all future
  -- instantiations, while domain constructor is traced.
  loadFunctor domainConstructor
  listOfLocalOps := getOption("LOCAL",options)
  if listOfLocalOps then
    traceDomainLocalOps(domainConstructor,listOfLocalOps,
      [opt for opt in options | opt isnt ['LOCAL,:.]])
  listOfLocalOps and not getOption("OPS",options) => NIL
  for [argl,.,:domain] in HGET($ConstructorCache,domainConstructor)
    repeat spadTrace(domain,options)
  $trace_names := [domainConstructor, :$trace_names]
  innerDomainConstructor := INTERN STRCONC(domainConstructor,'";")
  if FBOUNDP innerDomainConstructor then domainConstructor := innerDomainConstructor
  EMBED(domainConstructor,
    ['LAMBDA, ['_&REST, 'args],
      ['PROG, ['domain],
        ['SETQ,'domain,['APPLY,domainConstructor,'args]],
        ['spadTrace,'domain,MKQ options],
        ['RETURN,'domain]]] )

untraceDomainConstructor domainConstructor ==
  --untrace all the domains in domainConstructor, and unembed it
  $trace_names :=
    [df for df in $trace_names | keepTraced?(df, domainConstructor)] where
      keepTraced?(df, domainConstructor) ==
        (df is [dc,:.]) and (isDomainOrPackage dc) and
           ((IFCAR devaluate dc) = domainConstructor) =>
               untrace2(dc, [])
               false
        true
  untraceAllDomainLocalOps domainConstructor
  innerDomainConstructor := INTERN STRCONC(domainConstructor,'";")
  if FBOUNDP innerDomainConstructor then UNEMBED innerDomainConstructor
    else UNEMBED domainConstructor
  $trace_names := delete(domainConstructor, $trace_names)

flattenOperationAlist(opAlist) ==
   res:= nil
   for [op,:mmList] in opAlist repeat
     res:=[:res,:[[op,:mm] for mm in mmList]]
   res

mapLetPrint(x,val,currentFunction) ==
  x:= getAliasIfTracedMapParameter(x,currentFunction)
  currentFunction:= getBpiNameIfTracedMap currentFunction
  letPrint(x,val,currentFunction)

-- This is the version for use when we have no idea
-- what print representation to use for the data object

letPrint(x,val,currentFunction) ==
  if $letAssoc and
    ((y:= LASSOC(currentFunction,$letAssoc)) or (y:= LASSOC("all",$letAssoc))) then
      if (y="all" or MEMQ(x,y)) and
        not (IS_GENVAR(x) or isSharpVarWithNum(x) or GENSYMP x) then
         sayBrightlyNT [:bright x,": "]
         PRIN0 shortenForPrinting val
         TERPRI()
      if (y:= hasPair("BREAK",y)) and
        (y="all" or MEMQ(x,y) and
          (not MEMQ((PNAME x).(0),'($ _#)) and not GENSYMP x)) then
            break [:bright currentFunction,'"breaks after",:bright x,'":= ",
              shortenForPrinting val]
  val

-- This is the version for use when we have already
-- converted the data into type "Expression"
letPrint2(x,printform,currentFunction) ==
  $BreakMode:local := nil
  if $letAssoc and
    ((y:= LASSOC(currentFunction,$letAssoc)) or (y:= LASSOC("all",$letAssoc))) then
      if (y="all" or MEMQ(x,y)) and
        not (IS_GENVAR(x) or isSharpVarWithNum(x) or GENSYMP x) then
         $BreakMode:='letPrint2
         flag := CATCH('letPrint2, mathprint ["=",x,printform], true)
         if not(flag) then PRINT(printform)
      if (y:= hasPair("BREAK",y)) and
        (y="all" or MEMQ(x,y) and
          (not MEMQ((PNAME x).(0),'($ _#)) and not GENSYMP x)) then
            break [:bright currentFunction,'"breaks after",:bright x,":= ",
              printform]
  x

-- This is the version for use when we have our hands on a function
-- to convert the data into type "Expression"

letPrint3(x,xval,printfn,currentFunction) ==
  $BreakMode:local := nil
  if $letAssoc and
    ((y:= LASSOC(currentFunction,$letAssoc)) or (y:= LASSOC("all",$letAssoc))) then
      if (y="all" or MEMQ(x,y)) and
        not (IS_GENVAR(x) or isSharpVarWithNum(x) or GENSYMP x) then
         $BreakMode:='letPrint2
         flag := CATCH('letPrint2,
                       mathprint ["=", x, SPADCALL(xval, printfn)], true)
         if not(flag) then PRINT(xval)
      if (y:= hasPair("BREAK",y)) and
        (y="all" or MEMQ(x,y) and
          (not MEMQ((PNAME x).(0),'($ _#)) and not GENSYMP x)) then
            break [:bright currentFunction,'"breaks after",:bright x,'":= ",
              xval]
  x

getAliasIfTracedMapParameter(x,currentFunction) ==
  isSharpVarWithNum x =>
    aliasList:= get(currentFunction,'alias,$InteractiveFrame) =>
      aliasList.(STRING2PINT_-N(SUBSTRING(PNAME x,1,NIL),1)-1)
  x

getBpiNameIfTracedMap(name) ==
  lmm:= get(name,'localModemap,$InteractiveFrame) =>
      MEMQ(bpiName := CADAR lmm, $trace_names) => bpiName
  name

hasPair(key,l) ==
  atom l => nil
  l is [[ =key,:a],:.] => a
  hasPair(key,rest l)

shortenForPrinting val ==
  isDomainOrPackage val => devaluate val
  val

spadTraceAlias(domainId,op,n) ==
  INTERNL(domainId,".",op,",",STRINGIMAGE n)

getOption(opt,l) ==
  y:= assoc(opt,l) => rest y

reportSpadTrace(header,[op,sig,n,:t]) ==
  null $traceNoisely => nil
  msg:= [header,'%b,op,":",'%d,rest sig," -> ",first sig," in slot ",n]
  namePart:= nil --(t is (.,.,name,:.) => (" named ",name); NIL)
  tracePart:=
    t is [y,:.] and not null y =>
      (y="all" => ['%b,"all",'%d,"vars"]; [" vars: ",y])
    NIL
  sayBrightly [:msg,:namePart,:tracePart]

orderBySlotNumber l ==
  ASSOCRIGHT orderList [[n,:x] for (x:= [.,.,n,:.]) in l]

_/TRACEREPLY() ==
  null $trace_names => MAKESTRING '"   Nothing is traced."
  for x in $trace_names repeat
    x is [d,:.] and isDomainOrPackage d =>
      domainList:= [devaluate d,:domainList]
    functionList:= [x,:functionList]
  [:functionList,:domainList,"traced"]

spadReply() ==
  [printName x for x in $trace_names] where
    printName x ==
      x is [d,:.] and isDomainOrPackage d => devaluate d
      x

spadUntrace(domain,options) ==
  not isDomainOrPackage domain => userError '"bad argument to untrace"
  anyifTrue:= null options
  listOfOperations:= getOption("ops:",options)
  domainId := devaluate domain
  null (pair := assoc(domain, $trace_names)) =>
    sayMSG ['"   No functions in",
      :bright prefix2String domainId,'"are now traced."]
  sigSlotNumberAlist:= rest pair
  for (pair := [op, sig, n, lv, bpiPointer]) in sigSlotNumberAlist |
    anyifTrue or MEMQ(op,listOfOperations) repeat
      rplac(first domain.n, bpiPointer)
      rplac(CDDDR pair, nil)
      if assocPair:= assoc(BPINAME bpiPointer,$letAssoc) then
        $letAssoc := REMOVER($letAssoc,assocPair)
  newSigSlotNumberAlist:= [x for x in sigSlotNumberAlist | CDDDR x]
  newSigSlotNumberAlist => rplac(rest pair, newSigSlotNumberAlist)
  $trace_names := DELASC(domain, $trace_names)
  spadReply()

prTraceNames() ==
  (for x in $trace_names repeat PRINT fn x; nil) where
    fn x ==
      x is [d,:t] and isDomainOrPackage d => [devaluate d,:t]
      x

traceReply() ==
  $domains: local:= nil
  $packages: local:= nil
  $constructors: local:= nil
  null $trace_names =>
    sayMessage '"   Nothing is traced now."
  sayBrightly '" "
  for x in $trace_names repeat
    x is [d,:.] and (isDomainOrPackage d) => addTraceItem d
    atom x =>
      isFunctor x => addTraceItem x
      (IS_GENVAR x =>
        addTraceItem EVAL x; functionList:= [x,:functionList])
    userError '"bad argument to trace"
  functionList:= "append"/[[rassocSub(x,$mapSubNameAlist),'" "]
    for x in functionList | not isSubForRedundantMapName x]
  if functionList then
    2 = #functionList =>
      sayMSG ["   Function traced: ",:functionList]
    (22 + sayBrightlyLength functionList) <= $LINELENGTH =>
      sayMSG ["   Functions traced: ",:functionList]
    sayBrightly "   Functions traced:"
    sayBrightly flowSegmentedMsg(functionList,$LINELENGTH,6)
  if $domains then
    displayList:= concat(prefix2String first $domains,
          [:concat('",",'" ",prefix2String x) for x in rest $domains])
    if atom displayList then displayList:= [displayList]
    sayBrightly '"   Domains traced: "
    sayBrightly flowSegmentedMsg(displayList,$LINELENGTH,6)
  if $packages then
    displayList:= concat(prefix2String first $packages,
          [:concat(", ",prefix2String x) for x in rest $packages])
    if atom displayList then displayList:= [displayList]
    sayBrightly '"   Packages traced: "
    sayBrightly flowSegmentedMsg(displayList,$LINELENGTH,6)
  if $constructors then
    displayList:= concat(abbreviate first $constructors,
          [:concat(", ",abbreviate x) for x in rest $constructors])
    if atom displayList then displayList:= [displayList]
    sayBrightly '"   Parameterized constructors traced:"
    sayBrightly flowSegmentedMsg(displayList,$LINELENGTH,6)

addTraceItem d ==
  constructor? d => $constructors:=[d,:$constructors]
  isDomain d => $domains:= [devaluate d,:$domains]
  isDomainOrPackage d => $packages:= [devaluate d,:$packages]

_?t() ==
  null $trace_names => sayMSG bright '"nothing is traced"
  for x in $trace_names | atom x and not IS_GENVAR x repeat
    if llm:= get(x,'localModemap,$InteractiveFrame) then
      x:= (LIST (CADAR llm))
    sayMSG ['"Function",:bright rassocSub(x,$mapSubNameAlist),'"traced"]
  for x in $trace_names | x is [d, :l] and isDomainOrPackage d repeat
    suffix:=
      isDomain d => '"domain"
      '"package"
    sayBrightly ['"   Functions traced in ",suffix,'%b,devaluate d,'%d,":"]
    for x in orderBySlotNumber l repeat reportSpadTrace("   ",take(4,x))
    TERPRI()

tracelet(fn, bin_def, vars) ==
  if bin_def and stupidIsSpadFunction bin_def then
    if COMPILED_-FUNCTION_-P bin_def then fn := BPINAME bin_def
  fn = 'Undef => nil
  vars:=
    vars="all" => "all"
    l:= LASSOC(fn,$letAssoc) => union(vars,l)
    vars
  $letAssoc:= [[fn,:vars],:$letAssoc]
  $TRACELETFLAG : local := true
  $QuickLet : local := false
  not MEMQ(fn, $traceletFunctions) and not IS_GENVAR fn and COMPILED_-FUNCTION_-P SYMBOL_-FUNCTION fn
    and not stupidIsSpadFunction fn and not GENSYMP fn =>
      ($traceletFunctions:= [fn,:$traceletFunctions]; compileBoot fn ;
       $traceletFunctions:= delete(fn,$traceletFunctions) )

breaklet(fn, bin_def, vars) ==
                       --vars is "all" or a list of variables
  --$letAssoc ==> (.. (=fn .. (BREAK . all))) OR (.. (=fn .. (BREAK . vl)))
  if bin_def and stupidIsSpadFunction bin_def then
    if COMPILED_-FUNCTION_-P bin_def then fn := BPINAME bin_def
  fn = "Undef" => nil
  fnEntry:= LASSOC(fn,$letAssoc)
  vars:=
    pair:= assoc("BREAK",fnEntry) => union(vars,rest pair)
    vars
  $letAssoc:=
    null fnEntry => [[fn,:LIST ["BREAK",:vars]],:$letAssoc]
    pair => (RPLACD(pair,vars); $letAssoc)
  $QuickLet:local := false
  not MEMQ(fn,$traceletFunctions) and not stupidIsSpadFunction fn
    and not GENSYMP fn =>
      $traceletFunctions:= [fn,:$traceletFunctions]
      compileBoot fn
      $traceletFunctions:= delete(fn,$traceletFunctions)

stupidIsSpadFunction fn ==
  -- returns true if the function pname has a semi-colon in it
  -- eventually, this will use isSpadFunction from luke boot
  STRPOS('"_;",PNAME fn,0,NIL)

break msg ==
  condition:= MONITOR_-EVALTRAN(_/BREAKCONDITION, nil)
  -- The next line is to try to deal with some reported cases of unwanted
  -- backtraces appearing, MCD.
  ENABLE_-BACKTRACE(nil)
  EVAL condition =>
    sayBrightly msg
    INTERRUPT()

compileBoot fn ==
  SAY("need to recompile: ", fn)
