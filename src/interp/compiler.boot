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

DEFPARAMETER($currentFunctionLevel, 0)
DEFPARAMETER($tryRecompileArguments, true)
DEFPARAMETER($insideCompTypeOf, false)
DEFPARAMETER($locVarsTypes, nil)

initEnvHashTable(l) ==
  for u in first(first(l)) repeat
      for v in rest(u) repeat
            HPUT($envHashTable, [first u, first v], true)

compTopLevel(x,m,e) ==
  $killOptimizeIfTrue: local:= false
  $forceAdd: local:= false
  $compTimeSum: local := 0
  $resolveTimeSum: local := 0
  $envHashTable: local := MAKE_-HASHTABLE 'EQUAL
  initEnvHashTable(e)
  initEnvHashTable($CategoryFrame)
  -- The next line allows the new compiler to be tested interactively.
  compFun := 'compOrCroak
  x is ["DEF",:.] or x is ["where",["DEF",:.],:.] =>
    ([val,mode,.]:= FUNCALL(compFun,x,m,e); [val,mode,e])
        --keep old environment after top level function defs
  FUNCALL(compFun,x,m,e)

compUniquely(x,m,e) ==
  $compUniquelyIfTrue: local:= true
  CATCH("compUniquely",comp(x,m,e))

compOrCroak(x,m,e) == compOrCroak1(x,m,e,'comp)

compOrCroak1(x,m,e,compFn) ==
  fn(x,m,e,nil,nil,compFn) where
    fn(x,m,e,$compStack,$compErrorMessageStack,compFn) ==
      T:= CATCH("compOrCroak",FUNCALL(compFn,x,m,e)) => T
      --stackAndThrow here and moan in UT LISP K does the appropriate THROW
      $compStack:= [[x,m,e,$exitModeStack],:$compStack]
      $s:=
        compactify $compStack where
          compactify al ==
            null al => nil
            LASSOC(first first al,rest al) => compactify rest al
            [first al,:compactify rest al]
      $level:= #$s
      errorMessage:=
        if $compErrorMessageStack
           then first $compErrorMessageStack
           else "unspecified error"
      $scanIfTrue =>
        stackSemanticError(errorMessage,mkErrorExpr $level)
        ["failedCompilation",m,e]
      displaySemanticErrors()
      SAY("****** comp fails at level ",$level," with expression: ******")
      displayComp $level
      userError errorMessage

tc() ==
  comp($x,$m,$f)


comp(x,m,e) ==
  T:= compNoStacking(x,m,e) => ($compStack:= nil; T)
  $compStack:= [[x,m,e,$exitModeStack],:$compStack]
  nil

compNoStacking(x,m,e) ==
  T:= comp2(x,m,e) =>
    (m=$EmptyMode and T.mode=$Representation => [T.expr,"$",T.env]; T)
         --$Representation is bound in compDefineFunctor, set by doIt
         --this hack says that when something is undeclared, $ is
         --preferred to the underlying representation -- RDJ 9/12/83
  compNoStacking1(x,m,e,$compStack)

compNoStacking1(x,m,e,$compStack) ==
  u:= get(if m="$" then "Rep" else m,"value",e) =>
    (T:= comp2(x,u.expr,e) => [T.expr,m,T.env]; nil)
  nil

comp2(x,m,e) ==
  [y,m',e]:= comp3(x,m,e) or return nil
  --if null atom y and isDomainForm(y,e) then e := addDomain(x,e)
        --line commented out to prevent adding derived domain forms
  m~=m' and ($bootStrapMode or isDomainForm(m',e))=>[y,m',addDomain(m',e)]
        --isDomainForm test needed to prevent error while compiling Ring
        --$bootStrapMode-test necessary for compiling Ring in $bootStrapMode
  [y,m',e]

comp3(x,m,$e) ==
  --returns a Triple or else nil to signal can't do
  $e:= addDomain(m,$e)
  e:= $e --for debugging purposes
  m is ["Mapping",:.] => compWithMappingMode(x,m,e)
  m is ["QUOTE",a] => (x=a => [x,m,$e]; nil)
  STRINGP m => (atom x => (m=x or m=STRINGIMAGE x => [m,m,e]; nil); nil)
  not x or atom x => compAtom(x,m,e)
  op:= first x
  getmode(op,e) is ["Mapping",:ml] and (u:= applyMapping(x,m,e,ml)) => u
  op=":" => compColon(x,m,e)
  op="::" => compCoerce(x,m,e)
  not ($insideCompTypeOf=true) and stringPrefix?('"TypeOf",PNAME op) =>
    compTypeOf(x,m,e)
  t:= compExpression(x,m,e)
  t is [x',m',e'] and not member(m',getDomainsInScope e') =>
    [x',m',addDomain(m',e')]
  t

compTypeOf(x:=[op,:argl],m,e) ==
  $insideCompTypeOf: local := true
  newModemap:= EQSUBSTLIST(argl,$FormalMapVariableList,get(op,'modemap,e))
  e:= put(op,'modemap,newModemap,e)
  comp3(x,m,e)

hasFormalMapVariable(x, vl) ==
  $formalMapVariables: local := vl
  null vl => false
  ScanOrPairVec(function hasone?, x) where
     hasone? x == MEMQ(x,$formalMapVariables)

argsToSig(args) ==
    args is [":", v, t] => [[v], [t]]
    sig1 := []
    arg1 := []
    bad := false
    for arg in args repeat
        arg is [":", v, t] =>
             sig1 := [t, :sig1]
             arg1 := [v, :arg1]
        bad := true
    bad => [nil, nil]
    [REVERSE(arg1), REVERSE(sig1)]

compLambda(x is ["+->", vl, body], m, e) ==
    vl is [":", args, target] =>
        args :=
             args is ["@Tuple", :a1] => a1
             args
        LISTP(args) =>
             [arg1, sig1] := argsToSig(args)
             sig1 or NULL(args) =>
                 ress := compAtSign(["@", ["+->", arg1, body],
                                  ["Mapping", target, :sig1]], m, e)
                 ress
             stackAndThrow ["compLambda: malformed argument list", x]
        stackAndThrow ["compLambda: malformed argument list", x]
    stackAndThrow ["compLambda: signature needed", x]

getFreeList(u, bound, free, e) ==
    atom u =>
        not IDENTP u => free
        MEMQ(u,bound) => free
        v := ASSQ(u, free) =>
            RPLACD(v, 1 + CDR v)
            free
        not getmode(u, e) => free
        [[u, :1], :free]
    op := first u
    MEMQ(op, '(QUOTE GO function)) => free
    EQ(op, 'LAMBDA) =>
        lvl := CADR u
        avl := []
        for evl in lvl repeat
            el :=
                ATOM(evl) => evl
                first(evl)
            avl := [el, :avl]
        bound := UNIONQ(bound, avl)
        for v in CDDR u repeat
            free := getFreeList(v, bound, free, e)
        free
    EQ(op, 'PROG) =>
        bound := UNIONQ(bound, CADR u)
        for v in CDDR u | NOT ATOM v repeat
            free := getFreeList(v, bound, free, e)
        free
    EQ(op, 'SPROG) =>
        bound := UNIONQ(bound, [first uu for uu in CADR u])
        for v in CDDR u | NOT ATOM v repeat
            free := getFreeList(v, bound, free, e)
        free
    EQ(op, 'SEQ) =>
        for v in rest u | NOT ATOM v repeat
            free := getFreeList(v, bound, free, e)
        free
    EQ(op, 'COND) =>
        for v in rest u repeat
            for vv in v repeat
                free := getFreeList(vv, bound, free, e)
        free
    if ATOM op then u := rest u  --Atomic functions aren't descended
    for v in u repeat
        free := getFreeList(v, bound, free, e)
    free

compWithMappingMode(x, m, oldE) ==
  compWithMappingMode1(x, m, oldE, $formalArgList)

compWithMappingMode1(x, m is ["Mapping", m', :sl], oldE, $formalArgList) ==
  $killOptimizeIfTrue: local:= true
  e:= oldE
  isFunctor x =>
    if get(x,"modemap",$CategoryFrame) is [[[.,target,:argModeList],.],:.] and
      (and/[extendsCategoryForm("$",s,mode) for mode in argModeList for s in sl]
        ) and extendsCategoryForm("$",target,m') then return [x,m,e]
  if STRINGP x then x:= INTERN x
  ress := nil
  old_style := true
  if x is ["+->", vl, nx] then
      old_style := false
      vl is [":", :.] =>
         ress := compLambda(x,m,oldE)
         -- In case Boot gets fixed
         ress
      vl :=
          vl is ["@Tuple", :vl1] => vl1
          vl
      vl :=
         IDENTP(vl) => [vl]
         LISTP(vl) and (and/[SYMBOLP(v) for v in vl])=> vl
         stackAndThrow ["bad +-> arguments:", vl]
      $formalArgList := [:vl, :$formalArgList]
      #sl ~= #vl =>
         stackAndThrow [_
           "number of arguments to +-> does not match, expected:", #sl]
      x := nx
  else
      vl:= take(#sl,$FormalMapVariableList)
  ress => ress
  $returnMode : local := m'
  $currentFunctionLevel : local := #$exitModeStack
  old_style and not null vl and not hasFormalMapVariable(x, vl) =>
      vln := [GENSYM() for v in vl]
      $formalArgList := [:vln, :$formalArgList]
      for m in sl for v in vln repeat
          [.,.,e]:= compMakeDeclaration([":",v,m],$EmptyMode,e)
      [u,.,.] := comp([x,:vln],m',e) or return nil
      extractCodeAndConstructTriple(u, m, oldE)
  null vl and (t := comp([x], m', e)) =>
    [u,.,.] := t
    extractCodeAndConstructTriple(u, m, oldE)
  for m in sl for v in vl repeat
      [.,.,e]:= compMakeDeclaration([":",v,m],$EmptyMode,e)
  [u,.,.]:= comp(x,m',e) or return nil
  (uu := simpleCall(u, vl, m, oldE)) => uu
  catchTag:= MKQ GENSYM()
  u := replaceExitEtc(u, catchTag, "TAGGEDreturn", $returnMode)
  u := ["CATCH", catchTag, u]
  uu := optimizeFunctionDef [nil, ['LAMBDA, vl, u]]
  --  At this point, we have a function that we would like to pass.
  --  Unfortunately, it makes various free variable references outside
  --  itself.  So we build a mini-vector that contains them all, and
  --  pass this as the environment to our inner function.
  $FUNNAME :local := nil
  $FUNNAME_TAIL : local := [nil]
  expandedFunction := compTranDryRun CADR uu
  frees := getFreeList(expandedFunction, vl, nil, e)
  expandedFunction :=
            --One free can go by itself, more than one needs a vector
         --An A-list name . number of times used
    #frees = 0 =>
        ['LAMBDA, addNilTypesToArgs [:vl, "$$"], :CDDR expandedFunction]
    #frees = 1 =>
      vec:=first first frees
      ['LAMBDA, addNilTypesToArgs [:vl, vec], :CDDR expandedFunction]
    scode:=nil
    vec:=nil
    locals:=nil
    i:=-1
    for v in frees repeat
      i:=i+1
      vec:=[first v,:vec]
      scode:=[['SETQ, first v, [($QuickCode => 'QREFELT;'ELT),"$$",i]], :scode]
      locals:=[first v, :locals]
    body:= CDDR expandedFunction
    if locals then
      if body is [['DECLARE,:.],:.] then
        body := [first body, ['PROG, locals, :scode,
                              ['RETURN, ['PROGN, :rest body]]]]
      else body:=[['PROG,locals,:scode,['RETURN,['PROGN,:body]]]]
    vec:=['VECTOR,:NREVERSE vec]
    ['LAMBDA, addNilTypesToArgs [:vl, "$$"], :body]
  fname:=['CLOSEDFN,expandedFunction]
         --Like QUOTE, but gets compiled
  uu:=
    frees => ['CONS,fname,vec]
    ['LIST,fname]
  [uu,m,oldE]

simpleCall(u, vl, m, oldE) ==
    u is ["call", fn, :avl] and avl = vl =>
        if fn is ["applyFun", a] then fn := a
        fn = "mkRecord" => nil
        [fn,m,oldE]
    nil

extractCodeAndConstructTriple(u, m, oldE) ==
  u is ["call",fn,:.] =>
    if fn is ["applyFun",a] then fn := a
    [fn,m,oldE]
  [op,:.,env] := u
  [["CONS",["function",op],env],m,oldE]

compExpression(x,m,e) ==
  $insideExpressionIfTrue: local:= true
  SYMBOLP(first x) and (fn := GET(first x, "SPECIAL")) =>
    FUNCALL(fn,x,m,e)
  compForm(x,m,e)

compAtom(x, m, e) ==
    res := compAtom1(x, m, e) => res
    compAtomWithModemap(x, m, e, get(x, "modemap", e))

compAtom1(x, m, e) ==
  t:=
    isSymbol x =>
      compSymbol(x,m,e) or return nil
    STRINGP x => [x,x,e]
    [x,primitiveType x or return nil,e]
  convert(t,m)

primitiveType x ==
  x is nil => $EmptyMode
  STRINGP x => $String
  INTEGERP x =>
    x=0 => $NonNegativeInteger
    x>0 => $PositiveInteger
    true => $NegativeInteger
  FLOATP x => $DoubleFloat
  nil

DEFPARAMETER($compForModeIfTrue, false)

compSymbol(s,m,e) ==
  s="$NoValue" => ["$NoValue",$NoValueMode,e]
  isFluid s => [s,getmode(s,e) or return nil,e]
  s="true" => ['(QUOTE T),$Boolean,e]
  s="false" => [false,$Boolean,e]
  s = m => [["QUOTE", s], s, e]
  v:= get(s,"value",e) =>
--+
    MEMQ(s,$functorLocalParameters) =>
        NRTgetLocalIndex s
        [s,v.mode,e] --s will be replaced by an ELT form in beforeCompile
    [s,v.mode,e] --s has been SETQd
  m':= getmode(s,e) =>
    if not member(s,$formalArgList) and not MEMQ(s,$FormalMapVariableList) and
      not isFunction(s,e) and null ($compForModeIfTrue=true) then errorRef s
    [s,m',e] --s is a declared argument
  MEMQ(s,$FormalMapVariableList) => stackMessage ["no mode found for",s]
  not isFunction(s,e) => errorRef s

convertOrCroak(T,m) ==
  u:= convert(T,m) => u
  userError ["CANNOT CONVERT: ",T.expr,"%l"," OF MODE: ",T.mode,"%l",
    " TO MODE: ",m,"%l"]

convert(T,m) ==
  coerce(T,resolve(T.mode,m) or return nil)

maxSuperType(m,e) ==
  typ:= get(m,"SuperDomain",e) => maxSuperType(typ,e)
  m

hasType(x,e) ==
  fn get(x,"condition",e) where
    fn x ==
      null x => nil
      x is [["case",.,y],:.] => y
      fn rest x

compForm(form,m,e) ==
  T:=
    compForm1(form,m,e) or compArgumentsAndTryAgain(form,m,e) or return
      stackMessageIfNone ["cannot compile","%b",form,"%d"]
  T

compArgumentsAndTryAgain(form is [.,:argl],m,e) ==
  not($tryRecompileArguments) or null(argl) => nil
  -- used in case: f(g(x)) where f is in domain introduced by
  -- comping g, e.g. for (ELT (ELT x a) b), environment can have no
  -- modemap with selector b
  form is ["Sel", a, .] => nil
  u:= for x in argl repeat [.,.,e]:= comp(x,$EmptyMode,e) or return "failed"
  u="failed" => nil
  compForm1(form,m,e)

-- FIXME: we should check the argument.
outputComp(x,e) ==
  u:=comp(['_:_:, x, $OutputForm], $OutputForm, e) => u
  x is ['construct,:argl] =>
    [['LIST, ['QUOTE, 'CONCAT], :[([.,.,e] := outputComp(x, e)).expr
        for x in argl]], $OutputForm, e]
  (v:= get(x,"value",e)) and (v.mode is ['Union,:l]) =>
    [['coerceUn2E, x, v.mode], $OutputForm, e]
  SAY ["outputComp strange x ", x]
  nil

compSel1(domain, op, argl, m, e) ==
    domain="Lisp" =>
        [[op, :[([., ., e] := compOrCroak(x, $EmptyMode, e)).expr
           for x in argl]], m, e]
    (op = "COLLECT") and coerceable(domain, m, e) =>
      (T := comp([op, :argl], domain, e) or return nil; coerce(T, m))
    -- Next clause added JHD 8/Feb/94: the clause after doesn't work
    -- since addDomain refuses to add modemaps from Mapping
    e :=
        domain is ['Mapping, :.] =>
            augModemapsFromDomain1(domain, domain, e)
        addDomain(domain, e)
    mml := [x for x in getFormModemaps([op, :argl], e)
              | x is [[ =domain, :.], :.]]
    (ans := compForm2([op, :argl], m, e, mml)) => ans
    op = "construct" and coerceable(domain, m, e) =>
        (T := comp([op, :argl], domain, e) or return nil; coerce(T, m))
    nil

compForm1(form is [op,:argl],m,e) ==
  op="error" =>
      #argl = 1 =>
          arg := first(argl)
          u := comp(arg, $String, e) =>
              [[op, u.expr], m, e]
          SAY ["compiling call to error ", argl]
          u := outputComp(arg, e) =>
              [[op, ['LIST, ['QUOTE, 'mathprint], u.expr]], m, e]
          nil
      SAY ["compiling call to error ", argl]
      nil
  op is ["Sel", domain, op'] => compSel1(domain, op', argl, m, e)

  e:= addDomain(m,e) --???unneccessary because of comp2's call???
  (mmList:= getFormModemaps(form,e)) and (T:= compForm2(form,m,e,mmList)) => T
  compToApply(op,argl,m,e)

compExpressionList(argl,m,e) ==
  Tl := [[.,.,e] := comp(x, $OutputForm, e) or return "failed" for x in argl]
  Tl="failed" => nil
  convert([["LIST", :[y.expr for y in Tl]], $OutputForm, e], m)

compForm2(form is [op,:argl],m,e,modemapList) ==
  sargl:= TAKE(# argl, $TriangleVariableList)
  aList:= [[sa,:a] for a in argl for sa in sargl]
  modemapList:= SUBLIS(aList,modemapList)
  Tl:=
    [[.,.,e]:= T
      for x in argl while (isSimple x and (T:= compUniquely(x,$EmptyMode,e)))]
  or/[x for x in Tl] =>
    partialModeList:= [(x => x.mode; nil) for x in Tl]
    compFormPartiallyBottomUp(form,m,e,modemapList,partialModeList) or
      compForm3(form,m,e,modemapList)
  compForm3(form,m,e,modemapList)

compFormPartiallyBottomUp(form,m,e,modemapList,partialModeList) ==
  mmList:= [mm for mm in modemapList | compFormMatch(mm,partialModeList)] =>
    compForm3(form,m,e,mmList)

compFormMatch(mm,partialModeList) ==
  mm is [[.,.,:argModeList],:.] and match(argModeList,partialModeList) where
    match(a,b) ==
      null b => true
      null first b => match(rest a,rest b)
      first a=first b and match(rest a,rest b)

compForm3(form is [op,:argl],m,e,modemapList) ==
  T:=
    or/
      [compFormWithModemap(form,m,e,first (mml:= ml))
        for ml in tails modemapList]
  $compUniquelyIfTrue =>
    or/[compFormWithModemap(form,m,e,mm) for mm in rest mml] =>
      THROW("compUniquely",nil)
    T
  T

getFormModemaps(form is [op,:argl],e) ==
  op is ["Sel", domain, op1] =>
    [x for x in getFormModemaps([op1,:argl],e) | x is [[ =domain,:.],:.]]
  null atom op => nil
  modemapList:= get(op,"modemap",e)
  if $insideCategoryPackageIfTrue then
    modemapList := [x for x in modemapList | x is [[dom,:.],:.] and dom ~= '$]
  if op = "elt" and #argl = 2 or op = "setelt!" and #argl = 3 then
      modemapList := eltModemapFilter(argl.1, modemapList, e) or return nil
  nargs:= #argl
  finalModemapList:= [mm for (mm:= [[.,.,:sig],:.]) in modemapList | #sig=nargs]
  modemapList and null finalModemapList =>
    stackMessage ["no modemap for","%b",op,"%d","with ",nargs," arguments"]
  finalModemapList

eltModemapFilter(name,mmList,e) ==
  isConstantId(name,e) =>
    l:= [mm for mm in mmList | mm is [[.,.,.,sel,:.],:.] and sel=name] => l
            -- setelt! has extra parameter
    stackMessage ["selector variable: ",name," is undeclared and unbound"]
    nil
  mmList

substituteIntoFunctorModemap(argl,modemap is [[dc,:sig],:.],e) ==
  #dc~=#sig =>
    keyedSystemError("S2GE0016",['"substituteIntoFunctorModemap",
      '"Incompatible maps"])
  #argl=#rest sig =>
                        --here, we actually have a functor form
    sig:= EQSUBSTLIST(argl,rest dc,sig)
      --make new modemap, subst. actual for formal parametersinto modemap
    Tl:= [[.,.,e]:= compOrCroak(a,m,e) for a in argl for m in rest sig]
    substitutionList:= [[x,:T.expr] for x in rest dc for T in Tl]
    [SUBLIS(substitutionList,modemap),e]
  nil

--% SPECIAL EVALUATION FUNCTIONS

--% SETQ

compSetq(["LET",form,val],m,E) == compSetq1(form,val,m,E)

compSetq1(form,val,m,E) ==
  IDENTP form => setqSingle(form,val,m,E)
  form is [":",x,y] =>
    [.,.,E']:= compMakeDeclaration(form,$EmptyMode,E)
    compSetq(["LET",x,val],m,E')
  form is [op,:l] =>
    op="CONS"  => setqMultiple(uncons form,val,m,E)
    op = "@Tuple" => setqMultiple(l, val, m, E)
    setqSetelt(form,val,m,E)

compMakeDeclaration(x,m,e) ==
  $insideExpressionIfTrue: local := false
  compColon(x,m,e)

setqSetelt([v,:s],val,m,E) ==
    comp(["setelt!", v, :s, val], m, E)

setqSingle(id,val,m,E) ==
  $insideSetqSingleIfTrue: local:= true
    --used for comping domain forms within functions
  currentProplist:= getProplist(id,E)
  m'':=
    get(id,'mode,E) or getmode(id,E) or
       (if m=$NoValueMode then $EmptyMode else m)
-- m'':= LASSOC("mode",currentProplist) or $EmptyMode
       --for above line to work, line 3 of compNoStackingis required
  T:=
    eval or return nil where
      eval() ==
        T:= comp(val,m'',E) => T
        not get(id,"mode",E) and m'' ~= (maxm'':=maxSuperType(m'',E)) and
           (T:=comp(val,maxm'',E)) => T
        (T:= comp(val,$EmptyMode,E)) and getmode(T.mode,E) =>
          assignError(val,T.mode,id,m'')
  m'' = $EmptyMode and T.mode = $EmptyMode =>
      stackMessage ["No mode in assignment to: ", id]
  finish_setq_single(T, m, id, val, currentProplist)

finish_setq_single(T, m, id, val, currentProplist) ==
  T' := [x, m', e'] := convert(T, m) or return nil
  if $profileCompiler = true then
    null IDENTP id => nil
    key :=
      MEMQ(id,rest $form) => 'arguments
      'locals
    profileRecord(key,id,T.mode)
  newProplist:= consProplistOf(id,currentProplist,"value",removeEnv [val,:rest T])
  e':= (PAIRP id => e'; addBinding(id,newProplist,e'))
  if isDomainForm(val,e') then
    if isDomainInScope(id,e') then
      stackWarning ["domain valued variable","%b",id,"%d",
        "has been reassigned within its scope"]
    e':= augModemapsFromDomain1(id,val,e')
      --all we do now is to allocate a slot number for lhs
      --e.g. the LET form below will be changed by putInLocalDomainReferences
--+
  saveLocVarsTypeDecl(x, id, e')

  if (k:=NRTassocIndex(id))
     then form:=['SETELT,"$",k,x]
     else form:=
         $QuickLet => ["LET",id,x]
         ["LET",id,x,
            (isDomainForm(x, e') => ['ELT, id, 0]; first outputComp(id, e'))]
  [form,m',e']

saveLocVarsTypeDecl(x, id, e) ==
    t := getmode(id, e) =>
        t := (t = '$EmptyMode => nil; ATOM(t) => [t]; t)
        typeDecl := ASSOC(id, $locVarsTypes)
        null typeDecl =>
            if null t then
                SAY("Local variable ", id, " lacks type.")
            else $locVarsTypes := ACONS(id, t, $locVarsTypes)
        t' := CDR(typeDecl)
        not EQUAL(t, t') =>
            if not null t' then
                SAY("Local variable ", id, " type redefined: ", t, " to ", t')
            RPLACD(typeDecl, t)

assignError(val,m',form,m) ==
  message:=
    val =>
      ["CANNOT ASSIGN: ",val,"%l","   OF MODE: ",m',"%l","   TO: ",form,"%l",
        "   OF MODE: ",m]
    ["CANNOT ASSIGN: ",val,"%l","   TO: ",form,"%l","   OF MODE: ",m]
  stackMessage message

MKPROGN(l) == MKPF(l, "PROGN")

setqMultiple(nameList,val,m,e) ==
  val is ["CONS",:.] and m=$NoValueMode =>
    setqMultipleExplicit(nameList,uncons val,m,e)
  val is ["@Tuple", :l] and m = $NoValueMode =>
      setqMultipleExplicit(nameList,l,m,e)
  -- 1 create a gensym, add to local environment, compile and assign rhs
  g:= genVariable()
  e:= addBinding(g,nil,e)
  T:= [.,m1,.]:= compSetq1(g,val,$EmptyMode,e) or return nil
  e:= put(g,"mode",m1,e)
  [x,m',e]:= convert(T,m) or return nil
  -- 1.1 exit if result is a list
  m1 is ["List",D] =>
    g2 := genVariable()
    e := addBinding(g2, nil, e)
    e := put(g2, "mode", m1, e)
    T := compSetq1(g2, g, m1, e) or return nil
    [x2, ., e] := convert(T, m1) or return nil
    ass_list := []
    for y in nameList repeat
        e := put(y, "value", [genSomeVariable(), D, $noEnv], e)
        ass_list := cons(["LET", y, ["SPADfirst", g2]], ass_list)
        ass_list := cons(["LET", g2, ["CDR", g2]], ass_list)
    ass_list := nreverse(rest(ass_list))
    convert([["PROGN",x, x2, :ass_list, g], m', e], m)
  -- 2 verify that the #nameList = number of parts of right-hand-side
  selectorModePairs:=
                                                --list of modes
    decompose(m1,#nameList,e) or return nil where
      decompose(t,length,e) ==
        t is ["Record",:l] => [[name,:mode] for [":",name,mode] in l]
        comp(t,$EmptyMode,e) is [.,["RecordCategory",:l],.] =>
          [[name,:mode] for [":",name,mode] in l]
        stackMessage ["no multiple assigns to mode: ",t]
  #nameList~=#selectorModePairs =>
    stackMessage [val," must decompose into ",#nameList," components"]
  -- 3 generate code; return
  assignList:=
    [([.,.,e]:= compSetq1(x,["elt",g,y],z,e) or return "failed").expr
      for x in nameList for [y,:z] in selectorModePairs]
  if assignList="failed" then NIL
  else [MKPROGN [x,:assignList,g],m',e]

setqMultipleExplicit(nameList,valList,m,e) ==
  #nameList~=#valList =>
    stackMessage ["Multiple assignment error; # of items in: ",nameList,
      "must = # in: ",valList]
  gensymList:= [genVariable() for name in nameList]
  assignList:=
             --should be fixed to declare genVar when possible
    [[.,.,e]:= compSetq1(g,val,$EmptyMode,e) or return "failed"
      for g in gensymList for val in valList]
  assignList="failed" => nil
  reAssignList:=
    [[.,.,e]:= compSetq1(name,g,$EmptyMode,e) or return "failed"
      for g in gensymList for name in nameList]
  reAssignList="failed" => nil
  [["PROGN",:[T.expr for T in assignList],:[T.expr for T in reAssignList]],
    $NoValueMode, (last reAssignList).env]

--% WHERE
compWhere([.,form,:exprList],m,eInit) ==
  $insideExpressionIfTrue: local:= false
  $insideWhereIfTrue: local:= true
  e:= eInit
  u:=
    for item in exprList repeat
      [.,.,e]:= comp(item,$EmptyMode,e) or return "failed"
  u="failed" => return nil
  $insideWhereIfTrue:= false
  [x,m,eAfter]:= comp(macroExpand(form,eBefore:= e),m,e) or return nil
  eFinal:=
    del:= deltaContour(eAfter,eBefore) => addContour(del,eInit)
    eInit
  [x,m,eFinal]

compConstruct(form is ["construct",:l],m,e) ==
  y:= modeIsAggregateOf("List",m,e) =>
    T:= compList(l,["List",CADR y],e) => convert(T,m)
    compForm(form,m,e)
  y:= modeIsAggregateOf("Vector",m,e) =>
    T:= compVector(l,["Vector",CADR y],e) => convert(T,m)
    compForm(form,m,e)
  T:= compForm(form,m,e) => T
  for D in getDomainsInScope e repeat
    (y:=modeIsAggregateOf("List",D,e)) and
      (T:= compList(l,["List",CADR y],e)) and (T':= convert(T,m)) =>
         return T'
    (y:=modeIsAggregateOf("Vector",D,e)) and
      (T:= compVector(l,["Vector",CADR y],e)) and (T':= convert(T,m)) =>
         return T'

compQuote(expr is [QUOTE, e1], m, e) ==
  SYMBOLP(e1) => [expr, ["Symbol"], e]
  stackAndThrow ["Strange argument to QUOTE", expr]
  -- [expr,m,e]

compList(l,m is ["List",mUnder],e) ==
  null l => [NIL,m,e]
  Tl:= [[.,mUnder,e]:= comp(x,mUnder,e) or return "failed" for x in l]
  Tl="failed" => nil
  T:= [["LIST",:[T.expr for T in Tl]],["List",mUnder],e]

compVector(l,m is ["Vector",mUnder],e) ==
  null l => [$EmptyVector,m,e]
  Tl:= [[.,mUnder,e]:= comp(x,mUnder,e) or return "failed" for x in l]
  Tl="failed" => nil
  [["VECTOR",:[T.expr for T in Tl]],m,e]

--% MACROS
compMacro(form,m,e) ==
  ["MDEF",lhs,signature,specialCases,rhs]:= form
  prhs :=
    rhs is ['CATEGORY,:.] => ['"-- the constructor category"]
    rhs is ['Join,:.]     => ['"-- the constructor category"]
    rhs is ['CAPSULE,:.]  => ['"-- the constructor capsule"]
    rhs is ['add,:.]      => ['"-- the constructor capsule"]
    formatUnabbreviated rhs
  sayBrightly ['"   processing macro definition",'%b,
    :formatUnabbreviated lhs,'" ==> ",:prhs,'%d]
  ATOM(lhs) => userError("Malformed macro definition")
  nrhs :=
      (margs := rest(lhs)) => [rhs, :margs]
      [rhs]
  m=$EmptyMode or m=$NoValueMode =>
    ["/throwAway", $NoValueMode, put(first lhs, "macro", nrhs, e)]

--% SEQ

compSeq(["SEQ",:l],m,e) == compSeq1(l,[m,:$exitModeStack],e)

compSeq1(l,$exitModeStack,e) ==
  $insideExpressionIfTrue: local := false
  $finalEnv: local := false
           --used in replaceExitEtc.
  c:=
    [([.,.,e]:=


      --this used to be compOrCroak-- but changed so we can back out

        ($insideExpressionIfTrue:= NIL; compSeqItem(x,$NoValueMode,e) or return
          "failed")).expr for x in l]
  if c="failed" then return nil
  catchTag:= MKQ GENSYM()
  form:= ["SEQ",:replaceExitEtc(c,catchTag,"TAGGEDexit",$exitModeStack.(0))]
  [["CATCH",catchTag,form],$exitModeStack.(0),$finalEnv]

compSeqItem(x,m,e) == comp(macroExpand(x,e),m,e)

replaceExitEtc(x,tag,opFlag,opMode) ==
  (fn(x,tag,opFlag,opMode); x) where
    fn(x,tag,opFlag,opMode) ==
      atom x => nil
      x is ["QUOTE",:.] => nil
      x is [ =opFlag,n,t] =>
        rplac(first t,replaceExitEtc(first t, tag, opFlag, opMode))
        n = 0 =>
          $finalEnv:=
                  --bound in compSeq1 and compDefineCapsuleFunction
            $finalEnv => intersectionEnvironment($finalEnv,t.env)
            t.env
          rplac(first x,"THROW")
          rplac(CADR x,tag)
          rplac(CADDR x,(convertOrCroak(t,opMode)).expr)
        true => rplac(CADR x,CADR x-1)
      x is [key,n,t] and MEMQ(key,'(TAGGEDreturn TAGGEDexit)) =>
        rplac(first t,replaceExitEtc(first t,tag,opFlag,opMode))
      replaceExitEtc(first x,tag,opFlag,opMode)
      replaceExitEtc(rest x,tag,opFlag,opMode)


--% try

comp_try(["try", expr, catcher, finallizer], m, e) ==
    $exitModeStack : local := [m, :$exitModeStack]
    $insideExpressionIfTrue : local := false
    if catcher then
        stackAndThrow ["comp_try: catch unimplemented"]
    ([c1, m1, .] := comp(expr, m, e)) or return nil
    ([c2, ., .] := comp(finallizer, $EmptyMode, e)) or return nil
    [["finally", c1, c2], m1, e]

--% SUCHTHAT
compSuchthat([.,x,p],m,e) ==
  [x',m',e]:= comp(x,m,e) or return nil
  [p',.,e]:= comp(p,$Boolean,e) or return nil
  e:= put(x',"condition",p',e)
  [x',m',e]

--% exit

compExit(["exit",level,x],m,e) ==
  index:= level-1
  $exitModeStack = [] => comp(x,m,e)
  m1:= $exitModeStack.index
  [x',m',e']:=
    u:=
      comp(x,m1,e) or return
        stackMessageIfNone ["cannot compile exit expression",x,"in mode",m1]
  modifyModeStack(m',index)
  [["TAGGEDexit",index,u],m,e]

modifyModeStack(m,index) ==
  $reportExitModeStack =>
    SAY("exitModeStack: ",COPY $exitModeStack," ====> ",
      ($exitModeStack.index:= resolve(m,$exitModeStack.index); $exitModeStack))
  $exitModeStack.index:= resolve(m,$exitModeStack.index)

compLeave(["leave",level,x],m,e) ==
  index:= #$exitModeStack-1-$leaveLevelStack.(level-1)
  [x',m',e']:= u:= comp(x,$exitModeStack.index,e) or return nil
  modifyModeStack(m',index)
  [["TAGGEDexit",index,u],m,e]

--% return

compReturn(["return", x], m, e) ==
  ns := #$exitModeStack
  ns = $currentFunctionLevel =>
    stackSemanticError(["the return before","%b",x,"%d","is unneccessary"],nil)
    nil
  index := MAX(0, ns - $currentFunctionLevel - 1)
  $returnMode:= resolve($exitModeStack.index,$returnMode)
  [x',m',e']:= u:= comp(x,$returnMode,e) or return nil
  $returnMode:= resolve(m',$returnMode)
  modifyModeStack(m',index)
  [["TAGGEDreturn",0,u],m,e']

--% ELT

compSel(form is ["Sel", aDomain, anOp], m, E) ==
  aDomain="Lisp" =>
    [anOp',m,E] where anOp'() == (anOp=$Zero => 0; anOp=$One => 1; anOp)
  anOp := (anOp = $Zero => "Zero"; anOp = $One => "One"; anOp)
  compSel1(aDomain, anOp, [], m, E)

--% HAS

compHas(pred is ["has",a,b],m,$e) ==
  --b is (":",:.) => (.,.,E):= comp(b,$EmptyMode,E)
  $e:= chaseInferences(pred,$e)
  --pred':= ("has",a',b') := formatHas(pred)
  predCode := compHasFormat1 pred
  coerce([predCode,$Boolean,$e],m)

compHasFormat1(pred is ["has", a, b]) ==
    [a, :.] := comp(a, $EmptyMode, $e) or return nil
    b is ["ATTRIBUTE", c] => BREAK()
    b is ["SIGNATURE", op, sig] =>
        ["HasSignature", a,
          mkList [MKQ op, mkList [mkDomainConstructor type for type in sig]]]
    isDomainForm(b, $EmptyEnvironment) => ["EQUAL", a, b]
    ["HasCategory", a, mkDomainConstructor b]

--used in various other places to make the discrimination
compHasFormat (pred is ["has",olda,b]) ==
  argl := rest $form
  formals := TAKE(#argl,$FormalMapVariableList)
  a := SUBLISLIS(argl,formals,olda)
  [a,:.] := comp(a,$EmptyMode,$e) or return nil
  a := SUBLISLIS(formals,argl,a)
  b is ["ATTRIBUTE",c] => BREAK()
  b is ["SIGNATURE",op,sig] =>
     ["HasSignature",a,
       mkList [MKQ op,mkList [mkDomainConstructor type for type in sig]]]
  isDomainForm(b,$EmptyEnvironment) => ["EQUAL",a,b]
  ["HasCategory",a,mkDomainConstructor b]

--% IF

compIf(["IF",a,b,c],m,E) ==
  [xa,ma,Ea,Einv]:= compBoolean(a,$Boolean,E) or return nil
  [xb,mb,Eb]:= Tb:= compFromIf(b,m,Ea) or return nil
  [xc,mc,Ec]:= Tc:= compFromIf(c,resolve(mb,m),Einv) or return nil
  xb':= coerce(Tb,mc) or return nil
  x:= ["IF", xa, xb'.expr, xc]
  (returnEnv:= Env(xb'.env,Ec,xb'.expr,xc,E)) where
    Env(bEnv,cEnv,b,c,E) ==
      canReturn(b,0,0,true) =>
        (canReturn(c,0,0,true) => intersectionEnvironment(bEnv,cEnv); bEnv)
      canReturn(c,0,0,true) => cEnv
      E
  [x,mc,returnEnv]

canReturn(expr,level,exitCount,ValueFlag) ==  --SPAD: exit and friends
  atom expr => ValueFlag and level=exitCount
  (op:= first expr)="QUOTE" => ValueFlag and level=exitCount
  op="TAGGEDexit" =>
    expr is [.,count,data] => canReturn(data.expr,level,count,count=level)
  level=exitCount and not ValueFlag => nil
  op="SEQ" => or/[canReturn(u,level+1,exitCount,false) for u in rest expr]
  op="TAGGEDreturn" => nil
  op="CATCH" =>
    [.,gs,data]:= expr
    (findThrow(gs,data,level,exitCount,ValueFlag) => true) where
      findThrow(gs,expr,level,exitCount,ValueFlag) ==
        atom expr => nil
        expr is ["THROW", =gs,data] => true
            --this is pessimistic, but I know of no more accurate idea
        expr is ["SEQ",:l] =>
          or/[findThrow(gs,u,level+1,exitCount,ValueFlag) for u in l]
        or/[findThrow(gs,u,level,exitCount,ValueFlag) for u in rest expr]
    canReturn(data,level,exitCount,ValueFlag)
  op = "COND" =>
    level = exitCount =>
      or/[canReturn(last u,level,exitCount,ValueFlag) for u in rest expr]
    or/[or/[canReturn(u,level,exitCount,ValueFlag) for u in v]
                for v in rest expr]
  op="IF" =>
    expr is [.,a,b,c]
    if not canReturn(a,0,0,true) then
      SAY "IF statement can not cause consequents to be executed"
      pp expr
    canReturn(a,level,exitCount,nil) or canReturn(b,level,exitCount,ValueFlag)
      or canReturn(c,level,exitCount,ValueFlag)
  op = "SPROG" =>
      expr is [., defs, body]
      canReturn(body, level, exitCount, ValueFlag)
  op = "LAMBDA" =>
      expr is [., args, :body]
      and/[canReturn(u, level, exitCount, ValueFlag) for u in body]
  --now we have an ordinary form
  atom op => and/[canReturn(u,level,exitCount,ValueFlag) for u in expr]
  op is ["XLAM",args,bods] =>
    and/[canReturn(u,level,exitCount,ValueFlag) for u in expr]
  systemErrorHere '"canReturn" --for the time being

compBoolean(p,m,E) ==
  [p',m,E]:= comp(p,m,E) or return nil
  [p',m,getSuccessEnvironment(p,E),getInverseEnvironment(p,E)]

getSuccessEnvironment(a,e) ==
  -- the next four lines try to ensure that explicit special-case tests
  --  prevent implicit ones from being generated
  a is ["has",x,m] =>
    e
  a is ["is",id,m] =>
    IDENTP id and isDomainForm(m,$EmptyEnvironment) =>
         currentProplist:= getProplist(id,e)
         [.,.,e] := T := comp(m,$EmptyMode,e) or return nil -- duplicates compIs
         newProplist:= consProplistOf(id,currentProplist,"value",[m,:rest removeEnv T])
         addBinding(id,newProplist,e)
    e
  a is ["case",x,m] and IDENTP x =>
    put(x,"condition",[a,:get(x,"condition",e)],e)
  e

getInverseEnvironment(a,E) ==
  atom a => E
  [op,:argl]:= a
-- the next five lines try to ensure that explicit special-case tests
-- prevent implicit ones from being generated
  op="has" =>
    [x,m]:= argl
    E
  a is ["case",x,m] and IDENTP x =>
           --the next two lines are necessary to get 3-branched Unions to work
           -- old-style unions, that is
    (get(x,"condition",E) is [["OR",:oldpred]]) and member(a,oldpred) =>
      put(x,"condition",LIST MKPF(delete(a,oldpred),"OR"),E)
    getUnionMode(x,E) is ["Union",:l]
    l':= delete(m,l)
    for u in l' repeat
       if u is ['_:,=m,:.] then l':=delete(u,l')
    newpred:= MKPF([["case",x,m'] for m' in l'],"OR")
    put(x,"condition",[newpred,:get(x,"condition",E)],E)
  E

getUnionMode(x,e) ==
  m:=
    atom x => getmode(x,e)
    return nil
  isUnionMode(m,e)

isUnionMode(m,e) ==
  m is ["Union",:.] => m
  (m':= getmode(m,e)) is ["Mapping",["UnionCategory",:.]] => CADR m'
  v:= get(if m="$" then "Rep" else m,"value",e) =>
    (v.expr is ["Union",:.] => v.expr; nil)
  nil

compFromIf(a,m,E) ==
  a="noBranch" => ["noBranch",m,E]
  true => comp(a,m,E)

compImport(["import",:doms],m,e) ==
  for dom in doms repeat e:=addDomain(dom,e)
  ["/throwAway",$NoValueMode,e]

--Will the jerk who commented out these two functions please NOT do so
--again.  These functions ARE needed, and case can NOT be done by
--modemap alone.  The reason is that A case B requires to take A
--evaluated, but B unevaluated.  Therefore a special function is
--required.  You may have thought that you had tested this on "failed"
--etc., but "failed" evaluates to it's own mode.  Try it on x case $
--next time.
--                An angry JHD - August 15th., 1984

compCase(["case",x,m'],m,e) ==
  e:= addDomain(m',e)
  T:= compCase1(x,m',e) => coerce(T,m)
  nil

compCase1(x,m,e) ==
  [x',m',e']:= comp(x,$EmptyMode,e) or return nil
  u:=
    [cexpr
      for (modemap:= [map,cexpr]) in getModemapList("case",2,e') | map is [.,.,s,
        t] and modeEqual(t,m) and modeEqual(s,m')] or return nil
  fn:= (or/[selfn for [cond,selfn] in u | cond=true]) or return nil
  [["call",fn,x'],$Boolean,e']

compColon([":",f,t],m,e) ==
  $insideExpressionIfTrue=true => compColonInside(f,m,e,t)
    --if inside an expression, ":" means to convert to m "on faith"
  t:=
    atom t and (t':= assoc(t,getDomainsInScope e)) => t'
    isDomainForm(t,e) and not $insideCategoryIfTrue =>
      (if not member(t,getDomainsInScope e) then e:= addDomain(t,e); t)
    isDomainForm(t,e) or isCategoryForm(t,e) => t
    t is ["Mapping",m',:r] => t
    unknownTypeError t
    t
  f is ["LISTOF",:l] =>
    (for x in l repeat T:= [.,.,e]:= compColon([":",x,t],m,e); T)
  e:=
    f is [op,:argl] =>
      --for MPOLY--replace parameters by formal arguments: RDJ 3/83
      newTarget:= EQSUBSTLIST(take(#argl,$FormalMapVariableList),
        [(x is [":",a,m] => a; x) for x in argl],t)
      signature:=
        ["Mapping",newTarget,:
          [(x is [":",a,m] => m;
              getmode(x,e) or systemErrorHere '"compColonOld") for x in argl]]
      put(op,"mode",signature,e)
    put(f,"mode",t,e)
  if not $bootStrapMode and $insideFunctorIfTrue and
    makeCategoryForm(t,e) is [catform,e] then
        e:= put(f,"value",[genSomeVariable(),t,$noEnv],e)
  ["/throwAway",getmode(f,e),e]

unknownTypeError name ==
  name:=
    name is [op,:.] => op
    name
  stackSemanticError(["%b",name,"%d","is not a known type"],nil)

compPretend(["pretend",x,t],m,e) ==
  e:= addDomain(t,e)
  T:= comp(x,t,e) or comp(x,$EmptyMode,e) or return nil
  if T.mode=t then warningMessage:= ["pretend",t," -- should replace by @"]
  if opOf(T.mode) = 'Union and opOf(m) ~= 'Union then
     stackWarning(["cannot pretend ",x," of mode ",T.mode," to mode ",m])
  T:= [T.expr,t,T.env]
  T':= coerce(T,m) => (if warningMessage then stackWarning warningMessage; T')

compColonInside(x,m,e,m') ==
    return stackSemanticError([_
        "compColonInside: colon inside expressions is unsupported"], nil)

compIs(["is",a,b],m,e) ==
  [aval,am,e] := comp(a,$EmptyMode,e) or return nil
  [bval,bm,e] := comp(b,$EmptyMode,e) or return nil
  T:= [["domainEqual",aval,bval],$Boolean,e]
  coerce(T,m)

--%  Functions for coercion by the compiler

--  The function coerce is used by the old compiler for coercions.
--  The function coerceInteractive is used by the interpreter.
--  One should always call the correct function, since the represent-
--  ation of basic objects may not be the same.
--
-- Type in returned triple is m when m is not $EmptyMode,
-- otherwise it is type from T
coerce(T,m) ==
  $InteractiveMode =>
    keyedSystemError("S2GE0016",['"coerce",
      '"function coerce called from the interpreter."])
  rplac(CADR T,substitute("$",$Rep,CADR T))
  T':= coerceEasy(T,m) => T'
  T':= coerceSubset(T,m) => T'
  T':= coerceHard(T,m) => T'
  T.expr = "$fromCoerceable$" or isSomeDomainVariable m => nil
      -- if from coerceable, this coerce was just a trial coercion
      -- from compFormWithModemap to filter through the modemaps
  stackMessage fn(T.expr,T.mode,m) where
    fn(x,m1,m2) ==
      ["Cannot coerce","%b",x,"%d","%l","      of mode","%b",m1,"%d","%l",
        "      to mode","%b",m2,"%d"]

coerceEasy(T,m) ==
  m=$EmptyMode => T
  m=$NoValueMode or m=$Void => [T.expr,m,T.env]
  T.mode =m => T
  T.mode =$Exit =>
      [["PROGN", T.expr, ["userError", '"Did not really exit."]],
        m,T.env]
  T.mode=$EmptyMode or modeEqualSubst(T.mode,m,T.env) =>
    [T.expr,m,T.env]

coerceSubset([x,m,e],m') ==
  isSubset(m,m',e) or m="Rep" and m'="$" => [x,m',e]
  m is ['SubDomain,=m',:.] => [x,m',e]
  INTEGERP x and (pred:= isSubset(m',maxSuperType(m,e),e)) -- again temporary
    and eval substitute(x,"*",pred) =>
      [x,m',e]
  nil

coerceHard(T,m) ==
  $e: local:= T.env
  m':= T.mode
  STRINGP m' and modeEqual(m,$String) => [T.expr,m,$e]
  STRINGP T.expr and modeEqual(m', $String) and modeEqual(m, $Symbol) =>
      [["QUOTE", INTERN(T.expr, "BOOT")], m, $e]
  modeEqual(m',m) or
    (get(m',"value",$e) is [m'',:.] or getmode(m',$e) is ["Mapping",m'']) and
      modeEqual(m'',m) or
        (get(m,"value",$e) is [m'',:.] or getmode(m,$e) is ["Mapping",m'']) and
          modeEqual(m'',m') => [T.expr,m,T.env]
  STRINGP T.expr and T.expr=m => [T.expr,m,$e]
  isCategoryForm(m,$e) =>
      $bootStrapMode = true => [T.expr,m,$e]
      extendsCategoryForm(T.expr,T.mode,m) => [T.expr,m,$e]
      coerceExtraHard(T,m)
  coerceExtraHard(T,m)

coerceExtraHard(T is [x,m',e],m) ==
  T':= autoCoerceByModemap(T,m) => T'
  isUnionMode(m',e) is ["Union",:l] and (t:= hasType(x,e)) and
    member(t,l) and (T':= autoCoerceByModemap(T,t)) and
      (T'':= coerce(T',m)) => T''
  m' is ['Record, :.] and m = $OutputForm =>
      [['coerceRe2E,x,['ELT,COPY m',0]],m,e]
  nil

coerceable(m,m',e) ==
  m=m' => m
  -- must find any free parameters in m
  sl:= pmatch(m',m) => SUBLIS(sl,m')
  coerce(["$fromCoerceable$",m,e],m') => m'
  nil

coerceExit([x,m,e],m') ==
  m':= resolve(m,m')
  x':= replaceExitEtc(x,catchTag:= MKQ GENSYM(),"TAGGEDexit",$exitMode)
  coerce([["CATCH",catchTag,x'],m,e],m')

compAtSign(["@",x,m'],m,e) ==
  e:= addDomain(m',e)
  T:= comp(x,m',e) or return nil
  coerce(T,m)

compCoerce(["::",x,m'],m,e) ==
  e:= addDomain(m',e)
  T:= compCoerce1(x,m',e) => coerce(T,m)

compCoerce1(x,m',e) ==
  T:= comp(x,m',e) or comp(x,$EmptyMode,e) or return nil
  m1:=
    STRINGP T.mode => $String
    T.mode
  m':=resolve(m1,m')
  T:=[T.expr,m1,T.env]
  T':= coerce(T,m') => T'
  T':= coerceByModemap(T,m') => T'
  pred:=isSubset(m',T.mode,e) =>
    gg:=GENSYM()
    pred:= substitute(gg,"*",pred)
    code := ['PROG1, ['LET, gg, T.expr],
                     ['check_subtype2, pred, MKQ m', MKQ T.mode, gg]]
    [code,m',T.env]

coerceByModemap([x,m,e],m') ==
--+ modified 6/27 for new runtime system
  u:=
    [modemap
      for (modemap:= [map,cexpr]) in getModemapList("coerce",1,e) | map is [.,t,
        s] and (modeEqual(t,m') or isSubset(t,m',e))
           and (modeEqual(s,m) or isSubset(m,s,e))] or return nil

  --mm:= (or/[mm for (mm:=[.,[cond,.]]) in u | cond=true]) or return nil
  mm:=first u  -- patch for non-trival conditons
  fn :=
    genDeltaEntry ['coerce,:mm]
  [["call",fn,x],m',e]

autoCoerceByModemap([x,source,e],target) ==
  u:=
    [cexpr
      for (modemap:= [map,cexpr]) in getModemapList("autoCoerce",1,e) | map is [
        .,t,s] and modeEqual(t,target) and modeEqual(s,source)] or return nil
  fn:= (or/[selfn for [cond,selfn] in u | cond=true]) or return nil
  source is ["Union",:l] and member(target,l) =>
    (y:= get(x,"condition",e)) and (or/[u is ["case",., =target] for u in y])
       => [["call",fn,x],target,e]
    x="$fromCoerceable$" => nil
    stackMessage ["cannot coerce: ",x,"%l","      of mode: ",source,"%l",
      "      to: ",target," without a case statement"]
  [["call",fn,x],target,e]

--% Very old resolve
-- should only be used in the old (preWATT) compiler

resolve(din,dout) ==
  din=$NoValueMode or dout=$NoValueMode => $NoValueMode
  dout=$EmptyMode => din
  din ~= dout and STRINGP dout and modeEqual(din, $String) => nil
  dout

modeEqual(x,y) ==
  -- this is the late modeEqual
  -- orders Unions
  atom x or atom y => x=y
  #x ~=#y => nil
  x is ['Union,:xl] and y is ['Union,:yl] =>
    for x1 in xl repeat
      for y1 in yl repeat
        modeEqual(x1,y1) =>
          xl := delete(x1,xl)
          yl := delete(y1,yl)
          return nil
    xl or yl => nil
    true
  (and/[modeEqual(u,v) for u in x for v in y])

modeEqualSubst(m1,m,e) ==
  atom m1 and EQ(m1, m) => true
  if atom m1 then
      m1 :=
          get(m1,"value",e) is [m0,:.] => m0
          m1
  if atom m then
      m :=
          get(m,"value",e) is [m2,:.] => m2
          m
  atom m1 or atom m => m1 = m
  modeEqual(m1, m) => true
  -- atom m1 => get(m1,"value",e) is [m',:.] and modeEqual(m',m)
  m1 is [op,:l1] and m is [=op,:l2]  and # l1 = # l2 =>
-- Above length test inserted JHD 4:47 on 15/8/86
-- Otherwise Records can get fouled up - consider expressIdealElt
-- in the DEFAULTS package
        and/[modeEqualSubst(xm1,xm2,e) for xm1 in l1 for xm2 in l2]
  nil

--% Things to support )compile

compileSpad2Cmd args ==
    -- This is the old compiler
    -- Assume we entered from the "compiler" function, so args ~= nil
    -- and is a file with file extension .spad.

    path := pathname args
    pathnameType path ~= '"spad" => throwKeyedMsg("S2IZ0082", nil)
    not PROBE_-FILE path => throwKeyedMsg("S2IL0003",[namestring args])

    $edit_file := path
    sayKeyedMsg("S2IZ0038",[namestring args])

    optList :=  '( _
      break _
      constructor _
      functions _
      library _
      lisp _
      new _
      old _
      nobreak _
      nolibrary _
      noquiet _
      vartrace _
      quiet _
        )

    -- next three are for the OLD NEW compiler
    -- should be unhooked

    $scanIfTrue              : local := nil
    $compileOnlyCertainItems : local := nil
    $f                       : local := nil  -- compiler
    $m                       : local := nil  --   variables

    -- following are for )quick option for code generation
    $QuickLet   : local := true
    $QuickCode  : local := true

    fun         := ['rq, 'lib]
    constructor := nil

    for opt in $options repeat
        [optname,:optargs] := opt
        fullopt := selectOptionLC(optname,optList,nil)

        fullopt = 'new         => error "Internal error: compileSpad2Cmd got )new"
        fullopt = 'old         => NIL     -- no opt

        fullopt = 'library     => fun.1 := 'lib
        fullopt = 'nolibrary   => fun.1 := 'nolib

        -- Ignore quiet/nonquiet if "constructor" is given.
        fullopt = 'quiet       => if fun.0 ~= 'c then fun.0 := 'rq
        fullopt = 'noquiet     => if fun.0 ~= 'c then fun.0 := 'rf
        fullopt = 'nobreak     => $scanIfTrue := true
        fullopt = 'break       => $scanIfTrue := nil
        fullopt = 'vartrace      =>
          $QuickLet  := false
        fullopt = 'lisp        =>
          throwKeyedMsg("S2IZ0036",['")lisp"])
        fullopt = 'functions   =>
            null optargs =>
              throwKeyedMsg("S2IZ0037",['")functions"])
            $compileOnlyCertainItems := optargs
        fullopt = 'constructor =>
            null optargs =>
              throwKeyedMsg("S2IZ0037",['")constructor"])
            fun.0       := 'c
            constructor := [unabbrev o for o in optargs]
        throwKeyedMsg("S2IZ0036",[STRCONC('")",object2String optname)])

    $InteractiveMode : local := nil
    if $compileOnlyCertainItems then
        null constructor => sayKeyedMsg("S2IZ0040",NIL)
        compilerDoitWithScreenedLisplib(constructor, fun)
    else
        compilerDoit(constructor, fun)
    extendLocalLibdb $newConlist
    terminateSystemCommand()
    spadPrompt()

compilerDoit(constructor, fun) ==
    $byConstructors : local := []
    $constructorsSeen : local := []
    fun = ['rf, 'lib]   => read_or_compile(true, true)    -- Ignore "noquiet".
    fun = ['rf, 'nolib] => read_or_compile(false, false)
    fun = ['rq, 'lib]   => read_or_compile(true, true)
    fun = ['rq, 'nolib] => read_or_compile(true, false)
    fun = ['c,  'lib]   =>
      $byConstructors := [opOf x for x in constructor]
      read_or_compile(true, true)
      for ii in $byConstructors repeat
        null member(ii,$constructorsSeen) =>
          sayBrightly ['">>> Warning ",'%b,ii,'%d,'" was not found"]

compilerDoitWithScreenedLisplib(constructor, fun) ==
    EMBED('RWRITE,
          '(LAMBDA (KEY VALUE STREAM)
                   (COND ((AND (EQ STREAM $libFile)
                               (NOT (MEMBER KEY $saveableItems)))
                          VALUE)
                         ((NOT NIL)
                          (RWRITE KEY VALUE STREAM)))) )
    UNWIND_-PROTECT(compilerDoit(constructor,fun),
                   UNEMBED 'RWRITE)
