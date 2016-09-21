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

-- Functions which require special handlers (also see end of file)
DEFPARAMETER($specialOps, [ _
  "ADEF", "AlgExtension", "and", "case", "COERCE", "COLLECT", "construct", "Declare", "DEF", "Dollar", _
     "equation", "error", "free", "has", "IF", "is", "isnt", "iterate", "break", "LET", "local", "MDEF", "or", _
       "pretend", "QUOTE", "REDUCE", "REPEAT", "return", "SEQ", "TARGET", "Tuple", "typeOf", "where" ])

--% Handlers for map definitions

upDEF t ==
  -- performs map definitions.  value is thrown away
  t isnt [op,def,pred,.] => nil
  v:=addDefMap(['DEF,:def],pred)
  null(LISTP(def)) or null(def) =>
    keyedSystemError("S2GE0016",['"upDEF",'"bad map definition"])
  mapOp := first def
  if LISTP(mapOp) then
    null mapOp =>
      keyedSystemError("S2GE0016",['"upDEF",'"bad map definition"])
    mapOp := first mapOp
  put(mapOp,'value,v,$e)
  putValue(op,objNew(voidValue(), $Void))
  putModeSet(op,[$Void])

--% Handler for package calling and $ constants

upDollar t ==
  -- Puts "dollar" property in atree node, and calls bottom up
  t isnt [op,D,form] => nil
  t2 := t
  (not $genValue) and or/[CONTAINED(var,D) for var in $localVars] =>
    keyedMsgCompFailure("S2IS0032",NIL)
  EQ(D,'Lisp) => upLispCall(op,form)
  if VECP D and (SIZE(D) > 0) then D := D.0
  t := evaluateType unabbrev D
  categoryForm? t =>
    throwKeyedMsg("S2IE0012", [t])
  f := getUnname form
  if f = $immediateDataSymbol then
    f := objValUnwrap coerceInteractive(getValue form,$OutputForm)
    if f = '(construct) then f := "nil"
  ATOM(form) and (f ~= $immediateDataSymbol) and
    (u := findUniqueOpInDomain(op,f,t)) => u
  f in '(One Zero true false nil) and constantInDomain?([f],t) =>
    isPartialMode t => throwKeyedMsg("S2IS0020",NIL)
    if $genValue then
      val := wrap getConstantFromDomain([f],t)
    else val := ['getConstantFromDomain,['LIST,MKQ f],MKQ t]
    putValue(op,objNew(val,t))
    putModeSet(op,[t])

  nargs := #rest form

  (ms := upDollarTuple(op, f, t, t2, rest form, nargs)) => ms

  f ~= 'construct and null isOpInDomain(f,t,nargs) =>
    throwKeyedMsg("S2IS0023",[f,t])
  if (sig := findCommonSigInDomain(f,t,nargs)) then
    for x in sig for y in form repeat
      if x then putTarget(y,x)
  putAtree(first form,'dollar,t)
  ms := bottomUp form
  f in '(One Zero) and PAIRP(ms) and first(ms) = $OutputForm =>
    throwKeyedMsg("S2IS0021",[f,t])
  putValue(op,getValue first form)
  putModeSet(op,ms)


upDollarTuple(op, f, t, t2, args, nargs) ==
  -- this function tries to find a tuple function to use
  nargs = 1 and getUnname first args = "Tuple" => NIL
  nargs = 1 and (ms := bottomUp first args) and ms is [["Tuple",.]] => NIL
  null (singles := isOpInDomain(f,t,1)) => NIL
  tuple := NIL
  for [[.,arg], :.] in singles while null tuple repeat
    if arg is ['Tuple,.] then tuple := arg
  null tuple => NIL
  [.,D,form] := t2
  newArg := [mkAtreeNode "Tuple",:args]
  putTarget(newArg, tuple)
  ms := bottomUp newArg
  first ms ~= tuple => NIL
  form := [first form, newArg]
  putAtree(first form,'dollar,t)
  ms := bottomUp form
  putValue(op,getValue first form)
  putModeSet(op,ms)

upLispCall(op,t) ==
  -- process $Lisp calls
  if atom t then code:=getUnname t else
    [lispOp,:argl]:= t
    not(functionp(lispOp.0) or macrop(lispOp.0)) =>
      throwKeyedMsg("S2IS0024",[lispOp.0])
    for arg in argl repeat bottomUp arg
    code:=[getUnname lispOp,
      :[getArgValue(arg,computedMode arg) for arg in argl]]
  code :=
    $genValue => wrap timedEVALFUN code
    code
  rt := '(SExpression)
  putValue(op,objNew(code,rt))
  putModeSet(op,[rt])

--% Handlers for equation

upequation tree ==
  -- only handle this if there is a target of Boolean
  -- this should speed things up a bit
  tree isnt [op,lhs,rhs] => NIL
  $Boolean ~= getTarget(op) => NIL
  null VECP op => NIL
  -- change equation into '='
  op.0 := "="
  bottomUp tree

--% Handler for error

uperror t ==
  -- when compiling a function, this merely inserts another argument
  -- which is the name of the function.
  not $compilingMap => NIL
  t isnt [op,msg] => NIL
  msgMs := bottomUp msg
  msgMs isnt [=$String] => NIL
  RPLACD(t,[mkAtree object2String $mapName,msg])
  bottomUp t

--% Handlers for free and local

upfree t ==
  putValue(t,objNew('(voidValue),$Void))
  putModeSet(t,[$Void])

uplocal t ==
  putValue(t,objNew('(voidValue),$Void))
  putModeSet(t,[$Void])

upfreeWithType(var,type) ==
  sayKeyedMsg("S2IS0055",['"free",var])
  var

uplocalWithType(var,type) ==
  sayKeyedMsg("S2IS0055",['"local",var])
  var

--% Handlers for has

uphas t ==
  t isnt [op,type,prop] => nil
  -- handler for category and attribute queries
  type :=
    isLocalVar(type) => ['unabbrev, type]
    MKQ unabbrev type
  catCode :=
    prop := unabbrev SUBST('$, '%, prop)
    prop is [":", :.] => MKQ prop
    ['evaluateType, MKQ prop]
  code:=['newHasTest,['evaluateType, type], catCode]
  if $genValue then code := wrap timedEVALFUN code
  putValue(op,objNew(code,$Boolean))
  putModeSet(op,[$Boolean])

--% Handlers for IF

upIF t ==
  t isnt [op,cond,a,b] => nil
  bottomUpPredicate(cond,'"if/when")
  $genValue => interpIF(op,cond,a,b)
  compileIF(op,cond,a,b,t)

compileIF(op,cond,a,b,t) ==
  -- type analyzer for compiled case where types of both branches of
  --  IF are resolved.
  ms1 := bottomUp a
  [m1] := ms1
  b = 'noBranch =>
    evalIF(op,rest t,$Void)
    putModeSet(op,[$Void])
  b = 'noMapVal =>
    -- if this was a return statement, we take the mode to be that
    -- of what is being returned.
    if getUnname a = "return" then
      ms1 := bottomUp CADR a
      [m1] := ms1
    evalIF(op,rest t,m1)
    putModeSet(op,ms1)
  ms2 := bottomUp b
  [m2] := ms2
  m:=
    m2=m1 => m1
    m2 = $Exit => m1
    m1 = $Exit => m2
    if EQCAR(m1,'Symbol) then
      m1:=getMinimalVarMode(getUnname a,$declaredMode)
    if EQCAR(m2,'Symbol) then
      m2:=getMinimalVarMode(getUnname b,$declaredMode)
    (r := resolveTTAny(m2,m1)) => r
    rempropI($mapName,'localModemap)
    rempropI($mapName,'localVars)
    rempropI($mapName,'mapBody)
    throwKeyedMsg("S2IS0026",[m2,m1])
  evalIF(op,rest t,m)
  putModeSet(op,[m])

evalIF(op,[cond,a,b],m) ==
  -- generate code form compiled IF
  elseCode:=
    b='noMapVal =>
      [[MKQ true, ['throwKeyedMsg,MKQ "S2IM0018",
        ['CONS,MKQ object2Identifier $mapName,NIL]]]]
    b = 'noBranch => [[MKQ true, ['voidValue]]]
    [[MKQ true,genIFvalCode(b,m)]]
  code:=['COND,[getArgValue(cond,$Boolean),
    genIFvalCode(a,m)],:elseCode]
  triple:= objNew(code,m)
  putValue(op,triple)

genIFvalCode(t,m) ==
  -- passes type information down braches of IF statement
  --  So that coercions can be performed on data at branches of IF.
  m1 := computedMode t
  m1=m => getArgValue(t,m)
  code:=objVal getValue t
  IFcodeTran(code,m,m1)

IFcodeTran(code,m,m1) ==
  -- coerces values at branches of IF
  null code => code
  code is ['spadThrowBrightly,:.] => code
  m1 = $Exit => code
  code isnt ['COND,[p1,a1],[''T,a2]] =>
    m = $Void => code
    code' := coerceInteractive(objNew(quote2Wrapped code,m1),m) =>
      wrapped2Quote objVal code'
    throwKeyedMsgCannotCoerceWithValue(quote2Wrapped code,m1,m)
  a1:=IFcodeTran(a1,m,m1)
  a2:=IFcodeTran(a2,m,m1)
  ['COND,[p1,a1],[''T,a2]]

interpIF(op,cond,a,b) ==
  -- non-compiled version of IF type analyzer.  Doesn't resolve accross
  --  branches of the IF.
  val:= getValue cond
  val:= coerceInteractive(val,$Boolean) =>
    objValUnwrap(val) => upIFgenValue(op,a)
    EQ(b,'noBranch) =>
      putValue(op,objNew(voidValue(), $Void))
      putModeSet(op,[$Void])
    upIFgenValue(op,b)
  throwKeyedMsg("S2IS0031",NIL)

upIFgenValue(op,tree) ==
  -- evaluates tree and transfers the results to op
  ms:=bottomUp tree
  val:= getValue tree
  putValue(op,val)
  putModeSet(op,ms)

--% Handlers for is

upis t ==
  t isnt [op,a,pattern] => nil
  $opIsIs : local := true
  upisAndIsnt t

upisnt t ==
  t isnt [op,a,pattern] => nil
  $opIsIs : local := nil
  upisAndIsnt t

upisAndIsnt(t:=[op,a,pattern]) ==
  -- handler for "is" pattern matching
  mS:= bottomUp a
  mS isnt [m] =>
    keyedSystemError("S2GE0016",['"upisAndIsnt",'"non-unique modeset"])
  putPvarModes(removeConstruct pattern,m)
  evalis(op,rest t,m)
  putModeSet(op,[$Boolean])

putPvarModes(pattern,m) ==
  -- Puts the modes for the pattern variables into $env
  m isnt ['List,um] => throwKeyedMsg("S2IS0030",NIL)
  for pvar in pattern repeat
      IDENTP pvar => put(pvar, 'mode, um, $env)
      pvar is ['_:, var] => put(var, 'mode, m, $env)
      pvar is ['_=, var] => put(var, 'mode, um, $env)
      putPvarModes(pvar, um)

evalis(op,[a,pattern],mode) ==
  -- actually handles is and isnt
  if $opIsIs
    then fun := 'evalIsPredicate
    else fun := 'evalIsntPredicate
  if isLocalPred pattern then
    code:= compileIs(a,pattern)
  else code:=[fun,getArgValue(a,mode),
    MKQ pattern,MKQ mode]
  triple:=
    $genValue => objNewWrap(timedEVALFUN code,$Boolean)
    objNew(code,$Boolean)
  putValue(op,triple)

isLocalPred pattern ==
  -- returns true if the is predicate is to be compiled
  for pat in pattern repeat
    IDENTP pat and isLocalVar(pat) => return true
    pat is ['_:,var] and isLocalVar(var) => return true
    pat is ['_=,var] and isLocalVar(var) => return true

compileIs(val,pattern) ==
  -- produce code for compiled "is" predicate.  makes pattern variables
  --  into local variables of the function
  vars:= NIL
  for pat in rest pattern repeat
    IDENTP(pat) and isLocalVar(pat) => vars:=[pat,:vars]
    pat is ['_:,var] => vars:= [var,:vars]
    pat is ['_=,var] => vars:= [var,:vars]
  predCode:=['LET,g:=GENSYM(),['isPatternMatch,
    getArgValue(val,computedMode val),MKQ removeConstruct pattern]]
  for var in REMDUP vars repeat
    assignCode:=[['LET,var,['CDR,['ASSQ,MKQ var,g]]],:assignCode]
  null $opIsIs =>
    ['COND,[['EQ,predCode,MKQ 'failed],['SEQ,:assignCode,MKQ 'T]]]
  ['COND,[['NOT,['EQ,predCode,MKQ 'failed]],['SEQ,:assignCode,MKQ 'T]]]

evalIsPredicate(value,pattern,mode) ==
  --This function pattern matches value to pattern, and returns
  --true if it matches, and false otherwise.  As a side effect
  --if the pattern matches then the bindings given in the pattern
  --are made
  pattern:= removeConstruct pattern
  not ((valueAlist:=isPatternMatch(value,pattern))='failed) =>
    for [id,:value] in valueAlist repeat
      evalLETchangeValue(id,objNewWrap(value,get(id,'mode,$env)))
    true
  false

evalIsntPredicate(value,pattern,mode) ==
  evalIsPredicate(value,pattern,mode) => NIL
  'TRUE

removeConstruct pat ==
  -- removes the "construct" from the beginning of patterns
  if pat is ['construct,:p] then pat:=p
  if pat is ['cons, a, b] then pat := [a, ['_:, b]]
  atom pat => pat
  RPLACA(pat, removeConstruct first pat)
  RPLACD(pat, removeConstruct rest pat)
  pat

isPatternMatch(l,pats) ==
  -- perform the actual pattern match
  $subs: local := NIL
  isPatMatch(l,pats)
  $subs

isPatMatch(l,pats) ==
  null pats =>
    null l => $subs
    $subs:='failed
  null l =>
    null pats => $subs
    pats is [['_:,var]] =>
      $subs := [[var],:$subs]
    $subs:='failed
  pats is [pat,:restPats] =>
    IDENTP pat =>
      $subs:=[[pat,:first l],:$subs]
      isPatMatch(rest l,restPats)
    pat is ['_=,var] =>
      p:=ASSQ(var,$subs) =>
        first l = rest p => isPatMatch(rest l, restPats)
        $subs:='failed
      $subs:='failed
    pat is ['_:,var] =>
      n:=#restPats
      m:=#l-n
      m<0 => $subs:='failed
      ZEROP n => $subs:=[[var,:l],:$subs]
      $subs:=[[var,:[x for x in l for i in 1..m]],:$subs]
      isPatMatch(DROP(m,l),restPats)
    isPatMatch(first l,pat) = 'failed => 'failed
    isPatMatch(rest l,restPats)
  keyedSystemError("S2GE0016",['"isPatMatch",
     '"unknown form of is predicate"])

--% Handler for iterate

upiterate t ==
  null $repeatBodyLabel => throwKeyedMsg("S2IS0029",['"iterate"])
  $iterateCount := $iterateCount + 1
  code := ['THROW,$repeatBodyLabel,'(voidValue)]
  $genValue => THROW(eval $repeatBodyLabel,voidValue())
  putValue(t,objNew(code,$Void))
  putModeSet(t,[$Void])

--% Handler for break

upbreak t ==
  t isnt [op,.] => nil
  null $repeatLabel => throwKeyedMsg("S2IS0029",['"break"])
  $breakCount := $breakCount + 1
  code := ['THROW,$repeatLabel,'(voidValue)]
  $genValue => THROW(eval $repeatLabel,voidValue())
  putValue(op,objNew(code,$Void))
  putModeSet(op,[$Void])

--% Handlers for LET

upLET t ==
  -- analyzes and evaluates the righthand side, and does the variable
  -- binding
  t isnt [op,lhs,rhs] => nil
  $declaredMode: local := NIL
  PAIRP lhs =>
    var:= getUnname first lhs
    var = 'construct => upLETWithPatternOnLhs t
    var = 'QUOTE => throwKeyedMsg("S2IS0027",['"A quoted form"])
    upLETWithFormOnLhs(op,lhs,rhs)
  var:= getUnname lhs
  var = $immediateDataSymbol =>
    -- following will be immediate data, so probably ok to not
    -- specially format it
    obj := objValUnwrap coerceInteractive(getValue lhs,$OutputForm)
    throwKeyedMsg("S2IS0027",[obj])
  var in '(% %%) =>               -- for history
    throwKeyedMsg("S2IS0027",[var])
  (IDENTP var) and not (var in '(true false elt QUOTE)) =>
    var ~= (var' := unabbrev(var)) =>  -- constructor abbreviation
      throwKeyedMsg("S2IS0028",[var,var'])
    if get(var,'isInterpreterFunction,$e) then
      putHist(var,'isInterpreterFunction,false,$e)
      sayKeyedMsg("S2IS0049",['"Function",var])
    else if get(var,'isInterpreterRule,$e) then
      putHist(var,'isInterpreterRule,false,$e)
      sayKeyedMsg("S2IS0049",['"Rule",var])
    not isTupleForm(rhs) and (m := isType rhs) => upLETtype(op,lhs,m)
    transferPropsToNode(var,lhs)
    if ( m:= getMode(lhs) ) then
      $declaredMode := m
      putTarget(rhs,m)
    if (val := getValue lhs) and (objMode val = $Boolean) and
      getUnname(rhs) = 'equation then putTarget(rhs,$Boolean)
    (rhsMs:= bottomUp rhs) = [$Void] =>
      throwKeyedMsg("S2IS0034",[var])
    val:=evalLET(lhs,rhs)
    putValue(op,val)
    putModeSet(op,[objMode(val)])
  throwKeyedMsg("S2IS0027",[var])

isTupleForm f ==
    -- have to do following since "Tuple" is an internal form name
    getUnname f ~= "Tuple" => false
    f is [op,:args] and VECP(op) and getUnname(op) = "Tuple" =>
        #args ~= 1 => true
        isTupleForm first args => true
        isType first args => false
        true
    false

evalLET(lhs,rhs) ==
  -- lhs is a vector for a variable, and rhs is the evaluated atree
  --  for the value which is coerced to the mode of lhs
  $useConvertForCoercions: local := true
  v' := (v:= getValue rhs)
  ((not getMode lhs) and (getModeSet rhs is [.])) or
    get(getUnname lhs,'autoDeclare,$env) =>
      v:=
        $genValue => v
        objNew(wrapped2Quote objVal v,objMode v)
      evalLETput(lhs,v)
  t1:= objMode v
  t2' := (t2 := getMode lhs)
  value:=
    t1 = t2 =>
      $genValue => v
      objNew(wrapped2Quote objVal v,objMode v)
    if isPartialMode t2 then
      if EQCAR(t1,'Symbol) and $declaredMode then
        t1:= getMinimalVarMode(objValUnwrap v,$declaredMode)
      t' := t2
      null (t2 := resolveTM(t1,t2)) =>
        if not t2 then t2 := t'
        throwKeyedMsg("S2IS0035",[t1,t2])
    null (v := getArgValue(rhs,t2)) =>
      isWrapped(objVal v') and (v2:=coerceInteractive(v',$OutputForm)) =>
        throwKeyedMsg("S2IS0036",[objValUnwrap v2,t2])
      throwKeyedMsg("S2IS0037",[t2])
    t2 and objNew(($genValue => wrap timedEVALFUN v ; v),t2)
  value => evalLETput(lhs,value)
  throwKeyedMsgCannotCoerceWithValue(objVal v,t1,getMode lhs)

evalLETput(lhs,value) ==
  -- put value into the cell for lhs
  name:= getUnname lhs
  if not $genValue then
    code:=
      isLocalVar(name) =>
        om := objMode(value)
        dm := get(name,'mode,$env)
        dm and not ((om = dm) or isSubDomain(om,dm) or
          isSubDomain(dm,om)) =>
            compFailure ['"   The type of the local variable",
              :bright name,'"has changed in the computation."]
        if dm and isSubDomain(dm,om) then put(name,'mode,om,$env)
        ['LET,name,objVal value,$mapName]
               -- $mapName is set in analyzeMap
      om := objMode value
      dm := get(name, 'mode, $env) or objMode(get(name, 'value, $e))
      dm and (null $compilingMap) and not(om = dm) and not(isSubDomain(om, dm)) =>
        THROW('loopCompiler,'tryInterpOnly)
      ['unwrap,['evalLETchangeValue,MKQ name,
        objNewCode(['wrap,objVal value],objMode value)]]
    value:= objNew(code,objMode value)
    isLocalVar(name) =>
      if not get(name,'mode,$env) then put(name,'autoDeclare,'T,$env)
      put(name,'mode,objMode(value),$env)
    put(name,'automode,objMode(value),$env)
  $genValue and evalLETchangeValue(name,value)
  putValue(lhs,value)

upLETWithPatternOnLhs(t := [op,pattern,a]) ==
  $opIsIs : local := true
  [m] := bottomUp a
  putPvarModes(pattern,m)
  object := evalis(op,[a,pattern],m)
  -- have to change code to return value of a
  failCode :=
    ['spadThrowBrightly,['concat,
      '"   Pattern",['QUOTE,bright form2String pattern],
        '"is not matched in assignment to right-hand side."]]
  if $genValue
    then
      null objValUnwrap object => eval failCode
      putValue(op,getValue a)
    else
      code := ['COND,[objVal object,objVal getValue a],[''T,failCode]]
      putValue(op,objNew(code,m))
  putModeSet(op,[m])

evalLETchangeValue(name,value) ==
  -- write the value of name into the environment, clearing dependent
  --  maps if its type changes from its last value
  localEnv := PAIRP $env
  clearCompilationsFlag :=
    val:= (localEnv and get(name,'value,$env)) or get(name,'value,$e)
    null val =>
      not ((localEnv and get(name,'mode,$env)) or get(name,'mode,$e))
    objMode val ~= objMode(value)
  if clearCompilationsFlag then
    clearDependencies(name,true)
  if localEnv and isLocalVar(name)
    then $env:= putHist(name,'value,value,$env)
    else putIntSymTab(name,'value,value,$e)
  objVal value

upLETWithFormOnLhs(op,lhs,rhs) ==
  -- bottomUp for assignment to forms (setelt, table or tuple)
  lhs' := getUnnameIfCan lhs
  rhs' := getUnnameIfCan rhs
  lhs' = 'Tuple =>
    rhs' ~= 'Tuple => throwKeyedMsg("S2IS0039",NIL)
    #(lhs) ~= #(rhs) => throwKeyedMsg("S2IS0038",NIL)
    -- generate a sequence of assignments, using local variables
    -- to first hold the assignments so that things like
    -- (t1,t2) := (t2,t1) will work.
    seq := []
    temps := [GENSYM() for l in rest lhs]
    for lvar in temps repeat mkLocalVar($mapName,lvar)
    for l in reverse rest lhs for t in temps repeat
      transferPropsToNode(getUnname l,l)
      let := mkAtreeNode 'LET
      t'  := mkAtreeNode t
      if m := getMode(l) then putMode(t',m)
      seq := cons([let,l,t'],seq)
    for t in temps for r in reverse rest rhs
      for l in reverse rest lhs repeat
        let := mkAtreeNode 'LET
        t'  := mkAtreeNode t
        if m := getMode(l) then putMode(t',m)
        seq := cons([let,t',r],seq)
    seq := cons(mkAtreeNode 'SEQ,seq)
    ms := bottomUp seq
    putValue(op,getValue seq)
    putModeSet(op,ms)
  rhs' = 'Tuple => throwKeyedMsg("S2IS0039",NIL)
  tree:= seteltable(lhs,rhs) => upSetelt(op,lhs,tree)
  throwKeyedMsg("S2IS0060", NIL)
--  upTableSetelt(op,lhs,rhs)

get_opname_if_can(f) ==
    VECP(f) => f.0
    nil

seteltable(lhs is [f,:argl],rhs) ==
  -- produces the setelt form for trees such as "l.2:= 3"
  g := get_opname_if_can f
  EQ(g,'elt) => altSeteltable [:argl, rhs]
  altSeteltable [:lhs,rhs]

altSeteltable args ==
    for x in args repeat bottomUp x
    newOps := [mkAtreeNode "setelt!", mkAtreeNode "set!"]
    form := NIL

    -- first look for exact matches for any of the possibilities
    while not form for newOp in newOps  repeat
        if selectMms(newOp, args, NIL) then form := [newOp, :args]

    -- now try retracting arguments after the first
    while not form and ( "and"/[retractAtree(a) for a in rest args] ) repeat
        while not form for newOp in newOps  repeat
            if selectMms(newOp, args, NIL) then form := [newOp, :args]

    form


upSetelt(op,lhs,tree) ==
  -- type analyzes implicit setelt forms
  var:=opOf lhs
  transferPropsToNode(getUnname var,var)
  if (m1:=getMode var) then $declaredMode:= m1
  if m1 or ((v1 := getValue var) and (m1 := objMode v1)) then
    putModeSet(var,[m1])
  ms := bottomUp tree
  putValue(op,getValue tree)
  putModeSet(op,ms)

upTableSetelt(op,lhs is [htOp,:args],rhs) ==
  -- called only for undeclared, uninitialized table setelts
  ("*" = (PNAME getUnname htOp).0) and (1 ~= # args) =>
    throwKeyedMsg("S2IS0040",NIL)
  # args ~= 1 =>
    throwKeyedMsg("S2IS0041",[[getUnname htOp,'".[",
      getUnname first args,
        ['",",getUnname arg for arg in rest args],'"]"]])
  keyMode := '(Any)
  putMode (htOp,['Table,keyMode,'(Any)])
  -- if we are to use a new table, we must call the "table"
  -- function to give it an initial value.
  bottomUp [mkAtreeNode 'LET,htOp,[mkAtreeNode 'table]]
  tableCode := objVal getValue htOp
  r := upSetelt(op, lhs, [mkAtreeNode "setelt!", :lhs, rhs])
  $genValue => r
  -- construct code
  t := getValue op
  putValue(op,objNew(['PROGN,tableCode,objVal t],objMode t))
  r

isType t ==
  -- Returns the evaluated type if t is a tree representing a type,
  -- and NIL otherwise
   op:=opOf t
   VECP op =>
     isMap(op:= getUnname op) => NIL
     op = 'Mapping =>
       argTypes := [isType type for type in rest t]
       "or"/[null type for type in argTypes] => nil
       ['Mapping, :argTypes]
     isLocalVar(op) => NIL
     d := isDomainValuedVariable op => d
     type:=
       -- next line handles subscripted vars
         (abbreviation?(op) or (op = 'typeOf) or
           constructor?(op) or (op in '(Record Union Enumeration))) and
             unabbrev unVectorize t
     type and evaluateType type
   d := isDomainValuedVariable op => d
   NIL

upLETtype(op,lhs,type) ==
  -- performs type assignment
  opName:= getUnname lhs
  (not $genValue) and or/[CONTAINED(var,type) for var in $localVars] =>
    compFailure ['"   Cannot compile type assignment to",:bright opName]
  mode :=
    if isPartialMode type then '(Mode)
    else if categoryForm?(type) then '(Category)
         else '(Type)
  val:= objNew(type,mode)
  if isLocalVar(opName) then put(opName,'value,val,$env)
  else putHist(opName,'value,val,$e)
  putValue(op,val)
  -- have to fix the following
  putModeSet(op,[mode])

assignSymbol(symbol, value, domain) ==
-- Special function for binding an interpreter variable from within algebra
-- code.  Does not do the assignment and returns nil, if the variable is
-- already assigned
  val := get(symbol, 'value, $e) => nil
  obj := objNew(wrap value, devaluate domain)
  put(symbol, 'value, obj, $e)
  true

--% Handler for Interpreter Macros

getInterpMacroNames() ==
  names := [n for [n,:.] in $InterpreterMacroAlist]
  if (e := CAAR $InteractiveFrame) and (m := assoc("--macros--",e)) then
    names := append(names, [n for [n, :.] in rest m])
  MSORT names

isInterpMacro name ==
  -- look in local and then global environment for a macro
  null IDENTP name => NIL
  name in $specialOps => NIL
  (m := get("--macros--",name,$env)) => m
  (m := get("--macros--",name,$e))   => m
  (m := get("--macros--",name,$InteractiveFrame))   => m
  -- $InterpreterMacroAlist will probably be phased out soon
  (sv := assoc(name, $InterpreterMacroAlist)) => CONS(NIL, rest sv)
  NIL

--% Handlers for prefix QUOTE

upQUOTE t ==
  t isnt [op,expr] => NIL
  ms:= list
    m:= getBasicMode expr => m
    IDENTP expr =>
--    $useSymbolNotVariable => $Symbol
      ['Variable,expr]
    $OutputForm
  evalQUOTE(op,[expr],ms)
  putModeSet(op,ms)

evalQUOTE(op,[expr],[m]) ==
  triple:=
    $genValue => objNewWrap(expr,m)
    objNew(['QUOTE,expr],m)
  putValue(op,triple)

--% Handler for pretend

uppretend t ==
  t isnt [op,expr,type] => NIL
  mode := evaluateType unabbrev type
  not isValidType(mode) => throwKeyedMsg("S2IE0004",[mode])
  bottomUp expr
  putValue(op,objNew(objVal getValue expr,mode))
  putModeSet(op,[mode])

--% Handlers for REDUCE

getReduceFunction(op,type,result, locale) ==
  -- return the function cell for operation with the signature
  --  (type,type) -> type, possible from locale
  if type is ['Variable,var] then
    args := [arg := mkAtreeNode var,arg]
    putValue(arg,objNewWrap(var,type))
  else
    args := [arg := mkAtreeNode "%1",arg]
    if type=$Symbol then putValue(arg,objNewWrap("%1",$Symbol))
  putModeSet(arg,[type])
  vecOp:=mkAtreeNode op
  transferPropsToNode(op,vecOp)
  if locale then putAtree(vecOp,'dollar,locale)
  mmS:= selectMms(vecOp,args,result)
  mm:= or/[mm for (mm:=[[.,:sig],fun,cond]) in mmS |
    (isHomogeneousArgs sig) and and/[null c for c in cond]]
  null mm => 'failed
  [[dc,:sig],fun,:.]:=mm
  dc = 'local => [MKQ [fun, :'local], :first sig]
  dcVector := evalDomain dc
  $compilingMap =>
    k := NRTgetMinivectorIndex(
      NRTcompiledLookup(op,sig,dcVector),op,sig,dcVector)
    ['ELT,"$$$",k]  --$$$ denotes minivector
  env:=
    NRTcompiledLookup(op,sig,dcVector)
  MKQ env

isHomogeneous sig ==
  --return true if sig describes a homogeneous binary operation
  sig.0=sig.1 and sig.1=sig.2

isHomogeneousArgs sig ==
  --return true if sig describes a homogeneous binary operation
  sig.1=sig.2

--% Handlers for REPEAT

transformREPEAT [:itrl,body] ==
  -- syntactic transformation of repeat iterators, called from mkAtree2
  iterList:=[:iterTran1 for it in itrl] where iterTran1 ==
    it is ['STEP,index,lower,step,:upperList] =>
      [['STEP,index,mkAtree1 lower,mkAtree1 step,:[mkAtree1 upper
        for upper in upperList]]]
    it is ['IN,index,s] =>
      [['IN,index,mkAtree1 s]]
    it is ['ON,index,s] =>
      [['IN,index,mkAtree1 ['tails,s]]]
    it is ['WHILE,b] =>
      [['WHILE,mkAtree1 b]]
    it is ['_|,pred] =>
      [['SUCHTHAT,mkAtree1 pred]]
    it is [op,:.] and (op in '(VALUE UNTIL)) => nil
  bodyTree:=mkAtree1 body
  iterList:=NCONC(iterList,[:iterTran2 for it in itrl]) where iterTran2 ==
    it is ['STEP,:.] => nil
    it is ['IN,:.] => nil
    it is ['ON,:.] => nil
    it is ['WHILE,:.] => nil
    it is [op,b] and (op in '(UNTIL VALUE)) =>
      [[op,mkAtree1 b]]
    it is ['_|,pred] => nil
    keyedSystemError("S2GE0016",
      ['"transformREPEAT",'"Unknown type of iterator"])
  [:iterList,bodyTree]

upREPEAT t ==
  -- REPEATS always return void() of Void
  -- assures throw to interpret-code mode goes to outermost loop
  $repeatLabel : local := MKQ GENSYM()
  $breakCount  : local := 0
  $repeatBodyLabel : local := MKQ GENSYM()
  $iterateCount    : local := 0
  $compilingLoop => upREPEAT1 t
  upREPEAT0 t

upREPEAT0 t ==
  -- sets up catch point for interp-only mode
  $compilingLoop: local := true
  ms := CATCH('loopCompiler,upREPEAT1 t)
  ms = 'tryInterpOnly => interpOnlyREPEAT t
  ms

upREPEAT1 t ==
  -- repeat loop handler with compiled body
  -- see if it has the expected form
  t isnt [op,:itrl,body] => NIL
  -- determine the mode of the repeat loop. At the moment, if there
  -- there are no iterators and there are no "break" statements, then
  -- the return type is Exit, otherwise Void.
  repeatMode :=
    null(itrl) and ($breakCount=0) => $Void
    $Void

  -- if interpreting, go do that
  $interpOnly => interpREPEAT(op,itrl,body,repeatMode)

  -- analyze iterators and loop body
  upLoopIters itrl
  bottomUpCompile body

  -- now that the body is analyzed, we should know everything that
  -- is in the UNTIL clause
  for itr in itrl repeat
    itr is ['UNTIL, pred] => bottomUpCompilePredicate(pred,'"until")

  -- now go do it
  evalREPEAT(op,rest t,repeatMode)
  putModeSet(op,[repeatMode])

evalREPEAT(op,[:itrl,body],repeatMode) ==
  -- generate code for loop
  bodyMode := computedMode body
  bodyCode := getArgValue(body,bodyMode)
  if $iterateCount > 0 then
    bodyCode := ['CATCH,$repeatBodyLabel,bodyCode]
  code := ['REPEAT,:[evalLoopIter itr for itr in itrl], bodyCode]
  if repeatMode = $Void then code := ['OR,code,'(voidValue)]
  code := timedOptimization code
  if $breakCount > 0 then code := ['CATCH,$repeatLabel,code]
  val:=
    $genValue =>
      timedEVALFUN code
      objNewWrap(voidValue(),repeatMode)
    objNew(code,repeatMode)
  putValue(op,val)

interpOnlyREPEAT t ==
  -- interpret-code mode call to upREPEAT
  $genValue: local := true
  $interpOnly: local := true
  upREPEAT1 t

interpREPEAT(op,itrl,body,repeatMode) ==
  -- performs interpret-code repeat
  $indexVars: local := NIL
  $indexTypes: local := NIL
  code :=
      -- we must insert a CATCH for the iterate clause
      ['REPEAT,:[interpIter itr for itr in itrl],
        ['CATCH,$repeatBodyLabel,interpLoop(body,$indexVars,
          $indexTypes,nil)]]
  CATCH($repeatLabel,timedEVALFUN code)
  val:= objNewWrap(voidValue(),repeatMode)
  putValue(op,val)
  putModeSet(op,[repeatMode])

interpLoop(expr,indexList,indexTypes,requiredType) ==
  -- generates code for interp-only repeat body
  ['interpLoopIter,MKQ expr,MKQ indexList,['LIST,:indexList],
    MKQ indexTypes, MKQ requiredType]

interpLoopIter(exp,indexList,indexVals,indexTypes,requiredType) ==
  -- call interpreter on exp with loop vars in indexList with given
  --  values and types, requiredType is used from interpCOLLECT
  --  to indicate the required type of the result
  emptyAtree exp
  for i in indexList for val in indexVals for type in indexTypes repeat
    put(i,'value,objNewWrap(val,type),$env)
  bottomUp exp
  v:= getValue exp
  val :=
    null requiredType => v
    coerceInteractive(v,requiredType)
  null val =>
    throwKeyedMsgCannotCoerceWithValue(objVal v,objMode v,requiredType)
  objValUnwrap val

--% Handler for return

upreturn t ==
  -- make sure we are in a user function
  t isnt [op,val] => NIL
  (null $compilingMap) and (null $interpOnly) =>
    throwKeyedMsg("S2IS0047",NIL)
  if $mapTarget then putTarget(val,$mapTarget)
  bottomUp val
  if $mapTarget
    then
      val' := getArgValue(val, $mapTarget)
      m := $mapTarget
    else
      val' := wrapped2Quote objVal getValue val
      m := computedMode val
  cn := mapCatchName $mapName
  $mapReturnTypes := insert(m, $mapReturnTypes)
  $mapThrowCount := $mapThrowCount + 1
  -- if $genValue then we are interpreting the map
  $genValue => THROW(cn,objNewWrap(removeQuote val',m))
  putValue(op,objNew(['THROW,MKQ cn,val'],m))
  putModeSet(op,[$Exit])

--% Handler for SEQ

upSEQ u ==
  -- assumes that exits were translated into if-then-elses
  -- handles flat SEQs and embedded returns
  u isnt [op,:args] => NIL
  if (target := getTarget(op)) then putTarget(last args, target)
  for x in args repeat bottomUp x
  null (m := computedMode last args) =>
    keyedSystemError("S2GE0016",['"upSEQ",
      '"last line of SEQ has no mode"])
  evalSEQ(op,args,m)
  putModeSet(op,[m])

evalSEQ(op,args,m) ==
  -- generate code for SEQ
  [:argl,last] := args
  val:=
    $genValue => getValue last
    bodyCode := nil
    for x in args repeat
      (m1 := computedMode x) =>
        (av := getArgValue(x,m1)) ~= voidValue() =>
          bodyCode := [av,:bodyCode]
    code:=
      bodyCode is [c] => c
      ['PROGN,:reverse bodyCode]
    objNew(code,m)
  putValue(op,val)

--% Handlers for Tuple

upTuple t ==
  --Computes the common mode set of the construct by resolving across
  --the argument list, and evaluating
  t isnt [op,:l] => nil
  dol := getAtree(op,'dollar)
  tar := getTarget(op) or dol
  null l => upNullTuple(op,l,tar)
  isTaggedUnion tar => upTaggedUnionConstruct(op,l,tar)
  aggs := '(List)
  if tar and PAIRP(tar) and not isPartialMode(tar) then
    first(tar) in aggs =>
      ud := CADR tar
      for x in l repeat if not getTarget(x) then putTarget(x,ud)
    first(tar) in '(Matrix SquareMatrix RectangularMatrix) =>
      vec := ['List,underDomainOf tar]
      for x in l repeat if not getTarget(x) then putTarget(x,vec)
  argModeSetList:= [bottomUp x for x in l]
  eltTypes := replaceSymbols([first x for x in argModeSetList],l)
  if not isPartialMode(tar) and tar is ['Tuple,ud] then
    mode := ['Tuple, resolveTypeListAny cons(ud,eltTypes)]
  else mode := ['Tuple, resolveTypeListAny eltTypes]
  if isPartialMode tar then tar:=resolveTM(mode,tar)
  evalTuple(op,l,mode,tar)

evalTuple(op,l,m,tar) ==
  [agg,:.,underMode]:= m
  code := asTupleNewCode(#l,
    [(getArgValue(x,underMode) or throwKeyedMsg("S2IC0007",[underMode])) for x in l])
  val :=
    $genValue => objNewWrap(timedEVALFUN code,m)
    objNew(code,m)
  if tar then val1 := coerceInteractive(val,tar) else val1 := val

  val1 =>
    putValue(op,val1)
    putModeSet(op,[tar or m])
  putValue(op,val)
  putModeSet(op,[m])

upNullTuple(op,l,tar) ==
  -- handler for the empty tuple
  defMode :=
    tar and tar is [a,b] and (a in '(Stream Vector List)) and
      not isPartialMode(b) => ['Tuple,b]
    '(Tuple (None))
  val := objNewWrap(asTupleNew(0,NIL), defMode)
  tar and not isPartialMode(tar) =>
    null (val' := coerceInteractive(val,tar)) =>
      throwKeyedMsg("S2IS0013",[tar])
    putValue(op,val')
    putModeSet(op,[tar])
  putValue(op,val)
  putModeSet(op,[defMode])

--% Handler for typeOf

uptypeOf form ==
  form isnt [op, arg] => NIL
  if VECP arg then transferPropsToNode(getUnname arg,arg)
  if m := isType(arg) then
    m :=
      categoryForm?(m) => '(Category)
      isPartialMode m  => '(Mode)
      '(Type)
  else if not (m := getMode arg) then [m] := bottomUp arg
  t := typeOfType m
  putValue(op, objNew(m,t))
  putModeSet(op,[t])

typeOfType type ==
  type in '((Mode) (Type)) => '(Category)
  '(Type)

--% Handler for where

upwhere t ==
  -- upwhere does the puts in where into a local environment
  t isnt [op,tree,clause] => NIL
  -- since the "clause" might be a local macro, we now call mkAtree
  -- on the "tree" part (it is not yet a vat)
  not $genValue =>
    compFailure [:bright '"  where",
      '"for compiled code is not yet implemented."]
  $whereCacheList : local := nil
  [env,:e] := upwhereClause(clause,$env,$e)
  tree := upwhereMkAtree(tree,env,e)
  if x := getAtree(op,'dollar) then
    atom tree => throwKeyedMsg("S2IS0048",NIL)
    putAtree(first tree, 'dollar, x)
  upwhereMain(tree,env,e)
  val := getValue tree
  putValue(op,val)
  result := putModeSet(op,getModeSet tree)
  wcl := [op for op in $whereCacheList]
  for op in wcl repeat clearDependencies(op,'T)
  result

upwhereClause(tree,env,e) ==
  -- uses the variable bindings from env and e and returns an environment
  -- of its own bindings
  $env: local := copyHack env
  $e: local := copyHack e
  bottomUp tree
  [$env,:$e]

upwhereMkAtree(tree,$env,$e) == mkAtree tree

upwhereMain(tree,$env,$e) ==
  -- uses local copies of $env and $e while evaluating tree
  bottomUp tree

copyHack(env) ==
  -- makes a copy of an environment with the exception of pairs
  -- (localModemap . something)
  c:= CAAR env
  d:= [fn p for p in c] where fn(p) ==
    CONS(first p, [(EQCAR(q, 'localModemap) => q; copy q) for q in rest p])
  [[d]]

-- Creates the function names of the special function handlers and puts
--  them on the property list of the function name


for name in $specialOps repeat
    (
      functionName:=INTERNL('up,name) ;
      MAKEPROP(name,'up,functionName) ;
      functionName
     )
