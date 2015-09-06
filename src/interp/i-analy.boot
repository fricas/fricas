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

--% Interpreter Analysis Functions

--% Basic Object Type Identification

getBasicMode x ==  getBasicMode0(x,$useIntegerSubdomain)

getBasicMode0(x,useIntegerSubdomain) ==
  --  if x is one of the basic types (Integer String Float Boolean) then
  --  this function returns its type, and nil otherwise
  x is nil => $EmptyMode
  STRINGP x => $String
  INTEGERP x =>
    useIntegerSubdomain =>
      x > 0 => $PositiveInteger
      x = 0 => $NonNegativeInteger
      $Integer
    $Integer
  FLOATP x => $DoubleFloat
  (x='noBranch) or (x='noValue) => $NoValueMode
  nil

getBasicObject x ==
  INTEGERP    x =>
    t :=
      not $useIntegerSubdomain => $Integer
      x > 0 => $PositiveInteger
      x = 0 => $NonNegativeInteger
      $Integer
    objNewWrap(x,t)
  STRINGP x => objNewWrap(x,$String)
  FLOATP  x => objNewWrap(x,$DoubleFloat)
  NIL

getMinimalVariableTower(var,t) ==
  -- gets the minimal polynomial subtower of t that contains the
  -- given variable. Returns NIL if none.
  STRINGP(t) or IDENTP(t) => NIL
  t = $Symbol => t
  t is ['Variable,u] =>
    (u = var) => t
    NIL
  t is ['Polynomial,.] => t
  t is [up,t',u,.] and MEMQ(up,$univariateDomains) =>
    -- power series have one more arg and different ordering
    u = var => t
    getMinimalVariableTower(var,t')
  t is [up,u,t'] and MEMQ(up,$univariateDomains) =>
    u = var => t
    getMinimalVariableTower(var,t')
  t is [mp,u,t'] and MEMQ(mp,$multivariateDomains) =>
    var in u => t
    getMinimalVariableTower(var,t')
  null (t' := underDomainOf t) => NIL
  getMinimalVariableTower(var,t')

getMinimalVarMode(id,m) ==
  --  This function finds the minimum polynomial subtower type of the
  --  polynomial domain tower m which id to which can be coerced
  --  It includes all polys above the found level if they are
  --  contiguous.
  --  E.g.:    x and G P[y] P[x] I ---> P[y] P[x] I
  --           x and P[y] G P[x] I ---> P[x] I
  m is ['Mapping, :.] => m
  defaultMode :=
    $Symbol
  null m => defaultMode
  (vl := polyVarlist m) and ((id in vl) or 'all in vl) =>
    SUBSTQ('(Integer),$EmptyMode,m)
  (um := underDomainOf m) => getMinimalVarMode(id,um)
  defaultMode

polyVarlist m ==
  --  If m is a polynomial type this function returns a list of its
  --  top level variables, and nil otherwise
  -- ignore any QuotientFields that may separate poly types
  m is [=$QuotientField,op] => polyVarlist op
  m is [op,a,:.] =>
    op in '(UnivariateTaylorSeries UnivariateLaurentSeries
      UnivariatePuiseuxSeries) =>
        [., ., a, :.] := m
        a := removeQuote a
        [a]
    op in '(Polynomial Expression) =>
      '(all)
    a := removeQuote a
    op in '(UnivariatePolynomial) =>
      [a]
    op in $multivariateDomains =>
          a
  nil

--% Pushing Down Target Information

pushDownTargetInfo(op,target,arglist) ==
  -- put target info on args for certain operations
  target = $OutputForm => NIL
  target = $Any        => NIL
  target is ['Union, dom, tag] and tag = '"failed" => NIL
  n := LENGTH arglist
  pushDownOnArithmeticVariables(op,target,arglist)
  (pdArgs := pushDownOp?(op,n)) =>
    for i in pdArgs repeat
      x := arglist.i
      if not getTarget(x) then putTarget(x,target)
  nargs := #arglist
  1 = nargs =>
    (op = 'SEGMENT) and (target is ['UniversalSegment,S]) =>
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,S)
  2 = nargs =>
    op = "*" =>            -- only push down on 1st arg if not immed
      if not getTarget CADR arglist then putTarget(CADR arglist,target)
      getTarget(x := first arglist) => NIL
      if getUnname(x) ~= $immediateDataSymbol then putTarget(x,target)
    op = "**" or op = "^" =>           -- push down on base
      if not getTarget first arglist then putTarget(first arglist, target)
    (op = 'equation) and (target is ['Equation,S]) =>
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,S)
    (op = '_/) =>
      targ :=
        target is ['Fraction,S] => S
        target
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,targ)
    (op = 'SEGMENT) and (target is ['Segment,S]) =>
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,S)
    (op = 'SEGMENT) and (target is ['UniversalSegment,S]) =>
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,S)
    NIL
  NIL

pushDownOnArithmeticVariables(op,target,arglist) ==
  -- tries to push appropriate target information onto variable
  -- occurring in arithmetic expressions
  PAIRP(target) and first(target) = 'Variable => NIL
  not MEMQ(op,'(_+ _- _* _*_* _/)) => NIL
  not containsPolynomial(target)   => NIL
  for x in arglist for i in 1.. repeat
    VECP(x) =>   -- leaf
      transferPropsToNode(xn := getUnname(x),x)
      getValue(x) or (xn = $immediateDataSymbol) => NIL
      t := getMinimalVariableTower(xn,target) or target
      if not getTarget(x) then putTarget(x,t)
    PAIRP(x) =>  -- node
      [op',:arglist'] := x
      pushDownOnArithmeticVariables(getUnname op',target,arglist')
  arglist

pushDownOp?(op,n) ==
  -- determine if for op with n arguments whether for all modemaps
  -- the target type is equal to one or more arguments. If so, a list
  -- of the appropriate arguments is returned.
  ops := [sig for [sig,:.] in getModemapsFromDatabase(op,n)]
  null ops => NIL
  op in '(_+ _* _- _exquo) => [i for i in 0..(n-1)]
  -- each signature has form
  -- [domain of implementation, target, arg1, arg2, ...]
  -- sameAsTarg is a vector that counts the number of modemaps that
  -- have the corresponding argument equal to the target type
  sameAsTarg := GETZEROVEC n
  numMms := LENGTH ops
  for [.,targ,:argl] in ops repeat
    for arg in argl for i in 0.. repeat
      targ = arg => SETELT(sameAsTarg,i,1 + sameAsTarg.i)
  -- now see which args have their count = numMms
  ok := NIL
  for i in 0..(n-1) repeat
    if numMms = sameAsTarg.i then ok := cons(i,ok)
  reverse ok

--% Bottom Up Processing

-- Also see I-SPEC BOOT for special handlers and I-MAP BOOT for
-- user function processing.

bottomUp t ==
  -- bottomUp takes an attributed tree, and returns the modeSet for it.
  -- As a side-effect it also evaluates the tree.
  t is [op,:argl] =>
    tar := getTarget op
    getUnname(op) ~= $immediateDataSymbol and (v := getValue op) =>
      om := objMode(v)
      null tar => [om]
      (r := resolveTM(om,tar)) => [r]
      [om]
    if atom op then
      opName:= getUnname op
      if opName in $localVars then
        putModeSet(op,bottomUpIdentifier(op,opName))
      else
        transferPropsToNode(opName,op)
    else
      opName := NIL
      bottomUp op

    opVal := getValue op

    -- call a special handler if we are not being package called
    dol := getAtree(op,'dollar) and (opName ~= 'construct)

    (null dol) and (fn := GET(opName, "up")) and (u := FUNCALL(fn, t)) => u
    nargs := #argl
    if opName then for x in argl for i in 1.. repeat
      putAtree(x,'callingFunction,opName)
      putAtree(x,'argumentNumber,i)
      putAtree(x,'totalArgs,nargs)

    if tar then pushDownTargetInfo(opName,tar,argl)

    -- see if we are calling a declared user map
    -- if so, push down the declared types as targets on the args
    if opVal and (objVal opVal  is ['SPADMAP,:.]) and
      (getMode op is ['Mapping,:ms]) and (nargs + 1= #ms) then
        for m in rest ms for x in argl repeat putTarget(x,m)

    argModeSetList:= [bottomUp x for x in argl]

    if not tar and opName = "*" and nargs = 2 then
        [[t1],[t2]] := argModeSetList
        tar := computeTypeWithVariablesTarget(t1, t2)
        tar =>
            pushDownTargetInfo(opName,tar,argl)
            argModeSetList:= [bottomUp x for x in argl]

    ms := bottomUpForm(t,op,opName,argl,argModeSetList)
    -- If this is a type producing form, then we don't want
    -- to store the representation object in the environment.
    -- Rather, we want to record the reified canonical form.
    if ms is [m] and (m is ["Mode"] or isCategoryForm(m,$e))
    then putValue(t,objNew(devaluate objValUnwrap getValue t, m))

    -- given no target or package calling, force integer constants to
    -- belong to tightest possible subdomain

    op := first t                -- may have changed in bottomUpElt
    $useIntegerSubdomain and null tar and null dol and
      isEqualOrSubDomain(first ms,$Integer) =>
        val := objVal getValue op
        isWrapped val =>       -- constant if wrapped
          val := unwrap val
          bm := getBasicMode val
          putValue(op,objNewWrap(val,bm))
          putModeSet(op,[bm])
        ms
    ms
  m := getBasicMode t => [m]
  IDENTP (id := getUnname t) =>
    putModeSet(t,bottomUpIdentifier(t,id))
  keyedSystemError("S2GE0016",['"bottomUp",'"unknown object form"])

computeTypeWithVariablesTarget(p, q) ==
    polyVarlist(p) or polyVarlist(q) =>
        t := resolveTT(p, q)
        polyVarlist(t) => t
        NIL
    NIL

bottomUpCompile t ==
  $genValue:local := false
  ms := bottomUp t
  compTran1 objVal getValue t
  ms

bottomUpUseSubdomain t ==
  $useIntegerSubdomain : local := true
  ms := bottomUp t
  ($immediateDataSymbol ~= getUnname(t)) or ($Integer ~= first(ms)) => ms
  null INTEGERP(num := objValUnwrap getValue t) => ms
  o := getBasicObject(num)
  putValue(t,o)
  ms := [objMode o]
  putModeSet(t,ms)
  ms

bottomUpPredicate(pred, name) ==
  putTarget(pred,$Boolean)
  ms := bottomUp pred
  $Boolean ~= first ms => throwKeyedMsg("S2IB0001", [name])
  ms

bottomUpCompilePredicate(pred, name) ==
  $genValue:local := false
  bottomUpPredicate(pred,name)

bottomUpIdentifier(t,id) ==
  m := isType t => bottomUpType(t, m)
  EQ(id,'noMapVal) => throwKeyedMsg("S2IB0002", NIL)
  EQ(id,'noBranch) =>
    keyedSystemError("S2GE0016",
      ['"bottomUpIdentifier",'"trying to evaluate noBranch"])
  transferPropsToNode(id,t)
  defaultType := ['Variable,id]
  -- This was meant to stop building silly symbols but had some unfortunate
  -- side effects, like not being able to say e:=foo in the interpreter.  MCD
--  defaultType :=
--    getModemapsFromDatabase(id,1) =>
--      userError ['"Cannot use operation name as a variable: ", id]
--    ['Variable, id]
  u := getValue t => --non-cached values MAY be re-evaluated
    tar := getTarget t
    expr:= objVal u
    om := objMode(u)
    (om ~= $EmptyMode) and (om isnt ['RuleCalled,.]) =>
      $genValue or GENSYMP(id) =>
        null tar => [om]
        (r := resolveTM(om,tar)) => [r]
        [om]
      bottomUpDefault(t,id,defaultType,getTarget t)
    interpRewriteRule(t,id,expr) or
      (isMapExpr expr and [objMode(u)]) or
        keyedSystemError("S2GE0016",
          ['"bottomUpIdentifier",'"cannot evaluate identifier"])
  bottomUpDefault(t,id,defaultType,getTarget t)

bottomUpDefault(t,id,defaultMode,target) ==
  if $genValue
    then bottomUpDefaultEval(t,id,defaultMode,target,nil)
    else bottomUpDefaultCompile(t,id,defaultMode,target,nil)

bottomUpDefaultEval(t,id,defaultMode,target,isSub) ==
  -- try to get value case.

  -- 1. declared mode but no value case
  (m := getMode t) =>
    m is ['Mapping,:.] => throwKeyedMsg("S2IB0003",[getUnname t])

    -- hmm, try to treat it like target mode or declared mode
    if isPartialMode(m) then m := resolveTM(['Variable,id],m)
    -- if there is a target, probably want it to be that way and not
    -- declared mode. Like "x" in second line:
    --   x : P[x] I
    --   y : P[x] I
    target and not isSub and
      (val := coerceInteractive(objNewWrap(id,['Variable,id]),target))=>
        putValue(t,val)
        [target]
    -- Ok, see if we can make it into declared mode from symbolic form
    -- For example, (x : P[x] I; x + 1)
    not target and not isSub and m and
      (val := coerceInteractive(objNewWrap(id,['Variable,id]),m)) =>
        putValue(t,val)
        [m]
    -- give up
    throwKeyedMsg("S2IB0004", [id, m])

  -- 2. no value and no mode case
  val := objNewWrap(id,defaultMode)
  (null target) or (defaultMode = target) =>
    putValue(t,val)
    [defaultMode]
  if isPartialMode target then
    -- this hackery will go away when Symbol is not the default type
    if defaultMode = $Symbol and (target is [D,x,.]) then
      (D in $univariateDomains and (x = id)) or
        (D in $multivariateDomains and (id in x)) =>
           dmode := [D,x,$Integer]
           (val' := coerceInteractive(objNewWrap(id,
             ['Variable,id]),dmode)) =>
               defaultMode := dmode
               val := val'
      NIL
    target := resolveTM(defaultMode,target)
  -- The following is experimental.  SCM 10/11/90
  if target and (tm := getMinimalVarMode(id, target)) then
    target := tm
  (null target) or null (val' := coerceInteractive(val,target)) =>
    putValue(t,val)
    [defaultMode]
  putValue(t,val')
  [target]

bottomUpDefaultCompile(t,id,defaultMode,target,isSub) ==
  tmode := getMode t
  tval  := getValue t
  expr:=
    id in $localVars => id
    tmode or tval =>
      envMode := tmode or objMode tval
      envMode is ['Variable, :.] => objVal tval
      id = $immediateDataSymbol => objVal tval
      ['getValueFromEnvironment,MKQ id,MKQ envMode]
    wrap id
  tmode and tval and (mdv := objMode tval) =>
    if isPartialMode tmode then
      null (tmode := resolveTM(mdv,tmode)) =>
        keyedMsgCompFailure("S2IB0010",NIL)
    putValue(t,objNew(expr,tmode))
    [tmode]
  tmode or (tval and (tmode := objMode tval)) =>
    putValue(t,objNew(expr,tmode))
    [tmode]
  obj := objNew(expr,defaultMode)
  canCoerceFrom(defaultMode, target) and
    (obj' := coerceInteractive(obj, target)) =>
        putValue(t, obj')
        [target]
  putValue(t,obj)
  [defaultMode]

interpRewriteRule(t,id,expr) ==
  null get(id,'isInterpreterRule,$e) => NIL
  (ms:= selectLocalMms(t,id,nil,nil)) and (ms:=evalForm(t,id,nil,ms)) =>
    ms
  nil

bottomUpForm(t,op,opName,argl,argModeSetList) ==
  not($inRetract) =>
    bottomUpForm3(t,op,opName,argl,argModeSetList)
  bottomUpForm2(t,op,opName,argl,argModeSetList)

bottomUpForm3(t,op,opName,argl,argModeSetList) ==
  $origArgModeSetList:local  := COPY argModeSetList
  bottomUpForm2(t,op,opName,argl,argModeSetList)

bottomUpForm2(t,op,opName,argl,argModeSetList) ==
  not atom t and EQ(opName,"%%") => bottomUpPercent t
  opVal := getValue op

  -- for things with objects in operator position, be careful before
  -- we enter general modemap selection

  lookForIt :=
    getAtree(op,'dollar) => true
    not opVal => true
    opMode := objMode opVal
    not (opModeTop := IFCAR opMode) => true
    opModeTop in '(Record Union) => false
    opModeTop in '(Variable Mapping FunctionCalled RuleCalled AnonymousFunction) => true
    false

  -- get rid of Union($, "failed") except when op is "=" and all
  -- modesets are the same

  $genValue and
    not (opName = "=" and argModeSetList is [[m],[=m]] and m is ['Union,:.]) and
      (u := bottomUpFormUntaggedUnionRetract(t,op,opName,argl,argModeSetList)) => u

  lookForIt and (u := bottomUpFormTuple(t, op, opName, argl, argModeSetList)) => u

  -- opName can change in the call to selectMms

  (lookForIt and (mmS := selectMms(op,argl,getTarget op))) and
    (mS := evalForm(op,opName := getUnname op,argl,mmS)) =>
      putModeSet(op,mS)
  bottomUpForm0(t,op,opName,argl,argModeSetList)

bottomUpFormTuple(t, op, opName, args, argModeSetList) ==
  getAtree(op,'dollar) => NIL
  null (singles := getModemapsFromDatabase(opName, 1)) => NIL

  -- see if any of the modemaps have Tuple arguments
  haveTuple := false
  for mm in singles while not haveTuple repeat
    if getFirstArgTypeFromMm(mm) is ["Tuple",.] then haveTuple := true
  not haveTuple => nil
  nargs := #args
  nargs = 1 and getUnname first args = "Tuple" => NIL
  nargs = 1 and (ms := bottomUp first args) and
    (ms is [["Tuple",.]] or ms is [["List",.]]) => NIL

  -- now make the args into a tuple

  newArg := [mkAtreeNode "Tuple",:args]
  bottomUp [op, newArg]

printableArgModeSetList() ==
  amsl := nil
  for a in reverse $origArgModeSetList repeat
    b := prefix2String first a
    if ATOM b then b := [b]
    amsl := ['%l,:b,:amsl]
  if amsl then amsl := rest amsl
  amsl

bottomUpForm0(t,op,opName,argl,argModeSetList) ==
  op0 := op
  opName0 := opName

  m := isType t =>
    bottomUpType(t, m)

  opName = 'copy and argModeSetList is [[['Record,:rargs]]] =>
    -- this is a hack until Records go through the normal
    -- modemap selection process
    rtype := ['Record,:rargs]
    code := optRECORDCOPY(['RECORDCOPY, getArgValue(first argl, rtype),
                           #rargs])
    if $genValue then code := wrap timedEVALFUN code
    val := objNew(code,rtype)
    putValue(t,val)
    putModeSet(t,[rtype])

  m := getModeOrFirstModeSetIfThere op
  m is ['Record,:.] and argModeSetList is [[['Variable,x]]] and
      member(x,getUnionOrRecordTags m) and (u := bottomUpElt t) => u
  m is ['Union,:.] and argModeSetList is [[['Variable,x]]] =>
      member(x,getUnionOrRecordTags m) and (u := bottomUpElt t) => u
      not $genValue =>
        amsl := printableArgModeSetList()
        throwKeyedMsgSP("S2IB0008",['"the union object",amsl], op)
      object := retract getValue op
      object = 'failed =>
        throwKeyedMsgSP("S2IB0008",['"the union object",amsl], op)
      putModeSet(op,[objMode(object)])
      putValue(op,object)
      (u := bottomUpElt t) => u
      bottomUpForm0(t,op,opName,argl,argModeSetList)

  (opName ~= "elt") and (opName ~= "apply") and
    #argl = 1 and first first argModeSetList is ['Variable, var]
      and var in '(first last rest) and
        isEltable(op, argl, #argl) and (u := bottomUpElt t) => u

  $genValue and
    ( u:= bottomUpFormRetract(t,op,opName,argl,argModeSetList) ) => u

  (opName ~= "elt") and (opName ~= "apply") and
    isEltable(op, argl, #argl) and (u := bottomUpElt t) => u

  if FIXP $HTCompanionWindowID then
    mkCompanionPage('operationError, t)

  amsl := printableArgModeSetList()
  opName1 :=
    opName0 = $immediateDataSymbol =>
        (o := coerceInteractive(getValue op0,$OutputForm)) =>
            outputTran2 objValUnwrap o
        NIL
    opName0

  if null(opName1) then
    opName1 :=
        (o := getValue op0) => prefix2String objMode o
        '"<unknown type>"
    msgKey :=
        null amsl => "S2IB0013"
        "S2IB0012"
  else
    msgKey :=
        null amsl => "S2IB0011"
        (n := isSharpVarWithNum opName1) =>
            opName1 := n
            "S2IB0008g"
        "S2IB0008"

  sayIntelligentMessageAboutOpAvailability(opName1, #argl)

  not $genValue =>
    keyedMsgCompFailureSP(msgKey,[opName1, amsl], op0)
  throwKeyedMsgSP(msgKey,[opName1, amsl], op0)

sayIntelligentMessageAboutOpAvailability(opName, nArgs) ==
  -- see if we can give some decent messages about the availability if
  -- library messages

  NUMBERP opName => NIL

  oo :=  object2Identifier opOf opName
  if ( oo = "%" ) or ( domainForm? opName ) then
    opName := "elt"

  nAllExposedMmsWithName := #getModemapsFromDatabase(opName, NIL)
  nAllMmsWithName        := #getAllModemapsFromDatabase(opName, NIL)

  -- first see if there are ANY ops with this name

  if nAllMmsWithName = 0 then
    sayKeyedMsg("S2IB0008a", [opName])
  else if nAllExposedMmsWithName = 0 then
    nAllMmsWithName = 1 => sayKeyedMsg("S2IB0008b", [opName])
    sayKeyedMsg("S2IB0008c", [opName, nAllMmsWithName])
  else
    -- now talk about specific arguments
    nAllExposedMmsWithNameAndArgs   := #getModemapsFromDatabase(opName, nArgs)
    nAllMmsWithNameAndArgs          := #getAllModemapsFromDatabase(opName, nArgs)
    nAllMmsWithNameAndArgs = 0 =>
        sayKeyedMsg("S2IB0008d", [opName, nArgs, nAllExposedMmsWithName, nAllMmsWithName - nAllExposedMmsWithName])
    nAllExposedMmsWithNameAndArgs = 0 =>
        sayKeyedMsg("S2IB0008e", [opName, nArgs, nAllMmsWithNameAndArgs - nAllExposedMmsWithNameAndArgs])
    sayKeyedMsg("S2IB0008f", [opName, nArgs, nAllExposedMmsWithNameAndArgs, nAllMmsWithNameAndArgs - nAllExposedMmsWithNameAndArgs])
  nil

bottomUpType(t, type) ==
  mode :=
    if isPartialMode type then '(Mode)
    else if categoryForm?(type) then '(Category)
         else '(Type)
  val:= objNew(type,mode)
  putValue(t,val)
  -- have to fix the following
  putModeSet(t,[mode])

bottomUpPercent(tree is [op,:argl]) ==
  -- handles a call %%(5), which means the output of step 5
  -- %%() is the same as %%(-1)
  null argl =>
    val:= fetchOutput(-1)
    putValue(op,val)
    putModeSet(op,[objMode(val)])
  argl is [t] =>
    i:= getArgValue(t,$Integer) =>
      val:= fetchOutput i
      putValue(op,val)
      putModeSet(op,[objMode(val)])
    throwKeyedMsgSP("S2IB0006", NIL, t)
  throwKeyedMsgSP("S2IB0006", NIL, op)

bottomUpFormRetract(t,op,opName,argl,amsl) ==
  -- tries to find one argument, which can be pulled back, and calls
  -- bottomUpForm again. We do not retract the first argument to a
  -- setelt, because this is presumably a destructive operation and
  -- the retract can create a new object.

  -- if no such operation exists in the database, don't bother
  $inRetract: local := true
  null getAllModemapsFromDatabase(getUnname op,#argl) => NIL

  u := bottomUpFormAnyUnionRetract(t,op,opName,argl,amsl) => u

  a  := NIL
  b  := NIL
  ms := NIL
  for x in argl for m in amsl for i in 1.. repeat
    -- do not retract first arg of a setelt
    (i = 1) and (opName = "setelt!") =>
        a := [x,:a]
        ms := [m,:ms]
    (i = 1) and (opName = "set!") =>
        a := [x,:a]
        ms := [m,:ms]
    if PAIRP(m) and first(m) = $EmptyMode then return NIL
    object:= retract getValue x
    a:= [x,:a]
    EQ(object,'failed) =>
        putAtree(x,'retracted,nil)
        ms := [m, :ms]
    b:= true
    RPLACA(m,objMode(object))
    ms := [COPY_-TREE m, :ms]
    putAtree(x,'retracted,true)
    putValue(x,object)
    putModeSet(x,[objMode(object)])
  --insert pulled-back items
  a := nreverse a
  ms := nreverse ms

  -- check that we haven't seen these types before
  typesHad := getAtree(t, 'typesHad)
  if member(ms, typesHad) then b := nil
  else putAtree(t, 'typesHad, cons(ms, typesHad))

  b and bottomUpForm(t,op,opName,a,amsl)

retractAtree atr ==
    object:= retract getValue atr
    EQ(object,'failed) =>
        putAtree(atr,'retracted,nil)
        nil
    putAtree(atr,'retracted,true)
    putValue(atr,object)
    putModeSet(atr,[objMode(object)])
    true

bottomUpFormAnyUnionRetract(t,op,opName,argl,amsl) ==
  -- see if we have a Union

  ok := NIL
  for m in amsl while not ok repeat
    if atom first(m) then return NIL
    first m = $Any => ok := true
    (first first m = 'Union) => ok := true
  not ok => NIL

  a:= NIL
  b:= NIL

  for x in argl for m in amsl for i in 0.. repeat
    m0 := first m
    if ( (m0 = $Any) or (first m0 = 'Union) ) and
      ('failed~=(object:=retract getValue x)) then
        b := true
        RPLACA(m,objMode(object))
        putModeSet(x,[objMode(object)])
        putValue(x,object)
    a := cons(x,a)
  b and bottomUpForm(t,op,opName,nreverse a,amsl)

bottomUpFormUntaggedUnionRetract(t,op,opName,argl,amsl) ==
  -- see if we have a Union with no tags, if so retract all such guys

  ok := NIL
  for [m] in amsl while not ok repeat
    if atom m then return NIL
    if m is ['Union, :.] and null getUnionOrRecordTags m then ok := true
  not ok => NIL

  a:= NIL
  b:= NIL

  for x in argl for m in amsl for i in 0.. repeat
    m0 := first m
    if (m0 is ['Union, :.] and null getUnionOrRecordTags m0) and
      ('failed ~= (object:=retract getValue x)) then
        b := true
        RPLACA(m,objMode(object))
        putModeSet(x,[objMode(object)])
        putValue(x,object)
    a := cons(x,a)
  b and bottomUpForm(t,op,opName,nreverse a,amsl)

bottomUpElt (form:=[op,:argl]) ==
  -- this transfers expressions that look like function calls into
  -- forms with elt or apply.

    ms := bottomUp op
    ms and (ms is [['Union,:.]] or ms is [['Record,:.]]) =>
        rplac(rest form, [op, :argl])
        rplac(first form, mkAtreeNode "elt")
        bottomUp form

    target  := getTarget form

    newOps := [mkAtreeNode "elt", mkAtreeNode "apply"]
    u := nil

    while not u for newOp in newOps repeat
        newArgs := [op,:argl]
        if selectMms(newOp, newArgs, target) then
            rplac(rest form, newArgs)
            rplac(first form, newOp)
            u := bottomUp form

    while not u and ( "and"/[retractAtree(a) for a in newArgs] ) repeat
        while not u for newOp in newOps repeat
            newArgs := [op,:argl]
            if selectMms(newOp, newArgs, target) then
                rplac(rest form, newArgs)
                rplac(first form, newOp)
                u := bottomUp form
    u

isEltable(op,argl,numArgs) ==
  -- determines if the object might possible have an elt function
  -- we exclude Mapping and Variable types explicitly
  v := getValue op =>
    ZEROP numArgs => true
    not(m := objMode(v)) => nil
    m is ['Mapping, :.] => nil
    objVal(v) is ['SPADMAP, :mapDef] and numMapArgs(mapDef) > 0 => nil
    true
  m := getMode op =>
    ZEROP numArgs => true
    m is ['Mapping, :.] => nil
    true
  numArgs ~= 1 => nil
  name := getUnname op
  name = 'SEQ => nil
--not (name in '(a e h s)) and getAllModemapsFromDatabase(name, nil) => nil
  arg := first argl
  (getUnname arg) ~= 'construct => nil
  true
