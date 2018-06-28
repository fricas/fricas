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

--% User Function Creation and Analysis Code

DEFPARAMETER($mapTarget, nil)
DEFPARAMETER($mapReturnTypes, nil)
DEFPARAMETER($mapName, 'noMapName)
DEFPARAMETER($mapThrowCount, 0) -- times a "return" occurs in map
DEFPARAMETER($compilingMap, NIL)
DEFPARAMETER($definingMap, NIL)

--% Generating internal names for functions

makeInternalMapName(userName,numArgs,numMms,extraPart) ==
  name := CONCAT('"*",STRINGIMAGE numArgs,'";",
    object2String userName,'";",STRINGIMAGE numMms,'";",
      object2String frameName first $interpreterFrameRing )
  if extraPart then name := CONCAT(name,'";",extraPart)
  INTERN name

isInternalMapName name ==
  -- this only returns true or false as a "best guess"
  (not IDENTP(name)) or (name = "*") or (name = "**") => false
  sz := SIZE (name' := PNAME name)
  (sz < 7) or (char("*") ~= name'.0) => false
  null DIGITP name'.1 => false
  null STRPOS('"_;",name',1,NIL) => false
  -- good enough
  true

makeInternalMapMinivectorName(name) ==
  STRINGP name =>
    INTERN STRCONC(name,'";MV")
  INTERN STRCONC(PNAME name,'";MV")

mkCacheName(name) == INTERNL(STRINGIMAGE name,'";AL")

mkAuxiliaryName(name) == INTERNL(STRINGIMAGE name,'";AUX")

--% Adding a function definition

isMapExpr x == x is ['SPADMAP, :.]

isMap x ==
  y := get(x,'value,$InteractiveFrame) =>
    objVal y is ['SPADMAP, :.] => x

addDefMap(['DEF,lhs,mapsig,.,rhs],pred) ==
  -- Create a new map, add to an existing one, or define a variable
  --   compute the dependencies for a map

  -- next check is for bad forms on the lhs of the ==, such as
  -- numbers, constants.
  if not PAIRP lhs then
    op := lhs
    putHist(op,'isInterpreterRule,true,$e)
    putHist(op,'isInterpreterFunction,false,$e)
    lhs := [lhs]
  else
    -- this is a function definition. If it has been declared
    -- previously, make sure it is Mapping.
    op := first lhs
    (oldMode := get(op,'mode,$e)) and oldMode isnt ['Mapping,:.] =>
      throwKeyedMsg("S2IM0001",[op,oldMode])
    putHist(op,'isInterpreterRule,false,$e)
    putHist(op,'isInterpreterFunction,true,$e)

  (NUMBERP(op) or op in '(true false nil % %%)) =>
    throwKeyedMsg("S2IM0002",[lhs])

  -- verify a constructor abbreviation is not used on the lhs
  op ~= (op' := unabbrev op) => throwKeyedMsg("S2IM0003",[op,op'])

  -- get the formal parameters. These should only be atomic symbols
  -- that are not numbers.
  parameters := [p for p in rest lhs | IDENTP(p)]

  -- see if a signature has been given. if anything in mapsig is NIL,
  -- then declaration was omitted.
  someDecs := nil
  allDecs := true
  mapmode := ['Mapping]
  $env:local := [[NIL]]
  $genValue:local := true       --evaluate all generated code
  for d in mapsig repeat
    if d then
      someDecs := true
      d' := evaluateType unabbrev d
      isPartialMode d' => throwKeyedMsg("S2IM0004",NIL)
--      tree := mkAtree d'
--      null (d' := isType tree) => throwKeyedMsg("S2IM0005",[d])
      mapmode := [d',:mapmode]
    else allDecs := false
  if allDecs then
    mapmode := nreverse mapmode
    putHist(op,'mode,mapmode,$e)
    sayKeyedMsg("S2IM0006",[formatOpSignature(op,rest mapmode)])
  else if someDecs then throwKeyedMsg("S2IM0007",[op])

  -- if map is declared, check that signature arg count is the
  -- same as what is given.
  if get(op,'mode,$e) is ['Mapping,.,:mapargs] then
    EQCAR(rhs,'rules) =>
      0 ~= (numargs := # rest lhs) =>
        throwKeyedMsg("S2IM0027",[numargs,op])
    # rest lhs ~= # mapargs => throwKeyedMsg("S2IM0008",[op])
  --get all the user variables in the map definition.  This is a multi
  --step process as this should not include recursive calls to the map
  --itself, or the formal parameters
  userVariables1 := getUserIdentifiersIn rhs
  $freeVars: local := NIL
  $localVars: local := NIL
  for parm in parameters repeat mkLocalVar($mapName,parm)
  userVariables2 := setDifference(userVariables1,findLocalVars(op,rhs))
  userVariables3 := setDifference(userVariables2, parameters)
  userVariables4 := REMDUP setDifference (userVariables3, [op])

  --figure out the new dependencies for the new map (what it depends on)
  newDependencies := makeNewDependencies (op, userVariables4)
  putDependencies (op, newDependencies)
  clearDependencies(op)
  addMap(lhs,rhs,pred)

addMap(lhs,rhs,pred) ==
  [op,:argl] := lhs
  $sl: local:= nil
  formalArgList:= [mkFormalArg(makeArgumentIntoNumber x,s)
    for x in argl for s in $FormalMapVariableList]
  argList:=
    [fn for x in formalArgList] where
      fn ==
        if x is ["SUCHTHAT",s,p] then (predList:= [p,:predList]; x:= s)
        x
  mkMapAlias(op,argl)
  argPredList:= NREVERSE predList
  finalPred :=
-- handle g(a,T)==a+T confusion between pred=T and T variable
    MKPF((pred and (pred ~= 'T) => [:argPredList,SUBLISNQ($sl,pred)]; argPredList),"and")
  body:= SUBLISNQ($sl,rhs)
  oldMap :=
    (obj := get(op,'value,$InteractiveFrame)) => objVal obj
    NIL
  newMap := augmentMap(op,argList,finalPred,body,oldMap)
  null newMap =>
    sayRemoveFunctionOrValue op
    putHist(op,'alias,nil,$e)
    ""      -- clears value--- see return from addDefMap in tree2Atree1
  if get(op,'isInterpreterRule,$e) then type := ['RuleCalled,op]
  else type := ['FunctionCalled,op]
  recursive :=
    depthOfRecursion(op,newMap) = 0 => false
    true
  putHist(op,'recursive,recursive,$e)
  objNew(newMap,type)

augmentMap(op,args,pred,body,oldMap) ==
  pattern:= makePattern(args,pred)
  newMap:=deleteMap(op,pattern,oldMap)
  body="" =>
    if newMap=oldMap then
      sayMSG ['"   Cannot find part of",:bright op,'"to delete."]
    newMap  --just delete rule if body is 
  entry:= [pattern,:body]
  resultMap:=
    newMap is ["SPADMAP", :tail] => ["SPADMAP", :tail, entry]
    ["SPADMAP", entry]
  resultMap

deleteMap(op,pattern,map) ==
  map is ["SPADMAP", :tail] =>
    newMap := ['SPADMAP, :[x for x in tail | w]] where w ==
      x is [=pattern,:replacement] => sayDroppingFunctions(op,[x])
      true
    null rest newMap => nil
    newMap
  NIL

getUserIdentifiersIn body ==
  null body => nil
  IDENTP body =>
    isSharpVarWithNum body => nil
    body="" => nil
    [body]
  body is ["WRAPPED",:.] => nil
  (body is ["COLLECT",:itl,body1]) or (body is ['REPEAT,:itl,body1]) =>
    userIds :=
      set_sum(getUserIdentifiersInIterators itl, getUserIdentifiersIn body1)
    set_difference(userIds, getIteratorIds itl)
  body is [op,:l] =>
    argIdList:= "append"/[getUserIdentifiersIn y for y in l]
    bodyIdList :=
      not (GETL(op,'Nud) or GETL(op,'Led) or GETL(op,'up))=>
        NCONC(getUserIdentifiersIn op, argIdList)
      argIdList
    REMDUP bodyIdList

getUserIdentifiersInIterators itl ==
  for x in itl repeat
    x is ["STEP",i,:l] =>
      varList:= [:"append"/[getUserIdentifiersIn y for y in l],:varList]
    x is ["IN",.,y]   => varList:= [:getUserIdentifiersIn y,:varList]
    x is ["ON",.,y]   => varList:= [:getUserIdentifiersIn y,:varList]
    x is [op,a] and op in '(_| WHILE UNTIL) =>
      varList:= [:getUserIdentifiersIn a,:varList]
    keyedSystemError("S2GE0016",['"getUserIdentifiersInIterators",
      '"unknown iterator construct"])
  REMDUP varList

getIteratorIds itl ==
  for x in itl repeat
    x is ["STEP",i,:.] => varList:= [i,:varList]
    x is ["IN",y,:.]   => varList:= [y,:varList]
    x is ["ON",y,:.]   => varList:= [y,:varList]
    nil
  varList

makeArgumentIntoNumber x ==
  x=$Zero => 0
  x=$One => 1
  atom x => x
  x is ["-",n] and NUMBERP n => -n
  [removeZeroOne first x,:removeZeroOne rest x]

mkMapAlias(op,argl) ==
  u:= mkAliasList argl
  newAlias :=
    alias:= get(op,"alias",$e) => [(y => y; x) for x in alias for y in u]
    u
  $e:= putHist(op,"alias",newAlias,$e)

mkAliasList l == fn(l,nil) where fn(l,acc) ==
  null l => NREVERSE acc
  not IDENTP first l or first l in acc => fn(rest l,[nil,:acc])
  fn(rest l,[first l,:acc])

args2Tuple args ==
  args is [first,:rest] =>
    null rest => first
    ["Tuple",:args]
  nil

makePattern(args,pred) ==
  nargs:= #args
  nargs = 1 =>
    pred is ["=","#1",n] => n
    addPatternPred("#1",pred)
  u:= canMakeTuple(nargs,pred) => u
  addPatternPred(["Tuple",:TAKE(nargs,$FormalMapVariableList)],pred)

addPatternPred(arg,pred) ==
  pred=true => arg
  ["|",arg,pred]

canMakeTuple(nargs,pred) ==
  pred is ["and",:l] and nargs=#l and
    (u:= [(x is ["=",=y,a] => a; return nil)
      for y in $FormalMapVariableList for x in orderList l]) =>
        ["Tuple",:u]

sayRemoveFunctionOrValue x ==
  (obj := getValue x) and (md := objMode obj) =>
    md = $EmptyMode =>
      sayMessage ['"  ",:bright x,'"now has no function parts."]
    sayMessage ['"   value for",:bright x,'"has been removed."]
  sayMessage ['"  ",:bright x,'"has no value so this does nothing."]

sayDroppingFunctions(op,l) ==
  sayKeyedMsg("S2IM0017",[#l,op])
  if $displayDroppedMap then
    for [pattern,:replacement] in l repeat
      displaySingleRule(op,pattern,replacement)
  nil

makeRuleForm(op,pattern)==
  pattern is ["Tuple",:l] => [op,:l]
  [op,:pattern]

mkFormalArg(x,s) ==
  isConstantArgument x => ["SUCHTHAT",s,["=",s,x]]
  isPatternArgument x => ["SUCHTHAT",s,["is",s,x]]
  IDENTP x =>
    y:= LASSOC(x,$sl) => ["SUCHTHAT",s,["=",s,y]]
    $sl:= [[x,:s],:$sl]
    s
  ['SUCHTHAT,s,["=",s,x]]

isConstantArgument x ==
  NUMBERP x => x
  x is ["QUOTE",.] => x

isPatternArgument x == x is ["construct",:.]

--% Map dependencies

makeNewDependencies (op, userVariables) ==
  null userVariables => nil
  --add the new dependencies
  [[(first userVariables),op],
    :makeNewDependencies (op, rest userVariables)]

putDependencies (op, dependencies) ==
  oldDependencies := getFlag "$dependencies"
  --remove the obsolete dependencies:  all those that applied to the
  --old definition, but may not apply here.  If they do, they'll be
  --in the list of new dependencies anyway
  oldDependencies := removeObsoleteDependencies (op, oldDependencies) where
    removeObsoleteDependencies (op, oldDep) ==
      null oldDep => nil
      op = rest first oldDep =>
        removeObsoleteDependencies (op, rest oldDep)
      [first oldDep,:removeObsoleteDependencies (op, rest oldDep)]
  --Create the list of dependencies to output.  This will be all the
  --old dependencies that are still applicable, and all the new ones
  --that have just been generated.  Remember that the list of
  --dependencies does not just include those for the map just being
  --defined, but includes those for all maps and variables that exist
  newDependencies := union (dependencies, oldDependencies)
  putFlag ("$dependencies", newDependencies)

clearDependencies(x) ==
    clearDep1(x, [], [], COPY getFlag "$dependencies")

clearDep1(x,toDoList,doneList,depList) ==
  x in doneList => nil
  clearCache x
  newDone:= [x,:doneList]
  until null a repeat
    a:= ASSQ(x,depList)
    a =>
      depList:= delete(a,depList)
      toDoList:= union(toDoList,
        setDifference(rest a, doneList))
  toDoList is [a,:res] => clearDep1(a,res,newDone,depList)
  'done

--% Formatting and displaying maps

displayRule(op,rule) ==
  null rule => nil
  mathprint ["CONCAT", "Definition:   ", outputMapTran(op, rule)]
  nil

outputFormat(x,m) ==
  -- this is largely junk and is being phased out
  IDENTP m => x
  m=$OutputForm or m=$EmptyMode => x
  categoryForm?(m) => x
  isMapExpr x => x
  containsVars x => x
  atom(x) and first(m) = 'List => x
  (x is ['construct,:.]) and m = '(List (Expression)) => x
  T:= coerceInteractive(objNewWrap(x,maximalSuperType(m)),
    $OutputForm) or return x
  objValUnwrap T

displaySingleRule(op, pattern, replacement) ==
  mathprint outputMapTran(op, ['SPADMAP, [pattern, :replacement]])

simplifyMapPattern (x,alias) ==
  for a in alias
    for m in $FormalMapVariableList | a and not CONTAINED(a,x) repeat
      x:= substitute(a,m,x)
  [lhs,:rhs]:= x
  rhs := simplifyMapConstructorRefs rhs
  x := [lhs,:rhs]
  lhs is ["|",y,pred] =>
    pred:= predTran pred
    sl:= getEqualSublis pred =>
      y':= SUBLIS(sl,y)
      pred:= unTrivialize SUBLIS(sl,pred) where unTrivialize x ==
        x is [op,:l] and op in '(_and _or) =>
          MKPF([unTrivialize y for y in l],op)
        x is [op,a,=a] and op in '(_= is)=> true
        x
      rhs':= SUBLIS(sl,rhs)
      pred=true => [y',:rhs']
      [["PAREN",["|",y',pred]],:rhs']
    pred=true => [y,:rhs]
    [["PAREN",["|",y,pred]],:rhs]
  lhs=true => ["true",:rhs]
  x

simplifyMapConstructorRefs form ==
  -- try to linear format constructor names
  ATOM form => form
  [op,:args] := form
  op in '(exit SEQ) =>
    [op,:[simplifyMapConstructorRefs a for a in args]]
  op in '(REPEAT) =>
    [op,first args,:[simplifyMapConstructorRefs a for a in rest args]]
  op in '(_: _:_: _@) =>
    args is [obj,dom] =>
      dom' := prefix2String dom
      --if ATOM dom' then dom' := [dom']
      --[op,obj,APPLY('CONCAT,dom')]
      dom'' :=
          ATOM dom' => dom'
          NULL rest dom' => first dom'
          APPLY('CONCAT, dom')
      [op,obj, dom'']
    form
  form

predTran x ==
  x is ["IF",a,b,c] =>
    c = "false" => MKPF([predTran a,predTran b],"and")
    b = "true" => MKPF([predTran a,predTran c],"or")
    b = "false" and c = "true" => ["not",predTran a]
    x
  x

getEqualSublis pred == fn(pred,nil) where fn(x,sl) ==
  (x:= SUBLIS(sl,x)) is [op,:l] and op in '(_and _or) =>
    for y in l repeat sl:= fn(y,sl)
    sl
  x is ["is",a,b] => [[a,:b],:sl]
  x is ["=",a,b] =>
    IDENTP a and not CONTAINED(a,b) => [[a,:b],:sl]
    IDENTP b and not CONTAINED(b,a) => [[b,:a],:sl]
    sl
  sl

--% User function analysis

mapCatchName mapname ==
   INTERN STRCONC('"$",STRINGIMAGE mapname,'"CatchMapIdentifier$")

analyzeMap(op,argTypes,mapDef, tar) ==
  -- Top level enty point for map type analysis.  Sets up catch point
  --  for interpret-code mode.
  $compilingMap:local := true
  $definingMap:local := true
  $minivector     : local := nil   -- later becomes value of $minivectorName
  $mapThrowCount  : local := 0     -- number of "return"s encountered
  $mapReturnTypes : local := nil   -- list of types from returns
  $repeatLabel    : local := nil   -- for loops; see upREPEAT
  $breakCount     : local := 0     -- breaks from loops; ditto
  $mapTarget      : local := tar
  $interpOnly: local := NIL
  $mapName : local := op.0
  if get($mapName,'recursive,$e) then
    argTypes := [f t for t in argTypes] where
      f x ==
        isEqualOrSubDomain(x,$Integer) => $Integer
        x
  mapAndArgTypes := [$mapName,:argTypes]
  member(mapAndArgTypes,$analyzingMapList) =>
    -- if the map is declared, return the target type
    (getMode op) is ['Mapping,target,:.] => target
    throwKeyedMsg("S2IM0009",
      [$mapName,['" ", map for [map,:.] in $analyzingMapList]])
  PUSH(mapAndArgTypes,$analyzingMapList)
  mapDef := mapDefsWithCorrectArgCount(#argTypes, mapDef)
  null mapDef => (POP $analyzingMapList; nil)

  UNWIND_-PROTECT(x:=CATCH('mapCompiler,analyzeMap0(op,argTypes,mapDef)),
    POP $analyzingMapList)
  x='tryInterpOnly =>
    opName:=getUnname op
    fun := mkInterpFun(op,opName,argTypes)
    if getMode op isnt ['Mapping,:sig] then
      sig := [nil,:[nil for type in argTypes]]
    $e:=putHist(opName,'localModemap,
      [[['interpOnly,:sig],fun,NIL]],$e)
  x

analyzeMap0(op,argTypes,mapDef) ==
  -- Type analyze and compile a map.  Returns the target type of the map.
  --  only called if there is no applicable compiled map
  $MapArgumentTypeList:local:= argTypes
  numMapArgs mapDef ~= #argTypes => nil
  ((m:=getMode op) is ['Mapping,:sig]) or (m and (sig:=[m])) =>
    -- op has mapping property only if user has declared the signature
    analyzeDeclaredMap(op,argTypes,sig,mapDef,$mapList)
  analyzeUndeclaredMap(getUnname op,argTypes,mapDef,$mapList)

compFailure msg ==
  -- Called when compilation fails in such a way that interpret-code
  --  mode might be of some use.
  not $useCoerceOrCroak =>    THROW('coerceOrCroaker, 'croaked)
  if $reportInterpOnly then
    sayMSG msg
    sayMSG '"   We will attempt to interpret the code."
  null $compilingMap => THROW('loopCompiler,'tryInterpOnly)
  THROW('mapCompiler,'tryInterpOnly)

mkInterpFun(op,opName,argTypes) ==
  -- creates a function form to put in fun slot of interp-only
  -- local modemaps
  getMode op isnt ['Mapping,:sig] => nil
  parms := [var for type in argTypes for var in $FormalMapVariableList]
  arglCode := ['LIST,:[argCode for type in argTypes
    for argName in parms]] where argCode ==
      ['putValueValue,['mkAtreeNode,MKQ argName],
        objNewCode(['wrap,argName],type)]
  funName := GENSYM()
  body:=['rewriteMap1,MKQ opName,arglCode,MKQ sig]
  putMapCode(opName,body,sig,funName,parms,false)
  genMapCode(opName,body,sig,funName,parms,false)
  funName

rewriteMap(op,opName,argl) ==
  -- interpret-code handler for maps.  Recursively calls the interpreter
  --   on the body of the map.
  not $genValue =>
    get(opName,'mode,$e) isnt ['Mapping,:sig] =>
      compFailure  ['"   Cannot compile map:",:bright opName]
    arglCode := ['LIST,:[argCode for arg in argl for argName in
      $FormalMapVariableList]] where argCode ==
        atype := OR(getMode arg, IFCAR(getModeSet arg))
        ['putValueValue,['mkAtreeNode,MKQ argName],
          objNewCode(['wrap,wrapped2Quote(objVal getValue arg)],
                      atype)]
    putValue(op,objNew(['rewriteMap1,MKQ opName,arglCode,MKQ sig],
      first sig))
    putModeSet(op, [first sig])
  rewriteMap0(op,opName,argl)

putBodyInEnv(opName, numArgs) ==
  val := get(opName, 'value, $e)
  val is [., 'SPADMAP, :bod] =>
    $e := putHist(opName, 'mapBody, combineMapParts
      mapDefsWithCorrectArgCount(numArgs, bod), $e)
  'failed

removeBodyFromEnv(opName) ==
  $e := putHist(opName, 'mapBody, nil, $e)


rewriteMap0(op,opName,argl) ==
  -- $genValue case of map rewriting
  putBodyInEnv(opName, #argl)
  if (s := get(opName,'mode,$e)) then
    tar := CADR s
    argTypes := CDDR s
  else
    tar:= nil
    argTypes:= nil
  get(opName,'mode,$e) is ['Mapping,tar,:argTypes]
  $env: local := [[NIL]]
  for arg in argl
    for var in $FormalMapVariableList repeat
      if argTypes then
        t := first argTypes
        argTypes := rest argTypes
        val :=
          t is ['Mapping,:.] => getValue arg
          coerceInteractive(getValue arg,t)
      else
        val:= getValue arg
      $env:=put(var,'value,val,$env)
      if VECP arg then $env := put(var,'name,getUnname arg,$env)
      (m := getMode arg) => $env := put(var,'mode,m,$env)
  null (val:= interpMap(opName,tar)) =>
    throwKeyedMsg("S2IM0010",[opName])
  putValue(op,val)
  removeBodyFromEnv(opName)
  ms := putModeSet(op,[objMode val])

rewriteMap1(opName,argl,sig) ==
  -- compiled case of map rewriting
  putBodyInEnv(opName, #argl)
  if sig then
      tar := first sig
      argTypes := rest sig
  else
    tar:= nil
    argTypes:= nil
  evArgl := NIL
  for arg in reverse argl repeat
    v := getValue arg
    evArgl := [objNew(objVal v, objMode v),:evArgl]
  $env : local := [[NIL]]
  for arg in argl for evArg in evArgl
    for var in $FormalMapVariableList repeat
      if argTypes then
        t := first argTypes
        argTypes := rest argTypes
        val :=
          t is ['Mapping,:.] => evArg
          coerceInteractive(evArg,t)
      else
        val:= evArg
      $env:=put(var,'value,val,$env)
      if VECP arg then $env := put(var,'name,getUnname arg,$env)
      (m := getMode arg) => $env := put(var,'mode,m,$env)
  val:= interpMap(opName,tar)
  removeBodyFromEnv(opName)
  objValUnwrap(val)

interpMap(opName,tar) ==
  -- call the interpreter recursively on map body
  $genValue : local:= true
  $interpMapTag : local := nil
  $interpOnly : local := true
  $localVars : local := NIL
  for lvar in get(opName,'localVars,$e) repeat mkLocalVar(opName,lvar)
  $mapName : local := opName
  $mapTarget : local := tar
  body:= get(opName,'mapBody,$e)
  savedTimerStack := COPY $timedNameStack
  catchName := mapCatchName $mapName
  c := CATCH(catchName, interpret1(body,tar,nil))
--  $interpMapTag and $interpMapTag ~= mapCatchName $mapName =>
--    THROW($interpMapTag,c)
  while savedTimerStack ~= $timedNameStack repeat
    stopTimingProcess peekTimedName()
  c  -- better be a triple

analyzeDeclaredMap(op,argTypes,sig,mapDef,$mapList) ==
  -- analyzes and compiles maps with declared signatures.  argTypes
  -- is a list of types of the arguments, sig is the declared signature
  -- mapDef is the stored form of the map body.
  opName := getUnname op
  $mapList:=[opName,:$mapList]
  $mapTarget := first sig
  (mmS:= get(opName,'localModemap,$e)) and
    (mm:= or/[mm for (mm:=[[.,:mmSig],:.]) in mmS | mmSig=sig]) =>
      compileCoerceMap(opName,argTypes,mm)
  -- The declared map needs to be compiled
  compileDeclaredMap(opName,sig,mapDef)
  argTypes ~= rest sig =>
    analyzeDeclaredMap(op,argTypes,sig,mapDef,$mapList)
  first sig

compileDeclaredMap(op,sig,mapDef) ==
  -- Type analyzes and compiles a map with a declared signature.
  -- creates a local modemap and puts it into the environment
  $localVars: local := nil
  $freeVars: local := nil
  $env:local:= [[NIL]]
  parms := [var for var in $FormalMapVariableList for m in rest sig]
  for m in rest sig for var in parms repeat
    $env:= put(var,'mode,m,$env)
  body:= getMapBody(op,mapDef)
  for lvar in parms repeat mkLocalVar($mapName,lvar)
  for lvar in getLocalVars(op,body) repeat mkLocalVar($mapName,lvar)
  name := makeLocalModemap(op,sig)
  val  := compileBody(body, first sig)
  isRecursive := (depthOfRecursion(op,body) > 0)
  putMapCode(op,objVal val,sig,name,parms,isRecursive)
  genMapCode(op,objVal val,sig,name,parms,isRecursive)
  first sig

putMapCode(op,code,sig,name,parms,isRecursive) ==
  -- saves the generated code and some other information about the
  -- function
  codeInfo := VECTOR(op,code,sig,name,parms,isRecursive)
  allCode := [codeInfo,:get(op,'generatedCode,$e)]
  $e := putHist(op,'generatedCode,allCode,$e)
  op

makeLocalModemap(op,sig) ==
  -- create a local modemap for op with sig, and put it into $e
  if (currentMms := get(op,'localModemap,$e)) then
    untraceMapSubNames [CADAR currentMms]
  newName := makeInternalMapName(op,#sig-1,1+#currentMms,NIL)
  newMm := [['local,:sig],newName,nil]
  mms := [newMm,:currentMms]
  $e := putHist(op,'localModemap,mms,$e)
  newName

genMapCode(op,body,sig,fnName,parms,isRecursive) ==
  -- calls the lisp compiler on the body of a map
  if lmm:= get(op,'localModemap,$InteractiveFrame) then
    untraceMapSubNames [CADAR lmm]
  op0 :=
    (n := isSharpVarWithNum op) =>
        STRCONC('"<argument ",object2String n,'">")
    op
  if get(op,'isInterpreterRule,$e) then
    sayKeyedMsg("S2IM0014", [op0, (PAIRP sig => prefix2String first sig;
                                   '"?")])
  else sayKeyedMsg("S2IM0015",[op0,formatSignature sig])
  $whereCacheList := [op,:$whereCacheList]

  -- RSS: 6-21-94
  -- The following code ensures that local variables really are local
  -- to a function. We will unnecessarily generate preliminary LETs for
  -- loop variables and variables that do have LET expressions, but that
  -- can be finessed later.

  locals := SETDIFFERENCE(COPY $localVars, parms)
  if locals then
    lets := [['LET, l, ''UNINITIALIZED__VARIABLE, op] for l in locals]
    body := ['PROGN, :lets, body]

  reportFunctionCompilation(op,fnName,parms,
    wrapMapBodyWithCatch flattenCOND body,isRecursive)

compileBody(body,target) ==
  -- recursively calls the interpreter on the map body
  --  returns a triple with the LISP code for body in the value cell
  $insideCompileBodyIfTrue: local := true
  $genValue: local := false
  $declaredMode:local := target
  r := interpret1(body,target,nil)

compileCoerceMap(op,argTypes,mm) ==
  -- compiles call to user-declared map where the arguments need
  --  to be coerced. mm is the modemap for the declared map.
  $insideCompileBodyIfTrue: local := true
  $genValue: local := false
  [[.,:sig],imp,.]:= mm
  parms := [var for var in $FormalMapVariableList for t in rest sig]
  name := makeLocalModemap(op, [first sig, :argTypes])
  argCode := [objVal(coerceInteractive(objNew(arg,t1),t2) or
    throwKeyedMsg("S2IC0001",[arg,$mapName,t1,t2]))
      for t1 in argTypes for t2 in rest sig for arg in parms]
  $insideCompileBodyIfTrue := false
  parms:= [:parms,'envArg]
  body := ['SPADCALL,:argCode,['LIST,['function,imp]]]
  minivectorName := makeInternalMapMinivectorName(name)
  body := SUBST(minivectorName,"$$$",body)
  SET(minivectorName,LIST2REFVEC $minivector)
  compileInteractive [name,['LAMBDA,parms,body]]
  first sig

depthOfRecursion(opName,body) ==
  -- returns the "depth" of recursive calls of opName in body
  mapRecurDepth(opName, [nil], body)

mapRecurDepth(opName,opList,body) ==
  -- walks over the map body counting depth of recursive calls
  --  expanding the bodies of maps called in body
  atom body => 0
  body is [op,:argl] =>
    argc:=
      atom argl => 0
      argl => "MAX"/[mapRecurDepth(opName,opList,x) for x in argl]
      0
    op in first(opList) => argc
    op=opName => 1 + argc
    (obj := get(op, 'value, $e)) and objVal obj is ['SPADMAP, :mapDef] =>
      opList.0 := [op, :first(opList)]
      mapRecurDepth(opName, opList, getMapBody(op, mapDef))
        + argc
    argc
  keyedSystemError("S2GE0016",['"mapRecurDepth",
    '"unknown function form"])

analyzeUndeclaredMap(op,argTypes,mapDef,$mapList) ==
  -- Computes the signature of the map named op, and compiles the body
  $freeVars:local := NIL
  $localVars: local := NIL
  $env:local:= [[NIL]]
  $mapList := [op,:$mapList]
  parms:=[var for var in $FormalMapVariableList for m in argTypes]
  for m in argTypes for var in parms repeat
    put(var,'autoDeclare,'T,$env)
    put(var,'mode,m,$env)
  body:= getMapBody(op,mapDef)
  for lvar in parms repeat mkLocalVar($mapName,lvar)
  for lvar in getLocalVars(op,body) repeat mkLocalVar($mapName,lvar)
  (n:= depthOfRecursion(op,body)) = 0 =>
    analyzeNonRecursiveMap(op,argTypes,body,parms)
  analyzeRecursiveMap(op,argTypes,body,parms,n)

analyzeNonRecursiveMap(op,argTypes,body,parms) ==
  -- analyze and compile a non-recursive map definition
  T := compileBody(body,$mapTarget)
  if $mapThrowCount > 0 then
    t := objMode T
    b := and/[(t = rt) for rt in $mapReturnTypes]
    not b =>
      t := resolveTypeListAny [t,:$mapReturnTypes]
      if not $mapTarget then $mapTarget := t
      T := compileBody(body,$mapTarget)
  sig := [objMode T,:argTypes]
  name:= makeLocalModemap(op,sig)
  putMapCode(op,objVal T,sig,name,parms,false)
  genMapCode(op,objVal T,sig,name,parms,false)
  objMode(T)

analyzeRecursiveMap(op,argTypes,body,parms,n) ==
  -- analyze and compile a non-recursive map definition
  --  makes guess at signature by analyzing non-recursive part of body
  --  then re-analyzes the entire body until the signature doesn't change
  localMapInfo := saveDependentMapInfo(op, rest $mapList)
  tar := CATCH('interpreter,analyzeNonRecur(op,body,$localVars))
  for i in 0..n until not sigChanged repeat
    sigChanged:= false
    name := makeLocalModemap(op,sig:=[tar,:argTypes])
    code := compileBody(body,$mapTarget)
    objMode(code) ~= tar =>
      sigChanged:= true
      tar := objMode(code)
      restoreDependentMapInfo(op, rest $mapList, localMapInfo)
  sigChanged => throwKeyedMsg("S2IM0011",[op])
  putMapCode(op,objVal code,sig,name,parms,true)
  genMapCode(op,objVal code,sig,name,parms,true)
  tar

saveDependentMapInfo(op,opList) ==
  not (op in opList) =>
    lmml := [[op, :get(op, 'localModemap, $e)]]
    gcl := [[op, :get(op, 'generatedCode, $e)]]
    for [dep1,dep2] in getFlag("$dependencies") | dep1=op repeat
      [lmml', :gcl'] := saveDependentMapInfo(dep2, [op, :opList])
      lmms := nconc(lmml', lmml)
      gcl := nconc(gcl', gcl)
    [lmms, :gcl]
  nil

restoreDependentMapInfo(op, opList, [lmml,:gcl]) ==
  not (op in opList) =>
    clearDependentMaps(op,opList)
    for [op, :lmm] in lmml repeat
      $e := putHist(op,'localModemap,lmm,$e)
    for [op, :gc] in gcl repeat
      $e := putHist(op,'generatedCode,gc,$e)

clearDependentMaps(op,opList) ==
  -- clears the local modemaps of all the maps that depend on op
  not (op in opList) =>
    $e := putHist(op,'localModemap,nil,$e)
    $e := putHist(op,'generatedCode,nil,$e)
    for [dep1,dep2] in getFlag("$dependencies") | dep1=op repeat
      clearDependentMaps(dep2,[op,:opList])

analyzeNonRecur(op,body,$localVars) ==
  -- type analyze the non-recursive part of a map body
  nrp := nonRecursivePart(op,body)
  for lvar in findLocalVars(op,nrp) repeat mkLocalVar($mapName,lvar)
  objMode(compileBody(nrp,$mapTarget))

nonRecursivePart(opName, funBody) ==
  -- takes funBody, which is the parse tree of the definition of
  --  a function, and returns a list of the parts
  --  of the function which are not recursive in the name opName
  body:= expandRecursiveBody([opName], funBody)
  ((nrp:=nonRecursivePart1(opName, body)) ~= 'noMapVal) => nrp
  throwKeyedMsg("S2IM0012",[opName])

expandRecursiveBody(alreadyExpanded, body) ==
  -- replaces calls to other maps with their bodies
  atom body =>
    (obj := get(body, 'value, $e)) and objVal obj is ['SPADMAP, :mapDef] and
      ((numMapArgs mapDef) = 0) => getMapBody(body,mapDef)
    body
  body is [op,:argl] =>
    not (op in alreadyExpanded) =>
      (obj := get(op, 'value, $e)) and objVal obj is ['SPADMAP, :mapDef] =>
        newBody:= getMapBody(op,mapDef)
        for arg in argl for var in $FormalMapVariableList repeat
            newBody := substitute(arg, var, newBody)
        expandRecursiveBody([op,:alreadyExpanded],newBody)
      [op,:[expandRecursiveBody(alreadyExpanded,arg) for arg in argl]]
    [op,:[expandRecursiveBody(alreadyExpanded,arg) for arg in argl]]
  keyedSystemError("S2GE0016",['"expandRecursiveBody",
    '"unknown form of function body"])

nonRecursivePart1(opName, funBody) ==
  -- returns a function body which contains only the parts of funBody
  --  which do not call the function opName
  funBody is ['IF,a,b,c] =>
    nra:=nonRecursivePart1(opName,a)
    nra = 'noMapVal => 'noMapVal
    nrb:=nonRecursivePart1(opName,b)
    nrc:=nonRecursivePart1(opName,c)
    not (nrb in '(noMapVal noBranch)) => ['IF,nra,nrb,nrc]
    not (nrc in '(noMapVal noBranch)) => ['IF,['not,nra],nrc,nrb]
    'noMapVal
  not containsOp(funBody,'IF) =>
    notCalled(opName,funBody) => funBody
    'noMapVal
  funBody is [op,:argl] =>
    op=opName => 'noMapVal
    args:= [nonRecursivePart1(opName,arg) for arg in argl]
    MEMQ('noMapVal,args) => 'noMapVal
    [op,:args]
  funBody

containsOp(body,op) ==
  -- true IFF body contains an op statement
  body is [ =op,:.] => true
  body is [.,:argl] => or/[containsOp(arg,op) for arg in argl]
  false

notCalled(opName,form) ==
  -- returns true if opName is not called in the form
  atom form => true
  form is [op,:argl] =>
    op=opName => false
    and/[notCalled(opName,x) for x in argl]
  keyedSystemError("S2GE0016",['"notCalled",
    '"unknown form of function body"])

mapDefsWithCorrectArgCount(n, mapDef) ==
  [def for def in mapDef | (numArgs first def) = n]

numMapArgs(mapDef is [[args,:.],:.]) ==
  -- returns the number of arguemnts to the map whose body is mapDef
  numArgs args

numArgs args ==
  args is ['_|,a,:.] => numArgs a
  args is ['Tuple,:argl] => #argl
  null args => 0
  1

combineMapParts(mapTail) ==
  -- transforms a piece-wise function definition into an if-then-else
  --  statement.  Uses noBranch to indicate undefined branch
  null mapTail => 'noMapVal
  mapTail is [[cond,:part],:restMap] =>
    isSharpVarWithNum cond or (cond is ['Tuple,:args] and
      and/[isSharpVarWithNum arg for arg in args]) or (null cond) => part
    ['IF,mkMapPred cond,part,combineMapParts restMap]
  keyedSystemError("S2GE0016",['"combineMapParts",
    '"unknown function form"])

mkMapPred cond ==
  -- create the predicate on map arguments, derived from "when" clauses
  cond is ['_|,args,pred] => mapPredTran pred
  cond is ['Tuple,:vals] =>
    mkValueCheck(vals,1)
  mkValCheck(cond,1)

mkValueCheck(vals,i) ==
  -- creates predicate for specific value check (i.e f 1 == 1)
  vals is [val] => mkValCheck(val,i)
  ['and,mkValCheck(first vals,i),mkValueCheck(rest vals,i+1)]

mkValCheck(val,i) ==
  -- create equality check for map predicates
  isSharpVarWithNum val => 'true
  ['_=,mkSharpVar i,val]

mkSharpVar i ==
  -- create #i
  INTERN CONCAT('"#",STRINGIMAGE i)

mapPredTran pred ==
  -- transforms "x in i..j" to "x>=i and x<=j"
  pred is ["in", var, ['SEGMENT, lb]] => mkLessOrEqual(lb, var)
  pred is ["in", var, ['SEGMENT, lb, ub]] =>
    null ub => mkLessOrEqual(lb,var)
    ['and,mkLessOrEqual(lb,var),mkLessOrEqual(var,ub)]
  pred

findLocalVars(op,form) ==
  -- analyzes form for local and free variables, and returns the list
  --  of locals
  findLocalVars1(op,form)
  $localVars

findLocalVars1(op,form) ==
  -- sets the two lists $localVars and $freeVars
  atom form =>
    not IDENTP form or isSharpVarWithNum form => nil
    isLocalVar(form) or isFreeVar(form) => nil
    mkFreeVar($mapName,form)
  form is ['local, :vars] =>
    for x in vars repeat
      ATOM x => mkLocalVar(op, x)
  form is ['free, :vars] =>
    for x in vars repeat
      ATOM x => mkFreeVar(op, x)
  form is ['LET,a,b] =>
    (a is ['Tuple,:vars]) and (b is ['Tuple,:vals]) =>
      for var in vars for val in vals repeat
        findLocalVars1(op,['LET,var,val])
    a is ['construct,:pat] =>
      for var in listOfVariables pat repeat mkLocalVar(op,var)
      findLocalVars1(op,b)
    (atom a) or (a is ['_:,a,.]) =>
      mkLocalVar(op,a)
      findLocalVars1(op,b)
    findLocalVars(op,b)
    for x in a repeat findLocalVars1(op,x)
  form is ['_:,a,.] =>
    mkLocalVar(op,a)
  form is ['is,l,pattern] =>
    findLocalVars1(op,l)
    for var in listOfVariables rest pattern repeat mkLocalVar(op, var)
  form is [oper,:itrl,body] and MEMQ(oper,'(REPEAT COLLECT)) =>
    findLocalsInLoop(op,itrl,body)
  form is [y,:argl] =>
    y is 'Record => nil
    for x in argl repeat findLocalVars1(op,x)
  keyedSystemError("S2IM0020",[op])

findLocalsInLoop(op,itrl,body) ==
  for it in itrl repeat
    it is ['STEP,index,lower,step,:upperList] =>
      mkLocalVar(op,index)
      findLocalVars1(op,lower)
      for up in upperList repeat findLocalVars1(op,up)
    it is ['IN,index,s] =>
      mkLocalVar(op,index) ; findLocalVars1(op,s)
    it is ['WHILE,b] =>
      findLocalVars1(op,b)
    it is ['_|,pred] =>
      findLocalVars1(op,pred)
  findLocalVars1(op,body)
  for it in itrl repeat
    it is [op,b] and (op in '(UNTIL)) =>
      findLocalVars1(op,b)

isLocalVar(var) == member(var,$localVars)

mkLocalVar(op,var) ==
  -- add var to the local variable list
  isFreeVar(var) => $localVars
  $localVars:= insert(var,$localVars)

isFreeVar(var) == member(var,$freeVars)

mkFreeVar(op,var) ==
  -- op here for symmetry with mkLocalVar
  $freeVars:= insert(var,$freeVars)

listOfVariables pat ==
  -- return a list of the variables in pat, which is an "is" pattern
  IDENTP pat => (pat='_. => nil ; [pat])
  pat is ['_:,var] or pat is ['_=,var] =>
    (var='_. => NIL ; [var])
  PAIRP pat => REMDUP [:listOfVariables p for p in pat]
  nil

getMapBody(op,mapDef) ==
  -- looks in $e for a map body; if not found it computes then stores it
  get(op,'mapBody,$e) or
    combineMapParts mapDef
--    $e:= putHist(op,'mapBody,body:= combineMapParts mapDef,$e)
--    body

getLocalVars(op,body) ==
  -- looks in $e for local vars; if not found, computes then stores them
  get(op,'localVars,$e) or
    $e:= putHist(op,'localVars,lv:=findLocalVars(op,body),$e)
    lv

--  DO NOT BELIEVE ALL OF THE FOLLOWING (IT IS OLD)

--  VARIABLES.  Variables may or may not have a mode property.  If
--  present, any value which is assigned or generated by that variable
--  is first coerced to that mode before being assigned or returned.
--
--
--  Variables are given a triple [val,m,e] as a "value" property on
--  its property list in the environment.  The expression val has the
--  forms:
--
--        (WRAPPED . y)       --value of x is y (don't re-evaluate)
--        y --anything else   --value of x is obtained by evaluating y
--
--  A wrapped expression is created by an assignment.  In the second
--  case, y can never contain embedded wrapped expressions.  The mode
--  part m of the triple is the type of y in the wrapped case and is
--  consistent with the declared mode if given.  The mode part of an
--  unwrapped value is always $EmptyMode.  The e part is usually NIL
--  but may be used to hold a partial closure.
--
--  Effect of changes.  A rule can be built up for a variable by
--  successive rules involving conditional expressions.  However, once
--  a value is assigned to the variable or an unconditional definition
--  is given, any existing value is replaced by the new entry.  When
--  the mode of a variable is declared, an wrapped value is coerced to
--  the new mode; if this is not possible, the user is notified that
--  the current value is discarded and why.  When the mode is
--  redeclared and an upwrapped value is present, the value is
--  retained; the only other effect is to coerce any cached values
--  from the old mode to the new one.
--
--  Caches.  When a variable x is evaluated and re-evaluation occurs,
--  the triple produced by that evaluation is stored under "cache" on
--  the property list of x. This cached triple is cleared whenever any
--  of the variables which x's value depend upon change.  Dependencies
--  are stored on $dependencies whose value has the form [[a b ..] ..]
--  to indicate that when a is changed, b .. must have all cached
--  values destroyed.  In the case of parameterized forms which are
--  represented by maps, we currently can cache values only when the
--  compiler option is turned on by )on c s meaning "on compiler with
--  the save option".  When f is compiled as f;1, it then has an alist
--  f;1;AL which records these values.  If f depends globally on a's
--  value, all cached values of all local functions defined for f have
--  to be declared.  If a's mode should change, then all compilations
--  of f must be thrown away.
--
--  PARAMETERIZED FORMS.  These always have values [val,m,e] where val
--  are "maps".
--
--  The structure of maps:
--   (SPADMAP (pattern . rewrite) ...)   where
--     pattern has forms:  arg-pattern
--                         (Tuple arg-pattern ...)
--     rewrite has forms:  (WRAPPED . value)      --don't re-evaluate
--                         computational object   --don't (bother to)
--                                                  re-evaluate
--                         anything else          --yes, re-evaluate
--
--  When assigning values to a map, each new value must have a type
--  which is consistent with those already assigned.  Initially, type
--  of SPADMAP is $EmptyMode.  When the map is first assigned a value, the
--  type of the SPADMAP is RPLACDed to be (Mapping target source ..).
--  When the map is next assigned, the type of both source and target
--  is upgraded to be consistent with those values already computed.
--  Of course, if new and old source and target are identical, nothing
--  need happen to existing entries.  However, if the new and old are
--  different, all existing entries of the map are coerce to the new
--  data type.
--
--  Mode analysis.  This is done on the bottomUp phase of the process.
--  If a function has been given a mapping declaration, this map is
--  placed in as the mode of the map under the "value" property of the
--  variable.  Of course, these modes may be partial types in case a
--  mode analysis is still necessary.  If no mapping declaration, a
--  total mode analysis of the function, given its input arguments, is
--  done.  This will result a signature involving types only.
--
--  If the compiler is on, the function is then compiled given this
--  signature involving types.  If the map is value of a variable f, a
--  function is given name f;1, f is given a "localModemap" property
--  with modemap ((dummy target source ..) (T f;1)) so that the next
--  time f is applied to arguments which coerce to the source
--  arguments of this local modemap, f;1 will be invoked.
