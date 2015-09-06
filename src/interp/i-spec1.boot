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

)if false
Handlers for Special Forms (1 of 2)

This file contains the functions which do type analysis and
evaluation of special functions in the interpreter.
Special functions are ones which are not defined in the algebra
code, such as assignment, construct, COLLECT and declaration.

Operators which require special handlers all have a LISP "up"
property which is the name of the special handler, which is
always the word "up" followed by the operator name.
If an operator has this "up" property the handler is called
automatically from bottomUp instead of general modemap selection.

The up handlers are usually split into two pieces, the first is
the up function itself, which performs the type analysis, and an
"eval" function, which generates (and executes, if required) the
code for the function.
The up functions always take a single argument, which is the
entire attributed tree for the operation, and return the modeSet
of the node, which is a singleton list containing the type
computed for the node.
The eval functions can take any arguments deemed necessary.
Actual evaluation is done if $genValue is true, otherwise code is
generated.
(See the function analyzeMap for other things that may affect
what is generated in these functions.)

These functions are required to do two things:
  1) do a putValue on the operator vector with the computed value
     of the node, which is a triple.  This is usually done in the
     eval functions.
  2) do a putModeSet on the operator vector with a list of the
     computed type of the node.  This is usually done in the
     up functions.

There are several special modes used in these functions:
  1) Void is the mode that should be used for all statements
     that do not otherwise return values, such as declarations,
     loops, IF-THEN's without ELSE's, etc..
  2) $NoValueMode used to be used in situations
     where Void is now used, and is being phased out completely.
)endif

-- Functions which require special handlers (also see end of file)

DEFPARAMETER($repeatLabel, NIL)
DEFPARAMETER($breakCount, 0)
DEFPARAMETER($anonymousMapCounter, 0)

--% Void stuff

voidValue() == '"()"

--% Handlers for Anonymous Function Definitions

upADEF t ==
  t isnt [.,[vars,types,.,body],pred,.] => NIL
  -- do some checking on what we got
  for var in vars repeat
    if not IDENTP(var) then throwKeyedMsg("S2IS0057",[var])
  -- unabbreviate types
  types := [(if t then evaluateType unabbrev t else NIL) for t in types]
  -- we do not allow partial types
  if isPartialMode(m := first types) then throwKeyedMsg("S2IS0058",[m])

  -- we want everything to be declared or nothing. The exception is that
  -- we do not require a target type since we will compute one anyway.
  if null(m) and rest types then
    m := first rest types
    types' := rest rest types
  else
    types' := rest types
  for type in types' repeat
    if (type and null m) or (m and null type) then
      throwKeyedMsg("S2IS0059",NIL)
    if isPartialMode type  then throwKeyedMsg("S2IS0058",[type])

--  $localVars: local := nil
--  $freeVars:  local := nil
--  $env:       local := [[NIL]]
  $compilingMap : local := true

  -- if there is a predicate, merge it in with the body
  if pred ~= true then body := ['IF,pred,body,'noMapVal]

  tar := getTarget t
  null m and tar is ['Mapping,.,:argTypes] and (#vars = #argTypes) =>
    if isPartialMode tar then throwKeyedMsg("S2IS0058",[tar])
    evalTargetedADEF(t,vars,rest tar,body)
  null m => evalUntargetedADEF(t,vars,types,body)
  evalTargetedADEF(t,vars,types,body)

evalUntargetedADEF(t,vars,types,body) ==
  -- recreate a parse form
  $freeVariables := []
  if vars is [var]
    then vars := var
    else vars := ['Tuple,:vars]
  val := objNewWrap(["+->",vars,body],$AnonymousFunction)
  putValue(t,val)
  putModeSet(t,[objMode val])

evalTargetedADEF(t,vars,types,body) ==
  $mapName : local := makeInternalMapName('"anonymousFunction",
    #vars,$anonymousMapCounter,'"internal")
  $anonymousMapCounter := 1 + $anonymousMapCounter
  $compilingMap   : local := true  -- state that we are trying to compile
  $mapThrowCount  : local := 0     -- number of "return"s encountered
  $mapReturnTypes : local := nil   -- list of types from returns
  $repeatLabel    : local := nil   -- for loops; see upREPEAT
  $breakCount     : local := 0     -- breaks from loops; ditto

  -- now substitute formal names for the parm variables
  -- this is used in the interpret-code case, but isn't so bad any way
  -- since it makes the bodies look more like regular map bodies

  sublist := [[var,:GENSYM()] for var in vars]
  body := sublisNQ(sublist,body)
  vars := [rest v for v in sublist]

  for m in rest types for var in vars repeat
    $env:= put(var,'mode,m,$env)
    mkLocalVar($mapName,var)
  for lvar in getLocalVars($mapName,body) repeat
    mkLocalVar($mapName,lvar)
  -- set up catch point for interpret-code mode
  x := CATCH('mapCompiler,compileTargetedADEF(t,vars,types,body))
  x = 'tryInterpOnly => mkInterpTargetedADEF(t,vars,types,body)
  x

mkInterpTargetedADEF(t,vars,types,oldBody) ==
  null first types =>
    throwKeyedMsg("S2IS0056",NIL)
    throwMessage '"   map result type needed but not present."
  arglCode := ['LIST,:[argCode for type in rest types for var in vars]]
    where argCode == ['putValueValue,['mkAtreeNode,MKQ var],
      objNewCode(['wrap,var],type)]
  put($mapName,'mapBody,oldBody,$e)
  body := ['rewriteMap1,MKQ $mapName,arglCode,MKQ types]
  compileADEFBody(t,vars,types,body,first types)

compileTargetedADEF(t,vars,types,body) ==
  val := compileBody(body, first types)
  computedResultType := objMode val
  body := wrapMapBodyWithCatch flattenCOND objVal val
  compileADEFBody(t,vars,types,body,computedResultType)

compileADEFBody(t,vars,types,body,computedResultType) ==
--+
  $compiledOpNameList := [$mapName]
  minivectorName := makeInternalMapMinivectorName(PNAME $mapName)
  $minivectorNames := [[$mapName,:minivectorName],:$minivectorNames]
  body := SUBST(minivectorName,"$$$",body)
  if $compilingInputFile then
    $minivectorCode := [:$minivectorCode,minivectorName]
  SET(minivectorName,LIST2REFVEC $minivector)

  -- The use of the three variables $definingMap, $genValue and $compilingMap
  -- is to cover the following cases:
  --
  -- $definingMap: This is set in analyzeMap and covers examples like:
  --  addx x == ((y: Integer): Integer +-> x + y)
  --  g := addx 10
  --  g 3
  -- i.e. we are storing the mapping as an object.
  --
  -- $compilingMap: This covers mappings which are created and applied "on the
  -- "fly", for example:
  --  [map(h +-> D(h, t), v) for v in [t]]
  --
  -- $genValue: This seems to be needed when we create a map as an argument
  -- for a constructor, e.g.:
  --  Dx: LODO(EXPR INT, f +-> D(f, x)) := D()
  --
  -- MCD 13/3/96
  $freeVariables := []
  $boundVariables := [minivectorName,:vars]
  body1 := checkForFreeVariables1(body, "ALL", $boundVariables)
  if not($definingMap or $freeVariables) then
    fun := compileInteractive [$mapName,['LAMBDA,[:vars,'envArg],body]]
    code := wrap RPLACA(fun, SYMBOL_-FUNCTION first fun)
  else
    body := body1
    fun := ['function,['LAMBDA,[:vars,'envArg],body]]
    code := ['CONS, fun, ["VECTOR", :reverse $freeVariables]]

  val := objNew(code,rt := ['Mapping,computedResultType,:rest types])
  putValue(t,val)
  putModeSet(t,[rt])

--% Handler for Algebraic Extensions

upAlgExtension t ==
  -- handler for algebraic extension declaration.  These are of
  --  the form "a | a**2+1", and have the effect that "a" is declared
  --  to be a simple algebraic extension, with respect to the given
  --  polynomial, and given the value "a" in this type.
  t isnt [op,var,eq] => nil
  null $genValue => throwKeyedMsg("S2IS0001",NIL)
  a := getUnname var
  clearCmdParts ['propert,a]  --clear properties of a
  algExtension:= eq2AlgExtension eq
  upmode := ['UnivariatePolynomial,a,$EmptyMode]
  $declaredMode : local := upmode
  putTarget(algExtension,upmode)
  ms:= bottomUp algExtension
  triple:= getValue algExtension
  upmode:= resolveTMOrCroak(objMode(triple),upmode)
  null (T:= coerceInteractive(triple,upmode)) =>
    throwKeyedMsgCannotCoerceWithValue(objVal(triple),
      objMode(triple),upmode)
  newmode := objMode T
  (field := resolveTCat(CADDR newmode,'(Field))) or
    throwKeyedMsg("S2IS0002",[eq])
  pd:= ['UnivariatePolynomial,a,field]
  null (canonicalAE:= coerceInteractive(T,pd)) =>
    throwKeyedMsgCannotCoerceWithValue(objVal T,objMode T,pd)
  sae:= ['SimpleAlgebraicExtension,field,pd,objValUnwrap canonicalAE]
  saeTypeSynonym := INTERN STRCONC('"SAE",STRINGIMAGE a)
  saeTypeSynonymValue := objNew(sae,'(Type))
  fun := getFunctionFromDomain('generator,sae,NIL)
  expr:= wrap SPADCALL(fun)
  putHist(saeTypeSynonym,'value,saeTypeSynonymValue,$e)
  putHist(a,'mode,sae,$e)
  putHist(a,'value,T2:= objNew(expr,sae),$e)
  clearDependencies(a,true)
  if $printTypeIfTrue then
    sayKeyedMsg("S2IS0003",NIL)
    sayMSG concat ['%l,'"   ",saeTypeSynonym,'" := ",
      :prefix2String objVal saeTypeSynonymValue]
    sayMSG concat ['"   ",a,'" : ",saeTypeSynonym,'" := ",a]
  putValue(op,T2)
  putModeSet(op,[sae])

eq2AlgExtension eq ==
  -- transforms "a=b" to a-b for processing
  eq is [op,:l] and VECP op and (getUnname op='equation) =>
    [mkAtreeNode "-",:l]
  eq

--% Handlers for booleans

upand x ==
  -- generates code for  and  forms. The second argument is only
  -- evaluated if the first argument is true.
  x isnt [op,term1,term2] => NIL
  putTarget(term1,$Boolean)
  putTarget(term2,$Boolean)
  ms := bottomUp term1
  ms isnt [=$Boolean] => throwKeyedMsgSP("S2IS0054",[1,'"_"and_""],term1)
  $genValue =>
    BooleanEquality(objValUnwrap(getValue term1),
      getConstantFromDomain('(false),$Boolean)) =>
        putValue(x,getValue term1)
        putModeSet(x,ms)
    -- first term is true, so look at the second one
    ms := bottomUp term2
    ms isnt [=$Boolean] => throwKeyedMsgSP("S2IS0054",[2,'"_"and_""],term2)
    putValue(x,getValue term2)
    putModeSet(x,ms)

  ms := bottomUp term2
  ms isnt [=$Boolean] => throwKeyedMsgSP("S2IS0054",[2,'"_"and_""],term2)
  -- generate an IF expression and let the rest of the code handle it
  cond := [mkAtreeNode "=",mkAtree 'false,term1]
  putTarget(cond,$Boolean)
  code := [mkAtreeNode 'IF,cond,mkAtree 'false,term2]
  putTarget(code,$Boolean)
  bottomUp code
  putValue(x,getValue code)
  putModeSet(x,ms)

upor x ==
  -- generates code for  or  forms. The second argument is only
  -- evaluated if the first argument is false.
  x isnt [op,term1,term2] => NIL
  putTarget(term1,$Boolean)
  putTarget(term2,$Boolean)
  ms := bottomUp term1
  ms isnt [=$Boolean] => throwKeyedMsgSP("S2IS0054",[1,'"_"or_""],term1)
  $genValue =>
    BooleanEquality(objValUnwrap(getValue term1),
      getConstantFromDomain('(true),$Boolean)) =>
        putValue(x,getValue term1)
        putModeSet(x,ms)
    -- first term is false, so look at the second one
    ms := bottomUp term2
    ms isnt [=$Boolean] => throwKeyedMsgSP("S2IS0054",[2,'"_"or_""],term2)
    putValue(x,getValue term2)
    putModeSet(x,ms)

  ms := bottomUp term2
  ms isnt [=$Boolean] => throwKeyedMsgSP("S2IS0054",[2,'"_"or_""],term2)
  -- generate an IF expression and let the rest of the code handle it
  cond := [mkAtreeNode "=",mkAtree 'true,term1]
  putTarget(cond,$Boolean)
  code := [mkAtreeNode 'IF,cond,mkAtree 'true,term2]
  putTarget(code,$Boolean)
  bottomUp code
  putValue(x,getValue code)
  putModeSet(x,ms)

--% Handlers for case

upcase t ==
  t isnt [op,lhs,rhs] => nil
  bottomUp lhs
  triple := getValue lhs
  objMode(triple) isnt ['Union,:unionDoms] =>
    throwKeyedMsg("S2IS0004",NIL)
  if (rhs' := isDomainValuedVariable(rhs)) then rhs := rhs'
  if first unionDoms is ['_:,.,.] then
     for i in 0.. for d in unionDoms repeat
        if d is ['_:,=rhs,.] then rhstag := i
     if NULL rhstag then error "upcase: bad Union form"
     $genValue =>
        rhstag = first unwrap objVal triple => code := wrap 'TRUE
        code := wrap NIL
     code :=
        ['COND,
          [['EQL,rhstag,['CAR,['unwrap,objVal triple]]],
            ''TRUE],
              [''T,NIL]]
  else
    $genValue =>
        t' := coerceUnion2Branch triple
        rhs = objMode t' => code := wrap 'TRUE
        code := wrap NIL
    triple' := objNewCode(['wrap,objVal triple],objMode triple)
    code :=
        ['COND,
          [['EQUAL,MKQ rhs,['objMode,['coerceUnion2Branch,triple']]],
            ''TRUE],
              [''T,NIL]]
  putValue(op,objNew(code,$Boolean))
  putModeSet(op,[$Boolean])

--% Handlers for TARGET

upTARGET t ==
  -- Evaluates the rhs to a mode,which is used as the target type for
  -- the lhs.
  t isnt [op,lhs,rhs] => nil
  -- do not (yet) support local variables on the rhs
  (not $genValue) and or/[CONTAINED(var,rhs) for var in $localVars] =>
    keyedMsgCompFailure("S2IC0010",[rhs])
  $declaredMode: local := NIL
  m:= evaluateType unabbrev rhs
  not isLegitimateMode(m,NIL,NIL) => throwKeyedMsg("S2IE0004",[m])
  $declaredMode:= m
  not atom(lhs) and putTarget(lhs,m)
  ms := bottomUp lhs
  first ms ~= m =>
    throwKeyedMsg("S2IC0011",[first ms,m])
  if categoryForm?(m) then
      putValue(op, objNew(devaluate objValUnwrap getValue lhs, m))
  else
      putValue(op,getValue lhs)
  putModeSet(op,ms)

--% Handlers for COERCE

upCOERCE t ==
  -- evaluate the lhs and then tries to coerce the result to the
  -- mode which is the rhs.
  -- previous to 5/16/89, this had the same semantics as
  --    (lhs@rhs) :: rhs
  -- this must be made explicit now.
  t isnt [op,lhs,rhs] => nil
  $useConvertForCoercions : local := true
  -- do not (yet) support local variables on the rhs
  (not $genValue) and or/[CONTAINED(var,rhs) for var in $localVars] =>
    keyedMsgCompFailure("S2IC0006",[rhs])
  $declaredMode: local := NIL
  m := evaluateType unabbrev rhs
  not isLegitimateMode(m,NIL,NIL) => throwKeyedMsg("S2IE0004",[m])
  $declaredMode:= m
  -- 05/16/89 (RSS) following line commented out to give correct
  -- semantic difference between :: and @
  bottomUp lhs
  type:=evalCOERCE(op,lhs,m)
  putModeSet(op,[type])

evalCOERCE(op,tree,m) ==
  -- the value of tree is coerced to mode m
  -- this is not necessary, if the target property of tree was used
  v  := getValue tree
  t1 := objMode(v)
  if $genValue and t1 is ['Union,:.] then
    v := coerceUnion2Branch v
    t1 := objMode(v)
  e  := objVal(v)
  value:=
    t1=m => v
    t2 :=
      if isPartialMode m
        then
          $genValue and (t1 = '(Symbol)) and containsPolynomial m =>
            resolveTM(['UnivariatePolynomial,objValUnwrap(v),'(Integer)],m)
          resolveTM(t1,m)
        else m
    null t2 => throwKeyedMsgCannotCoerceWithValue(e,t1,m)
    $genValue => coerceOrRetract(v,t2)
    objNew(getArgValue(tree,t2),t2)
  val:= value or throwKeyedMsgCannotCoerceWithValue(e,t1,m)
  if categoryForm?(m) then
      putValue(op, objNew(devaluate objValUnwrap val, m))
  else
      putValue(op,val)
  objMode(val)

--% Handlers for COLLECT

transformCollect [:itrl,body] ==
  -- syntactic transformation for COLLECT form, called from mkAtree1
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
  iterList:=NCONC(iterList,[:iterTran2 for it in itrl]) where
    iterTran2 ==
      it is ['STEP,:.] => nil
      it is ['IN,:.] => nil
      it is ['ON,:.] => nil
      it is ['WHILE,:.] => nil
      it is [op,b] and (op in '(UNTIL)) =>
        [[op,mkAtree1 b]]
      it is ['_|,pred] => nil
      keyedSystemError("S2GE0016",
        ['"transformCollect",'"Unknown type of iterator"])
  [:iterList,bodyTree]

upCOLLECT t ==
  -- $compilingLoop variable insures that throw to interp-only mode
  --   goes to the outermost loop.
  $compilingLoop => upCOLLECT1 t
  upCOLLECT0 t

upCOLLECT0 t ==
  -- sets up catch point for interpret-code mode
  $compilingLoop: local := true
  ms:=CATCH('loopCompiler,upCOLLECT1 t)
  ms = 'tryInterpOnly => interpOnlyCOLLECT t
  ms

upCOLLECT1 t ==
  t isnt [op,:itrl,body] => nil
  -- upCOLLECT with compiled body
  if (target := getTarget t) and not getTarget(body) then
    if target is [agg,S] and agg in '(List Vector Stream InfiniteTuple) then
      putTarget(body,S)
  $interpOnly => interpCOLLECT(op,itrl,body)
  isStreamCollect itrl => collectStream(t,op,itrl,body)
  upLoopIters itrl
  ms:= bottomUpCompile body
  [m]:= ms
  for itr in itrl repeat
    itr is ['UNTIL, pred] => bottomUpCompilePredicate(pred,'"until")
  mode:= ['Tuple,m]
  evalCOLLECT(op,rest t,mode)
  putModeSet(op,[mode])

upLoopIters itrl ==
  -- type analyze iterator loop iterators
  for iter in itrl repeat
    iter is ['WHILE,pred] =>
      bottomUpCompilePredicate(pred,'"while")
    iter is ['SUCHTHAT,pred] =>
      bottomUpCompilePredicate(pred,'"|")
    iter is ['UNTIL,:.] =>
      NIL      -- handle after body is analyzed
    iter is ['IN,index,s] =>
      upLoopIterIN(iter,index,s)
    iter is ['STEP,index,lower,step,:upperList] =>
      upLoopIterSTEP(index,lower,step,upperList)
      -- following is an optimization
      typeIsASmallInteger(get(index,'mode,$env)) =>
        RPLACA(iter,'ISTEP)
    NIL       -- should have error msg here?

upLoopIterIN(iter,index,s) ==
  iterMs := bottomUp s

  null IDENTP index =>  throwKeyedMsg("S2IS0005",[index])

  if $genValue and first iterMs is ['Union,:.] then
    v := coerceUnion2Branch getValue s
    m := objMode v
    putValue(s,v)
    putMode(s,m)
    iterMs := [m]
    putModeSet(s,iterMs)

  -- transform segment variable into STEP
  iterMs is [['Segment,.]] or iterMs is [['UniversalSegment,.]] =>
    lower := [mkAtreeNode 'lo,s]
    step := [mkAtreeNode 'incr, s]
    upperList :=
      CAAR(iterMs) = 'Segment => [[mkAtreeNode 'hi,s]]
      NIL
    upLoopIterSTEP(index,lower,step,upperList)
    newIter := ['STEP,index,lower,step,:upperList]
    RPLACA(iter, first newIter)
    RPLACD(iter, rest newIter)

  iterMs isnt [['List,ud]] => throwKeyedMsg("S2IS0006",[index])
  put(index,'mode,ud,$env)
  mkLocalVar('"the iterator expression",index)

upLoopIterSTEP(index,lower,step,upperList) ==
  null IDENTP index => throwKeyedMsg("S2IS0005",[index])
  ltype := IFCAR bottomUpUseSubdomain(lower)
  not (typeIsASmallInteger(ltype) or isEqualOrSubDomain(ltype,$Integer))=>
    throwKeyedMsg("S2IS0007",['"lower"])
  stype := IFCAR bottomUpUseSubdomain(step)
  not (typeIsASmallInteger(stype) or isEqualOrSubDomain(stype,$Integer))=>
    throwKeyedMsg("S2IS0008",NIL)
  types := [ltype]
  utype := nil
  for upper in upperList repeat
    utype := IFCAR bottomUpUseSubdomain(upper)
    not (typeIsASmallInteger(utype) or isEqualOrSubDomain(utype,$Integer))=>
      throwKeyedMsg("S2IS0007",['"upper"])
  if utype then types := [utype, :types]
  else types := [stype, :types]
  type := resolveTypeListAny REMDUP types
  put(index,'mode,type,$env)
  mkLocalVar('"the iterator expression",index)

evalCOLLECT(op,[:itrl,body],m) ==
  iters := [evalLoopIter itr for itr in itrl]
  bod := getArgValue(body,computedMode body)
  if bod isnt ['SPADCALL,:.] then bod := ['unwrap, bod]
  code := timedOptimization asTupleNewCode0 ['COLLECT,:iters,bod]
  if $genValue then code := wrap timedEVALFUN code
  putValue(op,objNew(code,m))

falseFun(x) == nil

evalLoopIter itr ==
  -- generate code for loop iterator
  itr is ['STEP,index,lower,step,:upperList] =>
    ['STEP,getUnname index,getArgValue(lower,$Integer),
      getArgValue(step,$Integer),
        :[getArgValue(upper,$Integer) for upper in upperList]]
  itr is ['ISTEP,index,lower,step,:upperList] =>
    ['ISTEP,getUnname index,getArgValue(lower,$SmallInteger),
      getArgValue(step,$SmallInteger),
        :[getArgValue(upper,$SmallInteger) for upper in upperList]]
  itr is ['IN,index,s] =>
    ['IN,getUnname index,getArgValue(s,['List,get(index,'mode,$env)])]
  (itr is [x,pred]) and (x in '(WHILE UNTIL SUCHTHAT)) =>
    [x,getArgValue(pred,$Boolean)]

interpCOLLECT(op,itrl,body) ==
  -- interpret-code mode COLLECT handler
  $collectTypeList: local := NIL
  $indexVars: local := NIL
  $indexTypes: local := NIL
  emptyAtree op
  emptyAtree itrl
  emptyAtree body
  code := ['COLLECT,:[interpIter itr for itr in itrl],
    interpCOLLECTbody(body,$indexVars,$indexTypes)]
  value := timedEVALFUN code
  t :=
    null value => '(None)
    last $collectTypeList
  rm := ['Tuple,t]
  value := [objValUnwrap coerceInteractive(objNewWrap(v,m),t)
    for v in value for m in $collectTypeList]
  putValue(op,objNewWrap(asTupleNew(#value, value),rm))
  putModeSet(op,[rm])

interpIter itr ==
  -- interpret loop iterator
  itr is ['STEP,index,lower,step,:upperList] =>
    $indexVars:= [getUnname index,:$indexVars]
    [m]:= bottomUp lower
    $indexTypes:= [m,:$indexTypes]
    for up in upperList repeat bottomUp up
    ['STEP,getUnname index,getArgValue(lower,$Integer),
      getArgValue(step,$Integer),
        :[getArgValue(upper,$Integer) for upper in upperList]]
  itr is ['ISTEP,index,lower,step,:upperList] =>
    $indexVars:= [getUnname index,:$indexVars]
    [m]:= bottomUp lower
    $indexTypes:= [m,:$indexTypes]
    for up in upperList repeat bottomUp up
    ['ISTEP,getUnname index,getArgValue(lower,$SmallInteger),
      getArgValue(step,$SmallInteger),
        :[getArgValue(upper,$SmallInteger) for upper in upperList]]
  itr is ['IN,index,s] =>
    $indexVars:=[getUnname index,:$indexVars]
    [m]:= bottomUp s
    m isnt ['List,um] => throwKeyedMsg("S2IS0009",[m])
    $indexTypes:=[um,:$indexTypes]
    ['IN,getUnname index,getArgValue(s,m)]
  (itr is [x,pred]) and (x in '(WHILE UNTIL SUCHTHAT)) =>
    [x,interpLoop(pred,$indexVars,$indexTypes,$Boolean)]

interpOnlyCOLLECT t ==
  -- called when compilation failed in COLLECT body, not in compiling map
  $genValue: local := true
  $interpOnly: local := true
  upCOLLECT t

interpCOLLECTbody(expr,indexList,indexTypes) ==
  -- generate code for interpret-code collect
  ['interpCOLLECTbodyIter,MKQ expr,MKQ indexList,['LIST,:indexList],
    MKQ indexTypes]

interpCOLLECTbodyIter(exp,indexList,indexVals,indexTypes) ==
  -- execute interpret-code collect body.  keeps list of type of
  --  elements in list in $collectTypeList.
  emptyAtree exp
  for i in indexList for val in indexVals for type in indexTypes repeat
    put(i,'value,objNewWrap(val,type),$env)
  [m]:=bottomUp exp
  $collectTypeList:=
    null $collectTypeList => [rm:=m]
    [:$collectTypeList,rm:=resolveTT(m,last $collectTypeList)]
  null rm => throwKeyedMsg("S2IS0010",NIL)
  value:=
    rm ~= m => coerceInteractive(getValue exp,rm)
    getValue exp
  objValUnwrap(value)

--% Stream Collect functions

isStreamCollect itrl ==
  -- calls bottomUp on iterators and if any of them are streams
  -- then whole shebang is a stream
  isStream := false
  for itr in itrl until isStream repeat
    itr is ['IN,.,s] =>
      iterMs := bottomUp s
      iterMs is [['Stream,:.]] => isStream := true
      iterMs is [['InfiniteTuple,:.]] => isStream := true
      iterMs is [['UniversalSegment,:.]] => isStream := true
    itr is ['STEP,.,.,.] => isStream := true
  isStream

collectStream(t,op,itrl,body) ==
  v := CATCH('loopCompiler,collectStream1(t,op,itrl,body))
  v = 'tryInterpOnly => throwKeyedMsg("S2IS0011",NIL)
  v

collectStream1(t,op,itrl,body) ==
  $indexVars:local := NIL
  upStreamIters itrl
  if #$indexVars = 1 then mode:=collectOneStream(t,op,itrl,body)
  else mode:=collectSeveralStreams(t,op,itrl,body)
  putModeSet(op,[mode])

upStreamIters itrl ==
  -- type analyze stream collect loop iterators
  for iter in itrl repeat
    iter is ['IN,index,s] =>
      upStreamIterIN(iter,index,s)
    iter is ['STEP,index,lower,step,:upperList] =>
      upStreamIterSTEP(index,lower,step,upperList)

upStreamIterIN(iter,index,s) ==
  iterMs := bottomUp s

  -- transform segment variable into STEP
  iterMs is [['Segment,.]] or iterMs is [['UniversalSegment,.]] =>
    lower := [mkAtreeNode 'lo, s]
    step := [mkAtreeNode 'incr, s]
    upperList :=
      CAAR(iterMs) = 'Segment => [[mkAtreeNode 'hi,s]]
      NIL
    upStreamIterSTEP(index,lower,step,upperList)
    newIter := ['STEP,index,lower,step,:upperList]
    RPLACA(iter, first newIter)
    RPLACD(iter, rest newIter)

  (iterMs isnt [['List,ud]]) and (iterMs isnt [['Stream,ud]])
    and (iterMs isnt [['InfinitTuple, ud]]) =>
      throwKeyedMsg("S2IS0006",[index])
  put(index,'mode,ud,$env)
  mkLocalVar('"the iterator expression",index)
  s :=
    iterMs is [['List,ud],:.] =>
      form:=[mkAtreeNode 'pretend, [mkAtreeNode 'COERCE,s,['Stream,ud]],
             ['InfiniteTuple, ud]]
      bottomUp form
      form
    s
  $indexVars:= [[index,:s],:$indexVars]

upStreamIterSTEP(index,lower,step,upperList) ==
  null isEqualOrSubDomain(ltype := IFCAR bottomUpUseSubdomain(lower),
    $Integer) => throwKeyedMsg("S2IS0007",['"lower"])
  null isEqualOrSubDomain(stype := IFCAR bottomUpUseSubdomain(step),
    $Integer) => throwKeyedMsg("S2IS0008",NIL)
  for upper in upperList repeat
    null isEqualOrSubDomain(IFCAR bottomUpUseSubdomain(upper),
      $Integer) => throwKeyedMsg("S2IS0007",['"upper"])

  put(index,'mode,type := resolveTT(ltype,stype),$env)
  null type => throwKeyedMsg("S2IS0010", nil)
  mkLocalVar('"the iterator expression",index)

  s :=
    null upperList =>
      -- create the function that does the appropriate incrementing
      genFun := 'stream
      form := [mkAtreeNode genFun,
        [[mkAtreeNode 'Dollar, ['IncrementingMaps,type],
          mkAtreeNode 'incrementBy],step],lower]
      bottomUp form
      form
    form := [mkAtreeNode 'SEGMENT,lower,first upperList]
    putTarget(form,['Segment,type])
    form := [mkAtreeNode 'construct,form]
    putTarget(form,['List,['Segment,type]])
    form := [mkAtreeNode 'expand,form]
    putTarget(form,'(List (Integer)))
    form:=[mkAtreeNode 'pretend, [mkAtreeNode 'COERCE,form,['Stream,$Integer]],
           ['InfiniteTuple, $Integer]]
    bottomUp form
    form
  $indexVars:= [[index,:s],:$indexVars]

collectOneStream(t,op,itrl,body) ==
  -- build stream collect for case of iterating over a single stream
  --  In this case we don't need to build records
  form := mkAndApplyPredicates itrl
  bodyVec := mkIterFun(first $indexVars, body, $localVars)
  form := [mkAtreeNode 'map,bodyVec,form]
  bottomUp form
  val := getValue form
  m := objMode val
  m isnt ['Stream, ud] and m isnt ['InfiniteTuple, ud] =>
    systemError '"Not a Stream"
  newVal := objNew(objVal val, ['InfiniteTuple, ud])
  putValue(op,newVal)
  objMode newVal

mkAndApplyPredicates itrl ==
  -- for one index variable case for now.  may generalize later
  [indSet] := $indexVars
  [.,:s] := indSet
  for iter in itrl repeat
    iter is ['WHILE,pred] =>
      fun := 'filterWhile
      predVec := mkIterFun(indSet,pred,$localVars)
      s := [mkAtreeNode fun,predVec,s]
    iter is ['UNTIL,pred] =>
      fun := 'filterUntil
      predVec := mkIterFun(indSet,pred,$localVars)
      s := [mkAtreeNode fun,predVec,s]
    iter is ['SUCHTHAT,pred] =>
      fun := 'select
      putTarget(pred,$Boolean)
      predVec := mkIterFun(indSet,pred,$localVars)
      s := [mkAtreeNode fun,predVec,s]
  s

mkIterFun([index,:s],funBody,$localVars) ==
  -- transform funBody into a lambda with index as the parameter
  mode := objMode getValue s
  mode isnt ['Stream, indMode] and mode isnt ['InfiniteTuple, indMode] =>
    keyedSystemError('"S2GE0016", '("mkIterFun" "bad stream index type"))
  put(index,'mode,indMode,$env)
  mkLocalVar($mapName,index)
  [m]:=bottomUpCompile funBody
  mapMode := ['Mapping,m,indMode]
  $freeVariables := []
  $boundVariables := [index]
  -- CCL does not support upwards funargs, so we check for any free variables
  -- and pass them into the lambda as part of envArg.
  body := checkForFreeVariables(getValue funBody,$localVars)
  val:=['function,['LAMBDA,[index,'envArg],objVal body]]
  vec := mkAtreeNode GENSYM()
  putValue(vec,objNew(['CONS,val,["VECTOR",:reverse $freeVariables]],mapMode))
  vec

checkIterationForFreeVariables(op, itl, locals) ==
    boundVars := getIteratorIds itl
    $boundVariables := APPEND(boundVars, $boundVariables)
    r := [op, :[checkForFreeVariables(a, locals) for a in itl]]
    for var in boundVars repeat
        $boundVariables := delete(var, $boundVariables)
    r

checkForFreeVariables1(v, locals, $boundVariables) ==
    checkForFreeVariables(v, locals)

checkForFreeVariables(v,locals) ==
  -- v is the body of a lambda expression.  The list $boundVariables is all the
  -- bound variables, the parameter locals contains local variables which might
  -- be free, or the token ALL, which means that any parameter is a candidate
  -- to be free.
  NULL v => v
  SYMBOLP v =>
    v="$$$" => v -- Placeholder for mini-vector
    MEMQ(v,$boundVariables) => v
    p := POSITION(v,$freeVariables) =>
      ["ELT","envArg",positionInVec(p,#($freeVariables))]
    (locals = "ALL") or MEMQ(v,locals) =>
      $freeVariables := [v,:$freeVariables]
      ["ELT","envArg",positionInVec(0,#($freeVariables))]
    v
  LISTP v =>
    rest(LASTTAIL v) => -- Must be a better way to check for a genuine list?
      v
    [op,:args] := v
    LISTP op =>
      -- Might have a mode at the front of a list, or be calling a function
      -- which returns a function.
      [checkForFreeVariables(op,locals),:[checkForFreeVariables(a,locals) for a in args]]
    op = "LETT" => -- Expands to a SETQ.
      ["SETF",:[checkForFreeVariables(a,locals) for a in args]]
    op = "COLLECT" => -- Introduces a new bound variable?
        checkIterationForFreeVariables(op, args, locals)
    op = "REPEAT" => -- Introduces a new bound variable?
        checkIterationForFreeVariables(op, args, locals)
    op = "LET" =>
      args is [var,form,name] =>
        -- This is some bizarre LET, not what one would expect in Common Lisp!
        -- Treat var as a free variable, since it may be bound out of scope
        -- if we are in a lambda within another lambda.
        newvar :=
          p := POSITION(var,$freeVariables) =>
            ["ELT","envArg",positionInVec(p,#($freeVariables))]
          if not(MEMQ(var, $boundVariables)) then
              $boundVariables := cons(var, $boundVariables)
          var
        ["SETF",newvar,checkForFreeVariables(form,locals)]
      error "Non-simple variable bindings are not currently supported"
    op = "PROG" =>
      error "Non-simple variable bindings are not currently supported"
    op = "LAMBDA" => v
    op = "QUOTE" => v
    op = "getValueFromEnvironment" => v
    op = "local" =>
        nargs := []
        for a in args repeat
            a is [":", var, dom] =>
                dom := checkForFreeVariables(dom, locals)
                if not(MEMQ(var, $boundVariables)) then
                    $boundVariables := cons(var, $boundVariables)
                nargs := cons([":", var, dom], nargs)
            if not(MEMQ(a, $boundVariables)) then
                $boundVariables := cons(a, $boundVariables)
            nargs := cons(a, nargs)
        ["local", :NREVERSE(nargs)]
    [op,:[checkForFreeVariables(a,locals) for a in args]]
  v

positionInVec(p,l) ==
  -- We cons up the free list, but need to keep positions consistent so
  -- count from the end of the list.
  l-p-1

collectSeveralStreams(t,op,itrl,body) ==
  -- performs collects over several streams in parallel
  $index: local := nil
  [form,:zipType] := mkZipCode $indexVars
  form := mkAndApplyZippedPredicates($indexVars, form,zipType,itrl)
  vec := mkIterZippedFun($indexVars,body,zipType,$localVars)
  form := [mkAtreeNode 'map, vec, form]
  bottomUp form
  val := getValue form
  m := objMode val
  m isnt ['Stream, ud] and m isnt ['InfiniteTuple, ud] =>
    systemError '"Not a Stream"
  newVal := objNew(objVal val, ['InfiniteTuple, ud])
  putValue(op,newVal)
  objMode newVal

mkZipCode indexList ==
  -- create interpreter form for turning a list of parallel streams
  -- into a stream of nested record types.  returns [form,:recordType]
  #indexList = 2 =>
    [[.,:s2],[.,:s1]] := indexList
    t1 := CADR objMode getValue s1
    t2 := CADR objMode getValue s2
    zipType := ['Record,['_:,'part1,t1], ['_:,'part2,t2] ]
    zipFun := [mkAtreeNode 'Dollar, ['MakeRecord,mkEvalable t1,
                                     mkEvalable t2],
               mkAtreeNode 'makeRecord]
    form := [mkAtreeNode 'map,zipFun,s1,s2]
    [form,:zipType]
  [form, :zipType] := mkZipCode rest indexList
  [[.,:s],:.] := indexList
  t := CADR objMode getValue s
  zipFun := [mkAtreeNode 'Dollar, ['MakeRecord,mkEvalable t,
                                   mkEvalable zipType],
             mkAtreeNode 'makeRecord]
  form := [mkAtreeNode 'map,zipFun,s,form]
  zipType := ['Record,['_:,'part1,t],['_:,'part2,zipType]]
  [form,:zipType]

mkAndApplyZippedPredicates (indexList, s,zipType,itrl) ==
  -- for one index variable case for now.  may generalize later
  for iter in itrl repeat
    iter is ['WHILE,pred] =>
      predVec := mkIterZippedFun(indexList,pred,zipType,$localVars)
      s := [mkAtreeNode 'swhile,predVec,s]
    iter is ['UNTIL,pred] =>
      predVec := mkIterZippedFun(indexList,pred,zipType,$localVars)
      s := [mkAtreeNode 'suntil,predVec,s]
    iter is ['SUCHTHAT,pred] =>
      putTarget(pred,$Boolean)
      predVec := mkIterZippedFun(indexList,pred,zipType,$localVars)
      s := [mkAtreeNode 'select,predVec,s]
  s

mkIterZippedFun(indexList,funBody,zipType,$localVars) ==
  -- transform funBody into a lamda with $index as the parameter
  numVars:= #$indexVars
  for [var,:.] in $indexVars repeat
    funBody := subVecNodes(mkIterVarSub(var,numVars),var,funBody)
  put($index,'mode,zipType,$env)
  mkLocalVar($mapName,$index)
  [m]:=bottomUpCompile funBody
  mapMode := ['Mapping,m,zipType]
  $freeVariables := []
  $boundVariables := [$index]
  -- CCL does not support upwards funargs, so we check for any free variables
  -- and pass them into the lambda as part of envArg.
  body :=
   [checkForFreeVariables(form,$localVars) for form in getValue funBody]
  val:=['function,['LAMBDA,[$index,'envArg],objVal body]]
  vec := mkAtreeNode GENSYM()
  putValue(vec,objNew(['CONS,val,["VECTOR",:reverse $freeVariables]],mapMode))
  vec

subVecNodes(new,old,form) ==
  ATOM form =>
    (VECP form) and (form.0 = old) => new
    form
  [subVecNodes(new, old, first form), :subVecNodes(new, old, rest form)]

mkIterVarSub(var,numVars) ==
  n := iterVarPos var
  n=2 =>
    [mkAtreeNode 'elt,mkNestedElts(numVars-2),mkAtreeNode 'part2]
  n=1 =>
    [mkAtreeNode 'elt,mkNestedElts(numVars-2),mkAtreeNode 'part1]
  [mkAtreeNode 'elt,mkNestedElts(numVars-n),mkAtreeNode 'part1]

iterVarPos var ==
  for [index,:.] in reverse $indexVars for i in 1.. repeat
    index=var => return(i)

mkNestedElts n ==
  n=0 => mkAtreeNode($index or ($index:= GENSYM()))
  [mkAtreeNode 'elt, mkNestedElts(n-1), mkAtreeNode 'part2]

--% Handlers for construct

upconstruct t ==
  --Computes the common mode set of the construct by resolving across
  --the argument list, and evaluating
  t isnt [op,:l] => nil
  dol := getAtree(op,'dollar)
  tar := getTarget(op) or dol
  null l => upNullList(op,l,tar)
  tar is ['Record,:types] => upRecordConstruct(op,l,tar)
  isTaggedUnion tar => upTaggedUnionConstruct(op,l,tar)
  aggs := '(List)
  if tar and PAIRP(tar) and not isPartialMode(tar) then
    first(tar) in aggs =>
      ud :=
        (l is [[realOp, :.]]) and (getUnname(realOp) = 'COLLECT) => tar
        CADR tar
      for x in l repeat if not getTarget(x) then putTarget(x,ud)
    first(tar) in '(Matrix SquareMatrix RectangularMatrix) =>
      vec := ['List,underDomainOf tar]
      for x in l repeat if not getTarget(x) then putTarget(x,vec)
  argModeSetList:= [bottomUp x for x in l]
  dol and dol is [topType,:.] and not (topType in aggs) =>
    (mmS:= selectMms(op,l,tar)) and (mS:= evalForm(op,getUnname op,l,mmS)) =>
      putModeSet(op,mS)
    NIL
  (tar and tar is [topType,:.] and not (topType in aggs)) and
    (mmS:= modemapsHavingTarget(selectMms(op,l,tar),tar)) and
        (mS:= evalForm(op,getUnname op,l,mmS)) =>
          putModeSet(op,mS)
  eltTypes := replaceSymbols([first x for x in argModeSetList],l)
  eltTypes is [['Tuple, td]] =>
    mode := ['List, td]
    evalTupleConstruct(op, l, mode, tar)
  eltTypes is [['InfiniteTuple, td]] =>
    mode := ['Stream, td]
    evalInfiniteTupleConstruct(op, l, mode, tar)
  if not isPartialMode(tar) and tar is ['List,ud] then
    mode := ['List, resolveTypeListAny cons(ud,eltTypes)]
  else mode := ['List, resolveTypeListAny eltTypes]
  if isPartialMode tar then tar:=resolveTM(mode,tar)
  evalconstruct(op,l,mode,tar)

modemapsHavingTarget(mmS,target) ==
  -- returns those modemaps have the signature result matching the
  -- given target
  [mm for mm in mmS | ([[.,res,:.],:.] := mm) and res = target]

evalTupleConstruct(op,l,m,tar) ==
  ['List, ud] := m
  code := ['APPEND,
    :([["asTupleAsList", getArgValueOrThrow(x,['Tuple, ud])] for x in l])]
  val :=
    $genValue => objNewWrap(timedEVALFUN code,m)
    objNew(code,m)

  (val1 := coerceInteractive(val,tar or m)) =>
    putValue(op,val1)
    putModeSet(op,[tar or m])
  putValue(op,val)
  putModeSet(op,[m])

evalInfiniteTupleConstruct(op,l,m,tar) ==
  ['Stream, ud] := m
  code := first [(getArgValue(x,['InfiniteTuple, ud]) or
    throwKeyedMsg("S2IC0007",[['InifinteTuple, ud]])) for x in l]
  val :=
    $genValue => objNewWrap(timedEVALFUN code,m)
    objNew(code,m)
  if tar then val1 := coerceInteractive(val,tar) else val1 := val

  val1 =>
    putValue(op,val1)
    putModeSet(op,[tar or m])
  putValue(op,val)
  putModeSet(op,[m])

evalconstruct(op,l,m,tar) ==
  [agg,:.,underMode]:= m
  code := ['LIST, :(argCode:=[(getArgValue(x,underMode) or
    throwKeyedMsg("S2IC0007",[underMode])) for x in l])]
  val :=
    $genValue => objNewWrap(timedEVALFUN code,m)
    objNew(code,m)
  if tar then val1 := coerceInteractive(val,tar) else val1 := val

  val1 =>
    putValue(op,val1)
    putModeSet(op,[tar or m])
  putValue(op,val)
  putModeSet(op,[m])

replaceSymbols(modeList,l) ==
  -- replaces symbol types with their corresponding polynomial types
  --  if not all type are symbols
  not ($Symbol in modeList) => modeList
  modeList is [a,:b] and and/[a=x for x in b] => modeList
  [if m=$Symbol then getMinimalVarMode(objValUnwrap(getValue arg),
    $declaredMode) else m for m in modeList for arg in l]

upNullList(op,l,tar) ==
  -- handler for [] (empty list)
  defMode :=
    tar and tar is [a,b] and (a in '(Stream Vector List)) and
      not isPartialMode(b) => ['List,b]
    '(List (None))
  val := objNewWrap(NIL,defMode)
  tar and not isPartialMode(tar) =>
    null (val' := coerceInteractive(val,tar)) =>
      throwKeyedMsg("S2IS0013",[tar])
    putValue(op,val')
    putModeSet(op,[tar])
  putValue(op,val)
  putModeSet(op,[defMode])

upTaggedUnionConstruct(op,l,tar) ==
  -- special handler for tagged union constructors
  tar isnt [.,:types] => nil
  #l ~= 1 => throwKeyedMsg("S2IS0051",[#l,tar])
  bottomUp first l
  obj := getValue first l
  (code := coerceInteractive(getValue first l,tar)) or
    throwKeyedMsgCannotCoerceWithValue(objVal obj, objMode obj,tar)
  putValue(op,code)
  putModeSet(op,[tar])

upRecordConstruct(op,l,tar) ==
  -- special handler for record constructors
  tar isnt [.,:types] => nil
  argModes := nil
  for arg in l repeat bottomUp arg
  argCode :=
    [(getArgValue(arg,type) or throwKeyedMsgCannotCoerceWithValue(
      objVal getValue arg,objMode getValue arg,type))
        for arg in l for ['_:,.,type] in types]
  len := #l
  code :=
    (len = 1) => ['CONS, :argCode, '()]
    (len = 2) => ['CONS,:argCode]
    ['VECTOR,:argCode]
  if $genValue then code :=  wrap timedEVALFUN code
  putValue(op,objNew(code,tar))
  putModeSet(op,[tar])

--% Handlers for declarations

upDeclare t ==
  t isnt  [op,lhs,rhs] => nil
  (not $genValue) and or/[CONTAINED(var,rhs) for var in $localVars] =>
    keyedMsgCompFailure("S2IS0014",[lhs])
  mode := evaluateType unabbrev rhs
  mode = $Void => throwKeyedMsgSP("S2IS0015",NIL,op)
  not isLegitimateMode(mode,nil,nil) => throwKeyedMsgSP("S2IE0004",[mode],op)
  categoryForm?(mode) => throwKeyedMsgSP("S2IE0011",[mode, 'category],op)
  packageForm?(mode) => throwKeyedMsgSP("S2IE0011",[mode, 'package],op)
  junk :=
    lhs is ['free,['Tuple,:vars]] or lhs is ['free,['LISTOF,:vars]] or
      lhs is ['free,:vars] =>
        for var in vars repeat declare(['free,var],mode)
    lhs is ['local,['Tuple,:vars]] or lhs is ['local,['LISTOF,:vars]] or
      lhs is ['local,:vars] =>
        for var in vars repeat declare(['local,var],mode)
    lhs is ['Tuple,:vars] or lhs is ['LISTOF,:vars] =>
      for var in vars repeat declare(var,mode)
    declare(lhs,mode)
  putValue(op,objNewWrap(voidValue(), $Void))
  putModeSet(op,[$Void])

declare(var,mode) ==
  -- performs declaration.
  -- 10/31/89: no longer coerces value to new declared type
  if var is ['local,v] then
    uplocalWithType(v,mode)
    var := v
  if var is ['free,v] then
    upfreeWithType(v,mode)
    var := v
  not IDENTP(var) =>
    throwKeyedMsg("S2IS0016",[STRINGIMAGE var])
  var in '(% %%) => throwKeyedMsg("S2IS0050",[var])
  if get(var,'isInterpreterFunction,$e) then
    mode isnt ['Mapping,.,:args] =>
      throwKeyedMsg("S2IS0017",[var,mode])
    -- validate that the new declaration has the defined # of args
    mapval := objVal get(var,'value,$e)
    -- mapval looks like '(SPADMAP (args . defn))
    margs := CAADR mapval
    -- if one args, margs is not a pair, just #1 or NIL
    -- otherwise it looks like (Tuple #1 #2 ...)
    nargs :=
      null margs => 0
      PAIRP margs => -1 + #margs
      1
    nargs ~= #args => throwKeyedMsg("S2IM0008",[var])
  if $compilingMap then mkLocalVar($mapName,var)
  else clearDependencies(var,true)
  isLocalVar(var) => put(var,'mode,mode,$env)
  mode is ['Mapping,:.] => declareMap(var,mode)
  v := get(var,'value,$e) =>
    -- only allow this if either
    --   - value already has given type
    --   - new mode is same as old declared mode
    objMode(v) = mode => putHist(var,'mode,mode,$e)
    mode = get(var,'mode,$e) => NIL   -- nothing to do
    throwKeyedMsg("S2IS0052",[var,mode])
  putHist(var,'mode,mode,$e)

declareMap(var,mode) ==
  -- declare a Mapping property
  (v := get(var, 'value, $e)) and objVal(v) isnt ['SPADMAP, :.] =>
      objMode(v) = mode => putHist(var, 'mode, mode, $e)
      mode = get(var, 'mode, $e) => nil
      throwKeyedMsg("S2IS0019", [var])
  isPartialMode mode => throwKeyedMsg("S2IM0004",NIL)
  putHist(var,'mode,mode,$e)

containsLocalVar(tree) ==
    or/[CONTAINED(var, tree) for var in $localVars] or
       CONTAINED("$$$", tree)

getAndEvalConstructorArgument tree ==
  triple := getValue tree
  objMode triple = '(Type) => triple
  isWrapped objVal(triple) => triple
  containsLocalVar objVal triple =>
      compFailure('"   Local variable or parameter used in type")
  objNewWrap(timedEVALFUN objVal(triple), objMode(triple))

replaceSharps(x,d) ==
  -- replaces all sharps in x by the arguments of domain d
  -- all replaces the triangle variables
  SL:= NIL
  for e in rest d for var in $FormalMapVariableList repeat
    SL:= CONS(CONS(var,e),SL)
  x := subCopy(x,SL)
  SL:= NIL
  for e in rest d for var in $TriangleVariableList repeat
    SL:= CONS(CONS(var,e),SL)
  subCopy(x,SL)

isDomainValuedVariable form ==
  -- returns the value of form if form is a variable with a type value
  IDENTP form and (val := (
    get(form,'value,$InteractiveFrame) or _
    (PAIRP($env) and get(form,'value,$env)) or _
    (PAIRP($e) and get(form,'value,$e)))) and _
      categoryForm?(objMode(val)) =>
        objValUnwrap(val)
  nil

evalCategory(d,c) ==
  -- tests whether domain d has category c
  isPartialMode d or ofCategory(d,c)

isOkInterpMode m ==
  isPartialMode(m) => isLegitimateMode(m,nil,nil)
  isValidType(m) and isLegitimateMode(m,nil,nil)

isLegitimateRecordOrTaggedUnion u ==
  and/[x is [":",.,d] and isLegitimateMode(d,nil,nil) for x in u]

isPolynomialMode m ==
  -- If m is a polynomial type this function returns a list of its
  --  variables, and nil otherwise
  m is [op,a,:rargs] =>
    a := removeQuote a
    MEMQ(op, '(Polynomial Expression))=> 'all
    op = 'UnivariatePolynomial => LIST a
    op = 'Variable       => LIST a
    MEMQ(op, '(MultivariatePolynomial DistributedMultivariatePolynomial
      HomogeneousDistributedMultivariatePolynomial)) => a
    NIL
  NIL

containsPolynomial m ==
  not PAIRP(m) => NIL
  [d,:.] := m
  d in $univariateDomains or d in $multivariateDomains or
      d = 'Polynomial => true
  (m' := underDomainOf m) and containsPolynomial m'

containsVariables m ==
  not PAIRP(m) => NIL
  [d,:.] := m
  d in $univariateDomains or d in $multivariateDomains => true
  (m' := underDomainOf m) and containsVariables m'

listOfDuplicates l ==
  l is [x,:l'] =>
    x in l' => [x,:listOfDuplicates deleteAll(x,l')]
    listOfDuplicates l'

-- The following function removes all occurrences of x from the list l

deleteAll(x,l) ==
  null l => nil
  x = first(l) => deleteAll(x, rest l)
  [first l,:deleteAll(x,rest l)]
