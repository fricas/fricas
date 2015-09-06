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
New Selection of Modemaps

selection of applicable modemaps is done in two steps:
  first it tries to find a modemap inside an argument domain, and if
  this fails, by evaluation of pattern modemaps
the result is a list of functions with signatures, which have the
  following form:
  [sig,elt,cond] where
    sig is the signature gained by evaluating the modemap condition
    elt is the slot number to get the implementation
    cond are runtime checks which are the results of evaluating the
    modemap condition

the following flags are used:
 $Coerce is NIL, if function selection is done which requires exact
   matches (e.g. for coercion functions)
 if $SubDom is true, then runtime checks have to be compiled
)endif

DEFPARAMETER($constructorExposureList, '(Boolean Integer String))

sayFunctionSelection(op,args,target,dc,func) ==
  $abbreviateTypes : local := true
  startTimingProcess 'debug
  fsig := formatSignatureArgs args
  if not LISTP fsig then fsig := LIST fsig
  if func then func := bright ['"by ",func]
  sayMSG concat ['%l,:bright '"Function Selection for",op,:func,'%l,
    '"      Arguments:",:bright fsig]
  if target then sayMSG concat ['"      Target type:",
    :bright prefix2String target]
  if dc  then sayMSG concat ['"      From:     ",
    :bright prefix2String dc]
  stopTimingProcess 'debug

sayFunctionSelectionResult(op,args,mmS) ==
  $abbreviateTypes : local := true
  startTimingProcess 'debug
  if mmS then printMms mmS
  else sayMSG concat ['"   -> no function",:bright op,
    '"found for arguments",:bright formatSignatureArgs args]
  stopTimingProcess 'debug

selectMms(op,args,$declaredMode) ==
  -- selects applicable modemaps for node op and arguments args
  -- if there is no local modemap, and it is not a package call, then
  --   the cached function selectMms1 is called
  startTimingProcess 'modemaps
  n:= getUnname op
  val := getValue op
  opMode := objMode val

  -- see if we have a functional parameter
  ((isSharpVarWithNum(n) and opMode) or (val and opMode)) and
      opMode is ['Mapping,:ta] =>
        imp :=
          val => wrapped2Quote objVal val
          n
        [[['local,:ta], imp , NIL]]

  ((isSharpVarWithNum(n) and opMode) or (val and opMode)) and
      opMode is ['Variable,f] =>
         emptyAtree op
         op.0 := f
         selectMms(op,args,$declaredMode)

  isSharpVarWithNum(n) and opMode is ['FunctionCalled,f] =>
         op.0 := f
         selectMms(op,args,$declaredMode)

  types1 := getOpArgTypes(n,args)
  numArgs := #args
  member($EmptyMode,types1) => NIL

  tar := getTarget op
  dc  := getAtree(op,'dollar)

  null dc and val and objMode(val) = $AnonymousFunction =>
      tree := mkAtree objValUnwrap getValue op
      putTarget(tree,['Mapping,tar,:types1])
      bottomUp tree
      val := getValue tree
      [[['local,:rest objMode val], wrapped2Quote objVal val, NIL]]

  if (n = 'map) and (first types1 = $AnonymousFunction)
    then
      tree := mkAtree objValUnwrap getValue first args
      ut :=
        tar => underDomainOf tar
        NIL
      ua := [underDomainOf x for x in rest types1]
      member(NIL,ua) => NIL
      putTarget(tree,['Mapping,ut,:ua])
      bottomUp tree
      val := getValue tree
      types1 := [objMode val,:rest types1]
      RPLACA(args,tree)

  if numArgs = 1 and (n = "numer" or n = "denom") and
    isEqualOrSubDomain(first types1,$Integer) and null dc then
      dc := ['Fraction, $Integer]
      putAtree(op, 'dollar, dc)


  if $reportBottomUpFlag then sayFunctionSelection(n,types1,tar,dc,NIL)

  identType := 'Variable
  for x in types1 while not $declaredMode repeat
      not EQCAR(x,identType) => $declaredMode:= x
  types2 := [altTypeOf(x,y,$declaredMode) for x in types1 for y in args]

  mmS:=
    dc => selectDollarMms(dc,n,types1,types2)

    if n = "/" and tar = $Integer then
      tar := $RationalNumber
      putTarget(op,tar)

    -- now to speed up some standard selections
    if not tar then
      tar := defaultTarget(op,n,#types1,types1)
      if tar and $reportBottomUpFlag then
        sayMSG concat ['"      Default target type:",
          :bright prefix2String tar]

    selectLocalMms(op,n,types1,tar) or
      (VECTORP op and selectMms1(n,tar,types1,types2,'T))
  if $reportBottomUpFlag then sayFunctionSelectionResult(n,types1,mmS)
  stopTimingProcess 'modemaps
  mmS

-- selectMms1 is in clammed.boot

selectMms2(op,tar,args1,args2,$Coerce) ==
  -- decides whether to find functions from a domain or package
  --   or by general modemap evaluation
  or/[STRINGP arg for arg in args1] => NIL
  if tar = $EmptyMode then tar := NIL
  nargs := #args1
  mmS := NIL
  mmS :=
    -- special case map for the time being
    $Coerce and (op = 'map) and (2 = nargs) and
      (first(args1) is ['Variable,fun]) =>
        null (ud := underDomainOf CADR args1) => NIL
        if tar then ut := underDomainOf(tar)
        else ut := nil
        null (mapMms := selectMms1(fun,ut,[ud],[NIL],true)) => NIL
        mapMm := CDAAR mapMms
        selectMms1(op,tar,[['Mapping,:mapMm],CADR args1],
          [NIL,CADR args2],$Coerce)

    $Coerce and (op = 'map) and (2 = nargs) and
      (first(args1) is ['FunctionCalled,fun]) =>
        null (ud := underDomainOf CADR args1) => NIL
        if tar then ut := underDomainOf(tar)
        else ut := nil
        funNode := mkAtreeNode fun
        transferPropsToNode(fun,funNode)
        null (mapMms := selectLocalMms(funNode,fun,[ud],NIL)) => NIL
        mapMm := CDAAR mapMms
        selectMms1(op,tar,[['Mapping,:mapMm],CADR args1],
          [NIL,CADR args2],$Coerce)

    -- get the argument domains and the target
    a := nil
    for x in args1 repeat if x then a := cons(x,a)
    for x in args2 repeat if x then a := cons(x,a)
    if tar and not isPartialMode tar then a := cons(tar,a)

    -- for typically homogeneous functions, throw in resolve too
    if op in '(_= _+ _* _- ) then
      r := resolveTypeList a
      if r ~= nil then a := cons(r,a)

    if tar and not isPartialMode tar then
      if xx := underDomainOf(tar) then a := cons(xx,a)
    for x in args1 repeat
      PAIRP(x) and first(x) in '(List Vector Stream FiniteSet Array) =>
        xx := underDomainOf(x) => a := cons(xx,a)

    -- now extend this list with those from the arguments to
    -- any Unions, Mapping or Records

    a' := nil
    a := nreverse REMDUP a
    for x in a repeat
      null x => 'iterate
      x is ['Union,:l] =>
        -- check if we have a tagged union
        l and first l is [":",:.] =>
          for [.,.,t] in l repeat
            a' := cons(t,a')
        a' := append(reverse l,a')
      x is ['Mapping,:l] => a' := append(reverse l,a')
      x is ['Record,:l] =>
        a' := append(reverse [CADDR s for s in l],a')
      x is ['FunctionCalled,name] =>
        (xm := get(name,'mode,$e)) and not isPartialMode xm =>
          a' := cons(xm,a')
    a := append(a,REMDUP a')
    a := [x for x in a | PAIRP(x)]

    -- step 1. see if we have one without coercing
    a' := a
    while a repeat
      x := first a
      a := rest a
      ATOM x => 'iterate
      mmS := append(mmS, findFunctionInDomain(op,x,tar,args1,args2,NIL,NIL))

    -- step 2. if we didn't get one, trying coercing (if we are
    --         suppose to)

    if null(mmS) and $Coerce then
      a := a'
      while a repeat
        x := first a
        a := rest a
        ATOM x => 'iterate
        mmS := append(mmS,
          findFunctionInDomain(op,x,tar,args1,args2,$Coerce,NIL))

    mmS or selectMmsGen(op,tar,args1,args2)
  mmS and orderMms(op, mmS,args1,args2,tar)

isAVariableType t ==
    t is ['Variable,.] or t = $Symbol or t is ['OrderedVariableList,.]

defaultTarget(opNode,op,nargs,args) ==
  -- this is for efficiency. Chooses standard targets for operations
  -- when no target exists.

  target := nil

  nargs = 0 =>
    op = 'nil =>
      putTarget(opNode, target := '(List (None)))
      target
    op = 'true  or op = 'false =>
      putTarget(opNode, target := $Boolean)
      target
    op = 'pi =>
      putTarget(opNode, target := ['Pi])
      target
    op = 'infinity =>
      putTarget(opNode, target := ['OnePointCompletion, $Integer])
      target
    member(op, '(plusInfinity minusInfinity)) =>
      putTarget(opNode, target := ['OrderedCompletion, $Integer])
      target
    target

  a1 := first args
  ATOM a1 => target
  a1f := QCAR a1

  nargs = 1 =>
    op = 'kernel =>
      putTarget(opNode, target := ['Kernel, ['Expression, $Integer]])
      target
    op = 'list =>
      putTarget(opNode, target := ['List, a1])
      target
    target

  a2 := CADR args

  nargs >= 2 and op = "draw" and a1 is ['FunctionCalled,sym] and a2 is ['Segment,.] =>

    -- this clears up some confusion over 2D and 3D graphics

    symNode := mkAtreeNode sym
    transferPropsToNode(sym,symNode)

    nargs >= 3 and CADDR args is ['Segment,.] =>
      selectLocalMms(symNode,sym,[$DoubleFloat, $DoubleFloat],NIL)
      putTarget(opNode, target := '(ThreeDimensionalViewport))
      target

    (mms := selectLocalMms(symNode,sym,[$DoubleFloat],NIL)) =>
      [.,targ,:.] := CAAR mms
      targ = $DoubleFloat =>
          putTarget(opNode, target := '(TwoDimensionalViewport))
          target
      targ = ['Point, $DoubleFloat] =>
          putTarget(opNode, target := '(ThreeDimensionalViewport))
          target
      target

    target

  nargs >= 2 and op = "makeObject" and a1 is ['FunctionCalled,sym] and a2 is ['Segment,.] =>
    -- we won't actually bother to put a target on makeObject
    -- this is just to figure out what the first arg is
    symNode := mkAtreeNode sym
    transferPropsToNode(sym,symNode)

    nargs >= 3 and CADDR args is ['Segment,.] =>
      selectLocalMms(symNode,sym,[$DoubleFloat, $DoubleFloat],NIL)
      target

    selectLocalMms(symNode,sym,[$DoubleFloat],NIL)
    target

  nargs = 2 =>
    op = "elt" =>
        a1 = '(BasicOperator) and a2 is ['List, ['OrderedVariableList, .]] =>
           ['Expression, $Integer]
        target

    op = "eval" =>
        a1 is ['Expression,b1] and a2 is ['Equation, ['Polynomial,b2]] =>
            target :=
              canCoerce(b2, a1) => a1
              t := resolveTT(b1, b2)
              (not t) or (t = $Any) => nil
              resolveTT(a1, t)
            if target then putTarget(opNode, target)
            target
        a1 is ['Equation, .] and a2 is ['Equation, .] =>
            target := resolveTT(a1, a2)
            if target and not (target = $Any) then putTarget(opNode,target)
            else target := nil
            target
        a1 is ['Equation, .] and a2 is ['List, a2e] and a2e is ['Equation, .] =>
            target := resolveTT(a1, a2e)
            if target and not (target = $Any) then putTarget(opNode,target)
            else target := nil
            target
        a2 is ['Equation, a2e] or a2 is ['List, ['Equation, a2e]] =>
            target := resolveTT(a1, a2e)
            if target and not (target = $Any) then putTarget(opNode,target)
            else target := nil
            target

    op = "**" or op = "^" =>
      a2 = $Integer =>
        if (target := resolveTCat(a1,'(Field))) then
          putTarget(opNode,target)
        target
      a1 = '(AlgebraicNumber) and (a2 = $Float or a2 = $DoubleFloat) =>
          target := ['Expression, a2]
          putTarget(opNode,target)
          target
      a1 = '(AlgebraicNumber) and a2 is ['Complex, a3] and (a3 = $Float or a3 = $DoubleFloat) =>
          target := ['Expression, a3]
          putTarget(opNode,target)
          target
      ((a2 = $RationalNumber) and
        (typeIsASmallInteger(a1) or isEqualOrSubDomain(a1,$Integer))) =>
            putTarget(opNode, target := '(AlgebraicNumber))
            target
      ((a2 = $RationalNumber) and (isAVariableType(a1)
          or a1 is ['Polynomial, .])) =>
            putTarget(opNode, target := defaultTargetFE a1)
            target
      isAVariableType(a1) and (a2 = $PositiveInteger or a2 = $NonNegativeInteger) =>
          putTarget(opNode, target := '(Polynomial (Integer)))
          target
      isAVariableType(a2) =>
        putTarget(opNode, target := defaultTargetFE a1)
        target
      a2 is ['Polynomial, D] =>
        (a1 = a2) or isAVariableType(a1)
         or (a1 = D)
          or ((a1 is [=$QuotientField, D1]) and (D1 = a1)) =>
            putTarget(opNode, target := defaultTargetFE a2)
            target
        target
      target

    op = '_/ =>
      isEqualOrSubDomain(a1, $Integer) and isEqualOrSubDomain(a2, $Integer) =>
        putTarget(opNode, target := $RationalNumber)
        target
      a1 = a2 =>
        if (target := resolveTCat(first args, '(Field))) then
          putTarget(opNode,target)
        target
      a1 is ['Variable,.] and a2 is ['Variable,.] =>
        putTarget(opNode,target := mkRationalFunction  '(Integer))
        target
      isEqualOrSubDomain(a1,$Integer) and a2 is ['Variable,.] =>
        putTarget(opNode,target := mkRationalFunction '(Integer))
        target
      a1 is ['Variable,.] and
        a2 is ['Polynomial,D] =>
          putTarget(opNode,target := mkRationalFunction D)
          target
        target
      a2 is ['Variable,.] and
        a1 is ['Polynomial,D] =>
          putTarget(opNode,target := mkRationalFunction D)
          target
        target
      a2 is ['Polynomial,D] and (a1 = D) =>
        putTarget(opNode,target := mkRationalFunction D)
        target
      target

  a3 := CADDR args
  nargs = 3 =>
    op = "eval" =>
        a3 is ['List, a3e] =>
            target := resolveTT(a1, a3e)
            if not (target = $Any) then putTarget(opNode,target)
            else target := nil
            target

        target := resolveTT(a1, a3)
        if not (target = $Any) then putTarget(opNode,target)
        else target := nil
        target
  target

mkRationalFunction D ==  ['Fraction, ['Polynomial, D]]

defaultTargetFE(a,:options) ==
  a is ['Variable,.] or a = $RationalNumber or MEMQ(QCAR a,
    [QCAR $Symbol,
     'Pi]) or typeIsASmallInteger(a) or isEqualOrSubDomain(a, $Integer) or
       a = '(AlgebraicNumber) =>
          IFCAR options => [$FunctionalExpression, ['Complex, $Integer]]
          [$FunctionalExpression, $Integer]
  a is ['Complex,uD] => defaultTargetFE(uD, true)
  a is [D, uD] and MEMQ(D, '(Polynomial Fraction)) =>
     defaultTargetFE(uD, IFCAR options)
  a is [=$FunctionalExpression,.] => a
  IFCAR options => [$FunctionalExpression, ['Complex, a]]
  [$FunctionalExpression, a]

altTypeOf(type,val,$declaredMode) ==
  (EQCAR(type,'Symbol) or EQCAR(type,'Variable)) and
    (a := getMinimalVarMode(objValUnwrap getValue(val),$declaredMode)) =>
      a
  type is ['OrderedVariableList,vl] and
    INTEGERP(val1 := objValUnwrap getValue(val)) and
      (a := getMinimalVarMode(vl.(val1 - 1),$declaredMode)) =>
        a
  type = $PositiveInteger    => $Integer
  type = $NonNegativeInteger => $Integer
  type = '(List (PositiveInteger)) => '(List (Integer))
  NIL

getOpArgTypes(opname, args) ==
  l := getOpArgTypes1(opname, args)
  [f(a,opname) for a in l] where
    f(x,op) ==
      x is ['FunctionCalled,g] and op ~= 'name =>
        m := get(g,'mode,$e) =>
          m is ['Mapping,:.] => m
          x
        x
      x

getOpArgTypes1(opname, args) ==
  null args => NIL
  -- special cases first
  opname = 'coef and args is [b,n] =>
    [first getModeSet b, first getModeSetUseSubdomain n]
  opname = 'monom and args is [d,c] =>
    [first getModeSetUseSubdomain d, first getModeSet c]
  opname = 'monom and args is [v,d,c] =>
    [first getModeSet v, first getModeSetUseSubdomain d, first getModeSet c]
  (opname = 'cons) and (2 = #args) and (CADR(args) = 'nil) =>
    ms := [first getModeSet x for x in args]
    if CADR(ms) = '(List (None)) then
      ms := [first ms,['List,first ms]]
    ms
  nargs := #args
  v := argCouldBelongToSubdomain(opname,nargs)
  mss := NIL
  for i in 0..(nargs-1) for x in args repeat
    ms :=
        v.i = 0 => first getModeSet x
        first getModeSetUseSubdomain x
    mss := [ms,:mss]
  nreverse mss

argCouldBelongToSubdomain(op, nargs) ==
  -- this returns a vector containing 0 or ^0 for each argument.
  -- if ^0, this indicates that there exists a modemap for the
  -- op that needs a subdomain in that position
  nargs = 0 => NIL
  v := GETZEROVEC nargs
  isMap(op) => v
  mms := getModemapsFromDatabase(op,nargs)
  null mms => v
  nargs:=nargs-1
  -- each signature has form
  -- [domain of implementation, target, arg1, arg2, ...]
  for [sig,cond,:.] in mms repeat
    for t in CDDR sig for i in 0..(nargs) repeat
      CONTAINEDisDomain(t,cond) =>
          v.i := 1 + v.i
  v

CONTAINEDisDomain(symbol,cond) ==
-- looks for [isSubDomain,symbol,[domain]] in cond: returning T or NIL
-- with domain being one of PositiveInteger and NonNegativeInteger
   ATOM cond => false
   MEMQ(QCAR cond,'(AND OR and or)) =>
       or/[CONTAINEDisDomain(symbol, u) for u in QCDR cond]
   EQ(QCAR cond,'isDomain) =>
       EQ(symbol,CADR cond) and PAIRP(dom:=CADDR cond) and
         MEMQ(dom,'(PositiveInteger NonNegativeInteger))
   false

selectDollarMms(dc,name,types1,types2) ==
  -- finds functions for name in domain dc
  isPartialMode dc => throwKeyedMsg("S2IF0001",NIL)
  mmS := findFunctionInDomain(name,dc,NIL,types1,types2,'T,'T) =>
    orderMms(name, mmS,types1,types2,NIL)
  if $reportBottomUpFlag then sayMSG
    ["%b",'"          function not found in ",prefix2String dc,"%d","%l"]
  NIL

selectLocalMms(op,name,types,tar) ==
  -- partial rewrite, looks now for exact local modemap
  mmS:= getLocalMms(name,types,tar) => mmS
  obj := getValue op
  obj and (objVal obj is ['SPADMAP, :mapDef]) and
    analyzeMap(op,types,mapDef,tar) and getLocalMms(name,types,tar)

-- next defn may be better, test when more time. RSS 3/11/94
-- selectLocalMms(op,name,types,tar) ==
--  mmS := getLocalMms(name,types,tar)
--  -- if no target, just return what we got
--  mmS and null tar => mmS
--  matchingMms := nil
--  for mm in mmS repeat
--    [., targ, :.] := mm
--    if tar = targ then matchingMms := cons(mm, matchingMms)
--  -- if we got some exact matchs on the target, return them
--  matchingMms => nreverse matchingMms
--
--  obj := getValue op
--  obj and (objVal obj is ['SPADMAP, :mapDef]) and
--    analyzeMap(op,types,mapDef,tar) and getLocalMms(name,types,tar)

getLocalMms(name,types,tar) ==
  -- looks for exact or subsumed local modemap in $e
  mmS := NIL
  for  (mm:=[dcSig,:.]) in get(name,'localModemap,$e) repeat
    -- check format and destructure
    dcSig isnt [dc,result,:args] => NIL
    -- make number of args is correct
    #types ~= #args => NIL
    -- check for equal or subsumed arguments
    subsume := (not $useIntegerSubdomain) or (tar = result) or
      get(name,'recursive,$e)
    acceptableArgs :=
      and/[f(b,a,subsume) for a in args for b in types] where
        f(x,y,subsume) ==
          if subsume
            then isEqualOrSubDomain(x,y)
            else x = y
    not acceptableArgs =>
      -- interpreted maps are ok
      dc = 'interpOnly and not($Coerce)=> mmS := [mm,:mmS]
      NIL
    mmS := [mm,:mmS]
  nreverse mmS

-- Helper to avoid bad coercions (SF 2974970). See
--
-- http://groups.google.com/group/fricas-devel/browse_thread/thread/a93abc242431a6bc?hl=en#
--
-- for more info.
isApproximate(t) ==
    op := first(t)
    member(op, ["Float", "DoubleFloat"]) => true
    member(op, ["Complex", "Expression", "List", "Polynomial",
                "Matrix", "Vector"]) => isApproximate(first(rest(t)))
    false

mmCost(name, sig,cond,tar,args1,args2) ==
  cost := mmCost0(name, sig,cond,tar,args1,args2)
  res := CADR sig
  res = $PositiveInteger => cost - 2
  res = $NonNegativeInteger => cost - 1
  res = $DoubleFloat => cost + 1
  cost

mmCost0(name, sig,cond,tar,args1,args2) ==
  sigArgs := CDDR sig
  n:=
    null cond => 1
    not (or/cond) => 1
    0

  -- try to favor homogeneous multiplication

--if name = "*" and 2 = #sigArgs and first sigArgs ~= first rest sigArgs then n := n + 1

  -- because of obscure problem in evalMm, sometimes we will have extra
  -- modemaps with the wrong number of arguments if we want to the one
  -- with no arguments and the name is overloaded. Thus check for this.

  nargs := #args1

  if args1 then
    for x1 in args1 for x2 in args2 for x3 in sigArgs repeat
      n := n +
        isEqualOrSubDomain(x1,x3) => 0
        topcon := first deconstructT x1
        topcon2 := first deconstructT x3
        topcon = topcon2 => 3
        first topcon2 = 'Mapping => 2
        4
      if isApproximate(x1) ~= isApproximate(x3) then
          n := n + 10*nargs
  else if sigArgs then n := n + 100000000000

  res := CADR sig
  res=tar => 10000*n
  10000*n + 1000*domainDepth(res) + hitListOfTarget(res)

orderMms(name, mmS,args1,args2,tar) ==
  -- it counts the number of necessary coercions of the argument types
  -- if this isn't enough, it compares the target types
  mmS and null rest mmS => mmS
  mS:= NIL
  N:= NIL
  for mm in MSORT mmS repeat
    [sig,.,cond]:= mm
    b:= 'T
    p:= CONS(m := mmCost(name, sig,cond,tar,args1,args2),mm)
    mS:=
      null mS => list p
      m < CAAR mS => CONS(p,mS)
      S:= mS
      until b repeat
        b := null rest S or m < CAADR S =>
          RPLACD(S, CONS(p, rest S))
        S := rest S
      mS
  mmS and [rest p for p in mS]

domainDepth(d) ==
  -- computes the depth of lisp structure d
  atom d => 0
  MAX(domainDepth(first d) + 1, domainDepth(rest d))

hitListOfTarget(t) ==
  -- assigns a number between 1 and 998 to a type t

  -- want to make it hard to go to Polynomial Pi

  t = '(Polynomial (Pi)) => 90000

  EQ(first t, 'Polynomial) => 300
  EQ(first t, 'List) => 400
  EQ(first t, 'Matrix) => 910
  EQ(first t, 'UniversalSegment) => 501
  EQ(first t, 'Union) => 999
  EQ(first t, 'Expression) => 1600
  500

isOpInDomain(opName,dom,nargs) ==
  -- returns true only if there is an op in the given domain with
  -- the given number of arguments
  mmList := ASSQ(opName, getOperationAlistFromLisplib first dom)
  mmList := subCopy(mmList,constructSubst dom)
  null mmList => NIL
  gotOne := NIL
  nargs := nargs + 1
  for mm in rest mmList while not gotOne repeat
    nargs = #first mm => gotOne := [mm, :gotOne]
  gotOne

findCommonSigInDomain(opName,dom,nargs) ==
  -- this looks at all signatures in dom with given opName and nargs
  -- number of arguments. If no matches, returns NIL. Otherwise returns
  -- a "signature" where a type position is non-NIL only if all
  -- signatures shares that type .
  first(dom) in '(Union Record Mapping) => NIL
  mmList := ASSQ(opName, getOperationAlistFromLisplib first dom)
  mmList := subCopy(mmList,constructSubst dom)
  null mmList => NIL
  gotOne := NIL
  nargs := nargs + 1
  vec := NIL
  for mm in rest mmList repeat
    nargs = #first mm =>
      null vec  => vec := LIST2VEC first mm
      for i in 0.. for x in first mm repeat
        if vec.i and vec.i ~= x then vec.i := NIL
  VEC2LIST vec

findUniqueOpInDomain(op,opName,dom) ==
  -- return function named op in domain dom if unique, choose one if not
  mmList := ASSQ(opName, getOperationAlistFromLisplib first dom)
  mmList := subCopy(mmList,constructSubst dom)
  null mmList =>
    throwKeyedMsg("S2IS0021",[opName,dom])
  mmList := rest mmList   -- ignore the operator name
  -- use evaluation type context to narrow down the candidate set
  if target := getTarget op then
      mmList := [mm for mm in mmList | mm is [=rest target,:.]]
      null mmList => throwKeyedMsg("S2IS0061",[opName,target,dom])
  if #mmList > 1 then
    mm := selectMostGeneralMm mmList
    sayKeyedMsg("S2IS0022", [opName, dom, ['Mapping, :first mm]])
  else mm := first mmList
  [sig,slot,:.] := mm
  fun :=
--+
      $genValue =>
         compiledLookupCheck(opName,sig,evalDomain dom)
      NRTcompileEvalForm(opName, sig, evalDomain dom)
  NULL(fun) or NULL(PAIRP(fun)) => NIL
  first fun = function(Undef) => throwKeyedMsg("S2IS0023", [opName, dom])
  binVal :=
    $genValue => wrap fun
    fun
  putValue(op,objNew(binVal,m:=['Mapping,:sig]))
  putModeSet(op,[m])

selectMostGeneralMm mmList ==
  -- selects the modemap in mmList with arguments all the other
  -- argument types can be coerced to
  -- also selects function with #args closest to 2
  min := 100
  mml := mmList
  while mml repeat
    [mm,:mml] := mml
    sz := #first mm
    if (met := ABS(sz - 3)) < min then
      min := met
      fsz := sz
  mmList := [mm for mm in mmList | (#first mm) = fsz]
  mml := rest mmList
  genMm := first mmList
  while mml repeat
    [mm,:mml] := mml
    and/[canCoerceFrom(genMmArg,mmArg) for mmArg in CDAR mm
      for genMmArg in CDAR genMm] => genMm := mm
  genMm

findFunctionInDomain(op,dc,tar,args1,args2,$Coerce,$SubDom) ==
  -- looks for a modemap for op with signature  args1 -> tar
  --   in the domain of computation dc
  -- tar may be NIL (= unknown)
  null isLegitimateMode(tar, nil, nil) => nil
  dcName := first dc
  member(dcName,'(Union Record Mapping Enumeration)) =>
    -- First cut code that ignores args2, $Coerce and $SubDom
    -- When domains no longer have to have Set, the hard coded 6 and 7
    -- should go.
    op = '_= =>
        #args1 ~= 2 or args1.0 ~= dc or args1.1 ~= dc => NIL
        tar and tar ~= '(Boolean) => NIL
        [[[dc, '(Boolean), dc, dc], ['(Boolean),'$,'$], [NIL, NIL]]]
    op = 'coerce =>
        dcName='Enumeration and (args1.0=$Symbol or tar=dc)=>
           [[[dc, dc, $Symbol], ['$,$Symbol], [NIL, NIL]]]
        args1.0 ~= dc => NIL
        tar and tar ~= $OutputForm => NIL
        [[[dc, $OutputForm, dc], [$OutputForm, '$], [NIL, NIL]]]
    member(dcName,'(Record Union)) =>
      findFunctionInCategory(op,dc,tar,args1,args2,$Coerce,$SubDom)
    NIL
  fun:= NIL
  ( p := ASSQ(op,getOperationAlistFromLisplib dcName) ) and
    SL := constructSubst dc
    -- if the arglist is homogeneous, first look for homogeneous
    -- functions. If we don't find any, look at remaining ones
    if isHomogeneousList args1 then
      q := NIL
      r := NIL
      for mm in rest p repeat
        -- CDAR of mm is the signature argument list
        if isHomogeneousList CDAR mm then q := [mm,:q]
        else r := [mm,:r]
      q := allOrMatchingMms(q,args1,tar,dc)
      for mm in q repeat
        fun:= nconc(fun,findFunctionInDomain1(mm,op,tar,args1,args2,SL))
      r := reverse r
    else r := rest p
    r := allOrMatchingMms(r,args1,tar,dc)
    if not fun then    -- consider remaining modemaps
      for mm in r repeat
        fun:= nconc(fun,findFunctionInDomain1(mm,op,tar,args1,args2,SL))
  if not fun and $reportBottomUpFlag then
    sayMSG concat
      ['"   -> no appropriate",:bright op,'"found in",
        :bright prefix2String dc]
  fun

allOrMatchingMms(mms,args1,tar,dc) ==
  -- if there are exact matches on the arg types, return them
  -- otherwise return the original list
  null mms or null rest mms => mms
  x := NIL
  for mm in mms repeat
    [sig,:.] := mm
    [res,:args] := MSUBSTQ(dc,"$",sig)
    args ~= args1 => nil
    x := CONS(mm,x)
  if x then x
  else mms

isHomogeneousList y ==
  y is [x] => true
  y and rest y =>
    z := first y
    "and"/[x = z for x in rest y]
  NIL

findFunctionInDomain1(omm,op,tar,args1,args2,SL) ==
  dc := rest (dollarPair := ASSQ('$, SL))
  -- need to drop '$ from SL
  mm:= subCopy(omm, SL)
  -- tests whether modemap mm is appropriate for the function
  -- defined by op, target type tar and argument types args
  $RTC:local:= NIL
  -- $RTC is a list of run-time checks to be performed

  [sig,slot,cond,y] := mm
  [osig,:.]  := omm
  osig := subCopy(osig, SUBSTQ(CONS('$,'$), dollarPair, SL))
  if CONTAINED('_#, sig) or CONTAINED('construct, sig) then
    sig := [replaceSharpCalls t for t in sig]
  matchMmCond cond and matchMmSig(mm,tar,args1,args2) and
    EQ(y,'Subsumed) and
      -- hmmmm: do Union check in following because (as in DP)
      -- Unions are subsumed by total modemaps which are in the
      -- mm list in findFunctionInDomain.
      y := 'ELT      -- if subsumed fails try it again
      not $SubDom and first sig isnt ['Union, :.] and slot is [tar, :args] and
        (f := findFunctionInDomain(op,dc,tar,args,args,NIL,NIL)) => f
    EQ(y,'ELT) => [[CONS(dc,sig),osig,nreverse $RTC]]
    EQ(y,'CONST) => [[CONS(dc,sig),osig,nreverse $RTC]]
    EQ(y,'ASCONST) => [[CONS(dc,sig),osig,nreverse $RTC]]
    y is ['XLAM,:.] => [[CONS(dc,sig),y,nreverse $RTC]]
    sayKeyedMsg("S2IF0006",[y])
    NIL

--------------------> NEW DEFINITION (override in xrun.boot.pamphlet)
findFunctionInCategory(op,dc,tar,args1,args2,$Coerce,$SubDom) ==
  -- looks for a modemap for op with signature  args1 -> tar
  --   in the domain of computation dc
  -- tar may be NIL (= unknown)
  dcName := first dc
  not MEMQ(dcName,'(Record Union Enumeration)) => NIL
  fun:= NIL
 --  cat := constructorCategory dc
  makeFunc := GET(dcName, "makeFunctionList") or
      systemErrorHere '"findFunctionInCategory"
  [funlist,.] := FUNCALL(makeFunc,"$",dc,$CategoryFrame)
  -- get list of implementations and remove sharps
  maxargs := -1
  impls := nil
  for [a,b,d] in funlist repeat
    not EQ(a,op) => nil
    d is ['XLAM,xargs,:.] =>
      if PAIRP(xargs) then maxargs := MAX(maxargs,#xargs)
      else maxargs := MAX(maxargs,1)
      impls := cons([b,nil,true,d],impls)
    impls := cons([b,d,true,d],impls)
  impls := NREVERSE impls
  if maxargs ~= -1 then
    SL:= NIL
    for i in 1..maxargs repeat
      impls := SUBSTQ(GENSYM(),INTERNL('"#",STRINGIMAGE i),impls)
  impls and
    SL:= constructSubst dc
    for mm in impls repeat
      fun:= nconc(fun,findFunctionInDomain1(mm,op,tar,args1,args2,SL))
  if not fun and $reportBottomUpFlag then
    sayMSG concat
      ['"   -> no appropriate",:bright op,'"found in",
        :bright prefix2String dc]
  fun

matchMmCond(cond) ==
  -- tests the condition, which comes with a modemap
  -- cond is 'T or a list, but I hate to test for 'T (ALBI)
  $domPvar: local := nil
  atom cond or
    cond is ['AND,:conds] or cond is ['and,:conds] =>
      and/[matchMmCond c for c in conds]
    cond is ['OR,:conds] or cond is ['or,:conds] =>
      or/[matchMmCond c for c in conds]
    cond is ['has,dom,x] =>
      hasCaty(dom,x,NIL) ~= 'failed
    cond is ['not,cond1] => not matchMmCond cond1
    keyedSystemError("S2GE0016",
      ['"matchMmCond",'"unknown form of condition"])

matchMmSig(mm,tar,args1,args2) ==
  -- matches the modemap signature against  args1 -> tar
  -- if necessary, runtime checks are created for subdomains
  -- then the modemap condition is evaluated
  [sig,:.]:= mm
  if CONTAINED('_#, sig) then
    sig := [replaceSharpCalls COPY t for t in sig]
  null args1 => matchMmSigTar(tar, first sig)
  a := rest sig
  arg:= NIL
  for i in 1.. while args1 and args2 and a until not b repeat
    x1 := first args1
    args1 := rest args1
    x2 := first args2
    args2 := rest args2
    x := first a
    a := rest a
    rtc:= NIL
    if x is ['SubDomain,y,:.] then x:= y
    b := isEqualOrSubDomain(x1,x) or
      (STRINGP(x) and (x1 is ['Variable,v]) and (x = PNAME v)) or
        $SubDom and isSubDomain(x,x1) => rtc:= 'T
        $Coerce => x2=x or canCoerceFrom(x1,x)
        x1 is ['Variable,:.] and x = '(Symbol)
    $RTC:= CONS(rtc,$RTC)
  null args1 and null a and b and matchMmSigTar(tar, first sig)

matchMmSigTar(t1,t2) ==
  -- t1 is a target type specified by :: or by a declared variable
  -- t2 is the target of a modemap signature
  null t1 or
    isEqualOrSubDomain(t2,t1) => true
    if t2 is ['Union,a,b] then
      if a='"failed" then return matchMmSigTar(t1, b)
      if b='"failed" then return matchMmSigTar(t1, a)
    $Coerce and
      isPartialMode t1 => resolveTM(t2,t1)
-- I think this should be true  -SCM
--    true
      canCoerceFrom(t2,t1)

constructSubst(d) ==
  -- constructs a substitution which substitutes d for $
  -- and the arguments of d for #1, #2 ..
  SL:= list CONS('$,d)
  for x in rest d for v in $FormalMapVariableList repeat
    SL:= CONS(CONS(v,x),SL)
  SL

filterModemapsFromPackages(mms, names, op) ==
  -- mms is a list of modemaps
  -- names is a list of domain constructors
  -- this returns a 2-list containing those modemaps that have one
  -- of the names in the package source of the modemap and all the
  -- rest of the modemaps in the second element.
  good := NIL
  bad  := NIL
  -- hack to speed up factorization choices for mpolys and to overcome
  -- some poor naming of packages
  mpolys := '("Polynomial" "MultivariatePolynomial"
   "DistributedMultivariatePolynomial"
      "HomogeneousDistributedMultivariatePolynomial")
  mpacks := '("MFactorize" "MRationalFactorize")
  for mm in mms repeat
    isFreeFunctionFromMm(mm) => bad := cons(mm, bad)
    type := getDomainFromMm mm
    null type => bad := cons(mm,bad)
    if PAIRP type then type := first type
    GETDATABASE(type,'CONSTRUCTORKIND) = 'category => bad := cons(mm,bad)
    name := object2String type
    found := nil
    for n in names while not found repeat
      STRPOS(n,name,0,NIL) => found := true
      -- hack, hack
      (op = 'factor) and member(n,mpolys) and member(name,mpacks) =>
        found := true
    if found
      then good := cons(mm, good)
      else bad := cons(mm,bad)
  [good,bad]


isTowerWithSubdomain(towerType,elem) ==
  not PAIRP towerType => NIL
  dt := deconstructT towerType
  2 ~= #dt => NIL
  s := underDomainOf(towerType)
  s = elem => towerType
  isEqualOrSubDomain(s,elem) and constructM(first dt,[elem])

exact?(mmS, tar, args) ==
    ex := inex := NIL
    for (mm := [sig, [mmC, :.], :.]) in mmS repeat
        [c, t, :a] := sig
        ok := true
        for pat in a for arg in args while ok repeat
            not CONTAINED(['isDomain, pat, arg], mmC) => ok := NIL
        ok => ex := CONS(mm, ex)
        inex := CONS(mm, inex)
    [ex, inex]

matchMms(mmaps, op, tar, args1, args2) ==
    mmS := NIL
    for [sig, mmC] in mmaps repeat
        -- sig is [dc, result, :args]
        $Subst :=
            tar and not isPartialMode tar =>
                -- throw in the target if it is not the same as one
                -- of the arguments
                res := CADR sig
                member(res, CDDR sig) => NIL
                [[res, :tar]]
            NIL
        [c, t, :a] := sig
        if a then matchTypes(a, args1, args2)
        not EQ($Subst, 'failed) =>
            mmS := nconc(evalMm(op, tar, sig, mmC), mmS)
    mmS

selectMmsGen(op,tar,args1,args2) ==
  -- general modemap evaluation of op with argument types args1
  -- evaluates the condition and looks for the slot number
  -- returns all functions which are applicable
  -- args2 is a list of polynomial types for symbols
  $Subst: local := NIL
  $SymbolType: local := NIL

  null (S := getModemapsFromDatabase(op, LENGTH args1)) => NIL

  if (op = 'map) and (2 = #args1) and
    (first(args1) is ['Mapping, ., elem]) and
      (a := isTowerWithSubdomain(CADR args1,elem))
        then args1 := [first args1, a]

  -- we first split the modemaps into two groups:
  --   haves:    these are from packages that have one of the top level
  --             constructor names in the package name
  --   havenots: everything else

  -- get top level constructor names for constructors with parameters
  conNames := nil
  if op = 'reshape then args := APPEND(rest args1, rest args2)
  else args := APPEND(args1,args2)
  if tar then args := [tar,:args]
  -- for common aggregates, use under domain also
  for a in REMDUP args repeat
    a =>
      atom a => nil
      fa := QCAR a
      fa in '(Record Union) => NIL
      conNames := insert(STRINGIMAGE fa, conNames)

  if conNames
    then [haves,havenots] := filterModemapsFromPackages(S,conNames,op)
    else
      haves := NIL
      havenots := S

  mmS := NIL

  if $reportBottomUpFlag then
    sayMSG ['%l,:bright '"Modemaps from Associated Packages"]

  if haves then
    [havesExact, havesInexact] := exact?(haves, tar, args1)
    if $reportBottomUpFlag then
      for mm in APPEND(havesExact,havesInexact) for i in 1.. repeat
        sayModemapWithNumber(mm,i)
    if havesExact then
      mmS := matchMms(havesExact, op, tar, args1, args2)
      if mmS then
        if $reportBottomUpFlag then
          sayMSG '"   found an exact match!"
        return mmS
    mmS := matchMms(havesInexact,op,tar,args1,args2)
  else if $reportBottomUpFlag then sayMSG '"   no modemaps"
  mmS => mmS

  if $reportBottomUpFlag then
    sayMSG ['%l,:bright '"Remaining General Modemaps"]
  --  for mm in havenots for i in 1.. repeat sayModemapWithNumber(mm,i)

  if havenots then
    [havesNExact,havesNInexact] := exact?(havenots,tar,args1)
    if $reportBottomUpFlag then
      for mm in APPEND(havesNExact,havesNInexact) for i in 1.. repeat
        sayModemapWithNumber(mm,i)
    if havesNExact then
      mmS := matchMms(havesNExact,op,tar,args1,args2)
      if mmS then
        if $reportBottomUpFlag then
          sayMSG '"   found an exact match!"
        return mmS
    mmS := matchMms(havesNInexact,op,tar,args1,args2)
  else if $reportBottomUpFlag then sayMSG '"   no modemaps"
  mmS

matchTypes(pm,args1,args2) ==
  -- pm is a list of pattern variables, args1 a list of argument types,
  --   args2 a list of polynomial types for symbols
  -- the result is a match from pm to args, if one exists
  for v in pm for t1 in args1 for t2 in args2 until $Subst='failed repeat
    p:= ASSQ(v,$Subst) =>
      t := rest p
      t=t1 => $Coerce and EQCAR(t1,'Symbol) and
        (q := ASSQ(v,$SymbolType)) and t2 and
          (t3 := resolveTT(rest q, t2)) and
            RPLACD(q, t3)
      $Coerce =>
        if EQCAR(t,'Symbol) and (q := ASSQ(v,$SymbolType)) then
          t := rest q
        if EQCAR(t1,'Symbol) and t2 then t1:= t2
        t0 := resolveTT(t,t1) => RPLACD(p,t0)
        $Subst:= 'failed
      $Subst:= 'failed
    $Subst:= CONS(CONS(v,t1),$Subst)
    if EQCAR(t1,'Symbol) and t2 then $SymbolType:= CONS(CONS(v,t2),$SymbolType)

evalMm(op,tar,sig,mmC) ==
  -- evaluates a modemap with signature sig and condition mmC
  -- the result is a list of lists [sig,slot,cond] or NIL
  --if $Coerce is NIL, tar has to be the same as the computed target type
  mS:= NIL
  for st in evalMmStack mmC repeat
    SL:= evalMmCond(op,sig,st)
    not EQ(SL,'failed) =>
      SL := fixUpTypeArgs SL
      sig:= [subCopy(deepSubCopy(x,SL),$Subst) for x in sig]
      not containsVars sig =>
        isFreeFunctionFromMmCond mmC and (m := evalMmFreeFunction(op,tar,sig,mmC)) =>
           mS:= nconc(m,mS)
        "or"/[not isValidType(arg) for arg in sig] => nil
        [dc,t,:args]:= sig
        $Coerce or null tar or tar=t =>
          mS:= nconc(findFunctionInDomain(op,dc,t,args,args,NIL,'T),mS)
  mS

evalMmFreeFunction(op,tar,sig,mmC) ==
  [dc,t,:args]:= sig
  $Coerce or null tar or tar=t =>
     nilArgs := nil
     for a in args repeat nilArgs := [NIL,:nilArgs]
     [[[["__FreeFunction__",:dc],t,:args], [t, :args], nilArgs]]
  nil

evalMmStack(mmC) ==
  -- translates the modemap condition mmC into a list of stacks
  mmC is ['AND,:a] =>
    ["NCONC"/[evalMmStackInner cond for cond in a]]
  mmC is ['OR,:args] => [:evalMmStack a for a in args]
  mmC is ['partial,:mmD] => evalMmStack mmD
  mmC is ['ofCategory,pvar,cat] and cat is ['Join,:args] =>
    evalMmStack CONS('AND,[['ofCategory,pvar,c] for c in args])
  mmC is ['ofType,:.] => [NIL]
  mmC is ['has,pat,x] =>
    x = 'ATTRIBUTE => BREAK()
    x = 'SIGNATURE =>
      [[['ofCategory,pat,['CATEGORY,'unknown,x]]]]
    [['ofCategory,pat,x]]
  [[mmC]]

evalMmStackInner(mmC) ==
  mmC is ['OR,:args] =>
    keyedSystemError("S2GE0016",
      ['"evalMmStackInner",'"OR condition nested inside an AND"])
  mmC is ['partial,:mmD] => evalMmStackInner mmD
  mmC is ['ofCategory,pvar,cat] and cat is ['Join,:args] =>
    [['ofCategory, pvar, c] for c in args]
  mmC is ['ofType,:.] => NIL
  mmC is ['isAsConstant] => NIL
  mmC is ['has,pat,x] =>
    x = 'ATTRIBUTE => BREAK()
    x = 'SIGNATURE =>
      [['ofCategory,pat,['CATEGORY,'unknown,x]]]
    [['ofCategory,pat,x]]
  [mmC]

evalMmCond(op,sig,st) ==
  $insideEvalMmCondIfTrue : local := true
  evalMmCond0(op,sig,st)

evalMmCond0(op,sig,st) ==
  -- evaluates the nonempty list of modemap conditions st
  -- the result is either 'failed or a substitution list
  SL:= evalMmDom st
  SL='failed => 'failed
  for p in SL until p1 and not b repeat b:=
    p1 := ASSQ(first p, $Subst)
    p1 and
      t1 := rest p1
      t := rest p
      t=t1 or
        containsVars t =>
          if $Coerce and EQCAR(t1, 'Symbol) then t1 := getSymbolType first p
          resolveTM1(t1,t)
        $Coerce and
          -- if we are looking at the result of a function, the coerce
          -- goes the opposite direction
          (t1 = $AnonymousFunction and t is ['Mapping, :.]) => t
          first p = CADR sig and not member(first p, CDDR sig) =>
            canCoerceFrom(t,t1) => 'T
            NIL
          canCoerceFrom(t1,t) => 'T
          isSubDomain(t,t1) => RPLACD(p,t1)
          EQCAR(t1, 'Symbol) and canCoerceFrom(getSymbolType first p, t)
  ( SL and p1 and not b and 'failed ) or evalMmCat(op,sig,st,SL)

fixUpTypeArgs SL ==
  for (p := [v, :t2]) in SL repeat
    t1 := LASSOC(v, $Subst)
    null t1 => RPLACD(p,replaceSharpCalls t2)
    RPLACD(p, coerceTypeArgs(t1, t2, SL))
  SL

replaceSharpCalls t ==
  noSharpCallsHere t => t
  doReplaceSharpCalls t

doReplaceSharpCalls t ==
  ATOM t => t
  t is ['_#, l] => #l
  t is ['construct,: l] => EVAL ['LIST,:l]
  [first t, :[doReplaceSharpCalls u for u in rest t]]

noSharpCallsHere t ==
  t isnt [con, :args] => true
  MEMQ(con,'(construct _#)) => NIL
  and/[noSharpCallsHere u for u in args]

coerceTypeArgs(t1, t2, SL) ==
  -- if the type t has type-valued arguments, coerce them to the new types,
  -- if needed.
  t1 isnt [con1, :args1] or t2 isnt [con2, :args2] => t2
  con1 ~= con2 => t2
  coSig := rest GETDATABASE(first t1, 'COSIG)
  and/coSig => t2
  csub1 := constructSubst t1
  csub2 := constructSubst t2
  cs1 := rest getConstructorSignature con1
  cs2 := rest getConstructorSignature con2
  [con1, :
    [makeConstrArg(arg1, arg2, constrArg(c1,csub1,SL),
      constrArg(c2,csub2,SL), cs)
       for arg1 in args1 for arg2 in args2 for c1 in cs1 for c2 in cs2
         for cs in coSig]]

constrArg(v,sl,SL) ==
  x := LASSOC(v,sl) =>
    y := LASSOC(x,SL) => y
    y := LASSOC(x, $Subst) => y
    x
  y := LASSOC(x, $Subst) => y
  v

makeConstrArg(arg1, arg2, t1, t2, cs) ==
  if arg1 is ['_#, l] then arg1 := # l
  if arg2 is ['_#, l] then arg2 := # l
  cs => arg2
  t1 = t2 => arg2
  obj1 := objNewWrap(arg1, t1)
  obj2 := coerceInt(obj1, t2)
  null obj2 => throwKeyedMsgCannotCoerceWithValue(wrap arg1,t1,t2)
  objValUnwrap obj2

evalMmDom(st) ==
  -- evals all isDomain(v,d) of st
  SL:= NIL
  for mmC in st until SL='failed repeat
    mmC is ['isDomain,v,d] =>
      STRINGP d => SL:= 'failed
      p := ASSQ(v, SL) and not (d = rest p) => SL := 'failed
      d1:= subCopy(d,SL)
      CONSP(d1) and MEMQ(v,d1) => SL:= 'failed
      SL:= augmentSub(v,d1,SL)
    mmC is ['isFreeFunction,v,fun] =>
      SL:= augmentSub(v,subCopy(fun,SL),SL)
  SL

orderMmCatStack st ==
  -- tries to reorder stack so that free pattern variables appear
  -- as parameters first
  null(st) or null rest(st) => st
  vars := DELETE_-DUPLICATES [CADR(s) for s in st | isPatternVar(CADR(s))]
  null vars => st
  havevars := nil
  haventvars := nil
  for s in st repeat
    cat := CADDR s
    mem := nil
    for v in vars while not mem repeat
      if MEMQ(v,cat) then
        mem := true
        havevars := cons(s,havevars)
    if not mem then haventvars := cons(s,haventvars)
  null havevars => st
  st := nreverse nconc(haventvars,havevars)
  SORT(st, function mmCatComp)

mmCatComp(c1, c2) ==
  b1 := ASSQ(CADR c1, $Subst)
  b2 := ASSQ(CADR c2, $Subst)
  b1 and null(b2) => true
  false

evalMmCat(op,sig,stack,SL) ==
  -- evaluates all ofCategory's of stack as soon as possible
  $hope:local:= NIL
  numConds:= #stack
  stack:= orderMmCatStack [mmC for mmC in stack | EQCAR(mmC,'ofCategory)]
  while stack until not makingProgress repeat
    st := stack
    stack := NIL
    makingProgress := NIL
    for mmC in st repeat
      S:= evalMmCat1(mmC,op, SL)
      S='failed and $hope =>
        stack:= CONS(mmC,stack)
      S = 'failed => return S
      not atom S =>
        makingProgress:= 'T
        SL:= mergeSubs(S,SL)
  if stack or S='failed then 'failed else SL

evalMmCat1(mmC is ['ofCategory,d,c],op, SL) ==
  -- evaluates mmC using information from the lisplib
  -- d may contain variables, and the substitution list $Subst is used
  -- the result is a substitution or failed
  $domPvar: local := NIL
  $hope:= NIL
  NSL:= hasCate(d,c,SL)
  NSL='failed and isPatternVar d and $Coerce and ( p:= ASSQ(d,$Subst) )
    and (EQCAR(rest p, 'Variable) or EQCAR(rest p, 'Symbol)) =>
      RPLACD(p,getSymbolType d)
      hasCate(d,c,SL)
  NSL='failed and isPatternVar d =>
    -- following is hack to take care of the case where we have a
    -- free substitution variable with a category condition on it.
    -- This would arise, for example, where a package has an argument
    -- that is not in a needed modemap.  After making the following
    -- dummy substitutions, the package can be instantiated and the
    -- modemap used.       RSS 12-22-85
    -- If c is not Set, Ring or Field then the more general mechanism
    dom := defaultTypeForCategory(c, SL)
    null dom =>
      op ~= 'coerce => 'failed -- evalMmCatLastChance(d,c,SL)
    null (p := ASSQ(d,$Subst)) =>
      dom =>
        NSL := [CONS(d,dom)]
      op ~= 'coerce => 'failed -- evalMmCatLastChance(d,c,SL)
    if containsVars dom then dom := resolveTM(rest p, dom)
    $Coerce and canCoerce(rest p, dom) =>
      NSL := [CONS(d,dom)]
    op ~= 'coerce => 'failed -- evalMmCatLastChance(d,c,SL)
  NSL

hasCate(dom,cat,SL) ==
  -- asks whether dom has cat under SL
  -- augments substitution SL or returns 'failed
  dom = $EmptyMode => NIL
  isPatternVar dom =>
    (p := ASSQ(dom, SL)) and ((NSL := hasCate(rest p, cat, SL)) ~= 'failed) =>
       NSL
    (p:= ASSQ(dom,$Subst)) or (p := ASSQ(dom, SL)) =>
--      S := hasCate(rest p, cat, augmentSub(first p, rest p, copy SL))
      S := hasCate1(rest p, cat, SL, dom)
      not (S='failed) => S
      hasCateSpecial(dom, rest p, cat, SL)
    if SL ~= 'failed then $hope:= 'T
    'failed
  SL1 := [[v,:d] for [v,:d] in SL | not containsVariables d]
  if SL1 then cat := subCopy(cat, SL1)
  hasCaty(dom,cat,SL)

hasCate1(dom, cat, SL, domPvar) ==
  $domPvar:local := domPvar
  hasCate(dom, cat, SL)

hasCateSpecial(v,dom,cat,SL) ==
  -- v is a pattern variable, dom it's binding under $Subst
  -- tries to change dom, so that it has category cat under SL
  -- the result is a substitution list or 'failed
  dom is ['FactoredForm,arg] =>
    if isSubDomain(arg,$Integer) then arg := $Integer
    d := ['FactoredRing,arg]
    SL:= hasCate(arg,'(Ring),augmentSub(v,d,SL))
    SL = 'failed => 'failed
    hasCaty(d,cat,SL)
  EQCAR(cat,'Field) or EQCAR(cat, 'DivisionRing) =>
    if isSubDomain(dom,$Integer) then dom := $Integer
    d:= [$QuotientField, dom]
    hasCaty(dom,'(IntegralDomain),augmentSub(v,d,SL))
  cat is ['PolynomialCategory, d, :.] =>
    dom' := ['Polynomial, d]
    (containsVars d or canCoerceFrom(dom, dom'))
       and hasCaty(dom', cat, augmentSub(v,dom',SL))
  isSubDomain(dom,$Integer) =>
    NSL:= hasCate($Integer,cat,augmentSub(v,$Integer,SL))
    NSL = 'failed =>
      hasCateSpecialNew(v, dom, cat, SL)
    hasCaty($Integer,cat,NSL)
  hasCateSpecialNew(v, dom, cat, SL)

-- to be used in $newSystem only
hasCateSpecialNew(v,dom,cat,SL) ==
  fe := member(QCAR cat, '(ElementaryFunctionCategory
       TrigonometricFunctionCategory ArcTrigonometricFunctionCategory
        HyperbolicFunctionCategory ArcHyperbolicFunctionCategory
         PrimitiveFunctionCategory SpecialFunctionCategory Evalable
          CombinatorialOpsCategory TranscendentalFunctionCategory
           AlgebraicallyClosedFunctionSpace ExpressionSpace
             LiouvillianFunctionCategory FunctionSpace))
  alg := member(QCAR cat, '(RadicalCategory AlgebraicallyClosedField))
  fefull := fe or alg or EQCAR(cat, 'CombinatorialFunctionCategory)
  partialResult :=
    EQCAR(dom, 'Variable) or EQCAR(dom, 'Symbol) =>
      first(cat) in
       '(SemiGroup AbelianSemiGroup Monoid AbelianGroup AbelianMonoid
         PartialDifferentialRing Ring InputForm) =>
                d := ['Polynomial, $Integer]
                augmentSub(v, d, SL)
      EQCAR(cat, 'Group) =>
        d := ['Fraction, ['Polynomial, $Integer]]
        augmentSub(v, d, SL)
      fefull =>
        d := defaultTargetFE dom
        augmentSub(v, d, SL)
      'failed
    isEqualOrSubDomain(dom, $Integer) =>
      fe =>
        d := defaultTargetFE $Integer
        augmentSub(v, d, SL)
      alg =>
        d := '(AlgebraicNumber)
        --d := defaultTargetFE $Integer
        augmentSub(v, d, SL)
      'failed
    underDomainOf dom = $ComplexInteger =>
      d := defaultTargetFE $ComplexInteger
      hasCaty(d,cat,augmentSub(v, d, SL))
    (dom = $RationalNumber) and alg =>
      d := '(AlgebraicNumber)
      --d := defaultTargetFE $Integer
      augmentSub(v, d, SL)
    fefull =>
      d := defaultTargetFE dom
      augmentSub(v, d, SL)
    'failed
  partialResult = 'failed => 'failed
  hasCaty(d, cat, partialResult)

hasCaty(d,cat,SL) ==
  -- calls hasCat, which looks up a hashtable and returns:
  -- 1. T, NIL or a (has x1 x2) condition, if cat is not parameterized
  -- 2. a list of pairs (argument to cat,condition) otherwise
  -- then the substitution SL is augmented, or the result is 'failed
  cat is ['CATEGORY,.,:y] => hasAttSig(d,subCopy(y,constructSubst d),SL)
  cat is ['SIGNATURE,foo,sig] =>
    hasSig(d,foo,subCopy(sig,constructSubst d),SL)
  cat is ['ATTRIBUTE,a] => BREAK()
  x:= hasCat(opOf d,opOf cat) =>
    y:= IFCDR cat =>
      S  := constructSubst d
      for [z,:cond] in x until not (S1='failed) repeat
        S' := [[p, :mkDomPvar(p, d, z, y)] for [p,:d] in S]
        if $domPvar then
          dom := [first d, :[domArg(arg, i, z, y) for i in 0..
                           for arg in rest d]]
          SL := augmentSub($domPvar, dom, copy SL)
        z' := [domArg2(a, S, S') for a in z]
        S1:= unifyStruct(y,z',copy SL)
        if not (S1='failed) then S1:=
          atom cond => S1
          ncond := subCopy(cond, S)
          ncond is ['has, =d, =cat] => 'failed
          hasCaty1(ncond,S1)
      S1
    atom x => SL
    ncond := subCopy(x, constructSubst d)
    ncond is ['has, =d, =cat] => 'failed
    hasCaty1(ncond, SL)
  'failed

mkDomPvar(p, d, subs, y) ==
  l := MEMQ(p, $FormalMapVariableList) =>
    domArg(d, #$FormalMapVariableList - #l, subs, y)
  d

domArg(type, i, subs, y) ==
  p := MEMQ($FormalMapVariableList.i, subs) =>
    y.(#subs - #p)
  type

domArg2(arg, SL1, SL2) ==
  isSharpVar arg => subCopy(arg, SL1)
  arg = '_$ and $domPvar => $domPvar
  subCopy(arg, SL2)

hasCaty1(cond,SL) ==
  -- cond is either a (has a b) or an OR clause of such conditions
  -- SL is augmented, if cond is true, otherwise the result is 'failed
  $domPvar: local := NIL
  cond is ['has,a,b] => hasCate(a,b,SL)
  cond is ['AND,:args] =>
    for x in args while not (S='failed) repeat S:=
      x is ['has,a,b] => hasCate(a,b, SL)
      -- next line is for an obscure bug in the table
      x is [['has,a,b]] => hasCate(a,b, SL)
      --'failed
      hasCaty1(x, SL)
    S
  cond is ['OR,:args] =>
    for x in args until not (S='failed) repeat S:=
      x is ['has,a,b] => hasCate(a,b,copy SL)
      -- next line is for an obscure bug in the table
      x is [['has,a,b]] => hasCate(a,b,copy SL)
      --'failed
      hasCaty1(x, copy SL)
    S
  keyedSystemError("S2GE0016",
    ['"hasCaty1",'"unexpected condition from category table"])

hasAttSig(d,x,SL) ==
  -- d is domain, x a list of attributes and signatures
  -- the result is an augmented SL, if d has x, 'failed otherwise
  for y in x until SL='failed repeat SL:=
    y is ['ATTRIBUTE,a] => BREAK()
    y is ['SIGNATURE,foo,s] => hasSig(d,foo,s,SL)
    keyedSystemError("S2GE0016",
      ['"hasAttSig",'"unexpected form of unnamed category"])
  SL

hasSigAnd(andCls, S0, SL) ==
  dead := NIL
  SA := 'failed
  for cls in andCls while not dead repeat
    SA :=
      atom cls => copy SL
      cls is ['has,a,b] =>
        hasCate(subCopy(a,S0),subCopy(b,S0),copy SL)
      keyedSystemError("S2GE0016",
        ['"hasSigAnd",'"unexpected condition for signature"])
    if SA = 'failed then dead := true
  SA

hasSigOr(orCls, S0, SL) ==
  found := NIL
  SA := 'failed
  for cls in orCls until found repeat
    SA :=
      atom cls => copy SL
      cls is ['has,a,b] =>
        hasCate(subCopy(a,S0),subCopy(b,S0),copy SL)
      cls is ['AND,:andCls] or cls is ['and,:andCls] =>
        hasSigAnd(andCls, S0, SL)
      keyedSystemError("S2GE0016",
        ['"hasSigOr",'"unexpected condition for signature"])
    if SA ~= 'failed then found := true
  SA

hasSig(dom,foo,sig,SL) ==
  -- tests whether domain dom has function foo with signature sig
  -- under substitution SL
  $domPvar: local := nil
  fun := constructor? first dom =>
    S0:= constructSubst dom
    p := ASSQ(foo, getOperationAlistFromLisplib first dom) =>
      for [x, ., cond, .] in rest p until not (S = 'failed) repeat
        S:=
          atom cond => copy SL
          cond is ['has,a,b] =>
            hasCate(subCopy(a,S0),subCopy(b,S0),copy SL)
          cond is ['AND,:andCls] or cond is ['and,:andCls] =>
            hasSigAnd(andCls, S0, SL)
          cond is ['OR,:orCls] or cond is ['or,:orCls] =>
            hasSigOr(orCls, S0, SL)
          keyedSystemError("S2GE0016",
             ['"hasSig",'"unexpected condition for signature"])
        not (S='failed) => S:= unifyStruct(subCopy(x,S0),sig,S)
      S
    'failed
  'failed

hasCatExpression(cond,SL) ==
  cond is ['OR,:l] =>
    or/[(y:=hasCatExpression(x,SL)) ~= 'failed for x in l] => y
  cond is ['AND,:l] =>
    and/[(SL:= hasCatExpression(x,SL)) ~= 'failed for x in l] => SL
  cond is ['has,a,b] => hasCate(a,b,SL)
  keyedSystemError("S2GE0016",
    ['"hasSig",'"unexpected condition for attribute"])

unifyStruct(s1,s2,SL) ==
  -- tests for equality of s1 and s2 under substitutions SL and $Subst
  -- the result is a substitution list or 'failed
  s1=s2 => SL
  if s1 is ['_:,x,.] then s1:= x
  if s2 is ['_:,x,.] then s2:= x
  if not atom s1 and first s1 = '_# then s1 := LENGTH CADR s1
  if not atom s2 and first s2 = '_# then s2 := LENGTH CADR s2
  s1=s2 => SL
  isPatternVar s1 => unifyStructVar(s1,s2,SL)
  isPatternVar s2 => unifyStructVar(s2,s1,SL)
  atom s1 or atom s2 => 'failed
  until null s1 or null s2 or SL='failed repeat
    SL := unifyStruct(first s1, first s2, SL)
    s1 := rest s1
    s2 := rest s2
    atom s1 =>
        if s1 = s2 then s2 := nil
        s1 := nil
    atom s2 => s2 := nil
  s1 or s2 => 'failed
  SL

unifyStructVar(v,s,SL) ==
  -- the first argument is a pattern variable, which is not substituted
  -- by SL
  CONTAINED(v,s) => 'failed
  ps := LASSOC(s, SL)
  s1 := (ps => ps; s)
  (s0 := LASSOC(v, SL)) or (s0 := LASSOC(v,$Subst)) =>
    S:= unifyStruct(s0,s1,copy SL)
    S='failed =>
      $Coerce and not atom s0 and constructor? first s0 =>
        containsVars s0 or containsVars s1 =>
          ns0 := subCopy(s0, SL)
          ns1 := subCopy(s1, SL)
          containsVars ns0 or containsVars ns1 =>
            $hope:= 'T
            'failed
          if canCoerce(ns0, ns1) then s3 := s1
          else if canCoerce(ns1, ns0) then s3 := s0
          else s3 := nil
          s3 =>
            if (s3 ~= s0) then SL := augmentSub(v,s3,SL)
            if (s3 ~= s1) and isPatternVar(s) then SL := augmentSub(s,s3,SL)
            SL
          'failed
        $domPvar =>
          s3 := resolveTT(s0,s1)
          s3 =>
            if (s3 ~= s0) then SL := augmentSub(v,s3,SL)
            if (s3 ~= s1) and isPatternVar(s) then SL := augmentSub(s,s3,SL)
            SL
          'failed
--        isSubDomain(s,s0) => augmentSub(v,s0,SL)
        'failed
      'failed
    augmentSub(v,s,S)
  augmentSub(v,s,SL)

ofCategory(dom,cat) ==
  -- entry point to category evaluation from other points than type
  --   analysis
  -- the result is true or NIL
  $Subst:local:= NIL
  $hope:local := NIL
  IDENTP dom => NIL
  cat is ['Join,:cats] => and/[ofCategory(dom,c) for c in cats]
  (hasCaty(dom,cat,NIL) ~= 'failed)

printMms(mmS) ==
  -- mmS a list of modemap signatures
  sayMSG '" "
  for [sig,imp,.] in mmS for i in 1.. repeat
    istr := STRCONC('"[",STRINGIMAGE i,'"]")
    if QCSIZE(istr) = 3 then istr := STRCONC(istr,'" ")
    sayMSG [:bright istr, '"signature:   ", :formatSignature rest sig]
    first sig = 'local =>
      sayMSG ['"      implemented: local function ",imp]
    imp is ['XLAM,:.] =>
      sayMSG concat('"      implemented: XLAM from ",
        prefix2String first sig)
    sayMSG concat('"      implemented: slot ",imp,
      '" from ", prefix2String first sig)
  sayMSG '" "

containsVars(t) ==
  -- tests whether term t contains a * variable
  atom t => isPatternVar t
  containsVars1(t)

containsVars1(t) ==
  -- recursive version, which works on a list
  [t1,:t2]:= t
  atom t1 =>
    isPatternVar t1 or
      atom t2 => isPatternVar t2
      containsVars1(t2)
  containsVars1(t1) or
    atom t2 => isPatternVar t2
    containsVars1(t2)

-- [[isPartialMode]] tests whether m contains [[$EmptyMode]]. The
-- constant [[$EmptyMode]] (defined in bootfuns.lisp) evaluates to
-- [[|$EmptyMode|]]. This constants is inserted in a modemap during
-- compile time if the modemap is not yet complete.
isPartialMode m ==
  CONTAINED($EmptyMode,m)


getSymbolType var ==
-- var is a pattern variable
  p := ASSQ(var, $SymbolType) => rest p
  t:= '(Polynomial (Integer))
  $SymbolType:= CONS(CONS(var,t),$SymbolType)
  t

isEqualOrSubDomain(d1,d2) ==
  -- last 2 parts are for tagged unions (hack for now, RSS)
  (d1=d2) or isSubDomain(d1,d2) or
    (atom(d1) and ((d2 is ['Variable,=d1]) or (d2 is [=d1])))
     or (atom(d2) and ((d1 is ['Variable,=d2]) or (d1 is [=d2])))

defaultTypeForCategory(cat, SL) ==
  -- this function returns a domain belonging to cat
  -- note that it is important to note that in some contexts one
  -- might not want to use this result. For example, evalMmCat1
  -- calls this and should possibly fail in some cases.
  cat := subCopy(cat, SL)
  c := first cat
  d := GETDATABASE(c, 'DEFAULTDOMAIN)
  d => [d, :rest cat]
  cat is [c] =>
    c = 'Field => $RationalNumber
    c in '(Ring IntegralDomain EuclideanDomain GcdDomain
      OrderedRing DifferentialRing) => '(Integer)
    c = 'OrderedSet => $Symbol
    c = 'FloatingPointSystem => '(Float)
    NIL
  cat is [c,p1] =>
    c = 'FiniteLinearAggregate => ['Vector, p1]
    c = 'VectorCategory => ['Vector, p1]
    c = 'SetAggregate => ['Set, p1]
    c = 'SegmentCategory => ['Segment, p1]
    NIL
  cat is [c,p1,p2] =>
    NIL
  cat is [c,p1,p2,p3] =>
    cat is ['MatrixCategory, d, ['Vector, =d], ['Vector, =d]] =>
      ['Matrix, d]
    NIL
  NIL
