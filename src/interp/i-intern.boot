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
Internal Interpreter Facilities

Vectorized Attributed Trees

The interpreter translates parse forms into vats for analysis.
These contain a number of slots in each node for information.
The leaves are now all vectors, though the leaves for basic types
such as integers and strings used to just be the objects themselves.
The vectors for the leaves with such constants now have the value
of $immediateDataSymbol as their name. Their are undoubtably still
some functions that still check whether a leaf is a constant. Note
that if it is not a vector it is a subtree.

attributed tree nodes have the following form:
slot         description
----         -----------------------------------------------------
 0           operation name or literal
 1           declared mode of variable
 2           computed value of subtree from this node
 3           modeset: list of single computed mode of subtree
 4           prop list for extra things
)endif

DEFPARAMETER($useParserSrcPos, NIL)
DEFPARAMETER($transferParserSrcPos, NIL)

DEFCONST($failure, GENSYM())

--  Making Trees

mkAtreeNode x ==
  -- maker of attrib tree node
  v := MAKE_VEC(5)
  v.0 := x
  v

mkAtree x ==
  -- maker of attrib tree from parser form
  mkAtree1 mkAtreeExpandMacros x

mkAtreeWithSrcPos(form, posnForm) ==
    posnForm and $useParserSrcPos => pf2Atree(posnForm)
    transferSrcPosInfo(posnForm, mkAtree form)

mkAtree1WithSrcPos(form, posnForm) ==
  transferSrcPosInfo(posnForm, mkAtree1 form)

mkAtreeNodeWithSrcPos(form, posnForm) ==
  transferSrcPosInfo(posnForm, mkAtreeNode form)

transferSrcPosInfo(pf, atree) ==
    not (pf and $transferParserSrcPos) => atree
    pos := pfPosOrNopos(pf)
    pfNoPosition?(pos) => atree

    -- following is a hack because parser code for getting filename
    -- seems wrong.
    fn := lnPlaceOfOrigin poGetLineObject(pos)
    if NULL fn or fn = '"strings" then fn := '"console"

    putSrcPos(atree, fn, pfSourceText(pf), pfLinePosn(pos), pfCharPosn(pos))
    atree

mkAtreeExpandMacros x ==
  -- handle macro expansion. if the macros have args we require that
  -- we match the correct number of args
  if x isnt ['MDEF,:.] and x isnt ['DEF,['macro,:.],:.] then
    atom x and (m := isInterpMacro x) =>
      [args,:body] := m
      args => 'doNothing
      x := body
    x is [op,:argl] =>
      op = 'QUOTE => 'doNothing
      op = "where" and argl is [before, after] =>
        -- in a where clause, what follows "where" (the "after" parm
        -- above) might be a local macro, so do not expand the "before"
        -- part yet
        x := [op,before,mkAtreeExpandMacros after]
      argl := [mkAtreeExpandMacros a for a in argl]
      (m := isInterpMacro op) =>
        [args,:body] := m
        #args = #argl =>
          sl := [[a,:s] for a in args for s in argl]
          x := SUBLISNQ(sl,body)
        null args => x := [body,:argl]
        x := [op,:argl]
      x := [mkAtreeExpandMacros op,:argl]
  x

mkAtree1 x ==
  -- first special handler for making attrib tree
  null x => throwKeyedMsg("S2IP0005",['"NIL"])
  VECP x => x
  atom x =>
    x in '(noBranch noMapVal) => x
    x in '(nil true false) => mkAtree2([x],x,NIL)
    x = '_/throwAway =>
      -- don't want to actually compute this
      tree := mkAtree1 '(void)
      putValue(tree,objNewWrap(voidValue(),$Void))
      putModeSet(tree,[$Void])
      tree
    getBasicMode x =>
      v := mkAtreeNode $immediateDataSymbol
      putValue(v,getBasicObject x)
      v
    IDENTP x => mkAtreeNode x
    keyedSystemError("S2II0002",[x])
  x is [op,:argl] => mkAtree2(x,op,argl)
  systemErrorHere '"mkAtree1"

-- mkAtree2 and mkAtree3 were created because mkAtree1 got so big

mkAtree2(x,op,argl) ==
  nargl := #argl
  (op= '_-) and (nargl = 1) and (INTEGERP first argl) =>
    mkAtree1(- first argl)
  op='_: and argl is [y,z] => [mkAtreeNode 'Declare,:argl]
  op='COLLECT => [mkAtreeNode op,:transformCollect argl]
  op= 'break =>
    argl is [.,val] =>
      if val = '$NoValue then val := '(void)
      [mkAtreeNode op,mkAtree1 val]
    [mkAtreeNode op,mkAtree1 '(void)]
  op= "return" =>
    argl is [val] =>
      if val = '$NoValue then val := '(void)
      [mkAtreeNode op,mkAtree1 val]
    [mkAtreeNode op,mkAtree1 '(void)]
  op='exit => mkAtree1 CADR argl
  op = 'QUOTE => [mkAtreeNode op,:argl]
  op='SEGMENT =>
    argl is [a] => [mkAtreeNode op, mkAtree1 a]
    z :=
      null argl.1 => nil
      mkAtree1 argl.1
    [mkAtreeNode op, mkAtree1 argl.0,z]
  op in '(pretend is isnt) =>
    [mkAtreeNode op,mkAtree1 first argl,:rest argl]
  op =  '_:_: =>
    [mkAtreeNode 'COERCE,mkAtree1 first argl,CADR argl]
  x is ['_@, expr, type] =>
    t := evaluateType unabbrev type
    t = '(DoubleFloat) and expr is [['_$elt, '(Float), 'float], :args] =>
        mkAtree1 [['_$elt, '(DoubleFloat), 'float], :args]
    t = '(DoubleFloat) and INTEGERP expr =>
        v := mkAtreeNode $immediateDataSymbol
        putValue(v,getBasicObject float expr)
        v
    t = '(Float) and INTEGERP expr =>
        mkAtree1 ["::", expr, t]
    typeIsASmallInteger(t) and INTEGERP expr =>
        mkAtree1 ["::", expr, t]
    [mkAtreeNode 'TARGET,mkAtree1 expr, type]
  (op = "case") and (nargl = 2)  =>
    [mkAtreeNode "case", mkAtree1 first argl, unabbrev CADR argl]
  op='REPEAT => [mkAtreeNode op,:transformREPEAT argl]
  op='LET and argl is [['construct,:.],rhs] =>
    [mkAtreeNode 'LET,first argl,mkAtree1 rhs]
  op='LET and argl is [['_:,a,.],rhs] =>
    mkAtree1 ['SEQ,first argl,['LET,a,rhs]]
  op is ['_$elt,D,op1] =>
    op1 is '_= =>
      a' := [mkAtreeNode '_=,:[mkAtree1 arg for arg in argl]]
      [mkAtreeNode 'Dollar,D,a']
    [mkAtreeNode 'Dollar,D,mkAtree1 [op1,:argl]]
  op='_$elt =>
    argl is [D,a] =>
      INTEGERP a =>
        a = 0 => mkAtree1 [['_$elt,D,'Zero]]
        a = 1 => mkAtree1 [['_$elt,D,'One]]
        t := evaluateType unabbrev [D]
        typeIsASmallInteger(t) and SINTP a =>
            v := mkAtreeNode $immediateDataSymbol
            putValue(v,mkObjWrap(a, t))
            v
        mkAtree1 ["*",a,[['_$elt,D,'One]]]
      [mkAtreeNode 'Dollar,D,mkAtree1 a]
    keyedSystemError("S2II0003",['"$",argl,
      '"not qualifying an operator"])
  mkAtree3(x,op,argl)

mkAtree3fn(a, b) ==
    a and b =>
         if a = b then a
         else throwMessage '"   double declaration of parameter"
    a or b

mkAtree3(x,op,argl) ==
  op='REDUCE and argl is [op1,axis,body] =>
    [mkAtreeNode op,axis,mkAtree1 op1,mkAtree1 body]
  op='has => [mkAtreeNode op, :argl]
  op='_| => [mkAtreeNode 'AlgExtension,:[mkAtree1 arg for arg in argl]]
  op='_= => [mkAtreeNode 'equation,:[mkAtree1 arg for arg in argl]]
  op='not and argl is [["=",lhs,rhs]] =>
    [mkAtreeNode 'not,[mkAtreeNode "=",mkAtree1 lhs,mkAtree1 rhs]]
  op = "in" and argl is [var, ['SEGMENT, lb, ul]] =>
    upTest:=
      null ul => NIL
      mkLessOrEqual(var,ul)
    lowTest:=mkLessOrEqual(lb,var)
    z :=
      ul => ['and,lowTest,upTest]
      lowTest
    mkAtree1 z
  x is ['IF,p,'noBranch,a] => mkAtree1 ['IF,['not,p],a,'noBranch]
  x is ['RULEDEF, :.] => [mkAtreeNode 'RULEDEF, :rest x]
  x is ['MDEF,sym,junk1,junk2,val] =>
    -- new macros look like  macro f ==  or macro f(x) ===
    -- so transform into that format
    mkAtree1 ['DEF,['macro,sym],junk1,junk2,val]
  x is ["+->",funargs,funbody] =>
    if funbody is [":",body,type] then
      types := [type]
      funbody := body
    else types := [NIL]
    v := collectDefTypesAndPreds funargs
    types := [:types,:v.1]
    [mkAtreeNode 'ADEF,[v.0,types,[NIL for a in types],funbody],
      if v.2 then v.2 else true, false]
  x is ['ADEF,arg,:r] =>
    r := mkAtreeValueOf r
    v :=
      null arg => VECTOR(NIL,NIL,NIL)
      PAIRP arg and rest arg and first arg~= "|" =>
        collectDefTypesAndPreds ['Tuple,:arg]
      null rest arg => collectDefTypesAndPreds first arg
      collectDefTypesAndPreds arg
    [types,:r'] := r
    at := [mkAtree3fn(x, y) for x in rest types for y in v.1]
    r := [[first types,:at],:r']
    [mkAtreeNode 'ADEF,[v.0,:r],if v.2 then v.2 else true,false]
  x is ["where", before, after] =>
    [mkAtreeNode "where", before, mkAtree1 after]
  x is ['DEF,['macro,form],.,.,body] =>
    [mkAtreeNode 'MDEF,form,body]
  x is ['DEF,a,:r] =>
    r := mkAtreeValueOf r
    a is [op,:arg] =>
      v :=
        null arg => VECTOR(NIL,NIL,NIL)
        PAIRP arg and rest arg and first arg~= "|" =>
          collectDefTypesAndPreds ['Tuple,:arg]
        null rest arg => collectDefTypesAndPreds first arg
        collectDefTypesAndPreds arg
      [types,:r'] := r
      at := [mkAtree3fn(x, y) for x in rest types for y in v.1]
      r := [[first types,:at],:r']
      [mkAtreeNode 'DEF,[[op,:v.0],:r],if v.2 then v.2 else true,false]
    [mkAtreeNode 'DEF,[a,:r],true,false]
--x is ['when,y,pred] =>
--  y isnt ['DEF,a,:r] =>
--    keyedSystemError("S2II0003",['"when",y,'"improper argument form"])
--  a is [op,p1,:pr] =>
--    null pr => mkAtree1 ['DEF,[op,["|",p1,pred]],:r]
--    mkAtree1 ['DEF,[op,["|",['Tuple,p1,:pr],pred]],:r]
--  [mkAtreeNode 'DEF, rest y, pred, false]
--x is ['otherwise,u] =>
--  throwMessage '"   otherwise is no longer supported."
  z :=
    getBasicMode op =>
      v := mkAtreeNode $immediateDataSymbol
      putValue(v,getBasicObject op)
      v
    atom op => mkAtreeNode op
    mkAtree1 op
  [z,:[mkAtree1 y for y in argl]]

addPred(old, new) ==
    null new => old
    null old => new
    ['and, old, new]

collectDefTypesAndPreds args ==
  -- given an arglist to a DEF-like form, this function returns
  -- a vector of three things:
  --   slot 0: just the variables
  --   slot 1: the type declarations on the variables
  --   slot 2: a predicate for all arguments
  pred := types := vars := NIL
  junk :=
    IDENTP args =>
      types := [NIL]
      vars  := [args]
    args is [":",var,type] =>
      types := [type]
      var is ["|",var',p] =>
        vars := [var']
        pred := addPred(pred, p)
      vars := [var]
    args is ["|",var,p] =>
      pred := addPred(pred,p)
      var is [":",var',type] =>
        types := [type]
        vars := [var']
      var is ['Tuple,:.] or var is ["|",:.] =>
        v := collectDefTypesAndPreds var
        vars  := [:vars,:v.0]
        types := [:types,:v.1]
        pred  := addPred(pred,v.2)
      vars := [var]
      types := [NIL]
    args is ['Tuple,:args'] =>
      for a in args' repeat
        v := collectDefTypesAndPreds a
        vars  := [:vars,first v.0]
        types := [:types,first v.1]
        pred  := addPred(pred,v.2)
    types := [NIL]
    vars  := [args]
  VECTOR(vars,types,pred)

mkAtreeValueOf l ==
  -- scans for ['valueOf,atom]
  not CONTAINED('valueOf,l) => l
  mkAtreeValueOf1 l

mkAtreeValueOf1 l ==
  null l or atom l or null rest l => l
  l is ['valueOf,u] and IDENTP u =>
    v := mkAtreeNode $immediateDataSymbol
    putValue(v,get(u,'value,$InteractiveFrame) or
      objNewWrap(u,['Variable,u]))
    v
  [mkAtreeValueOf1 x for x in l]

mkLessOrEqual(lhs,rhs) == ['not,['_<,rhs,lhs]]

emptyAtree expr ==
  -- remove mode, value, and misc. info from attrib tree
  VECP expr =>
    $immediateDataSymbol = expr.0 => nil
    expr.1:= NIL
    expr.2:= NIL
    expr.3:= NIL
    -- kill proplist too?
  atom expr => nil
  for e in expr repeat emptyAtree e

unVectorize body ==
  -- transforms from an atree back into a tree
  VECP body =>
    name := getUnname body
    name ~= $immediateDataSymbol => name
    objValUnwrap getValue body
  atom body => body
  body is [op,:argl] =>
    newOp:=unVectorize op
    if newOp = 'SUCHTHAT then newOp := '_|
    if newOp = 'COERCE then newOp := '_:_:
    if newOp = 'Dollar then newOp := "$elt"
    [newOp,:unVectorize argl]
  systemErrorHere '"unVectorize"


--  Stuffing and Getting Info

putAtree(x,prop,val) ==
  x is [op,:.] =>
    -- only willing to add property if op is a vector
    -- otherwise will be pushing to deeply into calling structure
    if VECP op then putAtree(op,prop,val)
    x
  null VECP x => x     -- just ignore it
  n := QLASSQ(prop,'((mode . 1) (value . 2) (modeSet . 3)))
    => x.n := val
  x.4 := insertShortAlist(prop,val,x.4)
  x

getAtree(x,prop) ==
  x is [op,:.] =>
    -- only willing to get property if op is a vector
    -- otherwise will be pushing to deeply into calling structure
    VECP op => getAtree(op,prop)
    NIL
  null VECP x => NIL     -- just ignore it
  n:= QLASSQ(prop,'((mode . 1) (value . 2) (modeSet . 3)))
    => x.n
  QLASSQ(prop,x.4)

putTarget(x, targ) ==
  -- want to put nil modes perhaps to clear old target
  if targ = $EmptyMode then targ := nil
  putAtree(x,'target,targ)

getTarget(x) == getAtree(x,'target)

insertShortAlist(prop,val,al) ==
  pair := ASSQ(prop,al) =>
    RPLACD(pair,val)
    al
  [[prop,:val],:al]

transferPropsToNode(x,t) ==
  propList := getProplist(x,$env)
  QLASSQ('Led,propList) or QLASSQ('Nud,propList) => nil
  node :=
    VECP t => t
    first t
  for prop in '(mode localModemap value name generatedCode)
    repeat transfer(x,node,prop)
      where
        transfer(x,node,prop) ==
          u := get(x,prop,$env) => putAtree(node,prop,u)
          (not (x in $localVars)) and (u := get(x,prop,$e)) =>
            putAtree(node,prop,u)
  if not getMode(t) and (am := get(x,'automode,$env)) then
    putModeSet(t,[am])
    putMode(t,am)
  t

isLeaf x == atom x     --may be a number or a vector

getMode x ==
  x is [op,:.] => getMode op
  VECP x => x.1
  m := getBasicMode x => m
  keyedSystemError("S2II0001",[x])

putMode(x,y) ==
  x is [op,:.] => putMode(op,y)
  null VECP x => keyedSystemError("S2II0001",[x])
  x.1 := y

getValue x ==
  VECP x => x.2
  atom x =>
    t := getBasicObject x => t
    keyedSystemError("S2II0001",[x])
  getValue first x

putValue(x,y) ==
  x is [op,:.] => putValue(op,y)
  null VECP x => keyedSystemError("S2II0001",[x])
  x.2 := y

putValueValue(vec,val) ==
  putValue(vec,val)
  vec

getUnnameIfCan x ==
  VECP x => x.0
  x is [op,:.] => getUnnameIfCan op
  atom x => x
  nil

getUnname x ==
  x is [op,:.] => getUnname op
  getUnname1 x

getUnname1 x ==
  VECP x => x.0
  null atom x => keyedSystemError("S2II0001",[x])
  x

computedMode t ==
  getModeSet t is [m] => m
  keyedSystemError("S2GE0016",['"computedMode",'"non-singleton modeset"])

putModeSet(x,y) ==
  x is [op,:.] => putModeSet(op,y)
  not VECP x => keyedSystemError("S2II0001",[x])
  x.3 := y
  y

getModeOrFirstModeSetIfThere x ==
  x is [op,:.] => getModeOrFirstModeSetIfThere op
  VECP x =>
    m := x.1 => m
    val := x.2 => objMode val
    y := x.aModeSet =>
      (y = [$EmptyMode]) and ((m := getMode x) is ['Mapping,:.]) => m
      first y
    NIL
  m := getBasicMode x => m
  NIL

getModeSet x ==
  x and PAIRP x => getModeSet first x
  VECP x =>
    y:= x.aModeSet =>
      (y = [$EmptyMode]) and ((m := getMode x) is ['Mapping,:.]) =>
        [m]
      y
    keyedSystemError("S2GE0016",['"getModeSet",'"no mode set"])
  m:= getBasicMode x => [m]
  null atom x => getModeSet first x
  keyedSystemError("S2GE0016",['"getModeSet",
    '"not an attributed tree"])

getModeSetUseSubdomain x ==
  x and PAIRP x => getModeSetUseSubdomain first x
  VECP(x) =>
    -- don't play subdomain games with retracted args
    getAtree(x,'retracted) => getModeSet x
    y := x.aModeSet =>
      (y = [$EmptyMode]) and ((m := getMode x) is ['Mapping,:.]) =>
        [m]
      val := getValue x
      (x.0 = $immediateDataSymbol) and (y = [$Integer]) =>
        val := objValUnwrap val
        m := getBasicMode0(val,true)
        x.2 := objNewWrap(val,m)
        x.aModeSet := [m]
        [m]
      null val => y
      isEqualOrSubDomain(objMode(val),$Integer) and
        INTEGERP(f := objValUnwrap val) =>
          [getBasicMode0(f,true)]
      y
    keyedSystemError("S2GE0016",
      ['"getModeSetUseSubomain",'"no mode set"])
  m := getBasicMode0(x,true) => [m]
  null atom x => getModeSetUseSubdomain first x
  keyedSystemError("S2GE0016",
    ['"getModeSetUseSubomain",'"not an attributed tree"])


--% Environment Utilities

-- getValueFromEnvironment(x,mode) ==
--   $failure ~= (v := getValueFromSpecificEnvironment(x,mode,$env)) => v
--   $failure ~= (v := getValueFromSpecificEnvironment(x,mode,$e))   => v
--   throwKeyedMsg("S2IE0001",[x])
getValueFromEnvironment(x,mode) ==
  $failure ~= (v := getValueFromSpecificEnvironment(x,mode,$env)) => v
  $failure ~= (v := getValueFromSpecificEnvironment(x,mode,$e))   => v
  null(v := coerceInt(objNew(x, ['Variable, x]), mode)) =>
     throwKeyedMsg("S2IE0001",[x])
  objValUnwrap v

getValueFromSpecificEnvironment(id,mode,e) ==
  PAIRP e =>
    u := get(id,'value,e) =>
      objMode(u) = $EmptyMode =>
        systemErrorHere '"getValueFromSpecificEnvironment"
      v := objValUnwrap u
      mode isnt ['Mapping,:mapSig] => v
      v isnt ['SPADMAP, :.] => v
      v' := coerceInt(u,mode)
      null v' => throwKeyedMsg("S2IC0002",[objMode u,mode])
      objValUnwrap v'

    m := get(id,'mode,e) =>
      -- See if we can make it into declared mode from symbolic form
      -- For example, (x : P[x] I; x + 1)
      if isPartialMode(m) then m' := resolveTM(['Variable,id],m)
      else m' := m
      m' and
        (u := coerceInteractive(objNewWrap(id,['Variable,id]),m')) =>
          objValUnwrap u

      throwKeyedMsg("S2IE0002",[id,m])
    $failure
  $failure

addBindingInteractive(var,proplist,e is [[curContour,:.],:.]) ==
  -- change proplist of var in e destructively
  u := ASSQ(var,curContour) =>
    RPLACD(u,proplist)
    e
  rplac(CAAR e, [[var, :proplist], :curContour])
  e

augProplistInteractive(proplist,prop,val) ==
  u := ASSQ(prop,proplist) =>
    RPLACD(u,val)
    proplist
  [[prop,:val],:proplist]

getFlag x == get("--flags--",x,$e)

putFlag(flag,value) ==
  $e := put ("--flags--", flag, value, $e)

get(x,prop,e) ==
  $InteractiveMode => get0(x,prop,e)
  get1(x,prop,e)

get0(x,prop,e) ==
  null atom x => get(QCAR x,prop,e)
  (pl := getProplist(x, e)) => QLASSQ(prop, pl)
  nil

get1(x,prop,e) ==
    --this is the old get
  negHash := nil
  null atom x => get(QCAR x,prop,e)
  if $envHashTable and (not(EQ($CategoryFrame, e))) and (not(EQ(prop, "modemap"))) then
    null (HGET($envHashTable, [x, prop])) => return nil
    negHash := null (HGET($envHashTable, [x, prop]))
  prop="modemap" and $insideCapsuleFunctionIfTrue=true =>
    ress := LASSOC("modemap",getProplist(x,$CapsuleModemapFrame))
              or get2(x,prop,e)
    -- SAY ["get1", x, prop, ress and true]
    ress
  ress := LASSOC(prop,getProplist(x,e)) or get2(x,prop,e)
  if ress and negHash then
    SAY ["get1", x, prop, ress and true]
  ress

get2(x,prop,e) ==
  prop="modemap" and constructor? x =>
    (u := getConstructorModemap(x)) => [u]
    nil
  nil

getI(x,prop) == get(x,prop,$InteractiveFrame)

putI(x,prop,val) == ($InteractiveFrame := put(x,prop,val,$InteractiveFrame))

getIProplist x == getProplist(x,$InteractiveFrame)

rempropI(x,prop) ==
  id:=
    atom x => x
    first x
  getI(id,prop) =>
    recordNewValue(id,prop,NIL)
    recordOldValue(id,prop,getI(id,prop))
    $InteractiveFrame:= remprop(id,prop,$InteractiveFrame)

remprop(x,prop,e) ==
  u:= assoc(prop,pl:= getProplist(x,e)) =>
    e:= addBinding(x,DELASC(first u,pl),e)
    e
  e

fastSearchCurrentEnv(x,currentEnv) ==
  u := QLASSQ(x, first currentEnv) => u
  while (currentEnv:= QCDR currentEnv) repeat
    u := QLASSQ(x, first currentEnv) => u

put(x,prop,val,e) ==
  $InteractiveMode and not EQ(e,$CategoryFrame) =>
    putIntSymTab(x,prop,val,e)
  --e must never be $CapsuleModemapFrame
  null atom x => put(first x,prop,val,e)
  newProplist:= augProplistOf(x,prop,val,e)
  prop="modemap" and $insideCapsuleFunctionIfTrue=true =>
    SAY ["**** modemap PUT on CapsuleModemapFrame: ",val]
    $CapsuleModemapFrame:=
      addBinding(x,augProplistOf(x,"modemap",val,$CapsuleModemapFrame),
        $CapsuleModemapFrame)
    e
  addBinding(x,newProplist,e)

putIntSymTab(x,prop,val,e) ==
  null atom x => putIntSymTab(first x,prop,val,e)
  pl0 := pl := search(x,e)
  pl :=
    null pl => [[prop,:val]]
    u := ASSQ(prop,pl) =>
      RPLACD(u,val)
      pl
    lp := LASTNODE pl
    u := [[prop,:val]]
    RPLACD(lp,u)
    pl
  EQ(pl0,pl) => e
  addIntSymTabBinding(x,pl,e)

addIntSymTabBinding(var,proplist,e is [[curContour,:.],:.]) ==
  -- change proplist of var in e destructively
  u := ASSQ(var,curContour) =>
    RPLACD(u,proplist)
    e
  rplac(CAAR e, [[var, :proplist], :curContour])
  e


--% Source and position information

-- In the following, src is a string containing an original input line,
-- line is the line number of the string within the source file,
-- and col is the index within src of the start of the form represented
-- by x. x is a VAT.

putSrcPos(x, file, src, line, col) ==
    putAtree(x, 'srcAndPos, srcPos_New(file, src, line, col))

getSrcPos(x) == getAtree(x, 'srcAndPos)

srcPosNew(file, src, line, col) == LIST2VEC [file, src, line, col]

srcPosFile(sp) ==
    if sp then sp.0 else nil

srcPosSource(sp) ==
    if sp then sp.1 else nil

srcPosLine(sp) ==
    if sp then sp.2 else nil

srcPosColumn(sp) ==
    if sp then sp.3 else nil

srcPosDisplay(sp) ==
    null sp => nil
    s := STRCONC('"_"", srcPosFile sp, '"_", line ",
        STRINGIMAGE srcPosLine sp, '": ")
    sayBrightly [s, srcPosSource sp]
    col  := srcPosColumn sp
    dots :=
        col = 0 => '""
        fillerSpaces(col, '".")
    sayBrightly [fillerSpaces(#s, '" "), dots, '"^"]
    true

--% Functions on interpreter objects

-- Interpreter objects used to be called triples because they had the
-- structure [value, type, environment].  For many years, the environment
-- was not used, so finally in January, 1990, the structure of objects
-- was changed to be (type . value).  This was chosen because it was the
-- structure of objects of type Any.  Sometimes the values are wrapped
-- (see the function isWrapped to see what this means physically).
-- Wrapped values are not actual values belonging to their types.  An
-- unwrapped value must be evaluated to get an actual value.  A wrapped
-- value must be unwrapped before being passed to a library function.
-- Typically, an unwrapped value in the interpreter consists of LISP
-- code, e.g., parts of a function that is being constructed.
--                 RSS 1/14/90

-- These are the new structure functions.

mkObj(val, mode) == CONS(mode,val)              -- old names
mkObjWrap(val, mode) == CONS(mode,wrap val)
mkObjCode(val, mode) == ['CONS, MKQ mode,val ]

objNew(val, mode) == CONS(mode,val)             -- new names as of 10/14/93
objNewWrap(val, mode) == CONS(mode,wrap val)
objNewCode(val, mode) == ['CONS, MKQ mode,val ]
objSetVal(obj,val) == RPLACD(obj,val)
objSetMode(obj,mode) == RPLACA(obj,mode)

objVal obj == rest obj
objValUnwrap obj == unwrap rest obj
objMode obj == first obj

objCodeVal obj == CADDR obj
objCodeMode obj == CADR obj




--% Library compiler structures needed by the interpreter

-- Tuples and Crosses

asTupleNew(size, listOfElts) == CONS(size, LIST2VEC listOfElts)
asTupleNew0(listOfElts) == CONS(#listOfElts, LIST2VEC listOfElts)

asTupleNewCode(size, listOfElts) == ["asTupleNew", size, ['LIST, :listOfElts]]
asTupleNewCode0(listForm) == ["asTupleNew0", listForm]

asTupleSize(at) == first at
asTupleAsVector(at) == rest at
asTupleAsList(at) == VEC2LIST asTupleAsVector at
