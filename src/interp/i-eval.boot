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

--% Constructor Evaluation

-- For use from compiled code

quoteNontypeArgs(t) ==
    t is [.] => t
    op := opOf t
    loadIfNecessary op
    args := rest t
    cs := rest GETDATABASE(op, 'COSIG)
    nargs := [if c then quoteNontypeArgs(a) else ["QUOTE", a]
                for a in args for c in cs]
    [op, :nargs]

evalType(t) == EVAL(quoteNontypeArgs(t))

---

$noEvalTypeMsg := nil
$evalDomain := nil

evalDomain form ==
  if $evalDomain then
    sayMSG concat('"   instantiating","%b",prefix2String form,"%d")
  startTimingProcess 'instantiation
  result := eval mkEvalable form
  stopTimingProcess 'instantiation
  result

mkEvalable form ==
  form is [op,:argl] =>
    op="QUOTE" => form
    op="WRAPPED" => mkEvalable devaluate argl
    op="Record" => mkEvalableRecord form
    op="Union"  => mkEvalableUnion  form
    op="Mapping"=> mkEvalableMapping form
    op="Enumeration" => form
    loadIfNecessary op
    kind:= GETDATABASE(op,'CONSTRUCTORKIND)
    cosig := GETDATABASE(op, 'COSIG) =>
      [op,:[val for x in argl for typeFlag in rest cosig]] where val ==
        typeFlag =>
          kind = 'category => MKQ x
          VECP x => MKQ x
          loadIfNecessary x
          mkEvalable x
        x is ['QUOTE,:.] => x
        x is ['_#,y] => ['SIZE,MKQ y]
        MKQ x
    [op,:[mkEvalable x for x in argl]]
  form=$EmptyMode => $Integer
  IDENTP form and constructor?(form) => [form]
  FBPIP form => BREAK()
  form

mkEvalableMapping form ==
  [first form,:[mkEvalable d for d in rest form]]

mkEvalableRecord form ==
  [first form,:[[":",n,mkEvalable d] for [":",n,d] in rest form]]

mkEvalableUnion form ==
  isTaggedUnion form =>
    [first form,:[[":",n,mkEvalable d] for [":",n,d] in rest form]]
  [first form,:[mkEvalable d for d in rest form]]

evaluateType form ==
  -- Takes a parsed, unabbreviated type and evaluates it, replacing
  --  type valued variables with their values, and calling bottomUp
  --  on non-type valued arguemnts to the constructor
  --  and finally checking to see whether the type satisfies the
  --  conditions of its modemap
  domain:= isDomainValuedVariable form => domain
  form = $EmptyMode => form
  form = "?"        => $EmptyMode
  STRINGP form => form
  form = "$" => form
  form is ['typeOf,.] =>
    form' := mkAtree form
    bottomUp form'
    objVal getValue(form')
  form is [op,:argl] =>
    op='CATEGORY =>
      argl is [x,:sigs] => [op,x,:[evaluateSignature(s) for s in sigs]]
      form
    op in '(Join Mapping) =>
      [op,:[evaluateType arg for arg in argl]]
    op='Union  =>
      argl and first argl is [x,.,.] and member(x,'(_: Declare)) =>
        [op,:[['_:,sel,evaluateType type] for ['_:,sel,type] in argl]]
      [op,:[evaluateType arg for arg in argl]]
    op='Record =>
      [op,:[['_:,sel,evaluateType type] for ['_:,sel,type] in argl]]
    op='Enumeration => form
    evaluateFormAsType form
  constructor? form =>
    ATOM form => evaluateType [form]
    throwEvalTypeMsg("S2IE0003",[form,form])
  throwEvalTypeMsg("S2IE0004", [form])

++ `form' used in a context where a type (domain or category) is
++ expected.  Attempt to fully evaluate it.  Error if the resulting
++ value is not a type.  When successful, the result is the reified
++ canonical form of the type.
evaluateFormAsType form ==
  form is [op,:args] and constructor? op => evaluateType1 form
  t := mkAtree form
  -- ??? Maybe we should be more careful about generalized types.
  bottomUp t is [m] and (m = ["Mode"] or isCategoryForm(m)) =>
    objVal getValue t
  throwEvalTypeMsg("S2IE0004",[form])

evaluateType1 form ==
  --evaluates the arguments passed to a constructor
  [op,:argl]:= form
  constructor? op =>
    null (sig := getConstructorSignature form) =>
       throwEvalTypeMsg("S2IE0005",[form])
    [.,:ml] := sig
    ml := replaceSharps(ml,form)
    # argl ~= #ml => throwEvalTypeMsg("S2IE0003",[form,form])
    for x in argl for m in ml for argnum in 1.. repeat
      typeList := [v,:typeList] where v ==
        categoryForm?(m) =>
          m := evaluateType MSUBSTQ(x,'_$,m)
          evalCategory(x' := (evaluateType x), m) => x'
          throwEvalTypeMsg("S2IE0004",[form])
        m := evaluateType m
        GETDATABASE(opOf m,'CONSTRUCTORKIND) = 'domain and
            (tree := mkAtree x) and  putTarget(tree,m) and ((bottomUp tree) is [m1]) =>
                [zt,:zv]:= z1:= getAndEvalConstructorArgument tree
                (v1 := coerceOrRetract(z1, m)) => objValUnwrap v1
                throwKeyedMsgCannotCoerceWithValue(zv,zt,m)
        throwEvalTypeMsg("S2IE0006",[makeOrdinal argnum,m,form])
    [op,:NREVERSE typeList]
  throwEvalTypeMsg("S2IE0007",[op])

throwEvalTypeMsg(msg, args) ==
  $justUnparseType : local := true
  $noEvalTypeMsg => spadThrow()
  throwKeyedMsg(msg, args)

makeOrdinal i ==
  ('(first second third fourth fifth sixth seventh eighth ninth tenth)).(i-1)

evaluateSignature sig ==
  -- calls evaluateType on a signature
  sig is [ ='SIGNATURE,fun,sigl] =>
    ['SIGNATURE,fun,
      [(t = '_$ => t; evaluateType(t)) for t in sigl]]
  sig

--% Code Evaluation

-- This code generates, then evaluates code during the bottom up phase
-- of interpretation

splitIntoBlocksOf200 a ==
  null a => nil
  [[first (r:=x) for x in tails a for i in 1..200],
    :splitIntoBlocksOf200 rest r]

evalForm(op,opName,argl,mmS) ==
  -- applies the first applicable function

  for mm in mmS until form repeat
    [sig,fun,cond]:= mm
    (CAR sig) = 'interpOnly => form := CAR sig
    #argl ~= #CDDR sig => 'skip ---> RDJ 6/95
    form:=
      $genValue or null cond =>
        [getArgValue2(x,t,sideEffectedArg?(t,sig,opName),opName) or return NIL
         for x in argl for t in CDDR sig]
      [getArgValueComp2(x,t,c,sideEffectedArg?(t,sig,opName),opName) or return NIL
        for x in argl for t in CDDR sig for c in cond]
    form or null argl =>
      dc:= CAR sig
      form :=
        dc='local => --[fun,:form]
          atom fun =>
            fun in $localVars => ['SPADCALL,:form,fun]
            [fun,:form,NIL]
          ['SPADCALL,:form,fun]
        dc is ["__FreeFunction__",:freeFun] =>
          ['SPADCALL,:form,freeFun]
        fun is ['XLAM,xargs,:xbody] =>
          rec :=  first form
          xbody is [['RECORDELT,.,ind,len]] =>
            optRECORDELT([CAAR xbody,rec,ind,len])
          xbody is [['SETRECORDELT,.,ind,len,.]] =>
            optSETRECORDELT([CAAR xbody,rec,ind,len,CADDR form])
          xbody is [['RECORDCOPY,.,len]] =>
            optRECORDCOPY([CAAR xbody,rec,len])
          ['FUNCALL,['function , ['LAMBDA,xargs,:xbody]],:TAKE(#xargs, form)]
        dcVector := evalDomain dc
        fun0 := NRTcompileEvalForm(opName,fun,dcVector)
        null fun0 => throwKeyedMsg("S2IE0008",[opName])
        [bpi,:domain] := fun0
        EQ(bpi,function Undef) =>
         sayKeyedMsg("S2IE0009",[opName,formatSignature CDR sig,CAR sig])
         NIL
        if $NRTmonitorIfTrue = true then
          sayBrightlyNT ['"Applying ",first fun0,'" to:"]
          pp [devaluateDeeply x for x in form]
        _$:fluid := domain
        ['SPADCALL, :form, fun0]
  not form => nil
--  not form => throwKeyedMsg("S2IE0008",[opName])
  form='interpOnly => rewriteMap(op,opName,argl)
  targetType := CADR sig
  if CONTAINED('_#,targetType) then targetType := NRTtypeHack targetType
  evalFormMkValue(op,form,targetType)

sideEffectedArg?(t,sig,opName) ==
  opString := SYMBOL_-NAME opName
  (opName ~= "setelt!") and (ELT(opString, #opString-1) ~= char '_!) => nil
  dc := first sig
  t = dc

getArgValue(a, t) ==
  atom a and not VECP a =>
    t' := coerceOrRetract(getBasicObject a,t)
    t' and wrapped2Quote objVal t'
  v := getArgValue1(a, t) => v
  alt := altTypeOf(objMode getValue a, a, nil) =>
    t' := coerceInt(getValue a, alt)
    t' := coerceOrRetract(t',t)
    t' and wrapped2Quote objVal t'
  nil

getArgValue1(a,t) ==
  -- creates a value for a, coercing to t
  t' := getValue(a) =>
    (m := getMode a) and (m is ['Mapping,:ml]) and (m = t) and
      objValUnwrap(t') is ['SPADMAP, :.] =>
        getMappingArgValue(a,t,m)
    t' := coerceOrRetract(t',t)
    t' and wrapped2Quote objVal t'
  systemErrorHere '"getArgValue"

getArgValue2(a,t,se?,opName) ==
  se? and (objMode(getValue a) ~= t) =>
    throwKeyedMsg("S2IE0013", [opName, objMode(getValue a), t])
  getArgValue(a,t)

getArgValueOrThrow(x, type) ==
  getArgValue(x,type) or throwKeyedMsg("S2IC0007",[type])

getMappingArgValue(a,t,m is ['Mapping,:ml]) ==
  (una := getUnname a) in $localVars =>
    $genValue =>
      name := get(una,'name,$env)
      a.0 := name
      mmS := selectLocalMms(a,name,rest ml, nil)
      or/[mm for mm in mmS |
        (mm is [[., :ml1],oldName,:.] and ml=ml1)] =>
            MKQ [COERCE(oldName, 'FUNCTION)]
      NIL
    una
  mmS := selectLocalMms(a,una,rest ml, nil)
  or/[mm for mm in mmS |
    (mm is [[., :ml1],oldName,:.] and ml=ml1)] =>
        MKQ [COERCE(oldName, 'FUNCTION)]
  NIL

getArgValueComp2(arg, type, cond, se?, opName) ==
  se? and (objMode(getValue arg) ~= type) =>
    throwKeyedMsg("S2IE0013", [opName, objMode(getValue arg), type])
  getArgValueComp(arg, type, cond)

getArgValueComp(arg,type,cond) ==
  -- getArgValue for compiled case.  if there is a condition then
  --  v must be data to verify that coerceInteractive succeeds.
  v:= getArgValue(arg,type)
  null v => nil
  null cond => v
  v is ['QUOTE,:.] or getBasicMode v => v
  n := getUnnameIfCan arg
  if num := isSharpVarWithNum n then
    not $compilingMap => n := 'unknownVar
    alias := get($mapName,'alias,$e)
    n := alias.(num - 1)
  keyedMsgCompFailure("S2IE0010",[n])

evalFormMkValue(op,form,tm) ==
  val:=
    u:=
      $genValue => wrap timedEVALFUN form
      form
    objNew(u,tm)
--+
  if $NRTmonitorIfTrue = true then
    sayBrightlyNT ['"Value of ",op.0,'" ===> "]
    pp unwrap u
  putValue(op,val)
  [tm]

--% Some Antique Comments About the Interpreter

--EVAL BOOT contains the top level interface to the Scratchhpad-II
--interpreter.  The Entry point into the interpreter from the parser is
--processInteractive.
--The type analysis algorithm is contained in the file BOTMUP BOOT,
--and MODSEL boot,
--the map handling routines are in MAP BOOT and NEWMAP BOOT, and
--the interactive coerce routines are in COERCE BOOT and COERCEFN BOOT.
--
--Conventions:
--    All spad values in the interpreter are passed around in triples.
--    These are lists of three items: [value,mode,environment].  The value
--    may be wrapped (this is a pair whose CAR is the atom WRAPPED and
--    whose CDR is the value), which indicates that it is a real value,
--    or unwrapped in which case it needs to be EVALed to produce the
--    proper value.  The mode is the type of value, and should always be
--    completely specified (not contain $EmptyMode).  The environment
--    is always empty, and is included for historical reasons.
--
--Modemaps:
--    Modemaps are descriptions of compiled Spad function which the
--    interpreter uses to perform type analysis. They consist of patterns
--    of types for the arguments, and conditions the types must satisfy
--    for the function to apply.  For each function name there is a list
--    of modemaps in file MODEMAP DATABASE for each distinct function with
--    that name. The following is the list of the modemaps for "*"
--    (multiplication. The first modemap (the one with the labels) is for
--    module mltiplication which is multiplication of an element of a
--    module by a member of its scalar domain.
--
--   This is the signature pattern for the modemap, it is of the form:
--    (DomainOfComputation TargetType <ArgumentType ...>)
--          |
--          |                This is the predicate that needs to be
--          |                 satisfied for the modemap to apply
--          |                            |
--          V                            |
--     /-----------/                     |
-- ( ( (*1 *1 *2 *1)                     V
--       /-----------------------------------------------------------/
--     ( (AND (ofCategory *1 (Module *2)) (ofCategory *2 (SimpleRing))) )
--      . CATDEF) <-- This is the file where the function was defined
--   ( (*1 *1 *2 *1)
--     ( (AND (isDomain *2 (Integer)) (ofCategory *1 (AbelianGroup))) )
--      . CATDEF)
--   ( (*1 *1 *2 *1)
--     ( (AND
--         (isDomain *2 (NonNegativeInteger))
--         (ofCategory *1 (AbelianMonoid))) )
--      . CATDEF)
--   ((*1 *1 *1 *1) ((ofCategory *1 (SemiGroup)) ) . CATDEF)
--      )
--
--Environments:
--    Environments associate properties with atoms.
--    (see CUTIL BOOT for the exact structure of environments).
--    Some common properties are:
-- modeSet:
--    During interpretation we build a modeSet property for each node in
--    the expression.  This is (in theory) a list of all the types
--    possible for the node.  In the current implementation these
--    modeSets always contain a single type.
-- value:
--    Value properties are always triples.  This is where the values of
--    variables are stored.  We also build value properties for internal
--    nodes during the bottom up phase.
-- mode:
--    This is the declared type of an identifier.
--
--  There are several different environments used in the interpreter:
--    $InteractiveFrame : this is the environment where the user
--     values are stored.  Any side effects of evaluation of a top-level
--     expression are stored in this environment.  It is always used as
--     the starting environment for interpretation.
--    $e : This is the name used for $InteractiveFrame while interpreting.
--    $env : This is local environment used by the interpreter.
--     Only temporary information (such as types of local variables is
--     stored in $env.
--     It is thrown away after evaluation of each expression.
--
--Frequently used global variables:
--    $genValue : if true then evaluate generated code, otherwise leave
--      code unevaluated.  If $genValue is false then we are compiling.
--    $op: name of the top level operator (unused except in map printing)
--    $mapList:  list of maps being type analyzed, used in recursive
--               map type anlysis.
--    $compilingMap: true when compiling a map, used to detect where to
--                   THROW when interpret-only is invoked
--    $compilingLoop: true when compiling a loop body, used to control
--                    nesting level of interp-only loop CATCH points
--    $interpOnly: true when in interpret only mode, used to call
--                 alternate forms of COLLECT and REPEAT.
--    $inCOLLECT: true when compiling a COLLECT, used only for hacked
--                stream compiler.
--    $declaredMode: Weak type propagation for symbols, set in upCOERCE
--                   and upLET.  This variable is used to determine
--                   the alternate polynomial types of Symbols.
--    $localVars: list of local variables in a map body
--    $MapArgumentTypeList: hack for stream compilation
