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
Special coercion routines

This is the newly revised set of coercion functions to work with
the new library and the new runtime system.

coerceByTable is driven off $CoerceTable which is used to match
the top-level constructors of the source and object types.  The
form of $CoerceTable is an alist where the "properties" are the
source top-level constructors and the values are triples
                target-domain coercion-type function
where target-domain is the top-level constructor of the target,
coercion-type is one of 'total, 'partial or 'indeterm, and
function is the name of the function to call to handle the
coercion. coercion-type is used by canCoerce and friends: 'total
means that a coercion can definitely be performed, 'partial means
that one cannot tell whether a coercion can be performed unless
you have the actual data (like telling whether a Polynomial Integer
can be coerced to an Integer: you have to know whether it is a
constant polynomial), and 'indeterm means that you might be able
to tell without data, but you need to call the function with the
argument "$fromCoerceable$" for a response of true or false. As an
example of this last kind, you may be able to coerce a list to a
vector but you have to know what the underlying types are. So
List Integer is coerceable to Vector Integer but List Float is
not necessarily coerceable to Vector Integer.

The functions always take three arguments:
     value                this is the unwrapped source object
     source-type          this is the type of the source
     target-type          this is the requested type of the target
For ethical reasons and to avoid eternal damnation, we try to use
library functions to perform a lot of the structure manipulations.
However, we sometimes cheat for efficiency reasons, particularly to
avoid intermediate instantiations.

the following are older comments:

This file  contains the special  coercion routines that  convert from
one  datatype to  another  in the  interpreter.   The  choice of  the
primary special routine is made by  the function coerceByTable.  Note
that not all coercions use these functions, as some are done via SPAD
algebra code  and controlled by  the function coerceByFunction.   See
the file COERCE BOOT for more information.

some assumption about the call of commute and embed functions:
embed functions are called for one level embedding only,
  e.g. I to P I, but not I to P G I
commute functions are called for two types which differ only in the
  permutation of the two top type constructors
  e.g. G P RN to P G RN, but not G P I to P G RN or
    P[x] G RN to G P RN

all functions in this file should call canCoerce and coerceInt, as
  opposed to canCoerceFrom and coerceInteractive

all these coercion functions have the following result:
1. if u=$fromCoerceable$, then TRUE or NIL
2. if the coercion succeeds, the coerced value (this may be NIL)
3. if the coercion fails, they throw to a catch point in
     coerceByTable
)endif

DEFPARAMETER($coerceFailure, GENSYM())

position1(x,y) ==
  -- this is used where we want to assume a 1-based index
  1 + position(x,y)

--% Direct Product, New and Old

DP2DP(u,source is [.,n,S],target is [.,m,T]) ==
  n ~= m => nil
  u = '_$fromCoerceable_$ => canCoerce(S,T)
  null (u' := coerceInt(objNewWrap(u,['Vector,S]),['Vector,T])) =>
    coercionFailure()
  objValUnwrap u'

--% Distributed Multivariate Polynomials, New and Old

Dmp2Dmp(u,source is [dmp,v1,S], target is [.,v2,T]) ==
  -- the variable lists must share some variables, or u is a constant
  u = '_$fromCoerceable_$ =>
    v:= intersection(v1,v2)
    v and
      w2:= SETDIFFERENCE(v2,v)
      t1:= if w1 then [dmp,w1,S] else S
      t2:= if w2 then [dmp,w2,T] else T
      canCoerce(t1,t2)
  null u => domainZero(target)
  u is [[e,:c]] and e=LIST2VEC [0 for v in v1] =>
    z:= coerceInt(objNewWrap(c,S),target) => objValUnwrap(z)
    coercionFailure()
  v:= intersection(v1,v2) =>
    w1:= SETDIFFERENCE(v1,v) =>
      coerceDmp1(u,source,target,v,w1)
    coerceDmp2(u,source,target)
  coercionFailure()

coerceDmp1(u,source is [.,v1,S],target is [.,v2,T],v,w) ==
  -- coerces one Dmp to another, where v1 is not a subset of v2
  -- v is the intersection, w the complement of v1 and v2
  t:= ['DistributedMultivariatePolynomial,w,S]
  x:= domainZero(target)
  one:= domainOne(T)
  plusfunc:= getFunctionFromDomain('_+,target,[target,target])
  multfunc:= getFunctionFromDomain('_*,target,[target,target])
  pat1:= [member(x,v) for x in v1]
  pat2:= [member(x,w) for x in v1]
  pat3:= [member(x,v) and POSN1(x,v) for x in v2]
  for [e,:c] in u until not z repeat
    exp:= LIST2VEC [y for x in pat2 for y in VEC2LIST e | x]
    z:= coerceInt(objNewWrap([CONS(exp,c)],t),target) =>
      li:= [y for x in pat1 for y in VEC2LIST e | x]
      a:= [CONS(LIST2VEC [if x then li.x else 0 for x in pat3],one)]
      x:= SPADCALL(x,SPADCALL(objValUnwrap(z),a,multfunc),plusfunc)
  z => x
  coercionFailure()

coerceDmp2(u,source is [.,v1,S],target is [.,v2,T]) ==
  -- coerces one Dmp to another, where v1 is included in v2
  x:= domainZero(target)
  one:= domainOne(T)
  plusfunc:= getFunctionFromDomain('_+,target,[target,target])
  multfunc:= getFunctionFromDomain('_*,target,[target,target])
  pat:= [member(x,v1) and POSN1(x,v1) for x in v2]
  for [e,:c] in u until not z repeat
    z:= coerceInt(objNewWrap(c,S),target) =>
      li:= VEC2LIST e
      a:= [CONS(LIST2VEC [if x then li.x else 0 for x in pat],one)]
      x:= SPADCALL(x,SPADCALL(objValUnwrap(z),a,multfunc),plusfunc)
    NIL
  z => x
  coercionFailure()

Dmp2Expr(u,source is [dmp,vars,S], target is [Expr,T]) ==
    u = '_$fromCoerceable_$ => canCoerce(S, target)

    null vars =>
        [[., :c]] := u
        not (c := coerceInt(objNewWrap(c, S), target)) => coercionFailure()
        objValUnwrap(c)

    syms := [objValUnwrap coerceInt(objNewWrap(var, $Symbol), target) for
                var in vars]
    sum := domainZero(target)

    plus := getFunctionFromDomain("+",  target, [target, target])
    mult := getFunctionFromDomain("*",  target, [target, target])
    expn := getFunctionFromDomain("^", target, [target, $Integer])

    for [e, :c] in u repeat
        not (c := coerceInt(objNewWrap(c, S), target)) => coercionFailure()
        c := objValUnwrap(c)
        term := domainOne(target)
        for i in 0.. for sym in syms repeat
            exp := e.i
            e.i > 0 => term := SPADCALL(term, SPADCALL(sym, e.i, expn), mult)
        sum := SPADCALL(sum, SPADCALL(c, term, mult), plus)

    sum

Dmp2Mp(u, source is [dmp, x, S], target is [mp, y, T]) ==
  source' := [dmp,y,T]
  u = '_$fromCoerceable_$ =>
    x = y => canCoerce(S,T)
    canCoerce(source',target)
  null u => domainZero(target)  -- 0 dmp is = nil
  x ~= y =>
    (u' := coerceInt(objNewWrap(u,source),source')) or coercionFailure()
    (u' := coerceInt(u',target)) or coercionFailure()
    objValUnwrap(u')

  -- slight optimization for case #u = 1, x=y , #x =1 and S=T
  -- I know it's pathological, but it may avoid an instantiation
  (x=y) and (1 = #u) and (1 = #x) and (S = T) =>
    [1,1,[(CAAR u).0,0,:CDAR u]]

  (u' := coerceDmpCoeffs(u,S,T)) = 'failed =>
    coercionFailure()
  plusfunc := getFunctionFromDomain("+",target,[target,target])
  u'' := genMpFromDmpTerm(u'.0, 0)
  for i in 1..(#u' - 1) repeat
    u'' := SPADCALL(u'',genMpFromDmpTerm(u'.i, 0),plusfunc)
  u''

coerceDmpCoeffs(u,S,T) ==
  -- u is a dmp, S is domain of coeffs, T is domain to coerce coeffs to
  S = T => u
  u' := nil
  bad := nil
  for [e,:c] in u repeat
    bad => nil
    null (c' := coerceInt(objNewWrap(c,S),T)) => return (bad := true)
    u' := [[e,:objValUnwrap(c')],:u']
  bad => 'failed
  nreverse u'

sortAndReorderDmpExponents(u,vl) ==
  vl' := reverse MSORT vl
  n := (-1) + #vl
  pos := LIST2VEC LZeros (n+1)
  for i in 0..n repeat pos.i := position(vl.i,vl')
  u' := nil
  for [e,:c] in u repeat
    e' := LIST2VEC LZeros (n+1)
    for i in 0..n repeat e'.(pos.i) := e.i
    u' := [[e',:c],:u']
  reverse u'

domain2NDmp(u, source, target is [., y, T]) ==
  target' := ['DistributedMultivariatePolynomial,y,T]
  u = '_$fromCoerceable_$ => canCoerce(source,target')
  (u' := coerceInt(objNewWrap(u,source),target')) =>
    (u'' := coerceInt(u',target)) =>
      objValUnwrap(u'')
    coercionFailure()
  coercionFailure()

Dmp2NDmp(u,source is [dmp,x,S],target is [ndmp,y,T]) ==
  -- a null DMP = 0
  null u => domainZero(target)
  target' := [dmp,y,T]
  u = '_$fromCoerceable_$ => Dmp2Dmp(u,source,target')
  (u' := Dmp2Dmp(u,source,target')) => addDmpLikeTermsAsTarget(u',target)
  coercionFailure()

addDmpLikeTermsAsTarget(u,target) ==
  u' := domainZero(target)
  func := getFunctionFromDomain("+",target,[target,target])
  for t in u repeat u' := SPADCALL(u',[t],func)
  u'

-- rewrite ?
Dmp2P(u, source is [dmp,vl, S], target is [.,T]) ==
  -- a null DMP = 0
  null u => domainZero(target)
  u = '_$fromCoerceable_$ =>
    t := canCoerce(S,T)
    null t => canCoerce(S,target)
    t

  S is ['Polynomial,.] =>
    mp := coerceInt(objNewWrap(u,source),['MultivariatePolynomial,vl,S])
      or coercionFailure()
    p := coerceInt(mp,target) or coercionFailure()
    objValUnwrap p

  -- slight optimization for case #u = 1, #vl =1 and S=T
  -- I know it's pathological, but it may avoid an instantiation
  (1 = #u) and (1 = #vl) and (S = T) =>
    -- scalar
    (lexp:= (CAAR u).0) = 0 =>
       [0,:CDAR u]
    [1,vl.0,[lexp,0,:CDAR u]]

  vl' := reverse MSORT vl
  source' := [dmp,vl',S]
  target' := ['MultivariatePolynomial,vl',S]
  u' := sortAndReorderDmpExponents(u,vl)
  u' := coerceInt(objNewWrap(u',source'),target')
  if u' then
    u' := translateMpVars2PVars (objValUnwrap(u'),vl')
    u' := coerceInt(objNewWrap(u',['Polynomial,S]),target)
  u' => objValUnwrap(u')
  -- get drastic. create monomials
  source' := [dmp,vl,T]
  u' := domainZero(target)
  oneT := domainOne(T)
  plusfunc := getFunctionFromDomain("+",target,[target,target])
  multfunc := getFunctionFromDomain("*",target,[target,target])
  for [e,:c] in u repeat
    (c' := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    (e' := coerceInt(objNewWrap([[e,:oneT]],source'),target)) or
      coercionFailure()
    t := SPADCALL(objValUnwrap(e'),objValUnwrap(c'),multfunc)
    u' := SPADCALL(u',t,plusfunc)
  coercionFailure()

translateMpVars2PVars (u, vl) ==
  u is [ =1, v, :termlist] =>
    [ 1, vl.(v-1),
      :[[e,:translateMpVars2PVars(c,vl)] for [e,:c] in termlist]]
  u

Dmp2Up(u, source is [dmp,vl,S],target is [up,var,T]) ==
  null u =>    -- this is true if u = 0
    domainZero(target)

  u = '_$fromCoerceable_$ =>
    member(var,vl) =>
      vl' := remove(vl,var)
      null vl' =>         -- no remaining variables
        canCoerce(S,T)
      null rest vl' =>    -- one remaining variable
        canCoerce([up,first vl',S],T)
      canCoerce([dmp,vl',S], T)
    canCoerce(source,T)

  -- check constant case
  (null rest u) and (first(u) is [e,:c]) and
    ( and/[(0 = e.i) for i in 0..(-1 + #vl)] ) =>
      (x := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
      objValUnwrap(x)

  -- check non-member case
  null member(var,vl) =>
    (u' := coerceInt(objNewWrap(u,source),T)) or coercionFailure()
    [[0,:objValUnwrap u']]

  vl' := remove(vl,var)

  -- only one variable in DMP case
  null vl' =>
    u' := nreverse SORTBY('CAR,[[e.0,:c] for [e,:c] in u])
    (u' := coerceInt(objNewWrap(u',[up,var,S]),target)) or
      coercionFailure()
    objValUnwrap u'

  S1 := [dmp,vl',S]
  plusfunc:= getFunctionFromDomain('_+,T,[T,T])
  zero := getConstantFromDomain('(Zero),T)
  x := NIL
  pos:= POSN1(var,vl)
  for [e,:c] in u until not y repeat
    exp:= e.pos
    e1:= removeVectorElt(e,pos)
    y:= coerceInt(objNewWrap([[e1,:c]],S1),T) =>
      -- need to be careful about zeros
      p:= ASSQ(exp,x) =>
        c' := SPADCALL(rest p, objValUnwrap(y), plusfunc)
        c' = zero => x := REMALIST(x,exp)
        RPLACD(p,c')
      zero = objValUnwrap(y) => 'iterate
      x := CONS(CONS(exp,objValUnwrap(y)),x)
  y => nreverse SORTBY('CAR,x)
  coercionFailure()

removeVectorElt(v,pos) ==
  -- removes the pos'th element from vector v
  LIST2VEC [x for x in VEC2LIST v for y in 0.. | not (y=pos)]

removeListElt(l,pos) ==
  pos = 0 => rest l
  [first l, :removeListElt(rest l, pos - 1)]

NDmp2domain(u,source is [ndmp,x,S],target) ==
  -- a null NDMP = 0
  null u => domainZero(target)
  dmp := 'DistributedMultivariatePolynomial
  source' := [dmp,x,S]
  u = '_$fromCoerceable_$ => canCoerce(source',target)
  u' := addDmpLikeTermsAsTarget(u,source')
  (u'' := coerceInt(objNewWrap(u',source'),target)) =>
    objValUnwrap(u'')
  coercionFailure()

NDmp2NDmp(u,source is [ndmp,x,S],target is [.,y,T]) ==
  -- a null NDMP = 0
  null u => domainZero(target)
  dmp := 'DistributedMultivariatePolynomial
  source' := [dmp,x,S]
  target' := [dmp,y,T]
  u = '_$fromCoerceable_$ => canCoerce(source',target')
  u' := addDmpLikeTermsAsTarget(u,source')
  (u'' := coerceInt(objNewWrap(u',source'),target')) =>
    addDmpLikeTermsAsTarget(objValUnwrap(u''),target)
  coercionFailure()

--% Expression

Expr2Complex(u,source is [.,S], target is [.,T]) ==
    u = '_$fromCoerceable_$ => nil   -- can't tell, in general

    not member(S, [$Integer, $Float, $DoubleFloat]) => coercionFailure()
    not member(T, [$Float, $DoubleFloat]) => coercionFailure()

    complexNumeric := getFunctionFromDomain("complexNumeric", ['Numeric, S], [source])

    -- the following might fail
    cf := SPADCALL(u,complexNumeric)  -- returns a Float
    T = $DoubleFloat =>
        null (z := coerceInt(objNewWrap(cf, ['Complex, $Float]), ['Complex, $DoubleFloat])) =>
            coercionFailure()
        objValUnwrap z
    cf

Expr2Dmp(u,source is [Expr,S], target is [dmp,v2,T]) ==
    u = '_$fromCoerceable_$ => canCoerce(source, T)

    null v2 =>
        not (z := coerceInt(objNewWrap(u, source), T)) => coercionFailure()
        [[LIST2VEC NIL, :objValUnwrap z]]

    obj := objNewWrap(u, source)
    univ := coerceInt(obj, ['UnivariatePolynomial, first v2, T])
    not univ =>
        T = source => coercionFailure()
        not (z := coerceInt(obj, [dmp, v2, source])) =>
            coercionFailure()
        z := objValUnwrap z
        for term in z repeat
            [., :c] := term
            not (c := coerceInt(objNewWrap(c, source), T)) => coercionFailure()
            RPLACD(term, objValUnwrap c)
        z

    univ := objValUnwrap univ

    -- only one variable

    null rest v2 =>
        for term in univ repeat
            RPLACA(term, VECTOR first term)
        univ

    -- more than one variable

    summands := nil
    for [e,:c] in univ repeat
        summands := Expr2Dmp1(summands,
            LIST2VEC [e, :[0 for v in rest v2]], c, T, 1, rest v2, T)

    plus := getFunctionFromDomain("+", target, [target, target])
    sum  := domainZero target
    for summand in summands repeat
        sum := SPADCALL([summand], sum, plus)
    sum

Expr2Dmp1(summands, vec, c, source, index, varList, T) ==
    if null varList then
        if not (source = T) then
            not (c := coerceInt(objNewWrap(c, source), T)) => coercionFailure()
            c := objValUnwrap c
        summands := [[vec, :c], :summands]
    else
        univ := coerceInt(objNewWrap(c, source),
            ['UnivariatePolynomial, first varList, T])
        univ := objValUnwrap univ

        for [e,:c] in univ repeat
            vec := COPY_-SEQ vec
            vec.index := e
            summands := Expr2Dmp1(summands, vec, c, T, index+1, rest varList, T)
    summands

Expr2Mp(u,source is [Expr,S], target is [.,v2,T]) ==
    u = '_$fromCoerceable_$ => canCoerce(source, T)

    dmp := ['DistributedMultivariatePolynomial,v2,T]
    d   := Expr2Dmp(u,source, dmp)
    not (m := coerceInt(objNewWrap(d, dmp), target)) => coercionFailure()
    objValUnwrap m

Expr2Up(u,source is [Expr,S], target is [.,var,T]) ==
    u = '_$fromCoerceable_$ => canCoerce(source, T)
    kernelFunc := getFunctionFromDomain("kernels", source, [source])
    kernelDom  := ['Kernel, source]
    nameFunc   := getFunctionFromDomain("name", kernelDom, [kernelDom])
    kernels    := SPADCALL(u,kernelFunc)
    v1         := [SPADCALL(kernel, nameFunc) for kernel in kernels]

    not member(var, v1) => coercionFailure()

    -- variable is a kernel

    varKernel  := kernels.(POSN1(var, v1))
    univFunc   := getFunctionFromDomain("univariate", source, [source, kernelDom])
    sup        := ['SparseUnivariatePolynomial, source]

    fracUniv   := SPADCALL(u, varKernel, univFunc)
    denom      := rest fracUniv

    not equalOne(denom, sup) => coercionFailure()

    numer      := first fracUniv
    uniType := ['UnivariatePolynomial, var, source]
    (z := coerceInt(objNewWrap(numer, uniType), target)) => objValUnwrap z
    coercionFailure()

--% Kernels over Expr

Ker2Ker(u,source is [.,S], target is [.,T]) ==
  u = '_$fromCoerceable_$ => canCoerce(S, T)
  not (m := coerceInt(objNewWrap(u, source), S)) => coercionFailure()
  u' := objValUnwrap m
  not (m' := coerceInt(objNewWrap(u', S), T)) => coercionFailure()
  u'' := objValUnwrap m'
  not (m'' := coerceInt(objNewWrap(u'', T), target)) => coercionFailure()
  objValUnwrap m''

Ker2Expr(u,source is [.,S], target) ==
  u = '_$fromCoerceable_$ => canCoerce(S, target)
  not (m := coerceByFunction(objNewWrap(u, source), S)) => coercionFailure()
  u':= objValUnwrap m
  not (m' := coerceInt(objNewWrap(u', S), target)) => coercionFailure()
  objValUnwrap m'


--% Factored objects

Factored2Factored(u,oldmode,newmode) ==
  [.,oldargmode,:.]:= oldmode
  [.,newargmode,:.]:= newmode
  u = '_$fromCoerceable_$ => canCoerce(oldargmode,newargmode)
  u' := unwrap u
  unit' := coerceInt(objNewWrap(first u',oldargmode),newargmode)
  null unit' => coercionFailure()
  factors := IFCDR u'
  factors' := [(coerceFFE(x,oldargmode,newargmode)) for x in factors]
  member('failed,factors') => coercionFailure()
  [objValUnwrap(unit'),:factors']

coerceFFE(ffe, oldmode, newmode) ==
  fac' := coerceInt(objNewWrap(ffe.1,oldmode),newmode)
  null fac' => 'failed
  LIST2VEC [ffe.0,objValUnwrap(fac'),ffe.2]

--% Complex

Complex2underDomain(u,[.,S],target) ==
  u = '_$fromCoerceable_$ => nil
  [r,:i] := u
  i=domainZero(S) =>
    [r',.,.]:= coerceInt(objNewWrap(r,S),target) or
      coercionFailure()
    r'
  coercionFailure()

Complex2FR(u,S is [.,R],target is [.,T]) ==
  u = '_$fromCoerceable_$ =>
    S ~= T => nil
    R = $Integer => true
    nil
  S ~= T => coercionFailure()
  package :=
    R = $Integer => ['GaussianFactorizationPackage]
    coercionFailure()
  factor := getFunctionFromDomain('factor,package,[S])
  SPADCALL(u,factor)

Complex2Expr(u, source is [.,S], target is [., T]) ==
  u = '_$fromCoerceable_$ =>
    T is ['Complex, T1] and canCoerceFrom(S, T1) or coercionFailure()
  E := defaultTargetFE source
  negOne := coerceInt(objNewWrap(-1, $Integer), E)
  null negOne => coercionFailure()
  sqrtFun := getFunctionFromDomain('sqrt, E, [E])
  i := SPADCALL(objValUnwrap negOne, sqrtFun)
  realFun := getFunctionFromDomain('real, source, [source])
  imagFun := getFunctionFromDomain('imag, source, [source])
  real := SPADCALL(u, realFun)
  imag := SPADCALL(u, imagFun)
  realExp := coerceInt(objNewWrap(real, S), E)
  null realExp => coercionFailure()
  imagExp := coerceInt(objNewWrap(imag, S), E)
  null imagExp => coercionFailure()
  timesFun := getFunctionFromDomain('_*, E, [E, E])
  plusFun  := getFunctionFromDomain('_+, E, [E, E])
  newVal := SPADCALL(objValUnwrap(realExp),
             SPADCALL(i, objValUnwrap imagExp, timesFun), plusFun)
  newObj := objNewWrap(newVal, E)
  finalObj := coerceInt(newObj, target)
  finalObj => objValUnwrap finalObj
  coercionFailure()

--% Integer

I2EI(n,source,target) ==
  n = '_$fromCoerceable_$ => nil
  if not ODDP(n) then n else coercionFailure()

I2OI(n,source,target) ==
  n = '_$fromCoerceable_$ => nil
  if ODDP(n) then n else coercionFailure()

I2PI(n,source,target) ==
  n = '_$fromCoerceable_$ => nil
  if n > 0 then n else coercionFailure()

I2NNI(n,source,target) ==
  n = '_$fromCoerceable_$ => nil
  if n >= 0 then n else coercionFailure()

--% List

L2Tuple(val, source is [.,S], target is [.,T]) ==
    val = '_$fromCoerceable_$ => canCoerce(S,T)
    null (object := coerceInt1(mkObjWrap(val,source), ['List, T])) =>
      coercionFailure()
    asTupleNew0 objValUnwrap object

L2DP(l, source is [.,S], target is [.,n,T]) ==
  -- need to know size of the list
  l = '_$fromCoerceable_$ => nil
  n ~= SIZE l => coercionFailure()
  (v := coerceInt(objNewWrap(LIST2VEC l,['Vector,S]),['Vector,T])) or
    coercionFailure()
  V2DP(objValUnwrap v, ['Vector, T], target)

V2DP(v, source is [.,S], target is [.,n,T]) ==
  -- need to know size of the vector
  v = '_$fromCoerceable_$ => nil
  n ~= SIZE v => coercionFailure()
  (v1 := coerceInt(objNewWrap(v,source),['Vector,T])) or
    coercionFailure()
  dpFun  := getFunctionFromDomain('directProduct, target, [['Vector,T]])
  SPADCALL(objValUnwrap v1, dpFun)

L2V(l, source is [.,S], target is [.,T]) ==
  l = '_$fromCoerceable_$ => canCoerce(S,T)
  (v := coerceInt(objNewWrap(LIST2VEC l,['Vector,S]),target)) or
    coercionFailure()
  objValUnwrap(v)

V2L(v, source is [.,S], target is [.,T]) ==
  v = '_$fromCoerceable_$ => canCoerce(S,T)
  (l := coerceInt(objNewWrap(VEC2LIST v,['List,S]),target)) or
    coercionFailure()
  objValUnwrap(l)

L2M(u,[.,D],[.,R]) ==
  u = '_$fromCoerceable_$ => nil
  D is ['List,E] and isRectangularList(u, n := #u, m :=# first u) =>
    v := MAKE_MATRIX(n, m)
    for x in u for i in 0..(n-1) repeat
      for y in x for j in 0..(m-1) repeat
        (y' := coerceInt(objNewWrap(y,E),R)) or coercionFailure()
        QSETAREF2O(v, i, j, objValUnwrap(y'), 0, 0)
    v
  coercionFailure()

L2Record(l,[.,D],[.,:al]) ==
  l = '_$fromCoerceable_$ => nil
  #l = #al =>
    v:= [u for x in l for [":",.,D'] in al] where u ==
      T:= coerceInt(objNewWrap(x,D),D') or return 'failed
      objValUnwrap(T)
    v = 'failed => coercionFailure()
    #v = 2 => [v.0,:v.1]
    LIST2VEC v
  coercionFailure()

L2Rm(u,source is [.,D],target is [.,n,m,R]) ==
  u = '_$fromCoerceable_$ => nil
  D is ['List,E] and isRectangularList(u,n,m) =>
    L2M(u,source,['Matrix,R])
  coercionFailure()

L2Sm(u,source is [.,D],[.,n,R]) ==
  u = '_$fromCoerceable_$ => nil
  D is ['List,E] and isRectangularList(u,n,n) =>
    L2M(u,source,['Matrix,R])
  coercionFailure()

L2Set(x,source is [.,S],target is [.,T]) ==
  x = '_$fromCoerceable_$ => canCoerce(S,T)
  -- call library function  brace  to get a set
  target' := ['Set,S]
  u := objNewWrap(
    SPADCALL(x,getFunctionFromDomain('brace,target',[source])),
      target')
  (u := coerceInt(u,target)) or coercionFailure()
  objValUnwrap u

Set2L(x,source is [.,S],target is [.,T]) ==
  x = '_$fromCoerceable_$ => canCoerce(S,T)
  -- call library function  destruct  to get a list
  u := objNewWrap(
    SPADCALL(x,getFunctionFromDomain('destruct,source,[source])),
      ['List,S])
  (u := coerceInt(u,target)) or coercionFailure()
  objValUnwrap u

Agg2Agg(x,source is [agg1,S],target is [.,T]) ==
  x = '_$fromCoerceable_$ => canCoerce(S,T)
  S = T => coercionFailure()         -- library function
  target' := [agg1,T]
  (u := coerceInt(objNewWrap(x,source),target')) or coercionFailure()
  (u := coerceInt(u,target)) or coercionFailure()
  objValUnwrap u

Agg2L2Agg(x,source is [.,S],target) ==
  -- tries to use list as an intermediate type
  mid := ['List,S]
  x = '_$fromCoerceable_$ =>
    canCoerce(source,mid) and canCoerce(mid,target)
  (u := coerceInt(objNewWrap(x,source),mid)) or coercionFailure()
  (u := coerceInt(u,target)) or coercionFailure()
  objValUnwrap u

isRectangularList(x,p,q) ==
  p=0 or p=#x =>
    n:= #first x
    and/[n=#y for y in rest x] => p=0 or q=n

--% Matrix

M2VV(x) ==
    n := ANROWS(x)
    m := ANCOLS(x)
    v := MAKE_-ARRAY(n)
    for i in 0..(n - 1) repeat
        vi := MAKE_-ARRAY(m)
        for j in 0..(m - 1) repeat
            QSETAREF1(vi, j, QAREF2O(x, i, j, 0, 0))
        QSETAREF1(v, i, vi)
    v

M2L(x,[.,S],target) ==
  mid := ['Vector,['Vector,S]]
  x = '_$fromCoerceable_$ => canCoerce(mid,target)
  (u := coerceInt(objNewWrap(M2VV x, mid), target)) or coercionFailure()
  objValUnwrap u

M2M(x,[.,R],[.,S]) ==
    x = '_$fromCoerceable_$ => canCoerce(R,S)
    n := ANROWS(x)
    m := ANCOLS(x)
    v := MAKE_MATRIX(n, m)
    for i in 0..(n - 1) repeat
        for j in 0..(m - 1) repeat
            y := QAREF2O(x, i, j, 0, 0)
            (y' := coerceInt(objNewWrap(y, R), S)) or coercionFailure()
            QSETAREF2O(v, i, j, objValUnwrap y', 0, 0)
    v

M2Rm(x,source is [.,R],[.,p,q,S]) ==
    x = '_$fromCoerceable_$ => nil
    n := ANROWS(x)
    m := ANCOLS(x)
    n = p and m = q => M2M(x, source, [nil, S])
    coercionFailure()

M2Sm(x,source is [.,R],[.,p,S]) ==
    x = '_$fromCoerceable_$ => nil
    n := ANROWS(x)
    m := ANCOLS(x)
    n = m and m = p => M2M(x, source, [nil, S])
    coercionFailure()

M2V(x,[.,S],target) ==
  mid := ['Vector,['Vector,S]]
  x = '_$fromCoerceable_$ =>  canCoerce(mid,target)
  (u := coerceInt(objNewWrap(M2VV x, mid), target)) or coercionFailure()
  objValUnwrap u

--% Multivariate Polynomial

Mp2Dmp(u, source is [., x, S], target is [dmp, y, T]) ==
  -- Change the representation to a DMP with the same variables and
  -- coerce.
  target' := [dmp,x,S]
  u = '_$fromCoerceable_$ => canCoerce(target',target)

  -- check if we have a constant
  u is [ =0,:c] =>
    null (u' := coerceInt(objNewWrap(c,S),target)) =>
      coercionFailure()
    objValUnwrap(u')

  plus := getFunctionFromDomain('_+,target',[target',target'])
  mult := getFunctionFromDomain('_*,target',[target',target'])
  one := domainOne(S)
  zero := domainZero(S)
  (u' := coerceInt(objNewWrap(Mp2SimilarDmp(u,S,#x,plus,mult,one,zero),
    target'),target)) or coercionFailure()
  objValUnwrap(u')

Mp2SimilarDmp(u,S,n,plus,mult,one,zero) ==
  u is [ =0,:c] =>
    c = zero => NIL  -- zero for dmp
    [[LIST2VEC LZeros n,:c]]
  u is [ =1,x,:terms] =>
    u' := NIL  -- zero for dmp
    for [e,:c] in terms repeat
      e' := LIST2VEC LZeros n
      e'.(x-1) := e
      t := [[e',:one]]
      t := SPADCALL(t,Mp2SimilarDmp(c,S,n,plus,mult,one,zero),mult)
      u' := SPADCALL(u',t,plus)
    u'

Mp2Expr(u,source is [mp,vars,S], target is [Expr,T]) ==
    u = '_$fromCoerceable_$ => canCoerce(S, target)

    dmp := ['DistributedMultivariatePolynomial, vars, S]
    not (d := coerceInt(objNewWrap(u, source), dmp)) => coercionFailure()
    Dmp2Expr(objValUnwrap d, dmp, target)

Mp2FR(u,S is [.,vl,R],[.,T]) ==
  u = '_$fromCoerceable_$ =>
    S ~= T => nil
    R in '((Integer) (Fraction (Integer))) => true
    nil
  S ~= T => coercionFailure()
  package :=
    R = $Integer =>
      ovl := ['OrderedVariableList, vl]
      ['MultivariateFactorize,ovl, ['IndexedExponents, ovl],R,S]
    R is ['Fraction, D] =>
      ovl := ['OrderedVariableList, vl]
      package := ['MRationalFactorize,['IndexedExponents, ovl], ovl, D, S]
    coercionFailure()
  factor := getFunctionFromDomain('factor,package,[S])
  SPADCALL(u,factor)

Mp2Mp(u,source is [mp,x,S], target is [.,y,T]) ==
  -- need not deal with case of x = y (coerceByMapping)
  common := intersection(y,x)
  x' := SETDIFFERENCE(x,common)
  y' := SETDIFFERENCE(y,common)

  u = '_$fromCoerceable_$ =>
    x = y => canCoerce(S,T)
    null common => canCoerce(source,T)
    null x' => canCoerce(S,target)
    null y' => canCoerce([mp,x',S],T)
    canCoerce([mp,x',S],[mp,y',T])

  -- first check for constant case
  u is [ =0,:c] =>
    (u' := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    objValUnwrap(u')

  plus  := getFunctionFromDomain('_+,target,[target,target])

  -- now no-common-variables case

  null common =>
    times := getFunctionFromDomain('_*,target,[target,target])
    expn  := getFunctionFromDomain("^", target,
      [target,$NonNegativeInteger])
    Mp2MpAux0(u,S,target,x,plus,times,expn)

  -- if source vars are all in target
  null x' =>
    monom := getFunctionFromDomain('monomial,target,
      [target,['OrderedVariableList,y],$NonNegativeInteger])
    Mp2MpAux1(u,S,target,x,y,plus,monom)

  -- if target vars are all in source
  null y' =>    -- change source to MP[common] MP[x'] S
    univariate := getFunctionFromDomain('univariate,
      source,[source,['OrderedVariableList,x]])
    u' := Mp2MpAux2(u,x,common,x',common,x',univariate,S,NIL)
    (u' := coerceInt(objNewWrap(u', [mp,common,[mp,x',S]]),target)) or
      coercionFailure()
    objValUnwrap(u')

  -- we have a mixture
  (u' := coerceInt(objNewWrap(u,source),[mp,common,[mp,x',S]])) or
    coercionFailure()
  (u' := coerceInt(u',target)) or coercionFailure()
  objValUnwrap(u')

Mp2MpAux0(u,S,target,vars,plus,times,expn) ==
  -- for case when no common variables
  u is [ =0,:c] =>
    (u' := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    objValUnwrap(u')
  [.,var,:terms] := u
  [mp,.,T] := target
  x := coerceInt(objNewWrap(vars.(var-1),['Variable,vars.(var-1)]),
    [mp,vars,$Integer]) or coercionFailure()
  (x := coerceInt(x,T)) or coercionFailure()
  x := [0,:objValUnwrap x]
  sum := domainZero(target)
  for [e,:c] in terms repeat
    prod := SPADCALL(SPADCALL(x,e,expn),
      Mp2MpAux0(c,S,target,vars,plus,times,expn),times)
    sum := SPADCALL(sum,prod,plus)
  sum

Mp2MpAux1(u,S,target,varl1,varl2,plus,monom) ==
  -- for case when source vars are all in target
  u is [ =0,:c] =>
    (u' := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    objValUnwrap(u')
  [.,var,:terms] := u
  sum := domainZero(target)
  for [e,:c] in terms repeat
    mon := SPADCALL( Mp2MpAux1(c,S,target,varl1,varl2,plus,monom),
      position1(varl1.(var-1), varl2),e,monom)
    sum := SPADCALL(sum,mon,plus)
  sum

Mp2MpAux2(u,x,oldcomm,oldrest,common,restvars,univariate,S,isUnder) ==
  -- target vars are all in source
  mp2 := ['MultivariatePolynomial,oldcomm,['MultivariatePolynomial,
    oldrest,S]]
  common =>
    u is [ =0,:c] =>
      (u' := coerceInt(objNewWrap(c,S),mp2)) or coercionFailure()
      objValUnwrap(u')
    [var,:common] := common
    u' := SPADCALL(u,position1(var,x),univariate)
    null(rest(u')) and (first(first(u')) = 0) =>
      Mp2MpAux2(u,x,oldcomm,oldrest,common,restvars,univariate,S,isUnder)
    [1,position1(var,oldcomm),:[[e,:Mp2MpAux2(c,x,oldcomm,oldrest,
      common,restvars,univariate,S,isUnder)] for [e,:c] in u']]
  null isUnder =>
    [0,:Mp2MpAux2(u,x,oldcomm,oldrest,common,restvars,univariate,S,true)]
  -- just treat like elt of [mp,x',S]
  u is [ =0,:c] => u
  [var,:restvars] := restvars
  u' := SPADCALL(u,position1(var,x),univariate)
  null(rest(u')) and (first(first(u')) = 0) =>
    Mp2MpAux2(u,x,oldcomm,oldrest,common,restvars,univariate,S,isUnder)
  [1,position1(var,oldrest),:[[e,:Mp2MpAux2(c,x,oldcomm,oldrest,
    common,restvars,univariate,S,isUnder)] for [e,:c] in u']]

genMpFromDmpTerm(u, oldlen) ==

  -- given one term of a DMP representation of a polynomial, this creates
  -- the corresponding MP term.

  patlen := oldlen
  [e,:c] := u
  numexps := # e
  patlen >= numexps => [0, :c]
  for i in patlen..(numexps - 1) repeat
    e.i = 0 => patlen := patlen + 1
    return nil
  patlen >= numexps => [0, :c]
  [1, 1+patlen, [e.patlen,:genMpFromDmpTerm(u,patlen+1)]]

Mp2P(u, source is [mp,vl, S], target is [p,R]) ==
  u = '_$fromCoerceable_$ => canCoerce(S,target)
  S is ['Polynomial,.] => MpP2P(u,vl,S,R)
  vl' := REVERSE MSORT vl
  -- if Mp2Mp fails, a THROW will occur
  u' := Mp2Mp(u,source,[mp,vl',S])
  u' := translateMpVars2PVars (u',vl')
  (u' := coerceInt(objNewWrap(u',[p,S]),target)) or coercionFailure()
  objValUnwrap(u')

MpP2P(u,vl,PS,R) ==
  -- u has type MP(vl,PS). Want to coerce to P R.
  PR := ['Polynomial,R]
  u is [ =0,:c] =>
    (u' :=coerceInt(objNewWrap(c,PS),PR)) or
      coercionFailure()
    objValUnwrap u'
  [ .,pos,:ec] := u
  multivariate := getFunctionFromDomain('multivariate,
    PR,[['SparseUnivariatePolynomial,PR],$Symbol])
  sup := [[e,:MpP2P(c,vl,PS,R)] for [e,:c] in ec]
  p := SPADCALL(sup,vl.(pos-1),multivariate)
  --(p' :=coerceInt(objNewWrap(p,PS),['Polynomial,R])) or coercionFailure()
  --objValUnwrap(p')

Mp2Up(u,source is [mp,vl,S],target is [up,x,T]) ==
  u = '_$fromCoerceable_$ =>
    member(x,vl) =>
      vl = [x] => canCoerce(S,T)
      canCoerce([mp,delete(x,vl),S],T)
    canCoerce(source,T)

  u is [ =0,:c] =>      -- constant polynomial?
    (u' := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    objValUnwrap u'

  null member(x,vl) =>
    (u' := coerceInt(objNewWrap(u,source),T)) or coercionFailure()
    [[0,:objValUnwrap(u')]]

  vl = [x] =>
    u' := [[e,:c] for [e,.,:c] in CDDR u]
    (u' := coerceInt(objNewWrap(u',[up,x,S]),target))
      or coercionFailure()
    objValUnwrap u'

  -- do a univariate to transform u to a UP(x,P S) and then coerce again
  var := position1(x,vl)
  UPP := ['UnivariatePolynomial,x,source]
  univariate := getFunctionFromDomain('univariate,
    source,[source,['OrderedVariableList,vl]])
  upU := SPADCALL(u,var,univariate)  -- we may assume this has type UPP
  (u' := coerceInt(objNewWrap(upU,UPP),target)) or coercionFailure()
  objValUnwrap u'

--% OrderedVariableList

OV2OV(u,source is [.,svl], target is [.,tvl]) ==
  svl = intersection(svl,tvl) =>
    u = '_$fromCoerceable_$ => true
    position1(svl.(u-1),tvl)
  u = '_$fromCoerceable_$ => nil
  coercionFailure()

OV2P(u,source is [.,svl], target is [.,T]) ==
  u = '_$fromCoerceable_$ => true
  v := svl.(unwrap(u)-1)
  [1,v,[1,0,:domainOne(T)]]

OV2poly(u,source is [.,svl], target is [p,vl,T]) ==
  u = '_$fromCoerceable_$ =>
    p = 'UnivariatePolynomial => (# svl = 1) and (p = svl.0)
    and/[member(v,vl) for v in svl]
  v := svl.(unwrap(u)-1)
  val' := [1,:domainOne(T)]
  p = 'UnivariatePolynomial =>
    v ~= vl => coercionFailure()
    [[1,:domainOne(T)]]
  null member(v,vl) => coercionFailure()
  val' := [[1,:domainOne(T)]]
  source' := ['UnivariatePolynomial,v,T]
  (u' := coerceInt(objNewWrap(val',source'),target)) or
    coercionFailure()
  objValUnwrap(u')

OV2SE(u,source is [.,svl], target) ==
  u = '_$fromCoerceable_$ => true
  svl.(unwrap(u)-1)

OV2Sy(u,source is [.,svl], target) ==
  u = '_$fromCoerceable_$ => true
  svl.(unwrap(u)-1)

--% Polynomial

varsInPoly(u) ==
  u is [ =1, v, :termlist] =>
    [v,:varsInPoly(c) for [e,:c] in termlist]
  nil

P2FR(u,S is [.,R],[.,T]) ==
  u = '_$fromCoerceable_$ =>
    S ~= T => nil
    R in '((Integer) (Fraction (Integer))) => true
    nil
  S ~= T => coercionFailure()
  package :=
    R = $Integer =>
      ['MultivariateFactorize,$Symbol,['IndexedExponents, $Symbol],R,S]
    R is ['Fraction, D] =>
      package := ['MRationalFactorize,['IndexedExponents, $Symbol],$Symbol,
                 D, S]
    coercionFailure()
  factor := getFunctionFromDomain('factor,package,[S])
  SPADCALL(u,factor)

P2Dmp(u, source is [., S], target is [., y, T]) ==
  u = '_$fromCoerceable_$ =>
    -- might be able to say yes
    canCoerce(source,T)
  u is [ =0,:c] =>       -- polynomial is a constant
    (u' := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    objValUnwrap(u')
  univariate := getFunctionFromDomain('univariate,
    source,[source,$Symbol])
  plus := getFunctionFromDomain("+",target,[target,target])
  monom := getFunctionFromDomain('monomial,target,
    [target,['OrderedVariableList,y],$NonNegativeInteger])
  P2DmpAux(u,source,S,target,copy y,y,T,univariate,plus,monom)

P2Expr(u, source is [.,S], target is [., T]) ==
  u = '_$fromCoerceable_$ =>
    canCoerce(S, T)
  S = T => coercionFailure()
  newS := ['Polynomial, T]
  val := coerceInt(objNewWrap(u, source), newS)
  null val => coercionFailure()
  val := coerceInt(val, target)
  null val => coercionFailure()
  objValUnwrap val

P2DmpAux(u,source,S,target,varlist,vars,T,univariate,plus,monom) ==
  u is [ =0,:c] =>       -- polynomial is a constant
    (u' := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    objValUnwrap(u')

  -- if no variables left, try to go to underdomain of target (T)
  null vars =>
    (u' := coerceInt(objNewWrap(u,source),T)) or coercionFailure()
    -- if successful, embed
    (u' := coerceByFunction(u',target)) or coercionFailure()
    objValUnwrap(u')

  -- there are variables, so get them out of u
  [x,:vars] := vars
  sup := SPADCALL(u,x,univariate)  -- this is a SUP P S
  null sup =>           -- zero? unlikely.
    domainZero(target)
  -- degree 0 polynomial? (variable did not occur)
  null(rest(sup)) and first(sup) is [ =0,:c] =>
    -- call again, but with one less var
    P2DmpAux(c,source,S,target,varlist,vars,T,univariate,plus,monom)
  var := position1(x,varlist)
  u' := domainZero(target)
  for [e,:c] in sup repeat
    u'' := SPADCALL(
      P2DmpAux(c,source,S,target,varlist,vars,T,univariate,plus,monom),
        var,e,monom)
    u' := SPADCALL(u',u'',plus)
  u'

P2Mp(u, source is [., S], target is [., y, T]) ==
  u = '_$fromCoerceable_$ =>
    -- might be able to say yes
    canCoerce(source,T)
  univariate := getFunctionFromDomain('univariate,
    source,[source,$Symbol])
  P2MpAux(u,source,S,target,copy y,y,T,univariate)

P2MpAux(u,source,S,target,varlist,vars,T,univariate) ==
  u is [ =0,:c] =>       -- polynomial is a constant
    (u' := coerceInt(objNewWrap(c,S),target)) or
      coercionFailure()
    objValUnwrap(u')

  -- if no variables left, try to go to underdomain of target (T)
  null vars =>
    (u' := coerceInt(objNewWrap(u,source),T)) or
      coercionFailure()
    -- if successful, embed
    [ 0,:objValUnwrap(u')]

  -- there are variables, so get them out of u
  [x,:vars] := vars
  sup := SPADCALL(u,x,univariate)  -- this is a SUP P S
  null sup =>           -- zero? unlikely.
    domainZero(target)
  -- degree 0 polynomial? (variable did not occur)
  null(rest(sup)) and first(sup) is [ =0,:c] =>
    -- call again, but with one less var
    P2MpAux(c,source,S,target,varlist,vars,T,univariate)
  terms := [[e,:P2MpAux(c,source,S,target,varlist,vars,T,univariate)] for
    [e,:c] in sup]
  [1, position1(x,varlist), :terms]

varIsOnlyVarInPoly(u, var) ==
  u is [ =1, v, :termlist] =>
    v ~= var => nil
    and/[varIsOnlyVarInPoly(c,var) for [e,:c] in termlist]
  true

P2Up(u,source is [.,S],target is [.,x,T]) ==
  u = '_$fromCoerceable_$ => canCoerce(source,T)
  u is [ =0,:c] =>
    (u' := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    objValUnwrap(u')

  -- see if the target var is the polynomial vars
  varsFun := getFunctionFromDomain('variables,source,[source])
  vars := SPADCALL(u,varsFun)
  not member(x,vars) =>
    (u' := coerceInt(objNewWrap(u,source),T)) or coercionFailure()
    [[0,:objValUnwrap(u')]]

  #vars = 1 and S = T =>
      univariate := getFunctionFromDomain('univariate,
          source,[source])
      SPADCALL(u, univariate)
  -- do a univariate to transform u to a UP(x,P S) and then coerce again
  UPP := ['SparseUnivariatePolynomial, source]
  univariate := getFunctionFromDomain('univariate,
    source,[source,$Symbol])
  upU := SPADCALL(u,x,univariate)  -- we may assume this has type UPP
  SUP2Up_aux(upU, UPP, target)
  -- (u' := coerceInt(objNewWrap(upU,UPP),target)) or coercionFailure()
  -- objValUnwrap(u')

--% Fraction

Qf2PF(u,source is [.,D],target) ==
  u = '_$fromCoerceable_$ => canCoerce(D,target)
  [num,:den] := u
  num':= coerceInt(objNewWrap(num,D),target) or
    coercionFailure()
  num' := objValUnwrap num'
  den':= coerceInt(objNewWrap(den,D),target) or
    coercionFailure()
  den' := objValUnwrap den'
  equalZero(den', target) => throwKeyedMsg("S2IA0001",NIL)
  SPADCALL(num',den', getFunctionFromDomain("/",target,[target,target]))

Qf2domain(u,source is [.,D],target) ==
  -- tests whether it is an element of the underlying domain
  useUnder := (ut := underDomainOf target) and canCoerce(source,ut)
  u = '_$fromCoerceable_$ => useUnder
  not (containsPolynomial(D) and containsPolynomial(target)) and
    useUnder => coercionFailure()    -- let other mechanism handle it
  [num, :den] := u
  (num' := coerceInt(objNewWrap(num,D),target)) or coercionFailure()
  num' := objValUnwrap(num')
  equalOne(den,D) => num'
  (target is [.,[=$QuotientField,T]]) or
    (target is [.,.,[=$QuotientField,T]]) =>
      (den' := coerceInt(objNewWrap(den,D),T)) or coercionFailure()
      den' := [domainOne(T),:objValUnwrap(den')]
      timesfunc:= getFunctionFromDomain('_*,target,
        [[$QuotientField,T],target])
      SPADCALL(den',num',timesfunc)
  coercionFailure()

Qf2EF(u,[.,S],target) ==
  u = '_$fromCoerceable_$ => canCoerce(S,target)
  [num,:den] := u
  (num' := coerceInt(objNewWrap(num,S),target)) or
    coercionFailure()
  (den' := coerceInt(objNewWrap(den,S),target)) or
    coercionFailure()
  divfun := getFunctionFromDomain("/",target,[target,target])
  SPADCALL(objValUnwrap(num'),objValUnwrap(den'),divfun)

Qf2Qf(u0,[.,S],target is [.,T]) ==
  u0 = '_$fromCoerceable_$ =>
    S = ['Polynomial, [$QuotientField, $Integer]] and
      T = '(Polynomial (Integer)) => true
    canCoerce(S,T)
  [a,:b] := u0
  S = ['Polynomial, [$QuotientField, $Integer]] and
    T = '(Polynomial (Integer)) =>
      (a' := coerceInt(objNewWrap(a,S),target)) =>
        (b' := coerceInt(objNewWrap(b,S),target)) =>
          divfunc:= getFunctionFromDomain('_/,target,[target,target])
          SPADCALL(objValUnwrap(a'),objValUnwrap(b'),divfunc)
        coercionFailure()
      coercionFailure()
  (a' := coerceInt(objNewWrap(a,S),T)) =>
    (b' := coerceInt(objNewWrap(b,S),T)) =>
      [objValUnwrap(a'),:objValUnwrap(b')]
    coercionFailure()
  coercionFailure()

-- partOf(x,i) ==
--   VECP x => x.i
--   i=0 => first x
--   i=1 => rest x
--   systemError '"partOf"

--% RectangularMatrix

Rm2L(x,[.,.,.,R],target) == M2L(x,['Matrix,R],target)

Rm2M(x,[.,.,.,R],target is [.,S]) == M2M(x,[nil,R],target)

Rm2Sm(x,[.,n,m,S],[.,p,R]) ==
  x = '_$fromCoerceable_$ => n=m and m=p and canCoerce(S,R)
  n=m and m=p =>
    M2M(x,[nil,S],[nil,R])
  coercionFailure()

Rm2V(x,[.,.,.,R],target) == M2V(x,['Matrix,R],target)

--% Script

Scr2Scr(u, source is [.,S], target is [.,T]) ==
  u = '_$fromCoerceable_$ => canCoerce(S,T)
  null (v := coerceInt(objNewWrap(rest u, S), T)) =>
    coercionFailure()
  [first u, :objValUnwrap(v)]

--% SparseUnivariatePolynomialnimial

SUP2Up_aux(u,source is [.,S],target is [.,x,T]) ==
    -- must be careful in case any of the coeffs come back 0
    u' := NIL
    zero := getConstantFromDomain('(Zero),T)
    for [e,:c] in u repeat
        c' := objValUnwrap (coerceInt(objNewWrap(c,S),T) or
            coercionFailure())
        c' = zero => 'iterate
        u' := [[e,:c'],:u']
    nreverse u'


SUP2Up(u,source is [.,S],target is [.,x,T]) ==
  u = '_$fromCoerceable_$ => canCoerce(source,T) or canCoerce(S,T)
  null u => u
  S = T => u
  -- try to go underneath first
  null (u' := coerceInt(objNewWrap(u,source),T)) =>
      SUP2Up_aux(u, source, target)
  [[0,:objValUnwrap u']]

--% SquareMatrix

Sm2L(x,[.,.,R],target) == M2L(x,['Matrix,R],target)

Sm2M(x,[.,n,R],target is [.,S]) == M2M(x,[nil,R],target)

Sm2PolyType(u,source is [sm,n,S], target is [pol,vl,T]) ==
  -- only really handles cases like:
  --      SM[2] P I -> P[x,y] SM[2] P I
  -- works for UP, MP, DMP and NDMP
  u = '_$fromCoerceable_$ => canCoerce(source,T)
  -- first want to check case S is Polynomial
  S is ['Polynomial,S'] =>
    -- check to see if variable occurs in any of the terms
    if ATOM vl
      then vl' := [vl]
      else vl' := vl
    novars := true
    for i in 0..(n-1) while novars repeat
      for j in 0..(n-1) while novars repeat
        varsUsed := varsInPoly(AREF(u, i, j))
        or/[member(x,varsUsed) for x in vl'] => novars := nil
    novars => coercionFailure()
    source' := [sm,n,[pol,vl,S]]
    null (u' := coerceInt(objNewWrap(u,source),source')) =>
      coercionFailure()
    null (u' := coerceInt(u',target)) =>
      coercionFailure()
    objValUnwrap(u')
  -- let other cases be handled by standard machinery
  coercionFailure()

Sm2Rm(x,[.,n,R],[.,p,q,S]) ==
  x = '_$fromCoerceable_$ => p=q and p=n and canCoerce(R,S)
  p=q and p=n =>
    M2M(x,[nil,R],[nil,S])
  coercionFailure()

Sm2V(x,[.,.,R],target) == M2V(x,['Matrix,R],target)

--% Symbol

Sy2OV(u,source,target is [.,vl]) ==
  u = '_$fromCoerceable_$ => nil
  res := position1(u,vl)
  res = 0 => coercionFailure()
  res

Sy2Dmp(u,source,target is [dmp,vl,S]) ==
  u = '_$fromCoerceable_$ => canCoerce(source,S)
  len:= #vl
  -1~=(n:= position(u,vl)) =>
    u:= wrap LIST [LIST2VEC [(n=i => 1; 0) for i in 0..len-1],:1]
    objValUnwrap(coerceInt(objNew(u,[dmp,vl,$Integer]),target))
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [[Zeros len,:objValUnwrap u]]

Sy2Mp(u,source,target is [mp,vl,S]) ==
  u = '_$fromCoerceable_$ => canCoerce(source,S)
  (n:= position1(u,vl)) ~= 0 =>
    [1,n,[1,0,:domainOne(S)]]
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [0,:objValUnwrap(u)]

Sy2NDmp(u,source,target is [ndmp,vl,S]) ==
  u = '_$fromCoerceable_$ => canCoerce(source,S)
  len:= #vl
  -1~=(n:= position(u,vl)) =>
    u:= wrap LIST [LIST2VEC [(n=i => 1; 0) for i in 0..len-1],:1]
    objValUnwrap(coerceInt(objNew(u,[ndmp,vl,$Integer]),target))
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [[Zeros len,:objValUnwrap(u)]]

Sy2P(u,source,target is [poly,S]) ==
  u = '_$fromCoerceable_$ => true
  -- first try to get it into an underdomain
  if (S ~= $Integer) then
    u' := coerceInt(objNewWrap(u,source),S)
    if u' then return [0,:objValUnwrap(u')]
  -- if that failed, return it as a polynomial variable
  [1,u,[1,0,:domainOne(S)]]

Sy2Up(u,source,target is [up,x,S]) ==
  u = '_$fromCoerceable_$ => canCoerce(source,S)
  u=x => [[1,:domainOne(S)]]
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [[0,:objValUnwrap u]]

Sy2Var(u,source,target is [.,x]) ==
  u = '_$fromCoerceable_$ => NIL
  u=x => u
  coercionFailure()

--% Univariate Polynomial

Up2Dmp(u,source is ['UnivariatePolynomial,var,S],
 target is ['DistributedMultivariatePolynomial,vl,T]) ==
  -- var must be a member of vl, or u is a constant
  u = '_$fromCoerceable_$ => member(var,vl) and canCoerce(S,target)
  null u => domainZero(target)
  u is [[e,:c]] and e=0 =>
    z:= coerceInt(objNewWrap(c,S),target) => objValUnwrap(z)
    coercionFailure()
  member(var,vl) =>
    x:= domainZero(target)
    one:= domainOne(T)
    plusfunc:= getFunctionFromDomain('_+,target,[target,target])
    multfunc:= getFunctionFromDomain('_*,target,[target,target])
    n:= #vl ; p:= POSN1(var,vl)
    l1:= not (p=0) and [0 for m in 1..p]
    l2:= not (p=n-1) and [0 for m in p..n-2]
    for [e,:c] in u until not z repeat
      z:= coerceInt(objNewWrap(c,S),target) =>
        y:= SPADCALL(objValUnwrap(z),
          [[LIST2VEC [:l1,e,:l2],:one]],multfunc)
        x:= SPADCALL(x,y,plusfunc)
    z => x
    coercionFailure()
  coercionFailure()

Up2Expr(u,source is [up,var,S], target is [Expr,T]) ==
    u = '_$fromCoerceable_$ => canCoerce(S, target)

    null u => domainZero(target)

    u is [[e,:c]] and e=0 =>
        (z := coerceInt(objNewWrap(c, S), target)) => objValUnwrap(z)
        coercionFailure()

    sym := objValUnwrap coerceInt(objNewWrap(var, $Symbol), target)

    plus := getFunctionFromDomain("+",  target, [target, target])
    mult := getFunctionFromDomain("*",  target, [target, target])
    expn := getFunctionFromDomain("^", target, [target, $Integer])

    -- coerce via Horner's rule

    [e1, :c1] := first u
    if not (S = target) then
        not (c1 := coerceInt(objNewWrap(c1, S), target)) => coercionFailure()
        c1 := objValUnwrap(c1)

    for [e2, :c2] in rest u repeat
        coef :=
            e1 - e2 = 1 => sym
            SPADCALL(sym, e1-e2, expn)
        if not (S = target) then
            not (c2 := coerceInt(objNewWrap(c2, S), target)) =>
                coercionFailure()
            c2 := objValUnwrap(c2)
        coef := SPADCALL(SPADCALL(c1, coef, mult), c2, plus)
        e1 := e2
        c1 := coef

    e1 = 0 => c1
    e1 = 1 => SPADCALL(sym, c1, mult)
    SPADCALL(SPADCALL(sym, e1, expn), c1, mult)

Up2FR(u,S is [.,x,R],target is [.,T]) ==
  u = '_$fromCoerceable_$ =>
    S ~= T => nil
    R in '((Integer) (Fraction (Integer))) => true
    nil
  S ~= T => coercionFailure()
  package :=
    R = $Integer => ['UnivariateFactorize,S]
    R = $RationalNumber => package := ['RationalFactorize,S]
    coercionFailure()
  factor := getFunctionFromDomain('factor,package,[S])
  SPADCALL(u,factor)

Up2Mp(u,source is [.,x,S], target is [.,vl,T]) ==
  u = '_$fromCoerceable_$ =>
    member(x,vl) => canCoerce(S,T)
    canCoerce(source,T)

  null u => domainZero(target)

  null(rest(u)) and (first(u) is [e,:c]) and e=0 =>
    x:= coerceInt(objNewWrap(c,S),target) => objValUnwrap(x)
    coercionFailure()

  null member(x,vl) =>
    (x := coerceInt(objNewWrap(u,source),T)) or coercionFailure()
    [0,:objValUnwrap(x)]

  plus  := getFunctionFromDomain('_+,target,[target,target])
  monom := getFunctionFromDomain('monomial,target,
    [target,['OrderedVariableList,vl],$NonNegativeInteger])
  sum := domainZero(target)
  pos := position1(x,vl)

  for [e,:c] in u repeat
    (p := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    mon := SPADCALL(objValUnwrap(p),pos,e,monom)
    sum := SPADCALL(sum,mon,plus)
  sum

Up2P(u,source is [.,var,S],target is [.,T]) ==
  u = '_$fromCoerceable_$ => canCoerce(S,target)
  null u => domainZero(target)
  u is [[e,:c]] and e=0 =>
    x:= coerceInt(objNewWrap(c,S),target) => objValUnwrap(x)
    coercionFailure()
  S = T =>
      res := []
      for [e,:c] in u repeat
          res := cons([e, 0, :c], res)
      [1, var, :NREVERSE res]
  pol:= domainZero(target)
  one:= domainOne(T)
  plusfunc := getFunctionFromDomain("+",target,[target,target])
  multfunc := getFunctionFromDomain("*",target,[target,target])
  for [e,:c] in u until not x repeat
    x:= coerceInt(objNewWrap(c,S),target) =>
      term:= SPADCALL([1,var,[e,0,:one]],objValUnwrap(x),multfunc)
      pol:= SPADCALL(pol,term,plusfunc)
    coercionFailure()
  x => pol
  coercionFailure()

Up2SUP(u,source is [.,x,S],target is [.,T]) ==
  u = '_$fromCoerceable_$ => canCoerce(source,T) or canCoerce(S,T)
  null u => u
  S = T => u
  -- try to go underneath first
  null (u' := coerceInt(objNewWrap(u,source),T)) =>
    u' := NIL
    zero := getConstantFromDomain('(Zero),T)
    for [e,:c] in u repeat
      c' := objValUnwrap (coerceInt(objNewWrap(c,S),T) or
        coercionFailure())
      c' = zero => 'iterate
      u' := [[e,:c'],:u']
    nreverse u'
  [[0,:objValUnwrap u']]

Up2Up(u,source is [.,v1,S], target is [.,v2,T]) ==
  -- if v1 = v2 then this is handled by coerceIntByMap
  -- this only handles case where poly is a constant
  u = '_$fromCoerceable_$ =>
    v1=v2 => canCoerce(S,T)
    canCoerce(source,T)
  null u => u
  u is [[e,:c]] and e=0 =>
    x:= coerceInt(objNewWrap(c,S),target) => objValUnwrap(x)
    coercionFailure()
  coercionFailure()

insertAlist(a,b,l) ==
  null l => [[a,:b]]
  a = l.0.0 => (rplac(CDAR l, b); l)
  _?ORDER(l.0.0,a) => [[a,:b],:l]
  (fn(a,b,l);l) where fn(a,b,l) ==
    null rest l => rplac(rest l, [[a, :b]])
    a = l.1.0 => rplac(rest(l.1), b)
    _?ORDER(l.1.0, a) => rplac(rest l, [[a, :b], :rest l])
    fn(a, b, rest l)

--% Union

Un2E(x,source,target) ==
  ['Union,:branches] := source
  x = '_$fromCoerceable_$ =>
    and/[canCoerce(t, target) for t in branches | not STRINGP t]
  coerceUn2E(x,source)

--% Variable

Var2OV(u,source,target is [.,vl]) ==
  sym := CADR source
  u = '_$fromCoerceable_$ => member(sym,vl)
  member(sym,vl) => position1(sym,vl)
  coercionFailure()

Var2Dmp(u,source,target is [dmp,vl,S]) ==
  sym := CADR source
  u = '_$fromCoerceable_$ => member(sym,vl) or canCoerce(source,S)

  len := #vl
  -1 ~= (n:= position(sym,vl)) =>
    LIST [LIST2VEC [(n=i => 1; 0) for i in 0..len-1],
      :getConstantFromDomain('(One),S)]
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [[Zeros len,:objValUnwrap u]]

Var2Gdmp(u,source,target is [dmp,vl,S]) ==
  sym := CADR source
  u = '_$fromCoerceable_$ => member(sym,vl) or canCoerce(source,S)

  len := #vl
  -1 ~= (n:= position(sym,vl)) =>
    LIST [LIST2VEC [(n=i => 1; 0) for i in 0..len-1],
      :getConstantFromDomain('(One),S)]
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [[Zeros len,:objValUnwrap u]]

Var2Mp(u,source,target is [mp,vl,S]) ==
  sym := CADR source
  u = '_$fromCoerceable_$ => member(sym,vl) or canCoerce(source,S)
  (n:= position1(u,vl)) ~= 0 =>
    [1,n,[1,0,:getConstantFromDomain('(One),S)]]
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [0,:objValUnwrap u]

Var2NDmp(u,source,target is [ndmp,vl,S]) ==
  sym := CADR source
  u = '_$fromCoerceable_$ => member(sym,vl) or canCoerce(source,S)

  len:= #vl
  -1~=(n:= position(u,vl)) =>
    LIST [LIST2VEC [(n=i => 1; 0) for i in 0..len-1],
      :getConstantFromDomain('(One),S)]
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [[Zeros len,:objValUnwrap(u)]]

Var2P(u,source,target is [poly,S]) ==
  sym := CADR source
  u = '_$fromCoerceable_$ => true

  -- first try to get it into an underdomain
  if (S ~= $Integer) then
    u' := coerceInt(objNewWrap(u,source),S)
    if u' then return [0,:objValUnwrap(u')]
  -- if that failed, return it as a polynomial variable
  [1,sym,[1,0,:getConstantFromDomain('(One),S)]]

Var2QF(u,source,target is [qf,S]) ==
  u = '_$fromCoerceable_$ => canCoerce(source,S)

  S = $Integer => coercionFailure()
  sym := CADR source
  (u' := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [objValUnwrap u',:getConstantFromDomain('(One),S)]

Var2Up(u,source,target is [up,x,S]) ==
  sym := CADR source
  u = '_$fromCoerceable_$ => (sym = x) or canCoerce(source,S)

  x=sym => [[1,:getConstantFromDomain('(One),S)]]
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [[0,:objValUnwrap u]]

Var2SUP(u,source,target is [sup,S]) ==
  sym := CADR source
  u = '_$fromCoerceable_$ => (sym = "?") or canCoerce(source,S)

  sym = "?" => [[1,:getConstantFromDomain('(One),S)]]
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  [[0,:objValUnwrap u]]

Var2UpS(u,source,target is [ups,x,S]) ==
  sym := CADR source
  u = '_$fromCoerceable_$ => (sym = x) or canCoerce(source,S)

  mid := ['UnivariatePolynomial,x,S]
  x = sym =>
    u := Var2Up(u,source,mid)
    (u := coerceInt(objNewWrap(u,mid),target)) or coercionFailure()
    objValUnwrap u
  (u := coerceInt(objNewWrap(u,source),S)) or coercionFailure()
  (u := coerceInt(u,target)) or coercionFailure()
  objValUnwrap u

Var2OtherPS(u,source,target is [.,x,S]) ==
  sym := CADR source
  mid := ['UnivariatePowerSeries,x,S]
  u = '_$fromCoerceable_$ =>
    (sym = x) or (canCoerce(source,mid) and canCoerce(mid,target))
  u := Var2UpS(u,source,mid)
  (u := coerceInt(objNewWrap(u,mid),target)) or coercionFailure()
  objValUnwrap u

--% Vector

V2M(u,[.,D],[.,R]) ==
  u = '_$fromCoerceable_$ => nil
    -- D is ['Vector,:.] => nil  -- don't have data
    -- canCoerce(D,R)
  -- first see if we are coercing a vector of vectors
  D is ['Vector,E] and
    isRectangularVector(u, n := MAXINDEX u, m := MAXINDEX u.0) =>
      res := MAKE_MATRIX(n + 1, m + 1)
      for i in 0..n repeat
          x := u.i
          for j in 0..m repeat
              y' := objValUnwrap(coerceInt(objNewWrap(x.j, E), R))
              QSETAREF2O(res, i, j, y', 0, 0)
      res
  -- if not, try making it into a 1 by n matrix
  coercionFailure()
--LIST2VEC [LIST2VEC [objValUnwrap(coerceInt(objNewWrap(u.i,D),R))
--  for i in 0..MAXINDEX(u)]]

V2Rm(u, source is [., D], [., n, m, R]) ==
  u = '_$fromCoerceable_$ => nil
  D is [.,E,:.] and isRectangularVector(u,n-1,m-1) =>
      V2M(u, source, ["Matrix", R])
  coercionFailure()

V2Sm(u, source is [., D], [., n, R]) ==
  u = '_$fromCoerceable_$ => nil
  D is [.,E,:.] and isRectangularVector(u,n-1,n-1) =>
      V2M(u, source, ["Matrix", R])
  coercionFailure()

isRectangularVector(x,p,q) ==
  MAXINDEX x = p =>
    and/[q=MAXINDEX x.i for i in 0..p]

-- Polynomial and Expression to Univariate series types

P2Uts(u, source, target) ==
  P2Us(u,source, target, 'taylor)

P2Uls(u, source, target) ==
  P2Us(u,source, target, 'laurent)

P2Upxs(u, source, target) ==
  P2Us(u,source, target, 'puiseux)

P2Us(u, source is [.,S], target is [.,T,var,cen], type) ==
  u = '_$fromCoerceable_$ =>
    -- might be able to say yes
    canCoerce(S,T)
  T isnt ['Expression, :.] => coercionFailure()
  if S ~= '(Float) then S := $Integer
  obj := objNewWrap(u, source)
  E := ['Expression, S]
  newU := coerceInt(obj, E)
  null newU => coercionFailure()
  EQtype := ['Equation, E]
  eqfun := getFunctionFromDomain('_=, EQtype, [E,E])
  varE := coerceInt(objNewWrap(var, '(Symbol)), E)
  null varE => coercionFailure()
  cenE := coerceInt(objNewWrap(cen, T), E)
  null cenE => coercionFailure()
  eq := SPADCALL(objValUnwrap(varE), objValUnwrap(cenE), eqfun)
  package := ['ExpressionToUnivariatePowerSeries, S, E]
  func := getFunctionFromDomain(type, package, [E, EQtype])
  newObj := SPADCALL(objValUnwrap(newU), eq, func)
  newType := first newObj
  newVal  := rest newObj
  newType = target => newVal
  finalObj := coerceInt(objNewWrap(newVal, newType), target)
  null finalObj => coercionFailure()
  objValUnwrap finalObj


--% General Coercion Commutation Functions

-- general commutation functions are called with 5 values
--     u           object of type source
--     source      type of u
--     S           underdomain of source
--     target      coercion target type
--     T           underdomain of T
-- Because of checking, can always assume S and T have underdomains.

--% Complex

commuteComplex(u,source,S,target,T) ==
  u = '_$fromCoerceable_$ =>
    canCoerce(S,target) and canCoerce(T,target)
  [real,:imag] := u
  (real := coerceInt(objNewWrap(real,S),target)) or coercionFailure()
  (imag := coerceInt(objNewWrap(imag,S),target)) or coercionFailure()
  T' := underDomainOf T
  i := [domainZero(T'),
       :domainOne(T')]
  (i := coerceInt(objNewWrap(i,T),target)) or coercionFailure()
  f := getFunctionFromDomain("*",target,[target,target])
  i := SPADCALL(objValUnwrap i, objValUnwrap imag, f)
  f := getFunctionFromDomain("+",target,[target,target])
  SPADCALL(objValUnwrap real,i,f)

--% Quaternion

commuteQuaternion(u,source,S,target,T) ==
  u = '_$fromCoerceable_$ =>
    canCoerce(S,target) and canCoerce(T,target)
  c := [objValUnwrap(coerceInt(objNewWrap(x,S),target)
    or coercionFailure()) for x in VEC2LIST u]
  q := '(Quaternion (Integer))
  e := [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]
  e := [(coerceInt(objNewWrap(LIST2VEC x,q),T)
    or coercionFailure()) for x in e]
  e :=[objValUnwrap(coerceInt(x,target) or coercionFailure()) for x in e]
  u' := domainZero(target)
  mult := getFunctionFromDomain("*",target,[target,target])
  plus := getFunctionFromDomain("+",target,[target,target])
  for x in c for y in e repeat
    u' := SPADCALL(u',SPADCALL(x,y,mult),plus)
  u'

--% Fraction

commuteFraction(u,source,S,target,T) ==
  u = '_$fromCoerceable_$ =>
    ofCategory(target,'(Field)) => canCoerce(S,target)
    canCoerce(S,T) and canCoerce(T,target)
  [n,:d] := u
  ofCategory(target,'(Field)) =>
    -- see if denominator can go over to target
    (d' := coerceInt(objNewWrap(d,S),target)) or coercionFailure()
    -- if so, try to invert it
    inv := getFunctionFromDomain('inv,target,[target])
    d' := SPADCALL(objValUnwrap d',inv)
    -- now coerce to target
    (n' := coerceInt(objNewWrap(n,S),target)) or coercionFailure()
    multfunc := getFunctionFromDomain("*",target,[target,target])
    SPADCALL(d',objValUnwrap n',multfunc)
  -- see if denominator can go over to QF part of target
  (d' := coerceInt(objNewWrap(d,S),T)) or coercionFailure()
  -- if so, try to invert it
  inv := getFunctionFromDomain('inv,T,[T])
  d' := SPADCALL(objValUnwrap d',inv)
  -- now coerce to target
  (d' := coerceInt(objNewWrap(d',T),target)) or coercionFailure()
  (n' := coerceInt(objNewWrap(n,S),target)) or coercionFailure()
  multfunc := getFunctionFromDomain("*",target,[target,target])
  SPADCALL(objValUnwrap d',objValUnwrap n',multfunc)

--% SquareMatrix

commuteSquareMatrix(u,source,S,target,T) ==
  u = '_$fromCoerceable_$ =>
    canCoerce(S,target) and canCoerce(T,target)
  -- commuting matrices of matrices should be a no-op
  S is ['SquareMatrix,:.] =>
     source=target => u
     coercionFailure()
  u' := domainZero(target)
  plusfunc := getFunctionFromDomain("+",target,[target,target])
  multfunc := getFunctionFromDomain("*",target,[target,target])
  zero := domainZero(S)
  [sm,n,:.] := source
  S' := [sm,n,$Integer]
  for i in 0..(n-1) repeat
    for j in 0..(n-1) repeat
      (e := QAREF2O(u, i, j, 0, 0)) = zero => 'iterate
      (e' := coerceInt(objNewWrap(e,S),target)) or coercionFailure()
      (Eij := coerceInt(objNewWrap(makeEijSquareMatrix(i,j,n),S'),T)) or
        coercionFailure()
      (Eij := coerceInt(Eij,target)) or coercionFailure()
      e' := SPADCALL(objValUnwrap(e'),objValUnwrap(Eij),multfunc)
      u' := SPADCALL(e',u',plusfunc)
  u'

makeEijSquareMatrix(i, j, dim) ==
    res := MAKE_MATRIX1(dim, dim, 0)
    QSETAREF2O(res, i, j, 1, 0, 0)
    res

--% Univariate Polynomial and Sparse Univariate Polynomial

commuteUnivariatePolynomial(u,source,S,target,T) ==
  commuteSparseUnivariatePolynomial(u,source,S,target,T)

commuteSparseUnivariatePolynomial(u,source,S,target,T) ==
  u = '_$fromCoerceable_$ =>
    canCoerce(S,target) and canCoerce(T,target)

  u' := domainZero(target)
  null u => u'

  T'  := underDomainOf T
  one := domainOne(T')
  monom := getFunctionFromDomain('monomial,T,[T',$NonNegativeInteger])
  plus  := getFunctionFromDomain("+",target,[target,target])
  times := getFunctionFromDomain("*",target,[target,target])

  for [e,:c] in u repeat
    (c := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    m := SPADCALL(one,e,monom)
    (m := coerceInt(objNewWrap(m,T),target)) or coercionFailure()
    c := objValUnwrap c
    m := objValUnwrap m
    u' := SPADCALL(u',SPADCALL(c,m,times),plus)
  u'

--% Multivariate Polynomials

commutePolynomial(u,source,S,target,T) ==
  commuteMPolyCat(u,source,S,target,T)

commuteMultivariatePolynomial(u,source,S,target,T) ==
  commuteMPolyCat(u,source,S,target,T)

commuteDistributedMultivariatePolynomial(u,source,S,target,T) ==
  commuteMPolyCat(u,source,S,target,T)

commuteNewDistributedMultivariatePolynomial(u,source,S,target,T) ==
  commuteMPolyCat(u,source,S,target,T)

commuteMPolyCat(u,source,S,target,T) ==
  u = '_$fromCoerceable_$ => canCoerce(S,target)
  -- check constant case
  isconstfun := getFunctionFromDomain("ground?",source,[source])
  SPADCALL(u,isconstfun) =>
    constfun := getFunctionFromDomain("ground",source,[source])
    c := SPADCALL(u,constfun)
    (u' := coerceInt(objNewWrap(c,S),target)) or coercionFailure()
    objValUnwrap(u')

  lmfun := getFunctionFromDomain('leadingMonomial,source,[source])
  lm := SPADCALL(u,lmfun)    -- has type source, is leading monom

  lcfun := getFunctionFromDomain('leadingCoefficient,source,[source])
  lc := SPADCALL(lm,lcfun)    -- has type S, is leading coef
  (lc' := coerceInt(objNewWrap(lc,S),target)) or coercionFailure()

  pmfun := getFunctionFromDomain('primitiveMonomials,source,[source])
  lm := first SPADCALL(lm,pmfun) -- now we have removed the leading coef
  (lm' := coerceInt(objNewWrap(lm,source),T)) or coercionFailure()
  (lm' := coerceInt(lm',target)) or coercionFailure()

  rdfun := getFunctionFromDomain('reductum,source,[source])
  rd := SPADCALL(u,rdfun)    -- has type source, is reductum
  (rd' := coerceInt(objNewWrap(rd,source),target)) or coercionFailure()

  lc' := objValUnwrap lc'
  lm' := objValUnwrap lm'
  rd' := objValUnwrap rd'

  plusfun := getFunctionFromDomain("+",target,[target,target])
  multfun := getFunctionFromDomain("*",target,[target,target])
  SPADCALL(SPADCALL(lc',lm',multfun),rd',plusfun)

------------------------------------------------------------------------
-- Format for alist member is:  domain  coercionType function
--   here coercionType can be one of 'total, 'partial or 'indeterm
--   (indeterminant - cannot tell in a simple way).
--
-- In terms of canCoerceFrom, 'total implies true, 'partial implies
--   false (just cannot tell without actual data) and 'indeterm means
--   to call the function with the data = "$fromCoerceable$" for a
--   response of true or false.
------------------------------------------------------------------------

DEFPARAMETER($CoerceTable, '(                                          _
  (Complex . ( _
    (Expression                       indeterm   Complex2Expr) _
    (Factored                         indeterm   Complex2FR) _
    (Integer                          partial    Complex2underDomain) _
    (PrimeField                       partial    Complex2underDomain) _
    ))_
  (DirectProduct . (                                                  _
    (DirectProduct                        partial    DP2DP)           _
   ))                                                                 _
  (DistributedMultivariatePolynomial . (                              _
    (DistributedMultivariatePolynomial    indeterm   Dmp2Dmp) _
    (Expression                           indeterm   Dmp2Expr) _
    (Factored                             indeterm   Mp2FR) _
    (HomogeneousDistributedMultivariatePolynomial indeterm   Dmp2NDmp) _
    (MultivariatePolynomial               indeterm   Dmp2Mp) _
    (Polynomial                           indeterm   Dmp2P) _
    (UnivariatePolynomial                 indeterm   Dmp2Up) _
    ))_
  (Expression . (
    (Complex                                         partial    Expr2Complex) _
    (DistributedMultivariatePolynomial               indeterm   Expr2Dmp) _
    (HomogeneousDistributedMultivariatePolynomial    indeterm   Expr2Dmp) _
    (MultivariatePolynomial                          indeterm   Expr2Mp) _
    (UnivariateLaurentSeries                         indeterm   P2Uls) _
    (UnivariatePolynomial                            indeterm   Expr2Up) _
    (UnivariatePuiseuxSeries                         indeterm   P2Upxs) _
    (UnivariateTaylorSeries                          indeterm   P2Uts) _
    )) _

  (Kernel . ( _
    (Kernel                                          indeterm   Ker2Ker) _
    (Expression                                      indeterm   Ker2Expr) _
     )) _

  (Factored . ( _
    (Factored                             indeterm   Factored2Factored) _
    ))_
  (Fraction . ( _
    (DistributedMultivariatePolynomial    partial    Qf2domain) _
    (Expression                           indeterm   Qf2EF) _
    (Fraction                             indeterm   Qf2Qf) _
    (HomogeneousDistributedMultivariatePolynomial partial    Qf2domain) _
    (Integer                              partial    Qf2domain) _
    (MultivariatePolynomial               partial    Qf2domain) _
    (Polynomial                           partial    Qf2domain) _
    (PrimeField                           indeterm   Qf2PF) _
    (UnivariateLaurentSeries              indeterm   P2Uls) _
    (UnivariatePolynomial                 partial    Qf2domain) _
    (UnivariatePuiseuxSeries              indeterm   P2Upxs) _
    (UnivariateTaylorSeries               indeterm   P2Uts) _
    ))_
  (Int . ( _
    (Expression                           total      ncI2E) _
    (Integer                              total      ncI2I) _
  ))_
  (Baby . ( _
    (Expression                           total      ncI2E) _
    (Integer                              total      ncI2I) _
  ))_
  (Integer . ( _
    (Baby                                 total      I2ncI) _
    (EvenInteger                          partial    I2EI) _
    (Int                                  total      I2ncI) _
    (NonNegativeInteger                   partial    I2NNI) _
    (OddInteger                           partial    I2OI) _
    (PositiveInteger                      partial    I2PI) _
    ))_
  (List . ( _
    (DirectProduct                        indeterm   L2DP) _
    (Matrix                               partial    L2M) _
    (Record                               partial    L2Record) _
    (RectangularMatrix                    partial    L2Rm) _
    (Set                                  indeterm   L2Set) _
    (SquareMatrix                         partial    L2Sm) _
    (Stream                               indeterm   Agg2Agg) _
    (Tuple                                indeterm   L2Tuple) _
    (Vector                               indeterm   L2V) _
    ))_
  (Matrix . ( _
    (List                                 indeterm   M2L) _
    (RectangularMatrix                    partial    M2Rm) _
    (SquareMatrix                         partial    M2Sm) _
    (Vector                               indeterm   M2L) _
    ))_
  (MultivariatePolynomial . ( _
    (DistributedMultivariatePolynomial    indeterm   Mp2Dmp) _
    (Expression                           indeterm   Mp2Expr) _
    (Factored                             indeterm   Mp2FR) _
    (HomogeneousDistributedMultivariatePolynomial indeterm   domain2NDmp) _
    (MultivariatePolynomial               indeterm   Mp2Mp) _
    (Polynomial                           indeterm   Mp2P) _
    (UnivariatePolynomial                 indeterm   Mp2Up) _
    ))_
  (HomogeneousDirectProduct . ( _
    (HomogeneousDirectProduct             indeterm   DP2DP) _
   ))_
  (HomogeneousDistributedMultivariatePolynomial . ( _
    (Complex                              indeterm   NDmp2domain) _
    (DistributedMultivariatePolynomial    indeterm   NDmp2domain) _
    (Expression                           indeterm   Dmp2Expr) _
    (Factored                             indeterm   Mp2FR) _
    (Fraction                             indeterm   NDmp2domain) _
    (HomogeneousDistributedMultivariatePolynomial indeterm   NDmp2NDmp) _
    (MultivariatePolynomial               indeterm   NDmp2domain) _
    (Polynomial                           indeterm   NDmp2domain) _
    (Quaternion                           indeterm   NDmp2domain) _
    (UnivariatePolynomial                 indeterm   NDmp2domain) _
    ))_
  (OrderedVariableList . ( _
    (DistributedMultivariatePolynomial    indeterm   OV2poly) _
    (HomogeneousDistributedMultivariatePolynomial indeterm   OV2poly) _
    (MultivariatePolynomial               indeterm   OV2poly) _
    (OrderedVariableList                  indeterm   OV2OV) _
    (Polynomial                           total      OV2P) _
    (Symbol                               total      OV2Sy) _
    (UnivariatePolynomial                 indeterm   OV2poly) _
    ))_
  (Polynomial . ( _
    (DistributedMultivariatePolynomial    indeterm   P2Dmp) _
    (Expression                           indeterm   P2Expr) _
    (Factored                             indeterm   P2FR) _
    (HomogeneousDistributedMultivariatePolynomial partial    domain2NDmp) _
    (MultivariatePolynomial               indeterm   P2Mp) _
    (UnivariateLaurentSeries              indeterm   P2Uls) _
    (UnivariatePolynomial                 indeterm   P2Up) _
    (UnivariatePuiseuxSeries              indeterm   P2Upxs) _
    (UnivariateTaylorSeries               indeterm   P2Uts) _
    ))_
  (Set . ( _
    (List                                 indeterm   Set2L) _
    (Vector                               indeterm   Agg2L2Agg) _
    ))_
  (RectangularMatrix . ( _
    (List                                 indeterm   Rm2L) _
    (Matrix                               indeterm   Rm2M) _
    (SquareMatrix                         indeterm   Rm2Sm) _
    (Vector                               indeterm   Rm2V) _
    ))_
  (SparseUnivariatePolynomial . ( _
    (UnivariatePolynomial                       indeterm   SUP2Up) _
    ))_
  (SquareMatrix . (
    -- ones for polys needed for M[2] P I -> P[x,y] M[2] P I, say
    (DistributedMultivariatePolynomial            partial    Sm2PolyType) _
    (HomogeneousDistributedMultivariatePolynomial partial    Sm2PolyType) _
    (List                                         indeterm   Sm2L) _
    (Matrix                                       indeterm   Sm2M) _
    (MultivariatePolynomial                       partial    Sm2PolyType) _
    (RectangularMatrix                            indeterm   Sm2Rm) _
    (UnivariatePolynomial                         indeterm   Sm2PolyType) _
    (Vector                                       indeterm   Sm2V) _
    ) ) _
  (Symbol . ( _
    (DistributedMultivariatePolynomial            indeterm   Sy2Dmp) _
    (HomogeneousDistributedMultivariatePolynomial indeterm   Sy2NDmp) _
    (MultivariatePolynomial                       indeterm   Sy2Mp) _
    (OrderedVariableList                          partial    Sy2OV) _
    (Polynomial                                   total      Sy2P) _
    (UnivariatePolynomial                         indeterm   Sy2Up) _
    (Variable                                     indeterm   Sy2Var) _
    ) ) _
  (UnivariatePolynomial . ( _
    (DistributedMultivariatePolynomial            indeterm   Up2Dmp) _
    (Expression                                   indeterm   Up2Expr) _
    (Factored                                     indeterm   Up2FR) _
    (HomogeneousDistributedMultivariatePolynomial indeterm   domain2NDmp) _
    (MultivariatePolynomial                       indeterm   Up2Mp) _
    (Polynomial                                   indeterm   Up2P) _
    (SparseUnivariatePolynomial                   indeterm   Up2SUP) _
    (UnivariatePolynomial                         indeterm   Up2Up) _
    ) ) _
  (Variable . ( _
    (ContinuedFractionPowerSeries                 indeterm   Var2OtherPS) _
    (DistributedMultivariatePolynomial            indeterm   Var2Dmp) _
    (Fraction                                     indeterm   Var2QF) _
    (GeneralDistributedMultivariatePolynomial     indeterm   Var2Gdmp) _
    (HomogeneousDistributedMultivariatePolynomial indeterm   Var2NDmp) _
    (MultivariatePolynomial                       indeterm   Var2Mp) _
    (OrderedVariableList                          indeterm   Var2OV) _
    (Polynomial                                   total      Var2P) _
    (SparseUnivariatePolynomial                   indeterm   Var2SUP) _
    (Symbol                                       total      Identity) _
    (UnivariatePolynomial                         indeterm   Var2Up) _
    (UnivariatePowerSeries                        indeterm   Var2UpS) _
    ) ) _
  (Vector . ( _
    (DirectProduct                        indeterm   V2DP) _
    (List                                 indeterm   V2L) _
    (Matrix                               indeterm   V2M) _
    (RectangularMatrix                    indeterm   V2Rm) _
    (Set                                  indeterm   Agg2L2Agg) _
    (SquareMatrix                         indeterm   V2Sm) _
    (Stream                               indeterm   Agg2Agg) _
    ) ) _
  ) )

-- this list is too long for the parser, so it has to be split into parts
-- specifies the commute functions
-- commute stands for partial commute function
--DEFPARAMETER($CommuteTable, '(                                           _
--  (DistributedMultivariatePolynomial . (                                _
--    (DistributedMultivariatePolynomial    commute    commuteMultPol)    _
--    (Complex                              commute    commuteMultPol)    _
--    (MultivariatePolynomial               commute    commuteMultPol)    _
--    (NewDistributedMultivariatePolynomial commute    commuteMultPol)    _
--    (Polynomial                           commute    commuteMultPol)    _
--    (Quaternion                           commute    commuteMultPol)    _
--    (Fraction                             commute    commuteMultPol)    _
--    (SquareMatrix                         commute    commuteMultPol)    _
--    (UnivariatePolynomial                 commute    commuteMultPol)    _
--    ))                                                                  _
--  (Complex . (                                                         _
--    (DistributedMultivariatePolynomial    commute    commuteG2)         _
--    (MultivariatePolynomial               commute    commuteG2)         _
--    (NewDistributedMultivariatePolynomial commute    commuteG2)         _
--    (Polynomial                           commute    commuteG1)         _
--    (Fraction                             commute    commuteG1)         _
--    (SquareMatrix                         commute    commuteG2)         _
--    (UnivariatePolynomial                 commute    commuteG2)         _
--    ))                                                                  _
--  (MultivariatePolynomial . (                                           _
--    (DistributedMultivariatePolynomial    commute    commuteMultPol)    _
--    (Complex                             commute    commuteMultPol)    _
--    (MultivariatePolynomial               commute    commuteMultPol)    _
--    (NewDistributedMultivariatePolynomial commute    commuteMultPol)    _
--    (Polynomial                           commute    commuteMultPol)    _
--    (Quaternion                           commute    commuteMultPol)    _
--    (Fraction                        commute    commuteMultPol)    _
--    (SquareMatrix                         commute    commuteMultPol)    _
--    (UnivariatePolynomial                       commute    commuteMultPol)    _
--    ))                                                                  _
--  (Polynomial . (                                                       _
--    (DistributedMultivariatePolynomial    commute    commuteMultPol)    _
--    (Complex                              commute    commuteMultPol)    _
--    (MultivariatePolynomial               commute    commuteMultPol)    _
--    (NewDistributedMultivariatePolynomial commute    commuteMultPol)    _
--    (Polynomial                           commute    commuteMultPol)    _
--    (Quaternion                           commute    commuteMultPol)    _
--    (Fraction                             commute    commuteMultPol)    _
--    (SquareMatrix                         commute    commuteMultPol)    _
--    (UnivariatePolynomial                 commute    commuteMultPol)    _
--    ))                                                                  _
--  (Quaternion . (                                                       _
--    (DistributedMultivariatePolynomial    commute    commuteQuat2)      _
--    (MultivariatePolynomial               commute    commuteQuat2)      _
--    (NewDistributedMultivariatePolynomial commute    commuteQuat2)      _
--    (Polynomial                           commute    commuteQuat1)      _
--    (SquareMatrix                         commute    commuteQuat2)      _
--    (UnivariatePolynomial                       commute    commuteQuat2)      _
--    ))                                                                  _
--  (SquareMatrix . (                                                     _
--    (DistributedMultivariatePolynomial    commute    commuteSm2)        _
--    (Complex                             commute    commuteSm1)        _
--    (MultivariatePolynomial               commute    commuteSm2)        _
--    (NewDistributedMultivariatePolynomial commute    commuteSm2)        _
--    (Polynomial                           commute    commuteSm1)        _
--    (Quaternion                           commute    commuteSm1)        _
--    (SparseUnivariatePolynomial           commute    commuteSm1)        _
--    (UnivariatePolynomial                 commute    commuteSm2)        _
--    ))                                                                  _
--  (UnivariatePolynomial . (                                                   _
--    (DistributedMultivariatePolynomial    commute    commuteUp2)        _
--    (Complex                              commute    commuteUp1)        _
--    (MultivariatePolynomial               commute    commuteUp2)        _
--    (NewDistributedMultivariatePolynomial commute    commuteUp2)        _
--    (Polynomial                           commute    commuteUp1)        _
--    (Quaternion                           commute    commuteUp1)        _
--    (Fraction                        commute    commuteUp1)        _
--    (SparseUnivariatePolynomial           commute    commuteUp1)        _
--    (SquareMatrix                         commute    commuteUp2)        _
--    (UnivariatePolynomial                 commute    commuteUp2)        _
--    ))                                                                  _
--  ))

DEFPARAMETER($CommuteTable, '(                                           _
  (Complex . (                                                         _
    (DistributedMultivariatePolynomial    commute    commuteG2)         _
    (MultivariatePolynomial               commute    commuteG2)         _
    (HomogeneousDistributedMultivariatePolynomial commute    commuteG2) _
    (Polynomial                           commute    commuteG1)         _
    (Fraction                             commute    commuteG1)         _
    (SquareMatrix                         commute    commuteG2)         _
    (UnivariatePolynomial                 commute    commuteG2)         _
    ))                                                                  _
  (Polynomial . (                                                       _
    (Complex                              commute    commuteMultPol)    _
    (MultivariatePolynomial               commute    commuteMultPol)    _
    (HomogeneousDistributedMultivariatePolynomial commute    commuteMultPol)_
    (Polynomial                           commute    commuteMultPol)    _
    (Quaternion                           commute    commuteMultPol)    _
    (Fraction                             commute    commuteMultPol)    _
    (SquareMatrix                         commute    commuteMultPol)    _
    (UnivariatePolynomial                 commute    commuteMultPol)    _
    ))                                                                  _
  (SquareMatrix . (                                                     _
    (DistributedMultivariatePolynomial    commute    commuteSm2)        _
    (Complex                              commute    commuteSm1)        _
    (MultivariatePolynomial               commute    commuteSm2)        _
    (HomogeneousDistributedMultivariatePolynomial commute    commuteSm2)_
    (Polynomial                           commute    commuteSm1)        _
    (Quaternion                           commute    commuteSm1)        _
    (SparseUnivariatePolynomial           commute    commuteSm1)        _
    (UnivariatePolynomial                 commute    commuteSm2)        _
    ))                                                                  _
  ))
