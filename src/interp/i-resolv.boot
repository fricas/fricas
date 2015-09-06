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
new resolution: types and modes

a type is any term (structure) which can be regarded as a
  functor call
a basic type is the call of a nullary functor (e.g. (Integer)),
  otherwise it is a structured type (e.g. (Polynomial (Integer)))
a functor together with its non-type arguments is called a
  type constructor

a mode is a type which can be partially specified, i.e. a term
  containing term variables
a term variable (denoted by control-L) stands for any nullary or unary function
  which was build from type constructors
this means, a term variable can be:
  a function LAMBDA ().T, where T is a type
  a function LAMBDA (X).T(X), where X is a variable for a type and
    T a type containing this variable
  a function LAMBDA X.X ("control-L can be disregarded")
examples:
  P(control-L) can stand for (Polynomial (Fraction (Integer)))
  G(control-L(I)) can stand for (Gaussian (Polynomial (Integer))), but also
    for (Gaussian (Integer))


Resolution of Two Types

this symmetric resolution is done the following way:
1. if the same type constructor occurs in both terms, then the
   type tower is built around this constructor (resolveTTEq)
2. the next step is to look for two constructors which have an
   "algebraic relationship", this means, a rewrite rule is
   applicable (e.g. UP(x,I) and MP([x,y],I))
   this is done by resolveTTRed
3. if none of this is true, then a tower of types is built
   e.g. resolve P I and G I to P G I
)endif

resolveTypeList u ==
  u is [a,:tail] =>

    -- if the list consists entirely of variables then keep it explicit
    allVars :=
      a is ['Variable,v] => [v]
      nil
    while allVars for b in tail repeat
        allVars :=
            b is ['Variable,v] => insert(v, allVars)
            nil
    allVars =>
        null rest allVars => ['Variable, first allVars]
        ['OrderedVariableList,nreverse allVars]

    for md in tail repeat
      a := resolveTT(md,a)
      null a => return nil
    a
  throwKeyedMsg("S2IR0002",NIL)

-- resolveTT is in CLAMMED BOOT

resolveTypeListAny tl ==
  rt := resolveTypeList tl
  null rt => $Any
  rt

resolveTTAny(t1,t2) ==
  (t3 := resolveTT(t1, t2)) => t3
  $Any

resolveTT1(t1,t2) ==
  -- this is the main symmetric resolve
  -- first it looks for equal constructors on both sides
  -- then it tries to use a rewrite rule
  -- and finally it builds up a tower
  t1=t2 => t1
  (t1 = '$NoValueMode) or (t2 = '$NoValueMode) => NIL
  (t1 = $Void) or (t2 = $Void) => $Void
  (t1 = $Any) or (t2 = $Any) => $Any
  t1 = '(Exit) => t2
  t2 = '(Exit) => t1
  t1 is ['Union,:.] => resolveTTUnion(t1,t2)
  t2 is ['Union,:.] => resolveTTUnion(t2,t1)
  STRINGP(t1) =>
    t2 = $String => t2
    NIL
  STRINGP(t2) =>
    t1 = $String => t1
    NIL
  null acceptableTypesToResolve(t1,t2) => NIL
  if compareTT(t1,t2) then
     t := t1
     t1 := t2
     t2 := t
  (t := resolveTTSpecial(t1,t2)) and isValidType t => t
  (t := resolveTTSpecial(t2,t1)) and isValidType t => t
  isSubTowerOf(t1,t2) and canCoerceFrom(t1,t2) => t2
  isSubTowerOf(t2,t1) and canCoerceFrom(t2,t1) => t1
  t := resolveTTRed(t1,t2) => t
  t := resolveTTCC(t1,t2) => t
  (t := resolveTTEq(t1,t2)) and isValidType t => t
  [c1,:arg1] := deconstructT t1
  arg1 and
    [c2,:arg2] := deconstructT t2
    arg2 and
      t := resolveTT1(last arg1,last arg2)
      t and ( resolveTT2(c1,c2,arg1,arg2,t) or
        resolveTT2(c2,c1,arg2,arg1,t) )

acceptableTypesToResolve(t1,t2) ==
  -- this is temporary. It ensures that two types that have coerces
  -- that really should be converts don't automatically resolve.
  -- when the coerces go away, so will this.
  acceptableTypesToResolve1(t1,t2) and
    acceptableTypesToResolve1(t2,t1)

acceptableTypesToResolve1(t1,t2) ==
  t1 = $Integer =>
    t2 = $String => NIL
    true
  t1 = $DoubleFloat or t1 = $Float =>
    t2 = $String => NIL
    t2 = [$QuotientField, $Integer] => NIL
    true
  true

resolveTT2(c1,c2,arg1,arg2,t) ==
  -- builds a tower and tests for all the necessary coercions
  t0 := constructM(c2,replaceLast(arg2,t))
  canCoerceFrom(t,t0) and
    t1 := constructM(c1,replaceLast(arg1,t0))
    canCoerceFrom(t0,t1) and t1

resolveTTUnion(t1 is ['Union,:doms],t2) ==
  unionDoms1 :=
    doms and first doms is [":",:.] =>
      tagged := true
      [t for [.,.,t] in doms]
    tagged := false
    doms
  member(t2,unionDoms1) => t1
  tagged => NIL
  t2 isnt ['Union,:doms2] =>
    ud := nil
    bad := nil
    for d in doms while not bad repeat
      d = '"failed" => ud := [d,:ud]
      null (d' := resolveTT(d,t2)) => bad := true
      ud := [d',:ud]
    bad => NIL
    ['Union,:REMDUP reverse ud]
  ud := nil
  bad := nil
  for d in doms2 while not bad repeat
    d = '"failed" => ud := append(ud,[d])
    null (d' := resolveTTUnion(t1,d)) => bad := true
    ud := append(ud, rest d')
  bad => NIL
  ['Union,:REMDUP ud]

resolveTTSpecial(t1,t2) ==
  -- tries to resolve things that would otherwise get mangled in the
  -- rest of the resolve world. I'll leave it for Albi to fix those
  -- things. (RSS 1/-86)

  -- following is just an efficiency hack
  (t1 = '(Symbol) or t1 is ['OrderedVariableList,.]) and PAIRP(t2) and
      first(t2) = 'Polynomial => t2

  (t1 = '(Symbol)) and ofCategory(t2, '(IntegerNumberSystem)) =>
    resolveTT1(['Polynomial, t2], t2)

  t1 = '(AlgebraicNumber) and (t2 = $Float or t2 = $DoubleFloat) =>
    ['Expression, t2]
  t1 = '(AlgebraicNumber) and (t2 = ['Complex, $Float] or t2 = ['Complex, $DoubleFloat]) =>
    ['Expression, CADR t2]

  t1 = '(AlgebraicNumber) and t2 is ['Complex,.] =>
    resolveTT1('(Expression (Integer)), t2)

  t1 is ['SimpleAlgebraicExtension,F,Rep,poly] =>
    t2 = Rep => t1
    t2 is ['UnivariatePolynomial,x,R] and (t3 := resolveTT(t1, R)) =>
      ['UnivariatePolynomial,x,t3]
    t2 is ['Variable,x] and (t3 := resolveTT(t1, F)) =>
      ['UnivariatePolynomial,x,t3]
    t2 is ['Polynomial,R] and (R' := resolveTT(Rep, t2)) =>
      R' = Rep => t1
      ['Polynomial,t1]
    canCoerceFrom(t2,F) => t1
    nil
  t1 = $PositiveInteger and ofCategory(t2,'(Ring)) =>
    resolveTT1($Integer,t2)
  t1 = $NonNegativeInteger and ofCategory(t2,'(Ring)) =>
    resolveTT1($Integer,t2)
  t1 is ['OrderedVariableList,[x]] => resolveTTSpecial(['Variable, x], t2)
  t1 is ['OrderedVariableList,vl] =>
    ofCategory(t2,'(Ring)) => resolveTT(['Polynomial,'(Integer)],t2)
    resolveTT($Symbol,t2)
  t1 is ['Variable,x] =>
    EQCAR(t2,'SimpleAlgebraicExtension) => resolveTTSpecial(t2,t1)
    t2 is ['UnivariatePolynomial,y,S] =>
      x = y => t2
      resolveTT1(['UnivariatePolynomial,x,'(Integer)],t2)
    t2 is ['Variable,y] =>
      x = y => t1
--    ['OrderedVariableList, MSORT [x,y]]
      $Symbol
    t2 = '(Symbol) => t2
    t2 is ['Polynomial,.] => t2
    t2 is ['OrderedVariableList, vl] and member(x,vl) => t2
    isPolynomialMode t2 => nil
    ofCategory(t2, '(IntegerNumberSystem)) => resolveTT(['Polynomial, t2], t2)
    resolveTT(['Polynomial,'(Integer)],t2)
  t1 is ['FunctionCalled,f] and t2 is ['FunctionCalled,g] =>
    null (mf := get(f,'mode,$e)) => NIL
    null (mg := get(g,'mode,$e)) => NIL
    mf ~= mg => NIL
    mf
  t1 is ['UnivariatePolynomial,x,S] =>
    EQCAR(t2,'Variable) =>
      resolveTTSpecial(t2,t1)
    EQCAR(t2,'SimpleAlgebraicExtension) =>
      resolveTTSpecial(t2,t1)
    t2 is ['UnivariatePolynomial,y,T] =>
      (x = y) and (U := resolveTT1(S,T)) and ['UnivariatePolynomial,x,U]
    nil
  t1 = '(Pi) =>
    t2 is ['Complex,d] => defaultTargetFE t2
    t2 is ['AlgebraicNumber] => defaultTargetFE t2
    EQCAR(t2, 'Variable) or t2 = $Symbol =>
      defaultTargetFE($Symbol)
    t2 is ['Polynomial, .] or t2 is ['Fraction, ['Polynomial, .]] =>
      defaultTargetFE(t2)
    nil
  t1 is ['Polynomial,['Complex,u1]] and t2 is ['Complex,u2] =>
    resolveTT1(t1,u2)
  t1 is ['Polynomial,R] and t2 is ['Complex,S] =>
    containsPolynomial(S) => resolveTT1(['Polynomial,['Complex,R]],t2)
    ['Polynomial,['Complex,resolveTT1(R,S)]]
  t1 is ['Expression, R] and t2 is ['Complex,S] =>
    dom' := resolveTT(R, t2)
    null dom' => nil
    ['Expression, dom']
  t1 is ['Segment, dom] and t2 isnt ['Segment,.] =>
    dom' := resolveTT(dom, t2)
    null dom' => nil
    ['Segment, dom']
  op1 := first(t1)
  op2 := first(t2)
  MEMQ(op1, '(GeneralUnivariatePowerSeries SparseUnivariateLaurentSeries _
            SparseUnivariatePuiseuxSeries SparseUnivariateTaylorSeries _
            UnivariateLaurentSeries UnivariatePuiseuxSeries _
            UnivariateTaylorSeries)) =>
      [., S1, var1, cen1] := t1
      op1 = op2 =>
          [., S2, var2, cen2] := t2
          not algEqual(var1, var2, $Symbol) => nil
          (U := resolveTT1(S1, S2)) =>
               cen1 :=
                   U = S1 => cen1
                   objValUnwrap(coerceInt(objNewWrap(cen1, S1), U))
               cen2 :=
                   U = S2 => cen2
                   objValUnwrap(coerceInt(objNewWrap(cen2, S2), U))
               algEqual(cen1, cen2, U) =>
                   [op1, U, var1, cen1]
               nil
          nil
  nil

resolveTTCC(t1,t2) ==
  -- tries to use canCoerceFrom information to see if types can be
  -- coerced to one another
  gt21 := GGREATERP(t2,t1)
  (c12 := canCoerceFrom(t1,t2)) and gt21 => t2
  c21 := canCoerceFrom(t2,t1)
  null (c12 or c21) => NIL
  c12 and not c21 => t2
  c21 and not c12 => t1
  -- both are coerceable to each other
  if gt21 then t1 else t2

resolveTTEq(t1,t2) ==
  -- tries to find the constructor of t1 somewhere in t2 (or vice versa)
  -- and move the other guy to the top
  [c1,:arg1] := deconstructT t1
  [c2,:arg2] := deconstructT t2
  t := resolveTTEq1(c1,arg1,[c2,arg2]) => t
  t := ( arg1 and resolveTTEq2(c2,arg2,[c1,arg1]) ) => t
  arg2 and resolveTTEq2(c1,arg1,[c2,arg2])

resolveTTEq1(c1,arg1,TL is [c2,arg2,:.]) ==
  -- takes care of basic types and of types with the same constructor
  -- calls resolveTT1 on the arguments in the second case
  null arg1 and null arg2 =>
    canCoerceFrom(c1,c2) => constructTowerT(c2,CDDR TL)
    canCoerceFrom(c2,c1) and constructTowerT(c1,CDDR TL)
  c1=c2 and
    [c2,arg2,:TL] := bubbleType TL
    until null arg1 or null arg2 or not t repeat
      t := resolveTT1(first arg1, first arg2) =>
        arg := CONS(t,arg)
        arg1 := rest arg1
        arg2 := rest arg2
    t and null arg1 and null arg2 and
      t0 := constructM(c1,nreverse arg)
      constructTowerT(t0,TL)

resolveTTEq2(c1,arg1,TL is [c,arg,:.]) ==
  -- tries to resolveTTEq the type [c1,arg1] with the last argument
  -- of the type represented by TL
  [c2,:arg2] := deconstructT last arg
  TL := [c2,arg2,:TL]
  t := resolveTTEq1(c1,arg1,TL) => t
  arg2 and resolveTTEq2(c1,arg1,TL)

resolveTTRed(t1,t2) ==
  -- the same function as resolveTTEq, but instead of testing for
  -- constructor equality, it looks whether a rewrite rule can be applied
  t := resolveTTRed1(t1,t2,NIL) => t
  [c1,:arg1] := deconstructT t1
  t := arg1 and resolveTTRed2(t2,last arg1,[c1,arg1]) => t
  [c2,:arg2] := deconstructT t2
  arg2 and resolveTTRed2(t1,last arg2,[c2,arg2])

resolveTTRed1(t1,t2,TL) ==
  -- tries to apply a reduction rule on (Resolve t1 t2)
  -- then it creates a type using the result and TL
  EQ(t,term1RW(t := ['Resolve,t1,t2],$Res)) and
    EQ(t,term1RW(t := ['Resolve,t2,t1],$Res)) => NIL
  [c2,:arg2] := deconstructT t2
  [c2,arg2,:TL] := bubbleType [c2,arg2,:TL]
  t2 := constructM(c2,arg2)
  l := term1RWall(['Resolve,t1,t2],$Res)
  for t0 in l until t repeat t := resolveTTRed3 t0
  l and t => constructTowerT(t,TL)
  l := term1RWall(['Resolve,t2,t1],$Res)
  for t0 in l until t repeat t := resolveTTRed3 t0
  l and t and constructTowerT(t,TL)

resolveTTRed2(t1,t2,TL) ==
  -- tries to resolveTTRed t1 and t2 and build a type using TL
  t := resolveTTRed1(t1,t2,TL) => t
  [c2,:arg2] := deconstructT t2
  arg2 and resolveTTRed2(t1,last arg2,[c2,arg2,:TL])

resolveTTRed3(t) ==
  -- recursive resolveTTRed which handles all subterms of the form
  -- (Resolve t1 t2) or subterms which have to be interpreted
  atom t => t
  t is ['Resolve,a,b] =>
    ( t1 := resolveTTRed3 a ) and ( t2 := resolveTTRed3 b ) and
      resolveTT1(t1,t2)
  t is ['Incl,a,b] => member(a,b) and b
  t is ['SetDiff,a,b] => intersection(a,b) and SETDIFFERENCE(a,b)
  t is ['SetComp,a,b] =>
    and/[member(x,a) for x in b] and SETDIFFERENCE(a,b)
  t is ['SetInter,a,b] => intersection(a,b)
  t is ['SetUnion,a,b] => union(a,b)
  t is ['VarEqual,a,b] => (a = b) and a
  t is ['SetEqual,a,b] =>
    (and/[member(x,a) for x in b] and and/[member(x,b) for x in a]) and a
  [( atom x and x ) or ((not cs and x and not interpOp? x and x)
    or resolveTTRed3 x) or return NIL
      for x in t for cs in GETDATABASE(first t, 'COSIG)]

interpOp?(op) ==
  PAIRP(op) and
    first(op) in '(Incl SetDiff SetComp SetInter SetUnion VarEqual SetEqual)

--% Resolve Type with Category

resolveTCat(t,c) ==
  -- this function attempts to find a type tc of category c such that
  -- t can be coerced to tc. NIL returned for failure.
  -- Example:  t = Integer, c = Field ==> tc = Fraction(Integer)

  -- first check whether t already belongs to c
  ofCategory(t,c) => t

  -- if t is built by a parametrized constructor and there is a
  -- condition on the parameter that matches the category, try to
  -- recurse. An example of this is (G I, Field) -> G RN

  rest(t) and (tc := resolveTCat1(t,c)) => tc

  -- now check some specific niladic categories
  c in '((Field) (EuclideanDomain)) and ofCategory(t,'(IntegralDomain))=>
      [$QuotientField, t]

  c = '(Field) and t = $Symbol =>
      [$QuotientField, ['Fraction, $Integer]]

  c = '(Ring) and t is ['FactoredForm,t0] => ['FactoredRing,t0]

  (t is [t0]) and (sd := getImmediateSuperDomain(t0)) and sd ~= t0 =>
    resolveTCat(sd,c)

  SIZE(td := deconstructT t) ~= 2=> NIL
  SIZE(tc := deconstructT c) ~= 2 => NIL
  ut := underDomainOf t
  null isValidType(uc := last tc) => NIL
  null canCoerceFrom(ut,uc) => NIL
  nt := constructT(first td,[uc])
  ofCategory(nt,c) => nt
  NIL

resolveTCat1(t,c) ==
  -- does the hard work of looking at conditions on under domains
  -- if null (ut := getUnderModeOf(t)) then ut := last dt
  null (conds := getConditionsForCategoryOnType(t,c)) => NIL
--rest(conds) => NIL   -- will handle later
  cond := first conds
  cond isnt [.,['has, pat, c1],:.] => NIL
  rest(c1) => NIL      -- make it simple

  argN := 0
  t1 := nil

  for ut in rest t for i in 1.. while (argN = 0) repeat
    sharp := INTERNL('"#",STRINGIMAGE i)
    sharp = pat =>
      argN := i
      t1 := ut

  null t1 => NIL
  null (t1' := resolveTCat(t1,c1)) => NIL
  t' := copy t
  t'.argN := t1'
  t'

getConditionsForCategoryOnType(t,cat) ==
  getConditionalCategoryOfType(t,[NIL],['ATTRIBUTE,cat])

getConditionalCategoryOfType(t,conditions,match) ==
  if PAIRP t then t := first t
  t in '(Union Mapping Record) => NIL
  conCat := GETDATABASE(t,'CONSTRUCTORCATEGORY)
  REMDUP rest getConditionalCategoryOfType1(conCat, conditions, match, [[]])

getConditionalCategoryOfType1(cat,conditions,match,seen) ==
  cat is ['Join,:cs] or cat is ['CATEGORY,:cs] =>
    null cs => conditions
    getConditionalCategoryOfType1([first cat,:rest cs],
     getConditionalCategoryOfType1(first cs,conditions,match,seen),
       match,seen)
  cat is ['IF,., cond,.] =>
    matchUpToPatternVars(cond,match,NIL) =>
      RPLACD(conditions, CONS(cat, rest conditions))
      conditions
    conditions
  cat is [catName,:.] and (GETDATABASE(catName,'CONSTRUCTORKIND) = 'category) =>
    cat in rest seen => conditions
    RPLACD(seen, [cat, :rest seen])
    subCat := GETDATABASE(catName,'CONSTRUCTORCATEGORY)
    -- substitute vars of cat into category
    for v in rest cat for vv in $TriangleVariableList repeat
      subCat := SUBST(v,vv,subCat)
    getConditionalCategoryOfType1(subCat,conditions,match,seen)
  conditions

matchUpToPatternVars(pat,form,patAlist) ==
  -- tries to match pattern variables (of the # form) in pat
  -- against expressions in form. If one is found, it is checked
  -- against the patAlist to make sure we are using the same expression
  -- each time.
  EQUAL(pat,form) => true
  isSharpVarWithNum(pat) =>
    -- see is pattern variable is in alist
    (p := assoc(pat, patAlist)) => EQUAL(form, rest p)
    patAlist := [[pat,:form],:patAlist]
    true
  PAIRP(pat) =>
    not (PAIRP form) => NIL
    matchUpToPatternVars(first pat, first form, patAlist) and
      matchUpToPatternVars(rest pat, rest form, patAlist)
  NIL

--% Resolve Type with Mode

-- only implemented for nullary control-L's (which stand for types)

resolveTMOrCroak(t,m) ==
  resolveTM(t,m) or throwKeyedMsg("S2IR0004",[t,m])

resolveTM(t,m) ==
  -- resolves a type with a mode which may be partially specified
  startTimingProcess 'resolve
  $Subst : local := NIL
  $Coerce : local := 'T
  m := SUBSTQ("**",$EmptyMode,m)
  tt := resolveTM1(t,m)
  result := tt and isValidType tt and tt
  stopTimingProcess 'resolve
  result

resolveTM1(t,m) ==
  -- general resolveTM, which looks for a term variable
  -- otherwise it looks whether the type has the same top level
  -- constructor as the mode, looks for a rewrite rule, or builds up
  -- a tower
  t=m => t
  m is ['Union,:.] => resolveTMUnion(t,m)
  m = '(Void) => m
  m = '(Any) => m
  m = '(Exit) => t
  containsVars m =>
    isPatternVar m =>
      p := ASSQ(m,$Subst) =>
        $Coerce =>
          tt := resolveTT1(t, rest p) => RPLACD(p, tt) and tt
          NIL
        t = rest p and t
      $Subst := CONS(CONS(m,t),$Subst)
      t
    atom(t) or atom(m) => NIL
    (t is ['Record,:tr]) and (m is ['Record,:mr]) and
      (tt := resolveTMRecord(tr,mr)) => tt
    t is ['Record,:.] or m is ['Record,:.] => NIL
    t is ['Variable, .] and m is ['Mapping, :.] => m
    t is ['FunctionCalled, .] and m is ['Mapping, :.] => m
    if isEqualOrSubDomain(t, $Integer) then
      t := $Integer
    tt := resolveTMEq(t,m) => tt
    $Coerce and
      tt := resolveTMRed(t,m) => tt
      resolveTM2(t,m)
  $Coerce and canCoerceFrom(t,m) and m

resolveTMRecord(tr,mr) ==
  #tr ~= #mr => NIL
  ok := true
  tt := NIL
  for ta in tr for ma in mr while ok repeat
    -- element is [':,tag,mode]
    CADR(ta) ~= CADR(ma) => ok := NIL      -- match tags
    ra := resolveTM1(CADDR ta, CADDR ma)   -- resolve modes
    null ra => ok := NIL
    tt := CONS([first ta, CADR ta, ra], tt)
  null ok => NIL
  ['Record,nreverse tt]

resolveTMUnion(t, m is ['Union,:ums]) ==
  isTaggedUnion m => resolveTMTaggedUnion(t,m)
  -- resolves t with a Union type
  t isnt ['Union,:uts] =>
    ums := REMDUP spliceTypeListForEmptyMode([t],ums)
    ums' := nil
    success := nil
    for um in ums repeat
      (um' := resolveTM1(t,um)) =>
        success := true
        um' in '(T TRUE) => ums' := [um,:ums']
        ums' := [um',:ums']
      ums' := [um,:ums']
    -- remove any duplicate domains that might have been created
    m' := ['Union,:REMDUP reverse ums']
    success =>
      null CONTAINED('_*_*,m') => m'
      t = $Integer => NIL
      resolveTM1($Integer,m')
    NIL
  -- t is actually a Union if we got here
  ums := REMDUP spliceTypeListForEmptyMode(uts,ums)
  bad := nil
  doms := nil
  for ut in uts while not bad repeat
    (m' := resolveTMUnion(ut,['Union,:ums])) =>
      doms := append(rest m', doms)
    bad := true
  bad => NIL
  ['Union,:REMDUP doms]

resolveTMTaggedUnion(t, m is ['Union,:ums]) ==
  NIL

spliceTypeListForEmptyMode(tl,ml) ==
  -- splice in tl for occurrence of ** in ml
  null ml => nil
  ml is [m,:ml'] =>
    m = "**" => append(tl,spliceTypeListForEmptyMode(tl,ml'))
    [m,:spliceTypeListForEmptyMode(tl,ml')]

resolveTM2(t,m) ==
  -- resolves t with the last argument of m and builds up a tower
  [cm,:argm] := deconstructT m
  argm and
    tt := resolveTM1(t,last argm)
    tt and
      ttt := constructM(cm,replaceLast(argm,tt))
      ttt and canCoerceFrom(tt,ttt) and ttt

resolveTMEq(t,m) ==
  -- tests whether t and m have the same top level constructor, which,
  -- in the case of t, could be bubbled up
  (res := resolveTMSpecial(t,m)) => res
  [cm,:argm] := deconstructT m
  c := containsVars cm
  TL := NIL
  until b or not t repeat
    [ct,:argt] := deconstructT t
    b :=
      c =>
        SL := resolveTMEq1(ct,cm)
        not EQ(SL,'failed)
      ct=cm
    not b =>
      TL := [ct,argt,:TL]
      t := argt and last argt
  b and
    t := resolveTMEq2(cm,argm,[ct,argt,:TL])
    if t then for p in SL repeat $Subst := augmentSub(first p, rest p, $Subst)
    t

resolveTMSpecial(t,m) ==
  -- a few special cases
  t = $AnonymousFunction and m is ['Mapping,:.] => m
  t is ['Variable,x] and m is ['OrderedVariableList,le] =>
    isPatternVar le => ['OrderedVariableList,[x]]
    PAIRP(le) and member(x,le) => le
    NIL
  t is ['Fraction, ['Complex, t1]] and m is ['Complex, m1] =>
    resolveTM1(['Complex, ['Fraction, t1]], m)
  t is ['Fraction, ['Polynomial, ['Complex, t1]]] and m is ['Complex, m1] =>
    resolveTM1(['Complex, ['Fraction, ['Polynomial, t1]]], m)
  t is ['Mapping,:lt] and m is ['Mapping,:lm] =>
    #lt ~= #lm => NIL
    l := NIL
    ok := true
    for at in lt for am in lm while ok repeat
      (ok := resolveTM1(at,am)) => l := [ok,:l]
    ok and ['Mapping,:reverse l]
  t is ['Segment,u] and m is ['UniversalSegment,.] =>
    resolveTM1(['UniversalSegment, u], m)
  NIL

resolveTMEq1(ct,cm) ==
  -- ct and cm are type constructors
  -- tests for a match from cm to ct
  -- the result is a substitution or 'failed
  not (first ct = first cm) => 'failed
  SL := NIL
  ct := rest ct
  cm := rest cm
  b := 'T
  while ct and cm and b repeat
    xt := first ct
    ct := rest ct
    xm := first cm
    cm := rest cm
    if not (atom xm) and first xm = ":"  --  i.e. Record
      and first xt = ":" and CADR xm = CADR xt then
        xm := CADDR xm
        xt := CADDR xt
    b :=
      xt=xm => 'T
      isPatternVar(xm) and
        p := ASSQ(xm, $Subst) => xt = rest p
        p := ASSQ(xm, SL) => xt = rest p
        SL := augmentSub(xm,xt,SL)
  b => SL
  'failed

resolveTMEq2(cm,argm,TL) ==
  -- [cm,argm] is a deconstructed mode,
  -- TL is a deconstructed type t
  [ct,argt,:TL] :=
    $Coerce => bubbleType TL
    TL
  argt0 := argt
  null TL and
    null argm => constructM(ct,argt)
--  null argm => NIL
    arg := NIL
    while argt and argm until not tt repeat
      x1 := first argt
      argt := rest argt
      x2 := first argm
      argm := rest argm
      tt := resolveTM1(x1,x2) =>
        arg := CONS(tt,arg)
    tt and arg = argt0 => constructT(ct, argt0)
    null argt and null argm and tt and constructM(ct,nreverse arg)

resolveTMRed(t,m) ==
  -- looks for an applicable rewrite rule at any level of t and tries
  --   to bubble this constructor up to the top to t
  TL := NIL
  until b or not t repeat
    [ct,:argt] := deconstructT t
    b := not EQ(t,term1RW(['Resolve,t,m],$ResMode)) and
      [c0,arg0,:TL0] := bubbleType [ct,argt,:TL]
      null TL0 and
        l := term1RWall(['Resolve,constructM(c0,arg0),m],$ResMode)
        for t0 in l until t repeat t := resolveTMRed1 t0
        l and t
    b or
      TL := [ct,argt,:TL]
      t := argt and last argt
  b and t

resolveTMRed1(t) ==
  -- recursive resolveTMRed which handles all subterms of the form
  -- (Resolve a b)
  atom t => t
  t is ['Resolve,a,b] =>
    ( a := resolveTMRed1 a ) and ( b := resolveTMRed1 b ) and
      resolveTM1(a,b)
  t is ['Incl,a,b] => PAIRP b and member(a,b) and b
  t is ['Diff,a,b] => PAIRP a and member(b,a) and SETDIFFERENCE(a,[b])
  t is ['SetIncl,a,b] => PAIRP b and and/[member(x,b) for x in a] and b
  t is ['SetDiff,a,b] => PAIRP b and PAIRP b and
                         intersection(a,b) and SETDIFFERENCE(a,b)
  t is ['VarEqual,a,b] => (a = b) and b
  t is ['SetComp,a,b] => PAIRP a and PAIRP b and
    and/[member(x,a) for x in b] and SETDIFFERENCE(a,b)
  t is ['SimpleAlgebraicExtension,a,b,p] =>  -- this is a hack. RSS
    ['SimpleAlgebraicExtension, resolveTMRed1 a, resolveTMRed1 b,p]
  [( atom x and x ) or resolveTMRed1 x or return NIL for x in t]

--% Type and Mode Representation

getUnderModeOf d ==
  not PAIRP d => NIL
  for a in rest d for m in rest destructT d repeat
    if m then return a

--deconstructM(t) ==
--  -- M is a type, which may contain type variables
--  -- results in a pair (type constructor . mode arguments)
--  rest t and constructor? first t =>
--    dt := destructT first t
--    args := [ x for d in dt for y in t | ( x := d and y ) ]
--    c := [ x for d in dt for y in t | ( x := not d and y ) ]
--    CONS(c,args)
--  CONS(t,NIL)

deconstructT(t) ==
  -- M is a type, which may contain type variables
  -- results in a pair (type constructor . mode arguments)
  IFCDR t and constructor? first t =>
    dt := destructT first t
    args := [ x for d in dt for y in t | ( x := d and y ) ]
    c := [ x for d in dt for y in t | ( x := not d and y ) ]
    CONS(c,args)
  CONS(t,NIL)

constructT(c,A) ==
  -- c is a type constructor, A a list of argument types
  A => [if d then POP A else POP c for d in destructT first c]
  c

constructM(c,A) ==
  -- replaces top level RE's or QF's by equivalent types, if possible
  #c > 1 and nontrivialCosig(first(c)) => nil
  containsVars(c) or containsVars(A) => NIL
  -- collapses illegal FE's
  first(c) = $FunctionalExpression => defaultTargetFE first A
  constructT(c,A)

replaceLast(A,t) ==
  -- replaces the last element of the nonempty list A by t (constructively
  nreverse RPLACA(reverse A,t)

nontrivialCosig(x) ==
   cs := GETDATABASE(x, "COSIG")
   sig := getConstructorSignature x
   not("and"/[c or freeOfSharpVars s for c in rest cs for s in rest sig])

destructT(functor)==
  -- provides a list of booleans, which indicate whether the arguments
  -- to the functor are category forms or not
  GETDATABASE(opOf functor,'COSIG)

constructTowerT(t,TL) ==
  -- t is a type, TL a list of constructors and argument lists
  -- t is embedded into TL
  while TL and t repeat
    [c,arg,:TL] := TL
    t0 := constructM(c,replaceLast(arg,t))
    t := canCoerceFrom(t,t0) and t0
  t

bubbleType(TL) ==
  -- tries to move the last constructor in TL upwards
  -- uses canCoerceFrom to test whether two constructors can be bubbled
  [c1,arg1,:T1] := TL
  null T1 or null arg1 => TL
  [c2,arg2,:T2] := T1
  t := last arg1
  t2 := constructM(c2,replaceLast(arg2,t))
  arg1 := replaceLast(arg1,t2)
  newCanCoerceCommute(c2,c1) or canCoerceCommute(c2, c1) =>
    bubbleType [c1,arg1,:T2]
  TL

bubbleConstructor(TL) ==
  -- TL is a nonempty list of type constructors and nonempty argument
  -- lists representing a deconstructed type
  -- then the lowest constructor is bubbled to the top
  [c,arg,:T1] := TL
  t := last arg
  until null T1 repeat
    [c1,arg1,:T1] := T1
    arg1 := replaceLast(arg1,t)
    t := constructT(c1,arg1)
  constructT(c,replaceLast(arg,t))

compareTT(t1,t2) ==
  -- 'T if type t1 is more nested than t2
  -- otherwise 'T if t1 is lexicographically greater than t2
  EQCAR(t1,$QuotientField) or
    MEMQ(opOf t2,[$QuotientField, 'SimpleAlgebraicExtension]) => NIL
    CGREATERP(PRIN2CVEC opOf t1,PRIN2CVEC opOf t2)
