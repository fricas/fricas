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

--% Functions on $clamList

-- These files are read in by the system so that they can be cached
-- properly.  Otherwise, must read in compiled versions and then
-- recompile these, resulting in wasted BPI space.

canCoerceFrom(mr,m) ==
  -- bind flag for recording/reporting instantiations
  -- (see recordInstantiation)
  $insideCanCoerceFrom: local := [mr,m]
  canCoerceFrom0(mr,m)

canCoerce(t1, t2) ==
  val := canCoerce1(t1, t2) => val
  t1 is ['Variable, :.] =>
    newMode := getMinimalVarMode(t1, nil)
    canCoerce1(t1, newMode) and canCoerce1(newMode, t2)
  nil

isValidType form ==
  -- returns true IFF form is a type whose arguments satisfy the
  --  predicate of the type constructor
  -- Note that some forms are said to be invalid because they would
  -- cause problems with the interpreter. Thus things like P P I
  -- are not valid.
  STRINGP form => true
  IDENTP  form => false
  form in '((Mode) (Type) (Category)) => true
  form is ['Record,:selectors] =>
    and/[isValidType type for [:.,type] in selectors]
  form is ['Enumeration,:args] =>
    null (and/[IDENTP x for x in args]) => false
    ((# args) = (# REMDUP args)) => true
    false
  form is ['Mapping,:mapargs] =>
    null mapargs => NIL
    and/[isValidType type for type in mapargs]
  form is ['Union,:args] =>
    -- check for a tagged union
    args and first args is [":",:.] =>
      and/[isValidType type for [:.,type] in args]
    null (and/[isValidType arg for arg in args]) => NIL
    ((# args) = (# REMDUP args)) => true
    sayKeyedMsg("S2IR0005",[form])
    NIL

  badDoubles := CONS($QuotientField, '(Complex Polynomial Expression))
  form is [T1, [T2, :.]] and T1 = T2 and member(T1, badDoubles) => NIL

  form is [=$QuotientField,D] and not isPartialMode(D) and
    ofCategory(D,'(Field)) => NIL
  form is ['UnivariatePolynomial, x, ['UnivariatePolynomial, y, .]] and x=y =>
    NIL
  form = '(Complex (AlgebraicNumber)) => NIL
  form is ['Expression, ['Kernel, . ]] => NIL
  form is [op,:argl] =>
    null constructor? op => nil
    cosig := GETDATABASE(op, 'COSIG)
    cosig and null rest cosig => -- niladic constructor
        null argl => true
        false
    null (sig := getConstructorSignature form) => nil
    [.,:cl] := sig
    -- following line is needed to deal with mutable domains
    if # cl ~= # argl and GENSYMP last argl then argl:= DROP(-1,argl)
    # cl ~= # argl => nil
    cl:= replaceSharps(cl,form)
    and/[isValid for x in argl for c in cl] where isValid ==
      categoryForm?(c) =>
        evalCategory(x, MSUBSTQ(x, '%, c)) and isValidType x
      not (GETDATABASE(opOf x, 'CONSTRUCTORKIND) = 'domain)

selectMms1(op,tar,args1,args2,$Coerce) ==
    selectMms2(op,tar,args1,args2,$Coerce)

coerceConvertMmSelection(funName,m1,m2) ==
  -- calls selectMms with $Coerce=NIL and tests for required
  -- target type. funName is either 'coerce or 'convert.
  $declaredMode : local:= NIL
  $reportBottomUpFlag : local:= NIL
  l := selectMms1(funName,m2,[m1],[m1],NIL)
  --  mmS := [[sig,[targ,arg],:pred] for x in l | x is [sig,[.,arg],:pred] and
  mmS := [x for x in l | x is [sig,:.] and hasCorrectTarget(m2,sig) and
           sig is [dc,targ,oarg] and isEqualOrSubDomain(m1,oarg)]
  mmS and first mmS

resolveTT(t1,t2) ==
  -- resolves two types
  -- this symmetric resolve looks for a type t to which both t1 and t2
  -- can be coerced
  -- if resolveTT fails, the result will be NIL
  startTimingProcess 'resolve
  null (t := resolveTT1(t1,t2)) =>
    stopTimingProcess 'resolve
    nil
  isValidType (t) =>
    stopTimingProcess 'resolve
    t
  stopTimingProcess 'resolve
  nil

isLegitimateMode(t,hasPolyMode,polyVarList) ==
  -- returns true IFF t is a valid type.  i.e. if t has no repeated
  --  variables, or two levels of Polynomial
  null t        => true    -- a terminating condition with underDomainOf
  t = $EmptyMode => true
  STRINGP t     => true
  ATOM t => false

  badDoubles := CONS($QuotientField, '(Complex Polynomial Expression))
  t is [T1, [T2, :.]] and T1 = T2 and member(T1, badDoubles) => NIL

  t is [=$QuotientField,D] and not isPartialMode(D) and
    ofCategory(D,'(Field)) => NIL
  t = '(Complex (AlgebraicNumber)) => NIL

  vl := isPolynomialMode t =>
    if vl~='all then
      var:= or/[(x in polyVarList => x;nil) for x in vl] => return false
      listOfDuplicates vl => return false
      polyVarList:= union(vl,polyVarList)
    hasPolyMode => false
    con := first t
    poly? := (con = 'Polynomial or con = 'Expression)
    isLegitimateMode(underDomainOf t,poly?,polyVarList)

  constructor? first t =>
    isLegitimateMode(underDomainOf t,hasPolyMode,polyVarList) => t
  t is ['Mapping,:ml] =>
    null ml => NIL
    -- first arg is target, which can be Void
    null isLegitimateMode(first ml,nil,nil) => NIL
    for m in rest ml repeat
      m = $Void =>
        return NIL
      null isLegitimateMode(m,nil,nil) => return NIL
    true
  t is ['Union,:ml] =>
    -- check for tagged union
    ml and first ml is [":",:.] => isLegitimateRecordOrTaggedUnion ml
    null (and/[isLegitimateMode(m,nil,nil) for m in ml]) => NIL
    ((# ml) = (# REMDUP ml)) => true
    NIL
  t is ['Record,:r] => isLegitimateRecordOrTaggedUnion r
  t is ['Enumeration,:r] =>
    null (and/[IDENTP x for x in r]) => false
    ((# r) = (# REMDUP r)) => true
    false
  false

underDomainOf t ==
  t = $RationalNumber => $Integer
  not PAIRP t => NIL
  d := deconstructT t
  1 = #d => NIL
  u := getUnderModeOf(t) => u
  last d

findRetractMms(st, tt) == findRetractMms1(st, tt)

getConstantFromDomain(form,domainForm) ==
    getConstantFromDomain1(form,domainForm)

interpLookup(funName, sig, dc) ==
    dcVector:= evalDomain dc
    NRTcompiledLookup(funName, sig, dcVector)
