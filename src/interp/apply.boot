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

oldCompilerAutoloadOnceTrigger() == nil

compAtomWithModemap(x, m, e, v) ==
    res := nil
    v1 := nil
    while not res for map in v | map is [[.,target],[.,fn]] repeat
        ATOM(fn) => "iterate"
        not(modeEqual(m, target)) => v1 := cons(map, v1)
        -- try exact match
        [[md, mr], :fnsel] := map
        compMapCond(x, md, [], fnsel) =>
            res := trans_delta(genDeltaEntry [x, :map], target, e)
        v1 := cons(map, v1)
    res => res
    v1 := NREVERSE(v1)
    -- now try inexact
    while not res for map in v1 repeat
        mr := resolve(target, m)
        not(mr) => "iterate"
        not coerceable(mr, m, e) => "iterate"
        [[md, mr], :fnsel] := map
        if compMapCond(x, md, [], fnsel) then
            res := trans_delta(genDeltaEntry [x, :map], target, e)
            res := convert(res, m)
    res

trans_delta(fn, target, e) ==
    fn1 :=
        fn is ["XLAM", :.] => [fn]
        ["call", fn]
    [fn1, target, e]

compToApply(op,argl,m,e) ==
  T:= compNoStacking(op,$EmptyMode,e) or return nil
  m1:= T.mode
  T.expr is ["QUOTE", =m1] => nil
  compApplication(op,argl,m,T.env,T)

compApplication(op,argl,m,e,T) ==
  T.mode is ['Mapping, retm, :argml] =>
    #argl ~= #argml => nil
    retm := resolve(m, retm)
    retm = $Category or isCategoryForm(retm) => nil  -- not handled
    argTl := [[.,.,e] := comp(x,m,e) or return "failed"
              for x in argl for m in argml]
    argTl = "failed" => nil
    form:=
      not (member(op,$formalArgList) or member(T.expr,$formalArgList)) and ATOM T.expr =>
        nprefix := $prefix or
        -- following needed for referencing local funs at capsule level
           getAbbreviation($op,#rest $form)
        [op',:[a.expr for a in argTl],"$"] where
          op':= INTERN STRCONC(encodeItem nprefix,";",encodeItem T.expr)
      ['call, ['applyFun, T.expr], :[a.expr for a in argTl]]
    coerce([form, retm, e],resolve(retm,m))
  -- If op is not 'elt' and not a mapping check if we can
  -- use appropriate 'elt'
  op = 'elt => nil
  eltForm := ['elt, op, :argl]
  comp(eltForm, m, e)

compFormWithModemap(form is [op,:argl],m,e,modemap) ==
  [map:= [.,target,:.],[pred,impl]]:= modemap
  if isCategoryForm(target) and isFunctor op then
    [modemap,e]:= substituteIntoFunctorModemap(argl,modemap,e) or return nil
    [map:= [.,target,:.],:cexpr]:= modemap
  sv:=listOfSharpVars map
  if sv then
     -- SAY [ "compiling ", op, " in compFormWithModemap,
     -- mode= ",map," sharp vars=",sv]
    for x in argl for ss in $FormalMapVariableList repeat
      if ss in sv then
        [map:= [.,target,:.],:cexpr]:= modemap :=SUBST(x,ss,modemap)
        -- SAY ["new map is",map]
  not (target':= coerceable(target,m,e)) => nil
  [f,Tl,sl]:= compApplyModemap(form,modemap,e,nil) or return nil

  --generate code; return
  T:=
    [x',m',e'] where
      m':= SUBLIS(sl,map.(1))
      x':=
        form':= [f,:[t.expr for t in Tl]]
        (m' = $Category or isCategoryForm(m')) and ATOM(f) => form'
        -- try to deal with new-style Unions where we know the conditions
        op = "elt" and f is ['XLAM,:.] and IDENTP(z := first argl) and
          (c:=get(z,'condition,e)) and
            c is [["case", =z, c1]] and
              (c1 is ['_:,=(CADR argl),=m] or EQ(c1,CADR argl) ) =>
-- first is a full tag, as placed by getInverseEnvironment
-- second is what getSuccessEnvironment will place there
                ["CDR",z]
        ["call",:form']
      e':=
        Tl => (last Tl).env
        e
  convert(T,m)

applyMapping([op,:argl],m,e,ml) ==
  #argl~=#ml-1 => nil
  isCategoryForm(first ml) =>
                                --is op a functor?
    pairlis:= [[v,:a] for a in argl for v in $FormalMapVariableList]
    ml' := SUBLIS(pairlis, ml)
    argl':=
      [T.expr for x in argl for m' in rest ml'] where
        T() == [.,.,e]:= comp(x,m',e) or return "failed"
    if argl'="failed" then return nil
    form:= [op,:argl']
    convert([form,first ml',e],m)
  argl':=
    [T.expr for x in argl for m' in rest ml] where
      T() == [.,.,e]:= comp(x,m',e) or return "failed"
  if argl'="failed" then return nil
  form:=
    not member(op,$formalArgList) and ATOM op and not get(op,'value,e) =>
      nprefix := $prefix or
   -- following needed for referencing local funs at capsule level
        getAbbreviation($op,#rest $form)
      [op',:argl',"$"] where
        op':= INTERN STRCONC(encodeItem nprefix,";",encodeItem op)
    ['call,['applyFun,op],:argl']
  pairlis:= [[v,:a] for a in argl' for v in $FormalMapVariableList]
  convert([form,SUBLIS(pairlis,first ml),e],m)

--% APPLY MODEMAPS

compApplyModemap(form,modemap,$e,sl) ==
  $generatingCall : local := true
  [op,:argl] := form                   --form to be compiled
  [[mc,mr,:margl],:fnsel] := modemap   --modemap we are testing

  -- $e     is the current environment
  -- sl     substitution list, nil means bottom-up, otherwise top-down

  -- 0.  fail immediately if #argl=#margl

  if #argl~=#margl then return nil

  -- 1.  use modemap to evaluate arguments, returning failed if
  --     not possible

  lt:=
    [[.,m',$e]:=
      comp(y,g,$e) or return "failed" where
        g:= SUBLIS(sl,m) where
            sl:= pmatchWithSl(m',m,sl) for y in argl for m in margl]
  lt="failed" => return nil

  -- 2.  coerce each argument to final domain, returning failed
  --     if not possible

  lt':= [coerce(y,d) or return "failed"
         for y in lt for d in SUBLIS(sl,margl)]
  lt'="failed" => return nil

  -- 3.  obtain domain-specific function, if possible, and return

  --$bindings is bound by compMapCond
  [f, bindings] := compMapCond(op, mc, sl, fnsel) or return nil

--+ can no longer trust what the modemap says for a reference into
--+ an exterior domain (it is calculating the displacement based on view
--+ information which is no longer valid; thus ignore this index and
--+ store the signature instead.

--$NRTflag=true and f is [op1,d,.] and NE(d,'$) and member(op1,'(ELT CONST)) =>
  f is [op1,d,.] and member(op1,'(ELT CONST)) =>
      [genDeltaEntry([op, :modemap]), lt', bindings]
  [f, lt', bindings]

compMapCond(op, mc, bindings, fnsel) ==
  or/[compMapCond'(u, op, mc, bindings) for u in fnsel]

compMapCond'([cexpr,fnexpr],op,dc,bindings) ==
  compMapCond''(cexpr,dc) => compMapCondFun(fnexpr,op,dc,bindings)
  stackMessage ["not known that",'%b,dc,'%d,"has",'%b,cexpr,'%d]

compMapCond''(cexpr,dc) ==
  cexpr=true => true
  cexpr is ["AND", :l] or cexpr is ["and", :l] =>
      and/[compMapCond''(u, dc) for u in l]
  cexpr is ["OR", :l] or cexpr is ["or", :l] =>
      or/[compMapCond''(u, dc) for u in l]
  cexpr is ["not",u] => not compMapCond''(u,dc)
  cexpr is ["has",name,cat] => (knownInfo cexpr => true; false)
        --for the time being we'll stop here - shouldn't happen so far
        --$disregardConditionIfTrue => true
        --stackSemanticError(("not known that",'%b,name,
        -- '%d,"has",'%b,cat,'%d),nil)
  BREAK()

compMapCondFun(fnexpr,op,dc,bindings) == [fnexpr,bindings]
