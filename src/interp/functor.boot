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

--%  Utilities

mkDevaluate a ==
  null a => nil
  a is ['QUOTE,a'] => (a' => a; nil)
  a='$ => MKQ '$
  a is ['LIST] => nil
  a is ['LIST,:.] => a
  ['devaluate,a]

compCategories u ==
  ATOM u => u
  not ATOM first u =>
    error ['"compCategories: need an atom in operator position", first u]
  first u = "Record" =>
    -- There is no modemap property for these guys so do it by hand.
    [first u, :[[":", a.1, compCategories1(a.2,'(SetCategory))] for a in rest u]]
  first u = "Union" or first u = "Mapping" =>
    -- There is no modemap property for these guys so do it by hand.
    [first u, :[compCategories1(a,'(SetCategory)) for a in rest u]]
  u is ['SubDomain,D,.] => compCategories D
  v:=get(first u,'modemap,$e)
  ATOM v =>
    error ['"compCategories: could not get proper modemap for operator",first u]
  rest v =>
    error ['"compCategories: unexpected stuff at end of modemap",
           rest v]
  v:= CDDAAR v
  v:=resolvePatternVars(v, rest u) -- replaces #n forms
  -- select the modemap part of the first entry, and skip result etc.
  u:=[first u,:[compCategories1(a,b) for a in rest u for b in v]]
  u

compCategories1(u,v) ==
-- v is the mode of u
  ATOM u => u
  isCategoryForm(v,$e) => compCategories u
  [c,:.] := comp(macroExpand(u,$e),v,$e) => c
  error 'compCategories1

optFunctorBody x ==
  atom x => x
  x is ['QUOTE,:l] => x
  x is ['DomainSubstitutionMacro,parms,body] =>
    optFunctorBody DomainSubstitutionFunction(parms,body)
  x is ['LIST,:l] =>
    null l => nil
    l:= [optFunctorBody u for u in l]
    and/[optFunctorBodyQuotable u for u in l] =>
      ['QUOTE,[optFunctorBodyRequote u for u in l]]
    l=rest x => x --CONS-saving hack
    ['LIST,:l]
  x is ['PROGN,:l] => ['PROGN,:optFunctorPROGN l]
  x is ['COND,:l] =>
--+
    l:=
      [CondClause u for u in l | u and first u] where
        CondClause [pred,:conseq] ==
          [optFunctorBody pred,:optFunctorPROGN conseq]
    l:= EFFACE('((QUOTE T)),l)
                   --delete any trailing ("T)
    null l => nil
    CAAR l='(QUOTE T) =>
      (null CDAR l => nil; null CDDAR l => CADAR l; ["PROGN",:CDAR l])
    null rest l and null CDAR l =>
            --there is no meat to this COND
      pred:= CAAR l
      atom pred => nil
      first pred="HasCategory" => nil
      ['COND,:l]
    ['COND,:l]
  [optFunctorBody u for u in x]

optFunctorBodyQuotable u ==
  null u => true
  NUMBERP u => true
  atom u => nil
  u is ['QUOTE,:.] => true
  nil

optFunctorBodyRequote u ==
  atom u => u
  u is ['QUOTE,v] => v
  systemErrorHere '"optFunctorBodyRequote"

optFunctorPROGN l ==
  l is [x,:l'] =>
    worthlessCode x => optFunctorPROGN l'
    l':= optFunctorBody l'
    l'=[nil] => [optFunctorBody x]
    [optFunctorBody x,:l']
  l

worthlessCode x ==
  x is ['COND,:l] and (and/[x is [.,y] and worthlessCode y for x in l]) => true
  x is ['PROGN,:l] => (null (l':= optFunctorPROGN l) => true; false)
  x is ['LIST] => true
  null x => true
  false

cons5(p,l) ==
  l and (CAAR l = first p) => [p,: rest l]
  LENGTH l < 5 => [p,:l]
  RPLACD(QCDDR(QCDDR l), nil)
  [p,:l]

--presence of GENSYM in arg-list differentiates mutable-domains
-- addMutableArg nameFormer ==
--   $mutableDomain =>
--     nameFormer is ['LIST,:.] => [:nameFormer, '(GENSYM)]
--     ['APPEND,nameFormer,'(LIST (GENSYM))]
--   nameFormer

mkDomainConstructor x ==
  atom x => mkDevaluate x
  x is ['Join] => nil
  x is ['LIST] => nil
  x is ['CATEGORY,:.] => MKQ x
  x is ['mkCategory,:.] => MKQ x
  x is ['_:,selector,dom] =>
    ['LIST,MKQ '_:,MKQ selector,mkDomainConstructor dom]
  x is ['Record,:argl] =>
    ['LIST,MKQ 'Record,:[mkDomainConstructor y for y in argl]]
  x is ['Join,:argl] =>
    ['LIST,MKQ 'Join,:[mkDomainConstructor y for y in argl]]
  x is ['call,:argl] => ['MKQ, optCall x]
        --The previous line added JHD/BMT 20/3/84
        --Necessary for proper compilation of DPOLY SPAD
  x is [op] => MKQ x
  x is [op,:argl] => ['LIST,MKQ op,:[mkDomainConstructor a for a in argl]]


DescendCodeAdd(base,flag) ==
  atom base => DescendCodeVarAdd(base,flag)
  not (modemap:=get(opOf base,'modemap,$CategoryFrame)) =>
      if getmode(opOf base,$e) is ["Mapping",target,:formalArgModes]
         then formalArgs:= take(#formalArgModes,$FormalMapVariableList)
                --argument substitution if parameterized?

         else keyedSystemError("S2OR0001",[opOf base])
      DescendCodeAdd1(base,flag,target,formalArgs,formalArgModes)
  for [[[.,:formalArgs],target,:formalArgModes],.] in modemap repeat
    (ans:= DescendCodeAdd1(base,flag,target,formalArgs,formalArgModes))=>
      return ans
  ans

DescendCodeAdd1(base,flag,target,formalArgs,formalArgModes) ==
  slist:= pairList(formalArgs,rest $addFormLhs)
         --base = comp $addFormLhs-- bound in compAdd
  e:= $e
  newModes:= SUBLIS(slist,formalArgModes)
  or/[not comp(u,m,e) for u in rest $addFormLhs for m in newModes] =>
    return nil
      --I should check that the actual arguments are of the right type
  for u in formalArgs for m in newModes repeat
    [.,.,e]:= compMakeDeclaration(['_:,u,m],m,e)
      --we can not substitute in the formal arguments before we comp
      --for that may change the shape of the object, but we must before
      --we match signatures
  cat:= (compMakeCategoryObject(target,e)).expr
  instantiatedBase:= GENVAR()
  n:=MAXINDEX cat
  code:=
    [u
      for i in 6..n | not atom cat.i and not atom (sig:= first cat.i)
         and
          (u:=
            SetFunctionSlots(SUBLIS(slist,sig),['ELT,instantiatedBase,i],flag,
              'adding))~=nil]
     --The code from here to the end is designed to replace repeated LOAD/STORE
     --combinations (SETELT ...(ELT ..)) by MVCs where this is practicable
  copyvec:=GETREFV (1+n)
  for u in code repeat
      if update(u,copyvec,[]) then code:=delete(u,code)
    where update(code,copyvec,sofar) ==
      ATOM code =>nil
      MEMQ(QCAR code,'(ELT QREFELT)) =>
          copyvec.(CADDR code):=union(copyvec.(CADDR code), sofar)
          true
      code is [x,name,number,u'] and MEMQ(x,'(SETELT QSETREFV)) =>
        update(u',copyvec,[[name,:number],:sofar])
  for i in 6..n repeat
    if copyvec.i then
      v:=[($QuickCode => 'QREFELT;'ELT),instantiatedBase,i]
      for u in copyvec.i repeat
        [name,:count]:=u
        v:=[($QuickCode => 'QSETREFV;'SETELT),name,count,v]
      code:=[v,:code]
  [['LET,instantiatedBase,base],:code]

DescendCode(code,flag,viewAssoc,EnvToPass) ==
  -- flag = true if we are walking down code always executed;
  -- otherwise set to conditions in which
  code=nil => nil
  code='noBranch => nil
  code is ['add,base,:codelist] =>
    codelist:=
      [v for u in codelist | (v:= DescendCode(u,flag,viewAssoc,EnvToPass))~=nil]
                  -- must do this first, to get this overriding Add code
    ['PROGN,:DescendCodeAdd(base,flag),:codelist]
  code is ['PROGN,:codelist] =>
    ['PROGN,:
            --Two REVERSEs leave original order, but ensure last guy wins
      NREVERSE [v for u in REVERSE codelist |
                    (v:= DescendCode(u,flag,viewAssoc,EnvToPass))~=nil]]
  code is ['COND,:condlist] =>
    c := [[u2 := ProcessCond(first u), :q] for u in condlist] where q ==
          null u2 => nil
          f:=
            TruthP u2 => flag;
            TruthP flag =>
               flag := ['NOT,u2]
               u2
            f1 := ['AND, flag, u2]
            flag := ['AND,flag,['NOT,u2]];
            f1
          [DescendCode(v, f,
            if first u is ['HasCategory,dom,cat]
              then [[dom,:cat],:viewAssoc]
              else viewAssoc,EnvToPass) for v in rest u]
    TruthP CAAR c => ['PROGN,:CDAR c]
    while (c and (last c is [c1] or last c is [c1,[]]) and
            (c1 = '(QUOTE T))) repeat
                   --strip out some worthless junk at the end
        c := NREVERSE rest NREVERSE c
    null c => '(LIST)
    ['COND,:c]
  code is ['LET,name,body,:.] =>
                    --only keep the names that are useful
    u:=member(name,$locals) =>
        CONTAINED('$,body) and isDomainForm(body,$e) =>
          --instantiate domains which depend on $ after constants are set
          code:=[($QuickCode => 'QSETREFV; 'SETELT),[($QuickCode => 'QREFELT; 'ELT),'$,5],#$locals-#u,code]
          $epilogue:=
            TruthP flag => [code,:$epilogue]
            [['COND, [ProcessCond(flag), code]], :$epilogue]
          nil
        code
    code -- doItIf deletes entries from $locals so can't optimize this
  code is ['CodeDefine,sig,implem] =>
             --Generated by doIt in COMPILER BOOT
    dom:= EnvToPass
    dom:=
      u := LASSOC(dom, viewAssoc) => ["getDomainView", dom, u]
      dom
    body:= ['CONS,implem,dom]
    u:= SetFunctionSlots(sig,body,flag,'original)
    ConstantCreator u =>
      if not (flag=true) then u := ['COND, [ProcessCond(flag), u]]
      $ConstantAssignments:= [u,:$ConstantAssignments]
      nil
    u
  code is ['_:,:.] => (RPLACA(code,'LIST); RPLACD(code,NIL))
      --Yes, I know that's a hack, but how else do you kill a line?
  code is ['LIST,:.] => nil
  code is ['devaluate,:.] => nil
  code is ['MDEF,:.] => nil
  code is ['call,:.] => code
  code is ['SETELT,:.] => code -- can be generated by doItIf
  code is ['QSETREFV,:.] => code -- can be generated by doItIf
  stackWarning ['"unknown Functor code ",code]
  code

ConstantCreator u ==
  null u => nil
  u is [q,.,.,u'] and (q='SETELT or q='QSETREFV) => ConstantCreator u'
  u is ['CONS,:.] => nil
  true

ProcessCond(cond) ==
  ncond := SUBLIS($pairlis,cond)
  INTEGERP POSN1(ncond,$NRTslot1PredicateList) => predicateBitRef ncond
  cond

SetFunctionSlots(sig,body,flag,mode) == --mode is either "original" or "adding"
--+
  catNames := ['$]
  for u in $catvecList for v in catNames repeat
    null body => return NIL
    for catImplem in LookUpSigSlots(sig,u.1) repeat
      if catImplem is [q,.,index] and (q='ELT or q='CONST) then
          if q is 'CONST and body is ['CONS,a,b] then
             body := ['CONS,'IDENTITY,['FUNCALL,a,b]]
          body:= [($QuickCode => 'QSETREFV; 'SETELT),v,index,body]
          if REFVECP $SetFunctions and TruthP flag then u.index:= true
                 --used by CheckVector to determine which ops are missing
          if v='$ then  -- i.e. we are looking at the principal view
            not REFVECP $SetFunctions => nil
                    --packages don't set it
            $MissingFunctionInfo.index:= flag
            TruthP $SetFunctions.index => (body:= nil; return nil)
                     -- the function was already assigned
            $SetFunctions.index:=
              TruthP flag => true
              not $SetFunctions.index=>flag --JHD didn't set $SF on this branch
              ["or",$SetFunctions.index,flag]
      else
          keyedSystemError("S2OR0002",[catImplem])
  body is ['SETELT,:.] => body
  body is ['QSETREFV,:.] => body
  nil

LookUpSigSlots(sig,siglist) ==
--+ must kill any implementations below of the form (ELT $ NIL)
  if $insideCategoryPackageIfTrue then
           sig := substitute('$,CADR($functorForm),sig)
  siglist := $lisplibOperationAlist
  REMDUP [implem for u in siglist | SigSlotsMatch(sig,first u,implem:=CADDR u)
              and IFCAR(IFCDR(IFCDR(implem)))]

SigSlotsMatch(sig,pattern,implem) ==
  sig=pattern => true
  not (LENGTH CADR sig=LENGTH CADR pattern) => nil
                       --CADR sig is the actual signature part
  not (first sig=first pattern) => nil
  pat' :=SUBSTQ($definition,'$,CADR pattern)
  sig' :=SUBSTQ($definition,'$,CADR sig)
  sig'=pat' => true
  --If we don't have this next test, then we'll recurse in SetFunctionSlots
  SourceLevelSubsume(sig',pat') => true
  nil

makeMissingFunctionEntry(alist,i) ==
  tran SUBLIS(alist,$MissingFunctionInfo.i) where
    tran x ==
      x is ["HasCategory",a,["QUOTE",b]] => ['has,a,b]
      x is [op,:l] and op in '(AND OR NOT) => [op,:[tran y for y in l]]
      x

--%  Under what conditions may views exist?

InvestigateConditions catvecListMaker ==
  -- given a principal view and a list of secondary views,
  -- discover under what conditions the secondary view are
  -- always present.
  $Conditions: local := nil
  $principal: local := nil
  [$principal,:secondaries]:= catvecListMaker
      --We are not interested in the principal view
      --The next block allows for the possibility that $principal may
      --have conditional secondary views
--+
  null secondaries => '(T)
      --return for packages which generally have no secondary views
  if $principal is [op,:.] then
    [principal',:.]:=compMakeCategoryObject($principal,$e)
              --Rather like eval, but quotes parameters first
    for u in CADR principal'.4 repeat
      if not TruthP(cond:=CADR u) then
        new := ['CATEGORY, 'domain,
                ['IF, cond, ['ATTRIBUTE, first u], 'noBranch]]
        $principal is ['Join,:l] =>
          not member(new,l) =>
            $principal:=['Join,:l,new]
        $principal:=['Join,$principal,new]
  principal' :=
    pessimise $principal where
      pessimise a ==
        atom a => a
        a is ['SIGNATURE,:.] => a
        a is ['IF,cond,:.] =>
          if not member(cond,$Conditions) then $Conditions:= [cond,:$Conditions]
          nil
        [pessimise first a,:pessimise rest a]
  null $Conditions => [true,:[true for u in secondaries]]
  PrincipalSecondaries:= getViewsConditions principal'
  MinimalPrimary := first first PrincipalSecondaries
  MaximalPrimary:= CAAR $domainShell.4
  necessarySecondaries:= [first u for u in PrincipalSecondaries | rest u=true]
  and/[member(u,necessarySecondaries) for u in secondaries] =>
    [true,:[true for u in secondaries]]
  HackSlot4:=
    MaximalPrimary = nil => nil
    MinimalPrimary=MaximalPrimary => nil
    MaximalPrimaries := [MaximalPrimary, :first (CatEval MaximalPrimary).4]
    MinimalPrimaries := [MinimalPrimary, :first (CatEval MinimalPrimary).4]
    MaximalPrimaries:=S_-(MaximalPrimaries,MinimalPrimaries)
    [[x] for x in MaximalPrimaries]
  ($Conditions:= Conds($principal,nil)) where
    Conds(code,previous) ==
           --each call takes a list of conditions, and returns a list
           --of refinements of that list
      atom code => [previous]
      code is ['DomainSubstitutionMacro,.,b] => Conds(b,previous)
      code is ['IF,a,b,c] => union(Conds(b,[a,:previous]),Conds(c,previous))
      code is ['PROGN,:l] => "union"/[Conds(u,previous) for u in l]
      code is ['CATEGORY,:l] => "union"/[Conds(u,previous) for u in l]
      code is ['Join,:l] => "union"/[Conds(u,previous) for u in l]
      [previous]
  $Conditions:= EFFACE(nil,[EFFACE(nil,u) for u in $Conditions])
  partList:=
    [getViewsConditions partPessimise($principal,cond) for cond in $Conditions]
  masterSecondaries:= secondaries
  for u in partList repeat
    for [v,:.] in u repeat
      if not member(v,secondaries) then secondaries:= [v,:secondaries]
  --PRETTYPRINT $Conditions
  --PRETTYPRINT masterSecondaries
  --PRETTYPRINT secondaries
  (list:= [mkNilT member(u,necessarySecondaries) for u in secondaries]) where
    mkNilT u ==
      u => true
      nil
  for u in $Conditions for newS in partList repeat
    --newS is a list of secondaries and conditions (over and above
    --u) for which they apply
    u:=
      LENGTH u=1 => first u
      ['AND,:u]
    for [v,:.] in newS repeat
      for v' in [v, :first (CatEval v).4] repeat
        if (w:= assoc(v', HackSlot4)) then
          rplac(rest w, if rest w then mkOr(u, rest w) else u)
    (list:= update(list,u,secondaries,newS)) where
      update(list,cond,secondaries,newS) ==
        (list2:=
          [flist(sec,newS,old,cond) for sec in secondaries for old in list]) where
            flist(sec,newS,old,cond) ==
              old=true => old
              for [newS2,:morecond] in newS repeat
                old:=
                  not AncestorP(sec,[newS2]) => old
                  cond2:= mkAnd(cond,morecond)
                  null old => cond2
                  mkOr(cond2,old)
              old
        list2
  list:= ICformat_loop(list, secondaries)
  [true,:[LASSOC(ms,list) for ms in masterSecondaries]]

ICformat_loop(list, secondaries) ==
  $ICformat_hash : local := MAKE_-HASHTABLE 'EQUAL
  [[sec,:ICformat u] for u in list for sec in secondaries]

ORreduce l ==
    for u in l | u is ['AND, :.] or u is ['and, :.] repeat
                                  --check that B causes (and A B) to go
        for v in l | not (v = u) repeat
            if member(v, u) or (and/[member(w, u) for w in v]) then
                l := delete(u, l)
                       --v subsumes u
                           --Note that we are ignoring AND as a component.
                           --Convince yourself that this code still works
    l

ICformat u ==
      atom u => u
      u is ['has,:.] =>
          (res := HGET($ICformat_hash, u)) => res
          res := compHasFormat u
          HPUT($ICformat_hash, u, res)
          res
      u is ['AND,:l] or u is ['and,:l] =>
        l:= REMDUP [ICformat v for [v,:l'] in tails l | not member(v,l')]
             -- we could have duplicates after, even if not before
        LENGTH l=1 => first l
        l1:= first l
        for u in rest l repeat
          l1:=mkAnd(u,l1)
        l1
      u is ['OR,:l] =>
        (l := ORreduce l)
        LENGTH l=1 => ICformat first l
        l:= ORreduce REMDUP [ICformat u for u in l]
                 --causes multiple ANDs to be squashed, etc.
                 -- and duplicates that have been built up by tidying
        (l:= Hasreduce l) where
          Hasreduce l ==
            for u in l | u is ['HasCategory,name,cond] and cond is ['QUOTE,
              cond] repeat
                                  --check that v causes descendants to go
                for v in l | not (v=u) and v is ['HasCategory, =name,['QUOTE,
                  cond2]] repeat if DescendantP(cond,cond2) then l:= delete(u,l)
                       --v subsumes u
            for u in l | u is ['AND,:l'] or u is ['and,:l'] repeat
              for u' in l' | u' is ['HasCategory,name,cond] and cond is ['QUOTE,
                cond] repeat
                                    --check that v causes descendants to go
                  for v in l | v is ['HasCategory, =name,['QUOTE,
                    cond2]] repeat if DescendantP(cond,cond2) then l:= delete(u,l)
                         --v subsumes u
            l
        LENGTH l=1 => first l
        ['OR,:l]
      systemErrorHere '"ICformat"

partPessimise(a,trueconds) ==
  atom a => a
  a is ['SIGNATURE,:.] => a
  a is ['IF,cond,:.] => (member(cond,trueconds) => a; nil)
  [partPessimise(first a,trueconds),:partPessimise(rest a,trueconds)]

getViewsConditions u ==

  --returns a list of all the categories that can be views of this one
  --paired with the condition under which they are such views
  [vec,:.]:= compMakeCategoryObject(u,$e) or
    systemErrorHere '"getViewsConditions"
  views:= [[first u,:CADR u] for u in CADR vec.4]
  null vec.0 =>
--+
    null first(vec.4) => views
    [[CAAR vec.4,:true],:views] --*
  [[vec.0,:true],:views] --*
      --the two lines marked  ensure that the principal view comes first
      --if you don't want it, CDR it off

DescendCodeVarAdd(base,flag) ==
   princview := first $catvecList
   [SetFunctionSlots(sig,SUBST('ELT,'CONST,implem),flag,'adding) repeat
       for i in 6..MAXINDEX princview |
         princview.i is [sig:=[op,types],:.] and
           LASSOC([base,:SUBST(base,'$,types)],get(op,'modemap,$e)) is
                  [[pred,implem]]]

resolvePatternVars(p,args) ==
  p := SUBLISLIS(args, $TriangleVariableList, p)
  SUBLISLIS(args, $FormalMapVariableList, p)
