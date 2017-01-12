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

-- Functions for building categories

Category() == nil --sorry to say, this hack is needed by isCategoryType

CategoryPrint(D,$e) ==
  SAY "--------------------------------------"
  SAY "Name (and arguments) of category:"
  PRETTYPRINT D.(0)
  SAY "operations:"
  PRETTYPRINT D.(1)
  SAY "attributes:"
  PRETTYPRINT D.2
  SAY "This is a sub-category of"
  PRETTYPRINT first D.4
  for u in CADR D.4 repeat
    SAY("This has an alternate view: slot ",rest u," corresponds to ",first u)
  for u in CADDR D.4 repeat
    SAY("This has a local domain: slot ",rest u," corresponds to ",first u)
  for j in 6..MAXINDEX D repeat
    u:= D.j
    null u => SAY "another domain"
    atom first u => SAY("Alternate View corresponding to: ",u)
    PRETTYPRINT u

-- Compute list of parameters which occur in signatures on the
-- sigList, removing duplicates, and skipping "known"
-- constuctors
sigParams(sigList) ==
  result := nil
  myhash := MAKE_-HASHTABLE 'EQUAL
  NewLocals:= nil
  for s in sigList repeat
    (NewLocals := Prepare (CADAR s, NewLocals)) where
      Prepare (u, l) == for v in u repeat l := Prepare2(v, l)
      Prepare2 (v,l) ==
          v is "$" => l
          STRINGP v => l
          atom v => [v,:l]
          MEMQ(first v,$PrimitiveDomainNames) => l
            --This variable is set in INIT LISP
            --It is a list of all the domains that we need not cache
          v is ["Union",:w] =>
            for x in stripUnionTags w repeat l := Prepare2 (x, l)
            l
          v is ["Mapping",:w] =>
            for x in w repeat l := Prepare2 (x, l)
            l
          v is ["List",w] => Prepare2 (w, l)
          v is ["Record",.,:w] =>
            for x in w repeat l := Prepare2 (CADDR x, l)
            l
          [v,:l]
  for s in NewLocals repeat
     if null(HGET(myhash, s)) then
        HPUT(myhash, s, true)
        result := [s,:result]
  result

-- create a new category vector
-- Arguments:
--   sigList              - list of all signatures
--   attList              - list of all conditional ancestors
--   PrincipalAncestor    - principal ancestor (if any)
mkCategory(sigList, attList, domList, PrincipalAncestor) ==
  NSigList := nil
  count := 6
  sigList:=
    [if s is [sig,pred]
       then
         or/[x is [[ =sig,.,:impl],:num] for x in NSigList] => [sig,pred,:impl]
                 --only needed for multiple copies of sig
         nsig:= mkOperatorEntry(sig,pred,count)
         NSigList:= [[nsig,:count],:NSigList]
         count:= count+1
         nsig
     else s for s in sigList]
  NewLocals := sigParams(sigList)
  OldLocals:= nil
  if PrincipalAncestor then for u in (OldLocals:= CADDR PrincipalAncestor.4)
     repeat NewLocals:= delete(first u,NewLocals)
  for u in NewLocals repeat
    (OldLocals:= [[u,:count],:OldLocals]; count:= count+1)
  v:= GETREFV 6
  v.(0):= nil
  v.(1):= sigList
  v.2:= attList
  v.3:= ["Category"]
  if not(PrincipalAncestor = nil) then
      v.4 := [first PrincipalAncestor.4, CADR PrincipalAncestor.4, OldLocals]
   else v.4 := [nil,nil,OldLocals] --associated categories and domains
  v.5:= domList
  v

isCategory a == REFVECP a and #a>5 and a.3=["Category"]

--% Subsumption code (for operators)

DropImplementations (a is [sig,pred,:implem]) ==
  if implem is [[q,:.]] and (q="ELT" or q="CONST")
     then if (q="ELT")  then [sig,pred]
                        else [[:sig,:'(constant)],pred]
     else a

SigListUnion(extra,original) ==
  --augments original with everything in extra that is not in original
  for (o:=[[ofn,osig,:.],opred,:.]) in original repeat
    -- The purpose of this loop is to detect cases when
    -- original list already contains given operation
    for x in SigListOpSubsume(o,extra) repeat
      [[xfn,xsig,:.],xpred,:.]:=x
      xfn=ofn and xsig=osig =>
              --checking name and signature, but not a 'constant' marker
        xpred=opred => extra:= delete(x,extra)
             --same signature and same predicate
        opred = true => extra:= delete(x,extra)
  for e in extra repeat
      [esig, epred, :.] := e
      for x in SigListOpSubsume(e, original) repeat
        --PRETTYPRINT(LIST("SigListOpSubsume",e,x))
          original := delete(x,original)
          [xsig, xpred, :ximplem] := x
          if ximplem then esig := [first esig, CADR esig]
             -- in case there's a constant marker
          e := [esig, mkOr(epred, xpred), :ximplem]
      original := [e, :original]
  original

mkOr(a,b) ==
  a=true => true
  b=true => true
  b=a => a
--PRETTYPRINT ("Condition merging",a,b)
  l:=
    a is ["OR",:a'] =>
      (b is ["OR",:b'] => union(a',b'); mkOr2(b,a') )
    b is ["OR",:b'] => mkOr2(a,b')
    (a is ["has",avar,acat]) and (b is ["has",=avar,bcat]) =>
      DescendantP(acat,bcat) => LIST b
      DescendantP(bcat,acat) => LIST a
      [a,b]
    a is ['AND,:a'] and member(b,a') => LIST b
    b is ['AND,:b'] and member(a,b') => LIST a
    a is ["and",:a'] and member(b,a') => LIST b
    b is ["and",:b'] and member(a,b') => LIST a
    [a,b]
  LENGTH l = 1 => first l
  ["OR",:l]

mkOr2(a,b) ==
  --a is a condition, "b" a list of them
  member(a,b) => b
  a is ["has",avar,acat] =>
    aRedundant:=false
    for c in b | c is ["has",=avar,ccat] repeat
      DescendantP(acat,ccat) =>
        return (aRedundant:=true)
      if DescendantP(ccat,acat) then b := delete(c,b)
    aRedundant => b
    [a,:b]
  [a,:b]

mkAnd(a,b) ==
  a=true => b
  b=true => a
  b=a => a
  --PRETTYPRINT ("Condition merging",a,b)
  l:=
    a is ["AND",:a'] =>
      (b is ["AND",:b'] => union(a',b'); mkAnd2(b,a') )
    b is ["AND",:b'] => mkAnd2(a,b')
    (a is ["has",avar,acat]) and (b is ["has",=avar,bcat]) =>
      DescendantP(acat,bcat) => LIST a
      DescendantP(bcat,acat) => LIST b
      [a,b]
    [a,b]
  LENGTH l = 1 => first l
  ["AND",:l]

mkAnd2(a,b) ==
  --a is a condition, "b" a list of them
  member(a,b) => b
  a is ["has",avar,acat] =>
    aRedundant:=false
    for c in b | c is ["has",=avar,ccat] repeat
      DescendantP(ccat,acat) =>
        return (aRedundant:=true)
      if DescendantP(acat,ccat) then b := delete(c,b)
    aRedundant => b
    [a,:b]
  [a,:b]

PredImplies(a,b) ==
    --true if a => b in the sense of logical implication
  a = false => true
  b = true => true
  a=b => true
  a is ["OR", :al] =>
      res := true
      for a1 in al while res repeat
          res := PredImplies(a1, b)
      res
  b is ["OR", :bl] =>
      for b1 in bl while not(res) repeat
          res := PredImplies(a, b1)
      res
  b is ["AND", :bl] =>
      res := true
      for b1 in bl while res repeat
          res := PredImplies(a, b1)
      res
  a is ["AND", :al] =>
      for a1 in al while not(res) repeat
          res := PredImplies(a1, b)
      res
  false         -- added by RDJ: 12/21/82
--error()       -- for the time being

SigListOpSubsume([[name1,sig1,:.],:.],list) ==
  --does m subsume another operator in the list?
        --see "operator subsumption" in SYSTEM SCRIPT
        --if it does, returns the subsumed member
  lsig1:=LENGTH sig1
  ans:=[]
  for (n:=[[name2,sig2,:.],:.]) in list repeat
    EQ(name1, name2) and EQL(lsig1,LENGTH sig2) and SourceLevelSubsume(sig1,sig2) =>
      ans:=[n,:ans]
  return ans

SourceLevelSubsume([out1,:in1],[out2,:in2]) ==
  -- Checks for source-level subsumption in the sense of SYSTEM SCRIPT
  --   true if the first signature subsumes the second
  SourceLevelSubset(out1,out2) and
    (and/[SourceLevelSubset(inarg2,inarg1) for inarg1 in in1 for inarg2 in in2])

SourceLevelSubset(a,b) ==
  --true if a is a source-level subset of b
  a=b => true
  false

--% Ancestor chasing code

get_cond(x) ==
    rest(x) => CADR x
    true

FindFundAncs l ==
  --l is a list of categories and associated conditions (a list of 2-lists
  --returns a list of them and all their fundamental ancestors
  --also as two-lists with the appropriate conditions
  l=nil => nil
  [l1, cond1] := first l
  f1:= CatEval l1
  ans := FindFundAncs rest l
  -- Does not work with Shoe (garbage items ???)
  --  ll := [[CatEval xf, mkAnd(cond1, xc)] for [xf, xc] in CADR f1.4]
  ll := [[CatEval first x, mkAnd(cond1, get_cond(x))] for x in CADR f1.4]
  for u in ll repeat
        [u1, uc] := u
        x:= ASSQ(u1, ans) =>
            ans:= [[u1, mkOr(CADR x, uc)],:delete(x,ans)]
        ans:= [u,:ans]
  f1.(0) = nil => ans
  --testing to see if l1 is already there
  x := ASSQ(l1, ans) => [[l1, mkOr(cond1, CADR x)],:delete(x,ans)]
  cond1 = true =>
      for x in first f1.4 repeat
            if y:= ASSQ(CatEval x,ans) then ans:= delete(y,ans)
      [first l,:ans]
  for x in first f1.4 repeat
    if y:= ASSQ(CatEval x,ans) then ans:=
      [[first y, mkOr(cond1, CADR y)], :delete(y, ans)]
  [first l,:ans]
  -- Our new thing may have, as an alternate view, a principal
  -- descendant of something previously added which is therefore
  -- subsumed

CatEval x ==
  REFVECP x => x
  $InteractiveMode => first compMakeCategoryObject(x, $CategoryFrame)
  first compMakeCategoryObject(x, $e)

AncestorP(xname,leaves) ==
  -- checks for being a principal ancestor of one of the leaves
  member(xname,leaves) => xname
  for y in leaves repeat
    member(xname,first (CatEval y).4) => return y

CondAncestorP(xname,leaves,condition) ==
  -- checks for being a principal ancestor of one of the leaves
  for u in leaves repeat
    u':=first u
    ucond:=
      null rest u => true
      first rest u
    xname = u' or member(xname,first (CatEval u').4) =>
      PredImplies(condition, ucond) => return u'

DescendantP(a,b) ==
  -- checks to see if a is any kind of Descendant of b
  a=b => true
  a is ["ATTRIBUTE",:.] => BREAK()
  a is ["SIGNATURE",:.] => nil
  a:= CatEval a
  b is ["ATTRIBUTE",b'] => BREAK()
  member(b,first a.4) => true
  AncestorP(b,[first u for u in CADR a.4]) => true
  nil


simplify_cond1(catname, cond) ==
    -- FIXME: this is ugly hack to get around compiler bug.
    -- Namely, sometimes '$' is not what it should be...
    $compForModeIfTrue => cond
    cond is ["has", "$", =catname] => nil
    cond is ["OR", :l] =>
        rl := []
        for c1 in l repeat
            nc := simplify_cond1(catname, c1)
            not(nc) => "iterate"
            rl := cons(nc, rl)
        rl = [] => nil
        #rl = 1 => first(rl)
        ["OR", :rl]
    cond is ["AND", :l] =>
        rl := []
        for c1 in l repeat
            nc := simplify_cond1(catname, c1)
            not(nc) =>
                rl := [nil]
                return nil
            rl := cons(nc, rl)
        rl = [] => true
        #rl = 1 => first(rl)
        ["AND", :rl]
    cond

simplify_cond2(vec, cond) ==
    vec.(0) = nil => cond
    simplify_cond1(vec.(0), cond)

--% The implementation of Join

-- given uncoditinal category vec0 and list of categories
-- with associated conditions l produce fundamental ancestors
-- of the join of vec0 and l
join_fundamental_ancestors(vec0, l) ==
  FundamentalAncestors := [[v, c] for x in CADR vec0.4 | (v := first x;
                              c := simplify_cond2(v, get_cond(x)))]
  if vec0.(0) then FundamentalAncestors:=
    [[vec0.(0)],:FundamentalAncestors]
                    --principal ancestor . all those already included
  -- Copy to avoid corrupting original vector

  for [b, condition] in FindFundAncs l repeat
      --This loop implements Category Subsumption
          --as described in SYSTEM SCRIPT
    if not (b.(0)=nil) then
                   --It's a named category
      bname:= b.(0)
      condition := simplify_cond1(bname, condition)
      not(condition) => "iterate"
      CondAncestorP(bname,FundamentalAncestors,condition) => nil
      if (uu := ASSQ(bname, FundamentalAncestors)) then
          FundamentalAncestors := delete(uu, FundamentalAncestors)
          condition := mkOr(condition, CADR(uu))
      PrinAncb:= first(b.4)
               --Principal Ancestors of b
      for anc in FundamentalAncestors repeat
        if member(first anc,PrinAncb) then
                  --This is the check for "Category Subsumption"
          anccond :=
              rest anc => CADR anc
              true
          if PredImplies(anccond, condition) then
              -- the new 'b' is more often true than the old one 'anc'
              FundamentalAncestors := delete(anc, FundamentalAncestors)
      FundamentalAncestors := [[b.(0), condition], :FundamentalAncestors]
  FundamentalAncestors

JoinInner(l,$e) ==
  NewCatVec := nil
  CondList := nil
  for u in l repeat
    for at in u.2 repeat
      at2:= first at
      if atom at2 then BREAK()
      null isCategoryForm(at2,$e) => BREAK()

      pred:= first rest at
        -- The predicate under which this category is conditional
      -- member(pred,get("$Information","special",$e)) => l:= [:l,CatEval at2]
          --It's true, so we add this as unconditional
      -- not (pred is ["and",:.]) => CondList:= [[CatEval at2,pred],:CondList]
      CondList:= [[CatEval at2,pred],:CondList]
  [NewCatVec, :l] := l
  l':= [:CondList,:[[u,true] for u in l]]
    -- This is a list of all the categories that this extends
    -- conditionally or unconditionally
  sigl := NewCatVec.(1)
  globalDomains := NewCatVec.5
  NewCatVec := COPY_-SEQ NewCatVec
  FundamentalAncestors := join_fundamental_ancestors(NewCatVec, l')

  for b in l repeat
    sigl:= SigListUnion([DropImplementations u for u in b.(1)],sigl)
    globalDomains:= [:globalDomains,:S_-(b.5,globalDomains)]
  for b in CondList repeat
    newpred:= first rest b
    sigl:=
      SigListUnion(
        [AddPredicate(DropImplementations u,newpred) for u in (first b).(1)],sigl) where
          AddPredicate(op is [sig,oldpred,:implem],newpred) ==
            newpred=true => op
            oldpred=true => [sig,newpred,:implem]
            [sig,MKPF([oldpred,newpred],"and"),:implem]
  c := first NewCatVec.4
  pName := NewCatVec.(0)
  if pName and not member(pName,c) then c:= [pName,:c]
  -- strip out the pointer to Principal Ancestor
  if pName then
      FundamentalAncestors :=
          [x for x in FundamentalAncestors | first(x) ~= pName]
  NewCatVec.4 := [c,FundamentalAncestors, CADDR NewCatVec.4]
  mkCategory(sigl, nil, globalDomains, NewCatVec)

Join(:L) ==
  env :=
     not(BOUNDP('$e)) or NULL($e) or $InteractiveMode => $CategoryFrame
     $e
  JoinInner(L, env)

isCategoryForm(x,e) ==
  x is [name,:.] => categoryForm? name
  atom x => u:= get(x,"macro",e) => isCategoryForm(u,e)
