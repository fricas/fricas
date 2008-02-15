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

-- Compute list of parameters which occur in singntures on the
-- sigList, remowing duplicates, and skipping "known"
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
--   domainOrPackage      - "domain" or "package" (marks kind of category
--                           object)
--   sigList              - list of all signatures
--   attList              - list of all attributes
--   PrincipalAncestor    - principal ancestor (if any) 
mkCategory(domainOrPackage,sigList,attList,domList,PrincipalAncestor) ==
  NSigList:= nil
  if PrincipalAncestor=nil then count:= 6 else count:= SIZE PrincipalAncestor
  sigList:=
    [if s is [sig,pred]
       then
         or/[x is [[ =sig,.,:impl],:num] for x in NSigList] => [sig,pred,:impl]
                 --only needed for multiple copies of sig
         num:= if domainOrPackage="domain" then count else count-5
         nsig:= mkOperatorEntry("domain",sig,pred,num)
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
  v:= GETREFV count
  v.(0):= nil
  v.(1):= sigList
  v.2:= attList
  v.3:= ["Category"]
  if not PrincipalAncestor=nil
     then
      for x in 6..SIZE PrincipalAncestor-1 repeat v.x:= PrincipalAncestor.x
      v.4:= [first PrincipalAncestor.4,CADR PrincipalAncestor.4,OldLocals]
   else v.4:= [nil,nil,OldLocals] --associated categories and domains
  v.5:= domList
  for [nsig,:sequence] in NSigList repeat v.sequence:= nsig
  v

isCategory a == REFVECP a and #a>5 and a.3=["Category"]
 
--% Subsumption code (for operators)
 
DropImplementations (a is [sig,pred,:implem]) ==
  if implem is [[q,:.]] and (q="ELT" or q="CONST")
     then if (q="ELT")  then [sig,pred]
                        else [[:sig,:'(constant)],pred]
     else a
 
SigListUnion(extra,original) ==
  --augments original %with everything in extra that is not in original
  for (o:=[[ofn,osig,:.],opred,:.]) in original repeat
    -- The purpose of this loop is to detect cases when the
    -- original list contains, e.g. ** with NonNegativeIntegers, and
    -- the extra list would like to add ** with PositiveIntegers.
    -- The PI map is therefore gives an implementation of "Subsumed"
    for x in SigListOpSubsume(o,extra) repeat
      [[xfn,xsig,:.],xpred,:.]:=x
      xfn=ofn and xsig=osig =>
              --checking name and signature, but not a 'constant' marker
        xpred=opred => extra:= delete(x,extra)
             --same signature and same predicate
        opred = true => extra:= delete(x,extra)
   -- PRETTYPRINT ("we ought to subsume",x,o)
      not MachineLevelSubsume(QCAR o,QCAR x) =>
         '"Source level subsumption not implemented"
      extra:= delete(x,extra)
  for e in extra repeat
    [esig,epred,:.]:= e
    eimplem:=[]
    for x in SigListOpSubsume(e,original) repeat
        --PRETTYPRINT(LIST("SigListOpSubsume",e,x))
      not MachineLevelSubsume(QCAR e,QCAR x) =>
        --systemError '"Source level subsumption not implemented"
        original:= [e,:original]
        return() -- this exits from the innermost for loop
      original:= delete(x,original)
      [xsig,xpred,:ximplem]:= x
--      if xsig ^= esig then   -- not quite strong enough
      if CAR xsig ^= CAR esig or CADR xsig ^= CADR esig then
-- the new version won't get confused by "constant"markers
         if ximplem is [["Subsumed",:.],:.] then
            original := [x,:original]
          else
            original:= [[xsig,xpred,["Subsumed",:esig]],:original]
       else epred:=mkOr(epred,xpred)
-- this used always to be done, as noted below, but that's not safe
      if not(ximplem is [["Subsumed",:.],:.]) then eimplem:= ximplem
      if eimplem then esig:=[CAR esig,CADR esig] 
           -- in case there's a constant marker
      e:= [esig,epred,:eimplem]
--    e:= [esig,mkOr(xpred,epred),:ximplem]
-- Original version -gets it wrong if the new operator is only
-- present under certain conditions
        -- We must pick up the previous implementation, if any
--+
      if ximplem is [[q,.,index]] and INTEGERP index and (q="ELT" or q="CONST")
        then $NewCatVec. index:= e
    original:= [e,:original]
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
  LENGTH l = 1 => CAR l
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
  LENGTH l = 1 => CAR l
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
 
SigListMember(m,list) ==
  list=nil => false
  SigEqual(m,first list) => true
  SigListMember(m,rest list)
 
SigEqual([sig1,pred1,:.],[sig2,pred2,:.]) ==
  -- Notice asymmetry: checks that arg1 is a consequence of arg2
  sig1=sig2 and PredImplies(pred2,pred1)
 
PredImplies(a,b) ==
    --true if a => b in the sense of logical implication
--a = "true" => true
  a=true => true
  a=b => true
  false         -- added by RDJ: 12/21/82
--error()       -- for the time being
 
SigListOpSubsume([[name1,sig1,:.],:.],list) ==
  --does m subsume another operator in the list?
        --see "operator subsumption" in SYSTEM SCRIPT
        --if it does, returns the subsumed member
  lsig1:=LENGTH sig1
  ans:=[]
  for (n:=[[name2,sig2,:.],:.]) in list repeat
    name1=name2 and EQ(lsig1,LENGTH sig2) and SourceLevelSubsume(sig1,sig2) =>
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
  $noSubsumption=true => false
  b is ["Union",:blist] and member(a,blist) => true
  BOUNDP '$noSubsets and $noSubsets => false
  atom b and assoc(a,GETL(b,"Subsets")) => true
  a is [a1] and b is [b1] and assoc(a1,GETL(b1,"Subsets")) => true
  nil
 
MachineLevelSubsume([name1,[out1,:in1],:flag1],[name2,[out2,:in2],:flag2]) ==
  -- Checks for machine-level subsumption in the sense of SYSTEM SCRIPT
  --  true if the first signature subsumes the second
  --  flag1 = flag2 and: this really should be checked, but
  name1=name2 and MachineLevelSubset(out1,out2) and
    (and/[MachineLevelSubset(inarg2,inarg1) for inarg1 in in1 for inarg2 in in2]
      )
 
MachineLevelSubset(a,b) ==
  --true if a is a machine-level subset of b
  a=b => true
  b is ["Union",:blist] and member(a,blist) and
    (and/[STRINGP x for x in blist | x^=a]) => true
           --all other branches must be distinct objects
  atom b and assoc(a,GETL(b,"Subsets")) => true
  a is [a1] and b is [b1] and assoc(a1,GETL(b1,"Subsets")) => true
             --we assume all subsets are true at the machine level
  nil
 
--% Ancestor chasing code
 
FindFundAncs l ==
  --l is a list of categories and associated conditions (a list of 2-lists
  --returns a list of them and all their fundamental ancestors
  --also as two-lists with the appropriate conditions
  l=nil => nil
  f1:= CatEval CAAR l
  f1.(0)=nil => FindFundAncs rest l
  ans:= FindFundAncs rest l
  for u in FindFundAncs [[CatEval first x,mkAnd(CADAR l,CADR x)]
   for x in CADR f1.4] repeat
    x:= ASSQ(first u,ans) =>
      ans:= [[first u,mkOr(CADR x,CADR u)],:delete(x,ans)]
    ans:= [u,:ans]
        --testing to see if CAR l is already there
  x:= ASSQ(CAAR l,ans) => [[CAAR l,mkOr(CADAR l,CADR x)],:delete(x,ans)]
  CADAR l=true =>
    for x in first f1.4 repeat if y:= ASSQ(CatEval x,ans) then ans:= delete(y,ans)
    [first l,:ans]
  for x in first f1.4 repeat
    if y:= ASSQ(CatEval x,ans) then ans:=
      [[first y,mkOr(CADAR l,CADR y)],:delete(y,ans)]
  [first l,:ans]
  -- Our new thing may have, as an alternate view, a principal
  -- descendant of something previously added which is therefore
  -- subsumed
 
CatEval x ==
  REFVECP x => x
  $InteractiveMode => CAR compMakeCategoryObject(x,$CategoryFrame)
  CAR compMakeCategoryObject(x,$e)
 
--RemovePrinAncs(l,leaves) ==
--  l=nil => nil
--  leaves:= [first y for y in leaves]
--               --remove the slot pointers
--  [x for x in l | not AncestorP(x.(0),leaves)]
 
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
      PredImplies(ucond,condition) => return u'
 
DescendantP(a,b) ==
  -- checks to see if a is any kind of Descendant of b
  a=b => true
  a is ["ATTRIBUTE",:.] => nil
  a is ["SIGNATURE",:.] => nil
  a:= CatEval a
  b is ["ATTRIBUTE",b'] =>
    (l := assoc(b',a.2)) => TruthP CADR l
  member(b,first a.4) => true
  AncestorP(b,[first u for u in CADR a.4]) => true
  nil
 
--% The implementation of Join
 
JoinInner(l,$e) ==
  $NewCatVec: local := nil
  CondList:= nil
  for u in l repeat
    for at in u.2 repeat
      at2:= first at
      if atom at2 then at2:=[at2]
        -- the variable $Attributes is built globally, so that true
        -- attributes can be detected without calling isCategoryForm
      QMEMQ(QCAR at2,$Attributes) => nil
      null isCategoryForm(at2,$e) =>
        $Attributes:=[QCAR at2,:$Attributes]
        nil
      pred:= first rest at
        -- The predicate under which this category is conditional
      member(pred,get("$Information","special",$e)) => l:= [:l,CatEval at2]
          --It's true, so we add this as unconditional
      not (pred is ["and",:.]) => CondList:= [[CatEval at2,pred],:CondList]
      pred':=
        [u
          for u in rest pred | not member(u,get("$Information","special",$e))
            and not (u=true)]
      null pred' => l:= [:l,CatEval at2]
      LENGTH pred'=1 => CondList:= [[CatEval at2,pred'],:CondList]
      CondList:= [[CatEval at2,["and",:pred']],:CondList]
  [$NewCatVec,:l]:= l
  l':= [:CondList,:[[u,true] for u in l]]
    -- This is a list of all the categories that this extends
    -- conditionally or unconditionally
  sigl:= $NewCatVec.(1)
  attl:= $NewCatVec.2
  globalDomains:= $NewCatVec.5
  FundamentalAncestors:= CADR $NewCatVec.4
  if $NewCatVec.(0) then FundamentalAncestors:=
    [[$NewCatVec.(0)],:FundamentalAncestors]
                    --principal ancestor . all those already included
  copied:= nil
  originalVector:= true
  -- we can not decide to extend the vector in multiple ways
  -- this flag helps us detect this case
  originalVector := false
    -- this skips buggy code which discards needed categories
  for [b,condition] in FindFundAncs l' repeat
      --This loop implements Category Subsumption
          --as described in SYSTEM SCRIPT
    if not (b.(0)=nil) then
                   --It's a named category
      bname:= b.(0)
      CondAncestorP(bname,FundamentalAncestors,condition) => nil
      (f:=AncestorP(bname,[first u for u in FundamentalAncestors])) =>
        [.,.,index] := assoc(f,FundamentalAncestors)
        FundamentalAncestors:=[[bname,condition,index],:FundamentalAncestors]
      PrinAncb:= first (CatEval bname).(4)
               --Principal Ancestors of b
      reallynew:= true
      for anc in FundamentalAncestors repeat
        if member(first anc,PrinAncb) then
                  --This is the check for "Category Subsumption"
          if rest anc
             then (anccond:= CADR anc; ancindex:= CADDR anc)
             else (anccond:= true; ancindex:= nil)
          if PredImplies(condition,anccond)
             then FundamentalAncestors:=
 
               -- the new 'b' is more often true than the old one 'anc'
              [[bname,condition,ancindex],:delete(anc,FundamentalAncestors)]
           else
            if ancindex and (PredImplies(anccond,condition); true)
-- I have no idea who effectively commented out the predImplies
-- JHD 25/8/86
               then
                     --the new 'b' is less often true
                newentry:=[bname,condition,ancindex]
                if not member(newentry,FundamentalAncestors) then
                  FundamentalAncestors:= [newentry,:FundamentalAncestors]
             else ancindex:= nil
          if not copied then
            $NewCatVec:= COPY_-SEQ $NewCatVec
            copied:= true
          if ancindex
             then ($NewCatVec.ancindex:= bname; reallynew:= nil)
             else
              if originalVector and (condition=true) then
                $NewCatVec:= CatEval bname
                copied:= nil
                FundamentalAncestors:= [[bname],:CADR $NewCatVec.4]
                         --bname is Principal, so comes first
                reallynew:= nil
                MEMQ(b,l) =>
                       --MEMQ since category vectors are guaranteed unique
                  (sigl:= $NewCatVec.(1); attl:= $NewCatVec.2; l:= delete(b,l))
             --     SAY("domain ",bname," subsumes")
             --     SAY("adding a conditional domain ",
             --         bname,
             --         " replacing",
             --         CAR anc)
                bCond:= ASSQ(b,CondList)
                CondList:= delete(bCond,CondList)
             -- value of bCond not used and could be NIL
             -- bCond:= CADR bCond
                globalDomains:= $NewCatVec.5
                for u in $NewCatVec.(1) repeat
                  if not member(u,sigl) then
                    [s,c,i]:= u
                    if c=true
                       then sigl:= [[s,condition,i],:sigl]
                       else sigl:= [[s,["and",condition,c],i],:sigl]
                for u in $NewCatVec.2 repeat
                  if not member(u,attl) then
                    [a,c]:= u
                    if c=true
                       then attl:= [[a,condition],:attl]
                       else attl:= [[a,["and",condition,c]],:attl]
      if reallynew then
        n:= SIZE $NewCatVec
        FundamentalAncestors:= [[b.(0),condition,n],:FundamentalAncestors]
        $NewCatVec:= LENGTHENVEC($NewCatVec,n+1)
-- We need to copy the vector otherwise the FundamentalAncestors
-- list will get stepped on while compiling "If R has ... " code
-- Camm Maguire July 26, 2003
--        copied:= true
        copied:= false
        originalvector:= false
        $NewCatVec.n:= b.(0)
  if not copied then $NewCatVec:= COPY_-SEQ $NewCatVec
    -- It is important to copy the vector now,
    -- in case SigListUnion alters it while
    -- performing Operator Subsumption
  for b in l repeat
    sigl:= SigListUnion([DropImplementations u for u in b.(1)],sigl)
    attl:=
-- next two lines are merely performance improvements
      MEMQ(attl,b.2) => b.2
      MEMQ(b.2,attl) => attl
      S_+(b.2,attl)
    globalDomains:= [:globalDomains,:S_-(b.5,globalDomains)]
  for b in CondList repeat
    newpred:= first rest b
    for u in (first b).2 repeat
      v:= assoc(first u,attl)
      null v =>
        attl:=
          CADR u=true => [[first u,newpred],:attl]
          [[first u,["and",newpred,CADR u]],:attl]
      CADR v=true => nil
      attl:= delete(v,attl)
      attl:=
        CADR u=true => [[first u,mkOr(CADR v,newpred)],:attl]
        [[first u,mkOr(CADR v,mkAnd(newpred,CADR u))],:attl]
    sigl:=
      SigListUnion(
        [AddPredicate(DropImplementations u,newpred) for u in (first b).(1)],sigl) where
          AddPredicate(op is [sig,oldpred,:implem],newpred) ==
            newpred=true => op
            oldpred=true => [sig,newpred,:implem]
            [sig,mkpf([oldpred,newpred],"and"),:implem]
  FundamentalAncestors:= [x for x in FundamentalAncestors | rest x]
               --strip out the pointer to Principal Ancestor
  c:= first $NewCatVec.4
  pName:= $NewCatVec.(0)
  if pName and not member(pName,c) then c:= [pName,:c]
  $NewCatVec.4:= [c,FundamentalAncestors,CADDR $NewCatVec.4]
  mkCategory("domain",sigl,attl,globalDomains,$NewCatVec)
 
isCategoryForm(x,e) ==
  x is [name,:.] => categoryForm? name
  atom x => u:= get(x,"macro",e) => isCategoryForm(u,e)
 
