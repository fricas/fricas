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

DEFPARAMETER($getUnexposedOperations, true)

--% Functions for manipulating MODEMAP DATABASE

augLisplibModemapsFromCategory(form is [op,:argl],body,signature) ==
  sl := [["$",:"*1"],:[[a,:p] for a in argl
    for p in rest $PatternVariableList]]
  form:= SUBLIS(sl,form)
  body:= SUBLIS(sl,body)
  signature:= SUBLIS(sl,signature)
  opAlist:= SUBLIS(sl,$domainShell.(1)) or return nil
  nonCategorySigAlist:=
    mkAlistOfExplicitCategoryOps substitute("*1","$",body)
  domainList:=
    [[a,m] for a in rest form for m in rest signature |
      isCategoryForm(m,$EmptyEnvironment)]
  catPredList:= [['ofCategory,:u] for u in [["*1",form],:domainList]]
  for (entry:= [[op,sig,:.],pred,sel]) in opAlist |
    member(sig,LASSOC(op,nonCategorySigAlist)) repeat
      pred':= MKPF([pred,:catPredList],'AND)
      modemap:= [["*1",:sig],[pred',sel]]
      $lisplibModemapAlist:=
        [[op,:interactiveModemapForm modemap],:$lisplibModemapAlist]

augmentLisplibModemapsFromFunctor(form,opAlist,signature) ==
  form:= [formOp,:argl]:= formal2Pattern form
  opAlist:= formal2Pattern opAlist
  signature:= formal2Pattern signature
  for u in form for v in signature repeat
    if MEMQ(u,$PatternVariableList) then
      -- we are going to be EVALing categories containing these
      -- pattern variables
      $e:=put(u,'mode,v,$e)
  nonCategorySigAlist:=
    mkAlistOfExplicitCategoryOps first signature or return nil
  for (entry:= [[op,sig,:.],pred,sel]) in opAlist |
    or/[(sig in catSig) for catSig in
      allLASSOCs(op,nonCategorySigAlist)] repeat
        skip:=
          argl and CONTAINED("$",rest sig) => 'SKIP
          nil
        sel:= substitute(form,"$",sel)
        patternList:= listOfPatternIds sig
        --get relevant predicates
        predList:=
          [[a,m] for a in argl for m in rest signature
            | MEMQ(a,$PatternVariableList)]
        sig:= substitute(form,"$",sig)
        pred':= MKPF([pred,:[mkDatabasePred y for y in predList]],'AND)
        l:=listOfPatternIds predList
        if "OR"/[null MEMQ(u,l) for u in argl] then
          sayMSG ['"cannot handle modemap for",:bright op,
                          '"by pattern match" ]
          skip:= 'SKIP
        modemap:= [[form,:sig],[pred',sel,:skip]]
        $lisplibModemapAlist:= [[op,:interactiveModemapForm modemap],
          :$lisplibModemapAlist]

saveUsersHashTable() ==
  erase_lib(['USERS, 'DATABASE])
  stream:= writeLib('USERS,'DATABASE)
  for k in MSORT HKEYS $usersTb repeat
    rwrite(k, HGET($usersTb, k), stream)
  RSHUT stream

saveDependentsHashTable() ==
  erase_lib(['DEPENDENTS, 'DATABASE])
  stream:= writeLib('DEPENDENTS,'DATABASE)
  for k in MSORT HKEYS $depTb repeat
    rwrite(k, HGET($depTb, k), stream)
  RSHUT stream

getUsersOfConstructor(con) ==
  stream := readLib('USERS, 'DATABASE)
  val := rread_list(con, stream)
  RSHUT stream
  val

getDependentsOfConstructor(con) ==
  stream := readLib('DEPENDENTS, 'DATABASE)
  val := rread_list(con, stream)
  RSHUT stream
  val

orderPredicateItems(pred1,sig,skip) ==
  pred:= signatureTran pred1
  pred is ["AND",:l] => orderPredTran(l,sig,skip)
  pred

orderPredTran(oldList,sig,skip) ==
  lastPreds:=nil
  --(1) make two kinds of predicates appear last:
  -----  (op *target ..) when *target does not appear later in sig
  -----  (isDomain *1 ..)
  for pred in oldList repeat
    ((pred is [op,pvar,.] and MEMQ(op,'(isDomain ofCategory))
       and pvar=first sig and not (pvar in rest sig)) or
        (not skip and pred is ['isDomain,pvar,.] and pvar="*1")) =>
          oldList:=delete(pred,oldList)
          lastPreds:=[pred,:lastPreds]
--sayBrightlyNT "lastPreds="
--pp lastPreds

  --(2a) lastDependList=list of all variables that lastPred forms depend upon
  lastDependList := "UNIONQ"/[listOfPatternIds x for x in lastPreds]
--sayBrightlyNT "lastDependList="
--pp lastDependList

  --(2b) dependList=list of all variables that isDom/ofCat forms depend upon
  dependList :=
    "UNIONQ"/[listOfPatternIds y for x in oldList |
      x is ['isDomain,.,y] or x is ['ofCategory,.,y]]
--sayBrightlyNT "dependList="
--pp dependList

  --(3a) newList= list of ofCat/isDom entries that don't depend on
  for x in oldList repeat
    if (x is ['ofCategory,v,body]) or (x is ['isDomain,v,body]) then
      indepvl:=listOfPatternIds v
      depvl:=listOfPatternIds body
    else
      indepvl := listOfPatternIds x
      depvl := nil
    (INTERSECTIONQ(indepvl,dependList) = nil)
        and INTERSECTIONQ(indepvl,lastDependList) =>
      somethingDone := true
      lastPreds := [:lastPreds,x]
      oldList := delete(x,oldList)
--if somethingDone then
--  sayBrightlyNT "Again lastPreds="
--  pp lastPreds
--  sayBrightlyNT "Again oldList="
--  pp oldList

  --(3b) newList= list of ofCat/isDom entries that don't depend on
  while oldList repeat
    for x in oldList repeat
      if (x is ['ofCategory,v,body]) or (x is ['isDomain,v,body]) then
        indepvl:=listOfPatternIds v
        depvl:=listOfPatternIds body
      else
        indepvl := listOfPatternIds x
        depvl := nil
      (INTERSECTIONQ(indepvl,dependList) = nil) =>
        dependList:= setDifference(dependList,depvl)
        newList:= [:newList,x]
--  sayBrightlyNT "newList="
--  pp newList

  --(4) noldList= what is left over
    (noldList:= setDifference(oldList,newList)) = oldList =>
--    sayMSG '"NOTE: Parameters to domain have circular dependencies"
      newList := [:newList,:oldList]
      return nil
    oldList:=noldList
--  sayBrightlyNT "noldList="
--  pp noldList

  for pred in newList repeat
    if pred is ['isDomain,x,y] or x is ['ofCategory,x,y] then
      ids:= listOfPatternIds y
      if and/[id in fullDependList for id in ids] then
        fullDependList:= insertWOC(x,fullDependList)
      fullDependList:= UNIONQ(fullDependList,ids)

  newList:=[:newList,:lastPreds]

--substitute (isDomain ..) forms as completely as possible to avoid false paths
  newList := isDomainSubst newList
  answer := [['AND,:newList],:INTERSECTIONQ(fullDependList,sig)]
--sayBrightlyNT '"answer="
--pp answer

isDomainSubst u == main where
  main ==
    u is [head,:tail] =>
      nhead :=
        head is ['isDomain,x,y] => ['isDomain,x,fn(y,tail)]
        head
      [nhead,:isDomainSubst rest u]
    u
  fn(x,alist) ==
    atom x =>
      IDENTP x and MEMQ(x,$PatternVariableList) and (s := findSub(x,alist)) => s
      x
    [first x, :[fn(y, alist) for y in rest x]]
  findSub(x,alist) ==
    null alist => nil
    alist is [['isDomain,y,z],:.] and x = y => z
    findSub(x,rest alist)

signatureTran pred ==
  atom pred => pred
  pred is ['has,D,catForm] and isCategoryForm(catForm,$e) =>
    ['ofCategory,D,catForm]
  [signatureTran p for p in pred]

interactiveModemapForm mm ==
  --  create modemap form for use by the interpreter.  This function
  --  replaces all specific domains mentioned in the modemap with pattern
  --  variables, and predicates
  mm := replaceVars(COPY mm,$PatternVariableList,$FormalMapVariableList)
  [pattern:=[dc,:sig],pred] := mm
  pred := [fn x for x in pred] where fn x ==
    x is [a,b,c] and a ~= 'isFreeFunction and atom c => [a,b,[c]]
    x
--pp pred
  [mmpat, patternAlist, partial, patvars] :=
    modemapPattern(pattern,sig)
--pp [pattern, mmpat, patternAlist, partial, patvars]
  [pred,domainPredicateList] :=
    substVars(pred,patternAlist,patvars)
--pp [pred,domainPredicateList]
  [pred,:dependList]:=
    fixUpPredicate(pred,domainPredicateList,partial,rest mmpat)
--pp [pred,dependList]
  [cond, :.] := pred
  [mmpat, cond]

modemapPattern(mmPattern,sig) ==
  --  Returns a list of the pattern of a modemap, an Alist of the
  --  substitutions made, a boolean flag indicating whether
  --  the result type is partial, and a list of unused pattern variables
  patternAlist := nil
  mmpat := nil
  patvars := $PatternVariableList
  partial := false
  for xTails in tails mmPattern repeat
    x := first xTails
    if x is ['Union,dom,tag] and tag = '"failed" and xTails=sig then
      x := dom
      partial := true
    patvar := rassoc(x,patternAlist)
    not null patvar => mmpat := [patvar,:mmpat]
    patvar := first patvars
    patvars := rest patvars
    mmpat := [patvar,:mmpat]
    patternAlist := [[patvar,:x],:patternAlist]
  [NREVERSE mmpat,patternAlist,partial,patvars]

substVars(pred,patternAlist,patternVarList) ==
  --make pattern variable substitutions
  domainPredicates := nil
  for [[patVar,:value],:.] in tails patternAlist repeat
    pred := substitute(patVar,value,pred)
    patternAlist := NSUBST(patVar, value, patternAlist)
    domainPredicates := substitute(patVar,value,domainPredicates)
    if not MEMQ(value,$FormalMapVariableList) then
      domainPredicates := [["isDomain",patVar,value],:domainPredicates]
  everything := [pred,patternAlist,domainPredicates]
  for var in $FormalMapVariableList repeat
    CONTAINED(var,everything) =>
      replacementVar := first patternVarList
      patternVarList := rest patternVarList
      pred := substitute(replacementVar,var,pred)
      domainPredicates := substitute(replacementVar,var,domainPredicates)
  [pred, domainPredicates]

fixUpPredicate(predClause, domainPreds, partial, sig) ==
  --  merge the predicates in predClause and domainPreds into a
  --  single predicate
  [predicate, fn, :skip] := predClause
  if first predicate = "AND" then
    predicates := APPEND(domainPreds,rest predicate)
  else if predicate ~= MKQ "T"
--was->then predicates:= REVERSE [predicate, :domainPreds]
       then predicates:= [predicate, :domainPreds]
       else predicates := domainPreds or [predicate]
  if #predicates > 1 then
    pred := ["AND",:predicates]
    [pred,:dependList]:=orderPredicateItems(pred,sig,skip)
  else
    pred := orderPredicateItems(first predicates,sig,skip)
    dependList:= if pred is ['isDomain,pvar,[.]] then [pvar] else nil
  pred := moveORsOutside pred
  if partial then pred := ["partial", :pred]
  [[pred, fn, :skip],:dependList]

moveORsOutside p ==
  p is ['AND,:q] =>
    q := [moveORsOutside r for r in q]
    x := or/[r for r in q | r is ['OR,:s]] =>
        moveORsOutside(['OR, :[['AND, :substitute(t, x, q)] for t in rest x]])
    ['AND,:q]
  p

replaceVars(x,oldvars,newvars) ==
  --  replace every identifier in oldvars with the corresponding
  --  identifier in newvars in the expression x
  for old in oldvars for new in newvars repeat
    x := substitute(new,old,x)
  x

getDomainFromMm mm ==
  -- Returns the Domain (or package or category) of origin from a pattern
  -- modemap
  [., cond] := mm
  if cond is ['partial, :c] then cond := c
  condList :=
    cond is ['AND, :cl] => cl
    cond is ['OR, ['AND, :cl],:.] => cl  --all cl's should give same info
    [cond]
  val :=
    for condition in condList repeat
      condition is ['isDomain, "*1", dom] => return opOf dom
      condition is ['ofCategory, "*1", cat] and _
          not(member(opOf cat, ["finiteAggregate", "shallowlyMutable", _
               "arbitraryPrecision", "canonicalUnitNormal"]))_
             => return opOf cat
  null val =>
    keyedSystemError("S2GE0016",
      ['"getDomainFromMm",'"Can't find domain in modemap condition"])
  val

getFirstArgTypeFromMm mm ==
  -- Returns the type of the first argument or nil
  [pats, cond] := mm
  [.,.,:args] := pats
  null args => nil
  arg1 := first args
  if cond is ['partial, :c] then cond := c
  condList :=
    cond is ['AND, :cl] => cl
    cond is ['OR, ['AND, :cl],:.] => cl  --all cl's should give same info
    [cond]
  type := nil
  for condition in condList while not type repeat
      if condition is ['isDomain, a1, dom] and a1=arg1 then type := dom
  type

isFreeFunctionFromMm mm ==
  -- This returns true is the modemap represents a free function, ie,
  -- one not coming from a domain or category.
  [., cond] := mm
  isFreeFunctionFromMmCond cond

isFreeFunctionFromMmCond cond ==
  -- This returns true is the modemap represents a free function, ie,
  -- one not coming from a domain or category.
  if cond is ['partial, :c] then cond := c
  condList :=
    cond is ['AND, :cl] => cl
    cond is ['OR, ['AND, :cl],:.] => cl  --all cl's should give same info
    [cond]
  iff := false
  for condition in condList while not iff repeat
      if condition is ['isFreeFunction, :.] then iff := true
  iff

getAllModemapsFromDatabase(op,nargs) ==
  $getUnexposedOperations: local := true
  startTimingProcess 'diskread
  ans := getSystemModemaps(op,nargs)
  stopTimingProcess 'diskread
  ans

getModemapsFromDatabase(op,nargs) ==
  $getUnexposedOperations: local := false
  startTimingProcess 'diskread
  ans := getSystemModemaps(op,nargs)
  stopTimingProcess 'diskread
  ans

getSystemModemaps(op,nargs) ==
  mml:= GETDATABASE(op,'OPERATION) =>
    mms := NIL
    for (x := [[.,:sig],.]) in mml repeat
      (NUMBERP nargs) and (nargs ~= #QCDR sig) => 'iterate
      $getUnexposedOperations or isFreeFunctionFromMm(x) or
        isExposedConstructor(getDomainFromMm(x)) => mms := [x,:mms]
      'iterate
    mms
  nil

mkAlistOfExplicitCategoryOps target ==
  if target is ['add,a,:l] then
    target:=a
  target is ['Join,:l] =>
    "union"/[mkAlistOfExplicitCategoryOps cat for cat in l]
  target is ['CATEGORY,.,:l] =>
    l:= flattenSignatureList ['PROGN,:l]
    u:=
      [[atomizeOp op,:sig] for x in l | x is ['SIGNATURE,op,sig,:.]]
            where
              atomizeOp op ==
                atom op => op
                op is [a] => a
                keyedSystemError("S2GE0016",
                  ['"mkAlistOfExplicitCategoryOps",'"bad signature"])
    opList:= REMDUP ASSOCLEFT u
    [[x,:fn(x,u)] for x in opList] where
      fn(op,u) ==
        u is [[a,:b],:c] => (a=op => [b,:fn(op,c)]; fn(op,c))
  isCategoryForm(target,$e) => nil
  keyedSystemError("S2GE0016",
    ['"mkAlistOfExplicitCategoryOps",'"bad signature"])

flattenSignatureList(x) ==
  atom x => nil
  x is ['SIGNATURE,:.] => [x]
  x is ['IF,cond,b1,b2] =>
     append(flattenSignatureList b1, flattenSignatureList b2)
  x is ['PROGN,:l] =>
     ll:= []
     for x in l repeat
        x is ['SIGNATURE,:.] => ll:=cons(x,ll)
        ll:= append(flattenSignatureList x,ll)
     ll
  nil

mkDatabasePred [a,t] ==
  isCategoryForm(t,$e) => ['ofCategory,a,t]
  ['ofType,a,t]

formal2Pattern x ==
  SUBLIS(pairList($FormalMapVariableList,rest $PatternVariableList),x)

updateDatabase(fname,cname,systemdir?) ==
 -- for now in NRUNTIME do database update only if forced
  not $forceDatabaseUpdate => nil
  clearClams()
  clearAllSlams []
  if constructor? cname then
    if GET(cname, 'LOADED) then
      clearConstructorCaches()

REMOVER(lst,item) ==
  --destructively removes item from lst
  not PAIRP lst =>
    lst=item => nil
    lst
  first lst=item => rest lst
  RPLNODE(lst,REMOVER(first lst,item),REMOVER(rest lst,item))

allLASSOCs(op,alist) ==
  [value for [key,:value] in alist | key = op]

--% Miscellaneous Stuff

getOplistForConstructorForm (form := [op,:argl]) ==
  --  The new form is an op-Alist which has entries (<op> . signature-Alist)
  --    where signature-Alist has entries (<signature> . item)
  --      where item has form (<slotNumber> <condition> <kind>)
  --        where <kind> =  ELT | CONST | (XLAM..) ..
  pairlis:= [[fv,:arg] for fv in $FormalMapVariableList for arg in argl]
  opAlist := getOperationAlistFromLisplib op
  [:getOplistWithUniqueSignatures(op,pairlis,signatureAlist)
      for [op,:signatureAlist] in opAlist]

getOplistWithUniqueSignatures(op,pairlis,signatureAlist) ==
  alist:= nil
  for [sig, :[slotNumber, pred, kind]] in signatureAlist repeat
      key := SUBLIS(pairlis, [op, sig])
      term := assoc(key, alist)
      if null term then
          alist := cons([key, pred, [kind, nil, slotNumber]], alist)
      else
          value := rest term
          oldpred := first value
          newpred :=
              oldpred = true or pred = true => true
              oldpred = pred => oldpred
              oldpred is ['OR, :predl] =>
                  member(pred, predl) => oldpred
                  ['OR, pred, :predl]
              ['OR, pred, oldpred]
          RPLACA(value, newpred)
  alist

--% Code For Modemap Insertion

insertModemap(new,mmList) ==
  null mmList => [new]
--isMoreSpecific(new,old:= first mmList) => [new,:mmList]
--[old,:insertModemap(new,rest mmList)]
  [new,:mmList]

--% Exposure Group Code

dropPrefix(fn) ==
  member(fn.0,[char "?",char "-",char "+"]) => SUBSTRING(fn,1,nil)
  fn

DEFPARAMETER($globalExposureHash, nil)

initExposureHash() ==
    $globalExposureHash := MAKE_-HASH_-TABLE()
    for grdata in $globalExposureGroupAlist repeat
        group := first(grdata)
        alist := rest(grdata)
        for pair in alist repeat
            name := first(pair)
            ogr := HGET($globalExposureHash, name)
            HPUT($globalExposureHash, name, [group, :ogr])

isExposedConstructor name ==
  -- this function checks the local exposure data in the frame to
  -- see if the given constructor is exposed. The format of
  -- $localExposureData is a vector with
  --   slot 0: list of groups exposed in the frame
  --   slot 1: list of constructors explicitly exposed
  --   slot 2: list of constructors explicitly hidden
  -- check if it is explicitly hidden
  MEMQ(name,'(Union Record Mapping)) => true
  MEMQ(name,$localExposureData.2) => false
  -- check if it is explicitly exposed
  MEMQ(name,$localExposureData.1) => true
  -- check if it is in an exposed group
  found := NIL
  if null($globalExposureHash) then
      initExposureHash()
  exd := HGET($globalExposureHash, name)
  for g in $localExposureData.0 while not found repeat
      null(g in exd) => 'iterate
      found := true
  found

displayExposedGroups() ==
  sayKeyedMsg("S2IZ0049A",[$interpreterFrameName])
  if null $localExposureData.0
    then centerAndHighlight '"there are no exposed groups"
    else for g in $localExposureData.0 repeat
      centerAndHighlight g

displayExposedConstructors() ==
  sayKeyedMsg("S2IZ0049B",NIL)
  if null $localExposureData.1
    then centerAndHighlight
      '"there are no explicitly exposed constructors"
    else for c in $localExposureData.1 repeat
      centerAndHighlight c

displayHiddenConstructors() ==
  sayKeyedMsg("S2IZ0049C",NIL)
  if null $localExposureData.2
    then centerAndHighlight
      '"there are no explicitly hidden constructors"
    else for c in $localExposureData.2 repeat
      centerAndHighlight c

getOperationAlistFromLisplib x ==
    u := GETDATABASE(x, 'OPERATIONALIST)
    --  u := removeZeroOneDestructively u
    null u => u          -- this can happen for Object
    CAAR u = '_$unique => rest u
    f := addConsDB '(NIL T ELT)
    for [op, :sigList] in u repeat
        for items in tails sigList repeat
            [sig, :r] := first items
            if r is [., :s] then
                if s is [., :t] then
                    if t is [.] then nil
                    else RPLACD(s, QCDDR f)
                else RPLACD(r, QCDR f)
            else RPLACD(first items, f)
            RPLACA(items, addConsDB first items)
    u and markUnique u

markUnique x ==
    u := first x
    RPLACA(x, '(_$unique))
    RPLACD(x, [u, :rest x])
    rest x

--=======================================================================
--          Creation of System Sig/Pred Vectors & Hash Tables
--=======================================================================

addConsDB x == x
