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

DEFVAR($has_category_hash, nil)
DEFVAR($ancestor_hash, nil)

compressHashTable(ht) == ht

hasCat(domainOrCatName,catName) ==
  catName='Type  -- every domain is a Type
   or GETDATABASE([domainOrCatName,:catName],'HASCATEGORY)

showCategoryTable con ==
  [[b,:val] for (key :=[a,:b]) in HKEYS $has_category_hash
     | a = con and (val := HGET($has_category_hash, key))]

displayCategoryTable(:options) ==
    conList := IFCAR options
    ct := MAKE_-HASHTABLE('ID)
    for (key := [a, :b]) in HKEYS $has_category_hash repeat
        HPUT(ct, a, [[b, :HGET($has_category_hash, key)], :HGET(ct, a)])
    for id in HKEYS ct | null conList or MEMQ(id,conList) repeat
        sayMSG [:bright id, '"extends:"]
        PRINT HGET(ct, id)

genCategoryTable() ==
  $ancestors_hash := MAKE_-HASHTABLE('ID)
  $has_category_hash := MAKE_-HASHTABLE('UEQUAL)
  genTempCategoryTable()
  domainList:=
    [con for con in allConstructors()
      | GETDATABASE(con,'CONSTRUCTORKIND) = 'domain]
  domainTable:= [addDomainToTable(con,getConstrCat catl) for con
    in domainList | catl := GETDATABASE(con,'CONSTRUCTORCATEGORY)]
  -- $nonLisplibDomains, $noCategoryDomains are set in BUILDOM BOOT
  specialDs := SETDIFFERENCE($nonLisplibDomains,$noCategoryDomains)
  domainTable:= [:[addDomainToTable(id, getConstrCat (eval [id]).3)
    for id in specialDs], :domainTable]
  for [id,:entry] in domainTable repeat
    for [a,:b] in encodeCategoryAlist(id,entry) repeat
      HPUT($has_category_hash, [id, :a], b)
  simpTempCategoryTable()
  compressHashTable $ancestors_hash
  simpCategoryTable()
  compressHashTable $has_category_hash

simpTempCategoryTable() ==
  for id in HKEYS $ancestors_hash repeat
    for (u:=[a,:b]) in GETDATABASE(id,'ANCESTORS) repeat
      RPLACD(u,simpHasPred b)

simpCategoryTable() == main where
  main ==
    for key in HKEYS $has_category_hash repeat
      entry := HGET($has_category_hash, key)
      null entry => HREM($has_category_hash, key)
      change :=
        atom opOf entry => simpHasPred entry
        [[x,:npred] for [x,:pred] in entry | npred := simpHasPred pred]
      HPUT($has_category_hash, key, change)

simpHasPred(pred) == simpHasPred2(pred, [])

simpHasPred2(pred, options) == main where
  main ==
    $hasArgs: local := IFCDR IFCAR options
    simp pred
  simp pred ==
    pred is [op,:r] =>
      op = 'has => simpHas(pred,first r,first rest r)
      op = 'HasCategory => simp ['has, first r, simpDevaluate CADR r]
      op = 'HasSignature =>
         [op,sig] := simpDevaluate CADR r
         ['has, first r, ['SIGNATURE, op, sig]]
      op = 'HasAttribute => BREAK()
      MEMQ(op,'(AND OR NOT)) =>
        null (u := MKPF([simp p for p in r],op)) => nil
        u is '(QUOTE T) => true
        simpBool u
      op = 'hasArgs => ($hasArgs => $hasArgs = r; pred)
      null r and opOf op = 'has => simp first pred
      pred is '(QUOTE T) => true
      op1 := LASSOC(op,'((and . AND)(or . OR)(not . NOT))) => simp [op1,:r]
    pred in '(T etc) => pred
    null pred => nil
    pred
  simpDevaluate a == EVAL SUBST('QUOTE,'devaluate,a)
  simpHas(pred,a,b) ==
    b is ['ATTRIBUTE,attr] => BREAK()
    b is ['SIGNATURE,op,sig] => simpHasSignature(pred,a,op,sig)
    IDENTP a or hasIdent b => pred
    npred := eval pred
    IDENTP npred or null hasIdent npred => npred
    pred
  eval (pred := ['has,d,cat]) ==
    x := hasCat(first d, first cat)
    y := rest cat =>
      npred := or/[p for [args,:p] in x | y = args] => simp npred
      false  --if not there, it is false
    x

simpHasSignature(pred,conform,op,sig) == --eval w/o loading
  IDENTP conform => pred
  [conname,:args] := conform
  n := #sig
  u := LASSOC(op,GETDATABASE(conname,'OPERATIONALIST))
  candidates := [x for (x := [sig1,:.]) in u | #sig1 = #sig]  or return false
  match := or/[x for (x := [sig1,:.]) in candidates
                | sig = sublisFormal(args,sig1)] or return false
  simpHasPred(match is [sig,.,:p] and sublisFormal(args,p) or true)

hasIdent pred ==
  pred is [op,:r] =>
    op = 'QUOTE => false
    or/[hasIdent x for x in r]
  pred = '_$ => false
  IDENTP pred => true
  false

addDomainToTable(id,catl) ==
  alist:= nil
  for cat in catl repeat
    cat is ['CATEGORY,:.] => nil
    cat is ['IF,pred,cat1,:.] =>
      newAlist:=
        [[a,:quickAnd(pred,b)] for [a,:b] in getCategoryExtensionAlist0 cat1]
      alist:= [:alist,:newAlist]
    alist:= [:alist,:getCategoryExtensionAlist0 cat]
  [id,:alist]

genTempCategoryTable() ==
  --generates hashtable with key=categoryName and value of the form
  --     ((form . pred) ..) meaning that
  --           "IF pred THEN ofCategory(key,form)"
  --  where form can involve #1, #2, ... the parameters of key
  for con in allConstructors()  repeat
    GETDATABASE(con,'CONSTRUCTORKIND) = 'category =>
      addToCategoryTable con
  for id in HKEYS $ancestors_hash repeat
    item := HGET($ancestors_hash, id)
    for (u:=[.,:b]) in item repeat
      RPLACD(u,simpCatPredicate simpBool b)
    HPUT($ancestors_hash, id, listSort(function GLESSEQP, item))

addToCategoryTable con ==
  u := CAAR GETDATABASE(con,'CONSTRUCTORMODEMAP) --domain
  alist := getCategoryExtensionAlist u
  HPUT($ancestors_hash, first u, alist)
  alist

encodeCategoryAlist(id,alist) ==
  newAl:= nil
  for [a,:b] in alist repeat
    [key,:argl] := a
    newEntry:=
      argl => [[argl,:b]]
      b
    u:= assoc(key,newAl) =>
      argl => RPLACD(u,encodeUnion(id,first newEntry,rest u))
      if newEntry ~= rest u then
        p:= moreGeneralCategoryPredicate(id,newEntry,rest u) => RPLACD(u,p)
        sayMSG '"Duplicate entries:"
        PRINT [newEntry,rest u]
    newAl:= [[key,:newEntry],:newAl]
  newAl

encodeUnion(id,new:=[a,:b],alist) ==
  u := assoc(a,alist) =>
    RPLACD(u,moreGeneralCategoryPredicate(id,b,rest u))
    alist
  [new,:alist]

moreGeneralCategoryPredicate(id,new,old) ==
  old = 'T or new = 'T => 'T
  old is ['has,a,b] and new is ['has,=a,c] =>
    tempExtendsCat(b,c) => new
    tempExtendsCat(c,b) => old
    ['OR,old,new]
  mkCategoryOr(new,old)

mkCategoryOr(new,old) ==
  old is ['OR,:l] => simpCategoryOr(new,l)
  ['OR,old,new]

simpCategoryOr(new,l) ==
  newExtendsAnOld:= false
  anOldExtendsNew:= false
  ['has,a,b] := new
  newList:= nil
  for pred in l repeat
    pred is ['has,=a,c] =>
      tempExtendsCat(c,b) => anOldExtendsNew:= true
      if tempExtendsCat(b,c) then newExtendsAnOld:= true
      newList:= [pred,:newList]
    newList:= [pred,:newList]
  if not newExtendsAnOld then newList:= [new,:newList]
  newList is [.] => first newList
  ['OR,:newList]

tempExtendsCat(b,c) ==
  or/[first c = a for [[a,:.],:.] in GETDATABASE(first b,'ANCESTORS)]

getCategoryExtensionAlist0 cform ==
  [[cform,:'T],:getCategoryExtensionAlist cform]

getCategoryExtensionAlist cform ==
  --avoids substitution as much as possible
  u:= GETDATABASE(first cform,'ANCESTORS) => formalSubstitute(cform,u)
  mkCategoryExtensionAlist cform

formalSubstitute(form:=[.,:argl],u) ==
  isFormalArgumentList argl => u
  EQSUBSTLIST(argl,$FormalMapVariableList,u)

isFormalArgumentList argl ==
  and/[x=fa for x in argl for fa in $FormalMapVariableList]

mkCategoryExtensionAlist cform ==
  not CONSP cform => nil
  cop := first cform
  MEMQ(cop, $CategoryNames) => mkCategoryExtensionAlistBasic cform
  catlist := formalSubstitute(cform, first getConstructorExports(cform, true))
  extendsList:= nil
  for [cat,:pred] in catlist repeat
    newList := getCategoryExtensionAlist0 cat
    finalList :=
      pred = 'T => newList
      [[a,:quickAnd(b,pred)] for [a,:b] in newList]
    extendsList:= catPairUnion(extendsList,finalList,cop,cat)
  extendsList

-- following code to handle Unions Records Mapping etc.
mkCategoryExtensionAlistBasic cform ==
  cop := first cform
--category:= eval cform
  category :=      -- changed by RSS on 7/29/87
    macrop cop => eval cform
    APPLY(cop, rest cform)
  extendsList:= [[x,:'T] for x in category.4.0]
  for [cat,pred,:.] in category.4.1 repeat
    newList := getCategoryExtensionAlist0 cat
    finalList :=
      pred = 'T => newList
      [[a,:quickAnd(b,pred)] for [a,:b] in newList]
    extendsList:= catPairUnion(extendsList,finalList,cop,cat)
  extendsList

catPairUnion(oldList,newList,op,cat) ==
  for pair in newList repeat
    u:= assoc(first pair,oldList) =>
      rest u = rest pair => nil
      RPLACD(u,addConflict(rest pair,rest u)) where addConflict(new,old) ==
        quickOr(new,old)
    oldList:= [pair,:oldList]
  oldList

simpCatPredicate p ==
  p is ['OR,:l] =>
    (u:= simpOrUnion l) is [p] => p
    ['OR,:u]
  p

simpOrUnion l ==
  if l then simpOrUnion1(first l,simpOrUnion rest l)
  else l

simpOrUnion1(x,l) ==
  null l => [x]
  p:= mergeOr(x,first l) => [p,:rest l]
  [first l,:simpOrUnion1(x,rest l)]

mergeOr(x,y) ==
  x is ['has,a,b] and y is ['has,=a,c] =>
    testExtend(b,c) => y
    testExtend(c,b) => x
    nil
  nil

testExtend(a:=[op,:argl],b) ==
  (u:= GETDATABASE(op,'ANCESTORS)) and (val:= LASSOC(b,u)) =>
    formalSubstitute(a,val)
  nil

getConstrCat(x) ==
-- gets a different representation of the constructorCategory from the
-- lisplib, which is a list of named categories or conditions
  x:= if x is ['Join,:y] then y else [x]
  cats:= NIL
  for y in x repeat
    y is ['CATEGORY,.,:z] =>
      for zz in z repeat cats := makeCatPred(zz, cats, true)
    cats:= CONS(y,cats)
  cats:= nreverse cats
  cats


makeCatPred(zz, cats, thePred) ==
  if zz is ['IF,curPred := ['has,z1,z2],ats,.] then
    ats := if ats is ['PROGN,:atl] then atl else [ats]
    for at in ats repeat
--      at is ['ATTRIBUTE,z3] =>
--          BREAK()
      if at is ['ATTRIBUTE,z3] and not atom z3 and
        constructor? first z3 then
          cats:= CONS(['IF,quickAnd(['has,z1,z2], thePred),z3,'noBranch],cats)
      at is ['IF, pred, :.] =>
        cats := makeCatPred(at, cats, curPred)
  cats

getConstructorExports(conform, do_constr) == categoryParts(conform,
  GETDATABASE(opOf conform, 'CONSTRUCTORCATEGORY), do_constr)

DEFVAR($attrlist)
DEFVAR($oplist)
DEFVAR($conslist)

categoryParts(conform, category, do_constr) == main where
  main ==
    $attrlist: local := nil
    $oplist  : local := nil
    $conslist: local := nil
    conname := opOf conform
    for x in exportsOf(category) repeat build(x,true)
    $attrlist := listSort(function GLESSEQP,$attrlist)
    $oplist   := listSort(function GLESSEQP,$oplist)
    res := [$attrlist,:$oplist]
    if do_constr then res := [listSort(function GLESSEQP, $conslist), :res]
    if GETDATABASE(conname,'CONSTRUCTORKIND) = 'category then
      tvl := TAKE(#rest conform,$TriangleVariableList)
      res := SUBLISLIS($FormalMapVariableList,tvl,res)
    res
  build(item,pred) ==
    item is ['SIGNATURE,op,sig,:.] => $oplist   := [[opOf op,sig,:pred],:$oplist]
    --note: opOf is needed!!! Bug in compiler puts in (One) and (Zero)
    item is ['ATTRIBUTE,attr] =>
      constructor? opOf attr =>
        $conslist := [[attr,:pred],:$conslist]
        nil
      opOf attr = 'nil => 'skip
      $attrlist := [[opOf attr,IFCDR attr,:pred],:$attrlist]
    item is ['TYPE,op,type] =>
        $oplist := [[op,[type],:pred],:$oplist]
    item is ['IF,pred1,s1,s2] =>
      build(s1,quickAnd(pred,pred1))
      s2 => build(s2,quickAnd(pred,['NOT,pred1]))
    item is ['PROGN,:r] => for x in r repeat build(x,pred)
    item in '(noBranch) => 'ok
    null item => 'ok
    systemError '"build error"
  exportsOf(target) ==
    target is ['CATEGORY,.,:r] => r
    target is ['Join,:r,f] =>
      for x in r repeat $conslist := [[x,:true],:$conslist]
      exportsOf f
    $conslist := [[target,:true],:$conslist]
    nil

updateCategoryTable(cname,kind) ==
  $updateCatTableIfTrue =>
    kind = 'package => nil
    kind = 'category => updateCategoryTableForCategory(cname)
    updateCategoryTableForDomain(cname,getConstrCat(
      GETDATABASE(cname,'CONSTRUCTORCATEGORY)))
--+
  kind = 'domain =>
    updateCategoryTableForDomain(cname,getConstrCat(
      GETDATABASE(cname,'CONSTRUCTORCATEGORY)))

updateCategoryTableForCategory(cname) ==
  clearTempCategoryTable([[cname,'category]])
  addToCategoryTable(cname)
  for id in HKEYS $ancestors_hash repeat
      for (u:=[.,:b]) in GETDATABASE(id,'ANCESTORS) repeat
        RPLACD(u,simpCatPredicate simpBool b)

updateCategoryTableForDomain(cname,category) ==
  clearCategoryTable(cname)
  [cname,:domainEntry]:= addDomainToTable(cname,category)
  for [a,:b] in encodeCategoryAlist(cname,domainEntry) repeat
    HPUT($has_category_hash, [cname, :a], b)
  $doNotCompressHashTableIfTrue = true => $has_category_hash
  compressHashTable $has_category_hash

clearCategoryTable($cname) ==
  MAPHASH('clearCategoryTable1, $has_category_hash)

clearCategoryTable1(key,val) ==
  (first key = $cname) => HREM($has_category_hash, key)
  nil

clearTempCategoryTable(catNames) ==
  for key in HKEYS($ancestors_hash) repeat
    MEMQ(key,catNames) => nil
    extensions:= nil
    for (extension:= [catForm,:.]) in GETDATABASE(key,'ANCESTORS)
      repeat
        MEMQ(first catForm, catNames) => nil
        extensions:= [extension,:extensions]
    HPUT($ancestors_hash, key, extensions)
