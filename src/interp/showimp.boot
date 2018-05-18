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

$returnNowhereFromGoGet := false

showSummary dom ==
  showPredicates dom
  showAttributes dom
  showFrom dom
  showImp dom

--=======================================================================
--          Show Where Functions in Domain are Implemented
--=======================================================================
showImp(dom,:options) ==
  sayBrightly '"-------------Operation summary-----------------"
  missingOnlyFlag := IFCAR options
  domainForm := devaluate dom
  [nam, :.] := domainForm
  $predicateList: local := GETDATABASE(nam,'PREDICATES)
  predVector := dom.3
  u := getDomainOpTable(dom,true)
  --sort into 4 groups: domain exports, unexports, default exports, others
  for (x := [.,.,:key]) in u repeat
    key = domainForm => domexports := [x,:domexports]
    FIXP key => unexports := [x,:unexports]
    isDefaultPackageForm? key => defexports := [x,:defexports]
    key = 'nowhere => nowheres := [x,:nowheres]
    key = 'constant => constants := [x,:constants]
    others := [x,:others]   --add chain domains go here
  sayBrightly
    nowheres => ['"Functions exported but not implemented by",
      :bright form2String domainForm,'":"]
    [:bright form2String domainForm,'"implements all exported operations"]
  showDomainsOp1(nowheres,'nowhere)
  missingOnlyFlag => 'done

  --first display those exported by the domain, then add chain guys
  u := [:domexports,:constants,:SORTBY('CDDR,others)]
  while u repeat
    [., ., :key] := first u
    sayBrightly
      key = 'constant =>
        ["Constants implemented by",:bright form2String key,'":"]
      ["Functions implemented by",:bright form2String key,'":"]
    u := showDomainsOp1(u,key)
  u := SORTBY('CDDR,defexports)
  while u repeat
    [., ., :key] := first u
    defop := INTERN(SUBSTRING((s := PNAME first key), 0, MAXINDEX s))
    domainForm := [defop,:CDDR key]
    sayBrightly ["Default functions from",:bright form2String domainForm,'":"]
    u := showDomainsOp1(u,key)
  u := SORTBY('CDDR,unexports)
  while u repeat
    [., ., :key] := first u
    sayBrightly ["Not exported: "]
    u := showDomainsOp1(u,key)

--=======================================================================
--          Show Information Directly From Domains
--=======================================================================
showFrom(D,:option) ==
  ops := IFCAR option
  alist := nil
  domainForm := devaluate D
  [nam,:.] := domainForm
  $predicateList: local := GETDATABASE(nam,'PREDICATES)
  for (opSig := [op,sig]) in getDomainSigs1(D,ops) repeat
    u := from?(D,op,sig)
    x := ASSOC(u,alist) => RPLACD(x,[opSig,:rest x])
    alist := [[u,opSig],:alist]
  for [conform,:l] in alist repeat
    sayBrightly concat('"From ",form2String conform,'":")
    for [op,sig] in l repeat sayBrightly ['"   ",:formatOpSignature(op,sig)]

--=======================================================================
--               Functions implementing showFrom
--=======================================================================
getDomainOps D ==
  domname := D.0
  conname := first domname
  $predicateList: local := GETDATABASE(conname,'PREDICATES)
  REMDUP listSort(function GLESSEQP,ASSOCLEFT getDomainOpTable(D,nil))

getDomainSigs(D,:option) ==
  domname := D.0
  conname := first domname
  $predicateList: local := GETDATABASE(conname,'PREDICATES)
  getDomainSigs1(D,first option)

getDomainSigs1(D,ops) == listSort(function GLESSEQP,u) where
  u == [x for x in getDomainOpTable(D, nil) |
          null ops or MEMQ(first x, ops)]

getDomainDocs(D,:option) ==
  domname := D.0
  conname := first domname
  $predicateList: local := GETDATABASE(conname,'PREDICATES)
  ops := IFCAR option
  [[op,sig,:getInheritanceByDoc(D,op,sig)] for [op,sig] in getDomainSigs1(D,ops)]

--=======================================================================
--          Getting Inheritance Info from Documentation in Lisplib
--=======================================================================
from?(D, op, sig) == IFCAR IFCDR getInheritanceByDoc(D, op, sig)

getExtensionsOfDomain domain ==
  u := getDomainExtensionsOfDomain domain
  cats := getCategoriesOfDomain domain
  for x in u repeat
    cats := union(cats,getCategoriesOfDomain EVAL x)
  [:u,:cats]

getDomainExtensionsOfDomain domain ==
  acc := nil
  d := domain
  while (u := devaluateSlotDomain(5,d)) repeat
    acc := [u,:acc]
    d := EVAL u
  acc

devaluateSlotDomain(u,dollar) ==
  u = '$ => devaluate dollar
  FIXP u and VECP (y := dollar.u) => devaluate y
  u is ['NRTEVAL,y] => MKQ eval y
  u is ['QUOTE,y] => u
  u is [op,:argl] => [op,:[devaluateSlotDomain(x,dollar) for x in argl]]
  devaluate evalSlotDomain(u,dollar)

getCategoriesOfDomain domain ==
  predkeyVec := domain.4.0
  catforms := CADR domain.4
  [fn for i in 0..MAXINDEX predkeyVec | test] where
     test == predkeyVec.i and
       (x := catforms . i) isnt ['DomainSubstitutionMacro,:.]
     fn ==
       VECP x => devaluate x
       devaluateSlotDomain(x,domain)

getInheritanceByDoc(D,op,sig,:options) ==
--gets inheritance and documentation information by looking in the LISPLIB
--for each ancestor of the domain
  catList := IFCAR options or getExtensionsOfDomain D
  getDocDomainForOpSig(op,sig,devaluate D,D) or
    or/[fn for x in catList] or '(NIL NIL)
      where fn == getDocDomainForOpSig(op,sig,substDomainArgs(D,x),D)

getDocDomainForOpSig(op,sig,dollar,D) ==
  (u := LASSOC(op, GETDATABASE(first dollar, 'DOCUMENTATION)))
    and (doc := or/[[d,dollar] for [s,:d] in u | compareSig(sig,s,D,dollar)])

--=======================================================================
--               Functions implementing showImp
--=======================================================================
showDomainsOp1(u,key) ==
  while u and first u is [op, sig, : =key] repeat
    sayBrightly ['"   ",:formatOpSignature(op,sig)]
    u := rest u
  u

getDomainRefName(dom,nam) ==
  PAIRP nam => [getDomainRefName(dom,x) for x in nam]
  not FIXP nam => nam
  slot := dom.nam
  VECP slot => slot.0
  slot is ['SETELT,:.] => getDomainRefName(dom,getDomainSeteltForm slot)
  slot

getDomainSeteltForm ['SETELT,.,.,form] ==
  form is ['evalSlotDomain,u,d] => devaluateSlotDomain(u,d)
  VECP form => systemError()
  form

showPredicates dom ==
  sayBrightly '"--------------------Predicate summary-------------------"
  conname := first(dom.0)
  predvector := dom.3
  predicateList := GETDATABASE(conname,'PREDICATES)
  for i in 1.. for p in predicateList repeat
    prefix :=
      testBitVector(predvector,i) => '"true : "
      '"false: "
    sayBrightly [prefix,:pred2English p]

showAttributes dom ==
  sayBrightly '"--------------------Attribute summary-------------------"
  conname := first(dom.0)
  abb := getConstructorAbbreviation conname
  predvector := dom.3
  for [a,:p] in dom.2 repeat
    prefix :=
      testBitVector(predvector,p) => '"true : "
      '"false: "
    sayBrightly concat(prefix,form2String a)

showGoGet dom ==
  numvec := CDDR dom.4
  for i in 6..MAXINDEX dom | (slot := dom.i) is [=FUNCTION newGoGet,dol,index,:op] repeat
    numOfArgs := numvec.index
    whereNumber := numvec.(index := index + 1)
    signumList :=
      [formatLazyDomainForm(dom,numvec.(index + i)) for i in 0..numOfArgs]
    index := index + numOfArgs + 1
    namePart :=
      concat(bright "from",form2String formatLazyDomainForm(dom,whereNumber))
    sayBrightly [i,'": ",:formatOpSignature(op,signumList),:namePart]

formatLazyDomain(dom,x) ==
  VECP x => devaluate x
  x is [dollar,slotNumber,:form] => formatLazyDomainForm(dom,form)
  systemError nil

formatLazyDomainForm(dom,x) ==
  x = 0 => ["$"]
  FIXP x => formatLazyDomain(dom,dom.x)
  atom x => x
  x is ['NRTEVAL,y] => (atom y => [y]; y)
  [first x,:[formatLazyDomainForm(dom,y) for y in rest x]]
