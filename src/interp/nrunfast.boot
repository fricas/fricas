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

--=======================================================================
--                     Basic Functions
--=======================================================================
initNewWorld() ==
  $monitorNewWorld := false

isNewWorldDomain domain == INTEGERP domain.3    --see HasCategory/Attribute

getDomainByteVector dom == CDDR dom.4

getDomainView(domain,catform) == domain

-- getting constants

makeSpadConstant [fn, dollar, slot] ==
    val := FUNCALL(fn, dollar)
    u := dollar.slot
    RPLACA(u, function IDENTITY)
    RPLACD(u, val)
    val

--=======================================================
--                 Lookup From Compiled Code
--=======================================================
newGoGet(:l) ==
  [:arglist,env] := l
  slot := replaceGoGetSlot env
  APPLY(first slot,[:arglist,rest slot])  --SPADCALL it!

forceLazySlot(f) ==
    not(EQ(first f, function newGoGet)) => f
    replaceGoGetSlot(rest f)

--=======================================================
--       Lookup Function in Slot 1 (via SPADCALL)
--=======================================================

newLookupInTable(op,sig,dollar,[domain,opvec],flag) ==
  dollar = nil => systemError()
  $lookupDefaults = true =>
      -- lookup first in my cats
      newLookupInCategories(op, sig, domain, dollar, true)
        or newLookupInAddChain(op, sig, domain, dollar)
  --fast path when called from newGoGet
  success := false
  if $monitorNewWorld then
    sayLooking(concat('"---->",form2String devaluate domain,
      '"----> searching op table for:","%l","  "),op,sig,dollar)
  someMatch := false
  numvec := getDomainByteVector domain
  predvec := domain.3
  max := MAXINDEX opvec
  k := getOpCode(op,opvec,max) or return
    flag => newLookupInAddChain(op,sig,domain,dollar)
    nil
  maxIndex := MAXINDEX numvec
  start := ELT(opvec,k)
  finish :=
    greater_SI(max, k) => opvec.(add_SI(k, 2))
    maxIndex
  if greater_SI(finish, maxIndex) then systemError '"limit too large"
  numArgs := sub_SI(#sig, 1)
  success := nil
  $isDefaultingPackage: local :=
    -- use special defaulting handler when dollar non-trivial
    dollar ~= domain and isDefaultPackageForm? devaluate domain
  while finish > start repeat
    PROGN
      i := start
      numArgs ~= (numTableArgs :=numvec.i) => nil
      predIndex := numvec.(i := inc_SI i)
      predIndex ~= 0 and null testBitVector(predvec, predIndex) => nil
      loc := newCompareSig(sig, numvec, (i := inc_SI i), dollar, domain)
      null loc => nil  --signifies no match
      loc = 1 => (someMatch := true)
      loc = 0 =>
        start := add_SI(start, add_SI(numTableArgs, 4))
        i := start + 2
        someMatch := true --mark so that if subsumption fails, look for original
        subsumptionSig :=
          [newExpandTypeSlot(numvec.(add_SI(i, j)),
            dollar,domain) for j in 0..numTableArgs]
        if $monitorNewWorld then
          sayBrightly [formatOpSignature(op,sig),'"--?-->",
            formatOpSignature(op,subsumptionSig)]
        nil
      slot := domain.loc
      null atom slot =>
        EQ(QCAR slot,FUNCTION newGoGet) => someMatch:=true
                   --treat as if operation were not there
        --if EQ(QCAR slot, function newGoGet) then
        --  UNWIND_-PROTECT --break infinite recursion
        --    ((SETELT(domain,loc,'skip); slot := replaceGoGetSlot QCDR slot),
        --      if domain.loc = 'skip then domain.loc := slot)
        return (success := slot)
      slot = 'skip =>       --recursive call from above 'replaceGoGetSlot
        return (success := newLookupInAddChain(op,sig,domain,dollar))
      systemError '"unexpected format"
    start := add_SI(start, add_SI(numTableArgs, 4))
  success ~= 'failed and success =>
    if $monitorNewWorld then
        if PAIRP success then
            sayLooking1(concat('"<----", form2String(first success)),
                        rest success)
        else sayLooking1('"<----XXX---", success)
    success
  subsumptionSig and (u:= basicLookup(op,subsumptionSig,domain,dollar)) => u
  flag or someMatch => newLookupInAddChain(op,sig,domain,dollar)
  nil

AND_char := ELT('"&", 0)

isDefaultPackageForm? x == x is [op,:.]
  and IDENTP op and (s := PNAME op).(MAXINDEX s) = AND_char


--=======================================================
--       Lookup Addlist (from lookupInDomainTable or lookupInDomain)
--=======================================================
newLookupInAddChain(op,sig,addFormDomain,dollar) ==
  if $monitorNewWorld then sayLooking1('"looking up add-chain: ",addFormDomain)
  addFunction:=newLookupInDomain(op,sig,addFormDomain,dollar,5)
  addFunction =>
    if $monitorNewWorld then
      sayLooking1(concat('"<----add-chain function found for ",
        form2String devaluate addFormDomain, '"<----"), rest addFunction)
    addFunction
  nil

--=======================================================
--   Lookup In Domain (from lookupInAddChain)
--=======================================================
newLookupInDomain(op,sig,addFormDomain,dollar,index) ==
  addFormCell := addFormDomain.index =>
    INTEGERP IFCAR addFormCell =>
      or/[newLookupInDomain(op,sig,addFormDomain,dollar,i) for i in addFormCell]
    if null VECP addFormCell then lazyDomainSet(addFormCell,addFormDomain,index)
    lookupInDomainVector(op,sig,addFormDomain.index,dollar)
  nil

--=======================================================
--       Category Default Lookup (from goGet or lookupInAddChain)
--=======================================================
newLookupInCategories(op, sig, dom, dollar, check_num) ==
  slot4 := dom.4
  catVec := CADR slot4
  SIZE catVec = 0 => nil                      --early exit if no categories
  INTEGERP IFCDR catVec.0 => BREAK()
  $lookupDefaults : local := nil
  if $monitorNewWorld = true then sayBrightly concat('"----->",
    form2String devaluate dom,'"-----> searching default packages for ",op)
  predvec := dom.3
  packageVec := QCAR slot4
  nsig := substitute(dom.0, dollar.0, sig)
  for i in 0..MAXINDEX packageVec |
       (entry := packageVec.i) and entry ~= 'T repeat
    package :=
      VECP entry =>
         if $monitorNewWorld then
           sayLooking1('"already instantiated cat package",entry)
         entry
      IDENTP entry =>
        cat := catVec.i
        packageForm := nil
        if not GET(entry, 'LOADED) then loadLib entry
        infovec := GET(entry, 'infovec)
        success :=
            opvec := infovec.1
            max := MAXINDEX opvec
            code := getOpCode(op,opvec,max)
            null code => nil
            byteVector := CDDDR infovec.3
            endPos :=
              code+2 > max => SIZE byteVector
              opvec.(code+2)
            check_num and not(nrunNumArgCheck(#(QCDR sig), byteVector,
                                              opvec.code, endPos)) => nil
            --numOfArgs := byteVector.(opvec.code)
            --numOfArgs ~= #(QCDR sig) => nil
            packageForm := [entry, '$, :rest cat]
            package := evalSlotDomain(packageForm,dom)
            packageVec.i := package
            package
        null success =>
          if $monitorNewWorld = true then
            sayBrightlyNT '"  not in: "
            pp (packageForm and devaluate package or entry)
          nil
        if $monitorNewWorld then
          sayLooking1('"candidate default package instantiated: ",success)
        success
      entry
    null package => nil
    if $monitorNewWorld then
      sayLooking1('"Looking at instantiated package ",package)
    res := basicLookup(op,sig,package,dollar) =>
      if $monitorNewWorld = true then
        sayBrightly '"candidate default package succeeds"
      return res
    if $monitorNewWorld = true then
      sayBrightly '"candidate fails -- continuing to search categories"
    nil

nrunNumArgCheck(num,bytevec,start,finish) ==
   args := bytevec.start
   num = args => true
   (start := start + args + 4) = finish => nil
   nrunNumArgCheck(num,bytevec,start,finish)

--=======================================================
--         Compare Signature to One Derived from Table
--=======================================================
newCompareSig(sig, numvec, index, dollar, domain) ==
  k := index
  null (target := first sig)
   or lazyMatchArg(target,numvec.k,dollar,domain) =>
     and/[lazyMatchArg(s,numvec.(k := i),dollar,domain)
              for s in rest sig for i in (index+1)..] => numvec.(inc_SI k)
     nil
  nil

--=======================================================
--     Compare Signature to One Derived from Table
--=======================================================
lazyMatchArg(s,a,dollar,domain) == lazyMatchArg2(s,a,dollar,domain,true)

lazyMatch(source,lazyt,dollar,domain) ==
  lazyt is [op, :argl] and null atom source and op = first source
    and #(sargl := rest source) = #argl =>
      MEMQ(op,'(Record Union)) and first argl is [":",:.] =>
        and/[stag = atag and lazyMatchArg(s,a,dollar,domain)
              for [.,stag,s] in sargl for [.,atag,a] in argl]
      MEMQ(op,'(Union Mapping QUOTE)) =>
         and/[lazyMatchArg(s,a,dollar,domain) for s in sargl for a in argl]
      coSig := GETDATABASE(op,'COSIG)
      NULL coSig => error ["bad Constructor op", op]
      and/[lazyMatchArg2(s,a,dollar,domain,flag)
           for s in sargl for a in argl for flag in rest coSig]
  STRINGP source and lazyt is ['QUOTE,=source] => true
  NUMBERP source =>
      lazyt is ['_#, slotNum] => source = #(domain.slotNum)
      lazyt is ['call,'LENGTH, slotNum] => source = #(domain.slotNum)
      lazyt is ['LENGTH, slotNum] => source = #(domain.slotNum)
      nil
  source is ['construct,:l] => l = lazyt
  -- A hideous hack on the same lines as the previous four lines JHD/MCD
  nil


lazyMatchArgDollarCheck(s,d,dollarName,domainName) ==
  #s ~= #d => nil
  scoSig := GETDATABASE(opOf s,'COSIG) or return nil
  if MEMQ(opOf s, '(Union Mapping Record)) then
     scoSig := [true for x in s]
  and/[fn for x in rest s for arg in rest d for xt in rest scoSig] where
   fn ==
    x = arg => true
    x is ['elt,someDomain,opname] => lookupInDomainByName(opname,evalDomain someDomain,arg)
    x = '$ and (arg = dollarName or arg = domainName) => true
    x = dollarName and arg = domainName => true
    ATOM x or ATOM arg => false
    xt and first x = first arg =>
      lazyMatchArgDollarCheck(x,arg,dollarName,domainName)
    false

lookupInDomainByName(op,domain,arg) ==
  atom arg => nil
  opvec := domain . 1 . 2
  numvec := getDomainByteVector domain
  predvec := domain.3
  max := MAXINDEX opvec
  k := getOpCode(op,opvec,max) or return nil
  maxIndex := MAXINDEX numvec
  start := ELT(opvec,k)
  finish :=
    greater_SI(max, k) => opvec.(add_SI(k, 2))
    maxIndex
  if greater_SI(finish, maxIndex) then systemError '"limit too large"
  success := false
  while finish > start repeat
    i := start
    numberOfArgs :=numvec.i
    predIndex := numvec.(i := inc_SI i)
    predIndex ~= 0 and null testBitVector(predvec, predIndex) => nil
    slotIndex := numvec.(i + 2 + numberOfArgs)
    newStart := add_SI(start, add_SI(numberOfArgs, 4))
    slot := domain.slotIndex
    null atom slot and EQ(first slot, first arg) and EQ(rest slot, rest arg) =>
        return (success := true)
    start := add_SI(start, add_SI(numberOfArgs, 4))
  success

--=======================================================
--        Expand Signature from Encoded Slot Form
--=======================================================

newExpandTypeSlot(slot, dollar, domain) ==
--> returns domain form for dollar.slot
   newExpandLocalType(sigDomainVal(dollar, domain, slot), dollar,domain)

newExpandLocalType(lazyt, dollar, domain) ==
    VECP lazyt => lazyt.0
    isDomain lazyt => devaluate lazyt
    ATOM lazyt => lazyt
    newExpandLocalTypeForm(lazyt, dollar, domain)

newExpandLocalTypeForm([functorName,:argl],dollar,domain) ==
  MEMQ(functorName, '(Record Union)) and first argl is [":",:.] =>
    [functorName,:[['_:,tag,newExpandLocalTypeArgs(dom,dollar,domain,true)]
                                 for [.,tag,dom] in argl]]
  MEMQ(functorName, '(Union Mapping)) =>
          [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,true) for a in argl]]
  functorName = 'QUOTE => [functorName,:argl]
  coSig := GETDATABASE(functorName,'COSIG)
  NULL coSig => error ["bad functorName", functorName]
  [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,flag)
        for a in argl for flag in rest coSig]]

newExpandLocalTypeArgs(u,dollar,domain,typeFlag) ==
  u = '$ => u
  INTEGERP u =>
     typeFlag => newExpandTypeSlot(u, dollar,domain)
     domain.u
  u is ['NRTEVAL,y] => nrtEval(y,domain)
  u is ['QUOTE,y] => y
  u = "$$" => domain.0
  atom u => u   --can be first, rest, etc.
  newExpandLocalTypeForm(u,dollar,domain)

nrtEval(expr,dom) ==
  $:fluid := dom
  eval expr

domainVal(dollar,domain,index) ==
--returns a domain or a lazy slot
  index = 0 => dollar
  index = 2 => domain
  domain.index

sigDomainVal(dollar,domain,index) ==
--returns a domain or a lazy slot
  index = 0 => "$"
  index = 2 => domain
  domain.index

--=======================================================
--                   HasCategory/Attribute
--=======================================================
-- PLEASE NOTE: This function has the rather charming side-effect that
-- e.g. it works if domform is an Aldor Category.  This is being used
-- by extendscategoryForm in c-util to allow Aldor domains to be used
-- in spad code.  Please do not break this!  An example is the use of
-- Interval (an Aldor domain) by SIGNEF in limitps.spad.  MCD.
newHasTest(domform,catOrAtt) ==
  NULL(domform) => systemError '"newHasTest expects domain form"
  domform is [dom,:.] and dom in '(Union Record Mapping Enumeration) =>
    ofCategory(domform, catOrAtt)
  catOrAtt = '(Type) => true
  GETDATABASE(opOf domform, 'ASHARP?) => fn(domform,catOrAtt) where
  -- atom (infovec := getInfovec opOf domform) => fn(domform,catOrAtt) where
    fn(a,b) ==
      categoryForm?(a) => assoc(b, ancestorsOf(a, nil))
      isPartialMode a => throwKeyedMsg("S2IS0025",NIL)
      b is ["SIGNATURE",:opSig] =>
        HasSignature(evalDomain a,opSig)
      b is ["ATTRIBUTE",attr] =>
          BREAK()
      hasCaty(a,b,NIL) ~= 'failed
      HasCategory(evalDomain a,b) => true -- for asharp domains: must return Boolean
  op := opOf catOrAtt
  isAtom := atom catOrAtt
  null isAtom and op = 'Join =>
    and/[newHasTest(domform,x) for x in rest catOrAtt]
-- we will refuse to say yes for 'Cat has Cat'
--GETDATABASE(opOf domform,'CONSTRUCTORKIND) = 'category => throwKeyedMsg("S2IS0025",NIL)
-- on second thoughts we won't!
  catOrAtt is [":", fun, ["Mapping", :sig1]] =>
      evaluateType ["Mapping", :sig1] is ["Mapping", :sig2]  =>
         not(null(HasSignature(domform, [fun, sig2])))
      systemError '"strange Mapping type in newHasTest"
  GETDATABASE(opOf domform,'CONSTRUCTORKIND) = 'category =>
      domform = catOrAtt => 'T
      for [aCat,:cond] in ancestorsOf(domform,NIL) |  aCat = catOrAtt  repeat
         return evalCond cond where
           evalCond x ==
             ATOM x => x
             [pred,:l] := x
             pred = 'has =>
                  l is [ w1,['ATTRIBUTE,w2]] =>
                       BREAK()
                       newHasTest(w1,w2)
                  l is [ w1, ['SIGNATURE, :w2]] =>
                      compiledLookup(first w2, CADR w2, eval mkEvalable w1)
                  newHasTest(first  l ,first rest l)
             pred = 'OR => or/[evalCond i for i in l]
             pred = 'AND => and/[evalCond i for i in l]
             x
  null isAtom and constructor? op  =>
    domain := eval mkEvalable domform
    newHasCategory(domain,catOrAtt)
  systemError '"newHasTest expects category form"

--=======================================================
--                   Utility Functions
--=======================================================

sayLooking(prefix,op,sig,dom) ==
  $monitorNewWorld := false
  dollar := devaluate dom
  atom dollar or VECP dollar or or/[VECP x for x in dollar] => systemError nil
  sayBrightly
    concat(prefix,formatOpSignature(op,sig),bright '"from ",form2String dollar)
  $monitorNewWorld := true

sayLooking1(prefix,dom) ==
  $monitorNewWorld := false
  dollar :=
    VECP dom => devaluate dom
    devaluateList dom
  sayBrightly concat(prefix,form2String dollar)
  $monitorNewWorld := true

cc() == -- don't remove this function
  clearConstructorCaches()
  clearClams()
