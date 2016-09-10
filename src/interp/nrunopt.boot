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
--            Generate Code to Create Infovec
--=======================================================================
getInfovecCode() ==
--Function called by compDefineFunctor1 to create infovec at compile time
  ['LIST,
    MKQ makeDomainTemplate $template,
      MKQ makeCompactDirect $NRTslot1Info,
        MKQ [],
          NRTmakeCategoryAlist(),
            MKQ $lookupFunction]

--=======================================================================
--         Generation of Domain Vector Template (Compile Time)
--=======================================================================
makeDomainTemplate vec ==
--NOTES: This function is called at compile time to create the template
--  (slot 0 of the infovec); called by getInfovecCode from compDefineFunctor1
  newVec := GETREFV SIZE vec
  for index in 0..MAXINDEX vec repeat
    item := vec.index
    null item => nil
    newVec.index :=
      atom item => item
      null atom first item => makeGoGetSlot(item,index)
      item
  $byteVec := "append"/NREVERSE $byteVec
  newVec

makeGoGetSlot(item,index) ==
--NOTES: creates byte vec strings for LATCH slots
--these parts of the $byteVec are created first; see also makeCompactDirect
  [sig,whereToGo,op,:flag] := item
  n := #sig - 1
  newcode := [n,whereToGo,:makeCompactSigCode(sig),index]
  $byteVec := [newcode,:$byteVec]
  curAddress := $byteAddress
  $byteAddress := $byteAddress + n + 4
  [curAddress,:op]

--=======================================================================
--                Generate OpTable at Compile Time
--=======================================================================
--> called by getInfovecCode (see top of this file) from compDefineFunctor1
makeCompactDirect u ==
  $predListLength :local := LENGTH $NRTslot1PredicateList
  $byteVecAcc: local := nil
  [nam,[addForm,:opList]] := u
  --pp opList
  d := [[op,y] for [op,:items] in opList | y := makeCompactDirect1(op,items)]
  $byteVec := [:$byteVec,:"append"/NREVERSE $byteVecAcc]
  LIST2VEC ("append"/d)

makeCompactDirect1(op,items) ==
--NOTES: creates byte codes for ops implemented by the domain
    curAddress := $byteAddress
    $op: local := op  --temp hack by RDJ 8/90 (see orderBySubsumption)
    newcodes :=
      "append"/[u for y in orderBySubsumption items | u := fn y] or return nil
    $byteVecAcc := [newcodes,:$byteVecAcc]
    curAddress
 where fn y ==
  [sig,:r] := y
  if r is [n,:s] then
    slot :=
      n is [p, :.] => p  --the rest is linenumber of function definition
      n
    predCode :=
      s is [pred,:.] => predicateBitIndex pred
      0
  --> drop items which are not present (predCode = -1)
  predCode = -1 => return nil
  --> drop items with NIL slots if lookup function is incomplete
  if null slot then
     $lookupFunction = 'lookupIncomplete => return nil
     slot := 1   --signals that operation is not present
  n := #sig - 1
  $byteAddress := $byteAddress + n + 4
  res := [n,predCode,:makeCompactSigCode(sig),slot]
  res

orderBySubsumption items == reverse(items)

makeCompactSigCode(sig) == [fn for x in sig] where
  fn ==
    x = '_$_$ => 2
    x = '$ => 0
    NULL INTEGERP x => systemError ['"code vector slot is ",x,"; must be number"]
    x

--=======================================================================
--              Instantiation Code (Stuffslots)
--=======================================================================
stuffDomainSlots dollar ==
  domname := devaluate dollar
  infovec := GET(opOf domname, 'infovec)
  lookupFunction := getLookupFun infovec
  lookupFunction :=
    lookupFunction = 'lookupIncomplete => function lookupIncomplete
    function lookupComplete
  template := infovec.0
  if template.5 then stuffSlot(dollar,5,template.5)
  for i in (6 + # rest domname)..MAXINDEX template | item := template.i repeat
    stuffSlot(dollar,i,item)
  dollar.1 := LIST(lookupFunction,dollar,infovec.1)
  dollar.2 := infovec.2
  proto4 := infovec.3
  dollar.4 :=
    VECP CDDR proto4 => BREAK()
    bitVector := dollar.3
    predvec := first proto4
    packagevec := CADR proto4
    auxvec := LIST2VEC [fn for i in 0..MAXINDEX predvec] where fn ==
      null testBitVector(bitVector,predvec.i) => nil
      packagevec.i or 'T
    [auxvec,:CDDR proto4]

getLookupFun infovec ==
  MAXINDEX infovec = 4 => infovec.4
  'lookupIncomplete

stuffSlot(dollar,i,item) ==
  dollar.i :=
    atom item => [SYMBOL_-FUNCTION item,:dollar]
    item is [n,:op] and INTEGERP n => [FUNCTION newGoGet,dollar,:item]
    item is ['CONS,.,['FUNCALL,a,b]] =>
      b = '$ => [FUNCTION makeSpadConstant,eval a,dollar,i]
      sayBrightlyNT '"Unexpected constant environment!!"
      pp devaluate b
      nil
    item                --new form

--=======================================================================
--                Predicate utilities
--=======================================================================

predicateBitIndex x ==
      u := simpBool transHasCode x
      u = 'T  =>  0
      u = nil => -1
      p := POSN1(u,$NRTslot1PredicateList) => p + 1
      systemError nil

predicateBitRef x ==
  x = 'T => 'T
  ['testBitVector,'pv_$,predicateBitIndex x]

makePrefixForm(u,op) ==
  u := MKPF(u,op)
  u = ''T => 'T
  u
--=======================================================================
--               Generate Slot 3 Predicate Vector
--=======================================================================
makePredicateBitVector pl ==   --called by buildFunctor
  if $insideCategoryPackageIfTrue = true then
    pl := union(pl,$categoryPredicateList)
  $predGensymAlist := nil
  for p in removeAttributePredicates pl repeat
    pred := simpBool transHasCode p
    atom pred => 'skip                --skip over T and NIL
    if isHasDollarPred pred then
      lasts := insert(pred,lasts)
      for q in stripOutNonDollarPreds pred repeat firsts := insert(q,firsts)
    else
      firsts := insert(pred,firsts)
  firstPl := SUBLIS($pairlis,NREVERSE orderByContainment firsts)
  lastPl  := SUBLIS($pairlis,NREVERSE orderByContainment lasts)
  firstCode:=
    ['buildPredVector,0,0,mungeAddGensyms(firstPl,$predGensymAlist)]
  lastCode := augmentPredCode(# firstPl,lastPl)
  $lisplibPredicates := [:firstPl,:lastPl] --what is stored under 'predicates
  [$lisplibPredicates,firstCode,:lastCode]  --$pairlis set by compDefineFunctor1

augmentPredCode(n,lastPl) ==
  ['LIST,:pl] := mungeAddGensyms(lastPl,$predGensymAlist)
  delta := 2^n
  l := [(u := MKPF([x, ['augmentPredVector, '$, delta]], 'AND);
         delta:=2 * delta; u) for x in pl]

augmentPredVector(dollar,value) ==
  QSETREFV(dollar,3,value + QVELT(dollar,3))

isHasDollarPred pred ==
  pred is [op,:r] =>
    MEMQ(op,'(AND and OR or NOT not)) => or/[isHasDollarPred x for x in r]
    op is "HasCategory" => first r = '$
    false
  false

stripOutNonDollarPreds pred ==
  pred is [op,:r] and MEMQ(op,'(AND and OR or NOT not)) =>
    "append"/[stripOutNonDollarPreds x for x in r]
  not isHasDollarPred pred => [pred]
  nil

removeAttributePredicates pl ==
  [fn p for p in pl] where
    fn p ==
      p is [op,:argl] and op in '(AND and OR or NOT not) =>
          makePrefixForm(fnl argl,op)
      p is ['has,'$,['ATTRIBUTE,a]] => BREAK()
      p
    fnl p == [fn x for x in p]

transHasCode x ==
  atom x => x
  op := QCAR x
  op is "HasCategory" => x
  EQ(op,'has) => compHasFormat x
  [transHasCode y for y in x]

mungeAddGensyms(u,gal) ==
  ['LIST,:[fn(x,gal,0) for x in u]] where fn(x,gal,n) ==
    atom x => x
    g := LASSOC(x,gal) =>
      n = 0 => ['LET,g,x]
      g
    [first x,:[fn(y,gal,n + 1) for y in rest x]]

orderByContainment pl ==
  null pl or null rest pl => pl
  max := first pl
  for x in rest pl repeat
    if (y := CONTAINED(max,x)) then
      if null assoc(max,$predGensymAlist)
      then $predGensymAlist := [[max,:GENSYM()],:$predGensymAlist]
    else if CONTAINED(x,max)
         then if null assoc(x,$predGensymAlist) then $predGensymAlist := [[x,:GENSYM()],:$predGensymAlist]
    if y then max := x
  [max,:orderByContainment delete(max,pl)]

buildBitTable(:l) == fn(REVERSE l,0) where fn(l,n) ==
  null l => n
  n := n + n
  if QCAR l then n := n + 1
  fn(rest l,n)

buildPredVector(init, n, l) == fn(init, 2^n, l) where fn(acc, n, l) ==
  null l => acc
  if first l then acc := acc + n
  fn(acc,n + n,rest l)

testBitVector(vec,i) ==
--bit vector indices are always 1 larger than position in vector
  EQ(i,0) => true
  LOGBITP(i - 1,vec)

bitsOf n ==
  n = 0 => 0
  1 + bitsOf(QUOTIENT(n, 2))

--=======================================================================
--               Generate Slot 4 Constructor Vectors
--=======================================================================
NRTmakeCategoryAlist() ==
  $depthAssocCache: local := MAKE_-HASHTABLE 'ID
  $catAncestorAlist: local := NIL
  pcAlist := [:[[x,:'T] for x in $uncondAlist],:$condAlist]
  $levelAlist: local := depthAssocList [CAAR x for x in pcAlist]
  opcAlist := NREVERSE SORTBY(function NRTcatCompare,pcAlist)
  newPairlis := [[5 + i,:b] for [.,:b] in $pairlis for i in 1..]
  slot1 := [[a,:k] for [a,:b] in SUBLIS($pairlis,opcAlist)
                   | (k := predicateBitIndex b) ~= -1]
  slot0 := [hasDefaultPackage opOf a for [a,:b] in slot1]
  sixEtc := [5 + i for i in 1..#$pairlis]
  formals := ASSOCRIGHT $pairlis
  for x in slot1 repeat
      RPLACA(x, EQSUBSTLIST(["$$"], ["$"], first x))
  -----------code to make a new style slot4 -----------------
  predList := ASSOCRIGHT slot1  --is list of predicate indices
  maxPredList := "MAX"/predList
  catformvec := [encodeCatform(x, sixEtc, formals)
                   for x in ASSOCLEFT slot1]
  maxElement := "MAX"/$byteVec
  ['CONS, ['makeByteWordVec2,MAX(maxPredList,1),MKQ predList],
    ['CONS, MKQ LIST2VEC slot0,
      ['CONS, MKQ LIST2VEC catformvec,
        ['makeByteWordVec2,maxElement,MKQ $byteVec]]]]
  --NOTE: this is new form: old form satisfies VECP CDDR form

encodeCatform(x, inds, formals) ==
    k := NRTassocIndex x => k
    atom x =>
        res := nil
        for ind in inds for formal in formals while not(res) repeat
            if EQ(x, formal) then res := ind
        res => res
        SYMBOLP(x) => x
        ["QUOTE", x]
    atom rest x => x
    [first(x), :[encodeCatform(y, inds, formals) for y in rest x]]

NRTcatCompare [catform,:pred] == LASSOC(first catform,$levelAlist)

hasDefaultPackage catname ==
  defname := INTERN STRCONC(catname,'"&")
  constructor? defname => defname
--MEMQ(defname,allConstructors()) => defname
  nil


--=======================================================================
--             Generate Category Level Alist
--=======================================================================

depthAssocList u ==
  MEMQ('DomainSubstitutionMacro,u) => BREAK()
  REMDUP ("append"/[depthAssoc(y) for y in u])

depthAssoc x ==
  y := HGET($depthAssocCache,x) => y
  x is ['Join,:u] or (u := getCatAncestors x) =>
    v := depthAssocList u
    HPUT($depthAssocCache,x,[[x,:n],:v])
      where n == 1 + "MAX"/[rest y for y in v]
  HPUT($depthAssocCache,x,[[x,:0]])

getCatAncestors x ==  [CAAR y for y in parentsOf opOf x]

--=======================================================================
--                     Display Template
--=======================================================================
dc(:r) ==
  con := IFCAR r
  options := IFCDR r
  ok := MEMQ(con,allConstructors()) or (con := abbreviation? con)
  null ok =>
    sayBrightly '"Format is: dc(<constructor name or abbreviation>,option)"
    sayBrightly
      '"options are: all (default), slots, preds, cats, data, ops, optable"
  option := IFCAR options
  option = 'all or null option => dcAll con
  option = 'slots   =>  dcSlots con
  option = 'preds   =>  dcPreds  con
  option = 'cats    =>  dcCats  con
  option = 'data    =>  dcData  con
  option = 'ops     =>  dcOps   con
  option = 'size    =>  dcSize( con,'full)
  option = 'optable =>  dcOpTable con

dcSlots con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  template := $infovec.0
  for i in 5..MAXINDEX template repeat
    sayBrightlyNT bright i
    item := template.i
    item is [n,:op] and INTEGERP n => dcOpLatchPrint(op,n)
    null item and i > 5 => sayBrightly ['"arg  ",STRCONC('"#",STRINGIMAGE(i - 5))]
    atom item => sayBrightly ['"fun  ",item]
    item is ['CONS,.,['FUNCALL,[.,a],b]] => sayBrightly ['"constant ",a]
    sayBrightly concat('"lazy ",form2String formatSlotDomain i)

dcOpLatchPrint(op,index) ==
  numvec := getCodeVector()
  numOfArgs := numvec.index
  whereNumber := numvec.(index := index + 1)
  signumList := dcSig(numvec,index + 1,numOfArgs)
  index := index + numOfArgs + 1
  namePart := concat(bright "from",
    dollarPercentTran form2String formatSlotDomain whereNumber)
  sayBrightly ['"latch",:formatOpSignature(op,signumList),:namePart]

getInfovec name ==
  u := GET(name, 'infovec) => u
  GET(name, 'LOADED) => nil
  fullLibName := GETDATABASE(name,'OBJECT) or return nil
  startTimingProcess 'load
  loadLibNoUpdate(name, name, fullLibName)
  GET(name, 'infovec)

getOpSegment index ==
  numOfArgs := (vec := getCodeVector()).index
  [vec.i for i in index..(index + numOfArgs + 3)]

getCodeVector() ==
  proto4 := $infovec.3
  u := CDDR proto4
  VECP u => BREAK()
  rest u                 --new style

formatSlotDomain x ==
  x = 0 => ["$"]
  x = 2 => ["$$"]
  INTEGERP x =>
    val := $infovec.0.x
    null val => [STRCONC('"#",STRINGIMAGE (x  - 5))]
    formatSlotDomain val
  atom x => x
  x is ['NRTEVAL,y] => (atom y => [y]; y)
  x is ['QUOTE, .] => x
  [first x,:[formatSlotDomain y for y in rest x]]

--=======================================================================
--                     Display OpTable
--=======================================================================
dcOpTable con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  template := $infovec.0
  $predvec: local := GETDATABASE(name, 'PREDICATES)
  opTable := $infovec.1
  for i in 0..MAXINDEX opTable repeat
    op := opTable.i
    i := i + 1
    startIndex := opTable.i
    stopIndex :=
      i + 1 > MAXINDEX opTable => MAXINDEX getCodeVector()
      opTable.(i + 2)
    curIndex := startIndex
    while curIndex < stopIndex repeat
      curIndex := dcOpPrint(op,curIndex)

dcOpPrint(op,index) ==
  numvec := getCodeVector()
  segment := getOpSegment index
  numOfArgs := numvec.index
  index := index + 1
  predNumber := numvec.index
  index := index + 1
  signumList := dcSig(numvec,index,numOfArgs)
  index := index + numOfArgs + 1
  slotNumber := numvec.index
  suffix :=
    predNumber = 0 => nil
    [:bright '"if",:pred2English $predvec.(predNumber - 1)]
  namePart := bright
    slotNumber = 0 => '"subsumed by next entry"
    slotNumber = 1 => '"missing"
    name := $infovec.0.slotNumber
    atom name => name
    '"looked up"
  sayBrightly [:formatOpSignature(op,signumList),:namePart, :suffix]
  index + 1

dcSig(numvec,index,numOfArgs) ==
  [formatSlotDomain numvec.(index + i) for i in 0..numOfArgs]

dcPreds con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  $predvec:= GETDATABASE(name, 'PREDICATES)
  for i in 0..MAXINDEX $predvec repeat
    sayBrightlyNT bright (i + 1)
    sayBrightly pred2English $predvec.i

dcCats con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  u := $infovec.3
  VECP CDDR u => BREAK()
  $predvec:= GETDATABASE(name, 'PREDICATES)
  catpredvec := first u
  catinfo := CADR u
  catvec := CADDR u
  for i in 0..MAXINDEX catvec repeat
    sayBrightlyNT bright i
    form := catvec.i
    predNumber := catpredvec.i
    suffix :=
      predNumber = 0 => nil
      [:bright '"if",:pred2English $predvec.(predNumber - 1)]
    extra :=
      null (info := catinfo.i) => nil
      IDENTP info => bright '"package"
      bright '"instantiated"
    sayBrightly concat(form2String formatSlotDomain form,suffix,extra)

dcData con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  sayBrightly '"Operation data from slot 1"
  print_full1 $infovec.1
  vec := getCodeVector()
  vec := (PAIRP vec => rest vec; vec)
  sayBrightly ['"Information vector has ",SIZE vec,'" entries"]
  dcData1 vec

dcData1 vec ==
  n := MAXINDEX vec
  tens := n / 10
  for i in 0..tens repeat
    start := 10*i
    sayBrightlyNT rightJustifyString(STRINGIMAGE start,6)
    sayBrightlyNT '"  |"
    for j in start..MIN(start + 9,n) repeat
      sayBrightlyNT rightJustifyString(STRINGIMAGE vec.j,6)
    sayNewLine()
  vec

dcSize(:options) ==
  con := IFCAR options
  options := rest options
  null con => dcSizeAll()
  quiet := MEMQ('quiet,options)
  full := MEMQ('full,options)
  name := abbreviation? con or con
  infovec := getInfovec name
  template := infovec.0
  maxindex := MAXINDEX template
  latch := 0  --# of go get slots
  lazy  := 0  --# of lazy domain slots
  fun   := 0  --# of function slots
  lazyNodes := 0 --# of nodes needed for lazy domain slots
  for i in 5..maxindex repeat
    atom (item := template.i) =>   fun := fun + 1
    INTEGERP first item    => latch := latch + 1
    'T                 =>
       lazy := lazy + 1
       lazyNodes := lazyNodes + numberOfNodes item
  tSize := sum(vectorSize(1 + maxindex),nodeSize(lazyNodes + latch))
  -- functions are free in the template vector
  oSize := vectorSize(SIZE infovec.1)
  aSize := numberOfNodes infovec.2
  slot4 := infovec.3
  catvec :=
    VECP CDDR slot4 => BREAK()
    CADDR slot4
  n := MAXINDEX catvec
  cSize := sum(nodeSize(2), vectorSize(SIZE first slot4), vectorSize(n + 1),
               nodeSize(+/[numberOfNodes catvec.i for i in 0..n]))
  codeVector :=
    VECP CDDR slot4 => BREAK()
    CDDDR slot4
  vSize := halfWordSize(SIZE codeVector)
  itotal := sum(tSize,oSize,aSize,cSize,vSize)
  if null quiet then sayBrightly ['"infovec total = ",itotal,'" BYTES"]
  if null quiet then
    lookupFun := getLookupFun infovec
    suffix := (lookupFun = 'lookupIncomplete => '"incomplete"; '"complete")
    sayBrightly ['"template    = ",tSize]
    sayBrightly ['"operations  = ",oSize,'" (",suffix,'")"]
    sayBrightly ['"categories  = ",cSize]
    sayBrightly ['"data vector = ",vSize]
  if null quiet then
    sayBrightly ['"number of function slots (one extra node) = ",fun]
    sayBrightly ['"number of latch slots (2 extra nodes) = ",latch]
    sayBrightly ['"number of lazy slots (no extra nodes) = ",lazy]
    sayBrightly ['"size of domain vectors = ",1 + maxindex,'" slots"]
  vtotal := itotal + nodeSize(fun)       --fun   slot is ($ . function)
  vtotal := vtotal + nodeSize(2 * latch) --latch slot is (newGoGet $ . code)
  --NOTE: lazy slots require no cost     --lazy  slot is lazyDomainForm
  if null quiet then sayBrightly ['"domain size = ",vtotal,'" BYTES"]
  etotal := nodeSize(fun + 2 * latch) + vectorSize(1 + maxindex)
  if null quiet then sayBrightly ['"cost per instantiation = ",etotal,'" BYTES"]
  vtotal

dcSizeAll() ==
  count := 0
  total := 0
  for x in allConstructors() | null atom GET(x, 'infovec) repeat
    count := count + 1
    s := dcSize(x,'quiet)
    sayBrightly [s,'" : ",x]
    total := total + s
  sayBrightly '"------------total-------------"
  sayBrightly [count," constructors; ",total," BYTES"]

sum(:l) == +/l

nodeSize(n) == 12 * n

vectorSize(n) == 4 * (1 + n)

halfWordSize(n) ==
  n < 128 => n / 2
  n < 256 => n
  2 * n

numberOfNodes(x) ==
  atom x => 0
  1 + numberOfNodes first x + numberOfNodes rest x

template con ==
  con := abbreviation? con or con
  ppTemplate (getInfovec con).0

ppTemplate vec ==
  for i in 0..MAXINDEX vec repeat
    sayBrightlyNT bright i
    pp vec.i

infovec con ==
  con := abbreviation? con or con
  u := getInfovec con
  sayBrightly '"---------------slot 0 is template-------------------"
  ppTemplate u.0
  sayBrightly '"---------------slot 1 is op table-------------------"
  print_full1 u.1
  sayBrightly '"---------------slot 3.0 is catpredvec---------------"
  print_full1 u.3.0
  sayBrightly '"---------------slot 3.1 is catinfovec---------------"
  print_full1 u.3.1
  sayBrightly '"---------------slot 3.2 is catvec-------------------"
  print_full1 u.3.2
  sayBrightly '"---------------tail of slot 3 is datavector---------"
  dcData1 CDDDR u.3
  'done

dcAll con ==
  con := abbreviation? con or con
  $infovec : local := getInfovec con
  complete? :=
    #$infovec = 4 => false
    $infovec.4 = 'lookupComplete
  sayBrightly '"----------------Template-----------------"
  dcSlots con
  sayBrightly
    complete? => '"----------Complete Ops----------------"
    '"----------Incomplete Ops---------------"
  dcOpTable con
  sayBrightly '"----------------Preds-----------------"
  dcPreds con
  sayBrightly '"----------------Cats-----------------"
  dcCats con
  sayBrightly '"----------------Data------------------"
  dcData con
  sayBrightly '"----------------Size------------------"
  dcSize(con,'full)
  'done

dcOps conname ==
  for [op,:u] in REVERSE getOperationAlistFromLisplib conname repeat
    for [sig,slot,pred,key,:.] in u repeat
      suffix :=
        atom pred => nil
        concat('" if ",pred2English pred)
      sayBrightly [:formatOpSignature(op,sig),:suffix]

--=======================================================================
--              Compute the lookup function (complete or incomplete)
--=======================================================================
NRTgetLookupFunction(domform,exCategory,addForm) ==
  domform := SUBLIS($pairlis,domform)
  addForm := SUBLIS($pairlis,addForm)
  $why: local := nil
  atom addForm => 'lookupComplete
  extends := NRTextendsCategory1(domform,exCategory,getExportCategory addForm)
  if null extends then
    [u,msg,:v] := $why
    sayBrightly '"--------------non extending category----------------------"
    sayBrightlyNT ['"..",:bright form2String domform,"of cat "]
    PRINT u
    sayBrightlyNT bright msg
    if v then PRINT first v else TERPRI()
  extends => 'lookupIncomplete
  'lookupComplete

getExportCategory form ==
  [op,:argl] := form
  op = 'Record => ['RecordCategory,:argl]
  op = 'Union => ['UnionCategory,:argl]
  functorModemap := GETDATABASE(op,'CONSTRUCTORMODEMAP)
  [[.,target,:tl],:.] := functorModemap
  EQSUBSTLIST(argl,$FormalMapVariableList,target)

NRTextendsCategory1(domform,exCategory,addForm) ==
  addForm is ["@Tuple", :r] =>
    and/[extendsCategory(domform,exCategory,x) for x in r]
  extendsCategory(domform,exCategory,addForm)

--=======================================================================
--         Compute if a domain constructor is forgetful functor
--=======================================================================
extendsCategory(dom,u,v) ==
  --does category u extend category v (yes iff u contains everything in v)
  --is dom of category u also of category v?
  u=v => true
  v is ["Join",:l] => and/[extendsCategory(dom,u,x) for x in l]
  v is ["CATEGORY",.,:l] => and/[extendsCategory(dom,u,x) for x in l]
  v := substSlotNumbers(v,$template,$functorForm)
  extendsCategoryBasic0(dom,u,v) => true
  $why :=
    v is ['SIGNATURE,op,sig] => [u,['"  has no ",:formatOpSignature(op,sig)]]
    [u,'" has no",v]
  nil

extendsCategoryBasic0(dom,u,v) ==
  v is ['IF,p,['ATTRIBUTE,c],.] =>
    -- BREAK()
    uVec := (compMakeCategoryObject(u, $EmptyEnvironment)).expr
    null atom c and isCategoryForm(c,nil) =>
      slot4 := uVec.4
      LASSOC(c,CADR slot4) is [=p,:.]
    slot2 := uVec.2
    LASSOC(c,slot2) is [=p,:.]
  extendsCategoryBasic(dom,u,v)

extendsCategoryBasic(dom,u,v) ==
  u is ["Join",:l] => or/[extendsCategoryBasic(dom,x,v) for x in l]
  u = v => true
  uVec := (compMakeCategoryObject(u, $EmptyEnvironment)).expr
  isCategoryForm(v,nil) => catExtendsCat?(u,v,uVec)
  v is ['SIGNATURE,op,sig] =>
      res := false
      for csig in uVec.1 repeat
          not(csig is [[=op, sig], pred, :.]) => "iterate"
          pred = true =>
              res := true
              return true
      res
  u is ['CATEGORY,.,:l] =>
    v is ['IF,:.] => member(v,l)
    nil
  nil

catExtendsCat?(u,v,uvec) ==
  u = v => true
  uvec := uvec or (compMakeCategoryObject(u, $EmptyEnvironment)).expr
  slot4 := uvec.4
  prinAncestorList := first slot4
  member(v,prinAncestorList) => true
  vOp := IFCAR v
  if similarForm := assoc(vOp,prinAncestorList) then
    PRINT u
    sayBrightlyNT '"   extends "
    PRINT similarForm
    sayBrightlyNT '"   but not "
    PRINT v
  or/[catExtendsCat?(x,v,nil) for x in ASSOCLEFT CADR slot4]

substSlotNumbers(form,template,domain) ==
  form is [op,:.] and
    MEMQ(op,allConstructors()) => expandType(form,template,domain)
  form is ['SIGNATURE,op,sig] =>
    ['SIGNATURE,op,[substSlotNumbers(x,template,domain) for x in sig]]
  form is ['CATEGORY,k,:u] =>
    ['CATEGORY,k,:[substSlotNumbers(x,template,domain) for x in u]]
  expandType(form,template,domain)

expandType(lazyt,template,domform) ==
  atom lazyt => expandTypeArgs(lazyt,template,domform)
  [functorName,:argl] := lazyt
  MEMQ(functorName, '(Record Union)) and first argl is [":",:.] =>
     [functorName,:[['_:,tag,expandTypeArgs(dom,template,domform)]
                                 for [.,tag,dom] in argl]]
  lazyt is ['local,x] =>
    n := POSN1(x,$FormalMapVariableList)
    ELT(domform,1 + n)
  [functorName,:[expandTypeArgs(a,template,domform) for a in argl]]

expandTypeArgs(u,template,domform) ==
  u = '$ => u --template.0      -------eliminate this as $ is rep by 0
  INTEGERP u => expandType(templateVal(template, domform, u), template,domform)
  u is ['NRTEVAL,y] => y  --eval  y
  u is ['QUOTE,y] => y
  atom u => u
  expandType(u,template,domform)

templateVal(template,domform,index) ==
--returns a domform or a lazy slot
  index = 0 => harhar() --template
  template.index
