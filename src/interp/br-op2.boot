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

--====================> WAS br-op2.boot <================================

--=======================================================================
--                   Operation Description
--=======================================================================

htSayConstructor(key, u) ==
  u is ['CATEGORY,kind,:r] =>
    htSayList(['"a ", kind, '" "])
    htSayExplicitExports(r)
  key = 'is =>
    htSay '"the domain "
    bcConform(u,true)
  htSay
    key = 'is => '"the domain "
    kind := get_database(opOf(u), 'CONSTRUCTORKIND)
    kind = 'domain => '"an element of "
    '"a domain of "
  u is ['Join,:middle,r] =>
    rest middle =>
      htSay '"categories "
      bcConform(first middle,true)
      for x in rest middle repeat
        htSay '", "
        bcConform(x,true)
      r is ['CATEGORY,.,:r] =>
        htSay '" and "
        htSayExplicitExports(r)
      htSay '" and "
      bcConform(r,true)
    htSay '"category "
    bcConform(first middle,true)
    r is ['CATEGORY,.,:r] =>
     htSay '" "
     htSayExplicitExports(r)
    htSay '" and "
    bcConform(r,true)
  htSayList([kind, '" "])
  bcConform(u, true)

htSayExplicitExports r ==
  htSay '"with explicit exports"
  $displayReturnValue => nil
  htSay '":"
  for x in r repeat
    htSay '"\newline "
    x is ['SIGNATURE,op,sig] =>
      ops := escapeSpecialChars STRINGIMAGE op
      htMakePage [['bcLinks,[ops,'"",'oPage,ops]]]
      htSay '": "
      bcConform ['Mapping,:sig]
    x is ['ATTRIBUTE, a] => BREAK()
    x is ['IF,:.] =>
      htSay('"{\em if ...}")
    systemError()

displayBreakIntoAnds pred ==
  pred is [op,:u] and member(op,'(and AND)) => u
  [pred]

htSayValue t ==
  t is ['Mapping,target,:source] =>
      htSay('"a function from ")
      htSayTuple source
      htSay '" to "
      htSayArgument target
  t = '(Category) => htSay('"a category")
  t is [op,:.] and MEMQ(op,'(Join CATEGORY)) or constructor? opOf t =>
    htSayConstructor(nil,t)
  htSay('"an element of domain ")
  htSayArgument t                            --continue for operations

htSayArgument t == --called only for operations not for constructors
  null $signature => htSay ['"{\em ",t,'"}"]
  MEMQ(t, '(_%)) =>
    $conkind = '"category" and $conlength > 20 =>
      $generalSearch? => htSay '"{\em D} of the origin category"
      addWhereList("%", 'is, nil)
      htSayStandard '"{\em %}"
    htSayStandard '"{\em %}"
  not IDENTP t => bcConform(t,true)
  k := position(t,$conargs)
  if k > -1 then
    typeOfArg := (rest $signature).k
    addWhereList(t,'member,typeOfArg)
  htSayList(['"{\em ", t, '"}"])

addWhereList(id,kind,typ) ==
  $whereList := insert([id,kind,:typ],$whereList)

htSayTuple t ==
  null t => htSay '"()"
  null rest t => htSayArgument first t
  htSay '"("
  htSayArgument first t
  for d in rest t repeat
    htSay '","
    htSayArgument d
  htSay '")"

dbGetDisplayFormForOp(op,sig,doc) ==
    dbGetFormFromDocumentation(op, sig, doc)
        or dbMakeContrivedForm(op, sig, true)

dbGetFormFromDocumentation(op,sig,x) ==
  $ncMsgList : local := nil
  doc := (STRINGP x => x; first x)
  STRINGP doc and
     (stringPrefix?('"\spad{",doc) and (k := 6) or
       stringPrefix?('"\s{",doc) and (k := 3)) =>
    n := charPosition($charRbrace,doc,k)
    s := SUBSTRING(doc,k,n - k)
    parse := ncParseFromString s
    parse is [=op,:.] and #parse = #sig => parse
  nil

dbMakeContrivedForm(op, sig, down?) ==
  $NumberList : local := '(i j k l m n i1 j1 k1 l1 m1 n1 i2 j2 k2 l2 m2 n2 i3 j3 k3 l3 m3 n3 i4 j4 k4 l4 m4 n4 )
  $ElementList: local := '(x y z u v w x1 y1 z1 u1 v1 w1 x2 y2 z2 u2 v2 w2 x3 y3 z3 u3 v3 w3 x4 y4 z4 u4 v4 w4 )
  $FunctionList:local := '(f g h d e F G H)
  $DomainList:  local := '(R S D E T A B C M N P Q U V W)
  op = '"0" => [0]
  op = '"1" => [1]
  [op, :[dbChooseOperandName(s, down?) for s in rest sig]]

dbChooseOperandName(typ, down?) ==
  typ is ['Mapping,:.] =>
    x := first $FunctionList
    $FunctionList := rest $FunctionList
    x
  name := opOf typ
  kind :=
        name = "%" => 'domain
        get_database(name, 'CONSTRUCTORKIND)
  s := PNAME opOf typ
  kind ~= 'category =>
    anySubstring?('"Integer",s,0) or anySubstring?('"Number",s,0) =>
      x := first $NumberList
      $NumberList := rest $NumberList
      x
    x :=
      down? =>
        y := DOWNCASE typ
        x :=
          member(y,$ElementList) => y
          first $ElementList
      first $ElementList
    $ElementList := delete(x,$ElementList)
    x
  x := first $DomainList
  $DomainList := rest $DomainList
  x

getSubstSigIfPossible sig ==
  getSubstSignature sig or sig

fullSubstitute(x,y,z) ==  --substitutes deeply: x for y in list z
  z = y => x
  atom z => z
  [fullSubstitute(x,y,u) for u in z]

getSubstCandidates sig ==
  candidates := nil
  for x in sig for i in 1.. | x is [.,.,:.] repeat
    getSubstQualify(x,i,sig) => candidates := getSubstInsert(x,candidates)
    y := or/[getSubstQualify(y,i,sig) for y in rest x | y is [.,.,:.]] =>
      candidates := insert(y,candidates)
  candidates

getSubstSignature sig ==
    candidates := getSubstCandidates sig
    null candidates => nil
    D := first $DomainList
    $DomainList := rest $DomainList
    winner := first candidates
    newsig := fullSubstitute(D,winner,sig)
    sig :=
      null rest candidates => newsig
      count := NUMOFNODES newsig
      for x in rest candidates repeat
        trial := fullSubstitute(D,x,sig)
        trialCount := NUMOFNODES trial
        trialCount < count =>
          newsig := trial
          count  := trialCount
          winner := x
      newsig
    addWhereList(D,'is,winner)
    newsig

getSubstQualify(x,i,sig) ==
    or/[CONTAINED(x,y) for y in sig for j in 1.. | j ~= i] => x
    false

getSubstInsert(x,candidates) ==
    return insert(x,candidates)
    null candidates => [x]
    or/[CONTAINED(x,y) for y in candidates] => candidates
    y := or/[CONTAINED(y, x) for y in candidates] =>
        substitute(x, y, candidates)
    candidates


--=======================================================================
--                   Get Attribute/Operation Alist
--=======================================================================

try_eval(pred, ancestors) ==
    pred is [op, :args] =>
        op = "OR" =>
            nargs := [v for arg in args | (v := try_eval(arg, ancestors))]
            NULL(nargs) => false
            member('T, nargs) => 'T
            #nargs = 1 => first(nargs)
            [op, :nargs]
        op = "AND" =>
            nargs := [v for arg in args |
                      not((v := try_eval(arg, ancestors)) = 'T)]
            NULL(nargs) => 'T
            member(false, nargs) => false
            #nargs = 1 => first(nargs)
            [op, :nargs]
        op = "has" and args is [="%", cat] =>
            av := assoc(cat, ancestors)
            NULL(av) => false
            av is [cat, :'T] => 'T
            pred
        pred
    pred

simp_pred(pred, ancestors) ==
    npred := simpHasPred(pred)
    NULL(ancestors) => npred
    NULL(npred) => npred
    npred = 'T => npred
    try_eval(npred, ancestors)

-- returns alist with elements of form [op, :sigl] where
-- sigl is list of elements of form [sig, pred, nil, nil]
koOps(conform, domname) ==
    conform0 := conform
    conform := domname or conform
    [conname,:args] := conform
    kind := get_database(conname, 'CONSTRUCTORKIND)
    ancestors :=
        kind = "domain" or kind = "package" =>
            ancestorsOf(conform0, domname)
        []
    res := koCatOps(conform, domname, ancestors)
    listSort(function GLESSEQP, res)

zeroOneConvert x ==
  x = 'Zero => 0
  x = 'One  => 1
  x

kFormatSlotDomain1(x, infovec) ==
              fn formatSlotDomain1(x, infovec) where fn x ==
  atom x => x
  (op := first x) = '_% => '_%
  op = 'local => CADR x
  op = ":" => [":",CADR x,fn CADDR x]
  MEMQ(op,$Primitives) or constructor? op =>
    [fn y for y in x]
  INTEGERP op => op
  op = 'QUOTE and atom CADR x => CADR x
  x

koCatOps(conform, domname, ancestors) ==
  conname := opOf conform
  oplist := REVERSE(get_database(conname, 'OPERATIONALIST))
  oplist := sublisFormal(IFCDR domname or IFCDR conform ,oplist)
  --check below for INTEGERP key to avoid subsumed signatures
  [[zeroOneConvert op,:nalist] for [op,:alist] in oplist
      | nalist := koCatOps1(alist, ancestors)]

koCatOps1(alist, ancestors) == [x for item in alist | x := pair] where
  pair ==
    [sig,:r] := item
    null r => [sig,true]
    [key,:options] := r
    null (pred := IFCAR options) =>
      IFCAR IFCDR options = 'ASCONST => [sig,'ASCONST]
      [sig,true]
    npred := simp_pred(pred, ancestors) => [sig, npred]
    false

--=======================================================================
--           New code for search operation alist for exact matches
--=======================================================================

opPageFast opAlist == --called by oSearch
  htPage := htInitPage(nil,nil)
  htpSetProperty(htPage,'opAlist,opAlist)
  htpSetProperty(htPage,'expandOperations,'lists)
  dbShowOp1(htPage, opAlist, 'names)

opPageFastPath opstring ==
--return nil
  x := STRINGIMAGE opstring
  charPosition(char '_*,x,0) < #x => nil     --quit if name has * in it
  op := (STRINGP x => INTERN x; x)
  mmList := getAllModemapsFromDatabase(op,nil) or return nil
  opAlist := [[op,:[item for mm in mmList]]] where item ==
    [predList, origin, sig] := modemap2Sig(op, mm)
    predicate := predList and MKPF(predList,'AND)
    exposed? := isExposedConstructor opOf origin
    [sig, predicate, origin, exposed?]
  opAlist

modemap2Sig(op,mm) ==
  [dcSig, conds] := mm
  [dc, :sig] := dcSig
  partial? :=
    conds is ['partial,:r] => conds := r
    false
  condlist := modemap2SigConds conds
  [origin, vlist, flist] := getDcForm(dc, condlist) or return nil
  subcondlist := SUBLISLIS(flist, vlist, condlist)
  [predList,vlist, flist] := getSigSubst(subcondlist, nil, vlist, flist)
  if partial? then
      target := sig.0
      ntarget := ['Union, target, '"failed"]
      sig := substitute(ntarget, target, sig)
  alist := findSubstitutionOrder? pairlis(vlist, flist) or systemError()
  predList := substInOrder(alist, predList)
  nsig := substInOrder(alist, sig)
  if hasPatternVar nsig or hasPatternVar predList then
    pp '"--------------"
    pp op
    pp predList
    pp nsig
    pp mm
--pause nsig
  [predList, origin, substitute("%", origin, nsig)]

modemap2SigConds conds ==
  conds is ['OR,:r] => modemap2SigConds first r
  conds is ['AND,:r] => r
  [conds]

hasPatternVar x ==
  IDENTP x and (x ~= "**") => isPatternVar x
  atom x => false
  or/[hasPatternVar y for y in x]

getDcForm(dc, condlist) ==
  -- FIXME: normally first condition on *1 gives origin, but not
  -- always.  In particular, if we get category with no operations
  -- than this is clearly wrong, so try next (happens with attributes).
  -- We should make this reliable.
  candidates := [x for x in condlist | x is [k,=dc,:.]
                 and MEMQ(k, '(ofCategory isDomain))]
  null(candidates) => nil
  [ofWord,id,cform] := first(candidates)
  if #candidates > 1 and ofWord = 'ofCategory and _
       null(get_database(opOf(cform), 'MODEMAPS)) then
     [ofWord,id,cform] := first(rest(candidates))
  conform := getConstructorForm opOf cform
  ofWord = 'ofCategory =>
    [conform, ["*1", :rest cform], ["%", :rest conform]]
  ofWord = 'isDomain =>
    [conform, ["*1", :rest cform], ["%", :rest conform]]
  systemError()

getSigSubst(u, pl, vl, fl) ==
  u is [item, :r] =>
    item is ['AND,:s] =>
       [pl, vl, fl] := getSigSubst(s, pl, vl, fl)
       getSigSubst(r, pl, vl, fl)
    [key, v, f] := item
    key = 'isDomain => getSigSubst(r, pl, [v, :vl], [f, :fl])
    key = 'ofCategory => getSigSubst(r, pl, [$Dmarker, :vl], [f, :fl])
    key = 'ofType    => getSigSubst(r, pl, vl, fl)
    key = 'has => getSigSubst(r, [item, :pl], vl, fl)
    key = 'not => getSigSubst(r, [item, :pl], vl, fl)
    systemError()
  [pl, vl, fl]


pairlis(u,v) ==
  null u or null v => nil
  [[first u,:first v],:pairlis(rest u, rest v)]
