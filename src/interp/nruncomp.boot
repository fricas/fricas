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

-----------------------------NEW buildFunctor CODE-----------------------------
NRTaddDeltaCode() ==
--NOTES: This function is called from NRTbuildFunctor to initially
--  fill slots in $template. The $template so created is stored in the
--  NRLIB. On load, makeDomainTemplate is called on this $template to
--  create a template which becomes slot 0 of the infovec for the constructor.
--The template has 6 kinds of entries:
--  (1) formal arguments and local variables, represented by (QUOTE <entry>)
--      this conflicts by (5) but is ok since each is explicitly set by
--      instantiator code;
--  (2) domains, represented by lazy forms, e.g. (Foo 12 17 6)
--  (3) latch slots, represented SPADCALLable forms which goGet an operation
--      from a domain then cache the operation in the same slot
--  (4) functions, represented by identifiers which are names of functions
--  (5) identifiers/strings, parts of signatures (now parts of signatures
--      now must all have slot numbers, represented by (QUOTE <entry>)
--  (6) constants, like 0 and 1, represented by (CONS .. ) form
  kvec := first $catvecList
  for i in $NRTbase.. for item in REVERSE $NRTdeltaList
    for compItem in REVERSE $NRTdeltaListComp
      |null (s:=kvec.i) repeat
        $template.i:= deltaTran(item,compItem)
  $template.5 :=
    $NRTaddForm =>
      $NRTaddForm is ["@Tuple", :y] => NREVERSE y
      NRTencode($NRTaddForm,$addForm)
    nil

deltaTran(item,compItem) ==
  item is ['domain,lhs,:.] => NRTencode(lhs,compItem)
  --NOTE: all items but signatures are wrapped with domain forms
  [op,:modemap] := item
  [dcSig,[.,[kind,:.]]] := modemap
  [dc,:sig] := dcSig
  sig := substitute('$,dc,substitute("$$",'$,sig))
  dcCode :=
    dc = '$ => 0
    NRTassocIndex dc or keyedSystemError("S2NR0004",[dc])
  formalSig:= SUBLISLIS($FormalMapVariableList,$formalArgList,sig)
  kindFlag:= (kind = 'CONST => 'CONST; nil)
  newSig := [NRTassocIndex x or x for x in formalSig]
  [newSig,dcCode,op,:kindFlag]

NRTreplaceAllLocalReferences(form) ==
  $devaluateList :local := []
  NRTputInLocalReferences form

NRTencode(x,y) == encode(x,y,true, true) where
  encode(x, compForm, firstTime, domain) ==
      -- converts a domain form to a lazy domain form; everything other than
      -- the operation name should be assigned a slot
      not(firstTime) and (k := NRTassocIndex x) =>
          not(domain) and INTEGERP(k) =>
              ['NRTEVAL, [($QuickCode => 'QREFELT; 'ELT), "$", k]]
          k
      VECP(x) => systemErrorHere '"NRTencode"
      PAIRP(x) =>
          QCAR(x) = 'Record or x is ['Union, ['_:, a, b], :.] =>
              [QCAR(x), :[['_:, a, encode(b, c, false, true)]
               for [., a, b] in QCDR(x) for [., =a, c] in rest compForm]]
          constructor?(QCAR(x)) or MEMQ(QCAR x, '(Union Mapping)) =>
              cosig := rest GETDATABASE(QCAR(x), 'COSIG)
              if NULL(cosig) then
                  cosig := [true for y in QCDR(x)]
              [QCAR x, :[encode(y, z, false, cdom) for y in QCDR(x)
                          for z in rest compForm for cdom in cosig]]
          ['NRTEVAL, NRTreplaceAllLocalReferences(
                             COPY_-TREE(lispize(compForm)))]
      MEMQ(x, $formalArgList) =>
          v := $FormalMapVariableList.(POSN1(x, $formalArgList))
          firstTime => ['local, v]
          domain => v
          ['NRTEVAL, [($QuickCode => 'QREFELT; 'ELT), "$", v]]
      x = '$ => x
      x = "$$" => x
      ['QUOTE, x]

--------------FUNCTIONS CALLED DURING CAPSULE FUNCTION COMPILATION-------------
listOfBoundVars form ==
-- Only called from the function genDeltaEntry below
  form = '$ => []
  IDENTP form and (u:=get(form,'value,$e)) =>
    u:=u.expr
    MEMQ(IFCAR u, '(Union Record)) => listOfBoundVars u
    [form]
  atom form => []
  first form = 'QUOTE => []
  EQ(first form, ":") => listOfBoundVars CADDR form
  -- We don't want to pick up the tag, only the domain
  "union"/[listOfBoundVars x for x in rest form]

optDeltaEntry(op,sig,dc,eltOrConst) ==
  $killOptimizeIfTrue = true => nil
  $bootstrapDomains = true =>
    nil
  ndc :=
    dc = '$ => $functorForm
    atom dc and (dcval := get(dc,'value,$e)) => dcval.expr
    dc
  sig := substitute(ndc, dc, sig)
  not MEMQ(IFCAR ndc, $optimizableConstructorNames) => nil
  dcval := optCallEval ndc
  -- substitute guarantees to use EQUAL testing
  sig := substitute(devaluate dcval, ndc, sig)
  if rest ndc then
     for new in rest devaluate dcval for old in rest ndc repeat
       sig := substitute(new, old, sig)
     -- optCallEval sends (List X) to (List (Integer)) etc,
     -- so we should make the same transformation
  fn := compiledLookup(op,sig,dcval)
  if null fn then
    -- following code is to handle selectors like first, rest
     nsig := [quoteSelector tt for tt in sig] where
       quoteSelector(x) ==
         not(IDENTP x) => x
         get(x,'value,$e) => x
         x='$ => x
         MKQ x
     fn := compiledLookup(op,nsig,dcval)
     if null fn then return nil
  eltOrConst="CONST" => ['XLAM,'ignore,MKQ SPADCALL fn]
  GETL(compileTimeBindingOf first fn,'SPADreplace)

genDeltaEntry opMmPair ==
--called from compApplyModemap
--$NRTdeltaLength=0.. always equals length of $NRTdeltaList
  $compUniquelyIfTrue: local:= false
  [op,[dc,:sig],[.,cform:=[eltOrConst,.,nsig]]] := opMmPair
  if $profileCompiler = true then profileRecord(dc,op,sig)
  eltOrConst = 'XLAM => cform
  if atom dc then
    dc = "$" => nsig := sig
    if NUMBERP nsig then nsig := substitute('$,dc,substitute("$$","$",sig))
    -- following hack needed to invert Rep to $ substitution
--  if odc = 'Rep and cform is [.,.,osig] then sig:=osig
  newimp := optDeltaEntry(op,nsig,dc,eltOrConst) => newimp
  setDifference(listOfBoundVars dc,$functorLocalParameters) ~= [] =>
    ['applyFun,['compiledLookupCheck,MKQ op,
         mkList consSig(nsig,dc),consDomainForm(dc,nil)]]
  odc := dc
  if null atom dc then dc := substitute("$$",'$,dc)
 --   sig := substitute('$,dc,sig)
 --   cform := substitute('$,dc,cform)
  opModemapPair :=
    [op,[dc,:[genDeltaSig x for x in nsig]],['T,cform]] -- force pred to T
  if null NRTassocIndex dc and dc ~= $NRTaddForm and
    (member(dc,$functorLocalParameters) or null atom dc) then
    --create "domain" entry to $NRTdeltaList
      $NRTdeltaList:= [['domain,NRTaddInner dc,:dc],:$NRTdeltaList]
      saveNRTdeltaListComp:= $NRTdeltaListComp:=[nil,:$NRTdeltaListComp]
      $NRTdeltaLength := $NRTdeltaLength+1
      compEntry := (compOrCroak(odc, $EmptyMode, $e)).expr
      RPLACA(saveNRTdeltaListComp,compEntry)
  u :=
    [eltOrConst,'$,$NRTbase+$NRTdeltaLength-index] where index ==
      (n:= POSN1(opModemapPair,$NRTdeltaList)) => n + 1
        --n + 1 since $NRTdeltaLength is 1 too large
      $NRTdeltaList:= [opModemapPair,:$NRTdeltaList]
      $NRTdeltaListComp:=[nil,:$NRTdeltaListComp]
      $NRTdeltaLength := $NRTdeltaLength+1
      0
  u

genDeltaSig x ==
  NRTgetLocalIndex x

NRTassocIndex x == --returns index of "domain" entry x in al
  NULL x => x
  x = $NRTaddForm => 5
  k := or/[i for i in 1.. for y in $NRTdeltaList
            | first(y) = 'domain and NTH(1, y) = x] =>
    $NRTbase + $NRTdeltaLength - k
  nil

NRTgetLocalIndex(item) ==
  k := NRTassocIndex item => k
  item = $NRTaddForm => 5
  item = '$ => 0
  item = '_$_$ => 2
  value:=
    MEMQ(item,$formalArgList) => item
    nil
  atom item and null MEMQ(item,'($ _$_$))
   and null value =>  --give slots to atoms
    $NRTdeltaList:= [['domain,NRTaddInner item,:value],:$NRTdeltaList]
    $NRTdeltaListComp:=[item,:$NRTdeltaListComp]
    $NRTdeltaLength := $NRTdeltaLength+1
    $NRTbase + $NRTdeltaLength - 1
  $NRTdeltaList:= [['domain,NRTaddInner item,:value],:$NRTdeltaList]
  saveNRTdeltaListComp:= $NRTdeltaListComp:=[nil,:$NRTdeltaListComp]
  saveIndex := $NRTbase + $NRTdeltaLength
  $NRTdeltaLength := $NRTdeltaLength+1
  compEntry := comp_delta_entry(item)
  RPLACA(saveNRTdeltaListComp,compEntry)
  saveIndex

DEFVAR($generatingCall, nil)

comp_delta_entry(item) ==
    $generatingCall and cheap_comp_delta_entry(item) => item
    (compOrCroak(item, $EmptyMode, $e)).expr

cheap_comp_delta_entry(item) ==
    item is [op, :args] =>
        not(ATOM(op)) => false
        null(cosig := GETDATABASE(op, 'COSIG)) => false
        ok := true
        for arg in args for tp in rest(cosig) while ok repeat
            ok :=
                not(tp) => false
                arg = '$ => true
                MEMBER(arg, $functorLocalParameters) => true
                cheap_comp_delta_entry(arg)
        ok
    false

NRTassignCapsuleFunctionSlot(op,sig) ==
--called from compDefineCapsuleFunction
  opSig := [op,sig]
  [.,.,implementation] := NRTisExported? opSig or return nil
    --if opSig is not exported, it is local and need not be assigned
  if $insideCategoryPackageIfTrue then
      sig := substitute('$,CADR($functorForm),sig)
  sig := [genDeltaSig x for x in sig]
  opModemapPair := [op,['_$,:sig],['T,implementation]]
  POSN1(opModemapPair,$NRTdeltaList) => nil   --already there
  $NRTdeltaList:= [opModemapPair,:$NRTdeltaList]
  $NRTdeltaListComp := [nil,:$NRTdeltaListComp]
  $NRTdeltaLength := $NRTdeltaLength+1

NRTisExported? opSig ==
  or/[u for u in $domainShell.1 | u.0 = opSig]

consOpSig(op,sig,dc) ==
  if null atom op then
    keyedSystemError("S2GE0016",['"consOpSig",'"bad operator in table"])
  mkList [MKQ op,mkList consSig(sig,dc)]

consSig(sig,dc) == [consDomainName(sigpart,dc) for sigpart in sig]

consDomainName(x,dc) ==
  x = dc => ''$
  x = '$ => ''$
  x = "$$" => ['devaluate,'$]
  x is [op,:argl] =>
    (op = 'Record) or (op = 'Union and argl is [[":",:.],:.])  =>
       mkList [MKQ op,
         :[['LIST,MKQ '_:,MKQ tag,consDomainName(dom,dc)]
                   for [.,tag,dom] in argl]]
    isFunctor op or op = 'Mapping or constructor? op =>
         -- call to constructor? needed if op was compiled in $bootStrapMode
        mkList [MKQ op,:[consDomainName(y,dc) for y in argl]]
    substitute('$,"$$",x)
  x = [] => x
  (y := LASSOC(x,$devaluateList)) => y
  k:=NRTassocIndex x =>
    ['devaluate,['ELT,'$,k]]
  get(x,'value,$e) =>
    isDomainForm(x,$e) => ['devaluate,x]
    x
  MKQ x

consDomainForm(x,dc) ==
  x = '$ => '$
  x is [op,:argl] =>
     op = ":" and argl is [tag, value] => [op, tag, consDomainForm(value,dc)]
     [op,:[consDomainForm(y,dc) for y in argl]]
  x = [] => x
  (y := LASSOC(x,$devaluateList)) => y
  k:=NRTassocIndex x => ['ELT,'$,k]
  get(x,'value,$e) or get(x,'mode,$e) => x
  MKQ x

-- First cut at resolving self-referential conditions.  FIXME: should
-- handle cyclic dependencies and conditions requiring matching at
-- runtime.

get_self_preds2(p, acc) ==
    p is [op, :l] =>
        MEMQ(op, '(AND and OR or NOT not)) => get_self_preds1(l, acc)
        op is "HasCategory" =>
            first(l) = "$" => CONS(CADR(l), acc)
            acc
        acc
    acc

get_self_preds1(pl, acc) ==
    for p in pl repeat
        acc := get_self_preds2(p, acc)
    acc

get_self_preds(pl) == REMDUP get_self_preds1(pl, nil)

boolean_subst_and(l, sub_data) ==
    res := []
    for cond in l repeat
        nc := boolean_subst1(cond, sub_data)
        nc = true => "iterate"
        not(nc) =>
            res := [nc]
            return first(res)
        res := cons(nc, res)
    res = [] => true
    #res = 1 => first(res)
    ["AND", :nreverse(res)]

boolean_subst_or(l, sub_data) ==
    res := []
    for cond in l repeat
        nc := boolean_subst1(cond, sub_data)
        nc = true =>
            res := [nc]
            return first(res)
        not(nc) => "iterate"
        res := cons(nc, res)
    res = [] => false
    #res = 1 => first(res)
    ["OR", :nreverse(res)]

boolean_subst_not(cond, sub_data) ==
   sub_data1 := rest(rest(sub_data))
   nc := boolean_subst1(cond, [FUNCTION boolean_substitute1, nil, :sub_data1])
   nc = true => false
   not(nc) => true
   ["NOT", nc]

boolean_do_subst1(cond, sub_data) ==
    fun := first(sub_data)
    FUNCALL(fun, cond, rest(sub_data))

boolean_subst1(cond, sub_data) ==
    cond = true => cond
    cond is [op, :l] =>
        MEMQ(op, '(AND and)) => boolean_subst_and(l, sub_data)
        MEMQ(op, '(OR or)) => boolean_subst_or(l, sub_data)
        MEMQ(op, '(NOT not)) => boolean_subst_not(first(l), sub_data)
        boolean_do_subst1(cond, sub_data)
    cond

boolean_substitute1(cond, sub_data) ==
    sub_data := rest(sub_data)
    good_preds := first(rest(sub_data))
    nc := LASSOC(cond, good_preds)
    nc =>
        RPLACA(sub_data, true)
        first(nc)
    cond

boolean_substitute_cond(cond, sub_data) ==
    cond = first(sub_data) =>
        RPLACA(rest(sub_data), true)
        false
    boolean_substitute1(cond, sub_data)

mk_has_dollar_quote(cat) ==
    ["HasCategory", "$", ["QUOTE", cat]]

boolean_subst(condCats, cats, sub_data1) ==
    [boolean_subst1(cond, [FUNCTION boolean_substitute_cond,
                           mk_has_dollar_quote(cat), :sub_data1])
          for cond in condCats for cat in cats]

simplify_self_preds1(catvecListMaker, condCats) ==
    self_preds := get_self_preds(condCats)
    self_preds := [cat for p in self_preds | p is ["QUOTE", cat]]
    self_preds = [] => [condCats, false]
    false_preds := []
    for c1 in self_preds repeat
        op1 := opOf(c1)
        hl := []
        found := false
        for c2 in catvecListMaker for cond in condCats repeat
            c1 = c2 =>
                found_preds := CONS([c1, cond], found_preds)
                found := true
            if op1 = opOf(c2) then
                hl := CONS([c2, cond], hl)
        if not(found) and not(hl) then
            false_preds := CONS(c1, false_preds)
    good_preds := [cc for cc in found_preds |
                     cc is [cat, cond] and not(isHasDollarPred(cond))]
    good_preds := [:[[mk_has_dollar_quote(cat), false] for cat in false_preds],
                   :[[mk_has_dollar_quote(cat), cond] for cc in good_preds
                      | cc is [cat, cond]]]
    sub_data1 := [false, good_preds]
    condCats := boolean_subst(condCats, catvecListMaker, sub_data1)
    if not(first(sub_data1)) then
        userError(["simplify_self_preds1: cannot simplify", $op, self_preds])
    [condCats, first(sub_data1)]

simplify_self_preds(catvecListMaker, condCats) ==
    progress := true
    while progress repeat
        [condCats, progress] := simplify_self_preds1(catvecListMaker, condCats)
    condCats

buildFunctor($definition is [name,:args],sig,code,$locals,$e) ==
--PARAMETERS
--  $definition: constructor form, e.g. (SquareMatrix 10 (RationalNumber))
--  sig: signature of constructor form
--  code: result of "doIt", converting body of capsule to CodeDefine forms, e.g.
--       (PROGN (LET Rep ...)
--              (: (ListOf x y) $)
--              (CodeDefine (<op> <signature> <functionName>))
--              (COND ((HasCategory $ ...) (PROGN ...))) ..)
--  $locals: list of variables to go into slot 5, e.g. (R Rep R,1 R,2 R,3 R,4)
--           same as $functorLocalParameters
--           this list is not augmented by this function
--  $e: environment
--GLOBAL VARIABLES REFERENCED:
--  $domainShell: passed in from compDefineFunctor1
--  $QuickCode: compilation flag

  if code is ['add,.,newstuff] then code := newstuff

  changeDirectoryInSlot1()  --this extends $NRTslot1PredicateList

  --pp '"=================="
  --for item in $NRTdeltaList repeat pp item

--LOCAL BOUND FLUID VARIABLES:
  $GENNO: local:= 0     --bound in compDefineFunctor1, then as parameter here
  $catvecList: local    --list of vectors v1..vn for each view
  $hasCategoryAlist: local  --list of GENSYMs bound to (HasCategory ..) items
  $SetFunctions: local  --copy of p view with preds telling when fnct defined
  $MissingFunctionInfo: local --vector marking which functions are assigned
  $ConstantAssignments: local --code for creation of constants
  $epilogue: local := nil     --code to set slot 5, things to be done last
  $extraParms:local  --Set in DomainSubstitutionFunction, used in setVector12
  $devaluateList: local --Bound to ((#1 . dv$1)..) where &1 := devaluate #1 later
  $devaluateList:= [[arg,:b] for arg in args for b in $ModeVariableList]
------------------------
  oldtime := get_run_time()
  [catsig, :argsig] := sig
  catvecListMaker:=REMDUP
    [(comp(catsig, $EmptyMode, $e)).expr,
      :[compCategories first u for u in CADR $domainShell.4]]
  condCats:= InvestigateConditions [catsig, :rest catvecListMaker]
  -- a list, one for each element of catvecListMaker
  -- indicating under what conditions this
  -- category should be present.  true => always
  makeCatvecCode:= first catvecListMaker
  emptyVector := VECTOR()
  domainShell := GETREFV (6 + $NRTdeltaLength)
  for i in 0..4 repeat domainShell.i := $domainShell.i
    --we will clobber elements; copy since $domainShell may be a cached vector
  $template := GETREFV (6 + $NRTdeltaLength)
  $catvecList:= [domainShell,:[emptyVector for u in CADR domainShell.4]]
  $SetFunctions:= GETREFV SIZE domainShell
  $MissingFunctionInfo:= GETREFV SIZE domainShell
  catNames := ['$, :[GENVAR() for u in rest catvecListMaker]]
  domname:='dv_$

  condCats := [simpBool(cc) for cc in condCats]
  condCats := simplify_self_preds(catvecListMaker, condCats)
-->  Do this now to create predicate vector; then DescendCode can refer
-->  to predicate vector if it can
  [$uncondAlist,:$condAlist] :=    --bound in compDefineFunctor1
      NRTsetVector4Part1(catNames, catvecListMaker, condCats)
  [$NRTslot1PredicateList,predBitVectorCode1,:predBitVectorCode2] :=
      makePredicateBitVector [:ASSOCRIGHT $condAlist,:$NRTslot1PredicateList]

  storeOperationCode := DescendCode(code, true, nil, first catNames)
  outsideFunctionCode:= NRTaddDeltaCode()
  storeOperationCode:= NRTputInLocalReferences storeOperationCode
  NRTdescendCodeTran(storeOperationCode,nil) --side effects storeOperationCode
  codePart2:=
      argStuffCode :=
        [[$setelt,'$,i,v] for i in 6.. for v in $FormalMapVariableList
          for arg in rest $definition]
      if MEMQ($NRTaddForm,$locals) then
         addargname := $FormalMapVariableList.(POSN1($NRTaddForm,$locals))
         argStuffCode := [[$setelt,'$,5,addargname],:argStuffCode]
      [['stuffDomainSlots,'$],:argStuffCode,
         :predBitVectorCode2, ['SETF, 'pv_$, ['QREFELT, '$, 3]],
            storeOperationCode]

  $CheckVectorList := NRTcheckVector domainShell
--CODE: part 1
  codePart1:= [:devaluateCode, createDomainCode,
                createViewCode,setVector0Code, slot3Code,:slamCode] where
    devaluateCode:= [['LET,b,['devaluate,a]] for [a,:b] in $devaluateList]
    createDomainCode:=
        ['LET, domname, ['LIST, MKQ first $definition,
                         :ASSOCRIGHT $devaluateList]]
    createViewCode:= ['LET,'$,['GETREFV, 6+$NRTdeltaLength]]
    setVector0Code:=[$setelt,'$,0,'dv_$]
    slot3Code := ['QSETREFV,'$,3,['LET,'pv_$,predBitVectorCode1]]
    slamCode:=
      isCategoryPackageName opOf $definition => nil
      [NRTaddToSlam($definition,'$)]

--CODE: part 3
  $ConstantAssignments :=
      [NRTputInLocalReferences code for code in $ConstantAssignments]
  codePart3:= [:$ConstantAssignments,:$epilogue]
  ans :=
    ['PROGN,:optFunctorPROGN [:codePart1,:codePart2,:codePart3], '$]
  ans:= minimalise ans
  SAY ['"time taken in buildFunctor: ", get_run_time() - oldtime]
  --sayBrightly '"------------------functor code: -------------------"
  --pp ans
  ans

NRTcheckVector domainShell ==
--RETURNS: an alist (((op,sig),:pred) ...) of missing functions
  alist := nil
  for i in 6..MAXINDEX domainShell repeat
--Vector elements can be one of
-- (a) T           -- item was marked
-- (b) NIL         -- item is a domain; will be filled in by setVector4part3
-- (c) categoryForm-- it was a domain view; now irrelevant
-- (d) op-signature-- store missing function info in $CheckVectorList
    v:= domainShell.i
    v=true => nil  --item is marked; ignore
    null v => nil  --a domain, which setVector4part3 will fill in
    atom v => systemErrorHere '"CheckVector"
    atom first v => nil  --category form; ignore
    assoc(first v,alist) => nil
    alist:=
      [[first v,:$SetFunctions.i],:alist]
  alist

NRTsetVector4Part1(sigs, forms, conds) ==
    uncond_list := nil
    cond_list := nil
    for sig in reverse sigs for form in reverse forms
           for cond in reverse conds repeat
        sig = '$ =>
            domainList :=
                [optimize COPY IFCAR comp(d, $EmptyMode, $e) or
                   d for d in $domainShell.4.0]
            uncond_list := APPEND(domainList, uncond_list)
            if isCategoryForm(form, $e) then
                uncond_list := [form, :uncond_list]
        evalform := eval mkEvalableCategoryForm form
        cond = true =>
            uncond_list := [form, :APPEND(evalform.4.0, uncond_list)]
        cond_list := [[cond,[form, :evalform.4.0]], :cond_list]

    reducedUncondlist := REMDUP uncond_list
    reducedConlist := [[x, :y] for [x,z] in cond_list |
                         y := SETDIFFERENCE(z, reducedUncondlist)]
    revCondlist := reverseCondlist reducedConlist
    orCondlist := [[x, :MKPF(y, 'OR)] for [x, :y] in revCondlist]
    [reducedUncondlist, :orCondlist]

reverseCondlist cl ==
  alist := nil
  for [x,:y] in cl repeat
    for z in y repeat
      u := assoc(z,alist)
      null u => alist := [[z,x],:alist]
      member(x, rest u) => nil
      RPLACD(u, [x, :rest u])
  alist

NRTmakeSlot1Info() ==
-- 4 cases:
-- a:T == b add c  --- slot1 directory has #s for entries defined in c
-- a:T == b        --- slot1 has all slot #s = NIL (see compFunctorBody)
-- a == b add c    --- not allowed (line 7 of getTargetFromRhs)
  pairlis :=
    $insideCategoryPackageIfTrue = true =>
      [:argl,dollarName] := rest $form
      [[dollarName,:'_$],:mkSlot1sublis argl]
    mkSlot1sublis rest $form
  lisplibOpAlist := transformOperationAlist SUBLIS(pairlis, $domainShell.1)
  opList :=
    $insideCategoryPackageIfTrue = true => slot1Filter lisplibOpAlist
    lisplibOpAlist
  addList := SUBLIS(pairlis,$NRTaddForm)
  [first $form,[addList,:opList]]

mkSlot1sublis argl ==
  [[a,:b] for a in argl for b in $FormalMapVariableList]

slot1Filter opList ==
--include only those ops which are defined within the capsule
  [u for x in opList | u := fn x] where
    fn [op,:l] ==
      u := [entry for entry in l | INTEGERP CADR entry] => [op,:u]
      nil

NRToptimizeHas u ==
--u is a list ((pred cond)...) -- see optFunctorBody
--produces an alist: (((HasCategory a b) . GENSYM)...)
  u is [a,:b] =>
    a='HasCategory => LASSOC(u,$hasCategoryAlist) or
      $hasCategoryAlist := [[u,:(y:=GENSYM())],:$hasCategoryAlist]
      y
    a='has => NRToptimizeHas ['HasCategory,first b,MKQ first rest b]
    a = 'QUOTE => u
    [NRToptimizeHas a,:NRToptimizeHas b]
  u

NRTaddToSlam([name,:argnames],shell) ==
  $mutableDomain => return nil
  null argnames => addToConstructorCache(name,nil,shell)
  args:= ['LIST,:ASSOCRIGHT $devaluateList]
  addToConstructorCache(name,args,shell)

genOperationAlist() ==
  $lisplibOperationAlist := [sigloc entry for entry in $domainShell.1] where
    sigloc [opsig,pred,fnsel] ==
        if pred ~= 'T then
          pred := simpBool pred
          $NRTslot1PredicateList := insert(pred,$NRTslot1PredicateList)
        fnsel is [op,a,:.] and (op = 'ELT or op = 'CONST) =>
          if $insideCategoryPackageIfTrue then
              opsig := substitute('$,CADR($functorForm),opsig)
          [opsig,pred,[op,a,vectorLocation(first opsig,CADR opsig)]]
        [opsig,pred,fnsel]

changeDirectoryInSlot1() ==  --called by buildFunctor
  genOperationAlist()
  sortedOplist := listSort(function GLESSEQP,
                           COPY_-LIST $lisplibOperationAlist,function CADR)
  $lastPred :local := nil
  $newEnv :local := $e
  $domainShell.1 := [fn entry for entry in sortedOplist] where
    fn [[op,sig],pred,fnsel] ==
       if $lastPred ~= pred then
            $newEnv := deepChaseInferences(pred,$e)
            $lastPred := pred
       [[op, genSlotSig(sig, $newEnv)], pred, fnsel]

genSlotSig(sig, $e) ==
   [genDeltaSig t for t in sig]

DEFPARAMETER($infoHash, nil)

deepChaseInferences(pred,$e) ==
    $infoHash : local := MAKE_-HASHTABLE 'EQUAL
    deepChaseInferences1(pred,$e)

deepChaseInferences1(pred,$e) ==
    pred is ['AND,:preds] or pred is ['and,:preds] =>
        for p in preds repeat $e := deepChaseInferences1(p,$e)
        $e
    pred is ['OR,pred1,:.] or pred is ['or,pred1,:.] => $e
    --    deepChaseInferences1(pred1,$e)
    pred is 'T or pred is ['NOT,:.] or pred is ['not,:.] => $e
    chaseInferences(pred,$e)

vectorLocation(op,sig) ==
  u := or/[i for i in 1.. for u in $NRTdeltaList
        | u is [=op,[='$,: xsig],:.] and sig=NRTsubstDelta(xsig) ]
  u => $NRTdeltaLength - u + 6
  nil    -- this signals that calls should be forwarded

NRTsubstDelta(initSig) ==
  sig := [replaceSlotTypes s for s in initSig] where
     replaceSlotTypes(t) ==
        atom t =>
          not INTEGERP t => t
          t = 0 => '$
          t = 2 => '_$_$
          t = 5 => $NRTaddForm
          u:= $NRTdeltaList.($NRTdeltaLength+5-t)
          first u = 'domain => CADR u
          error "bad $NRTdeltaList entry"
        MEMQ(first t, '(Mapping Union Record _:)) =>
           [first t, :[replaceSlotTypes(x) for x in rest t]]
        t
-----------------------------SLOT1 DATABASE------------------------------------

NRTputInLocalReferences bod ==
  $elt: local := ($QuickCode => 'QREFELT; 'ELT)
  NRTputInHead bod

NRTputInHead bod ==
  atom bod => bod
  bod is ['SPADCALL,:args,fn] =>
    NRTputInTail rest bod --NOTE: args = COPY of rest bod
    -- The following test allows function-returning expressions
    fn is [elt,dom,ind] and not (dom='$) and MEMQ(elt,'(ELT QREFELT CONST)) =>
      k:= NRTassocIndex dom => RPLACA(LASTNODE bod,[$elt,'_$,k])
      nil
    NRTputInHead fn
    bod
  bod is ["COND",:clauses] =>
    for cc in clauses repeat NRTputInTail cc
    bod
  bod is ["QUOTE",:.] => bod
  bod is ["CLOSEDFN",:.] => bod
  bod is ["SPADCONST",dom,ind] =>
    BREAK()
    RPLACA(bod,$elt)
    dom = '_$ => nil
    k:= NRTassocIndex dom =>
      RPLACA(LASTNODE bod,[$elt,'_$,k])
      bod
    keyedSystemError("S2GE0016",['"NRTputInHead",
       '"unexpected SPADCONST form"])
  NRTputInHead first bod
  NRTputInTail rest bod
  bod

NRTputInTail x ==
  for y in tails x repeat
    atom (u := first y) =>
      EQ(u,'$) or LASSOC(u,$devaluateList) => nil
      k:= NRTassocIndex u =>
        atom u => RPLACA(y,[$elt,'_$,k])
        -- u atomic means that the slot will always contain a vector
        BREAK()
      --this reference must check that slot is a vector
      nil
    NRTputInHead u
  x

--=======================================================================
--               Functions Creating Lisplib Information
--=======================================================================
NRTdescendCodeTran(u, condList) ==
    -- NRTbuildFunctor calls to fill $template slots with names of
    -- compiled functions
    null u => nil
    u is ['LIST] => nil
    u is [op, ., i, a] and MEMQ(op, '(SETELT QSETREFV)) =>
        null condList and a is ['CONS, fn, :.] =>
            RPLACA(u, 'LIST)
            RPLACD(u, nil)
            $template.i :=
                fn = 'IDENTITY => a
                fn is ['dispatchFunction, fn'] => fn'
                fn
        a is ['CONS, 'IDENTITY, ['FUNCALL, fn, "$"]] =>
            na := [['FUNCTION, 'makeSpadConstant], ["LIST", fn, "$", i]]
            RPLACD(a, na)
            nil
        nil   --code for this will be generated by the instantiator
    u is ['COND, :c] =>
        for [pred, :y] in c|y repeat
            NRTdescendCodeTran(first y, [pred, :condList])
    u is ['PROGN, :c] => for x in c repeat NRTdescendCodeTran(x, condList)
    nil

--=======================================================================
--                  Miscellaneous Functions
--=======================================================================
NRTaddInner x ==
--called by genDeltaEntry and others that affect $NRTdeltaList
  PROGN
    atom x => nil
    x is ['Record, :l] =>
        for [., ., y] in l repeat NRTinnerGetLocalIndex y
    first x in '(Union Mapping) =>
        for y in rest x repeat
            y is [":", ., z] => NRTinnerGetLocalIndex z
            NRTinnerGetLocalIndex y
    x is ['SubDomain, y, :.] => NRTinnerGetLocalIndex y
    getConstructorSignature x is [., :ml] =>
        for y in rest x for m in ml | not (y = '$) repeat
            isCategoryForm(m, $CategoryFrame) => NRTinnerGetLocalIndex y
    keyedSystemError("S2NR0003", [x])
  x

-- NRTaddInner should call following function instead of NRTgetLocalIndex
-- This would prevent putting spurious items in $NRTdeltaList
NRTinnerGetLocalIndex x ==
    atom x => x
    -- following test should skip Unions, Records, Mapping
    MEMQ(opOf x, '(Union Record Mapping)) => NRTgetLocalIndex x
    constructor?(x) => NRTgetLocalIndex x
    NRTaddInner x
