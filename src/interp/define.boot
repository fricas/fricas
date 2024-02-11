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

DEFPARAMETER($newCompCompare, false)

--% FUNCTIONS WHICH MUNCH ON == STATEMENTS

compDefine(form,m,e) ==
  result:= compDefine1(form,m,e)
  result

compDefine1(form,m,e) ==
  --1. decompose after macro-expanding form
  ['DEF, lhs, signature, rhs] := form := macroExpand(form, e)
  $insideWhereIfTrue and isMacro(form,e) and (m=$EmptyMode or m=$NoValueMode)
     => [lhs,m,put(first lhs,'macro,rhs,e)]
  null signature.target and not MEMQ(IFCAR rhs, $ConstructorNames) and
    (sig:= getSignatureFromMode(lhs,e)) =>
  -- here signature of lhs is determined by a previous declaration
      compDefine1(['DEF, lhs, [first sig, :rest signature], rhs], m, e)
  $insideCapsuleFunctionIfTrue =>
      compInternalFunction(form, m, e)
  if signature.target=$Category then $insideCategoryIfTrue:= true

-- RDJ (11/83): when argument and return types are all declared,
--  or arguments have types declared in the environment,
--  and there is no existing modemap for this signature, add
--  the modemap by a declaration, then strip off declarations and recurse
  e := compDefineAddSignature(lhs,signature,e)
-- 2. if signature list for arguments is not empty, replace ('DEF,..) by
--       ('where,('DEF,..),..) with an empty signature list;
--     otherwise, fill in all NILs in the signature
  not (and/[null x for x in rest signature]) => compDefWhereClause(form,m,e)
  signature.target=$Category =>
      compDefineCategory(form, m, e, nil, $formalArgList)
  isDomainForm(rhs,e) and not $insideFunctorIfTrue =>
    if null signature.target then signature:=
      [getTargetFromRhs(lhs,rhs,giveFormalParametersValues(rest lhs,e)),:
          rest signature]
    rhs:= addEmptyCapsuleIfNecessary(signature.target,rhs)
    new_prefix := getAbbreviation(first(lhs), #(rest(lhs)))
    compDefineFunctor(['DEF, lhs, signature, rhs], m, e,
                      new_prefix, $formalArgList)
  null($functorForm) => stackAndThrow ['"bad == form ",form]
  compDefineCapsuleFunction(form, m, e, $prefix, $formalArgList)

compDefineAddSignature([op,:argl],signature,e) ==
  (sig:= hasFullSignature(argl,signature,e)) and
   not assoc(['%, :sig], LASSOC('modemap, getProplist(op, e))) =>
     declForm:=
       [":",[op,:[[":",x,m] for x in argl for m in rest sig]],first signature]
     [.,.,e]:= comp(declForm,$EmptyMode,e)
     e
  e

hasFullSignature(argl,[target,:ml],e) ==
  target =>
    u:= [m or get(x,"mode",e) or return 'failed for x in argl for m in ml]
    u~='failed => [target,:u]

addEmptyCapsuleIfNecessary(target,rhs) ==
  MEMQ(IFCAR rhs, $SpecialDomainNames) => rhs
  ['add,rhs,['CAPSULE]]

getTargetFromRhs(lhs,rhs,e) ==
  --undeclared target mode obtained from rhs expression
  rhs is ['CAPSULE,:.] =>
    stackSemanticError(['"target category of ",lhs,
      '" cannot be determined from definition"],nil)
  rhs is ['SubDomain,D,:.] => getTargetFromRhs(lhs,D,e)
  rhs is ['add,D,['CAPSULE,:.]] => getTargetFromRhs(lhs,D,e)
  rhs is ['Record,:l] => ['RecordCategory,:l]
  rhs is ['Union,:l] => ['UnionCategory,:l]
  [.,target,.]:= compOrCroak(rhs,$EmptyMode,e)
  target is ["Category"] =>
      stackAndThrow(['"Only domains and packages can get mode form target",
                     lhs])
  target

giveFormalParametersValues(argl,e) ==
  for x in argl repeat
    e:= put(x,'value,[genSomeVariable(),get(x,'mode,e),nil],e)
  e

macroExpandInPlace(x,e) ==
  y:= macroExpand(x,e)
  atom x or atom y => y
  RPLACA(x,first y)
  RPLACD(x,rest y)
  x

macroExpand(x,e) ==   --not worked out yet
  atom x =>
      u := get(x, 'macro, e) =>
          null(rest(u)) =>
              macroExpand(first u, e)
          SAY(['"u =", u])
          userError('"macro call needs arguments")
      x
  x is ['DEF, lhs, sig, rhs] =>
    ['DEF, macroExpand(lhs, e), macroExpandList(sig, e), macroExpand(rhs, e)]
  x is [op, :args] =>
      ATOM(op) =>
          u := get(op, 'macro, e) =>
              margs := rest(u)
              u := first(u)
              null(margs) => [macroExpand(u, e), :macroExpandList(args, e)]
              #args = #margs =>
                  macroExpand(SUBLISLIS(args, margs, u), e)
              userError('"invalid macro call, #args ~= #margs")
          [op, :macroExpandList(args, e)]
      macroExpandList(x,e)
  macroExpandList(x,e)

macroExpandList(l,e) ==
  [macroExpand(x,e) for x in l]

compDefineCategory1(df is ['DEF, form, sig, body], m, e, prefix, fal) ==
  categoryCapsule :=
--+
    body is ['add,cat,capsule] =>
      body := cat
      capsule
    nil
  [d, m, e] := compDefineCategory2(form, sig, body, m, e, prefix, fal)
--+ next two lines
  if categoryCapsule and not $bootStrapMode then [.,.,e] :=
    $insideCategoryPackageIfTrue: local := true  --see NRTmakeSlot1
-->
    $categoryPredicateList: local :=
        makeCategoryPredicates(form,$lisplibCategory)
    compDefine1(mkCategoryPackage(form, categoryCapsule, e),
                $EmptyMode, e)
  [d,m,e]

makeCategoryPredicates(form,u) ==
      $tvl := TAKE(#rest form,$TriangleVariableList)
      $mvl := TAKE(#rest form,rest $FormalMapVariableList)
      fn(u,nil) where
        fn(u,pl) ==
          u is ['Join,:.,a] => fn(a,pl)
          u is ['has,:.] => insert(EQSUBSTLIST($mvl,$tvl,u),pl)
          u is [op, :.] and MEMQ(op, ["SIGNATURE", "ATTRIBUTE"]) =>
               -- EQ(op, 'ATTRIBUTE) => BREAK()
               pl
          atom u => pl
          fnl(u,pl)
        fnl(u,pl) ==
          for x in u repeat pl := fn(x,pl)
          pl

--+ the following function
mkCategoryPackage(form is [op, :argl], def, e) ==
  packageName:= INTERN(STRCONC(PNAME op,'"&"))
  packageAbb := INTERN(STRCONC(get_database(op, 'ABBREVIATION), '"-"))
  $options:local := []
  -- This stops the next line from becoming confused
  abbreviationsSpad2Cmd ['domain,packageAbb,packageName]
  -- This is a little odd, but the parser insists on calling
  -- domains, rather than packages
  nameForDollar := first SETDIFFERENCE('(S A B C D E F G H I),argl)
  packageArgl := [nameForDollar,:argl]
  capsuleDefAlist := fn(def,nil) where fn(x,oplist) ==
    atom x => oplist
    x is ['DEF,y,:.] => [y,:oplist]
    fn(rest x,fn(first x,oplist))
  catvec := eval mkEvalableCategoryForm(form, e)
  fullCatOpList := (JoinInner([catvec])).1
  catOpList :=
    --note: this gets too many modemaps in general
    --   this is cut down in NRTmakeSlot1
    [['SIGNATURE,op1,sig] for [[op1,sig],:.] in fullCatOpList
         --above line calls the category constructor just compiled
        | assoc(op1,capsuleDefAlist)]
  null catOpList => nil
  packageCategory := ['CATEGORY,'domain,
                     :SUBLISLIS(argl,$FormalMapVariableList,catOpList)]
  nils:= [nil for x in argl]
  packageSig := [packageCategory,form,:nils]
  $categoryPredicateList := SUBST(nameForDollar, '%, $categoryPredicateList)
  SUBST(nameForDollar, '%,
      ['DEF, [packageName, :packageArgl], packageSig, def])

compDefineCategory2(form, signature, body, m, e,
  $prefix,$formalArgList) ==
    --1. bind global variables
    $insideCategoryIfTrue: local:= true
    $definition : local := form
                 --used by DomainSubstitutionFunction
    $extraParms: local := nil
             --Set in DomainSubstitutionFunction, used further down
--  1.1  augment e to add declaration $: <form>
    $op: local := nil
    [$op, :argl] := form
    e := addBinding("%", [['mode, :form]], e)

--  2. obtain signature
    signature':=
        [first signature, :[getArgumentModeOrMoan(a, form, e) for a in argl]]
    e:= giveFormalParametersValues(argl,e)

--   3. replace arguments by $1,..., substitute into body,
--     and introduce declarations into environment
    sargl:= TAKE(# argl, $TriangleVariableList)
    sform := [$op, :sargl]
    $functorForm : local := sform
    $formalArgList:= [:sargl,:$formalArgList]
    aList:= [[a,:sa] for a in argl for sa in sargl]
    formalBody:= SUBLIS(aList,body)
    signature' := SUBLIS(aList,signature')
--Begin lines for category default definitions
    $functionStats: local:= [0,0]
    $functorStats: local:= [0,0]
    $addForm: local:= nil
    $functor_cosig1 : local := [categoryForm?(t) for t in rest(signature')]
    for x in sargl for t in rest signature' repeat
      [.,.,e]:= compMakeDeclaration([":",x,t],m,e)

--   4. compile body in environment of type declarations for arguments
    op':= $op
    -- following line causes cats with no with or Join to be fresh copies
    if opOf(formalBody)~='Join and opOf(formalBody)~='mkCategory then
           formalBody := ['Join, formalBody]
    body:= optFunctorBody (compOrCroak(formalBody,signature'.target,e)).expr
    if $extraParms then
      formals:=actuals:=nil
      for u in $extraParms repeat
        formals := [first u, :formals]
        actuals:=[MKQ CDR u,:actuals]
      body := ['subst_in_cat, ['QUOTE, formals],
                                       ['LIST, :actuals], body]
    if argl then body:=  -- always subst for args after extraparms
        ['subst_in_cat, ['QUOTE, sargl], ['LIST, :sargl], body]
    -- FIXME: generate call to 'devaluate' only for domains
    body:=
        ['PROG1, ['LET, g:= GENSYM(), body],
                 ['SETELT, g, 0, mkConstructor(sform)]]
    fun := do_compile([op', ['category_functor, sargl, body]], e)

--  5. give operator a 'modemap property
    pairlis:= [[a,:v] for a in argl for v in $FormalMapVariableList]
    parSignature:= SUBLIS(pairlis,signature')
    parForm:= SUBLIS(pairlis,form)
    --Equivalent to the following two lines, we hope
    if null sargl then
      evalAndRwriteLispForm('NILADIC,
            ['MAKEPROP,['QUOTE,op'],'(QUOTE NILADIC),true])

--   6. put modemaps into InteractiveModemapFrame
    $lisplibCategory:= formalBody
    if $LISPLIB then
      $lisplibForm:= form
      $lisplibKind:= 'category
      modemap:= [[parForm,:parSignature],[true,op']]
      $lisplibModemap:= modemap
      $lisplibParents  :=
        getParentsFor($op,$FormalMapVariableList,$lisplibCategory)
      $lisplibAncestors := computeAncestorsOf(sform, nil)
      $lisplibAbbreviation := constructor? $op
      domainShell := eval [op', :MAPCAR('MKQ, sargl)]
      augLisplibModemapsFromCategory(sform, formalBody, signature',
                                     domainShell)
    [fun, '(Category), e]

mkConstructor form ==
  atom form => BREAK()
  null rest form => ['QUOTE,[first form]]
  ['LIST, MKQ first form, :rest(form)]

compDefineCategory(df,m,e,prefix,fal) ==
  $lisplibCategory: local := nil
  not $insideFunctorIfTrue and $LISPLIB =>
    compDefineLisplib(df,m,e,prefix,fal,'compDefineCategory1)
  compDefineCategory1(df,m,e,prefix,fal)

compDefineFunctor(df,m,e,prefix,fal) ==
  $domainShell: local -- holds the category of the object being compiled
  $LISPLIB => compDefineLisplib(df,m,e,prefix,fal,'compDefineFunctor1)
  compDefineFunctor1(df,m,e,prefix,fal)

compDefineFunctor1(df is ['DEF, form, signature, body],
  m, e, $prefix, $formalArgList) ==
--  1. bind global variables
    $addForm: local := nil

    $functionStats: local:= [0,0]
    $functorStats: local:= [0,0]
    $signature: local := nil
    $Representation: local := nil
         --Set in doIt, accessed in the compiler - compNoStacking
    $functorLocalParameters: local := nil
    $CheckVectorList: local := nil
                  --prevents CheckVector from printing out same message twice
    $insideFunctorIfTrue: local:= true
    $genSDVar: local:= 0
    originale := e
    $op: local := nil
    [$op,:argl]:= form
    $formalArgList:= [:argl,:$formalArgList]
    $pairlis := [[a,:v] for a in argl for v in $FormalMapVariableList]
    $mutableDomain: local :=
      -- all defaulting packages should have caching turned off
       isCategoryPackageName $op or
         (if BOUNDP '$mutableDomains then MEMQ($op,$mutableDomains)
            else false )   --true if domain has mutable state
    signature':=
      [first signature, :[getArgumentModeOrMoan(a, form, e) for a in argl]]
    $functorForm : local := form
    if null first signature' then BREAK()
    target:= first signature'
    e := giveFormalParametersValues(argl, e)
    [ds, ., e] := compMakeCategoryObject(target, e) or
      sayBrightly '"   cannot produce category object:"
      pp target
      userError '"cannot produce category object"
--+ copy needed since slot1 is reset; compMake.. can return a cached vector
    base_shell := COPY_-SEQ ds
    $domainShell := base_shell
--+ 7 lines for $NRT follow
-->--these globals used by NRTmakeCategoryAlist, set by NRTsetVector4Part1
    $condAlist: local := nil
    $uncondAlist: local := nil
-->>-- next global initialized here, reset by NRTbuildFunctor
    $NRTslot1PredicateList: local := nil
       --this is used below to set $lisplibSlot1 global
    $NRTbase: local := 6 -- equals length of $domainShell
    $NRTaddForm: local := nil   -- see compAdd; NRTmakeSlot1
    $NRTdeltaLength: local := 0 -- length of $NRTdeltaList
    $NRTdeltaList: local := nil --list of misc. elts used in compiled fncts
    -- parallel to $NRTdeltaList, list of COMP-ed forms for $NRTdeltaList
    $NRTdeltaListComp: local := nil
    -- the above optimizes the calls to local domains
    $template: local:= nil --stored in the lisplib (if $NRTvec = true)
    $functionLocations: local := nil --locations of defined functions in source
    $functor_cosig1 : local := [categoryForm?(t) for t in rest(signature')]
    -- generate slots for arguments first, then for $NRTaddForm in compAdd
    for x in argl repeat NRTgetLocalIndex(x, e)
    [., ., e] := compMakeDeclaration([":", '%, target], m, e)


    if $insideCategoryPackageIfTrue~= true  then
        e := augModemapsFromCategory('%, '%, target, e)
    $signature:= signature'
    parSignature:= SUBLIS($pairlis,signature')
    parForm:= SUBLIS($pairlis,form)

--  (3.1) now make a list of the functor's local parameters; for
--  domain D in argl,check its signature: if domain, its type is Join(A1,..,An);
--  in this case, D is replaced by D1,..,Dn (gensyms) which are set
--  to the A1,..,An view of D
--+
    $functorLocalParameters := argl
    dollar :=
        $insideCategoryPackageIfTrue => first(argl)
        "$$"
    e := makeFunctorArgumentParameters(argl, rest signature',
                                     first signature', dollar, e)
 -- must do above to bring categories into scope --see line 5 of genDomainView
--  4. compile body in environment of type declarations for arguments
    op':= $op
    rettype:= signature'.target
    T := compFunctorBody(body, rettype, e, base_shell)

    body':= T.expr
    lamOrSlam :=
        $mutableDomain => 'mutable_domain_functor
        'domain_functor
    fun := do_compile(SUBLIS($pairlis, [op', [lamOrSlam, argl, body']]), e)
    --The above statement stops substitutions getting in one another's way
--+
    operationAlist := SUBLIS($pairlis,$lisplibOperationAlist)
    if $LISPLIB then
      augmentLisplibModemapsFromFunctor(parForm,operationAlist,parSignature)
    $functorStats := addStats($functorStats, $functionStats)
    reportOnFunctorCompilation($functorStats)

--  5. give operator a 'modemap property
    if $LISPLIB then
      modemap:= [[parForm,:parSignature],[true,op']]
      $lisplibModemap:= modemap
      $lisplibCategory := modemap.mmTarget
      $lisplibParents  :=
        getParentsFor($op,$FormalMapVariableList,$lisplibCategory)
      $lisplibAncestors := computeAncestorsOf(form, nil)
      $lisplibAbbreviation := constructor? $op
    $insideFunctorIfTrue:= false
    if $LISPLIB then
      $lisplibKind:=
------->This next line prohibits changing the KIND once given
--------kk := get_database($op, 'CONSTRUCTORKIND) => kk
        target is ["CATEGORY",key,:.] and key~="domain" => 'package
        'domain
      $lisplibForm:= form
      if null $bootStrapMode then
        NRTslot1Info := NRTmakeSlot1Info(form, base_shell)
        $lookupFunction: local :=
            NRTgetLookupFunction(form, CADAR $lisplibModemap, $NRTaddForm)
            --either lookupComplete (for forgetful guys) or lookupIncomplete
        $byteAddress :local := 0
        $byteVec :local := nil
        $NRTslot1PredicateList :=
          [simpBool x for x in $NRTslot1PredicateList]
        output_lisp_form(['MAKEPROP, MKQ $op, ''infovec,
                          getInfovecCode(NRTslot1Info, e)])
      $lisplibOperationAlist:= operationAlist
      $lisplibMissingFunctions:= $CheckVectorList
    if null argl then
      evalAndRwriteLispForm('NILADIC,
            ['MAKEPROP, ['QUOTE,op'], ['QUOTE,'NILADIC], true])
    [fun, ['Mapping, :signature'], originale]

compFunctorBody(body, m, e, base_shell) ==
  $bootStrapMode = true =>
    genOperationAlist(base_shell)
    [bootStrapError($functorForm, $edit_file), m, e]
  T:= compOrCroak(body,m,e)
  body is [op,:.] and MEMQ(op,'(add CAPSULE)) => T
  $NRTaddForm :=
    body is ["SubDomain",domainForm,predicate] => domainForm
    body
  T

reportOnFunctorCompilation(functorStats) ==
  displayMissingFunctions()
  if $semanticErrorStack then sayBrightly '" "
  displaySemanticErrors()
  if $warningStack then sayBrightly '" "
  displayWarnings()
  [byteCount, elapsedSeconds] := functorStats
  sayBrightly ['%l,:bright '"  Cumulative Statistics for Constructor",
    $op]
  timeString := normalizeStatAndStringify elapsedSeconds
  sayBrightly ['"      Time:",:bright timeString,'"seconds"]
  sayBrightly '" "
  'done

displayMissingFunctions() ==
  null $CheckVectorList => nil
  loc := nil
  exp := nil
  for [[op,sig,:.],:pred] in $CheckVectorList  | null pred repeat
    null member(op,$formalArgList) and
      getmode(op,$env) is ['Mapping,:.] =>
        loc := [[op,sig],:loc]
    exp := [[op,sig],:exp]
  if loc then
    sayBrightly ['%l,:bright '"  Missing Local Functions:"]
    for [op,sig] in loc for i in 1.. repeat
      sayBrightly ['"      [",i,'"]",:bright op,
        '": ",:formatUnabbreviatedSig sig]
  if exp then
    sayBrightly ['%l,:bright '"  Missing Exported Functions:"]
    for [op,sig] in exp for i in 1.. repeat
      sayBrightly ['"      [",i,'"]",:bright op,
        '": ",:formatUnabbreviatedSig sig]

--% domain view code

makeFunctorArgumentParameters(argl, sigl, target, dollar, e) ==
  $forceAdd: local:= true
  $ConditionalOperators: local := nil
  $tmp_e := e
  for a in argl for s in sigl repeat fn(a, dollar,
                                   augmentSig(s,findExtras(a,target)))
          where
    findExtras(a,target) ==
      --  see if conditional information implies anything else
      --  in the signature of a
      target is ['Join,:l] => "union"/[findExtras(a,x) for x in l]
      target is ['CATEGORY,.,:l] => "union"/[findExtras1(a,x) for x in l] where
        findExtras1(a,x) ==
          x is ['AND,:l] => "union"/[findExtras1(a,y) for y in l]
          x is ['OR,:l] => "union"/[findExtras1(a,y) for y in l]
          x is ['IF,c,p,q] =>
            union(findExtrasP(a,c),
                  union(findExtras1(a,p),findExtras1(a,q))) where
              findExtrasP(a,x) ==
                x is ['AND,:l] => "union"/[findExtrasP(a,y) for y in l]
                x is ['OR,:l] => "union"/[findExtrasP(a,y) for y in l]
                x is ['has,=a,y] and y is ['SIGNATURE,:.] => [y]
                nil
        nil
    augmentSig(s,ss) ==
       -- if we find something extra, add it to the signature
      null ss => s
      for u in ss repeat
        $ConditionalOperators:=[CDR u,:$ConditionalOperators]
      s is ['Join,:sl] =>
        u := ASSQ('CATEGORY, ss) => BREAK()
        ['Join,:sl,['CATEGORY,'package,:ss]]
      ['Join,s,['CATEGORY,'package,:ss]]
    fn(a, dollar, s) ==
      not(ATOM(a)) => BREAK()
      if isCategoryForm(s) then
        s is ["Join", :catlist] => genDomainViewList(a, dollar, rest s)
        genDomainView(a, dollar, s)
  $tmp_e

genDomainViewList(id, dollar, catlist) ==
  null catlist => nil
  catlist is [y] and not isCategoryForm(y) => nil
  for c in catlist repeat
      genDomainView(id, dollar, c)

genDomainView(viewName, dollar, c) ==
  c is ['CATEGORY, ., :l] => genDomainOps(viewName, c)
  c := substitute(dollar, "%", c)
  $tmp_e := augModemapsFromCategory(viewName, nil, c, $tmp_e)

genDomainOps(viewName, cat) ==
  oplist := getOperationAlist(viewName, viewName, cat)
  oplist:= substNames(viewName, viewName, oplist)
  for [opsig,cond,:.] in oplist for i in 0.. repeat
    if opsig in $ConditionalOperators then cond:=nil
    [op, sig] := opsig
    $tmp_e:= addModemap(op, viewName, sig, cond, ['ELT, viewName, i], $tmp_e)

compDefWhereClause(['DEF, form, signature, body], m, e) ==
-- form is lhs (f a1 ... an) of definition; body is rhs;
-- signature is (t0 t1 ... tn) where t0= target type, ti=type of ai, i > 0;

-- removes declarative and assignment information from form and
-- signature, placing it in list L, replacing form by ("where",form',:L),
-- signature by a list of NILs (signifying declarations are in e)
  $sigAlist: local := nil
  $predAlist: local := nil

-- 1. create sigList= list of all signatures which have embedded
--    declarations moved into global variable $sigAlist
  sigList:=
    [transformType(x) for a in rest form for x in rest signature]
       where
        transformType x ==
          atom x => x
          x is [":",R,Rtype] =>
            ($sigAlist:= [[R,:transformType Rtype],:$sigAlist]; x)
          x is ['Record,:.] => x --RDJ 8/83
          [first x,:[transformType y for y in rest x]]

-- 2. replace each argument of the form (|| x p) by x, recording
--    the given predicate in global variable $predAlist
  argList:=
    [removeSuchthat a for a in rest form] where
      removeSuchthat x ==
        x is ["|",y,p] =>
            BREAK()
            ($predAlist:= [[y,:p],:$predAlist]; y)
        x

  argList2 := [a for a in argList for t in sigList | not(NULL(t))]
  sigList2 := [t for t in sigList | not(NULL(t))]

-- 3. obtain a list of parameter identifiers (x1 .. xn) ordered so that
--       the type of xi is independent of xj if i < j
  varList:=
    orderByDependency(ASSOCLEFT argDepAlist,ASSOCRIGHT argDepAlist) where
      argDepAlist:=
        [[x,:dependencies] for [x,:y] in argSigAlist] where
          dependencies() ==
            union(listOfIdentifiersIn y,
              delete(x,listOfIdentifiersIn LASSOC(x,$predAlist)))
          argSigAlist:= [:$sigAlist,:pairList(argList2, sigList2)]

-- 4. construct a WhereList which declares and/or defines the xi's in
--    the order constructed in step 3
  (whereList:= [addSuchthat(x,[":",x,LASSOC(x,argSigAlist)]) for x in varList])
     where addSuchthat(x,y) == (p:= LASSOC(x,$predAlist) => ["|",y,p]; y)

-- 5. compile new ('DEF,("where",form',:WhereList),:.) where
--    all argument parameters of form' are bound/declared in WhereList
  comp(form',m,e) where
    form':=
      ["where",defform,:whereList] where
        defform:=
          ['DEF, form'', signature', body] where
            form'':= [first form,:argList]
            signature':= [first signature,:[nil for x in rest signature]]

orderByDependency(vl,dl) == vl

compInternalFunction(df is ['DEF, form, signature, body], m, e) ==
    -- FIXME: should correctly handle import in nested functions
    -- $CapsuleDomainsInScope : local := e
    [op, :argl] := form
    not(IDENTP(op)) =>
        stackAndThrow ['"Bad name for internal function:", op]
    nbody := ["+->", argl, body]
    fmode := ["Mapping", :signature]
    [., ., e'] := compMakeDeclaration([":", op, fmode], $EmptyMode, e)
    T := compWithMappingMode(nbody, fmode, e')
    T or return nil
    currentProplist := getProplist(op, e)
    finish_setq_single(T, fmode, op, nbody, currentProplist)

compDefineCapsuleFunction(df is ['DEF, form, signature, body],
  m,oldE,$prefix,$formalArgList) ==
    e := oldE
    --1. bind global variables
    $functionStats: local:= [0,0]
    $finalEnv: local := nil
             --used by ReplaceExitEtc to get a common environment
    $locVarsTypes: local := []
    $initCapsuleErrorCount: local:= #$semanticErrorStack
    $insideCapsuleFunctionIfTrue: local:= true
    $CapsuleModemapFrame: local:= e
    $CapsuleDomainsInScope: local:= get("$DomainsInScope","special",e)
    $iterate_tag : local := []
    $returnMode:= m
    $op: local := nil
    [$op,:argl]:= form
    $formalArgList:= [:argl,:$formalArgList]

    --let target and local signatures help determine modes of arguments
    argModeList:=
      identSig:= hasSigInTargetCategory(argl,form,first signature,e) =>
        (e:= checkAndDeclare(argl,form,identSig,e); rest identSig)
      [getArgumentModeOrMoan(a,form,e) for a in argl]
    signature':= [first signature,:argModeList]
    if null identSig then  --make $op a local function
      oldE := put($op,'mode,['Mapping,:signature'],oldE)

    --obtain target type if not given
    if null first signature' then signature':=
      identSig => identSig
      getSignature($op,rest signature',e) or return nil

    --replace ##1,.. in signature by arguments
--    pp signature'
--  pp '"------after----"
--  pp signature'
    e:= giveFormalParametersValues(argl,e)

    $signatureOfForm:= signature' --this global is bound in compCapsuleItems
    $functionLocations := [[[$op, signature']],
      :$functionLocations]
    e:= addDomain(first signature',e)

    --4. introduce needed domains into extendedEnv
    for domain in signature' repeat e:= addDomain(domain,e)

    --6. compile body in environment with extended environment
    rettype := resolve(signature'.target, $returnMode)

    localOrExported :=
      null member($op,$formalArgList) and
        getmode($op,e) is ['Mapping,:.] => 'local
      'exported

    --6a skip if compiling only certain items but not this one
    -- could be moved closer to the top
    formattedSig := formatUnabbreviated ['Mapping,:signature']
    sayBrightly ['"   compiling ",localOrExported,
      :bright $op,'": ",:formattedSig]

    T := CATCH('compCapsuleBody, compOrCroak(body,rettype,e))
           or [$ClearBodyToken, rettype, e]
--+
    NRTassignCapsuleFunctionSlot($op, signature', $domainShell, e)
    if $newCompCompare=true then
         SAY '"The old compiler generates:"
         prTriple T
--  A THROW to the above CATCH occurs if too many semantic errors occur
--  see stackSemanticError
    catchTag:= MKQ GENSYM()
    fun:=
      body':= replaceExitEtc(T.expr,catchTag,"TAGGEDreturn",$returnMode)
      finalBody:= ["CATCH",catchTag,body']
      do_compile([$op, ["LAMBDA", [:argl, '%], finalBody]], oldE)
    $functorStats:= addStats($functorStats,$functionStats)


--  7. give operator a 'value property
    [fun,['Mapping,:signature'],oldE] -- oldE:= put($op,'value,removeEnv val,e)

getSignatureFromMode(form,e) ==
  getmode(opOf form,e) is ['Mapping,:signature] =>
    #form~=#signature => stackAndThrow ['"Wrong number of arguments: ",form]
    EQSUBSTLIST(rest form,take(#rest form,$FormalMapVariableList),signature)

hasSigInTargetCategory(argl,form,opsig,e) ==
  mList:= [getArgumentMode(x,e) for x in argl]
    --each element is a declared mode for the variable or nil if none exists
  potentialSigList:=
    REMDUP
      [sig
        for [[opName,sig,:.],:.] in $domainShell.(1) |
          fn(opName,sig,opsig,mList,form)] where
            fn(opName,sig,opsig,mList,form) ==
              opName=$op and #sig=#form and (null opsig or opsig=first sig) and
                (and/[compareMode2Arg(x,m) for x in mList for m in rest sig])
  c:= #potentialSigList
  1=c => first potentialSigList
    --accept only those signatures op right length which match declared modes
  0=c => (#(sig:= getSignatureFromMode(form,e))=#form => sig; nil)
  1<c =>
    sig:= first potentialSigList
    stackWarning ['"signature of lhs not unique:",:bright sig,'"chosen"]
    sig
  nil --this branch will force all arguments to be declared

compareMode2Arg(x,m) == null x or modeEqual(x,m)

getArgumentModeOrMoan(x,form,e) ==
  getArgumentMode(x,e) or
    stackSemanticError(['"argument ",x,'" of ",form,'" is not declared"],nil)

getArgumentMode(x,e) ==
  STRINGP x => x
  m:= get(x,'mode,e) => m

checkAndDeclare(argl,form,sig,e) ==

-- arguments with declared types must agree with those in sig;
-- those that don't get declarations put into e
  for a in argl for m in rest sig repeat
    m1:= getArgumentMode(a,e) =>
      not modeEqual(m1,m) =>
        stack:= ['"   ",:bright a,'"must have type ",m,
          '" not ",m1,'%l,:stack]
    e:= put(a,'mode,m,e)
  if stack then
    sayBrightly ['"   Parameters of ",:bright first form,
      '" are of wrong type:",'%l,:stack]
  e

getSignature(op, argModeList, e) ==
  1=#
    (sigl:=
      REMDUP
        [sig for [[dc, :sig], [pred, :.]]
           in (mmList := get(op, 'modemap, e)) | dc = '% and
               rest sig=argModeList and known_in_env(pred, e)]) => first sigl
  null sigl =>
    (u := getmode(op, e)) is ['Mapping, :sig] => sig
    SAY '"************* USER ERROR **********"
    SAY('"available signatures for ",op,'": ")
    if null mmList
       then SAY '"    NONE"
       else for [[dc,:sig],:.] in mmList repeat printSignature('"     ",op,sig)
    printSignature('"NEED ",op,['"?",:argModeList])
    nil
  for u in sigl repeat
    for v in sigl | not (u=v) repeat
      if SourceLevelSubsume(u,v) then sigl:= delete(v,sigl)
              --before we complain about duplicate signatures, we should
              --check that we do not have for example, a partial - as
              --well as a total one.  SourceLevelSubsume (from CATEGORY BOOT)
              --should do this
  1=#sigl => first sigl
  stackSemanticError(['"duplicate signatures for ",op,'": ",argModeList],nil)


putInLocalDomainReferences (def := [opName,[lam,varl,body]]) ==
  $elt: local := ($QuickCode => 'QREFELT; 'ELT)
--+
  NRTputInTail CDDADR def
  def


isLocalFunction(op, e) ==
    null member(op, $formalArgList) and
        getmode(op, e) is ['Mapping, :.]

do_compile(u, e) ==
  [op,lamExpr] := u
  if $suffix then
    $suffix:= $suffix+1
    op':=
      opexport:=nil
      -- FIXME: Used only for side effect on opexport
      opmodes:=
        [sel
          for [[DC, :sig], [., sel]] in get(op, 'modemap, e) |
            DC = '% and (opexport := true) and
             (and/[modeEqual(x,y) for x in sig for y in $signatureOfForm])]
      isLocalFunction(op, e) =>
        if opexport then userError ['%b,op,'%d,'" is local and exported"]
        INTERN STRCONC(encodeItem $prefix, '";", encodeItem op)
      encodeFunctionName(op,$functorForm,$signatureOfForm,";",$suffix)
    u:= [op',lamExpr]
  optimizedBody:= optimizeFunctionDef u
  stuffToCompile:=
    if null $insideCapsuleFunctionIfTrue
       then optimizedBody
       else putInLocalDomainReferences optimizedBody
  $doNotCompileJustPrint=true => (PRETTYPRINT stuffToCompile; op')
  result:= spadCompileOrSetq stuffToCompile
  functionStats:=[0,elapsedTime()]
  $functionStats:= addStats($functionStats,functionStats)
  printStats functionStats
  result

spadCompileOrSetq (form is [nam,[lam,vl,body]]) ==
        --bizarre hack to take account of the existence of "known" functions
        --good for performance (LISPLLIB size, BPI size, NILSEC)
  CONTAINED($ClearBodyToken, body) =>
      sayBrightly ['"  ", :bright nam, '" not compiled"]
  if vl is [:vl',E] and body is [nam',: =vl'] then
      output_lisp_form(['PUT,MKQ nam,MKQ 'SPADreplace,MKQ nam'])
      sayBrightly ['"     ",:bright nam,'"is replaced by",:bright nam']
  else if (ATOM body or and/[ATOM x for x in body])
         and vl is [:vl',E] and not CONTAINED(E,body) then
           macform := ['XLAM,vl',body]
           output_lisp_form(['PUT,MKQ nam,MKQ 'SPADreplace,MKQ macform])
           sayBrightly ['"     ",:bright nam,'"is replaced by",:bright body]
  $insideCapsuleFunctionIfTrue => first COMP form
  compileConstructor form

compileConstructor form ==
  u:= compileConstructor1 form
  clearClams()                  --clear all CLAMmed functions
  u

compileConstructor1 (form:=[fn,[key,vl,:bodyl]]) ==
-- fn is the name of some category/domain/package constructor;
-- we will cache all of its values on $ConstructorCache with reference
-- counts
  auxfn := INTERNL1(fn, '";")
  output_lisp_form(["DECLAIM", ["NOTINLINE", auxfn]])
  if key = 'category_functor
      then u := compAndDefine form
      else u := COMP form
  clearConstructorCache fn      --clear cache for constructor
  first u

constructMacro (form is [nam,[lam,vl,body]]) ==
  not (and/[atom x for x in vl]) =>
    stackSemanticError(['"illegal parameters for macro: ",vl],nil)
  ["XLAM",vl':= [x for x in vl | IDENTP x],body]

uncons x ==
  atom x => x
  x is ["CONS",a,b] => [a,:uncons b]

--% CAPSULE

bootStrapError(functorForm,sourceFile) ==
  ['COND, _
    ['$bootStrapMode, _
        ['VECTOR,mkDomainConstructor functorForm,nil,nil,nil,nil,nil]],
    [''T, ['systemError, ['LIST, ''%b, MKQ first functorForm, ''%d, '"from", _
      ''%b,MKQ namestring sourceFile,''%d,'"needs to be compiled"]]]]

compAdd(['add,$addForm,capsule],m,e) ==
  addForm := $addForm
  $bootStrapMode = true =>
    if addForm is ["@Tuple", :.] then code := nil
       else [code, m, e]:= comp(addForm, m, e)
    [['COND, _
       ['$bootStrapMode, _
           code],_
       [''T, ['systemError, ['LIST, ''%b, MKQ first $functorForm, ''%d,
         '"from", ''%b, MKQ namestring($edit_file), ''%d, _
         '"needs to be compiled"]]]],
     m, e]
  $addFormLhs: local:= addForm
  if addForm is ["SubDomain", domainForm, predicate] then
--+
    $NRTaddForm := domainForm
    NRTgetLocalIndex(domainForm, e)
    --need to generate slot for add form since all $ go-get
    --  slots will need to access it
    [$addForm, m1, e] := compSubDomain1(domainForm, predicate, m, e)
  else
--+
    $NRTaddForm := addForm
    [$addForm, m1, e]:=
        addForm is ["@Tuple", :.] => BREAK()
        compOrCroak(addForm, $EmptyMode, e)
  not(isCategoryForm(m1)) or m1 = '(Category) =>
      userError(concat('"need domain before 'add', got", addForm,
                       '"of type", m1))
  compCapsule(capsule,m,e)

compTuple2Record u == ['Record,:[[":",i,x] for i in 1.. for x in rest u]]

compCapsule(['CAPSULE,:itemList],m,e) ==
  $bootStrapMode = true =>
      [bootStrapError($functorForm, $edit_file), m, e]
  compCapsuleInner(itemList, m, addDomain('%, e))

compSubDomain(["SubDomain",domainForm,predicate],m,e) ==
  $addFormLhs: local:= domainForm
  $addForm: local := nil
  $NRTaddForm := domainForm
  [$addForm,.,e]:= compSubDomain1(domainForm,predicate,m,e)
--+
  compCapsule(['CAPSULE],m,e)

compSubDomain1(domainForm,predicate,m,e) ==
  [.,.,e]:=
    compMakeDeclaration([":","#1",domainForm],$EmptyMode,addDomain(domainForm,e))
  u:=
    compOrCroak(predicate,$Boolean,e) or
      stackSemanticError(['"predicate: ",predicate,
        '" cannot be interpreted with #1: ",domainForm],nil)
  prefixPredicate:= lispize u.expr
  $lisplibSuperDomain:=
    [domainForm,predicate]
  evalAndRwriteLispForm('evalOnLoad2,
    ['SETQ,'$CategoryFrame,['put,op':= ['QUOTE,$op],'
     (QUOTE SuperDomain),dF':= ['QUOTE,domainForm],['put,dF','(QUOTE SubDomain),[
       'CONS,['QUOTE,[$op,:prefixPredicate]],['DELASC,op',['get,dF','
         (QUOTE SubDomain),'$CategoryFrame]]],'$CategoryFrame]]])
  [domainForm,m,e]

compCapsuleInner(itemList,m,e) ==
  e:= addInformation(m,e)
           --puts a new 'special' property of $Information
  data:= ["PROGN",:itemList]
      --RPLACd by compCapsuleItems and Friends
  e:= compCapsuleItems(itemList,nil,e)
  localParList:= $functorLocalParameters
  code:=
    $insideCategoryIfTrue and not $insideCategoryPackageIfTrue => BREAK()
    processFunctor($functorForm, $signature, data, localParList, e)
  [MKPF([code],"PROGN"),m,e]

--% PROCESS FUNCTOR CODE

processFunctor(form,signature,data,localParList,e) ==
  buildFunctor(form, signature, data, localParList, $domainShell, e)

compCapsuleItems(itemlist, $predl, e) ==
  $signatureOfForm: local := nil
  $suffix: local:= 0
  for item in itemlist repeat e := compSingleCapsuleItem(item, $predl, e)
  e

compSingleCapsuleItem(item, $predl, e) ==
  doIt(macroExpandInPlace(item, e), $predl, e)

doIt(item, $predl, e) ==
  $GENNO: local:= 0
  item is ['SEQ,:l,['exit,1,x]] =>
    RPLACA(item,"PROGN")
    RPLACA(LASTNODE item,x)
    for it1 in rest item repeat e := compSingleCapsuleItem(it1, $predl, e)
        --This will RPLAC as appropriate
    e
  isDomainForm(item, e) =>
     -- convert naked top level domains to import
    u:= ['import, [first item,:rest item]]
    userError ['"Use: import ", [first item,:rest item]]
    RPLACA(item,first u)
    RPLACD(item,rest u)
    doIt(item, $predl, e)
  item is [":=", lhs, rhs, :.] =>
    not (compOrCroak(item, $EmptyMode, e) is [code, ., e]) =>
      stackSemanticError(['"cannot compile assigned value to",:bright lhs],nil)
      e
    not (code is ['LET,lhs',rhs',:.] and atom lhs') =>
      code is ["PROGN",:.] =>
         stackSemanticError(['"multiple assignment ",item,'" not allowed"],nil)
         e
      RPLACA(item,first code)
      RPLACD(item,rest code)
      e
    lhs:= lhs'
    if not member(IFCAR rhs, $NonMentionableDomainNames) and
      not MEMQ(lhs, $functorLocalParameters) then
         $functorLocalParameters:= [:$functorLocalParameters,lhs]
    if code is ['LET, ., rhs', :.] and isDomainForm(rhs', e) then
      if lhs="Rep" then
        -- FIXME: $Representation is set unconditionally, but
        -- assignment to Rep may be conditional ...
        $Representation := (get("Rep", 'value, e)).(0)
           --$Representation bound by compDefineFunctor, used in compNoStacking
--+
--+
--+
    code is ['LET, :.] =>
      RPLACA(item,($QuickCode => 'QSETREFV;'SETELT))
      rhsCode:=
       rhs'
      RPLACD(item, ['%, NRTgetLocalIndex(lhs, e), rhsCode])
      e
    RPLACA(item,first code)
    RPLACD(item,rest code)
    e
  item is [":", a, t] =>
      [., ., e] := compOrCroak(item, $EmptyMode, e)
      e
  item is ['import,:doms] =>
     for dom in doms repeat
       sayBrightly ['"   importing ",:formatUnabbreviated dom]
     [., ., e] := compOrCroak(item, $EmptyMode, e)
     RPLACA(item,'PROGN)
     RPLACD(item,NIL) -- creates a no-op
     e
  item is ["IF", :.] => doItIf(item, $predl, e)
  item is ["where", b, :l] => doItWhere(item, $predl, e)
  item is ["MDEF", :.] =>
      [., ., e] := compOrCroak(item, $EmptyMode, e)
      e
  item is ['DEF,[op,:.],:.] =>
    [., ., e] := t := compOrCroak(item, $EmptyMode, e)
    RPLACA(item,"CodeDefine")
        --Note that DescendCode, in CodeDefine, is looking for this
    RPLACD(CADR item,[$signatureOfForm])
      --This is how the signature is updated for buildFunctor to recognise
--+
    functionPart:= ['dispatchFunction,t.expr]
    RPLACA(CDDR item,functionPart)
    RPLACD(CDDR item,nil)
    e
  u := compOrCroak(item, $EmptyMode, e) =>
    ([code, ., e] := u; RPLACA(item, first code); RPLACD(item, rest code))
    e
  true => cannotDo()

isMacro(x,e) ==
  x is ['DEF, [op, :args], signature, body] and
    null get(op,'modemap,e) and null args and null get(op,'mode,e)
      and signature is [nil] => body

-- FIXME: we ignore effects of computation of condition and
-- do not merge branches
doItIf(item is [., p, x, y], $predl, e) ==
    olde := e
    [p', ., e] := comp(p, $Boolean, e) or userError ['"not a Boolean:", p]
    if x ~= "noBranch" then
        compSingleCapsuleItem(x, $predl, getSuccessEnvironment(p, e))
    if y ~= "noBranch" then
        compSingleCapsuleItem(y, $predl, getInverseEnvironment(p, olde))
    RPLACA(item, "COND")
    RPLACD(item, [[p', x], ['(QUOTE T), y]])
    olde

doItWhere(item is [.,form,:exprList], $predl, eInit) ==
  $insideWhereIfTrue: local:= true
  e:= eInit
  u:=
    for it1 in exprList repeat
      e := compSingleCapsuleItem(it1, $predl, e)
  $insideWhereIfTrue:= false
  form1 := macroExpand(form, eBefore := e)
  eAfter := compSingleCapsuleItem(form1, $predl, e)
  eFinal:=
    del:= deltaContour(eAfter, eBefore) => addContour(del, eInit)
    eInit
  RPLACA(item, "PROGN")
  RPLACD(item, [["PROGN", :exprList], form1])
  eFinal


--% CATEGORY AND DOMAIN FUNCTIONS

compJoin(["Join",:argl],m,e) ==
  catList:= [(compForMode(x,$Category,e) or return 'failed).expr for x in argl]
  catList='failed => stackSemanticError(['"cannot form Join of: ",argl],nil)
  catList':=
    [extract for x in catList] where
      extract() ==
        x is ["Join", ["mkCategory",:y]] => ["mkCategory",:y]
        isCategoryForm(x) =>
          parameters:=
            union("append"/[getParms(y,e) for y in rest x],parameters)
              where getParms(y,e) ==
                atom y =>
                  isDomainForm(y,e) => LIST y
                  nil
                y is ['LENGTH,y'] =>
                  BREAK()
                  [y,y']
                LIST y
          x
        x is ["DomainSubstitutionMacro",pl,body] =>
            parameters := union(pl, parameters)
            body is ["Join", ["mkCategory",:y]] => ["mkCategory",:y]
            body
        x is ["mkCategory",:.] => x
        atom x and getmode(x,e)=$Category => x
        stackSemanticError(['"invalid argument to Join: ",x],nil)
        x
  T:= [wrapDomainSub(parameters,["Join",:catList']),$Category,e]
  convert(T,m)

compForMode(x,m,e) ==
  $compForModeIfTrue: local:= true
  comp(x,m,e)

compMakeCategoryObject(c, e) ==
  not isCategoryForm(c) => nil
  u := mkEvalableCategoryForm(c, e) => [c_eval u, $Category, e]
  nil

quotifyCategoryArgument x == MKQ x

makeCategoryForm(c,e) ==
  not isCategoryForm(c) => nil
  [x,m,e]:= compOrCroak(c,$EmptyMode,e)
  [x,e]

mk_acc() == [[], []]

push_at_list(ati, acc) == acc.1 := [ati, :acc.1]

get_at_list(acc) == acc.1

push_sig_list(sig, acc) == acc.0 := [sig, :acc.0]

get_sigs_list(acc) == acc.0

compCategory(x,m,e) ==
  (m:= resolve(m,["Category"]))=["Category"] and x is ['CATEGORY,
    domainOrPackage,:l] =>
      acc := mk_acc()
      for x in l repeat compCategoryItem(x, nil, acc)
      rep := mkExplicitCategoryFunction(get_sigs_list(acc), get_at_list(acc))
    --if inside compDefineCategory, provide for category argument substitution
      [rep,m,e]
  systemErrorHere '"compCategory"

mkExplicitCategoryFunction(sigList, atList) ==
  ["Join",
    ["mkCategory", ['LIST, :REVERSE sigList], ['LIST,
      :REVERSE atList], nil, nil]]

wrapDomainSub(parameters,x) ==
   ["DomainSubstitutionMacro",parameters,x]

DomainSubstitutionFunction(definition, parameters,body) ==
  --see optFunctorBody
  if parameters then
    (body:= Subst(definition, parameters,body)) where
      Subst(definition, parameters,body) ==
        ATOM body =>
          MEMQ(body,parameters) => MKQ body
          body
        member(body,parameters) =>
          g:=GENSYM()
          $extraParms:=PUSH([g,:body],$extraParms)
           --Used in SetVector12 to generate a substitution list
           --bound in buildFunctor
           --For categories, bound and used in compDefineCategory
          MKQ g
        first body="QUOTE" => body
        PAIRP definition and
            isFunctor first body and
              first body ~= first definition
          =>  ['QUOTE,optimize body]
        [Subst(definition, parameters,u) for u in body]
  not (body is ["Join",:.]) => body
  body is ["Join", ["mkCategory", :.]] => body
  atom definition => body
  null rest definition => body
           --should not bother if it will only be called once
  name := INTERN STRCONC(IFCAR definition, ";CAT")
  output_lisp_defparameter(name, nil)
  body:= ["COND",[name],['(QUOTE T),['SETQ,name,body]]]
  body

compCategoryItem(x, predl, acc) ==
  x is nil => nil
  --1. if x is a conditional expression, recurse; otherwise, form the predicate
  x is ["IF",a,b,c] =>
    predl':= [a,:predl]
    if b ~= "noBranch" then compCategoryItem(b, predl', acc)
    c="noBranch" => nil
    predl':= [["not",a],:predl]
    compCategoryItem(c, predl', acc)
  pred:= (predl => MKPF(predl,"AND"); true)

  --2. if attribute, push it and return
  x is ["ATTRIBUTE", 'nil] => BREAK()
  x is ["ATTRIBUTE", y] =>
       -- should generate something else for conditional categories
       -- BREAK()
       push_at_list(MKQ [y, pred], acc)

  --3. it may be a list, with PROGN as the CAR, and some information as the CDR
  x is ["PROGN", :l] => for u in l repeat compCategoryItem(u, predl, acc)

-- 4. otherwise, x gives a signature for a
--    single operator name or a list of names; if a list of names,
--    recurse
  ["SIGNATURE",op,:sig]:= x
  null atom op =>
      for y in op repeat compCategoryItem(["SIGNATURE", y, :sig], predl, acc)

  --4. branch on a single type or a signature with source and target
  push_sig_list(MKQ [rest x, pred], acc)
