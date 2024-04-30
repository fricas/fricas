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
--              Pages Initiated from HyperDoc Pages
--=======================================================================

conPage(a) ==
  --The next 4 lines allow e.g. MATRIX INT  ==> Matrix Integer (see kPage)
  form :=
    atom a => [a]
    a
  $conArgstrings : local := [form2HtString x for x in IFCDR a]
  a := first form
  da := DOWNCASE a
  pageName := QLASSQ(da, '((type . CategoryType) (union . DomainUnion) _
                           (record . DomainRecord) (mapping . DomainMapping) _
                           (enumeration . DomainEnumeration))) =>
      downlink pageName       --special jump out for primitive domains
  line := conPageFastPath da  => kPage(line, [form]) --lower case name of cons?
  line := conPageFastPath UPCASE a => kPage(line, [form]) --upper case an abbr?
  ySearch a       --slow search (include default packages)

conPageFastPath x == --called by conPage and constructorSearch
--gets line quickly for constructor name or abbreviation
  s := STRINGIMAGE x
  charPosition(char '_*,s,0) < #s => nil     --quit if name has * in it
  name := (STRINGP x => INTERN x; x)
  entry := HGET($lowerCaseConTb,name) or return nil
  conPageConEntry first entry

conPageConEntry entry ==
  $conname: local := nil
  $conform: local := nil
  $exposed?:local := nil
  $doc:     local := nil
  $kind:    local := nil
  buildLibdbConEntry entry

kxPage(htPage,name) == downlink name

kArgPage(htPage,arg) ==
  [op,:args] := conform := htpProperty(htPage,'conform)
  domname := htpProperty(htPage,'domname)
  heading := htpProperty(htPage,'heading)
  source := CDDAR getConstructorModemap op
  n := position(arg,args)
  typeForm := sublisFormal(args,source . n)
  domTypeForm := mkDomTypeForm(typeForm,conform,domname)
  descendants := domainDescendantsOf(typeForm,domTypeForm)
  htpSetProperty(htPage,'cAlist,descendants)
  rank :=
    n > 4 => nil
    ('(First Second Third Fourth Fifth)).n
  htpSetProperty(htPage,'rank,rank)
  htpSetProperty(htPage,'thing,'"argument")
  dbShowCons(htPage,'names)

mkDomTypeForm(typeForm,conform,domname) == --called by kargPage
  domname => SUBLISLIS(rest domname,rest conform,typeForm)
  typeForm is ['Join,:r] => ['Join,:[mkDomTypeForm(t,conform,domname) for t in r]]
  null hasIdent typeForm => typeForm
  nil

domainDescendantsOf(conform,domform) == main where --called by kargPage
  main ==
    conform is [op,:r] =>
      op = 'Join => jfn(r, IFCDR domform)
      op = 'CATEGORY => nil
      domainsOf(conform,domform)
    domainsOf(conform,domform)
  jfn([y,:r],domlist) ==  --keep only those domains that appear in ALL parts of Join
    alist := domainsOf(y,IFCAR domlist)
    for x in r repeat
      domlist := IFCDR domlist
      x is ['CATEGORY,.,:r] => alist := catScreen(r,alist)
      keepList := nil
      for [item,:pred] in domainsOf(x,IFCAR domlist) repeat
        u := assoc(item,alist) =>
            keepList := [[item, :quickAnd(rest u, pred)], :keepList]
      alist := keepList
    for pair in alist repeat RPLACD(pair, simpHasPred rest pair)
    listSort(function GLESSEQP, alist)
  catScreen(r,alist) ==
    for x in r repeat
      x isnt [op1,:.] and MEMQ(op1,'(ATTRIBUTE SIGNATURE)) => systemError x
      alist := [[item,:npred] for [item,:pred] in alist |
        (pred1 := simpHasPred ['has,item,x]) and (npred := quickAnd(pred1,pred))]
    alist

--=======================================================================
--                   Branches of Constructor Page
--=======================================================================

kePage(htPage,junk) ==
  [kind,name,nargs,xflag,sig,args,abbrev,comments] := htpProperty(htPage,'parts)
  constring       := STRCONC(name,args)
  domname         := kDomainName(htPage,kind,name,nargs)
  domname is ['error,:.] => errorPage(htPage,domname)
  htpSetProperty(htPage,'domname,domname)
  $conformsAreDomains: local := domname
  conform         := mkConform(kind,name,args)
  conname         := opOf conform
  heading := [capitalize kind,'" {\sf ",
               (domname => form2HtString(domname,nil,true); constring),'"}"]
  data := sublisFormal(IFCDR domname or rest conform,
                       getConstructorExports((domname or conform),true))
  [conlist, :oplist] := data
  if domname then
    for x in conlist repeat  rplac(rest x, simpHasPred rest x)
    for x in oplist   repeat rplac(CDDR x, simpHasPred CDDR x)
  prefix := pluralSay(#conlist + #oplist, '"Export", '"Exports")
  page := htInitPage([:prefix,'" of ",:heading],htCopyProplist htPage)
  htSayStandard '"\beginmenu "
  htpSetProperty(page,'data,data)
  if conlist then
    htMakePage [['bcLinks,[menuButton(),'"",'dbShowCons1,conlist,'names]]]
    htSayStandard '"\tab{2}"
    htSay  '"All attributes and operations from:"
    bcConPredTable(conlist,opOf conform,rest conform)
  if oplist then
    if conlist then htBigSkip()
    kePageDisplay(page, kePageOpAlist(oplist))
  htSayStandard '" \endmenu "
  htShowPage()

kePageOpAlist oplist ==
  opAlist := nil
  for [op,sig,:pred] in oplist repeat
    u := LASSOC(op,opAlist)
    opAlist := insertAlist(zeroOneConvert op,[[sig,pred],:u],opAlist)
  opAlist

kePageDisplay(htPage, opAlist) ==
  count := #opAlist
  total := +/[#(rest entry) for entry in opAlist]
  count = 0 => nil
  htpSetProperty(htPage, 'opAlist, opAlist)
  htpSetProperty(htPage, 'expandOperations, 'lists)  --mark as unexpanded
  which := '"operation"
  htMakePage [['bcLinks,[menuButton(),'"",'dbShowOps,which,'names]]]
  htSayStandard '"\tab{2}"
  if count ~= total then
    if count = 1
    then htSay('"1 name for ")
    else htSayList([STRINGIMAGE count, '" names for "])
  if total > 1
    then htSayList([STRINGIMAGE total, '" ", pluralize which,
                   '" are explicitly exported:"])
    else htSayList(['"1 ", which, '" is explicitly exported:"])
  data := dbGatherData(htPage, opAlist, '"operation", 'names)
  dbShowOpItems(which,data,false)

ksPage(htPage,junk) ==
  [kind,name,nargs,xpart,sig,args,abbrev,comments] := htpProperty(htPage,'parts)
  domname         := kDomainName(htPage,kind,name,nargs)
  domname is ['error,:.] => errorPage(htPage,domname)
  heading :=
    null domname => htpProperty(htPage,'heading)
    ['"{\sf ",form2HtString(domname,nil,true),'"}"]
  if domname then
    htpSetProperty(htPage,'domname,domname)
    htpSetProperty(htPage,'heading,heading)
  domain  := (kind = '"category" => nil; EVAL domname)
  conform:= htpProperty(htPage,'conform)
  page := htInitPageNoScroll(htCopyProplist htPage,
                             ['"Search order for ",:heading])
  htSay '"When an operation is not defined by the domain, the following domains are searched in order for a _"default definition"
  htSayStandard '"\beginscroll "
  u := dbSearchOrder(conform,domname,domain)
  htpSetProperty(htPage,'cAlist,u)
  htpSetProperty(htPage,'thing,'"constructor")
  dbShowCons(htPage,'names)

dbSearchOrder(conform,domname,$domain) ==  --domain = nil or set to live domain
  conform := domname or conform
  name:= opOf conform
  infovec := dbInfovec name or return nil  --exit for categories
  u := infovec.3
  $predvec:=
        $domain => $domain.3
        get_database(name, 'PREDICATES)
  catpredvec := first u
  catinfo    := CADR u
  catvec     := CADDR u
  catforms := [[pakform,:pred] for i in 0..MAXINDEX catvec | test ] where
    test ==
      pred := simpCatPredicate
        p:=SUBLISLIS(rest conform,$FormalMapVariableList,kTestPred catpredvec.i)
        $domain => EVAL p
        p
      if domname and CONTAINED('%, pred) then pred := SUBST(domname, '%, pred)
      (pak := catinfo . i) and pred   --only those with default packages
    pakform ==
      pak and not IDENTP pak => devaluate pak --in case it has been instantiated
      catform := kFormatSlotDomain1(catvec.i, infovec)
      res := dbSubConform(rest(conform), [pak, "%", :rest(catform)])
      if domname then res := SUBST(domname, '%, res)
      res
  [:dbAddChain conform,:catforms]

kcpPage(htPage,junk) ==
  [kind,name,nargs,xpart,sig,args,abbrev,comments] := htpProperty(htPage,'parts)
  domname         := kDomainName(htPage,kind,name,nargs)
  domname is ['error,:.] => errorPage(htPage,domname)
  heading :=
    null domname => htpProperty(htPage,'heading)
    ['"{\sf ",form2HtString(domname,nil,true),'"}"]
  if domname then
    htpSetProperty(htPage,'domname,domname)
    htpSetProperty(htPage,'heading,heading)
  conform := htpProperty(htPage,'conform)
  conname := opOf conform
  page := htInitPage(['"Parents of ",:heading],htCopyProplist htPage)
  parents := parentsOf conname --was listSort(function GLESSEQP, =this)
  if domname then parents := SUBLISLIS(rest domname,rest conform,parents)
  htpSetProperty(htPage,'cAlist,parents)
  htpSetProperty(htPage,'thing,'"parent")
  choice :=
    domname => 'parameters
    'names
  dbShowCons(htPage,choice)

reduceAlistForDomain(alist,domform,conform) == --called from kccPage
  alist := SUBLISLIS(rest domform,rest conform,alist)
  for pair in alist repeat RPLACD(pair, simpHasPred2(rest pair, domform))
  [pair for (pair := [.,:pred]) in alist | pred]

kcaPage(htPage,junk) ==
  kcaPage1(htPage,'"category",'" an ",'"ancestor",function ancestorsOf, false)

kcdPage(htPage,junk) ==
  kcaPage1(htPage,'"category",'" a ",'"descendant",function descendantsOf,true)

kcdoPage(htPage,junk)==
  kcaPage1(htPage,'"domain",'" a ",'"descendant",function domainsOf, false)

kcaPage1(htPage,kind,article,whichever,fn, isCatDescendants?) ==
  [kind,name,nargs,xpart,sig,args,abbrev,comments] := htpProperty(htPage,'parts)
  domname         := kDomainName(htPage,kind,name,nargs)
  domname is ['error,:.] => errorPage(htPage,domname)
  heading :=
    null domname => htpProperty(htPage,'heading)
    ['"{\sf ",form2HtString(domname,nil,true),'"}"]
  if domname and not isCatDescendants? then
    htpSetProperty(htPage,'domname,domname)
    htpSetProperty(htPage,'heading,heading)
  conform := htpProperty(htPage,'conform)
  conname := opOf conform
  ancestors := FUNCALL(fn, conform, domname)
  if whichever ~= '"ancestor" then
    ancestors := augmentHasArgs(ancestors,conform)
  ancestors := listSort(function GLESSEQP,ancestors)
  -- if domname then ancestors := SUBST(domname, '%, ancestors)
  htpSetProperty(htPage,'cAlist,ancestors)
  htpSetProperty(htPage,'thing,whichever)
  choice :=
--  domname => 'parameters
    'names
  dbShowCons(htPage,choice)

kccPage(htPage,junk) ==
  [kind,name,nargs,xpart,sig,args,abbrev,comments] := htpProperty(htPage,'parts)
  domname         := kDomainName(htPage,kind,name,nargs)
  domname is ['error,:.] => errorPage(htPage,domname)
  heading :=
    null domname => htpProperty(htPage,'heading)
    ['"{\sf ",form2HtString(domname,nil,true),'"}"]
  if domname then
    htpSetProperty(htPage,'domname,domname)
    htpSetProperty(htPage,'heading,heading)
  conform := htpProperty(htPage,'conform)
  conname := opOf conform
  page := htInitPage(['"Children of ",:heading],htCopyProplist htPage)
  children:= augmentHasArgs(childrenOf conform,conform)
  if domname then children := reduceAlistForDomain(children,domname,conform)
  htpSetProperty(htPage,'cAlist,children)
  htpSetProperty(htPage,'thing,'"child")
  dbShowCons(htPage,'names)

augmentHasArgs(alist,conform) ==
  conname := opOf conform
  args    := IFCDR conform or return alist
  n       := #args
  [[name,:pred] for [name,:p] in alist] where pred ==
     extractHasArgs p is [a,:b] => p
     quickAnd(p, ['hasArgs, :TAKE(n, IFCDR getConstructorForm opOf name)])

kcdePage(htPage,junk) ==
  [kind,name,nargs,xflag,sig,args,abbrev,comments] := htpProperty(htPage,'parts)
  conname         := INTERN name
  constring       := STRCONC(name,args)
  conform         :=
    kind ~= '"default package" => ncParseFromString constring
    [INTERN name,:rest ncParseFromString STRCONC(char 'd,args)]  --because of &
  pakname         :=
--  kind = '"category" => INTERN STRCONC(name,char '_&)
    opOf conform
  domList := getDependentsOfConstructor pakname
  cAlist := [[getConstructorForm x,:true] for x in domList]
  htpSetProperty(htPage,'cAlist,cAlist)
  htpSetProperty(htPage,'thing,'"dependent")
  dbShowCons(htPage,'names)

kcuPage(htPage,junk) ==
  [kind,name,nargs,xflag,sig,args,abbrev,comments] := htpProperty(htPage,'parts)
  conname         := INTERN name
  constring       := STRCONC(name,args)
  conform         :=
    kind ~= '"default package" => ncParseFromString constring
    [INTERN name,:rest ncParseFromString STRCONC(char 'd,args)]  --because of &
  pakname         :=
    kind = '"category" => INTERN STRCONC(name,char '_&)
    opOf conform
  domList := getUsersOfConstructor pakname
  cAlist := [[getConstructorForm x,:true] for x in domList]
  htpSetProperty(htPage,'cAlist,cAlist)
  htpSetProperty(htPage,'thing,'"user")
  dbShowCons(htPage,'names)

kcnPage(htPage,junk) ==
--if reached by a category, that category has a default package
  [kind,name,nargs,xpart,sig,args,abbrev,comments] := htpProperty(htPage,'parts)
  domname         := kDomainName(htPage,kind,name,nargs)
  domname is ['error,:.] => errorPage(htPage,domname)
  heading :=
    null domname => htpProperty(htPage,'heading)
    ['"{\sf ",form2HtString(domname,nil,true),'"}"]
  if domname then
    htpSetProperty(htPage,'domname,domname)
    htpSetProperty(htPage,'heading,heading)
  conform:= htpProperty(htPage,'conform)
  pakname         :=
    kind = '"category" => INTERN STRCONC(PNAME conname,char '_&)
    opOf conform
  domList := getImports pakname
  if domname then
      domList := SUBLISLIS([domname, :rest(domname)],
                           ['%, :rest(conform)], domList)
  cAlist := [[x,:true] for x in domList]
  htpSetProperty(htPage,'cAlist,cAlist)
  htpSetProperty(htPage,'thing,'"benefactor")
  dbShowCons(htPage,'names)

koPageInputAreaUnchanged?(htPage, nargs) ==
  [htpLabelInputString(htPage,INTERN STRCONC('"*",STRINGIMAGE i)) for i in 1..nargs]
      = htpProperty(htPage,'inputAreaList)

kDomainName(htPage,kind,name,nargs) ==
  htpSetProperty(htPage,'domname,nil)
  inputAreaList :=
    [htpLabelInputString(htPage,var) for i in 1..nargs for var in $PatternVariableList]
  htpSetProperty(htPage,'inputAreaList,inputAreaList)
  conname := INTERN name
  args := [kArgumentCheck(domain?,x) or nil for x in inputAreaList
              for domain? in rest(get_database(conname, 'COSIG))]
  or/[null x for x in args] =>
    (n := +/[1 for x in args | x]) > 0 =>
      ['error,nil,'"\centerline{You gave values for only {\em ",n,'" } of the {\em ",#args,'"}}",'"\centerline{parameters of {\sf ",name,'"}}\vspace{1}\centerline{Please enter either {\em all} or {\em none} of the type parameters}"]
    nil
  argString :=
    null args => '"()"
    argTailPart :=
      "STRCONC"/["STRCONC"/ ['",", :x] for x in IFCDR args]
    "STRCONC"/['"(",:first args,argTailPart,'")"]
  typeForm := CATCH('SPAD_READER, unabbrev mkConform(kind, name, argString)) or
    ['error,'invalidType,STRCONC(name,argString)]
  null (evaluatedTypeForm := kisValidType typeForm) =>
    ['error,'invalidType,STRCONC(name,argString)]
  dbMkEvalable evaluatedTypeForm

kArgumentCheck(domain?,s) ==
  s = '"" => nil
  domain? and (form := conSpecialString? s) =>
    null IFCDR form => [STRINGIMAGE opOf form]
    form2String form
  [s]

dbMkEvalable form ==
--like mkEvalable except that it does NOT quote domains
--does not do "loadIfNecessary"
  [op,:.] := form
  kind := get_database(op, 'CONSTRUCTORKIND)
  kind = 'category => form
  mkEvalable form

topLevelInterpEval x ==
  $ProcessInteractiveValue: fluid := true
  $noEvalTypeMsg: fluid := true
  processInteractive(x,nil)

kisValidType typeForm ==
  $ProcessInteractiveValue: fluid := true
  $noEvalTypeMsg: fluid := true
  $BreakMode : local := 'throw_reader
  CATCH('SPAD_READER, processInteractive(typeForm, nil))
    is [[h,:.],:t] and member(h,'(Type Category)) => t
  false

parseNoMacroFromString(s) ==
   s := next(function ncloopParse,
        next(function lineoftoks,incString s))
   StreamNull s => nil
   pf2Sex first rest first s



mkConform(kind,name,argString) ==
  kind ~= '"default package" =>
    form := STRCONC(name,argString)
    parse := parseNoMacroFromString form
    null parse =>
      sayBrightlyNT '"Won't parse: "
      pp form
      systemError '"Keywords in argument list?"
    ATOM parse => [parse]
    parse
  [INTERN name,:rest ncParseFromString STRCONC(char 'd,argString)]  --& case

--=======================================================================
--           Operation Page for a Domain Form from Scratch
--=======================================================================
conOpPage(htPage,conform) ==
  updown := dbCompositeWithMap htPage
  updown = '"DOWN" =>
    domname := htpProperty(htPage,'domname)
    conOpPage1(dbExtractUnderlyingDomain domname,[['updomain,:domname]])
  domname := htpProperty(htPage,'updomain)
  conOpPage1(domname,nil)

dbCompositeWithMap htPage ==
  htpProperty(htPage,'updomain) => '"UP"
  domain := htpProperty(htPage,'domname)
  null domain => false
  opAlist := htpProperty(htPage,'opAlist)
--not LASSOC('map,opAlist) => false
  dbExtractUnderlyingDomain htpProperty(htPage,'domname) => '"DOWN"
  false

dbExtractUnderlyingDomain domain == or/[x for x in IFCDR domain | isValidType x]

--conform is atomic if no parameters, otherwise must be valid domain form
conOpPage1(conform, options) ==
--constructors    Cname\#\E\sig \args   \abb \comments (C is C, D, P, X)
  bindingsAlist := options
  conname       := opOf conform
  MEMQ(conname,$Primitives) =>
     dbSpecialOperations conname
  domname         :=                        --> !!note!! <--
    null atom conform => conform
    nil
  line := conPageFastPath conname
  [kind,name,nargs,xflag,sig,args,abbrev,comments]:=parts:= dbXParts(line,7,1)
  isFile := null kind
  kind := kind or '"package"
  RPLACA(parts,kind)
  constring       := STRCONC(name,args)
  conform         := mkConform(kind,name,args)
  capitalKind     := capitalize kind
  signature       := ncParseFromString sig
  emString        := ['"{\sf ",constring,'"}"]
  heading := [capitalKind,'" ",:emString]
  if not isExposedConstructor conname then heading := ['"Unexposed ",:heading]
  page := htInitPage(heading,nil)
  htpSetProperty(page,'isFile,true)
  htpSetProperty(page,'fromConOpPage1,true)
  htpSetProperty(page,'parts,parts)
  htpSetProperty(page,'heading,heading)
  htpSetProperty(page,'kind,kind)
  htpSetProperty(page,'domname,domname)         --> !!note!! <--
  htpSetProperty(page,'conform,conform)
  htpSetProperty(page,'signature,signature)
  for [a,:b] in bindingsAlist repeat htpSetProperty(page,a,b)
  koPage(page,'"operation")

--=======================================================================
--           Operation Page from Main Page
--=======================================================================
koPage(htPage,which) ==
  which = '"attribute" => BREAK()
  [kind,name,nargs,xflag,sig,args,abbrev,comments] := htpProperty(htPage,'parts)
  constring       := STRCONC(name,args)
  conname         := INTERN name
  domname         :=
    (u := htpProperty(htPage,'domname)) is [=conname,:.]
      and  (htpProperty(htPage,'fromConOpPage1) = true or
             koPageInputAreaUnchanged?(htPage,nargs)) => u
    kDomainName(htPage,kind,name,nargs)
  domname is ['error,:.] => errorPage(htPage,domname)
  htpSetProperty(htPage,'domname,domname)
  headingString :=
    domname => form2HtString(domname,nil,true)
    constring
  heading := [capitalize kind,'" {\sf ",headingString,'"}"]
  htpSetProperty(htPage,'which,which)
  htpSetProperty(htPage,'heading,heading)
  conform := htpProperty(htPage,'conform)
  opAlist := koOps(conform, domname)
  if selectedOperation := htpProperty(htPage,'selectedOperation) then
    opAlist := [assoc(selectedOperation,opAlist) or systemError()]
  dbShowOperationsFromConform(htPage,which,opAlist)

koaPageFilterByName(htPage,functionToCall) ==
  htpLabelInputString(htPage,'filter) = '"" =>
    koaPageFilterByCategory(htPage,functionToCall)
  filter := pmTransFilter(dbGetInputString htPage)
--WARNING: this call should check for ['error,:.] returned
  which   := htpProperty(htPage,'which)
  opAlist :=
      [x for x in htpProperty(htPage,'opAlist) | superMatch?(filter,DOWNCASE STRINGIMAGE first x)]
  htpSetProperty(htPage,'opAlist,opAlist)
  FUNCALL(functionToCall,htPage,nil)

--=======================================================================
--                  Get Constructor Documentation
--=======================================================================

dbDocTable conform ==
--assumes $docTableHash bound --see dbExpandOpAlistIfNecessary
  table := HGET($docTableHash,conform) => table
  $docTable : local := MAKE_HASHTABLE('EQ)
  --process in reverse order so that closest cover up farthest
  for x in originsInOrder conform repeat dbAddDocTable x
  dbAddDocTable conform
  HPUT($docTableHash,conform,$docTable)
  $docTable

originsInOrder conform ==  --domain = nil or set to live domain
--from dcCats
  [con,:argl] := conform
  get_database(con, 'CONSTRUCTORKIND) = 'category =>
      ASSOCLEFT ancestorsOf(conform,nil)
  acc := ASSOCLEFT parentsOf con
  for x in acc repeat
    for y in originsInOrder x repeat acc := insert(y,acc)
  acc

dbAddDocTable conform ==
  conname := opOf conform
  storedArgs := rest getConstructorForm conname
  for [op, :alist] in SUBLISLIS(["%", :rest(conform)],
    ["%", :storedArgs], get_database(opOf(conform), 'DOCUMENTATION))
      repeat
       op1 :=
         op = '(Zero) => 0
         op = '(One) => 1
         op
       for [sig,doc] in alist repeat
         HPUT($docTable,op1,[[conform,:alist],:HGET($docTable,op1)])
    --note opOf is needed!!! for some reason, One and Zero appear within prens

dbGetDocTable(op, $sig, docTable, which, aux) == main where
--docTable is [[origin,entry1,...,:code] ...] where
--  each entry is [sig,doc] and code is NIL or else a topic code for op
  main ==
    which = '"attribute" => BREAK()
    if null FIXP op and DIGITP (s := STRINGIMAGE op).0 then
          BREAK()
          op := string2Integer s
    -- the above hack should be removed after 3/94 when 0 is not |0|
    aux is [[packageName,:.],:pred] =>
      origin :=
        pred => ['ifp,:aux]
        first aux
      [origin]
    or/[gn x for x in HGET(docTable,op)]
  gn u ==  --u is [origin,entry1,...,:code]
    $conform := first u              --origin
    if ATOM $conform then $conform := [$conform]
    code     := LASTATOM u         --optional topic code
    comments := or/[p for entry in rest u | p := hn entry] or return nil
    [$conform,first comments,:code]
  hn [sig,:doc] ==
    pred := #$sig = #sig and
      alteredSig := SUBLISLIS(IFCDR $conform, $FormalMapVariableList, sig)
      alteredSig = $sig
    pred =>
      doc =>
        doc is ['constant,:r] => r
        doc
      '("")
    false

kTestPred n ==
  n = 0 => true
  $domain => testBitVector($predvec,n)
  simpHasPred $predvec.(n - 1)

dbAddChainDomain conform ==
  [name,:args] := conform
  infovec := dbInfovec(name) or return nil  --exit for categories
  template := infovec.0
  null (form := template . 5) => nil
  dbSubConform(args, kFormatSlotDomain1(devaluate(form), infovec))

dbSubConform(args,u) ==
  atom u =>
    (n := position(u,$FormalMapVariableList)) >= 0 => args . n
    u
  u is ['local,y] => dbSubConform(args,y)
  [dbSubConform(args,x) for x in u]

dbAddChain conform ==
  u := dbAddChainDomain conform =>
    atom u => nil
    [[u,:true],:dbAddChain u]
  nil

--=======================================================================
--                Constructor Page Menu
--=======================================================================

dbShowCons(htPage, key) ==
  cAlist  := htpProperty(htPage,'cAlist)
  key = 'filter =>
    filter := pmTransFilter(dbGetInputString(htPage))
    filter is ['error,:.] => bcErrorPage filter
    abbrev? := htpProperty(htPage,'exclusion) = 'abbrs
    u := [x for x in cAlist | test] where test ==
      conname := CAAR x
      subject := (abbrev? => constructor? conname; conname)
      superMatch?(filter,DOWNCASE STRINGIMAGE subject)
    null u => emptySearchPage('"constructor", filter, false)
    htPage := htInitPageNoHeading(htCopyProplist htPage)
    htpSetProperty(htPage,'cAlist,u)
    dbShowCons(htPage,htpProperty(htPage,'exclusion))
  if MEMQ(key,'(exposureOn exposureOff)) then
    $exposedOnlyIfTrue :=
      key = 'exposureOn => 'T
      NIL
    key := htpProperty(htPage,'exclusion)
  dbShowCons1(htPage,cAlist,key)

conPageChoose conname ==
  cAlist := [[getConstructorForm conname,:true]]
  dbShowCons1(nil,cAlist,'names)

dbShowCons1(htPage,cAlist,key) ==
  conlist := REMDUP [item for x in cAlist | pred] where
    pred ==
      item := first x
      $exposedOnlyIfTrue => isExposedConstructor opOf item
      item
  conlist is [.] => conPage
    htPage and htpProperty(htPage,'domname) => first conlist
    opOf first conlist
  conlist := [opOf x for x in conlist]
  kinds := "union"/[dbConstructorKind x for x in conlist]
  kind :=
    kinds is [a] => a
    'constructor
  proplist :=
    htPage => htCopyProplist htPage
    nil
  page := htInitPageNoScroll(proplist,dbConsHeading(htPage,conlist,key,kind))
  if u := htpProperty(page,'specialMessage) then APPLY(first u,rest u)
  htSayStandard('"\beginscroll ")
  htpSetProperty(page,'cAlist,cAlist)
  $conformsAreDomains: local := htpProperty(page,'domname)
  do
  --key = 'catfilter => dbShowCatFilter(page,key)
    key = 'names => bcNameConTable conlist
    key = 'abbrs =>
        bcAbbTable(page, [[con ,get_database(con, 'ABBREVIATION)]
                          for con in conlist])
    key = 'files => BREAK()
    key = 'documentation   => dbShowConsDoc(page,conlist)
    if $exposedOnlyIfTrue then
      cAlist := [x for x in cAlist | isExposedConstructor opOf first x]
    key = 'conditions =>     dbShowConditions(page,cAlist,kind)
    key = 'parameters => bcConTable REMDUP ASSOCLEFT cAlist
    key = 'kinds => dbShowConsKinds cAlist
  dbConsExposureMessage()
  htSayStandard('"\endscroll ")
  dbPresentCons(page,kind,key)
  htShowPageNoScroll()


dbConsExposureMessage() ==
  $atLeastOneUnexposed =>
      htSay '"\newline{}-------------\newline{}{\em *} = unexposed"


dbShowConsKindsFilter(htPage,[kind,cAlist]) ==
  htpSetProperty(htPage,'cAlist,cAlist)
  dbShowCons(htPage,htpProperty(htPage,'exclusion))

dbShowConsDoc(htPage,conlist) ==
  null rest conlist => dbShowConsDoc1(htPage,getConstructorForm opOf first conlist,nil)
  cAlist := htpProperty(htPage,'cAlist)
  --the following code is necessary to skip over duplicates on cAlist
  index := 0
  for x in REMDUP conlist repeat
  -- for x in conlist repeat
    dbShowConsDoc1(htPage,getConstructorForm x,i) where i ==
      while CAAAR cAlist ~= x repeat
        index := index + 1
        cAlist := rest cAlist
        null cAlist => systemError ()
      index

dbShowConsDoc1(htPage,conform,indexOrNil) ==
  [conname,:conargs] := conform
  MEMQ(conname,$Primitives) =>
      [["constructor", ["NIL", doc]], :.] := GET(conname, 'documentation)
      sig :=
         conname = 'Enumeration =>
             '((CATEGORY domain) (Symbol) (Symbol))
         '((CATEGORY domain) (SetCategory) (SetCategory))
      displayDomainOp(htPage, '"constructor", conform, conname, sig, true,
                      doc, indexOrNil, 'dbSelectCon, nil, nil)
  exposeFlag := isExposedConstructor conname
  doc := [getConstructorDocumentation conname]
  signature := getConstructorSignature conname
  sig :=
    get_database(conname, 'CONSTRUCTORKIND) = 'category =>
      SUBLISLIS(conargs,$TriangleVariableList,signature)
    sublisFormal(conargs,signature)
  displayDomainOp(htPage,'"constructor",conform,conname,sig,true,doc,indexOrNil,'dbSelectCon,null exposeFlag,nil)
  --NOTE that we pass conform is as "origin"

getConstructorDocumentation conname ==
  LASSOC('constructor, get_database(conname, 'DOCUMENTATION))
    is [[nil,line,:.],:.] and line or '""

dbSelectCon(htPage,which,index) ==
  conPage opOf first (htpProperty(htPage, 'cAlist)) . index

dbShowConditions(htPage,cAlist,kind) ==
  conform := htpProperty(htPage,'conform)
  conname := opOf conform
  article := htpProperty(htPage,'article)
  whichever := htpProperty(htPage,'whichever)
  [consNoPred,:consPred] := splitConTable cAlist
  singular := [kind,'" is"]
  plural   := [pluralize STRINGIMAGE kind,'" are"]
  dbSayItems(#consNoPred,singular,plural,'" unconditional")
  bcConPredTable(consNoPred,conname)
  htSayHrule()
  dbSayItems(#consPred,singular,plural,'" conditional")
  bcConPredTable(consPred,conname)

dbConsHeading(htPage,conlist,view,kind) ==
  thing := htPage and htpProperty(htPage,'thing) or '"constructor"
  place :=
    htPage => htpProperty(htPage,'domname) or htpProperty(htPage,'conform)
    nil
  count := #(REMDUP conlist)
  -- count := #conlist
  thing = '"benefactor" =>
    [STRINGIMAGE count,'" Constructors Used by ",form2HtString(place,nil,true)]
  modifier :=
    thing = '"argument" =>
      rank := htPage and htpProperty(htPage,'rank)
      ['" Possible ",rank,'" "]
    kind = 'constructor => ['" "]
    ['" ",capitalize STRINGIMAGE kind,'" "]
--  count = 1 =>
--    ['"Select name or a {\em view} at the bottom"]
  exposureWord :=
    $exposedOnlyIfTrue => '(" Exposed ")
    nil
  prefix :=
    count = 1 => [STRINGIMAGE count,:modifier,capitalize thing]
    firstWord := (count = 0 => '"No "; STRINGIMAGE count)
    [firstWord,:exposureWord, :modifier,capitalize pluralize thing]
  placepart :=
    place => ['" of {\em ",form2HtString(place,nil,true),"}"]
    nil
  heading := [:prefix,:placepart]
  connective :=
    member(view,'(abbrs files kinds)) => '" as "
    '" with "
  if count ~= 0 and member(view,'(abbrs files parameters conditions)) then heading:= [:heading,'" viewed",connective,'"{\em ",STRINGIMAGE view,'"}"]
  heading

dbShowConstructorLines lines ==
  cAlist := [[getConstructorForm intern dbName line,:true] for line in lines]
  dbShowCons1(nil,listSort(function GLESSEQP,cAlist),'names)

isAsharpFileName? con == false

--=======================================================================
--             Special Code for Union, Mapping, and Record
--=======================================================================

dbSpecialOperations(conname) ==
  page := htInitPage(nil,nil)
  conform := getConstructorForm conname
  opAlist := dbSpecialExpandIfNecessary(conform,rest GETL(conname,'documentation))
  fromHeading := ['" from domain {\sf ",form2HtString conform,'"}"]
  htpSetProperty(page,'fromHeading,fromHeading)
  htpSetProperty(page,'conform,conform)
  htpSetProperty(page,'opAlist,opAlist)
  htpSetProperty(page,'noUsage,true)
  htpSetProperty(page,'condition?,'no)
  dbShowOp1(page,opAlist,'"operation",'names)

dbSpecialExpandIfNecessary(conform,opAlist) ==
  opAlist is [[op,[sig,:r],:.],:.] and rest r => opAlist
  for [op,:u] in opAlist repeat
    for pair in u repeat
      [sig,comments] := pair
      RPLACD(pair,['T,conform,'T,comments]) --[sig,pred,origin,exposeFg,doc]
  opAlist

PUT('Record,'documentation,'(
  (constructor (NIL NIL))
  (_=  (((Boolean) _% _%)
   "\spad{r = s} tests for equality of two records \spad{r} and \spad{s}"))
  (coerce (((OutputForm) _%)
   "\spad{coerce(r)} returns an representation of \spad{r} as an output form")
         ((_% (List (Any)))
   "\spad{coerce(u)}, where \spad{u} is the list \spad{[x,y]} for \spad{x} of type \spad{A} and \spad{y} of type \spad{B}, returns the record \spad{[a:x,b:y]}"))
  (construct ((_% A B)
   "\spad{construct(x, y)} returns the record \spad{[a:x,b:y]}"))
  (elt ((A % "a")
   "\spad{r . a} returns the value stored in record \spad{r} under selector \spad{a}.")
      ((B % "b")
   "\spad{r . b} returns the value stored in record \spad{r} under selector \spad{b}."))
  (setelt_! ((A % "a" A)
   "\spad{r . a := x} destructively replaces the value stored in record \spad{r} under selector \spad{a} by the value of \spad{x}. Error: if \spad{r} has not been previously assigned a value.")
         ((B % "b" B)
   "\spad{r . b := y} destructively replaces the value stored in record \spad{r} under selector \spad{b} by the value of \spad{y}. Error: if \spad{r} has not been previously assigned a value."))
   ))

PUT('UntaggedUnion, 'documentation, '(
  (constructor (NIL NIL))
  (_=  (((Boolean) % %)
    "\spad{u = v} tests if two objects of the union are equal, that is, u and v are hold objects of same branch which are equal."))
  (case (((Boolean) % "A")
    "\spad{u case A} tests if \spad{u} is of the type \spad{A} branch of the union.")
        (((Boolean) % "B")
    "\spad{u case B} tests if \spad{u} is of the \spad{B} branch of the union."))
  (coerce ((A %)
    "\spad{coerce(u)} returns \spad{x} of type \spad{A} if \spad{x} is of the \spad{A} branch of the union. Error: if \spad{u} is of the \spad{B} branch of the union.")
          ((B %)
    "\spad{coerce(u)} returns \spad{x} of type \spad{B} if \spad{x} is of the \spad{B} branch of the union. Error: if \spad{u} is of the \spad{A} branch of the union.")
          ((% A)
    "\spad{coerce(x)}, where \spad{x} has type \spad{A}, returns \spad{x} as a union type.")
          ((% B)
    "\spad{coerce(y)}, where \spad{y} has type \spad{B}, returns \spad{y} as a union type."))
  ))

PUT('Union, 'documentation, '(
  (constructor (NIL NIL))
  (_=  (((Boolean) % %)
    "\spad{u = v} tests if two objects of the union are equal, that is, \spad{u} and \spad{v} are objects of same branch which are equal."))
  (case (((Boolean) % "A")
    "\spad{u case a} tests if \spad{u} is of branch \spad{a} of the union.")
                (((Boolean) % "B")
    "\spad{u case b} tests if \spad{u} is of branch \spad{b} of the union."))
  (coerce ((A %)
    "\spad{coerce(u)} returns \spad{x} of type \spad{A} if \spad{x} is of branch \spad{a} of the union. Error: if \spad{u} is of branch \spad{b} of the union.")
          ((B %)
    "\spad{coerce(u)} returns \spad{x} of type \spad{B} if \spad{x} is of branch \spad{b} branch of the union. Error: if \spad{u} is of the \spad{a} branch of the union.")
          ((% A)
    "\spad{coerce(x)}, where \spad{x} has type \spad{A}, returns \spad{x} as a union type.")
          ((% B)
    "\spad{coerce(y)}, where \spad{y} has type \spad{B}, returns \spad{y} as a union type."))
  ))

PUT('Mapping, 'documentation, '(
  (constructor (NIL NIL))
  (_=  (((Boolean) % %)
    "\spad{u = v} tests if mapping objects are equal."))
   ))

PUT('Enumeration, 'documentation, '(
        (constructor (NIL NIL))
  (_= (((Boolean) _% _%)
    "\spad{e = f} tests for equality of two enumerations \spad{e} and \spad{f}"))
  (_^_= (((Boolean) _% _%)
    "\spad{e ~= f} tests that two enumerations \spad{e} and \spad{f} are nont equal"))
  (coerce (((OutputForm) _%)
     "\spad{coerce(e)} returns a representation of enumeration \spad{r} as an output form")
          ((_% (Symbol))
     "\spad{coerce(s)} converts a symbol \spad{s} into an enumeration which has \spad{s} as a member symbol"))
  ))
