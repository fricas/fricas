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

kePage(htPage) ==
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
  htMakePage [['bcLinks, [menuButton(), '"", 'dbShowOps, 'names]]]
  htSayStandard '"\tab{2}"
  if count ~= total then
    if count = 1
    then htSay('"1 name for ")
    else htSayList([STRINGIMAGE count, '" names for "])
  if total > 1
    then htSayList([STRINGIMAGE total, '" ", pluralize which,
                   '" are explicitly exported:"])
    else htSayList(['"1 ", which, '" is explicitly exported:"])
  data := dbGatherData(htPage, opAlist, 'names)
  dbShowOpItems(data, false)

ksPage(htPage) ==
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

kcpPage(htPage) ==
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

kcaPage(htPage) ==
    kcaPage1(htPage, '"category", '" an ", '"ancestor",
             function ancestorsOf, false)

kcdPage(htPage) ==
    kcaPage1(htPage, '"category", '" a ", '"descendant",
             function descendantsOf, true)

kcdoPage(htPage)==
    kcaPage1(htPage, '"domain", '" a ", '"descendant",
             function domainsOf, false)

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

kccPage(htPage) ==
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

kcdePage(htPage) ==
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

kcuPage(htPage) ==
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

kcnPage(htPage) ==
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

--=======================================================================
--           Operation Page from Main Page
--=======================================================================
koPage(htPage) ==
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
  htpSetProperty(htPage,'heading,heading)
  conform := htpProperty(htPage,'conform)
  opAlist := koOps(conform, domname)
  if selectedOperation := htpProperty(htPage,'selectedOperation) then
    opAlist := [assoc(selectedOperation,opAlist) or systemError nil]
  dbShowOperationsFromConform(htPage, opAlist)

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
         op = ["0"] => 0
         op = ["1"] => 1
         op
       for [sig,doc] in alist repeat
         HPUT($docTable,op1,[[conform,:alist],:HGET($docTable,op1)])
    --note opOf is needed!!! for some reason, One and Zero appear within prens

dbGetDocTable(op, $sig, docTable) == main where
--docTable is [[origin,entry1,...,:code] ...] where
--  each entry is [sig,doc] and code is NIL or else a topic code for op
  main == or/[gn(x) for x in HGET(docTable, op)]
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
        bcAbbTable(page, [[con, get_database(con, 'ABBREVIATION)]
                           for con in conlist])
    key = 'files => BREAK()
    key = 'documentation => dbShowConsDoc(page, conlist)
    if $exposedOnlyIfTrue then
      cAlist := [x for x in cAlist | isExposedConstructor opOf first x]
    key = 'conditions => dbShowConditions(page, cAlist, kind)
    key = 'parameters => bcConTable REMDUP ASSOCLEFT cAlist
    key = 'kinds => dbShowConsKinds cAlist
  dbConsExposureMessage()
  htSayStandard('"\endscroll ")
  dbPresentCons(page, kind, key)
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
        null cAlist => systemError nil
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

dbSelectCon(htPage, index) ==
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

