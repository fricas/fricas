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

--====================> WAS b-saturn.boot <================================
-- New file as of 6/95
$atLeastOneUnexposed := false

page() == $curPage

--=======================================================================
--            Functions that affect $saturnPage
--=======================================================================

--------------------> OLD DEFINITION (override in br-util.boot)
htSay(x) ==
    bcHt(x)

htSayCold x ==
  htSay '"\lispLink{}{"
  htSay x
  htSay '"}"

htSayStandard(x) ==  --do AT MOST for $standard
    bcHt(x)

htSayStandardList(lx) ==
    htSayList(lx)

htSayList(lx) ==
  for x in lx repeat bcHt(x)

--------------------> NEW DEFINITION (override in ht-util.boot)
bcHt line ==
  $newPage =>  --this path affects both saturn and old lines
    text :=
      PAIRP line => [['text, :line]]
      STRINGP line => line
      [['text, line]]
    htpAddToPageDescription($curPage, text)
  PAIRP line =>
    $htLineList := NCONC(nreverse mapStringize COPY_-LIST line, $htLineList)
  $htLineList := [basicStringize line, :$htLineList]

--=======================================================================
--                        New issueHT
--=======================================================================

--------------------> NEW DEFINITION (see ht-util.boot)
htShowPage() ==
-- show the page which has been computed
  htSayStandard '"\endscroll"
  htShowPageNoScroll()

htShowPageNoScroll() ==
-- show the page which has been computed
  htSayStandard '"\autobuttons"
  htpSetPageDescription($curPage, nreverse htpPageDescription $curPage)
  $newPage := false
  $htLineList := nil
  htMakePage htpPageDescription $curPage
  if $htLineList then line := concatenateStringList(nreverse $htLineList)
  issueHTStandard line
  endHTPage()


issueHTStandard line == --called by htMakePageNoScroll and htMakeErrorPage
    sockSendInt($MenuServer, $SendLine)
    sockSendString($MenuServer, line)

--------------------> NEW DEFINITION (override in ht-util.boot)
htMakeErrorPage htPage ==
  $newPage := false
  $htLineList := nil
  $curPage := htPage
  htMakePage htpPageDescription htPage
  line := concatenateStringList(nreverse $htLineList)
  issueHT line
  endHTPage()

--=======================================================================
--            htMakePage and friends
--=======================================================================

--------------------> NEW DEFINITION (override in ht-util.boot)
htMakePage itemList ==
  if $newPage then
     htpAddToPageDescription($curPage, itemList)
  htMakePage1 itemList

--------------------> NEW DEFINITION (override in ht-util.boot)
htMakePage1 itemList ==
-- make a page given the description in itemList
  for u in itemList repeat
    itemType := 'text
    items :=
      STRINGP u => u
      ATOM u => STRINGIMAGE u
      STRINGP first u => u
      u is ['text, :s] => s
      itemType := first u
      rest u
    itemType = 'text              => iht items
    itemType = 'lispLinks         => htLispLinks items
    itemType = 'lispmemoLinks     => htLispMemoLinks items
    itemType = 'bcLinks           => htBcLinks(items)
    itemType = 'bcLispLinks       => htBcLispLinks items           --->
    itemType = 'radioButtons      => htRadioButtons items
    itemType = 'bcRadioButtons    => htBcRadioButtons items
    itemType = 'inputStrings      => htInputStrings items
    itemType = 'domainConditions  => htProcessDomainConditions items
    itemType = 'bcStrings         => htProcessBcStrings items
    itemType = 'toggleButtons     => htProcessToggleButtons items
    itemType = 'bcButtons         => htProcessBcButtons items
    itemType = 'doneButton        => htProcessDoneButton items
    itemType = 'doitButton        => htProcessDoitButton items
    systemError '"unexpected branch"

menuButton() == '"\menuitemstyle{}"


endHTPage() ==
    sockSendInt($MenuServer, $EndOfPage)

htSayHrule() == bcHt
  '"\horizontalline{}\newline{}"

htpAddInputAreaProp(htPage, label, prop) ==
  SETELT(htPage, 5, [[label, nil, nil, nil, :prop], :ELT(htPage, 5)])

htpSetLabelInputString(htPage, label, val) ==
  -- value user typed as input string on page
  props := LASSOC(label, htpInputAreaAlist htPage)
  props => SETELT(props, 0, STRINGIMAGE val)
  nil

--------------------> NEW DEFINITION (override in ht-util.boot)
htDoneButton(func, htPage, :optionalArgs) ==
------> Handle argument values passed from page if present
  if optionalArgs then
    htpSetInputAreaAlist(htPage, first optionalArgs)
  typeCheckInputAreas htPage =>
    htMakeErrorPage htPage
  NULL FBOUNDP func =>
    systemError ['"unknown function", func]
  FUNCALL(SYMBOL_-FUNCTION func, htPage)

--------------------> NEW DEFINITION (override in ht-util.boot)
htBcLinks(links) ==
  [links,options] := beforeAfter('options,links)
  for [message, info, func, :value] in links repeat
    link := '"\lispdownlink"
    htMakeButton(link, message, mkCurryFun(func, value))
    bcIssueHt info

--------------------> NEW DEFINITION (override in ht-util.boot)
htBcLispLinks links ==
  [links,options] := beforeAfter('options,links)
  for [message, info, func, :value] in links repeat
    link :=
      '"\lisplink"
    htMakeButton(link ,message, mkCurryFun(func, value))
    bcIssueHt info

htMakeButton(htCommand, message, func) ==
  iht [htCommand, '"{"]
  bcIssueHt message
  iht ['"}{(|htDoneButton| '|", func, '"| (PROGN "]
  for [id, ., ., ., type, :.] in htpInputAreaAlist $curPage repeat
    iht ['"(|htpSetLabelInputString| ", htpName $curPage, '"'|", id, '"| "]
    if type = 'string then
      iht ['"_"\stringvalue{", id, '"}_""]
    else
      iht ['"_"\boxvalue{", id, '"}_""]
    iht '") "
  iht [htpName $curPage, '"))}"]

htpAddToPageDescription(htPage, pageDescrip) ==
  newDescript :=
    STRINGP pageDescrip => [pageDescrip, :ELT(htPage, 7)]
    nconc(nreverse COPY_-LIST pageDescrip, ELT(htPage, 7))
  SETELT(htPage, 7, newDescript)


--------------------> NEW DEFINITION (override in ht-util.boot)
htProcessBcStrings strings ==
  for [numChars, default, stringName, spadType, :filter] in strings repeat
    mess2 := '""
    if NULL LASSOC(stringName, htpInputAreaAlist page()) then
      setUpDefault(stringName, ['string, default, spadType, filter])
    if htpLabelErrorMsg(page(), stringName) then
      iht ['"\centerline{{\em ", htpLabelErrorMsg(page(), stringName), '"}}"]
      mess2 := CONCAT(mess2, bcSadFaces())
      htpSetLabelErrorMsg(page(), stringName, nil)
    iht ['"\inputstring{", stringName, '"}{",
         numChars, '"}{", htpLabelDefault(page(),stringName), '"} ", mess2]

--------------------> NEW DEFINITION (override in ht-util.boot)
setUpDefault(name, props) ==
  htpAddInputAreaProp(page(), name, props)

--------------------> NEW DEFINITION (override in ht-util.boot)
htInitPage(title, propList) ==
-- start defining a hyperTeX page
    page := htInitPageNoScroll(propList, title)
    htSayStandard '"\beginscroll "
    page

--------------------> NEW DEFINITION <--------------------------
htInitPageNoScroll(propList, title) ==
--start defining a hyperTeX page
    page := htInitPageNoHeading(propList)
    htSayStandard ['"\begin{page}{", htpName page, '"}{"]
    htSay title
    htSayStandard '"} "
    page

--------------------> NEW DEFINITION <--------------------------
htInitPageNoHeading(propList) ==
--start defining a hyperTeX page
  $atLeastOneUnexposed := nil
  page := htpMakeEmptyPage(propList)
  $curPage := page
  $newPage := true
  $htLineList := nil
  page

--------------------> NEW DEFINITION <--------------------------
htpMakeEmptyPage(propList) ==
  name := GENTEMP()
  $activePageList := [name, :$activePageList]
  SET(name, val := VECTOR(name, nil, nil, nil, nil, nil, propList, nil))
  val

--=======================================================================
--              Redefinitions from br-con.boot
--=======================================================================
kPage(line, options) == --any cat, dom, package, default package
--constructors    Cname\#\E\sig \args   \abb \comments (C is C, D, P, X)
  parts := dbXParts(line,7,1)
  [kind,name,nargs,xflag,sig,args,abbrev,comments] := parts
  form := IFCAR options
  isFile := null kind
  kind := kind or '"package"
  RPLACA(parts,kind)
  conform         := mkConform(kind,name,args)
  conname         := opOf conform
  capitalKind     := capitalize kind
  signature       := ncParseFromString sig
  sourceFileName  := dbSourceFile INTERN name
  constrings      :=
    IFCDR form => dbConformGenUnder form
    [STRCONC(name,args)]
  emString        := ['"{\sf ",:constrings,'"}"]
  heading := [capitalKind,'" ",:emString]
  if not isExposedConstructor conname then heading := ['"Unexposed ",:heading]
  if name=abbrev then abbrev := asyAbbreviation(conname,nargs)
  page := htInitPageNoHeading(nil)
  htAddHeading heading
  htSayStandard("\beginscroll ")
  htpSetProperty(page,'isFile,true)
  htpSetProperty(page,'parts,parts)
  htpSetProperty(page,'heading,heading)
  htpSetProperty(page,'kind,kind)
  if asharpConstructorName? conname then
    htpSetProperty(page,'isAsharpConstructor,true)
  htpSetProperty(page,'conform,conform)
  htpSetProperty(page,'signature,signature)
  ---what follows is stuff from kiPage with domain = nil
  $conformsAreDomains := nil
  dbShowConsDoc1(page,conform,nil)
  if kind ~= 'category and nargs > 0 then addParameterTemplates(page,conform)
  if $atLeastOneUnexposed then htSay '"\newline{}{\em *} = unexposed"
  htSayStandard("\endscroll ")
  kPageContextMenu page
  htShowPageNoScroll()

kPageContextMenu page ==
  [kind,name,nargs,xpart,sig,args,abbrev,comments] := htpProperty(page,'parts)
  conform := htpProperty(page,'conform)
  conname := opOf conform
  htBeginTable()
  htSay '"{"
  htMakePage [['bcLinks,['Ancestors,'"",'kcaPage,nil]]]
  if kind = '"category" then
    htSay '"}{"
    htMakePage [['bcLinks,['Children,'"",'kccPage,nil]]]
  if not asharpConstructorName? conname then
    htSay '"}{"
    htMakePage [['bcLinks,['Dependents,'"",'kcdePage,nil]]]
  if kind = '"category" then
    htSay '"}{"
    htMakePage [['bcLinks,['Descendents,'"",'kcdPage,nil]]]
  if kind = '"category" then
    htSay '"}{"
    if not asharpConstructorName? conname then
      htMakePage [['bcLinks,['Domains,'"",'kcdoPage,nil]]]
    else htSay '"{\em Domains}"
  htSay '"}{"
  if kind ~= '"category" and (pathname := dbHasExamplePage conname)
    then htMakePage [['bcLinks,['Examples,'"",'kxPage,pathname]]]
    else htSay '"{\em Examples}"
  htSay '"}{"
  htMakePage [['bcLinks,['Exports,'"",'kePage,nil]]]
  htSay '"}{"
  htMakePage [['bcLinks,['Operations,'"",'koPage,'"operation"]]]
  htSay '"}{"
  htMakePage [['bcLinks,['Parents,'"",'kcpPage,'"operation"]]]
  if kind ~= '"category" then
    htSay '"}{"
    if not asharpConstructorName? conname
    then  htMakePage [['bcLinks,["Search Path",'"",'ksPage,nil]]]
    else htSay '"{\em Search Path}"
  if kind ~= '"category" then
    htSay '"}{"
    htMakePage [['bcLinks,['Users,'"",'kcuPage,nil]]]
    htSay '"}{"
    htMakePage [['bcLinks,['Uses,'"",'kcnPage,nil]]]
  htSay '"}"
  htEndTable()

--------------------> NEW DEFINITION (see br-con.boot)
dbPresentCons(htPage,kind,:exclusions) ==
  htpSetProperty(htPage,'exclusion,first exclusions)
  cAlist := htpProperty(htPage,'cAlist)
  empty? := null cAlist
  one?   := null rest cAlist
  one? := empty? or one?
  exposedUnexposedFlag := $includeUnexposed? --used to be star?       4/92
  star?  := true     --always include information on exposed/unexposed   4/92
  htBeginTable()
  htSay '"{"
  if one? or member('abbrs,exclusions)
    then htSay '"{\em Abbreviations}"
    else htMakePage [['bcLispLinks,['"Abbreviations",'"",'dbShowCons,'abbrs]]]
  htSay '"}{"
  if one? or member('conditions, exclusions) or
        and/[rest x = true for x in cAlist]
    then htSay '"{\em Conditions}"
    else htMakePage [['bcLispLinks,['"Conditions",'"",'dbShowCons,'conditions]]]
  htSay '"}{"
  if empty? or member('documentation,exclusions)
    then htSay '"{\em Descriptions}"
    else htMakePage [['bcLispLinks,['"Descriptions",'"",'dbShowCons,'documentation]]]
  htSay '"}{"
  if one? or null rest cAlist
    then htSay '"{\em Filter}"
    else htMakePage
      [['bcLinks,['"Filter",'"",'htFilterPage,['dbShowCons,'filter]]]]
  htSay '"}{"
  if one? or member('kinds,exclusions) or kind ~= 'constructor
    then htSay '"{\em Kinds}"
    else htMakePage [['bcLispLinks,['"Kinds",'"",'dbShowCons,'kinds]]]
  htSay '"}{"
  if one? or member('names,exclusions)
    then htSay '"{\em Names}"
    else htMakePage [['bcLispLinks,['"Names",'"",'dbShowCons,'names]]]
  htSay '"}{"
  if one? or member('parameters, exclusions) or not(or/[CDAR x for x in cAlist])
    then htSay '"{\em Parameters}"
    else htMakePage [['bcLispLinks,['"Parameters",'"",'dbShowCons,'parameters]]]
  htSay '"}{"
  if $exposedOnlyIfTrue
    then
      if one?
      then htSay '"{\em Unexposed Also}"
      else htMakePage [['bcLinks,['"Unexposed Also",'"",'dbShowCons,'exposureOff]]]
    else
      if one?
      then htSay '"{\em Exposed Only}"
      else htMakePage [['bcLinks,['"Exposed Only",'"",'dbShowCons,'exposureOn]]]
  htSay '"}"
  htEndTable()

htFilterPage(htPage,args) ==
  htInitPage("Filter String",htCopyProplist htPage)
  htSay "\centerline{Enter filter string (use {\em *} for wild card):}"
  htSay '"\centerline{"
  htMakePage [['bcStrings, [50,'"",'filter,'EM]]]
  htSay '"}\vspace{1}\centerline{"
  htMakePage [['bcLispLinks,['"\fbox{Filter}",'"",:args]]]
  htSay '"}"
  htShowPage()

dbShowConsKinds cAlist ==
  cats := doms := paks := defs := nil
  for x in cAlist repeat
    op := CAAR x
    kind := dbConstructorKind op
    kind  = 'category => cats := [x,:cats]
    kind = 'domain    => doms := [x,:doms]
    kind = 'package   => paks := [x,:paks]
    defs := [x,:defs]
  lists := [NREVERSE cats,NREVERSE doms,NREVERSE paks,NREVERSE defs]
  htBeginMenu 'description
  htSayStandard '"\indent{1}"
  kinds := +/[1 for x in lists | #x > 0]
  for kind in '("category" "domain" "package" "default package") for x in lists | #x > 0 repeat
    htSayStandard '"\item"
    if kinds = 1
       then htSay menuButton()
       else htMakePage
         [['bcLinks,[menuButton(),'"",'dbShowConsKindsFilter,[kind,x]]]]
    htSayStandard '"\tab{1}"
    htSayList(['"{\em ", c := #x, '" "])
    htSay(c > 1 => pluralize kind; kind)
    htSay '":}"
    bcConTable REMDUP [CAAR y for y in x]
  htEndMenu 'description
  htSayStandard '"\indent{0}"

addParameterTemplates(page, conform) ==
---------------> from kPage <-----------------------
  parlist := [STRINGIMAGE par for par in rest conform]
  manuelsCode? := "MAX"/[#s for s in parlist] > 10
  w := (manuelsCode? => 55; 23)
  htSay '"Optional argument value"
  htSay
    rest parlist => '"s:"
    '":"
  odd := false
  for parname in $PatternVariableList for par in rest conform repeat
    htSayStandard (odd or manuelsCode? => "\newline";"\tab{29}")
    odd := not odd
    argstring :=
      $conArgstrings is [a,:r] => ($conArgstrings := r; a)
      '""
    htMakePage [['text,'"{\em ",par,'"} = "],
        ['bcStrings,[w - #STRINGIMAGE par,argstring,parname,'EM]]]

--------------------> NEW DEFINITION (see br-con.boot)
kPageArgs([op,:args],[.,.,:source]) ==
  firstTime := true
  coSig := rest GETDATABASE(op,'COSIG)
  for x in args for t in source for pred in coSig repeat
    if firstTime then firstTime := false
                 else
                   htSayStandard '", and"
    htSayStandard '"\newline "
    typeForm := (t is [":",.,t1] => t1; t)
    if pred = true
      then htMakePage [['bcLinks,[x,'"",'kArgPage,x]]]
      else htSayList(['"{\em ", x, '"}"])
    htSayStandardList(['"\tab{", STRINGIMAGE( # PNAME x), '"}, "])
    htSay
      pred => '"a domain of category "
      '"an element of the domain "
    bcConform(typeForm,true)

--=======================================================================
--              Redefinitions from br-op1.boot
--=======================================================================
--------------------> NEW DEFINITION (see br-op1.boot)
dbConform form ==
--one button for the main constructor page of a type
  ["\conf{",:form2StringList opOf form,'"}{",:form2Fence dbOuttran form,'"}"]

--------------------> NEW DEFINITION (see br-op1.boot)
htTab s == htSayStandardList(['"\tab{", s, '"}"])

--------------------> NEW DEFINITION (see br-op1.boot)
dbGatherThenShow(htPage,opAlist,which,data,constructorIfTrue,word,fn) ==
  single? := null rest data
  htBeginMenu 'description
  bincount := 0
  for [thing,exposeFlag,:items] in data repeat
    htSayStandard ('"\item")
    if single? then htSay(menuButton())
    else
      htMakePage
        [['bcLinks,[menuButton(),'"",'dbShowOps,which,bincount]]]
      button := mkButtonBox (1 + bincount)
    htSay '"{\em "
    htSay
      thing = 'nowhere => '"implemented nowhere"
      thing = 'constant => '"constant"
      thing = '_$ => '"by the domain"
      INTEGERP thing => '"unexported"
      constructorIfTrue =>
        htSay word
        atom thing => '" an unknown constructor"
        '""
      atom thing => '"unconditional"
      '""
    htSay '"}"
    if null atom thing then
      if constructorIfTrue then
          htSayList(['" {\em ", dbShowKind thing, '"}"])
      htSay '" "
      FUNCALL(fn,thing)
    htSay('":\newline ")
    dbShowOpSigList(which,items,(1 + bincount) * 8192)
    bincount := bincount + 1
  htEndMenu 'description

--------------------> NEW DEFINITION (see br-op1.boot)
dbPresentOps(htPage,which,:exclusions) ==
  asharp? := htpProperty(htPage,'isAsharpConstructor)
  fromConPage? := (conname := opOf htpProperty(htPage,'conform))
  usage? := nil
  star? := not fromConPage? or which = '"package operation"
  implementation? := not asharp? and
    $UserLevel = 'development and $conformsAreDomains --and not $includeUnexposed?
  rightmost? := star? or (implementation? and not $includeUnexposed?)
  if INTEGERP first exclusions then exclusions := ['documentation]
  htpSetProperty(htPage,'exclusion,first exclusions)
  opAlist :=
    which = '"operation" => htpProperty(htPage,'opAlist)
    htpProperty(htPage,'attrAlist)
  empty? := null opAlist
  one?   := opAlist is [entry] and 2 = #entry
  one? := empty? or one?
  htBeginTable()
  htSay '"{"
  if one? or member('conditions,exclusions)
                 or (htpProperty(htPage,'condition?) = 'no)
      then htSay '"{\em Conditions}"
      else htMakePage [['bcLispLinks,['"Conditions",'"",'dbShowOps,which,'conditions]]]
  htSay '"}{"
  if empty? or member('documentation,exclusions)
    then htSay '"{\em Descriptions}"
    else htMakePage [['bcLispLinks,['"Descriptions",'"",'dbShowOps,which,'documentation]]]
  htSay '"}{"
  if null IFCDR opAlist
    then htSay '"{\em Filter}"
    else htMakePage [['bcLinks,['"Filter ",'"",'htFilterPage,['dbShowOps,which,'filter]]]]
  htSay '"}{"
  if one? or member('names,exclusions) or null IFCDR opAlist
    then htSay '"{\em Names}"
    else htMakePage [['bcLispLinks,['"Names",'"",'dbShowOps,which,'names]]]
  if not star? then
    htSay '"}{"
    if not implementation? or member('implementation,exclusions) or which = '"attribute" or
      ((conname := opOf htpProperty(htPage,'conform))
        and GETDATABASE(conname,'CONSTRUCTORKIND) = 'category)
    then htSay '"{\em Implementations}"
    else htMakePage
      [['bcLispLinks,['"Implementations",'"",'dbShowOps,which,'implementation]]]
  htSay '"}{"
  if one? or member('origins,exclusions)
    then htSay '"{\em Origins}"
    else htMakePage [['bcLispLinks,['"Origins",'"",'dbShowOps,which,'origins]]]
  htSay '"}{"
  if one? or member('parameters,exclusions) --also test for some parameter
      or not dbDoesOneOpHaveParameters? opAlist
    then htSay '"{\em Parameters}"
    else htMakePage [['bcLispLinks,['"Parameters",'"",'dbShowOps,which,'parameters]]]
  htSay '"}{"
  if which ~= '"attribute" then
    if one? or member('signatures,exclusions)
      then htSay '"{\em Signatures}"
      else htMakePage [['bcLispLinks,['"Signatures",'"",'dbShowOps,which,'signatures]]]
  htSay '"}"
  if star? then
    htSay '"{"
    if $exposedOnlyIfTrue
    then if one?
         then htSay '"{\em Unexposed Also}"
         else htMakePage [['bcLinks,['"Unexposed Also",'"",'dbShowOps,which,'exposureOff]]]
    else if one?
         then htSay '"{\em Exposed Only}"
         else htMakePage [['bcLinks,['"Exposed Only",'"",'dbShowOps, which,'exposureOn]]]
    htSay '"}"
  htEndTable()

--=======================================================================
--              Redefinitions from br-search.boot
--=======================================================================
---------------------> OLD DEFINITION (override in br-search.boot)
htShowPageStar() ==
  htSayStandard '"\endscroll "
  if $exposedOnlyIfTrue then
    htMakePage [['bcLinks,['"Unexposed Also",'"",'repeatSearch,NIL]]]
  else
    htMakePage [['bcLinks,['"Exposed Only",'"",'repeatSearch,'T]]]
  htShowPageNoScroll()

--=======================================================================
--              Redefinitions from br-op2.boot
--=======================================================================

--------------> NEW DEFINITION (see br-op2.boot)
displayDomainOp(htPage,which,origin,op,sig,predicate,
                doc,index,chooseFn,unexposed?,$generalSearch?) ==
  $chooseDownCaseOfType : local := true   --see dbGetContrivedForm
  $whereList  : local := nil
  $NumberList : local := '(i j k l m n i1 j1 k1 l1 m1 n1 i2 j2 k2 l2 m2 n2 i3 j3 k3 l3 m3 n3 i4 j4 k4 l4 m4 n4 )
  $ElementList: local := '(x y z u v w x1 y1 z1 u1 v1 w1 x2 y2 z2 u2 v2 w2 x3 y3 z3 u3 v3 w3 x4 y4 z4 u4 v4 w4 )
  $FunctionList:local := '(f g h d e F G H)
  $DomainList:  local := '(D R S E T A B C M N P Q U V W)
  exactlyOneOpSig     := null index
  conform   := htpProperty(htPage,'domname) or htpProperty(htPage,'conform)
                 or origin
  if $generalSearch? then $DomainList := rest $DomainList
  opform :=
    which = '"attribute" =>
      null sig => [op]
      [op,sig]
    which = '"constructor" => origin
    dbGetDisplayFormForOp(op,sig,doc)
  htSayStandard('"\newline")
  -----------------------------------------------------------
  if exactlyOneOpSig
    then htSay menuButton()
    else htMakePage
      [['bcLinks,[menuButton(),'"",chooseFn,which,index]]]
  htSayStandard '"\tab{2}"
  op   := IFCAR opform
  args := IFCDR opform
  ops := escapeSpecialChars STRINGIMAGE op
  n := #sig
  do
    n = 2 and GETL(op, 'Nud) =>
        htSayList([ops, '" {\em ", quickForm2HtString IFCAR args, '"}"])
    n = 3 and GETL(op, 'Led) =>
        htSayList(['"{\em ", quickForm2HtString IFCAR args, '"} ", ops,
              '" {\em ", quickForm2HtString IFCAR IFCDR args, '"}"])
    if unexposed? and $includeUnexposed? then
      htSayUnexposed()
    htSay(ops)
    predicate='ASCONST or GETDATABASE(op,'NILADIC) or member(op,'(0 1)) => 'skip
    which = '"attribute" and null args => 'skip
    htSay('"(")
    if IFCAR args then
        htSayList(['"{\em ", quickForm2HtString IFCAR args, '"}"])
    for x in IFCDR args repeat
        htSayList(['",{\em ", quickForm2HtString x, '"}"])
    htSay('")")
  -----------prepare to print description---------------------
  constring := form2HtString conform
  conname   := first conform
  $conkind   : local := htpProperty(htPage,'kind) -- a string e.g. "category"
                          or STRINGIMAGE GETDATABASE(conname,'CONSTRUCTORKIND)
  $conlength : local := #constring
  $conform   : local := conform
  $conargs   : local := rest conform
  if which = '"operation" then
    $signature : local :=
      MEMQ(conname,$Primitives) => nil
      CDAR getConstructorModemap conname
    --RDJ: this next line is necessary until compiler bug is fixed
    --that forgets to substitute #variables for t#variables;
    --check the signature for SegmentExpansionCategory, e.g.
    tvarlist := TAKE(# $conargs,$TriangleVariableList)
    $signature := SUBLISLIS($FormalMapVariableList,tvarlist,$signature)
  $sig :=
    which = '"attribute" or which = '"constructor" => sig
    $conkind ~= '"package" => sig
    symbolsUsed := [x for x in rest conform | IDENTP x]
    $DomainList := SETDIFFERENCE($DomainList,symbolsUsed)
    getSubstSigIfPossible sig
  -----------------------------------------------------------
  if member(which,'("operation" "constructor")) then
    $displayReturnValue: local := nil
    if args then
      htSayStandard('"\newline\tab{2}{\em Arguments:}")
      coSig := IFCDR GETDATABASE(op, 'COSIG)  --check if op is constructor
      for a in args for t in rest $sig repeat
            htSayIndentRel2(15, true)
            position := IFCAR relatives
            relatives := IFCDR relatives
            if IFCAR coSig and t ~= '(Type)
              then htMakePage [['bcLinks,[a,'"",'kArgPage,a]]]
              else htSayList(['"{\em ", form2HtString(a), '"}"])
            htSay ", "
            coSig := IFCDR coSig
            htSayValue t
            htSayIndentRel2(-15, true)
            htSayStandard('"\newline ")
    if first $sig then
      $displayReturnValue := true
      htSayStandard('"\newline\tab{2}")
      htSay '"{\em Returns:}"
      htSayIndentRel2(15, true)
      htSayValue first $sig
      htSayIndentRel2(-15, true)
  -----------------------------------------------------------
  if origin and ($generalSearch? or origin ~= conform) and op~=opOf origin then
    htSayStandard('"\newline\tab{2}{\em Origin:}")
    htSayIndentRel(15)
    if not isExposedConstructor opOf origin and $includeUnexposed?
       then htSayUnexposed()
    bcConform(origin,true)
    htSayIndentRel(-15)
  -----------------------------------------------------------
  if not MEMQ(predicate,'(T ASCONST)) then
    pred := sublisFormal(IFCDR conform, predicate)
    count := #pred
    htSayStandard('"\newline\tab{2}{\em Conditions:}")
    for p in displayBreakIntoAnds SUBST($conform,"$",pred) repeat
      htSayIndentRel2(15, count > 1)
      bcPred(p,$conform,true)
      htSayIndentRel2(-15, count > 1)
      htSayStandard('"\newline ")
  -----------------------------------------------------------
  if $whereList then
    count := #$whereList
    htSayStandard('"\newline\tab{2}{\em Where:}")
    if assoc("$",$whereList) then
      htSayIndentRel2(15, true)
      htSayStandard '"{\em \$} is "
      htSay
        $conkind = '"category" => '"of category "
        '"the domain "
      bcConform(conform,true,true)
      htSayIndentRel2(-15, true)
    for [d,key,:t] in $whereList | d ~= "$" repeat
      htSayIndentRel2(15, count > 1)
      htSayList(["{\em ", d, "} is "])
      htSayConstructor(key, sublisFormal(IFCDR conform, t))
      htSayIndentRel2(-15, count > 1)
  -----------------------------------------------------------
  if doc and (doc ~= '"" and (doc isnt [d] or d ~= '"")) then
    htSayStandard('"\newline\tab{2}{\em Description:}")
    htSayIndentRel(15)
    if doc = $charFauxNewline then htSay $charNewline
    else
       ndoc:=
          -- we are confused whether doc is a string or a list of strings
          CONSP doc =>  [SUBSTITUTE($charNewline, $charFauxNewline, i) for i in doc]
          SUBSTITUTE($charNewline, $charFauxNewline,doc)
       htSay ndoc
    htSayIndentRel(-15)
  --------> print abbr and source file for constructors <---------
  if which = '"constructor" then
    if (abbr := GETDATABASE(conname,'ABBREVIATION)) then
      htSayStandard('"\tab{2}{\em Abbreviation:}")
      htSayIndentRel(15)
      htSay abbr
      htSayIndentRel(-15)
      htSayStandard('"\newline{}")
    htSayStandard('"\tab{2}{\em Source File:}")
    htSayIndentRel(15)
    htSaySourceFile conname
    htSayIndentRel(-15)

htSaySourceFile conname ==
  sourceFileName := (GETDATABASE(conname,'SOURCEFILE) or '"none")
  filename :=  extractFileNameFromPath sourceFileName
  htMakePage [['text,'"\unixcommand{",filename,'"}{_\$AXIOM/lib/SPADEDIT ",
              sourceFileName, '" ", conname, '"}"]]

--------------------> NEW DEFINITION (see br-op2.boot)
htSayIndentRel(n) == htSayIndentRel2(n, false)

htSayIndentRel2(n, flag) ==
  m := ABS n
  if flag then m := m + 2
  htSayStandard
    n > 0 =>
      flag => ['"\indent{",STRINGIMAGE m,'"}\tab{-2}"]
      ['"\indent{",STRINGIMAGE m,'"}\tab{0}"]
    n < 0 => ['"\indent{0}\newline "]

htSayUnexposed() ==
  htSay '"{\em *}"
  $atLeastOneUnexposed := true
--=======================================================================
--                       Page Operations
--=======================================================================

htBeginTable() ==
  htSayStandard '"\table{"

htEndTable() ==
  htSayStandard '"}"

htBeginMenu(kind) ==
  htSayStandard '"\beginmenu "

htEndMenu(kind) ==
  htSayStandard '"\endmenu "

htSayConstructorName(nameShown, name) ==
    htSayStandard ["\lispdownlink{",nameShown,'"}{(|conPage| '|",name,'"|)}"]

--------------------> NEW DEFINITION (see ht-util.boot)
htAddHeading(title) ==
  htNewPage title
  page()

------------> called by htAddHeading, htInitPageNoScroll <-----------
htNewPage title ==
    htSayStandardList(['"\begin{page}{", htpName $curPage, '"}{"])
    htSayStandard title
    htSayStandard '"}"

--=======================================================================
--                       Utilities
--=======================================================================

htBlank() ==
    htSayStandard '"\space{1}"

htBlanks(n) ==
    htSayStandard STRCONC('"\space{",STRINGIMAGE n,'"}")

unTab s ==
  STRINGP s => unTab1 s
  atom s => s
  [unTab1 first s, :rest s]

unTab1 s ==
  STRING_<('"\tab{", s) = 5 and (k := charPosition(char '_}, s, 4)) =>
      SUBSTRING(s, k + 1, nil)
  s

satBreak() ==
  htSayStandard '"\item "

htBigSkip() ==
  htSayStandard '"\vspace{1}\newline "

satDownLink(s,code) ==
  htSayStandard '"\lispdownlink{"
  htSayStandard s
  htSayStandard '"}{"
  htSayStandard code
  htSayStandard '"}"

satTypeDownLink(s,code) ==
  htSayStandard '"\lispdownlink{"
  htSayStandard s
  htSayStandard '"}{"
  htSayStandard code
  htSayStandard '"}"

mkButtonBox n == STRCONC('"\buttonbox{", STRINGIMAGE n, '"}")

purgeNewConstructorLines(lines, conlist) ==
  [x for x in lines | not screenLocalLine(x, conlist)]

screenLocalLine(line, conlist) ==
  k := dbKind line
  con := INTERN
    k = char 'o or k = char 'a =>
      s := dbPart(line,5,1)
      k := charPosition(char '_(,s,1)
      SUBSTRING(s,1,k - 1)
    dbName line
  MEMQ(con, conlist)

--------------> NEW DEFINITION (see br-data.boot)
purgeLocalLibdb() ==   --called by the user through a clear command?
  $newConstructorList := nil
  deleteFile '"libdb.text"

