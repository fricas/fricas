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

--====================> WAS b-op1.boot <================================

--=======================================================================
--                   Operation Page Menu
--=======================================================================
--opAlist has form [[op,:alist],:.]  where each alist
--        has form [sig,pred,origin,exposeFlag,comments]

dbFromConstructor?(htPage) == htpProperty(htPage,'conform)

dbDoesOneOpHaveParameters? opAlist ==
  or/[(or/[fn for x in items]) for [op,:items] in opAlist] where fn ==
    STRINGP x => dbPart(x,2,1) ~= '"0"
    IFCAR x
--============================================================================
--               Master Switch Functions for Operation Views
--============================================================================

dbShowOps(htPage, key) ==
  if MEMQ(key,'(extended basic all)) then
    $groupChoice := key
    key := htpProperty(htPage,'key) or 'names
  opAlist := htpProperty(htPage, 'opAlist)
  key = 'generalise =>
    arg  := STRINGIMAGE CAAR opAlist
    oPage arg
  key = 'filter =>
    filter := pmTransFilter(dbGetInputString htPage)
    filter is ['error,:.] => bcErrorPage filter
    opAlist:= [x for x in opAlist | superMatch?(filter,DOWNCASE STRINGIMAGE opOf x)]
    null(opAlist) => emptySearchPage('"operation", filter, false)
    htPage := htInitPageNoHeading(htCopyProplist htPage)
    htpSetProperty(htPage, 'opAlist, opAlist)
    if not (htpProperty(htPage, 'condition?) = 'no) then
        dbResetOpAlistCondition(htPage, opAlist)
    dbShowOps(htPage, htpProperty(htPage, 'exclusion))
  htpSetProperty(htPage,'key,key)
  if MEMQ(key,'(exposureOn exposureOff)) then
    $exposedOnlyIfTrue :=
       key = 'exposureOn => 'T
       nil
    key := htpProperty(htPage,'exclusion)
  dbShowOp1(htPage, opAlist, key)

reduceByGroup(htPage,opAlist) ==
  not dbFromConstructor?(htPage) or null $groupChoice => opAlist
  dbExpandOpAlistIfNecessary(htPage, opAlist, false)
  bitNumber := HGET($topicHash,$groupChoice)
  res := [[op,:newItems] for [op,:items] in opAlist | newItems] where
    newItems ==
      null bitNumber => items
      [x for x in items | FIXP (code := LASTATOM x) and LOGBITP(bitNumber,code)]
  res

dbShowOp1(htPage, opAlist, key) ==
  --set up for filtering below in dbGatherData
  which := '"operation"
  if INTEGERP key then
    -- BREAK()
    opAlist := dbSelectData(htPage,opAlist,key)
    ------> Jump out for constructor names in file <--------
  INTEGERP key and opAlist is [[con,:.]] and htpProperty(htPage,'isFile)
      and constructor? con => return conPageChoose con
  if INTEGERP key then
    htPage := htInitPageNoHeading(htCopyProplist htPage)
    htpSetProperty(htPage, 'opAlist, opAlist)
    if not (htpProperty(htPage, 'condition?) = 'no) then
        dbResetOpAlistCondition(htPage, opAlist)
  dbExpandOpAlistIfNecessary(htPage, opAlist, false)
  if $exposedOnlyIfTrue and not dbFromConstructor?(htPage) then
  --opAlist is expanded to form [[op,[sig,pred,origin,exposed,comments],...],...]
    opAlist:=[item for [op,:items] in opAlist | item] where
      item ==
        acc := nil
        for x in items | x.3 repeat acc:= [x,:acc]
        null acc => nil
        [op,:NREVERSE acc]
  $conformsAreDomains : local := htpProperty(htPage,'domname)
  opCount := opAlistCount(opAlist)
  branch :=
    INTEGERP key =>
      opCount <= $opDescriptionThreshold => 'documentation
      'names
    key = 'names and null rest opAlist =>      --means a single op
      opCount <= $opDescriptionThreshold => 'documentation
      'names
    key
  [what,whats,fn] := LASSOC(branch,$OpViewTable)
  data := dbGatherData(htPage, opAlist, branch)
  dataCount := +/[1 for x in data | (what = '"Name" and $exposedOnlyIfTrue => atom x; true)]
  namedPart :=
    null rest opAlist =>
      ops := escapeSpecialChars STRINGIMAGE CAAR opAlist
      ['" {\em ",ops,'"}"]
    nil
  if what = '"Condition" and null IFCAR IFCAR data then dataCount := dataCount - 1
  exposurePart :=
    $exposedOnlyIfTrue => '(" Exposed ")
    nil
  firstPart :=
    opCount = 0 => ['"No ",:exposurePart, pluralize capitalize which]
    dataCount = 1 or dataCount = opCount =>
      opCount = 1 => [:exposurePart, capitalize which,:namedPart]
      [STRINGIMAGE opCount,'" ",:exposurePart,
         pluralize capitalize which,:namedPart]
    prefix := pluralSay(dataCount,what,whats)
    [:prefix,'" for ",STRINGIMAGE opCount,'" ",pluralize capitalize which,:namedPart]
  page := htInitPageNoHeading(htCopyProplist htPage)
  ------------>above line used to call htInitPageHoHeading<----------
  htAddHeading dbShowOpHeading([:firstPart,:fromHeading page], branch)
  htpSetProperty(page,'data,data)
  htpSetProperty(page,'branch,branch)
  -- the only place where specialMessage property is set seems to be commented. out
  if u := htpProperty(page,'specialMessage) then APPLY(first u,rest u)
  htSayStandard('"\beginscroll ")
  FUNCALL(fn, page, opAlist, data) --apply branch function
  if $atLeastOneUnexposed then
      htSay '"{\em *} = unexposed"
  htSayStandard('"\endscroll ")
  dbPresentOps(page, branch)
  htShowPageNoScroll()

opAlistCount(opAlist) == +/[foo for [op, :items] in opAlist] where foo ==
  null $exposedOnlyIfTrue => #items
  +/[1 for w in items | null (p := CDDR w) or p . 1]

dbShowOpHeading(heading, branch) ==
  suffix :=
--  branch = 'signatures => '" viewed as signatures"
    branch = 'parameters => '" viewed with parameters"
    branch = 'origins    => '" organized by origins"
    branch = 'conditions => '" organized by conditions"
    '""
  [:heading, suffix]

fromHeading htPage ==
  null htPage => '""
  $pn := [htPage.0,'"}{"]
  updomain := htpProperty(htPage,'updomain) =>
    dnForm  := dbExtractUnderlyingDomain updomain
    dnString:= form2StringList dnForm
    dnFence := form2Fence  dnForm
--  upString:= form2StringList updomain
    upFence := form2Fence  updomain
    upOp    := PNAME opOf  updomain
    ['" {\em from} ",:dbConformGen dnForm,'" {\em under} \ops{",upOp,'"}{",:$pn,:upFence,'"}"]
  domname  := htpProperty(htPage,'domname)
  numberOfUnderlyingDomains :=
        #[x for x in rest(get_database(opOf(domname), 'COSIG)) | x]
  IFCDR domname => ['" {\em from} ", :dbConformGen domname]
  htpProperty(htPage,'fromHeading)

conform2StringList(form, opFn, argFn) ==
  [op1,:args] := form
  op := IFCAR HGET($lowerCaseConTb,op1) or op1
  null args => APPLY(opFn,[op])
  special := MEMQ(op,'(Union Record Mapping))
  cosig :=
    special => ['T for x in args]
    rest(get_database(op, 'COSIG))
  atypes :=
    special => cosig
    rest(CDAR(get_database(op, 'CONSTRUCTORMODEMAP)))
  sargl := [fn for x in args for atype in atypes for pred in cosig] where fn ==
    keyword :=
      special and x is [":",y,t] =>
        x := t
        y
      nil
    res :=
      pred =>
        STRINGP x => [x]
        u := APPLY(argFn,[x])
        atom u and [u] or u
      typ := sublisFormal(args,atype)
      if x is ['QUOTE,a] then x := a
      u := mathform2HtString algCoerceInteractive(x,typ,'(OutputForm)) => [u]
      NUMBERP x or STRINGP x => [x]
      systemError()
    keyword => [keyword,'": ",:res]
    res
  op = 'Mapping => dbMapping2StringList sargl
  head :=
    special => [op]
    APPLY(opFn,[form])
  [:head,'"(",:first sargl,:"append"/[[",",:y] for y in rest sargl],'")"]


dbMapping2StringList [target,:sl] ==
  null sl => target
  restPart :=
    null rest sl => nil
    "append"/[[",",:y] for y in rest sl]
  sourcePart :=
    restPart => ['"(",:first sl,:restPart,'")"]
    first sl
  [:sourcePart,'" -> ",:target]

dbOuttran form ==
  if LISTP form then
    [op,:args] := form
  else
    op := form
    args := nil
  cosig := rest(get_database(op, 'COSIG))
  atypes := rest(CDAR(get_database(op, 'CONSTRUCTORMODEMAP)))
  argl := [fn for x in args for atype in atypes for pred in cosig] where fn ==
    pred => x
    typ := sublisFormal(args,atype)
    arg :=
      x is ['QUOTE,a] => a
      x
    res := mathform2HtString algCoerceInteractive(arg,typ,'(OutputForm))
    NUMBERP res or STRINGP res => res
    ['QUOTE,res]
  [op,:argl]

dbConformGen form == dbConformGen1(form,true)
--many buttons: one for the type and one for each inner type
--NOTE: must only be called on types KNOWN to be correct

dbConformGenUnder form == dbConformGen1(form,false)
--same as above, except buttons only for the inner types

dbConformGen1(form,opButton?) ==
  opFunction :=
    opButton? => FUNCTION dbConform
    FUNCTION conname2StringList
  originalOp := opOf form
  op := unAbbreviateIfNecessary opOf form
  args := IFCDR form
  form :=
    originalOp=op => form
    [op, :args]
  args => conform2StringList(form, opFunction, FUNCTION dbConformGen)
  APPLY(opFunction,[form])

unAbbreviateIfNecessary op == IFCAR HGET($lowerCaseConTb, op) or op

conname2StringList form == [PNAME unAbbreviateIfNecessary opOf form]

--===========================================================================
--               Data Gathering Code
--============================================================================
dbGatherData(htPage, opAlist, key) ==
  key = 'implementation => dbGatherDataImplementation(htPage, opAlist)
  dataFunction := LASSOC(key,table) where
    table ==
      $dbDataFunctionAlist or
        ($dbDataFunctionAlist := [
          ['signatures,:function dbMakeSignature],
            ['parameters,:function dbContrivedForm],
              ['origins,:function dbGetOrigin],
                ['domains,:function dbGetOrigin],
                  ['conditions,:function dbGetCondition]])
  null dataFunction =>
    --key= names or filter or documentation; do not expand
    if $exposedOnlyIfTrue and not dbFromConstructor?(htPage) then
        opAlist := htpProperty(htPage, 'opAlist)
    acc := nil
    initialExposure :=
        -- never star ops from a constructor
        htPage and htpProperty(htPage, 'conform) => true
        false
    for [op,:alist] in opAlist repeat
      exposureFlag := initialExposure
      while alist repeat
        item := first alist
        isExposed? :=
          STRINGP item => dbExposed?(item,char 'o)   --unexpanded case
          null (r := rest rest item) => true      --assume true if unexpanded
          r . 1                                   --expanded case
        if isExposed? then return (exposureFlag := true)
        alist := rest alist
      node :=
        exposureFlag => op
        [op,nil]
      acc := [node,:acc]
    NREVERSE acc
  data := nil
  dbExpandOpAlistIfNecessary(htPage, opAlist, false)
  --create data, a list of the form ((entry,exposeFlag,:entries)...)
  for [op,:alist] in opAlist repeat
    for item in alist repeat
      entry := FUNCALL(dataFunction,op,item)--get key item
      exposeFlag :=                         --is the current op-sig exposed?
        null (r := rest rest item) => true  --not given, assume yes
        r . 1                               --is  given, use value
      tail :=
        item is [.,'ASCONST,:.] => 'ASCONST
        nil
      newEntry :=
        u := assoc(entry,data) =>           --key seen before? look on DATA
          RPLACA(rest u, CADR u or exposeFlag)--yes, expose if any 1 is exposed
          u
        data := [y := [entry,exposeFlag,:tail],:data]
        y                                   --no, create new entry in DATA
      if member(key,'(origins conditions)) then
        r := CDDR newEntry
        if atom r then r := nil             --clear out possible 'ASCONST
        RPLACD(rest newEntry,             --store op/sigs under key if needed
          insert([dbMakeSignature(op,item),exposeFlag,:tail],r))
  if member(key,'(origins conditions)) then
    for entry in data repeat   --sort list of entries (after the 2nd)
      tail := CDDR entry
      tail :=
        atom tail => tail
        listSort(function LEXLESSEQP,tail)
      RPLACD(rest entry, tail)
  data := listSort(function LEXLESSEQP,data)
  data

dbGatherDataImplementation(htPage, opAlist) ==
--returns data, of form ((implementor exposed? entry entry...)...
--  where entry has form ((op sig . implementor) . stuff)
  conform := htpProperty(htPage,'conform)
  domainForm  := htpProperty(htPage,'domname)
  dom     := EVAL domainForm
  [nam, :.] := domainForm
  $predicateList: local := get_database(nam, 'PREDICATES)
  u := getDomainOpTable2(dom, true, ASSOCLEFT opAlist)
  --u has form ((op,sig,:implementor)...)
  --sort into 4 groups: domain exports, unexports, default exports, others

  for (x := [.,.,:key]) in u for i in 0.. repeat
    key = domainForm => domexports := [x,:domexports]
    INTEGERP key => unexports := [x,:unexports]
    isDefaultPackageForm? key => defexports := [x,:defexports]
    key = 'nowhere => nowheres := [x,:nowheres]
    key = 'constant =>constants := [x,:constants]
    others := [x,:others]   --add chain domains go here
  fn [nowheres,constants,domexports,SORTBY('CDDR,NREVERSE others),SORTBY('CDDR,
               NREVERSE defexports),SORTBY('CDDR,NREVERSE unexports)] where
    fn l ==
      alist := nil
      for u in l repeat
        while u repeat
          key := CDDAR u  --implementor
          entries :=
            [[first u, true],
             :[u and [first u, true] while key = CDDAR(u := rest u)]]
          alist := [[key,gn key,:entries],:alist]
      NREVERSE alist
    gn key ==
      atom key => true
      isExposedConstructor first key

dbSelectData(htPage,opAlist,key) ==
  branch := htpProperty(htPage,'branch)
  data   := htpProperty(htPage,'data)
  MEMQ(branch,'(signatures parameters)) =>
    dbReduceOpAlist(opAlist,data.key,branch)
  MEMQ(branch,'(origins conditions implementation)) =>
    key < 8192 => dbReduceOpAlist(opAlist,data.key,branch)
    [newkey,binkey] := DIVIDE(key,8192)  --newkey is 1 too large
    innerData := CDDR data.(newkey - 1)
    dbReduceOpAlist(opAlist,innerData.binkey,'signatures)
  [opAlist . key]

dbReduceOpAlist(opAlist,data,branch) ==
  branch = 'signatures => dbReduceBySignature(opAlist,CAAR data,CADAR data)
  branch = 'origins => dbReduceBySelection(opAlist, first data, function CADDR)
  branch = 'conditions =>
      dbReduceBySelection(opAlist, first data, function CADR)
  branch = 'implementation => dbReduceByOpSignature(opAlist,CDDR data)
  branch = 'parameters => dbReduceByForm(opAlist, first data)
  systemError ['"Unexpected branch: ",branch]

dbReduceByOpSignature(opAlist,datalist) ==
--reduces opAlist by implementation datalist, one of the form
--    (((op,sig,:implementor),:stuff),...)
  ops := [CAAR x for x in datalist] --x is [[op,sig,:implementor],:.]
  acc := nil
  for [op,:alist] in opAlist | MEMQ(op,ops) repeat
    entryList := [entry for (entry := [sig,:.]) in alist | test] where test ==
      or/[x for x in datalist | x is [[=op,=sig,:.],:.]]
    entryList => acc := [[op,:NREVERSE entryList],:acc]
  NREVERSE acc

dbReduceBySignature(opAlist,op,sig) ==
--reduces opAlist to one with a fixed op and sig
  [[op,:[x for x in LASSOC(op,opAlist) | x is [=sig,:.]]]]

dbReduceByForm(opAlist,form) ==
  acc := nil
  for [op,:alist] in opAlist repeat
    items := [x for x in alist | dbContrivedForm(op,x) = form] =>
      acc := [[op,:items],:acc]
  NREVERSE acc

dbReduceBySelection(opAlist,key,fn) ==
  acc := nil
  for [op,:alist] in opAlist repeat
    items := [x for x in alist | FUNCALL(fn,x) = key] =>
      acc := [[op,:items],:acc]
  NREVERSE acc

dbContrivedForm(op,[sig,:.]) ==
  dbMakeContrivedForm(op,sig)

dbMakeSignature(op,[sig,:.]) == [op,sig]  --getDomainOpTable format

dbGetOrigin(op,[.,.,origin,:.]) == origin

dbGetCondition(op,[.,pred,:.]) == pred

--============================================================================
--               Branches of Views
--============================================================================
dbShowOpNames(htPage, opAlist, data) ==
  single? := opAlist and null rest data
  single? =>
    ops := escapeSpecialChars STRINGIMAGE CAAR opAlist
    htSayStandard('"Select a view below")
  exposedOnly? := $exposedOnlyIfTrue and not dbFromConstructor?(htPage)
  dbShowOpItems(data, exposedOnly?)

dbShowOpItems(data, exposedOnly?) ==
  htBeginTable()
  for i in 0.. for item in data repeat
    if atom item then
      op := item
      exposeFlag := true
    else
      [op,exposeFlag] := item
    ops := escapeSpecialChars STRINGIMAGE op
    exposeFlag or not exposedOnly? =>
      htSay('"{")
      bcStarSpaceOp(ops,exposeFlag)
      htMakePage [['bcLinks, [ops, '"", 'dbShowOps, i]]]
      htSay('"}")
  htEndTable()

simpOrDumb(new,old) ==
  new = 'etc => 'etc
  atom new => old
  'etc

dbShowOpOrigins(htPage, opAlist, data) ==
    dbGatherThenShow(htPage, opAlist, data, true, '"from",
                     function bcStarConform)

dbShowOpImplementations(htPage, opAlist, data) ==
    $from_show_implementations : local := true
    dbGatherThenShow(htPage, opAlist, data, true, '"by",
                     function bcStarConform)

dbShowOpConditions(htPage, opAlist, data) ==
    dbGatherThenShow(htPage, opAlist, data, false, nil, function bcPred)

dbShowKind conform ==
  conname := first conform
  kind := get_database(conname, 'CONSTRUCTORKIND)
  kind = 'domain =>
    (s := PNAME conname).(MAXINDEX s) = '_& => '"default package"
    '"domain"
  PNAME kind

dbShowOpSignatures(htPage, opAlist, data) == dbShowOpSigList(data, 0)

dbShowOpSigList(dataItems, count) ==
--dataItems is (((op,sig,:.),exposureFlag,...)
  single? := null rest dataItems
  htBeginTable()
  for [[op,sig,:.],exposureFlag,:tail] in dataItems repeat
    ops := escapeSpecialChars STRINGIMAGE op
    htSay '"{"
    htSayExpose(ops,exposureFlag)
    htMakePage [['bcLinks, [ops, '"", 'dbShowOps, count]]]
    htSay '": "
    if tail = 'ASCONST then
        bcConform(first(sig))
    else
        bcConform(['Mapping, :sig])
    htSay '"}"
    count := count + 1
  htEndTable()
  count

dbShowOpParameters(htPage, opAlist, data) ==
  single? := null rest data
  count := 0
  htBeginTable()
  for item in data repeat
    [opform,exposeFlag,:tail] := item
    op := intern IFCAR opform
    args := IFCDR opform
    ops := escapeSpecialChars STRINGIMAGE op
    htSay '"{"
    htSayExpose(ops,exposeFlag)
    n := #opform
    do
      n = 2 and GETL(op, 'Nud) =>
        dbShowOpParameterJump(ops, count, single?)
        htSayList(['" {\em ", IFCAR args, '"}"])
      n = 3 and GETL(op, 'Led) =>
        htSayList(['"{\em ", IFCAR args, '"} "])
        dbShowOpParameterJump(ops, count, single?)
        htSayList(['" {\em ", IFCAR IFCDR args, '"}"])
      dbShowOpParameterJump(ops, count, single?)
      tail = 'ASCONST or member(op,'(0 1)) => 'skip
      htSay('"(")
      if IFCAR args then htSayList(['"{\em ", IFCAR args, '"}"])
      for x in IFCDR args repeat
        htSayList(['", {\em ", x, '"}"])
      htSay('")")
    htSay '"}"
    count := count + 1
  htEndTable()

dbShowOpParameterJump(ops, count, single?) ==
  single? => htSayList(['"{\em ", ops, '"}"])
  htMakePage [['bcLinks, [ops, '"", 'dbShowOps, count]]]

dbShowOpDocumentation(htPage, opAlist, data) ==
  if $exposedOnlyIfTrue and not dbFromConstructor?(htPage) then
      opAlist := htpProperty(htPage, 'opAlist)
  conform := htpProperty(htPage, 'domname) or htpProperty(htPage, 'conform)
  expand := dbExpandOpAlistIfNecessary(htPage, opAlist, false)
  if expand then
      condata := dbGatherData(htPage, opAlist, 'conditions)
      htpSetProperty(htPage, 'conditionData, condata)
  base := -8192
  exactlyOneOpSig := opAlist is [[.,.]] --checked by displayDomainOp
  for [op,:alist] in opAlist repeat
    base := 8192 + base
    for item in alist for j in 0.. repeat
      [sig,predicate,origin,exposeFlag,comments] := item
      exposeFlag or not $exposedOnlyIfTrue =>
        if comments ~= '"" and STRINGP comments and (k := string2Integer comments) then
          comments :=
            MEMQ(k,'(0 1)) => '""
            dbReadComments k
          tail := CDDDDR item
          RPLACA(tail,comments)
        doc := (STRINGP comments and comments ~= '"" => comments; nil)
        pred := predicate or true
        index := (exactlyOneOpSig => nil; base + j)
        displayDomainOp(htPage, '"operation", origin, op, sig, pred,
                        doc, index, 'dbChooseDomainOp, null(exposeFlag), true)

dbChooseDomainOp(htPage, index) ==
  [opKey,entryKey] := DIVIDE(index,8192)
  opAlist := htpProperty(htPage, 'opAlist)
  [op, :entries] := opAlist(opKey)
  entry := entries(entryKey)
  htPage := htInitPageNoHeading(htCopyProplist(htPage))
  htpSetProperty(htPage, 'opAlist, [[op, entry]])
  if not (htpProperty(htPage, 'condition?) = 'no) then
      dbResetOpAlistCondition(htPage, opAlist)
  dbShowOps(htPage, 'documentation)

htSayExpose(op, flag) ==
  $includeUnexposed? =>
    flag => htBlank()
    op.0 = char '_* => htSay '"{\em *} "
    htSayUnexposed()
  htSay '""
--============================================================================
--               Branch-in From Other Places
--============================================================================
dbShowOperationsFromConform(htPage, opAlist) ==  --branch in with lists
  $groupChoice := nil
  conform := htpProperty(htPage,'conform)
  --prepare opAlist for possible filtering of groups
  if null BOUNDP '$topicHash then
    $topicHash := MAKE_HASHTABLE('EQ)
    for [x,:c] in '((extended . 0) (basic . 1) (hidden . 2)) repeat
      HPUT($topicHash,x,c)
  domform := htpProperty(htPage,'domname)
  if htpProperty(htPage, 'kind) = '"category" then
      domform := false
  if domform then
    $conformsAreDomains : local := true
    opAlist := reduceOpAlistForDomain(opAlist, domform, conform)
  conform := domform or conform
  kind := capitalize htpProperty(htPage,'kind)
  exposePart :=
    isExposedConstructor opOf conform => '""
    '" Unexposed "
  fromPart :=
    domform => evalableConstructor2HtString domform
    form2HtString conform
  heading :=
    ['" from ",exposePart,kind,'" {\em ",fromPart,'"}"]
  htpSetProperty(htPage, 'expandOperations, 'lists)
  htpSetProperty(htPage,'fromHeading,heading)
  reducedOpAlist := reduceByGroup(htPage, opAlist)
  htpSetProperty(htPage, 'principalOpAlist, opAlist)
  htpSetProperty(htPage, 'opAlist, reducedOpAlist)
  if domform then
      htpSetProperty(htPage, 'condition?, 'no)
  else
      dbResetOpAlistCondition(htPage, opAlist)
  dbShowOp1(htPage, reducedOpAlist, 'names)

reduceOpAlistForDomain(opAlist,domform,conform) ==
--destructively simplify all predicates; filter out any that fail
  form1 := [domform,:rest domform]
  form2 := ['%, :rest(conform)]
  new_opAlist := []
  for pair in opAlist repeat
    n_items := [test for item in rest pair | test] where test ==
      [head,:tail] := item
      first tail = true => item
      pred := simpHasPred SUBLISLIS(form1,form2,QCAR tail)
      null pred => false
      RPLACD(item,[pred])
      item
    if not(null(n_items)) then
        n_pair := cons(first(pair), n_items)
        new_opAlist := cons(n_pair, new_opAlist)
  NREVERSE(new_opAlist)

dbShowOperationLines(linelist) ==  --branch in with lines
  htPage := htInitPage(nil,nil)  --create empty page
  opAlist := nil
  lines := linelist
  while lines repeat
    name := dbName (x := first lines)
    pile := [x]
    while (lines := rest lines) and name = dbName (x := first lines) repeat
      pile := [x,:pile]
    opAlist := [[name,:NREVERSE pile],:opAlist]
  -- sorting list of pairs (String, List(String))
  opAlist := listSort(function LEXLESSEQP,NREVERSE opAlist)
  htpSetProperty(htPage, 'opAlist, opAlist)
  htpSetProperty(htPage, 'expandOperations, 'strings)
  dbResetOpAlistCondition(htPage, opAlist)
  dbShowOp1(htPage, opAlist, 'names)

--============================================================================
--                Code to Expand opAlist
--============================================================================
dbResetOpAlistCondition(htPage, opAlist) ==
    value := dbExpandOpAlistIfNecessary(htPage, opAlist, true)
    htpSetProperty(htPage, 'condition?, (value => 'yes; 'no))
    value

dbExpandOpAlistIfNecessary(htPage, opAlist, condition?) ==
--if condition? = true, stop when you find a non-trivial predicate
--otherwise, expand in full
--RETURNS:
--  non-trivial predicate, if condition? = true and it finds one
--  nil,                   otherwise
--SIDE-EFFECT: this function references the "expand" property (set elsewhere):
--  'strings, if not fully expanded and it contains strings
--            i.e. opAlist is ((op . (string ...))...) if unexpanded
--  'lists,   if not fully expanded and it contains lists
--            i.e. opAlist is ((op . ((sig pred) ...))...) if unexpanded
    condition? := condition? and not $exposedOnlyIfTrue
    value      := nil  --return value
    expandFlag := htpProperty(htPage, 'expandOperations)
    expandFlag = 'fullyExpanded => nil
    expandFlag = 'strings => --strings are partially expanded
      for pair in opAlist repeat
        [op,:lines] := pair
        acc := nil
        for line in lines repeat
        --NOTE: we must expand all lines here for a given op
        --      since below we will change opAlist
        --Case 1: Already expanded; just cons it onto ACC
          null STRINGP line => --already expanded
            if condition? then --this could have been expanded at a lower level
              if null atom (pred := CADR line) then value := pred
            acc := [line,:acc] --this one is already expanded; record it anyway
        --Case 2: unexpanded; expand it then cons it onto ACC
          [name,nargs,xflag,sigs,conname,pred,comments] := dbParts(line,7,1)
          predicate := ncParseFromString pred
          if condition? and null atom predicate then value := predicate
          sig := ncParseFromString sigs --is (Mapping,:.)
          if sig isnt ['Mapping, :.] then
              sayBrightly(['"Unexpected signature for ", name, '": ", sigs])
          else
              sig := rest(sig)
          conname := intern dbNewConname line
          origin := [conname,:getConstructorArgs conname]
          exposeFlag := dbExposed?(line,char 'o)
          acc := [[sig,predicate,origin,exposeFlag,comments],:acc]
        --always store the fruits of our labor:
        RPLACD(pair,NREVERSE acc)             --at least partially expand it
        condition? and value => return value  --early exit
      value => value
      condition? => nil
      htpSetProperty(htPage, 'expandOperations, 'fullyExpanded)
    expandFlag = 'lists => --lists are partially expanded
      -- entry is [sig, predicate, origin, exposeFlag, comments]
      $value: local := nil
      $docTableHash := MAKE_HASHTABLE('EQUAL)
      packageSymbol := false
      domform := htpProperty(htPage,'domname) or htpProperty(htPage,'conform)
      if isDefaultPackageName opOf domform then
         catname := intern SUBSTRING(s := PNAME opOf domform,0,MAXINDEX s)
         packageSymbol := first rest domform
         domform := [catname,:rest rest domform]  --skip first argument ($)
      docTable:= dbDocTable domform
      for [op,:alist] in opAlist repeat
        for [sig,:tail] in alist repeat
          condition? => --the only purpose here is to find a non-trivial pred
            null atom (pred := first tail) => return ($value := pred)
            'skip
          u :=
            tail is [.,origin,:.] and origin =>
                dbGetDocTable(op, sig, dbDocTable(origin), nil)
            if packageSymbol then sig := substitute('%, packageSymbol, sig)
            dbGetDocTable(op, sig, docTable, nil)
          origin := IFCAR u or origin
          docCode := IFCDR u   --> (doc . code)
          RPLACD(tail,[origin,isExposedConstructor opOf origin,:docCode])
        $value => return $value
      $value => $value
      condition? => nil
      htpSetProperty(htPage, 'expandOperations, 'fullyExpanded)
    'done

getRegistry(op,sig) ==
  u := get_database('AttributeRegistry, 'DOCUMENTATION)
  v := LASSOC(op,u)
  match := or/[y for y in v | y is [['attribute,: =sig],:.]] => CADR match
  '""

evalableConstructor2HtString domform ==
  if VECP domform then domform := devaluate domform
  conname := first domform
  coSig   := rest(get_database(conname, 'COSIG))
  --entries are T for arguments which are domains; NIL for computational objects
  and/[x for x in coSig] => form2HtString(domform,nil,true)
  arglist := [unquote x for x in rest domform] where
    unquote arg  ==
      arg is [f,:args] =>
        f = 'QUOTE => first args
        [f,:[unquote x for x in args]]
      arg
  fargtypes := CDDAR(get_database(conname, 'CONSTRUCTORMODEMAP))
--argtypes:= sublisFormal(arglist,fargtypes)
  form2HtString([conname,:[fn for arg in arglist for x in coSig
                   for ftype in fargtypes]],nil,true) where
    fn ==
      x => arg
      typ := sublisFormal(arglist,ftype)
      mathform2HtString algCoerceInteractive(arg,typ,'(OutputForm))

fortexp0 x ==
  e_to_f := getFunctionFromDomain("expression2Fortran", ['FortranCodeTools],
                                 [$OutputForm])
  f := SPADCALL(x, e_to_f)
  p := position('"%l",f)
  p < 0 => f
  l := NIL
  while p < 0 repeat
    [t,:f] := f
    l := [t,:l]
  NREVERSE ['"...",:l]

mathform2HtString form == escapeString
  form is ['QUOTE,a] => STRCONC('"'","STRCONC"/fortexp0 a)
  form is ['BRACKET,['AGGLST,:arg]] =>
    if arg is ['construct,:r] then arg := r
    arg :=
      atom arg => [arg]
      [y for x in arg | y := (x is ['QUOTE,a] => a; x)]
    tailPart := "STRCONC"/[STRCONC('",",STRINGIMAGE x) for x in rest arg]
    STRCONC('"[",STRINGIMAGE first arg,tailPart,'"]")
  form is ['BRACKET,['AGGLST,'QUOTE,arg]] =>
    if atom arg then arg := [arg]
    tailPart := "STRCONC"/[STRCONC('",",x) for x in rest arg]
    STRCONC('"[",first arg,tailPart,'"]")
  atom form => form
  "STRCONC"/fortexp0 form

--============================================================================
--                Getting Operations from Domain
--============================================================================

getDomainOpTable(dom, fromIfTrue) == getDomainOpTable2(dom, fromIfTrue, [])

getDomainOpTable2(dom, fromIfTrue, ops) ==
  $returnNowhereFromGoGet: local := true
  domname := dom.0
  conname := first domname
  abb := getConstructorAbbreviation conname
  opAlist := getOperationAlistFromLisplib conname
  "append"/[REMDUP [[op1,:fn] for [sig,slot,pred,key,:.] in u
              | ((null ops and (op1 := op)) or (op1 := memq(op, ops)))]
                 for [op,:u] in opAlist] where
    memq(op,ops) ==   --dirty trick to get 0 and 1 instead of Zero and One
      MEMQ(op,ops) => op
      EQ(op,'One)  => MEMQ(1,ops) and 1
      EQ(op,'Zero) => MEMQ(0,ops) and 0
      false
    fn ==
      sig1 := sublisFormal(rest domname,sig)
      predValue := evalDomainOpPred(dom,pred)
      info :=
        null predValue =>
          1   -- signifies not exported
        null fromIfTrue => nil
        cell := compiledLookup(op,sig1,dom) =>
          [f,:r] := cell
          f = 'nowhere => 'nowhere           --see replaceGoGetSlot
          f = function makeSpadConstant => 'constant
          f = function IDENTITY => 'constant
          f = function newGoGet =>
              substitute('_%, domname, devaluate(first(r)))
          null VECP r => systemError devaluateList r
          substitute('_%, domname, devaluate(r))
        'nowhere
      [sig1,:info]

evalDomainOpPred2(dom, pred) ==
    $predicateList : local := get_database(first(dom.0), 'PREDICATES)
    evalDomainOpPred(dom,pred)

evalDomainOpPred(dom,pred) == process(dom,pred) where
  process(dom,pred) ==
    u := convert(dom,pred)
    u = 'T => true
    evpred(dom,u)
  convert(dom,pred) ==
    pred is [op,:argl] =>
      MEMQ(op,'(AND and)) => ['AND,:[convert(dom,x) for x in argl]]
      MEMQ(op,'(OR or))   => ['OR,:[convert(dom,x) for x in argl]]
      MEMQ(op,'(NOT not)) => ['NOT,convert(dom,first argl)]
      op = 'has =>
        [arg,p] := argl
        p is ['ATTRIBUTE,a] => BREAK()
        ['HasCategory,arg,convertCatArg p]
      systemError '"unknown predicate form"
    pred = 'T => true
    systemError nil
  convertCatArg p ==
    atom p or #p = 1 => MKQ p
    ['LIST,MKQ first p,:[convertCatArg x for x in rest p]]
  evpred(dom,pred) ==
    k := POSN1(pred,$predicateList) => testBitVector(dom.3,k + 1)
    evpred1(dom,pred)
  evpred1(dom,pred) ==
    pred is [op,:argl] =>
      MEMQ(op,'(AND and)) => "and"/[evpred1(dom,x) for x in argl]
      MEMQ(op,'(OR or))   =>  "or"/[evpred1(dom,x) for x in argl]
      op = 'NOT => not evpred1(dom,first argl)
      k := POSN1(pred,$predicateList) => testBitVector(dom.3,k + 1)
      op = 'HasAttribute => BREAK()
      nil
    pred = 'T => true
    systemError '"unknown atomic predicate form"
