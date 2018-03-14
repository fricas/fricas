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

-- from alql.boot
getBrowseDatabase(kind) ==
    $includeUnexposed? : local := true
    not member(kind,'("o" "k" "c" "d" "p")) => nil
    grepConstruct('"*", INTERN kind)

--=======================================================================
--              Grepping Database libdb.text
-- Redone 12/95 for Saturn; previous function grep renamed as grepFile
-- This function now either returns a filename or a list of strings
--=======================================================================
grepConstruct(s,key,:options) == --key = a o c d p x k (all) . (aok) w (doc)
--Called from genSearch with key = "." and "w"
--key = "." means a o c d p x
--option1 = true means return the result as a file
--All searches of the database call this function to get relevant lines
--from libdb.text. Returns either a list of lines (usual case) or else
--an alist of the form ((kind . <list of lines for that kind>) ...)
  $localLibdb : local := fnameExists? '"libdb.text" and '"libdb.text"
  lines := grepConstruct1(s,key)
  lines is ['error,:.] => lines
  IFCAR options => grepSplit(lines,key = 'w)    --leave now if a constructor
  MEMQ(key,'(o a)) => dbScreenForDefaultFunctions lines --kill default lines if a/o
  lines

grepConstruct1(s,key) ==
--returns the name of file (WITHOUT .text.$SPADNUM on the end)
  $key     : local := key
  if key = 'k and          --convert 'k to 'y if name contains an "&"
    or/[s . i = char '_& for i in 0..MAXINDEX s] then key := 'y
  filter := pmTransFilter STRINGIMAGE s  --parses and-or-not form
  filter is ['error,:.] => filter        --exit on parser error
  pattern := mkGrepPattern(filter,key)  --create string to pass to "grep"
  grepConstructDo(pattern, key)  --do the "grep"---see b-saturn.boot

grepConstructDo(x, key) ==
--atom x => grepFile(x, key,'i)
  $localLibdb =>
    oldLines := purgeNewConstructorLines(grepf(x,key,false),$newConstructorList)
    newLines := grepf(x,$localLibdb,false)
    union(oldLines, newLines)
  grepf(x,key,false)

dbExposed?(line,kind) == -- does line come from an unexposed constructor?
  conname := INTERN
    kind = char 'a or kind = char 'o => dbNewConname line --get conname from middle
    dbName line
  isExposedConstructor conname

dbScreenForDefaultFunctions lines == [x for x in lines | not isDefaultOpAtt x]

isDefaultOpAtt x == x.(1 + dbTickIndex(x,4,0)) = char 'x

grepForAbbrev(s,key) ==
--checks that filter s is not * and is all uppercase; if so, look for abbrevs
  u := HGET($lowerCaseConTb,s) => ['Abbreviations,u]    --try cheap test first
  s := STRINGIMAGE s
  someLowerCaseChar := false
  someUpperCaseChar := false
  for i in 0..MAXINDEX s repeat
    c := s . i
    LOWER_-CASE_-P c => return (someLowerCaseChar := true)
    UPPER_-CASE_-P c => someUpperCaseChar := true
  someLowerCaseChar or not someUpperCaseChar => false
  pattern := DOWNCASE s
  ['Abbreviations ,:[GETDATABASE(x,'CONSTRUCTORFORM)
    for x in allConstructors() | test]] where test ==
         not $includeUnexposed? and not isExposedConstructor x => false
         a := GETDATABASE(x,'ABBREVIATION)
         match?(pattern,PNAME a) and not HGET($defaultPackageNamesHT,x)

applyGrep(x,filename) ==
  atom x => grepFile(x,filename,'i)
  $localLibdb =>
    a := purgeNewConstructorLines(grepf(x,filename,false),$newConstructorList)
    b := grepf(x,$localLibdb,false)
    grepCombine(a,b)
  grepf(x,filename,false)

grepCombine(a,b) == MSORT union(a,b)

grepf(pattern,s,not?) ==  --s=sourceFile or list of strings
  pattern is [op,:argl] =>
    op = "and" =>
      while argl is [arg,:argl] repeat
        s := grepf(arg,s,not?)  -- filter by successive greps
      s
    op = "or" =>
      targetStack := nil
      "union"/[grepf(arg,s,not?) for arg in argl]
    op = "not" =>
      not? => grepf(first argl,s,false)
      --could be the first time so have to get all of same $key
      lines := grepf(mkGrepPattern('"*",$key),s,false)
      grepf(first argl,lines,true)
    systemError nil
  option :=
    not? => 'iv
    'i
  source :=
    LISTP s => dbWriteLines s
    s
  grepFile(pattern,source,option)

pmTransFilter s ==
--result is either a string or (op ..) where op= and,or,not and arg are results
  if $browseMixedCase = true then s := DOWNCASE s
  or/[isFilterDelimiter? s.i or s.i = $charUnderscore for i in 0..MAXINDEX s]
    => (parse := pmParseFromString s) and checkPmParse parse or
        ['error,'"Illegal search string",'"\vspace{3}\center{{\em Your search string} ",escapeSpecialChars s,'" {\em has incorrect syntax}}"]
  or/[s . i = char '_* and s.(i + 1) = char '_*
      and (i=0 or s . (i - 1) ~= char $charUnderscore) for i in 0..(MAXINDEX s - 1)]
       => ['error,'"Illegal search string",'"\vspace{3}\center{Consecutive {\em *}'s are not allowed in search patterns}"]
  s

checkPmParse parse ==
  STRINGP parse => parse
  (fn parse => parse) where fn(u) ==
    u is [op,:args] =>
      MEMQ(op,'(and or not)) and and/[checkPmParse x for x in args]
    STRINGP u => true
    false
  nil

dnForm x ==
  STRINGP x => x
  x is ['not,argl] =>
    argl is ['or,:orargs]=>
       ['and, :[dnForm negate u for u in orargs]] where negate s ==
          s is ['not,argx] => argx
          ['not,s]
    argl is ['and,:andargs]=>
       ['or,:[dnForm negate u for u in andargs]]
    argl is ['not,notargl]=>
       dnForm notargl
    x
  x is ['or,:argl1] => ['or,:[dnForm u for u in argl1]]
  x is ['and,:argl2] => ['and,:[dnForm u for u in argl2]]
  x

pmParseFromString s ==
  u := ncParseFromString pmPreparse s
  dnForm flatten u where flatten s ==
    s is [op,:argl] =>
      STRINGP op => STRCONC(op,"STRCONC"/[STRCONC('" ",x) for x in argl])
      [op,:[flatten x for x in argl]]
    s

pmPreparse s == hn fn(s,0,#s) where--stupid insertion of chars to get correct parse
  hn x == SUBLISLIS('(and or not),'("and" "or" "not"),x)
  fn(s,n,siz) ==  --main function: s is string, n is origin
    n = siz => '""
    i := firstNonDelim(s,n) or return SUBSTRING(s,n,nil)
    j := firstDelim(s,i + 1) or siz
    t := gn(s,i,j - 1)
    middle :=
      member(t,'("and" "or" "not")) => t
      --the following 2 lines make commutative("*") parse correctly!!!!
      t.0 = char '_" => t
      j < siz - 1 and s.j = char '_( => t
      STRCONC(char '_",t,char '_")
    STRCONC(SUBSTRING(s,n,i - n),middle,fn(s,j,siz))
  gn(s,i,j) ==    --replace each underscore by 4 underscores!
    n := or/[k for k in i..j | s.k = $charUnderscore] =>
      STRCONC(SUBSTRING(s,i,n - i + 1),$charUnderscore,gn(s,n + 1,j))
    SUBSTRING(s,i,j - i + 1)

firstNonDelim(s,n) ==  or/[k for k in n..MAXINDEX s | not isFilterDelimiter? s.k]
firstDelim(s,n) ==  or/[k for k in n..MAXINDEX s | isFilterDelimiter? s.k]

isFilterDelimiter? c == MEMQ(c,$pmFilterDelimiters)

grepSplit(lines,doc?) ==
  if doc? then
    instream2 := OPEN STRCONC(getEnv '"AXIOM",'"/algebra/libdb.text")
  cons := atts := doms := nil
  while lines is [line, :lines] repeat
    if doc? then
        N:=PARSE_-INTEGER dbPart(line,1,-1)
        if NUMBERP N then
           FILE_-POSITION(instream2,N)
           line := read_line instream2
    kind := dbKind line
    not $includeUnexposed? and not dbExposed?(line,kind) => 'skip
    (kind = char 'a or kind = char 'o) and isDefaultOpAtt line => 'skip
    PROGN
      kind = char 'c => cats := insert(line,cats)
      kind = char 'd => doms := insert(line,doms)
      kind = char 'x => defs := insert(line,defs)
      kind = char 'p => paks := insert(line,paks)
      kind = char 'a => atts := insert(line,atts)
      kind = char 'o => ops :=  insert(line,ops)
      kind = char '_- => 'skip                --for now
      systemError 'kind
  if doc? then CLOSE instream2
  [['"attribute",:NREVERSE atts],
     ['"operation",:NREVERSE ops],
       ['"category",:NREVERSE cats],
         ['"domain",:NREVERSE doms],
           ['"package",:NREVERSE paks]
--           ['"default_ package",:NREVERSE defs]   -- drop defaults
               ]

mkUpDownPattern s == recurse(s,0,#s) where
  recurse(s,i,n) ==
    i = n => '""
    STRCONC(fixchar(s.i),recurse(s,i + 1,n))
  fixchar(c) ==
    ALPHA_-CHAR_-P c =>
      STRCONC(char '_[,CHAR_-UPCASE c,CHAR_-DOWNCASE c,char '_])
    c

mkGrepPattern(s,key) ==
  --called by grepConstruct1 and grepf
  atom s => mkGrepPattern1(s,key)
  [first s,:[mkGrepPattern(x,key) for x in rest s]]

mkGrepPattern1(x,:options) == --called by mkGrepPattern (and grepConstructName?)
  $options : local := options
  s := STRINGIMAGE x
--s := DOWNCASE STRINGIMAGE x
  addOptions remUnderscores addWilds split(g s,char '_*) where
    addWilds sl ==    --add wild cards (sl is list of parts between *'s)
      IFCAR sl = '"" => h(IFCDR sl,[$wild1])
      h(sl,nil)
    g s  ==    --remove "*"s around pattern for text match
      not MEMQ('w,$options) => s
      if s.0 = char '_* then s := SUBSTRING(s,1,nil)
      if s.(k := MAXINDEX s) = char '_* then s := SUBSTRING(s,0,k)
      s
    h(sl,res) == --helper for wild cards
      sl is [s,:r] => h(r,[$wild1,s,:res])
      res := rest res
      if not MEMQ('w,$options) then
        if first res ~= '"" then res := ['"`",:res]
        else if res is [.,p,:r] and p = $wild1 then res := r
      "STRCONC"/NREVERSE res
    remUnderscores s ==
      (k := charPosition(char $charUnderscore,s,0)) < MAXINDEX s =>
        STRCONC(SUBSTRING(s,0,k),'"[",s.(k + 1),'"]",
                remUnderscores(SUBSTRING(s,k + 2,nil)))
      s
    split(s,char) ==
      max := MAXINDEX s + 1
      f := -1
      [SUBSTRING(s,i,f-i)
        while ((i := f + 1) <= max) and (f := charPosition(char,s,i))]
    charPosition(c,t,startpos) ==  --honors underscores
      n := SIZE t
      if startpos < 0 or startpos > n then error "index out of range"
      k:= startpos
      for i in startpos .. n-1 while c ~= ELT(t,i)
        or i > startpos and ELT(t,i-1) = '__ repeat  (k := k+1)
      k
    addOptions s ==  --add front anchor
      --options a o c d p x   denote standard items
      --options w             means  comments
      --option  t             means  text
      --option  s             means  signature
      --option  n             means  number of arguments
      --option  i             means  predicate
      --option  none          means  NO PREFIX
      one := ($options is [x,:$options] and x => x; '"[^x]")
      tick := '"[^`]*`"
      one = 'w => s
      one = 'none => (s = '"`" => '"^."; STRCONC('"^",s))
      prefix :=
        one = 't => STRCONC(tick,tick,tick,tick,tick,".*")
        one = 'n => tick
        one = 'i => STRCONC(tick,tick,tick,tick)
        one = 's => STRCONC(tick,tick,tick)
--      true => '""    ----> never put on following prefixes
        one = 'k => '"[cdp]"
        one = 'y => '"[cdpx]"
        STRINGIMAGE one
      s = $wild1 => STRCONC('"^",prefix)
      STRCONC('"^",prefix,s)

oPage(a,:b) == --called by \spadfun{opname}
  oSearch (IFCAR b or a) --always take slow path

oPageFrom(opname,conname) == --called by \spadfunFrom{opname}{conname}
  htPage := htInitPage(nil,nil) --create empty page and fill in needed properties
  htpSetProperty(htPage,'conform,conform := getConstructorForm conname)
  htpSetProperty(htPage,'kind,STRINGIMAGE GETDATABASE(conname,'CONSTRUCTORKIND))
  itemlist := assoc(opname,koOps(conform,nil)) --all operations name "opname"
  null itemlist => systemError [conform,'" has no operation named ",opname]
  opAlist := [itemlist]
  dbShowOperationsFromConform(htPage,'"operation",opAlist)

aPage(a,:b) ==  --called by \spadatt{a}
  $attributeArgs : local := nil
  arg := IFCAR b or a
  s   := pmParseFromString STRINGIMAGE arg
  searchOn :=
    ATOM s => s
    IFCAR s
  $attributeArgs : local := IFCAR IFCDR s
  aSearch searchOn
--must recognize that not all attributes can be found in database
--e.g. constant(deriv) is not but appears in a conditional in LODO

spadType(x) ==  --called by \spadtype{x} from HyperDoc
  s := PNAME x
  form := ncParseFromString s or
            systemError ['"Argument: ",s,'" to spadType won't parse"]
  if atom form then form := [form]
  op    := opOf form
  looksLikeDomainForm form => APPLY(function conPage,form)
  conPage(op)

looksLikeDomainForm x ==
  entry := getCDTEntry(opOf x,true) or return false
  coSig := LASSOC('coSig,CDDR entry)
  k := #coSig
  atom x => k = 1
  k ~= #x => false
  and/[p for key in rest coSig for arg in rest x] where
    p ==
      key => looksLikeDomainForm arg
      not IDENTP arg

spadSys(x) ==   --called by \spadsyscom{x}
  s := PNAME x
  if s.0 = char '_) then s := SUBSTRING(s,1,nil)
  form := ncParseFromString s or
           systemError ['"Argument: ",s,'" to spadType won't parse"]
  htSystemCommands PNAME opOf form

--=======================================================================
--                   Name and General Search
--=======================================================================
aokSearch filter ==  genSearch(filter,true)  --"General" from HD (see man0.ht)
--General search for constructs but NOT documentation

genSearch(filter,:options) == --"Complete" from HD (see man0.ht) and aokSearch
--General + documentation search
  null (filter := checkFilter filter) => nil  --in case of filter error
  filter = '"*" => htErrorStar()
  includeDoc? := not IFCAR options
--give summaries for how many a o c d p x match filter
  regSearchAlist := grepConstruct(STRINGIMAGE filter,".",true)
  regSearchAlist is ['error,:.] => bcErrorPage regSearchAlist
  key := removeSurroundingStars filter
  if includeDoc? then
    docSearchAlist := grepConstruct(key,'w,true)
    docSearchAlist is ['error,:.] => bcErrorPage docSearchAlist
    docSearchAlist := [x for x in docSearchAlist | x.0 ~= char 'x]--drop defaults
  genSearch1(filter,genSearchTran regSearchAlist,genSearchTran docSearchAlist)

genSearchTran alist == [[x,y,:y] for [x,:y] in alist]


genSearch1(filter,reg,doc) ==
  regSearchAlist := searchDropUnexposedLines reg
  docSearchAlist := searchDropUnexposedLines doc
  key := removeSurroundingStars filter
  regCount := searchCount regSearchAlist
  docCount := searchCount docSearchAlist
  count := regCount + docCount
  count = 0 => emptySearchPage('"entry",filter,true)
  count = 1 =>
    alist := (regCount = 1 => regSearchAlist; docSearchAlist)
    showNamedConstruct(or/[x for x in alist | CADR x])
  summarize? :=
    docSearchAlist => true
    nonEmpties := [pair for pair in regSearchAlist | #(CADR pair) > 0]
    not(nonEmpties is [pair])
  not summarize? => showNamedConstruct pair
  -----------generate a summary page---------------------------
  plural :=
    $exposedOnlyIfTrue => '"exposed entries match"
    '"entries match"
  prefix := pluralSay(count,'"", plural)
  emfilter := ['"{\em ",escapeSpecialChars STRINGIMAGE filter,'"}"]
  header := [:prefix,'" ",:emfilter]
  page := htInitPage(header,nil)
  htpSetProperty(page,'regSearchAlist,regSearchAlist)
  htpSetProperty(page,'docSearchAlist,docSearchAlist)
  htpSetProperty(page,'filter,filter)
  if docSearchAlist then
      dbSayItems(['"{\bf Construct Summary:} ",regCount],'"name matches",'"names match")
  for [kind,:pair] in regSearchAlist for i in 0.. | #(first pair) > 0 repeat
    bcHt '"\newline{}"
    htSayStandard '"\tab{2}"
    genSearchSay(pair,summarize?,kind,i,'showConstruct)
  if docSearchAlist then
    htSaySaturn '"\bigskip{}"
    dbSayItems(['"\newline{\bf Documentation Summary:} ",docCount],'"mention",'"mentions",'" of {\em ",key,'"}")
    for [kind,:pair] in docSearchAlist for i in 0.. | #(first pair) > 0 repeat
      bcHt "\newline{}"
      htSayStandard '"\tab{2}"
      genSearchSay(pair,true,kind,i,'showDoc)
  htShowPageStar()
searchDropUnexposedLines alist ==
  [[op,[pred for line in lines | pred],:lines] for [op,.,:lines] in alist] where
    pred ==
      not $exposedOnlyIfTrue or dbExposed?(line,dbKind line) => line
      nil

repeatSearch(htPage,newValue) ==
  $exposedOnlyIfTrue := newValue
  filter := htpProperty(htPage,'filter)
  reg    := htpProperty(htPage,'regSearchAlist)
  doc    := htpProperty(htPage,'docSearchAlist)
  reg => genSearch1(filter,reg,doc)
  docSearch1(filter,doc)

searchCount u == +/[# y for [x,y,:.] in u]

showDoc(htPage,count) ==
  showIt(htPage,count,htpProperty(htPage,'docSearchAlist))

showConstruct(htPage,count) ==
  showIt(htPage,count,htpProperty(htPage,'regSearchAlist))

showIt(htPage,index,searchAlist) ==
  filter      := htpProperty(htPage,'filter)
  [relativeIndex,n] := DIVIDE(index,8)
  relativeIndex = 0 => showNamedConstruct(searchAlist.n)
  [kind,items,:.] := searchAlist . n
  for j in 1.. while j < relativeIndex repeat items := rest items
  firstName := dbName first items --select name then gather all of same name
  lines := [line for line in items while dbName line = firstName]
  showNamedConstruct [kind,nil,:lines]

showNamedConstruct([kind,.,:lines]) == dbSearch(lines,kind,'"")

genSearchSay(pair,summarize,kind,who,fn) ==
  [u,:fullLineList] := pair
  count := #u
  uniqueCount := genSearchUniqueCount u
  short := summarize and uniqueCount >= $browseCountThreshold
  htMakePage
    [['bcLinks,[menuButton(),'"",'genSearchSayJump,[fullLineList,kind]]]]
  if count = 0 then htSayList(['"{\em No ", kind, '"} "])
  else if count = 1 then
    htSayList(['"{\em 1 ", kind, '"} "])
  else
    htSayList(['"{\em ", count, '" ", pluralize kind, '"} "])
  short => 'done
  if uniqueCount ~= 1 then
    htSayStandard '"\indent{4}"
    htSay '"\newline "
    htBeginTable()
  lastid := nil
  groups := organizeByName u
  i := 1
  for group in groups repeat
    id := dbGetName first group
    if $includeUnexposed? then
      exposed? := or/[dbExposed?(item,dbKind item) for item in group]
    bcHt '"{"
    if $includeUnexposed? then
        exposed? => htBlank()
        htSayUnexposed()
    htMakePage [['bcLinks, [id,'"",fn,who + 8*i]]]
    i := i + #group
    bcHt '"}"
  if uniqueCount ~= 1 then
     htEndTable()
     htSayStandard '"\indent{0}"

organizeByName u ==
  [[(u := rest u; x) while u and head = dbName (x := first u)]
      while u and (head := dbName first u)]

genSearchSayJump(htPage,[lines,kind]) ==
  filter := htpProperty(htPage,'filter)
  dbSearch(lines,kind,filter)

genSearchUniqueCount(u) ==
--count the unique number of items (if less than $browseCountThreshold)
  count := 0
  lastid := nil
  for item in u while count < $browseCountThreshold repeat
    id := dbGetName item
    if id ~= lastid then
      count := count + 1
      lastid := id
  count

dbGetName line == SUBSTRING(line,1,charPosition($tick,line,1) - 1)

pluralSay(count,singular,plural) ==
    count = 0 => concat('"No ", singular)
    count = 1 => concat('"1 ", singular)
    concat(count, '" ", plural)


--=======================================================================
--                   Documentation Search
--=======================================================================
docSearch filter ==  --"Documentation" from HD (see man0.ht)
  null (filter := checkFilter filter) => nil  --in case of filter error
  filter = '"*" => htErrorStar()
  key := removeSurroundingStars filter
  docSearchAlist := grepConstruct(filter,'w,true)
  docSearchAlist is ['error,:.] => bcErrorPage docSearchAlist
  docSearchAlist := [x for x in docSearchAlist | x.0 ~= char 'x] --drop defaults
  docSearch1(filter,genSearchTran docSearchAlist)

docSearch1(filter,doc) ==
  docSearchAlist := searchDropUnexposedLines doc
  count := searchCount docSearchAlist
  count = 0 => emptySearchPage('"entry",filter,true)
  count = 1 => showNamedConstruct(or/[x for x in docSearchAlist | CADR x])
  prefix := pluralSay(count,'"entry matches",'"entries match")
  emfilter := ['"{\em ",escapeSpecialChars STRINGIMAGE filter,'"}"]
  header := [:prefix,'" ",:emfilter]
  page := htInitPage(header,nil)
  htpSetProperty(page,'docSearchAlist,docSearchAlist)
  htpSetProperty(page,'regSearchAlist,nil)
  htpSetProperty(page,'filter,filter)
  dbSayItems(['"\newline Documentation Summary: ",count],'"mention",'"mentions",'" of {\em ",filter,'"}")
  for [kind,:pair] in docSearchAlist for i in 0.. | #(first pair) > 0 repeat
    bcHt '"\newline{}"
    htSayStandard '"\tab{2}"
    genSearchSay(pair,true,kind,i,'showDoc)
  htShowPageStar()

removeSurroundingStars filter ==
  key := STRINGIMAGE filter
  if key.0 = char '_* then key := SUBSTRING(key,1,nil)
  if key.(max := MAXINDEX key) = char '_* then key := SUBSTRING(key,0,max)
  key

showNamedDoc([kind,:lines],index) ==
  dbGather(kind,lines,index - 1,true)

sayDocMessage message ==
  htSay('"{\em ")
  if message is [leftEnd,left,middle,right,rightEnd] then
    htSayList([leftEnd, left, '"}"])
    if left ~= '"" and left.(MAXINDEX left) = $blank then htBlank()
    htSay middle
    if right ~= '"" and right.0 = $blank then htBlank()
    htSayList(['"{\em ", right, rightEnd])
  else
    htSay message
  htSay ('"}")

stripOffSegments(s,n) ==
  progress := true
  while n > 0 and progress = true repeat
    n := n - 1
    k := charPosition(char '_`,s,0)
    new := SUBSTRING(s,k + 1,nil)
    #new < #s => s := new
    progress := false
  n = 0 => s
  nil

replaceTicksBySpaces s ==
  n := -1
  max := MAXINDEX s
  while (n := charPosition(char '_`,s,n + 1)) <= max repeat SETELT(s,n,char '_ )
  s

checkFilter filter ==
  filter := STRINGIMAGE filter
  filter = '"" => '"*"
  trimString filter

aSearch filter ==  --called from HD (man0.ht): general attribute search
  null (filter := checkFilter filter) => nil  --in case of filter error
  dbSearch(grepConstruct(filter,'a),'"attribute",filter)

oSearch filter == -- called from HD (man0.ht): operation search
  opAlist := opPageFastPath filter => opPageFast opAlist
  key := 'o
  null (filter := checkFilter filter) => nil  --in case of filter error
  filter = '"*" => grepSearchQuery('"operation",[filter,key,'"operation",'oSearchGrep])
  oSearchGrep(filter,key,'"operation")

oSearchGrep(filter,key,kind) == --called from grepSearchQuery/oSearch
  dbSearch(grepConstruct(filter,'o),kind,filter)

grepSearchQuery(kind,items) ==
  page := htInitPage('"Query Page",nil)
  htpSetProperty(page,'items,items)
  htQuery(['"{\em Do you want a list of {\em all} ",pluralize kind,'"?\vspace{1}}"],'grepSearchJump,true)
  htShowPage()

cSearch filter ==  --called from HD (man0.ht): category search
   constructorSearch(checkFilter filter,'c,'"category")

dSearch filter ==  --called from HD (man0.ht): domain search
   constructorSearch(checkFilter filter,'d,'"domain")

pSearch filter ==  --called from HD (man0.ht): package search
   constructorSearch(checkFilter filter,'p,'"package")

xSearch filter ==  --called from HD (man0.ht): default package search
   constructorSearch(checkFilter filter,'x,'"default package")

kSearch filter ==  --called from HD (man0.ht): constructor search (no defaults)
   constructorSearch(checkFilter filter,'k,'"constructor")

ySearch filter == --called from conPage: like kSearch but defaults included
  constructorSearch(checkFilter filter,'y,'"constructor")

constructorSearch(filter,key,kind) ==
  null filter => nil      --in case of filter error
  (parse := conSpecialString? filter) => conPage parse
  pageName := LASSOC(DOWNCASE filter,'(("union" . DomainUnion)("record" . DomainRecord)("mapping" . DomainMapping) ("enumeration" . DomainEnumeration))) =>
    downlink pageName
  name := (STRINGP filter => INTERN filter; filter)
  if u := HGET($lowerCaseConTb,name) then filter := STRINGIMAGE first u
  line := conPageFastPath DOWNCASE filter =>
    code := dbKind line
    newkind :=
      code = char 'p => '"package"
      code = char 'd => '"domain"
      code = char 'c => '"category"
      nil
    kind = '"constructor" or kind = newkind => kPage(line, [])
    page := htInitPage('"Query Page",nil)
    htpSetProperty(page,'line,line)
    message :=
      ['"{\em ",dbName line,'"} is not a {\em ",kind,'"} but a {\em ",
        newkind,'"}. Would you like to view it?\vspace{1}"]
    htQuery(message, 'grepConstructorSearch,true)
    htShowPage()
  filter = '"*" => grepSearchQuery(kind,[filter,key,kind,'constructorSearchGrep])
  constructorSearchGrep(filter,key,kind)

grepConstructorSearch(htPage, yes) == kPage(htpProperty(htPage, 'line), [])

conSpecialString?(filter) == conSpecialString2?(filter, false)

conSpecialString2?(filter, secondTime) ==
  parse :=
    words := string2Words filter is [s] => ncParseFromString s
    and/[not member(x,'("and" "or" "not")) for x in words] => ncParseFromString filter
    false
  null parse => nil
  form := conLowerCaseConTran parse
  MEMQ(IFCAR form, '(and or not)) or CONTAINED("*", form) => nil
  filter = '"Mapping" =>nil
  u := kisValidType form => u
  secondTime => false
  u := "STRCONC"/[string2Constructor x for x in dbString2Words filter]
  conSpecialString2?(u, true)

dbString2Words l ==
  i := 0
  [w while dbWordFrom(l,i) is [w,i]]

$dbDelimiters := [char " " , char "(", char ")"]

dbWordFrom(l,i) ==
  maxIndex := MAXINDEX l
  while maxIndex >= i and l.i = char " " repeat i := i + 1
  if maxIndex >= i and member(l.i, $dbDelimiters) then return [l.i, i + 1]
  k := or/[j for j in i..maxIndex | not member(l.j, $dbDelimiters)] or return nil
  buf := '""
  while k <= maxIndex and not member(c := l.k, $dbDelimiters) repeat
    ch :=
      c = char '__   => l.(k := 1+k)  --this may exceed bounds
      c
    buf := STRCONC(buf,ch)
    k := k + 1
  [buf,k]

conLowerCaseConTran x ==
  IDENTP x => IFCAR HGET($lowerCaseConTb, x) or x
  atom x   => x
  [conLowerCaseConTran y for y in x]

string2Constructor x ==
  not STRINGP x => x
  IFCAR HGET($lowerCaseConTb, INTERN DOWNCASE x) or x

constructorSearchGrep(filter,key,kind) ==
  dbSearch(grepConstruct(filter,key),kind,filter)

grepSearchJump(htPage,yes) ==
  [filter,key,kind,fn] := htpProperty(htPage,'items)
  FUNCALL(fn,filter,key,kind)

--=======================================================================
--            Branch Functions After Database Search
--=======================================================================
dbSearch(lines,kind,filter) == --called by attribute, operation, constructor search
  lines is ['error,:.] => bcErrorPage lines
  null filter => nil      --means filter error
  lines is ['Abbreviations,:r] => dbSearchAbbrev(lines,kind,filter)
  if member(kind,'("attribute" "operation")) then --should not be necessary!!
    lines := dbScreenForDefaultFunctions lines
  count := #lines
  count = 0 => emptySearchPage(kind, filter, false)
  member(kind,'("attribute" "operation")) => dbShowOperationLines(kind,lines)
  dbShowConstructorLines lines

dbSearchAbbrev([.,:conlist],kind,filter) ==
  null conlist => emptySearchPage('"abbreviation", filter, false)
  kind := intern kind
  if kind ~= 'constructor then
    conlist := [x for x in conlist | LASSOC('kind,IFCDR IFCDR x) = kind]
  conlist is [[nam,:.]] => conPage DOWNCASE nam
  cAlist := [[con,:true] for con in conlist]
  htPage := htInitPage('"",nil)
  htpSetProperty(htPage,'cAlist,cAlist)
  htpSetProperty(htPage,'thing,nil)
  return dbShowCons(htPage,'names)
  page := htInitPage([#conlist,
    '" Abbreviations Match {\em ",STRINGIMAGE filter,'"}"],nil)
  for [nam,abbr,:r] in conlist repeat
    kind := LASSOC('kind,r)
    htSayList(['"\newline{\em ", s := STRINGIMAGE abbr])
    htSayStandard '"\tab{10}"
    htSay '"}"
    htSay kind
    htSayStandard '"\tab{19}"
    bcCon nam
  htShowPage()

--=======================================================================
--                   Selectable Search
--=======================================================================
detailedSearch(filter) ==
  page := htInitPage('"Detailed Search with Options",nil)
  filter   := escapeSpecialChars PNAME filter
  bcHt '"Select what you want to search for, then click on {\em Search} below"
  bcHt '"\newline{\it Note:} Logical searches using {\em and}, {\em or}, and {\em not} are not permitted here."
  htSayHrule()
  htMakePage '(
    (text . "\newline")
    (bcRadioButtons which
      (  "\tab{3}{\em Operations}"
         ((text . "\newline\space{3}")
          (text . "name")       (bcStrings (14 "*" opname EM))
          (text . " \#args")    (bcStrings (1  "*" opnargs EM))
          (text . " signature") (bcStrings (14 "*" opsig EM))
          (text . "\vspace{1}\newline "))
         ops)
      (  "\tab{3}{\em Attributes}"
         ((text . "\newline\space{3}")
          (text . "name")       (bcStrings (14 "*" attrname EM))
          (text . " \#args ")   (bcStrings (1  "*" attrnargs EM))
          (text . " arguments ")(bcStrings (14 "*" attrargs EM))
          (text . "\vspace{1}\newline "))
         attrs)
      (  "\tab{3}{\em Constructors}"
         ((text . "\tab{17}")
          (bcButtons (1 cats)) (text . " {\em categories} ")
          (bcButtons (1 doms)) (text . " {\em domains} ")
          (bcButtons (1 paks)) (text . " {\em packages} ")
          (bcButtons (1 defs)) (text . " {\em defaults} ")
          (text . "\newline\tab{3}")
          (text . "name")   (bcStrings (14 "*" conname EM))
          (text . " \#args") (bcStrings (1  "*" connargs EM))
          (text . "signature") (bcStrings (14 "*" consig EM))
          (text . "\vspace{1}\newline "))
          cons)
--      (   "\tab{3}{\em Documentation}"
--          ((text . "\tab{26}key")
--           (bcStrings (28 "*" docfilter EM)))
--          doc)
                )
    (text . "\vspace{1}\newline\center{ ")
    (bcLinks ("\box{Search}" "" generalSearchDo NIL))
    (text . "}"))
  htShowPage()

generalSearchDo(htPage,flag) ==
--$exposedOnlyIfTrue := (flag => 'T; nil)
  $htPage := htPage
  alist := htpInputAreaAlist htPage
  which := htpButtonValue(htPage,'which)
  selectors :=
    which = 'cons => '(conname connargs consig)
    which = 'ops  => '(opname  opnargs  opsig)
    '(attrname attrnargs attrargs)
  name := generalSearchString(htPage,selectors.0)
  nargs:= generalSearchString(htPage,selectors.1)
  npat := standardizeSignature generalSearchString(htPage,selectors.2)
  kindCode :=
    which = 'ops => char 'o
    which = 'attrs => char 'a
    acc := '""
    if htButtonOn?(htPage,'cats) then acc := STRCONC(char 'c,acc)
    if htButtonOn?(htPage,'doms) then acc := STRCONC(char 'd,acc)
    if htButtonOn?(htPage,'paks) then acc := STRCONC(char 'p,acc)
    if htButtonOn?(htPage,'defs) then acc := STRCONC(char 'x,acc)
    n := #acc
    n = 0 or n = 4 => '"[cdpx]"
    n = 1 => acc
    STRCONC(char '_[,acc,char '_])
  form := mkDetailedGrepPattern(kindCode,name,nargs,npat)
  lines := applyGrep(form,'libdb)
--lines := dbReadLines resultFile
  if MEMQ(which,'(ops attrs)) then lines := dbScreenForDefaultFunctions lines
  kind :=
    which = 'cons =>
      n = 1 =>
        htButtonOn?(htPage,'cats) => '"category"
        htButtonOn?(htPage,'doms) => '"domain"
        htButtonOn?(htPage,'paks) => '"package"
        '"default package"
      '"constructor"
    which = 'ops  => '"operation"
    '"attribute"
  null lines => emptySearchPage(kind, nil, false)
  dbSearch(lines,kind,'"filter")

generalSearchString(htPage,sel) ==
  string := htpLabelInputString(htPage,sel)
  string = '"" => '"*"
  string

htButtonOn?(htPage,key) ==
  LASSOC(key,htpInputAreaAlist htPage) is [a,:.] and a = '" t"

mkDetailedGrepPattern(kind,name,nargs,argOrSig) == main where
  main ==
    nottick := '"[^`]"
    name := replaceGrepStar name
    firstPart :=
      $saturn => STRCONC(char '_^,name)
      STRCONC(char '_^,kind,name)
    nargsPart := replaceGrepStar nargs
    exposedPart := char '_.   --always get exposed/unexposed
    patPart := replaceGrepStar argOrSig
    simp STRCONC(conc(firstPart,conc(nargsPart,conc(exposedPart, patPart))),$tick)
  conc(a,b) ==
    b = '"[^`]*" or b = char '_. => a
    STRCONC(a,$tick,b)
  simp a ==
    m := MAXINDEX a
    m > 6 and a.(m-5) = char '_[ and a.(m-4) = char '_^
      and     a.(m-3) = $tick    and a.(m-2) = char '_]
          and a.(m-1) = char '_* and a.m = $tick
            => simp SUBSTRING(a,0,m-5)
    a

replaceGrepStar s ==
  s = "" => s
  final := MAXINDEX s
  i := charPosition(char '_*,s,0)
  i > final => s
  STRCONC(SUBSTRING(s,0,i),'"[^`]*",replaceGrepStar SUBSTRING(s,i + 1,nil))

standardizeSignature(s) == underscoreDollars
  s.0 = char '_( => s
  k := STRPOS('"->",s,0,nil) or return s --will fail except perhaps on constants
  s.(k - 1) = char '_) => STRCONC(char '_(,s)
  STRCONC(char '_(,SUBSTRING(s,0,k),char '_),SUBSTRING(s,k,nil))

underscoreDollars(s) == fn(s,0,MAXINDEX s) where
  fn(s,i,n) ==
    i > n => '""
    (m := charPosition(char '_$,s,i)) > n => SUBSTRING(s,i,nil)
    STRCONC(SUBSTRING(s,i,m - i),'"___$",fn(s,m + 1,n))

--=======================================================================
--                         I/O Code
--=======================================================================

getTempPath kind == mkGrepFile kind

dbWriteLines(s, :options) ==
  pathname := IFCAR options or getTempPath 'source
  $outStream: local := MAKE_-OUTSTREAM pathname
  for x in s repeat writedb x
  SHUT $outStream
  pathname

dbReadLines target == --AIX only--called by grepFile
  instream := OPEN target
  lines := [read_line instream while not EOFP instream]
  CLOSE instream
  lines

dbGetCommentOrigin line ==
--Given a comment line in comdb, returns line in libdb pointing to it
--Comment lines have format  [dcpxoa]xxxxxx`ccccc... where
--x's give pointer into libdb, c's are comments
  firstPart := dbPart(line,1,-1)
  key := INTERN SUBSTRING(firstPart,0,1)    --extract this and throw away
  address := SUBSTRING(firstPart, 1, nil)   --address in libdb
  instream := OPEN grepSource key           --this always returns libdb now
  FILE_-POSITION(instream,PARSE_-INTEGER address)
  line := read_line instream
  CLOSE instream
  line

grepSource key ==
  key = 'libdb   => STRCONC($SPADROOT,'"/algebra/libdb.text")
  key = 'gloss   => STRCONC($SPADROOT,'"/algebra/glosskey.text")
  key = $localLibdb => $localLibdb
  mkGrepTextfile
    MEMQ(key, '(_. a c d k o p x)) => 'libdb
    'comdb

mkGrepTextfile s == STRCONC($SPADROOT,"/algebra/", STRINGIMAGE s, '".text")

mkGrepFile s ==  --called to generate a path name for a temporary grep file
  prefix :=
    $standard or $aixTestSaturn => '"/tmp/"
    STRCONC($SPADROOT,'"/algebra/")
  suffix := getEnv '"SPADNUM"
  STRCONC(prefix, PNAME s,'".txt.", suffix)

--=======================================================================
--                     Grepping Code
--=======================================================================

grepFile(pattern, key, option) ==
  options := [option]
  source := grepSource key
  lines :=
    not PROBE_-FILE source => NIL
    $standard or $aixTestSaturn =>
    -----AIX Version----------
      target := getTempPath 'target
      casepart :=
        MEMQ('iv,options)=> '"-vi"
        '"-i"
      command := STRCONC('"grep ", casepart, '" '", pattern, '"' ", source)
      OBEY STRCONC(command, '" > ",target)
      dbReadLines target
    ----Windows Version------
    invert? := MEMQ('iv, options)
    GREP(source, pattern, false, not invert?)
  dbUnpatchLines lines

dbUnpatchLines lines ==  --concatenate long lines together, skip blank lines
  dash := char '_-
  acc := nil
  while lines is [line, :lines] repeat
    #line = 0 => 'skip     --skip blank lines
    acc :=
      line.0 = dash and line.1 = dash =>
        [STRCONC(first acc,SUBSTRING(line,2,nil)),:rest acc]
      [line,:acc]
  -- following call to NREVERSE needed to keep lines properly sorted
  NREVERSE acc  ------> added by BMT 12/95
