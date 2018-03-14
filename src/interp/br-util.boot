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

--====================> WAS b-util.boot <================================

--=======================================================================
--                     AXIOM Browser
-- Initial entry is from man0.ht page to one of these functions:
--   kSearch (cSearch, dSearch, or pSearch), for constructors
--   oSearch, for operations
--   aokSearch, for general search
--   docSearch, for documentation search
--   genSearch, for complete search
--=======================================================================

browserAutoloadOnceTrigger() == nil

----------------------> Global Variables <-----------------------
$includeUnexposed? := true   --default setting
$tick := char '_`            --field separator for database files
$charUnderscore := ('__)     --needed because of parser bug
$wild1 := '"[^`]*"           --phrase used to convert keys to grep strings
$browseCountThreshold := 10  --the maximum number of names that will display
                             --on a general search
$opDescriptionThreshold := 4 --if <= 4 operations with unique name, give desc
                             --otherwise, give signatures
$browseMixedCase := true     --distinquish case in the browser?
$docTable := nil             --cache for documentation table
$conArgstrings := nil        --bound by conPage so that kPage
                             --will display arguments if given
$conformsAreDomains  := false     --are all arguments of a constructor given?
$returnNowhereFromGoGet := false  --special branch out for goget for browser
$dbDataFunctionAlist := nil       --set by dbGatherData
$domain   := nil             --bound in koOps
$infovec  := nil             --bound in koOps
$predvec  := nil             --bound in koOps
$exposedOnlyIfTrue := nil    --see repeatSearch, dbShowOps, dbShowCon
$bcMultipleNames := nil      --see bcNameConTable
$docTableHash := MAKE_-HASHTABLE 'EQUAL  --see dbExpandOpAlistIfNecessary
$groupChoice := nil  --see dbShowOperationsFromConform

------------------> Initial Settings <---------------------
$pmFilterDelimiters := [char '_(,char '_),char '_ ]
$dbKindAlist :=
  [[char 'a,:'"attribute"],[char 'o,:'"operation"],
    [char 'd,:'"domain"],[char 'p,:'"package"],
      [char 'c,:'"category"],[char 'x,:'"default_ package"]]
$OpViewTable := '(
  (names           "Name"      "Names"           dbShowOpNames)
  (documentation   "Name"      "Names"           dbShowOpDocumentation)
  (domains         "Domain"    "Domains"         dbShowOpDomains)
  (signatures      "Signature" "Signatures"      dbShowOpSignatures)
  (parameters      "Form"      "Forms"           dbShowOpParameters)
  (origins         "Origin"    "Origins"         dbShowOpOrigins)
  (implementation  nil         "Implementation Domains" dbShowOpImplementations)
  (conditions      "Condition" "Conditions"      dbShowOpConditions))

bcBlankLine() == bcHt '"\vspace{1}\newline "

pluralize k ==
  k = '"child" => '"children"
  k = '"category" => '"categories"
  k = '"entry" => '"entries"
  STRCONC(k,'"s")

capitalize s ==
  LASSOC(s,'( _
      ("domain"   . "Domain") _
      ("category" . "Category") _
      ("package"  . "Package") _
      ("default package" . "Default Package"))) or
    res := COPY_-SEQ s
    SETELT(res,0,UPCASE res.0)
    res

escapeSpecialIds u ==   --very expensive function
  x := LASSOC(u,$htCharAlist) => [x]
  #u = 1 =>
    member(u, $htSpecialChars) => [CONCAT('"_\", u)]
    [u]
  c := char u.0
  or/[c = char y for y in $htSpecialChars] =>
    [CONCAT('"_\",u)]
  [u]

escapeString com ==   --this makes changes on single comment lines
-- was htexCom
  look := 0
  while look repeat
    look >= SIZE com => look := []
    look := STRPOSL ('"${}#%", com, look, [])
    if look then
      com := RPLACSTR (com,look,0,'"\")  --note RPLACSTR copies!!!
      look := look + 2
  com

htPred2English(x,:options) ==
  $emList :local := IFCAR options   --list of identifiers to be emphasised
  $precList: local := '((OR 10 . "or") (AND 9 . "and")
     (_< 5) (_<_= 5) (_> 5) (_>_= 5) (_= 5) (_^_= 5) (or 10) (and 9))
  fn(x,100) where
    fn(x,prec) ==
      x is [op,:l] =>
        LASSOC(op,$precList) is [iprec,:rename] =>
          if iprec > prec then htSay '"("
          fn(first l,iprec)
          for y in rest l repeat
            htSayList(['" ", rename or op, '" "])
            fn(y,iprec)
          if iprec > prec then htSay '")"
        if prec < 5 then htSay '"("
        gn(x,op,l,prec)
        if prec < 5 then htSay '")"
      x = 'etc => htSay '"..."
      IDENTP x and not MEMQ(x,$emList) => htSay escapeSpecialIds PNAME x
      htSay form2HtString(x,$emList)
    gn(x,op,l,prec) ==
      MEMQ(op,'(NOT not)) =>
        htSay('"not ")
        fn(first l,0)
      op = 'HasCategory =>
        bcConform(first l,$emList)
        htSay('" has ")
        bcConform(CADADR l,$emList)
      op = 'HasAttribute => BREAK()
      MEMQ(op,'(has ofCategory)) =>
        bcConform(first l,$emList)
        htSay('" has ")
        [a,b] := l
        bcConform(b, $emList)
      bcConform(x,$emList)

unMkEvalable u ==
 u is ['QUOTE,a] => a
 u is ['LIST,:r] => [unMkEvalable x for x in r]
 u

args2HtString(x) ==
  null x => '""
  emList := nil
  SUBSTRING(form2HtString(['f,:x],emList),1,nil)

quickForm2HtString(x) ==
  atom x => STRINGIMAGE x
  form2HtString x

form2HtString(x,:options) ==
  $emList:local := IFCAR options   --list of atoms to be emphasized
  $brief: local := IFCAR IFCDR options --see dbShowOperationsFromConform (lib11)
  fn(x) where
    fn x ==
      atom x =>
        MEMQ(x,$FormalMapVariableList) => STRCONC('"\",STRINGIMAGE x)
        u := escapeSpecialChars STRINGIMAGE x
        MEMQ(x,$emList) => STRCONC('"{\em ",u,'"}")
        STRINGP x => STRCONC('"_"",u,'"_"")
        u
      first x = 'QUOTE => STRCONC('"'",sexpr2HtString first rest x)
      first x = ":" => STRCONC(fn first rest x,'": ",fn first rest rest x)
      first x = 'Mapping =>
        STRCONC(fnTail(rest rest x,'"()"),'"->",fn first rest x)
      first x = 'construct => fnTail(rest x,'"[]")
      tail := fnTail(rest x,'"()")
      head := fn first x
--    $brief and #head + #tail > 35 => STRCONC(head,'"(...)")
      STRCONC(head,tail)
    fnTail(x,str) ==
      null x => '""
      STRCONC(str . 0,fn first x,fnTailTail rest x,str . 1)
    fnTailTail x ==
      null x => '""
      STRCONC('",",fn first x,fnTailTail rest x)

sexpr2HtString x ==
  atom x => form2HtString x
  STRCONC('"(",fn x,'")") where fn x ==
    r := rest x
    suffix :=
      null r => '""
      atom r => STRCONC('" . ",form2HtString rest x)
      STRCONC('" ",fn r)
    STRCONC(sexpr2HtString first x,suffix)

form2LispString(x) ==
  atom x =>
    x = '_$ => '"__$"
    MEMQ(x,$FormalMapVariableList) => STRCONC(STRINGIMAGE '__, STRINGIMAGE x)
    STRINGP x => STRCONC('"_"",STRINGIMAGE x,'"_"")
    STRINGIMAGE x
  x is ['QUOTE,a] => STRCONC('"'",sexpr2LispString a)
  x is [":",a,b] => STRCONC(form2LispString a,'":",form2LispString b)
  first x = 'Mapping =>
    null rest (r := rest x) => STRCONC('"()->",form2LispString first r)
    STRCONC(args2LispString rest r,'"->",form2LispString first r)
  STRCONC(form2LispString first x,args2LispString rest x)

sexpr2LispString x ==
  atom x => form2LispString x
  STRCONC('"(",fn x,'")") where fn x ==
    r := rest x
    suffix :=
      null r => '""
      atom r => STRCONC('" . ",form2LispString rest x)
      STRCONC('" ",fn r)
    STRCONC(sexpr2HtString first x,suffix)

args2LispString x ==
  null x => '""
  STRCONC('"(",form2LispString first x,fnTailTail rest x,'")") where
    fnTailTail x ==
      null x => '""
      STRCONC('",",form2LispString first x,fnTailTail rest x)

dbConstructorKind x ==
  target := CADAR GETDATABASE(x,'CONSTRUCTORMODEMAP)
  target = '(Category) => 'category
  target is ['CATEGORY,'package,:.] => 'package
  HGET($defaultPackageNamesHT,x) => 'default_ package
  'domain

getConstructorForm name ==
  name = 'Union   => '(Union  (_: a A) (_: b B))
  name = 'UntaggedUnion => '(Union A B)
  name = 'Record  => '(Record (_: a A) (_: b B))
  name = 'Mapping => '(Mapping T S)
  name = 'Enumeration => '(Enumeration a b)
  GETDATABASE(name,'CONSTRUCTORFORM)

getConstructorArgs conname == rest getConstructorForm conname

bcConform1 form == main where
    main ==
        form is ['ifp,form1,:pred] =>
            hd form1
            bcPred pred
        hd form
    hd form ==
        atom form =>
            not MEMQ(form,$Primitives) and null constructor? form =>
                s := STRINGIMAGE form
                (s.0 = char '_#) =>
                     (n := POSN1(form, $FormalFunctionParameterList)) =>
                          htSay form2HtString ($FormalMapVariableList . n)
                     htSay '"\"
                     htSay form
                htSay escapeSpecialChars STRINGIMAGE form
            s := STRINGIMAGE form
            $italicHead? => htSayItalics s
            $bcMultipleNames =>
                satTypeDownLink(s, ['"(|conPageChoose| '|", s, '"|)"])
            satTypeDownLink(s, ["(|conPage| '|", s, '"|)"])
        (head := QCAR form) = 'QUOTE =>
            htSay('"'")
            hd CADR form
        head = 'SIGNATURE =>
            htSayList([first(rest(form)), '": "])
            mapping CADDR form
        head = 'Mapping and rest form => mapping rest form
        head = ":" =>
            hd CADR form
            htSay '": "
            hd CADDR form
        QCDR form and dbEvalableConstructor? form =>
            bcConstructor(form)
        hd head
        null (r := QCDR form) => nil
        tl QCDR form
    mapping [target,:source] ==
        tuple source
        bcHt
            $saturn => '" {\ttrarrow} "
            '" -> "
        hd target
    tuple u ==
        null u => bcHt '"()"
        null rest u => hd u
        bcHt '"("
        hd first u
        for x in rest u repeat
            bcHt '","
            hd x
        bcHt '")"
    tl u ==
        bcHt '"("
        firstTime := true
        for x in u repeat
            if not firstTime then bcHt '","
            firstTime := false
            hd x
        bcHt '")"
    say x ==
        if $italics? then bcHt '"{\em "
        if x = 'etc then x := '"..."
        bcHt escapeSpecialIds STRINGIMAGE x
        if $italics? then bcHt '"}"

bcConform(form,:options) ==
  $italics?    : local := IFCAR options
  $italicHead? : local := IFCAR IFCDR options
  bcConform1 form

bcConstructor(form) == htSayList dbConformGen form

conform2HtString form ==
  for u in form2String form repeat
    htSay u

$from_show_implementations := false

dbEvalableConstructor? form ==
    form is [op,:argl] =>
        null(cosig := GETDATABASE(op, 'COSIG)) => false
        cosig := rest cosig
        #cosig ~= #argl => false
        res := true
        for x in argl for pred in cosig while res repeat
            pred => res :=  dbEvalableConstructor? x
            $from_show_implementations => "iterate"
            x is ['QUOTE, y] and STRINGP(y) => "iterate"
            res := false
        res
    false

htSayItalics s == htSayList(['"{\em ", s, '"}"])

bcCon(name,:options) ==
  argString := '""
  s := STRINGIMAGE name
  bcStar name
  htSayConstructorName(s,s)
  htSay argString

bcAbb(name,abb) ==
  s := STRINGIMAGE name
  a := STRINGIMAGE abb
  bcStar name
  htSayConstructorName(a,s)

bcStar name ==
  if $includeUnexposed? and not isExposedConstructor name then htSayUnexposed()

bcStarSpace name ==
  null $includeUnexposed? => nil
  not isExposedConstructor name => htSayUnexposed()
  htBlank()

bcStarSpaceOp(op,exposed?) ==
  null $includeUnexposed? => nil
  not exposed? =>
    htSayUnexposed()
    if op.0 = char '_* then htSay '" "
  htBlank()

bcStarConform form ==
  bcStar opOf form
  bcConform form

dbSourceFile name ==
  u:= GETDATABASE(name,'SOURCEFILE)
  null u => '""
  n := PATHNAME_-NAME u
  t := PATHNAME_-TYPE u
  STRCONC(n,'".",t)

asharpConstructorName? name ==
  u:= GETDATABASE(name,'SOURCEFILE)
  u and PATHNAME_-TYPE u = '"as"

asharpConstructors() ==
  [x for x in allConstructors() | not asharpConstructorName? x]

extractFileNameFromPath s == fn(s,0,#s) where
  fn(s,i,m) ==
    k := charPosition(char '_/,s,i)
    k = m => SUBSTRING(s,i,nil)
    fn(s,k + 1,m)

bcOpTable(u,fn) ==
  htBeginTable()
  firstTime := true
  for op in u for i in 0.. repeat
    if firstTime then firstTime := false
    else htSaySaturn '"&"
    htSay '"{"
    htMakePage [['bcLinks,[escapeSpecialChars STRINGIMAGE opOf op,'"",fn,i]]]
    htSay '"}"
  htEndTable()

bcNameConTable u ==
  $bcMultipleNames: local := (#u ~= 1)
  bcConTable REMDUP u
  -- bcConTable u

bcConTable u ==
  htBeginTable()
  firstTime := true
  for con in u repeat
    if firstTime then firstTime := false
    else htSaySaturn '"&"
    htSay '"{"
    bcStarSpace opOf con
    bcConform con
    htSay '"}"
  htEndTable()

bcAbbTable u ==
  htBeginTable()
  firstTime := true
  for x in REMDUP u repeat        --allow x to be NIL meaning "no abbreviation"
  -- for x in u repeat    --allow x to be NIL meaning "no abbreviation"
    if firstTime then firstTime := false
    else htSaySaturn '"&"
    if x is [con,abb,:.] then
      htSay '"{"
      bcAbb(con,abb)
      htSay '"}"
  htEndTable()

bcConPredTable(u,conname,:options) ==
  italicList := IFCAR options
  htBeginTable()
  firstTime := true
  for [conform,:pred] in u repeat
    if firstTime then firstTime := false
    else htSaySaturn '"&"
    htSay '"{"
    bcStarSpace opOf conform
    form :=
      atom conform => getConstructorForm conform
      conform
    bcConform(form,italicList)
    if extractHasArgs pred is [arglist,:pred] then
      htSay('" {\em of} ")
      bcConform([conname,:arglist],italicList,true)
    if pred ~= 'etc then bcPred(pred,italicList)
    htSay '"}"
  htEndTable()

bcPred(pred,:options) ==
  pred = '"" or pred = true or null pred => 'skip
  italicList := IFCAR options
  if not IFCAR IFCDR options then htSay '" {\em if} "
  htPred2English(pred,italicList)

extractHasArgs pred ==
  x := find pred or return nil where find x ==
    x is [op,:argl] =>
      op = 'hasArgs => x
      MEMQ(op,'(AND OR NOT)) => or/[find y for y in argl]
      nil
    nil
  [rest x, :simpBool substitute('T, x, pred)]

splitConTable cons ==
  uncond := cond := nil
  for (pair := [con,:pred]) in cons repeat
    null pred => 'skip
    pred = 'T or pred is ['hasArgs,:.]  => uncond := [pair,:uncond]
    cond := [pair,:cond]
  [NREVERSE uncond,:NREVERSE cond]

bcNameTable(u,fn,:option) ==   --option if * prefix
  htSay '"\newline"
  htBeginTable()
  firstTime := true
  for x in u repeat
    if firstTime then firstTime := false
    else htSaySaturn '"&"
    htSay '"{"
    if IFCAR option then bcStar x
    htMakePage [['bcLinks,[s := escapeSpecialChars STRINGIMAGE x,'"",fn,s]]]
    htSay '"}"
  htEndTable()

bcNameCountTable(u, fn, gn) ==
  linkFunction := 'bcLispLinks
  htSay '"\newline"
  htBeginTable()
  firstTime := true
  for i in 0.. for x in u repeat
    if firstTime then firstTime := false
    else htSaySaturn '"&"
    htSay '"{"
    htMakePage [[linkFunction,[FUNCALL(fn,x),'"",gn,i]]]
    htSay '"}"
  htEndTable()

dbSayItemsItalics(:u) ==
  htSay '"{\em "
  APPLY(function dbSayItems,u)
  htSay '"}"

dbSayItems(countOrPrefix,singular,plural,:options) ==
  bcHt '"\newline "
  count :=
   countOrPrefix is [:prefix,c] =>
     htSay prefix
     c
   countOrPrefix
  if count = 0 then htSayList(['"No ", singular])
  else if count = 1 then htSayList(['"1 ", singular])
  else htSayList([count, '" ", plural])
  for x in options repeat bcHt x
  if count ~= 0 then bcHt '":"

dbBasicConstructor? conname == member(dbSourceFile conname,'("catdef" "coerce"))

htCopyProplist htPage == [[x,:y] for [x,:y] in htpPropertyList htPage]

dbInfovec name ==
  'category = GETDATABASE(name,'CONSTRUCTORKIND) => nil
  GETDATABASE(name, 'ASHARP?) => nil
  loadLibIfNotLoaded(name)
  u := GET(name, 'infovec) => u

emptySearchPage(kind, filter, skipNamePart) ==
  heading := ['"No ",capitalize kind,'" Found"]
  htInitPage(heading,nil)
  exposePart :=
    null $includeUnexposed? => '"{\em exposed} "
    '""
  htSayList(['"\vspace{1}\newline\centerline{There is no ", exposePart,
             kind, '" matching pattern}\newline\centerline{{\em "])
  if filter then htPred2English filter
  htSay '"}}"
  htShowPage()

string2Integer s ==
  and/[DIGIT_-CHAR_-P (s.i) for i in 0..MAXINDEX s] => PARSE_-INTEGER s
  nil

dbGetInputString htPage ==
  s := htpLabelInputString(htPage,'filter)
  null s or s = '"" => '"*"
  s



--=======================================================================
--                   Error Pages
--=======================================================================
bcErrorPage u ==
  u is ['error,:r] =>
    htInitPage(first r,nil)
    bcBlankLine()
    for x in rest r repeat htSay x
    htShowPage()
  systemError '"Unexpected error message"

errorPage(htPage,[heading,kind,:info]) ==
  kind = 'invalidType => kInvalidTypePage first info
  if heading = 'error then htInitPage('"Error",nil) else
                           htInitPage(heading,nil)
  bcBlankLine()
  for x in info repeat htSay x
  htShowPage()

htErrorStar() ==
  errorPage(nil,['"{\em *} not a valid search string",nil,'"\vspace{3}\centerline{{\em *} is not a valid search string for a general search}\centerline{\em {it would match everything!}}"])

htQueryPage(htPage,heading,message,query,fn) ==
  htInitPage(heading,nil)
  htSay message
  htQuery(query, fn, false)
  htShowPage()

htQuery(question, fn, upLink?) ==
  if question then
    htSay('"\vspace{1}\centerline{")
    htSay question
    htSay('"}")
  htSay('"\centerline{")
  htMakePage [['bcLispLinks,['"\fbox{Yes}",'"",fn,'yes]]]
  htBlanks(4)
  if upLink?
    then htSay('"\downlink{\fbox{No}}{UpPage}")
    else htMakePage [['bcLispLinks,['"\fbox{No}",'"",fn,'no]]]
  htSay('"}")

kInvalidTypePage form ==
  htInitPage('"Error",nil)
  bcBlankLine()
  htSay('"\centerline{You gave an invalid type:}\newline\centerline{{\sf ")
  htSayList([form2HtString form, '"}}"])
  htShowPage()

--=======================================================================
--       Utility Functions for Manipulating Browse Datalines
--=======================================================================
dbpHasDefaultCategory? s ==  #s > 1 and s.1 = char 'x  --s is part 3 of line

dbKind line == line.0

dbKindString kind == LASSOC(kind,$dbKindAlist)

dbName line == escapeString SUBSTRING(line,1,charPosition($tick,line,1) - 1)

dbAttr line == STRCONC(dbName line,escapeString dbPart(line,4,0))

dbPart(line,n,k) ==  --returns part n of line (n=1,..) beginning in column k
  n = 1 => SUBSTRING(line,k + 1,charPosition($tick,line,k + 1) - k - 1)
  dbPart(line,n - 1,charPosition($tick,line,k + 1))

dbXParts(line,n,m) ==
  [.,nargs,:r] := dbParts(line,n,m)
  [dbKindString line.0,dbName line,PARSE_-INTEGER nargs,:r]

dbParts(line,n,m) ==  --split line into n parts beginning in column m
  n = 0 => nil
  [SUBSTRING(line,m,-m + (k := charPosition($tick,line,m))),
    :dbParts(line,n - 1,k + 1)]

dbConname(line) == dbPart(line,5,1)

dbComments line ==  dbReadComments(string2Integer dbPart(line,7,1))

dbNewConname(line) == --dbName line unless kind is 'a or 'o => name in 5th pos.
  (kind := line.0) = char 'a or kind = char 'o =>
    conform := dbPart(line,5,1)
    k := charPosition(char '_(,conform,1)
    SUBSTRING(conform,1,k - 1)
  dbName line

dbTickIndex(line,n,k) == --returns index of nth tick in line starting at k
  n = 1 => charPosition($tick,line,k)
  dbTickIndex(line,n - 1,1 + charPosition($tick,line,k))

mySort u == listSort(function GLESSEQP,u)

-- from bc-util

bcFinish(name,arg,:args) == bcGen bcMkFunction(name,arg,args)

bcMkFunction(name,arg,args) ==
  args := [x for x in args | x]
  STRCONC(name,'"(",arg,"STRCONC"/[STRCONC('",", x) for x in args],'")")

bcFindString(s,i,n,char) ==  or/[j for j in i..n | s.j = char]

bcGen command ==
  htInitPage('"Basic Command",nil)
  string :=
    #command < 50 => STRCONC('"{\centerline{\tt ",command,'" }}")
    STRCONC('"{\tt ",command,'" }")
  htMakePage [
     '(text
        "{Here is the FriCAS command you could have issued to compute this result:}"
            "\vspace{2}\newline "),
      ['text,:string]]
  htMakeDoitButton('"Do It", command)
  htShowPage()

bcOptional s ==
  s = '"" => '"2"
  s

bcvspace() == bcHt '"\vspace{1}\newline "

bcString2WordList s == fn(s,0,MAXINDEX s) where
  fn(s,i,n) ==
    i > n => nil
    k := or/[j for j in i..n | s.j ~= char '_  ]
    null INTEGERP k => nil
    l := bcFindString(s,k + 1,n,char '_  )
    null INTEGERP l => [SUBSTRING(s,k,nil)]
    [SUBSTRING(s,k,l-k),:fn(s,l + 1,n)]


bcwords2liststring u ==
  null u => nil
  STRCONC('"[",first u,fn rest u) where
    fn(u) ==
      null u => '"]"
      STRCONC('", ",first u,fn rest u)

bcVectorGen vec == bcwords2liststring vec

bcError string ==
  sayBrightlyNT '"NOTE: "
  sayBrightly string

bcDrawIt(ind,a,b) == STRCONC(ind,'"=",a,'"..",b)

bcNotReady htPage ==
  htInitPage('"Basic Command",nil)
  htMakePage '(
     (text .
        "{\centerline{\em This facility will soon be available}}"))
  htShowPage()

htStringPad(n,w) ==
  s := STRINGIMAGE n
  ws := #s
  STRCONC('"\space{",STRINGIMAGE (w - ws + 1),'"}",s)

htMkName(s,n) == STRCONC(s,STRINGIMAGE n)
