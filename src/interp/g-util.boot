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

--% Various lispy things

Identity x == x

length1? l == PAIRP l and not PAIRP QCDR l

length2? l == PAIRP l and PAIRP (l := QCDR l) and not PAIRP QCDR l

pairList(u,v) == [[x,:y] for x in u for y in v]

concatenateStringList(l) ==
    ll := 0
    for s in l repeat ll := ll + LENGTH(s)
    result := MAKE_-STRING(ll)
    ll := 0
    for s in l repeat
        replaceString(result, s, ll)
        ll := ll + LENGTH(s)
    result

GETL(op, prop) == op and SYMBOLP(op) and GET(op, prop)

GETALIST(alist,prop) == CDR assoc(prop,alist)

PUTALIST(alist,prop,val) ==
  null alist => [[prop,:val]]
  pair := assoc(prop,alist) =>
    CDR pair = val => alist
    -- else we fall over Lucid's read-only storage feature again
    QRPLACD(pair,val)
    alist
  QRPLACD(LASTNODE alist,[[prop,:val]])
  alist

REMALIST(alist,prop) ==
  null alist => alist
  alist is [[ =prop,:.],:r] =>
    null r => NIL
    QRPLACA(alist,CAR r)
    QRPLACD(alist,CDR r)
    alist
  null rest alist => alist
  l := alist
  ok := true
  while ok repeat
    [.,[p,:.],:r] := l
    p = prop =>
      ok := NIL
      QRPLACD(l,r)
    if null (l := QCDR l) or null rest l then ok := NIL
  alist

--% association list functions

deleteAssoc(x,y) ==
  y is [[a,:.],:y'] =>
   a=x => deleteAssoc(x,y')
   [first y,:deleteAssoc(x,y')]
  y

insertWOC(x,y) ==
  null y => [x]
  (fn(x,y); y) where fn(x,y is [h,:t]) ==
    x=h => nil
    null t =>
      RPLACD(y,[h,:t])
      RPLACA(y,x)
    fn(x,t)

--% Miscellaneous Functions for Working with Strings

fillerSpaces(n, charPart) ==
  n <= 0 => '""
  make_full_CVEC(n, charPart)

centerString(text,width,fillchar) ==
  wid := entryWidth text
  wid >= width => text
  f := DIVIDE(width - wid,2)
  fill1 := ""
  for i in 1..(f.0) repeat
    fill1 := STRCONC(fillchar,fill1)
  fill2:= fill1
  if f.1 ~= 0 then fill1 := STRCONC(fillchar,fill1)
  [fill1,text,fill2]

stringPrefix?(pref,str) ==
  -- sees if the first #pref letters of str are pref
  -- replaces STRINGPREFIXP
  null (STRINGP(pref) and STRINGP(str)) => NIL
  (lp := QCSIZE pref) = 0 => true
  lp > QCSIZE str => NIL
  ok := true
  i := 0
  while ok and (i < lp) repeat
    not EQL(SCHAR(pref,i),SCHAR(str,i)) => ok := NIL
    i := i + 1
  ok

dropLeadingBlanks str ==
  str := object2String str
  l := QCSIZE str
  nb := NIL
  i := 0
  while (i < l) and not nb repeat
    if SCHAR(str,i) ~= SCHAR('" ",0) then nb := i
    else i := i + 1
  nb = 0 => str
  nb => SUBSTRING(str,nb,NIL)
  '""

concat(:l) == concatList l

concatList [x,:y] ==
  null y => x
  null x => concatList y
  concat1(x,concatList y)

concat1(x,y) ==
  null x => y
  atom x => (null y => x; atom y => [x,y]; [x,:y])
  null y => x
  atom y => [:x,y]
  [:x,:y]

--% Miscellaneous

freeOfSharpVars x ==
  atom x => not isSharpVarWithNum x
  freeOfSharpVars first x and freeOfSharpVars rest x

listOfSharpVars x ==
  atom x => (isSharpVarWithNum x => LIST x; nil)
  union(listOfSharpVars first x,listOfSharpVars rest x)

listOfPatternIds x ==
  isPatternVar x => [x]
  atom x => nil
  x is ['QUOTE,:.] => nil
  UNIONQ(listOfPatternIds first x,listOfPatternIds rest x)

isPatternVar v ==
  -- a pattern variable consists of a star followed by a star or digit(s)
  IDENTP(v) and MEMQ(v,'(_*_* _*1 _*2 _*3 _*4 _*5 _*6 _*7 _*8 _*9 _*10
    _*11 _*12 _*13 _*14 _*15 _*16 _*17 _*18 _*19 _*20)) and true

removeZeroOne x ==
  -- replace all occurrences of (Zero) and (One) with
  -- 0 and 1
  x = $Zero => 0
  x = $One => 1
  atom x => x
  [removeZeroOne first x,:removeZeroOne rest x]

removeZeroOneDestructively t ==
  -- replace all occurrences of (Zero) and (One) with
  -- 0 and 1 destructively
  t = $Zero => 0
  t = $One => 1
  atom t => t
  RPLNODE(t,removeZeroOneDestructively first t,
    removeZeroOneDestructively rest t)

--% Inplace Merge Sort for Lists
-- MBM April/88

-- listSort(pred,list) or listSort(pred,list,key)
-- the pred function is a boolean valued function defining the ordering
-- the key function extracts the key from an item for comparison by pred

listSort(pred,list,:optional) ==
   NOT functionp pred => error "listSort: first arg must be a function"
   NOT LISTP list => error "listSort: second argument must be a list"
   NULL optional => mergeSort(pred,function Identity,list,LENGTH list)
   key := CAR optional
   NOT functionp key => error "listSort: last arg must be a function"
   mergeSort(pred,key,list,LENGTH list)

-- non-destructive merge sort using NOT GGREATERP as predicate
MSORT list == listSort(function GLESSEQP, COPY_-LIST list)

-- destructive merge sort using NOT GGREATERP as predicate
NMSORT list == listSort(function GLESSEQP, list)

-- non-destructive merge sort using ?ORDER as predicate
orderList l == listSort(function _?ORDER, COPY_-LIST l)

mergeInPlace(f,g,p,q) ==
   -- merge the two sorted lists p and q
   if NULL p then return p
   if NULL q then return q
   if FUNCALL(f,FUNCALL(g, QCAR p),FUNCALL(g, QCAR q))
   then (r := t := p; p := QCDR p)
   else (r := t := q; q := QCDR q)
   while not NULL p and not NULL q repeat
      if FUNCALL(f,FUNCALL(g,QCAR p),FUNCALL(g,QCAR q))
      then (QRPLACD(t,p); t := p; p := QCDR p)
      else (QRPLACD(t,q); t := q; q := QCDR q)
   if NULL p then QRPLACD(t,q) else QRPLACD(t,p)
   r

mergeSort(f,g,p,n) ==
   if eql_SI(n, 2) and
        FUNCALL(f, FUNCALL(g, QCADR p), FUNCALL(g, QCAR p)) then
      t := p
      p := QCDR p
      QRPLACD(p,t)
      QRPLACD(t,NIL)
   if less_SI(n, 3) then return p
   -- split the list p into p and q of equal length
   l := quo_SI(n, 2)
   t := p
   for i in 1..l-1 repeat t := QCDR t
   q := rest t
   QRPLACD(t,NIL)
   p := mergeSort(f,g,p,l)
   q := mergeSort(f, g, q, sub_SI(n, l))
   mergeInPlace(f,g,p,q)

--% Throwing with glorious highlighting (maybe)

throw_to_reader() == THROW('SPAD_READER, nil)

spadThrow() ==
  if $interpOnly and $mapName then
    putHist($mapName,'localModemap, nil, $e)
  $BreakMode = 'throw_reader => throw_to_reader()
  handleLispBreakLoop($BreakMode)

spadThrowBrightly x ==
  sayBrightly x
  spadThrow()

--% Type Formatting Without Abbreviation

formatUnabbreviatedSig sig ==
  null sig => ["() -> ()"]
  [target,:args] := sig
  target := formatUnabbreviated target
  null args => ['"() -> ",:target]
  null rest args => [:formatUnabbreviated QCAR args,'" -> ",:target]
  args := formatUnabbreviatedTuple args
  ['"(",:args,'") -> ",:target]

formatUnabbreviatedTuple t ==
  -- t is a list of types
  null t => t
  atom t => [t]
  t0 := formatUnabbreviated QCAR t
  null rest t => t0
  [:t0,'",",:formatUnabbreviatedTuple QCDR t]

formatUnabbreviated t ==
  atom t =>
    [t]
  null t =>
    ['"()"]
  t is [p,sel,arg] and p in '(_: ":") =>
    [sel,'": ",:formatUnabbreviated arg]
  t is ['Union,:args] =>
    ['Union,'"(",:formatUnabbreviatedTuple args,'")"]
  t is ['Mapping,:args] =>
    formatUnabbreviatedSig args
  t is ['Record,:args] =>
    ['Record,'"(",:formatUnabbreviatedTuple args,'")"]
  t is [arg] =>
    t
  t is [arg,arg1] =>
    [arg,'" ",:formatUnabbreviated arg1]
  t is [arg,:args] =>
    [arg,'"(",:formatUnabbreviatedTuple args,'")"]
  t

sublisNQ(al,e) ==
  atom al => e
  fn(al,e) where fn(al,e) ==
    atom e =>
      for x in al repeat
        EQ(first x,e) => return (e := rest x)
      e
    EQ(a := first e,'QUOTE) => e
    u := fn(al,a)
    v := fn(al,rest e)
    EQ(a,u) and EQ(rest e,v) => e
    [u,:v]

opOf x ==
  atom x => x
  first x

getProplist(x,E) ==
  not atom x => getProplist(first x,E)
  u:= search(x,E) => u
  --$InteractiveMode => nil
  --$InteractiveMode and (u:= search(x,$InteractiveFrame)) => u
  (pl:=search(x,$CategoryFrame)) =>
    pl

search(x,e is [curEnv,:tailEnv]) ==
  tailEnv =>
    BREAK()
  searchCurrentEnv(x,curEnv)

searchCurrentEnv(x,currentEnv) ==
  for contour in currentEnv repeat
    if u:= ASSQ(x,contour) then return (signal:= u)
  IFCDR signal

augProplist(proplist,prop,val) ==
  $InteractiveMode => augProplistInteractive(proplist,prop,val)
  while (proplist is [[ =prop,:.],:proplist']) repeat proplist:= proplist'
  val=(u:= LASSOC(prop,proplist)) => proplist
  null val =>
    null u => proplist
    DELLASOS(prop,proplist)
  [[prop,:val],:proplist]

augProplistOf(var,prop,val,e) ==
  proplist:= getProplist(var,e)
  semchkProplist(var,proplist,prop,val)
  augProplist(proplist,prop,val)

semchkProplist(x,proplist,prop,val) ==
  prop="isLiteral" =>
    LASSOC("value",proplist) or LASSOC("mode",proplist) => warnLiteral x
  MEMQ(prop,'(mode value)) =>
    LASSOC("isLiteral",proplist) => warnLiteral x

DEFPARAMETER($envHashTable, nil)

addBinding(var,proplist,e is [[curContour,:tailContour],:tailEnv]) ==
  EQ(proplist,getProplist(var,e)) => e
  if $envHashTable then
    for u in proplist repeat
      HPUT($envHashTable, [var, CAR u], true)
  $InteractiveMode => addBindingInteractive(var,proplist,e)
  if curContour is [[ =var,:.],:.] then curContour:= rest curContour
                 --Previous line should save some space
  [[[lx,:curContour],:tailContour],:tailEnv] where lx:= [var,:proplist]

position(x,l) ==
  posn(x,l,0) where
    posn(x,l,n) ==
      null l => -1
      x=first l => n
      posn(x,rest l,n+1)

insert(x,y) ==
  member(x,y) => y
  [x,:y]

after(u,v) ==
  r:= u
  for x in u for y in v repeat r:= rest r
  r


$blank := char ('_ )

trimString s ==
  leftTrim rightTrim s

leftTrim s ==
  k := MAXINDEX s
  k < 0 => s
  s.0 = $blank =>
    for i in 0..k while s.i = $blank repeat (j := i)
    SUBSTRING(s,j + 1,nil)
  s

rightTrim s ==  -- assumed a non-empty string
  k := MAXINDEX s
  k < 0 => s
  s.k = $blank =>
    for i in k..0 by -1 while s.i = $blank repeat (j := i)
    SUBSTRING(s,0,j)
  s

pp x ==
  PRETTYPRINT x
  nil

quickAnd(a,b) ==
  a = true => b
  b = true => a
  a = false or b = false => false
  simpBool ['AND,a,b]

quickOr(a,b) ==
  a = true or b = true => true
  b = false => a
  a = false => b
  simpCatPredicate simpBool ['OR,a,b]

intern x ==
  STRINGP x =>
    DIGITP x.0 => string2Integer x
    INTERN x
  x

-- variables used by browser

$htHash      := MAKE_-HASH_-TABLE()
$glossHash   := MAKE_-HASH_-TABLE()
$lispHash    := MAKE_-HASH_-TABLE()
$sysHash     := MAKE_-HASH_-TABLE()
$htSystemCommands := '(
 (boot . development) clear display (fin . development) edit help
 frame history load quit read set show synonym system
 trace what )
$currentSysList := [opOf x for x in $htSystemCommands] --see ht-root
$outStream   := nil
$recheckingFlag    := false     --see transformAndRecheckComments
$exposeFlag        := false     --if true, messages go to $outStream
$exposeFlagHeading := false     --see htcheck.boot
$checkingXmptex? := false       --see htcheck.boot
$exposeDocHeading:= nil         --see htcheck.boot
$charPlus := char '_+
$charBlank:= (char '_ )
$charLbrace:= char '_{
$charRbrace:= char '_}
$charBack := char '_\
$charDash := char '_-

$charTab            := CODE_-CHAR(9)
$charNewline        := CODE_-CHAR(10)
$charFauxNewline    := CODE_-CHAR(25)
$stringNewline      := PNAME CODE_-CHAR(10)
$stringFauxNewline  := PNAME CODE_-CHAR(25)

$charExclusions := [char 'a, char 'A]
$charQuote := char '_'
$charSemiColon := char '_;
$charComma     := char '_,
$charPeriod    := char '_.
$checkPrenAlist := [[char '_(,:char '_)],[char '_{,:char '_}],[char '_[,:char '_]]]
$charEscapeList:= [char '_%,char '_#,$charBack]
$charIdentifierEndings := [char '__, char '_!, char '_?]
$charSplitList := [$charComma,$charPeriod,char '_[, char '_],$charLbrace, $charRbrace, char '_(, char '_), char '_$, char '_%]
$charDelimiters := [$charBlank, char '_(, char '_), $charBack]
$HTspadmacros := '("\spadtype" "\spadcommand" "\spadop" "\spadfun" "\spadatt" "\spadsyscom" "\spad" "\s")
$HTmacs := [
  ['"\beginmenu",$charRbrace,'"menu",$charLbrace,'"\begin"],
   ['"\endmenu",$charRbrace,'"menu",$charLbrace,'"\end"],
     ['"\beginitems",$charRbrace,'"items",$charLbrace,'"\begin"],
       ['"\enditems",$charRbrace,'"items",$charLbrace,'"\end"],
         ['"\beginscroll",$charRbrace,'"scroll",$charLbrace,'"\begin"],
           ['"\endscroll",$charRbrace,'"scroll",$charLbrace,'"\end"]]

$HTlinks := '(
  "\downlink"
  "\menulink"
  "\menudownlink"
  "\menuwindowlink"
  "\menumemolink")

$HTlisplinks := '(
  "\lispdownlink"
  "\menulispdownlink"
  "\menulispwindowlink"
  "\menulispmemolink"
  "\lispwindowlink"
  "\lispmemolink")

$beginEndList := '(
  "page"
  "items"
  "menu"
  "scroll"
  "verbatim"
  "detail")

isDefaultPackageName x == (s := PNAME x).(MAXINDEX s) = char '_&

-- from packtran

packageTran sex == sex

zeroOneTran sex == sex

-- from i-util

--% Utility Functions Used Only by the Intepreter

-- A wrapped value represents something that need not be evaluated
-- when code is generated.  This includes objects from domains or things
-- that just happed to evaluate to themselves.  Typically generated
-- lisp code is unwrapped.

wrap x ==
  isWrapped x => x
  ['WRAPPED,:x]

isWrapped x == x is ['WRAPPED,:.] or NUMBERP x or FLOATP x or STRINGP x

unwrap x ==
  NUMBERP x or FLOATP x or STRINGP x => x
  x is ["WRAPPED",:y] => y
  x

wrapped2Quote x ==
  x is ["WRAPPED",:y] => MKQ y
  x

quote2Wrapped x ==
  x is ['QUOTE,y] => wrap y
  x

removeQuote x ==
  x is ["QUOTE",y] => y
  x

--% The function for making prompts

spadPrompt() ==
  SAY '"   FriCAS"
  sayNewLine()

princPrompt() ==
  ioHook("startPrompt")
  PRINC MKPROMPT()
  ioHook("endOfPrompt")

MKPROMPT() ==
  $inputPromptType = 'none    => '""
  $inputPromptType = 'plain   => '"-> "
  $inputPromptType = 'step    =>
    STRCONC('"(",STRINGIMAGE $IOindex,'") -> ")
  $inputPromptType = 'frame   =>
    STRCONC(STRINGIMAGE $interpreterFrameName,
      '" (",STRINGIMAGE $IOindex,'") -> ")
  STRCONC(STRINGIMAGE $interpreterFrameName,
   '" [", SUBSTRING(CURRENTTIME(),8,NIL),'"] [",
    STRINGIMAGE $IOindex, '"] -> ")

--% Miscellaneous

-- formerly in clammed.boot

isSubDomain(d1,d2) ==
  -- d1 and d2 are different domains
  subDomainList := '(Integer NonNegativeInteger PositiveInteger)
  ATOM d1 or ATOM d2 => nil
  l := MEMQ(first d2, subDomainList) =>
    MEMQ(first d1, rest l)
  nil

-- functions used at run-time which were formerly in the compiler files

Undef(:u) ==
  u':= last u
  [[domain,slot],op,sig]:= u'
  domain':=eval mkEvalable domain
  not EQ(first ELT(domain', slot), Undef) =>
  -- OK - the function is now defined
    [:u'',.]:=u
    if $reportBottomUpFlag then
      sayMessage concat ['"   Retrospective determination of slot",'%b,
        slot,'%d,'"of",'%b,:prefix2String domain,'%d]
    APPLY(first ELT(domain', slot), [:u'', rest ELT(domain', slot)])
  throwKeyedMsg("S2IF0008",[formatOpSignature(op,sig),domain])

TruthP x ==
    --True if x is a predicate that's always true
  x is nil => nil
  x=true => true
  x is ['QUOTE,:.] => true
  nil
