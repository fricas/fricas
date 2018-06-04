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

getSignatureDocumentation(lineno) ==
    $docList : local := nil
    recordDocumentation("Signature", lineno)
    rest first $docList

record_on_docList(key, item) ==
    $docList := [[key, :item], :$docList]

recordAttributeDocumentation(['ATTRIBUTE, att], lineno) ==
  name := opOf att
  UPPER_-CASE_-P (PNAME name).0 => nil
  recordDocumentation([name,['attribute,:IFCDR postTransform att]],lineno)

recordDocumentation(key,lineno) ==
  recordHeaderDocumentation lineno
  u:= collectComBlock lineno
  --record NIL to mean "there was no documentation"
  $maxSignatureLineNumber := lineno
  $docList := [[key,:u],:$docList]
  -- leave first of $docList alone as required by collectAndDeleteAssoc

recordHeaderDocumentation lineno ==
  if $maxSignatureLineNumber = 0 then
    al := [p for (p := [n,:u]) in $COMBLOCKLIST
               | NULL n or NULL lineno or n < lineno]
    $COMBLOCKLIST := SETDIFFERENCE($COMBLOCKLIST,al)
    $headerDocumentation := ASSOCRIGHT al
    if $headerDocumentation then $maxSignatureLineNumber := 1 --see postDef
    $headerDocumentation

collectComBlock x ==
  $COMBLOCKLIST is [[=x,:val],:.] =>
    u := [:val,:collectAndDeleteAssoc x]
    $COMBLOCKLIST := rest $COMBLOCKLIST
    u
  collectAndDeleteAssoc x

collectAndDeleteAssoc x ==
--u is (.. (x . a) .. (x . b) .. )  ==> (a b ..) deleting entries from u
--assumes that the first element is useless
  for y in tails $COMBLOCKLIST | (s := rest y) repeat
    while s and first s is [=x,:r] repeat
      res := [:res,:r]
      s := rest s
      RPLACD(y,s)
  res

finalizeDocumentation() ==
  -- skip during bootstrap to save time
  $bootStrapMode => []
  unusedCommentLineNumbers := [x for (x := [n,:r]) in $COMBLOCKLIST | r]
  docList := SUBST("$","%",transDocList($op,$docList))
  if u := [sig for [sig,:doc] in docList | null doc] then
    for y in u repeat
      y = 'constructor => noHeading := true
      y is [x,b] and b is [='attribute,:r] =>
        attributes := [[x,:r],:attributes]
      signatures := [y,:signatures]
    name := first $lisplibForm
    if noHeading or signatures or unusedCommentLineNumbers then
      say_msg('"%b Constructor documentation warnings (++ comments): %d", nil)
      bigcnt := 1
      if noHeading or signatures then
        say_msg('"%1 The constructor %2b has missing documentation.",
                [STRCONC(STRINGIMAGE bigcnt,'"."),name])
        bigcnt := bigcnt + 1
        litcnt := 1
        if noHeading then
          say_msg('"%x3 %1 The constructor %2b is missing the heading description.",
            [STRCONC('"(",STRINGIMAGE litcnt,'")"),name])
          litcnt := litcnt + 1
        if signatures then
          say_msg('"%x3 %1 The following functions do not have documentation:",
                  [STRCONC('"(",STRINGIMAGE litcnt,'")")])
          litcnt := litcnt + 1
          for [op,sig] in signatures repeat
            s := formatOpSignature(op,sig)
            sayMSG
              atom s => ['%x9,s]
              ['%x9,:s]
      if unusedCommentLineNumbers then
        say_msg('"%1 The constructor %2b has incorrectly placed documentation.",[STRCONC(STRINGIMAGE bigcnt,'"."),name])
        for [n,r] in unusedCommentLineNumbers repeat
          sayMSG ['"   ",:bright n,'"   ",r]
  hn [[:fn(sig,$e),:doc] for [sig,:doc] in docList] where
    fn(x,e) ==
      atom x => [x,nil]
      if #x > 2 then x := TAKE(2,x)
      SUBLISLIS($FormalMapVariableList,rest $lisplibForm,
        macroExpand(x,e))
    hn u ==
     -- ((op,sig,doc), ...)  --> ((op ((sig doc) ...)) ...)
      opList := REMDUP ASSOCLEFT u
      [[op,:[[sig,doc] for [op1,sig,doc] in u | op = op1]] for op in opList]

--=======================================================================
--             Transformation of ++ comments
--=======================================================================
transDocList($constructorName,doclist) == --returns ((key line)...)
--called ONLY by finalizeDocumentation
--if $exposeFlag then messages go to file $outStream; flag=nil by default
  sayBrightly ['"   Processing ",$constructorName,'" for Browser database:"]
  commentList := transDoc($constructorName,doclist)
  acc := nil
  for entry in commentList repeat
    entry is ['constructor,x] =>
      conEntry => checkDocError ['"Spurious comments: ",x]
      conEntry := entry
    acc := [entry,:acc]
  conEntry => [conEntry,:acc]
  checkDocError1 ['"Missing Description"]
  acc

transDoc(conname,doclist) ==
--$exposeFlag and not isExposedConstructor conname => nil
--skip over unexposed constructors when checking system files
  $x: local := nil
  rlist := REVERSE doclist
  for [$x,:lines] in rlist repeat
    $attribute? : local := $x is [.,[key]] and key = 'attribute
    null lines =>
      $attribute? => nil
      checkDocError1 ['"Not documented!!!!"]
    u := checkTrim($x,(STRINGP lines => [lines]; $x = 'constructor => first lines; lines))
    $argl : local := nil    --set by checkGetArgs
-- tpd: related domain information doesn't exist
--    if v := checkExtract('"Related Domains:",u) then
--      $lisplibRelatedDomains:=[w for x in gn(v) | w := fn(x)] where
--        gn(v) ==  --note: unabbrev checks for correct number of arguments
--          s := checkExtractItemList v
--          parse := ncParseFromString s  --is a single conform or a tuple
--          null parse => nil
--          parse is ['Tuple,:r] => r
--          [parse]
--        fn(x) ==
--          expectedNumOfArgs := checkNumOfArgs x
--          null expectedNumOfArgs =>
--            checkDocError ['"Unknown constructor name?: ",opOf x]
--            x
--          expectedNumOfArgs ~= (n := #(IFCDR x)) =>
--            n = 0 => checkDocError1
--              ['"You must give arguments to the _"Related Domain_": ",x]
--            checkDocError
--              ['"_"Related Domain_" has wrong number of arguments: ",x]
--            nil
--          n=0 and atom x => [x]
--          x
    longline :=
      $x = 'constructor =>
        v :=checkExtract('"Description:",u) or u and
              checkExtract('"Description:",
                [STRCONC('"Description: ",first u),:rest u])
        transformAndRecheckComments('constructor,v or u)
      transformAndRecheckComments($x,u)
    acc := [[$x,longline],:acc]  --processor assumes a list of lines
  NREVERSE acc

checkExtractItemList l ==  --items are separated by commas or end of line
  acc := nil               --l is list of remaining lines
  while l repeat           --stop when you get to a line with a colon
    m := MAXINDEX first l
    k := charPosition(char '_:,first l,0)
    k <= m => return nil
    acc := [first l,:acc]
    l := rest l
  "STRCONC"/[x for x in NREVERSE acc]

transformAndRecheckComments(name,lines) ==
  $checkingXmptex? := false
  $x            : local := name
  $name         : local := 'GlossaryPage
  $origin       : local := 'gloss
  $recheckingFlag : local := false
  $exposeFlagHeading : local := ['"--------",name,'"---------"]
  if null $exposeFlag then sayBrightly $exposeFlagHeading
  u := checkComments(name,lines)
  $recheckingFlag := true
  checkRewrite(name,[u])
  $recheckingFlag := false
  u

checkRewrite(name, lines) ==    --similar to checkComments from c-doc
    $checkErrorFlag: local := true
    margin := 0
    lines := checkRemoveComments lines
    u := lines
    if $checkingXmptex? then
      u := [checkAddIndented(x,margin) for x in u]
    $argl := checkGetArgs first u      --set $argl
    u2 := nil
    verbatim := nil
    for x in u repeat
        w := newString2Words x
        verbatim =>
          w and first w = '"\end{verbatim}" =>
            verbatim := false
            u2 := append(u2, w)
          u2 := append(u2, [x])
        w and first w = '"\begin{verbatim}" =>
            verbatim := true
            u2 := append(u2, w)
        u2 := append(u2, w)
    u := u2
    u := checkAddSpaces u
    u := checkSplit2Words u
    u := checkAddMacros u
    u := checkTexht u
--  checkBalance u
    okBefore := null $checkErrorFlag
    checkArguments u
    if $checkErrorFlag then u := checkFixCommonProblem u
    checkRecordHash u
--  u := checkTranVerbatim u
    checkDecorateForHt u

checkTexht u ==
  count := 0
  acc   := nil
  while u repeat
    x := first u
    if x = '"\texht" and (u := IFCDR u) then
        if not (IFCAR u = $charLbrace) then
           checkDocError '"First left brace after \texht missing"
        count := 1  -- drop first argument including braces of \texht
        while ((y := IFCAR (u := rest u))~= $charRbrace or count > 1) repeat
          if y = $charLbrace then count := count + 1
          if y = $charRbrace then count := count - 1
        x :=  IFCAR (u := rest u)  -- drop first right brace of 1st arg
    if x = '"\httex" and (u := IFCDR u) and (IFCAR u = $charLbrace) then
        acc := [IFCAR u,:acc]      --left  brace: add it
        while (y := IFCAR (u := rest u)) ~= $charRbrace repeat (acc := [y,:acc])
        acc := [IFCAR u,:acc]      --right brace: add it
        x :=  IFCAR (u := rest u)  --left brace:  forget it
        while IFCAR (u := rest u) ~= $charRbrace repeat 'skip
        x :=  IFCAR (u := rest u)  --forget right brace: move to next char
    acc := [x,:acc]
    u := rest u
  NREVERSE acc

checkRecordHash u ==
  while u repeat
    x := first u
    if STRINGP x and x.0 = $charBack then
      if member(x,$HTlinks) and (u := checkLookForLeftBrace IFCDR u)
           and (u := checkLookForRightBrace IFCDR u)
             and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
        htname := intern IFCAR u
        entry := HGET($htHash,htname) or [nil]
        HPUT($htHash,htname,[first entry,:[[$name,:$origin],:rest entry]])
      else if member(x,$HTlisplinks) and (u := checkLookForLeftBrace IFCDR u)
            and (u := checkLookForRightBrace IFCDR u)
              and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
        htname := intern checkGetLispFunctionName checkGetStringBeforeRightBrace u
        entry := HGET($lispHash,htname) or [nil]
        HPUT($lispHash,htname,[first entry,:[[$name,:$origin],:rest entry]])
      else if ((p := member(x,'("\gloss" "\spadglos")))
                 or (q := member(x,'("\glossSee" "\spadglosSee"))))
                    and (u := checkLookForLeftBrace IFCDR u)
                      and (u := IFCDR u) then
          if q then
             u := checkLookForRightBrace u
             u := checkLookForLeftBrace IFCDR u
             u := IFCDR u
          htname := intern checkGetStringBeforeRightBrace u
          entry := HGET($glossHash,htname) or [nil]
          HPUT($glossHash,htname,[first entry,:[[$name,:$origin],:rest entry]])
      else if x = '"\spadsys" and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
          s := checkGetStringBeforeRightBrace u
          if s.0 = char '_) then s := SUBSTRING(s,1,nil)
          parse := checkGetParse s
          null parse => checkDocError ['"Unparseable \spadtype: ",s]
          not member(opOf parse,$currentSysList) =>
            checkDocError ['"Bad system command: ",s]
          atom parse or not (parse is ['set,arg]) => 'ok  ---assume ok
          not spadSysChoose($setOptions,arg) =>
            checkDocError ['"Incorrect \spadsys: ",s]
            entry := HGET($sysHash,htname) or [nil]
            HPUT($sysHash,htname,[first entry,:[[$name,:$origin],:rest entry]])
      else if x = '"\spadtype" and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
          s := checkGetStringBeforeRightBrace u
          parse := checkGetParse s
          null parse => checkDocError ['"Unparseable \spadtype: ",s]
          n := checkNumOfArgs parse
          null n => checkDocError ['"Unknown \spadtype: ", s]
          atom parse and n > 0 => 'skip
          null (key := checkIsValidType parse) =>
            checkDocError ['"Unknown \spadtype: ", s]
          atom key => 'ok
          checkDocError ['"Wrong number of arguments: ",form2HtString key]
      else if member(x,'("\spadop" "\keyword")) and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
          x := intern checkGetStringBeforeRightBrace u
          not (GETL(x,'Led) or GETL(x,'Nud)) =>
            checkDocError ['"Unknown \spadop: ",x]
    u := rest u
  'done

checkGetParse s == ncParseFromString removeBackslashes s

removeBackslashes s ==
    s = '"" => '""
    (k := charPosition($charBack,s,0)) < #s =>
      k = 0 => removeBackslashes SUBSTRING(s,1,nil)
      STRCONC(SUBSTRING(s,0,k),removeBackslashes SUBSTRING(s,k + 1,nil))
    s

checkNumOfArgs conform ==
  conname := opOf conform
  constructor? conname or (conname := abbreviation? conname) =>
    #GETDATABASE(conname,'CONSTRUCTORARGS)
  nil  --signals error

checkIsValidType form == main where
--returns ok if correct, form is wrong number of arguments, nil if unknown
  main ==
    atom form => 'ok
    [op,:args] := form
    conname := (constructor? op => op; abbreviation? op)
    null conname => nil
    fn(form,GETDATABASE(conname,'COSIG))
  fn(form,coSig) ==
    #form ~= #coSig => form
    or/[null checkIsValidType x for x in rest form for flag in rest coSig | flag]
      => nil
    'ok

checkGetLispFunctionName s ==
  n := #s
  (k := charPosition(char '_|,s,1)) and k < n and
    (j := charPosition(char '_|,s,k + 1)) and j < n => SUBSTRING(s,k + 1,j-k-1)
  checkDocError ['"Ill-formed lisp expression : ",s]
  'illformed

checkGetStringBeforeRightBrace u ==
  acc := nil
  while u repeat
    x := first u
    x = $charRbrace => return "STRCONC"/(NREVERSE acc)
    acc := [x,:acc]
    u := rest u

--  checkTranVerbatim u ==
--    acc := nil
--    while u repeat
--      x := first u
--      x = '"\begin" and checkTranVerbatimMiddle u is [middle,:r] =>
--        acc := [$charRbrace,:middle,$charLbrace,'"\spadpaste",:acc]
--        u := r
--      if x = '"\spadcommand" then x := '"\spadpaste"
--      acc := [x,:acc]
--      u := rest u
--    NREVERSE acc
--
--  checkTranVerbatimMiddle u ==
--      (y := IFCAR (v := IFCDR u)) = $charLbrace and
--        (y := IFCAR (v := IFCDR v)) = '"verbatim" and
--          (y := IFCAR (v := IFCDR v)) = $charRbrace =>
--             w := IFCDR v
--             middle := nil
--             while w and (z := first w) ~= '"\end" repeat
--               middle := [z,:middle]
--               w := rest w
--             if (y := IFCAR (w := IFCDR w)) = $charLbrace and
--               (y := IFCAR (w := IFCDR w))  = '"verbatim" and
--                 (y := IFCAR (w := IFCDR w)) = $charRbrace then
--                    u := IFCDR w
--             else
--                checkDocError '"Missing \end{verbatim}"
--                u := w
--             [middle,:u]
--
--  checkTranVerbatim1 u ==
--    acc := nil
--    while u repeat
--      x := first u
--      x = '"\begin" and (y := IFCAR (v := IFCDR u)) = $charLbrace and
--        (y := IFCAR (v := IFCDR v)) = '"verbatim" and
--          (y := IFCAR (v := IFCDR v)) = $charRbrace =>
--             w := IFCDR v
--             middle := nil
--             while w and (z := first w) ~= '"\end" repeat
--               middle := [z,:middle]
--               w := rest w
--             if (y := IFCAR (w := IFCDR w)) = $charLbrace and
--               (y := IFCAR (w := IFCDR w))  = '"verbatim" and
--                 (y := IFCAR (w := IFCDR w)) = $charRbrace then
--                    u := IFCDR w
--             acc := [$charRbrace,:middle,$charLbrace,'"\spadpaste",:acc]
--      if x = '"\spadcommand" then x := '"\spadpaste"
--      acc := [x,:acc]
--      u := rest u
--    NREVERSE acc

appendOver [head,:tail] ==
 acc := LASTNODE head
 for x in tail repeat
   end := LASTNODE x
   RPLACD(acc,x)
   acc := end
 head

checkRemoveComments lines ==
  while lines repeat
    do
      line := checkTrimCommented first lines
      if firstNonBlankPosition line >= 0 then acc := [line,:acc]
    lines := rest lines
  NREVERSE acc

checkTrimCommented line ==
  n := #line
  k := htcharPosition(char '_%,line,0)
  --line beginning with % is a comment
  k = 0 => '""
  --remarks beginning with %% are comments
  k >= n - 1 or line.(k + 1) ~= char '_% => line
  k < #line => SUBSTRING(line,0,k)
  line

htcharPosition(char,line,i) ==
  m := #line
  k := charPosition(char,line,i)
  k = m => k
  k > 0 =>
    line.(k - 1) ~= $charBack => k
    htcharPosition(char,line,k + 1)
  0

checkAddMacros u ==
  acc := nil
  verbatim := false
  while u repeat
    x := first u
    acc :=
      x = '"\end{verbatim}" =>
        verbatim := false
        [x, :acc]
      verbatim => [x, :acc]
      x = '"\begin{verbatim}" =>
        verbatim := true
        [x, :acc]
      y := LASSOC(x,$HTmacs) => [:y,:acc]
      [x,:acc]
    u := rest u
  NREVERSE acc

checkComments(nameSig,lines) == main where
  main ==
    $checkErrorFlag: local := false
    margin := checkGetMargin lines
    if (null BOUNDP '$attribute? or null $attribute?)
      and nameSig ~= 'constructor then lines :=
        [checkTransformFirsts(first nameSig,first lines,margin),:rest lines]
    u := checkIndentedLines(lines, margin)
    $argl := checkGetArgs first u      --set $argl
    u2 := nil
    verbatim := nil
    for x in u repeat
        w := newString2Words x
        verbatim =>
          w and first w = '"\end{verbatim}" =>
            verbatim := false
            u2 := append(u2, w)
          u2 := append(u2, [x])
        w and first w = '"\begin{verbatim}" =>
            verbatim := true
            u2 := append(u2, w)
        u2 := append(u2, w)
    u := u2
    u := checkAddSpaces u
    u := checkIeEg u
    u := checkSplit2Words u
    checkBalance u
    okBefore := null $checkErrorFlag
    checkArguments u
    if $checkErrorFlag then u := checkFixCommonProblem u
    v := checkDecorate u
    res := "STRCONC"/[y for y in v]
    res := checkAddPeriod res
    if $checkErrorFlag then pp res
    res

checkIndentedLines(u, margin) ==
  verbatim := false
  u2 := nil
  for x in u repeat
    k := firstNonBlankPosition x
    k = -1 =>
        verbatim => u2 := [:u2, $charFauxNewline]
        u2 := [:u2, '"\blankline "]
    s := SUBSTRING(x, k, nil)
    s = '"\begin{verbatim}" =>
        verbatim := true
        u2 := [:u2, s]
    s = '"\end{verbatim}" =>
        verbatim := false
        u2 := [:u2, s]
    verbatim => u2 := [:u2, SUBSTRING(x, margin, nil)]
    margin = k => u2 := [:u2, s]
    u2 := [:u2, STRCONC('"\indented{",STRINGIMAGE(k-margin),'"}{",checkAddSpaceSegments(s,0),'"}")]
  u2

newString2Words l ==
  not STRINGP l => [l]
  m := MAXINDEX l
  m = -1 => NIL
  i := 0
  [w while newWordFrom(l,i,m) is [w,i]]

newWordFrom(l,i,m) ==
  while i <= m and l.i = $charBlank repeat i := i + 1
  i > m => NIL
  i0 := i
  ch := l.i
  ch = $charFauxNewline => [$stringFauxNewline, i+ 1]
  done := false
  while i <= m and not done repeat
    ch := l.i
    ch = $charBlank or ch = $charFauxNewline => done := true
    i := i + 1
  [SUBSTRING(l, i0, i - i0), i]

checkAddPeriod s ==  --No, just leave blank at the end (rdj: 10/18/91)
  m := MAXINDEX s
  lastChar := s . m
  lastChar = char '_! or lastChar = char '_? or lastChar = char '_. => s
  lastChar = char '_, or lastChar = char '_; =>
    s . m := (char '_.)
    s
  s

checkGetArgs u ==
  NOT STRINGP u => nil
  m := MAXINDEX u
  k := firstNonBlankPosition(u)
  k > 0 => checkGetArgs SUBSTRING(u,k,nil)
  stringPrefix?('"\spad{",u) =>
    k := getMatchingRightPren(u,6,char '_{,char '_}) or m
    checkGetArgs SUBSTRING(u,6,k-6)
  (i := charPosition(char '_(,u,0)) > m => nil
  (u . m) ~= char '_) => nil
  while (k := charPosition($charComma,u,i + 1)) < m repeat
    acc := [trimString SUBSTRING(u,i + 1,k - i - 1),:acc]
    i := k
  NREVERSE [SUBSTRING(u,i + 1,m - i - 1),:acc]

checkGetMargin lines ==
  while lines repeat
    do
      x := first lines
      k := firstNonBlankPosition x
      k = -1 => nil
      margin := (margin => MIN(margin,k); k)
    lines := rest lines
  margin or 0

firstNonBlankPosition(x,:options) ==
  start := IFCAR options or 0
  k := -1
  for i in start..MAXINDEX x repeat
    if x.i ~= $charBlank then return (k := i)
  k

checkAddIndented(x,margin) ==
  k := firstNonBlankPosition x
  k = -1 => '"\blankline "
  margin = k => x
  STRCONC('"\indented{",STRINGIMAGE(k-margin),'"}{",checkAddSpaceSegments(SUBSTRING(x,k,nil),0),'"}")

checkAddSpaceSegments(u,k) ==
  m := MAXINDEX u
  i := charPosition($charBlank,u,k)
  m < i => u
  j := i
  while (j := j + 1) < m and u.j = (char '_  ) repeat 'continue
  n := j - i   --number of blanks
  n > 1 => STRCONC(SUBSTRING(u,0,i),'"\space{",
             STRINGIMAGE n,'"}",checkAddSpaceSegments(SUBSTRING(u,i + n,nil),0))
  checkAddSpaceSegments(u,j)

checkTrim($x,lines) == main where
  main ==
    s := [wherePP first lines]
    for x in rest lines repeat
      j := wherePP x
      if not MEMQ(j,s) then
        checkDocError [$x,'" has varying indentation levels"]
        s := [j,:s]
    [trim y for y in lines]
  wherePP(u) ==
    k := charPosition($charPlus,u,0)
    k = #u or charPosition($charPlus,u,k + 1) ~= k + 1 =>
      systemError '" Improper comment found"
    k
  trim(s) ==
    k := wherePP(s)
    return SUBSTRING(s,k + 2,nil)
    m := MAXINDEX s
    n := k + 2
    for j in (k + 2)..m while s.j = $charBlank repeat (n := n + 1)
    SUBSTRING(s,n,nil)

checkExtract(header,lines) ==
  while lines repeat
    line := first lines
    k := firstNonBlankPosition line           --k gives margin of Description:
    substring?(header,line,k) => return nil
    lines := rest lines
  null lines => nil
  u := first lines
  j := charPosition(char '_:,u,k)
  margin := k
  firstLines :=
    (k := firstNonBlankPosition(u,j + 1)) ~= -1 =>
      [SUBSTRING(u,j + 1,nil),:rest lines]
    rest lines
  --now look for another header; if found skip all rest of these lines
  acc := nil
  for line in firstLines repeat
    do
      m := #line
      (k := firstNonBlankPosition line) = -1     => 'skip  --include if blank
      k > margin                                 => 'skip  --include if idented
      not UPPER_-CASE_-P line.k                  => 'skip  --also if not upcased
      (j := charPosition(char '_:,line,k)) = m   => 'skip  --or if not colon, or
      (i := charPosition(char '_ ,line,k+1)) < j => 'skip  --blank before colon
      return nil
    acc := [line,:acc]
  NREVERSE acc

checkFixCommonProblem u ==
  acc := nil
  while u repeat
    x := first u
    x = $charLbrace and member(next := IFCAR rest u,$HTspadmacros) and
                       (IFCAR IFCDR rest u ~= $charLbrace) =>
      checkDocError ['"Reversing ",next,'" and left brace"]
      acc := [$charLbrace,next,:acc]  --reverse order of brace and command
      u := rest rest u
    acc := [x,:acc]
    u := rest u
  NREVERSE acc

checkDecorate u ==
  count := 0
  spadflag := false    --means OK to wrap single letter words with \s{}
  mathSymbolsOk := false
  acc := nil
  verbatim := false
  while u repeat
    x := first u

    if not verbatim then
      if x = '"\em" then
        if count > 0 then
          mathSymbolsOk := count - 1
          spadflag := count - 1
        else checkDocError ['"\em must be enclosed in braces"]
      if member(x,'("\spadpaste" "\spad" "\spadop")) then mathSymbolsOk := count
      if member(x,'("\s" "\spadtype" "\spadsys" "\example" "\andexample" "\spadop" "\spad" "\spadignore" "\spadpaste" "\spadcommand" "\footnote")) then spadflag := count
      else if x = $charLbrace then
        count := count + 1
      else if x = $charRbrace then
        count := count - 1
        if mathSymbolsOk = count then mathSymbolsOk := false
        if spadflag = count then spadflag := false
      else if not mathSymbolsOk and member(x,'("+" "*" "=" "==" "->")) then
        if $checkingXmptex? then
          checkDocError ["Symbol ",x,'" appearing outside \spad{}"]

    acc :=
      x = '"\end{verbatim}" =>
        verbatim := false
        [x, :acc]
      verbatim => [x, :acc]
      x = '"\begin{verbatim}" =>
        verbatim := true
        [x, :acc]

      x = '"\begin" and first (v := IFCDR u) = $charLbrace and
        first (v := IFCDR v) = '"detail" and first (v := IFCDR v) = $charRbrace
          =>
            u := v
            ['"\blankline ",:acc]
      x = '"\end" and first (v := IFCDR u) = $charLbrace and
        first (v := IFCDR v) = '"detail" and first (v := IFCDR v) = $charRbrace
          =>
            u := v
            acc
      x = char '_$ or x = '"$"  => ['"\$",:acc]
      x = char '_% or x = '"%"  => ['"\%",:acc]
      x = char '_, or x = '","  => ['",{}",:acc]
      x = '"\spad" => ['"\spad",:acc]
      STRINGP x and DIGITP x.0 => [x,:acc]
      null spadflag and
        (CHARP x and ALPHA_-CHAR_-P x and not MEMQ(x,$charExclusions) or
          member(x,$argl)) => [$charRbrace,x,$charLbrace,'"\spad",:acc]
      null spadflag and ((STRINGP x and not (x.0 = $charBack) and DIGITP(x.(MAXINDEX x))) or member(x, '("true" "false"))) =>
        [$charRbrace,x,$charLbrace,'"\spad",:acc]  --wrap x1, alpha3, etc
      CHARP x => [checkAddBackSlashes x,:acc]
      xcount := #x
      xcount = 3 and x.1 = char 't and x.2 = char 'h =>
        ['"th",$charRbrace,x.0,$charLbrace,'"\spad",:acc]
      xcount = 4 and x.1 = char '_- and x.2 = char 't and x.3 = char 'h =>
        ['"-th",$charRbrace,x.0,$charLbrace,'"\spad",:acc]
      xcount = 2 and x.1 = char 'i or              --wrap ei, xi, hi
        null spadflag and xcount > 0 and xcount < 4 and not member(x,'("th" "rd" "st")) and
          hasNoVowels x =>                         --wrap words with no vowels
            [$charRbrace, checkAddBackSlashes x, $charLbrace, '"\spad", :acc]
      [checkAddBackSlashes x,:acc]
    u := rest u
  NREVERSE acc

hasNoVowels x ==
  max := MAXINDEX x
  x.max = char 'y => false
  and/[not isVowel(x.i) for i in 0..max]

isVowel c ==
  EQ(c,char 'a) or EQ(c,char 'e) or EQ(c,char 'i) or EQ(c,char 'o) or EQ(c,char 'u) or
    EQ(c,char 'A) or EQ(c,char 'E) or EQ(c,char 'I) or EQ(c,char 'O) or EQ(c,char 'U)


checkAddBackSlashes s ==
  (CHARP s and (c := s)) or (#s = 1 and (c := s.0)) =>
    MEMQ(c, $charEscapeList) => STRCONC($charBack, c)
    s
  k := 0
  m := MAXINDEX s
  insertIndex := nil
  while k <= m repeat
    do
      char := s.k
      char = $charBack => k := k + 2
      MEMQ(char,$charEscapeList) => return (insertIndex := k)
    k := k + 1
  insertIndex => checkAddBackSlashes STRCONC(SUBSTRING(s,0,insertIndex),$charBack,s.k,SUBSTRING(s,insertIndex + 1,nil))
  s

checkAddSpaces u ==
  null u => nil
  null rest u => u
  space := $charBlank
  u2 := nil
  for i in 1.. for f in u repeat
    -- want newlines before and after begin/end verbatim and between lines
    -- since this might be written to a file, we can't really use
    -- newline characters. The Browser and HD will do the translation
    -- later.
    if f = '"\begin{verbatim}" then
        space := $charFauxNewline
        if null u2 then u2 := [space]

    if i > 1 then u2 := [:u2, space, f]
    else u2 := [:u2, f]

    if f = '"\end{verbatim}" then
        u2 := [:u2, space]
        space := $charBlank
  u2

checkIeEg u ==
  acc := nil
  verbatim := false
  while u repeat
    x := first u
    acc :=
      x = '"\end{verbatim}" =>
        verbatim := false
        [x, :acc]
      verbatim => [x, :acc]
      x = '"\begin{verbatim}" =>
        verbatim := true
        [x, :acc]
      z := checkIeEgfun x => [:NREVERSE z,:acc]
      [x,:acc]
    u := rest u
  NREVERSE acc

checkIeEgfun x ==
  CHARP x => nil
  x = '"" => nil
  m := MAXINDEX x
  for k in 0..(m - 3) repeat
    x.(k + 1) = $charPeriod and x.(k + 3) = $charPeriod and
     (x.k = char 'i and x.(k + 2) = char 'e and (key := '"that is")
       or x.k = char 'e and x.(k + 2) = char 'g and (key := '"for example")) =>
          firstPart := (k > 0 => [SUBSTRING(x,0,k)]; nil)
          result := [:firstPart,'"\spadignore{",SUBSTRING(x,k,4),'"}",
                     :checkIeEgfun SUBSTRING(x,k+4,nil)]
  result

checkSplit2Words u ==
  acc := nil
  while u repeat
    x := first u
    acc :=
      x = '"\end{verbatim}" =>
        verbatim := false
        [x, :acc]
      verbatim => [x, :acc]
      x = '"\begin{verbatim}" =>
        verbatim := true
        [x, :acc]
      z := checkSplitBrace x => [:NREVERSE z,:acc]
      [x,:acc]
    u := rest u
  NREVERSE acc

checkSplitBrace x ==
  CHARP x => [x]
  #x = 1 => [x.0]
  (u := checkSplitBackslash x)
     and rest u  => "append"/[checkSplitBrace y for y in u]
  m := MAXINDEX x
  (u := checkSplitOn x)
     and rest u  => "append"/[checkSplitBrace y for y in u]
  (u := checkSplitPunctuation x)
     and rest u  => "append"/[checkSplitBrace y for y in u]
  [x]

checkSplitBackslash x ==
  not STRINGP x => [x]
  m := MAXINDEX x
  (k := charPosition($charBack,x,0)) < m =>
    m = 1 or ALPHA_-CHAR_-P(x . (k + 1)) =>     --starts with a backslash so..
      (k := charPosition($charBack,x,1)) < m => --..see if there is another
         [SUBSTRING(x,0,k),:checkSplitBackslash SUBSTRING(x,k,nil)]  -- yup
      [x]                                       --no, just return line
    k = 0 => --starts with backspace but x.1 is not a letter; break it up
      [SUBSTRING(x,0,2),:checkSplitBackslash SUBSTRING(x,2,nil)]
    u := SUBSTRING(x,0,k)
    v := SUBSTRING(x,k,2)
    k + 1 = m => [u,v]
    [u,v,:checkSplitBackslash SUBSTRING(x,k + 2,nil)]
  [x]

checkSplitPunctuation x ==
  CHARP x => [x]
  m := MAXINDEX x
  m < 1 => [x]
  lastchar := x.m
  lastchar = $charPeriod and x.(m - 1) = $charPeriod =>
    m = 1 => [x]
    m > 3 and x.(m-2) = $charPeriod =>
      [:checkSplitPunctuation SUBSTRING(x,0,m-2),'"..."]
    [:checkSplitPunctuation SUBSTRING(x,0,m-1),'".."]
  lastchar = $charPeriod or lastchar = $charSemiColon or lastchar = $charComma
    => [SUBSTRING(x,0,m),lastchar]
  m > 1 and x.(m - 1) = $charQuote => [SUBSTRING(x,0,m - 1),SUBSTRING(x,m-1,nil)]
  (k := charPosition($charBack,x,0)) < m =>
    k = 0 =>
      m = 1 or HGET($htMacroTable,x) or ALPHA_-CHAR_-P x.1 => [x]
      v := SUBSTRING(x,2,nil)
      [SUBSTRING(x,0,2),:checkSplitPunctuation v]
    u := SUBSTRING(x,0,k)
    v := SUBSTRING(x,k,nil)
    [:checkSplitPunctuation u,:checkSplitPunctuation v]
  (k := charPosition($charDash,x,1)) < m =>
    u := SUBSTRING(x,k + 1,nil)
    [SUBSTRING(x,0,k),$charDash,:checkSplitPunctuation u]
  [x]

checkSplitOn(x) ==
  CHARP x => [x]
  l := $charSplitList
  m := MAXINDEX x
  while l repeat
    char := first l
    do
      m = 0 and x.0 = char => return (k := -1)  --special exit
      k := charPosition(char,x,0)
      k > 0 and x.(k - 1) = $charBack => [x]
      k <= m => return k
    l := rest l
  null l => [x]
  k = -1 => [char]
  k = 0 => [char,SUBSTRING(x,1,nil)]
  k = MAXINDEX x => [SUBSTRING(x,0,k),char]
  [SUBSTRING(x,0,k),char,:checkSplitOn SUBSTRING(x,k + 1,nil)]


checkBalance u ==
  checkBeginEnd u
  stack := nil
  while u repeat
    do
      x := first u
      openClose := assoc(x, $checkPrenAlist) =>  --is it an open bracket?
          stack := [first openClose, :stack]   --yes, push the open bracket
      open  := rassoc(x,$checkPrenAlist) =>  --it is a close bracket!
        stack is [top,:restStack] => --does corresponding open bracket match?
          if open ~= top then          --yes: just pop the stack
            checkDocError
              ['"Mismatch: left ",checkSayBracket top,'" matches right ",checkSayBracket open]
          stack := restStack
        checkDocError ['"Missing left ",checkSayBracket open]
    u := rest u
  if stack then
    for x in NREVERSE stack repeat
      checkDocError ['"Missing right ",checkSayBracket x]
  u

checkSayBracket x ==
  x = char '_( or x = char '_) => '"pren"
  x = char '_{ or x = char '_} => '"brace"
  '"bracket"

checkBeginEnd u ==
  beginEndStack := nil
  while u repeat
    IDENTITY
      x := first u
      STRINGP x and x.0 = $charBack and #x > 2 and not HGET($htMacroTable,x)
        and not (x = '"\spadignore") and IFCAR IFCDR u = $charLbrace
          and not
            (substring?('"\radiobox",x,0) or substring?('"\inputbox",x,0))=>
             --allow 0 argument guys to pass through
              checkDocError ["Unexpected HT command: ",x]
      x = '"\beginitems" =>
        beginEndStack := ["items",:beginEndStack]
      x = '"\begin" =>
        u is [., =$charLbrace, y, :r] and first r = $charRbrace =>
          if not member(y,$beginEndList) then
            checkDocError ['"Unknown begin type: \begin{",y,'"}"]
          beginEndStack := [y,:beginEndStack]
          u := r
        checkDocError ['"Improper \begin command"]
      x = '"\item" =>
        member(IFCAR beginEndStack,'("items" "menu")) => nil
        null beginEndStack =>
          checkDocError ['"\item appears outside a \begin-\end"]
        checkDocError ['"\item appears within a \begin{",IFCAR beginEndStack,'"}.."]
      x = '"\end" =>
        u is [., =$charLbrace, y, :r] and first r = $charRbrace =>
          y = IFCAR beginEndStack =>
            beginEndStack := rest beginEndStack
            u := r
          checkDocError ['"Trying to match \begin{",IFCAR beginEndStack,'"} with \end{",y,"}"]
        checkDocError ['"Improper \end command"]
    u := rest u
  beginEndStack => checkDocError ['"Missing \end{",first beginEndStack,'"}"]
  'ok

checkArguments u ==
  while u repeat
    do
      x := first u
      null (k := HGET($htMacroTable,x)) => 'skip
      k = 0 => 'skip
      k > 0 => checkHTargs(x,rest u,k,nil)
      checkHTargs(x,rest u,-k,true)
    u := rest u
  u

checkHTargs(keyword,u,nargs,integerValue?) ==
--u should start with an open brace ...
   nargs = 0 => 'ok
   if not (u := checkLookForLeftBrace u) then
     return checkDocError ['"Missing argument for ",keyword]
   if not (u := checkLookForRightBrace IFCDR u) then
     return checkDocError ['"Missing right brace for ",keyword]
   checkHTargs(keyword,rest u,nargs - 1,integerValue?)

checkLookForLeftBrace(u) ==   --return line beginning with left brace
  while u repeat
    x := first u
    if x = $charLbrace then return u
    x ~= $charBlank => return nil
    u := rest u
  u

checkLookForRightBrace(u) ==  --return line beginning with right brace
  count := 0
  while u repeat
    x := first u
    do
      x = $charRbrace =>
        count = 0 => return (found := u)
        count := count - 1
      x = $charLbrace => count := count + 1
    u := rest u
  found

checkInteger s ==
  CHARP s => false
  s = '"" => false
  and/[DIGIT_-CHAR_-P s.i for i in 0..MAXINDEX s]

checkTransformFirsts(opname,u,margin) ==
--case 1: \spad{...
--case 2: form(args)
--case 3: form arg
--case 4: op arg
--case 5: arg op arg
  namestring := PNAME opname
  if namestring = '"Zero" then namestring := '"0"
  else if namestring = '"One" then namestring := '"1"
  margin > 0 =>
    s := leftTrim u
    STRCONC(fillerSpaces(margin, '" "), checkTransformFirsts(opname, s, 0))
  m := MAXINDEX u
  m < 2 => u
  u.0 = $charBack => u
  ALPHA_-CHAR_-P u.0 =>
    i := checkSkipToken(u,0,m) or return u
    j := checkSkipBlanks(u,i,m) or return u
    open := u.j
    open = char '_[ and (close := char '_]) or
          open = char '_(  and (close := char '_)) =>
      k := getMatchingRightPren(u,j + 1,open,close)
      namestring ~= (firstWord := SUBSTRING(u,0,i)) =>
        checkDocError ['"Improper first word in comments: ",firstWord]
        u
      null k =>
         if open = char '_[
           then checkDocError ['"Missing close bracket on first line: ", u]
           else checkDocError ['"Missing close parenthesis on first line: ", u]
         u
      STRCONC('"\spad{",SUBSTRING(u,0,k + 1),'"}",SUBSTRING(u,k + 1,nil))
    k := checkSkipToken(u,j,m) or return u
    infixOp := INTERN SUBSTRING(u,j,k - j)
    not GETL(infixOp,'Led) =>                                     --case 3
      namestring ~= (firstWord := SUBSTRING(u,0,i)) =>
        checkDocError ['"Improper first word in comments: ",firstWord]
        u
      #(p := PNAME infixOp) = 1 and (open := p.0) and
        (close := LASSOC(open,$checkPrenAlist)) =>  --have an open bracket
          l := getMatchingRightPren(u,k + 1,open,close)
          if l > MAXINDEX u then l := k - 1
          STRCONC('"\spad{",SUBSTRING(u,0,l + 1),'"}",SUBSTRING(u,l + 1,nil))
      STRCONC('"\spad{",SUBSTRING(u,0,k),'"}",SUBSTRING(u,k,nil))
    l := checkSkipBlanks(u,k,m) or return u
    n := checkSkipToken(u,l,m) or return u
    namestring ~= PNAME infixOp =>
      checkDocError ['"Improper initial operator in comments: ",infixOp]
      u
    STRCONC('"\spad{",SUBSTRING(u,0,n),'"}",SUBSTRING(u,n,nil))   --case 5
  true =>          -- not ALPHA_-CHAR_-P u.0 =>
    i := checkSkipToken(u,0,m) or return u
    namestring ~= (firstWord := SUBSTRING(u,0,i)) =>
      checkDocError ['"Improper first word in comments: ",firstWord]
      u
    prefixOp := INTERN SUBSTRING(u,0,i)
    not GETL(prefixOp,'Nud) =>
      u ---what could this be?
    j := checkSkipBlanks(u,i,m) or return u
    u.j = char '_( =>                                            --case 4
      j := getMatchingRightPren(u,j + 1,char '_(,char '_))
      j > m => u
      STRCONC('"\spad{",SUBSTRING(u,0,j + 1),'"}",SUBSTRING(u,j + 1,nil))
    k := checkSkipToken(u,j,m) or return u
    namestring ~= (firstWord := SUBSTRING(u,0,i)) =>
      checkDocError ['"Improper first word in comments: ",firstWord]
      u
    STRCONC('"\spad{",SUBSTRING(u,0,k),'"}",SUBSTRING(u,k,nil))

getMatchingRightPren(u,j,open,close) ==
  count := 0
  m := MAXINDEX u
  for i in j..m repeat
    c := u . i
    do
      c = close =>
        count = 0 => return (found := i)
        count := count - 1
      c = open => count := count + 1
  found

checkSkipBlanks(u,i,m) ==
  while i < m and u.i = $charBlank repeat i := i + 1
  i = m => nil
  i

checkSkipToken(u,i,m) ==
  ALPHA_-CHAR_-P(u.i) => checkSkipIdentifierToken(u,i,m)
  checkSkipOpToken(u,i,m)

checkSkipOpToken(u,i,m) ==
  while i < m and
    (not(checkAlphabetic(u.i)) and not(member(u.i,$charDelimiters))) repeat
      i := i + 1
  i = m => nil
  i

checkSkipIdentifierToken(u,i,m) ==
  while i < m and checkAlphabetic u.i repeat i := i + 1
  i = m => nil
  i

checkAlphabetic c ==
  ALPHA_-CHAR_-P c or DIGITP c or MEMQ(c,$charIdentifierEndings)


--=======================================================================
--             Report Documentation Error
--=======================================================================
checkDocError1 u ==
--when compiling for documentation, ignore certain errors
  BOUNDP '$compileDocumentation and $compileDocumentation => nil
  checkDocError u

checkDocError u ==
  $checkErrorFlag := true
  msg :=
    $recheckingFlag =>
      $constructorName => checkDocMessage u
      concat('"> ",u)
    $constructorName => checkDocMessage u
    u
  if $exposeFlag and $exposeFlagHeading then
    sayBrightly1($exposeFlagHeading,$outStream)
    sayBrightly $exposeFlagHeading
    $exposeFlagHeading := nil
  sayBrightly msg
  if $exposeFlag then sayBrightly1(msg,$outStream)
  --if called by checkDocFile (see file checkdoc.boot)

checkDocMessage u ==
  sourcefile := GETDATABASE($constructorName,'SOURCEFILE)
  person := '"---"
  middle :=
    BOUNDP '$x => ['"(",$x,'"): "]
    ['": "]
  concat(person,'">",sourcefile,'"-->",$constructorName,middle,u)

checkDecorateForHt u ==
  count := 0
  spadflag := false    --means OK to wrap single letter words with \s{}
  while u repeat
    x := first u
    do
      if x = '"\em" then
        if count > 0 then spadflag := count - 1
        else checkDocError ['"\em must be enclosed in braces"]
      if member(x,'("\s" "\spadop" "\spadtype" "\spad" "\spadpaste" "\spadcommand" "\footnote")) then spadflag := count
      else if x = $charLbrace then count := count + 1
      else if x = $charRbrace then
        count := count - 1
        if spadflag = count then spadflag := false
      else if not spadflag and member(x,'("+" "*" "=" "==" "->")) then
        if $checkingXmptex? then
          checkDocError ["Symbol ",x,'" appearing outside \spad{}"]
      x = '"$" or x = '"%" => checkDocError ['"Unescaped ",x]
    u := rest u
  u
