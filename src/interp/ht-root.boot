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

$historyDisplayWidth := 120
$newline := char 10

downlink page ==
  $saturn => downlinkSaturn page
  htInitPage('"Bridge",nil)
  htSayList(['"\replacepage{", page, '"}"])
  htShowPage()

downlinkSaturn fn ==
  u := dbReadLines(fn)
  lines := '""
  while u is [line,:u] repeat
    n := MAXINDEX line
    n < 1 => nil
    line.0 = (char '_%) => nil
    lines := STRCONC(lines,line)
  issueHTSaturn lines

dbNonEmptyPattern pattern ==
  null pattern => '"*"
  pattern := STRINGIMAGE pattern
  #pattern > 0 => pattern
  '"*"

htSystemVariables() == main where
  main ==
    not $fullScreenSysVars => htSetVars()
    classlevel := $UserLevel
    $levels : local := '(compiler development interpreter)
    $heading  : local := nil
    while classlevel ~= first $levels repeat $levels := rest $levels
    table := NREVERSE fn($setOptions,nil,true)
    htInitPage('"System Variables",nil)
    htSay '"\beginmenu"
    lastHeading := nil
    for [heading,name,message,.,key,variable,options,func] in table repeat
      htSay('"\newline\item ")
      if heading = lastHeading then htSay '"\tab{8}" else
        htSayList([heading, '"\tab{8}"])
        lastHeading := heading
      htSayList(['"{\em ", name, "}\tab{22}", message])
      htSay('"\tab{80}")
      key = 'FUNCTION =>
         null options => htMakePage [['bcLinks,['"reset",'"",func,nil]]]
         [msg,class,var,valuesOrFunction,:.] := first options  --skip first message
         functionTail(name,class,var,valuesOrFunction)
         for option in rest options repeat
           option is ['break,:.] => 'skip
           [msg,class,var,valuesOrFunction,:.] := option
           htSayList(['"\newline\tab{22}", msg,'"\tab{80}"])
           functionTail(name,class,var,valuesOrFunction)
      val := eval variable
      displayOptions(name,key,variable,val,options)
    htSay '"\endmenu"
    htShowPage()
  functionTail(name,class,var,valuesOrFunction) ==
    val := eval var
    atom valuesOrFunction =>
      htMakePage '((domainConditions (isDomain STR (String))))
      htMakePage [['bcLinks,['"reset",'"",'htSetSystemVariableKind,[var,name,nil]]]]
      htMakePage [['bcStrings,[30,STRINGIMAGE val,name,valuesOrFunction]]]
    displayOptions(name,class,var,val,valuesOrFunction)
  displayOptions(name,class,variable,val,options) ==
    class = 'INTEGER =>
      htMakePage [['bcLispLinks,[[['text,options.0,'"-",options.1 or '""]],'"",'htSetSystemVariableKind,[variable,name,'PARSE_-INTEGER]]]]
      htMakePage '((domainConditions (isDomain INT (Integer))))
      htMakePage  [['bcStrings,[5,STRINGIMAGE val,name,'INT]]]
    class = 'STRING =>
      htSay('"{\em ",val,'"}\space{1}")
    for x in options repeat
      val = x or val = true and x = 'on or null val and x = 'off =>
        htSay('"{\em ",x,'"}\space{1}")
      htMakePage [['bcLispLinks,[x,'" ",'htSetSystemVariable,[variable,x]]]]
  fn(t,al,firstTime) ==
    atom t => al
    if firstTime then $heading := opOf first t
    fn(rest t,gn(first t,al),firstTime)
  gn(t,al) ==
    [.,.,class,key,.,options,:.] := t
    not MEMQ(class,$levels) => al
    key = 'LITERALS or key = 'INTEGER or key = 'STRING => [[$heading,:t],:al]
    key = 'TREE => fn(options,al,false)
    key = 'FUNCTION => [[$heading,:t],:al]
    systemError key

htSetSystemVariableKind(htPage,[variable,name,fun]) ==
  value := htpLabelInputString(htPage,name)
  if STRINGP value and fun then value := FUNCALL(fun,value)
--SCM::what to do???  if not FIXP value then userError ???
  SET(variable,value)
  htSystemVariables ()

htSetSystemVariable(htPage,[name,value]) ==
  value :=
    value = 'on => true
    value = 'off => nil
    value
  SET(name,value)
  htSystemVariables ()

htGloss(pattern) == htGlossPage(nil,dbNonEmptyPattern pattern or '"*",true)

htGlossPage(htPage,pattern,tryAgain?) ==
  $wildCard: local := char '_*
  pattern = '"*" => downlink 'GlossaryPage
  filter := pmTransFilter pattern
  grepForm := mkGrepPattern(filter,'none)
  $key: local := 'none
  results := applyGrep(grepForm,'gloss)
  --pathname := STRCONC('"/tmp/",PNAME resultFile,'".text.", getEnv '"SPADNUM")
  --instream := MAKE_-INSTREAM pathname
  defstream := MAKE_-INSTREAM STRCONC(getEnv '"AXIOM",'"/algebra/glossdef.text")
  lines := gatherGlossLines(results,defstream)
  -- OBEY STRCONC('"rm -f ", pathname)
  --PROBE_-FILE(pathname) and DELETE_-FILE(pathname)
  --SHUT instream
  heading :=
    pattern = '"" => '"Glossary"
    null lines => ['"No glossary items match {\em ",pattern,'"}"]
    ['"Glossary items matching {\em ",pattern,'"}"]
  null lines =>
    tryAgain? and #pattern > 0 =>
      (pattern.(k := MAXINDEX(pattern))) = char 's =>
        htGlossPage(htPage,SUBSTRING(pattern,0,k),true)
      UPPER_-CASE_-P pattern.0 =>
        htGlossPage(htPage,DOWNCASE pattern,false)
      errorPage(htPage,['"Sorry",nil,['"\centerline{",:heading,'"}"]])
    errorPage(htPage,['"Sorry",nil,['"\centerline{",:heading,'"}"]])
  htInitPageNoScroll(nil,heading)
  htSay('"\beginscroll\beginmenu")
  for line in lines repeat
    tick := charPosition($tick,line,1)
    htSayList(['"\item{\em \menuitemstyle{}}\tab{0}{\em ",
               escapeString SUBSTRING(line,0,tick),'"} ",
               SUBSTRING(line,tick + 1,nil)])
  htSay '"\endmenu "
  htSay '"\endscroll\newline "
  htMakePage [['bcLinks,['"Search",'"",'htGlossSearch,nil]]]
  htSay '" for glossary entry matching "
  htMakePage [['bcStrings, [24,'"*",'filter,'EM]]]
  htShowPageNoScroll()

gatherGlossLines(results,defstream) ==
  acc := nil
  for keyline in results repeat
    --keyline := read_line instream
    n := charPosition($tick,keyline,0)
    keyAndTick := SUBSTRING(keyline,0,n + 1)
    byteAddress := string2Integer SUBSTRING(keyline,n + 1,nil)
    FILE_-POSITION(defstream,byteAddress)
    line := read_line defstream
    k := charPosition($tick,line,1)
    pointer := SUBSTRING(line,0,k)
    def := SUBSTRING(line,k + 1,nil)
    xtralines := nil
    while not EOFP defstream and (x := read_line defstream) and
      (j := charPosition($tick,x,1)) and (nextPointer := SUBSTRING(x,0,j))
        and (nextPointer = pointer) repeat
          xtralines := [SUBSTRING(x,j + 1,nil),:xtralines]
    acc := [STRCONC(keyAndTick,def, "STRCONC"/NREVERSE xtralines),:acc]
  REVERSE acc

htGlossSearch(htPage,junk) ==  htGloss htpLabelInputString(htPage,'filter)

htGreekSearch(filter) ==
  ss := dbNonEmptyPattern filter
  s := pmTransFilter ss
  s is ['error,:.] => bcErrorPage s
  not s => errorPage(nil,[['"Missing search string"],nil,
    '"\vspace{2}\centerline{To select one of the greek letters:}\newline ",
      '"\centerline{{\em first} enter a search key into the input area}\newline ",
        '"\centerline{{\em then } move the mouse cursor to the work {\em search} and click}"])
  filter := patternCheck s
  names := '(alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu pi)
  for x in names repeat
    superMatch?(filter,PNAME x) => matches := [x,:matches]
    nonmatches := [x,:nonmatches]
  matches    := NREVERSE matches
  nonmatches := NREVERSE nonmatches
  htInitPage('"Greek Names",nil)
  null matches =>
    htInitPage(['"Greek names matching search string {\em ",ss,'"}"],nil)
    htSay("\vspace{2}\centerline{Sorry, but no greek letters match your search string}\centerline{{\em ",ss,"}}\centerline{Click on the up-arrow to try again}")
    htShowPage()
  htInitPage(['"Greek letters matching search string {\em ",ss,'"}"],nil)
  if nonmatches
    then htSayList([
       '"The greek letters that {\em match} your search string {\em ",
       ss, '"}:"])
    else htSay('"Your search string {\em ",ss,"} matches all of the greek letters:")
  htSay('"{\em \table{")
  for x in matches repeat htSayList(['"{", x, '"}"])
  htSay('"}}\vspace{1}")
  if nonmatches then
    htSay('"The greek letters that {\em do not match} your search string:{\em \table{")
    for x in nonmatches repeat htSayList(['"{", x, '"}"])
    htSay('"}}")
  htShowPage()

htTextSearch(filter) ==
  s := pmTransFilter dbNonEmptyPattern filter
  s is ['error,:.] => bcErrorPage s
  not s => errorPage(nil,[['"Missing search string"],nil,
    '"\vspace{2}\centerline{To select one of the lines of text:}\newline ",
      '"\centerline{{\em first} enter a search key into the input area}\newline ",
        '"\centerline{{\em then } move the mouse cursor to the work {\em search} and click}"])
  filter := s
  lines := ['"{{\em Fruit flies} *like* a {\em banana and califlower ears.}}",
            '"{{\em Sneak Sears Silas with Savings Snatch}}"]
  for x in lines repeat
    superMatch?(filter,x) => matches := [x,:matches]
    nonmatches := [x,:nonmatches]
  matches    := NREVERSE matches
  nonmatches := NREVERSE nonmatches
  htInitPage('"Text Matches",nil)
  null matches =>
    htInitPage(['"Lines matching search string {\em ",s,'"}"],nil)
    htSay("\vspace{2}\centerline{Sorry, but no lines match your search string}\centerline{{\em ",s,"}}\centerline{Click on the up-arrow to try again}")
    htShowPage()
  htInitPage(['"Lines matching search string {\em ",s,'"}"],nil)
  if nonmatches
    then htSayList([
           '"The lines that {\em match} your search string {\em ",
           s, '"}:"])
    else htSay('"Your search string {\em ",s,"} matches both lines:")
  htSay('"{\em \table{")
  for x in matches repeat htSayList(['"{", x, '"}"])
  htSay('"}}\vspace{1}")
  if nonmatches then
    htSay('"The line that {\em does not match} your search string:{\em \table{")
    for x in nonmatches repeat htSayList(['"{", x, '"}"])
    htSay('"}}")
  htShowPage()

mkUnixPattern s ==
  u := mkUpDownPattern s
  starPositions := REVERSE [i for i in 1..(-1 + MAXINDEX u) | u.i = $wild]
  for i in starPositions repeat
    u := STRCONC(SUBSTRING(u,0,i),'".*",SUBSTRING(u,i + 1,nil))
  if u.0 ~= $wild then u := STRCONC('"[^a-zA-Z]",u)
                  else u := SUBSTRING(u,1,nil)
  if u.(k := MAXINDEX u) ~= $wild then u := STRCONC(u,'"[^a-zA-Z]")
                                  else u := SUBSTRING(u,0,k)
  u
