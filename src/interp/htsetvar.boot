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

downlink page ==
  htInitPage('"Bridge",nil)
  htSayList(['"\replacepage{", page, '"}"])
  htShowPage()

dbNonEmptyPattern pattern ==
  null pattern => '"*"
  pattern := STRINGIMAGE pattern
  #pattern > 0 => pattern
  '"*"

htSystemVariables() ==
    not $fullScreenSysVars => htSetVars()
    classlevel := $UserLevel
    $levels : local := '(compiler development interpreter)
    $heading  : local := nil
    while classlevel ~= first $levels repeat $levels := rest $levels
    table := NREVERSE(hsv_fn($setOptions, nil, true))
    page := htInitPage('"System Variables", nil)
    ht_add_string(page, '"\beginmenu")
    lastHeading := nil
    for [heading,name,message,.,key,variable,options,func] in table repeat
      ht_add_string(page, '"\newline\item ")
      if heading = lastHeading then ht_add_string(page, '"\tab{8}") else
          ht_add_strings(page, [PNAME(heading), '"\tab{8}"])
          lastHeading := heading
      ht_add_strings(page, ['"{\em ", PNAME(name), "}\tab{22}", message])
      ht_add_string(page, '"\tab{80}")
      key = 'FUNCTION =>
         null options => ht_add_to_page(page,
                                     [['bcLinks, ['"reset", '"", func]]])
         [msg,class,var,valuesOrFunction,:.] := first options  --skip first message
         functionTail(page, name, class, var, valuesOrFunction)
         for option in rest options repeat
           option is ['break,:.] => 'skip
           [msg,class,var,valuesOrFunction,:.] := option
           ht_add_strings(page, ['"\newline\tab{22}", msg,'"\tab{80}"])
           functionTail(page, name, class, var, valuesOrFunction)
      val := eval variable
      displayOptions(page, name, key, variable, val, options)
    ht_add_string(page, '"\endmenu")
    htShowPage1(page)

functionTail(page, name, class, var, valuesOrFunction) ==
    val := eval var
    atom valuesOrFunction =>
        ht_add_to_page(page, '((domainConditions (isDomain STR (String)))))
        ht_add_to_page(page, [['bcLinks,
            ['"reset", '"", 'htSetSystemVariableKind, [var, name, nil]]]])
        ht_add_to_page(page, [['bcStrings,
            [30, STRINGIMAGE(val), name, valuesOrFunction]]])
    displayOptions(page, name, class, var, val, valuesOrFunction)

displayOptions(page, name, class, variable, val, options) ==
    class = 'INTEGER =>
        opt1_str :=
            options.1 => STRINGIMAGE(options.1)
            ""
        ht_add_to_page(page, [['bcLispLinks,
            [STRCONC(STRINGIMAGE(options.0), '"-", opt1_str), '"",
             'htSetSystemVariableKind, [variable, name, 'PARSE_-INTEGER]]]])
        ht_add_to_page(page, '((domainConditions (isDomain INT (Integer)))))
        ht_add_to_page(page, [['bcStrings, [5, STRINGIMAGE(val), name, 'INT]]])
    class = 'STRING =>
        ht_add_strings(page, ['"{\em ", val, '"}\space{1}"])
    for x in options repeat
      val = x or val = true and x = 'on or null val and x = 'off =>
            ht_add_strings(page, ['"{\em ", STRINGIMAGE(x), '"}\space{1}"])
      ht_add_to_page(page, [['bcLispLinks,
                    [x, '" ", 'htSetSystemVariable, [variable, x]]]])

hsv_fn(lt, al, firstTime) ==
    for t in lt repeat
        if firstTime then $heading := opOf(t)
        al := hsv_gn(t, al)
    al

hsv_gn(t, al) ==
    [.,.,class,key,.,options,:.] := t
    not MEMQ(class,$levels) => al
    key = 'LITERALS or key = 'INTEGER or key = 'STRING => [[$heading,:t],:al]
    key = 'TREE => hsv_fn(options, al, false)
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
  defstream := MAKE_INSTREAM(STRCONC($spadroot,
                                     '"/algebra/glossdef.text"))
  lines := gatherGlossLines(results,defstream)
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
    n := charPosition($tick,keyline,0)
    keyAndTick := SUBSTRING(keyline,0,n + 1)
    byteAddress := string2Integer SUBSTRING(keyline,n + 1,nil)
    FILE_-POSITION(defstream,byteAddress)
    line := read_line defstream
    k := charPosition($tick,line,1)
    pointer := SUBSTRING(line,0,k)
    def := SUBSTRING(line,k + 1,nil)
    xtralines := nil
    while (x := read_line defstream) and
      (j := charPosition($tick,x,1)) and (nextPointer := SUBSTRING(x,0,j))
        and (nextPointer = pointer) repeat
          xtralines := [SUBSTRING(x,j + 1,nil),:xtralines]
    acc := [STRCONC(keyAndTick,def, "STRCONC"/NREVERSE xtralines),:acc]
  REVERSE acc

htGlossSearch(htPage,junk) ==  htGloss htpLabelInputString(htPage,'filter)


htSetVars() ==
  $path := nil
  $lastTree := nil
  htShowSetTree($setOptions)

htShowSetTree(setTree) ==
  page := htInitPage(mkSetTitle(),nil)
  htpSetProperty(page, 'setTree, setTree)
  links := nil
  maxWidth1 := maxWidth2 := 0
  for setData in setTree repeat
    satisfiesUserLevel setData.setLevel =>
      okList := [setData,:okList]
      maxWidth1 := MAX(# PNAME setData.setName,maxWidth1)
      maxWidth2 := MAX(htShowCount STRINGIMAGE setData.setLabel,maxWidth2)
  maxWidth1 := MAX(9,maxWidth1)
  maxWidth2 := MAX(41,maxWidth2)
  tabset1 := STRINGIMAGE (maxWidth1)
  tabset2 := STRINGIMAGE (maxWidth2 + maxWidth1 + 1)
  ht_add_strings(page, ['"\tab{2}\newline Variable\tab{",
    STRINGIMAGE (maxWidth1 + quotient_INT(maxWidth2, 3)),
     '"}Description\tab{",STRINGIMAGE(maxWidth2 + maxWidth1 + 2),
      '"}Value\newline\beginitems "])
  for setData in REVERSE okList repeat
      ht_add_string(page, '"\item")
      label := STRCONC('"\menuitemstyle{",setData.setName,'"}")
      vv := htShowSetTreeValue(setData)
      if vv = '"" then
          vv := '"????"
      links := [label,[['text,'"\tab{",tabset1,'"}",setData.setLabel,'"\tab{",tabset2,'"}{\em ", vv, '"}"]],
                'htShowSetPage, setData.setName]
      ht_add_to_page(page, [['bcLispLinks, links, 'options, '(indent . 0)]])
  ht_add_string(page, '"\enditems")
  htShowPage1(page)

htShowCount s == --# discounting {\em .. }
  m := #s
  m < 8 => m - 1
  i := 0
  count := 0
  while i < m - 7 repeat
    s.i = char '_{ and  s.(i+1) = char '_\ and s.(i+2) = char 'e
      and s.(i+3) = char 'm => i := i + 6     --discount {\em }
    i := i + 1
    count := count + 1
  count + (m - i)

htShowSetTreeValue(setData) ==
  st := setData.setType
  st = 'FUNCTION => object2String FUNCALL(setData.setVar,"%display%")
  st = 'INTEGER  => object2String eval setData.setVar
  st = 'STRING  => object2String eval setData.setVar
  st = 'LITERALS =>
    object2String translateTrueFalse2YesNo eval setData.setVar
  st = 'TREE     => '"..."
  systemError([])

mkSetTitle() == STRCONC('"Command {\em )set ",listOfStrings2String $path,'"}")

listOfStrings2String u ==
  null u => '""
  STRCONC(listOfStrings2String rest u,'" ",stringize first u)

htShowSetPage(htPage, branch) ==
  setTree := htpProperty(htPage, 'setTree)
  $path := [branch]
  setData := assoc(branch, setTree)
  null setData =>
    systemError('"No Set Data")
  st := setData.setType
  st = 'FUNCTION => htShowFunctionPage(htPage, setData)
  st = 'INTEGER  =>  htShowIntegerPage(htPage,setData)
  st = 'LITERALS => htShowLiteralsPage(htPage, setData)
  st = 'TREE     => htShowSetTree(setData.setLeaf)

  st = 'STRING   =>  -- have to add this
     htSetNotAvailable(htPage,'")set compiler")

  systemError '"Unknown data type"

htShowLiteralsPage(htPage, setData) ==
  htSetLiterals(htPage,setData.setName,setData.setLabel,
                setData.setVar,setData.setLeaf,'htSetLiteral)

htSetLiterals(htPage,name,message,variable,values,functionToCall) ==
  page := htInitPage('"Set Command", htpPropertyList htPage)
  htpSetProperty(page, 'variable, variable)
  htSetLiterals2(page, name, message, EVAL(variable), values, functionToCall)

ht_set_description(page, name, message) ==
    ht_add_strings(page, ['"\centerline{Set {\em ", PNAME(name),
                          '"}}\newline"])
    ht_add_strings(page, ['"{\em Description: } ", message,
                          '"\newline\vspace{1} "])

htSetLiterals2(page, name, message, cval, values, functionToCall) ==
  ht_set_description(page, name, message)
  ht_add_string(page, '"Select one of the following: \newline\tab{3} ")
  links := [[STRCONC('"",STRINGIMAGE opt), '"\newline\tab{3}", functionToCall, opt] for opt in values]
  ht_add_to_page(page, [['bcLispLinks, :links]])
  bcHt ['"\indent{0}\newline\vspace{1} The current setting is: {\em ",
        translateTrueFalse2YesNo(cval), '"} "]
  htShowPage1(page)

htSetLiteral(htPage, val) ==
  htInitPage('"Set Command", nil)
  SET(htpProperty(htPage, 'variable), translateYesNo2TrueFalse val)
  htKill(htPage,val)

htShowIntegerPage(htPage, setData) ==
  page := htInitPage(mkSetTitle(), htpPropertyList htPage)
  htpSetProperty(page, 'variable, setData.setVar)
  ht_set_description(page, setData.setName, setData.setLabel)
  [$htInitial,$htFinal] := setData.setLeaf
  if $htFinal = $htInitial + 1 then
      ht_add_strings(page, ['"Enter the integer {\em ", stringize($htInitial),
                            '"} or {\em ", stringize($htFinal), '"}:"])
  else if null $htFinal then
      ht_add_strings(page, ['"Enter an integer greater than {\em ",
                            stringize($htInitial - 1), '"}:"])
  else
      ht_add_strings(page, ['"Enter an integer between {\em ",
         stringize($htInitial), '"} and {\em ", stringize($htFinal), '"}:"])
  ht_add_to_page(page, [
    '(domainConditions (Satisfies S chkRange)),
      ['bcStrings, [5, eval(setData.setVar), 'value, 'S]]])
  htMakeDoneButton('"Select to Set Value", 'htSetInteger)
  htShowPage1(page)

htSetInteger(htPage) ==
  htInitPage(mkSetTitle(), nil)
  val := chkRange htpLabelInputString(htPage,'value)
  not INTEGERP val =>
    errorPage(htPage,['"Value Error",nil,'"\vspace{3}\centerline{{\em ",val,'"}}\vspace{2}\newline\centerline{Click on \UpBitmap{} to re-enter value}"])
  SET(htpProperty(htPage, 'variable), val)
  htKill(htPage,val)

htShowFunctionPage(htPage,setData) ==
  htpSetProperty(htPage, 'setData, setData)
  fn := setData.setDef => FUNCALL(fn,htPage)
  htpSetProperty(htPage,'parts, setData.setLeaf)
  htShowFunctionPageContinued(htPage)

htShowFunctionPageContinued(htPage) ==
  parts := htpProperty(htPage,'parts)
  setData := htpProperty(htPage,'setData)
  [[phrase,kind,variable,checker,initValue,:.],:restParts] := parts
  htpSetProperty(htPage, 'variable, variable)
  htpSetProperty(htPage, 'checker, checker)
  htpSetProperty(htPage, 'parts, restParts)
  kind = 'LITERALS => htSetLiterals(htPage,setData.setName,
                                    phrase,variable,checker,'htFunctionSetLiteral)
  htShowFunctionPageContinued2(htPage, setData, EVAL(variable), checker,
                               phrase, 'htSetFunCommand)

htShowFunctionPageContinued2(htPage, setData, cval, checker, phrase,
                             fun_to_call) ==
  page := htInitPage(mkSetTitle(), htpPropertyList htPage)
  ht_set_description(page, setData.setName, setData.setLabel)
  ht_add_to_page(page,
    [ ['domainConditions, ['Satisfies,'S,checker]],
      ['text,:phrase],
        ['inputStrings,
          [ '"", '"", 60, cval, 'value, 'S]]])
  htMakeDoneButton('"Select To Set Value", fun_to_call)
  htShowPage1(page)

htFunctionSetLiteral(htPage, val) ==
  htInitPage('"Set Command", nil)
  SET(htpProperty(htPage, 'variable), translateYesNo2TrueFalse val)
  htSetFunCommandContinue(htPage,val)

htSetFunCommand(htPage) ==
  variable := htpProperty(htPage,'variable)
  checker := htpProperty(htPage,'checker)
  value := htCheck(checker,htpLabelInputString(htPage,'value))
  SET(variable,value) --kill this later
  htSetFunCommandContinue(htPage,value)

htSetFunCommandContinue(htPage,value) ==
  parts := htpProperty(htPage,'parts)
  continue :=
    null parts => false
    parts is [['break,predicate],:restParts] => eval predicate
    true
  continue =>
    htpSetProperty(htPage,'parts,restParts)
    htShowFunctionPageContinued(htPage)
  htKill(htPage,value)

htKill(htPage,value) ==
  page := htInitPage('"System Command", nil)
  string := STRCONC('"{\em )set ",listOfStrings2String [value,:$path],'"}")
  ht_add_to_page(page, [
     '(text
        "{Here is the FriCAS system command you could have issued:}"
            "\vspace{2}\newline\centerline{\tt"),
      ['text,:string]])
  ht_add_to_page(page, '((text . "}\vspace{1}\newline\rm")))
  ht_add_string(page, '"\vspace{2}{Select \  \UpButton{} \  to go back.}")
  ht_add_string(page,
      '"\newline{Select \  \ExitButton{QuitPage} \  to remove this window.}")
  htShowPage1(page)

htSetNotAvailable(htPage,whatToType) ==
  page := htInitPage('"Unavailable system command", nil)
  string := STRCONC('"{\em ",whatToType,'"}")
  ht_add_to_page(page, [
     '(text "\vspace{1}\newline"
        "{Sorry, but this system command is not available through HyperDoc. Please directly issue this command in a FriCAS window for more information:}"
            "\vspace{2}\newline\centerline{\tt"),
      ['text,:string]])
  ht_add_to_page(page, '((text . "}\vspace{1}\newline")))
  htShowPage1(page)

htDoNothing(htPage,command) == nil

htCheck(checker,value) ==
  PAIRP checker => htCheckList(checker,parseWord value)
  FUNCALL(checker,value)

parseWord x ==
  STRINGP x =>
    and/[char_to_digit(x.i) for i in 0..MAXINDEX(x)] => PARSE_-INTEGER(x)
    INTERN x
  x

htCheckList(checker,value) ==
  if value in '(y ye yes Y YE YES) then value := 'yes
  if value in '(n no N NO) then value := 'no
  checker is [n,m] and INTEGERP n =>
    m = n + 1 =>
      value in checker => value
      n
    null m =>
      INTEGERP value and value >= n => value
      n
    INTEGERP m =>
      INTEGERP value and value >= n and value <= m => value
      n
  value in checker => value
  first checker
--  emlist := "STRCONC"/[STRCONC('" {\em ",PNAME x,'"} ") for x in checker]
--  STRCONC('"Please enter one of: ",emlist)

translateYesNoToTrueFalse x ==
  x = 'yes => true
  x = 'no => false
  x

chkNameList x ==
  u := bcString2ListWords x
  parsedNames := [ncParseFromString x for x in u]
  and/[IDENTP x for x in parsedNames] => parsedNames
  '"Please enter a list of identifiers separated by blanks"

chkPosInteger s ==
  (u := parseOnly s) and INTEGERP u and u > 0 => u
  '"Please enter a positive integer"

chkOutputFileName s ==
    u := find_symbol(s)
    first(u) = 0 and rest(u) = 'console => 'console
    s

chkDirectory s == s

chkNonNegativeInteger s ==
  (u := ncParseFromString s) and INTEGERP u and u >= 0 => u
  '"Please enter a non-negative integer"

chkRange s ==
  (u := ncParseFromString s) and INTEGERP u
    and u >= $htInitial and (NULL $htFinal or u <= $htFinal)
      => u
  null $htFinal =>
    STRCONC('"Please enter an integer greater than ",stringize ($htInitial - 1))
  STRCONC('"Please enter an integer between ",stringize $htInitial,'" and ",
            stringize $htFinal)

chkAllNonNegativeInteger s ==
  (u := ncParseFromString s) and u in '(a al all A AL ALL) and 'ALL
    or chkNonNegativeInteger s
       or '"Please enter {\em all} or a non-negative integer"

htMakePathKey path ==
  null path => systemError '"path is not set"
  INTERN fn(PNAME first path,rest path) where
    fn(a,b) ==
      null b => a
      fn(STRCONC(a,'".",PNAME first b),rest b)

htSetHistory htPage ==
  msg := '"when the history facility is on (yes), results of computations are saved in memory"
  data := ['history,msg,'history,'LITERALS,'$HiFiAccess,'(on off yes no)]
  htShowLiteralsPage(htPage,data)

htSetOutputLibrary htPage ==
  htSetNotAvailable(htPage,'")set compiler output")

htSetInputLibrary htPage ==
  htSetNotAvailable(htPage,'")set compiler input")

htSetExpose htPage ==
  htSetNotAvailable(htPage,'")set expose")

htSetOutputCharacters htPage ==
  htSetNotAvailable(htPage,'")set output characters")

htSetOutputPage(page) ==
    setData := htpProperty(page, 'setData)
    branch := first(setData)
    rec := get_out_rec(branch)
    htpSetProperty(page, 'output_rec, rec)
    page := htInitPage('"Set Command", htpPropertyList(page))
    htSetLiterals2(page, setData.setName, setData.setLabel,
                   (rec.$on_off => 'on; 'off), '(on off),
                   'htSetOutputPage2)

htSetOutputPage2(page, val) ==
    rec := htpProperty(page, 'output_rec)
    rec.$on_off := translateYesNo2TrueFalse(val)
    rec.$on_off =>
        setData := htpProperty(page, 'setData)
        branch := first(setData)
        phrase := CONCAT('"where ", STRINGIMAGE(branch),
                    '" printing goes (enter {\em console} or a pathname)?")
        checker := 'chkOutputFileName
        htpSetProperty(page, 'checker, checker)
        htShowFunctionPageContinued2(page, setData, rec.$file_off,
                                     checker, phrase,
                                     'htSetOutputPage3)
    htKill(page, val)

htSetOutputPage3(page) ==
    setData := htpProperty(page, 'setData)
    checker := htpProperty(page, 'checker)
    val := htCheck(checker, htpLabelInputString(page, 'value))
    FUNCALL(setData.setVar, [val])
    htKill(page, val)

htSetCache(htPage) ==
  $path := '(cache functions)
  page := htInitPage(mkSetTitle(), nil)
  $valueList := nil
  ht_add_to_page(page, '(
   (text
    "Use this system command to cause the FriCAS interpreter to `remember' "
    "past values of interpreter functions. "
    "To remember a past value of a function, the interpreter "
    "sets up a {\em cache} for that function based on argument values. "
    "When a value is cached for a given argument value, its value is gotten "
    "from the cache and not recomputed. Caching can often save much "
    "computing time, particularly with recursive functions or functions that "
    "are expensive to compute and that are called repeatedly "
    "with the same argument."
    "\vspace{1}\newline ")
   (domainConditions (Satisfies S chkNameList))
   (text
      "Enter below a list of interpreter functions you would like specially cached. "
      "Use the name {\em all} to give a default setting for all "
      "interpreter functions. "
      "\vspace{1}\newline "
      "Enter {\em all} or a list of names (separate names by blanks):")
   (inputStrings ("" "" 60 "all" names S))
   (doneButton "Push to enter names" htCacheAddChoice)))
  htShowPage1(page)

htCacheAddChoice htPage ==
  names := bcString2WordList htpLabelInputString(htPage,'names)
  $valueList := [listOfStrings2String names,:$valueList]
  null names => htCacheAddQuery()
  null rest names => htCacheOne names
  page := htInitPage(mkSetTitle(),nil)
  htpSetProperty(page,'names,names)
  ht_add_to_page(page, '(
    (domainConditions (Satisfies ALLPI chkAllPositiveInteger))
    (text
      "For each function, enter below a {\em cache length}, a positive integer. "
      "This number tells how many past values will "
      "be cached. "
      "A cache length of {\em 0} means the function won't be cached. "
      "To cache all past values, "
      "enter {\em all}."
      "\vspace{1}\newline "
      "For each function name, enter {\em all} or a positive integer:")))
  for i in 1.. for name in names repeat ht_add_to_page(page, [
      ['inputStrings,
        [STRCONC('"Function {\em ", name, '"} will cache"),
          '"values", 5, 10, htMakeLabel('"c", i), 'ALLPI]]])
  htMakeDoneButton('"Select to Set Values", 'htCacheSet)
  htShowPage1(page)

htMakeLabel(prefix,i) == INTERN STRCONC(prefix,stringize i)

htCacheSet htPage ==
  names := htpProperty(htPage,'names)
  for i in 1.. for name in names repeat
    num := chkAllNonNegativeInteger
             htpLabelInputString(htPage,htMakeLabel('"c",i))
    $cacheAlist := assoc_add(INTERN(name), num, $cacheAlist)
  if (n := LASSOC('all,$cacheAlist)) then
    $cacheCount := n
    $cacheAlist := deleteAssoc('all,$cacheAlist)
  page := htInitPage('"Cache Summary", nil)
  ht_add_string(page, '"In general, interpreter functions ")
  if $cacheCount = 0 then
      ht_add_string(page, '"will {\em not} be cached.")
  else
      ht_add_string(page, '"cache ");
      htAllOrNum(page, $cacheCount);
      ht_add_string(page, '"} values.")
  ht_add_string(page, '"\vspace{1}\newline ")
  if $cacheAlist then
      for [name, :val] in $cacheAlist | val ~= $cacheCount repeat
          ht_add_strings(page, ['"\newline function {\em ", stringize(name),
                                '"} will cache "])
          htAllOrNum(page, val)
          ht_add_string(page, '"} values")
  htShowPage1(page)

htAllOrNum(page, val) ==
    str :=
        val = 'all => '"{\em all"
        val = 0 => '"{\em no"
        STRCONC('"the last {\em ",stringize val)
    ht_add_string(page, str)

htCacheOne names ==
  page := htInitPage(mkSetTitle(),nil)
  htpSetProperty(page,'names,names)
  ht_add_to_page(page, '(
    (domainConditions (Satisfies ALLPI chkAllPositiveInteger))
    (text
      "Enter below a {\em cache length}, a positive integer. "
      "This number tells how many past values will "
      "be cached. To cache all past values, "
      "enter {\em all}."
      "\vspace{1}\newline ")
    (inputStrings
      ("Enter {\em all} or a positive integer:"
       "" 5 10 c1 ALLPI))))
  htMakeDoneButton('"Select to Set Value", 'htCacheSet)
  htShowPage1(page)
