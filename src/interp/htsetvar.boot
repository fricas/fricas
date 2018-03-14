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

htsv() ==
  startHTPage(50)
  htSetVars()

htSetVars() ==
  $path := nil
  $lastTree := nil
  if 0 ~= LASTATOM $setOptions then htMarkTree($setOptions,0)
  htShowSetTree($setOptions)

htShowSetTree(setTree) ==
  $path := TAKE(- LASTATOM setTree,$path)
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
  tabset2 := STRINGIMAGE (maxWidth2 + maxWidth1 - 1)
  htSayList(['"\tab{2}\newline Variable\tab{",
    STRINGIMAGE (maxWidth1 + QUOTIENT(maxWidth2, 3)),
     '"}Description\tab{",STRINGIMAGE(maxWidth2 + maxWidth1 + 2),
      '"}Value\newline\beginitems "])
  for setData in REVERSE okList repeat
      htSay '"\item"
      label := STRCONC('"\menuitemstyle{",setData.setName,'"}")
      links := [label,[['text,'"\tab{",tabset1,'"}",setData.setLabel,'"\tab{",tabset2,'"}{\em ",htShowSetTreeValue setData,'"}"]],
                'htShowSetPage, setData.setName]
      htMakePage [['bcLispLinks, links,'options,'(indent . 0)]]
  htSay '"\enditems"
  htShowPage()

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
  systemError()

mkSetTitle() == STRCONC('"Command {\em )set ",listOfStrings2String $path,'"}")

listOfStrings2String u ==
  null u => '""
  STRCONC(listOfStrings2String rest u,'" ",stringize first u)

htShowSetPage(htPage, branch) ==
  setTree := htpProperty(htPage, 'setTree)
  $path := [branch,:TAKE(- LASTATOM setTree,$path)]
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
  bcHt ['"\centerline{Set {\em ", name, '"}}\newline"]
  bcHt ['"{\em Description: } ", message, '"\newline\vspace{1} "]
  bcHt '"Select one of the following: \newline\tab{3} "
  links := [[STRCONC('"",STRINGIMAGE opt), '"\newline\tab{3}", functionToCall, opt] for opt in values]
  htMakePage [['bcLispLinks, :links]]
  bcHt ["\indent{0}\newline\vspace{1} The current setting is: {\em ",
        translateTrueFalse2YesNo EVAL variable, '"} "]
  htShowPage()

htSetLiteral(htPage, val) ==
  htInitPage('"Set Command", nil)
  SET(htpProperty(htPage, 'variable), translateYesNo2TrueFalse val)
  htKill(htPage,val)

htShowIntegerPage(htPage, setData) ==
  page := htInitPage(mkSetTitle(), htpPropertyList htPage)
  htpSetProperty(page, 'variable, setData.setVar)
  bcHt ['"\centerline{Set {\em ", setData.setName, '"}}\newline"]
  message := setData.setLabel
  bcHt ['"{\em Description: } ", message, '"\newline\vspace{1} "]
  [$htInitial,$htFinal] := setData.setLeaf
  if $htFinal = $htInitial + 1
    then
      bcHt '"Enter the integer {\em "
      bcHt stringize $htInitial
      bcHt '"} or {\em "
      bcHt stringize $htFinal
      bcHt '"}:"
    else if null $htFinal then
      bcHt '"Enter an integer greater than {\em "
      bcHt stringize ($htInitial - 1)
      bcHt '"}:"
    else
      bcHt '"Enter an integer between {\em "
      bcHt stringize $htInitial
      bcHt '"} and {\em "
      bcHt stringize $htFinal
      bcHt '"}:"
  htMakePage [
    '(domainConditions (Satisfies S chkRange)),
      ['bcStrings,[5,eval setData.setVar,'value,'S]]]
  htSetvarDoneButton('"Select to Set Value",'htSetInteger)
  htShowPage()

htSetInteger(htPage) ==
  htInitPage(mkSetTitle(), nil)
  val := chkRange htpLabelInputString(htPage,'value)
  not INTEGERP val =>
    errorPage(htPage,['"Value Error",nil,'"\vspace{3}\centerline{{\em ",val,'"}}\vspace{2}\newline\centerline{Click on \UpBitmap{} to re-enter value}"])
  SET(htpProperty(htPage, 'variable), val)
  htKill(htPage,val)

htShowFunctionPage(htPage,setData) ==
  fn := setData.setDef => FUNCALL(fn,htPage)
  htpSetProperty(htPage,'setData,setData)
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
  page := htInitPage(mkSetTitle(), htpPropertyList htPage)
  bcHt ['"\centerline{Set {\em ", setData.setName, '"}}\newline"]
  bcHt ['"{\em Description: } ", setData.setLabel, '"\newline\vspace{1} "]
  currentValue := EVAL variable
  htMakePage
    [ ['domainConditions, ['Satisfies,'S,checker]],
      ['text,:phrase],
        ['inputStrings,
          [ '"", '"", 60, currentValue, 'value, 'S]]]
  htSetvarDoneButton('"Select To Set Value",'htSetFunCommand)
  htShowPage()

htSetvarDoneButton(message, func) ==
  bcHt '"\newline\vspace{1}\centerline{"

  if message = '"Select to Set Value" or message = '"Select to Set Values"  then
    bchtMakeButton('"\lisplink",'"\ControlBitmap{ClickToSet}", func)
  else
    bchtMakeButton('"\lisplink",CONCAT('"\fbox{", message, '"}"), func)

  bcHt '"} "


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
  htInitPage('"System Command", nil)
  string := STRCONC('"{\em )set ",listOfStrings2String [value,:$path],'"}")
  htMakePage [
     '(text
        "{Here is the FriCAS system command you could have issued:}"
            "\vspace{2}\newline\centerline{\tt"),
      ['text,:string]]
  htMakePage '((text . "}\vspace{1}\newline\rm"))
  htSay '"\vspace{2}{Select \  \UpButton{} \  to go back.}"
  htSay '"\newline{Select \  \ExitButton{QuitPage} \  to remove this window.}"
  htProcessDoitButton ['"Press to Remove Page",'"",'htDoNothing]
  htShowPage()

htSetNotAvailable(htPage,whatToType) ==
  page := htInitPage('"Unavailable Set Command", htpPropertyList htPage)
  htInitPage('"Unavailable System Command", nil)
  string := STRCONC('"{\em ",whatToType,'"}")
  htMakePage [
     '(text "\vspace{1}\newline"
        "{Sorry, but this system command is not available through HyperDoc. Please directly issue this command in an FriCAS window for more information:}"
            "\vspace{2}\newline\centerline{\tt"),
      ['text,:string]]
  htMakePage '((text . "}\vspace{1}\newline"))
  htProcessDoitButton ['"Press to Remove Page",'"",'htDoNothing]
  htShowPage()

htDoNothing(htPage,command) == nil

htCheck(checker,value) ==
  PAIRP checker => htCheckList(checker,parseWord value)
  FUNCALL(checker,value)

parseWord x ==
  STRINGP x =>
    and/[DIGITP x.i for i in 0..MAXINDEX x] => PARSE_-INTEGER x
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
  bcString2WordList s in '(CONSOLE console) => 'console
  chkDirectory s

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

htMarkTree(tree,n) ==
  RPLACD(LASTTAIL tree,n)
  for branch in tree repeat
    branch.3 = 'TREE => htMarkTree(branch.5,n + 1)

htSetHistory htPage ==
  msg := "when the history facility is on (yes), results of computations are saved in memory"
  data := ['history,msg,'history,'LITERALS,'$HiFiAccess,'(on off yes no)]
  htShowLiteralsPage(htPage,data)

htSetOutputLibrary htPage ==
  htSetNotAvailable(htPage,'")set compiler output")

htSetInputLibrary htPage ==
  htSetNotAvailable(htPage,'")set compiler input")

htSetExpose htPage ==
  htSetNotAvailable(htPage,'")set expose")

htSetKernelProtect htPage ==
 htSetNotAvailable(htPage,'")set kernel protect")

htSetKernelWarn htPage ==
 htSetNotAvailable(htPage,'")set kernel warn")

htSetOutputCharacters htPage ==
  htSetNotAvailable(htPage,'")set output characters")

htSetLinkerArgs htPage ==
  htSetNotAvailable(htPage,'")set fortran calling linker")

htSetCache(htPage,:options) ==
  $path := '(functions cache)
  htPage := htInitPage(mkSetTitle(),nil)
  $valueList := nil
  htMakePage '(
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
   (doneButton "Push to enter names" htCacheAddChoice))
  htShowPage()

htCacheAddChoice htPage ==
  names := bcString2WordList htpLabelInputString(htPage,'names)
  $valueList := [listOfStrings2String names,:$valueList]
  null names => htCacheAddQuery()
  null rest names => htCacheOne names
  page := htInitPage(mkSetTitle(),nil)
  htpSetProperty(page,'names,names)
  htMakePage '(
    (domainConditions (Satisfies ALLPI chkAllPositiveInteger))
    (text
      "For each function, enter below a {\em cache length}, a positive integer. "
      "This number tells how many past values will "
      "be cached. "
      "A cache length of {\em 0} means the function won't be cached. "
      "To cache all past values, "
      "enter {\em all}."
      "\vspace{1}\newline "
      "For each function name, enter {\em all} or a positive integer:"))
  for i in 1.. for name in names repeat htMakePage [
      ['inputStrings,
        [STRCONC('"Function {\em ",name,'"} will cache"),
          '"values",5,10,htMakeLabel('"c",i),'ALLPI]]]
  htSetvarDoneButton('"Select to Set Values",'htCacheSet)
  htShowPage()

htMakeLabel(prefix,i) == INTERN STRCONC(prefix,stringize i)

htCacheSet htPage ==
  names := htpProperty(htPage,'names)
  for i in 1.. for name in names repeat
    num := chkAllNonNegativeInteger
             htpLabelInputString(htPage,htMakeLabel('"c",i))
    $cacheAlist := ADDASSOC(INTERN name,num,$cacheAlist)
  if (n := LASSOC('all,$cacheAlist)) then
    $cacheCount := n
    $cacheAlist := deleteAssoc('all,$cacheAlist)
  htInitPage('"Cache Summary",nil)
  bcHt '"In general, interpreter functions "
  bcHt
    $cacheCount = 0 => "will {\em not} be cached."
    bcHt '"cache "
    htAllOrNum $cacheCount
    '"} values."
  bcHt '"\vspace{1}\newline "
  if $cacheAlist then
--    bcHt '" However, \indent{3}"
    for [name,:val] in $cacheAlist | val ~= $cacheCount repeat
      bcHt '"\newline function {\em "
      bcHt stringize name
      bcHt '"} will cache "
      htAllOrNum val
      bcHt '"} values"
  htProcessDoitButton ['"Press to Remove Page",'"",'htDoNothing]
  htShowPage()

htAllOrNum val == bcHt
  val = 'all => '"{\em all"
  val = 0 => '"{\em no"
  STRCONC('"the last {\em ",stringize val)

htCacheOne names ==
  page := htInitPage(mkSetTitle(),nil)
  htpSetProperty(page,'names,names)
  htMakePage '(
    (domainConditions (Satisfies ALLPI chkAllPositiveInteger))
    (text
      "Enter below a {\em cache length}, a positive integer. "
      "This number tells how many past values will "
      "be cached. To cache all past values, "
      "enter {\em all}."
      "\vspace{1}\newline ")
    (inputStrings
      ("Enter {\em all} or a positive integer:"
       "" 5 10 c1 ALLPI)))
  htSetvarDoneButton('"Select to Set Value",'htCacheSet)
  htShowPage()
