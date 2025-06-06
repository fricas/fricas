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

)if false
\section{Top level function calling conventions}
Conventions:
\begin{list}{}
\item when called with argument "\%initialize", a function will
set the appropriate variables to their default states.
\item when called with argument "\%display\%", a function will return a
current state information suitable for sayBrightly
\item when called with argument "\%describe\%", a function will print
a description of itself and any conditions it imposes.
\item otherwise, a function may interpret its arguments as it sees
appropriate.
\end{list}
Also by convention each top level function named in the FUNCTION
slot (see the data structure in setvart.boot\cite{1}) has an
associated describe function. Thus, for example,
setOutputFortran is accompanied by function to describe
its arguments, such as describeSetOutputFortran.
)endif
initializeSetVariables (setTree) ==
  -- this function passes through the table of set variable information
  -- and initializes the variables to their default definitions.
  for setData in setTree repeat
    st := setData.setType
    st = 'FUNCTION =>
      -- here setVar is really the name of a function to call
      if functionp(setData.setVar)
        then FUNCALL( setData.setVar,"%initialize%")
        else sayMSG '"   Function not implemented."
    st = 'INTEGER  =>
      SET(setData.setVar, setData.setDef)
    st = 'STRING  =>
      SET(setData.setVar, setData.setDef)
    st = 'LITERALS =>
      SET(setData.setVar, translateYesNo2TrueFalse setData.setDef)
    st = 'TREE =>
      initializeSetVariables(setData.setLeaf)

resetWorkspaceVariables () ==
  -- this replaces def in DEBUG LISP
  -- this function resets many workspace variables to their default
  -- values. Some things are reset by start and not reset by restart.
  SETQ($count_list                  , NIL)
  $edit_file := nil
  SETQ($timer_list                  , NIL)
  SETQ($CommandSynonymAlist         , COPY($InitialCommandSynonymAlist))
  SETQ($IOindex                     , 1  )
  SETQ($e                           , [[NIL]])
  SETQ($env                         , [[NIL]])

  -- many variables set by the following

  initializeSetVariables($setOptions)

translateYesNo2TrueFalse x ==
  x in '(yes on) => true
  x in '(no off)  => false
  x

translateTrueFalse2YesNo x ==
  x = true  => 'on
  x = false => 'off
  x

set l ==
  ioHook("startSysCmd", "set")
  UNWIND_-PROTECT(set1(l, $setOptions), ioHook("endSysCmd", "set"))

set1(l,setTree) ==
  null l => displaySetVariableSettings(setTree,"")
  $setOptionNames : local := [x.0 for x in setTree]
  arg := selectOption(DOWNCASE first l, $setOptionNames, 'optionError)
  setData := [arg,:LASSOC(arg,setTree)]

  -- check is the user is authorized for the set variable
  null satisfiesUserLevel setData.setLevel =>
    sayKeyedMsg("S2IZ0007",[$UserLevel,'"set option"])

  1 = #l => displaySetOptionInformation(arg,setData)
  st := setData.setType

  st = 'FUNCTION =>
    -- allow the user to set the default
    setfunarg :=
      l.1 = 'DEFAULT => "%initialize%"
      IFCDR l
    if functionp(setData.setVar)
      then FUNCALL( setData.setVar,setfunarg)
      else sayMSG '"   Function not implemented."
    -- if so set, then show option information
    if $displaySetValue then displaySetOptionInformation(arg,setData)
    NIL

  st = 'STRING   =>
    arg2 := l.1
    if arg2 = 'DEFAULT
      then SET(setData.setVar, setData.setDef)
      else if arg2 then SET(setData.setVar, arg2)
    -- if so set or not a valid choice, then show option information
    if $displaySetValue or (null arg2) then
      displaySetOptionInformation(arg,setData)
    NIL

  st = 'INTEGER  =>
    -- validate the option, allowing the user to set the default
    arg2 :=
      num := l.1
      (FIXP num) and (num >= (setData.setLeaf).0) and
        (null (upperlimit := setData.setLeaf.1) or num <= upperlimit) => num
      selectOption(l.1,['default,:setData.setLeaf],nil)
    if arg2 = 'DEFAULT
      then SET(setData.setVar, setData.setDef)
      else if arg2 then SET(setData.setVar, arg2)
    -- if so set or not a valid choice, then show option information
    if $displaySetValue or (null arg2) then
      displaySetOptionInformation(arg,setData)
    null arg2 => sayMessage ['" Your value",:bright object2String l.1,
        '"is not among the valid choices."]
    NIL

  st = 'LITERALS =>
    -- validate the option, allowing the user to set the default
    if (arg2 := selectOption(l.1,['default,:setData.setLeaf],nil)) then
      if arg2 = 'DEFAULT
        then SET(setData.setVar, translateYesNo2TrueFalse setData.setDef)
        else
          SET(setData.setVar, translateYesNo2TrueFalse arg2)
    -- if so set or not a valid choice, then show option information
    if $displaySetValue or (null arg2) then
      displaySetOptionInformation(arg,setData)
    null arg2 => sayMessage ['" Your value",:bright object2String l.1,
        '"is not among the valid choices."]
    NIL

  -- for a sub-tree, we must recurse
  st = 'TREE =>
    set1(IFCDR l, setData.setLeaf)
    NIL
  sayMessage ['"Cannot handle set tree node type",:bright st,"yet"]
  NIL

displaySetOptionInformation(arg,setData) ==
  st := setData.setType
  -- if the option is a sub-tree, show the full menu
  st = 'TREE =>
    displaySetVariableSettings(setData.setLeaf,setData.setName)

  -- otherwise we want to show the current setting
  centerAndHighlight (STRCONC('"The ",object2String arg,'" Option"),
                      $LINELENGTH,specialChar 'hbar)
  sayBrightly ['%l,:bright '"Description:",setData.setLabel]

  st = 'FUNCTION =>
    TERPRI()
    if functionp(setData.setVar)
      then FUNCALL(setData.setVar,"%describe%")
      else sayMSG '"   Function not implemented."

  st = 'INTEGER  =>
    sayMessage ['" The",:bright arg,'"option",
      '" may be followed by an integer in the range",
       :bright (setData.setLeaf).0,'"to",'%l,
        :bright (setData.setLeaf).1,'"inclusive.",
         '" The current setting is",:bright eval setData.setVar]

  st = 'STRING  =>
    sayMessage ['" The",:bright arg,'"option",
      '" is followed by a string enclosed in double quote marks.", '%l,
         '" The current setting is",:bright ["_"",eval setData.setVar, "_""]]

  st = 'LITERALS =>
    sayMessage ['" The",:bright arg,'"option",
      '" may be followed by any one of the following:"]
    current := translateTrueFalse2YesNo eval setData.setVar
    for name in setData.setLeaf repeat
      if name = current
        then sayBrightly ['" ->",:bright object2String name]
        else sayBrightly ['"    ",object2String name]
    sayMessage '" The current setting is indicated within the list."
    if (setData.setLeaf = '(yes no on off)) or
      (setData.setLeaf = '(yes no on off long)) then
       sayMessage [:bright '"yes",'"and",:bright '"no",
        '"have the same effect as",:bright '"on",'"and",:bright '"off",
          '"respectively."]

displaySetVariableSettings(setTree,label) ==
  if label = "" then label := '")set"
    else label := STRCONC('" ",object2String label,'" ")
  centerAndHighlight(STRCONC('"Current Values of ",label,
    '" Variables"),$LINELENGTH," ")
  TERPRI()
  sayBrightly ['"Variable     ",
               '"Description                                ",
                 '"Current Value"]
  SAY filler_chars($LINELENGTH, hbar_special_char())
  subtree := nil
  for setData in setTree repeat
    null satisfiesUserLevel setData.setLevel => nil
    setOption := object2String setData.setName
    setOption := STRCONC(setOption, filler_spaces(13 - #setOption),
                         setData.setLabel)
    setOption := STRCONC(setOption, filler_spaces(55 - #setOption))
    st := setData.setType
    st = 'FUNCTION =>
      opt :=
        functionp(setData.setVar) => FUNCALL( setData.setVar,"%display%")
        '"unimplemented"
      if PAIRP opt then opt := [:[o,'" "] for o in opt]
      sayBrightly concat(setOption,'%b,opt,'%d)
    st = 'STRING   =>
      opt := object2String eval setData.setVar
      sayBrightly [setOption,:bright opt]
    st = 'INTEGER  =>
      opt := object2String eval setData.setVar
      sayBrightly [setOption,:bright opt]
    st = 'LITERALS =>
      opt := object2String translateTrueFalse2YesNo eval setData.setVar
      sayBrightly [setOption,:bright opt]
    st = 'TREE     =>
      sayBrightly [setOption,:bright '"..."]
      subtree := true
      subname := object2String setData.setName
  TERPRI()
  subtree =>
    sayBrightly ['"Variables with current values of",:bright '"...",
      '"have further sub-options. For example,"]
    sayBrightly ['"issue",:bright '")set ",subname,
      '" to see what the options are for",:bright subname,'".",'%l,
        '"For more information, issue",:bright '")help set",'"."]

setAsharpArgs arg ==
  arg = "%initialize%" =>
    $asharpCmdlineFlags := '"-O -Fasy -Fao -Flsp -lfricas -Mno-ALDOR__W__WillObsolete -DFriCAS -Y $FRICAS/algebra -I $FRICAS/algebra"
  arg = "%display%" =>
    $asharpCmdlineFlags
  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeAsharpArgs()
  $asharpCmdlineFlags := first(arg)

describeAsharpArgs() ==
  sayBrightly LIST (
   '%b,'")set compiler args ",'%d,_
   '"is used to tell FriCAS how to invoke the library compiler ",'%l,_
   '" when compiling code for FriCAS.",'%l,_
   '" The args option is followed by a string enclosed in double quotes.",'%l,'%l,_
   '" The current setting is",'%l,'%b,'"_"",$asharpCmdlineFlags,'"_"",'%d)

setInputLibrary arg ==
  arg = "%initialize%" =>
   true
  nil

-- FIXME: Is this used ??
setOutputLibrary arg == false

describeOutputLibraryArgs() ==
  sayBrightly LIST (
   '%b,'")set compiler output library",'%d,_
   '"is used to tell the compiler where to place", '%l,_
   '"compiled code generated by the library compiler.  By default it goes",'%l,_
   '"in a file called",'%b, '"user.lib", '%d, '"in the current directory."
    )

describeInputLibraryArgs() ==
  sayBrightly LIST (
   '%b,'")set compiler input add library",'%d,_
   '"is used to tell FriCAS to add", '%b, '"library", '%d, '"to",'%l,
   '"the front of the path which determines where compiled code is loaded from.",_
   '%l, '%b,'")set compiler input drop library",'%d,_
   '"is used to tell FriCAS to remove", '%b, '"library", '%d, '%l,_
   '"from this path."
    )

setExpose arg ==
  arg = "%initialize%" => loadExposureGroupData()
  arg = "%display%" => '"..."

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    --  give msg about exposure groups
    displayExposedGroups()
    --  give msg about explicitly exposed constructors
    sayMSG '" "
    displayExposedConstructors()
    --  give msg about explicitly hidden constructors
    sayMSG '" "
    displayHiddenConstructors()
    -- give some more details
    sayMSG '" "
    sayKeyedMsg("S2IZ0049D", ['"exposed"])

  arg is [fn,:fnargs] and (fn := selectOptionLC(fn,
    '(add drop initialize),NIL)) =>
      fn = 'add  =>  setExposeAdd fnargs
      fn = 'drop =>  setExposeDrop fnargs
      fn = 'initialize => setExpose "%initialize%"
      NIL
  setExpose NIL

setExposeAdd arg ==
  (null arg) =>
    centerAndHighlight ('"The add Option",$LINELENGTH,specialChar 'hbar)
    --  give msg about exposure groups
    displayExposedGroups()
    --  give msg about explicitly exposed constructors
    sayMSG '" "
    displayExposedConstructors()
    sayMSG '" "
    sayKeyedMsg("S2IZ0049E",NIL)
  arg is [fn,:fnargs] and (fn := selectOptionLC(fn,
    '(group constructor),NIL)) =>
      fn = 'group  =>  setExposeAddGroup fnargs
      fn = 'constructor =>  setExposeAddConstr fnargs
      NIL
  setExposeAdd NIL

setExposeAddGroup arg ==
  (null arg) =>
    centerAndHighlight('"The group Option",$LINELENGTH,specialChar 'hbar)
    --  give msg about exposure groups
    displayExposedGroups()
    sayMSG '" "
    sayKeyedMsg("S2IZ0049G", ['"exposed"])
    sayMSG '" "
    sayAsManyPerLineAsPossible [object2String first x for x in
      $globalExposureGroupAlist]
  for x in arg repeat
    if PAIRP x then x := QCAR x
    x = 'all =>
      $localExposureData.0 :=[first x for x in $globalExposureGroupAlist]
      $localExposureData.1 :=NIL
      $localExposureData.2 :=NIL
      displayExposedGroups()
      sayMSG '" "
      displayExposedConstructors()
      sayMSG '" "
      displayHiddenConstructors()
      clearClams()
    null GETALIST($globalExposureGroupAlist,x) =>
      sayKeyedMsg("S2IZ0049H",[x])
    member(x,$localExposureData.0) =>
      sayKeyedMsg("S2IZ0049I",[x,$interpreterFrameName])
    $localExposureData.0 := MSORT cons(x,$localExposureData.0)
    sayKeyedMsg("S2IZ0049R",[x,$interpreterFrameName])
    clearClams()

setExposeAddConstr2(arg, noquiet) ==
  (null arg) =>
    centerAndHighlight ('"The constructor Option",$LINELENGTH,
      specialChar 'hbar)
    --  give msg about explicitly exposed constructors
    displayExposedConstructors()
  for x in arg repeat
    x := unabbrev x
    if PAIRP x then x := QCAR x
    -- if the constructor is known, we know what type it is
    null(get_database(x, 'CONSTRUCTORKIND)) =>
      sayKeyedMsg("S2IZ0049J",[x])
    member(x,$localExposureData.1) =>
        if noquiet then
            sayKeyedMsg("S2IZ0049K", [x, $interpreterFrameName])
    -- if the constructor is explicitly hidden, then remove that
    if member(x,$localExposureData.2) then
      $localExposureData.2 := delete(x,$localExposureData.2)
    $localExposureData.1 := MSORT cons(x,$localExposureData.1)
    clearClams()
    if noquiet then
        sayKeyedMsg("S2IZ0049P", [x, $interpreterFrameName])

setExposeAddConstr(arg) ==
    setExposeAddConstr2(arg, true)

setExposeDrop arg ==
  (null arg) =>
    centerAndHighlight ('"The drop Option",$LINELENGTH,specialChar 'hbar)
    --  give msg about explicitly hidden constructors
    displayHiddenConstructors()
    sayMSG '" "
    sayKeyedMsg("S2IZ0049F",NIL)
  arg is [fn,:fnargs] and (fn := selectOptionLC(fn,
    '(group constructor),NIL)) =>
      fn = 'group  =>  setExposeDropGroup fnargs
      fn = 'constructor =>  setExposeDropConstr fnargs
      NIL
  setExposeDrop NIL

setExposeDropGroup arg ==
  (null arg) =>
    centerAndHighlight ('"The group Option",$LINELENGTH,specialChar 'hbar)
    sayKeyedMsg("S2IZ0049L",NIL)
    sayMSG '" "
    displayExposedGroups()
  for x in arg repeat
    if PAIRP x then x := QCAR x
    x = 'all =>
      $localExposureData.0 := NIL
      $localExposureData.1 := NIL
      $localExposureData.2 := NIL
      displayExposedGroups()
      sayMSG '" "
      displayExposedConstructors()
      sayMSG '" "
      displayHiddenConstructors()
      clearClams()
    member(x,$localExposureData.0) =>
      $localExposureData.0 := delete(x,$localExposureData.0)
      clearClams()
      sayKeyedMsg("S2IZ0049S",[x,$interpreterFrameName])
    GETALIST($globalExposureGroupAlist,x) =>
      sayKeyedMsg("S2IZ0049I",[x,$interpreterFrameName])
    sayKeyedMsg("S2IZ0049H",[x])

setExposeDropConstr arg ==
  (null arg) =>
    centerAndHighlight ('"The constructor Option",$LINELENGTH,
      specialChar 'hbar)
    sayKeyedMsg("S2IZ0049N",NIL)
    sayMSG '" "
    displayExposedConstructors()
    sayMSG '" "
    displayHiddenConstructors()
  for x in arg repeat
    x := unabbrev x
    if PAIRP x then x := QCAR x
    -- if the constructor is known, we know what type it is
    null(get_database(x, 'CONSTRUCTORKIND)) =>
      sayKeyedMsg("S2IZ0049J",[x])
    member(x,$localExposureData.2) =>
      sayKeyedMsg("S2IZ0049O",[x,$interpreterFrameName])
    if member(x,$localExposureData.1) then
      $localExposureData.1 := delete(x,$localExposureData.1)
    $localExposureData.2 := MSORT cons(x,$localExposureData.2)
    clearClams()
    sayKeyedMsg("S2IZ0049Q",[x,$interpreterFrameName])

setFunctionsCache arg ==
  $options : local := NIL
  arg = "%initialize%" =>
    $cacheCount := 0
    $cacheAlist := NIL
  arg = "%display%" =>
    null $cacheAlist => object2String $cacheCount
    '"..."
  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetFunctionsCache()
    TERPRI()
    sayAllCacheCounts()
  n := first arg
  (n ~= 'all) and ((not FIXP n) or (n < 0)) =>
    sayMessage ['"Your value of",:bright n,'"is invalid because ..."]
    describeSetFunctionsCache()
    terminateSystemCommand()
  if (rest arg) then $options := [['vars,:rest arg]]
  countCache n

countCache n ==
  $options =>
    $options is [["vars",:l]] =>
      for x in l repeat
        NULL IDENTP x => sayKeyedMsg("S2IF0007",[x])
        $cacheAlist:= insertAlist(x,n,$cacheAlist)
        cacheCountName := INTERNL1(x, '";COUNT")
        SET(cacheCountName,n)
        sayCacheCount(x,n)
    optionError(CAAR $options,nil)
  sayCacheCount(nil,$cacheCount:= n)

describeSetFunctionsCache() ==
  sayBrightly LIST(
    '%b,'")set functions cache",'%d,'"is used to tell FriCAS how many",'%l,_
    '" values computed by interpreter functions should be saved.  This can save ",'%l, _
    '" quite a bit of time in recursive functions, though one must consider that",'%l,_
    '" the cached values will take up (perhaps valuable) room in the workspace.",'%l,'%l,_
    '" The value given  after",'%b,'"cache",'%d,'"must either be the",_
    '" word",'%b,'"all",'%d,'"or a positive",'%l,_
    '" integer.  This may be followed by any number of function names whose cache",'%l,_
    '" sizes you wish to so set.  If no functions are given, the default cache",'%l,_
    '" size is set.",'%l,'" Examples:",_
    '"   )set fun cache all         )set fun cache 10 f g Legendre")

sayAllCacheCounts () ==
  sayCacheCount(nil,$cacheCount)
  $cacheAlist =>
    TERPRI()
--    SAY '" However,"
    for [x,:n] in $cacheAlist | n ~= $cacheCount repeat sayCacheCount(x,n)

sayCacheCount(fn,n) ==
  prefix:=
    fn => ['"function",:bright linearFormatName fn]
    n = 0 => ['"interpreter functions "]
    ['"In general, interpreter functions "]
  n = 0 =>
    fn =>
      sayBrightly ['"   Caching for ",:prefix,
        '"is turned off"]
    sayBrightly '" In general, functions will cache no returned values."
  phrase:=
    n="all" => [:bright '"all",'"values."]
    n=1 => ['" only the last value."]
    ['" the last",:bright n,'"values."]
  sayBrightly ['"   ",:prefix,'"will cache",:phrase]

setHistory arg ==
  -- this is just a front end for the history functions
  arg = "%initialize%" => nil

  current := object2String translateTrueFalse2YesNo $HiFiAccess
  arg = "%display%" => current

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    sayMessage ['" The",:bright '"history",'"option",
      '" may be followed by any one of the following:"]
    for name in '("on" "off" "yes" "no") repeat
      if name = current
        then sayBrightly ['" ->",:bright name]
        else sayBrightly ['"    ",name]
    TERPRI()
    sayBrightly '" The current setting is indicated within the list."
    sayBrightly [:bright '"yes",'"and",:bright '"no",
     '"have the same effect as",:bright '"on",'"and",:bright '"off",
       '"respectively."]
    if $useInternalHistoryTable
      then wh := '"memory"
      else wh := '"a file"
    sayBrightly ['%l,'" When the history facility is active, the data",
      '" is kept in ",wh,'"."]
    sayMessage ['" Issue",:bright '")help history",
      '"for more information."]

  arg is [fn] and
   (fn := DOWNCASE(fn)) in '(y n ye yes no on of off) =>
    $options := [[fn]]
    historySpad2Cmd()
  setHistory NIL


describeSetOutputU(s_nam, o_nam, o_ext, is_on, cs) ==
         sayBrightly LIST ('%b,'")set output ", s_nam,'%d,_
  '"is used to tell FriCAS to turn ", o_nam, '"-style output",'%l,_
  '"printing on and off, and where to place the output.  By default, the",'%l,_
  '"destination for the output is the screen but printing is turned off.",'%l,_
  '%l, '"Syntax:   )set output ", s_nam, '" <arg>",'%l,_
  '"    where arg can be one of",'%l,_
  '"  on          turn ", o_nam, _
   (is_on => '" printing on (default state)"; '" printing on") ,'%l,_
  '"  off         turn ", o_nam,
   (is_on => '" printing off"; '" printing off (default state)"),'%l,_
  '"  console     send ", o_nam, '" output to screen (default state)",'%l,_
  '"  fp<.fe>     send ", o_nam, _
    '" output to file with file prefix fp and file",'%l, _
  '"              extension .fe. If not given, .fe defaults to .", _
    o_ext, '".",'%l, '%l,_
  '"If you wish to send the output to a file, you may need to issue this command",'%l,_
  '"twice: once with", '%b, '"on", '%d, _
    '"and once with the file name. For example, to send", '%l,_
  o_nam, '" output to the file", '%b, '"polymer.", o_ext, '",", '%d, _
  '"issue the two commands", '%l, _
  '%l, _
  '"  )set output ", s_nam, '" on", '%l, _
  '"  )set output ", s_nam, '" polymer", '%l,_
  '%l, _
  '"The output is placed in the directory from which you invoked FriCAS or", _
  '%l, '"the one you set with the )cd system command.", '%l, _
  '"The current setting is: ",'%b, cs,'%d)

stream_close(st) ==
    if first(st) then CLOSE(rest(st))

try_open(fn, ft, append) ==
    fn := PNAME(fn)
    ft := PNAME(ft)
    if not((ptype := file_extention(fn)) = '"") then
        fn := drop_extention(fn)
        ft := ptype
    filename := make_full_namestring(make_filename2(fn, ft))
    null filename => [NIL, NIL]
    (testStream := makeStream(append, filename)) => [testStream, filename]
    [NIL, NIL]

setOutputAlgebra arg ==
  arg = "%initialize%" =>
    $algebraOutputStream := mkOutputConsoleStream()
    $algebraOutputFile := '"CONSOLE"
    $algebraFormat := true

  arg = "%display%" =>
    if $algebraFormat then label := '"On:" else label := '"Off:"
    STRCONC(label,$algebraOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputAlgebra()

  -- try to figure out what the argument is

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'spout]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(algebra algebra))
    UPCASE(fn) in '(NO OFF)  => $algebraFormat := NIL
    UPCASE(fn) in '(YES ON) => $algebraFormat := true
    UPCASE(fn) = 'CONSOLE =>
      stream_close($algebraOutputStream)
      $algebraOutputStream := mkOutputConsoleStream()
      $algebraOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    [testStream, filename] := try_open(fn, ft, false)
    testStream =>
      stream_close($algebraOutputStream)
      $algebraOutputStream := testStream
      $algebraOutputFile := filename
      sayKeyedMsg("S2IV0004",['"Algebra",$algebraOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft])

  sayKeyedMsg("S2IV0005",NIL)
  describeSetOutputAlgebra()

describeSetOutputAlgebra() == describeSetOutputU(
    '"algebra", '"algebra", '"spout", true, setOutputAlgebra "%display%")

setOutputCharacters arg ==
  -- this sets the special character set
  arg = "%initialize%" =>
    $specialCharacters := $plainRTspecialCharacters

  current :=
    $specialCharacters = $RTspecialCharacters      => '"default"
    $specialCharacters = $plainRTspecialCharacters => '"plain"
    '"unknown"
  arg = "%display%" => current

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    sayMessage ['" The",:bright '"characters",'"option",
      '" may be followed by any one of the following:"]
    for name in '("default" "plain") repeat
      if name = current
        then sayBrightly ['" ->",:bright name]
        else sayBrightly ['"    ",name]
    TERPRI()
    sayBrightly '" The current setting is indicated within the list.  This option determines "
    sayBrightly '" the special characters used for algebraic output.  This is what the"
    sayBrightly '" current choice of special characters looks like:"
    l := NIL
    for [char,:.] in $specialCharacterAlist repeat
      s := STRCONC('"   ",PNAME char,'" is shown as ",
        PNAME specialChar(char))
      l := cons(s,l)
    sayAsManyPerLineAsPossible reverse l

  arg is [fn] and (fn := DOWNCASE(fn)) =>
    fn = 'default => $specialCharacters := $RTspecialCharacters
    fn = 'plain   => $specialCharacters := $plainRTspecialCharacters
    setOutputCharacters NIL
  setOutputCharacters NIL

makeStream(append,filename) ==
  append => make_append_stream(filename)
  make_out_stream(filename)

setOutputFortran arg ==
  arg = "%initialize%" =>
    $fortranOutputStream := mkOutputConsoleStream()
    $fortranOutputFile := '"CONSOLE"
    $fortranFormat := NIL

  arg = "%display%" =>
    if $fortranFormat then label := '"On:" else label := '"Off:"
    STRCONC(label,$fortranOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputFortran()

  -- try to figure out what the argument is

  append := NIL
  quiet := NIL
  while LISTP arg and UPCASE(first arg) in '(APPEND QUIET) repeat
    if UPCASE first(arg) = 'APPEND then append := true
    else if UPCASE first(arg) = 'QUIET then quiet := true
    arg := rest(arg)

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'sfort]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(FORTRAN fortran))
    UPCASE(fn) in '(NO OFF)  => $fortranFormat := NIL
    UPCASE(fn) in '(YES ON)  => $fortranFormat := true
    UPCASE(fn) = 'CONSOLE =>
      stream_close($fortranOutputStream)
      $fortranOutputStream := mkOutputConsoleStream()
      $fortranOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    [testStream, filename] := try_open(fn, ft, append)
    testStream =>
      stream_close($fortranOutputStream)
      $fortranOutputStream := testStream
      $fortranOutputFile := filename
      if null quiet then sayKeyedMsg("S2IV0004",['FORTRAN,$fortranOutputFile])
    if null quiet then sayKeyedMsg("S2IV0003",[fn,ft])
  if null quiet then sayKeyedMsg("S2IV0005",NIL)
  describeSetOutputFortran()

describeSetOutputFortran() == describeSetOutputU(
    '"fortran", '"FORTRAN", '"sfort", false, setOutputFortran "%display%")

setOutputMathml arg ==
  arg = "%initialize%" =>
    $mathmlOutputStream := mkOutputConsoleStream()
    $mathmlOutputFile := '"CONSOLE"
    $mathmlFormat := NIL

  arg = "%display%" =>
    if $mathmlFormat then label := '"On:" else label := '"Off:"
    STRCONC(label,$mathmlOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputMathml()

  -- try to figure out what the argument is

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'smml]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(MathML mathml))
    UPCASE(fn) in '(NO OFF)  => $mathmlFormat := NIL
    UPCASE(fn) in '(YES ON) => $mathmlFormat := true
    UPCASE(fn) = 'CONSOLE =>
      stream_close($mathmlOutputStream)
      $mathmlOutputStream := mkOutputConsoleStream()
      $mathmlOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    [testStream, filename] := try_open(fn, ft, false)
    testStream =>
      stream_close($mathmlOutputStream)
      $mathmlOutputStream := testStream
      $mathmlOutputFile := filename
      sayKeyedMsg("S2IV0004",['"MathML",$mathmlOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft])

  sayKeyedMsg("S2IV0005",NIL)
  describeSetOutputMathml()

describeSetOutputMathml() == describeSetOutputU(
    '"mathml", '"MathML", '"smml", false, setOutputMathml "%display%")

setOutputTexmacs arg ==
  arg = "%initialize%" =>
    $texmacsOutputStream := mkOutputConsoleStream()
    $texmacsOutputFile := '"CONSOLE"
    $texmacsFormat := NIL

  arg = "%display%" =>
    if $texmacsFormat then label := '"On:" else label := '"Off:"
    STRCONC(label,$texmacsOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputTexmacs()

  -- try to figure out what the argument is

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'stmx]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(Texmacs texmacs))
    UPCASE(fn) in '(NO OFF)  => $texmacsFormat := NIL
    UPCASE(fn) in '(YES ON) => $texmacsFormat := true
    UPCASE(fn) = 'CONSOLE =>
      stream_close($texmacsOutputStream)
      $texmacsOutputStream := mkOutputConsoleStream()
      $texmacsOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    [testStream, filename] := try_open(fn, ft, false)
    testStream =>
      stream_close($texmacsOutputStream)
      $texmacsOutputStream := testStream
      $texmacsOutputFile := filename
      sayKeyedMsg("S2IV0004",['"Texmacs",$texmacsOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft])

  sayKeyedMsg("S2IV0005",NIL)
  describeSetOutputTexmacs()

describeSetOutputTexmacs() == describeSetOutputU(
    '"texmacs", '"Texmacs", '"stmx", false, setOutputTexmacs "%display%")


setOutputHtml arg ==
  arg = "%initialize%" =>
    $htmlOutputStream := mkOutputConsoleStream()
    $htmlOutputFile := '"CONSOLE"
    $htmlFormat := NIL

  arg = "%display%" =>
    if $htmlFormat then label := '"On:" else label := '"Off:"
    STRCONC(label, $htmlOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputHtml()

  -- try to figure out what the argument is

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'shtml]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(HTML html))
    UPCASE(fn) in '(NO OFF)  => $htmlFormat := NIL
    UPCASE(fn) in '(YES ON) => $htmlFormat := true
    UPCASE(fn) = 'CONSOLE =>
      stream_close($htmlOutputStream)
      $htmlOutputStream := mkOutputConsoleStream()
      $htmlOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    [testStream, filename] := try_open(fn, ft, false)
    testStream =>
      stream_close($htmlOutputStream)
      $htmlOutputStream := testStream
      $htmlOutputFile := filename
      sayKeyedMsg("S2IV0004",['"HTML",$htmlOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft])

  sayKeyedMsg("S2IV0005",NIL)
  describeSetOutputHtml()

describeSetOutputHtml() == describeSetOutputU(
    '"html", '"HTML", '"shtml", false, setOutputHtml "%display%")

setOutputOpenMath arg ==
  arg = "%initialize%" =>
    $openMathOutputStream := mkOutputConsoleStream()
    $openMathOutputFile := '"CONSOLE"
    $openMathFormat := NIL

  arg = "%display%" =>
    if $openMathFormat then label := '"On:" else label := '"Off:"
    STRCONC(label,$openMathOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputOpenMath()

  -- try to figure out what the argument is

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'som]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(OpenMath openmath))
    UPCASE(fn) in '(NO OFF)  => $openMathFormat := NIL
    UPCASE(fn) in '(YES ON) => $openMathFormat := true
    UPCASE(fn) = 'CONSOLE =>
      stream_close($openMathOutputStream)
      $openMathOutputStream := mkOutputConsoleStream()
      $openMathOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    [testStream, filename] := try_open(fn, ft, false)
    testStream =>
      stream_close($openMathOutputStream)
      $openMathOutputStream := testStream
      $openMathOutputFile := filename
      sayKeyedMsg("S2IV0004",['"OpenMath",$openMathOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft])

  sayKeyedMsg("S2IV0005",NIL)
  describeSetOutputOpenMath()

describeSetOutputOpenMath() == describeSetOutputU(
    '"openmath", '"OpenMath", '"som", false, setOutputOpenMath "%display%")


setOutputTex arg ==
  arg = "%initialize%" =>
    $texOutputStream := mkOutputConsoleStream()
    $texOutputFile := '"CONSOLE"
    $texFormat := NIL

  arg = "%display%" =>
    if $texFormat then label := '"On:" else label := '"Off:"
    STRCONC(label,$texOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputTex()

  -- try to figure out what the argument is

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'stex]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(TeX tex))
    UPCASE(fn) in '(NO OFF)  => $texFormat := NIL
    UPCASE(fn) in '(YES ON) => $texFormat := true
    UPCASE(fn) = 'CONSOLE =>
      stream_close($texOutputStream)
      $texOutputStream := mkOutputConsoleStream()
      $texOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    [testStream, filename] := try_open(fn, ft, false)
    testStream =>
      stream_close($texOutputStream)
      $texOutputStream := testStream
      $texOutputFile := filename
      sayKeyedMsg("S2IV0004",['"TeX",$texOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft])

  sayKeyedMsg("S2IV0005",NIL)
  describeSetOutputTex()

describeSetOutputTex() == describeSetOutputU(
    '"tex", '"TeX", '"stex", false, setOutputTex "%display%")


setOutputFormatted arg ==
  arg = "%initialize%" =>
    $formattedOutputStream := mkOutputConsoleStream()
    $formattedOutputFile := '"CONSOLE"
    $formattedFormat := NIL

  arg = "%display%" =>
    if $formattedFormat then label := '"On:" else label := '"Off:"
    STRCONC(label, $formattedOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputFormatted()

  -- try to figure out what the argument is

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'formatted]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(FORMATTED formatted))
    UPCASE(fn) in '(NO OFF) => $formattedFormat := NIL
    UPCASE(fn) in '(YES ON) => $formattedFormat := true
    UPCASE(fn) = 'CONSOLE =>
      stream_close($formattedOutputStream)
      $formattedOutputStream := mkOutputConsoleStream()
      $formattedOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    [testStream, filename] := try_open(fn, ft, false)
    testStream =>
      stream_close($formattedOutputStream)
      $formattedOutputStream := testStream
      $formattedOutputFile := filename
      sayKeyedMsg("S2IV0004",['"FORMATTED",$formattedOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft])

  sayKeyedMsg("S2IV0005",NIL)
  describeSetOutputFormatted()

describeSetOutputFormatted() == describeSetOutputU(
    '"formatted",'"formatted",'"formatted",false,setOutputFormatted "%display%")

setStreamsCalculate arg ==
  arg = "%initialize%" =>
    $streamCount := 10
  arg = "%display%" =>
    object2String $streamCount
  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetStreamsCalculate()
  n := first arg
  (n ~= 'all) and ((not FIXP n) or (n < 0)) =>
    sayMessage ['"Your value of",:bright n,'"is invalid because ..."]
    describeSetStreamsCalculate()
    terminateSystemCommand()
  $streamCount := n

describeSetStreamsCalculate() == sayKeyedMsg("S2IV0001",[$streamCount])
