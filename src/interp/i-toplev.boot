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

-- This file contains the top-most code for receiving parser output,
-- calling the analysis routines and printing the result output. It
-- also contains several flavors of routines that start the interpreter
-- from LISP.


--% Top Level Interpreter Code

-- When $QuiteCommand is true Spad will not produce any output from
--  a top level command
DEFPARAMETER($QuietCommand, NIL)
-- When $ProcessInteractiveValue is true, we don't want the value printed
-- or recorded.
DEFPARAMETER($ProcessInteractiveValue, NIL)

DEFPARAMETER($QuietCommand_tmp, nil)
intSetQuiet() ==
  $QuietCommand_tmp := true

intUnsetQuiet() ==
  $QuietCommand_tmp := nil

--% Starting the interpreter from LISP

interpsysInitialization(display_messages) ==
  -- The function  start  begins the interpreter process, reading in
  -- the profile and printing start-up messages.
  $PrintCompilerMessageIfTrue: local := nil
  resetWorkspaceVariables()
  save_displayStartMsgs := $displayStartMsgs
  if not(display_messages) then
      $displayStartMsgs := display_messages
  initHist()
  initNewWorld()
  interpOpen(display_messages)
  createInitializers()
  if $displayStartMsgs then sayKeyedMsg("S2IZ0053",['"interpreter"])
  initializeTimedNames()
  $InteractiveFrame := makeInitialModemapFrame()
  initializeInterpreterFrameRing()
  setOutputAlgebra "%initialize%"
  loadExposureGroupData()
  if $displayStartMsgs then sayKeyedMsg("S2IZ0053",['"database"])
  mkLowerCaseConTable()
  if not $ruleSetsInitialized then initializeRuleSets()
  if $displayStartMsgs then sayKeyedMsg("S2IZ0053",['"constructors"])
  makeConstructorsAutoLoad()
  GCMSG(NIL)
  SETQ($IOindex,1)
  if $displayStartMsgs then sayKeyedMsg("S2IZ0053",['"history"])
  initHist()
  if $displayStartMsgs then spadStartUpMsgs()
  $superHash := MAKE_HASHTABLE('EQUAL)
  $displayStartMsgs := save_displayStartMsgs

interpsys_restart() ==
  $IOindex := 1
  $InteractiveFrame := makeInitialModemapFrame()
  loadExposureGroupData()
  initHist()
  initializeInterpreterFrameRing()
  buildHtMacroTable()

  if $displayStartMsgs then spadStartUpMsgs()
  $currentLine := nil
  interpOpen(true) -- open up the interpreter database
  operationOpen(true) -- all of the operations known to the system
  categoryOpen(true) -- answer hasCategory question
  browseOpen(true)
  makeConstructorsAutoLoad()
  createInitializers2()

readSpadProfileIfThere() ==
  -- reads SPADPROF INPUT if it exists
  file := getEnv('"FRICAS_INITFILE")
  file = '"" => nil
  efile :=
    make_input_filename(file) => file
    file := ['_.fricas, 'input]
    make_input_filename(file) => file
    file := ['_.axiom, 'input]
    make_input_filename(file) => file
    NIL
  efile =>
    $edit_file := efile
    read_or_compile(true, false)
  NIL

--% Parser Output --> Interpreter

DEFPARAMETER($inRetract, nil)

processInteractive(form, posnForm) ==
    $timedNameStack : local := NIL
    $statsInfo : local := NIL
    initializeTimedStack()
    finally(
        object := processInteractive0(form, posnForm),
          while $timedNameStack repeat stopTimingProcess peekTimedName())
    object

processInteractive0(form, posnForm) ==
  --  Top-level dispatcher for the interpreter.  It sets local variables
  --  and then calls processInteractive1 to do most of the work.
  --  This function receives the output from the parser.

  $op: local:= (form is [op,:.] => op; form) --name of operator
  $Coerce: local := NIL
  $compErrorMessageStack:local := nil
  $freeVars : local := NIL
  $mapList:local := NIL            --list of maps being type analyzed
  $compilingMap:local:= NIL        --true when compiling a map
  $compilingLoop:local:= NIL       --true when compiling a loop body
  $interpOnly: local := NIL        --true when in interpret only mode
  $whereCacheList: local := NIL    --maps compiled because of where
  $timeGlobalName: local := '$compTimeSum  --see incrementTimeSum
  $declaredMode: local := NIL      --Weak type propagation for symbols
  $localVars:local := NIL          --list of local variables in function
  $analyzingMapList:local := NIL   --names of maps currently being
                                   --analyzed
  $instantCoerceCount: local := 0
  $instantCanCoerceCount: local := 0
  $instantMmCondCount: local := 0
  $minivector: local := NIL
  $domPvar: local := NIL
  $inRetract: local := NIL
  object := processInteractive1(form, posnForm)
  if not($ProcessInteractiveValue) then
    if $reportInstantiations = true then
      reportInstantiations()
      CLRHASH $instantRecord
    writeHistModesAndValues()
    updateHist()
  if $printTimeIfTrue then printTime()
  if $printStorageIfTrue then printStorage()
  object

processInteractive1(form, posnForm) ==
  -- calls the analysis and output printing routines
  $e : local := $InteractiveFrame
  recordFrame 'system

  startTimingProcess 'analysis
  object   := interpretTopLevel(form, posnForm)
  stopTimingProcess 'analysis

  startTimingProcess 'print
  if not($ProcessInteractiveValue) then
    recordAndPrint(objValUnwrap object,objMode object)
  recordFrame 'normal
  stopTimingProcess 'print

  object

ncParseAndInterpretString s ==
   processInteractive(parseFromString(s), nil)

--% Result Output Printing

recordAndPrint(x,md) ==
  --  Prints out the value x which is of type m, and records the changes
  --  in environment $e into $InteractiveFrame
  --  $printAnyIfTrue  is documented in setvart.boot. controlled with )se me any
  if md = '(Any) and $printAnyIfTrue  then
    md' := first  x
    x' := rest x
  else
    x' := x
    md' := md
  mode:= (md=$EmptyMode => quadSch(); md)
  if (md ~= $Void) or $printVoidIfTrue then
    if null $collectOutput then TERPRI(get_algebra_stream())
    if $QuietCommand = false then
      output(x',md')
  putHist('%,'value,objNewWrap(x,md),$e)
  if $printTypeIfTrue then printType(x', md')
  'done

printType(x, m) ==  -- m is the mode/type of the result
  if m is ['Union, :argl] then
    x' := retract(objNewWrap(x,m))
    m' := objMode x'
    m := ['Union, :[arg for arg in argl | sameUnionBranch(arg, m')], '"..."]
  if $printTypeIfTrue then
    type_string := outputDomainConstructor(m)
    $collectOutput =>
        $outputLines :=
            [justifyMyType msgText("S2GL0012", [type_string]), :$outputLines]
    sayKeyedMsg("S2GL0012", [type_string])

sameUnionBranch(uArg, m) ==
  uArg is [":", ., t] => t = m
  uArg = m

msgText(key, args) ==
  msg := segmentKeyedMsg getKeyedMsg key
  msg := substituteSegmentedMsg(msg,args)
  msg := flowSegmentedMsg(msg,$LINELENGTH,$MARGIN)
  concatenateStringList([STRINGIMAGE x for x in CDAR msg])

justifyMyType(t) ==
  len := #t
  len > $LINELENGTH => t
  CONCAT(fillerSpaces($LINELENGTH-len, '" "), t)

typeTimePrin x ==
  maprinSpecial(x,0,79)

printTime() ==
  $collectOutput => nil
  s := makeLongTimeString($interpreterTimedNames, $interpreterTimedClasses)
  sayKeyedMsg("S2GL0013", [s])

printStorage() ==
  $collectOutput => nil
  storeString :=
    makeLongSpaceString($interpreterTimedNames, $interpreterTimedClasses)
  sayKeyedMsg("S2GL0016",[storeString])

--%  Interpreter Middle-Level Driver + Utilities

interpretTopLevel(x, posnForm) ==
  --  Top level entry point from processInteractive1.  Sets up catch
  --  for a thrown result
  savedTimerStack := COPY $timedNameStack
  c := CATCH('interpreter,interpret(x, posnForm))
  while savedTimerStack ~= $timedNameStack repeat
    stopTimingProcess peekTimedName()
  c = 'tryAgain => interpretTopLevel(x, posnForm)
  c

interpret(x, posnForm) ==
  --type analyzes and evaluates expression x, returns object
  $env:local := [[NIL]]
  $genValue:local := true       --evaluate all generated code
  -- counter used to limit recursion depth during resolve
  $resolve_level : local := 0
  interpret1(x,nil,posnForm)

interpret1(x,rootMode,posnForm) ==
  -- dispatcher for the type analysis routines.  type analyzes and
  -- evaluates the expression x in the rootMode (if non-nil)
  -- which may be $EmptyMode.  returns an object if evaluating, and a
  -- modeset otherwise

  -- create the attributed tree

  node := mkAtreeWithSrcPos(x, posnForm)
  if rootMode then putTarget(node,rootMode)

  -- do type analysis and evaluation of expression.  The real guts

  modeSet:= bottomUp node
  newRootMode := (null rootMode => first modeSet ; rootMode)
  argVal := getArgValue(node, newRootMode)
  argVal and not $genValue => objNew(argVal, newRootMode)
  argVal and (val:=getValue node) => interpret2(val,newRootMode,posnForm)
  keyedSystemError("S2IS0053",[x])

interpret2(object,m1,posnForm) ==
  x := objVal object
  m := objMode object
  m=$EmptyMode =>
    x is [op, :.]  and op in '(SPADMAP STREAM) => objNew(x, m1)
    m1 = $EmptyMode => objNew(x,m)
    systemErrorHere '"interpret2"
  m1 =>
    if (ans := coerceInteractive(object,m1)) then ans
    else throwKeyedMsgCannotCoerceWithValue(x,m,m1)
  object
