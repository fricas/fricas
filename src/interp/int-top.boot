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


)if false
This is the top level loop when reading from the input console.  This
function calls itself after processing the current line.  Because of
this it is important that the underlying common lisp supports
tail-recursion.

Normally we never really exit this function.

We read a string from the input. The serverReadLine\cite{1} function
is a special readline function that handles communication with the
session manager code, which is a separate process running in parallel.
In the usual case it just returns the current string.

If the user enters a blank line ([[#a=]]) then just put up another prompt
and then tail-recursively call [[intloopReadConsole]].

If the user has set [[$DALYMODE]] to true and the new line starts with
an open parenthesis then the input is assumed to be a lisp expression
and is evaluated by the underlying common lisp. This is useful if you
are doing a lot of debugging. Commands can also be executed in the
underlying common lisp by using the [[)lisp]] command. In either case we
tail-recursively call [[intloopReadConsole]].

If the user typed [[)fin]] then we exit the loop and drop into the
underlying common lisp. You can use the [[(restart)]] function call
to return to the top level loop.

If the input line starts with a close parenthesis we parse the
input line as a command rather than an expression. We execute the command
and then tail-recursively call [[intloopReadConsole]].

If the input line contains a trailing underscore, which is the standard
end-of-line escape character, then we continue to read the line by
tail-recursively calling [[intloopReadConsole]].

If none of the above conditions occur we simply evaluate the input line
and then tail-recursively call [[intloopReadConsole]].
)endif

)package "BOOT"

-- User function to call when performing interactive input or output.

-- $ioHook receives two arguments, namely a symbol identifying curent i/o
-- event, and a list (or nil) containing optional arguments, depending on the
-- event.

-- One possible way to use it is:
-- )lisp (setf |$ioHook| (lambda (x arg) (format t "<~S>~%" x)))

DEFPARAMETER($ioHook, nil)
DEFPARAMETER($erMsgToss, false)

ioHook(x, :args) ==
   if $ioHook then FUNCALL($ioHook, x, args)

--% INTERPRETER TOP LEVEL

-- Variables to control phases and their output

$ncmParse :=            NIL
$ncmMacro :=            NIL
$ncmPhase :=      NIL

evalInlineCode() ==
  args := getCLArgs()
  while args repeat
    arg := first args
    args := rest args
    if arg = '"-eval" and args then
      CATCH('SPAD_READER, CATCH('top_level, parseAndEvalStr first(args)))
      args := rest args

spad() ==
  -- starts the interpreter, read in profiles, etc.
  $PrintCompilerMessageIfTrue: local
  setOutputAlgebra "%initialize%"
  readSpadProfileIfThere()
  evalInlineCode()
  runspad()
  'EndOfSpad

runspad() ==
  mode:='restart
  while mode='restart repeat
    resetStackLimits()
    CATCH($quitTag, CATCH('coerceFailure,
                  mode:=CATCH('top_level, ncTopLevel())))

ncTopLevel() ==
-- Top-level read-parse-eval-print loop for the interpreter.  Uses
-- the Bill Burge's parser.
  _*EOF_*: fluid := NIL
  $InteractiveMode :fluid := true
  $e:fluid := $InteractiveFrame
  ncIntLoop()

++ If the interpreter is spwan by the session manager, then
++ each successful connection also creates its own frame.
++ In particular, the only time we get to do anything in the `initial'
++ frame is when we get the first connection.  In that case, we would
++ be asked by the session manager to create a frame.  The client is
++ not aware of that,  It is therefore confusing to display a prompt,
++ because all this horse-threading happens behind the client's back.
printFirstPrompt?() ==
    $interpreterFrameName ~= "initial" or not($SpadServer)

ncIntLoop() ==
  intloop()


intloop () ==
    mode := "restart"
    while mode = "restart" repeat
      resetStackLimits()
      mode := CATCH("top_level",
                    SpadInterpretStream(1, ["TIM", "DALY", "?"], true))


SpadInterpretStream(step_num, source, interactive?) ==
    pile?                    := not interactive?
--  following seems useless and causes ccl package problems
--    $InteractiveMode : local := false

    $newcompErrorCount: local := 0 -- SMW Feb 2/90.
                                   -- Used in highComplete, ncHardError etc.

    $inclAssertions: local := ["AIX", "CommonLisp"] -- Jan 28/90


    $lastPos               : local := $nopos   ------------>!!!
    $erMsgToss             : local := false --------------->!!!
    $ncMsgList             : local := nil

    interactive? =>
        if printFirstPrompt?() then
            princPrompt()
        intloopReadConsole([], step_num)
        []
    intloopInclude (source,0)
    []

    -----------------------------------------------------------------

SpadInterpretFile fn ==
  SpadInterpretStream(1, fn, nil)

ncINTERPFILE(file, echo) ==
  $EchoLines : local := echo
  $ReadingFile : local := true
  SpadInterpretFile(file)

setCurrentLine s ==
  v := $currentLine
  $currentLine :=
     NULL(v) => s
     u :=
        STRINGP(s) => [s]
        s
     STRINGP(v) => [v, :u]
     RPLACD(LASTNODE(v), u)
     v

intloopReadConsole(b, n)==
    ioHook("startReadLine")
    a:= serverReadLine(_*STANDARD_-INPUT_*)
    ioHook("endOfReadLine")
    not STRINGP a => leaveScratchpad()
    b = [] and #a=0 =>
             princPrompt()
             intloopReadConsole([], n)
    $DALYMODE and intloopPrefix?('"(",a) =>
            intnplisp(a)
            princPrompt()
            intloopReadConsole([], n)
    pfx := stripSpaces intloopPrefix?('")fi",a)
    pfx and ((pfx = '")fi") or (pfx = '")fin")) => []
    b = [] and (d := intloopPrefix?('")", a)) =>
             setCurrentLine d
             c := ncloopCommand(d,n)
             princPrompt()
             intloopReadConsole([], c)
    b := CONS(a, b)
    ncloopEscaped a => intloopReadConsole(b, n)
    c := intloopProcessStrings(nreverse b, n)
    princPrompt()
    intloopReadConsole([], c)

-- The 'intloopPrefix?' function tests if the string 'prefix' is
-- is a prefix of the string 'whole', ignoring leading whitespace.
intloopPrefix?(prefix,whole) ==
     #prefix > #whole => false
     good := true
     leading := true
     spaces := 0
     i := 0
     len := #prefix
     wlen := #whole
     for j in 0.. while (good and i < len and j < wlen) repeat
       good := (prefix.i = whole.j) or (leading and (whole.j = char " "))
       if prefix.i = whole.j then i := i+1
       if (whole.j = char " ") and leading then
         spaces := spaces + 1
       else leading := false
     spaces = wlen => nil
     if good then SUBSTRING(whole,spaces,nil) else good


intloopProcess(n,interactive,s)==
     StreamNull s => n
     [lines, ptree] := first s
     pfAbSynOp?(ptree,"command")=>
            if interactive then setCurrentLine tokPart ptree
            InterpExecuteSpadSystemCommand(tokPart ptree)
            intloopProcess(n, interactive, rest s)
     intloopProcess(intloopSpadProcess(n, lines, ptree, interactive),
                 interactive, rest s)

intloopEchoParse s==
         [dq, stream] := first s
         [lines, restl] := ncloopDQlines(dq, $lines)
         setCurrentLine(mkLineList(lines))
         if $EchoLines then ncloopPrintLines lines
         $lines := restl
         cons([[lines, npParse dqToList dq]], rest s)

intloopInclude0(st, name, n) ==
    $lines:local:=incStream(st,name)
    intloopProcess(n,false,
      next(function intloopEchoParse,
        next(function insertpile,
          next(function lineoftoks,$lines))))

intloopInclude(name, n) ==
    handle_input_file(name, function intloopInclude0, [name, n])
      or error('"File not found")

intloopInclude1(name,n) ==
          a:=ncloopIncFileName name
          a => intloopInclude(a,n)
          n

fakepile(s) ==
    if npNull s then [false, 0, [], s]
    else
        [h, t] := [car s, cdr s]
        ss := cdr(h)
        ress := car h
        while not npNull t repeat
            h := car (car t)
            t := cdr t
            ress := dqAppend(ress, h)
        cons([[ress, :ss]], t)

intloopProcessStrings(s, n) ==
     setCurrentLine s
     intloopProcess(n, true,
         next(function ncloopParse,
             next(function fakepile,
                 next(function lineoftoks, incStrings s))))

$pfMacros := []

clearMacroTable() ==
   SETF($pfMacros, nil)

getParserMacros() == $pfMacros

displayParserMacro m ==
   m := ASSQ(m, $pfMacros)
   NULL m => nil
   pfPrintSrcLines(CADDR(m))

intSetNeedToSignalSessionManager() ==
    $NeedToSignalSessionManager := true

intloopSpadProcess(stepNo,lines,ptree,interactive?)==
    $stepNo:local := stepNo
    $currentCarrier := cc := ['carrier]
    ncPutQ(cc, 'stepNumber, stepNo)
    ncPutQ(cc, 'messages, $ncMsgList)
    ncPutQ(cc, 'lines, lines)
    $ncMsgList := nil
    result := CATCH("SpadCompileItem",
     CATCH("coerceFailure", CATCH("SPAD_READER",
       interp(cc, ptree, interactive?)))) where

        interp(cc, ptree, interactive?) ==
            ncConversationPhase(function phParse,            [cc, ptree])
            ncConversationPhase(function phMacro,            [cc])
            ncConversationPhase(function phIntReportMsgs,[cc, interactive?])
            ncConversationPhase(function phInterpret,        [cc])

            #ncEltQ(cc, 'messages) ~= 0 => ncError()

    intSetNeedToSignalSessionManager()
    $prevCarrier := $currentCarrier
    result = 'ncEnd     => stepNo
    result = 'ncError   => stepNo
    result = 'ncEndItem => stepNo
    stepNo+1

phInterpret carrier ==
  ptree := ncEltQ(carrier, 'ptree)
  val := intInterpretPform(ptree)
  ncPutQ(carrier, 'value, val)

intInterpretPform pf ==
  processInteractive(zeroOneTran(packageTran(pf2Sex pf)), pf)

--% phReportMsgs: carrier[lines,messages,..]-> carrier[lines,messages,..]
phIntReportMsgs(carrier, interactive?) ==
    $erMsgToss => 'OK
    lines := ncEltQ(carrier, 'lines)
    msgs  := ncEltQ(carrier, 'messages)
    nerr  := #msgs
    ncPutQ(carrier, 'ok?, nerr = 0)
    nerr = 0 => 'OK
    processMsgList(msgs, lines)
    intSayKeyedMsg ('S2CTP010,[nerr])
    'OK

intSayKeyedMsg(key, args) ==
  sayKeyedMsg(packageTran key, packageTran args)

mkLineList lines ==
  l := [rest line for line in lines | nonBlank rest line]
  #l = 1 => first l
  l

nonBlank str ==
  value := false
  for i in 0..MAXINDEX str repeat
    str.i ~= char " " =>
      value := true
      return value
  value

ncloopCommand (line,n) ==
         a:=ncloopPrefix?('")include",line)=>
                  ncloopInclude1( a,n)
         InterpExecuteSpadSystemCommand(line)
         n

ncloopEscaped x == #x > 0 and x.(#x - 1) = '"__".0

ncloopDQlines (dq,stream)==
        StreamNull stream
        a:= poGlobalLinePosn tokPosn CADR dq
        b:= poGlobalLinePosn CAAR stream
        streamChop (a-b+1,stream)

streamChop(n,s)==
    if StreamNull s
    then [nil,nil]
    else if EQL(n,0)
         then [nil,s]
         else
            [a,b]:= streamChop(n-1,cdr s)
            line:=car s
            c := ncloopPrefix?('")command", rest line)
            d:= cons(car line,if c then c else cdr line)
            [cons(d,a),b]

ncloopPrintLines lines ==
        for line in lines repeat WRITE_-LINE rest line
        WRITE_-LINE '" "

ncloopIncFileName string==
                fn := incFileName string
                not fn =>
                    WRITE_-LINE (CONCAT(string, '" not found"))
                    []
                fn

ncloopParse s==
         [dq, stream] := first s
         [lines, .] := ncloopDQlines(dq, stream)
         cons([[lines, npParse dqToList dq]], rest s)

ncloopInclude0(st, name, n) ==
     $lines:local := incStream(st, name)
     ncloopProcess(n,false,
         next(function ncloopEchoParse,
           next(function insertpile,
            next(function lineoftoks,$lines))))

ncloopInclude(name, n) ==
    handle_input_file(name, function ncloopInclude0, [name, n])
      or error('"File not found")

ncloopInclude1(name,n) ==
          a:=ncloopIncFileName name
          a => ncloopInclude(a,n)
          n

incString s== incRenumber incLude(0,[s],0,['"strings"] ,[Top])

incStrings(s) == incRenumber incLude(0, s, 0, ['"strings"], [Top])

ncError() ==
    THROW("SpadCompileItem",'ncError)

--% Compilation Carriers
--  This data structure is used to carry information between phases.

--% phParse: carrier[tokens,...] -> carrier[ptree, tokens,...]
phParse(carrier,ptree) ==
    phBegin 'Parsing
    if $ncmParse then
           nothing
           intSayKeyedMsg ('S2CTP003,[%pform ptree])
    ncPutQ(carrier, 'ptree, ptree)
    'OK


--% phMacro: carrier[ptree,...] -> carrier[ptree, ptreePremacro,...]
phMacro carrier ==
    phBegin 'Macroing
    ptree  := ncEltQ(carrier, 'ptree)
    ncPutQ(carrier, 'ptreePremacro, ptree)

    ptree  := macroExpanded ptree
    if $ncmMacro then
        intSayKeyedMsg ('S2CTP007,[%pform ptree] )

    ncPutQ(carrier, 'ptree, ptree)
    'OK

ncConversationPhase(fn, args) ==
    carrier := first args

    $ncMsgList: local := []
    $convPhase: local := 'NoPhase

    UNWIND_-PROTECT( APPLY(fn, args), wrapup(carrier) ) where
        wrapup(carrier) ==
            for m in $ncMsgList repeat
                ncPutQ(carrier, 'messages, [m, :ncEltQ(carrier, 'messages)])

ncloopPrefix?(prefix,whole) ==
     #prefix > #whole => false
     good:=true
     for i in 0..#prefix-1 for j in 0.. while good repeat
                good:= prefix.i = whole.j
     if good then SUBSTRING(whole,#prefix,nil) else good

phBegin id ==
    $convPhase := id
    if $ncmPhase then intSayKeyedMsg('S2CTP021,[id])
