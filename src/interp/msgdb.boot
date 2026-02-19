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
Description of Messages

FriCAS messages are read from a flat file database and returned
as one long string.  They are preceded in the database by a key and
this is how they are referenced from code.  For example, one key is
S2IL0001 which means:
   S2          Scratchpad II designation
   I           from the interpreter
   L           originally from LISPLIB BOOT
   0001        a sequence number

Each message may contain formatting codes and parameter codes.
The formatting codes are:
   %b          turn on bright printing
   %ceoff      turn off centering
   %ceon       turn on centering
   %d          turn off bright printing
   %f          user defined printing
   %i          start indentation of 3 more spaces
   %l          start a new line
   %m          math-print an expression
   %rjoff      turn off right justification (actually ragged left)
   %rjon       turn on right justification (actually ragged left)
   %s          pretty-print as an S-expression
   %u          unindent 3 spaces
   %x#         insert # spaces

The parameter codes look like %1, %2b, %3p, %4m, %5bp, %6s where the
digit is the parameter number ans the letters following indicate
additional formatting. You can indicate as many additional formatting
qualifiers as you like, to the degree they make sense. The "p" code
means to call prefix2String on the parameter, a standard way of
printing abbreviated types.  The "P" operator maps prefix2String over
its arguments.  The "o" operation formats the argument as an operation
name.  "b" means to print that parameter in
a bold (bright) font. "c" means to center that parameter on a
new line.  "f" means that the parameter is a list [fn, :args]
and that "fn" is to be called on "args" to get the text. "r" means
to right justify (ragged left) the argument.

Look in the file with the name defined in $defaultMsgDatabaseName
above for examples.
)endif

--% Message Database Code and Message Utility Functions

$testingSystem := false
$sayBrightlyStream := nil
$highlightAllowed := false

DEFPARAMETER($testingErrorPrefix, '"Daly Bug")

escape_strings(l) ==
    ATOM(l) => l
    res := []
    for s in l repeat
        if IDENTP s then s := PNAME s
        res := cons(s, res)
        not(STRINGP(s)) => "iterate"
        #s < 1 => "iterate"
        s.0 = char "%" =>
            s1 := STRCONC('"\", s)
            res := cons(s1, rest(res))
        #s > 1 and s.0 = char "\" and s.1 = char "%" =>
            res := cons('"\", rest(res))
            res := cons(s, res)
        "iterate"
    NREVERSE(res)

--% Accessing the Database

string2Words l ==
  i := 0
  [w while wordFrom(l,i) is [w,i]]

wordFrom(l,i) ==
  maxIndex := MAXINDEX l
  k := or/[j for j in i..maxIndex | l.j ~= char ('_ ) ] or return nil
  k0 := k
  while k <= maxIndex and (c := l.k) ~= char ('_ ) repeat
    k := k + 1
  [SUBSEQ(l, k0, k), k + 1]

DEFPARAMETER($msg_hash, nil)

cacheKeyedMsg1(in_file) ==
    key := nil
    line := '""
    msg := '""
    repeat
        line := READ_-LINE(in_file, nil, nil)
        null(line) =>
            if key then HPUT($msg_hash, key, msg)
            -- THROW('DONE, nil)
            return nil
        #line = 0 => "iterate"
        line.0 = char('"S") =>
            if key then HPUT($msg_hash, key, msg)
            key := INTERN(line, "BOOT")
            msg := '""
        msg := CONCAT(msg, line)

cacheKeyedMsg(db_name) ==
    CATCH('DONE, handle_input_file(db_name, function cacheKeyedMsg1, []))

getKeyedMsg(key) ==
    STRINGP(key) => key
    if not($msg_hash) then
        $msg_hash := MAKE_HASHTABLE('EQ)
        cacheKeyedMsg($defaultMsgDatabaseName)
    HGET($msg_hash, key)

--% Formatting and Printing Keyed Messages

segmentKeyedMsg(msg) == string2Words msg

segmentedMsgPreprocess x ==
  ATOM x => x
  [head,:tail] := x
  center := rightJust := NIL
  if head in '(%ceon "%ceon") then center := true
  if head in '(%rjon "%rjon") then rightJust := true
  center or rightJust =>
    -- start collecting terms
    y := NIL
    ok := true
    while tail and ok repeat
      [t,:tail] := tail
      t in '(%ceoff "%ceoff" %rjoff "%rjoff") => ok := NIL
      y := CONS(segmentedMsgPreprocess t,y)
    head1 := [(center => '"%ce"; '"%rj"),:NREVERSE y]
    NULL tail => [head1]
    [head1,:segmentedMsgPreprocess tail]
  head1 := segmentedMsgPreprocess head
  tail1 := segmentedMsgPreprocess tail
  EQ(head,head1) and EQ(tail,tail1) => x
  [head1,:tail1]

removeAttributes msg ==
    --takes a segmented message and returns it with the attributes
    --separted.
    first msg ~= '"%atbeg" =>
        [msg,NIL]
    attList := []
    until item = '"%atend" repeat
        msg     := rest  msg
        item    := first msg
        attList := [INTERN item,:attList]
    msg := rest msg
    attList := rest attList
    [msg,attList]

substituteSegmentedMsg(msg,args) ==
  -- this does substitution of the parameters
  l := NIL
  nargs := #args
  for x in segmentedMsgPreprocess msg repeat
    -- x is a list
    PAIRP x =>
      l := cons(substituteSegmentedMsg(x,args),l)
    c := x.0
    n := STRINGLENGTH x

    -- x requires parameter substitution
    (x.0 = char "%") and (n > 1) and (DIGITP x.1) =>
      a := DIG2FIX x.1
      arg :=
        a <= nargs => args.(a-1)
        '"???"
      -- now pull out qualifiers
      q := NIL
      for i in 2..(n-1) repeat q := cons(x.i,q)
      -- Note 'f processing must come first.
      if MEMQ(char 'f,q) then
          arg :=
              PAIRP arg => APPLY(first arg, rest arg)
              arg
      if MEMQ(char 'm,q) then arg := [['"%m",:arg]]
      if MEMQ(char 's,q) then arg := [['"%s",:arg]]
      if MEMQ(char 'p,q) then
          arg := escape_strings(prefix2String arg)
      if MEMQ(char 'P,q) then
          arg := [prefix2String x for x in arg]

      if MEMQ(char 'c,q) then arg := [['"%ce",:arg]]
      if MEMQ(char 'r,q) then arg := [['"%rj",:arg]]

      if MEMQ(char 'l,q) then l := cons('"%l",l)
      if MEMQ(char 'b,q) then l := cons('"%b",l)
      --we splice in arguments that are lists
      --if y is not specified, then the adding of blanks is
      --stifled after the first item in the list until the
      --end of the list. (using %n and %y)
      l :=
         PAIRP(arg) =>
           MEMQ(char 'y, q) or (first arg = '"%y") or ((LENGTH arg) = 1)  =>
             APPEND(REVERSE arg, l)
           head := first arg
           tail := rest arg
           ['"%y",:APPEND(REVERSE tail, ['"%n",head,:l ]) ]
         cons(arg,l)
      if MEMQ(char 'b,q) then l := cons('"%d",l)
      for ch in '(_. _, _! _: _; _?) repeat
        if MEMQ(char ch,q) then l := cons(ch,l)

    --x is a plain word
    l := cons(x,l)
  addBlanks NREVERSE l

addBlanks msg ==
  -- adds proper blanks
  null PAIRP msg => msg
  null msg => msg
  LENGTH msg = 1 => msg
  blanksOff := false
  x := first msg
  if x = '"%n" then
    blanksOff := true
    msg1 := []
  else
    msg1 := LIST x
  blank := '" "
  for y in rest msg repeat
    y in '("%n" %n) => blanksOff := true
    y in '("%y" %y) => blanksOff  := false
    if noBlankAfterP x or noBlankBeforeP y or blanksOff then
       msg1 := [y,:msg1]
    else
       msg1 := [y,blank,:msg1]
    x := y
  NREVERSE msg1


DEFPARAMETER($msgdbPrims, '( %b %d %l %i %u %U %n %x %ce %rj "%U" "%b" "%d" "%l" "%i" "%u" "%U" "%n" "%x" "%ce" "%rj"))
DEFPARAMETER($msgdbPunct, '(_. _, _! _: _; _] _)  "." "," "!" ":" ";" "]" ")"  ))
DEFPARAMETER($msgdbNoBlanksBeforeGroup, ['" ", " ", '"%", "%",_
                            :$msgdbPrims, :$msgdbPunct])
DEFPARAMETER($msgdbListPrims, '(%m %s %ce %rj "%m" "%s" "%ce" "%rj"))

noBlankBeforeP word==
    INTEGERP word => false
    word in $msgdbNoBlanksBeforeGroup => true
    if STRINGP word and SIZE word > 1 then
       word.0 = char '% and word.1 = char 'x => return true
       word.0 = char " " => return true
    (PAIRP word) and (first word in $msgdbListPrims) => true
    false

$msgdbPunct := '(_[ _(  "[" "(" )
DEFPARAMETER($msgdbNoBlanksAfterGroup, ['" ", " ",'"%" ,"%",_
                          :$msgdbPrims,:$msgdbPunct])

noBlankAfterP word==
    INTEGERP word => false
    word in $msgdbNoBlanksAfterGroup => true
    if STRINGP word and (s := SIZE word) > 1 then
       word.0 = char '% and word.1 = char 'x => return true
       word.(s-1) = char " " => return true
    (PAIRP word) and (first word in $msgdbListPrims) => true
    false

cleanUpSegmentedMsg msg ==
  -- removes any junk like double blanks
  -- takes a reversed msg and puts it in the correct order
  null PAIRP msg => msg
  blanks := ['" "," "]
  haveBlank := NIL
  prims :=
    '(%b %d %l %i %u %m %ce %rj _
     "%b" "%d" "%l" "%i" "%m" "%u" "%ce" "%rj")
  msg1 := NIL
  for x in msg repeat
    if haveBlank and ((x in blanks) or (x in prims)) then
      msg1 := rest msg1
    msg1 := cons(x,msg1)
    haveBlank := (x in blanks => true; NIL)
  msg1

----------------------------------------
sayPatternMsg(msg,args) ==
  ioHook("startPatternMsg", msg, args)
  msg := segmentKeyedMsg msg
  msg := substituteSegmentedMsg(msg,args)
  sayMSG flowSegmentedMsg(msg,$LINELENGTH,3)
  ioHook("endPatternMsg", msg)

throwPatternMsg(key,args) ==
  sayMSG '" "
  if $testingSystem then sayMSG $testingErrorPrefix
  sayPatternMsg(key,args)
  spadThrow()

say_msg(key, msg, args) ==
  ioHook("startKeyedMsg", key, args)
  say_msg_local(msg, args)
  ioHook("endOfKeyedMsg", key)

sayKeyedMsg(key, args) == say_msg(key, getKeyedMsg(key), args)

say_msg_local(msg, args) ==
  msg := segmentKeyedMsg msg
  msg := substituteSegmentedMsg(msg,args)
  msg' := flowSegmentedMsg(msg,$LINELENGTH,$MARGIN)
  if $printMsgsToFile then sayMSG2File msg'
  sayMSG msg'

throw_error_msg(kind, key, msg, args) == throw_msg(key, msg, args)

throwKeyedErrorMsg(kind, key, args) ==
    throw_error_msg(kind, key, getKeyedMsg(key), args)

throw_msg_pos(key, msg, args, tree) ==
    if tree and (sp := getSrcPos(tree)) then
        sayMSG '" "
        srcPosDisplay(sp)
    throw_msg(key, msg, args)

throwKeyedMsgSP(key, args, atree) ==
    throw_msg_pos(key, getKeyedMsg(key), args, atree)

throwKeyedMsg(key, args) ==
    throw_msg(key, getKeyedMsg(key), args)

throw_msg(key, msg, args) ==
    $noEvalTypeMsg => spadThrow()
    sayMSG '" "
    if $testingSystem then sayMSG $testingErrorPrefix
    say_msg(key, msg, args)
    spadThrow()

throwListOfKeyedMsgs(descKey,descArgs,l) ==
  -- idea is that descKey and descArgs are the message describing
  -- what the list is about and l is a list of [key,args] messages
  -- the messages in the list are numbered and should have a %1 as
  -- the first token in the message text.
  sayMSG '" "
  if $testingSystem then sayMSG $testingErrorPrefix
  sayKeyedMsg(descKey,descArgs)
  sayMSG '" "
  for [key,args] in l for i in 1.. repeat
    n := STRCONC(object2String i,'".")
    sayKeyedMsg(key,[n,:args])
  spadThrow()

--  breakKeyedMsg is like throwKeyedMsg except that the user is given
--  a chance to play around in a break loop if $BreakMode is not 'nobreak

break_msg(key, msg, args) ==
    say_msg(key, msg, args)
    handleLispBreakLoop($BreakMode)

system_error(key, msg, args) ==
    say_msg("S2GE0000", '"Internal Error", NIL)
    break_msg(key, msg, args)

keyedSystemError(key, args) == system_error(key, getKeyedMsg(key), args)

systemErrorHere(fname) ==
    system_error("S2GE0017",
                 '"Unexpected error in call to system function %1b", [fname])

queryUserKeyedMsg(key, args) == query_user_msg(key, getKeyedMsg(key), args)

query_user_msg(key, msg, args) ==
  -- display message and return reply
  conStream := get_console_input()
  say_msg(key, msg, args)
  ioHook("startQueryUser")
  ans := read_line conStream
  ioHook("endOfQueryUser")
  ans

queryUserKeyedMsg(key, args) == query_user_msg(key, getKeyedMsg(key), args)

flowSegmentedMsg(msg, len, offset) ==
  -- tries to break a sayBrightly-type input msg into multiple
  -- lines, with offset and given length.
  -- msgs that are entirely centered or right justified are not flowed
  msg is [[ce,:.]] and ce in '(%ce "%ce" %rj "%rj") => msg

  -- msgs that are entirely centered are not flowed
  msg is [[ce, :.]] and ce in '(%ce "%ce") => msg

  potentialMarg := 0
  actualMarg    := 0

  off := (offset <= 0 => '""; filler_spaces(offset))
  off1:= (offset <= 1 => '""; filler_spaces(offset - 1))
  firstLine := true

  PAIRP msg =>
    lnl := offset
    if msg is [a,:.] and a in '(%b %d _  "%b" "%d" " ") then
      nl :=  [off1]
      lnl := lnl - 1
    else nl := [off]
    for f in msg repeat
      f in '("%l" %l) =>
        actualMarg := potentialMarg
        if lnl = 99999 then nl := ['%l,:nl]
        lnl := 99999
      PAIRP(f) and first(f) in '("%m" %m '%ce "%ce" %rj "%rj") =>
        actualMarg := potentialMarg
        nl := [f,'%l,:nl]
        lnl := 199999
      f in '("%i" %i ) =>
        potentialMarg := potentialMarg + 3
        nl := [f,:nl]
      PAIRP(f) and first(f) in '("%t" %t) =>
        potentialMarg := potentialMarg + rest f
        nl := [f,:nl]
      sbl := sayBrightlyLength f
      tot := lnl + offset + sbl + actualMarg
      if firstLine then
        firstLine  := false
        offset := offset + offset
        off1   := STRCONC(off, off1)
        off    := STRCONC(off, off)
      if (tot <= len) or (sbl = 1 and tot = len) then
        nl := [f,:nl]
        lnl := lnl + sbl
      else
        f in '(%b %d _  "%b" "%d" " ") =>
          nl := [f,off1,'%l,:nl]
          actualMarg := potentialMarg
          lnl := -1 + offset + sbl
        nl := [f,off,'%l,:nl]
        lnl := offset + sbl
    concat nreverse nl
  concat('%l,off,msg)

--% Other handy things

msg_comp_failure1(key, msg, args, tree) ==
  -- Called when compilation fails in such a way that interpret-code
  --  mode might be of some use.
  not $useCoerceOrCroak =>   THROW('coerceOrCroaker, 'croaked)
  if not($Coerce) and  $reportInterpOnly then
      if tree and  (sp := getSrcPos(tree)) then
          sayMSG '" "
          srcPosDisplay(sp)
      say_msg(key, msg, args)
      say_msg("S2IB0009",
          '"FriCAS will attempt to step through and interpret the code.", [])
  null $compilingMap => THROW('loopCompiler,'tryInterpOnly)
  THROW('mapCompiler,'tryInterpOnly)

msg_comp_failure(key, msg, args) == msg_comp_failure1(key, msg, args, [])

keyedMsgCompFailure(key, args) ==
    msg_comp_failure(key, getKeyedMsg(key), args)

keyedMsgCompFailureSP(key, args, atree) ==
    msg_comp_failure1(key, getKeyedMsg(key), args, atree)

throwKeyedMsgCannotCoerceWithValue(val,t1,t2) ==
  val' :=
     not($genValue) => nil
     coerceInteractive(mkObj(val,t1),$OutputForm)
  if not(isWrapped(val')) then val' := nil
  null (val') => throw_msg("S2IC0002",
        '"Cannot convert the value from type %1bp to %2bp .", [t1, t2])
  val' := objValUnwrap(val')
  throw_msg("S2IC0003",
            '"Cannot convert from type %1bp to %2bp for value %3m",
            [t1, t2, val'])

--% Some Standard Message Printing Functions

bright x == ['"%b", :(PAIRP(x) and NULL rest LASTNODE x => x; [x]), '"%d"]

mkMessage msg ==
  msg and (PAIRP msg) and ((first msg) in '(%l "%l"))  and
    ((last msg) in '(%l "%l")) => concat msg
  concat('%l,msg,'%l)

sayMessage msg == sayMSG mkMessage msg

sayNewLine() == TERPRI()

say_new_line(str) ==
  -- Note: this function should *always* be used by sayBrightly and
  -- friends rather than TERPRI
  TERPRI(str)

sayString(x, str) ==
  -- Note: this function should *always* be used by sayBrightly and
  -- friends rather than PRINTEXP
  PRINTEXP(x, str)

spadStartUpMsgs() ==
  -- messages displayed when the system starts up
  $LINELENGTH < 60 => NIL
  bar := filler_chars($LINELENGTH, hbar_special_char())
  say_msg("S2GL0001", CONCAT(
      '"%ceon %b FriCAS Computer Algebra System %d %l",
      '" Version: %1 built with %2 %l Timestamp: %3 %ceoff"),
      [$build_version, $lisp_id_string, $build_date])
  sayMSG bar
  say_msg("S2GL0018C",
      '"Issue %b )copyright %d to view copyright notices.", nil)
  say_msg("S2GL0018D",
      '"Issue %b )summary %d for a summary of useful system commands.", nil)
  say_msg("S2GL0003B",
      '"Issue %b )quit %d to leave FriCAS and return to %1 .", [$opSysName])
  sayMSG bar
  sayMSG '" "

--% Some Advanced Formatting Functions

brightPrint(x, str) ==
  marg := 0
  for y in x repeat
      marg := brightPrint0(y, str, marg)
  NIL

brightPrint0(x, str, marg) ==
  if IDENTP x then x := PNAME x

  -- if the first character is a backslash and the second is a percent sign,
  -- don't try to give the token any special interpretation. Just print
  -- it without the backslash.

  STRINGP x and STRINGLENGTH x > 1 and x.0 = char "\" and x.1 = char "%" =>
      sayString(SUBSTRING(x, 1, NIL), str)
      marg
  x = '"%l" =>
      say_new_line(str)
      for i in 1..marg repeat
          sayString('" ", str)
      marg
  x = '"%i" =>
      marg + 3
  x = '"%u" =>
      marg := marg - 3
      marg < 0 => 0
      marg
  x = '"%U" =>
      0
  x = '"%" =>
      sayString('" ", str)
      marg
  x = '"%%" =>
      sayString('"%", str)
      marg
  x = '"%b" =>
      NULL $highlightAllowed =>
          sayString('" ", str)
          marg
      sayString($highlightFontOn, str)
      marg
  k := blankIndicator x =>
      BLANKS(k, str)
      marg
  x = '"%d" =>
      NULL $highlightAllowed =>
          sayString('" ", str)
          marg
      sayString($highlightFontOff, str)
      marg
  STRINGP x =>
      sayString(x, str)
      marg
  brightPrintHighlight(x, str, marg)

blankIndicator x ==
  if IDENTP x then x := PNAME x
  null STRINGP x or MAXINDEX x < 1 => nil
  x.0 = char '% and x.1 = char 'x =>
    MAXINDEX x > 1 => PARSE_-INTEGER SUBSTRING(x,2,nil)
    1
  nil

brightPrint1(x, str, marg) ==
  if x in '(%l "%l") then say_new_line(str)
  else if STRINGP x then sayString(x, str)
       else marg := brightPrintHighlight(x, str, marg)
  marg

brightPrintHighlight(x, str, marg) ==
  IDENTP x =>
    pn := PNAME x
    sayString(pn, str)
    marg
  -- following line helps find certain bugs that slip through
  -- also see sayBrightlyLength1
  VECP x =>
      sayString('"UNPRINTABLE", str)
      marg
  ATOM x =>
      sayString(object2String(x), str)
      marg
  [key,:rst] := x
  if IDENTP key then key:=PNAME key
  key = '"%m" =>
      mathprint rst
      marg
  key in '("%p" "%s") =>
      PRETTYPRIN0(rst, str)
      marg
  key = '"%ce" => brightPrintCenter(rst, str, marg)
  key = '"%rj" => brightPrintRightJustify(rst, str, marg)
  key = '"%t"  => marg + tabber(rst)
  sayString('"(", str)
  marg := brightPrint1(key, str, marg)
  if EQ(key,'TAGGEDreturn) then
    rst := [first rst, CADR rst, CADDR rst, '"environment (omitted)"]
  for y in rst repeat
    sayString('" ", str)
    marg := brightPrint1(y, str, marg)
  if rst and (la := LASTATOM rst) then
    sayString('" . ", str)
    marg := brightPrint1(la, str, marg)
  sayString('")", str)
  marg

tabber num ==
    maxTab := 50
    num > maxTab => maxTab
    num

brightPrintCenter(x, str, marg) ==
  -- centers rst within $LINELENGTH, checking for %l's
  ATOM x =>
    x := object2String x
    wid := STRINGLENGTH x
    if wid < $LINELENGTH then
      f := DIVIDE($LINELENGTH - wid,2)
      x := LIST(filler_spaces(f.0), x)
    for y in x repeat
        marg := brightPrint0(y, str, marg)
    marg
  y := NIL
  ok := true
  while x and ok repeat
    if first(x) in '(%l "%l") then ok := NIL
    else y := cons(first x, y)
    x := rest x
  y := NREVERSE y
  wid := sayBrightlyLength y
  if wid < $LINELENGTH then
    f := DIVIDE($LINELENGTH - wid,2)
    y := CONS(filler_spaces(f.0), y)
  for z in y repeat
      marg := brightPrint0(z, str, marg)
  if x then
      say_new_line(str)
      marg := brightPrintCenter(x, str, marg)
  marg

brightPrintRightJustify(x, str, marg) ==
  -- right justifies rst within $LINELENGTH, checking for %l's
  ATOM x =>
    x := object2String x
    wid := STRINGLENGTH x
    wid < $LINELENGTH =>
      x := LIST(filler_spaces($LINELENGTH - wid), x)
      for y in x repeat
          marg := brightPrint0(y, str, marg)
      marg
    brightPrint0(x, str, marg)
  y := NIL
  ok := true
  while x and ok repeat
    if first(x) in '(%l "%l") then ok := NIL
    else y := cons(first x, y)
    x := rest x
  y := NREVERSE y
  wid := sayBrightlyLength y
  if wid < $LINELENGTH then
    y := CONS(filler_spaces($LINELENGTH - wid), y)
  for z in y repeat
      marg := brightPrint0(z, str, marg)
  if x then
    say_new_line(str)
    marg := brightPrintRightJustify(x, str, marg)
  marg

--% Message Formatting Utilities

sayBrightlyLength l ==
  null l => 0
  atom l => sayBrightlyLength1 l
  sayBrightlyLength1 first l + sayBrightlyLength rest l

sayBrightlyLength1 x ==
  member(x,'("%b" "%d" %b %d)) =>
    NULL $highlightAllowed => 1
    1
  member(x,'("%l" %l)) => 0
  STRINGP x and STRINGLENGTH x > 2 and x.0 = char "%" and x.1 = char "x" =>
      DIGITP(x.2)
  STRINGP x => STRINGLENGTH x
  IDENTP x => STRINGLENGTH PNAME x
  -- following line helps find certain bugs that slip through
  -- also see brightPrintHighlight
  VECP x => STRINGLENGTH '"UNPRINTABLE"
  ATOM x => STRINGLENGTH STRINGIMAGE x
  2 + sayBrightlyLength x

sayAsManyPerLineAsPossible l ==
  -- it is assumed that l is a list of strings
  l := [atom2String a for a in l]
  m := 1 + "MAX"/[SIZE(a) for a in l]
  -- w will be the field width in which we will display the elements
  m > $LINELENGTH =>
    for a in l repeat sayMSG a
    NIL
  w := MIN(m + 3,$LINELENGTH)
  -- p is the number of elements per line
  p := QUOTIENT($LINELENGTH,w)
  n := # l
  str := '""
  for i in 0..(n-1) repeat
    [c,:l] := l
    str := STRCONC(str,c,filler_spaces(w - #c))
    REMAINDER(i+1,p) = 0 => (sayMSG str ; str := '"" )
  if str ~= '"" then sayMSG str
  NIL

say2PerLine l == say2PerLineWidth(l, QUOTIENT($LINELENGTH, 2))

say2PerLineWidth(l,n) ==
  [short,long] := say2Split(l,nil,nil,n)
  say2PerLineThatFit short
  for x in long repeat sayLongOperation x
  sayBrightly '""

say2Split(l,short,long,width) ==
  l is [x,:l'] =>
    sayWidth x < width => say2Split(l',[x,:short],long,width)
    say2Split(l',short,[x,:long],width)
  [nreverse short,nreverse long]

sayLongOperation x ==
  sayWidth x > $LINELENGTH and (splitListOn(x,"if") is [front,back]) =>
    sayBrightly front
    BLANKS (6 + # PNAME front.1)
    sayBrightly back
  sayBrightly x

splitListOn(x,key) ==
  key in x =>
    while first x ~= key repeat
      y:= [first x,:y]
      x:= rest x
    [nreverse y,x]
  nil

say2PerLineThatFit l ==
  while l repeat
    sayBrightlyNT first l
    sayBrightlyNT
      filler_spaces((QUOTIENT($LINELENGTH, 2) - sayDisplayWidth first l))
    (l:= rest l) =>
      sayBrightlyNT first l
      l:= rest l
      sayBrightly '""
    sayBrightly '""

sayDisplayWidth x ==
  PAIRP x =>
    +/[fn y for y in x] where fn y ==
      y in '(%b %d "%b" "%d") => 1
      k := blankIndicator y => k
      sayDisplayWidth y
  x = "%%" or x = '"%%" => 1
  # atom2String x

sayWidth x ==
  atom x => # atom2String x
  +/[fn y for y in x] where fn y ==
    sayWidth y

pp2Cols(al) ==
  while al repeat
    [[abb,:name],:al]:= al
    ppPair(abb,name)
    if canFit2ndEntry(name,al) then
      [[abb,:name],:al]:= al
      TAB (QUOTIENT($LINELENGTH, 2))
      ppPair(abb,name)
    sayNewLine()
  nil

ppPair(abb,name) ==
    sayBrightlyNT([:bright(abb), filler_spaces(8 - entryWidth(abb)), name])

canFit2ndEntry(name,al) ==
  wid := QUOTIENT($LINELENGTH, 2) - 10
  null al => nil
  entryWidth name > wid => nil
  entryWidth CDAR al > wid => nil
  'T

entryWidth x == # atom2String x

center80 text == centerNoHighlight(text,$LINELENGTH,'" ")

centerAndHighlight(text,:argList) ==
  width := IFCAR argList or $LINELENGTH
  fillchar := IFCAR IFCDR argList or '" "
  wid := entryWidth text + 2
  wid >= width - 2 => sayBrightly ['%b,text,'%d]
  f := DIVIDE(width - wid - 2,2)
  fill1 := '""
  for i in 1..(f.0) repeat
    fill1 := STRCONC(fillchar,fill1)
  if f.1 = 0 then fill2 := fill1 else fill2 := STRCONC(fillchar,fill1)
  sayBrightly [fill1,'%b,text,'%d,fill2]
  nil

centerNoHighlight(text,:argList) == sayBrightly center(text,argList)

center(text,argList) ==
  width := IFCAR argList or $LINELENGTH
  fillchar := IFCAR IFCDR argList or '" "
  if (u:= splitSayBrightlyArgument text) then [text,:moreLines]:= u
  wid := sayBrightlyLength text
  wid >= width - 2 => sayBrightly text
  f := DIVIDE(width - wid - 2,2)
  fill1 := '""
  for i in 1..(f.0) repeat
    fill1 := STRCONC(fillchar,fill1)
  if f.1 = 0 then fill2 := fill1 else fill2 := STRCONC(fillchar,fill1)
  concat(fill1,text,fill2)

splitSayBrightly u ==
  width:= 0
  while u and (width:= width + sayWidth first u) < $LINELENGTH repeat
    segment:= [first u,:segment]
    u := rest u
  null u => NREVERSE segment
  segment => [:NREVERSE segment,"%l",:splitSayBrightly(u)]
  u

splitSayBrightlyArgument u ==
  atom u => nil
  while splitListSayBrightly u is [head,:u] repeat result:= [head,:result]
  result => [:NREVERSE result,u]
  [u]

splitListSayBrightly u ==
  for x in tails u repeat
    y := rest x
    null y => nil
    first y = '%l =>
      RPLACD(x,nil)
      ans:= [u,:rest y]
  ans

-- Output to Character Streams

sayBrightlyNT2(x, str) ==
    NULL(x) => nil
    $sayBrightlyStream => sayBrightlyNT1(x, $sayBrightlyStream)
    sayBrightlyNT1(x, str)

sayBrightlyNT1(x, str) ==
    if x then
        ATOM(x) =>
            brightPrint0(x, str, 0)
        brightPrint(x, str)

sayBrightlyNT(x) == sayBrightlyNT2(x, get_lisp_std_out())

sayBrightly2(x, str) ==
    NULL(x) => nil
    $sayBrightlyStream => sayBrightly1(x, $sayBrightlyStream)
    sayBrightly1(x, str)

sayBrightly1(x, str) ==
    if x then
        sayBrightlyNT1(x, str)
        TERPRI(str)
        FORCE_-OUTPUT(str)

sayBrightly(x) == sayBrightly2(x,  get_lisp_std_out())

sayBrightlyI(x) ==
    NULL(x) => nil
    sayBrightly1(x, get_lisp_error_out())

sayMSGNT(x) == sayBrightlyNT1(x, get_algebra_stream())

say_simple(x, str) ==
    if x then
        if true then
            ATOM(x) => sayString(x, str)
            for y in x repeat
                sayString(y, str)
        TERPRI(str)

sayHtml(x) == say_simple(x, get_html_stream())

sayMathML(x) == say_simple(x, get_mathml_stream())

sayTeX(x) == say_simple(x, get_tex_stream())

sayTexmacs(x) == say_simple(x, get_texmacs_stream())

saySpadMsg(x) == sayBrightly1(x, get_algebra_stream())

sayALGEBRA(x) == sayBrightly1(x, get_algebra_stream())

sayMSG(x) == sayBrightly1(x, get_algebra_stream())

sayFormatted(x) == say_simple(x, get_formatted_stream())

sayMSG2File(msg) ==
    file := make_filename2('"spadmsg", '"listing")
    str := MAKE_OUTSTREAM(file)
    sayBrightly1(msg, str)
    SHUT(str)

--=======================================================================
--                Utility Functions
--=======================================================================

$htSpecialChars := ['"_#", '"[", '"]", '"%", '"{", '"}", '"_\",
                    '"$", '"&", '"^", '"__", '"_~"]

$htCharAlist := '(
  ("$"  . "\%")
  ("[]" . "\[\]")
  ("{}" . "\{\}")
  ("\\" . "\\\\")
  ("\/" . "\\/" )
  ("/\" . "/\\" ) )

escapeSpecialChars s ==
  u := LASSOC(s,$htCharAlist) => u
  member(s, $htSpecialChars) => STRCONC('"_\", s)
  s
