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



$compBugPrefix :=      '"Bug!"
$compErrorPrefix :=    '"Error"

--error message facility
$nopos   := ['noposition]

-- Items from MSG BOOT I
$preLength := 11
$LOGLENGTH := $LINELENGTH - 6

$imPrTagGuys := ['unimple, 'bug, 'debug, 'say, 'warn]
$toWhereGuys := ['fileOnly, 'screenOnly ]
$imPrGuys    := ['imPr]


$ncMsgList := nil


--%  Messages for the USERS of the compiler.
-- The program being compiled has a minor error.
-- Give a message and continue processing.
ncSoftError(pos, erMsgKey, erArgL) ==
    processKeyedError(
       msgCreate('error, pos, erMsgKey, erArgL, $compErrorPrefix))

-- The program being compiled is seriously incorrect.
-- Give message and throw to a recovery point.
ncHardError(pos, erMsgKey, erArgL) ==
    processKeyedError(
          msgCreate('error, pos, erMsgKey, erArgL, $compErrorPrefix))
    ncError()

-- Bug in the compiler: something which shouldn't have happened did.
ncBug(erMsgKey, erArgL) ==
  processKeyedError(
        msgCreate('bug, $nopos, erMsgKey, erArgL, $compBugPrefix))
  -- The next line is to try to deal with some reported cases of unwanted
  -- backtraces appearing, MCD.
  ENABLE_BACKTRACE(nil)
  BREAK()
  ncAbort()

--% Lower level functions

--msgObject  tag -- category of msg
--                    -- attributes as a-list
--                        'imPr  => dont save for list processing
--                        toWhere, screen or file
--                        'norep => only display once in list
--           pos -- position with possible FROM/TO tag
--           key -- key for message database
--          argL -- arguments to be placed in the msg test
--        prefix -- things like "Error: "
--          text -- the actual text

msgCreate(tag,posWTag,key,argL,optPre) ==
    msg := [tag,posWTag,key,argL,optPre,NIL]
    putDatabaseStuff msg
    initImPr    msg
    initToWhere msg
    msg

processKeyedError msg ==
    msgImPr? msg =>
      msgOutputter msg
    $ncMsgList := cons (msg, $ncMsgList)

---------------------------------
--%getting info from db.
putDatabaseStuff msg ==
    text := getMsgInfoFromKey msg
    setMsgText(msg,text)

getMsgInfoFromKey msg ==
    text :=
        msgKey := getMsgKey? msg =>   --temp  oldmsgs use key tostoretext
           getKeyedMsg msgKey
        getMsgKey msg                  --temp oldmsgs
    text := segmentKeyedMsg(text)
    substituteSegmentedMsg(text, getMsgArgL(msg))


-----------------------
--%character position marking

processChPosesForOneLine msgList ==
    chPosList := posPointers msgList
    for msg in msgList repeat
        if getMsgFTTag? msg then
            putFTText (msg,chPosList)
        posLetter := rest ASSOC(poCharPosn getMsgPos msg, chPosList)
        oldPre := getMsgPrefix msg
        setMsgPrefix (msg,STRCONC(oldPre,_
                     filler_spaces($preLength - 4 - SIZE(oldPre)), posLetter))
    leaderMsg := makeLeaderMsg chPosList
    NCONC(msgList,LIST leaderMsg)  --a back cons

posPointers msgList ==
--gets all the char posns for msgs on one line
--associates them with a uppercase letter
    pointers  := '"ABCDEFGHIJKLMONPQRS"
    increment := 0
    posList:= []
    ftPosList := []
    for msg in msgList repeat
       pos := poCharPosn getMsgPos msg
       if pos ~= IFCAR posList then
         posList := [pos,:posList]
       if getMsgFTTag?(msg) = 'FROMTO then
         ftPosList := [poCharPosn getMsgPos2 msg,:ftPosList]
    for toPos in ftPosList repeat
           posList := insertPos(toPos,posList)
    for pos in posList repeat
        posLetterList := [[pos,:pointers.increment],:posLetterList]
        increment := increment + 1
    posLetterList

insertPos(newPos,posList) ==
--inserts a position in the proper place of a position list
--used for the 2nd pos of a fromto
    done := false
    bot  := [0,:posList]
    top  := []
    while not done repeat
        top  := [first bot, :top]
        bot  := rest bot
        NULL(bot) =>
           top := [newPos,:top]
           done := true
        pos  := first bot
        done :=
          pos < newPos => false
          pos = newPos => true
          pos > newPos =>
            top := [newPos,:top]
            true
    for pp in top repeat
        bot := [pp, :bot]
    rest bot

putFTText (msg,chPosList) ==
    tag := getMsgFTTag? msg
    pos := poCharPosn getMsgPos msg
    charMarker := rest ASSOC(pos, chPosList)
    tag = 'FROM =>
        markingText := ['"(from ",charMarker,'" and on) "]
        setMsgText(msg,[:markingText,:getMsgText msg])
    tag = 'TO =>
        markingText := ['"(up to ",charMarker,'") "]
        setMsgText(msg,[:markingText,:getMsgText msg])
    tag = 'FROMTO =>
       pos2 := poCharPosn getMsgPos2 msg
       charMarker2 := rest ASSOC(pos2, chPosList)
       markingText := ['"(from ",charMarker,'" up to ",_
           charMarker2,'") "]
       setMsgText(msg,[:markingText,:getMsgText msg])

--called from parameter list of nc message functions
position_from(pos) == ['FROM,   pos]
position_to(pos) == ['TO,     pos]
position_from_to(pos1, pos2) == ['FROMTO, pos1, pos2]

------------------------
--%processing error lists
processMsgList(erMsgList, lineList) ==
    $outputList : local := []--grows in queueUp errors
    erMsgList  := erMsgSort erMsgList
    for line in lineList repeat
        msgLine := makeMsgFromLine line
        $outputList := [msgLine,:$outputList]
        globalNumOfLine := poGlobalLinePosn getMsgPos msgLine
        erMsgList :=
             queueUpErrors(globalNumOfLine,erMsgList)
    $outputList := append(erMsgList,$outputList)  --the nopos's
    listOutputter reverse $outputList

erMsgSort erMsgList ==
    [msgWPos,msgWOPos] := erMsgSep erMsgList
    msgWPos  := listSort(function erMsgCompare, msgWPos)
    msgWOPos := reverse msgWOPos
    [:msgWPos,:msgWOPos]

erMsgCompare(ob1,ob2)==
    pos1 :=  getMsgPos ob1
    pos2 :=  getMsgPos ob2
    compareposns(pos2,pos1)

erMsgSep erMsgList ==
    msgWPos  := []
    msgWOPos := []
    for msg in erMsgList repeat
        if poNopos? getMsgPos msg then
          msgWOPos := [msg,:msgWOPos]
        else
          msgWPos  := [msg,:msgWPos]
    [msgWPos,msgWOPos]

getLinePos line  == first line
getLineText line == rest line

queueUpErrors(globalNumOfLine,msgList)==
    thisPosMsgs := []
    notThisPosMsgs := []
    for msg in msgList _
      while thisPosIsLess(getMsgPos msg,globalNumOfLine) repeat
    --these are msgs that refer to positions from earlier compilations
        if not redundant (msg,notThisPosMsgs) then
           notThisPosMsgs := [msg,:notThisPosMsgs]
        msgList := rest msgList
    for msg in msgList _
      while thisPosIsEqual(getMsgPos msg,globalNumOfLine) repeat
       if not redundant (msg,thisPosMsgs) then
           thisPosMsgs := [msg,:thisPosMsgs]
       msgList := rest msgList
    if thisPosMsgs then
        thisPosMsgs := processChPosesForOneLine  thisPosMsgs
        $outputList := NCONC(thisPosMsgs,$outputList)
    if notThisPosMsgs then
        $outputList := NCONC(notThisPosMsgs,$outputList)
    msgList

redundant(msg, thisPosMsgs) == MEMBER(msg, thisPosMsgs)

thisPosIsLess(pos,num) ==
    poNopos? pos => NIL
    poGlobalLinePosn pos < num

thisPosIsEqual(pos,num) ==
    poNopos? pos => NIL
    poGlobalLinePosn pos = num

--%outputting stuff

listOutputter outputList ==
    for msg in outputList repeat
        msgOutputter msg

msgOutputter msg  ==
    st := getStFromMsg msg
    shouldFlow := not (leader? msg or line? msg)
    if toScreen? msg then
       if shouldFlow then
          st := flowSegmentedMsg(st,$LINELENGTH,0)
       sayBrightly st
    if toFile? msg then
       if shouldFlow then
          st := flowSegmentedMsg(st,$LOGLENGTH,0)
       alreadyOpened := alreadyOpened? msg

toScreen? msg ==  getMsgToWhere msg ~= 'fileOnly

toFile? msg   ==
     getMsgToWhere msg ~= 'screenOnly

alreadyOpened? msg ==
       not msgImPr? msg

getStFromMsg msg ==
    preStL := getPreStL getMsgPrefix? msg
    getMsgTag  msg = 'line =>
          ['"%x1" , :preStL,_
           getMsgText msg]
    posStL := getPosStL msg
    [posStL, getMsgLitSym(msg), :preStL, tabbing(msg), :getMsgText(msg)]

tabbing msg ==
    chPos := 2
    if getMsgPrefix? msg then
      chPos := chPos + $preLength - 1
    ["%t",:chPos]

getPosStL msg ==
    not showMsgPos? msg => '""
    msgPos := getMsgPos msg
    howMuch :=
        msgImPr? msg =>
            decideHowMuch (msgPos,$lastPos)
        listDecideHowMuch (msgPos,$lastPos)
    $lastPos := msgPos
    fullPrintedPos := ppos msgPos
    printedFileName :=  ['"%x2",'"[",:remLine fullPrintedPos,'"]" ]
    printedLineNum  :=  ['"%x2",'"[",:remFile fullPrintedPos,'"]" ]
    printedOrigin   :=  ['"%x2",'"[",:fullPrintedPos,'"]" ]
    howMuch  = 'ORG  => [:printedOrigin, '%l]
    howMuch  = 'LINE => [:printedLineNum, '%l]
    howMuch  = 'FILE => [:printedFileName, '%l]
    howMuch  = 'ALL  => [:printedFileName, '%l,_
                         :printedLineNum,  '%l]
    '""

showMsgPos? msg ==
    $erMsgToss or (not msgImPr? msg and not msgLeader? msg)

remFile positionList ==
        IFCDR IFCDR positionList

remLine positionList ==
        [IFCAR positionList]

decideHowMuch(pos,oldPos) ==
--when printing a msg, we wish not to show pos info that was
--shown for a previous msg with identical pos info.
--org prints out the word noposition or console
    ((poNopos? pos) and (poNopos? oldPos)) or _
      ((poPosImmediate? pos) and (poPosImmediate? oldPos))  => 'NONE
    (poNopos? pos) or (poPosImmediate? pos) => 'ORG
    (poNopos? oldPos) or (poPosImmediate? oldPos) => 'ALL
    poFileName oldPos ~= poFileName pos => 'ALL
    poLinePosn oldPos ~= poLinePosn pos => 'LINE
    'NONE

listDecideHowMuch(pos,oldPos) ==
    ((poNopos? pos) and (poNopos? oldPos)) or _
      ((poPosImmediate? pos) and (poPosImmediate? oldPos))  => 'NONE
    (poNopos? pos)     => 'ORG
    (poNopos? oldPos)  => 'NONE
    poGlobalLinePosn pos < poGlobalLinePosn oldPos =>
        poPosImmediate? pos => 'ORG
        'LINE
    --(poNopos? pos) or (poPosImmediate? pos) => 'ORG
    'NONE

getPreStL optPre ==
    null optPre => [filler_spaces(2)]
    spses :=
      (extraPlaces := ($preLength - (SIZE optPre) - 3)) > 0 =>
            filler_spaces(extraPlaces)
      '""
    ['%b, optPre,spses,'":", '%d]

-----------------------------
--% these functions handle the attributes

initImPr msg  ==
    $erMsgToss or MEMQ (getMsgTag msg,$imPrTagGuys) =>
        setMsgUnforcedAttr (msg,'$imPrGuys,'imPr)

initToWhere msg  ==
    MEMBER ('trace,getMsgCatAttr (msg,'catless)) =>
          setMsgUnforcedAttr (msg,'$toWhereGuys,'screenOnly)

msgImPr? msg ==
    (getMsgCatAttr (msg,'$imPrGuys) = 'imPr)

msgLeader? msg ==
    getMsgTag msg = 'leader

getMsgToWhere msg ==
    getMsgCatAttr (msg,'$toWhereGuys)

getMsgCatAttr  (msg,cat) ==
    IFCDR ASSQ(cat, ncAlist msg)

setMsgUnforcedAttr(msg,cat,attr) ==
    cat = 'catless => setMsgCatlessAttr(msg,attr)
    not ASSQ(cat, ncAlist msg) => ncPutQ(msg,cat,attr)

setMsgCatlessAttr(msg,attr) ==
    ncPutQ(msg,catless,CONS (attr, IFCDR ASSQ(catless, ncAlist msg)))

--------------------------------------
--% these functions directly interact with the message object

makeLeaderMsg chPosList ==
    st := filler_spaces($preLength - 3)
    oldPos := -1
    for [posNum,:posLetter] in reverse chPosList repeat
        st := STRCONC(st, filler_chars((posNum - oldPos - 1), '"."),
                      posLetter)
        oldPos := posNum
    ['leader,$nopos,'nokey,NIL,NIL,[st]]

makeMsgFromLine line ==
    posOfLine  := getLinePos line
    textOfLine := getLineText line
    localNumOfLine  :=
        i := poLinePosn posOfLine
        stNum := STRINGIMAGE i
        STRCONC(filler_spaces($preLength - 7 - SIZE stNum), stNum)
    ['line,posOfLine,NIL,NIL, STRCONC('"Line", localNumOfLine),_
        textOfLine]

getMsgTag msg == ncTag msg

getMsgTag? msg ==
   IFCAR MEMBER (getMsgTag msg,_
       ['line,'old,'error,'warn,'bug,'unimple,'remark,'stat,'say,'debug])

leader? msg == getMsgTag msg = 'leader
line?   msg == getMsgTag msg = 'line

getMsgPosTagOb msg == msg.1

getMsgPos msg ==
    getMsgFTTag? msg => CADR getMsgPosTagOb msg
    getMsgPosTagOb msg

getMsgPos2 msg ==
    getMsgFTTag? msg => CADDR getMsgPosTagOb msg
    ncBug('"not a from to",[])

getMsgFTTag? msg == IFCAR MEMBER (IFCAR getMsgPosTagOb msg,_
                      ['FROM,'TO,'FROMTO])

getMsgKey msg == msg.2

getMsgKey? msg == IDENTP (val := getMsgKey msg) => val

getMsgArgL msg == msg.3

getMsgPrefix? msg ==
    (pre := msg.4) = 'noPre => NIL
    pre

getMsgPrefix  msg == msg.4


getMsgLitSym msg ==
    getMsgKey? msg => '" "
    '"*"

getMsgText msg == msg.5

setMsgPrefix (msg,val) == msg.4 := val

setMsgText (msg,val) == msg.5 := val
