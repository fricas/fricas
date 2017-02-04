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

--% Formatting functions for various compiler data objects.
--  These are used as [%origin o, %id n] for %1f %2f... style arguments
--  in a keyed message.
--  SMW, SG June 88

%id a     == [FUNCTION IDENTITY, a]

-- Union(FileName,"strings","console")
%origin x ==
    [function porigin, x]

porigin x == x

ppos p ==
    pfNoPosition? p => ['"no position"]
    pfImmediate? p  => ['"console"]
    cpos := pfCharPosn p
    lpos := pfLinePosn p
    org  := porigin pfFileName p
    [org,'" ",'"line",'" ",lpos]

--keyStuff ::= keynumber | [ one or more keySeqs ]
--keySeq   ::= keynumber optargList optdbn
--optARgL  ::= [ 0 or more arguments ] | nothing at all
--optDbn   ::= ['dbN , databaseName ] | nothing at all

-- Includer

incStringStream s==
   incRenumber incLude(0,incRgen s,0,['"strings"] ,[Top])

incFile fn==
   incRenumber incLude(0,incRgen OPEN fn,0,[fn],[Top])

incStream(st, fn) ==
   incRenumber incLude(0,incRgen st,0,[fn],[Top])

incFileInput    fn == incRgen  MAKE_-INSTREAM fn
incConsoleInput () == incRgen  MAKE_-INSTREAM 0

incLine(eb, str, gno, lno, ufo) ==
            ln := lnCreate(eb,str,gno,lno,ufo)
            CONS(CONS(ln,1), str)

incPos f == first f

incRenumberItem(f, i) ==
            l := CAAR f
            lnSetGlobalNum(l, i)
            f

incRenumberLine(xl, gno) ==
            l := incRenumberItem(xl.0, gno)
            incHandleMessage xl
            l

incRenumber ssx == incZip (function incRenumberLine, ssx, incIgen 0)

incPrefix?(prefix, start, whole) ==
            #prefix > #whole-start => false
            good:=true
            for i in 0..#prefix-1 for j in start.. while good repeat
                good:= prefix.i = whole.j
            good

incCommand?(s) == #s > 1 and s.0 = char ")" and not (s.1 = char " ")

incCommands :=
            ['"say"    , _
             '"include", _
             '"console", _
             '"fin"    , _
             '"assert" , _
             '"if"     , _
             '"elseif" , _
             '"else"   , _
             '"endif" ]

incClassify(s) ==
            not incCommand? s => [false,0, '""]
            i := 1; n := #s
            while i < n and s.i = char " " repeat i := i + 1
            i >= n => [true,0,'"other"]
            eb := (i = 1 => 0; i)
            bad:=true
            for p in incCommands while bad repeat
                incPrefix?(p, i, s) =>
                    bad:=false
                    p1 :=p
            if bad then [true,0,'"other"] else [true,eb,p1]

incCommandTail(s, info) ==
            start := (info.1 = 0 => 1; info.1)
            incDrop(start+#info.2+1, s)

incDrop(n, b) ==
            n >= #b => ""
            SUBSTRING(b,n,nil)


inclFname(s, info) == incFileName incCommandTail(s, info)

incBiteOff x ==
          n:=STRPOSL('" ",x,0,true)-- first nonspace
          if null n
          then false -- all spaces
          else
             n1:=STRPOSL ('" ",x,n,nil)
             if null n1 -- all nonspaces
             then [SUBSTRING(x,n,nil),'""]
             else [SUBSTRING(x,n,n1-n),SUBSTRING(x,n1,nil)]

incTrunc (n,x)==
     if #x>n
     then SUBSTRING(x,0,n)
     else x

incFileName x == first incBiteOff x

fileNameStrings fn==[PNAME(fn.0),PNAME(fn.1),PNAME(fn.2)]

ifCond(s, info) ==
    word := INTERN DROPTRAILINGBLANKS(incCommandTail(s, info))
    member(word, $inclAssertions)

assertCond(s, info) ==
    word := INTERN DROPTRAILINGBLANKS(incCommandTail(s, info))
    if not member(word, $inclAssertions) then
        $inclAssertions := [word, :$inclAssertions]


incActive?(fn,ufos)==MEMBER(fn,ufos)

incNConsoles ufos==
        a:=MEMBER('"console",ufos)
        if a then 1+incNConsoles rest a else 0

Top            := 01
IfSkipToEnd    := 10
IfKeepPart     := 11
IfSkipPart     := 12
ElseifSkipToEnd:= 20
ElseifKeepPart := 21
ElseifSkipPart := 22
ElseSkipToEnd  := 30
ElseKeepPart   := 31
Continuation   := 41

Top?     (st) == QUOTIENT(st,10) = 0
If?      (st) == QUOTIENT(st,10) = 1
Elseif?  (st) == QUOTIENT(st,10) = 2
Else?    (st) == QUOTIENT(st,10) = 3
SkipEnd? (st) == REMAINDER(st,10) = 0
KeepPart?(st) == REMAINDER(st,10) = 1
SkipPart?(st) == REMAINDER(st,10) = 2
Skipping?(st) == not KeepPart? st

        --% Message Handling
incHandleMessage(xl) ==
          xl.1.1 = "none" =>
              0
          xl.1.1 = "error" =>
              inclHandleError(incPos xl.0, xl.1.0)
          xl.1.1 = "warning" =>
              inclHandleWarning(incPos xl.0, xl.1.0)
          xl.1.1 = "say" =>
              inclHandleSay(incPos xl.0, xl.1.0)
          inclHandleBug(incPos xl.0, xl.1.0)

xlOK(eb, str, lno, ufo)  ==
                [incLine(eb, str, -1, lno, ufo), [NIL, "none"]]

xlOK1(eb, str,str1, lno, ufo)  ==
                [incLine1(eb, str,str1, -1, lno, ufo), [NIL, "none"]]

incLine1(eb, str,str1, gno, lno, ufo) ==
            ln := lnCreate(eb,str,gno,lno,ufo)
            CONS(CONS(ln,1), str1)
xlSkip(eb, str, lno, ufo) ==
        str := CONCAT('"-- Omitting:", str)
        [incLine(eb, str, -1, lno, ufo), [NIL, "none"]]

xlMsg(eb, str, lno, ufo, mess) ==
                [incLine(eb, str, -1, lno, ufo), mess]

xlPrematureEOF(eb, str, lno, ufos) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgPrematureEOF(ufos.0),"error"])

xlPrematureFin(eb, str, lno, ufos) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgPrematureFin(ufos.0),"error"])

xlFileCycle(eb, str, lno, ufos, fn) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgFileCycle(ufos,fn),"error"])

xlNoFile(eb, str, lno, ufos) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgNoFile(), "error"])

xlCannotRead(eb, str, lno, ufos, fn) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgCannotRead(fn), "error"])

xlConsole(eb, str, lno, ufos)  ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgConsole(),"say"])

xlConActive(eb, str, lno, ufos, n) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgConActive(n),"warning"])

xlConStill(eb, str, lno, ufos, n) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgConStill(n), "say"])

xlSkippingFin(eb, str, lno, ufos) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgFinSkipped(),"warning"])

xlIfBug(eb, str, lno, ufos) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgIfBug(), "bug"])

xlCmdBug(eb, str, lno, ufos) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgCmdBug(), "bug"])

xlSay(eb, str, lno, ufos, x) ==
          xlMsg(eb, str, lno,ufos.0,
              [inclmsgSay(x), "say"])

xlIfSyntax(eb, str, lno,ufos,info,sts) ==
          st := sts.0
          found := info.2
          context :=
              Top? st  => "not in an )if...)endif"
              Else? st => "after an )else"
              "but can't figure out where"
          xlMsg(eb, str, lno, ufos.0,
               [inclmsgIfSyntax(ufos.0,found,context), "error"])

        --% This is it

incLude(eb, ss, ln, ufos, states) ==
       Delay(function incLude1,[eb, ss, ln, ufos, states])

Rest s ==> incLude(eb, rest ss, lno, ufos, states)

incLude1 (:z) ==
            [eb, ss, ln, ufos, states]:=z
            lno       := ln+1
            state     := states.0

            StreamNull ss =>
                not Top? state =>
                    cons(xlPrematureEOF(eb,
                     '")--premature end",  lno,ufos), StreamNil)
                StreamNil

            str  :=  EXPAND_-TABS first ss
            has_cont :=
                (nn := #str) < 1 => false
                str.(nn - 1) = char('"__")

            state = Continuation =>
                rs :=
                    has_cont => Rest(s)
                    incLude(eb, rest ss, lno, ufos, rest(states))
                Skipping?(states.1) => cons(xlSkip(eb,str,lno,ufos.0), rs)
                cons(xlOK(eb, str, lno, ufos.0), rs)

            info :=  incClassify str

            not info.0 =>
                rs :=
                    has_cont => incLude(eb, rest ss, lno, ufos,
                                        cons(Continuation, states))
                    Rest(s)
                Skipping? state => cons(xlSkip(eb,str,lno,ufos.0), rs)
                cons(xlOK(eb, str, lno, ufos.0), rs)

            info.2 = '"other" =>
                Skipping? state => cons(xlSkip(eb,str,lno,ufos.0), Rest s)
                cons(xlOK1(eb, str,CONCAT('")command",str), lno, ufos.0),
                                          Rest s)

            info.2 = '"say" =>
                Skipping? state => cons(xlSkip(eb,str,lno,ufos.0), Rest s)
                str := incCommandTail(str, info)
                cons(xlSay(eb, str, lno, ufos, str),
                     cons(xlOK(eb,str,lno,ufos.0), Rest s))

            info.2 = '"include" =>
                Skipping? state =>
                     cons(xlSkip(eb,str,lno,ufos.0), Rest s)
                fn1 := inclFname(str, info)
                not fn1 =>
                    cons(xlNoFile(eb, str, lno, ufos), Rest s)
                not PROBE_-FILE fn1 =>
                    cons(xlCannotRead(eb, str, lno,ufos,fn1),Rest s)
                incActive?(fn1,ufos) =>
                    cons(xlFileCycle (eb, str, lno,ufos,fn1),Rest s)
                Includee  :=
                  incLude(eb+info.1,incFileInput fn1,0,
                            cons(fn1,ufos), cons(Top,states))
                cons(
                    xlOK(eb,str,lno,ufos.0),
                          incAppend(Includee, Rest s))

            info.2 = '"console" =>
                Skipping? state => cons(xlSkip(eb,str,lno,ufos.0), Rest s)
                Head :=
                 incLude(eb+info.1,incConsoleInput(),0,
                     cons('"console",ufos),cons(Top,states) )
                Tail := Rest s

                n := incNConsoles ufos
                if n > 0 then
                   Head := cons(xlConActive(eb, str, lno,ufos,n),Head)
                   Tail :=
                       cons(xlConStill (eb, str, lno,ufos,n),Tail)

                Head := cons (xlConsole(eb, str, lno,ufos), Head)
                cons(xlOK(eb,str,lno,ufos.0),incAppend(Head,Tail))

            info.2 = '"fin" =>
                Skipping? state =>
                    cons(xlSkippingFin(eb, str, lno,ufos), Rest s)
                not Top? state  =>
                    cons(xlPrematureFin(eb, str, lno,ufos), StreamNil)
                cons(xlOK(eb,str,lno,ufos.0), StreamNil)

            info.2 = '"assert" =>
                Skipping? state =>
                    cons(xlSkippingFin(eb, str, lno,ufos), Rest s)
                assertCond(str, info)
                cons(xlOK(eb,str,lno,ufos.0), incAppend(Includee, Rest s))

            info.2 = '"if" =>
                s1 :=
                    Skipping? state => IfSkipToEnd
                    if ifCond(str,info) then IfKeepPart else IfSkipPart
                cons(xlOK(eb,str,lno,ufos.0),
                      incLude(eb, rest ss, lno, ufos, cons(s1, states)))
            info.2 = '"elseif" =>
                not If? state and not Elseif? state =>
                    cons(xlIfSyntax(eb, str,lno,ufos,info,states),
                            StreamNil)

                if SkipEnd? state or KeepPart? state or SkipPart? state
                then
                     s1:=if SkipPart? state
                         then
                            pred := ifCond(str,info)
                            if pred
                            then ElseifKeepPart
                            else ElseifSkipPart
                         else ElseifSkipToEnd
                     cons(xlOK(eb,str,lno,ufos.0),
                        incLude(eb, rest ss, lno, ufos, cons(s1, rest states)))
                else
                    cons(xlIfBug(eb, str, lno,ufos), StreamNil)

            info.2 = '"else" =>
                not If? state and not Elseif? state =>
                    cons(xlIfSyntax(eb, str,lno,ufos,info,states),
                           StreamNil)
                if SkipEnd? state or KeepPart? state or SkipPart? state
                then
                      s1 :=if SkipPart? state
                           then ElseKeepPart
                           else ElseSkipToEnd
                      cons(xlOK(eb,str,lno,ufos.0),
                        incLude(eb, rest ss, lno, ufos, cons(s1, rest states)))
                else
                    cons(xlIfBug(eb, str, lno,ufos), StreamNil)

            info.2 = '"endif" =>
                Top? state =>
                    cons(xlIfSyntax(eb, str,lno,ufos,info,states),
                        StreamNil)
                cons(xlOK(eb,str,lno,ufos.0),
                         incLude(eb, rest ss, lno, ufos, rest states))

            cons(xlCmdBug(eb, str, lno,ufos), StreamNil)

--% Message handling for the source includer
--  SMW June 88

inclHandleError(pos, [key, args]) ==
    ncSoftError(pos, key, args)
inclHandleWarning(pos, [key, args]) ==
    ncSoftError(pos, key,args)
inclHandleBug(pos, [key, args]) ==
    ncBug(key, args)
inclHandleSay(pos, [key, args]) ==
    ncSoftError(pos, key, args)

inclmsgSay str  ==
    ['S2CI0001, [%id str]]
inclmsgPrematureEOF ufo  ==
    ['S2CI0002, [%origin ufo]]
inclmsgPrematureFin ufo  ==
    ['S2CI0003, [%origin ufo]]
inclmsgFileCycle(ufos,fn) ==
    flist := [porigin n for n in reverse ufos]
    f1    := porigin fn
    cycle := [:[:[n,'"==>"] for n in flist], f1]
    ['S2CI0004, [%id cycle, %id f1]]
inclmsgConsole   () ==
    ['S2CI0005, []]
inclmsgConActive n  ==
    ['S2CI0006, [%id n]]
inclmsgConStill  n  ==
    ['S2CI0007, [%id n]]
inclmsgFinSkipped() ==
    ['S2CI0008, []]
inclmsgIfSyntax(ufo,found,context) ==
    found := CONCAT('")", found)
    ['S2CI0009, [%id found, %id context, %origin ufo]]
inclmsgNoFile() ==
    ['S2CI0010, []]
inclmsgCannotRead fn ==
    ['S2CI0011, [fn]]
inclmsgIfBug() ==
    ['S2CB0002, []]
inclmsgCmdBug() ==
    ['S2CB0003, []]
