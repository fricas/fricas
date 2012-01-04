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

)package "BOOTTRAN"
-- BOOT INCLUDER

-- Line syntax is
--
--  Include ::= (SimpleLine | If )*  | ( )fin | empty)
--
--  SimpleLine::=
--        PlainLine |            includes the line
--        )say line |            outputs line to console
--        )eval line |           evaluates the boot line
--                                 nothing included
--        )line line |           line is reproduced as is in lisp output
--        )lisp line |           line is read by lisp READ
--        )package line |        produces (IN-PACKAGE line) in lisp
--                                     output
--        )include filename |    includes the file as boot code
--        )includelisp filename |  includes the file as lisp code
--                                   read by lisp READ
--        )includelines  filename |  includes the file as is
--                                     in lisp output
--
-- If ::= )if SimpleLine* ElseLines )endif
--
-- ElseLines ::= )else SimpleLine* | )elseif SimpleLine* ElseLines | empty
bStreamNil:=["nullstream"]

shoeFileMap(f, fn)==
     a:=shoeInputFile fn
     null a =>
        shoeConsole CONCAT(fn,'" NOT FOUND")
        bStreamNil
     shoeConsole CONCAT('"READING ",fn)
     shoeInclude  bAddLineNumber(bMap(f,bRgen a),bIgen 0)

shoeFileInput fn==shoeFileMap(function IDENTITY,fn)

shoePrefixLisp x== CONCAT('")lisp",x)
shoeLispFileInput fn== shoeFileMap(function shoePrefixLisp,fn)

shoePrefixLine x== CONCAT('")line",x)
shoeLineFileInput fn== shoeFileMap(function shoePrefixLine,fn)

shoePrefix?(prefix,whole) ==
     #prefix > #whole => false
     good:=true
     for i in 0..#prefix-1 for j in 0.. while good repeat
                good:= prefix.i = whole.j
     if good then SUBSTRING(whole,#prefix,nil) else good

shoePlainLine?(s) ==
         #s = 0 =>  true
         not(s.0 = char ")")

shoeSay?          s  == shoePrefix?('")say",         s)
shoeEval?         s  == shoePrefix?('")eval",        s)
shoeInclude?      s  == shoePrefix?('")include",     s)
shoeFin?          s  == shoePrefix?('")fin",         s)
shoeIf?           s  == shoePrefix?('")if",          s)
shoeEndIf?        s  == shoePrefix?('")endif",       s)
shoeElse?         s  == shoePrefix?('")else",        s)
shoeElseIf?       s  == shoePrefix?('")elseif",      s)
shoePackage?      s  == shoePrefix?('")package",     s)
shoeLisp?         s  == shoePrefix?('")lisp",        s)
shoeIncludeLisp?  s  == shoePrefix?('")includelisp" ,s)
shoeLine?         s  == shoePrefix?('")line",        s)
shoeIncludeLines? s  == shoePrefix?('")includelines",s)
shoeIncludeFunction? s  == shoePrefix?('")includefunction",s)

shoeBiteOff x==
         n:=STRPOSL('" ",x,0,true)
         null n =>  false
         n1:=STRPOSL ('" ",x,n,nil)
         null n1 =>  [SUBSTRING(x,n,nil),'""]
         [SUBSTRING(x,n,n1-n),SUBSTRING(x,n1,nil)]

shoeFileName x==
         a:=shoeBiteOff x
         null a =>  '""
         c:=shoeBiteOff CADR a
         null c =>  CAR a
         CONCAT(CAR a,'".",CAR c)

shoeFnFileName x==
         a:=shoeBiteOff x
         null a =>  ['"",'""]
         c:=shoeFileName CADR a
         null c =>  [CAR a,'""]
         [CAR a, c]

shoeFunctionFileInput1(a, fn, fun) ==
    shoeInclude bAddLineNumber(shoeFindLines(fn, fun, a), bIgen 0)

shoeFunctionFileInput [fun,fn]==
    shoeOpenInputFile(fn, FUNCTION shoeFunctionFileInput1, [fn, fun])

shoeInclude s== bDelay(function shoeInclude1,[s])
shoeInclude1 s==
      bStreamNull s=> s
      [h,:t]  :=s
      string  :=CAR h
      command :=shoeFin? string  => bStreamNil
      command :=shoeIf? string   => shoeThen([true],[STTOMC command],t)
      bAppend(shoeSimpleLine h,shoeInclude t)

shoeSimpleLine(h) ==
      string  :=CAR h
      shoePlainLine? string=> [h]
      command:=shoeLisp? string => [h]
      command:=shoeIncludeLisp? string =>
                  shoeLispFileInput shoeFileName command
      command:=shoeIncludeFunction? string =>
                  shoeFunctionFileInput shoeFnFileName command
      command:=shoeLine? string => [h]
      command:=shoeIncludeLines? string =>
                  shoeLineFileInput shoeFileName command
      command:=shoeInclude? string => shoeFileInput shoeFileName command
      command:=shoePackage? string => [h]
      command:=shoeSay? string =>
                shoeConsole command
                nil
      command:=shoeEval? string =>
                STTOMC command
                nil
      shoeLineSyntaxError(h)
      nil

shoeThen(keep,b,s)== bDelay(function shoeThen1,[keep,b,s])
shoeThen1(keep,b,s)==
    bPremStreamNull s=> s
    [h,:t]  :=s
    string  :=CAR h
    command :=shoeFin? string  => bPremStreamNil(h)
    keep1:= car keep
    b1   := car b
    command :=shoeIf? string  =>
      keep1 and b1=>  shoeThen(cons(true,keep),cons(STTOMC command,b),t)
      shoeThen(cons(false,keep),cons(false,b),t)
    command :=shoeElseIf? string=>
      keep1 and not b1=>
          shoeThen(cons(true,rest keep),cons(STTOMC command,rest b),t)
      shoeThen(cons(false,rest keep),cons(false,rest b),t)
    command :=shoeElse? string =>
     keep1 and not b1=>shoeElse(cons(true,rest keep),cons(true,rest b),t)
     shoeElse(cons(false,rest keep),cons(false,rest b),t)
    command :=shoeEndIf? string=>
         null cdr b=>  shoeInclude t
         shoeThen(rest keep,rest b,t)
    keep1 and b1 => bAppend(shoeSimpleLine h,shoeThen(keep,b,t))
    shoeThen(keep,b,t)

shoeElse(keep,b,s)== bDelay(function shoeElse1,[keep,b,s])
shoeElse1(keep,b,s)==
    bPremStreamNull s=> s
    [h,:t]  :=s
    string  :=CAR h
    command :=shoeFin? string => bPremStreamNil(h)
    b1:=car b
    keep1:=car keep
    command :=shoeIf? string=>
      keep1 and b1=> shoeThen(cons(true,keep),cons(STTOMC command,b),t)
      shoeThen(cons(false,keep),cons(false,b),t)
    command :=shoeEndIf? string =>
         null cdr b=>  shoeInclude t
         shoeThen(rest keep,rest b,t)
    keep1 and b1 => bAppend(shoeSimpleLine h,shoeElse(keep,b,t))
    shoeElse(keep,b,t)

shoeLineSyntaxError(h)==
     shoeConsole CONCAT('"INCLUSION SYNTAX ERROR IN LINE ",
                                STRINGIMAGE CDR h)
     shoeConsole car h
     shoeConsole '"LINE IGNORED"

bPremStreamNil(h)==
       shoeConsole CONCAT('"UNEXPECTED )fin IN LINE ",STRINGIMAGE CDR h)
       shoeConsole car h
       shoeConsole '"REST OF FILE IGNORED"
       bStreamNil

bPremStreamNull(s)==
     if bStreamNull s
     then
        shoeConsole '"FILE TERMINATED BEFORE )endif"
        true
     else false
