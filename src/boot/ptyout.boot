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

-- (boottocl "filename") translates the file "filename.boot" to
-- the common lisp file "filename.clisp"

BOOTTOCL (fn, outfn) ==
         $bfClamming:local:=false
         BOOTTOCLLINES(fn, outfn)

shoeOpenInputBoottranFile(fn, fun, args) ==
    doInBoottranPackage(handle_input_file(fn, fun, args))

-- (bootclam "filename") translates the file "filename.boot" to
-- the common lisp file "filename.clisp" , producing, for each function
-- a hash table to store previously computed values indexed by argument
-- list.

BOOTCLAM (fn, outfn) ==
    $bfClamming : local := true
    BOOTTOCLCLINES(fn, outfn)

BOOTTOCLLINES(fn, outfn)==
    infn := shoeAddbootIfNec fn
    if NULL(outfn) then
        outfn := CONCAT(shoeRemovebootIfNec fn, '".clisp")
    shoeOpenInputBoottranFile(infn, FUNCTION shoeClLines, [fn, outfn])

shoeClLines1(stream, a) ==
    shoeFileTrees(shoeTransformStream a, stream)

shoeClLines(a, fn, outfn)==
      if null a
      then shoeNotFound fn
      else
          $GenVarCounter : local := 0
          handle_output_file(outfn, FUNCTION shoeClLines1, [a])
          shoeConsole CONCAT(outfn, '" PRODUCED")

-- (boottoclc "filename") translates the file "filename.boot" to
-- the common lisp file "filename.clisp" with the original boot
-- code as comments

BOOTTOCLC(fn, outfn) ==
    $bfClamming : local := false
    BOOTTOCLCLINES(fn, outfn)

BOOTTOCLCLINES(fn, outfn)==
  infn:=shoeAddbootIfNec fn
  if NULL(outfn) then
      outfn := CONCAT(shoeRemovebootIfNec fn, '".clisp")
  shoeOpenInputBoottranFile(infn, FUNCTION shoeClCLines, [fn, outfn])

shoeClCLines1(stream, a) ==
    shoeFileTrees(shoeTransformToFile(stream,
        shoeInclude bAddLineNumber(bRgen a, bIgen 0)), stream)

shoeClCLines(a, fn, outfn)==
      if null a
      then shoeNotFound fn
      else
          $GenVarCounter : local := 0
          handle_output_file(outfn, FUNCTION shoeClCLines1, [a])
          shoeConsole CONCAT(outfn,'" PRODUCED")

-- (boottomc "filename") translates the file "filename.boot"
-- to machine code and loads it one item at a time

BOOTTOMC fn==
    $bfClamming : local := false
    $GenVarCounter : local := 0
    infn := shoeAddbootIfNec fn
    shoeOpenInputBoottranFile(infn, FUNCTION shoeMc, [fn])

shoeMc(a,fn)==
   if null a
   then shoeNotFound fn
   else
     shoePCompileTrees shoeTransformStream a
     shoeConsole CONCAT(fn,'" COMPILED AND LOADED")

-- (boot "filename") translates the file "filename.boot" to
-- the common lisp file "filename.clisp", compiles it and loads
-- the bbin/o file.

COMPILE_-BOOT_-FILE fn == BOOT fn

BOOT fn ==
     $bfClamming:local:=false
     outfn:=CONCAT(shoeRemovebootIfNec fn,'".clisp")
     a:=BOOTTOCL (fn, outfn)
     null a => nil
     outbin := shoeCOMPILE_-FILE outfn
     LOAD outbin

EVAL_-BOOT_-FILE fn ==
    outfn := CONCAT(shoeRemovebootIfNec fn, '".clisp")
    BOOTTOCL(fn, outfn)
    LOAD outfn


-- (boot "filename") translates the file "filename.boot"
-- and prints the result at the console

doBO fn ==
    $GenVarCounter : local := 0
    infn := shoeAddbootIfNec fn
    shoeOpenInputBoottranFile(infn, FUNCTION shoeToConsole, [fn])

BO fn ==
    $bfClamming : local := false
    doBO(fn)

BOCLAM fn ==
    $bfClamming : local := true
    doBO(fn)

shoeToConsole(a,fn)==
     if null a
     then shoeNotFound fn
     else
        shoeConsoleTrees shoeTransformToConsole
          shoeInclude bAddLineNumber(bRgen a,bIgen 0)

-- (stout "string") translates the string "string"
-- and prints the result at the console

STOUT string==   PSTOUT [string]
--   $GenVarCounter:local := 0
--   $bfClamming:local:=false
--   shoeConsoleTrees shoeTransformString [string]

STTOSEX0 string ==
    $GenVarCounter:local := 0
    shoeTransformString [string]

STTOSEX1 string ==
    doInBoottranPackage(STTOSEX0 string)

STTOSEX string ==
    $bfClamming : local := false
    a := STTOSEX1 string
    bStreamPackageNull a => nil
    stripm(CAR a, _*PACKAGE_*, FIND_-PACKAGE '"BOOTTRAN")

STEVAL string ==
    fn := STTOSEX string
    EVAL fn

-- (sttomc "string") translates the string "string"
-- to common lisp, and compiles it.

STTOMC string==
    $bfClamming : local := false
    a := STTOSEX1 string
    result :=
        bStreamPackageNull a => nil
        shoePCompile car a
    result


shoeCompileTrees s==
    while not bStreamNull s repeat
         shoeCompile car s
         s:=cdr s

shoeCompile fn==
    fn is ['DEFUN,name,bv,:body]=>
          COMPILE (name,['LAMBDA,bv,:body])
    EVAL fn

shoeNotFound fn ==
   shoeConsole CONCAT(fn ,'" NOT FOUND")
   nil

shoeTransform str==
    bNext(function shoeTreeConstruct,
      bNext(function shoePileInsert,
        bNext(function shoeLineToks, str)))

shoeTransformString s==
    shoeTransform shoeInclude bAddLineNumber(s,bIgen 0)
shoeTransformStream s==shoeTransformString bRgen s
--  shoeTransform shoeInclude bAddLineNumber(bRgen s,bIgen 0)

shoeTransformToConsole str==
    bNext(function shoeConsoleItem,
      bNext(function shoePileInsert,
        bNext(function shoeLineToks, str)))

shoeTransformToFile(fn,str)==
    bFileNext(fn,
      bNext(function shoePileInsert,
        bNext(function shoeLineToks, str)))

shoeConsoleItem (str)==
        dq:=CAR str
        shoeConsoleLines shoeDQlines dq
        cons(shoeParseTrees dq,CDR str)

bFileNext(fn,s)==bDelay(function bFileNext1,[fn,s])

bFileNext1(fn,s)==
      bStreamNull s=> ["nullstream"]
      dq:=CAR s
      shoeFileLines(shoeDQlines dq,fn)
      bAppend(shoeParseTrees dq,bFileNext(fn,cdr s))

shoeParseTrees dq==
        toklist := dqToList dq
        null toklist => []
        shoeOutParse toklist

shoeTreeConstruct (str)==
        cons(shoeParseTrees CAR str,CDR str)

shoeDQlines dq==
        a:= CDAAR shoeLastTokPosn dq
        b:= CDAAR shoeFirstTokPosn dq
        streamTake (a-b+1,CAR shoeFirstTokPosn dq)

streamTake(n,s)==
    if bStreamNull s
    then nil
    else if EQL(n,0)
         then nil
         else cons(car s,streamTake(n-1,cdr s))

shoeFileLines (lines,fn) ==
        shoeFileLine( '" ",fn)
        for line in lines repeat shoeFileLine (shoeAddComment line,fn)
        shoeFileLine ('" ",fn)

shoeConsoleLines lines ==
        shoeConsole '" "
        for line in lines repeat shoeConsole shoeAddComment line
        shoeConsole '" "

shoeFileLine(x, stream) ==
    WRITE_-LINE(x, stream)
    x

shoeFileTrees(s,st)==
        while not bStreamNull s repeat
            a:=CAR s
            if EQCAR (a,"+LINE")
            then shoeFileLine(CADR a,st)
            else REALLYPRETTYPRINT(a,st)
            s:=CDR s


shoePPtoFile(x, stream) ==
    SHOENOTPRETTYPRINT(x, stream)
    x

shoeConsoleTrees s ==
    while not bStreamPackageNull s repeat
--      while not bStreamNull s repeat
          fn:=stripm(CAR s,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
          REALLYPRETTYPRINT fn
          s:=CDR s

shoeAddComment l==  CONCAT('"; ",CAR l)

shoeOutParse stream ==
    $inputStream :local:= stream
    $stack:local       :=nil
    $stok:local := nil
    $ttok:local := nil
    $op:local :=nil
    $wheredefs:local:=nil
    $typings:local:=nil
    $returns:local :=nil
    $bpCount:local:=0
    $bpParenCount:local:=0
    bpFirstTok()
    found:=CATCH("TRAPPOINT",bpOutItem())
    if found="TRAPPED"
    then  nil
    else if not bStreamNull $inputStream
         then
          bpGeneralErrorHere()
          nil
         else if null $stack
              then
                bpGeneralErrorHere()
                nil
              else CAR $stack

bpOutItem()==
    bpComma() or bpTrap()
    b:=bpPop1()
    ATOM(b) => bpPush [ b ]
    EQCAR(b,"TUPLE")=> bpPush cdr b
    EQCAR(b,"+LINE")=> bpPush [ b ]
    b is ["L%T",l,r] and IDENTP l =>
                 bpPush [shoeEVALANDFILEACTQ ["SETQ",l,r]]
    MEMQ(CAR(b), '(SETANDFILEQ SETQ DEFPARAMETER DEFCONSTANT DEFVAR)) =>
       bpPush [ b ]
    b:=shoeCompTran ["LAMBDA",["x"],b]
    bpPush [shoeEVALANDFILEACTQ CADDR b]

shoeEVALANDFILEACTQ x==  ["EVAL-WHEN", ["EVAL","LOAD"], x]

SoftShoeError(posn,key)==
    shoeConsole CONCAT('"ERROR IN LINE ",STRINGIMAGE lineNo posn)
    shoeConsole lineString posn
    shoeConsole CONCAT(shoeSpaces lineCharacter posn,'"|")
    shoeConsole key

shoeSpaces n  ==  MAKE_-FULL_-CVEC(n, '".")

bpIgnoredFromTo(pos1, pos2) ==
    shoeConsole CONCAT('"ignored from line ", STRINGIMAGE lineNo pos1)
    shoeConsole lineString pos1
    shoeConsole CONCAT(shoeSpaces lineCharacter pos1,'"|")
    shoeConsole CONCAT('"ignored through line ", STRINGIMAGE lineNo pos2)
    shoeConsole lineString pos2
    shoeConsole CONCAT(shoeSpaces lineCharacter pos2,'"|")

lineNo p==CDAAR p
lineString p==CAAAR p
lineCharacter p==CDR p

bStreamNull x==
  null x or EQCAR (x,"nullstream") => true
  while EQCAR(x,"nonnullstream") repeat
          st:=APPLY(CADR x,CDDR x)
          RPLACA(x,CAR st)
          RPLACD(x,CDR st)
  EQCAR(x,"nullstream")

bDelay(f,x)==cons("nonnullstream",[f,:x])

bAppend(x,y)==bDelay(function bAppend1,[x,y])

bAppend1(:z)==
     if bStreamNull car z
     then if bStreamNull CADR z
          then ["nullstream"]
          else CADR z
     else cons(CAAR z,bAppend(CDAR z,CADR z))

bMap(f,x)==bDelay(function bMap1, [f,x])

bMap1(:z)==
     [f,x]:=z
     if bStreamNull x
     then bStreamNil
     else cons(FUNCALL(f,car x),bMap(f,cdr x))

bNext(f,s)==bDelay(function bNext1,[f,s])

bNext1(f,s)==
      bStreamNull s=> ["nullstream"]
      h:= APPLY(f, [s])
      bAppend(car h,bNext(f,cdr h))

bRgen s==bDelay(function bRgen1,[s])

bRgen1(:s) ==
        a:=shoeread_-line car s
        if shoePLACEP a
        then
--          shoeCLOSE car s
            ["nullstream"]
        else cons(a,bRgen car s)

bIgen n==bDelay(function bIgen1,[n])

bIgen1(:n)==
        n:=car n+1
        cons(n,bIgen n)

bAddLineNumber(f1,f2)==bDelay(function bAddLineNumber1,[f1,f2])

bAddLineNumber1(:f)==
     [f1,f2] := f
     bStreamNull f1 =>  ["nullstream"]
     bStreamNull f2 =>  ["nullstream"]
     cons(cons(CAR f1,CAR f2),bAddLineNumber(CDR f1,CDR f2))

shoeAddbootIfNec s==shoeAddStringIfNec('".boot",s)

shoeRemovebootIfNec s==shoeRemoveStringIfNec('".boot",s)

shoeAddStringIfNec(str,s)==
       a:=STRPOS(str,s,0,nil)
       if null a
       then CONCAT(s,str)
       else s

shoeRemoveStringIfNec(str,s)==
       a:=STRPOS(str,s,0,nil)
       if null a
       then s
       else SUBSTRING(s,0,a)

-- DEFUSE prints the definitions not used and the words used and
-- not defined in the input file and common lisp.
-- FIXME: seem to mishandle packages

DEFUSE fn==
  infn:=CONCAT(fn,'".boot")
  handle_input_file(infn, FUNCTION shoeDfu, [fn])

shoeDfu(a,fn)==
  if null a
  then shoeNotFound fn
  else
     $lispWordTable:local :=MAKE_-HASHTABLE ("EQ")
     DO_-SYMBOLS(i(FIND_-PACKAGE "LISP"),HPUT($lispWordTable,i,true))
     $bootDefined:local :=MAKE_-HASHTABLE "EQ"
     $bootUsed:local :=MAKE_-HASHTABLE "EQ"
     $bootDefinedTwice:local:=nil
     $GenVarCounter:local :=0
     $bfClamming:local:=false
     shoeDefUse shoeTransformStream a
     out:=CONCAT(fn,'".defuse")
     handle_output_file(out, FUNCTION shoeReport, [])
     shoeConsole CONCAT(out,'" PRODUCED")

shoeReport stream==
          shoeFileLine('"DEFINED and not USED",stream)
          a:=[i for i in HKEYS $bootDefined | not GETHASH(i,$bootUsed)]
          bootOut(SSORT a,stream)
          shoeFileLine('"             ",stream)
          shoeFileLine('"DEFINED TWICE",stream)
          bootOut(SSORT $bootDefinedTwice,stream)
          shoeFileLine('"             ",stream)
          shoeFileLine('"USED and not DEFINED",stream)
          a:=[i for i in HKEYS $bootUsed |
                     not GETHASH(i,$bootDefined)]
          for i in SSORT a repeat
             b:=CONCAT(PNAME i,'" is used in ")
             bootOutLines( SSORT GETHASH(i,$bootUsed),stream,b)

shoeDefUse(s)==
        while not bStreamPackageNull s repeat
            defuse([],CAR s)
            s:=CDR s

defuse(e,x)==
     x:=stripm(x,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
     $used:local:=nil
     [nee,niens]:=
        x is ['DEFUN,name,bv,:body] => [name,['LAMBDA,bv,:body]]
        x is ['DEFMACRO,name,bv,:body] => [name,['LAMBDA,bv,:body]]
        x is ["EVAL_-WHEN",.,["SETQ",id,exp]]=>[id,exp]
        x is ["SETQ",id,exp]=>[id,exp]
        ["TOP-LEVEL", x]
     if GETHASH(nee,$bootDefined)
     then
        $bootDefinedTwice:=
               nee="TOP-LEVEL"=> $bootDefinedTwice
               cons(nee,$bootDefinedTwice)
     else HPUT($bootDefined,nee,true)
     defuse1 (e,niens)
     for i in $used repeat
        HPUT($bootUsed,i,cons(nee,GETHASH(i,$bootUsed)))

defuse1(e,y)==
     ATOM y =>
         IDENTP y =>
            $used:=
                 MEMQ(y,e)=>$used
                 MEMQ(y,$used)=>$used
                 defusebuiltin y =>$used
                 UNION([y],$used)
         []
     y is ["LAMBDA",a,:b]=> defuse1 (append(unfluidlist a,e),b)
     y is ["PROG",a,:b]=>
            [dol,ndol]:=defSeparate a
            for i in dol repeat
                  HPUT($bootDefined,i,true)
            defuse1 (append(ndol,e),b)
     y is ["QUOTE",:a] => []
     y is ["+LINE",:a] => []
     for i in y repeat defuse1(e,i)

defSeparate x==
      if null x
      then [[],[]]
      else
         f:=car x
         [x1,x2]:=defSeparate cdr x
         if bfBeginsDollar f
         then [cons(f,x1),x2]
         else [x1,cons(f,x2)]
unfluidlist x==
    NULL x => []
    ATOM x=> [x]
    x is ["&REST",y]=> [y]
    cons(car x,unfluidlist cdr x)

defusebuiltin x==  GETHASH(x,$lispWordTable)

bootOut (l,outfn)==
      for i in l repeat shoeFileLine (CONCAT ('"   ",PNAME i),outfn)

CLESSP(s1,s2)==not(SHOEGREATERP(s1,s2))
SSORT l == SORT(l,function CLESSP)

bootOutLines(l,outfn,s)==
  if null l
  then shoeFileLine(s,outfn)
  else
     a:=PNAME car l
     if #s +#a > 70
     then
          shoeFileLine(s,outfn)
          bootOutLines(l,outfn,'" ")
     else bootOutLines(cdr l,outfn,CONCAT(s,'" ",a))


-- (xref "fn") produces a cross reference listing in "fn.xref"
-- It contains each name
-- used in "fn.boot", together with a list of functions that use it.
-- FIXME: seem to mishandle packages

XREF fn==
  infn:=CONCAT(fn,'".boot")
  handle_input_file(infn, FUNCTION shoeXref, [fn])

shoeXref(a,fn)==
  if null a
  then shoeNotFound fn
  else
     $lispWordTable:local :=MAKE_-HASHTABLE ("EQ")
     DO_-SYMBOLS(i(FIND_-PACKAGE "LISP"),HPUT($lispWordTable,i,true))
     $bootDefined:local :=MAKE_-HASHTABLE "EQ"
     $bootUsed:local :=MAKE_-HASHTABLE "EQ"
     $GenVarCounter:local :=0
     $bfClamming:local:=false
     shoeDefUse shoeTransformStream a
     out:=CONCAT(fn,'".xref")
     handle_output_file(out, FUNCTION shoeXReport, [])
     shoeConsole CONCAT(out,'" PRODUCED")


shoeXReport stream==
          shoeFileLine('"USED and where DEFINED",stream)
          c:=SSORT HKEYS $bootUsed
          for i in c repeat
             a:=CONCAT(PNAME i,'" is used in ")
             bootOutLines( SSORT GETHASH(i,$bootUsed),stream,a)

-------------------------------------------------------------------

shoeTransform2 str==
    bNext(function shoeItem,
      streamTake(1, bNext(function shoePileInsert,
           bNext(function shoeLineToks, str))))

shoeItem (str)==
        dq:=CAR str
        cons([[CAR line for line in  shoeDQlines dq]],CDR str)


shoeFindLines(fn,name,a)==
   if null a
   then
      shoeNotFound fn
      []
   else
      [lines,b]:=shoePackageStartsAt([],#name,name, shoeInclude
                        bAddLineNumber(bRgen a,bIgen 0))
      b:=shoeTransform2 b
      if bStreamNull b
      then
           shoeConsole CONCAT (name,'" not found in ",fn)
           []
      else
         if null lines
         then shoeConsole '")package not found"
         append(reverse lines,car b)

shoePackageStartsAt (lines,sz,name,stream)==
   bStreamNull stream => [[],['nullstream]]
   a:=CAAR stream
   if #a >= 8 and SUBSTRING(a,0,8)='")package"
   then shoePackageStartsAt(cons(CAAR stream,lines),sz,name,CDR stream)
   else
     if #a<sz
     then shoePackageStartsAt(lines, sz,name,CDR stream)
     else if SUBSTRING(a,0,sz)=name and (#a>sz and not shoeIdChar(a.sz))
          then [lines,stream]
          else shoePackageStartsAt(lines,sz,name,CDR stream)

stripm (x,pk,bt)==
   ATOM x =>
             IDENTP x =>
                 SYMBOL_-PACKAGE x = bt => INTERN(PNAME x,pk)
                 x
             x
   CONS(stripm(CAR x,pk,bt),stripm(CDR x,pk,bt))

shoePCompile  fn==
    fn:=stripm(fn,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
    fn is ['DEFUN,name,bv,:body]=>
          COMPILE (name,['LAMBDA,bv,:body])
    EVAL fn

FC(name,fn)==
   $bfClamming:local:=false
   $GenVarCounter:local := 0
   infn:=shoeAddbootIfNec fn
   handle_input_file(infn, FUNCTION shoeFindName, [fn, name])

shoeFindName(a, fn, name)==
     lines:=shoeFindLines(fn,name,a)
     shoePCompileTrees shoeTransformString lines

shoePCompileTrees s==
    while not bStreamPackageNull s repeat
         REALLYPRETTYPRINT shoePCompile car s
         s:=cdr s

bStreamPackageNull s == doInBoottranPackage(bStreamNull s)

PSTTOMC string==
   $GenVarCounter:local := 0
   $bfClamming:local:=false
   shoePCompileTrees shoeTransformString string

BOOTLOOP ()==
    a:=READ_-LINE()
    #a=0=>
         WRITE_-LINE '"Boot Loop; to exit type ] "
         BOOTLOOP()
    b:=shoePrefix? ('")console",a)
    b =>
         stream:= _*TERMINAL_-IO_*
         PSTTOMC bRgen stream
         BOOTLOOP()
    a.0='"]".0 => nil
    PSTTOMC [a]
    BOOTLOOP()

BOOTPO ()==
    a:=READ_-LINE()
    #a=0=>
         WRITE_-LINE '"Boot Loop; to exit type ] "
         BOOTPO()
    b:=shoePrefix? ('")console",a)
    b =>
         stream:= _*TERMINAL_-IO_*
         PSTOUT bRgen stream
         BOOTPO()
    a.0='"]".0 => nil
    PSTOUT [a]
    BOOTPO()

PSTOUT0 string ==
   $GenVarCounter:local := 0
   $bfClamming:local:=false
   shoeConsoleTrees shoeTransformString string

PSTOUT string == doInBoottranPackage(PSTOUT0 string)
