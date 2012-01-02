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
EQCAR(x,y)== CONSP x and EQ(CAR x,y)
 
dqUnit s==(a:=[s];CONS(a,a))
 
dqAppend(x,y)==
    if null x
    then y
    else if null y
         then x
         else
              RPLACD (CDR x,CAR y)
              RPLACD (x,    CDR y)
              x
 
dqConcat ld==
    if null ld
    then nil
    else if null rest ld
         then first ld
         else dqAppend(first ld,dqConcat rest ld)
 
dqToList s==if null s then nil else CAR s
 
shoeConstructToken(ln,lp,b,n)==[b.0,b.1,:cons(lp,n)]
shoeTokType x== CAR x
shoeTokPart x== CADR x
shoeTokPosn x== CDDR x
shoeTokConstruct(x,y,z)==[x,y,:z]

DEFVAR($f)
DEFVAR($floatok)
DEFVAR($linepos)
DEFVAR($ln)
DEFVAR($n)
DEFVAR($r)
DEFVAR($sz)

shoeNextLine(s)==
     if bStreamNull s
     then false
     else
       $linepos:=s
       $f:= CAR s
       $r:= CDR s
       $ln:=CAR $f
       $n:=STRPOSL('" ",$ln,0,true)
       $sz :=# $ln
       null $n => true
       QENUM($ln,$n)=shoeTAB =>
                  a:=MAKE_-FULL_-CVEC (7-REM($n,8) ,'" ")
                  $ln.$n:='" ".0
                  $ln:=CONCAT(a,$ln)
                  s1:=cons(cons($ln,CDR $f),$r)
                  shoeNextLine s1
       true
 
shoeLineToks(s)==
   $f: local:=nil
   $r:local :=nil
   $ln:local :=nil
   $n:local:=nil
   $sz:local := nil
   $floatok:local:=true
   $linepos:local:=s
   not shoeNextLine s =>  CONS(nil,nil)
   null $n => shoeLineToks $r
   fst:=QENUM($ln,0)
   EQL(fst,shoeCLOSEPAREN)=>
            command:=shoeLine? $ln=>
              dq:=dqUnit shoeConstructToken
                       ($ln,$linepos,shoeLeafLine command,0)
              cons([dq],$r)
            command:=shoeLisp? $ln=> shoeLispToken($r,command)
            command:=shoePackage? $ln=>
       --       z:=car shoeBiteOff command
              a:=CONCAT('"(IN-PACKAGE ",command,'")")
              dq:=dqUnit shoeConstructToken
                       ($ln,$linepos,shoeLeafLisp a,0)
              cons([dq],$r)
            shoeLineToks $r
   toks:=[]
   while $n<$sz repeat toks:=dqAppend(toks,shoeToken())
   null toks => shoeLineToks $r
   cons([toks],$r)
 
shoeLispToken(s,string)==
      string:=
            # string=0 or EQL(QENUM(string,0),QENUM('";",0))=> '""
            string
      ln:=$ln
      linepos:=$linepos
      [r,:st]:=shoeAccumulateLines(s,string)
      dq:=dqUnit shoeConstructToken(ln,linepos,shoeLeafLisp st,0)
      cons([dq],r)
 
shoeAccumulateLines(s,string)==
   not shoeNextLine s =>  CONS(s,string)
   null $n => shoeAccumulateLines($r,string)
   # $ln=0 => shoeAccumulateLines($r,string)
   fst:=QENUM($ln,0)
   EQL(fst,shoeCLOSEPAREN)=>
            command:=shoeLisp? $ln
            command and #command>0 =>
                EQL(QENUM(command,0),QENUM('";",0))=>
                            shoeAccumulateLines($r,string)
                a:=STRPOS('";",command,0,nil)
                a=>
                  shoeAccumulateLines($r,
                     CONCAT(string,SUBSTRING(command,0,a-1)))
                shoeAccumulateLines($r,CONCAT(string,command))
            shoeAccumulateLines($r,string)
   CONS(s,string)
 
shoeToken () ==
      ln:=$ln
      c:=QENUM($ln,$n)
      linepos:=$linepos
      n:=$n
      ch:=$ln.$n
      b:=
            shoeStartsComment()          =>
                           shoeComment()
                           []
            shoeStartsNegComment()       =>
                           shoeNegComment()
                           []
            c=shoeLispESCAPE      =>
                           shoeLispEscape()
            shoePunctuation c        => shoePunct ()
            shoeStartsId ch          => shoeWord  (false)
            c=shoeSPACE              =>
                           shoeSpace ()
                           []
            c = shoeSTRING_CHAR        => shoeString ()
            shoeDigit ch               => shoeNumber ()
            c=shoeESCAPE               => shoeEscape()
            c=shoeTAB                  =>
                                       $n:=$n+1
                                       []
            shoeError ()
      null b => nil
      dqUnit shoeConstructToken(ln,linepos,b,n)
 
-- to pair badge and badgee
shoeLeafId x==  ["ID",INTERN x]
 
shoeLeafKey x==["KEY",shoeKeyWord x]
 
shoeLeafInteger x==["INTEGER",shoeIntValue x]
 
shoeLeafFloat(a,w,e)==
    b:=shoeIntValue CONCAT(a,w)
    c:=DOUBLE b *  EXPT(DOUBLE 10, e-#w)
    ["FLOAT",c]
 
shoeLeafString x  == ["STRING",x]
 
shoeLeafLisp x    == ["LISP",x]
shoeLeafLispExp x    == ["LISPEXP",x]
 
shoeLeafLine x    == ["LINE",x]
 
shoeLeafComment x == ["COMMENT", x]
 
shoeLeafNegComment x== ["NEGCOMMENT", x]
 
shoeLeafError x   == ["ERROR",x]
 
shoeLeafSpaces x  == ["SPACES",x]
 
shoeLispEscape()==
         $n:=$n+1
         if $n>=$sz
         then
             SoftShoeError(cons($linepos,$n),'"lisp escape error")
             shoeLeafError ($ln.$n)
         else
            a:=shoeReadLispString($ln,$n)
            null a =>
             SoftShoeError(cons($linepos,$n),'"lisp escape error")
             shoeLeafError ($ln.$n)
            [exp,n]:=a
            null n =>
                 $n:= $sz
                 shoeLeafLispExp  exp
            $n:=n
            shoeLeafLispExp  exp
shoeEscape()==
         $n:=$n+1
         a:=shoeEsc()
         if a then shoeWord true else nil
 
shoeEsc()==
     if $n>=$sz
     then if shoeNextLine($r)
          then
               while null $n repeat shoeNextLine($r)
               shoeEsc()
               false
          else false
     else
           n1:=STRPOSL('" ",$ln,$n,true)
           if null n1
           then
               shoeNextLine($r)
               while null $n repeat shoeNextLine($r)
               shoeEsc()
               false
           else true
 
shoeStartsComment()==
    if $n<$sz
    then
         if QENUM($ln,$n)=shoePLUSCOMMENT
         then
            www:=$n+1
            if www>=$sz
            then false
            else QENUM($ln,www) = shoePLUSCOMMENT
         else false
    else false
 
shoeStartsNegComment()==
    if $n< $sz
    then
         if QENUM($ln,$n)=shoeMINUSCOMMENT
         then
            www:=$n+1
            if www>=$sz
            then false
            else QENUM($ln,www) = shoeMINUSCOMMENT
         else false
    else false
 
shoeNegComment()==
      n:=$n
      $n:=$sz
      shoeLeafNegComment SUBSTRING($ln,n,nil)
 
shoeComment()==
      n:=$n
      $n:=$sz
      shoeLeafComment SUBSTRING($ln,n,nil)
 
shoePunct()==
            sss:=shoeMatch($ln,$n)
            $n:=$n+#sss
            shoeKeyTr sss
 
shoeKeyTr w==
       if EQ(shoeKeyWord w,"DOT")
       then if $floatok
            then shoePossFloat(w)
            else shoeLeafKey w
       else
           $floatok:=not shoeCloser w
           shoeLeafKey w
 
shoePossFloat (w)==
     if $n>=$sz or not shoeDigit $ln.$n
     then shoeLeafKey w
     else
       w:=shoeInteger()
       shoeExponent('"0",w)
 
 
shoeSpace()==
           n:=$n
           $n:=STRPOSL('" ",$ln,$n,true)
           $floatok:=true
           if null $n
           then
              shoeLeafSpaces 0
              $n:= # $ln
           else shoeLeafSpaces ($n-n)

shoe_e_concat(s1, s2) ==
    #s2 = 0 => s1
    shoeIdChar(s2.0) => CONCAT(s1, "__", s2)
    CONCAT(s1, s2) 

shoeString()==
            $n:=$n+1
            $floatok:=false
            shoeLeafString shoeS ()

shoeS()==
   if $n>=$sz
   then
     SoftShoeError(cons($linepos,$n),'"quote added")
     '""
   else
           n:=$n
           strsym :=STRPOS ('"_"",$ln,$n,nil) or $sz
           escsym:=STRPOS ('"__"
                          ,$ln,$n,nil)  or $sz
           mn:=MIN(strsym,escsym)
           if mn=$sz
           then
                 $n:=$sz
                 SoftShoeError(cons($linepos,$n),'"quote added")
                 SUBSTRING($ln,n,nil)
           else if mn=strsym
                then
                   $n:=mn+1
                   SUBSTRING($ln,n,mn-n)
                else
                  str:=SUBSTRING($ln,n,mn-n)
                  $n:=mn+1
                  a:=shoeEsc()
                  b:=if a
                     then
                       ec := $ln.$n
                       $n:=$n+1
                       CONCAT(ec, shoeS())
                     else shoeS()
                  shoe_e_concat(str, b)

shoeIdEnd(line,n)==
     while n<#line and shoeIdChar line.n repeat n:=n+1
     n

shoeDigit x== DIGIT_-CHAR_-P x

shoeW(b)==
       n1:=$n
       $n:=$n+1
       l:=$sz
       endid:=shoeIdEnd($ln,$n)
       if endid=l or (not(QENUM($ln,endid) = shoeESCAPE))
       then
           $n:=endid
           [b,SUBSTRING($ln,n1,endid-n1)]
       else
           str:=SUBSTRING($ln,n1,endid-n1)
           $n:=endid+1
           a:=shoeEsc()
           bb:=if a
               then shoeW(true)
               else [b,'""]   --  escape finds space or newline
           -- [bb.0 or b,CONCAT(str,bb.1)]
           [bb.0 or b, shoe_e_concat(str, bb.1)]

shoeWord(esp) ==
          aaa:=shoeW(false)
          w:=aaa.1
          $floatok:=false
          if esp or aaa.0
          then shoeLeafId w
          else if shoeKeyWordP w
               then
                  $floatok:=true
                  shoeLeafKey w
               else shoeLeafId  w

shoeInteger()==shoeInteger1(false)

shoeInteger1(zro) ==
       n:=$n
       l:= $sz
       while $n<l and shoeDigit($ln.$n) repeat $n:=$n+1
       if $n=l or (not(QENUM($ln,$n) = shoeESCAPE))
       then if n=$n and zro
            then '"0"
            else SUBSTRING($ln,n,$n-n)
       else
             str:=SUBSTRING($ln,n,$n-n)
             $n:=$n+1
             a:=shoeEsc()
             bb:=shoeInteger1(zro)
             CONCAT(str,bb)
 
shoeIntValue(s) ==
       ns := #s
       ival := 0
       for i in 0..ns-1 repeat
           d := shoeOrdToNum ELT(s,i)
           ival := 10*ival + d
       ival
 
shoeNumber() ==
       a := shoeInteger()
       if $n>=$sz
       then shoeLeafInteger a
       else
           if $floatok and QENUM($ln,$n)=shoeDOT
           then
             n:=$n
             $n:=$n+1
             if  $n<$sz and QENUM($ln,$n)=shoeDOT
             then
               $n:=n
               shoeLeafInteger a
             else
               w:=shoeInteger1(true)
               shoeExponent(a,w)
           else shoeLeafInteger a
 
shoeExponent(a,w)==
     if $n>=$sz
     then shoeLeafFloat(a,w,0)
     else
        n:=$n
        c:=QENUM($ln,$n)
        if c=shoeEXPONENT1 or c=shoeEXPONENT2
        then
           $n:=$n+1
           if $n>=$sz
           then
             $n:=n
             shoeLeafFloat(a,w,0)
           else if shoeDigit($ln.$n)
                then
                  e:=shoeInteger()
                  e:=shoeIntValue e
                  shoeLeafFloat(a,w,e)
                else
                  c1:=QENUM($ln,$n)
                  if c1=shoePLUSCOMMENT or c1=shoeMINUSCOMMENT
                  then
                    $n:=$n+1
                    if $n>=$sz
                    then
                      $n:=n
                      shoeLeafFloat(a,w,0)
                    else
                      if shoeDigit($ln.$n)
                      then
                        e:=shoeInteger()
                        e:=shoeIntValue e
                        shoeLeafFloat(a,w,
                          (if c1=shoeMINUSCOMMENT then MINUS e else e))
                      else
                        $n:=n
                        shoeLeafFloat(a,w,0)
        else shoeLeafFloat(a,w,0)
 
shoeError()==
      n:=$n
      $n:=$n+1
      SoftShoeError(cons($linepos,n),
        CONCAT( '"The character whose number is ",
                STRINGIMAGE QENUM($ln,n),'" is not a Boot character"))
      shoeLeafError ($ln.n)
 
shoeOrdToNum x== DIGIT_-CHAR_-P x
 
shoeKeyWord st   == GETHASH(st,shoeKeyTable)
 
shoeKeyWordP st  ==  not null GETHASH(st,shoeKeyTable)
 
shoeInsert(s,d) ==
      l := #s
      h := QENUM(s,0)
      u := ELT(d,h)
      n := #u
      k:=0
      while l <= #(ELT(u,k)) repeat
          k:=k+1
      v := MAKE_-VEC(n+1)
      for i in 0..k-1 repeat VEC_-SETELT(v,i,ELT(u,i))
      VEC_-SETELT(v,k,s)
      for i in k..n-1 repeat VEC_-SETELT(v,i+1,ELT(u,i))
      VEC_-SETELT(d,h,v)
      s
 
shoeMatch(l,i)==shoeSubStringMatch(l,shoeDict,i)
 
shoeSubStringMatch (l,d,i)==
       h:= QENUM(l, i)
       u:=ELT(d,h)
       ll:=SIZE l
       done:=false
       s1:='""
       for j in 0.. SIZE u - 1 while not done repeat
          s:=ELT(u,j)
          ls:=SIZE s
          done:=if ls+i > ll
                then false
                else
                 eql:= true
                 for k in 1..ls-1 while eql repeat
                    eql:= EQL(QENUM(s,k),QENUM(l,k+i))
                 if eql
                 then
                   s1:=s
                   true
                 else false
       s1
 
shoePunctuation c== shoePun.c =1
 
shoeKeyTableCons()==
   KeyTable:=MAKE_-HASHTABLE("CVEC")
   for st in shoeKeyWords repeat
      HPUT(KeyTable,CAR st,CADR st)
   KeyTable
 
shoeDictCons()==
      l:= HKEYS shoeKeyTable
      d :=
          a:=MAKE_-VEC(256)
          b:=MAKE_-VEC(1)
          VEC_-SETELT(b,0,MAKE_-CVEC 0)
          for i in 0..255 repeat VEC_-SETELT(a,i,b)
          a
      for s in l repeat shoeInsert(s,d)
      d
 
 
shoePunCons()==
    listing := HKEYS shoeKeyTable
    a:=MAKE_-BVEC 256
    for i in 0..255 repeat BVEC_-SETELT(a,i,0)
    for k in listing repeat
       if not shoeStartsId k.0
       then BVEC_-SETELT(a,QENUM(k,0),1)
    a
