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
shoeFirstTokPosn t== shoeTokPosn CAAR t
shoeLastTokPosn  t== shoeTokPosn CADR t
shoePileColumn t==CDR shoeTokPosn CAAR t
 
-- s is a token-dq-stream
 
shoePileInsert (s)==
     if bStreamNull s
     then cons([],s)
     else
         toktype:=shoeTokType CAAAR s
         if toktype ="LISP"  or toktype = "LINE"
         then cons([car s],cdr s)
         else
            a:=shoePileTree(-1,s)
            cons([a.2],a.3)
 
shoePileTree(n,s)==
    if bStreamNull s
    then [false,n,[],s]
    else
        [h,t]:=[car s,cdr s]
        hh:=shoePileColumn h
        if hh > n
        then shoePileForests(h,hh,t)
        else [false,n,[],s]
 
eqshoePileTree(n,s)==
    if bStreamNull s
    then [false,n,[],s]
    else
        [h,t]:=[car s,cdr s]
        hh:=shoePileColumn h
        if hh = n
        then shoePileForests(h,hh,t)
        else [false,n,[],s]
 
shoePileForest(n,s)==
     [b,hh,h,t]:= shoePileTree(n,s)
     if b
     then
       [h1,t1]:=shoePileForest1(hh,t)
       [cons(h,h1),t1]
     else [[],s]
 
shoePileForest1(n,s)==
     [b,n1,h,t]:= eqshoePileTree(n,s)
     if b
     then
       [h1,t1]:=shoePileForest1(n,t)
       [cons(h,h1),t1]
     else [[],s]
 
shoePileForests(h,n,s)==
      [h1,t1]:=shoePileForest(n,s)
      if bStreamNull h1
      then [true,n,h,s]
      else shoePileForests(shoePileCtree(h,h1),n,t1)
 
shoePileCtree(x,y)==dqAppend(x,shoePileCforest y)
 
-- only enshoePiles forests with >=2 trees
 
shoePileCforest x==
   if null x
   then []
   else if null cdr x
        then car x
        else
           a:=car x
           b:=shoePileCoagulate(a,rest x)
           if null cdr b
           then car b
           else shoeEnPile shoeSeparatePiles b
 
shoePileCoagulate(a,b)==
    if null b
    then [a]
    else
      c:=car b
      if EQ(shoeTokPart CAAR c,"THEN") or EQ(shoeTokPart CAAR c,"ELSE")
      then shoePileCoagulate (dqAppend(a,c),cdr b)
      else
         d:=CADR a
         e:=shoeTokPart d
         if EQCAR(d,"KEY") and
               (GET(e,"SHOEINF") or EQ(e,"COMMA") or EQ(e,"SEMICOLON"))
         then shoePileCoagulate(dqAppend(a,c),cdr b)
         else cons(a,shoePileCoagulate(c,rest b))
 
shoeSeparatePiles x==
  if null x
  then []
  else if null cdr x
       then car x
       else
         a:=car x
         semicolon:=dqUnit
                shoeTokConstruct("KEY", "BACKSET",shoeLastTokPosn a)
         dqConcat [a,semicolon,shoeSeparatePiles cdr x]
 
shoeEnPile x==
   dqConcat [dqUnit shoeTokConstruct("KEY","SETTAB",shoeFirstTokPosn x),
             x, _
             dqUnit shoeTokConstruct("KEY","BACKTAB",shoeLastTokPosn  x)]
 
