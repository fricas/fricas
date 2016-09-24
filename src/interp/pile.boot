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

-- Dequeue functions

-- dqUnit makes a unit dq i.e. a dq with one item, from the item

-- dqAppend appends 2 dq's, destroying the first

-- dqConcat concatenates a list of dq's, destroying all but the last

-- dqToList transforms a dq to a list

dqUnit s==(a:=[s];CONS(a,a))

dqAppend(x,y)==
    if null x
    then y
    else if null y
         then x
         else
              RPLACD(CDR x, first y)
              RPLACD (x,    CDR y)
              x

dqConcat ld==
    if null ld
    then nil
    else if null rest ld
         then first ld
         else dqAppend(first ld,dqConcat rest ld)

dqToList s == if null s then nil else first s

-- Pile functions

-- insertpiles converts a line-list to a line-forest where

-- a line is a token-dequeue and has a column which is an integer.
-- an A-forest is an A-tree-list
-- an A-tree has a root which is an A, and subtrees which is an A-forest.

-- A forest with more than one tree corresponds to a Scratchpad pile
-- structure (t1;t2;t3;...;tn), and a tree corresponds to a pile item.
-- The ( ; and ) tokens are inserted into a >1-forest, otherwise
-- the root of the first tree is concatenated with its forest.
-- column t is the number of spaces before the first non-space in line t

pileColumn t == rest tokPosn CAAR t
pileComment t== EQ(tokType CAAR t,"negcomment")
pilePlusComment t== EQ(tokType CAAR t,"comment")

-- insertpile is used by next so s is non-null
-- bite off a line-tree, return it and the remaining line-list.

countParens(s, opar, cpar) ==
   ress := 0
   for stok in dqToList s repeat
      t := tokPart stok
      if EQ(CAAR stok,"key") and EQ(t, opar) then
          ress := ress + 1
      if EQ(CAAR stok,"key") and EQ(t, cpar) then
          ress := ress - 1
   ress

nopile (s, opar, cpar) ==
   -- SAY("nopile")
   if npNull s
   then [false,0,[],s]
   else
      [h,t]:=[car s,cdr s]
      h := car h
      ress := h
      balance := countParens(h, opar, cpar)
      -- SAY("balance = ", balance)
      while not npNull t and balance > 0 repeat
         h := car (car t)
         t := cdr t
         ress := dqAppend(ress, h)
         balance := balance + countParens(h, opar, cpar)
         -- SAY("balance = ", balance)
      -- SAY("ress=", ress)
      -- FIXME: we should return a pair [deque, stream], but
      -- now we return nil instead of a stream
      cons([[ress]], t)

DEFPARAMETER($nopiles, false)

setNopiles (t) ==
    $nopiles := t

piles () ==
    $nopiles := false

insertpile (s)==
     $nopiles = "{" => nopile (s, "{", "}")
     $nopiles = "(" => nopile (s, "(", ")")
     if npNull s
     then [false,0,[],s]
     else
       [h,t]:=[car s,cdr s]
       if pilePlusComment h
       then
          [h1,t1]:=pilePlusComments s
          a:=pileTree(-1,t1)
          cons([pileCforest [:h1,a.2]],a.3)
       else
         stream:=CADAR s
         a:=pileTree(-1,s)
         cons([[a.2,stream]],a.3)

pilePlusComments s==
      if npNull s
      then [[],s]
      else
       [h,t]:=[car s,cdr s]
       if pilePlusComment h
       then
         [h1,t1]:=pilePlusComments t
         [cons(h,h1),t1]
       else [[],s]

pileTree(n,s)==
    if npNull s
    then [false,n,[],s]
    else
        [h,t]:=[car s,cdr s]
        hh := pileColumn first h
        if hh > n
        then pileForests(first h, hh, t)
        else [false,n,[],s]

eqpileTree(n,s)==
    if npNull s
    then [false,n,[],s]
    else
        [h,t]:=[car s,cdr s]
        hh := pileColumn first h
        if hh = n
        then pileForests(first h, hh, t)
        else [false,n,[],s]

pileForest(n,s)==
     [b,hh,h,t]:= pileTree(n,s)
     if b
     then
       [h1,t1]:=pileForest1(hh,t)
       [cons(h,h1),t1]
     else [[],s]

pileForest1(n,s)==
     [b,n1,h,t]:= eqpileTree(n,s)
     if b
     then
       [h1,t1]:=pileForest1(n,t)
       [cons(h,h1),t1]
     else [[],s]

pileForests(h,n,s)==
      [h1,t1]:=pileForest(n,s)
      if npNull h1
      then [true,n,h,s]
      else pileForests(pileCtree(h,h1),n,t1)

pileCtree(x,y)==dqAppend(x,pileCforest y)

-- only enpiles forests with >=2 trees

first_tok(t) == CAAR t
last_tok(t) == CADR t

pileCforest x==
   if null x
   then []
   else if null cdr x
        then
           f:= car x
           if EQ(tokPart first_tok(f),"if")
           then enPile f
           else f
        else enPile separatePiles x

firstTokPosn t== tokPosn first_tok(t)
lastTokPosn  t== tokPosn last_tok(t)

separatePiles x==
  if null x
  then []
  else if null cdr x
       then car x
       else
         a:=car x
         lta := tokPart(last_tok(a))
         ftb := tokPart(first_tok(car(cdr x)))
         EQ(lta, "COLON") or EQ(lta, "SEMICOLON") or EQ(lta, "(") or
           EQ(lta, "[") or EQ(lta, "{") or EQ(ftb, "in") or
             EQ(ftb, "then") or EQ(ftb, "else") or EQ(ftb, ")") or
               EQ(ftb, "]") or EQ(ftb, "}") =>
                   dqConcat [a, separatePiles cdr x]
         semicolon:=dqUnit tokConstruct("key", "BACKSET",lastTokPosn a)
         dqConcat [a,semicolon,separatePiles cdr x]

enPile x==
   dqConcat [dqUnit tokConstruct("key","SETTAB",firstTokPosn x),
             x, _
             dqUnit tokConstruct("key","BACKTAB",lastTokPosn  x)]
