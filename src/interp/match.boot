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

DEFPARAMETER($wildCard, char "*")

maskMatch?(mask,subject) ==
  null mask => true
  if null STRINGP subject then subject := PNAME subject
  or/[match?(pattern,subject) for pattern in mask]

substring?(part, whole, startpos) ==
--This function should be replaced by STRING<
  np := SIZE part
  nw := SIZE whole
  np > nw - startpos => false
  and/[CHAR_-EQUAL(ELT(part, ip), ELT(whole, iw))
      for ip in 0..np-1 for iw in startpos.. ]

anySubstring?(part,whole,startpos) ==
  np := SIZE part
  nw := SIZE whole
  or/[((k := i) and and/[CHAR_-EQUAL(ELT(part, ip),ELT(whole, iw))
       for ip in 0..np - 1 for iw in i..]) for i in startpos..nw - np] => k

charPosition(c,t,startpos) ==
  n := SIZE t
  startpos < 0 or startpos > n => n
  k:= startpos
  for i in startpos .. n-1 repeat
    c = ELT(t,i) => return nil
    k := k+1
  k

rightCharPosition(c,t,startpos) == --startpos often equals MAXINDEX t (rightmost)
  k := startpos
  for i in startpos..0 by -1 while c ~= ELT(t,i) repeat (k := k - 1)
  k

stringPosition(s,t,startpos) ==
  n := SIZE t
  if startpos < 0 or startpos > n then error "index out of range"
  if SIZE s = 0 then return startpos -- bug in STRPOS
  r := STRPOS(s,t,startpos,NIL)
  if EQ(r,NIL) then n else r

superMatch?(opattern,subject) ==  --subject assumed to be DOWNCASEd
  $wildCard : local := char '_*
  pattern := patternCheck opattern
  logicalMatch?(pattern,subject)

logicalMatch?(pattern,subject) ==  --subject assumed to be DOWNCASEd
  pattern is [op,:argl] =>
    op = "and" => and/[superMatch?(p,subject) for p in argl]
    op = "or"  =>  or/[superMatch?(p,subject) for p in argl]
    op = "not" =>  not superMatch?(first argl,subject)
    systemError '"unknown pattern form"
  basicMatch?(pattern,subject)

patternCheck pattern == main where
 --checks for escape characters, maybe new $wildCard
  main ==
--  pattern := pmTransFilter pattern   --should no longer need this (rdj:10/1/91)
    u := pos(char '__,pattern)
    null u => pattern
    not(and/[equal(pattern,i + 1,$wildCard) for i in u]) =>
      sayBrightly ['"Invalid use of underscores in pattern: ",pattern]
      '"!!!!!!!!!!!!!!"
    c := wild(pattern,'(_$ _# _% _& _@))
--  sayBrightlyNT ['"Choosing new wild card"]
--  pp c
    $oldWild  :local := $wildCard
    $wildCard := c
    pattern := mknew(pattern,first u,rest u,SUBSTRING(pattern,0,first u))
--  sayBrightlyNT ['"Replacing pattern by"]
--  pp pattern
    pattern
  mknew(old,i,r,new) ==
    new := STRCONC(new,old.(i + 1))  --add underscored character to string
    null r => STRCONC(new,subWild(SUBSTRING(old,i + 2,nil),0))
    mknew(old,first r,rest r,
          STRCONC(new,subWild(SUBSTRING(old,i + 2,(first r) - i - 1),i + 1)))
  subWild(s,i) ==
    (k := charPosition($oldWild,s,i)) < #s =>
      STRCONC(SUBSTRING(s,i,k - i),$wildCard,subWild(s,k + 1))
    SUBSTRING(s,i,nil)
  pos(c,s) ==
    i := 0
    n := MAXINDEX s
    acc := nil
    repeat
      k := charPosition(c,s,i)
      k > n => return NREVERSE acc
      acc := [k,:acc]
      i := k + 1
  equal(p,n,c) ==
    n > MAXINDEX p => false
    p.n = c
  wild(p,u) ==
    for id in u repeat
      c := char id
      not(or/[p.i = c for i in 0..MAXINDEX(p)]) => return c

match?(pattern,subject) ==  --returns index of first character that matches
  basicMatch?(pattern,DOWNCASE subject)

basicMatch?(pattern,target) ==
  n := #pattern
  p := charPosition($wildCard,pattern,0)
  p = n => (pattern = target) and 0
  if p ~= 0 then
     -- pattern does not begin with a wild card
     ans := 0
     s := SUBSTRING(pattern,0,p) --[pattern.i for i in 0..p-1]
     not substring?(s,target,0) => return false
  else if n = 1 then return 0
  i := p   -- starting position for searching the target
  q := charPosition($wildCard,pattern,p+1)
  ltarget := #target
  while q ~= n repeat
     s := SUBSTRING(pattern,p+1,q-p-1) --[pattern.i for i in (p+1..q-1)]
     i := stringPosition(s,target,i)
     if null ans then ans := stringPosition(s,target,p)
     -- for patterns beginning with wildcard, ans gives position of first match
     if i = ltarget then return (returnFlag := true)
     i := i + #s
     p := q
     q := charPosition($wildCard,pattern,q+1)
  returnFlag => false
  if p ~= q-1 then
     -- pattern does not end with a wildcard
     s := SUBSTRING(pattern,p+1,q-p-1) --[pattern.i for i in (p+1..q-1)]
     if not suffix?(s,target) then return false
     if null ans then ans := 1  --pattern is a word preceded by a *
  ans

stringMatches?(pattern, subject) ==
    FIXP basicMatch?(pattern,subject) => true
    false

matchSegment?(pattern,subject,k) ==
  matchAnySegment?(pattern,DOWNCASE subject,k,nil)

matchAnySegment?(pattern,target,k,nc) ==  --k = start position; nc=#chars or NIL
  n := #pattern
  p := charPosition($wildCard,pattern,0)
  p = n =>
    m := stringPosition(pattern,target,k)
    m = #target => nil
    null nc => true
    m <= k + nc - n
  if k ~= 0 and nc then
    target := SUBSTRING(target,k,nc)
    k := 0
  if p ~= 0 then
     -- pattern does not begin with a wild card
     ans := 0
     s := SUBSTRING(pattern,0,p) --[pattern.i for i in 0..p-1]
     not substring?(s,target,k) => return false
  else if n = 1 then return true
  i := p + k  -- starting position for searching the target
  q := charPosition($wildCard,pattern,p+1)
  ltarget := #target
  while q ~= n repeat
     s := SUBSTRING(pattern,p+1,q-p-1) --[pattern.i for i in (p+1..q-1)]
     i := stringPosition(s,target,i)
     if i = ltarget then return (returnFlag := true)
     i := i + #s
     p := q
     q := charPosition($wildCard,pattern,q+1)
  returnFlag => false
  if p ~= q-1 then
     -- pattern does not end with a '&
     s := SUBSTRING(pattern,p+1,q-p-1) --[pattern.i for i in (p+1..q-1)]
     if not suffix?(s,target) then return false
     if null ans then ans := 1  --pattern is a word preceded by a *
  true

infix?(s,t,x) == #s + #t >= #x and prefix?(s,x) and suffix?(t,x)

prefix?(s,t) == substring?(s,t,0)

suffix?(s,t) ==
  m := #s; n := #t
  if m > n then return false
  substring?(s,t,(n-m))
