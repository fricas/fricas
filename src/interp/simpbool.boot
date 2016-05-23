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

simpBool x == dnf_to_pf reduceDnf b_to_dnf x

reduceDnf u ==
-- (OR (AND ..b..) b) ==> (OR  b  )
  atom u => u
  for x in u repeat
    ok := true
    for y in u repeat
      x = y => 'skip
      dnfContains(x,y) => return (ok := false)
    ok = true => acc := [x,:acc]
  NREVERSE acc

dnfContains([a,b],[c,d]) == fn(a,c) and fn(b,d) where
  fn(x,y) == and/[member(u,x) for u in y]

dnf_to_pf(x) ==
  x = 'true => 'T
  x = 'false => nil
  atom x => x
  MKPF(
    [MKPF([:[k for k in b],:[['not,k] for k in a]],'AND) for [a,b] in x],'OR)

b_to_dnf x ==
  x = 'T => 'true
  x = NIL => 'false
  atom x => bassert x
  [op,:argl] := x
  MEMQ(op,'(AND and)) => band argl
  MEMQ(op,'(OR or))   => bor argl
  MEMQ(op,'(NOT not)) => bnot first argl
  bassert x
band x ==
  x is [h, :t] => andDnf(b_to_dnf h, band t)
  'true
bor x ==
  x is [a, :b] => orDnf(b_to_dnf a, bor b)
  'false
bnot x == notDnf b_to_dnf x
bassert x == [[nil,[x]]]
bassertNot x == [[[x],nil]]
------------------------Disjunctive Normal Form Code-----------------------
--
--        dnf is  true | false | [coaf ... ]
--           the last case means disjunction of coaf-s on the list
--        coaf is true | false | [neg pos]
--           in the last case neg and pos are lists of items
--           this case means conjuntion of items in pos and
--           negations of items in neg

orDnf(a,b) ==                   -- or:  (dnf, dnf) -> dnf
  a = 'false => b
  b = 'false => a
  a = 'true or b = 'true => 'true
  null a => b     --null list means false
  a is [c] = coafOrDnf(c,b)
  coafOrDnf(first a,orDnf(rest a,b))

andDnf(a,b) ==                  -- and: (dnf, dnf) -> dnf
  a = 'true => b
  b = 'true => a
  a = 'false or b = 'false => 'false
  null a => 'false  --null list means false
  a is [c] => coafAndDnf(c,b)
  x := coafAndDnf(first a,b)
  y := andDnf(rest a,b)
  x = 'false => y
  y = 'false => x
  ordUnion(x,y)

notDnf l ==                     -- not: dnf  ->  dnf
  l = 'true => 'false
  l = 'false => 'true
  null l =>     'true --null list means false
  l is [x] => notCoaf x
  andDnf(notCoaf first l,notDnf rest l)

coafOrDnf(a,l) ==               -- or:  (coaf, dnf) -> dnf
  a = 'true or l = 'true => 'true
  a = 'false => l
  member(a,l) => l
  y := notCoaf a
  x := ordIntersection(y,l)
  null x => ordUnion([a], l)
  x = y => 'true
  ordUnion(notDnf ordSetDiff(y,x),l)

coafAndDnf(a,b) ==              --and: (coaf, dnf) -> dnf
  a = 'true => b
  a = 'false => 'false
  [c,:r] := b
  x := coafAndCoaf(a, c)      --dnf
  null r => x
  y := coafAndDnf(a,r)       --dnf
  x = 'false => y
  y = 'false => x
  ordUnion(x,y)

coafAndCoaf([a,b],[p,q]) ==                  --and: (coaf,coaf) -> dnf
  ordIntersection(a,q) or ordIntersection(b,p) => 'false
  [[ordUnion(a,p),ordUnion(b,q)]]

notCoaf [a,b] ==
    orderList([:[[nil, [x]] for x in a], :[[[x], nil] for x in b]])

ordUnion(a,b) ==
  a isnt [c,:r] => b
  b isnt [d,:s] => a
  c=d => [c,:ordUnion(r,s)]
  ?ORDER(c, d) => [c, :ordUnion(r, b)]
  [d,:ordUnion(s,a)]
ordIntersection(a,b) ==
  a isnt [h,:t] => nil
  member(h,b) => [h,:ordIntersection(t,b)]
  ordIntersection(t,b)
ordSetDiff(a,b) ==
  b isnt [h,:t] => a
  member(h,a) => ordSetDiff(delete(h,a),t)
  ordSetDiff(a,t)
-------------
testPredList u ==
  for x in u repeat
    y := simpBool x
    x = y => nil
    pp x
    pp '"==========>"
    pp y
