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

--% Transformation of Parser Output

)if false
This is the top-level function in this file.

When parsing spad code we walk an source code expression such as

[[P ==> PositiveInteger]]

This gets translated by [[|postTransform|]]\cite{1} into

[[(MDEF P NIL NIL (|PositiveInteger|))]]

[[|parseTranform|]] is called with this expression. The [[%]] symbol,
which represents the current domain, is replaced with the [[$]] symbol
internally. This hack was introduced because the Aldor compiler wanted
to use [[%]] for the [[current domain]]. The Spad compiler used [[$]].
In order not to have to change this everywhere we do a subsitution here.
)endif

parseTransform x ==
  $defOp: local:= nil
  x := SUBST('$, '%, x) -- for new compiler compatibility
  parseTran x

)if false
[[|parseTran|]] sees an expression such as

[[(MDEF P NIL NIL (|PositiveInteger|))]]

It walks the
expression, which is a list, item by item (note the tail recursive
call in this function). In general, we are converting certain
source-level constructs into internal constructs. Note the subtle
way that functions get called in this file. The information about
what function to call is stored on the property list of the symbol.

For example, given the form: [[(|has| S (|OrderedSet|))]]
the symbol [[|has|]] occurs in the car of the list. [[|parseTran|]]
assigns [[$op]] to be [[|has|]] and [[argl]] to be the list
[[(S (|OrderedSet|))]]. Next, a local function [[g]], which checks
for the compile-time elts, returns [[$op]] unchanged. The variable
[[u]] is set to [[|has|]].

Since [[|has|]] is an atom we do
[[(GET '|has| '|parseTran|)]] which returns [[|parseHas|]]
because the symbol [[|has|]] contains the association
[[|parseTran| |parseHas|]] on it's symbol property list.
You can see this by calling [[(symbol-plist '|has|)]].

This ends up calling [[(|parseHas| '(S (|OrderedSet|)))]].

The [[|parseTran|]] function walks the entire s-expression
calling special parsers for various special forms in the input.
This does things like reverse tests so that [[(if (not x) a b)]]
becomes [[(if x b a)]], etc.
)endif

parseTran x ==
  atom x => parseAtom x
  [op, :argl] := x
  u := (op is ['Sel, ., x] => x; op)
  SYMBOLP(u) and (fn := GET(u, 'parseTran)) =>
      if op ~= u then SAY(["parseTran op ~= u", op, u])
      FUNCALL(fn, argl)
  [parseTran op, :parseTranList argl]


parseAtom x ==
 -- next line for compatibility with new compiler
  x = 'break => parseLeave ['$NoValue]
  x

parseTranList l == [parseTran(y) for y in l]

parseHas [x,y] ==
  mkand [['has,x,u] for u in fn y] where
    mkand x ==
      x is [a] => a
      ["and",:x]
    fn y ==
      y is [":" ,op,['Mapping,:map]] =>
         op:= (STRINGP op => INTERN op; op)
         [['SIGNATURE,op,map]]
      y is ['Join,:u] => "append"/[fn z for z in u]
      y is ['CATEGORY,:u] => "append"/[fn z for z in u]
      y is ['SIGNATURE,:.] => [y]
      [makeNonAtomic y]

parseDEF [lhs,tList,specialList,body] ==
  setDefOp lhs
  ['DEF, parseLhs lhs, parseTranList tList, parseTranList specialList,
    parseTran(body)]

parseLhs x ==
  atom x => parseTran x
  atom first x => [parseTran first x, :[parseTran y for y in rest x]]
  parseTran x

parseMDEF [lhs,tList,specialList,body] ==
  ['MDEF, parseTran lhs, parseTranList tList, parseTranList specialList,
    parseTran(body)]

parseCategory x ==
  l:= parseTranList x
  key:=
    CONTAINED("$",l) => "domain"
    'package
  ['CATEGORY,key,:l]

parseAnd u ==
  null u => 'true
  null rest u => first u
  parseIf [parseTran first u,parseAnd rest u,"false"]

parseOr u ==
  null u => 'false
  null rest u => first u
  (x:= parseTran first u) is ['not,y] => parseIf [y,parseOr rest u,'true]
  true => parseIf [x,'true,parseOr rest u]

parseNot u ==
  parseTran ['IF,first u,:'(false true)]

parseExit [a,:b] ==
  --  note: I wanted to convert 1s to 0s here to facilitate indexing in
  --   comp code; unfortunately, parseTran-ning is sometimes done more
  --   than once so that the count can be decremented more than once
  a:= parseTran a
  b:= parseTran b
  b =>
    null INTEGERP a =>
      (MOAN('"first arg ",a,'" for exit must be integer"); ['exit,1,a])
    ['exit,a,:b]
  ['exit,1,a]

parseLeave [a,:b] ==
  a:= parseTran a
  b:= parseTran b
  b =>
    null INTEGERP a =>
      (MOAN('"first arg ",a,'" for 'leave' must be integer"); ['leave,1,a])
    ['leave,a,:b]
  ['leave,1,a]

parseJoin l ==
  ['Join,:fn parseTranList l] where
    fn l ==
      null l => nil
      l is [['Join,:x],:y] => [:x,:fn y]
      [first l,:fn rest l]

parseSegment p ==
  p is [a,b] =>
    b => ['SEGMENT,parseTran a, parseTran b]
    ['SEGMENT,parseTran a]
  ['SEGMENT,:p]

parseIf t ==
  t isnt [p,a,b] => t
  ifTran(parseTran p,parseTran a,parseTran b) where
    ifTran(p,a,b) ==
      p = 'true  => a
      p = 'false  => b
      p is ['not,p'] => ifTran(p',b,a)
      p is ['IF,p',a',b'] => ifTran(p',ifTran(a',COPY a,COPY b),ifTran(b',a,b))
      p is ['SEQ,:l,['exit,1,p']] =>
        ['SEQ,:l,['exit,1,ifTran(p',incExitLevel a,incExitLevel b)]]
         --this assumes that l has no exits
      a is ['IF, =p,a',.] => ['IF,p,a',b]
      b is ['IF, =p,.,b'] => ['IF,p,a,b']
      makeSimplePredicateOrNil p is ['SEQ,:s,['exit,1,val]] =>
        parseTran ['SEQ,:s,['exit,1,incExitLevel ['IF,val,a,b]]]
      ['IF,p,a,b]

makeSimplePredicateOrNil p ==
  isSimple p => nil
  u:= isAlmostSimple p => u
  true => wrapSEQExit [['LET, [":", g := GENSYM(), ["Boolean"]], p], g]

parseSeq l ==
  not (l is [:.,['exit,:.]]) =>
    postError ['"   Invalid ending to block: ",last l]
  transSeq(parseTranList(l))

transSeq l ==
  null l => nil
  null rest l => decExitLevel first l
  [item,:tail]:= l
  item is ['SEQ,:l,['exit,1,['IF,p,['exit, =2,q],'noBranch]]] and
    (and/[x is ['LET,:.] for x in l]) =>
      ['SEQ,:[decExitLevel x for x in l],['exit,1,['IF,decExitLevel p,
        decExitLevel q,transSeq tail]]]
  item is ['IF,a,['exit,1,b],'noBranch] =>
    ['IF,decExitLevel a,decExitLevel b,transSeq tail]
  item is ['IF,a,'noBranch,['exit,1,b]] =>
    ['IF,decExitLevel a,transSeq tail,decExitLevel b]
  (y:= transSeq tail) is ['SEQ,:s] => ['SEQ,item,:s]
  ['SEQ,item,['exit,1,incExitLevel y]]
