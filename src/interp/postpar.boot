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

-- Yet Another Parser Transformation File
-- These functions are used by for SPAD code

postTransform y ==
  x:= y
  u:= postTran x
  if u is ["@Tuple", :l, [":", y, t]] and (and/[IDENTP x for x in l]) then
      u := [":", ['LISTOF, :l, y], t]
  postTransformCheck u
  u

displayPreCompilationErrors() ==
  n:= #($postStack:= REMDUP NREVERSE $postStack)
  n=0 => nil
  errors:=
    1<n => '"errors"
    '"error"
  heading:=
        $topOp ~= '$topOp => ['"   ",$topOp,'" has"]
        ['"   You have"]
  sayBrightly [:heading, '%b, n, '%d, '"precompilation ", errors, '":"]
  if 1<n then
    (for x in $postStack for i in 1.. repeat sayMath ['"   ",i,'"_) ",:x])
    else sayMath ['"    ",:first $postStack]
  TERPRI()

postTran x ==
  atom x =>
    postAtom x
  op := first x
  IDENTP(op) and (f := GET(op, 'postTran)) => FUNCALL(f, x)
  op is ['Sel, a, b] =>
    u:= postTran [b,:rest x]
    [postTran op,:rest u]
  op~=(y:= postOp op) => [y,:postTranList rest x]
  postForm x

postTranList x == [postTran y for y in x]

postBigFloat x ==
  [.,mant, expon] := x
  postTran [["Sel", '(Float), 'float], [",", [",", mant, expon], 10]]

postAdd ['add,a,:b] ==
  null b => postCapsule a
  ['add,postTran a,postCapsule first b]

checkWarning msg == postError concat('"Parsing error: ",msg)

checkWarningIndentation() ==
  checkWarning ['"Apparent indentation error following",:bright "add"]

postCapsule x ==
  x isnt [op,:.] => checkWarningIndentation()
  op = ";" => ['CAPSULE,:postBlockItemList postFlatten(x,";")]
  op = "if" or INTEGERP op or op = "==" => ['CAPSULE, postBlockItem x]
  checkWarningIndentation()

postQUOTE x == x

postColon u ==
  u is [":",x] => [":",postTran x]
  u is [":",x,y] => [":",postTran x,:postType y]

postColonColon u ==
  -- for Lisp package calling
  -- boot syntax is package::fun but probably need to parenthesize it
  postForm u

postAtSign ["@",x,y] == ["@",postTran x,:postType y]

postPretend ['pretend,x,y] == ['pretend,postTran x,:postType y]

postConstruct u ==
  u is ['construct,b] =>
    a:= (b is [",",:.] => comma2Tuple b; b)
    a is ['SEGMENT,p,q] => ['construct,postTranSegment(p,q)]
    a is ["@Tuple", :l] =>
      or/[x is [":",y] for x in l] => postMakeCons l
      or/[x is ['SEGMENT,:.] for x in l] => tuple2List l
      ['construct,:postTranList l]
    ['construct,postTran a]
  u

postError msg ==
  xmsg:=
    BOUNDP("$defOp") => [$defOp, '": " , :msg]
    msg
  $postStack:= [xmsg,:$postStack]
  nil

postMakeCons l ==
  null l => nil
  l is [[":",a],:l'] =>
    l' => ['append,postTran a,postMakeCons l']
    postTran a
  ['cons,postTran first l,postMakeCons rest l]

postAtom x ==
  x=0 => '(Zero)
  x=1 => '(One)
  EQ(x,'T) => 'T_$ -- rename T in spad code to T$
  IDENTP x and GETDATABASE(x,'NILADIC) => LIST x
  x

postBlockItemList l == [postBlockItem x for x in l]

postBlockItem x ==
  x:= postTran x
  x is ["@Tuple", :l, [":", y, t]] and (and/[IDENTP x for x in l]) =>
    [":",['LISTOF,:l,y],t]
  x

postCategory (u is ['CATEGORY,:l]) ==
  --RDJ: ugh_ please -- someone take away need for PROGN as soon as possible
  null l => u
  op :=
    $insidePostCategoryIfTrue = true => 'PROGN
    'CATEGORY
  [op,:[fn x for x in l]] where fn x ==
    $insidePostCategoryIfTrue: local := true
    postTran x

postComma u == postTuple comma2Tuple u

comma2Tuple u == ["@Tuple", :postFlatten(u, ",")]

postDef [defOp,lhs,rhs] ==
--+
  lhs is ["macro",name] => postMDef ["==>",name,rhs]

  recordHeaderDocumentation nil
  if $maxSignatureLineNumber ~= 0 then
    $docList := [['constructor,:$headerDocumentation],:$docList]
    $maxSignatureLineNumber := 0
    --reset this for next constructor; see recordDocumentation
  lhs:= postTran lhs
  [form,targetType]:=
    lhs is [":",:.] => rest lhs
    [lhs,nil]
  if atom form then form := [form]
  newLhs:=
    [op,:argl]:= [(x is [":",a,.] => a; x) for x in form]
    [op,:postDefArgs argl]
  argTypeList:=
    [(x is [":",.,t] => t; nil) for x in rest form]
  typeList:= [targetType,:argTypeList]
  specialCaseForm := [nil for x in form]
  trhs :=
      rhs is ["=>", a, b] => ['IF,postTran a, postTran b, 'noBranch]
      postTran rhs
  ['DEF, newLhs, typeList, specialCaseForm, trhs]

postDefArgs argl ==
  null argl => argl
  argl is [[":",a],:b] =>
    b => postError
      ['"   Argument",:bright a,'"of indefinite length must be last"]
    atom a or a is ['QUOTE,:.] => a
    postError
      ['"   Argument",:bright a,'"of indefinite length must be a name"]
  [first argl,:postDefArgs rest argl]

postMDef(t) ==
  [.,lhs,rhs] := t
  lhs := postTran lhs
  [form,targetType]:=
    lhs is [":",:.] => rest lhs
    [lhs,nil]
  form:=
    atom form => LIST form
    form
  newLhs:= [(x is [":",a,:.] => a; x) for x in form]
  typeList:= [targetType,:[(x is [":",.,t] => t; nil) for x in rest form]]
  ['MDEF,newLhs,typeList,[nil for x in form],postTran rhs]

postExit ["=>",a,b] == ['IF,postTran a,['exit,postTran b],'noBranch]


postFlatten(x,op) ==
  x is [ =op,a,b] => [:postFlatten(a,op),:postFlatten(b,op)]
  LIST x

postForm (u is [op,:argl]) ==
  x:=
    atom op =>
      argl':= postTranList argl
      [op,:argl']
    u:= postTranList u
    if u is [["@Tuple", :.], :.] then
      postError ['"  ",:bright u,
        '"is illegal because tuples cannot be applied!",'%l,
          '"   Did you misuse infix dot?"]
    u
  x is [., ["@Tuple", :y]] => [first x, :y]
  x

postQuote [.,a] == ['QUOTE,a]

postIf t ==
  t isnt ["if",:l] => t
  ['IF, :[(null(x := postTran x) => 'noBranch; x)
    for x in l]]

postJoin ['Join,a,:l] ==
  a:= postTran a
  l:= postTranList l
  if l is [b] and b is [name,:.] and MEMQ(name,'(ATTRIBUTE SIGNATURE)) then l
    := LIST ['CATEGORY,b]
  al:=
    a is ["@Tuple", :c] => c
    LIST a
  ['Join,:al,:l]

postMapping u  ==
  u isnt ["->",source,target] => u
  ['Mapping,postTran target,:unTuple postTran source]

postOp x ==
  x = ":=" => 'LET
  x='Attribute => 'ATTRIBUTE
  x

postRepeat ['REPEAT,:m,x] == ['REPEAT,:postIteratorList m,postTran x]

postSEGMENT ['SEGMENT,a,b] ==
  key:= [a,'"..",:(b => [b]; nil)]
  postError ['"   Improper placement of segment",:bright key]

postCollect [constructOp,:m,x] ==
  x is [['Sel, D, 'construct], :y] =>
    postCollect [['Sel, D, 'COLLECT], :m, ['construct, :y]]
  itl:= postIteratorList m
  x:= (x is ['construct,r] => r; x)  --added 84/8/31
  y:= postTran x
  finish(constructOp,itl,y) where
    finish(op,itl,y) ==
      y is [":",a] => ['REDUCE,'append,0,[op,:itl,a]]
      y is ["@Tuple", :l] =>
        newBody:=
          or/[x is [":",y] for x in l] => postMakeCons l
          or/[x is ['SEGMENT,:.] for x in l] => tuple2List l
          ['construct,:postTranList l]
        ['REDUCE,'append,0,[op,:itl,newBody]]
      [op,:itl,y]

postIteratorList x ==
  x is [p,:l] =>
    (p:= postTran p) is ['IN,y,u] =>
      u is ["|",a,b] => [['IN,y,postInSeq a],["|",b],:postIteratorList l]
      [['IN,y,postInSeq u],:postIteratorList l]
    p is  ['INBY, y, u, v] =>
      u is ["|",a,b] =>
          [['INBY, y, postInSeq a, v], ["|",b], :postIteratorList l]
      [['INBY, y, u, v], :postIteratorList l]
    [p,:postIteratorList l]
  x

postin arg ==
  arg isnt ["in",i,seq] => systemErrorHere '"postin"
  ["in",postTran i, postInSeq seq]

postIn arg ==
  arg isnt ['IN,i,seq] => systemErrorHere '"postIn"
  ['IN,postTran i,postInSeq seq]

postInSeq seq ==
  seq is ['SEGMENT,p,q] => postTranSegment(p,q)
  seq is ["@Tuple", :l] => tuple2List l
  postTran seq

postTranSegment(p,q) == ['SEGMENT,postTran p,(q => postTran q; nil)]

tuple2List l ==
  l is [a,:l'] =>
    u:= tuple2List l'
    null u => ['construct,postTran a]
    ["cons", postTran a, u]
  nil

postReduce ['Reduce,op,expr] ==
  expr is ['COLLECT, :.] =>
    ['REDUCE,op,0,postTran expr]
  postReduce ['Reduce,op,['COLLECT,['IN,g:= GENSYM(),expr],
    ['construct,  g]]]

postFlattenLeft(x,op) ==--
  x is [ =op,a,b] => [:postFlattenLeft(a,op),b]
  [x]

postSemiColon u ==
    [:l, x] := postFlattenLeft(u, ";")
    ['SEQ, :postBlockItemList l, ["exit", postTran x]]

postSignature1(op, sig) ==
    sig1 := postType sig
    op := postAtom (STRINGP op => INTERN op; op)
    sig is ["->",:.] =>
        ["SIGNATURE",op,:removeSuperfluousMapping killColons sig1]
    ["SIGNATURE", op, killColons sig1, "constant"]

postSignature ['Signature, op, sig, doc] ==
    res1 := postSignature1(op, sig)
    if res1 then record_on_docList(rest res1, doc)
    res1

killColons x ==
  atom x => x
  x is ['Record,:.] => x
  x is ['Union,:.] => x
  x is [":",.,y] => killColons y
  [killColons first x,:killColons rest x]

postSlash ['_/,a,b] ==
  STRINGP a => postTran ['Reduce,INTERN a,b]
  ['_/,postTran a,postTran b]

removeSuperfluousMapping sig1 ==
  --get rid of this asap
  sig1 is [x,:y] and x is ['Mapping,:.] => [rest x,:y]
  sig1

postType typ ==
  typ is ["->",source,target] =>
    source="constant" => [LIST postTran target,"constant"]
    LIST ['Mapping,postTran target,:unTuple postTran source]
  typ is ["->",target] => LIST ['Mapping,postTran target]
  LIST postTran typ

postTuple u ==
  u is ["@Tuple"] => u
  u is ["@Tuple", :l, a] => (["@Tuple", :postTranList rest u])

postWhere ["where",a,b] ==
    ["where", postTran a, postTran b]

postWith ["with",a] ==
  $insidePostCategoryIfTrue: local := true
  a:= postTran a
  a is [op,:.] and MEMQ(op,'(SIGNATURE ATTRIBUTE IF)) => ['CATEGORY,a]
  a is ['PROGN,:b] => ['CATEGORY,:b]
  a

-- should set $topOp
postTransformCheck x ==
  $defOp: local:= nil
  postcheck x

postcheck x ==
  atom x => nil
  x is ['DEF,form,[target,:.],:.] =>
    setDefOp form
    nil
  x is ['QUOTE,:.] => nil
  postcheck first x
  postcheck rest x

setDefOp f ==
  if f is [":",g,:.] then f := g
  f := (atom f => f; first f)
  if $topOp then $defOp:= f else $topOp:= f

unTuple x ==
  x is ["@Tuple", :y] => y
  LIST x
