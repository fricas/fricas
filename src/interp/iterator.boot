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

--% ITERATORS

compReduce(form,m,e) ==
 compReduce1(form,m,e,$formalArgList)

compReduce1(form is ["REDUCE",op,.,collectForm],m,e,$formalArgList) ==
  [collectOp,:itl,body]:= collectForm
  if STRINGP op then op:= INTERN op
  not MEMQ(collectOp,'(COLLECT COLLECTV COLLECTVEC)) =>
        systemError ["illegal reduction form:",form]
  $sideEffectsList: local := nil
  $until: local := nil
  $initList: local := nil
  $endTestList: local := nil
  itl := [([., e] := compIterator(x, e) or return "failed").(0) for x in itl]
  itl="failed" => return nil
  acc:= GENSYM()
  afterFirst:= GENSYM()
  bodyVal:= GENSYM()
  [part1, m, e] := comp([":=", bodyVal, body], m, e) or return nil
  [part2, ., e] := comp([":=", acc, bodyVal], m, e) or return nil
  [part3, ., e] := comp([":=", acc, parseTran [op, acc, bodyVal]], m, e)
                     or return nil
  identityCode:=
    id:= getIdentity(op,e) => u.expr where u() == comp(id,m,e) or return nil
    ["IdentityError",MKQ op]
  finalCode:=
    ["PROGN",
      ["LET",afterFirst,nil],
       ["REPEAT",:itl,
        ["PROGN",part1,
          ["IF", afterFirst,part3,
                   ["PROGN",part2,["LET",afterFirst,MKQ true]]]]],
                    ["IF",afterFirst,acc,identityCode]]
  if $until then
    [untilCode,.,e]:= comp($until,$Boolean,e)
    finalCode:= substitute(["UNTIL",untilCode],'$until,finalCode)
  [finalCode,m,e]

$identity_list := [ _
   ["+", ["Zero"]], _
   ["*", ["One"]], _
   ['gcd, ["Zero"]], _
   ['lcm, ["One"]], _
   ['append, ['construct]], _
   ['union, ['construct]], _
   ['strconc, '""], _
   ['and, 'true], _
   ['or, 'false]]

getIdentity(x,e) ==
    av := ASSQ(x, $identity_list)
    av => av.1
    nil

compRepeatOrCollect(form,m,e) ==
  fn(form,[m,:$exitModeStack],[#$exitModeStack,:$leaveLevelStack],$formalArgList
    ,e) where
      fn(form,$exitModeStack,$leaveLevelStack,$formalArgList,e) ==
        $until: local := nil
        [repeatOrCollect,:itl,body]:= form
        itl':=
          [([x',e]:= compIterator(x,e) or return "failed"; x') for x in itl]
        itl'="failed" => nil
        targetMode:= first $exitModeStack
        bodyMode:=
          repeatOrCollect="COLLECT" =>
            targetMode = '$EmptyMode => '$EmptyMode
            (u:=modeIsAggregateOf('List,targetMode,e)) =>
              CADR u
            (u:=modeIsAggregateOf('PrimitiveArray,targetMode,e)) =>
              repeatOrCollect:='COLLECTV
              CADR u
            (u:=modeIsAggregateOf('Vector,targetMode,e)) =>
              repeatOrCollect:='COLLECTVEC
              CADR u
            stackMessage('"Invalid collect bodytype")
            return nil
            -- If we're doing a collect, and the type isn't conformable
            -- then we've boobed. JHD 26.July.1990
          $NoValueMode
        [body',m',e']:=
          -- (m1:= listOrVectorElementMode targetMode) and comp(body,m1,e) or
            comp(body, bodyMode, e) or return nil
        if $until then
          [untilCode,.,e']:= comp($until,$Boolean,e')
          itl':= substitute(["UNTIL",untilCode],'$until,itl')
        form':= [repeatOrCollect,:itl',body']
        m'':=
          repeatOrCollect="COLLECT" =>
            (u := modeIsAggregateOf('List, targetMode, e)) => first u
            ["List",m']
          repeatOrCollect="COLLECTV" =>
            (u := modeIsAggregateOf('PrimitiveArray, targetMode, e)) => first u
            ["PrimitiveArray",m']
          repeatOrCollect="COLLECTVEC" =>
            (u := modeIsAggregateOf('Vector, targetMode, e)) => first u
            ["Vector",m']
          m'
        coerceExit([form',m'',e'],targetMode)

listOrVectorElementMode x ==
  x is [a,b,:.] and member(a,'(PrimitiveArray Vector List)) => b

genLetHelper(op, arg, d, var) ==
    form0 := [["elt", d, op], arg]
    [":=", var, form0]

compInitGstep(y, ef, sf, mOver, e) ==
    gvar := genSomeVariable()
    [., ., e] := compMakeDeclaration([":", gvar, mOver], $EmptyMode, e)
    form := ["SEQ", [":=", gvar, y],
                    genLetHelper("emptyFun", gvar, mOver, ef),
                       genLetHelper("stepFun", gvar, mOver, sf),
                         ["exit", 1, 1]]
    res := compSeq(form, $Integer, e)
    res => res
    nil

compIterator1(it, e) ==
  it is ["IN",x,y] =>
    --these two lines must be in this order, to get "for f in list f"
    --to give  an error message if f is undefined
    [y',m,e]:= comp(y,$EmptyMode,e) or return nil
    $formalArgList:= [x,:$formalArgList]
    ([mOver, mUnder] := modeIsAggregateOf("Generator", m, e)) =>
        if null get(x,"mode",e) then
            [.,.,e] := compMakeDeclaration([":",x,mUnder],$EmptyMode,e)
                           or return nil
        e:= put(x, "value", [genSomeVariable(), mUnder, e], e)
        ef := genSomeVariable()
        sf := genSomeVariable()
        [y'', ., .] := compInitGstep(y, ef, sf, mOver, e) or return nil
        res := ["GSTEP", x, ef, sf, y'']
        SAY([res, mUnder])
        [res, e]
    [mOver, mUnder] :=
      modeIsAggregateOf("List",m,e) or return
         stackMessage ["mode: ",m," must be a list of some mode"]
    if null get(x,"mode",e) then [.,.,e]:=
      compMakeDeclaration([":",x,mUnder],$EmptyMode,e) or return nil
    e:= put(x,"value",[genSomeVariable(),mUnder,e],e)
    [y'',m'',e] := coerce([y',m,e], mOver) or return nil
    [["IN",x,y''],e]
  it is ["ON",x,y] =>
    $formalArgList:= [x,:$formalArgList]
    [y',m,e]:= comp(y,$EmptyMode,e) or return nil
    [mOver,mUnder]:=
      modeIsAggregateOf("List",m,e) or return
        stackMessage ["mode: ",m," must be a list of other modes"]
    if null get(x,"mode",e) then [.,.,e]:=
      compMakeDeclaration([":",x,m],$EmptyMode,e) or return nil
    e:= put(x,"value",[genSomeVariable(),m,e],e)
    [y'',m'',e] := coerce([y',m,e], mOver) or return nil
    [["ON",x,y''],e]
  it is ["STEP",index,start,inc,:optFinal] =>
    $formalArgList:= [index,:$formalArgList]
    --if all start/inc/end compile as small integers, then loop
    --is compiled as a small integer loop
    final':= nil
    (start' := comp(start, $SingleInteger, e)) and
      (inc':= comp(inc,$NonNegativeInteger,start'.env)) and
        (not (optFinal is [final]) or
          (final' := comp(final, $SingleInteger, inc'.env))) =>
            indexmode:=
              comp(start,$NonNegativeInteger,e) =>
                      $NonNegativeInteger
              $SingleInteger
            if null get(index,"mode",e) then [.,.,e]:=
              compMakeDeclaration([":",index,indexmode],$EmptyMode,
                (final' => final'.env; inc'.env)) or return nil
            e:= put(index,"value",[genSomeVariable(),indexmode,e],e)
            if final' then optFinal:= [final'.expr]
            [["ISTEP",index,start'.expr,inc'.expr,:optFinal],e]
    [start,.,e]:=
      comp(start,$Integer,e) or return
        stackMessage ["start value of index: ",start," must be an integer"]
    [inc,.,e]:=
      comp(inc,$Integer,e) or return
        stackMessage ["index increment:",inc," must be an integer"]
    if optFinal is [final] then
      [final,.,e]:=
        comp(final,$Integer,e) or return
          stackMessage ["final value of index: ",final," must be an integer"]
      optFinal:= [final]
    indexmode:=
      comp(CADDR it,$NonNegativeInteger,e) => $NonNegativeInteger
      $Integer
    if null get(index,"mode",e) then [.,.,e]:=
      compMakeDeclaration([":",index,indexmode],$EmptyMode,e) or return nil
    e:= put(index,"value",[genSomeVariable(),indexmode,e],e)
    [["STEP",index,start,inc,:optFinal],e]
  it is ["WHILE",p] =>
    [p',m,e]:=
      comp(p,$Boolean,e) or return
        stackMessage ["WHILE operand: ",p," is not Boolean valued"]
    [["WHILE",p'],e]
  it is ["UNTIL",p] => ($until:= p; ['$until,e])
  it is ["|",x] =>
    u:=
      comp(x,$Boolean,e) or return
        stackMessage ["SUCHTHAT operand: ",x," is not Boolean value"]
    [["|",u.expr],u.env]
  nil

match_segment(i, n) ==
    n is ['SEGMENT,a] => ['STEP,i,a,1]
    n is ['SEGMENT, a, b] => (b => ['STEP, i, a, 1, b]; ['STEP, i, a, 1])
    ['IN, i, n]

compIterator(it, e) ==
    it is ["INBY", i, n, inc] =>
        u := match_segment(i, n)
        u isnt ['STEP, i, a, 1, :r] =>
            stackAndThrow ["   You cannot use", "%b", "by", "%d",
                      "except for an explicitly indexed sequence."]
        compIterator1(['STEP, i, a, inc, :r], e)
    it is ["IN", i, n] =>
        compIterator1(match_segment(i, n), e)
    compIterator1(it, e)

modeIsAggregateOf(ListOrVector,m,e) ==
  m is [ =ListOrVector,R] => [m,R]
--m = '$EmptyMode => [m,m] I don't think this is correct, breaks POLY +
  m is ["Union",:l] =>
    mList:= [pair for m' in l | (pair:= modeIsAggregateOf(ListOrVector,m',e))]
    1=#mList => first mList
  name:=
    m is [fn,:.] => fn
    m="$" => "Rep"
    m
  get(name,"value",e) is [[ =ListOrVector,R],:.] => [m,R]

