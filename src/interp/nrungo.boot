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

--=======================================================
--             Lookup From Interpreter
--=======================================================

NRTevalDomain form ==
  form is ['SETELT,:.] =>
      BREAK()
      eval form
  evalDomain form

compiledLookupCheck(op,sig,dollar) ==
  fn := compiledLookup(op,sig,dollar)

  fn = nil =>
    keyedSystemError("S2NR0001",[op,formatSignature sig,dollar.0])
  fn

--=======================================================
--                 Lookup From Compiled Code
--=======================================================

NRTreplaceLocalTypes(t,dom) ==
   atom t =>
     not INTEGERP t => t
     t:= dom.t
     if PAIRP t then t:= NRTevalDomain t
     t.0
   MEMQ(first t, '(Mapping Union Record _:)) =>
      [first t, :[NRTreplaceLocalTypes(x, dom) for x in rest t]]
   t

substDomainArgs(domain,object) ==
    form := devaluate domain
    SUBLISLIS([form,:rest form],["$$",:$FormalMapVariableList],object)

--=======================================================
--       Lookup Function in Slot 1 (via SPADCALL)
--=======================================================
lookupInTable(op,sig,dollar,[domain,table]) ==
  success := false
  someMatch := false
  while not success for [sig1, :code] in QLASSQ(op, table) repeat
    success :=
      null compareSig(sig,sig1,dollar.0,domain) => false
      loc := code
      loc = 0 => BREAK()
      slot := domain.loc
      lookupDisplay(op,sig,domain,'" !! found in NEW table!!")
      slot
  success

--=======================================================
--                       Predicates
--=======================================================

compareSig(sig,tableSig,dollar,domain) ==
  not (#sig = #tableSig) => false
  null (target := first sig)
   or lazyCompareSigEqual(target,first tableSig,dollar,domain) =>
     and/[lazyCompareSigEqual(s,t,dollar,domain)
              for s in rest sig for t in rest tableSig]

lazyCompareSigEqual(s,tslot,dollar,domain) ==
  tslot = '$ => s = tslot
  INTEGERP tslot and PAIRP(lazyt:=domain.tslot) and PAIRP s =>
      lazyt is [.,.,.,[.,item,.]] and
        item is [., [functorName, :.]] and functorName = first s =>
          compareSigEqual(s,(NRTevalDomain lazyt).0,dollar,domain)
      nil
  compareSigEqual(s,NRTreplaceLocalTypes(tslot,domain),dollar,domain)


compareSigEqual(s,t,dollar,domain) ==
  EQUAL(s,t) => true
  ATOM t =>
    u :=
      EQ(t,'$) => dollar
      isSharpVar t =>
        VECP domain => ELT(rest domain.0,POSN1(t,$FormalMapVariableList))
        ELT(rest domain,POSN1(t,$FormalMapVariableList))
      STRINGP t and IDENTP s => (s := PNAME s; t)
      nil
    s = '$ => compareSigEqual(dollar,u,dollar,domain)
    u => compareSigEqual(s,u,dollar,domain)
    EQUAL(s,u)
  EQ(s,'$) => compareSigEqual(dollar,t,dollar,domain)
  ATOM s => nil
  #s ~= #t => nil
  match := true
  for u in s for v in t repeat
    not compareSigEqual(u,v,dollar,domain) => return(match:=false)
  match

-----------------------Compiler for Interpreter---------------------------------

NRT_opt_call(u, opName, sigTail,dcVector) ==
    dc := devaluate(dcVector)
    -- sayBrightly(["NRT_opt_call ", u, opName, sigTail, dc])
    not MEMQ(IFCAR dc, $optimizableConstructorNames) => nil
    NULL(name := BPINAME(first u))  => nil
    fn := GETL(name, 'SPADreplace) =>
        n := #dcVector
        flag := true
        k := -1
        for i in 0..(n - 1) while flag repeat
            if dcVector.i = u then
                k := i
                flag := false
        k >= 0 => ["ELT", dc, k]
        nil
    nil

NRTcompileEvalForm(opName,sigTail,dcVector) ==
  u := NRTcompiledLookup(opName,sigTail,dcVector)
  not ($insideCompileBodyIfTrue = true) => MKQ u
  res1 := NRT_opt_call(u, opName, sigTail, dcVector) => res1
  k := NRTgetMinivectorIndex(u,opName,sigTail,dcVector)
  ['ELT,"$$$",k]  --$$$ denotes minivector

NRTtypeHack t ==
  ATOM t => t
  first t = '_# => # CADR t
  [first t, :[NRTtypeHack tt for tt in rest t]]

NRTgetMinivectorIndex(u,op,sig,domVector) ==
  s := # $minivector
  k := or/[k for k in 0..(s-1)
        for x in $minivector | EQ(x,u)] => k
  $minivector := [:$minivector,u]
  if $compilingInputFile then
    $minivectorCode := [:$minivectorCode,[op,sig,devaluate domVector]]
--  pp '"-- minivectorCode -->"
--  pp $minivectorCode
  s

is_op_slot(slot, dom, k, minivector_name, int_vec, bool_vec) ==
    dom = minivector_name => EQ(slot, $minivector.k)
    dom = ["Integer"] => EQ(slot, int_vec.k)
    dom = ["Boolean"] => EQ(slot, bool_vec.k)
    nil

NRTisRecurrenceRelation(op,body,minivectorName) ==
  -- returns [body p1 p2 ... pk] for a k-term recurrence relation
  -- where the n-th term is computed using the (n-1)st,...,(n-k)th
  -- whose values are initially computed using the expressions
  -- p1,...,pk respectively; body has #2,#3,... in place of
  -- f(k-1),f(k-2),...

  body isnt ['COND,:pcl] => false
  -- body should have a conditional expression which
  -- gives k boundary values, one general term plus possibly an
  -- "out of domain" condition
--pcl is [:.,[ ''T,:mess]] and not (CONTAINED('throwMessage,mess) or
--  CONTAINED('throwKeyedMsg,mess)) => NIL
  pcl := [x for x in pcl | not (x is [''T,:mess] and
    (CONTAINED('throwMessage,mess) or
      CONTAINED('throwKeyedMsg,mess)))]
  integer := EVAL $Integer
  iequalSlot := compiledLookupCheck("=", '((Boolean) $ $), integer)
  lt_slot := compiledLookupCheck("<", '((Boolean) $ $), integer)
  le_slot := compiledLookupCheck("<=", '((Boolean) $ $), integer)
  gt_slot := compiledLookupCheck(">", '((Boolean) $ $), integer)
  ge_slot := compiledLookupCheck(">=", '((Boolean) $ $), integer)
  bf := '(Boolean)
  bf_vec := EVAL bf
  notpSlot := compiledLookupCheck("not", '((Boolean)(Boolean)), bf_vec)
  for [p,c] in pcl repeat
    p is ['SPADCALL, sharpVar, n1, ['ELT, dom, slot]] and
      is_op_slot(iequalSlot, dom, slot, minivectorName, integer, bf_vec) =>
        initList:= [[n1,:c],:initList]
        sharpList := insert(sharpVar,sharpList)
        n:=n1
    miscList:= [[p,c],:miscList]
  miscList isnt [[generalPred,generalTerm]] or sharpList isnt [sharpArg] =>
      return false
    --first general term starts at n

  --Must have at least one special value; insist that they be consecutive
  null initList => false
  specialValues:= MSORT ASSOCLEFT initList
  or/[null INTEGERP n for n in specialValues] => false
  minIndex:= "MIN"/specialValues
  not (and/[i=x for i in minIndex..(minIndex+n-1) for x in specialValues]) =>
    sayKeyedMsg("S2IX0005",
      ["append"/[['" ",sv]  for sv in specialValues]])
    return nil

  --Determine the order k of the recurrence and index n of first general term
  k:= #specialValues
  n:= k+minIndex
  --Check general predicate
  predOk :=
    generalPred is '(QUOTE T) => true
    generalPred is ['SPADCALL, m1, m2, ['ELT, dom, slot]] =>
        m2 = sharpArg and is_op_slot(lt_slot, dom, slot,
                            minivectorName, integer, bf_vec) => m1 + 1
        m2 = sharpArg and is_op_slot(le_slot, dom, slot,
                            minivectorName, integer, bf_vec) => m1
        m1 = sharpArg and is_op_slot(gt_slot, dom, slot,
                            minivectorName, integer, bf_vec) => m2 + 1
        m1 = sharpArg and is_op_slot(ge_slot, dom, slot,
                            minivectorName, integer, bf_vec) => m2
    generalPred is ['SPADCALL, ['SPADCALL, =sharpArg, m,
      ['ELT, dom1, slot1]], ['ELT, dom2, slot2]] and
        is_op_slot(notSlot, dom2, slot2, minivectorName, integer, bf_vec)
          and is_op_slot(lt_slot, dom1, slot1,
                         minivectorName, integer, bf_vec) => m
    generalPred is ['NOT, ['SPADCALL, =sharpArg, m,
       ['ELT, dom, slot]]] and
          is_op_slot(lt_slot, dom, slot, minivectorName, integer, bf_vec) => m
    return nil
  INTEGERP predOk and predOk ~= n =>
    sayKeyedMsg("S2IX0006",[n,m])
    return nil

  --Check general term for references to just the k previous values
  diffCell := compiledLookupCheck("-", '($ $ $), integer)
  --Check general term for references to just the k previous values
  sharpPosition := PARSE_-INTEGER SUBSTRING(sharpArg,1,nil)
  al:= mkDiffAssoc(op, generalTerm, k, sharpPosition, sharpArg,
                   diffCell, minivectorName, integer, bf_vec)
  null al => false
  '$failed in al => false
  body:= generalTerm
  for [a,:b] in al repeat
    body:= substitute(b,a,body)
  result:= [body,sharpArg,n-1,:NREVERSE [LASSOC(i,initList) or
      systemErrorHere('"NRTisRecurrenceRelation")
        for i in minIndex..(n-1)]]

mkDiffAssoc(op, body, k, sharpPosition, sharpArg, diffCell,
            vecname, int_vec, bool_vec) ==
  -- returns alist which should not have any entries = $failed
  -- form substitution list of the form:
  -- ( ((f (,DIFFERENCE #1 1)) . #2) ((f (,DIFFERENCE #1 2)) . #3) ...)
  --   but also checking that all difference values lie in 1..k
  atom body => nil
  body is ['COND,:pl] =>
    "union"/[mkDiffAssoc(op, c, k, sharpPosition, sharpArg, diffCell,
                         vecname, int_vec, bool_vec) for [p, c] in pl]
  body is [fn,:argl] =>
    (fn = op) and argl.(sharpPosition-1) is
      ['SPADCALL, =sharpArg, n, ['ELT, dom, slot]] and
        is_op_slot(diffCell, dom, slot, vecname, int_vec, bool_vec) =>
          NUMBERP n and n > 0 and n <= k =>
              [[body, :$TriangleVariableList.n]]
          ['$failed]
    "union"/[mkDiffAssoc(op, x, k, sharpPosition, sharpArg, diffCell,
                         vecname, int_vec, bool_vec) for x in argl]
  systemErrorHere '"mkDiffAssoc"
