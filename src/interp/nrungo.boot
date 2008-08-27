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


--=======================================================
--             Lookup From Interpreter
--=======================================================

NRTevalDomain form ==
  form is ['SETELT,:.] => eval form
  evalDomain form

compiledLookupCheck(op,sig,dollar) ==
  fn := compiledLookup(op,sig,dollar)

  -- NEW COMPILER COMPATIBILITY ON

  if      (fn = nil)  and (op = "^") then
    fn := compiledLookup("**",sig,dollar)
  else if (fn = nil)  and (op = "**") then
    fn := compiledLookup("^",sig,dollar)

  -- NEW COMPILER COMPATIBILITY OFF

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
   MEMQ(CAR t,'(Mapping Union Record _:)) =>
      [CAR t,:[NRTreplaceLocalTypes(x,dom) for x in rest t]]
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
  while not success for [sig1,:code] in LASSQ(op,table) repeat
    success :=
      null compareSig(sig,sig1,dollar.0,domain) => false
      code is ['subsumed,a] =>
            subsumptionSig :=
               EQSUBSTLIST(rest(domain.0),$FormalMapVariableList,a)
            someMatch:=true
            false
      predIndex := QSQUOTIENT(code,8192)
      predIndex ^= 0 => BREAK()
      loc := QSQUOTIENT(QSREMAINDER(code,8192),2)
      loc = 0 => BREAK()
      slot := domain.loc
      EQCAR(slot,'goGet) =>
        lookupDisplay(op,sig,domain,'" !! goGet found, will ignore")
        BREAK()
      NULL slot =>
        lookupDisplay(op,sig,domain,'" !! null slot entry, continuing")
        BREAK()
      lookupDisplay(op,sig,domain,'" !! found in NEW table!!")
      slot
  NE(success,'failed) and success => success
  subsumptionSig and (u:= SPADCALL(op,subsumptionSig,dollar,domain.1)) => u
  someMatch => BREAK()
  nil

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
        item is [.,[functorName,:.]] and functorName = CAR s =>
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
  #s ^= #t => nil
  match := true
  for u in s for v in t repeat
    not compareSigEqual(u,v,dollar,domain) => return(match:=false)
  match

-----------------------Compiler for Interpreter---------------------------------
NRTcompileEvalForm(opName,sigTail,dcVector) ==
  u := NRTcompiledLookup(opName,sigTail,dcVector)
  not ($insideCompileBodyIfTrue = true) => MKQ u
  k := NRTgetMinivectorIndex(u,opName,sigTail,dcVector)
  ['ELT,"$$$",k]  --$$$ denotes minivector

NRTtypeHack t ==
  ATOM t => t
  CAR t = '_# => # CADR t
  [CAR t,:[NRTtypeHack tt for tt in CDR t]]

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
  integer := EVALFUN $Integer
  iequalSlot:=compiledLookupCheck("=",'((Boolean) $ $),integer)
  lesspSlot:=compiledLookupCheck("<",'((Boolean) $ $),integer)
  bf := '(Boolean)
  notpSlot:= compiledLookupCheck("not",'((Boolean)(Boolean)),EVALFUN bf)
  for [p,c] in pcl repeat
    p is ['SPADCALL,sharpVar,n1,['ELT,=minivectorName,slot]]
      and EQ(iequalSlot,$minivector.slot) =>
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
    generalPred is ['SPADCALL,m,=sharpArg,['ELT,=minivectorName,slot]]
      and EQ(lesspSlot,$minivector.slot)=> m+1
    generalPred is ['SPADCALL,['SPADCALL,=sharpArg,m,
      ['ELT,=minivectorName,slot]], ['ELT,=minivectorName,notSlot]]
        and EQ(lesspSlot,$minivector.slot)
          and EQ(notpSlot,$minivector.notSlot) => m
    generalPred is ['NOT,['SPADCALL,=sharpArg,m,['ELT,=minivectorName, =lesspSlot]]]
      and EQ(lesspSlot,$minivector.slot) => m
    return nil
  INTEGERP predOk and predOk ^= n =>
    sayKeyedMsg("S2IX0006",[n,m])
    return nil

  --Check general term for references to just the k previous values
  diffCell:=compiledLookupCheck("-",'($ $ $),integer)
  diffSlot := or/[i for i in 0.. for x in $minivector | EQ(x,diffCell)]
                or return nil
  --Check general term for references to just the k previous values
  sharpPosition := PARSE_-INTEGER SUBSTRING(sharpArg,1,nil)
  al:= mkDiffAssoc(op,generalTerm,k,sharpPosition,sharpArg,diffSlot,minivectorName)
  null al => false
  '$failed in al => false
  body:= generalTerm
  for [a,:b] in al repeat
    body:= substitute(b,a,body)
  result:= [body,sharpArg,n-1,:NREVERSE [LASSOC(i,initList) or
      systemErrorHere('"NRTisRecurrenceRelation")
        for i in minIndex..(n-1)]]

mkDiffAssoc(op,body,k,sharpPosition,sharpArg,diffSlot,vecname) ==
  -- returns alist which should not have any entries = $failed
  -- form substitution list of the form:
  -- ( ((f (,DIFFERENCE #1 1)) . #2) ((f (,DIFFERENCE #1 2)) . #3) ...)
  --   but also checking that all difference values lie in 1..k
  atom body => nil
  body is ['COND,:pl] =>
    "union"/[mkDiffAssoc(op,c,k,sharpPosition,sharpArg,diffSlot,vecname) for [p,c] in pl]
  body is [fn,:argl] =>
    (fn = op) and argl.(sharpPosition-1) is
      ['SPADCALL,=sharpArg,n,['ELT,=vecname,=diffSlot]] =>
          NUMP n and n > 0 and n <= k =>
            [[body,:$TriangleVariableList.n]]
          ['$failed]
    "union"/[mkDiffAssoc(op,x,k,sharpPosition,sharpArg,diffSlot,vecname) for x in argl]
  systemErrorHere '"mkDiffAssoc"
