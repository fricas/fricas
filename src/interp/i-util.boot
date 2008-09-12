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


--% Utility Functions Used Only by the Intepreter

-- A wrapped value represents something that need not be evaluated
-- when code is generated.  This includes objects from domains or things
-- that just happed to evaluate to themselves.  Typically generated
-- lisp code is unwrapped.

wrap x ==
  isWrapped x => x
  ['WRAPPED,:x]

isWrapped x == x is ['WRAPPED,:.] or NUMBERP x or FLOATP x or STRINGP x

unwrap x ==
  NUMBERP x or FLOATP x or STRINGP x => x
  x is ["WRAPPED",:y] => y
  x

wrapped2Quote x ==
  x is ["WRAPPED",:y] => MKQ y
  x

quote2Wrapped x ==
  x is ['QUOTE,y] => wrap y
  x

removeQuote x ==
  x is ["QUOTE",y] => y
  x

--% The function for making prompts

spadPrompt() ==
  SAY '"   FriCAS"
  sayNewLine()

MKPROMPT() ==
  $inputPromptType = 'none    => '""
  $inputPromptType = 'plain   => '"-> "
  $inputPromptType = 'step    =>
    STRCONC('"(",STRINGIMAGE $IOindex,'") -> ")
  $inputPromptType = 'frame   =>
    STRCONC(STRINGIMAGE $interpreterFrameName,
      '" (",STRINGIMAGE $IOindex,'") -> ")
  STRCONC(STRINGIMAGE $interpreterFrameName,
   '" [", SUBSTRING(CURRENTTIME(),8,NIL),'"] [",
    STRINGIMAGE $IOindex, '"] -> ")

--% Miscellaneous

Zeros n ==
  BOUNDP '$ZeroVecCache and #$ZeroVecCache=n => $ZeroVecCache
  $ZeroVecCache:= MAKE_-VEC n
  for i in 0..n-1 repeat $ZeroVecCache.i:=0
  $ZeroVecCache

LZeros n ==
  n < 1 => nil
  l := [0]
  for i in 2..n repeat l := [0, :l]
  l

-- subrToName x == BPINAME x

-- formerly in clammed.boot

isSubDomain(d1,d2) ==
  -- d1 and d2 are different domains
  subDomainList := '(Integer NonNegativeInteger PositiveInteger)
  ATOM d1 or ATOM d2 => nil
  l := MEMQ(CAR d2, subDomainList) =>
    MEMQ(CAR d1, CDR l)
  nil

$variableNumberAlist := nil

variableNumber(x) ==
  p := ASSQ(x, $variableNumberAlist)
  null p => 
    $variableNumberAlist := [[x,:0], :$variableNumberAlist]
    0
  RPLACD(p, 1+CDR p)
  CDR p

-- functions used at run-time which were formerly in the compiler files

Undef(:u) ==
  u':= last u
  [[domain,slot],op,sig]:= u'
  domain':=eval mkEvalable domain
  not EQ(CAR ELT(domain',slot),Undef) =>
-- OK - thefunction is now defined
    [:u'',.]:=u
    if $reportBottomUpFlag then
      sayMessage concat ['"   Retrospective determination of slot",'%b,
        slot,'%d,'"of",'%b,:prefix2String domain,'%d]
    APPLY(CAR ELT(domain',slot),[:u'',CDR ELT(domain',slot)])
  throwKeyedMsg("S2IF0008",[formatOpSignature(op,sig),domain])

devaluateList l == [devaluate d for d in l]

HasSignature(domain,[op,sig]) ==
  compiledLookup(op,sig,domain)

addModemap(op,mc,sig,pred,fn,$e) ==
  $InteractiveMode => $e
  if knownInfo pred then pred:=true
  $insideCapsuleFunctionIfTrue=true =>
    $CapsuleModemapFrame :=
      addModemap0(op,mc,sig,pred,fn,$CapsuleModemapFrame)
    $e
  addModemap0(op,mc,sig,pred,fn,$e)

lispize x == first optimize [x]

mkPredList listOfEntries ==
     [['EQCAR,"#1",i] for arg in listOfEntries for i in 0..]

TruthP x ==
    --True if x is a predicate that's always true
  x is nil => nil
  x=true => true
  x is ['QUOTE,:.] => true
  nil

