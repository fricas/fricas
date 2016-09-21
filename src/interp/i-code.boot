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

--% Interpreter Code Generation Routines

--Modified by JHD 9/9/93 to fix a problem with coerces inside
--interpreter functions being used as mappings. They were being
--handled with $useCoerceOrCroak being NIL, and therefore internal
--coercions were not correctly handled. Fix: remove dependence
--on $useCoerceOrCroak, and test explicitly for Mapping types.

--% COERCE

intCodeGenCOERCE(triple,t2) ==
  -- NOTE: returns a triple
  t1 := objMode triple
  t1 = $EmptyMode => NIL
  t1 = t2 => triple
  val := objVal triple

)if false
  -- if request is for a coerce to t2 from a coerce from
  -- to to t1, and t1 = Void or canCoerce(t0,t2), then optimize

  (val is ['coerceOrCroak,trip,t1', .]) and
    (t0 := objCodeMode trip) and ([.,val0] := objCodeVal trip) and
      ( (t1 = $Void) or canCoerceFrom(removeQuote t0,t2) ) =>
         -- just generate code for coercion, don't coerce constants
         -- might be too big
         intCodeGenCOERCE(objNew(val0, removeQuote t0), t2)
)endif

  val is ['THROW,label,code] =>
    if label is ['QUOTE, l] then label := l
    null($compilingMap) or (label ~= mapCatchName($mapName)) =>
      objNew(['THROW,label,wrapped2Quote objVal
        intCodeGenCOERCE(objNew(code,t1),t2)],t2)
    -- we have a return statement. just send it back as is
    objNew(val,t2)

  val is ['PROGN,:code,lastCode] =>
    objNew(['PROGN,:code,wrapped2Quote objVal
      intCodeGenCOERCE(objNew(lastCode,t1),t2)],t2)

  val is ['COND,:conds] =>
    objNew(['COND,
      :[[p,wrapped2Quote objVal intCodeGenCOERCE(objNew(v,t1),t2)]
        for [p,v] in conds]],t2)

  -- specially handle subdomain
  absolutelyCanCoerceByCheating(t1,t2) => objNew(val,t2)

  -- specially handle coerce to Any
  t2 = '(Any) => objNew(['CONS,MKQ t1,val],t2)

  -- optimize coerces from Any
  (t1 = '(Any)) and (val is [ ='CONS,t1',val']) =>
    intCodeGenCOERCE(objNew(val',removeQuote t1'),t2)

  -- specially handle coerce from Equation to Boolean
  (t1 is ['Equation,:.]) and (t2 = $Boolean) =>
    coerceByFunction(triple,t2)

  -- next is hack for if-then-elses
  (t1 = '$NoValueMode) and (val is ['COND,pred]) =>
    code :=
      ['COND,pred,
        [MKQ true,['throwKeyedMsg,MKQ "S2IM0016",MKQ $mapName]]]
    objNew(code,t2)

  -- optimize coerces to OutputForm
  t2 = $OutputForm =>
    coerceByFunction(triple,t2)

  isSubDomain(t1, $Integer) =>
    intCodeGenCOERCE(objNew(val, $Integer), t2)

  -- generate code
  -- 1. See if the coercion will go through (absolutely)
  --    Must be careful about variables or else things like
  --    P I --> P[x] P I might not have the x in the original polynomial
  --    put in the correct place

  (not containsVariables(t2)) and canCoerceByFunction(t1,t2) =>
    -- try coerceByFunction
    (not canCoerceByMap(t1,t2)) and
      (code := coerceByFunction(triple,t2)) => code
    intCodeGenCoerce1(val,t1,t2)

  -- 2. Set up a failure point otherwise

  intCodeGenCoerce1(val,t1,t2)

intCodeGenCoerce1(val,t1,t2) ==
  -- Internal function to previous one
  -- designed to ensure that we don't use coerceOrCroak on mappings
--(t2 is ['Mapping,:.]) => THROW('coerceOrCroaker, 'croaked)
  objNew(['coerceOrCroak,mkObjCode(['wrap,val],t1),
        MKQ t2, MKQ $mapName],t2)

--% Map components

wrapMapBodyWithCatch body ==
  -- places a CATCH around the map body
  -- note that we will someday have to fix up the catch identifier
  -- to use the generated internal map name
  $mapThrowCount = 0 => body
  ['CATCH, MKQ mapCatchName $mapName, body]
