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

--% Translation of Expression to FORTRAN

do_with_error_env0(f) ==
    $fortError : fluid := nil
    checkLines SPADCALL(f)

do_with_error_env2(int_to_floats?, f) ==
    $fortInts2Floats : fluid := int_to_floats?
    $fortError : fluid := nil
    checkLines SPADCALL(f)

do_with_error_env3(f) ==
    $fortError : fluid := nil
    $fortranSegment : fluid := nil
    $fortInts2Floats : fluid := nil
    SPADCALL(f)

do_with_error_env4(nf, ints2floats?, f) ==
  $fortInts2Floats : fluid := ints2floats?
  $exp2FortTempVarIndex : local := 0
  $fortName : fluid := SPADCALL(nf)
  SPADCALL(f)

newFortranTempVar() ==
    ff := getFunctionFromDomain("newFortranTempVar", ['FortranCodeTools],
                                [])
    SPADCALL(ff)

--% Optimization of Expression

exp2FortOptimize e ==
  -- $fortranOptimizationLevel means:
  --   0         just extract arrays
  --   1         extract common subexpressions
  --   2         try to optimize computing of powers
  $exprStack : local := NIL
  atom e => [e]
  $fortranOptimizationLevel = 0 =>
    e1 := exp2FortOptimizeArray e
    NREVERSE [e1,:$exprStack]
  e := minimalise e
  for e1 in exp2FortOptimizeCS  e repeat
    e2 := exp2FortOptimizeArray e1
    $exprStack := [e2,:$exprStack]
  NREVERSE $exprStack


exp2FortOptimizeCS e ==
  $fortCsList : local := NIL
  $fortCsHash : local := MAKE_-HASHTABLE 'EQ
  f := exp2FortOptimizeCS1(e, nil)
  NREVERSE [f,:$fortCsList]

-- bug fix to beenHere
-- Thu Nov 05 12:01:46 CUT 1992 , Author: TTT
-- Used in exp2FortOprtimizeCS
-- Original file : newfort.boot
beenHere(e,n) ==
  n.0 := n.0 + 1                      -- increase count (initially 1)
  n.0 = 2 =>                          -- first time back again
    var := n.1 := newFortranTempVar() -- stuff n.1 with new var
    loc := n.2                    -- get expression
    if loc then
-- using COPY-TREE : RPLAC does not smash $fortCsList
-- which led to inconsistencies in assignment of temp. vars.
      $fortCsList := COPY_-TREE [["=",var,e], :$fortCsList]
      RPLACA(loc, var)
    var
  n.1                     -- been here before, so just get variable

exp2FortOptimizeCS1(e, e0) ==
  -- we do nothing with atoms or simple lists containing atoms
  atom(e) or (atom first e and null rest e) => e
  e is [op,arg] and object2Identifier op = "-" and atom arg => e

  -- see if we have been here before
  not (object2Identifier QCAR e in '(ROW AGGLST)) and
    (n := HGET($fortCsHash,e)) => beenHere(e,n) -- where

  -- descend sucessive CARs of CDRs of e
  f := e
  while f repeat
    RPLACA(f, exp2FortOptimizeCS1(QCAR f, f))
    g := QCDR f
    -- check to see of we have an non-NIL atomic CDR
    g and atom g => f := NIL
    f := g

  MEMQ(object2Identifier QCAR e,'(ROW AGGLST)) => e

  -- see if we have already seen this expression
  n := HGET($fortCsHash,e)
  null n =>
    n := VECTOR(1, NIL, e0)
    HPUT($fortCsHash, e, n)
    e
  beenHere(e, n)



exp2FortOptimizeArray e ==
  -- this handles arrays
  atom e => e
  [op,:args] := e
  op1 := object2Identifier op
  op1 in '(BRACE BRACKET) =>
    args is [['AGGLST,:elts]] =>
      LISTP first elts and first first elts in '(BRACE BRACKET) => fortError1 e
      -- var := newFortranTempVar()
      var := $fortName
      $exprStack := [[op,var,['AGGLST,:exp2FortOptimizeArray elts]],
        :$exprStack]
      var
  EQ(op1,'MATRIX) =>
    -- var := newFortranTempVar()
    var := $fortName
    -- args looks like [NIL,[ROW,...],[ROW,...]]
    $exprStack := [[op,var,:exp2FortOptimizeArray args],:$exprStack]
    var
  [exp2FortOptimizeArray op,:exp2FortOptimizeArray args]


--% FORTRAN Line Breaking

-- The Fortran error functions
fortError1 u ==
  $fortError := "t"
  sayErrorly("Fortran translation error",
             "   No corresponding Fortran structure for:")
  mathPrint u

fortError(u,v) ==
  $fortError := "t"
  msg := STRCONC("   ",STRINGIMAGE u);
  sayErrorly("Fortran translation error",msg)
  mathPrint v

--% Top Level Things to Call
-- The names are the same as those used in the old fortran code

fortexp0 x ==
  e_to_f := getFunctionFromDomain("expression2Fortran", ['FortranCodeTools],
                                 [$OutputForm])
  f := SPADCALL(x, e_to_f)
  p := position('"%l",f)
  p < 0 => f
  l := NIL
  while p < 0 repeat
    [t,:f] := f
    l := [t,:l]
  NREVERSE ['"...",:l]

displayLines1 lines ==
  for l in lines repeat
    PRINTEXP(l,$fortranOutputStream)
    TERPRI($fortranOutputStream)

displayLines lines ==
  if not $fortError then displayLines1 lines

checkLines lines ==
  $fortError => []
  lines


--------------------------format.boot------------------------------------------


changeExprLength(i) ==
  $maximumFortranExpressionLength := $maximumFortranExpressionLength + i

