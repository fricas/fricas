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

--% Utilities

DEFPARAMETER($fluidVars, nil)
DEFPARAMETER($locVars, nil)
DEFPARAMETER($PrettyPrint, false)
DEFPARAMETER($COMPILE, true)

-- $LET := 'SPADLET    -- LET is a standard macro in Common Lisp

flattenCOND body ==
  -- transforms nested COND clauses to flat ones, if possible
  body isnt ['COND,:.] => body
  ['COND,:extractCONDClauses body]

extractCONDClauses clauses ==
  -- extracts nested COND clauses into a flat structure
  clauses is ['COND, [pred1,:act1],:restClauses] =>
    if act1 is [['PROGN,:acts]] then act1 := acts
    restClauses is [[''T,restCond]] =>
      [[pred1,:act1],:extractCONDClauses restCond]
    [[pred1,:act1],:restClauses]
  [[''T,clauses]]

-- from comp.lisp

COMP_-1(x) ==
  [fname, lamex, :.] := x
  $FUNNAME : local := fname
  $FUNNAME__TAIL : local := [fname]
  $CLOSEDFNS : local := nil
  lamex := compTran lamex
  compNewnam lamex
  if FBOUNDP(fname) then
      FORMAT(true, '"~&~%;;;     ***       ~S REDEFINED~%", fname)
  [[fname, lamex], :$CLOSEDFNS]

COMP_-2(args) ==
    [name, [type, argl, :bodyl], :junk] := args
    junk => MOAN (FORMAT(nil, '"******pren error in (~S (~S ...) ...)",_
                         name, type))
    type is "SLAM" => BREAK()
    LASSQ(name, $clamList) => compClam(name, argl, bodyl, $clamList)
    type is "SPADSLAM" => compSPADSLAM(name, argl, bodyl)
    bodyl := [name, [type, argl, :bodyl]]
    if $PrettyPrint then PPRINT(bodyl)
    if NULL($COMPILE) then
      SAY '"No Compilation"
    else
      COMP370([bodyl])
    name

COMP(lfun) ==
    #lfun ~= 1 => BREAK()
    [COMP_-2 nf for nf in COMP_-1(CAR(lfun))]
    
compSPADSLAM(name, argl, bodyl) ==
    al := INTERNL(name, '";AL")
    auxfn := INTERNL(name, '";")
    g1 := GENSYM()
    g2 := GENSYM()
    u :=
         not(argl) => [[], [], [auxfn]]
         not(CDR(argl)) => [[g1], ["devaluate", g1], [auxfn, g1]]
         [g1, ["devaluateList", g1], _
           ["APPLY", ["FUNCTION", auxfn], g1]]
    [arg, argtran, app] := u
    la1 :=
         argl => [["SETQ", g2, ["assoc", argtran, al]], ["CDR", g2]]
         [al]
    la2 :=
         argl =>
              [true, ["SETQ", al,
                           ["cons5",
                                ["CONS", argtran, ["SETQ", g2, app]], al]],
                            g2]
         [true, ["SETQ", al, app]]
    lamex := ["LAMBDA", arg,
                ["LET", [g2],
                  ["COND", la1, la2]]]
    SETANDFILE(al, nil)
    u := [name,lamex]
    if $PrettyPrint then PRETTYPRINT(u)
    COMP370([u])
    u := [auxfn, ["LAMBDA", argl, :bodyl]]
    if $PrettyPrint then PRETTYPRINT(u)
    COMP370([u])
    name

makeClosedfnName() ==
    INTERNL($FUNNAME, '"!", STRINGIMAGE(LENGTH($CLOSEDFNS)))

lambdaHelper1(y) ==
    NOT(MEMQ(y, $locVars)) =>
        $locVars := [y, :$locVars]
        $newBindings := [y, :$newBindings]

lambdaHelper2(y) == MEMQ(y, $newBindings)

compTran1(x) ==
    ATOM(x) => nil
    u := CAR(x)
    u = "QUOTE" => nil
    if u = "MAKEPROP" and $TRACELETFLAG then
        RPLAC(CAR x, "MAKEPROP-SAY")
    u = "DCQ" =>
        SAY(["DCQ found in", x])
        BREAK()
    MEMQ(u, '(SPADLET SETQ LET)) =>
        if NOT($BOOT) or MEMQ($FUNNAME, $traceletFunctions) then
            NCONC(x, $FUNNAME__TAIL)
            RPLACA(x, "LETT")
        else if $TRACELETFLAG then
            -- this devious trick (due to RDJ) is needed since the compile
            -- looks only at global variables in top-level environment;
            -- thus SPADLET cannot itself test for such flags (7/83).
            RPLACA(x, "/TRACE-LET")
        else if u = "LET" then RPLACA(x, "SPADLET")
        compTran1(CDDR x)
        NOT(u = "SETQ") =>
            IDENTP(CADR(x)) => PUSHLOCVAR(CADR(x))
            EQCAR(CADR(x), "FLUID") =>
                PUSH(CADADR(x), $fluidVars)
                RPLAC(CADR(x), CADADR(x))
            MAPC(FUNCTION PUSHLOCVAR, LISTOFATOMS(CADR x))
    MEMQ(u, '(PROG LAMBDA)) =>
        $newBindings : local := nil
        MAPCAR(FUNCTION lambdaHelper1, x.1)
        res := compTran1(CDDR(x))
        $locVars := REMOVE_-IF(FUNCTION lambdaHelper2, $locVars)
        [u, CADR(x), :res]
    compTran1 u
    compTran1(CDR x)

compTran(x) ==
    $fluidVars : local := nil
    $locVars : local := nil
    [x1, x2, :xl3] := compExpand(x)
    compTran1 (xl3)
    [x3, :xlt3] := xl3
    x3 :=
        NULL(xlt3) and (ATOM(x3) or _
                            CAR(x3) = "SEQ" or _
                            not(CONTAINED("EXIT", x3))) => x3
        ["SEQ", :xl3]
    fluids := REMDUP(NREVERSE($fluidVars))
    $locVars := S_-(S_-(REMDUP(NREVERSE($locVars)), fluids), LISTOFATOMS (x2))
    lvars := APPEND(fluids, $locVars)
    x3 :=
        fluids => ["PROG", lvars, ["DECLARE", ["SPECIAL", :fluids]], _
                           ["RETURN", x3]]
        lvars or CONTAINED("RETURN", x3) => ["PROG", lvars, ["RETURN", x3]]
        x3
    fluids := compFluidize(x2)
    fluids => [x1, x2, ["DECLARE", ["SPECIAL", :fluids]], x3]
    [x1, x2, x3]

compNewnam(x) ==
    ATOM(x) => nil
    y := CAR(x)
    ATOM(y) =>
        if not(y = "QUOTE") then compNewnam(CDR(x))
        if y = "CLOSEDFN" and BOUNDP('$CLOSEDFNS) then
            u := makeClosedfnName()
            PUSH([u, CADR(x)], $CLOSEDFNS)
            RPLACA(x, "FUNCTION")
            RPLACA(CDR(x), u)
    compNewnam(CAR(x))
    compNewnam(CDR(x))

compFluidize(x) ==
    x and SYMBOLP(x) and x ~= "$" and x ~= "$$" and _
      SCHAR('"$", 0) = SCHAR(PNAME(x), 0) _
      and not(DIGITP (SCHAR(PNAME(x), 1))) => x
    ATOM(x) => nil
    QCAR(x) = "FLUID" => SECOND(x)
    a := compFluidize(QCAR(x))
    b := compFluidize(QCDR(x))
    a => CONS(a, b)
    b

compFluidize1(x) ==
    x and SYMBOLP(x) and x ~= "$" and x ~= "$$" and _
      SCHAR('"$", 0) = SCHAR(PNAME(x), 0) _
      and not(DIGITP (SCHAR(PNAME(x), 1))) => ["FLUID", x]
    ATOM(x) => x
    QCAR(x) = "FLUID" => x
    a := compFluidize1(QCAR(x))
    b := compFluidize1(QCDR(x))
    a = QCAR(x) and b = QCDR(x) => x
    CONS(a, b)

PUSHLOCVAR(x) ==
    x ~= "$" and SCHAR('"$", 0) = SCHAR(PNAME(x), 0) _
      and (not(SCHAR('",", 0) = SCHAR(PNAME(x), 1)) or BREAK())
      and not(DIGITP (SCHAR(PNAME(x), 1))) => nil
    PUSH(x, $locVars)

compExpand(x) ==
    ATOM(x) => x
    x is ["QUOTE",:.] => x
    x is ["SPADREDUCE", op, axis, body] =>
       axis ~= 0 => BREAK()
       compExpand(expandSPADREDUCE(op, body))
    MEMQ(CAR x, $COMP_-MACROLIST) => compExpand (MACROEXPAND_-1 (x))
    a := compExpand (car x)
    b := compExpand (cdr x)
    a = CAR x and b = CDR x => x
    CONS(a, b)
