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

)if false

PURPOSE: Comp is a modified version of Compile which is a preprocessor for
         calls to Lisp Compile.  It searches for variable assignments of
         form (SPADLET a b). It allows you to create local variables without
         declaring them local by moving them into a PROG variable list.

         Comp recognizes as new lambda types the forms SPADSLAM, SLAM,
         and entries on $clamList.  These cache results.  ("Saving LAMbda".)
         If the function is called with EQUAL arguments, returns the previous
         result computed.

         Comp expands iteration constructs (REPEAT, COLLECT, ...).

         The package also causes traced things which are recompiled to
         become untraced.

         This code was used for Boot, but now is only used on output
         of Spad and interpreter compilers.
)endif

COMP_-1(x) ==
  [fname, lamex, :.] := x
  $FUNNAME : local := fname
  $FUNNAME_TAIL : local := [fname]
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
    type is 'domain_functor =>
        compClam(name, argl, bodyl, "$ConstructorCache",
                 'domainEqualList, ['count])
    type is 'category_functor => compSPADSLAM(name, argl, bodyl)
    if type = 'mutable_domain_functor then
        type := 'LAMBDA
    bodyl := [name, [type, argl, :bodyl]]
    if $PrettyPrint then PPRINT(bodyl)
    if NULL($COMPILE) then
      SAY '"No Compilation"
    else
      COMP370(bodyl)
    name

COMP(fun) == [COMP_-2 nf for nf in COMP_-1(fun)]

compSPADSLAM(name, argl, bodyl) ==
    al := INTERNL(name, '";AL")
    auxfn := INTERNL(name, '";")
    g1 := GENSYM()
    g2 := GENSYM()
    u :=
         not(argl) => [[], [], [auxfn]]
         not(rest(argl)) => [[g1], ["devaluate", g1], [auxfn, g1]]
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
    output_lisp_defparameter(al, nil)
    u := [name,lamex]
    if $PrettyPrint then PRETTYPRINT(u)
    COMP370(u)
    u := [auxfn, ["LAMBDA", argl, :bodyl]]
    if $PrettyPrint then PRETTYPRINT(u)
    COMP370(u)
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
    u := first(x)
    u = "QUOTE" => nil
    if u = "MAKEPROP" and $TRACELETFLAG then
        rplac(first x, "MAKEPROP-SAY")
    MEMQ(u, '(SPADLET SETQ LET)) =>
        NCONC(x, $FUNNAME_TAIL)
        RPLACA(x, "LETT")
        compTran1(CDDR x)
        NOT(u = "SETQ") =>
            IDENTP(CADR(x)) => PUSHLOCVAR(CADR(x))
            EQCAR(CADR(x), "FLUID") =>
                PUSH(CADADR(x), $fluidVars)
                rplac(CADR(x), CADADR(x))
            BREAK()
            MAPC(FUNCTION PUSHLOCVAR, LISTOFATOMS(CADR x))
    MEMQ(u, '(PROG LAMBDA)) =>
        $newBindings : local := nil
        MAPCAR(FUNCTION lambdaHelper1, x.1)
        res := compTran1(CDDR(x))
        $locVars := REMOVE_-IF(FUNCTION lambdaHelper2, $locVars)
        [u, CADR(x), :res]
    compTran1 u
    compTran1(rest x)

compTranDryRun(x) ==
    $insideCapsuleFunctionIfTrue : local := false
    compTran(x)

compTran(x) ==
    $fluidVars : local := nil
    $locVars : local := nil
    [x1, x2, :xl3] := comp_expand(x)
    compTran1 (xl3)
    [x3, :xlt3] := xl3
    x3 :=
        NULL(xlt3) and (ATOM(x3) or _
                            first(x3) = "SEQ" or _
                            not(CONTAINED("EXIT", x3))) => x3
        ["SEQ", :xl3]
    fluids := REMDUP(NREVERSE($fluidVars))
    $locVars := S_-(S_-(REMDUP(NREVERSE($locVars)), fluids), LISTOFATOMS (x2))
    lvars := APPEND(fluids, $locVars)
    x3 :=
        fluids =>
            ["SPROG", compSpadProg(lvars),
             ["DECLARE", ["SPECIAL", :fluids]], x3]
        lvars or CONTAINED("RETURN", x3) =>
            ["SPROG", compSpadProg(lvars), x3]
        x3
    fluids := compFluidize(x2)
    x2 := addTypesToArgs(x2)
    fluids => [x1, x2, ["DECLARE", ["SPECIAL", :fluids]], x3]
    [x1, x2, x3]

addTypesToArgs(args) ==
    $insideCapsuleFunctionIfTrue =>
        sig := $signatureOfForm
        spadTypes := [(ATOM(t) => [t]; t) for t in [:rest(sig), first(sig)]]
        [[a, :t] for a in args for t in spadTypes]
    args

addNilTypesToArgs(args) ==
    $insideCapsuleFunctionIfTrue =>
        [[arg, nil] for arg in args]
    args

compSpadProg(lvars) ==
    lvarTypes := ($insideCapsuleFunctionIfTrue => $locVarsTypes; nil)
    types := []
    for lvar in lvars repeat
        x := ASSOC(lvar, lvarTypes)
        types := [[lvar, (x => rest(x); nil)], :types]
    NREVERSE(types)

compNewnam(x) ==
    ATOM(x) => nil
    y := first(x)
    ATOM(y) =>
        if not(y = "QUOTE") then compNewnam(rest(x))
        if y = "CLOSEDFN" and BOUNDP('$CLOSEDFNS) then
            u := makeClosedfnName()
            PUSH([u, CADR(x)], $CLOSEDFNS)
            RPLACA(x, "FUNCTION")
            RPLACA(rest(x), u)
    compNewnam(first(x))
    compNewnam(rest(x))

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

PUSHLOCVAR(x) ==
    x ~= "$" and SCHAR('"$", 0) = SCHAR(PNAME(x), 0) _
      and (not(SCHAR('",", 0) = SCHAR(PNAME(x), 1)) or BREAK())
      and not(DIGITP (SCHAR(PNAME(x), 1))) => nil
    PUSH(x, $locVars)

comp_expand(x) ==
    ATOM(x) => x
    x is ["QUOTE",:.] => x
    x is ["SPADREDUCE", op, axis, body] => BREAK()
    x is ["REPEAT", :body] => comp_expand(expandREPEAT(body))
    x is ["COLLECT", :body] => comp_expand(expandCOLLECT(body))
    x is ["COLLECTV", :body] => comp_expand(expandCOLLECTV(body))
    x is ["COLLECTVEC", :body] => comp_expand(expandCOLLECTV(body))
    a := comp_expand (car x)
    b := comp_expand (cdr x)
    a = first x and b = rest x => x
    CONS(a, b)

repeat_tran(l, lp) ==
    ATOM(l) => ERROR('"REPEAT FORMAT ERROR")
    IFCAR(IFCAR(l)) in '(EXIT RESET IN ON GSTEP ISTEP STEP
                     UNTIL WHILE SUCHTHAT) =>
        repeat_tran(rest(l), [first(l), :lp])
    [NREVERSE(lp), :MKPF(l, "PROGN")]

expandCOLLECT(l) ==
    [conds, :body] := repeat_tran(l, [])
    -- create init of accumulate
    init := ["SPADLET", G := GENSYM(), []]
    ASSOC("EXIT", conds) => BREAK()
    res := ["NREVERSE", G]
    -- next code to accumulate result
    acc := ["SETQ", G, ["CONS", body, G]]
    ["PROGN", init, ["REPEAT", ["EXIT", res], :conds, acc]]

BADDO(OL) == ERROR(FORMAT(nil, '"BAD DO FORMAT~%~A", OL))

expandDO(vl, endtest, exitforms, body_forms) ==
    vars := []
    u_vars := []
    u_vals := []
    inits := []
    for vi in vl repeat
        [v, init] := vi
        not(IDENTP(v)) => BADDO(OL)
        vars := [v, :vars]
        inits := [init, :inits]
        if vi is [., ., u_val] then
            u_vars := [v, :u_vars]
            u_vals := [u_val, :u_vals]
    if endtest then endtest := ["COND", [endtest, ["GO", "G191"]]]
    exitforms := ["EXIT", exitforms]
    u_vars3 := nil
    for vv in u_vars for uu in u_vals repeat
        u_vars3 :=
            NULL(u_vars3) => ["SETQ", vv, uu]
            ["SETQ", vv, ["PROG1", uu, u_vars3]]
    lets := [["SPADLET", var, init] for var in vars for init in inits]
    ["SEQ", :lets, :["G190", endtest, body_forms,
          u_vars3, ["GO", "G190"], "G191", exitforms]]

seq_opt(seq) ==
   seq is ["SEQ", ["EXIT", body]] and body is ["SEQ",:.] => body
   seq

MK_inc_SI(x) ==
    ATOM(x) => ['inc_SI, x]
    x is [op, xx, 1] and (op = 'sub_SI or op = "-") => xx
    ['inc_SI, x]

expandREPEAT(l) ==
    [conds, :body] := repeat_tran(l, [])
    tests := []
    vl := []
    result_expr := nil
    for X in conds repeat
        ATOM(X) => BREAK()
        U := rest(X)
        -- A hack to increase the likelihood of small integers
        if X is ["STEP", ., i1, i2, :.] and member(i1, '(2 1 0 (One) (Zero)))
           and member(i2, '(1 (One))) then X := ["ISTEP", :U]
        op := first(X)
        op = "GSTEP" =>
            [var, empty_form, step_form, init_form] := U
            tests := [["OR", ["SPADCALL", empty_form],
                             ["PROGN", ["SETQ", var, ["SPADCALL", step_form]],
                                  nil]], :tests]
            vl := [[var, init_form], :vl]
        op = "STEP" =>
            [var, start, inc, :op_limit] := U
            -- If not constant compute only once
            if not(INTEGERP(inc)) then
                vl := [[(tmp := GENSYM()), inc], :vl]
                inc := tmp
            if op_limit then
                -- If not constant compute only once
                if not(INTEGERP(final := first(op_limit))) then
                    vl := [[(tmp := GENSYM()), final], :vl]
                    final := tmp
                tests :=
                  [(INTEGERP(inc) =>
                     [(MINUSP(inc) => "<" ; ">"), var, final];
                        ["IF", ["MINUSP", inc],
                          ["<", var, final],
                            [">", var, final]]),
                              :tests]
            vl := [[var, start, ["+", var, inc]], :vl]
        op = "ISTEP" =>
            [var, start, inc, :op_limit] := U
            -- If not constant compute only once
            if not(INTEGERP(inc)) then
                vl := [[(tmp := GENSYM()), inc], :vl]
                inc := tmp
            if op_limit then
                if not(INTEGERP(final := first(op_limit))) then
                    -- If not constant compute only once
                    vl := [[(tmp := GENSYM()), final], :vl]
                    final := tmp
                tests :=
                  [(INTEGERP(inc) =>
                     [(negative?_SI(inc) => "less_SI" ; "greater_SI"),
                       var, final];
                        ["IF", ["negative?_SI", inc],
                          ["less_SI", var, final],
                            ["greater_SI", var, final]]),
                              :tests]
            vl := [[var, start,
                 (member(inc, '(1 (One))) => MK_inc_SI(first(U));
                   ["add_SI", var, inc])], :vl]
        op = "ON" =>
            tests := [["ATOM", first(U)], :tests]
            vl := [[first(U), CADR(U), ["CDR", first(U)]], :vl]
        op = "RESET" => tests := [["PROGN", first(U), nil], :tests]
        op = "IN" =>
            tt :=
                SYMBOLP(first(U)) and SYMBOL_-PACKAGE(first(U))
                  and $TRACELETFLAG =>
                    [["/TRACELET-PRINT", first(U), (first U)]]
                nil
            tests := [["OR", ["ATOM", (G := GENSYM())],
                             ["PROGN", ["SETQ", first(U), ["CAR", G]],
                               :APPEND(tt, [nil])]], :tests]
            vl := [[G, CADR(U), ["CDR", G]], :vl]
            vl := [[first(U), nil], :vl]
        op = "UNTIL" =>
            G := GENSYM()
            tests := [G, :tests]
            vl := [[G, nil, first(U)], :vl]
        op = "WHILE" => tests := [["NULL", first(U)], :tests]
        op = "SUCHTHAT" => body := ["COND", [first(U), body]]
        op = "EXIT" =>
            result_expr => BREAK()
            result_expr := first(U)
        FAIL()
    expandDO(NREVERSE(vl), MKPF(NREVERSE(tests), "OR"), result_expr,
             seq_opt(["SEQ", ["EXIT", body]]))

expandCOLLECTV(l) ==
    -- If we can work out how often we will go round allocate a vector first
    conds :=  []
    [body, :iters] := REVERSE(l)
    counter_var := nil
    ret_val := nil
    for iter in iters repeat
        op := first(iter)
        op in '(SUCHTHAT WHILE UNTIL GSTEP) =>
            ret_val := ["LIST2VEC", ["COLLECT", :l]]
            return nil -- break loop
        op in '(IN ON) =>
            conds := [["SIZE", CADDR(iter)], :conds]
        op in '(STEP ISTEP) =>
            [., var, start, step, :opt_limit] := iter
            if start = 0 and step = 1 then
                counter_var := var
            -- there may not be a limit
            if opt_limit then
                limit := first(opt_limit)
                cond :=
                    step = 1 =>
                        start = 1 => limit
                        start = 0 => MK_inc_SI(limit)
                        MK_inc_SI(["-", limit, start])
                    start = 1 => ["/", limit, step]
                    start = 0 => ["/", MK_inc_SI(limit), step]
                    ["/", ["-", MK_inc_SI(limit), start],
                                            step]
                conds := [cond, :conds]
        ERROR('"Cannot handle COLLECTV expansion")
    ret_val => ret_val
    if NULL(counter_var) then
        counter_var := GENSYM()
        iters := [["ISTEP", counter_var, 0, 1], :iters]
    lv :=
        NULL(conds) => FAIL()
        NULL(rest(conds)) => first(conds)
        ["MIN", :conds]
    res := GENSYM()
    ["PROGN", ["SPADLET", res, ["GETREFV", lv]],
              ["REPEAT", :iters, ["SETELT", res, counter_var, body]],
                 res]

DEFPARAMETER($comp370_apply, nil)

COMP370(fn) ==
    not(fn is [fname, [ltype, args, :body]]) => BREAK()
    args :=
        NULL(args) => args
        LISTP(args) and $insideCapsuleFunctionIfTrue =>
            [(STRINGP(CAR(arg)) => CONS(GENTEMP(), CDR(arg));
              not(SYMBOLP(CAR(arg))) => BREAK();
              arg)
             for arg in args]
        SYMBOLP(args) => ["&REST", args]
        ATOM(args) => BREAK()
        [(STRINGP(arg) => GENTEMP(); not(SYMBOLP(arg)) => BREAK(); arg)
            for arg in args]
    defun := if $insideCapsuleFunctionIfTrue then "SDEFUN" else "DEFUN"
    nbody := [defun, fname, args, :body]
    if $comp370_apply then
        FUNCALL($comp370_apply, fname, nbody)

MKPF(l, op) ==
    if GET(op, "NARY") then
        l := MKPFFLATTEN1(l, op, nil)
    MKPF1(l, op)

MKPFFLATTEN(x, op) ==
    ATOM(x) => x
    EQL(first(x), op) => [op, :MKPFFLATTEN1(rest x, op, nil)]
    [MKPFFLATTEN(first x, op), :MKPFFLATTEN(rest x, op)]

MKPFFLATTEN1(l, op, r) ==
    NULL(l) => r
    x := MKPFFLATTEN(first(l), op)
    MKPFFLATTEN1(rest l, op, APPEND(r, (x is [=op, :r1] => r1; [x])))

MKPF1(l, op) ==
    op = "PLUS" => BREAK()
    op = "TIMES" => BREAK()
    op = "QUOTIENT" => BREAK()
    op = "MINUS" => BREAK()
    op = "DIFFERENCE" => BREAK()
    op = "EXPT" =>
        l is [x, y] =>
            EQL(y, 0) => 1
            EQL(y, 1) => x
            member(x, '(0 1 (ZERO) (ONE))) => x
            ["EXPT", :l]
        FAIL()
    op = "OR" =>
        MEMBER(true, l) => ["QUOTE", true]
        l := REMOVE(false, l)
        NULL(l) => false
        rest(l) => ["OR", :l]
        first(l)
    op = "or" =>
        MEMBER(true, l) => true
        l := REMOVE(false, l)
        NULL(l) => false
        rest(l) => ["or", :l]
        first(l)
    op = "NULL" =>
        rest(l) => FAIL()
        l is [["NULL", :l1]] => first(l1)
        first(l) = true => false
        NULL(first(l)) => ["QUOTE", true]
        ["NULL", :l]
    op = "and" =>
        l := REMOVE(true, REMOVE("true", l))
        NULL(l) => true
        rest(l) => ["and", :l]
        first(l)
    op = "AND" =>
        l := REMOVE(true, REMOVE("true", l))
        NULL(l) => ["QUOTE", true]
        rest(l) => ["AND", :l]
        first(l)
    op = "PROGN" =>
        l := REMOVE(nil, l)
        NULL(l) => nil
        rest(l) => ["PROGN", :l]
        first(l)
    op = "SEQ" =>
        l is [["EXIT", :l1], :.] => first(l1)
        rest(l) => ["SEQ", :l]
        first(l)
    op = "LIST" =>
        l => ["LIST", :l]
        nil
    op = "CONS" =>
        rest(l) => ["CONS", :l]
        first(l)
    [op, :l]
