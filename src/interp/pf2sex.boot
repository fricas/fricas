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

$dotdot := INTERN('"..", '"BOOT")

-- Pftree to s-expression translation.  Used to interface the new parser
-- technology to the interpreter.  The input is a parseTree and the
-- output is an old-parser-style s-expression

pf2Sex pf ==
  intUnsetQuiet()
  $insideRule:local := false
  $insideApplication: local := false
  $insideSEQ: local := false
  pf2Sex1 pf

pf2Sex1 pf ==
  pfNothing? pf =>
    "noBranch"
  pfSymbol? pf =>
    $insideRule = 'left =>
      s := pfSymbolSymbol pf
      ["constant", ["QUOTE", s]]
    ["QUOTE", pfSymbolSymbol pf]
  pfLiteral? pf =>
    pfLiteral2Sex pf
  pfId? pf =>
    $insideRule =>
      s := pfIdSymbol pf
      SymMemQ(s, '(%pi %e %i)) => s
      ["QUOTE", s]
    pfIdSymbol pf
  pfApplication? pf =>
    pfApplication2Sex pf
  pfTuple? pf =>
    ["Tuple", :[pf2Sex1 x for x in pf0TupleParts pf]]
  pfIf? pf =>
    ['IF, pf2Sex1 pfIfCond pf, pf2Sex1 pfIfThen pf, pf2Sex1 pfIfElse pf]
  pfTagged? pf =>
    tag := pfTaggedTag pf
    tagPart :=
      pfTuple? tag =>
        ['Tuple, :[pf2Sex1 arg for arg in pf0TupleParts tag]]
      pf2Sex1 tag
    [":", tagPart, pf2Sex1 pfTaggedExpr pf]
  pfCoerceto? pf =>
    ["::", pf2Sex1 pfCoercetoExpr pf, pf2Sex1 pfCoercetoType pf]
  pfPretend? pf =>
    ["pretend", pf2Sex1 pfPretendExpr pf, pf2Sex1 pfPretendType pf]
  pfFromdom? pf =>
    op := opTran  pf2Sex1 pfFromdomWhat pf
--  if op = "braceFromCurly" then op := "brace"
    if op = "braceFromCurly" then op := "SEQ"
    ["$elt", pf2Sex1 pfFromdomDomain pf, op]
  pfSequence? pf =>
    pfSequence2Sex pf
  pfExit? pf =>
    $insideSEQ => ["exit", pf2Sex1 pfExitCond pf, pf2Sex1 pfExitExpr pf]
    ["IF", pf2Sex1 pfExitCond pf, pf2Sex1 pfExitExpr pf, "noBranch"]
  pfLoop? pf =>
    ["REPEAT", :loopIters2Sex  pf0LoopIterators pf]
  pfCollect? pf =>
    pfCollect2Sex pf
  pfForin? pf =>
    ["IN", :[pf2Sex1 x for x in pf0ForinLhs pf], pf2Sex1 pfForinWhole pf]
  pfWhile? pf =>
    ["WHILE", pf2Sex1 pfWhileCond pf]
  pfSuchthat? pf =>
    $insideRule = 'left =>
      keyedSystemError("S2GE0017", ['"pf2Sex1: pfSuchThat"])
    ["|", pf2Sex1 pfSuchthatCond pf]
  pfDo? pf =>
    pf2Sex1 pfDoBody pf
  pfTyped? pf =>
    type := pfTypedType pf
    pfNothing? type => pf2Sex1 pfTypedId pf
    [":", pf2Sex1 pfTypedId pf, pf2Sex1 pfTypedType pf]
  pfAssign? pf =>
    idList := [pf2Sex1 x for x in pf0AssignLhsItems pf]
    if #idList ~= 1 then idList := ['Tuple, :idList]
    else idList := first idList
    ["LET", idList, pf2Sex1 pfAssignRhs pf]
  pfDefinition? pf =>
    pfDefinition2Sex pf
  pfLambda? pf =>
    pfLambda2Sex pf
  pfMLambda? pf =>
    "/throwAway"
  pfRestrict? pf =>
    ["@", pf2Sex1 pfRestrictExpr pf, pf2Sex1 pfRestrictType pf]
  pfFree? pf =>
    ['free, :[pf2Sex1 item for item in pf0FreeItems pf]]
  pfLocal? pf =>
    ['local, :[pf2Sex1 item for item in pf0LocalItems pf]]
  pfWrong? pf =>
   spadThrow()
  pfAnd? pf =>
    ["and", pf2Sex1 pfAndLeft pf, pf2Sex1 pfAndRight pf]
  pfOr? pf =>
    ["or", pf2Sex1 pfOrLeft pf, pf2Sex1 pfOrRight pf]
  pfNot? pf =>
    ["not", pf2Sex1 pfNotArg pf]
  pfNovalue? pf =>
    intSetQuiet()
    ["SEQ", pf2Sex1 pfNovalueExpr pf]
  pfRule? pf =>
    pfRule2Sex pf
  pfBreak? pf =>
    ["break", pfBreakFrom pf]
  pfMacro? pf =>
    "/throwAway"
  pfReturn? pf =>
    ["return", pf2Sex1 pfReturnExpr pf]
  pfIterate? pf =>
    ["iterate"]
  pfWhere? pf =>
    args := [pf2Sex1 p for p in pf0WhereContext pf]
    #args = 1 =>
      ["where", pf2Sex1 pfWhereExpr pf, :args]
    ["where", pf2Sex1 pfWhereExpr pf, ["SEQ", :args]]

  -- under strange circumstances/piling, system commands can wind
  -- up in expressions. This just passes it through as a string for
  -- the user to figure out what happened.
  pfAbSynOp(pf) = "command" => tokPart(pf)

  keyedSystemError("S2GE0017", ['"pf2Sex1"])

pfLiteral2Sex pf ==
  type := pfLiteralClass pf
  type = 'integer =>
    READ_-FROM_-STRING pfLiteralString pf
  type = 'string or type = 'char =>
    pfLiteralString pf
  type = 'float =>
    float2Sex pfLiteralString pf
  type = 'symbol =>
    $insideRule =>
      s := pfSymbolSymbol pf
      ["QUOTE", s]
    pfSymbolSymbol pf
  type = 'expression =>
      ["QUOTE", pfLeafToken pf]
  keyedSystemError("S2GE0017", ['"pfLiteral2Sex: unexpected form"])

symEqual(sym, sym2) == EQ(sym, sym2)

SymMemQ(sy, l) == MEMQ(sy, l)

pmDontQuote? sy ==
   SymMemQ(sy, '(_+ _- _* _*_* _^ _/ log exp pi sqrt ei li erf ci si dilog _
              sin cos tan cot sec csc asin acos atan acot asec acsc _
              sinh cosh tanh coth sech csch asinh acosh atanh acoth asech acsc))

pfOp2Sex pf ==
  alreadyQuoted := pfSymbol? pf
  op := pf2Sex1 pf
  op is ["QUOTE", realOp] =>
    $insideRule = 'left => realOp
    $insideRule = 'right =>
      pmDontQuote? realOp => realOp
      $quotedOpList := [op, :$quotedOpList]
      op
    symEqual(realOp, "|") => realOp
    symEqual(realOp, ":") => realOp
    symEqual(realOp, "?") => realOp
    op
  op

pfApplication2Sex pf ==
  $insideApplication: local := true
  op := pfOp2Sex pfApplicationOp pf
  op := opTran op
  op = "->" =>
    args := pf0TupleParts pfApplicationArg pf
    if pfTuple? first args then
      typeList := [pf2Sex1 arg for arg in pf0TupleParts first args]
    else
      typeList := [pf2Sex1 first args]
    args := [pf2Sex1 CADR args, :typeList]
    ["Mapping", :args]
  symEqual(op, ":") and $insideRule = 'left =>
    ["multiple", pf2Sex pfApplicationArg pf]
  symEqual(op, "?") and $insideRule = 'left =>
    ["optional", pf2Sex pfApplicationArg pf]
  args := pfApplicationArg pf
  pfTuple? args =>
    symEqual(op, "|") and $insideRule = 'left =>
      pfSuchThat2Sex args
    argSex := rest pf2Sex1 args
    symEqual(op, "AND") =>
      ["and", first argSex, CADR argSex]
    symEqual(op, "OR") =>
      ["or", first argSex, CADR argSex]
    symEqual(op, "Iterate") =>
      ["iterate"]
    symEqual(op, "by") =>
      ["BY", :argSex]
    symEqual(op, "braceFromCurly") =>
--    ["brace", ["construct", :argSex]]
      argSex is ["SEQ",:.] => argSex
      ["SEQ", :argSex]
    op is [qt, realOp] and symEqual(qt, "QUOTE") =>
       ["applyQuote", op, :argSex]
    val := hasOptArgs? argSex => [op, :val]
    [op, :argSex]
  op is [qt, realOp] and symEqual(qt, "QUOTE") =>
     ["applyQuote", op, pf2Sex1 args]
  symEqual(op, "braceFromCurly") =>
--  ["brace", ["construct", pf2Sex1 args]]
    x := pf2Sex1 args
    x is ["SEQ", :.] => x
    ["SEQ", x]
  symEqual(op, "by") =>
      ["BY", pf2Sex1 args]
  [op, pf2Sex1 args]

hasOptArgs? argSex ==
  nonOpt := nil
  opt := nil
  for arg in argSex repeat
    arg is ["OPTARG", lhs, rhs] =>
      opt := [[lhs, rhs], :opt]
    nonOpt := [arg, :nonOpt]
  null opt => nil
  NCONC (nreverse nonOpt, [["construct", :nreverse opt]])

pfDefinition2Sex pf ==
  $insideApplication =>
    ["OPTARG", pf2Sex1 first pf0DefinitionLhsItems pf,
     pf2Sex1 pfDefinitionRhs pf]
  idList := [pf2Sex1 x for x in pf0DefinitionLhsItems pf]
  #idList ~= 1 =>
    systemError '"lhs of definition must be a single item in the interpreter"
  id := first idList
  rhs := pfDefinitionRhs pf
  [argList, :body] := pfLambdaTran rhs
  ["DEF", (argList = 'id => id; [id, :argList]), :body]

pfLambdaTran pf ==
  pfLambda? pf =>
    argTypeList := nil
    argList := nil
    for arg in pf0LambdaArgs pf repeat
      pfTyped? arg =>
        argList := [pfCollectArgTran pfTypedId arg, :argList]
        pfNothing? pfTypedType arg =>
          argTypeList := [nil, :argTypeList]
        argTypeList := [pf2Sex1 pfTypedType arg, :argTypeList]
      systemError '"definition args should be typed"
    argList := nreverse argList
    retType :=
      pfNothing? pfLambdaRets pf => nil
      pf2Sex1 pfLambdaRets pf
    argTypeList := [retType, :nreverse argTypeList]
    [argList, :[argTypeList, [nil for arg in argTypeList],
      pf2Sex1 pfLambdaBody pf]]
  ['id, :['(()), '(()), pf2Sex1 pf]]

pfLambda2Sex pf ==
  [argList, :body] := pfLambdaTran pf
  ["ADEF", argList, :body]

pfCollectArgTran pf ==
  pfCollect? pf =>
    conds := [pf2Sex1 x for x in pfParts pfCollectIterators pf]
    id := pf2Sex1 pfCollectBody pf
    conds is [["|", cond]] =>
      ["|", id, cond]
    [id, :conds]
  pf2Sex1 pf

opTran op ==
  op = $dotdot => "SEGMENT"
  op = "[]" => "construct"
  op = "{}" => "braceFromCurly"
  op = "IS" => "is"
  op

pfSequence2Sex pf ==
  $insideSEQ:local := true
  seq := pfSequence2Sex0 [pf2Sex1 x for x in pf0SequenceArgs pf]
  seq is ["SEQ", :ruleList] and ruleList is [["rule", :.], :.] =>
    ["ruleset", ["construct", :ruleList]]
  seq

pfSequence2Sex0 seqList ==
  null seqList => "noBranch"
  seqTranList := []
  while seqList ~= nil repeat
    item := first seqList
    item is ["exit", cond, value] =>
      item := ["IF", cond, value, pfSequence2Sex0 rest seqList]
      seqTranList := [item, :seqTranList]
      seqList := nil
    seqTranList := [item ,:seqTranList]
    seqList := rest seqList
  #seqTranList = 1 => first seqTranList
  ["SEQ", :nreverse seqTranList]

intNewFloat() == ["Float"]

string_to_int(s) ==  READ_-FROM_-STRING(s)

float2Sex [i_str, fr_str, e_str] ==
  int_part := string_to_int(i_str)
  exp_part := string_to_int(e_str)
  bfForm := make_float(int_part, string_to_int(fr_str),
    LENGTH fr_str, exp_part)
  [., frac, exp] := bfForm
  [["$elt", intNewFloat(), 'float], frac, exp, 10]

loopIters2Sex iterList ==
  result := nil
  for iter in iterList repeat
    sex := pf2Sex1 iter
    sex is ['IN, var, ['SEGMENT, i, ["BY", incr]]] =>
      result := [['STEP, var, i, incr], :result]
    sex is ['IN, var, ["BY", ['SEGMENT, i, j], incr]] =>
      result := [['STEP, var, i, incr, j], :result]
    sex is ['IN, var, ['SEGMENT, i, j]] =>
      result := [['STEP, var, i, 1, j], :result]
    result := [sex, :result]
  nreverse result

pfCollect2Sex pf ==
  sex := ["COLLECT", :loopIters2Sex pfParts pfCollectIterators pf,
    pf2Sex1 pfCollectBody pf]
  sex is ["COLLECT", ["|", cond], var] and SYMBOLP var =>
    ["|", var, cond]
  sex

pfRule2Sex pf ==
  $quotedOpList:local := nil
  $predicateList:local := nil
  $multiVarPredicateList:local := nil
  lhs := pfLhsRule2Sex pfRuleLhsItems pf
  rhs := pfRhsRule2Sex pfRuleRhs pf
  lhs := ruleLhsTran lhs
  rulePredicateTran
    $quotedOpList => ["rule", lhs, rhs, ["construct", :$quotedOpList]]
    ["rule", lhs, rhs]


ruleLhsTran ruleLhs ==
  for pred in $predicateList repeat
    [name, predLhs, :predRhs] := pred
    vars := patternVarsOf predRhs
    rest(vars) =>  -- if there is more than one patternVariable
      ruleLhs := NSUBST(predLhs, name, ruleLhs)
      $multiVarPredicateList := [pred, :$multiVarPredicateList]
    predicate :=
      [., var] := predLhs
      ["suchThat", predLhs, ["ADEF", [var],
        '((Boolean) (Expression (Integer))), '(() ()), predRhs]]
    ruleLhs := NSUBST(predicate, name, ruleLhs)
  ruleLhs

rulePredicateTran rule ==
  null $multiVarPredicateList => rule
  varList := patternVarsOf [rhs for [.,.,:rhs] in $multiVarPredicateList]
  predBody :=
    rest $multiVarPredicateList =>
      ['AND, :[:pvarPredTran(rhs, varList) for [.,.,:rhs] in
        $multiVarPredicateList]]
    [[.,.,:rhs],:.] := $multiVarPredicateList
    pvarPredTran(rhs, varList)
  ['suchThat, rule,
   ['construct, :[["QUOTE", var] for var in varList]],
    ['ADEF, '(predicateVariable),
     '((Boolean) (List (Expression (Integer)))), '(() ()),
      predBody]]

pvarPredTran(rhs, varList) ==
  for var in varList for i in 1.. repeat
    rhs := NSUBST(['elt, 'predicateVariable, i], var, rhs)
  rhs

patternVarsOf expr ==
  patternVarsOf1(expr, nil)

patternVarsOf1(expr, varList) ==
  NULL expr => varList
  ATOM expr =>
    null SYMBOLP expr => varList
    SymMemQ(expr, varList) => varList
    [expr, :varList]
  expr is [op, :argl] =>
    for arg in argl repeat
      varList := patternVarsOf1(arg, varList)
    varList
  varList

pfLhsRule2Sex lhs ==
  $insideRule: local := 'left
  pf2Sex1 lhs


pfRhsRule2Sex rhs ==
  $insideRule: local := 'right
  pf2Sex1 rhs

pfSuchThat2Sex args ==
  name := GENTEMP()
  argList := pf0TupleParts args
  lhsSex := pf2Sex1 first argList
  rhsSex := pf2Sex CADR argList
  $predicateList := [[name, lhsSex, :rhsSex], :$predicateList]
  name
