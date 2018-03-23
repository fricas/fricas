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

--% Attributed Structures (astr)
-- For objects which are pairs where the first field is either just a tag
-- (an identifier) or a pair which is the tag and an association list.

-- Pick off the tag
ncTag x ==
   not PAIRP x => ncBug('S2CB0031,[])
   x := QCAR x
   IDENTP x => x
   not PAIRP x => ncBug('S2CB0031,[])
   QCAR x

-- Pick off the property list
ncAlist x ==
   not PAIRP x => ncBug('S2CB0031,[])
   x := QCAR x
   IDENTP x => NIL
   not PAIRP x => ncBug('S2CB0031,[])
   QCDR x

 --- Get the entry for key k on x's association list
ncEltQ(x,k) ==
   r := ASSQ(k,ncAlist x)
   NULL r => ncBug ('S2CB0007,[k])
   rest r

-- Put (k . v) on the association list of x and return v
-- case1: ncPutQ(x,k,v) where k is a key (an identifier), v a value
--        put the pair (k . v) on the association list of x and return v
-- case2: ncPutQ(x,k,v) where k is a list of keys, v a list of values
--        equivalent to [ncPutQ(x,key,val) for key in k for val in v]
ncPutQ(x,k,v) ==
   LISTP k =>
      for key in k for val in v repeat ncPutQ(x,key,val)
      v
   r := ASSQ(k,ncAlist x)
   if NULL r then
      r := CONS( CONS(k,v), ncAlist x)
      RPLACA(x,CONS(ncTag x,r))
   else
      RPLACD(r,v)
   v

)if false
Abstract Syntax Trees

This file provides functions to create and examine abstract
syntax trees.  These are called pform, for short.
The definition of valid pforms see ABSTRACT BOOT.

!! This file also contains constructors for concrete syntax, although
!! they should be somewhere else.

THE PFORM DATA STRUCTURE
 Leaves: [hd, tok, pos]
 Trees:  [hd, tree, tree, ...]
 hd is either an id or (id . alist)
)endif

--constructer and selectors for leaf tokens

tokConstruct(hd,tok,:pos)==
         a:=cons(hd,tok)
         IFCAR pos =>
             pfNoPosition? first pos => a
             ncPutQ(a, "posn", first pos)
             a
         a

tokType x== ncTag x
tokPart x == rest x
tokPosn x==
     a:= ASSQ("posn",ncAlist x)
     if a then rest a else pfNoPosition()

pfAbSynOp form ==
    hd := first form
    IFCAR hd or hd

pfAbSynOp?(form, op) ==
    hd := first form
    EQ(hd, op) or EQCAR(hd, op)

pfLeaf? form ==
  MEMQ(pfAbSynOp form,
       '(id idsy symbol string char float expression integer
          Document error))

pfLeaf(x,y,:z)      == tokConstruct(x,y, IFCAR z or pfNoPosition())
pfLeafToken form    == tokPart form
pfLeafPosition form == tokPosn form

pfTree(x,y)         == CONS(x,y)       -- was ==>
pfParts  form       == rest form       -- was ==>
pfFirst  form       == CADR form       -- was ==>
pfSecond form       == CADDR form       -- was ==>

--% SPECIAL NODES
pfListOf x          == pfTree('listOf,x)
pfListOf? x         == pfAbSynOp?(x,'listOf)
pfAppend list       == APPLY(function APPEND,list)

pfNothing ()        == pfTree('nothing, [])
pfNothing? form     == pfAbSynOp?(form, 'nothing)

-- SemiColon

pfSemiColon(pfbody) == pfTree('SemiColon, [pfbody])
pfSemiColon?(pf)    == pfAbSynOp? (pf, 'SemiColon)
pfSemiColonBody pf   == CADR pf       -- was ==>

--% LEAVES
pfId(expr)               == pfLeaf('id, expr)
pfIdPos(expr,pos)        == pfLeaf('id,expr,pos)
pfId? form               ==
        pfAbSynOp?(form,'id) or pfAbSynOp?(form,'idsy)
pfSymbolVariable? form   == pfAbSynOp?(form,'idsy)
pfIdSymbol form          == tokPart form
--pfAmpersand(amptok,name) == name

pfDocument strings       == pfLeaf('Document, strings)
pfDocument? form         == pfAbSynOp?(form, 'Document)
pfDocumentText form      == tokPart form

pfLiteral? form ==
      MEMQ(pfAbSynOp form,'(integer symbol expression
                    one zero char string float))

pfLiteralClass form      == pfAbSynOp form
pfLiteralString form     == tokPart form

pfStringConstString form     == tokPart form

pfExpression(expr, :optpos) ==
               pfLeaf("expression", expr, IFCAR optpos)
pfExpression? form          == pfAbSynOp?(form, 'expression)

pfSymbol(expr, :optpos) ==
               pfLeaf("symbol", expr, IFCAR optpos)

pfSymb(expr, :optpos) ==
         if pfLeaf? expr
         then pfSymbol(tokPart expr,IFCAR optpos)
         else pfExpression(pfSexpr expr,IFCAR optpos)

pfSymbol? form          == pfAbSynOp?(form, 'symbol)

pfSymbolSymbol form     == tokPart form

--% TREES
-- parser interface functions
-- these are potential sources of trouble in macro expansion

-- the comment is attached to all signatutres
pfWDec(doc,name)   == [pfWDeclare(i,doc) for i in pfParts name]

pfTweakIf form==
    a:=pfIfElse form
    b:=if pfNothing? a then pfListOf [] else a
    pfTree('WIf,[pfIfCond form,pfIfThen form,b])

pfInfApplication(op,left,right)==
   EQ(pfIdSymbol op,"and")=> pfAnd (left,right)
   EQ(pfIdSymbol op, "or")=> pfOr (left,right)
   pfApplication(op,pfTuple pfListOf [left,right])

pfAnd(pfleft, pfright) == pfTree('And, [pfleft, pfright])
pfAnd?(pf) == pfAbSynOp? (pf, 'And)
pfAndLeft pf == CADR pf       -- was ==>
pfAndRight pf == CADDR pf       -- was ==>

pfOr(pfleft, pfright) == pfTree('Or, [pfleft, pfright])
pfOr?(pf) == pfAbSynOp? (pf, 'Or)
pfOrLeft pf == CADR pf       -- was ==>
pfOrRight pf == CADDR pf       -- was ==>

pfNot(arg) == pfTree('Not, [arg])
pfNot?(pf) == pfAbSynOp? (pf, 'Not)
pfNotArg pf == CADR pf       -- was ==>

pfEnSequence a==
           if null a
           then  pfTuple pfListOf a
           else if null cdr a
                then  car a
                else  pfSequence pfListOf a
pfFromDom(dom,expr)==
    if pfApplication? expr
    then pfApplication(pfFromdom(pfApplicationOp expr,dom),
                       pfApplicationArg expr)
    else pfFromdom(expr,dom)

pfReturnTyped(type,body)==pfTree('returntyped,[type,body])

pfLam(variable,body)==-- called from parser
    rets:= if pfAbSynOp?(body,'returntyped)
           then pfFirst body
           else pfNothing ()
    bdy:= if pfAbSynOp?(body,'returntyped) then pfSecond body else body
    pfLambda(variable,rets,bdy)

pfIfThenOnly(pred,first)==pfIf(pred,first,pfNothing())

pfLp(iterators,body)==
       pfLoop pfListOf [:iterators,pfDo body]
pfLoop1 body == pfLoop pfListOf [pfDo body]


pfExitNoCond value== pfExit(pfNothing(),value)

pfReturnNoName(value)==pfReturn(value,pfNothing())

pfBrace(a,part)==pfApplication(pfIdPos( "{}",tokPosn a),part)

pfBracket(a,part) ==  pfApplication(pfIdPos( "[]",tokPosn a),part)

pfParen(a,part)==part

pfPile(part)==part

pfSpread(l,t)==  [pfTyped(i,t) for i in l]

pfTupleList form== pfParts pfTupleParts form

--The rest have been generated from ABCUT INPUT
-- 1/31/89


--              Add         / Application  / Assign /
--              Coerceto    / Collect      / ComDefinition  / DeclPart /
--              Exit        / Export       / Free /
--              Fromdom     / Id           / If            / Inline /
--              Iterate     / Lambda /
--              Break       / Literal      / Local         / Loop   /
--              MLambda     / Pretend      / Restrict    / Return /
--              Sequence    / Tagged       / Tuple         / Typing /
--              Where       / With

pfExpr? pf ==
     pfAdd? pf or _
     pfApplication? pf or _
     pfAssign? pf or _
     pfCoerceto? pf or _
     pfCollect? pf or _
     pfComDefinition? pf or _
     pfDeclPart? pf or _
     pfExit? pf or _
     pfExport? pf or _
     pfFree? pf or _
     pfFromdom? pf or _
     pfId? pf or _
     pfIf? pf or _
     pfInline? pf or _
     pfIterate? pf or _
     pfLambda? pf or _
     pfBreak? pf or _
     pfLiteral? pf or _
     pfLocal? pf or _
     pfLoop? pf or _
     pfMLambda? pf or _
     pfPretend? pf or _
     pfRestrict? pf or _
     pfReturn? pf or _
     pfTagged? pf or _
     pfTuple? pf or _
     pfWhere? pf or _
     pfWith? pf


pfDeclPart? pf ==
     pfTyping? pf or _
     pfImport? pf or _
     pfDefinition? pf or _
     pfSequence? pf or _
     pfDWhere? pf or _
     pfMacro? pf


-- Wrong       := (Why: Document, Rubble: [Expr])

pfWrong(pfwhy, pfrubble) == pfTree('Wrong, [pfwhy, pfrubble])
pfWrong?(pf) == pfAbSynOp? (pf, 'Wrong)
pfWrongWhy pf == CADR pf       -- was ==>
pfWrongRubble pf == CADDR pf       -- was ==>
pf0WrongRubble pf == pfParts pfWrongRubble pf


-- Add         := (Base: [Typed],  Addin: Expr)

pfAdd(pfbase, pfaddin,:addon) ==
    lhs := if addon
           then first addon
           else pfNothing()
    pfTree('Add, [pfbase, pfaddin,lhs])

pfAdd?(pf) == pfAbSynOp? (pf, 'Add)
pfAddBase pf == CADR pf       -- was ==>
pfAddAddin pf == CADDR pf       -- was ==>
pfAddAddon pf == CADDDR pf       -- was ==>
pf0AddBase pf == pfParts pfAddBase pf



-- DWhere      := (Context: [DeclPart], Expr: [DeclPart])

pfDWhere(pfcontext, pfexpr) == pfTree('DWhere, [pfcontext, pfexpr])
pfDWhere?(pf) == pfAbSynOp? (pf, 'DWhere)
pfDWhereContext pf == CADR pf       -- was ==>
pfDWhereExpr pf == CADDR pf       -- was ==>



-- With        := (Base: [Typed],  Within: [WithPart])

pfWith(pfbase, pfwithin,pfwithon) ==
         pfTree('With, [pfbase, pfwithin,pfwithon])
pfWith?(pf) == pfAbSynOp? (pf, 'With)
pfWithBase pf == CADR pf       -- was ==>
pfWithWithin pf == CADDR pf       -- was ==>
pfWithWithon pf == CADDDR pf       -- was ==>
pf0WithBase pf == pfParts pfWithBase pf
pf0WithWithin pf == pfParts pfWithWithin pf


-- WIf         := (Cond: Primary, Then: [WithPart], Else: [WithPart])

pfWIf(pfcond, pfthen, pfelse) == pfTree('WIf, [pfcond, pfthen, pfelse])
pfWIf?(pf) == pfAbSynOp? (pf, 'WIf)
pfWIfCond pf == CADR pf       -- was ==>
pfWIfThen pf == CADDR pf       -- was ==>
pfWIfElse pf == CADDDR pf       -- was ==>

-- WDeclare    := (Signature: Typed, Doc: ? Document)

pfWDeclare(pfsignature, pfdoc) == pfTree('WDeclare, [pfsignature, _
pfdoc])
pfWDeclare?(pf) == pfAbSynOp? (pf, 'WDeclare)
pfWDeclareSignature pf == CADR pf       -- was ==>
pfWDeclareDoc pf == CADDR pf       -- was ==>


-- Attribute   := (Expr: Primary)

pfAttribute(pfexpr) == pfTree('Attribute, [pfexpr])
pfAttribute?(pf) == pfAbSynOp? (pf, 'Attribute)
pfAttributeExpr pf == CADR pf       -- was ==>


-- Typed       := (Id: Id,    Type: ? Type)

pfTyped(pfid, pftype) == pfTree('Typed, [pfid, pftype])
pfTyped?(pf) == pfAbSynOp? (pf, 'Typed)
pfTypedId pf == CADR pf       -- was ==>
pfTypedType pf == CADDR pf       -- was ==>


-- Application := (Op:   Expr, Arg:    Expr)

pfApplication(pfop, pfarg) ==
        pfTree('Application, [pfop, pfarg])

pfApplication?(pf) == pfAbSynOp? (pf, 'Application)
pfApplicationOp pf == CADR pf       -- was ==>
pfApplicationArg pf == CADDR pf       -- was ==>


-- Tuple       := (Parts: [Expr])

pfTupleListOf(pfparts) == pfTuple pfListOf pfparts
pfTuple(pfparts) == pfTree('Tuple, [pfparts])
pfTuple?(pf) == pfAbSynOp? (pf, 'Tuple)
pfTupleParts pf == CADR pf       -- was ==>
pf0TupleParts pf == pfParts pfTupleParts pf


-- Tagged      := (Tag:  Expr, Expr:   Expr)

pfTagged(pftag, pfexpr) == pfTree('Tagged, [pftag, pfexpr])
pfTagged?(pf) == pfAbSynOp? (pf, 'Tagged)
pfTaggedTag pf == CADR pf       -- was ==>
pfTaggedExpr pf == CADDR pf       -- was ==>


-- Pretend     := (Expr: Expr, Type:   Type)

pfPretend(pfexpr, pftype) == pfTree('Pretend, [pfexpr, pftype])
pfPretend?(pf) == pfAbSynOp? (pf, 'Pretend)
pfPretendExpr pf == CADR pf       -- was ==>
pfPretendType pf == CADDR pf       -- was ==>


-- Restrict    := (Expr: Expr, Type:   Type)

pfRestrict(pfexpr, pftype) == pfTree('Restrict, [pfexpr, pftype])
pfRestrict?(pf) == pfAbSynOp? (pf, 'Restrict)
pfRestrictExpr pf == CADR pf       -- was ==>
pfRestrictType pf == CADDR pf       -- was ==>


-- Coerceto    := (Expr: Expr, Type:   Type)

pfCoerceto(pfexpr, pftype) == pfTree('Coerceto, [pfexpr, pftype])
pfCoerceto?(pf) == pfAbSynOp? (pf, 'Coerceto)
pfCoercetoExpr pf == CADR pf       -- was ==>
pfCoercetoType pf == CADDR pf       -- was ==>


-- Fromdom     := (What: Id,   Domain: Type)

pfFromdom(pfwhat, pfdomain) == pfTree('Fromdom, [pfwhat, pfdomain])
pfFromdom?(pf) == pfAbSynOp? (pf, 'Fromdom)
pfFromdomWhat pf == CADR pf       -- was ==>
pfFromdomDomain pf == CADDR pf       -- was ==>


-- Lambda     := (Args: [Typed], Rets: ? Type, Body: Expr)

pfLambda(pfargs, pfrets, pfbody) == pfTree('Lambda, [pfargs, pfrets, _
pfbody])
pfLambda?(pf) == pfAbSynOp? (pf, 'Lambda)
pfLambdaArgs pf == CADR pf       -- was ==>
pfLambdaRets pf == CADDR pf       -- was ==>
pfLambdaBody pf == CADDDR pf       -- was ==>
pf0LambdaArgs pf == pfParts pfLambdaArgs pf
pfFix pf== pfApplication(pfId "Y",pf)


-- MLambda     := (Args: [Id], Body: Expr)

pfMLambda(pfargs, pfbody) == pfTree('MLambda, [pfargs, pfbody])
pfMLambda?(pf) == pfAbSynOp? (pf, 'MLambda)
pfMLambdaArgs pf == CADR pf       -- was ==>
pfMLambdaBody pf == CADDR pf       -- was ==>
pf0MLambdaArgs pf == pfParts pfMLambdaArgs pf


-- Where       := (Context: [DeclPart], Expr: Expr)

pfWhere(pfcontext, pfexpr) == pfTree('Where, [pfcontext, pfexpr])
pfWhere?(pf) == pfAbSynOp? (pf, 'Where)
pfWhereContext pf == CADR pf       -- was ==>
pfWhereExpr pf == CADDR pf       -- was ==>
pf0WhereContext pf == pfParts pfWhereContext pf


-- If          := (Cond: Expr, Then: Expr, Else: ? Expr)

pfIf(pfcond, pfthen, pfelse) == pfTree('If, [pfcond, pfthen, pfelse])
pfIf?(pf) == pfAbSynOp? (pf, 'If)
pfIfCond pf == CADR pf       -- was ==>
pfIfThen pf == CADDR pf       -- was ==>
pfIfElse pf == CADDDR pf       -- was ==>


-- Sequence    := (Args: [Expr])

pfSequence(pfargs) == pfTree('Sequence, [pfargs])
pfSequence?(pf) == pfAbSynOp? (pf, 'Sequence)
pfSequenceArgs pf == CADR pf       -- was ==>
pf0SequenceArgs pf == pfParts pfSequenceArgs pf


-- Novalue     := (Expr: Expr)

pfNovalue(pfexpr) == pfTree('Novalue, [pfexpr])
pfNovalue?(pf) == pfAbSynOp? (pf, 'Novalue)
pfNovalueExpr pf == CADR pf       -- was ==>


-- Loop        := (Iterators: [Iterator])

pfLoop(pfiterators) == pfTree('Loop, [pfiterators])
pfLoop?(pf) == pfAbSynOp? (pf, 'Loop)
pfLoopIterators pf == CADR pf       -- was ==>
pf0LoopIterators pf == pfParts pfLoopIterators pf


-- Collect     := (Body: Expr, Iterators: [Iterator])

pfCollect(pfbody, pfiterators) == pfTree('Collect, [pfbody, _
pfiterators])
pfCollect?(pf) == pfAbSynOp? (pf, 'Collect)
pfCollectBody pf == CADR pf       -- was ==>
pfCollectIterators pf == CADDR pf       -- was ==>
pf0CollectIterators pf == pfParts pfCollectIterators pf


-- Forin       := (Lhs: [AssLhs], Whole: Expr)

pfForin(pflhs, pfwhole) == pfTree('Forin, [pflhs, pfwhole])
pfForin?(pf) == pfAbSynOp? (pf, 'Forin)
pfForinLhs pf == CADR pf       -- was ==>
pfForinWhole pf == CADDR pf       -- was ==>
pf0ForinLhs pf == pfParts pfForinLhs pf


-- While       := (Cond: Expr)

pfWhile(pfcond) == pfTree('While, [pfcond])
pfWhile?(pf) == pfAbSynOp? (pf, 'While)
pfWhileCond pf == CADR pf       -- was ==>


-- Until       := (Cond: Expr)

--pfUntil(pfcond) == pfTree('Until, [pfcond])
--pfUntil?(pf) == pfAbSynOp? (pf, 'Until)
--pfUntilCond pf == CADR pf       -- was ==>


-- Suchthat    := (Cond: Expr)

pfSuchthat(pfcond) == pfTree('Suchthat, [pfcond])
pfSuchthat?(pf) == pfAbSynOp? (pf, 'Suchthat)
pfSuchthatCond pf == CADR pf       -- was ==>


-- Do          := (Body: Expr)

pfDo(pfbody) == pfTree('Do, [pfbody])
pfDo?(pf) == pfAbSynOp? (pf, 'Do)
pfDoBody pf == CADR pf       -- was ==>


-- Iterate     := (From: ? Id)

pfIterate(pffrom) == pfTree('Iterate, [pffrom])
pfIterate?(pf) == pfAbSynOp? (pf, 'Iterate)
pfIterateFrom pf == CADR pf       -- was ==>


-- Break       := (From: ? Id)

pfBreak(pffrom) == pfTree('Break, [pffrom])
pfBreak?(pf) == pfAbSynOp? (pf, 'Break)
pfBreakFrom pf == CADR pf       -- was ==>


-- Return      := (Expr: ? Expr, From: ? Id)

pfReturn(pfexpr, pffrom) == pfTree('Return, [pfexpr, pffrom])
pfReturn?(pf) == pfAbSynOp? (pf, 'Return)
pfReturnExpr pf == CADR pf       -- was ==>
pfReturnFrom pf == CADDR pf       -- was ==>


-- Exit        := (Cond: ? Expr, Expr: ? Expr)

pfExit(pfcond, pfexpr) == pfTree('Exit, [pfcond, pfexpr])
pfExit?(pf) == pfAbSynOp? (pf, 'Exit)
pfExitCond pf == CADR pf       -- was ==>
pfExitExpr pf == CADDR pf       -- was ==>


-- Macro       := (Lhs:  Id,     Rhs: ExprorNot)

pfMacro(pflhs, pfrhs) == pfTree('Macro, [pflhs, pfrhs])
pfMacro?(pf) == pfAbSynOp? (pf, 'Macro)
pfMacroLhs pf == CADR pf       -- was ==>
pfMacroRhs pf == CADDR pf       -- was ==>


-- Definition  := (LhsItems:  [Typed], Rhs:  Expr)

pfDefinition(pflhsitems, pfrhs) == pfTree('Definition, [pflhsitems, pfrhs])
pfDefinition?(pf) == pfAbSynOp? (pf, 'Definition)
pfDefinitionLhsItems pf == CADR pf       -- was ==>
pfDefinitionRhs pf == CADDR pf       -- was ==>
pf0DefinitionLhsItems pf == pfParts pfDefinitionLhsItems pf

pfRule(pflhsitems, pfrhs) == pfTree('Rule, [pflhsitems, _
pfrhs])
pfRule?(pf) == pfAbSynOp? (pf, 'Rule)
pfRuleLhsItems pf == CADR pf       -- was ==>
pfRuleRhs pf == CADDR pf       -- was ==>

-- ComDefinition := (Doc:Document,Def:Definition)

pfComDefinition(pfdoc, pfdef) == pfTree('ComDefinition, [pfdoc, pfdef] )
pfComDefinition?(pf) == pfAbSynOp? (pf, 'ComDefinition)
pfComDefinitionDoc pf == CADR pf       -- was ==>
pfComDefinitionDef pf == CADDR pf       -- was ==>


-- DefinitionSequence    := (Args: [DeclPart])

pfDefinitionSequenceArgs pf == CADR pf       -- was ==>

-- Export      := (Def:   Definition)

pfExportDef pf == CADR pf       -- was ==>

-- Assign      := (LhsItems:  [AssLhs], Rhs:  Expr)

pfAssign(pflhsitems, pfrhs) == pfTree('Assign, [pflhsitems, pfrhs])
pfAssign?(pf) == pfAbSynOp? (pf, 'Assign)
pfAssignLhsItems pf == CADR pf       -- was ==>
pfAssignRhs pf == CADDR pf       -- was ==>
pf0AssignLhsItems pf == pfParts pfAssignLhsItems pf


-- Typing      := (Items: [Typed])

pfTyping(pfitems) == pfTree('Typing, [pfitems])
pfTyping?(pf) == pfAbSynOp? (pf, 'Typing)
pfTypingItems pf == CADR pf       -- was ==>
pf0TypingItems pf == pfParts pfTypingItems pf


-- Export       := (Items: [Typed])

pfExport(pfitems) == pfTree('Export, [pfitems])
pfExport?(pf) == pfAbSynOp? (pf, 'Export)
pfExportItems pf == CADR pf       -- was ==>
pf0ExportItems pf == pfParts pfExportItems pf


-- Local       := (Items: [Typed])

pfLocal(pfitems) == pfTree('Local, [pfitems])
pfLocal?(pf) == pfAbSynOp? (pf, 'Local)
pfLocalItems pf == CADR pf       -- was ==>
pf0LocalItems pf == pfParts pfLocalItems pf

-- Free        := (Items: [Typed])

pfFree(pfitems) == pfTree('Free, [pfitems])
pfFree?(pf) == pfAbSynOp? (pf, 'Free)
pfFreeItems pf == CADR pf       -- was ==>
pf0FreeItems pf == pfParts pfFreeItems pf


-- Import      := (Items: [QualType])

pfImport(pfitems) == pfTree('Import, [pfitems])
pfImport?(pf) == pfAbSynOp? (pf, 'Import)
pfImportItems pf == CADR pf       -- was ==>
pf0ImportItems pf == pfParts pfImportItems pf


-- Inline      := (Items: [QualType])

pfInline(pfitems) == pfTree('Inline, [pfitems])
pfInline?(pf) == pfAbSynOp? (pf, 'Inline)
pfInlineItems pf == CADR pf       -- was ==>

-- QualType    := (Type: Type, Qual: ? Type)

pfQualType(pftype, pfqual) == pfTree('QualType, [pftype, pfqual])
pfQualType?(pf) == pfAbSynOp? (pf, 'QualType)
pfQualTypeType pf == CADR pf       -- was ==>
pfQualTypeQual pf == CADDR pf       -- was ==>

pfSuch(x,y)== pfInfApplication(pfId "|",x,y)

pfTaggedToTyped x==
  rt:=if pfTagged? x then pfTaggedExpr x else pfNothing()
  form:= if pfTagged? x then pfTaggedTag x else x
  not pfId? form =>
      a:=pfId GENSYM()
      pfTyped(pfSuch(a,
           pfInfApplication (pfId "=", a,form)),rt)
  pfTyped(form,rt)

pfTaggedToTyped1 x==
    pfCollect1? x => pfCollectVariable1 x
    pfDefinition? x => pfTyped(x,pfNothing())
    pfTaggedToTyped x

pfCollectVariable1 x==
      a := pfApplicationArg x
      var:=first pf0TupleParts a
      id:=pfTaggedToTyped var
      pfTyped(pfSuch(pfTypedId id,CADR pf0TupleParts a),
              pfTypedType id)

pfPushBody(t,args,body)==
        if null args
        then  body
        else if null rest args
              then  pfLambda(first args,t,body)
              else
                 pfLambda(first args,pfNothing(),
                     pfPushBody(t,rest args,body))

pfCheckItOut x ==
  rt:=if pfTagged? x then pfTaggedExpr x else pfNothing()
  form:= if pfTagged? x then pfTaggedTag x else x
  pfId? form => [pfListOf [pfTyped(form,rt)],nil,rt]
  pfCollect1? form =>
                [pfListOf [pfCollectVariable1 form],nil,rt]
  pfTuple? form =>
       [pfListOf [pfTaggedToTyped i for i in pf0TupleParts form],nil,rt]
  pfDefinition? form =>
       [pfListOf [pfTyped(form,pfNothing())],nil,rt]
  pfApplication? form =>
          ls:=pfFlattenApp form
          op:= pfTaggedToTyped1 first ls
          args:=[pfTransformArg i for i in rest ls]
          [pfListOf [op],args,rt]
  npTrapForm form

pfCollect1? x==
        pfApplication? x =>
              a:=pfApplicationOp x
              pfId? a => pfIdSymbol a = "|"
              false
        false

pfTransformArg  args==
          argl:= if pfTuple? args then pf0TupleParts args else [args]
          pfListOf [pfTaggedToTyped1 i for i in argl]


pfCheckMacroOut form ==
  pfId? form => [form,nil]
  pfApplication? form =>
          ls:=pfFlattenApp form
          op:= pfCheckId first ls
          args:=[pfCheckArg i for i in rest ls]
          [op,args]
  npTrapForm form

pfCheckArg args==
          argl:= if pfTuple? args then pf0TupleParts args else [args]
          pfListOf [pfCheckId i for i in argl]

pfCheckId form==   if not pfId? form then npTrapForm(form) else form

pfPushMacroBody(args,body)==
    null args =>   body
    pfMLambda(first args,pfPushMacroBody(rest args,body))

pfFlattenApp x==
   pfApplication? x=>
             pfCollect1? x =>[ x ]
             append (pfFlattenApp pfApplicationOp x,
                        pfFlattenApp pfApplicationArg x)
   [x]


--% Utility operations on Abstract Syntax Trees (former ptrop.boot)

-- An S-expression which people can read.
pfSexpr pform ==
    strip pform where
         strip pform ==
            pfId? pform       => pfIdSymbol pform
            pfLiteral?  pform => pfLiteralString pform
            pfLeaf? pform     => tokPart pform

            pfApplication? pform =>
                args :=
                    a := pfApplicationArg pform
                    if pfTuple? a then pf0TupleParts a else [a]
                [strip p for p in cons(pfApplicationOp pform, args)]

            cons(pfAbSynOp pform, [strip p for p in pfParts pform])

pfCopyWithPos( pform , pos ) ==
    pfLeaf? pform =>         pfLeaf( pfAbSynOp pform , tokPart pform , pos )
    pfTree( pfAbSynOp pform , [ pfCopyWithPos( p , pos ) for p in pfParts pform ] )

pfMapParts(f, pform) ==
    pfLeaf? pform => pform
    parts0 := pfParts pform
    parts1 := [FUNCALL(f, p) for p in parts0]
    -- Return the original if no changes.
    same := true
    for p0 in parts0 for p1 in parts1 while same repeat same := EQ(p0,p1)
    same => pform
    pfTree(pfAbSynOp pform, parts1)


pf0ApplicationArgs pform ==
    arg := pfApplicationArg pform
    pf0FlattenSyntacticTuple arg

pf0FlattenSyntacticTuple pform ==
    not pfTuple? pform => [pform]
    [:pf0FlattenSyntacticTuple p for p in pf0TupleParts pform]
