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

-- This file contains the constructors for the domains that cannot
-- be written in ScratchpadII yet.  They are not cached because they
-- are very cheap to instantiate.
-- SMW and SCM July 86

DEFPARAMETER($noCategoryDomains, '(Mode))
DEFPARAMETER($nonLisplibDomains,
  APPEND($Primitives,$noCategoryDomains))

--% Record
--  Want to eventually have the elts and setelts.
--  Record is a macro in BUILDOM LISP. It takes out the colons.

isRecord type == type is ['Record,:.]

Record0 args ==
    dom := GETREFV 11
    -- JHD added an extra slot to cache EQUAL methods
    dom.0 := ['Record, :[['_:, first a, devaluate rest a] for a in args]]
    dom.1 :=
           [function lookupInTable,dom,
               [['_=,[[['Boolean],'_$,'_$],:6]],
                ['_~_=,[[['Boolean],'_$,'_$],:10]],
                 ['coerce, [[$OutputForm, '_$], :7]]]]
    dom.2 := NIL
    dom.3 := ['RecordCategory,:QCDR dom.0]
    dom.4 :=
          [[ '(SetCategory) ], [ '(BasicType),
             '(CoercibleTo (OutputForm)), '(SetCategory) ]]
    dom.5 := [rest a for a in args]
    dom.6 := [function RecordEqual, :dom]
    dom.7 := [function RecordPrint, :dom]
    dom.8 := [function Undef, :dom]
  -- following is cache for equality functions
    dom.9 := if (n:= LENGTH args) <= 2
              then [NIL,:NIL]
              else GETREFV n
    dom.10 := [function RecordUnEqual, :dom]
    dom

RecordEqual(x,y,dom) ==
  PAIRP x =>
    b:=
       SPADCALL(first x, first y, first(dom.9) or
                           first RPLACA(dom.9, findEqualFun(dom.5.0)))
    NULL rest(dom.5) => b
    b and
       SPADCALL(rest x, rest y, rest(dom.9) or
                           rest RPLACD(dom.9, findEqualFun(dom.5.1)))
  VECP x =>
    equalfuns := dom.9
    and/[SPADCALL(x.i,y.i,equalfuns.i or (equalfuns.i:=findEqualFun(fdom)))
         for i in 0.. for fdom in dom.5]
  error '"Bug: Silly record representation"

RecordUnEqual(x,y,dom) == not(RecordEqual(x,y,dom))

RecordPrint(x,dom) == coerceRe2E(x,dom.3)

coerceVal2E(x,m) ==
   -- first catch "failed" etc.
   STRINGP m and (x = m) => STRCONC('"_"", x, '"_"")
   objValUnwrap coerceByFunction(objNewWrap(x, m), $OutputForm)

findEqualFun(dom) ==
  compiledLookup('_=,[$Boolean,'$,'$],dom)

coerceRe2E(x,source) ==
  n := #rest(source)
  n = 1 =>
    ['construct,
     ['_=, source.1.1, coerceVal2E(first x, source.1.2)] ]
  n = 2 =>
    ['construct,
     ['_=, source.1.1, coerceVal2E(first x, source.1.2)], _
     ['_=, source.2.1, coerceVal2E(rest x, source.2.2)] ]
  VECP x =>
    ['construct,
     :[['_=,tag,coerceVal2E(x.i, fdom)]
       for i in 0.. for [.,tag,fdom] in rest source]]
  error '"Bug: ridiculous record representation"


--% Union
--  Want to eventually have the coerce to and from branch types.

Union(:args) ==
    dom := GETREFV 10
    dom.0 := ['Union, :[(if a is ['_:,tag,domval] then ['_:,tag,devaluate domval]
                          else devaluate a) for a in args]]
    dom.1 :=
            [function lookupInTable,dom,
               [['_=,[[['Boolean],'_$,'_$],:6]],
                ['_~_=, [[['Boolean],'_$,'_$],:9]],
                 ['coerce,[[$OutputForm, '_$],:7]]]]
    dom.2 := NIL
    dom.3 :=
      '(SetCategory)
    dom.4 :=
          [[ '(SetCategory) ],[ '(BasicType),
             '(CoercibleTo (OutputForm)),  '(SetCategory) ]]
    dom.5 := args
    dom.6 := [function UnionEqual, :dom]
    dom.7 := [function UnionPrint, :dom]
    dom.8 := [function Undef, :dom]
    dom.9 := [function UnionUnEqual, :dom]
    dom

UnionEqual(x, y, dom) ==
  ['Union,:branches] := dom.0
  predlist := mkPredList branches
  same := false
  res := false
  for b in stripUnionTags branches for p in predlist while not same repeat
    p is ["EQCAR", "#1", n] =>
        EQCAR(x, n) and EQCAR(y, n) =>
            same := true
            STRINGP b => res := (x = y)
            x := rest x
            y := rest y
            res := SPADCALL(x, y, findEqualFun(evalDomain b))
    typeFun := COERCE(['LAMBDA, '(_#1), p], 'FUNCTION)
    FUNCALL(typeFun,x) and FUNCALL(typeFun,y) =>
        same := true
        STRINGP b => res := (x = y)
        res := SPADCALL(x, y, findEqualFun(evalDomain b))
  res

UnionUnEqual(x, y, dom) == not(UnionEqual(x, y, dom))

UnionPrint(x, dom) == coerceUn2E(x, dom.0)

coerceUn2E(x,source) ==
  ['Union,:branches] := source
  predlist := mkPredList branches
  found := false
  for b in stripUnionTags branches for p in predlist while not(found) repeat
      found :=
          p is ["EQCAR", "#1", n] => EQCAR(x, n)
          typeFun := COERCE(['LAMBDA, '(_#1), p], 'FUNCTION)
          FUNCALL(typeFun,x)
      if found then
          if p is ['EQCAR, :.] then x := rest x
          res := coerceVal2E(x,b)
  not(found) =>
    error '"Union bug: Cannot find appropriate branch for coerce to E"
  res

mkPredList listOfEntries ==
     [['EQCAR,"#1",i] for arg in listOfEntries for i in 0..]

--% Mapping
--  Want to eventually have elt: ($, args) -> target

Mapping(:args) ==
    dom := GETREFV 10
    dom.0 := ['Mapping, :[devaluate a for a in args]]
    dom.1 :=
            [function lookupInTable,dom,
               [['_=,[[['Boolean],'_$,'_$],:6]],
                 ['coerce,[[$OutputForm, '_$],:7]]]]
    dom.2 := NIL
    dom.3 :=
      '(SetCategory)
    dom.4 :=
          [[ '(SetCategory) ],[ '(BasicType),
             '(CoercibleTo (OutputForm)), '(SetCategory) ]]
    dom.5 := args
    dom.6 := [function MappingEqual, :dom]
    dom.7 := [function MappingPrint, :dom]
    dom.8 := [function Undef, :dom]
    dom.9 := [function MappingUnEqual, :dom]
    dom

MappingEqual(x, y, dom) == EQ(x,y)

MappingUnEqual(x, y, dom) == not(EQ(x,y))

MappingPrint(x, dom) == coerceMap2E(x)

coerceMap2E(x) ==
  -- nrlib domain
  ARRAYP rest x => ['theMap, BPINAME first x,
    if $testingSystem then 0 else REMAINDER(HASHEQ rest x, 1000)]
  -- aldor
  ['theMap, BPINAME first x]

--% Enumeration
-- Enumeration is a Lisp macro since it wants unevaluated arguments
-- Enumeration0 below is a function, so it needs explicit quotes for
-- arguments

Enumeration0(:args) ==
    dom := GETREFV 10
    -- JHD added an extra slot to cache EQUAL methods
    dom.0 := ['Enumeration, :args]
    dom.1 :=
           [function lookupInTable,dom,
               [['_=,[[['Boolean],'_$,'_$],:6]],
                 ['coerce,[[$OutputForm, '_$],:7], [['_$, $Symbol], :8]]
                         ]]
    dom.2 := NIL
    dom.3 := ['EnumerationCategory,:QCDR dom.0]
    dom.4 :=
          [[ '(SetCategory) ], [ '(BasicType),
             '(CoercibleTo (OutputForm)), '(SetCategory) ]]
    dom.5 := args
    dom.6 := [function EnumEqual, :dom]
    dom.7 := [function EnumPrint, :dom]
    dom.8 := [function createEnum, :dom]
    dom.9 := [function EnumUnEqual, :dom]
    dom

EnumEqual(e1,e2,dom) == e1=e2
EnumUnEqual(e1,e2,dom) == not(EnumEqual(e1,e2,dom))
EnumPrint(enum, dom) == dom.5.enum
createEnum(sym, dom) ==
  args := dom.5
  val := -1
  for v in args for i in 0.. repeat
     sym=v => return(val:=i)
  val<0 => error ["Cannot coerce",sym,"to",["Enumeration",:args]]
  val

--% INSTANTIATORS

RecordCategory(:x) == constructorCategory ['Record,:x]

EnumerationCategory(:x) == constructorCategory ["Enumeration",:x]

UnionCategory(:x) == constructorCategory ["Union",:x]


constructorCategory (title is [op,:.]) ==
  constructorFunction:= GET(op, "makeFunctionList") or
              systemErrorHere '"constructorCategory"
  [funlist,.]:= FUNCALL(constructorFunction,"$",title,$CategoryFrame)
  oplist:= [[[a,b],true,c] for [a,b,c] in funlist]
  cat:=
    JoinInner([SetCategory(), mkCategory(oplist, nil, nil, nil)],
      $EmptyEnvironment)
  cat.(0):= title
  cat

--mkMappingFunList(nam,mapForm,e) == [[],e]
mkMappingFunList(nam,mapForm,e) ==
  dc := GENSYM()
  sigFunAlist:=
    [['_=,[['Boolean],nam ,nam],['ELT,dc,6]],
     ['_~_=, [['Boolean], nam, nam], ['ELT, dc, 9]],
       ['coerce, [$OutputForm, nam], ['ELT, dc, 7]]]
  [substitute(nam,dc,substitute("$",'Rep,sigFunAlist)),e]

mkRecordFunList(nam,['Record,:Alist],e) ==
  len:= #Alist

--  for (.,a,.) in Alist do
--    if getmode(a,e) then MOAN("Symbol: ",a,
--        " must not be both a variable and literal")
--    e:= put(a,"isLiteral","true",e)
  dc := GENSYM()
  sigFunAlist:=
     --:((a,(A,nam),('XLAM,("$1","$2"),('RECORDELT,"$1",i,len)))
     --       for i in 0..,(.,a,A) in Alist),

    [['construct,[nam,:[A for [.,a,A] in Alist]],'mkRecord],
     ['_=, [['Boolean], nam, nam], ['ELT, dc, 6]],
      ['_~_=, [['Boolean], nam, nam], ['ELT, dc, 10]],
       ['coerce, [$OutputForm, nam], ['ELT, dc, 7]],:
        [['elt,[A,nam,PNAME a],['XLAM,["$1","$2"],['RECORDELT,"$1",i,len]]]
            for i in 0.. for [.,a,A] in Alist],:
          [["setelt!", [A, nam, PNAME a, A], ['XLAM, ["$1", "$2", "$3"],
            ['SETRECORDELT,"$1",i, len,"$3"]]]
              for i in 0.. for [.,a,A] in Alist],:
                [['copy,[nam,nam],['XLAM,["$1"],['RECORDCOPY,
                  "$1",len]]]]]
  [substitute(nam,dc,substitute("$",'Rep,sigFunAlist)),e]

mkNewUnionFunList(name,form is ['Union,:listOfEntries],e) ==
  dc := name
  if name = 'Rep then name := '$
  --2. create coercions from subtypes to subUnion
  cList:=
    [['_=,[['Boolean],name ,name],['ELT,dc,6]],
     ['_~_=, [['Boolean], name, name], ['ELT, dc, 9]],
     ['coerce, [$OutputForm, name], ['ELT, dc, 7]],:
       ("append"/
        [[['construct,[name,type],['XLAM,["#1"],['CONS,i,"#1"]]],
          ['elt,[type,name,tag],cdownFun],
            ["case", ['(Boolean), name, tag],
               ['XLAM,["#1"],['QEQCAR,"#1",i]]]]
                 for [.,tag,type] in listOfEntries for i in 0..])] where
                   cdownFun() ==
                    gg:=GENSYM()
                    $InteractiveMode =>
                      ['XLAM,["#1"],['PROG1,['QCDR,"#1"],
                        ['check_union2, ['QEQCAR, "#1", i], type, form, "#1"]]]
                    ['XLAM,["#1"],['PROG2,['LET,gg,"#1"],['QCDR,gg],
                      ['check_union2, ['QEQCAR, gg, i], type, form, gg]]]
  [cList,e]

mkEnumerationFunList(nam,['Enumeration,:SL],e) ==
  len:= #SL
  dc := nam
  cList :=
    [nil,
      ['_=,[['Boolean],nam ,nam],['ELT,dc,6]],
       ['_~_=, [['Boolean], nam, nam], ['ELT, dc, 9]],
        ['_^_=,[['Boolean],nam ,nam],['ELT,dc,7]],
          ['coerce,[nam, ['Symbol]], ['ELT, dc, 8]],
            ['coerce,[['OutputForm],nam],['ELT,dc, 9]]]
  [substitute(nam, dc, cList),e]

mkUnionFunList(op,form is ['Union,:listOfEntries],e) ==
  first listOfEntries is [":",.,.] => mkNewUnionFunList(op,form,e)
  --1. create representations of subtypes
  predList:= mkPredList listOfEntries
  g:=GENSYM()
  --2. create coercions from subtypes to subUnion
  cList:=
   [['_=,[['Boolean],g ,g],['ELT,op,6]],
    ['_~_=, [['Boolean], g, g], ['ELT,op,9]],
    ['coerce, [$OutputForm, g], ['ELT, op, 7]],:
     ("append"/
      [[['autoCoerce,[g,t],upFun],
        ['coerce,[t,g],cdownFun],
        ['autoCoerce,[t,g],downFun], --this should be removed eventually
        ["case", ['(Boolean), g, t], typeFun]]
          for p in predList for t in listOfEntries])] where
             upFun() ==
               p is ['EQCAR,x,n] => ['XLAM,["#1"],['CONS,n,"#1"]]
               ['XLAM,["#1"],"#1"]
             cdownFun() ==
               gg:=GENSYM()
               if p is ['EQCAR,x,n] then
                  ref:=['QCDR,gg]
                  q:= ['QEQCAR, gg, n]
               else
                  ref:=gg
                  q:= substitute(gg,"#1",p)
               ['XLAM,["#1"],['PROG2,['LET,gg,"#1"],ref,
                    ['check_union2, q, t, form, gg]]]
             downFun() ==
                p is ['EQCAR,x,.] =>
                  ['XLAM,["#1"],['QCDR,"#1"]]
                ['XLAM,["#1"],"#1"]
             typeFun() ==
                p is ['EQCAR,x,n] =>
                  ['XLAM,["#1"],['QEQCAR,x,n]]
                ['XLAM,["#1"],p]
  op:=
    op='Rep => '$
    op
  cList:= substitute(op,g,cList)
  [cList,e]
