-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- Copyright (c) 2011-2012, Ralf Hemmecke <ralf@hemmecke.de>
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

$stripTypes := false

-- Although default operations might be implemented conditionally, the
-- internal data structure does not store the condition. The only
-- reasonable assumption one can make about the types of the |#i| arguments
-- is to pretend that they are such as required by the respective
-- constructor.
-- $pretendFlag will be set to true, if default operations are detected.
-- TODO: This is a reasonable assumption, but wrong in general.
$pretendFlag := false

-- The variables $modemapArgs and $augmentedArgs and the code surrounding it
-- are due to a weakness in the Aldor compiler. This code can be removed
-- if this "bug" is fixed in the Aldor compiler.
-- See the aldor-l mailinglist for a bug demonstrating a weakness
-- of the Aldor 1.1.0 compiler.
-- http://aldor.org/pipermail/aldor-l_aldor.org/2011-December/001328.html
-- http://groups.google.com/group/fricas-devel/browse_thread/thread/c7d1c908a07758d2

-- The main idea of the algorithm is as follows. Whenever a "if A has T ..."
-- is detected, the type T is added for the argument A in $augmentedArgs.
-- Inside the "then" block the type of A is augmented to whatever is stored
-- in $augmentedArgs. However, in a block like
---- if S has SetCategory then
----    if S has Evalable S then Evalable S
-- when S is a parameter in the original modemap and has type T, then
-- we must output something like:
---- if S has SetCategory then
----    if S has Evalable(S pretend Join(SetCategory, T)) then
----        Evalable(S pretend Join(
----                                Evalable(S pretend Join(SetCategory, T)),
----                                Join(SetCategory, T)))
-- Note in particular that inside "S pretend ..." we must remove one
-- layer from $augmentedArgs since otherwise we get infinite recursion.

-- Store the constructor modemap.
-- $modemapArgs will be an assoc list of the form
-- ((|#1| . T1) (|#2| . T2) ...)
-- where Ti is the actual (untranslated) type of the i-th argument
-- as extracted from modemap in the function modemapToAx.
$modemapArgs := nil

-- Whenever we enter something like
---- if |#i| has C then ...
-- we try to extract the type T of |#i| from $augmentedArgs or $modemapArgs.
-- (in this order).
-- Then augment T with C, i.e. form Join(C, T) and store that type in
-- the association list $augmentedArgs.
-- A special case is the situation "if % has C then ...". Then C will be
-- ignored (which is actually wrong) and in the "then" block we
-- pretend % to whatever is required for an argument of a type constructor.
-- This is somewhat like in the code used for $pretendFlag=true.

-- $augmentedArgs is a list with entries of the form
-- (|#i| . v)
-- where v is of the form
-- (pred_n pred_n-1 ... pred1 cat)
-- cat -- is the type of |#i| as given in the argument list of the constructor
-- in question.
-- pred_k -- is something of the form ('has name type) which comes from the
-- surrounding "if A has type then ..." and A corresponds to |#i|.
-- With each "if" condition, $augmentedArgs grows for this particular argument.
$augmentedArgs := nil

$defaultFlag := false
$baseForms := nil
$literals  := nil

spad2AxTranslatorAutoloadOnceTrigger any == true


$extendedDomains := nil

setExtendedDomains(l) ==
        $extendedDomains := l

--rhx: Function seems to be unused.
makeAxFile(filename, constructors) ==
  axForm := makeAxExportForm(filename, constructors)
  st := MAKE_-OUTSTREAM(filename)
  PPRINT(axForm,st)
  CLOSE st

makeAxExportForm(filename, constructors) ==
  $defaultFlag : local := false
  $literals := []
  -- Note that Tuple, Exit, Type are language defined idenifiers
  -- in Aldor.
  axForms :=
     [modemapToAx(modemap) for cname in constructors |
            (modemap:=GETDATABASE(cname,'CONSTRUCTORMODEMAP)) and
              (not (cname in '(Tuple Exit Type))) and
                not isDefaultPackageName cname]
  if $baseForms then
     axForms := [:$baseForms, :axForms]
  -- If the category has a default definition then $defaultFlag will be true.
  -- That is used to give dummy definitions for functions inside the
  -- default body.
  if $defaultFlag then
     axForms :=
        [['Foreign, ['Declare, 'dummyDefault, 'Exit], 'Lisp], :axForms]
  axForms := APPEND(axDoLiterals(), axForms)
  axForm := ['Sequence, _
               ['Import, [], 'AxiomLib], ['Import, [], 'Boolean], :axForms]
  axForm


stripType type ==
  $stripTypes =>
     categoryForm? type => 'Type
     type
  type

modemapToAx(modemap) ==
  modemap is [[consform, target,:argtypes],.]
  consform is [constructor,:args]

  -- Category forms show |t#i| instead of |#i|. In atypes |t#i| is replaced
  -- by |#i|.
  atypes := if categoryForm? consform then
      SUBLISLIS($FormalMapVariableList, $TriangleVariableList, argtypes)
    else
      argtypes
  $modemapArgs : local := [cons(a,t) for a in args for t in atypes]

  argdecls:=['Comma, : [axFormatDecl(a, stripType t) for a in args for t in argtypes]]
  resultType :=  axFormatType stripType target
  categoryForm? constructor =>
      categoryInfo := GETDATABASE(constructor,'CONSTRUCTORCATEGORY)
      categoryInfo := SUBLISLIS($FormalMapVariableList, $TriangleVariableList,
                       categoryInfo)
      NULL args =>
          ['Define,['Declare, constructor,'Category],
               addDefaults(constructor, axFormatType categoryInfo)]
      ['Define,
          ['Declare, constructor, ['Apply, "->", optcomma argdecls, 'Category]],
           ['Lambda, argdecls, 'Category,
             ['Label, constructor,
               addDefaults(constructor, axFormatType categoryInfo)]]]
  constructor in $extendedDomains =>
     NULL args =>
        ['Extend, ['Define, ['Declare, constructor, resultType],
            ['Add, ['PretendTo, ['Add, [], []], resultType], []]]]
     conscat := INTERN(STRCONC(SYMBOL_-NAME(constructor), "ExtendCategory"),"BOOT")
     rtype := ['Apply, conscat, :args]
--     if resultType is ['With,a,b] then
--        if not(b is ['Sequence,:withseq]) then withseq := [b]
--        cosigs := rest GETDATABASE(constructor, 'COSIG)
--        exportargs := [['Export, [], arg, []] for arg in args for p in cosigs | p]
--        resultType := ['With,a,['Sequence,:APPEND(exportargs, withseq)]]
     consdef := ['Define,
        ['Declare, conscat, ['Apply, "->", optcomma argdecls, 'Category]],
          ['Lambda, argdecls, 'Category, ['Label, conscat, resultType]]]
     ['Sequence, consdef,
      ['Extend, ['Define,
        ['Declare, constructor, ['Apply, "->", optcomma argdecls, rtype]],
          ['Lambda, argdecls, rtype,
            ['Label, constructor,
                ['Add, ['PretendTo, ['Add, [], []], rtype], []]]]]]]
  NULL args =>
     ['Export, ['Declare, constructor, resultType],[],[]]
--  if resultType is ['With,a,b] then
--     if not(b is ['Sequence,:withseq]) then withseq := [b]
--     cosigs := rest GETDATABASE(constructor, 'COSIG)
--     exportargs := [['Export, [], arg, []] for arg in args for p in cosigs | p]
--     resultType := ['With,a,['Sequence,:APPEND(exportargs, withseq)]]
  ['Export, ['Declare, constructor, ['Apply, "->", optcomma argdecls, resultType]],[],[]]

optcomma [op,:args] ==
   # args = 1 => first args
   [op,:args]

axFormatDecl(sym, type) ==
   if sym = '$ then sym := '%
   opOf type in '(StreamAggregate FiniteLinearAggregate) =>
        ['Declare, sym, 'Type]
   ['Declare, sym, axFormatType type]

makeTypeSequence l ==
   ['Sequence,: delete('Type, l)]

axFormatAttrib(typeform) ==
  atom typeform => typeform
  axFormatType typeform

axFormatType(typeform) ==
  atom typeform =>
     typeform = '$ => '%
     STRINGP typeform =>
        ['Apply,'Enumeration, INTERN typeform]
     INTEGERP typeform =>
       -- need to test for PositiveInteger vs Integer
        axAddLiteral('integer, 'PositiveInteger, 'Literal)
        ['RestrictTo, ['LitInteger, STRINGIMAGE typeform ], 'PositiveInteger]
     FLOATP typeform => ['LitFloat, STRINGIMAGE typeform]
     MEMQ(typeform,$TriangleVariableList) =>
        SUBLISLIS($FormalMapVariableList, $TriangleVariableList, typeform)
     MEMQ(typeform, $FormalMapVariableList) => typeform
     axAddLiteral('string, 'Symbol, 'Literal)
     ['RestrictTo, ['LitString, PNAME typeform], 'Symbol]
  typeform is ['construct,: args] =>
      axAddLiteral('bracket, ['Apply, 'List, 'Symbol], [ 'Apply, 'Tuple, 'Symbol])
      axAddLiteral('string, 'Symbol, 'Literal)
      ['RestrictTo, ['Apply, 'bracket,
                        :[axFormatType a for a in args]],
                          ['Apply, 'List, 'Symbol] ]
  typeform is [op] =>
    op = '$ => '%
    op = 'Void => ['Comma]
    op
  typeform is ['local, val] => axFormatType val
  typeform is ['QUOTE, val] => axFormatType val
  typeform is ['Join,:cats,lastcat] =>
      lastcat is ['CATEGORY,type,:ops] =>
         ['With, [],
            makeTypeSequence(
               APPEND([axFormatType c for c in cats],
                        [axFormatOp op for op in ops]))]
      ['With, [], makeTypeSequence([axFormatType c for c in rest typeform])]
  typeform is ['CATEGORY, type, :ops] =>
      ['With, [],  axFormatOpList ops]
  typeform is ['Mapping, target, :argtypes] =>
      ['Apply, "->",
               ['Comma, :[axFormatType t for t in argtypes]],
                axFormatType target]
  typeform is ['_:, name, type] => axFormatDecl(name,type)
  typeform is ['Union, :args] =>
      first args is ['_:,.,.] =>
         ['Apply, 'Union, :[axFormatType a for a in args]]
      taglist := []
      valueCount := 0
      for x in args repeat
          tag :=
            STRINGP x => INTERN x
            x is ['QUOTE,val] and STRINGP val => INTERN val
            valueCount := valueCount + 1
            INTERNL("value", STRINGIMAGE valueCount)
          taglist := [tag ,: taglist]
      ['Apply, 'Union, :[axFormatDecl(name,type) for name in reverse taglist
                                for type in args]]
  typeform is ['Dictionary,['Record,:args]] =>
      ['Apply, 'Dictionary,
          ['PretendTo, axFormatType CADR typeform, 'SetCategory]]
  typeform is ['FileCategory,xx,['Record,:args]] =>
      ['Apply, 'FileCategory, axFormatType xx,
          ['PretendTo, axFormatType CADDR typeform, 'SetCategory]]
  typeform is [op,:args] =>
      $pretendFlag and constructor? op and
        GETDATABASE(op,'CONSTRUCTORMODEMAP) is [[.,target,:argtypes],.] =>
          ['Apply, op, :[pretendTo(a, t) for a in args for t in argtypes]]
      -- $augmentedArgs is non-empty if we are inside a "if A has T then ..."
      -- block. In this case we must augment the type of A by T.
      -- In nearly all cases t is ignored, but is needed for %.
      not(null $augmentedArgs) and constructor? op and
        GETDATABASE(op,'CONSTRUCTORMODEMAP) is [[.,target,:argtypes],.] =>
          ['Apply, op, :[augmentTo(a, t) for a in args for t in argtypes]]
      MEMQ(op, '(SquareMatrix SquareMatrixCategory DirectProduct
         DirectProductCategory RadixExpansion)) and
            GETDATABASE(op,'CONSTRUCTORMODEMAP) is [[.,target,arg1type,:restargs],.] =>
               ['Apply, op,
                  ['PretendTo, axFormatType first args, axFormatType arg1type],
                     :[axFormatType a for a in rest args]]
      ['Apply, op, :[axFormatType a for a in args]]
  error "unknown entry type"

pretendTo(a, t) == ['PretendTo, axFormatType a, axFormatType t]

-- Whenever "a" appears as a key in $augmentedArgs, issue code that
-- consists of a |PretendTo| with the augmented type for the argument "a".
-- We cannot augment $ (= %) so we treat this case by the old method.
-- We could do a bit better, because we actually store some augmented type
-- information for % in $augmentedArgs. But not yet.
augmentTo(a, t) ==
  a = '$ => pretendTo(a, t)
  ax := axFormatType a -- a looks like |#i|
  not(null(kv:=ASSOC(a,$augmentedArgs))) =>
      ['PretendTo, ax, formatAugmentedType(rest kv, a, $augmentedArgs)]
  not(null(kv := ASSOC(a, $modemapArgs))) =>
      ['PretendTo, ax, axFormatType rest kv]
  ax

-- v the type to format (the form of v is as in $augmentedArgs, see above)
-- a the argument of form |#i| to which v corresponds (for recursion)
-- augargs temporary value of $augmentedArgs (it shrinks while doing
--   recursive calls)
-- The function is similar to axFormatType, but makes sure that arguments
-- of the form |#i| are pretended to be of an respective augmented type.
formatAugmentedType(v, a, augargs) ==
  $augmentedArgs:local := deleteFirstPred(a, augargs)
  axFormattedPred := axFormatPred first v
  axFormattedPred is ['Test, ['Has, arg, augtype]]
  c := cdr v
  null cdr c => -- the last argument is 'ignore or a type from $modemapArgs
    first c = 'ignore => augtype
    ['With, ['Apply, 'Join, augtype, axFormatType first c], []]
  ['With, ['Apply, 'Join, augtype, formatAugmentedType(c, a, $augmentedArgs)], []]


axFormatOpList ops == ['Sequence,:[axFormatOp o for o in ops]]

axOpTran(name) ==
   ATOM name =>
      name = 'elt => 'apply
      name = 'setelt => "set!"
      name = 'SEGMENT => ".."
      name = 1 => '_1
      name = 0 => '_0
      name
   opOf name = 'Zero => '_0
   opOf name = 'One => '_1
   error "bad op name"

axFormatOpSig(name, [result,:argtypes]) ==
   ['Declare, axOpTran name,
         ['Apply, "->", ['Comma, :[axFormatType t for t in argtypes]],
                        axFormatType result]]

axFormatConstantOp(name, [result]) ==
   ['Declare, axOpTran name, axFormatType result]

axFormatPred pred ==
   atom pred => pred
   [op,:args] := pred
   op = 'IF => axFormatOp pred
   op = 'has =>
      [name,type] := args
      if name = '$ then name := '%
      else name := axFormatOp name
      ftype := axFormatOp type
      if ftype is ['Declare,:.] then
           ftype := ['With, [], ftype]
      ['Test,['Has,name, ftype]]
   axArglist := [axFormatPred arg for arg in args]
   op = 'AND => ['And,:axArglist]
   op = 'OR  => ['Or,:axArglist]
   op = 'NOT => ['Not,:axArglist]
   error "unknown predicate"


-- This function is where we grow $augmentedType.
-- If for some arg of the form |#i| the test axFormattedPred is a 'Has test,
-- then we add pred to the type to the type value for |#i|.
axFormatAugmentOp(op, axFormattedPred, pred, augargs) ==
  if axFormattedPred is ['Test, ['Has, arg, augtype]] then
      -- Find arg in augargs or in $modemapArgs.
      -- If found we build up $augmentedArgs.
      -- To each key a list of pred(s) is stored together with the
      -- last type being a type from $modemapArgs or 'ignore.
      kv := ASSOC(arg, augargs)
      v := if null kv then
               if arg = '% then
                   ['ignore] -- want "categoryOf(%)" (will be ignored)
                 else
                   kv := ASSOC(arg, $modemapArgs)
                   -- $modemapArgs stuff must be unformatted
                   if null kv then
                       nil
                     else
                       -- We don't want Join with category Type.
                       if rest kv = ['Type] then ['ignore] else [rest kv]
             else
                 rest kv
      -- note that v is a list of the form [pred_n, pred_n-1, ..., pred1, cat]
      -- cat is only *one* item!!!
      if not(null v) then
          $augmentedArgs:local := [[arg, pred, :v], :delete(kv, augargs)]
  -- Now $augmentedArgs is set correctly and we pass it to axFormatOp.
  axFormatOp op

-- Delete the first predicate corresponding to key in the association
-- list assoclist.
-- This corresponds to deleting one 'Has level corresponding to key.
-- This function is mainly here to prevent infinite recursion.
deleteFirstPred(key, assoclist) ==
  null assoclist => assoclist
  assoclist is [kv, :t]
  kv is [k, pred, :v]
  k = key =>
      null v => t
      null cdr v => t
      [[k, :v], :t]
  [kv, :deleteFirstPred(key, t)]


axFormatOp op ==
   op is ['IF, pred, trueops, falseops] =>
      -- ops are either single op or ['PROGN, ops]
      -- In case we meet "if A has T then X else Y", we augment the type of
      -- A by T inside X. Inside Y nothing is augmented.
      -- We only care about such A that are parameters in a sourrounding
      -- constructor, i.e. something looking like |#i|.
      -- The form "if not(A has T) then ..." is not supported.
      axFormattedPred := axFormatPred pred
      NULL(trueops) or trueops='noBranch =>
         ['If, ['Test,['Not, axFormattedPred]], axFormatOp falseops, []]
      ['If, axFormattedPred,
            axFormatAugmentOp(trueops, axFormattedPred, pred, $augmentedArgs),
              axFormatOp falseops]
   op is ['SIGNATURE, name, type] => axFormatOpSig(name,type)
   op is ['SIGNATURE, name, type, 'constant] =>
            axFormatConstantOp(name,type)
   op is ['ATTRIBUTE, attributeOrCategory] =>
       categoryForm? attributeOrCategory =>
           axFormatType attributeOrCategory
       ['RestrictTo, axFormatAttrib attributeOrCategory, 'Category]
   op is ['PROGN, :ops] => axFormatOpList ops
   op is 'noBranch => []
   axFormatType op

addDefaults(catname, withform) ==
  withform isnt ['With, joins, ['Sequence,: oplist]] =>
     error "bad category body"
  null(defaults := getDefaultingOps catname) => withform
  defaultdefs := [makeDefaultDef(decl) for decl in defaults]
  ['With, joins,
     ['Sequence, :oplist, ['Default, ['Sequence,: defaultdefs]]]]

makeDefaultDef(decl) ==
  decl isnt ['Declare, op, type] =>
       error "bad default definition"
  $defaultFlag := true
  type is ['Apply, "->", args, result] =>
       ['Define, decl, ['Lambda, makeDefaultArgs args, result,
                    ['Label, op, 'dummyDefault]]]
  ['Define, ['Declare, op, type], 'dummyDefault]

makeDefaultArgs args ==
  args isnt ['Comma,:argl] => error "bad default argument list"
  ['Comma,: [['Declare,v,t] for v in $TriangleVariableList for t in argl]]

getDefaultingOps catname ==
  not(name:=hasDefaultPackage catname) => nil
  $infovec: local := getInfovec name
  opTable := $infovec.1
  $opList:local  := nil
  for i in 0..MAXINDEX opTable repeat
    op := opTable.i
    i := i + 1
    startIndex := opTable.i
    stopIndex :=
      i + 1 > MAXINDEX opTable => MAXINDEX getCodeVector()
      opTable.(i + 2)
    curIndex := startIndex
    while curIndex < stopIndex repeat
      curIndex := get1defaultOp(op,curIndex)
  $pretendFlag : local := true
  catops := GETDATABASE(catname, 'OPERATIONALIST)
  [axFormatDefaultOpSig(op,sig,catops) for opsig in $opList | opsig is [op,sig]]

axFormatDefaultOpSig(op, sig, catops) ==
  #sig > 1 => axFormatOpSig(op,sig)
  nsig := substitute('$, '($), sig) -- dcSig listifies '$ ??
  (catsigs := LASSOC(op, catops)) and
    (catsig := assoc(nsig, catsigs)) and last(catsig) = 'CONST =>
       axFormatConstantOp(op, sig)
  axFormatOpSig(op,sig)

get1defaultOp(op,index) ==
  numvec := getCodeVector()
  segment := getOpSegment index
  numOfArgs := numvec.index
  index := index + 1
  predNumber := numvec.index
  index := index + 1
  signumList :=
 -- following substitution fixes the problem that default packages
 -- have $ added as a first arg, thus other arg counts are off by 1.
    SUBLISLIS($FormalMapVariableList, rest $FormalMapVariableList,
             dcSig(numvec,index,numOfArgs))
  index := index + numOfArgs + 1
  slotNumber := numvec.index
  if not([op,signumList] in $opList) then
     $opList := [[op,signumList],:$opList]
  index + 1

axAddLiteral(name, type, dom) ==
  elt := [name, type, dom]
  if not member( elt, $literals) then
     $literals := [elt, :$literals]

axDoLiterals() ==
  [ [ 'Import,
          [ 'With, [],
            ['Declare, name, [ 'Apply, '_-_> , dom , '_% ]]],
                 type ] for [name, type, dom] in $literals]
