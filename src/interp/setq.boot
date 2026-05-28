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

$printTimeIfTrue := false

$frameMessages := nil
$displayStartMsgs := nil

$localVars := []


 -- These were originally in SPAD LISP

$UserLevel := 'development
$reportInstantiations := nil
$reportEachInstantiation := nil
$reportCounts := nil
$doNotCompileJustPrint := nil
$PrintCompilerMessageIfTrue := true
$Rep := "$Rep"
$scanIfTrue := nil
$Representation := nil
$definition := nil
$env := nil
$e := nil
$getPutTrace := nil

 -- **** X. Random tables

MAKEPROP("~>", 'Led, ["~>", "~>", 122, 121])
MAKEPROP('EQUATNUM, 'Nud, ['dummy, 'dummy, 0, 0])
MAKEPROP('EQUATNUM, 'Led, ['dummy, 'dummy, 10000, 0])
MAKEPROP('LET, 'Led, [":=", "LET", 125, 124])
MAKEPROP('SEGMENT, 'Led, ["..", "SEGMENT", 401, 699, ["boot-Seg"]])
MAKEPROP('SEGMENT, 'isSuffix, true)

$mutableDomains := []     -- checked in DEFINE BOOT
$maxSignatureLineNumber := 0
$functionLocations := []
$globalExposureGroupAlist := []
$localExposureDataDefault := VECTOR(['basic, 'categories], [], [])
$localExposureData := VECTOR(['basic, 'categories], [], [])
$ReadingFile := false

 -- Used by Spad stream machinery
$NonNullStream := '"NonNullStream"
$NullStream := '"NullStream"
$UninitializedStream := '"UninitializedStream"

$SPAD_ERRORS  := VECTOR(0, 0, 0)
$InteractiveMode := true

$ruleSetsInitialized := false

 -- This is association list, but since all values are lists we just
 -- write is list of lists
$InterpreterMacroAlist := [
        ["%i", 'complex, 0, 1],
        ["%e", 'exp, 1],
        ["%pi", 'pi],
        ["SF", 'DoubleFloat],
        ["%infinity", 'infinity],
        ["%plusInfinity", 'plusInfinity],
        ["%minusInfinity", 'minusInfinity]]

 -- These were originally in INIT LISP

$InitialCommandSynonymAlist := [
       ["?",         :'"what commands"],
       ["apropos",   :'"what things"],
       ["cache",     :'"set functions cache"],
       ["cl",        :'"clear"],
       ["co",        :'"compile"],
       ["d",         :'"display"],
       ["expose",    :'"set expose add constructor"],
       ["fortran",   :'"set output fortran"],
       ["h",         :'"help"],
       ["hd",        :'"system hypertex &"],
       ["ju",        :'"julia"],
       ["jud",        :'"juliad"],
       ["kclam",     :'"boot clearClams ( )"],
       ["killcaches",:'"boot clearConstructorCaches()"],
       ["prompt",    :'"set message prompt"],
       ["recurrence",:'"set functions recurrence"],
       ["restore",   :'"history )restore"],
       ["save",      :'"history )save"],
       ["startGraphics",   :'"system $FRICAS/lib/viewman &"],
       ["stopGraphics",    :'"lisp (|sockSendSignal| 2 15)"],
       ["time",      :'"set message time"],
       ["storage",   :'"set message storage"],
       ["type",      :'"set message type"],
       ["unexpose",  :'"set expose drop constructor"],
       ["wc",        :'"what categories"],
       ["wd",        :'"what domains"],
       ["wp",        :'"what packages"],
       ["ws",        :'"what synonyms"]]

$CommandSynonymAlist := COPY($InitialCommandSynonymAlist)

DEFPARAMETER($ConstructorCache, MAKE_HASHTABLE('EQ))
$instantRecord := MAKE_HASHTABLE('EQ)

$useIntegerSubdomain := true

 -- See CLAMMED BOOT for defs of following functions
$clamList := [
  ['canCoerce, 'hash, 'EQUAL, 'count],
  ['canCoerceFrom, 'hash, 'EQUAL, 'count],
  ['coerceConvertMmSelection, 'hash, 'EQUAL, 'count],
  ['isLegitimateMode, 'hash, 'EQUAL, 'count],
  ['isValidType, 'hash, 'EQUAL, 'count],
  ['resolveTT,   'hash, 'EQUAL, 'count],
  ['selectMms1, 'hash, 'EQUAL, 'count],
  ['underDomainOf, 'hash, 'EQUAL, 'count],
  ['findRetractMms, 'hash, 'EQUAL, 'count],
  ['getConstantFromDomain, 'hash, 'EQUAL, 'count],
  ['interpLookup, 'hash, 'EQUAL, 'count]]

 -- the following symbol holds the canonical "failed" value
$failed := '"failed"

$univariateDomains := [
    'UnivariatePolynomial,
    'UnivariateTaylorSeries,
    'UnivariateLaurentSeries,
    'UnivariatePuiseuxSeries]

$multivariateDomains := [
    'MultivariatePolynomial,
    'DistributedMultivariatePolynomial,
    'HomogeneousDistributedMultivariatePolynomial,
    'GeneralDistributedMultivariatePolynomial]

$Primitives := ['Union, 'Mapping, 'Record, 'Enumeration]

$DomainsWithoutLisplibs := [
  'CAPSULE, 'Union, 'Record, 'SubDomain, 'Mapping, 'Enumeration, 'Mode]

$QuickCode := true -- controls generation of QREFELT etc.
$QuickLet := true  -- controls generation of LET tracing.
$domainTraceNameAssoc := []   -- alist of traced domains
$tracedMapSignatures := false
$highlightAllowed := true

$ConstructorNames := ['SubDomain, 'Union, 'Record]

$SpecialDomainNames := ["add", 'CAPSULE, 'SubDomain, 'Union, 'Record]

$CategoryNames := [
   'Category, 'CATEGORY, 'RecordCategory, 'Join, 'EnumerationCategory,
   'UnionCategory]

$printStorageIfTrue := false
$noEnv := []

$SideEffectFreeFunctionList := [
  'null, "case", "0", "1", ":", "::", 'has, 'Mapping, 'Record, 'Union,
  'Enumeration, 'elt, "=", ">", ">=", "<", "<=", 'MEMBER, 'is, 'isnt,
  'ATOM]


$AnonymousFunction := ['AnonymousFunction]
$Any := ['Any]
$BFtag := ":BF:"
$Boolean := ['Boolean]
$Category := ['Category]
$Exit :=  ['Exit]

$OutputForm := ['OutputForm]
$Float := ['Float]
$DoubleFloat := ['DoubleFloat]

$Integer := ['Integer]
$SingleInteger := ['SingleInteger]
$ComplexInteger := ['Complex, $Integer]
$NonNegativeInteger := ['NonNegativeInteger]
$PositiveInteger := ['PositiveInteger]
$RationalNumber := ['Fraction, ['Integer]]
$String := ['String]
$Symbol := ['Symbol]
$Void := ['Void]
$QuotientField := 'Fraction
$FunctionalExpression := 'Expression

$InteractiveFrame := [[[]]]
$DomainsInScope := [[]]
$EmptyEnvironment := [[[]]]
$EmptyMode := "$EmptyMode"
$NoValue := "$NoValue"
$NoValueMode := 'NoValueMode
$DummyFunctorNames := ['Mapping]
$EmptyVector := VECTOR()
$Index := 0
$true := ['QUOTE, true]
$false := false
$suffix := nil
$BasicPredicates := '(INTEGERP STRINGP FLOATP)
$reportCompilation := false
$streamCount := 0
$cacheAlist := []
$cacheCount := 0
$reportExitModeStack := false
$prefix := nil
$formalArgList := []

$FormalMapVariableList := [
    "#1", "#2", "#3", "#4", "#5", "#6", "#7", "#8", "#9", "#10",
    "#11", "#12", "#13", "#14", "#15", "#16", "#17", "#18", "#19", "#20",
    "#21", "#22", "#23", "#24", "#25", "#26", "#27", "#28", "#29", "#30",
    "#31", "#32", "#33", "#34", "#35", "#36", "#37", "#38", "#39", "#40",
    "#41", "#42", "#43", "#44", "#45", "#46", "#47", "#48", "#49", "#50"]

$PatternVariableList := [
    "*1", "*2", "*3", "*4", "*5", "*6", "*7", "*8", "*9", "*10", "*11",
    "*12", "*13", "*14", "*15", "*16", "*17", "*18", "*19", "*20", "*21",
    "*22", "*23", "*24", "*25", "*26", "*27", "*28", "*29", "*30", "*31",
    "*32", "*33", "*34", "*35", "*36", "*37", "*38", "*39", "*40", "*41",
    "*42", "*43", "*44", "*45", "*46", "*47", "*48", "*49", "*50"]

$ModeVariableList := [
    "DV$1", "DV$2", "DV$3", "DV$4", "DV$5", "DV$6", "DV$7", "DV$8", "DV$9",
    "DV$10", "DV$11", "DV$12", "DV$13", "DV$14", "DV$15", "DV$16", "DV$17",
    "DV$18", "DV$19", "DV$20"]

$TriangleVariableList := [
    "t#1", "t#2", "t#3", "t#4", "t#5", "t#6", "t#7", "t#8", "t#9", "t#10",
    "t#11", "t#12", "t#13", "t#14", "t#15", "t#16", "t#17", "t#18",
    "t#19", "t#20", "t#21", "t#22", "t#23", "t#24", "t#25", "t#26", "t#27",
    "t#28", "t#29", "t#30", "t#31", "t#32", "t#33", "t#34", "t#35", "t#36",
    "t#37", "t#38", "t#39", "t#40", "t#41", "t#42", "t#43", "t#44", "t#45",
    "t#46", "t#47", "t#48", "t#49", "t#50"]

$FormalFunctionParameterList := [
    "##1", "##2", "##3", "##4", "##5", "##6", "##7", "##8", "##9", "##10",
    "##11", "##12", "##13", "##14", "##15"]

$PrimitiveDomainNames := [
    'List, 'Integer, 'NonNegativeInteger, 'PositiveInteger,
    'SingleInteger, 'String, 'Boolean]

$optimizableConstructorNames := [
    'List, 'Integer, 'PositiveInteger, 'NonNegativeInteger, 'SingleInteger,
    'String, 'Boolean, 'Symbol, 'DoubleFloat, 'PrimitiveArray, 'Vector,
    'Matrix, 'OneDimensionalArray, 'TwoDimensionalArray, 'U32Vector,
    'U32Matrix, 'U16Vector, 'U16Matrix, 'U8Vector, 'U8Matrix,
    'I32Vector, 'I32Matrix, 'I16Vector, 'I16Matrix, 'I8Vector, 'I8Matrix,
    'U64Int, 'PrimitiveTwoDimensionalArray,
    'DoubleFloatVector, 'DoubleFloatMatrix, 'ComplexDoubleFloatVector,
    'ComplexDoubleFloatMatrix, 'Character, 'SortedExponentVector,
    'HashState]

$Zero := ["0"]
$One  := ["1"]

$NonMentionableDomainNames := [
      "%", 'Rep, 'Record, 'Union, 'Mapping, 'Enumeration]

 --  modemap:==  ( <map> (p e) (p e) ... (p e) )  "
 --  modemaplist:= ( modemap ... )  "

$CategoryFrame := [[[]]]

$InitialDomainsInScope := ["$EmptyMode", "$NoValueMode"]

$InitialModemapFrame := [[[]]]

$NRTaddForm := []
$NRTdeltaList := []
$NRTdeltaListComp := []
$NRTbase := 0
$NRTdeltaLength := 0
$NRTmonitorIfTrue := false
