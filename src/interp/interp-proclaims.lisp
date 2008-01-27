#+:GCL
(progn
  (eval-when (:execute :compile-toplevel :load-toplevel)
                 (proclaim '(optimize (safety 1))))
(IN-PACKAGE "BOOT") 
(PROCLAIM '(FTYPE (FUNCTION (*) (VALUES T T)) READLINE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FIXNUM) CHAR2NUM FOAM:|ProgHashCode|
            FOAM:|strLength| LINE-NUMBER |eq0| |nothingWidth|
            |nothingSub| |nothingSuper| LINE-LAST-INDEX
            LINE-CURRENT-INDEX |widthSC|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FUNCTION) FOAM::FOAMPROGINFOSTRUCT-FUNCALL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FOAM:|SInt|)
            FOAM::FOAMPROGINFOSTRUCT-HASHVAL)) 
(PROCLAIM
    '(FTYPE (FUNCTION ((VECTOR T) (VECTOR T)) T) VMLISP::VGREATERP
            VMLISP::LEXVGREATERP)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) (VALUES T T)) FOAM:AXIOMXL-FILE-INIT-NAME
            |mkSharpVar| |makeCharacter| |mapCatchName| |queryUser|
            MONITOR-INFO FILE-GETTER-NAME |mkDomainCatName|
            |getKeyedMsg| |mkCacheName| |mkAuxiliaryName|)) 
(PROCLAIM '(FTYPE (FUNCTION ((VECTOR T)) T) TRIMLZ)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) (VALUES T T)) |read-line|)) 
(PROCLAIM '(FTYPE (FUNCTION (STRING FIXNUM) T) |subWord|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) FIXNUM) QSQUOTIENT QSREMAINDER
            FOAM:|SetProgHashCode| QENUM |attributeCategoryParentCount|
            GETCHARN)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) (VALUES T T)) |htMakeLabel|
            |fetchKeyedMsg|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T T T T) *) |makeFortranFun|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) *) |formatRight|
            |compileConstructorLib| |quoteApp| |argsapp| |appargs|
            |inApp| |appsc| |appfrac| |exptApp| |overbarApp|
            |appHorizLine| |overlabelApp| /D-1 |appmat| |formatLeft|
            |makeStream| |newExpandLocalTypeArgs| |formatFn|
            FOAM:|fputss| FOAM:|fgetss| |conform2StringList|
            |patternCheck,mknew| |kDomainName| |koPageAux| |dbShowOp1|
            APP |appagg| |binomialApp| |charyTrouble1| |appsub|
            |slashApp| |appsetq| |makeStatString|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) *) |apprpar1| |appargs1| |appagg1|
            |matrixBorder| |htQueryPage| |compileAndLink| |apphor|
            |appvertline| |applpar| |applpar1|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) *) $FCOPY LOCALDATABASE FE |ncBug|
            CONCAT)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) *) |htSetLiterals| |kcaPage1|
            |makeLongStatStringByProperty|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) *) BUILD-DEPSYS |htGlossPage|
            |checkCondition| |compTopLevel| GETOP
            |checkTransformFirsts| |dbShowOpAllDomains| |templateVal|
            |dbChooseDomainOp| |whoUsesOperation| |exp2FortSpecial|
            |dbShowCons1| |dbSelectCon| |dbShowOperationsFromConform|
            |genSearch1| |dbSearch| |constructorSearch|
            |underscoreDollars,fn| |oSearchGrep| |selectOption|
            |constructorSearchGrep| |dbInfoChoose1| |bcDrawIt2|
            |wiReplaceNode| |chk,fn| |replacePercentByDollar,fn|
            |getSlotFromDomain| |bcMkFunction| |Qf2F| |selectOptionLC|
            |bcDrawIt| |compUniquely| |compExpression| |ncloopInclude0|
            |asytranForm| |asytranFormSpecial| |asytranApplySpecial|
            SOCK-GET-STRING |showIt| |pmPreparse,fn| |pmPreparse,gn|
            |dbSearchAbbrev| |mkUpDownPattern,recurse| |htMkPath|
            |getVal|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T T) *) BUILD-INTERPSYS)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) T) |constructorAbbreviationErrorCheck|
            |BesselasymptB| |optCallSpecially| |getDocDomainForOpSig|
            |reportFunctionCacheAll| |clngammacase2| |constoken|
            |writeMalloc| |printDec| |htPred2English,gn| |prepareData|
            |protectedNagCall| |axiomType| |DescendCode|
            |SetFunctionSlots| |InvestigateConditions,update|
            |htSystemVariables,functionTail| |replaceExitEtc,fn|
            |compNoStacking1| |compClam| |getModemapListFromDomain|
            |say2Split| |compColonInside| |haddProp| |npEnclosed|
            ASHARPMKAUTOLOADFUNCTOR ASHARPMKAUTOLOADCATEGORY
            |getMatchingRightPren| |checkHTargs| |mkOperatorEntry|
            |catPairUnion| |lookupUF| |newLookupInCategories|
            |lookupFF| |simpHasSignature| |compareSig|
            |lazyCompareSigEqual| |lookupInAddChain|
            |lookupInCategories| |lookupInTable| |lookupDisplay|
            |dbShowOpConditions| |dbShowOpParameterJump|
            |dbShowOpImplementations| |dbShowOpParameters|
            |dbShowOpOrigins| |dbShowOpSignatures| |getSigSubst|
            STRPOSL |optDeltaEntry| |lazyMatchArg| |nrunNumArgCheck|
            |nextown2| |semchkProplist| |interpREPEAT|
            |markInsertChanges| |markLambda| |markTran|
            |markAutoCoerceDown| |compBoolean|
            |mkAndApplyZippedPredicates| |compNot| |compOr| |compAnd|
            |makeCommonEnvironment,fn| |compMapCondFun|
            |compApplyModemap| |compMapCond| |compMapCond'|
            |compToApply| REDUCE-N |applyMapping| |compFormWithModemap|
            |compAtomWithModemap| |ancestorsRecur|
            |checkCommentsForBraces| |dbShowOpDocumentation|
            |dbShowOpNames| REDUCE-N-1 |dbGatherData| |dbConsHeading|
            REDUCE-N-2 |termMatch| |matchAnySegment?| |replaceExitEtc|
            |put| |checkAndDeclare| |hasSigInTargetCategory|
            |mkDetailedGrepPattern| |displayInfoOp| |dbShowInfoOp|
            |compReduce1| STRPOS |letPrint3| |intloopSpadProcess|
            |zagApp| |findBalancingBrace| |appelse| |appChar|
            |appInfix| |htMakeButtonSaturn| |vconcatapp| |superSubApp|
            |xLate| |appconc| MAKELIB |appparu| |charySemiColon|
            |charyElse| |charyEquatnum| |bcFindString| |charySplit|
            |charyMinus| VMLISP::DCQGENEXP |augProplistOf| |putHist|
            |charyTrouble| |evalUntargetedADEF| |evalTargetedADEF|
            |mergeInPlace| |upLoopIterSTEP| |mergeSort| |interpLoop|
            |collectStream| |collectStream1| |lazyMatch|
            |lazyMatchArgDollarCheck| |interpCOLLECTbodyIter|
            |lookupInCompactTable| |sayLooking| |upStreamIterSTEP|
            |lookupIncomplete| |newLookupInAddChain|
            |hashNewLookupInCategories| |lookupComplete|
            |newLookupInCategories1| |lazyMatchAssocV|
            |collectSeveralStreams| |mkIterZippedFun| |compareSigEqual|
            |mkInterpTargetedADEF| |compileTargetedADEF|
            |collectOneStream| |oldCompLookupNoDefaults| |evalTuple|
            |interpIF| |getReduceFunction| |NRTgetMinivectorIndex|
            |xlPrematureFin| |xlPrematureEOF| |xlCmdBug|
            |analyzeNonRecursiveMap| |xlIfBug| |makeInternalMapName|
            |printCName| |xlSkippingFin| |clearDep1| |xlConsole|
            |domArg| |xlOK| |mkDomPvar| |hasSig| |xlSkip|
            |lookupInDomainVector| |putIntSymTab|
            |basicLookupCheckDefaults| |basicLookup|
            |findConstructorSlotNumber| |oldCompLookup| MAKE-FLOAT
            |analyzeUndeclaredMap| |getFileProperty|
            |compDefWhereClause,fetchType| |compSubDomain1|
            |putFileProperty| |srcPosNew| |substNames|
            |mac0MLambdaApply| |mac0ExpandBody| |genDomainView|
            |getArgValue2| |compFunctorBody| |analyzeMap|
            |defaultTarget| |selectDollarMms| |selectMmsGen|
            |allOrMatchingMms| |evalMmCat| |matchMmSig| /LOCATE
            |hasCateSpecialNew| |evalMm| |evalMmFreeFunction|
            |hasCateSpecial| |hasCate1| |boxApp| |concatApp| |appsum|
            |altSuperSubApp| |concatbApp| |appSum| |binomApp| |aggApp|
            |fixUpPredicate| |stepApp| |appneg| |setqMultipleExplicit|
            |braceApp| |compSetq1| |timesApp| |rootApp| |bracketApp|
            |plusApp| |appparu1| |bigopWidth| |P2Us| |pi2App| |boxLApp|
            |compOrCroak1| |piApp| |compForm2| |compForm3|
            |getConditionalCategoryOfType1| |indefIntegralApp|
            |nothingApp| |evalconstruct| |evalInfiniteTupleConstruct|
            |evalTupleConstruct| |consProplistOf| |setqMultiple|
            |coerceImmediateSubDomain| |intApp| |setqSingle|
            |assignError| |sigma2App| |canReturn| |appext| |centerApp|
            |sigmaApp| |stringApp| |MpP2P| |evalForm| |selectLocalMms|
            |bottomUpDefault| |canCoerceTopMatching|
            |catchCoerceFailure| |asGetModemaps| |asytranCategory|
            |asytranCategoryItem| |asytranDeclaration|
            |InvestigateConditions,flist| |condUnabbrev|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) T) |assocCache| |assocCacheShift|
            |assocCacheShiftCount| |pileForests| |isLegitimateMode;|
            |hasFileProperty;| |coerceConvertMmSelection;|
            |hasFilePropertyNoCache| |writeLib1| |rwrite|
            |getOplistWithUniqueSignatures| |checkSkipOpToken|
            |checkSkipIdentifierToken| |readLib1| |checkSkipBlanks|
            MAKE-PARSE-FUNC-FLATTEN-1 |checkSkipToken|
            |getDocForCategory| |newWordFrom| PRINT-XDR-STREAM
            |getDocForDomain| |getDoc| |htcharPosition| |mapRecurDepth|
            THETACHECK |RecordEqual| |flowSegmentedMsg| |rewriteMap0|
            |UnionEqual| |restoreDependentMapInfo|
            |mkEnumerationFunList| |dcSig| |mkMappingFunList|
            |analyzeNonRecur| |mkUnionFunList| |addMap|
            |mkRecordFunList| |fortCall| |MappingEqual| |axAddLiteral|
            |CondAncestorP| |writeStringLengths| |writeXDR|
            |updateDatabase| |deleteMap| |fnameNew| |parseTypeError|
            |axFormatDefaultOpSig| |htpSetProperty|
            |moreGeneralCategoryPredicate| |rewriteMap1| |encodeUnion|
            |displayMap| |makeCatPred| |lookupInDomainByName|
            |compileDeclaredMap| |simpHasAttribute| |compileCoerceMap|
            |displaySingleRule| |simpHasPred,simpHas| |hasAtt|
            |substDollarArgs| |hasAttSig| |NRTisRecurrenceRelation|
            SPADRWRITE0 |dbShowOpSigList| SPADRWRITE |dbSelectData|
            |recordNewValue| |dbReduceOpAlist| |recordOldValue| PUT
            |listOfCategoryEntriesIf| |orderUnionEntries,split|
            |dbResetOpAlistCondition| |getSlotNumberFromOperationAlist|
            MSUBST |algCoerceInteractive| |isSuperDomain|
            |buildPredVector,fn| |extendsCategoryBasic|
            |recordOldValue0| DEFIOSTREAM |catExtendsCat?|
            |PARSE-getSemanticForm| |recordNewValue0| |expandType|
            |expandTypeArgs| |getSlotFromFunctor| |stuffSlot|
            |addConstructorModemaps| |dbPresentOpsSaturn|
            |compDefWhereClause| |reduceOpAlistForDomain| |get1|
            |mungeAddGensyms,fn| |get2| |dbReduceBySelection| |get0|
            |throwListOfKeyedMsgs| |extendsCategoryBasic0|
            |getConstructorOpsAndAtts| EQSUBSTLIST |substSlotNumbers|
            |mkExplicitCategoryFunction| |dbReduceBySignature|
            |findDomainSlotNumber| |addIntSymTabBinding|
            |extendsCategory| |sigsMatch| |buildPredVector|
            |compDefineAddSignature| |dbParts| |hasFullSignature|
            |NRTextendsCategory1| |mkAtree2| |getSubstQualify|
            |mkAtree3| RPLNODE |markNumCheck|
            |getValueFromSpecificEnvironment|
            |fortFormatLabelledIfGoto| |compForMode| |getScriptName|
            |whoUsesMatch1?| |transferPropsToNode,transfer| HREMPROP
            |fullSubstitute| |genDomainOps| |getOperationAlist|
            |whoUsesMatch?| |remprop| |getfortarrayexp|
            |setMsgForcedAttr| |addWhereList| |P2Uts|
            |dbGetDisplayFormForOp| |Up2FR| RWRITE
            |dbGetFormFromDocumentation| |mac0Define| |anySubstring?|
            |getMappingArgValue| VMLISP::MAKE-ENTRY |compContained|
            |NRTsetVector4a| |getArgValueComp| |NRTsetVector4Part1|
            |altTypeOf| |mac0InfiniteExpansion| |NRTencode,encode|
            |setMsgUnforcedAttr| |consOpSig| |genDomainViewList|
            |genSlotSig| |compSubDomain| |compCapsule|
            |sideEffectedArg?| |newExpandGoGetTypeSlot|
            |evalFormMkValue| MAKEOP |doItIf| |compSingleCapsuleItem|
            |insertEntry| |compJoin| |nextown| |rewriteMap| |mkFortFn|
            |NRTgetLookupFunction| |parseIf,ifTran| |exp2Fort2|
            |evalQUOTE| |lisplibWrite| |evalSEQ| |getLocalMms|
            |IFcodeTran| |makeFunctorArgumentParameters| |exp2FortFn|
            |selectMmsGen,exact?| |fortFormatHead| |addContour,fn1|
            REDUCE-1 |getLocalMms,f| |traverse,traverseInner|
            |isOpInDomain| |upTableSetelt| |printSignature|
            |compDefine| MACRO-MISSINGARGS |compCategory|
            |NRTvectorCopy| |addContour,fn3| |getTargetFromRhs|
            |commandAmbiguityError| |charPosition| |unifyStructVar|
            |traverse| |augmentSub| |dbPart| |unifyStruct|
            |commandErrorMessage| |compAdd| |substituteOp|
            |filterModemapsFromPackages| |displayModemap| |constrArg|
            |displayType| |evalMmCond0| |maprinSpecial| |comp|
            |hasCaty| |displayMode| |numOfOccurencesOf,fn| |evalMmCond|
            |mkNewCapsuleItem| VMLISP::QUOREM |hasCate| |pmatchWithSl|
            |matchTypes| |displayCondition| |findUniqueOpInDomain|
            |hasSigOr| |getLocOf| |displayValue|
            |intersectionContour,buildModeAssoc| |hasSigAnd| |get|
            |findCommonSigInDomain| |sigDomainVal| |evalMmCat1|
            GEQNSUBSTLIST |coerceTypeArgs| |markInsertSeq|
            |compNoStacking| |domArg2| |transImplementation| |L2Tuple|
            V2M GEQSUBSTLIST DEF-INNER |libConstructorSig,g| |OV2Sy|
            |coerceable| |Qf2EF| |markMultipleExplicit|
            |substituteIntoFunctorModemap| |Sy2P| |adjExitLevel| I2NNI
            |getParentsFor| |Rm2L| |asytranApply| |Var2OtherPS|
            |explodeIfs,fn| |Var2UpS| |dbSplit| OV2SE |charybdis|
            |buildLibAttr| |NDmp2domain| |buildLibOp| |Var2Up|
            |transKCatAlist| |Expr2Mp| |dbTickIndex| |Expr2Dmp|
            |insertShortAlist| |Sy2NDmp| |sublisFormal,sublisFormal1|
            |Dmp2P| PUTALIST |Sy2Mp| |Var2SUP| |dbSetOpAlistCondition|
            |Factored2Factored| |compiledLookup| I2PI |insertAlist|
            |P2Expr| |reduceAlistForDomain| |P2Up| HPUT |P2Dmp|
            |Var2FS| |dbXParts| |Sy2Dmp| |kePageDisplay| B-MDEF
            |markCallCoerce| |dbShowOpItems| |Ker2Expr| MKPFFLATTEN-1
            |Sy2OV| |dbSearchOrder| |Var2QF| CARCDRX1 |Sm2V| M2V
            |Var2P| AS-INSERT1 I2OI AS-INSERT P2FR PROPERTY
            |makeEijSquareMatrix| |mkDomTypeForm| |Set2L|
            |stringPosition| |Sm2Rm| |patternCheck,equal| DEF
            |rightCharPosition| |Var2NDmp| |infix?| |Dmp2Dmp|
            |matchSegment?| |coerceDmp2| |stringMatch| |rread|
            |skipBlanks| I2EI |formatDollar| |dbPresentConsSaturn|
            |Var2Mp| MAKE-DEFUN |compCapsuleInner| |compOrCroak|
            |profileRecord| |markCoerce| |Mp2FR| |getSignature|
            |Qf2domain| |traceDomainLocalOps| |getArgumentModeOrMoan|
            |compCapsuleItems| |filterListOfStringsWithFn| |L2Set|
            |mkGrepPattern1,charPosition| |Var2Gdmp| |displayModemap,g|
            COMP-ILAM |filterAndFormatConstructors| COMP-SPADSLAM
            |userLevelErrorMessage| |addBinding| |L2Sm|
            |dbShowConsDoc1| |makePathname| |mkCategoryPackage|
            |mkConform| COMP-SLAM |dbInfoFindCat| L2M |markChanges|
            |compReduce| |compDefine1| |dbShowInfoList|
            |markEncodeLoop| |markRecord| |dbShowConditions| |Mp2Expr|
            |compRepeatOrCollect| |Ker2Ker| |dbInfoOrigin| |Var2Dmp|
            |dbConstructorDoc| |Dmp2NDmp| |interpret2| |Sm2PolyType|
            |htpSetLabelInputString| |Var2OV| |letPrint2| |letPrint|
            |orderPredicateItems| |wrapBraces| |mapLetPrint|
            |htpAddInputAreaProp| |L2Rm| |substVars|
            |getOpBindingPower| |markCase| |OV2poly|
            |infixArgNeedsParens| |Sm2M| |compPART|
            |augmentLisplibModemapsFromFunctor| |compNoStackingAux|
            |linearFinalRequest| |compWI| |bcInputEquations,f| |compMI|
            OV2P |needBlankForRoot| |htpSetLabelErrorMsg| |Rn2F|
            |isBreakSegment?| |formatSpill2| |substring?| |Sm2L|
            |sublisMatAlist| MAKESPAD |splitConcat| |formatComments|
            |Un2E| |SUP2Up| |reportCategory| OV2OV |insertAlist,fn|
            |getLocationsOf| |longext| |replaceVars|
            |buildNewDefinition| |npParenthesize| |compreduce|
            |bcString2WordList,fn| |compFromIf| VMLISP::ECQGENEXP
            |markPaths| |Scr2Scr| |formatIF2| VMLISP::RCQGENEXP
            |outputString| |L2Record| |outputNumber| |Rm2V|
            VMLISP::DODSETQ ADDASSOC |pfInfApplication| |domain2NDmp|
            |insertString| |Up2Up| |npAndOr|
            |augLisplibModemapsFromCategory| |npListofFun| |P2Mp|
            |compWithMappingMode,FreeList| |optSpecialCall|
            |orderPredTran| |pfPushBody| |Rm2Sm| |pfIf| |incZip| |Rm2M|
            |augProplist| |augProplistInteractive| |Up2SUP|
            |formatColonWith| |centerString| |Mp2Up| |evalCOLLECT|
            |Mp2Dmp| |interpCOLLECTbody| |LargeMatrixp| |upLoopIterIN|
            DP2DP |position,posn| |Dmp2Up| |domainVal| |Up2P|
            |subVecNodes| |Complex2Expr| |addBindingInteractive|
            |seteltModemapFilter| |interpCOLLECT| /MONITORX
            |upTaggedUnionConstruct| |upRecordConstruct| |P2Upxs|
            |newExpandTypeSlot| |upNullList| |coerceTraceFunValue2E|
            |upStreamIterIN| |Complex2FR| |getCatForm| |Up2Mp|
            |oldAxiomAddChild| V2L |evalCOERCE| |P2Uls| |M2Sm|
            |lookupPred| |coerceTraceArgs2E|
            |oldAxiomDomainHasCategory| |Complex2underDomain|
            |mkIterFun| |resolveTTRed2| |attributeCategoryBuild|
            |Agg2L2Agg| |oldAxiomCategoryBuild| |resolveTTRed1|
            |fnameMake| |upLETtype| MONITOR-PRINARGS
            |upLETWithFormOnLhs| |eltModemapFilter| |lazyMatchAssocV1|
            |coerceOrCroak| |oldAxiomCategoryNthParent| |resolveTTEq2|
            |assignSymbol| |evalIsntPredicate| |resolveTTEq1|
            |evalIsPredicate| |matchUpToPatternVars|
            |SpadInterpretStream| |getConditionalCategoryOfType|
            |upSetelt| |getSubDomainPredicate| |upNullTuple| |evalIF|
            |resolveTMEq2| |intloopProcess| |coerceIntX| |evalis|
            |compSymbol| |evalREPEAT| |coerceSubDomain|
            |compExpressionList| |upwhereMain| |NRTcompileEvalForm|
            |upwhereMkAtree| |upwhereClause| |setqMultiple,decompose|
            |intloopInclude0| |permuteToOrder| |retractUnderDomain|
            |intloopSpadProcess,interp| |compList| |incPrefix?|
            |inclmsgIfSyntax| SMALL-ENOUGH-COUNT
            |renamePatternVariables1| |isRectangularList|
            |newExpandLocalType| |augModemapsFromDomain1|
            |newExpandLocalTypeForm| |canCoerceByFunction1|
            |oldAxiomPreCategoryBuild| |sayFunctionSelectionResult|
            |getFunctionFromDomain| |compForm| |lazyOldAxiomAddChild|
            |compTypeOf| |getOpCode| |comp3| |lazyDomainSet|
            |coerceOrFail| |application2String|
            |computeTTTranspositions,compress| |coerce0,fn| |putI|
            |algEqual| |mkInterpFun| |compiledLookupCheck| |interpret1|
            |coerceOrThrowFailure| |comp0| |analyzeMap0|
            |reportOpSymbol,sayMms| |NRTcompiledLookup|
            |compConstruct1| |spad2BootCoerce| |findLocalsInLoop|
            |M2Rm| M2M |compNoStacking0| L2V |Mp2P| |compNoStacking01|
            |Mp2Mp| |coerceDmpCoeffs| |Expr2Complex| |Dmp2Expr|
            |coerceFFE| M2L |V2Sm| |isRectangularVector| V2DP L2DP
            |Up2Expr| |Qf2Qf| |NDmp2NDmp| |V2Rm| |Qf2PF| |Dmp2Mp|
            |Up2Dmp| |Sy2Var| |Agg2Agg| |nsubst| |Expr2Up| |Sy2Up|
            |pvarCondList1| |interpRewriteRule| |putAtree| |isEltable|
            |selectMms| |throwKeyedMsgSP| |pushDownTargetInfo|
            |pushDownOnArithmeticVariables| |keyedMsgCompFailureSP|
            |intCodeGenCoerce1| |throwKeyedMsgCannotCoerceWithValue|
            |asytranForm1| |hput| |asyCattranOp1| |charyTop|
            |asyMakeOperationAlist| |asGetExports| ELEMN |asySig1|
            |ncPutQ| |throwKeyedErrorMsg|
            |mkUserConstructorAbbreviation| |unabbrevSpecialForms|
            |nAssocQ| |errorSupervisor1| |argumentDataError|
            |BesselasymptA| |htpSetLabelSpadValue| |from?| |clngamma|
            |pspadOpBindingPower| |formatDeftranIf| |formatAddDef|
            |formatPileLine| |formatOpBindingPower|
            |formatDeftranCapsule| |genCaseTag| |chebevalarr| |PsiBack|
            |logH| SUBSTRING |PiMinusLogSinPi| |besselIcheb|
            |chebstarevalarr| |chebf01coefmake| |clngammacase23|
            |PsiAsymptoticOrder| |grepf| |clngammacase1| |cotdiffeval|
            |BesselIAsympt| |lffloat| |substringMatch|
            |makeResultRecord| |makeCompilation|
            |extractFileNameFromPath,fn| |makeAspGenerators|
            |makeAspGenerators1| |mkNewUnionFunList| |EnumEqual|
            |cleanUpAfterNagman| |sySpecificErrorAtToken|
            |prepareResults,defaultValue| |pfLambda| |pfWIf|
            |SigSlotsMatch| |DomainPrint1| |DescendCodeAdd1,update|
            |pfTLambda| |htSystemVariables,fn| |postCollect,finish|
            |npBackTrack| |bchtMakeButton| |compWhere| |compVector|
            |compAtom| MACRO-INVALIDARGS |getUniqueModemap|
            |modeIsAggregateOf| |compArgumentsAndTryAgain| |compForm1|
            |mergeModemap| |compSubsetCategory| SUBSTEQ |compString|
            |augModemapsFromDomain| |compWithMappingMode|
            |extractCodeAndConstructTriple| QESET |compCat| |pfWith|
            |compMakeDeclaration| |extendsCategoryForm| |compSeq|
            FOAM:|FormatNumber| |compSeq1| |compReturn| |isSubset|
            |getModemapList| |compCase1| |compCoerce1| |compPretend|
            |compMacro| |compConstructorCategory| |compCoerce|
            |compColon| |stringIsWordOf?| |compSetq| |compLeave|
            |npList| SUBLISLIS |modeEqualSubst| |compIf| |compIs|
            |comp2| |compImport| |coerce,fn| |throwKeyedMsgFromDb|
            |sayKeyedMsgFromDb| |compHas| |compExit| |compElt|
            |compConstruct| |compCons| |compCons1| |compSeqItem|
            |recordInstantiation1| |compCase| |compQuote|
            |recordInstantiation| |compAtSign| |compSuchthat|
            |markReduceIterator| |addToConstructorCache|
            |loadLibNoUpdate| SETDATABASE |lassocShiftWithFunction|
            |markAny| |rplacaSubst| |rplacaSubst,fn|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) T) |remove| REMOVEQ RREAD |listSort|
            MATCH-TOKEN NREMOVEQ NREMOVE |pfLeaf| BPITRACE
            MATCH-LISP-TAG |tokConstruct| |pfAdd| LINE-NEW-LINE)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T *) T) |msgCreate|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T) T) |writeCFile| |Mp2MpAux2|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T) T) |compDefineCategory2|
            |P2MpAux| |makeFort1|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T T) T) |P2DmpAux|
            |makeSpadFun|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T T T) T) |displayDomainOp|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T) T) |findFunctionInCategory|
            |Mp2MpAux1| |Mp2MpAux0| |Expr2Dmp1| |Mp2SimilarDmp|
            |bigopAppAux| |findFunctionInDomain| |abbreviationError|
            |lisplibError| |invokeNagman| |mkNewModemapList|
            |mkDiffAssoc| |dbGatherThenShow| |appInfixArg|
            |lazyOldAxiomDomainLookupExport|
            |oldAxiomDomainLookupExport|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) T) |compFormWithModemap1|
            |analyzeRecursiveMap| |augmentMap|
            |reportFunctionCompilation| |putSrcPos|
            |hasSigInTargetCategory,fn| |encodeFunctionName|
            |getArgValueComp2| |augModemapsFromCategory|
            |compDefineFunctor1| |augModemapsFromCategoryRep|
            |compDefineFunctor| |processFunctor| |buildFunctor|
            |selectMmsGen,matchMms| |makeConstrArg|
            |commuteSparseUnivariatePolynomial|
            |commuteUnivariatePolynomial| |commuteSquareMatrix|
            |coerceDmp1| |aggregateApp| |compDefineCategory1|
            |commuteFraction| |compDefineCategory| |commuteQuaternion|
            |commuteComplex| |resolveTT2| |concatApp1|
            |compFormPartiallyBottomUp| |canReturn,findThrow|
            |orderMms| |sayFunctionSelection| MATCH-FUNCTION-DEF
            |commuteNewDistributedMultivariatePolynomial|
            |commuteMPolyCat|
            |commuteDistributedMultivariatePolynomial|
            |commuteMultivariatePolynomial| |commutePolynomial|
            |bottomUpDefaultCompile| |bottomUpDefaultEval|
            |bottomUpFormTuple| |bottomUpFormAnyUnionRetract|
            |bottomUpForm| |bottomUpFormUntaggedUnionRetract|
            |bottomUpFormRetract| |bottomUpForm2| |bottomUpForm0|
            |bottomUpForm3| |coerceByTable| |compileRecurrenceRelation|
            |logS| |spadify| |prepareResults| |DescendCodeAdd1|
            |htSystemVariables,displayOptions| |evalAndSub| FINCOMBLOCK
            |compIf,Env| LOCALASY |mkCacheVec| LOCALNRLIB |selectMms1;|
            |selectMms2| |mkCategory| |newCompareSig| |lookupInDomain|
            |fortFormatDo| |newLookupInDomain| |getNewDefaultPackage|
            |printLabelledList| |compApplication|
            |dbExpandOpAlistIfNecessary| -REDUCE
            |compDefineCapsuleFunction| |genSearchSay|
            |compRepeatOrCollect,fn| |dbGetDocTable| |apprpar|
            WRITE-TAG-LINE |concatTrouble| |charyBinary| |split2|
            |needStar| |lazyMatchArg2| |newLookupInTable|
            |hashNewLookupInTable| |compileADEFBody| |interpLoopIter|
            |compileIF| |markCoerceByModemap| |xlCannotRead| |xlMsg|
            |xlNoSuchFile| |incLine| |xlFileCycle| |xlConStill|
            |xlConActive| |xlSay| |xlOK1| |incLude|
            |analyzeDeclaredMap| |setqSetelt|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T *) T) |getInheritanceByDoc| |ncHardError|
            TOKEN-INSTALL |ncSoftError| |lnCreate|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) T) |smallIntegerStep|
            |compDefineLisplib| |compConLib1| |addModemap| |mmCost|
            |findFunctionInDomain1| /WRITEUPDATE |mmCost0| |/D,2,LIB|
            |processFunctorOrPackage| |compOrCroak1,fn| /D-2
            |BesselIBackRecur| |invokeFortran| |nagCall| |makeFort|
            |addModemapKnown| |addModemap1| |addEltModemap| |compHash|
            |compApply| |kdPageInfo| |addModemap0| |bracketagglist|
            |attributeLookupExport| |upDollarTuple| |xlIfSyntax|
            |incLine1| |oldAxiomCategoryLookupExport| |genMapCode|
            |putMapCode|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T *) T) RPLACSTR)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL *) MKPROMPT |sendHTErrorSignal| |testPage|
            RECLAIM |minusInfinity| |plusInfinity| SERVER-SWITCH
            |executeQuietCommand| |serverSwitch| |scanS|
            |sendNagmanErrorSignal| |htSystemVariables| |htSetVars|
            |mkSetTitle| |npCategory| PARSE-CONS_SEXPR PARSE-SEXPR
            PARSE-REF_SEXPR PARSE-EXPR2 PARSE-EXPR1 |htsv|
            |npDefinitionItem| |npDefn| |npMacro| |npMDEFinition|
            |npRule| RESETHASHTABLES |batchExecute| |quit|
            |quitSpad2Cmd| |pquit| |pquitSpad2Cmd| |continue|
            |copyright| |htShowPageNoScroll| |writeSaturnSuffix|
            PARSE-LOCAL_VAR |htErrorStar| |queryClients| |onDisk|
            |endHTPage| |readSpadProfileIfThere| |bcDraw3Dpar1|
            |bcDraw3Dpar| |htShowPageStarSaturn| |htShowPageStar|
            |bcDraw3Dfun| |bcDraw2Dpar| |bcSum| |bcSeries| |bcProduct|
            |bcLimit| |bcIndefiniteIntegrate| |bcDraw|
            |bcDifferentiate| |bcDefiniteIntegrate| |bcDraw2Dfun|
            MAKE-TAGS-FILE |bcSolve| |npPrimary1| |generateResultsName|
            |generateDataName| |htShowPage| |PARSE-Label| |bcMatrix|
            |PARSE-Primary1| |PARSE-Enclosure| |bcDraw2DSolve|
            |PARSE-Selector| |PARSE-Category| |PARSE-Option|
            |PARSE-TokenOption| |PARSE-Sexpr1| |PARSE-Sexpr|
            |PARSE-Scripts| |PARSE-SpecialCommand|
            |PARSE-FloatBasePart| |PARSE-FloatBase| |PARSE-Leave|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL T) |npDollar| |npSQualTypelist|
            PARSE-NON_DEST_REF PARSE-OPT_EXPR PARSE-REPEATOR
            |npCategoryL| PARSE-SEXPR_STRING |npProduct| PARSE-TEST
            |npIterators| PARSE-EXPR |npWhile|
            |displayPreCompilationErrors| PARSE-N_TEST |npForIn|
            PARSE-REP_TEST |npGives| PARSE-FIL_TEST |npLogical|
            PARSE-SUBEXPR |npExpress| PARSE-FID PARSE-RULE |npExpress1|
            PARSE-HEADER |npCommaBackSet| PARSE-RULE1 |npQualType|
            |npADD| |npConditionalStatement| |npQualifiedDefinition|
            |npPushId| |npVariable| |npDefinitionOrStatement|
            |npAssignVariable| |npColon| |npAssignment|
            |profileDisplay| |computeDomainVariableAlist|
            MONITOR-READINTERP |npSingleRule| MONITOR-UNTESTED
            |npDefTail| |npQuiver| MONITOR-PERCENT |npDef|
            |npStatement| |npImport| |npTyping| |npItem| |npQualDef|
            |npAssign| MONITOR-AUTOLOAD |npDefinition| MONITOR-RESULTS
            MONITOR-END |npPop3| MONITOR-INITTABLE |npAtom2|
            |npInfixOperator| |npPower| MONITOR-HELP |npMatch|
            MONITOR-REPORT |npMdef| |reportInstantiations| |npPrimary2|
            ?DOMAINS |?domains| |npSuch| |npMDEF| $TOTAL-ELAPSED-TIME
            |npDisjand| |npInfixOp| |npDiscrim|
            |clearConstructorAndLisplibCaches| |npVariableName|
            |clearConstructorCaches| |clearClams| |clearCategoryCaches|
            |cacheStats| |reportAndClearClams| |traceDown|
            |statRecordInstantiationEvent| |tc| |removeAllClams|
            |clamStats| CURRENT-SYMBOL |npPop1| |npTrap|
            |npApplication| |npPop2| |npApplication2| WRITE-WARMDATA
            WRITE-INTERPDB |npAssignVariablelist|
            |clearHashReferenceCounts| |npSignature| |pfNothing|
            |npSigItemlist| FAIL |npEncl| |npBDefinition|
            |npPrefixColon| $TOTAL-GC-TIME |npNext| |allOperations|
            WRITE-CATEGORYDB WRITE-OPERATIONDB WRITE-BROWSEDB
            WRITE-COMPRESS INITIAL-GETDATABASE CATEGORYOPEN BROWSEOPEN
            OPERATIONOPEN INTERPOPEN COMPRESSOPEN |combineDefinitions|
            CREATE-INITIALIZERS |poNoPosition|
            |saveDependentsHashTable| |saveUsersHashTable|
            |mkTopicHashTable| TOKEN-STACK-SHOW
            |terminateSystemCommand| |getSystemCommandLine|
            IOSTREAMS-SHOW |displayExposedConstructors|
            |finalizeDocumentation| REDUCE-STACK-SHOW CLEAR-HIGHLIGHT
            RESET-HIGHLIGHT RESTART0 |waitForViewport|
            |setViewportProcess| |undoINITIALIZE| |moveAroundLines|
            |simpCategoryTable| |simpTempCategoryTable| COMPFIN
            INPUT-CLEAR |genTempCategoryTable| |cc| |initNewWorld|
            |genCategoryTable| |dbOpsExposureMessage| |mkCheckRun|
            |htSayUnexposed| |NRTmakeCategoryAlist|
            |NRTgenFinalAttributeAlist| |dcSizeAll|
            |initialiseIntrinsicList| |tempLen|
            |changeDirectoryInSlot1| |NRTaddDeltaCode| |ncIntLoop|
            |newFortranTempVar| |currentSP| |elapsedTime| |traceUp|
            |getIntrinsicList| NEXT-CHAR |getInterpMacroNames|
            |synonymSpad2Cmd| |interpFunctionDepAlists| |isFalse|
            |printDashedLine| |satBreak| |up| |getWorkspaceNames|
            |getParserMacroNames| |oldCompilerAutoloadOnceTrigger|
            |saveC| |restoreC| |saveD| |restoreD| |undent| |formatLB|
            |newLine| |indent| |nearMargin| |getCommonImports|
            |getNumberTypesInScope| |purgeLocalLibdb| |dbSplitLibdb|
            |TrimCF| |displayWorkspaceNames| UP |displayWarnings|
            |buildGloss| |nextInterpreterFrame|
            |describeSetOutputOpenMath| |down| |displayFrameNames| DOWN
            |previousInterpreterFrame| SAME |same| CURRENTTIME
            |mkUsersHashTable| |markTryPaths| |allConstructors|
            |frameNames| |sayShowWarning| |credits|
            |mkDependentsHashTable| |buildDefaultPackageNamesHT|
            |dbAugmentConstructorDataTable| |menuButton|
            |htSaturnBreak| |random| CURRENT-CHAR
            |dbConsExposureMessage| FIRST-ERROR |writeSaturnPrefix|
            |on| |offDisk| |htBigSkip| PARSE-PROGRAM IN-META
            |traceReply| |?t| SKIP-BLANKS |pspacers| NEXT-LINES-SHOW
            HELP |resetCounters| |version| PARSE-DEST_REF
            SPAD_SHORT_ERROR |pcounters| SPAD_LONG_ERROR
            INIT-BOOT/SPAD-READER NEXT-LINES-CLEAR |resetTimers|
            |resetSpacers| |ptimers| |PARSE-Expression|
            |oldParserAutoloadOnceTrigger| |reportCount| |markFinish1|
            |spadReply| |listConstructorAbbreviations| BOOT-SKIP-BLANKS
            |updateFromCurrentInterpreterFrame|
            PARSE-ARGUMENT-DESIGNATOR IOSTAT PARSE-KEYWORD
            PARSE-SPADSTRING |initializeInterpreterFrameRing|
            PARSE-NUMBER |reportWhatOptions| TERSYSCOMMAND
            |PARSE-NewExpr| |makeInitialModemapFrame| UNGET-TOKENS
            |createCurrentInterpreterFrame| $SCREENSIZE
            |getParserMacros| |clearCmdCompletely| |clearCmdAll|
            |clearMacroTable| PARSE-IDENTIFIER
            |initializeSystemCommands| |htSayHrule| |htEndTable|
            |mkMenuButton| |mkCheck| |runspad| |markTerpri|
            |htBeginTable| |ncTopLevel| |spadStartUpMsgs|
            BUMPCOMPERRORCOUNT |initializeRuleSets|
            |loadExposureGroupData| |statisticsInitialization| |ut|
            |printStatisticsSummary| |printStorage| |prTraceNames|
            |getCodeVector| PARSE-BSTRING |spad| |axDoLiterals|
            |PARSE-Suffix| |npPPg| |PARSE-TokTail| |npPCg| PARSE-STRING
            |PARSE-InfixWith| |spadpo| |npPPf| |PARSE-With| |npPCff|
            CURRENT-TOKEN |PARSE-Form| |npPPff| |PARSE-Reduction|
            |intloop| |PARSE-SemiColon| |PARSE-Iterator|
            |PARSE-Primary| |off| ADVANCE-TOKEN |PARSE-ElseClause|
            |htEndTabular| |PARSE-Conditional| |htSaySaturnAmpersand|
            |PARSE-Name| NEXT-TOKEN |page| |PARSE-Sequence|
            |clearFrame| |PARSE-Data| |getSaturnExampleList|
            |PARSE-FormalParameter| |saturnTERPRI| |PARSE-IntegerTok|
            |bcSadFaces| YEARWEEK |PARSE-String| |PARSE-Quad|
            |npBPileDefinition| |PARSE-VarForm| |npTypified|
            |PARSE-Qualification| |npVariablelist| |PARSE-Prefix|
            |npTagged| |PARSE-Infix| |bcvspace| |PARSE-Application|
            |npTypeStyle| |clearCmdSortedCaches| |npColonQuery|
            |PARSE-Statement| |reFinish| |npPretend| |PARSE-Command|
            |npRestrict| |updateInCoreHist| |npCoerceTo|
            |processSynonyms| |npRelation| |disableHist| |npFirstTok|
            |PARSE-IteratorTail| |npVoid| |histFileName| |npSLocalItem|
            |PARSE-OpenBrace| |PARSE-Sequence1| |npLocalItemlist|
            |PARSE-OpenBracket| |npFix| |npDefaultItemlist|
            |PARSE-PrimaryNoFloat| |npSynthetic| |npAmpersandFrom|
            |npBy| |PARSE-Float| |npLet| |PARSE-PrimaryOrQM|
            |PARSE-TokenList| |PARSE-AnyId| |resetInCoreHist|
            |npTypeVariable| |PARSE-TokenCommandTail|
            |npSignatureDefinee| |isTokenDelimiter| |formatSC|
            |PARSE-ScriptItem| |PARSE-CommandTail| |npAtom1|
            |historySpad2Cmd| |PARSE-FormalParameterTok|
            |PARSE-SpecialKeyWord| |npConstTok|
            |writeHistModesAndValues| |PARSE-FloatTok|
            |PARSE-FloatExponent| |updateHist| |npLocalItem|
            |initHistList| |npLocalDecl| |initHist| |npExport|
            |PARSE-Exit| |npLocal| |oldHistFileName| |npInline|
            |PARSE-Return| |npFree|
            |spad2AsTranslatorAutoloadOnceTrigger| |PARSE-ReductionOp|
            |npInterval| |PARSE-LabelExpr| |npSegment| |PARSE-Import|
            |npArith| |writeHiFi| |npBreak| |npDefaultItem|
            |PARSE-Loop| |npDefaultDecl| |npReturn| |npSemiBackSet|
            |updateCurrentInterpreterFrame| |npSDefaultItem|
            |npTypeVariablelist| |npPileDefinitionlist|
            |npDefinitionlist| |PARSE-Seg| |npComma| |profileWrite|
            |npSymbolVariable| |isPackageFunction| |npId|
            |setOptKeyBlanks| |npSum| |getInfovecCode| |npTerm|
            |NRTmakeSlot1Info| |npRemainder|
            |reportOnFunctorCompilation| |npIterate|
            |displayMissingFunctions| |npLoop| ERRHUH |npSuchThat|
            IS-GENSYM |npSelector| |getSpecialCaseAssoc| |npIterator|
            |makeConstructorsAutoLoad| |npSigItem|
            |displayExposedGroups| |displayHiddenConstructors|
            |npSigDecl| |displaySemanticErrors| |statRecordLoadEvent|
            |clock| |startTimer| |spadPrompt| |stopTimer|
            |computeElapsedTime| |quadSch| /TRACEREPLY TRACELETREPLY
            |npLambda| |voidValue| /COMP |getDateAndTime|
            |coercionFailure| |computeElapsedSpace|
            |printableArgModeSetList| |asList| |popTimedName|
            |npBacksetElse| |extendConstructorDataTable|
            |peekTimedName| |fin| |npQualTypelist| |npPileExit|
            |npExit| |mkLowerCaseConTable| |statisticsSummary|
            |displayHeapStatsIfWanted| |update| |intUnsetQuiet|
            |intSetQuiet| |intSetNeedToSignalSessionManager|
            |intNewFloat| |leaveScratchpad| |spadThrow| |ncError|
            INITIALIZE |incConsoleInput| |inclmsgCmdBug| |inclmsgIfBug|
            |inclmsgFinSkipped| |inclmsgConsole| |rbrkSch| |lbrkSch|
            |returnToReader| |returnToTopLevel| EMBEDDED TOP
            |describeSetOutputTex| |describeSetOutputFortran|
            |describeSetLinkerArgs| |describeProtectSymbols|
            |describeOutputLibraryArgs| |describeSetFortDir|
            |describeSetFortTmpDir| |describeProtectedSymbolsWarning|
            |describeSetStreamsCalculate| |describeSetOutputFormula|
            |describeInputLibraryArgs| |resetWorkspaceVariables|
            |Category| |describeAsharpArgs| |describeSetOutputAlgebra|
            |sayAllCacheCounts| |describeSetFunctionsCache|
            |nangenericcomplex| |createTypeEquivRules|
            |createResolveTTRules| |createResolveTMRules| FRICAS-RESTART
            |bcBlankLine| |browserAutoloadOnceTrigger|
            |scanKeyTableCons| |scanToken| ADVANCE-CHAR |scanEsc|
            |scanError| |scanEscape| |scanNumber| |asharpConstructors|
            |scanString| |scanSpace| |scanPunct| |scanNegComment|
            |startsNegComment?| POP-REDUCTION |scanComment|
            |startsComment?| |scanPunCons| |scanDictCons|
            |resetStackLimits| |npRecoverTrap| |syGeneralErrorHere|
            |DPname| |pfNoPosition| |buildHtMacroTable|
            |checkWarningIndentation| |npDecl| FOAM:|fiGetDebugVar|
            |npType| |npAmpersand| |npName| |npFromdom| |insideCat|
            |spillLine| |optNewLine| /EMBEDREPLY |npSCategory|
            |npPrimary| |npState| |npDefaultValue|
            |npAssignVariableName| |npPDefinition|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) *) $ERASE |foo| |htDoneButton|
            |makeSpadCommand| /RF |/RQ,LIB| |mkGrepPattern1|
            |nothingFoundPage| |dbNotAvailablePage| |htSetCache|
            NEXT-LINE /EF INIT-MEMORY-CONFIG /RQ |newGoGet| |dbShowOps|
            |oPage| |aPage| |emptySearchPage| |conOpPage1| |conPage|
            |kPage| |genSearch| |dbShowCons| |form2HtString| |bcFinish|
            |Undef| META-SYNTAX-ERROR)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) *) |dbSpecialOperations|
            |bcLinearSolveMatrix1| |issueHTStandard| |justifyMyType|
            |stringList2String| |getCallBackFn| |bcString2HyString2|
            |bcDifferentiateGen| |bcwords2liststring,fn|
            |bcIndefiniteIntegrateGen| |htMakeErrorPage| |linkGen|
            |issueHT| |optCallEval| |setOutputAlgebra| |tokType|
            |timedEvaluate| |roundStat| |bracketString|
            |bcDraw2DparGen| |ExecuteSpadSystemCommand|
            |bcDraw3Dpar1Gen| |bcProductGen| |ts| |bcRealLimitGen|
            |bcSumGen| |formatSexpr| |bcDraw3DparGen| |bcDraw3DfunGen|
            |aggwidth| WIDTH |consCommentsTran|
            |bcDefiniteIntegrateGen| |bcInputMatrixByFormulaGen|
            |bcReadMatrix| |systemCommand| |safeWritify|
            |unAbbreviateKeyword| |replacePercentByDollar|
            |brightPrint0AsTeX| |sayDisplayStringWidth|
            |initializeLisplib| |getMsgTag| |poFileName|
            |mac0InfiniteExpansion,name| |NRTtypeHack| |getMsgPos2|
            |outputTran| |replaceSharpCalls| /UNTRACE-0
            |doReplaceSharpCalls| DEFTRAN LIST2STRING DEF-ISNT
            |quoteSuper| |quoteSub| MK_LEFORM MK_LEFORM-CONS |aggSuper|
            |oldParseString| |outformWidth| |aggSub| |agggwidth|
            |agggsuper| |agggsub| |obj2String| |compileFileQuietly|
            |exptSub| |mathPrint| |rootSub| INIT-LIB-FILE-GETTER
            |parseTransform| INIT-FILE-GETTER |overbarWidth|
            |entryWidth| FILE-RUNNER MONITOR-EVALAFTER |editFile|
            |overlabelWidth| |object2String| |readForDoc| /TRACE-0
            |checkNumOfArgs| LENGTH2STR |openServer|
            |removeBackslashes| |checkAddBackSlashes| |matSub| /RF-1
            /MKINFILENAM |docreport| |qTSuper| |qTSub| |sayMSGNT|
            |ExecuteInterpSystemCommand| |pfFileName| |linkToHTPage|
            |InterpExecuteSpadSystemCommand| |killHTPage| |alistSize|
            |startReplaceHTPage| |parseTranList| |startHTPopUpPage|
            |parseLhs| |parseLeftArrow| |parseUpArrow| |parseNotEqual|
            |parseDollarNotEqual| |parseDollarGreaterEqual|
            |parseDollarLessEqual| |parseGreaterEqual| |parseLessEqual|
            |scriptTranRow1| |scriptTran| |scriptTranRow| |npPC| |npPP|
            QUOTE-IF-STRING |numMapArgs| |dbConformGenUnder|
            |listOfEntries| |conformString| |dbConformGen|
            |evalableConstructor2HtString| |halfWordSize|
            |fortFormatCharacterTypes,mkCharName| |opPageFast|
            |fortFormatCharacterTypes,par2string|
            VMLISP::DELETE-DIRECTORY VMLISP::GET-IO-INDEX-STREAM
            |form2StringWithPrens| BPINAME
            VMLISP::GET-INPUT-INDEX-STREAM |prefix2String|
            |form2StringAsTeX| |prefix2StringAsTeX| |asyJoinPart|
            |printLine| |sockSendWakeup| |sockGetFloat| PRINT-LINE
            SOCK-SEND-WAKEUP SOCK-GET-FLOAT |htMakeInputList|
            |popSatOutput| |subrname| SOCK-GET-INT |sayString|
            OPEN-SERVER |protectedEVAL| |setOutputTex|
            |setOutputFortran| |formatElt| |set| |setLinkerArgs|
            |protectSymbols| |protectedSymbolsWarning|
            |setStreamsCalculate| |setOutputFormula|
            |setFunctionsCache| |spadType| |spadSys| |mkGrepFile|
            |mkGrepPattern1,addOptions| |downlink|
            |mkGrepPattern1,remUnderscores| |lispStringList2String|
            BRIGHTPRINT-0 |mkUpDownPattern| |mkUpDownPattern,fixchar|
            |conform2String| |cSearch| |verbatimize|
            |pmParseFromString,flatten| |htCommandToInputLine|
            |dbSpecialExports| |detailedSearch| |docSearch|
            |form2HtString,fnTailTail| |buildLibdbConEntry|
            |form2HtString,fn| |sexpr2HtString| |doItLet1|
            |kInvalidTypePage| |dbSpecialDescription|
            |args2LispString,fnTailTail| |sexpr2LispString,fn|
            |args2LispString| |sexpr2LispString| |sexpr2HtString,fn|
            |spleI| |mkButtonBox| |assignSlotToPred| |doItDef|
            |dbComments| |sockGetInt| |parseAndEvalStr|
            |parseAndEvalStr1| |doItDomain| |dbMkEvalable| |mkEvalable|
            |conPageChoose| KILL-TRAILING-BLANKS |ySearch| |aSearch|
            |close| |kSearch| |compileBoot| |aokSearch|
            |showNamedConstruct| |reportOpsFromUnitDirectly1| |oSearch|
            |tabsToBlanks| |underscoreDollars| |mkGrepTextfile|
            |reportOpsFromUnitDirectly0| |lnFileName|
            |setOutputOpenMath| |pfGlobalLinePosn| |quoteString|
            |postTran| |decodeScripts| |htGloss| |htTutorialSearch|
            |postInSeq| |htTextSearch| |htGreekSearch| |postMakeCons|
            |postCategory,fn| |htShowFunctionPageContinued|
            |htCacheSet| |concatWithBlanks| |htSetFunCommand|
            |NRTevalDomain| |listOfStrings2String| |withAsharpCmd|
            |htCacheOne| |htShowSetTree| |htShowSetTreeValue|
            |postBigFloat| |htSetInteger| |frame| |chkRange|
            |postConstruct| |postSlash| |frameSpad2Cmd|
            |htCacheAddChoice| |addNewInterpreterFrame| |startHTPage|
            |htSetLinkerArgs| |htSetOutputCharacters| |htSetKernelWarn|
            |htSetKernelProtect| |obey| |htSetExpose|
            |htSetInputLibrary| |buildLibdbString| |htSetOutputLibrary|
            |htSetHistory| |condErrorMsg| MONITOR-RESTORE
            |brightPrintCenterAsTeX| |brightPrint0| |dbReadComments|
            |sayWidth,fn| |brightPrintCenter| |poGlobalLinePosn|
            |clearClam| |brightPrintHighlightAsTeX| |incHandleMessage|
            |brightPrintHighlight| |pred2English| |sayDisplayWidth,fn|
            |prefix2String0| |sayDisplayWidth| |form2StringLocal|
            |formatOpType| |form2String1| |ncTag| |ncAlist|
            |tuple2String,f| |bcErrorPage| |formatREDUCE|
            |formatAttributeArg| |formString| |form2String|
            |dbSourceFile| MAKE-REASONABLE |formatRecord| NUMOFARGS
            GET-TOKEN |markSay| |formatFunctionCall| MAKE-DEPSYS
            |formatreduce| |formatForm| |formatUnion|
            |formatFormNoColonDecl| OBEY |numArgs|
            |formatSignatureArgs0| |formatSignatureArgs| |sayWidth|
            SRCABBREVS |exp2FortOptimizeArray| |bcMatrixGen|
            |fortError1| |bcwords2liststring| |fortPre1|
            |bcGenExplicitMatrix| |spadcall1| |bcGen| |fortPreRoot|
            |checkPrecision| |fix2FortranFloat|
            |normalizeStatAndStringify| |mkParameterList,par2string|
            |printAny| |printString| |formatIterator| |summary| |show|
            |showSpad2Cmd| |fixObjectForPrinting| |savesystem|
            |escapeSpecialChars| |encodeItem| |form2LispString|
            |formatEnumeration| |bcSeriesGen| |subspan|
            |bcPuiseuxSeriesGen| |bcLaurentSeriesGen| |superspan|
            |bcSeriesByFormulaGen| |bcNotReady| |bcDraw2DfunGen|
            |bcTaylorSeriesGen| |bcDraw2DSolveGen| |bcComplexLimitGen|
            |saturnPRINTEXP| |bcSeriesExpansionGen| COMPILE-BOOT-FILE
            |replaceGrepStar| |bcCreateVariableString| |grepSource|
            |bcGenEquations| |xSearch| |vConcatSuper| |pSearch|
            BOOT-LOAD |dSearch| |doSystemCommand| |bcSolveNumerically1|
            |standardizeSignature| |bcLinearSolveEqnsGen|
            |conPageFastPath| |bcMakeUnknowns| |bcInputSolveInfo|
            |conPageConEntry| |bcInputEquationsEnd|
            |bcSystemSolveEqns1| |quickForm2HtString|
            |bcLinearSolveEqns1| |dbAttr| |bcVectorGen| |printBasic|
            |pluralize| |parseTran| |subSuper|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) T) IOCLEAR |leadingSubstring?| NILFN RKEYIDS
            $FILEP |bcConform| |categoryParts| |centerNoHighlight|
            INTERRUPT LISP-BREAK-FROM-AXIOM MAKE-DATABASE
            |defaultTargetFE| /DUPDATE /UPDATE /MONITOR |systemError|
            |asCategoryParts| RDEFOUTSTREAM RDEFINSTREAM
            VMLISP::SETQERROR |throwMessage| TOPLEVEL |grepFile|
            |printRecordFile| |wasIs| |htFile2RecordFile|
            |inputFile2RecordFile| |htFile2InputFile| |bcComments|
            |bcNameTable| |dbSayItemsItalics| MONITOR-ENABLE
            |htPred2English| |returnStLFromKey| |interpret|
            MAKE-MONITOR-DATA |formatInfixOp| |level| |Enumeration,LAM|
            LEVEL MONITOR-TESTED |resolveTT| MONITOR-RESET
            |isLegitimateMode| |hasFileProperty| MONITOR-DISABLE
            |coerceConvertMmSelection| |canCoerce| |saveState|
            |selectMms1| |canCoerceFrom| MAKE-TOKEN MAKE-LINE
            |centerAndHighlight| |getOpDoc| MAKE-STACK
            |firstNonBlankPosition| MAKE-XDR-STREAM INITROOT
            |EnumerationCategory,LAM| |Mapping| |RecordCategory,LAM|
            |Union| |restoreState| |UnionCategory,LAM|
            |displayCategoryTable| MAKE-REDUCTION READ-A-LINE
            |dbPresentOps| |buildBitTable| |htBlank|
            |dbMakeContrivedForm| |dcSize| |sum| |args2HtString| |dc|
            |bcNameCountTable| VMLISP::MAKE-LIBSTREAM |nextown1|
            |next1| |markImport| META SAY MOAN |mkWi| VMREAD STREAM-EOF
            |formatPrefixOp| CROAK |format| |incAppend1| |synonym|
            |grepConstruct| |markPrint| VMLISP::LOTSOF |htBeginMenu|
            |formatAtom| |bcCon| |koOps| |dbWriteLines| |formatInfix|
            |catsOf| |getDomainOpTable| |formatPrefix|
            |htInitPageNoScroll| |conSpecialString?| |htSayStandard|
            |domainsOf| |dbPresentCons| |htBcLinks| |pluralSay|
            |getConstructorExports| |sublisFormal| NEXT-META-LINE
            |htLispLinks| META-META-ERROR-HANDLER |dbHeading|
            NEXT-BOOT-LINE |concat| SPAD_SYNTAX_ERROR BOOT |htQuery|
            SPAD |htSayIndentRel| |bcConPredTable| |htSaySaturn|
            |dbSayItems| |simpHasPred| |start| |protectedPrompt|
            |htpMakeEmptyPage| |htMakeButton| |htSayIfStandard|
            |buildLibdb| |htSay| |incZip1| |incIgen1| |incRgen1|
            |runOldAxiomFunctor| |formatDefForm| |incLude1|
            FOAM::MAKE-FOAMPROGINFOSTRUCT |bcPred| |formatWithDef|
            |sayNewLine|)) 
(PROCLAIM '(FTYPE (FUNCTION (T) CHARACTER) LINE-CURRENT-CHAR)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T) FIXNUM) RSETCLASS |rwrite128|)) 
(PROCLAIM '(FTYPE (FUNCTION (T) STRING) |make_spaces| LINE-BUFFER)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) T) |compile,isLocalFunction| |pfTuple|
            |PacPrint| |bottomUp| |dbpHasDefaultCategory?|
            |macSubstituteOuter| |pfPretendExpr| |mkAtreeNode|
            |erMsgSep| |clearConstructorCache| |formJoin2| |keyItem|
            |retract| |stackMessage| |pfMacroRhs|
            |bcLinearSolveMatrixInhomo,f| |isListConstructor|
            |getUnionOrRecordTags| |dbAddChainDomain| |mkConstructor|
            |parseCategory| |pfHide?| |StreamNull| |qModemap|
            |pfDocumentText| |polyVarlist| LIBCHECK |pfPretend?|
            |pfDocument?| |parseDropAssertions| |mkBold|
            |splitListSayBrightly| |listOfIdentifiersIn|
            |bcLinearExtractMatrix| |pfDWhereContext| |incString|
            |postSignature| |formatModemap| |printMap| |parseAtSign|
            |removeQuote| |knownInfo| |isInitialMap| |pfCoercetoType|
            |outerProduct| SPAD-CLEAR-INPUT |showMsgPos?| |printEnv|
            |pfMacroLhs| |parseHasRhs| |transSeq| |killColons|
            |isMapExpr| |bcString2HyString| |macExpand| |parseCoerce|
            |postError| |getTarget| |mkEvalableMapping|
            |pfCoercetoExpr| |getCategoryExtensionAlist0|
            |NeedAtLeastOneFunctionInThisFile| |macLambda|
            |pfSymbolVariable?| |getMsgInfoFromKey| |evaluateType0|
            |tabber| |parseSegment| |pfLambdaArgs| |formatExit|
            |getStFromMsg| |pfSequence| |getUnnameIfCan| |pfCoerceto?|
            |npPileBracketed| |macWhere| |decExitLevel| |formatMI|
            |tabbing| |parseColon| |removeSuperfluousMapping|
            |parseReturn| |ConstantCreator| |formatNonAtom|
            |getMsgLitSym| |initializeSetVariables| |pfTaggedExpr|
            |npAnyNo| |pfApplication?| |pfMLambdaArgs|
            |splitSayBrightly| |dbShowConstructorLines|
            |formatCOLLECT1| |pfTaggedTag| |parsePretend| |postAtom|
            |bcOptional| |getPosStL| |getCategoryExtensionAlist|
            |pfDefinitionLhsItems| |dbName| |pfMacro?|
            |brightPrintRightJustify| |parseType| VMLISP::EQUABLE
            |formatConstruct| |doItIf,localExtras| |pfTagged?|
            |makeSpadConstant| |formatDoCommentLine| |getMsgText|
            |pfIfElse| |RecordInner| |genDeltaSpecialSig| VMLISP::*LAM
            |formatvoid| |mkEvalableUnion| |parseTypeEvaluate|
            |pf0WithWithin| |postType| |formatENUM| |subSub|
            |pfLambda?| |splitSayBrightlyArgument| |isRecord|
            |childrenOf| |pfListOf| VMLISP::RCQEXP |getMsgPrefix?|
            |parseMDEF| |newHasTest,evalCond| |htBcLispLinks|
            |lnString| |mkEvalableRecord| DATABASE-ABBREVIATION
            |postScripts| |flattenOps| |pfWhere?| |sayMSG| |getPreStL|
            DATABASE-ANCESTORS |pfIfThen| |makeOrdinal|
            |parseDollarGreaterThan| NUMOFNODES |typeCheckInputAreas|
            |translateTrueFalse2YesNo| |mac0GetName| |npInfGeneric|
            |parseLETD| |getAndEvalConstructorArgument| |msgOutputter|
            TRANSPGVAR |postWith| |pfLeaf?| |slashWidth|
            |hashTable2Alist| |parseLET| |getIProplist|
            |mkEvalableCategoryForm| |pfWithWithin| |kisValidType|
            |isBinaryInfix| |getMsgTag?| |stuffDomainSlots|
            LINE-AT-END-P |mkAtreeValueOf| |devaluateDeeply|
            |slashSuper| |kCheckArgumentNumbers|
            |collectDefTypesAndPreds| |chkNameList| |pfLeafPosition|
            |poNoPosition?| VMLISP::COMPILE1
            |compDefineFunctor1,FindRep| |markElt2Apply| TRBLANKS
            |isSymbol| |pfAbSynOp| |poImmediate?| |listOutputter|
            |topicCode| INFIXTOK |pfTypedId| |poIsPos?|
            MAKE-STRING-ADJUSTABLE TOKEN-TYPE |transIs| |markPath|
            |formatSignature| |processChPosesForOneLine| |slashSub|
            |freeOfSharpVars| |pf0LambdaArgs| |pfPile| IS-CONSOLE
            |pf0WithBase| |updateSourceFiles| |getExportCategory|
            |unVectorize| |pause| |koCatOps1| |CatEval| |sayMessage|
            |getModeSetUseSubdomain| |hashString| |formatSignature0|
            |npQualified| MKQSADD1 |patternCheck| |transUnCons|
            |dropPrefix| |isInternalFunctionName| SKIP-TO-ENDIF
            |postElt| |getModeSet| DATABASE-CONSTRUCTOR MAKE-CVEC
            |objEnv| |poCharPosn| |npParened| |pfPosn| |formatMapping|
            |npConditional| |posPointers| |canRemoveIsDomain?|
            |NRTgenInitialAttributeAlist| |postSEGMENT|
            |isLegitimateRecordOrTaggedUnion| |getInfoAlist| TRMETA1
            |formIterator2String| |makeMsgFromLine| |lnImmediate?|
            |pfWithBase| |dcCats1| |removeIsDomains|
            |stackMessageIfNone| THETA_ERROR BVEC-COPY
            MAKE-ABSOLUTE-FILENAME |listOfDuplicates| |getLookupFun|
            |formatAttribute| |letWidth| |mkRationalFunction|
            |postIteratorList| |axOpTran| |parseLeave|
            |formTuple2String| VMLISP::FLAT-BV-LIST MACROEXPANDALL
            |pfPosImmediate?| |npEqPeek| PREPARSEREADLINE FUNARGP
            |isCategoryPackageName| |isPolynomialMode| MKQ
            |listOfCategoryEntries| |ncParseAndInterpretString|
            |numOfSpadArguments| |npElse| |erMsgSort| |postForm|
            |pfSourceToken| |pfPrintSrcLines| |mkDatabasePred|
            |args2Tuple| |sortCarString| |isAVariableType|
            |mkCategory,Prepare| |niladicHack| |blankList|
            |translateYesNoToTrueFalse| |msgNoRep?| |equiType|
            TRY-GET-TOKEN |dbGatherDataImplementation,fn| |pfAppend|
            |getPrincipalView| |htAllOrNum| |axFormatOpList|
            |pfWithWithon| |removeBodyFromEnv| |npMissing| |To|
            |pfFirst| |postOp| |NRTcatCompare| |form2StringWithWhere|
            VMLISP::PLIST2ALIST |hitListOfTarget| HKEYS
            |dbGatherDataImplementation,gn| |reportOpSymbol|
            PREPARSEREADLINE1 SUBANQ |stringize| |template| |apropos|
            |From| |pfNot| TOKEN-SYMBOL |exp2FortOptimizeCS|
            |formatModemap,fn| |pfFix| |npDDInfKey| |domainDepth|
            |postTuple| |postExit| TRIMSTRING |listOfVariables|
            |parseWord| |isType| SKIP-IFBLOCK |outputConstructTran|
            |NRTgetLocalIndexClear| |axFormatOp| |bottomUpElt|
            |constructSubst| |pfAttributeExpr| |markKillButIfs|
            |isFreeVar| |pfTyping| |containsVars| REROOT |dcAtts|
            |retractAtree| |pfAttribute?| COMP370 |dcSlots|
            |bottomUpPercent| |postMapping| |outputTranSEQ| TRMETA
            |evalMmDom| |optcomma| |fetchOutput| |isLocalVar|
            |postMDef| DIG2FIX |tokPart| |outputTranRepeat|
            |pfAttribute| |abbreviationsSpad2Cmd| GETREFV |dcOpTable|
            |expr2String| |outputTranReduce| |pfLoop|
            |formatUnabbreviatedSig| |postDef| |displayTranModemap|
            |log| |isInternalMapName| |npInfKey| UPCASE
            |optFunctorBody| |getConstructorArgs|
            |bottomUpUseSubdomain| LN |pfDo| |optimize| |npRestore|
            DEF-WHERECLAUSELIST MBPIP |atom2String| |emptyAtree|
            |namestring| STATEP |npWith| |templateParts|
            |makeInternalMapMinivectorName| |mymy| QSORT
            MATCH-ADVANCE-STRING PLACEP |outputTranCollect|
            |pfWDeclareDoc| |dqToList| |pfId| |postCategory|
            |isFreeFunctionFromMmCond| |dqConcat| |aplTran|
            |isSharpVarWithNum| |outputMapTran| |isHomogeneousList|
            |pfSecond| |isUncompiledMap| |pfWDeclareSignature|
            |cleanUpSegmentedMsg| |dbNewConname| |getBasicObject|
            |pfWDeclare?| |pfTupleParts| |containsBang| |npSemiListing|
            |printMms| NOTE FBPIP |getSymbolType| |htMakePathKey|
            |pfCheckInfop| |escapeString| |bottomUpCompile| |postJoin|
            |isFreeFunctionFromMm| /UNTRACE-REDUCE |npWConditional|
            |nodeSize| |mkAlistOfExplicitCategoryOps|
            |pf0CollectIterators| |fortexp0| |matchMmCond|
            |postTransformCheck| LINE-P |pfExport| |object2Identifier|
            |npBraced| |mkAlistOfExplicitCategoryOps,atomizeOp|
            |selectMostGeneralMm| |vectorSize| |pfLocal|
            |fixUpTypeArgs| |chkAllNonNegativeInteger| |pfExport?|
            |pfWhereContext| |getBasicMode| |handleLispBreakLoop|
            |myLastAtom| TRACEOPTIONS PROPLIST |postIf| |unwrap|
            |evalMmStack| |optSEQ,getRidOfTemps| EBCDIC
            RECOMPILE-DIRECTORY |chkNonNegativeInteger| |pfDeclPart?|
            |isDefaultPackageForm?| |optSPADCALL| SHOWBIND
            |postPretend| |optimizeFunctionDef| |numberOfNodes|
            |upREPEAT1| PREPARSE-ECHO |isWrapped| |pfFree|
            |npCompMissing| |pfDWhere?| |reverseCondlist| DROPENV
            |pfImport?| |pfId?| |length1?| GETZEROVEC |makeDefaultDef|
            |dcOps| |formatATTRIBUTE| |optXLAMCond| UNVEC UNEMBED
            |pfTyping?| ATENDOFUNIT |containsPolynomial| |formatPAREN|
            |noSharpCallsHere| |postAtSign| PARSEPRINT |pfSuchthat|
            |htMakePage1| |removeAttributePredicates,fn|
            |getModeOrFirstModeSetIfThere| |untraceDomainConstructor|
            |optCONDtail| |old2NewModemaps| |string2OpAlist|
            |pfComDefinition?| |npBracketed| |isDomain| |pfTLambda?|
            |removeAttributePredicates,fnl| |genDeltaSig| |upREPEAT0|
            |optPredicateIfTrue| |pfWhile| |htProcessDoitButton|
            FOAM:|formatDFloat| |getFunctionSourceFile| |optCons|
            RE-ENABLE-INT |pfAdd?| |npAdd| |InvestigateConditions|
            FOAM:|formatSFloat| |isMap| |optSEQ| NUM2CHAR
            |pf0ExportItems| |wrapMapBodyWithCatch| HACKFORIS1
            PREPARSE1 FOAM:|formatBInt| |pfBreak| HACKFORIS
            |displayModemaps| |blankLine?| |pfExportItems|
            FOAM:|formatSInt| |containsVars1| |pfExpr?| |uplocal|
            |orderMmCatStack| |optSEQ,tryToRemoveSEQ| CHARP IVECP
            |htProcessDoneButton| |evalMmStackInner| LIST2VEC
            |getDefaultingOps| |htpRadioButtonAlist| |pfWith?|
            |fortFormatElseIf| DEF-IN2ON |containsVariables| |topics|
            |getOpSegment| MONITOR-DATA-COUNT |pfCheckArg|
            |NRTsubstDelta,replaceSlotTypes| LISTOFQUOTES
            |pf0TypingItems| |htProcessBcButtons| |wrapped2Quote|
            |new2OldTran| |removeIsDomainD| |pfTypingItems|
            |objCodeVal| |resolveTypeList| |topLevelInterpEval|
            |objCodeMode| |tdPrint| |newConstruct| |pfGetLineObject|
            RPACKFILE MONITOR-DATA-NAME |InvestigateConditions,reshape|
            |newIf2Cond| |formatSignatureAsTeX| |asyUnTuple|
            |newDef2Def| |firstTokPosn| |asyTypeUnitList| |asTupleNew0|
            |lnFileName?| |parseOr| DEF-MESSAGE1
            RECOMPILE-LIB-FILE-IF-NECESSARY |htProcessToggleButtons|
            |parseIf| |asyComma?| LIST2STRING1 |htpDomainPvarSubstList|
            |parseImplies| DEF-WHERE |interactiveModemapForm|
            |parseEquivalence| |upfree| DEF-SEQ |isTaggedUnion|
            |slot1Filter| SEQOPT |pfCheckId| LIBSTREAM-DIRNAME
            |asIsCategoryForm| |parseAnd| DEF-IS
            MONITOR-DATA-SOURCEFILE |htProcessDomainConditions|
            |helpSpad2Cmd| |opOf| |checkInteger| |indentFortLevel|
            DEF-EQUAL |profileTran| |getConstructorSignature|
            |asySubstMapping| |NRTsubstDelta| DEF-MESSAGE PNAME |spool|
            MONITOR-DELETE |parseNot| |upREPEAT| |setOutputCharacters|
            |axFormatType| |sayAsManyPerLineAsPossible|
            |asyTypeMapping| DEF-CATEGORY |?modemaps|
            |pfSequenceToList| |extractHasArgs| |asyCATEGORY|
            DEF-REPEAT MAKE-BVEC /VERSIONCHECK MONITOR-DATA-MONITORP
            |read| |encodeCatform| DEF-COND |getDefaultProps|
            |pfSequenceArgs| |getPossibleViews| |asyShorten| DEF-LESSP
            |sayRemoveFunctionOrValue| |htInputStrings| |readSpad2Cmd|
            SMINT-ABLE |renamePatternVariables| GET-A-LINE
            |displayMacros| |createAbbreviation| |fortFormatIf|
            DEF-COLLECT |pfSequence?| |getConstructorDocumentation|
            FOAM:|Halt| |astran| |evalableConstructor2HtString,unquote|
            |pfQualTypeQual| |pfNovalueExpr| |htBcRadioButtons|
            |asMakeAlist| DEF-STRING |parseExclusiveOr| |pvarCondList|
            MONITOR-EXPOSEDP |warnLiteral| |asyParents|
            |orderByContainment| |Zeros| |getConstructorModemap|
            DEF-SETELT |addConsDB| |pfNovalue?| |ICformat|
            |asyDocumentation| |stripOutNonDollarPreds| DEF-RENAME1
            CACHEKEYEDMSG KILL-COMMENTS |porigin| GCOPY
            |asyConstructorModemap| DEF-ELT XDR-STREAM-HANDLE
            |makeTypeSequence| |markKillAll| |pfTupleListOf| |asytran|
            |upDEF| |DEF-:| |asyPredTran| |isHasDollarPred|
            |topicCode,fn| DEF-ADDLET |htpDomainVariableAlist|
            |makeArgumentIntoNumber| |InvestigateConditions,mkNilT|
            |koAttrs,fn| |asyPredTran1| |htRadioButtons|
            |axFormatAttrib| |pfNotArg| |pfQualTypeType| |as|
            |markPath1| |checkTrim,trim| MONITOR-APROPOS
            |displayOperations| |pfNot?| XDR-STREAM-P |categoryForm?|
            |transHasCode| |libConstructorSig| |asytranLiteral|
            |changeDirectoryInSlot1,fn| |parentsOf| MONITOR-DATA-P
            |asytranEnumItem| |axFormatCondOp| |checkDocError|
            |listOfTopics| |constructor?| |bootFind| |htLispMemoLinks|
            |hackToRemoveAnd| |libConstructorSig,fn|
            |asyGetAbbrevFromComments| |foobum| |pfQualType?|
            |checkTrim,wherePP| PRINT-RULE |intern| |ppEnv|
            |checkDecorateForHt| |asyTypeJoinPartPred|
            |removeAttributePredicates| |npProcessSynonym|
            |zeroOneConversion| XDR-STREAM-NAME |listOfSharpVars|
            |asyArgs| |getCatAncestors| |pfLinePosn| |checkRecordHash|
            |compileAsharpLispCmd| |asyArg| |makeCompactDirect1,fn|
            |asyFindAttrs| |checkIsValidType| |asyAncestors|
            |axFormatPred| |asyAncestorList| |asyTypeJoinItem| SHUT
            |pfCharPosn| SETLETPRINTFLAG |fileConstructors|
            |isLowerCaseLetter| |isAlmostSimple| |depthAssoc|
            |pfImmediate?| |abbreviation?| |libdbTrim| SOURCEPATH
            |asAll| |checkGetParse| COMP |error| |isAlmostSimple,fn|
            |depthAssocList| |listOrVectorElementMode|
            |checkGetStringBeforeRightBrace| |asyTypeJoinPartIf|
            |isFunctor| |asyType| |checkGetLispFunctionName|
            |asyTypeJoin| MAKE-DIRECTORY |untraceMapSubNames| GETGENSYM
            |stripLisp| |asyTypeJoinPartExport|
            |markCheckForAttributes,fn| |asyCattranOp| |fromHeading|
            |mapPredTran| |checkLookForRightBrace| |wordDelimiter?|
            |pfNoPosition?| |predicateBitRef| |parentsOfForm|
            |htAddHeading| |getUnderModeOf| |isSideEffectFree|
            |asyMkpred| |%pos| |checkLookForLeftBrace|
            |makeDefaultArgs| |checkFixCommonProblem|
            FOAM::PROCESS-IMPORT-ENTRY |infovec| |stripType| |ltrace|
            |asyLooksLikeCatForm?| |zeroOneConvertAlist|
            |checkArguments| |deconstructT| |asyCosigType| |dqUnitCopy|
            |mkAliasList| |checkTexht| |dbMkForm| |attribute?| |trace|
            |asMakeAlistForFunction| |dqUnit| |optFunctorPROGN|
            |dbInfoSig| |compileAsharpCmd| TRARGPRINT |modemapToAx|
            MSORT |getAttributesFromCATEGORY| |formatApplication|
            |%fname| |worthlessCode| |isDefaultPackageName|
            |formatApplication0| |reduceImports1| |displayProplist,fn|
            |mySort| |makeLazyOldAxiomDispatchDomain| |pfname|
            |optFunctorBody,CondClause| |getEqualSublis| |yumyum|
            |eqType| |mkNiladics| |unknownTypeError| |optCall|
            |removeEnv| |%origin| |explodeIfs| |pfAssignRhs|
            |numberize| |formatSelection| |folks| |formatHasDotLeadOp|
            |mkRepititionAssoc| |load| |hasNewInfoText| |myWritable?|
            |loadSpad2Cmd| |asyExtractDescription| |asyCattran1|
            |getInfovec| |dbReadLines| |simpCattran|
            |pf0AssignLhsItems| |predTran| |asyCattran| ?VALUE
            |asyCatItem| |fnameReadable?| |help| |?value|
            |asyExportAlist| |pfAssign?| DATABASE-P |quoteWidth|
            |hasDefaultPackage| |trimComments| DEF-INSERT_LET1
            FOAM:|printNewLine| |bootAbsorbSEQsAndPROGNs| |quotify|
            |boxSuper| |splitConTable| |displayDatabase|
            |formatIfThenElse| |getTargetWI|
            |bootAbsorbSEQsAndPROGNs,flatten| |pfDoBody| |compFailure|
            |markOrigName| |bootTran| |reportHashCacheStats|
            DEF-WHERECLAUSE |asyConstructorArg| DEF-STRINGTOQUOTE
            |fnameType| |bootLabelsForGO| MONITOR-DIRNAME |boxSub|
            |markLhs| DEF-INSERT_LET |dbGetDocTable,gn|
            |setExtendedDomains| |getCommons| |bootPROGN|
            |spreadGlossText| |pfDo?| LIST2CONS-1 |formatFATARROW|
            ?PROPERTIES |asyTypeMakePred| |bootTransform|
            |simplifyMapConstructorRefs| |formatIf1| |bootSEQ|
            |mkHashCountAlist| |concatWidth| |isTrue| |string2Integer|
            |tryToRemoveSEQ| DEF-IS-REMDUP1 |formatTuple| |nakedEXIT?|
            |asyExtractAbbreviation| |altSuperSubWidth| |code2Classes|
            |asyConstructorArgs| |altSuperSubSuper| |ppFull|
            |mergeCONDsWithEXITs| |concatbWidth| |getGlossLines|
            |pfSuchthatCond| LIST2CONS |fnameName| SET-PREFIX |foobar|
            |?properties| |asyTypeJoinStack| |altSuperSubSub|
            PRINT-FLUIDS |markInsertParts| |bootCOND| DEF-IS-REMDUP
            |StringToDir| |markKillExpr| |asyTypeUnit| |concatSuper|
            |formatRB| |isCloseDelimiter| |bootAND| DEF-IS-EQLIST
            |spad2AxTranslatorAutoloadOnceTrigger| |td|
            LINE-ADVANCE-CHAR |concatSub| |recordFrame| |breakComments|
            |getParentsForDomain| |new2OldDefForm| |issueHTSaturn|
            |formatPreferPile| |unabbrev| |string2PrintImage|
            DATABASE-COSIG |prTriple| |kTestPred| |binomWidth|
            |htEndMenu| |prModemaps| |bootOR| |binomSuper|
            |rhsOfLetIsDomainForm| |?m| GET-META-TOKEN |isGensym|
            |addFieldNames| |asyTypeJoinPartWith| |optSEQ,SEQToCOND|
            DEF-SELECT |segmentKeyedMsg| |formatImport|
            |hasNewInfoAlist| |bootIF| |asyTypeItem|
            |formatSelectionOp| |asyCosig| |binomSub| |addCommas|
            |formatSelectionOp1| |bootAND,flatten| COMP-TRAN-1
            |source2Number| |importFromFrame|
            |bootPushEXITintoCONDclause| PUSHLOCVAR |modemap2SigConds|
            |stop| |decExitLevel,removeExit0| |optimize,opt|
            COMP-EXPAND |addTraceItem| |ifize| |formatIF3| |unErrorRef|
            |moveTruePred2End| |inSuper| |getSubstCandidates| |unLet|
            GET-BSTRING-TOKEN |boolBin| |formatSIGNATURE|
            |htpPageDescription| REMDUP |asyIsCatForm| COMP-NEWNAM
            |doItLet| |optCond| |untraceAllDomainLocalOps|
            |bootOR,flatten| |formatNextConstructItem| COMP-TRAN
            |doItSeq| |dbDocTable| |bright| |asCategoryParts,exportsOf|
            |inSub| |htSayExplicitExports| |fortran2Lines|
            |removeEXITFromCOND| |pfReturnNoName|
            |fortFormatCharacterTypes| COMP-FLUIDIZE GET-STRING-TOKEN
            |flattenCOND| |optMkRecord| |addInputLibrary| |saturnTran|
            |inWidth| |holdIt| |uppretend| |bcUnixTable|
            |formatUnabbreviated| |extractCONDClauses|
            |npListAndRecover| |dropInputLibrary| |corrupted?|
            |mkTabularItem| |openOutputLibrary| |hashable|
            |moveORsOutside| |opPageFastPath| |trimString|
            |formatCATEGORY| |optCatch| |stepSuper|
            GET-IDENTIFIER-TOKEN |mergeableCOND|
            |formatDeftranCategory| |npTuple| BVEC-NOT |knownEqualPred|
            |pf0SequenceArgs|
            |fortFormatCharacterTypes,mkParameterList2|
            TOKEN-LOOKAHEAD-TYPE |removeEXITFromCOND?|
            |compileTimeBindingOf| |outputTranMatrix| |printAsTeX|
            |orderBySlotNumber| |fracwidth| |typeOfType|
            |zeroOneConvert| |optimizeFunctionDef,removeTopLevelCatch|
            |isAsharpFileName?| |formatBy| |stepSub| |domainForm?|
            |exp2FortOptimizeCS1,popCsStacks| |compQuietly|
            |isMenuItemStyle?| |makeByteWordVec| |getSourceWI|
            |listOfPatternIds| |saturnTranText| |exp2FortOptimizeCS1|
            |markStep| |optEQ| |kFormatSlotDomain,fn| |checkLines|
            |unabbrevAndLoad| |sockGetStringFrom| |fracsuper|
            |optLESSP| |fortFormatTypes,unravel| |pfIterate| |bcError|
            |traceSpad2Cmd| |markMacroTran| |abbQuery| |opt-|
            |uptypeOf| |compArgumentConditions| |optQSMINUS| COMP-1
            |transOnlyOption| |formatCapsuleFunction| |pfLoop1|
            |getOplistForConstructorForm| |kPageContextMenu|
            |formatStepOne?| |stepWidth| |optMINUS| |formatSlotDomain|
            |bcString2WordList| |statement2Fortran| COMP-2 |maprin0|
            |optSuchthat| |getSubstSignature| |unTab1| |trace1|
            |setAutoLoadProperty| |compAndDefine| |parseGreaterThan|
            |shortenForPrinting| LINE-PRINT
            |getConstructorUnabbreviation| |abbreviate| |displayLines1|
            |saveMapSig| |getLisplibName| |fracsub| |optRECORDCOPY|
            |getfortexp1| |getBpiNameIfTracedMap| OPTIMIZE&PRINT
            |exptSuper| |optSETRECORDELT| LINE-PAST-END-P
            |getPartialConstructorModemapSig| |mathPrintTran|
            |recordAndPrintTest| |expression2Fortran| |COMP,FLUIDIZE|
            |fortran2Lines1| |maximalSuperType| |npParse|
            |mkTabularItem,fn| |upQUOTE| |formatMap|
            |categoryParts,exportsOf| |PullAndExecuteSpadSystemCommand|
            |untrace| |getImmediateSuperDomain| |exptWidth|
            |timedEVALFUN| |koOps,trim| |htNewPage|
            |augmentLowerCaseConTable| |rootWidth| |isPatternVar|
            |htpName| |dispStatement| |stripOffArgumentConditions|
            |isNameOfType| |pfDocument| |with| |prTraceNames,fn|
            |makeSimplePredicateOrNil| |objMode| |getImpliedImports|
            |exptNeedsPren| DATABASE-CONSTRUCTORKIND
            |isDomainValuedVariable| |markPathsMacro| |minusWidth|
            |updateTimedName| |htMakePageSaturn| SPAD_ERROR_LOC
            |packageForm?| |ppf| |simpHasPred,eval| |getTraceOptions|
            |sayMSG2File| |isCapitalWord| |concatList|
            |simpHasPred,simp| |mkMessage| |pfTweakIf|
            |compressHashTable| |dispfortexp1| |zagSuper|
            |fnameDirectory| |fortranCleanUp| |clearCache|
            |timedOptimization| |maprin| |displayBreakIntoAnds|
            |IdentityError| |simpOrUnion| |height| |transTraceItem|
            |pfCheckItOut| |concatTrouble,fixUp| |zagSub|
            |loadIfNecessary| |clearCategoryTable| |inputPrompt|
            |simplifyMapPattern,unTrivialize| |mathprint|
            |timedAlgebraEvaluation| |genSearchTran| |timesWidth|
            |flattenOperationAlist| |pushSatOutput| |rootSuper|
            |variableNumber| |pushTimedName| |spadTrace,g|
            REDUCTION-VALUE |exp2FortOptimize| |fracpart|
            |transCategoryItem| |mkPredList,fn| |extendLocalLibdb|
            |negintp| |closeInterpreterFrame| |significantStat|
            |interactiveModemapForm,fn| |isTraceGensym| |parseCases|
            |deleteFile| |removeSurroundingStars| |fortPre| |intpart|
            |printNamedStats| |largeMatrixAlist| |compileAsharpCmd1|
            |DirToString| |getTraceOption| |optRECORDELT| |sumWidth|
            |htPopSaturn| |optIF2COND| LOG10 |last|
            |htMakePageStandard| C-TO-R |htpDestroyPage| |aggWidth|
            TOKEN-PRINT |checkFilter| |incRgen| C-TO-S |pfOrRight|
            |undo| |isPatternArgument| S-TO-C |splitIntoBlocksOf200|
            |zagWidth| |getConstrCat| PREPARSE |tokTran| |pfOrLeft|
            |getMapSubNames| |incIgen| |dbKind|
            |getPreviousMapSubNames| MONITOR-LIBNAME |pi2Width|
            |htQuote| CGAMMA ?MODE RGAMMA |parseSystemCmd| |str2Tex|
            |pfOr?| |LZeros| CLNGAMMA RLNGAMMA |wrap| MONITOR-FILE
            |signatureTran| |isConstantArgument| |getDomainOps| |?mode|
            |showGoGet| |pfAndRight| LINE-CURRENT-SEGMENT
            |showAttributes| |coerceSpadArgs2E| |showPredicates|
            |pfAndLeft| |frameName| |clear| |showSummary|
            |mkCategoryExtensionAlistBasic| |getExtensionsOfDomain|
            |dumbTokenize| |str2Outform| |pfAnd?| |getDomainSeteltForm|
            |whatConstructors| |edit| |stupidIsSpadFunction|
            |getCategoriesOfDomain| |destructT| |parse2Outform|
            MONITOR-SPADFILE |objValUnwrap| |sayBrightlyLength|
            |getDomainExtensionsOfDomain| |bnot| |userError|
            |getDomainsInScope| STACK-CLEAR |stackTraceOptionError|
            |editSpad2Cmd| |clearAllSlams| |compRenameOp| |notDnf| |pp|
            |pfWrong?| |macrop| |yyyyy| |htMakePage| GET-BOOT-TOKEN
            |b2dnf| |displayComp| |showCategoryTable| |ordList|
            |mkErrorExpr| MONITOR-PARSE |bor|
            |getDefaultPackageClients| |band| |pi2Sup| |PARSE-LedPart|
            |reportOpsFromUnitDirectly| |bassert| |evalLoopIter|
            |compOrCroak1,compactify| |markKillAllRecursive| |notCoaf|
            |pi2Sub| |formatUnabbreviatedTuple| |pf0LocalItems|
            |markWhereTran| |coerceSpadFunValue2E| |formatELT| |list3|
            |displayOperationsFromLisplib| |convertSpadToAsFile|
            |formatSEGMENT| |list2| |overbarSuper| |length2?|
            MONITOR-DECR |xxxxx| |htpPropertyList| |formatCOND| |list1|
            |say2PerLine| |Identity| |searchCount| |dnf2pf|
            |getArgumentConstructors,fn| |upADEF| |pfLocal?| |be|
            |formatSCOND| |getArgumentConstructors,gn| |outputOp|
            |bool| GET-SPECIAL-TOKEN |formatQUOTE| |reduceDnf|
            |compiler| |pfNovalue| |analyzeMap,f| |domainToGenvar|
            |formatCONS| |bassertNot| |IS_#GENVAR| |resolveTMRed1|
            |prove| |resolveTTRed3| |pf0FreeItems| |testPredList|
            |formatWHERE| |display| |fnameWritable?| LISTOFATOMS
            |npItem1| |PARSE-NudPart| |formatREPEAT| |nodeCount|
            |orderList| |formatCOLLECT| |mkCircularAlist|
            |clearSlam,LAM| |displaySpad2Cmd| |pfFree?|
            DATABASE-CONSTRUCTORFORM MONITOR-EVALBEFORE
            |searchDropUnexposedLines| |getCacheCount| |upLoopIters|
            |tcheck| |pfRestrictType| |PARSE-Expr| GET-SPADSTRING-TOKEN
            |makeCompactDirect| NMSORT |interpOp?| |htSayTuple|
            |hashCount| |frameEnvironment| |pr| |pathnameName|
            MONITOR-INCR |bcHt| LASTATOM |pathnameDirectory|
            |pfRestrictExpr| |compileAsharpArchiveCmd|
            |parseAndEvalToHypertex| |interpIter| |getIteratorIds|
            |oldParseAndInterpret| |npLetQualified|
            |parseAndInterpToString| |getArgumentConstructors|
            |functionp| SPADSYSNAMEP |isConstructorForm| |parseJoin|
            |parseAndEvalToStringEqNum| |buildLibAttrs| |quoteCatOp|
            |pfRestrict?| |genDomainTraceName| |dcPreds| |setHistory|
            |compileSpad2Cmd| GET-NUMBER-TOKEN |isLetter|
            MONITOR-BLANKS |library| |parseJoin,fn|
            GET-ARGUMENT-DESIGNATOR-TOKEN |setExposeAddGroup|
            |htSayArgument| |mkNestedElts| |piWidth| MONITOR-NRLIB
            |getUserIdentifiersInIterators| |setFortDir|
            |charRangeTest| |spadReply,printName|
            |validateOutputDirectory| |instantiate| |pfDefinition?|
            |htpInputAreaAlist| |parseIsnt| |setOutputLibrary|
            |isUpperCaseLetter| WHOCALLED |abbreviations|
            |charyTopWidth| |getUserIdentifiersIn| |bubbleType|
            |parseBigelt| |setExposeDropConstr| |flattenSexpr|
            |putWidth| |PARSE-GliphTok| |setExposeDropGroup|
            CONSOLEINPUTP |piSup| OPTIONS2UC |kePageOpAlist|
            |setExposeDrop| |isStreamCollect| |getTraceOption,hn|
            |setFortTmpDir| |removeZeroOneDestructively|
            |fileNameStrings| |parseIs| |setExposeAdd|
            |DropImplementations| BOOT-TOKEN-LOOKAHEAD-TYPE |setExpose|
            |StringToCompStr| |undoCount| |parseInBy| |setInputLibrary|
            |boolODDP| |overlabelSuper| |stringer| |inclmsgCannotRead|
            |setAsharpArgs| |changeToNamedInterpreterFrame| LISTOFFREES
            |outputTranIf| |sayFORMULA| |countCache| |pathnameType|
            MAKE-SYMBOL-OF |cgamma| IS_GENVAR |rightTrim|
            |htInitPageNoHeading| |rgamma| GENSYMP |spadThrowBrightly|
            |undoLocalModemapHack| |removeUndoLines| |clngammacase3|
            |saturnHasExamplePage| ASSOCRIGHT |cgammaBernsum|
            |dropLeadingBlanks| |NRTgetOperationAlistFromLisplib|
            /OPTIONS |reportUndo| STACK-SIZE |cgammaAdjust| OPTIONAL
            |getDomainByteVector| |iht| |interpOnlyCOLLECT|
            |histFileErase| |lnrgammaRatapprox| |bcIssueHt|
            |phiRatapprox| |pathname| |upCOLLECT| |histInputFileName|
            |lnrgamma| |upAlgExtension| PAPPP |flattenSignatureList|
            STACK-STORE |gammaRatapprox| |collectAndDeleteAssoc|
            |formatDOLLAR| |eq2AlgExtension| BRIGHTPRINT
            |gammaRatkernel| |gammaStirling| |readHiFi| |parseIn|
            |checkSplitBrace| |getFirstArgTypeFromMm| |bcConform1|
            |restoreHistory| |PsiIntpart| |keyp|
            |checkSplitPunctuation| |isFilterDelimiter?| SEC
            |clearCmdParts| STACK-UPDATED |checkSplitOn|
            |mkDetailedGrepPattern,simp| |upCOLLECT0| |cgammat|
            |changeDirectoryInSlot1,sigloc| |loadLib| |lisp2Boot|
            |bcConform1,hd| |makeDomainTemplate| |FindFundAncs|
            |checkSplitBackslash| |isDefaultOpAtt| |upCOLLECT1|
            |binomialWidth| STACK-POP |replaceTicksBySpaces| |upand|
            |parseHas| COT |htSaySourceFile| |formatBrace|
            |checkAlphabetic| |conform2OutputForm| |upDeclare|
            |formatWithDefault| |basicStringize| |parseHas,mkand|
            |isDomainSubst| |lncgamma| |NRTreplaceAllLocalReferences|
            |mkZipCode| |clearSpad2Cmd| |dbGetName| |orderCatAnc|
            |mapStringize| |pfTupleList| |mkSlot1sublis| |getToken|
            |TruthP| |pfWIfElse| |binomialSuper| |isOkInterpMode|
            |makeHistFileName| |bcConform1,mapping| |parseHas,fn|
            |pfWIfThen| |NRTputInLocalReferences| SECH
            |changeHistListLen| |outputTranMatrix,outtranRow|
            |pfLeafToken| |mkGrepPattern1,addWilds| ACSCH |showHistory|
            |parseExit| |pfWIfCond| DATABASE-CONSTRUCTORCATEGORY ACOTH
            |setIOindex| |isCategory| |NRTputInHead| ASECH
            |pfLiteralClass| |pfWIf?| |saveHistory| |NRTcheckVector|
            |mkGrepPattern1,g| |PARSE-NBGliphTok| |bcConform1,tuple|
            DATABASE-OBJECT |organizeByName| |dewritify,dewritifyInner|
            |pfAssignLhsItems| |formatIteratorTail| |setHistoryCore|
            UNDERSCORE |pfRetractToType| |binomialSub| |charDigitVal|
            |getFieldNames| |collectComBlock| |getTempPath|
            |dewritify,is?| |vConcatWidth| |formatOp| |pfSexpr|
            |formatComma| DATABASE-MODEMAPS BOOTTRAN::BOOTTOCL
            |writify| |formatCut| |formatWithKillSEQ| |getDomainFromMm|
            |NRTmakeSlot1| |formatTail1| |history| /MDEF
            DATABASE-OPERATIONALIST |looksLikeDomainForm| |buildLibOps|
            |gensymInt| STACK-TOP |markTranJoin| |dewritify|
            |bcConform1,tl| |pfRetractToExpr| DATABASE-DEPENDENTS
            |NRTisExported?| |formatTail| |markTranCategory| |deMatrix|
            |pfNopos?| TRANSLIST |addComment| |markCheckForAttributes|
            |lnExtraBlanks| |splitIntoOptionBlocks| DATABASE-USERS
            |makePredicateBitVector| |replaceCapsulePart| |sumWidthA|
            |consLineBuffer| DATABASE-PARENTS |formatConstructItem|
            |markConstructorForm| TOKEN-NONBLANK |mkCommentLines|
            |pfRetractTo?| |formal2Pattern| |writedb| TRANSLATE
            |undoFromFile| |pfPlaceOfOrigin| |tokPosn| |pfExpression?|
            FLOATEXPID |htSayItalics| |getFirstWord|
            |finalizeDocumentation,hn| MONITOR-CHECKPOINT
            |dbGetDocTable,hn| |formatApplication2| |pileColumn|
            |genSearchUniqueCount| |pfWhileCond| |spadClosure?|
            |formatPren| |pf0FlattenSyntacticTuple| STACK-P
            |underDomainOf| |pfSexpr,strip| |absym| |isSharpVar|
            |bustUnion| |ravel| DATABASE-PREDICATES |formatUNCOERCE|
            |writify,writifyInner| |pmPreparse| |poPlaceOfOrigin|
            |undoChanges| |underDomainOf;| HAS_SHARP_VAR |dcData1|
            |undoInCore| |fragmentsToLine| |formatMDEF|
            |dbUnpatchLines| |getSlot1| |formatHasDollarOp|
            |evaluateLines| LINE-NEXT-CHAR |pileCforest|
            |isTypeProbably?| |formatFree| |writifyComplain|
            REDUCTION-RULE |%id| |markSpliceInChanges| |formatDot|
            |unwritable?| |pfFileName?| |verifyRecordFile|
            DATABASE-ATTRIBUTES |dbDoesOneOpHaveParameters?|
            |dbSpecialDisplayOpChar?| |dbHasExamplePage| |NRTaddInner|
            |pfWhile?| |enPile| |isExistingFile| |checkExtractItemList|
            |sayDocMessage| |pfForinWhole| |mkDomainConstructor|
            |poFileName?| |recordHeaderDocumentation| |ppTemplate|
            |pfIfCond| |eject| |removeAttributes| |parseAndEval|
            |recordAndPrintTest,fn| |separatePiles|
            |outputDomainConstructor| |formatDefaultDefs| |isLeaf|
            |checkIeEgfun| |pmParseFromString| |piSub| |brightPrint1|
            |srcPosDisplay| |getDomainHash| |appendOver|
            DATABASE-DOCUMENTATION |pf0ForinLhs| |srcPosColumn|
            |formatLocal| |pilePlusComments| |optFunctorBodyRequote|
            |conLowerCaseConTranTryHarder| |mkHasArgsPred| |aplTran1|
            |typeTimePrin| SET-FILE-GETTER |transformOperationAlist|
            |fnameExists?| |lefts| |upreturn| |pfCheckMacroOut|
            |srcPosSource| |hasAplExtension| |htTrimAtBackSlash|
            |formatCOMMENT| |pilePlusComment| |findEqualFun|
            |dbOuttran| |markPrintAttributes| /UNEMBED-Q
            |isSomeDomainVariable| |mkList| |htpDomainConditions|
            |setExposeAddConstr| |formatSEQ| |insertpile| PE
            |markPrintAbbreviation| |uperror| |pfForin?| |sayNonUnique|
            |pfInlineItems| |formatDEF| |dbBasicConstructor?| |pfIf?|
            |formatLocal1| |compDefWhereClause,removeSuchthat|
            |aplTranList| |lfnegcomment| |formatLET| |lastTokPosn|
            |what| |brightPrint| |formatOutput| |srcPosLine|
            |postDefArgs| |lfcomment| |pkey| |formatColon|
            |dbFromConstructor?| |moveImportsAfterDefinitions|
            |compTuple2Record| |nBlanks| |postTranScripts|
            |bcStarConform| /UNEMBED-1 |pf0TupleParts|
            |changeToEqualEqual| |putOut| |getCaps| |lfstring|
            |pfTuple?| |reduceImports| |srcPosFile| |pfSemiColonBody|
            |bcStar| |bitsOf| |typeIsASmallInteger| |markPartOp?|
            |isNewspadOperator| |mkAtreeValueOf1| |getHtMacroItem|
            |simpBool| DATABASE-SOURCEFILE |constructorCategory|
            |scanKeyTr| |indefIntegralWidth| |mathform2HtString|
            ?MODEMAPS |postTranScripts,fn| |extractHasArgs,find|
            |whatSpad2Cmd| |indefIntegralSup| |pfSemiColon?|
            |minimalise| |unTuple| |lfkey| |stackAndThrow| |postColon|
            |center80| |isPackageType| |evalDomain|
            |optFunctorBodyQuotable| |scanPossFloat| |dbShowKind|
            |conname2StringList| |isSimple|
            |makeCommonEnvironment,interE| |chkDirectory|
            |minimalise,min| |buttonNames| |scanCloser?|
            |indefIntegralSub| |postColonColon| |loadFunctor|
            |bcStarSpace| |postWhere| |pfLiteral?| |postcheckTarget|
            |pfInline| |keyword| |dcData| |primitiveType|
            |compDefWhereClause,transformType| |postcheck| |pf0AddBase|
            |loadLibIfNotLoaded| |unAbbreviateIfNecessary| |mkAtree|
            |npZeroOrMore| |mkDevaluate| |dbNonEmptyPattern|
            |pfAddBase| |parseAtom| |lineoftoks|
            |predicateBitIndexRemop| |postSemiColon|
            |mkCategoryPackage,gn| |systemErrorHere| |lisp2HT|
            |evalDomainOpPred,convertCatArg| |constructor| |postBlock|
            |postBlockItemList| |updateCategoryFrameForConstructor|
            LOG2 |getCType| |dbOpsForm| /UNTRACELET-2 |alqlGetParams|
            |pfParts| |convertOpAlist2compilerInfo|
            |minimalise,HashCheck| |postBlockItem| |form2Fence|
            |lisp2HT,fn| |form2StringList| |outputTranIterate|
            |deepestExpression| |getCategoryOpsAndAtts| |postQuote|
            |conform2HtString| |mkAndApplyPredicates| |makeNonAtomic|
            |postSequence| |lispize| |markCoerceChk| |nextline|
            |devaluateList| SIZE |upCOERCE| |errorRef|
            |translateYesNo2TrueFalse| |pfSemiColon| |pfSymbolSymbol|
            |getSrcPos| |formatPROGN| |postTranList| |unMkEvalable|
            |dbConform| |postComma| |numberOfEmptySlots|
            |mustInstantiate| |formatCAPSULE| |pfAddAddon|
            |dbConstructorDoc,fn| |upStreamIters| |pfSymbol?|
            |isSystemDirectory| |checkWarning| EOFP |int2Bool|
            |dbGetInputString| |dbMapping2StringList| /UNTRACELET-1
            |alqlGetOrigin| |sayBrightlyLength1| |getFlag|
            |removeTracedMapSigs| |downlinkSaturn| |pfAddAddin|
            |coerceMap2E| |keyword?| |pmTransFilter| |markFinishBody|
            |upconstruct| |intWidth| |pfEnSequence| |getMsgToWhere|
            |decodeScripts,fn| |findFrameInRing| |htSayList|
            |dbExtractUnderlyingDomain| |NRTgetLocalIndex|
            |alqlGetKindString| |comma2Tuple| |scanW| |pf0ImportItems|
            |mkUnixPattern| |parseConstruct| |blankIndicator|
            |isValidType| |upTARGET| |getOutputAbbreviatedForm|
            |hasOptArgs?| |falseFun| |npParenthesized|
            |mkExplicitCategoryFunction,fn| |isListOfIdentifiers|
            |tuple2List| |isLoaded?| |pfDefinitionRhs| |upLET|
            |isFluid| |updateCategoryFrameForCategory| |postCapsule|
            RENAME |closeOldAxiomFunctor| |htTab| |npboot| |iterVarPos|
            |alreadyOpened?| |npFromdom1| |parseTran,g|
            |string2Constructor| |mkQuote| |npEqKey| |remWidth|
            |pileComment| |pfImportItems| |orderBySubsumption|
            |upLETWithPatternOnLhs| |isListOfIdentifiersOrStrings|
            |pfSuchThat2Sex| |chkOutputFileName| |msgImPr?| |lfinteger|
            |pfInline?| |pf0DefinitionLhsItems| |dcCats| |isTupleForm|
            |postReduce| |pfReturnFrom| |matWidth| |CDRwithIncrement|
            |pfImport| RSHUT |isValidType;| |mkQuote,addQuote|
            |pfApplicationArg| |asTupleAsList| |chkPosInteger|
            |dbString2Words| |outputTranIteration| |compAndTrace|
            |pfListOf?| |lnGlobalNum| |npPush| |conLowerCaseConTran|
            |displayHashtable| |functionAndJacobian|
            |isExposedConstructor| |removeConstruct| SQUEEZE
            VMLISP::SIMPLE-ARGLIST |upcase| |pfCollect?| |pfFreeItems|
            |parseWhere| |ncParseFromString| |intSup| |lferror|
            |segmentedMsgPreprocess| |pf0TLambdaArgs| |lnLocalNum|
            |removeZeroOne| |dbConstructorDoc,gn| |rulePredicateTran|
            |isLocalPred| |postAdd| |parseVCONS| |npEncAp|
            |reassembleTowerIntoType| |pfOp2Sex| |pf0LoopIterators|
            |emptyInterpreterFrame| |pfTLambdaArgs|
            |pfSourcePositionlist| |digits2Names| |pfRuleRhs|
            |upequation| |pfUnSequence| |pfTLambdaBody| |isPartialMode|
            |addBlanks| |SpadInterpretFile| SHOWDATABASE |string2Words|
            |scanWord| |pfExitNoCond| |parseSeq| |intInterpretPform|
            UNSQUEEZE |postTupleCollect| |pf0WrongRubble|
            |pfSourcePositions| |compHasFormat| |dbCompositeWithMap|
            |altSeteltable| |npDotted| |string2BootTree| |upor|
            |pmDontQuote?| |makeOldAxiomDispatchDomain| |scanTransform|
            |pfRuleLhsItems| |packageTran| |postCollect|
            |extractFileNameFromPath| |isHomogeneous| |matSuper|
            |initCache| |pfWrongRubble| |whatCommands| |loopIters2Sex|
            IDENT-CHAR-LIT |htPred2English,fnAttr| |zeroOneTran|
            |hasFormalMapVariable,hasone?| BUMPERRORCOUNT
            |noBlankBeforeP| |intProcessSynonyms| VMLISP::REMOVE-FLUIDS
            |commandsForUserLevel| |pfTLambdaRets| |dbConname|
            |dbAddChain| |upbreak| |constructor2ConstructorForm|
            |pfLoop?| |pfWrongWhy| MAKE-ADJUSTABLE-STRING |digit?|
            |pfIterateFrom| MESSAGEPRINT |intnplisp| |intSub|
            |postRepeat| |pfLocalItems| |stopTimingProcess| |upDollar|
            |coerceUnion2Branch| |dnForm| MESSAGEPRINT-2 |addSpaces|
            |nplisp| |npAngleBared| DIGITP |noBlankAfterP|
            |kFormatSlotDomain| |PushMatrix| |setCurrentLine| |postIn|
            |pp2Cols| MESSAGEPRINT-1 |?comp| |dbKindString| MKPROGN
            |htShowCount| |devaluate| |copyHack| |satisfiesUserLevel|
            |lfspaces| |pfExitExpr| |simpCatPredicate| |copyHack,fn|
            |uncons| |postin| |dbInfovec| |upTuple|
            |retract2Specialization| VEC2LIST |dnForm,negate|
            |pfExitCond| |stripUnionTags| |pfCollectBody| |sigma2Width|
            |ncloopParse| |syminusp| |ncloopIncFileName| |postQUOTE|
            MAKE-VEC |dbGetCommentOrigin| |lfid| |compileQuietly|
            |NRTassocIndex| |phBegin| DATABASE-SPARE GCMSG
            |sayLongOperation| |Operators| |whatSpad2Cmd,fixpat|
            |getImports| |resolveTypeListAny| |ncloopEscaped|
            |pfCollectIterators| |upiterate| MONITOR-PRINTREST
            |mkAtree1| DEF-PROCESS |saySpadMsg|
            |isAlmostSimple,setAssignment| |upIF| |getLineText|
            |pfExit?| |mkPredList| |mkConArgSublis| |upisnt|
            |remHashEntriesWith0Count| |pfSourceText| |upisAndIsnt|
            |toFile?| |synonymsForUserLevel| |compileInteractive|
            |sayTeX| |spad2lisp| |float2Sex| |phInterpret| |getMsgArgL|
            |checkGetArgs| |isHomogeneousArgs| |poGetLineObject|
            IDENTITY |say2PerLineThatFit| |orderUnionEntries|
            |pfTyped?| |extwidth| DATABASE-DEFAULTDOMAIN CSC
            |getLinePos| DEF-RENAME |getUsersOfConstructor| |uphas|
            ACSC |loadIfNecessaryAndExists| |punctuation?| EQUABLE
            |phMacro| |varsInPoly| CSCH |lnPlaceOfOrigin|
            |pfRhsRule2Sex| |macroExpanded| |sigma2Sup| COTH
            |postTransform| |Record0| |upis| |dbShowConsKinds| ?COMP
            |pfLhsRule2Sex| |ncConversationPhase,wrapup| |stackWarning|
            COPY SMALL-ENOUGH DOWNCASE
            |processSynonymLine,removeKeyFromLine| |npBracked|
            |upwhere| |checkDocMessage| |pfFromdomDomain|
            |serverReadLine| |extsuper| |bcConTable| |sigma2Sub|
            |npListing| |makeFort,untangle| |ncloopPrintLines|
            |pfLiteralString| |extsub| |startTimingProcess|
            |mkLineList| |checkRemoveComments| DATABASE-NILADIC
            |makeCommonEnvironment,interLocalE| |nonBlank|
            |displayCacheFrequency| PARTCODET |makeFort,untangle2|
            |pfRule2Sex| |intloopEchoParse| VMLISP::LIBRARY-FILE
            |sigmaWidth| |upSEQ| /INITUPDATES |pfSuchthat?|
            |formatUnion,fn| |pmPreparse,hn| |incBiteOff|
            |checkTrimCommented| |sigmaSup| |formatTestForPartial|
            |makeOutputAsFortran| |SkipEnd?| VMLISP::GET-DIRECTORY-LIST
            |formatFunctionCall1| |rdigit?| |formatFunctionCallTail|
            |new2OldLisp| |incFileName| |Else?| VMLISP::PROBE-NAME
            |vec2Lists| |processSynonymLine| |pfLambda2Sex| |Elseif?|
            VMLISP::SPAD-FIXED-ARG |mkMat| |If?| |pfDefinition2Sex|
            PRINT-PACKAGE |npMoveTo| |inclmsgNoSuchFile|
            |inclmsgPrematureFin| |leftTrim| |complexRows|
            |incFileInput| VMLISP::LIBSTREAM-INDEXSTREAM
            |makeCommonEnvironment,interC| |makeLispList| |Top?|
            |segment| |fortSize,elen| |printSynonyms| |pfCollect2Sex|
            |dbEvalableConstructor?| |pfSourceStok|
            |inclmsgPrematureEOF| VMLISP::LIBSTREAM-INDEXTABLE ACTION
            |SkipPart?| |checkGetMargin| VMLISP::LIBSTREAM-MODE
            |quote2Wrapped| INITIALIZE-PREPARSE |exp2Fort1|
            |vec2Lists1| |pfApplication2Sex| |KeepPart?| |getCallBack|
            |clearParserMacro| |multiToUnivariate| |incNConsoles|
            |whoOwns| |break| |texFormat1| |pfLiteral2Sex|
            VMLISP::GETINDEXTABLE ASSOCLEFT
            |dbScreenForDefaultFunctions| |Skipping?| S-PROCESS
            |npNull| |pfWhereExpr| |incClassify| |unTab| |sayALGEBRA|
            |deltaContour,eliminateDuplicatePropertyLists|
            |newHelpSpad2Cmd| |spadTypeTTT| |pf0WhereContext|
            VMLISP::GET-INDEX-TABLE-FROM-STREAM |formatIF| EXPAND-TABS
            RETRANSLATE-DIRECTORY |isFloat| |dbChooseOperandName|
            |pfIterate?| |makeUnion| |incCommand?|
            |zsystemDevelopmentSpad2Cmd| |pfReturnExpr|
            |kPageContextMenuSaturn| |incRenumber| |fortSize|
            |pfReturn?| |incFile| |maPrin| |parseFromString|
            |fortExpSize| |setOutStream| VMLISP::LIBSTREAM-P
            RETRANSLATE-FILE-IF-NECESSARY |incPos| |saturnExampleLink|
            |parseAndEval1| |stripNil| |pfBreakFrom| |inclmsgSay|
            |NRTassocIndexAdd| |markAt| |pfBreak?| |explainLinear|
            |checkPmParse,fn| |parseAndEvalToString| |inclmsgConStill|
            |checkType| |unpart| |pfRule?| |incStringStream|
            |clearTempCategoryTable| |parseAndEvalToStringForHypertex|
            DATABASE-CONSTRUCTORMODEMAP |optDeltaEntry,quoteSelector|
            |interpOnlyREPEAT| |inclmsgConActive| RECOMPILE-ALL-LIBS
            |markInsertNextChange| |XDRFun| |%key| FOAM::TYPE2INIT
            |dbRead| |ppos| |string2SpadTree| |pair2list| |markRemove|
            FOAM::FOAM-FUNCTION-INFO |htSayCold| |printStats|
            |markSigTran| |GetValue| |NRToptimizeHas|
            RECOMPILE-LIB-DIRECTORY |checkPmParse| |markWrapPart|
            FOAM:|fiStrHash| |mkParameterList| |markInsertBodyParts|
            |hasToInfo| FOAM::INSERT-TYPES |listOfBoundVars|
            |htCopyProplist| TOKEN-P |pfStringConstString| |formatDEF1|
            |slot1Filter,fn| |makeLeaderMsg| FOAM:|fiGetDebugger|
            |formatPred| |addToCategoryTable|
            |putInLocalDomainReferences| |writeSaturnTable|
            |pathnameTypeId| |pfExportDef| IS_SHARP_VAR
            |simpHasPred,simpDevaluate| |pfPosOrNopos| |unStackWarning|
            |chaseInferences,foo| |killNestedInstantiations|
            |maprinRows| NREVERSE0 |prefix2Infix| |sigmaSub|
            RECOMPILE-ALL-FILES |NRTputInTail| |capitalize| |liftCond|
            |quotifyCategoryArgument| |fortFormatIntrinsics|
            FOAM::FOAMPROGINFOSTRUCT-P |getLisplibVersion|
            |getMsgPrefix| |writeSaturn| |sayFORTRAN|
            |pfDefinitionSequenceArgs| |formatInfo|
            FOAM:|fiSetDebugVar| |mkCategoryExtensionAlist|
            |unInstantiate| |htSayValue| ?M |markFinishItem|
            |addInformation,info| |maprinChk| |asTupleAsVector|
            |lispType| |updateCategoryTableForCategory|
            |lisplibDoRename| |clearCmdExcept| |asTupleSize|
            |infoToHas| |isFormalArgumentList| |finalizeLisplib|
            |pfComDefinitionDef| |bcAbbTable| |disallowNilAttribute|
            |writeSaturnPrint| |getSubstSigIfPossible| |displayLines|
            |addInfo| RECOMPILE-ALL-ALGEBRA-FILES |asTupleNewCode0|
            |checkForBoolean| |formatPredParts| |consBuffer|
            |bcConform1,say| |ident2PrintImage| |processKeyedError|
            |printInfo| |isIdentifier| |toScreen?| |isVowel|
            |escapeSpecialIds| |linearFormat| |defaultingFunction|
            |getOfCategoryArgument| |compileConstructor1| |npTrapForm|
            |bcNameConTable| |getOperationAlistFromLisplib|
            |compileDocumentation| |vConcatSub| |workfilesSpad2Cmd|
            |NRTinnerGetLocalIndex| |formatOperationAlistEntry|
            |getConstructorAbbreviation| |transformREPEAT|
            |pfTransformArg| |breakIntoLines| |formatIf| LOAD-DIRECTORY
            |line?| |checkAddPeriod| |retract1| |readLibPathFast|
            |isIntegerString| |vectorOfFunctions| |linearFormatName|
            |formatApplication1| |qTWidth| |newMKINFILENAM|
            |modemap2Signature| |dollarPercentTran|
            |decomposeTypeIntoTower| |getFunctionSourceFile1|
            |transformCollect| |pfTaggedToTyped1| |dbConstructorKind|
            |transcomparg| |postDoubleSharp| |msgLeader?|
            |checkDecorate| |compileConstructor| |cd| |pfFlattenApp|
            FUNLOC |pathname?| |initToWhere| BLANKP |stringWidth|
            |sumoverlist| |initImPr| |pfTaggedToTyped|
            |mathprintWithNumber| |putDatabaseStuff| |hasNoVowels|
            |markCatsub| |string2Float| COND-UCASE |htProcessBcStrings|
            |checkBalance| |texFormat| |getMsgPosTagOb| |stripSpaces|
            |dbGetExpandedOpAlist| |specialChar| |bubbleConstructor|
            |matSuperList| |pfIdSymbol| |checkSayBracket|
            |pfCollectVariable1| |hashCode?| |isSubForRedundantMapName|
            |formatPrenAux| |pfSequence2Sex| MATCH-STRING |formatWith|
            |superSubWidth| |mkAtreeExpandMacros| |dbAddDocTable|
            |InvestigateConditions,pessimise| |dbConstructorDoc,hn|
            |isDomainOrPackage| |formatArgList| |formatAdd| |getMsgPos|
            |checkBeginEnd| |zsystemdevelopment|
            |listOfPredOfTypePatternIds| |pf2Sex1| |macApplication|
            |pfCollect1?| |script2String| |getBrowseDatabase|
            |predicateBitIndex| |isInterpMacro| |getConstructorForm|
            NONBLANKLOC |dispfortexp| |form2Fence1| |isQuotient|
            |getMsgFTTag?| |checkIeEg| |isInterpOnlyMap|
            |replaceGoGetSlot| |wrapSEQExit| |leader?|
            DROPTRAILINGBLANKS |formulaFormat| |constructorName|
            |matSubList| |pf0ApplicationArgs| |workfiles|
            |compileSpadLispCmd| |boxWidth| |sayModemap|
            |formatForcePren| |originsInOrder| |sayMath| |opIsHasCat|
            |isSpecialBufferItem| |remFile| |pfComDefinitionDoc|
            |isNewWorldDomain| |form2FenceQuoteTail| |markUnique|
            |domainZero| |formCollect2String| INDENT-POS
            |DNameToSExpr1| |domainOne| |incExitLevel|
            |formatMacroCheck| |superSubSuper| |pfMLambda?|
            |pfSequence2Sex0| ASEC |tuple2String| |formatSelection1|
            |isRationalNumber| |whichCat| |DNameFixEnum|
            |pfApplicationOp| |getViewsConditions| |combineMapParts|
            |formJoin2String| |matLSum| |checkDocError1|
            |removeBindingI| |pfLoopIterators| NEXT-TAB-LOC
            |reverseDown| |unescapeStringsInForm| |mkErrorExpr,bracket|
            |addArgumentConditions,fn| |pfDWhereExpr|
            |displayProperties,sayFunctionDeps| |DNameToSExpr|
            |executeInterpreterCommand| |macId| |ICformat,Hasreduce|
            ACOT |parseAndInterpret| |augmentBodyByLoopDecls|
            |CompStrToString| |simplifyAttributeAlist| |getUnname1|
            |pfForinLhs| |getDependentsOfConstructor|
            |markInsertRepeat| |superSubSub| |remLine|
            |markInsertIterator| |TryGDC| |htMakeSaturnFilterPage|
            |displayMacro| |record2String| |pfSourcePosition|
            |ruleLhsTran| |writeSaturnLines| |ncSetCurrentLine|
            FOAM-USER::|AXL-spitSInt| BLANKCHARP |patternVarsOf|
            |spadCompileOrSetq| VMLISP::QUOTESOF |pvarsOfPattern|
            |formatDefault| |getMsgKey?| |prEnv| |form2FenceQuote|
            |genDeltaEntry| |computedMode| |htEscapeString|
            SPADTAGS-FROM-FILE |formatLeave| |hasIdent|
            |displayParserMacro| |formWrapId| |pfFromdomWhat| |mac0Get|
            |formatCategory| |checkAddMacros| VMLISP::DEQUOTE
            |getMsgKey| |matLSum2| |compCategories| |compilerMessage|
            |pfLambdaTran| |operationLink| |compile| |hasPatternVar|
            |ICformat,ORreduce| |pfLambdaBody| |evaluateType| |dcAll|
            |constructMacro| |opTran| OUR-WRITE-DATE |poPosImmediate?|
            |checkSplit2Words| |pfHidePart| MANEXP |pfMLambdaBody|
            |pfFromdom?| |parseNoMacroFromString| |poNopos?|
            |mapConsDB| |asharpConstructorName?| |evaluateType1|
            |pfLambdaRets| |hasType,fn| RECOMPILE-FILE-IF-NECESSARY
            |pf0MLambdaArgs| |parseDEF| |checkAddSpaces|
            |findSubstitutionOrder?| |makeMissingFunctionEntry,tran|
            VMLISP::ISQUOTEDP |evaluateSignature| |pfPretendType|
            |mkMapPred| |pfTypedType| |setDefOp| |markDeclaredImport|
            |macMacro| |newString2Words| |objVal| |poLinePosn|
            |suScWidth| |mkCategory,Prepare2| VMLISP::VARP |getValue|
            |pfCollectArgTran| |failCheck| |transIs1|
            |clearCategoryCache| |formatOpConstant| |pf2Sex| |getMode|
            |pfNothing?| |tr| |getUnname|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) *) MAKE-APPENDSTREAM MAKE-INSTREAM ASHARP
            COMPILE-LIB-FILE |sayBrightlyNT| MAKE-OUTSTREAM
            FOAM:COMPILE-AS-FILE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (VALUES T T)) |spadTraceAlias|
            FOAM:AXIOMXL-GLOBAL-NAME)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) (VALUES T T)) MDEF)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) T) PRINT-NEW-LINE PRINT-FULL
            GET-BOOT-IDENTIFIER-TOKEN COMPSPADFILES |F,PRINT-ONE|
            RDEFIOSTREAM BLANKS VMPRINT PRETTYPRINT CATCHALL TAB
            MAKE-INPUT-FILENAME MAKE-FULL-CVEC |getDomainSigs|
            |showImp| |showFrom| |getDomainDocs| MATCH-NEXT-TOKEN
            |sayBrightly| |LAM,EVALANDFILEACTQ| PRETTYPRIN0
            |sayBrightlyI| MATCH-CURRENT-TOKEN MAKE-HASHTABLE
            MAKE-FILENAME MACERR |fillerSpaces| |desiredMsg| FINDTAG
            |pfExpression| |pfSymbol| |pfSymb| MONITOR-ADD
            VMLISP::MAKE-FULL-NAMESTRING)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) *) SUFFIX QUOTIENT |bcInputMatrixByFormula|
            |bcInputExplicitMatrix| |htStringPad|
            |evalAndRwriteLispForm| |LAM,FILEACTQ| |chk|
            |mkAtreeWithSrcPos| |rwriteLispForm| COMPILE-DEFUN |doIt|
            BPIUNTRACE |print| |compilerDoitWithScreenedLisplib|
            DEFSTREAM |compilerDoit| MONITOR-PRINVALUE /TRACE-2
            |hasFormalMapVariable| |ScanOrPairVec| PRINMATHOR0
            |spadTrace| |output| |popUpNamedHTPage|
            |replaceNamedHTPage| |sockSendFloat| SOCK-SEND-SIGNAL
            SOCK-SEND-FLOAT SOCK-SEND-STRING SOCK-SEND-INT |sayErrorly|
            |saturnSayErrorly| |set1| |displaySetOptionInformation|
            |mkGrepPattern| |showDoc| |genSearchSayJump| |oPageFrom|
            |showConstruct| |htCommandToInputLine,fn|
            |grepConstructorSearch| |showNamedDoc| |diff|
            |form2HtString,fnTail| |xdrWrite| |spleI1| |diffCompare|
            |doItExpression| COMP_QUIETLY_USING_DRIVER
            |readData,xdrRead1| |xdrRead| |sockSendSignal|
            |htpLabelFilteredInputString| |htGlossSearch|
            |htSetSystemVariable| |htSetSystemVariableKind|
            |htSetNotAvailable| |htShowLiteralsPage| |htCheck|
            |htShowIntegerPage| |htShowFunctionPage|
            |htSetFunCommandContinue| |htKill| |htFunctionSetLiteral|
            |htShowSetPage| ADDCLOSE |htSetLiteral| |ppPair|
            |getMinimalVarMode| |checkAddSpaceSegments|
            |checkAddIndented| |alistSize,count| |dbConformGen1|
            |pickitForm| |koaPageFilterByCategory1| VMLISP::COPY-FILE
            VMLISP::COPY-LIB-DIRECTORY |spadcall2| |sublisV|
            |sublisV,suba| |fortError| |koPageFromKKPage| |kArgPage|
            |npsystem| |handleParsedSystemCommands|
            |handleTokensizeSystemCommands| |tokenSystemCommand|
            |reportOpsFromLisplib1| |handleNoParseCommands|
            |addPatchesToLongLines| |kArgumentCheck| COERCE-FAILURE-MSG
            |kxPage| |kcnPage| SAYBRIGHTLYNT1 |kcuPage| |ksPage|
            |conOpPage| |kcdoPage| |kcdePage| |kcdPage| |kccPage|
            |patternCheck,subWild| |kcaPage| |kcpPage| |sockSendInt|
            |kePage| |sockSendString| |koaPageFilterByName|
            |koaPageFilterByCategory| |koPageAux1| |kcPage| |getmode|
            |docSearch1| |grepSearchQuery| |repeatSearch|
            |reportOpsFromLisplib0| |reportOperations|
            |generalSearchDo| |grepSearchJump|
            |mkDetailedGrepPattern,conc| |kiPage| |spill| |errorPage|
            |dbShowConsKindsFilter| |koPage| |dbInfoChoose| |kciPage|
            |dbInfoChooseSingle| |formatReduce1| |dbSort| |msgText|
            |bcSeriesByFormula| |bcRealLimitGen1| |bcSeriesExpansion|
            |ncloopInclude| |bcComplexLimit| |bcRealLimit|
            |nary2Binary| |htFilterPage| |bcPuiseuxSeries|
            |bcLaurentSeries| |bcTaylorSeries| |bcLinearSolveMatrix|
            |bcMakeEquations| |bcMakeLinearEquations|
            |bcLinearSolveEqns| |bcSolveSingle| |bcInputEquations| FC
            |bcSystemSolve| |bcSolveEquationsNumerically|
            |bcSolveEquations| |bcLinearSolve| |bcLinearMatrixGen|
            |bcLinearSolveMatrixInhomoGen| |bcLinearSolveMatrixInhomo|
            |bcLinearSolveMatrixHomo| |finalExactRequest| |printMap1|
            |htMkName| |makeLongSpaceString| |makeLongTimeString|
            SPAD-SAVE |nrtEval| |ncloopCommand| |ncloopInclude1|
            |ncConversationPhase| |inclHandleBug| |evalSlotDomain|
            |ncEltQ| |formArguments2String,fn|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) T) |canCoerce;| |pileForest1| |canCoerce1|
            DAASENAME |pileTree| |eqpileTree| |pileCtree| |resolveTT;|
            WRAPDOMARGS |mkAliasList,fn| |evalCategory| |replaceSharps|
            |depthOfRecursion| |ofCategory| |formatJoinKey|
            |canCoerceFrom;| |canCoerceFrom0| |putBodyInEnv|
            |isEqualOrSubDomain| |mapDefsWithCorrectArgCount|
            |hasCorrectTarget| MAKE-DATABASES |sayModemapWithNumber|
            |resolveTT1| |applyWithOutputToString| |isDomainSubst,fn|
            |ncloopPrefix?| |isDomainSubst,findSub| |intloopPrefix?|
            |insertModemap| |phIntReportMsgs| FLAGP |processMsgList|
            REDUCTION-PRINT |phParse| |mkAlistOfExplicitCategoryOps,fn|
            |isPatMatch| REMOVER |intloopReadConsole| STACK-LOAD
            |streamChop| ESCAPE-KEYWORDS |inclFname| ?ORDER
            |allLASSOCs| |incDrop| MAKE-PARSE-FUNCTION1
            |inclmsgFileCycle| |pairList| |addDefaults| |assertCond|
            INITIAL-SUBSTRING-P |incActive?| |finalizeDocumentation,fn|
            |formatOperation| |incStream| |formatOpSignature|
            |get1defaultOp| |inclHandleSay| |sayKeyedMsg|
            |inclHandleWarning| |transDocList| |inclHandleError|
            MAKE-PARSE-FUNC-FLATTEN |incRenumberLine|
            |recordAttributeDocumentation| |incRenumberItem|
            |recordDocumentation| |compileBody| |lnSetGlobalNum|
            |recordSignatureDocumentation| |makeLocalModemap|
            FOAM::ALLOC-PROG-INFO S* |macroExpand| |liftCond,lcAnd|
            |checkRewrite| |saveDependentMapInfo| |actOnInfo|
            |checkComments| |mkJoin| |checkExtract| |plural|
            |checkTrim| |axFormatDecl| |spadSysChoose| |mkMapAlias|
            |has| |markReduce| |containedRight| |readData|
            |hashTypeForm| |checkIsValidType,fn| |axFormatConstantOp|
            |oldAxiomPreCategoryParents| |markParts| |transDoc|
            |oldAxiomCategoryDefaultPackage| |checkIndentedLines|
            |axFormatOpSig| |linearFormatForm| SAYBRIGHTLY1
            |mkFormalArg| |newHasAttribute| |pvarPredTran|
            |oldAxiomCategoryParentCount| |writeData|
            |findSubstitutionOrder?,fn| |intersectionEnvironment|
            |app2StringConcat0| |pfRule| |formDecl2String| |coerceExit|
            |mkValCheck| |sayLooking1| |mkAutoLoad| |resolveTM|
            |mkValueCheck| |formJoin1| |moveLinesAfter|
            |autoCoerceByModemap| |app2StringWrap| |coerceExtraHard|
            |isPointer?| |mkLessOrEqual| |hasType|
            |formArguments2String| |wordFrom| MAKENEWOP
            |getConstructorMode| |putValueValue|
            |getConstructorFormOfMode| |wt| |asTupleNew| |pfWhere|
            |objSetVal| DIVIDE |markRepeatBody| |coerceHard| |dqAppend|
            |objNewCode| FOAM::|magicEq1| |npRightAssoc|
            FOAM-USER::H-ERROR |coerceSubset| |makePattern|
            |displayRule| CONTAINED |reportCircularCacheStats| GETL
            |coerceInteractive| |mkCircularCountAlist| |makeAxFile|
            |canMakeTuple| |pfPushMacroBody| GGREATERP
            |clearDependencies| FOAM-USER::H-STRING |pfMacro|
            |formatOpSymbol| |coerceEasy| FOAM-USER::H-INTEGER
            CGREATERP |keyedSystemError| |getEqualSublis,fn|
            |addPatternPred| |chaseInferences| |sourceFilesToAxFile|
            |interpMap| |say2PerLineWidth| |mkLocalVar|
            |getFormModemaps| |getLocalVars| FLAG |prEnv,tran|
            |modemapsHavingTarget| SORTBY |markRepper| |qArg|
            |PPtoFile| |npMissingMate| |positionInVec| |canFit2ndEntry|
            |center| |simplifyMapPattern| |sayKeyedMsgLocal|
            |substituteCategoryArguments| |isDomainConstructorForm|
            |getMapBody| |mkUnion| PAIR |mkIterVarSub|
            |keyedSystemError1| |printEnv,tran|
            |lazyOldAxiomDomainDevaluate| |htpLabelInputString|
            |orderByDependency| |listTruncate|
            |lazyOldAxiomDomainHashCode| |newHasTest|
            |saturnKeyedSystemError| |declare| |getFunctorOpsAndAtts|
            |makeCategoryForm| |declareMap| |breakKeyedMsg|
            |markReduceSuchthat| ADDOPERATIONS |concat1|
            |fastSearchCurrentEnv| ASHARPMKAUTOLOADFUNCTION
            |upfreeWithType| |member| |putMode| |uplocalWithType|
            |splitListOn| |markCompare| |htpLabelSpadValue| |deleteAll|
            |sayKeyedMsgAsTeX| |putFlag| |oldAxiomCategoryDevaluate|
            |SymMemQ| |mkAtreeNodeWithSrcPos| SUBLISNQ |SExprToDName|
            |getMsgCatAttr| |addToSlam| |oldAxiomPreCategoryDevaluate|
            $FINDFILE |throwPatternMsg| |putDependencies|
            |DomainSubstitutionFunction| |checkForFreeVariables|
            |transferSrcPosInfo| DELDATABASE NCONC2 STACK-PUSH
            |sayPatternMsg| |isNestedInstantiation| |getKeyedMsgInDb|
            |DomainSubstitutionFunction,Subst|
            |oldAxiomDomainDevaluate| |mkAtree1WithSrcPos|
            |lassocShift| |newHasCategory| |wrapDomainSub|
            |htMakeTemplates| |orderedDefaults| SETDIFFERENCE
            |isKeyedMsgInDb| |htMakeDoneButton| |listInitialSegment|
            |patternVarsOf1| |compCategoryItem| |attributeNthParent|
            |writeLib| GETCONSTRUCTOR |oldAxiomDomainHashCode|
            |pfFromDom| |attributeHashCode| |symEqual|
            |oldAxiomPreCategoryHashCode| |domainEqualList|
            |attributeDevaluate|
            |putDependencies,removeObsoleteDependencies|
            SET-LIB-FILE-GETTER FOAM:|fputs| |pfApplication|
            FOAM:|fputc| DELASC |makeNewDependencies|
            |rightJustifyString| |oldAxiomCategoryHashCode|
            |remHashEntriesWith0Count,fn| |PARSE-Operation|
            |globalHashtableStats| |evalLET| |ListMemberQ?|
            |getDomainView| |htInitPage| |domainEqual| |ListMember?|
            |pfBrace| REMAINDER |compileIs| LASSOC |pfOr| |pfAnd|
            |notCalled| |evalLETchangeValue| |pfTLam| |isPatternMatch|
            |stringChar2Integer| |htpProperty| |seteltable| |reshape|
            |intSayKeyedMsg| INTERSECTIONQ |containsOp| |upLispCall|
            |hashCombine| |genIFvalCode| |evalLETput| |hashType|
            |AlistAssocQ| |spadSysBranch| |AlistRemoveQ|
            |htSystemVariables,gn| |intloopProcessString| |postFlatten|
            |makeRuleForm| |ncloopDQlines| |gatherGlossLines|
            |intloopInclude1| |postFlattenLeft| |intloopInclude|
            |postTranSegment| |upIFgenValue| SEGMENT |nonRecursivePart|
            |putPvarModes| |pfTyped| VMLISP::ECQEXP |postScriptsForm|
            |outputFormat| |htCheckList| |npTypedForm1|
            |htSetvarDoneButton| |htMakeDoitButton| |htMakePathKey,fn|
            |prnd| |npLeftAssoc| |reportAO| |htMarkTree| BVEC-XOR
            |pfCollect| BVEC-OR VMLISP::DCQEXP |pfQualType|
            |deltaContour| BVEC-AND ADD-PARENS-AND-SEMIS-TO-LINE
            BVEC-GREATER |getUniqueSignature| BVEC-EQUAL
            |AMFCR,redefinedList| BVEC-CONCAT |stringLE1|
            |putDomainsInScope| INITIAL-SUBSTRING BVEC-MAKE-FULL
            |compFormMatch,match| |scylla| HPUT* STOREBLANKS
            |mkSuperSub| FOAM:|printDFloat| |compFormMatch|
            |EqualBarGensym| |pfReturn| ESCAPED FOAM:|printSFloat|
            FOAM:|printBInt| PARSEPILES |pfSpread| |addNewDomain|
            |npTypedForm| |markFinish| |htDoNothing| |after|
            |AMFCR,redefined| |optCatch,changeThrowToGo| |domainMember|
            |optCatch,hasNoThrows| |optCatch,changeThrowToExit|
            STRING2ID-N |markGetPath| MONITOR-WRITE
            |optimizeFunctionDef,replaceThrowByReturn|
            |htpSetDomainPvarSubstList| |optCallSpecially,lookup|
            |coerceByModemap| |EqualBarGensym,fn| |htpLabelFilter|
            |pfLp| |profileDisplayOp| |optimizeFunctionDef,fn|
            |htpLabelSpadType| |htpSetRadioButtonAlist| |pfAssign|
            |pfWrong| |markConstruct| |pfForin|
            |htpSetDomainVariableAlist| |markComp| |pfDefinition|
            |convertOrCroak| |pfReturnTyped| |htpSetDomainConditions|
            |pfLam| |bcOpTable| |pfIfThenOnly| |xdrOpen| |pfExit|
            |scanExponent| |printNamedStatsByProperty| |scanCheckRadix|
            |Delay| |coerceUn2E| |initializeTimedNames|
            |inFirstNotSecond| |searchTailEnv| |coerceVal2E|
            |searchCurrentEnv| |EnumPrint| |search| |scanInsert|
            VMLISP::WRAP |insertWOC,fn| |RecordPrint| |mkObj|
            |coerceRe2E| |coerceInt| |syIgnoredFromTo| |deleteAssocWOC|
            |sySpecificErrorHere| |pfTree| |deleteAssocWOC,fn|
            |makeList| |deleteLassoc| |pfSuch| REMALIST
            |compCategories1| |sublisNQ| |pfParen| |BooleanEquality|
            |pfPretend| |sublisNQ,fn| |pfComDefinition| |pfMLambda|
            |dbInfoWrapOrigin| |resolvePatternVars| TAILFN |insert|
            |cons5| RPLACW |dbInfoSigMatch| |makeMissingFunctionEntry|
            |ancestorsOf| |pfHide| |compIterator| |getIdentity|
            |augmentHasArgs| |formatOpPren| |processInteractive1|
            |recordAndPrint| |interpretTopLevel| |pfBracketBar|
            |substituteSegmentedMsg| |dbSpecialExpandIfNecessary|
            |pfIdPos| |sameUnionBranch| |ProcessCond|
            |htpSetPageDescription| |testBitVector| |DescendCodeAdd|
            |LookUpSigSlots| |dbShowConsDoc| |containsString|
            |DomainPrintSubst| |printTypeAndTimeNormal|
            |satTypeDownLink| |partPessimise| |printTypeAndTimeSaturn|
            |formatQual| |mkDocLink| FOAM:|printSInt| |pfBraceBar|
            |addParameterTemplates| |hasPair| FOAM:|printString|
            |markReduceIn| FOAM:|printChar| |pfTagged| WI
            |htpAddToPageDescription| |HasCategory|
            |getAliasIfTracedMapParameter| |pfAbSynOp?|
            |formatDeftranJoin| |HasSignature| |printTypeAndTime|
            |phReportMsgs| |untraceDomainConstructor,keepTraced?|
            |tryToFit| |markCons| |HasAttribute| |pfWDeclare|
            |htpButtonValue| |InvestigateConditions,Conds|
            |htSayConstructorName| |getMapSig| |pfBracket|
            |spadTrace,isTraceable| |pfDWhere| |markRepeat|
            |removeOption| |DescendCodeVarAdd| |markCompColonInside|
            |screenLocalLine| |markCaseWas| |undoSteps| |orDnf| |agg|
            |dnfContains,fn| |diffAlist| |andReduce| |markSetq|
            |undoSingleStep| |simpBoolGiven| |htSayBind| |dnfContains|
            |bcConstructor| |coafAndCoaf| |formatIfExit| |checkArgs|
            |ordIntersection| SPADTAGS-FROM-DIRECTORY |ordSetDiff|
            |formatDeftranForm| |matSuperList1| |coafOrDnf|
            |consComments| |getBindingPowerOf| |predCircular|
            |matSubList1| |clearAllSlams,fn| |matWList1|
            |assocCircular| |recurrenceError| |htpLabelDefault|
            |countCircularAlist| |displaySetVariableSettings|
            |sayCacheCount| |htpLabelErrorMsg| |replaceNodeBy|
            |chebstareval| |setBootAutloadProperties|
            |BesselIAsymptOrder| |setUpDefault| |horner|
            |setBootAutoLoadProperty| |mkBootAutoLoad|
            |BesselKAsymptOrder| |cbeta| |matWList| |PsiAsymptotic|
            |mkGetPaths| |PsiEps| |logicalMatch?|
            |replaceNodeInStructureBy| |FloatError| |subCopy0|
            |cgammaG| |patternCheck,wild| |besselIback| |beforeAfter|
            |rPsiW| |deepSubCopyOrNil| |firstNonDelim|
            |patternCheck,pos| |chebf01| |deepSubCopy0| |BesselJ|
            |prefix?| |BesselI| |subCopyOrNil| |grepSplit|
            |htpSetInputAreaAlist| |grepConstruct1| |termRW1|
            |grepConstructDo| |processInteractive| |mkGrepPattern1,h|
            |termRW| |pfCoerceto| LENGTHENVEC |maskMatch?|
            |stripOffSegments| |tdAdd| |pfFromdom| |filterByTopic|
            |pfRetractTo| |addTopic2Documentation| |pfRestrict|
            |addStats| |mkGrepPattern1,split| |transferCodeCon|
            |testInput2Output| |compileCases| |hyperize|
            |transferClassCodes| |testPrin| |markAutoWas|
            |addArgumentConditions| |grepCombine|
            |NRTassignCapsuleFunctionSlot| |subMatch| |reportSpadTrace|
            RDROPITEMS |bcAbb| BVEC-NOR |lfrinteger| BVEC-NAND
            |getFortranType| |addDomain| |wl|
            |giveFormalParametersValues| |formatBlock| |scanIgnoreLine|
            PRINT-DEFUN |makeVector| |augmentTraceNames|
            |htPred2English,fn| |stripOffSubdomainConditions|
            |formatPiles| |posend| |breaklet| |untraceDomainLocalOps|
            |functionAndJacobian,DF| TRANSLABEL1 |isString?|
            |getOption| |formatDeftranREPEAT| TRANSLABEL |sort|
            |sayDroppingFunctions| |traceOptionError| |defLET2|
            GET-GLIPH-TOKEN |defLetForm| |funfind,LAM| |asyMapping|
            |mergePathnames| |defIS1| |subTypes| |asySig| |lassocSub|
            |defISReverse| |dbWordFrom| |addCARorCDR|
            |nonRecursivePart1| |commandUserLevelError| |defLET1|
            STRINGPAD |applyGrep| |asyExportAlist,fn| TRUNCLIST
            |htButtonOn?| |displayDatabase,fn| |expandRecursiveBody|
            |generalSearchString| |markGetPaths| |quickAnd|
            |zsystemdevelopment1| $REPLACE |incTrunc| |asyCattranSig|
            |addDefMap| |grepForAbbrev| |asySigTarget| |match?| UNIONQ
            |asyMkSignature| |commandError| |asCategoryParts,build|
            |optionUserLevelError| |firstDelim|
            |unabbrevRecordComponent| /READ |unabbrev1|
            |kciReduceOpAlist| |makeByteWordVec2| |dbInfoTran|
            |dollarTran| |condAbbrev| |ifCond| SPADRREAD
            |koPageInputAreaUnchanged?| |unabbrevUnionComponent|
            CHAR-EQ REPEAT-TRAN |htpLabelType| MKPFFLATTEN
            |errorSupervisor| |sayErrorly1| MKPF1
            |PARSE-rightBindingPowerOf| |chebeval| |rPsi|
            |ScanOrPairVec,ScanOrInner| |cpsireflect| -REPEAT |cPsi|
            |BesselJRecur| |CONTAINED,EQUAL| |intersection|
            |substFromAlist| |BesselJAsymptOrder| |CONTAINED,EQ|
            |BesselJAsympt| |PsiXotic| |f01| |kPageArgs| |brutef01|
            |dbSubConform| |coerce0| RBESSELJ CPSI |coerceRep| RPSI
            MARKHASH CHYPER0F1 |getI| CBESSELI RBESSELI CBESSELJ
            |formatLazyDomainForm| |formatLazyDomain|
            |domainDescendantsOf,jfn| |getDomainSigs1|
            |domainDescendantsOf,catScreen| |qt| |showDomainsOp1| |qe|
            LEXLESSEQP |devaluateSlotDomain| |compFormOrderModemaps|
            |getDomainRefName| GLESSEQP |andDnf| |ordUnion|
            |coafAndDnf| |orDel| |constantInDomain?|
            |translateMpVars2PVars| SUBB |addDmpLikeTermsAsTarget|
            |getCDTEntry| |genMpFromDmpTerm|
            |htMakeTemplates,substLabel| |doDoitButton|
            |domainDescendantsOf| |keyedMsgCompFailure| DO_LET |objNew|
            |putValue| |getAtree| |markAutoCoerceUp| |putModeSet|
            |bottomUpType| |bottomUpIdentifier| |deleteWOC| /EMBED-1
            |transferPropsToNode| |getArgValue| |next|
            |bottomUpCompilePredicate| |bottomUpPredicate| |suffix?|
            |putTarget| |getMinimalVariableTower|
            |computeTypeWithVariablesTarget| |mkCurryFun|
            |writeInputLines| |pushDownOp?| DEFINE-FUNCTION
            |sayIntelligentMessageAboutOpAvailability| |getBasicMode0|
            TAKE |mkObjCode| |mergeSignatureAndLocalVarAlists| REMFLAG
            |throwKeyedMsg1| |intCodeGenCOERCE| |childArgCheck| S+
            |canCoerceByMap| |canCoerceByFunction| POSN1 QLASSQ
            |saturnThrowKeyedMsg| |isSubDomain| |assocCar|
            |absolutelyCanCoerceByCheating| |childAssoc|
            |coerceCommuteTest| |asyGetAbbrevFromComments,fn|
            |ancestorsAdd| |asySplit| |asyWrap| |quickOr| GETDATABASE
            |asyAbbreviation,chk| MAKE-PARSE-FUNCTION |asyTypeJoinPart|
            |explodeIfs,gn| PREDECESSOR POINT
            |dbGatherDataImplementation| MKPF |dbMakeSignature|
            |asyDisplay| |dbExposed?| ERROR-FORMAT |getRegistry|
            |asyAbbreviation| |opAlistCount| |asyCattranConstructors|
            |DomainPrint| |bcStarSpaceOp| |makeSF|
            |evalDomainOpPred,convert| |asySimpPred|
            |formatGetBindingPowerOf| NLIST CHAR-NE NSTRCONC
            |evalDomainOpPred,evpred| DIVIDE2 QUOTIENT2
            |evalDomainOpPred,evpred1| SETANDFILE |htpSetName|
            PUSH-REDUCTION |pmatch| |delete| /TRACE-1 |resolveTMEq|
            |evalDomainOpPred| |getUnionMode| |getDomainOpTable,memq|
            |resolveTMEq1| |isUnionMode| |coerceInt2Union|
            |incCommandTail| |superMatch?| |resolveTMSpecial|
            |coerceIntFromUnion| |resolveTMRecord| NREVERSE-N
            |resolveTMUnion| |isFunction| |coerceIntAlgebraicConstant|
            TRUNCLIST-1 SETDIFFERENCEQ |coerceIntTower| |coerceRetract|
            -REDUCE-OP |compareTypeLists|
            |convertOpAlist2compilerInfo,formatSig| |modifyModeStack|
            OR2 |replaceSymbols| |makeAxExportForm|
            |coerceIntTableOrFunction| AND2 |isDomainForm|
            |coerceIntSpecial| |formatSpill| /TRACELET-2
            |SubstWhileDesizingList| |displayProperties|
            |coerceIntPermute| |mkErrorExpr,highlight| |formatDEF0|
            |getProplist| |coerceBranch2Union| |mkErrorExpr,highlight1|
            ASSOCIATER |coerce| |rassoc| /TRACELET-1
            |numOfOccurencesOf| |retractByFunction| |sublisR|
            |constructT| |compMapCond''| MONITOR-PRINARGS-1 |getAndSay|
            |outputComp| |intersectionContour,interProplist|
            |pspadBindingPowerOf| |isDomainInScope| |getLisplibNoCache|
            |position| |canConvertByFunction| |satDownLink| |rempropI|
            |canCoerceLocal| |getmodeOrMapping| |maxSuperType|
            |intersectionContour,compare| |formatDeftranSEQ|
            |markReduceWhile| |canCoerceTower|
            |intersectionContour,modeCompare| |markCapsuleExpression|
            /UPDATE-1 |formatDeftran| |getAbbreviation| |capsuleStack|
            |coerceInt0| |koAttrs| |objSetMode|
            |GEQNSUBSTLIST,GSUBSTinner| MONITOR-GETVALUE
            |isCategoryForm| |getLisplib| MONITOR-EVALTRAN1 |resolve|
            |formatDeftranColon| |coerceIntByMapInner| |convert|
            |getConstantFromDomain| |flatten| |valueArgsEqual?|
            |traceDomainConstructor| |npsynonym| |coerceIntByMap|
            |equalZero| |getImports,import| |replaceLast|
            |formatDeftranRepper| |coerceIntTest| |formatSeqRepper|
            |isSubTowerOf| |modeEqual| |markRemImportsAndLeadingMacros|
            |starstarcond| |markExtractLeadingMacros|
            |markEncodeChanges| |equalOne| |displayWarning|
            |evalSharpOne| |canCoerceCommute| |addContour|
            |clearDependentMaps| |makeCategoryPredicates| |formatPren1|
            |deleteAssoc| |compDefWhereClause,addSuchthat|
            |purgeNewConstructorLines| NOTEQUALLIBS
            |filterListOfStrings| |modemapPattern|
            |asyDocumentation,fn| |removeVectorElt|
            |satisfiesRegularExpressions| GETALIST |displayProplist|
            |mathPrint1| |transformAndRecheckComments| |getOp|
            |getInverseEnvironment| |displaySemanticError|
            |getSuccessEnvironment| |asySignature| |getSystemModemaps|
            |insertWOC| |asyTypeUnitDeclare| |getModemapsFromDatabase|
            |markCompSymbol| |asyCatSignature| |SubstWhileDesizing|
            |dbSpreadComments| |resolveTTUnion| |formatDollar1|
            |markStepSI| |computeAncestorsOf| |resolveTTEq|
            |markReduceStep| |descendantsOf| |rightBindingPowerOf|
            /GETOPTION |resolveTTCC| |leftBindingPowerOf|
            |reportOpsFromLisplib| |stackSemanticError|
            /GETTRACEOPTIONS |resolveTTRed| |deltaTran| /TRACELET-PRINT
            |consSig| |resolveTTSpecial| |NRTaddToSlam| MONITOR-PRINT
            |deepChaseInferences| |compareTT| |opWidth| |isConstantId|
            |NRTdescendCodeTran| |mergeAppend|
            |acceptableTypesToResolve| |resolveTCat1|
            |NRTgetLocalIndex1| |getConditionsForCategoryOnType|
            |vectorLocation| |resolveTTAny| |resolveTMOrCroak|
            |outputMapTran0| |spliceTypeListForEmptyMode|
            MONITOR-EVALTRAN |constructTowerT| |throwKeyedMsg|
            |canCoerceExplicit2Mapping| |term1RWall|
            |absolutelyCannotCoerce| |rassocSub|
            |coerceOrConvertOrRetract| |term1RW| |coerceOrRetract|
            |resolveTMTaggedUnion| |rePackageTran| |canCoerceUnion|
            |ncINTERPFILE| |acceptableTypesToResolve1|
            |updateSymbolTable| |canCoercePermute| |incAppend|
            |computeTTTranspositions| |segment1| |resolveTM2|
            |intersectionContour,unifiable| |newCanCoerceCommute|
            |getStatement| |coerceIntCommute|
            |deltaContour,contourDifference| |resolveTMRed|
            |makeCommonEnvironment,makeSameLength| |coerceInt1|
            DELLASOS |addContour,fn| |argCouldBelongToSubdomain|
            |fortranifyFunctionName| ADDOPTIONS |thisPosIsLess|
            |displayOpModemaps| APPEND-N |fortFormatTypes1| |putFTText|
            |markPretend| CONS-N $SHOWLINE |hasOption| |assoc|
            |getModemap| |intersectionContour| |diff1| SETSIZE
            |sameMsg?| |commandErrorIfAmbiguous| EVAL-DEFUN
            |intersectionContour,computeIntersection| |mkPaths|
            |mkOpVec| |markPathsEqual| EFFACE |resolveTCat|
            PRINT-AND-EVAL-DEFUN |makeCommonEnvironment|
            |AssocBarGensym| |makeLiteral| |FromTo| |isLiteral| EMBED
            |compareMode2Arg| |subCopy| |PARSE-leftBindingPowerOf|
            |getOpArgTypes,f| |mapInto| |isTowerWithSubdomain|
            |addEmptyCapsuleIfNecessary| |stringMatches?| |constructM|
            |basicMatch?| |optionError| |bootStrapError|
            |getOpArgTypes| |NRTreplaceLocalTypes| |dqAddAppend|
            |dcOpPrint| |tracelet| |predicateBitIndex,pn| |spadPrint|
            /UNTRACE-2 |augmentPredCode| |resolveTM1| |mungeAddGensyms|
            |matchMmSigTar| |htSayExpose| LEXGREATERP /UNTRACE-1
            |makeCompactSigCode| |deepSubCopy|
            |evalDomainOpPred,process| |CONTAINEDisDomain|
            |makeGoGetSlot| |union| |hasCatExpression|
            |dbShowOpHeading| PAIRTRACE |makePrefixForm| |spadUntrace|
            |getSlotFromCategoryForm| |dbShowOperationLines|
            |defaultTypeForCategory| |buildBitTable,fn| DEF-IT
            |makeCompactDirect1| STRINGSUFFIX RPLPAIR |mmCatComp|
            |augmentPredVector| |mergeSubs| |simpOrDumb| DEF-LET
            |dbReduceByForm| |hasCaty1| |dbContrivedForm| |mkObjWrap|
            |dbReduceByOpSignature| |position1| |dcOpLatchPrint|
            DEF-IS2 |reduceByGroup| CONVERSATION |defLET|
            |dbGetCondition| |defLETdcq| FOAM:|PtrMagicEQ|
            |dbGetOrigin| |sortAndReorderDmpExponents| |koCatOps| WHDEF
            |modemap2Sig| |removeListElt| |substInOrder| |everyNth|
            |pairlis| LET_ERROR |getDcForm| |defIS| |koCatAttrsAdd|
            DEF-IS-REV |getSubstInsert| |markSimpleReduce| DEF-SELECT2
            |integerAssignment2Fortran1| |markRetract| DEF-SELECT1
            |markInValue| |koOps,fn| |addInformation|
            |getAllModemapsFromDatabase| |markCompAtom|
            |varIsOnlyVarInPoly| |lookupRight| |koOps,merge| S-
            |markReduceUntil| |exp2FortOptimizeCS1,pushCsStacks|
            |makeFunctorArgumentParameters,findExtrasP|
            |fortFormatTypes| |loadLibIfNecessary| |segment2|
            |markReduceBody| |rep| |whoUses|
            |collectDefTypesAndPreds,addPred|
            |fortranifyIntrinsicFunctionName| |setMsgPrefix|
            |expression2Fortran1| |setMsgCatlessAttr|
            |dispfortarrayexp| |systemDependentMkAutoload|
            |getSignatureFromMode| |fortFormatIfGoto| |formatPren1Aux|
            |makeFunctorArgumentParameters,findExtras| |koCatAttrs|
            |makeFunctorArgumentParameters,findExtras1|
            |dbGetContrivedForm| |autoLoad| |dispfortexpj| |isMacro|
            |assignment2Fortran1| |readLib| |beenHere|
            FOAM:|fiSetDebugger| |getValueFromEnvironment|
            |dispfortexpf| |unloadOneConstructor| |htSayConstructor|
            |compileCases,FindNamesFor| |stringPrefix?|
            |asTupleNewCode| VMLISP::PUTINDEXTABLE |macroExpandList|
            VMLISP::WRITE-INDEXTABLE |setMsgForcedAttrList|
            |macSubstituteId| |consDomainName| |NRTencode|
            |compileCases,isEltArgumentIn| |consDomainForm|
            |makeFunctorArgumentParameters,augmentSig| |mkAtree3,fn|
            |mkAbbrev| CARCDREXPAND |macroExpandInPlace| |addSuffix|
            |getErFromDbL| |compJoin,getParms| |subTree| |pfMapParts|
            |mkRepititionAssoc,mkRepfun| |erMsgCompare| |compareposns|
            |UnionPrint| |pfCopyWithPos| |JoinInner|
            |mkCategoryPackage,fn| |objNewWrap| |getArgumentMode|
            |coerceByFunction| |listDecideHowMuch| |MappingPrint|
            |throwEvalTypeMsg| |parseTypeEvaluateArgs|
            |splitEncodedFunctionName| |createEnum| |decideHowMuch|
            |parseTranCheckForRecord| |getArgValue1|
            |installConstructor| |showInOut| |setMsgText| |AncestorP|
            |setMsgUnforcedAttrList| |SourceLevelSubset|
            |genDomainViewList0| |JoinInner,AddPredicate|
            |macLambda,mac| |mkAnd| DROP |macWhere,mac| |mkOr|
            |makeFunctorArgumentParameters,fn| |SigListUnion|
            |PredImplies| |makeCategoryPredicates,fn| |DescendantP|
            |findLocalVars1| |makeCategoryPredicates,fnl| |mkOr2|
            |queryUserKeyedMsg| |showInput| |getArgValueOrThrow|
            |mkFreeVar| |mac0SubstituteOuter| |SourceLevelSubsume|
            |findLocalVars| |insertPos| |compMakeCategoryObject|
            |macLambdaParameterHandling| |MachineLevelSubset| APPLYR
            |genDomainViewName| |MachineLevelSubsume| |isKeyQualityP|
            |SigListOpSubsume| |queueUpErrors| |SigEqual|
            |thisPosIsEqual| |SigListMember| |getOpArgTypes1|
            |CategoryPrint| |redundant| |mkAnd2| |markLisp|
            |categoryParts,build| |catPairUnion,addConflict|
            |clearCategoryTable1| |markMacro| |parseCases,casefn|
            |hasCat| |superSub| |encodeCategoryAlist| |simpCategoryOr|
            |tempExtendsCat| CONVERSATION1 |addDomainToTable|
            |mkCategoryOr| /EMBED-Q |formalSubstitute|
            |updateCategoryTableForDomain| |simpCatHasAttribute|
            |testExtend| |mergeOr| |newHasTest,fn| |simpOrUnion1|
            |markFindOriginalSignature| |updateCategoryTable|
            |markFindCompare| |substDomainArgs| |lassocShiftQ| |pfWDec|
            |pileForest|)) 
(PROCLAIM '(FTYPE (FUNCTION NIL FIXNUM) HEAPELAPSED)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL (VALUES T T)) GENVAR MAKE-CLOSEDFN-NAME
            |genVariable| |genSomeVariable| |genDomainVar|))
)
