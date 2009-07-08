#+:GCL
(progn
  (eval-when (:execute :compile-toplevel :load-toplevel)
                 (proclaim '(optimize (safety 1) (debug 3))))

(IN-PACKAGE "BOOT") 
(PROCLAIM '(FTYPE (FUNCTION NIL (*)) FIRST-ERROR)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FIXNUM) FOAM:|ProgHashCode| FOAM:|strLength|
            CHAR2NUM |nothingSuper| LINE-LAST-INDEX |nothingSub|
            LINE-CURRENT-INDEX |nothingWidth| |eq0| |widthSC|
            |hitListOfTarget| LINE-NUMBER)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FUNCTION) FOAM::FOAMPROGINFOSTRUCT-FUNCALL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FOAM:|SInt|)
            FOAM::FOAMPROGINFOSTRUCT-HASHVAL)) 
(PROCLAIM
    '(FTYPE (FUNCTION ((VECTOR T) (VECTOR T)) T) VMLISP::VGREATERP
            VMLISP::LEXVGREATERP)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) (VALUES T T)) FILE-GETTER-NAME
            |makeCharacter| |fetchKeyedMsg| READLINE |mapCatchName|
            |mkCacheName| |mkAuxiliaryName| FOAM:AXIOMXL-FILE-INIT-NAME
            |mkSharpVar| |makeInternalMapMinivectorName| |getKeyedMsg|
            MONITOR-INFO |queryUser|)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) (VALUES T T)) |read-line|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) FIXNUM) QENUM QSQUOTIENT QSREMAINDER
            FOAM:|SetProgHashCode|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) *) |applpar1| |appargs1| |applpar|
            |matrixBorder| |apprpar1| |apprpar| |apphor| |appagg1|
            |appvertline| LOCALNRLIB |compileAndLink|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) *) |exp2FortSpecial| |Qf2F|
            |asytranFormSpecial| |parseIf,ifTran| |templateVal|
            |asytranApplySpecial| |asytranForm| |charyTop| |charybdis|
            GETOP |getVal| BUILD-DEPSYS |htMkPath| |selectOptionLC|
            |augModemapsFromDomain1| |selectOption| |ncloopInclude0|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) *) FOAM:|fputss| FOAM:|fgetss|
            |newExpandLocalTypeArgs| |appparu| |appagg| |appargs|
            |appHorizLine| |charyTrouble| |charyTrouble1|
            |constructorAbbreviationErrorCheck| |exptApp| |inApp|
            |quoteApp| |appmat| |makeStatString| |overlabelApp|
            |overbarApp| |appparu1| |appsc| |appsetq| |slashApp|
            |appsub| |patternCheck,mknew| |binomialApp| APP |appfrac|
            |argsapp|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) *) |makeLongStatStringByProperty|
            |addModemap| INTERPSYS-IMAGE-INIT)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) *) LOCALDATABASE CONCAT |ncBug|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T T T T) *) |makeFortranFun|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T) *) |lisplibError|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) T) |Ker2Ker| |pfIf| |Factored2Factored|
            |charPosition| |Mp2Expr| |interpCOLLECTbody| |Dmp2Expr|
            |Mp2P| |subVecNodes| |Mp2Mp| PROPERTY
            |upTaggedUnionConstruct| |Dmp2Up| |upRecordConstruct|
            |upNullList| |evalCOERCE| OV2SE |incZip| |Sm2M| |Sy2NDmp|
            |Expr2Complex| |M2Rm| |optSpecialCall| |intCodeGenCoerce1|
            |Sy2P| |nextown| P2FR CARCDRX1 |Mp2Dmp| |Up2Mp| |Up2P|
            |Sy2Mp| |V2Rm| |coerceDmpCoeffs| I2EI
            |augmentLisplibModemapsFromFunctor| |Set2L| |L2Sm| |Sy2OV|
            |Var2NDmp| |evalREPEAT| L2V |evalis| |asySig1| |M2Sm|
            |Sm2Rm| |Var2OV| |P2Expr| |readLib1| |SUP2Up| |upNullTuple|
            I2OI |getOplistWithUniqueSignatures| MKPFFLATTEN-1 L2M
            |insertAlist| |L2Rm| |upLETtype| PUTALIST |Sm2L| M2L
            |Var2OtherPS| |Var2UpS| V2M |V2Sm| |substVars|
            |upLETWithFormOnLhs| |Up2SUP| MONITOR-PRINARGS |upSetelt|
            |makeEijSquareMatrix| |mkFortFn| |replaceVars|
            |domain2NDmp| |Expr2Dmp| |maprinSpecial| |evalIF| |L2Tuple|
            SMALL-ENOUGH-COUNT |asyMakeOperationAlist| |exp2FortFn|
            |Rn2F| |resolveTMEq2| |Agg2L2Agg| |coerceDmp2| |Mp2FR|
            OV2OV |Var2Up| |asGetExports| EQSUBSTLIST V2DP |L2Record|
            |Var2Mp| |coerceFFE| L2DP |L2Set| |Sm2V| M2V
            |clngammacase23| |Mp2Up| |clngammacase1| /MONITORX
            |mkIterFun| |UnionEqual| |P2Uls| |upStreamIterIN| |P2Dmp|
            |fortFormatLabelledIfGoto| |P2Mp| |MappingEqual|
            |Qf2domain| |Rm2M| M2M |Qf2EF| |Sm2PolyType| |Up2FR|
            |insertAlist,fn| |P2Upxs| |PARSE-getSemanticForm|
            |evalCOLLECT| |upLoopIterIN| |interpCOLLECT|
            |simpHasPred,simpHas| |mapLetPrint|
            |moreGeneralCategoryPredicate| |letPrint| |encodeUnion|
            |letPrint2| |outputString| |lookupInDomainByName| |longext|
            |newExpandGoGetTypeSlot| RWRITE |sigDomainVal| ADDOPTIONS
            |logH| |matchUpToPatternVars| |PsiAsymptoticOrder|
            |NRTisRecurrenceRelation| |chebf01coefmake| |clngamma|
            PRINT-XDR-STREAM |besselIcheb| |chebstarevalarr|
            |getConditionalCategoryOfType| |resolveTTEq2|
            |resolveTTEq1| |expandTypeArgs| |BesselIAsympt|
            |expandType| |throwListOfKeyedMsgs| |PsiBack| |getCatForm|
            |resolveTTRed2| |buildPredVector,fn| |chebevalarr|
            |resolveTTRed1| |BesselasymptA| |domainVal|
            |PiMinusLogSinPi| QESET |lazyOldAxiomAddChild|
            |sublisMatAlist| |algCoerceInteractive| |cotdiffeval|
            |mungeAddGensyms,fn| |pfInfApplication| |lazyDomainSet|
            |getOpCode| |getOpBindingPower|
            |throwKeyedMsgCannotCoerceWithValue| |newExpandTypeSlot|
            |infixArgNeedsParens| |setMsgUnforcedAttr| |mac0Define|
            |npList| |newExpandLocalType| |mac0InfiniteExpansion| HPUT
            |oldAxiomAddChild| |newExpandLocalTypeForm|
            |needBlankForRoot| |assocCache| |oldAxiomCategoryBuild|
            |assocCacheShift| |assocCacheShiftCount|
            |recordInstantiation| |addToConstructorCache|
            |asytranApply| MSUBST |splitConcat| SUBSTRING |hput|
            |retractUnderDomain| |recordInstantiation1| |outputNumber|
            |isRectangularList| |lassocShiftWithFunction|
            |sySpecificErrorAtToken| |compiledLookupCheck| RPLNODE
            |asytranForm1| |reportOpSymbol,sayMms| |nsubst|
            |LargeMatrixp| |unabbrevSpecialForms| |nAssocQ| |get|
            |keyedMsgCompFailureSP| |throwKeyedMsgSP|
            |throwKeyedErrorMsg| |mkUserConstructorAbbreviation|
            |rewriteMap1| |isEltable| |interpret1| |NRTcompiledLookup|
            |deleteMap| |SpadInterpretStream| |mkRecordFunList|
            |mkUnionFunList| |coerceOrFail| |updateDatabase|
            |mkMappingFunList| |coerceOrThrowFailure|
            |prepareResults,defaultValue| |mkEnumerationFunList|
            |lffloat| |displaySingleRule| |npBackTrack|
            |loadLibNoUpdate| |findLocalsInLoop| |cleanUpAfterNagman|
            |spad2BootCoerce| HREMPROP |pfWith| |rewriteMap0|
            |Var2Gdmp| |addMap| |isRectangularVector| |fortCall|
            |incPrefix?| |makeResultRecord| SETDATABASE
            FOAM:|FormatNumber| |inclmsgIfSyntax| |makeCompilation|
            |makeAspGenerators| |asyCattranOp1| |npListofFun|
            |substringMatch| |insertEntry| |makeAspGenerators1|
            |exp2Fort2| |analyzeMap0| |ncPutQ| |position,posn|
            |addBindingInteractive| |pfTLambda| |orderPredTran|
            |getfortarrayexp| |augProplist| |orderPredicateItems|
            |mapRecurDepth| |augLisplibModemapsFromCategory|
            |pileForests| |displayMap| |compileDeclaredMap|
            |fortFormatHead| |compileCoerceMap|
            |restoreDependentMapInfo| |analyzeNonRecur| |mkInterpFun|
            DEF-INNER |compiledLookup| |mkAtree3| |substring?| B-MDEF
            |anySubstring?| |interpret2| |patternCheck,equal|
            |insertShortAlist| |stringMatch| |addIntSymTabBinding|
            |rightCharPosition| |stringPosition| |coerceIntX| |get1|
            |matchSegment?| |get0| |getValueFromSpecificEnvironment|
            |putAtree| |infix?| |interpRewriteRule| |addBinding|
            |commandErrorMessage| VMLISP::MAKE-ENTRY |putI|
            |coerceSubDomain| |mkAtree2| |dcSig|
            |getSubDomainPredicate| SPADRWRITE |canCoerceByFunction1|
            |NRTcompileEvalForm| |extendsCategoryBasic|
            |filterAndFormatConstructors| |catExtendsCat?| ADDASSOC
            |transferPropsToNode,transfer| |remprop| |recordOldValue|
            |pfWIf| |recordNewValue| |intloopSpadProcess,interp|
            AS-INSERT |get2| AS-INSERT1 |stuffSlot| |recordOldValue0|
            |coerceOrCroak| |displayModemap| |extendsCategoryBasic0|
            |evalMmCond| |intloopProcess| |displayType|
            |substSlotNumbers| |EnumEqual| |matchTypes| |displayMode|
            |NRTextendsCategory1| |RecordEqual| |displayCondition|
            |extendsCategory| |constrArg| |displayValue| |hasSigOr|
            |hasSigAnd| |getLocalMms| |mkNewUnionFunList| |hasAtt|
            |NRTgetLookupFunction| |argumentDataError| PUT |hasAttSig|
            |fnameMake| |evalMmCond0| |buildPredVector|
            |userLevelErrorMessage| |oldAxiomCategoryNthParent|
            |evalMmCat1| |fnameNew| |oldAxiomPreCategoryBuild|
            |augmentSub| |coerceTypeArgs| |pfLambda| |getLocalMms,f|
            |findCommonSigInDomain| |findUniqueOpInDomain| |writeLib1|
            THETACHECK DEF |makeCatPred| |lisplibWrite|
            |getMappingArgValue| ELEMN |altTypeOf| |getArgValueComp|
            |filterListOfStringsWithFn| |hasCaty|
            |filterModemapsFromPackages| SUBSTEQ |hasCate|
            |unifyStruct| |displayModemap,g| |unifyStructVar| |domArg2|
            SUBLISLIS |commandAmbiguityError| |isOpInDomain|
            |pushDownOnArithmeticVariables| |setMsgForcedAttr|
            |permuteToOrder| |computeTTTranspositions,compress|
            |selectMmsGen,exact?| |sayFunctionSelectionResult|
            |centerString| |Dmp2Mp| |intloopInclude0| I2NNI |OV2Sy|
            |Expr2Up| |NDmp2NDmp| |augProplistInteractive| |Complex2FR|
            |algEqual| |pushDownTargetInfo| |Dmp2NDmp| |Rm2L|
            |expandSPADREDUCE| |hasFileProperty;|
            |hasFilePropertyNoCache| |OV2poly| REDUCE-1 |Qf2PF|
            |isLegitimateMode;| |flowSegmentedMsg| |Expr2Mp|
            |coerceConvertMmSelection;| |Ker2Expr|
            |Complex2underDomain| |recordNewValue0| |selectMms| |P2Up|
            SPADRWRITE0 |Up2Dmp| |Var2Dmp| |postCollect,finish|
            |Sy2Var| |Un2E| |getFunctionFromDomain| DP2DP |Var2SUP|
            |Rm2V| |NDmp2domain| |Up2Up| |Var2P| |Var2FS| |P2Uts|
            |npAndOr| |errorSupervisor1| |countParens| |assignSymbol|
            |upwhereMain| |rread| |rwrite| |application2String|
            |upwhereMkAtree| |upwhereClause| |upTableSetelt|
            |evalFormMkValue| |rewriteMap| |sideEffectedArg?|
            |evalQUOTE| |Complex2Expr| |writeStringLengths| |evalSEQ|
            |Sy2Dmp| |Sy2Up| |npParenthesize| |coerceTraceArgs2E|
            |evalIsntPredicate| |writeXDR| V2L |evalIsPredicate|
            |Qf2Qf| |Up2Expr| |pfPushBody| I2PI |IFcodeTran| |Agg2Agg|
            |compSPADSLAM| OV2P |traceDomainLocalOps| |Scr2Scr|
            |coerceTraceFunValue2E| |Rm2Sm| |Var2QF| |Dmp2Dmp| |Dmp2P|
            |nopile|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) T) |listSort| |categoryParts|
            |tokConstruct| |pfLeaf| NREMOVE |remove| RREAD |pfAdd|
            |wasIs| MATCH-TOKEN BPITRACE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T) T) /MONITOR |writeCFile|
            |Mp2MpAux2|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) T) |commuteComplex|
            |commutePolynomial| |compileRecurrenceRelation|
            |interpLoopIter| |commuteSquareMatrix| |compileIF|
            |fortFormatDo| |commuteDistributedMultivariatePolynomial|
            |commuteMPolyCat| |coerceDmp1| |compileADEFBody|
            |newLookupInDomain| |concatTrouble| |charyBinary|
            |getNewDefaultPackage| |split2| |newCompareSig| |logS|
            |resolveTT2| |lazyMatchArg2| |newLookupInTable|
            |hashNewLookupInTable| |needStar| |concatApp1| |xlSay|
            |xlOK1| |xlMsg| |selectMms1;| |aggregateApp| |selectMms2|
            |incLine| |bottomUpForm0| |bottomUpFormRetract| LOCALASY
            |augmentMap| |analyzeRecursiveMap| |bottomUpFormTuple|
            |prepareResults| |bottomUpFormUntaggedUnionRetract|
            |bottomUpForm| |bottomUpForm2| |bottomUpForm3| |spadify|
            |incLude| |analyzeDeclaredMap| |xlConStill| |xlConActive|
            |xlFileCycle| |xlCannotRead| |xlNoSuchFile|
            |reportFunctionCompilation| |bottomUpFormAnyUnionRetract|
            |coerceByTable| |bottomUpDefaultCompile|
            |bottomUpDefaultEval| |makeConstrArg| |putSrcPos|
            |printLabelledList| |mkCacheVec| |selectMmsGen,matchMms|
            |orderMms| |sayFunctionSelection|
            |commuteSparseUnivariatePolynomial|
            |commuteUnivariatePolynomial| |commuteQuaternion|
            |commuteMultivariatePolynomial| |commuteFraction|
            FINCOMBLOCK |getArgValueComp2| -REDUCE
            |commuteNewDistributedMultivariatePolynomial|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) T) |MpP2P| STRPOS |evalUntargetedADEF|
            |evalTargetedADEF| |optCallSpecially| |interpLoop|
            |nextown2| |reportFunctionCacheAll| |put| |evalTuple|
            |interpREPEAT| |fixUpPredicate| |mergeSort| |termMatch|
            |collectSeveralStreams| |mkIterZippedFun|
            |mkAndApplyZippedPredicates| |collectOneStream|
            |upStreamIterSTEP| |P2Us| |evalconstruct|
            |evalInfiniteTupleConstruct| |interpCOLLECTbodyIter|
            |evalTupleConstruct| |mkInterpTargetedADEF| |catPairUnion|
            |compileTargetedADEF| |newLookupInCategories|
            |nrunNumArgCheck| |simpHasSignature| |upLoopIterSTEP|
            |collectStream| |collectStream1| |letPrint3|
            |charySemiColon| |lazyMatchArg| |charyEquatnum|
            |charySplit| |charyMinus| |compareSig|
            |lazyCompareSigEqual| |getConditionalCategoryOfType1|
            |lookupInTable| |say2Split| |lookupDisplay|
            |oldCompLookupNoDefaults| |BesselasymptB| |lazyMatch|
            |lazyMatchArgDollarCheck| |typeToForm,fn| |appsum|
            |appelse| |appInfix| |appSum| |lookupIncomplete|
            |clngammacase2| |newLookupInAddChain| |sayLooking|
            |hashNewLookupInCategories| |newLookupInCategories1|
            |mac0MLambdaApply| |mac0ExpandBody| |lookupComplete|
            |compareSigEqual| |basicLookupCheckDefaults|
            |lazyMatchAssocV| |asGetModemaps| |haddProp| |appconc|
            |asytranCategoryItem| |asytranDeclaration|
            |asytranCategory| |appChar| |sigma2App| |xlOK| |xlSkip|
            |intApp| |xlPrematureEOF| |indefIntegralApp| |xLate|
            |piApp| |condUnabbrev| |pi2App| |boxLApp| |bracketApp|
            |braceApp| |charyElse| ASHARPMKAUTOLOADFUNCTOR
            ASHARPMKAUTOLOADCATEGORY |analyzeNonRecursiveMap|
            |canCoerceTopMatching| |protectedNagCall| STRPOSL |zagApp|
            |constoken| |prepareData| |npEnclosed| |clearDep1|
            |makeInternalMapName| |plusApp| |timesApp| MAKE-FLOAT
            |stepApp| |bigopWidth| |aggApp| |xlCmdBug| |xlIfBug|
            |axiomType| |concatbApp| |concatApp| |xlPrematureFin|
            |xlSkippingFin| |analyzeUndeclaredMap| |xlConsole|
            |stringApp| |semchkProplist| |sigmaApp| |mergeInPlace|
            |augProplistOf| |putHist| |nothingApp| |rootApp|
            |superSubApp| |vconcatapp| |coerceImmediateSubDomain|
            |putIntSymTab| |srcPosNew| |matchAnySegment?| |evalForm|
            |selectLocalMms| |bottomUpDefault| |appneg| |binomApp|
            |altSuperSubApp| |intloopSpadProcess| |evalMmFreeFunction|
            |boxApp| |centerApp| |analyzeMap| |appext| |hasSig|
            |evalMmCat| |getFileProperty| |hasCateSpecial| |hasCate1|
            |basicLookup| |hasCateSpecialNew| |lookupInDomainVector|
            |oldCompLookup| REDUCE-N |putFileProperty| REDUCE-N-1
            REDUCE-N-2 |getArgValue2| |compClam| |defaultTarget|
            |selectDollarMms| |domArg| |matchMmSig| |allOrMatchingMms|
            |selectMmsGen| |mkDomPvar| |evalMm| |catchCoerceFailure|
            |interpIF| |getReduceFunction| |NRTgetMinivectorIndex|
            |printCName| |writeMalloc| |printDec|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T) T) |Mp2MpAux1| |Mp2MpAux0|
            |Mp2SimilarDmp| |Expr2Dmp1| |mkDiffAssoc|
            |lazyOldAxiomDomainLookupExport|
            |oldAxiomDomainLookupExport| |appInfixArg| |bigopAppAux|
            |abbreviationError| |invokeNagman| |findFunctionInCategory|
            |findFunctionInDomain|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) T) |upDollarTuple| |bracketagglist|
            |BesselIBackRecur| |incLine1| |nagCall| |xlIfSyntax|
            |makeFort| |invokeFortran| |genMapCode| |putMapCode|
            |compDefineLisplib| |oldAxiomCategoryLookupExport|
            |compHash| |mmCost0| |findFunctionInDomain1| |mmCost|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T *) T) |ncHardError| |lnCreate|
            |asCategoryParts| |ncSoftError| TOKEN-INSTALL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T) T) |P2MpAux| |makeFort1|
            BUILD-INTERPSYS)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T T) T) |P2DmpAux|
            |makeSpadFun|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T *) T) RPLACSTR)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T *) T) |msgCreate|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL *) |listConstructorAbbreviations|
            |PARSE-Sexpr1| |PARSE-Selector| |/RQ,LIB| /RF-1
            |PARSE-Label| |PARSE-Sexpr| |readSpadProfileIfThere| HELP
            |npMDEFinition| |describeSetStreamsCalculate|
            |npDefinitionItem| |npDefn| |npMacro| RECLAIM
            |sendHTErrorSignal| |testPage| |executeQuietCommand|
            |copyright| |pquit| |quit| |sendNagmanErrorSignal|
            |generateResultsName| |generateDataName| MKPROMPT
            |princPrompt| |scanS| |displayFrameNames| RESETHASHTABLES
            |printStatisticsSummary| |printStorage| |leaveScratchpad|
            |clearCmdAll| |queryClients| |disableHist| |quitSpad2Cmd|
            |pquitSpad2Cmd| |PARSE-Leave| |PARSE-Category|
            |PARSE-Enclosure| /RQ /RF |PARSE-Primary1| |npPrimary1|
            |npRule| |npCategory|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL T) |saveUsersHashTable| |npBacksetElse|
            |npDefaultItem| |npDefaultDecl| |npTypeVariable|
            |npRestrict| |npSum| $TOTAL-GC-TIME |pspacers| FAIL
            |displayHiddenConstructors| |mkOutputConsoleStream|
            |currentSP| |extendConstructorDataTable| /EMBEDREPLY
            |stopTimer| |returnToReader| |PARSE-Infix| |PARSE-Prefix|
            REDUCE-STACK-SHOW CURRENT-SCANNER-CHAR |PARSE-Form|
            |PARSE-Reduction| |PARSE-String| |PARSE-IntegerTok|
            CURRENT-SYMBOL |simpCategoryTable| |simpTempCategoryTable|
            RESET-HIGHLIGHT |setViewportProcess| |PARSE-AnyId|
            |PARSE-Primary| |PARSE-PrimaryNoFloat| |cc|
            |PARSE-IteratorTail| |PARSE-Iterator| |PARSE-Statement|
            |PARSE-Float| |PARSE-Name| |PARSE-Application|
            |waitForViewport| |PARSE-ReductionOp| |PARSE-Seg|
            |PARSE-OpenBrace| |PARSE-Sequence1| |PARSE-OpenBracket|
            |PARSE-Suffix| |PARSE-TokTail| |PARSE-InfixWith|
            |PARSE-With| |nangenericcomplex| CLEAR-HIGHLIGHT
            |dcSizeAll| EMBEDDED |makeInitialModemapFrame|
            |statisticsInitialization| |initializeInterpreterFrameRing|
            |spadStartUpMsgs| RESTART0 |spad| MONITOR-HELP MONITOR-END
            |getCodeVector| MONITOR-AUTOLOAD MONITOR-PERCENT
            |npVariableName| |npDecl| |npName| |npTypeVariablelist|
            /TRACEREPLY |processGlobals| |npIterators|
            |processGlobals1| |sayAllCacheCounts| |npWhile|
            |describeSetFunctionsCache| |npForIn| |npMDEF| |npProduct|
            |npExpress| |describeSetOutputAlgebra| |npExpress1|
            |describeAsharpArgs| |npSigDecl| |npType| |npComma|
            |npAssignVariablelist| |npSQualTypelist|
            |describeSetOutputOpenMath| |npSDefaultItem|
            |describeSetLinkerArgs| |npColonQuery| |npMatch|
            |describeSetOutputTex| |npSignature| |npSigItemlist|
            FOAM:|fiGetDebugVar| |npPop3|
            |statRecordInstantiationEvent| |npPower| |quadSch|
            |npSegment| |npPushId| |npDef| |reportInstantiations|
            |processSynonyms| |npImport| |npTyping| |PARSE-NewExpr|
            |asList| YEARWEEK |clearHashReferenceCounts| CURRENTTIME
            |version| |clearCategoryCaches| |syGeneralErrorHere|
            |clamStats| |cacheStats| |reportAndClearClams| |rbrkSch|
            |lbrkSch| |removeAllClams| |inclmsgIfBug|
            |loadExposureGroupData| FRICAS-INIT |inclmsgConsole|
            |inclmsgFinSkipped| |mkLowerCaseConTable|
            |describeInputLibraryArgs| |inclmsgCmdBug|
            |describeSetFortTmpDir| |describeSetFortDir| |credits|
            |describeSetOutputFortran| |printableArgModeSetList|
            |describeSetOutputFormula| |intloop| |npPPg|
            |describeSetOutputMathml| |npPop1|
            |describeOutputLibraryArgs| |piles| |resetStackLimits|
            |npPCg| |scanError| |npTrap| |scanEscape| |npState|
            |scanEsc| WRITE-WARMDATA |pfNothing| |scanNumber|
            WRITE-INTERPDB |npPop2| |npVariable| |npCategoryL| |npMdef|
            |scanDictCons| |npDefTail| |scanToken| WRITE-CATEGORYDB
            |allOperations| |npQuiver| WRITE-OPERATIONDB |scanString|
            WRITE-BROWSEDB |scanSpace| |npPileExit| |scanPunct|
            INITIAL-GETDATABASE CATEGORYOPEN |npStatement| BROWSEOPEN
            |npAssign| |scanNegComment| OPERATIONOPEN
            |startsNegComment?| WRITE-COMPRESS |scanComment|
            |npDefinitionOrStatement| |startsComment?| |npAtom2|
            |npFromdom| |npPrefixColon| |npAmpersand| |npInfixOperator|
            |npPretend| |npExit| |scanPunCons| |npDefinition|
            |npPrimary2| |npPPf| |initialiseIntrinsicList|
            |computeElapsedTime| |scanKeyTableCons| |tempLen|
            |npRecoverTrap| |incConsoleInput| CURRENT-TOKEN
            |displayExposedConstructors| |displayExposedGroups|
            |displayHeapStatsIfWanted| |nextInterpreterFrame|
            |peekTimedName| |newFortranTempVar| FRICAS-RESTART
            |popTimedName| |npDisjand| |getIntrinsicList|
            |previousInterpreterFrame| |npConstTok|
            |resetWorkspaceVariables| |initHist| |computeElapsedSpace|
            |npLogical| |initNewWorld| |statisticsSummary| |npInfixOp|
            COMPRESSOPEN |npQualType| |synonymSpad2Cmd| INTERPOPEN
            CREATE-INITIALIZERS SPAD_SHORT_ERROR |npLambda| NEXT-TOKEN
            |getParserMacros| |clearFrame| SPAD_LONG_ERROR |npItem|
            IOSTAT |npADD| |pfNoPosition| ERRHUH
            |npConditionalStatement| |poNoPosition| IS-GENSYM
            PARSE-IDENTIFIER |npTagged| NEXT-SCANNER-CHAR ADVANCE-TOKEN
            |npLocalDecl| TOKEN-STACK-SHOW |npSelector| |npDiscrim|
            |npQualifiedDefinition| |spadpo| |intNewFloat|
            |npSLocalItem| |npLocalItemlist| |npAtom1| |npDollar|
            |npPDefinition| BUMPCOMPERRORCOUNT |npSigItem|
            |intSetQuiet| |createResolveTTRules| |runspad|
            |createResolveTMRules| |createTypeEquivRules|
            |makeConstructorsAutoLoad| |initializeRuleSets|
            |npVariablelist| |npSemiBackSet| |npExport| |intUnsetQuiet|
            |npQualDef| NEXT-LINES-CLEAR |npLocalItem| POP-REDUCTION
            |npLocal| |npApplication2| |npInline| |npDefinitionlist|
            |npSingleRule| PARSE-SPADFLOAT PARSE-ARGUMENT-DESIGNATOR
            PARSE-KEYWORD |writeHistModesAndValues| |frameNames|
            MONITOR-REPORT |writeHiFi| MONITOR-READINTERP
            |reportWhatOptions| MONITOR-RESULTS ADVANCE-CHAR
            CURRENT-CHAR |updateInCoreHist| BOOT-SKIP-BLANKS
            NEXT-LINES-SHOW NEXT-CHAR |ncIntLoop| GET-INTVAL
            MONITOR-UNTESTED |ncError| PARSE-SPADSTRING
            |getWorkspaceNames| MONITOR-INITTABLE PARSE-NUMBER
            |intSetNeedToSignalSessionManager| |statRecordLoadEvent|
            |reportCount| |getInfovecCode| |initHistList|
            |NRTmakeCategoryAlist| |NRTgenFinalAttributeAlist| TOP
            |displayPreCompilationErrors| |ncTopLevel| INITIALIZE
            |updateCurrentInterpreterFrame| |clearCmdSortedCaches|
            |clearCmdCompletely| |resetInCoreHist|
            |initializeSystemCommands|
            |updateFromCurrentInterpreterFrame|
            |createCurrentInterpreterFrame| |genTempCategoryTable|
            |allConstructors| |getSystemCommandLine|
            |getParserMacroNames| |genCategoryTable|
            $TOTAL-ELAPSED-TIME |updateHist| |clearMacroTable|
            |displayWorkspaceNames| |sayShowWarning| |setOptKeyBlanks|
            |clearConstructorAndLisplibCaches| |clearConstructorCaches|
            |clearClams| |prTraceNames| |interpFunctionDepAlists|
            |PARSE-Conditional| |PARSE-ElseClause| |historySpad2Cmd|
            |PARSE-Exit| |voidValue| |histFileName| |PARSE-SemiColon|
            |PARSE-Sequence| |PARSE-Return| |PARSE-FormalParameter|
            |PARSE-FormalParameterTok| |oldParserAutoloadOnceTrigger|
            TERSYSCOMMAND |oldHistFileName| |PARSE-Expression|
            |PARSE-LabelExpr| |PARSE-Data| |PARSE-Quad| |spadThrow|
            INIT-BOOT/SPAD-READER |PARSE-Import| |PARSE-Loop|
            |PARSE-VarForm| |PARSE-Qualification|
            |terminateSystemCommand| |spadPrompt| |fin|
            |getInterpMacroNames| |npSuch| |npSynthetic|
            |npAmpersandFrom| |checkWarningIndentation| |npBy|
            |npIterator| |npSuchThat| |npEncl| |npBDefinition|
            |returnToTopLevel| |npSCategory| |npApplication|
            |traceReply| |npPrimary| |sayNewLine| |npDefaultValue|
            |npNext| |npFirstTok| |npPCff| |npAssignVariableName|
            |makeClosedfnName| |?t| |npVoid| |npTerm| |pcounters|
            |npCoerceTo| |npRemainder| |random| |npPileDefinitionlist|
            |npGives| |npFree| |npDefaultItemlist| |npRelation|
            |resetCounters| |npAssignVariable| |resetTimers|
            |coercionFailure| |npBPileDefinition| |npReturn|
            |resetSpacers| |npFix| |ptimers| |npLet| |npArith|
            |npIterate| |npCommaBackSet| |startTimer|
            |npSignatureDefinee| |npLoop| |clock| |spadReply|
            |npQualTypelist| |npPPff| |npTypeStyle| |npSymbolVariable|
            |npInterval| |npId| |npBreak| |npColon| |npAssignment|
            |npTypified| |saveDependentsHashTable|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) *) |newGoGet| META-SYNTAX-ERROR |Undef|
            NEXT-LINE $ERASE INIT-MEMORY-CONFIG)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) *) |getBrowseDatabase| INIT-LIB-FILE-GETTER
            |clearCmdParts| |abbQuery| |spadcall1| |optCallEval|
            MONITOR-EVALAFTER |fortPre1| |NRTinnerGetLocalIndex|
            MONITOR-PRINTREST |fortPreRoot| |assignSlotToPred|
            |checkPrecision| |fix2FortranFloat| |outputTran|
            |fortFormatCharacterTypes,mkCharName|
            |exp2FortOptimizeArray| |ExecuteInterpSystemCommand|
            |InterpExecuteSpadSystemCommand| |printBasic|
            |parseLeftArrow| |parseTranList| |parseUpArrow| |parseLhs|
            |brightPrintHighlight| |postSlash| |postConstruct|
            |brightPrintCenter| |brightPrintHighlightAsTeX|
            |postBigFloat| |brightPrint0AsTeX| |escapeSpecialChars|
            |halfWordSize| |sayDisplayWidth| |sayDisplayWidth,fn|
            |sayDisplayStringWidth| |postTran| |postInSeq| |npPP|
            |NRTevalDomain| |setExposeDrop| |setExposeAdd|
            |setFunctionsCache| |linkToHTPage| |startHTPopUpPage|
            |parseLessEqual| BPINAME |parseGreaterEqual| OBEY
            |setExpose| |parseNotEqual| |obj2String|
            |setStreamsCalculate| |parseAnd| |setOutputOpenMath|
            |setLinkerArgs| |setOutputTex| |getMsgPos2|
            |mac0InfiniteExpansion,name| |parseIf| |formString|
            |form2StringWithPrens| |parseNot| |parseOr| |superspan|
            |prefix2String| |form2StringAsTeX| |form2String1|
            |parseSeq| |subspan| |entryWidth| |mathprintWithNumber|
            |prefix2StringAsTeX| |parseTran| |clearClam| |startHTPage|
            |issueHT| |startReplaceHTPage| |killHTPage| MAKE-OUTSTREAM
            |asyGetAbbrevFromComments| |intern| SRCABBREVS |aggwidth|
            |asyJoinPart| |parseAndEvalStr| |parseAndEvalStr1|
            |mathPrint| |protectedEVAL| |outformWidth|
            |formatAttributeArg| |incHandleMessage| |clear| |close|
            |compQuietly| |compileFileQuietly| |display|
            |compileQuietly| |frame| |setOutputFortran| |history|
            |setOutputFormula| |setOutputMathml| |postCategory,fn|
            MAKE-APPENDSTREAM |phBegin| MAKE-INSTREAM |savesystem|
            |set| |show| |summary| FILE-RUNNER |what| |spleI| |apropos|
            |exptSub| INIT-FILE-GETTER BOOT-LOAD
            |makeSimplePredicateOrNil| |aggSub| |aggSuper| |transSeq|
            |quoteSub| |ncAlist| |quoteSuper| |ncTag|
            VMLISP::DELETE-DIRECTORY |numArgs| GET-TOKEN |matSub|
            VMLISP::GET-IO-INDEX-STREAM |addNewInterpreterFrame|
            VMLISP::GET-INPUT-INDEX-STREAM |restoreHistory|
            |changeHistListLen| |showHistory| |saveHistory|
            |normalizeStatAndStringify| |mkParameterList,par2string|
            |setHistoryCore| |roundStat| |tokType|
            |fortFormatCharacterTypes,par2string| |importFromFrame|
            |overlabelWidth| |sayString| |overbarWidth| |fortError1|
            |rootSub| |htCommandToInputLine| |lnFileName|
            |pfGlobalLinePosn| MAKE-REASONABLE |abbreviations|
            |compiler| |npPC| DEFTRAN |cd| DEF-WHERECLAUSELIST
            |justifyMyType| |verbatimize| DEF-ISNT |subSuper|
            |setOutputAlgebra| |vConcatSuper| WIDTH MONITOR-RESTORE
            |numMapArgs| |writifyComplain| |agggsub| |agggsuper|
            |agggwidth| |transHasCode| DEF-MESSAGE1 |timedEvaluate|
            |poGlobalLinePosn| |qTSub| |qTSuper| BRIGHTPRINT-0
            |histFileErase| |parseTransform| |sayMSGNT| |clearSpad2Cmd|
            |initializeLisplib| |displaySpad2Cmd| |popSatOutput|
            |reportOpsFromUnitDirectly0| |doSystemCommand|
            |whatSpad2Cmd| |fixObjectForPrinting|
            |mkEvalableCategoryForm| |tabsToBlanks| |porigin| |pfname|
            |pfFileName| |replaceSharpCalls| |doReplaceSharpCalls|
            |showSpad2Cmd| |getMsgTag| |unAbbreviateKeyword|
            |compileAsharpCmd1| |withAsharpCmd|
            |reportOpsFromUnitDirectly1| |systemCommand| |poFileName|
            MAKE-DEPSYS |sayWidth,fn| |frameSpad2Cmd|
            |brightPrintCenterAsTeX| |brightPrint0| |safeWritify|
            |formatSignatureArgs0| |formatSignatureArgs| |postMakeCons|
            |formatOpType| |editFile| |sayWidth| |mkEvalable|
            |reportOpSymbol| |form2StringLocal| |unparseInputForm|
            |tuple2String,f| |mathObject2String| |object2String|
            |NRTtypeHack| |pred2English| |form2String| |prefix2String0|
            |subrname| |abbreviationsSpad2Cmd|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) T) |incRgen1| |next1| |incZip1| INTERRUPT
            |incAppend1| |nextown1| |incIgen1| RKEYIDS CROAK MAKE-LINE
            MAKE-XDR-STREAM |dcSize| |sum| |Mapping| MONITOR-TESTED
            ENABLE-BACKTRACE |runOldAxiomFunctor| MONITOR-RESET
            MONITOR-DISABLE $FILEP |RecordCategory|
            |EnumerationCategory| |UnionCategory| NEXT-BOOT-LINE
            IOCLEAR |canCoerce| |canCoerceFrom|
            |coerceConvertMmSelection| |hasFileProperty|
            |isLegitimateMode| |resolveTT| |selectMms1| |Union|
            MAKE-DATABASE |synonym| |incLude1| INITROOT READ-A-LINE
            META-META-ERROR-HANDLER SAY |start| MAKE-STACK MOAN
            MAKE-TOKEN MAKE-REDUCTION RDEFINSTREAM RDEFOUTSTREAM
            |Enumeration0| MAKE-MONITOR-DATA SPAD_SYNTAX_ERROR
            MONITOR-ENABLE |dc| TOPLEVEL |buildBitTable|
            |displayCategoryTable| BOOT |spadCompile| |throwMessage|
            |concat| FOAM::MAKE-FOAMPROGINFOSTRUCT
            VMLISP::MAKE-LIBSTREAM)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) CHARACTER) NUM2CHAR LINE-CURRENT-CHAR EBCDIC)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) FIXNUM) LINE-NEW-LINE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) STRING) LINE-BUFFER |make_spaces|
            |stripSpaces|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) T) |bootAND,flatten| |traceSpad2Cmd|
            |bootAND| |TruthP| |alqlGetOrigin| |helpSpad2Cmd|
            |readSpad2Cmd| |shortenForPrinting| |alqlGetKindString|
            |optCall| NMSORT |transOnlyOption| |stackTraceOptionError|
            |makeLazyOldAxiomDispatchDomain| UNVEC |interpIter|
            |alqlGetParams| |npParened| |incRgen| DATABASE-CONSTRUCTOR
            |coerceSpadArgs2E| DATABASE-ANCESTORS
            |getBpiNameIfTracedMap| |upAlgExtension| DROPENV
            |pfSymbolSymbol| |flattenOperationAlist| SHOWBIND
            |pfSymbol?| DATABASE-SPARE |spadTrace,g| |firstTokPosn|
            |rulePredicateTran| |StreamNull| DATABASE-DEPENDENTS
            |ruleLhsTran| VMLISP::REMOVE-FLUIDS DATABASE-USERS
            |domainToGenvar| |unabbrevAndLoad| |genDomainTraceName|
            SHOWDATABASE |prTraceNames,fn| |postAtom|
            |getTraceOption,hn| |unTuple| |upCOERCE| CHARP |npNull|
            |upor| |untraceAllDomainLocalOps| |optCond|
            FOAM-USER::|AXL-spitSInt| |clearSlam| |upconstruct|
            |incIgen| |isFreeFunctionFromMmCond| RPACKFILE
            |isFreeFunctionFromMm| |upDeclare| |getCacheCount|
            |isDomainSubst| |optimize| |string2Float| |falseFun|
            |hashCount| |formal2Pattern| |upADEF| |optRECORDCOPY|
            |optimizeFunctionDef,removeTopLevelCatch|
            |flattenSignatureList| |pfRestrict?| |mkDatabasePred|
            |pfDefinition?| |listOfPatternIds| |pfAssignRhs|
            |mkAlistOfExplicitCategoryOps| |pf0AssignLhsItems|
            RECOMPILE-LIB-FILE-IF-NECESSARY |getFirstArgTypeFromMm|
            LIBSTREAM-DIRNAME |isLocalPred| |asyCATEGORY|
            |clearAllSlams| MONITOR-EVALBEFORE |upbreak|
            VMLISP::LIBSTREAM-INDEXSTREAM |getUsersOfConstructor|
            |upTuple| |asyTypeJoin| |bool| PNAME |hackToRemoveAnd|
            HAS_SHARP_VAR |mkAlistOfExplicitCategoryOps,atomizeOp|
            |abbreviation?| |dropLeadingBlanks| |upfree|
            |asyExportAlist| MAKE-BVEC |removeConstruct| |asyArgs|
            |isSharpVarWithNum| MSORT |asyArg| |asyAncestors|
            |gensymInt| IS_SHARP_VAR |moveORsOutside| |isTupleForm|
            |asyAncestorList| |getfortexp1| |isExposedConstructor|
            |removeZeroOne| |expression2Fortran| |asyConstructorArgs|
            |asyCosig| |removeZeroOneDestructively| |fortFormatElseIf|
            |asyCosigType| |trimComments| |formatUnabbreviatedTuple|
            |NRTaddInner| |interactiveModemapForm,fn| |trimString|
            |formatUnabbreviated| FOAM:|printNewLine| |isHomogeneous|
            |asyIsCatForm| |formatUnabbreviatedSig| |addConsDB|
            |asySubstMapping| |length2?|
            |NRTgetOperationAlistFromLisplib| |LZeros| |isFloat|
            |markUnique| |upLETWithPatternOnLhs| |makeSpadConstant|
            |upequation| |asTupleNew0| |fortFormatIf| |uplocal| |opOf|
            |fortFormatCharacterTypes,mkParameterList2| |mapConsDB|
            |asyTypeJoinItem| |mkMat| |asyTypeJoinPartIf|
            |asytranLiteral| |asytranEnumItem| |interpOp?|
            |asyTypeJoinPartExport| |asIsCategoryForm| |fortSize|
            |resolveTMRed1| |zeroOneConversion| |fortSize,elen|
            |fortexp0| |getAttributesFromCATEGORY| |nameLen|
            |pfAssign?| |exp2FortOptimizeCS1,popCsStacks| |pfDoBody|
            |asyExtractDescription| |pfDo?| FOAM::INSERT-TYPES
            |pfSuchthatCond| /UNEMBED-1 |pp| |fortFormatTypes,unravel|
            |interactiveModemapForm| |fortFormatCharacterTypes|
            /UNEMBED-Q |asyMkpred| |mkZipCode| |asyLooksLikeCatForm?|
            |objValUnwrap| |fortFormatIntrinsics| |asyCattran1|
            |exp2FortOptimizeCS| |mkAndApplyPredicates| |asAll|
            |exp2FortOptimizeCS1| |upStreamIters| |evalDomain|
            |stripUnionTags| |dispfortexp1| |displayDatabase|
            |asyTypeMapping| |coerceMap2E| |eq2AlgExtension|
            FOAM:|Halt| |constructor?| |isOkInterpMode| |asyCatItem|
            |isRecord| |mkNiladics| |asyType| |functionp| |list1|
            |upand| |depthAssoc| |macrop| |bassertNot|
            |interpOnlyCOLLECT| |removeAttributePredicates| |ordList|
            |upCOLLECT| |makePredicateBitVector| |dnf2pf| |be|
            |evalLoopIter| |reduceDnf| |pathname?| OPTIONS2UC LINE-P
            |mkCategoryExtensionAlistBasic| |isFunctor| |testPredList|
            |getCategoryExtensionAlist0| IDENT-CHAR-LIT |b2dnf|
            |isFormalArgumentList| |bnot| |spadThrowBrightly|
            |upTARGET| |clearCategoryTable| |bor| |upcase|
            |PARSE-LedPart| |simpHasPred,simpDevaluate| |band| EQUABLE
            |hasIdent| |isUncompiledMap| |bassert| |upCOLLECT0|
            |intProcessSynonyms| |PARSE-NudPart| |rightTrim|
            |isInterpOnlyMap| |upCOLLECT1| |leftTrim|
            |isDomainOrPackage| |upLoopIters| |notDnf|
            |simpCatPredicate| |pfFileName?| |simpOrUnion| |notCoaf|
            VMLISP::FLAT-BV-LIST |PARSE-GliphTok| VMLISP::EQUABLE MKQ
            |getCategoryExtensionAlist| |list3| STACK-UPDATED
            |mkCategoryExtensionAlist| |isSubForRedundantMapName|
            |isStreamCollect| STACK-SIZE |list2| |simpHasPred,eval|
            |mkNestedElts| |iterVarPos| XDR-STREAM-P |prove|
            CACHEKEYEDMSG |simpHasPred,simp| |isInitialMap| STACK-STORE
            |printMap| /VERSIONCHECK ACTION |categoryParts,exportsOf|
            COND-UCASE |PARSE-NBGliphTok| OPTIONAL |parseType|
            |nodeCount| |newHasTest,evalCond| |parseHasRhs|
            |compressHashTable| TRACEOPTIONS |parseJoin,fn|
            |mkCircularAlist| |updateCategoryTableForCategory|
            /UNTRACE-0 |simpBool| MAKE-SYMBOL-OF |parseTypeEvaluate|
            |addToCategoryTable| |parseHas,fn| |PARSE-Expr| |transIs|
            |dbSpecialDisplayOpChar?| |parseHas,mkand| |pfSuchthat?|
            |pfWhileCond| /UNTRACE-REDUCE |postWith|
            |untraceDomainConstructor| |pfWhile?|
            |brightPrintRightJustify| |pfForinWhole| |isDomain|
            |postError| XDR-STREAM-HANDLE |postQUOTE| |parseCases|
            |postCollect| |isExistingFile| |brightPrint1|
            |cgammaAdjust| |tabber| |getToken| |intpart| SMALL-ENOUGH
            |postin| |sayLongOperation| |postIn| |say2PerLineThatFit|
            |cgammaBernsum| MONITOR-BLANKS |postRepeat| COPY
            |containsVariables| DOWNCASE |postTupleCollect|
            |blankIndicator| |postAdd| XDR-STREAM-NAME |postReduce|
            |cgammat| |postComma| |center80| |bubbleType|
            |setOutputCharacters| |postSemiColon| |postWhere|
            |destructT| |getLookupFun| |postColonColon| |brightPrint|
            |gammaRatkernel| FOAM::PROCESS-IMPORT-ENTRY |postColon|
            |splitSayBrightly| |nodeSize| |postAtSign| |noBlankBeforeP|
            |PsiIntpart| |vectorSize| |postPretend| |postIf|
            |splitSayBrightlyArgument| |numberOfNodes| |postJoin|
            |splitListSayBrightly| |fracpart| |noBlankAfterP|
            |postSignature| |postCategory| |postDef| |dcOps|
            |resolveTTRed3| |simplifyAttributeAlist| |postMDef|
            |string2Words| |phiRatapprox| |dcCats1| MONITOR-FILE
            |postMapping| |sayBrightlyLength1| |nontrivialCosig|
            |formatSlotDomain| |addBlanks| |lnrgammaRatapprox| |dcData|
            |postExit| |dcData1| |operationLink| |gammaRatapprox|
            |charyTopWidth| MONITOR-DECR |postTuple| |putWidth|
            MONITOR-DATA-P /TRACE-0 |makeCompactSigCode|
            |postTransformCheck| |dcCats| |hasDefaultPackage|
            |concatTrouble,fixUp| |isTaggedUnion| |sumWidthA|
            |killColons| /OPTIONS MONITOR-CHECKPOINT |postcheck|
            |pfLambdaTran| |negintp| |npInfGeneric| |pfSequence2Sex|
            |clngammacase3| |pf2Sex1| |unInstantiate|
            |SubstWhileDesizingList| |pfTyped?| |pfSequence2Sex0|
            |pfDefinition2Sex| |getDomainHash| |pfDefinitionRhs|
            |pf0DefinitionLhsItems| |getConstructorModemap|
            |pf0TupleParts| |gammaStirling| |handleKind| |float2Sex|
            |sumoverlist| |pfApplication2Sex| |outputConstructTran|
            |npListing| |countCache| |exptNeedsPren|
            |processKeyedError| |matSuperList| |outputTranSEQ|
            |npZeroOrMore| |whichCat| |tuple2List| |setInputLibrary|
            |isDefaultPackageForm?| |getMsgInfoFromKey|
            |postIteratorList| |getDomainByteVector| |pfLambda?|
            |setExposeAddGroup| |matSubList| |getMsgArgL| |new2OldTran|
            |pfLeaf?| |removeAttributes| |macSubstituteOuter|
            |outputTranRepeat| |getMsgKey| |outputTranReduce|
            |getMsgKey?| |loadLib| |setAsharpArgs| |outputTranCollect|
            |pfParts| |outputTranIf| |listOutputter| |CompStrToString|
            |pfIdSymbol| |outputMapTran| |msgOutputter|
            |parseGreaterThan| |outputTranMatrix| |pfSourcePosition|
            |flattenOps| |processChPosesForOneLine| |pfMLambdaBody|
            |npTuple| |parseColon| |pf0MLambdaArgs|
            |outputTranIteration| |parseCoerce| SHUT |setHistory|
            |isQuotient| |getMsgPos| |parseAtSign| |DNameFixEnum|
            |macExpand| |asyShorten| |npQualified| |DNameToSExpr|
            |texFormat1| |makeMsgFromLine| |parseCategory|
            |DNameToSExpr1| |macApplication| |matLSum|
            |createAbbreviation| |pf0ApplicationArgs| |asyComma?|
            |poLinePosn| |parseConstruct| |pfMLambda?| REROOT
            |isRationalNumber| |asMakeAlistForFunction| |getLineText|
            |parseDEF| |pfApplicationOp| DIG2FIX |initCache|
            |getLinePos| |isCategoryPackageName| |mac0Get| |parseExit|
            |closeOldAxiomFunctor| |npDDInfKey| |getMsgPosTagOb|
            |parseHas| |mac0GetName| |matLSum2| |asyTypeJoinPartPred|
            |tokPart| |getMsgFTTag?| |pfLeafPosition| |maprinRows|
            |displayCacheFrequency| |asyTypeUnit| |alreadyOpened?|
            |parseIn| |pfAbSynOp| |outputOp| |asyUnTuple| |tokPosn|
            |msgImPr?| |outputTranIterate| |asyTypeItem| |erMsgSort|
            |parseInBy| FOAM:|fiSetDebugVar| |pfTypedId| LOG2
            |remWidth| |abbreviate| |npInfKey| |erMsgSep| |parseIs|
            |pf0LambdaArgs| |toFile?| |parseIsnt| MAKE-CVEC |macId|
            |dollarPercentTran| GET-BOOT-TOKEN |getMsgToWhere|
            |parseJoin| |simpCattran| |makeLeaderMsg| |macMacro| SIZE
            |asyCattran| |parseLeave| |macLambda| EOFP
            |clearCategoryCache| |parseLET| |macWhere| CGAMMA
            |displayHashtable| |sumOrParen| |parseMDEF|
            |pfApplication?| C-TO-S RSHUT |clearConstructorCache|
            |productOrParen| DEF-RENAME S-TO-C |pfMacro?|
            VMLISP::LIBSTREAM-INDEXTABLE |new2OldLisp| |pfWhere?|
            |cgamma| |sayModemap| |string2SpadTree| |pfNothing?|
            |formatOpConstant| |asCategoryParts,exportsOf|
            |parsePretend| |typeIsASmallInteger| |pfMacroRhs|
            |outputDomainConstructor| |pfMacroLhs| RLNGAMMA
            |typeTimePrin| |asyPredTran1| |parseReturn| |lnrgamma|
            |asyTypeMakePred| |npBracked| |parseSegment| |height|
            |pfTuple?| C-TO-R |formatModemap,fn| |pfApplicationArg|
            |parseWhere| |opTran| |constructor2ConstructorForm|
            |formIterator2String| |asyExtractAbbreviation| DIGITP
            |pfOp2Sex| |asyTypeJoinStack| SEC CLNGAMMA
            |remHashEntriesWith0Count| |blankList| |astran| |lncgamma|
            |asMakeAlist| |absym| |numberOfEmptySlots| |formWrapId|
            |asyParents| |retract2Specialization| VEC2LIST |keyp|
            |reportHashCacheStats| |isInternalFunctionName|
            |asyDocumentation| |coerceUnion2Branch| COMP370
            |mkHashCountAlist| |findSubstitutionOrder?|
            |asyConstructorModemap| MAKE-VEC RGAMMA |syminusp|
            |asytran| |resolveTypeListAny| GCMSG |rgamma|
            |binop2String| GETREFV |matrix2String|
            |asyTypeJoinPartWith| SOURCEPATH |formCollect2String|
            |postDoubleSharp| |npMoveTo| |texFormat| |constructorName|
            |postDefArgs| |mathmlFormat| |CDRwithIncrement|
            |record2String| |as| |varsInPoly| |pfSourceStok|
            |sigmaWidth| |last| |dispfortexp| |isBinaryInfix|
            |formulaFormat| |asyTypeUnitList| |sigma2Sub|
            |formatSignature| |asyConstructorArg| |sigma2Sup|
            |freeOfSharpVars| |asyPredTran| |sigma2Width| MBPIP
            |mathPrintTran| |formatSignature0| |largeMatrixAlist|
            |specialChar| QSORT |deMatrix| |optIF2COND| |powerOrParen|
            |intSub| PLACEP |parseAndInterpToString| |optQSMINUS|
            |incFileName| |formatIf| |intSup| FOAM:|formatDFloat|
            |parseAndInterpret| |maPrin| |optXLAMCond|
            |inclmsgConActive| |canRemoveIsDomain?| |intWidth|
            FOAM:|formatSFloat| |oldParseAndInterpret|
            FOAM::FOAM-FUNCTION-INFO |underDomainOf;|
            |inclmsgNoSuchFile| FOAM:|formatBInt| VMLISP::COMPILE1
            |parseAndEvalToStringEqNum| |getUnderModeOf|
            FOAM:|formatSInt| |deconstructT| |indefIntegralSub| |Else?|
            |expr2String| |indefIntegralSup| |packageForm?|
            |inclmsgPrematureEOF| |atom2String| |indefIntegralWidth|
            |parseAndEvalToString| |isValidType|
            |getImmediateSuperDomain| |piSub|
            |parseAndEvalToStringForHypertex| |isValidType;| |piSup|
            |parseAndEvalToHypertex| |underDomainOf| |fileNameStrings|
            |form2Fence| |piWidth| |incStringStream|
            |matrix2String,outtranRow|
            |getPartialConstructorModemapSig|
            |formatOperationAlistEntry| |pi2Sub| GENSYMP
            |wrapped2Quote| |formatMapping| |pi2Sup| |inclmsgSay|
            |formatAttribute| |pi2Width| |augmentLowerCaseConTable|
            |objCodeVal| |PushMatrix| |aggWidth| |objCodeMode|
            |outputTranMatrix,outtranRow| |isNameOfType| |objMode| COMP
            |maprinChk| |postSequence| |SubstWhileDesizing|
            |postTranList| |domainForm?| |makeOldAxiomDispatchDomain|
            |compAndDefine| |stringer| |incFile|
            |getConstructorUnabbreviation| |translateTrueFalse2YesNo|
            COMP-2 |sayMath| |inclmsgConStill| |isDomainValuedVariable|
            COMP-1 |inclmsgCannotRead| |validateOutputDirectory|
            |inclmsgPrematureFin| DATABASE-PARENTS |setOutputLibrary|
            DATABASE-SOURCEFILE |sayMessage| |setFortTmpDir|
            |setAutoLoadProperty| |setFortDir| DATABASE-PREDICATES
            |pfBreakFrom| HKEYS |maximalSuperType| |edit| |pfBreak?|
            |getValue| |getConstructorAbbreviation| DATABASE-ATTRIBUTES
            |pfRule?| |retract| |help| |pfNovalueExpr|
            |wrapMapBodyWithCatch| |npPush| |bubbleConstructor|
            DATABASE-COSIG |npEqKey| |library| |SpadInterpretFile|
            DATABASE-CONSTRUCTORMODEMAP |mkAliasList| |load|
            |ncSetCurrentLine| |ltrace| |pfAppend| |setCurrentLine|
            |nopiles| |npListAndRecover| |setExposeAddConstr|
            |setExposeDropConstr| |makeArgumentIntoNumber|
            |pfSequenceToList| |domainOne| DATABASE-CONSTRUCTORKIND
            |setExposeDropGroup| |pfListOf| |phInterpret|
            DATABASE-CONSTRUCTORFORM |initializeSetVariables|
            |pfSequenceArgs| |intInterpretPform| |lispType| UPCASE
            |bottomUpElt| DATABASE-ABBREVIATION
            |translateYesNo2TrueFalse| |isPatternArgument| |read|
            |pfSequence?| |checkForBoolean| |getUnionOrRecordTags|
            |isConstantArgument| |npParenthesized|
            |getModeOrFirstModeSetIfThere| |ncConversationPhase,wrapup|
            |lfspaces| SET-FILE-GETTER |pf2Sex| |spadTypeTTT| |spool|
            |zeroOneTran| |lferror| |phMacro| |npRestore|
            |binomialWidth| |retract1| SQUEEZE |npEqPeek| |domainZero|
            |scanWord| |listOfVariables| |pfNovalue?| |isFreeVar|
            |trace| |pfNotArg| |npWith| |zagSub| |nextline|
            |isLocalVar| |undo| |zagSuper| |compFailure|
            |devaluateList| |multiToUnivariate| |zagWidth|
            DATABASE-DEFAULTDOMAIN |args2Tuple| |variableNumber|
            |workfiles| FBPIP |pfIfElse| DATABASE-NILADIC
            |npCompMissing| |functionAndJacobian| |isType| |pfIfThen|
            |lfinteger| DATABASE-CONSTRUCTORCATEGORY |pfIfCond|
            DATABASE-OBJECT |removeSuperfluousMapping| |spad2lisp|
            |bottomUp| |pfIf?| DATABASE-MODEMAPS |postType| |vec2Lists|
            |mkAtreeNode| DATABASE-OPERATIONALIST |complexRows|
            |getUnname| |Zeros| DATABASE-DOCUMENTATION |XDRFun|
            |getBasicObject| |pfCheckMacroOut| |prefix2Infix|
            |getUserIdentifiersIn| |pfNot?| |vec2Lists1|
            VMLISP::GET-INDEX-TABLE-FROM-STREAM |digit?| |pfOrRight|
            |npBraced| |bottomUpPercent| |pfOrLeft| |fetchOutput|
            DATABASE-P |isInternalMapName| |pfOr?| |incClassify|
            UNEMBED |punctuation?| |lfid| |incCommand?| |expandMacros|
            |getIteratorIds| |npAdd| |timesWidth| |isNiladic|
            |getUserIdentifiersInIterators| |int2Bool| |getCon|
            UNSQUEEZE |combineMapParts| |exptWidth| |walkWhereList|
            |frameName| |addSpaces| |lfstring| |pair2list| |exptSuper|
            |If?| |lfkey| |pf0ForinLhs| |stepWidth| |keyword?|
            NEXT-TAB-LOC |pfForin?| |stepSub| |scanW| INDENT-POS
            |pfCollect?| |incRenumber| |stepSuper| |scanKeyTr|
            |pf0LoopIterators| |setDefOp| EXPAND-TABS |maprin| |incPos|
            |makeUnion| |inWidth| |transUnCons| |maprin0| |incBiteOff|
            |npLetQualified| |vectorOfFunctions| |inSub| |parseBigelt|
            |lineoftoks| NONBLANKLOC |makeFort,untangle| |inSuper|
            |Top?| |makeFort,untangle2| |asyCattranOp| |lfnegcomment|
            |Skipping?| |lfcomment| BLANKP |isInterpMacro| |KeepPart?|
            |npEncAp| |scanPossFloat| |asTupleSize| |npSemiListing|
            |mkQuote| |scanCloser?| |makeLispList| |transIs1|
            |pfLambda2Sex| |keyword| |isListConstructor|
            |breakIntoLines| |SkipPart?| |concatSub| |updateTimedName|
            |parseTran,g| |scanTransform| |pfLoop?| |SkipEnd?|
            |stripNil| |concatSuper| |parseAtom| |segment| |pfExitExpr|
            |concatbWidth| |timedEVALFUN| |transCategoryItem|
            |fortExpSize| |rdigit?| |pfExitCond| |Elseif?|
            |makeOutputAsFortran| |pushTimedName| |pfExit?|
            |separatePiles| |mkQuote,addQuote| |concatWidth|
            |exp2Fort1| |str2Outform| |parse2Outform| |fortranCleanUp|
            |addCommas| |quoteWidth| |mkMapPred| |isDefaultPackageName|
            |incNConsoles| |stringWidth| |printNamedStats|
            |fortran2Lines1| |mapPredTran| |getDependentsOfConstructor|
            FOAM:|fiStrHash| |incFileInput|
            |simplifyMapPattern,unTrivialize|
            |getOplistForConstructorForm| |sigmaSub|
            |timedAlgebraEvaluation| STACK-TOP |displayLines|
            |getOperationAlistFromLisplib| |sigmaSup| |pfRetractToExpr|
            |pr| |fortran2Lines| |getEqualSublis| |pfSecond|
            LINE-ADVANCE-CHAR |predTran| |matSuper| LINE-AT-END-P
            |pfLoopIterators| |checkLines| |simplifyMapConstructorRefs|
            |displayOperationsFromLisplib| |matWidth| GET-A-LINE
            |pf0AddBase| |signatureTran| INTERPSYS-ECL-IMAGE-INIT
            |significantStat| MAKE-STRING-ADJUSTABLE |pf0WithBase|
            |charDigitVal| VMLISP::VARP |pfDefinitionSequenceArgs|
            |indentFortLevel| |listOfSharpVars| FOAM:|fiGetDebugger|
            |pf0FlattenSyntacticTuple| |checkType| |pfAttributeExpr|
            |enPile| |getDomainFromMm| |npPileBracketed| TOKEN-NONBLANK
            |pf0TLambdaArgs| |mkParameterList| |pilePlusComment|
            |pfPile| TRY-GET-TOKEN |pfWIfElse| |npMissing| TOKEN-TYPE
            |pfRetractTo?| |pileColumn| |dropPrefix| |displayLines1|
            |pilePlusComments| |histInputFileName| |overlabelSuper|
            |insertpile| |statement2Fortran| |frameEnvironment|
            |dispStatement| |parseFromString| |timedOptimization|
            TOKEN-PRINT MESSAGEPRINT |pushSatOutput| |overbarSuper|
            |lispize| STACK-P MESSAGEPRINT-1 |changeExprLength|
            LINE-PRINT REDUCTION-RULE MESSAGEPRINT-2 |exp2FortOptimize|
            |fortPre| |lnExtraBlanks| LINE-PAST-END-P |npParse| TOKEN-P
            |pfSourceToken| STACK-POP |evaluateLines| |systemError|
            |pfFirst| |pfLiteralString| |string2BootTree| |pfDocument|
            |pfGetLineObject| |pfLeafToken| |pfAndRight|
            VMLISP::GET-DIRECTORY-LIST |rootSuper| |error|
            |pfLiteralClass| |pfAndLeft| |rootWidth|
            |htTrimAtBackSlash| |pfAnd?| |verifyRecordFile| |pfWrong?|
            SPAD_ERROR_LOC LINE-NEXT-CHAR FOAM::TYPE2INIT |lnLocalNum|
            LIST2CONS |devaluate| |lnFileName?| LIST2CONS-1
            |ncParseAndInterpretString| |pfUnSequence| STACK-CLEAR
            |lnImmediate?| DEF-WHERE |packageTran| TOKEN-SYMBOL
            |poNoPosition?| |analyzeMap,f| MKPROGN |letWidth|
            |poIsPos?| |untraceMapSubNames| |pfNopos?| |npDotted|
            |poFileName?| |removeBodyFromEnv| |slashSub|
            |recordAndPrintTest,fn| |pfPosn| |sayRemoveFunctionOrValue|
            HACKFORIS1 |slashSuper| |pfSequence| |poImmediate?|
            |slashWidth| |poGetLineObject| |lnString| |hashable| DEF-IS
            |pmDontQuote?| |pfFix| |lnPlaceOfOrigin| |lnGlobalNum|
            |hasOptArgs?| |pfPosOrNopos| |pfPlaceOfOrigin| DEF-EQUAL
            |pfCollect2Sex| |segmentKeyedMsg| |subSub|
            |poPlaceOfOrigin| |DEF-::| |printAsTeX| |mkAtree1|
            |pfSourcePositions| |suScWidth| |pfSourcePositionlist|
            DEF-REPEAT |mkAtreeValueOf| |pfPosImmediate?| |npBracketed|
            |collectDefTypesAndPreds| HACKFORIS |superSubSub|
            |npAngleBared| |unwrap| |patternCheck| |superSubSuper|
            |getUnname1| |isWrapped| DEF-COLLECT |superSubWidth|
            |npFromdom1| DEF-SEQ DEF-LESSP |pfEnSequence| SMINT-ABLE
            |vConcatSub| |pfTuple| |srcPosLine| DEF-IS-REMDUP1
            DROPTRAILINGBLANKS |recordAndPrintTest| |vConcatWidth|
            |pfExport| |srcPosColumn| |pfNovalue| |pfFreeItems|
            |binomialSub| |npItem1| |pfDWhereContext| |mkAtreeValueOf1|
            |binomialSuper| |pfWDeclareDoc| MACROEXPANDALL
            |pf0LocalItems| |pfTLambda?| |pfLocal?| |retractAtree|
            REDUCTION-VALUE |pfComDefinitionDef| |pfLocal| PREPARSE
            |pfLoop| |srcPosDisplay| |displayMacros| PRINT-PACKAGE
            |pfDo| |Record0| |srcPosFile| |displayOperations|
            |pfAddAddin| MAKE-ADJUSTABLE-STRING |pfWithWithon|
            |pfCollectBody| |computedMode| |pfCollectIterators|
            |polyVarlist| BUMPERRORCOUNT |pfSemiColon| |loopIters2Sex|
            |pfDocument?| |dewritify| |pfSuchThat2Sex|
            VMLISP::SIMPLE-ARGLIST |pf0WrongRubble| |isLeaf|
            |setNopiles| BVEC-COPY |pfWrongRubble| |getFlag|
            |makeCompactDirect1,fn| MONITOR-DATA-COUNT
            |bottomUpCompile| |pfQualTypeType|
            |sayAsManyPerLineAsPossible| |orderBySubsumption|
            MONITOR-DATA-NAME |objVal| |pfWithWithin| |getIProplist|
            MONITOR-DATA-SOURCEFILE |pfTupleList| |asTupleAsVector|
            MONITOR-NRLIB GET-SPECIAL-TOKEN |pfInline| |sumWidth|
            GET-SPADSTRING-TOKEN |transformREPEAT| |dcAtts|
            MONITOR-EXPOSEDP GET-SPADNUM-TOKEN |pfWDeclareSignature|
            |transformCollect| |getCatAncestors| MONITOR-LIBNAME
            GET-ARGUMENT-DESIGNATOR-TOKEN |pfWhereContext| |dcSlots|
            |whatCommands| |minusWidth| |pfIterateFrom| |unVectorize|
            MONITOR-APROPOS DEF-SETELT |pfTupleListOf| |getSrcPos|
            BVEC-NOT |whatSpad2Cmd,fixpat| DEF-ELT |fracsub| |pfWhile|
            REMDUP |npProcessSynonym| |DEF-:| MONITOR-PARSE
            BOOT-TOKEN-LOOKAHEAD-TYPE |pf0ExportItems| |getOpSegment|
            DEF-RENAME1 MONITOR-SPADFILE |fracsuper| |pfExportItems|
            |dcAll| DEF-MESSAGE MONITOR-INCR |fracwidth| |pfHidePart|
            ASSOCLEFT |asTupleNewCode0| |postForm| |dcPreds|
            |removeUndoLines| MONITOR-DELETE |pfComDefinitionDoc|
            ASSOCRIGHT |postOp| |dcOpTable| |undoCount| DEF-STRING
            |macroExpanded| |pfWrongWhy| |predicateBitIndexRemop|
            |isIntegerString| DEF-COND |predicateBitRef|
            |findFrameInRing| |nonBlank| |predicateBitIndex|
            |constructor| |bootTransform| LINE-SUBSEQ-FROM
            |pfQualType?| |encodeCatform|
            |changeToNamedInterpreterFrame| DEF-IS-REMDUP
            |pfStringConstString| |dewritify,is?| DEF-IS-EQLIST
            MONITOR-DATA-MONITORP |pfWDeclare?|
            |removeAttributePredicates,fn| DEF-IN2ON |binomSub|
            |pfReturnFrom| |removeAttributePredicates,fnl|
            |unwritable?| DEF-INSERT_LET1 MONITOR-DIRNAME |binomSuper|
            |incString| VMLISP::LIBSTREAM-P |pf0ImportItems|
            |removeBindingI| |binomWidth| |ncloopParse| |pfImportItems|
            DEF-WHERECLAUSE |pfSymbolVariable?| STREAM-EOF
            |mkAtreeExpandMacros| DEF-STRINGTOQUOTE |pfHide?|
            DEF-ADDLET |altSuperSubSub| |splitIntoBlocksOf200|
            DEF-INSERT_LET |altSuperSubSuper| |break| |pfWithBase|
            |containsVars| |ppTemplate| |compFluidize1|
            |pfPrintSrcLines| |altSuperSubWidth| |constructorCategory|
            |pfListOf?| |bitsOf| |pfAddBase| |fixUpTypeArgs|
            |displayProperties,sayFunctionDeps| |pfForinLhs| |sayTeX|
            |displayMacro| |boxSub| |pfCheckArg| |evalMmStack|
            |S_process| |displayParserMacro| |getExportCategory|
            |boxSuper| |pfCheckId| |boxWidth| |ncloopPrintLines|
            |pf0WithWithin| |mkLineList| |pfRetractToType| |dqConcat|
            |findEqualFun| |pfExportDef| |makeCompactDirect| |template|
            |dqUnitCopy| |qTWidth| |intloopEchoParse|
            |makeDomainTemplate| |dqUnit| VMLISP::GETINDEXTABLE IVECP
            |systemErrorHere| |sayBrightlyLength| |infovec| |dqToList|
            |patternVarsOf| LIST2VEC |pathnameDirectory|
            |ncloopEscaped| |pfRule2Sex| |mkPredList| |pfNot|
            |walkForm| |pfExpression?| |pfLiteral2Sex| |flattenSemi|
            |extsub| |pfWhereExpr| |getInfovec| BRIGHTPRINT
            |postTransform| |undoChanges| |serverReadLine| |extsuper|
            |isSystemDirectory| |pfInlineItems| |mkRationalFunction|
            |pp2Cols| |stuffDomainSlots| |extwidth| |ncloopIncFileName|
            |pfId| |saySpadMsg| |whatConstructors| |undoInCore|
            |loadLibIfNotLoaded| |sayALGEBRA| |orderMmCatStack|
            |fnameType| IS-CONSOLE |updateCategoryFrameForConstructor|
            |pfAttribute?| |sayMSG| |updateCategoryFrameForCategory|
            |pfTupleParts| DEF-PROCESS |getFirstWord| |NRTcatCompare|
            |StringToDir| |convertOpAlist2compilerInfo| |pfTLambdaBody|
            |sayMSG2File| |editSpad2Cmd| |fnameDirectory| |pfWIfThen|
            |orderByContainment| |DirToString| |pf0WhereContext|
            |stripOutNonDollarPreds| |fnameExists?| |pfIterate?|
            |loadIfNecessaryAndExists| |getSymbolType|
            |isHasDollarPred| |myWritable?| |pfReturnExpr|
            |pfTransformArg| |evalMmDom| INITIALIZE-PREPARSE
            |depthAssocList| |fnameWritable?| |pfReturn?|
            |pfTaggedToTyped1| |sayFORTRAN| |clearCmdExcept|
            |fnameReadable?| |StringToCompStr| |lisplibDoRename|
            |pfSexpr| |fnameName| |finalizeLisplib| |pfSexpr,strip|
            |sayFORMULA| |pfSemiColonBody| |containsVars1|
            MAKE-ABSOLUTE-FILENAME |IdentityError| |pfInline?|
            |mathprint| |pathnameTypeId| |opIsHasCat| |pfFlattenApp|
            |hashCode?| |pfCollect1?| |isNewWorldDomain| |pfExitNoCond|
            |selectMostGeneralMm| |instantiate| |pfImport|
            |resolveTypeList| |pfTLambdaRets| |clearTempCategoryTable|
            |npTrapForm| |writify,writifyInner| |pfTaggedToTyped|
            |getConstrCat| |replaceGoGetSlot| |quoteCatOp|
            |pfCollectVariable1| |getModeSetUseSubdomain|
            |closeInterpreterFrame| |readLibPathFast| |pfWIfCond|
            |getModeSet| |pfDefinitionLhsItems| |isMap| |pfExport?|
            |bright| |pfMLambdaArgs| |pfSemiColon?| |pfFromdomDomain|
            |makeByteWordVec| |readHiFi| |pfFromdomWhat|
            |getSlot1FromCategoryForm| |pfDeclPart?| THETA_ERROR
            |pfFromdom?| |loadFunctor| |pfDWhere?| |pfPretendType|
            |clearParserMacro| |transformOperationAlist| |pfImport?|
            |userError| |srcPosSource| |showCategoryTable|
            |knownEqualPred| |getFunctorOpsAndAtts| |pfTyping?|
            |emptyInterpreterFrame| |getCategoryOpsAndAtts|
            |pfSuchthat| |pfSourceText| |undoFromFile|
            |pfAssignLhsItems| |pf0TypingItems| |pfTypingItems|
            |optCONDtail| |dewritify,dewritifyInner| |pfQualTypeQual|
            |failCheck| |optPredicateIfTrue| |killNestedInstantiations|
            |evaluateFormAsType| |compileTimeBindingOf| |pfDWhereExpr|
            VMLISP::LIBRARY-FILE |optimize,opt| |setIOindex| |%fname|
            |writify| |pfComDefinition?| |%id| |processSynonymLine|
            |pfLambdaArgs| |makeOrdinal|
            |processSynonymLine,removeKeyFromLine|
            |NRTgenInitialAttributeAlist| |pfTLambdaArgs|
            |getAndEvalConstructorArgument| |workfilesSpad2Cmd|
            |pfCheckInfop| |%origin| |pfWIf?| |getUnnameIfCan|
            |printSynonyms| |pfDocumentText| |ppos| |pfAdd?|
            |synonymsForUserLevel| |pfPretendExpr| LISTOFATOMS
            |pfLinePosn| |pfPretend?| |pfExpr?| LASTATOM |pfCharPosn|
            |pfWith?| |pfCoercetoType| |pfImmediate?| |deleteFile|
            |pf0CollectIterators| |pfCoercetoExpr| |pfNoPosition?|
            |compileAsharpArchiveCmd| |isAVariableType|
            |compileSpadLispCmd| |matchMmCond| |compileAsharpLispCmd|
            NUMOFNODES |optEQ| |compileAsharpCmd| |constructSubst|
            |optCons| |toScreen?| |optLESSP| |getMsgPrefix|
            |optimizeFunctionDef| |line?| |noSharpCallsHere| |leader?|
            |optMkRecord| |optSuchthat| |getMsgText| |printMms|
            |optSETRECORDELT| |poCharPosn| |undoLocalModemapHack|
            |getStFromMsg| |tabbing| MANEXP SUBANQ |makeHistFileName|
            |getMsgLitSym| |getPosStL| |getMsgPrefix?| ACOT
            |evalMmStackInner| |stopTimingProcess| |getPreStL| COT
            |compileInteractive| |posPointers| CSC IS_GENVAR
            |startTimingProcess| |intnplisp| ACSC |nplisp| |msgNoRep?|
            ASEC VMLISP::SPAD-FIXED-ARG |emptyAtree| CSCH |getMsgTag?|
            COTH |commandsForUserLevel| |initToWhere| SECH
            |domainDepth| ACSCH |removeTracedMapSigs| |nopilesSpad2Cmd|
            |initImPr| ACOTH |newHelpSpad2Cmd| GETZEROVEC
            |putDatabaseStuff| ASECH |pathnameName|
            VMLISP::LIBSTREAM-MODE |getTarget| |To| |updateSourceFiles|
            |remFile| |pathnameType| |remLine| |spadClosure?|
            |decomposeTypeIntoTower| |loadSpad2Cmd|
            |reportOpsFromUnitDirectly| |showMsgPos?| |say2PerLine|
            |From| LOG10 |poNopos?| |isHomogeneousList| |Identity|
            |reportUndo| |msgLeader?| |orderList| |tokTran|
            |parseSystemCmd| |poPosImmediate?| |str2Tex| |dumbTokenize|
            |wrap| |mkAtree| |asTupleAsList| |pfLambdaBody|
            |categoryForm?| |containsPolynomial| |pfLambdaRets|
            |reassembleTowerIntoType| |splitIntoOptionBlocks|
            |bottomUpUseSubdomain| |pfTypedType|
            |getConstructorSignature| |pfCollectArgTran|
            |satisfiesUserLevel| |segmentedMsgPreprocess|
            VMLISP::PROBE-NAME |isPartialMode| |getBasicMode|
            |formatArgList| |isMapExpr| |npboot| |stripLisp|
            |displayTranModemap| |getMode| |formatModemap|
            |removeQuote| |cleanUpSegmentedMsg| |postBlock|
            |script2String| |comma2Tuple| |postCapsule| |recordFrame|
            |eqType| FOAM::FOAMPROGINFOSTRUCT-P |removeIsDomainD|
            |postBlockItemList| |isLegitimateRecordOrTaggedUnion|
            |ncParseFromString| |postBlockItem| |upLET|
            |listOfDuplicates| |formatSignatureAsTeX| |postSEGMENT|
            |isPolynomialMode| |postElt| |upIF| |equiType|
            |object2Identifier| |newDef2Def| |uperror| |npWConditional|
            |new2OldDefForm| |linearFormat| |pfTweakIf| |newIf2Cond|
            |typeToInputForm| |copyHack| |form2Fence1| |npConditional|
            |newConstruct| |typeToOutputForm| |copyHack,fn|
            |optSPADCALL| |form2FenceQuote| |postQuote| |optRECORDELT|
            |checkWarning| |isHomogeneousArgs| |optCatch| |unabbrev|
            |uphas| |linearFormatName| |concatList| |pfCoerceto?|
            |upreturn| |form2StringList| |pfTaggedExpr| |upisnt|
            |listOfPredOfTypePatternIds| |pfTaggedTag| |lastTokPosn|
            |upisAndIsnt| |isPatternVar| |mkMessage| |pfTagged?|
            |pileComment| |typeOfType| |optMINUS| |removeIsDomains|
            |clearCache| |mkEvalableMapping| |altSeteltable|
            |optSEQ,tryToRemoveSEQ| |getLisplibName|
            |handleLispBreakLoop| |expandDO| |mkEvalableUnion|
            |uptypeOf| |form2StringWithWhere| SEQOPT |mkEvalableRecord|
            |pathname| |form2FenceQuoteTail| MKQSADD1 |loadIfNecessary|
            |upDEF| |namestring| |pfAttribute| |pfLiteral?|
            |removeEXITFromCOND| INFIXTOK |evaluateType0| |upDollar|
            |isSharpVar| |length1?| |formJoin2| |addTraceItem|
            |flattenCOND| |evaluateType| |extractCONDClauses|
            |evaluateType1| |compileBoot| |numOfSpadArguments|
            |bootPROGN| |tuple2String| PREPARSE-ECHO
            |evaluateSignature| |stupidIsSpadFunction| |optSEQ|
            |bootAbsorbSEQsAndPROGNs| ATENDOFUNIT |evalType| |trace1|
            |upiterate| |optSEQ,getRidOfTemps| PREPARSEREADLINE
            |quoteNontypeArgs| |saveMapSig| |optSEQ,SEQToCOND| |pfId?|
            |bootSEQ| |formJoin2String| SKIP-TO-ENDIF |upwhere|
            |bootTran| |formTuple2String| SKIP-IFBLOCK |upREPEAT0|
            |minimalise| |pfRuleRhs| |tryToRemoveSEQ| |untrace|
            |interpOnlyREPEAT| |minimalise,min| |pfRhsRule2Sex|
            |nakedEXIT?| |coerceOrParen| |devaluateDeeply| |upREPEAT1|
            |minimalise,HashCheck| |pfRuleLhsItems| |pfTyping|
            |mergeCONDsWithEXITs| |appOrParen| PREPARSEREADLINE1
            |getTraceOptions| |upREPEAT| |pfLhsRule2Sex| |compFluidize|
            |pfFree| |transTraceItem| |mergeableCOND| PARSEPRINT
            |compExpand| |formatPredParts| PREPARSE1 |upis| |opt-|
            |bootCOND| PUSHLOCVAR |pfReturnNoName| WHOCALLED
            |getTraceOption| SPADSYSNAMEP |upQUOTE| |compTran1|
            |lambdaHelper1| |pfIterate| |getMapSubNames|
            |bootPushEXITintoCONDclause| |pf0SequenceArgs|
            |getPreviousMapSubNames| |upSEQ| |bootIF| |isTraceGensym|
            |compNewnam| |pfLoop1| NREVERSE0 |getCType| |compTran|
            |spadReply,printName| |quote2Wrapped| |pfCheckItOut|
            |uppretend| |bootAbsorbSEQsAndPROGNs,flatten|
            |bootLabelsForGO| |orderBySlotNumber| |pfAddAddon|
            |bootOR,flatten| |hashString| |pf0FreeItems| |bootOR|
            |isListOfIdentifiers| |pfFree?| |npAnyNo| |lambdaHelper2|
            |pfLocalItems| |isListOfIdentifiersOrStrings|
            |pfRestrictType| |removeEXITFromCOND?| |pfBreak|
            |pfRestrictExpr| |pileCforest| |npElse|
            |coerceSpadFunValue2E|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) *) COMPILE-LIB-FILE |ioHook| ASHARP
            FOAM:COMPILE-AS-FILE |sayBrightlyNT|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (VALUES T T)) FOAM:AXIOMXL-GLOBAL-NAME
            |spadTraceAlias|)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) STRING) MAKE-FULL-CVEC)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) T) |centerAndHighlight| MAKE-INPUT-FILENAME
            |getConstructorExports| |simpHasPred| |centerNoHighlight|
            MAKE-HASHTABLE MAKE-FILENAME MACERR CATCHALL TAB
            PRETTYPRINT |pfSymb| |F,PRINT-ONE| |desiredMsg|
            |fillerSpaces| VMLISP::MAKE-FULL-NAMESTRING VMREAD
            |LAM,EVALANDFILEACTQ| |interpret| PRINT-FULL BLANKS
            |htFile2RecordFile| |inputFile2RecordFile|
            |htFile2InputFile| |printRecordFile| MATCH-CURRENT-TOKEN
            RDEFIOSTREAM GET-BOOT-IDENTIFIER-TOKEN MONITOR-ADD
            |sayBrightly| |sayBrightlyI| |pfExpression| |pfSymbol|
            MATCH-NEXT-TOKEN |defaultTargetFE| PRETTYPRIN0
            BOOTTRAN::BOOTTOCL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) *) |spadcall2| VMLISP::COPY-FILE
            VMLISP::COPY-LIB-DIRECTORY MONITOR-PRINT MONITOR-PRINARGS-1
            |spadTrace| |printMap1| MONITOR-PRINVALUE PRINMATHOR0
            |ppPair| SPAD-SAVE |nrtEval| DEFSTREAM |evalSlotDomain|
            |isDomainForm| /TRACE-2 |replaceNamedHTPage|
            |popUpNamedHTPage| |set1| |displaySetOptionInformation|
            |makeLongTimeString| |makeLongSpaceString| QUOTIENT
            |formArguments2String,fn| |output| |print| |inclHandleBug|
            COMP_QUIETLY_USING_DRIVER |makeStream| |intSayKeyedMsg|
            |spleI1| |readData,xdrRead1| |sayKeyedMsg| |xdrWrite|
            |ncEltQ| |fortError| |htCommandToInputLine,fn|
            COERCE-FAILURE-MSG |printTypeAndTime|
            |printTypeAndTimeNormal| |msgText| |mkAtreeWithSrcPos|
            |patternCheck,subWild| COMPILE-DEFUN |ScanOrPairVec|
            |ncConversationPhase| |writeInputLines| |sayErrorly|
            |intloopReadConsole| SAYBRIGHTLYNT1 |LAM,FILEACTQ|
            |ncloopCommand| |saturnSayErrorly| |ncloopInclude1| $FCOPY
            SUFFIX |reportOpsFromLisplib0| |rwriteLispForm|
            |evalAndRwriteLispForm| |isDomainConstructorForm|
            |displayProperties| |reportOperations|
            |reportOpsFromLisplib1| |reportOpsFromLisplib|
            |handleParsedSystemCommands| |ncloopInclude|
            |handleTokensizeSystemCommands| |tokenSystemCommand|
            |handleNoParseCommands| |npsystem| |getMinimalVarMode|
            |xdrRead| ADDCLOSE /TRACELET-PRINT)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (*)) MACRO-MISSINGARGS MACRO-INVALIDARGS)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) T) BPIUNTRACE |subTypes| |defLET2|
            |EqualBarGensym,fn| |hasPair| |defISReverse|
            |optCatch,changeThrowToExit| |deleteAssocWOC,fn|
            |addCARorCDR| |defLET1| |position| FLAG |stringMatches?|
            |Delay| |basicMatch?| |getAliasIfTracedMapParameter|
            |incAppend| |varIsOnlyVarInPoly| SET-LIB-FILE-GETTER |next|
            |removeListElt| |traceOptionError| |unabbrev1|
            |intCodeGenCOERCE| |reportSpadTrace| INTERSECTIONQ
            |canCoerceByMap| |declare| |EqualBarGensym|
            FOAM::|magicEq1| |upfreeWithType| |isDomainSubst,findSub|
            |uplocalWithType| |allLASSOCs| |assocCircular| |deleteAll|
            |positionInVec| REPEAT-TRAN |genMpFromDmpTerm|
            |isDomainSubst,fn| -REPEAT |pairList| |compileIs|
            |asyMapping| |search| |asySig| |searchCurrentEnv| MKPF
            MONITOR-EVALTRAN |deleteAssocWOC| FLAGP MONITOR-EVALTRAN1
            REMALIST MKPF1 MKPFFLATTEN MONITOR-GETVALUE |writeLib|
            |putPvarModes| |concat1| S- FOAM:|printDFloat| GETDATABASE
            GETALIST FOAM:|printSFloat| |deleteAssoc| S*
            |fortFormatIfGoto| FOAM:|printBInt| FOAM:|printSInt|
            FOAM:|printString| |modemapPattern| FOAM:|printChar|
            |seteltable| |evalLETput| |evalLETchangeValue|
            |asyMkSignature| |asyDocumentation,fn|
            |sortAndReorderDmpExponents| |NRTdescendCodeTran|
            /GETOPTION |canCoerceByFunction|
            |spliceTypeListForEmptyMode| |resolveTMTaggedUnion|
            |fortranifyFunctionName| |assignment2Fortran1|
            |resolveTMEq1| |exp2FortOptimizeCS1,pushCsStacks|
            |resolveTMSpecial| |integerAssignment2Fortran1|
            |getStatement| /EMBED-1 |resolveTTAny| |position1|
            |subCopy0| |subCopyOrNil| /EMBED-Q |deepSubCopy0|
            |deepSubCopyOrNil| |termRW1|
            |fortranifyIntrinsicFunctionName| |asySigTarget| |termRW|
            |dispfortarrayexp| |removeVectorElt| |fortFormatTypes1|
            FOAM-USER::H-ERROR |BesselJAsymptOrder|
            |asyCattranConstructors| |objNewWrap| |fortFormatTypes|
            |coerceByFunction| |BesselJAsympt| SETANDFILE
            |BesselIAsymptOrder| |cbeta| |displayDatabase,fn|
            |MappingPrint| |checkForFreeVariables| |RecordPrint|
            |orDel| FOAM:|PtrMagicEQ| ?ORDER /UNTRACE-2 |ordUnion|
            |coafAndDnf| |replaceSymbols| |traceDomainConstructor|
            |coafAndCoaf| /GETTRACEOPTIONS |modemapsHavingTarget|
            |PARSE-leftBindingPowerOf| |deleteLassoc| |tracelet|
            |declareMap| |PARSE-Operation| |ordSetDiff| |coafOrDnf|
            |ordIntersection| |dnfContains| |formalSubstitute| |andDnf|
            |rassocSub| |mkIterVarSub| |rePackageTran| |andReduce|
            |orDnf| |mkCategoryOr| |simpBoolGiven| |tempExtendsCat|
            |dnfContains,fn| |spadPrint| STACK-PUSH |testExtend|
            |encodeCategoryAlist| |newHasTest,fn| |hasCat| CARCDREXPAND
            |catPairUnion,addConflict| |quickOr| |countCircularAlist|
            |newHasTest| |updateCategoryTableForDomain|
            |clearAllSlams,fn| |recurrenceError| |addDomainToTable|
            CHAR-NE |parseTypeEvaluateArgs| |predCircular|
            |PARSE-rightBindingPowerOf| CHYPER0F1 |chebf01|
            |parseTranCheckForRecord| RBESSELJ |wordFrom| /UNTRACE-1
            |canFit2ndEntry| |parseCases,casefn| |spadUntrace|
            |acceptableTypesToResolve| |acceptableTypesToResolve1|
            |substDomainArgs| |resolveTTUnion| |NRTreplaceLocalTypes|
            |chebstareval| |resolveTCat1| |assoc| |say2PerLineWidth|
            |getConditionsForCategoryOnType| EFFACE |cgammaG|
            |resolveTTEq| |isSubDomain| |resolveTTCC|
            |absolutelyCanCoerceByCheating| |resolveTTRed| EMBED
            |center| |resolveTTSpecial| |compareTT| |constructTowerT|
            |sayPatternMsg| |besselIback| |resolveTMOrCroak| |horner|
            |oldAxiomDomainHashCode| |sayKeyedMsgLocal| |resolveTM2|
            |FloatError| |resolveTMUnion| |oldAxiomCategoryDevaluate|
            |resolveTMRed| |resolveTMEq| |hashTypeForm|
            |resolveTMRecord| |PsiAsymptotic| |throwKeyedMsg1|
            |oldAxiomPreCategoryParents| |saturnThrowKeyedMsg|
            |term1RWall| |makePrefixForm| |term1RW| LEXGREATERP
            |chebeval| |equalOne| |rPsiW| |makeGoGetSlot|
            |lazyOldAxiomDomainDevaluate| RPLPAIR
            |oldAxiomCategoryDefaultPackage| |matSuperList1|
            |typeToForm| |throwPatternMsg| |brutef01| |npLeftAssoc|
            |cpsireflect| |BesselKAsymptOrder| |pfApplication|
            |oldAxiomPreCategoryDevaluate| |postFlattenLeft| HPUT*
            /TRACE-1 |PsiXotic| |sayLooking1| |hashType| |f01|
            |replaceArgDefs| |pfTyped| |hashCombine| |replaceArgDefs1|
            |PsiEps| |computeTargetMode| |setMsgText|
            |oldAxiomCategoryHashCode| |BesselJRecur| |coerceIntTest|
            |oldAxiomCategoryParentCount| |replaceArgDef1|
            |oldAxiomDomainDevaluate| |symEqual|
            |coerceOrConvertOrRetract| |macLambda,mac|
            |coerceOrRetract| |getMsgCatAttr| |pfMapParts|
            |setMsgUnforcedAttrList| |mac0SubstituteOuter|
            |testBitVector| |macSubstituteId| |getBindingPowerOf|
            |coerceIntTableOrFunction| |postTranSegment|
            |macLambdaParameterHandling| |matSubList1| |has|
            |pfCollect| STRING2ID-N |insertPos| |pfSpread|
            |newHasCategory| |displaySetVariableSettings|
            |queueUpErrors| |thisPosIsEqual| |asyAbbreviation|
            |redundant| |macWhere,mac| |asyAbbreviation,chk|
            |thisPosIsLess| |asySplit| |rep|
            |oldAxiomPreCategoryHashCode| |checkArgs| |asyDisplay|
            |pfWDec| |opWidth| |lassocShiftQ| |matWList| |asySignature|
            |mkCircularCountAlist| $FINDFILE |asyExportAlist,fn|
            |listTruncate| |npTypedForm1| |pfCopyWithPos|
            |formArguments2String| SETDIFFERENCE |npRightAssoc|
            |setMsgPrefix| |agg| |outputMapTran0| |asyWrap| |addToSlam|
            |displayOpModemaps| DIVIDE |retractByFunction| CBESSELJ
            |BesselJ| GETL |formatOpSymbol| |coerceIntCommute|
            |everyNth| |coerceCommuteTest| |rightJustifyString|
            RBESSELI |quickAnd| |BesselI| RPSI |rPsi| |matWList1|
            |app2StringWrap| |constructT| CBESSELI $REPLACE CPSI UNIONQ
            |cPsi| |globalHashtableStats| |findSubstitutionOrder?,fn|
            |reportCircularCacheStats| |asyTypeJoinPart|
            |syIgnoredFromTo| |lassocShift| |asyTypeUnitDeclare|
            |asyCatSignature| |formJoin1| |coerceInt0|
            |sySpecificErrorHere| RDROPITEMS |objSetMode| |inclFname|
            |asyGetAbbrevFromComments,fn| |getConstantFromDomain|
            FOAM:|fputs| |union| |remHashEntriesWith0Count,fn|
            FOAM:|fputc| |intersection| |AssocBarGensym| |mathPrint1|
            |substInOrder| DEFINE-FUNCTION |sayModemapWithNumber|
            |canCoerceFrom;| |canCoerceFrom0| |getCDTEntry| |incDrop|
            |formatJoinKey| |coerceInteractive| |condAbbrev|
            |app2StringConcat0| MARKHASH |throwKeyedMsg| |scylla|
            |unabbrevRecordComponent| |unabbrevUnionComponent|
            |mkSuperSub| |inclmsgFileCycle| |linearFormatForm|
            |mkObjCode| |inclHandleSay| |incRenumberLine| WRAPDOMARGS
            |incRenumberItem| GETCONSTRUCTOR |prnd| |putModeSet|
            |bottomUpType| |lnSetGlobalNum|
            |sayIntelligentMessageAboutOpAvailability| |interpMap|
            |sayCacheCount| ADDOPERATIONS |putBodyInEnv| POSN1
            |mkObjWrap| |npMissingMate| |makePattern| |replaceLast|
            |mkMapAlias| |constructM| |coerceIntByMap|
            |installConstructor| |outputFormat| ERROR-FORMAT
            |makeVector| |canCoerceExplicit2Mapping| |mkFormalArg|
            |canCoerceUnion| |phIntReportMsgs| |canCoerceTower|
            |processMsgList| |canCoerceLocal| ASHARPMKAUTOLOADFUNCTION
            |displayRule| |makeList| |putValue| |objNew|
            |coerceIntByMapInner| |findLocalVars1| |valueArgsEqual?|
            |mkFreeVar| DELDATABASE |constantInDomain?| |HasSignature|
            |lfrinteger| |getArgValue| |scanExponent| NCONC2
            |inFirstNotSecond| |scanCheckRadix| |addDefMap|
            |coerceRetract| |pfPushMacroBody| |clearDependencies|
            |pfMacro| |getAtree| |scanInsert| |putDependencies|
            |makeNewDependencies| |leftBindingPowerOf|
            |rightBindingPowerOf| |sayDroppingFunctions| |makeRuleForm|
            |readData| |pfExit| |writeData| |mkAliasList,fn|
            |inclHandleWarning| |incTrunc| |ifCond| DAASENAME
            |incCommandTail| |replaceArgDef| |setBootAutloadProperties|
            MAKE-DATABASES |inclHandleError| |xdrOpen| |incStream|
            |setBootAutoLoadProperty| |npTypedForm| |subMatch|
            |asySimpPred| MAKEOP |asyCattranSig| MAKENEWOP |pfWhere|
            |scanIgnoreLine| |patternVarsOf1| |applyWithOutputToString|
            |asCategoryParts,build| |SymMemQ| |mkAtree1WithSrcPos|
            |mkBootAutoLoad| |posend| VMLISP::PUTINDEXTABLE |segment1|
            |assertCond| |putDependencies,removeObsoleteDependencies|
            |sublisNQ,fn| |sublisNQ| |segment2| |mkValCheck|
            |mkValueCheck| REMOVER |printNamedStatsByProperty| |pfHide|
            |dispfortexpf| |pfTagged| |getProplist|
            |mapDefsWithCorrectArgCount| |incActive?|
            |initializeTimedNames| |expression2Fortran1|
            |stringChar2Integer| |dispfortexpj| |getEqualSublis,fn|
            FOAM-USER::H-STRING |pfTLam| |insertWOC| |beenHere|
            |getMapBody| |mkAlistOfExplicitCategoryOps,fn| |insert|
            |nonRecursivePart1| |getSystemModemaps| |notCalled|
            FOAM:|fiSetDebugger| |containsOp|
            |getAllModemapsFromDatabase| |expandRecursiveBody|
            |getModemapsFromDatabase| |updateSymbolTable|
            |pfComDefinition| |simplifyMapPattern| |insertModemap|
            |depthOfRecursion| |pfQualType| |compileBody| |hasOption|
            |makeLocalModemap| |pfReturnTyped| |testPrin|
            |getLocalVars| |pfLam| |mkLocalVar| |testInput2Output|
            |pvarPredTran| |pfWrong| TRANSLABEL |canMakeTuple|
            VMLISP::WRITE-INDEXTABLE TRANSLABEL1 |addPatternPred|
            DEF-IS-REV |findLocalVars| |nonRecursivePart|
            |saveDependentMapInfo| PUSH-REDUCTION
            |printTypeAndTimeSaturn| |hyperize|
            |substituteSegmentedMsg| DEF-IS2 |mkAtree3,fn|
            |patternCheck,pos| |sameUnionBranch| DEF-IT |logicalMatch?|
            |superMatch?| |mkLessOrEqual| WHDEF |pfFromDom|
            |bottomUpCompilePredicate| |patternCheck,wild|
            |bottomUpPredicate| |match?| |processInteractive1|
            |recordAndPrint| |interpretTopLevel| PRINT-AND-EVAL-DEFUN
            |coerceInt1| PRINT-DEFUN EVAL-DEFUN BVEC-MAKE-FULL
            |suffix?| |coerceInt2Union| |coerceIntFromUnion|
            |maskMatch?| |prefix?| |asTupleNew|
            |ScanOrPairVec,ScanOrInner| |getValueFromEnvironment|
            |computeTypeWithVariablesTarget| |pfPretend|
            |mkAtreeNodeWithSrcPos| SPADRREAD REMOVE-ESCAPES
            |collectDefTypesAndPreds,addPred| |makeCompactDirect1|
            MONITOR-WRITE BVEC-CONCAT |filterListOfStrings|
            |augmentPredVector| |pushDownOp?| GET-GLIPH-TOKEN
            BVEC-EQUAL |dcOpLatchPrint| BVEC-GREATER
            |coerceIntAlgebraicConstant| |pfParen| CONTAINED BVEC-AND
            |coerceIntTower| |CONTAINED,EQ| BVEC-OR |compareTypeLists|
            |CONTAINED,EQUAL| |asTupleNewCode| BVEC-XOR DEF-LET
            |coerceBranch2Union| S+ BVEC-NAND |defLET|
            |absolutelyCannotCoerce| |pfBracket| |objNewCode| BVEC-NOR
            FOAM-USER::H-INTEGER PREDECESSOR |putMode| |dcOpPrint|
            DELLASOS |defIS| |rempropI| CHAR-EQ LASSOC |rassoc|
            LET_ERROR |pfCoerceto| QLASSQ |getI| |phParse|
            |fastSearchCurrentEnv| |objSetVal| PAIR |putFlag|
            |commandUserLevelError| LENGTHENVEC |putValueValue|
            |undoSteps| |ncloopDQlines| |undoSingleStep|
            |predicateBitIndex,pn| |phReportMsgs| |pfDWhere|
            |commandError| |deepSubCopy| |streamChop| |breaklet|
            |coerceRe2E| |ncloopPrefix?| REMAINDER |coerceVal2E|
            |createEnum| SAYBRIGHTLY1 |pfAbSynOp?| |EnumPrint|
            |mmCatComp| |sayErrorly1| |matchMmSigTar| |dqAppend|
            |lazyOldAxiomDomainHashCode| |pfFromdom| |getAndSay|
            |pfMLambda| |pfBrace| |intloopProcessString| |pfRestrict|
            |dqAddAppend| |canCoercePermute| |UnionPrint|
            |intloopPrefix?| |coerceUn2E| |coerceIntSpecial|
            |equalZero| |convertOpAlist2compilerInfo,formatSig|
            |newCanCoerceCommute| |resolveTCat| |augmentPredCode|
            |intloopInclude1| |loadLibIfNecessary| |pfWDeclare|
            |mungeAddGensyms| |pfSuch| |processInteractive|
            |optionError| |buildBitTable,fn| |ncINTERPFILE|
            |defaultTypeForCategory| |getLisplibNoCache| |makePathname|
            |evalSharpOne| |optionUserLevelError| |pfBracketBar|
            |resolveTM1| |pfIdPos| |unloadOneConstructor| GLESSEQP
            LEXLESSEQP |mkAutoLoad| |HasCategory| ASSOCIATER -REDUCE-OP
            |mergePathnames| |getOpArgTypes1| NREVERSE-N GGREATERP
            FOAM::ALLOC-PROG-INFO CGREATERP |mergeSubs| |SExprToDName|
            |autoLoad| |updateCategoryTable| |domainEqual| SORTBY
            DELASC |argCouldBelongToSubdomain|
            |systemDependentMkAutoload| |canConvertByFunction|
            |mergeOr| CONS-N SEGMENT |readLib| APPEND-N |simpOrUnion1|
            |isNestedInstantiation| |pfOr| |clearCategoryTable1|
            |makeByteWordVec2| |simpCategoryOr| |member|
            |transferSrcPosInfo| |categoryParts,build| |showInput|
            |pfAnd| |getArgValue1| |pfRetractTo| |getLisplib|
            |getConstructorOpsAndAtts| |showInOut| |pfBraceBar| DROP
            TAKE TRUNCLIST TRUNCLIST-1 |hasCaty1| |putFTText|
            |getOpArgTypes,f| SUBLISNQ |domainEqualList| SUBB |subCopy|
            |keyedSystemError| |satisfiesRegularExpressions|
            |commandErrorIfAmbiguous| |hasCatExpression| |sameMsg?|
            |getOpArgTypes| |FromTo|
            |untraceDomainConstructor,keepTraced?| |isKeyQualityP|
            |lassocSub| |CONTAINEDisDomain| |spadTrace,isTraceable|
            |setMsgForcedAttrList| |AlistAssocQ| |removeOption|
            |putTarget| |computeTTTranspositions|
            |getMinimalVariableTower| |ListMemberQ?|
            |listDecideHowMuch| |ListMember?| |isSubTowerOf|
            |decideHowMuch| |AlistRemoveQ| |isTowerWithSubdomain|
            |diffAlist| |setMsgCatlessAttr| |intloopInclude|
            |canCoerceCommute| |erMsgCompare| |compareposns|
            |insertWOC,fn| |stringPrefix?| |mkObj|
            |addDmpLikeTermsAsTarget| |coerceInt| |evalCategory|
            |transferPropsToNode| |replaceSharps| NLIST |ofCategory|
            |getBasicMode0| |npsynonym| |bottomUpIdentifier|
            |keyedSystemError1| |breakKeyedMsg| |resolveTT;| |plural|
            |resolveTM| |splitListOn| |keyedMsgCompFailure|
            |saturnKeyedSystemError| |sayKeyedMsgAsTeX| |canCoerce;|
            |canCoerce1| |resolveTT1| /READ |delete|
            |isEqualOrSubDomain| |hasCorrectTarget| AND2 |upIFgenValue|
            OR2 |dollarTran| |evalLET| |coerceIntPermute|
            |clearDependentMaps|
            |optimizeFunctionDef,replaceThrowByReturn|
            |errorSupervisor| |upLispCall| |pileForest1| |postFlatten|
            |sort| |eqpileTree| |pileForest| QUOTIENT2 |pileTree|
            |optCatch,changeThrowToGo| INTEXQUO |optCatch,hasNoThrows|
            |containedRight| |pileCtree| |pfForin| DIVIDE2
            |formatOperation| ESCAPED |getArgValueOrThrow|
            |formatOpSignature| PARSEPILES |throwEvalTypeMsg|
            ADD-PARENS-AND-SEMIS-TO-LINE |augmentTraceNames|
            |queryUserKeyedMsg| |makeSF| |pfRule|
            |optimizeFunctionDef,fn| |formDecl2String| STOREBLANKS
            INITIAL-SUBSTRING |untraceDomainLocalOps| |BooleanEquality|
            |isPatMatch| |functionAndJacobian,DF| |isPointer?|
            |isPatternMatch| |defLetForm| |pfReturn| |wt|
            |translateMpVars2PVars| |getMapSig| DO_LET |genIFvalCode|
            |getFortranType| |pfDefinition| |pfLp| |wl| |isString?|
            |getOption| |after| |funfind| |pfTree| PAIRTRACE |pfAssign|
            |defIS1| |pfIfThenOnly|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL FIXNUM) HEAPELAPSED CURRENT-CHAR-INDEX)) 
(PROCLAIM '(FTYPE (FUNCTION NIL (VALUES T T)) GENVAR))
) 
