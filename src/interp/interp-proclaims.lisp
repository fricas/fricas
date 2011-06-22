#+:GCL
(progn
  (eval-when (:execute :compile-toplevel :load-toplevel)
                 (proclaim '(optimize (safety 1) (debug 3))))

(IN-PACKAGE "BOOT") 

(PROCLAIM '(FTYPE (FUNCTION NIL (*)) FIRST-ERROR)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FIXNUM) LINE-CURRENT-INDEX LINE-LAST-INDEX
            LINE-NUMBER CHAR2NUM FOAM:|ProgHashCode| FOAM:|strLength|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FUNCTION) FOAM::FOAMPROGINFOSTRUCT-FUNCALL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FOAM:|SInt|)
            FOAM::FOAMPROGINFOSTRUCT-HASHVAL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) (VALUES T T)) MONITOR-INFO FILE-GETTER-NAME
            FOAM:AXIOMXL-FILE-INIT-NAME |fetchKeyedMsg|)) 
(PROCLAIM
    '(FTYPE (FUNCTION ((VECTOR T) (VECTOR T)) T) LEXVGREATERP
            VGREATERP)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) (VALUES T T)) |read-line|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) FIXNUM) QSREMAINDER QENUM
            FOAM:|SetProgHashCode| QSQUOTIENT)) 
(PROCLAIM '(FTYPE (FUNCTION (T T) (*)) COMPILE-DEFUN)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) *) |argumentDataError|
            |algCoerceInteractive| |selectOptionLC| |errorSupervisor1|
            |newExpandGoGetTypeSlot| |asytranFormSpecial|
            |prepareResults,defaultValue| |asytranApplySpecial|
            |asytranForm| |getVal| |asytranForm1| |evalMmCat1|
            |rewriteMap| |interpret2| |asySig1| |interpLookup;|
            |evalREPEAT| |upwhereMain| |recordInstantiation|
            |upwhereMkAtree| |NRTcompiledLookup| |evalIF|
            |parseIf,ifTran| |evalQUOTE| |evalSEQ|
            |upTaggedUnionConstruct| |upRecordConstruct| GETOP
            |upNullList| |mkIterFun| |mapRecurDepth|
            |augModemapsFromDomain1| |newExpandLocalType|
            |newExpandLocalTypeForm| |stringMatch| BUILD-DEPSYS
            |RecordEqual| |mkAtree2| |templateVal| |recordOldValue|
            |recordNewValue| CARCDRX1 |compiledLookupCheck|
            |compiledLookup| |sayFunctionSelectionResult|
            |findUniqueOpInDomain| |optSpecialCall| |hasCaty| |hasCate|
            |ncloopInclude0| |Un2E| |application2String| |selectOption|
            |evalCOLLECT| |interpCOLLECT| |newExpandTypeSlot|
            |getValueFromSpecificEnvironment| |upNullTuple|
            |upLETWithFormOnLhs| |upSetelt| |evalis| |interpret1|
            |upLETtype| |htMkPath| |exp2FortSpecial|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) *) |constructorAbbreviationErrorCheck|
            |typeToForm,fn| |appparu| |lazyMatchArg|
            |newExpandLocalTypeArgs| |asytranCategoryItem| |evalForm|
            |hasCateSpecial| |hasCate1| |asytranDeclaration| |exptApp|
            |inApp| |quoteApp| |overbarApp| |interpREPEAT| |appparu1|
            |compClam| |appsc| |appsetq| |slashApp| |appsub| |appargs|
            |binomialApp| |evalUntargetedADEF| |evalTargetedADEF| APP
            |appfrac| |appagg| |argsapp| |collectSeveralStreams|
            |basicLookup| |charyTrouble1| |collectOneStream|
            |lookupInDomainVector| |basicLookupCheckDefaults| |appmat|
            |patternCheck,mknew| |lazyMatch| |overlabelApp|
            |appHorizLine| FOAM:|fputss| |appelse| FOAM:|fgetss|
            |optCallSpecially| |hasCateSpecialNew| |makeStatString|
            |axiomType| |evalconstruct| |evalInfiniteTupleConstruct|
            |evalTupleConstruct| |collectStream1|
            |mkInterpTargetedADEF| |compileTargetedADEF|
            |oldCompLookup| |sayLooking| |interpIF| |evalTuple|
            |lookupComplete| |oldCompLookupNoDefaults|
            |lookupIncomplete|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) *) |appvertline| |apprpar1|
            |newLookupInDomain| INTERPSYS-IMAGE-INIT |apprpar|
            |matrixBorder| |applpar| |compileAndLink| |bottomUpForm|
            |bottomUpForm2| |bottomUpForm3| |bottomUpForm0|
            |bottomUpFormRetract| |bottomUpFormTuple|
            |bottomUpFormUntaggedUnionRetract|
            |bottomUpFormAnyUnionRetract| |appargs1| |appagg1|
            |lazyMatchArg2| |apphor| |applpar1| |sayFunctionSelection|
            LOCALNRLIB |compileADEFBody| |hashNewLookupInTable|
            |compileIF| |newLookupInTable|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) *) CONCAT LOCALDATABASE |listSort|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T) *) |lisplibError| |mkDiffAssoc|
            |goGetTracer|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) *) |upDollarTuple| |addModemap|
            |compHash| |makeLongStatStringByProperty|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T T) *) |makeFortranFun|
            |makeSpadFun|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) T) |intloopProcess| |pfPushBody|
            |throwKeyedErrorMsg| |mkUserConstructorAbbreviation|
            |setMsgUnforcedAttr| |coerceOrCroak|
            |throwKeyedMsgCannotCoerceWithValue| |ncHardError|
            |spad2BootCoerce| |coerceOrFail| |maprinSpecial|
            SMALL-ENOUGH-COUNT |fortFormatHead|
            |restoreDependentMapInfo| |coerceTraceArgs2E|
            |spadTraceAlias| MONITOR-PRINARGS |algEqual|
            |canCoerceByFunction1| |retractUnderDomain| |outputString|
            |lffloat| |coerceSubDomain| |infixArgNeedsParens|
            |permuteToOrder| |getOpBindingPower| EQSUBSTLIST
            |coerceDmpCoeffs| |Dmp2P| |sublisMatAlist|
            |NRTisRecurrenceRelation| OV2SE |Var2Gdmp|
            |lookupInDomainByName| |Qf2PF| L2M M2V |Var2QF|
            |sigDomainVal| |Dmp2Up| I2OI |Qf2EF| |Var2Up|
            |makeEijSquareMatrix| |L2Set| |Var2Mp| |Var2SUP| L2V HPUT
            |P2Uls| |Qf2F| |Var2NDmp| |Agg2L2Agg| |Complex2Expr|
            |coerceDmp2| |substringMatch| |domain2NDmp| I2EI
            |NDmp2domain| |Dmp2NDmp| OV2P |makeCatPred| |Var2OtherPS|
            |asytranApply| |coerceFFE| |sayMms| |Mp2Expr| |OV2poly|
            |Sy2Mp| |Sy2Var| |Mp2Dmp| |Rm2V| |Sm2M| |stuffSlot| |V2Rm|
            |Rm2Sm| |M2Rm| M2M |P2Expr| OV2OV |Rm2L|
            |writeStringLengths| |Up2Expr| |insertAlist,fn| |Agg2Agg|
            |Var2Dmp| |sySpecificErrorAtToken| |Qf2Qf| |Var2P|
            SUBSTRING |Dmp2Expr| |V2Sm| |Expr2Up| |isRectangularVector|
            V2L |P2Uts| |expandType| I2PI |P2Up| |expandTypeArgs|
            |Up2Dmp| |P2Upxs| |push_lform2| |OV2Sy| |L2Record|
            |writeXDR| |Sm2Rm| |P2Mp| |Expr2Dmp| |Sm2V| |M2Sm|
            |getFunctionFromDomain| |L2Sm| |Up2Up| |interpRewriteRule|
            |mungeAddGensyms,fn| |nsubst| |Var2FS| |makeCompilation|
            |makeAspGenerators| |getMappingArgValue|
            |makeAspGenerators1| |evalFormMkValue| |cleanUpAfterNagman|
            |coerceTypeArgs| |evalMmCond| |sideEffectedArg?|
            |evalMmCond0| |domArg2| |fortCall| |hasAttSig|
            |mkRecordFunList| |findCommonSigInDomain| |mkUnionFunList|
            |asyMakeOperationAlist| |getLocalMms,f| |mkMappingFunList|
            |mkEnumerationFunList| |getLocalMms| |matchTypes|
            |constrArg| |hasSigOr| |isOpInDomain| |BesselIAsympt|
            |postCollect,finish| |pfLambda| |PsiBack| |pfTLambda|
            |cotdiffeval| AS-INSERT |pfWIf| |npListofFun|
            |keyedMsgCompFailureSP| |throwKeyedMsgSP| |makeSpadKernel|
            |pfWith| |isEltable| |push_form2| |isLegitimateMode;|
            |hasFileProperty;| |coerceConvertMmSelection;| MSUBST
            |selectMms| |hasFilePropertyNoCache|
            |addToConstructorCache| |mapLetPrint| |exp2FortFn|
            |letPrint| |recordInstantiation1| |exp2Fort2|
            |evalIsntPredicate| |evalIsPredicate| |rwrite| ELEMN |get|
            RPLNODE |pushDownOnArithmeticVariables| |letPrint2| |rread|
            |insertEntry| |upwhereClause| |pushDownTargetInfo|
            |putAtree| |recordNewValue0| |upTableSetelt|
            |commandErrorMessage| |displayModemap,g| MKPFFLATTEN-1
            |getfortarrayexp| |assignSymbol| |fortFormatLabelledIfGoto|
            |pfIf| |needBlankForRoot| PROPERTY |assocCacheShiftCount|
            |upLoopIterIN| |assocCacheShift| |assocCache| |evalCOERCE|
            HREMPROP SUBSTEQ |charyTop| |lassocShiftWithFunction|
            ADDASSOC |loadLibNoUpdate| |charybdis| |updateDatabase|
            |interpCOLLECTbody| SUBLISLIS |incZip| |nextown|
            |orderPredicateItems| |orderPredTran| PUT |LargeMatrixp|
            |oldAxiomCategoryNthParent| |oldAxiomPreCategoryBuild|
            |writeLib1| |intloopSpadProcess,interp|
            |traceDomainLocalOps| |longext| |readLib1| |stringPosition|
            |analyzeNonRecur| |SpadInterpretStream| |splitConcat|
            |substVars| |lazyDomainSet| |substring?| |charPosition|
            |infix?| |replaceVars| |compileDeclaredMap|
            |rightCharPosition| |lazyOldAxiomAddChild| |deleteMap|
            |patternCheck,equal| |anySubstring?| |domainVal|
            |lisplibWrite| |matchSegment?| |displaySingleRule|
            |asyCattranOp1| VMLISP::MAKE-ENTRY |mkInterpFun|
            FOAM:|FormatNumber| |mkAtree3| |analyzeMap0| |asGetExports|
            |displayMap| |outputNumber| |addIntSymTabBinding|
            |rewriteMap1| |get1| |get0| |putI| PRINT-XDR-STREAM |get2|
            |throwListOfKeyedMsgs| |dcSig| |EnumEqual|
            |transferPropsToNode,transfer| |MappingEqual|
            |mkNewUnionFunList| |remprop| |resolveTTEq2| |resolveTTEq1|
            |NRTextendsCategory1| |resolveTTRed2| |extendsCategory|
            |resolveTTRed1| |matchUpToPatternVars| |getArgValueComp|
            |NRTgetLookupFunction| |resolveTMEq2| |buildPredVector|
            |simpHasPred,simpHas| |buildPredVector,fn| |altTypeOf|
            |computeTTTranspositions,compress|
            |getConditionalCategoryOfType| |extendsCategoryBasic|
            |pfInfApplication| |catExtendsCat?| |exact?|
            |filterModemapsFromPackages| |moreGeneralCategoryPredicate|
            |extendsCategoryBasic0| |encodeUnion|
            |getSubDomainPredicate| |substSlotNumbers|
            |Factored2Factored| |Sy2Up| |Dmp2Dmp| |Complex2FR|
            |coerceIntX| |Qf2domain| |Sm2L| M2L |npParenthesize|
            |augmentSub| |Sy2Dmp| |hasSigAnd| |NRTcompileEvalForm|
            |Sy2P| |unifyStructVar| |intloopInclude0|
            |coerceOrThrowFailure| |Up2P| |unifyStruct| I2NNI |SUP2Up|
            |isRectangularList| |SUP2Up_aux| |Sm2PolyType| |Mp2FR|
            DP2DP |Expr2Mp| |Mp2P| |Mp2Mp| |P2Dmp| |NDmp2NDmp|
            |npBackTrack| P2FR |L2Rm| |Up2SUP| |Dmp2Mp| |Var2UpS|
            |Ker2Ker| |Scr2Scr| |Var2OV| |hput| |L2Tuple| |Set2L|
            |Rm2M| |Up2Mp| |Ker2Expr| V2DP |Mp2Up| |Sy2OV| |Up2FR| L2DP
            |Complex2underDomain| V2M |Expr2Complex| |Rn2F| |ncPutQ|
            SPADRWRITE0 |Sy2NDmp| SPADRWRITE |coerceTraceFunValue2E|
            |recordOldValue0| |userLevelErrorMessage| |UnionEqual|
            |displayModemap| |displayType| |displayMode|
            |displayCondition| |displayValue| |makeResultRecord|
            |flowSegmentedMsg| |addBinding| |augProplistInteractive|
            |unabbrevSpecialForms| SETDATABASE |augProplist|
            |addBindingInteractive| |pileForests| |position,posn|
            |commandAmbiguityError| |clngamma| |clngammacase23|
            |clngammacase1| |nopile| |countParens|
            |filterAndFormatConstructors| |upStreamIterIN| PUTALIST
            |centerString| |checkIterationForFreeVariables|
            |subVecNodes| |PiMinusLogSinPi| |filterListOfStringsWithFn|
            |incPrefix?| |logH| |besselIcheb| |chebstarevalarr|
            |getOplistWithUniqueSignatures| |chebf01coefmake|
            |insertAlist| |oldAxiomAddChild| |compSPADSLAM|
            |augLisplibModemapsFromCategory| |getOpCode|
            |BesselasymptA| |npList| |IFcodeTran|
            |augmentLisplibModemapsFromFunctor| |restoreHistory2|
            |chebevalarr| |insertShortAlist| |PsiAsymptoticOrder|
            RWRITE |getCatForm| /MONITORX |mac0Define|
            |inclmsgIfSyntax| |compileCoerceMap|
            |oldAxiomCategoryBuild| |npAndOr| |addMap| |ncSoftError|
            |mac0InfiniteExpansion| |fnameNew| |fnameMake|
            |intCodeGenCoerce1| |mkFortFn| |findLocalsInLoop| QESET
            |rewriteMap0|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) T) NREMOVE |pfAdd| |remove| RREAD |wasIs|
            |categoryParts| BPITRACE |pfLeaf| |tokConstruct|
            MATCH-TOKEN)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T) T) |writeCFile| |Mp2MpAux2|
            /MONITOR)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) T) |mac0MLambdaApply| |clearDep1|
            |canCoerceTopMatching| |constoken| |xLate|
            |coerceImmediateSubDomain| |appconc| |bigopWidth|
            |reportFunctionCacheAll| |newLookupInCategories| |put|
            |appSum| |putHist| |appsum| |nrunNumArgCheck|
            |catPairUnion| |compareSig| |P2Us| |lazyCompareSigEqual|
            |lookupInTable| |asytranCategory| |push_form3| |evalMmCat|
            |printCName| |asGetModemaps| |evalMmFreeFunction|
            |writeMalloc| |matchMmSig| |selectLocalMms| |piApp|
            |bottomUpDefault| |printDec| |pi2App| |bracketApp|
            |braceApp| |getArgValue2| |prepareData| |hasSig| |domArg|
            |mkDomPvar| |evalMm| STRPOS |analyzeMap| |plusApp|
            |timesApp| |tensorApp| |stepApp| |aggApp| |concatbApp|
            |concatApp| |stringApp| |sigmaApp| |sigma2App| |intApp|
            |indefIntegralApp| MAKE-FLOAT |primeApp|
            |getReduceFunction| |letPrint3| |NRTgetMinivectorIndex|
            |rootApp| |superSubApp| |vconcatapp| STRPOSL
            |upLoopIterSTEP| |zagApp| |charyTrouble| |xlPrematureEOF|
            |charyElse| ASHARPMKAUTOLOADFUNCTOR
            ASHARPMKAUTOLOADCATEGORY |appneg| |appChar| |interpLoop|
            |nextown2| |boxLApp| |binomApp| |mkIterZippedFun|
            |mkAndApplyZippedPredicates| |compareSigEqual| |haddProp|
            |altSuperSubApp| |charySemiColon| |putFileProperty|
            |intloopSpadProcess| |boxApp| |analyzeUndeclaredMap|
            |analyzeNonRecursiveMap| |fixUpPredicate| |centerApp|
            |charyEquatnum| |appext| |nothingApp|
            |lazyMatchArgDollarCheck| |matchAnySegment?| |charySplit|
            |goGetTracer0| |charyMinus| |putIntSymTab| |appInfix|
            |xlConsole| |say2Split| |getConditionalCategoryOfType1|
            |simpHasSignature| |srcPosNew| |catchCoerceFailure|
            |defaultTarget| |selectMmsGen| |selectDollarMms|
            |allOrMatchingMms| |getFunctionFromDomain1| |MpP2P|
            |augProplistOf| |protectedNagCall| |mergeSort|
            |mergeInPlace| |condUnabbrev| |semchkProplist|
            |upStreamIterSTEP| |xlSkippingFin| |termMatch|
            |clngammacase2| |npEnclosed| |collectStream|
            |interpCOLLECTbodyIter| |newLookupInAddChain| |xlOK|
            |getFileProperty| |xlCmdBug| |BesselasymptB|
            |hashNewLookupInCategories| |xlPrematureFin|
            |newLookupInCategories1| |expandDO| |xlSkip|
            |lazyMatchAssocV| |xlIfBug| |lookupDisplay|
            |mac0ExpandBody| |makeInternalMapName|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) T) |coerceByTable| |augmentMap|
            |compileRecurrenceRelation| |newCompareSig|
            |commuteFraction| |getNewDefaultPackage|
            |commuteSparseUnivariatePolynomial|
            |reportFunctionCompilation| |commuteUnivariatePolynomial|
            |needStar| |coerceDmp1| |commuteSquareMatrix|
            |commutePolynomial| |commuteQuaternion|
            |bottomUpDefaultCompile| |bottomUpDefaultEval|
            |getArgValueComp2| |prepareResults| |spadify|
            |makeConstrArg| |selectMms1;| |selectMms2| |interpLoopIter|
            |fortFormatDo| |concatApp1| |xlNoSuchFile| |split2|
            |aggregateApp| |analyzeRecursiveMap| |charyBinary|
            |analyzeDeclaredMap| |mkCacheVec| |concatTrouble|
            |putSrcPos| |resolveTT2| FINCOMBLOCK |matchMms|
            |commuteNewDistributedMultivariatePolynomial|
            |commuteMPolyCat| |orderMms|
            |commuteDistributedMultivariatePolynomial| |commuteComplex|
            |commuteMultivariatePolynomial| LOCALASY |xlMsg|
            |printLabelledList| |xlFileCycle| |xlSay| |xlOK1| |incLine|
            |logS| |incLude| |xlCannotRead| |xlConStill| |msgCreate|
            |xlConActive|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T) T) |bigopAppAux| |appInfixArg|
            |Mp2SimilarDmp| |Expr2Dmp1|
            |lazyOldAxiomDomainLookupExport| |findFunctionInCategory|
            |Mp2MpAux1| |Mp2MpAux0| |findFunctionInDomain|
            |invokeNagman| |abbreviationError|
            |oldAxiomDomainLookupExport|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) T) |mmCost| |invokeFortran|
            |bracketagglist| |genMapCode| |putMapCode| BUILD-INTERPSYS
            |incLine1| |findFunctionInDomain1| |mmCost0|
            |goGetTracerHelper| |makeFort| |nagCall| |BesselIBackRecur|
            |compDefineLisplib| |oldAxiomCategoryLookupExport|
            |xlIfSyntax|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T *) T) |asCategoryParts| |lnCreate|
            TOKEN-INSTALL)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T T T T) T) |P2DmpAux|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T) T) |P2MpAux| |makeFort1|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T *) T) RPLACSTR)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL *) |quit| |scanS| RESETHASHTABLES |testPage|
            |sendHTErrorSignal| |executeQuietCommand| |parse_Label|
            |sendNagmanErrorSignal| |parse_Enclosure|
            |printStatisticsSummary| |printStorage| |parse_Sequence|
            |parse_Sexpr| |npPrimary1| |parse_Primary1| |parse_Sexpr1|
            |copyright| |parse_Category| |pquit| RECLAIM |clearCmdAll|
            |listConstructorAbbreviations| |displayFrameNames|
            |queryClients| MKPROMPT |updateHist| |leaveScratchpad|
            |princPrompt| |describeSetStreamsCalculate| |/RQ,LIB| /RF-1
            |extendConstructorDataTable| HELP /RQ FAIL /RF
            |npDefinitionItem| |npDefn| |npMacro| |disableHist|
            |quitSpad2Cmd| |generateResultsName| |generateDataName|
            |getWorkspaceNames| |pquitSpad2Cmd| |npMDEFinition|
            |npCategory| |getInterpMacroNames| |npRule|
            |readSpadProfileIfThere|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL T) CURRENT-CHAR NEXT-CHAR |npSuch|
            |intSetNeedToSignalSessionManager| |npCommaBackSet|
            |npSigItem| |scanDictCons| |npPPf| |npPower| |scanEsc|
            PREV-LINE-SHOW |scanError| |npSignature| NEXT-LINES-SHOW
            TOKEN-STACK-SHOW |scanEscape| NEXT-LINES-CLEAR
            |returnToTopLevel| |returnToReader| |scanToken| |stopTimer|
            |scanNumber| |scanString| |scanSpace| |scanPunct|
            |resetStackLimits| |scanNegComment| |startsNegComment?|
            |scanComment| |startsComment?| BUMPCOMPERRORCOUNT
            |spadReply| |pcounters| |ptimers| MONITOR-UNTESTED
            MONITOR-HELP MONITOR-INITTABLE MONITOR-END MONITOR-AUTOLOAD
            MONITOR-PERCENT FRICAS-RESTART |interpsysInitialization|
            |prTraceNames| |cc| |resetTimers| |resetSpacers|
            |makeInitialModemapFrame| |loadExposureGroupData|
            |statisticsInitialization| |initHist| |scanPunCons|
            |initializeInterpreterFrameRing| CURRENTTIME
            |scanKeyTableCons| |spadStartUpMsgs| RESTART0 FRICAS-INIT
            |spad| |syGeneralErrorHere| |oldParserAutoloadOnceTrigger|
            |parse_Expression| |npRecoverTrap| |sayNewLine|
            INIT-BOOT/SPAD-READER |parse_Expr1000| |parse_TokTail|
            |initializeSystemCommands| |parse_ElseClause|
            |parse_Iterator| |parse_Primary| POP-REDUCTION |parse_Name|
            TOP |parse_Reduction| |parse_Form| |parse_Float| |asList|
            |parse_PrimaryNoFloat| |spadpo| |parse_VarForm|
            |intUnsetQuiet| |intSetQuiet| |parse_Quad| |parse_String|
            |parse_IntegerTok| |parse_FormalParameter|
            |parse_IteratorTail| |init_parser_properties|
            |makeConstructorsAutoLoad| |initializeRuleSets|
            |initNewWorld| |resetWorkspaceVariables| |version|
            FOAM:|fiGetDebugVar| |parse_SemiColon| |parse_InfixWith|
            |parse_Suffix| |npAssign| |parse_Seg| |npIterators|
            |parse_Loop| |npSigDecl| |parse_Import| |parse_With|
            |parse_Data| |npType| |parse_LabelExpr| |parse_Return|
            |parse_Leave| |parse_Exit| |npSDefaultItem|
            |parse_Conditional| |npDefinition| |nangenericcomplex|
            |parse_Infix| |parse_Prefix| |npADD| |parse_Qualification|
            |npQualTypelist| |npAssignment| |npPrimary2| |npItem|
            |parse_ReductionOp| |npDefaultItem| |parse_Application|
            |npDefaultDecl| |parse_Selector| |npApplication2|
            |npFirstTok| |parse_AnyId| |npRelation| |parse_Sequence1|
            |npVariable| |credits| |npCategoryL| |npTerm|
            |printableArgModeSetList| |npRemainder| |cacheStats|
            |clearClams| |npMdef| |clearCategoryCaches|
            |processGlobals| |processGlobals1| |clearConstructorCaches|
            |allConstructors| |npAtom2| |npPrefixColon| |npAmpersand|
            |piles| |npInfixOperator| |npPileExit|
            |npPileDefinitionlist| |npTypeStyle|
            |statRecordInstantiationEvent| |reportAndClearClams|
            |clearConstructorAndLisplibCaches| |newFortranTempVar|
            |updateCurrentInterpreterFrame| |clearHashReferenceCounts|
            |tempLen| |reportInstantiations| |removeAllClams|
            |currentSP| |npSynthetic| |npAmpersandFrom| |npQualType|
            |createCurrentInterpreterFrame| |getIntrinsicList|
            |npLambda| |initHistList| |npVariablelist| |npEncl|
            |clearFrame| |npSemiBackSet| |npDisjand|
            |clearCmdSortedCaches| |npLogical| |clearCmdCompletely|
            |npSCategory| |mkOutputConsoleStream| |npPCg|
            |npDefinitionlist| |resetInCoreHist| |npSum| |npPPff|
            |npDiscrim| |nextInterpreterFrame| |npSLocalItem|
            |previousInterpreterFrame| |inclmsgIfBug| |allOperations|
            |clamStats| |printFirstPrompt?| |clearMacroTable| |intloop|
            |displayExposedConstructors| |describeSetOutputFortran|
            |initExposureHash| |traceReply| |pspacers|
            |describeSetFortTmpDir| |sayAllCacheCounts| TERSYSCOMMAND
            |describeSetFunctionsCache| |fin|
            |describeInputLibraryArgs| |describeAsharpArgs|
            |describeSetLinkerArgs| |describeOutputLibraryArgs|
            |describeSetOutputTex| |random| |describeSetOutputAlgebra|
            |describeSetOutputTexmacs| /TRACEREPLY
            |describeSetOutputMathml| |describeSetOutputHtml|
            MONITOR-READINTERP |ncError| |getParserMacros|
            |describeSetFortDir| |quadSch| |setViewportProcess|
            MONITOR-RESULTS |describeSetOutputOpenMath| MONITOR-REPORT
            |waitForViewport| |describeSetOutputFormula|
            |processSynonyms| CLEAR-HIGHLIGHT RESET-HIGHLIGHT YEARWEEK
            |getCodeVector| |?t| |resetCounters| |genTempCategoryTable|
            |NRTmakeCategoryAlist| |spadThrow| |terminateSystemCommand|
            |spadPrompt| |genCategoryTable| |getInfovecCode|
            |simpCategoryTable| |displayPreCompilationErrors|
            |simpTempCategoryTable| |npPCff| |npSelector| |npPrimary|
            |npQuiver| |npLocal| |coercionFailure| |npTypeVariablelist|
            |dcSizeAll| |npInline| |npTypeVariable|
            |npSignatureDefinee| |npImport| |npLocalItem| |npLocalDecl|
            |npSingleRule| |npDefTail| |npTypified| |npFree|
            |npDefinitionOrStatement| |voidValue| |npDef| |npStatement|
            |npState| |npBreak| |popTimedName| |pfNothing|
            |peekTimedName| |npVariableName| |npDecl| |npBacksetElse|
            |npGives| GET-INTVAL |npReturn| |parse_KEYWORD| |npExpress|
            |statRecordLoadEvent| |npAssignVariable| SPAD_LONG_ERROR
            |npColon| SPAD_SHORT_ERROR |npIterate| |computeElapsedTime|
            |computeElapsedSpace| |npLoop| |displayHeapStatsIfWanted|
            |statisticsSummary| CURRENT-SYMBOL |parse_SPADSTRING|
            |histFileName| |parse_ARGUMENT-DESIGNATOR| ADVANCE-TOKEN
            |parse_SPADFLOAT| |parse_IDENTIFIER| |rbrkSch|
            |parse_NUMBER| |parse_AKEYWORD| |lbrkSch| |reportCount|
            |startTimer| |clock| |updateFromCurrentInterpreterFrame|
            |oldHistFileName| |categoryOpen| |browseOpen|
            |operationOpen| |interpOpen| |interpFunctionDepAlists|
            |compressOpen| |getParserMacroNames| |createInitializers|
            WRITE-WARMDATA |displayWorkspaceNames| WRITE-INTERPDB
            WRITE-CATEGORYDB WRITE-OPERATIONDB WRITE-BROWSEDB
            WRITE-COMPRESS |saveDependentsHashTable|
            |saveUsersHashTable| |checkWarningIndentation|
            |mkLowerCaseConTable| |parse_new_expr| BOOT-SKIP-BLANKS
            INITIAL-GETDATABASE |sayShowWarning| REDUCE-STACK-SHOW
            |reportWhatOptions| |inclmsgConsole| |inclmsgFinSkipped|
            |synonymSpad2Cmd| |pop_stack_1| |pop_stack_2|
            |getSystemCommandLine| |pop_stack_3| |current_symbol|
            |writeHiFi| |next_symbol| |intNewFloat| |advance_token|
            |updateInCoreHist| |npPop1| PREV-LINE-CLEAR
            |displayExposedGroups| |npAtom1| |npFromdom|
            |npBDefinition| |npDollar| |npConstTok|
            CURRENT-SCANNER-CHAR |npName| NEXT-SCANNER-CHAR
            |npPDefinition| |npLocalItemlist| |writeHistModesAndValues|
            |poNoPosition| |frameNames| ADVANCE-CHAR
            |npQualifiedDefinition| |inclmsgCmdBug| |npColonQuery|
            |displayHiddenConstructors| |npPop2| IOSTAT |npPretend|
            |makeClosedfnName| |npPop3| |npRestrict| |pfNoPosition|
            |npTrap| |npCoerceTo| |npExit| |npPushId| |npSuchThat|
            |npBy| |npConditionalStatement| |npInterval|
            |historySpad2Cmd| |npSegment| |npQualDef| |npArith|
            |npMDEF| |npAssignVariablelist| |npApplication| |npWhile|
            |npTagged| |npSQualTypelist| |npIterator| CURRENT-TOKEN
            |npForIn| NEXT-TOKEN |npSigItemlist| |npVoid| |npNext|
            |incConsoleInput| |npInfixOp| |npSymbolVariable| |npTyping|
            |npId| |npDefaultItemlist| |npBPileDefinition| |npExport|
            |npFix| EMBEDDED |npLet| |initialiseIntrinsicList|
            |npComma| |ncTopLevel| |npExpress1| |ncIntLoop|
            |setOptKeyBlanks| |npDefaultValue| |npMatch| |npProduct|
            |runspad| |npAssignVariableName| |evalInlineCode|
            |createResolveTMRules| NEXT-SYMBOL |createResolveTTRules|
            |npPPg|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) *) NEXT-LINE INIT-MEMORY-CONFIG |newGoGet|
            META-SYNTAX-ERROR |Undef| $ERASE CROAK)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) *) GET-TOKEN |fortError1| |combineMapParts|
            |fortFormatCharacterTypes,par2string|
            |mac0InfiniteExpansion,name| MONITOR-PRINTREST
            |fortFormatCharacterTypes,mkCharName| |IdentityError|
            BPINAME |outputTran| OBEY |coerceUnion2Branch| |poFileName|
            |handleLispBreakLoop| |popSatOutput| |sayString|
            |retract2Specialization| |subspan| |spleI|
            |compileTimeBindingOf| |postMakeCons| |optRECORDELT|
            |optSETRECORDELT| |optCallEval| |obj2String|
            |outputTranMatrix,outtranRow| |newHasTest,evalCond|
            |parseSeq| |parseTran| |postSlash| |postConstruct|
            |postBigFloat| |postin| MAKE-DEPSYS |postIn| |brightPrint0|
            |mathObject2String| |makeSimplePredicateOrNil|
            |form2StringAsTeX| |prefix2StringAsTeX| |transSeq|
            |form2String| |form2StringLocal| |brightPrintCenter|
            |form2StringWithPrens| |parseLeftArrow|
            |brightPrintCenterAsTeX| |unparseInputForm|
            |escapeSpecialChars| |startReplaceHTPage| |formatOpType|
            |asytranEnumItem| |brightPrintHighlightAsTeX| |startHTPage|
            |productOrParen| MAKE-OUTSTREAM |lispType| |powerOrParen|
            BOOT-LOAD |issueHT| |linkToHTPage| |undoFromFile|
            |startHTPopUpPage| |pfFileName| |killHTPage| |porigin|
            |tuple2String,fn2| |multiToUnivariate|
            |getModeSetUseSubdomain| |asyCattran1| |getModeSet|
            |prefix2String0| |reportOpSymbol| |postTran|
            |restoreHistory| |pfname| |changeHistListLen| |showHistory|
            |bottomUp| |parseAndEvalStr1| |prefix2Infix| |matchMmCond|
            |asyTypeJoinPartWith| |binop2String| |sumOrParen|
            |getCType| |formString| |removeIsDomains|
            |formatAttributeArg| |justifyMyType| |coerceOrParen|
            |appOrParen| |form2String1| |formWrapId|
            |functionAndJacobian| |parseLessEqual| |parseGreaterEqual|
            |parseNotEqual| |parseAnd| |unVectorize| |parseIf|
            |postCategory,fn| |NRTinnerGetLocalIndex| |parseNot|
            |parseOr| |compQuietly| |compileFileQuietly|
            |compileQuietly| |exptSub| |npPP| |aggSub| |aggSuper|
            |sayBrightlyNT| |quoteSub| |quoteSuper| |compiler| |cd|
            |clear| |close| |dnf2pf| |getValue| |frame| |bottomUpElt|
            |history| |bottomUpPercent| |getUnname| |upisnt|
            |upisAndIsnt| |pfCollectArgTran|
            |mkParameterList,par2string| |uperror| |savesystem|
            |pf2Sex1| |set| |upis| |show| |uppretend| |summary|
            |parseTranList| |pfRhsRule2Sex| |uplocal| |what| |getMode|
            |pfLhsRule2Sex| COMP370 |pfDefinition2Sex| |rootSub|
            |editFile| |fortPre1| |parseLhs| |pfLiteral2Sex| |upDEF|
            |safeWritify| |upQUOTE| |apropos| |upbreak| |upreturn|
            |upDollar| |upfree| |writifyComplain| |subSuper| |uphas|
            |postInSeq| |upREPEAT0| MAKE-APPENDSTREAM |upREPEAT|
            MAKE-INSTREAM |uptypeOf| |printBasic| |upSEQ| |upequation|
            |vConcatSuper| |htCommandToInputLine| INIT-FILE-GETTER
            |upCOERCE| |systemCommand| INIT-LIB-FILE-GETTER
            |frameSpad2Cmd| |upADEF| |systemErrorHere|
            |addNewInterpreterFrame| |importFromFrame| |tabsToBlanks|
            |showSpad2Cmd| |upAlgExtension| |clearCmdParts|
            |undoInCore| |compileAsharpCmd1| |withAsharpCmd|
            |systemError| MAKE-REASONABLE FILE-RUNNER |spadcall1|
            |mathPrint| |agggsub| |interpOnlyCOLLECT| |agggsuper|
            |upCOLLECT| |upDeclare| |upcase| |mkEvalableCategoryForm|
            |mathprintWithNumber| |setOutputFortran| |setExpose|
            |parseAndEvalStr| |setExposeDrop| |setExposeAdd|
            |abbreviations| |qTSub| |phBegin| |qTSuper|
            |replaceGoGetSlot| |getSlot1FromCategoryForm|
            |stopTimingProcess| |getBrowseDatabase| |poGlobalLinePosn|
            |setFunctionsCache| |mkAlistOfExplicitCategoryOps| |matSub|
            |mkAlistOfExplicitCategoryOps,atomizeOp| |setLinkerArgs|
            |pf2Sex| |error| |setOutputTex| |initializeLisplib|
            |setOutputMathml| |asyCosig| |setOutputTexmacs|
            |asyCosigType| |setOutputHtml| |mkAtree1|
            |setStreamsCalculate| |string2BootTree| |setOutputOpenMath|
            |superspan| |brightPrintHighlight| |asyTypeJoinPartPred|
            |setOutputFormula| |brightPrint0AsTeX| |asyTypeUnit|
            MONITOR-RESTORE |sayMSGNT| |asyJoinPart| |incHandleMessage|
            |asyGetAbbrevFromComments| |predicateBitIndex| |getMsgPos2|
            |simpHasPred,eval| |computedMode| |parseTransform|
            |getMsgTag| |doSystemCommand| |predicateBitIndexRemop|
            |simpHasPred,simp| |mkAtree| |transHasCode| |optCall|
            |optSPADCALL| |evalMmStackInner|
            |normalizeStatAndStringify| VMLISP::DELETE-DIRECTORY
            VMLISP::GET-IO-INDEX-STREAM VMLISP::GET-INPUT-INDEX-STREAM
            |ncAlist| |abbreviationsSpad2Cmd| |saveHistory| |intern|
            |ncTag| |histFileErase| |formatSignatureArgs0|
            |setHistoryCore| |numOfSpadArguments| |formatSignatureArgs|
            |undoCount| |prefix2String| |formIterator2String|
            |findEqualFun| |formJoin2String| |spadTypeTTT|
            |fixObjectForPrinting| |reportOpsFromUnitDirectly0|
            |pred2English| |reportOpsFromUnitDirectly1|
            |vectorOfFunctions| |object2String| |formJoin2|
            |mkEvalable| |abbQuery| |userError| |whatSpad2Cmd|
            |dewritify| |dewritify,dewritifyInner| |simpBool|
            |upconstruct| |clearSpad2Cmd| |unAbbreviateKeyword| |upand|
            MSORT |string2Float| NMSORT |orderList| |c_to_r|
            |ExecuteInterpSystemCommand|
            |InterpExecuteSpadSystemCommand| |upCOLLECT0| |upCOLLECT1|
            |upTARGET| |getDomainFromMm| |upor| COMP-2
            |parseFromString| |lnFileName| |npPC| |pfGlobalLinePosn|
            |upIF| |whatConstructors| |upTuple| |getUnname1|
            |interpOnlyREPEAT| |upREPEAT1| |upiterate| |DNameToSExpr1|
            |upLETWithPatternOnLhs| |upLET| |verbatimize|
            |wrapMapBodyWithCatch| |fortPreRoot| |checkPrecision|
            |tokType| |fix2FortranFloat|
            |simplifyMapPattern,unTrivialize| |predTran|
            |setOutputAlgebra|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) T) $FILEP READ-A-LINE TOPLEVEL MAKE-LINE
            MAKE-STACK |throwMessage| MONITOR-ENABLE MONITOR-TESTED
            MONITOR-RESET MONITOR-DISABLE INITROOT
            FOAM::MAKE-FOAMPROGINFOSTRUCT VMLISP::MAKE-LIBSTREAM
            |Union| |Mapping| |dcSize| |spadCompile| |RecordCategory|
            |EnumerationCategory| |dc| |UnionCategory| |canCoerce|
            |canCoerceFrom| |coerceConvertMmSelection|
            |hasFileProperty| |isLegitimateMode| |isValidType|
            |resolveTT| |selectMms1| |underDomainOf| |findRetractMms|
            |getConstantFromDomain| |interpLookup| SAY MOAN |synonym|
            MAKE-SPAD-KERNEL RKEYIDS |incRgen1| |next1| |incZip1|
            |incAppend1| |nextown1| |incIgen1| |runOldAxiomFunctor|
            |buildBitTable| RDEFINSTREAM RDEFOUTSTREAM
            MAKE-MONITOR-DATA MAKE-XDR-STREAM |concat| |sum|
            |displayCategoryTable| |Enumeration0| ENABLE-BACKTRACE
            MAKE-DATABASE SPAD_SYNTAX_ERROR INTERRUPT MAKE-TOKEN
            NEXT-BOOT-LINE IOCLEAR |incLude1| MAKE-REDUCTION)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) CHARACTER) LINE-CURRENT-CHAR NUM2CHAR EBCDIC)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) FIXNUM) LINE-NEW-LINE)) 
(PROCLAIM '(FTYPE (FUNCTION (T) STRING) LINE-BUFFER |make_spaces|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) T) |maximalSuperType| |From| |isDomain|
            |npWConditional| |expression2Fortran| |removeBodyFromEnv|
            MAKE-SYMBOL-OF |pfTweakIf| |macSubstituteOuter|
            TRY-GET-TOKEN |poNopos?| SMALL-ENOUGH |dispfortexp1|
            |pf0ApplicationArgs| |getIteratorIds| |hashString|
            |pfCheckItOut| |pfApplicationOp| |fortran2Lines1|
            |listOfVariables| |setAutoLoadProperty| HAS_SHARP_VAR
            LINE-AT-END-P LINE-NEXT-CHAR |lfstring| |isSharpVarWithNum|
            |pfNothing?| |getUserIdentifiersInIterators|
            LINE-ADVANCE-CHAR |nextline| |npSemiListing| |pfMacroRhs|
            |checkType| |compFailure| IS_SHARP_VAR |pfMacroLhs|
            |fortFormatCharacterTypes,mkParameterList2| |mkSharpVar|
            |rdigit?| |remFile| MAKE-STRING-ADJUSTABLE |remLine|
            |pfPile| |sayRemoveFunctionOrValue| |npMissing|
            MONITOR-BLANKS |lferror| |makeArgumentIntoNumber|
            |queryUser| |exp2FortOptimizeCS1| |scanWord|
            |getMsgInfoFromKey| |poPosImmediate?| |fortSize,elen|
            WHOCALLED SPADSYSNAMEP |dispStatement| |digit?| |nameLen|
            |fortFormatTypes,unravel| LINE-P |indentFortLevel|
            |reassembleTowerIntoType| |mathprint| |punctuation?|
            |fortFormatCharacterTypes| VMLISP::SPAD-FIXED-ARG |lfid|
            |fortexp0| |pushSatOutput| |retract1| STACK-P |mkMessage|
            |flattenOps| |fortFormatElseIf| |fortSize| |fortFormatIf|
            |lfinteger| |sayALGEBRA| |lfspaces| |postBlockItemList|
            |postType| |optRECORDCOPY| |scanTransform| |comma2Tuple|
            |optCond| |height| |outputOp| |TruthP| |keyp|
            |addTraceItem| |aggwidth| |optLESSP| |maprin0| |htmlFormat|
            |optCatch| |outputConstructTran| |optimize| |outputTranSEQ|
            |outputTranRepeat| |subrname| |outputTranReduce| |optCons|
            |outputTranCollect| |outputTranIf| |outputMapTran|
            |mkCircularAlist| |outputTranMatrix| MONITOR-DELETE
            VMLISP::FLAT-BV-LIST |texmacsFormat| |nodeCount|
            |matSuperList| |matSubList| |matLSum| MONITOR-DIRNAME
            MONITOR-FILE |matLSum2| |parseReturn| |PushMatrix|
            |parseSegment| |makeInternalMapMinivectorName| MONITOR-DECR
            |parseWhere| |getCacheCount| VMLISP::EQUABLE |putWidth|
            |Zeros| |mkAuxiliaryName| |mkCacheName| |postWith| |absym|
            |timedOptimization| |syminusp| |postQUOTE| |compileBoot|
            |postCollect| |stupidIsSpadFunction|
            FOAM::PROCESS-IMPORT-ENTRY |clearCache| |tabber|
            |clearAllSlams| |brightPrintRightJustify| |clearSlam|
            |isQuotient| |scanW| |postRepeat| |addBlanks|
            |noBlankBeforeP| |lfkey| |postTupleCollect| |noBlankAfterP|
            |keyword?| |postAdd| SHUT |isListOfIdentifiers| |scanKeyTr|
            |clearTempCategoryTable| |mkCategoryExtensionAlist|
            |postReduce| |isListOfIdentifiersOrStrings| |scanPossFloat|
            |mkCategoryExtensionAlistBasic| |sayDisplayWidth,fn|
            |scanCloser?| |postComma| |sayDisplayWidth|
            |formatSignature| |getDomainHash| |keyword| |LZeros|
            |blankIndicator| |getConstructorModemap| REROOT
            |lineoftoks| |postSemiColon| |saySpadMsg| |form2FenceQuote|
            DIG2FIX |postWhere| |form2FenceQuoteTail| |postColonColon|
            |showCategoryTable| |postColon| |string2Words|
            |transCategoryItem| |asyTypeItem| |lfnegcomment|
            |postAtSign| |asyExportAlist| |lfcomment| |postPretend|
            |isTraceGensym| |postIf| |updateCategoryTableForCategory|
            |operationLink| |asytranLiteral| |getTraceOption,hn|
            |postJoin| LOG2 |stackTraceOptionError|
            |getBpiNameIfTracedMap| |postSignature| |unabbrev|
            |postCategory| |stuffDomainSlots| |brightPrint1|
            |asyPredTran1| |sayBrightlyLength1| SIZE |getLookupFun|
            |postDef| |checkForBoolean| |findSubstitutionOrder?| EOFP
            |dcAll| |pfSourceStok| |ppos| |postMDef|
            |formatOperationAlistEntry| RSHUT |npMoveTo| |dcData|
            |brightPrint| |formatIf| |last| |dcCats| |postMapping|
            |sayTexmacs| |editSpad2Cmd| |canRemoveIsDomain?|
            |pfLinePosn| |dcPreds| |pfCharPosn| |dcAtts| |postExit|
            |splitSayBrightlyArgument| |npboot| |expr2String|
            |asyCattran| |pfImmediate?| |dcOpTable|
            |splitListSayBrightly| |atom2String| |push_lform0|
            |pfNoPosition?| |dcSlots| |postTuple|
            |dbSpecialDisplayOpChar?| |setIOindex| |simpCattran|
            |%fname| |getInfovec| |stripLisp| |asMakeAlistForFunction|
            |push_form0| |%id| |orderMmCatStack| |polyVarlist|
            |removeAttributePredicates,fn| |addSpaces|
            |formTuple2String| |hackToRemoveAnd| |removeQuote|
            |%origin| |listOfPredOfTypePatternIds| |asyArgs| DIGITP
            |parseAndEvalToStringEqNum| |removeAttributePredicates,fnl|
            |nplisp| |object2Identifier| |asyArg| |resolveTypeList|
            |XDRFun| STACK-SIZE |asyCatItem| |parseAndInterpret|
            |dcOps| |pathnameTypeId| |constructorName| VEC2LIST
            |protectedEVAL| |formatSlotDomain| |int2Bool| |abbreviate|
            |parse_GliphTok| |replaceSharpCalls|
            |parseAndEvalToStringForHypertex| |asyTypeJoinItem|
            |getSymbolType| MAKE-VEC |parseAndEvalToHypertex| |piSub|
            |asyTypeJoinPartIf| |containsVars1| GCMSG
            |parseAndEvalToString| |piSup| STACK-CLEAR
            |formatPredParts| |asyTypeJoinPartExport| |evalMmDom|
            |parseAndInterpToString| |piWidth| |mkQuote,addQuote|
            REDUCTION-VALUE |asyCATEGORY| |asyExtractDescription|
            |pi2Sub| GENSYMP |formatMapping| |trimComments|
            |isValidType;| |pi2Sup| |pi2Width| STACK-STORE
            |NRTtypeHack| |asyTypeJoin| |fixUpTypeArgs| |categoryForm?|
            |aggWidth| |mkQuote| |devaluateDeeply| |asyTypeJoinStack|
            |formatAttribute| |getConstructorSignature| |printAsTeX|
            LINE-PRINT READLINE |isPartialMode| |constructor|
            |asyTypeMapping| LINE-PAST-END-P |evaluateFormAsType|
            |asyExtractAbbreviation| |evaluateType1| |spad2lisp|
            |makeOrdinal| VMLISP::LIBSTREAM-INDEXTABLE |vec2Lists|
            |getAndEvalConstructorArgument| |asyType| |complexRows|
            TOKEN-SYMBOL |evaluateType| |evaluateSignature|
            |asMakeAlist| |emptyAtree| |zeroOneConversion|
            |parse_NBGliphTok| |makeOutputAsFortran| |asySubstMapping|
            |getKeyedMsg| |asyParents| |segmentKeyedMsg|
            |parseGreaterThan| |isMap| |asyDocumentation| |printMms|
            |parseColon| |parseCoerce| |parseAtSign|
            |ncParseAndInterpretString| |recordAndPrintTest|
            |parseCategory| |parseConstruct| |parseDEF| |parseExit|
            |parseHas| |getIProplist| |tuple2List| |asTupleAsVector|
            |parseIn| BUMPERRORCOUNT |makeSpadConstant| MAKE-CVEC
            |parseInBy| FOAM:|fiStrHash| |parseIs| |markUnique|
            |parseIsnt| |addConsDB| COMP |parseJoin| |parseLeave|
            FOAM:|fiGetDebugger| |parseLET| |NRTaddInner| |parseMDEF|
            |compAndDefine| |parsePretend| FOAM:|fiSetDebugVar|
            |postDefArgs| |c_to_rf| |sumWidth| |pfReturnFrom| |pfNot|
            |unTuple| |timesWidth| |pfLoop1| |postIteratorList|
            |pfDWhereContext| |phiRatapprox| |tensorWidth| |break|
            |npCompMissing| |exptWidth| |pfTLambda?| |PsiIntpart|
            |pfExpression?| |exptSuper| |pfParts| |lnrgammaRatapprox|
            |pfWDeclareDoc| |r_lngamma| |stepWidth|
            |pfDefinitionSequenceArgs| |stepSub| |pf0SequenceArgs|
            |pfQualTypeType| REMDUP |stepSuper| |pfAppend| |pfWith?|
            ASSOCLEFT |pfSequence?| ASSOCRIGHT |inWidth|
            |pfTaggedToTyped| |inSub| |inSuper| |pfId|
            |pfSequenceToList| |pfSecond| |pfMLambdaArgs| |npAdd|
            |pfTypingItems| |pfFreeItems| |npLetQualified| |pfInline|
            |concatSub| |sayBrightlyI| |concatSuper| |pfSuchthat|
            |parse_Expr| |concatbWidth| |pfComDefinitionDef|
            |pfCollectVariable1| |sayMSG2File| |parse_LedPart|
            |concatWidth| |pfWDeclareSignature| |parse_NudPart|
            |pfNovalue| |pfWhereContext| |pfQualType?| |list3|
            SPAD-KERNEL-OP |pfCollect1?| |notCoaf| |quoteWidth|
            SPAD-KERNEL-ARG |pfTaggedToTyped1| SPAD-KERNEL-NEST
            |testPredList| |stringWidth| SPAD-KERNEL-P
            |transOnlyOption| |spadReply,printName| |tokPart| |notDnf|
            |sigmaSub| |sigmaSup| |npWith| |sigmaWidth| |walkWhereList|
            |be| |reduceDnf| MKQ |handleKind| |display| |ordList|
            |retract| |sigma2Sub| |underDomainOf;| DROPTRAILINGBLANKS
            |isNiladic| |edit| |sigma2Sup| |getCon| |list1|
            |sigma2Width| |help| |bnot| |getUnionOrRecordTags| |b2dnf|
            |getModeOrFirstModeSetIfThere| |clearCategoryCache|
            |intSub| |expandMacros| |library| VMLISP::LIBSTREAM-P
            |isType| |intSup| |pfCheckMacroOut| |load| |bor| |intWidth|
            |ltrace| |reportHashCacheStats| |traceSpad2Cmd|
            |isTupleForm| |nopiles| |band| |getTarget|
            |rulePredicateTran| |eval| |untrace| |bassert|
            |indefIntegralSub| |isLegitimateRecordOrTaggedUnion|
            |indefIntegralSup| |listOfDuplicates| |pfTLambdaArgs|
            |ruleLhsTran| |displayCacheFrequency| |read|
            |indefIntegralWidth| |isPolynomialMode| |pfForinLhs|
            EXPAND-TABS |mkHashCountAlist| |pf0WrongRubble|
            |remHashEntriesWith0Count| |statement2Fortran|
            |mkAtreeNode| |overlabelSuper| |pfWrongRubble|
            |npListAndRecover| |pfCollectBody| |fetchOutput|
            |overlabelWidth| |pf0AddBase| |parseAtom|
            |pfCollectIterators| |npPileBracketed| |breakIntoLines|
            |numberOfEmptySlots| |fortranCleanUp| |spool|
            |bottomUpUseSubdomain| |pfAddBase| |parseTran,g|
            |pfCollect?| |exp2FortOptimize| |isHomogeneousArgs|
            |getBasicObject| |pfOp2Sex| |fortPre| |overbarSuper|
            |getUnderModeOf| |pfAttributeExpr| |transIs| |segment|
            |isMapExpr| |overbarWidth| |deconstructT| |pfRetractTo?|
            |isListConstructor| |pmDontQuote?| |exp2Fort1| |trace|
            |pfWIfElse| |transIs1| |pfSymbol?| |undo| |objVal|
            |pfTupleParts| |opOf| |pfTupleList| |pfSuchThat2Sex|
            |outputDomainConstructor| |exp2FortOptimizeCS|
            |altSeteltable| |workfiles| |pfLocalItems| |parseHas,fn|
            |pf0TupleParts| |typeTimePrin| |exp2FortOptimizeArray|
            |primeSub| |pfInlineItems| |parseHas,mkand|
            |constructor2ConstructorForm| |displayLines1| |primeSuper|
            |unabbrevAndLoad| |mkParameterList| |primeWidth|
            VMLISP::VARP |pfAddAddon| MACROEXPANDALL |pfRule2Sex|
            |entryWidth| GETREFV |pfWithWithon| |parseHasRhs|
            |pfLambda2Sex| |displayLines| |getBasicMode| |isSharpVar|
            |pfTLambdaBody| NEXT-TAB-LOC |fortran2Lines|
            |abbreviation?| NONBLANKLOC |pfCollect2Sex| GETZEROVEC
            |rootSuper| |displayMacros| |pathname| |pfWIfThen|
            |loadIfNecessary| INDENT-POS |pfSequence2Sex|
            |clearConstructorCache| |isHomogeneous| |postBlock|
            |rootWidth| |changeToNamedInterpreterFrame| |namestring|
            |pfAttribute?| BLANKP |pfApplication2Sex| QSORT
            |containsPolynomial| |postDoubleSharp| |pfHidePart|
            |parseJoin,fn| |displayHashtable| CHARP |fortExpSize|
            PLACEP |killColons| |unwrap| |eq0| |pfWrongWhy|
            |pfWhereExpr| |isWrapped| |displayOperations|
            |pfLoopIterators| |pf0WhereContext| |helpSpad2Cmd|
            |pfStringConstString| |pfIterate?| |copyHack|
            |removeSuperfluousMapping| |widthSC|
            |emptyInterpreterFrame| |pfTransformArg| |pfReturnExpr|
            |addCommas| |postcheck| |pfTLambdaRets| |pfReturn?|
            |fortFormatIntrinsics| |setDefOp| |bottomUpCompile|
            |letWidth| |writify,writifyInner| |pfSymbolVariable?|
            |pfBreakFrom| |upwhere| |numMapArgs| |postElt|
            |spadClosure?| |pfHide?| |pfBreak?| |checkLines| RPACKFILE
            |retractAtree| |pfWIfCond| |functionp| |pfRule?| |slashSub|
            |pfExportDef| |macrop| |pfNovalueExpr| |slashSuper|
            |pfListOf?| |pfNovalue?| HKEYS |slashWidth| |pfNotArg|
            |exp2FortOptimizeCS1,popCsStacks| |pfIterateFrom| |pfNot?|
            RECOMPILE-LIB-FILE-IF-NECESSARY |unwritable?| |minimalise|
            |pf0TypingItems| |pfOrRight| |copyHack,fn| |subSub|
            |minimalise,min| |pfImport| |pfOrLeft| |postQuote|
            |clearParserMacro| |minimalise,HashCheck| |pfDWhereExpr|
            |pfOr?| LIBSTREAM-DIRNAME |suScWidth| |pfAddAddin|
            |pfAndRight| |npElse| |quote2Wrapped| |pfWIf?| |pfAndLeft|
            |pfAnd?| PNAME |superSubSub| |loadSpad2Cmd| |pfWrong?|
            |superSubSuper| |clearCmdExcept| |destructT|
            |pf0LocalItems| |typeOfType| |superSubWidth| |pfLocal?|
            MAKE-BVEC |pf0FreeItems| MKQSADD1 |pfFree?| |vConcatSub|
            |pfRestrictType| UPCASE |SubstWhileDesizingList|
            |removeUndoLines| |verifyRecordFile| |pfRestrictExpr|
            |pfAttribute| |rMkIstream| |SubstWhileDesizing|
            |vConcatWidth| |isIntegerString| |pfRestrict?| |rMkOstream|
            |isInitialMap| |deleteFile| |pfDefinition?| NUM2USTR
            IS_GENVAR |pfAssignRhs| |pfSequence| |binomialSub|
            |displaySpad2Cmd| |pf0AssignLhsItems| |binomialSuper|
            |initCache| |pfAssign?| |pfSequenceArgs| |binomialWidth|
            |formulaFormat| NUMOFNODES |tokTran| |pfTypedType|
            |stringer| SHOWDATABASE |pfTyped?| |pfFix| SUBANQ
            |exptNeedsPren| |zagSub| FBPIP |deMatrix| |zagSuper|
            |incFileInput| |printMap| FOAM:|printNewLine| |zagWidth|
            |devaluateList| DATABASE-P |Skipping?| |CDRwithIncrement|
            |Top?| |incFile| |charyTopWidth| |fileNameStrings|
            |displayOperationsFromLisplib| |inclmsgConActive|
            |eq2AlgExtension| |incRgen| WIDTH |say2PerLine| LASTATOM
            |inclmsgNoSuchFile| |variableNumber| |StreamNull|
            |minusWidth| |undoChanges| |hashCount| |mkNestedElts|
            |setExposeAddConstr| |maPrin| |inclmsgPrematureEOF|
            |iterVarPos| |npNull| MESSAGEPRINT UNEMBED |mathPrintTran|
            |fracsub| MESSAGEPRINT-1 |workfilesSpad2Cmd|
            SET-FILE-GETTER MESSAGEPRINT-2 |interpIter| |fracsuper|
            |sayBrightly| |makeOldAxiomDispatchDomain| |fracwidth|
            |pathnameName| |updateSourceFiles| |devaluate|
            |DNameToSExpr| |falseFun| |pathnameType| |instantiate|
            |agggwidth| |outformWidth| |killNestedInstantiations|
            |maprin| |binomSub| |unInstantiate| LIST2VEC |incIgen|
            |binomSuper| |binomWidth| |mkZipCode| |signatureTran|
            |isNewWorldDomain| |setCurrentLine| |maprinChk|
            |dropPrefix| |altSuperSubSub| |quoteCatOp|
            |mkAndApplyPredicates| |altSuperSubSuper|
            |altSuperSubWidth| |upStreamIters| |coerceSpadFunValue2E|
            |translateTrueFalse2YesNo| |incString| IS-CONSOLE
            |sumWidthA| |isExposedConstructor| |ncloopParse|
            |getPreviousMapSubNames| |boxSub| |moveORsOutside|
            |boxSuper| |hasDefaultPackage|
            |convertOpAlist2compilerInfo| |remWidth|
            |getFirstArgTypeFromMm| |boxWidth| |sumoverlist|
            |StringToCompStr| |concatTrouble,fixUp|
            |getDependentsOfConstructor| |trace1|
            |getFunctorOpsAndAtts| |validateOutputDirectory|
            MAKE-ABSOLUTE-FILENAME |getMapSubNames| |qTWidth|
            |startTimingProcess| |transformOperationAlist| |countCache|
            |SpadInterpretFile| |texFormat| |getCategoryOpsAndAtts|
            |ncloopPrintLines| |compileInteractive| |alqlGetOrigin|
            VMLISP::GET-INDEX-TABLE-FROM-STREAM |isFreeVar|
            |closeOldAxiomFunctor| |pp| |setFortTmpDir| |mkLineList|
            |interactiveModemapForm,fn| |extsub| |nonBlank| |extsuper|
            |lisplibDoRename| |ncloopEscaped|
            |isFreeFunctionFromMmCond| |extwidth| |clearClam|
            |isFreeFunctionFromMm| |isLocalVar| |alqlGetKindString|
            FOAM:|formatDFloat| |translateYesNo2TrueFalse| |numArgs|
            |alqlGetParams| FOAM:|formatSFloat| |phInterpret|
            |sayMessage| |dcData1| FOAM:|formatBInt|
            |intInterpretPform| |patternCheck| |matSuper|
            |makePredicateBitVector| FOAM:|formatSInt| |hashCode?|
            |flattenSignatureList| |matWidth| |template|
            |isSystemDirectory| |setAsharpArgs| |intloopEchoParse|
            |finalizeLisplib| |nothingSuper| |isPatternArgument|
            |phMacro| |nothingSub| |isConstantArgument|
            |asyAncestorList| |initializeSetVariables| |nothingWidth|
            |asyAncestors| |macroExpanded| |mkNiladics| |saveMapSig|
            |asyConstructorArgs| |setInputLibrary| |asyConstructorArg|
            |setOutputLibrary| |asyComma?| |S_process|
            |loadLibIfNotLoaded| |stripSpaces| |asyCattranOp|
            |readLibPathFast| |intnplisp| |shortenForPrinting|
            |updateCategoryFrameForConstructor| |genDomainTraceName|
            |serverReadLine| |getTraceOptions|
            |asCategoryParts,exportsOf| |setExposeAddGroup|
            |displayParserMacro| |asyShorten| |pfPrintSrcLines|
            |createAbbreviation| MONITOR-CHECKPOINT |mapPredTran|
            |getTraceOption| MONITOR-DATA-MONITORP |outputTranIterate|
            |isInternalMapName| |constructor?| |setExposeDropConstr|
            MONITOR-DATA-COUNT |transTraceItem| |asyTypeMakePred|
            |setExposeDropGroup| MONITOR-DATA-SOURCEFILE
            |domainToGenvar| MONITOR-DATA-NAME |args2Tuple|
            |mkAtreeValueOf| INTERPSYS-ECL-IMAGE-INIT |maprinRows|
            |setFortDir| |collectDefTypesAndPreds| |mkMapPred|
            |makeByteWordVec| MONITOR-NRLIB |analyzeMap,f|
            CACHEKEYEDMSG |isRationalNumber| |mathmlFormat|
            |segmentedMsgPreprocess| |hashable| MONITOR-EXPOSEDP
            SPAD-KERNEL-POSIT XDR-STREAM-HANDLE |sayTeX|
            VMLISP::LIBSTREAM-INDEXSTREAM |evalDomain| MONITOR-LIBNAME
            |frameName| |largeMatrixAlist| FOAM:|Halt| |stripUnionTags|
            MONITOR-APROPOS |setHistory| |mkPredList| |makeCharacter|
            |setOutputCharacters| |sayWidth,fn| |constructorCategory|
            |asTupleSize| MONITOR-PARSE |sayMathML| XDR-STREAM-NAME
            MONITOR-SPADFILE |sayDisplayStringWidth| MONITOR-DATA-P
            |dispfortexp| |asyPredTran| |srcPosColumn| XDR-STREAM-P
            |getAttributesFromCATEGORY| VMLISP::PROBE-NAME MONITOR-INCR
            |texFormat1| |center80| /VERSIONCHECK |transformREPEAT|
            |sayLongOperation| |transformCollect| |outputTranIteration|
            |asyUnTuple| |postTranList| |encodeCatform| |sayHtml|
            |removeTracedMapSigs| |postForm| |say2PerLineThatFit|
            |postCapsule| |isRecord| |asTupleNewCode0|
            |makeCompactSigCode| |depthAssoc| |removeAttributes|
            |orderBySlotNumber| |postError| |coerceMap2E|
            |mkAtreeValueOf1| |depthAssocList| |isExistingFile|
            |srcPosFile| |getCatAncestors| |splitSayBrightly|
            |pathname?| |orderBySubsumption| |sayFORMULA| |getFlag|
            |makeCompactDirect1,fn| |halfWordSize| |Record0|
            |getExportCategory| |untraceAllDomainLocalOps|
            |flattenOperationAlist| |opTran| |isFormalArgumentList|
            |spadTrace,g| |hasOptArgs?| |pfSourceText| |patternVarsOf|
            |addToCategoryTable| |nodeSize| |prTraceNames,fn|
            |poGetLineObject| |getCategoryExtensionAlist| |bitsOf|
            |showMsgPos?| |getUnnameIfCan| |lnPlaceOfOrigin|
            |simpHasPred,simpDevaluate| |msgLeader?|
            |mkEvalableMapping| |pfPosOrNopos| FOAM::TYPE2INIT
            |msgImPr?| |mkEvalableUnion| |mkEvalableRecord|
            |simpOrUnion| |getMsgFTTag?| |ppTemplate| |getMsgPosTagOb|
            |quoteNontypeArgs| |bubbleType| |posPointers| |evalType|
            |orderByContainment| |srcPosLine| |dcCats1| |interpOp?|
            |getMsgPos| |failCheck| |walkForm| |expandREPEAT|
            |hasIdent| |makeDomainTemplate| |poCharPosn| PREPARSE1
            |flattenSemi| |isLeaf| |removeAttributePredicates|
            |makeMsgFromLine| PARSEPRINT |postTransform|
            |compressHashTable| |getSrcPos| |resolveTTRed3|
            PREPARSEREADLINE |mkAtreeExpandMacros| |makeCompactDirect|
            |poLinePosn| ATENDOFUNIT |isInterpMacro|
            |decomposeTypeIntoTower| |stripOutNonDollarPreds|
            |resolveTMRed1| |getLineText| PREPARSE-ECHO
            |isHasDollarPred| |getLinePos| |getConstrCat|
            |srcPosDisplay| |NRTcatCompare| |nontrivialCosig|
            |srcPosSource| |typeToOutputForm| |vectorSize|
            |getMsgPrefix?| |npParenthesized| |mkRationalFunction|
            |erMsgSort| |isAVariableType| |domainZero| |numberOfNodes|
            |erMsgSep| PREPARSEREADLINE1 |knownEqualPred|
            |categoryParts,exportsOf| |removeBindingI| |getMsgKey|
            SKIP-IFBLOCK |getCategoryExtensionAlist0| |getMsgTag?|
            |getMsgKey?| |npInfGeneric| |sayMSG| |typeToInputForm|
            |getOpSegment| |whichCat| SKIP-TO-ENDIF
            |typeIsASmallInteger| |processChPosesForOneLine|
            |optIF2COND| INFIXTOK |optSEQ,getRidOfTemps| |optXLAMCond|
            |clearCategoryTable| |pfLocal| |optCONDtail| |domainOne|
            |optPredicateIfTrue| |isHomogeneousList| |predicateBitRef|
            |npListing| |optSEQ,SEQToCOND| |constructSubst|
            |bubbleConstructor| |npDotted| |optSEQ|
            |selectMostGeneralMm|
            |optimizeFunctionDef,removeTopLevelCatch|
            VMLISP::LIBRARY-FILE |optSEQ,tryToRemoveSEQ| |infovec|
            |optimize,opt| |isApproximate| |opt-| |optimizeFunctionDef|
            |optEQ| |optQSMINUS| CSCH |pfFree| |asTupleAsList|
            |optMINUS| |optMkRecord| |containsVars| |optSuchthat|
            |doReplaceSharpCalls| |noSharpCallsHere| |npRestore|
            |hitListOfTarget| |isTaggedUnion| |resolveTypeListAny|
            |npZeroOrMore| |domainDepth| MANEXP |varsInPoly|
            |evalMmStack| ACOT |updateTimedName| |pfBreak| COT
            |printNamedStats| GET-ARGUMENT-DESIGNATOR-TOKEN
            GET-SPADNUM-TOKEN SEC GET-SPADSTRING-TOKEN CSC
            GET-SPECIAL-TOKEN ACSC |timedEvaluate| ASEC
            |splitIntoBlocksOf200| COTH |pfReturnNoName|
            |asyTypeUnitList| |timedEVALFUN| SECH
            MAKE-ADJUSTABLE-STRING |asyConstructorModemap|
            |timedAlgebraEvaluation| ACSCH ACOTH |asIsCategoryForm|
            |pushTimedName| ASECH |asyMkpred| |significantStat|
            |asytran| |roundStat| |pfIterate| SPAD_ERROR_LOC |npTuple|
            |asyLooksLikeCatForm?| LOG10 |asyIsCatForm| |asTupleNew0|
            |displayDatabase| |lispize| |interactiveModemapForm|
            |astran| |str2Tex| FOAM-USER::|AXL-spitSInt| |trimString|
            BVEC-COPY |pr| |displayMacro| |histInputFileName|
            |makeHistFileName| |displayTranModemap| BVEC-NOT
            |getFirstWord| |matrix2String| |newHelpSpad2Cmd|
            |matrix2String,outtranRow| |tuple2String| |readHiFi|
            |blankList| |sayBrightlyLength| |formatModemap|
            MONITOR-EVALBEFORE |sayWidth| |undoLocalModemapHack|
            |postTransformCheck| |formatModemap,fn|
            |satisfiesUserLevel| MONITOR-EVALAFTER |removeIsDomainD|
            |recordFrame| |closeInterpreterFrame| |formCollect2String|
            UNVEC |tuple2String,fn1| TOKEN-TYPE |makeFort,untangle|
            TOKEN-NONBLANK |isInternalFunctionName| REDUCTION-RULE
            |makeUnion| |record2String| |stripNil| UNSQUEEZE
            |makeFort,untangle2| |sayMath|
            |displayProperties,sayFunctionDeps| |isBinaryInfix|
            |postAtom| |expandCOLLECT| |postOp| |formatSignature0|
            |specialChar| LISTOFATOMS DROPENV |formatArgList| SHOWBIND
            |isDefaultPackageName| |linearFormat| FOAM::INSERT-TYPES
            |packageTran| |sayModemap| |zeroOneTran| DATABASE-SPARE
            |vec2Lists1| DATABASE-DEPENDENTS
            |reportOpsFromUnitDirectly| |form2StringList| |pair2list|
            |form2StringWithWhere| |commandsForUserLevel|
            |formatOpConstant| |sayAsManyPerLineAsPossible|
            |dollarPercentTran| |findFrameInRing|
            FOAM::FOAM-FUNCTION-INFO |makeLispList| |concatList|
            |script2String| DATABASE-CONSTRUCTORMODEMAP
            |freeOfSharpVars| |cleanUpSegmentedMsg| |listOfSharpVars|
            |isPatternVar| |listOfPatternIds| |pfDoBody|
            |removeZeroOne| |linearFormatName| |pfDo?|
            |removeZeroOneDestructively| |pfSuchthatCond| |Identity|
            |form2Fence| |pfSuchthat?| |bright| |form2Fence1|
            |pfWhileCond| |checkWarning| |pf0CollectIterators|
            |pfWhile?| VMLISP::GET-DIRECTORY-LIST |pfForinWhole|
            |postBlockItem| |formatUnabbreviated| SQUEEZE
            |pf0ImportItems| |domainForm?| |pf0ForinLhs|
            |formatUnabbreviatedTuple| DATABASE-USERS |pfImportItems|
            |pfForin?| |formatUnabbreviatedSig| DATABASE-PARENTS
            |pfCheckArg| |getConstructorUnabbreviation|
            |pf0LoopIterators| DATABASE-SOURCEFILE |pfCheckId|
            |loopIters2Sex| |ncParseFromString| DATABASE-PREDICATES
            |getConstructorAbbreviation| |pfLoop?| |parse2Outform|
            DATABASE-CONSTRUCTORFORM |pfWithWithin| |isNameOfType|
            |pfExitExpr| |str2Outform| DATABASE-DOCUMENTATION
            |pfTupleListOf| |objMode| |pfExitCond| |wrap|
            DATABASE-OPERATIONALIST |pfComDefinitionDoc| |pfExit?|
            |pfSexpr| |pfFromdomDomain| DATABASE-OBJECT |pfSexpr,strip|
            |pfFromdomWhat| |objValUnwrap| |pfFromdom?|
            |makeLazyOldAxiomDispatchDomain| |pfPretendType|
            DATABASE-NILADIC |pfInline?| |augmentLowerCaseConTable|
            |pfPretendExpr| DATABASE-MODEMAPS |pfSemiColonBody|
            INITIALIZE-PREPARSE |pfPretend?| DATABASE-DEFAULTDOMAIN
            |pfExitNoCond| |compileAsharpArchiveCmd|
            |getPartialConstructorModemapSig| |list2| PRINT-PACKAGE
            |pfCoercetoType| |pfDefinitionLhsItems|
            |isDomainValuedVariable| |bassertNot| PREPARSE
            |pfCoercetoExpr| |separatePiles| DATABASE-COSIG
            |pfAssignLhsItems| |pfCoerceto?| |pf0ExportItems|
            |pfTaggedExpr| DATABASE-CONSTRUCTORKIND |pfExportItems|
            |processSynonymLine,removeKeyFromLine| |packageForm?|
            |r_gamma| BOOT-TOKEN-LOOKAHEAD-TYPE |pfTaggedTag|
            |pileColumn| |npTrapForm| |intProcessSynonyms|
            |getImmediateSuperDomain| |gammaRatapprox| |pfTagged?|
            |rightTrim| |pilePlusComments| DATABASE-ANCESTORS
            |pfWDeclare?| |gammaStirling| |pfIfElse| |leftTrim|
            |postSEGMENT| |pilePlusComment|
            DATABASE-CONSTRUCTORCATEGORY |pfFlattenApp| TOKEN-P
            |pfIfThen| DATABASE-ABBREVIATION |compileSpadLispCmd|
            TOKEN-PRINT |pfIfCond| |simpCatPredicate|
            DATABASE-CONSTRUCTOR |pfWithBase| |pathnameDirectory|
            |pfIf?| |insertpile| |pfExport?| |pfTuple?|
            FOAM::FOAMPROGINFOSTRUCT-P |length1?| |printSynonyms|
            |pfLiteral?| |length2?| |pfDeclPart?| |whatCommands|
            |pfSymbolSymbol| |pfDWhere?| |pfApplicationArg|
            |concatenateStringList| |pfImport?| |pfSequence2Sex0|
            |lastTokPosn| |pfTyping?| |whatSpad2Cmd,fixpat| |cgamma|
            |pfLambdaTran| |pileComment| |pfQualTypeQual| |c_gamma|
            |float2Sex| |firstTokPosn| |compileAsharpLispCmd| |c_to_s|
            |pfLiteralString| |pfSemiColon?| |s_to_c| |pfLeafToken|
            |inclmsgSay| |pfLiteralClass| |pfComDefinition?|
            |pfDefinitionRhs| |pf0WithWithin| |processSynonymLine|
            |pf0DefinitionLhsItems| |postSequence| |pfRetractToType|
            |pfLambdaBody| |dropLeadingBlanks| |pfCheckInfop|
            |compileAsharpCmd| |clngammacase3| |pfLambdaRets|
            |lnString| |pfWhile| |SkipEnd?| |pfPosn| |pfDocumentText|
            |npProcessSynonym| |cgammat| |evalLoopIter| |pfRuleRhs|
            |poImmediate?| |pfAdd?| |npPush| |incCommand?|
            |upLoopIters| |pfRuleLhsItems| |lnGlobalNum|
            |synonymsForUserLevel| |poNoPosition?|
            |pf0FlattenSyntacticTuple| |pfEnSequence| |incClassify|
            OPTIONAL |pfPlaceOfOrigin| ACTION |poPlaceOfOrigin|
            |pfExpr?| |pfListOf| |Else?| PREV-LINE-SET
            |pfSourcePositions| |pfRetractToExpr| |writify| |pfTuple|
            |cgammaAdjust| |isStreamCollect| |lnImmediate?| |pfLoop|
            |dewritify,is?| |npEqKey| |coerceSpadArgs2E|
            |pfSourcePositionlist| |pfDo| |cgammaBernsum| |pileCforest|
            |pfPosImmediate?| |pfLambdaArgs| |getUsersOfConstructor|
            |incNConsoles| |extractCONDClauses| |enPile|
            |lnExtraBlanks| |pfDocument?| |nopilesSpad2Cmd| |compTran|
            |pfSourceToken| |setNopiles| |incRenumber| |compNewnam|
            |pfFirst| |pfSemiColon| |readSpad2Cmd| |intpart|
            |isOkInterpMode| |pfFileName?| |pf0WithBase|
            |parseSystemCmd| |incBiteOff| |NRTevalDomain| COMP-1
            |poFileName?| |pf0TLambdaArgs| |dumbTokenize| |incPos|
            GET-BOOT-TOKEN |pfGetLineObject| |mkDatabasePred|
            |opIsHasCat| |compTran1| |pfUnSequence| |inclmsgConStill|
            LINE-SUBSEQ-FROM PUSHLOCVAR |inclmsgCannotRead| |fracpart|
            |lambdaHelper1| |getOplistForConstructorForm| |c_lngamma|
            |lambdaHelper2| |lnFileName?|
            |getOperationAlistFromLisplib| |npQualified| |Elseif?|
            |lncgamma| |updateCategoryFrameForCategory| |comp_expand|
            |lnLocalNum| |formal2Pattern| GET-A-LINE |compFluidize|
            |poIsPos?| |pfNopos?| |splitIntoOptionBlocks|
            |isDefaultPackageForm?| |compFluidize1|
            VMLISP::LIBSTREAM-MODE |isDomainSubst| |negintp|
            |getDomainByteVector| |loadIfNecessaryAndExists|
            |expandCOLLECTV| |pp2Cols| |npInfKey| |inclmsgPrematureFin|
            |loadFunctor| BADDO |npEncAp| |seq_opt| |npAnyNo|
            |gammaRatkernel| |loadLib| |reportUndo| |flattenCOND|
            |frameEnvironment| |npConditional| |makeLeaderMsg|
            |npItem1| |incFileName| |getMsgPrefix| |npFromdom1|
            |listOutputter| VMLISP::GETINDEXTABLE |If?| |pfIdSymbol|
            STACK-TOP |alreadyOpened?| |DNameFixEnum| |mac0Get|
            |simplifyMapConstructorRefs| |incStringStream|
            STACK-UPDATED |toFile?| |npDDInfKey| |CompStrToString|
            |isLocalPred| |npBracketed| |getMsgToWhere|
            |pfLeafPosition| |dqConcat| |npAngleBared| |pfAbSynOp|
            |mkAliasList| |npBraced| |removeConstruct| |dqToList|
            |npBracked| |SkipPart?| |processKeyedError|
            |pf0MLambdaArgs| |dqUnit| |npParened| |KeepPart?|
            |pfMLambda?| |dqUnitCopy| |getMsgArgL| |npParse|
            |pfTypedId| |toScreen?| |pf0LambdaArgs| COPY |pfDocument|
            |isCategoryPackageName| OPTIONS2UC |pfLambda?| DOWNCASE
            |tokPosn| |line?| |isFunctor| |pfLeaf?| |htTrimAtBackSlash|
            |msgNoRep?| |npEqPeek| |evaluateLines| |leader?|
            |macExpand| |spadThrowBrightly| |pfTyping|
            |recordAndPrintTest,fn| |getStFromMsg| |pfSourcePosition|
            |tabbing| |mac0GetName| |mapCatchName|
            |getUserIdentifiersIn| |myWritable?| |getMsgLitSym|
            |isUncompiledMap| |pfMLambdaBody| |changeExprLength|
            |fnameType| |pfExport| |getPosStL| |isInterpOnlyMap|
            |untraceMapSubNames| |StringToDir| |isDomainOrPackage|
            |getMsgText| |fnameDirectory| |getPreStL| |macApplication|
            |DirToString| |isSubForRedundantMapName|
            |ncloopIncFileName| |macId| |fnameExists?| |To|
            |ncConversationPhase,wrapup| |macMacro| |containsVariables|
            |fnameWritable?| |initToWhere| |macLambda| |fnameReadable?|
            |initImPr| |isFloat| |macWhere| |fnameName| STACK-POP
            |putDatabaseStuff| COND-UCASE |pfApplication?| |mkMat|
            |wrapped2Quote| |getEqualSublis| |pfId?| |msgOutputter|
            |pfMacro?| |objCodeVal| |untraceDomainConstructor|
            |pfWhere?| |getfortexp1| |objCodeMode|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) *) |interpret| |ioHook|
            FOAM:COMPILE-AS-FILE ASHARP)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (VALUES T T)) FOAM:AXIOMXL-GLOBAL-NAME)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) STRING) MAKE-FULL-CVEC)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) *) |spadUntrace| PRINMATHOR0
            |intSayKeyedMsg| |getMapBody| |fortError| MONITOR-PRINT
            |depthOfRecursion| |spleI1| MONITOR-PRINARGS-1
            |updateSymbolTable| |errorSupervisor| |coerceInt2Union|
            |coerceBranch2Union| |sayErrorly| |saturnSayErrorly|
            |coerceIntTableOrFunction| |canCoerceUnion| |newHasTest|
            /UNTRACE-2 |sayKeyedMsg| |newHasTest,fn| |printMap1|
            QUOTIENT |keyedSystemError1| |breakKeyedMsg|
            |saturnKeyedSystemError| |replaceNamedHTPage|
            |popUpNamedHTPage| |ppPair| |npsystem| |getFortranType|
            |asySig| |asySigTarget| |has| |printTypeAndTimeNormal|
            |hasCaty1| |bottomUpType| |putModeSet| |putValue|
            |asyTypeJoinPart| |canCoerce;| |canCoerce1|
            |printTypeAndTime| COMP_QUIETLY_USING_DRIVER |r_besselj|
            |getMinimalVarMode| MAKEOP |evalLET| |bottomUpIdentifier|
            |upLispCall| |rightJustifyString| |isPatMatch|
            |reportOperations| ?ORDER |evalLETput| |ScanOrPairVec|
            |upIFgenValue| VMLISP::COPY-FILE VMLISP::COPY-LIB-DIRECTORY
            |make_compiler_output_stream| TAKE
            |htCommandToInputLine,fn| DROP |sayBrightlyNT2|
            |sayBrightlyNT1| MKPF |tokenSystemCommand| |output|
            |spadcall2| |HasCategory| |HasSignature| $FCOPY
            |checkForFreeVariables| |isDomainConstructorForm|
            |isDomainForm| |ncloopInclude1| |makeStream| SUFFIX
            |keyedSystemError| |evalSlotDomain| |rwriteLispForm|
            |evalAndRwriteLispForm| |set1| |patternCheck,subWild|
            |displaySetOptionInformation| |ncloopCommand|
            |logicalMatch?| |superMatch?| |intloopReadConsole|
            |expandRecursiveBody| COERCE-FAILURE-MSG
            |ncConversationPhase| |asCategoryParts,build|
            |asySignature| |asyCattranConstructors| |putMode|
            |createEnum| CARCDREXPAND |print| |coerceByFunction|
            SPAD-SAVE |mkAtreeWithSrcPos| |getArgValue1|
            |categoryParts,build| ADDCLOSE |coerceInt1|
            |predicateBitIndex,pn| |coerceInt0| |hasCatExpression|
            |makeLongSpaceString| |ncloopInclude| |makeLongTimeString|
            |ncBug| |quickAnd| |ncEltQ| |writeInputLines|
            |outputLispForm| MONITOR-PRINVALUE |reportOpsFromLisplib0|
            |reportOpsFromLisplib| |reportOpsFromLisplib1| |UnionPrint|
            |coerceUn2E| |RecordPrint| |coerceRe2E| |displayProperties|
            |app2StringWrap| BPIUNTRACE |xdrWrite| |readData,xdrRead1|
            |xdrRead| |formArguments2String,fn|
            |unabbrevRecordComponent| |r_besseli|
            |handleTokensizeSystemCommands| |handleNoParseCommands|
            |handleParsedSystemCommands| |inclHandleBug| SPADRREAD
            /TRACELET-PRINT |notCalled| |intCodeGenCOERCE| |spadTrace|
            MKPF1 |findLocalVars1| /TRACE-2)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) T) |desiredMsg| MONITOR-ADD |simpHasPred|
            |getConstructorExports| |defaultTargetFE| PRETTYPRINT
            VMREAD MATCH-CURRENT-TOKEN PRETTYPRIN0 CATCHALL TAB
            |F,PRINT-ONE| BLANKS PRINT-FULL COMPILE-LIB-FILE
            MAKE-INPUT-FILENAME |printRecordFile| |htFile2InputFile|
            |centerAndHighlight| |centerNoHighlight|
            VMLISP::MAKE-FULL-NAMESTRING GET-BOOT-IDENTIFIER-TOKEN
            |LAM,EVALANDFILEACTQ| |pfExpression| |pfSymbol|
            MATCH-NEXT-TOKEN |pfSymb| |inputFile2RecordFile|
            |htFile2RecordFile| MAKE-HASHTABLE MAKE-FILENAME)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) T) |mac0SubstituteOuter| |thisPosIsEqual|
            |fortFormatIfGoto| |interpMap| RPLPAIR |phReportMsgs|
            |putBodyInEnv| |pfDefinition| |beenHere| |isKeyQualityP|
            MONITOR-EVALTRAN |processMsgList| |redundant|
            |pfCopyWithPos| |dispfortexpf| |scanInsert|
            MONITOR-EVALTRAN1 |macWhere,mac| |coerceOrConvertOrRetract|
            |thisPosIsLess| |coerceOrRetract| |erMsgCompare|
            MONITOR-GETVALUE |compareposns| STRING2ID-N
            |listDecideHowMuch| |decideHowMuch|
            |integerAssignment2Fortran1| |mkMapAlias| |setMsgText|
            |assignment2Fortran1| |setMsgUnforcedAttrList| |typeToForm|
            |mkFormalArg| |dispfortarrayexp| |fortFormatTypes1|
            |coerceIntFromUnion| |equalZero| |sayErrorly1|
            |spadTrace,isTraceable| |canCoerceExplicit2Mapping|
            |fortFormatTypes| |coerceRetract| FOAM::ALLOC-PROG-INFO
            |scanCheckRadix| |checkArgs| |lfrinteger| |canCoerceLocal|
            |scanExponent| |canCoerceTower| |absolutelyCannotCoerce|
            |getOption| |getAliasIfTracedMapParameter|
            |coerceIntSpecial| |coerceCommuteTest| |postTranSegment|
            |coerceIntPermute| |computeTTTranspositions|
            |EqualBarGensym| |untraceDomainConstructor,keepTraced?|
            |canConvertByFunction| |optCatch,changeThrowToGo|
            |getBindingPowerOf| |optCatch,hasNoThrows|
            |optCatch,changeThrowToExit| |matSubList1|
            |optimizeFunctionDef,fn|
            |optimizeFunctionDef,replaceThrowByReturn| |mkSuperSub|
            |assocCircular| |predCircular| |recurrenceError|
            |outputMapTran0| |matWList| |genMpFromDmpTerm|
            |setBootAutoLoadProperty| |mkBootAutoLoad|
            |translateMpVars2PVars| |sortAndReorderDmpExponents|
            |opWidth| |traceOptionError| |clearAllSlams,fn|
            |setBootAutloadProperties| |wordFrom| |agg| |getDomainView|
            |removeVectorElt| |posend| |mergeOr| |testExtend|
            |sayPatternMsg| |subMatch| |position1| |hashCombine|
            |formatOpSignature| |hashType| |formatOpSymbol|
            |removeListElt| |formalSubstitute| |NRTreplaceLocalTypes|
            |scanIgnoreLine| |clearCategoryTable1|
            |catPairUnion,addConflict| |substDomainArgs|
            |sayKeyedMsgAsTeX| |updateCategoryTableForDomain|
            |addDmpLikeTermsAsTarget| $FINDFILE |updateCategoryTable|
            $REPLACE |sayModemapWithNumber| UNIONQ |canFit2ndEntry|
            |makeVector| |asyMkSignature| |npsynonym| |throwKeyedMsg1|
            |optionError| |findSubstitutionOrder?,fn| |makeList|
            |saturnThrowKeyedMsg| |sySpecificErrorHere| |wt| |asyWrap|
            |isString?| |sayKeyedMsgLocal| |showInput|
            |argCouldBelongToSubdomain| |push_form1| |syIgnoredFromTo|
            |hasOption| |isPointer?| |mergeSubs| |intersection|
            |asyDisplay| |repetition| |queryUserKeyedMsg|
            |makePrefixForm| |showInOut| |bottomUpPredicate| |wl|
            |parse_rightBindingPowerOf| DEFINE-FUNCTION
            |mergePathnames| |putTarget| |CONTAINEDisDomain|
            |containedRight| |satisfiesRegularExpressions|
            |functionAndJacobian,DF| |push_lform1| |asyMapping|
            |mmCatComp| |defaultTypeForCategory| |parse_Operation|
            STACK-PUSH |computeTypeWithVariablesTarget|
            |asyDocumentation,fn| RDROPITEMS |evalCategory| |msgText|
            |parse_leftBindingPowerOf| |sameUnionBranch|
            |formatOperation| |parse_getSemanticForm| |union|
            |replaceSharps| |printTypeAndTimeSaturn| |inFirstNotSecond|
            |ofCategory| |throwEvalTypeMsg|
            |asyGetAbbrevFromComments,fn| PUSH-REDUCTION
            |processInteractive1| FOAM-USER::H-STRING |recordAndPrint|
            |interpretTopLevel| |deepSubCopy| |substituteSegmentedMsg|
            SETDIFFERENCE |asyTypeUnitDeclare| |resolveTM1| MKPFFLATTEN
            |putValueValue| GETDATABASE |NRTdescendCodeTran|
            FOAM-USER::H-INTEGER FOAM:|fiSetDebugger| POSN1 |pfTree|
            |throwKeyedMsg| |c_besselj| |BesselJ| |pfFromdom| |pfLp|
            |pfPretend| |BesselKAsymptOrder| |pfHide| ERROR-FORMAT
            |pfSpread| PREDECESSOR |BesselJRecur| |BesselJAsymptOrder|
            |rPsiW| LASSOC |pfSuch| |rassoc| QLASSQ |pfTLam|
            |BesselJAsympt| PAIR |BesselIAsymptOrder| |c_hyper0f1|
            |PsiEps| LENGTHENVEC |c_psi| |pfWhere| |cpsireflect|
            |pfIdPos| |pfAssign| |ordSetDiff| |objNew| |pfWDeclare|
            |npTypedForm| |orDel| |ordIntersection| |replaceArgDef|
            |sayIntelligentMessageAboutOpAvailability| |replaceArgDef1|
            |andDnf| |replaceArgDefs1| |getConstantFromDomain;|
            |computeTargetMode| |findRetractMms;| |dnfContains|
            |findRetractMms1| PRINT-AND-EVAL-DEFUN |replaceArgDefs|
            |resolveTT;| EVAL-DEFUN |pfPushMacroBody| |ordUnion|
            |getArgValue| |pfMacro| |coafAndDnf| |canCoerceFrom;|
            |leftBindingPowerOf| |reportCircularCacheStats| |orDnf|
            |isEqualOrSubDomain| |rightBindingPowerOf|
            |hasCorrectTarget| DIVIDE |pvarPredTran|
            |mkCircularCountAlist| |coafAndCoaf| GETL |dnfContains,fn|
            |pfExit| |coafOrDnf| |getAtree| |canCoerceFrom0| FLAGP
            |getConstantFromDomain1| |applyWithOutputToString|
            |remHashEntriesWith0Count,fn| |expression2Fortran1|
            |resolveTT1| VMLISP::WRITE-INDEXTABLE |pfComDefinition|
            |symEqual| |resolveTM| |listTruncate| |transferPropsToNode|
            |parseTranCheckForRecord| |keyedMsgCompFailure|
            |bottomUpCompilePredicate| |pfParen| |SymMemQ|
            |getMinimalVariableTower| |dispfortexpj| |getBasicMode0|
            |fillerSpaces| |evalLETchangeValue| |pushDownOp?|
            |pfBracket| |isPatternMatch| |postFlattenLeft| |segment2|
            |addToSlam| INTERSECTIONQ |segment1| |globalHashtableStats|
            SETANDFILE |domainEqualList| |pfWDec| |getStatement|
            |commandError| MAKEARR1 |resolveTMRed|
            |fortranifyIntrinsicFunctionName| GETREFV32 |pfRetractTo|
            |term1RWall| GLESSEQP |term1RW| LEXLESSEQP |pfQualType|
            |exp2FortOptimizeCS1,pushCsStacks| |resolveTMEq|
            |resolveTMRecord| |pfReturnTyped| |resolveTMUnion| |pfLam|
            |resolveTMTaggedUnion| |spliceTypeListForEmptyMode|
            |pfIfThenOnly| |ScanOrPairVec,ScanOrInner| |pfBrace|
            |resolveTTAny| |constructTowerT| FLAG |testInput2Output|
            |replaceSymbols| S* FOAM:|printDFloat|
            |modemapsHavingTarget| FOAM:|printSFloat|
            |countCircularAlist| FOAM:|printBInt| TRUNCLIST-1
            FOAM:|printSInt| FOAM:|printString| SET-LIB-FILE-GETTER
            SUBB FOAM:|printChar| |CONTAINED,EQ| DELDATABASE
            |deleteAll| NCONC2 |CONTAINED,EQUAL| |declare|
            GETCONSTRUCTOR DELASC |declareMap| |lassocShift|
            |upfreeWithType| |scylla| DELLASOS |delete|
            |uplocalWithType| WRAPDOMARGS |sayBrightly2| |sayBrightly1|
            |installConstructor| |prnd| |Delay| |makePathname|
            TRUNCLIST ADDOPERATIONS SUBLISNQ ASHARPMKAUTOLOADFUNCTION
            |incAppend| |spadPrint| CONTAINED |next| S+ |mathPrint1|
            REMAINDER |matWList1| PRINT-DEFUN
            |lazyOldAxiomDomainDevaluate| |loadLibIfNecessary|
            |newHasCategory| |isNestedInstantiation|
            |convertOpAlist2compilerInfo,formatSig| |everyNth|
            |oldAxiomPreCategoryParents| |streamChop|
            |getAllModemapsFromDatabase| |ncloopPrefix?|
            |getSystemModemaps| |matSuperList1|
            |displaySetVariableSettings|
            |oldAxiomCategoryDefaultPackage| |ncloopDQlines|
            |putDependencies,removeObsoleteDependencies| |hashTypeForm|
            |systemDependentMkAutoload| |pfAbSynOp?| REMOVER
            |mkAutoLoad| |augmentTraceNames|
            |lazyOldAxiomDomainHashCode| |unloadOneConstructor|
            |oldAxiomCategoryDevaluate| |ncINTERPFILE| |writeLib|
            |SExprToDName| |isDomainSubst,findSub| |mkFreeVar|
            FOAM:|fputs| |sayCacheCount| FOAM:|fputc| |prefix?|
            GGREATERP |nonRecursivePart| |autoLoad|
            |intloopProcessString| CGREATERP |modemapPattern|
            |stringMatches?| |oldAxiomCategoryParentCount| |suffix?|
            /READ |basicMatch?| |saveDependentMapInfo| |domainEqual|
            SORTBY |mkAlistOfExplicitCategoryOps,fn| |sort| QUOTIENT2
            INTEXQUO |phIntReportMsgs| |makePattern| |buildBitTable,fn|
            |intloopPrefix?| |patternCheck,wild| |displayRule| DIVIDE2
            |getEqualSublis,fn| |patternCheck,pos| |nrtEval| |makeSF|
            |removeOption| |getConstructorOpsAndAtts| |match?|
            |hasPair| |phParse| |nonRecursivePart1| |BooleanEquality|
            |getMapSig| |readLib| |maskMatch?| |sayDroppingFunctions|
            FOAM::|magicEq1| |evalSharpOne| |mkValCheck| |makeRuleForm|
            |funfind| |mkValueCheck| |addPatternPred| |canMakeTuple|
            SEGMENT |mapDefsWithCorrectArgCount| |mkAtree3fn|
            |asyExportAlist,fn| MONITOR-WRITE |mkLessOrEqual|
            |makeByteWordVec2| |outputFormat| |asyAbbreviation|
            |asySplit| |asySimpPred| |dcOpLatchPrint|
            |fastSearchCurrentEnv| |incRenumberLine| |center|
            |asyAbbreviation,chk| |EnumPrint| FOAM:|PtrMagicEQ|
            |incRenumberItem| |asyCatSignature| |coerceVal2E|
            |splitListOn| |objNewWrap| |getI| |reportSpadTrace|
            |MappingPrint| |putFlag| |resolveTTEq| |throwPatternMsg|
            |rempropI| |say2PerLineWidth| |subTypes|
            |acceptableTypesToResolve1| |lassocSub| |resolveTTCC|
            |asTupleNewCode| |makeCompactDirect1| |resolveTTRed|
            |objSetMode| |transferSrcPosInfo| |makeGoGetSlot|
            |untraceDomainLocalOps| |patternVarsOf1| |resolveTTSpecial|
            |resolveTCat1| |mkCategoryOr| |compareTT| |simpCategoryOr|
            |addPred| |augmentPredCode| |acceptableTypesToResolve|
            |getMsgCatAttr| |objSetVal| |mungeAddGensyms|
            |resolveTTUnion| |getArgValueOrThrow| |simpOrUnion1|
            |tempExtendsCat| |resolveTMEq1| |insertPos|
            |resolveTMSpecial| |resolveTMOrCroak| |rep|
            |processInteractive| |encodeCategoryAlist| |objNewCode|
            |getConditionsForCategoryOnType| |equalOne| PARSEPILES
            |addDomainToTable| |resolveTM2| ASSOCIATER |asTupleNew|
            |isSubTowerOf| ESCAPED |mkAtreeNodeWithSrcPos|
            |npLeftAssoc| |resolveTCat| |retractByFunction|
            |pfApplication| INITIAL-SUBSTRING STOREBLANKS
            ADD-PARENS-AND-SEMIS-TO-LINE |isTowerWithSubdomain|
            |dcOpPrint| |npRightAssoc| |hasCat| |canCoerceCommute|
            |replaceLast| |matchMmSigTar| |constructM|
            |augmentPredVector| |coerceIntTest| |varIsOnlyVarInPoly|
            |subCopy| |canCoercePermute| |newCanCoerceCommute|
            |getOpArgTypes| |getOpArgTypes,f| |clearDependentMaps|
            |getOpArgTypes1| |pfRule| |coerceIntAlgebraicConstant|
            |coerceIntTower| |compareTypeLists| |EqualBarGensym,fn|
            |coerceIntByMapInner| |valueArgsEqual?| |constructT|
            |coerceIntByMap| |coerceIntCommute| |pfTyped|
            |printNamedStatsByProperty| |constantInDomain?| |mkObjWrap|
            GET-GLIPH-TOKEN REMOVE-ESCAPES |pfReturn| TRANSLABEL1
            |AlistAssocQ| |ListMemberQ?| |ListMember?| |AlistRemoveQ|
            |displayDatabase,fn| |initializeTimedNames|
            |intloopInclude| |asyCattranSig| BVEC-MAKE-FULL |position|
            |insert| BVEC-CONCAT BVEC-EQUAL |quickOr| BVEC-GREATER
            BVEC-AND BVEC-OR BVEC-XOR BVEC-NAND BVEC-NOR TRANSLABEL
            |undoSingleStep| FOAM-USER::H-ERROR |undoSteps| S-
            |substInOrder| |plural| |commandUserLevelError| |setSf|
            |formDecl2String| /GETOPTION |formJoin1| DAASENAME
            |writeData| |formArguments2String| |xdrOpen| |readData|
            |getAndSay| MAKE-DATABASES |linearFormatForm|
            |insertWOC,fn| |coerceInteractive| |concat1|
            |filterListOfStrings| |formatJoinKey| |displayOpModemaps|
            |app2StringConcat0| |pfCoerceto| |postFlatten|
            |sublisNQ,fn| |unabbrev1| |pfBracketBar| |mkObj|
            |coerceInt| |unabbrevUnionComponent| |search|
            |searchCurrentEnv| |getProplist| |getCDTEntry| |pileTree|
            |condAbbrev| |commandErrorIfAmbiguous| |FloatError|
            |pileCtree| |pfDWhere| |pileForest| |pfOr| |cPsi|
            |PsiXotic| |pairList| |pileForest1| |r_psi| |eqpileTree|
            |PsiAsymptotic| GETALIST REMALIST |pfMLambda|
            |deleteAssocWOC| |insertWOC| |pfAnd| |c_besseli|
            |push_reduction| |inclmsgFileCycle| |BesselI|
            |stringPrefix?| |subCopyOrNil| |horner| |positionInVec|
            |stringChar2Integer| |subCopy0| |cbeta| |mkIterVarSub|
            |match_current_token| |sublisNQ| |pfRestrict|
            |lnSetGlobalNum| |chebstareval| |pfBraceBar|
            |npMissingMate| |chebf01| |incTrunc| |ifCond|
            |incCommandTail| |optionUserLevelError| |insertModemap|
            |incStream| |pfTagged| |cgammaG| |getModemapsFromDatabase|
            |besselIback| |incActive?| |allLASSOCs| |sayLooking1|
            VMLISP::PUTINDEXTABLE |getLisplibNoCache| |genIFvalCode|
            |f01| |testBitVector| |brutef01| |member| CHAR-EQ
            |isDomainSubst,fn| |dollarTran| |getLisplib| |repeat_tran|
            |deepSubCopyOrNil| |deepSubCopy0| |getValueFromEnvironment|
            |diffAlist| |mkAtree1WithSrcPos| |inclHandleSay| |chebeval|
            |setMsgPrefix| |oldAxiomCategoryHashCode| |seteltable|
            |inclHandleWarning| |pfFromDom| |inclFname| |putFTText|
            |inclHandleError| |oldAxiomPreCategoryDevaluate|
            |macSubstituteId| |queueUpErrors| |compileIs|
            |npTypedForm1| |macLambdaParameterHandling| |dqAddAppend|
            |incDrop| |makeLocalModemap| |oldAxiomDomainDevaluate|
            |putPvarModes| |dqAppend| |mkAliasList,fn| |assoc|
            |pfForin| |after| |compileBody| |sameMsg?|
            |oldAxiomDomainHashCode| |oldAxiomPreCategoryHashCode|
            EFFACE |assertCond| |pfWrong| |addDefMap|
            |setMsgCatlessAttr| |clearDependencies|
            |traceDomainConstructor| |putDependencies| EMBED
            /GETTRACEOPTIONS |makeNewDependencies| |FromTo|
            |findLocalVars| |tracelet| |mkLocalVar| |breaklet|
            |hyperize| |macLambda,mac| |mkObjCode| |testPrin|
            |pfCollect| |pfMapParts| |containsOp| |canCoerceByMap|
            |canCoerceByFunction| |rassocSub| |deleteLassoc|
            |getLocalVars| |intloopInclude1| |deleteAssoc|
            |isSubDomain| |simplifyMapPattern| LEXGREATERP
            |deleteAssocWOC,fn| |absolutelyCanCoerceByCheating|
            |fortranifyFunctionName|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL FIXNUM) HEAPELAPSED CURRENT-CHAR-INDEX)) 
(PROCLAIM '(FTYPE (FUNCTION NIL (VALUES T T)) GENVAR)) 
)
