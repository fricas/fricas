#+:GCL
(progn
  (eval-when (:execute :compile-toplevel :load-toplevel)
                 (proclaim '(optimize (safety 1) (debug 3))))

(IN-PACKAGE "BOOT") 
(PROCLAIM '(FTYPE (FUNCTION NIL (*)) FIRST-ERROR)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FIXNUM) |nothingSuper| |nothingSub|
            |nothingWidth| FOAM:|ProgHashCode| FOAM:|strLength|
            |hitListOfTarget| CHAR2NUM LINE-NUMBER LINE-LAST-INDEX
            LINE-CURRENT-INDEX |eq0| |widthSC|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FUNCTION) FOAM::FOAMPROGINFOSTRUCT-FUNCALL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FOAM:|SInt|)
            FOAM::FOAMPROGINFOSTRUCT-HASHVAL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) (VALUES T T)) |mkSharpVar| |mkCacheName|
            MONITOR-INFO |mkAuxiliaryName|
            |makeInternalMapMinivectorName| |queryUser| |fetchKeyedMsg|
            READLINE FILE-GETTER-NAME |getKeyedMsg| |mapCatchName|
            |makeCharacter| FOAM:AXIOMXL-FILE-INIT-NAME)) 
(PROCLAIM
    '(FTYPE (FUNCTION ((VECTOR T) (VECTOR T)) T) VMLISP::VGREATERP
            VMLISP::LEXVGREATERP)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) (VALUES T T)) |read-line|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) FIXNUM) FOAM:|SetProgHashCode| QSQUOTIENT
            QSREMAINDER QENUM)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) *) |argsapp| |appmat| |overlabelApp|
            |constructorAbbreviationErrorCheck|
            |newExpandLocalTypeArgs| FOAM:|fputss| FOAM:|fgetss|
            |appagg| |appparu| |charyTrouble| |charyTrouble1| |exptApp|
            |inApp| |quoteApp| |appHorizLine| |appargs| |overbarApp|
            |appparu1| |appsc| |appsetq| |slashApp| |appsub|
            |binomialApp| |makeStatString| APP |appfrac|
            |patternCheck,mknew|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) *) |templateVal| |augModemapsFromDomain1|
            |parseIf,ifTran| |getVal| |ncloopInclude0|
            |asytranFormSpecial| |asytranApplySpecial| |selectOption|
            |selectOptionLC| |Qf2F| |asytranForm| GETOP |charyTop|
            |htMkPath| |exp2FortSpecial| |charybdis| BUILD-DEPSYS)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) *) |apprpar1| LOCALNRLIB
            |compileAndLink| |apprpar| |applpar| |applpar1| |appagg1|
            |apphor| |appvertline| |appargs1| |matrixBorder|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) *) |makeLongStatStringByProperty|
            |addModemap| INTERPSYS-IMAGE-INIT)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) *) LOCALDATABASE |ncBug| CONCAT)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T) *) |lisplibError|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T T T T) *) |makeFortranFun|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) T) |anySubstring?|
            |intloopSpadProcess,interp| |rightCharPosition|
            |lisplibWrite| |infix?| |incPrefix?| AS-INSERT
            |traceDomainLocalOps| AS-INSERT1 |mapRecurDepth|
            |interpret1| |addMap| |getCatForm| |newExpandLocalType|
            |displayType| |writeLib1| |makeCompilation|
            |makeAspGenerators| |makeAspGenerators1|
            |cleanUpAfterNagman| |restoreDependentMapInfo|
            |analyzeNonRecur| |rewriteMap0| |reportOpSymbol,sayMms| DEF
            |getFunctionFromDomain| THETACHECK
            |prepareResults,defaultValue| |compiledLookup| |mkAtree2|
            |remprop| |postCollect,finish| |recordOldValue|
            |recordNewValue| ELEMN |addIntSymTabBinding| |mkAtree3|
            |putAtree| |isEltable| |intloopInclude0| |compSPADSLAM|
            SUBSTEQ |charPosition| |interpRewriteRule| |writeXDR|
            |insertShortAlist| |incZip| VMLISP::MAKE-ENTRY
            |getValueFromSpecificEnvironment| SUBLISLIS |substVars|
            |makeResultRecord| |nextown| |rwrite| EQSUBSTLIST
            |replaceVars| |transferPropsToNode,transfer| |rread|
            |writeStringLengths| |putI| ADDASSOC |readLib1|
            |throwListOfKeyedMsgs| |buildPredVector| |orderPredTran|
            |pileForests| |augLisplibModemapsFromCategory|
            |unifyStructVar| |orderPredicateItems| |unifyStruct|
            |nopile| |updateDatabase| |matchTypes| |throwKeyedErrorMsg|
            |mkUserConstructorAbbreviation| |countParens|
            |isOpInDomain| |getLocalMms| |getLocalMms,f|
            |unabbrevSpecialForms| |getOplistWithUniqueSignatures|
            |pfInfApplication| |insertAlist| |get| |domArg2|
            |augmentLisplibModemapsFromFunctor| |hasAttSig|
            |findUniqueOpInDomain| SPADRWRITE SPADRWRITE0 |evalMmCat1|
            |hasSigOr| |clngamma| |clngammacase23|
            |commandErrorMessage| |addBindingInteractive|
            |clngammacase1| |displayModemap,g| |userLevelErrorMessage|
            |PsiAsymptoticOrder| |get1| |cotdiffeval| |get2| |get0|
            |PiMinusLogSinPi| |chebstarevalarr| |BesselasymptA|
            |pfPushBody| |logH| |chebf01coefmake| |besselIcheb|
            |constrArg| |chebevalarr| |hasCate| |npParenthesize|
            |hasCaty| |augmentSub| |findCommonSigInDomain|
            |selectMmsGen,exact?| |filterModemapsFromPackages| PROPERTY
            |coerceTypeArgs| |hasSigAnd| |evalMmCond| |evalMmCond0|
            |NDmp2domain| |Dmp2NDmp| |fortFormatHead| |Dmp2Dmp| I2PI
            CARCDRX1 |Up2Expr| |Qf2PF| |Scr2Scr| |Dmp2Up| |Complex2FR|
            |nAssocQ| |P2Dmp| |Rm2Sm| DP2DP |Un2E| |P2Up| |Qf2EF|
            |getfortarrayexp| |Qf2Qf| |Sy2OV| |Sy2Up| |PsiBack|
            |fortFormatLabelledIfGoto| |Up2FR| V2M |upTableSetelt|
            |Up2P| |maprinSpecial| |Var2OtherPS| MKPFFLATTEN-1
            |argumentDataError| SMALL-ENOUGH-COUNT
            |NRTisRecurrenceRelation| |assignSymbol| |asySig1|
            |upNullTuple| |simpHasPred,simpHas| |getMappingArgValue|
            |BesselIAsympt| |moreGeneralCategoryPredicate|
            |encodeUnion| |evalFormMkValue| |asytranApply| |rewriteMap|
            |evalREPEAT| |evalSEQ| |makeCatPred| |sigDomainVal|
            |sideEffectedArg?| |asytranForm1| /MONITORX |altTypeOf|
            |getArgValueComp| |upwhereMain| |upwhereMkAtree|
            |upwhereClause| |Expr2Complex| |Sm2PolyType| |domainVal|
            |Complex2Expr| |lazyDomainSet| V2L |L2Rm| |Up2SUP| |V2Rm|
            |hput| |Dmp2P| |oldAxiomAddChild| |Up2Dmp| |getOpCode|
            |coerceTraceArgs2E| |insertAlist,fn| |P2Uls|
            |expandTypeArgs| |expandType| |SUP2Up| |Sy2Var| I2OI
            |filterAndFormatConstructors| |coerceTraceFunValue2E|
            |newExpandGoGetTypeSlot| OV2OV |lookupInDomainByName|
            |L2Tuple| |filterListOfStringsWithFn| |errorSupervisor1|
            |newExpandTypeSlot| MSUBST |Sm2V| |mac0Define| |Mp2Dmp|
            |Sy2Dmp| |lazyOldAxiomAddChild| |coerceDmp2|
            |Factored2Factored| |coerceFFE| PRINT-XDR-STREAM |Mp2Up|
            |setMsgForcedAttr| |oldAxiomCategoryBuild| |P2Mp|
            |oldAxiomPreCategoryBuild| |inclmsgIfSyntax| |Mp2Expr|
            |Dmp2Expr| |P2Expr| |NDmp2NDmp| |Agg2Agg| |Agg2L2Agg|
            |Var2QF| |NRTextendsCategory1| |oldAxiomCategoryNthParent|
            |L2Record| |extendsCategory| |newExpandLocalTypeForm|
            |mapLetPrint| |letPrint| |Sm2M| |coerceDmpCoeffs| RPLNODE
            |letPrint2| |Mp2FR| |Sy2NDmp| |Ker2Expr| |evalis|
            |displayMode| OV2P |setMsgUnforcedAttr| L2V
            |displayCondition| |P2Upxs| |Var2Gdmp| |upLETWithFormOnLhs|
            |Var2NDmp| |upSetelt| |Dmp2Mp| |mac0InfiniteExpansion|
            |Set2L| |evalQUOTE| |Rm2M| |substringMatch|
            |throwKeyedMsgSP| |domain2NDmp| I2EI |displayValue|
            |Var2OV| |upLETtype| V2DP |mungeAddGensyms,fn|
            |intloopProcess| |evalIsntPredicate| |evalIsPredicate|
            |stuffSlot| |IFcodeTran| |dcSig| L2DP |L2Sm|
            |sySpecificErrorAtToken| |evalIF| OV2SE |Sm2Rm|
            MONITOR-PRINARGS |substring?| |Var2Dmp| |stringPosition|
            HREMPROP |P2Uts| |Expr2Mp| |Expr2Dmp| |OV2Sy|
            |extendsCategoryBasic| I2NNI |buildPredVector,fn|
            |Qf2domain| |NRTgetLookupFunction| |L2Set| |Sy2P| |Up2Up|
            |Expr2Up| |Ker2Ker| |lassocShiftWithFunction| |Up2Mp|
            |Var2P| |Var2UpS| |Var2Up| |restoreHistory2| |V2Sm|
            |upTaggedUnionConstruct| |isRectangularVector|
            |upRecordConstruct| |upNullList| |assocCache| |Mp2Mp|
            |assocCacheShift| |Mp2P| |assocCacheShiftCount|
            |evalCOERCE| |Var2FS| |M2Sm| P2FR |Var2Mp| |Sm2L|
            |subVecNodes| M2L |Rm2L| |application2String|
            |interpCOLLECTbody| |Rm2V| M2V |isLegitimateMode;|
            |upLoopIterIN| |mkIterFun| |flowSegmentedMsg|
            |upStreamIterIN| |hasFileProperty;| |spad2BootCoerce|
            |throwKeyedMsgCannotCoerceWithValue|
            |coerceConvertMmSelection;| |lffloat| |interpCOLLECT|
            |evalCOLLECT| |computeTTTranspositions,compress|
            |npListofFun| |NRTcompiledLookup| RWRITE
            |asyMakeOperationAlist| |hasFilePropertyNoCache|
            |compiledLookupCheck| |coerceOrCroak|
            |sayFunctionSelectionResult| |centerString|
            |algCoerceInteractive| ADDOPTIONS |extendsCategoryBasic0|
            |substSlotNumbers| |npAndOr| |loadLibNoUpdate|
            |augProplist| |Complex2underDomain| |resolveTTEq2|
            |resolveTTEq1| |makeEijSquareMatrix| |M2Rm| M2M
            |resolveTTRed2| |Var2SUP| |resolveTTRed1| L2M |OV2poly|
            |Rn2F| |Sy2Mp| |getConditionalCategoryOfType|
            |resolveTMEq2| PUTALIST |matchUpToPatternVars|
            |coerceOrFail| |augProplistInteractive| QESET
            |asyCattranOp1| |algEqual| |recordNewValue0|
            |coerceOrThrowFailure| SETDATABASE |asGetExports|
            |permuteToOrder| |coerceIntX| |coerceSubDomain|
            |catExtendsCat?| |NRTcompileEvalForm| |optSpecialCall|
            |longext| |canCoerceByFunction1| |getSubDomainPredicate|
            B-MDEF HPUT |interpret2| |fortCall| |mkRecordFunList|
            |ncPutQ| |pfLambda| |mkUnionFunList| |mkMappingFunList|
            |pfIf| |sublisMatAlist| |mkEnumerationFunList| |fnameNew|
            |fnameMake| |retractUnderDomain| |pfTLambda| |outputNumber|
            |npBackTrack| DEF-INNER |pfWIf| |isRectangularList|
            |pfWith| |splitConcat| |selectMms| SUBSTRING |mkFortFn|
            |PARSE-getSemanticForm| |insertEntry| |pushDownTargetInfo|
            |npList| |outputString| |pushDownOnArithmeticVariables|
            |addBinding| |exp2Fort2| |exp2FortFn| |infixArgNeedsParens|
            |keyedMsgCompFailureSP| PUT |intCodeGenCoerce1| |nsubst|
            |LargeMatrixp| |mkInterpFun| |deleteMap| |analyzeMap0|
            |MappingEqual| FOAM:|FormatNumber| |EnumEqual|
            |displaySingleRule| |compileCoerceMap| |compileDeclaredMap|
            |mkNewUnionFunList| |recordOldValue0| |UnionEqual|
            |displayMap| |SpadInterpretStream| |findLocalsInLoop|
            |RecordEqual| |rewriteMap1| |matchSegment?|
            |recordInstantiation| |stringMatch| |patternCheck,equal|
            |displayModemap| |recordInstantiation1| |needBlankForRoot|
            |commandAmbiguityError| |position,posn| |getOpBindingPower|
            |addToConstructorCache|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) T) |mkCacheVec| |analyzeRecursiveMap|
            |augmentMap| |bottomUpFormRetract| |bottomUpForm|
            |bottomUpFormAnyUnionRetract| |putSrcPos| LOCALASY
            |compileRecurrenceRelation| |makeConstrArg|
            |selectMmsGen,matchMms| |commuteQuaternion|
            |commuteNewDistributedMultivariatePolynomial| |compileIF|
            |logS| |getNewDefaultPackage| |getArgValueComp2|
            |newLookupInDomain| |newCompareSig| |lazyMatchArg2|
            |interpLoopIter| |commuteFraction| |newLookupInTable|
            |hashNewLookupInTable| |commuteComplex| |coerceDmp1|
            |commuteDistributedMultivariatePolynomial| |xlConActive|
            |xlNoSuchFile| |printLabelledList| |commuteMPolyCat|
            |commutePolynomial| |compileADEFBody|
            |commuteSparseUnivariatePolynomial|
            |commuteUnivariatePolynomial|
            |commuteMultivariatePolynomial| |selectMms1;| |selectMms2|
            |sayFunctionSelection| |orderMms| |resolveTT2|
            |commuteSquareMatrix| |charyBinary| |prepareResults|
            |split2| |coerceByTable| |spadify| |concatTrouble|
            |concatApp1| |bottomUpDefaultCompile| |bottomUpDefaultEval|
            |bottomUpForm2| |bottomUpForm3| |bottomUpForm0|
            |bottomUpFormTuple| |aggregateApp|
            |bottomUpFormUntaggedUnionRetract| |fortFormatDo|
            |analyzeDeclaredMap| |xlFileCycle| |xlMsg| |xlSay| |xlOK1|
            |incLine| |reportFunctionCompilation| |incLude| |needStar|
            |xlCannotRead| |xlConStill| FINCOMBLOCK)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) T) |xlCmdBug| |compClam|
            |xlPrematureFin| |binomApp| |xlSkip| |basicLookup|
            |altSuperSubApp| |lookupDisplay| |boxApp| |oldCompLookup|
            |oldCompLookupNoDefaults| |lazyMatchAssocV| |lazyMatch|
            |centerApp| |getFileProperty| |appext|
            |makeInternalMapName| |protectedNagCall| |nothingApp|
            |analyzeNonRecursiveMap| |putFileProperty| |termMatch|
            |mergeInPlace| |bottomUpDefault| |printCName| |writeMalloc|
            |printDec| |fixUpPredicate| |srcPosNew| |nextown2|
            |evalMmCat| ASHARPMKAUTOLOADFUNCTOR |evalMm|
            ASHARPMKAUTOLOADCATEGORY |hasCateSpecial|
            |reportFunctionCacheAll| |hasCate1| |condUnabbrev|
            |analyzeMap| |matchMmSig| |domArg| |hasSig| |put|
            |mkDomPvar| |putIntSymTab| |BesselasymptB| |selectMmsGen|
            |hasCateSpecialNew| -REDUCE |evalMmFreeFunction|
            |allOrMatchingMms| |defaultTarget| |selectDollarMms|
            |evalTuple| |lazyCompareSigEqual| |clngammacase2|
            |interpIF| |lookupInTable| |getReduceFunction|
            |NRTgetMinivectorIndex| |simpHasSignature| |lazyMatchArg|
            |interpREPEAT| |catPairUnion| |getArgValue2|
            |newLookupInCategories| |nrunNumArgCheck|
            |lazyMatchArgDollarCheck| |lookupIncomplete|
            |hashNewLookupInCategories| |newLookupInCategories1|
            |sayLooking| |mac0MLambdaApply| |compareSig|
            |collectStream1| |say2Split| |newLookupInAddChain|
            |interpCOLLECTbodyIter| |evalUntargetedADEF|
            |evalTargetedADEF| |lookupComplete| |compareSigEqual|
            |xlIfBug| |lookupInDomainVector| |basicLookupCheckDefaults|
            |xlSkippingFin| |mac0ExpandBody| |xlConsole| |letPrint3|
            |xlPrematureEOF| |P2Us| |intloopSpadProcess|
            |matchAnySegment?| STRPOSL |mkInterpTargetedADEF|
            |compileTargetedADEF| |evalconstruct|
            |evalInfiniteTupleConstruct| |evalTupleConstruct| |MpP2P|
            |mkIterZippedFun| |mkAndApplyZippedPredicates|
            |upLoopIterSTEP| |interpLoop| |collectSeveralStreams|
            |collectOneStream| |upStreamIterSTEP|
            |canCoerceTopMatching| |collectStream| |catchCoerceFailure|
            |constoken| |semchkProplist| |augProplistOf|
            |asGetModemaps| |asytranCategoryItem| |asytranDeclaration|
            |asytranCategory| |getConditionalCategoryOfType1|
            |indefIntegralApp| |putHist| |piApp| |pi2App| |bracketApp|
            |braceApp| |charySemiColon| |optCallSpecially|
            |coerceImmediateSubDomain| |appelse| |prepareData|
            |appInfix| |charyEquatnum| |charySplit| |typeToForm,fn|
            |charyMinus| |appsum| |boxLApp| |axiomType| |appconc|
            |plusApp| |charyElse| |timesApp| |mergeSort| |evalForm|
            |selectLocalMms| |stepApp| |appSum| |aggApp| |bigopWidth|
            |concatbApp| |concatApp| |stringApp| |npEnclosed|
            |sigmaApp| |sigma2App| |intApp| |xLate| |appChar| |rootApp|
            |clearDep1| |analyzeUndeclaredMap| |superSubApp|
            |vconcatapp| |zagApp| STRPOS |haddProp| |appneg| MAKE-FLOAT
            |xlOK|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T) T) |bigopAppAux|
            |oldAxiomDomainLookupExport| |abbreviationError|
            |invokeNagman| |findFunctionInCategory| |mkDiffAssoc|
            |lazyOldAxiomDomainLookupExport| |Mp2SimilarDmp|
            |Mp2MpAux1| |Expr2Dmp1| |Mp2MpAux0| |findFunctionInDomain|
            |appInfixArg|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) T) |compHash| |compDefineLisplib|
            |genMapCode| |putMapCode| |nagCall| |mmCost| |mmCost0|
            |findFunctionInDomain1| |BesselIBackRecur| |upDollarTuple|
            |xlIfSyntax| |oldAxiomCategoryLookupExport| |makeFort|
            |invokeFortran| |bracketagglist| |incLine1|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T *) T) RPLACSTR)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T) T) |makeFort1| |P2MpAux|
            BUILD-INTERPSYS)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T *) T) |asCategoryParts| |lnCreate|
            |ncHardError| |ncSoftError| TOKEN-INSTALL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) T) BPITRACE |pfAdd| MATCH-TOKEN
            |categoryParts| |remove| |pfLeaf| RREAD |tokConstruct|
            |listSort| |wasIs| NREMOVE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T) T) |writeCFile| /MONITOR
            |Mp2MpAux2|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T *) T) |msgCreate|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T T) T) |makeSpadFun|
            |P2DmpAux|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL *) |sendNagmanErrorSignal| /RQ /RF
            |npMDEFinition| |npCategory| |testPage| |displayFrameNames|
            |clearCmdAll| |quitSpad2Cmd| |leaveScratchpad|
            |disableHist| |describeSetStreamsCalculate| RECLAIM /RF-1
            HELP |PARSE-Leave| |PARSE-Label| |PARSE-Selector| |/RQ,LIB|
            |PARSE-Sexpr| |PARSE-Category| |PARSE-Sexpr1|
            |queryClients| |PARSE-Enclosure| |executeQuietCommand|
            |pquitSpad2Cmd| |readSpadProfileIfThere| |npPrimary1|
            |scanS| |npRule| |generateResultsName| |generateDataName|
            |printStatisticsSummary| |printStorage| |quit|
            |PARSE-Primary1| |npDefinitionItem| |npDefn| |npMacro|
            |sendHTErrorSignal| |copyright| RESETHASHTABLES MKPROMPT
            |princPrompt| |pquit| |listConstructorAbbreviations|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL T) |displayWorkspaceNames| |removeAllClams|
            MONITOR-AUTOLOAD |prTraceNames| MONITOR-PERCENT
            MONITOR-READINTERP MONITOR-RESULTS |mkOutputConsoleStream|
            MONITOR-REPORT |PARSE-Name| |displayPreCompilationErrors|
            |PARSE-VarForm| MONITOR-UNTESTED INITIALIZE
            MONITOR-INITTABLE MONITOR-END MONITOR-HELP |pfNoPosition|
            |poNoPosition| |sayNewLine| |resetSpacers| |pspacers|
            |pcounters| |ptimers| |npExit| TERSYSCOMMAND |spadThrow|
            |terminateSystemCommand| |setOptKeyBlanks| |spadReply|
            |spadPrompt| |fin| |asList| FOAM:|fiGetDebugVar|
            INITIAL-GETDATABASE |computeElapsedSpace| CATEGORYOPEN
            |displayHeapStatsIfWanted| BROWSEOPEN OPERATIONOPEN
            $TOTAL-ELAPSED-TIME |random| |npColonQuery| |npType|
            |npPop1| |npMatch| |npBreak| |pfNothing|
            |displayExposedConstructors| REDUCE-STACK-SHOW
            |popTimedName| |displayExposedGroups| |npPop2|
            |statRecordLoadEvent| |statisticsSummary|
            |displayHiddenConstructors| |npState| |npSegment|
            |npFromdom| |nextInterpreterFrame|
            |previousInterpreterFrame| CURRENT-SCANNER-CHAR
            |oldParserAutoloadOnceTrigger| |PARSE-Expression|
            |getInfovecCode| INIT-BOOT/SPAD-READER
            |updateCurrentInterpreterFrame| |clearCmdSortedCaches|
            |clearCmdCompletely| |npForIn| |npAssignVariableName|
            |getParserMacroNames| |npItem| |getParserMacros|
            |npFirstTok| |npVariable| |historySpad2Cmd| |npComma|
            |npDefaultItem| |npDefaultDecl| |npTypeVariable|
            |resetInCoreHist| |npTagged| |npExpress1| |histFileName|
            |npADD| |npConditionalStatement| |initHistList|
            |npSelector| |npQualType| |reportWhatOptions|
            |npQualifiedDefinition| |npLambda| |npPPg| |npSingleRule|
            |npQuiver| |npPCg| |npDiscrim| |npLocalDecl|
            |npVariableName| |npVoid| |npExport| |npLocal| |npInline|
            |npFree| |npPPf| |npAtom1| |npDollar| |npPDefinition|
            |npSigItem| |npSigDecl| FAIL |tempLen|
            |initialiseIntrinsicList| |mkLowerCaseConTable|
            |getIntrinsicList| NEXT-TOKEN |returnToReader| |currentSP|
            |returnToTopLevel| |simpCategoryTable|
            |simpTempCategoryTable| |nangenericcomplex| /EMBEDREPLY
            |describeSetOutputOpenMath| |cc| |genTempCategoryTable|
            |allConstructors| |stopTimer| |describeSetLinkerArgs|
            |describeSetOutputMathml| |genCategoryTable|
            |getCodeVector| |describeSetOutputFormula| |startTimer|
            |describeSetFortTmpDir| |clock| |describeInputLibraryArgs|
            |describeSetFortDir| |sayAllCacheCounts|
            |describeSetOutputAlgebra| |describeSetOutputFortran|
            |describeSetFunctionsCache| |describeSetOutputTex| TOP
            |describeAsharpArgs| |PARSE-Conditional| CLEAR-HIGHLIGHT
            |PARSE-ElseClause| |describeOutputLibraryArgs| |PARSE-Exit|
            RESET-HIGHLIGHT |PARSE-Return| |PARSE-LabelExpr|
            |setViewportProcess| |PARSE-Primary| |waitForViewport|
            |PARSE-Data| |dcSizeAll| |PARSE-Import| |incConsoleInput|
            |PARSE-Float| |PARSE-String| |PARSE-IntegerTok|
            |scanDictCons| |inclmsgIfBug| |scanPunCons| |PARSE-AnyId|
            |synonymSpad2Cmd| |PARSE-Loop| |PARSE-Iterator|
            |PARSE-ReductionOp| |inclmsgConsole| |inclmsgFinSkipped|
            |initializeSystemCommands| |PARSE-Qualification|
            |PARSE-Application| |PARSE-Infix| |intNewFloat|
            |PARSE-Statement| |syGeneralErrorHere| |PARSE-Prefix|
            |oldHistFileName| |npRecoverTrap| |processGlobals|
            |PARSE-Form| |intSetNeedToSignalSessionManager|
            |PARSE-Reduction| |PARSE-Sequence| |processGlobals1|
            |PARSE-OpenBrace| |PARSE-Sequence1| |PARSE-OpenBracket|
            |PARSE-IteratorTail| |PARSE-FormalParameter|
            |getInterpMacroNames| |interpFunctionDepAlists|
            |getWorkspaceNames| |PARSE-Quad| |ncTopLevel| |writeHiFi|
            |updateInCoreHist| |clearCategoryCaches|
            |clearConstructorCaches| |makeInitialModemapFrame|
            |statisticsInitialization| |initializeInterpreterFrameRing|
            |cacheStats| |reportAndClearClams| |spadStartUpMsgs|
            |clearClams| |rbrkSch| RESTART0 |sayShowWarning| |spad|
            |reportInstantiations| |lbrkSch| /TRACEREPLY
            |clearConstructorAndLisplibCaches| |clamStats|
            NEXT-SCANNER-CHAR TOKEN-STACK-SHOW |npInfixOp|
            |npPrefixColon| |npNext| |createResolveTMRules|
            |createResolveTTRules| |npCoerceTo| |npSCategory|
            |npApplication| |npPrimary| |npDefaultValue|
            |npBacksetElse| |scanKeyTableCons| |npBPileDefinition|
            |scanEsc| |npFix| |npLet| |npArith| |scanError|
            |npAmpersand| |npCommaBackSet| |scanToken| |scanEscape|
            |scanNumber| |npColon| |scanString| |scanSpace|
            |npTypified| |npDefTail| |scanPunct|
            |npDefinitionOrStatement| |npSymbolVariable|
            |scanNegComment| |npId| |startsNegComment?|
            |npSignatureDefinee| |scanComment| |npInfixOperator|
            |startsComment?| |npDecl| |npPileDefinitionlist| |npSum|
            WRITE-WARMDATA |npDisjand| |npWhile| |npQualDef|
            |resetStackLimits| |npConstTok| WRITE-INTERPDB
            |processSynonyms| |npRestrict| |npIterator| |PARSE-NewExpr|
            |npSuchThat| YEARWEEK WRITE-CATEGORYDB EMBEDDED
            WRITE-OPERATIONDB |extendConstructorDataTable|
            WRITE-BROWSEDB WRITE-COMPRESS |allOperations| |frameNames|
            |reportCount| |coercionFailure| |npCategoryL|
            |npDefaultItemlist| |npSLocalItem| |npPrimary2|
            |writeHistModesAndValues| |npLocalItemlist|
            |saveDependentsHashTable| ERRHUH |loadExposureGroupData|
            |npApplication2| |saveUsersHashTable| IS-GENSYM
            |npVariablelist| |npPop3| |npTerm| FRICAS-INIT
            |npRemainder| |npSemiBackSet| |voidValue| |npRelation|
            |npDefinitionlist| |npPCff| |npLocalItem| |intSetQuiet|
            |npGives| |npPPff| |npTypeStyle| |npPretend|
            |npAssignVariable| |spadpo| |runspad| |npPushId|
            |makeConstructorsAutoLoad| |initializeRuleSets| |npLogical|
            |npMDEF| |npQualTypelist| |intUnsetQuiet| |npInterval|
            |npReturn| |npAssignment| |npTrap| |npName|
            |npTypeVariablelist| |npExpress| |PARSE-Seg| |npAtom2|
            |newFortranTempVar| |npSDefaultItem| |npProduct| |npMdef|
            |npDefinition| |PARSE-PrimaryNoFloat| |npPower|
            |PARSE-TokTail| |npSuch| PARSE-SPADFLOAT
            |PARSE-FormalParameterTok| ADVANCE-TOKEN |npPileExit|
            |npIterate| |npSignature| NEXT-LINES-CLEAR |npSigItemlist|
            CURRENT-SYMBOL PARSE-ARGUMENT-DESIGNATOR
            |printableArgModeSetList| |PARSE-Suffix| |PARSE-InfixWith|
            PARSE-KEYWORD |PARSE-With| POP-REDUCTION |PARSE-SemiColon|
            |npDef| PARSE-SPADSTRING |npStatement| |npImport|
            |npTyping| CURRENT-CHAR ADVANCE-CHAR |npAssignVariablelist|
            NEXT-CHAR |npLoop| GET-INTVAL |npAssign| NEXT-LINES-SHOW
            CURRENT-TOKEN |npIterators| |version| |npSQualTypelist|
            |npEncl| |npBDefinition| |npSynthetic| |npAmpersandFrom|
            CURRENTTIME |npBy| |quadSch| SPAD_SHORT_ERROR
            SPAD_LONG_ERROR PARSE-NUMBER FRICAS-RESTART
            BOOT-SKIP-BLANKS |resetWorkspaceVariables| |initHist|
            |initNewWorld| COMPRESSOPEN IOSTAT INTERPOPEN
            PARSE-IDENTIFIER CREATE-INITIALIZERS |credits|
            |NRTmakeCategoryAlist| |piles| |NRTgenFinalAttributeAlist|
            BUMPCOMPERRORCOUNT |updateFromCurrentInterpreterFrame|
            |createCurrentInterpreterFrame| |makeClosedfnName|
            |getSystemCommandLine| |clearHashReferenceCounts|
            |checkWarningIndentation| |computeElapsedTime| |ncIntLoop|
            |updateHist| |peekTimedName| |intloop| |clearFrame|
            |traceReply| $TOTAL-GC-TIME |clearMacroTable| |?t|
            |statRecordInstantiationEvent| |inclmsgCmdBug|
            |resetCounters| |resetTimers| |ncError|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) *) INIT-MEMORY-CONFIG |newGoGet| NEXT-LINE
            $ERASE |Undef| META-SYNTAX-ERROR)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) *) |agggsub| |agggsuper| |agggwidth|
            |NRTevalDomain| |reportOpsFromUnitDirectly0|
            |reportOpsFromUnitDirectly1| |initializeLisplib| |numArgs|
            |showSpad2Cmd| |mkEvalableCategoryForm| |qTSub| |qTSuper|
            BRIGHTPRINT-0 MONITOR-RESTORE |parseTransform| |sayMSGNT|
            |matSub| |mkEvalable| |sayWidth| |formatSignatureArgs0|
            |lnFileName| |form2StringAsTeX| |prefix2StringAsTeX|
            |pfGlobalLinePosn| |NRTinnerGetLocalIndex|
            |formatSignatureArgs| |prefix2String0| |formString|
            |form2String| |reportOpSymbol| |form2StringWithPrens|
            |makeSimplePredicateOrNil| MAKE-REASONABLE |postInSeq|
            |postMakeCons| |tuple2String,f| |object2String|
            |mathObject2String| |poFileName| |formatAttributeArg|
            |numMapArgs| |form2StringLocal| |getBrowseDatabase|
            |spadcall1| |editFile| |brightPrint0| |roundStat|
            |sayDisplayWidth| |npPP| |doReplaceSharpCalls|
            |timedEvaluate| |startHTPage| |frameSpad2Cmd| |abbQuery|
            |addNewInterpreterFrame| MAKE-DEPSYS |importFromFrame|
            GET-TOKEN |clearSpad2Cmd| |withAsharpCmd| |safeWritify|
            |restoreHistory| |changeHistListLen| |showHistory|
            |saveHistory| |setHistoryCore| |histFileErase|
            |replaceSharpCalls| VMLISP::DELETE-DIRECTORY
            |fortFormatCharacterTypes,mkCharName|
            VMLISP::GET-IO-INDEX-STREAM VMLISP::GET-INPUT-INDEX-STREAM
            MONITOR-PRINTREST |outputTran| |NRTtypeHack|
            |asyGetAbbrevFromComments| |intern| |setOutputOpenMath|
            |setExposeDrop| |setExposeAdd| |setOutputMathml|
            |setLinkerArgs| |setStreamsCalculate| |setExpose|
            |setOutputFormula| |ncAlist| |mac0InfiniteExpansion,name|
            |setOutputFortran| |setFunctionsCache| |clearCmdParts|
            |setOutputTex| |popSatOutput| |sayString| |spleI|
            |sayDisplayStringWidth| |writifyComplain|
            |brightPrintCenter| |brightPrintCenterAsTeX|
            |sayDisplayWidth,fn| |sayWidth,fn| |getMsgTag|
            |brightPrintHighlightAsTeX| |escapeSpecialChars|
            |brightPrintHighlight| |unAbbreviateKeyword| |transHasCode|
            |brightPrint0AsTeX| |getMsgPos2| MONITOR-EVALAFTER
            MAKE-APPENDSTREAM MAKE-INSTREAM |poGlobalLinePosn|
            |parseAndEvalStr1| |protectedEVAL| |parseAndEvalStr|
            |form2String1| |postTran| |htCommandToInputLine|
            |unparseInputForm| |entryWidth| |verbatimize|
            |formatOpType| |parseAnd| |pred2English| |parseIf| |npPC|
            |outformWidth| |compQuietly| |parseNot| |mathPrint|
            |compileFileQuietly| |parseOr| |compileQuietly|
            |prefix2String| |parseSeq| |parseTran| |asyJoinPart|
            |tokType| FILE-RUNNER |optCallEval| |postSlash|
            INIT-FILE-GETTER |postConstruct| |postBigFloat|
            |ExecuteInterpSystemCommand|
            |InterpExecuteSpadSystemCommand| SRCABBREVS
            |doSystemCommand| INIT-LIB-FILE-GETTER DEFTRAN
            |tabsToBlanks| |abbreviationsSpad2Cmd| BPINAME OBEY
            |aggwidth| |justifyMyType| |ncTag| |savesystem| |set|
            |show| |summary| DEF-MESSAGE1 DEF-WHERECLAUSELIST |issueHT|
            |setOutputAlgebra| |startReplaceHTPage| |what| |killHTPage|
            DEF-ISNT |subrname| |halfWordSize| |linkToHTPage|
            |parseLessEqual| |parseGreaterEqual| |parseNotEqual|
            |parseTranList| |printBasic| |parseLeftArrow| |parseLhs|
            |parseUpArrow| |apropos| |fortPre1| |exptSub|
            |fortFormatCharacterTypes,par2string| |mathprintWithNumber|
            |checkPrecision| |fix2FortranFloat|
            |mkParameterList,par2string| |aggSub| |aggSuper|
            |startHTPopUpPage| |fortError1| BOOT-LOAD |quoteSub|
            |exp2FortOptimizeArray| |quoteSuper| |obj2String|
            |superspan| MAKE-OUTSTREAM |overlabelWidth| |overbarWidth|
            |rootSub| |abbreviations| |compiler| |cd| |clear| |close|
            |display| |transSeq| |frame| |history| |pfFileName|
            |subSuper| |porigin| |pfname| |whatSpad2Cmd| |vConcatSuper|
            |compileAsharpCmd1| |postCategory,fn|
            |fixObjectForPrinting| |incHandleMessage| |systemCommand|
            |normalizeStatAndStringify| WIDTH |phBegin| |subspan|
            |displaySpad2Cmd|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) T) RKEYIDS MONITOR-RESET MONITOR-DISABLE
            MAKE-MONITOR-DATA MONITOR-ENABLE MONITOR-TESTED SAY
            |concat| MOAN |dcSize| RDEFINSTREAM RDEFOUTSTREAM
            FOAM::MAKE-FOAMPROGINFOSTRUCT VMLISP::MAKE-LIBSTREAM
            |incIgen1| MAKE-REDUCTION |incRgen1| |next1| |incZip1|
            |incAppend1| |nextown1| MAKE-DATABASE BOOT |spadCompile|
            INTERRUPT TOPLEVEL |displayCategoryTable| |sum| CROAK
            |runOldAxiomFunctor| MAKE-XDR-STREAM |incLude1|
            |buildBitTable| ENABLE-BACKTRACE |throwMessage|
            |EnumerationCategory| |UnionCategory| MAKE-LINE
            NEXT-BOOT-LINE IOCLEAR |canCoerce| |canCoerceFrom|
            |coerceConvertMmSelection| |hasFileProperty|
            |isLegitimateMode| |resolveTT| |selectMms1| $FILEP |dc|
            |Union| |Mapping| |RecordCategory| |synonym| MAKE-STACK
            READ-A-LINE META-META-ERROR-HANDLER INITROOT
            SPAD_SYNTAX_ERROR |start| |Enumeration0| MAKE-TOKEN)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) CHARACTER) LINE-CURRENT-CHAR EBCDIC NUM2CHAR)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) FIXNUM) LINE-NEW-LINE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) STRING) |make_spaces| LINE-BUFFER
            |stripSpaces|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) T) |fracsuper| PREPARSE-ECHO |typeTimePrin|
            |fracwidth| ATENDOFUNIT |ncloopParse|
            |transformOperationAlist| |displayMacros|
            |outputDomainConstructor| |inclmsgPrematureFin| PARSEPRINT
            |pfPrintSrcLines| |texFormat| |orderBySlotNumber|
            |displayOperations| RPACKFILE |incClassify| PREPARSE1
            |dewritify,dewritifyInner| |makeCompactDirect| INFIXTOK
            |predTran| |getCategoryOpsAndAtts|
            |getSlot1FromCategoryForm| |compileInteractive|
            |orderBySubsumption| |simplifyMapConstructorRefs|
            |orderByContainment| |domainToGenvar|
            |makeCompactDirect1,fn| |stripOutNonDollarPreds|
            |genDomainTraceName| STREAM-EOF |loadLibIfNotLoaded|
            RECOMPILE-LIB-FILE-IF-NECESSARY |binomSub|
            |predicateBitIndex| |isHasDollarPred|
            |getBpiNameIfTracedMap| |lisplibDoRename| |binomSuper|
            MONITOR-EXPOSEDP |mkMapPred| |finalizeLisplib|
            |spadClosure?| |binomWidth| |incBiteOff| |If?|
            |DNameFixEnum| |reportOpsFromUnitDirectly|
            LIBSTREAM-DIRNAME |mapPredTran| |CompStrToString|
            |coerceSpadArgs2E| |altSuperSubSub| MONITOR-DATA-COUNT
            |getIteratorIds| |hashCode?| |coerceSpadFunValue2E|
            |sayTeX| |printSynonyms| |altSuperSubSuper| |isTraceGensym|
            |whatCommands| |altSuperSubWidth| |DNameToSExpr|
            |updateCategoryFrameForConstructor| PNAME
            MONITOR-CHECKPOINT |getUserIdentifiersInIterators|
            |DNameToSExpr1| |updateCategoryFrameForCategory|
            MONITOR-DATA-MONITORP |getUserIdentifiersIn|
            |displayOperationsFromLisplib| |boxSub|
            MONITOR-DATA-SOURCEFILE |say2PerLine| |boxSuper|
            MONITOR-DATA-NAME |processSynonymLine| MAKE-BVEC |boxWidth|
            |undoCount| |workfilesSpad2Cmd| MONITOR-NRLIB
            |npProcessSynonym| |rMkIstream| |walkForm| |qTWidth|
            MONITOR-LIBNAME |break| |rMkOstream| |flattenSemi|
            MONITOR-APROPOS BRIGHTPRINT |killNestedInstantiations|
            |postTransform| |pfPosn| VMLISP::PROBE-NAME |poImmediate?|
            MONITOR-PARSE |compFailure| |saySpadMsg| |lnString|
            MONITOR-SPADFILE |isDefaultPackageForm?| |unInstantiate|
            |lnGlobalNum| |extsub| MONITOR-INCR |closeOldAxiomFunctor|
            |sayALGEBRA| |pfPlaceOfOrigin| |extsuper| |getToken|
            MONITOR-DELETE |untraceMapSubNames| |sayMSG| |extwidth|
            |poPlaceOfOrigin| DEF-PROCESS |pfSourcePositions|
            |sayMSG2File| |poNoPosition?| |convertOpAlist2compilerInfo|
            FOAM:|printNewLine| |lnImmediate?|
            |loadIfNecessaryAndExists| |pfSourcePositionlist|
            |matSuper| |simplifyMapPattern,unTrivialize|
            |pfPosImmediate?| |matWidth| |getEqualSublis|
            INITIALIZE-PREPARSE |pfFileName?| MONITOR-DIRNAME
            |sayFORTRAN| |poFileName?| |object2Identifier| MONITOR-FILE
            |lnExtraBlanks| |sayFORMULA| |pfSourceToken| |pfFirst|
            MONITOR-DATA-P |pfGetLineObject| |transCategoryItem|
            MONITOR-DECR |formatSignatureAsTeX| |formatOpConstant|
            |makeSpadConstant| MESSAGEPRINT |loadFunctor|
            |analyzeMap,f| |markUnique| MESSAGEPRINT-1 |expr2String|
            |addConsDB| |formatArgList| |lnLocalNum| |atom2String|
            |removeBodyFromEnv| MESSAGEPRINT-2 |lnFileName?|
            |NRTaddInner| |script2String| |poIsPos?| |pfNopos?|
            |NRTgetOperationAlistFromLisplib| |systemError|
            |getOpSegment| |combineMapParts| |dcAll| MANEXP |error|
            |isInternalMapName| |pfIdSymbol| ACOT
            |sayRemoveFunctionOrValue| |mac0Get|
            |listOfPredOfTypePatternIds| COT |postIteratorList|
            |isPatternVar| |makeByteWordVec| |getCType| CSC
            |new2OldDefForm| |form2StringWithWhere|
            |makeArgumentIntoNumber| ACSC |new2OldTran| |addSpaces|
            ASEC |ncParseFromString| |mkAtreeExpandMacros| THETA_ERROR
            CSCH |pair2list| |postTransformCheck| |isInterpMacro|
            |sayModemap| |userError| |knownEqualPred| COTH |postcheck|
            |displayTranModemap| SECH |tuple2List| |mkAtree1| ACSCH
            |lispType| |formatModemap| ACOTH |checkForBoolean|
            |hashable| ASECH |productOrParen| FOAM::INSERT-TYPES
            |unTuple| |powerOrParen| |getMsgFTTag?| |prefix2Infix|
            |postTranList| |alreadyOpened?| |functionAndJacobian|
            |comma2Tuple| |remLine| |postDoubleSharp| |removeBindingI|
            |erMsgSort| |killColons| |formatSignature| |toFile?|
            |fetchOutput| |isLeaf| |getMsgToWhere|
            |removeSuperfluousMapping| |form2Fence1| |makeLeaderMsg|
            |checkWarning| |form2FenceQuote| |getPreviousMapSubNames|
            |parse2Outform| LOG10 |postDefArgs| |mkAtreeValueOf|
            |removeIsDomainD| |poPosImmediate?| |untrace| |str2Outform|
            FOAM::FOAMPROGINFOSTRUCT-P |binop2String|
            |removeTracedMapSigs| |getValue| |pfForin?| |sumOrParen|
            |toScreen?| |retract| |pfCollect?| |pfSourceText|
            |formatPredParts| |transTraceItem| LISTOFATOMS
            |pf0LoopIterators| |formatOperationAlistEntry| LASTATOM
            |getUnname| |pfLoop?| |poGetLineObject| |formatIf|
            |makeLispList| |getMsgPrefix| |pfExitExpr|
            |lnPlaceOfOrigin| |mkQuote,addQuote| |showMsgPos?|
            |getTraceOption| |getMode| |pfExitCond| |pfPosOrNopos|
            |formatMapping| |makeOutputAsFortran| |line?|
            |stackTraceOptionError| |bootLabelsForGO| |pfExit?|
            |leader?| |isListOfIdentifiers| |removeEXITFromCOND?|
            |objVal| |pfFromdomDomain| |incIgen|
            |isListOfIdentifiersOrStrings| NUMOFNODES |pfFromdomWhat|
            LINE-P |getMsgText| |getTraceOption,hn| |pfFromdom?|
            STACK-CLEAR |mkAtreeValueOf1| |poCharPosn|
            |transOnlyOption| |pfPretendType| |asTupleAsVector|
            |incRgen| |getStFromMsg| |spadTrace,g| |alqlGetOrigin|
            |isMapExpr| |pfPretendExpr| |tabbing|
            |flattenOperationAlist| |pfPretend?| |srcPosFile|
            |getMsgLitSym| |alqlGetKindString| |pfCoercetoType|
            |computedMode| |getPosStL| |shortenForPrinting|
            |alqlGetParams| |getTarget| |pfCoercetoExpr| |srcPosSource|
            |StreamNull| |getPreStL| |pfCoerceto?| |transformREPEAT|
            |isType| |pfTaggedExpr| |transformCollect| |posPointers|
            |untraceAllDomainLocalOps| |pfTaggedTag| SUBANQ |pfTagged?|
            |spadReply,printName| |pfIfElse| |To| |prTraceNames,fn|
            |compNewnam| |pfIfThen| |npNull| |addTraceItem| |compTran|
            |vec2Lists| |pfIfCond| |bootAbsorbSEQsAndPROGNs,flatten|
            |pfIf?| |srcPosLine| |pfTuple?| |testPredList|
            |getCatAncestors| |interactiveModemapForm,fn|
            |asyTypeJoinPartPred| |isSharpVar| |pfLiteral?|
            |srcPosColumn| |list1| |dcData| |asyTypeUnit|
            |pfSequence2Sex0| |list3| |getFlag| |dcData1| |pathname|
            |astran| |pfCollectBody| |dnf2pf| |int2Bool|
            |getDomainFromMm| |namestring| |asMakeAlist|
            |pfCollectIterators| |be| |srcPosDisplay|
            |dbSpecialDisplayOpChar?| |isExposedConstructor|
            |asyParents| |pfDefinitionRhs| |reduceDnf|
            |asTupleNewCode0| |brightPrint| |isFreeFunctionFromMm|
            |asyDocumentation| |pf0DefinitionLhsItems| REMDUP
            |getDependentsOfConstructor| FOAM:|fiGetDebugger|
            |asyConstructorModemap| |hasOptArgs?| |prove|
            DATABASE-PREDICATES |asytran| |pfApplicationArg|
            |getSrcPos| |clearAllSlams| |multiToUnivariate|
            |getIProplist| DATABASE-CONSTRUCTORFORM ASSOCLEFT |as|
            |bassertNot| DATABASE-DOCUMENTATION ASSOCRIGHT |list2|
            DATABASE-OPERATIONALIST |minimalise| |minimalise,min|
            |addBlanks| FOAM:|fiSetDebugVar| |minimalise,HashCheck|
            |operationLink| VMLISP::LIBSTREAM-INDEXTABLE
            |NRTcatCompare| UNSQUEEZE |isDomainSubst| IS_GENVAR
            |vec2Lists1| |emptyAtree| |segmentedMsgPreprocess|
            |listOfPatternIds| |pileCforest| |sayBrightlyLength1|
            |getFirstArgTypeFromMm| |vectorOfFunctions| |enPile|
            DATABASE-COSIG |sayLongOperation| |signatureTran|
            |makeFort,untangle| |separatePiles| |getSymbolType|
            DATABASE-CONSTRUCTORMODEMAP |splitListSayBrightly|
            |evalMmDom| DATABASE-CONSTRUCTORKIND |say2PerLineThatFit|
            |mkAlistOfExplicitCategoryOps| |pileColumn| |hashCount|
            |pilePlusComments| |nodeCount|
            |mkAlistOfExplicitCategoryOps,atomizeOp| |npPush|
            |pilePlusComment| |mkCircularAlist| |flattenSignatureList|
            |npWith| |insertpile| |isFreeFunctionFromMmCond|
            |evalMmStack| |clearSlam| MSORT |pileComment|
            |setExposeAddConstr| |S_process| DATABASE-ABBREVIATION
            |moveORsOutside| |mkDatabasePred| |getDomainHash|
            |lastTokPosn| |getCacheCount| SET-FILE-GETTER
            |getConstructorModemap| |setAutoLoadProperty| |pfBreak|
            |firstTokPosn| |npEqKey| |pfRuleRhs| |maximalSuperType|
            |pfRhsRule2Sex| |noSharpCallsHere| |npRestore|
            |updateTimedName| |pfRuleLhsItems| |pfLhsRule2Sex|
            |mkRationalFunction| |splitIntoBlocksOf200| |pfOp2Sex|
            |isNameOfType| |dropPrefix| |selectMostGeneralMm|
            |constructor| |objMode| |npInfGeneric| |pmDontQuote?|
            |compileSpadLispCmd| |timedEVALFUN| |pfSymbol?|
            |getModeSetUseSubdomain| |helpSpad2Cmd|
            |getConstructorAbbreviation| |formal2Pattern|
            |pushTimedName| |getModeSet| |newHelpSpad2Cmd|
            |getLisplibName| |getUsersOfConstructor| |printNamedStats|
            |opTran| |matchMmCond| |unabbrevAndLoad|
            |getOplistForConstructorForm| |patternVarsOf|
            |getPartialConstructorModemapSig|
            |getOperationAlistFromLisplib| FOAM:|formatDFloat|
            FOAM:|formatSFloat| |pfLambda2Sex| FOAM:|formatBInt|
            |pfDefinition2Sex| FOAM:|formatSInt| TOKEN-P
            |pfCollect2Sex| |closeInterpreterFrame| |pfSequence2Sex|
            |pfApplication2Sex| |gensymInt| |pfWhereExpr| |recordFrame|
            |charDigitVal| |pf0WhereContext| |zeroOneTran| |pfIterate?|
            |formatUnabbreviatedTuple| |pfReturnExpr|
            VMLISP::GET-DIRECTORY-LIST |undoLocalModemapHack|
            |formatUnabbreviated| |pfReturn?| |makeDomainTemplate|
            |formatUnabbreviatedSig| |pfBreakFrom| |template|
            |pfBreak?| |pfRule?| |infovec| |pfNovalueExpr| |pfNovalue?|
            |collectDefTypesAndPreds| |pfNotArg| |pfNot?| |getUnname1|
            |clearCmdExcept| |pfOrRight| |pfOrLeft| |pfOr?|
            |pfAndRight| |unVectorize| |cgammat| |compileAsharpLispCmd|
            |pfAndLeft| |pfId?| |pfAnd?| |pfWrong?| |pfNovalue|
            |pf0LocalItems| |pfLocal?| |asTupleSize| |pf0FreeItems|
            FOAM::TYPE2INIT |pfFree?| |pfRestrictType|
            |loadIfNecessary| |pfRestrictExpr| |failCheck|
            |pfRestrict?| |expandDO| |pfDefinition?| |intpart| SEQOPT
            |pfAssignRhs| MKQSADD1 |pf0AssignLhsItems| |pfCheckItOut|
            |pfAssign?| |pfDoBody| |phiRatapprox| |pfDo?|
            |pfSuchthatCond| |lnrgammaRatapprox| |pfSuchthat?|
            |pfWhileCond| VMLISP::LIBRARY-FILE |readHiFi| |npDotted|
            |pfWhile?| |pfForinWhole| |whatSpad2Cmd,fixpat|
            |pf0ForinLhs| |containsVars1| |parseSystemCmd|
            |dumbTokenize| |resolveTypeList| |parseFromString|
            |tokTran| |b2dnf| |npConditional| |bnot| |bor|
            |domainDepth| |band| |bassert| |npParenthesized| |isMap|
            |evalMmStackInner| NREVERSE0 UNVEC |orderMmCatStack|
            |notDnf| |isDomainValuedVariable| |pfSequence|
            |isAVariableType| |notCoaf| |packageForm?| DROPENV SHOWBIND
            |constructSubst| |ordList| VMLISP::FLAT-BV-LIST |pfFix|
            |printMms| |orderList| |npSemiListing| |containsVars|
            VMLISP::EQUABLE |npFromdom1| C-TO-R |isHomogeneousList|
            |fixUpTypeArgs| CLNGAMMA C-TO-S S-TO-C |lncgamma|
            |string2Float| |checkType| |fortFormatCharacterTypes|
            |fortFormatCharacterTypes,mkParameterList2| RGAMMA |rgamma|
            |mkParameterList| VMLISP::GET-INDEX-TABLE-FROM-STREAM
            |fortFormatIntrinsics| |compExpand| |fortFormatElseIf|
            C-TO-RF PUSHLOCVAR |fortFormatIf| CGAMMA
            |removeEXITFromCOND| |cgamma| |getImmediateSuperDomain|
            |compressHashTable| RLNGAMMA |dispfortexp1|
            |augmentLowerCaseConTable| |lnrgamma| |clearCategoryTable|
            |fortran2Lines1| |domainForm?|
            |updateCategoryTableForCategory| |simpBool|
            |getConstructorUnabbreviation| |simpCatPredicate|
            |addToCategoryTable| |clearTempCategoryTable|
            |dispStatement| |negintp| |categoryParts,exportsOf|
            |getfortexp1| |getAttributesFromCATEGORY| STACK-P
            |fracpart| |asyTypeJoinPartWith| TOKEN-PRINT |getConstrCat|
            |asyIsCatForm| |showCategoryTable| |PsiIntpart|
            |asyCattran1| |asyCattranOp| |clngammacase3|
            |cgammaBernsum| |upequation| |asMakeAlistForFunction|
            |uplocal| |getCategoryExtensionAlist| |asyUnTuple|
            |simpHasPred,simpDevaluate| |asyTypeItem| NMSORT
            |cgammaAdjust| |copyHack| |evalType| |trimComments|
            |quoteNontypeArgs| |gammaRatapprox| |upIF| |uperror|
            |simpHasPred,simp| |asAll| |gammaRatkernel| |hasIdent|
            |evaluateType1| |gammaStirling| |asyAncestorList|
            |makeOrdinal| |upREPEAT0| |getAndEvalConstructorArgument|
            |interpOnlyREPEAT| |evaluateType| |upREPEAT1| |mkNiladics|
            |isHomogeneous| |simpHasPred,eval| /UNEMBED-1
            |newHasTest,evalCond| |asyArg| |devaluateDeeply| |uphas|
            |getCategoryExtensionAlist0| /UNEMBED-Q |upREPEAT| |upSEQ|
            |evaluateFormAsType| |setExposeDropConstr| |upisnt|
            |asyTypeJoinItem| |setExposeDropGroup| |uppretend|
            |evaluateSignature| |bright| |abbreviation?|
            |asyTypeJoinPartIf| |getUnnameIfCan| |upTuple| |asyMkpred|
            |mkEvalableMapping| |translateYesNo2TrueFalse|
            |mkCategoryExtensionAlist| |asyLooksLikeCatForm?|
            |mkEvalableUnion| |setInputLibrary|
            |mkCategoryExtensionAlistBasic| |mkEvalableRecord|
            |instantiate| |typeOfType| |asyTypeMapping| |evaluateType0|
            |setExposeAddGroup| |isFormalArgumentList|
            |displayDatabase| |uptypeOf| |upbreak|
            |asyTypeJoinPartExport| |upwhere| |asytranEnumItem|
            |upDollar| |simpOrUnion| |trimString|
            |translateTrueFalse2YesNo| |asyExportAlist|
            |asytranLiteral| |vectorSize| |StringToCompStr|
            |getExportCategory| |asCategoryParts,exportsOf|
            |setHistory| |depthAssoc| |quoteCatOp| WHOCALLED
            SPADSYSNAMEP |asyType| |validateOutputDirectory|
            |getInfovec| |editSpad2Cmd| |updateSourceFiles| MAKE-CVEC
            |pathnameName| |processKeyedError| |setFortTmpDir|
            |pathnameType| |pathnameDirectory| |lambdaHelper2|
            |functionp| |From| |setOutputLibrary| |macrop| |pp2Cols|
            |setFortDir| |loadLib| |whatConstructors| |poNopos?|
            |countCache| |asTupleNew0| |sayAsManyPerLineAsPossible|
            VMLISP::GETINDEXTABLE |handleLispBreakLoop|
            |getDomainByteVector| |mac0GetName| |msgNoRep?|
            |isCategoryPackageName| |hashString|
            |isSubForRedundantMapName| |macApplication| |upand|
            |pf0ApplicationArgs| |PARSE-Expr| |getMsgPos|
            |upAlgExtension| |pfMLambda?| XDR-STREAM-NAME
            |IdentityError| |whichCat| IDENT-CHAR-LIT |pfApplicationOp|
            XDR-STREAM-HANDLE |upcase| |erMsgSep| |hasDefaultPackage|
            |mathprint| |setAsharpArgs| |upDeclare| |pfLeafPosition|
            |initializeSetVariables| |upor| EQUABLE |pfAbSynOp|
            |setOutputCharacters| |getLineText| XDR-STREAM-P
            |getMsgPosTagOb| |upADEF| |splitSayBrightlyArgument|
            |opIsHasCat| |pf0MLambdaArgs| CACHEKEYEDMSG |Top?|
            |initToWhere| |pfTypedId| |Skipping?| |initImPr|
            |pf0LambdaArgs| |KeepPart?| |putDatabaseStuff|
            |isOkInterpMode| |string2Words| |pfLambda?| /VERSIONCHECK
            MKQ |isNewWorldDomain| COND-UCASE |pfLeaf?| |getLinePos|
            |center80| |pfMLambdaBody| COMP370 |SkipPart?|
            |msgOutputter| FOAM:|Halt| |macId| |msgImPr?|
            FOAM::PROCESS-IMPORT-ENTRY GETREFV TRACEOPTIONS |rdigit?|
            |getMsgPrefix?| |numberOfNodes| |pfSourcePosition|
            |keyword?| |getMsgKey| |noBlankBeforeP| /UNTRACE-0
            |macMacro| MBPIP |lfkey| |getMsgTag?| |noBlankAfterP|
            |pfNothing?| STACK-SIZE |scanW| |macSubstituteOuter|
            |scanKeyTr| |pfMacroRhs| QSORT |pathname?| |upisAndIsnt|
            |dcPreds| |tabber| |synonymsForUserLevel| |pfMacroLhs|
            PLACEP |incFileName| |lineoftoks| |dcOpTable| |macExpand|
            |inclmsgConActive| |removeConstruct|
            |makePredicateBitVector| |brightPrintRightJustify|
            |macLambda| |inclmsgNoSuchFile| |getMsgInfoFromKey|
            |macWhere| STACK-STORE |lfnegcomment| |getMsgArgL| |upDEF|
            |readLibPathFast| /UNTRACE-REDUCE |pfApplication?|
            |lfcomment| |inclmsgPrematureEOF| |removeAttributes|
            |isSystemDirectory| |untraceDomainConstructor| |pfMacro?|
            |PARSE-NBGliphTok| |getMsgKey?| |blankIndicator|
            |getFunctorOpsAndAtts| |isDomain| |pfWhere?| |pfLambdaTran|
            |scanPossFloat| |msgLeader?| |pfLambdaBody|
            |fileNameStrings| |scanCloser?| |polyVarlist|
            |pfLambdaRets| |incStringStream| |keyword| |upis|
            |bottomUpPercent| |brightPrint1| |npboot| |pf2Sex1|
            |listOutputter| |upLETWithPatternOnLhs| |pfTypedType|
            |upQUOTE| |splitSayBrightly| SMALL-ENOUGH HKEYS
            |pfCollectArgTran| |processChPosesForOneLine| |copyHack,fn|
            |pfTyped?| |remFile| MONITOR-BLANKS |pfSuchThat2Sex|
            |makeMsgFromLine| |isLocalPred| |stripLisp|
            MONITOR-EVALBEFORE |makeCompactSigCode| |pf0TupleParts|
            |predicateBitIndexRemop| |poLinePosn| |upLET|
            |formatSlotDomain| |loopIters2Sex| |dcCats| MAKE-SYMBOL-OF
            HAS_SHARP_VAR |dcCats1| |float2Sex| |isTupleForm| |dcOps|
            |pfLiteral2Sex| |pfSourceStok| |stuffDomainSlots|
            |pfSymbolSymbol| |postTupleCollect| |PARSE-LedPart|
            |setCurrentLine| |npMoveTo| |makeHistFileName|
            |isSharpVarWithNum| |pfLiteralString| |postAdd| |LZeros|
            |PARSE-NudPart| |readSpad2Cmd| |pfLeafToken|
            |quote2Wrapped| IS_SHARP_VAR |dcAtts| |pfLiteralClass|
            |postReduce| |ncloopPrintLines| |handleKind|
            |pathnameTypeId| |dcSlots| UPCASE |postComma| |pfRule2Sex|
            |nplisp| |rulePredicateTran| |mkLineList| |intnplisp|
            |postSemiColon| |ruleLhsTran| |nonBlank| |upreturn|
            |splitIntoOptionBlocks| |postWhere| |upiterate|
            |dewritify,is?| |postColonColon| |upfree| |isNiladic|
            |postColon| |Zeros| |getCon| |displayMacro| |postAtSign|
            |altSeteltable| |displayParserMacro| |postPretend|
            |isHomogeneousArgs| |walkWhereList| FBPIP |postIf|
            |intloopEchoParse| |expandMacros| |postJoin|
            |parseAndEvalToStringForHypertex| |unwritable?|
            |postSignature| |parseAndEvalToHypertex| |getLookupFun|
            |getFirstWord| |postCategory| |ncloopEscaped|
            |oldParseAndInterpret| |removeAttributePredicates|
            |postDef| |parseAndInterpToString|
            |removeAttributePredicates,fn| |replaceGoGetSlot|
            |parseAndInterpret| |predicateBitRef|
            |emptyInterpreterFrame| |postMDef| |serverReadLine|
            |undoFromFile| |ncloopIncFileName|
            |parseAndEvalToStringEqNum| |satisfiesUserLevel|
            |postMapping| |parseAndEvalToString| /TRACE-0 |postExit|
            |nopilesSpad2Cmd| |upconstruct| |reportUndo|
            |CDRwithIncrement| |postTuple| |removeUndoLines| UNEMBED
            /OPTIONS |histInputFileName| |initCache| |appOrParen|
            |pushSatOutput| |formWrapId| |formatAttribute| |concatList|
            FOAM::FOAM-FUNCTION-INFO |findFrameInRing| |matrix2String|
            |mkMessage| |evaluateLines| |dewritify| |clearCache|
            |matrix2String,outtranRow| |changeToNamedInterpreterFrame|
            |tuple2String| |setNopiles| TRY-GET-TOKEN
            |verifyRecordFile| |evalLoopIter| IVECP
            |numberOfEmptySlots| |form2FenceQuoteTail|
            |eq2AlgExtension| LIST2VEC GET-A-LINE VMLISP::COMPILE1
            |clearClam| |formCollect2String| MAKE-STRING-ADJUSTABLE
            |formIterator2String| LINE-NEXT-CHAR |form2StringList|
            |optEQ| |mkNestedElts| |dollarPercentTran| |upCOERCE|
            IS-CONSOLE |formJoin2String| |pfListOf| |iterVarPos|
            |pfTuple| |makeFort,untangle2| |formTuple2String|
            |npPileBracketed| |mkZipCode| |blankList| |coerceOrParen|
            |interpIter| |isLegitimateRecordOrTaggedUnion|
            MAKE-ABSOLUTE-FILENAME |tokPosn| |resolveTypeListAny|
            |listOfDuplicates| |specialChar| |varsInPoly|
            |isPolynomialMode| |typeToInputForm| |isPartialMode|
            |pfAttribute| |mkAndApplyPredicates| |underDomainOf;|
            |upStreamIters| |cleanUpSegmentedMsg| OPTIONS2UC
            |findSubstitutionOrder?| |isFunctor| |isValidType;|
            |npWConditional| |parseColon| |lfstring| |falseFun|
            |parseCoerce| |upCOLLECT0| |parseAtSign|
            |interpOnlyCOLLECT| |unabbrev| |scanTransform| |upCOLLECT1|
            |spadThrowBrightly| |parseCategory| |retract1| |upCOLLECT|
            |isInternalFunctionName| |upTARGET| |removeIsDomains|
            |parseConstruct| |lfspaces| |isUncompiledMap|
            |canRemoveIsDomain?| |npEncAp| |parseDEF| |nextline|
            |isInterpOnlyMap| |record2String| |isDomainOrPackage|
            |parseExit| |domainOne| |lferror| |upLoopIters|
            |abbreviate| |parseHas| |reassembleTowerIntoType|
            |isStreamCollect| |npTuple| |formJoin2| |parseIn|
            |typeToOutputForm| |categoryForm?| |asyComma?|
            |linearFormat| |npInfKey| |parseInBy| |digit?|
            |charyTopWidth| |compAndDefine| |getConstructorSignature|
            |constructorName| |parseIs| COMP-2 |getUnderModeOf|
            |isBinaryInfix| |npAnyNo| |parseIsnt| |domainZero| COMP-1
            |deconstructT| |freeOfSharpVars| |parseJoin| |scanWord|
            |asyPredTran| |formatSignature0| |sayMath|
            |linearFormatName| |parseLeave| |punctuation?|
            |hackToRemoveAnd| |parseLET| |isTaggedUnion| |lfid|
            |mathPrintTran| |asyArgs| |form2Fence| |tokPart|
            |parseMDEF| |numOfSpadArguments| |asyAncestors| |opOf|
            |asyTypeUnitList| |formatModemap,fn| |asySubstMapping|
            |parsePretend| |asyExtractDescription| |removeZeroOne|
            |asyConstructorArgs| |parseReturn| |asyConstructorArg|
            |npListAndRecover| |parseSegment| |lfinteger| |packageTran|
            GET-BOOT-TOKEN |Identity| |asyCosig|
            DATABASE-CONSTRUCTORCATEGORY |asyCosigType| |parseWhere|
            |resolveTTRed3| DATABASE-P |asyExtractAbbreviation|
            |pfDefinitionSequenceArgs| |interpOp?| |asyCATEGORY|
            |npItem1| |compileTimeBindingOf| |resolveTMRed1|
            |asyCattran| DEF-RENAME |optSETRECORDELT| |pfExpr?|
            VMLISP::SPAD-FIXED-ARG DATABASE-SOURCEFILE |asyCatItem|
            |new2OldLisp| |optimize| |pf0AddBase| |string2SpadTree|
            |optimize,opt| |pfAttributeExpr| |pr| |destructT|
            DATABASE-DEFAULTDOMAIN COPY |pf0TLambdaArgs|
            DATABASE-NILADIC DOWNCASE |pp| |pfDefinitionLhsItems|
            DATABASE-OBJECT |optimizeFunctionDef| |pfInline|
            DATABASE-MODEMAPS |asyTypeJoin|
            |optimizeFunctionDef,removeTopLevelCatch| |pfRetractTo?|
            SQUEEZE |asyTypeJoinStack| |opt-| |pfSuchthat|
            DATABASE-CONSTRUCTOR |pfWIfElse| |pfSemiColon|
            |interactiveModemapForm| |optRECORDCOPY| |pfDocument?|
            |asIsCategoryForm| SEC |optRECORDELT| |pfWithWithon|
            |postWith| |pfComDefinitionDef| |pf0FlattenSyntacticTuple|
            |str2Tex| |pfAddAddon| |wrap| |nontrivialCosig|
            |wrapped2Quote| |postQUOTE| |pf0WrongRubble| |objCodeVal|
            |postCollect| |pfWrongRubble| |isDefaultPackageName|
            |objCodeMode| |pfTLambdaBody| |dropLeadingBlanks|
            |spadTypeTTT| |asyTypeMakePred| |pfIterateFrom|
            |removeZeroOneDestructively| SOURCEPATH |postin|
            |pfTransformArg| |bubbleType| DATABASE-DEPENDENTS
            |zeroOneConversion| |intProcessSynonyms| |postIn|
            |pfWIfThen| DATABASE-USERS |makeUnion| |asyShorten|
            |postRepeat| |pfAttribute?| DATABASE-PARENTS
            |createAbbreviation| |pfTupleParts| DATABASE-ANCESTORS
            |stripNil| |isValidType| |pf0CollectIterators| VMLISP::VARP
            |mkQuote| |indefIntegralSub| |pfFlattenApp|
            |makeLazyOldAxiomDispatchDomain| DEF-EQUAL |constructor?|
            |indefIntegralSup| |pfTupleList| |DEF-::| |underDomainOf|
            |indefIntegralWidth| |pfComDefinitionDoc| DEF-REPEAT
            |frameEnvironment| |pf0ImportItems|
            |makeOldAxiomDispatchDomain| |piSub| |pfImportItems|
            |asyPredTran1| |piSup| HACKFORIS |simpCattran| |piWidth|
            |npTrapForm| |sayBrightlyLength| |pfUnSequence|
            |pfQualTypeQual| DEF-COLLECT |pfCheckArg| LIST2CONS-1
            |isIntegerString| |pi2Sub| |decomposeTypeIntoTower|
            |pfCheckId| DEF-SEQ |compileAsharpArchiveCmd| |pi2Sup|
            DEF-LESSP VMLISP::REMOVE-FLUIDS |pi2Width| |npZeroOrMore|
            |pfWIfCond| NEXT-TAB-LOC SMINT-ABLE |aggWidth|
            |pfTLambdaRets| INDENT-POS DEF-IS-REMDUP1 |npDDInfKey|
            |pfAddAddin| |SubstWhileDesizing| |pfHidePart| EXPAND-TABS
            |pfWrongWhy| |optCons| NONBLANKLOC VMLISP::SIMPLE-ARGLIST
            |optSuchthat| DEF-SETELT |outputTranIteration| SHOWDATABASE
            DEF-ELT |XDRFun| |npAdd| BLANKP DATABASE-SPARE |DEF-:|
            |recordAndPrintTest| |optLESSP| |mkAtree| DEF-RENAME1
            |optMkRecord| |asTupleAsList| |pfTweakIf| |remWidth|
            DEF-MESSAGE |dqConcat| |optCall| |typeIsASmallInteger|
            |recordAndPrintTest,fn| |breakIntoLines| DEF-WHERE
            |printAsTeX| |dqUnitCopy| |optCONDtail| DEF-STRING |dqUnit|
            |optPredicateIfTrue| DEF-COND |dqToList| |pfLocal|
            |htTrimAtBackSlash| |optSPADCALL| |bootTransform|
            |ncParseAndInterpretString| |optSEQ,getRidOfTemps| SHUT
            DEF-IS-REMDUP HACKFORIS1 |bubbleConstructor| DEF-IS-EQLIST
            |pfMLambdaArgs| |pfTyping| |pfNot| |texFormat1| LIST2CONS
            |fnameWritable?| |npElse| |myWritable?| |pfForinLhs|
            DEF-IN2ON |read| |fnameReadable?| |pfExpression?| REROOT
            DEF-INSERT_LET1 |fnameName| |pfFree| DIG2FIX MKPROGN
            |segmentKeyedMsg| |mergeCONDsWithEXITs| |pfExport|
            |pfWith?| DEF-STRINGTOQUOTE |optCatch| |mergeableCOND|
            |pfWDeclareSignature| |pf0SequenceArgs| |sumoverlist|
            |spool| DEF-INSERT_LET FOAM-USER::|AXL-spitSInt|
            |fnameType| |bootAND,flatten| |pfSequence?| |maprin|
            |StringToDir| |optSEQ,tryToRemoveSEQ| |bootAND| |pfPile|
            |pfAssignLhsItems| |fnameDirectory| |optSEQ| |npMissing|
            |pfSecond| DEF-WHERECLAUSE |npEqPeek| |DirToString|
            |optSEQ,SEQToCOND| |trace| DEF-ADDLET |fnameExists?|
            |optCond| |bootCOND| |pfImport| LOG2 |undo| |compFluidize1|
            |retract2Specialization| |TruthP|
            |bootPushEXITintoCONDclause| |pfWhereContext| |syminusp|
            |transIs| |coerceUnion2Branch| |bootIF| |optMINUS|
            |transIs1| |workfiles| |pfReturnNoName| |bootPROGN|
            |maprinRows| |isListConstructor| |spad2lisp| |optIF2COND|
            |pfWithWithin| |pfSequenceArgs| |maprinChk| |parseType|
            DEF-IS |bottomUpElt| |complexRows| |optXLAMCond|
            |parseHasRhs| |nodeSize| |bottomUp|
            |bootAbsorbSEQsAndPROGNs| |pfWDeclare?| SIZE |retractAtree|
            |bootSEQ| |optQSMINUS| |pfId| |postError| EOFP |bootTran|
            |parseJoin,fn| |pfParts| |mkAtreeNode| |parseGreaterThan|
            |tryToRemoveSEQ| |pfInlineItems|
            |outputTranMatrix,outtranRow| |parseHas,fn| |fortSize|
            |pfAppend| STACK-UPDATED |pfLoopIterators| |fortSize,elen|
            RSHUT |npListing| |variableNumber| OPTIONAL |pfFreeItems|
            |parseHas,mkand| |expression2Fortran| |devaluateList|
            |nakedEXIT?| |pfCheckMacroOut| |fortranCleanUp|
            |pfSequenceToList| |PARSE-GliphTok| |bootOR,flatten|
            |pfTupleListOf| |npParse| |isExistingFile|
            |exp2FortOptimize| |bootOR| |pfWithBase| |parseTran,g|
            |fortPre| |pfDocument| |segment| |pfSexpr| |parseAtom|
            |exp2Fort1| REDUCTION-RULE |getBasicMode| |pfSexpr,strip|
            |exptNeedsPren| |mkMat| LINE-ADVANCE-CHAR |unwrap|
            |pfSemiColonBody| |isInitialMap| |pfIterate| |isWrapped|
            |timedAlgebraEvaluation| TOKEN-SYMBOL |parseCases|
            |pfInline?| |bool| |fortExpSize| ACTION |pfExitNoCond|
            |addCommas| |pf0WithWithin| |transUnCons| DIGITP
            |getBasicObject| |timesWidth| |pfRetractToType| |setDefOp|
            COMP |npQualified| PRINT-PACKAGE |pf0ExportItems|
            |fortFormatTypes,unravel| |npLetQualified| |exptWidth|
            |pfExportItems| |printMap|
            |exp2FortOptimizeCS1,popCsStacks| |pfExport?|
            |indentFortLevel| VEC2LIST |exptSuper| REDUCTION-VALUE
            |pfSemiColon?| |mathmlFormat| |parseBigelt| |fortPreRoot|
            GETZEROVEC PREPARSE VMLISP::LIBSTREAM-MODE MAKE-VEC
            |stepWidth| |pfDeclPart?| |dispfortexp| |statement2Fortran|
            GCMSG |containsPolynomial| |stepSub| |pfDWhere?|
            |stepSuper| |pfImport?| |formulaFormat| |isFloat|
            |pfTyping?| |inWidth| |pfExportDef| |npBracketed| |inSub|
            STACK-TOP |npAngleBared| |inSuper| GET-SPECIAL-TOKEN
            |pfComDefinition?| |putWidth| TOKEN-NONBLANK |npBraced|
            |bottomUpCompile| GET-SPADSTRING-TOKEN |pfCheckInfop|
            |npBracked| |bottomUpUseSubdomain| MAKE-ADJUSTABLE-STRING
            LINE-AT-END-P |checkLines| |npParened| GET-SPADNUM-TOKEN
            |pfRetractToExpr| |fortran2Lines| |removeQuote| |maprin0|
            |pfDocumentText| TOKEN-TYPE |pfLoop1| |pfAdd?|
            |getUnionOrRecordTags| |concatSub|
            GET-ARGUMENT-DESIGNATOR-TOKEN |pf0WithBase|
            |getModeOrFirstModeSetIfThere| |concatSuper|
            |outputConstructTran| |exp2FortOptimizeCS| |npCompMissing|
            |concatbWidth| |outputTranSEQ| |exp2FortOptimizeCS1|
            VMLISP::LIBSTREAM-P |outputTranRepeat| |concatWidth|
            |outputTranReduce| |outputTranCollect| |displayLines1|
            |outputTranIf| |outputMapTran| |quoteWidth|
            |outputTranMatrix| |changeExprLength| |flattenOps|
            |fortexp0| |stringWidth| |pfEnSequence|
            |wrapMapBodyWithCatch| |displayLines| |concatTrouble,fixUp|
            |nameLen| |sigmaSub| |sigmaSup| |height| |sigmaWidth|
            |outputOp| |containsVariables| |matSuperList| |matSubList|
            |sigma2Sub| |matLSum| |sigma2Sup| |last| |sigma2Width|
            |matLSum2| |intSub| |intSup| |sumWidthA| |intWidth| |absym|
            |pfStringConstString| LINE-SUBSEQ-FROM |deMatrix|
            |overlabelSuper| |pfTaggedToTyped1| |pfTaggedToTyped|
            |pfCollectVariable1| BUMPERRORCOUNT
            INTERPSYS-ECL-IMAGE-INIT |overbarSuper| |pfCollect1?|
            |stringer| |pfDWhereExpr| |maPrin| SPAD_ERROR_LOC
            |pfQualTypeType| BOOT-TOKEN-LOOKAHEAD-TYPE |pfLambdaArgs|
            |pfTLambdaArgs| |largeMatrixAlist| |pfSymbolVariable?|
            |PushMatrix| |pfHide?| LINE-PRINT |rootSuper| |pfWIf?|
            |rootWidth| |pf0TypingItems| LINE-PAST-END-P
            |SubstWhileDesizingList| |devaluate| |pfTypingItems|
            |pfListOf?| |pfAddBase| |pfWhile| GENSYMP |pfDo|
            |sayMessage| |pfLocalItems| |letWidth| |pfLoop|
            |pfReturnFrom| |length1?| |pfDWhereContext| |flattenCOND|
            |string2BootTree| |slashSub| |pfQualType?| |mkAliasList|
            |extractCONDClauses| |edit| |slashSuper| |pfWDeclareDoc|
            |compTran1| |slashWidth| |help| |coerceMap2E|
            |parseTypeEvaluate| |ppos| |subSub| |pfTLambda?|
            |isFreeVar| |library| |objValUnwrap| |load| |suScWidth|
            |isPatternArgument| |pfLinePosn| |ltrace| |encodeCatform|
            |isConstantArgument| |pfCharPosn| |nopiles|
            |depthAssocList| |postForm| |pfImmediate?| |superSubSub|
            |postQuote| |pfNoPosition?| |superSubSuper| |postOp|
            |%fname| |superSubWidth| |postAtom| |constructorCategory|
            DROPTRAILINGBLANKS |postSequence| |%origin| |postElt| |%id|
            |vConcatSub| |postSEGMENT| |systemErrorHere| |postBlock|
            |args2Tuple| |compileAsharpCmd| |vConcatWidth| |inclmsgSay|
            |postBlockItemList| |stripUnionTags| MACROEXPANDALL
            |SkipEnd?| |postType| |mkPredList|
            VMLISP::LIBSTREAM-INDEXSTREAM |isRecord| |binomialSub|
            |frameName| |binomialSuper| |postBlockItem| |Record0|
            |compFluidize| |binomialWidth| |incFileInput| |length2?|
            |newConstruct| |rightTrim| |evalDomain| |clearParserMacro|
            |displayCacheFrequency| |newIf2Cond| |findEqualFun|
            |undoChanges| |remHashEntriesWith0Count| |zagSub|
            |newDef2Def| |incString| |loadSpad2Cmd|
            |constructor2ConstructorForm| |zagSuper| |postCapsule|
            |listOfVariables| |processSynonymLine,removeKeyFromLine|
            |zagWidth| |isLocalVar| BVEC-COPY |incPos| |leftTrim|
            |commandsForUserLevel| |timedOptimization| |incFile|
            |undoInCore| |lispize| |incRenumber| |SpadInterpretFile|
            |lambdaHelper1| |clearCategoryCache| |displayHashtable|
            |significantStat| STACK-POP |bitsOf| |ncSetCurrentLine|
            |clearConstructorCache| |removeAttributePredicates,fnl|
            |phInterpret| |compileBoot| |writify| |Else?| SKIP-TO-ENDIF
            |ppTemplate| |intInterpretPform| |traceSpad2Cmd|
            |writify,writifyInner| |incNConsoles| PREPARSEREADLINE
            |stopTimingProcess| |pf2Sex| |outputTranIterate| |trace1|
            |setIOindex| |sumWidth| |Elseif?| PREPARSEREADLINE1
            |phMacro| |isQuotient| |saveMapSig| |startTimingProcess|
            CHARP |inclmsgConStill| |isRationalNumber| |deleteFile|
            BVEC-NOT |inclmsgCannotRead| |macroExpanded|
            |getTraceOptions| |displayProperties,sayFunctionDeps|
            |reportHashCacheStats| |minusWidth| SKIP-IFBLOCK
            |mkHashCountAlist| |listOfSharpVars| |incCommand?|
            |patternCheck| |stupidIsSpadFunction| FOAM:|fiStrHash|
            |fracsub| |ncConversationPhase,wrapup| |keyp|
            |getMapSubNames|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) *) COMPILE-LIB-FILE |sayBrightlyNT| ASHARP
            |ioHook| FOAM:COMPILE-AS-FILE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (VALUES T T)) |spadTraceAlias|
            FOAM:AXIOMXL-GLOBAL-NAME)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) STRING) MAKE-FULL-CVEC)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) *) |output| |rwriteLispForm| ADDCLOSE
            |reportOpsFromLisplib0| |evalAndRwriteLispForm|
            |reportOpsFromLisplib| |reportOpsFromLisplib1|
            |reportOperations| |isDomainConstructorForm| SAYBRIGHTLYNT1
            |isDomainForm| COERCE-FAILURE-MSG |ncloopInclude|
            |mkAtreeWithSrcPos| |spadcall2| |ppPair|
            |makeLongTimeString| |makeLongSpaceString|
            |readData,xdrRead1| |xdrRead| |writeInputLines|
            /TRACELET-PRINT MONITOR-PRINARGS-1 QUOTIENT |set1|
            |displaySetOptionInformation| |nrtEval| |evalSlotDomain|
            |outputLispForm| |sayErrorly| |saturnSayErrorly|
            |makeStream| |spleI1| |npsystem| MONITOR-PRINVALUE
            |handleParsedSystemCommands| PRINMATHOR0
            |handleTokensizeSystemCommands| |tokenSystemCommand|
            |intSayKeyedMsg| |handleNoParseCommands| MONITOR-PRINT
            |intloopReadConsole| |ncloopCommand| DEFSTREAM
            |htCommandToInputLine,fn| SPAD-SAVE /TRACE-2
            |formArguments2String,fn| SUFFIX |getMinimalVarMode|
            |spadTrace| COMP_QUIETLY_USING_DRIVER VMLISP::COPY-FILE
            VMLISP::COPY-LIB-DIRECTORY |printTypeAndTime| |xdrWrite|
            |printTypeAndTimeNormal| |ncEltQ| |msgText| |printMap1|
            |replaceNamedHTPage| |popUpNamedHTPage| |fortError| |print|
            $FCOPY |sayKeyedMsg| COMPILE-DEFUN |inclHandleBug|
            |ncloopInclude1| |ScanOrPairVec| |patternCheck,subWild|
            |ncConversationPhase| |displayProperties|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) T) PRINT-FULL MAKE-INPUT-FILENAME
            |sayBrightly| |sayBrightlyI| MONITOR-ADD BLANKS |interpret|
            |centerAndHighlight| |simpHasPred| CATCHALL
            |getConstructorExports| TAB |defaultTargetFE|
            |LAM,EVALANDFILEACTQ| |F,PRINT-ONE| |desiredMsg|
            |centerNoHighlight| MATCH-NEXT-TOKEN |htFile2InputFile|
            VMLISP::MAKE-FULL-NAMESTRING |fillerSpaces|
            |printRecordFile| |htFile2RecordFile| |pfSymb|
            BOOTTRAN::BOOTTOCL MAKE-HASHTABLE MAKE-FILENAME MACERR
            |inputFile2RecordFile| PRETTYPRINT |pfExpression|
            MATCH-CURRENT-TOKEN GET-BOOT-IDENTIFIER-TOKEN VMREAD
            |pfSymbol| PRETTYPRIN0)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (*)) MACRO-MISSINGARGS MACRO-INVALIDARGS)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) T) |lnSetGlobalNum| |reportSpadTrace|
            |prefix?| |spadTrace,isTraceable| PAIR
            |isNestedInstantiation| |traceOptionError| |hasOption|
            |putDependencies,removeObsoleteDependencies|
            |mungeAddGensyms| |incTrunc| LENGTHENVEC |domainEqualList|
            |ifCond| |getMapBody| |ScanOrPairVec,ScanOrInner|
            |incStream| |getAliasIfTracedMapParameter| |mkValCheck|
            GETREFV32 |mkValueCheck| |getConstructorOpsAndAtts|
            SAYBRIGHTLY1 |loadLibIfNecessary| |compileBody| |addDefMap|
            MONITOR-WRITE |clearDependencies| |putDependencies|
            |makeNewDependencies| |domainEqual| |findLocalVars|
            |hashTypeForm| FOAM:|printDFloat| |breaklet| |mkLocalVar|
            |oldAxiomCategoryHashCode| |newHasCategory| |getLisplib|
            FOAM:|printSFloat| |makeLocalModemap| FOAM:|printBInt|
            |convertOpAlist2compilerInfo,formatSig| FOAM:|printSInt|
            |processInteractive| |oldAxiomCategoryParentCount|
            |oldAxiomDomainDevaluate| |unloadOneConstructor|
            FOAM:|printString| |getLocalVars| |SExprToDName|
            |mkAutoLoad| FOAM:|printChar| |lazyOldAxiomDomainHashCode|
            |makePathname| |evalSharpOne| |simplifyMapPattern| GLESSEQP
            LEXLESSEQP |substInOrder| |getLisplibNoCache|
            |saveDependentMapInfo| |autoLoad| ASSOCIATER
            |systemDependentMkAutoload| |makeRuleForm|
            |NRTdescendCodeTran| |formatOpSymbol| |linearFormatForm|
            |readLib| |interpMap| |putBodyInEnv| |dcOpPrint|
            |formatOperation| |depthOfRecursion| |plural|
            |getEqualSublis,fn| |macSubstituteId| SEGMENT
            |queryUserKeyedMsg| |mkMapAlias| |subCopy0| DELASC |wl|
            |subCopyOrNil| |makeByteWordVec2| |deepSubCopy0|
            |deepSubCopyOrNil| |objNewCode| |makeVector|
            |postTranSegment| |sayModemapWithNumber| |rempropI|
            |makeList| |AlistAssocQ| |getI| |subTypes| |ListMemberQ?|
            |formatOpSignature| |functionAndJacobian,DF| |ListMember?|
            |postFlatten| |AlistRemoveQ| |mkAtree3,fn|
            |listDecideHowMuch| |getAtree| |pfCollect|
            |mkAtree1WithSrcPos| |setMsgPrefix| |sublisNQ,fn|
            |transferSrcPosInfo| |lassocSub| |intloopInclude|
            |decideHowMuch| |xdrOpen| |getMapSig|
            |untraceDomainLocalOps| DROP |objSetVal| TAKE |putFTText|
            |defLET1| |bottomUpIdentifier| |Delay| TRUNCLIST /READ
            |bottomUpType| TRUNCLIST-1 |asTupleNewCode| |incAppend|
            |hasPair| |isPointer?| |resolveTM| |next| |getOption|
            |stringMatches?| |wt| |transferPropsToNode| |basicMatch?|
            SUBLISNQ |getMsgCatAttr|
            |untraceDomainConstructor,keepTraced?| |FromTo| SUBB
            |mkAtreeNodeWithSrcPos| |putMode| |getValueFromEnvironment|
            |coafAndDnf| |buildBitTable,fn| CONTAINED |modemapPattern|
            |sort| |CONTAINED,EQ| |CONTAINED,EQUAL| QUOTIENT2 S+
            |getSystemModemaps| INTEXQUO |splitListOn| |readData|
            PREDECESSOR |dnfContains| |objSetMode| DIVIDE2
            |sayKeyedMsgLocal| |isString?| ADDOPERATIONS |wordFrom|
            DELLASOS FOAM:|fiSetDebugger| |asyMkSignature|
            |getAllModemapsFromDatabase| FOAM::|magicEq1| |asTupleNew|
            |canFit2ndEntry| |getModemapsFromDatabase|
            |sayKeyedMsgAsTeX| LASSOC GETALIST |andReduce| |rassoc|
            |isDomainSubst,findSub| |simpBoolGiven|
            |makeCompactDirect1| QLASSQ
            |mkAlistOfExplicitCategoryOps,fn| |makeSF| |pileCtree|
            DAASENAME |insertWOC| |pileForest| |resolveTM1|
            |BooleanEquality| |clearAllSlams,fn| |pileTree|
            |recurrenceError| |npTypedForm| |getCDTEntry| |deepSubCopy|
            |predCircular| |installConstructor| |throwKeyedMsg|
            |assocCircular| |writeLib| |pileForest1|
            |defaultTypeForCategory| |countCircularAlist| |hashType|
            |eqpileTree| ASHARPMKAUTOLOADFUNCTION |isDomainSubst,fn|
            REMOVER |unabbrev1| |allLASSOCs| |npRightAssoc|
            |unabbrevRecordComponent| |unabbrevUnionComponent|
            |resolveTCat| |pfApplication| FOAM:|fputs| |symEqual|
            |insertModemap| FOAM:|fputc| |hasCaty1| |patternVarsOf1|
            |pairList| |printNamedStatsByProperty| |SymMemQ|
            |pvarPredTran| |expandSPADREDUCE| REDUCE-1 |mergeSubs|
            |diffAlist| |undoSingleStep| |commandError| |delete|
            |BesselIAsymptOrder| |optionUserLevelError| |putValueValue|
            |horner| |BesselKAsymptOrder|
            |collectDefTypesAndPreds,addPred| |brutef01| |cbeta|
            |fastSearchCurrentEnv| |rPsiW| |mkLessOrEqual| |putFlag|
            |cpsireflect| |PsiXotic| |pfForin| |BesselJRecur|
            |getArgValueOrThrow| |BesselJAsymptOrder| |BesselJAsympt|
            |pfDefinition| |cgammaG| |getOpArgTypes| |chebstareval|
            |getOpArgTypes,f| |getOpArgTypes1| |besselIback| |f01|
            SPADRREAD |hasCatExpression| |pfQualType| |pfReturnTyped|
            |pfLam| PAIRTRACE |mmCatComp| |pfRule| BPIUNTRACE
            |isTowerWithSubdomain| |argCouldBelongToSubdomain|
            |ordSetDiff| |CONTAINEDisDomain| |coafOrDnf| DO_LET
            |ordUnion| |orDel| |pfSpread| |matchMmSigTar|
            |ordIntersection| |andDnf| |subCopy| |coafAndCoaf| |orDnf|
            |dnfContains,fn| |sublisNQ| |stringChar2Integer|
            |pfFromDom| |concat1| |insert| RBESSELI FLAG |BesselI| RPSI
            |rPsi| |segment1| |dispfortarrayexp| |expression2Fortran1|
            CBESSELI CPSI |cPsi| CHYPER0F1 |fortFormatTypes| |chebf01|
            RBESSELJ |BesselJ| REPEAT-TRAN -REPEAT
            |updateCategoryTableForDomain| |segment2| CBESSELJ
            |fortranifyIntrinsicFunctionName| |condAbbrev|
            |encodeCategoryAlist| |addDomainToTable| |dispfortexpf|
            |dispfortexpj| |updateCategoryTable| |assignment2Fortran1|
            MKPF |PsiAsymptotic| |integerAssignment2Fortran1|
            |asyCattranConstructors| FLAGP |testExtend|
            |formalSubstitute| |asyAbbreviation,chk| MKPF1 MKPFFLATTEN
            /GETOPTION S- |upIFgenValue| |mkCategoryOr|
            |asCategoryParts,build| |simpCategoryOr| |chebeval| S*
            |mergeOr| |PsiEps| |NRTreplaceLocalTypes| |tempExtendsCat|
            |substDomainArgs| |asyDocumentation,fn| |throwEvalTypeMsg|
            |getArgValue1| |FloatError| /EMBED-1 /EMBED-Q GETDATABASE
            |newHasTest,fn| |simpOrUnion1| |newHasTest| |hasCat|
            |asyGetAbbrevFromComments,fn| |catPairUnion,addConflict|
            |quickOr| SETDIFFERENCE |upLispCall|
            |displaySetVariableSettings| |displayDatabase,fn|
            |categoryParts,build| |sayLooking1| SETANDFILE
            |HasCategory| |sayCacheCount| |defLET2| |defLetForm|
            |defISReverse| |oldAxiomCategoryDevaluate| |thisPosIsEqual|
            |after| |errorSupervisor| |redundant| |sameMsg?| ?ORDER
            |filterListOfStrings| DIVIDE |oldAxiomPreCategoryParents|
            |upfreeWithType| |thisPosIsLess| GETL |uplocalWithType|
            |erMsgCompare| |sayErrorly1| |compareposns| |rassocSub|
            |lazyOldAxiomDomainDevaluate|
            |oldAxiomCategoryDefaultPackage| |pfMapParts|
            |macLambdaParameterHandling| |setMsgCatlessAttr|
            |isKeyQualityP| |removeVectorElt| |testBitVector| |center|
            |throwKeyedMsg1| |oldAxiomPreCategoryDevaluate|
            |saturnThrowKeyedMsg| |addDmpLikeTermsAsTarget| |deleteAll|
            |setMsgForcedAttrList| |breakKeyedMsg| CHAR-NE
            |throwPatternMsg| |pfCopyWithPos| |sayPatternMsg|
            |genMpFromDmpTerm| |oldAxiomDomainHashCode| |isPatMatch|
            |oldAxiomPreCategoryHashCode| |rep| |evalLETput|
            FOAM-USER::H-ERROR |undoSteps| |inclFname| |setMsgText|
            |evalLETchangeValue| |keyedSystemError1| |subMatch|
            FOAM:|PtrMagicEQ| |setMsgUnforcedAttrList|
            |saturnKeyedSystemError| |scanIgnoreLine| |putPvarModes|
            |augmentPredCode| /UNTRACE-1 |npsynonym|
            |commandErrorIfAmbiguous| |incDrop| CARCDREXPAND
            |seteltable| |spadUntrace| |satisfiesRegularExpressions|
            |macLambda,mac| |insertPos| |mac0SubstituteOuter|
            |position1| |posend| |queueUpErrors| |macWhere,mac|
            |dollarTran| |compileIs| |putModeSet| |say2PerLineWidth|
            MONITOR-EVALTRAN |makePrefixForm| |evalLET| |getAndSay|
            MONITOR-EVALTRAN1 |sortAndReorderDmpExponents|
            |dcOpLatchPrint| |pfAbSynOp?| MONITOR-GETVALUE
            |syIgnoredFromTo| |computeTargetMode| |mergePathnames|
            |isPatternMatch| |sySpecificErrorHere| |match?|
            |genIFvalCode| |replaceArgDef| |varIsOnlyVarInPoly|
            |replaceArgDef1| |suffix?| |ncloopDQlines|
            |replaceArgDefs1| |streamChop| |phReportMsgs|
            |replaceArgDefs| |processMsgList| NCONC2
            |intloopProcessString| |intloopPrefix?| |addToSlam|
            |intloopInclude1| |checkForFreeVariables| |optionError|
            |positionInVec| |lassocShift| |lassocShiftQ| /TRACE-1
            REMAINDER |testPrin| |replaceSymbols|
            |modemapsHavingTarget| |translateMpVars2PVars|
            |mkIterVarSub| |EqualBarGensym,fn| |npMissingMate|
            |hashCombine| |removeListElt| |has| |formDecl2String|
            |coerceInteractive| |ofCategory| |formArguments2String|
            |coerceInt0| GGREATERP |containedRight| |coerceIntPermute|
            CGREATERP |computeTTTranspositions| /UNTRACE-2
            FOAM::ALLOC-PROG-INFO |isSubTowerOf| |resolveTT;|
            |findSubstitutionOrder?,fn| |equalZero|
            |traceDomainConstructor| |displayOpModemaps|
            |canCoerceExplicit2Mapping| /GETTRACEOPTIONS |canCoerce;|
            SORTBY |canCoerce1| |app2StringWrap|
            |absolutelyCannotCoerce| |tracelet| |canCoerceFrom;|
            |canCoerceUnion| |resolveTT1| |coerceIntCommute| |member|
            |canCoerceFrom0| |isEqualOrSubDomain| |hasCorrectTarget|
            |asySignature| |formJoin1| |declare| |evalCategory|
            |asySig| |declareMap| |asySigTarget| |formatJoinKey|
            |mkObjWrap| |replaceSharps| |asyTypeUnitDeclare|
            |constantInDomain?| |asyCatSignature| |coerceBranch2Union|
            |coerceIntByMap| |canConvertByFunction| |app2StringConcat0|
            FOAM-USER::H-INTEGER |coerceOrConvertOrRetract|
            |mathPrint1| |coerceIntByMapInner| |spadPrint| POSN1
            |valueArgsEqual?| |scanInsert| |pfTyped| |lfrinteger|
            |scanExponent| |augmentPredVector| |asyMapping|
            |clearDependentMaps| |scanCheckRadix| |rePackageTran|
            |pfTagged| ERROR-FORMAT |assoc| |pfBraceBar| |resolveTTAny|
            EFFACE |asyDisplay| |AssocBarGensym| |resolveTTEq|
            |EqualBarGensym| |asyWrap| EMBED |resolveTTCC|
            |resolveTTRed| |asyTypeJoinPart| |optCatch,changeThrowToGo|
            |resolveTTSpecial| |compareTT| |acceptableTypesToResolve|
            |acceptableTypesToResolve1| |mkObj| |resolveTTUnion|
            GETCONSTRUCTOR |stringPrefix?| |asyAbbreviation| |quickAnd|
            |coerceInt| |resolveTMEq1| REMALIST |search|
            |resolveTMSpecial| |getFortranType| LEXGREATERP
            |deleteAssoc| |asyExportAlist,fn| |constructTowerT|
            |asySplit| |spliceTypeListForEmptyMode| |equalOne|
            |resolveTMTaggedUnion| |resolveTMOrCroak| WRAPDOMARGS
            |asyCattranSig| |getConstantFromDomain| |resolveTCat1|
            DEF-IT |asySimpPred| |coerceCommuteTest|
            |getConditionsForCategoryOnType| RPLPAIR |pfParen|
            |term1RWall| SET-LIB-FILE-GETTER |term1RW| WHDEF
            |coerceOrRetract| |resolveTM2| |resolveTMUnion|
            |resolveTMRed| MAKE-DATABASES HPUT* |coerceInt1|
            |pfBracket| |resolveTMEq| |resolveTMRecord|
            |commandUserLevelError| |coerceInt2Union|
            |coerceIntFromUnion| |coerceIntAlgebraicConstant|
            |everyNth| DEF-LET |coerceIntTower| |defLET|
            |coerceRetract| STRING2ID-N |compareTypeLists|
            |recordAndPrint| |applyWithOutputToString| DELDATABASE
            |dqAddAppend| |optCatch,changeThrowToExit| |hyperize|
            |defIS| |dqAppend| |prnd| |interpretTopLevel| |writeData|
            |processInteractive1|
            |optimizeFunctionDef,replaceThrowByReturn|
            |retractByFunction| |canCoerceTower| |testInput2Output|
            |inFirstNotSecond| |canCoerceCommute|
            |printTypeAndTimeSaturn| |pfIdPos| |matSuperList1|
            |replaceLast| DEF-IS-REV |constructM| |coerceIntTest|
            DEF-IS2 |getArgValue| |canCoercePermute| |pfIfThenOnly|
            |matSubList1| |substituteSegmentedMsg| |canCoerceLocal|
            |newCanCoerceCommute| |pfHide| |coerceIntTableOrFunction|
            |sameUnionBranch| |optCatch,hasNoThrows| |constructT|
            $FINDFILE |typeToForm| |pfTLam| MAKEOP |coerceIntSpecial|
            MAKENEWOP |matWList1| |pfMLambda| |pfAssign| LET_ERROR
            |pfReturn| |predicateBitIndex,pn| |pfCoerceto|
            |optimizeFunctionDef,fn| |parseTranCheckForRecord|
            |outputMapTran0| STACK-PUSH |pfWDeclare|
            |updateSymbolTable| |getMinimalVariableTower|
            |pfPushMacroBody| |pfMacro| |scylla| |keyedMsgCompFailure|
            |pfDWhere| |objNew| |npTypedForm1| |pfWrong| MARKHASH
            |bottomUpCompilePredicate| |defIS1| |pfBracketBar|
            |bottomUpPredicate| |npLeftAssoc| |PARSE-Operation|
            PUSH-REDUCTION |pfExit| |parseCases,casefn|
            |deleteAssocWOC,fn| |insertWOC,fn|
            |computeTypeWithVariablesTarget|
            |PARSE-rightBindingPowerOf| |initializeTimedNames| |pfWDec|
            |putTarget| |deleteAssocWOC| |PARSE-leftBindingPowerOf|
            REMOVE-ESCAPES |fortranifyFunctionName| |pfWhere|
            |pushDownOp?| |pfOr| GET-GLIPH-TOKEN |checkArgs| |pfAnd|
            FOAM-USER::H-STRING |opWidth| |getStatement| |pfRestrict|
            RDROPITEMS |sayIntelligentMessageAboutOpAvailability|
            |getBindingPowerOf| |pfSuch| |setBootAutloadProperties|
            CHAR-EQ |union| |pfLp| |mkSuperSub|
            |setBootAutoLoadProperty| |getBasicMode0| |beenHere|
            |exp2FortOptimizeCS1,pushCsStacks| |fortFormatIfGoto|
            MAKEARR1 |mkBootAutoLoad| |mkObjCode| $REPLACE
            |fortFormatTypes1| UNIONQ |intCodeGenCOERCE|
            |canCoerceByMap| |canCoerceByFunction| |isSubDomain|
            |absolutelyCanCoerceByCheating| |matWList| |intersection|
            VMLISP::WRITE-INDEXTABLE |pfRetractTo| |pfTree|
            DEFINE-FUNCTION TRANSLABEL1 TRANSLABEL |agg| |mkFormalArg|
            |canMakeTuple| |addPatternPred| |clearCategoryTable1|
            |makePattern| |HasSignature| |pfPretend| |MappingPrint|
            |parseTypeEvaluateArgs| |pfComDefinition| |findLocalVars1|
            |coerceVal2E| |pfFromdom| |mkAliasList,fn| |pfBrace|
            |objNewWrap| |coerceByFunction|
            |mapDefsWithCorrectArgCount| |createEnum| |outputFormat|
            |EnumPrint| |assertCond| |notCalled| |incCommandTail|
            |sayDroppingFunctions| |nonRecursivePart1|
            PRINT-AND-EVAL-DEFUN |inclmsgFileCycle| |containsOp|
            PRINT-DEFUN |coerceUn2E| EVAL-DEFUN BVEC-MAKE-FULL
            |addCARorCDR| |expandRecursiveBody| |position|
            |displayRule| |UnionPrint| |postFlattenLeft|
            |nonRecursivePart| |mkCircularCountAlist| |inclHandleSay|
            |mkFreeVar| |RecordPrint| |inclHandleWarning| |coerceRe2E|
            |inclHandleError| |putValue| |ncloopPrefix?|
            |globalHashtableStats| |rightJustifyString| BVEC-CONCAT
            |ncINTERPFILE| |keyedSystemError| |funfind| BVEC-EQUAL
            |incRenumberLine| |leftBindingPowerOf| |showInput|
            BVEC-GREATER |incRenumberItem| |rightBindingPowerOf|
            BVEC-AND |patternCheck,wild| |listTruncate|
            VMLISP::PUTINDEXTABLE BVEC-OR |makeGoGetSlot|
            |phIntReportMsgs| |augmentTraceNames| BVEC-XOR |incActive?|
            INITIAL-SUBSTRING BVEC-NAND BVEC-NOR |maskMatch?|
            STOREBLANKS |getProplist| |logicalMatch?| INTERSECTIONQ
            |superMatch?| ESCAPED |phParse| |showInOut|
            |searchCurrentEnv| PARSEPILES |reportCircularCacheStats|
            |deleteLassoc| ADD-PARENS-AND-SEMIS-TO-LINE |removeOption|
            |remHashEntriesWith0Count,fn| |patternCheck,pos|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL FIXNUM) HEAPELAPSED CURRENT-CHAR-INDEX)) 
(PROCLAIM '(FTYPE (FUNCTION NIL (VALUES T T)) GENVAR))
)
