#+:GCL
(progn
  (eval-when (:execute :compile-toplevel :load-toplevel)
                 (proclaim '(optimize (safety 1) (debug 3))))

(IN-PACKAGE "BOOT")
(PROCLAIM '(FTYPE (FUNCTION NIL (*)) FIRST-ERROR))
(PROCLAIM
    '(FTYPE (FUNCTION (T) FIXNUM) LINE-NUMBER FOAM:|ProgHashCode|
            FOAM:|strLength| CHAR2NUM LINE-CURRENT-INDEX
            LINE-LAST-INDEX))
(PROCLAIM
    '(FTYPE (FUNCTION (T) FUNCTION) FOAM::FOAMPROGINFOSTRUCT-FUNCALL))
(PROCLAIM
    '(FTYPE (FUNCTION (T) FOAM:|SInt|)
            FOAM::FOAMPROGINFOSTRUCT-HASHVAL))
(PROCLAIM
    '(FTYPE (FUNCTION ((VECTOR T) (VECTOR T)) T) VGREATERP
            LEXVGREATERP))
(PROCLAIM
    '(FTYPE (FUNCTION (T) (VALUES T T)) FOAM:AXIOMXL-FILE-INIT-NAME
            MONITOR-INFO FILE-GETTER-NAME |fetchKeyedMsg|))
(PROCLAIM '(FTYPE (FUNCTION (T *) (VALUES T T)) |read-line|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T) FIXNUM) FOAM:|SetProgHashCode| QENUM
            QSREMAINDER QSQUOTIENT))
(PROCLAIM '(FTYPE (FUNCTION (T T) (*)) COMPILE-DEFUN))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) *) INTERPSYS-IMAGE-INIT |apprpar1|
            |lazyMatchArg2| |compileIF| |compileADEFBody|
            |matrixBorder| |appvertline| |sayFunctionSelection|
            LOCALNRLIB |hashNewLookupInTable| |newLookupInTable|
            |compileAndLink| |bottomUpForm0| |bottomUpFormRetract|
            |bottomUpFormTuple| |bottomUpForm2|
            |bottomUpFormUntaggedUnionRetract| |bottomUpForm|
            |bottomUpFormAnyUnionRetract| |bottomUpForm3|
            |newLookupInDomain| |appargs1| |apphor| |applpar1|
            |appagg1| |applpar| |apprpar|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) *) |constructorAbbreviationErrorCheck|
            |typeToForm,fn| |charyTrouble1| |oldCompLookup|
            |lookupInDomainVector| |oldCompLookupNoDefaults|
            |basicLookupCheckDefaults| |basicLookup| |interpREPEAT|
            |lazyMatch| |interpIF| |evalUntargetedADEF|
            |evalTargetedADEF| |mkInterpTargetedADEF|
            |compileTargetedADEF| |optCallSpecially| |collectStream1|
            |collectSeveralStreams| |collectOneStream| FOAM:|fputss|
            FOAM:|fgetss| |evalconstruct| APP |slashApp| |inApp|
            |appmat| |makeStatString| |appparu| |appfrac|
            |appHorizLine| |appparu1| |charyTrouble| |overlabelApp|
            |exptApp| |quoteApp| |asytranCategoryItem|
            |asytranDeclaration| |compClam|
            |evalInfiniteTupleConstruct| |evalTupleConstruct|
            |sayLooking| |lookupComplete| |lookupIncomplete| |appsetq|
            |binomialApp| |axiomType| |evalTuple| |patternCheck,mknew|
            |evalForm| |lazyMatchArg| |newExpandLocalTypeArgs|
            |appargs| |overbarApp| |appsc| |appsub| |appagg| |argsapp|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) *) |getValueFromSpecificEnvironment|
            |ncloopInclude0| |mkAtree2| |asytranForm1| |Un2E|
            BUILD-DEPSYS |htMkPath| |RecordEqual| |compiledLookup|
            |asySig1| |errorSupervisor1| |selectOptionLC|
            |asytranFormSpecial| |upwhereMain| |upwhereMkAtree|
            |evalREPEAT| |evalSEQ| |argumentDataError| |evalis|
            |evalQUOTE| |evalIF| |upSetelt| |evalCOLLECT|
            |optSpecialCall| GETOP CARCDRX1 |upTaggedUnionConstruct|
            |upRecordConstruct| |upNullList| |application2String|
            |algCoerceInteractive| |compiledLookupCheck| |stringMatch|
            |charybdis| |recordOldValue| |recordNewValue|
            |sayFunctionSelectionResult| |recordInstantiation|
            |interpret1| |asytranForm| |interpret2| |selectOption|
            |newExpandTypeSlot| |mkIterFun| |interpCOLLECT|
            |newExpandLocalType| |upLETtype| |upLETWithFormOnLhs|
            |newExpandLocalTypeForm| |upNullTuple| |templateVal|
            |augModemapsFromDomain1| |exp2FortSpecial| |parseIf,ifTran|
            |findUniqueOpInDomain| |rewriteMap|
            |newExpandGoGetTypeSlot| |getVal|
            |prepareResults,defaultValue| |asytranApplySpecial|
            |charyTop| |interpLookup;| |NRTcompiledLookup|
            |mapRecurDepth|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) *) |listSort| LOCALDATABASE CONCAT))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T) *) |lisplibError| |mkDiffAssoc|
            |goGetTracer|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T T) *) |makeSpadFun|
            |makeFortranFun|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) *) |upDollarTuple|
            |makeLongStatStringByProperty| |compHash| |addModemap|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) T) |intloopInclude0| |mkNewUnionFunList|
            |get1| |fnameMake| |get0| |spad2BootCoerce|
            |throwKeyedMsgCannotCoerceWithValue| |putI|
            |throwKeyedErrorMsg| |mkUserConstructorAbbreviation|
            |pileForests| |mkUnionFunList| |fnameNew| |coerceIntX|
            |OV2Sy| |nopile| |unabbrevSpecialForms| |Expr2Mp| L2V
            |RecordUnEqual| |P2Uts| |coerceSubDomain| |countParens|
            |Ker2Ker| |unifyStruct| |domArg2| |augmentSub| |get|
            |Complex2Expr| |chebevalarr| |coerceTypeArgs| V2L
            |constrArg| FOAM:|FormatNumber| |NRTcompileEvalForm|
            |Mp2Dmp| |hasSigAnd| |Rm2L| |hasCate| |mkMappingFunList|
            |Scr2Scr| |getLocalMms| |mkEnumerationFunList|
            |inclmsgIfSyntax| |L2Record| |push_form2|
            |retractUnderDomain| |Var2OV| |pfWith|
            |fortFormatLabelledIfGoto| |canCoerceByFunction1|
            |makeCatPred| |push_lform2| |npListofFun|
            |simpHasPred,simpHas| |incPrefix?| |getLocalMms,f|
            |ncSoftError| |upwhereClause| |hput| |mkFortFn|
            |evalMmCond| |insertEntry| |substitute| |lazyDomainSet|
            |exp2Fort2| |incZip| |exp2FortFn| |letPrint| |domainVal|
            |oldAxiomCategoryNthParent| |oldAxiomPreCategoryBuild|
            |exact?| |filterModemapsFromPackages| |evalIsntPredicate|
            |evalIsPredicate| |assignSymbol| |letPrint2| |nextown|
            |upTableSetelt| |substringMatch| ADDASSOC |Dmp2Up|
            |coerceOrFail| |pfLambda| |pfTLambda| |evalCOERCE|
            |lffloat| |coerceOrCroak| AS-INSERT |subVecNodes|
            |getSubDomainPredicate| |sySpecificErrorAtToken|
            |permuteToOrder| |lisplibWrite| |matchTypes|
            |computeTTTranspositions,compress| |upLoopIterIN|
            |coerceOrThrowFailure| |unifyStructVar| |interpCOLLECTbody|
            |pfWIf| |userLevelErrorMessage|
            |moreGeneralCategoryPredicate| RWRITE
            |checkIterationForFreeVariables| |flowSegmentedMsg|
            |commandErrorMessage| |algEqual| |encodeUnion|
            |anySubstring?| |rightCharPosition| |infix?|
            |getOpBindingPower| |infixArgNeedsParens| |npAndOr| QESET
            |restoreHistory2| |npBackTrack| |dcSig| |buildPredVector|
            PUT |buildPredVector,fn| SETDATABASE |ncPutQ|
            |extendsCategoryBasic| |catExtendsCat?|
            |extendsCategoryBasic0| HREMPROP |substSlotNumbers|
            |Expr2Complex| |getFunctionFromDomain| |remprop| ELEMN
            |sayMms| |charPosition| VMLISP::MAKE-ENTRY
            |loadLibNoUpdate| |outputNumber| |isRectangularList|
            |deleteMap| |recordInstantiation1| |displaySingleRule|
            |updateDatabase| |needBlankForRoot| |throwListOfKeyedMsgs|
            |compileDeclaredMap| |Sy2P| |M2Sm| M2M
            |restoreDependentMapInfo| |Sy2NDmp| |longext| |Sy2Var|
            |recordNewValue0| |augmentLisplibModemapsFromFunctor|
            |Var2Gdmp| |addToConstructorCache| |NDmp2domain|
            |compileCoerceMap| |P2Up| |SUP2Up_aux| |recordOldValue0|
            L2M |analyzeNonRecur| |P2Mp| |Var2Mp| |V2Rm|
            |isRectangularVector| |Factored2Factored| |coerceFFE|
            PUTALIST |L2Set| |replaceVars| |Agg2L2Agg| |centerString|
            |Up2P| |makeEijSquareMatrix| |augProplist| |Qf2Qf|
            |Expr2Dmp| |Var2UpS| |Var2Up| I2EI I2OI |P2Expr| |Sm2L| M2L
            |Up2SUP| |Var2Dmp| |coerceDmp2| |Rn2F| |assocCache| |Sy2Mp|
            |Sy2Up| V2M |assocCacheShift| |Agg2Agg| |Qf2F|
            |augProplistInteractive| |SUP2Up| |Sy2OV|
            |lassocShiftWithFunction| |Complex2underDomain| |Rm2V| M2V
            |isOpInDomain| |assocCacheShiftCount| |V2Sm|
            |insertAlist,fn| |Dmp2Expr| |M2Rm| |Sy2Dmp|
            |asyMakeOperationAlist| |Complex2FR| |logH| |Mp2P| |Mp2Mp|
            |Var2OtherPS| |besselIcheb| |L2Sm| |PsiBack| I2PI
            |addBindingInteractive| P2FR |cotdiffeval| |Qf2PF| |Up2Up|
            |Var2SUP| |traceDomainLocalOps| |domain2NDmp|
            |augLisplibModemapsFromCategory| |Mp2Expr| |Rm2Sm| |Up2Mp|
            |coerceDmpCoeffs| |postCollect,finish| OV2P OV2SE
            PRINT-XDR-STREAM |Sm2V| |Up2Dmp| |position,posn|
            |NDmp2NDmp| |asytranApply| |Up2Expr| |addBinding|
            MKPFFLATTEN-1 RPLNODE I2NNI |readLib1| |Mp2FR|
            |getOplistWithUniqueSignatures| |P2Upxs| |Dmp2Mp|
            |Sm2PolyType| |displayModemap,g| |insertAlist|
            |cleanUpAfterNagman| |L2Rm| |orderPredicateItems| |OV2poly|
            |substVars| |Up2FR| |getOpCode| |UnionUnEqual| |UnionEqual|
            |lazyOldAxiomAddChild| |orderPredTran| SPADRWRITE
            SPADRWRITE0 |upStreamIterIN| |MappingUnEqual|
            |asGetExports| |clngamma| |getCatForm| |BesselasymptA|
            |oldAxiomCategoryBuild| |ncHardError| |PsiAsymptoticOrder|
            |mac0Define| |spadTraceAlias| |clngammacase23|
            |clngammacase1| PROPERTY |oldAxiomAddChild|
            |NRTisRecurrenceRelation| |evalMmCond0| |chebstarevalarr|
            |chebf01coefmake| |keyedMsgCompFailureSP|
            |coerceConvertMmSelection;| |expandTypeArgs|
            |PiMinusLogSinPi| |MappingEqual| |expandType|
            |throwKeyedMsgSP| |makeCompilation| |BesselIAsympt|
            |makeAspGenerators| |mac0InfiniteExpansion| |isEltable|
            |EnumUnEqual| |IFcodeTran| |writeLib1| |EnumEqual|
            |resolveTTRed2| |resolveTTRed1| |matchUpToPatternVars|
            |isLegitimateMode;| |getConditionalCategoryOfType|
            |Ker2Expr| |Var2P| |Dmp2Dmp| |NRTextendsCategory1|
            |extendsCategory| |Dmp2P| |getArgValueComp| |Set2L|
            |mungeAddGensyms,fn| |Var2FS| |fortFormatHead|
            |patternCheck,equal| OV2OV |NRTgetLookupFunction|
            |getfortarrayexp| |Sm2Rm| |stuffSlot| |selectMms|
            |altTypeOf| |substring?| |maprinSpecial| |stringPosition|
            |putAtree| |matchSegment?| |resolveTMEq2| V2DP SUBSTEQ
            |hasSigOr| |P2Dmp| |L2Tuple| SMALL-ENOUGH-COUNT /MONITORX
            L2DP |hasFileProperty;| |evalMmCat1|
            |coerceTraceFunValue2E| |Rm2M| |Var2QF| |evalFormMkValue|
            |resolveTTEq2| |Mp2Up| |resolveTTEq1| |P2Uls|
            |sigDomainVal| |coerceTraceArgs2E| |Qf2EF| |Var2NDmp|
            MONITOR-PRINARGS |Qf2domain| |lookupInDomainByName|
            |Expr2Up| |pushDownTargetInfo| |sideEffectedArg?|
            |asyCattranOp1| |hasFilePropertyNoCache| |mapLetPrint|
            SUBLISLIS |getMappingArgValue| |Dmp2NDmp|
            |interpRewriteRule| |pushDownOnArithmeticVariables|
            |writeStringLengths| |intloopSpadProcess,interp|
            |npParenthesize| |setMsgUnforcedAttr| |SpadInterpretStream|
            DP2DP |Sm2M| |intloopProcess| |writeXDR| |pfInfApplication|
            |pfIf| |npList| HPUT |rewriteMap1| |fortCall| |rewriteMap0|
            |makeResultRecord| |makeAspGenerators1| |addMap|
            |findLocalsInLoop| |displayModemap|
            |filterAndFormatConstructors| SUBSTRING |outputString|
            |commandAmbiguityError| |findCommonSigInDomain|
            |LargeMatrixp| |transferPropsToNode,transfer| |splitConcat|
            |mkInterpFun| EQSUBSTLIST |mkAtree3| |compSPADSLAM|
            |analyzeMap0| |hasCaty| |pfPushBody| |displayMap|
            |filterListOfStringsWithFn| |get2| |hasAttSig|
            |sublisMatAlist| |displayType| |insertShortAlist|
            |intCodeGenCoerce1| |displayMode| |displayCondition|
            |displayValue| |rread| |rwrite| |makeSpadKernel|
            |addIntSymTabBinding| |mkRecordFunList|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) T)
            |commuteNewDistributedMultivariatePolynomial|
            |commuteSparseUnivariatePolynomial| |charyBinary| |xlMsg|
            |makeConstrArg| |incLine| |split2| |incLude| |logS|
            |xlConStill| |xlConActive| |xlFileCycle| |needStar|
            |xlCannotRead| |xlNoSuchFile| |xlSay| |matchMms|
            |interpLoopIter| |reportFunctionCompilation| |concatApp1|
            |commuteUnivariatePolynomial| |coerceByTable| |orderMms|
            |augmentMap| LOCALASY |commuteComplex| |commuteFraction|
            |commuteSquareMatrix| |coerceDmp1|
            |commuteDistributedMultivariatePolynomial|
            |commuteMPolyCat| |mkCacheVec| |commuteQuaternion|
            |commuteMultivariatePolynomial| |compileRecurrenceRelation|
            |commutePolynomial| |xlOK1| |printLabelledList|
            |selectMms1;| |selectMms2| |prepareResults| |spadify|
            |getArgValueComp2| |fortFormatDo| |resolveTT2|
            |newCompareSig| |getNewDefaultPackage| |msgCreate|
            |bottomUpDefaultCompile| |bottomUpDefaultEval|
            |aggregateApp| |putSrcPos| |analyzeRecursiveMap|
            |analyzeDeclaredMap| |concatTrouble| FINCOMBLOCK))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) T) |reportFunctionCacheAll|
            |condUnabbrev| MAKE-FLOAT |piApp| |domArg| |mkDomPvar|
            |xlPrematureFin| |superSubApp| |xlSkip| |charyEquatnum|
            |getFunctionFromDomain1| |appelse| |evalMmFreeFunction|
            |charySplit| |sigmaApp| |charyMinus| |appext| |appSum|
            |xlCmdBug| |xlIfBug| |stringApp| |xlSkippingFin|
            |push_form3| |xlConsole| |plusApp| |xlOK| |xlPrematureEOF|
            |selectMmsGen| |simpHasSignature| |getReduceFunction|
            |NRTgetMinivectorIndex| |selectDollarMms| |compareSigEqual|
            |catPairUnion| |lazyMatchArgDollarCheck| |npEnclosed|
            |letPrint3| |goGetTracer0| |matchMmSig| |nextown2|
            |canCoerceTopMatching| |allOrMatchingMms| |hasCateSpecial|
            |hasCateSpecialNew| |interpCOLLECTbodyIter| |hasCate1|
            |analyzeMap| |mkIterZippedFun| |mkAndApplyZippedPredicates|
            |upLoopIterSTEP| |interpLoop| |putFileProperty|
            |coerceImmediateSubDomain| |bigopWidth| |mac0MLambdaApply|
            |concatApp| |appInfix| |appChar| |mac0ExpandBody| STRPOSL
            ASHARPMKAUTOLOADFUNCTOR ASHARPMKAUTOLOADCATEGORY
            |defaultTarget| |catchCoerceFailure| |zagApp| |pi2App|
            |appconc| |vconcatapp| |boxLApp| |charySemiColon|
            |primeApp| |appneg| |boxApp| |tensorApp| |bracketApp|
            |nothingApp| |altSuperSubApp| |charyElse| |timesApp|
            |concatbApp| |braceApp| |sigma2App| |put|
            |makeInternalMapName| |indefIntegralApp| |rootApp|
            |semchkProplist| |mergeSort| |mergeInPlace| |augProplistOf|
            |haddProp| |asGetModemaps| |asytranCategory| |putHist|
            |MpP2P| STRPOS |P2Us| |protectedNagCall| |fixUpPredicate|
            |newLookupInAddChain| |hashNewLookupInCategories|
            |collectStream| |upStreamIterSTEP| |constoken|
            |lazyMatchAssocV| |BesselasymptB| |lazyCompareSigEqual|
            |lookupInTable| |compareSig| |prepareData| |lookupDisplay|
            |newLookupInCategories1| |clngammacase2| |binomApp|
            |getFileProperty| |termMatch|
            |getConditionalCategoryOfType1| |matchAnySegment?|
            |evalMmCat| |say2Split| |newLookupInCategories|
            |getArgValue2| |nrunNumArgCheck| |bottomUpDefault|
            |intloopSpadProcess| |selectLocalMms| |printCName|
            |writeMalloc| |printDec| |evalMm| |clearDep1| |stepApp|
            |aggApp| |analyzeUndeclaredMap| |analyzeNonRecursiveMap|
            |hasSig| |centerApp| |intApp| |xLate| |expandDO|
            |srcPosNew| |appsum| |putIntSymTab|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) T) |bracketagglist| |incLine1|
            BUILD-INTERPSYS |xlIfSyntax| |findFunctionInDomain1|
            |compDefineLisplib| |genMapCode| |putMapCode| |nagCall|
            |oldAxiomCategoryLookupExport| |BesselIBackRecur|
            |goGetTracerHelper| |mmCost| |makeFort| |mmCost0|
            |invokeFortran|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) T) |wasIs| |tokConstruct| |categoryParts|
            |pfAdd| |remove| RREAD BPITRACE |pfLeaf| MATCH-TOKEN
            NREMOVE))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T) T) |Mp2MpAux2| /MONITOR
            |writeCFile|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T *) T) |asCategoryParts| TOKEN-INSTALL
            |lnCreate|))
(PROCLAIM '(FTYPE (FUNCTION (T T T T *) T) RPLACSTR))
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T T T T) T) |P2DmpAux|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T) T) |bigopAppAux|
            |findFunctionInCategory| |Mp2SimilarDmp|
            |abbreviationError| |oldAxiomDomainLookupExport|
            |appInfixArg| |findFunctionInDomain| |Expr2Dmp1|
            |Mp2MpAux1| |Mp2MpAux0| |invokeNagman|
            |lazyOldAxiomDomainLookupExport|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T) T) |P2MpAux| |makeFort1|))
(PROCLAIM
    '(FTYPE (FUNCTION NIL *) RESETHASHTABLES
            |describeSetStreamsCalculate| |parse_Label| |parse_Sexpr|
            |parse_Category| |extendConstructorDataTable| |scanS|
            |npPrimary1| FAIL |listConstructorAbbreviations|
            |displayFrameNames| |quit| |executeQuietCommand|
            |npCategory| |npDefinitionItem| |npDefn| |npMacro|
            |/RQ,LIB| /RF-1 /RF RECLAIM |printStatisticsSummary|
            |printStorage| |copyright| |updateHist| |parse_Primary1|
            |disableHist| |readSpadProfileIfThere| /RQ |parse_Sexpr1|
            |generateResultsName| |princPrompt| |parse_Sequence|
            MKPROMPT |queryClients| |parse_Enclosure|
            |sendNagmanErrorSignal| |clearCmdAll| |generateDataName|
            |sendHTErrorSignal| |npRule| |testPage|
            |getInterpMacroNames| HELP |npMDEFinition|
            |leaveScratchpad| |quitSpad2Cmd| |pquitSpad2Cmd|
            |getWorkspaceNames| |pquit|))
(PROCLAIM
    '(FTYPE (FUNCTION NIL T) |oldParserAutoloadOnceTrigger|
            |parse_Expression| |describeSetOutputFormula|
            FRICAS-RESTART |describeOutputLibraryArgs|
            |describeSetLinkerArgs| |npPrefixColon|
            |interpsysInitialization| |npNext| |sayAllCacheCounts|
            |piles| |describeSetFunctionsCache| |processSynonyms|
            YEARWEEK |npReturn| |mkLowerCaseConTable| |npLocalDecl|
            |npTypeVariablelist| INIT-BOOT/SPAD-READER |npVariable|
            |nangenericcomplex| |loadExposureGroupData| |npIterate|
            |npSum| FRICAS-INIT |npLambda| |quadSch| |voidValue|
            |npAmpersand| |interpsys_restart| |npTrap| |spad| |npName|
            |init_parser_properties| |spadThrow| |parse_Prefix|
            |npPPff| |statisticsInitialization| |parse_TokTail|
            |parse_Iterator| |npDiscrim| |npLoop| |npSuch|
            REDUCE-STACK-SHOW |parse_Primary| |parse_LabelExpr|
            |npDefinitionlist| |incConsoleInput| |npFree|
            BUMPCOMPERRORCOUNT |parse_InfixWith| |npSynthetic|
            |parse_With| |npPop1| |npVariablelist| |npProduct|
            |parse_Data| |npSLocalItem| |returnToTopLevel|
            |returnToReader| |parse_Name| |npSemiBackSet|
            |npLocalItemlist| |parse_Import| |parse_Application|
            |parse_Selector| |npPop2| POP-REDUCTION
            |parse_IteratorTail| |inclmsgIfBug| |currentSP| |npPop3|
            |npAtom2| |getIntrinsicList| |npBreak|
            |npQualifiedDefinition| |pfNothing| |npInterval|
            |initialiseIntrinsicList| |npItem| |npLocalItem|
            |npQualDef| |pop_stack_1| |pop_stack_2| |scanNegComment|
            |startsNegComment?| |newFortranTempVar|
            |mkOutputConsoleStream| |scanComment| |npConstTok|
            |startsComment?| |scanKeyTableCons| |scanEsc| |asList|
            |genTempCategoryTable| /TRACEREPLY |allConstructors|
            |scanPunCons| |spadPrompt| |terminateSystemCommand|
            TERSYSCOMMAND |frameNames| |updateInCoreHist|
            WRITE-BROWSEDB |coercionFailure| |scanDictCons|
            |syGeneralErrorHere| |npRecoverTrap| IOSTAT NEXT-LINES-SHOW
            TOP NEXT-LINES-CLEAR |?t|
            |updateFromCurrentInterpreterFrame| |histFileName|
            |rbrkSch| |npState| |lbrkSch| |getParserMacroNames|
            |npPower| |prTraceNames| |genCategoryTable|
            |simpCategoryTable| |nextInterpreterFrame|
            |simpTempCategoryTable| |previousInterpreterFrame|
            |npFirstTok| EMBEDDED |npQualTypelist| |resetInCoreHist|
            |initHistList| |sayShowWarning| |npGives| MONITOR-INITTABLE
            |clearMacroTable| |npComma| MONITOR-RESULTS
            MONITOR-UNTESTED |npSegment| MONITOR-READINTERP |npFromdom|
            |getCodeVector| FOAM:|fiGetDebugVar| MONITOR-AUTOLOAD
            |npPushId| |npAssignVariable| MONITOR-END MONITOR-PERCENT
            |npColon| MONITOR-HELP WRITE-COMPRESS |npRestrict|
            |NRTmakeCategoryAlist| |allOperations| MONITOR-REPORT
            |npExpress1| |npADD| |npConditionalStatement|
            CURRENT-SYMBOL ADVANCE-TOKEN |dcSizeAll|
            INITIAL-GETDATABASE |getInfovecCode| |createInitializers|
            WRITE-WARMDATA WRITE-INTERPDB |popTimedName|
            WRITE-CATEGORYDB WRITE-OPERATIONDB |peekTimedName|
            |saveDependentsHashTable| |statisticsSummary|
            |saveUsersHashTable| |computeElapsedTime| |categoryOpen|
            |browseOpen| |sayNewLine| |operationOpen|
            |createInitializers2| |computeElapsedSpace| |traceReply|
            |displayHeapStatsIfWanted| |interpOpen| |resetCounters|
            |compressOpen| |pop_stack_3| |current_symbol| |next_symbol|
            NEXT-SYMBOL |advance_token| |statRecordLoadEvent|
            |clamStats| |reportCount| |reportInstantiations|
            PREV-LINE-SHOW |credits| |initializeSystemCommands|
            PREV-LINE-CLEAR |parse_AKEYWORD| |parse_KEYWORD| |initHist|
            |oldHistFileName| |clearClams| |clearFrame| |cacheStats|
            |statRecordInstantiationEvent| |reportAndClearClams|
            GET-INTVAL |getParserMacros| |clearHashReferenceCounts|
            |removeAllClams| |poNoPosition| |parse_IDENTIFIER|
            |npDefaultValue| |npDefinitionOrStatement|
            |parse_SPADSTRING| |npSDefaultItem| |npInfixOperator|
            |npInfixOp| |npId| |npBy| |npSigDecl| SPAD_SHORT_ERROR
            SPAD_LONG_ERROR |npType| |npVariableName| |pfNoPosition|
            |npDecl| |npSymbolVariable| |parse_NUMBER| |npDef|
            |npStatement| |waitForViewport| BOOT-SKIP-BLANKS |npImport|
            |npTyping| |displayPreCompilationErrors| |npDefinition|
            |pspacers| |checkWarningIndentation|
            |describeSetOutputOpenMath| |setViewportProcess|
            CURRENT-TOKEN |parse_new_expr| CLEAR-HIGHLIGHT
            RESET-HIGHLIGHT NEXT-TOKEN |npPCff|
            |displayHiddenConstructors| |describeSetOutputAlgebra|
            |intUnsetQuiet| |describeSetOutputHtml| |npEncl|
            |initExposureHash| |npBDefinition|
            |describeSetOutputFortran| |npIterators| |parse_Loop|
            |npWhile| |parse_ReductionOp| |npForIn|
            |makeInitialModemapFrame| |parse_Qualification|
            |describeSetOutputTexmacs| |parse_SemiColon|
            |parse_ElseClause| |parse_Conditional| |npAssignment|
            |parse_Infix| |displayExposedConstructors|
            |describeSetFortTmpDir| |makeConstructorsAutoLoad|
            |spadStartUpMsgs| |parse_PrimaryNoFloat| |npPretend|
            |initializeRuleSets| |updateCurrentInterpreterFrame|
            |parse_SPADFLOAT| |parse_Form| |describeSetOutputTex|
            |initNewWorld| |parse_Reduction| |writeHiFi|
            |resetWorkspaceVariables| |describeSetOutputMathml|
            |intSetQuiet| |parse_ARGUMENT-DESIGNATOR|
            |describeAsharpArgs| |describeInputLibraryArgs|
            |describeSetFortDir| |npCommaBackSet| |parse_String|
            |parse_IntegerTok| |npAssign| |npSignature| |parse_AnyId|
            |parse_Expr1000| |npSigItemlist| |parse_Float| |npPPf|
            |parse_Sequence1| |parse_Seg| |parse_FormalParameter|
            |parse_Exit| |createCurrentInterpreterFrame| |parse_Leave|
            |parse_Quad| |clearCmdSortedCaches| |npTagged|
            |clearCmdCompletely| |parse_VarForm| |resetStackLimits|
            |npExpress| |parse_Return|
            |clearConstructorAndLisplibCaches| |parse_Suffix|
            |displayExposedGroups| |historySpad2Cmd|
            |clearConstructorCaches| |npMatch| |clearCategoryCaches|
            |npPCg| |scanToken| |synonymSpad2Cmd| |npTypeStyle|
            |scanError| |npColonQuery| |scanEscape| |npCoerceTo|
            |scanNumber| |scanString| |npPileDefinitionlist|
            |scanSpace| |fin| |npPPg| |scanPunct| |npRelation|
            |npDefTail| |npMdef| |stopTimer| |clock| |npQuiver|
            |startTimer| |resetSpacers| |ptimers|
            |printableArgModeSetList| |tempLen| CURRENTTIME |random|
            |inclmsgConsole| |inclmsgFinSkipped| |npPileExit|
            TOKEN-STACK-SHOW |npSCategory| |npApplication| |npPrimary|
            |npBPileDefinition| |inclmsgCmdBug| |npExit| |cc| |npVoid|
            |npSQualTypelist| |setOptKeyBlanks| |npSignatureDefinee|
            |spadReply| |intSetNeedToSignalSessionManager| |ncError|
            |npArith| |evalInlineCode| |npAmpersandFrom| |ncIntLoop|
            |npFix| |intloop| CURRENT-SCANNER-CHAR NEXT-SCANNER-CHAR
            NEXT-CHAR |printFirstPrompt?| |npLet| |npDefaultItemlist|
            |npAtom1| |npDollar| |processGlobals| |npPDefinition|
            |boo_comp_cats| |npMDEF| ADVANCE-CHAR |processGlobals1|
            |npExport| |ncTopLevel| |runspad| |npBacksetElse|
            |npDefaultItem| |npDefaultDecl| |npTypeVariable|
            |npSelector| |npApplication2| |getSystemCommandLine|
            |npAssignVariablelist| |interpFunctionDepAlists|
            CURRENT-CHAR |npDisjand| |npLogical| |reportWhatOptions|
            |npLocal| |resetTimers| |npInline|
            |initializeInterpreterFrameRing| |pcounters| |npTerm|
            |writeHistModesAndValues| |npRemainder| |npPrimary2|
            |npTypified| |npAssignVariableName| |npCategoryL|
            |makeClosedfnName| |npQualType| |npSingleRule| |npIterator|
            |npSuchThat| |npSigItem| |intNewFloat|
            |displayWorkspaceNames|))
(PROCLAIM
    '(FTYPE (FUNCTION (*) *) INIT-MEMORY-CONFIG $ERASE
            META-SYNTAX-ERROR CROAK |Undef| |newGoGet| NEXT-LINE))
(PROCLAIM
    '(FTYPE (FUNCTION (T) *) |wrapMapBodyWithCatch| |unVectorize|
            MAKE-DEPSYS |asyJoinPart| |setOutputFormula| |set|
            |agggsub| |setLinkerArgs| |userError| |aggSub|
            |string2BootTree| |setStreamsCalculate| BOOT-LOAD
            |setFunctionsCache| |vConcatSuper| |htCommandToInputLine|
            |subSuper| |quoteSub| |abbQuery| |rootSub| |verbatimize|
            |incHandleMessage| |mkAtree| |tuple2String,fn2|
            |object2String| |phBegin| |formIterator2String|
            |mkParameterList,par2string| |handleLispBreakLoop|
            |simpHasPred,simp| |poGlobalLinePosn| |form2StringLocal|
            |DNameToSExpr1| |IdentityError| |upfree| |fortError1|
            |popSatOutput| |simpHasPred,eval| |sayString|
            |fortFormatCharacterTypes,par2string| |uptypeOf|
            |upiterate| |upSEQ| |fortFormatCharacterTypes,mkCharName|
            |uplocal| |fortPre1| |coerceUnion2Branch|
            |compileTimeBindingOf| |optCallEval| |upQUOTE| |asyCosig|
            |upis| |upIF| |upDollar| |spadcall1| |uperror| |upREPEAT0|
            |simpBool| |upREPEAT| |retract2Specialization|
            |matchMmCond| |evalMmStackInner| |optRECORDELT| |upCOERCE|
            |compileAsharpCmd1| |cd| |upADEF| |unparseInputForm|
            |spleI| |getSlot1FromCategoryForm| |getModeSetUseSubdomain|
            |numOfSpadArguments| |form2StringWithPrens| |form2String|
            |initializeLisplib| |ncAlist| |mkEvalableCategoryForm|
            |interpOnlyCOLLECT| |upCOLLECT| |parseFromString|
            |sayBrightlyNT| |upTARGET| |formJoin2String|
            |abbreviationsSpad2Cmd| |upAlgExtension| |clearCmdParts|
            |systemCommand| |upconstruct| |fixObjectForPrinting|
            |tokType| |superspan| |writifyComplain| |doSystemCommand|
            |form2String1| |unAbbreviateKeyword| |getModeSet|
            |frameSpad2Cmd| |addNewInterpreterFrame|
            |NRTinnerGetLocalIndex| |importFromFrame| |subspan|
            |restoreHistory| |changeHistListLen| |showHistory|
            |saveHistory| |mkEvalable| |npPC| |setHistoryCore|
            |histFileErase| |formatOpType| |reportOpsFromUnitDirectly1|
            |frame| MONITOR-RESTORE |powerOrParen| |coerceOrParen|
            |qTSuper| |undoFromFile| INIT-LIB-FILE-GETTER
            |prefix2String| |formString| |ncTag| |predicateBitIndex|
            |sumOrParen| |productOrParen| |appOrParen| |porigin|
            |optCall| |pfname| |pred2English| |optSETRECORDELT|
            |optSPADCALL| |prefix2String0| |pfFileName| |sayMSGNT|
            |removeIsDomains| |mathprintWithNumber| |formWrapId|
            |parseTran| |obj2String| |aggSuper| |reportOpSymbol|
            |apropos| |normalizeStatAndStringify| |quoteSuper|
            |stopTimingProcess| |getBrowseDatabase| |predTran|
            |mathPrint| |brightPrintHighlight| FILE-RUNNER
            INIT-FILE-GETTER |brightPrintCenter|
            |brightPrintHighlightAsTeX| |binop2String| |agggsuper|
            |brightPrint0AsTeX| |exptSub| |savesystem| |string2Float|
            |brightPrint0| |brightPrintCenterAsTeX|
            |mkAlistOfExplicitCategoryOps| |replaceGoGetSlot| MSORT
            |pf2Sex| |mkAlistOfExplicitCategoryOps,atomizeOp|
            |undoInCore| |compiler| |show| |showSpad2Cmd| |what|
            |pfGlobalLinePosn| |abbreviations| VMLISP::DELETE-DIRECTORY
            VMLISP::GET-IO-INDEX-STREAM |ExecuteInterpSystemCommand|
            VMLISP::GET-INPUT-INDEX-STREAM
            |InterpExecuteSpadSystemCommand| |lnFileName|
            |parseAndEvalStr1| |formJoin2| |npPP| |formatAttributeArg|
            |postTran| |postInSeq| |setOutputOpenMath|
            |mathObject2String| |form2StringAsTeX| |prefix2StringAsTeX|
            |setExposeDrop| |orderList| |formatSignatureArgs0|
            |asyCattran1| |setExposeAdd| |postMakeCons|
            |formatSignatureArgs| |undoCount| |postCategory,fn| COMP370
            |setOutputFortran| |justifyMyType| |setOutputMathml| NMSORT
            |getDomainFromMm| |setOutputHtml| |setOutputTexmacs|
            |setOutputAlgebra| |setOutputTex| |dewritify| |pf2Sex1|
            |dewritify,dewritifyInner| |pfLhsRule2Sex| |setExpose|
            |pfCollectArgTran| |pfDefinition2Sex| |close|
            |pfLiteral2Sex| MAKE-APPENDSTREAM MAKE-INSTREAM
            |clearSpad2Cmd| |systemError| |upDeclare| |upor|
            |asytranEnumItem| |error| |asyCosigType| |withAsharpCmd|
            |history| |linkToHTPage| MAKE-REASONABLE |issueHT|
            |killHTPage| |upand| |startHTPopUpPage|
            |startReplaceHTPage| |upcase| |lispType| |upCOLLECT0|
            |startHTPage| |upCOLLECT1| |c_to_r| |upLET|
            |asyGetAbbrevFromComments| |upLETWithPatternOnLhs|
            |findEqualFun| |mac0InfiniteExpansion,name| |upisnt|
            |intern| |upisAndIsnt| |printBasic| |uphas|
            |predicateBitIndexRemop| |interpOnlyREPEAT| |transSeq|
            |transHasCode| |upREPEAT1| |upbreak| |upTuple| |parseSeq|
            |spadTypeTTT| |uppretend| |upreturn| |systemErrorHere|
            |upequation| |getValue| |parseTranList| |parseLhs|
            |bottomUpElt| |upDEF| |parseNot| |checkPrecision|
            |bottomUp| |fix2FortranFloat| |getUnname| |parseOr|
            |getMsgTag| |parseIf| |bottomUpPercent|
            |makeSimplePredicateOrNil| |parseAnd| |parseLeftArrow|
            |getMsgPos2| |outputTran| |getMode| MAKE-OUTSTREAM
            |newHasTest,evalCond| |poFileName| |asyTypeJoinPartWith|
            |escapeSpecialChars| GET-TOKEN |parseAndEvalStr|
            |prefix2Infix| |asyTypeUnit| MONITOR-PRINTREST
            |parseTransform| |boo_comp1| |asyTypeJoinPartPred| BPINAME
            |getCType| |compQuietly| |compileFileQuietly| OBEY
            |compileQuietly| |simplifyMapPattern,unTrivialize|
            |multiToUnivariate| |combineMapParts| |safeWritify|
            |summary| |matSub| |vectorOfFunctions| |whatSpad2Cmd|
            |functionAndJacobian| |clear| |postBigFloat| |computedMode|
            COMP-2 |postConstruct| |whatConstructors| |mkAtree1|
            |outputTranMatrix,outtranRow| |postin| |postIn| |postSlash|
            |getUnname1| |reportOpsFromUnitDirectly0| |qTSub| |dnf2pf|
            |pfRhsRule2Sex| |editFile| |tabsToBlanks|))
(PROCLAIM
    '(FTYPE (FUNCTION (*) T) |Enumeration0| |spadCompile|
            FOAM::MAKE-FOAMPROGINFOSTRUCT VMLISP::MAKE-LIBSTREAM
            INITROOT |UnionCategory| |RecordCategory| |concat|
            |incLude1| |throwMessage| TOPLEVEL RKEYIDS |incAppend1|
            |next1| |incRgen1| |nextown1| |incIgen1| |incZip1|
            READ-A-LINE IOCLEAR MAKE-STACK |displayCategoryTable|
            MONITOR-RESET MAKE-MONITOR-DATA MONITOR-TESTED
            MONITOR-ENABLE MONITOR-DISABLE MAKE-DATABASE |sum|
            SPAD_SYNTAX_ERROR MAKE-TOKEN MAKE-XDR-STREAM MAKE-REDUCTION
            SAY MOAN |synonym| |runOldAxiomFunctor| |isValidType|
            ENABLE-BACKTRACE |coerceConvertMmSelection| |underDomainOf|
            |dc| |dcSize| |Mapping| |canCoerceFrom| |isLegitimateMode|
            |buildBitTable| |Union| |hasFileProperty| RDEFOUTSTREAM
            RDEFINSTREAM |resolveTT| |getConstantFromDomain|
            |canCoerce| |selectMms1| INTERRUPT $FILEP NEXT-BOOT-LINE
            |EnumerationCategory| MAKE-LINE MAKE-SPAD-KERNEL
            |interpLookup| |findRetractMms|))
(PROCLAIM
    '(FTYPE (FUNCTION (T) CHARACTER) EBCDIC NUM2CHAR LINE-CURRENT-CHAR))
(PROCLAIM '(FTYPE (FUNCTION (T T *) FIXNUM) LINE-NEW-LINE))
(PROCLAIM '(FTYPE (FUNCTION (T) STRING) LINE-BUFFER |make_spaces|))
(PROCLAIM
    '(FTYPE (FUNCTION (T) T) |hasOptArgs?| PREPARSEREADLINE
            |myWritable?| |pfApplicationArg| |getSrcPos| MACROEXPANDALL
            |patternVarsOf| |asyTypeUnitList| |fnameDirectory|
            |mkAtreeExpandMacros| |asyCattran| |fnameExists?|
            |pfLambdaBody| |isInterpMacro| |pfLambdaRets|
            |asyPredTran1| |Record0| |pfLiteralString| |pfLeafToken|
            |pfLiteralClass| |StringToDir| |fnameWritable?|
            |typeIsASmallInteger| |hashCount| |fnameName| |nodeCount|
            |isLeaf| |minimalise,HashCheck| |fnameReadable?|
            |DirToString| |minimalise,min| |setAutoLoadProperty|
            |fnameType| |minimalise| |maximalSuperType| |pileColumn|
            |unabbrevAndLoad| |pileCforest| |insertpile|
            INTERPSYS-ECL-IMAGE-INIT |packageForm?| |pilePlusComments|
            |pilePlusComment| |remWidth| |pileComment| |primeSub|
            |setFortDir| |superSubSub| |countCache| |superSubWidth|
            |enPile| |separatePiles| |firstTokPosn| |setInputLibrary|
            FOAM::TYPE2INIT |lastTokPosn|
            |getPartialConstructorModemapSig|
            VMLISP::LIBSTREAM-INDEXSTREAM |binomSub| VMLISP::VARP
            |sayMath| |asyConstructorArg| |domainForm?|
            |verifyRecordFile| |intWidth| |setHistory| |getCacheCount|
            |isDomainValuedVariable| |asyCatItem| |eq0| |isNameOfType|
            |indefIntegralSub| |setOutputLibrary| |pfRule?| |objMode|
            |evaluateLines| |matWidth| |pfNovalueExpr| |pfNovalue?|
            |pfNotArg| |getConstructorAbbreviation| |tuple2String,fn1|
            |tokPosn| |pfNot?| |c_gamma| |pfOrRight| SEC
            |asyTypeJoinItem| |fracwidth| |pfOrLeft|
            |getImmediateSuperDomain| |pfOr?| |htTrimAtBackSlash|
            |pfAndRight| |pfAndLeft| |npPush|
            |getConstructorUnabbreviation| |SkipEnd?| |overbarSuper|
            |pfAnd?| |pfWrong?| |recordAndPrintTest,fn|
            |pfReturnNoName| |pf0LocalItems| |npEqPeek| |pfLocal?|
            |PsiIntpart| |pf0FreeItems| |augmentLowerCaseConTable|
            |pfFree?| |incFile| |trimString| |gammaRatapprox|
            |pfRestrictType| |npListing| |incRenumber|
            |asyTypeJoinPartIf| |outputOp| |pfRestrictExpr|
            |pfRestrict?| |incBiteOff| |asyCATEGORY| |gammaRatkernel|
            |asTupleAsList| |pfDefinition?| |gammaStirling|
            |pfAssignRhs| |pfIterate| |slashSuper| |r_lngamma|
            |pf0AssignLhsItems| |zagSub| |pfAssign?| |incFileInput|
            |pfTypedType| |KeepPart?| |stringWidth| |c_to_s|
            |typeToInputForm| |pfTyped?| |s_to_c| |pfDoBody| |zagSuper|
            |pfDo?| |bassertNot| |asyTypeJoinPartExport|
            |altSuperSubSuper| |lnrgammaRatapprox| |exp2FortOptimizeCS|
            |pfSuchthatCond| |SkipPart?| |phiRatapprox|
            |exp2FortOptimizeCS1| |pfSuchthat?| |SubstWhileDesizing|
            |nameLen| |pfWhileCond| |asyTypeJoin| |Elseif?|
            |phInterpret| |agggwidth| |addCommas| |pfWhile?| |If?|
            |pfForinWhole| |binomialWidth|
            |exp2FortOptimizeCS1,popCsStacks| |npEqKey| |concatList|
            |phMacro| |mkParameterList| |pf0ForinLhs| |mkMessage|
            |cgammaAdjust| |intSup| |typeToOutputForm| |pfForin?|
            |clearCache| |ncloopIncFileName| |cgammaBernsum|
            |pf0LoopIterators| |inWidth| |loopIters2Sex| |parse_Expr|
            |pfLoop1| |concatSub| |displayLines| |pfLoop?|
            |ncloopPrintLines| |r_gamma| |fortran2Lines| |pfExitExpr|
            |incNConsoles| FOAM:|fiStrHash| |pfPrintSrcLines|
            |fracsuper| |dispStatement| |pfExitCond| |mkLineList|
            |timesWidth| |statement2Fortran| |pfExit?| |negintp|
            |fortFormatIntrinsics| |pfFromdomDomain|
            |simpHasPred,simpDevaluate| |intloopEchoParse| |pi2Width|
            |pfFromdomWhat| |queryUser| |parse_LedPart| |blankList|
            |checkLines| |opTran| |parse_NudPart| FOAM:|fiGetDebugger|
            |changeExprLength| |pfFromdom?| |form2FenceQuote|
            |fortranCleanUp| |pfPretendType| |pfFree|
            |exp2FortOptimize| |pfPretendExpr| |coerceSpadFunValue2E|
            |npAdd| |fortPre| |pfPretend?| |getBpiNameIfTracedMap|
            |segment| |pfCoercetoType| |simpOrUnion| |Skipping?|
            |DNameToSExpr| |exp2Fort1| |pfCoercetoExpr| |incClassify|
            |fortFormatTypes,unravel| |pfCoerceto?| |pfSequenceArgs|
            |fortexp0| |pfTaggedExpr| |Top?| |expression2Fortran|
            |bubbleConstructor| |pfTaggedTag| |npParse| |pfTagged?|
            |incCommand?| |asIsCategoryForm| |CompStrToString|
            |pfIfElse| |closeOldAxiomFunctor| |pfDocument| |pfIfThen|
            |inclmsgConActive| |exp2FortOptimizeArray| |pfIfCond|
            |inclmsgNoSuchFile| |simpCattran| |mathprint| |Else?|
            |hashCode?| |inclmsgPrematureEOF| |mkMat| RPACKFILE
            |pushSatOutput| |hasIdent| |typeOfType| |pfBreak|
            FOAM::INSERT-TYPES |isNewWorldDomain|
            |fortFormatCharacterTypes|
            |fortFormatCharacterTypes,mkParameterList2| |upwhere|
            |instantiate| |pfNovalue| RECOMPILE-LIB-FILE-IF-NECESSARY
            |asyConstructorArgs| |pfWithBase| |clearCategoryTable|
            |fortSize,elen| LIBSTREAM-DIRNAME |abbreviation?|
            |pfAssignLhsItems| |pfEnSequence| |asyTypeMapping|
            |pfSecond| |getfortexp1| |optRECORDCOPY|
            |mkCategoryExtensionAlistBasic| |isLocalPred|
            |DNameFixEnum| |pfLoop| |fortExpSize| |npItem1| |pfDo|
            PNAME |removeConstruct| |pf0WrongRubble| |optCons|
            |pfWrongRubble| |npAngleBared| |pf0TLambdaArgs| MAKE-BVEC
            |getCategoryExtensionAlist0| |lfid| |pfTLambdaArgs|
            |checkType| |eval| |asyType| |pfWhile| |showCategoryTable|
            |StreamNull| |pfReturnFrom| |displayLines1| |rMkIstream|
            |pfTupleParts| |rMkOstream| |getConstrCat| |npRestore|
            NUM2USTR |lfcomment| |altSeteltable| |opIsHasCat|
            |pfTupleList| |pfSemiColon| |scanTransform| |pf0WithWithin|
            |incRgen| |pfWhereContext| |interactiveModemapForm|
            |lfnegcomment| |scanKeyTr| |pf0AddBase| |lfkey| |pfAddBase|
            |pfDocument?| |optimize,opt| READLINE |scanPossFloat|
            |pfAttributeExpr| |functionp| |scanCloser?| |pfImport|
            |retract1| |optimize| FOAM:|printNewLine| |keyword|
            |pfSemiColon?| |getTraceOptions| |pfMLambdaArgs| |subrname|
            |keyword?| |pfDocumentText| |pfTaggedToTyped|
            |getTraceOption| |incIgen| |pfTLambda?| REMDUP |optIF2COND|
            |simpCatPredicate| |npNull| |pfForinLhs| |optXLAMCond|
            |lineoftoks| |pfSexpr,strip| |pfDWhereExpr|
            |replaceSharpCalls| |pfId| |pfWIfCond| |addToCategoryTable|
            |orderBySlotNumber| |nextline| |macrop| |evalMmStack|
            |optCatch| |pfAddAddin| |lfinteger| |pfWIfElse|
            |pfExportDef| |optQSMINUS| |evalLoopIter| |lferror|
            |compileAsharpArchiveCmd| |pfSexpr| ASSOCLEFT |rdigit?|
            ASSOCRIGHT |pfNot| |pfWrongWhy| |form2FenceQuoteTail|
            |library| |pfHidePart| |pfInline| |pfRetractToType|
            |pfDWhereContext| |pathnameName| |hashString| |pf0WithBase|
            |pfWIf?| |containsVars| |pfRetractTo?| |pfWDeclareDoc|
            |mkNestedElts| |killNestedInstantiations| |pfTransformArg|
            LINE-PRINT |iterVarPos| |lfstring| |pfFreeItems|
            |pfTaggedToTyped1| |lfspaces| DATABASE-PREDICATES
            |pfInlineItems| |loadLibIfNotLoaded|
            |pfDefinitionSequenceArgs| |formatOperationAlistEntry|
            |scanW| DATABASE-DOCUMENTATION |pfWIfThen| |pfCheckArg|
            |isHomogeneousList| |pfHide?| DATABASE-CONSTRUCTORFORM
            |pfCheckId| |categoryParts,exportsOf| |upStreamIters|
            |formatIf| |pfAttribute?| |transformOperationAlist|
            |pfListOf?| |containsLocalVar| |pfSemiColonBody|
            |pfDefinitionLhsItems| |isOkInterpMode|
            |pfStringConstString| SQUEEZE |pfFlattenApp|
            |pfLoopIterators| |pfCollect1?| |tuple2String|
            |pf0FlattenSyntacticTuple| DATABASE-SOURCEFILE
            |isTaggedUnion| |npTrapForm| |compressHashTable|
            |pfAddAddon| |pfWDeclareSignature| |pfSymbolVariable?|
            |reassembleTowerIntoType| |getFunctorOpsAndAtts|
            |pfLambdaArgs| LINE-ADVANCE-CHAR |c_lngamma|
            |pf0ExportItems| |getCategoryOpsAndAtts| |pfSuchthat|
            |optCONDtail| LINE-NEXT-CHAR |pfExportItems| |pfWithWithon|
            |optPredicateIfTrue| |canRemoveIsDomain?| |pfSourceStok|
            |unInstantiate| |pfTLambdaBody| |optSEQ,getRidOfTemps|
            LINE-PAST-END-P |findSubstitutionOrder?| |domainZero|
            |lisplibDoRename| |pfComDefinitionDef| |mkZipCode|
            |readLibPathFast| |pfExpression?|
            |updateCategoryTableForCategory| |optSEQ,SEQToCOND|
            |npMoveTo| |finalizeLisplib| |pfCollectVariable1| |optSEQ|
            |pf0ImportItems| LINE-AT-END-P |sayBrightly| |pfExitNoCond|
            |expandREPEAT| |displayTranModemap| |pfTLambdaRets|
            |listOfPredOfTypePatternIds| |pfIterateFrom|
            |pfQualTypeQual| |optSEQ,tryToRemoveSEQ| |formatSignature|
            |c_to_rf| |dumbTokenize| |pf0CollectIterators|
            |sayBrightlyI| |pfComDefinitionDoc| |optSuchthat| |tokTran|
            |updateCategoryFrameForConstructor| |pfTupleListOf|
            |clearTempCategoryTable| |sayMessage|
            |updateCategoryFrameForCategory| |interpIter| FOAM:|Halt|
            |loadIfNecessaryAndExists| |mkCategoryExtensionAlist|
            |formTuple2String| |notCoaf| |addTraceItem|
            |decomposeTypeIntoTower| |compileAsharpCmd| |sayMSG2File|
            |list1| STACK-P |eq2AlgExtension| |stupidIsSpadFunction|
            |formatSignature0| |stepWidth| |readHiFi| |loadFunctor|
            WIDTH |bright| STACK-STORE |formatModemap| |sumoverlist|
            STACK-SIZE |sigma2Width| |sayMSG| STACK-TOP
            |clearParserMacro| STACK-UPDATED |cleanUpSegmentedMsg|
            |pi2Sup| |macApplication| |isFormalArgumentList| |height|
            |formatArgList| |reportOpsFromUnitDirectly|
            |pf0ApplicationArgs| |linearFormat| |pfMLambda?|
            |say2PerLine| |npZeroOrMore| STACK-CLEAR |specialChar|
            |pfApplicationOp| |pfUnSequence| |pfListOf|
            |pf0SequenceArgs| |macId| COPY |pfAppend| DOWNCASE |keyp|
            |pfSourcePosition| |markUnique| |pfSequence?| |mac0Get|
            |getCategoryExtensionAlist| |NRTaddInner| |pfIdSymbol|
            |putWidth| |form2Fence| MONITOR-DATA-COUNT |piSub|
            MONITOR-DATA-NAME |form2Fence1| MONITOR-DATA-SOURCEFILE
            |boxSub| |tokPart| MONITOR-APROPOS |primeSuper|
            MONITOR-SPADFILE |superSubSuper| MONITOR-PARSE |maprinRows|
            MAKE-HASHTABLE MONITOR-INCR |maprinChk| MONITOR-FILE
            |npDDInfKey| |unabbrev| |maPrin| |npInfKey| MONITOR-DELETE
            |deMatrix| |constructorName| |aggwidth| |ltrace|
            MONITOR-LIBNAME |trace| |sigmaSub|
            FOAM::PROCESS-IMPORT-ENTRY |abbreviate| MONITOR-DIRNAME
            |absym| |findFrameInRing| |tensorWidth| MONITOR-CHECKPOINT
            |isQuotient| |deleteFile| MONITOR-DATA-MONITORP
            |largeMatrixAlist| |makeOldAxiomDispatchDomain|
            |changeToNamedInterpreterFrame| |concatTrouble,fixUp|
            DATABASE-USERS |emptyInterpreterFrame| |syminusp|
            |npProcessSynonym| |npTuple| |processSynonymLine|
            |getOpSegment| |binomWidth| |printSynonyms|
            |altSuperSubSub| |script2String| NEXT-TAB-LOC
            MONITOR-DATA-P VMLISP::LIBSTREAM-MODE |ppTemplate|
            FOAM:|fiSetDebugVar| MONITOR-NRLIB |formatModemap,fn|
            INDENT-POS EXPAND-TABS |makeCompactDirect1,fn|
            DATABASE-PARENTS |orderBySubsumption| NONBLANKLOC
            MONITOR-DECR |depthAssocList| MONITOR-EXPOSEDP
            |halfWordSize| UNSQUEEZE BLANKP
            |optimizeFunctionDef,removeTopLevelCatch| |addConsDB|
            DATABASE-DEPENDENTS |opt-| |makeSpadConstant|
            |formatSlotDomain| |optEQ| UPCASE
            |removeAttributePredicates,fn| DATABASE-ANCESTORS |dcCats1|
            |makeLeaderMsg| DATABASE-NILADIC |makeCompactSigCode|
            DATABASE-OBJECT |optimizeFunctionDef| |nodeSize|
            |getMsgToWhere| DATABASE-MODEMAPS DATABASE-OPERATIONALIST
            |vectorSize| |formatPredParts| DATABASE-CONSTRUCTORCATEGORY
            |%origin| DATABASE-CONSTRUCTORMODEMAP DATABASE-COSIG
            |optMkRecord| FBPIP |ppos| DATABASE-CONSTRUCTORKIND
            |optCond| |bitsOf| DATABASE-ABBREVIATION |getInfovec|
            SHOWDATABASE |TruthP| |numberOfNodes|
            DATABASE-DEFAULTDOMAIN |optLESSP| PREPARSEREADLINE1
            |optMINUS| |pfLinePosn| |makeCompactDirect| ATENDOFUNIT
            |NRTcatCompare| |texFormat| DATABASE-CONSTRUCTOR
            |pfCharPosn| |breakIntoLines| |pfImmediate?| SKIP-IFBLOCK
            |makeDomainTemplate| |dispfortexp| |depthAssoc|
            |texmacsFormat| |ncParseFromString| FOAM:|formatDFloat|
            INFIXTOK |mathmlFormat| UNEMBED |pfNoPosition?|
            FOAM:|formatSFloat| PARSEPRINT
            |removeAttributePredicates,fnl| FOAM:|formatBInt| PREPARSE1
            |infovec| FOAM:|formatSInt| |predicateBitRef| |template|
            |matSubList| |outputConstructTran| |srcPosDisplay|
            |outputTranSEQ| |srcPosColumn| |notDnf| |outputTranRepeat|
            |srcPosSource| |%id| |updateTimedName| |outputTranReduce|
            |%fname| |isInternalFunctionName| |getMsgInfoFromKey|
            |outputTranCollect| VMLISP::PROBE-NAME COMP |getLineText|
            |outputTranIf| |parseHasRhs| |outputMapTran| LIST2VEC
            |prTraceNames,fn| |linearFormatName| |bnot| |From|
            |outputTranMatrix| |printNamedStats| |record2String|
            |b2dnf| |postError| |alreadyOpened?| |flattenOps| |list2|
            |toFile?| |bor| |parseTran,g| |toScreen?| |alqlGetParams|
            |getFlag| |parseInBy| |compAndDefine| |line?|
            |overbarWidth| |vConcatWidth| |parseIn| |leader?| |matLSum|
            |transformREPEAT| |getStFromMsg| |stringer|
            |indefIntegralWidth| |transformCollect| |band| |exptSuper|
            |bassert| |containsVars1| |domainOne| |piSup| |roundStat|
            IS-CONSOLE |reduceDnf| |letWidth| |subSub| |pfWDeclare?|
            |mathPrintTran| |untraceAllDomainLocalOps| |pfLocalItems|
            SET-FILE-GETTER |pfImportItems| |alqlGetKindString|
            FOAM-USER::|AXL-spitSInt| |pfDWhere?| |removeAttributes|
            |overlabelSuper| |erMsgSep| |vConcatSub|
            |timedOptimization| MAKE-ABSOLUTE-FILENAME |expr2String|
            |pfQualTypeType| |startTimingProcess| |pfAdd?|
            |resolveTypeListAny| |concatSuper| |pfWithWithin|
            |atom2String| |pfExpr?| |rootWidth| |varsInPoly| |lispize|
            |pfWith?| |setExposeAddConstr| |rootSuper|
            |reportHashCacheStats| |pfInline?| |sigmaSup|
            |timedEvaluate| |simplifyMapConstructorRefs| |pfExport?|
            |sayWidth,fn| |formatAttribute| |pfDeclPart?| DATABASE-P
            |sigma2Sub| |mkHashCountAlist| |pfComDefinition?|
            |extsuper| |splitIntoBlocksOf200| |displayCacheFrequency|
            |pfRetractToExpr| |brightPrintRightJustify|
            |outputTranIteration| |pfImport?|
            |makeLazyOldAxiomDispatchDomain| |outputTranIterate|
            |alqlGetOrigin| DATABASE-SPARE |pi2Sub| |brightPrint1|
            |primeWidth| |timedEVALFUN| |matrix2String| |tabber|
            |stepSub| |pfTyping?| |blankIndicator| |significantStat|
            |pf0TypingItems| |sayMathML| |charyTopWidth|
            |pfTypingItems| |string2Words| |matLSum2| |minusWidth|
            LISTOFATOMS |numberOfEmptySlots| |altSuperSubWidth|
            |formCollect2String| |nothingSub| LASTATOM |pushTimedName|
            |isFreeVar| |sigma2Sup| |Zeros| |displayOperations|
            |binomialSuper| VMLISP::FLAT-BV-LIST |pfQualType?|
            |addBlanks| |timedAlgebraEvaluation| |mkAliasList|
            |noBlankBeforeP| |concatWidth| |analyzeMap,f|
            |isBinaryInfix| |pfCheckInfop| |noBlankAfterP|
            |exptNeedsPren| |splitSayBrightlyArgument| OPTIONAL
            |sayBrightlyLength| |makeInternalMapMinivectorName|
            |splitListSayBrightly| |mkDatabasePred| |texFormat1|
            |outputDomainConstructor| |sayHtml| ACTION |recordFrame|
            |nothingSuper| |listOfPatternIds| |synonymsForUserLevel|
            |exptWidth| |isLocalVar| |asyMkpred| |typeTimePrin|
            |writify,writifyInner| |widthSC| |spadClosure?| |fracsub|
            |clearClam| |boxSuper| |intnplisp| |initCache| |piWidth|
            |sayBrightlyLength1| |formal2Pattern| |unwritable?|
            |dbSpecialDisplayOpChar?| PREV-LINE-SET |readSpad2Cmd|
            |nothingWidth| VMLISP::EQUABLE GET-SPECIAL-TOKEN
            |pathnameTypeId| |stepSuper| |asyShorten| |pp2Cols|
            |concatbWidth| |macroExpanded| |slashSub| |incString|
            VMLISP::LIBRARY-FILE |entryWidth| |flattenSignatureList|
            |undoChanges| |remHashEntriesWith0Count|
            GET-SPADSTRING-TOKEN |createAbbreviation|
            MAKE-ADJUSTABLE-STRING |isDefaultPackageName|
            |displaySpad2Cmd| |updateSourceFiles| |rightTrim| |display|
            |asMakeAlistForFunction| |loadSpad2Cmd| GET-SPADNUM-TOKEN
            |load| |leftTrim| |packageTran| |list3|
            |constructor2ConstructorForm|
            |displayOperationsFromLisplib|
            GET-ARGUMENT-DESIGNATOR-TOKEN |displayHashtable|
            |pfPlaceOfOrigin| |poPlaceOfOrigin| |dewritify,is?|
            |lnLocalNum| |parse2Outform| |poNoPosition?| |str2Tex|
            |editSpad2Cmd| |poIsPos?| |asyLooksLikeCatForm?| |wrap|
            |edit| |workfilesSpad2Cmd| |pfPosn|
            FOAM::FOAMPROGINFOSTRUCT-P GENSYMP |str2Outform|
            |poFileName?| |workfiles| |lnFileName?|
            |clearCategoryCache| |pfNopos?| |lnGlobalNum| |pfParts|
            |pfSourcePositionlist| |pfSourcePositions| |lnString|
            |asyUnTuple| |lnExtraBlanks| |poImmediate?|
            |CDRwithIncrement| SPAD-KERNEL-OP
            |formatUnabbreviatedTuple| |lnImmediate?|
            |parseAndEvalToStringForHypertex| |pfFileName?|
            |parseAndEvalToString| |formatUnabbreviated|
            |pfPosImmediate?| |parseAndEvalToHypertex|
            |formatUnabbreviatedSig| |freeOfSharpVars|
            |parseAndEvalToStringEqNum| |pfSourceToken| STACK-POP
            |parseAndInterpret| |pfFirst| BUMPERRORCOUNT |asyTypeItem|
            |pfGetLineObject| |protectedEVAL| |sayModemap|
            |parseAndInterpToString| |dropLeadingBlanks| |asyComma?|
            |pr| |compileInteractive| |form2StringList| MAKE-CVEC
            |listOfSharpVars| TOKEN-P |removeIsDomainD| SPAD_ERROR_LOC
            |opOf| |postIf| |formatMapping| BOOT-TOKEN-LOOKAHEAD-TYPE
            |intpart| |clearSlam| XDR-STREAM-P |removeZeroOne|
            |postWith| |length1?| TOKEN-SYMBOL |tuple2List| TOKEN-TYPE
            TOKEN-NONBLANK |saveMapSig| |postCapsule|
            |matrix2String,outtranRow| |S_process|
            |setExposeDropConstr| XDR-STREAM-HANDLE |postBlockItemList|
            |asyArgs| |spool| |postBlockItem| |dollarPercentTran|
            |setExposeDropGroup| |postMDef| PREPARSE
            |translateYesNo2TrueFalse| |pfSequenceToList| |postExit|
            PRINT-PACKAGE |moveORsOutside| CACHEKEYEDMSG |Identity|
            |ncParseAndInterpretString| INITIALIZE-PREPARSE
            |setExposeAddGroup| XDR-STREAM-NAME |concatenateStringList|
            |postTranList| /VERSIONCHECK |npPileBracketed| CHARP
            |postTransformCheck| |printAsTeX| |setOutputCharacters|
            |recordAndPrintTest| |postcheck| TOKEN-PRINT
            |isExposedConstructor| |undo| |asyCattranOp| |postAdd|
            |initializeSetVariables| |parse_GliphTok| |postReduce|
            |isPatternVar| |read| |formatOpConstant| |mkCircularAlist|
            GETREFV |getFirstArgTypeFromMm| |splitIntoOptionBlocks|
            |push_lform0| |stripSpaces| |translateTrueFalse2YesNo|
            |isExistingFile| |object2Identifier| |pathname?|
            |compileSpadLispCmd| |astran| |getKeyedMsg|
            |form2StringWithWhere| |pathnameType| |segmentKeyedMsg|
            |sayWidth| |isFreeFunctionFromMm| |pathnameDirectory|
            |validateOutputDirectory| QSORT |help| |asMakeAlist|
            |LZeros| PLACEP |helpSpad2Cmd| |npBraced|
            |removeZeroOneDestructively| REDUCTION-RULE
            |getUsersOfConstructor| |setFortTmpDir| |pfOp2Sex|
            |length2?| |npSemiListing| |pmDontQuote?| REDUCTION-VALUE
            |clearAllSlams| |asyParents| |npQualified|
            |getOplistForConstructorForm| |npLetQualified| |pfSymbol?|
            |push_form0| |asyDocumentation| |removeUndoLines|
            |asyConstructorModemap| |zeroOneTran|
            |getOperationAlistFromLisplib| |pfCollectBody| HKEYS
            |isDomainSubst| |setAsharpArgs| |pfCollectIterators|
            |transOnlyOption| |pfCollect?| |stackTraceOptionError|
            |parse_NBGliphTok| |npboot| |pfRule2Sex| |dropPrefix|
            |nplisp| |pfLambda2Sex| |domainToGenvar| |stripLisp|
            |asytran| |pfCollect2Sex| |asyPredTran|
            |closeInterpreterFrame| VMLISP::GET-INDEX-TABLE-FROM-STREAM
            MESSAGEPRINT |pfSequence2Sex| |getDependentsOfConstructor|
            MESSAGEPRINT-1 |npWConditional| |pfApplication2Sex|
            |isFreeFunctionFromMmCond| |pfTweakIf| |hackToRemoveAnd|
            |newHelpSpad2Cmd| MESSAGEPRINT-2 |npConditional|
            |pfWhereExpr| |pf0WhereContext| |getDomainHash|
            |variableNumber| |pfIterate?| |getConstructorModemap|
            |devaluateList| |clearCmdExcept| |pfReturnExpr| |pfReturn?|
            |asyAncestors| |isDefaultPackageForm?| |pfBreakFrom|
            |pfBreak?| |genDomainTraceName| |interactiveModemapForm,fn|
            |getDomainByteVector| |compileAsharpLispCmd|
            |clearConstructorCache| |spadReply,printName| |upLoopIters|
            |npCompMissing| |inclmsgPrematureFin|
            |mkAndApplyPredicates| |quoteCatOp| |npWith|
            |zeroOneConversion| |nopilesSpad2Cmd| |signatureTran|
            |setNopiles| |npBracked| |devaluate| |isStreamCollect|
            |npListAndRecover| |makeOutputAsFortran| |lncgamma|
            |makeFort,untangle| |getFirstWord| |expandCOLLECT|
            |frameEnvironment| |digit?| |getAttributesFromCATEGORY|
            |histInputFileName| |falseFun| |StringToCompStr| |nopiles|
            MKQSADD1 |scanWord| |parseSystemCmd| |hashable| |addSpaces|
            |coerceMap2E| |asySubstMapping| |npInfGeneric|
            |pfMLambdaBody| |checkForBoolean| |punctuation?|
            |pf0MLambdaArgs| |mkRationalFunction|
            |flattenOperationAlist| |hasDefaultPackage| |macMacro|
            |spadTrace,g| |cgamma| |pfNothing?| |evalDomain|
            |macSubstituteOuter| |NRTevalDomain| |pfMacroRhs| SHOWBIND
            |fracpart| |pfMacroLhs| |isCategoryPackageName| |macLambda|
            FOAM::FOAM-FUNCTION-INFO |spadThrowBrightly| |isTupleForm|
            |macWhere| |knownEqualPred| |stripUnionTags|
            |isHomogeneous| |clngammacase3| |makeByteWordVec|
            |mkPredList| |pfCheckMacroOut| |loadLib| |cgammat|
            |mac0GetName| |asyExtractAbbreviation| DROPTRAILINGBLANKS
            |npParened| |SubstWhileDesizingList| |spad2lisp|
            |pfLambda?| |isInitialMap| HAS_SHARP_VAR |pfLeaf?|
            |constructorCategory| |isHomogeneousArgs| IS_SHARP_VAR
            |pfLeafPosition| |inSuper| VMLISP::LIBSTREAM-P
            MONITOR-EVALAFTER |pfAbSynOp| |aggWidth| |mkQuote|
            |asyIsCatForm| |pfTypedId| |untrace| |makeLispList|
            |dcOpTable| |parseAtom| |pf0LambdaArgs| |intSub|
            |parseLeave| |macExpand| |qTWidth| |dcOps| MONITOR-BLANKS
            |overlabelWidth| |dcData| |pfApplication?| |printMap|
            |dcCats| |transIs| |pfMacro?| |dcPreds| |transIs1|
            |isSystemDirectory| |pfWhere?| |underDomainOf;| |makeUnion|
            |dcSlots| COND-UCASE |isListConstructor| |compileBoot|
            |dcAll| |parseDEF| |isTraceGensym| |makePredicateBitVector|
            |convertOpAlist2compilerInfo| |retract| |transTraceItem|
            VEC2LIST |coerceSpadArgs2E| |quote2Wrapped| |setDefOp|
            |fortran2Lines1| |orderByContainment| MAKE-VEC
            |isSubForRedundantMapName| |parseMDEF| |fortSize|
            |getUnionOrRecordTags| |stripOutNonDollarPreds| GCMSG
            |copyHack| |asCategoryParts,exportsOf| |parseHas,fn|
            |getModeOrFirstModeSetIfThere| |isHasDollarPred|
            |copyHack,fn| |removeAttributePredicates|
            VMLISP::GET-DIRECTORY-LIST |parseLET| |indentFortLevel|
            |resolveTTRed3| |dcData1| |isDomainOrPackage| |fortPreRoot|
            |interpOp?| |isInterpOnlyMap| |parseHas,mkand|
            |isUncompiledMap| |parseJoin,fn| |parsePretend| |isType|
            |isValidType;| |getMapSubNames| |getExportCategory|
            |parseIs| |parseHas| |isFloat| |parseJoin| |mkAtreeNode|
            |whichCat| |parseReturn| |nontrivialCosig| |getMsgTag?|
            |isFunctor| |patternCheck| |getCatAncestors| |constructor?|
            NUMOFNODES |categoryForm?| |encodeCatform|
            |asyTypeJoinStack| |parseIsnt| MKQ |getMsgPrefix|
            |resolveTMRed1| OPTIONS2UC |getMsgPos|
            VMLISP::GETINDEXTABLE |stuffDomainSlots| |getMsgPosTagOb|
            |asyTypeMakePred| |dispfortexp1| |getTarget| |bubbleType|
            |getMsgFTTag?| |getConstructorSignature| |getLookupFun|
            |msgLeader?| |fortFormatIf| |remFile| |fortFormatElseIf|
            |parseConstruct| |isAVariableType| |parseSegment|
            |getMsgText| |parseWhere| |poCharPosn| |parseAtSign|
            |orderMmCatStack| SMALL-ENOUGH |listOutputter| |pp|
            |parseExit| |numMapArgs| |isPartialMode| |destructT|
            |parseCoerce| |objVal| |evaluateFormAsType| |asTupleNew0|
            |makeMsgFromLine| |transCategoryItem| |getBasicObject|
            |erMsgSort| |parseColon| |fetchOutput| |getLinePos|
            |parseCategory| |printMms| |remLine| |last| |mkNiladics|
            |getSymbolType| |break| |polyVarlist| |poNopos?|
            |evaluateType1| |getBasicMode| |sayLongOperation|
            |evalMmDom| |poPosImmediate?| |brightPrint| |unwrap|
            |emptyAtree| |fileNameStrings| |To| MONITOR-EVALBEFORE
            |isWrapped| |resolveTypeList| |incFileName|
            |say2PerLineThatFit| |doReplaceSharpCalls| |poLinePosn|
            SPADSYSNAMEP |noSharpCallsHere| |sayDisplayWidth|
            |isApproximate| WHOCALLED |posPointers|
            |isLegitimateRecordOrTaggedUnion| |inclmsgSay|
            |getUnnameIfCan| |NRTtypeHack| |hitListOfTarget|
            |listOfDuplicates| |domainDepth| |incPos| |center80|
            |isPolynomialMode| |devaluateDeeply| |msgNoRep?|
            |pfAttribute| |sayDisplayWidth,fn| |incStringStream|
            |showMsgPos?| |sayFORMULA| SUBANQ |msgImPr?| UNVEC
            |operationLink| |evalType| |tabbing| |quoteNontypeArgs|
            |inclmsgConStill| |getMsgPrefix?| |bottomUpUseSubdomain|
            |inclmsgCannotRead| |getMsgArgL| |segmentedMsgPreprocess|
            |pfSequence| |getMsgLitSym| |splitSayBrightly| GETZEROVEC
            |getMsgKey?| |evaluateSignature| |getPosStL| |evaluateType|
            |getPreStL| |displayDatabase| |saySpadMsg|
            |shortenForPrinting| |asyArg| |sayTexmacs| TRY-GET-TOKEN
            |stripNil| |mkEvalableMapping| |processChPosesForOneLine|
            |npAnyNo| |isMapExpr| |untraceDomainConstructor|
            |sayDisplayStringWidth| |initToWhere| |sayTeX|
            |mkEvalableUnion| |initImPr| |isDomain| MAKE-SYMBOL-OF
            |retractAtree| |mkEvalableRecord| |putDatabaseStuff|
            |bottomUpCompile| |getUnderModeOf| |getMsgKey|
            VMLISP::LIBSTREAM-INDEXTABLE |processKeyedError|
            |getPreviousMapSubNames| |int2Bool| |setCurrentLine|
            |msgOutputter| |traceSpad2Cmd| |pfFix|
            |removeTracedMapSigs| |containsPolynomial|
            |isListOfIdentifiers| |handleKind| |asyAncestorList|
            |isNiladic| |expandMacros| GET-BOOT-TOKEN |vec2Lists|
            DROPENV |pfTyping| |getCon| VMLISP::SPAD-FIXED-ARG
            |complexRows| |trimComments| |walkForm| |ncloopEscaped|
            |asyExportAlist| |walkWhereList| |loadIfNecessary|
            |asytranLiteral| |serverReadLine| |postTransform|
            LINE-SUBSEQ-FROM |removeQuote| |mkQuote,addQuote|
            |SpadInterpretFile| |pfExport| |nonBlank| |makeOrdinal|
            |intInterpretPform| |flattenSemi| |npElse|
            |untraceMapSubNames| MAKE-STRING-ADJUSTABLE
            |getAndEvalConstructorArgument| |mkMapPred| GET-A-LINE
            |ncloopParse| |failCheck| |mapPredTran| |mkSharpVar|
            |vec2Lists1| |ncConversationPhase,wrapup| |pair2list|
            |npEncAp| |asyExtractDescription| SHUT |getEqualSublis|
            |makeFort,untangle2| |args2Tuple| |XDRFun|
            |npParenthesized| |removeBodyFromEnv| REROOT DIG2FIX
            |compFailure| |mkAuxiliaryName| |sayRemoveFunctionOrValue|
            |npDotted| LOG2 |makeArgumentIntoNumber| |constructor|
            |postCollect| |postIteratorList| SIZE |postColonColon|
            |suScWidth| |npFromdom1| EOFP |postForm| |listOfVariables|
            |writify| |checkWarning| |matSuper| BVEC-COPY
            |postCategory| |binomialSub| RSHUT |postElt| |npMissing|
            |whatCommands| |postTupleCollect| |isPatternArgument|
            |npBracketed| |postType| |isConstantArgument| |isRecord|
            LINE-P SPAD-KERNEL-ARG |whatSpad2Cmd,fixpat| |unTuple|
            |indefIntegralSup| |postColon|
            |displayProperties,sayFunctionDeps| |postSemiColon|
            |getUserIdentifiersIn| |sayALGEBRA| |pfLocal|
            |deconstructT| |postBlock| |getIteratorIds|
            SPAD-KERNEL-NEST |makeHistFileName|
            |getUserIdentifiersInIterators| |trace1| |isMap|
            |postDoubleSharp| |isListOfIdentifiersOrStrings| |sumWidth|
            MANEXP |comma2Tuple| |getTraceOption,hn| |sumWidthA|
            |undoLocalModemapHack| |killColons| DIGITP BVEC-NOT
            |postMapping| |satisfiesUserLevel|
            |removeSuperfluousMapping| |isInternalMapName| |quoteWidth|
            ACOT PUSHLOCVAR |constructSubst| |postRepeat| |srcPosFile|
            |postJoin| |asTupleAsVector| |pfId?|
            |processSynonymLine,removeKeyFromLine| |fixUpTypeArgs|
            |pfTuple| COT |lambdaHelper2| |postAtom| |extsub|
            SPAD-KERNEL-P CSC |postTuple| |numArgs| |mkAtreeValueOf|
            COMP-1 |postComma| |makeCharacter| |setIOindex|
            |collectDefTypesAndPreds| |compNewnam| |postDef| ACSC
            |postDefArgs| |htmlFormat| SPAD-KERNEL-POSIT |frameName|
            |dqToList| |pfCheckItOut| |isIntegerString| ASEC |compTran|
            |postOp| |mkCacheName| |slashWidth| |pfSourceText| CSCH
            |postAtSign| |dqUnitCopy| |boxWidth| |selectMostGeneralMm|
            |poGetLineObject| COTH |postPretend| |displayMacro|
            |lnPlaceOfOrigin| |objValUnwrap| SECH |postQUOTE|
            |sigmaWidth| |displayParserMacro| |pfIf?| |pfPosOrNopos|
            ACSCH |pf0TupleParts| |compFluidize| ACOTH |outformWidth|
            |pfTuple?| ASECH |postSignature| |asTupleSize| |compTran1|
            |dqUnit| |pfLiteral?| |comp_expand| |postQuote|
            |pfSymbolSymbol| |removeBindingI| |seq_opt| |postWhere|
            |formulaFormat| |mkAtreeValueOf1| |float2Sex|
            |containsVariables| |postSEGMENT| |dqConcat| |extwidth|
            |pfPile| |lambdaHelper1| |rulePredicateTran| |srcPosLine|
            |isSharpVarWithNum| |binomSuper|
            |sayAsManyPerLineAsPossible| |ruleLhsTran|
            |asTupleNewCode0| |expandCOLLECTV| |inSub| |pfRuleRhs|
            |isSharpVar| |extractCONDClauses| |ordList| |PushMatrix|
            |reportUndo| |getIProplist| |be| |wrapped2Quote|
            |pfRuleLhsItems| |compFluidize1| |testPredList|
            |mapCatchName| |displayMacros| |pfSequence2Sex0| IS_GENVAR
            BADDO LOG10 |zagWidth| |intProcessSynonyms| |pfLambdaTran|
            |pathname| |flattenCOND| |matSuperList| |objCodeVal|
            |commandsForUserLevel| |pfDefinitionRhs| |isRationalNumber|
            |pf0DefinitionLhsItems| |objCodeMode| |namestring|
            PREPARSE-ECHO |maprin| |pfSuchThat2Sex| SKIP-TO-ENDIF
            |maprin0|))
(PROCLAIM
    '(FTYPE (FUNCTION (T *) *) FOAM:COMPILE-AS-FILE ASHARP |ioHook|
            |interpret|))
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (VALUES T T)) FOAM:AXIOMXL-GLOBAL-NAME))
(PROCLAIM '(FTYPE (FUNCTION (T *) STRING) MAKE-FULL-CVEC))
(PROCLAIM
    '(FTYPE (FUNCTION (T T) *) |ncloopInclude| |coerceInt1|
            |coerceInt2Union| |set1| |unabbrevRecordComponent| |print|
            VMLISP::COPY-FILE VMLISP::COPY-LIB-DIRECTORY |sayKeyedMsg|
            |htCommandToInputLine,fn| |RecordPrint| |coerceRe2E|
            |inclHandleBug| SPAD-SAVE MAKEOP |coerceInt0| |sayErrorly|
            |saturnSayErrorly| |errorSupervisor| |updateSymbolTable|
            |upIFgenValue| |evalSlotDomain| |coerceIntTableOrFunction|
            |make_compiler_output_stream| |spadcall2| CARCDREXPAND
            |spleI1| |categoryParts,build| |rwriteLispForm| ?ORDER
            |evalAndRwriteLispForm| |checkForFreeVariables| |r_besseli|
            |ncBug| |canCoerceUnion| |sayBrightlyNT2| |sayBrightlyNT1|
            |coerceBranch2Union| |tokenSystemCommand| |isDomainForm|
            |isDomainConstructorForm| SPADRREAD
            |handleTokensizeSystemCommands|
            |handleParsedSystemCommands| |formArguments2String,fn|
            |writeInputLines| |ncEltQ| |output| ADDCLOSE
            |app2StringWrap| |makeLongTimeString| |makeLongSpaceString|
            SUFFIX |depthOfRecursion| |getMapBody| |ppPair| |formJoin1|
            |reportOperations| |notCalled| DROP |keyedSystemError1|
            |expandRecursiveBody| |breakKeyedMsg| |keyedSystemError|
            |saturnKeyedSystemError| |rightJustifyString| |printMap1|
            |asyCattranConstructors| |printTypeAndTime|
            |printTypeAndTimeNormal| |displaySetOptionInformation| MKPF
            MKPF1 |makeStream| |ScanOrPairVec| |handleNoParseCommands|
            |npsystem| COERCE-FAILURE-MSG |UnionPrint| |coerceUn2E|
            |replaceNamedHTPage| |getFortranType| |popUpNamedHTPage|
            |r_besselj| /TRACE-2 |evalLET| |isPatMatch|
            |predicateBitIndex,pn| |putModeSet| |bottomUpType| QUOTIENT
            |spadTrace| |asySigTarget| |upLispCall| |evalLETput|
            |asCategoryParts,build| |fortError| |putValue| |quickAnd|
            |patternCheck,subWild| |logicalMatch?| |superMatch?| TAKE
            /UNTRACE-2 PRINMATHOR0 |createEnum| |getArgValue1|
            MONITOR-PRINVALUE |canCoerce;| |canCoerce1|
            |getMinimalVarMode| |newHasTest| |hasCatExpression|
            |bottomUpIdentifier| BPIUNTRACE |spadUntrace|
            MONITOR-PRINARGS-1 MONITOR-PRINT |ncConversationPhase|
            /TRACELET-PRINT |intloopReadConsole| |intSayKeyedMsg|
            $FCOPY |xdrWrite| |ncloopCommand| COMP_QUIETLY_USING_DRIVER
            |ncloopInclude1| |outputLispForm| |asyTypeUnitDeclare|
            |readData,xdrRead1| |findLocalVars1| |xdrRead|
            |reportOpsFromLisplib0| |reportOpsFromLisplib|
            |reportOpsFromLisplib1| |hasCaty1| |displayProperties|
            |intCodeGenCOERCE| |putMode| |coerceByFunction|
            |mkAtreeWithSrcPos|))
(PROCLAIM
    '(FTYPE (FUNCTION (T *) T) |defaultTargetFE| PRINT-FULL
            |printRecordFile| |simpHasPred| |getConstructorExports|
            COMPILE-LIB-FILE MAKE-INPUT-FILENAME |pfSymb|
            |pfExpression| |htFile2RecordFile| |inputFile2RecordFile|
            |htFile2InputFile| MAKE-FILENAME MONITOR-ADD
            VMLISP::MAKE-FULL-NAMESTRING |desiredMsg|
            MATCH-CURRENT-TOKEN |pfSymbol| VMREAD
            GET-BOOT-IDENTIFIER-TOKEN PRETTYPRIN0 |F,PRINT-ONE|
            |centerAndHighlight| BLANKS CATCHALL TAB
            |centerNoHighlight| MATCH-NEXT-TOKEN |LAM,EVALANDFILEACTQ|
            PRETTYPRINT))
(PROCLAIM
    '(FTYPE (FUNCTION (T T) T) INITIAL-SUBSTRING |patternVarsOf1|
            |getValueFromEnvironment| |objSetVal| |condAbbrev|
            |asTupleNewCode| |throwKeyedMsg| |typeToForm|
            |clearAllSlams,fn| |eqpileTree| BVEC-MAKE-FULL |pileCtree|
            |getOpArgTypes| |unabbrev1| |pileTree| |pileForest|
            |getOpArgTypes,f| |pileForest1| |leftBindingPowerOf|
            |delete| |rightBindingPowerOf| |unabbrevUnionComponent|
            |getOpArgTypes1| |coerceIntFromUnion| ASSOCIATER
            |setBootAutloadProperties| |setBootAutoLoadProperty|
            |testInput2Output| |coerceIntAlgebraicConstant| |hyperize|
            |testPrin| |coerceIntTower| |getCDTEntry| |c_besseli|
            |coerceRetract| |mkBootAutoLoad| |compareTypeLists|
            |asyMapping| |incRenumberLine| |pfReturn| |incRenumberItem|
            |pfForin| |dnfContains| |FloatError| |dnfContains,fn|
            |createResolveTTRules| |createResolveTMRules|
            |inclHandleSay| |c_besselj| |npLeftAssoc| |andDnf|
            |ordUnion| |pfReturnTyped| |retractByFunction| |coafAndDnf|
            |BesselJ| |streamChop| |pfLam| |cPsi| |PsiXotic|
            |phIntReportMsgs| |r_psi| |clearDependentMaps| |ifCond|
            |PsiAsymptotic| |push_form1| |assertCond|
            |exp2FortOptimizeCS1,pushCsStacks| |BesselKAsymptOrder|
            |phParse| |fortFormatTypes1| |intloopInclude1| |horner|
            |pfLp| |f01| |brutef01| |incActive?| |constructT|
            |inclFname| |getMapSig| |incCommandTail| |canCoerceTower|
            |canCoerceCommute| |sayErrorly1| |simpOrUnion1|
            |repetition| |beenHere| |pfWrong| |inclHandleWarning|
            |oldAxiomDomainHashCode| |lazyOldAxiomDomainHashCode|
            |hashTypeForm| |incTrunc| |HasCategory| |replaceLast|
            |inclHandleError| |newHasCategory| |constructM|
            |coerceIntTest| |lazyOldAxiomDomainDevaluate|
            |canCoercePermute| |pfDWhere| |oldAxiomCategoryParentCount|
            |canCoerceLocal| |pfTagged| |sayLooking1|
            |fortFormatIfGoto| |compileIs| |newCanCoerceCommute|
            |oldAxiomCategoryDevaluate| |evalSharpOne| |segment1|
            |push_reduction| |optCatch,changeThrowToGo| |SExprToDName|
            |coerceCommuteTest| |mkObjWrap| |genIFvalCode| |Delay|
            |fortFormatTypes| |optCatch,hasNoThrows| |putPvarModes|
            |nrtEval| |fortranifyFunctionName| |incAppend| |pfWDeclare|
            |oldAxiomPreCategoryDevaluate| |coerceIntSpecial|
            |seteltable| |pfComDefinition| |coerceIntPermute| |next|
            |coerceIntCommute| FOAM:|printDFloat| FOAM:|printSFloat|
            |addDomainToTable| |coerceIntByMap| FOAM:|printBInt|
            |pfParen| |resolveTTCC| FOAM:|printSInt| |mkCategoryOr|
            |optCatch,changeThrowToExit| |simpCategoryOr|
            |isTowerWithSubdomain| FOAM:|printString| |pfBrace|
            FOAM:|printChar| PREDECESSOR |pfRetractTo| |subMatch|
            |pfSuch| |positionInVec| DELASC |pfHide| |replaceSymbols|
            |reportSpadTrace| |scanIgnoreLine| DELLASOS |pfBracket|
            |pfFromdom| |subTypes| |modemapsHavingTarget| |lfrinteger|
            LASSOC |pfTLam| |coerceOrConvertOrRetract|
            |optimizeFunctionDef,replaceThrowByReturn| |tempExtendsCat|
            |scanExponent| |optimizeFunctionDef,fn| |scanCheckRadix|
            |coerceOrRetract| |mergeOr| |testExtend| |getLisplib|
            |pfCoerceto| |rassoc| |isNestedInstantiation| QLASSQ
            |posend| PAIR |scanInsert| |updateCategoryTableForDomain|
            |systemDependentMkAutoload| |mkAutoLoad|
            |absolutelyCannotCoerce| |formatOpSignature|
            |unloadOneConstructor| |besselIback| |cbeta|
            |getConstructorOpsAndAtts| |mkIterVarSub|
            |encodeCategoryAlist| |autoLoad| |pfPretend| SORTBY
            WRAPDOMARGS |syIgnoredFromTo| LENGTHENVEC
            |canCoerceExplicit2Mapping| |pfBracketBar| |declare|
            |declareMap| |sySpecificErrorHere| |after| |upfreeWithType|
            |uplocalWithType| |sayBrightly2| |rPsiW| |member|
            |sayBrightly1| |formatOpSymbol| |EqualBarGensym| CHAR-EQ
            |canConvertByFunction| |optionUserLevelError|
            |loadLibIfNecessary| |dollarTran| |coafOrDnf|
            |formDecl2String| |ordSetDiff| |sayModemapWithNumber|
            |makePathname| |readLib| |formalSubstitute|
            FOAM:|PtrMagicEQ| |commandError| |deepSubCopyOrNil|
            |deepSubCopy0| |subCopyOrNil| |hasPair| |assoc|
            |npRightAssoc| |equalOne| |prefix?| |pfMapParts| EFFACE
            |formArguments2String| |pfCopyWithPos|
            |computeTTTranspositions| EMBED |getBindingPowerOf|
            |constantInDomain?| |macLambda,mac| |coerceInteractive|
            MONITOR-WRITE |opWidth| |equalZero| |updateCategoryTable|
            |matSuperList1| |isSubTowerOf| |hasCat| LEXGREATERP
            |catPairUnion,addConflict| |prnd| |outputMapTran0|
            |clearCategoryTable1| |dcOpPrint| ADDOPERATIONS |quickOr|
            FOAM:|fiSetDebugger| |linearFormatForm| |everyNth|
            |mkSuperSub| SET-LIB-FILE-GETTER |makeCompactDirect1|
            |augmentPredVector| |EqualBarGensym,fn| |npTypedForm|
            |NRTdescendCodeTran| |app2StringConcat0| FOAM::|magicEq1|
            |rep| |getMsgCatAttr| |makeGoGetSlot| NCONC2
            |applyWithOutputToString| FOAM:|fputs| |spadPrint|
            FOAM:|fputc| GETCONSTRUCTOR STOREBLANKS |dcOpLatchPrint|
            DELDATABASE DAASENAME MAKE-DATABASES ESCAPED |getI|
            REMAINDER |orDel| |ordIntersection| |coerceIntByMapInner|
            |objNewCode| |coafAndCoaf| FOAM::ALLOC-PROG-INFO
            |formatOperation| DIVIDE2 |putFlag| |setMsgCatlessAttr|
            |rempropI| |parseTranCheckForRecord| |valueArgsEqual?|
            |asTupleNew| |printNamedStatsByProperty| |putValueValue|
            |initializeTimedNames| |orDnf| |FromTo| |traceOptionError|
            |isKeyQualityP| |listTruncate| |erMsgCompare|
            |compareposns| |mathPrint1| |queryUserKeyedMsg| GLESSEQP
            |stringMatches?| |pfTree| |makePattern| |basicMatch?|
            |sayDroppingFunctions| |installConstructor|
            |remHashEntriesWith0Count,fn| GGREATERP CGREATERP
            |formatJoinKey| |containsOp| ASHARPMKAUTOLOADFUNCTION
            |matWList| |matWList1| |outputFormat| |mkFreeVar|
            |substInOrder| |match_current_token| |pfOr|
            |reportCircularCacheStats| |pfRestrict| |compileBody|
            |pfMLambda| |makeLocalModemap| |mkCircularCountAlist|
            |scylla| |getLocalVars| |mkLocalVar| |pfAnd| |pfBraceBar|
            |canFit2ndEntry| LEXLESSEQP |pfIdPos| |mkMapAlias|
            |addDmpLikeTermsAsTarget| |nonRecursivePart1| |position1|
            |sayPatternMsg| |allLASSOCs| |throwKeyedMsg1|
            |saturnThrowKeyedMsg| |findLocalVars| |asyAbbreviation|
            |insertWOC,fn| |nonRecursivePart| |search|
            |searchCurrentEnv| |addToSlam| |stringChar2Integer|
            |sayKeyedMsgAsTeX| |asyAbbreviation,chk| /READ
            |splitListOn| |mergePathnames| |writeLib| |asySplit|
            |mkAlistOfExplicitCategoryOps,fn| |fillerSpaces|
            |getProplist| |commandErrorIfAmbiguous| |concat1| |asyWrap|
            REMALIST |optionError| |stringPrefix?| REMOVE-ESCAPES
            |asyDisplay| |deleteAssocWOC,fn| |insert| |asySignature|
            |domainEqualList| |deleteAssocWOC| |globalHashtableStats|
            |mkObj| |coerceInt| |commandUserLevelError| |lassocShift|
            |BesselI| |pfSpread| |pfTyped| STACK-PUSH
            |displayOpModemaps| |cgammaG| |translateMpVars2PVars|
            |chebeval| |findSubstitutionOrder?,fn| TRANSLABEL1
            GETDATABASE |containedRight| TRANSLABEL |postTranSegment|
            |cpsireflect| |getOption| DIVIDE |sublisNQ,fn| GETL
            |postFlatten| |removeVectorElt| |processInteractive1|
            |genMpFromDmpTerm| |recordAndPrint| |interpretTopLevel|
            |processInteractive| |sayCacheCount| INTERSECTIONQ
            |pfApplication| |undoSteps| |recurrenceError| |push_lform1|
            |displaySetVariableSettings| |countCircularAlist|
            |deleteLassoc| |msgText| |plural| |sameUnionBranch|
            |getModemapsFromDatabase| |position| FLAGP
            |substituteSegmentedMsg| |getSystemModemaps|
            |sortAndReorderDmpExponents| |predCircular| |sublisNQ|
            |printTypeAndTimeSaturn| |assocCircular| |pfAssign|
            MKPFFLATTEN |parse_Operation| S- GET-GLIPH-TOKEN |symEqual|
            S* |deleteAssoc| |pfWhere| GETALIST
            |parse_rightBindingPowerOf| |npsynonym| |isDomainSubst,fn|
            |getAllModemapsFromDatabase| |removeOption| |pfWDec|
            REMOVER PUSH-REDUCTION |HasSignature|
            |isDomainSubst,findSub| |hashCombine| |hashType|
            |parse_getSemanticForm| |testBitVector|
            |varIsOnlyVarInPoly| |npTypedForm1| |deepSubCopy|
            |modemapPattern| |parse_leftBindingPowerOf|
            |BooleanEquality| |pairList| |pfCollect| |insertWOC|
            |domainEqual| |oldAxiomPreCategoryParents| |BesselJRecur|
            |insertModemap| |BesselJAsymptOrder| |deleteAll|
            |hasOption| |BesselJAsympt| |ScanOrPairVec,ScanOrInner|
            |makeSF| |MappingPrint| |oldAxiomCategoryHashCode| FLAG
            |has| |chebstareval| |substDomainArgs|
            |spadTrace,isTraceable| |oldAxiomCategoryDefaultPackage|
            |NRTreplaceLocalTypes| |oldAxiomPreCategoryHashCode|
            |inFirstNotSecond| MAKEARR1 |resolveTT;| MONITOR-GETVALUE
            |c_hyper0f1| FOAM-USER::H-ERROR |pfPushMacroBody| |chebf01|
            |macSubstituteId| |BesselIAsymptOrder|
            |oldAxiomDomainDevaluate| |pfMacro| |macWhere,mac|
            |resolveTT1| |sort| |c_psi| |mac0SubstituteOuter|
            |isPatternMatch| MONITOR-EVALTRAN1
            |macLambdaParameterHandling| |getConstantFromDomain;|
            |asyGetAbbrevFromComments,fn| |PsiEps|
            |getConstantFromDomain1| FOAM-USER::H-STRING
            MONITOR-EVALTRAN |sayIntelligentMessageAboutOpAvailability|
            |untraceDomainLocalOps| |asySig| GETREFV32
            |traceDomainConstructor| |getLisplibNoCache|
            |evalLETchangeValue| |augmentPredCode|
            |convertOpAlist2compilerInfo,formatSig|
            FOAM-USER::H-INTEGER |expression2Fortran1| |canCoerceFrom;|
            |mungeAddGensyms| |rassocSub| |resolveTTRed|
            |fortranifyIntrinsicFunctionName| |makePrefixForm|
            |subCopy0| |dispfortexpj| |objNew| SETDIFFERENCE |segment2|
            |getArgValue| |buildBitTable,fn| |breaklet| RDROPITEMS
            |resolveTMOrCroak| |tracelet| |acceptableTypesToResolve1|
            |union| |resolveTTSpecial| |maskMatch?| |compareTT|
            /GETTRACEOPTIONS |match?| |getStatement| |evalCategory|
            |getAtree| |acceptableTypesToResolve| |dispfortarrayexp|
            |resolveTM2| |setMsgPrefix| |patternCheck,wild|
            |resolveTMRed| |assignment2Fortran1| |patternCheck,pos|
            |replaceSharps| |term1RWall| TRUNCLIST
            |integerAssignment2Fortran1| |suffix?| |term1RW|
            |displayDatabase,fn| |resolveTCat1|
            |getConditionsForCategoryOnType| |putFTText| $REPLACE
            |ofCategory| TRUNCLIST-1 |dispfortexpf| |resolveTMEq|
            |setMsgText| UNIONQ |resolveTMEq1| |resolveTMSpecial|
            |resolveTTUnion| |throwEvalTypeMsg| INTEXQUO
            |queueUpErrors| |constructTowerT| |asyExportAlist,fn|
            |makeByteWordVec2| |spliceTypeListForEmptyMode| |mergeSubs|
            |resolveTMTaggedUnion| |getArgValueOrThrow| |resolveTTAny|
            |resolveTCat| |resolveTMRecord| |thisPosIsEqual| QUOTIENT2
            |resolveTMUnion| |defaultTypeForCategory| |matchMmSigTar|
            |listDecideHowMuch| |newHasTest,fn| |asyTypeJoinPart|
            |say2PerLineWidth| |resolveTTEq| |decideHowMuch|
            |computeTypeWithVariablesTarget| |incStream|
            |removeListElt| |insertPos| |intersection| |center|
            |putTarget| |inclmsgFileCycle| |redundant| |pfExit|
            |sameMsg?| SUBLISNQ |resolveTM1| |incDrop|
            |transferPropsToNode| |getDomainView| |lnSetGlobalNum|
            DEFINE-FUNCTION |resolveTM| SUBB |findRetractMms;|
            |thisPosIsLess| |throwPatternMsg| |pushDownOp?|
            |findRetractMms1| |asyCattranSig| |wordFrom|
            |getAliasIfTracedMapParameter| |asySimpPred|
            |sayKeyedMsgLocal| |funfind| |isEqualOrSubDomain| CONTAINED
            |hasCorrectTarget| |CONTAINED,EQ| |CONTAINED,EQUAL| S+
            |getMinimalVariableTower| |lassocSub| |npMissingMate|
            |computeTargetMode| |wt| |setMsgUnforcedAttrList|
            |isString?| /GETOPTION |keyedMsgCompFailure| |makeVector|
            VMLISP::PUTINDEXTABLE |replaceArgDef| |phReportMsgs|
            |replaceArgDef1| |processMsgList|
            |bottomUpCompilePredicate| |writeData|
            |intloopProcessString| |setSf| |replaceArgDefs| RPLPAIR
            |bottomUpPredicate| |replaceArgDefs1| |xdrOpen|
            |intloopPrefix?| |isPointer?| |getBasicMode0|
            |ncINTERPFILE| SETANDFILE STRING2ID-N |pfAbSynOp?|
            |ncloopPrefix?| ERROR-FORMAT |mkValCheck| |pfIfThenOnly|
            VMLISP::WRITE-INDEXTABLE |mkValueCheck| |wl|
            |ncloopDQlines| |asyCatSignature| |simplifyMapPattern|
            |asyMkSignature| POSN1 |getEqualSublis,fn|
            |saveDependentMapInfo| |EnumPrint| |asyDocumentation,fn|
            |interpMap| |putBodyInEnv| |readData| |mmCatComp| $FINDFILE
            |functionAndJacobian,DF| |mkFormalArg|
            |filterListOfStrings| |mkAliasList,fn| |pfFromDom|
            |satisfiesRegularExpressions| |clearDependencies|
            |argCouldBelongToSubdomain| |putDependencies| BVEC-CONCAT
            |CONTAINEDisDomain|
            |putDependencies,removeObsoleteDependencies| |makeList|
            |postFlattenLeft| BVEC-EQUAL |makeNewDependencies|
            BVEC-GREATER |addDefMap| |augmentTraceNames| BVEC-AND
            BVEC-OR SEGMENT |addPatternPred| BVEC-XOR |canMakeTuple|
            BVEC-NAND BVEC-NOR |checkArgs| |undoSingleStep|
            |fastSearchCurrentEnv| |displayRule| |coerceVal2E|
            |canCoerceFrom0| |subCopy| |postSignature1|
            |mapDefsWithCorrectArgCount| |showInput| |mkAtree3fn|
            |repeat_tran| |agg| |showInOut| |makeRuleForm|
            |pfDefinition| |mkLessOrEqual| |transferSrcPosInfo|
            |dqAppend| |untraceDomainConstructor,keepTraced?|
            |pfQualType| |matSubList1| |pfRule| |mkAtree1WithSrcPos|
            |SymMemQ| |AlistAssocQ| |canCoerceByMap|
            |mkAtreeNodeWithSrcPos| |canCoerceByFunction| |getAndSay|
            |dqAddAppend| |pvarPredTran| |ListMemberQ?| |isSubDomain|
            |objNewWrap| |absolutelyCanCoerceByCheating| |diffAlist|
            |ListMember?| |AlistRemoveQ| |objSetMode| PARSEPILES
            PRINT-AND-EVAL-DEFUN ADD-PARENS-AND-SEMIS-TO-LINE
            PRINT-DEFUN |intloopInclude| |mkObjCode| |addPred|
            EVAL-DEFUN))
(PROCLAIM
    '(FTYPE (FUNCTION NIL FIXNUM) HEAPELAPSED CURRENT-CHAR-INDEX))
(PROCLAIM '(FTYPE (FUNCTION NIL (VALUES T T)) GENVAR))
)
