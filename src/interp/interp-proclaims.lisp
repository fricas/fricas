#+:GCL
(progn
  (eval-when (:execute :compile-toplevel :load-toplevel)
                 (proclaim '(optimize (safety 1) (debug 3))))

(IN-PACKAGE "BOOT") 
(PROCLAIM '(FTYPE (FUNCTION NIL (*)) FIRST-ERROR)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FIXNUM) LINE-NUMBER |eq0| |nothingWidth|
            |nothingSuper| |nothingSub| |widthSC| LINE-CURRENT-INDEX
            FOAM:|ProgHashCode| FOAM:|strLength| CHAR2NUM
            |hitListOfTarget| LINE-LAST-INDEX)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FUNCTION) FOAM::FOAMPROGINFOSTRUCT-FUNCALL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FOAM:|SInt|)
            FOAM::FOAMPROGINFOSTRUCT-HASHVAL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) (VALUES T T)) FILE-GETTER-NAME
            |makeInternalMapMinivectorName| |fetchKeyedMsg|
            |mkSharpVar| |makeCharacter| READLINE |mkCacheName|
            |mkAuxiliaryName| |getKeyedMsg| FOAM:AXIOMXL-FILE-INIT-NAME
            MONITOR-INFO |mapCatchName| |queryUser|)) 
(PROCLAIM
    '(FTYPE (FUNCTION ((VECTOR T) (VECTOR T)) T) VMLISP::VGREATERP
            VMLISP::LEXVGREATERP)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) (VALUES T T)) |read-line|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) FIXNUM) QSREMAINDER QSQUOTIENT
            FOAM:|SetProgHashCode| QENUM)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) *) |selectOption| |parseIf,ifTran|
            |templateVal| |getSlotFromDomain| |augModemapsFromDomain1|
            |getVal| |Qf2F| |htMkPath| BUILD-DEPSYS |asytranForm|
            |asytranApplySpecial| |asytranFormSpecial| |ncloopInclude0|
            |exp2FortSpecial| |selectOptionLC| GETOP)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) *) |overlabelApp| |overbarApp|
            |argsapp| |appparu1| |appsc| |appsetq| |appsub| |appfrac|
            |slashApp| |appmat| |inApp| |exptApp|
            |constructorAbbreviationErrorCheck| |quoteApp| APP
            |appargs| |appagg| |appHorizLine| |appparu| |charyTrouble1|
            |newExpandLocalTypeArgs| |patternCheck,mknew|
            |makeStatString| FOAM:|fputss| FOAM:|fgetss| |binomialApp|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) *) CONCAT |ncBug| LOCALDATABASE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) *) INTERPSYS-IMAGE-INIT
            |addModemap| |makeLongStatStringByProperty|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T T T T) *) |makeFortranFun|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) *) LOCALNRLIB |appagg1| |appargs1|
            |apprpar1| |applpar1| |applpar| |apprpar| |appvertline|
            |matrixBorder| |apphor| |compileAndLink|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T) *) |lisplibError|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) T) |pfLambda|
            |throwKeyedMsgCannotCoerceWithValue| |argumentDataError|
            THETACHECK |retractUnderDomain| |pfInfApplication| |evalIF|
            |IFcodeTran| |evalis| |pfIf| |subVecNodes|
            |simpHasPred,simpHas| |upNullList| |upRecordConstruct|
            |upTaggedUnionConstruct| |maprinSpecial| SMALL-ENOUGH-COUNT
            |encodeUnion| |moreGeneralCategoryPredicate| DEF
            |makeCatPred| |userLevelErrorMessage|
            |commandAmbiguityError| |commandErrorMessage| SETDATABASE
            |inclmsgIfSyntax| |postCollect,finish| |throwKeyedErrorMsg|
            |get| |throwListOfKeyedMsgs| |unabbrevSpecialForms|
            ADDOPTIONS |evalCOERCE| |interpCOLLECT|
            |mkUserConstructorAbbreviation| |evalCOLLECT| |nAssocQ|
            |longext| |upLoopIterIN| HPUT |interpCOLLECTbody|
            |upStreamIterIN| |mkIterFun| |writeLib1| QESET SUBSTRING
            |lffloat| |mapRecurDepth| |substringMatch|
            |analyzeNonRecur| |compSPADSLAM| |restoreDependentMapInfo|
            |buildPredVector,fn| |rread| |rwrite| |readLib1|
            |incPrefix?| |orderPredTran| |sySpecificErrorAtToken|
            |findLocalsInLoop| |sideEffectedArg?| |replaceVars|
            |rewriteMap1| EQSUBSTLIST |substVars| |rewriteMap|
            |orderPredicateItems| |evalFormMkValue| |displayMap|
            |altTypeOf| |dcSig| |infixArgNeedsParens|
            |getMappingArgValue| |getArgValueComp|
            |getOplistWithUniqueSignatures| |insertAlist|
            |NRTextendsCategory1| |insertShortAlist| |extendsCategory|
            |augLisplibModemapsFromCategory|
            |transferPropsToNode,transfer| |substSlotNumbers|
            |augmentLisplibModemapsFromFunctor| |LargeMatrixp|
            |extendsCategoryBasic0| |setMsgUnforcedAttr|
            |getValueFromSpecificEnvironment| |extendsCategoryBasic|
            |get0| |catExtendsCat?| |setMsgForcedAttr| |get1|
            |expandType| |expandTypeArgs| |get2| |updateDatabase|
            |splitConcat| |recordNewValue| |charybdis| |recordOldValue|
            |remprop| |addMap| |addIntSymTabBinding|
            |NRTgetLookupFunction| |RecordEqual| |deleteMap| |charyTop|
            |compiledLookup| |UnionEqual| |buildPredVector|
            |sublisMatAlist| |outputNumber| |outputString| |putI|
            |displaySingleRule| |MappingEqual| |EnumEqual|
            |mkNewUnionFunList| |analyzeMap0| |mkInterpFun|
            PRINT-XDR-STREAM |mkMappingFunList| |mkRecordFunList|
            |mkEnumerationFunList| |mkUnionFunList| |rewriteMap0|
            |interpret1| |compileCoerceMap| MSUBST |compileDeclaredMap|
            |ncPutQ| |stuffSlot| |getSlotNumberFromOperationAlist|
            |mac0Define| |sigsMatch| |rightCharPosition| |stringMatch|
            |matchSegment?| |infix?| |nsubst| |mungeAddGensyms,fn|
            RPLNODE |newExpandGoGetTypeSlot| |isLegitimateMode;|
            |domainVal| |augProplistInteractive| |getCatForm| |incZip|
            |augProplist| |getOpBindingPower| |addBindingInteractive|
            |needBlankForRoot| |position,posn| |mapLetPrint| |letPrint|
            |lazyOldAxiomAddChild| |letPrint2| |nextown| |charPosition|
            PUTALIST |oldAxiomCategoryNthParent| |centerString|
            |oldAxiomCategoryBuild| |oldAxiomAddChild| HREMPROP
            |mac0InfiniteExpansion| |addBinding|
            |NRTisRecurrenceRelation| |loadLibNoUpdate| |interpret2|
            |fortCall| VMLISP::MAKE-ENTRY |resolveTTEq1| |resolveTTEq2|
            |makeResultRecord| |resolveTTRed1| |resolveTTRed2|
            |prepareResults,defaultValue|
            |getConditionalCategoryOfType| |PARSE-getSemanticForm|
            |matchUpToPatternVars| |mkAtree2| |cleanUpAfterNagman|
            |mkAtree3| |getOpCode| |resolveTMEq2| |newExpandTypeSlot|
            |lazyDomainSet| |hasAttSig| |hasAtt| RWRITE |domArg2|
            |application2String| |unifyStruct| |hasFileProperty;|
            |hasFilePropertyNoCache| |hasSigAnd| |lookupInDomainByName|
            |hasSigOr| |sigDomainVal| |newExpandLocalType|
            |unifyStructVar| |newExpandLocalTypeForm| |isOpInDomain|
            |findCommonSigInDomain| |findUniqueOpInDomain|
            |coerceConvertMmSelection;| |npList|
            |getFunctionFromDomain| |pfPushBody|
            |addToConstructorCache| |recordInstantiation| SUBLISLIS
            |recordInstantiation1| PUT |writeXDR| |writeStringLengths|
            AS-INSERT |getLocalMms| |getLocalMms,f| |makeAspGenerators|
            |makeCompilation| |makeAspGenerators1|
            |oldAxiomPreCategoryBuild| |flowSegmentedMsg| |npAndOr|
            |hasCaty| FOAM:|FormatNumber| |assocCacheShiftCount| |P2Up|
            |matchTypes| |assocCacheShift| |Qf2PF| |chebevalarr|
            |assocCache| |Qf2domain| |filterModemapsFromPackages|
            |BesselIAsympt| |Qf2EF| |selectMmsGen,exact?| |Qf2Qf|
            |Rm2L| |Rm2M| |evalMmCond| |substring?| |Rm2Sm|
            |stringPosition| |Rm2V| |Scr2Scr| |SUP2Up|
            |lassocShiftWithFunction| |Sm2L| AS-INSERT1 |Sm2M|
            |evalMmCond0| |Sm2PolyType| |Sm2Rm| |reportOpSymbol,sayMms|
            |Sm2V| |Sy2OV| |Sy2Dmp| |coerceTypeArgs| |Sy2Mp|
            |filterAndFormatConstructors| |Sy2NDmp| |Sy2P| |constrArg|
            |Sy2Up| |filterListOfStringsWithFn| |Sy2Var| |augmentSub|
            |Up2Dmp| |Up2Expr| |Up2FR| |evalMmCat1| |Up2Mp|
            |traceDomainLocalOps| ELEMN |Up2P| |hasCate| |Up2SUP|
            |Up2Up| |Un2E| |Var2OV| |pfWith| |Var2Dmp| |Var2Gdmp|
            |Var2Mp| |Var2NDmp| |Var2P| |Var2QF| |Var2FS| |Var2SUP|
            |asyCattranOp1| |Var2OtherPS| V2M |V2Rm| |V2Sm| |P2Uts|
            |computeTTTranspositions,compress| |P2Uls| |pfWIf|
            |npListofFun| |permuteToOrder| |P2Upxs| PROPERTY
            |NRTcompiledLookup| |spad2BootCoerce| DEF-INNER
            |insertAlist,fn| |hput| |Var2Up| |Var2UpS|
            |isRectangularVector| |makeEijSquareMatrix| DP2DP |Dmp2Mp|
            |getfortarrayexp| |domain2NDmp| |Dmp2NDmp|
            |throwKeyedMsgSP| |Dmp2P| SPADRWRITE |Dmp2Up| B-MDEF
            |isEltable| |NDmp2domain| |fortFormatLabelledIfGoto|
            |NDmp2NDmp| |clngamma| |Expr2Complex| |clngammacase1|
            |Expr2Mp| |fortFormatHead| |clngammacase23|
            |keyedMsgCompFailureSP| |recordNewValue0| |Expr2Up|
            |PiMinusLogSinPi| |recordOldValue0| |Ker2Ker| |Ker2Expr|
            |logH| |optSpecialCall| |Factored2Factored| SPADRWRITE0
            |Complex2underDomain| |Complex2FR| |Complex2Expr| I2EI
            |asGetExports| I2OI I2PI I2NNI |cotdiffeval| |L2Tuple|
            CARCDRX1 L2DP |pfTLambda| L2V |PsiAsymptoticOrder| V2L
            |L2Record| |PsiBack| |npBackTrack| |L2Rm| |L2Sm|
            MKPFFLATTEN-1 |L2Set| |Set2L| |Agg2Agg| |lisplibWrite|
            |Agg2L2Agg| |coerceOrCroak| |M2Rm| |M2Sm| |chebf01coefmake|
            |isRectangularList| |Mp2Dmp| |chebstarevalarr| |Mp2Expr|
            |coerceOrFail| |Mp2FR| |Mp2P| |besselIcheb| |fnameMake|
            |Mp2Up| REDUCE-1 OV2OV |asyMakeOperationAlist|
            |BesselasymptA| |sayFunctionSelectionResult| OV2P
            |compiledLookupCheck| |OV2poly| |asytranForm1| OV2SE
            |countParens| |fnameNew| |algEqual| |OV2Sy| |nopile| P2FR
            |asytranApply| |SpadInterpretStream| |P2Dmp|
            |upLETWithFormOnLhs| |P2Expr| |intCodeGenCoerce1| |P2Mp|
            |upLETtype| |pileForests| |getConstructorOpsAndAtts|
            |upSetelt| SUBSTEQ |evalQUOTE| ADDASSOC
            |canCoerceByFunction1| |exp2Fort2| |evalREPEAT|
            |algCoerceInteractive| |exp2FortFn| |evalSEQ| |upNullTuple|
            |intloopProcess| |asySig1| |upwhereClause|
            |pushDownOnArithmeticVariables| |upwhereMkAtree|
            |upwhereMain| |coerceTraceArgs2E|
            |intloopSpadProcess,interp| MONITOR-PRINARGS
            |coerceSubDomain| |coerceDmp2| |coerceDmpCoeffs|
            |coerceTraceFunValue2E| |NRTcompileEvalForm| |mkFortFn|
            |Dmp2Dmp| |coerceIntX| |putAtree| |getSubDomainPredicate|
            |pushDownTargetInfo| |patternCheck,equal| |Expr2Dmp|
            |upTableSetelt| |displayValue| |coerceFFE| |assignSymbol|
            |displayCondition| |displayMode| |coerceOrThrowFailure|
            |displayType| V2DP |intloopInclude0| L2M |displayModemap|
            M2M |displayModemap,g| |Dmp2Expr| |insertEntry|
            |interpRewriteRule| |Mp2Mp| |Rn2F| M2L M2V |selectMms|
            |npParenthesize| |evalIsPredicate| |evalIsntPredicate|
            |errorSupervisor1| /MONITORX |anySubstring?|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) T) |catchCoerceFailure| REDUCE-N
            |interpIF| |indefIntegralApp| |intApp| |putFileProperty|
            |appext| |appneg| |mkAndApplyZippedPredicates|
            |mkIterZippedFun| |simpHasSignature| |evalTupleConstruct|
            |evalInfiniteTupleConstruct| |evalconstruct| |rootApp|
            |boxApp| |nothingApp| |zagApp| |catPairUnion| |stringApp|
            |timesApp| |stepApp| |centerApp| |concatApp| |concatbApp|
            |say2Split| |plusApp| |bracketApp| |xlPrematureEOF|
            |braceApp| |xlSkip| |xlOK| |aggApp| |sigmaApp| |sigma2App|
            |xlConsole| |evalTargetedADEF| |xlSkippingFin|
            |evalUntargetedADEF| |xlPrematureFin| |compileTargetedADEF|
            |mkInterpTargetedADEF| |condUnabbrev| |piApp| |xlIfBug|
            |pi2App| |xlCmdBug| |charyMinus| |charySplit|
            |charyEquatnum| |collectStream| |upLoopIterSTEP|
            |charySemiColon| |interpLoop| |collectStream1|
            |collectOneStream| |appInfix| |collectSeveralStreams|
            |upStreamIterSTEP| |appsum| |boxLApp| |getFileProperty|
            |termMatch| |analyzeNonRecursiveMap| |put| |getArgValue2|
            |fixUpPredicate| |appSum| |appconc| |xLate| |bigopWidth|
            |putIntSymTab| |lookupInTable| |srcPosNew| |charyElse|
            |charyTrouble| |constoken| |clearDep1| |lookupComplete|
            |analyzeUndeclaredMap| |findConstructorSlotNumber|
            |makeInternalMapName| |appChar| |appelse| |lazyMatch|
            |hashNewLookupInCategories| |lazyMatchAssocV|
            |semchkProplist| |mac0ExpandBody| |letPrint3| |nextown2|
            STRPOSL |interpCOLLECTbodyIter| |compareSig|
            |mac0MLambdaApply| |lookupDisplay| |augProplistOf|
            |lazyCompareSigEqual| |compareSigEqual| |lookupIncomplete|
            |prepareData| |getConditionalCategoryOfType1|
            |protectedNagCall| MAKE-FLOAT |newLookupInCategories|
            |newLookupInAddChain| |sayLooking| |reportFunctionCacheAll|
            |basicLookup| |hasCateSpecial| |hasCateSpecialNew|
            |lookupInDomainVector| |newLookupInCategories1| |hasSig|
            |nrunNumArgCheck| |mkDomPvar| |domArg| |lazyMatchArg|
            |lazyMatchArgDollarCheck| |mergeSort| |mergeInPlace|
            |putHist| |printDec| |writeMalloc| |printCName| |compClam|
            |analyzeMap| |axiomType| |allOrMatchingMms|
            |oldCompLookupNoDefaults| |oldCompLookup| |matchMmSig|
            |basicLookupCheckDefaults| |evalMm| |matchAnySegment?|
            |evalMmFreeFunction| |evalMmCat| |hasCate1| |haddProp|
            |P2Us| STRPOS |optCallSpecially| |clngammacase2|
            ASHARPMKAUTOLOADCATEGORY |selectDollarMms|
            ASHARPMKAUTOLOADFUNCTOR |defaultTarget| |selectMmsGen|
            |asGetModemaps| |asytranDeclaration| |BesselasymptB|
            REDUCE-N-1 |asytranCategory| REDUCE-N-2
            |canCoerceTopMatching| |asytranCategoryItem|
            |NRTgetMinivectorIndex| |intloopSpadProcess| |interpREPEAT|
            |typeToForm,fn| |evalTuple| |coerceImmediateSubDomain|
            |superSubApp| |getReduceFunction| |binomApp|
            |altSuperSubApp| |bottomUpDefault| |MpP2P| |vconcatapp|
            |selectLocalMms| |evalForm| |npEnclosed|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) T) |interpLoopIter| |compileIF|
            LOCALASY |incLine| |xlMsg| |xlOK1| |xlSay| |xlNoSuchFile|
            |xlCannotRead| |xlFileCycle| |xlConActive| |xlConStill|
            |compileADEFBody| FINCOMBLOCK |charyBinary| |concatTrouble|
            |split2| |incLude| |reportFunctionCompilation|
            |analyzeRecursiveMap| TOKEN-INSTALL1 |needStar|
            |getArgValueComp2| |concatApp1| |aggregateApp| |augmentMap|
            |analyzeDeclaredMap| |getNewDefaultPackage|
            |hashNewLookupInTable| |prepareResults| |resolveTT2|
            |spadify| |putSrcPos| |compileRecurrenceRelation|
            |newCompareSig| |newLookupInDomain| |lazyMatchArg2|
            |selectMms1;| |selectMms2| |newLookupInTable|
            |commuteNewDistributedMultivariatePolynomial| |mkCacheVec|
            |selectMmsGen,matchMms| |makeConstrArg| |printLabelledList|
            |coerceByTable| |commuteComplex| |commuteQuaternion|
            |commuteFraction| |commuteSquareMatrix|
            |commuteUnivariatePolynomial| |commutePolynomial|
            |commuteMultivariatePolynomial|
            |commuteDistributedMultivariatePolynomial|
            |commuteSparseUnivariatePolynomial| |commuteMPolyCat|
            |fortFormatDo| |bottomUpFormRetract|
            |bottomUpFormAnyUnionRetract| |logS| |sayFunctionSelection|
            |orderMms| -REDUCE |coerceDmp1| |bottomUpForm|
            |bottomUpDefaultEval| |bottomUpDefaultCompile|
            |bottomUpForm3| |bottomUpForm2|
            |bottomUpFormUntaggedUnionRetract| |bottomUpFormTuple|
            |bottomUpForm0|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) T) |pfLeaf| |listSort| |categoryParts|
            NREMOVE MATCH-TOKEN |remove| RREAD |wasIs| |pfAdd| BPITRACE
            |tokConstruct|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T) T) |makeFort1| BUILD-INTERPSYS
            |P2MpAux|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T T) T) |makeSpadFun|
            |P2DmpAux|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T) T) /MONITOR |writeCFile|
            |Mp2MpAux2|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T) T) |appInfixArg|
            |abbreviationError| |bigopAppAux|
            |lazyOldAxiomDomainLookupExport|
            |oldAxiomDomainLookupExport| |mkDiffAssoc| |invokeNagman|
            |findFunctionInCategory| |findFunctionInDomain| |Expr2Dmp1|
            |Mp2SimilarDmp| |Mp2MpAux0| |Mp2MpAux1|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) T) |upDollarTuple| |incLine1|
            |xlIfSyntax| |bracketagglist| |compDefineLisplib|
            |putMapCode| |genMapCode| |oldAxiomCategoryLookupExport|
            |nagCall| |makeFort| |compHash| |mmCost0| |mmCost|
            |findFunctionInDomain1| |BesselIBackRecur| |invokeFortran|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T *) T) |ncSoftError| |lnCreate|
            TOKEN-INSTALL |ncHardError| |asCategoryParts|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T *) T) |msgCreate|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T *) T) RPLACSTR)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL *) |executeQuietCommand| |testPage|
            |sendHTErrorSignal| |listConstructorAbbreviations|
            |clearCmdAll| |queryClients| HELP |scanS| |copyright|
            |pquit| |quit| /RF-1 |/RQ,LIB| RECLAIM /RQ |printStorage|
            |printStatisticsSummary| MKPROMPT |princPrompt|
            |generateDataName| |generateResultsName|
            |sendNagmanErrorSignal| |npDefinitionItem| /RF |npCategory|
            RESETHASHTABLES |npMDEFinition| |readSpadProfileIfThere|
            |pquitSpad2Cmd| |quitSpad2Cmd| |leaveScratchpad|
            |npPrimary1| |npMacro| |npDefn| |npRule|
            |describeSetStreamsCalculate| |displayFrameNames|
            |PARSE-Primary1| |PARSE-Label| |PARSE-Sexpr| |PARSE-Sexpr1|
            |disableHist| |PARSE-Category| |PARSE-Selector|
            |PARSE-Leave| |PARSE-Enclosure|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL T) |npPPf| CURRENT-SCANNER-CHAR |spadThrow|
            |PARSE-VarForm| |PARSE-Name| |npPPg| |PARSE-Application|
            |PARSE-Statement| |npPCff| TOP |coercionFailure|
            |PARSE-Form| |npPCg| /TRACEREPLY |PARSE-Reduction|
            |npState| |npPushId| |npInfixOp| |describeAsharpArgs|
            |npLogical| |npBacksetElse| /EMBEDREPLY FRICAS-RESTART
            |resetWorkspaceVariables| OPERATIONOPEN |initHist|
            |initNewWorld| BROWSEOPEN CATEGORYOPEN INITIAL-GETDATABASE
            |ncError| |quadSch| WRITE-COMPRESS WRITE-BROWSEDB
            WRITE-OPERATIONDB WRITE-CATEGORYDB WRITE-INTERPDB
            TERSYSCOMMAND WRITE-WARMDATA |terminateSystemCommand|
            |genCategoryTable| |npFirstTok| |npItem| |inclmsgConsole|
            |inclmsgFinSkipped| |clearCmdCompletely| |inclmsgIfBug|
            |checkWarningIndentation| |clearCmdSortedCaches|
            |inclmsgCmdBug| |updateCurrentInterpreterFrame|
            |resetInCoreHist| |histFileName| |clearMacroTable|
            |getParserMacroNames| |incConsoleInput|
            |mkLowerCaseConTable| |PARSE-SemiColon| NEXT-SCANNER-CHAR
            |PARSE-InfixWith| EMBEDDED |PARSE-Suffix| |PARSE-Seg|
            SPAD_SHORT_ERROR ERRHUH SPAD_LONG_ERROR IOSTAT GET-INTVAL
            CURRENT-CHAR ADVANCE-CHAR |version| |scanKeyTableCons|
            |scanDictCons| |scanPunCons| |makeClosedfnName| NEXT-CHAR
            BOOT-SKIP-BLANKS NEXT-LINES-CLEAR NEXT-LINES-SHOW
            |syGeneralErrorHere| |npRecoverTrap| PARSE-NUMBER
            ADVANCE-TOKEN |getCodeVector| PARSE-IDENTIFIER
            PARSE-SPADFLOAT PARSE-ARGUMENT-DESIGNATOR PARSE-KEYWORD
            PARSE-SPADSTRING |genTempCategoryTable| |allConstructors|
            |dcSizeAll| |simpTempCategoryTable| |simpCategoryTable|
            |credits| |fin| |saveUsersHashTable|
            |saveDependentsHashTable| |getInfovecCode| |piles|
            |displayExposedGroups| |displayExposedConstructors|
            |displayHiddenConstructors| |scanToken| |startsComment?|
            |scanComment| RESET-HIGHLIGHT |startsNegComment?|
            |displayPreCompilationErrors| CLEAR-HIGHLIGHT
            |scanNegComment| CURRENT-TOKEN |scanPunct|
            REDUCE-STACK-SHOW |scanSpace| |scanString| |scanNumber|
            NEXT-TOKEN |scanEscape| |setViewportProcess| |scanError|
            |waitForViewport| |scanEsc| TOKEN-STACK-SHOW CURRENTTIME
            |NRTgenFinalAttributeAlist| |NRTmakeCategoryAlist| |random|
            INITIALIZE |createResolveTTRules| |createResolveTMRules|
            |createTypeEquivRules| |processGlobals1| |setOptKeyBlanks|
            |initializeRuleSets| |makeConstructorsAutoLoad|
            |processGlobals| |runspad| IS-GENSYM |intSetQuiet|
            |intUnsetQuiet| |spadpo| |PARSE-Iterator|
            |computeElapsedSpace| |displayHeapStatsIfWanted|
            |statisticsSummary| |PARSE-Primary| CURRENT-SYMBOL
            |PARSE-FormalParameterTok| |npVariableName| |npDefinition|
            |npCommaBackSet| |npComma| |npDefinitionlist|
            |npTypeVariablelist| |npSignatureDefinee|
            |npDefaultItemlist| |cacheStats| |clearClams|
            |npSDefaultItem| |clearCategoryCaches| |npDefaultDecl|
            |npDefaultItem| |clearConstructorCaches|
            |npPileDefinitionlist| |npDefTail| YEARWEEK
            |statRecordInstantiationEvent| |npAssignVariablelist|
            |reportAndClearClams| BUMPCOMPERRORCOUNT
            |clearConstructorAndLisplibCaches|
            |clearHashReferenceCounts| |reportInstantiations|
            |removeAllClams| |lbrkSch| |npExit| |rbrkSch| |npLambda|
            |npDef| |npVariable| |npQualType| |npSQualTypelist|
            |sayNewLine| |npQualTypelist| |reportWhatOptions|
            |npTypeVariable| |npLocalDecl| |npSLocalItem| |npLocalItem|
            |npLocalItemlist| |npDefinitionOrStatement| |peekTimedName|
            |npSingleRule| |clamStats| |statRecordLoadEvent|
            |npCategoryL| |popTimedName| |computeElapsedTime|
            |pfNoPosition| |npSCategory| FOAM:|fiGetDebugVar|
            |npDefaultValue| |npSignature| |pspacers| |npSigDecl|
            |npSigItem| |prTraceNames| |npSigItemlist| |npSemiBackSet|
            |npVariablelist| |asList| |npRemainder| |cc| |npTerm|
            MONITOR-RESULTS |npSum| |initializeSystemCommands|
            MONITOR-READINTERP |npArith| |npSegment| |tempLen|
            |npInterval| |extendConstructorDataTable| |npBy|
            |npAmpersandFrom| MONITOR-HELP |npSynthetic| |npRelation|
            |writeHistModesAndValues| |npQuiver| |npDiscrim|
            |reportCount| |npDisjand| |clearFrame| MONITOR-INITTABLE
            |npSuch| MONITOR-END |npMatch| |npQualifiedDefinition|
            |npConditionalStatement| |printableArgModeSetList| |npADD|
            |npExpress1| |npIterators| MONITOR-UNTESTED
            |nangenericcomplex| |npForIn| $TOTAL-ELAPSED-TIME
            |updateInCoreHist| |npIterator| |writeHiFi| |npWhile|
            |npSuchThat| MONITOR-AUTOLOAD COMPRESSOPEN $TOTAL-GC-TIME
            |npExpress| |currentSP| INTERPOPEN |updateHist| |npLoop|
            MONITOR-REPORT |initialiseIntrinsicList|
            CREATE-INITIALIZERS |npIterate| |getIntrinsicList|
            |npReturn| MONITOR-PERCENT |npBreak| |npFree| |npImport|
            INIT-BOOT/SPAD-READER |npInline| |PARSE-Expression|
            |npLocal| |intNewFloat| POP-REDUCTION |npExport| |npTyping|
            FAIL |npVoid| |processSynonyms| |npStatement|
            |PARSE-NewExpr| |makeInitialModemapFrame| |npMdef|
            |loadExposureGroupData| |statisticsInitialization| |npMDEF|
            |initializeInterpreterFrameRing| |npAssignment|
            |spadStartUpMsgs| |npAssignVariable| RESTART0 |npGives|
            FRICAS-INIT |spad| |npDecl| |npAssignVariableName|
            |traceReply| |oldParserAutoloadOnceTrigger|
            |mkOutputConsoleStream| |npAssign| |npPileExit|
            |resetStackLimits| |ptimers| |ncTopLevel| |pcounters|
            |ncIntLoop| |resetSpacers| |intloop| |resetTimers|
            |describeSetFortTmpDir| |npId| |resetCounters|
            |npSymbolVariable| |?t| |describeSetFortDir|
            |describeSetLinkerArgs| |sayShowWarning|
            |describeSetFunctionsCache| |sayAllCacheCounts| |npAtom1|
            |describeSetOutputAlgebra| |npLet|
            |describeSetOutputFortran| |npFix| |synonymSpad2Cmd|
            |npBPileDefinition| |getSystemCommandLine|
            |describeSetOutputMathml| |describeSetOutputOpenMath|
            |npAtom2| |newFortranTempVar| |describeSetOutputFormula|
            |npPDefinition| |describeSetOutputTex| |npName|
            |npConstTok| |npDollar| |spadPrompt| |npBDefinition|
            |stopTimer| |npFromdom| |poNoPosition| |npInfixOperator|
            |describeOutputLibraryArgs| |npAmpersand|
            |describeInputLibraryArgs| |npPrefixColon|
            |intSetNeedToSignalSessionManager| |npEncl| |voidValue|
            |startTimer| |npApplication| |clock|
            |displayWorkspaceNames| |spadReply| |npPrimary2|
            |npSelector| |npPrimary| |getParserMacros|
            |getWorkspaceNames| |npApplication2| |npType|
            |allOperations| |interpFunctionDepAlists|
            |getInterpMacroNames| |npCoerceTo| |npRestrict| |npPretend|
            |npColonQuery| |npTypeStyle| |npTypified| |npTagged|
            |previousInterpreterFrame| |npColon| |npPower|
            |nextInterpreterFrame| |npProduct| |initHistList|
            |PARSE-TokTail| |npQualDef|
            |updateFromCurrentInterpreterFrame| |PARSE-PrimaryNoFloat|
            |npPop1| |createCurrentInterpreterFrame| |PARSE-ElseClause|
            |frameNames| |PARSE-Float| |historySpad2Cmd|
            |oldHistFileName| |PARSE-Loop| |PARSE-String|
            |PARSE-Import| |PARSE-IntegerTok| |npNext| |PARSE-With|
            |PARSE-Data| |PARSE-LabelExpr| |PARSE-AnyId| |PARSE-Return|
            |PARSE-Exit| |PARSE-ReductionOp| |PARSE-Sequence|
            |PARSE-Conditional| |PARSE-OpenBrace| |returnToTopLevel|
            |PARSE-Sequence1| |returnToReader| |PARSE-OpenBracket|
            |pfNothing| |PARSE-FormalParameter| |PARSE-IteratorTail|
            |PARSE-Qualification| |npTrap| |npPop3| |PARSE-Infix|
            |npPop2| |PARSE-Prefix| |npPPff| |PARSE-Quad|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) *) NEXT-LINE INIT-MEMORY-CONFIG |Undef|
            $ERASE META-SYNTAX-ERROR |newGoGet|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) *) GET-TOKEN |pfFileName| |overlabelWidth|
            |overbarWidth| |setExposeAdd| |setExposeDrop| |agggsub|
            |agggsuper| |agggwidth| |parseAndEvalStr| |protectedEVAL|
            |parseTransform| MONITOR-PRINTREST |parseAndEvalStr1|
            |outputTran| |systemCommand| |rootSub| |startHTPopUpPage|
            |startReplaceHTPage| |killHTPage| |linkToHTPage|
            INIT-FILE-GETTER |abbreviationsSpad2Cmd|
            INIT-LIB-FILE-GETTER |postInSeq| |whatSpad2Cmd|
            |clearSpad2Cmd| |exptSub| |sayDisplayWidth| |postMakeCons|
            |sayDisplayWidth,fn| |sayWidth,fn| |aggSub|
            |fixObjectForPrinting| |aggSuper| |outformWidth| |quoteSub|
            |quoteSuper| |sayDisplayStringWidth| |clearCmdParts|
            |transSeq| |parseUpArrow| |ExecuteInterpSystemCommand|
            |abbQuery| OBEY VMLISP::DELETE-DIRECTORY |parseLhs|
            VMLISP::GET-IO-INDEX-STREAM |aggwidth| |parseTranList|
            |apropos| VMLISP::GET-INPUT-INDEX-STREAM
            |NRTinnerGetLocalIndex| WIDTH |makeSimplePredicateOrNil|
            |assignSlotToPred| |matSub| DEFTRAN BPINAME
            |incHandleMessage| |editFile| |subspan|
            |escapeSpecialChars| |postCategory,fn| |superspan|
            |NRTtypeHack| |brightPrint0| |brightPrint0AsTeX|
            |brightPrintHighlight| |abbreviations|
            |brightPrintHighlightAsTeX| |compiler| |brightPrintCenter|
            |cd| |halfWordSize| |brightPrintCenterAsTeX| |clear|
            |close| |poFileName| |display| |frame| |history|
            |savesystem| |mathprintWithNumber| |set| |parseLeftArrow|
            |show| |summary| |printBasic| |what| |spleI| MAKE-OUTSTREAM
            |verbatimize| |ncAlist| |ncTag| |transHasCode| |obj2String|
            |startHTPage| |issueHT| |getMsgPos2| |spadcall1|
            |getBrowseDatabase| MAKE-APPENDSTREAM MAKE-INSTREAM
            |setOutputAlgebra| |getMsgTag| |NRTevalDomain| |postTran|
            |intern| |justifyMyType| |mathObject2String| DEF-MESSAGE1
            |compQuietly| |compileFileQuietly| |parseAnd| |parseIf|
            |parseNot| |parseOr| |parseSeq| |parseTran| |postSlash|
            |postConstruct| |postBigFloat| |compileQuietly|
            |tuple2String,f| |clearClam| |mkEvalable| |reportOpSymbol|
            SRCABBREVS |mac0InfiniteExpansion,name| |entryWidth|
            |formString| |sayMSGNT| |object2String| |replaceSharpCalls|
            |mkEvalableCategoryForm| |sayWidth| |npPC| BRIGHTPRINT-0
            |doReplaceSharpCalls| |htCommandToInputLine|
            MAKE-REASONABLE |normalizeStatAndStringify| |roundStat|
            |lnFileName| |tabsToBlanks| |unAbbreviateKeyword|
            |form2String1| |asyJoinPart| |poGlobalLinePosn|
            |timedEvaluate| |pfGlobalLinePosn| |phBegin|
            DEF-WHERECLAUSELIST |writifyComplain| |doSystemCommand|
            DEF-ISNT |subrname| |parseLessEqual| MONITOR-RESTORE
            |parseGreaterEqual| |optCall| |parseNotEqual| |numMapArgs|
            |optCallEval| MAKE-DEPSYS |initializeLisplib| |safeWritify|
            |numArgs| |npPP| |optSPADCALL| BOOT-LOAD |showSpad2Cmd|
            |reportOpsFromUnitDirectly0| |reportOpsFromUnitDirectly1|
            |InterpExecuteSpadSystemCommand| |tokType|
            |setOutputFortran| |asyGetAbbrevFromComments|
            |setOutputMathml| |setOutputOpenMath| |setOutputFormula|
            |exp2FortOptimizeArray| |setOutputTex| |compileAsharpCmd1|
            |withAsharpCmd| |fortError1| |setExpose| |displaySpad2Cmd|
            |setLinkerArgs| MONITOR-EVALAFTER |setFunctionsCache|
            |setStreamsCalculate| |fortPre1| |qTSub| |qTSuper|
            |mkParameterList,par2string| |subSuper|
            |fortFormatCharacterTypes,par2string|
            |fortFormatCharacterTypes,mkCharName| |frameSpad2Cmd|
            |importFromFrame| |fix2FortranFloat| |checkPrecision|
            |addNewInterpreterFrame| |vConcatSuper| |mathPrint|
            |histFileErase| |setHistoryCore| |saveHistory|
            |showHistory| |changeHistListLen| |restoreHistory|
            |sayString| FILE-RUNNER |popSatOutput| |porigin| |pfname|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) T) READ-A-LINE |hasFileProperty|
            |throwMessage| |isLegitimateMode| |resolveTT| |selectMms1|
            MAKE-STACK NEXT-BOOT-LINE |Union| |Mapping| |Enumeration0|
            BOOT |RecordCategory| INITROOT |EnumerationCategory|
            |UnionCategory| |displayCategoryTable| |incLude1|
            META-META-ERROR-HANDLER MAKE-REDUCTION
            FOAM::MAKE-FOAMPROGINFOSTRUCT MAKE-TOKEN $FILEP
            VMLISP::MAKE-LIBSTREAM |canCoerce| |canCoerceFrom|
            |coerceConvertMmSelection| |dcSize| MAKE-DATABASE
            SPAD_SYNTAX_ERROR |sum| |runOldAxiomFunctor|
            |buildBitTable| |synonym| MAKE-LINE MAKE-XDR-STREAM
            RDEFINSTREAM RDEFOUTSTREAM |incRgen1| |incIgen1| |incZip1|
            |incAppend1| |next1| |nextown1| MAKE-MONITOR-DATA |dc| SAY
            |concat| MONITOR-ENABLE MONITOR-DISABLE MONITOR-RESET
            MONITOR-TESTED |start| CROAK RKEYIDS IOCLEAR SPAD MOAN
            ENABLE-BACKTRACE INTERRUPT TOPLEVEL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) CHARACTER) LINE-CURRENT-CHAR EBCDIC NUM2CHAR)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) FIXNUM) LINE-NEW-LINE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) STRING) LINE-BUFFER |stripSpaces|
            |make_spaces|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) T) |pfNoPosition?| |pfSecond|
            |npListAndRecover| TOKEN-PRINT |pfImmediate?| |pfAppend|
            /TRACE-0 |pfCharPosn| LINE-AT-END-P /OPTIONS |pfLinePosn|
            |npPileBracketed| FOAM-USER::|AXL-spitSInt|
            |overlabelSuper| REDUCTION-RULE |%id| |incFile|
            |IdentityError| |%origin| |retract1| NREVERSE0 STACK-P
            |overbarSuper| OPTIONS2UC |%fname| |retract2Specialization|
            |npInfGeneric| LINE-ADVANCE-CHAR |isFunctor| |%pos|
            |isValidType| GET-A-LINE |fileNameStrings| |pfIdSymbol|
            MAKE-STRING-ADJUSTABLE |pfMLambda?| |indefIntegralSub|
            |pf0MLambdaArgs| |spadThrowBrightly| |indefIntegralSup|
            |coerceUnion2Branch| |translateYesNo2TrueFalse|
            |underDomainOf| |npRestore| |pfAbSynOp|
            |initializeSetVariables| |isUncompiledMap|
            |indefIntegralWidth| |hashString| |isInterpOnlyMap|
            |intSub| |isDomainOrPackage| |pfLeafPosition| |intSup|
            |pf2Sex1| |quote2Wrapped| |npInfKey| |pfSymbol?|
            |translateTrueFalse2YesNo| |npDDInfKey|
            |isSubForRedundantMapName| |intWidth| |pfSymbolSymbol|
            |upisAndIsnt| |pfLiteral?| |removeConstruct| |npElse|
            |getOutputAbbreviatedForm| |pfLiteral2Sex| COND-UCASE
            |printMap| |setExposeAddGroup| |pfApplication2Sex|
            |isLocalPred| |setExposeAddConstr| /UNTRACE-0
            |setExposeDropGroup| |pfTuple?| GET-BOOT-TOKEN |error|
            |setExposeDropConstr| |pf0TupleParts| /UNTRACE-REDUCE
            |pfIf?| |upLETWithPatternOnLhs| |untraceDomainConstructor|
            |pfIfCond| |Record0| |pfIfThen| SMALL-ENOUGH |mkZipCode|
            |pfIfElse| |pfTagged?| |pfTaggedTag| |simpHasPred,simp|
            HAS_SHARP_VAR |pfTaggedExpr| |parseAndInterpret|
            |pfCoerceto?| |hasIdent| |extsub| |iterVarPos|
            |oldParseAndInterpret| |pfCoercetoExpr| |simpHasPred,eval|
            IS_SHARP_VAR |extsuper| |mkNestedElts| |pfCoercetoType|
            |postTransform| |extwidth| |pfPretend?|
            |simpHasPred,simpDevaluate|
            |parseAndEvalToStringForHypertex| |pfPretendExpr|
            VMLISP::SPAD-FIXED-ARG |simpBool| |fracsub| |pfPretendType|
            MONITOR-BLANKS |fracsuper| |pfFromdom?|
            |getCategoryExtensionAlist0| |fracwidth|
            |parseAndEvalToHypertex| |pfFromdomWhat| |slashSub|
            |parseAndEvalToString| |opTran| |addToCategoryTable|
            |slashSuper| |parseAndEvalToStringEqNum| |pfFromdomDomain|
            |simpCatPredicate| |parseAndInterpToString|
            |pfSequence2Sex| |slashWidth| |pfExit?| |pfExitCond|
            |getCategoryExtensionAlist| /UNEMBED-1 |pfExitExpr|
            |boxSub| |pfLoop?| |boxSuper| |pf0LoopIterators| |boxWidth|
            |upADEF| |loopIters2Sex| |upAlgExtension| |pfCollect?|
            |upand| |upor| |mkCategoryExtensionAlist| |upcase|
            UNSQUEEZE |isFormalArgumentList|
            VMLISP::GET-INDEX-TABLE-FROM-STREAM |upTARGET|
            |mkCategoryExtensionAlistBasic| |zagWidth| |upCOERCE|
            |astran| |rootWidth| |falseFun| |upconstruct| |rootSuper|
            |upDeclare| SET-FILE-GETTER |isOkInterpMode| |simpOrUnion|
            |matWidth| |minusWidth| |constructor?| |pathnameType|
            |incNConsoles| |KeepPart?| |stringWidth| |incPos| |sayMath|
            |categoryParts,exportsOf| |helpSpad2Cmd|
            |getCategoryOpsAndAtts| |updateCategoryTableForCategory|
            |commandsForUserLevel| |getFunctorOpsAndAtts| |postDefArgs|
            |stepSub| |clearTempCategoryTable| |satisfiesUserLevel|
            |transformOperationAlist| |stepSuper| |clearCategoryTable|
            |postDoubleSharp| |stepWidth| |showCategoryTable| SQUEEZE
            VMLISP::GET-DIRECTORY-LIST |killColons|
            |makeOldAxiomDispatchDomain| |inclmsgPrematureEOF|
            |removeSuperfluousMapping| |inSub| |newConstruct| |inSuper|
            |inclmsgPrematureFin| |newIf2Cond| |inWidth| |tokPosn|
            |postIteratorList| |makeLazyOldAxiomDispatchDomain|
            |inclmsgNoSuchFile| |pfDocument| |inclmsgCannotRead|
            |newDef2Def| |pfListOf| |concatSub| |inclmsgConActive|
            |postBlockItem| |concatSuper| |inclmsgConStill|
            |postTransformCheck| |concatWidth| |postCapsule|
            |concatbWidth| DATABASE-P |comma2Tuple| |clearCmdExcept|
            |inclmsgSay| |postQuote| |exptSuper| |Top?| |tuple2List|
            |exptWidth| |Else?| |say2PerLineThatFit| |timesWidth|
            |checkWarning| |sayLongOperation| |deleteFile|
            |postSEGMENT| |incClassify| |aggWidth| |clearParserMacro|
            |Skipping?| |postElt| |augmentLowerCaseConTable|
            |getConstructorAbbreviation| |postSequence|
            |closeInterpreterFrame| |compileAsharpCmd| |abbreviation?|
            |splitSayBrightlyArgument| |parseTran,g| |splitSayBrightly|
            |transIs1| |setAutoLoadProperty| |splitListSayBrightly|
            |transIs| |dbSpecialDisplayOpChar?| |quoteWidth|
            PREPARSEREADLINE |sigmaSub| |incFileInput| SKIP-TO-ENDIF
            |sigmaSup| |getPartialConstructorModemapSig| SKIP-IFBLOCK
            |postError| |readLibPathFast| |parseJoin,fn| |sigmaWidth|
            PREPARSEREADLINE1 |sigma2Width| |objMode| |sigma2Sub|
            |isDomainValuedVariable| |sigma2Sup| |parseAtom| |piSub|
            |isListConstructor| INFIXTOK |center80| |piSup|
            |parseHas,fn| |If?| PARSEPRINT |parseHasRhs|
            FOAM::TYPE2INIT |Elseif?| PREPARSE1 |piWidth| |pi2Width|
            |eq2AlgExtension| |domainForm?| |SkipEnd?| |pi2Sub|
            |packageForm?| |SkipPart?| |pi2Sup| |parseHas,mkand|
            |getImmediateSuperDomain| PREPARSE-ECHO |upCOLLECT1|
            |maximalSuperType| ATENDOFUNIT |upCOLLECT0|
            |getLisplibName| |incStringStream| |interpOnlyCOLLECT|
            |getConstructorUnabbreviation| |isStreamCollect|
            |parseType| |intProcessSynonyms| |parseTypeEvaluate|
            |unabbrevAndLoad| |upLoopIters| |isNameOfType| SHUT COPY
            DOWNCASE |evalLoopIter| |interpIter| |parseCases| TOKEN-P
            |outputTranIterate| REROOT |upCOLLECT| DIG2FIX STACK-CLEAR
            |pathname?| |upStreamIters|
            |NRTgetOperationAlistFromLisplib| |setDefOp|
            |isExistingFile| |addConsDB| |transUnCons| |unInstantiate|
            |markUnique| |parseBigelt| |loadIfNecessaryAndExists|
            |mkAndApplyPredicates| LOG2 |NRTaddInner| |mathprint|
            |lfspaces| SIZE |mapConsDB| SPAD_ERROR_LOC |lfstring| EOFP
            |scanTransform| LINE-PRINT |zagSuper| RSHUT |zagSub|
            |scanW| LINE-PAST-END-P |matSuper| |keyword?| DEF-IS
            |rdigit?| |lfinteger| |bootOR,flatten| |matLSum2|
            |bootAND,flatten| MAKE-ADJUSTABLE-STRING |matSubList|
            |bootLabelsForGO| |matLSum| |bootIF| |pfMacroLhs|
            |matSuperList| |lferror| LINE-SUBSEQ-FROM |bootOR|
            |pfMacroRhs| |sumoverlist| |bootAND| |pfSourcePosition|
            DIGITP |compTran| |compNewnam| |sumWidthA| |compTran1|
            VEC2LIST PUSHLOCVAR GET-SPECIAL-TOKEN
            |mkAlistOfExplicitCategoryOps| |lambdaHelper1|
            GET-SPADSTRING-TOKEN MAKE-VEC REDUCTION-VALUE
            |lambdaHelper2| |interactiveModemapForm| |vConcatWidth|
            GCMSG |compExpand| |formal2Pattern| |mathPrintTran|
            PREPARSE |compFluidize| |incRenumber| |mkMapPred|
            PRINT-PACKAGE |mapPredTran| |namestring| |bitsOf|
            |listOfPatternIds| |compFluidize1| GET-SPADNUM-TOKEN
            |mkEvalableUnion| |isSharpVarWithNum| |depthAssocList|
            |mkDatabasePred| |removeEXITFromCOND?| |mkEvalableMapping|
            |NRTcatCompare| |bootTransform| |string2Words|
            |loadIfNecessary| |hasDefaultPackage| MSORT
            GET-ARGUMENT-DESIGNATOR-TOKEN |postcheck|
            |segmentedMsgPreprocess| |evaluateSignature|
            |encodeCatform| FOAM::INSERT-TYPES |incCommand?| |postForm|
            |isLocalVar| |evaluateType| |signatureTran| |isFreeVar|
            |postAtom| |evaluateType1| |depthAssoc|
            BOOT-TOKEN-LOOKAHEAD-TYPE |postTranList| |operationLink|
            |getCatAncestors| |postOp| |listOfVariables| |addBlanks|
            |evaluateFormAsType| VMLISP::REMOVE-FLUIDS |dcAll|
            |isDomainSubst| |incFileName| |postBlock| |noBlankAfterP|
            |getAndEvalConstructorArgument| |dcSlots| |pfSourceStok|
            |postBlockItemList| |incBiteOff| |isInternalMapName|
            |noBlankBeforeP| |makeOrdinal| |dcAtts| |dcCats| |npMoveTo|
            |loadFunctor| |unTuple| |dcData|
            |interactiveModemapForm,fn| |isRationalNumber| |dcOps|
            |new2OldDefForm| |args2Tuple| |isQuotient| BUMPERRORCOUNT
            |devaluateDeeply| |getLisplibVersion| |new2OldTran|
            |dcOpTable| |postType| |minimalise,min| TOKEN-SYMBOL
            |exptNeedsPren| |minimalise,HashCheck| |formatSlotDomain|
            |moveORsOutside| |minimalise| |isFreeFunctionFromMmCond|
            |height| |isFreeFunctionFromMm| |getOpSegment|
            VMLISP::GETINDEXTABLE |getDomainFromMm| |bright| |syminusp|
            |getUnnameIfCan| |dcCats1| |isExposedConstructor| |absym|
            |blankIndicator| |evaluateType0| |dcData1|
            |flattenSignatureList| |getConstrCat| |numberOfNodes|
            |mkAlistOfExplicitCategoryOps,atomizeOp| |vectorSize|
            |putWidth| |brightPrintRightJustify| |nodeSize|
            |clearAllSlams| |tabber| |maprin0| |brightPrint1|
            |updateCategoryFrameForCategory|
            |getOperationAlistFromLisplib| |compressHashTable|
            |updateCategoryFrameForConstructor| |ppTemplate|
            |brightPrint| |maprinChk| |frameName| |dcPreds|
            |maprinRows| |sayBrightlyLength1|
            VMLISP::LIBSTREAM-INDEXTABLE |getExportCategory| |maPrin|
            |opOf| |unVectorize| VMLISP::LIBRARY-FILE |deMatrix|
            |poLinePosn| |largeMatrixAlist| |edit|
            |SubstWhileDesizingList| |getUnname1| |PushMatrix| |help|
            |getUsersOfConstructor| |whichCat| |SubstWhileDesizing|
            |getDependentsOfConstructor| |getFirstArgTypeFromMm|
            |library| |load| |getUserIdentifiersIn| |getLinePos|
            |getConstructorModemap| |ltrace| |getLineText| |nopiles|
            |getOplistForConstructorForm| |getMsgPosTagOb|
            |concatTrouble,fixUp| |stuffDomainSlots| |predicateBitRef|
            |dropPrefix| |From| |makePredicateBitVector| |To|
            |formulaFormat| |transCategoryItem| |read| |texFormat|
            |template| MAKE-CVEC |makeArgumentIntoNumber|
            |mathmlFormat| |srcPosFile| |dispfortexp| |srcPosLine|
            |spool| |outputOp| |srcPosSource| |findEqualFun|
            |sayRemoveFunctionOrValue| |srcPosColumn| |isInitialMap|
            |asTupleAsVector| FOAM:|fiSetDebugVar| |trace| |undo|
            |objValUnwrap| |charyTopWidth| VMLISP::EQUABLE
            |getUserIdentifiersInIterators| |workfiles|
            |getIteratorIds| |isLeaf| |nextline| |mkAliasList|
            |computedMode| |mkPredList| |getFlag| LINE-P
            |stripUnionTags| |walkForm| TRY-GET-TOKEN |evalDomain|
            |sayMessage| |flattenSemi| |getIProplist|
            |isConstantArgument| |removeBindingI| |isPatternArgument|
            |lfid| DEF-PROCESS |coerceMap2E| CACHEKEYEDMSG |getSrcPos|
            |punctuation?| XDR-STREAM-NAME |srcPosDisplay|
            INITIALIZE-PREPARSE |simplifyMapPattern,unTrivialize|
            |scanWord| |knownEqualPred| |simplifyMapConstructorRefs|
            |constructorCategory| |predTran| |systemErrorHere|
            |getEqualSublis| |asTupleNewCode0| |digit?| |asTupleSize|
            |analyzeMap,f| |isDefaultPackageName| |isRecord|
            |gensymInt| |lfnegcomment| |lfcomment| |scanKeyTr|
            LINE-NEXT-CHAR XDR-STREAM-P |makeDomainTemplate|
            |scanPossFloat| |compFailure| |makeCompactDirect| |lfkey|
            |scanCloser?| |combineMapParts| |killNestedInstantiations|
            |makeCompactSigCode| |removeBodyFromEnv| XDR-STREAM-HANDLE
            |predicateBitIndex| FOAM:|formatDFloat| |getMsgKey|
            |orderBySubsumption| FOAM:|formatSFloat| |getMsgPrefix?|
            |verifyRecordFile| |makeCompactDirect1,fn|
            FOAM:|formatBInt| |htTrimAtBackSlash| |getLookupFun|
            FOAM:|formatSInt| |msgImPr?| |msgOutputter| |last|
            |makeSpadConstant| |remWidth| |getMsgInfoFromKey|
            |simplifyAttributeAlist| |untraceMapSubNames| S-PROCESS
            |getMsgKey?| |bustUnion| |predicateBitIndexRemop|
            |macSubstituteOuter| |removeAttributes| TRIMSTRING
            |getMsgArgL| /VERSIONCHECK |removeAttributePredicates|
            |pfNothing?| |stringer| |posPointers| |isHasDollarPred|
            |pfMLambdaBody| |getMsgFTTag?| |stripOutNonDollarPreds|
            |maprin| |orderByContainment| |makeByteWordVec|
            |texFormat1| |getMsgPos| |hashable| |poCharPosn|
            |outputTranMatrix| COMP370 |outputMapTran| |getMsgPrefix|
            |removeAttributePredicates,fn| VMLISP::VARP |flattenOps|
            GETREFV |removeAttributePredicates,fnl| |outputTranIf|
            |makeLeaderMsg| |log| VMLISP::PROBE-NAME
            |outputTranCollect| BVEC-COPY |outputTranReduce| MBPIP
            |outputTranRepeat| |getMsgText| |StreamNull|
            |outputTranSEQ| |erMsgSort| QSORT |outputConstructTran|
            |makeMsgFromLine| |eqType| PLACEP |incRgen| |listOutputter|
            GENSYMP |erMsgSep| |equiType| |incIgen|
            |outputTranIteration| |isPolynomialMode|
            |outputTranMatrix,outtranRow| |listOfDuplicates| BVEC-NOT
            |poNopos?| |isLegitimateRecordOrTaggedUnion| |keyp|
            |underDomainOf;| |closeOldAxiomFunctor| |deconstructT|
            |getUnderModeOf| HKEYS |mac0Get| |rightTrim|
            |processChPosesForOneLine| |leftTrim| |msgNoRep?|
            |expandMacros| |pfApplicationOp| |charDigitVal|
            |getStFromMsg| |bool| |leader?| |npNull| |length2?| |line?|
            |walkWhereList| |toScreen?| |toFile?| |getCon|
            |alreadyOpened?| |isNiladic| |alqlGetKindString|
            |getMsgToWhere| |alqlGetOrigin| UPCASE |handleKind|
            |alqlGetParams| |getPreStL| |postMapping|
            |dropLeadingBlanks| |postExit| NMSORT |getPosStL|
            |postTuple| |orderList| |getMsgLitSym| |pf0ApplicationArgs|
            |tabbing| DEF-COLLECT |str2Tex| |showMsgPos?|
            |isCategoryPackageName| DEF-SEQ FBPIP |packageTran|
            |convertOpAlist2compilerInfo| LIST2CONS
            |loadLibIfNotLoaded| |remLine| LIST2CONS-1 |trimString|
            |remFile| DEF-LESSP |pr| |msgLeader?| SMINT-ABLE
            |poPosImmediate?| |macExpand| |recordAndPrintTest|
            |isDomain| DEF-SETELT |pfWhere?| |getDomainHash| DEF-ELT
            |macWhere| |DEF-:| |pfLambda?| DEF-RENAME1 COMP
            MONITOR-DELETE |script2String| DEF-MESSAGE |loadLib|
            |linearFormatName| DEF-STRING |isSystemDirectory|
            |numOfSpadArguments| |printAsTeX| DEF-COND COMP-2
            DEF-IS-REMDUP1 COMP-1 |segmentKeyedMsg| DEF-INSERT_LET1
            |object2Identifier| |blankList| |string2Float|
            |compAndDefine| |form2StringList|
            |ncParseAndInterpretString| |makeOutputAsFortran|
            |addSpaces| |constructor| |macLambda| |pair2list|
            |pfMacro?| |vec2Lists1| |as| UNEMBED |macMacro| |macId|
            |vec2Lists| |isSharpVar| |complexRows| |bassertNot|
            |pathname| |spad2lisp| |testPredList| |mkEvalableRecord|
            IVECP |printNamedStats| LIST2VEC VMLISP::LIBSTREAM-P
            OPTIONAL |bubbleType| |int2Bool| |timedAlgebraEvaluation|
            |variableNumber| |timedOptimization| |timedEVALFUN|
            |PARSE-Expr| |lispType| |checkForBoolean| IS-CONSOLE
            |pfApplication?| |resolveTTRed3| |macApplication|
            |mkAtreeExpandMacros| |interpOp?| |nakedEXIT?| |mkAtree1|
            |mkCircularAlist| |flattenCOND| |infovec| |mergeableCOND|
            |removeEXITFromCOND| |pfPosOrNopos| |mergeCONDsWithEXITs|
            |poGetLineObject| |nodeCount| |length1?| |lnPlaceOfOrigin|
            |extractCONDClauses| ACTION |pfSourceText|
            VMLISP::SIMPLE-ARGLIST |bootCOND| MAKE-ABSOLUTE-FILENAME
            |bootPushEXITintoCONDclause| |isInterpMacro| |parseColon|
            EXPAND-TABS |bootTran| |PARSE-LedPart| |parseCoerce|
            |getCacheCount| |bootSEQ| STACK-SIZE |transformCollect|
            |parseAtSign| |breakIntoLines| |bootPROGN|
            |getDomainByteVector| |transformREPEAT|
            |bootAbsorbSEQsAndPROGNs,flatten| |parseCategory|
            |clearSlam| |bootAbsorbSEQsAndPROGNs|
            |isDefaultPackageForm?| |multiToUnivariate|
            |parseConstruct| |tryToRemoveSEQ| |collectDefTypesAndPreds|
            |functionAndJacobian| |parseDEF| |mkAtreeValueOf|
            |resolveTMRed1| |vectorOfFunctions| |parseExit| |destructT|
            |parseHas| MKPROGN |nontrivialCosig| |mkAtreeValueOf1|
            |parseIn| |parseInBy| |parseIs| |parseIsnt| |parseJoin|
            |parseLeave| |parseLET| |constructorName| |parseMDEF|
            |record2String| MONITOR-DATA-NAME MONITOR-DATA-MONITORP
            |listOfSharpVars| |freeOfSharpVars| MONITOR-DATA-SOURCEFILE
            |removeZeroOne| |parsePretend| |removeZeroOneDestructively|
            |isValidType;| |parseReturn| |Identity| |isPartialMode|
            |parseSegment| |formJoin2String| |getInfovec| |break|
            |tuple2String| |getConstructorSignature| |parseWhere|
            |formCollect2String| |matrix2String| |formatUnabbreviated|
            |containsVars1| |categoryForm?| |appOrParen|
            |formatUnabbreviatedTuple| |postWith| |isBinaryInfix|
            |formatUnabbreviatedSig| |newHasTest,evalCond|
            |binop2String| |matrix2String,outtranRow| |parse2Outform|
            |postQUOTE| |sumOrParen| |str2Outform| |isNewWorldDomain|
            |postCollect| |productOrParen| |wrap| |powerOrParen|
            |postin| |coerceOrParen| |npQualified| |postIn|
            |postRepeat| |pfTupleListOf| |formJoin2| |npTuple|
            |compileInteractive| |postTupleCollect| |pp| |postAdd|
            |formatAttribute| |formTuple2String| |npBracketed|
            |postReduce| |formIterator2String| |stopTimingProcess|
            |postComma| |postSemiColon| |postWhere| |linearFormat|
            |pfTyping| |postColonColon| |formatArgList| |postColon|
            |clearCategoryCache| |postAtSign| |isInternalFunctionName|
            FOAM::FOAM-FUNCTION-INFO |postPretend| |form2Fence|
            |postIf| |pred2English| |pfSequence| |reportHashCacheStats|
            |postJoin| |formatPredParts| |ncParseFromString|
            |displayCacheFrequency| |postSignature| |pfCheckItOut|
            |mkHashCountAlist| |makeFort,untangle2| |postCategory|
            |form2Fence1| |remHashEntriesWith0Count|
            |makeFort,untangle| |postDef| |postMDef| |form2FenceQuote|
            |numberOfEmptySlots| |string2SpadTree| LASTATOM
            |form2FenceQuoteTail| |pfCheckMacroOut| SOURCEPATH
            |formatOpType| MONITOR-INCR |formatOperationAlistEntry|
            |getCType| |pfSequenceToList| |processKeyedError|
            |formatOpConstant| |pfSequenceArgs|
            |outputDomainConstructor| |formatSignatureAsTeX| |npParse|
            |typeTimePrin| |formatSignatureArgs| |mac0GetName|
            |putDatabaseStuff| |constructor2ConstructorForm| REMDUP
            |expr2String| |initImPr| ASSOCLEFT |prefix2StringAsTeX|
            |initToWhere| ASSOCRIGHT |form2StringWithWhere|
            |getMsgTag?| |form2StringWithPrens| |getModeSet|
            |unparseInputForm| |getModeSetUseSubdomain|
            |clearConstructorCache| |XDRFun| |prefix2Infix| |isMap|
            |displayHashtable| |spadTypeTTT| |mkQuote,addQuote|
            |keyword| |CompStrToString| |makeLispList| STREAM-EOF
            |DNameToSExpr1| |DNameToSExpr| |mkQuote| |DNameFixEnum|
            |domainDepth| |StringToCompStr| |stripNil| |sayTeX|
            |makeUnion| |constructSubst| |instantiate| |saySpadMsg|
            |displayTranModemap| |selectMostGeneralMm| |sayALGEBRA|
            |quoteCatOp| |sayMSG| |formatModemap| |isHomogeneousList|
            |opIsHasCat| |sayMSG2File| |hashCode?| |sayFORTRAN|
            |intpart| |sayFORMULA| |cleanUpSegmentedMsg| |matchMmCond|
            |sayModemap| |listOfPredOfTypePatternIds| |pfReturnNoName|
            |isPatternVar| |npCompMissing| |findSubstitutionOrder?|
            VMLISP::COMPILE1 |pfLoop1| |pfSuchthat| |initCache|
            |prefix2String| |pfWhile| |formatModemap,fn| |evalMmStack|
            |canRemoveIsDomain?| |pfBreak| DROPTRAILINGBLANKS
            |pfIterate| |fixUpTypeArgs| |removeIsDomains|
            |devaluateList| |removeIsDomainD| |containsVars|
            |CDRwithIncrement| |undoCount| |formatIf| |pfParts|
            |evalMmStackInner| |reportUndo| |pfUnSequence| |evalMmDom|
            |recordFrame| |getSymbolType| |pfImport| BRIGHTPRINT
            |undoLocalModemapHack| |unabbrev| |pfInline| |hashCount|
            VMLISP::LIBSTREAM-MODE |form2String| |evaluateLines|
            MESSAGEPRINT |whatSpad2Cmd,fixpat| |noSharpCallsHere|
            MESSAGEPRINT-1 |pfPosn| MESSAGEPRINT-2 |whatCommands|
            |formatSignature| |lnString| |whatConstructors|
            |specialChar| |poIsPos?| |be| |systemError|
            |formatAttributeArg| |asyTypeJoinStack| |lnFileName?|
            |reduceDnf| |pp2Cols| |formatMapping| |pfFree|
            |asyTypeJoinPartWith| |pfFirst| |dnf2pf| |devaluate|
            |prefix2String0| |pfLocal| |orderMmCatStack|
            |asyTypeJoinPartExport| |pfSourcePositionlist|
            |synonymsForUserLevel| |dollarPercentTran| |pfExport|
            |asyTypeJoinPartIf| |pfSourcePositions| MACROEXPANDALL
            |formatSignature0| FOAM:|fiStrHash| |asyTypeJoinItem|
            |recordAndPrintTest,fn| |lnLocalNum| |formatSignatureArgs0|
            |npLetQualified| |asyTypeJoinPartPred| |lnGlobalNum|
            NEXT-TAB-LOC |workfilesSpad2Cmd| |atom2String| |pfFix|
            |updateTimedName| |asyTypeUnit| |untraceAllDomainLocalOps|
            NONBLANKLOC |form2StringAsTeX| |pushTimedName|
            |pfExitNoCond| |poFileName?| |b2dnf| INDENT-POS
            |getFirstWord| |form2StringLocal| |pfHide?|
            |asyTypeUnitList| |getBpiNameIfTracedMap| |poPlaceOfOrigin|
            |bassert| BLANKP |pfHidePart| |lnImmediate?|
            |shortenForPrinting| |band| |abbreviate|
            FOAM:|fiGetDebugger| |pfTupleList| |poImmediate?| |bor|
            |splitIntoOptionBlocks| |formWrapId| |pfExpr?| |bnot|
            |pfSequence?| |pfWrongWhy| |asyCatItem|
            |spadReply,printName| |lnExtraBlanks| |pf0SequenceArgs|
            |lispize| |pf0WrongRubble| |asyCattran| |prTraceNames,fn|
            |pfGetLineObject| |pfAddAddin| |asyCattran1| |addTraceItem|
            |pfSourceToken| |notDnf| |stripLisp| |npWConditional|
            |splitIntoBlocksOf200| |pfAddAddon| |stupidIsSpadFunction|
            |nplisp| |pf0AddBase| |asyCattranOp| |compileBoot|
            |pfFileName?| |npboot| |significantStat| |pfPlaceOfOrigin|
            |functionp| |pfAttribute| |macroExpanded| |pfDWhereContext|
            |pfNopos?| |notCoaf| |macrop| |ncConversationPhase,wrapup|
            FOAM:|Halt| |pfDWhereExpr| |isTraceGensym|
            |pfPosImmediate?| |npProcessSynonym| |pfWithWithon|
            |asyPredTran1| |isIntegerString| |ncSetCurrentLine|
            |pf0WithBase| |simpCattran| |dumbTokenize|
            |decomposeTypeIntoTower| |pf0WithWithin| |asyTypeItem|
            |parseSystemCmd| |asyUnTuple| |list1| |tokTran| |pfWIf?|
            |list2| |parseFromString| |reassembleTowerIntoType|
            |pfWIfCond| |asCategoryParts,exportsOf| |list3|
            |npParenthesized| DEF-WHERECLAUSE |pfWIfThen|
            |asyTypeMakePred| |ordList| DEF-ADDLET |pfWIfElse| |prove|
            |pfWDeclare?| |asAll| IS_GENVAR |countCache| DEF-SELECT
            |pfWDeclareSignature| |domainOne| |pfWDeclareDoc|
            |displayDatabase| MONITOR-DATA-COUNT |domainZero|
            DEF-STRINGTOQUOTE |startTimingProcess| |pfAttribute?|
            |asyExportAlist| MONITOR-EXPOSEDP DEF-INSERT_LET
            |pfAttributeExpr| |asyIsCatForm|
            |changeToNamedInterpreterFrame| MONITOR-PARSE
            |pfRetractTo?| MONITOR-LIBNAME |pfRetractToExpr|
            |undoInCore| |pfRetractToType| |undoFromFile|
            |isTaggedUnion| |dispStatement| DEF-IS-REMDUP |pfTLambda?|
            DEF-IS-EQLIST |pfTLambdaRets| |fortexp0| DEF-IN2ON
            |pfTLambdaBody| MONITOR-DATA-P DEF-WHERE |pf0TLambdaArgs|
            HACKFORIS1 |pf0CollectIterators| |fracpart| |getfortexp1|
            |pfIterateFrom| |intnplisp| |DEF-::| |pfReturnFrom|
            |gammaStirling| |optimize| DEF-REPEAT |gammaRatapprox|
            |optimizeFunctionDef,removeTopLevelCatch|
            |pfComDefinitionDoc| |lnrgammaRatapprox| |optXLAMCond|
            MONITOR-DECR |indentFortLevel| HACKFORIS
            |pfComDefinitionDef| |phiRatapprox| |readHiFi| |optIF2COND|
            |pfDefinitionSequenceArgs| |retract| |pfExportDef|
            |gammaRatkernel| |histInputFileName| |optimize,opt|
            |pf0TypingItems| |removeUndoLines| |fortFormatIf|
            |pf0ExportItems| INTERPSYS-ECL-IMAGE-INIT
            MONITOR-CHECKPOINT |fortFormatElseIf| |pf0ImportItems|
            |parseGreaterThan| DATABASE-ABBREVIATION |npZeroOrMore|
            |pfInlineItems| |string2BootTree| DATABASE-ANCESTORS
            MONITOR-DIRNAME |pfQualType?| DATABASE-CONSTRUCTOR
            |fetchOutput| |fortFormatIntrinsics| |pfQualTypeType|
            DATABASE-CONSTRUCTORCATEGORY |undoChanges| MONITOR-NRLIB
            |printMms| |pfQualTypeQual| |pfDefinitionRhs|
            DATABASE-CONSTRUCTORKIND |retractAtree| CHARP |emptyAtree|
            |cgammat| |pfLambdaTran| DATABASE-CONSTRUCTORMODEMAP
            MONITOR-SPADFILE |pfCollectArgTran| DATABASE-COSIG
            |bottomUpCompile| |clngammacase3| |pfLambdaRets|
            DATABASE-DEFAULTDOMAIN |setIOindex| |bottomUpUseSubdomain|
            |compileTimeBindingOf| MONITOR-APROPOS
            |getAttributesFromCATEGORY| |cgammaBernsum| |pfLambdaBody|
            IDENT-CHAR-LIT DATABASE-MODEMAPS DEF-EQUAL
            |resolveTypeList| |pfCollectIterators| EQUABLE
            DATABASE-NILADIC |TruthP| |zeroOneConversion|
            |cgammaAdjust| |pfCollectBody| MKQ DATABASE-OBJECT
            |dewritify| |pfSequence2Sex0| DATABASE-OPERATIONALIST
            RPACKFILE FOAM::PROCESS-IMPORT-ENTRY |isAVariableType|
            RENAME DATABASE-DOCUMENTATION |writify|
            |optPredicateIfTrue| |mkRationalFunction| |pfRuleLhsItems|
            |new2OldLisp| DATABASE-CONSTRUCTORFORM |optCONDtail|
            |dqUnit| |pfLhsRule2Sex| DEF-RENAME DATABASE-ATTRIBUTES
            |replaceGoGetSlot| |optSEQ| |dqConcat| |pfDo| |pfRuleRhs|
            DATABASE-PREDICATES |spadClosure?| |pfLoop|
            |asIsCategoryForm| |pfRhsRule2Sex| DATABASE-SOURCEFILE
            |writify,writifyInner| RECOMPILE-LIB-FILE-IF-NECESSARY
            |optSEQ,getRidOfTemps| |dqUnitCopy| |trimComments|
            |ruleLhsTran| DATABASE-PARENTS |unwritable?|
            |optSEQ,SEQToCOND| |dqToList| |pfTupleParts| |PsiIntpart|
            |rulePredicateTran| DATABASE-USERS |optSEQ,tryToRemoveSEQ|
            |pfAdd?| |asyExtractDescription| |patternVarsOf|
            |finalizeLisplib| LIBSTREAM-DIRNAME |optimizeFunctionDef|
            |pfComDefinition?| DATABASE-DEPENDENTS |lisplibDoRename|
            |dewritify,dewritifyInner| |optCatch| |pfDeclPart?|
            |asyAncestors| |negintp| |dewritify,is?| |pfExport?|
            |asyArgs| DATABASE-SPARE PNAME |optCons| |pfInline?|
            |hackToRemoveAnd| |loadSpad2Cmd| |optMkRecord| |varsInPoly|
            |pfWith?| |sayBrightlyLength| |fnameExists?| |optCond|
            |resolveTypeListAny| |pfTyping?| |asyPredTran|
            |traceSpad2Cmd| SEQOPT |nopilesSpad2Cmd| MAKE-BVEC |pfId?|
            |fnameDirectory| |pfImport?| |asyAncestorList|
            |getMapSubNames| MKQSADD1 |setNopiles| |failCheck|
            |StringToDir| |optRECORDELT| |typeIsASmallInteger|
            |pfDWhere?| |mkNiladics| |DirToString| |optSETRECORDELT|
            |pfWrongRubble| OUR-WRITE-DATE |trace1| |objCodeMode|
            |myWritable?| |optRECORDCOPY| |pfAddBase|
            |asyLooksLikeCatForm?| MAKE-DIRECTORY |objCodeVal|
            |npListing| |optSuchthat| |pfWithBase| |asyMkpred|
            |getTraceOption| |readSpad2Cmd| |fnameName| |optMINUS|
            |pfWithWithin| |pathnameTypeId| |fnameType| |optQSMINUS|
            |pfLambdaArgs| |asyArg| |untrace| |wrapped2Quote| |opt-|
            |fnameReadable?| |pfId| |fnameWritable?| |optLESSP|
            |pfTLambdaArgs| |npConditional| |optEQ| |pfMLambdaArgs|
            |pfTweakIf| THETA_ERROR |containsVariables|
            |pfWhereContext| |pilePlusComment| |pfLoopIterators|
            |validateOutputDirectory| |pilePlusComments| |pfForinLhs|
            |asytranLiteral| |isTupleForm| |userError|
            |reportOpsFromUnitDirectly| |transTraceItem|
            |pfDefinitionLhsItems| |asytranEnumItem| |pileCforest|
            LISTOFATOMS |npEqPeek| |SpadInterpretFile|
            |getTraceOptions| FOAM::FOAMPROGINFOSTRUCT-P
            |pfAssignLhsItems| |pileColumn| |wrapMapBodyWithCatch|
            FOAM:|printNewLine| |setCurrentLine| |saveMapSig|
            |pfTypingItems| |serverReadLine| |statement2Fortran|
            |pfExportItems| NUMOFNODES |npEncAp| |say2PerLine|
            |fortran2Lines| |pfLocalItems| |asyCosig|
            |stackTraceOptionError| |NRTgenInitialAttributeAlist|
            |checkLines| |pfFreeItems| |asyConstructorArgs|
            |altSeteltable| |domainToGenvar|
            |displayOperationsFromLisplib| |ncloopEscaped|
            |bubbleConstructor| |exp2FortOptimize| |pfImportItems|
            |asyConstructorArg| |transOnlyOption| SUBANQ |fortPre|
            |asyCosigType| |isHomogeneousArgs| |enPile|
            |getTraceOption,hn| |segment| |pfCollect1?| |separatePiles|
            |isListOfIdentifiersOrStrings| |printSynonyms| |exp2Fort1|
            |pfCollectVariable1| |createAbbreviation| |upREPEAT1|
            |lastTokPosn| |isListOfIdentifiers| |processSynonymLine|
            |fortranCleanUp| |pfTaggedToTyped| |upREPEAT0|
            |firstTokPosn| |genDomainTraceName|
            |processSynonymLine,removeKeyFromLine| |mkLineList|
            |pfFlattenApp| |asyShorten| |interpOnlyREPEAT|
            |pileComment| |npAdd| |ncloopPrintLines| |pfTaggedToTyped1|
            |removeTracedMapSigs| |compileAsharpLispCmd| |npWith|
            |intloopEchoParse| |pfTransformArg| |coerceSpadArgs2E|
            |compileSpadLispCmd| |insertpile| |npTrapForm|
            |asyExtractAbbreviation| |compileAsharpArchiveCmd|
            |lineoftoks| |pfCheckId| |coerceSpadFunValue2E|
            |typeToOutputForm| |exp2FortOptimizeCS| |pfCheckArg|
            |getPreviousMapSubNames| |polyVarlist| |ncloopIncFileName|
            |typeToInputForm| |exp2FortOptimizeCS1| |typeOfType|
            |setAsharpArgs| |removeQuote| |ncloopParse| |pfSexpr,strip|
            |asyTypeJoin| |setInputLibrary| |updateSourceFiles|
            WHOCALLED |incString| |pf0FlattenSyntacticTuple|
            |asyCATEGORY| |setOutputLibrary| |pathnameName|
            |pfPrintSrcLines| SPADSYSNAMEP
            |exp2FortOptimizeCS1,popCsStacks| |pfListOf?| |spadTrace,g|
            |poNoPosition?| |getTarget| |pfSemiColon| |asyType|
            |copyHack| |pathnameDirectory| |pfSemiColon?|
            |fortran2Lines1| |pfCollect2Sex| |asyTypeMapping|
            |copyHack,fn| |flattenOperationAlist| |getUnname|
            MONITOR-EVALBEFORE |displayLines| |pfSemiColonBody|
            |pfForin?| |upDEF| |setFortTmpDir| |containsPolynomial|
            |npAnyNo| |asTupleAsList| |expression2Fortran|
            |pfSymbolVariable?| VMLISP::LIBSTREAM-INDEXSTREAM
            |pf0ForinLhs| |upDollar| |setFortDir| |patternCheck|
            |displayOperations| |phMacro| |dispfortexp1| |pfForinWhole|
            |pfDocument?| |upequation| |displayMacros| |getValue|
            |npFromdom1| |orderBySlotNumber| |pfWhile?|
            |pfDocumentText| |uperror| |phInterpret| |mkAtree|
            |displayLines1| |pfStringConstString| |pfWhileCond|
            |upfree| |setHistory| GETZEROVEC |intInterpretPform|
            |LZeros| |pfSuchthat?| |pfExpression?| |uplocal|
            |setOutputCharacters| |displayParserMacro| |pf2Sex| |mkMat|
            |pfSuchthatCond| |pfNot| |uphas| |displayMacro|
            |zeroOneTran| |pfDo?| |upIF| |npDotted| UNVEC |pfDoBody|
            |upis| |bottomUp| |pfTyped?| |upisnt|
            |sayAsManyPerLineAsPossible| |nonBlank| |pfTypedType|
            |upiterate| |changeExprLength| |pfAssign?| |upbreak|
            |qTWidth| |checkType| |pf0AssignLhsItems| |upLET| |objVal|
            |subSub| |pfAssignRhs| |lnrgamma| |getMode|
            |fortFormatTypes,unravel| |pfDefinition?| S-TO-C |letWidth|
            |fortFormatCharacterTypes| |pfDefinition2Sex| |lncgamma|
            |asTupleNew0| |sumWidth| |mkParameterList| CSCH
            |pfLambda2Sex| |upQUOTE| C-TO-S VMLISP::FLAT-BV-LIST
            |pfRestrict?| |uppretend| |rgamma|
            |displayProperties,sayFunctionDeps| |superSubSub| |nameLen|
            |pfRestrictExpr| |cgamma| |isWrapped| |addCommas|
            |pfRestrictType| |isHomogeneous| |unwrap| |binomSub|
            |pfFree?| |upREPEAT| |editSpad2Cmd| |getBasicMode|
            |binomSuper| MANEXP |pf0FreeItems| |upreturn|
            |newHelpSpad2Cmd| |getBasicObject| |binomWidth| ACOT
            |pfLocal?| |upSEQ| C-TO-R
            |fortFormatCharacterTypes,mkParameterList2| COT
            |pf0LocalItems| |upTuple| |isType| |altSuperSubSub| SEC
            |pfWrong?| |uptypeOf| |altSuperSubSuper| |isFloat| CSC
            |pfAnd?| |upwhere| RLNGAMMA |altSuperSubWidth|
            |PARSE-NudPart| ACSC |pfAndLeft| CLNGAMMA STACK-UPDATED
            |superSubWidth| |fortPreRoot| ASEC |pfAndRight| RGAMMA
            |isMapExpr| |superSubSuper| COTH |pfOr?| CGAMMA
            TRACEOPTIONS |suScWidth| |PARSE-GliphTok| |fortExpSize|
            SECH |pfOrLeft| |emptyInterpreterFrame| |fortSize| ACSCH
            |pfOrRight| |fortSize,elen| ACOTH |pfNot?| |npEqKey|
            |makeHistFileName| |vConcatSub| ASECH |pfNotArg|
            |pfNovalue?| |npItem1| |findFrameInRing| |Zeros|
            |binomialSub| |pfNovalueExpr| |pfEnSequence|
            |binomialSuper| |pfRule?| |npPush| |frameEnvironment|
            |bottomUpPercent| |binomialWidth| |pfRule2Sex| |pfNovalue|
            /UNEMBED-Q LOG10 |pfBreak?| |pfBreakFrom| |tokPart|
            |pfReturn?| STACK-STORE |pfReturnExpr| |mkAtreeNode|
            |pfIterate?| |PARSE-NBGliphTok| |pf0WhereContext| |pfTuple|
            |getModeOrFirstModeSetIfThere| DROPENV |pfWhereExpr|
            |getUnionOrRecordTags| SHOWBIND |pfLiteralClass|
            |bottomUpElt| MONITOR-FILE |pfLiteralString| |float2Sex|
            |pfLeafToken| |asytran| |pmDontQuote?|
            |handleLispBreakLoop| MAKE-SYMBOL-OF
            |asyConstructorModemap| |asyDocumentation| |pfOp2Sex|
            |npParened| |asyParents| |pfApplicationArg| |npBracked|
            |asMakeAlist| |pfSuchThat2Sex| |npBraced| |asySubstMapping|
            |hasOptArgs?| |npAngleBared| |getToken|
            |pf0DefinitionLhsItems| |asyComma?| SHOWDATABASE
            |npMissing| STACK-POP |pfLeaf?| |pfSexpr|
            |asMakeAlistForFunction| |pfPile| |pushSatOutput|
            |pf0LambdaArgs| TOKEN-TYPE |pfTypedId| STACK-TOP
            |pfCheckInfop| |clearCache| TOKEN-FIRST-CHAR TOKEN-NONBLANK
            |concatList| |npSemiListing| |mkMessage| |ppos|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) *) ASHARP |ioHook| |sayBrightlyNT|
            FOAM:COMPILE-AS-FILE COMPILE-LIB-FILE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (VALUES T T)) FOAM:AXIOMXL-GLOBAL-NAME
            |spadTraceAlias|)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) STRING) MAKE-FULL-CVEC)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) T) BOOTTRAN::BOOTTOCL |pfSymb|
            |getConstructorExports| |centerNoHighlight| PRETTYPRINT
            MAKE-HASHTABLE MAKE-FILENAME MACERR
            GET-BOOT-IDENTIFIER-TOKEN MATCH-CURRENT-TOKEN |simpHasPred|
            |centerAndHighlight| CATCHALL TAB |F,PRINT-ONE|
            |printRecordFile| |htFile2RecordFile| VMREAD
            |LAM,EVALANDFILEACTQ| PRETTYPRIN0 MONITOR-ADD PRINT-FULL
            |desiredMsg| |fillerSpaces| |sayBrightlyI| BLANKS
            |sayBrightly| |htFile2InputFile| |inputFile2RecordFile|
            MAKE-INPUT-FILENAME VMLISP::MAKE-FULL-NAMESTRING
            |interpret| |defaultTargetFE| RDEFIOSTREAM MATCH-NEXT-TOKEN
            |pfSymbol| |pfExpression|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) *) /TRACE-2 |set1|
            |isDomainConstructorForm| |displaySetOptionInformation|
            |spadTrace| |evalAndRwriteLispForm| COERCE-FAILURE-MSG
            PRINMATHOR0 MONITOR-PRINT MONITOR-PRINARGS-1 |sayKeyedMsg|
            |replaceNamedHTPage| |popUpNamedHTPage| |inclHandleBug|
            |ppPair| ADDCLOSE |rwriteLispForm| |output| |spleI1|
            QUOTIENT |printMap1| |mkAtreeWithSrcPos| |ncEltQ|
            VMLISP::COPY-FILE VMLISP::COPY-LIB-DIRECTORY |spadcall2|
            |printTypeAndTime| /TRACELET-PRINT |printTypeAndTimeNormal|
            |msgText| COMP_QUIETLY_USING_DRIVER |isDomainForm|
            |makeLongTimeString| |LAM,FILEACTQ| |makeLongSpaceString|
            |xdrWrite| |xdrRead| COMPILE-DEFUN |readData,xdrRead1|
            SUFFIX |evalSlotDomain| |getMinimalVarMode| |nrtEval|
            |formArguments2String,fn| $FCOPY SPAD-SAVE SAYBRIGHTLYNT1
            |htCommandToInputLine,fn| |handleNoParseCommands|
            |ncloopInclude1| |handleTokensizeSystemCommands|
            |handleParsedSystemCommands| |npsystem|
            |tokenSystemCommand| |print| |writeInputLines|
            |ScanOrPairVec| |reportOperations| |reportOpsFromLisplib0|
            |intloopReadConsole| |reportOpsFromLisplib1|
            |reportOpsFromLisplib| |ncloopCommand| |makeStream|
            |ncConversationPhase| |patternCheck,subWild|
            |displayProperties| MONITOR-PRINVALUE |fortError|
            |intSayKeyedMsg| DEFSTREAM |ncloopInclude| |sayErrorly|
            |saturnSayErrorly|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (*)) MACRO-INVALIDARGS MACRO-MISSINGARGS)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) T) |coerceOrConvertOrRetract| /TRACE-1
            |coerceOrRetract| NREVERSE-N |incStream| CONS-N
            |traceOptionError| |incTrunc| |upLispCall| APPEND-N
            |npRightAssoc| |traceDomainConstructor| |coerceRetract|
            |pfApplication| /GETTRACEOPTIONS |constructT| |tracelet|
            |genIFvalCode| |hashCombine| |displaySetVariableSettings|
            |readLib| |upIFgenValue| |macLambdaParameterHandling|
            |rassocSub| |mac0SubstituteOuter| |spadPrint|
            |putPvarModes| |SymMemQ| |pfIfThenOnly| |compileIs|
            /UNTRACE-1 |isPatMatch| |spadUntrace| |positionInVec|
            |hasCat| MONITOR-EVALTRAN |mkIterVarSub| MONITOR-EVALTRAN1
            MONITOR-GETVALUE |modemapsHavingTarget| |replaceSymbols|
            |quickAnd| |setBootAutoLoadProperty| |mkBootAutoLoad|
            GLESSEQP /EMBED-1 |declare| /EMBED-Q |uplocalWithType|
            |upfreeWithType| |declareMap| |deleteAll| |tempExtendsCat|
            |mkCategoryOr| |simpCategoryOr| |formalSubstitute|
            DAASENAME |has| |quickOr| |catPairUnion,addConflict|
            ASHARPMKAUTOLOADFUNCTION ADDOPERATIONS
            VMLISP::PUTINDEXTABLE |simpOrUnion1| |mergeOr| |testExtend|
            VMLISP::WRITE-INDEXTABLE |categoryParts,build|
            |inclHandleError| |inclHandleWarning|
            |updateCategoryTableForDomain| |inclHandleSay|
            |getSlotFromCategoryForm| |updateCategoryTable|
            |inclmsgFileCycle| DELDATABASE GETCONSTRUCTOR WRAPDOMARGS
            |postFlatten| |say2PerLineWidth| |getValueFromEnvironment|
            |postTranSegment| |splitListOn| |canFit2ndEntry|
            |throwKeyedMsg| |center| |inclFname| |installConstructor|
            |getCDTEntry| |incActive?| |parseTranCheckForRecord|
            |condAbbrev| STOREBLANKS |unabbrev1| INITIAL-SUBSTRING
            |throwPatternMsg| ADD-PARENS-AND-SEMIS-TO-LINE |assertCond|
            |ifCond| |unabbrevUnionComponent| |checkForFreeVariables|
            |unabbrevRecordComponent| ESCAPED PARSEPILES
            |BooleanEquality| |assoc| ERROR-FORMAT MAKEOP MAKENEWOP
            |parseTypeEvaluateArgs| EFFACE |rePackageTran|
            |parseCases,casefn| EMBED |getLisplib| |getLisplibNoCache|
            $FINDFILE |NRTdescendCodeTran| |agg| /UNTRACE-2 LEXGREATERP
            |scanExponent| |posend| |position| RPLPAIR |defLET|
            |termRW1| |matWList| TRANSLABEL1 HPUT* |scanCheckRadix|
            |subCopy0| |lfrinteger| |subCopyOrNil| REMOVE-ESCAPES
            |macSubstituteId| |deepSubCopy0| |deepSubCopyOrNil|
            |termRW| |matWList1| |scanInsert| |saveDependentMapInfo|
            |matSubList1| STRING2ID-N |matSuperList1| GET-GLIPH-TOKEN
            |nonRecursivePart| ASSOCIATER |expandRecursiveBody|
            |nonRecursivePart1| |everyNth| |containsOp| |notCalled|
            |mathPrint1| |allLASSOCs| |lnSetGlobalNum| |mkValueCheck|
            |HasCategory| |wordFrom| |incRenumberItem| |mkValCheck|
            |HasSignature| CHAR-EQ |writeLib| RDROPITEMS
            |incRenumberLine| |findLocalVars1| |union| |incDrop|
            |mkFreeVar| |throwEvalTypeMsg| |sySpecificErrorHere|
            |insertWOC| |incCommandTail| |isDomainSubst,findSub|
            |postFlattenLeft| |isDomainSubst,fn| |sayPatternMsg|
            |syIgnoredFromTo| |autoLoad| TRANSLABEL |addDefMap|
            |sayKeyedMsgLocal| |modemapPattern| |makeRuleForm|
            |saturnThrowKeyedMsg| PUSH-REDUCTION |displayRule|
            |throwKeyedMsg1| |dcOpLatchPrint| |outputFormat|
            |sayKeyedMsgAsTeX| |saturnKeyedSystemError| |getArgValue1|
            |simplifyMapPattern| |keyedSystemError1|
            |getSystemModemaps| |clearCategoryTable1| |breakKeyedMsg|
            |dcOpPrint| GETDATABASE |mkAlistOfExplicitCategoryOps,fn|
            |getBindingPowerOf| |getArgValueOrThrow| |addDomainToTable|
            |pairList| |opWidth| |encodeCategoryAlist|
            |loadLibIfNecessary| REMOVER
            |convertOpAlist2compilerInfo,formatSig| SETDIFFERENCE
            GETALIST |isKeyQualityP| |putMode| |getMsgCatAttr|
            |setMsgCatlessAttr| |fastSearchCurrentEnv|
            |getAllModemapsFromDatabase| |rep|
            |getModemapsFromDatabase| |mkLocalVar| |getI|
            |findLocalVars| |insertModemap| |makeNewDependencies|
            |putDependencies| |prnd| |clearDependencies| |FromTo|
            |mkFormalArg| |RecordPrint| |mkMapAlias| |coerceRe2E|
            |depthOfRecursion| |objNewWrap| |makePattern|
            |coerceByFunction| |sayDroppingFunctions|
            |mkAtree1WithSrcPos| |mkAtreeNodeWithSrcPos| |coerceVal2E|
            |scylla| |putValueValue| |scanIgnoreLine| |UnionPrint|
            |mkAliasList,fn| |addPatternPred| |augmentPredVector|
            |canMakeTuple| |putFlag| |coerceUn2E| |MappingPrint|
            |rempropI| |putDependencies,removeObsoleteDependencies|
            |makeByteWordVec2| |EnumPrint| |objNewCode| |createEnum|
            |objSetVal| |asTupleNewCode| |getEqualSublis,fn|
            |mapDefsWithCorrectArgCount| |subMatch| FOAM:|fputs|
            FOAM:|fputc| $REPLACE |putBodyInEnv| UNIONQ |makeGoGetSlot|
            |interpMap| |sort| |testPrin| |resolveTTAny|
            |makeCompactDirect1| QUOTIENT2 |resolveTMOrCroak|
            |hyperize| INTEXQUO DIVIDE DIVIDE2 |getMapBody|
            |getLocalVars| GETL |makeSF| |makeLocalModemap|
            |compileBody| |setMsgUnforcedAttrList| |makePrefixForm|
            |setMsgText| |predicateBitIndex,pn| LEXLESSEQP
            |intersection| |putFTText| |mungeAddGensyms|
            |augmentPredCode| BVEC-MAKE-FULL DEFINE-FUNCTION
            |setMsgPrefix| |insertPos| |buildBitTable,fn| BVEC-CONCAT
            BVEC-EQUAL BVEC-GREATER |Delay| BVEC-AND |mkSuperSub|
            |queueUpErrors| |resolveTT1| BVEC-OR |searchCurrentEnv|
            |checkArgs| BVEC-XOR |outputMapTran0| BVEC-NAND
            |getProplist| |erMsgCompare| BVEC-NOR |compareposns|
            |thisPosIsLess| |incAppend| |redundant| |thisPosIsEqual|
            |next| |lazyOldAxiomDomainHashCode| |pfCopyWithPos|
            |lazyOldAxiomDomainDevaluate| |sameMsg?| |replaceArgDef1|
            |oldAxiomPreCategoryHashCode| |replaceArgDef|
            |oldAxiomPreCategoryDevaluate| |replaceArgDefs1|
            |basicMatch?| |oldAxiomCategoryDevaluate| |replaceArgDefs|
            REMALIST |oldAxiomCategoryParentCount| |stringMatches?|
            |deleteAssocWOC| |stringPrefix?| |oldAxiomCategoryHashCode|
            |stringChar2Integer| |computeTargetMode|
            |oldAxiomDomainHashCode| |oldAxiomDomainDevaluate|
            |NRTreplaceLocalTypes| |processInteractive1| |sublisNQ|
            |interpretTopLevel| |recordAndPrint| |decideHowMuch|
            |listDecideHowMuch| NCONC2 |processInteractive| |insert|
            |systemDependentMkAutoload| |substDomainArgs|
            SET-LIB-FILE-GETTER |printTypeAndTimeSaturn| |hashType|
            |sameUnionBranch| |isNestedInstantiation| |plural|
            |substituteSegmentedMsg| POSN1 |resolveTTUnion|
            |makeVector| |acceptableTypesToResolve| |compareTT|
            |simpBoolGiven| |resolveTTSpecial| |resolveTTRed| REMAINDER
            |resolveTTCC| |resolveTTEq| |initializeTimedNames|
            |acceptableTypesToResolve1| |inFirstNotSecond|
            |constructTowerT| FOAM::ALLOC-PROG-INFO STACK-PUSH
            |makeList| |term1RW| |term1RWall| |asTupleNew| |xdrOpen|
            |assocCircular| |predCircular| EVAL-DEFUN |pfMapParts|
            |resolveTCat1| |macWhere,mac|
            |getConditionsForCategoryOnType| |transferSrcPosInfo|
            |recurrenceError| |PARSE-Operation| |writeData|
            |leftBindingPowerOf| |rightBindingPowerOf|
            |PARSE-rightBindingPowerOf| |resolveTMUnion| |readData|
            |resolveTMRecord| |PARSE-leftBindingPowerOf| |resolveTMEq|
            |resolveTMRed| |resolveTM2| |applyWithOutputToString|
            |resolveTMTaggedUnion| |functionAndJacobian,DF| GGREATERP
            |spliceTypeListForEmptyMode| |clearAllSlams,fn| CGREATERP
            |resolveTMSpecial| |mkLessOrEqual| |resolveTMEq1|
            |testBitVector| |defLetForm| SORTBY |defLET2| |mkAtree3,fn|
            |collectDefTypesAndPreds,addPred| |sayLooking1| |defLET1|
            |addCARorCDR| |defISReverse| |defIS1| |member| |defIS|
            |deleteLassoc| |canCoerceFrom;| |app2StringWrap|
            |deleteAssoc| |canCoerceFrom0| |match?| |formJoin1|
            |deleteAssocWOC,fn| |canCoerce;| |insertWOC,fn|
            |canCoerce1| |concat1| |hasCaty1| |formArguments2String|
            |formDecl2String| |ofCategory| |hasCatExpression|
            |breaklet| |replaceSharps| |evalCategory| |sublisNQ,fn|
            |newHasTest,fn| |newHasCategory| |hasCorrectTarget|
            |isEqualOrSubDomain| |pfWhere| |mkObj| |resolveTT;|
            |coerceInt| |coerceInteractive| |search| |keyedSystemError|
            |formatJoinKey| |linearFormatForm|
            |reportCircularCacheStats| |delete| |pfLam|
            |mkCircularCountAlist| |pfReturnTyped| AND2
            FOAM::|magicEq1| OR2 |pfDefinition|
            |remHashEntriesWith0Count,fn| |getFortranType|
            |displayOpModemaps| |pfMacro| |pfPushMacroBody|
            FOAM-USER::H-ERROR TRUNCLIST |pfRule| SUBLISNQ
            |listTruncate| |formatOperation| |wl| CONTAINED S+
            |setMsgForcedAttrList| PREDECESSOR |rightJustifyString|
            |wt| |isPointer?| LASSOC |getOpArgTypes,f| |rassoc|
            |isString?| QLASSQ |argCouldBelongToSubdomain| PAIR
            |CONTAINEDisDomain| LENGTHENVEC |addToSlam|
            FOAM-USER::H-STRING |lassocShiftQ| |app2StringConcat0|
            |globalHashtableStats| |SExprToDName| |domainEqualList|
            |hashTypeForm| |domainEqual| |subCopy|
            |oldAxiomCategoryDefaultPackage|
            |oldAxiomPreCategoryParents| FOAM-USER::H-INTEGER |cbeta|
            |pfReturn| |cpsireflect| |matchMmSigTar| |f01| |chebeval|
            |pfLp| |containedRight| |isTowerWithSubdomain|
            |chebstareval| |findSubstitutionOrder?,fn|
            |countCircularAlist| |pfForin| |substInOrder| |pfQualType|
            |deepSubCopy| PRINT-AND-EVAL-DEFUN |suffix?| ?ORDER
            |undoSteps| |lassocShift| |diffAlist|
            |sayModemapWithNumber| |commandError| SAYBRIGHTLY1
            |queryUserKeyedMsg| |commandUserLevelError|
            |undoSingleStep| |resolveTM1| |testInput2Output|
            |commandErrorIfAmbiguous| |makePathname| |pfSpread|
            |formatOpSignature| |formatOpSymbol| |asyTypeJoinPart|
            |optionError| |mmCatComp| |filterListOfStrings|
            |dnfContains| PRINT-DEFUN |dnfContains,fn| |mergeSubs|
            |satisfiesRegularExpressions|
            |untraceDomainConstructor,keepTraced?|
            |defaultTypeForCategory| |andReduce|
            |getAliasIfTracedMapParameter| |asyTypeUnitDeclare|
            |ncloopPrefix?| |asyCatSignature| |hasPair|
            |printNamedStatsByProperty| FOAM:|fiSetDebugger| |andDnf|
            |streamChop| |orDnf| SETANDFILE |asyCattranConstructors|
            |coafOrDnf| |coafAndDnf| |coerceIntPermute| |pfDWhere|
            |ordUnion| |coerceIntSpecial| |asySimpPred| |funfind|
            |npsynonym| |coerceIntTableOrFunction| |asyCattranSig|
            |ordIntersection| |ncINTERPFILE| |orDel|
            |coerceCommuteTest| |ordSetDiff| |pfWDec| |intloopInclude1|
            |coafAndCoaf| |phReportMsgs| DEF-SELECT1 DEF-SELECT2
            |asCategoryParts,build| FLAG |mkObjWrap| FOAM:|PtrMagicEQ|
            DEF-LET |constantInDomain?| |asyMkSignature| CPSI RBESSELJ
            |hasOption| CBESSELJ |optionUserLevelError| |equalOne|
            RBESSELI |equalZero| |segment2| WHDEF CBESSELI
            |isSubTowerOf| |assignment2Fortran1| LET_ERROR CHYPER0F1
            |canConvertByFunction| |integerAssignment2Fortran1| DEF-IS2
            |getStatement| SETDIFFERENCEQ |dispfortexpf| |dispfortexpj|
            |optimizeFunctionDef,replaceThrowByReturn| |FloatError|
            |optimizeFunctionDef,fn| |dispfortarrayexp|
            |pfComDefinition| DEF-IT DEF-IS-REV |horner|
            |fortFormatIfGoto| |pfCollect| |optCatch,changeThrowToExit|
            MONITOR-WRITE |optCatch,hasNoThrows|
            |sayIntelligentMessageAboutOpAvailability|
            |optCatch,changeThrowToGo| |fortFormatTypes| |cgammaG|
            MARKHASH |optCallSpecially,lookup| |getOpArgTypes|
            |asyAbbreviation| |showInOut| INTERSECTIONQ
            |setBootAutloadProperties| |showInput|
            |bottomUpCompilePredicate| |mkAutoLoad| SPADRREAD
            |EqualBarGensym| |resolveTCat| |PsiEps| CARCDREXPAND
            |EqualBarGensym,fn| |asyDisplay| |rPsiW| |getOpArgTypes1|
            |asySignature| |displayDatabase,fn| REPEAT-TRAN |dqAppend|
            |PsiAsymptotic| -REPEAT |pfIdPos| MKPF
            |asyDocumentation,fn| FLAGP |dqAddAppend|
            |asyExportAlist,fn| |PsiXotic| |pvarPredTran| MKPF1
            |patternVarsOf1| MKPFFLATTEN |unloadOneConstructor|
            |brutef01| S- |ScanOrPairVec,ScanOrInner| |BesselJAsympt|
            S* |BesselJAsymptOrder| |pfAssign| |AssocBarGensym|
            |augmentTraceNames| |BesselJRecur| |pfTyped|
            |retractByFunction| |besselIback| DO_LET NLIST
            |untraceDomainLocalOps| |intCodeGenCOERCE| -REDUCE-OP /READ
            |pfExit| |BesselIAsymptOrder| DELASC
            |absolutelyCanCoerceByCheating| |getConstantFromDomain|
            |BesselKAsymptOrder| SEGMENT |isSubDomain|
            FOAM:|printDFloat| FOAM:|printSFloat| |canCoerceUnion|
            |canCoerceByFunction| FOAM:|printBInt|
            |absolutelyCannotCoerce| |canCoerceByMap| FOAM:|printSInt|
            |canCoerceExplicit2Mapping| |pileTree| FOAM:|printString|
            |mkObjCode| FOAM:|printChar| |canCoerceLocal| |evalLET|
            TAKE |canCoerceTower| |evalLETput| DROP
            |newCanCoerceCommute| |getMapSig| |asyWrap| |seteltable|
            |sayCacheCount| |pileForest1| |intloopPrefix?|
            |canCoercePermute| |eqpileTree| TRUNCLIST-1 |coerceIntTest|
            |pileForest| |pileCtree| |intloopProcessString|
            |replaceLast| |pfSuch| SUBB |pfAbSynOp?| |constructM|
            |CONTAINED,EQ| |canCoerceCommute| |CONTAINED,EQUAL|
            |ncloopDQlines| |coerceIntCommute| |asySplit|
            |computeTTTranspositions| |lassocSub| DELLASOS
            |updateSymbolTable| |asyAbbreviation,chk| |typeToForm|
            |asyGetAbbrevFromComments,fn| |subTypes| |asySig|
            |mergePathnames| |getBasicMode0| |asySigTarget| |pfMLambda|
            |reportSpadTrace| |clearDependentMaps| |beenHere|
            |coerceInt0| |exp2FortOptimizeCS1,pushCsStacks| |getOption|
            |pushDownOp?| |objSetMode| |asyMapping| |coerceInt1|
            |removeOption| |putTarget| |phParse|
            |spadTrace,isTraceable| |transferPropsToNode|
            |coerceIntFromUnion| |logicalMatch?| |phIntReportMsgs|
            |coerceInt2Union| |expression2Fortran1| |superMatch?|
            |getMinimalVariableTower| |pfFromDom| |genMpFromDmpTerm|
            |resolveTM| |compareTypeLists| |bottomUpIdentifier|
            |addDmpLikeTermsAsTarget| |coerceIntTower| |pfTLam|
            |putModeSet| |processMsgList| |sortAndReorderDmpExponents|
            |coerceIntAlgebraicConstant| |translateMpVars2PVars|
            |fortranifyFunctionName| |getAtree| |removeVectorElt|
            |fortranifyIntrinsicFunctionName| |pfRestrict|
            |removeListElt| |patternCheck,pos| |npTypedForm|
            |getAndSay| |pfCoerceto| |evalSharpOne| |patternCheck,wild|
            |pfRetractTo| |coerceBranch2Union|
            |computeTypeWithVariablesTarget| |pfPretend|
            |valueArgsEqual?| |objNew| |coerceIntByMap|
            |fortFormatTypes1| |putValue| |rPsi| |cPsi| |pfTagged|
            |BesselJ| |npTypedForm1| |bottomUpPredicate| |BesselI|
            |chebf01| |bottomUpType| |position1| |npLeftAssoc| RPSI
            |pfWrong| |varIsOnlyVarInPoly| |keyedMsgCompFailure|
            |segment1| /GETOPTION |AlistAssocQ| |ListMemberQ?|
            |ListMember?| |newHasTest| |AlistRemoveQ| PAIRTRACE
            BPIUNTRACE |intloopInclude| |npMissingMate| |getArgValue|
            |pfParen| |isPatternMatch| |pfBracket| |errorSupervisor|
            CHAR-NE |pfBracketBar| |pfBrace| |pfBraceBar| |symEqual|
            |pfHide| |pfTree| |macLambda,mac| |prefix?| |sayErrorly1|
            |maskMatch?| MAKE-DATABASES |evalLETchangeValue|
            |dollarTran| |pfWDeclare| |pfAnd| |coerceIntByMapInner|
            |pfOr| |after| |pfFromdom|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL FIXNUM) CURRENT-CHAR-INDEX HEAPELAPSED)) 
(PROCLAIM '(FTYPE (FUNCTION NIL (VALUES T T)) GENVAR))
) 
