#+:GCL
(progn
  (eval-when (:execute :compile-toplevel :load-toplevel)
                 (proclaim '(optimize (safety 1) (debug 3))))

(IN-PACKAGE "BOOT") 
(PROCLAIM
    '(FTYPE (FUNCTION NIL (*)) FIRST-ERROR |sendHTErrorSignal|
            |queryClients| |sendNagmanErrorSignal|)) 
(PROCLAIM '(FTYPE (FUNCTION (*) (VALUES T T)) READLINE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FIXNUM) LINE-CURRENT-INDEX LINE-LAST-INDEX
            LINE-NUMBER |hitListOfTarget| |eq0| |widthSC|
            FOAM:|ProgHashCode| FOAM:|strLength| |nothingSuper|
            |nothingSub| |nothingWidth| CHAR2NUM)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FUNCTION) FOAM::FOAMPROGINFOSTRUCT-FUNCALL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) FOAM:|SInt|)
            FOAM::FOAMPROGINFOSTRUCT-HASHVAL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) (VALUES T T)) MONITOR-INFO
            |makeInternalMapMinivectorName| |mkAuxiliaryName|
            |mkCacheName| |getKeyedMsg| |mapCatchName| FILE-GETTER-NAME
            |makeCharacter| |queryUser| FOAM:AXIOMXL-FILE-INIT-NAME
            |mkSharpVar|)) 
(PROCLAIM
    '(FTYPE (FUNCTION ((VECTOR T) (VECTOR T)) T) VMLISP::VGREATERP
            VMLISP::LEXVGREATERP)) 
(PROCLAIM '(FTYPE (FUNCTION ((VECTOR T)) T) TRIMLZ)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) (*)) |killHTPage| |linkToHTPage|
            |startHTPage| |issueHT| |startHTPopUpPage|
            |startReplaceHTPage|)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) (VALUES T T)) |read-line|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) FIXNUM) QSREMAINDER QENUM QSQUOTIENT
            |attributeCategoryParentCount| FOAM:|SetProgHashCode|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T) (VALUES T T)) |fetchKeyedMsg|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) (*)) |replaceNamedHTPage|
            |popUpNamedHTPage|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) *) |Qf2F| |asytranApplySpecial|
            |ncloopInclude0| BUILD-DEPSYS |getVal| GETOP
            |parseIf,ifTran| |getSlotFromDomain|
            |augModemapsFromDomain1| |selectOptionLC| |selectOption|
            |htMkPath| |extendsCategory| |NRTextendsCategory1|
            |asytranFormSpecial| |asytranForm| |exp2FortSpecial|
            |templateVal|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) *) LOCALNRLIB |apprpar1| |appagg1|
            |applpar1| |appvertline| |appargs1| |matrixBorder| |apphor|
            |apprpar| |applpar| |compileAndLink|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) *) |newExpandLocalTypeArgs|
            |appHorizLine| |charyTrouble1| |appparu|
            |constructorAbbreviationErrorCheck| |appargs| |inApp|
            |quoteApp| |makeStatString| |appparu1| |appsc| |appsetq|
            |slashApp| |appsub| |binomialApp| APP |exptApp| |appfrac|
            |argsapp| |appagg| |patternCheck,mknew| |appmat|
            |overlabelApp| |overbarApp| FOAM:|fputss| FOAM:|fgetss|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) *) |makeLongStatStringByProperty|
            |addModemap| INTERPSYS-IMAGE-INIT)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T) *) |lisplibError|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) *) CONCAT LOCALDATABASE |ncBug|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T T T T T T) *) |makeFortranFun|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) T) |canCoerceByFunction1| |L2Sm| L2M
            |Scr2Scr| |Sm2M| |altTypeOf| |Sm2Rm| |getMappingArgValue|
            I2NNI |spad2BootCoerce| |P2Uts| |augProplistInteractive|
            |Expr2Mp| |Expr2Dmp| |Dmp2Mp| |augProplist| I2EI L2V
            |getArgValueComp| |coerceIntX| |addBindingInteractive|
            |Set2L| |position,posn| |Rm2Sm| |Var2Up| |evalFormMkValue|
            |Expr2Up| |rewriteMap| |NRTcompileEvalForm| OV2OV
            |isRectangularList| |sideEffectedArg?| |substring?|
            |patternCheck,equal| PUTALIST |stringPosition|
            |centerString| RWRITE |setMsgForcedAttr| ADDOPTIONS
            |BesselIAsympt| |get1| |get2| |PsiBack|
            |PsiAsymptoticOrder| |cotdiffeval| |get0|
            |setMsgUnforcedAttr| |addIntSymTabBinding| |remprop|
            |insertShortAlist| |PiMinusLogSinPi| |logH|
            |recordOldValue| |recordNewValue| QESET |chebstarevalarr|
            |chebf01coefmake| |fortFormatHead|
            |transferPropsToNode,transfer| |besselIcheb| AS-INSERT
            |getfortarrayexp| |P2Expr| |NDmp2NDmp| |orderPredTran|
            |Dmp2NDmp| |Dmp2Dmp| |Up2SUP| |Ker2Expr| |substVars|
            |addMap| |Var2NDmp| |Var2P| |Var2FS| |replaceVars|
            |fnameNew| |Qf2EF| HPUT |orderPredicateItems| |fnameMake|
            |Rn2F| |coerceDmp2| |optSpecialCall|
            |getOplistWithUniqueSignatures| |mapRecurDepth| |writeLib1|
            |Qf2Qf| MAKE-PARSE-FUNC-FLATTEN-1 |lookupInDomainByName|
            |augLisplibModemapsFromCategory| |sigDomainVal|
            |NDmp2domain| |P2Uls| I2OI |Complex2Expr| |L2Tuple|
            |mkInterpFun| |newExpandGoGetTypeSlot| |P2Dmp| |P2Mp|
            |Qf2domain| |Mp2FR| |domain2NDmp| |displaySingleRule|
            |Sy2Up| |npAndOr| |Sy2NDmp| OV2P |restoreDependentMapInfo|
            |Sm2V| SUBSTRING M2V |Un2E| |analyzeNonRecur| |V2Sm| |L2Rm|
            |intloopInclude0| |M2Rm| |analyzeMap0| |Mp2Up| |Rm2L| M2L
            |npListofFun| |application2String| |Sy2Dmp| DP2DP
            |Dmp2Expr| |deleteMap| |Rm2V| |compileDeclaredMap| |SUP2Up|
            |pileForests| |Sm2L| |Dmp2Up| |mkNewUnionFunList| |V2Rm|
            |EnumEqual| PROPERTY |nopile| |compileCoerceMap|
            |UnionEqual| DEF |countParens| |MappingEqual| |RecordEqual|
            PUT |charPosition| MAKESPAD |rewriteMap1| |interpret1|
            DEF-INNER |assignSymbol| |moreGeneralCategoryPredicate|
            |upTableSetelt| |encodeUnion| |upSetelt|
            |simpHasPred,simpHas| |NRTisRecurrenceRelation| SUBLISLIS
            |evalQUOTE| |makeCatPred| |upLETtype| |nsubst| |evalis|
            |compiledLookup| |upwhereMain| REDUCE-1 |evalIF|
            |sublisMatAlist| |upLETWithFormOnLhs| |charybdis|
            |upwhereMkAtree| |upwhereClause| |IFcodeTran| |charyTop|
            |splitConcat| |interpret2| SUBSTEQ |coerceTraceFunValue2E|
            |evalIsntPredicate| ADDASSOC |pfTLambda| |evalIsPredicate|
            |getFunctionFromDomain| AS-INSERT1
            |coerceConvertMmSelection;| |reportOpSymbol,sayMms|
            |outputString| |longext| |isLegitimateMode;|
            |hasFileProperty;| ELEMN |fortCall| |writeStringLengths|
            |hasFilePropertyNoCache| B-MDEF |writeXDR| |isEltable|
            |selectMms| |keyedMsgCompFailureSP| |throwKeyedMsgSP|
            |rwrite| |getConditionalCategoryOfType| |resolveTMEq2|
            |filterListOfStringsWithFn| |rread| |flowSegmentedMsg|
            |makeResultRecord| |pushDownOnArithmeticVariables|
            |selectMmsGen,exact?| |prepareResults,defaultValue|
            |displayModemap| |filterModemapsFromPackages|
            |displayModemap,g| |resolveTTEq2| |getLocalMms|
            |resolveTTEq1| |evalMmCond0| |userLevelErrorMessage|
            |addToConstructorCache| |commandAmbiguityError| |incZip|
            |recordInstantiation| |resolveTTRed2| |coerceTypeArgs|
            |resolveTTRed1| |recordInstantiation1| |nextown|
            |evalMmCond| |intCodeGenCoerce1| |matchTypes| |displayType|
            |recordNewValue0| |displayMode| |recordOldValue0|
            |getValueFromSpecificEnvironment| |matchUpToPatternVars|
            |displayCondition| CARCDRX1 |mkAtree2| |mkAtree3|
            |coerceOrThrowFailure| |displayValue| MKPFFLATTEN-1 |get|
            |pfInfApplication| |mkIterFun| |mapLetPrint| SETDATABASE
            |letPrint| |upStreamIterIN| |loadLibNoUpdate| |letPrint2|
            |augmentLisplibModemapsFromFunctor| |maprinSpecial|
            SMALL-ENOUGH-COUNT |upLoopIterIN| |putI| |readLib1|
            |insertAlist| |coerceTraceArgs2E| MONITOR-PRINARGS
            |updateDatabase| |npList| |traceDomainLocalOps| |sigsMatch|
            |asyCattranOp1| |pfWith| VMLISP::MAKE-ENTRY
            |getConstructorOpsAndAtts| THETACHECK |pfPushBody|
            |pfLambda| |infixArgNeedsParens| |ncPutQ| |lisplibWrite|
            |pfIf| |pfWIf| |outputNumber| |mkRecordFunList|
            |mkUnionFunList| |mkMappingFunList| |unabbrevSpecialForms|
            |mkEnumerationFunList| |needBlankForRoot|
            |throwKeyedErrorMsg| |lassocShiftWithFunction| |nAssocQ|
            |LargeMatrixp| |findCommonSigInDomain|
            |mkUserConstructorAbbreviation| |evalMmCat1| |hasCaty|
            |augmentSub| |hasCate| |getCatForm| |getLocalMms,f|
            |findUniqueOpInDomain| |chebevalarr| |lazyOldAxiomAddChild|
            |BesselasymptA| |evalREPEAT| |errorSupervisor1|
            |upNullTuple| |hasAtt| |unifyStruct| |evalSEQ|
            |lazyDomainSet| |newExpandLocalType| EQSUBSTLIST |clngamma|
            |hasSigOr| |newExpandLocalTypeForm| |clngammacase23|
            |hasSigAnd| |clngammacase1| |hasAttSig| |unifyStructVar|
            |argumentDataError| |oldAxiomAddChild| |constrArg|
            |attributeCategoryBuild| |isOpInDomain| |newExpandTypeSlot|
            |commandErrorMessage| |postCollect,finish| /MONITORX
            |domainVal| |domArg2| |oldAxiomCategoryBuild|
            |oldAxiomPreCategoryBuild| SPADRWRITE SPADRWRITE0
            |interpCOLLECT| |evalCOLLECT|
            |getSlotNumberFromOperationAlist| |addBinding|
            |interpRewriteRule| |oldAxiomCategoryNthParent|
            |filterAndFormatConstructors| |PARSE-getSemanticForm|
            |getOpCode| PRINT-XDR-STREAM |throwKeyedMsgFromDb|
            |sayKeyedMsgFromDb| |substringMatch| |pushDownTargetInfo|
            |putAtree| |evalCOERCE| |upTaggedUnionConstruct|
            |throwListOfKeyedMsgs| |upRecordConstruct| |subVecNodes|
            |upNullList| |extendsCategoryBasic0| |substSlotNumbers|
            |lffloat| |interpCOLLECTbody| |assocCacheShiftCount|
            |assocCacheShift| |extendsCategoryBasic| |assocCache|
            FOAM:|FormatNumber| |catExtendsCat?| |NRTgetLookupFunction|
            |stuffSlot| |asytranApply| COMP-SPADSLAM COMP-SLAM
            |asySig1| |rewriteMap0| |asytranForm1| |findLocalsInLoop|
            |displayMap| |getOpBindingPower|
            |sayFunctionSelectionResult| |hput|
            |fortFormatLabelledIfGoto| MSUBST |compiledLookupCheck|
            |asGetExports| |mkFortFn| |coerceOrCroak|
            |SpadInterpretStream| |NRTcompiledLookup| |intloopProcess|
            |asyMakeOperationAlist| |coerceOrFail| |matchSegment?|
            |anySubstring?| |insertEntry| |exp2Fort2| |exp2FortFn| I2PI
            |infix?| P2FR |stringMatch| RPLNODE |makeEijSquareMatrix|
            |rightCharPosition| |Sm2PolyType| |Factored2Factored|
            |coerceFFE| |Mp2P| |intloopSpadProcess,interp| |Mp2Mp|
            |Dmp2P| |Ker2Ker| |Var2Mp| |Up2Expr| |Up2FR| |Up2Up|
            |Var2Dmp| |npBackTrack| |expandType| |expandTypeArgs|
            |Agg2Agg| |dcSig| |L2Record| |permuteToOrder| |Mp2Dmp|
            |computeTTTranspositions,compress| |Up2Dmp| |Var2Gdmp|
            |OV2Sy| HREMPROP |throwKeyedMsgCannotCoerceWithValue|
            |Var2SUP| |algCoerceInteractive| |mac0Define|
            |isRectangularVector| |incPrefix?| |getSubDomainPredicate|
            |Sy2Var| |buildPredVector,fn| |Sy2OV| |cleanUpAfterNagman|
            |sySpecificErrorAtToken| |makeCompilation| |Expr2Complex|
            |Complex2underDomain| |Sy2P| |algEqual| |inclmsgIfSyntax|
            |Var2OtherPS| |Up2P| |makeAspGenerators| |Var2UpS| |P2Upxs|
            |makeAspGenerators1| V2M OV2SE |Sy2Mp| |Complex2FR| |Up2Mp|
            |OV2poly| |mac0InfiniteExpansion| |Agg2L2Agg| |Qf2PF|
            |Mp2Expr| |mungeAddGensyms,fn| |insertAlist,fn| |Var2OV|
            |npParenthesize| V2DP |Var2QF| |P2Up| |coerceSubDomain|
            |buildPredVector| V2L L2DP |Rm2M| M2M |L2Set| |M2Sm|
            MAKE-DEFUN |retractUnderDomain| |coerceDmpCoeffs|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T) T) |xlOK1| |getArgValueComp2|
            |commuteMultivariatePolynomial| |getNewDefaultPackage|
            |coerceDmp1| |newCompareSig| |newLookupInDomain|
            |commuteDistributedMultivariatePolynomial|
            |commuteQuaternion| |analyzeRecursiveMap|
            |analyzeDeclaredMap| |augmentMap|
            |compileRecurrenceRelation| |reportFunctionCompilation|
            FINCOMBLOCK -REDUCE |split2| |compileIF| |concatTrouble|
            |selectMms1;| |charyBinary| |spadify| |selectMms2|
            |aggregateApp| |incLude| |bottomUpFormRetract|
            |bottomUpForm0| |bottomUpFormTuple|
            |bottomUpFormUntaggedUnionRetract| |selectMmsGen,matchMms|
            |bottomUpForm2| |bottomUpForm3| |resolveTT2|
            |printLabelledList| |putSrcPos| LOCALASY |needStar| |logS|
            |concatApp1| WRITE-TAG-LINE |interpLoopIter| |mkCacheVec|
            |makeConstrArg| |lazyMatchArg2| |newLookupInTable|
            |hashNewLookupInTable| |bottomUpDefaultCompile|
            |compileADEFBody| |bottomUpDefaultEval|
            |bottomUpFormAnyUnionRetract| |bottomUpForm|
            |sayFunctionSelection| |orderMms| |commuteSquareMatrix|
            |fortFormatDo| |incLine| |xlCannotRead| |xlMsg|
            |xlConStill| |commuteComplex|
            |commuteSparseUnivariatePolynomial|
            |commuteUnivariatePolynomial| |commuteMPolyCat|
            |commutePolynomial| |prepareResults| |coerceByTable|
            |commuteNewDistributedMultivariatePolynomial|
            |commuteFraction| |xlConActive| |xlFileCycle|
            |xlNoSuchFile| |xlSay|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T *) T) |lnCreate| |ncSoftError|
            TOKEN-INSTALL |asCategoryParts| |ncHardError|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T) T) |getArgValue2| |xlPrematureEOF|
            |P2Us| |canCoerceTopMatching| |semchkProplist| |domArg|
            |mkDomPvar| |augProplistOf| |putIntSymTab|
            |asytranCategory| |asytranCategoryItem| |fixUpPredicate|
            |optCallSpecially| |lazyMatchArg| |newLookupInCategories|
            |nrunNumArgCheck| MAKE-FLOAT |appneg|
            |analyzeUndeclaredMap| |npEnclosed|
            |analyzeNonRecursiveMap| |clearDep1| MAKELIB |interpREPEAT|
            |reportFunctionCacheAll| |put| |putHist| REDUCE-N-1
            REDUCE-N-2 |evalTuple| |appconc| |interpIF| |charyTrouble|
            |charyElse| |charySemiColon| STRPOS |charyEquatnum|
            |charySplit| |charyMinus| |printCName| |writeMalloc|
            |printDec| |axiomType| |evalForm| |analyzeMap|
            |selectLocalMms| |protectedNagCall| |evalMmFreeFunction|
            |evalMm| |nextown2| |getConditionalCategoryOfType1|
            |compClam| |collectOneStream| |upStreamIterSTEP|
            |letPrint3| |srcPosNew| |upLoopIterSTEP|
            ASHARPMKAUTOLOADFUNCTOR ASHARPMKAUTOLOADCATEGORY
            |getFileProperty| REDUCE-N |appInfix| |putFileProperty|
            |appSum| |termMatch| |bigopWidth| |condUnabbrev|
            |evalMmCat| |lookupComplete| |hasCateSpecial|
            |hasCateSpecialNew| |hasCate1| |clngammacase2|
            |compareSigEqual| |oldCompLookupNoDefaults|
            |intloopSpadProcess| |haddProp| |BesselasymptB|
            |selectMmsGen| |xLate| |boxLApp| |basicLookupCheckDefaults|
            |hasSig| |getReduceFunction| |lazyMatchAssocV|
            |NRTgetMinivectorIndex| |newLookupInCategories1|
            |lookupDisplay| |basicLookup| |matchMmSig| |sayLooking|
            |lazyMatch| |lazyMatchArgDollarCheck| |aggApp| |concatbApp|
            |allOrMatchingMms| |defaultTarget| |selectDollarMms|
            |concatApp| |collectStream| |lookupIncomplete|
            |bottomUpDefault| |findConstructorSlotNumber| |stringApp|
            |sigmaApp| |lookupInDomainVector| |oldCompLookup|
            |evalconstruct| |newLookupInAddChain| |sigma2App|
            |evalInfiniteTupleConstruct| |evalTupleConstruct|
            |hashNewLookupInCategories| |collectStream1|
            |collectSeveralStreams| |intApp| |evalUntargetedADEF|
            |evalTargetedADEF| |indefIntegralApp|
            |mkInterpTargetedADEF| |compileTargetedADEF| |piApp|
            |pi2App| |bracketApp| |braceApp| |mkIterZippedFun|
            |interpCOLLECTbodyIter| |mkAndApplyZippedPredicates|
            |rootApp| |say2Split| |interpLoop| |catPairUnion|
            |superSubApp| |simpHasSignature| |vconcatapp| |zagApp|
            |makeInternalMapName| |plusApp| |timesApp| |stepApp|
            |appsum| |typeToForm,fn| |appChar| |binomApp|
            |asGetModemaps| |asytranDeclaration| |altSuperSubApp|
            |boxApp| |appelse| |matchAnySegment?| |centerApp| |appext|
            |MpP2P| |nothingApp| |lazyCompareSigEqual| |constoken|
            |compareSig| |lookupInTable| |xlOK| |xlCmdBug| STRPOSL
            |xlPrematureFin| |catchCoerceFailure| |xlSkip|
            |coerceImmediateSubDomain| |xlIfBug| |prepareData|
            |mergeSort| |xlSkippingFin| |mac0MLambdaApply| |xlConsole|
            |mac0ExpandBody| |mergeInPlace|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T *) T) MATCH-TOKEN NREMOVE BPITRACE |wasIs|
            |tokConstruct| MATCH-LISP-TAG |categoryParts| |remove|
            RREAD |returnStLFromKey| |pfLeaf| |pfAdd| |listSort|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T) T) |genMapCode| |putMapCode|
            |upDollarTuple| |bracketagglist| |nagCall|
            |oldAxiomCategoryLookupExport| |BesselIBackRecur|
            |compHash| |mmCost0| |findFunctionInDomain1|
            |compDefineLisplib| |mmCost| |attributeLookupExport|
            |incLine1| |makeFort| |invokeFortran| |xlIfSyntax|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T) T) |writeCFile| /MONITOR
            |Mp2MpAux2|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T) T) |Expr2Dmp1|
            |findFunctionInCategory| |invokeNagman| |appInfixArg|
            |abbreviationError| |bigopAppAux|
            |lazyOldAxiomDomainLookupExport|
            |oldAxiomDomainLookupExport| |findFunctionInDomain|
            |Mp2MpAux1| |Mp2MpAux0| |mkDiffAssoc| |Mp2SimilarDmp|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T) T) |P2MpAux| BUILD-INTERPSYS
            |makeFort1|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T T T T T T T T) T) |P2DmpAux|
            |makeSpadFun|)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T *) T) RPLACSTR)) 
(PROCLAIM '(FTYPE (FUNCTION (T T T T T *) T) |msgCreate|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL *) |npPrimary1| PARSE-SEXPR PARSE-REF_SEXPR
            PARSE-CONS_SEXPR PARSE-LOCAL_VAR
            |describeSetStreamsCalculate| |printStatisticsSummary|
            |printStorage| MKPROMPT |princPrompt| |disableHist|
            RESETHASHTABLES |leaveScratchpad| |generateDataName|
            |displayFrameNames| |readSpadProfileIfThere|
            |listConstructorAbbreviations| MAKE-TAGS-FILE
            |npMDEFinition| |npCategory| |PARSE-Leave| |testPage|
            |clearCmdAll| |pquitSpad2Cmd| |quitSpad2Cmd| |PARSE-Label|
            HELP |PARSE-Sexpr| |pquit| |quit| |scanS| PARSE-EXPR1
            PARSE-EXPR2 RECLAIM |copyright| |PARSE-Category|
            |PARSE-Selector| |PARSE-Primary1| |PARSE-FloatBasePart|
            |npRule| |PARSE-Enclosure| |npDefn| |PARSE-Sexpr1|
            |npMacro| |generateResultsName| |npDefinitionItem|
            |executeQuietCommand|)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL T) |npSynthetic| |npBreak| |npPileExit|
            |npEncl| |traceReply| |inclmsgIfBug| |npCommaBackSet|
            |npSLocalItem| MONITOR-UNTESTED MONITOR-INITTABLE
            REDUCE-STACK-SHOW |voidValue| MONITOR-END CURRENT-SYMBOL
            NEXT-CHAR ADVANCE-CHAR IOSTAT EMBEDDED WRITE-WARMDATA
            WRITE-INTERPDB WRITE-CATEGORYDB |saveDependentsHashTable|
            |saveUsersHashTable| UNGET-TOKENS SKIP-BLANKS
            |initialiseIntrinsicList| CURRENT-CHAR |getIntrinsicList|
            |resetTimers| |displayHiddenConstructors| NEXT-LINES-SHOW
            TOKEN-STACK-SHOW INPUT-CLEAR NEXT-LINES-CLEAR
            IOSTREAMS-SHOW |cc| TERSYSCOMMAND |npWhile| PARSE-RULE1
            PARSE-FID |fin| |npConstTok| PARSE-EXPR |npNext|
            PARSE-SUBEXPR PARSE-FIL_TEST |npAmpersandFrom| PARSE-N_TEST
            |npFromdom| |rbrkSch| PARSE-TEST |npAmpersand| |npIterator|
            |npSuchThat| |lbrkSch| |npForIn| PARSE-SEXPR_STRING
            |random| PARSE-NON_DEST_REF |prTraceNames| PARSE-DEST_REF
            INITIALIZE |describeSetOutputAlgebra| ERRHUH
            |describeSetLinkerArgs| |npInfixOp| |npPushId| |npBy|
            |npVariable| |npSCategory| |describeSetOutputFormula|
            |npApplication| |npPrimary| |describeSetFortDir|
            |npDefaultValue| |npBacksetElse| |describeSetOutputFortran|
            YEARWEEK |npTagged| |npExpress1|
            |describeSetOutputOpenMath| |npADD|
            |npConditionalStatement| |describeSetOutputTex|
            |npTypeVariable| |npSignatureDefinee| IS-GENSYM
            |describeOutputLibraryArgs| |describeInputLibraryArgs|
            CURRENTTIME |npItem| |sayAllCacheCounts| |npFirstTok|
            |describeSetFunctionsCache| |npLocalItem| |poNoPosition|
            |describeSetOutputMathml| |npLocalDecl| |simpCategoryTable|
            |simpTempCategoryTable| |npDiscrim| |npDefTail|
            |npDefinitionOrStatement| |describeSetFortTmpDir|
            |pfNoPosition| |describeAsharpArgs| |spadThrow| |pspacers|
            |terminateSystemCommand| |spadPrompt|
            |displayPreCompilationErrors| |spadpo| |intUnsetQuiet|
            |clock| |intSetQuiet| |writeHistModesAndValues|
            |updateFromCurrentInterpreterFrame|
            |updateCurrentInterpreterFrame| |resetInCoreHist|
            $TOTAL-GC-TIME |clearCmdSortedCaches| |updateInCoreHist|
            TOP |spadReply| INIT-BOOT/SPAD-READER |PARSE-Expression|
            |sayShowWarning| POP-REDUCTION |processSynonyms|
            |PARSE-NewExpr| ADVANCE-TOKEN |makeInitialModemapFrame|
            |mkOutputConsoleStream| |loadExposureGroupData|
            |nextInterpreterFrame| |statisticsInitialization|
            |initializeInterpreterFrameRing| |previousInterpreterFrame|
            |spadStartUpMsgs| |statRecordInstantiationEvent|
            CURRENT-TOKEN RESTART0 NEXT-TOKEN FRICAS-INIT /TRACEREPLY
            |spad| |createCurrentInterpreterFrame|
            |reportAndClearClams| WRITE-OPERATIONDB
            |clearConstructorAndLisplibCaches|
            |clearHashReferenceCounts| |oldParserAutoloadOnceTrigger|
            |reportInstantiations| BUMPCOMPERRORCOUNT WRITE-BROWSEDB
            FAIL |displayExposedConstructors| |removeAllClams|
            |displayExposedGroups| |coercionFailure| |npPop1|
            PARSE-KEYWORD |npPop2| PARSE-ARGUMENT-DESIGNATOR
            |setOptKeyBlanks| INITIAL-GETDATABASE |npState|
            |npSignature| |npExit| |pfNothing| |npPPf|
            |displayWorkspaceNames| |npSigItemlist| |npApplication2|
            CATEGORYOPEN |npTerm| |npRemainder| BROWSEOPEN /EMBEDREPLY
            WRITE-COMPRESS |updateHist| |npTrap| |allOperations|
            |npDefinition| |npType| OPERATIONOPEN |npMDEF|
            |npSymbolVariable| |stopTimer| |npCategoryL| |npSigItem|
            |npSigDecl| |npId| |npDefaultItemlist| |npColon|
            |npRelation| |startTimer| |PARSE-SemiColon| |npTypified|
            |npAssignVariablelist| |npSQualTypelist| |npInterval|
            |PARSE-InfixWith| |npSegment| |PARSE-Suffix| |npArith|
            |npDisjand| |npSemiBackSet| |npVariablelist| |PARSE-Seg|
            |npSDefaultItem| |npPop3| |npTypeStyle| |PARSE-Loop|
            |npColonQuery| |PARSE-Import| |npPretend| |PARSE-With|
            |npCoerceTo| |PARSE-Data| |npRestrict| |PARSE-LabelExpr|
            |npDefinitionlist| |npSum| |npDecl| |PARSE-Return|
            |extendConstructorDataTable| |npExport| |PARSE-Exit|
            |PARSE-Conditional| |npLocalItemlist| |npPCff|
            |npAssignVariableName| |asList| |npLocal| |npInline|
            |npAtom2| |npProduct| |npMatch| |npPPff| |resetSpacers|
            |mkLowerCaseConTable| |pcounters| |ptimers| |quadSch|
            |clamStats| |intSetNeedToSignalSessionManager|
            |getInterpMacroNames| |version| |nangenericcomplex|
            |checkWarningIndentation| FRICAS-RESTART
            |resetWorkspaceVariables| |returnToReader| |initHist|
            $TOTAL-ELAPSED-TIME |returnToTopLevel| |initNewWorld|
            COMPRESSOPEN INTERPOPEN CREATE-INITIALIZERS |cacheStats|
            |clearClams| |clearCategoryCaches| |frameNames|
            |clearConstructorCaches| |allConstructors|
            |historySpad2Cmd| |clearFrame| |printableArgModeSetList|
            |PARSE-AnyId| |PARSE-Sequence| |PARSE-Sequence1|
            |getParserMacroNames| |PARSE-OpenBracket| |getParserMacros|
            |getWorkspaceNames| |PARSE-OpenBrace| |initHistList|
            BOOT-SKIP-BLANKS |clearCmdCompletely| |histFileName|
            |oldHistFileName| |PARSE-TokTail| |popTimedName|
            |setViewportProcess| |createTypeEquivRules|
            |interpFunctionDepAlists| CLEAR-HIGHLIGHT
            |PARSE-ElseClause| |createResolveTMRules| |peekTimedName|
            SPAD_LONG_ERROR |PARSE-Primary| |createResolveTTRules|
            |statRecordLoadEvent| RESET-HIGHLIGHT SPAD_SHORT_ERROR
            |getSystemCommandLine| |computeElapsedTime|
            |waitForViewport| |?t| |PARSE-PrimaryNoFloat|
            |PARSE-String| |resetCounters| |PARSE-IntegerTok|
            |PARSE-FormalParameter| |writeHiFi|
            |PARSE-FormalParameterTok| |clearMacroTable|
            |displayHeapStatsIfWanted| |piles| |PARSE-IteratorTail|
            |computeElapsedSpace| |scanEsc| |statisticsSummary|
            PARSE-SPADSTRING |reportWhatOptions| |synonymSpad2Cmd|
            |scanKeyTableCons| |reportCount| |scanDictCons| |scanError|
            |scanEscape| |scanNumber| |initializeSystemCommands|
            |genTempCategoryTable| |runspad| |makeConstructorsAutoLoad|
            |initializeRuleSets| |genCategoryTable| |intNewFloat|
            |inclmsgConsole| |inclmsgFinSkipped| FOAM:|fiGetDebugVar|
            |resetStackLimits| PARSE-OPT_EXPR PARSE-REPEATOR
            PARSE-HEADER PARSE-RULE |currentSP| PARSE-REP_TEST
            |ncTopLevel| |ncIntLoop| |credits| |intloop|
            |PARSE-Statement| |PARSE-Infix| |PARSE-Prefix|
            |PARSE-Qualification| |newFortranTempVar| |ncError|
            |PARSE-Iterator| |tempLen| |PARSE-Reduction|
            |PARSE-ReductionOp| |npPrefixColon| |PARSE-Form|
            |npInfixOperator| |scanToken| |PARSE-Application| |npName|
            |npPileDefinitionlist| |scanString| |scanSpace|
            |npSelector| |scanPunct| |PARSE-Float| |npGives|
            |PARSE-FloatBase| MONITOR-AUTOLOAD MONITOR-PERCENT
            |scanNegComment| |PARSE-FloatExponent| |startsNegComment?|
            |npQualDef| MONITOR-HELP |scanComment| |PARSE-FloatTok|
            |startsComment?| |PARSE-Quad| |npAssignVariable|
            |inclmsgCmdBug| |scanPunCons| |PARSE-VarForm| |PARSE-Name|
            |npQualifiedDefinition| MONITOR-READINTERP
            |npBPileDefinition| |getCodeVector| |getInfovecCode|
            MONITOR-RESULTS |NRTmakeCategoryAlist| |npQualTypelist|
            |NRTgenFinalAttributeAlist| |npFix| MONITOR-REPORT
            |npLogical| |npComma| |npFree| |npDefaultItem|
            |syGeneralErrorHere| |npDefaultDecl| |npIterators|
            |npAssignment| |npQualType| |npLet| |npAtom1| |npLambda|
            |npRecoverTrap| |npBDefinition| |npDollar| |npPDefinition|
            |npExpress| IN-META PARSE-PROGRAM |npAssign| |npSingleRule|
            |npVoid| |npReturn| |npPrimary2| |npVariableName| |npPPg|
            |npIterate| |npQuiver| |npPower| PARSE-STRING |npDef|
            |dcSizeAll| |npSuch| PARSE-BSTRING |npStatement|
            |incConsoleInput| |npLoop| PARSE-IDENTIFIER |npImport|
            PARSE-NUMBER |npTyping| |npPCg| |npTypeVariablelist|
            |npMdef|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) *) NEXT-LINE |newGoGet| /RQ /RF |Undef|
            META-SYNTAX-ERROR INIT-MEMORY-CONFIG |/RQ,LIB| /EF $ERASE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (*) T) MAKE-LINE MAKE-STACK MONITOR-ENABLE
            |incIgen1| MONITOR-TESTED |incRgen1| |next1| NEXT-META-LINE
            META-META-ERROR-HANDLER RDEFINSTREAM RDEFOUTSTREAM
            MAKE-REDUCTION $FILEP MAKE-TOKEN STREAM-EOF |sayNewLine|
            READ-A-LINE SAY |displayCategoryTable| META MOAN
            ENABLE-BACKTRACE RKEYIDS LISP-BREAK-FROM-AXIOM INTERRUPT
            IOCLEAR |Enumeration0| |incZip1| |incAppend1| |nextown1|
            CROAK MAKE-DATABASE FOAM::MAKE-FOAMPROGINFOSTRUCT
            |RecordCategory| |EnumerationCategory| |UnionCategory|
            NEXT-BOOT-LINE SPAD BOOT |runOldAxiomFunctor| |concat|
            |throwMessage| INITROOT TOPLEVEL |start| |canCoerce|
            |canCoerceFrom| |coerceConvertMmSelection|
            |hasFileProperty| |isLegitimateMode| SPAD_SYNTAX_ERROR
            |resolveTT| |selectMms1| |Union| |Mapping| MAKE-XDR-STREAM
            |synonym| |dc| |buildBitTable| VMLISP::MAKE-LIBSTREAM
            MONITOR-RESET MONITOR-DISABLE MAKE-MONITOR-DATA |sum|
            |incLude1| |dcSize|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) *) |NRTtypeHack| INIT-FILE-GETTER
            |fortFormatCharacterTypes,par2string| INIT-LIB-FILE-GETTER
            QUOTE-IF-STRING BPINAME OBEY GET-TOKEN |sayMSGNT| |optCall|
            WIDTH |optSPADCALL| |formatOpType| |form2String| |tokType|
            |form2String1| |subrname| |optCallEval| |npPP|
            |setStreamsCalculate| |setLinkerArgs| |setOutputFormula|
            |numArgs| DEF-MESSAGE1 |getBrowseDatabase| |object2String|
            |setOutputFortran| |tr| |setOutputOpenMath| |setOutputTex|
            DEF-WHERECLAUSELIST |setOutputMathml| |npPC| MAKE-OUTSTREAM
            |setFunctionsCache| |setExposeDrop| |setExposeAdd| DEF-ISNT
            |setExpose| |lnFileName| |pfGlobalLinePosn|
            |parseTransform| |setOutputAlgebra| |doSystemCommand|
            |justifyMyType| MONITOR-EVALAFTER |form2StringAsTeX|
            |form2StringLocal| |mathprintWithNumber| |reportOpSymbol|
            BRIGHTPRINT-0 |prefix2StringAsTeX| |restoreHistory|
            |changeHistListLen| |showHistory| |saveHistory|
            |outformWidth| |setHistoryCore| MAKE-REASONABLE
            |clearSpad2Cmd| |incHandleMessage| DEFTRAN |sayWidth|
            |prefix2String| |tuple2String,f| |mathObject2String|
            |numMapArgs| |prefix2String0| MAKE-DEPSYS
            |reportOpsFromUnitDirectly1| |editFile|
            |htCommandToInputLine| |reportOpsFromUnitDirectly0|
            |formString| |compileAsharpCmd1| |addNewInterpreterFrame|
            |doReplaceSharpCalls| |abbreviationsSpad2Cmd| |spadcall1|
            |importFromFrame| BOOT-LOAD |entryWidth| |getMsgPos2|
            |fixObjectForPrinting| |parseTranList| |withAsharpCmd|
            FILE-RUNNER COMPILE-BOOT-FILE |unAbbreviateKeyword|
            |getMsgTag| VMLISP::DELETE-DIRECTORY |parseLhs|
            MONITOR-PRINTREST VMLISP::GET-IO-INDEX-STREAM |outputTran|
            |makeSimplePredicateOrNil| |poFileName|
            VMLISP::GET-INPUT-INDEX-STREAM |transSeq| |parseUpArrow|
            |parseLeftArrow| |postTran| |ncTag| |ncAlist|
            |mac0InfiniteExpansion,name| |parseLessEqual| SRCABBREVS
            |parseGreaterEqual| |parseDollarLessEqual|
            |parseDollarGreaterEqual| |parseDollarNotEqual| |parseNot|
            |parseNotEqual| |NRTinnerGetLocalIndex| |obj2String|
            |parseAnd| |abbQuery| |assignSlotToPred| |parseIf|
            |mathPrint| |parseOr| |popSatOutput| |sayString| |parseSeq|
            |parseTran| |postMakeCons| |postSlash| |postConstruct|
            |postCategory,fn| |postBigFloat| |clearClam|
            |replaceSharpCalls| |postInSeq| |mkEvalableCategoryForm|
            |aggSub| |frameSpad2Cmd| |aggSuper| |initializeLisplib|
            |verbatimize| |quoteSub| |displaySpad2Cmd| |quoteSuper|
            |NRTevalDomain| |histFileErase| /RF-1 |whatSpad2Cmd|
            |systemCommand| |timedEvaluate| |tabsToBlanks|
            |showSpad2Cmd| |sayWidth,fn| |safeWritify| |spleI|
            |writifyComplain| |normalizeStatAndStringify| |savesystem|
            |escapeSpecialChars| |roundStat| |set| |show| |summary|
            |brightPrint0| |what| |clearCmdParts| |sayDisplayWidth|
            |sayDisplayWidth,fn| |overbarWidth|
            |brightPrintCenterAsTeX| |rootSub| |transHasCode|
            |brightPrintCenter| |brightPrintHighlightAsTeX| |apropos|
            |sayDisplayStringWidth| |brightPrintHighlight|
            |brightPrint0AsTeX| |subSuper| |compQuietly|
            |compileFileQuietly| |vConcatSuper| |compileQuietly|
            |InterpExecuteSpadSystemCommand|
            |ExecuteInterpSystemCommand| |pfFileName| |asyJoinPart|
            |porigin| |pfname| |exptSub| |subspan| |superspan|
            |poGlobalLinePosn| |agggsub| |agggsuper| |agggwidth|
            |abbreviations| |compiler| |fortPre1| |cd|
            |asyGetAbbrevFromComments| |printBasic| |clear|
            |checkPrecision| |close|
            |fortFormatCharacterTypes,mkCharName| |aggwidth|
            |fix2FortranFloat| |display| |qTSub| |frame| |qTSuper|
            |history| |intern| |phBegin| |form2StringWithPrens|
            |matSub| |mkEvalable| |fortError1| |exp2FortOptimizeArray|
            |pred2English| |overlabelWidth| |formatAttributeArg|
            |formatSignatureArgs0| |mkParameterList,par2string|
            |formatSignatureArgs| MAKE-APPENDSTREAM MAKE-INSTREAM
            |halfWordSize| MONITOR-RESTORE KILL-TRAILING-BLANKS
            |parseAndEvalStr1| |parseAndEvalStr|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) CHARACTER) LINE-CURRENT-CHAR NUM2CHAR EBCDIC)) 
(PROCLAIM '(FTYPE (FUNCTION (T T *) FIXNUM) LINE-NEW-LINE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) STRING) DROPTRAILINGBLANKS LINE-BUFFER
            |stripSpaces| |make_spaces|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T) T) |macId| |parse2Outform| |pfSequenceArgs|
            |isHasDollarPred| |evaluateFormAsType|
            |typeIsASmallInteger| |str2Outform| |dcData|
            |pfCheckMacroOut| |macMacro| |wrap| |incFileName|
            |evaluateType1| |bubbleConstructor| |macLambda|
            |inclmsgConActive| |pfApplication?| |inclmsgNoSuchFile|
            |pfBreak| IS-CONSOLE |pfMacro?| |objValUnwrap| |pfTyping|
            LINE-P |pfWhere?| |inclmsgPrematureEOF| |pfNovalue|
            |mkEvalableMapping| |pfNothing?| |incFile| |makeOrdinal|
            |pfMacroRhs| |getAndEvalConstructorArgument| |pfMacroLhs|
            |evaluateType| |fileNameStrings| MAKE-ABSOLUTE-FILENAME
            STACK-P |mkEvalableUnion| MONITOR-INCR |mkEvalableRecord|
            TOKEN-P |getUnnameIfCan| |evaluateSignature| |rightTrim|
            |leftTrim| |incIgen| |simpBool| MONITOR-DIRNAME
            |asTupleAsList| |simpCatPredicate| MONITOR-FILE |npNull|
            |devaluateDeeply| |StreamNull| |charDigitVal|
            VMLISP::LIBRARY-FILE |bool| MONITOR-DECR |incRgen|
            |length2?| |evaluateType0| |failCheck| OPTIONAL ACTION
            |getMsgPos| |erMsgSep| |dropLeadingBlanks| |poNopos?|
            MATCH-ADVANCE-STRING MSORT |getLineText|
            VMLISP::GET-DIRECTORY-LIST NMSORT |processKeyedError|
            |orderList| |selectMostGeneralMm| |str2Tex| |whichCat|
            GET-META-TOKEN |trimString| VMLISP::PROBE-NAME |pr|
            |getLinePos| GET-A-LINE |msgOutputter| |msgImPr?|
            |hashable| |getIProplist| |getMsgPrefix?| |getMsgKey|
            |getMsgTag?| TRANSPGVAR |getMsgPosTagOb| GETGENSYM COPY
            |srcPosLine| |listOutputter| DOWNCASE DATABASE-CONSTRUCTOR
            |srcPosDisplay| |pfOr?| DATABASE-ANCESTORS |srcPosColumn|
            |pfAndRight| |makeLazyOldAxiomDispatchDomain| FLOATEXPID
            |srcPosSource| |pfAndLeft| DATABASE-SPARE |getToken|
            |srcPosFile| |pfAnd?| |processChPosesForOneLine|
            DATABASE-DEPENDENTS |pfWrong?| SHOWDATABASE |asTupleSize|
            |pf0LocalItems| |pfLocal?| |mkAtreeExpandMacros|
            |pf0FreeItems| |negintp| |isInterpMacro| |pfFree?|
            |fracpart| |pfRestrictType| |initToWhere| COMP
            |pfRestrictExpr| |initImPr| |fortexp0| |pfRestrict?|
            |putDatabaseStuff| |cgammat| |expression2Fortran|
            |pfDefinition?| |fortranCleanUp| |pfAssignRhs|
            |getMsgInfoFromKey| |intpart| |pf0AssignLhsItems|
            |getMsgArgL| |fortSize| |pfAssign?| |fortSize,elen|
            |pfTypedType| BVEC-COPY |pfTyped?| |pfDoBody| LINE-AT-END-P
            |devaluateList| |fortFormatElseIf| |removeBindingI| |pfDo?|
            DATABASE-CONSTRUCTORCATEGORY |pfSourceText|
            |pfSuchthatCond| DATABASE-OPERATIONALIST |poGetLineObject|
            |pfSuchthat?| |phiRatapprox| |fortFormatIntrinsics|
            |lnPlaceOfOrigin| |pfWhileCond| DATABASE-OBJECT
            |lnrgammaRatapprox| |pfPosOrNopos| |pfWhile?| STACK-POP
            DATABASE-NILADIC |fortFormatIf| |computedMode|
            |pfForinWhole| DATABASE-MODEMAPS |PsiIntpart|
            |dispfortexp1| |pf0ForinLhs| DATABASE-USERS BVEC-NOT
            |pfForin?| DATABASE-PARENTS |fortran2Lines1|
            |pf0LoopIterators| |asyComma?| |loopIters2Sex|
            |makeOldAxiomDispatchDomain| |pfLoop?| MATCH-STRING
            |pfExitExpr| |dispStatement| |pfExitCond| CONSOLEINPUTP
            |pfExit?| |getfortexp1| |pfFromdomDomain| UNDERSCORE
            |pfFromdomWhat| |pfFromdom?| |pfPretendType|
            LINE-CURRENT-SEGMENT |analyzeMap,f| |pfPretendExpr|
            |testPredList| |sayTeX| |pfPretend?| |isDomainSubst|
            |pfCoercetoType| |signatureTran| |mapPredTran|
            |pfCoercetoExpr| MAKE-SYMBOL-OF |pfCoerceto?| TRY-GET-TOKEN
            |notCoaf| |saySpadMsg| |fnameReadable?| |pfTaggedExpr| SHUT
            |sayALGEBRA| |fnameName| |pfTaggedTag| |optEQ|
            LINE-NEXT-CHAR |interactiveModemapForm,fn| |dnf2pf|
            |sayMSG| |DirToString| |pfTagged?| |hashString|
            |optMkRecord| LINE-ADVANCE-CHAR |be| |pfIfElse| |bnot|
            |sayMSG2File| |pfIfThen| |getOplistForConstructorForm|
            |notDnf| |sayFORTRAN| |myWritable?| |pfIfCond| |b2dnf|
            |sayFORMULA| |StringToDir| |getUserIdentifiersIn| |pfIf?|
            |optimize| |getOperationAlistFromLisplib|
            MAKE-STRING-ADJUSTABLE |bor| |newHasTest,evalCond|
            |fnameType| |pfLiteral?| |optimize,opt| REROOT STACK-CLEAR
            |list3| |removeEXITFromCOND| |fnameDirectory|
            |pfSequence2Sex0| DIG2FIX |mkDatabasePred| |band|
            |mergeCONDsWithEXITs| |fnameExists?| |pfLambdaTran|
            VMLISP::COMPILE1 |length1?| |fnameWritable?|
            |pfDefinitionRhs| |optLESSP| |mkAlistOfExplicitCategoryOps|
            |extractCONDClauses| |pf0DefinitionLhsItems| |optSEQ|
            |bootCOND| |pfLambdaBody| |optSEQ,tryToRemoveSEQ|
            |bootPushEXITintoCONDclause| |pfLambdaRets|
            |optSEQ,getRidOfTemps| |bootTran| |optSEQ,SEQToCOND|
            |bootSEQ| |optimizeFunctionDef| |bootPROGN|
            |optimizeFunctionDef,removeTopLevelCatch|
            |bootAbsorbSEQsAndPROGNs,flatten| LOG2
            |bootAbsorbSEQsAndPROGNs| |tryToRemoveSEQ| |asTupleNew0|
            |optCond| |TruthP| SIZE |optCONDtail| |asytranEnumItem|
            EOFP |optPredicateIfTrue| |asyTypeJoinStack|
            |asyTypeMakePred| RSHUT |combineMapParts| |optMINUS|
            |mkMapPred| |bootOR,flatten| |isFreeVar| |bootAND,flatten|
            |sumWidth| |optCatch| |bootLabelsForGO| |bootIF| |optCons|
            EXPAND-TABS |bootOR| |unabbrev| |bootAND|
            |matrix2String,outtranRow| |compileTimeBindingOf|
            |breakIntoLines| |removeEXITFromCOND?| |optQSMINUS| CSCH
            |optSETRECORDELT| |isLocalVar| |tuple2String|
            |listOfPredOfTypePatternIds| |npTuple| |removeIsDomains|
            |optIF2COND| |untrace| MANEXP |optXLAMCond|
            |removeTracedMapSigs| ACOT S-PROCESS |optRECORDCOPY| COT
            |pfEnSequence| |formWrapId| DIGITP |shortenForPrinting|
            |opt-| SEC |isInternalFunctionName| |optSuchthat| CSC
            DEF-INSERT_LET1 |pfListOf| |optRECORDELT| |stripUnionTags|
            ACSC |pfTuple| |binop2String| |mkPredList|
            |translateYesNo2TrueFalse| ASEC |functionp| MKPROGN
            |matrix2String| VEC2LIST |isRecord| COTH |macrop|
            |isInternalMapName| |npPileBracketed| |formCollect2String|
            |pileColumn| SECH |asyTypeJoinPartWith|
            |sayRemoveFunctionOrValue| DEF-ADDLET |formJoin2String|
            MAKE-VEC |string2Float| |pileCforest| ACSCH DEF-RENAME1
            GCMSG |enPile| ACOTH DEF-STRING |constructorName|
            |separatePiles| |initializeSetVariables| ASECH
            |makeArgumentIntoNumber| DEF-WHERE |record2String|
            DEF-REPEAT |npCompMissing| |pilePlusComments|
            |translateTrueFalse2YesNo| DEF-COLLECT |isBinaryInfix|
            |pilePlusComment| |setOutputLibrary| |appOrParen|
            |insertpile| INTERPSYS-ECL-IMAGE-INIT DEF-COND
            |pfAttribute| |formatSignature| DEF-MESSAGE |pileComment|
            |string2BootTree| IS_GENVAR |evalDomain| |string2SpadTree|
            |IS_#GENVAR| |mkAliasList| HACKFORIS |findEqualFun|
            |setFortDir| |npWConditional| |lastTokPosn|
            |validateOutputDirectory| SPAD-CLEAR-INPUT HACKFORIS1
            |concatList| |coerceMap2E| |firstTokPosn| SOURCEPATH
            |freeOfSharpVars| |getIteratorIds| DEF-IS |listOfSharpVars|
            |alqlGetOrigin| |constructorCategory| |args2Tuple|
            DEF-IN2ON |makeByteWordVec| |isPatternVar| LIBCHECK
            |getUserIdentifiersInIterators| DEFIOSTREAM DEF-IS-REMDUP
            |setExposeDropConstr| |untraceMapSubNames| DEF-IS-EQLIST
            |npParenthesized| |poImmediate?| |setExposeDropGroup|
            |npParse| |lnString| |removeBodyFromEnv| LIST2CONS
            |lnGlobalNum| |pfDocument| |pfPosn| |countCache|
            DEF-IS-REMDUP1 |pfPlaceOfOrigin| |setExposeAddGroup|
            |getEqualSublis| LIST2CONS-1 |poPlaceOfOrigin|
            |simpHasPred,simp| |pfSourcePositions| |predTran|
            DEF-WHERECLAUSE |mkCircularAlist| |poNoPosition?|
            |lnImmediate?| |clearTempCategoryTable| |setInputLibrary|
            |npInfKey| |pfSourcePositionlist| |DEF-:| |pfPosImmediate?|
            |upisnt| DEF-ELT |nodeCount| |pfFileName?| |upisAndIsnt|
            DEF-SETELT |poFileName?| DEF-LESSP |setFortTmpDir| DEF-SEQ
            |last| |lnExtraBlanks| |setHistory| LASTATOM
            |altSeteltable| DEF-EQUAL |pfSourceToken|
            |categoryParts,exportsOf| |copyHack| |pfFirst|
            |copyHack,fn| |DEF-::| |pfGetLineObject| |setAsharpArgs|
            DEF-SELECT |simpHasPred,simpDevaluate| |lnLocalNum|
            |getConstrCat| |transTraceItem| |getCacheCount|
            |lnFileName?| |poIsPos?| |pfNopos?|
            |getCategoryExtensionAlist0| |getTraceOption| REMDUP
            ASSOCLEFT |upQUOTE| |postTransform| ASSOCRIGHT |upbreak|
            |uppretend| |timedOptimization| |upreturn| |typeOfType|
            DEF-PROCESS |clearCache| |upfree| |clearAllSlams|
            VMLISP::FLAT-BV-LIST |isTupleForm| INITIALIZE-PREPARSE
            |clearSlam| PREPARSE1 |uptypeOf| PARSEPRINT SEQOPT
            PREPARSEREADLINE |removeConstruct| |knownEqualPred|
            |recordAndPrintTest| MKQSADD1 ATENDOFUNIT PREPARSE-ECHO
            |isHomogeneous| |outputTranMatrix,outtranRow|
            VMLISP::REMOVE-FLUIDS GENSYMP |texFormat| PREPARSEREADLINE1
            |upis| SKIP-IFBLOCK |ncParseAndInterpretString|
            |packageTran| THETA_ERROR SKIP-TO-ENDIF |userError|
            INFIXTOK |upLETWithPatternOnLhs| LISTOFATOMS
            |quote2Wrapped| |nakedEXIT?| |flattenCOND|
            |segmentKeyedMsg| NUMOFNODES |mergeableCOND| |printAsTeX|
            |isLocalPred| |pfTLambdaArgs| |isHomogeneousArgs|
            |pfCheckInfop| SUBANQ |upequation| |pfAttributeExpr|
            |pf0FlattenSyntacticTuple| |formatSignature0| |uplocal|
            |pfRetractTo?| |underDomainOf;| |pfExpression?|
            |specialChar| |pfQualTypeType| |sumoverlist|
            |getUnderModeOf| |pfWith?| |removeIsDomainD| |pfAdd?|
            |deconstructT| |pfWIfElse| |pfCollectVariable1| |upLET|
            |pfIterateFrom| UNVEC |upwhere| |pfInline| |pfSecond|
            |handleLispBreakLoop| |upIF| |pfDocumentText| |npboot|
            |changeToNamedInterpreterFrame| |variableNumber|
            |pfLoopIterators| |pfExpr?| |pfCollect1?| |pfAddAddon|
            |charyTopWidth| BRIGHTPRINT |pfWithWithin|
            |form2StringWithWhere| |pfWithWithon| |pfReturnFrom|
            |productOrParen| |pf0CollectIterators| |remWidth| |eqType|
            MESSAGEPRINT |pfImport| |powerOrParen| |pfTLambdaBody|
            MESSAGEPRINT-1 |pfTaggedToTyped1| |formJoin2| |pfWIfThen|
            |isValidType;| MESSAGEPRINT-2 |pfAttribute?| |pfWDeclare?|
            |pfTupleParts| |pfComDefinitionDef| |systemError|
            |form2Fence| VMLISP::VARP |pfSemiColon|
            |isLegitimateRecordOrTaggedUnion| |pfDocument?|
            |pfTupleListOf| |listOfDuplicates| |devaluate|
            |pf0WrongRubble| |pfInlineItems| |isPolynomialMode|
            |formatAttribute| |asyTypeUnitList| |pfWrongRubble|
            |mathmlFormat| |removeUndoLines| |equiType| MACROEXPANDALL
            |pfWithBase| |formatIf| |pf0ExportItems|
            |histInputFileName| |incFileInput| |pfRetractToType|
            |pfExportItems| |dispfortexp| |readHiFi| |Else?|
            |isPartialMode| NEXT-TAB-LOC |pfAddAddin| |formulaFormat|
            |pf0TypingItems| NONBLANKLOC |formatArgList| |asyParents|
            |pfTLambdaRets| |processSynonymLine,removeKeyFromLine|
            INDENT-POS |pfTypingItems| |script2String| |pfTupleList|
            |spad2lisp| BLANKP |pfTransformArg| |displayTranModemap|
            TRACEOPTIONS |pfWIfCond| STACK-SIZE |pf0WithWithin|
            |formatModemap| |stripLisp| CHARP |pfSexpr,strip|
            |categoryForm?| DEF-INSERT_LET |pf0ImportItems|
            |dumbTokenize| |incPos| |formatModemap,fn|
            DEF-STRINGTOQUOTE |pfImportItems| |stringer|
            VMLISP::SIMPLE-ARGLIST |pfFreeItems|
            |getConstructorSignature| |incStringStream|
            |interactiveModemapForm| |bootTransform| |pfQualType?|
            |outputOp| |closeInterpreterFrame| |pfComDefinitionDoc|
            |incRenumber| |asIsCategoryForm| SMINT-ABLE
            |pfAssignLhsItems| |canRemoveIsDomain?| |pfInline?|
            /UNEMBED-Q |makeHistFileName| REDUCTION-VALUE |pfSexpr|
            |pfWhile| |updateSourceFiles| |pfSemiColonBody|
            |pfDWhereExpr| |pathnameName| |bottomUpElt|
            |form2StringList| |pfHidePart| |pathnameType|
            |pfExitNoCond| |sayModemap| |pfWrongWhy| |getCType|
            |pathnameDirectory| RPACKFILE |pf0AddBase| |mkAtreeNode|
            |pfWIf?| |reportOpsFromUnitDirectly| STACK-STORE
            |pfQualTypeQual| |formatMapping| |int2Bool|
            |pfStringConstString| |say2PerLine| |pfExport?| DROPENV
            |whatConstructors| SHOWBIND |nplisp| |pfRetractToExpr|
            VMLISP::EQUABLE |spadTypeTTT| |getMapSubNames|
            |pfLambdaArgs| |getUnionOrRecordTags| |makeUnion|
            |coerceSpadFunValue2E| |getModeOrFirstModeSetIfThere|
            |abbreviate| |stripNil| |setIOindex|
            RECOMPILE-LIB-FILE-IF-NECESSARY |pfDeclPart?| |error|
            |isSharpVar| |sumOrParen| STACK-UPDATED |pfDWhere?|
            |pfImport?| |namestring| |emptyAtree| |new2OldLisp|
            |Record0| |pfTyping?| |bubbleType| |isIntegerString|
            LIBSTREAM-DIRNAME DEF-RENAME |pfSuchthat| GETZEROVEC
            |cleanUpSegmentedMsg| |tokTran| LINE-PRINT
            |pfDefinitionLhsItems| |form2FenceQuoteTail|
            |parseSystemCmd| |pfSemiColon?| |containsPolynomial|
            |form2FenceQuote| |resolveTMRed1| PNAME LINE-PAST-END-P
            |parseFromString| |bottomUpPercent| |linearFormat|
            |evaluateLines| |lispType| |resolveTTRed3| |pf0WithBase|
            |fetchOutput| |formatOpConstant| |interpOp?|
            |compileAsharpArchiveCmd| |coerceOrParen| |isAVariableType|
            |compileSpadLispCmd| MAKE-BVEC |pfComDefinition?|
            |dollarPercentTran| |makeLispList| |compileAsharpLispCmd|
            TOKEN-PRINT |pf0TLambdaArgs| |mkHashCountAlist|
            |compileAsharpCmd| |displayProperties,sayFunctionDeps|
            |remHashEntriesWith0Count| |destructT| |polyVarlist|
            |removeQuote| |numberOfEmptySlots| |bottomUpCompile|
            |multiToUnivariate| FOAM-USER::|AXL-spitSInt| TOKEN-SYMBOL
            /TRACE-0 /OPTIONS |noSharpCallsHere| |fixUpTypeArgs|
            |functionAndJacobian| |reportUndo| |XDRFun| OPTIONS2UC
            |outputDomainConstructor| |prefix2Infix|
            |emptyInterpreterFrame| |removeAttributes| |isFunctor|
            |typeTimePrin| FOAM:|printNewLine| LOAD-DIRECTORY
            |getMsgKey?| |constructor2ConstructorForm| |evalMmStack|
            |sayAsManyPerLineAsPossible| |makeMsgFromLine|
            RECOMPILE-ALL-ALGEBRA-FILES |poLinePosn|
            |spadThrowBrightly| |evalMmStackInner| |parseAtom|
            RECOMPILE-ALL-FILES |containsVariables| |parseTran,g|
            RECOMPILE-LIB-DIRECTORY |getMsgFTTag?| |isUncompiledMap|
            RECOMPILE-ALL-LIBS |alreadyOpened?| |isInterpOnlyMap|
            |transIs| RETRANSLATE-DIRECTORY |erMsgSort|
            |isDomainOrPackage| |wrapped2Quote| |clearConstructorCache|
            |isListConstructor| |toFile?| |transIs1| SQUEEZE
            |frameEnvironment| |getMsgToWhere| IDENT-CHAR-LIT
            |isSubForRedundantMapName| |objCodeVal| |displayHashtable|
            |opOf| |makeLeaderMsg| EQUABLE |objCodeMode| TRANSLIST
            |recordFrame| |msgLeader?| MKQ |minimalise,HashCheck|
            |wrapMapBodyWithCatch| |parseType| |minimalise| COND-UCASE
            |transUnCons| RENAME |minimalise,min| |parseTypeEvaluate|
            |processSynonymLine| |toScreen?| /UNTRACE-0 |isTaggedUnion|
            DATABASE-PREDICATES |printSynonyms| |getMsgPrefix|
            |constructor?| DATABASE-ATTRIBUTES |line?| /UNTRACE-REDUCE
            |mkAtree1| |pathname| DATABASE-DOCUMENTATION
            |synonymsForUserLevel| |leader?| |untraceDomainConstructor|
            |transformREPEAT| |isExistingFile| DATABASE-CONSTRUCTORFORM
            |isDomain| |transformCollect| |abbreviation?|
            DATABASE-SOURCEFILE |displayMacro| |getMsgText|
            |getConstructorModemap| SET-FILE-GETTER |poCharPosn|
            |mkAlistOfExplicitCategoryOps,atomizeOp| SMALL-ENOUGH
            |getUnname1| |flattenSignatureList| |getStFromMsg|
            |npRestore| |getFlag| UNSQUEEZE |tabbing| |mkAtreeValueOf|
            |parseHas,fn| |clearParserMacro| |getDomainFromMm|
            |getMsgLitSym| HAS_SHARP_VAR |mkAtreeValueOf1|
            |parseHas,mkand| |isExposedConstructor| |getPosStL|
            |getSrcPos| |mkAndApplyPredicates| |unabbrevAndLoad|
            |undoCount| |isFreeFunctionFromMm| |getPreStL|
            |isSharpVarWithNum| |unVectorize| |upStreamIters|
            DATABASE-COSIG |splitIntoOptionBlocks| |npPush|
            |asTupleNewCode0| |parseHasRhs| |remFile| |npInfGeneric|
            IS_SHARP_VAR |collectDefTypesAndPreds| |loadIfNecessary|
            |remLine| |formal2Pattern| |posPointers|
            |satisfiesUserLevel| |dropPrefix| |npSemiListing|
            |parseJoin,fn| MONITOR-BLANKS |prTraceNames,fn|
            |asTupleAsVector| DATABASE-P |poPosImmediate?|
            |displayMacros| |moveORsOutside| |npQualified| |isLeaf|
            DATABASE-DEFAULTDOMAIN |asyTypeUnit| |displayOperations|
            |getFirstArgTypeFromMm| |npLetQualified| |undoFromFile|
            |msgNoRep?| |spadReply,printName| |postin|
            |getUsersOfConstructor| |npAdd| |postIn| |writify|
            |showMsgPos?| |orderBySlotNumber| |postRepeat|
            |writify,writifyInner| |isFreeFunctionFromMmCond| |npEqKey|
            /UNEMBED-1 |getFirstWord| |To| |postTupleCollect|
            |getDependentsOfConstructor| |From| |tokPosn| WHOCALLED
            |postAdd| |transCategoryItem| |tokPart| SPADSYSNAMEP
            DATABASE-CONSTRUCTORMODEMAP |postReduce| |readLibPathFast|
            |traceSpad2Cmd| |postComma| |killNestedInstantiations|
            |parseBigelt| MONITOR-EVALBEFORE |setExposeAddConstr|
            |parseCases| |npAnyNo| |postSemiColon|
            DATABASE-CONSTRUCTORKIND |postWhere| |npBracketed|
            |isTraceGensym| |postColonColon| DATABASE-ABBREVIATION
            |npAngleBared| |npDDInfKey| |postColon| |npBraced|
            |domainToGenvar| |postAtSign| |npBracked| |postPretend|
            |unInstantiate| |npParened| |pfDWhereContext| |postIf|
            |pfHide?| |postJoin| |asyTypeJoinPartExport|
            |pfWDeclareDoc| |center80| |asyConstructorArg|
            |postSignature| |splitSayBrightlyArgument| |pfParts|
            |astran| |postCategory| |pfAppend| |as| |pfTLambda?|
            |postDef| |npListing| |pfSymbolVariable?| |npEqPeek|
            |asyCattran1| |pfAddBase| |postMDef| |asyConstructorArgs|
            |pfTweakIf| |pfForinLhs| |isSystemDirectory| |dqUnit|
            |asyCosig| |pfDo| |bassert| |postMapping| |asyTypeMapping|
            |pfListOf?| |dqUnitCopy| |pfLoop| |postExit| |dqConcat|
            |pfExportDef| |prove| |pfExport| |pfMLambdaArgs|
            |postTuple| |dqToList| |asyCattranOp| |pfCheckArg|
            |pfCheckId| |list2| |simpCattran| |reduceDnf| |pfId?|
            |bassertNot| |loadLibIfNotLoaded| |list1|
            |convertOpAlist2compilerInfo| |pfLocal| |loadFunctor|
            |asyType| |pfWDeclareSignature| |npWith| |pfLocalItems|
            |ordList| |loadIfNecessaryAndExists| |npTrapForm|
            |pfWhereContext| |getFunctorOpsAndAtts| NREVERSE0
            |pfDefinitionSequenceArgs| |getCategoryOpsAndAtts|
            |pfFlattenApp| |pfNot| |transformOperationAlist|
            |pfCheckItOut| |pfTaggedToTyped| |texFormat1|
            |isInitialMap| |pfId| |npElse| |outputTranReduce|
            |NRTgenInitialAttributeAlist| |exptNeedsPren|
            |getLisplibVersion| |absym| |syminusp| |compileBoot|
            |stupidIsSpadFunction| |maprin| |coerceSpadArgs2E|
            |outputTranCollect| C-TO-S S-TO-C |outputTranIf|
            |isQuotient| RGAMMA GET-BOOT-TOKEN |rgamma|
            |largeMatrixAlist| |mac0GetName|
            |getPartialConstructorModemapSig| |pfIdSymbol|
            |isDomainValuedVariable| |printMap| C-TO-R
            |isRationalNumber| |parseGreaterThan| CGAMMA |cgamma|
            |flattenOps| |CDRwithIncrement|
            |NRTgetOperationAlistFromLisplib| |parseDollarGreaterThan|
            RLNGAMMA |makeSpadConstant| |lnrgamma|
            |getOutputAbbreviatedForm| |mapConsDB|
            |augmentLowerCaseConTable| |maprin0| |mathPrintTran|
            |addConsDB| |constructSubst| |setAutoLoadProperty|
            |parseColon| |maximalSuperType| |outputMapTran|
            |NRTaddInner| |parseCoerce| |hashCount| CLNGAMMA
            |getConstructorAbbreviation| |outputTranMatrix|
            |parseAtSign| |lncgamma| |getLisplibName| |markUnique|
            |parseCategory| |orderMmCatStack| |packageForm?|
            |matSuperList| |getImmediateSuperDomain| |matSubList|
            |parseConstruct| |quoteCatOp| |matLSum| |parseDEF|
            |flattenOperationAlist| |isNameOfType| |containsVars|
            |objMode| |matLSum2| |parseExit| MAKE-DIRECTORY
            |spadTrace,g| |domainForm?| |parseHas| |clngammacase3|
            |getConstructorUnabbreviation| RECOMPILE-FILE-IF-NECESSARY
            |cgammaBernsum| |parseIn| OUR-WRITE-DATE |parseInBy|
            |upREPEAT0| RETRANSLATE-FILE-IF-NECESSARY |DNameFixEnum|
            |cgammaAdjust| |postTransformCheck| |parseIs|
            |interpOnlyREPEAT| |CompStrToString| |ncloopParse|
            |outputTranIterate| |parseIsnt| |sayMath| |containsVars1|
            |upREPEAT1| |pfPrintSrcLines| |postAtom| |parseJoin|
            |getSymbolType| |uperror| SPADTAGS-FROM-FILE
            |IdentityError| |postOp| |mkRationalFunction|
            |DNameToSExpr1| |postTranList| |parseLeave| |domainDepth|
            |postForm| |parseLET| |evalMmDom| |upREPEAT| BLANKCHARP
            |gammaRatapprox| |postCapsule| |parseMDEF| TRANSLATE
            |mathprint| |resolveTypeList| |hasDefaultPackage|
            |postError| |uphas| |gammaRatkernel| |parsePretend|
            |bright| |upiterate| |gammaStirling| |checkWarning|
            |pushSatOutput| |parseReturn| |getInfovec| |upSEQ|
            |orderCatAnc| |postBlockItem| |parseSegment| |upTuple|
            |upDEF| |mkMessage| |postBlockItemList| |upDollar|
            |intProcessSynonyms| PRINT-PACKAGE |postType| |parseWhere|
            SET-PREFIX |comma2Tuple| PRINT-RULE |startTimingProcess|
            |isDefaultPackageForm?| |postWith| |loadLib| |tuple2List|
            |getModeSetUseSubdomain| |compileInteractive|
            BUMPERRORCOUNT |getModeSet| |pp| |StringToCompStr|
            |postQUOTE| |postCollect| |stopTimingProcess| |hashCode?|
            |postDefArgs| |helpSpad2Cmd| |stepSuper| |newHelpSpad2Cmd|
            |unTuple| |matchMmCond| |inWidth| |postIteratorList|
            |getDomainByteVector| |inSub| |clearCategoryCache|
            |isValidType| |inSuper| |printMms| |bottomUpUseSubdomain|
            |systemErrorHere| |isMap| |underDomainOf| |postBlock|
            |getBasicObject| |reportHashCacheStats| |postDoubleSharp|
            |getValue| |htTrimAtBackSlash| |killColons| |getUnname|
            |updateCategoryFrameForConstructor| |displayCacheFrequency|
            |removeSuperfluousMapping| |editSpad2Cmd| |bottomUp|
            |updateCategoryFrameForCategory| |verifyRecordFile|
            |closeOldAxiomFunctor| |concatSub| |postcheck|
            |isHomogeneousList| |upCOLLECT0| |recordAndPrintTest,fn|
            |concatSuper| |setDefOp| |interpOnlyCOLLECT| |getBasicMode|
            |lisplibDoRename| |opIsHasCat| |concatbWidth| |new2OldTran|
            |loadSpad2Cmd| |upCOLLECT1| |finalizeLisplib|
            |DNameToSExpr| |newDef2Def| |concatWidth| |newIf2Cond|
            |npProcessSynonym| |newConstruct| |new2OldDefForm| PREPARSE
            |isMapExpr| |upLoopIters| |bustUnion| |isStreamCollect|
            |quoteWidth| |postElt| |mkNestedElts| |clearCmdExcept|
            |postQuote| BOOT-TOKEN-LOOKAHEAD-TYPE |objVal| |upCOLLECT|
            |addTraceItem| |stringWidth| |postSEGMENT| |getTarget|
            |iterVarPos| |isCategoryPackageName| |postSequence|
            GET-ARGUMENT-DESIGNATOR-TOKEN |upDeclare| |trace1|
            |dewritify| GET-SPADSTRING-TOKEN |isType| |upor|
            |saveMapSig| |sigmaSub| |dewritify,dewritifyInner|
            |PARSE-GliphTok| |sigmaSup| |updateTimedName|
            |isNewWorldDomain| |whatCommands| |sigmaWidth|
            |setOutputCharacters| |workfilesSpad2Cmd| |instantiate|
            |pushTimedName| |commandsForUserLevel| CACHEKEYEDMSG
            SPAD_ERROR_LOC |getMode| |sigma2Sub| |findFrameInRing|
            |pp2Cols| |sigma2Sup| XDR-STREAM-HANDLE |eq2AlgExtension|
            |getTraceOptions| |sigma2Width| |splitIntoBlocksOf200|
            XDR-STREAM-NAME |segmentedMsgPreprocess| |timedEVALFUN|
            |genDomainTraceName| |displayOperationsFromLisplib|
            |lispize| |retract| |scanCloser?| |undoLocalModemapHack|
            |intSub| |printNamedStats| XDR-STREAM-P |unwrap| |load|
            |keyword| |intSup| |timedAlgebraEvaluation| /VERSIONCHECK
            |isWrapped| |ltrace| |spadClosure?| |intWidth| |dcData1|
            |upADEF| |nopiles| |scanPossFloat| |PARSE-NBGliphTok|
            |deleteFile| |encodeCatform| |lfkey| |indefIntegralSub|
            |significantStat| |indefIntegralSup| |dcCats| |read|
            |digit?| |indefIntegralWidth| |removeAttributePredicates|
            |retractAtree| |falseFun| |pathname?|
            |removeAttributePredicates,fn| |evalLoopIter| |scanW|
            |intnplisp| |dbSpecialDisplayOpChar?| |piSub| |upCOERCE|
            |readSpad2Cmd| |piSup| |ppTemplate| |spool|
            |pathnameTypeId| |piWidth| |dcAtts| FOAM::INSERT-TYPES
            |splitListSayBrightly| |removeAttributePredicates,fnl|
            |lfstring| |dcSlots| |pi2Sub| |trace| |scanTransform|
            |unwritable?| |brightPrint| |pi2Sup| |isOkInterpMode|
            |undo| |pi2Width| |upAlgExtension| |sayBrightlyLength|
            |sayBrightlyLength1| |aggWidth| |workfiles|
            |displayParserMacro| |dcAll| |upand| |lfspaces|
            |dewritify,is?| |dcPreds| |nopilesSpad2Cmd| |string2Words|
            |interpIter| |lferror| |setNopiles| |noBlankBeforeP|
            |bitsOf| |upcase| |whatSpad2Cmd,fixpat| |tabber|
            |getExportCategory| |scanWord| |replaceGoGetSlot|
            |upconstruct| |nextline| |undoChanges| |upTARGET|
            |undoInCore| |sayLongOperation| |say2PerLineThatFit|
            |lfinteger| |mkZipCode| |initCache| |noBlankAfterP|
            |makePredicateBitVector| |rootSuper|
            |brightPrintRightJustify| |simpHasPred,eval| |rootWidth|
            |brightPrint1| |addToCategoryTable| |splitSayBrightly|
            |getCategoryExtensionAlist| |simplifyAttributeAlist|
            |addBlanks| |simpOrUnion| |operationLink| |getDomainHash|
            |mkCategoryExtensionAlist| |mkCategoryExtensionAlistBasic|
            COMP-2 |NRTcatCompare| |asyAncestorList| |letWidth| COMP-1
            |template| |isFormalArgumentList| |infovec| |slashSub|
            |blankIndicator| |predicateBitIndexRemop|
            |alqlGetKindString| |slashSuper| |compressHashTable|
            |predicateBitRef| FOAM::FOAMPROGINFOSTRUCT-P
            |alqlGetParams| |slashWidth| |clearCategoryTable|
            |dropInputLibrary| |stuffDomainSlots|
            |updateCategoryTableForCategory| COMP-NEWNAM |subSub|
            |pfSuchThat2Sex| COMP-EXPAND |pf2Sex1| |suScWidth|
            COMP-TRAN-1 |pf0TupleParts| COMP-FLUIDIZE |float2Sex|
            |COMP,FLUIDIZE| |superSubSub| PUSHLOCVAR |patternVarsOf|
            |superSubSuper| |showCategoryTable| |compAndDefine|
            |superSubWidth| |hasIdent| |pfRule2Sex| |rulePredicateTran|
            |openOutputLibrary| |ruleLhsTran| |vConcatSub|
            |simplifyMapConstructorRefs| |addInputLibrary| |pfRuleRhs|
            FOAM:|fiStrHash| |pfRhsRule2Sex| |asyExtractAbbreviation|
            |vConcatWidth| |pfRuleLhsItems| |pfLhsRule2Sex|
            |asytranLiteral| |binomialSub| COMP-TRAN |pfOp2Sex|
            |binomialSuper| |compFailure| |%id| |binomialWidth|
            |%origin| |pmDontQuote?| |simplifyMapPattern,unTrivialize|
            |%key| |pfSymbol?| |asyPredTran| FOAM:|fiGetDebugger|
            |ppos| |pfApplication2Sex| |asyPredTran1| |zagSub|
            |isPatternArgument| RECOMPILE-DIRECTORY |hasOptArgs?|
            |zagSuper| |isConstantArgument| |pfTuple?| |zagWidth|
            |pfLinePosn| |pfApplicationArg| |break| |frameName|
            |pfCharPosn| |opTran| |sayMessage| |pfImmediate?|
            |asyDocumentation| |pfNoPosition?| |pfLiteral2Sex|
            |asyExtractDescription| |timesWidth| |%pos| |inclmsgSay|
            |pfSymbolSymbol| |trimComments| |pfLiteralString|
            FOAM:|fiSetDebugVar| |exptWidth| |listOfVariables| |%fname|
            |pfLeafToken| |hackToRemoveAnd| |pfLiteralClass| |asAll|
            |exptSuper| |pfCollectArgTran| |pfCollectBody| LOG10
            |asyArgs| |stepWidth| VMLISP::LIBSTREAM-P
            |pfCollectIterators| |asyArg| |stepSub|
            |resolveTypeListAny| |pfCollect?| |asyConstructorModemap|
            |varsInPoly| |mkLineList| |pfLambda2Sex| |asyUnTuple|
            |minusWidth| |nonBlank| |pfDefinition2Sex| |asyTypeItem|
            |pfCollect2Sex| MAKE-CVEC |pfSequence2Sex| |asyCATEGORY|
            |fracsub| |pfWhereExpr| |asyCattran| |pf0WhereContext|
            |asyCatItem| |fracsuper| |domainZero| |pfIterate?|
            |asCategoryParts,exportsOf| |fracwidth| |pfReturnExpr|
            |maprinRows| |setCurrentLine| |pfReturn?| |asyIsCatForm|
            |maprinChk| |intloopEchoParse| |pfBreakFrom|
            |indentFortLevel| |ncloopPrintLines| |pfBreak?|
            |asyExportAlist| |sumWidthA| |ncloopIncFileName|
            |asMakeAlist| |pfRule?| |checkLines| |outputConstructTran|
            |asMakeAlistForFunction| |pfNovalueExpr| |asytran|
            |fortran2Lines| |putWidth| |pfNovalue?| |statement2Fortran|
            |maPrin| |ncloopEscaped| |pfNotArg| |binomSub|
            |changeExprLength| |deMatrix| |zeroOneConversion| |pfNot?|
            |mkNiladics| |binomSuper| |mkMat| |PushMatrix| |pfOrRight|
            |asyTypeJoin| |binomWidth| |concatTrouble,fixUp|
            |serverReadLine| |getAttributesFromCATEGORY| |pfOrLeft|
            |asyCosigType| |outputTranSEQ| |fortPreRoot| |height|
            |typeToInputForm| |asySubstMapping|
            FOAM::PROCESS-IMPORT-ENTRY |asyShorten| |altSuperSubSub|
            |constructor| |altSuperSubSuper|
            |exp2FortOptimizeCS1,popCsStacks| |createAbbreviation|
            |altSuperSubWidth| |incString| TRIMSTRING |boxSub|
            |isFloat| |boxSuper| |asyMkpred| |boxWidth| FOAM::TYPE2INIT
            |asyTypeJoinItem| |SpadInterpretFile| |edit| |keyp|
            FOAM:|formatDFloat| |displayDatabase| |addCommas|
            |outputTranRepeat| COMP370 |ncSetCurrentLine| |help|
            FOAM:|formatSFloat| |asyLooksLikeCatForm?| |qTWidth|
            |outputTranIteration| |phInterpret| FOAM:|formatBInt|
            |asyTypeJoinPartIf| GETREFV |intInterpretPform|
            FOAM:|formatSInt| |asyTypeJoinPartPred|
            FOAM::FOAM-FUNCTION-INFO |library| |patternCheck| |log|
            |pf2Sex| |displayLines| |zeroOneTran| |extsub| |nameLen|
            MBPIP VMLISP::SPAD-FIXED-ARG |phMacro| |formatPredParts|
            |extsuper| |exp2FortOptimizeCS| |formIterator2String|
            |extwidth| |exp2FortOptimizeCS1| |macroExpanded| QSORT
            |blankList| |matSuper| PLACEP |ncConversationPhase,wrapup|
            |form2Fence1| |matWidth| |fortExpSize| |numOfSpadArguments|
            |findSubstitutionOrder?| |PARSE-Expr| |expr2String|
            VMLISP::LIBSTREAM-INDEXSTREAM |atom2String| |PARSE-LedPart|
            |pkey| |PARSE-NudPart| |fortFormatCharacterTypes|
            |linearFormatName|
            |fortFormatCharacterTypes,mkParameterList2|
            |formatOperationAlistEntry| |overlabelSuper|
            |exp2FortOptimize| |mkParameterList| |Zeros| HKEYS
            |formTuple2String| |npListAndRecover| |overbarSuper|
            |displayLines1| |formatSignatureAsTeX|
            |fortFormatTypes,unravel| |punctuation?|
            VMLISP::LIBSTREAM-INDEXTABLE |object2Identifier|
            |incNConsoles| |checkType| |lfid| |fortPre| |npItem1|
            |Elseif?| MONITOR-DATA-COUNT FOAM:|Halt| |segment|
            |inclmsgConStill| VMLISP::LIBSTREAM-MODE |npDotted|
            |inclmsgCannotRead| |exp2Fort1| MONITOR-DELETE
            MONITOR-LIBNAME STACK-TOP |keyword?| |scanKeyTr|
            |npConditional| TOKEN-TYPE |makeFort,untangle|
            |getOpSegment| MONITOR-CHECKPOINT |LZeros| UPCASE
            TOKEN-NONBLANK |rdigit?| |makeFort,untangle2| |pfSequence|
            MONITOR-DATA-MONITORP |lineoftoks|
            |reassembleTowerIntoType| |checkForBoolean|
            MONITOR-DATA-SOURCEFILE REDUCTION-RULE |lfnegcomment|
            |pair2list| |makeCompactSigCode| |pfUnSequence|
            MONITOR-DATA-NAME |incCommand?| |lfcomment| |complexRows|
            |pf0SequenceArgs| |inclmsgPrematureFin|
            |decomposeTypeIntoTower| |vectorOfFunctions| |pfSequence?|
            |pfPile| MONITOR-NRLIB |stackTraceOptionError| |npMissing|
            |makeCompactDirect| |isListOfIdentifiers| |pfFix|
            |makeDomainTemplate| |mac0Get| MONITOR-EXPOSEDP
            |isListOfIdentifiersOrStrings| |depthAssoc| |addSpaces|
            MONITOR-APROPOS |incClassify| FBPIP |getTraceOption,hn|
            |pfFree| |depthAssocList| VMLISP::GETINDEXTABLE
            |makeOutputAsFortran| |macWhere| |isDefaultPackageName|
            MONITOR-PARSE |getCatAncestors| MONITOR-SPADFILE
            |gensymInt| |mkQuote| |pfSourceStok| |mkQuote,addQuote|
            |pfLeafPosition| MONITOR-DATA-P |npMoveTo| |pfAbSynOp|
            |predicateBitIndex| |If?| |coerceUnion2Branch| |vec2Lists|
            |pf0MLambdaArgs| |vec2Lists1| |pfMLambda?| |incBiteOff|
            |nodeSize| |pfTypedId| |asyAncestors| |formatSlotDomain|
            |pf0LambdaArgs| |npEncAp| |Top?| |vectorSize|
            |transOnlyOption| |pfLambda?| |Skipping?| KILL-COMMENTS
            |getPreviousMapSubNames| |typeToOutputForm| |pfLeaf?|
            |KeepPart?| |dcOpTable| UNEMBED |getBpiNameIfTracedMap|
            |macExpand| TOKEN-LOOKAHEAD-TYPE |untraceAllDomainLocalOps|
            |pfMLambdaBody| |npFromdom1| GET-IDENTIFIER-TOKEN
            VMLISP::GET-INDEX-TABLE-FROM-STREAM |domainOne|
            |listOfPatternIds| |pfReturnNoName| |SkipPart?|
            |orderBySubsumption| GET-NUMBER-TOKEN
            |parseAndEvalToStringForHypertex| |removeZeroOne|
            |npZeroOrMore| |SkipEnd?| GET-STRING-TOKEN
            |removeZeroOneDestructively| |dcCats1| GET-BSTRING-TOKEN
            |parseAndEvalToHypertex| |Identity| |makeCompactDirect1,fn|
            GET-SPECIAL-TOKEN |oldParseAndInterpret|
            |macSubstituteOuter| |pfIterate| MAKE-ADJUSTABLE-STRING
            |parseAndInterpret| IVECP |protectedEVAL| |mkAtree|
            |formatUnabbreviated| |getLookupFun| LIST2VEC
            |parseAndInterpToString| |pfSourcePosition| |retract1|
            |formatUnabbreviatedTuple| |pfLoop1| |numberOfNodes|
            |parseAndEvalToStringEqNum| |macApplication|
            |formatUnabbreviatedSig| |orderByContainment|
            |parseAndEvalToString| |pf0ApplicationArgs| |dcOps|
            PRINT-FLUIDS |pfApplicationOp| |retract2Specialization|
            |ncParseFromString| |stripOutNonDollarPreds|
            |pfSequenceToList|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) *) ASHARP |sayBrightlyNT| |ioHook|
            COMPILE-LIB-FILE FOAM:COMPILE-AS-FILE)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (VALUES T T)) |spadTraceAlias|
            FOAM:AXIOMXL-GLOBAL-NAME)) 
(PROCLAIM '(FTYPE (FUNCTION (T *) STRING) MAKE-FULL-CVEC)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) *) SUFFIX VMLISP::COPY-FILE
            VMLISP::COPY-LIB-DIRECTORY |spadcall2| |mkAtreeWithSrcPos|
            COMPILE-DEFUN |set1| |displaySetOptionInformation| $FCOPY
            |makeLongTimeString| |makeStream| SPAD-SAVE
            |printTypeAndTime| |printTypeAndTimeNormal| |msgText|
            ADDCLOSE MONITOR-PRINVALUE |getMinimalVarMode|
            SAYBRIGHTLYNT1 |writeInputLines| DEFSTREAM |inclHandleBug|
            |readData,xdrRead1| |print| |xdrRead|
            |formArguments2String,fn| |xdrWrite| COERCE-FAILURE-MSG
            |reportOperations| |reportOpsFromLisplib0| /TRACE-2
            |spadTrace| |handleParsedSystemCommands|
            |handleTokensizeSystemCommands| PRINMATHOR0
            |tokenSystemCommand| MONITOR-PRINT |handleNoParseCommands|
            MONITOR-PRINARGS-1 |displayProperties| /TRACELET-PRINT
            |ncEltQ| |rwriteLispForm| |evalAndRwriteLispForm|
            |isDomainConstructorForm| |isDomainForm| |output|
            |sayErrorly| |saturnSayErrorly| |sayKeyedMsg|
            |evalSlotDomain| |nrtEval| |htCommandToInputLine,fn|
            QUOTIENT |makeLongSpaceString| |ScanOrPairVec| |npsystem|
            |reportOpsFromLisplib| |ppPair| |reportOpsFromLisplib1|
            |spleI1| COMP_QUIETLY_USING_DRIVER |ncloopInclude|
            |intSayKeyedMsg| |intloopReadConsole| |ncloopCommand|
            |ncloopInclude1| |printMap1| |patternCheck,subWild|
            |ncConversationPhase| |fortError| |LAM,FILEACTQ|)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T *) T) MONITOR-ADD MATCH-NEXT-TOKEN
            |desiredMsg| PRINT-NEW-LINE MAKE-HASHTABLE MAKE-FILENAME
            MACERR |centerAndHighlight| |sayBrightlyI| BLANKS
            PRETTYPRINT |defaultTargetFE| |pfSymb| PRINT-FULL
            RDEFIOSTREAM |simpHasPred| VMLISP::MAKE-FULL-NAMESTRING
            VMREAD |LAM,EVALANDFILEACTQ| PRETTYPRIN0 |sayBrightly|
            |pfExpression| |htFile2RecordFile| |inputFile2RecordFile|
            |htFile2InputFile| MAKE-INPUT-FILENAME MATCH-CURRENT-TOKEN
            COMPSPADFILES |fillerSpaces| FINDTAG |centerNoHighlight|
            |pfSymbol| |printRecordFile| GET-BOOT-IDENTIFIER-TOKEN
            |getConstructorExports| CATCHALL TAB |F,PRINT-ONE|
            |interpret| BOOTTRAN::BOOTTOCL)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T T) (*)) MACRO-INVALIDARGS MACRO-MISSINGARGS)) 
(PROCLAIM
    '(FTYPE (FUNCTION (T T) T) |pfPushMacroBody|
            |getAliasIfTracedMapParameter| |pfCopyWithPos| |pfMacro|
            |throwEvalTypeMsg| |canCoerceTower| |getMapSig| |mkObj|
            |replaceLast| |coerceInt| |getArgValue1| |constructT|
            |canCoerceLocal| |search| |searchCurrentEnv| |incDrop|
            |coerceInt0| |getProplist| |objSetMode| |retractByFunction|
            |coerceInt1| |coerceIntAlgebraicConstant| GGREATERP
            |coerceIntTower| CGREATERP VMLISP::PUTINDEXTABLE
            |compareTypeLists| |Delay| VMLISP::WRITE-INDEXTABLE SORTBY
            |getArgValueOrThrow| |suffix?| |pairList| |incAppend|
            GETALIST |next| |match?| |member| |maskMatch?| REMALIST
            |deleteAssocWOC| |insertWOC| |erMsgCompare| |compareposns|
            |stringPrefix?| |matchMmSigTar| |stringChar2Integer|
            |getOpArgTypes| |getOpArgTypes,f| |getOpArgTypes1|
            |sublisNQ| |insert| |setMsgCatlessAttr| |rep| |quickAnd|
            |isKeyQualityP| |quickOr| |assoc| |fastSearchCurrentEnv|
            |putFlag| EFFACE |rPsiW| |insertPos| CHAR-EQ |setMsgText|
            CHAR-NE EMBED |queueUpErrors| |cpsireflect|
            |BesselIAsymptOrder| |dollarTran| |thisPosIsEqual| |horner|
            |redundant| MAKE-DATABASES |BesselKAsymptOrder|
            |thisPosIsLess| |f01| |setMsgUnforcedAttrList| |objNewCode|
            |cbeta| |PsiXotic| |setMsgForcedAttrList| |putMode|
            BVEC-MAKE-FULL LEXGREATERP |rempropI| |cgammaG|
            |dispfortarrayexp| |mkAtree1WithSrcPos| |BesselJRecur|
            |expression2Fortran1| |transferSrcPosInfo| BVEC-CONCAT
            SET-LIB-FILE-GETTER |BesselJAsymptOrder| BVEC-EQUAL
            BVEC-GREATER RPLPAIR |BesselJAsympt| |updateSymbolTable|
            BVEC-AND DELDATABASE |asyWrap| BVEC-OR
            |integerAssignment2Fortran1| BVEC-XOR |chebstareval|
            BVEC-NAND |asySignature| BVEC-NOR |asySig| HPUT*
            |asySigTarget| INITIAL-SUBSTRING-P |besselIback|
            |dispfortexpf| |objSetVal| |dispfortexpj| |asTupleNewCode|
            LENGTHENVEC |mkAtreeNodeWithSrcPos| STRING2ID-N
            |coafAndCoaf| |addDmpLikeTermsAsTarget| ESCAPE-KEYWORDS
            |ordUnion| |ordIntersection| |mkValueCheck| |andReduce|
            |mkValCheck| |orDnf| |coafOrDnf| |addDefMap|
            |EqualBarGensym| |EqualBarGensym,fn| |modemapPattern|
            |simpBoolGiven| |clearDependencies| |putDependencies|
            |makeNewDependencies| |optCallSpecially,lookup|
            |findLocalVars| |mkLocalVar| |newHasTest|
            |optimizeFunctionDef,fn| |removeVectorElt|
            |optimizeFunctionDef,replaceThrowByReturn|
            MAKE-PARSE-FUNCTION1 |andDnf| MAKE-PARSE-FUNC-FLATTEN
            MAKE-PARSE-FUNCTION $FINDFILE |AssocBarGensym|
            |optCatch,changeThrowToGo| |getMapBody| |newHasTest,fn|
            PRINT-AND-EVAL-DEFUN |defLetForm| EVAL-DEFUN |defLET2|
            |defLET1| |mkFreeVar| |addCARorCDR|
            |optCatch,changeThrowToExit| |sayDroppingFunctions|
            |defISReverse| |deleteWOC| |defIS1| |leftBindingPowerOf|
            /READ |position| |rightBindingPowerOf| |sort|
            |optCatch,hasNoThrows| QUOTIENT2 |compileBody|
            |applyWithOutputToString| |makeLocalModemap| DIVIDE2
            |saveDependentMapInfo| |makeSF| |getOption|
            |asyTypeJoinPart| |putBodyInEnv| |BooleanEquality|
            |mapDefsWithCorrectArgCount| |app2StringWrap| |hasPair|
            |lassocSub| |evalSharpOne| |coerceUn2E| |npMissingMate|
            |coerceVal2E| GLESSEQP |containedRight|
            |displaySetVariableSettings| |depthOfRecursion| DEF-LET
            LEXLESSEQP |pileCtree| |getLocalVars| |pileForest|
            |varIsOnlyVarInPoly| |coerceRe2E| |formDecl2String|
            |createEnum| |mkMapAlias| |pfForin| |EnumPrint| |pileTree|
            |printNamedStatsByProperty| |mkFormalArg| |formJoin1|
            |UnionPrint| FLAG |makePattern| |RecordPrint|
            |addPatternPred| |defIS| |objNewWrap| |sayCacheCount|
            |canMakeTuple| |deleteLassoc| |coerceByFunction|
            |deleteAssoc| |formArguments2String| |deleteAssocWOC,fn|
            |pileForest1| RDROPITEMS |insertWOC,fn| |eqpileTree|
            |mkAliasList,fn| DEF-IT |MappingPrint| SEGMENT |union|
            |putDependencies,removeObsoleteDependencies| DEF-SELECT1
            |concat1| |stringMatches?| |getEqualSublis,fn| DEF-SELECT2
            |npTypedForm1| |basicMatch?| |findLocalVars1| |defLET|
            |makeByteWordVec2| DEF-IS2 |nonRecursivePart| STRINGPAD
            |pfWrong| |interpMap| DEF-IS-REV |makeRuleForm| $REPLACE
            |assocCircular| UNIONQ |testExtend| |predCircular|
            |formalSubstitute| WHDEF |delete| |recurrenceError|
            |hasCat| AND2 |encodeCategoryAlist| OR2 |mergeOr|
            |catPairUnion,addConflict| |simpOrUnion1| TRUNCLIST
            |upIFgenValue| SUBLISNQ |evalLETchangeValue| CONTAINED
            |isPatternMatch| S+ |isPatMatch| |intersection|
            |addDomainToTable| |untraceDomainLocalOps| PREDECESSOR
            |evalLET| |evalLETput| LASSOC |rassoc| |processInteractive|
            |clearAllSlams,fn| QLASSQ DEFINE-FUNCTION PAIR S*
            ASSOCIATER |recordAndPrint| |putPvarModes| DO_LET |agg|
            NLIST PARSEPILES |interpretTopLevel| |processInteractive1|
            ESCAPED |genIFvalCode| -REDUCE-OP DELASC INITIAL-SUBSTRING
            |seteltable| STOREBLANKS |prnd| |printTypeAndTimeSaturn|
            ADD-PARENS-AND-SEMIS-TO-LINE |matSuperList1|
            |substituteSegmentedMsg| TAKE DROP |compileIs| |opWidth|
            TRUNCLIST-1 |sameUnionBranch| |scylla| SUBB |CONTAINED,EQ|
            |CONTAINED,EQUAL| |canCoerce;| DELLASOS |matSubList1|
            |canCoerce1| ?ORDER |pfTLam| |HasSignature| SAYBRIGHTLY1
            |pfCoerceto| |pfBraceBar| |everyNth| |isEqualOrSubDomain|
            |queryUserKeyedMsg| |hasCorrectTarget| |resolveTT;|
            |makePathname| |resolveTT1| SETDIFFERENCEQ |pfMLambda|
            |formatJoinKey| |app2StringConcat0| |pfDWhere| |plural|
            |findSubstitutionOrder?,fn| PRINT-DEFUN |ofCategory|
            |formatOpSignature| |pfWDeclare| |linearFormatForm|
            SPADRREAD |canCoerceFrom;| |canCoerceFrom0|
            |filterListOfStrings| |inclHandleSay|
            |satisfiesRegularExpressions| |inclHandleWarning|
            |evalCategory| |pfParen| |inclHandleError| |pfBracketBar|
            |replaceSharps| INTERSECTIONQ /GETOPTION |isPointer?|
            |substInOrder| |pfComDefinition| SETANDFILE |pfBracket|
            |wt| STACK-LOAD |incRenumberLine| |pfRetractTo|
            |incRenumberItem| PAIRTRACE BPIUNTRACE STACK-PUSH
            |getFortranType| |getConditionsForCategoryOnType| |wl|
            |isString?| |pfRestrict| |coerceInteractive|
            |formatOperation| |showInput| |formatOpSymbol| |showInOut|
            PUSH-REDUCTION |pfOr| |getArgValue| |writeData|
            |untraceDomainConstructor,keepTraced?|
            |sayModemapWithNumber| |xdrOpen| |pushDownOp?| |testPrin|
            |resolveTMEq1| |resolveTMSpecial| |testInput2Output|
            |makeVector| |pfAnd| |isTowerWithSubdomain|
            |ScanOrPairVec,ScanOrInner| |makeList| |constructM|
            |pfTagged| |readData| |functionAndJacobian,DF|
            |constructTowerT| |resolveTCat| |mergePathnames| |after|
            |remHashEntriesWith0Count,fn| |resolveTTAny| |hasOption|
            |resolveTM1| FOAM:|printDFloat| |resolveTTEq| |resolveTTCC|
            FOAM:|printSFloat| |resolveTTRed| /TRACE-1 FOAM:|printBInt|
            |diffAlist| |listTruncate| FOAM:|printSInt|
            |resolveTTSpecial| FOAM:|printString| |deepSubCopy|
            |resolveTCat1| FOAM:|printChar| |intCodeGenCOERCE|
            |compareTT| |traceDomainConstructor|
            |acceptableTypesToResolve| |canCoerceByMap|
            /GETTRACEOPTIONS |rightJustifyString|
            |acceptableTypesToResolve1| |commandErrorIfAmbiguous|
            |canCoerceByFunction| |resolveTTUnion| |tracelet| MARKHASH
            |isSubDomain| |resolveTM2| |absolutelyCanCoerceByCheating|
            |resolveTMRed| |term1RWall| |term1RW| |resolveTMEq|
            |rassocSub| |getValueFromEnvironment| |resolveTMRecord|
            |parseTranCheckForRecord| |addToSlam| CARCDREXPAND
            |mkObjCode| |getI| |resolveTMUnion| |setMsgPrefix|
            |lassocShiftQ| |mkAtree3,fn| |resolveTMTaggedUnion|
            |globalHashtableStats| |spliceTypeListForEmptyMode|
            /UNTRACE-1 |domainEqualList| |resolveTMOrCroak| REPEAT-TRAN
            -REPEAT |allLASSOCs| |putFTText| MKPF
            |mkAlistOfExplicitCategoryOps,fn| FLAGP |spadUntrace|
            |putValueValue| |npRightAssoc| |parseTypeEvaluateArgs|
            MKPF1 |coerceOrRetract| |getSystemModemaps| MKPFFLATTEN
            MONITOR-EVALTRAN DAASENAME S- |getAndSay| MONITOR-EVALTRAN1
            |pfApplication| TRANSLABEL |isDomainSubst,fn|
            MONITOR-GETVALUE |undoSteps| |getAllModemapsFromDatabase|
            |undoSingleStep| |getModemapsFromDatabase| |pfExit|
            |collectDefTypesAndPreds,addPred| |deleteAll| WRAPDOMARGS
            |reportSpadTrace| |declare| |parseCases,casefn|
            GETCONSTRUCTOR |sameMsg?| |spadTrace,isTraceable|
            |declareMap| |insertModemap| |listDecideHowMuch| |pfWDec|
            |upfreeWithType| |removeOption| |mkLessOrEqual|
            |uplocalWithType| |asyTypeUnitDeclare| |decideHowMuch|
            |asTupleNew| |asyCatSignature| |isDomainSubst,findSub|
            |pfWhere| |asCategoryParts,build| |getMsgCatAttr| /EMBED-1
            /EMBED-Q |FromTo| REMOVER MAKEOP MAKENEWOP |subTypes|
            |installConstructor| |traceOptionError|
            |isNestedInstantiation| |loadLibIfNecessary| |mkAutoLoad|
            ADDOPERATIONS |keyedSystemError1| |readLib| |breakKeyedMsg|
            ASHARPMKAUTOLOADFUNCTION |getLisplib| |center| |funfind|
            |saturnKeyedSystemError| |writeLib| |sayKeyedMsgAsTeX|
            |npLeftAssoc| |pfPretend| |pfTree| |autoLoad|
            |asyCattranConstructors| |systemDependentMkAutoload|
            |coafAndDnf| |npTypedForm|
            |convertOpAlist2compilerInfo,formatSig| |dqAddAppend|
            |dnfContains,fn| |pfTyped| |dnfContains| |asySimpPred|
            |dqAppend| |asyCattranSig| |pfFromdom| |pfBrace|
            |asyMkSignature| |pfIdPos| |ordSetDiff| |orDel|
            |getLisplibNoCache| NREVERSE-N CONS-N
            |getSlotFromCategoryForm| APPEND-N |pfDefinition| |pfSuch|
            |pfIfThenOnly| |pfHide| |termRW| |has| |matWList1|
            |termRW1| |subCopy0| |subCopyOrNil| CPSI |deepSubCopy0|
            |unabbrevRecordComponent| |cPsi| |unabbrev1|
            |deepSubCopyOrNil| |getCDTEntry| CHYPER0F1 |spadPrint|
            |macSubstituteId| |chebf01| RBESSELJ |BesselJ| CBESSELJ
            |unabbrevUnionComponent| |checkArgs| |lassocShift|
            |NRTdescendCodeTran| RBESSELI |BesselI| RPSI |mkSuperSub|
            |subCopy| |rPsi| |mergeSubs| CBESSELI |SExprToDName|
            |condAbbrev| |matWList| |oldAxiomCategoryDevaluate|
            |attributeNthParent| |mathPrint1| |domainEqual|
            |oldAxiomPreCategoryParents| |mmCatComp|
            SPADTAGS-FROM-DIRECTORY |PsiEps| |pfAbSynOp?|
            |lazyOldAxiomDomainDevaluate| /UNTRACE-2 |sayErrorly1|
            |outputMapTran0| |upLispCall|
            |oldAxiomCategoryDefaultPackage| |FloatError|
            |setBootAutoLoadProperty| |hasCatExpression|
            |oldAxiomPreCategoryDevaluate| |mkBootAutoLoad|
            |orderedDefaults| |chebeval| |PsiAsymptotic|
            |errorSupervisor| |sayLooking1| |postFlatten|
            |attributeHashCode| |newHasCategory| |brutef01|
            |oldAxiomDomainDevaluate| |postTranSegment|
            |defaultTypeForCategory| |keyedSystemError|
            |oldAxiomPreCategoryHashCode| |argCouldBelongToSubdomain|
            |hashTypeForm| |oldAxiomCategoryHashCode|
            |attributeDevaluate| |oldAxiomCategoryParentCount|
            GETDATABASE |setBootAutloadProperties| |rePackageTran|
            |throwKeyedMsg| |commandError| |CONTAINEDisDomain|
            |putModeSet| |postFlattenLeft| |putValue|
            |reportCircularCacheStats| |hasCaty1|
            |mkCircularCountAlist| |unloadOneConstructor|
            |getBasicMode0| |npsynonym| |bottomUpIdentifier|
            |optionError| |bottomUpType| |hyperize|
            |lazyOldAxiomDomainHashCode| LET_ERROR
            |optionUserLevelError| |resolveTM| |oldAxiomDomainHashCode|
            |PARSE-rightBindingPowerOf| |augmentTraceNames|
            |transferPropsToNode| |HasCategory| FOAM::|magicEq1|
            GET-GLIPH-TOKEN |getMinimalVariableTower|
            |checkForFreeVariables| TRANSLABEL1 |positionInVec|
            FOAM-USER::H-ERROR |keyedMsgCompFailure| |objNew|
            |PARSE-Operation| |PARSE-leftBindingPowerOf|
            |testBitVector| |sayIntelligentMessageAboutOpAvailability|
            |wordFrom| |replaceSymbols| |modemapsHavingTarget|
            |initializeTimedNames| |computeTypeWithVariablesTarget|
            |getKeyedMsgInDb| |makePrefixForm| |putTarget|
            |scanExponent| |sayKeyedMsgLocal| FOAM-USER::H-STRING
            |sayPatternMsg| |getAtree| |canFit2ndEntry|
            |scanIgnoreLine| |bottomUpCompilePredicate|
            |dcOpLatchPrint| |bottomUpPredicate| FOAM-USER::H-INTEGER
            |posend| |mkIterVarSub| |scanInsert| |throwKeyedMsg1|
            |saturnThrowKeyedMsg| |makeCompactDirect1|
            |say2PerLineWidth| |lfrinteger| |commandUserLevelError|
            |scanCheckRadix| |countCircularAlist| |clearCategoryTable1|
            FOAM::ALLOC-PROG-INFO |buildBitTable,fn| |hashType|
            |isKeyedMsgInDb| |hashCombine| |augmentPredVector|
            |splitListOn| |updateCategoryTableForDomain|
            |throwPatternMsg| NOTEQUALLIBS |updateCategoryTable|
            |mkCategoryOr| |simpCategoryOr| |tempExtendsCat|
            |categoryParts,build| |patternVarsOf1| |nonRecursivePart1|
            POSN1 |notCalled| |containsOp| |pvarPredTran|
            |expandRecursiveBody| |symEqual| |simplifyMapPattern|
            SETDIFFERENCE FOAM:|fiSetDebugger| |breaklet| |SymMemQ|
            |outputFormat| |displayRule| |inclmsgFileCycle|
            |AlistAssocQ| |asyDocumentation,fn| |ListMemberQ?|
            |ListMember?| |AlistRemoveQ| |intloopInclude|
            |canConvertByFunction| |coerceIntByMapInner|
            |ncloopDQlines| |asyMapping| |isSubTowerOf| |streamChop|
            ERROR-FORMAT |valueArgsEqual?| |getBindingPowerOf|
            |phReportMsgs| |coerceIntByMap| |processMsgList|
            |constantInDomain?| |coerceIntTableOrFunction|
            |asyExportAlist,fn| |coerceIntSpecial| DIVIDE
            |coerceCommuteTest| |intloopProcessString| |asyDisplay|
            |coerceIntPermute| GETL |coerceOrConvertOrRetract|
            |intloopPrefix?| |clearDependentMaps| |intloopInclude1|
            |asyAbbreviation,chk| |SubstWhileDesizingList| |mkObjWrap|
            |asyAbbreviation| |asySplit| |SubstWhileDesizing|
            |fortranifyFunctionName| |equalOne| FOAM:|fputs|
            |fortranifyIntrinsicFunctionName| |ncloopPrefix?|
            FOAM:|fputc| |asyGetAbbrevFromComments,fn|
            |displayDatabase,fn| |patternCheck,wild| |ncINTERPFILE|
            |assignment2Fortran1| |patternCheck,pos| |logicalMatch?|
            |superMatch?| |translateMpVars2PVars| |prefix?|
            |fortFormatIfGoto| |phIntReportMsgs| |fortFormatTypes1|
            |position1| |beenHere| |exp2FortOptimizeCS1,pushCsStacks|
            |phParse| |segment2| |displayOpModemaps| |getStatement|
            |sortAndReorderDmpExponents| |NRTreplaceLocalTypes|
            |substDomainArgs| |fortFormatTypes| |incActive?| |segment1|
            |dcOpPrint| |subMatch| |computeTTTranspositions|
            FOAM:|PtrMagicEQ| |makeGoGetSlot| MONITOR-WRITE
            |lnSetGlobalNum| |canCoerceCommute| |removeListElt| NCONC2
            |macWhere,mac| |incTrunc| |sySpecificErrorHere|
            |coerceInt2Union| |macLambdaParameterHandling| |ifCond|
            |pfAssign| |coerceBranch2Union| |pfQualType|
            |incCommandTail| |canCoerceExplicit2Mapping| |pfSpread|
            |coerceIntFromUnion| |pfReturnTyped| |incStream|
            |predicateBitIndex,pn| |syIgnoredFromTo| |equalZero|
            |pfLam| |getConstantFromDomain| |pfCollect|
            |absolutelyCannotCoerce| |pfRule| |typeToForm|
            |inFirstNotSecond| |pfFromDom| |augmentPredCode|
            |coerceIntTest| |mungeAddGensyms| |canCoerceUnion|
            |macLambda,mac| |pfReturn| REMAINDER |pfMapParts|
            |canCoercePermute| |mac0SubstituteOuter| |assertCond|
            |newCanCoerceCommute| |coerceIntCommute| |coerceRetract|
            |pfLp| |sublisNQ,fn| |inclFname| |genMpFromDmpTerm|)) 
(PROCLAIM '(FTYPE (FUNCTION NIL FIXNUM) HEAPELAPSED)) 
(PROCLAIM
    '(FTYPE (FUNCTION NIL (VALUES T T)) GENVAR MAKE-CLOSEDFN-NAME))
)
