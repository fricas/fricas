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

reportFunctionCompilation(op,nam,argl,body,isRecursive) ==
  -- for an alternate definition of this function which does not allow
  -- dynamic caching, see SLAMOLD BOOT
--+
  $compiledOpNameList := [nam]
  minivectorName := makeInternalMapMinivectorName(nam)
  $minivectorNames := [[op,:minivectorName],:$minivectorNames]
  body := substitute(minivectorName, "$$$", body)
  if $compilingInputFile then
    $minivectorCode := [:$minivectorCode,minivectorName]
  SET(minivectorName,LIST2REFVEC $minivector)
  argl := COPY argl     -- play it safe for optimization
  init :=
    not(isRecursive and $compileRecurrence and #argl = 1) => nil
    NRTisRecurrenceRelation(nam,body,minivectorName)
  init => compileRecurrenceRelation(op,nam,argl,body,init)
  cacheCount:= getCacheCount op
  cacheCount = "all" => reportFunctionCacheAll(op,nam,argl,body)
  cacheCount = 0 or null argl =>
    function:= [nam,['LAMBDA,[:argl,'envArg],body]]
    compileInteractive function
    nam
  num :=
    FIXP cacheCount =>
      cacheCount < 1 =>
        keyedSystemError("S2IM0019",[cacheCount,op])
      cacheCount
    keyedSystemError("S2IM0019",[cacheCount,op])
  sayKeyedMsg("S2IX0003",[op,num])
  auxfn := mkAuxiliaryName nam
  g1:= GENSYM()  --argument or argument list
  [arg,computeValue] :=
    null argl => [nil,[auxfn]]
    argl is [.] => [[g1, 'envArg],[auxfn,g1, 'envArg]]  --g1 is a parameter
    [g1, ['APPLY, MKQ auxfn, g1]]          --g1 is a parameter list
  cacheName := mkCacheName nam
  g2:= GENSYM()  --length of cache or arg-value pair
  g3:= GENSYM()  --value computed by calling function
  secondPredPair:=
    null argl => [cacheName]
    [['SETQ,g3,['assocCircular,g1,cacheName]],['CDR,g3]]
  thirdPredPair:=
    null argl => ['(QUOTE T),['SETQ,cacheName,computeValue]]
    ['(QUOTE T),
      ['SETQ,g2,computeValue],
        ['SETQ,g3,
            ['CAR,['SETQ,cacheName,['predCircular,cacheName,cacheCount]]]],
          ['RPLACA,g3,g1],
            ['RPLACD,g3,g2],
              g2]
  codeBody:=
    ['PROG,[g2,g3],['RETURN,['COND,secondPredPair,thirdPredPair]]]
  -- cannot use envArg in next statement without redoing much
  -- of above.
  lamex:= ['LAMBDA, arg, codeBody]
  mainFunction:= [nam,lamex]
  computeFunction:= [auxfn,['LAMBDA,[:argl, 'envArg],body]]
  compileInteractive mainFunction
  compileInteractive computeFunction
  cacheType:= 'function
  cacheResetCode:= ['SETQ,cacheName,['mkCircularAlist,cacheCount]]
  cacheCountCode:= ['countCircularAlist,cacheName,cacheCount]
  cacheVector:=
    mkCacheVec(op,cacheName,cacheType,cacheResetCode,cacheCountCode)
  $e:= put(nam,'cacheInfo, cacheVector,$e)
  eval cacheResetCode
  SET(cacheName,mkCircularAlist cacheCount)
  nam

getCacheCount fn ==
  n:= LASSOC(fn,$cacheAlist) => n
  $cacheCount

reportFunctionCacheAll(op,nam,argl,body) ==
  sayKeyedMsg("S2IX0004",[op])
  auxfn:= mkAuxiliaryName nam
  g1:= GENSYM()  --argument or argument list
  [arg,computeValue] :=
    null argl => [['envArg],[auxfn, 'envArg]]
    argl is [.] => [[g1, 'envArg],[auxfn,g1, 'envArg]]  --g1 is a parameter
    [g1, ['APPLY, MKQ auxfn, g1]]          --g1 is a parameter list
  if null argl then g1:=nil
  cacheName:= mkCacheName nam
  g2:= GENSYM()  --value computed by calling function
  secondPredPair:= [['SETQ,g2,['HGET,cacheName,g1]],g2]
  thirdPredPair:= ['(QUOTE T),['HPUT,cacheName,g1,computeValue]]
  codeBody:= ['PROG,[g2],['RETURN,['COND,secondPredPair,thirdPredPair]]]
  lamex:= ['LAMBDA, arg, codeBody]
  mainFunction:= [nam,lamex]
  computeFunction:= [auxfn,['LAMBDA,[:argl, 'envArg],body]]
  compileInteractive mainFunction
  compileInteractive computeFunction
  cacheType:= 'hash_-table
  cacheResetCode:= ['SETQ,cacheName,['MAKE_-HASHTABLE,''UEQUAL]]
  cacheCountCode:= ['hashCount,cacheName]
  cacheVector:=
    mkCacheVec(op,cacheName,cacheType,cacheResetCode,cacheCountCode)
  $e:= put(nam,'cacheInfo, cacheVector,$e)
  eval cacheResetCode
  nam

hashCount table ==
  +/[ADD1 nodeCount HGET(table,key) for key in HKEYS table]

mkCircularAlist n ==
  l:= [[$failed,:$failed] for i in 1..n]
  RPLACD(LASTNODE l,l)

countCircularAlist(cal,n) ==
  +/[nodeCount x for x in cal for i in 1..n]

predCircular(al,n) ==
  for i in 1..QSSUB1 n repeat al:= QCDR al
  al

assocCircular(x,al) ==  --like ASSOC except that al is circular
  forwardPointer:= al
  val:= nil
  until EQ(forwardPointer,al) repeat
    EQUAL(CAAR forwardPointer,x) => return (val:= CAR forwardPointer)
    forwardPointer:= CDR forwardPointer
  val

compileRecurrenceRelation(op,nam,argl,junk,[body,sharpArg,n,:initCode]) ==
  k:= #initCode
  extraArgumentCode :=
    extraArguments := [x for x in argl | x ~= sharpArg] =>
      extraArguments is [x] => x
      ['LIST,:extraArguments]
    nil
  g:= GENSYM()
  gIndex:= GENSYM()
  gsList:= [GENSYM() for x in initCode]
  auxfn := mkAuxiliaryName(nam)
  $compiledOpNameList := [:$compiledOpNameList,auxfn]
  stateNam:= GENVAR()
  stateVar:= GENSYM()
  stateVal:= GENSYM()
  lastArg := INTERNL STRCONC('"#",STRINGIMAGE QSADD1 LENGTH argl)
  decomposeCode:=
    [['LET,gIndex,['ELT,lastArg,0]],:[['LET,g,['ELT,lastArg,i]]
      for g in gsList for i in 1..]]
  gsRev:= REVERSE gsList
  rotateCode:= [['LET,p,q] for p in gsRev for q in [:rest gsRev,g]]
  advanceCode:= ['LET,gIndex,['ADD1,gIndex]]

  newTripleCode := ['LIST,sharpArg,:gsList]
  newStateCode :=
    null extraArguments => ['SETQ,stateNam,newTripleCode]
    ['HPUT,stateNam,extraArgumentCode,newTripleCode]

  computeFunction:= [auxfn,['LAMBDA, cargl, cbody]] where
    cargl:= [:argl,lastArg]
    returnValue:= ['PROGN,newStateCode,first gsList]
    cbody:=
      endTest:=
        ['COND, [['EQL,sharpArg,gIndex],['RETURN,returnValue]]]
      newValueCode := ['LET, g, substitute(gIndex, sharpArg,
        EQSUBSTLIST(gsList,rest $TriangleVariableList,body))]
      ['PROGN,:decomposeCode,
        ['REPEAT,['WHILE,'T],['PROGN,endTest,advanceCode,
          newValueCode,:rotateCode]]]
  fromScratchInit:=
    [['LET,gIndex,n],:[['LET,g,x] for g in gsList for x in initCode]]
  continueInit:=
    [['LET,gIndex,['ELT,stateVar,0]],
      :[['LET,g,['ELT,stateVar,i]] for g in gsList for i in 1..]]
  mainFunction:= [nam,['LAMBDA, margl, mbody]] where
    margl:= [:argl,'envArg]
    max:= GENSYM()
    tripleCode := ['CONS,n,['LIST,:initCode]]

    -- initialSetCode initializes the global variable if necessary and
    --  also binds "stateVar" to its current value
    initialSetCode :=
      initialValueCode :=
        extraArguments => ['MAKE_-HASHTABLE,''UEQUAL]
        tripleCode
      cacheResetCode := ['SETQ,stateNam,initialValueCode]
      ['COND,[['NULL,['AND,['BOUNDP,MKQ stateNam], _
                          ['PAIRP,stateNam]]],    _
                 ['LET,stateVar,cacheResetCode]], _
             [''T, ['LET,stateVar,stateNam]]]

    -- when there are extra arguments, initialResetCode resets "stateVar"
    --  to the hashtable entry for the extra arguments
    initialResetCode :=
      null extraArguments => nil
      [['LET,stateVar,['OR,
         ['HGET,stateVar,extraArgumentCode],
          ['HPUT,stateVar,extraArgumentCode,tripleCode]]]]

    mbody :=
      preset := [initialSetCode,:initialResetCode,['LET,max,['ELT,stateVar,0]]]
      phrase1:= [['AND, ['LET, max, ['ELT, stateVar, 0]],
                      [">=", sharpArg, max]], [auxfn,:argl,stateVar]]
      phrase2:= [[">", sharpArg, ['SETQ, max, ['DIFFERENCE, max, k]]],
                  ['ELT,stateVar,['QSADD1,['QSDIFFERENCE,k,['DIFFERENCE,sharpArg,max]]]]]
      phrase3:= [[">", sharpArg, n], [auxfn, :argl, ['LIST, n, :initCode]]]
      phrase4:= [[">", sharpArg, n - k],
        ['ELT,['LIST,:initCode],['QSDIFFERENCE,n,sharpArg]]]
      phrase5:= ['(QUOTE T),['recurrenceError,MKQ op,sharpArg]]
      ['PROGN,:preset,['COND,phrase1,phrase2,phrase3,phrase4,phrase5]]
  sayKeyedMsg("S2IX0001",[op])
  compileInteractive computeFunction
  compileInteractive mainFunction
  cacheType:= 'recurrence
  cacheCountCode:= ['nodeCount,stateNam]
  cacheVector:= mkCacheVec(op,stateNam,cacheType,cacheResetCode,cacheCountCode)
  $e:= put(nam,'cacheInfo, cacheVector,$e)
  nam

nodeCount x == NUMOFNODES x

recurrenceError(op,arg) == throwKeyedMsg("S2IX0002",[op,arg])

mkCacheVec(op,nam,kind,resetCode,countCode) ==
  [op,nam,kind,resetCode,countCode]

-- reportCacheStore vl ==
--   sayMSG concat(centerString('"Name",22,'" "),"   Kind          #Cells")
--   sayMSG concat(centerString('"----",22,'" "),"   ----          ------")
--   for x in vl repeat reportCacheStoreFor x
--
-- op2String op ==
--   u:= linearFormatName op
--   atom u => PNAME u
--   "STRCONC"/u
--
-- reportCacheStorePrint(op,kind,count) ==
--   ops:= op2String op
--   opString:= centerString(ops,22,'" ")
--   kindString:= centerString(PNAME kind,10,'" ")
--   countString:= centerString(count,19,'" ")
--   sayMSG concat(opString,kindString,countString)
--
-- reportCacheStoreFor op ==
--   u:= getI(op,'localModemap) =>
--     for [['local,target,:.],[.,fn],:.] in u repeat
--       [op1,cacheName,kind,.,countCode]:= getI(fn,'cacheInfo) or
--         keyedSystemError("S2GE0016",['"reportCacheStoreFor",
--           '"missing cache information vector"])
--       reportCacheStorePrint(op,kind,eval countCode)
--     true
--   u:= getI(op,"cache") =>
--     reportCacheStorePrint(op,'variable,nodeCount u)
--   nil

clearCache x ==
  get(x,'localModemap,$e) or get(x,'mapBody,$e) =>
    for [map,:sub] in $mapSubNameAlist repeat
      map=x => _/UNTRACE_-2(sub,NIL)
    $e:= putHist(x,'localModemap,nil,$e)
    $e:= putHist(x,'mapBody,nil,$e)
    $e:= putHist(x,'localVars,nil,$e)
    sayKeyedMsg("S2IX0007",[x])

compileInteractive fn ==
  if $InteractiveMode then startTimingProcess 'compilation
  if $reportCompilation then
    sayBrightlyI bright '"Generated LISP code for function:"
    pp fn
  optfn :=
     $InteractiveMode => [timedOptimization fn]
     [fn]
  result := compQuietly optfn
  if $InteractiveMode then stopTimingProcess 'compilation
  result

clearAllSlams x ==
  fn(x,nil) where
    fn(thoseToClear,thoseCleared) ==
      for x in thoseToClear | not MEMQ(x,thoseCleared) repeat
        slamListName:= mkCacheName x
        SET(slamListName,nil)
        thoseCleared:= ADJOIN(x,thoseCleared)
        someMoreToClear:=
          setDifference(LASSOC(x,$functorDependencyAlist),[:thoseToClear,:
            thoseCleared])
        NCONC(thoseToClear,someMoreToClear)

clearSlam(functor)==
  id:= mkCacheName functor
  SET(id,nil)
