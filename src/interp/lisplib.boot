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

--% Standard Library Creation Functions

readLib(fn, ft) ==
  -- see if it exists first
  p := pathname [fn, ft]
  rMkIstream(p)

writeLib(fn, ft) == rMkOstream([fn, ft])

lisplibWrite(prop,val,filename) ==
  -- this may someday not write NIL keys, but it will now
  if $LISPLIB then
     rwrite(prop,val,filename)


evalAndRwriteLispForm(key,form) ==
  eval form
  rwriteLispForm(key,form)

rwriteLispForm(key,form) ==
  if $LISPLIB then
    rwrite( key,form,$libFile)
    output_lisp_form(form)

--% Uninstantiating

unInstantiate(clist) ==
  for c in clist repeat
    clearConstructorCache(c)
  killNestedInstantiations(clist)

killNestedInstantiations(deps) ==
  for key in HKEYS($ConstructorCache)
    repeat
      for [arg,count,:inst] in HGET($ConstructorCache,key) repeat
        isNestedInstantiation(inst.0,deps) =>
          HREMPROP($ConstructorCache,key,arg)

isNestedInstantiation(form,deps) ==
  form is [op,:argl] =>
    op in deps => true
    or/[isNestedInstantiation(x,deps) for x in argl]
  false

--% Loading

loadLibIfNotLoaded libName ==
  -- replaces old SpadCondLoad
  -- loads is library is not already loaded
  GET(libName, 'LOADED) => NIL
  loadLib libName

loadLib cname ==
  startTimingProcess 'load
  fullLibName := GETDATABASE(cname,'OBJECT) or return nil
  systemdir? := isSystemDirectory(pathnameDirectory fullLibName)
  update? := $forceDatabaseUpdate or not systemdir?
  not update? =>
     loadLibNoUpdate(cname, cname, fullLibName)
  kind := GETDATABASE(cname,'CONSTRUCTORKIND)
  if $printLoadMsgs then
    sayKeyedMsg("S2IL0002",[namestring fullLibName,kind,cname])
  load_quietly(fullLibName)
  clearConstructorCache cname
  updateDatabase(cname,cname,systemdir?)
  installConstructor(cname,kind)
  u := GETDATABASE(cname, 'CONSTRUCTORMODEMAP)
  updateCategoryTable(cname,kind)
  coSig :=
      u =>
          [[.,:sig],:.] := u
          CONS(NIL, [categoryForm?(x) for x in rest sig])
      NIL
  -- in following, add property value false or NIL to possibly clear
  -- old value
  if null rest GETDATABASE(cname, 'CONSTRUCTORFORM) then
      MAKEPROP(cname,'NILADIC,'T)
    else
      REMPROP(cname,'NILADIC)
  MAKEPROP(cname,'LOADED,fullLibName)
  if $InteractiveMode then $CategoryFrame := [[nil]]
  stopTimingProcess 'load
  'T

loadLibNoUpdate(cname, libName, fullLibName) ==
  kind := GETDATABASE(cname,'CONSTRUCTORKIND)
  if $printLoadMsgs then
    sayKeyedMsg("S2IL0002",[namestring fullLibName,kind,cname])
  if CATCH('VERSIONCHECK, load_quietly(fullLibName)) = -1
    then
      PRINC('"   wrong library version...recompile ")
      PRINC(fullLibName)
      TERPRI()
      TOPLEVEL()
    else
     clearConstructorCache cname
     installConstructor(cname,kind)
     MAKEPROP(cname,'LOADED,fullLibName)
     if $InteractiveMode then $CategoryFrame := [[nil]]
     stopTimingProcess 'load
  'T

loadIfNecessary u == loadLibIfNecessary(u,true)

loadIfNecessaryAndExists u == loadLibIfNecessary(u,nil)

loadLibIfNecessary(u,mustExist) ==
  u = '$EmptyMode => u
  null atom u => loadLibIfNecessary(first u,mustExist)
  value:=
    functionp(u) or macrop(u) => u
    GET(u, 'LOADED) => u
    loadLib u => u
  null $InteractiveMode and ((null (y:= getProplist(u,$CategoryFrame)))
    or (null LASSOC('isFunctor,y)) and (null LASSOC('isCategory,y))) =>
      y:= GETDATABASE(u,'CONSTRUCTORKIND) =>
         y = 'category =>
            updateCategoryFrameForCategory u
         updateCategoryFrameForConstructor u
      throwKeyedMsg("S2IL0005",[u])
  value

convertOpAlist2compilerInfo(opalist) ==
   "append"/[[formatSig(op,sig) for sig in siglist]
                for [op,:siglist] in opalist] where
      formatSig(op, [typelist, slot,:stuff]) ==
          pred := if stuff then first stuff else 'T
          impl := if rest stuff then CADR stuff else 'ELT -- handles 'CONST
          [[op, typelist], pred, [impl, '$, slot]]

updateCategoryFrameForConstructor(constructor) ==
   opAlist := GETDATABASE(constructor, 'OPERATIONALIST)
   [[dc,:sig],[pred,impl]] := GETDATABASE(constructor, 'CONSTRUCTORMODEMAP)
   $CategoryFrame := put(constructor,'isFunctor,
       convertOpAlist2compilerInfo(opAlist),
       addModemap(constructor, dc, sig, pred, impl,
           put(constructor, 'mode, ['Mapping,:sig], $CategoryFrame)))

updateCategoryFrameForCategory(category) ==
   di := GETDATABASE(category, 'CONSTRUCTORMODEMAP)
   if di then
       [[dc,:sig],[pred,impl]] := di
       $CategoryFrame :=
           addModemap(category, dc, sig, pred, impl, $CategoryFrame)
   $CategoryFrame := put(category, 'isCategory, 'T, $CategoryFrame)

loadFunctor u ==
  null atom u => loadFunctor first u
  loadLibIfNotLoaded u
  u

makeConstructorsAutoLoad() ==
  for cnam in allConstructors() repeat
    REMPROP(cnam,'LOADED)
    if GETDATABASE(cnam,'NILADIC)
     then PUT(cnam,'NILADIC,'T)
     else REMPROP(cnam,'NILADIC)
    systemDependentMkAutoload(cnam,cnam)

systemDependentMkAutoload(fn,cnam) ==
    FBOUNDP(cnam) => "next"
    asharpName := GETDATABASE(cnam, 'ASHARP?) =>
         kind := GETDATABASE(cnam, 'CONSTRUCTORKIND)
         cosig := GETDATABASE(cnam, 'COSIG)
         file := GETDATABASE(cnam, 'OBJECT)
         SET_-LIB_-FILE_-GETTER(file, cnam)
         kind = 'category =>
              ASHARPMKAUTOLOADCATEGORY(file, cnam, asharpName, cosig)
         ASHARPMKAUTOLOADFUNCTOR(file, cnam, asharpName, cosig)
    SETF(SYMBOL_-FUNCTION cnam,mkAutoLoad(fn, cnam))

autoLoad(abb,cname) ==
  if not GET(cname, 'LOADED) then
      FMAKUNBOUND cname
      loadLib cname
  SYMBOL_-FUNCTION cname

setAutoLoadProperty(name) ==
  REMPROP(name,'LOADED)
  SETF(SYMBOL_-FUNCTION name,mkAutoLoad(name, name))

unloadOneConstructor(cnam,fn) ==
    REMPROP(cnam,'LOADED)
    SETF(SYMBOL_-FUNCTION cnam,mkAutoLoad(fn, cnam))

--% Compilation

compDefineLisplib(df:=["DEF",[op,:.],:.],m,e,prefix,fal,fn) ==
  --fn= compDefineCategory OR compDefineFunctor
  sayMSG fillerSpaces(72,'"-")
  $LISPLIB: local := 'T
  $op: local := op
  $lisplibPredicates: local := NIL -- set by makePredicateBitVector
  $lisplibForm: local := NIL
  $lisplibKind: local := NIL
  $lisplibAbbreviation: local := NIL
  $lisplibParents: local := NIL
  $lisplibAncestors: local := NIL
  $lisplibModemap: local := NIL
  $lisplibModemapAlist: local := NIL
  $lisplibOperationAlist: local := NIL
  $lisplibSuperDomain: local := NIL
  $libFile: local := NIL
  $lisplibCategory: local := nil
  $compiler_output_stream : local := nil
  --for categories, is rhs of definition; otherwise, is target of functor
  --will eventually become the "constructorCategory" property in lisplib
  --set in compDefineCategory1 if category, otherwise in finalizeLisplib
  libName := getConstructorAbbreviation op
  sayMSG ['"   initializing ",$spadLibFT,:bright libName,
    '"for",:bright op]
  -- following guarantee's compiler output files get closed.
  UNWIND_-PROTECT(
      PROGN(initializeLisplib libName,
            sayMSG ['"   compiling into ", $spadLibFT, :bright libName],
            res := FUNCALL(fn, df, m, e, prefix, fal),
            sayMSG ['"   finalizing ",$spadLibFT,:bright libName],
            finalizeLisplib libName),
      PROGN(if $compiler_output_stream then CLOSE($compiler_output_stream),
            RSHUT $libFile))
  lisplibDoRename(libName)
  filearg := make_full_namestring([libName, $spadLibFT])
  RPACKFILE filearg
  FRESH_-LINE $algebraOutputStream
  sayMSG fillerSpaces(72,'"-")
  unloadOneConstructor(op,libName)
  LOCALDATABASE(LIST GETDATABASE(op,'ABBREVIATION),NIL)
  $newConlist := [op, :$newConlist]  ---------->  bound in function "compiler"
  if $lisplibKind = 'category
    then updateCategoryFrameForCategory op
     else updateCategoryFrameForConstructor op
  res

initializeLisplib libName ==
  erase_lib([libName, 'ERRORLIB])
  $libFile:= writeLib(libName,'ERRORLIB)
  $compiler_output_stream := make_compiler_output_stream($libFile, libName)

finalizeLisplib libName ==
  lisplibWrite('"constructorForm",removeZeroOne $lisplibForm,$libFile)
  lisplibWrite('"constructorKind",kind:=removeZeroOne $lisplibKind,$libFile)
  lisplibWrite('"constructorModemap",removeZeroOne $lisplibModemap,$libFile)
  $lisplibCategory:= $lisplibCategory or $lisplibModemap.mmTarget
  -- set to target of modemap for package/domain constructors;
  -- to the right-hand sides (the definition) for category constructors
  lisplibWrite('"constructorCategory",$lisplibCategory,$libFile)
  lisplibWrite('"sourceFile", namestring($edit_file), $libFile)
  lisplibWrite('"modemaps",removeZeroOne $lisplibModemapAlist,$libFile)
  opsAndAtts:= getConstructorOpsAndAtts($lisplibForm, kind)
  lisplibWrite('"operationAlist", removeZeroOne first opsAndAtts, $libFile)
  lisplibWrite('"superDomain",removeZeroOne $lisplibSuperDomain,$libFile)
  lisplibWrite('"predicates",removeZeroOne  $lisplibPredicates,$libFile)
  lisplibWrite('"abbreviation",$lisplibAbbreviation,$libFile)
  lisplibWrite('"parents",removeZeroOne $lisplibParents,$libFile)
  lisplibWrite('"ancestors",removeZeroOne $lisplibAncestors,$libFile)
  lisplibWrite('"documentation",finalizeDocumentation(),$libFile)
  if $profileCompiler then profileWrite()
  if $lisplibForm and null rest $lisplibForm then
    MAKEPROP(first $lisplibForm, 'NILADIC, 'T)

lisplibDoRename(libName) ==
    replace_lib([libName, 'ERRORLIB], [libName, $spadLibFT])

lisplibError(cname,fname,type,cn,fn,typ,error) ==
  $bootStrapMode and error = "wrongType" => nil
  sayMSG bright ['"  Illegal ",$spadLibFT]
  error in '(duplicateAbb  wrongType) =>
    sayKeyedMsg("S2IL0007",
      [namestring [fname,$spadLibFT],type,cname,typ,cn])
  error is 'abbIsName =>
    throwKeyedMsg("S2IL0008",[fname,typ,namestring [fn,$spadLibFT]])

getPartialConstructorModemapSig(c) ==
  (s := getConstructorSignature c) => rest s
  throwEvalTypeMsg("S2IL0015",[c])

getConstructorOpsAndAtts(form, kind) ==
  kind is 'category => getCategoryOpsAndAtts(form)
  getFunctorOpsAndAtts(form)

getCategoryOpsAndAtts(catForm) ==
  -- returns [operations, :attributes] of first catForm
  [transformOperationAlist getSlot1FromCategoryForm(catForm)]

getFunctorOpsAndAtts(form) ==
  [transformOperationAlist $lisplibOperationAlist]

transformOperationAlist operationAlist ==
  --  this transforms the operationAlist which is written out onto LISPLIBs.
  --  The original form of this list is a list of items of the form:
  --        ((<op> <signature>) (<condition> (ELT $ n)))
  --  The new form is an op-Alist which has entries (<op> . signature-Alist)
  --      where signature-Alist has entries (<signature> . item)
  --        where item has form (<slotNumber> <condition> <kind>)
  --          where <kind> =
  --             NIL  => function
  --             CONST => constant ... and others
  newAlist:= nil
  for [[op,sig,:.],condition,implementation] in operationAlist repeat
    kind:=
      implementation is [eltEtc,.,n] and eltEtc in '(CONST ELT) => eltEtc
      implementation is [impOp,:.] =>
        impOp = 'XLAM => implementation
        impOp = CONST => impOp
        keyedSystemError("S2IL0025",[impOp])
      implementation = 'mkRecord => 'mkRecord
      keyedSystemError("S2IL0025",[implementation])
    signatureItem:=
      if u:= assoc([op,sig],$functionLocations) then n := [n,:rest u]
      kind = 'ELT =>
        condition = 'T => [sig,n]
        [sig,n,condition]
      [sig,n,condition,kind]
    itemList := insert(signatureItem, QLASSQ(op, newAlist))
    newAlist:= insertAlist(op,itemList,newAlist)
  newAlist

getConstructorModemap form ==
  GETDATABASE(opOf form, 'CONSTRUCTORMODEMAP)

getConstructorSignature form ==
  (mm := GETDATABASE(opOf(form),'CONSTRUCTORMODEMAP)) =>
    [[.,:sig],:.] := mm
    sig
  NIL

--% from MODEMAP BOOT

augModemapsFromDomain1(name,functorForm,e) ==
  GET(IFCAR functorForm, "makeFunctionList") =>
    addConstructorModemaps(name,functorForm,e)
  atom functorForm and (catform:= getmode(functorForm,e)) =>
    augModemapsFromCategory(name,name,functorForm,catform,e)
  mappingForm := getmodeOrMapping(IFCAR functorForm, e) =>
    ["Mapping",categoryForm,:functArgTypes]:= mappingForm
    catform:= substituteCategoryArguments(rest functorForm,categoryForm)
    augModemapsFromCategory(name,name,functorForm,catform,e)
  stackMessage [functorForm," is an unknown mode"]
  e

getSlot1FromCategoryForm ([op, :argl]) ==
  u:= eval [op,:MAPCAR('MKQ,TAKE(#argl,$FormalMapVariableList))]
  null VECP u =>
    systemErrorHere '"getSlot1FromCategoryForm"
  u.1


--% constructor evaluation
--  The following functions are used by the compiler but are modified
--  here for use with new LISPLIB scheme

mkEvalableCategoryForm c ==       --from DEFINE
  c is [op,:argl] =>
    op="Join" =>
        nargs := [mkEvalableCategoryForm x or return nil for x in argl]
        nargs => ["Join", :nargs]
    op is "DomainSubstitutionMacro" =>
        --$extraParms :local
        --catobj := EVAL c -- DomainSubstitutionFunction makes $extraParms
        --mkEvalableCategoryForm sublisV($extraParms, catobj)
        mkEvalableCategoryForm CADR argl
    op is "mkCategory" => c
    MEMQ(op,$CategoryNames) =>
        [x,m,$e]:= compOrCroak(c,$EmptyMode,$e)
        m=$Category => optFunctorBody x
    --loadIfNecessary op
    GETDATABASE(op,'CONSTRUCTORKIND) = 'category or
      get(op,"isCategory",$CategoryFrame) =>
        [op,:[quotifyCategoryArgument x for x in argl]]
    [x,m,$e]:= compOrCroak(c,$EmptyMode,$e)
    m=$Category => x
  MKQ c

isDomainForm(D,e) ==
  --added for MPOLY 3/83 by RDJ
  MEMQ(IFCAR D, $SpecialDomainNames) or isFunctor D or
    -- ((D is ['Mapping,target,:.]) and isCategoryForm(target,e)) or
     ((getmode(D,e) is ['Mapping,target,:.]) and isCategoryForm(target,e)) or
       isCategoryForm(getmode(D,e),e) or isDomainConstructorForm(D,e)

isDomainConstructorForm(D,e) ==
  D is [op,:argl] and (u:= get(op,"value",e)) and
    u is [.,["Mapping",target,:.],:.] and
      isCategoryForm(EQSUBSTLIST(argl,$FormalMapVariableList,target),e)

isFunctor x ==
  op:= opOf x
  not IDENTP op => false
  $InteractiveMode =>
    MEMQ(op,'(Union SubDomain Mapping Record)) => true
    MEMQ(GETDATABASE(op,'CONSTRUCTORKIND),'(domain package))
  u:= get(op,'isFunctor,$CategoryFrame)
    or MEMQ(op,'(SubDomain Union Record)) => u
  constructor? op =>
    prop := get(op,'isFunctor,$CategoryFrame) => prop
    if GETDATABASE(op,'CONSTRUCTORKIND) = 'category
      then updateCategoryFrameForCategory op
      else updateCategoryFrameForConstructor op
    get(op,'isFunctor,$CategoryFrame)
  nil
