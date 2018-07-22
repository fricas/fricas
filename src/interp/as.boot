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

--global hash tables for new compiler
$docHash  := MAKE_HASHTABLE('EQUAL)
$conHash  := MAKE_HASHTABLE('EQUAL)
$opHash   := MAKE_HASHTABLE('EQUAL)
$asyPrint := false

asList() ==
  OBEY '"rm -f temp.text"
  OBEY '"ls as/*.asy > temp.text"
  instream := OPEN '"temp.text"
  lines := [read_line instream while not EOFP instream]
  CLOSE instream
  lines

astran asyFile ==
--global hash tables for new compiler
  $docHash  := MAKE_HASHTABLE('EQUAL)
  $conHash := MAKE_HASHTABLE('EQUAL)
  $constantHash := MAKE_HASHTABLE('EQUAL)
  $niladics : local := nil
  $asyFile: local := asyFile
  $asFilename: local := STRCONC(PATHNAME_-NAME asyFile,'".as")
  asytran asyFile
  conlist := [x for x in HKEYS $conHash | HGET($conHash,x) isnt [.,.,"function",:.]]
  $mmAlist : local :=
    [[con,:asyConstructorModemap con] for con in conlist]
  $docAlist : local :=
    [[con,:REMDUP asyDocumentation con] for con in conlist]
  $parentsHash : local := MAKE_HASHTABLE('EQUAL)
--$childrenHash: local := MAKE_HASHTABLE('EQUAL)
  for con in conlist repeat
    parents := asyParents con
    HPUT($parentsHash,con,asyParents con)
--  for [parent,:pred] in parents repeat
--    parentOp := opOf parent
--    HPUT($childrenHash,parentOp,insert([con,:pred],HGET($childrenHash,parentOp)))
  $newConlist := union(conlist, $newConlist)
  [[x,:asMakeAlist x] for x in HKEYS $conHash]

asyParents(conform) ==
  acc := nil
  con:= opOf conform
--formals := TAKE(#formalParams,$TriangleVariableList)
  modemap := LASSOC(con,$mmAlist)
  $constructorCategory :local := asySubstMapping CADAR modemap
  for x in folks $constructorCategory repeat
--  x := SUBLISLIS(formalParams,formals,x)
--  x := SUBLISLIS(IFCDR conform,formalParams,x)
    acc := [:explodeIfs x,:acc]
  NREVERSE acc

asySubstMapping u ==
  u is [op,:r] =>
    op = "->" =>
       [s, t] := r
       args :=
          s is [op,:u] and asyComma? op => [asySubstMapping y for y in u]
          [asySubstMapping s]
       ['Mapping, asySubstMapping t, :args]
    [asySubstMapping x for x in u]
  u

asyMkSignature(con,sig) ==
--  atom sig => ['TYPE,con,sig]
-- following line converts constants into nullary functions
  atom sig => ['SIGNATURE,con,[sig]]
  ['SIGNATURE,con,sig]

asMakeAlist con ==
  record := HGET($conHash,con)
  [form,sig,predlist,kind,exposure,comments,typeCode,:filename] := first record
--TTT in case we put the wrong thing in for niladic catgrs
--if ATOM(form) and kind='category then form:=[form]
  if ATOM(form) then form:=[form]
  kind = 'function => asMakeAlistForFunction con
  abb := asyAbbreviation(con, #(IFCDR sig))
  if null IFCDR form then PUT(opOf form, 'NILADIC, 'T)
  modemap := asySubstMapping LASSOC(con,$mmAlist)
  $constructorCategory :local := CADAR modemap
  parents := mySort HGET($parentsHash,con)
--children:= mySort HGET($childrenHash,con)
  alists  := HGET($opHash,con)
  opAlist := SUBLISLIS($FormalMapVariableList, IFCDR form, CDDR alists)
  ancestorAlist := SUBLISLIS($FormalMapVariableList, IFCDR form, first alists)
  catAttrs := [[x,:true] for x in getAttributesFromCATEGORY $constructorCategory]
  attributeAlist := REMDUP [:CADR alists,:catAttrs]
  documentation :=
      SUBLISLIS($FormalMapVariableList, IFCDR form, LASSOC(con, $docAlist))
  filestring := STRCONC(PATHNAME_-NAME STRINGIMAGE filename,'".as")
  constantPart := HGET($constantHash,con) and [['constant,:true]]
  niladicPart := MEMQ(con,$niladics) and [['NILADIC,:true]]
  falist := TAKE(#IFCDR form, $FormalMapVariableList)
  constructorCategory :=
    kind = 'category =>
      talist := TAKE(#IFCDR form, $TriangleVariableList)
      SUBLISLIS(talist, falist, $constructorCategory)
    SUBLISLIS(falist, IFCDR form, $constructorCategory)
  if constructorCategory='Category then kind := 'category
  exportAlist := asGetExports(kind, form, constructorCategory)
  constructorModemap  := SUBLISLIS(falist, IFCDR form, modemap)
--TTT fix a niladic category constructormodemap (remove the joins)
  if kind = 'category then
     SETF(CADAR(constructorModemap),['Category])
  res := [['constructorForm,:form],:constantPart,:niladicPart,
           ['constructorKind,:kind],
             ['constructorModemap,:constructorModemap],
              ['abbreviation,:abb],
               ['constructorCategory,:constructorCategory],
                ['parents,:parents],
                 ['attributes,:attributeAlist],
                  ['ancestors,:ancestorAlist],
                   --                ['children,:children],
                   ['sourceFile,:filestring],
                    ['operationAlist,:zeroOneConversion opAlist],
                     ['modemaps,:asGetModemaps(exportAlist,form,kind,modemap)],
                       ['sourcefile,:$asFilename],
                         ['typeCode,:typeCode],
                          ['documentation,:documentation]]
  if $asyPrint then asyDisplay(con,res)
  res

asGetExports(kind, conform, catform) ==
  [., :op_lst] := categoryParts1(kind, conform, catform, false) or return nil
  -- ensure that signatures are lists
  [[op, sigpred] for [op,sig,:pred] in op_lst] where
    sigpred ==
      pred :=
        pred = "T" => nil
        pred
      [sig, nil, :pred]

asMakeAlistForFunction fn ==
  record := HGET($conHash,fn)
  [form,sig,predlist,kind,exposure,comments,typeCode,:filename] := first record
  modemap := LASSOC(fn,$mmAlist)
  newsig := asySignature(sig,nil)
  opAlist := [[fn,[newsig,nil,:predlist]]]
  res := [['modemaps,:asGetModemaps(opAlist,fn,'function,modemap)],
            ['typeCode,:typeCode]]
  if $asyPrint then asyDisplay(fn,res)
  res

getAttributesFromCATEGORY catform ==
  catform is ['CATEGORY,.,:r] => [y for x in r | x is ['ATTRIBUTE,y]]
  catform is ['Join,:m,x]     => getAttributesFromCATEGORY x
  nil

displayDatabase x == main where
  main ==
    for y in
     '(CONSTRUCTORFORM CONSTRUCTORKIND _
       CONSTRUCTORMODEMAP _
       ABBREVIATION _
       CONSTRUCTORCATEGORY _
       PARENTS _
       ANCESTORS _
       SOURCEFILE _
       OPERATIONALIST _
       MODEMAPS _
       SOURCEFILE _
       DOCUMENTATION) repeat fn(x,y)
  fn(x,y) ==
    sayBrightly ['"----------------- ",y,'" --------------------"]
    pp GETDATABASE(x,y)

-- For some reason Dick has modified as.boot to convert the
-- identifier |0| or |1| to an integer in the list of operations.
-- This is WRONG, all existing code assumes that operation names
-- are always identifiers not numbers.
-- This function breaks the ability of the interpreter to find
-- |0| or |1| as exports of new compiler domains.
-- Unless someone has a strong reason for keeping the change,
-- this function should be no-opped, i.e.
-- zeroOneConversion opAlist == opAlist
-- If this change is made, then we are able to find asharp constants again.
--   bmt Mar 26, 1994  and executed by rss

zeroOneConversion opAlist == opAlist
--   for u in opAlist repeat
--     [op,:.] := u
--     DIGITP (PNAME op).0 => RPLACA(u, string2Integer PNAME op)
--   opAlist

asyDisplay(con,alist) ==
  banner := '"=============================="
  sayBrightly [banner,'" ",con,'" ",banner]
  for [prop,:value] in alist repeat
    sayBrightlyNT [prop,'": "]
    pp value

asGetModemaps(opAlist,oform,kind,modemap) ==
  acc:= nil
  rpvl:=
    MEMQ(kind, '(category function)) => rest $PatternVariableList -- *1 is special for $
    $PatternVariableList
  form := [opOf oform, :[y for x in IFCDR oform for y in rpvl]]
  dc :=
    MEMQ(kind, '(category function)) => "*1"
    form
  pred1 :=
    kind = 'category => [["*1",form]]
    nil
  signature  := CDAR modemap
  domainList :=
    [[a,m] for a in rest form for m in rest signature |
       asIsCategoryForm m]
  catPredList:=
    kind = 'function => [["isFreeFunction","*1",opOf form]]
    [['ofCategory,:u] for u in [:pred1,:domainList]]
--  for [op,:itemlist] in SUBLISLIS(rpvl, $FormalMapVariableList,opAlist) repeat
--  the code seems to oscillate between generating $FormalMapVariableList
--  and generating $TriangleVariableList
  for [op,:itemlist] in SUBLISLIS(rpvl, $FormalMapVariableList,opAlist) repeat
    for [sig0, pred] in itemlist repeat
      sig := SUBST(dc,"$",sig0)
      pred:= SUBST(dc,"$",pred)
      sig := SUBLISLIS(rpvl, IFCDR oform, sig)
      pred:= SUBLISLIS(rpvl, IFCDR oform, pred)
      pred := pred or 'T
  ----------> Constants change <--------------
      if IDENTP sig0 then
          sig := [sig]
          pred := MKPF([pred,'(isAsConstant)],'AND)
      pred' := MKPF([pred,:catPredList],'AND)
      mm := [[dc,:sig],[pred']]
      acc := [[op,:interactiveModemapForm mm],:acc]
  NREVERSE acc

asIsCategoryForm m ==
  m = 'BasicType or GETDATABASE(opOf m,'CONSTRUCTORKIND) = 'category

asyDocumentation con ==
  docHash := HGET($docHash,con)
  u := [[op,:[fn(x,op) for x in rec]] for op in HKEYS docHash
           | rec := HGET(docHash,op)] where fn(x,op) ==
    [form,sig,pred,origin,where?,comments,:.] := x
    ----------> Constants change <--------------
    if IDENTP sig then sig := [sig]
    [asySignature(sig,nil),trimComments comments]
  [form,sig,pred,origin,where?,comments] := first HGET($conHash,con)
  --above "first" assumes only one entry
  comments := trimComments asyExtractDescription comments
  [:u,['constructor,[nil,comments]]]

asyExtractDescription str ==
  k := STRPOS('"Description:",str,0,nil) => asyExtractDescription SUBSTRING(str,k + 12,nil)
  k := STRPOS('"Author:",str,0,nil) => asyExtractDescription SUBSTRING(str,0,k)
  str

trimComments str ==
  null str or str = '"" => '""
  m := MAXINDEX str
  str := SUBSTRING(str,0,m)
  trimString str

asyExportAlist con ==
--format of 'operationAlist property of LISPLIBS (as returned from koOps):
--    <sig slotNumberOrNil optPred optELT>
--!!! asyFile NEED: need to know if function is implemented by domain!!!
  docHash := HGET($docHash,con)
  [[op,:[fn(x,op) for x in rec]] for op in HKEYS docHash | rec := HGET(docHash,op)]
       where fn(x,op) ==
    [form,sig,pred,origin,where?,comments,:.] := x
    tail :=
      pred => [pred]
      nil
    newSig := asySignature(sig,nil)
    [newSig,nil,:tail]

asyMakeOperationAlist(con,proplist, key) ==
  oplist :=
    u := LASSOC('domExports,proplist) =>
      kind := 'domain
      u
    u := LASSOC('catExports,proplist) =>
      kind := 'category
      u
    key = 'domain =>
      kind := 'domain
      u := NIL
    return nil
  ht := MAKE_HASHTABLE('EQUAL)
  ancestorAlist := nil
  for ['Declare,id,form,r] in oplist repeat
    id = "%%" =>
      opOf form = con => nil
      y := asyAncestors form
      if opOf(y)~=con then ancestorAlist := [ [y,:true],:ancestorAlist]
    idForm   :=
      form is ['Apply,'_-_>,source,target] => [id,:asyArgs source]
  ----------> Constants change <--------------
      id
    pred :=
      LASSOC('condition,r) is p => hackToRemoveAnd p
      nil
    sig := asySignature(asytranForm(form,[idForm],nil),nil)
    entry :=
      --id ~= "%%" and IDENTP idForm => [[sig],nil,nil,'ASCONST]
      id ~= "%%" and IDENTP idForm =>
          pred => [[sig],nil,asyPredTran pred,'ASCONST]
          [[sig],nil,true,'ASCONST]
      pred => [sig,nil,asyPredTran pred]
      [sig]
    HPUT(ht,id,[entry,:HGET(ht,id)])
  opalist := [[op,:REMDUP HGET(ht,op)] for op in HKEYS ht]
  HPUT($opHash,con,[ancestorAlist,nil,:opalist])

hackToRemoveAnd p ==
---remove this as soon as .asy files do not contain forms (And pred) forms
  p is ['And,q,:r] =>
    r => ['AND,q,:r]
    q
  p

asyAncestors x ==
  x is ['Apply,:r] => asyAncestorList r
  x is [op,y,:.] and MEMQ(op, '(PretendTo RestrictTo)) => asyAncestors y
  atom x =>
    x = '_% => '_$
    MEMQ(x, $niladics)       => [x]
    GETDATABASE(x ,'NILADIC) => [x]
    x
  asyAncestorList x

asyAncestorList x == [asyAncestors y for y in x]
--============================================================================
--       Build Operation Alist from sig
--============================================================================

--format of operations as returned from koOps
--    <sig pred pakOriginOrNil TifPakExposedOrNil>
--    <sig pred origin         exposed?>

--abb,kind,file,sourcefile,coSig,dbLineNumber,constructorArgs,libfile
--((sig  where(NIL or #)  condition(T or pred)  ELT) ...
--expanded lists are: sig, predicate, origin, exposeFlag, comments

--============================================================================
--       Building Hash Tables for Operations/Constructors
--============================================================================
asytran fn ==
--put operations into table format for browser:
--    <sig pred origin         exposed? comments>
  inStream := OPEN fn
  sayBrightly ['"   Reading ",fn]
  u := VMREAD inStream
  $niladics := mkNiladics u
  for x in $niladics repeat PUT(x,'NILADIC,true)
  for d in u repeat
    ['Declare,name,:.] := d
    name = "%%" => 'skip       --skip over top-level properties
    $docHashLocal: local := MAKE_HASHTABLE('EQUAL)
    asytranDeclaration(d,'(top),nil,false)
    if null name then BREAK()
    HPUT($docHash,name,$docHashLocal)
  CLOSE inStream
  'done

mkNiladics u ==
  [name for x in u | x is ['Declare,name,y,:.] and y isnt ['Apply,'_-_>,:.]]

asytranDeclaration(dform,levels,predlist,local?) ==
  ['Declare,id,form,r] := dform
  id = 'failed => id
  IFCAR dform ~= 'Declare => systemError '"asytranDeclaration"
  if levels = '(top) then
    if form isnt ['Apply,"->",:.] then HPUT($constantHash,id,true)
  comments := LASSOC('documentation,r) or '""
  idForm   :=
    levels is ['top,:.] =>
      form is ['Apply,'_-_>,source,target] => [id,:asyArgs source]
      id
  ----------> Constants change <--------------
    id
  newsig  := asytranForm(form,[idForm,:levels],local?)
  key :=
    levels is ['top,:.] =>
      MEMQ(id,'(%% Category Type)) => 'constant
      asyLooksLikeCatForm? form => 'category
      form is ['Apply, '_-_>,.,u] =>
        if u is ['Apply, construc,:.] then u:= construc
        GETDATABASE(opOf u,'CONSTRUCTORKIND) = 'domain  => 'function
        asyLooksLikeCatForm? u => 'category
        'domain
      'domain
    first levels
  typeCode := LASSOC('symeTypeCode,r)
  record := [idForm,newsig,asyMkpred predlist,key,true,comments,typeCode,:$asyFile]
  if not local? then
    ht :=
      levels = '(top) => $conHash
      $docHashLocal
    HPUT(ht,id,[record,:HGET(ht,id)])
  if levels = '(top) then asyMakeOperationAlist(id,r, key)
  ['Declare,id,newsig,r]

asyLooksLikeCatForm? x ==
--TTT don't see a Third in my version ....
  x is ['Define, ['Declare, ., ['Apply, 'Third,:.],:.],:.] or
   x is ['Define, ['Declare, ., 'Category ],:.]

asyIsCatForm form ==
  form is ['Apply,:r] =>
    r is ['_-_>,.,a] => asyIsCatForm a
    r is ['Third,'Type,:.] => true
    false
  false

asyArgs source ==
  args :=
    source is [op,:u] and asyComma? op => u
    [source]
  [asyArg x for x in args]

asyArg x ==
  x is ['Declare,id,:.] => id
  x

asyMkpred predlist ==
  null predlist => nil
  predlist is [p] => p
  ['AND,:predlist]

asytranForm(form,levels,local?) ==
  u := asytranForm1(form,levels,local?)
  null u => hahah()
  u

asytranForm1(form,levels,local?) ==
  form is ['With,left,cat] =>
--  left ~= nil       => error '"WITH cannot take a left argument yet"
    asytranCategory(form,levels,nil,local?)
  form is ['Apply,:.]   => asytranApply(form,levels,local?)
  form is ['Declare,:.] => asytranDeclaration(form,levels,nil,local?)
  form is ['Comma,:r]  => ['Comma,:[asytranForm(x,levels,local?) for x in r]]
--form is ['_-_>,:s] => asytranMapping(s,levels,local?)
  form is [op,a,b] and MEMQ(a,'(PretendTo RestrictTo)) =>
    asytranForm1(a,levels,local?)
  form is ['LitInteger,s] =>
        READ_-FROM_-STRING(s)
  form is ['Define,:.]  =>
    form is ['Define,['Declare,.,x,:.],rest] =>
--TTT i don't know about this one but looks ok
      x = 'Category => asytranForm1(rest,levels, local?)
      asytranForm1(x,levels,local?)
    error '"DEFINE forms are not handled yet"
  if form = '_% then $hasPerCent := true
  IDENTP form =>
    form = "%" => "$"
    GETL(form,'NILADIC) => [form]
    form
  [asytranForm(x,levels,local?) for x in form]

asytranApply(['Apply,name,:arglist],levels,local?) ==
  MEMQ(name,'(Record Union)) =>
    [name,:[asytranApplySpecial(x, levels, local?) for x in arglist]]
  null arglist => [name]
  name is [ 'RestrictTo, :.] =>
    asytranApply(['Apply, first rest name, :arglist], levels, local?)
  name is [ 'Qualify, :.] =>
    asytranApply(['Apply, first rest name, :arglist], levels, local?)
  name is 'string => asytranLiteral first arglist
  name is 'integer => asytranLiteral first arglist
  name is 'float => asytranLiteral first arglist
  name = 'Enumeration =>
    ["Enumeration",:[asytranEnumItem arg for arg in arglist]]
  [:argl,lastArg] := arglist
  [name,:[asytranFormSpecial(arg,levels,true) for arg in argl],
          asytranFormSpecial(lastArg,levels,false)]

asytranLiteral(lit) ==
  first rest lit

asytranEnumItem arg ==
  arg is ['Declare, name, :.] => name
  error '"Bad Enumeration entry"

asytranApplySpecial(x, levels, local?) ==
  x is ['Declare, name, typ, :.] => [":",name,asytranForm(typ, levels, local?)]
  asytranForm(x, levels, local?)

asytranFormSpecial(x, levels, local?) ==  --> this throws away variable name (revise later)
  x is ['Declare, name, typ, :.] => asytranForm(typ, levels, local?)
  asytranForm(x, levels, local?)

asytranCategory(form,levels,predlist,local?) ==
  cat :=
    form is ['With,left,right] =>
      right is ['Blank,:.] => ['Sequence]
      right
    form
  left :=
    form is ['With,left,right] =>
      left is ['Blank,:.] => nil
      left
    nil
  $hasPerCent: local := nil
  items :=
    cat is ['Sequence,:s] => s
    [cat]
  catTable := MAKE_HASHTABLE('EQUAL)
  catList  := nil
  for x in items | x repeat
    if null x then systemError()
    dform := asytranCategoryItem(x,levels,predlist,local?)
    null dform => nil
    dform is ['Declare,id,record,r] =>
      HPUT(catTable,id,[asyWrap(record,predlist),:HGET(catTable,id)])
    catList := [asyWrap(dform,predlist),:catList]
  keys := listSort(function GLESSEQP,HKEYS catTable)
  right1 := NREVERSE catList
  right2 := [[key,:HGET(catTable,key)] for key in keys]
  right :=
    right2 => [:right1,['Exports,:right2]]
    right1
  res :=
    left => [left,:right]
    right
  res is [x] and x is ['IF,:.] => x
  ['With,:res]

asyWrap(record,predlist) ==
  predlist => ['IF,MKPF(predlist,'AND),record]
  record

asytranCategoryItem(x,levels,predlist,local?) ==
  x is ['If,predicate,item,:r] =>
    IFCAR r => error '"ELSE expressions not allowed yet in conditionals"
    pred :=
      predicate is ['Test,r] => r
      predicate
    asytranCategory(item,levels,[pred,:predlist],local?)
  MEMQ(IFCAR x, '(Default Foreign)) => nil
  x is ['Declare,:.] => asytranDeclaration(x,levels,predlist,local?)
  x

--============================================================================
--          Extending Constructor Datatable
--============================================================================
--FORMAT of $constructorDataTable entry:
--abb kind libFile sourceFile coSig constructorArgs
--alist is ((kind . domain) (libFile . MATRIX) (sourceFile . "matrix")
--         (coSig NIL T) (dbLineNumber . 29187) (constructorArgs R)
--  (modemap . (
--    (|Matrix| |#1|)
--      (Join (MatrixCategory #1 (Vector #1) (Vector #1))
--        (CATEGORY domain
--          (SIGNATURE diagonalMatrix ($ (Vector #1)))
--          (IF (has #1 (Field))
--            (SIGNATURE inverse ((Union $ "failed") $)) noBranch)))
--      (Ring))
--    (T Matrix))   )
extendConstructorDataTable() ==
  for x in listSort(function GLESSEQP,HKEYS $conHash) repeat
     record := HGET($conHash,x)
     [form,sig,predlist,origin,exposure,comments,typeCode,:filename] := first record
     abb := asyAbbreviation(x,#(rest sig))
     kind := 'domain
     --Note: this "first" assumes that there is ONLY one sig per name
     cosig := [nil,:asyCosig sig]
     args  := asyConstructorArgs sig
     tb :=
       [[x,abb,
          ['kind,:kind],
            ['cosig,:cosig],
              ['libfile,filename],
                ['sourceFile,STRINGIMAGE filename],
                  ['constructorArgs,:args]],:tb]
  listSort(function GLESSEQP,ASSOCLEFT tb)

asyConstructorArgs sig ==
  sig is ['With,:.] => nil
  sig is ['_-_>,source,target] =>
    source is [op,:argl] and asyComma? op => [asyConstructorArg x for x in argl]
    [asyConstructorArg source]

asyConstructorArg x ==
  x is ['Declare,name,t,:.] => name
  x

asyCosig sig ==    --can be a type or could be a signature
  atom sig or sig is ['With,:.] => nil
  sig is ['_-_>,source,target] =>
    source is [op,:argl] and asyComma? op => [asyCosigType x for x in argl]
    [asyCosigType source]
  error false

asyCosigType u ==
  u is [name,t] =>
    t is [fn,:.] =>
      asyComma? fn => fn
      fn = 'With  => 'T
      nil
    t = 'Type => 'T
    error '"Unknown atomic type"
  error false

asyAbbreviation(id,n) ==  chk(id,main) where   --> n = number of arguments
  main ==
    a := createAbbreviation id => a
    name := PNAME id
--  #name < 8 => INTERN UPCASE name
    parts := asySplit(name,MAXINDEX name)
    newname := "STRCONC"/[asyShorten x for x in parts]
    #newname < 8 => INTERN newname
    tryname := SUBSTRING(name,0,7)
    not createAbbreviation tryname => INTERN UPCASE tryname
    nil
  chk(conname,abb) ==
    (xx := asyGetAbbrevFromComments conname) => xx
    con := abbreviation? abb =>
      conname = con => abb
      conname
    abb

asyGetAbbrevFromComments con ==
  docHash := HGET($docHash,con)
  u := [[op,:[fn(x,op) for x in rec]] for op in HKEYS docHash
           | rec := HGET(docHash,op)] where fn(x,op) ==
    [form,sig,pred,origin,where?,comments,:.] := x
    ----------> Constants change <--------------
    if IDENTP sig then sig := [sig]
    [asySignature(sig,nil),trimComments comments]
  [form,sig,pred,origin,where?,comments] := first HGET($conHash,con)
  --above "first" assumes only one entry
  x := asyExtractAbbreviation comments
  x => intern x
  NIL

asyExtractAbbreviation str ==
        not (k:= STRPOS('"Abbrev: ",str,0,nil)) => NIL
        str := SUBSTRING(str, k+8, nil)
        k := STRPOS($stringNewline, str,0,nil)
        k => SUBSTRING(str, 0, k)
        str

asyShorten x ==
  y := createAbbreviation x
    or LASSOC(x,
        '(("Small" . "SM") ("Single" ."S") ("Half" . "H")("Point" . "PT")
            ("Floating" . "F") ("System" . "SYS") ("Number" . "N")
             ("Inventor" . "IV")
              ("Finite" . "F") ("Double" . "D") ("Builtin" . "BI"))) => y
  UPCASE x

asySplit(name,end) ==
  end < 1 => [name]
  k := 0
  for i in 1..end while LOWER_-CASE_-P name.i repeat k := i
  k := k + 1
  [SUBSTRING(name,0,k),:asySplit(SUBSTRING(name,k,nil),end-k)]

createAbbreviation s ==
  if STRINGP s then s := INTERN s
  a := constructor? s
  a ~= s => a
  nil

--============================================================================
--       extending getConstructorModemap Property
--============================================================================
--Note: modemap property is built when getConstructorModemap is called

asyConstructorModemap con ==
  HGET($conHash,con) isnt [record,:.] => nil   --not there
  [form,sig,predlist,kind,exposure,comments,typeCode,:filename] := record
  $kind: local := kind
  --NOTE: sig has the form (-> source target) or simply (target)
  $constructorArgs : local := IFCDR form
  signature := asySignature(sig,false)
  formals := ['_$,:TAKE(#$constructorArgs,$FormalMapVariableList)]
  mm := [[[con,:$constructorArgs],:signature],['T,con]]
  SUBLISLIS(formals,['_%,:$constructorArgs],mm)

asySignature(sig,names?) ==
  sig is ['Join,:.] => [asySig(sig,nil)]
  sig is ['With,:.] => [asySig(sig,nil)]
  sig is ['_-_>,source,target] =>
    target :=
      names? => ['dummy,target]
      target
    source is [op,:argl] and asyComma? op =>
      [asySigTarget(target,names?),:[asySig(x,names?) for x in argl]]
    [asySigTarget(target,names?),asySig(source,names?)]
  ----------> The following is a hack for constants which are category names<--
  sig is ['Third,:.] => [asySig(sig,nil)]
  ----------> Constants change <--------------
  asySig(sig,nil)

asySigTarget(u,name?) == asySig1(u,name?,true)

asySig(u,name?) == asySig1(u,name?,false)

asySig1(u,name?,target?) ==
  x :=
    name? and u is [name,t] => t
    u
  x is [fn,:r] =>
    fn = 'Join => asyTypeJoin r       ---------> jump out to newer code 4/94
    MEMQ(fn, '(RestrictTo PretendTo)) => asySig(first r,name?)
    asyComma? fn =>
      u := [asySig(x,name?) for x in r]
      target? =>
        null u => '(Void)
        -- this implies a multiple value return, not currently supported
        -- in the interpreter
        ['Multi,:u]
      u
    fn = 'With  => asyCATEGORY r
    fn = 'Third =>
      r is [b] =>
        b is ['With,:s]  => asyCATEGORY s
        b is ['Blank,:.] => asyCATEGORY nil
      error x
    fn = 'Apply and r is ['_-_>,:s] => asyMapping(s,name?)
    fn = '_-_> => asyMapping(r,name?)
    fn = 'Declare and r is [name,typ,:.] =>
        asySig1(typ, name?, target?)
    x is '(_%) => '(_$)
    [fn,:[asySig(x,name?) for x in r]]
--x = 'Type => '(Type)
  x = '_% => '_$
  x

asyMapping([a,b],name?) ==
  newa := asySig(a,name?)
  b    := asySig(b,name?)
  args :=
    a is [op,:r] and asyComma? op => newa
    [a]
  ['Mapping,b,:args]

--============================================================================
--       code for asySignatures of the form (Join,:...)
--============================================================================
asyType x ==
  x is [fn,:r] =>
    fn = 'Join => asyTypeJoin r
    MEMQ(fn, '(RestrictTo PretendTo)) => asyType first r
    asyComma? fn =>
      u := [asyType x for x in r]
      u
    fn = 'With  => asyCATEGORY r
    fn = '_-_> => asyTypeMapping r
    fn = 'Apply => r
--  fn = 'Declare and r is [name,typ,:.] => typ
    x is '(_%) => '(_$)
    x
--x = 'Type => '(Type)
  x = '_% => '_$
  x

asyTypeJoin r ==
  $conStack : local := nil
  $opStack  : local := nil
  $predlist : local := nil
  for x in r repeat asyTypeJoinPart(x,$predlist)
  catpart :=
    $opStack => ['CATEGORY,$kind,:asyTypeJoinStack REVERSE $opStack]
    nil
  conpart := asyTypeJoinStack REVERSE $conStack
  conpart =>
    catpart     => ['Join,:conpart,catpart]
    rest conpart => ['Join, :conpart]
    conpart
  catpart

asyTypeJoinPart(x,$predlist) ==
  x is ['Join,:y] => for z in y repeat asyTypeJoinPart(z, $predlist)
  x is ['With,:y] => for p in y repeat asyTypeJoinPartWith p
  asyTypeJoinPartWith x

asyTypeJoinPartWith x ==
  x is ['Exports,:y] => for p in y repeat asyTypeJoinPartExport p
  x is ['Exports,:.] => systemError 'exports
  x is ['Comma]  => nil
  x is ['Export,:y]  => nil
  x is ['IF,:r] => asyTypeJoinPartIf r
  x is ['Sequence,:x] => for y in x repeat asyTypeJoinItem y
  asyTypeJoinItem x

asyTypeJoinPartIf [pred,value] ==
  predlist := [asyTypeJoinPartPred pred,:$predlist]
  asyTypeJoinPart(value,predlist)

asyTypeJoinPartPred x ==
  x is ['Test, y] => asyTypeUnit y
  asyTypeUnit x

asyTypeJoinItem x ==
  result := asyTypeUnit x
  isLowerCaseLetter (PNAME opOf result).0 =>
    $opStack := [[['ATTRIBUTE,result],:$predlist],:$opStack]
  $conStack := [[result,:$predlist],:$conStack]

asyTypeMapping([a,b]) ==
  a := asyTypeUnit a
  b := asyTypeUnit b
  args :=
    a is [op,:r] and asyComma? op => r
    [a]
  ['Mapping,b,:args]

asyTypeUnit x ==
  x is [fn,:r] =>
    fn = 'Join => systemError 'Join ----->asyTypeJoin r
    MEMQ(fn, '(RestrictTo PretendTo)) => asyTypeUnit first r
    asyComma? fn =>
      u := [asyTypeUnit x for x in r]
      u
    fn = 'With  => asyCATEGORY r
    fn = '_-_> => asyTypeMapping r
    fn = 'Apply => asyTypeUnitList r
    fn = 'Declare and r is [name,typ,:.] => asyTypeUnitDeclare(name,typ)
    x is '(_%) => '(_$)
    [fn,:asyTypeUnitList r]
  GETL(x,'NILADIC) => [x]
--x = 'Type => '(Type)
  x = '_% => '_$
  x

asyTypeUnitList x == [asyTypeUnit y for y in x]

asyTypeUnitDeclare(op,typ) ==
  typ is ['Apply, :r] => asyCatSignature(op,r)
  asyTypeUnit typ
--============================================================================
--               Translator for ['With,:.]
--============================================================================
asyCATEGORY x ==
  if x is [join,:y] and join is ['Apply,:s] then
    exports := y
    joins :=
      s is ['Join,:r] => [asyJoinPart u for u in r]
      [asyJoinPart s]
  else if x is [id,:y] and IDENTP id then
    joins := [[id]]
    exports := y
  else
    joins   := nil
    exports := x
  cats       := exports
  operations := nil
  if exports is [:r,['Exports,:ops]] then
    cats := r
    operations := ops
  exportPart :=
    ['CATEGORY,'domain,:"APPEND"/[asyCatItem y for y in operations]]
  cats := "append"/[asyCattran c for c in cats]
  joins or cats =>
    ['Join,:joins,:cats, exportPart]
  exportPart

simpCattran x ==
  u := asyCattran x
  u is [y] => y
  ['Join,:u]

asyCattran x ==
  x is ['With,:r] => "append"/[asyCattran1 x for x in r]
  x is ['IF,:.]   => "append"/[asyCattranConstructors(x,nil)]
  [x]

asyCattran1 x ==
  x is ['Exports,:y] => "append"/[asyCattranOp u for u in y]
  x is ['IF,:.]      => "append"/[asyCattranConstructors(x,nil)]
  systemError nil

asyCattranOp [op,:items] ==
  "append"/[asyCattranOp1(op,item,nil) for item in items]

asyCattranOp1(op, item, predlist) ==
  item is ['IF, p, x] =>
    pred := asyPredTran
      p is ['Test,t] => t
      p
--    x is ['IF,:.] => "append"/[asyCattranOp1('IF, x, [pred,:predlist])]
--  This line used to call asyCattranOp1 with too few arguments.  Following
--  fix suggested by RDJ.
    x is ['IF,:.] => "append"/[asyCattranOp1(op,y,[pred,:predlist]) for y in x]
    [['IF, asySimpPred(pred,predlist), asyCattranSig(op,x), 'noBranch]]
  [asyCattranSig(op,item)]

asyPredTran p == asyPredTran1 asyJoinPart p

asyPredTran1 p ==
  p is ['Has,x,y] => ['has,x, simpCattran y]
  p is ['Test, q] => asyPredTran1 q
  p is [op,:r] and MEMQ(op,'(AND OR NOT)) =>
    [op,:[asyPredTran1 q for q in r]]
  p

asyCattranConstructors(item, predlist) ==
  item is ['IF, p, x] =>
    pred := asyPredTran
      p is ['Test,t] => t
      p
    x is ['IF,:.] => "append"/[asyCattranConstructors(x, [pred,:predlist])]
    form := ['ATTRIBUTE, asyJoinPart x]
    [['IF, asySimpPred(pred,predlist), form, 'noBranch]]
  systemError()

asySimpPred(p, predlist) ==
  while predlist is [q,:predlist] repeat p := quickAnd(q,p)
  p

asyCattranSig(op,y) ==
  y isnt ["->",source,t] =>
-- following makes constants into nullary functions
     ['SIGNATURE, op, [asyTypeUnit y]]
  s :=
    source is ['Comma,:s] => [asyTypeUnit z for z in s]
    [asyTypeUnit source]
  t := asyTypeUnit t
  null t => ['SIGNATURE,op,s]
  ['SIGNATURE,op,[t,:s]]

asyJoinPart x ==
  IDENTP x => [x]
  asytranForm(x,nil,true)

asyCatItem item ==
  atom item  => [item]
  item is ['IF,.,.] => [item]
  [op,:sigs] := item
  [asyCatSignature(op,sig) for sig in sigs | sig]

asyCatSignature(op,sig) ==
  sig is ['_-_>,source,target] =>
     ['SIGNATURE,op, [asyTypeItem target,:asyUnTuple source]]
  ----------> Constants change <--------------
-- following line converts constants into nullary functions
  ['SIGNATURE,op,[asyTypeItem sig]]

asyUnTuple x ==
  x is [op,:u] and asyComma? op => [asyTypeItem y for y in u]
  [asyTypeItem x]

asyTypeItem x ==
  atom x =>
    x = '_%         => '_$
    x
  x is ['_-_>,a,b] =>
      ['Mapping,b,:asyUnTuple a]
  x is ['Apply,:r] =>
    r is ['_-_>,a,b] =>
      ['Mapping,b,:asyUnTuple a]
    r is ['Record,:parts] =>
      ['Record,:[[":",a,b] for ['Declare,a,b,:.] in parts]]
    r is ['Segment,:parts] =>
      ['Segment,:[asyTypeItem x for x in parts]]
    asytranApply(x,nil,true)
  x is ['Declare,.,t,:.] => asyTypeItem t
  x is ['Comma,:args] =>
    -- this implies a multiple value return, not currently supported
    -- in the interpreter
    args => ['Multi,:[asyTypeItem y for y in args]]
    ['Void]
  [asyTypeItem y for y in x]

--============================================================================
--               Utilities
--============================================================================
asyComma? op == MEMQ(op,'(Comma Multi))


hput(table,name,value) ==
  if null name then systemError()
  HPUT(table,name,value)

--============================================================================
--               Dead Code (for a very odd value of 'dead')
--============================================================================
asyTypeJoinPartExport x ==
  [op,:items] := x
  for y in items repeat
    y isnt ["->",source,t] =>
--       sig := ['TYPE, op, asyTypeUnit y]
-- converts constants to nullary functions (this code isn't dead)
       sig := ['SIGNATURE, op, [asyTypeUnit y]]
       $opStack := [[sig,:$predlist],:$opStack]
    s :=
      source is ['Comma,:s] => [asyTypeUnit z for z in s]
      [asyTypeUnit source]
    t := asyTypeUnit t
    sig :=
      null t => ['SIGNATURE,op,s]
      ['SIGNATURE,op,[t,:s]]
    $opStack := [[sig,:$predlist],:$opStack]

--============================================================================
--               Code to create opDead Code
--============================================================================
asyTypeJoinStack r ==
  al := [[[x while r is [[x, :q],:s] and p = q and (r := s; true)],:p]
           while r is [[.,:p],:.]]
  result := "append"/[fn for [y,:p] in al] where fn ==
    p => [['IF,asyTypeMakePred p,:y]]
    y
  result

asyTypeMakePred [p,:u] ==
  while u is [q,:u] repeat p := quickAnd(q,p)
  p
