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

makeFort(name,args,decls,results,returnType,aspInfo) ==
  -- Create an executable Fortran file to call a given library function,
  -- and a stub Axiom function to process its arguments.
  -- the following is a list of objects for which values need not be
  -- passed by the user.
  dummies := [SECOND(u) for u in args | EQUAL(car u,0)]
  args := [untangle2(u) for u in args] -- lose spad Union representation
    where untangle2 u ==
      atom (v := rest(u)) => v
      first(v)
  userArgs := [u for u in args | not member(u,dummies)]  -- Temporary
  decls := [untangle(u) for u in decls] -- lose spad Union representation
    where untangle u ==
      [if atom(rest(v)) then rest(v) else _
        [if atom(w) then w else rest(w) for w in rest(v)] for v in u]
  makeFort1(name,args,userArgs,dummies,decls,results,returnType,aspInfo)

makeFort1(name,args,userArgs,dummies,decls,results,returnType,aspInfo) ==
  asps := [first(u) for u in aspInfo]
  -- Now reorder the arguments so that all the scalars come first, so
  -- that when we come to deal with arrays we know all the dimensions.
  scalarArgs := [u for u in args | atom getFortranType(u,decls)]
  arrayArgs := [u for u in args | not member(u,scalarArgs)]
  orderedArgs := [:scalarArgs,:arrayArgs]
  file := if $fortranDirectory then
    STRCONC($fortranDirectory,"/",STRINGIMAGE name)
  else
    STRINGIMAGE name
  makeFortranFun(name,orderedArgs,args,dummies,decls,results,file,
                 $fortranDirectory,returnType,asps)
  makeSpadFun(name,userArgs,orderedArgs,dummies,decls,results,returnType,asps,
              aspInfo,file)
  name

makeFortranFun(name,args,fortranArgs,dummies,decls,results,file,dir,
               returnType,asps) ==
  -- Create a C file to call the library function, and compile it.
  fp := MAKE_-OUTSTREAM(STRCONC(file,".c"))
  writeCFile(name,args,fortranArgs,dummies,decls,results,returnType,asps,fp)
  if null dir then dir := '"."
  asps => SYSTEM STRCONC("cc -c ",file,".c ; mv ",file,".o ",dir)
  SYSTEM STRCONC("cc ",file,".c -o ",file,".spadexe ",$fortranLibraries)

writeCFile(name,args,fortranArgs,dummies,decls,results,returnType,asps,fp) ==
  WRITE_-LINE('"#include <stdio.h>",fp)
  WRITE_-LINE('"#include <sys/select.h>",fp)
  WRITE_-LINE('"#include <rpc/rpc.h>",fp)
  WRITE_-LINE('"#ifndef NULL",fp)
  WRITE_-LINE('"#define NULL 0",fp)
  WRITE_-LINE('"#endif  NULL",fp)
  WRITE_-LINE('"#define MAX__ARRAY(x) (x ? x :  20000)",fp)
  WRITE_-LINE('"#define CHECK(x) if (!x) {fprintf(stderr,_"xdr failed_"); exit(1);}",fp)
  WRITE_-LINE('"void main()",fp)
  WRITE_-LINE('"{",fp)
  WRITE_-LINE('"  XDR xdrs;",fp)
  WRITE_-LINE('"  {",fp)
  if $addUnderscoreToFortranNames then
    routineName := STRCONC(name, UNDERBAR)
  else
    routineName := name
  -- If it is a function then give it somewhere to stick its result:
  if returnType then
    returnName := INTERN STRCONC(name,"__result")
    wl(['"    ",getCType returnType,'" ",returnName,'",",routineName,'"();"],fp)
  -- print out type declarations for the Fortran parameters, and build an
  -- ordered list of pairs [<parameter> , <type>]
  argList := nil
  for a in args repeat
    argList := [[a, getCType getFortranType(a,decls)], :argList]
    printDec(SECOND first argList,a,asps,fp)
  argList := nreverse argList;
  -- read in the data
  WRITE_-LINE('"    xdrstdio__create(&xdrs, stdin, XDR__DECODE);",fp)
  for a in argList repeat
    if LISTP SECOND a then writeMalloc(first a,first SECOND a,rest SECOND a,fp)
    not MEMQ(first a,[:dummies,:asps]) => writeXDR(a,'"&xdrs",fp)
  -- now call the Library routine.  FORTRAN names may have an underscore
  -- appended.
  if returnType then
    wt(['"    ",returnName,'"="],fp)
  else
    wt(['"    "],fp)
  wt([routineName,'"("],fp)
  if first fortranArgs then
    printCName(first fortranArgs,isPointer?(first fortranArgs,decls),asps,fp)
  for a in rest fortranArgs repeat
    PRINC('",",fp)
    printCName(a,isPointer?(a,decls),asps,fp)
  writeStringLengths(fortranArgs,decls,fp)
  WRITE_-LINE('");",fp)
  -- now export the results.
  WRITE_-LINE('"    xdrstdio__create(&xdrs, stdout, XDR__ENCODE);",fp)
  if returnType then
    writeXDR([returnName,getCType returnType],'"&xdrs",fp)
  for r in results repeat
    writeXDR([r,getCType getFortranType(r,decls)],'"&xdrs",fp)
  WRITE_-LINE('"    exit(0);",fp)
  WRITE_-LINE('"  }",fp)
  WRITE_-LINE('"}",fp)

writeStringLengths(fortranArgs,decls,fp) ==
  for a in fortranArgs repeat
    if isString?(a,decls) then wt(['",&",a,'"__length"],fp)

isString?(u,decls) ==
  EQUAL(ty := getFortranType(u,decls),"character") or
    LISTP(ty) and EQUAL(first ty,"character")

isPointer?(u,decls) ==
  ty := getFortranType(u,decls)
  LISTP(ty) or ty in ["character","complex","double complex"]

printCName(u,ispointer,asps,fp) ==
  member(u,asps) =>
    PRINC(u,fp)
    if $addUnderscoreToFortranNames then PRINC(UNDERBAR, fp)
  if not ispointer then PRINC('"&",fp)
  PRINC(u,fp)

getFortranType(u,decls) ==
  -- find u in decls, return the given (Fortran) type.
  result := nil
  for d in decls repeat for dec in rest d repeat
    atom(dec) and dec=u =>
      return( result := first d )
    LISTP(dec) and first(dec)=u =>
      return( result := [first d,:rest dec] )
  result => result
  error ['"Undeclared Fortran parameter: ",u]

getCType t ==
  -- Return the equivalent C type.
  LISTP(t) =>
    --[if first(t)="character" then '"char" else getCType first t,:rest t]
    first(t)="character" => ['"char",:rest t]
    first(t)="complex" => ['"float",2,:rest t]
    first(t)="double complex" =>  ['"double",2,:rest t]
    [getCType first t,:rest t]
  t="double" => '"double"
  t="double precision" => '"double"
  t="integer" => '"int"
  t="real" => '"float"
  t="logical" => '"int"
  t="character" => ['"char",1]
  t="complex" => ['"float",2] --'"Complex" -- we use our own typedef
  t="double complex" =>  ['"double",2] --'"DComplex" -- we use our own typedef
  error ['"Unrecognised Fortran type: ",t]

XDRFun t ==
  LISTP(ty := SECOND t) =>
    if first(ty)='"char" then '"wrapstring" else '"array"
  ty

printDec(type,dec,asps,fp) ==
  wt(['"    ",if LISTP(type) then first(type) else type,'" "],fp)
  member(dec,asps) =>
    if $addUnderscoreToFortranNames then
      wl([dec, UNDERBAR,'"();"],fp)
    else
      wl([dec,'"();"],fp)
  LISTP(type) =>
    wl(['"*",dec,'" = NULL;"],fp)
    wl(['"    u__int ",dec, '"__length = 0;"],fp)
  type = '"char" =>
    wl(['"*",dec,'" = NULL;"],fp)
  wl([dec, '";"],fp)

writeXDR(v,str,fp) ==
  -- Generate the calls to the filters which will read from the temp
  -- file.  The CHECK macro ensures that the translation worked.
  underscore := STRING CHAR('"__:",0) -- to avoid a compiler bug which won't
                                     -- parse " ... __" properly.
  wt(['"    CHECK(xdr",underscore, XDRFun(v), '"(", str, '",&", first(v)],fp)
  if (LISTP (ty :=SECOND v)) and not EQUAL(first ty,'"char") then
    wt(['",&",first(v),'"__length,MAX__ARRAY(",first(v),'"__length),"],fp)
    wt(['"sizeof(",first(ty),'"),xdr",underscore,first ty],fp)
  wl(['"));"],fp)

prefix2Infix(l) ==
  atom(l) => [l]
  #l=2 => [first l,"(",:prefix2Infix SECOND l,")"]
  #l=3 => ["(",:prefix2Infix SECOND l,first l,:prefix2Infix THIRD l,")"]
  error '"Function in array dimensions with more than two arguments"

writeMalloc(name,type,dims,fp) ==
  -- Write out a malloc for array arguments
  -- Need the size as well
  wl(['"    ",name,'"__length=",prefix2Infix first dims,:[:["*",:prefix2Infix u]
      for u in rest dims],'";"], fp)
  type = '"char" =>
    wl(['"    ",name,'"=(",type," *)malloc((1+",name,
       '"__length)*sizeof(",type,'"));"],fp)
  wl(['"    ",name,'"=(",type," *)malloc(",name,
     '"__length*sizeof(",type,'"));"],fp)

wl (l,fp) ==
  for u in l repeat PRINC(u,fp)
  TERPRI(fp)

wt (l,fp) ==
  for u in l repeat PRINC(u,fp)

-- spadRecordType(v,decs) ==
--   -- Build a lisp representation of the declaration of a spad record.
--   -- This will be the returned type of the spad function which calls the
--   -- Fortran code.
--   ["Record",:[spadRecordType1(u,decs) for u in v]]
--
-- spadRecordType1(u,decls) ==
--   -- Create a list of the form '( |:| u <spadTypeTTT u>)
--   [":",u,spadTypeTTT getFortranType(u,decls)]

spadTypeTTT u ==
  -- Return the spad domain equivalent to the given Fortran type.
  -- Changed by MCD 8/4/94 to reflect correct format for domains in
  -- current system.
  LISTP u =>
    first(u)="character" => ["String"]
    first(u)="logical" and #u=2 => ["List",["Boolean"]]
    first(u)="logical" => ["List",["List",["Boolean"]]]
    #u=2 => ["Matrix",spadTypeTTT first u]
    #u=3 => ["Matrix",spadTypeTTT first u]
    #u=4 => ["ThreeDimensionalMatrix",spadTypeTTT first u]
    error '"Can only handle one-, two- and three-dimensional matrices"
  u = "double" => ["DoubleFloat"]
  u = "double precision" => ["DoubleFloat"]
  u = "real" => ["DoubleFloat"]
  u = "integer" => ["Integer"]
  u = "logical" => ["Boolean"]
  u = "character" => ["String"]
  u = "complex" => ["Complex",["DoubleFloat"]]
  u = "double complex" => ["Complex",["DoubleFloat"]]
  error ['"Unrecognised Fortran type: ",u]

mkQuote l ==
 [addQuote(u)for u in l] where
    addQuote u ==
      atom u => ['QUOTE,u]
      ["construct",:[addQuote(v) for v in u]]

makeLispList(l) ==
  outputList := []
  for u in l repeat
    outputList := [:outputList, _
                  if atom(u) then ['QUOTE,u] else [["$elt","Lisp","construct"],_
                  :makeLispList(u)]]
  outputList

makeSpadFun(name,userArgs,args,dummies,decls,results,returnType,asps,aspInfo,
            file) ==
  -- Create an interpreter function for the user to call.

  fType := ["List", ["Record" , [":","key","Symbol"], [":","entry","Any"]]]

  -- To make sure the spad interpreter isn't confused:
  if returnType then
    returnName := INTERN STRCONC(name,"Result")
    decls := [[returnType,returnName], :decls]
    results := [returnName, :results]
  argNames := [INTERN STRCONC(STRINGIMAGE(u),'"__arg") for u in userArgs]
  aType := [axiomType(a,decls,asps,aspInfo) for a in userArgs]
  aspTypes := [SECOND NTH(POSITION(u,userArgs),aType) for u in asps]
  nilLst := MAKE_-LIST(#args+1)
  decPar := [["$elt","Lisp","construct"],:makeLispList decls]
  fargNames := [INTERN STRCONC(STRINGIMAGE(u),'"__arg") for u in args |
                 not (MEMQ(u,dummies) or MEMQ(u,asps)) ]
  for u in asps repeat
    fargNames := delete(INTERN STRCONC(STRINGIMAGE(u),'"__arg"),fargNames)
  resPar := ["construct",["@",["construct",:fargNames],_
             ["List",["Any"]]]]
  call := [["$elt","Lisp","invokeFortran"],STRCONC(file,".spadexe"),_
           [["$elt","Lisp","construct"],:mkQuote args],_
           [["$elt","Lisp","construct"],:mkQuote union(asps,dummies)], decPar,_
           [["$elt","Lisp","construct"],:mkQuote results],resPar]
  if asps then
    -- Make a unique(ish) id for asp files
    aspId := STRCONC(getEnv('"SPADNUM"), GENTEMP('"NAG"))
    body := ["SEQ",:makeAspGenerators(asps,aspTypes,aspId),_
             makeCompilation(asps,file,aspId),_
             ["pretend",call,fType] ]
  else
    body := ["pretend",call,fType]
  interpret(["DEF",[name,:argNames],["Result",:aType],nilLst,_
             [["$elt","Result","construct"],body]], nil)

stripNil u ==
  [first(u), ["construct", :CADR(u)], if CADDR(u) then "true" else "false"]

makeUnion aspType ==
  -- The argument is the type of the asp to be generated.  We would like to
  -- allow the user to be able to provide a fileName as an alternative
  -- argument, so this builds the Union of aspType and FileName.
  ["Union",[":","fp",aspType],[":","fn","FileName"]]

axiomType(a,decls,asps,aspInfo) ==
  a in asps =>
    entry := first [u for u in aspInfo | first(u) = a]
    ftc := ["$elt","FortranType","construct"]
    rc  := ["$elt", _
             ["Record",[":","key","Symbol"],[":","entry","FortranType"]], _
            "construct"]
    makeUnion ["FortranProgram",_
      a,_
      CADR(entry),_
      ["construct",:mkQuote CADDR entry], _
      [ ["$elt", "SymbolTable","symbolTable"],_
          ["construct",_
            :[[rc,first(v),[ftc,:stripNil rest(v)]] for v in CADDDR entry]]_
    ] ]
  spadTypeTTT(getFortranType(a,decls))

makeAspGenerators(asps,types,aspId) ==
-- The code generated here will manipulate the Fortran output stack and write
-- the asps out as Fortran.
  [:makeAspGenerators1(u,v,aspId) for u in asps for v in types]

makeAspGenerators1(asp,type,aspId) ==
  [[["$elt","FOP","pushFortranOutputStack"] ,_
    ["filename",'"",STRCONC(STRINGIMAGE asp,aspId),'"f"]] , _
   makeOutputAsFortran INTERN STRCONC(STRINGIMAGE(asp),'"__arg"), _
   [["$elt","FOP","popFortranOutputStack"]]   _
  ]

makeOutputAsFortran arg ==
  ["IF",["case",arg,"fn"],["outputAsFortran",[arg,"fn"]],_
                          ["outputAsFortran",[arg,"fp"]] ]

makeCompilation(asps,file,aspId) ==
  [["$elt","Lisp","compileAndLink"],_
   ["construct",:[STRCONC(STRINGIMAGE a,aspId,'".f") for a in asps]], _
   $fortranCompilerName,_
   STRCONC(file,'".o"),_
   STRCONC(file,'".spadexe"),_
   $fortranLibraries]


compileAndLink(fortFileList,fortCompiler,cFile,outFile,linkerArgs) ==
  SYSTEM STRCONC (fortCompiler, addSpaces fortFileList,_
                  cFile, " -o ",outFile," ",linkerArgs)

addSpaces(stringList) ==
  l := " "
  for s in stringList repeat l := STRCONC(l,s," ")
  l

complexRows z ==
-- Take a list of lists of complexes (i.e. pairs of floats) and
-- make them look like a Fortran vector!
  [:[:pair2list(u.i) for u in z] for i in 0..#(z.0)-1]

pair2list u == [car u,cdr u]
vec2Lists1 u == [ELT(u,i) for i in 0..#u-1]
vec2Lists u == [vec2Lists1 ELT(u,i) for i in 0..#u-1]

spad2lisp(u) ==
  -- Turn complexes into arrays of floats
  first first(u)="Complex" =>
    makeVector([makeVector([CADR u,CDDR u],'DOUBLE_-FLOAT)],NIL)
  -- Turn arrays of complexes into arrays of floats so that tarnsposing
  -- them puts them in the correct fortran order
  first first(u)="Matrix" and first SECOND first(u) = "Complex" =>
    makeVector([makeVector(complexRows vec2Lists rest u,'DOUBLE_-FLOAT)],NIL)
  rest(u)

invokeFortran(objFile,args,dummies,decls,results,actual) ==
  actual := [spad2lisp(u) for u in first actual]
  returnedValues := spadify( _
        fortCall(objFile,prepareData(args,dummies,actual,decls),_
                 prepareResults(results,args,dummies,actual,decls)),_
                            results,decls,inFirstNotSecond(args,dummies),actual)

--  -- If there are one or two elements in returnedValues we must return a
--  -- cons cell, otherwise a vector.  This is to match the internal
--  -- representation of an Axiom Record.
--  #returnedValues = 1 => returnedValues
--  #returnedValues = 2 => CONS(first returnedValues,SECOND returnedValues)
--  makeVector(returnedValues,nil)

int2Bool u ==
  -- Return something which looks like an axiom boolean
  u=1 => "TRUE"
  NIL

makeResultRecord(name,type,value) ==
  -- Take an object returned by the NAG routine and make it into an AXIOM
  -- object of type Record(key:Symbol,entry:Any) for use by Result.
  CONS(name,CONS(spadTypeTTT type,value))

spadify(l,results,decls,names,actual) ==
  -- The elements of list l are the output forms returned from the Fortran
  -- code: integers, floats and vectors.  Return spad forms of these, of
  -- type Record(key:Symbol,entry:Any) (for use with the Result domain).
  SETQ(RESULTS,l)
  spadForms := nil
  for i in 0..(#l -1) repeat
    fort := NTH(i,l)
    name := NTH(i,results)
    ty := getFortranType(name,decls)
    -- Result is a string
    STRINGP fort =>
      spadForms := [makeResultRecord(name,ty,fort), :spadForms]
    -- Result is a Complex Scalar
    ty in ["double complex" , "complex"] =>
       spadForms := [makeResultRecord(name,ty, _
                                     CONS(ELT(fort,0),ELT(fort,1)) ),:spadForms]
    -- Result is a Complex vector or array
    LISTP(ty) and first(ty) in ["double complex" , "complex"] =>
      dims := [getVal(u,names,actual) for u in rest ty]
      els := nil
      if #dims=1 then
        els := [makeVector([CONS(ELT(fort,2*i),ELT(fort,2*i+1)) _
                for i in 0..(first(dims)-1)],nil)]
      else if #dims=2 then
        for r in 0..(first(dims) - 1) repeat
          innerEls := nil
          for c in 0..(SECOND(dims) - 1) repeat
            offset := 2*(c*first(dims)+r)
            innerEls := [CONS(ELT(fort,offset),ELT(fort,offset+1)),:innerEls]
          els := [makeVector(NREVERSE innerEls,nil),:els]
      else
         error ['"Can't cope with complex output dimensions higher than 2"]
      spadForms := [makeResultRecord(name,ty,makeVector(NREVERSE els,nil)),
                    :spadForms]
    -- Result is a Boolean vector or array
    LISTP(ty) and first(ty)="logical" and #ty=2 =>
      dim := getVal(first rest ty,names,actual)
      spadForms := [makeResultRecord(name,ty,_
                          [int2Bool ELT(fort,i) for i in 0..dim-1]), :spadForms]
    LISTP(ty) and first(ty)="logical" =>
      dims := [getVal(u,names,actual) for u in rest ty]
      els := nil
      if #dims=2 then
        for r in 0..(first(dims) - 1) repeat
          innerEls := nil
          for c in 0..(SECOND(dims) - 1) repeat
            innerEls := [int2Bool ELT(fort,c*first(dims)+r),:innerEls]
          els := [NREVERSE innerEls,:els]
      else
         error ['"Can't cope with logical output dimensions higher than 2"]
      spadForms := [makeResultRecord(name,ty,NREVERSE els), :spadForms]
    -- Result is a vector or array
    VECTORP fort =>
      dims := [getVal(u,names,actual) for u in rest ty]
      els := nil
      -- Check to see whether we are dealing with a dummy (0-dimensional) array.
      if MEMQ(0,dims) then
        els := [[]]
      else if #dims=1 then
        els := [makeVector([ELT(fort,i) for i in 0..(first(dims)-1)],nil)]
      else if #dims=2 then
        for r in 0..(first(dims) - 1) repeat
          innerEls := nil
          for c in 0..(SECOND(dims) - 1) repeat
            innerEls := [ELT(fort,c*first(dims)+r),:innerEls]
          els := [makeVector(NREVERSE innerEls,nil),:els]
      else if #dims=3 then
        iDim := first(dims)
        jDim := SECOND dims
        kDim := THIRD dims
        for r in 0..(iDim - 1) repeat
          middleEls := nil
          for c in 0..(jDim - 1) repeat
            innerEls := nil
            for p in 0..(kDim - 1) repeat
              offset := p*jDim + c*kDim + r
              innerEls := [ELT(fort,offset),:innerEls]
            middleEls := [makeVector(NREVERSE innerEls,nil),:middleEls]
          els := [makeVector(NREVERSE middleEls,nil),:els]
      else
         error ['"Can't cope with output dimensions higher than 3"]
      if not MEMQ(0,dims) then els := makeVector(NREVERSE els,nil)
      spadForms := [makeResultRecord(name,ty,els), :spadForms]
    -- Result is a Boolean Scalar
    atom fort and ty="logical" =>
      spadForms := [makeResultRecord(name,ty,int2Bool fort), :spadForms]
    -- Result is a Scalar
    atom fort =>
      spadForms := [makeResultRecord(name,ty,fort),:spadForms]
    error ['"Unrecognised output format: ",fort]
  NREVERSE spadForms

lispType u ==
  -- Return the lisp type equivalent to the given Fortran type.
  LISTP u => lispType first u
  u = "real" => "SHORT-FLOAT"
  u = "double" => "DOUBLE-FLOAT"
  u = "double precision" => "DOUBLE-FLOAT"
  u = "integer" => "FIXNUM"
  u = "logical" => "BOOLEAN"
  u = "character" => "CHARACTER"
  u = "complex" => "SHORT-FLOAT"
  u = "double complex" => "DOUBLE-FLOAT"
  error ['"Unrecognised Fortran type: ",u]

getVal(u,names,values) ==
  -- if u is the i'th element of names, return the i'th element of values,
  -- otherwise if it is an arithmetic expression evaluate it.
  NUMBERP(u) => u
  LISTP(u) => eval [first(u), :[getVal(v,names,values) for v in rest u]]
  (place := POSITION(u,names)) => NTH(place,values)
  error ['"No value found for parameter: ",u]


prepareData(args,dummies,values,decls) ==
-- TTT: we don't
-- writeData handles all the mess
   [args,dummies,values,decls]


checkForBoolean u ==
  u = "BOOLEAN" => "FIXNUM"
  u

prepareResults(results,args,dummies,values,decls) ==
  -- Create the floating point zeros (boot doesn't like 0.0d0, 0.0D0 etc)
  shortZero : fluid := COERCE(0.0,'SHORT_-FLOAT)
  longZero : fluid := COERCE(0.0,'DOUBLE_-FLOAT)
  data := nil
  for u in results repeat
    type := getFortranType(u,decls)
    data := [defaultValue(type,inFirstNotSecond(args,dummies),values),:data]
      where defaultValue(type,argNames,actual) ==
        LISTP(type) and first(type)="character" => MAKE_-STRING(1)
        LISTP(type) and first(type) in ["complex","double complex"] =>
          makeVector(  makeList(
            2*APPLY('_*,[getVal(tt,argNames,actual) for tt in rest(type)]),_
            if first(type)="complex" then shortZero else longZero),_
          if first(type)="complex" then "SHORT-FLOAT" else "DOUBLE-FLOAT" )
        LISTP type => makeVector(_
          makeList(
            APPLY('_*,[getVal(tt,argNames,actual) for tt in rest(type)]),_
            defaultValue(first type,argNames,actual)),_
          checkForBoolean lispType first(type) )
        type = "integer" => 0
        type = "real" => shortZero
        type = "double" => longZero
        type = "double precision" => longZero
        type = "logical" => 0
        type = "character" => MAKE_-STRING(1)
        type = "complex" => makeVector([shortZero,shortZero],'SHORT_-FLOAT)
        type = "double complex" => makeVector([longZero,longZero],'DOUBLE_-FLOAT)
        error ['"Unrecognised Fortran type: ",type]
  NREVERSE data

writeData(tmpFile,indata) ==
  -- Write the elements of the list data to a temporary file.  Return the
  -- name of that file.
  --
  str := MAKE_-OUTSTREAM(tmpFile)
  xstr := xdrOpen(str,true)
  [args,dummies,values,decls] := indata
  for v in values repeat
        -- the two Boolean values
        v = "T" =>
                xdrWrite(xstr,1)
        NULL v =>
                xdrWrite(xstr,0)
        -- characters
        STRINGP v =>
                xdrWrite(xstr,v)
        -- some array
        VECTORP v =>
                rows := first ARRAY_-DIMENSIONS(v)
                -- is it 2d or more (most likely) ?
                VECTORP ELT(v,0) =>
                        cols := first ARRAY_-DIMENSIONS(ELT(v, 0))
                        -- is it 3d ?
                        VECTORP ELT(ELT(v,0),0) =>
                                planes := first
                                    ARRAY_-DIMENSIONS(ELT(ELT(v, 0), 0))
                                -- write 3d array
                                xdrWrite(xstr,rows*cols*planes)
                                for k in 0..planes-1 repeat
                                        for j in 0..cols-1 repeat
                                                for i in 0..rows-1 repeat
                                                        xdrWrite(xstr,ELT(ELT(ELT(v,i),j),k))
                        -- write 2d array
                        xdrWrite(xstr,rows*cols)
                        for j in 0..cols-1 repeat
                                for i in 0..rows-1 repeat xdrWrite(xstr,ELT(ELT(v,i),j))
                -- write 1d array
                xdrWrite(xstr,rows)
                for i in 0..rows-1 repeat xdrWrite(xstr,ELT(v,i))
        -- this is used for lists of booleans apparently in f01
        LISTP v =>
                xdrWrite(xstr,LENGTH v)
                for el in v repeat
                        if el then xdrWrite(xstr,1) else xdrWrite(xstr,0)
        -- integers
        INTEGERP v =>
                xdrWrite(xstr,v)
        -- floats
        FLOATP v =>
                xdrWrite(xstr,v)
  SHUT(str)
  tmpFile

readData(tmpFile,results) ==
  -- read in the results from tmpFile.  The list results is a list of
  -- dummy objects of the correct type which will receive the data.
  str := MAKE_-INSTREAM(tmpFile)
  xstr := xdrOpen(str,false)
  results := [xdrRead1(xstr,r) for r in results] where
    xdrRead1(x,dummy) ==
      VECTORP(dummy) and ZEROP(LENGTH dummy) => dummy
      xdrRead(x,dummy)
  SHUT(str)
  results

generateDataName()==STRCONC($fortranTmpDir,getEnv('"HOST"),
    getEnv('"SPADNUM"), GENTEMP('"NAG"),'"data")
generateResultsName()==STRCONC($fortranTmpDir,getEnv('"HOST"),
    getEnv('"SPADNUM"), GENTEMP('"NAG"),'"results")


fortCall(objFile,data,results) ==
  tmpFile1 := writeData(generateDataName(),data)
  tmpFile2 := generateResultsName()
  SYSTEM STRCONC(objFile," < ",tmpFile1," > ",tmpFile2)
  results := readData(tmpFile2,results)
  -- SYSTEM STRCONC("rm -f ",tmpFile1," ",tmpFile2)
  PROBE_-FILE(tmpFile1) and DELETE_-FILE(tmpFile1)
  PROBE_-FILE(tmpFile2) and DELETE_-FILE(tmpFile2)
  results

-- Globals
-- $fortranDirectory := nil
-- $fortranLibraries := '"-L/usr/local/lib/f90 -lf90 -L/usr/local/lib -lnag -lm"
-- $fortranTmpDir := '"/tmp/"
-- $addUnderscoreToFortranNames := true
-- $fortranCompilerName := '"f90"

inFirstNotSecond(f,s)==
 [i for i in f | not i in s]

-- Code for use in the Windows version of the AXIOM/NAG interface.

multiToUnivariate f ==
  -- Take an AnonymousFunction, replace the bound variables by references to
  -- elements of a vector, and compile it.
  (first f) ~= "+->" => error "in multiToUnivariate: not an AnonymousFunction"
  if PAIRP CADR f then
    vars := CDADR f -- throw away 'Tuple at start of variable list
  else
    vars := [CADR f]
  body := COPY_-TREE CADDR f
  newVariable := GENSYM()
  for index in 0..#vars-1 repeat
    -- Remember that AXIOM lists, vectors etc are indexed from 1
    body := NSUBST(["elt",newVariable,index+1],vars.(index),body)
  -- We want a Vector DoubleFloat -> DoubleFloat
  target := [["DoubleFloat"],["Vector",["DoubleFloat"]]]
  rest interpret(["ADEF",[newVariable],target,[[],[]],body], nil)

functionAndJacobian f ==
  -- Take a mapping into n functions of n variables, produce code which will
  -- evaluate function and jacobian values.
  (first f) ~= "+->" => error "in functionAndJacobian: not an AnonymousFunction"
  if PAIRP CADR f then
    vars := CDADR f -- throw away 'Tuple at start of variable list
  else
    vars := [CADR f]
  #(vars) ~= #(CDADDR f) =>
    error "number of variables should equal number of functions"
  funBodies := COPY_-TREE CDADDR f
  jacBodies := [:[DF(f,v) for v in vars] for f in funBodies] where
    DF(fn,var) ==
      ["@",["convert",["differentiate",fn,var]],"InputForm"]
  jacBodies := CDDR interpret(
        [["$elt",["List",["InputForm"]],"construct"],:jacBodies], nil)
  newVariable := GENSYM()
  for index in 0..#vars-1 repeat
    -- Remember that AXIOM lists, vectors etc are indexed from 1
    funBodies := NSUBST(["elt",newVariable,index+1],vars.(index),funBodies)
    jacBodies := NSUBST(["elt",newVariable,index+1],vars.(index),jacBodies)
  target := [["Vector",["DoubleFloat"]],["Vector",["DoubleFloat"]],["Integer"]]
  rest interpret(
    ["ADEF",[newVariable,"flag"],target,[[],[],[]],_
            ["IF", ["=","flag",1],_
                   ["vector",["construct",:funBodies]],_
                   ["vector",["construct",:jacBodies]]]], nil)


vectorOfFunctions f ==
  -- Take a mapping into n functions of m variables, produce code which will
  -- evaluate function values.
  (first f) ~= "+->" => error "in vectorOfFunctions: not an AnonymousFunction"
  if PAIRP CADR f then
    vars := CDADR f -- throw away 'Tuple at start of variable list
  else
    vars := [CADR f]
  funBodies := COPY_-TREE CDADDR f
  newVariable := GENSYM()
  for index in 0..#vars-1 repeat
    -- Remember that AXIOM lists, vectors etc are indexed from 1
    funBodies := NSUBST(["elt",newVariable,index+1],vars.(index),funBodies)
  target := [["Vector",["DoubleFloat"]],["Vector",["DoubleFloat"]]]
  rest interpret(
     ["ADEF",[newVariable],target,[[],[]],["vector",["construct",:funBodies]]],
     nil)
