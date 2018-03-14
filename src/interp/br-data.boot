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

lefts u ==
   [x for x in HKEYS  $has_category_hash | rest x = u]


--============================================================================
--              Build Library Database (libdb.text,...)
--============================================================================
--Format for libdb.text:
--  constructors    Cname\#\I\sig \args   \abb \comments (C is C, D, P, X)
--  operations      Op  \#\E\sig \conname\pred\comments (E is one of U/E)
--  attributes      Aname\#\E\args\conname\pred\comments
--  I = <x if exposed><d if category with a default package>
buildLibdb(domainList) ==  --called by make-databases (daase.lisp.pamphlet)
  $OpLst: local := nil
  $AttrLst: local := nil
  $DomLst : local := nil
  $CatLst : local := nil
  $PakLst : local := nil
  $DefLst : local := nil
  $outStream: local := MAKE_-OUTSTREAM '"temp.text"
  --build local libdb if list of domains is given
  if null domainList then
    comments :=
      '"\spad{Union(A,B,...,C)} is a primitive type in FriCAS used to represent objects of type \spad{A} or of type \spad{B} or...or of type \spad{C}."
    writedb
      buildLibdbString ['"dUnion",1,'"x",'"special",'"(A,B,...,C)",'UNION,comments]
    comments :=
      '"\spad{Record(a:A,b:B,...,c:C)} is a primitive type in FriCAS used to represent composite objects made up of objects of type \spad{A}, \spad{B},..., \spad{C} which are indexed by _"keys_" (identifiers) \spad{a},\spad{b},...,\spad{c}."
    writedb
      buildLibdbString ['"dRecord",1,'"x",'"special",'"(a:A,b:B,...,c:C)",'RECORD,comments]
    comments :=
      '"\spad{Mapping(T,S)} is a primitive type in FriCAS used to represent mappings from source type \spad{S} to target type \spad{T}. Similarly, \spad{Mapping(T,A,B)} denotes a mapping from source type \spad{(A,B)} to target type \spad{T}."
    writedb
      buildLibdbString ['"dMapping",1,'"x",'"special",'"(T,S)",'MAPPING,comments]
    comments :=
      '"\spad{Enumeration(a,b,...,c)} is a primitive type in FriCAS used to represent the object composed of the symbols \spad{a},\spad{b},..., and \spad{c}."
    writedb
      buildLibdbString ['"dEnumeration",1,'"x",'"special",'"(a,b,...,c)",'ENUM,comments]
  $conname: local := nil
  $conform: local := nil
  $exposed?:local := nil
  $doc:     local := nil
  $kind:    local := nil
  constructorList := domainList or allConstructors()
  for con in constructorList repeat
    writedb buildLibdbConEntry con
    [attrlist,:oplist] := getConstructorExports $conform
    buildLibOps oplist
    buildLibAttrs attrlist
  SHUT $outStream
  domainList => 'done         --leave new database in temp.text
  OBEY '"sort  _"temp.text_"  > _"libdb.text_""
  RENAME_-FILE('"libdb.text", '"olibdb.text")
  deleteFile '"temp.text"

buildLibdbConEntry conname ==
    NULL GETDATABASE(conname, 'CONSTRUCTORMODEMAP) => nil
    abb:=GETDATABASE(conname,'ABBREVIATION)
    $conname := conname
    conform := GETDATABASE(conname,'CONSTRUCTORFORM) or [conname] --hack for Category,..
    $conform := dbMkForm SUBST('T,"T$",conform)
    null $conform => nil
    $exposed? := (isExposedConstructor conname => '"x"; '"n")
    $doc      := GETDATABASE(conname, 'DOCUMENTATION)
    pname := PNAME conname
    kind  := GETDATABASE(conname,'CONSTRUCTORKIND)
    if kind = 'domain
      and GETDATABASE(conname,'CONSTRUCTORMODEMAP) is [[.,t,:.],:.]
       and t is ['CATEGORY,'package,:.] then kind := 'package
    $kind :=
      pname.(MAXINDEX pname) = char '_& => 'x
      DOWNCASE (PNAME kind).0
    argl := rest $conform
    conComments :=
      LASSOC('constructor,$doc) is [[=nil,:r]] => libdbTrim concatWithBlanks r
      '""
    argpart:= SUBSTRING(form2HtString ['f,:argl],1,nil)
    sigpart:= libConstructorSig $conform
    header := STRCONC($kind,PNAME conname)
    buildLibdbString [header,#argl,$exposed?,sigpart,argpart,abb,conComments]

dbMkForm x == atom x and [x] or x

buildLibdbString [x,:u] ==
  STRCONC(STRINGIMAGE x,"STRCONC"/[STRCONC('"`",STRINGIMAGE y) for y in u])

libConstructorSig [conname,:argl] ==
  [[.,:sig],:.] := GETDATABASE(conname,'CONSTRUCTORMODEMAP)
  formals := TAKE(#argl,$FormalMapVariableList)
  sig := SUBLISLIS(formals,$TriangleVariableList,sig)
  keys := [g(f,sig,i) for f in formals for i in 1..] where
    g(x,u,i) ==  --does x appear in any but i-th element of u?
      or/[CONTAINED(x,y) for y in u for j in 1.. | j ~= i]
  sig := fn SUBLISLIS(argl,$FormalMapVariableList,sig) where
    fn x ==
      atom x => x
      x is ['Join,a,:r] => ['Join,fn a,'etc]
      x is ['CATEGORY,:.] => 'etc
      [fn y for y in x]
  sig := [first sig,:[(k => [":",a,s]; s)
            for a in argl for s in rest sig for k in keys]]
  sigpart:= form2LispString ['Mapping,:sig]
  if null ncParseFromString sigpart then
    sayBrightly ['"Won't parse: ",sigpart]
  sigpart

concatWithBlanks r ==
  r is [head,:tail] =>
    tail => STRCONC(head,'" ",concatWithBlanks tail)
    head
  '""

writedb(u) ==
  not STRINGP u => nil        --skip if not a string
  PRINTEXP(addPatchesToLongLines(u,500),$outStream)
  --positions for tick(1), dashes(2), and address(9), i.e. 12
  TERPRI $outStream

addPatchesToLongLines(s,n) ==
  #s > n => STRCONC(SUBSTRING(s,0,n),
              addPatchesToLongLines(STRCONC('"--",SUBSTRING(s,n,nil)),n))
  s

buildLibOps oplist == for [op,sig,:pred] in oplist repeat buildLibOp(op,sig,pred)

buildLibOp(op,sig,pred) ==
--operations      OKop  \#\sig \conname\pred\comments (K is U or C)
  nsig := SUBLISLIS(rest $conform,$FormalMapVariableList,sig)
  pred := SUBLISLIS(rest $conform,$FormalMapVariableList,pred)
  nsig := SUBST('T,"T$",nsig)   --this ancient artifact causes troubles!
  pred := SUBST('T,"T$",pred)
  sigpart:= form2LispString ['Mapping,:nsig]
  predString := (pred = 'T => '""; form2LispString pred)
  sop :=
    (s := STRINGIMAGE op) = '"One" => '"1"
    s = '"Zero" => '"0"
    s
  header := STRCONC('"o",sop)
  conform:= STRCONC($kind,form2LispString $conform)
  comments:= libdbTrim concatWithBlanks LASSOC(sig,LASSOC(op,$doc))
  checkCommentsForBraces('operation,sop,sigpart,comments)
  writedb
    buildLibdbString [header,# rest sig,$exposed?,sigpart,conform,predString,comments]

libdbTrim s ==
  k := MAXINDEX s
  k < 0 => s
  for i in 0..k repeat
    s.i = $Newline => SETELT(s,i,char '_ )
  trimString s

checkCommentsForBraces(kind,sop,sigpart,comments) ==
  count := 0
  for i in 0..MAXINDEX comments repeat
    c := comments.i
    c = char '_{ => count := count + 1
    c = char '_} =>
      count := count - 1
      count < 0 => missingLeft := true
  if count < 0 or missingLeft then
    tail :=
      kind = 'attribute => [sop,'"(",sigpart,'")"]
      [sop,'": ",sigpart]
    sayBrightly ['"(",$conname,'" documentation) missing left brace--> ",:tail]
  if count > 0 then
    sayBrightly ['"(",$conname,'" documentation) missing right brace--> ",:tail]
  if count ~= 0 or missingLeft then pp comments

buildLibAttrs attrlist ==
  for [name,argl,:pred] in attrlist repeat buildLibAttr(name,argl,pred)

buildLibAttr(name,argl,pred) ==
--attributes      AKname\#\args\conname\pred\comments (K is U or C)
  header := STRCONC('"a",STRINGIMAGE name)
  argPart:= SUBSTRING(form2LispString ['f,:argl],1,nil)
  pred := SUBLISLIS(rest $conform,$FormalMapVariableList,pred)
  predString := (pred = 'T => '""; form2LispString pred)
  header := STRCONC('"a",STRINGIMAGE name)
  conname := STRCONC($kind,form2LispString $conname)
  comments:= concatWithBlanks LASSOC(['attribute,:argl],LASSOC(name,$doc))
  checkCommentsForBraces('attribute,STRINGIMAGE name,argl,comments)
  writedb
    buildLibdbString [header,# argl,$exposed?,argPart,conname,predString,comments]

dbHasExamplePage conname ==
  sname    := STRINGIMAGE conname
  abb      := constructor? conname
  ucname   := UPCASE STRINGIMAGE abb
  pathname :=STRCONC(getEnv '"AXIOM",'"/share/hypertex/pages/",ucname,'".ht")
  isExistingFile pathname => INTERN STRCONC(sname,'"XmpPage")
  nil

dbReadComments(n) ==
  n = 0 => '""
  instream := MAKE_-INSTREAM STRCONC(getEnv('"AXIOM"),'"/algebra/comdb.text")
  FILE_-POSITION(instream,n)
  line := read_line instream
  k := dbTickIndex(line,1,1)
  line := SUBSTRING(line,k + 1,nil)
  while not EOFP instream and (x := read_line instream) and
    (k := MAXINDEX x) and (j := dbTickIndex(x,1,1)) and (j < k) and
      x.(j := j + 1) = char '_- and x.(j := j + 1) = char '_- repeat
        xtralines := [SUBSTRING(x,j + 1,nil),:xtralines]
  SHUT instream
  STRCONC(line, "STRCONC"/NREVERSE xtralines)

dbSplitLibdb() ==
  instream := MAKE_-INSTREAM  '"olibdb.text"
  outstream:= MAKE_-OUTSTREAM '"libdb.text"
  comstream:= MAKE_-OUTSTREAM '"comdb.text"
  PRINTEXP(0,    comstream)
  PRINTEXP($tick,comstream)
  PRINTEXP('"",  comstream)
  TERPRI(comstream)
  while not EOFP instream repeat
    line := read_line instream
    outP := FILE_-POSITION outstream
    comP := FILE_-POSITION comstream
    [prefix,:comments] := dbSplit(line,6,1)
    PRINTEXP(prefix,outstream)
    PRINTEXP($tick ,outstream)
    null comments =>
      PRINTEXP(0,outstream)
      TERPRI(outstream)
    PRINTEXP(comP,outstream)
    TERPRI(outstream)
    PRINTEXP(outP  ,comstream)
    PRINTEXP($tick ,comstream)
    PRINTEXP(first comments,comstream)
    TERPRI(comstream)
    for c in rest comments repeat
      PRINTEXP(outP  ,comstream)
      PRINTEXP($tick ,comstream)
      PRINTEXP(c, comstream)
      TERPRI(comstream)
  SHUT instream
  SHUT outstream
  SHUT comstream
  deleteFile '"olibdb.text"

dbSplit(line,n,k) ==
  k := charPosition($tick,line,k + 1)
  n = 1 => [SUBSTRING(line,0,k),:dbSpreadComments(SUBSTRING(line,k + 1,nil),0)]
  dbSplit(line,n - 1,k)

dbSpreadComments(line,n) ==
  line = '"" => nil
  k := charPosition(char '_-,line,n + 2)
  k >= MAXINDEX line => [SUBSTRING(line,n,nil)]
  line.(k + 1) ~= char '_- =>
    u := dbSpreadComments(line,k)
    [STRCONC(SUBSTRING(line,n,k - n),first u),:rest u]
  [SUBSTRING(line,n,k - n),:dbSpreadComments(SUBSTRING(line,k,nil),0)]

--============================================================================
--                  Build Glossary
--============================================================================
buildGloss() ==  --called by buildDatabase (database.boot)
--starting with gloss.text, build glosskey.text and glossdef.text
  $constructorName : local := nil
  $exposeFlag : local := true
  $outStream: local := MAKE_-OUTSTREAM '"temp.text"
  $x : local := nil
  $attribute? : local := true     --do not surround first word
  pathname := '"gloss.text"
  instream := MAKE_-INSTREAM pathname
  keypath  := '"glosskey.text"
  OBEY STRCONC('"rm -f ",keypath)
  outstream:= MAKE_-OUTSTREAM keypath
  htpath   := '"gloss.ht"
  OBEY STRCONC('"rm -f ",htpath)
  htstream:= MAKE_-OUTSTREAM htpath
  defpath  := '"glossdef.text"
  defstream:= MAKE_-OUTSTREAM defpath
  pairs := getGlossLines instream
  PRINTEXP('"\begin{page}{GlossaryPage}{G l o s s a r y}\beginscroll\beginmenu",htstream)
  for [name,:line] in pairs repeat
    outP  := FILE_-POSITION outstream
    defP  := FILE_-POSITION defstream
    lines := spreadGlossText transformAndRecheckComments(name,[line])
    PRINTEXP(name, outstream)
    PRINTEXP($tick,outstream)
    PRINTEXP(defP, outstream)
    TERPRI(outstream)
--  PRINTEXP('"\item\newline{\em \menuitemstyle{}}\tab{0}{\em ",htstream)
    PRINTEXP('"\item\newline{\em \menuitemstyle{}}{\em ",htstream)
    PRINTEXP(name,        htstream)
    PRINTEXP('"}\space{}",htstream)
    TERPRI(htstream)
    for x in lines repeat
      PRINTEXP(outP, defstream)
      PRINTEXP($tick,defstream)
      PRINTEXP(x,    defstream)
      TERPRI defstream
    PRINTEXP("STRCONC"/lines,htstream)
    TERPRI htstream
  PRINTEXP('"\endmenu\endscroll",htstream)
  PRINTEXP('"\lispdownlink{Search}{(|htGloss| _"\stringvalue{pattern}_")} for glossary entry matching \inputstring{pattern}{24}{*}",htstream)
  PRINTEXP('"\end{page}",htstream)
  SHUT instream
  SHUT outstream
  SHUT defstream
  SHUT htstream
  SHUT $outStream

spreadGlossText(line) ==
--this function breaks up a line into chunks
--eventually long line is put into gloss.text as several chunks as follows:
----- key1`this is the first chunk
----- XXX`and this is the second
----- XXX`and this is the third
----- key2`and this is the fourth
--where XXX is the file position of key1
--this is because grepping will only pick up the first 512 characters
  line = '"" => nil
  MAXINDEX line > 500 => [SUBSTRING(line,0,500),:spreadGlossText(SUBSTRING(line,500,nil))]
  [line]

getGlossLines instream ==
--instream has text of the form:
----- key1`this is the first line
----- and this is the second
----- key2'and this is the third
--result is
----- key1'this is the first line and this is the second
----- key2'and this is the third
  keys := nil
  text := nil
  lastLineHadTick := false
  while not EOFP instream repeat
    line := read_line instream
    #line = 0 => 'skip
    n := charPosition($tick,line,0)
    last := IFCAR text
    n > MAXINDEX line =>  --this line is continuation of previous line; concat it
      fill :=
        #last = 0 =>
          lastLineHadTick => '""
          '"\blankline "
        #last > 0 and last.(MAXINDEX last) ~= $charBlank => $charBlank
        '""
      lastLineHadTick := false
      text := [STRCONC(last,fill,line),:rest text]
    lastLineHadTick := true
    keys := [SUBSTRING(line,0,n),:keys]
    text := [SUBSTRING(line,n + 1,nil),:text]
  ASSOCRIGHT listSort(function GLESSEQP,[[DOWNCASE key,key,:def] for key in keys for def in text])
  --this complication sorts them after lower casing the keys

--============================================================================
--                  Build Users HashTable
-- This database is written out as USERS.DATABASE (database.boot) and read using
-- function getUsersOfConstructor. See functions whoUses and kcuPage in browser.
--============================================================================
mkUsersHashTable() ==  --called by make-databases (daase.lisp.pamphlet)
  $usersTb := MAKE_-HASH_-TABLE()
  for x in allConstructors() repeat
    for conform in getImports x repeat
      name := opOf conform
      if not MEMQ(name,'(QUOTE)) then
        HPUT($usersTb,name,insert(x,HGET($usersTb,name)))
  for k in HKEYS $usersTb repeat
    HPUT($usersTb,k,listSort(function GLESSEQP,HGET($usersTb,k)))
  for x in allConstructors() | isDefaultPackageName x repeat
    HPUT($usersTb,x,getDefaultPackageClients x)
  $usersTb

getDefaultPackageClients con ==  --called by mkUsersHashTable
  catname := INTERN SUBSTRING(s := PNAME con,0,MAXINDEX s)
  for [catAncestor,:.] in childrenOf([catname]) repeat
    pakname := INTERN STRCONC(PNAME catAncestor,'"&")
    if getCDTEntry(pakname,true) then acc := [pakname,:acc]
    acc := union([CAAR x for x in domainsOf([catAncestor],nil)],acc)
  listSort(function GLESSEQP,acc)

--============================================================================
--               Build Dependents Hashtable
-- This hashtable is written out by database.boot as DEPENDENTS.DATABASE
-- and read back in by getDependentsOfConstructor (see database.boot)
-- This information is used by function kcdePage when a user asks for the
-- dependents of a constructor.
--============================================================================
mkDependentsHashTable() == --called by make-databases (database.boot)
  $depTb := MAKE_-HASH_-TABLE()
  for nam in allConstructors() repeat
    for con in getArgumentConstructors nam repeat
      HPUT($depTb,con,[nam,:HGET($depTb,con)])
  for k in HKEYS $depTb repeat
    HPUT($depTb,k,listSort(function GLESSEQP,HGET($depTb,k)))
  $depTb

getArgumentConstructors con == --called by mkDependentsHashTable
  argtypes := IFCDR IFCAR getConstructorModemap con or return nil
  fn argtypes where
    fn(u) == "union"/[gn x for x in u]
    gn(x) ==
      atom x => nil
      x is ['Join,:r] => fn(r)
      x is ['CATEGORY,:.] => nil
      constructor? first x => [first x,:fn rest x]
      fn rest x

getImports conname == --called by mkUsersHashTable
  conform := GETDATABASE(conname,'CONSTRUCTORFORM)
  infovec := dbInfovec conname or return nil
  template := infovec.0
  u := [import(i,template)
          for i in 5..(MAXINDEX template) | test]  where
    test == template.i is [op,:.] and IDENTP op
              and not MEMQ(op,'(Mapping Union Record Enumeration CONS QUOTE local))
    import(x,template) ==
      x is [op,:args] =>
        op = 'QUOTE or op = 'NRTEVAL => first args
        op = 'local => first args
        op = 'Record =>
          ['Record,:[[":",CADR y,import(CADDR y,template)] for y in args]]

--TTT next three lines: handles some tagged/untagged Union case.
        op = 'Union=>
          args is [['_:,:x1],:x2] =>
--          CAAR args = '_: => -- tagged!
               ['Union,:[[":",CADR y,import(CADDR y,template)] for y in args]]
          [op,:[import(y,template) for y in args]]

        [op,:[import(y,template) for y in args]]
      INTEGERP x => import(template.x,template)
      x = '$ => '$
      x = "$$" => "$$"
      STRINGP x => x
      systemError '"bad argument in template"
  listSort(function GLESSEQP,SUBLISLIS(rest conform,$FormalMapVariableList,u))


--============================================================================
--                 Get Hierarchical Information
--============================================================================
getParentsFor(cname,formalParams,constructorCategory) ==
--called by compDefineFunctor1
  acc := nil
  formals := TAKE(#formalParams,$TriangleVariableList)
  constructorForm := GETDATABASE(cname, 'CONSTRUCTORFORM)
  for x in folks constructorCategory repeat
    x := SUBLISLIS(formalParams,formals,x)
    x := SUBLISLIS(IFCDR constructorForm,formalParams,x)
    acc := [:explodeIfs x,:acc]
  NREVERSE acc

parentsOf con == --called by kcpPage, ancestorsRecur
  if null BOUNDP '$parentsCache then SETQ($parentsCache,MAKE_-HASHTABLE 'ID)
  HGET($parentsCache,con) or
    parents := getParentsForDomain con
    HPUT($parentsCache,con,parents)
    parents

parentsOfForm [op,:argl] ==
  parents := parentsOf op
  null argl or argl = (newArgl := rest GETDATABASE(op,'CONSTRUCTORFORM)) =>
    parents
  SUBLISLIS(argl, newArgl, parents)

getParentsForDomain domname  == --called by parentsOf
  acc := nil
  for x in folks GETDATABASE(domname,'CONSTRUCTORCATEGORY) repeat
    x :=
      GETDATABASE(domname,'CONSTRUCTORKIND) = 'category =>
        sublisFormal(IFCDR getConstructorForm domname,x,$TriangleVariableList)
      sublisFormal(IFCDR getConstructorForm domname,x)
    acc := [:explodeIfs x,:acc]
  NREVERSE acc

explodeIfs x == main where  --called by getParents, getParentsForDomain
  main ==
    x is ['IF,p,a,b] => fn(p,a,b)
    [[x,:true]]
  fn(p,a,b) ==
    [:"append"/[gn(p,y) for y in a],:"append"/[gn(['NOT,p],y) for y in b]]
  gn(p,a) ==
    a is ['IF,q,b,:.] => fn(MKPF([p,q],'AND),b,nil)
    [[a,:p]]

folks u == --called by getParents and getParentsForDomain
  atom u => nil
  u is [op,:v] and MEMQ(op,'(Join PROGN))
    or u is ['CATEGORY,a,:v] => "append"/[folks x for x in v]
  u is ['SIGNATURE,:.] => nil
  u is ['TYPE,:.] => nil
  u is ['ATTRIBUTE,a] =>
    PAIRP a and constructor? opOf a => folks a
    nil
  u is ['IF,p,q,r] =>
    q1 := folks q
    r1 := folks r
    q1 or r1 => [['IF,p,q1,r1]]
    nil
  [u]

descendantsOf(conform,domform) ==  --called by kcdPage
  'category = GETDATABASE((conname := opOf conform),'CONSTRUCTORKIND) =>
    cats := catsOf(conform,domform)
    [op,:argl] := conform
    null argl or argl = (newArgl := rest (GETDATABASE(op,'CONSTRUCTORFORM)))
        => cats
    SUBLISLIS(argl, newArgl, cats)
  'notAvailable

childrenOf conform ==
  [pair for pair in descendantsOf(conform,nil) |
    childAssoc(conform,parentsOfForm first pair)]

childAssoc(form,alist) ==
  null (argl := rest form) => assoc(form, alist)
  u := assocCar(opOf form, alist) => childArgCheck(argl, rest first u) and u
  nil

assocCar(x, al) == or/[pair for pair in al | x = CAAR pair]

childArgCheck(argl, nargl) ==
  and/[fn for x in argl for y in nargl for i in 0..] where
    fn ==
      x = y or constructor? opOf y => true
      isSharpVar y => i = POSN1(y, $FormalMapVariableList)
      false

--computeDescendantsOf cat ==
--dynamically generates descendants
--  hash := MAKE_-HASHTABLE 'UEQUAL
--  for [child,:pred] in childrenOf cat repeat
--    childForm := getConstructorForm child
--    HPUT(hash,childForm,pred)
--    for [form,:pred] in descendantsOf(childForm,nil) repeat
--      newPred :=
--        oldPred := HGET(hash,form) => quickOr(oldPred,pred)
--        pred
--      HPUT(hash,form,newPred)
--  mySort [[key,:HGET(hash,key)] for key in HKEYS hash]

ancestorsOf(conform,domform) ==  --called by kcaPage, originsInOrder,...
  'category = GETDATABASE((conname := opOf conform),'CONSTRUCTORKIND) =>
       alist := GETDATABASE(conname,'ANCESTORS)
       argl := IFCDR domform or IFCDR conform
       [pair for [a,:b] in alist | pair] where pair ==
         left :=  sublisFormal(argl,a)
         right := sublisFormal(argl,b)
         if domform then right := simpHasPred right
         null right => false
         [left,:right]
  computeAncestorsOf(conform,domform)

computeAncestorsOf(conform,domform) ==
  $done: local := MAKE_-HASHTABLE 'UEQUAL
  $if:   local := MAKE_-HASHTABLE 'ID
  ancestorsRecur(conform,domform,true,true)
  acc := nil
  for op in listSort(function GLESSEQP,HKEYS $if) repeat
    for pair in HGET($if,op) repeat acc := [pair,:acc]
  NREVERSE acc

ancestorsRecur(conform,domform,pred,firstTime?) == --called by ancestorsOf
  op      := opOf conform
  pred = HGET($done,conform) => nil   --skip if already processed
  parents :=
    firstTime? and ($insideCategoryIfTrue or $insideFunctorIfTrue) =>
      $lisplibParents
    parentsOf op
  originalConform :=
    firstTime? and ($insideCategoryIfTrue or $insideFunctorIfTrue) => $form
    getConstructorForm op
  if conform ~= originalConform then
    parents := SUBLISLIS(IFCDR conform,IFCDR originalConform,parents)
  for [newform,:p] in parents repeat
    if domform and rest domform then
      newdomform := SUBLISLIS(rest domform,rest conform,newform)
      p          := SUBLISLIS(rest domform,rest conform,p)
    newPred := quickAnd(pred,p)
    ancestorsAdd(simpHasPred newPred,newdomform or newform)
    ancestorsRecur(newform,newdomform,newPred,false)
  HPUT($done,conform,pred)                  --mark as already processed

ancestorsAdd(pred,form) == --called by ancestorsRecur
  null pred => nil
  op := IFCAR form or form
  alist := HGET($if,op)
  existingNode := assoc(form,alist) =>
    RPLACD(existingNode, quickOr(rest existingNode, pred))
  HPUT($if,op,[[form,:pred],:alist])

domainsOf(conform, domname) ==
  conname := opOf conform
  u := [key for key in HKEYS $has_category_hash
    | key is [anc,: =conname]]
  --u is list of pairs (a . b) where b = conname
  --we sort u then replace each b by the predicate for which this is true
  s := listSort(function GLESSEQP,COPY u)
  s := [[first pair, :GETDATABASE(pair, 'HASCATEGORY)] for pair in s]
  transKCatAlist(conform,domname,listSort(function GLESSEQP,s))

catsOf(conform, domname) ==
  conname := opOf conform
  alist := nil
  for key in allConstructors() repeat
    for item in GETDATABASE(key,'ANCESTORS) | conname = CAAR item repeat
      [[op,:args],:pred] := item
      newItem :=
        args => [[args,:pred],:LASSOC(key,alist)]
        pred
      alist := insertShortAlist(key,newItem,alist)
  transKCatAlist(conform,domname,listSort(function GLESSEQP,alist))

transKCatAlist(conform,domname,s) == main where
  main ==
    domname => --accept only exact matches after substitution
      domargs := rest domname
      acc := nil
      rest conform =>
        for pair in s repeat --pair has form [con,[conargs,:pred],...]]
          leftForm := getConstructorForm first pair
          for (ap := [args, :pred]) in rest pair repeat
            match? :=
              domargs = args => true
              HAS_SHARP_VAR args => domargs = sublisFormal(IFCDR domname, args)
              nil
            null match? => 'skip
            npred := sublisFormal(IFCDR leftForm, pred)
            acc := [[leftForm,:npred],:acc]
        NREVERSE acc
      --conform has no arguments so each pair has form [con,:pred]
      for pair in s repeat
        leftForm := getConstructorForm first pair or systemError nil
        RPLACA(pair,leftForm)
        RPLACD(pair, sublisFormal(IFCDR leftForm, rest pair))
      s
    --no domname, so look for special argument combinations
    acc := nil
    IFCDR conform =>
      farglist := TAKE(#rest conform,$FormalMapVariableList)
      for pair in s repeat --pair has form [con,[conargs,:pred],...]]
        leftForm := getConstructorForm first pair
        for (ap := [args, :pred]) in rest pair repeat
          hasArgsForm? := args ~= farglist
          npred := sublisFormal(IFCDR leftForm, pred)
          if hasArgsForm? then
            subargs := sublisFormal(IFCDR leftForm, args)
            hpred :=
--            $hasArgsList => mkHasArgsPred subargs
              ['hasArgs,:subargs]
            npred := quickAnd(hpred,npred)
          acc := [[leftForm,:npred],:acc]
      NREVERSE acc
    for pair in s repeat --pair has form [con,:pred]
      leftForm := getConstructorForm first pair
      RPLACA(pair,leftForm)
      RPLACD(pair, sublisFormal(IFCDR leftForm, rest pair))
    s

mkHasArgsPred subargs ==
--$hasArgsList gives arguments of original constructor,e.g. LODO(A,M)
--M is required to be Join(B,...); in looking for the domains of B
--  we can find that if B has special value C, it can
  systemError subargs

sublisFormal(args,exp,:options) == main where
  main ==  --use only on LIST structures; see also sublisFormalAlist
    $formals: local := IFCAR options or $FormalMapVariableList
    null args => exp
    sublisFormal1(args,exp,#args - 1)
  sublisFormal1(args,x,n) ==    --[sublisFormal1(args,y) for y in x]
    x is [.,:.] =>
      acc := nil
      y := x
      while null atom y repeat
        acc := [sublisFormal1(args,QCAR y,n),:acc]
        y := QCDR y
      r := NREVERSE acc
      if y then
        nd := LASTNODE r
        RPLACD(nd,sublisFormal1(args,y,n))
      r
    IDENTP x =>
      j := or/[i for f in $formals for i in 0..n | EQ(f,x)] =>
          args.j
      x
    x

--=======================================================================
--            Build Table of Lower Case Constructor Names
--=======================================================================

buildDefaultPackageNamesHT() ==
  $defaultPackageNamesHT := MAKE_-HASH_-TABLE()
  for nam in allConstructors() | isDefaultPackageName nam repeat
    HPUT($defaultPackageNamesHT,nam,true)
  $defaultPackageNamesHT

$defaultPackageNamesHT := buildDefaultPackageNamesHT()

--=======================================================================
--            Code for Private Libdbs
--=======================================================================
-- $createLocalLibDb := false

extendLocalLibdb conlist ==   --  called by astran
  not $createLocalLibDb => nil
  null conlist => nil
  buildLibdb conlist          --> puts datafile into temp.text
  $newConstructorList := union(conlist, $newConstructorList)
  localLibdb := '"libdb.text"
  not PROBE_-FILE '"libdb.text" =>
    RENAME_-FILE('"temp.text",'"libdb.text")
  oldlines := purgeNewConstructorLines(dbReadLines localLibdb, conlist)
  newlines := dbReadLines '"temp.text"
  dbWriteLines(MSORT union(oldlines,newlines), '"libdb.text")
  PROBE_-FILE '"temp.text" => deleteFile '"temp.text"
