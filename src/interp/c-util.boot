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

$genSDVar := 0

$previousTime := 0

$warningStack := []
$semanticErrorStack := []

DROP(n, l) ==
    n >= 0 =>
        while n > 0 repeat
            l := rest(l)
        l
    TAKE(#l + n, l)

TAKE(n, l) ==
    n >= 0 => [x for x in l for i in 1..n]
    DROP(#l + n, l)

displaySemanticErrors() ==
  n:= #($semanticErrorStack:= REMDUP $semanticErrorStack)
  n=0 => nil
  l:= NREVERSE $semanticErrorStack
  $semanticErrorStack:= nil
  sayBrightly bright '"  Semantic Errors:"
  displaySemanticError(l,CUROUTSTREAM)
  sayBrightly '" "
  displayWarnings()

displaySemanticError(l,stream) ==
  for x in l for i in 1.. repeat
    sayBrightly2(['"      [", i, '"] ", :first x], stream)

displayWarnings() ==
  n:= #($warningStack:= REMDUP $warningStack)
  n=0 => nil
  sayBrightly bright '"  Warnings:"
  l := NREVERSE $warningStack
  displayWarning(l,CUROUTSTREAM)
  $warningStack:= nil
  sayBrightly '" "

displayWarning(l,stream) ==
  for x in l for i in 1.. repeat
    sayBrightly2(['"      [", i, '"] ", :x], stream)

displayComp level ==
  $bright:= " << "
  $dim:= " >> "
  if $insideCapsuleFunctionIfTrue=true then
    sayBrightly ['"error in function",'%b,$op,'%d,'%l]
  pp removeZeroOne mkErrorExpr level
  sayBrightly ['"****** level",'%b,level,'%d,'" ******"]
  [x, m, f, $exitModeStack] := $s.(level - 1)
  SAY('"x:= ", x)
  SAY('"m:= ", m)
  SAY '"f:="
  limited_print1_stdout(f)
  nil

mkErrorExpr level ==
  bracket ASSOCLEFT DROP(level-#$s,$s) where
    bracket l ==
      #l<2 => l
      l is [a,b] =>
        highlight(b,a) where
          highlight(b,a) ==
            atom b =>
              substitute(var,b,a) where
                var:= INTERN STRCONC(STRINGIMAGE $bright,STRINGIMAGE b,STRINGIMAGE $dim)
            highlight1(b,a) where
              highlight1(b,a) ==
                atom a => a
                a is [ =b,:c] => [$bright,b,$dim,:c]
                [highlight1(b,first a),:highlight1(b,rest a)]
      substitute(bracket rest l,first rest l,first l)

errorRef s == stackWarning ['%b,s,'%d,'"has no value"]

unErrorRef s == unStackWarning ['%b,s,'%d,'"has no value"]

--% ENVIRONMENT FUNCTIONS

consProplistOf(var,proplist,prop,val) ==
  semchkProplist(var, proplist, prop)
  $InteractiveMode and (u:= assoc(prop,proplist)) =>
    RPLACD(u,val)
    proplist
  [[prop,:val],:proplist]

warnLiteral x ==
    stackWarning(['%b,x,'%d, '"is BOTH a variable and a literal"])

intersectionEnvironment(e,e') ==
  ce:= makeCommonEnvironment(e,e')
  ic := intersectionContour(deltaContour(e, ce), deltaContour(e', ce), ce)
  e'':= (ic => addContour(ic,ce); ce)

deltaContour([il1, :el],[il2, :el']) ==
  not el=el' => systemError '"deltaContour" --a cop out for now
  n1 := #il1
  n2 := #il2
  dl := []
  for i in 1..(n1 - n2) repeat
      dl := cons(first(il1), dl)
      il1 := rest(il1)
  c1 := first(il1)
  c2 := first(il2)
  rest(il1) ~= rest(il2) => systemError '"deltaContour 2" --a cop out for now
  cd := [first x for x in tails c1 while (x~=c2)]
  dl := cons(cd, dl)
  res0 := []
  for l in dl repeat
      res0 := APPEND(l, res0)
  res := eliminateDuplicatePropertyLists res0 where
    eliminateDuplicatePropertyLists contour ==
      contour is [[x,:.],:contour'] =>
        LASSOC(x,contour') =>
                               --save some CONSing if possible
          [first contour,:DELLASOS(x,eliminateDuplicatePropertyLists contour')]
        [first contour,:eliminateDuplicatePropertyLists contour']
      nil
  res

intersectionContour(c, c', ce) ==
  $var: local := nil
  computeIntersection(c, c', ce) where
    computeIntersection(c, c', ce) ==
      varlist:= REMDUP ASSOCLEFT c
      varlist':= REMDUP ASSOCLEFT c'
      interVars:= intersection(varlist,varlist')
      unionVars:= union(varlist,varlist')
      diffVars:= setDifference(unionVars,interVars)
      modeAssoc := buildModeAssoc(diffVars, c, c', ce)
      [:modeAssoc,:
        [[x,:proplist]
          for [x,:y] in c | member(x,interVars) and
            (proplist := interProplist(y, LASSOC($var := x, c'), ce))]]
    interProplist(p, p', ce) ==
                            --p is new proplist; p' is old one
        [:modeCompare(p, p', ce), :[pair' for pair in p |
               (pair' := compare(pair, p', ce))]]
    buildModeAssoc(varlist, c, c', ce) ==
      [[x, :mp] for x in varlist |
          (mp := modeCompare(LASSOC(x, c), LASSOC(x, c'), ce))]
    compare(pair is [prop,:val], p', ce) ==
      --1. if the property-value pair are identical, accept it immediately
      pair=(pair':= assoc(prop,p')) => pair
      --2. if property="value" and modes are unifiable, give intersection
      --       property="value" but value=genSomeVariable)()
      (val':= IFCDR pair') and prop = "value" and
        (m:= unifiable(val.mode, val'.mode, ce)) =>
            ["value",genSomeVariable(), m, nil]
            --this tells us that an undeclared variable received
            --two different values but with identical modes
      --3. property="mode" is covered by modeCompare
      prop="mode" => nil
    modeCompare(p, p', ce) ==
      pair:= assoc("mode",p) =>
        pair':= assoc("mode",p') =>
          m'' := unifiable(rest pair, rest pair', ce) => LIST ["mode", :m'']
          stackWarning(['%b,$var,'%d,'"has two modes: "])
        LIST ["conditionalmode",:rest pair]
        --LIST pair
      pair':= assoc("mode",p') => LIST ["conditionalmode",:rest pair']
    unifiable(m1, m2, ce) ==
      m1=m2 => m1
        --we may need to add code to coerce up to tagged unions
        --but this can not be done here, but should be done by compIf
      m:=
        m1 is ["Union",:.] =>
          m2 is ["Union", :.] => ["Union", :set_sum(rest m1, rest m2)]
          ["Union", :set_sum(rest m1, [m2])]
        m2 is ["Union",:.] => ["Union", :set_sum(rest m2, [m1])]
        ["Union",m1,m2]
      for u in getDomainsInScope ce repeat
        if u is ["Union",:u'] and (and/[member(v,u') for v in rest m]) then
          return m
        --this loop will return NIL if not satisfied

addContour(c,E is [cur,:tail]) ==
  [NCONC(fn(c,E),cur),:tail] where
    fn(c,e) ==
        for [x,:proplist] in c repeat
           fn1(x,proplist,getProplist(x,e)) where
              fn1(x,p,ee) ==
                for pv in p repeat fn3(x,pv,ee) where
                 fn3(x,pv,e) ==
                   [p,:v]:=pv
                   if member(x,$getPutTrace) then
                     pp([x,"has",pv])
                   if p="conditionalmode" then
                     RPLACA(pv,"mode")
                     --check for conflicts with earlier mode
                     if vv:=LASSOC("mode",e) then
                        if v ~=vv then
                          stackWarning ['"The conditional modes ",
                                     v,'" and ",vv,'" conflict"]
        LIST c

makeCommonEnvironment(e,e') ==
  interE makeSameLength(e,e') where  --$ie:=
    interE [e,e'] ==
      rest e=rest e' => [interLocalE makeSameLength(first e,first e'),:rest e]
      interE [rest e,rest e']
    interLocalE [le,le'] ==
      rest le=rest le' =>
        [interC makeSameLength(first le,first le'),:rest le]
      interLocalE [rest le,rest le']
    interC [c,c'] ==
      c=c' => c
      interC [rest c,rest c']
    makeSameLength(x,y) ==
      fn(x,y,#x,#y) where
        fn(x,y,nx,ny) ==
          nx>ny => fn(rest x,y,nx-1,ny)
          nx<ny => fn(x,rest y,nx,ny-1)
          [x,y]

printEnv E ==
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      SAY('"******CONTOUR ",j,'", LEVEL ",i,'":******")
      for z in y repeat
        TERPRI()
        SAY('"Properties Of: ",first z)
        for u in rest z repeat
          PRIN0 first u
          printString '": "
          PRETTYPRINT tran(rest u,first u) where
            tran(val,prop) ==
              prop="value" => DROP(-1,val)
              val

prEnv E ==
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      SAY('"******CONTOUR ",j,'", LEVEL ",i,'":******")
      for z in y | not LASSOC("modemap",rest z) repeat
        TERPRI()
        SAY('"Properties Of: ",first z)
        for u in rest z repeat
          PRIN0 first u
          printString '": "
          PRETTYPRINT tran(rest u,first u) where
            tran(val,prop) ==
              prop="value" => DROP(-1,val)
              val

prModemaps E ==
  listOfOperatorsSeenSoFar:= nil
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      for z in y | null member(first z,listOfOperatorsSeenSoFar) and
        (modemap:= LASSOC("modemap",rest z)) repeat
          listOfOperatorsSeenSoFar:= [first z,:listOfOperatorsSeenSoFar]
          TERPRI()
          PRIN0 first z
          printString '": "
          PRETTYPRINT modemap

prTriple T ==
   SAY '"Code:"
   pp T.0
   SAY '"Mode:"
   pp T.1

TrimCF() ==
  new:= nil
  old:= CAAR $CategoryFrame
  for u in old repeat
    if not ASSQ(first u,new) then
      uold:= rest u
      unew:= nil
      for v in uold repeat if not ASSQ(first v,unew) then unew:= [v,:unew]
      new:= [[first u,:NREVERSE unew],:new]
  $CategoryFrame:= [[NREVERSE new]]
  nil


--% PREDICATES


isConstantId(name,e) ==
  IDENTP name =>
    pl:= getProplist(name,e) =>
      (LASSOC("value",pl) or LASSOC("mode",pl) => false; true)
    true
  false

isFalse() == nil

isFluid s == SYMBOLP(s) and #(n := PNAME(s)) > 0 and "$" = n.0

isFunction(x,e) ==
    get(x, "modemap", e) or GETL(x, "comp_special") or x = "case"
      or getmode(x, e) is ["Mapping", :.]

isLiteral(x,e) == get(x,"isLiteral",e)

makeLiteral(x,e) == put(x,"isLiteral","true",e)

isSomeDomainVariable s ==
  IDENTP s and #(x:= PNAME s)>2 and x.(0)="#" and x.(1)="#"

is_integer_subset(s, t) ==
    t = "Integer" =>
        s = "PositiveInteger" => [">", "*", 0]
        s = "NonNegativeInteger" => [">=", "*", 0]
        s = "SingleInteger" => ["SINTP", "*"]
    t = "NonNegativeInteger" and s = "PositiveInteger" => [">", "*", 0]
    false

isSubset(x,y,e) ==
  x = "%" and y = "Rep" or x = y or is_integer_subset(opOf(x), opOf(y)) or
      LASSOC(opOf(x), get(opOf(y), "SubDomain", e)) or
        opOf(y) = 'Type

isDomainInScope(domain,e) ==
  domainList:= getDomainsInScope e
  atom domain =>
    MEMQ(domain,domainList) => true
    not IDENTP domain or isSomeDomainVariable domain => true
    false
  (name:= first domain)="Category" => true
  ASSQ(name,domainList) => true
  isFunctor name => false
  true --is not a functor

isSymbol x == IDENTP x

isSimple x ==
  atom x => true
  x is [op,:argl] and
    isSideEffectFree op and (and/[isSimple y for y in argl])

isSideEffectFree op ==
  constructor? op or member(op,$SideEffectFreeFunctionList) or
    op is ["Sel", ., op'] and isSideEffectFree op'

isAlmostSimple x ==
  --returns (<new predicate> . <list of assignments>) or nil
  $assignmentList: local --$assigmentList is only used in this function
  transform:=
    fn x where
      fn x ==
        atom x or null rest x => x
        [op,y,:l]:= x
        op="has" => x
        op="is" => x
        op = ":=" =>
          IDENTP y => (setAssignment LIST x; y)
          true => (setAssignment [[":=", g := genVariable(), :l],
                                  [":=", y, g]]; g)
        isSideEffectFree op => [op, :mapInto(rest x, function fn)]
        true => $assignmentList:= "failed"
      setAssignment x ==
        $assignmentList="failed" => nil
        $assignmentList:= [:$assignmentList,:x]
  $assignmentList="failed" => nil
  wrapSEQExit [:$assignmentList,transform]

incExitLevel u ==
  adjExitLevel(u,1,1)
  u

decExitLevel u ==
  (adjExitLevel(u,1,-1); removeExit0 u) where
    removeExit0 x ==
      atom x => x
      x is ["exit",0,u] => removeExit0 u
      [removeExit0 first x,:removeExit0 rest x]

adjExitLevel(x,seqnum,inc) ==
  atom x => x
  x is [op,:l] and MEMQ(op,'(SEQ REPEAT COLLECT)) =>
    for u in l repeat adjExitLevel(u,seqnum+1,inc)
  x is ["exit",n,u] =>
    (adjExitLevel(u,seqnum,inc); seqnum>n => x; rplac(CADR x,n+inc))
  x is [op,:l] => for u in l repeat adjExitLevel(u,seqnum,inc)

wrapSEQExit l ==
  null rest l => first l
  [:c,x]:= [incExitLevel u for u in l]
  ["SEQ",:c,["exit",1,x]]


--% UTILITY FUNCTIONS

removeEnv t == [t.expr,t.mode,$EmptyEnvironment]  -- t is a triple

makeNonAtomic x ==
  atom x => [x]
  x

flatten(l,key) ==
  null l => nil
  first l is [k,:r] and k=key => [:r,:flatten(rest l,key)]
  [first l,:flatten(rest l,key)]

genDomainVar() ==
  $Index:= $Index+1
  INTERNL1('"#D", STRINGIMAGE($Index))

genVariable() ==
  INTERNL1('"#G", STRINGIMAGE($genSDVar := $genSDVar + 1))

genSomeVariable() ==
  INTERNL1('"##", STRINGIMAGE($genSDVar := $genSDVar + 1))

listOfIdentifiersIn x ==
  IDENTP x => [x]
  x is [op,:l] => REMDUP ("append"/[listOfIdentifiersIn y for y in l])
  nil

mapInto(x,fn) == [FUNCALL(fn,y) for y in x]

numOfOccurencesOf(x,y) ==
  fn(x,y,0) where
    fn(x,y,n) ==
      null y => 0
      x=y => n+1
      atom y => n
      fn(x,first y,n)+fn(x,rest y,n)

compilerMessage x ==
  $PrintCompilerMessageIfTrue => APPLY("SAY",x)

printDashedLine() ==
  SAY
   '"--------------------------------------------------------------------------"

stackSemanticError(msg,expr) ==
  if $insideCapsuleFunctionIfTrue then msg:= [$op,": ",:msg]
  if atom msg then msg:= LIST msg
  entry:= [msg,expr]
  if not member(entry,$semanticErrorStack) then $semanticErrorStack:=
    [entry,:$semanticErrorStack]
  $scanIfTrue and $insideCapsuleFunctionIfTrue=true and #$semanticErrorStack-
    $initCapsuleErrorCount>3 => THROW("compCapsuleBody",nil)
  nil

stackWarning msg ==
  if $insideCapsuleFunctionIfTrue then msg:= [$op,": ",:msg]
  if not member(msg,$warningStack) then $warningStack:= [msg,:$warningStack]
  nil

unStackWarning msg ==
  if $insideCapsuleFunctionIfTrue then msg:= [$op,": ",:msg]
  $warningStack:= NREMOVE($warningStack, msg)
  nil

stackMessage msg ==
  $compErrorMessageStack:= [msg,:$compErrorMessageStack]
  nil

stackMessageIfNone msg ==
  --used in situations such as compForm where the earliest message is wanted
  if null $compErrorMessageStack then $compErrorMessageStack:=
    [msg,:$compErrorMessageStack]
  nil

stackAndThrow msg ==
  $compErrorMessageStack:= [msg,:$compErrorMessageStack]
  THROW("compOrCroak",nil)

printString x == PRINTEXP (STRINGP x => x; PNAME x)

printAny x == if atom x then printString x else PRIN0 x

printSignature(before,op,[target,:argSigList]) ==
  printString before
  printString op
  printString '": _("
  if argSigList then
    printAny first argSigList
    for m in rest argSigList repeat (printString '","; printAny m)
  printString '"_) -> "
  printAny target
  TERPRI()

pmatch(s,p) == pmatchWithSl(s,p,"ok")

pmatchWithSl(s,p,al) ==
  s=$EmptyMode => nil
  s=p => al
  v:= assoc(p,al) => s=rest v or al
  MEMQ(p,$PatternVariableList) => [[p,:s],:al]
  null atom p and null atom s and (al':= pmatchWithSl(first s,first p,al)) and
    pmatchWithSl(rest s,rest p,al')

elapsedTime() ==
  currentTime := get_run_time()
  elapsedSeconds:= (currentTime-$previousTime)*1.0/$timerTicksPerSecond
  $previousTime:= currentTime
  elapsedSeconds

addStats([a,b],[c,d]) == [a+c,b+d]

printStats [byteCount,elapsedSeconds] ==
  timeString := normalizeStatAndStringify elapsedSeconds
  if byteCount = 0 then SAY('"Time: ",timeString,'" SEC.") else
    SAY('"Size: ",byteCount,'" BYTES     Time: ",timeString,'" SEC.")
  TERPRI()
  nil

extendsCategoryForm(domain, form, form', e) ==
  --is domain of category form also of category form'?
  --domain is only used for ensuring that X being a Ring means that it
  --satisfies (Algebra X)
  form=form' => true
  form=$Category => nil
  form' = $Category => nil
  form' is ["Join", :l] => and/[extendsCategoryForm(domain, form, x, e)
                                for x in l]
  form' is ["CATEGORY",.,:l] =>
    and/[extendsCategoryForm(domain, form, x, e) for x in l]
  form is ["Join", :l] => or/[extendsCategoryForm(domain, x, form', e)
                              for x in l]
  form is ["CATEGORY",.,:l] =>
    member(form',l) or
      stackWarning ['"not known that ",form','" is of mode ",form] or true
  isCategoryForm(form) =>
          --Constructs the associated vector
    formVec := (compMakeCategoryObject(form, e)).expr
            --Must be e to pick up locally bound domains
    form' is ["SIGNATURE",op,args,:.] =>
        assoc([op,args],formVec.(1)) or
            assoc(SUBSTQ(domain, "%", [op, args]),
                  SUBSTQ(domain, "%", formVec.(1)))
    form' is ["ATTRIBUTE",at] => BREAK()
    form' is ["IF",:.] => true --temporary hack so comp won't fail
    -- Are we dealing with an Aldor category?  If so use the "has" function ...
    # formVec = 1 => newHasTest(form,form')
    catvlist:= formVec.4
    member(form',first catvlist) or
     member(form', SUBSTQ(domain, "%", first(catvlist))) or
      (or/
        [extendsCategoryForm(domain, SUBSTQ(domain, "%", cat), form', e)
          for [cat,:.] in CADR catvlist])
  nil

getmode(x,e) ==
  prop:=getProplist(x,e)
  u := QLASSQ("value", prop) => u.mode
  QLASSQ("mode", prop)

getmodeOrMapping(x,e) ==
  u:= getmode(x,e) => u
  (u:= get(x,"modemap",e)) is [[[.,:map],.],:.] => ["Mapping",:map]
  nil

substituteOp(op',op,x) ==
  atom x => x
  [(op=(f:= first x) => op'; f),:[substituteOp(op',op,y) for y in rest x]]

 -- following is only intended for substituting in domains slots 1 and 4
 -- signatures and categories
sublis_vec(p, e) ==
  LIST2VEC [suba(p, e.i) for i in 0..MAXINDEX e] where
    suba(p,e) ==
      STRINGP e => e
      atom e => (y:= ASSQ(e,p) => rest y; e)
      u:= suba(p,QCAR e)
      v:= suba(p,QCDR e)
      EQ(QCAR e,u) and EQ(QCDR e,v) => e
      [u,:v]

subst_in_cat(fp, ap, cv) ==
    pp := MAPCAR(FUNCTION CONS, fp, ap)
    sublis_vec(pp, cv)

