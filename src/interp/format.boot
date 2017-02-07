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

--% Functions for display formatting system objects

-- some of these are redundant and should be compacted
$formatSigAsTeX := 1

--% Formatting modemaps

sayModemap m ==
  -- sayMSG formatModemap displayTranModemap m
  sayMSG formatModemap old2NewModemaps displayTranModemap m

sayModemapWithNumber(m,n) ==
  msg := reverse cleanUpSegmentedMsg reverse ["%i","%i",'" ",
    STRCONC(lbrkSch(),object2String n,rbrkSch()),
      :formatModemap displayTranModemap m,"%u","%u"]
  sayMSG flowSegmentedMsg(reverse msg,$LINELENGTH,3)

displayOpModemaps(op,modemaps) ==
  TERPRI()
  count:= #modemaps
  phrase:= (count=1 => 'modemap;'modemaps)
  sayMSG ['%b,count,'%d,phrase,'" for",'%b,op,'%d,'":"]
  for modemap in modemaps repeat sayModemap modemap

displayTranModemap (mm is [[x,:sig],[pred,:y],:z]) ==
  -- The next 8 lines are a HACK to deal with the "partial" definition
  -- JHD/RSS
  if pred is ['partial,:pred'] then
    [b,:c]:=sig
    sig:=[['Union,b,'"failed"],:c]
    mm:=[[x,:sig],[pred',:y],:z]
  else if pred = 'partial then
    [b,:c]:=sig
    sig:=[['Union,b,'"failed"],:c]
    mm:=[[x,:sig],y,:z]
  mm' := EQSUBSTLIST('(m n p q r s t i j k l),
    MSORT listOfPredOfTypePatternIds pred,mm)
  EQSUBSTLIST('(D D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 D14),
    MSORT listOfPatternIds [sig,[pred,:y]],mm')

listOfPredOfTypePatternIds p ==
  p is ['AND,:lp] or p is ['OR,:lp] =>
    UNIONQ([:listOfPredOfTypePatternIds p1 for p1 in lp],NIL)
  p is [op,a,.] and op = 'ofType =>
    isPatternVar a => [a]
    nil
  nil

removeIsDomains pred ==
  pred is ['isDomain,a,b] => true
  pred is ['AND,:predl] =>
    MKPF([x for x in predl | x isnt ['isDomain,:.]],'AND)
  pred

canRemoveIsDomain? pred ==
  -- returns nil OR an alist for substitutions of domains ordered so that
  -- after substituting for each pair in turn, no left-hand names remain
  alist :=
    pred is ['isDomain,a,b] => [[a,:b],:alist]
    pred is ['AND,:predl] =>
      [[a,:b] for pred in predl | pred is ['isDomain,a,b]]
  findSubstitutionOrder? alist

findSubstitutionOrder? alist == fn(alist,nil) where
  -- returns NIL or an appropriate substituion order
  fn(alist,res) ==
    null alist => NREVERSE res
    choice := or/[x for (x:=[a,:b]) in alist | null containedRight(a,alist)] =>
      fn(delete(choice,alist),[choice,:res])
    nil

containedRight(x,alist)== or/[CONTAINED(x,y) for [.,:y] in alist]

DEFPARAMETER($Dmarker, "<Dmarker>")

removeIsDomainD pred ==
  pred is ['isDomain, =$Dmarker, D] =>
    [D,nil]
  pred is ['AND,:preds] =>
    D := nil
    for p in preds while not D repeat
      p is ['isDomain, =$Dmarker, D1] =>
        D := D1
        npreds := delete(['isDomain, $Dmarker, D1], preds)
    D =>
      1 = #npreds => [D,first npreds]
      [D,['AND,:npreds]]
    nil
  nil

formatModemap modemap ==
  [[dc,target,:sl],pred,:.]:= modemap
  if alist := canRemoveIsDomain? pred then
    dc:= substInOrder(alist,dc)
    pred:= substInOrder(alist,removeIsDomains pred)
    target:= substInOrder(alist,target)
    sl:= substInOrder(alist,sl)
  else if removeIsDomainD pred is [D,npred] then
    pred := SUBST(D, $Dmarker, npred)
    target := SUBST(D, $Dmarker, target)
    sl := SUBST(D, $Dmarker, sl)
  predPart:= formatIf pred
  targetPart:= prefix2String target
  argTypeList:=
    null sl => nil
    concat(prefix2String first sl,fn(rest sl)) where
      fn l ==
        null l => nil
        concat(",",prefix2String first l,fn rest l)
  argPart:=
    #sl<2 => argTypeList
    ['"_(",:argTypeList,'"_)"]
  fromPart:=
    if dc = $Dmarker and D
      then concat('%b,'"from",'%d,prefix2String D)
      else concat('%b,'"from",'%d,prefix2String dc)
  firstPart:= concat('" ",argPart,'" -> ",targetPart)
  sayWidth firstPart + sayWidth fromPart > 74 => --allow 5 spaces for " [n]"
    fromPart:= concat('" ",fromPart)
    secondPart :=
      sayWidth fromPart + sayWidth predPart < 75 =>
        concat(fromPart,predPart)
      concat(fromPart,'%l,predPart)
    concat(firstPart,'%l,secondPart)
  firstPart:= concat(firstPart,fromPart)
  sayWidth firstPart + sayWidth predPart < 80 =>
    concat(firstPart,predPart)
  concat(firstPart,'%l,predPart)

substInOrder(alist,x) ==
  alist is [[a, :b], :y] => substInOrder(y, substitute(b, a, x))
  x

sayMms(op, mms, label) ==
    m := # mms
    sayMSG
        m = 1 =>
            ['"There is one", :bright label, '"function called",
              :bright op, '":"]
        ['"There are ", m, :bright label, '"functions called",
            :bright op, '":"]
    for mm in mms for i in 1.. repeat
        sayModemapWithNumber(mm, i)

reportOpSymbol op1 ==
  op := (STRINGP op1 => INTERN op1; op1)
  modemaps := getAllModemapsFromDatabase(op,nil)
  null modemaps =>
    ok := true
    sayKeyedMsg("S2IF0010",[op1])
    if SIZE PNAME op1 < 3 then
      x := UPCASE queryUserKeyedMsg("S2IZ0060",[op1])
      null MEMQ(STRING2ID_-N(x,1),'(Y YES)) =>
        ok := nil
        sayKeyedMsg("S2IZ0061",[op1])
    ok => apropos [op1]
  sayNewLine()
  -- filter modemaps on whether they are exposed
  mmsE := mmsU := NIL
  for mm in modemaps repeat
    isFreeFunctionFromMm(mm) or isExposedConstructor getDomainFromMm(mm) => mmsE := [mm,:mmsE]
    mmsU := [mm,:mmsU]
  if mmsE then
    sayMms(op, mmsE, '"exposed")
  if mmsU then
    if mmsE then sayNewLine()
    sayMms(op,mmsU,'"unexposed")
  nil

formatOpType (form:=[op,:argl]) ==
  null argl => unabbrev op
  form2String [unabbrev op, :argl]

formatOperationAlistEntry (entry:= [op,:modemaps]) ==
  -- alist has entries of the form: ((op sig) . pred)
  -- opsig on this list => op is defined only when the predicate is true
  ans:= nil
  for [sig,.,:predtail] in modemaps repeat
    pred := (predtail is [p,:.] => p; 'T)
    -- operation is always defined
    ans :=
      [concat(formatOpSignature(op,sig),formatIf pred),:ans]
  ans

formatOperation([[op,sig],.,[fn,.,n]],domain) ==
    formatOpSignature(op,sig)

formatOpSignature(op,sig) ==
  concat('%b,formatOpSymbol(op,sig),'%d,": ",formatSignature sig)

formatOpConstant op ==
  concat('%b,formatOpSymbol(op,'($)),'%d,'": constant")

formatOpSymbol(op,sig) ==
  if op = 'Zero then op := "0"
  else if op = 'One then op := "1"
  null sig => op
  quad := specialChar 'quad
  n := #sig
  (op = 'elt) and (n = 3) =>
    (CADR(sig) = '_$) =>
      STRINGP (sel := CADDR(sig)) =>
        [quad,".",sel]
      [quad,".",quad]
    op
  STRINGP op or GETL(op,"Led") or GETL(op,"Nud") =>
    n = 3 =>
      if op = 'SEGMENT then op := '".."
      op = "in" => [quad, '" ", op, '" ", quad]
-- stop exquo from being displayed as infix (since it is not accepted
-- as such by the interpreter)
      op = 'exquo => op
      [quad,op,quad]
    n = 2 =>
      not GETL(op,"Nud") => [quad,op]
      [op,quad]
    op
  op

dollarPercentTran x ==
    -- Translate $ to %. We actually return %% so that the message
    -- printer will display a single %
    x is [y,:z] =>
        y1 := dollarPercentTran y
        z1 := dollarPercentTran z
        EQ(y, y1) and EQ(z, z1) => x
        [y1, :z1]
    x = "$" or x = '"$" => "%%"
    x

formatSignature sig ==
  $formatSigAsTeX: local := 1
  formatSignature0 sig

formatSignatureArgs sml ==
  $formatSigAsTeX: local := 1
  formatSignatureArgs0 sml

formatSignature0 sig ==
  null sig => "() -> ()"
  INTEGERP sig => '"hashcode"
  [tm,:sml] := sig
  sourcePart:= formatSignatureArgs0 sml
  targetPart:= prefix2String0 tm
  dollarPercentTran concat(sourcePart,concat(" -> ",targetPart))

formatSignatureArgs0(sml) ==
-- formats the arguments of a signature
  null sml => ["_(_)"]
  null rest sml => prefix2String0 first sml
  argList:= prefix2String0 first sml
  for m in rest sml repeat
    argList:= concat(argList,concat(",",prefix2String0 m))
  concat("_(",concat(argList,"_)"))

--% Conversions to string form

expr2String x ==
  atom (u:= prefix2String0 x) => u
  "STRCONC"/[atom2String y for y in u]

-- exports (this is a badly named bit of sillyness)
prefix2StringAsTeX form ==
  form2StringAsTeX form

prefix2String form ==
  $formatSigAsTeX: local := 1
  form2StringLocal form

-- local version
prefix2String0 form ==
  form2StringLocal form

--  SUBRP form => formWrapId BPINAME form
--  atom form =>
--    form=$EmptyMode or form=$quadSymbol => formWrapId specialChar 'quad
--    STRINGP form => formWrapId form
--    IDENTP form =>
--      constructor? form => app2StringWrap(formWrapId form, [form])
--      formWrapId form
--    formWrapId STRINGIMAGE form

form2StringWithWhere u ==
  $permitWhere : local := true
  $whereList: local := nil
  s:= form2String u
  $whereList => concat(s,'%b,'"where",'%d,"%i",$whereList,"%u")
  s

form2StringWithPrens form ==
  null (argl := rest form) => [first form]
  null rest argl => [first form,"(",first argl,")"]
  form2String form

formString u ==
  x := form2String u
  atom x => STRINGIMAGE x
  "STRCONC"/[STRINGIMAGE y for y in x]

DEFPARAMETER($from_unparse, false)

unparseInputForm u ==
  $formatSigAsTeX: local := 1
  $InteractiveMode: local := false
  $from_unparse : local := true
  form2StringLocal u

form2String u ==
  $formatSigAsTeX: local := 1
  form2StringLocal u

form2StringAsTeX u ==
  $formatSigAsTeX: local := 2
  form2StringLocal u

form2StringLocal u ==
--+
  $NRTmonitorIfTrue : local := nil
  $fortInts2Floats  : local := nil
  form2String1 u

constructorName con ==
  $abbreviateTypes => abbreviate con
  con

DEFPARAMETER($justUnparseType, false)

form2String1 u ==
  ATOM u =>
    u=$EmptyMode or u=$quadSymbol => formWrapId specialChar 'quad
    IDENTP u =>
      constructor? u => app2StringWrap(formWrapId u, [u])
      u
    SUBRP u => formWrapId BPINAME u
    STRINGP u => formWrapId u
    WRITE_-TO_-STRING formWrapId u
  u1 := u
  op := first u
  argl := rest u
  op='Join or op= 'mkCategory => formJoin1(op,argl)
  $InteractiveMode and (u:= constructor? op) =>
    null argl => app2StringWrap(formWrapId constructorName op, u1)
    op = "NTuple"  => [ form2String1 first argl, "*"]
    op = "Map"     => ["(",:formatSignature0 [argl.1,argl.0],")"]
    op = 'Record => record2String(argl)
    $justUnparseType or null(conSig := getConstructorSignature op) =>
      application2String(constructorName op,[form2String1(a) for a in argl], u1)
    ml := rest conSig
    if not freeOfSharpVars ml then
      ml:=SUBLIS([[pvar,:val] for pvar in $FormalMapVariableList
        for val in argl], ml)
    argl:= formArguments2String(argl,ml)
      -- extra null check to handle mutable domain hack.
    null argl => constructorName op
    application2String(constructorName op,argl, u1)
  op = "Mapping" => ["(",:formatSignature argl,")"]
  op = "Record" => record2String(argl)
  op = 'Union  =>
    application2String(op,[form2String1 x for x in argl], u1)
  op = ":" =>
      null argl => [ '":" ]
      null rest argl => [ '":", form2String1 first argl ]
      formDecl2String(argl.0,argl.1)
  op = "#" and PAIRP argl and LISTP first argl =>
    STRINGIMAGE SIZE first argl
  op = 'Join => formJoin2String argl
  op = "ATTRIBUTE" => form2String1 first argl
  op='Zero => 0
  op='One => 1
  op = 'AGGLST => tuple2String [form2String1 x for x in argl]
  op = 'BRACKET =>
    argl' := form2String1 first argl
    ["[",:(atom argl' => [argl']; argl'),"]"]
  op = 'SUB => sub_to_string(argl)
  op = 'SUPERSUB => sub_to_string(argl)
  op = "SIGNATURE" =>
     [operation,sig] := argl
     concat(operation,": ",formatSignature sig)
  op = 'COLLECT => formCollect2String argl
  op = 'construct =>
    concat(lbrkSch(),
           tuple2String [form2String1 x for x in argl],rbrkSch())
  op = "SEGMENT" =>
    null argl => '".."
    lo := form2String1 first argl
    argl := rest argl
    (null argl) or null (first argl) => [lo, '".."]
    [lo, '"..", form2String1 first argl]
  op = "MATRIX" => matrix2String argl
  u1 is ["ROOT", arg1] =>
     concat("sqrt(", appOrParen(arg1),")")
  u1 is ["ROOT", arg1, arg2] =>
     concat("nthRoot(", appOrParen(arg1),",",appOrParen(arg2),")")
     --concat(appOrParen(arg1), '"^", appOrParen(["OVER",1,arg2]))
  u1 is ["$elt", t, f] =>
     concat(form2String1 f, '"$", form2String1 t)
  #argl = 2 and (isBinaryInfix op or op = "::" or op = '"::"_
     or op = "@" or op = '"@" or op = "pretend" or op = '"pretend"_
     or op = "OVER" or op = '"OVER") =>
          binop2String [op,:argl]
  application2String(op,[form2String1 x for x in argl], u1)

matrix2String x ==
  concat(lbrkSch(),
    tuple2String [outtranRow ri for ri in rest(x)],rbrkSch()) where
      outtranRow x ==
        concat(lbrkSch(),
          tuple2String [form2String1 ei for ei in rest(x)], rbrkSch())

binop2String x ==
    $curExpr : local := x
    x is ["=", arg1, arg2] or x is ['"=", arg1, arg2] =>
        concat(sumOrParen(arg1), '"=", sumOrParen(arg2))
    sumOrParen(x)

sumOrParen(x) ==
   x is [op, arg1, arg2] =>
       op = "+" or op = '"+" =>
           concat(sumOrParen(arg1), '"+", productOrParen(arg2))
       op = "-" or op = '"-" =>
           concat(sumOrParen(arg1), '"-", productOrParen(arg2))
       op = "/" or op = '"/" or op = "OVER" or op = '"OVER" =>
           concat(appOrParen(arg1), '"/", appOrParen(arg2))
       productOrParen(x)
   productOrParen(x)

productOrParen(x) ==
   x is [op, arg1, arg2] =>
       op = "*" or op ='"*" =>
           concat(productOrParen(arg1), '"*",  powerOrParen(arg2))
       powerOrParen(x)
   powerOrParen(x)

powerOrParen(x) ==
   x is [op, arg1, arg2] =>
      op = "**" or op = '"**" or op = "^" or op = '"^"  =>
           concat(coerceOrParen(arg1), '"^", coerceOrParen(arg2))
      coerceOrParen(x)
   coerceOrParen(x)

coerceOrParen(x) ==
   x is [op, arg1, arg2] =>
      op = "::" or op = '"::" =>
           concat(coerceOrParen(arg1), '"::", appOrParen(arg2))
      op = "@" or op = '"@" =>
           concat(coerceOrParen(arg1), '"@", appOrParen(arg2))
      op = "pretend" or op = '"pretend" =>
           concat(coerceOrParen(arg1), '" ", '"pretend", '" ",_
                   appOrParen(arg2))
      appOrParen(x)
   appOrParen(x)

appOrParen(x) ==
   SYMBOLP(x) => formWrapId x
   INTEGERP(x) =>
       x >=0 => WRITE_-TO_-STRING x
       concat('"(",WRITE_-TO_-STRING x,'")")
   -- Kludge to avoid extra parentheses printing a SparseUnivariatePolynomial
   x = '"?" => formWrapId x
   ATOM(x) => concat('"(", form2String1(x), '")")
   [op, :argl] := x
   (op = "-" or op = '"-") and #argl = 1 =>
       concat('"(", '"-", appOrParen(first argl), '")")
   EQ(x, $curExpr) => BREAK()
   op is ["$elt", f, t] =>
       form2String1 x
   -- Put parenthesis around anything special
   not(SYMBOLP op) or GET(op, "Led") or GET(op, "Nud")_
     or op= 'mkCategory or op = "SEGMENT" _
     or op = 'construct or op = 'COLLECT or op = "SIGNATURE"_
     or op = 'BRACKET or op = 'AGGLST or op = "ATTRIBUTE"_
     or op = "#" =>
        concat('"(", form2String1(x), '")")
   op = "Zero" => '"0"
   op = "One" => '"1"
   form2String1 x


formWrapId id ==
  $formatSigAsTeX = 1 => id
  $formatSigAsTeX = 2 =>
    sep := '"`"
    FORMAT(NIL,'"\verb~a~a~a",sep, id, sep)
  error "Bad formatSigValue"

formArguments2String(argl,ml) == [fn(x,m) for x in argl for m in ml] where
  fn(x,m) ==
    x=$EmptyMode or x=$quadSymbol => specialChar 'quad
    STRINGP(x) or IDENTP(x) => x
    x is [ ='_:,:.] => form2String1 x
    x is ["QUOTE", y] =>
        m = $Symbol and SYMBOLP(y) => y
        form2String1 x
    isValidType(m) and PAIRP(m) and
      (GETDATABASE(first(m),'CONSTRUCTORKIND) = 'domain) =>
        (x' := coerceInteractive(objNewWrap(x,m),$OutputForm)) =>
          form2String1 objValUnwrap x'
        form2String1 x
    form2String1 x

formDecl2String(left,right) ==
  $declVar: local := left
  whereBefore := $whereList
  ls:= form2StringLocal left
  rs:= form2StringLocal right
  $whereList ~= whereBefore and $permitWhere => ls
  concat(form2StringLocal ls,'": ",rs)

formJoin1(op,u) ==
  if op = 'Join then [:argl,last] := u else (argl := nil; last := [op,:u])
  last is [id, :r] and id in '(mkCategory CATEGORY) =>
    if id = "CATEGORY" then r := rest(r)
    $abbreviateJoin = true => concat(formJoin2 argl,'%b,'"with",'%d,'"...")
    $permitWhere = true =>
      opList:= formatJoinKey(r,id)
      $whereList:= concat($whereList,"%l",$declVar,": ",
        formJoin2 argl,'%b,'"with",'%d,"%i",opList,"%u")
      formJoin2 argl
    opList:= formatJoinKey(r,id)
    suffix := concat('%b,'"with",'%d,"%i",opList,"%u")
    concat(formJoin2 argl,suffix)
  formJoin2 u

formatJoinKey(r,key) ==
  key = 'mkCategory =>
    r is [opPart,catPart,:.] =>
      opString :=
        opPart is [='LIST,:u] =>
          "append"/[concat("%l",formatOpSignature(op,sig),formatIf pred)
            for [='QUOTE,[[op,sig],pred]] in u]
        nil
      catString :=
        catPart is [='LIST,:u] =>
          "append"/[concat("%l",'" ",form2StringLocal con,formatIf pred)
            for [='QUOTE,[con,pred]] in u]
        nil
      concat(opString,catString)
    '"?? unknown mkCategory format ??"
  -- otherwise we have the CATEGORY form
  "append"/[fn for x in r] where fn ==
    x is ['SIGNATURE,op,sig] => concat("%l",formatOpSignature(op,sig))
    x is ['ATTRIBUTE,a] => concat("%l",formatAttribute a)
    x

formJoin2 argl ==
-- argl is a list of categories NOT containing a "with"
  null argl => '""
  1=#argl => form2StringLocal argl.0
  application2String('Join,[form2StringLocal x for x in argl], NIL)

formJoin2String (u:=[:argl,last]) ==
  last is ["CATEGORY",.,:atsigList] =>
    postString:= concat("_(",formTuple2String atsigList,"_)")
    #argl=1 => concat(first argl,'" with ",postString)
    concat(application2String('Join,argl, NIL)," with ",postString)
  application2String('Join,u, NIL)

sub_to_string(u) ==
    [op, :argl] := u
    fo := form2String1(op)
    if atom(fo) then fo := [fo];
    rargl := REVERSE(argl)
    resl := []
    for arg in rargl repeat
        resl = [] and arg = [] => "iterate"
        if resl then resl := cons(";", resl)
        fa := form2String1(arg)
        if atom(fa) then fa := [fa]
        resl := [:fa, :resl]
    [:fo, "[", :resl, "]"]

formCollect2String [:itl,body] ==
  ["_(",body,:"append"/[formIterator2String x for x in itl],"_)"]

formIterator2String x ==
  x is ["STEP",y,s,.,:l] =>
    tail:= (l is [f] => form2StringLocal f; nil)
    concat("for ",y," in ",s,'"..",tail)
  x is ["tails",y] => concat("tails ",formatIterator y)
  x is ["reverse",y] => concat("reverse ",formatIterator y)
  x is ["|",y,p] => concat(formatIterator y," | ",form2StringLocal p)
  x is ["until",p] => concat("until ",form2StringLocal p)
  x is ["while",p] => concat("while ",form2StringLocal p)
  systemErrorHere "formatIterator"

tuple2String argl ==
  fn1 argl where
    fn1 argl ==
        null argl => nil
        string := first argl
        if member(string, '("failed" "nil" "prime" "sqfr" "irred"))
            then string := STRCONC('"_"", string, '"_"")
            else string :=
                ATOM string => object2String string
                [fn2 x for x in string]
        for x in rest argl repeat
            if member(x, '("failed" "nil" "prime" "sqfr" "irred")) then
                x := STRCONC('"_"", x, '"_"")
            string := concat(string, concat(",", fn2 x))
        string
    fn2 x ==
      ATOM x => object2String x
      -- [fn2 first x, :f rest x]
      [fn2 y for y in x]

linearFormatName x ==
  atom x => x
  linearFormat x

linearFormat x ==
  atom x => x
  x is [op,:argl] and atom op =>
    argPart:=
      argl is [a,:l] => [a,:"append"/[[",",x] for x in l]]
      nil
    [op,"(",:argPart,")"]
  [linearFormat y for y in x]

formatArgList l ==
  null l => nil
  acc:= linearFormat first l
  for x in rest l repeat
    acc:= concat(acc,",",linearFormat x)
  acc

formTuple2String argl ==
  null argl => nil
  string:= form2StringLocal first argl
  for x in rest argl repeat
    string:= concat(string,concat(",",form2StringLocal x))
  string

isInternalFunctionName(op) ==
  (not IDENTP(op)) or (op = "*") or (op = "**") => NIL
  (1 = SIZE(op':= PNAME op)) or (char("*") ~= op'.0) => NIL
  -- if there is a semicolon in the name then it is the name of
  -- a compiled spad function
  null (e := STRPOS('"_;",op',1,NIL)) => NIL
  (char(" ") = (y := op'.1)) or (char("*") = y) => NIL
  table := MAKETRTTABLE('"0123456789",NIL)
  s := STRPOSL(table,op',1,true)
  null(s) or s > e => NIL
  SUBSTRING(op',s,e-s)

application2String(op,argl, linkInfo) ==
  op is ["$elt", t, f] =>
      concat(application2String(f, argl, linkInfo), '"$", _
             form2String1 t)
  null argl =>
    res1 :=
       (op' := isInternalFunctionName(op)) => op'
       app2StringWrap(formWrapId op, linkInfo)
    $from_unparse => concat(res1,'"()")
    res1
  1=#argl =>
    first argl is ["<",:.] => concat(op,first argl)
    concat(app2StringWrap(formWrapId op, linkInfo), '"(", first argl, '")")
--op in '(UP SM) =>
--  newop:= (op = "UP" => "P";"M")
--  concat(newop,concat(lbrkSch(),argl.0,rbrkSch(),argl.1))
--op='RM  =>concat("M",concat(lbrkSch(),
--                     argl.0,",",argl.1,rbrkSch(),argl.2))
--op='MP =>concat("P",concat(argl.0,argl.1))
  op='SEGMENT =>
    null argl => '".."
    (null rest argl) or (null first rest argl) =>
      concat(first argl, '"..")
    concat('"(", first argl, concat('"..", first rest argl), '")")
  concat(app2StringWrap(formWrapId op, linkInfo) ,
                        concat("_(",concat(tuple2String argl,"_)")))

app2StringConcat0(x,y) ==
  FORMAT(NIL, '"~a ~a", x, y)

app2StringWrap(string, linkInfo) ==
  not linkInfo => string
  $formatSigAsTeX = 1 => string
  $formatSigAsTeX = 2 =>
    str2 :=  "app2StringConcat0"/form2Fence linkInfo
    sep := '"`"
    FORMAT(NIL, '"\lispLink{\verb!(|conPage| '~a)!}{~a}",
          str2, string)
  error "Bad value for $formatSigAsTeX"

record2String x ==
  argPart := NIL
  for [":",a,b] in x repeat argPart:=
    concat(argPart,",",a,": ",form2StringLocal b)
  null argPart => '"Record()"
  concat("Record_(",rest argPart,"_)")

plural(n,string) ==
  suffix:=
    n = 1 => '""
    '"s"
  [:bright n,string,suffix]

formatIf pred ==
  not pred => nil
  pred in '(T (QUOTE T)) => nil
  concat('%b,'"if",'%d,pred2English pred)

formatPredParts s ==
  s is ['QUOTE,s1] => formatPredParts s1
  s is ['LIST,:s1] => [formatPredParts s2 for s2 in s1]
  s is ['devaluate,s1] => formatPredParts s1
  s is ['getDomainView,s1,.] => formatPredParts s1
  s is ['SUBST,a,b,c] =>    -- this is a signature
    BREAK()
    s1 := formatPredParts substitute(formatPredParts a,b,c)
    s1 isnt [fun,sig] => s1
    ['SIGNATURE,fun,[formatPredParts(r) for r in sig]]
  s

form_to_abbrev(x) ==
    $abbreviateTypes : local := true
    form2String(x)

pred2English x ==
  x is ['IF,cond,thenClause,elseClause] =>
    c := concat('"if ",pred2English cond)
    t := concat('" then ",pred2English thenClause)
    e := concat('" else ",pred2English elseClause)
    concat(c,t,e)
  x is ['AND,:l] =>
    tail:="append"/[concat(bright '"and",pred2English x) for x in rest l]
    concat(pred2English first l,tail)
  x is ['OR,:l] =>
    tail:= "append"/[concat(bright '"or",pred2English x) for x in rest l]
    concat(pred2English first l,tail)
  x is ['NOT,l] =>
    concat('"not ",pred2English l)
  x is [op,a,b] and op in '(has ofCategory) =>
    concat(pred2English a, '%b, '"has",'%d, form_to_abbrev b)
  x is [op,a,b] and op in '(HasSignature HasCategory) =>
    concat(prefix2String0 formatPredParts a,'%b,'"has",'%d,
      prefix2String0 formatPredParts b)
  x is [op,a,b] and op in '(ofType getDomainView) =>
    if b is ['QUOTE,b'] then b := b'
    concat(pred2English a, '": ", form_to_abbrev b)
  x is [op,a,b] and op in '(isDomain domainEqual) =>
    concat(pred2English a, '" = ", form_to_abbrev b)
  x is [op,:.] and (translation := LASSOC(op,'(
    (_< . " < ") (_<_= . " <= ")
      (_> . " > ") (_>_= . " >= ") (_=  . " = ") (_^_= . " _^_= ")))) =>
        concat(pred2English a,translation,pred2English b)
  x is ['ATTRIBUTE, form] => BREAK()
  form2String x

mathObject2String x ==
  CHARACTERP x => COERCE([x],'STRING)
  object2String x

object2String x ==
  STRINGP x => x
  IDENTP x  => PNAME x
  NULL x    => '""
  PAIRP  x  => STRCONC(object2String first x, object2String rest x)
  WRITE_-TO_-STRING x

object2Identifier x ==
  IDENTP x  => x
  STRINGP x => INTERN x
  INTERN WRITE_-TO_-STRING x

blankList x == "append"/[[BLANK,y] for y in x]


string2Float s ==
  -- takes a string, calls the parser on it and returns a float object
  p := ncParseFromString s
  p isnt [["$elt", FloatDomain, "float"], x, y, z] =>
    systemError '"string2Float: did not get a float expression"
  flt := getFunctionFromDomain("float", FloatDomain,
    [$Integer, $Integer, $PositiveInteger])
  SPADCALL(x, y, z, flt)



form2Fence form ==
  -- body of dbMkEvalable
  [op, :.] := form
  kind := GETDATABASE(op,'CONSTRUCTORKIND)
  kind = 'category => form2Fence1 form
  form2Fence1 mkEvalable form

form2Fence1 x ==
  x is [op,:argl] =>
    op = 'QUOTE => ['"(QUOTE ",:form2FenceQuote first argl,'")"]
    ['"(", FORMAT(NIL, '"|~a|", op),:"append"/[form2Fence1 y for y in argl],'")"]
  x = "$" => ["%"]
  IDENTP x => [FORMAT(NIL, '"|~a|", x)]
--  [x]
  ['"  ", x]

form2FenceQuote x ==
  NUMBERP x => [STRINGIMAGE x]
  SYMBOLP x => [FORMAT(NIL, '"|~a|", x)]
  atom    x => ['"??"]
  ['"(",:form2FenceQuote first x,:form2FenceQuoteTail rest x]

form2FenceQuoteTail x ==
  null x => ['")"]
  atom x => ['" . ",:form2FenceQuote x,'")"]
  ['" ",:form2FenceQuote first x,:form2FenceQuoteTail rest x]

form2StringList u ==
  atom (r := form2String u) => [r]
  r
