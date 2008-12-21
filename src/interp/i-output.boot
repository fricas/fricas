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


--Modified JHD February 1993: see files miscout.input for some tests of this
-- General principle is that maprin0 is the top-level routine,
-- which calls maprinChk to print the object (placing certain large
-- matrices on a look-aside list), then calls maprinRows to print these.
-- These prints call maprinChk recursively, and maprinChk has to ensure that
-- we do not end up in an infinite recursion: matrix1 = matrix2 ...

--% Output display routines

SETANDFILEQ($plainRTspecialCharacters,[
    '_+,      -- upper left corner   (+)
    '_+,      -- upper right corner  (+)
    '_+,      -- lower left corner   (+)
    '_+,      -- lower right corner  (+)
    '_|,      -- vertical bar
    '_-,      -- horizontal bar      (-)
    '_?,      -- APL quad            (?)
    '_[,      -- left bracket
    '_],      -- right bracket
    '_{,      -- left brace
    '_},      -- right brace
    '_+,      -- top    box tee      (+)
    '_+,      -- bottom box tee      (+)
    '_+,      -- right  box tee      (+)
    '_+,      -- left   box tee      (+)
    '_+,      -- center box tee      (+)
    '_\       -- back slash
     ])

makeCharacter n == INTERN(STRING(CODE_-CHAR n))

SETANDFILEQ($RTspecialCharacters,[
    makeCharacter 218,      -- upper left corner   (+)
    makeCharacter 191,      -- upper right corner  (+)
    makeCharacter 192,      -- lower left corner   (+)
    makeCharacter 217,      -- lower right corner  (+)
    makeCharacter 179,      -- vertical bar
    makeCharacter 196,      -- horizontal bar      (-)
    $quadSymbol,      -- APL quad            (?)
    '_[,      -- left bracket
    '_],      -- right bracket
    '_{,      -- left brace
    '_},      -- right brace
    makeCharacter 194,      -- top    box tee      (+)
    makeCharacter 193,      -- bottom box tee      (+)
    makeCharacter 180,      -- right  box tee      (+)
    makeCharacter 195,      -- left   box tee      (+)
    makeCharacter 197,      -- center box tee      (+)
    '_\       -- back slash
     ])

SETANDFILEQ($specialCharacters,$RTspecialCharacters)

SETANDFILEQ($specialCharacterAlist, '(
  (ulc  .  0)_
  (urc  .  1)_
  (llc  .  2)_
  (lrc  .  3)_
  (vbar .  4)_
  (hbar .  5)_
  (quad .  6)_
  (lbrk .  7)_
  (rbrk .  8)_
  (lbrc .  9)_
  (rbrc . 10)_
  (ttee . 11)_
  (btee . 12)_
  (rtee . 13)_
  (ltee . 14)_
  (ctee . 15)_
  (bslash . 16)_
  ))

$collectOutput := nil

specialChar(symbol) ==
  -- looks up symbol in $specialCharacterAlist, gets the index
  -- into the EBCDIC table, and returns the appropriate character
  null (code := IFCDR ASSQ(symbol,$specialCharacterAlist)) => '"?"
  ELT($specialCharacters,code)

rbrkSch() == PNAME specialChar 'rbrk
lbrkSch() == PNAME specialChar 'lbrk
quadSch() == PNAME specialChar 'quad

isBinaryInfix x ==
    x in '(_= _+ _- _* _/ _*_* _^ "=" "+" "-" "*" "/" "**" "^")

stringApp([.,u],x,y,d) ==
  appChar(STRCONC($DoubleQuote,atom2String u,$DoubleQuote),x,y,d)

stringWidth u ==
  u is [.,u] or THROW('outputFailure,'outputFailure)
  2+#u

obj2String o ==
  atom o =>
    STRINGP o => o
    o = " " => '" "
    o = ")" => '")"
    o = "(" => '"("
    STRINGIMAGE o
  APPLY('STRCONC,[obj2String o' for o' in o])

APP(u,x,y,d) ==
  atom u => appChar(atom2String u,x,y,d)
  u is [[op,:.],a] and (s:= GETL(op,'PREFIXOP)) =>
    GETL(op,'isSuffix) => appChar(s,x+WIDTH a,y,APP(a,x,y,d))
    APP(a,x+#s,y,appChar(s,x,y,d))
  u is [[id,:.],:.] =>
    fn := GETL(id,'APP) => FUNCALL(fn,u,x,y,d)
    not NUMBERP id and (d':= appInfix(u,x,y,d))=> d'
    appelse(u,x,y,d)
  appelse(u,x,y,d)

atom2String x ==
  IDENTP x => PNAME x
  STRINGP x => x
  stringer x

-- General convention in the "app..." functions:
-- Added from an attempt to fix bugs by JHD: 2 Aug 89
-- the first argument is what has to be printed
-- the second - x - is the horizontal distance along the page
--      at which to start
-- the third - y - is some vertical hacking control
-- the fourth - d - is the "layout" so far
-- these functions return an updated "layout so far" in general

appChar(string,x,y,d) ==
  if CHARP string then string := PNAME string
  line:= LASSOC(y,d) =>
    if MAXINDEX string = 1 and char(string.0) = "%" then
      string.1="b" =>
        bumpDeltaIfTrue:= true
        string.0:= EBCDIC 29
        string.1:= EBCDIC 200
      string.1="d" =>
        bumpDeltaIfTrue:= true
        string.0:= EBCDIC 29
        string.1:= EBCDIC 65
    shiftedX:= (y=0 => x+$highlightDelta; x)
      --shift x for brightening characters -- presently only if y=0
    RPLACSTR(line,shiftedX,n:=#string,string,0,n)
    if bumpDeltaIfTrue=true then $highlightDelta:= $highlightDelta+1
    d
  appChar(string,x,y,nconc(d,[[y,:GETFULLSTR(10+$LINELENGTH+$MARGIN," ")]]))

print(x,domain) ==
  dom:= devaluate domain
  $InteractiveMode: local:= true
  $dontDisplayEquatnum: local:= true
  output(x,dom)

mathprintWithNumber x ==
  ioHook("startAlgebraOutput")
  x:= outputTran x
  maprin
    $IOindex => ['EQUATNUM,$IOindex,x]
    x
  ioHook("endOfAlgebraOutput")

mathprint x == 
   x := outputTran x
   $saturn => texFormat1 x
   maprin x

sayMath u ==
  for x in u repeat acc:= concat(acc,linearFormatName x)
  sayALGEBRA acc

--% Output transformations

outputTran x ==
  x in '("failed" "nil" "prime" "sqfr" "irred") =>
    STRCONC('"_"",x,'"_"")
  STRINGP x => x
  VECP x =>
    outputTran ['BRACKET,['AGGLST,:[x.i for i in 0..MAXINDEX x]]]
  NUMBERP x =>
    MINUSP x => ["-",MINUS x]
    x
  atom x =>
    x=$EmptyMode => specialChar 'quad
    x
  x is [c,var,mode] and c in '(_pretend _: _:_: _@) =>
    var := outputTran var
    if PAIRP var then var := ['PAREN,var]
    ['CONCATB,var,c,obj2String prefix2String mode]
  x is ['ADEF,vars,.,.,body] =>
    vars :=
        vars is [x] => x
        ['Tuple,:vars]
    outputTran ["+->", vars, body]
  x is ['MATRIX,:m] => outputTranMatrix m
  x is ['matrix,['construct,c]] and
    c is ['COLLECT,:m,d] and d is ['construct,e] and e is ['COLLECT,:.] =>
      outputTran ['COLLECT,:m,e]
  x is ['LIST,:l] => outputTran ['BRACKET,['AGGLST,:l]]
  x is ['MAP,:l] => outputMapTran l
  x is ['brace, :l]    =>
    ['BRACE,  ['AGGLST,:[outputTran y for y in l]]]
  x is ['return,l] => ['return,outputTran l]
  x is ['return,.,:l] => ['return,:outputTran l]
  x is ['construct,:l] =>
    ['BRACKET,['AGGLST,:[outputTran y for y in l]]]

  x is [["$elt",domain,"float"], x, y, z] and (domain = $DoubleFloat or
    domain is ['Float]) and INTEGERP x and INTEGERP y and INTEGERP z and
        z > 0  and (float := getFunctionFromDomain("float",domain,[$Integer,$Integer,$PositiveInteger])) =>
            f := SPADCALL(x,y,z,float)
            o := coerceInteractive(mkObjWrap(f, domain), '(OutputForm))
            objValUnwrap o

  [op,:l]:= flattenOps x
  --needed since "op" is string in some spad code
  if STRINGP op then (op := INTERN op; x:= [op,:l])
  op = 'LAMBDA_-CLOSURE => 'Closure
  x is ['break,:.] => 'break
  x is ['SEGMENT,a] =>
    a' := outputTran a
    if LISTP a' then a' := ['PAREN,a']
    ['SEGMENT,a']
  x is ['SEGMENT,a,b] =>
    a' := outputTran a
    b' := outputTran b
    if LISTP a' then a' := ['PAREN,a']
    if LISTP b' then b' := ['PAREN,b']
    ['SEGMENT,a',b']

  op is ["$elt",targ,fun] or not $InteractiveMode and op is ["elt",targ,fun] =>
    -- l has the args
    targ' := obj2String prefix2String targ
    if 2 = #targ then targ' := ['PAREN,targ']
    ['CONCAT,outputTran [fun,:l],'"$",targ']
  x is ["$elt",targ,c] or not $InteractiveMode and x is ["elt",targ,c] =>
    targ' := obj2String prefix2String targ
    if 2 = #targ then targ' := ['PAREN,targ']
    ['CONCAT,outputTran c,'"$",targ']
  x is ["-",a,b] =>
    a := outputTran a
    b := outputTran b
    INTEGERP b =>
      b < 0 => ["+",a,-b]
      ["+",a,["-",b]]
    b is ["-",c] => ["+",a,c]
    ["+",a,["-",b]]

  -- next stuff translates exp(log(foo4)/foo3) into ROOT(foo4,foo3)
  (x is ["**", ='"%e",foo1]) and (foo1 is [ ='"/",foo2, foo3]) and
    INTEGERP(foo3) and (foo2 is ['log,foo4]) =>
       foo3 = 2 => ['ROOT,outputTran foo4]
       ['ROOT,outputTran foo4,outputTran foo3]
  (x is ["**", ='"%e",foo1]) and (foo1 is [op',foo2, foo3]) and
    (op' = '"*") and ((foo3 is ['log,foo4]) or (foo2 is ['log,foo4])) =>
       foo3 is ['log,foo4] =>
         ["**", outputTran foo4, outputTran foo2]
       foo4 := CADR foo2
       ["**", outputTran foo4, outputTran foo3]
  op = 'IF       => outputTranIf x
  op = 'COLLECT  => outputTranCollect x
  op = 'REDUCE   => outputTranReduce x
  op = 'REPEAT   => outputTranRepeat x
  op = 'SEQ      => outputTranSEQ x
  op in '(cons nconc) => outputConstructTran x
  l:= [outputTran y for y in l]
  op = "*" =>
     l is [a] => outputTran a
     l is [["-",a],:b] =>
       -- now this is tricky because we've already outputTran the list
       -- expect trouble when outputTran hits b again 
       -- some things object to being outputTran twice ,e.g.matrices
       -- same thing a bit lower down for "/" 
       a=1 => outputTran ["-",[op,:b]]
       outputTran ["-",[op,a,:b]]
     [op,:"append"/[(ss is ["*",:ll] => ll; [ss]) for ss in l]]
  op = "+" =>
     l is [a] => outputTran a
     [op,:"append"/[(ss is ["+",:ll] => ll; [ss]) for ss in l]]
  op = "/" =>
    if $fractionDisplayType = 'horizontal then op := 'SLASH
    else op := 'OVER
    l is [["-",a],:b] => outputTran ["-",[op,a,:b]]
    [outputTran op,:l]
  op="|" and l is [["Tuple",:u],pred] =>
    ['PAREN,["|",['AGGLST,:l],pred]]
  op='Tuple  => ['PAREN,['AGGLST,:l]]
  op='LISTOF => ['AGGLST,:l]
  IDENTP op and not (op in '(_* _*_*) ) and char("*") = (PNAME op).0 =>
    mkSuperSub(op,l)
  [outputTran op,:l]

-- The next two functions are designed to replace successive instances of
-- binary functions with the n-ary equivalent, cutting down on recursion
-- in outputTran and in partciular allowing big polynomials to be printed
-- without stack overflow.  MCD.
flattenOps l ==
  [op, :args ] := l
  op in ['"+",'"*","+","*"] =>
    [op,:checkArgs(op,args)]
  l

checkArgs(op,tail) ==
  head := []
  while tail repeat
    term := first tail
    atom term =>
      head := [term,:head]
      tail := rest tail
    not LISTP term => -- never happens?
      head := [term,:head]
      tail := rest tail
    op=first term =>
      tail := [:rest term,:rest tail]
    head := [term,:head]
    tail := rest tail
  REVERSE head
   
outputTranSEQ ['SEQ,:l,exitform] ==
  if exitform is ['exit,.,a] then exitform := a
  ['SC,:[outputTran x for x in l],outputTran exitform]

outputTranIf ['IF,x,y,z] ==
  y = 'noBranch =>
    ['CONCATB,'if,['CONCATB,'not,outputTran x],'then,outputTran z]
  z = 'noBranch =>
    ['CONCATB,'if,outputTran x,'then,outputTran y]
  y' := outputTran y
  z' := outputTran z
--y' is ['SC,:.] or z' is ['SC,:.] =>
-- ['CONCATB,'if,outputTran x,
--   ['SC,['CONCATB,'then,y'],['CONCATB,'else,z']]]
--['CONCATB,'if,outputTran x,'then,outputTran y,'else,outputTran z]
  ['CONCATB,'if,outputTran x,
    ['SC,['CONCATB,'then,y'],['CONCATB,'else,z']]]

outputMapTran l ==
  null l => NIL         -- should not happen

  -- display subscripts linearly
  $linearFormatScripts : local := true

  -- get the real names of the parameters
  alias := get($op,'alias,$InteractiveFrame)

  rest l =>             -- if multiple forms, call repeatedly
    ['SC,:[outputMapTran0(ll,alias) for ll in l]]
  outputMapTran0(first l,alias)

outputMapTran0(argDef,alias) ==
  arg := first argDef
  def := rest  argDef
  [arg',:def'] := simplifyMapPattern(argDef,alias)
  arg' := outputTran arg'
  if null arg' then arg' := '"()"
  ['CONCATB,$op,outputTran arg',"==",outputTran def']

outputTranReduce ['REDUCE,op,.,body] ==
  ['CONCAT,op,"/",outputTran body]

outputTranRepeat ["REPEAT",:itl,body] ==
  body' := outputTran body
  itl =>
    itlist:= outputTranIteration itl
    ['CONCATB,itlist,'repeat,body']
  ['CONCATB,'repeat,body']

outputTranCollect [.,:itl,body] ==
  itlist:= outputTranIteration itl
  ['BRACKET,['CONCATB,outputTran body,itlist]]

outputTranIteration itl ==
  null rest itl => outputTranIterate first itl
  ['CONCATB,outputTranIterate first itl,outputTranIteration rest itl]

outputTranIterate x ==
  x is ['STEP,n,init,step,:final] =>
    init' := outputTran init
    if LISTP init then init' := ['PAREN,init']
    final' :=
      final =>
        LISTP first final => [['PAREN,outputTran first final]]
        [outputTran first final]
      NIL
    ['STEP,outputTran n,init',outputTran step,:final']
  x is ["IN",n,s] => ["IN",outputTran n,outputTran s]
  x is [op,p] and op in '(_| UNTIL WHILE) =>
    op:= DOWNCASE op
    ['CONCATB,op,outputTran p]
  throwKeyedMsg("S2IX0008",['outputTranIterate,['"illegal iterate: ",x]])

outputConstructTran x ==
  x is [op,a,b] =>
    a:= outputTran a
    b:= outputTran b
    op="cons" =>
      b is ['construct,:l] => ['construct,a,:l]
      ['BRACKET,['AGGLST,:[a,[":",b]]]]
    op="nconc" =>
      aPart :=
        a is ['construct,c] and c is ['SEGMENT,:.] => c
        [":",a]
      b is ['construct,:l] => ['construct,aPart,:l]
      ['BRACKET,['AGGLST,aPart,[":",b]]]
    [op,a,b]
  atom x => x
  [outputTran first x,:outputConstructTran rest x]

outputTranMatrix x ==
  not VECP x =>
    -- assume that the only reason is that we've been done before
    ["MATRIX",:x]
    --keyedSystemError("S2GE0016",['"outputTranMatrix",
    -- '"improper internal form for matrix found in output routines"])
  ["MATRIX",nil,:[outtranRow x.i for i in 0..MAXINDEX x]] where
    outtranRow x ==
      not VECP x =>
        keyedSystemError("S2GE0016",['"outputTranMatrix",
          '"improper internal form for matrix found in output routines"])
      ["ROW",:[outputTran x.i for i in 0..MAXINDEX x]]

mkSuperSub(op,argl) ==
  $linearFormatScripts => linearFormatForm(op,argl)
--  l := [(STRINGP f => f; STRINGIMAGE f)
--    for f in linearFormatForm(op,argl)]
--  "STRCONC"/l
  s:= PNAME op
  indexList:= [PARSE_-INTEGER PNAME d for i in 1.. while
    (DIGITP (d:= s.(maxIndex:= i)))]
  cleanOp:= INTERN ("STRCONC"/[PNAME s.i for i in maxIndex..MAXINDEX s])
  -- if there is just a subscript use the SUB special form
  #indexList=2 =>
    subPart:= ['SUB,cleanOp,:take(indexList.1,argl)]
    l:= drop(indexList.1,argl) => [subPart,:l]
    subPart
  -- otherwise use the SUPERSUB form
  superSubPart := NIL
  for i in rest indexList repeat
    scripts :=
      this:= take(i,argl)
      argl:= drop(i,argl)
      i=0 => ['AGGLST]
      i=1 => first this
      ['AGGLST,:this]
    superSubPart := cons(scripts,superSubPart)
  superSub := ['SUPERSUB,cleanOp,:reverse superSubPart]
  argl => [superSub,:argl]
  superSub

timesApp(u,x,y,d) ==
  rightPrec:= getOpBindingPower("*","Led","right")
  firstTime:= true
  for arg in rest u repeat
    op:= keyp arg
    if not firstTime and (needBlankForRoot(lastOp,op,arg) or
       needStar(wasSimple,wasQuotient,wasNumber,arg,op) or
        wasNumber and op = 'ROOT and subspan arg = 1) then
      d:= APP(BLANK,x,y,d)
      x:= x+1
    [d,x]:= appInfixArg(arg,x,y,d,rightPrec,"left",nil) --app in a right arg
    wasSimple:= atom arg and not NUMBERP arg or isRationalNumber arg
    wasQuotient:= isQuotient op
    wasNumber:= NUMBERP arg
    lastOp := op
    firstTime:= nil
  d

needBlankForRoot(lastOp,op,arg) ==
  lastOp ^= "^" and lastOp ^= "**" and not(subspan(arg)>0) => false
  op = "**" and keyp CADR arg = 'ROOT => true
  op = "^" and keyp CADR arg = 'ROOT => true
  op = 'ROOT and CDDR arg => true
  false

stepApp([.,a,init,one,:optFinal],x,y,d) ==
  d:= appChar('"for ",x,y,d)
  d:= APP(a,w:=x+4,y,d)
  d:= appChar('" in ",w:=w+WIDTH a,y,d)
  d:= APP(init,w:=w+4,y,d)
  d:= APP('"..",w:=w+WIDTH init,y,d)
  if optFinal then d:= APP(first optFinal,w+2,y,d)
  d

stepSub [.,a,init,one,:optFinal] ==
  m:= MAX(subspan a,subspan init)
  optFinal => MAX(m,subspan first optFinal)
  m

stepSuper [.,a,init,one,:optFinal] ==
  m:= MAX(superspan a,superspan init)
  optFinal => MAX(m,superspan first optFinal)
  m

stepWidth [.,a,init,one,:optFinal] ==
   10+WIDTH a+WIDTH init+(optFinal => WIDTH first optFinal; 0)

inApp([.,a,s],x,y,d) ==  --for [IN,a,s]
  d:= appChar('"for ",x,y,d)
  d:= APP(a,x+4,y,d)
  d:= appChar('" in ",x+WIDTH a+4,y,d)
  APP(s,x+WIDTH a+8,y,d)

inSub [.,a,s] == MAX(subspan a,subspan s)

inSuper [.,a,s] == MAX(superspan a,superspan s)

inWidth [.,a,s] == 8+WIDTH a+WIDTH s

centerApp([.,u],x,y,d) ==
  d := APP(u,x,y,d)

concatApp([.,:l],x,y,d) == concatApp1(l,x,y,d,0)

concatbApp([.,:l],x,y,d) == concatApp1(l,x,y,d,1)

concatApp1(l,x,y,d,n) ==
  for u in l repeat
    d:= APP(u,x,y,d)
    x:=x+WIDTH u+n
  d

concatSub [.,:l] == "MAX"/[subspan x for x in l]

concatSuper [.,:l] == "MAX"/[superspan x for x in l]

concatWidth [.,:l] == +/[WIDTH x for x in l]

concatbWidth [.,:l] == +/[1+WIDTH x for x in l]-1

exptApp([.,a,b],x,y,d) ==
  pren:= exptNeedsPren a
  d:=
    pren => appparu(a,x,y,d)
    APP(a,x,y,d)
  x':= x+WIDTH a+(pren => 2;0)
  y':= 1+y+superspan a+subspan b + (0=superspan a => 0; -1)
  APP(b,x',y',d)

exptNeedsPren a ==
  atom a and null (INTEGERP a and a < 0)  => false
  key:= keyp a
  key = "OVER" => true  -- added JHD 2/Aug/90
  (key="SUB") or (null GETL(key,"Nud") and null GETL(key,"Led")) => false
  true

exptSub u == subspan CADR u

exptSuper [.,a,b] == superspan a+height b+(superspan a=0 => 0;-1)

exptWidth [.,a,b] == WIDTH a+WIDTH b+(exptNeedsPren a => 2;0)

needStar(wasSimple,wasQuotient,wasNumber,cur,op) ==
  wasQuotient or isQuotient op => true
  wasSimple =>
    atom cur or keyp cur="SUB" or isRationalNumber cur or op="**" or op = "^" or
      (atom op and not NUMBERP op and not GETL(op,"APP"))
  wasNumber =>
    NUMBERP(cur) or isRationalNumber cur or
        ((op="**" or op ="^") and NUMBERP(CADR cur))

isQuotient op ==
  op="/" or op="OVER"

timesWidth u ==
  rightPrec:= getOpBindingPower("*","Led","right")
  firstTime:= true
  w:= 0
  for arg in rest u repeat
    op:= keyp arg
    if not firstTime and needStar(wasSimple,wasQuotient,wasNumber,arg,op) then
      w:= w+1
    if infixArgNeedsParens(arg, rightPrec, "left") then w:= w+2
    w:= w+WIDTH arg
    wasSimple:= atom arg and not NUMBERP arg --or isRationalNumber arg
    wasQuotient:= isQuotient op
    wasNumber:= NUMBERP arg
    firstTime:= nil
  w

plusApp([.,frst,:rst],x,y,d) ==
  appSum(rst,x+WIDTH frst,y,APP(frst,x,y,d))

appSum(u,x,y,d) ==
  for arg in u repeat
    infixOp:=
      syminusp arg => "-"
      "+"
    opString:= GETL(infixOp,"INFIXOP") or '","
    d:= APP(opString,x,y,d)
    x:= x+WIDTH opString
    arg:= absym arg --negate a neg. number or remove leading "-"
    rightPrec:= getOpBindingPower(infixOp,"Led","right")
    if infixOp = "-" then rightPrec:=rightPrec  +1
    -- that +1 added JHD 2 Aug 89 to prevent x-(y+z) printing as x-y+z
    -- Sutor found the example:
    -- )cl all
    -- p : P[x] P I := x - y - z
    -- p :: P[x] FR P I
    -- trailingCoef %
    [d,x]:= appInfixArg(arg,x,y,d,rightPrec,"left",nil) --app in a right arg
  d

appInfix(e,x,y,d) ==
  op := keyp e
  leftPrec:= getOpBindingPower(op,"Led","left")
  leftPrec = 1000 => return nil --no infix operator is allowed default value
  rightPrec:= getOpBindingPower(op,"Led","right")
  #e < 2 => throwKeyedMsg("S2IX0008",['appInfix,
      '"fewer than 2 arguments to an infix function"])
  opString:= GETL(op,"INFIXOP") or '","
  opWidth:= WIDTH opString
  [.,frst,:rst]:= e
  null rst =>
    GETL(op,"isSuffix") =>
      [d,x]:= appInfixArg(frst,x,y,d,leftPrec,"right",opString)
      d:= appChar(opString,x,y,d)
    THROW('outputFailure,'outputFailure)
  [d,x]:= appInfixArg(frst,x,y,d,leftPrec,"right",opString) --app in left arg
  for arg in rst repeat
    d:= appChar(opString,x,y,d) --app in the infix operator
    x:= x+opWidth
    [d,x]:= appInfixArg(arg,x,y,d,rightPrec,"left",opString) --app in right arg
  d

appconc(d,x,y,w) == NCONC(d,[[[x,:y],:w]])

infixArgNeedsParens(arg, prec, leftOrRight) ==
  prec > getBindingPowerOf(leftOrRight, arg) + 1

appInfixArg(u,x,y,d,prec,leftOrRight,string) ==
  insertPrensIfTrue:= infixArgNeedsParens(u,prec,leftOrRight)
  d:=
    insertPrensIfTrue => appparu(u,x,y,d)
    APP(u,x,y,d)
  x:= x+WIDTH u
  if string then d:= appconc(d,x,y,string)
  [d,(insertPrensIfTrue => x+2; x)]

getBindingPowerOf(key,x) ==
  --binding powers can be found in file NEWAUX LISP
  x is ['REDUCE,:.] => (key='left => 130; key='right => 0)
  x is ["REPEAT",:.] => (key="left" => 130; key="right" => 0)
  x is ["COND",:.] => (key="left" => 130; key="right" => 0)
  x is [op,:argl] =>
    if op is [a,:.] then op:= a
    op = 'SLASH => getBindingPowerOf(key,["/",:argl]) - 1
    op = 'OVER  => getBindingPowerOf(key,["/",:argl])
    (n:= #argl)=1 =>
      key="left" and (m:= getOpBindingPower(op,"Nud","left")) => m
      key="right" and (m:= getOpBindingPower(op,"Nud","right")) => m
      1000
    n>1 =>
      key="left" and (m:= getOpBindingPower(op,"Led","left")) => m
      key="right" and (m:= getOpBindingPower(op,"Led","right")) => m
      op="ELT" => 1002
      1000
    1000
  1002

getOpBindingPower(op,LedOrNud,leftOrRight) ==
  if op in '(SLASH OVER) then op := "/"
  exception:=
    leftOrRight="left" => 0
    105
  bp:=
    leftOrRight="left" => leftBindingPowerOf(op,LedOrNud)
    rightBindingPowerOf(op,LedOrNud)
  bp^=exception => bp
  1000

--% Brackets
bracketApp(u,x,y,d) ==
  u is [.,u] or THROW('outputFailure,'outputFailure)
  d:= appChar(specialChar 'lbrk,x,y,d)
  d:=APP(u,x+1,y,d)
  appChar(specialChar 'rbrk,x+1+WIDTH u,y,d)

--% Braces
braceApp(u,x,y,d) ==
  u is [.,u] or THROW('outputFailure,'outputFailure)
  d:= appChar(specialChar 'lbrc,x,y,d)
  d:=APP(u,x+1,y,d)
  appChar(specialChar 'rbrc,x+1+WIDTH u,y,d)

--% Aggregates
aggWidth u ==
  rest u is [a,:l] => WIDTH a + +/[1+WIDTH x for x in l]
  0

aggSub u == subspan rest u

aggSuper u == superspan rest u

aggApp(u,x,y,d) == aggregateApp(rest u,x,y,d,",")

aggregateApp(u,x,y,d,s) ==
  if u is [a,:l] then
    d:= APP(a,x,y,d)
    x:= x+WIDTH a
    for b in l repeat
      d:= APP(s,x,y,d)
      d:= APP(b,x+1,y,d)
      x:= x+1+WIDTH b
  d

--% Function to compute Width

outformWidth u ==  --WIDTH as called from OUTFORM to do a COPY
  STRINGP u =>
    u = $EmptyString => 0
    u.0="%" and ((u.1 = char 'b) or (u.1 = char 'd)) => 1
    #u
  atom u => # atom2String u
  WIDTH COPY u

WIDTH u ==
  STRINGP u =>
    u = $EmptyString => 0
    u.0="%" and ((u.1 = char 'b) or (u.1 = char 'd)) => 1
    #u
  INTEGERP u => 
    if (u < 0) then 
      negative := 1
      u := -u
    else
      negative := 0

    -- Try and be fairly exact for smallish integers:
    u < 100000000 =>
        l :=
              u < 10 =>       1
              u < 100 =>      2
              u < 1000 =>     3
              u < 10000 =>    4
              u < 100000 =>   5
              u < 1000000 =>  6
              u < 10000000 => 7
              8
        l + negative
    k := INTEGER_-LENGTH(u)
    k > MOST_-POSITIVE_-DOUBLE_-FLOAT =>
        SAY("Number too big")
        THROW('outputFailure,'outputFailure)

    if (k < 61) then
        l10 := LOG10 (FLOAT (u, 1.0))
    else
        su := ASH(u, - (k - 54))
        l10 := LOG10 (FLOAT (su, 1.0)) 
              -- we want full double precision here because the second
              -- term may be much bigger than the first one, so we use
              -- very precise estimate of log(2)/log(10)
              + 0.301029995663981195213738894724 * FLOAT ((k - 54), 1.0)
    -- Add bias to l10 to have only one-sided error
    l10i := FLOOR(l10 + 1.0e-9)

    l10i < 10000 =>
       -- Check if sure
       l10 - 1.0e-9 > l10i => 1 + negative + l10i
       u < EXPT(10, l10i) => negative + l10i
       1 + negative + l10i

    -- width is very large, it would be expensive to compute it
    -- accuratly, so we just make sure that we overestimate.
    -- l10 should have about 14 digits of accuracy
    1 + negative + FLOOR(l10 * (1.0 + 1.0e-12))

  atom u => # atom2String u
  putWidth u is [[.,:n],:.] => n
  THROW('outputFailure,'outputFailure)

putWidth u ==
  atom u or u is [[.,:n],:.] and NUMBERP n => u
  op:= keyp u
--NUMBERP op => nil
  leftPrec:= getBindingPowerOf("left",u)
  rightPrec:= getBindingPowerOf("right",u)
  [firstEl,:l] := u
  interSpace:=
    GETL(firstEl,"INFIXOP") => 0
    1
  argsWidth:=
    l is [firstArg,:restArg] =>
      RPLACA(rest u,putWidth firstArg)
      for y in tails restArg repeat RPLACA(y,putWidth first y)
      widthFirstArg:=
        0=interSpace and infixArgNeedsParens(firstArg,leftPrec,"right")=>
          2+WIDTH firstArg
        WIDTH firstArg
      widthFirstArg + +/[interSpace+w for x in restArg] where w ==
        0=interSpace and infixArgNeedsParens(x, rightPrec, "left") =>
          2+WIDTH x
        WIDTH x
    0
  newFirst:=
    atom (oldFirst:= first u) =>
      fn:= GETL(oldFirst,"WIDTH") =>
        [oldFirst,:FUNCALL(fn,[oldFirst,:l])]
      if l then ll := rest l else ll := nil
      [oldFirst,:opWidth(oldFirst,ll)+argsWidth]
    [putWidth oldFirst,:2+WIDTH oldFirst+argsWidth]
  RPLACA(u,newFirst)
  u

opWidth(op,has2Arguments) ==
  op = "EQUATNUM" => 4
  NUMBERP op => 2+SIZE STRINGIMAGE op
  null has2Arguments =>
    a:= GETL(op,"PREFIXOP") => SIZE a
    2+SIZE PNAME op
  a:= GETL(op,"INFIXOP") => SIZE a
  2+SIZE PNAME op

matrixBorder(x,y1,y2,d,leftOrRight) ==
  y1 = y2 =>
    c :=
      leftOrRight = 'left => specialChar('lbrk)
      specialChar('rbrk)
    APP(c,x,y1,d)
  for y in y1..y2 repeat
    c :=
      y = y1 =>
        leftOrRight = 'left => specialChar('llc)
        specialChar('lrc)
      y = y2 =>
        leftOrRight = 'left => specialChar('ulc)
        specialChar('urc)
      specialChar('vbar)
    d := APP(c,x,y,d)
  d

isRationalNumber x == nil

widthSC u == 10000

--% The over-large matrix package

maprinSpecial(x,$MARGIN,$LINELENGTH) == maprin0 x
-- above line changed JHD 13/2/93 since it used to call maPrin

maprin x ==
  if $demoFlag=true then recordOrCompareDemoResult x
  CATCH('output,maprin0 x)
  nil

maprin0 x ==
  $MatrixCount:local :=0
  $MatrixList:local :=nil
  maprinChk x
  if $MatrixList then maprinRows $MatrixList
  -- above line moved JHD 28/2/93 to catch all routes through maprinChk

maprinChk x ==
  null $MatrixList => maPrin x
  ATOM x and (u:= assoc(x,$MatrixList)) =>
    $MatrixList := delete(u,$MatrixList)
    maPrin deMatrix CDR u
  x is ["=",arg,y]  =>     --case for tracing with )math and printing matrices
    u:= assoc(y,$MatrixList) =>
      -- we don't want to print matrix1 = matrix2 ...
      $MatrixList := delete(u,$MatrixList)
      maPrin ["=",arg, deMatrix CDR u]
    maPrin x
  x is ['EQUATNUM,n,y] =>
    $MatrixList is [[name,:value]] and y=name =>
      $MatrixList:=[]   -- we are pulling this one off
      maPrin ['EQUATNUM,n, deMatrix value]
    IDENTP y => --------this part is never called
      -- Not true: JHD 28/2/93
      -- m:=[[1,2,3],[4,5,6],[7,8,9]]
      -- mm:=[[m,1,0],[0,m,1],[0,1,m]]
      -- and try to print mm**5
      u := assoc(y,$MatrixList)
      --$MatrixList := deleteAssoc(first u,$MatrixList)
      -- deleteAssoc no longer exists
      $MatrixList := delete(u,$MatrixList)
      maPrin ['EQUATNUM,n,rest u]
      if not $collectOutput then TERPRI $algebraOutputStream
    maPrin x
  maPrin x
  -- above line added JHD 13/2/93 since otherwise x gets lost

maprinRows matrixList ==
  if not $collectOutput then TERPRI($algebraOutputStream)
  while matrixList repeat
    y:=NREVERSE matrixList
    --Makes the matrices come out in order, since CONSed on backwards
    matrixList:=nil
    firstName := first first y
    for [name,:m] in y for n in 0.. repeat
      if not $collectOutput then TERPRI($algebraOutputStream)
      andWhere := (name = firstName => '"where "; '"and ")
      line := STRCONC(andWhere, PNAME name)
      maprinChk ["=",line,m]
      -- note that this could place a new element on $MatrixList, hence the loop

deMatrix m ==
    ['BRACKET,['AGGLST,
        :[['BRACKET,['AGGLST,:rest row]] for row in CDDR m]]]

LargeMatrixp(u,width, dist) ==
  --  sees if there is a matrix wider than 'width' in the next 'dist'
  --  part of u, a sized charybdis structure.
  --  NIL if not, first such matrix if there is one
  ATOM u => nil
  CDAR u <= width => nil
       --CDAR is the width of a charybdis structure
  op:=CAAR u
  op = 'MATRIX => largeMatrixAlist u
         --We already know the structure is more than 'width' wide
  MEMQ(op,'(LET RARROW SEGMENT _- CONCAT CONCATB PAREN BRACKET BRACE)) =>
      --Each of these prints the arguments in a width 3 smaller
    dist:=dist-3
    width:=width-3
    ans:=
      for v in CDR u repeat
        (ans:=LargeMatrixp(v,width,dist)) => return largeMatrixAlist ans
        dist:=dist - WIDTH v
        dist<0 => return nil
    ans
      --Relying that falling out of a loop gives nil
  MEMQ(op,'(_+ _* )) =>
      --Each of these prints the first argument in a width 3 smaller
    (ans:=LargeMatrixp(CADR u,width-3,dist)) => largeMatrixAlist ans
    n:=3+WIDTH CADR u
    dist:=dist-n
    ans:=
      for v in CDDR u repeat
        (ans:=LargeMatrixp(v,width,dist)) => return largeMatrixAlist ans
        dist:=dist - WIDTH v
        dist<0 => return nil
    ans
      --Relying that falling out of a loop gives nil
  ans:=
    for v in CDR u repeat
      (ans:=LargeMatrixp(v,width,dist)) => return largeMatrixAlist ans
      dist:=dist - WIDTH v
      dist<0 => return nil
  ans
    --Relying that falling out of a loop gives nil

largeMatrixAlist u ==
  u is [op,:r] =>
    op is ['MATRIX,:.] => deMatrix u
    largeMatrixAlist op or largeMatrixAlist r
  nil

PushMatrix m ==
    --Adds the matrix to the look-aside list, and returns a name for it
  name:=
    for v in $MatrixList repeat
        EQUAL(m,CDR v) => return CAR v
  name => name
  name:=INTERNL('"matrix",STRINGIMAGE($MatrixCount:=$MatrixCount+1))
  $MatrixList:=[[name,:m],:$MatrixList]
  name

quoteApp([.,a],x,y,d) == APP(a,x+1,y,appChar(PNAME "'",x,y,d))

quoteSub [.,a] == subspan a

quoteSuper [.,a] == superspan a

quoteWidth [.,a] == 1 + WIDTH a

SubstWhileDesizing(u) ==
    --Replaces all occurrences of matrix by name in u
    --Taking out any outdated size information as it goes
  ATOM u => u
  [[op,:n],:l]:=u
  --name := rassoc(u,$MatrixList) => name
  -- doesn't work since rassoc seems to use an EQ test, and returns the
  -- pair anyway. JHD 28/2/93
  op = 'MATRIX =>
    l':=SubstWhileDesizingList(CDR l)
    u :=
      -- CDR l=l' => u
      -- this was a CONS-saving optimisation, but it doesn't work JHD 28/2/93
      [op,nil,:l']
    PushMatrix u
  l':=SubstWhileDesizingList(l)
  -- [op,:l']
  ATOM op => [op,:l']
  [SubstWhileDesizing(op),:l']


SubstWhileDesizingList(u) ==
   u is [a,:b] =>
     res:= 
       ATOM a => [a] 
       [SubstWhileDesizing(a)] 
     tail:=res
     for i in b repeat
        if ATOM i then  RPLACD(tail,[i]) else RPLACD(tail,[SubstWhileDesizing(i)])
        tail:=CDR tail
     res   
   u  

--% Printing of Sigmas , Pis and Intsigns

sigmaSub u ==
       --The depth function for sigmas with lower limit only
  MAX(1 + height CADR u, subspan CADDR u)

sigmaSup u ==
       --The height function for sigmas with lower limit only
  MAX(1, superspan CADDR u)

sigmaApp(u,x,y,d) ==
  u is [.,bot,arg] or THROW('outputFailure,'outputFailure)
  bigopAppAux(bot,nil,arg,x,y,d,'sigma)

sigma2App(u,x,y,d) ==
  [.,bot,top,arg]:=u
  bigopAppAux(bot,top,arg,x,y,d,'sigma)

bigopWidth(bot,top,arg,kind) ==
  kindWidth := (kind = 'pi => 5; 3)
  MAX(kindWidth,WIDTH bot,(top => WIDTH top; 0)) + 2 + WIDTH arg

bigopAppAux(bot,top,arg,x,y,d,kind) ==
  botWidth := (bot => WIDTH bot; 0)
  topWidth := WIDTH top
  opWidth :=
    kind = 'pi => 5
    3
  maxWidth := MAX(opWidth,botWidth,topWidth)
  xCenter := (maxWidth-1)/ 2 + x
  d:=APP(arg,x+2+maxWidth,y,d)
  d:=
      atom bot and SIZE atom2String bot = 1 => APP(bot,xCenter,y-2,d)
      APP(bot,x + (maxWidth - botWidth)/2,y-2-superspan bot,d)
  if top then
    d:=
      atom top and SIZE atom2String top = 1 => APP(top,xCenter,y+2,d)
      APP(top,x + (maxWidth - topWidth)/2,y+2+subspan top,d)
  delta := (kind = 'pi => 2; 1)
  opCode :=
    kind = 'sigma =>
      [['(0 .  0),:'">"],_
       ['(0 .  1),:specialChar('hbar)],_
       ['(0 . -1),:specialChar('hbar)],_
       ['(1 .  1),:specialChar('hbar)],_
       ['(1 . -1),:specialChar('hbar)],_
       ['(2 .  1),:specialChar('urc )],_
       ['(2 . -1),:specialChar('lrc )]]
    kind = 'pi =>
      [['(0 .  1),:specialChar('ulc )],_
       ['(1 .  0),:specialChar('vbar)],_
       ['(1 .  1),:specialChar('ttee)],_
       ['(1 . -1),:specialChar('vbar)],_
       ['(2 .  1),:specialChar('hbar)],_
       ['(3 .  0),:specialChar('vbar)],_
       ['(3 .  1),:specialChar('ttee)],_
       ['(3 . -1),:specialChar('vbar)],_
       ['(4 .  1),:specialChar('urc )]]
    THROW('outputFailure,'outputFailure)
  xLate(opCode,xCenter - delta,y,d)

sigmaWidth [.,bot,arg] == bigopWidth(bot,nil,arg,'sigma)
sigma2Width [.,bot,top,arg] == bigopWidth(bot,top,arg,'sigma)

sigma2Sub u ==
       --The depth function for sigmas with 2 limits
  MAX(1 + height CADR u, subspan CADDDR u)

sigma2Sup u ==
       --The depth function for sigmas with 2 limits
  MAX(1 + height CADDR u, superspan CADDDR u)

piSub u ==
       --The depth function for pi's (products)
  MAX(1 + height CADR u, subspan CADDR u)

piSup u ==
       --The height function for pi's (products)
  MAX(1, superspan CADDR u)

piApp(u,x,y,d) ==
  u is [.,bot,arg] or THROW('outputFailure,'outputFailure)
  bigopAppAux(bot,nil,arg,x,y,d,'pi)

piWidth [.,bot,arg] == bigopWidth(bot,nil,arg,'pi)
pi2Width [.,bot,top,arg] == bigopWidth(bot,top,arg,'pi)

pi2Sub u ==
       --The depth function for pi's with 2 limits
  MAX(1 + height CADR u, subspan CADDDR u)

pi2Sup u ==
       --The depth function for pi's with 2 limits
  MAX(1 + height CADDR u, superspan CADDDR u)

pi2App(u,x,y,d) ==
  [.,bot,top,arg]:=u
  bigopAppAux(bot,top,arg,x,y,d,'pi)

overlabelSuper [.,a,b] == 1 + height a + superspan b

overlabelWidth [.,a,b] == WIDTH b

overlabelApp([.,a,b], x, y, d) ==
  underApp:= APP(b,x,y,d)
  endPoint := x + WIDTH b - 1
  middle := QUOTIENT(x + endPoint,2)
  h := y + superspan b + 1
  d := APP(a,middle,h + 1,d)
  apphor(x,x+WIDTH b-1,y+superspan b+1,d,"|")

overbarSuper u == 1 + superspan u.1

overbarWidth u == WIDTH u.1

overbarApp(u,x,y,d) ==
  underApp:= APP(u.1,x,y,d)
  apphor(x,x+WIDTH u.1-1,y+superspan u.1+1,d,UNDERBAR)

indefIntegralSub u ==
   -- form is INDEFINTEGRAL(expr,dx)
   MAX(1,subspan u.1,subspan u.2)

indefIntegralSup u ==
   -- form is INDEFINTEGRAL(expr,dx)
   MAX(1,superspan u.1,superspan u.2)

indefIntegralApp(u,x,y,d) ==
   -- form is INDEFINTEGRAL(expr,dx)
  [.,expr,dx]:=u
  d := APP(expr,x+4,y,d)
  d := APP(dx,x+5+WIDTH expr,y,d)
  xLate( [['(0 . -1),:specialChar('llc) ],_
          ['(1 . -1),:specialChar('lrc) ],_
          ['(1 .  0),:specialChar('vbar)],_
          ['(1 .  1),:specialChar('ulc) ],_
          ['(2 .  1),:specialChar('urc) ]], x,y,d)

indefIntegralWidth u ==
  -- form is INDEFINTEGRAL(expr,dx)
  # u ^= 3 => THROW('outputFailure,'outputFailure)
  5 + WIDTH u.1 + WIDTH u.2

intSub u ==
   MAX(1 + height u.1, subspan u.3)

intSup u ==
   MAX(1 + height u.2, superspan u.3)

intApp(u,x,y,d) ==
  [.,bot,top,arg]:=u
  d:=APP(arg,x+4+MAX(-4 + WIDTH bot, WIDTH top),y,d)
  d:=APP(bot,x,y-2-superspan bot,d)
  d:=APP(top,x+3,y+2+subspan top,d)
  xLate( [['(0 . -1),:specialChar('llc) ],_
          ['(1 . -1),:specialChar('lrc) ],_
          ['(1 .  0),:specialChar('vbar)],_
          ['(1 .  1),:specialChar('ulc) ],_
          ['(2 .  1),:specialChar('urc) ]], x,y,d)

intWidth u ==
  # u < 4 => THROW('outputFailure,'outputFailure)
  MAX(-4 + WIDTH u.1, WIDTH u.2) + WIDTH u.3 + 5

xLate(l,x,y,d) ==
  for [[a,:b],:c] in l repeat
    d:= appChar(c,x+a,y+b,d)
  d

concatTrouble(u,d,start,lineLength,$addBlankIfTrue) ==
  [x,:l] := splitConcat(u,lineLength,true)
  null l =>
    sayALGEBRA ['%l,'%b,'"  Too wide to Print",'%d]
    THROW('output,nil)
  charybdis(fixUp x,start,lineLength)
  for y in l repeat
    if d then prnd(start,d)
    if lineLength > 2 then
       charybdis(fixUp y,start+2,lineLength-2) -- JHD needs this to avoid lunacy
      else charybdis(fixUp y,start,1) -- JHD needs this to avoid lunacy
  BLANK
 where
  fixUp x ==
    rest x =>
      $addBlankIfTrue => ['CONCATB,:x]
      ["CONCAT",:x]
    first x

splitConcat(list,maxWidth,firstTimeIfTrue) ==
  null list => nil
  -- split list l into a list of n lists, each of which
  -- has width < maxWidth
  totalWidth:= 0
  oneOrZero := ($addBlankIfTrue => 1; 0)
  l := list
  maxW:= (firstTimeIfTrue => maxWidth; maxWidth-2)
  maxW < 1 => [[x] for x in l] -- JHD 22.8.95, otherwise things can break
  for x in tails l
    while (width := oneOrZero + WIDTH first x + totalWidth) < maxW repeat
      l:= x
      totalWidth:= width
  x:= rest l
  RPLAC(rest l,nil)
  [list,:splitConcat(x,maxWidth,nil)]

spadPrint(x,m) ==
  m = $NoValueMode => x
  if not $collectOutput then TERPRI $algebraOutputStream
  output(x,m)
  if not $collectOutput then TERPRI $algebraOutputStream

formulaFormat expr ==
  sff := '(ScriptFormulaFormat)
  formatFn := getFunctionFromDomain("coerce",sff,[$OutputForm])
  displayFn := getFunctionFromDomain("display",sff,[sff])
  SPADCALL(SPADCALL(expr,formatFn),displayFn)
  if not $collectOutput then
    TERPRI $algebraOutputStream
    FORCE_-OUTPUT $formulaOutputStream
  NIL

texFormat expr ==
  ioHook("startTeXOutput")
  tf := '(TexFormat)
  formatFn := 
    getFunctionFromDomain("convert",tf,[$OutputForm,$Integer])
  displayFn := getFunctionFromDomain("display",tf,[tf])
  SPADCALL(SPADCALL(expr,$IOindex,formatFn),displayFn)
  TERPRI $texOutputStream
  FORCE_-OUTPUT $texOutputStream
  ioHook("endOfTeXOutput")
  NIL

texFormat1 expr ==
  tf := '(TexFormat)
  formatFn := getFunctionFromDomain("coerce",tf, [$OutputForm])
  displayFn := getFunctionFromDomain("display",tf,[tf])
  SPADCALL(SPADCALL(expr,formatFn),displayFn)
  TERPRI $texOutputStream
  FORCE_-OUTPUT $texOutputStream
  NIL

mathmlFormat expr ==
  mml := '(MathMLFormat)
  mmlrep := '(String)
  formatFn := getFunctionFromDomain("coerce",mml,[$OutputForm])
  displayFn := getFunctionFromDomain("display",mml,[mmlrep])
  SPADCALL(SPADCALL(expr,formatFn),displayFn)
  TERPRI $mathmlOutputStream
  FORCE_-OUTPUT $mathmlOutputStream
  NIL


output(expr,domain) ==
  if isWrapped expr then expr := unwrap expr
  isMapExpr expr =>
    if $formulaFormat then formulaFormat expr
    if $texFormat     then texFormat expr
    if $algebraFormat then mathprintWithNumber expr
    if $mathmlFormat  then mathmlFormat expr
  categoryForm? domain or domain in '((Mode) (Domain) (SubDomain (Domain))) =>
    if $algebraFormat then
      mathprintWithNumber outputDomainConstructor expr
    if $texFormat     then
      texFormat outputDomainConstructor expr
  T := coerceInteractive(objNewWrap(expr,domain),$OutputForm) =>
    x := objValUnwrap T
    if $formulaFormat then formulaFormat x
    if $fortranFormat then
      dispfortexp x
      if not $collectOutput then TERPRI $fortranOutputStream
      FORCE_-OUTPUT $fortranOutputStream
    if $algebraFormat then
      mathprintWithNumber x
    if $texFormat     then texFormat x
    if $mathmlFormat  then mathmlFormat x
  (FUNCTIONP(opOf domain)) and (not(SYMBOLP(opOf domain))) and
    (printfun := compiledLookup("<<",'(TextWriter TextWriter $), evalDomain domain))
       and (textwrit := compiledLookup("print", '($), TextWriter())) =>
     sayMSGNT [:bright '"AXIOM-XL",'"output:   "]
     SPADCALL(SPADCALL textwrit, expr, printfun)
     sayMSGNT '%l

  -- big hack for tuples for new compiler
  domain is ['Tuple, S] => output(asTupleAsList expr, ['List, S])

  sayALGEBRA [:bright '"LISP",'"output:",'%l,expr or '"NIL"]

outputNumber(start,linelength,num) ==
  if start > 1 then blnks := fillerSpaces(start-1,'" ")
  else blnks := '""
  under:='"__"
  firsttime:=(linelength>3)
  if linelength>2 then
     linelength:=linelength-1 
  while SIZE(num) > linelength repeat
    if $collectOutput then
       $outputLines := [CONCAT(blnks, SUBSTRING(num,0,linelength),under),
                        :$outputLines]
    else
      sayALGEBRA [blnks,
                  SUBSTRING(num,0,linelength),under]
    num := SUBSTRING(num,linelength,NIL)
    if firsttime then 
         blnks:=CONCAT(blnks,'" ")
         linelength:=linelength-1
         firsttime:=NIL
  if $collectOutput then
    $outputLines := [CONCAT(blnks, num), :$outputLines]
  else
    sayALGEBRA [blnks, num]

outputString(start,linelength,str) ==
  if start > 1 then blnks := fillerSpaces(start-1,'" ")
  else blnks := '""
  while SIZE(str) > linelength repeat
    if $collectOutput then
       $outputLines := [CONCAT(blnks, SUBSTRING(str,0,linelength)),
                        :$outputLines]
    else
      sayALGEBRA [blnks, SUBSTRING(str,0,linelength)]
    str := SUBSTRING(str,linelength,NIL)
  if $collectOutput then
    $outputLines := [CONCAT(blnks, str), :$outputLines]
  else
    sayALGEBRA [blnks, str]

outputDomainConstructor form ==
  if VECTORP form then form := devaluate form
  atom (u:= prefix2String form) => u
  v:= [object2String(x) for x in u]
  -- return INTERNL eval ['STRCONC,:v]
  return INTERN (FORMAT(NIL, '"~{~A~}", v))

getOutputAbbreviatedForm form ==
  form is [op,:argl] =>
    op in '(Union Record) => outputDomainConstructor form
    op is "Mapping" => formatMapping argl
    u:= constructor? op or op
    null argl => u
    ml:= getPartialConstructorModemapSig(op)
    argl:= [fn for x in argl for m in ml] where fn ==
      categoryForm?(m) => outputDomainConstructor x
      x' := coerceInteractive(objNewWrap(x,m),$OutputForm)
      x' => objValUnwrap x'
      '"unprintableObject"
    [u,:argl]
  form

outputOp x ==
  x is [op,:args] and (GETL(op,"LED") or GETL(op,"NUD")) =>
    n:=
      GETL(op,"NARY") => 2
      #args
    newop:= INTERN STRCONC("*",STRINGIMAGE n,PNAME op)
    [newop,:[outputOp y for y in args]]
  x

--% MAP PRINTER (FROM EV BOOT)

printMap u ==
  printBasic specialChar 'lbrk
  initialFlag:= isInitialMap u
  if u is [x,:l] then
    printMap1(x,initialFlag and x is [[n],:.] and n=1)
    for y in l repeat (printBasic " , "; printMap1(y,initialFlag))
  printBasic specialChar 'rbrk
  if not $collectOutput then TERPRI $algebraOutputStream

isInitialMap u ==
  u is [[[n],.],:l] and INTEGERP n and
    (and/[x is [[ =i],.] for x in l for i in n+1..])

printMap1(x,initialFlag) ==
  initialFlag => printBasic CADR x
  if CDAR x then printBasic first x else printBasic CAAR x
  printBasic " E "
  printBasic CADR x

printBasic x ==
  x='(One) => PRIN1(1,$algebraOutputStream)
  x='(Zero) => PRIN1(0,$algebraOutputStream)
  IDENTP x => PRINTEXP(PNAME x,$algebraOutputStream)
  atom x => PRIN1(x,$algebraOutputStream)
  PRIN0(x,$algebraOutputStream)

charybdis(u,start,linelength) ==
  EQ(keyp u,'EQUATNUM) and not (CDDR u) =>
    charybdis(['PAREN,u.1],start,linelength)
  charyTop(u,start,linelength)

charyTop(u,start,linelength) ==
  u is ['SC,:l] or u is [['SC,:.],:l] =>
    for a in l repeat charyTop(a,start,linelength)
    '" "
  u is [['CONCATB,:.],:m,[['SC,:.],:l]] =>
    charyTop(['CONCATB,:m],start,linelength)
    charyTop(['SC,:l],start+2,linelength-2)
  u is ['CENTER,a] =>
    b := charyTopWidth a
    (w := WIDTH(b)) > linelength-start => charyTop(a,start,linelength)
    charyTop(b,(linelength-start-w)/2,linelength)
  v := charyTopWidth u
  EQ(keyp u,'ELSE) => charyElse(u,v,start,linelength)
  WIDTH(v) > linelength => charyTrouble(u,v,start,linelength)
  d := APP(v,start,0,nil)
  n := superspan v
  m := - subspan v
-->
  $testOutputLineFlag =>
    $testOutputLineList :=
      [:ASSOCRIGHT SORTBY('CAR,d),:$testOutputLineList]
  until n < m repeat
    scylla(n,d)
    n := n - 1
  '" "

charyTopWidth u ==
    atom u => u
    atom first u => putWidth u
    NUMBERP CDAR u => u
    putWidth u

charyTrouble(u,v,start,linelength) ==
  al:= LargeMatrixp(u,linelength,2*linelength) =>
    --$MatrixList =>
      --[[m,:m1]] := al
      --maPrin sublisMatAlist(m,m1,u)
      --above three lines commented out JHD 25/2/93 since don't work
    u := SubstWhileDesizing(u)
    maprinChk u
  charyTrouble1(u,v,start,linelength)

sublisMatAlist(m,m1,u) ==
  u is [op,:r] =>
    op is ['MATRIX,:.] and u=m => m1
    op1 := sublisMatAlist(m,m1,op)
    r1 := [sublisMatAlist(m,m1,s) for s in r]
    op = op1 and r1 = r => u
    [op1,:r1]
  u

charyTrouble1(u,v,start,linelength) ==
  NUMBERP u => outputNumber(start,linelength,atom2String u)
  atom u => outputString(start,linelength,atom2String u)
  EQ(x:= keyp u,'_-) => charyMinus(u,v,start,linelength)
  MEMQ(x,'(_+ _* AGGLST)) => charySplit(u,v,start,linelength)
  EQ(x,'EQUATNUM) => charyEquatnum(u,v,start,linelength)
  d := GETL(x,'INFIXOP) => charyBinary(d,u,v,start,linelength)
  x = 'OVER  =>
    charyBinary(GETL("/",'INFIXOP),u,v,start,linelength)
  EQ(3,LENGTH u) and GETL(x,'Led) =>
    d:= PNAME first GETL(x,'Led)
    charyBinary(d,u,v,start,linelength)
  EQ(x,'CONCAT) =>
    concatTrouble(rest v,d,start,linelength,nil)
  EQ(x,'CONCATB) =>
    (rest v) is [loop, 'repeat, body] =>
      charyTop(['CONCATB,loop,'repeat],start,linelength)
      charyTop(body,start+2,linelength-2)
    (rest v) is [wu, loop, 'repeat, body] and
      (keyp wu) is ['CONCATB,wu',.] and wu' in '(while until) =>
        charyTop(['CONCATB,wu,loop,'repeat],start,linelength)
        charyTop(body,start+2,linelength-2)
    concatTrouble(rest v,d,start,linelength,true)
  GETL(x,'INFIXOP) => charySplit(u,v,start,linelength)
  EQ(x,'PAREN) and
    (EQ(keyp u.1,'AGGLST) and (v:= ",") or EQ(keyp u.1,'AGGSET) and
      (v:= ";")) => bracketagglist(rest u.1,start,linelength,v,"_(","_)")
  EQ(x,'PAREN) and EQ(keyp u.1,'CONCATB) =>
    bracketagglist(rest u.1,start,linelength," ","_(","_)")
  EQ(x,'BRACKET) and (EQ(keyp u.1,'AGGLST) and (v:= ",")) =>
    bracketagglist(rest u.1,start,linelength,v,
                   specialChar 'lbrk, specialChar 'rbrk)
  EQ(x,'BRACE) and (EQ(keyp u.1,'AGGLST) and (v:= ",")) =>
    bracketagglist(rest u.1,start,linelength,v,
                   specialChar 'lbrc, specialChar 'rbrc)
  EQ(x,'EXT) => longext(u,start,linelength)
  EQ(x,'MATRIX) => MATUNWND()
  EQ(x,'ELSE) => charyElse(u,v,start,linelength)
  EQ(x,'SC) => charySemiColon(u,v,start,linelength)
  charybdis(x,start,linelength)
  if rest u then charybdis(['ELSE,:rest u],start,linelength)
  -- changed from charybdis(...) by JHD 2 Aug 89, since rest u might be null
  '" "

charySemiColon(u,v,start,linelength) ==
  for a in rest u repeat
    charyTop(a,start,linelength)
  nil

charyMinus(u,v,start,linelength) ==
  charybdis('"-",start,linelength)
  charybdis(v.1,start+3,linelength-3)
  '" "

charyBinary(d,u,v,start,linelength) ==
  d in '(" := " "= ") =>
    charybdis(['CONCATB,v.1,d],start,linelength)
    charybdis(v.2,start+2,linelength-2)
    '" "
  charybdis(v.1,start+2,linelength-2)
  if d then prnd(start,d)
  charybdis(v.2,start+2,linelength-2)
  '" "

charyEquatnum(u,v,start,linelength) ==
  charybdis(['PAREN,u.1],start,linelength)
  charybdis(u.2,start,linelength)
  '" "

charySplit(u,v,start,linelength) ==
  v:= [first v.0,:rest v]
  m:= rest v
  WIDTH v.1 > linelength-2 =>
    charybdis(v.1,start+2,linelength-2)
    not (CDDR v) => '" "
    dm:= CDDR v
    ddm:= rest dm
    split2(u,dm,ddm,start,linelength)
  for i in 0.. repeat
    dm := rest m
    ddm := rest dm
    RPLACD(dm,nil)
    WIDTH v > linelength - 2 => return nil
    RPLAC(first v, first v.0)
    RPLACD(dm,ddm)
    m := rest m
  RPLAC(first v,first v.0)
  RPLACD(m,nil)
  charybdis(v,start + 2,linelength - 2)
  split2(u,dm,ddm,start,linelength)

split2(u,dm,ddm,start,linelength) ==
--prnd(start,(d:= GETL(keyp u,'INFIXOP) => d; opSrch(keyp u,OPLIST)))
  prnd(start,(d:= GETL(keyp u,'INFIXOP) => d; '","))
  RPLACD(dm,ddm)
  m:= WIDTH [keyp u,:dm]<linelength-2
  charybdis([keyp u,:dm],(m => start+2; start),(m => linelength-2; linelength))
  '" "

charyElse(u,v,start,linelength) ==
  charybdis(v.1,start+3,linelength-3)
  not (CDDR u) => '" "
  prnd(start,'",")
  charybdis(['ELSE,:CDDR v],start,linelength)
  '" "

scylla(n,v) ==
  y := LASSOC(n,v)
  null y => nil
  if STRINGP(y) then y := DROPTRAILINGBLANKS COPY y
  if $collectOutput then
    $outputLines := [y, :$outputLines]
  else
    PRINTEXP(y,$algebraOutputStream)
    TERPRI $algebraOutputStream
  nil

keyp(u) ==
  atom u => nil
  atom first u => first u
  CAAR u

absym x ==
  (NUMBERP x) and (MINUSP x) => -x
  not (atom x) and (keyp(x) = '_-) => CADR x
  x

agg(n,u) ==
  (n = 1) => CADR u
  agg(n - 1, rest u)

aggwidth u ==
  null u => 0
  null rest u => WIDTH first u
  1 + (WIDTH first u) + (aggwidth rest u)

argsapp(u,x,y,d) == appargs(rest u,x,y,d)

subspan u ==
  atom u => 0
  NUMBERP rest u => subspan first u
  (not atom first u             and_
   atom CAAR u           and_
   not NUMBERP CAAR u    and_
   GETL(CAAR u, 'SUBSPAN)    )    =>
   APPLY(GETL(CAAR u, 'SUBSPAN), LIST u)
  MAX(subspan first u, subspan rest u)

agggsub u == subspan rest u

superspan u ==
  atom u => 0
  NUMBERP rest u => superspan first u
  (not atom first u               and_
   atom CAAR u             and_
   not NUMBERP CAAR u      and_
   GETL(CAAR u, 'SUPERSPAN)    )    =>
   APPLY(GETL(CAAR u, 'SUPERSPAN), LIST u)
  MAX(superspan first u, superspan rest u)

agggsuper u == superspan rest u

agggwidth u == aggwidth rest u

appagg(u,x,y,d) == appagg1(u,x,y,d,'",")

appagg1(u,x,y,d,s) ==
  null u => d
  null rest u => APP(first u,x,y,d)
  temp := x + WIDTH first u
  temparg1 := APP(first u,x,y,d)
  temparg2 := APP(s,temp,y,temparg1)
  appagg1(rest u, 1 + temp, y, temparg2,s)

--Note the similarity between the definition below of appargs and above
--of appagg. (why?)

appargs(u,x,y,d) == appargs1(u,x,y,d,'";")

--Note that the definition of appargs1 below is identical to that of
--appagg1 above except that the former calls appargs and the latter
--calls appagg.

appargs1(u,x,y,d,s) ==
  null u => d
  null rest u => APP(first u,x,y,d)
  temp := x + WIDTH first u
  temparg1 := APP(first u,x,y,d)
  temparg2 := APP(s,temp,y,temparg1)
  true => appargs(rest u, 1 + temp, y, temparg2)

apprpar(x, y, y1, y2, d) ==
  (not (_*TALLPAR) or (y2 - y1 < 2)) => APP('")", x, y, d)
  true => APP('")", x, y2, apprpar1(x, y, y1, y2 - 1, d))

apprpar1(x, y, y1, y2, d) ==
  (y1 = y2) => APP('")", x, y2, d)
  true => APP('")", x, y2, apprpar1(x, y, y1, y2 - 1, d))

applpar(x, y, y1, y2, d) ==
  (not (_*TALLPAR) or (y2 - y1 < 2)) => APP('"(", x, y, d)
  true => APP('"(", x, y2, applpar1(x, y, y1, y2 - 1, d))

applpar1(x, y, y1, y2, d) ==
  (y1 = y2) => APP('"(", x, y2, d)
  true => APP('"(", x, y2, applpar1(x, y, y1, y2 - 1, d))

--The body of the function appelse assigns 6 local variables.
--It then finishes by calling apprpar.

appelse(u,x,y,d) ==
  w := WIDTH CAAR u
  b := y - subspan rest u
  p := y + superspan rest u
  temparg1 := APP(keyp u, x, y, d)
  temparg2 := applpar(x + w, y, b, p, temparg1)
  temparg3 := appagg(rest u, x + 1 + w, y, temparg2)
  apprpar(x + 1 + w + aggwidth rest u, y, b, p, temparg3)

appext(u,x,y,d) ==
  xptr := x
  yptr := y - (subspan CADR u + superspan agg(3,u) + 1)
  d := APP(CADR u,x,y,d)
  d := APP(agg(2,u),xptr,yptr,d)
  xptr := xptr + WIDTH agg(2,u)
  d := APP('"=", xptr, yptr,d)
  d := APP(agg(3,u), 1 + xptr, yptr, d)
  yptr := y + 1 + superspan CADR u + SUBSPAD agg(4,u)
  d := APP(agg(4,u), x, yptr, d)
  temp := 1 + WIDTH agg(2,u) +  WIDTH agg(3,u)
  n := MAX(WIDTH CADR u, WIDTH agg(4,u), temp)
  if EQCAR(first(z := agg(5,u)), 'EXT) and
   (EQ(n,3) or (n > 3 and not (atom z)) ) then
     n := 1 + n
  d := APP(z, x + n, y, d)

apphor(x1,x2,y,d,char) ==
  temp := (x1 = x2 => d; apphor(x1, x2 - 1, y, d,char))
  APP(char, x2, y, temp)

syminusp x ==
  NUMBERP x => MINUSP x
  not (atom x) and EQ(keyp x,'_-)

appsum(u, x, y, d) ==
  null u => d
  ac := absym first u
  sc :=
    syminusp first u => '"-"
    true => '"+"
  dp := member(keyp absym first u, '(_+ _-))
  tempx := x + WIDTH ac + (dp => 5; true => 3)
  tempdblock :=
    temparg1 := APP(sc, x + 1, y, d)
    dp =>
      bot := y - subspan ac
      top := y + superspan ac
      temparg2 := applpar(x + 3, y, bot, top, temparg1)
      temparg3 := APP(ac, x + 4, y, temparg2)
      apprpar(x + 4 + WIDTH ac, y, bot, top, temparg3)
    true => APP(ac, x + 3, y, temparg1)
  appsum(rest u, tempx, y, tempdblock)

appneg(u, x, y, d) ==
  appsum(LIST u, x - 1, y, d)

appparu(u, x, y, d) ==
  bot := y - subspan u
  top := y + superspan u
  temparg1 := applpar(x, y, bot, top, d)
  temparg2 := APP(u, x + 1, y, temparg1)
  apprpar(x + 1 + WIDTH u, y, bot, top, temparg2)

appparu1(u, x, y, d) ==
  appparu(CADR u, x, y, d)

appsc(u, x, y, d) ==
  appagg1(rest u, x, y, d, '";")

appsetq(u, x, y, d) ==
  w := WIDTH first u
  temparg1 := APP(CADR u, x, y, d)
  temparg2 := APP('":", x + w, y, temparg1)
  APP(CADR rest u, x + 2 + w, y, temparg2)

appsub(u, x, y, d) ==
  temparg1 := x + WIDTH CADR u
  temparg2 := y - 1 - superspan CDDR u
  temparg3 := APP(CADR u, x, y, d)
  appagg(CDDR u, temparg1, temparg2, temparg3)

eq0(u) == 0

height(u) ==
  superspan(u) + 1 + subspan(u)

extsub(u) ==
  MAX(subspan agg(5, u), height(agg(3, u)), subspan CADR u  )

extsuper(u) ==
  MAX(superspan CADR u + height agg(4, u), superspan agg(5, u) )

extwidth(u) ==
  n := MAX(WIDTH CADR u,
           WIDTH agg(4, u),
           1 + WIDTH agg(2, u) + WIDTH agg(3, u) )
  nil or
         (EQCAR(first(z := agg(5, u)), 'EXT) and _
          (EQ(n, 3) or ((n > 3) and null atom z) )  =>
          n := 1 + n)
  true => n + WIDTH agg(5, u)

appfrac(u, x, y, d) ==
  -- Added "1+" to both QUOTIENT statements so that when exact centering is
  -- not possible, expressions are offset to the right rather than left.
  -- MCD 16-8-95
  w := WIDTH u
  tempx := x + QUOTIENT(1+w - WIDTH CADR rest u, 2)
  tempy := y - superspan CADR rest u - 1
  temparg3 := APP(CADR rest u, tempx, tempy, d)
  temparg4 := apphor(x, x + w - 1, y, temparg3,specialChar('hbar))
  APP(CADR u,
        x + QUOTIENT(1+w - WIDTH CADR u, 2),
          y + 1 + subspan CADR u,
            temparg4)

fracsub(u) == height CADR rest u

fracsuper(u) == height CADR u

fracwidth(u) ==
  numw := WIDTH (num := CADR u)
  denw := WIDTH (den := CADDR u)
  if num is [[op,:.],:.] and op = 'OVER then numw := numw + 2
  if den is [[op,:.],:.] and op = 'OVER then denw := denw + 2
  MAX(numw,denw)

slashSub u ==
  MAX(1,subspan(CADR u),subspan(CADR rest u))

slashSuper u ==
  MAX(1,superspan(CADR u),superspan(CADR rest u))

slashApp(u, x, y, d) ==
  -- to print things as a/b as opposed to
  --      a
  --      -
  --      b
  temparg1 := APP(CADR u, x, y, d)
  temparg2 := APP('"/", x + WIDTH CADR u, y, temparg1)
  APP(CADR rest u,
     x + 1 + WIDTH CADR u, y, temparg2)

slashWidth(u) ==
  -- to print things as a/b as opposed to
  --      a
  --      -
  --      b
  1 + WIDTH CADR u + WIDTH CADR rest u

longext(u, i, n) ==
  x := REVERSE u
  y := first x
  u := remWidth(REVERSEWOC(CONS('" ", rest x)))
  charybdis(u, i, n)
  if not $collectOutput then TERPRI $algebraOutputStream
  charybdis(CONS('ELSE, LIST y), i, n)
  '" "

appvertline(char, x, yl, yu, d) ==
  yu < yl => d
  temparg :=  appvertline(char, x, yl, yu - 1, d)
  true => APP(char, x, yu, temparg)

appHorizLine(xl, xu, y, d) ==
  xu < xl => d
  temparg :=  appHorizLine(xl, xu - 1, y, d)
  true => APP(MATBORCH, xu, y, temparg)

rootApp(u, x, y, d) ==
  widB := WIDTH u.1
  supB := superspan u.1
  subB := subspan u.1
  if #u > 2 then
    widR := WIDTH u.2
    subR := subspan u.2
    d    := APP(u.2,  x, y - subB + 1 + subR, d)
  else
    widR := 1
  d := APP(u.1, x + widR + 1, y, d)
  d := apphor(x+widR+1, x+widR+widB, y+supB+1, d, specialChar('hbar))
  d := appvertline(specialChar('vbar), x+widR, y - subB, y + supB, d)
  d := APP(specialChar('ulc), x+widR, y + supB+1, d)
  d := APP(specialChar('urc), x + widR + widB + 1, y + supB+1, d)
  d := APP(specialChar('bslash), x + widR - 1, y - subB, d)

boxApp(u, x, y, d) ==
  CDDR u => boxLApp(u, x, y, d)
  a := 1 + superspan u.1
  b := 1 + subspan u.1
  w := 2 + WIDTH u.1
  d := appvertline(specialChar('vbar), x,y - b + 1, y + a - 1, d)
  d := appvertline(specialChar('vbar), x + w + 1, y - b,y + a,d)
  d := apphor(x + 1, x + w, y - b, d, specialChar('hbar))
  d := apphor(x + 1, x + w, y + a, d, specialChar('hbar))
  d := APP(specialChar('ulc), x,         y + a, d)
  d := APP(specialChar('urc), x + w + 1, y + a, d)
  d := APP(specialChar('llc), x,         y - b, d)
  d := APP(specialChar('lrc), x + w + 1, y - b, d)
  d := APP(u.1, 2 + x, y, d)

boxLApp(u, x, y, d) ==
  la := superspan u.2
  lb := subspan u.2
  lw := 2 + WIDTH u.2
  lh := 2 + la + lb
  a := superspan u.1+1
  b := subspan u.1+1
  w := MAX(lw, 2 + WIDTH u.1)
  -- next line used to have h instead of lh
  top := y + a + lh
  d := appvertline(MATBORCH, x, y - b, top, d)
  d := appHorizLine(x + 1, x + w, top, d)
  d := APP(u.2, 2 + x, y + a + lb + 1, d)
  d := appHorizLine(x + 1, x + lw, y + a, d)
  nil or
     lw < w => d := appvertline(MATBORCH, x + lw + 1, y + a, top - 1, d)
  d := APP(u.1, 2 + x, y, d)
  d := appHorizLine(x + 1, x + w, y - b, d)
  d := appvertline(MATBORCH, x + w + 1, y - b, top, d)

boxSub(x) ==
  subspan x.1+1

boxSuper(x) ==
  null CDR x => 0
  hl :=
    null CDDR x => 0
    true => 2 + subspan x.2 + superspan x.2
  true => hl+1 + superspan x.1

boxWidth(x) ==
  null CDR x => 0
  wl :=
    null CDDR x => 0
    true => WIDTH x.2
  true => 4 + MAX(wl, WIDTH x.1)

nothingWidth x ==
    0
nothingSuper x ==
    0
nothingSub x ==
    0
nothingApp(u, x, y, d) ==
    d

zagApp(u, x, y, d) ==
    w := WIDTH u
    denx := x + QUOTIENT(w - WIDTH CADR rest u, 2)
    deny := y - superspan CADR rest u - 1
    d    := APP(CADR rest u, denx, deny, d)
    numx := x + QUOTIENT(w - WIDTH CADR u, 2)
    numy := y+1 + subspan CADR u
    d    := APP(CADR u, numx, numy, d)
    a := 1 + zagSuper u
    b := 1 + zagSub u
    d := appvertline(specialChar('vbar), x,         y - b, y - 1, d)
    d := appvertline(specialChar('vbar), x + w - 1, y + 1, y + a, d)
    d := apphor(x, x + w - 2, y, d, specialChar('hbar))
    d := APP(specialChar('ulc), x, y, d)
    d := APP(specialChar('lrc), x + w - 1, y, d)

zagSub(u) ==
    height CADR rest u

zagSuper(u) ==
    height CADR u

zagWidth(x) ==
   #x = 1 => 0
   #x = 2 => 4 + WIDTH x.1
   4 + MAX(WIDTH x.1, WIDTH x.2)

rootWidth(x) ==
   #x <= 2 => 3 + WIDTH x.1
   2 + WIDTH x.1 + WIDTH x.2

rootSub(x) ==
   subspan x.1

rootSuper(x) ==
   normal := 1 + superspan x.1
   #x <= 2 => normal
   (radOver := height x.2 - height x.1) < 0 => normal
   normal + radOver

appmat(u, x, y, d) ==
   rows := CDDR u
   p := matSuper u
   q := matSub u
   d := matrixBorder(x, y - q, y + p, d, 'left)
   x := 1 + x
   yc := 1 + y + p
   w := CADR u
   wl := CDAR w
   subl := rest CADR w
   superl := rest CADR rest w
   repeat
      null rows => return(matrixBorder(x + WIDTH u - 2,
                                       y - q,
                                       y + p,
                                       d,
                                       'right))
      xc := x
      yc := yc - 1 - first superl
      w := wl
      row := CDAR rows
      repeat
            if flag = '"ON" then
               flag := '"OFF"
               return(nil)
            null row =>
                  repeat
                     yc := yc - 1 - first subl
                     subl := rest subl
                     superl := rest superl
                     rows := rest rows
                     return(flag  := '"ON"; nil)
            d := APP(first row,
                     xc + QUOTIENT(first w - WIDTH first row, 2),
                     yc,
                     d)
            xc := xc + 2 + first w
            row := rest row
            w := rest w

matSuper(x) ==
  (x := x.1) => -1 + QUOTIENT(first x.1 + first x.2, 2)
  true => ERROR('MAT)

matSub(x) ==
  (x := x.1) => QUOTIENT(-1 + first x.1 + first x.2, 2)
  true => ERROR('MAT)

matWidth(x) ==
  y := CDDR x  -- list of rows, each of form ((ROW . w) element element ...)
  numOfColumns := LENGTH CDAR y
  widthList := matLSum2 matWList(y, NLIST(numOfColumns, 0))
    --returns ["max width of entries in column i" for i in 1..numberOfRows]
  subspanList := matLSum matSubList y
  superspanList := matLSum matSuperList y
  RPLAC(x.1,[widthList, subspanList, superspanList])
  CAAR x.1

matLSum(x) ==
  CONS(sumoverlist x + LENGTH x, x)

matLSum2(x) ==
  CONS(sumoverlist x + 2*(LENGTH x), x)

matWList(x, y) ==
  null x => y
  true => matWList(rest x, matWList1(CDAR x, y) )

matWList1(x, y) ==
  null x => nil
  true => CONS(MAX(WIDTH first x, first y), matWList1(rest x, rest y) )

matSubList(x) ==  --computes the max/[subspan(e) for e in "row named x"]
  null x => nil
  true => CONS(matSubList1(CDAR x, 0), matSubList(rest x) )

matSubList1(x, y) ==
  null x => y
  true => matSubList1(rest x, MAX(y, subspan first x) )

matSuperList(x) ==  --computes the max/[superspan(e) for e in "row named x"]
  null x => nil
  true => CONS(matSuperList1(CDAR x, 0), matSuperList(rest x) )

matSuperList1(x, y) ==
  null x => y
  true => matSuperList1(rest x, MAX(y, superspan first x) )

minusWidth(u) ==
  -1 + sumWidthA rest u

-- opSrch(name, x) ==
--   LASSOC(name, x) or '","

bracketagglist(u, start, linelength, tchr, open, close) ==
  u := CONS(LIST('CONCAT, open, first u),
            [LIST('CONCAT, '" ", y) for y in rest u] )
  repeat
    s := 0
    for x in tails u repeat
             lastx := x
             ((s := s + WIDTH first x + 1) >= linelength) => return(s)
             null rest x => return(s := -1)
    nil or
       EQ(s, -1) => (nextu := nil)
       EQ(lastx, u) => ((nextu := rest u); RPLACD(u, nil) )
       true => ((nextu := lastx); RPLACD(PREDECESSOR(lastx, u), nil))
    for x in tails u repeat
           RPLACA(x, LIST('CONCAT, first x, tchr))
    if null nextu then RPLACA(CDDR last u, close)
    charybdis(ASSOCIATER('CONCAT, u), start, linelength)
    if $collectOutput then TERPRI $algebraOutputStream
    u := nextu
    null u => return(nil)

prnd(start, op) ==
-->
  $testOutputLineFlag =>
    string := STRCONC(fillerSpaces MAX(0,start - 1),op)
    $testOutputLineList := [string,:$testOutputLineList]
  PRINTEXP(fillerSpaces MAX(0,start - 1),$algebraOutputStream)
  $collectOutput =>
    string := STRCONC(fillerSpaces MAX(0,start - 1),op)
    $outputLines := [string, :$outputLines]
  PRINTEXP(op,$algebraOutputStream)
  TERPRI $algebraOutputStream

qTSub(u) ==
  subspan CADR u

qTSuper(u) ==
  superspan CADR u

qTWidth(u) ==
  2 + WIDTH CADR u

remWidth(x) ==
  atom x => x
  true => CONS( (atom first x => first x; true => CAAR x),
                MMAPCAR(remWidth, rest x) )

subSub(u) ==
  height CDDR u

subSuper u ==
  superspan u.1

letWidth u ==
  5 + WIDTH u.1 + WIDTH u.2

sumoverlist(u) == +/[x for x in u]

sumWidth u ==
  WIDTH u.1 + sumWidthA CDDR u

sumWidthA u ==
  not u => 0
  ( member(keyp absym first u,'(_+ _-)) => 5; true => 3) +
    WIDTH absym first u +
      sumWidthA rest u

superSubApp(u, x, y, di) ==
  a := first (u := rest u)
  b := first (u := rest u)
  c := first (u := KDR u) or '((NOTHING . 0))
  d := KAR   (u := KDR u) or '((NOTHING . 0))
  e := KADR  u            or '((NOTHING . 0))
  aox := MAX(wd := WIDTH d, we := WIDTH e)
  ar := superspan a
  ab := subspan a
  aw := WIDTH a
  di := APP(d, x + (aox - wd), 1 + ar + y + subspan d, di)
  di := APP(a, x + aox, y, di)
  di := APP(c, aox + aw + x, 1 + y + ar + subspan c, di)
  di := APP(e, x + (aox - we), y - 1 - MAX(superspan e, ab), di)
  di := APP(b, aox + aw + x, y - 1 - MAX(ab, superspan b), di)
  return di

stringer x ==
  STRINGP x => x
  EQ('_|, FETCHCHAR(s:= STRINGIMAGE x, 0)) =>
    RPLACSTR(s, 0, 1, "", nil, nil)
  s

superSubSub u ==
  a:= first (u:= rest u)
  b:= KAR (u := KDR u)
  e:= KAR KDR KDR KDR u
  return subspan a + MAX(height b, height e)

binomApp(u,x,y,d) ==
  [num,den] := rest u
  ysub := y - 1 - superspan den
  ysup := y + 1 + subspan num
  wden := WIDTH den
  wnum := WIDTH num
  w := MAX(wden,wnum)
  d := APP(den,x+1+(w - wden)/2,ysub,d)
  d := APP(num,x+1+(w - wnum)/2,ysup,d)
  hnum := height num
  hden := height den
  w := 1 + w
  for j in 0..(hnum - 1) repeat
    d := appChar(specialChar 'vbar,x,y + j,d)
    d := appChar(specialChar 'vbar,x + w,y + j,d)
  for j in 1..(hden - 1) repeat
    d := appChar(specialChar 'vbar,x,y - j,d)
    d := appChar(specialChar 'vbar,x + w,y - j,d)
  d := appChar(specialChar 'ulc,x,y + hnum,d)
  d := appChar(specialChar 'urc,x + w,y + hnum,d)
  d := appChar(specialChar 'llc,x,y - hden,d)
  d := appChar(specialChar 'lrc,x + w,y - hden,d)

binomSub u == height CADDR u
binomSuper u == height CADR u
binomWidth u == 2 + MAX(WIDTH CADR u, WIDTH CADDR u)

altSuperSubApp(u, x, y, di) ==
  a  := first (u := rest u)
  ar := superspan a
  ab := subspan a
  aw := WIDTH a
  di := APP(a, x, y, di)
  x  := x + aw

  sublist := everyNth(u := rest u, 2)
  suplist := everyNth(IFCDR u, 2)

  ysub := y - 1 - APPLY('MAX, [ab, :[superspan s for s in sublist]])
  ysup := y + 1 + APPLY('MAX, [ar, :[subspan   s for s in sublist]])
  for sub in sublist for sup in suplist repeat
      wsub := WIDTH sub
      wsup := WIDTH sup
      di := APP(sub, x, ysub, di)
      di := APP(sup, x, ysup, di)
      x := x + 1 + MAX(wsub, wsup)
  di

everyNth(l, n) ==
    [(e := l.0; for i in 1..n while l repeat l := rest l; e) while l]


altSuperSubSub u ==
  span := subspan CADR u
  sublist := everyNth(CDDR u, 2)
  for sub in sublist repeat
      h := height sub
      if h > span then span := h
  span

altSuperSubSuper u ==
  span := superspan CADR u
  suplist := everyNth(IFCDR CDDR u, 2)
  for sup in suplist repeat
      h := height sup
      if h > span then span := h
  span

altSuperSubWidth u ==
  w := WIDTH CADR u
  suplist := everyNth(IFCDR CDDR u, 2)
  sublist := everyNth(CDDR u, 2)
  for sup in suplist for sub in sublist repeat
      wsup := WIDTH sup
      wsub := WIDTH sub
      w := w + 1 + MAX(wsup, wsub)
  w

superSubWidth u ==
  a := first (u := rest u)
  b := first (u := rest u)
  c := first (u := KDR u) or '((NOTHING . 0))
  d := KAR   (u := KDR u) or '((NOTHING . 0))
  e := KADR  u            or '((NOTHING . 0))
  return MAX(WIDTH d, WIDTH e) + MAX(WIDTH b, WIDTH c) + WIDTH a

superSubSuper u ==
  a:= first (u := rest u)
  c:= KAR (u := KDR KDR u)
  d:= KADR u
  return superspan a + MAX(height c, height d)

suScWidth u ==
  WIDTH u.1 + aggwidth CDDR u

vconcatapp(u, x, y, d) ==
  w := vConcatWidth u
  y := y + superspan u.1 + 1
  for a in rest u repeat
      y := y - superspan a - 1
      xoff := QUOTIENT(w - WIDTH a, 2)
      d := APP(a, x + xoff, y, d)
      y := y - subspan a
  d

binomialApp(u, x, y, d) ==
  [.,b,a] := u
  w := vConcatWidth u
  d := APP('"(",x,y,d)
  x := x + 1
  y1 := y - height a
  xoff := QUOTIENT(w - WIDTH a, 2)
  d := APP(a, x + xoff, y1, d)
  y2 := y + height b
  xoff := QUOTIENT(w - WIDTH b, 2)
  d := APP(b, x + xoff, y2, d)
  x := x + w
  APP('")",x,y,d)

vConcatSub u ==
  subspan u.1 + +/[height a for a in CDDR u]
vConcatSuper u ==
  superspan u.1
vConcatWidth u ==
  w := 0
  for a in rest u repeat if (wa := WIDTH a) > w then w := wa
  w
binomialSub u ==  height u.2 + 1

binomialSuper u == height u.1 + 1

binomialWidth u == 2 + MAX(WIDTH u.1, WIDTH u.2)

mathPrint u ==
  if not $collectOutput then TERPRI $algebraOutputStream
  (u := STRINGP mathPrint1(mathPrintTran u, nil) =>
   PSTRING u; nil)

mathPrintTran u ==
  atom u => u
  true =>
    for x in tails u repeat
          RPLAC(first x, mathPrintTran first x)
    u

mathPrint1(x,fg) ==
  if fg and not $collectOutput then TERPRI $algebraOutputStream
  maPrin x
  if fg and not $collectOutput then TERPRI $algebraOutputStream

maPrin u ==
  null u => nil
-->
  if $runTestFlag or $mkTestFlag then
    $mkTestOutputStack := [COPY u, :$mkTestOutputStack]
  $highlightDelta := 0
  c := CATCH('outputFailure,charybdis(u, $MARGIN, $LINELENGTH))
  c ^= 'outputFailure => c
  sayKeyedMsg("S2IX0009",NIL)
  u is ['EQUATNUM,num,form] or u is [['EQUATNUM,:.],num,form] =>
    charybdis(['EQUATNUM,num], $MARGIN, $LINELENGTH)
    if not $collectOutput then
      TERPRI $algebraOutputStream
      PRETTYPRINT(form,$algebraOutputStream)
    form
  if not $collectOutput then PRETTYPRINT(u,$algebraOutputStream)
  nil
