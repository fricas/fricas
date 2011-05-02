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


)package "BOOTTRAN"

-- TRUE if we are currently building the syntax tree for an 'is' 
-- expression.
$inDefIS := false
 
bfGenSymbol()==
    $GenVarCounter:=$GenVarCounter+1
    INTERN(CONCAT ('"bfVar#",STRINGIMAGE $GenVarCounter))
 
bfListOf x==x
 
bfColon x== ["COLON",x]
 
bfSymbol x==
   STRINGP x=> x
   ['QUOTE,x]
 
bfDot()== "DOT"
 
bfSuffixDot x==[x,"DOT"]
 
bfEqual(name)== ["EQUAL",name]
 
bfBracket(part) == part
 
bfPile(part)    == part
 
bfAppend x== APPLY(function APPEND,x)
 
bfColonAppend (x,y) ==
     if null x
     then
      if y is ["BVQUOTE",:a]
      then ["&REST",["QUOTE",:a]]
      else ["&REST",y]
     else cons(CAR x,bfColonAppend(CDR x,y))
 
bfDefinition1(bflhsitems, bfrhs) ==
       ['DEF, bflhsitems,bfTuple nil, bfrhs]
 
bfDefinition2(bflhsitems, bfrhs,body) ==
       ['DEF,bflhsitems,bfrhs,body]
 
bfMDefinition2(bflhsitems, bfrhs,body) ==
       bfMDef('MDEF,bflhsitems,bfrhs,body)
 
bfCompDef [def,op,args,body]== bfDef(def,op,args,body)
 
bfBeginsDollar x==  EQL('"$".0,(PNAME x).0)
 
compFluid id== ["FLUID",id]
 
compFluidize x==
  IDENTP x and bfBeginsDollar x=>compFluid x
  ATOM x =>x
  EQCAR(x,"QUOTE")=>x
  cons(compFluidize(CAR x),compFluidize(CDR x))
 
bfTuple x== ["TUPLE",:x]
 
bfTupleP x==EQCAR(x,"TUPLE")
 
bfTupleIf x==
  if bfTupleP x
  then x
  else bfTuple x
 
bfTupleConstruct b ==
  a:= if bfTupleP b
      then cdr b
      else [b]
  or/[x is ["COLON",.] for x in a] => bfMakeCons a
  ["LIST",:a]
 
bfConstruct b ==
  a:= if bfTupleP b
      then cdr b
      else [b]
  bfMakeCons a
 
bfMakeCons l ==
  null l => NIL
  l is [["COLON",a],:l1] =>
    l1 => ['APPEND,a,bfMakeCons l1]
    a
  ['CONS,first l,bfMakeCons rest l]
 
bfFor(bflhs,U,step) ==
     if EQCAR (U,'tails)
     then  bfForTree('ON, bflhs, CADR U)
     else
       if EQCAR(U,"SEGMENT")
       then  bfSTEP(bflhs,CADR U,step,CADDR U)
       else  bfForTree('IN, bflhs, U)
 
bfForTree(OP,lhs,whole)==
         whole:=if bfTupleP whole then bfMakeCons cdr whole else whole
         ATOM lhs =>bfINON [OP,lhs,whole]
         lhs:=if bfTupleP lhs then CADR lhs else lhs
         EQCAR(lhs,"L%T") =>
             G:=CADR lhs
             [:bfINON [OP,G,whole],:bfSuchthat bfIS(G,CADDR lhs)]
         G:=bfGenSymbol()
         [:bfINON [OP,G,whole],:bfSuchthat bfIS(G,lhs)]
 
 
bfSTEP(id,fst,step,lst)==
      initvar:=[id]
      initval:=[fst]
      inc:=if ATOM step
           then step
           else
               g1:=bfGenSymbol()
               initvar:=cons(g1,initvar)
               initval:=cons(step,initval)
               g1
      final:=if ATOM lst
             then lst
             else
               g2:=bfGenSymbol()
               initvar:=cons(g2,initvar)
               initval:=cons(lst,initval)
               g2
      ex:=
          null lst=> []
          INTEGERP inc =>
              pred:=if MINUSP inc then "<" else ">"
              [[pred,id,final]]
          [['COND,[['MINUSP,inc],
                ["<",id,final]],['T,[">",id,final]]]]
      suc:=[['SETQ,id,["+",id,inc]]]
      [[initvar,initval,suc,[],ex,[]]]
 
 
bfINON x==
    [op,id,whole]:=x
    if EQ(op,"ON")
    then bfON(id,whole)
    else bfIN(id,whole)
 
bfIN(x,E)==
    g:=bfGenSymbol()
    [[[g,x],[E,nil],[['SETQ,g,['CDR, g]]],[],
        [['OR,['ATOM,g],['PROGN,['SETQ,x,['CAR,g]] ,'NIL]]],[]]]
 
bfON(x,E)==
    [[[x],[E],[['SETQ,x,['CDR, x]]],[],
        [['ATOM,x]],[]]]
 
bfSuchthat p== [[[],[],[],[p],[],[]]]
 
bfWhile p== [[[],[],[],[],[bfNOT p],[]]]
 
bfUntil p==
     g:=bfGenSymbol()
     [[[g],[nil],[['SETQ,g,p]],[],[g],[]]]
 
bfIterators x==["ITERATORS",:x]
 
bfCross x== ["CROSS",:x]
 
bfLp(iters,body)==
     EQCAR (iters,"ITERATORS")=>bfLp1(CDR iters,body)
     bfLpCross(CDR iters,body)
 
bfLpCross(iters,body)==
     if null cdr iters
     then bfLp(car iters,body)
     else bfLp(car iters,bfLpCross(cdr iters,body))
 
bfSep(iters)==
     if null iters
     then [[],[],[],[],[],[]]
     else
         f:=first iters
         r:=bfSep rest iters
         [append(i,j) for i in f for j in r]
 
bfReduce(op,y)==
     a:=if EQCAR(op,"QUOTE") then CADR op else op
     op:=bfReName a
     init:=GET(op,"SHOETHETA")
     g:=bfGenSymbol()
     g1:=bfGenSymbol()
     body := ["SETQ", g, [op, g, g1]]
     if null init
     then
        g2:=bfGenSymbol()
        init:=['CAR,g2]
        ny:=['CDR,g2]
        it:= ["ITERATORS",:[[[[g],[init],[],[],[],[g]]],bfIN(g1,ny)]]
        bfMKPROGN [['L%T,g2,y],bfLp(it,body)]
     else
        init:=car init
        it:= ["ITERATORS",:[[[[g],[init],[],[],[],[g]]],bfIN(g1,y)]]
        bfLp(it,body)
 
bfReduceCollect(op,y)==
   if EQCAR (y,"COLLECT")
   then
     body:=y.1
     itl:=y.2
     a:=if EQCAR(op,"QUOTE") then CADR op else op
     op:=bfReName a
     init:=GET(op,"SHOETHETA")
     bfOpReduce(op,init,body,itl)
   else
     a:=bfTupleConstruct (y.1)
     bfReduce(op,a)
 
-- delayed collect
 
bfDCollect(y,itl)== ["COLLECT",y,itl]
 
bfDTuple x== ["DTUPLE",x]
 
bfCollect(y,itl) ==
      y is ["COLON",a] => bf0APPEND(a,itl)
      y is ["TUPLE",:.] =>
        newBody:=bfConstruct y
        bf0APPEND(newBody,itl)
      bf0COLLECT(y,itl)
 
bf0COLLECT(y,itl)==bfListReduce('CONS,y,itl)
 
 
bf0APPEND(y,itl)==
     g:=bfGenSymbol()
     body:=['SETQ,g,['APPEND,['REVERSE,y],g]]
     extrait:= [[[g],[nil],[],[],[],[['NREVERSE,g]]]]
     bfLp2(extrait,itl,body)
 
bfListReduce(op,y,itl)==
     g:=bfGenSymbol()
     body:=['SETQ,g,[op,y,g]]
     extrait:= [[[g],[nil],[],[],[],[['NREVERSE,g]]]]
     bfLp2(extrait,itl,body)
 
bfLp1(iters,body)==
      [vars,inits,sucs,filters,exits,value]:=bfSep bfAppend iters
      nbody:=if null filters then body else bfAND [:filters,body]
      value:=if null value then "NIL" else car value
      exits:= ["COND",[bfOR exits,["RETURN",value]],
                  ['(QUOTE T),nbody]]
      loop:=
           [["LAMBDA",vars,
               ["LOOP",exits,:sucs]],:inits]
      loop
 
bfLp2(extrait,itl,body)==
     EQCAR (itl,"ITERATORS")=>bfLp1(cons(extrait,CDR itl),body)
     iters:=cdr itl
     bfLpCross
          ([["ITERATORS",extrait,:CDAR iters],:CDR iters],body)
 
bfOpReduce(op,init,y,itl)==
     g:=bfGenSymbol()
     body:=
         EQ(op,"AND")=>
                bfMKPROGN [["SETQ",g,y],
                    ['COND, [['NOT,g],['RETURN,'NIL]]]]
         EQ(op,"OR") =>
                bfMKPROGN [["SETQ",g,y],
                             ['COND, [g,['RETURN,g]]]]
         ['SETQ,g,[op,g,y]]
     if null init
     then
        g1:=bfGenSymbol()
        init:=['CAR,g1]
        y:=['CDR,g1]
        extrait:= [[[g],[init],[],[],[],[g]]]
        bfMKPROGN [['L%T,g1,y],bfLp2(extrait,itl,body)]
     else
        init:=car init
        extrait:= [[[g],[init],[],[],[],[g]]]
        bfLp2(extrait,itl,body)
 
bfLoop1 body == bfLp (bfIterators nil,body)
 
bfSegment1(lo)==     ["SEGMENT",lo,nil]
 
bfSegment2(lo,hi)==   ["SEGMENT",lo,hi]
 
bfForInBy(variable,collection,step)==
         bfFor(variable,collection,step)
 
bfForin(lhs,U)==bfFor(lhs,U,1)
 
bfLocal(a,b)==
         EQ(b,"FLUID")=>  compFluid a
         EQ(b,"fluid")=>  compFluid a
         EQ(b,"local") =>  compFluid a
    --   $typings:=cons(["TYPE",b,a],$typings)
         a
 
bfTake(n,x)==
     null x=>x
     n=0 => nil
     cons(car x,bfTake(n-1,cdr x))
 
bfDrop(n,x)==
     null x or n=0 =>x
     bfDrop(n-1,cdr x)
 
bfDefSequence l ==  ['SEQ,: l]
 
bfReturnNoName a ==
      ["RETURN",a]
 
bfSUBLIS(p,e)==
  ATOM e=>bfSUBLIS1(p,e)
  EQCAR(e,"QUOTE")=>e
  cons(bfSUBLIS(p,car e),bfSUBLIS(p,cdr e))
 
bfSUBLIS1(p,e)==
   null p =>e
   f:=CAR p
   EQ(CAR f,e)=>CDR f
   bfSUBLIS1(cdr p,e)
 
defSheepAndGoats(x)==
    EQCAR (x,"DEF") =>
        [def,op,args,body]:=x
        argl:=if bfTupleP args
              then cdr args
              else [args]
        if null argl
        then
          opassoc:=[[op,:body]]
          [opassoc,[],[]]
        else
          op1:=INTERN CONCAT(PNAME $op,'",",PNAME op)
          opassoc:=[[op,:op1]]
          defstack:=[["DEF",op1,args,body]]
          [opassoc,defstack,[]]
    EQCAR (x,"SEQ") =>  defSheepAndGoatsList(cdr x)
    [[],[],[x]]
 
defSheepAndGoatsList(x)==
     if null x
     then [[],[],[]]
     else
       [opassoc,defs,nondefs]    := defSheepAndGoats car x
       [opassoc1,defs1,nondefs1] := defSheepAndGoatsList cdr x
       [append(opassoc,opassoc1),append(defs,defs1),
            append(nondefs,nondefs1)]
--% LET
 
bfLetForm(lhs,rhs) ==   ['L%T,lhs,rhs]
 
bfLET1(lhs,rhs) ==
  IDENTP lhs         => bfLetForm(lhs,rhs)
  lhs is ['FLUID,.] => bfLetForm(lhs,rhs)
  IDENTP rhs and not bfCONTAINED(rhs,lhs) =>
    rhs1 := bfLET2(lhs,rhs)
    EQCAR(rhs1,'L%T) => bfMKPROGN [rhs1,rhs]
    EQCAR(rhs1,'PROGN) => APPEND(rhs1,[rhs])
    if IDENTP CAR rhs1 then rhs1 := CONS(rhs1,NIL)
    bfMKPROGN [:rhs1,rhs]
  CONSP(rhs) and EQCAR(rhs,'L%T) and IDENTP(name := CADR rhs) =>
    -- handle things like [a] := x := foo
    l1 := bfLET1(name,CADDR rhs)
    l2 := bfLET1(lhs,name)
    EQCAR(l2,'PROGN) => bfMKPROGN [l1,:CDR l2]
    if IDENTP CAR l2 then l2 := cons(l2,nil)
    bfMKPROGN [l1,:l2,name]
  g := INTERN CONCAT('"LETTMP#",STRINGIMAGE $letGenVarCounter)
  $letGenVarCounter := $letGenVarCounter + 1
  rhs1 := ['L%T,g,rhs]
  let1 := bfLET1(lhs,g)
  EQCAR(let1,'PROGN) => bfMKPROGN [rhs1,:CDR let1]
  if IDENTP CAR let1 then let1 := CONS(let1,NIL)
  bfMKPROGN [rhs1,:let1,g]
 
bfCONTAINED(x,y)==
    EQ(x,y) => true
    ATOM y=> false
    bfCONTAINED(x,car y) or bfCONTAINED(x,cdr y)
 
bfLET2(lhs,rhs) ==
  IDENTP lhs => bfLetForm(lhs,rhs)
  NULL lhs   => NIL
  lhs is ['FLUID,.] => bfLetForm(lhs,rhs)
  lhs is ['L%T,a,b] =>
    a := bfLET2(a,rhs)
    null (b := bfLET2(b,rhs)) => a
    ATOM b => [a,b]
    CONSP CAR b => CONS(a,b)
    [a,b]
  lhs is ['CONS,var1,var2] =>
    var1 = "DOT" or (CONSP(var1) and EQCAR(var1,'QUOTE)) =>
      bfLET2(var2,addCARorCDR('CDR,rhs))
    l1 := bfLET2(var1,addCARorCDR('CAR,rhs))
    null var2 or EQ(var2,"DOT") =>l1
    if CONSP l1 and ATOM CAR l1 then l1 := cons(l1,nil)
    IDENTP var2 =>
      [:l1,bfLetForm(var2,addCARorCDR('CDR,rhs))]
    l2 := bfLET2(var2,addCARorCDR('CDR,rhs))
    if CONSP l2 and ATOM CAR l2 then l2 := cons(l2,nil)
    APPEND(l1,l2)
  lhs is ['APPEND,var1,var2] =>
    patrev := bfISReverse(var2,var1)
    rev := ['REVERSE,rhs]
    g := INTERN CONCAT('"LETTMP#", STRINGIMAGE $letGenVarCounter)
    $letGenVarCounter := $letGenVarCounter + 1
    l2 := bfLET2(patrev,g)
    if CONSP l2 and ATOM CAR l2 then l2 := cons(l2,nil)
    var1 = "DOT" => [['L%T,g,rev],:l2]
    last l2 is ['L%T, =var1, val1] =>
      [['L%T,g,rev],:REVERSE CDR REVERSE l2,
       bfLetForm(var1,['NREVERSE,val1])]
    [['L%T,g,rev],:l2,bfLetForm(var1,['NREVERSE,var1])]
  lhs is ["EQUAL",var1] =>
    ['COND,[["EQUAL",var1,rhs],var1]]
  -- The original expression may be one that involves literals as 
  -- sub-patterns, e.g.
  --      ['SEQ, :l, ['exit, 1, x]] := item
  -- We continue the processing as if that expression had been written
  --      item is ['SEQ, :l, ['exit, 1, x]]
  -- and generate appropriate codes.
  --                  -- gdr/2007-04-02.
  isPred :=
    $inDefIS => bfIS1(rhs,lhs)
    bfIS(rhs,lhs)
  ['COND,[isPred,rhs]]
 
 
bfLET(lhs,rhs) ==
  $letGenVarCounter : local := 1
--  $inbfLet : local := true
  bfLET1(lhs,rhs)
 
addCARorCDR(acc,expr) ==
  NULL CONSP expr => [acc,expr]
  acc = 'CAR and EQCAR(expr,'REVERSE) =>
      ["CAR",["LAST",:CDR expr]]
 --   cons('last,CDR expr)
  funs := '(CAR CDR CAAR CDAR CADR CDDR CAAAR CADAR CAADR CADDR
            CDAAR CDDAR CDADR CDDDR)
  p := bfPosition(CAR expr,funs)
  p = -1 => [acc,expr]
  funsA := '(CAAR CADR CAAAR CADAR CAADR CADDR CAAAAR CAADAR CAAADR
             CAADDR CADAAR CADDAR CADADR CADDDR)
  funsR := '(CDAR CDDR CDAAR CDDAR CDADR CDDDR CDAAAR CDADAR CDAADR
             CDADDR CDDAAR CDDDAR CDDADR CDDDDR)
  if acc = 'CAR then CONS(funsA.p,CDR expr)
  else               CONS(funsR.p,CDR expr)
 
bfPosition(x,l) ==  bfPosn(x,l,0)
bfPosn(x,l,n) ==
      null l => -1
      x=first l => n
      bfPosn(x,rest l,n+1)
 
--% IS
 
bfISApplication(op,left,right)==
   EQ(op ,"IS")      => bfIS(left,right)
   EQ(op ,"ISNT")    => bfNOT bfIS(left,right)
   [op ,left,right]
 
bfIS(left,right)==
    $isGenVarCounter:local :=1
    $inDefIS :local :=true
    bfIS1(left,right)
 
bfISReverse(x,a) ==
  x is ['CONS,:.] =>
    NULL CADDR x => ['CONS,CADR x, a]
    y := bfISReverse(CADDR x, NIL)
    RPLACA(CDDR y,['CONS,CADR x,a])
    y
  bpSpecificErrorHere '"Error in bfISReverse"
  bpTrap()
 
bfIS1(lhs,rhs) ==
  NULL rhs =>
    ['NULL,lhs]
  STRINGP rhs =>
    ['EQUAL, lhs, ["QUOTE", rhs]]
  NUMBERP rhs =>
    ["EQUAL",lhs,rhs]
  ATOM rhs =>
    ['PROGN,bfLetForm(rhs,lhs),''T]
  rhs is ['QUOTE,a] =>
    IDENTP a => ['EQ,lhs,rhs]
    ["EQUAL",lhs,rhs]
  rhs is ['L%T,c,d] =>
    l :=
      bfLET(c,lhs)
--    $inbfLet => bfLET1(c,lhs)
--    bfLET(c,lhs)
    bfAND [bfIS1(lhs,d),bfMKPROGN [l,''T]]
  rhs is ["EQUAL",a] =>
    ["EQUAL",lhs,a]
  CONSP lhs =>
    g := INTERN CONCAT('"ISTMP#",STRINGIMAGE $isGenVarCounter)
    $isGenVarCounter := $isGenVarCounter + 1
    bfMKPROGN [['L%T,g,lhs],bfIS1(g,rhs)]
  rhs is ['CONS,a,b] =>
    a = "DOT" =>
      NULL b =>
        bfAND [['CONSP,lhs],
               ['EQ,['CDR,lhs],'NIL]]
      bfAND [['CONSP,lhs],
             bfIS1(['CDR,lhs],b)]
    NULL b =>
      bfAND [['CONSP,lhs],
             ['EQ,['CDR,lhs],'NIL],_
             bfIS1(['CAR,lhs],a)]
    b = "DOT" =>
      bfAND [['CONSP,lhs],bfIS1(['CAR,lhs],a)]
    a1 := bfIS1(['CAR,lhs],a)
    b1 := bfIS1(['CDR,lhs],b)
    a1 is ['PROGN,c,''T] and b1 is ['PROGN,:cls] =>
      bfAND [['CONSP,lhs],bfMKPROGN [c,:cls]]
    bfAND [['CONSP,lhs],a1,b1]
  rhs is ['APPEND,a,b] =>
    patrev := bfISReverse(b,a)
    g := INTERN CONCAT('"ISTMP#",STRINGIMAGE $isGenVarCounter)
    $isGenVarCounter := $isGenVarCounter + 1
    rev := bfAND [['CONSP,lhs],['PROGN,['L%T,g,['REVERSE,lhs]],''T]]
    l2 := bfIS1(g,patrev)
    if CONSP l2 and ATOM CAR l2 then l2 := cons(l2,nil)
    a = "DOT" => bfAND [rev,:l2]
    bfAND [rev,:l2,['PROGN,bfLetForm(a,['NREVERSE,a]),''T]]
  bpSpecificErrorHere '"bad IS code is generated"
  bpTrap()
 
bfApplication(bfop, bfarg) ==
         if bfTupleP bfarg
         then cons(bfop,CDR bfarg)
         else cons(bfop,[bfarg])
 
 
bfReName x==
   a:=GET(x,"SHOERENAME")
   if a
   then car a
   else  x
 
bfInfApplication(op,left,right)==
   EQ(op,"EQUAL") => bfQ(left,right)
   EQ(op,"/=")    => bfNOT bfQ(left,right)
   EQ(op,">")     => bfLessp(right,left)
   EQ(op,"<")     => bfLessp(left,right)
   EQ(op,"<=")    => bfNOT bfLessp(right,left)
   EQ(op,">=")    => bfNOT bfLessp(left,right)
   EQ(op,"OR")    => bfOR [left,right]
   EQ(op,"AND")   => bfAND [left,right]
   [op,left,right]
 
bfNOT x==
   x is ["NOT",a]=> a
   x is ["NULL",a]=> a
   ["NOT",x]
 
bfFlatten(op, x) ==
      EQCAR(x,op) => CDR x
      [x]
 
bfOR l  ==
       null l => NIL
       null cdr l => CAR l
       ["OR",:[:bfFlatten("OR",c) for c in l]]
 
bfAND l ==
       null l=> 'T
       null cdr l => CAR l
       ["AND",:[:bfFlatten("AND",c) for c in l]]
 
 
defQuoteId x==  EQCAR(x,"QUOTE") and IDENTP CADR x
 
bfSmintable x==
  INTEGERP x or CONSP x and
      MEMQ(CAR x, '(SIZE LENGTH))
 
bfQ(l,r)==
       if bfSmintable l or bfSmintable r
       then  ["EQL",l,r]
       else if defQuoteId l or defQuoteId r
            then  ["EQ",l,r]
            else
              if null l
              then ["NULL",r]
              else if null r
                   then ["NULL",l]
                   else ["EQUAL",l,r]
 
bfLessp(l,r)==
      if r=0
      then ["MINUSP", l]
      else ["<",l,r]
 
bfMDef (defOp,op,args,body) ==
  argl:=if bfTupleP args then cdr args else [args]
  [gargl,sgargl,nargl,largl]:=bfGargl argl
  sb:=[cons(i,j) for i in nargl for j in sgargl]
  body:= SUBLIS(sb,body)
  sb2 := [["CONS",["QUOTE",i],j] for i in sgargl for j in largl]
  body := ["SUBLIS",["LIST",:sb2],["QUOTE",body]]
  lamex:= ["MLAMBDA",gargl,body]
  def:= [op,lamex]
  bfTuple
     cons(shoeComp def,[:shoeComps bfDef1 d for d in $wheredefs])
 
bfGargl argl==
      if null argl
      then [[],[],[],[]]
      else
        [a,b,c,d]:=bfGargl cdr argl
        if car argl="&REST"
        then [cons(car argl,b),b,c,
             cons(["CONS",["QUOTE","LIST"],car d],cdr d)]
        else
            f:=bfGenSymbol()
            [cons(f,a),cons(f,b),cons(car argl,c),cons(f,d)]
 
bfDef1 [defOp,op,args,body] ==
  argl:=if bfTupleP args then cdr args else [args]
  [quotes,control,arglp,body]:=bfInsertLet (argl,body)
  quotes=>BREAK()
  [[op,["LAMBDA",arglp,body]]]
 
bfDef(defOp,op,args,body) ==
 $bfClamming =>
          [.,op1,arg1,:body1]:=shoeComp first bfDef1 [defOp,op,args,body]
          bfCompHash(op1,arg1,body1)
 bfTuple
  [:shoeComps bfDef1 d for d in  cons([defOp,op,args,body],$wheredefs)]
 
shoeComps  x==[shoeComp def for def in x]
shoeComp x==
     a:=shoeCompTran CADR x
     if EQCAR(a,"LAMBDA")
     then ["DEFUN",CAR x,CADR a,:CDDR a]
     else ["DEFMACRO",CAR x,CADR a,:CDDR a]
 
bfInsertLet(x,body)==
   if null x
   then [false,nil,x,body]
   else
      if x is ["&REST",a]
      then if a is ["QUOTE",b]
           then [true,"QUOTE",["&REST",b],body]
           else [false,nil,x,body]
      else
       [b,norq,name1,body1]:=  bfInsertLet1 (car x,body)
       [b1,norq1,name2,body2]:=  bfInsertLet (cdr x,body1)
       [b or b1,cons(norq,norq1),cons(name1,name2),body2]
 
bfInsertLet1(y,body)==
   if y is ["L%T",l,r]
   then  [false,nil,l,bfMKPROGN [bfLET(r,l),body]]
   else if IDENTP y
        then [false,nil,y,body]
        else
          if y is ["BVQUOTE",b]
          then [true,"QUOTE",b,body]
          else
            g:=bfGenSymbol()
            ATOM y => [false,nil,g,body]
            [false,nil,g,bfMKPROGN [bfLET(compFluidize y,g),body]]
 
shoeCompTran x==
   lamtype:=CAR x
   args   :=CADR x
   body   :=CDDR x
   $fluidVars:local:=nil
   $locVars:local:=nil
   $dollarVars:local:=nil
   shoeCompTran1 body
   $locVars:=SETDIFFERENCE(SETDIFFERENCE($locVars,
                                  $fluidVars),shoeATOMs args)
   body:=
       if $fluidVars or $locVars or $dollarVars or $typings
       then
         lvars:=append($fluidVars,$locVars)
         -- $fluidVars:=UNION($fluidVars,$dollarVars)
         if null $fluidVars
         then
            null $typings=> shoePROG(lvars,body)
            shoePROG(lvars,[["DECLARE",:$typings],:body])
         else
           fvars:=["DECLARE",["SPECIAL",:$fluidVars]]
           null $typings => shoePROG(lvars,[fvars,:body])
           shoePROG(lvars,[fvars,["DECLARE",:$typings],:body])
       else shoePROG([], body)
   fl:=shoeFluids args
   body:=if fl
         then
           fvs:=["DECLARE",["SPECIAL",:fl]]
           cons(fvs,body)
         else body
   [lamtype,args, :body]

shoePROG(v,b)==
    null b => [["PROG", v]]
    [:blist,blast] := b
    [["PROG",v,:blist,["RETURN", blast]]]

shoeFluids x==
         if null x
         then nil
         else if IDENTP x and bfBeginsDollar x
              then [x]
              else
                if EQCAR(x,"QUOTE")
                then []
                else
                  if ATOM x
                  then nil
                  else  append(shoeFluids car x,shoeFluids cdr x)
shoeATOMs x==
         if null x
         then nil
         else if ATOM x
              then [x]
              else append(shoeATOMs car x,shoeATOMs cdr x)
 
shoeCompTran1 x==
    ATOM x=>
                IDENTP x and bfBeginsDollar x=>
                    $dollarVars:=
                          MEMQ(x,$dollarVars)=>$dollarVars
                          cons(x,$dollarVars)
                nil
    U:=car x
    EQ(U,"QUOTE")=>nil
    x is ["L%T",l,r]=>
                RPLACA (x,"SETQ")
                shoeCompTran1 r
                IDENTP l =>
                  not bfBeginsDollar l=>
                    $locVars:=
                          MEMQ(l,$locVars)=>$locVars
                          cons(l,$locVars)
                  $dollarVars:=
                          MEMQ(l,$dollarVars)=>$dollarVars
                          cons(l,$dollarVars)
                EQCAR(l,"FLUID")=>
                    $fluidVars:=
                         MEMQ(CADR l,$fluidVars)=>$fluidVars
                         cons(CADR l,$fluidVars)
                    RPLACA (CDR x,CADR l)
    MEMQ(U,'(PROG LAMBDA))=>
         newbindings:=nil
         for y in CADR x repeat
             not MEMQ(y,$locVars)=>
                  $locVars:=cons(y,$locVars)
                  newbindings:=cons(y,newbindings)
         res:=shoeCompTran1 CDDR x
         $locVars:=[y for y in $locVars | not MEMQ(y,newbindings)]
    shoeCompTran1 car x
    shoeCompTran1 cdr x
 
bfTagged(a,b)==
    IDENTP a =>
         EQ(b,"FLUID") =>  bfLET(compFluid a,NIL)
         EQ(b,"fluid") =>  bfLET(compFluid a,NIL)
         EQ(b,"local") =>  bfLET(compFluid a,NIL)
         $typings:=cons(["TYPE",b,a],$typings)
         a
    ["THE",b,a]
 
bfAssign(l,r)==
   if bfTupleP l then bfSetelt(CADR l,CDDR l ,r) else bfLET(l,r)
 
bfSetelt(e,l,r)==
    if null cdr l
    then defSETELT(e,car l,r)
    else bfSetelt(bfElt(e,car l),cdr l,r)
 
bfElt(expr,sel)==
      y:=SYMBOLP sel and GET(sel,"SHOESELFUNCTION")
      y=>
         INTEGERP y => ["ELT",expr,y]
         [y,expr]
      ["ELT",expr,sel]
 
defSETELT(var,sel,expr)==
      y:=SYMBOLP sel and GET(sel,"SHOESELFUNCTION")
      y=>
         INTEGERP y => ["SETF",["ELT",var,y],expr]
         ["SETF",[y,var],expr]
      ["SETF",["ELT",var,sel],expr]
 
bfIfThenOnly(a,b)==
    b1:=if EQCAR (b,"PROGN") then CDR b else [b]
    ["COND",[a,:b1]]
 
bfIf(a,b,c)==
    b1:=if EQCAR (b,"PROGN") then CDR b else [b]
    EQCAR (c,"COND") => ["COND",[a,:b1],:CDR c]
    c1:=if EQCAR (c,"PROGN") then CDR c else [c]
    ["COND",[a,:b1],['(QUOTE T),:c1]]
 
bfExit(a,b)==  ["COND",[a,["IDENTITY",b]]]
 
bfMKPROGN l==
    a:=[:bfFlattenSeq c for c in tails l]
    null a=> nil
    null CDR a=> CAR a
    ["PROGN",:a]
 
bfFlattenSeq x ==
      null x=>NIL
      f:=CAR x
      ATOM f =>if CDR x then nil else [f]
      EQCAR(f,"PROGN") =>
              CDR x=>  [i for i in CDR f| not ATOM i]
              CDR f
      [f]
 
bfSequence l ==
      null l=> NIL
      transform:= [[a,b] for x in l while
              x is ["COND",[a,["IDENTITY",b]]]]
      no:=#transform
      before:= bfTake(no,l)
      aft   := bfDrop(no,l)
      null before =>
              null rest l =>
                   f:=first l
                   if EQCAR(f,"PROGN")
                   then bfSequence CDR f
                   else f
              bfMKPROGN [first l,bfSequence rest l]
      null aft => ["COND",:transform]
      ["COND",:transform,['(QUOTE T),bfSequence aft]]
 
bfWhere (context,expr)==
  [opassoc,defs,nondefs] := defSheepAndGoats context
  opassoc := [[op, :bfSUBLIS(opassoc, repl)]
               for d in opassoc | d is [op, :repl]]
  a:=[[def,op,args,bfSUBLIS(opassoc,body)]
               for d in defs  |d is [def,op,args,body]]
  $wheredefs:=append(a,$wheredefs)
  bfMKPROGN bfSUBLIS(opassoc,NCONC(nondefs,[expr]))
 
bfReadLisp string==bfTuple shoeReadLispString (string,0)
 
shoeReadLispString(s,n) ==
    l:=# s
    n >= l => nil
    READ_-FROM_-STRING CONCAT ( "(", SUBSTRING(s,n,l-n) ,")")

bfCompHash(op,argl,body) ==
  auxfn:= INTERN CONCAT (PNAME op,'";")
  computeFunction:= ["DEFUN",auxfn,argl,:body]
  bfTuple [computeFunction,:bfMain(auxfn,op)]
 
bfMain(auxfn,op)==
  g1:= bfGenSymbol()
  arg:=["&REST",g1]
  computeValue := ['APPLY,["FUNCTION",auxfn],g1]
  cacheName:= INTERN CONCAT (PNAME op,'";AL")
  g2:= bfGenSymbol()
  getCode:=   ['GETHASH,g1,cacheName]
  secondPredPair:= [['SETQ,g2,getCode],g2]
  putCode:=   ['SETF ,getCode,computeValue]
  thirdPredPair:= ['(QUOTE T),putCode]
  codeBody:= ['PROG,[g2],
               ['RETURN,['COND,secondPredPair,thirdPredPair]]]
  mainFunction:= ["DEFUN",op,arg,codeBody]
 
  cacheType:=     'hash_-table
  cacheResetCode:=   ['SETQ,cacheName,['MAKE_-HASHTABLE,
                        ["QUOTE","UEQUAL"]]]
  cacheCountCode:= ['hashCount,cacheName]
  cacheVector:=
      [op,cacheName,cacheType,cacheResetCode,cacheCountCode]
  [mainFunction,
    shoeEVALANDFILEACTQ
      ["SETF",["GET",
           ["QUOTE", op],["QUOTE",'cacheInfo]],["QUOTE", cacheVector]],
            shoeEVALANDFILEACTQ  cacheResetCode ]
 

 
bfNameOnly x==
      if x="t"
      then ["T"]
      else  [x]
 
bfNameArgs (x,y)==
    y:=if EQCAR(y,"TUPLE") then CDR y else [y]
    cons(x,y)
 
bfStruct(name,arglist)==
  bfTuple [bfCreateDef i for i in arglist]
 
bfCreateDef x==
     if null cdr x
     then
       f:=car x
       ["SETQ",f,["LIST",["QUOTE",f]]]
     else
       a:=[bfGenSymbol() for i in cdr x]
       ["DEFUN",car x,a,["CONS",["QUOTE",car x],["LIST",:a]]]
 
bfCaseItem(x,y)==[x,y]
 
bfCase(x,y)==
         g:=bfGenSymbol()
         g1:=bfGenSymbol()
         a:=bfLET(g,x)
         b:=bfLET(g1,["CDR",g])
         c:=bfCaseItems (g1,y)
         bfMKPROGN [a,b,["CASE",["CAR", g],:c]]
 
bfCaseItems(g,x)==  [bfCI(g,i,j) for [i,j] in x]
 
bfCI(g,x,y)==
    a:=cdr x
    if null a
    then [car x,y]
    else
       b:=[[i,bfCARCDR(j,g)] for i in a for j in 0..]
       [car x,["LET",b,y]]
 
bfCARCDR (n,g)==[INTERN CONCAT ('"CA",bfDs n,'"R"),g]
 
bfDs n== if n=0 then '"" else CONCAT('"D",bfDs(n-1))

