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

--% OPTIMIZER

optimizeFunctionDef(def) ==
  if $reportOptimization then
    sayBrightlyI bright '"Original LISP code:"
    pp def

  def' := optimize COPY def

  if $reportOptimization then
    sayBrightlyI bright '"Optimized LISP code:"
    pp def'
    sayBrightlyI bright '"Final LISP code:"
  [name,[slamOrLam,args,body]] := def'

  body':=
    removeTopLevelCatch body where
      removeTopLevelCatch body ==
        body is ["CATCH",g,u] =>
          removeTopLevelCatch replaceThrowByReturn(u,g)
        body
      replaceThrowByReturn(x,g) ==
        fn(x,g)
        x
      fn(x,g) ==
        x is ["THROW", =g,:u] =>
          rplac(first x,"RETURN")
          rplac(rest x,replaceThrowByReturn(u,g))
        atom x => nil
        replaceThrowByReturn(first x,g)
        replaceThrowByReturn(rest x,g)
  [name,[slamOrLam,args,body']]

lispize x == first optimize [x]

optimize x ==
  (opt x; x) where
    opt x ==
      atom x => nil
      (y:= first x)='QUOTE => nil
      y='CLOSEDFN => nil
      y is [["XLAM",argl,body],:a] =>
        optimize rest x
        argl = "ignore" => rplac(first x, body)
        if not (LENGTH argl<=LENGTH a) then
          SAY '"length mismatch in XLAM expression"
          PRETTYPRINT y
        rplac(first x, optimize optXLAMCond SUBLIS(pairList(argl, a), body))
      atom y =>
        optimize rest x
        y = "true" => rplac(first x, '(QUOTE (QUOTE T)))
        y = "false" => rplac(first x, nil)
      if first y = "IF" then (rplac(first x, optIF2COND y); y := first x)
      op := GET(subrname first y, "OPTIMIZE") =>
        (optimize rest x; rplac(first x, FUNCALL(op, optimize first x)))
      rplac(first x, optimize first x)
      optimize rest x

subrname u ==
  IDENTP u => u
  nil

optCatch (x is ["CATCH",g,a]) ==
  $InteractiveMode => x
  atom a => a
  if a is ["SEQ",:s,["THROW", =g,u]] then
    changeThrowToExit(s,g) where
      changeThrowToExit(s,g) ==
        atom s or MEMQ(first s,'(QUOTE SEQ REPEAT COLLECT)) => nil
        s is ["THROW", =g,:u] => (rplac(first s,"EXIT"); rplac(rest s,u))
        changeThrowToExit(first s,g)
        changeThrowToExit(rest s,g)
    rplac(rest a,[:s,["EXIT",u]])
    ["CATCH",y,a]:= optimize x
  if hasNoThrows(a, g) where
      hasNoThrows(a, g) ==
        a is ["THROW", =g,:.] => false
        atom a => true
        hasNoThrows(first a,g) and hasNoThrows(rest a,g)
     then (rplac(first x, first a); rplac(rest x, rest a))
   else
    val_sym := GENSYM()
    changeThrowToGo(a, g, val_sym) where
        changeThrowToGo(s, g, val_sym) ==
            atom s or first s='QUOTE => nil
            s is ["THROW", =g, u] =>
                changeThrowToGo(u, g, val_sym)
                rplac(first s, "PROGN")
                rplac(rest s, [["LET", val_sym, u], ["GO", CADR g]])
            changeThrowToGo(first s, g, val_sym)
            changeThrowToGo(rest s, g, val_sym)
    rplac(first x,"SEQ")
    rplac(rest x, [["EXIT",a], CADR g, ["EXIT", val_sym]])
  x

optSPADCALL(form is ['SPADCALL,:argl]) ==
  null $InteractiveMode => form
  -- last arg is function/env, but may be a form
  argl is [:argl,fun] =>
    fun is ['ELT,dom,slot] =>
      optCall ['call,['ELT,dom,slot],:argl]
    form
  form

optCall (x is ["call",:u]) ==
  -- destructively optimizes this new x
  x:= optimize [u]
  -- next should happen only as result of macro expansion
  atom first x => first x
  [fn,:a]:= first x
  atom fn => (rplac(rest x, a); rplac(first x, fn); x)
  fn is ["applyFun",name] =>
    (rplac(first x, "SPADCALL"); rplac(rest x, [:a, name]); x)
  fn is [q,R,n] and MEMQ(q,'(ELT QREFELT CONST)) =>
    not $bootStrapMode and (w:= optCallSpecially(q,x,n,R)) => w
    q="CONST" =>
--+
      ["spadConstant",R,n]
    --putInLocalDomainReferences will change this to ELT or QREFELT
    rplac(first x, "SPADCALL")
    if $QuickCode then RPLACA(fn,"QREFELT")
    rplac(rest x, [:a, fn])
    x
  systemErrorHere '"optCall"

optCallSpecially(q,x,n,R) ==
    MEMQ(IFCAR R, $optimizableConstructorNames) => optSpecialCall(x, R, n)
    (y:= get(R,"value",$e)) and
      MEMQ(opOf y.expr,$optimizableConstructorNames) =>
        optSpecialCall(x,y.expr,n)
    nil

optCallEval u ==
  -- Integer() is a lie, but otherwise we could not evaluate
  -- needed domains
  u is ["List",:.] => List Integer()
  u is ["Vector",:.] => Vector Integer()
  u is ["OneDimensionalArray", :.] => OneDimensionalArray Integer()
  u is ["PrimitiveArray",:.] => PrimitiveArray Integer()
  u is ["Matrix", :.] => Matrix Integer()
  u is ["TwoDimensionalArray", :.] => TwoDimensionalArray Integer()
  eval u

optCons (x is ["CONS",a,b]) ==
  a="NIL" =>
    b='NIL => (rplac(first x,'QUOTE); rplac(rest x,['NIL,:'NIL]); x)
    b is ['QUOTE,:c] => (rplac(first x,'QUOTE); rplac(rest x,['NIL,:c]); x)
    x
  a is ['QUOTE,a'] =>
    b='NIL => (rplac(first x,'QUOTE); rplac(rest x,[a',:'NIL]); x)
    b is ['QUOTE,:c] => (rplac(first x,'QUOTE); rplac(rest x,[a',:c]); x)
    x
  x

optSpecialCall(x,y,n) ==
  yval := optCallEval y
  CAAAR x="CONST" =>
    IFCAR yval.n = function Undef =>
      keyedSystemError("S2GE0016",['"optSpecialCall",
        '"invalid constant"])
    MKQ yval.n
  fn := GETL(compileTimeBindingOf first yval.n,'SPADreplace) =>
    rplac(rest x,CDAR x)
    rplac(first x,fn)
    if fn is ["XLAM",:.] then x:=first optimize [x]
    x
  [fn,:a]:= first x
  rplac(first x, "SPADCALL")
  if $QuickCode then RPLACA(fn,"QREFELT")
  rplac(rest x, [:a, fn])
  x

compileTimeBindingOf u ==
  NULL(name:= BPINAME u)  => keyedSystemError("S2OO0001",[u])
  name="Undef" => MOAN "optimiser found unknown function"
  name

optMkRecord ["mkRecord",:u] ==
  u is [x] => ["LIST",x]
  #u=2 => ["CONS",:u]
  ["VECTOR",:u]

optCond (x is ['COND,:l]) ==
  if l is [a,[aa,b]] and TruthP aa and b is ["COND",:c] then
    RPLACD(rest x,c)
  if l is [[p1,:c1],[p2,:c2],:.] then
    if (p1 is ['NULL,p1'] and p1' = p2) or (p2 is ['NULL,p2'] and p2' = p1) then
      l:=[[p1,:c1],['(QUOTE T),:c2]]
      RPLACD( x,l)
    c1 is ['NIL] and p2 = '(QUOTE T) and first c2 = '(QUOTE T) =>
      p1 is ['NULL,p1']=> return p1'
      return ['NULL,p1]
  l is [[p1,:c1],[p2,:c2],[p3,:c3]] and TruthP p3 =>
    EqualBarGensym(c1,c3) =>
      ["COND",[["OR",p1,["NULL",p2]],:c1],[['QUOTE,true],:c2]]
    EqualBarGensym(c1,c2) => ["COND",[["OR",p1,p2],:c1],[['QUOTE,true],:c3]]
    x
  for y in tails l repeat
    while y is [[a1,c1],[a2,c2],:y'] and EqualBarGensym(c1,c2) repeat
      a:=['OR,a1,a2]
      rplac(first first y, a)
      rplac(rest y, y')
  x

EqualBarGensym(x,y) ==
  $GensymAssoc: fluid := nil
  fn(x,y) where
    fn(x,y) ==
      x=y => true
      GENSYMP x and GENSYMP y =>
        z:= assoc(x,$GensymAssoc) => (y=rest z => true; false)
        $GensymAssoc:= [[x,:y],:$GensymAssoc]
        true
      null x => y is [g] and GENSYMP g
      null y => x is [g] and GENSYMP g
      atom x or atom y => false
      fn(first x,first y) and fn(rest x,rest y)

--Called early, to change IF to COND

optIF2COND ["IF",a,b,c] ==
  b is "noBranch" => ["COND",[["NULL",a],c]]
  c is "noBranch" => ["COND",[a,b]]
  c is ["IF",:.] => ["COND",[a,b],:rest optIF2COND c]
  c is ["COND",:p] => ["COND",[a,b],:p]
  ["COND",[a,b],[$true,c]]

optXLAMCond x ==
  x is ["COND",u:= [p,c],:l] =>
    (optPredicateIfTrue p => c; ["COND",u,:optCONDtail l])
  atom x => x
  rplac(first x, optXLAMCond first x)
  rplac(rest x, optXLAMCond rest x)
  x

optPredicateIfTrue p ==
  p is ['QUOTE,:.] => true
  p is [fn,x] and MEMQ(fn,$BasicPredicates) and FUNCALL(fn,x) => true
  nil

optCONDtail l ==
  null l => nil
  [frst:= [p,c],:l']:= l
  optPredicateIfTrue p => [[$true,c]]
  null rest l => [frst,[$true,["CondError"]]]
  [frst,:optCONDtail l']

optSEQ ["SEQ",:l] ==
  tryToRemoveSEQ SEQToCOND getRidOfTemps l where
    getRidOfTemps l ==
      null l => nil
      l is [["LET",g,x,:.],:r] and GENSYMP g and 2>numOfOccurencesOf(g,r) =>
        getRidOfTemps substitute(x,g,r)
      first l="/throwAway" => getRidOfTemps rest l
      --this gets rid of unwanted labels generated by declarations in SEQs
      [first l,:getRidOfTemps rest l]
    SEQToCOND l ==
      transform:= [[a,b] for x in l while (x is ["COND",[a,["EXIT",b]]])]
      before:= take(#transform,l)
      aft:= after(l,before)
      null before => ["SEQ",:aft]
      null aft => ["COND",:transform,'((QUOTE T) (conderr))]
      true => ["COND",:transform,['(QUOTE T),optSEQ ["SEQ",:aft]]]
    tryToRemoveSEQ l ==
      l is ["SEQ",[op,a]] and MEMQ(op,'(EXIT RETURN THROW)) => a
      l

optRECORDELT ["RECORDELT",name,ind,len] ==
  len=1 =>
    ind=0 => ["QCAR",name]
    keyedSystemError("S2OO0002",[ind])
  len=2 =>
    ind=0 => ["QCAR",name]
    ind=1 => ["QCDR",name]
    keyedSystemError("S2OO0002",[ind])
  ["QVELT",name,ind]

optSETRECORDELT ["SETRECORDELT",name,ind,len,expr] ==
  len=1 =>
    ind=0 => ["PROGN",["RPLACA",name,expr],["QCAR",name]]
    keyedSystemError("S2OO0002",[ind])
  len=2 =>
    ind=0 => ["PROGN",["RPLACA",name,expr],["QCAR",name]]
    ind=1 => ["PROGN",["RPLACD",name,expr],["QCDR",name]]
    keyedSystemError("S2OO0002",[ind])
  ["QSETVELT",name,ind,expr]

optRECORDCOPY ["RECORDCOPY",name,len] ==
  len=1 => ["LIST",["CAR",name]]
  len=2 => ["CONS",["CAR",name],["CDR",name]]
  ["MOVEVEC",["MAKE_-VEC",len],name]

optSuchthat [.,:u] == ["SUCHTHAT",:u]

optMINUS u == BREAK()

opt_minus_SI u ==
  u is ['minus_SI, v] =>
    NUMBERP v => -v
    u
  u

opt_- u ==
  u is ['_-,v] =>
    NUMBERP v => -v
    u
  u

optEQ u ==
  u is ['EQ,l,r] =>
    NUMBERP l and NUMBERP r => ['QUOTE,EQ(l,r)]
    -- That undoes some weird work in Boolean to do with the definition of true
    u
  u


for x in '( (call         optCall) _
              (SEQ          optSEQ)_
              (EQ           optEQ)_
              (MINUS        optMINUS)_
              (minus_SI     opt_minus_SI)_
              (_-           opt_-)_
              (SPADCALL     optSPADCALL)_
              (_|           optSuchthat)_
              (CATCH        optCatch)_
              (COND         optCond)_
              (mkRecord     optMkRecord)_
              (RECORDELT    optRECORDELT)_
              (SETRECORDELT optSETRECORDELT)_
              (RECORDCOPY   optRECORDCOPY)) _
      repeat MAKEPROP(CAR x,'OPTIMIZE, CADR x)
