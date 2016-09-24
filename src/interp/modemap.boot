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

--% EXTERNAL ROUTINES

--These functions are called from outside this file to add a domain
--   or to get the current domains in scope;

addDomain(domain,e) ==
  atom domain =>
    EQ(domain,"$EmptyMode") => e
    EQ(domain,"$NoValueMode") => e
    not IDENTP domain or 2<#(s:= STRINGIMAGE domain) and
      EQ(char "#",s.(0)) and EQ(char "#",s.(1)) => e
    MEMQ(domain,getDomainsInScope e) => e
    isLiteral(domain,e) => e
    addNewDomain(domain,e)
  (name:= first domain)='Category => e
  domainMember(domain,getDomainsInScope e) => e
  getmode(name,e) is ["Mapping",target,:.] and isCategoryForm(target,e)=>
      addNewDomain(domain,e)
    -- constructor? test needed for domains compiled with $bootStrapMode=true
  isFunctor name or constructor? name => addNewDomain(domain,e)
  if not isCategoryForm(domain,e) and
    not member(name,'(Mapping CATEGORY)) then
      unknownTypeError name
  e        --is not a functor

domainMember(dom,domList) == or/[modeEqual(dom,d) for d in domList]

--% MODEMAP FUNCTIONS

getModemapList(op,numOfArgs,e) ==
  op is ['Sel, D, op'] => getModemapListFromDomain(op', numOfArgs, D, e)
  [mm for
    (mm:= [[.,.,:sigl],:.]) in get(op,'modemap,e) | numOfArgs=#sigl]

getModemapListFromDomain(op,numOfArgs,D,e) ==
  [mm
    for (mm:= [[dc,:sig],:.]) in get(op,'modemap,e) | dc=D and #rest sig=
      numOfArgs]

addModemapKnown(op,mc,sig,pred,fn,$e) ==
--  if knownInfo pred then pred:=true
--  that line is handled elsewhere
  $insideCapsuleFunctionIfTrue=true =>
    $CapsuleModemapFrame :=
      addModemap0(op,mc,sig,pred,fn,$CapsuleModemapFrame)
    $e
  addModemap0(op,mc,sig,pred,fn,$e)

addModemap0(op,mc,sig,pred,fn,e) ==
  --mc is the "mode of computation"; fn the "implementation"
  $functorForm is ['CategoryDefaults,:.] and mc="$" => e
    --don't put CD modemaps into environment
  op = 'elt or op = "setelt!" => addEltModemap(op, mc, sig, pred, fn, e)
  addModemap1(op,mc,sig,pred,fn,e)

addEltModemap(op,mc,sig,pred,fn,e) ==
   --hack to change selectors from strings to identifiers; and to
   --add flag identifiers as literals in the envir
  op='elt and sig is [:lt,sel] =>
    STRINGP sel =>
      id:= INTERN sel
      if $insideCapsuleFunctionIfTrue=true
         then $e:= makeLiteral(id,$e)
         else e:= makeLiteral(id,e)
      addModemap1(op,mc,[:lt,id],pred,fn,e)
    -- atom sel => systemErrorHere '"addEltModemap"
    addModemap1(op,mc,sig,pred,fn,e)
  op = "setelt!" and sig is [:lt, sel, v] =>
    STRINGP sel =>
      id:= INTERN sel
      if $insideCapsuleFunctionIfTrue=true
         then $e:= makeLiteral(id,$e)
         else e:= makeLiteral(id,e)
      addModemap1(op,mc,[:lt,id,v],pred,fn,e)
    -- atom sel => systemError '"addEltModemap"
    addModemap1(op,mc,sig,pred,fn,e)
  systemErrorHere '"addEltModemap"

addModemap1(op,mc,sig,pred,fn,e) ==
   --mc is the "mode of computation"; fn the "implementation"
  if mc='Rep then
--     if fn is [kind,'Rep,.] and
               -- save old sig for NRUNTIME
--       (kind = 'ELT or kind = 'CONST) then fn:=[kind,'Rep,sig]
     sig:= substitute("$",'Rep,sig)
  currentProplist:= getProplist(op,e) or nil
  newModemapList:=
    mkNewModemapList(mc,sig,pred,fn,LASSOC('modemap,currentProplist),e,nil)
  newProplist:= augProplist(currentProplist,'modemap,newModemapList)
  unErrorRef op
        --There may have been a warning about op having no value
  addBinding(op, newProplist, e)

mkNewModemapList(mc,sig,pred,fn,curModemapList,e,filenameOrNil) ==
  entry:= [map:= [mc,:sig],[pred,fn],:filenameOrNil]
  member(entry,curModemapList) => curModemapList
  (oldMap:= assoc(map,curModemapList)) and oldMap is [.,[opred, =fn],:.] =>
    $forceAdd => mergeModemap(entry,curModemapList,e)
    opred=true => curModemapList
    if pred~=true and pred~=opred then pred:= ["OR",pred,opred]
    [if x=oldMap then [map,[pred,fn],:filenameOrNil] else x

  --if new modemap less general, put at end; otherwise, at front
      for x in curModemapList]
  $InteractiveMode => insertModemap(entry,curModemapList)
  mergeModemap(entry,curModemapList,e)

mergeModemap(entry is [[mc,:sig],[pred,:.],:.],modemapList,e) ==
  for (mmtail:= [[[mc',:sig'],[pred',:.],:.],:.]) in tails modemapList repeat
    mc=mc' or isSuperDomain(mc',mc,e) =>
      newmm:= nil
      mm:= modemapList
      while (not EQ(mm,mmtail)) repeat (newmm:= [first mm,:newmm]; mm:= rest mm)
      if (mc=mc') and (sig=sig') then
        --We only need one of these, unless the conditions are hairy
        not $forceAdd and TruthP pred' =>
          entry:=nil
              --the new predicate buys us nothing
          return modemapList
        TruthP pred => mmtail:=rest mmtail
          --the thing we matched against is useless, by comparison
      modemapList:= NCONC(NREVERSE newmm,[entry,:mmtail])
      entry:= nil
      return modemapList
  if entry then [:modemapList,entry] else modemapList

isSuperDomain(domainForm,domainForm',e) ==
  isSubset(domainForm',domainForm,e) => true
  domainForm='Rep and domainForm'="$" => true --regard $ as a subdomain of Rep
  LASSOC(opOf domainForm',get(domainForm,"SubDomain",e))

addNewDomain(domain,e) ==
  augModemapsFromDomain(domain,domain,e)

augModemapsFromDomain(name,functorForm,e) ==
  member(IFCAR name or name, $DummyFunctorNames) => e
  name=$Category or isCategoryForm(name,e) => e
  member(name, getDomainsInScope e) => e
  if u:= GETDATABASE(opOf functorForm,'SUPERDOMAIN) then
    e:= addNewDomain(first u,e)
    --need code to handle parameterized SuperDomains
  if innerDom:= listOrVectorElementMode name then e:= addDomain(innerDom,e)
  if name is ["Union",:dl] then for d in stripUnionTags dl
                         repeat e:= addDomain(d,e)
  augModemapsFromDomain1(name,functorForm,e)
     --see LISPLIB BOOT

substituteCategoryArguments(argl,catform) ==
  argl:= substitute("$$","$",argl)
  arglAssoc:= [[INTERNL("#",STRINGIMAGE i),:a] for i in 1.. for a in argl]
  SUBLIS(arglAssoc,catform)

augModemapsFromCategory(domainName,domainView,functorForm,categoryForm,e) ==
  [fnAlist,e]:= evalAndSub(domainName,domainView,functorForm,categoryForm,e)
  compilerMessage ["Adding ",domainName," modemaps"]
  e:= putDomainsInScope(domainName,e)
  condlist:=[]
  for [[op,sig,:.],cond,fnsel] in fnAlist repeat
      e:= addModemapKnown(op,domainName,sig,cond,fnsel,e)
  e

evalAndSub(domainName,viewName,functorForm,form,$e) ==
  isCategory form => [substNames(domainName,viewName,functorForm,form.(1)),$e]
  --next lines necessary-- see MPOLY for which $ is actual arg. --- RDJ 3/83
  if CONTAINED("$$",form) then $e:= put("$$","mode",get("$","mode",$e),$e)
  opAlist:= getOperationAlist(domainName,functorForm,form)
  substAlist:= substNames(domainName,viewName,functorForm,opAlist)
  [substAlist,$e]

getOperationAlist(name,functorForm,form) ==
  if atom name and GETDATABASE(name,'NILADIC) then functorForm:= [functorForm]
  (u:= isFunctor functorForm) and not
    ($insideFunctorIfTrue and first functorForm=first $functorForm) => u
  $insideFunctorIfTrue and name="$" =>
    ($domainShell => $domainShell.(1); systemError '"$ has no shell now")
  T:= compMakeCategoryObject(form,$e) => ([.,.,$e]:= T; T.expr.(1))
  stackMessage ["not a category form: ",form]

substNames(domainName,viewName,functorForm,opalist) ==
  functorForm := SUBSTQ("$$","$", functorForm)
  nameForDollar :=
    isCategoryPackageName functorForm => CADR functorForm
    domainName

       -- following calls to SUBSTQ must copy to save RPLAC's in
       -- putInLocalDomainReferences
  [[:SUBSTQ("$","$$",SUBSTQ(nameForDollar,"$",modemapform)),
       [sel, viewName,if domainName = "$" then pos else
                                         CADAR modemapform]]
     for [:modemapform,[sel,"$",pos]] in
          EQSUBSTLIST(IFCDR functorForm, $FormalMapVariableList, opalist)]


compCat(form is [functorName,:argl],m,e) ==
  fn := GET(functorName, "makeFunctionList") or return nil
  [funList,e]:= FUNCALL(fn,form,form,e)
  catForm:=
    ["Join",'(SetCategory),["CATEGORY","domain",:
      [["SIGNATURE",op,sig] for [op,sig,.] in funList | op~="="]]]
  --RDJ: for coercion purposes, it necessary to know it's a Set; I'm not
  --sure if it uses any of the other signatures(see extendsCategoryForm)
  [form,catForm,e]

addModemap(op, mc, sig, pred, fn, $e) ==
    $InteractiveMode => $e
    if knownInfo pred then pred := true
    $insideCapsuleFunctionIfTrue = true =>
        $CapsuleModemapFrame :=
          addModemap0(op, mc, sig, pred, fn, $CapsuleModemapFrame)
        $e
    addModemap0(op, mc, sig, pred, fn, $e)

addConstructorModemaps(name,form is [functorName,:.],e) ==
  $InteractiveMode: local:= nil
  e:= putDomainsInScope(name,e) --frame
  fn := GET(functorName, "makeFunctionList")
  [funList,e]:= FUNCALL(fn,name,form,e)
  for [op,sig,opcode] in funList repeat
    if opcode is [sel,dc,n] and sel='ELT then
          nsig := substitute("$$$",name,sig)
          nsig := substitute('$,"$$$",substitute("$$",'$,nsig))
          opcode := [sel,dc,nsig]
    e:= addModemap(op,name,sig,true,opcode,e)
  e


--The way XLAMs work:
--  ((XLAM ($1 $2 $3) (SETELT $1 0 $3)) X "c" V) ==> (SETELT X 0 V)

getDomainsInScope e ==
  $insideCapsuleFunctionIfTrue=true => $CapsuleDomainsInScope
  get("$DomainsInScope","special",e)

putDomainsInScope(x,e) ==
  l:= getDomainsInScope e
  if member(x,l) then SAY("****** Domain: ",x," already in scope")
  newValue:= [x,:delete(x,l)]
  $insideCapsuleFunctionIfTrue => ($CapsuleDomainsInScope:= newValue; e)
  put("$DomainsInScope","special",newValue,e)
