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

  -- HyperTeX basic Solve Command
$systemType := nil
$numberOfEquations := 0
$solutionMethod := nil

bcSolve() ==
  htInitPage('"Solve Basic Command", nil)
  htMakePage '(
   (text . "What do you want to solve? ")
   (text . "\beginmenu")
   (text . "\item ")
   (bcLinks ("\menuitemstyle{A System Of Linear Equations}" "" bcLinearSolve linear))
   (text . "\item ")
   (bcLinks ("\menuitemstyle{A System of Polynomial Equations}" "" bcSystemSolve  polynomial))
   (text . "\item ")
   (bcLinks ("\menuitemstyle{A Single Polynomial Equation}" "" bcSolveSingle onePolynomial))
   (text . "\endmenu"))
  htShowPage()

bcLinearSolve(p,nn) ==
  htInitPage('"Basic Solve Command", nil)
  htMakePage '(
    (text . "How do you want to enter the equations?")
    (text . "\beginmenu")
    (text . "\item ")
    (text . "\newline ")
    (bcLinks ("\menuitemstyle{Directly as equations}" "" bcLinearSolveEqns equations))
    (text . "\item ")
    (text . "\newline ")
    (bcLinks ("\menuitemstyle{In matrix form}" "" bcLinearSolveMatrix matrix))
    (text . "\indentrel{16}\tab{0}")
    (text . " \spad{AX = B}, where \spad{A} is a matrix of coefficients and \spad{B} is a vector" )
    (text . "\indentrel{-16}\item ")
    (text . "\endmenu"))
  htShowPage()

bcLinearSolveEqns(htPage, p) ==
  htInitPage('"Basic Solve Command", nil)
  htMakePage '(
    (domainConditions (isDomain PI (PositiveInteger)))
    (inputStrings
      ("Enter the {\em number} of equations:" "" 5 2 numberOfEquations PI)))
  htMakeDoneButton('"Continue", 'bcLinearSolveEqns1)
  htShowPage()

bcSystemSolve(htPage, p) ==
  htInitPage('"Basic Solve Command", nil)
  htMakePage '(
    (domainConditions (isDomain PI (PositiveInteger)))
    (inputStrings
      ("Enter the {\em number} of equations:" "" 5 2 numberOfEquations PI)))
  htMakeDoneButton('"Continue", 'bcSystemSolveEqns1)
  htShowPage()

bcSolveSingle(htPage,p) ==
  htpSetProperty(htPage,'systemType, 'onePolynomial)
  htpSetProperty(htPage,'exitFunction,'bcInputSolveInfo)
  bcInputEquations(htPage,'exact)

bcSystemSolveEqns1 htPage ==
  htpSetProperty(htPage,'systemType,'polynomial)
  htpSetProperty(htPage,'exitFunction,'bcInputSolveInfo)
  bcInputEquations(htPage,'exact)

bcLinearSolveEqns1 htPage ==
  htpSetProperty(htPage,'systemType,'linear)
  htpSetProperty(htPage,'exitFunction,'bcLinearSolveEqnsGen)
  bcInputEquations(htPage,'exact)

bcInputSolveInfo htPage ==
  page := htInitPage('"Solve Basic Command", htpPropertyList htPage)
  htpSetProperty(page,'numberOfEquations,htpProperty(htPage,'numberOfEquations))
  htpSetProperty(page,'inputArea,htpInputAreaAlist htPage)
  htMakePage '(
   (domainConditions (isDomain PI (PositiveInteger)))
   (text . "What would you like?")
   (text . "\beginmenu")
   (text . "\item ")
   (bcLinks ("\menuitemstyle{Exact Solutions}" "" bcSolveEquations exact))
   (text . "\indentrel{18}\tab{0} ")
   (text . "Solutions expressed in terms of {\em roots} of irreducible polynomials")
   (text . "\indentrel{-18}")
   (text . "\item ")
   (bcLinks ("\menuitemstyle{Numeric Solutions}" "" bcSolveEquationsNumerically numeric))
   (text . "\indentrel{18}\tab{0} ")
   (text . "Solutions expressed in terms of approximate real or complex {\em numbers}")
   (text . "\indentrel{-18}")
   (text . "\item ")
   (bcLinks ("\menuitemstyle{Radical Solutions}" "" bcSolveEquations radical))
   (text . "\indentrel{18}\tab{0} ")
   (text . "Solutions expressed in terms of {\em radicals} if it is possible")
   (text . "\indentrel{-18}")
   (text . "\endmenu"))
  htShowPage()

bcInputEquations(htPage,solutionMethod) ==
  numEqs :=
    htpProperty(htPage, 'systemType) = 'onePolynomial => 1
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage,'numberOfEquations)
    objValUnwrap htpLabelSpadValue(htPage, 'numberOfEquations)
  linearPred := htpProperty(htPage,'systemType) = 'linear
  labelList :=
    numEqs = 1 => '(
      (bcStrings (42 "x^2+1" l1 P))
      (text . " = ")
      (bcStrings (6 0 r1 P)))
    "append"/[f(i,numEqs,linearPred) for i in 1..numEqs] where f(i,n,linearp) ==
      spacer := (i > 99 => 0; i > 9 => 1; 2)
      prefix := STRCONC('"\newline\tab{2}{\em Equation ",STRINGIMAGE i,'":}")
      prefix := STRCONC(prefix,'"\space{",STRINGIMAGE spacer,'"}")
      lnam := INTERN STRCONC('"l",STRINGIMAGE i)
      rnam := INTERN STRCONC('"r",STRINGIMAGE i)
      var:=
        linearp => bcMakeLinearEquations(i,n)
        bcMakeEquations(i,n)
      [['text,:prefix],['bcStrings,[30,var,lnam,'P]],'(text . " = "),['bcStrings,[5,"0",rnam,'P]]]
  equationPart := [
     '(domainConditions
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage('"Solve Basic Command", htpPropertyList htPage)
  htpSetProperty(page, 'numberOfEquations, numEqs)
  htpSetProperty(page, 'solutionMethod,solutionMethod)
  htSay '"\newline\menuitemstyle{}\tab{2}"
  htSay
    numEqs = 1 => '"Enter the {\em Equation}:"
    '"Enter the {\em Equations}:"
  htSay '"\newline\tab{2}"
  htMakePage equationPart
  bcHt '"\blankline "
  htSay '"\newline\menuitemstyle{}\tab{2}"
  htMakePage
    numEqs = 1 =>  '(
      (text ."Enter the {\em unknown} (leave blank if implied): ")
      (text . "\tab{48}")
      (bcStrings (6 "x" unknowns S . quoteString)))
    ['(text . "Enter the unknowns (leave blank if implied):"),
     '(text . "\tab{44}"),
      ['bcStrings, [10,bcMakeUnknowns(numEqs),'unknowns,'P]]]
  htMakeDoneButton('"Continue", 'bcInputEquationsEnd)
  htShowPage()

bcCreateVariableString(i) ==
   STRCONC('"x",STRINGIMAGE i)

bcMakeUnknowns(number)==
   concatenateStringList([STRCONC(bcCreateVariableString(i),'" ")
                            for i in 1..number])

bcMakeEquations(i,number)==
   number =1 => STRCONC(bcCreateVariableString(1),"^2+1")
   bcCreateVariableString(i)
   STRCONC(
     STRCONC(
      concatenateStringList([STRCONC(bcCreateVariableString(j),"+")
                               for j in 1..number]),"1"),
        STRCONC("-2*",STRCONC(bcCreateVariableString(i),"^2")))


bcMakeLinearEquations(i,number)==
   number = 1 => bcCreateVariableString(1)
   number = 2 =>
        i=1 => STRCONC(bcCreateVariableString(1),STRCONC("+",bcCreateVariableString(2)))
        STRCONC(bcCreateVariableString(1),STRCONC("-",bcCreateVariableString(2)))
   STRCONC(
     STRCONC(
      concatenateStringList([STRCONC(bcCreateVariableString(j),"+")
                               for j in 1..number]),"1"),
        STRCONC("-2*",bcCreateVariableString(i)))


bcInputEquationsEnd htPage ==
  fun := htpProperty(htPage, 'exitFunction) => FUNCALL(fun,htPage)
  systemError nil

bcSolveEquationsNumerically(htPage,p) ==
  page := htInitPage('"Solve Basic Command", htpPropertyList htPage)
  htMakePage '(
    (text . "What would you like?")
    (radioButtons choice
      ("Real roots expressed as rational numbers" "" rr)
      ("Real roots expressed as floats" "" rf)
      ("Complex roots expressed as rational numbers" "" cr)
      ("Complex roots expressed as floats" "" cf))
      (text . "\vspace{1}\newline")
      (inputStrings
        ("Enter the number of desired {\em digits} of accuracy" "" 5 20 acc PI)))
  htMakeDoneButton('"Continue", 'bcSolveNumerically1)
  htShowPage()

bcSolveNumerically1(htPage) ==
 bcSolveEquations(htPage,'numeric)

--bcSolveNumerically1(htPage,kind) ==
-- htpSetProperty(htPage,'kind,kind)
-- bcSolveEquations(htPage,'numeric)

bcSolveEquations(htPage,solutionMethod) ==
  if solutionMethod = 'numeric then
    digits := htpLabelInputString(htPage,'acc)
    kind := htpButtonValue(htPage,'choice)
    accString :=
      kind in '(rf cf) => STRCONC('"1.e-",digits)
      STRCONC('"1/10^",digits)
  alist := htpProperty(htPage,'inputArea)
  [[.,varpart,:.],:r] := alist
  varlist := bcString2WordList varpart
  varString := (rest varlist => bcwords2liststring varlist; first varlist)
  eqnString := bcGenEquations r
  solutionMethod = 'numeric =>
    name :=
      kind in '(rf rr) => '"solve"
      '"complexSolve"
    bcFinish(name,eqnString,accString)
  name :=
    solutionMethod = 'radical => '"radicalSolve"
    '"solve"
  bcFinish(name,eqnString,varString,accString)

bcLinearSolveMatrix(htPage,junk) ==
  bcReadMatrix 'bcLinearSolveMatrix1

bcLinearSolveMatrix1 htPage ==
  page := htInitPage('"Linear Solve Basic Command",htpPropertyList htPage)
  htpSetProperty(page,'matrix,bcLinearExtractMatrix htPage)
  htMakePage '(
    (text . "The right side vector B is:")
    (lispLinks
      ("Zero:" "the system is homogeneous" bcLinearSolveMatrixHomo homo)
      ("Not zero:" "the system is not homogeneous" bcLinearSolveMatrixInhomo nothomo)))
  htShowPage()

bcLinearExtractMatrix htPage == REVERSE htpInputAreaAlist htPage

bcLinearSolveMatrixInhomo(htPage,junk) ==
  nrows := htpProperty(htPage,'nrows)
  ncols := htpProperty(htPage,'ncols)
  labelList :=
    [f(i) for i in 1..ncols] where f(i) ==
      spacer := (i > 99 => 0; i > 9 => 1; 2)
      prefix := STRCONC('"{\em Coefficient ",STRINGIMAGE i,'":}")
      if spacer ~= 0 then
        prefix := STRCONC(prefix,'"\space{",STRINGIMAGE spacer,'"}")
      name := INTERN STRCONC('"c",STRINGIMAGE i)
      [prefix,"",30, 0,name, 'P]
  page := htInitPage('"Linear Solve Basic Command",htpPropertyList htPage)
  htpSetProperty(page,'matrix,htpProperty(htPage,'matrix))
  htpSetProperty(page,'nrows,nrows)
  htpSetProperty(page,'ncols,ncols)
  htMakePage [
   '(domainConditions (isDomain P (Polynomial $EmptyMode))),
    '(text . "Enter the right side vector B:"),
        ['inputStrings, :labelList],
          '(text . "\vspace{1}\newline Do you want:" ),
             '(lispLinks
                 ("All the solutions?" "" bcLinearSolveMatrixInhomoGen all)
                  ("A particular solution?" "" bcLinearSolveMatrixInhomoGen particular))]
  htShowPage()

bcLinearSolveMatrixInhomoGen(htPage,key) ==  bcLinearMatrixGen(htPage,key)

bcLinearSolveMatrixHomo(htPage,key) == bcLinearMatrixGen(htPage,'homo)

bcLinearMatrixGen(htPage,key) ==
  matform := bcMatrixGen htPage
  key = 'homo => bcFinish('"nullSpace",matform)
  vector := [x.1 for x in REVERSE htpInputAreaAlist htPage]
  vecform := bcVectorGen vector
  form := bcMkFunction('"solve",matform,[vecform])
  bcGen
    key = 'particular => STRCONC(form,'".particular")
    form

linearFinalRequest(nhh,mat,vect) ==
  sayBrightly '"Do you want more information on the meaning of the output"
  sayBrightly '"   (1) no "
  sayBrightly '"   (2) yes "
  tt := bcQueryInteger(1,2,true)
  tt=1 => sayBrightly '"Bye Bye"
  tt=2 => explainLinear(nhh)

explainLinear(flag) ==
  flag="notHomogeneous" =>
   '("solve returns a particular solution and a basis for"
     "the vector space of solutions for the homogeneous part."
     "The particular solution is _"failed_" if one cannot be found.")
  flag= "homogeneous" =>
    '("solve returns a basis for"
      "the vector space of solutions for the homogeneous part")
  systemError nil

finalExactRequest(equations,unknowns) ==
  sayBrightly '"Do you like:"
  sayBrightly '"   (1) the solutions how they are displayed"
  sayBrightly '"   (2) to get ????"
  sayBrightly '"   (3) more information on the meaning of the output"
  tt := bcQueryInteger(1,3,true)
  tt=1 => sayBrightly '"Bye Bye"
  tt=2 => moreExactSolution(equations,unknowns,flag)
  tt=3 => explainExact(equations,unknowns)

bcLinearSolveEqnsGen htPage ==
  alist := htpInputAreaAlist htPage
  if vars := htpLabelInputString(htPage,'unknowns) then
    varlist := bcString2WordList vars
    varString := (rest varlist => bcwords2liststring varlist; first varlist)
    alist := rest alist  --know these are first on the list
  eqnString := bcGenEquations alist
  bcFinish('"solve",eqnString,varString)

bcGenEquations alist ==
  y := alist
  while y repeat
    right := (first y).1
    y := rest y
    left := (first y).1
    y := rest y
    eqnlist := [STRCONC(left,'" = ",right),:eqnlist]
  rest eqnlist => bcwords2liststring eqnlist
  first eqnlist
