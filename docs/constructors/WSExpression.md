# WSExpression

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic expressions using the MathLink Julia package. It supports the Eltable category (interface) so, for example using Fibonacci polynomials fibonacci(12,jWSExpr x) => 3 75 77 79 411 6 x + 35 x 2+ 56 x 2+ 36 x 2+ 10 x 2+ x %.5 => 10*x^9

**WSExpression is a domain constructor.**  
**Abbreviation for WSExpression is WSEXPR**  
**This constructor is exposed in this frame.**  
**520 names for 823 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 #? : % -> WSInteger                                    ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               Beta : (%, %) -> %
 Beta : (%, %, %) -> %                                  Chi : % -> %
 Ci : % -> %                                            D : (%, %) -> %
 D : (%, %, NonNegativeInteger) -> %                    D : (%, List(%)) -> %
 D : (%, List(%), List(NonNegativeInteger)) -> %        D : (%, List(Symbol)) -> %
 D : (%, (% -> %)) -> %                                 D : (%, (% -> %), NonNegativeInteger) -> %
 D : (%, Symbol) -> %                                   D : (%, Symbol, NonNegativeInteger) -> %
 Ei : % -> %                                            EiEn : (%, %) -> %
 Gamma : % -> %                                         Gamma : (%, %) -> %
 Gamma : (%, %, %) -> %                                 Shi : % -> %
 Si : % -> %                                            ?^? : (%, %) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 ?^? : (%, WSRational) -> %                             abs : % -> %
 accountingForm : % -> %                                accountingForm : (%, %) -> %
 acos : % -> %                                          acosh : % -> %
 acot : % -> %                                          acoth : % -> %
 acsc : % -> %                                          acsch : % -> %
 airyAi : % -> %                                        airyAiPrime : % -> %
 airyAiZero : % -> %                                    airyAiZero : (%, %) -> %
 airyBi : % -> %                                        airyBiPrime : % -> %
 airyBiZero : % -> %                                    airyBiZero : (%, %) -> %
 algtower : % -> List(Kernel(%))                        algtower : List(%) -> List(Kernel(%))
 angerJ : (%, %) -> %                                   angerJ : (%, %, %) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 apart : % -> %                                         apart : (%, %) -> %
 append : (%, %) -> %                                   applyQuote : (Symbol, %) -> %
 applyQuote : (Symbol, %, %) -> %                       applyQuote : (Symbol, %, %, %) -> %
 applyQuote : (Symbol, %, %, %, %) -> %                 applyQuote : (Symbol, List(%)) -> %
 argument : % -> %                                      asec : % -> %
 asech : % -> %                                         asin : % -> %
 asinh : % -> %                                         associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            assuming : (%, %) -> %
 atan : % -> %                                          atan : (%, %) -> %
 atanh : % -> %                                         barnesG : % -> %
 baseForm : (%, %) -> %                                 basis : () -> Vector(%)
 belong? : BasicOperator -> Boolean                     besselI : (%, %) -> %
 besselJ : (%, %) -> %                                  besselJZero : (%, %) -> %
 besselK : (%, %) -> %                                  besselY : (%, %) -> %
 besselYZero : (%, %) -> %                              betaRegularized : (%, %, %) -> %
 binomial : (%, %) -> %                                 box : % -> %
 cancel : % -> %                                        catalan : () -> %
 ceiling : % -> %                                       characteristic : () -> NonNegativeInteger
 charlierC : (%, %, %) -> %                             chebyshevT : (%, %) -> %
 chebyshevU : (%, %) -> %                               coefficient : (%, %) -> %
 coefficient : (%, %, %) -> %                           coefficientList : (%, %) -> %
 coefficientRules : % -> %                              coefficientRules : (%, %) -> %
 coerce : % -> %                                        coerce : Complex(Integer) -> %
 coerce : Equation(%) -> %                              coerce : Float -> %
 coerce : Fraction(%) -> %                              coerce : Fraction(Integer) -> %
 coerce : Fraction(Polynomial(%)) -> %                  coerce : Fraction(Polynomial(Fraction(%))) -> %
 coerce : Integer -> %                                  coerce : Kernel(%) -> %
 coerce : List(%) -> %                                  coerce : Polynomial(%) -> %
 coerce : Polynomial(Fraction(%)) -> %                  coerce : String -> %
 coerce : Symbol -> %                                   coerce : WSSymbol -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 coerce : % -> WSExpression                             coerce : % -> WSInteger
 coerce : % -> WSRational                               collect : (%, %) -> %
 collect : (%, WSList(%)) -> %                          commutator : (%, %) -> %
 complex : (%, %) -> %                                  complexExpand : % -> %
 complexExpand : (%, %) -> %                            conjugate : % -> %
 convert : Factored(%) -> %                             convert : SparseUnivariatePolynomial(%) -> %
 convert : Vector(%) -> %                               convert : % -> SparseUnivariatePolynomial(%)
 convert : % -> String                                  convert : % -> Vector(%)
 coordinates : Vector(%) -> Matrix(%)                   coordinates : % -> Vector(%)
 coordinates : (%, Vector(%)) -> Vector(%)              cos : % -> %
 cosh : % -> %                                          cot : % -> %
 coth : % -> %                                          coulombF : (%, %, %) -> %
 coulombG : (%, %, %) -> %                              coulombH1 : (%, %, %) -> %
 coulombH2 : (%, %, %) -> %                             csc : % -> %
 csch : % -> %                                          dSolve : (%, %, %) -> %
 dSolve : (Equation(%), %, %) -> %                      dSolveValue : (%, %, %) -> %
 dSolveValue : (Equation(%), %, %) -> %                 dawson : % -> %
 decimalForm : % -> %                                   decimalForm : (%, %) -> %
 decompose : (%, %) -> WSList(%)                        dedekindEta : % -> %
 defined? : % -> Boolean                                definingPolynomial : % -> %
 degree : () -> %                                       delete : (%, WSList(WSInteger)) -> %
 denominator : % -> %                                   derivative : (BasicOperator, %) -> %
 derivative : (BasicOperator, %, %) -> %                differentiate : (%, %) -> %
 differentiate : (%, %, NonNegativeInteger) -> %        differentiate : (%, List(%)) -> %
 differentiate : (%, List(Symbol)) -> %                 differentiate : (%, (% -> %)) -> %
 differentiate : (%, Symbol) -> %                       digamma : % -> %
 digamma : (%, %) -> %                                  dilog : % -> %
 dimensions : % -> WSList(WSInteger)                    diracDelta : % -> %
 dirichletEta : % -> %                                  dirichletL : (%, %, %) -> %
 discriminant : () -> %                                 discriminant : (%, %) -> %
 discriminant : Vector(%) -> %                          distribute : % -> %
 distribute : (%, %) -> %                               ellipticE : % -> %
 ellipticE : (%, %) -> %                                ellipticF : (%, %) -> %
 ellipticK : % -> %                                     ellipticPi : (%, %) -> %
 ellipticPi : (%, %, %) -> %                            ellipticTheta : (%, %, %) -> %
 ellipticThetaPrime : (%, %, %) -> %                    elt : (%, Integer) -> %
 elt : (BasicOperator, %) -> %                          elt : (BasicOperator, %, %) -> %
 elt : (BasicOperator, %, %, %) -> %                    elt : (BasicOperator, %, %, %, %) -> %
 elt : (BasicOperator, %, %, %, %, %) -> %              elt : (BasicOperator, %, %, %, %, %, %) -> %
 elt : (BasicOperator, List(%)) -> %                    engineeringForm : % -> %
 engineeringForm : (%, %) -> %                          erf : % -> %
 erf : (%, %) -> %                                      erfc : % -> %
 erfi : % -> %                                          euclideanSize : % -> NonNegativeInteger
 eulerE : WSInteger -> %                                eulerE : (WSInteger, %) -> %
 eulerGamma : () -> %                                   eulerPhi : WSInteger -> %
 eval : (%, %, %) -> %                                  eval : (%, BasicOperator, (% -> %)) -> %
 eval : (%, BasicOperator, (List(%) -> %)) -> %         eval : (%, Equation(%)) -> %
 eval : (%, Kernel(%), %) -> %                          eval : (%, List(%), List(%)) -> %
 eval : (%, List(Equation(%))) -> %                     eval : (%, List(Kernel(%)), List(%)) -> %
 eval : (%, List(Symbol), List((% -> %))) -> %          eval : (%, Symbol, (% -> %)) -> %
 eval : (%, Symbol, (List(%) -> %)) -> %                exactNumber? : % -> Boolean
 exp : () -> %                                          exp : % -> %
 expToTrig : % -> %                                     expand : % -> %
 expand : (%, %) -> %                                   expandDenominator : % -> %
 expandNumerator : % -> %                               exponent : (%, %) -> %
 exponent : (%, %, %) -> %                              ?exquo? : (%, %) -> Union(%,"failed")
 extendedExpand : % -> %                                extendedSimplify : % -> %
 extendedSimplify : (%, %) -> %                         extract : (%, NonNegativeInteger) -> %
 extract : (%, WSExpression) -> %                       factor : % -> %
 factor : (%, %) -> %                                   factor : % -> Factored(%)
 factorList : % -> WSList(WSList(%))                    factorPolynomial : % -> %
 factorSquareFree : % -> %                              factorSquareFreeList : % -> WSList(WSList(%))
 factorTerms : % -> %                                   factorTerms : (%, %) -> %
 factorTerms : (%, WSList(%)) -> %                      factorTermsList : % -> WSList(%)
 factorTermsList : (%, %) -> WSList(%)                  factorial : % -> %
 factorials : % -> %                                    factorials : (%, Symbol) -> %
 fibonacci : (%, %) -> %                                findInstance : (%, %) -> WSList(WSList(%))
 findInstance : (%, %, %) -> WSList(WSList(%))          findRoot : (%, %) -> %
 first : % -> %                                         floor : % -> %
 fourier : % -> %                                       fourier : (%, %) -> %
 fourier : WSList(%) -> WSList(%)                       fourier : (WSList(%), WSList(%)) -> WSList(%)
 fractionPart : % -> %                                  freeOf? : (%, %) -> Boolean
 freeOf? : (%, Symbol) -> Boolean                       fresnelC : % -> %
 fresnelS : % -> %                                      fromCoefficientRules : (%, %) -> %
 functionExpand : % -> %                                functionExpand : (%, %) -> %
 gammaRegularized : (%, %) -> %                         gcd : (%, %) -> %
 gcd : List(%) -> %                                     gegenbauerC : (%, %) -> %
 gegenbauerC : (%, %, %) -> %                           generator : () -> %
 goldenRatio : () -> %                                  groebnerBasis : (%, %) -> WSList(%)
 groebnerBasis : (%, %, %) -> WSList(%)                 ground : % -> %
 ground? : % -> Boolean                                 gudermannian : % -> %
 guessGeneratingFunction : (%, %) -> %                  guessGeneratingFunction : (WSList(%), %) -> %
 guessSequenceFunction : % -> %                         guessSequenceFunction : (%, %) -> %
 guessSequenceFunction : WSList(WSInteger) -> %         hahnQ : (%, %, %, %, %) -> %
 hahnR : (%, %, %, %, %) -> %                           hahnS : (%, %, %, %, %) -> %
 hahn_p : (%, %, %, %, %) -> %                          hankelH1 : (%, %) -> %
 hankelH2 : (%, %) -> %                                 haversine : % -> %
 height : % -> NonNegativeInteger                       hermiteH : (%, %) -> %
 hornerForm : (%, %) -> %                               hurwitzLerchPhi : (%, %, %) -> %
 hurwitzZeta : (%, %) -> %                              hyperFactorial : % -> %
 hypergeometric0F1 : (%, %) -> %                        hypergeometric0F1Regularized : (%, %) -> %
 hypergeometric1F1 : (%, %, %) -> %                     hypergeometric1F1Regularized : (%, %, %) -> %
 hypergeometricU : (%, %, %) -> %                       imag : % -> %
 imaginary : () -> %                                    insert : (%, %, WSInteger) -> %
 integer? : % -> Boolean                                integral : (%, SegmentBinding(%)) -> %
 integral : (%, Symbol) -> %                            integrate : (%, %) -> %
 integrate : (%, %, Segment(Integer)) -> %              integrate : (%, Symbol) -> %
 interpolatingPolynomial : (%, %) -> %                  intersection : (%, %) -> %
 inv : % -> %                                           inverseBetaRegularized : (%, %, %) -> %
 inverseErf : % -> %                                    inverseErfc : % -> %
 inverseFourier : % -> %                                inverseFourier : (%, %) -> %
 inverseFourier : WSList(%) -> %                        inverseGammaRegularized : (%, %) -> %
 inverseGudermannian : % -> %                           inverseHaversine : % -> %
 inverseJacobiCn : (%, %) -> %                          inverseJacobiSn : (%, %) -> %
 irreducible? : % -> Boolean                            irreducible? : (%, %) -> Boolean
 is? : (%, BasicOperator) -> Boolean                    is? : (%, Symbol) -> Boolean
 isPlus : % -> Union(List(%),"failed")                  isTimes : % -> Union(List(%),"failed")
 jWSAggregate : List(%) -> %                            jWSAssociation : WSList(%) -> %
 jWSData : () -> %                                      jWSData : % -> %
 jWSData : (%, %) -> %                                  jWSData : (%, %, %) -> %
 jWSData : String -> %                                  jWSData : (String, String) -> %
 jWSData : (String, String, String) -> %                jWSEqual : (%, %) -> %
 jWSExpr : DoubleFloat -> %                             jWSExpr : Float -> %
 jWSExpr : Fraction(Integer) -> %                       jWSExpr : Integer -> %
 jWSExpr : JLFloat -> %                                 jWSExpr : JLFloat64 -> %
 jWSExpr : List(%) -> %                                 jWSExpr : String -> %
 jWSExpr : Symbol -> %                                  jWSGreater : (%, %) -> %
 jWSGreaterEqual : (%, %) -> %                          jWSInterpret : String -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : (String, String, String) -> %
 jWSLess : (%, %) -> %                                  jWSLessEqual : (%, %) -> %
 jWSNotEqual : (%, %) -> %                              jWSQuantity : % -> %
 jWSQuantity : (%, %) -> %                              jWSRule : (%, %) -> %
 jWSRule : Equation(%) -> %                             jWSTable : (%, %) -> WSList(%)
 jWSTable : (%, %, %) -> WSList(WSList(%))              jacobiAmplitude : (%, %) -> %
 jacobiCn : (%, %) -> %                                 jacobiDn : (%, %) -> %
 jacobiP : (%, %, %, %) -> %                            jacobiSn : (%, %) -> %
 jacobiTheta : (%, %) -> %                              jacobiTheta : (WSInteger, %, %) -> %
 jacobiZeta : (%, %) -> %                               jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDisplay : WSExpression -> WSExpression               jlDump : JLObject -> Void
 jlEval : % -> %                                        jlEval : (%, String) -> %
 jlEval : (%, String, String) -> %                      jlEval : (%, String, String, String) -> %
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlGreedyEval : Boolean -> Void                         jlHead : % -> WSSymbol
 jlId : % -> JLInt64                                    jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlSymbolic : % -> String                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlWSAccuracy : % -> %
 jlWSDefined? : String -> Boolean                       jlWSPrecision : % -> %
 jlWSSetAccuracy : (%, %) -> %                          jlWSSetOptions : (%, %) -> %
 jlWSSetPrecision : (%, %) -> %                         jlimref : String -> %
 jlref : String -> %                                    join : (%, %) -> %
 kelvinBei : (%, %) -> %                                kelvinBer : (%, %) -> %
 kelvinKei : (%, %) -> %                                kelvinKer : (%, %) -> %
 kernel : (BasicOperator, %) -> %                       kernel : (BasicOperator, List(%)) -> %
 kernels : % -> List(Kernel(%))                         kernels : List(%) -> List(Kernel(%))
 key? : (%, %) -> Boolean                               keys : % -> %
 kleinInvariantJ : % -> %                               krawtchoukK : (%, %, %, %) -> %
 kummerM : (%, %, %) -> %                               kummerU : (%, %, %) -> %
 laguerreL : (%, %) -> %                                laguerreL : (%, %, %) -> %
 lambertW : % -> %                                      lambertW : (WSInteger, %) -> %
 last : % -> %                                          latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     legendreP : (%, %) -> %
 legendreP : (%, %, %) -> %                             legendreQ : (%, %) -> %
 legendreQ : (%, %, %) -> %                             length : % -> %
 length : % -> WSInteger                                lerchPhi : (%, %, %) -> %
 level : (%, %) -> WSList(%)                            level : (%, %, Boolean) -> WSList(%)
 li : % -> %                                            lift : % -> SparseUnivariatePolynomial(%)
 limit : (%, %) -> %                                    log : % -> %
 log10 : % -> %                                         log2 : % -> %
 logBarnesG : % -> %                                    logGamma : % -> %
 lommelS1 : (%, %, %) -> %                              lommelS2 : (%, %, %) -> %
 lookup : (%, %) -> %                                   lookup : (%, %, %) -> %
 machineNumber? : % -> Boolean                          mainKernel : % -> Union(Kernel(%),"failed")
 map : ((% -> %), %) -> %                               map : ((% -> %), Kernel(%)) -> %
 mathieuC : (%, %, %) -> %                              mathieuCPrime : (%, %, %) -> %
 mathieuCharacteristicA : (%, %) -> %                   mathieuCharacteristicB : (%, %) -> %
 mathieuCharacteristicExponent : (%, %) -> %            mathieuS : (%, %, %) -> %
 mathieuSPrime : (%, %, %) -> %                         matrixForm : % -> %
 maxLimit : (%, %) -> %                                 maximize : (%, %) -> %
 maximize : (%, %, %) -> %                              maximize : (%, Symbol) -> %
 meixnerM : (%, %, %, %) -> %                           meixnerP : (%, %, %, %) -> %
 member? : (%, %) -> Boolean                            minLimit : (%, %) -> %
 minimalPolynomial : (%, %) -> %                        minimalPolynomial : (%, %, %) -> %
 minimize : (%, %) -> %                                 minimize : (%, %, %) -> %
 minimize : (%, Symbol) -> %                            missing? : % -> Boolean
 modularLambda : % -> %                                 monomialList : % -> %
 mutable? : % -> Boolean                                negative? : % -> Boolean
 norm : % -> %                                          normal : % -> %
 normal : (%, %) -> %                                   nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            number? : % -> Boolean
 numberForm : % -> %                                    numberForm : (%, %) -> %
 numerDenom : % -> WSList(%)                            numerator : % -> %
 numeric : % -> WSExpression                            numeric : (%, PositiveInteger) -> WSExpression
 numeric? : % -> Boolean                                numericDSolve : (%, %, %) -> %
 numericDSolve : (%, %, %, %) -> %                      numericDSolve : (Equation(%), %, %) -> %
 numericDSolveValue : (%, %, %) -> %                    numericDSolveValue : (%, %, %, %) -> %
 numericDSolveValue : (Equation(%), %, %) -> %          numericIntegrate : (%, %) -> %
 numericIntegrate : (%, %, Segment(Integer)) -> %       numericMaximize : (%, %) -> %
 numericMaximize : (%, %, %) -> %                       numericMaximize : (%, Symbol) -> %
 numericMinimize : (%, %) -> %                          numericMinimize : (%, %, %) -> %
 numericMinimize : (%, Symbol) -> %                     numericProduct : (%, %) -> %
 numericProduct : (%, %, Segment(Integer)) -> %         numericSolve : (%, %) -> %
 numericSolve : (Equation(%), %) -> %                   numericSum : (%, %) -> %
 numericSum : (%, %, Segment(Integer)) -> %             one? : % -> Boolean
 operator : BasicOperator -> BasicOperator              operators : % -> List(BasicOperator)
 opposite? : (%, %) -> Boolean                          padeApproximant : (%, %) -> %
 parabolicCylinderD : (%, %) -> %                       paren : % -> %
 part : (%, WSInteger) -> %                             percentForm : % -> %
 percentForm : (%, %) -> %                              permutation : (%, %) -> %
 pi : () -> %                                           plenaryPower : (%, PositiveInteger) -> %
 pochhammer : (%, %) -> %                               polygamma : (%, %) -> %
 polylog : (%, %) -> %                                  polylog : (%, %, %) -> %
 polynomial? : (%, %) -> Boolean                        polynomial? : (%, WSList(%)) -> Boolean
 polynomialExpression? : (%, %) -> Boolean              polynomialExpression? : (%, WSList(%)) -> Boolean
 polynomialExtendedGCD : (%, %, %) -> %                 polynomialGCD : (%, %) -> %
 polynomialGCD : (%, %, %) -> %                         polynomialLCM : (%, %) -> %
 polynomialLCM : (%, %, %) -> %                         polynomialMod : (%, %) -> %
 polynomialQuotient : (%, %, %) -> %                    polynomialReduce : (%, %, %) -> %
 polynomialRemainder : (%, %, %) -> %                   positive? : % -> Boolean
 positiveInfinity : () -> %                             powerExpand : % -> %
 powerExpand : (%, %) -> %                              prepend : (%, %) -> %
 prime? : % -> Boolean                                  product : (%, %) -> %
 product : (%, %, Segment(Integer)) -> %                product : (%, SegmentBinding(%)) -> %
 product : (%, Symbol) -> %                             qBinomial : (%, %, %) -> %
 qFactorial : (%, %) -> %                               qGamma : (%, %) -> %
 qPochhammer : (%, %) -> %                              qPochhammer : (%, %, %) -> %
 qPolyGamma : (%, %) -> %                               qPolyGamma : (%, %, %) -> %
 qelt : (%, Integer) -> %                               qsetelt : (%, Integer, %) -> %
 qsetelt! : (%, Integer, %) -> %                        quantityForm : (%, %) -> %
 quantityForm : (%, WSList(%)) -> %                     quantityMagnitude : % -> %
 quantityUnit : % -> %                                  ?quo? : (%, %) -> %
 racahR : (%, %, %, %, %, %) -> %                       ramanujanTau : % -> %
 ramanujanTauL : % -> %                                 ramanujanTauTheta : % -> %
 ramanujanTauZ : % -> %                                 rank : () -> PositiveInteger
 rational? : % -> Boolean                               rationalApproximation : % -> %
 rationalApproximation : (%, %) -> %                    rationalExpression? : (%, %) -> Boolean
 rationalExpression? : (%, WSList(%)) -> Boolean        real : % -> %
 real? : % -> Boolean                                   realNumeric? : % -> Boolean
 realNumericSolve : (%, %) -> %                         realNumericSolve : (Equation(%), %) -> %
 recip : % -> Union(%,"failed")                         reduce : (%, %) -> %
 reduce : (%, %, %) -> %                                reduce : (Equation(%), %) -> %
 reduce : (Equation(%), %, %) -> %                      reduce : SparseUnivariatePolynomial(%) -> %
 reducedSystem : Matrix(%) -> Matrix(%)                 refine : (%, %) -> %
 regularRepresentation : % -> Matrix(%)                 ?rem? : (%, %) -> %
 removeDuplicates : % -> %                              replace : (%, %) -> %
 replace : (%, %, %) -> %                               replaceAll : (%, %) -> %
 replaceAt : (%, %, %) -> %                             replacePart : (%, %) -> %
 replaceRepeated : (%, %) -> %                          represents : Vector(%) -> %
 represents : (Vector(%), Vector(%)) -> %               residue : (%, %) -> %
 residueSum : (%, %) -> %                               rest : % -> %
 resultant : (%, %, %) -> %                             retract : % -> %
 retract : % -> Expression(Float)                       retract : % -> Expression(Integer)
 retract : % -> Fraction(Polynomial(%))                 retract : % -> Kernel(%)
 retract : % -> Polynomial(%)                           retract : % -> Symbol
 retractIfCan : % -> Union(%,"failed")                  retractIfCan : % -> Union(DoubleFloat,"failed")
 retractIfCan : % -> Union(JLFloat64,"failed")          retractIfCan : % -> Union(Kernel(%),"failed")
 retractIfCan : % -> Union(Polynomial(%),"failed")      retractIfCan : % -> Union(Symbol,"failed")
 reverse : % -> %                                       reverse : (%, WSInteger) -> %
 reverse : (%, WSList(WSInteger)) -> %                  riemannSiegelTheta : % -> %
 riemannSiegelZ : % -> %                                riemannZeta : % -> %
 riemannZeta : (%, %) -> %                              riffle : (%, %) -> %
 riffle : (%, %, %) -> %                                rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 rootOf : % -> %                                        rootOf : (%, Symbol) -> %
 rootOf : Polynomial(%) -> %                            rootOf : SparseUnivariatePolynomial(%) -> %
 rootReduce : % -> %                                    rootsOf : % -> List(%)
 rootsOf : (%, Symbol) -> List(%)                       rootsOf : Polynomial(%) -> List(%)
 round : % -> %                                         sample : () -> %
 scientificForm : % -> %                                scientificForm : (%, %) -> %
 sec : % -> %                                           sech : % -> %
 select : (%, %) -> %                                   select : (%, %, %) -> %
 series : (%, %) -> %                                   setIntersection : (%, %) -> %
 setelt : (%, Integer, %) -> %                          setelt! : (%, Integer, %) -> %
 siegelTheta : (%, %) -> %                              siegelTheta : (%, %, %) -> %
 sign : % -> %                                          simplify : % -> %
 simplify : (%, %) -> %                                 sin : % -> %
 sinc : % -> %                                          sinh : % -> %
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 solve : (%, %) -> WSList(WSList(%))                    solve : (%, %, %) -> WSList(WSList(%))
 solve : (Equation(%), %) -> WSList(WSList(%))          sort : % -> %
 sorted? : % -> Boolean                                 sphericalBesselJ : (%, %) -> %
 sphericalBesselY : (%, %) -> %                         sphericalHankelH1 : (%, %) -> %
 sphericalHankelH2 : (%, %) -> %                        sphericalHarmonicY : (%, %, %, %) -> %
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                stieltjesGamma : % -> %
 stieltjesGamma : (%, %) -> %                           string : % -> String
 struveH : (%, %) -> %                                  struveL : (%, %) -> %
 subResultants : (%, %, %) -> %                         subst : (%, Equation(%)) -> %
 subst : (%, List(Equation(%))) -> %                    subst : (%, List(Kernel(%)), List(%)) -> %
 subtractIfCan : (%, %) -> Union(%,"failed")            sum : (%, %) -> %
 sum : (%, %, Segment(Integer)) -> %                    sum : (%, Symbol) -> %
 summation : (%, SegmentBinding(%)) -> %                summation : (%, Symbol) -> %
 symmetricPolynomial : (%, WSList(%)) -> %              symmetricReduction : (%, WSList(%)) -> WSList(%)
 take : (%, Integer) -> %                               take : (%, WSList(WSInteger)) -> %
 tan : % -> %                                           tanh : % -> %
 toExpression : String -> %                             toExpression : (String, %) -> %
 toExpression : (String, %, %) -> %                     toString : % -> String
 toString : (%, %) -> String                            together : % -> %
 tower : % -> List(Kernel(%))                           tower : List(%) -> List(Kernel(%))
 trace : % -> %                                         traceMatrix : () -> Matrix(%)
 traceMatrix : Vector(%) -> Matrix(%)                   traditionalForm : % -> %
 trigExpand : % -> %                                    trigFactor : % -> %
 trigFactorList : % -> WSList(%)                        trigReduce : % -> %
 trigToExp : % -> %                                     union : (%, %) -> %
 unit? : % -> Boolean                                   unitCanonical : % -> %
 unitStep : % -> %                                      values : % -> %
 variables : % -> List(Symbol)                          variables : List(%) -> List(Symbol)
 variables : % -> WSList(%)                             weberE : (%, %) -> %
 weberE : (%, %, %) -> %                                weierstrassP : (%, %, %) -> %
 weierstrassPInverse : (%, %, %) -> %                   weierstrassPPrime : (%, %, %) -> %
 weierstrassSigma : (%, %, %) -> %                      weierstrassZeta : (%, %, %) -> %
 whittakerM : (%, %, %) -> %                            whittakerW : (%, %, %) -> %
 wilsonW : (%, %, %, %, %, %) -> %                      zernikeR : (%, %, %) -> %
 zero? : % -> Boolean                                   zeroOf : % -> %
 zeroOf : (%, Symbol) -> %                              zeroOf : Polynomial(%) -> %
 zeroOf : SparseUnivariatePolynomial(%) -> %            zerosOf : % -> List(%)
 zerosOf : (%, Symbol) -> List(%)                       zerosOf : Polynomial(%) -> List(%)
 ?~=? : (%, %) -> Boolean
 ?/? : (SparseMultivariatePolynomial(%,Kernel(%)), SparseMultivariatePolynomial(%,Kernel(%))) -> %
 D : (%, List(Symbol), List(NonNegativeInteger)) -> %
 characteristicPolynomial : % -> SparseUnivariatePolynomial(%)
 coerce : SparseMultivariatePolynomial(%,Kernel(%)) -> %
 coordinates : (Vector(%), Vector(%)) -> Matrix(%)
 definingPolynomial : () -> SparseUnivariatePolynomial(%)
 denom : % -> SparseMultivariatePolynomial(%,Kernel(%))
 derivationCoordinates : (Vector(%), (% -> %)) -> Matrix(%)
 differentiate : (%, List(%), List(NonNegativeInteger)) -> %
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> %
 differentiate : (%, (% -> %), NonNegativeInteger) -> %
 differentiate : (%, Symbol, NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 elt : (BasicOperator, %, %, %, %, %, %, %) -> %
 elt : (BasicOperator, %, %, %, %, %, %, %, %) -> %
 elt : (BasicOperator, %, %, %, %, %, %, %, %, %) -> %
 eval : (%, List(BasicOperator), List((% -> %))) -> %
 eval : (%, List(BasicOperator), List((List(%) -> %))) -> %
 eval : (%, List(Symbol), List((List(%) -> %))) -> %
 eval : (%, List(Symbol), List(NonNegativeInteger), List((% -> %))) -> %
 eval : (%, List(Symbol), List(NonNegativeInteger), List((List(%) -> %))) -> %
 eval : (%, Symbol, NonNegativeInteger, (% -> %)) -> %
 eval : (%, Symbol, NonNegativeInteger, (List(%) -> %)) -> %
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if % has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if % has PFECAT
 findInstance : (%, %, %, %) -> WSList(WSList(%))
 findInstance : (Equation(%), %) -> WSList(WSList(%))
 findInstance : (Equation(%), %, %) -> WSList(WSList(%))
 findInstance : (Equation(%), %, %, %) -> WSList(WSList(%))
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 inverseFourier : (WSList(%), WSList(%)) -> WSList(%)
 isExpt : % -> Union(Record(var: Kernel(%),exponent: Integer),"failed")
 isExpt : (%, BasicOperator) -> Union(Record(var: Kernel(%),exponent: Integer),"failed")
 isExpt : (%, Symbol) -> Union(Record(var: Kernel(%),exponent: Integer),"failed")
 isMult : % -> Union(Record(coef: Integer,var: Kernel(%)),"failed")
 isPower : % -> Union(Record(val: %,exponent: Integer),"failed")
 jlDisplay : (WSExpression, WSExpression) -> WSExpression
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 minPoly : Kernel(%) -> SparseUnivariatePolynomial(%)
 minimalPolynomial : % -> SparseUnivariatePolynomial(%)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 numer : % -> SparseMultivariatePolynomial(%,Kernel(%))
 polynomialQuotientRemainder : (%, %, %) -> WSList(%)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reduce : Fraction(SparseUnivariatePolynomial(%)) -> Union(%,"failed")
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(%),vec: Vector(%))
 regularRepresentation : (%, Vector(%)) -> Matrix(%)
 retract : % -> Fraction(Integer) if % has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Expression(Float),"failed")
 retractIfCan : % -> Union(Expression(Integer),"failed")
 retractIfCan : % -> Union(Fraction(Integer),"failed") if % has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Fraction(Polynomial(%)),"failed")
 rootOf : (SparseUnivariatePolynomial(%), Symbol) -> %
 rootSum : (%, SparseUnivariatePolynomial(%), Symbol) -> %
 rootsOf : SparseUnivariatePolynomial(%) -> List(%)
 rootsOf : (SparseUnivariatePolynomial(%), Symbol) -> List(%)
 solve : (Equation(%), %, %) -> WSList(WSList(%))
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if % has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if % has PFECAT
 symmetricReduction : (%, WSList(%), WSList(%)) -> WSList(%)
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
 univariate : (%, Kernel(%)) -> Fraction(SparseUnivariatePolynomial(%))
 zeroOf : (SparseUnivariatePolynomial(%), Symbol) -> %
 zerosOf : SparseUnivariatePolynomial(%) -> List(%)
 zerosOf : (SparseUnivariatePolynomial(%), Symbol) -> List(%)
```

## Operations added

### `EiEn` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L209)\]

EiEn(n,z) returns the exponential integral En(z).

- **Signature**: `(%,%)->%`

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L215)\]

Gamma(a,z1,z2) computes the generalized incomplete Gamma function.

- **Signature**: `(%,%,%)->%`

### `accountingForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1078)\]

accountingForm(x,n) returns the accounting printed representation of x.

- **Signature**: `(%)->%`

accountingForm(x,n) returns the accounting printed representation of x with n digits of precision.

- **Signature**: `(%,%)->%`

### `airyAiZero` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L146)\]

airyAiZero(n) is the n-th zero of the Airy function Ai(z).

- **Signature**: `(%)->%`

airyAiZero(n,x) is the n-th zero of the Airy function Ai(z) less than x.

- **Signature**: `(%,%)->%`

### `airyBiZero` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L151)\]

airyBiZero(n) is the n-th zero of the Airy function Bi(z).

- **Signature**: `(%)->%`

airyBiZero(n,x) is the n-th zero of the Airy function Bi(z) less than x.

- **Signature**: `(%,%)->%`

### `angerJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L120)\]

angerJ(v, n, z) is the associated Anger J function.

- **Signature**: `(%,%,%)->%`

### `apart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L910)\]

apart(expr) converts a rational expression as a sum of terms, reducing denominator(s).

- **Signature**: `(%)->%`

apart(expr, vars) converts a rational expression as a sum of terms as the one arg apart does but only for vars (the others are considered as constants).

- **Signature**: `(%,%)->%`

### `assuming` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L890)\]

assuming(assumption(s), expr) uses the assumptions for use of expr with refine, simplify and integrate for example. The assumption(s) are not always supported by MathLink. Use assumptions with 'refine' etc. directly instead.

- **Signature**: `(%,%)->%`

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L98)\]

atan(x,y) computes the arc tangent of y/x.

- **Signature**: `(%,%)->%`

### `barnesG` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L229)\]

barnesG(z) computes the Barnes G-function of z.

- **Signature**: `(%)->%`

### `baseForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1041)\]

baseForm(x, n) returns the printed representation of x in base b.

- **Signature**: `(%,%)->%`

### `besselJZero` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L128)\]

besselJZero(n,x) returns the n-th zero of the Bessel J n-th function.

- **Signature**: `(%,%)->%`

### `besselYZero` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L131)\]

besselYZero(n,x) returns the n-th zero of the Bessel Y n-th function.

- **Signature**: `(%,%)->%`

### `betaRegularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L211)\]

betaRegularized(x,a,b) computes the regularized incomplete Beta function.

- **Signature**: `(%,%,%)->%`

### `cancel` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L922)\]

cancel(expr) cancels common factors in numerators and denominators of the rational expression expr.

- **Signature**: `(%)->%`

### `catalan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L88)\]

catalan() returns Catalan's constant.

- **Signature**: `()->%`

### `ceiling` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L78)\]

ceiling(x) returns the smallest integer greater than or equal to x.

- **Signature**: `(%)->%`

### `chebyshevT` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L258)\]

chebyshevT(n, x) returns the Chebyshev polynomial of the first kind or evaluates it at x if x is a number.

- **Signature**: `(%,%)->%`

### `chebyshevU` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L261)\]

chebyshevU(n, x) returns Chebyshev polynomial of the second kind or evaluates it at x if x is a number.

- **Signature**: `(%,%)->%`

### `coefficient` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L708)\]

coefficient(p,expr) returns the coefficient of expr in p. 

**Example**:
```fricas
x:= jWSExpr x
```

**Example**:
```fricas
coefficient((x - y)^4, x * y^3)
```

- **Signature**: `(%,%)->%`

coefficient(p, expr, n) returns the coefficient of expr^n in p.

- **Signature**: `(%,%,%)->%`

### `coefficientList` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L714)\]

coefficientList(p,expr) returns the list of coefficients of expr in p.

- **Signature**: `(%,%)->%`

### `coefficientRules` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L716)\]

coefficientRules(p) returns the coefficients and exponents of p as WS rules.

- **Signature**: `(%)->%`

coefficientRules(p,vars) returns the coefficients and exponents of p with respect to var(s) as WS rules.

- **Signature**: `(%,%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1128)\]

coerce(gi) coerces gi to a WSExpression. Convenience function.

- **Signature**: `(Complex(Integer))->%`

coerce(eq) coerces the equation eq to a WSExpression equation.

- **Signature**: `(Equation(%))->%`

coerce(f) coerces the floating point number f to a WSExpression. Convenience function.

- **Signature**: `(Float)->%`

coerce(q) coerces the rational q to a WSExpression. Convenience function.

- **Signature**: `(Fraction(Integer))->%`

coerce(z) coerces the integer z to a WSExpression. Convenience function.

- **Signature**: `(Integer)->%`

coerce(list) coerces list of WSExpression.

- **Signature**: `(List(%))->%`

coerce(str) coerces the string str to a WSExpression evaluating str as a Wolfram Symbolic Language Expression. For example: 

**Example**:
```fricas
expr := "Sqrt[x]"::WSEXPR;jlEval(expr,"x=2.0")
```

- **Signature**: `(String)->%`

coerce(sym) coerces sym to a WSExpression.

- **Signature**: `(Symbol)->%`
- **Signature**: `(WSSymbol)->%`

coerce(expr) coerces expr to a WSInteger if possible.

- **Signature**: `(%)->WSInteger`

coerce(expr) coerces expr to a WSRational if possible.

- **Signature**: `(%)->WSRational`

### `collect` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L904)\]

collect(expr, var) collects same power terms with respect to variable var.

- **Signature**: `(%,%)->%`

collect(expr, vars) collects same power terms with respect to variables in vars.

- **Signature**: `(%,WSList(%))->%`

### `complexExpand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L942)\]

complexExpand(expr) expands expr assuming variables are real.

- **Signature**: `(%)->%`

complexExpand(expr, cvars) expands expr assuming all but cvars variables are real.

- **Signature**: `(%,%)->%`

### `coulombF` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L299)\]

coulombF(l,eta,ro) is the regular Coulomb wave function.

- **Signature**: `(%,%,%)->%`

### `coulombG` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L301)\]

coulombG(l,eta,ro) is the irregular Coulomb wave function.

- **Signature**: `(%,%,%)->%`

### `coulombH1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L303)\]

coulombH1(l,eta,ro) is the incoming irregular Coulomb wave function H^(+).

- **Signature**: `(%,%,%)->%`

### `coulombH2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L305)\]

coulombH2(l,eta,ro) is the incoming irregular Coulomb wave function H^(-).

- **Signature**: `(%,%,%)->%`

### `dSolve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L586)\]

dSolve(expr, funcs, vars) solves the (list of) differential equation(s) expr for the function(s) funcs with independent variable(s) vars. 

**Example**:
```fricas
x:=jWSExpr x;
```

**Example**:
```fricas
fx:=derivative(operator('f),0,x)
```

**Example**:
```fricas
fpr imex:=derivative(operator('f),1,x)
```

**Example**:
```fricas
dSolve(jWSEqual(fprimex + fx , a *sin(x)),fx,x)
```

- **Signature**: `(%,%,%)->%`

dSolve(eq, func,var) solves the differential equation eq for the function(s) funcs with independent variable(s) vars. 

**Example**:
```fricas
x:=jWSExpr x;
```

**Example**:
```fricas
fx:=derivative(operator('f),0,x)
```

**Example**:
```fricas
fprimex:=derivative(ope rator('f),1,x)
```

**Example**:
```fricas
dSolve(fprimex + fx = a * sin(x)/cos(x),fx,x)
```

- **Signature**: `(Equation(%),%,%)->%`

### `dSolveValue` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L600)\]

dSolveValue(expr,funcs, vars) returns the value determined by the differential equation(s) in expr for the function(s) funcs with independent variable(s) vars. 

**Example**:
```fricas
x:=jWSExpr x;
```

**Example**:
```fricas
f:=derivative(operator('f ),0)
```

**Example**:
```fricas
f0:=derivative(operator('f),0,0)
```

**Example**:
```fricas
fx:=derivative(operator('f),0,x)
```

**Example**:
```fricas
fprime x:=derivative(operator('f),1,x)
```

**Example**:
```fricas
dSolveValue(jWSExpr([jWSEqual(fprimex + fx , a *sin(x)/cos(x)), jWS Equal(f0,0)]),f,x)
```

- **Signature**: `(%,%,%)->%`

dSolveValue(eq, func,var) returns the value determined by the differential equation eq for the function func with independent variable var.

- **Signature**: `(Equation(%),%,%)->%`

### `dawson` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L104)\]

dawson(x) computes the Dawson integral of x.

- **Signature**: `(%)->%`

### `decimalForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1050)\]

decimalForm(x) returns the printed representation of x in decimal form i.e. without scientific notation.

- **Signature**: `(%)->%`

decimalForm(x, expr) returns the printed representation of x in decimal form with expr as specification (number of digits of precision or a 2-list of number of digits and the number of digits after the decimal point).

- **Signature**: `(%,%)->%`

### `decompose` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L836)\]

decompose(poly, x) is a polynomial decomposition function, here, related to x.

- **Signature**: `(%,%)->WSList(%)`

### `dedekindEta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L374)\]

dedekindEta(tau) computes the Dedekind modular elliptic eta.

- **Signature**: `(%)->%`

### `defined?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L35)\]

defined?(sym) checks whether or not sym is a WS symbol.

- **Signature**: `(%)->Boolean`

### `degree` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L82)\]

degree() returns conversion factor from degrees to radians, π/180.

- **Signature**: `()->%`

### `denominator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L672)\]

denominator(expr) returns the denominator of expr.

- **Signature**: `(%)->%`

### `derivative` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L578)\]

derivative(func,n) returns the derivative of order n of func. 

**Example**:
```fricas
fprime:=derivative(operator('f),1)
```

- **Signature**: `(BasicOperator,%)->%`

derivative(func, n, var) returns the derivative of order n of func applied to var.

**Example**:
```fricas
x := jWSExpr x
```

**Example**:
```fricas
fprimex:=derivative(operator('f),1,x)
```

- **Signature**: `(BasicOperator,%,%)->%`

### `digamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L126)\]

digamma(n,z) is the n-th derivative of the digamma function.

- **Signature**: `(%,%)->%`

### `dirichletEta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L376)\]

dirichletEta(z) computes the Dirichlet eta.

- **Signature**: `(%)->%`

### `dirichletL` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L161)\]

dirichletL(k,j,s) returns Dirichlet L-function of s, modulus k, index j.

- **Signature**: `(%,%,%)->%`

### `discriminant` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L775)\]

discriminant(p, x) returns the discriminant of p with respect to x.

- **Signature**: `(%,%)->%`

### `distribute` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L928)\]

distribute(expr) distributes expr over addition. For illustration: 

**Example**:
```fricas
distribute(jWSExpr "(x + y) * (a + b + c)")
```

- **Signature**: `(%)->%`

distribute(f,g) distributes f over g.

- **Signature**: `(%,%)->%`

### `ellipticE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L362)\]

ellipticE(x) computes the complete elliptic integral of the second kind.

- **Signature**: `(%)->%`

ellipticE(phi,m) computes the elliptic integral of the second kind.

- **Signature**: `(%,%)->%`

### `ellipticF` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L360)\]

ellipticF(phi,m) computes the elliptic integral of the first kind.

- **Signature**: `(%,%)->%`

### `ellipticK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L357)\]

ellipticK(m) computes the complete elliptic integral of the first kind.

- **Signature**: `(%)->%`

### `ellipticPi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L366)\]

ellipticPi(n,m) computes the complete elliptic integral of the third kind.

- **Signature**: `(%,%)->%`

ellipticPi(n,phi,m) computes the elliptic integral of the third kind.

- **Signature**: `(%,%,%)->%`

### `ellipticTheta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L347)\]

ellipticTheta(a, u, q) computes the theta function, a ranges from 1 to 4.

- **Signature**: `(%,%,%)->%`

### `ellipticThetaPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L350)\]

ellipticThetaPrime(a, u, q) computes the derivative of the theta function, a ranges from 1 to 4.

- **Signature**: `(%,%,%)->%`

### `engineeringForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1064)\]

engineeringForm(x) returns the printed representation of x in engineering form.

- **Signature**: `(%)->%`

engineeringForm(x,n) returns the printed representation of x in engineering form with n digits of precision.

- **Signature**: `(%,%)->%`

### `erf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L100)\]

erf(x,x1) computes the generalized error function.

- **Signature**: `(%,%)->%`

### `erfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L102)\]

erfc(x) computes the complementary error function.

- **Signature**: `(%)->%`

### `eulerE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L110)\]

eulerE(n) returns the Euler number En.

- **Signature**: `(WSInteger)->%`

eulerE(n,z) returns the Euler E polynomial of degree n.

- **Signature**: `(WSInteger,%)->%`

### `eulerGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L84)\]

eulerGamma() returns Euler's constant Gamma(γ).

- **Signature**: `()->%`

### `eulerPhi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L107)\]

eulerPhi(n) is the totient function, i.e. the number of integers that are relatively prime to n in the range [1,n].

- **Signature**: `(WSInteger)->%`

### `exactNumber?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L51)\]

exactNumber?(x) checks whether or not x is an exact number.

- **Signature**: `(%)->Boolean`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L80)\]

exp() returns ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `expToTrig` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L972)\]

expToTrig(expr) returns expr with exponentials converted to (hyperbolic) trigonometric functions.

- **Signature**: `(%)->%`

### `expand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L895)\]

expand(expr) puts out products and positive powers of integers of the expression expr.

- **Signature**: `(%)->%`

expand(expr, opt) is the expand version with excluded pattern-s or any other options available (for example "Modulus->p").

- **Signature**: `(%,%)->%`

### `expandDenominator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L919)\]

expandDenominator(expr) expands denominators of rational expression expr.

- **Signature**: `(%)->%`

### `expandNumerator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L917)\]

expandNumerator(expr) expands numerators of rational expression expr.

- **Signature**: `(%)->%`

### `exponent` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L697)\]

exponent(p,expr) returns the maximum exponent of p for expr. 

**Example**:
```fricas
x:= jWSExpr x;y := jWSExpr y
```

**Example**:
```fricas
p:=(x^2-2)^3*(y*x^3+x^11*y^7)*(y^5+x*y^2+x^11+y)
```

**Example**:
```fricas
exponent(%,(x^2-2))
```

- **Signature**: `(%,%)->%`

exponent(p, expr, map) applies map to the exponents related to expr and returns it. By default map = "Max".E xample: x:= jWSExpr x;y := jWSExpr y

**Example**:
```fricas
p:=expand((x^2-2)^3*(y*x^3+x^11*y^7)*(y^5+x*y^2+x^11+y))
```

Exa mple: exponent(p,x,"Min")

- **Signature**: `(%,%,%)->%`

### `extendedExpand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L901)\]

extendedExpand(expr) puts out all products and positive powers of integers.

- **Signature**: `(%)->%`

### `extendedSimplify` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L876)\]

extendedSimplify(expr) is the extended version of simplify. This is the full version of simplify.

**Example**:
```fricas
x: = jWSExpr x
```

**Example**:
```fricas
expr := Gamma(x)/Gamma(x-1) Compare with simplify(expr).
```

**Example**:
```fricas
extendedSimplify(ex pr)
```

- **Signature**: `(%)->%`

extendedSimplify(expr, assumptions) is the extended version of simplify with respect to assumptions or ExcludedForms. This is the full version.

- **Signature**: `(%,%)->%`

### `extract` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1017)\]

extract(expr,i) returns the i-th element of expr seen as a list.

- **Signature**: `(%,NonNegativeInteger)->%`

### `factor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L797)\]

factor(expr) factors the expression or polynomial expr.

- **Signature**: `(%)->%`

factor(expr, opt) factors the expression or polynomial expr. For example: 

**Example**:
```fricas
x := jWSExpr x;
```

**Example**:
```fricas
factor(1 + x^2, "GaussianIntegers -> True")
```

- **Signature**: `(%,%)->%`

### `factorList` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L823)\]

factorList(expr) factors the expression or polynomial expr, but returns the result as a list of pairs (factor, exponent).

- **Signature**: `(%)->WSList(WSList(%))`

### `factorPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L790)\]

factorPolynomial(p) factorizes the polynomial p. For example: 

**Example**:
```fricas
x := jWSExpr x
```

**Example**:
```fricas
p:=expand(ch ebyshevT(7,x)* chebyshevU(9,x))
```

**Example**:
```fricas
factorPolynomial p
```

- **Signature**: `(%)->%`

### `factorSquareFree` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L804)\]

factorSquareFree(expr) factors the expression or polynomial expr in square free factors.

- **Signature**: `(%)->%`

### `factorSquareFreeList` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L826)\]

factorSquareFreeList(expr) factors the expression or polynomial expr in square free factors but returns the result as a list of pairs (factor, exponent).

- **Signature**: `(%)->WSList(WSList(%))`

### `factorTerms` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L807)\]

factorTerms(p) factors out numerical factors of the expression or polynomial expr.

- **Signature**: `(%)->%`

factorTerms(p, var) factors out numerical factors of the expression or polynomial expr without those related to var.

- **Signature**: `(%,%)->%`

factorTerms(p, vars) factors the expression or polynomial expr by pulling out numerical factors without thoserelated to var(s).

- **Signature**: `(%,WSList(%))->%`

### `factorTermsList` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L816)\]

factorTermsList(expr) is the counterpart of factorTerms but here returned as a list of pairs (numerical factor, polynomial factor).

- **Signature**: `(%)->WSList(%)`

factorTermsList(expr, vars) is the counterpart of factorTerms but here returned as a list of pairs (numericalfactor, polynomial factor). The numerical factors related to var(s) are not factored.

- **Signature**: `(%,%)->WSList(%)`

### `fibonacci` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L233)\]

fibonacci(n, x) returns the Fibonacci polynomial or evaluates it at x if x is a number.

- **Signature**: `(%,%)->%`

### `findInstance` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L652)\]

findInstance(expr,lvars) tries to find an instance of the (in)equation in expr.

- **Signature**: `(%,%)->WSList(WSList(%))`

findInstance(expr,lvars,dom) tries to find an instance to the equation in expr.

- **Signature**: `(%,%,%)->WSList(WSList(%))`

findInstance(expr,lvars,dom, n) tries to find n instance(s) to the (in)equation in expr.

- **Signature**: `(%,%,%,%)->WSList(WSList(%))`

findInstance(expr,lvars) tries to find an instance of the equation in expr.

- **Signature**: `(Equation(%),%)->WSList(WSList(%))`

findInstance(expr,lvars,dom) tries to find an instance of the equation in expr.

- **Signature**: `(Equation(%),%,%)->WSList(WSList(%))`

findInstance(expr,lvars,dom,n) tries to find n instance(s) to the equation in expr.

- **Signature**: `(Equation(%),%,%,%)->WSList(WSList(%))`

### `findRoot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L849)\]

findRoot(expr,start) tries to find the root of expr starting at start. 

**Example**:
```fricas
x:= jWSExpr x
```

**Example**:
```fricas
find Root(sin(x) + cos(x), "{x, 0}")
```

- **Signature**: `(%,%)->%`

### `floor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L76)\]

floor(x) returns the greatest integer less than or equal to x

- **Signature**: `(%)->%`

### `fourier` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L549)\]

fourier(expr) returns the discrete Fourier transform from a list of numbers.

- **Signature**: `(%)->%`

fourier(expr, pos) returns the elements of the discrete Fourier transform from a list of numbers with position(s) in the list pos.

- **Signature**: `(%,%)->%`

fourier(list) returns the discrete Fourier transform from the list of numbers.

- **Signature**: `(WSList(%))->WSList(%)`

fourier(list, lpos) returns the elements of the discrete Fourier transform from a list of numbers with position(s) in the list lpos.

- **Signature**: `(WSList(%),WSList(%))->WSList(%)`

### `fromCoefficientRules` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L722)\]

fromCoefficientRules(list, vars) constructs the polynomial from the list of coefficients and exponents rules.

**Example**:
```fricas
x:= jWSExpr x;y := jWSExpr y
```

**Example**:
```fricas
coefficientRules((x + y)^2+x^11,jWSExpr [x,y])
```

**Example**:
```fricas
fr omCoefficientRules(%, jWSExpr [x,y])
```

- **Signature**: `(%,%)->%`

### `functionExpand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L950)\]

functionExpand(expr) tries to expand functions in expr to more elementary functions. For example:

**Example**:
```fricas
fu nctionExpand sphericalBesselJ(3,8)
```

- **Signature**: `(%)->%`

functionExpand(expr,assumptions) tries to expand functions in expr to more elementary functions assuming thatassumptions are satisfied.

- **Signature**: `(%,%)->%`

### `gammaRegularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L213)\]

gammaRegularized(a,x) computes the regularized incomplete Gamma function.

- **Signature**: `(%,%)->%`

### `gegenbauerC` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L252)\]

gegenbauerC(n,x) returns the renormalized form of the Gegenbauer polynomial or evaluates it at x if x is a number.

- **Signature**: `(%,%)->%`

gegenbauerC(n,lambda,x) returns the Gegenbauer polynomial or evaluates it at x if x is a number.

- **Signature**: `(%,%,%)->%`

### `goldenRatio` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L90)\]

goldenRatio() returns the golden ratio.

- **Signature**: `()->%`

### `groebnerBasis` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L777)\]

groebnerBasis(lpoly, lvar) computes a Groebner basis from the list of polynomials lpoly relative to the list of vars lvars.

- **Signature**: `(%,%)->WSList(%)`

groebnerBasis(lpoly, lvar, opt) computes a Groebner basis from the list of polynomials lpoly relative to the list of variables in lvars without variables in opt. Opt can also give the modulus to compute it: "Modulus -> p".

- **Signature**: `(%,%,%)->WSList(%)`

### `gudermannian` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L122)\]

gudermannian(z) computes the Gudermannian of z.

- **Signature**: `(%)->%`

### `guessGeneratingFunction` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L382)\]

guessGeneratingFunction(l,x) finds the generating function applied to x from the list of coefficients l. Or from the list of lists l (list of pairs as list (n-th, coef-th)). For example:

**Example**:
```fricas
guessGeneratingFunction (jWSExpr("1,2,4,8"),x)
```

- **Signature**: `(%,%)->%`

guessGeneratingFunction(l,x) finds the generating function applied to x from the list of coefficients l. Or from the list of lists l (list of pairs as list (n-th, coef-th)). For example:

**Example**:
```fricas
guessGeneratingFunction ([1,2,4,8],x)
```

- **Signature**: `(WSList(%),%)->%`

### `guessSequenceFunction` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L394)\]

guessSequenceFunction(l) finds the sequence from the list of coefficients l Or from the list of lists l (listof pairs as list (n-th, coef-th)). For example: 

**Example**:
```fricas
guessSequenceFunction(jWSExpr("1,2,4,8"))
```

- **Signature**: `(%)->%`

guessSequenceFunction(l,x) finds the sequence function applied to x from the list of coefficients l. For example: 

**Example**:
```fricas
guessSequenceFunction([1,2,4,jWSInt 8]::WSList(WSInteger),x)
```

- **Signature**: `(%,%)->%`

guessSequenceFunction(l) finds the sequence from the list of coefficients l. For example:

**Example**:
```fricas
guessSeque nceFunction(jWSExpr("1,2,4,8"))
```

- **Signature**: `(WSList(WSInteger))->%`

### `haversine` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L116)\]

haversine(z) computes the Haversine of z.

- **Signature**: `(%)->%`

### `hermiteH` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L264)\]

hermiteH(n, x) returns the Hermite polynomial or evaluates it at x if x is a number.

- **Signature**: `(%,%)->%`

### `hornerForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L934)\]

hornerForm(expr, x) returns the Horner form of expr (minimizing multiplications).

- **Signature**: `(%,%)->%`

### `hurwitzLerchPhi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L199)\]

hurwitzLerchPhi(z,s,a) computes the Hurwitz–Lerch transcendent phi function.

- **Signature**: `(%,%,%)->%`

### `hurwitzZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L197)\]

hurwitzZeta(s,a) computes the Hurwitz zeta.

- **Signature**: `(%,%)->%`

### `hyperFactorial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L231)\]

hyperFactorial(n) computes the hyperfactorial of n.

- **Signature**: `(%)->%`

### `hypergeometric0F1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L281)\]

hypergeometric0F1(a,z) is the hypergeometric 0F1.

- **Signature**: `(%,%)->%`

### `hypergeometric0F1Regularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L283)\]

hypergeometric0F1Regularized(a,z) is the regularized hypergeometric 0F1.

- **Signature**: `(%,%)->%`

### `hypergeometric1F1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L285)\]

hypergeometric1F1(a,b,z) is the Kummer confluent hypergeometric function 1F1.

- **Signature**: `(%,%,%)->%`

### `hypergeometric1F1Regularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L288)\]

hypergeometric1F1Regularized(a,b,z) is the regularized confluent hypergeometric function 1F1.

- **Signature**: `(%,%,%)->%`

### `hypergeometricU` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L291)\]

hypergeometricU(a,b,z) is the confluent hypergeometric function U.

- **Signature**: `(%,%,%)->%`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L42)\]

integer?(i) checks whether or not i is an integer.

- **Signature**: `(%)->Boolean`

### `integrate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L499)\]

integrate(expr, opts|var) integrates expr with respect to opt or var as options. For example:

**Example**:
```fricas
x:=jWS Expr x;integrate(1/(x^4-1),x)
```

**Example**:
```fricas
opt:=jWSList [x,-1,1]
```

**Example**:
```fricas
integrate(cos(x),opt) => 2 sin(1)
```

**Example**:
```fricas
integrate(cos(x),"{x,-1.0,1.0}") => 1.68294
```

- **Signature**: `(%,%)->%`

integrate(expr, var, seg) is the definite integration of expr with respect to var using segment seg.

- **Signature**: `(%,%,Segment(Integer))->%`

integrate(expr, var) is the indefinite integration of expr with respect to var.

- **Signature**: `(%,Symbol)->%`

### `interpolatingPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L839)\]

interpolatingPolynomial(lpoly,x) interpolates the list of polynomials lpoly with respect to x.

- **Signature**: `(%,%)->%`

### `inverseBetaRegularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L217)\]

inverseBetaRegularized(s,a,b) computes the inverse Beta regularized function.

- **Signature**: `(%,%,%)->%`

### `inverseErf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L204)\]

inverseErf(x) computes the inverse error function of x.

- **Signature**: `(%)->%`

### `inverseErfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L206)\]

inverseErfc(x) computes the inverse complementary error function of x.

- **Signature**: `(%)->%`

### `inverseFourier` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L563)\]

inverseFourier(expr) returns the discrete inverse Fourier transform from a list of numbers.

- **Signature**: `(%)->%`
- **Signature**: `(WSList(%))->%`

inverseFourier(expr, pos) returns the elements of the discrete inverse Fourier transform from a list of numbers with position(s) in the list pos.

- **Signature**: `(%,%)->%`

inverseFourier(list, lpos) returns the elements of the discrete inverse Fourier transform from a list of numbers with position(s) in the list lpos.

- **Signature**: `(WSList(%),WSList(%))->WSList(%)`

### `inverseGammaRegularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L219)\]

inverseGammaRegularized(a,s) computes the inverse Gamma regularized function.

- **Signature**: `(%,%)->%`

### `inverseGudermannian` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L124)\]

inverseGudermannian(z) computes the inverse Gudermannian.

- **Signature**: `(%)->%`

### `inverseHaversine` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L118)\]

inverseHaversine(z) computes the inverse Haversine.

- **Signature**: `(%)->%`

### `inverseJacobiCn` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L345)\]

inverseJacobiCn(nu, m) computes the inverse Jacobi CN elliptic function.

- **Signature**: `(%,%)->%`

### `inverseJacobiSn` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L343)\]

inverseJacobiSn(nu, m) computes the inverse Jacobi SN elliptic function.

- **Signature**: `(%,%)->%`

### `irreducible?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L830)\]

irreducible?(p) checks whether or not p is irreducible.

- **Signature**: `(%)->Boolean`

irreducible?(p) checks whether or not p is irreducible over Gaussian rationals or algebraic extensions.

- **Signature**: `(%,%)->Boolean`

### `jWSAssociation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1183)\]

jWSAssociation(rules) returns the associations (key->value) created from the rules.

- **Signature**: `(WSList(%))->%`

### `jWSData` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1168)\]

jWSData() returns the list of WS symbols. Note: Currently unprintable.

- **Signature**: `()->%`

jWSData(sym) returns the entity(ies) associated to sym(s).

- **Signature**: `(%)->%`
- **Signature**: `(String)->%`

jWSData(sym, prop) returns the property of sym.

- **Signature**: `(%,%)->%`
- **Signature**: `(String,String)->%`

jWSData(sym, prop, ann) returns the annotation for the property of sym.

- **Signature**: `(%,%,%)->%`
- **Signature**: `(String,String,String)->%`

### `jWSEqual` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1198)\]

jWSEqual(lhs,rhs) returns the Julia WS equality lhs == rhs.

- **Signature**: `(%,%)->%`

### `jWSExpr` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1218)\]

jWSExpr(r) returns the DoubleFloat as a WSExpression.

- **Signature**: `(DoubleFloat)->%`

jWSExpr(r) returns the Float r as a WSExpression.

- **Signature**: `(Float)->%`

jWSExpr(q) returns the Fraction(Integer) q as a WSExpression.

- **Signature**: `(Fraction(Integer))->%`

jWSExpr(z) returns the Integer z as a WSExpression.

- **Signature**: `(Integer)->%`

jWSExpr(r) returns the JLFloat r as a WSExpression.

- **Signature**: `(JLFloat)->%`

jWSExpr(r) returns the JLFloat64 as a WSExpression.

- **Signature**: `(JLFloat64)->%`

jWSExpr(list) returns the list of WSExpression as a WSExpression.

- **Signature**: `(List(%))->%`

jWSExpr(str) constructs str as a WSExpression evaluating str as a Wolfram Symbolic Language expression. For example: 

**Example**:
```fricas
jWSExpr "Factorial[5]"
```

**Example**:
```fricas
jWSExpr "3.14159"
```

**Example**:
```fricas
jlWSDateString(jWSExpr "Tomorr ow")
```

**Example**:
```fricas
toString jWSExpr "TextSentences[WikipediaData[_"Sun_"]][[;; 40]]"
```

**Example**:
```fricas
jWSExpr "Probabili ty[x < 1, x [Distributed] NormalDistribution[]]"
```

- **Signature**: `(String)->%`

jWSExpr(sym) coerces sym to a WSExpression. For example: x := jWSExpr x

- **Signature**: `(Symbol)->%`

### `jWSGreater` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1206)\]

jWSGreater(lhs,rhs) returns the Julia WS inequality lhs > rhs.

- **Signature**: `(%,%)->%`

### `jWSGreaterEqual` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1208)\]

jWSGreaterEqual(lhs,rhs) returns the Julia WS inequality lhs >= rhs.

- **Signature**: `(%,%)->%`

### `jWSLess` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1202)\]

jWSLess(lhs,rhs) returns the Julia WS inequality lhs < rhs.

- **Signature**: `(%,%)->%`

### `jWSLessEqual` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1204)\]

jWSLessEqual(lhs,rhs) returns the Julia WS inequality lhs <= rhs.

- **Signature**: `(%,%)->%`

### `jWSNotEqual` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1200)\]

jWSNotEqual(lhs,rhs) returns the Julia WS inequality lhs != rhs.

- **Signature**: `(%,%)->%`

### `jWSQuantity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1210)\]

jWSQuantity(jWSString(u)) returns quantity unit u of 1. For example: 

**Example**:
```fricas
jWSQuantity jWSString "Meter"
```

- **Signature**: `(%)->%`

jWSQuantity(x,jWSString(u)) returns quantity unit u of x. For example: 

**Example**:
```fricas
jWSQuantity(1.2, jWSString "M eter")
```

- **Signature**: `(%,%)->%`

### `jWSRule` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1190)\]

jWSRule(lhs,rhs) returns the Julia WS rule lhs->rhs.

- **Signature**: `(%,%)->%`

jWSRule(eq) returns the Julia WS rule lhs->rhs for the equation eq. For example:

**Example**:
```fricas
x := jWSExpr x
```

**Example**:
```fricas
p := 1/2 * legendreQ(5,x)
```

**Example**:
```fricas
replaceAll(p, jWSRule(x = jWSExpr "1.55555556444883838383833777333 333333"))
```

- **Signature**: `(Equation(%))->%`

### `jWSTable` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1186)\]

jWSTable(expr, range) applies the expr to the defined range.

- **Signature**: `(%,%)->WSList(%)`

jWSTable(expr, range1, range2) applies the expr to the defined ranges.

- **Signature**: `(%,%,%)->WSList(WSList(%))`

### `jacobiAmplitude` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L341)\]

jacobiAmplitude(u,m) computes the amplitude function am.

- **Signature**: `(%,%)->%`

### `jacobiP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L278)\]

jacobiP(n, a, b, x) returns the Jacobi polynomial or evaluates it at x if x is a number.

- **Signature**: `(%,%,%,%)->%`

### `jacobiTheta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L221)\]

jacobiTheta(n, z, m) are the Jacobi Theta functions.

- **Signature**: `(WSInteger,%,%)->%`

### `jacobiZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L371)\]

jacobiZeta(ϕ,m) computes the Jacobi Zeta function.

- **Signature**: `(%,%)->%`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1033)\]

jlDisplay(expr) returns the traditional form of expr. This is equivalent to: jWSExpr "Format[Sin[x]]" => sin(x)

- **Signature**: `(WSExpression)->WSExpression`

jlDisplay(expr, form) returns the `form` form of expr resulting for example in: "Format[Sin[x], TeXForm]" =>x

- **Signature**: `(WSExpression,WSExpression)->WSExpression`

### `jlEval` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L995)\]

jlEval(expr, param) evaluates expression expr with param as parameter(s). See also `eval` or `replaceAll`. For example: 

**Example**:
```fricas
x:=jWSExpr("x");jlEval(sqrt(x),"x=2.0")
```

- **Signature**: `(%,String)->%`

jlEval(expr, param1, param2) evaluates expression expr with param1 and param2 as parameters. See also `eval` or `replaceAll`. 

**Example**:
```fricas
a:=jWSExpr("a");b:=jWSExpr("b");
```

**Example**:
```fricas
jlEval(sqrt(a^2+b^2),"a=1.0","b=1.0")
```

- **Signature**: `(%,String,String)->%`

jlEval(expr, param11, param2, param3) evaluates expression expr with param1, param2 and param3 as parameters.See also `eval` or `replaceAll`.

- **Signature**: `(%,String,String,String)->%`

### `jlGreedyEval` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L992)\]

jlGreedyEval(bool) toggles automatic arithmetic operations. Plus[a, a] can become Times[2, a] using or not Julia `weval`.

- **Signature**: `(Boolean)->Void`

### `jlWSAccuracy` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1159)\]

jlWSAccuracy(expr) get accuracy of expr.

- **Signature**: `(%)->%`

### `jlWSDefined?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L66)\]

jlWSDefined?(sym) checks whether or not the symbol sym is defined in the WS language. For example:

**Example**:
```fricas
j lWSDefined? "Sin" => true
```

- **Signature**: `(String)->Boolean`

### `jlWSPrecision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1157)\]

jlWSPrecision get precision of expr.

- **Signature**: `(%)->%`

### `jlWSSetAccuracy` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1163)\]

jlWSSetAccuracy(expr, acc) sets accuracy of expr to acc.

- **Signature**: `(%,%)->%`

### `jlWSSetOptions` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1165)\]

jlWSSetOptions(type, opts) sets some internal engine options.

- **Signature**: `(%,%)->%`

### `jlWSSetPrecision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1161)\]

jlWSSetPrecision(expr, prec) sets precision of expr to prec.

- **Signature**: `(%,%)->%`

### `key?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L61)\]

key?(assoc,key) checks whether or not key exists in the association assoc.

- **Signature**: `(%,%)->Boolean`

### `keys` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1019)\]

keys(expr) returns the key elements in expr if any.

- **Signature**: `(%)->%`

### `kleinInvariantJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L378)\]

kleinInvariantJ(tau) computes the Klein's absolute invariant.

- **Signature**: `(%)->%`

### `laguerreL` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L267)\]

laguerreL(n, x) returns the Laguerre polynomial or evaluates it at x if x is a number. For example:

**Example**:
```fricas
laguerreL(5, jWSExpr x)
```

- **Signature**: `(%,%)->%`

laguerreL(n, a, x) returns the generalized Laguerre polynomial or evaluates it at x if x is a number.

- **Signature**: `(%,%,%)->%`

### `lambertW` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L114)\]

lambertW(k,z) returns the k-th solution to the LambertW function.

- **Signature**: `(WSInteger,%)->%`

### `legendreP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L236)\]

legendreP(n, x) returns the Legendre polynomial of the first kind or evaluates it at x if x is a number.

- **Signature**: `(%,%)->%`

legendreP(n, m, x) returns the associated Legendre polynomial of the first type or evaluates it at x if x is a number.

- **Signature**: `(%,%,%)->%`

### `legendreQ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L239)\]

legendreQ(n, x) returns the Legendre function of the second kind or evaluates it at x if x is a number.Examp le: legendreQ(3,jWSExpr x)

- **Signature**: `(%,%)->%`

legendreQ(n, m, x) returns the associated Legendre function of the second kind or evaluates it at x if x is anumber.

- **Signature**: `(%,%,%)->%`

### `length` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1010)\]

length(expr) returns the length of expr seen as a list.

- **Signature**: `(%)->%`

### `lerchPhi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L164)\]

lerchPhi(z,s,a) returns Lerch's transcendent phi of arguments.

- **Signature**: `(%,%,%)->%`

### `level` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1012)\]

level(expr, lev) returns the list of expression expr at level lev.

- **Signature**: `(%,%)->WSList(%)`

level(expr, lev, head) returns the list of expression expr at level lev with heads if head is true.

- **Signature**: `(%,%,Boolean)->WSList(%)`

### `limit` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L520)\]

limit(expr, params) returns the limit, eventually nested or multivariate, of expr. For example:

**Example**:
```fricas
x:=j WSExpr x; limit(sin(x)-sin(x-1/x),"x->Infinity")
```

- **Signature**: `(%,%)->%`

### `log10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L96)\]

log10(x) computes the logarithm of x in base 10.

- **Signature**: `(%)->%`

### `log2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L94)\]

log2(x) computes the logarithm of x in base 2.

- **Signature**: `(%)->%`

### `logBarnesG` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L227)\]

logBarnesG(x) is the logarithm of the Barnes G-function.

- **Signature**: `(%)->%`

### `logGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L225)\]

logGamma(z) returns the log-Gamma of z.

- **Signature**: `(%)->%`

### `lookup` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1023)\]

lookup(assocs,keys) returns value(s) associated to key(s).

- **Signature**: `(%,%)->%`

lookup(assocs,keys, defaultval) returns value(s) associated to key(s) if key(s) exist(s), otherwise defaultval

- **Signature**: `(%,%,%)->%`

### `machineNumber?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L48)\]

machineNumber?(expr) checks whether or not expr is a CPU/GPU supported number.

- **Signature**: `(%)->Boolean`

### `mathieuC` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L322)\]

mathieuC(a,q,z) is the even Mathieu function with characteristic a and parameter q.

- **Signature**: `(%,%,%)->%`

### `mathieuCPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L328)\]

mathieuCPrime(a,q,z) is the derivative of the even Mathieu function.

- **Signature**: `(%,%,%)->%`

### `mathieuCharacteristicA` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L332)\]

mathieuCharacteristicA(r,q) returns the characteristic for even Mathieu function.

- **Signature**: `(%,%)->%`

### `mathieuCharacteristicB` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L335)\]

mathieuCharacteristicB(r,q) returns the characteristic for odd Mathieu function.

- **Signature**: `(%,%)->%`

### `mathieuCharacteristicExponent` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L338)\]

mathieuCharacteristicExponent(a,q) returns the characteristic exponent of the Mathieu function.

- **Signature**: `(%,%)->%`

### `mathieuS` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L325)\]

mathieuS(b,q,z) is the odd Mathieu function with characteristic b and parameter q.

- **Signature**: `(%,%,%)->%`

### `mathieuSPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L330)\]

mathieuSPrime(b,q,z) is the derivative of the odd Mathieu function.

- **Signature**: `(%,%,%)->%`

### `matrixForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1084)\]

matrixForm(mat) returns a pretty-printable form of mat i.e. its WS 'MatrixForm'.

- **Signature**: `(%)->%`

### `maxLimit` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L528)\]

maxLimit(expr, params) returns the max limit, eventually nested or multivariate, of expr.

- **Signature**: `(%,%)->%`

### `maximize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L475)\]

maximize(expr, vars) is the WS symbolic maximization function. expr can contain constraints if it is a WS list of constraints with function to maximize as the first element. Global optimization function otherwise.

- **Signature**: `(%,%)->%`

maximize(expr, vars, dom) is the WS symbolic maximization function. dom restricts the domain of variables, for example, Integers.

- **Signature**: `(%,%,%)->%`

maximize(expr, sym) symbolically maximizes expression function expr with respect to sym. expr can contain constraints if it is a WS list of constraints with function to maximize as the first element. Global optimizationfunction otherwise.

- **Signature**: `(%,Symbol)->%`

### `member?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L59)\]

member?(list, expr) checks if expr is in list.

- **Signature**: `(%,%)->Boolean`

### `minLimit` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L525)\]

minLimit(expr, params) returns the min limit, eventually nested or multivariate, of expr.

- **Signature**: `(%,%)->%`

### `minimalPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L843)\]

minimalPolynomial(expr,var) returns the minimal polynomial in the variable var of the expression expr.

- **Signature**: `(%,%)->%`

minimalPolynomial(expr,var, elem) returns the minimal polynomial in the variable var of the expression expr.

- **Signature**: `(%,%,%)->%`

### `minimize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L447)\]

minimize(expr, vars) is the WS symbolic minimization function. expr can contain constraints if it is a WS list of constraints with function to minimize as the first element. Global optimization function otherwise.

- **Signature**: `(%,%)->%`

minimize(expr, vars, dom) is the WS symbolic minimization function. dom restricts the domain of variables, for example, Integers.

- **Signature**: `(%,%,%)->%`

minimize(expr, sym) symbolically minimizes expression function expr with respect to sym. expr can contain constraints if it is a WS list of constraints with function to minimize as the first element. Global optimizationfunction otherwise.

- **Signature**: `(%,Symbol)->%`

### `missing?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L64)\]

missing?(data) checks whether or not data is Missing.

- **Signature**: `(%)->Boolean`

### `modularLambda` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L380)\]

modularLambda() computes the lambda modular function.

- **Signature**: `(%)->%`

### `monomialList` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L728)\]

monomialList(p) returns the list of monomials in p.

- **Signature**: `(%)->%`

### `negative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L57)\]

negative?(expr) checks whether or not expr is negative.

- **Signature**: `(%)->Boolean`

### `normal` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L857)\]

normal(expr) converts expr to a normal expression from different expression types. Can be applied to a power series for example. For example: 

**Example**:
```fricas
x:=jWSExpr x
```

**Example**:
```fricas
s:=series(exp(x),jWSExpr "{x,0,10}")
```

Examp le: normal(s)::EXPR INT

- **Signature**: `(%)->%`

normal(expr, list(Head)||Head) converts objects in expr to a normal expression form from different expressiontypes, with Head, or a list of Head-s.

- **Signature**: `(%,%)->%`

### `number?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L46)\]

number?(expr) checks whether or not expr is a number.

- **Signature**: `(%)->Boolean`

### `numberForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1043)\]

numberForm(x) returns the default printed representation of x.

- **Signature**: `(%)->%`

numberForm(x, expr) returns the approximate printed representation of x with expr as specification (number ofdigits of precision or a 2-list of number of digits and the number of digits after the decimal point).

- **Signature**: `(%,%)->%`

### `numerDenom` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L674)\]

numerDenom(expr) returns the numerator and denominator of expr.

- **Signature**: `(%)->WSList(%)`

### `numerator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L670)\]

numerator(expr) returns the numerator of expr.

- **Signature**: `(%)->%`

### `numeric?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L53)\]

numeric?(x) checks whether or not x is or would yield a number.

- **Signature**: `(%)->Boolean`

### `numericDSolve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L614)\]

numericDSolve(expr,fun,xrange) solves numerically the differential equation(s) in expr for the function fun, in the range xrange. Other combinations of parameters are also available.

- **Signature**: `(%,%,%)->%`

numericDSolve(expr,fun,xrange,yrange) solves numerically the differential equation(s) in expr for the function fun, in the ranges xrange and yrange. Other combinations of parameters are also available (see documentation).

- **Signature**: `(%,%,%,%)->%`

numericDSolve(eq,fun,xrange) solves numerically the differential equation eq for the function fun, in the range xrange.

- **Signature**: `(Equation(%),%,%)->%`

### `numericDSolveValue` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L625)\]

numericDSolveValue(expr,fun,xrange) returns the numerical value solution of the differential equation(s) in expr for the function fun, in the range xrange. Other combinations of parameters are also available (see documentation).

- **Signature**: `(%,%,%)->%`

numericDSolveValue(expr,fun,xrange,yrange) returns the numerical solution of the differential equation(s) in expr for the function fun, in the ranges xrange and yrange. Other combinations of parameters are also available (see documentation).

- **Signature**: `(%,%,%,%)->%`

numericDSolveValue(eq,fun,xrange) returns the numerical solution of the differential equation eq for the function fun, in the range xrange.

- **Signature**: `(Equation(%),%,%)->%`

### `numericIntegrate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L514)\]

numericIntegrate(expr, opt|var) integrates numerically expr with respect to opt or var as options.

- **Signature**: `(%,%)->%`

numericIntegrate(expr, var, seg) integrates expr using segment seg with respect to var.

- **Signature**: `(%,%,Segment(Integer))->%`

### `numericMaximize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L488)\]

numericMaximize(expr, vars) maximizes numerically the expression function expr with respect to vars.

- **Signature**: `(%,%)->%`

numericMaximize(expr, vars, dom) maximizes numerically the expression function expr with respect to vars and vars restricted to the domain dom.

- **Signature**: `(%,%,%)->%`

numericMaximize(expr, sym) maximizes numerically the expression function expr with respect to sym.

- **Signature**: `(%,Symbol)->%`

### `numericMinimize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L460)\]

numericMinimize(expr, vars) minimizes numerically the expression function expr with respect to vars. For example, global optimization from the SIAM 100 digits challenge: 

**Example**:
```fricas
x := jWSExpr(x);y:=jWSExpr y;
```

**Example**:
```fricas
expr := exp(sin(50*x))+sin(60*exp(y))+ sin(70*sin(x))+ sin(sin(80*y))-sin(10*(x+y))+(x^2+y^2)/4
```

**Example**:
```fricas
nu mericMinimize(expr, jWSList [x,y])
```

- **Signature**: `(%,%)->%`

numericMinimize(expr, vars, dom) minimizes numerically the expression function expr with respect to vars and vars restricted to the domain dom.

- **Signature**: `(%,%,%)->%`

numericMinimize(expr, sym) minimizes numerically the expression function expr with respect to sym.

- **Signature**: `(%,Symbol)->%`

### `numericProduct` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L438)\]

numericProduct(f(n),range) returns an evaluated numerical approximation of the sum f(imin) + ... + f(imax) defined by the list range, for example 

**Example**:
```fricas
jWSExpr("{i, imin, imax}") . See Wolfram language specifications .
```

- **Signature**: `(%,%)->%`

numericProduct(f(n),n, a..b) returns an evaluated numerical approximation of the product f(a) * ... * f(b).

- **Signature**: `(%,%,Segment(Integer))->%`

### `numericSolve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L540)\]

numericSolve(expr, vars) returns the solution(s) to the expression expr.

- **Signature**: `(%,%)->%`

numericSolve(eq, vars) returns the solution(s) to the equation eq.

- **Signature**: `(Equation(%),%)->%`

### `numericSum` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L430)\]

numericSum(f(n),range) returns an evaluated numerical approximation of the sum f(imin) + ... + f(imax) defined by the list range, for example 

**Example**:
```fricas
jWSExpr("{i, imin, imax}") . See Wolfram language specifications.
```

- **Signature**: `(%,%)->%`

numericSum(f(n),n, a..b) returns an evaluated numerical approximation of the sum f(a) + ... + f(b).

- **Signature**: `(%,%,Segment(Integer))->%`

### `padeApproximant` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L739)\]

padeApproximant(expr, "x,x0, n,m") returns the Padé approximant at x0.

- **Signature**: `(%,%)->%`

### `parabolicCylinderD` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L297)\]

parabolicCylinderD(nu,x) computes the parabolic cylinder function D of x.

- **Signature**: `(%,%)->%`

### `percentForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1070)\]

percentForm(x) returns the printed representation of x in percent form. For example:

**Example**:
```fricas
percentForm jWS Expr 0.50
```

- **Signature**: `(%)->%`

percentForm(x,n) returns the printed representation of x in percent with n digits of precision.

- **Signature**: `(%,%)->%`

### `pochhammer` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L223)\]

pochhammer(a,n) returns the Pochhammer symbol.

- **Signature**: `(%,%)->%`

### `polylog` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L166)\]

polylog(n,p,x) is the Nielsen generalized polylogarithm function.

- **Signature**: `(%,%,%)->%`

### `polynomial?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L678)\]

polynomial?(p,x) checks whether or not p is a polynomial in x.

- **Signature**: `(%,%)->Boolean`

polynomial?(p,vlist) checks whether or not p is a polynomial in the list of variables vlist.

- **Signature**: `(%,WSList(%))->Boolean`

### `polynomialExpression?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L683)\]

polynomialExpression?(p,x) checks whether or not p is a polynomial expression in x.

- **Signature**: `(%,%)->Boolean`

polynomialExpression?(p,vlist) checks whether or not p is a polynomial expression in the list of variables vlist.

- **Signature**: `(%,WSList(%))->Boolean`

### `polynomialExtendedGCD` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L759)\]

polynomialExtendedGCD(p1, p2, x) returns the greatest common divisor of p1 and p2 considered as univariate polynomials in x

- **Signature**: `(%,%,%)->%`

### `polynomialGCD` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L753)\]

polynomialGCD(p1, p2) returns the greatest common divisor of p1 and p2.

- **Signature**: `(%,%)->%`

polynomialGCD(p1, p2, opt) returns the greatest common divisor of p1 and p2 with options opt, for example Modulus->p.

- **Signature**: `(%,%,%)->%`

### `polynomialLCM` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L762)\]

polynomialLCM(p1,p2) returns the least common divisor of p1 and p2.

- **Signature**: `(%,%)->%`

polynomialLCM(p1,p2,opt) returns the least common divisor of p1 and p2 with options opt, for example an Extension rule.

- **Signature**: `(%,%,%)->%`

### `polynomialMod` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L767)\]

polynomialMod(p,mod) reduces modulo p the integer coefficients of the polynomial p.

- **Signature**: `(%,%)->%`

### `polynomialQuotient` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L731)\]

polynomialQuotient(p1, p2, x) returns the quotient of p1 and p2 in x.

- **Signature**: `(%,%,%)->%`

### `polynomialQuotientRemainder` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L735)\]

polynomialQuotientRemainder(p1,p2,var) returns the quotient and remainder of p1 and p2 in x.

- **Signature**: `(%,%,%)->WSList(%)`

### `polynomialReduce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L785)\]

polynomialReduce(poly,lpoly,lvar) returns a minimal representation of the polynomial poly in terms of the polynomial list lpoly with respect to the list of variables lvar.

- **Signature**: `(%,%,%)->%`

### `polynomialRemainder` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L733)\]

polynomialRemainder(p1,p2, x) returns the remainder of p1 and p2 in x.

- **Signature**: `(%,%,%)->%`

### `positive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L55)\]

positive?(expr) checks whether or not expr is positive.

- **Signature**: `(%)->Boolean`

### `positiveInfinity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L86)\]

positiveInfinity() returns positive infinity (∞).

- **Signature**: `()->%`

### `powerExpand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L937)\]

powerExpand(expr) expands powers in expr assuming no branch cut.

- **Signature**: `(%)->%`

powerExpand(expr, sym) expands powers in expr with respect to sym, assuming no branch cut.

- **Signature**: `(%,%)->%`

### `product` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L422)\]

product(f(n),range) returns the product f(imin) * ... * f(imax) defined by the list range, for exampleExampl e: jWSExpr("{i, imin, imax}") . See Wolfram language specifications.

- **Signature**: `(%,%)->%`

product(f(n),n, a..b) returns the product f(a) * ... * f(b).

- **Signature**: `(%,%,Segment(Integer))->%`

product(f(n),n) returns the indefinite product of f(n).

- **Signature**: `(%,Symbol)->%`

### `qBinomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L313)\]

qBinomial(n,m,q) returns the q-analog of binomial coefficient.

- **Signature**: `(%,%,%)->%`

### `qFactorial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L311)\]

qFactorial(x,q) returns the q-analog of factorial of x.

- **Signature**: `(%,%)->%`

### `qGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L315)\]

qGamma(x,q) returns the q-analog of Euler Gamma of x.

- **Signature**: `(%,%)->%`

### `qPochhammer` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L307)\]

qPochhammer(x,q) returns the q-Pochhammer symbol of x.

- **Signature**: `(%,%)->%`

qPochhammer(x,q,n) returns the q-Pochhammer symbol of x.

- **Signature**: `(%,%,%)->%`

### `qPolyGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L317)\]

qPolyGamma(x,q) returns the q-Digamma of x.

- **Signature**: `(%,%)->%`

qPolyGamma(n,x,q) returns the n-th derivative of the q-digamma function of x.

- **Signature**: `(%,%,%)->%`

### `quantityForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1090)\]

quantityForm(expr,form) returns expr as a quantity with format form.

- **Signature**: `(%,%)->%`

quantityForm(expr,lform) returns expr as a quantity with a list of formats lform.

- **Signature**: `(%,WSList(%))->%`

### `quantityMagnitude` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1030)\]

quantityMagnitude(val) returns magnitude of val.

- **Signature**: `(%)->%`

### `quantityUnit` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1028)\]

quantityUnit(val) returns unit of val.

- **Signature**: `(%)->%`

### `ramanujanTau` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L181)\]

ramanujanTau(n) returns the Ramanujan tau of n.

- **Signature**: `(%)->%`

### `ramanujanTauL` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L183)\]

ramanujanTauL(s) computes the Ramanujan tau Dirichlet L-function of s.

- **Signature**: `(%)->%`

### `ramanujanTauTheta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L185)\]

ramanujanTauTheta(z) returns the Ramanujan tau theta of z.

- **Signature**: `(%)->%`

### `ramanujanTauZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L187)\]

ramanujanTauZ(t) computes the Ramanujan tau Z-function of t.

- **Signature**: `(%)->%`

### `rational?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L44)\]

rational?(q) checks whether or not q is a rational number.

- **Signature**: `(%)->Boolean`

### `rationalApproximation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L986)\]

rationalApproximation(expr) tries to find a rational approximation of the expression expr.

- **Signature**: `(%)->%`

rationalApproximation(expr, dx) tries to find a rational approximation of the expression expr within tolerance dx.

- **Signature**: `(%,%)->%`

### `rationalExpression?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L689)\]

rationalExpression?(p,x) checks whether or not p is a rational expression in x.

- **Signature**: `(%,%)->Boolean`

rationalExpression?(p,vlist) checks whether or not p is a rational expression in the list of variables vlist.

- **Signature**: `(%,WSList(%))->Boolean`

### `real?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L37)\]

real?(x) checks whether or not x represents a real number.

- **Signature**: `(%)->Boolean`

### `realNumeric?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L39)\]

realNumeric?(x) checks whether or not x represents a real value (numeric).

- **Signature**: `(%)->Boolean`

### `realNumericSolve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L544)\]

realNumericSolve(expr, vars) returns the real solution(s) to the expression expr.

- **Signature**: `(%,%)->%`

realNumericSolve(eq, vars) returns the real solution(s) to the equation eq.

- **Signature**: `(Equation(%),%)->%`

### `reduce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L644)\]

reduce(expr,lvars) tries to reduce the (in)equation in expr.

- **Signature**: `(%,%)->%`

reduce(expr,lvars,dom) tries to reduce the (in)equation in expr.

- **Signature**: `(%,%,%)->%`

reduce(expr,lvars) tries to reduce the equation in expr.

- **Signature**: `(Equation(%),%)->%`

reduce(expr,lvars,dom) tries to reduce the equation in expr.

- **Signature**: `(Equation(%),%,%)->%`

### `refine` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L887)\]

refine(expr, assums) refines the expression expr with assumptions assums.

- **Signature**: `(%,%)->%`

### `replace` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1096)\]

replace(expr, rule) applies rule(s) to expr.

- **Signature**: `(%,%)->%`

replace(expr, rule, lev) applies rule to expr with level lev.

- **Signature**: `(%,%,%)->%`

### `replaceAll` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1104)\]

replaceAll(expr, rule) applies rule(s) to expr.

- **Signature**: `(%,%)->%`

### `replaceAt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1102)\]

replaceAt(expr, part, n) replaces the n-th element of expr using rule(s).

- **Signature**: `(%,%,%)->%`

### `replacePart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1100)\]

replacePart(expr, part) replaces expr using rule(s) expressing position(s).

- **Signature**: `(%,%)->%`

### `replaceRepeated` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1106)\]

replaceRepeated(expr, rule) applies rule(s) to expr, but repeatedly.

- **Signature**: `(%,%)->%`

### `residue` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L639)\]

residue(expr, x,x0) returns the residue of expr at x0.

- **Signature**: `(%,%)->%`

### `residueSum` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L641)\]

residueSum(expr, var) returns the residue of expr. 

**Example**:
```fricas
residueSum(Gamma(x),x)
```

- **Signature**: `(%,%)->%`

### `resultant` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L770)\]

resultant(p1,p2,x) returns the resultant of p1 and p2.

- **Signature**: `(%,%,%)->%`

### `retract` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1120)\]

retract(expr) tries to retract expr to an Expression(Integer). Throws an error otherwise.

- **Signature**: `(%)->Expression(Float)`
- **Signature**: `(%)->Expression(Integer)`

### `retractIfCan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1112)\]

retractIfCan(expr) retracts expr to a DoubleFloat if it can be retracted to a Lisp machine float.

- **Signature**: `(%)->Union(DoubleFloat,"failed")`

retractIfCan(expr) tries to retract expr to an Expression(Float).

- **Signature**: `(%)->Union(Expression(Float),"failed")`

retractIfCan(expr) tries to retract expr to an Expression(Integer).

- **Signature**: `(%)->Union(Expression(Integer),"failed")`

retractIfCan(expr) retracts expr to a JLFloat64 if it can be retracted to a 64 bits machine float.

- **Signature**: `(%)->Union(JLFloat64,"failed")`

### `riemannSiegelTheta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L189)\]

riemannSiegelTheta(t) returns the Riemann-Siegel theta function of t.

- **Signature**: `(%)->%`

### `riemannSiegelZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L191)\]

riemannSiegelZ(t) computes the Riemann-Siegel Z function of t.

- **Signature**: `(%)->%`

### `riemannZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L202)\]

riemannZeta(s,a) is the generalized Riemann zeta function.

- **Signature**: `(%,%)->%`

### `rootReduce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L960)\]

rootReduce(expr) reduces root functions.

- **Signature**: `(%)->%`

### `round` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L74)\]

round(x) returns the integer closest to x.

- **Signature**: `(%)->%`

### `scientificForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1058)\]

scientificForm(x) returns the printed representation of x in scientific form.

- **Signature**: `(%)->%`

scientificForm(x,n) returns the printed representation of x in scientific form with n digits of precision.

- **Signature**: `(%,%)->%`

### `select` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1108)\]

select(expr, form) selects elements of expr if form(element) is true.

- **Signature**: `(%,%)->%`

select(expr, form, n) selects the n first elements of expr if form(element) is true.

- **Signature**: `(%,%,%)->%`

### `series` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L407)\]

series(expr, opt) returns a series from expr. 

**Example**:
```fricas
x:=jWSExpr(x);a:=jWSExpr(a);
```

**Example**:
```fricas
opt:=jWSList [ x,pi()$WSEXPR/4,7]
```

**Example**:
```fricas
series(sin(a*x),opt)
```

**Example**:
```fricas
series(cos(x),"{x, 0, 12}")
```

**Example**:
```fricas
series(in verseErfc(x),"{x,0,3}")
```

- **Signature**: `(%,%)->%`

### `siegelTheta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L353)\]

siegelTheta(tau, s) computes the Siegel theta function.

- **Signature**: `(%,%)->%`

siegelTheta(nu, tau, s) computes the Siegel theta function.

- **Signature**: `(%,%,%)->%`

### `simplify` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L868)\]

simplify(expr) simplifies the expr. 

**Example**:
```fricas
x:=jWSExpr("x"); simplify(sqrt(x^2)^2)
```

- **Signature**: `(%)->%`

simplify(expr, assumptions) simplifies the expression expr assuming that assumptions are satisfied. For example: 

**Example**:
```fricas
x:=jWSExpr("x"); simplify(sqrt(x^2), "x>0")
```

- **Signature**: `(%,%)->%`

### `sinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L92)\]

sinc(x) computes the unnormalized sinc of x, sin(x)/x and 0 if x = 0.

- **Signature**: `(%)->%`

### `solve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L532)\]

solve(expr, vars) tries to solve the expression expr.

- **Signature**: `(%,%)->WSList(WSList(%))`

solve(expr, vars, dom) tries to solve the expression expr.

- **Signature**: `(%,%,%)->WSList(WSList(%))`
- **Signature**: `(Equation(%),%,%)->WSList(WSList(%))`

solve(eq, vars) tries to solve the equation eq.

- **Signature**: `(Equation(%),%)->WSList(WSList(%))`

### `sphericalBesselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L134)\]

sphericalBesselJ(n,z) returns the spherical Bessel of the first kind of z.

- **Signature**: `(%,%)->%`

### `sphericalBesselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L137)\]

sphericalBesselY(n,z) returns the spherical Bessel of the second kind of z.

- **Signature**: `(%,%)->%`

### `sphericalHankelH1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L140)\]

sphericalHankelH1(n,z) returns the spherical Hankel function of the first kind of z.

- **Signature**: `(%,%)->%`

### `sphericalHankelH2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L143)\]

sphericalHankelH2(n,z) returns the spherical Hankel function of the second kind of z.

- **Signature**: `(%,%)->%`

### `sphericalHarmonicY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L249)\]

sphericalHarmonicY(l, m, theta, phi) returns the spherical harmonic Y function or evaluates it.

- **Signature**: `(%,%,%,%)->%`

### `stieltjesGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L193)\]

stieltjesGamma(n) returns the n-th Stieltjes constant.

- **Signature**: `(%)->%`

stieltjesGamma(n,a) returns the generalized n-th Stieltjes constant.

- **Signature**: `(%,%)->%`

### `subResultants` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L772)\]

subResultants(p1,p2,x) returns the subresultant of p1 and p2 with respect to x.

- **Signature**: `(%,%,%)->%`

### `sum` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L414)\]

sum(f(n),range) returns the sum f(imin) + ... + f(imax) defined by the list range, for example

**Example**:
```fricas
jWSEx pr("{i, imin, imax}") . See Wolfram Language specifications.
```

- **Signature**: `(%,%)->%`

sum(f(n),n, a..b) returns the sum f(a) + ... + f(b).

- **Signature**: `(%,%,Segment(Integer))->%`

sum(f(n),n) returns the indefinite sum of f(n).

- **Signature**: `(%,Symbol)->%`

### `symmetricPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L742)\]

symmetricPolynomial(n,lvars) returns the n-th elementary symmetric polynomial with respect to variables in lvars.

- **Signature**: `(%,WSList(%))->%`

### `symmetricReduction` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L745)\]

symmetricReduction(f,lvars) returns a pair of polynomials representing f = p+q where p is a symmetric polynomial, q the remainder.

- **Signature**: `(%,WSList(%))->WSList(%)`

symmetricReduction(f, lvars, replnt) returns a pair of polynomials representing f = p+q where p is a symmetric polynomial, q the remainder where variables in p replaced by the ones in replnt.

- **Signature**: `(%,WSList(%),WSList(%))->WSList(%)`

### `toExpression` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L978)\]

toExpression(expr) converts expr to a WS expression and evaluates it.

- **Signature**: `(String)->%`

toExpression(expr, form) converts expr to a WS expression and evaluates it with output in the format form.

- **Signature**: `(String,%)->%`

toExpression(expr, form, h) converts expr to a WS expression and evaluates it with output in the format form but wrapping the head with h. `Hold` for example.

- **Signature**: `(String,%,%)->%`

### `toString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L975)\]

toString(expr, form) returns the string representation of expr with WS language format form.

- **Signature**: `(%,%)->String`

### `together` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L925)\]

together(expr) puts together terms over a common denominator cancelling common factors.

- **Signature**: `(%)->%`

### `traditionalForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1087)\]

traditionalForm(expr) returns a traditional form of expr i.e. its WS 'TraditionalForm'.

- **Signature**: `(%)->%`

### `trigExpand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L947)\]

trigExpand(expr) tries to expand (hyperbolic) trigonometric functions in expr.

- **Signature**: `(%)->%`

### `trigFactor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L963)\]

trigFactor(expr) factors (hyperbolic) trigonometric functions in expr.

- **Signature**: `(%)->%`

### `trigFactorList` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L966)\]

trigFactorList(expr) returns a list of factors of (hyperbolic) trigonometric functions in expr.

- **Signature**: `(%)->WSList(%)`

### `trigReduce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L958)\]

trigReduce(expr) reduces power and products of trigonometric functions.

- **Signature**: `(%)->%`

### `trigToExp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L969)\]

trigToExp(expr) returns expr with (hyperbolic) trigonometric functions converted to, eventually complex, exponentials.

- **Signature**: `(%)->%`

### `values` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L1021)\]

values(expr) returns the values elements in expr.

- **Signature**: `(%)->%`

### `variables` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L695)\]

variables(p) returns the list of variables in p.

- **Signature**: `(%)->WSList(%)`

### `weberE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L156)\]

weberE(v,n,z) is the associated Weber E function.

- **Signature**: `(%,%,%)->%`

### `weierstrassP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L168)\]

weierstrassP(g2, g3, z) is the Weierstrass P function.

- **Signature**: `(%,%,%)->%`

### `weierstrassPInverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L177)\]

weierstrassPInverse(g2, g3, z) is the inverse of Weierstrass P function, defined by the formula WeierstrassP(g2, g3, WeierstrassPInverse(g2, g3, z)) = z.

- **Signature**: `(%,%,%)->%`

### `weierstrassPPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L170)\]

weierstrassPPrime(g2, g3, z) is the derivative of the Weierstrass P function.

- **Signature**: `(%,%,%)->%`

### `weierstrassSigma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L173)\]

weierstrassSigma(g2, g3, z) is the Weierstrass sigma function.

- **Signature**: `(%,%,%)->%`

### `weierstrassZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L175)\]

weierstrassZeta(g2, g3, z) is the Weierstrass Zeta function.

- **Signature**: `(%,%,%)->%`

### `whittakerM` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L293)\]

whittakerM(k,m,x) computes the Whittaker function M of x.

- **Signature**: `(%,%,%)->%`

### `whittakerW` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L295)\]

whittakerW(k,m,z) computes the Whittaker function W of z.

- **Signature**: `(%,%,%)->%`

### `zernikeR` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L275)\]

zernikeR(n, m, x) returns the Zernike radial polynomial or evaluates it at x if x is a number.

- **Signature**: `(%,%,%)->%`

### `zero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsexpr.spad#L31)\]

zero? x tries to determine if x is 0. For example: 

**Example**:
```fricas
expr:=0$WSEXPR/1*sqrt(17::WSEXPR); zero? expr
```

- **Signature**: `(%)->Boolean`
---
[Back to Index](../index.md)
