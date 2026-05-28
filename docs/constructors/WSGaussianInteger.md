# WSGaussianInteger

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1092)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic gaussian integers using the MathLink Julia package.

**WSGaussianInteger is a domain constructor.**  
**Abbreviation for WSGaussianInteger is WSGINT**  
**This constructor is exposed in this frame.**  
**108 names for 158 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, WSInteger) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (WSInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 -? : % -> %                                            ?-? : (%, %) -> %
 ?=? : (%, %) -> Boolean                                D : % -> %
 D : (%, (WSInteger -> WSInteger)) -> %                 D : (%, NonNegativeInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 basis : () -> Vector(%)                                characteristic : () -> NonNegativeInteger
 coerce : % -> %                                        coerce : Integer -> %
 coerce : WSInteger -> %                                coerce : % -> JLObject
 coerce : % -> OutputForm                               coerce : % -> WSExpression
 commutator : (%, %) -> %                               complex : (WSInteger, WSInteger) -> %
 conjugate : % -> %                                     convert : Vector(WSInteger) -> %
 convert : % -> Complex(DoubleFloat)                    convert : % -> Complex(Float)
 convert : % -> InputForm                               convert : % -> Pattern(Integer)
 convert : % -> String                                  convert : % -> Vector(WSInteger)
 coordinates : Vector(%) -> Matrix(WSInteger)           coordinates : % -> Vector(WSInteger)
 coordinates : (%, Vector(%)) -> Vector(WSInteger)      differentiate : % -> %
 differentiate : (%, NonNegativeInteger) -> %           discriminant : () -> WSInteger
 discriminant : Vector(%) -> WSInteger                  euclideanSize : % -> NonNegativeInteger
 ?exquo? : (%, %) -> Union(%,"failed")                  ?exquo? : (%, WSInteger) -> Union(%,"failed")
 factor : % -> Factored(%)                              gcd : (%, %) -> %
 gcd : List(%) -> %                                     generator : () -> %
 imag : % -> WSInteger                                  imaginary : () -> %
 jWSGI : (WSInteger, WSInteger) -> %                    jWSInterpret : String -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : (String, String, String) -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %, %, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlEval : % -> %                                        jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlHead : % -> WSSymbol
 jlId : % -> JLInt64                                    jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlSymbolic : % -> String                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     lift : % -> SparseUnivariatePolynomial(WSInteger)
 map : ((WSInteger -> WSInteger), %) -> %               missing? : % -> Boolean
 mutable? : % -> Boolean                                norm : % -> WSInteger
 nothing? : % -> Boolean                                numeric : % -> WSExpression
 numeric : (%, PositiveInteger) -> WSExpression         numeric? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    random : WSInteger -> %
 rank : () -> PositiveInteger                           rational : % -> Fraction(Integer)
 rational? : % -> Boolean                               real : % -> WSInteger
 recip : % -> Union(%,"failed")                         reducedSystem : Matrix(%) -> Matrix(WSInteger)
 regularRepresentation : % -> Matrix(WSInteger)         ?rem? : (%, %) -> %
 represents : Vector(WSInteger) -> %                    represents : (Vector(WSInteger), Vector(%)) -> %
 retract : % -> Integer                                 retract : % -> WSInteger
 retractIfCan : % -> Union(Integer,"failed")            retractIfCan : % -> Union(WSInteger,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 toString : % -> String                                 trace : % -> WSInteger
 traceMatrix : () -> Matrix(WSInteger)                  traceMatrix : Vector(%) -> Matrix(WSInteger)
 unit? : % -> Boolean                                   unitCanonical : % -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 D : (%, (WSInteger -> WSInteger), NonNegativeInteger) -> %
 characteristicPolynomial : % -> SparseUnivariatePolynomial(WSInteger)
 convert : SparseUnivariatePolynomial(WSInteger) -> %
 convert : % -> SparseUnivariatePolynomial(WSInteger)
 coordinates : (Vector(%), Vector(%)) -> Matrix(WSInteger)
 definingPolynomial : () -> SparseUnivariatePolynomial(WSInteger)
 differentiate : (%, (WSInteger -> WSInteger)) -> %
 differentiate : (%, (WSInteger -> WSInteger), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 rationalIfCan : % -> Union(Fraction(Integer),"failed")
 reduce : SparseUnivariatePolynomial(WSInteger) -> %
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(WSInteger),vec: Vector(WSInteger))
 regularRepresentation : (%, Vector(%)) -> Matrix(WSInteger)
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1107)\]

coerce(x): convenience function.

- **Signature**: `(Integer)->%`

### `jWSGI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1109)\]

jWSGI(re, im) constructs a WSGaussianInteger from real part re and imaginary part im.

- **Signature**: `(WSInteger,WSInteger)->%`

### `random` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1104)\]

random(n) returns a gaussian integer where real part is in the range 0..n as the imaginary part.

- **Signature**: `(WSInteger)->%`
---
[Back to Index](../index.md)
