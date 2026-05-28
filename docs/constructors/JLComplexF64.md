# JLComplexF64

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L613)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

JLComplexF64 implements complex 64 bits floating point arithmetic. Only basic arithmetic is supported. Convenience domain.

**JLComplexF64 is a domain constructor.**  
**Abbreviation for JLComplexF64 is JCF64**  
**This constructor is exposed in this frame.**  
**115 names for 165 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, JLFloat64) -> %                              ?*? : (Fraction(Integer), %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (JLFloat64, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (PositiveInteger, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?=? : (%, %) -> Boolean                                D : % -> %
 D : (%, (JLFloat64 -> JLFloat64)) -> %                 D : (%, NonNegativeInteger) -> %
 ?^? : (%, %) -> %                                      ?^? : (%, Fraction(Integer)) -> %
 ?^? : (%, Integer) -> %                                ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        abs : % -> %
 acos : % -> %                                          acosh : % -> %
 acot : % -> %                                          acoth : % -> %
 acsc : % -> %                                          acsch : % -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 argument : % -> JLFloat64                              asec : % -> %
 asech : % -> %                                         asin : % -> %
 asinh : % -> %                                         associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            atan : % -> %
 atanh : % -> %                                         basis : () -> Vector(%)
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Fraction(Integer) -> %                        coerce : Integer -> %
 coerce : JLFloat64 -> %                                coerce : % -> OutputForm
 commutator : (%, %) -> %                               complex : (JLFloat64, JLFloat64) -> %
 conjugate : % -> %                                     convert : Vector(JLFloat64) -> %
 convert : % -> Complex(DoubleFloat)                    convert : % -> Complex(Float)
 convert : % -> InputForm                               convert : % -> Pattern(Float)
 convert : % -> String                                  convert : % -> Vector(JLFloat64)
 coordinates : Vector(%) -> Matrix(JLFloat64)           coordinates : % -> Vector(JLFloat64)
 coordinates : (%, Vector(%)) -> Vector(JLFloat64)      cos : % -> %
 cosh : % -> %                                          cot : % -> %
 coth : % -> %                                          csc : % -> %
 csch : % -> %                                          differentiate : % -> %
 differentiate : (%, NonNegativeInteger) -> %           discriminant : () -> JLFloat64
 discriminant : Vector(%) -> JLFloat64                  euclideanSize : % -> NonNegativeInteger
 exp : % -> %                                           ?exquo? : (%, %) -> Union(%,"failed")
 ?exquo? : (%, JLFloat64) -> Union(%,"failed")          factor : % -> Factored(%)
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 generator : () -> %                                    imag : % -> JLFloat64
 imaginary : () -> %                                    inv : % -> %
 jcf64 : JLFloat64 -> %                                 jcf64 : (JLFloat64, JLFloat64) -> %
 jlApprox? : (%, %) -> Boolean                          latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     lift : % -> SparseUnivariatePolynomial(JLFloat64)
 log : % -> %                                           map : ((JLFloat64 -> JLFloat64), %) -> %
 norm : % -> JLFloat64                                  nthRoot : (%, Integer) -> %
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 pi : () -> %                                           plenaryPower : (%, PositiveInteger) -> %
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 rank : () -> PositiveInteger                           real : % -> JLFloat64
 recip : % -> Union(%,"failed")                         reducedSystem : Matrix(%) -> Matrix(JLFloat64)
 regularRepresentation : % -> Matrix(JLFloat64)         ?rem? : (%, %) -> %
 represents : Vector(JLFloat64) -> %                    represents : (Vector(JLFloat64), Vector(%)) -> %
 retract : % -> Fraction(Integer)                       retract : % -> Integer
 retract : % -> JLFloat64                               retractIfCan : % -> Union(Integer,"failed")
 retractIfCan : % -> Union(JLFloat64,"failed")          rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          sin : % -> %
 sinh : % -> %                                          sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 tan : % -> %                                           tanh : % -> %
 trace : % -> JLFloat64                                 traceMatrix : () -> Matrix(JLFloat64)
 traceMatrix : Vector(%) -> Matrix(JLFloat64)           unit? : % -> Boolean
 unitCanonical : % -> %                                 zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 D : (%, (JLFloat64 -> JLFloat64), NonNegativeInteger) -> %
 characteristicPolynomial : % -> SparseUnivariatePolynomial(JLFloat64)
 convert : SparseUnivariatePolynomial(JLFloat64) -> %
 convert : % -> SparseUnivariatePolynomial(JLFloat64)
 coordinates : (Vector(%), Vector(%)) -> Matrix(JLFloat64)
 definingPolynomial : () -> SparseUnivariatePolynomial(JLFloat64)
 derivationCoordinates : (Vector(%), (JLFloat64 -> JLFloat64)) -> Matrix(JLFloat64)
 differentiate : (%, (JLFloat64 -> JLFloat64)) -> %
 differentiate : (%, (JLFloat64 -> JLFloat64), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 minimalPolynomial : % -> SparseUnivariatePolynomial(JLFloat64)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 polarCoordinates : % -> Record(r: JLFloat64,phi: JLFloat64)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reduce : SparseUnivariatePolynomial(JLFloat64) -> %
 reduce : Fraction(SparseUnivariatePolynomial(JLFloat64)) -> Union(%,"failed")
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(JLFloat64),vec: Vector(JLFloat64))
 regularRepresentation : (%, Vector(%)) -> Matrix(JLFloat64)
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `jcf64` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L628)\]

jcf64(r) coerces r as a JLComplexF64 with the real part r.

- **Signature**: `(JLFloat64)->%`

jcf64(r,i) returns a JLComplexF64 with real part r and imaginary part i.

- **Signature**: `(JLFloat64,JLFloat64)->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L623)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`
---
[Back to Index](../index.md)
