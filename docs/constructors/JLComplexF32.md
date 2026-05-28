# JLComplexF32

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L388)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

JLComplexF32 implements complex 32 bits floating point arithmetic. Only basic arithmetic is supported. Convenience domain.

**JLComplexF32 is a domain constructor.**  
**Abbreviation for JLComplexF32 is JCF32**  
**This constructor is not exposed in this frame.**  
**115 names for 164 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, JLFloat32) -> %                              ?*? : (Fraction(Integer), %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (JLFloat32, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (PositiveInteger, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?=? : (%, %) -> Boolean                                D : % -> %
 D : (%, (JLFloat32 -> JLFloat32)) -> %                 D : (%, NonNegativeInteger) -> %
 ?^? : (%, %) -> %                                      ?^? : (%, Fraction(Integer)) -> %
 ?^? : (%, Integer) -> %                                ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        abs : % -> %
 acos : % -> %                                          acosh : % -> %
 acot : % -> %                                          acoth : % -> %
 acsc : % -> %                                          acsch : % -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 argument : % -> JLFloat32                              asec : % -> %
 asech : % -> %                                         asin : % -> %
 asinh : % -> %                                         associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            atan : % -> %
 atanh : % -> %                                         basis : () -> Vector(%)
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Fraction(Integer) -> %                        coerce : Integer -> %
 coerce : JLFloat32 -> %                                coerce : % -> OutputForm
 commutator : (%, %) -> %                               complex : (JLFloat32, JLFloat32) -> %
 conjugate : % -> %                                     convert : Vector(JLFloat32) -> %
 convert : % -> Complex(DoubleFloat)                    convert : % -> Complex(Float)
 convert : % -> Pattern(Float)                          convert : % -> String
 convert : % -> Vector(JLFloat32)                       coordinates : Vector(%) -> Matrix(JLFloat32)
 coordinates : % -> Vector(JLFloat32)                   coordinates : (%, Vector(%)) -> Vector(JLFloat32)
 cos : % -> %                                           cosh : % -> %
 cot : % -> %                                           coth : % -> %
 csc : % -> %                                           csch : % -> %
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 discriminant : () -> JLFloat32                         discriminant : Vector(%) -> JLFloat32
 euclideanSize : % -> NonNegativeInteger                exp : % -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  ?exquo? : (%, JLFloat32) -> Union(%,"failed")
 factor : % -> Factored(%)                              gcd : (%, %) -> %
 gcd : List(%) -> %                                     generator : () -> %
 imag : % -> JLFloat32                                  imaginary : () -> %
 inv : % -> %                                           jcf32 : JLFloat32 -> %
 jcf32 : (JLFloat32, JLFloat32) -> %                    jlApprox? : (%, %) -> Boolean
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 lift : % -> SparseUnivariatePolynomial(JLFloat32)      log : % -> %
 map : ((JLFloat32 -> JLFloat32), %) -> %               norm : % -> JLFloat32
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          pi : () -> %
 plenaryPower : (%, PositiveInteger) -> %               prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    rank : () -> PositiveInteger
 real : % -> JLFloat32                                  recip : % -> Union(%,"failed")
 reducedSystem : Matrix(%) -> Matrix(JLFloat32)         regularRepresentation : % -> Matrix(JLFloat32)
 ?rem? : (%, %) -> %                                    represents : Vector(JLFloat32) -> %
 represents : (Vector(JLFloat32), Vector(%)) -> %       retract : % -> Fraction(Integer)
 retract : % -> Integer                                 retract : % -> JLFloat32
 retractIfCan : % -> Union(Integer,"failed")            retractIfCan : % -> Union(JLFloat32,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sec : % -> %                                           sech : % -> %
 sin : % -> %                                           sinh : % -> %
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tan : % -> %
 tanh : % -> %                                          trace : % -> JLFloat32
 traceMatrix : () -> Matrix(JLFloat32)                  traceMatrix : Vector(%) -> Matrix(JLFloat32)
 unit? : % -> Boolean                                   unitCanonical : % -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 D : (%, (JLFloat32 -> JLFloat32), NonNegativeInteger) -> %
 characteristicPolynomial : % -> SparseUnivariatePolynomial(JLFloat32)
 convert : SparseUnivariatePolynomial(JLFloat32) -> %
 convert : % -> SparseUnivariatePolynomial(JLFloat32)
 coordinates : (Vector(%), Vector(%)) -> Matrix(JLFloat32)
 definingPolynomial : () -> SparseUnivariatePolynomial(JLFloat32)
 derivationCoordinates : (Vector(%), (JLFloat32 -> JLFloat32)) -> Matrix(JLFloat32)
 differentiate : (%, (JLFloat32 -> JLFloat32)) -> %
 differentiate : (%, (JLFloat32 -> JLFloat32), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 minimalPolynomial : % -> SparseUnivariatePolynomial(JLFloat32)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 polarCoordinates : % -> Record(r: JLFloat32,phi: JLFloat32)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reduce : SparseUnivariatePolynomial(JLFloat32) -> %
 reduce : Fraction(SparseUnivariatePolynomial(JLFloat32)) -> Union(%,"failed")
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(JLFloat32),vec: Vector(JLFloat32))
 regularRepresentation : (%, Vector(%)) -> Matrix(JLFloat32)
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `jcf32` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L403)\]

jcf32(r) coerces r as a JLComplexF32 with the real part r.

- **Signature**: `(JLFloat32)->%`

jcf32(r,i) returns a JLComplexF32 with real part r and imaginary part i.

- **Signature**: `(JLFloat32,JLFloat32)->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L398)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`
---
[Back to Index](../index.md)
