# WSComplex

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1193)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic complex numbers using the MathLink Julia package.

**WSComplex is a domain constructor.**  
**Abbreviation for WSComplex is WSCPLX**  
**This constructor is not exposed in this frame.**  
**158 names for 227 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, WSReal) -> %                                 ?*? : (Fraction(Integer), %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (WSInteger, %) -> %
 ?*? : (WSReal, %) -> %                                 ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?=? : (%, %) -> Boolean                                Chi : % -> %
 Ci : % -> %                                            D : % -> %
 D : (%, (WSReal -> WSReal)) -> %                       D : (%, NonNegativeInteger) -> %
 Ei : % -> %                                            Shi : % -> %
 Si : % -> %                                            ?^? : (%, %) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 abs : % -> %                                           acos : % -> %
 acosh : % -> %                                         acot : % -> %
 acoth : % -> %                                         acsc : % -> %
 acsch : % -> %                                         annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           argument : % -> WSReal
 asec : % -> %                                          asech : % -> %
 asin : % -> %                                          asinh : % -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 atan : % -> %                                          atan : (%, %) -> %
 atanh : % -> %                                         basis : () -> Vector(%)
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Complex(Integer) -> %                         coerce : Fraction(Integer) -> %
 coerce : Integer -> %                                  coerce : WSInteger -> %
 coerce : WSReal -> %                                   coerce : % -> Complex(DoubleFloat)
 coerce : % -> Complex(JLFloat64)                       coerce : % -> JLObject
 coerce : % -> OutputForm                               coerce : % -> WSExpression
 commutator : (%, %) -> %                               complex : (WSReal, WSReal) -> %
 conjugate : % -> %                                     convert : SparseUnivariatePolynomial(WSReal) -> %
 convert : Vector(WSReal) -> %                          convert : % -> Complex(DoubleFloat)
 convert : % -> Complex(Float)                          convert : % -> Pattern(Float)
 convert : % -> SparseUnivariatePolynomial(WSReal)      convert : % -> String
 convert : % -> Vector(WSReal)                          coordinates : Vector(%) -> Matrix(WSReal)
 coordinates : % -> Vector(WSReal)                      coordinates : (%, Vector(%)) -> Vector(WSReal)
 cos : % -> %                                           cosh : % -> %
 cot : % -> %                                           coth : % -> %
 csc : % -> %                                           csch : % -> %
 differentiate : % -> %                                 differentiate : (%, (WSReal -> WSReal)) -> %
 differentiate : (%, NonNegativeInteger) -> %           dilog : % -> %
 discriminant : () -> WSReal                            discriminant : Vector(%) -> WSReal
 erf : % -> %                                           erf : (%, %) -> %
 erfc : % -> %                                          erfi : % -> %
 euclideanSize : % -> NonNegativeInteger                exp : () -> %
 exp : % -> %                                           ?exquo? : (%, %) -> Union(%,"failed")
 ?exquo? : (%, WSReal) -> Union(%,"failed")             factor : % -> Factored(%)
 fresnelC : % -> %                                      fresnelS : % -> %
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 generator : () -> %                                    imag : % -> WSReal
 imaginary : () -> %                                    integral : (%, SegmentBinding(%)) -> %
 integral : (%, Symbol) -> %                            inv : % -> %
 jWSComplex : WSReal -> %                               jWSComplex : (WSReal, WSReal) -> %
 jWSInterpret : String -> %                             jWSInterpret : (String, String) -> %
 jWSInterpret : (String, String, String) -> %           jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApprox? : (%, %) -> Boolean
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
 leftRecip : % -> Union(%,"failed")                     li : % -> %
 lift : % -> SparseUnivariatePolynomial(WSReal)         log : % -> %
 log10 : % -> %                                         log2 : % -> %
 map : ((WSReal -> WSReal), %) -> %                     missing? : % -> Boolean
 mutable? : % -> Boolean                                norm : % -> WSReal
 nothing? : % -> Boolean                                nthRoot : (%, Integer) -> %
 numeric : % -> WSExpression                            numeric : (%, PositiveInteger) -> WSExpression
 numeric? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          pi : () -> %
 plenaryPower : (%, PositiveInteger) -> %               prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    rank : () -> PositiveInteger
 real : % -> WSReal                                     recip : % -> Union(%,"failed")
 reduce : SparseUnivariatePolynomial(WSReal) -> %       reducedSystem : Matrix(%) -> Matrix(WSReal)
 regularRepresentation : % -> Matrix(WSReal)            ?rem? : (%, %) -> %
 represents : Vector(WSReal) -> %                       represents : (Vector(WSReal), Vector(%)) -> %
 retract : % -> Fraction(Integer)                       retract : % -> Integer
 retract : % -> WSReal                                  retractIfCan : % -> Union(Integer,"failed")
 retractIfCan : % -> Union(WSReal,"failed")             rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          sin : % -> %
 sinc : % -> %                                          sinh : % -> %
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tan : % -> %
 tanh : % -> %                                          toString : % -> String
 toString : (%, WSExpression) -> String                 trace : % -> WSReal
 traceMatrix : () -> Matrix(WSReal)                     traceMatrix : Vector(%) -> Matrix(WSReal)
 unit? : % -> Boolean                                   unitCanonical : % -> %
 urand01 : () -> %                                      zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 D : (%, (WSReal -> WSReal), NonNegativeInteger) -> %
 characteristicPolynomial : % -> SparseUnivariatePolynomial(WSReal)
 coordinates : (Vector(%), Vector(%)) -> Matrix(WSReal)
 definingPolynomial : () -> SparseUnivariatePolynomial(WSReal)
 derivationCoordinates : (Vector(%), (WSReal -> WSReal)) -> Matrix(WSReal)
 differentiate : (%, (WSReal -> WSReal), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 minimalPolynomial : % -> SparseUnivariatePolynomial(WSReal)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 polarCoordinates : % -> Record(r: WSReal,phi: WSReal)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reduce : Fraction(SparseUnivariatePolynomial(WSReal)) -> Union(%,"failed")
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(WSReal),vec: Vector(WSReal))
 regularRepresentation : (%, Vector(%)) -> Matrix(WSReal)
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1221)\]

atan(z1,z2) computes the arc tangent of z2/z1.

- **Signature**: `(%,%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1230)\]

coerce(z) coerce z. Convenience function. -- %i operations for example

- **Signature**: `(Complex(Integer))->%`

coerce(int): coerces int. Convenience function.

- **Signature**: `(Integer)->%`
- **Signature**: `(WSInteger)->%`

coerce(z) coerces z to a FriCAS Complex(DoubleFloat).

- **Signature**: `(%)->Complex(DoubleFloat)`

coerce(z) coerces z to a FriCAS Complex(JLFloat64).

- **Signature**: `(%)->Complex(JLFloat64)`

### `complex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1240)\]

complex(re,im) constructs a WSComplex from real part re and imaginary part im.

- **Signature**: `(WSReal,WSReal)->%`

### `erf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1223)\]

erf(x) is the error function.

- **Signature**: `(%,%)->%`

### `erfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1225)\]

erfc(x) is the complementary error function.

- **Signature**: `(%)->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1213)\]

exp() returns the WSComplex ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `jWSComplex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1243)\]

jWSComplex(re) constructs a WSComplex with real part re.

- **Signature**: `(WSReal)->%`

jWSComplex(re, im) constructs a WSComplex from real part re and imaginary part im.

- **Signature**: `(WSReal,WSReal)->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1210)\]

jlApprox?(x,y) computes inexact equality comparison with WS default parameters (Equal).

- **Signature**: `(%,%)->Boolean`

### `log10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1219)\]

log10(z) computes the logarithm of z in base 10.

- **Signature**: `(%)->%`

### `log2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1217)\]

log2(z) computes the logarithm of z in base 2.

- **Signature**: `(%)->%`

### `sinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1215)\]

sinc(z) computes the unnormalized sinc of z, sin(z)/z and 0 if z = 0.

- **Signature**: `(%)->%`

### `toString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1227)\]

toString(expr, form) returns the string representation of expr with WS language format form.

- **Signature**: `(%,WSExpression)->String`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1206)\]

urand01() returns a unit square random complex number.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
