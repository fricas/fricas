# JLComplexFloat

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1099)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

JLComplexFloat implements arbitrary precision floating point arithmetic for complex numbers using Julia BigFloats (MPFR based). https://www.mpfr.org/

**JLComplexFloat is a domain constructor.**  
**Abbreviation for JLComplexFloat is JCFLOAT**  
**This constructor is not exposed in this frame.**  
**149 names for 222 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, Integer) -> %                                ?*? : (%, JLFloat) -> %
 ?*? : (Fraction(Integer), %) -> %                      ?*? : (Integer, %) -> %
 ?*? : (JLFloat, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?/? : (%, Integer) -> %                                ?/? : (Integer, %) -> %
 ?=? : (%, %) -> Boolean                                D : % -> %
 D : (%, (JLFloat -> JLFloat)) -> %                     D : (%, NonNegativeInteger) -> %
 ?^? : (%, %) -> %                                      ?^? : (%, Fraction(Integer)) -> %
 ?^? : (%, Integer) -> %                                ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        abs : % -> %
 acos : % -> %                                          acosh : % -> %
 acot : % -> %                                          acoth : % -> %
 acsc : % -> %                                          acsch : % -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 argument : % -> JLFloat                                asec : % -> %
 asech : % -> %                                         asin : % -> %
 asinh : % -> %                                         associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            atan : % -> %
 atan : (%, %) -> %                                     atanh : % -> %
 basis : () -> Vector(%)                                characteristic : () -> NonNegativeInteger
 cis : % -> %                                           cispi : % -> %
 coerce : % -> %                                        coerce : Float -> %
 coerce : Fraction(Integer) -> %                        coerce : Integer -> %
 coerce : JLFloat -> %                                  coerce : JLFloat64 -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               complex : (JLFloat, JLFloat) -> %
 conjugate : % -> %                                     convert : SparseUnivariatePolynomial(JLFloat) -> %
 convert : Vector(JLFloat) -> %                         convert : % -> Complex(DoubleFloat)
 convert : % -> Complex(Float)                          convert : % -> Pattern(Float)
 convert : % -> SparseUnivariatePolynomial(JLFloat)     convert : % -> String
 convert : % -> Vector(JLFloat)                         coordinates : Vector(%) -> Matrix(JLFloat)
 coordinates : % -> Vector(JLFloat)                     coordinates : (%, Vector(%)) -> Vector(JLFloat)
 cos : % -> %                                           cosh : % -> %
 cot : % -> %                                           coth : % -> %
 csc : % -> %                                           csch : % -> %
 differentiate : % -> %                                 differentiate : (%, (JLFloat -> JLFloat)) -> %
 differentiate : (%, NonNegativeInteger) -> %           discriminant : () -> JLFloat
 discriminant : Vector(%) -> JLFloat                    euclideanSize : % -> NonNegativeInteger
 exp : () -> %                                          exp : % -> %
 exp1 : () -> %                                         exp10 : % -> %
 exp2 : % -> %                                          expm1 : % -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  ?exquo? : (%, JLFloat) -> Union(%,"failed")
 factor : % -> Factored(%)                              finite? : % -> Boolean
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 generator : () -> %                                    imag : % -> JLFloat
 imaginary : () -> %                                    integer? : % -> Boolean
 inv : % -> %                                           jcfloat : Float -> %
 jcfloat : (Float, Float) -> %                          jcfloat : Fraction(Integer) -> %
 jcfloat : Integer -> %                                 jcfloat : (Integer, Integer) -> %
 jcfloat : JLFloat -> %                                 jcfloat : (JLFloat, JLFloat) -> %
 jcfloat : String -> %                                  jlAbout : % -> Void
 jlApply : (String, %) -> %                             jlApply : (String, %, %) -> %
 jlApply : (String, %, %, %) -> %                       jlApply : (String, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %, %, %) -> JLObject
 jlApprox? : (%, %) -> Boolean                          jlApprox? : (%, %, %) -> Boolean
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlId : % -> JLInt64                                    jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 lift : % -> SparseUnivariatePolynomial(JLFloat)        log : % -> %
 log10 : % -> %                                         log1p : % -> %
 log2 : % -> %                                          map : ((JLFloat -> JLFloat), %) -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 norm : % -> JLFloat                                    nothing? : % -> Boolean
 nrand : () -> %                                        nthRoot : (%, Integer) -> %
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 pi : () -> %                                           plenaryPower : (%, PositiveInteger) -> %
 precision : (%, PositiveInteger) -> %                  precision : % -> PositiveInteger
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 rank : () -> PositiveInteger                           real : % -> JLFloat
 real? : % -> Boolean                                   recip : % -> Union(%,"failed")
 reduce : SparseUnivariatePolynomial(JLFloat) -> %      reducedSystem : Matrix(%) -> Matrix(JLFloat)
 regularRepresentation : % -> Matrix(JLFloat)           ?rem? : (%, %) -> %
 represents : Vector(JLFloat) -> %                      represents : (Vector(JLFloat), Vector(%)) -> %
 retract : % -> Fraction(Integer)                       retract : % -> Integer
 retract : % -> JLFloat                                 retractIfCan : % -> Union(Integer,"failed")
 retractIfCan : % -> Union(JLFloat,"failed")            rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          sin : % -> %
 sinh : % -> %                                          sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 tan : % -> %                                           tanh : % -> %
 trace : % -> JLFloat                                   traceMatrix : () -> Matrix(JLFloat)
 traceMatrix : Vector(%) -> Matrix(JLFloat)             unit? : % -> Boolean
 unitCanonical : % -> %                                 urand01 : () -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 D : (%, (JLFloat -> JLFloat), NonNegativeInteger) -> %
 characteristicPolynomial : % -> SparseUnivariatePolynomial(JLFloat)
 coordinates : (Vector(%), Vector(%)) -> Matrix(JLFloat)
 definingPolynomial : () -> SparseUnivariatePolynomial(JLFloat)
 derivationCoordinates : (Vector(%), (JLFloat -> JLFloat)) -> Matrix(JLFloat)
 differentiate : (%, (JLFloat -> JLFloat), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 minimalPolynomial : % -> SparseUnivariatePolynomial(JLFloat)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 polarCoordinates : % -> Record(r: JLFloat,phi: JLFloat)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reduce : Fraction(SparseUnivariatePolynomial(JLFloat)) -> Union(%,"failed")
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(JLFloat),vec: Vector(JLFloat))
 regularRepresentation : (%, Vector(%)) -> Matrix(JLFloat)
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1168)\]

atan(x) computes the inverse tangent of x.

- **Signature**: `(%)->%`

atan(x, y) computes the inverse tangent of x/y.

- **Signature**: `(%,%)->%`

### `cis` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1159)\]

cis(x) returns exp(%i*x) computed efficiently.

- **Signature**: `(%)->%`

### `cispi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1161)\]

cispi(x) returns cis(%pi*x) computed efficiently.

- **Signature**: `(%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1178)\]

coerce(r) coerces to a JLComplexFloat.

- **Signature**: `(Float)->%`

coerce(r) coerces r to a JLComplexFloat.

- **Signature**: `(JLFloat64)->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1155)\]

exp() returns the JLComplexFloat ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `exp1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1157)\]

exp1() returns the JLComplexFloat ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `exp10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1147)\]

exp10(x) computes the base 10 exponential of x.

- **Signature**: `(%)->%`

### `exp2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1145)\]

exp2(x) computes the base 2 exponential of x.

- **Signature**: `(%)->%`

### `expm1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1142)\]

expm1(x) computes accurately ℯ^x-1. It avoids the loss of precision involved in the direct evaluation of exp(x)-1 for small values of x.

- **Signature**: `(%)->%`

### `finite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1125)\]

finite?(x) tests whether or not x is finite.

- **Signature**: `(%)->Boolean`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1127)\]

integer?(x) tests whether or not x is an integer.

- **Signature**: `(%)->Boolean`

### `jcfloat` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1182)\]

jcfloat(r) returns r as a JLComplexFloat.

- **Signature**: `(Float)->%`
- **Signature**: `(JLFloat)->%`

jcfloat(r,i) returns r+i as a JLComplexFloat.

- **Signature**: `(Float,Float)->%`
- **Signature**: `(Integer,Integer)->%`
- **Signature**: `(JLFloat,JLFloat)->%`

jcfloat(q) returns q as a JLComplexFloat.

- **Signature**: `(Fraction(Integer))->%`

jcfloat(z) returns z as a JLComplexFloat.

- **Signature**: `(Integer)->%`

jcfloat(str) returns str as a JLComplexFloat.

- **Signature**: `(String)->%`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1135)\]

jlApply(func, x) applies the function func with x as parameter.

- **Signature**: `(String,%)->%`

jlApply(func, x, y) applies the function func to x and y.

- **Signature**: `(String,%,%)->%`

jlApply(func, x, y, z) applies the function func to x, y and z.

- **Signature**: `(String,%,%,%)->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1115)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%)->Boolean`

jlApprox?(x,y,atol) computes inexact equality comparison with absolute tolerance atol. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%,%)->Boolean`

### `log10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1153)\]

log10(x) computes the base 10 logarithm of x.

- **Signature**: `(%)->%`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1149)\]

log1p(x) computes accurately log(1+x)

- **Signature**: `(%)->%`

### `log2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1151)\]

log2(x) computes the base 2 logarithm of x.

- **Signature**: `(%)->%`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1166)\]

nrand() returns an normally distributed random number.

- **Signature**: `()->%`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1131)\]

precision(x, p) returns a copy of x with precision p.

- **Signature**: `(%,PositiveInteger)->%`

precision(x) returns the precision of x.

- **Signature**: `(%)->PositiveInteger`

### `real?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1129)\]

real?(x) tests whether or not x is real.

- **Signature**: `(%)->Boolean`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1163)\]

urand01() returns an uniformly distributed random number contained in the unit disk.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
