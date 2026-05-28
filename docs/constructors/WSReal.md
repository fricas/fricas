# WSReal

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L502)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic real numbers using the MathLink Julia package.

**WSReal is a domain constructor.**  
**Abbreviation for WSReal is WSREAL**  
**This constructor is exposed in this frame.**  
**159 names for 216 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (Fraction(Integer), %) -> %                      ?*? : (Integer, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (PositiveInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 -? : % -> %                                            ?-? : (%, %) -> %
 ?/? : (%, %) -> %                                      ?/? : (%, Integer) -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               Chi : % -> %
 Ci : % -> %                                            D : % -> %
 D : (%, NonNegativeInteger) -> %                       Ei : % -> %
 Shi : % -> %                                           Si : % -> %
 ?^? : (%, %) -> %                                      ?^? : (%, Fraction(Integer)) -> %
 ?^? : (%, Integer) -> %                                ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        abs : % -> %
 acos : % -> %                                          acosh : % -> %
 acot : % -> %                                          acoth : % -> %
 acsc : % -> %                                          acsch : % -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 asec : % -> %                                          asech : % -> %
 asin : % -> %                                          asinh : % -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 atan : % -> %                                          atan : (%, %) -> %
 atanh : % -> %                                         base : () -> PositiveInteger
 bits : () -> PositiveInteger                           ceiling : % -> %
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : DoubleFloat -> %                              coerce : Float -> %
 coerce : Fraction(Integer) -> %                        coerce : Integer -> %
 coerce : JLFloat -> %                                  coerce : JLFloat64 -> %
 coerce : String -> %                                   coerce : % -> DoubleFloat
 coerce : % -> JLFloat                                  coerce : % -> JLFloat64
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 coerce : % -> WSExpression                             commutator : (%, %) -> %
 convert : % -> DoubleFloat                             convert : % -> Float
 convert : % -> Pattern(Float)                          convert : % -> String
 cos : % -> %                                           cosh : % -> %
 cot : % -> %                                           coth : % -> %
 csc : % -> %                                           csch : % -> %
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 digits : () -> PositiveInteger                         dilog : % -> %
 erf : % -> %                                           erf : (%, %) -> %
 erfc : % -> %                                          erfi : % -> %
 euclideanSize : % -> NonNegativeInteger                exp : () -> %
 exp : % -> %                                           exponent : % -> Integer
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 float : (Integer, Integer) -> %                        float : (Integer, Integer, PositiveInteger) -> %
 floor : % -> %                                         fractionPart : % -> %
 fresnelC : % -> %                                      fresnelS : % -> %
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 integerPart : % -> WSInteger                           integral : (%, SegmentBinding(%)) -> %
 integral : (%, Symbol) -> %                            inv : % -> %
 jWSInterpret : String -> %                             jWSInterpret : (String, String) -> %
 jWSInterpret : (String, String, String) -> %           jWSReal : DoubleFloat -> %
 jWSReal : Float -> %                                   jWSReal : Integer -> %
 jWSReal : JLFloat64 -> %                               jWSReal : String -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %, %, %) -> JLObject
 jlApprox? : (%, %) -> Boolean                          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlEval : % -> %
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlHead : % -> WSSymbol                                 jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlSymbolic : % -> String
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 li : % -> %                                            log : % -> %
 log10 : % -> %                                         log2 : % -> %
 mantissa : % -> Integer                                max : (%, %) -> %
 min : (%, %) -> %                                      missing? : % -> Boolean
 mutable? : % -> Boolean                                negative? : % -> Boolean
 norm : % -> %                                          nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            numeric : % -> WSExpression
 numeric : (%, PositiveInteger) -> WSExpression         numeric? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 order : % -> Integer                                   pi : () -> %
 plenaryPower : (%, PositiveInteger) -> %               positive? : % -> Boolean
 precision : () -> PositiveInteger                      prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    rationalApproximation : % -> WSRational
 rationalApproximation : (%, %) -> WSRational           recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    retract : % -> Fraction(Integer)
 retract : % -> Integer                                 retractIfCan : % -> Union(Integer,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    round : % -> %
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          sign : % -> Integer
 sin : % -> %                                           sinc : % -> %
 sinh : % -> %                                          sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 tan : % -> %                                           tanh : % -> %
 toString : % -> String                                 toString : (%, NonNegativeInteger) -> String
 truncate : % -> %                                      unit? : % -> Boolean
 unitCanonical : % -> %                                 urand01 : () -> %
 wholePart : % -> Integer                               zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 max : () -> % if not % has ATARBEX and not % has ATARBPR
 min : () -> % if not % has ATARBEX and not % has ATARBPR
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L533)\]

atan(x,y) computes the arc tangent of y/x.

- **Signature**: `(%,%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L545)\]

coerce(x) converts x to a WSReal.

- **Signature**: `(DoubleFloat)->%`
- **Signature**: `(Float)->%`
- **Signature**: `(JLFloat)->%`
- **Signature**: `(JLFloat64)->%`

coerce(str) constructs str as a WSReal.

- **Signature**: `(String)->%`

coerce(r) coerces r to a DoubleFloat.

- **Signature**: `(%)->DoubleFloat`

coerce(x) converts x to a JLFloat.

- **Signature**: `(%)->JLFloat`

coerce(r) coerces r to a JLFloat64.

- **Signature**: `(%)->JLFloat64`

### `erf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L535)\]

erf(x) computes the error function of x.

- **Signature**: `(%,%)->%`

### `erfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L537)\]

erfc(x) computes the complementary error function of x.

- **Signature**: `(%)->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L525)\]

exp() returns the WSAPReal ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `integerPart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L520)\]

integerPart(x) returns the integer part of x.

- **Signature**: `(%)->WSInteger`

### `jWSReal` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L561)\]

jWSReal(z) coerces z to a WSReal.

- **Signature**: `(DoubleFloat)->%`
- **Signature**: `(Float)->%`
- **Signature**: `(JLFloat64)->%`

jWSReal(z) coerces the integer z to a WSReal.

- **Signature**: `(Integer)->%`

jWSReal(str) constructs str as a WSReal.

- **Signature**: `(String)->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L522)\]

jlApprox?(x,y) computes inexact equality comparison with WS default parameters (Equal).

- **Signature**: `(%,%)->Boolean`

### `log10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L531)\]

log10(x) computes the logarithm of x in base 10.

- **Signature**: `(%)->%`

### `log2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L529)\]

log2(x) computes the logarithm of x in base 2.

- **Signature**: `(%)->%`

### `rationalApproximation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L539)\]

rationalApproximation(x) tries to find a rational approximation of x. Error if x can not be retracted.

- **Signature**: `(%)->WSRational`

rationalApproximation(x, dx) returns a rational approximation of x within tolerance dx. If dx = 0, converts it anyway.

- **Signature**: `(%,%)->WSRational`

### `sinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L527)\]

sinc(x) computes the unnormalized sinc of x, sin(x)/x.

- **Signature**: `(%)->%`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L518)\]

urand01() returns a uniformly distributed random number in the range 0..1.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
