# WSAPReal

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L797)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic arbitrary precision real numbers using Wolfram Symbolic Transport Protocol.

**WSAPReal(prec: PositiveInteger) is a domain constructor**  
**Abbreviation for WSAPReal is WSAPR**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 ?/? : (%, %) -> %                                      ?/? : (%, Integer) -> %
 0 : () -> %                                            1 : () -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               Chi : % -> %
 Ci : % -> %                                            D : % -> %
 D : (%, NonNegativeInteger) -> %                       Ei : % -> %
 Shi : % -> %                                           Si : % -> %
 ?^? : (%, %) -> %                                      ?^? : (%, Integer) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, PositiveInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     abs : % -> %
 acos : % -> %                                          acosh : % -> %
 acot : % -> %                                          acoth : % -> %
 acsc : % -> %                                          acsch : % -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 asec : % -> %                                          asech : % -> %
 asin : % -> %                                          asinh : % -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 atan : (%, %) -> %                                     atan : % -> %
 atanh : % -> %                                         base : () -> PositiveInteger
 bits : () -> PositiveInteger                           ceiling : % -> %
 characteristic : () -> NonNegativeInteger              coerce : JLFloat -> %
 coerce : Float -> %                                    coerce : String -> %
 coerce : % -> JLFloat                                  coerce : % -> %
 coerce : Fraction(Integer) -> %                        coerce : Integer -> %
 coerce : % -> WSExpression                             coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> Float                                   convert : % -> DoubleFloat
 convert : % -> Pattern(Float)                          convert : % -> String
 cos : % -> %                                           cosh : % -> %
 cot : % -> %                                           coth : % -> %
 csc : % -> %                                           csch : % -> %
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 digits : () -> PositiveInteger                         dilog : % -> %
 erf : (%, %) -> %                                      erf : % -> %
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
 jWSInterpret : (String, String, String) -> %           jWSInterpret : (String, String) -> %
 jWSInterpret : String -> %                             jWSReal : String -> %
 jWSReal : JLFloat -> %                                 jWSReal : Float -> %
 jWSReal : JLFloat64 -> %                               jWSReal : DoubleFloat -> %
 jWSReal : Integer -> %                                 jlAbout : % -> Void
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %) -> JLObject
 jlApply : (String, %) -> JLObject                      jlApprox? : (%, %) -> Boolean
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
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     li : % -> %
 log : % -> %                                           log10 : % -> %
 log2 : % -> %                                          mantissa : % -> Integer
 max : (%, %) -> %                                      min : (%, %) -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 negative? : % -> Boolean                               norm : % -> %
 nothing? : % -> Boolean                                nthRoot : (%, Integer) -> %
 numeric : (%, PositiveInteger) -> WSExpression         numeric : % -> WSExpression
 numeric? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> Integer
 pi : () -> %                                           plenaryPower : (%, PositiveInteger) -> %
 positive? : % -> Boolean                               precision : () -> PositiveInteger
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 rationalApproximation : (%, %) -> WSRational           rationalApproximation : % -> WSRational
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 retract : % -> Integer                                 retract : % -> Fraction(Integer)
 retractIfCan : % -> Union(Integer,"failed")            rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 round : % -> %                                         sample : () -> %
 sec : % -> %                                           sech : % -> %
 sign : % -> Integer                                    sin : % -> %
 sinc : % -> %                                          sinh : % -> %
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tan : % -> %
 tanh : % -> %                                          toString : (%, NonNegativeInteger) -> String
 toString : % -> String                                 truncate : % -> %
 unit? : % -> Boolean                                   unitCanonical : % -> %
 urand01 : () -> %                                      wholePart : % -> Integer
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ
 bits : PositiveInteger -> PositiveInteger if % has ATARBPR
 decreasePrecision : Integer -> PositiveInteger if % has ATARBPR
 digits : PositiveInteger -> PositiveInteger if % has ATARBPR
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 increasePrecision : Integer -> PositiveInteger if % has ATARBPR
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 max : () -> % if not % has ATARBEX and not % has ATARBPR
 min : () -> % if not % has ATARBEX and not % has ATARBPR
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 precision : PositiveInteger -> PositiveInteger if % has ATARBPR
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L827)\]

atan(x,y) computes the arc tangent of y/x.

- **Signature**: `(%,%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L839)\]

coerce(x) converts x as a WSAPReal.

- **Signature**: `(Float)->%`
- **Signature**: `(JLFloat)->%`

coerce(str) constructs str as a WSAPReal.

- **Signature**: `(String)->%`

coerce(x) converts x as a JLFloat.

- **Signature**: `(%)->JLFloat`

### `erf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L829)\]

erf(x) computes the error function of x.

- **Signature**: `(%,%)->%`

### `erfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L831)\]

erfc(x) computes the complementary error function of x.

- **Signature**: `(%)->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L819)\]

exp() returns the WSAPReal ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `integerPart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L814)\]

integerPart(x) returns the integer part of x.

- **Signature**: `(%)->WSInteger`

### `jWSReal` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L847)\]

jWSReal(f) coerces f to a WSAPReal.

- **Signature**: `(DoubleFloat)->%`
- **Signature**: `(Float)->%`
- **Signature**: `(JLFloat)->%`
- **Signature**: `(JLFloat64)->%`

jWSReal(i) coerces i to a WSAPReal.

- **Signature**: `(Integer)->%`

jWSReal(str) constructs str as a WSAPReal.

- **Signature**: `(String)->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L816)\]

jlApprox?(x,y) computes inexact equality comparison with WS default parameters (Equal).

- **Signature**: `(%,%)->Boolean`

### `log10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L825)\]

log10(x) computes the logarithm of x in base 10.

- **Signature**: `(%)->%`

### `log2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L823)\]

log2(x) computes the logarithm of x in base 2.

- **Signature**: `(%)->%`

### `rationalApproximation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L833)\]

rationalApproximation(x) tries to find a rational approximation of x.

- **Signature**: `(%)->WSRational`

rationalApproximation(x, dx) tries to find a rational approximation of x within tolerance dx.

- **Signature**: `(%,%)->WSRational`

### `sinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L821)\]

sinc(x) computes the unnormalized sinc of x, sin(x)/x and 0 if x = 0.

- **Signature**: `(%)->%`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L812)\]

urand01() returns a uniformly distributed random number in the range 0..1.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
