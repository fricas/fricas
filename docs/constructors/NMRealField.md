# NMRealField

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L673)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

NMRealField implements arbitrary precision ball arithmetic using the Nemo Julia package.

**NMRealField is a domain constructor.**  
**Abbreviation for NMRealField is NRF**  
**This constructor is exposed in this frame.**  
**174 names for 229 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, Integer) -> %                                ?*? : (Fraction(Integer), %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?/? : (%, Integer) -> %                                ?/? : (Integer, %) -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               D : % -> %
 D : (%, NonNegativeInteger) -> %                       Gamma : % -> %
 Gamma : (%, %) -> %                                    ?^? : (%, %) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 abs : % -> %                                           abs2 : % -> %
 accuracyBits : % -> JLInt64                            acos : % -> %
 acosh : % -> %                                         acot : % -> %
 acoth : % -> %                                         acsc : % -> %
 acsch : % -> %                                         addError! : (%, %) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 asec : % -> %                                          asech : % -> %
 asin : % -> %                                          asinh : % -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 atan : % -> %                                          atan : (%, %) -> %
 atanh : % -> %                                         base : () -> PositiveInteger
 bits : () -> PositiveInteger                           bits : PositiveInteger -> PositiveInteger
 catalan : () -> %                                      ceiling : % -> %
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Float -> %                                    coerce : Fraction(Integer) -> %
 coerce : Integer -> %                                  coerce : JLFloat64 -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               contains? : (%, %) -> Boolean
 contains? : (%, JLFloat) -> Boolean                    contains? : (%, NMFraction(NMInteger)) -> Boolean
 contains? : (%, NMInteger) -> Boolean                  containsNegative? : % -> Boolean
 containsNonNegative? : % -> Boolean                    containsNonPositive? : % -> Boolean
 containsPositive? : % -> Boolean                       containsZero? : % -> Boolean
 convert : % -> DoubleFloat                             convert : % -> Float
 convert : % -> JLFloat                                 convert : % -> NMFraction(NMInteger)
 convert : % -> Pattern(Float)                          convert : % -> String
 cos : % -> %                                           cosh : % -> %
 cot : % -> %                                           coth : % -> %
 csc : % -> %                                           csch : % -> %
 decreasePrecision : Integer -> PositiveInteger         differentiate : % -> %
 differentiate : (%, NonNegativeInteger) -> %           digits : () -> PositiveInteger
 digits : PositiveInteger -> PositiveInteger            equal? : (%, %) -> Boolean
 euclideanSize : % -> NonNegativeInteger                eulerGamma : () -> %
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 exp : () -> %                                          exp : % -> %
 exp1 : () -> %                                         expm1 : % -> %
 exponent : % -> Integer                                ?exquo? : (%, %) -> Union(%,"failed")
 factor : % -> Factored(%)                              finite? : % -> Boolean
 float : (Integer, Integer) -> %                        float : (Integer, Integer, PositiveInteger) -> %
 floor : % -> %                                         fractionPart : % -> %
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 glaisher : () -> %                                     hurwitzZeta : (%, %) -> %
 increasePrecision : Integer -> PositiveInteger         integer? : % -> Boolean
 inv : % -> %                                           jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlNMRing : () -> String                                jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 jnball : (%, %) -> %                                   jnrf : Float -> %
 jnrf : Fraction(Integer) -> %                          jnrf : Integer -> %
 jnrf : String -> %                                     khinchin : () -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     ldexp : (%, NMInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     log : % -> %
 log1p : % -> %                                         mantissa : % -> Integer
 max : (%, %) -> %                                      midpoint : % -> %
 min : (%, %) -> %                                      missing? : % -> Boolean
 mutable? : % -> Boolean                                negative? : % -> Boolean
 nonNegative? : % -> Boolean                            nonPositive? : % -> Boolean
 nonZero? : % -> Boolean                                norm : % -> %
 nothing? : % -> Boolean                                nthRoot : (%, Integer) -> %
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 order : % -> Integer                                   overlaps? : (%, %) -> Boolean
 pi : () -> %                                           plenaryPower : (%, PositiveInteger) -> %
 polyLog : (%, %) -> %                                  positive? : % -> Boolean
 precision : () -> PositiveInteger                      precision : PositiveInteger -> PositiveInteger
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 radius : % -> %                                        randtest : JLSymbol -> %
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 retract : % -> Fraction(Integer)                       retract : % -> Integer
 retractIfCan : % -> Union(Integer,"failed")            rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 round : % -> %                                         sample : () -> %
 sec : % -> %                                           sech : % -> %
 setUnion : (%, %) -> %                                 sign : % -> Integer
 sin : % -> %                                           sinh : % -> %
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tan : % -> %
 tanh : % -> %                                          toString : % -> String
 toString : (%, NonNegativeInteger) -> String           trim : % -> %
 truncate : % -> %                                      uniqueInteger : % -> Union(NMInteger,"failed")
 unit? : % -> Boolean                                   unitCanonical : % -> %
 urand01 : () -> %                                      wholePart : % -> Integer
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 guess : (%, NonNegativeInteger) -> NMAlgebraicNumber
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 max : () -> % if not % has ATARBEX and not % has ATARBPR
 min : () -> % if not % has ATARBEX and not % has ATARBPR
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 retractIfCan : NMAlgebraicNumber -> Union(%,"failed")
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L747)\]

Gamma(x) is the Euler Gamma function evaluated at x.

- **Signature**: `(%)->%`

Gamma(x,y) is the incomplete Gamma function.

- **Signature**: `(%,%)->%`

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L732)\]

abs2(x) returns the square of the absolute value of x.

- **Signature**: `(%)->%`

### `accuracyBits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L762)\]

accuracyBits(x) returns the relative accuracy of x in bits.

- **Signature**: `(%)->JLInt64`

### `addError!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L759)\]

addError!(x, y) adds the values (absolute) of the midpoint and radius of y to the radius of x.

- **Signature**: `(%,%)->%`

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L753)\]

atan(x, y) computes the inverse tangent of x/y.

- **Signature**: `(%,%)->%`

### `catalan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L710)\]

catalan() returns the Catalan's constant.

- **Signature**: `()->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L804)\]

coerce(x) coerces x.

- **Signature**: `(Float)->%`

coerce(q) coerces q.

- **Signature**: `(Fraction(Integer))->%`

coerce(x) coerces(x).

- **Signature**: `(JLFloat64)->%`

### `contains?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L773)\]

contains?(x,y) checks whether or not y is contained in x.

- **Signature**: `(%,%)->Boolean`
- **Signature**: `(%,JLFloat)->Boolean`
- **Signature**: `(%,NMFraction(NMInteger))->Boolean`
- **Signature**: `(%,NMInteger)->Boolean`

### `containsNegative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L786)\]

containsNegative?(x) cheks whether or not x contains any negative value.

- **Signature**: `(%)->Boolean`

### `containsNonNegative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L789)\]

containsNonNegative?(x) cheks whether or not x contains any non negative value.

- **Signature**: `(%)->Boolean`

### `containsNonPositive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L792)\]

containsNonPositive?(x) checks whether or not x contains any non positive value.

- **Signature**: `(%)->Boolean`

### `containsPositive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L783)\]

containsPositive?(x) cheks whether or not x contains any positive value.

- **Signature**: `(%)->Boolean`

### `containsZero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L781)\]

containsZero?(x) checks whether or not 0 is contained in x.

- **Signature**: `(%)->Boolean`

### `convert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L800)\]

convert(x) converts x to a JLFloat.

- **Signature**: `(%)->JLFloat`

convert(x) converts x to a NMFraction(NMInteger).

- **Signature**: `(%)->NMFraction(NMInteger)`

### `eulerGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L708)\]

eulerGamma() returns the Euler's constant gamma (γ).

- **Signature**: `()->%`

### `exact?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L696)\]

exact?(x) checks whether x is exact i.e. with 0 radius.

- **Signature**: `(%)->Boolean`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L704)\]

exp() returns the NMRealField representation of ℯ (exp(1)).

- **Signature**: `()->%`

### `exp1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L706)\]

exp1() returns the NMRealField representation of ℯ (exp(1)).

- **Signature**: `()->%`

### `expm1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L738)\]

expm1(x) computes accurately e^x-1. It avoids the loss of precision involved in the direct evaluation of exp(x)-1 for small values of x.

- **Signature**: `(%)->%`

### `finite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L693)\]

finite?(x) checks whether or not x is finite, not an infinity for example.

- **Signature**: `(%)->Boolean`

### `glaisher` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L714)\]

glaisher() returns the Glaisher's constant.

- **Signature**: `()->%`

### `guess` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L795)\]

guess(a, deg) returns the reconstructed algebraic number found if it succeeds, up to degree deg.

- **Signature**: `(%,NonNegativeInteger)->NMAlgebraicNumber`

### `hurwitzZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L745)\]

hurwitzZeta(s,a) returns the Hurwitz zeta function of s and a.

- **Signature**: `(%,%)->%`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L691)\]

integer?(x) checks whether or not x is an integer.

- **Signature**: `(%)->Boolean`

### `jnball` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L810)\]

jnball(x,r) returns a ball with midpoint x and radius r.

- **Signature**: `(%,%)->%`

### `jnrf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L812)\]

jnrf(x) returns x as a NMRealField element.

- **Signature**: `(Float)->%`

jnrf(q) returns q as a NMRealField element.

- **Signature**: `(Fraction(Integer))->%`

jnrf(i) returns i as a NMRealField element.

- **Signature**: `(Integer)->%`

jnrf(str) evaluates str in Julia to returns a NMRealField element.

- **Signature**: `(String)->%`

### `khinchin` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L712)\]

khinchin() returns the Khinchin's constant.

- **Signature**: `()->%`

### `ldexp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L743)\]

ldexp(x, n) returns x * 2^n.

- **Signature**: `(%,NMInteger)->%`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L741)\]

log1p(x) logarithm of 1+x computed accurately.

- **Signature**: `(%)->%`

### `midpoint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L755)\]

midpoint(x) returns the midpoint of x.

- **Signature**: `(%)->%`

### `nonNegative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L700)\]

nonNegative?(v) checks whether or not x is greater or equal to zero.

- **Signature**: `(%)->Boolean`

### `nonPositive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L702)\]

nonPositive?(v) checks whether or not x is lower or equal to zero.

- **Signature**: `(%)->Boolean`

### `nonZero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L698)\]

nonZero?(x) returns true if x is not equal to 0.

- **Signature**: `(%)->Boolean`

### `overlaps?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L771)\]

overlaps?(x,y) checks whether or not any part of x and y balls overlaps.

- **Signature**: `(%,%)->Boolean`

### `polyLog` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L751)\]

polyLog(x,y) returns the polyLog function applied to x and y.

- **Signature**: `(%,%)->%`

### `radius` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L757)\]

radius(x) returns the radius of x.

- **Signature**: `(%)->%`

### `randtest` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L719)\]

randtest(randtype) returns a random number depending on the Julia symbol randtype. :urandom a uniformly distributed random number contained in [0,1]. To test corner cases: :randtest returns a finite random number, :randtest_exact returns a zero radius random number, :randtest_precise returns a precise random number i.e. with a radius of around 2^-precision() the magnitude of the midpoint, :randtest_special returns a special random number where midpoint and/or radius might be NaNs or infinities, :randtest_wide returns a random number with a large radius that might be big relative to its midpoint.

- **Signature**: `(JLSymbol)->%`

### `retractIfCan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L798)\]

retractIfCan(x) retracts x if possible. "failed" othewise.

- **Signature**: `(NMAlgebraicNumber)->Union(%,"failed")`

### `setUnion` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L769)\]

setUnion(x,y) returns the unions of the intervals x and y.

- **Signature**: `(%,%)->%`

### `trim` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L764)\]

trim(x) rounds off insignificant bits from the midpoint.

- **Signature**: `(%)->%`

### `uniqueInteger` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L766)\]

uniqueInteger(x) returns a NMInteger if there is a unique integer in the interval x, "failed" otherwise.

- **Signature**: `(%)->Union(NMInteger,"failed")`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L716)\]

urand01() returns a uniformly distributed random number contained in [0,1].

- **Signature**: `()->%`
---
[Back to Index](../index.md)
