# NMRealBall

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L346)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

convenience domain to reflect Nemo ArbField(256), i.e. without parameters.

**NMRealBall is a domain constructor.**  
**Abbreviation for NMRealBall is NRB**  
**This constructor is exposed in this frame.**  
**174 names for 231 operations in this domain.**

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
 bits : % -> JLInt64                                    bits : () -> PositiveInteger
 bits : PositiveInteger -> PositiveInteger              catalan : () -> %
 ceiling : % -> %                                       characteristic : () -> NonNegativeInteger
 coerce : % -> %                                        coerce : Float -> %
 coerce : Fraction(Integer) -> %                        coerce : Integer -> %
 coerce : JLFloat64 -> %                                coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 contains? : (%, %) -> Boolean                          contains? : (%, JLFloat) -> Boolean
 contains? : (%, NMFraction(NMInteger)) -> Boolean      contains? : (%, NMInteger) -> Boolean
 containsNegative? : % -> Boolean                       containsNonNegative? : % -> Boolean
 containsNonPositive? : % -> Boolean                    containsPositive? : % -> Boolean
 containsZero? : % -> Boolean                           convert : % -> DoubleFloat
 convert : % -> Float                                   convert : % -> JLFloat
 convert : % -> NMFraction(NMInteger)                   convert : % -> Pattern(Float)
 convert : % -> String                                  cos : % -> %
 cosh : % -> %                                          cot : % -> %
 coth : % -> %                                          csc : % -> %
 csch : % -> %                                          decreasePrecision : Integer -> PositiveInteger
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 digits : () -> PositiveInteger                         digits : PositiveInteger -> PositiveInteger
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 eulerGamma : () -> %                                   exact? : % -> Boolean
 exactDivide : (%, %) -> %                              exp : () -> %
 exp : % -> %                                           exp1 : () -> %
 expm1 : % -> %                                         exponent : % -> Integer
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 finite? : % -> Boolean                                 float : (Integer, Integer) -> %
 float : (Integer, Integer, PositiveInteger) -> %       floor : % -> %
 fractionPart : % -> %                                  gcd : (%, %) -> %
 gcd : List(%) -> %                                     glaisher : () -> %
 hurwitzZeta : (%, %) -> %                              increasePrecision : Integer -> PositiveInteger
 integer? : % -> Boolean                                inv : % -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %, %, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlId : % -> JLInt64                                    jlNMRing : () -> String
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jnball : (%, %) -> %
 jnrb : Float -> %                                      jnrb : Fraction(Integer) -> %
 jnrb : Integer -> %                                    jnrb : NMAlgebraicNumber -> %
 jnrb : NMExactCalciumField -> %                        jnrb : String -> %
 khinchin : () -> %                                     latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 ldexp : (%, NMInteger) -> %                            leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 log : % -> %                                           log1p : % -> %
 mantissa : % -> Integer                                max : (%, %) -> %
 midpoint : % -> %                                      min : (%, %) -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 negative? : % -> Boolean                               nonNegative? : % -> Boolean
 nonPositive? : % -> Boolean                            nonZero? : % -> Boolean
 norm : % -> %                                          nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> Integer
 overlaps? : (%, %) -> Boolean                          pi : () -> %
 plenaryPower : (%, PositiveInteger) -> %               polyLog : (%, %) -> %
 positive? : % -> Boolean                               precision : () -> PositiveInteger
 precision : PositiveInteger -> PositiveInteger         prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    radius : % -> %
 randtest : JLSymbol -> %                               recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    retract : % -> Fraction(Integer)
 retract : % -> Integer                                 retractIfCan : % -> Union(Integer,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    round : % -> %
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          setUnion : (%, %) -> %
 sign : % -> Integer                                    sin : % -> %
 sinh : % -> %                                          sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 tan : % -> %                                           tanh : % -> %
 toString : % -> String                                 toString : (%, NonNegativeInteger) -> String
 trim : % -> %                                          truncate : % -> %
 uniqueInteger : % -> Union(NMInteger,"failed")         unit? : % -> Boolean
 unitCanonical : % -> %                                 urand01 : () -> %
 wholePart : % -> Integer                               zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
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
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L73)\]

Gamma(x) is the Euler Gamma function evaluated at x.

- **Signature**: `(%)->%`

Gamma(x,y) is the incomplete Gamma function.

- **Signature**: `(%,%)->%`

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L56)\]

abs2(x) returns the square of the absolute value of x.

- **Signature**: `(%)->%`

### `accuracyBits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L116)\]

accuracyBits(x) returns the relative accuracy of x in bits.

- **Signature**: `(%)->JLInt64`

### `addError!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L122)\]

addError!(x, y) adds the values (absolute) of the midpoint and radius of y to the radius of x.

- **Signature**: `(%,%)->%`

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L69)\]

atan(x, y) computes the inverse tangent of x/y.

- **Signature**: `(%,%)->%`

### `bits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L108)\]

bits(x) returns the bit length of the mantissa of x. For a result computed at prec bits of precision this canbe anywhere in the range 0 <= b <= prec. For example 0 has 0 bits, 0.75 has 2 bits, and 3.7 has 126 bits after rounding to prec = 128 (with the default rounding mode) because the two least significant bits are zero and thus get discarded. Source: flint-devel@googlegroups.com

- **Signature**: `(%)->JLInt64`

### `catalan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L34)\]

catalan() returns the Catalan's constant.

- **Signature**: `()->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L139)\]

coerce(x) coerces x.

- **Signature**: `(Float)->%`
- **Signature**: `(JLFloat64)->%`

### `contains?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L86)\]

contains?(x,y) checks whether or not y is contained in x.

- **Signature**: `(%,%)->Boolean`
- **Signature**: `(%,JLFloat)->Boolean`
- **Signature**: `(%,NMFraction(NMInteger))->Boolean`
- **Signature**: `(%,NMInteger)->Boolean`

### `containsNegative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L99)\]

containsNegative?(x) cheks whether or not x contains any negative value.

- **Signature**: `(%)->Boolean`

### `containsNonNegative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L102)\]

containsNonNegative?(x) cheks whether or not x contains any non negative value.

- **Signature**: `(%)->Boolean`

### `containsNonPositive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L105)\]

containsNonPositive?(x) checks whether or not x contains any non positive value.

- **Signature**: `(%)->Boolean`

### `containsPositive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L96)\]

containsPositive?(x) cheks whether or not x contains any positive value.

- **Signature**: `(%)->Boolean`

### `containsZero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L94)\]

containsZero?(x) checks whether or not 0 is contained in x.

- **Signature**: `(%)->Boolean`

### `convert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L135)\]

convert(x) converts x.

- **Signature**: `(%)->JLFloat`
- **Signature**: `(%)->NMFraction(NMInteger)`

### `eulerGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L32)\]

eulerGamma() returns the Euler's constant gamma (γ).

- **Signature**: `()->%`

### `exact?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L24)\]

exact?(x) checks whether x is exact i.e. with 0 radius.

- **Signature**: `(%)->Boolean`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L63)\]

exp() returns the NMArbField ℯ (exp(1)).

- **Signature**: `()->%`

### `exp1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L65)\]

exp1() returns the NMArbField ℯ (exp(1)).

- **Signature**: `()->%`

### `expm1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L58)\]

expm1(x) computes accurately e^x-1. It avoids the loss of precision involved in the direct evaluation of exp(x)-1 for small values of x.

- **Signature**: `(%)->%`

### `finite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L21)\]

finite?(x) checks whether or not x is finite, not an infinity for example.

- **Signature**: `(%)->Boolean`

### `glaisher` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L38)\]

glaisher() returns the Glaisher's constant.

- **Signature**: `()->%`

### `guess` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L132)\]

guess(a, deg) returns the reconstructed algebraic number found if it succeeds. Up to degree deg.

- **Signature**: `(%,NonNegativeInteger)->NMAlgebraicNumber`

### `hurwitzZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L71)\]

hurwitzZeta(s,a) returns the Hurwitz zeta function of s and a.

- **Signature**: `(%,%)->%`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L19)\]

integer?(x) checks whether or not x is an integer.

- **Signature**: `(%)->Boolean`

### `jnball` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L143)\]

jnball(x,r) returns a ball with midpoint x and radius r.

- **Signature**: `(%,%)->%`

### `jnrb` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L145)\]

jnrb(fl) returns fl as a real Arb ball.

- **Signature**: `(Float)->%`

jnrb(fi) returns fi as a real Arb ball.

- **Signature**: `(Fraction(Integer))->%`

jnrb(i) returns i as a real Arb ball.

- **Signature**: `(Integer)->%`

jnrb(jan) evaluates numerically jan by converting it to a real Arb field.

- **Signature**: `(NMAlgebraicNumber)->%`

jnrb(necf) evaluates numerically necf by converting it to a real Arb field.

- **Signature**: `(NMExactCalciumField)->%`

jnrb(str) evaluates str by converting it to a real Arb field.

- **Signature**: `(String)->%`

### `khinchin` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L36)\]

khinchin() returns the Khinchin's constant.

- **Signature**: `()->%`

### `ldexp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L67)\]

ldexp(x, n) returns x * 2^n.

- **Signature**: `(%,NMInteger)->%`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L61)\]

log1p(x) logarithm of 1+x computed accurately.

- **Signature**: `(%)->%`

### `midpoint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L118)\]

midpoint(x) returns the midpoint of x.

- **Signature**: `(%)->%`

### `nonNegative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L28)\]

nonNegative?(v) checks whether or not x is greater or equal to zero.

- **Signature**: `(%)->Boolean`

### `nonPositive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L30)\]

nonPositive?(v) checks whether or not x is lower or equal to zero.

- **Signature**: `(%)->Boolean`

### `nonZero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L26)\]

nonZero?(x) returns true if x is not equal to 0.

- **Signature**: `(%)->Boolean`

### `overlaps?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L83)\]

overlaps?(x,y) checks whether or not any part of x and y balls overlaps.

- **Signature**: `(%,%)->Boolean`

### `polyLog` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L77)\]

polyLog(x,y) returns the polyLog function applied to x and y.

- **Signature**: `(%,%)->%`

### `radius` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L120)\]

radius(x) returns the radius of x.

- **Signature**: `(%)->%`

### `randtest` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L43)\]

randtest(randtype) returns a random number depending on the Julia symbol randtype. :urandom a uniformly distributed random number contained in [0,1]. To test corner cases: :randtest returns a finite random number, :randtest_exact returns a zero radius random number, :randtest_precise returns a precise random number i.e. with a radius of around 2^-precision() the magnitude of the midpoint, :randtest_special returns a special random number where midpoint and/or radius might be NaNs or infinities, :randtest_wide returns a random number with a large radius that might be big relative to its midpoint.

- **Signature**: `(JLSymbol)->%`

### `setUnion` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L130)\]

setUnion(x,y) returns the unions of the intervals x and y.

- **Signature**: `(%,%)->%`

### `trim` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L125)\]

trim(x) rounds off insignificant bits from the midpoint.

- **Signature**: `(%)->%`

### `uniqueInteger` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L127)\]

uniqueInteger(x) returns a NMInteger if there is a unique integer in the interval x, "failed" otherwise.

- **Signature**: `(%)->Union(NMInteger,"failed")`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L40)\]

urand01() returns a uniformly distributed random number contained in [0,1].

- **Signature**: `()->%`
---
[Back to Index](../index.md)
