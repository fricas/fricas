# JLFloat32

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L188)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

JLFloat32 implements 32 bits floating point arithmetic using Julia Float32 type. Bear in mind that, like JLInt64, the internal representation depends on the underlying Lisp implementation, so the usual pure arithmetic operations occur there. For other functions like sqrt, log, exp, transcendental functions etc. the computation is performed at machine level using Julia (generally in C language, or even using assembly language).

**JLFloat32 is a domain constructor.**  
**Abbreviation for JLFloat32 is JF32**  
**This constructor is exposed in this frame.**  
**129 names for 169 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (Fraction(Integer), %) -> %                      ?*? : (Integer, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (PositiveInteger, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?/? : (%, Integer) -> %                                ?<? : (%, %) -> Boolean
 ?<=? : (%, %) -> Boolean                               ?=? : (%, %) -> Boolean
 ?>? : (%, %) -> Boolean                                ?>=? : (%, %) -> Boolean
 D : % -> %                                             D : (%, NonNegativeInteger) -> %
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
 bits : () -> PositiveInteger                           catalan : () -> %
 ceiling : % -> %                                       characteristic : () -> NonNegativeInteger
 coerce : % -> %                                        coerce : Fraction(Integer) -> %
 coerce : Integer -> %                                  coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : DoubleFloat -> %
 convert : JLFloat64 -> %                               convert : % -> DoubleFloat
 convert : % -> Float                                   convert : % -> Pattern(Float)
 convert : % -> String                                  cos : % -> %
 cosh : % -> %                                          cot : % -> %
 coth : % -> %                                          csc : % -> %
 csch : % -> %                                          differentiate : % -> %
 differentiate : (%, NonNegativeInteger) -> %           digits : () -> PositiveInteger
 euclideanSize : % -> NonNegativeInteger                eulerGamma : () -> %
 exp : () -> %                                          exp : % -> %
 expm1 : % -> %                                         exponent : % -> Integer
 exprand : () -> %                                      ?exquo? : (%, %) -> Union(%,"failed")
 factor : % -> Factored(%)                              float : (Integer, Integer) -> %
 float : (Integer, Integer, PositiveInteger) -> %       floor : % -> %
 fractionPart : % -> %                                  gcd : (%, %) -> %
 gcd : List(%) -> %                                     goldenRatio : () -> %
 inv : % -> %                                           jf32 : DoubleFloat -> %
 jf32 : Integer -> %                                    jf32 : JLFloat64 -> %
 jlApply : (String, %) -> %                             jlApply : (String, %, %) -> %
 jlApply : (String, %, %, %) -> %                       jlApprox? : (%, %) -> Boolean
 jlCApply : (String, JLSymbol, %) -> %                  jlCApply : (String, JLSymbol, %, %) -> %
 jlCApply : (String, JLSymbol, %, %, %) -> %            latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     log : % -> %
 log10 : % -> %                                         log2 : % -> %
 mantissa : % -> Integer                                max : (%, %) -> %
 min : (%, %) -> %                                      nan : () -> %
 negative? : % -> Boolean                               negativeInfinity : () -> %
 norm : % -> %                                          nrand : () -> %
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> Integer
 pi : () -> %                                           plenaryPower : (%, PositiveInteger) -> %
 positive? : % -> Boolean                               positiveInfinity : () -> %
 precision : () -> PositiveInteger                      prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    retract : % -> Fraction(Integer)
 retract : % -> Integer                                 retractIfCan : % -> Union(Integer,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    round : % -> %
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          sign : % -> Integer
 sin : % -> %                                           sinh : % -> %
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tan : % -> %
 tanh : % -> %                                          toString : % -> String
 toString : (%, NonNegativeInteger) -> String           truncate : % -> %
 unit? : % -> Boolean                                   unitCanonical : % -> %
 urand01 : () -> %                                      wholePart : % -> Integer
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
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

### `jf32` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L209)\]

jf32(x) coerces x to a Julia Float32.

- **Signature**: `(DoubleFloat)->%`
- **Signature**: `(JLFloat64)->%`

jf32(i) coerces i to a Julia Float32.

- **Signature**: `(Integer)->%`
---
[Back to Index](../index.md)
