# JLInt64

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L140)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain allows the manipulation of Julia Int64. Beware of internal Lisp implementations of machine integer, usually they differ, for example Lisp MOST-POSITIVE-FIXNUM on x86_64 GNU/Linux is 4611686018427387903 with SBCL whereas it is 1152921504606846975 with Clozure CL. They will be passed as an Int64 to Julia nevertheless,but returned value can possibly not fit in a Lisp fixnum. In Julia, typemax(Int64) is 9223372036854775807 on this arch. This domain is therefore not intended to perform "advanced" computation, it just includes basic arithmetic and is used, for example, for returned pivot vectors in linear algebra.

**JLInt64 is a domain constructor.**  
**Abbreviation for JLInt64 is JI64**  
**This constructor is exposed in this frame.**  
**93 names for 117 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (Integer, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (PositiveInteger, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?<? : (%, %) -> Boolean
 ?<=? : (%, %) -> Boolean                               ?=? : (%, %) -> Boolean
 ?>? : (%, %) -> Boolean                                ?>=? : (%, %) -> Boolean
 D : % -> %                                             D : (%, NonNegativeInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 abs : % -> %                                           addmod : (%, %, %) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 base : () -> %                                         binomial : (%, %) -> %
 bit? : (%, %) -> Boolean                               characteristic : () -> NonNegativeInteger
 coerce : % -> %                                        coerce : Integer -> %
 coerce : SingleInteger -> %                            coerce : % -> OutputForm
 coerce : % -> SingleInteger                            commutator : (%, %) -> %
 convert : % -> DoubleFloat                             convert : % -> Float
 convert : % -> InputForm                               convert : % -> Integer
 convert : % -> Pattern(Integer)                        convert : % -> String
 copy : % -> %                                          dec : % -> %
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 euclideanSize : % -> NonNegativeInteger                even? : % -> Boolean
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 factorial : % -> %                                     gcd : (%, %) -> %
 gcd : List(%) -> %                                     inc : % -> %
 init : () -> %                                         invmod : (%, %) -> %
 ji64 : Integer -> %                                    ji64 : SingleInteger -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 length : % -> %                                        mask : % -> %
 max : () -> %                                          max : (%, %) -> %
 min : () -> %                                          min : (%, %) -> %
 mulmod : (%, %, %) -> %                                negative? : % -> Boolean
 nextItem : % -> Union(%,"failed")                      odd? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 permutation : (%, %) -> %                              plenaryPower : (%, PositiveInteger) -> %
 positive? : % -> Boolean                               positiveRemainder : (%, %) -> %
 powmod : (%, %, %) -> %                                prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    random : % -> %
 rational : % -> Fraction(Integer)                      rational? : % -> Boolean
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 retract : % -> Integer                                 retractIfCan : % -> Union(Integer,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 shift : (%, %) -> %                                    sign : % -> Integer
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   submod : (%, %, %) -> %
 subtractIfCan : (%, %) -> Union(%,"failed")            symmetricRemainder : (%, %) -> %
 unit? : % -> Boolean                                   unitCanonical : % -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
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
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L163)\]

coerce(x) coerces x.

- **Signature**: `(SingleInteger)->%`

coerce(x) coerces x to a SingleInteger.

- **Signature**: `(%)->SingleInteger`

### `ji64` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L167)\]

ji64(x) returns x as a Julia Int64 (in Julia memory area). Error if x cannot be coerced.

- **Signature**: `(Integer)->%`

ji64(x) returns x as a Julia Int64 (in Julia memory area).

- **Signature**: `(SingleInteger)->%`

### `max` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L160)\]

max() returns the largest JLInt64 compatible with the underlying Common Lisp implementation.

- **Signature**: `()->%`

### `min` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L157)\]

min() returns the smallest JLInt64 compatible with the underlying Common Lisp implementation.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
