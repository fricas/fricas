# NMInteger

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L64)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This domain allows the manipulation of Nemo integers using the Nemo Julia package (FLINT based).

**NMInteger is a domain constructor.**  
**Abbreviation for NMInteger is NINT**  
**This constructor is exposed in this frame.**  
**123 names for 154 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Integer) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?<? : (%, %) -> Boolean
 ?<=? : (%, %) -> Boolean                               ?=? : (%, %) -> Boolean
 ?>? : (%, %) -> Boolean                                ?>=? : (%, %) -> Boolean
 D : % -> %                                             D : (%, NonNegativeInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 abs : % -> %                                           abs2 : % -> %
 addmod : (%, %, %) -> %                                annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            base : () -> %
 binomial : (%, %) -> %                                 bit? : (%, %) -> Boolean
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Integer -> %                                  coerce : JLInt64 -> %
 coerce : % -> Expression(Integer)                      coerce : % -> JLInt64
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> DoubleFloat
 convert : % -> Float                                   convert : % -> InputForm
 convert : % -> Integer                                 convert : % -> JLObjBigInt
 convert : % -> Pattern(Integer)                        convert : % -> String
 copy : % -> %                                          dec : % -> %
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 even? : % -> Boolean                                   exact? : % -> Boolean
 exactDivide : (%, %) -> %                              ?exquo? : (%, %) -> Union(%,"failed")
 factor : % -> Factored(%)                              factorial : % -> %
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 inc : % -> %                                           init : () -> %
 inverse : % -> NMFraction(NMInteger)                   invmod : (%, %) -> %
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
 jlref : String -> %                                    jninteger : Integer -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 length : % -> %                                        mask : % -> %
 max : (%, %) -> %                                      min : (%, %) -> %
 missing? : % -> Boolean                                mulmod : (%, %, %) -> %
 mutable? : % -> Boolean                                negative? : % -> Boolean
 nextItem : % -> Union(%,"failed")                      nothing? : % -> Boolean
 odd? : % -> Boolean                                    one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          permutation : (%, %) -> %
 plenaryPower : (%, PositiveInteger) -> %               positive? : % -> Boolean
 positiveRemainder : (%, %) -> %                        powmod : (%, %, %) -> %
 prime? : % -> Boolean                                  probablePrime? : % -> Boolean
 ?quo? : (%, %) -> %                                    random : % -> %
 random : Segment(Integer) -> %                         rational : % -> Fraction(Integer)
 rational? : % -> Boolean                               recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    retract : % -> Integer
 retractIfCan : % -> Union(Integer,"failed")            rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       shift : (%, %) -> %
 sign : % -> Integer                                    sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 submod : (%, %, %) -> %                                subtractIfCan : (%, %) -> Union(%,"failed")
 symmetricRemainder : (%, %) -> %                       unit? : % -> Boolean
 unitCanonical : % -> %                                 zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 rationalIfCan : % -> Union(Fraction(Integer),"failed")
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed")
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L81)\]

abs2(x) returns the squared absolute value of x.

- **Signature**: `(%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L91)\]

coerce(i) coerces i to a NMInteger.

- **Signature**: `(JLInt64)->%`

coerce(x) coerces x.

- **Signature**: `(%)->Expression(Integer)`

coerce(ni) to a JLInt64 if possible.

- **Signature**: `(%)->JLInt64`

### `convert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L89)\]

convert(i) converts i.

- **Signature**: `(%)->JLObjBigInt`

### `inverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L97)\]

inverse(x) returns the inverse of x.

- **Signature**: `(%)->NMFraction(NMInteger)`

### `jninteger` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L99)\]

jninteger(i) returns i as an NMInteger.

- **Signature**: `(Integer)->%`

### `probablePrime?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L83)\]

probablePrime?(n) checks whether or not n is a probable prime using a Baillie-PSW probable prime test with parameters chosen by Selfridge’s method. See https://flintlib.org/.

- **Signature**: `(%)->Boolean`

### `random` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L87)\]

random(seg) returns a random Nemo integer in the range seg.

- **Signature**: `(Segment(Integer))->%`
---
[Back to Index](../index.md)
