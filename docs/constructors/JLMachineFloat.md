# JLMachineFloat

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L38)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Parent category of Julia machine float domains implemented in Common Lisp.

**JLMachineFloat is a category constructor**  
**Abbreviation for JLMachineFloat is JMFLOAT**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?+? : (%, %) -> %                                      ?-? : (%, %) -> %
 -? : % -> %                                            ?/? : (%, %) -> %
 ?/? : (%, Integer) -> %                                0 : () -> %
 1 : () -> %                                            ?<? : (%, %) -> Boolean
 ?<=? : (%, %) -> Boolean                               ?=? : (%, %) -> Boolean
 ?>? : (%, %) -> Boolean                                ?>=? : (%, %) -> Boolean
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, Integer) -> %                                ?^? : (%, Fraction(Integer)) -> %
 abs : % -> %                                           annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            atan : (%, %) -> %
 base : () -> PositiveInteger                           bits : () -> PositiveInteger
 catalan : () -> %                                      ceiling : % -> %
 characteristic : () -> NonNegativeInteger              coerce : Integer -> %
 coerce : % -> %                                        coerce : Fraction(Integer) -> %
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> Float                                   convert : % -> DoubleFloat
 convert : % -> Pattern(Float)                          convert : % -> String
 digits : () -> PositiveInteger                         euclideanSize : % -> NonNegativeInteger
 eulerGamma : () -> %                                   exp : () -> %
 expm1 : % -> %                                         exponent : % -> Integer
 exprand : () -> %                                      ?exquo? : (%, %) -> Union(%,"failed")
 factor : % -> Factored(%)                              float : (Integer, Integer) -> %
 float : (Integer, Integer, PositiveInteger) -> %       floor : % -> %
 fractionPart : % -> %                                  gcd : (%, %) -> %
 gcd : List(%) -> %                                     goldenRatio : () -> %
 inv : % -> %                                           jlApply : (String, %, %, %) -> %
 jlApply : (String, %, %) -> %                          jlApply : (String, %) -> %
 jlApprox? : (%, %) -> Boolean                          jlCApply : (String, JLSymbol, %, %, %) -> %
 jlCApply : (String, JLSymbol, %, %) -> %               jlCApply : (String, JLSymbol, %) -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 log10 : % -> %                                         log2 : % -> %
 mantissa : % -> Integer                                max : (%, %) -> %
 min : (%, %) -> %                                      nan : () -> %
 negative? : % -> Boolean                               negativeInfinity : () -> %
 norm : % -> %                                          nrand : () -> %
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> Integer
 plenaryPower : (%, PositiveInteger) -> %               positive? : % -> Boolean
 positiveInfinity : () -> %                             precision : () -> PositiveInteger
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 retract : % -> Integer                                 retract : % -> Fraction(Integer)
 retractIfCan : % -> Union(Integer,"failed")            rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 round : % -> %                                         sample : () -> %
 sign : % -> Integer                                    sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 toString : % -> String                                 toString : (%, NonNegativeInteger) -> String
 truncate : % -> %                                      unit? : % -> Boolean
 unitCanonical : % -> %                                 urand01 : () -> %
 wholePart : % -> Integer                               zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
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
 max : () -> % if not % has ATARBPR and not % has ATARBEX
 min : () -> % if not % has ATARBPR and not % has ATARBEX
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 precision : PositiveInteger -> PositiveInteger if % has ATARBPR
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L89)\]

atan(x, y) computes the inverse tangent of x/y.

- **Signature**: `(%,%)->%`

### `catalan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L95)\]

catalan() returns the Catalan's constant.

- **Signature**: `()->%`

### `eulerGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L93)\]

eulerGamma() returns the Euler's constant gamma (γ).

- **Signature**: `()->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L91)\]

exp() returns the JLFloat64 ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `expm1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L81)\]

expm1(x) computes accurately e^x-1.

- **Signature**: `(%)->%`

### `exprand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L78)\]

exprand() returns a random number from the exponential distribution with scale 1.

- **Signature**: `()->%`

### `goldenRatio` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L97)\]

goldenRatio() returns the golden ratio.

- **Signature**: `()->%`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L52)\]

jlApply(func, x) applies func to argument x.

- **Signature**: `(String,%)->%`

jlApply(func, x, y) applies func to arguments x and y.

- **Signature**: `(String,%,%)->%`

jlApply(func, x, y, z) applies func to arguments x, y and z.

- **Signature**: `(String,%,%,%)->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L47)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%)->Boolean`

### `jlCApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L58)\]

jlCApply(lib, func, x) applies the C function func from the library lib to argument x. For example:

**Example**:
```fricas
jlCApply("libm.so.6",jsym(sqrt),jf64(2.0))
```

- **Signature**: `(String,JLSymbol,%)->%`

jlCApply(lib, func, x, y) applies the C function func from the library lib to arguments x and y. For example:

**Example**:
```fricas
jlCApply("libopenlibm", jsym(pow),jf64(2.7),jf64(3.0)) OpenLibm library is provided by Julia.
```

- **Signature**: `(String,JLSymbol,%,%)->%`

jlCApply(lib, func, x, y, z) applies the C function func from the library lib to arguments x, y and z. For example if you have the GNU Scientific Library installed: 

**Example**:
```fricas
jlCApply("libgsl",jsym(gsl_hypot3),jf64(2.0), jf64(7.0),jf64(9.0))
```

- **Signature**: `(String,JLSymbol,%,%,%)->%`

### `log10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L85)\]

log10(x) computes the base 10 logarithm of x.

- **Signature**: `(%)->%`

### `log2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L83)\]

log2(x) computes the base 2 logarithm of x.

- **Signature**: `(%)->%`

### `nan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L99)\]

nan() returns the Julia Float64 NaN (not a number) constant.

- **Signature**: `()->%`

### `negativeInfinity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L104)\]

negativeInfinity() returns the Julia Float64 negative infinity constant.

- **Signature**: `()->%`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L76)\]

nrand() returns a normally distributed random number.

- **Signature**: `()->%`

### `positiveInfinity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L101)\]

positiveInfinity() returns the Julia Float64 positive infinity constant.

- **Signature**: `()->%`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L73)\]

urand01() returns a uniformly distributed random number contained in [0,1].

- **Signature**: `()->%`
---
[Back to Index](../index.md)
