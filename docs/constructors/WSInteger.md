# WSInteger

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L263)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic integers using the MathLink Julia package.

**WSInteger is a domain constructor.**  
**Abbreviation for WSInteger is WSINT**  
**This constructor is exposed in this frame.**  
**125 names for 156 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (Integer, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (PositiveInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 -? : % -> %                                            ?-? : (%, %) -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               D : % -> %
 D : (%, NonNegativeInteger) -> %                       ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        abs : % -> %
 addmod : (%, %, %) -> %                                annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            base : () -> %
 binomial : (%, %) -> %                                 bit? : (%, %) -> Boolean
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               coerce : % -> WSExpression
 coerce : % -> WSRational                               commutator : (%, %) -> %
 convert : % -> DoubleFloat                             convert : % -> Float
 convert : % -> InputForm                               convert : % -> Integer
 convert : % -> Pattern(Integer)                        convert : % -> String
 copy : % -> %                                          dec : % -> %
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 euclideanSize : % -> NonNegativeInteger                even? : % -> Boolean
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 factor : % -> WSList(WSList(%))                        factorial : % -> %
 fibonacci : % -> %                                     gcd : (%, %) -> %
 gcd : List(%) -> %                                     hyperFactorial : % -> %
 inc : % -> %                                           init : () -> %
 integer : % -> Integer                                 invmod : (%, %) -> %
 jWSInt : Integer -> %                                  jWSInterpret : String -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : (String, String, String) -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %, %, %) -> JLObject
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
 leftRecip : % -> Union(%,"failed")                     length : % -> %
 mask : % -> %                                          max : (%, %) -> %
 min : (%, %) -> %                                      missing? : % -> Boolean
 mulmod : (%, %, %) -> %                                mutable? : % -> Boolean
 negative? : % -> Boolean                               nextItem : % -> Union(%,"failed")
 nothing? : % -> Boolean                                numeric : % -> WSExpression
 numeric : (%, PositiveInteger) -> WSExpression         numeric? : % -> Boolean
 odd? : % -> Boolean                                    one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          permutation : (%, %) -> %
 plenaryPower : (%, PositiveInteger) -> %               positive? : % -> Boolean
 positiveRemainder : (%, %) -> %                        powmod : (%, %, %) -> %
 prime? : % -> Boolean                                  qFactorial : (%, %) -> %
 ?quo? : (%, %) -> %                                    random : % -> %
 rational : % -> Fraction(Integer)                      rational? : % -> Boolean
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 retract : % -> Integer                                 retractIfCan : % -> Union(Integer,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 shift : (%, %) -> %                                    sign : % -> Integer
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 stirling1 : (%, %) -> %                                stirling2 : (%, %) -> %
 string : % -> String                                   submod : (%, %, %) -> %
 subtractIfCan : (%, %) -> Union(%,"failed")            symmetricRemainder : (%, %) -> %
 toString : % -> String                                 unit? : % -> Boolean
 unitCanonical : % -> %                                 zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
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

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L294)\]

coerce(z) coerces z.

- **Signature**: `(Integer)->%`
- **Signature**: `(%)->WSRational`

### `factor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L292)\]

factor(n) returns the WS factorization of n.

- **Signature**: `(%)->WSList(WSList(%))`

### `fibonacci` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L282)\]

fibonacci(n) returns the Fibonacci number Fn.

- **Signature**: `(%)->%`

### `hyperFactorial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L278)\]

hyperFactorial(n) computes the hyperfactorial of n.

- **Signature**: `(%)->%`

### `integer` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L276)\]

integer(i) returns i as a FriCAS Integer.

- **Signature**: `(%)->Integer`

### `jWSInt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L298)\]

jWSInt(int) constructs int as a WSInteger.

- **Signature**: `(Integer)->%`

### `qFactorial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L280)\]

qFactorial(x,q) returns the q-analog of factorial of x.

- **Signature**: `(%,%)->%`

### `stirling1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L284)\]

stirling1(n,m) returns the Stirling number of the first kind from n and m. For example:

**Example**:
```fricas
stirling1(20 , jWSInt 2)
```

- **Signature**: `(%,%)->%`

### `stirling2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L288)\]

stirling2(n,m) returns the Stirling number of the second kind from n and m. For example:

**Example**:
```fricas
stirling2(2 0, jWSInt 2)
```

- **Signature**: `(%,%)->%`
---
[Back to Index](../index.md)
