# JLObjFloat64

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L163)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Float64, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather, for returned value or function argument for example. Only basic arithmetic operations are supported.

**JLObjFloat64 is a domain constructor.**  
**Abbreviation for JLObjFloat64 is JOBF64**  
**This constructor is exposed in this frame.**  
**85 names for 116 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Integer) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?/? : (%, Integer) -> %                                ?/? : (Integer, %) -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               ?^? : (%, Fraction(Integer)) -> %
 ?^? : (%, Integer) -> %                                ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 cis : % -> JLObjComplexF64                             cispi : % -> JLObjComplexF64
 coerce : % -> %                                        coerce : DoubleFloat -> %
 coerce : Float -> %                                    coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : Float -> %
 convert : % -> String                                  euclideanSize : % -> NonNegativeInteger
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 inv : % -> %                                           jfloat64 : Float -> %
 jfloat64 : JLFloat64 -> %                              jfloat64 : String -> %
 jinf64 : () -> %                                       jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApprox? : (%, %) -> Boolean
 jlApprox? : (%, %, %) -> Boolean                       jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlF64CApply : (JLObjDynamicLinker, %) -> %
 jlF64CApply : (JLObjDynamicLinker, %, %) -> %          jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jnan64 : () -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 max : (%, %) -> %                                      min : (%, %) -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nothing? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          plenaryPower : (%, PositiveInteger) -> %
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 unit? : % -> Boolean                                   unitCanonical : % -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 jlF64CApply : (JLObjDynamicLinker, %, %, %) -> %
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `cis` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L211)\]

cis(x) returns exp(%i*x) computed efficiently.

- **Signature**: `(%)->JLObjComplexF64`

### `cispi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L213)\]

cispi(x) returns cis(%pi*x) computed efficiently.

- **Signature**: `(%)->JLObjComplexF64`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L217)\]

coerce(x) returns x as a JLObjFloat64.

- **Signature**: `(DoubleFloat)->%`

coerce(x) returns x as a JLObjFloat64 (truncate it). Convenience function.

- **Signature**: `(Float)->%`

### `convert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L215)\]

convert(x) returns x as a JLObjFloat64 (truncate it).

- **Signature**: `(Float)->%`

### `jfloat64` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L226)\]

jfloat64(x) returns x as a JLObjFloat64 (truncate it).

- **Signature**: `(Float)->%`

jfloat64(x) returns x as a JLObjFloat64 i.e. returns a 64 bits float in the memory area of Julia from the memory area of the underlying Common Lisp implementation.

- **Signature**: `(JLFloat64)->%`

jfloat64(str) evaluates str in Julia and returns the Julia Float64 object. For example:

**Example**:
```fricas
jfloat64("si n(pi)")
```

**Example**:
```fricas
jfloat64("0.7")
```

- **Signature**: `(String)->%`

### `jinf64` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L224)\]

jinf64() returns the Julia Infinity object.

- **Signature**: `()->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L183)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%)->Boolean`

jlApprox?(x,y,atol) computes inexact equality comparison with absolute tolerance atol. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%,%)->Boolean`

### `jlF64CApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L193)\]

jlF64CApply(func, x) applies the function pointer func to x. For example: 

**Example**:
```fricas
libm:= jlDlOpen "libopenli bm"
```

**Example**:
```fricas
squareRoot:=jlDlSym(libm,jsym(sqrt))
```

**Example**:
```fricas
jlF64CApply(squareRoot,jfloat64(2))
```

- **Signature**: `(JLObjDynamicLinker,%)->%`

jlF64CApply(func, x, y) applies the function pointer func to x and y. For example:

**Example**:
```fricas
libm:= jlDlOpen " libopenlibm"
```

**Example**:
```fricas
power:=jlDlSym(libm,jsym(pow))
```

**Example**:
```fricas
jlF64CApply(power,jfloat64(2),jfloat64(7))
```

- **Signature**: `(JLObjDynamicLinker,%,%)->%`

jlF64CApply(func, x, y, z) applies the function pointer func to x, y and z. For example with GSL-2.8:Example : gsl:= jlDlOpen "libgsl.so.28"

**Example**:
```fricas
hypot3:= jlDlSym(gsl,jsym(gsl_hypot3))
```

**Example**:
```fricas
jlF64CApply(hypot 3,jfloat64(2),jfloat64(7),jfloat64(9.0))
```

- **Signature**: `(JLObjDynamicLinker,%,%,%)->%`

### `jnan64` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L222)\]

jnan64() returns the Julia NaN (Not a Number) object.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
