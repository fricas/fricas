# JLObjFloat32

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L31)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Float32, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather, for returned value or function argument for example. Only basic arithmetic operations are supported.

**JLObjFloat32 is a domain constructor.**  
**Abbreviation for JLObjFloat32 is JOBF32**  
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
 cis : % -> JLObjComplexF32                             cispi : % -> JLObjComplexF32
 coerce : % -> %                                        coerce : DoubleFloat -> %
 coerce : Float -> %                                    coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : Float -> %
 convert : % -> String                                  euclideanSize : % -> NonNegativeInteger
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 inv : % -> %                                           jfloat32 : Float -> %
 jfloat32 : JLFloat32 -> %                              jfloat32 : String -> %
 jinf32 : () -> %                                       jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApprox? : (%, %) -> Boolean
 jlApprox? : (%, %, %) -> Boolean                       jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlF32CApply : (JLObjDynamicLinker, %) -> %
 jlF32CApply : (JLObjDynamicLinker, %, %) -> %          jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jnan32 : () -> %
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
 jlF32CApply : (JLObjDynamicLinker, %, %, %) -> %
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `cis` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L75)\]

cis(x) returns exp(%i*x) computed efficiently.

- **Signature**: `(%)->JLObjComplexF32`

### `cispi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L77)\]

cispi(x) returns cis(%pi*x) computed efficiently.

- **Signature**: `(%)->JLObjComplexF32`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L85)\]

coerce(x) returns x as a JLObjFloat32 (truncate it). Convenience function.

- **Signature**: `(DoubleFloat)->%`
- **Signature**: `(Float)->%`

### `convert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L83)\]

convert(x) returns x as a JLObjFloat32 (truncate it).

- **Signature**: `(Float)->%`

### `jfloat32` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L91)\]

jfloat32(x) returns x as a JLObjFloat32 (truncate it).

- **Signature**: `(Float)->%`
- **Signature**: `(JLFloat32)->%`

jfloat32(str) evaluates str in Julia and returns the Julia Float32 object. For example:

**Example**:
```fricas
jfloat32("si n(pi)")
```

**Example**:
```fricas
jfloat32("0.7")
```

- **Signature**: `(String)->%`

### `jinf32` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L81)\]

jinf32() returns the Julia Infinity object.

- **Signature**: `()->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L51)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%)->Boolean`

jlApprox?(x,y,atol) computes inexact equality comparison with absolute tolerance atol. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%,%)->Boolean`

### `jlF32CApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L61)\]

jlF32CApply(func, x) applies the function pointer func to x. For example: 

**Example**:
```fricas
libm:= jlDlOpen "libopenli bm"
```

**Example**:
```fricas
squareRoot:=jlDlSym(libm,jsym(sqrtf))
```

**Example**:
```fricas
jlF32CApply(squareRoot,jfloat32(2))
```

- **Signature**: `(JLObjDynamicLinker,%)->%`

jlF32CApply(func, x, y) applies the function pointer func to x and y. For example:

**Example**:
```fricas
libm:= jlDlOpen " libopenlibm"
```

**Example**:
```fricas
power:=jlDlSym(libm,jsym(powf))
```

**Example**:
```fricas
jlF32CApply(power,jfloat32(2),jfloat32(7))
```

- **Signature**: `(JLObjDynamicLinker,%,%)->%`

jlF32CApply(func, x, y, z) applies the function pointer func to x, y and z.

- **Signature**: `(JLObjDynamicLinker,%,%,%)->%`

### `jnan32` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L79)\]

jnan32() returns the Julia NaN (Not a Number) object.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
