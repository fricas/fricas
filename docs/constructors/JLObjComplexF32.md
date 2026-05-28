# JLObjComplexF32

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L300)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia ComplexF32, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argumentfor example. Only basic arithmetic operations are supported.

**JLObjComplexF32 is a domain constructor.**  
**Abbreviation for JLObjComplexF32 is JOBCF32**  
**This constructor is not exposed in this frame.**  
**74 names for 99 operations in this domain.**

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
 ?=? : (%, %) -> Boolean                                ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              cis : % -> %
 cispi : % -> %                                         coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> String                                  euclideanSize : % -> NonNegativeInteger
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 inv : % -> %                                           jcfloat32 : String -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> %
 jlApply : (String, %, %) -> %                          jlApply : (String, %, %, %) -> %
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     missing? : % -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sizeLess? : (%, %) -> Boolean
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 unit? : % -> Boolean                                   unitCanonical : % -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `cis` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L325)\]

cis(x) returns exp(%i*x) computed efficiently.

- **Signature**: `(%)->%`

### `cispi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L327)\]

cispi(x) returns cis(%pi*x) computed efficiently.

- **Signature**: `(%)->%`

### `jcfloat32` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L329)\]

jcfloat32(str) evaluates str in Julia and returns the Julia ComplexFloat32 object. For example:

**Example**:
```fricas
jcfl oat32 "cos(2pi)im"
```

- **Signature**: `(String)->%`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L318)\]

jlApply(func, x) applies the function func with x as parameter.

- **Signature**: `(String,%)->%`

jlApply(func, x, y) applies the function func to x and y.

- **Signature**: `(String,%,%)->%`

jlApply(func, x, y, z) applies the function func to x, y and z.

- **Signature**: `(String,%,%,%)->%`
---
[Back to Index](../index.md)
