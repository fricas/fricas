# JLObjBigInt

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L332)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia BigInt, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argument forexample. It is also not meant to replace NMInteger, which uses GMP, but basic arithmetic operations are supported. Use the GMP library: https://gmplib.org/

**JLObjBigInt is a domain constructor.**  
**Abbreviation for JLObjBigInt is JOBBINT**  
**This constructor is exposed in this frame.**  
**78 names for 99 operations in this domain.**

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
 ?>=? : (%, %) -> Boolean                               ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        abs : % -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> String                                  euclideanSize : % -> NonNegativeInteger
 ?exquo? : (%, %) -> Union(%,"failed")                  gcd : (%, %) -> %
 gcd : List(%) -> %                                     jbigint : Integer -> %
 jbigint : String -> %                                  jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlBICApply : (JLObjDynamicLinker, %) -> %
 jlBICApply : (JLObjDynamicLinker, %, %) -> %           jlBICApply : (JLObjDynamicLinker, %, %, %) -> %
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlId : % -> JLInt64                                    jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 max : (%, %) -> %                                      min : (%, %) -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 negative? : % -> Boolean                               nothing? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               positive? : % -> Boolean
 ?quo? : (%, %) -> %                                    recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sign : % -> Integer
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
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

### `jbigint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L360)\]

jbigint(z) converts z to a Julia BigInt.

- **Signature**: `(Integer)->%`

jbigint(z) returns the string str evaluated in Julia where the returned value is assumed to be a Julia BigInt.

**Example**:
```fricas
jbigint "BigInt(1234567890)"
```

- **Signature**: `(String)->%`

### `jlBICApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L348)\]

jlBICApply(func,x) applies the function pointer func to x. It is expected that the C function modifies the first parameters for the returned value (provided by FriCAS and returned here).

- **Signature**: `(JLObjDynamicLinker,%)->%`

jlBICApply(func,x,y) applies the function pointer func to x and y. It is expected that the C function modifies the first parameters for the returned value (provided by FriCAS and returned here).

- **Signature**: `(JLObjDynamicLinker,%,%)->%`

jlBICApply(func,x,y,z) applies the function pointer func to x, y and z. It is expected that the C function modifies the first parameters for the returned value (provided by FriCAS and returned here).

- **Signature**: `(JLObjDynamicLinker,%,%,%)->%`
---
[Back to Index](../index.md)
