# JLObjInt64

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L518)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Int64, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argument for example. Only basic arithmetic operations are supported.

**JLObjInt64 is a domain constructor.**  
**Abbreviation for JLObjInt64 is JOBI64**  
**This constructor is exposed in this frame.**  
**77 names for 97 operations in this domain.**

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
 coerce : Integer -> %                                  coerce : % -> Integer
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> String
 euclideanSize : % -> NonNegativeInteger                ?exquo? : (%, %) -> Union(%,"failed")
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 jint64 : Integer -> %                                  jint64 : String -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %, %, %) -> JLObject
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

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L532)\]

coerce(i) coerces i to an Integer.

- **Signature**: `(%)->Integer`

### `jint64` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L534)\]

jint64(i) returns i as a JLObjInt64.

- **Signature**: `(Integer)->%`

jint64(str) returns the Julia Int64 from the str command.

- **Signature**: `(String)->%`
---
[Back to Index](../index.md)
