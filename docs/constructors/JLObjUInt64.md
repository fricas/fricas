# JLObjUInt64

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L428)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia UInt64, 64 bits unsigned integers, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argument for example. Only basic arithmetic operations are supported. By default the hexadecimal notation is used. Use the juliaUI64Print to change it to false if the integer representation is required. Note that operations defined here check overflow imposing a performance penalty.

**JLObjUInt64 is a domain constructor.**  
**Abbreviation for JLObjUInt64 is JOBUI64**  
**This constructor is not exposed in this frame.**  
**78 names for 100 operations in this domain.**

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
 coerce : Integer -> %                                  coerce : NonNegativeInteger -> %
 coerce : % -> JLObject                                 coerce : % -> NonNegativeInteger
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> String                                  euclideanSize : % -> NonNegativeInteger
 ?exquo? : (%, %) -> Union(%,"failed")                  gcd : (%, %) -> %
 gcd : List(%) -> %                                     jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    juint64 : Integer -> %
 juint64 : NonNegativeInteger -> %                      juint64 : String -> %
 juliaUI64Print : Boolean -> Boolean                    latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     max : (%, %) -> %
 min : (%, %) -> %                                      missing? : % -> Boolean
 mutable? : % -> Boolean                                negative? : % -> Boolean
 nothing? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          plenaryPower : (%, PositiveInteger) -> %
 positive? : % -> Boolean                               ?quo? : (%, %) -> %
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sign : % -> Integer                                    sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            unit? : % -> Boolean
 unitCanonical : % -> %                                 zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
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

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L446)\]

coerce(z : NonNegativeInteger) returns z as a JLObjUInt64.

- **Signature**: `(NonNegativeInteger)->%`

coerce(i) coerces i to a NonNegativeInteger.

- **Signature**: `(%)->NonNegativeInteger`

### `juint64` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L454)\]

juint64(i) returns i as a JLObjUInt64 if possible.

- **Signature**: `(Integer)->%`

juint64(i) returns i as a JLObjUInt64.

- **Signature**: `(NonNegativeInteger)->%`

juint64(str) returns the Julia UInt64 from the str command.

- **Signature**: `(String)->%`

### `juliaUI64Print` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L450)\]

juliaUI64Print(bool) set the output mode. If set to false, use the FriCAS output mode. Otherwise use the Julia one, in hexadecimal notation (default).

- **Signature**: `(Boolean)->Boolean`
---
[Back to Index](../index.md)
