# JLObjRational

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L575)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Rationals, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argument for example.

**JLObjRational is a domain constructor.**  
**Abbreviation for JLObjRational is JOBRAT**  
**This constructor is exposed in this frame.**  
**63 names for 90 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Integer) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      ?+? : (%, Integer) -> %
 ?+? : (Integer, %) -> %                                -? : % -> %
 ?-? : (%, %) -> %                                      ?-? : (%, Integer) -> %
 ?-? : (Integer, %) -> %                                ?/? : (%, %) -> %
 ?/? : (%, Integer) -> %                                ?/? : (Integer, %) -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              coerce : Fraction(Integer) -> %
 coerce : Integer -> %                                  coerce : JLObject -> %
 coerce : String -> %                                   coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> String                                  denominator : % -> Integer
 ?exquo? : (%, %) -> Union(%,"failed")                  inv : % -> %
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
 jrational : Fraction(Integer) -> %                     jrational : (Integer, Integer) -> %
 jrational : String -> %                                latex : % -> String
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     max : (%, %) -> %
 min : (%, %) -> %                                      missing? : % -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 numerator : % -> Integer                               one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          recip : % -> Union(%,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 smaller? : (%, %) -> Boolean                           string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            unit? : % -> Boolean
 unitCanonical : % -> %                                 zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L606)\]

coerce(p) returns a Julia Rational from the Fraction p.

- **Signature**: `(Fraction(Integer))->%`

coerce(a) returns a Julia Rational from the JLObject a.

- **Signature**: `(JLObject)->%`

coerce(str) returns a Julia Rational from the str command.

- **Signature**: `(String)->%`

### `denominator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L610)\]

denominator(p) returns the denominator of the rational p.

- **Signature**: `(%)->Integer`

### `jrational` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L616)\]

jrational(p) returns a Julia Rational from the Fraction p.

- **Signature**: `(Fraction(Integer))->%`

jrational(num, den) returns the Julia Rational from the num and den.

- **Signature**: `(Integer,Integer)->%`

jrational(str) returns the Julia Rational from the str command.

- **Signature**: `(String)->%`

### `numerator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L608)\]

numerator(p) returns the numerator of the rational p.

- **Signature**: `(%)->Integer`
---
[Back to Index](../index.md)
