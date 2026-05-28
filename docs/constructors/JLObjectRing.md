# JLObjectRing

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L218)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Category of rings whose elements are of type JLOjectType.

**JLObjectRing is a category constructor**  
**Abbreviation for JLObjectRing is JOBRING**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (NMInteger, %) -> JLObject                       ?*? : (NonNegativeInteger, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (%, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?+? : (%, %) -> %
 -? : % -> %                                            ?-? : (%, %) -> %
 0 : () -> %                                            1 : () -> %
 ?=? : (%, %) -> Boolean                                ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> String
 jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlId : % -> JLInt64                                    jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 latex : % -> String                                    leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nothing? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          recip : % -> Union(%,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
```

## Operations added
---
[Back to Index](../index.md)
