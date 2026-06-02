# NMCommutativeRing

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L48)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

Parent category of Nemo commutative ring domains.

**NMCommutativeRing is a category constructor**  
**Abbreviation for NMCommutativeRing is NCRING**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 0 : () -> %                                            1 : () -> %
 ?=? : (%, %) -> Boolean                                ?^? : (%, PositiveInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> String                                  equal? : (%, %) -> Boolean
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlId : % -> JLInt64                                    jlNMRing : () -> String
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    latex : % -> String
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     missing? : % -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               recip : % -> Union(%,"failed")
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 unit? : % -> Boolean                                   zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
```

## Operations added
---
[Back to Index](../index.md)
