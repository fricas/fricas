# NMType

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L5)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

Parent category of Nemo domains.

**NMType is a category constructor**  
**Abbreviation for NMType is NTYPE**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : % -> JLObject
 coerce : % -> OutputForm                               convert : % -> String
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
 latex : % -> String                                    missing? : % -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 string : % -> String                                   ?~=? : (%, %) -> Boolean
```

## Operations added
---
[Back to Index](../index.md)
