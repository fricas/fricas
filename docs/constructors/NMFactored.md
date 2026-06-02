# NMFactored

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1909)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

Nemo factored objects.

**NMFactored(R: Join(NMRing,IntegralDomain)) is a domain constructor**  
**Abbreviation for NMFactored is NFR**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : % -> JLObject
 coerce : % -> OutputForm                               convert : % -> String
 factor : R -> Factored(R)                              factor : R -> %
 factorSquareFree : R -> Factored(R)                    factorSquareFree : R -> %
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
 string : % -> String                                   unit : % -> R
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `factor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1923)\]

factor(intd) returns the factorization of intd.

- **Signature**: `(R)->%`

factor(intd) returns the factorization of intd. Currently its printed OutputForm can be difficult to "interpret". Use factorList(factor(intd)@Factored(%)) to have a more understandable representation.

- **Signature**: `(R)->Factored(R)`

### `factorSquareFree` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1932)\]

factorSquareFree(intd) returns the factorization of intd.

- **Signature**: `(R)->%`
- **Signature**: `(R)->Factored(R)`

### `unit` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1936)\]

unit(fr) returns the unit part of the factorization.

- **Signature**: `(%)->R`
---
[Back to Index](../index.md)
