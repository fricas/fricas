# JLObjPair

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L682)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Pairs, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argument for example.

**JLObjPair(F: JLObjectType,L: JLObjectType) is a domain constructor**  
**Abbreviation for JLObjPair is JOBPAIR**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?=? : (%, %) -> Boolean
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  copy : % -> %
 elt : (%, Integer) -> JLObject                         elt : (%, JLSymbol) -> JLObject
 empty : () -> %                                        empty? : % -> Boolean
 eq? : (%, %) -> Boolean                                first : % -> F
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
 jpair : String -> %                                    last : % -> L
 latex : % -> String                                    less? : (%, NonNegativeInteger) -> Boolean
 missing? : % -> Boolean                                more? : (%, NonNegativeInteger) -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 qelt : (%, Integer) -> JLObject                        qelt : (%, JLSymbol) -> JLObject
 sample : () -> %                                       size? : (%, NonNegativeInteger) -> Boolean
 string : % -> String                                   ?~=? : (%, %) -> Boolean
```

## Operations added

### `first` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L694)\]

first(pair) returns the first element of pair.

- **Signature**: `(%)->F`

### `jpair` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L698)\]

jpair(str) returns a Julia Pair from the evaluation of the Julia command str. For example:

**Example**:
```fricas
jpair("_" a_"=>7")$JLObjPair(JLObject,JLObjInt64)
```

- **Signature**: `(String)->%`

### `last` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L696)\]

last(pair) returns the last element of pair.

- **Signature**: `(%)->L`
---
[Back to Index](../index.md)
