# JLObjectAggregate

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L173)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Category for arbitrary Julia aggregates.

**JLObjectAggregate is a category constructor**  
**Abbreviation for JLObjectAggregate is JOBAGG**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?=? : (%, %) -> Boolean
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  copy : % -> %
 elt : (%, Integer) -> JLObject                         elt : (%, JLSymbol) -> JLObject
 empty : () -> %                                        empty? : % -> Boolean
 eq? : (%, %) -> Boolean                                jlAbout : % -> Void
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %) -> JLObject
 jlApply : (String, %) -> JLObject                      jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    latex : % -> String
 less? : (%, NonNegativeInteger) -> Boolean             missing? : % -> Boolean
 more? : (%, NonNegativeInteger) -> Boolean             mutable? : % -> Boolean
 nothing? : % -> Boolean                                qelt : (%, Integer) -> JLObject
 qelt : (%, JLSymbol) -> JLObject                       sample : () -> %
 size? : (%, NonNegativeInteger) -> Boolean             string : % -> String
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L180)\]

elt(obj, ind) returns the element at index ind of obj. It can be used, for example, with a Tuple or a Vector.

- **Signature**: `(%,Integer)->JLObject`

elt(obj, sym) returns the property/element sym of obj. For example: 

**Example**:
```fricas
ret := jlApply("svd", jobject("r and(4,4)")); ret.U
```

- **Signature**: `(%,JLSymbol)->JLObject`

### `qelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L184)\]

qelt(obj, ind) returns the element at index ind of obj. It can be used, for example, with a Tuple or a Vector.

- **Signature**: `(%,Integer)->JLObject`

qelt(obj, sym) returns the property/element sym of obj. No checks are done regarding the existence of the symproperty. If it does not exist, Julia throws an error.

- **Signature**: `(%,JLSymbol)->JLObject`
---
[Back to Index](../index.md)
