# JLObjTuple

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L715)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Tuples, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argument forexample.

**JLObjTuple is a domain constructor.**  
**Abbreviation for JLObjTuple is JOBTPLE**  
**This constructor is exposed in this frame.**  
**37 names for 45 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?=? : (%, %) -> Boolean
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  copy : % -> %
 elt : (%, Integer) -> JLObject                         elt : (%, JLSymbol) -> JLObject
 empty : () -> %                                        empty? : % -> Boolean
 eq? : (%, %) -> Boolean                                jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jtuple : List(Any) -> %
 jtuple : String -> %                                   latex : % -> String
 less? : (%, NonNegativeInteger) -> Boolean             missing? : % -> Boolean
 more? : (%, NonNegativeInteger) -> Boolean             mutable? : % -> Boolean
 nothing? : % -> Boolean                                qelt : (%, Integer) -> JLObject
 qelt : (%, JLSymbol) -> JLObject                       sample : () -> %
 size? : (%, NonNegativeInteger) -> Boolean             string : % -> String
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `jtuple` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L726)\]

jtuple(lst) returns a Julia Tuple from the List lst. lst must be compatible with 1D format.

- **Signature**: `(List(Any))->%`

jtuple(str) returns a Julia Tuple from the evaluation of the Julia command str.

- **Signature**: `(String)->%`
---
[Back to Index](../index.md)
