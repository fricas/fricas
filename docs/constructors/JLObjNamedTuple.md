# JLObjNamedTuple

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L752)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Named Tuples, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argument for example.

**JLObjNamedTuple is a domain constructor.**  
**Abbreviation for JLObjNamedTuple is JOBNTPL**  
**This constructor is exposed in this frame.**  
**38 names for 45 operations in this domain.**

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
 jlref : String -> %                                    jnamedtuple : String -> %
 jntuple : Tuple(Any) -> %                              latex : % -> String
 less? : (%, NonNegativeInteger) -> Boolean             missing? : % -> Boolean
 more? : (%, NonNegativeInteger) -> Boolean             mutable? : % -> Boolean
 nothing? : % -> Boolean                                qelt : (%, Integer) -> JLObject
 qelt : (%, JLSymbol) -> JLObject                       sample : () -> %
 size? : (%, NonNegativeInteger) -> Boolean             string : % -> String
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `jnamedtuple` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L766)\]

jnamedtuple(str) returns a Julia Named Tuple from the evaluation of the Julia command str. To avoid bad function selection in the interpreter we rename jntuple to jnamedtuple.

- **Signature**: `(String)->%`

### `jntuple` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L763)\]

jntuple(lst) returns a Julia Named Tuple from the List lst. lst must be compatible with 1D format.

- **Signature**: `(Tuple(Any))->%`
---
[Back to Index](../index.md)
