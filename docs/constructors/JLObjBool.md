# JLObjBool

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L2)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia booleans, objects that are used within Julia, and not directly by the underlyingFriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argument for example.

**JLObjBool is a domain constructor.**  
**Abbreviation for JLObjBool is JOBBOOL**  
**This constructor is exposed in this frame.**  
**27 names for 33 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : % -> Boolean
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  jfalse : () -> %
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
 jtrue : () -> %                                        latex : % -> String
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nothing? : % -> Boolean                                string : % -> String
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L13)\]

coerce(jbool) coerces the Julia boolean value to a FriCAS Boolean.

- **Signature**: `(%)->Boolean`

### `jfalse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L18)\]

jfalse() returns the Julia boolean false value.

- **Signature**: `()->%`

### `jtrue` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L16)\]

jtrue() returns the Julia boolean true value.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
