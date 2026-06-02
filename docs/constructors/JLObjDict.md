# JLObjDict

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L415)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Dictionaries, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It is not meant to be used directly, but rather for returned value or function argument for example.

**JLObjDict is a domain constructor.**  
**Abbreviation for JLObjDict is JOBDICT**  
**This constructor is exposed in this frame.**  
**39 names for 54 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?=? : (%, %) -> Boolean
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  copy : % -> %
 delete! : (%, Any) -> JLObject                         delete! : (%, JLObject) -> JLObject
 delete! : (%, JLSymbol) -> JLObject                    elt : (%, Any) -> JLObject
 elt : (%, Integer) -> JLObject                         elt : (%, JLObject) -> JLObject
 elt : (%, JLSymbol) -> JLObject                        empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 jdict : String -> %                                    jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    latex : % -> String
 less? : (%, NonNegativeInteger) -> Boolean             missing? : % -> Boolean
 more? : (%, NonNegativeInteger) -> Boolean             mutable? : % -> Boolean
 nothing? : % -> Boolean                                qelt : (%, Any) -> JLObject
 qelt : (%, Integer) -> JLObject                        qelt : (%, JLObject) -> JLObject
 qelt : (%, JLSymbol) -> JLObject                       sample : () -> %
 setelt! : (%, Any, JLObject) -> %                      setelt! : (%, JLObject, JLObject) -> %
 setelt! : (%, JLSymbol, JLObject) -> %                 size? : (%, NonNegativeInteger) -> Boolean
 string : % -> String                                   ?~=? : (%, %) -> Boolean
```

## Operations added

### `delete!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L455)\]

delete!(dict, key) deletes the mapping corresponding to the the key of dict.

- **Signature**: `(%,Any)->JLObject`
- **Signature**: `(%,JLObject)->JLObject`
- **Signature**: `(%,JLSymbol)->JLObject`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L426)\]

elt(dict, key) returns the value associated to the key of dict.

- **Signature**: `(%,Any)->JLObject`

elt(dict, ind) returns the element at index ind of dict. It can be used, for example, with a Tuple or a Vector.

- **Signature**: `(%,JLObject)->JLObject`

elt(dict, sym) returns the value associated to the key sym of dict.

- **Signature**: `(%,JLSymbol)->JLObject`

### `jdict` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L464)\]

jdict(str) returns a Julia Dict from the Julia command str. For example: 

**Example**:
```fricas
jdict("Dict([(_"A_", 1), (_ "B_", 2)])")."B"
```

- **Signature**: `(String)->%`

### `qelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L428)\]

qelt(dict, key) returns the value associated to the key of dict. No checks are done regarding the existence of the key at the FriCAS level. Returns the Julia missing value if the key does not exist.

- **Signature**: `(%,Any)->JLObject`

qelt(dict, ind) returns the element at index ind of dict. It can be used, for example, with a Tuple or a Vector.

- **Signature**: `(%,JLObject)->JLObject`

qelt(dict, sym) returns the value associated to the key sym of dict. No checks are done regarding the existence of the key sym at the FriCAS level. Returns the Julia missing value if the key does not exist.

- **Signature**: `(%,JLSymbol)->JLObject`

### `setelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L446)\]

setelt!(dict, key, val) sets the val to the key of dict. If the key does not exist it will be created.

- **Signature**: `(%,Any,JLObject)->%`
- **Signature**: `(%,JLSymbol,JLObject)->%`

setelt!(dict, ind, val) sets the val to the index ind of dict. If the index does not exist it will be created.

- **Signature**: `(%,JLObject,JLObject)->%`
---
[Back to Index](../index.md)
