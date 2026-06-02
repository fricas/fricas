# JLObject

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L259)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Generic Julia objects i.e. objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. It also supports the JLObjectAggregate category.

**JLObject is a domain constructor.**  
**Abbreviation for JLObject is JOBJECT**  
**This constructor is exposed in this frame.**  
**44 names for 55 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?=? : (%, %) -> Boolean
 coerce : Integer -> %                                  coerce : JLSymbol -> %
 coerce : String -> %                                   coerce : % -> JLObject
 coerce : % -> OutputForm                               convert : % -> String
 copy : % -> %                                          elt : (%, Integer) -> JLObject
 elt : (%, JLSymbol) -> JLObject                        empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 isa? : (%, %) -> Boolean                               jany : () -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %, %, %) -> JLObject
 jlCollect : % -> JLVector(%)                           jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jmissing : () -> %
 jnothing : () -> %                                     jobject : Integer -> %
 jobject : String -> %                                  jstring : String -> %
 junion : List(%) -> %                                  latex : % -> String
 less? : (%, NonNegativeInteger) -> Boolean             missing? : % -> Boolean
 more? : (%, NonNegativeInteger) -> Boolean             mutable? : % -> Boolean
 nothing? : % -> Boolean                                qelt : (%, Integer) -> JLObject
 qelt : (%, JLSymbol) -> JLObject                       sample : () -> %
 size? : (%, NonNegativeInteger) -> Boolean             string : % -> String
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L273)\]

coerce(i) coerces i as a JLObject. Convenience function.

- **Signature**: `(Integer)->%`

coerce(x): convenience function.

- **Signature**: `(JLSymbol)->%`

coerce(str) coerces str as a JLObject. See 'jobject(String)' for more information. Convenience function.

- **Signature**: `(String)->%`

### `isa?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L271)\]

isa?(x, y) returns true if x is a subtype of y.

- **Signature**: `(%,%)->Boolean`

### `jany` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L285)\]

jany() returns the JLObject representing Any in Julia.

- **Signature**: `()->%`

### `jlCollect` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L282)\]

jlCollect(obj) collects element of a Julia iterator in a JLVector.

- **Signature**: `(%)->JLVector(%)`

### `jmissing` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L287)\]

jmissing() returns the JLObject representing Missing in Julia.

- **Signature**: `()->%`

### `jnothing` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L289)\]

jnothing() returns the JLObject representing Nothing in Julia.

- **Signature**: `()->%`

### `jobject` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L296)\]

jobject(n) returns the JLObject representing the integer n.

- **Signature**: `(Integer)->%`

jobject(str) constructs an arbitrary Julia object depending on string used (evaluated by Julia). For example:

**Example**:
```fricas
jobject("rand(Float64,(2,3,4))") returns a 2x3x4 array of Float64 elements. A Julia exception is ra ised if error(s) occur(s) during parsing/evaluation.
```

- **Signature**: `(String)->%`

### `jstring` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L293)\]

jstring(str) returns the JLObject representing the String str. Convenience function.

- **Signature**: `(String)->%`

### `junion` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L291)\]

junion(l) returns the JLObject representing the union of the types in l.

- **Signature**: `(List(%))->%`
---
[Back to Index](../index.md)
