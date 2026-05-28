# JLObjR

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L651)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia R objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. The RCall package has to be installed in Julia. Note that the RCall Julia package uses a similar scheme for Julia R objects to the one of jlFriCAS for JLObjectType references.

**JLObjR is a domain constructor.**  
**Abbreviation for JLObjR is JOBR**  
**This constructor is exposed in this frame.**  
**32 names for 45 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : Float -> %
 coerce : Integer -> %                                  coerce : String -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRApply : (String, %) -> %                            jlRApply : (String, %, %) -> %
 jlRApply : (String, %, %, %) -> %                      jlRApply : (String, %, %, %, %) -> %
 jlRApply : (String, %, %, %, %, %) -> %                jlRConvert : (String, %) -> JLObject
 jlRCopy : % -> JLObject                                jlRDocumentation : String -> JLObject
 jlRLibrary : String -> Void                            jlRPrint : % -> Void
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jr : JLObject -> %
 jr : String -> %                                       latex : % -> String
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nothing? : % -> Boolean                                string : % -> String
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L697)\]

coerce(f) coerces f to a Julia R object.

- **Signature**: `(Float)->%`

coerce(i) coerces the integer i to a Julia R object.

- **Signature**: `(Integer)->%`

coerce(str) coerces the string str to a Julia R string object.

- **Signature**: `(String)->%`

### `jlRApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L665)\]

jlRApply(func, robj) applies the R function func form the module __main__ to the R object robj.

- **Signature**: `(String,%)->%`

jlRApply(func, robj1, robj2) applies the R function func form the module __main__ to the R objects robj1 and robj2.

- **Signature**: `(String,%,%)->%`

jlRApply(func, robj1, robj2, robj3) applies the R function func form the module __main__ to the R objects robj1, robj2 and robj3.

- **Signature**: `(String,%,%,%)->%`

jlRApply(func, robj1, robj2, robj3, robj4) applies the R function func form the module __main__ to the R objects robj1, robj2, robj3 and robj4.

- **Signature**: `(String,%,%,%,%)->%`

jlRApply(func, robj1, robj2, robj3, robj4, robj5) applies the R function func form the module __main__ to theR objects robj1, robj2, robj3, robj4 and robj5.

- **Signature**: `(String,%,%,%,%,%)->%`

### `jlRConvert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L683)\]

jlRConvert(type, robj) converts the R object robj to the Julia type 'type'.

- **Signature**: `(String,%)->JLObject`

### `jlRCopy` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L680)\]

jlRCopy(robj) returns a Julia object copy of the R object robj if possible.

- **Signature**: `(%)->JLObject`

### `jlRDocumentation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L686)\]

jlRDocumentation(str) displays the R documentation for the string str. Use 'q' to quit the help viewer.

- **Signature**: `(String)->JLObject`

### `jlRLibrary` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L689)\]

jlRLibrary(pkg) loads the R package 'pkg' in the Julia R environment.

- **Signature**: `(String)->Void`

### `jlRPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L695)\]

jlRPrint(robj) prints the R representation of robj.

- **Signature**: `(%)->Void`

### `jr` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L703)\]

jr(obj) returns obj as a Julia R object.

- **Signature**: `(JLObject)->%`

jr(str) coerces the string str to a Julia R object. Use jlDisplay(robj) to display the R object robj. For example: 

**Example**:
```fricas
jr "optim(0, $(x -> x-cos(x)), method='L-BFGS-B')"
```

**Example**:
```fricas
a:= nrand(10,4)
```

**Example**:
```fricas
b := j r(a::JLMatrix(JLObjFloat64))
```

**Example**:
```fricas
jlRApply("prcomp", b)
```

- **Signature**: `(String)->%`
---
[Back to Index](../index.md)
