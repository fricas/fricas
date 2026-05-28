# JLObjPy

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L520)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Python objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp. The PythonCall package has to be installed in Julia. Note that the PythonCall Julia package uses a similar scheme for Julia Python objects to the one of jlFriCAS for JLObjectType references.

**JLObjPy is a domain constructor.**  
**Abbreviation for JLObjPy is JOBPY**  
**This constructor is exposed in this frame.**  
**30 names for 47 operations in this domain.**

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
 jlPyApply : (String, %) -> %                           jlPyApply : (String, %, %) -> %
 jlPyApply : (String, %, %, %) -> %                     jlPyApply : (String, %, %, %, %) -> %
 jlPyApply : (String, %, %, %, %, %) -> %               jlPyApply : (String, String, %) -> %
 jlPyApply : (String, String, %, %) -> %                jlPyApply : (String, String, %, %, %) -> %
 jlPyApply : (String, String, %, %, %, %) -> %          jlPyConvert : (String, %) -> JLObject
 jlPyDocumentation : String -> Void                     jlPyImport : String -> %
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jpython : String -> %
 latex : % -> String                                    missing? : % -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 string : % -> String                                   ?~=? : (%, %) -> Boolean
 jlPyApply : (String, String, %, %, %, %, %) -> %
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L582)\]

coerce(f) coerces f to a Julia Python object.

- **Signature**: `(Float)->%`

coerce(i) coerces the integer i to a Julia Python object.

- **Signature**: `(Integer)->%`

coerce(str) coerces the string str to a Julia Python string object.

- **Signature**: `(String)->%`

### `jlPyApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L536)\]

jlPyApply(func, pyobj) applies the Python function func form the module __main__ to the Python object pyobj.

- **Signature**: `(String,%)->%`

jlPyApply(func, pyobj1, pyobj2) applies the Python function func form the module __main__ to the Python objects pyobj1 and pyobj2.

- **Signature**: `(String,%,%)->%`

jlPyApply(func, pyobj1, pyobj2, pyobj3) applies the Python function func form the module __main__ to the Python objects pyobj1, pyobj2 and pyobj3.

- **Signature**: `(String,%,%,%)->%`

jlPyApply(func, pyobj1, pyobj2, pyobj3, pyobj4) applies the Python function func form the module __main__ to the Python objects pyobj1, pyobj2, pyobj3 and pyobj4.

- **Signature**: `(String,%,%,%,%)->%`

jlPyApply(func, pyobj1, pyobj2, pyobj3, pyobj4, pyobj5) applies the Python function func form the module __main__ to the Python objects pyobj1, pyobj2, pyobj3, pyobj4 and pyobj5.

- **Signature**: `(String,%,%,%,%,%)->%`

jlPyApply(mod, func, pyobj) applies the Python function func from the module mod to the Python object pyobj. For example: Numpy needs to be installed, see the Julia package CondaPkg. 

**Example**:
```fricas
jlPyApply("numpy", "array", jpyobj("[1,2,3]")) creates a numpy array from the Python list [1,2,3]. jlPyApply("math", "sqrt", 2) computes the square root of 2.
```

- **Signature**: `(String,String,%)->%`

jlPyApply(mod, func, pyobj1, pyobj2) applies the Python function func from the module mod to the Python objects pyobj1 and pyobj2

- **Signature**: `(String,String,%,%)->%`

jlPyApply(mod, func, pyobj1, pyobj2, pyobj3) applies the Python function func from the module mod to the Python objects pyobj1, pyobj2 and pyobj3.

- **Signature**: `(String,String,%,%,%)->%`

jlPyApply(mod, func, pyobj1, pyobj2, pyobj3, pyobj4) applies the Python function func from the module mod to the Python objects pyobj1, pyobj2, pyobj3 and pyobj4.

- **Signature**: `(String,String,%,%,%,%)->%`

jlPyApply(mod, func, pyobj1, pyobj2, pyobj3, pyobj4, pyobj5) applies the Python function func from the modulemod to the Python objects pyobj1, pyobj2, pyobj3, pyobj4 and pyobj5.

- **Signature**: `(String,String,%,%,%,%,%)->%`

### `jlPyConvert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L573)\]

jlPyConvert(type, pyobj) converts the Python object pyobj to the Julia type 'type'.

- **Signature**: `(String,%)->JLObject`

### `jlPyDocumentation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L576)\]

jlPyDocumentation(obj) displays the Python documentation for the Python object obj. Use 'q' followed by ENTERto quit.

- **Signature**: `(String)->Void`

### `jlPyImport` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L579)\]

jlPyImport(mod) imports the Python module mod in the Julia Python environment.

- **Signature**: `(String)->%`

### `jpython` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L588)\]

jpython(str) coerces the string str to a Julia Python object. Use jlDisplay(pyobj) to display the Python object pyobj.

- **Signature**: `(String)->%`
---
[Back to Index](../index.md)
