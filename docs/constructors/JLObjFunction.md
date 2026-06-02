# JLObjFunction

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L868)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Functions, objects that are used within Julia, and not directly by the underlying FriCAS Common Lisp.

**JLObjFunction(f: Symbol) is a domain constructor**  
**Abbreviation for JLObjFunction is JOBFUNC**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : FunctionCalled(f) -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> InputForm                               convert : % -> String
 elt : (%, List(JLObject)) -> JLObject                  elt : (%, JLObject, JLObject) -> JLObject
 elt : (%, JLObject) -> JLObject                        jfunction : String -> %
 jfunction : FunctionCalled(f) -> %                     jlAbout : % -> Void
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
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nothing? : % -> Boolean                                string : % -> String
 ?~=? : (%, %) -> Boolean
 elt : (%, JLObject, JLObject, JLObject) -> JLObject
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L893)\]

coerce(func) returns a Julia Function from the FriCASfunction func. func must be compatible with 1D format.

- **Signature**: `(FunctionCalled(f))->%`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L880)\]

elt(jfunction, arg) returns result of applying the Julia Function jfunction to the argument arg. Use it as: `jfunction(arg)`.

- **Signature**: `(%,JLObject)->JLObject`

elt(jfunction, arg1, arg2) returns result of applying the Julia Function jfunction to the arguments arg1 and arg2. Use it as: `jfunction(arg1, arg2)`.

- **Signature**: `(%,JLObject,JLObject)->JLObject`

elt(jfunction, arg1, arg2, arg3) returns result of applying the Julia Function jfunction to the arguments arg1, arg2 and arg3. Use it as: `jfunction(arg1, arg2, arg3)`.

- **Signature**: `(%,JLObject,JLObject,JLObject)->JLObject`

elt(jfunction, args) returns result of applying the Julia Function jfunction to the arguments in the list args.

- **Signature**: `(%,List(JLObject))->JLObject`

### `jfunction` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L896)\]

jfunction(func) returns a Julia Function from the FriCAS function func. func must be compatible with 1D format. 

**Example**:
```fricas
f(x)==sin(x) In FriCAS: f(0.7) In Julia:
```

**Example**:
```fricas
ff:=jfunction(f)
```

**Example**:
```fricas
ff(jfloat 0.7)
```

- **Signature**: `(FunctionCalled(f))->%`

jfunction(str) returns a Julia Function from the String str. str must be a valid Julia Function definition.

- **Signature**: `(String)->%`
---
[Back to Index](../index.md)
