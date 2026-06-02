# JLObjAnonymousFunction

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L791)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Convenience domain for Julia Anonymous Functions, objects that are used within Julia, and not directly by theunderlying FriCAS Common Lisp.

**JLObjAnonymousFunction is a domain constructor.**  
**Abbreviation for JLObjAnonymousFunction is JOBANFUN**  
**This constructor is exposed in this frame.**  
**27 names for 37 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : AnonymousFunction -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  elt : (%, JLObject) -> JLObject
 elt : (%, JLObject, JLObject) -> JLObject              elt : (%, List(JLObject)) -> JLObject
 jafunction : AnonymousFunction -> %                    jafunction : String -> %
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
 latex : % -> String                                    missing? : % -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 string : % -> String                                   ?~=? : (%, %) -> Boolean
 elt : (%, JLObject, JLObject, JLObject) -> JLObject
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L814)\]

coerce(af) returns a Julia Anonymous Function from the AnonymousFunction af must be compatible with 1D format.

- **Signature**: `(AnonymousFunction)->%`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L800)\]

elt(jaf, arg) returns result of applying the Julia Anonymous Function jaf to the argument arg. Use it as: `jaf(arg)`.

- **Signature**: `(%,JLObject)->JLObject`

elt(jaf, arg1, arg2) returns result of applying the Julia Anonymous Function jaf to the arguments arg1 and arg2. Use it as: `jaf(arg1, arg2)`.

- **Signature**: `(%,JLObject,JLObject)->JLObject`

elt(jaf, arg1, arg2, arg3) returns result of applying the Julia Anonymous Function jaf to the arguments arg1,arg2 and arg3. Use it as: `jaf(arg1, arg2, arg3)`.

- **Signature**: `(%,JLObject,JLObject,JLObject)->JLObject`

elt(jaf, args) returns result of applying the Julia Anonymous Function jaf to the arguments in the list args.Use it as: `jaf(args)`.

- **Signature**: `(%,List(JLObject))->JLObject`

### `jafunction` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jimobject.spad#L817)\]

jafunction(af) returns a Julia Anonymous Function from the AnonymousFunction af must be compatible with 1D format. 

**Example**:
```fricas
f:=jafunction(x +-> sqrt sqrt(x^2))
```

**Example**:
```fricas
f(jfloat64 2.0)
```

**Example**:
```fricas
jlType %
```

- **Signature**: `(AnonymousFunction)->%`

jafunction(str) returns a Julia Anonymous Function from the evaluation of the Julia command str.

- **Signature**: `(String)->%`
---
[Back to Index](../index.md)
