# JLObjectType

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L3)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Category for arbitrary Julia objects, more precisely objects that are used within Julia.

**JLObjectType is a category constructor**  
**Abbreviation for JLObjectType is JOBTYPE**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : % -> JLObject
 coerce : % -> OutputForm                               convert : % -> String
 jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObject
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
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L92)\]

coerce(obj) coerces obj to JLObject. Convenience function.

- **Signature**: `(%)->JLObject`

### `jlAbout` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L74)\]

jlAbout(obj) displays Julia information about obj if you have About.jl installed in Julia. In the Julia interpreter, enter in the package management mode with ']', and issue: pkg> add "https://github.com/tecosaur/About.jl"

- **Signature**: `(%)->Void`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L51)\]

jlApply(func, obj) applies the function func with obj as parameter and returns the result as a FriCAS JLObject. For example, using JLMatrix(JLObjFloat64): 

**Example**:
```fricas
M:=nrand(4,4);
```

**Example**:
```fricas
jlApply("svd", M::JLMatrix(JL ObjFloat64)).S should be "equivalent" to svdvals(M).
```

- **Signature**: `(String,%)->JLObject`

jlApply(func, obj1, obj2) applies the function func with obj1 and obj2 as parameters and returns the result as a FriCAS JLObject. For example (equivalent to map(cos, v)): 

**Example**:
```fricas
v:=urand01(5)$JLVector(JLFloat)
```

Examp le: jlApply("map", "cos", coerce v)

- **Signature**: `(String,%,%)->JLObject`

jlApply(func, obj1, obj2, obj3) applies the function func with obj1, obj2 and obj3 as parameters and returns the result as a FriCAS JLObject.

- **Signature**: `(String,%,%,%)->JLObject`

jlApply(func, obj1, obj2, obj3, obj4) applies the function func with obj1, obj2, obj3 and obj4 as parameters and returns the result as a FriCAS JLObject.

- **Signature**: `(String,%,%,%,%)->JLObject`

jlApply(func, obj1, obj2, obj3, obj4, obj5) applies the function func with obj1, obj2, obj3, obj4 and obj5 asparameters and returns the result as a FriCAS JLObject.

- **Signature**: `(String,%,%,%,%,%)->JLObject`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L38)\]

jlDisplay(v) pretty prints v (à la Julia).

- **Signature**: `(%)->Void`

### `jlDump` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L30)\]

jlDump(obj) dumps (shows) the Julia object (internal structure with type).

- **Signature**: `(JLObject)->Void`

### `jlFieldNames` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L84)\]

jlFieldNames(obj) returns the field names of obj.

- **Signature**: `(%)->JLObject`

### `jlGetField` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L86)\]

jlGetField(obj, sym) returns the property/element sym of obj.

- **Signature**: `(%,JLSymbol)->JLObject`

### `jlGetJuliaIndex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L26)\]

jlGetJuliaIndex(obj) returns the Julia indexed dictionary index referencing the object obj. Convenience function for use in the interpreter.

- **Signature**: `(%)->String`

### `jlGetProperty` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L82)\]

jlGetProperty(obj, sym) returns the property/element sym of obj.

- **Signature**: `(%,JLSymbol)->JLObject`

### `jlId` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L18)\]

jlId(obj) returns the Julia indexed dictionary index referencing the object obj.

- **Signature**: `(%)->JLInt64`

### `jlObject` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L88)\]

jlObject() returns the internal Julia name of the Julia module used. For example:

**Example**:
```fricas
jlObject()$NMUniva riatePolynomial(NINT,'x)
```

- **Signature**: `()->String`

### `jlPropertyNames` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L80)\]

jlPropertyNames(obj) returns the property/element names of obj.

- **Signature**: `(%)->JLObject`

### `jlRef` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L23)\]

jlRef(obj) returns the internal Lisp representation of the Julia object obj.

- **Signature**: `(%)->SExpression`

jlref(str) evaluates the Julia command str and returns the corresponding FriCAS Julia mutable object.

- **Signature**: `(String)->%`

### `jlText` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L40)\]

jlText(obj, mimest) returns the text representation of obj as a list of String lines with mime subtype mimest, for example "plain" for "text/plain" or "html" for "text/html" if implemented at Julia level. Use internallythe 'show' method. For example: 

**Example**:
```fricas
df:=jdframe nrand(6,3)
```

**Example**:
```fricas
jlText(df, "plain")
```

**Example**:
```fricas
jlT ext(df, "html")
```

**Example**:
```fricas
jlText(df, "csv")
```

**Example**:
```fricas
jlText(df, "latex")
```

- **Signature**: `(%,String)->List(String)`

### `jlType` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L21)\]

jlType(obj) returns the Julia type of obj.

- **Signature**: `(%)->Symbol`

### `jlimref` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L14)\]

jlimref(str) evaluates the Julia command str and returns the corresponding FriCAS Julia immutable object wrapped in a Julia RefValueAny

- **Signature**: `(String)->%`

### `jlref` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L11)\]

jlRef(obj) returns the internal Lisp representation of the Julia object obj.

- **Signature**: `(%)->SExpression`

jlref(str) evaluates the Julia command str and returns the corresponding FriCAS Julia mutable object.

- **Signature**: `(String)->%`

### `missing?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L36)\]

missing?(obj) checks whether or not obj is missing.

- **Signature**: `(%)->Boolean`

### `mutable?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L32)\]

mutable?(obj) checks whether or not obj is mutable.

- **Signature**: `(%)->Boolean`

### `nothing?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L34)\]

nothing?(obj) checks whether or not obj is nothing.

- **Signature**: `(%)->Boolean`
---
[Back to Index](../index.md)
