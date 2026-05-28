# JLObjDynamicLinker

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L328)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Generic Julia objects used for dynamically linking shared libraries.

**JLObjDynamicLinker is a domain constructor.**  
**Abbreviation for JLObjDynamicLinker is JOBDLINK**  
**This constructor is exposed in this frame.**  
**33 names for 47 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : % -> JLObject
 coerce : % -> OutputForm                               convert : % -> String
 jdlink : String -> %                                   jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDlCApply : (%, JLObject) -> JLObject                 jlDlCApply : (%, JLObject, JLObject) -> JLObject
 jlDlClose : % -> Boolean                               jlDlFindLib : String -> String
 jlDlList : () -> JLObject                              jlDlOpen : String -> %
 jlDlOpen : (String, Boolean) -> %                      jlDlOpen : (String, JLObject) -> %
 jlDlOpen : (String, JLObject, Boolean) -> %            jlDlPath : % -> String
 jlDlPath : String -> String                            jlDlSym : (%, JLSymbol) -> %
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
 jlDlCApply : (%, JLObject, JLObject, JLObject) -> JLObject
 jlDlCApply : (%, String, JLObject, String) -> JLObject
 jlDlCApply : (%, String, JLObject, String, JLObject, String) -> JLObject
 jlDlCApply : (%, String, JLObject, String, JLObject, String, JLObject, String) -> JLObject
```

## Operations added

### `jdlink` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L473)\]

jdlink(lib) opens/loads the dynamic shared library lib and returns a reference pointer that can be used to obtain addresses of its exported symbols using jlDlSym. It is up to the user to unload the shared object when itis no longer necessary using jlDlClose. 

**Example**:
```fricas
libm := jdlink "libopenlibm" Throws a Julia error if it is not found. Use jlDlOpen for a version with options and/or without Julia errors.
```

- **Signature**: `(String)->%`

### `jlDlCApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L340)\]

jlDlCApply(func,x) applies the function pointer func to x. The Julia type of x must be compatible with the Libdl Julia module. Type of the returned value is assumed to be the same as the type of x. For example:

**Example**:
```fricas
libm:= jlDlOpen "libopenlibm"
```

**Example**:
```fricas
squareRoot:=jlDlSym(libm,jsym(sqrt))
```

**Example**:
```fricas
jlDlCApply(squareRoo t,jobject("2."))
```

- **Signature**: `(%,JLObject)->JLObject`

jlDlCApply(func,x,y) applies the function pointer func to x and y. Type of the returned value is assumed to be the same as the type of x. For example: 

**Example**:
```fricas
libgsl:= jlDlOpen "libgsl"
```

**Example**:
```fricas
ipower:=jlDlSym(libgs l, gsl_pow_int)
```

**Example**:
```fricas
jlDlCApply(ipower,jobject("2.0"),jobject("7"))
```

- **Signature**: `(%,JLObject,JLObject)->JLObject`

jlDlCApply(func,x,y,z) applies the function pointer func to x, y and z. Returned value is assumed the same type as type of x. Use jlDlCApply with type options if necessary. For example with GSL:

**Example**:
```fricas
gsl:= jlDlOpen "libgsl"
```

**Example**:
```fricas
hypot3:= jlDlSym(gsl,jsym(gsl_hypot3))
```

**Example**:
```fricas
jlDlCApply(hypot3,jobject("2."),jobject( "7."),jobject("9.0"))
```

**Example**:
```fricas
jlDlCApply(hypot3,jfloat64("2."),jfloat64("7."),9.0::JLObjFloat64)
```

- **Signature**: `(%,JLObject,JLObject,JLObject)->JLObject`

jlDlCApply(func, ctype, x, xctype) applies the function pointer func to x given its Julia C type (xctype) or its supported Julia type, for example "Float64" or "Cdouble". Returned value is assumed to be a C type ctype (or any Julia supported types). For example: 

**Example**:
```fricas
libm:= jlDlOpen "libopenlibm"
```

**Example**:
```fricas
squareRoot:= jlD lSym(libm,jsym(sqrt))
```

**Example**:
```fricas
jlDlCApply(squareRoot, "Cdouble", jobject("2."), "Cdouble")
```

**Example**:
```fricas
sinus: =jlDlSym(libm,jsym(sin))
```

**Example**:
```fricas
jlDlCApply(sinus,"Float64", jobject("2."), "Float64")
```

- **Signature**: `(%,String,JLObject,String)->JLObject`

jlDlCApply(func, ctype, x, xctype, y, yctype) applies the function pointer func to x and y given their Julia C type (xctype and yctype) or their supported Julia type, for example "Float64" or "Cdouble". Returned value is assumed to be a C type ctype (or any Julia supported types).

- **Signature**: `(%,String,JLObject,String,JLObject,String)->JLObject`

jlDlCApply(func, ctype, x, xctype, y, yctype, z, zctype) applies the function pointer func to x, y and z given its Julia C type (xctype, yctype and zctype) or its supported Julia type, for example "Float64" or "Cdouble". Returned value is assumed to be a C type ctype (or any Julia supported types).

- **Signature**: `(%,String,JLObject,String,JLObject,String,JLObject,String)->JLObject`

### `jlDlClose` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L451)\]

jlDlClose(libPtr) unloads explicitly the shared library. Returns true if the shared object was successfully unloaded, false otherwise. 

**Example**:
```fricas
jlDlClose(lib)
```

- **Signature**: `(%)->Boolean`

### `jlDlFindLib` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L470)\]

jlDlFindLib(lib) returns the library lib. Returns an empty string if it is not found.

- **Signature**: `(String)->String`

### `jlDlList` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L461)\]

jlDlList() returns the list of loaded shared objects by the current process in a JLObject referencing a Juliavector of strings.

- **Signature**: `()->JLObject`

### `jlDlOpen` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L389)\]

jlDlOpen(lib) opens/loads the dynamic shared library lib and returns a reference pointer that can be used to obtain addresses of its exported symbols using jlDlSym. It is up to the user to unload the shared object when it is no longer used. Does not throw an error if the library cannot be loaded and returns 'nothing' in this case. Use the nothing? operation on the returned value if necessary.

- **Signature**: `(String)->%`

jlDlOpen(lib, throw) opens/loads the dynamic shared library lib and returns a reference pointer that can be used to obtain addresses of its exported symbols using jlDlSym. It is up to the user to unload the shared object when it is no longer used. 'throw' determines whether or not Julia throws an error if the library cannot be loaded. 

**Example**:
```fricas
jlDlOpen("libopenlibm", true)
```

- **Signature**: `(String,Boolean)->%`

jlDlOpen(lib, flags) opens/loads the dynamic shared library lib and returns a reference pointer that can be used to obtain addresses of its exported symbols using jlDlSym. It is up to the user to unload the shared object when it is no longer used. Does not throw an error if the library cannot be loaded by default and returns nothing in this case. Use the nothing? operation on the returned value if necessary. The Julia module Libdl mustbe loaded/imported before using this function. 

**Example**:
```fricas
jlUsing "Libdl" -- for flags
```

**Example**:
```fricas
jlDlOpen("lib openlibm", jobject "Libdl.RTLD_NOW") flags can be one or combination of: RTLD_DEEPBIND RTLD_FIRST RTLD_GLOBAL RTLD_LAZY RTLD_LOCAL RTLD_NODELETE RTLD_NOLOAD RTLD_NOW Note: the Julia module Libdl must be loaded or imported before using these flags.
```

- **Signature**: `(String,JLObject)->%`

jlDlOpen(lib, flags, throw) opens/loads the dynamic shared library lib and returns a reference pointer that can be used to obtain addresses of its exported symbols using jlDlSym. It is up to the user to unload the shared object when it is no longer used. Does not throw an error if the library cannot be loaded by default and returns nothing in this case. Use the nothing? operation on the returned value if necessary. 'throw' determines whether or not Julia throws an error if the library cannot be loaded. The Julia module must be be loaded/imported before using this function. 

**Example**:
```fricas
jlDlOpen("libopenlibm", jobject "Libdl.RTLD_NOW", false) flags can be one or combination of: RTLD_DEEPBIND RTLD_FIRST RTLD_GLOBAL RTLD_LAZY RTLD_LOCAL RTLD_NODELETE RTLD_NOLOAD RTLD_NOW Note: the Julia module Libdl must be loaded or imported before using these flags.
```

- **Signature**: `(String,JLObject,Boolean)->%`

### `jlDlPath` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L465)\]

jlDlPath(lib) returns the path of the loaded library lib.

- **Signature**: `(%)->String`

jlDlPath(lib) returns the path of the library lib. Julia warns if it is not found and returns an empty string.

- **Signature**: `(String)->String`

### `jlDlSym` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L456)\]

jlDlSym(lib, sym) returns the pointer to the symbol sym in the library referenced by lib. See jlDlOpen.Examp le: lnk:=jdlink "libjulia"

**Example**:
```fricas
sym := jlDlSym(lnk,"jl_eval_string")
```

- **Signature**: `(%,JLSymbol)->%`
---
[Back to Index](../index.md)
