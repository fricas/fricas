# JLUtilityFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This package provides different Julia utility functions.

**JLUtilityFunctions is a package constructor**  
**Abbreviation for JLUtilityFunctions is JUF**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 QQ : Fraction(Integer) -> NMFraction(NMInteger)        ZZ : Integer -> NMInteger
 jlAddPackage : String -> Void                          jlBuildPackage : String -> Void
 jlDefined? : JLSymbol -> Boolean                       jlDefined? : String -> Boolean
 jlEvalString : String -> Void                          jlEvalString : String -> Boolean
 jlEvalString : String -> JLInt64                       jlEvalString : String -> Integer
 jlEvalString : String -> NonNegativeInteger            jlEvalString : String -> PositiveInteger
 jlEvalString : String -> String                        jlEvalString : String -> JLFloat32
 jlEvalString : String -> JLFloat64                     jlGCCollect : () -> Void
 jlGCPackages : () -> Void                              jlImport : String -> Boolean
 jlInclude : String -> Boolean                          jlInitialize : Boolean -> Boolean
 jlListPackages : () -> Void                            jlRandomString : PositiveInteger -> String
 jlRemovePackage : String -> Void                       jlSeed! : JLInt64 -> Void
 jlTestPackage : String -> Void                         jlUpdateAllPackages : () -> Void
 jlUpdatePackage : String -> Void                       jlUsing : String -> Boolean
```

## Operations added

### `QQ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L90)\]

QQ(q) returns q as a Nemo rational. Convenience function.

- **Signature**: `(Fraction(Integer))->NMFraction(NMInteger)`

### `ZZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L93)\]

ZZ(i) returns i as a Nemo integer. Convenience function.

- **Signature**: `(Integer)->NMInteger`

### `jlAddPackage` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L28)\]

jlAddPackage(package) adds package to Julia. For example, jlAddPackage("PyCall") will add the PyCall package.

- **Signature**: `(String)->Void`

### `jlBuildPackage` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L34)\]

jlBuildPackage(package) builds package in Julia. For example, jlBuildPackage("PyCall") will build the PyCall package.

- **Signature**: `(String)->Void`

### `jlDefined?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L54)\]

jlDefined?(sym) checks whether or not sym is defined in Julia.

- **Signature**: `(JLSymbol)->Boolean`

jlDefined?(str) checks whether or not str is defined in Julia. For example: jlDefined?("sin") will check if sin is defined.

- **Signature**: `(String)->Boolean`

### `jlEvalString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L64)\]

jlEvalString(str) evaluates str in Julia, where the output is a Boolean.

- **Signature**: `(String)->Boolean`

jlEvalString(str) evaluates str in Julia, where the output is an Integer.

- **Signature**: `(String)->Integer`

jlEvalString(str) evaluates str in Julia, where the output is a JLFloat32.

- **Signature**: `(String)->JLFloat32`

jlEvalString(str) evaluates str in Julia, where the output is a JLFloat64.

- **Signature**: `(String)->JLFloat64`

jlEvalString(str) evaluates str in Julia, where the output is a JLInt64.

- **Signature**: `(String)->JLInt64`

jlEvalString(str) evaluates str in Julia, where the output is a NonNegativeInteger.

- **Signature**: `(String)->NonNegativeInteger`

jlEvalString(str) evaluates str in Julia, where the output is a PositiveInteger.

- **Signature**: `(String)->PositiveInteger`

jlEvalString(str) evaluates str in Julia, where the output is a String.

- **Signature**: `(String)->String`

jlEvalString(str) evaluates str in Julia with no output.

- **Signature**: `(String)->Void`

### `jlGCCollect` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L52)\]

jlGCCollect() forces a Julia garbage collector run.

- **Signature**: `()->Void`

### `jlGCPackages` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L47)\]

jlGCPackages() forces a Julia garbage collector run on all installed packages.

- **Signature**: `()->Void`

### `jlImport` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L24)\]

jlImport(modpackfunc) imports the module, package or function modpackfunc and makes it available. Returns false if it cannot be imported.

- **Signature**: `(String)->Boolean`

### `jlInclude` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L49)\]

jlInclude(file.jl) loads and evaluates the content of the Julia file file.jl in the current global scope.

- **Signature**: `(String)->Boolean`

### `jlInitialize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L16)\]

jlInitialize(init) initializes the Julia environment if init is true, otherwise clears the Julia environment.Normally, it should not be used since it is automatically called.

- **Signature**: `(Boolean)->Boolean`

### `jlListPackages` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L45)\]

jlListPackages() displays the status of all installed packages in Julia.

- **Signature**: `()->Void`

### `jlRandomString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L62)\]

jlRandomString(n) returns a random string of n characters.

- **Signature**: `(PositiveInteger)->String`

### `jlRemovePackage` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L31)\]

jlRemovePackage(package) removes package from Julia. For example, jlRemovePackage("PyCall") will remove the PyCall package.

- **Signature**: `(String)->Void`

### `jlSeed!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L59)\]

jlSeed!(n) reseeds the Julia random number generator if applicable (Some random number generators don't accept a seed).

- **Signature**: `(JLInt64)->Void`

### `jlTestPackage` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L42)\]

jlTestPackage(package) tests package in Julia. For example, jlTestPackage("PyCall") will test the PyCall package.

- **Signature**: `(String)->Void`

### `jlUpdateAllPackages` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L40)\]

jlUpdateAllPackages() updates all packages in Julia.

- **Signature**: `()->Void`

### `jlUpdatePackage` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L37)\]

jlUpdatePackage(package) updates package in Julia. For example, jlUpdatePackage("PyCall") will update the PyCall package.

- **Signature**: `(String)->Void`

### `jlUsing` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L20)\]

jlUsing(modpack) loads the module or package modpack and makes its exported names available. Returns false ifit cannot be loaded.

- **Signature**: `(String)->Boolean`
---
[Back to Index](../index.md)
