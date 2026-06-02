# JLF64ArrayFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jafunctions.spad#L147)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This package provides different Julia utility functions for operations using CPU/GPU supported data types. Internal package.

**JLF64ArrayFunctions is a package constructor**  
**Abbreviation for JLF64ArrayFunctions is JF64AF**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 jlApplyFunction : (String, JLFloat64Vector) -> Void
 jlApplyFunction : (String, JLComplexF64Vector) -> Void
 jlApplyFunction : (String, JLFloat64Vector) -> JLFloat64
 jlApplyFunction : (String, JLComplexF64Vector) -> JLFloat64
 jlApplyFunction : (String, JLFloat64Vector, JLFloat64) -> JLFloat64
 jlApplyFunction : (String, JLComplexF64Vector, JLFloat64) -> JLFloat64
 jlApplyFunction : (String, JLFloat64Vector, NonNegativeInteger) -> Void
 jlApplyFunction : (String, JLComplexF64Vector, NonNegativeInteger) -> Void
 jlApplyFunction : (String, JLFloat64Vector, NonNegativeInteger) -> JLFloat64
 jlApplyFunction : (String, JLComplexF64Vector, NonNegativeInteger) -> JLFloat64
 jlApplyFunction : (String, JLFloat64Vector, NonNegativeInteger, JLFloat64) -> JLFloat64
 jlApplyFunction : (String, JLComplexF64Vector, NonNegativeInteger, JLFloat64) -> JLFloat64
 jlApplyFunction! : (String, JLFloat64Vector, JLFloat64Vector) -> Void
 jlApplyFunction! : (String, JLComplexF64Vector, JLComplexF64Vector) -> Void
 jlApplyFunction! : (String, JLFloat64Vector, NonNegativeInteger, JLFloat64Vector, NonNegativeInteger) -> Void
 jlApplyFunction! : (String, JLComplexF64Vector, NonNegativeInteger, JLComplexF64Vector, NonNegativeInteger) -> Void
 jlApplyFunction! : (String, JLFloat64Vector, JLFloat64Vector, NonNegativeInteger) -> Void
 jlApplyFunction! : (String, JLComplexF64Vector, JLFloat64Vector, NonNegativeInteger) -> Void
 jlApplyFunction! : (String, JLFloat64Vector, JLComplexF64Vector, NonNegativeInteger) -> Void
 jlApplyFunction! : (String, JLComplexF64Vector, JLComplexF64Vector, NonNegativeInteger) -> Void
```

## Operations added

### `jlApplyFunction` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jafunctions.spad#L164)\]

jlApplyFunction(func, cvec) returns the output value of func applied to cvec.

- **Signature**: `(String,JLComplexF64Vector)->JLFloat64`

jlApplyFunction(func, cvec, p) returns the output value of func applied to cvec with parameter p.

- **Signature**: `(String,JLComplexF64Vector,JLFloat64)->JLFloat64`

jlApplyFunction(func, cmat, nr) returns the output value of func applied to cmat, internally a vector, where the number of rows is nr.

- **Signature**: `(String,JLComplexF64Vector,NonNegativeInteger)->JLFloat64`

jlApplyFunction(func, cmat, nr, p) returns the output value of func applied to cmat, internally a vector, where the number of rows is nr and p an additional parameter.

- **Signature**: `(String,JLComplexF64Vector,NonNegativeInteger,JLFloat64)->JLFloat64`

jlApplyFunction(func, vec) returns the output value of func applied to vec.

- **Signature**: `(String,JLFloat64Vector)->JLFloat64`

jlApplyFunction(func, vec, p) returns the output value of func applied to vec with parameter p.

- **Signature**: `(String,JLFloat64Vector,JLFloat64)->JLFloat64`

jlApplyFunction(func, mat, nr) returns the output value of func applied to mat, internally a vector, where the number of rows is nr.

- **Signature**: `(String,JLFloat64Vector,NonNegativeInteger)->JLFloat64`

jlApplyFunction(func, mat, nr, p) returns the output value of func applied to mat, internally a vector, wherethe number of rows is nr and p an additional parameter.

- **Signature**: `(String,JLFloat64Vector,NonNegativeInteger,JLFloat64)->JLFloat64`

jlApplyFunction(func, vec) applies func to cvec.

- **Signature**: `(String,JLComplexF64Vector)->Void`

jlApplyFunction(func, cmat, nr) applies func to the matrix mat, internally a vector, where the numer of rows is nr.

- **Signature**: `(String,JLComplexF64Vector,NonNegativeInteger)->Void`

jlApplyFunction(func, vec) applies func to vec.

- **Signature**: `(String,JLFloat64Vector)->Void`

jlApplyFunction(func, mat, nr) applies func to the matrix mat, internally a vector, where the numer of rows is nr.

- **Signature**: `(String,JLFloat64Vector,NonNegativeInteger)->Void`

### `jlApplyFunction!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jafunctions.spad#L181)\]

jlApplyFunction!(func, cvec1, cvec2) applies the Julia function func to cvec2 and overwrites cvec1 with the result. Please note that overwriting is done by Julia and not FriCAS. See Julia 'copy!' for example.

- **Signature**: `(String,JLComplexF64Vector,JLComplexF64Vector)->Void`

jlApplyFunction!(func, carray, cmat, nr) applies the function func to the matrix cmat and returns the result in carray. It is FriCAS that puts, at the C level, the resulting matrix in carray.

- **Signature**: `(String,JLComplexF64Vector,JLComplexF64Vector,NonNegativeInteger)->Void`

jlApplyFunction!(func, carray, mat, nr) applies the function func to the matrix mat and returns the result incarray. It is FriCAS that puts, at the C level, the resulting matrix in carray. Can be used with Julia 'sqrt'for example.

- **Signature**: `(String,JLComplexF64Vector,JLFloat64Vector,NonNegativeInteger)->Void`

jlApplyFunction!(func, cmat1, cmat2) applies the Julia function func to cmat2 and overwrites cmat1 with the result. Please note that overwriting is done by Julia and not FriCAS. See Julia 'copy!' or adjoint! for example.

- **Signature**: `(String,JLComplexF64Vector,NonNegativeInteger,JLComplexF64Vector,NonNegativeInteger)->Void`

jlApplyFunction!(func, array, cmat, nr) applies the function func to the matrix cmat and returns the result in array. It is FriCAS that puts, at the C level, the resulting matrix in array. Can be used with Julia 'real' or 'imag' for example.

- **Signature**: `(String,JLFloat64Vector,JLComplexF64Vector,NonNegativeInteger)->Void`

jlApplyFunction!(func, vec1, vec2) applies the Julia function func to vec2 and overwrites vec1 with the result. Please note that overwriting is done by Julia and not FriCAS. See Julia 'copy!' for example.

- **Signature**: `(String,JLFloat64Vector,JLFloat64Vector)->Void`

jlApplyFunction!(func, array, mat, nr) applies the function func to the matrix mat and returns the result in array. It is FriCAS that puts, at the C level, the resulting matrix in array. Can be used with Julia 'inv' forexample.

- **Signature**: `(String,JLFloat64Vector,JLFloat64Vector,NonNegativeInteger)->Void`

jlApplyFunction!(func, mat1, mat2) applies the Julia function func to mat2 and overwrites mat1 with the result. Please note that overwriting is done by Julia and not FriCAS. See Julia 'copy!' or transpose! for example.

- **Signature**: `(String,JLFloat64Vector,NonNegativeInteger,JLFloat64Vector,NonNegativeInteger)->Void`
---
[Back to Index](../index.md)
