# JLFloat32VectorFunctions2

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L170)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This package provides operations which all take as arguments JLFloat32Vector and functions from JLFloat32 to JLFloat32. The operations all iterate over their vector argument and either return a value of type JLFloat32Vector or a JLFloat32.

**JLFloat32VectorFunctions2 is a package constructor**  
**Abbreviation for JLFloat32VectorFunctions2 is JF32VEC2**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 map : ((JLFloat32 -> JLFloat32), JLFloat32Vector) -> JLFloat32Vector
 map : ((JLFloat32 -> Union(JLFloat32,"failed")), JLFloat32Vector) -> Union(JLFloat32Vector,"failed")
 reduce : (((JLFloat32, JLFloat32) -> JLFloat32), JLFloat32Vector, JLFloat32) -> JLFloat32
 scan : (((JLFloat32, JLFloat32) -> JLFloat32), JLFloat32Vector, JLFloat32) -> JLFloat32Vector
```

## Operations added

### `map` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L203)\]

map(f, v) applies the function f to every element of the vector v producing a new vector containing the values.

- **Signature**: `((JLFloat32)->JLFloat32,JLFloat32Vector)->JLFloat32Vector`

map(f, v) applies the function f to every element of the vector v producing a new vector containing the values or "failed".

- **Signature**: `((JLFloat32)->Union(JLFloat32,"failed"),JLFloat32Vector)->Union(JLFloat32Vector,"failed")`

### `reduce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L200)\]

reduce(func, vec, ident) combines the elements in vec using the binary function func. Argument ident is returned if vec is empty.

- **Signature**: `((JLFloat32,JLFloat32)->JLFloat32,JLFloat32Vector,JLFloat32)->JLFloat32`

### `scan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L195)\]

scan(func, vec, ident) creates a new vector whose elements are the result of applying reduce to the binary function func, increasing initial subsequences of the vector vec, and the element ident.

- **Signature**: `((JLFloat32,JLFloat32)->JLFloat32,JLFloat32Vector,JLFloat32)->JLFloat32Vector`
---
[Back to Index](../index.md)
