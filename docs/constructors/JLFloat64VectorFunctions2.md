# JLFloat64VectorFunctions2

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L242)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This package provides operations which all take as arguments JLFloat64Vector and functions from JLFloat64 to JLFloat64. The operations all iterate over their vector argument and either return a value of type JLFloat64Vector or a JLFloat64.

**JLFloat64VectorFunctions2 is a package constructor**  
**Abbreviation for JLFloat64VectorFunctions2 is JF64VEC2**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 map : ((JLFloat64 -> JLFloat64), JLFloat64Vector) -> JLFloat64Vector
 map : ((JLFloat64 -> Union(JLFloat64,"failed")), JLFloat64Vector) -> Union(JLFloat64Vector,"failed")
 reduce : (((JLFloat64, JLFloat64) -> JLFloat64), JLFloat64Vector, JLFloat64) -> JLFloat64
 scan : (((JLFloat64, JLFloat64) -> JLFloat64), JLFloat64Vector, JLFloat64) -> JLFloat64Vector
```

## Operations added

### `map` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L273)\]

map(f, v) applies the function f to every element of the vector v producing a new vector containing the values.

- **Signature**: `((JLFloat64)->JLFloat64,JLFloat64Vector)->JLFloat64Vector`

map(f, v) applies the function f to every element of the vector v producing a new vector containing the values or "failed".

- **Signature**: `((JLFloat64)->Union(JLFloat64,"failed"),JLFloat64Vector)->Union(JLFloat64Vector,"failed")`

### `reduce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L270)\]

reduce(func, vec, ident) combines the elements in vec using the binary function func. Argument ident is returned if vec is empty.

- **Signature**: `((JLFloat64,JLFloat64)->JLFloat64,JLFloat64Vector,JLFloat64)->JLFloat64`

### `scan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L265)\]

scan(func, vec, ident) creates a new vector whose elements are the result of applying reduce to the binary function func, increasing initial subsequences of the vector vec, and the element ident.

- **Signature**: `((JLFloat64,JLFloat64)->JLFloat64,JLFloat64Vector,JLFloat64)->JLFloat64Vector`
---
[Back to Index](../index.md)
