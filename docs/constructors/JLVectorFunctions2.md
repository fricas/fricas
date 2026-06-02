# JLVectorFunctions2

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1571)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This package provides operations which all take as arguments Julia vectors of elements of some type A and functions from A to another of type B. The operations all iterate over their vector argument and either return a value of type B or a vector over B.

**JLVectorFunctions2(A: JLObjectRing,B: JLObjectRing) is a package constructor**  
**Abbreviation for JLVectorFunctions2 is JVECTOR2**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 map : ((A -> B), JLVector(A)) -> JLVector(B)           reduce : (((A, B) -> B), JLVector(A), B) -> B
 map : ((A -> Union(B,"failed")), JLVector(A)) -> Union(JLVector(B),"failed")
 scan : (((A, B) -> B), JLVector(A), B) -> JLVector(B)
```

## Operations added

### `map` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1599)\]

map(f, v) applies the function f to every element of the vector v producing a new vector containing the values.

- **Signature**: `((A)->B,JLVector(A))->JLVector(B)`

map(f, v) applies the function f to every element of the vector v producing a new vector containing the values or "failed".

- **Signature**: `((A)->Union(B,"failed"),JLVector(A))->Union(JLVector(B),"failed")`

### `reduce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1596)\]

reduce(func, vec, ident) combines the elements in vec using the binary function func. Argument ident is returned if vec is empty.

- **Signature**: `((A,B)->B,JLVector(A),B)->B`

### `scan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1591)\]

scan(func, vec, ident) creates a new vector whose elements are the result of applying reduce to the binary function func, increasing initial subsequences of the vector vec, and the element ident.

- **Signature**: `((A,B)->B,JLVector(A),B)->JLVector(B)`
---
[Back to Index](../index.md)
