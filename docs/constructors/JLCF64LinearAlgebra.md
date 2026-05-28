# JLCF64LinearAlgebra

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L399)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Linear Algebra functions computed using Julia and its algorithms. 64 bits version.

**JLCF64LinearAlgebra is a package constructor**  
**Abbreviation for JLCF64LinearAlgebra is JCF64LA**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 condSkeel : JLComplexF64Matrix -> JLFloat64            conditionNumber : JLComplexF64Matrix -> JLFloat64
 eigvals : JLComplexF64Matrix -> JLComplexF64Vector     eigvals! : JLComplexF64Matrix -> JLComplexF64Vector
 eigvecs : JLComplexF64Matrix -> JLComplexF64Matrix     exp : JLComplexF64Matrix -> JLComplexF64Matrix
 log : JLComplexF64Matrix -> JLComplexF64Matrix         norm : JLComplexF64Vector -> JLFloat64
 norm : (JLComplexF64Vector, JLFloat64) -> JLFloat64    norm : JLComplexF64Matrix -> JLFloat64
 norm : (JLComplexF64Matrix, JLFloat64) -> JLFloat64    operatorNorm : JLComplexF64Matrix -> JLFloat64
 sqrt : JLComplexF64Matrix -> JLComplexF64Matrix        svdvals : JLComplexF64Matrix -> JLFloat64Vector
 svdvals! : JLComplexF64Matrix -> JLFloat64Vector
 conditionNumber : (JLComplexF64Matrix, JLFloat64) -> JLFloat64
 eigen : JLComplexF64Matrix -> Record(values: JLComplexF64Vector,vectors: JLComplexF64Matrix)
 eigen! : JLComplexF64Matrix -> Record(values: JLComplexF64Vector,vectors: JLComplexF64Matrix)
 eigenSystem : JLComplexF64Matrix -> Record(values: JLComplexF64Vector,leftVectors: JLComplexF64Matrix,rightVectors: JLComplexF64Matrix)
 eigenSystem! : JLComplexF64Matrix -> Record(values: JLComplexF64Vector,leftVectors: JLComplexF64Matrix,rightVectors: JLComplexF64Matrix)
 mpInverse : JLComplexF64Matrix -> JLComplexF64Matrix
 normalize : JLComplexF64Vector -> JLComplexF64Vector
 normalize : JLComplexF64Matrix -> JLComplexF64Matrix
 normalize! : JLComplexF64Vector -> JLComplexF64Vector
 normalize! : JLComplexF64Matrix -> JLComplexF64Matrix
 operatorNorm : (JLComplexF64Matrix, JLFloat64) -> JLFloat64
 rank : (JLComplexF64Matrix, JLFloat64) -> NonNegativeInteger
 rank! : (JLComplexF64Matrix, JLFloat64) -> NonNegativeInteger
 solve : (JLComplexF64Matrix, JLComplexF64Matrix) -> JLComplexF64Matrix
 solve! : (JLComplexF64Matrix, JLComplexF64Matrix) -> JLComplexF64Matrix
 svd : JLComplexF64Matrix -> Record(U: JLComplexF64Matrix,sv: JLFloat64Vector,Vt: JLComplexF64Matrix)
 svd! : JLComplexF64Matrix -> Record(U: JLComplexF64Matrix,sv: JLFloat64Vector,Vt: JLComplexF64Matrix)
```

## Operations added

### `condSkeel` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L440)\]

condSkeel(m) computes the Skeel condition number of m.

- **Signature**: `(JLComplexF64Matrix)->JLFloat64`

### `conditionNumber` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L436)\]

conditionNumber(m) computes the condition number of m.

- **Signature**: `(JLComplexF64Matrix)->JLFloat64`

conditionNumber(m, p) computes the p-condition number of m.

- **Signature**: `(JLComplexF64Matrix,JLFloat64)->JLFloat64`

### `eigen` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L460)\]

eigen(m) computes the spectral decomposition of m.

- **Signature**: `(JLComplexF64Matrix)->Record(values:JLComplexF64Vector,vectors:JLComplexF64Matrix)`

### `eigen!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L462)\]

eigen!(m) computes the spectral decomposition of m but overwrites m to save memory space.

- **Signature**: `(JLComplexF64Matrix)->Record(values:JLComplexF64Vector,vectors:JLComplexF64Matrix)`

### `eigenSystem` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L465)\]

eigenSystem(m) computes the spectral decomposition of m.

- **Signature**: `(JLComplexF64Matrix)->Record(values:JLComplexF64Vector,leftVectors:JLComplexF64Matrix,rightVectors:`

### `eigenSystem!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L468)\]

eigenSystem!(m) computes the spectral decomposition of m but overwrites m to save memory space.

- **Signature**: `(JLComplexF64Matrix)->Record(values:JLComplexF64Vector,leftVectors:JLComplexF64Matrix,rightVectors:`

### `eigvals` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L477)\]

eigvals(m) returns the eigen values of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Vector`

### `eigvals!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L479)\]

eigvals!(m) returns the eigen values of m but overwrites m to save memory space.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Vector`

### `eigvecs` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L482)\]

eigvecs(m) returns the eigen vectors of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L488)\]

exp(m) returns the matrix exponential of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `log` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L490)\]

log(m) tries to compute the principal matrix logarithm of m. Otherwise, returns a non principal matrix logarithm of m if possible.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `mpInverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L484)\]

mpInverse(m) returns the Moore-Penrose pseudo inverse of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `norm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L421)\]

norm(m) computes the 2-norm of m, also known as the Frobenius norm.

- **Signature**: `(JLComplexF64Matrix)->JLFloat64`

norm(m,p) computes the p-norm of m.

- **Signature**: `(JLComplexF64Matrix,JLFloat64)->JLFloat64`

norm(v) computes the 2-norm of v.

- **Signature**: `(JLComplexF64Vector)->JLFloat64`

norm(v,p) computes the p-norm of v.

- **Signature**: `(JLComplexF64Vector,JLFloat64)->JLFloat64`

### `normalize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L411)\]

normalize(m) returns normalized m such that its norm equals 1.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

normalize(v) returns normalized v such that its norm equals 1.

- **Signature**: `(JLComplexF64Vector)->JLComplexF64Vector`

### `normalize!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L413)\]

normalize!(m) destructively normalize m such that its norm equals 1.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

normalize!(v) destructively normalize v such that norm(v) equals 1.

- **Signature**: `(JLComplexF64Vector)->JLComplexF64Vector`

### `operatorNorm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L430)\]

operatorNorm(m) computes the operator norm of m induced by the vector 2-norm.

- **Signature**: `(JLComplexF64Matrix)->JLFloat64`

operatorNorm(m,p) computes the operator norm of m induced by the vector p-norm.

- **Signature**: `(JLComplexF64Matrix,JLFloat64)->JLFloat64`

### `rank` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L442)\]

rank(m, tol) computes rank of m. Counts singular value with magnitude greater than tol.

- **Signature**: `(JLComplexF64Matrix,JLFloat64)->NonNegativeInteger`

### `rank!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L445)\]

rank!(m, tol) computes rank of m. Counts singular value with magnitude greater than tol but overwrites m to save memory space.

- **Signature**: `(JLComplexF64Matrix,JLFloat64)->NonNegativeInteger`

### `solve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L449)\]

solve(A,B) solves the matrix equation A*X=B, and returns X.

- **Signature**: `(JLComplexF64Matrix,JLComplexF64Matrix)->JLComplexF64Matrix`

### `solve!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L451)\]

solve!(A,B) solves the matrix equation A*X=B. Overwrites B with matrix X and returns X.

- **Signature**: `(JLComplexF64Matrix,JLComplexF64Matrix)->JLComplexF64Matrix`

### `sqrt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L486)\]

sqrt(m) returns the principal square root of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `svd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L454)\]

svd(m) computes the singular value decomposition SVD of m such that SVD.U * diagonalMatrix(SVD.sv) * SVD.Vt =m.

- **Signature**: `(JLComplexF64Matrix)->Record(U:JLComplexF64Matrix,sv:JLFloat64Vector,Vt:JLComplexF64Matrix)`

### `svd!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L457)\]

svd!(m) is the same as svd(m) but overwrites m to save memory space.

- **Signature**: `(JLComplexF64Matrix)->Record(U:JLComplexF64Matrix,sv:JLFloat64Vector,Vt:JLComplexF64Matrix)`

### `svdvals` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L472)\]

svdvals(m) returns the singular values of m.

- **Signature**: `(JLComplexF64Matrix)->JLFloat64Vector`

### `svdvals!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L474)\]

svdvals!(m) returns the singular values of m but overwrites m to save memory space.

- **Signature**: `(JLComplexF64Matrix)->JLFloat64Vector`
---
[Back to Index](../index.md)
