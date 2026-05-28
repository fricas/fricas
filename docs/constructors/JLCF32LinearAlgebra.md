# JLCF32LinearAlgebra

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L393)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Linear Algebra functions computed using Julia and its algorithms. 32 bits version.

**JLCF32LinearAlgebra is a package constructor**  
**Abbreviation for JLCF32LinearAlgebra is JCF32LA**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 condSkeel : JLComplexF32Matrix -> JLFloat32            conditionNumber : JLComplexF32Matrix -> JLFloat32
 eigvals : JLComplexF32Matrix -> JLComplexF32Vector     eigvals! : JLComplexF32Matrix -> JLComplexF32Vector
 eigvecs : JLComplexF32Matrix -> JLComplexF32Matrix     exp : JLComplexF32Matrix -> JLComplexF32Matrix
 log : JLComplexF32Matrix -> JLComplexF32Matrix         norm : JLComplexF32Vector -> JLFloat32
 norm : (JLComplexF32Vector, JLFloat32) -> JLFloat32    norm : JLComplexF32Matrix -> JLFloat32
 norm : (JLComplexF32Matrix, JLFloat32) -> JLFloat32    operatorNorm : JLComplexF32Matrix -> JLFloat32
 sqrt : JLComplexF32Matrix -> JLComplexF32Matrix        svdvals : JLComplexF32Matrix -> JLFloat32Vector
 svdvals! : JLComplexF32Matrix -> JLFloat32Vector
 conditionNumber : (JLComplexF32Matrix, JLFloat32) -> JLFloat32
 eigen : JLComplexF32Matrix -> Record(values: JLComplexF32Vector,vectors: JLComplexF32Matrix)
 eigen! : JLComplexF32Matrix -> Record(values: JLComplexF32Vector,vectors: JLComplexF32Matrix)
 eigenSystem : JLComplexF32Matrix -> Record(values: JLComplexF32Vector,leftVectors: JLComplexF32Matrix,rightVectors: JLComplexF32Matrix)
 eigenSystem! : JLComplexF32Matrix -> Record(values: JLComplexF32Vector,leftVectors: JLComplexF32Matrix,rightVectors: JLComplexF32Matrix)
 mpInverse : JLComplexF32Matrix -> JLComplexF32Matrix
 normalize : JLComplexF32Vector -> JLComplexF32Vector
 normalize : JLComplexF32Matrix -> JLComplexF32Matrix
 normalize! : JLComplexF32Vector -> JLComplexF32Vector
 normalize! : JLComplexF32Matrix -> JLComplexF32Matrix
 operatorNorm : (JLComplexF32Matrix, JLFloat32) -> JLFloat32
 rank : (JLComplexF32Matrix, JLFloat32) -> NonNegativeInteger
 rank! : (JLComplexF32Matrix, JLFloat32) -> NonNegativeInteger
 solve : (JLComplexF32Matrix, JLComplexF32Matrix) -> JLComplexF32Matrix
 solve! : (JLComplexF32Matrix, JLComplexF32Matrix) -> JLComplexF32Matrix
 svd : JLComplexF32Matrix -> Record(U: JLComplexF32Matrix,sv: JLFloat32Vector,Vt: JLComplexF32Matrix)
 svd! : JLComplexF32Matrix -> Record(U: JLComplexF32Matrix,sv: JLFloat32Vector,Vt: JLComplexF32Matrix)
```

## Operations added

### `condSkeel` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L434)\]

condSkeel(m) computes the Skeel condition number of m.

- **Signature**: `(JLComplexF32Matrix)->JLFloat32`

### `conditionNumber` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L430)\]

conditionNumber(m) computes the condition number of m.

- **Signature**: `(JLComplexF32Matrix)->JLFloat32`

conditionNumber(m, p) computes the p-condition number of m.

- **Signature**: `(JLComplexF32Matrix,JLFloat32)->JLFloat32`

### `eigen` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L454)\]

eigen(m) computes the spectral decomposition of m.

- **Signature**: `(JLComplexF32Matrix)->Record(values:JLComplexF32Vector,vectors:JLComplexF32Matrix)`

### `eigen!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L456)\]

eigen!(m) computes the spectral decomposition of m but overwrites m to save memory space.

- **Signature**: `(JLComplexF32Matrix)->Record(values:JLComplexF32Vector,vectors:JLComplexF32Matrix)`

### `eigenSystem` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L459)\]

eigenSystem(m) computes the spectral decomposition of m.

- **Signature**: `(JLComplexF32Matrix)->Record(values:JLComplexF32Vector,leftVectors:JLComplexF32Matrix,rightVectors:`

### `eigenSystem!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L462)\]

eigenSystem!(m) computes the spectral decomposition of m but overwrites m to save memory space.

- **Signature**: `(JLComplexF32Matrix)->Record(values:JLComplexF32Vector,leftVectors:JLComplexF32Matrix,rightVectors:`

### `eigvals` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L471)\]

eigvals(m) returns the eigen values of m.

- **Signature**: `(JLComplexF32Matrix)->JLComplexF32Vector`

### `eigvals!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L473)\]

eigvals!(m) returns the eigen values of m but overwrites m to save memory space.

- **Signature**: `(JLComplexF32Matrix)->JLComplexF32Vector`

### `eigvecs` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L476)\]

eigvecs(m) returns the eigen vectors of m.

- **Signature**: `(JLComplexF32Matrix)->JLComplexF32Matrix`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L482)\]

exp(m) returns the matrix exponential of m.

- **Signature**: `(JLComplexF32Matrix)->JLComplexF32Matrix`

### `log` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L484)\]

log(m) tries to compute the principal matrix logarithm of m. Otherwise, returns a non principal matrix logarithm of m if possible.

- **Signature**: `(JLComplexF32Matrix)->JLComplexF32Matrix`

### `mpInverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L478)\]

mpInverse(m) returns the Moore-Penrose pseudo inverse of m.

- **Signature**: `(JLComplexF32Matrix)->JLComplexF32Matrix`

### `norm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L415)\]

norm(m) computes the 2-norm of m, also known as the Frobenius norm.

- **Signature**: `(JLComplexF32Matrix)->JLFloat32`

norm(m,p) computes the p-norm of m.

- **Signature**: `(JLComplexF32Matrix,JLFloat32)->JLFloat32`

norm(v) computes the 2-norm of v.

- **Signature**: `(JLComplexF32Vector)->JLFloat32`

norm(v,p) computes the p-norm of v.

- **Signature**: `(JLComplexF32Vector,JLFloat32)->JLFloat32`

### `normalize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L405)\]

normalize(m) returns normalized m such that its norm equals 1.

- **Signature**: `(JLComplexF32Matrix)->JLComplexF32Matrix`

normalize(v) returns normalized v such that its norm equals 1.

- **Signature**: `(JLComplexF32Vector)->JLComplexF32Vector`

### `normalize!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L407)\]

normalize!(m) destructively normalize m such that its norm equals 1.

- **Signature**: `(JLComplexF32Matrix)->JLComplexF32Matrix`

normalize!(v) destructively normalize v such that norm(v) equals 1.

- **Signature**: `(JLComplexF32Vector)->JLComplexF32Vector`

### `operatorNorm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L424)\]

operatorNorm(m) computes the operator norm of m induced by the vector 2-norm.

- **Signature**: `(JLComplexF32Matrix)->JLFloat32`

operatorNorm(m,p) computes the operator norm of m induced by the vector p-norm.

- **Signature**: `(JLComplexF32Matrix,JLFloat32)->JLFloat32`

### `rank` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L436)\]

rank(m, tol) computes rank of m. Counts singular value with magnitude greater than tol.

- **Signature**: `(JLComplexF32Matrix,JLFloat32)->NonNegativeInteger`

### `rank!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L439)\]

rank!(m, tol) computes rank of m. Counts singular value with magnitude greater than tol but overwrites m to save memory space.

- **Signature**: `(JLComplexF32Matrix,JLFloat32)->NonNegativeInteger`

### `solve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L443)\]

solve(A,B) solves the matrix equation A*X=B, and returns X.

- **Signature**: `(JLComplexF32Matrix,JLComplexF32Matrix)->JLComplexF32Matrix`

### `solve!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L445)\]

solve!(A,B) solves the matrix equation A*X=B. Overwrites B with matrix X and returns X.

- **Signature**: `(JLComplexF32Matrix,JLComplexF32Matrix)->JLComplexF32Matrix`

### `sqrt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L480)\]

sqrt(m) returns the principal square root of m.

- **Signature**: `(JLComplexF32Matrix)->JLComplexF32Matrix`

### `svd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L448)\]

svd(m) computes the singular value decomposition SVD of m such that SVD.U * diagonalMatrix(SVD.sv) * SVD.Vt =m.

- **Signature**: `(JLComplexF32Matrix)->Record(U:JLComplexF32Matrix,sv:JLFloat32Vector,Vt:JLComplexF32Matrix)`

### `svd!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L451)\]

svd!(m) is the same as svd(m) but overwrites m to save memory space.

- **Signature**: `(JLComplexF32Matrix)->Record(U:JLComplexF32Matrix,sv:JLFloat32Vector,Vt:JLComplexF32Matrix)`

### `svdvals` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L466)\]

svdvals(m) returns the singular values of m.

- **Signature**: `(JLComplexF32Matrix)->JLFloat32Vector`

### `svdvals!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L468)\]

svdvals!(m) returns the singular values of m but overwrites m to save memory space.

- **Signature**: `(JLComplexF32Matrix)->JLFloat32Vector`
---
[Back to Index](../index.md)
