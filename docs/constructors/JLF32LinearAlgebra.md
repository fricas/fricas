# JLF32LinearAlgebra

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Linear Algebra functions computed using Julia and its algorithms. 32 bits version.

**JLF32LinearAlgebra is a package constructor**  
**Abbreviation for JLF32LinearAlgebra is JF32LA**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 condSkeel : JLFloat32Matrix -> JLFloat32               conditionNumber : JLFloat32Matrix -> JLFloat32
 eigvals : JLFloat32Matrix -> JLComplexF32Vector        eigvals! : JLFloat32Matrix -> JLComplexF32Vector
 eigvecs : JLFloat32Matrix -> JLComplexF32Matrix        exp : JLFloat32Matrix -> JLFloat32Matrix
 log : JLFloat32Matrix -> JLComplexF32Matrix            logDeterminant : JLFloat32Matrix -> JLFloat32
 mpInverse : JLFloat32Matrix -> JLFloat32Matrix         norm : JLFloat32Vector -> JLFloat32
 norm : (JLFloat32Vector, JLFloat32) -> JLFloat32       norm : JLFloat32Matrix -> JLFloat32
 norm : (JLFloat32Matrix, JLFloat32) -> JLFloat32       normalize : JLFloat32Vector -> JLFloat32Vector
 normalize : JLFloat32Matrix -> JLFloat32Matrix         normalize! : JLFloat32Vector -> JLFloat32Vector
 normalize! : JLFloat32Matrix -> JLFloat32Matrix        operatorNorm : JLFloat32Matrix -> JLFloat32
 sqrt : JLFloat32Matrix -> JLComplexF32Matrix           svdvals : JLFloat32Matrix -> JLFloat32Vector
 svdvals! : JLFloat32Matrix -> JLFloat32Vector          trace : JLFloat32Matrix -> JLFloat32
 tril : JLFloat32Matrix -> JLFloat32Matrix              tril! : JLFloat32Matrix -> JLFloat32Matrix
 triu : JLFloat32Matrix -> JLFloat32Matrix              triu! : JLFloat32Matrix -> JLFloat32Matrix
 conditionNumber : (JLFloat32Matrix, JLFloat32) -> JLFloat32
 eigen : JLFloat32Matrix -> Record(values: JLComplexF32Vector,vectors: JLComplexF32Matrix)
 eigen! : JLFloat32Matrix -> Record(values: JLComplexF32Vector,vectors: JLComplexF32Matrix)
 eigenSystem : JLFloat32Matrix -> Record(values: JLComplexF32Vector,leftVectors: JLFloat32Matrix,rightVectors: JLFloat32Matrix)
 eigenSystem! : JLFloat32Matrix -> Record(values: JLComplexF32Vector,leftVectors: JLFloat32Matrix,rightVectors: JLFloat32Matrix)
 lu : JLFloat32Matrix -> Record(LU: JLFloat32Matrix,L: JLFloat32Matrix,U: JLFloat32Matrix,ipiv: JLInt64Vector)
 lu! : JLFloat32Matrix -> Record(LU: JLFloat32Matrix,ipiv: JLInt64Vector)
 luReorder : (JLFloat32Matrix, JLInt64Vector) -> JLFloat32Matrix
 luReorder! : (JLFloat32Matrix, JLInt64Vector) -> JLFloat32Matrix
 operatorNorm : (JLFloat32Matrix, JLFloat32) -> JLFloat32
 rank : (JLFloat32Matrix, JLFloat32) -> NonNegativeInteger
 rank! : (JLFloat32Matrix, JLFloat32) -> NonNegativeInteger
 solve : (JLFloat32Matrix, JLFloat32Matrix) -> JLFloat32Matrix
 solve! : (JLFloat32Matrix, JLFloat32Matrix) -> JLFloat32Matrix
 svd : JLFloat32Matrix -> Record(U: JLFloat32Matrix,sv: JLFloat32Vector,Vt: JLFloat32Matrix)
 svd! : JLFloat32Matrix -> Record(U: JLFloat32Matrix,sv: JLFloat32Vector,Vt: JLFloat32Matrix)
```

## Operations added

### `LU`

lu(m) computes the LU factorisation of m.

- **Signature**: `(JLFloat32Matrix)->Record(LU:JLFloat32Matrix,L:JLFloat32Matrix,U:JLFloat32Matrix,ipiv:JLInt64Vector`

### `condSkeel` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L43)\]

condSkeel(m) computes the Skeel condition number of m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32`

### `conditionNumber` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L39)\]

conditionNumber(m) computes the condition number of m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32`

conditionNumber(m, p) computes the p-condition number of m.

- **Signature**: `(JLFloat32Matrix,JLFloat32)->JLFloat32`

### `eigen` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L75)\]

eigen(m) computes the spectral decomposition of m.

- **Signature**: `(JLFloat32Matrix)->Record(values:JLComplexF32Vector,vectors:JLComplexF32Matrix)`

### `eigen!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L77)\]

eigen!(m) computes the spectral decomposition of m but overwrites m to save memory space.

- **Signature**: `(JLFloat32Matrix)->Record(values:JLComplexF32Vector,vectors:JLComplexF32Matrix)`

### `eigenSystem` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L80)\]

eigenSystem(m) computes the spectral decomposition of m. If the j-th eigenvalue (values) is real, then the left eigenvectors u(j) = column(leftVectors,j), the j-th column of leftVectors. If the j-th and (j+1)-st eigenvalues form a complex conjugate pair, then the left eigenvectors are u(j) = column(leftVectors,j) + %i*column(leftVectors,j+1) and u(j+1) = column(leftVectors,j) - %i*column(leftVectors,j+1). This applies also to rightVectors.

- **Signature**: `(JLFloat32Matrix)->Record(values:JLComplexF32Vector,leftVectors:JLFloat32Matrix,rightVectors:JLFloa`

### `eigenSystem!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L90)\]

eigenSystem!(m) computes the spectral decomposition of m but overwrites m to save memory space. If the j-th eigenvalue (values) is real, then the left eigenvectors u(j) = column(leftVectors,j), the j-th column of leftVectors. If the j-th and (j+1)-st eigenvalues form a complex conjugate pair, then the left eigenvectors are u(j)= column(leftVectors,j) + %i*column(leftVectors,j+1) and u(j+1) = column(leftVectors,j) - %i*column(leftVectors,j+1). This applies also to rightVectors.

- **Signature**: `(JLFloat32Matrix)->Record(values:JLComplexF32Vector,leftVectors:JLFloat32Matrix,rightVectors:JLFloa`

### `eigvals` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L68)\]

eigvals(m) returns the eigen values of m.

- **Signature**: `(JLFloat32Matrix)->JLComplexF32Vector`

### `eigvals!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L70)\]

eigvals!(m) returns the eigen values of m but overwrites m to save memory space.

- **Signature**: `(JLFloat32Matrix)->JLComplexF32Vector`

### `eigvecs` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L73)\]

eigvecs(m) returns the eigen vectors of m.

- **Signature**: `(JLFloat32Matrix)->JLComplexF32Matrix`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L122)\]

exp(m) returns the matrix exponential of m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Matrix`

### `log` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L124)\]

log(m) tries to compute the principal matrix logarithm of m. Otherwise, returns a non principal matrix logarithm of m if possible.

- **Signature**: `(JLFloat32Matrix)->JLComplexF32Matrix`

### `logDeterminant` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L54)\]

logDeterminant(m) computes the logarithm of the determinant of m, possibly with more accuracy and avoiding under/overflow.

- **Signature**: `(JLFloat32Matrix)->JLFloat32`

### `lu` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L106)\]

lu(m) computes the LU factorisation of m.

- **Signature**: `(JLFloat32Matrix)->Record(LU:JLFloat32Matrix,L:JLFloat32Matrix,U:JLFloat32Matrix,ipiv:JLInt64Vector`

### `lu!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L109)\]

lu!(m) computes in-place the LU factorisation of m. m is overwritten by its decomposition.

- **Signature**: `(JLFloat32Matrix)->Record(LU:JLFloat32Matrix,ipiv:JLInt64Vector)`

### `luReorder` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L112)\]

luReorder(mat, ipiv) returns a copy of mat reordered using the ipiv pivot indices.

- **Signature**: `(JLFloat32Matrix,JLInt64Vector)->JLFloat32Matrix`

### `luReorder!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L115)\]

luReorder!(mat, ipiv) returns mat reordered in-place using the ipiv pivot indices.

- **Signature**: `(JLFloat32Matrix,JLInt64Vector)->JLFloat32Matrix`

### `mpInverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L118)\]

mpInverse(m) returns the Moore-Penrose pseudo inverse of m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Matrix`

### `norm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L24)\]

norm(m) computes the 2-norm of m, also known as the Frobenius norm.

- **Signature**: `(JLFloat32Matrix)->JLFloat32`

norm(m,p) computes the p-norm of m.

- **Signature**: `(JLFloat32Matrix,JLFloat32)->JLFloat32`

norm(v) computes the 2-norm of v.

- **Signature**: `(JLFloat32Vector)->JLFloat32`

norm(v,p) computes the p-norm of v.

- **Signature**: `(JLFloat32Vector,JLFloat32)->JLFloat32`

### `normalize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L14)\]

normalize(m) returns normalized m such that its norm equals 1.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Matrix`

normalize(v) returns normalized v such that its norm equals 1.

- **Signature**: `(JLFloat32Vector)->JLFloat32Vector`

### `normalize!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L16)\]

normalize!(m) destructively normalize m such that its norm equals 1.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Matrix`

normalize!(v) destructively normalize v such that norm(v) equals 1.

- **Signature**: `(JLFloat32Vector)->JLFloat32Vector`

### `operatorNorm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L33)\]

operatorNorm(m) computes the operator norm of m induced by the vector 2-norm.

- **Signature**: `(JLFloat32Matrix)->JLFloat32`

operatorNorm(m,p) computes the operator norm of m induced by the vector p-norm.

- **Signature**: `(JLFloat32Matrix,JLFloat32)->JLFloat32`

### `rank` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L47)\]

rank(m, tol) computes the rank of m. Counts singular value with magnitude greater than tol.

- **Signature**: `(JLFloat32Matrix,JLFloat32)->NonNegativeInteger`

### `rank!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L50)\]

rank!(m, tol) computes the rank of m. Counts singular value with magnitude greater than tol but overwrites m to save memory space.

- **Signature**: `(JLFloat32Matrix,JLFloat32)->NonNegativeInteger`

### `solve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L101)\]

solve(A,B) solves the matrix equation A*X=B, and returns X.

- **Signature**: `(JLFloat32Matrix,JLFloat32Matrix)->JLFloat32Matrix`

### `solve!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L103)\]

solve!(A,B) solves the matrix equation A*X=B. Overwrites B with matrix X and returns X.

- **Signature**: `(JLFloat32Matrix,JLFloat32Matrix)->JLFloat32Matrix`

### `sqrt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L120)\]

sqrt(m) returns the principal square root of m.

- **Signature**: `(JLFloat32Matrix)->JLComplexF32Matrix`

### `svd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L57)\]

svd(m) computes the singular value decomposition SVD of m such that SVD.U * diagonalMatrix(SVD.sv) * SVD.Vt =m.

- **Signature**: `(JLFloat32Matrix)->Record(U:JLFloat32Matrix,sv:JLFloat32Vector,Vt:JLFloat32Matrix)`

### `svd!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L60)\]

svd!(m) is the same as svd(m) but overwrites m to save memory space.

- **Signature**: `(JLFloat32Matrix)->Record(U:JLFloat32Matrix,sv:JLFloat32Vector,Vt:JLFloat32Matrix)`

### `svdvals` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L63)\]

svdvals(m) returns the singular values of m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Vector`

### `svdvals!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L65)\]

svdvals!(m) returns the singular values of m but overwrites m to save memory space.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Vector`

### `trace` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L45)\]

trace(m) computes the trace of m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32`

### `tril` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L132)\]

tril(m) returns the lower triangular matrix of m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Matrix`

### `tril!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L134)\]

tril!(m) overwrites m with its lower triangular matrix counterpart. Returns m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Matrix`

### `triu` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L127)\]

triu(m) returns the upper triangular matrix of m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Matrix`

### `triu!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla32.spad#L129)\]

triu!(m) overwrites m with its upper triangular matrix counterpart. Returns m.

- **Signature**: `(JLFloat32Matrix)->JLFloat32Matrix`
---
[Back to Index](../index.md)
