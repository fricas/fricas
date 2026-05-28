# JLF64LinearAlgebra

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Linear Algebra functions computed using Julia and its algorithms. 64 bits version.

**JLF64LinearAlgebra is a package constructor**  
**Abbreviation for JLF64LinearAlgebra is JF64LA**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 condSkeel : JLFloat64Matrix -> JLFloat64               conditionNumber : JLFloat64Matrix -> JLFloat64
 eigvals : JLFloat64Matrix -> JLComplexF64Vector        eigvals! : JLFloat64Matrix -> JLComplexF64Vector
 eigvecs : JLFloat64Matrix -> JLComplexF64Matrix        exp : JLFloat64Matrix -> JLFloat64Matrix
 jlPeakFlops : () -> JLFloat64                          log : JLFloat64Matrix -> JLComplexF64Matrix
 logDeterminant : JLFloat64Matrix -> JLFloat64          mpInverse : JLFloat64Matrix -> JLFloat64Matrix
 norm : JLFloat64Vector -> JLFloat64                    norm : (JLFloat64Vector, JLFloat64) -> JLFloat64
 norm : JLFloat64Matrix -> JLFloat64                    norm : (JLFloat64Matrix, JLFloat64) -> JLFloat64
 normalize : JLFloat64Vector -> JLFloat64Vector         normalize : JLFloat64Matrix -> JLFloat64Matrix
 normalize! : JLFloat64Vector -> JLFloat64Vector        normalize! : JLFloat64Matrix -> JLFloat64Matrix
 operatorNorm : JLFloat64Matrix -> JLFloat64            sqrt : JLFloat64Matrix -> JLComplexF64Matrix
 svdvals : JLFloat64Matrix -> JLFloat64Vector           svdvals! : JLFloat64Matrix -> JLFloat64Vector
 trace : JLFloat64Matrix -> JLFloat64                   tril : JLFloat64Matrix -> JLFloat64Matrix
 tril! : JLFloat64Matrix -> JLFloat64Matrix             triu : JLFloat64Matrix -> JLFloat64Matrix
 triu! : JLFloat64Matrix -> JLFloat64Matrix
 conditionNumber : (JLFloat64Matrix, JLFloat64) -> JLFloat64
 eigen : JLFloat64Matrix -> Record(values: JLComplexF64Vector,vectors: JLComplexF64Matrix)
 eigen! : JLFloat64Matrix -> Record(values: JLComplexF64Vector,vectors: JLComplexF64Matrix)
 eigenSystem : JLFloat64Matrix -> Record(values: JLComplexF64Vector,leftVectors: JLFloat64Matrix,rightVectors: JLFloat64Matrix)
 eigenSystem! : JLFloat64Matrix -> Record(values: JLComplexF64Vector,leftVectors: JLFloat64Matrix,rightVectors: JLFloat64Matrix)
 lu : JLFloat64Matrix -> Record(LU: JLFloat64Matrix,L: JLFloat64Matrix,U: JLFloat64Matrix,ipiv: JLInt64Vector)
 lu! : JLFloat64Matrix -> Record(LU: JLFloat64Matrix,ipiv: JLInt64Vector)
 luReorder : (JLFloat64Matrix, JLInt64Vector) -> JLFloat64Matrix
 luReorder! : (JLFloat64Matrix, JLInt64Vector) -> JLFloat64Matrix
 operatorNorm : (JLFloat64Matrix, JLFloat64) -> JLFloat64
 rank : (JLFloat64Matrix, JLFloat64) -> NonNegativeInteger
 rank! : (JLFloat64Matrix, JLFloat64) -> NonNegativeInteger
 solve : (JLFloat64Matrix, JLFloat64Matrix) -> JLFloat64Matrix
 solve! : (JLFloat64Matrix, JLFloat64Matrix) -> JLFloat64Matrix
 svd : JLFloat64Matrix -> Record(U: JLFloat64Matrix,sv: JLFloat64Vector,Vt: JLFloat64Matrix)
 svd! : JLFloat64Matrix -> Record(U: JLFloat64Matrix,sv: JLFloat64Vector,Vt: JLFloat64Matrix)
```

## Operations added

### `LU`

lu(m) computes the LU factorisation of m.

- **Signature**: `(JLFloat64Matrix)->Record(LU:JLFloat64Matrix,L:JLFloat64Matrix,U:JLFloat64Matrix,ipiv:JLInt64Vector`

### `condSkeel` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L43)\]

condSkeel(m) computes the Skeel condition number of m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64`

### `conditionNumber` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L39)\]

conditionNumber(m) computes the condition number of m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64`

conditionNumber(m, p) computes the p-condition number of m.

- **Signature**: `(JLFloat64Matrix,JLFloat64)->JLFloat64`

### `eigen` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L75)\]

eigen(m) computes the spectral decomposition of m.

- **Signature**: `(JLFloat64Matrix)->Record(values:JLComplexF64Vector,vectors:JLComplexF64Matrix)`

### `eigen!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L77)\]

eigen!(m) computes the spectral decomposition of m but overwrites m to save memory space.

- **Signature**: `(JLFloat64Matrix)->Record(values:JLComplexF64Vector,vectors:JLComplexF64Matrix)`

### `eigenSystem` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L80)\]

eigenSystem(m) computes the spectral decomposition of m. If the j-th eigenvalue (values) is real, then the left eigenvectors u(j) = column(leftVectors,j), the j-th column of leftVectors. If the j-th and (j+1)-st eigenvalues form a complex conjugate pair, then the left eigenvectors are u(j) = column(leftVectors,j) + %i*column(leftVectors,j+1) and u(j+1) = column(leftVectors,j) - %i*column(leftVectors,j+1). This applies also to rightVectors.

- **Signature**: `(JLFloat64Matrix)->Record(values:JLComplexF64Vector,leftVectors:JLFloat64Matrix,rightVectors:JLFloa`

### `eigenSystem!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L90)\]

eigenSystem!(m) computes the spectral decomposition of m but overwrites m to save memory space. If the j-th eigenvalue (values) is real, then the left eigenvectors u(j) = column(leftVectors,j), the j-th column of leftVectors. If the j-th and (j+1)-st eigenvalues form a complex conjugate pair, then the left eigenvectors are u(j)= column(leftVectors,j) + %i*column(leftVectors,j+1) and u(j+1) = column(leftVectors,j) - %i*column(leftVectors,j+1). This applies also to rightVectors.

- **Signature**: `(JLFloat64Matrix)->Record(values:JLComplexF64Vector,leftVectors:JLFloat64Matrix,rightVectors:JLFloa`

### `eigvals` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L68)\]

eigvals(m) returns the eigen values of m.

- **Signature**: `(JLFloat64Matrix)->JLComplexF64Vector`

### `eigvals!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L70)\]

eigvals!(m) returns the eigen values of m but overwrites m to save memory space.

- **Signature**: `(JLFloat64Matrix)->JLComplexF64Vector`

### `eigvecs` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L73)\]

eigvecs(m) returns the eigen vectors of m.

- **Signature**: `(JLFloat64Matrix)->JLComplexF64Matrix`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L123)\]

exp(m) returns the matrix exponential of m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Matrix`

### `jlPeakFlops` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L138)\]

jlPeakFlops() returns the peak flop rate using matrix multiplication. You can modify the number of threads used or the BLAS/LAPACK libraries used to see if that fits your needs.

- **Signature**: `()->JLFloat64`

### `log` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L125)\]

log(m) tries to compute the principal matrix logarithm of m. Otherwise, returns a non principal matrix logarithm of m if possible.

- **Signature**: `(JLFloat64Matrix)->JLComplexF64Matrix`

### `logDeterminant` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L54)\]

logDeterminant(m) computes the logarithm of the determinant of m, possibly with more accuracy and avoiding under/overflow.

- **Signature**: `(JLFloat64Matrix)->JLFloat64`

### `lu` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L106)\]

lu(m) computes the LU factorisation of m.

- **Signature**: `(JLFloat64Matrix)->Record(LU:JLFloat64Matrix,L:JLFloat64Matrix,U:JLFloat64Matrix,ipiv:JLInt64Vector`

### `lu!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L109)\]

lu!(m) computes in-place the LU factorisation of m. m is overwritten by its decomposition.

- **Signature**: `(JLFloat64Matrix)->Record(LU:JLFloat64Matrix,ipiv:JLInt64Vector)`

### `luReorder` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L112)\]

luReorder(mat, ipiv) returns a copy of mat reordered using the ipiv pivot indices.

- **Signature**: `(JLFloat64Matrix,JLInt64Vector)->JLFloat64Matrix`

### `luReorder!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L116)\]

luReorder!(mat, ipiv) returns mat reordered in-place using the ipiv pivot indices.

- **Signature**: `(JLFloat64Matrix,JLInt64Vector)->JLFloat64Matrix`

### `mpInverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L119)\]

mpInverse(m) returns the Moore-Penrose pseudo inverse of m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Matrix`

### `norm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L24)\]

norm(m) computes the 2-norm of m, also known as the Frobenius norm.

- **Signature**: `(JLFloat64Matrix)->JLFloat64`

norm(m,p) computes the p-norm of m.

- **Signature**: `(JLFloat64Matrix,JLFloat64)->JLFloat64`

norm(v) computes the 2-norm of v.

- **Signature**: `(JLFloat64Vector)->JLFloat64`

norm(v,p) computes the p-norm of v.

- **Signature**: `(JLFloat64Vector,JLFloat64)->JLFloat64`

### `normalize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L14)\]

normalize(m) returns normalized m such that its norm equals 1.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Matrix`

normalize(v) returns normalized v such that its norm equals 1.

- **Signature**: `(JLFloat64Vector)->JLFloat64Vector`

### `normalize!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L16)\]

normalize!(m) destructively normalize m such that its norm equals 1.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Matrix`

normalize!(v) destructively normalize v such that norm(v) equals 1.

- **Signature**: `(JLFloat64Vector)->JLFloat64Vector`

### `operatorNorm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L33)\]

operatorNorm(m) computes the operator norm of m induced by the vector 2-norm.

- **Signature**: `(JLFloat64Matrix)->JLFloat64`

operatorNorm(m,p) computes the operator norm of m induced by the vector p-norm.

- **Signature**: `(JLFloat64Matrix,JLFloat64)->JLFloat64`

### `rank` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L47)\]

rank(m, tol) computes the rank of m. Counts singular value with magnitude greater than tol.

- **Signature**: `(JLFloat64Matrix,JLFloat64)->NonNegativeInteger`

### `rank!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L50)\]

rank!(m, tol) computes the rank of m. Counts singular value with magnitude greater than tol but overwrites m to save memory space.

- **Signature**: `(JLFloat64Matrix,JLFloat64)->NonNegativeInteger`

### `solve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L101)\]

solve(A,B) solves the matrix equation A*X=B, and returns X.

- **Signature**: `(JLFloat64Matrix,JLFloat64Matrix)->JLFloat64Matrix`

### `solve!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L103)\]

solve!(A,B) solves the matrix equation A*X=B. Overwrites B with matrix X and returns X.

- **Signature**: `(JLFloat64Matrix,JLFloat64Matrix)->JLFloat64Matrix`

### `sqrt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L121)\]

sqrt(m) returns the principal square root of m.

- **Signature**: `(JLFloat64Matrix)->JLComplexF64Matrix`

### `svd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L57)\]

svd(m) computes the singular value decomposition SVD of m such that SVD.U * diagonalMatrix(SVD.sv) * SVD.Vt =m.

- **Signature**: `(JLFloat64Matrix)->Record(U:JLFloat64Matrix,sv:JLFloat64Vector,Vt:JLFloat64Matrix)`

### `svd!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L60)\]

svd!(m) is the same as svd(m) but overwrites m to save memory space.

- **Signature**: `(JLFloat64Matrix)->Record(U:JLFloat64Matrix,sv:JLFloat64Vector,Vt:JLFloat64Matrix)`

### `svdvals` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L63)\]

svdvals(m) returns the singular values of m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Vector`

### `svdvals!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L65)\]

svdvals!(m) returns the singular values of m but overwrites m to save memory space.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Vector`

### `trace` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L45)\]

trace(m) computes the trace of m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64`

### `tril` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L133)\]

tril(m) returns the lower triangular matrix of m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Matrix`

### `tril!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L135)\]

tril!(m) overwrites m with its lower triangular matrix counterpart. Returns m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Matrix`

### `triu` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L128)\]

triu(m) returns the upper triangular matrix of m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Matrix`

### `triu!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L130)\]

triu!(m) overwrites m with its upper triangular matrix counterpart. Returns m.

- **Signature**: `(JLFloat64Matrix)->JLFloat64Matrix`
---
[Back to Index](../index.md)
