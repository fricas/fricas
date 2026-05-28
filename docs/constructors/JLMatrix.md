# JLMatrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1618)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a generic Julia matrix type stored in Julia with no bound checking on elt's. Minimum index is 1. Beware, for matrix with Nemo elements, contrary to Julia matrix, Nemo follows the C language convention, it wraps and uses row major representation.

**JLMatrix(R: JLObjectRing) is a domain constructor**  
**Abbreviation for JLMatrix is JMATRIX**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?*? : (%, R) -> % if R has SRNG
 ?*? : (R, %) -> % if R has SRNG                        ?*? : (%, %) -> % if R has SRNG
 ?*? : (Integer, %) -> % if R has ABELGRP               ?+? : (%, %) -> % if R has ABELMON
 -? : % -> % if R has ABELGRP                           ?-? : (%, %) -> % if R has ABELGRP
 ?/? : (%, R) -> % if R has FIELD                       ?=? : (%, %) -> Boolean
 Pfaffian : % -> R if R has COMRING                     ?^? : (%, Integer) -> % if R has FIELD
 antisymmetric? : % -> Boolean if R has ABELGRP         array2 : List(List(R)) -> %
 blockConcat : List(List(%)) -> %                       coerce : JLFloat32Matrix -> JLMatrix(JLObjFloat32)
 coerce : JLFloat64Matrix -> JLMatrix(JLObjFloat64)     coerce : % -> Matrix(R)
 coerce : JLVector(R) -> %                              coerce : % -> JLObject
 coerce : % -> OutputForm                               colSlice : % -> Segment(Integer)
 column : (%, Integer) -> JLVector(R)                   convert : % -> String
 copy : % -> %                                          determinant : % -> R if R has COMRING
 diagonal? : % -> Boolean if R has ABELMON              diagonalMatrix : JLVector(R) -> %
 diagonalMatrix : List(%) -> %                          diagonalMatrix : List(R) -> %
 eigenvalues : % -> JLVector(R) if R has NFIELD         elt : (%, List(Segment(Integer)), Integer) -> %
 elt : (%, Integer, List(Segment(Integer))) -> %        elt : (%, Segment(Integer), List(Integer)) -> %
 elt : (%, List(Integer), Segment(Integer)) -> %        elt : (%, List(Integer), List(Integer)) -> %
 elt : (%, List(Integer), Integer) -> %                 elt : (%, Integer, List(Integer)) -> %
 elt : (%, Integer, Integer, R) -> R                    elt : (%, Integer, Integer) -> R
 elt : (%, Integer) -> JLObject                         elt : (%, JLSymbol) -> JLObject
 empty : () -> %                                        empty? : % -> Boolean
 eq? : (%, %) -> Boolean                                factorize : JLMatrix(JLObjComplexF64) -> JLObject
 factorize : JLMatrix(JLObjFloat64) -> JLObject         factorize : JLMatrix(JLObjComplexF32) -> JLObject
 factorize : JLMatrix(JLObjFloat32) -> JLObject         fill! : (%, R) -> %
 hash : % -> SingleInteger if R has HASHABL             horizConcat : List(%) -> %
 horizConcat : (%, %) -> %                              horizSplit : (%, PositiveInteger) -> List(%)
 identity : NonNegativeInteger -> %                     inverse : % -> %
 jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlId : % -> JLInt64                                    jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 jmatrix : String -> %                                  kroneckerProduct : List(%) -> % if R has SRNG
 kroneckerProduct : (%, %) -> % if R has SRNG           kroneckerSum : List(%) -> %
 kroneckerSum : (%, %) -> %                             latex : % -> String
 less? : (%, NonNegativeInteger) -> Boolean             listOfLists : % -> List(List(R))
 map : (((R, R) -> R), %, %, R) -> %                    map : (((R, R) -> R), %, %) -> %
 map : ((R -> R), %) -> %                               map! : ((R -> R), %) -> %
 matrix : List(List(R)) -> %                            maxColIndex : % -> Integer
 maxRowIndex : % -> Integer                             members : % -> List(R) if % has ATFINAG
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 minordet : % -> R if R has COMRING                     missing? : % -> Boolean
 more? : (%, NonNegativeInteger) -> Boolean             mutable? : % -> Boolean
 ncols : % -> NonNegativeInteger                        nothing? : % -> Boolean
 nrows : % -> NonNegativeInteger                        parts : % -> List(R)
 qelt : (%, Integer, Integer) -> R                      qelt : (%, Integer) -> JLObject
 qelt : (%, JLSymbol) -> JLObject                       qsetelt! : (%, Integer, Integer, R) -> R
 rank : % -> NonNegativeInteger if R has INTDOM         row : (%, Integer) -> JLVector(R)
 rowEchelon : % -> % if R has EUCDOM                    rowSlice : % -> Segment(Integer)
 sample : () -> %                                       scalarMatrix : (NonNegativeInteger, R) -> %
 setColumn! : (%, Integer, JLVector(R)) -> %            setRow! : (%, Integer, JLVector(R)) -> %
 setelt! : (%, List(Integer), Integer, %) -> %          setelt! : (%, Integer, List(Integer), %) -> %
 setelt! : (%, Integer, Integer, R) -> R                setsubMatrix! : (%, Integer, Integer, %) -> %
 size? : (%, NonNegativeInteger) -> Boolean             smaller? : (%, %) -> Boolean if R has COMPAR
 square? : % -> Boolean                                 squareTop : % -> %
 string : % -> String                                   swapColumns! : (%, Integer, Integer) -> %
 swapRows! : (%, Integer, Integer) -> %                 symmetric? : % -> Boolean
 trace : % -> R                                         transpose : JLVector(R) -> %
 transpose : % -> %                                     vertConcat : List(%) -> %
 vertConcat : (%, %) -> %                               vertSplit : (%, PositiveInteger) -> List(%)
 zero? : % -> Boolean if R has ABELMON                  ?~=? : (%, %) -> Boolean
 ?*? : (JLVector(R), %) -> JLVector(R) if R has SRNG
 ?*? : (%, JLVector(R)) -> JLVector(R) if R has SRNG
 ?^? : (%, NonNegativeInteger) -> % if R has MONOID and R has SRNG
 any? : ((R -> Boolean), %) -> Boolean if % has ATFINAG
 blockSplit : (%, List(NonNegativeInteger), List(NonNegativeInteger)) -> List(List(%))
 blockSplit : (%, PositiveInteger, PositiveInteger) -> List(List(%))
 coerce : JLComplexF32Matrix -> JLMatrix(JLObjComplexF32)
 coerce : JLComplexF64Matrix -> JLMatrix(JLObjComplexF64)
 columnSpace : % -> List(JLVector(R)) if R has EUCDOM
 count : (R, %) -> NonNegativeInteger if % has ATFINAG and R has BASTYPE
 count : ((R -> Boolean), %) -> NonNegativeInteger if % has ATFINAG
 eigenSpaces : (String, %, JLSymbol) -> JLObjDict if R has NRING
 eigenSpaces : (%, JLSymbol) -> JLObjDict if R has NFIELD
 eigenvalues : (String, %) -> JLVector(JLObject) if R has NRING
 eigenvaluesWithMultiplicities : (String, %) -> JLVector(JLObjTuple) if R has NRING
 eigenvaluesWithMultiplicities : % -> JLVector(JLObjTuple) if R has NFIELD
 elt : (%, List(Segment(Integer)), List(Segment(Integer))) -> %
 elt : (%, List(Segment(Integer)), Segment(Integer)) -> %
 elt : (%, Segment(Integer), List(Segment(Integer))) -> %
 elt : (%, Segment(Integer), Segment(Integer)) -> %
 eval : (%, List(R), List(R)) -> % if R has EVALAB(R) and R has SETCAT
 eval : (%, R, R) -> % if R has EVALAB(R) and R has SETCAT
 eval : (%, Equation(R)) -> % if R has EVALAB(R) and R has SETCAT
 eval : (%, List(Equation(R))) -> % if R has EVALAB(R) and R has SETCAT
 every? : ((R -> Boolean), %) -> Boolean if % has ATFINAG
 exprand : (PositiveInteger, PositiveInteger) -> JLMatrix(JLFloat) if R has FPS and R has JOBRING and R has ATARBPR and not R has NTYPE
 ?exquo? : (%, R) -> Union(%,"failed") if R has INTDOM
 hashUpdate! : (HashState, %) -> HashState if R has HASHABL
 horizSplit : (%, List(NonNegativeInteger)) -> List(%)
 inverse : % -> Union(%,"failed") if R has FIELD
 invertIfCan : % -> Union(%,"failed") if R has INTDOM
 jlApprox? : (%, %) -> Boolean if not R has NTYPE
 kronecker_prod1 : (%, Integer, List(List(NonNegativeInteger)), List(%), NonNegativeInteger, NonNegativeInteger, Union(R,one)) -> Void
 matrix : (NonNegativeInteger, NonNegativeInteger, ((Integer, Integer) -> R)) -> %
 max : % -> R if % has ATFINAG and R has ORDSET
 max : (((R, R) -> Boolean), %) -> R if % has ATFINAG
 member? : (R, %) -> Boolean if % has ATFINAG and R has BASTYPE
 min : % -> R if % has ATFINAG and R has ORDSET
 new : (NonNegativeInteger, NonNegativeInteger, R) -> %
 nrand : (PositiveInteger, PositiveInteger) -> JLMatrix(JLComplexFloat) if R has COMPCAT(JFLOAT) and R has JOBRING and not R has NTYPE
 nrand : (PositiveInteger, PositiveInteger) -> JLMatrix(JLFloat) if R has FPS and R has JOBRING and R has ATARBPR and not R has NTYPE
 nullSpace : % -> List(JLVector(R)) if R has INTDOM
 nullity : % -> NonNegativeInteger if R has INTDOM
 positivePower : (%, Integer) -> % if R has SRNG
 qnew : (NonNegativeInteger, NonNegativeInteger) -> %
 radicalEigenvalues : % -> JLVector(NMAlgebraicNumber) if R has INS and R has NRING or R has ALGEBRA(NFRAC(NINT)) and R has NRING and not R has INS
 radicalEigenvaluesWithMultiplicities : % -> JLVector(JLObjTuple) if R has INS and R has NRING or R has ALGEBRA(NFRAC(NINT)) and R has NRING and not R has INS
 setelt! : (%, List(Segment(Integer)), List(Segment(Integer)), %) -> %
 setelt! : (%, List(Segment(Integer)), Segment(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Segment(Integer)), %) -> %
 setelt! : (%, Segment(Integer), List(Integer), %) -> %
 setelt! : (%, List(Integer), Segment(Integer), %) -> %
 setelt! : (%, Segment(Integer), Segment(Integer), %) -> %
 setelt! : (%, List(Integer), List(Integer), %) -> %
 setelt! : (%, List(Segment(Integer)), Integer, %) -> %
 setelt! : (%, Integer, List(Segment(Integer)), %) -> %
 subMatrix : (%, Integer, Integer, Integer, Integer) -> %
 urand01 : (PositiveInteger, PositiveInteger) -> JLMatrix(JLComplexFloat) if R has COMPCAT(JFLOAT) and R has JOBRING and not R has NTYPE
 urand01 : (PositiveInteger, PositiveInteger) -> JLMatrix(JLFloat) if R has FPS and R has JOBRING and R has ATARBPR and not R has NTYPE
 vertSplit : (%, List(NonNegativeInteger)) -> List(%)
 zero : (NonNegativeInteger, NonNegativeInteger) -> %
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1737)\]

coerce(x): convenience function.

- **Signature**: `(JLComplexF32Matrix)->JLMatrix(JLObjComplexF32)`
- **Signature**: `(JLComplexF64Matrix)->JLMatrix(JLObjComplexF64)`
- **Signature**: `(JLFloat32Matrix)->JLMatrix(JLObjFloat32)`
- **Signature**: `(JLFloat64Matrix)->JLMatrix(JLObjFloat64)`

coerce(m) coerces a copy of m to a Matrix(R).

- **Signature**: `(%)->Matrix(R)`

### `diagonalMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1735)\]

diagonalMatrix(v) returns a diagonal matrix with elements of v.

- **Signature**: `(JLVector(R))->%`

### `eigenSpaces` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1651)\]

eigenSpace(mat, side) returns a Julia Dict containing the :left or :right side of the eigen space of mat associated to the eigenvalue key. 

**Example**:
```fricas
a:=matrix([[111::NPF(127),91,50],[31,6,97],[117,63,6]])$JLMatrix(NPF(127))
```

**Example**:
```fricas
eigenSpaces(a, "left")
```

- **Signature**: `(%,JLSymbol)->JLObjDict`

eigenSpaces(JRing, mat, side) returns a Julia Dict containing the :left or :right side of the eigen space of mat associated to the eigenvalue key and the field JRing. 

**Example**:
```fricas
a:=matrix([[111::NINT,91,50],[31,6,97],[117 ,63,6]])$JLMatrix(NINT)
```

**Example**:
```fricas
eigenSpaces(jlNMRing()$NAN, a, "left")
```

- **Signature**: `(String,%,JLSymbol)->JLObjDict`

### `eigenvalues` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1656)\]

eigenvalues(mat) returns a Julia vector containing the eigenvalues of mat. 

**Example**:
```fricas
a:=matrix([[111::NPF(127) ,91,50],[31,6,97],[117,63,6]])$JLMatrix(NPF(127))
```

**Example**:
```fricas
eigenvalues(a)
```

- **Signature**: `(%)->JLVector(R)`

eigenvalues(JRing, mat) returns a Julia vector containing the eigenvalues of mat in the field JRing. The Julia vector of JLObject is of internal type JLRing. 

**Example**:
```fricas
a:=matrix([[111::NINT,91,50],[31,6,97],[117,63,6]])$ JLMatrix(NINT)
```

**Example**:
```fricas
eigenvalues(jlNMRing()$NCF, a)
```

- **Signature**: `(String,%)->JLVector(JLObject)`

### `eigenvaluesWithMultiplicities` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1660)\]

eigenvaluesWithMultiplicities(mat) returns a Julia vector containing Julia tuples of the eigenvalues and their multiplicities. The tuples are of internal type (%, JLObjInt64). 

**Example**:
```fricas
a:=matrix([[111::NPF(127),91,50],[ 31,6,97],[117,63,6]])$JLMatrix(NPF(127))
```

**Example**:
```fricas
eigenvaluesWithMultiplicities(a)
```

- **Signature**: `(%)->JLVector(JLObjTuple)`

eigenvaluesWithMultiplicities(JRing, mat) returns a Julia vector containing Julia tuples of the eigenvalues and their multiplicities in the field JRing. The tuples are of internal type (JLRing, JLObjInt64).

**Example**:
```fricas
a:= matrix([[111::NINT,91,50],[31,6,97],[117,63,6]])$JLMatrix(NINT)
```

**Example**:
```fricas
eigenvaluesWithMultiplicities(jlNMR ing()$NCF, a)
```

- **Signature**: `(String,%)->JLVector(JLObjTuple)`

### `exprand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1708)\]

exprand(m,n) returns a JLMatrix of size (m,n) with exponentially distributed random numbers.

**Example**:
```fricas
mat := exprand(4,4)$JLMatrix(JLFloat)
```

**Example**:
```fricas
svd := jlApply("svd", mat)
```

- **Signature**: `(PositiveInteger,PositiveInteger)->JLMatrix(JLFloat)`

### `factorize` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1747)\]

factorize(m) factorizes m using a suited matrix factorization for m. For a symmetric matrix the Bunch-Kaufmanfactorization will be chosen whereas for generic matrices, a LU or a QR factorization will be used.

- **Signature**: `(JLMatrix(JLObjComplexF32))->JLObject`
- **Signature**: `(JLMatrix(JLObjComplexF64))->JLObject`
- **Signature**: `(JLMatrix(JLObjFloat32))->JLObject`
- **Signature**: `(JLMatrix(JLObjFloat64))->JLObject`

### `identity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1733)\]

identity(n) returns a n by n identity matrix.

- **Signature**: `(NonNegativeInteger)->%`

### `inverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1727)\]

inverse(m) returns inverse matrix. Throws a Julia error if m is not invertible.

- **Signature**: `(%)->%`

### `invertIfCan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1684)\]

invertIfCan(m) returns the inverse of the matrix m. If the matrix is not invertible, "failed" is returned. Error: if the matrix is not square.

- **Signature**: `(%)->Union(%,"failed")`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1689)\]

jlApprox?(A,B) computes component-wise inexact equality with default parameters. Two numbers compare equal iftheir relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%)->Boolean`

### `jmatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1763)\]

jmatrix(str) evaluates the string str and returns the generated matrix. No checks are done at the FriCAS level.

- **Signature**: `(String)->%`

### `new` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1639)\]

new(m, n, x) creates a matrix of size m by n with all elements x.

- **Signature**: `(NonNegativeInteger,NonNegativeInteger,R)->%`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1702)\]

nrand(m,n) returns a JLMatrix of size (m,n) with normally distributed random numbers.

**Example**:
```fricas
mat := nrand(4 ,4)$JLMatrix(JLComplexFloat)
```

**Example**:
```fricas
qr := jlApply("qr", mat)
```

**Example**:
```fricas
qr.Q * qr.R
```

- **Signature**: `(PositiveInteger,PositiveInteger)->JLMatrix(JLComplexFloat)`

nrand(m,n) returns a JLMatrix of size (m,n) with normally distributed random numbers. For example:

**Example**:
```fricas
m at := nrand(4,4)$JLMatrix(JLFloat)
```

**Example**:
```fricas
chol := jlApply("cholesky", mat * transpose(mat))
```

**Example**:
```fricas
cho l.L * chol.U
```

- **Signature**: `(PositiveInteger,PositiveInteger)->JLMatrix(JLFloat)`

### `radicalEigenvalues` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1643)\]

radicalEigenvalues(mat) returns a Julia vector containing the eigenvalues of mat.

- **Signature**: `(%)->JLVector(NMAlgebraicNumber)`
- **Signature**: `(%)->JLVector(NMAlgebraicNumber)`

### `radicalEigenvaluesWithMultiplicities` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1646)\]

radicalEigenvaluesWithMultiplicities(mat) returns a Julia vector containing Julia tuples of the eigenvalues and their multiplicities. The tuples are of internal type (NMAlgebraicNumber, JLObjInt64).

- **Signature**: `(%)->JLVector(JLObjTuple)`
- **Signature**: `(%)->JLVector(JLObjTuple)`

### `trace` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1730)\]

trace(m) returns the trace of square matrix m. Julia error if m is not square.

- **Signature**: `(%)->R`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1696)\]

urand01(m,n) returns a JLMatrix of size (m,n) with uniformly distributed random number contained in the unit disk. 

**Example**:
```fricas
mat := urand01(4,4)$JLMatrix(JLComplexFloat)
```

**Example**:
```fricas
qr := jlApply("qr", mat)
```

**Example**:
```fricas
qr. Q * qr.R
```

- **Signature**: `(PositiveInteger,PositiveInteger)->JLMatrix(JLComplexFloat)`

urand01(m,n) returns a JLMatrix of size (m,n) with uniformly distributed random number contained in [0,1].

**Example**:
```fricas
mat := urand01(4,4)$JLMatrix(JLFloat)
```

**Example**:
```fricas
qr := jlApply("qr", mat)
```

**Example**:
```fricas
qr.Q * qr.R
```

- **Signature**: `(PositiveInteger,PositiveInteger)->JLMatrix(JLFloat)`
---
[Back to Index](../index.md)
