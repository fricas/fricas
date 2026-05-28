# JLComplexF64Matrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L751)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast JLComplexF64 matrix type with no bound checking on elt's. Minimum index is 1.

**JLComplexF64Matrix is a domain constructor.**  
**Abbreviation for JLComplexF64Matrix is JCF64MAT**  
**This constructor is not exposed in this frame.**  
**103 names for 155 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?*? : (%, %) -> %
 ?*? : (%, JLComplexF64) -> %                           ?*? : (Integer, %) -> %
 ?*? : (JLComplexF64, %) -> %                           ?+? : (%, %) -> %
 -? : % -> %                                            ?-? : (%, %) -> %
 ?/? : (%, JLComplexF64) -> %                           ?=? : (%, %) -> Boolean
 Pfaffian : % -> JLComplexF64                           ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     adjoint : % -> %
 adjoint! : (%, %) -> %                                 antisymmetric? : % -> Boolean
 any? : ((JLComplexF64 -> Boolean), %) -> Boolean       array2 : List(List(JLComplexF64)) -> %
 blockConcat : List(List(%)) -> %                       coerce : JLComplexF64Vector -> %
 coerce : JLFloat64Matrix -> %                          coerce : % -> OutputForm
 colSlice : % -> Segment(Integer)                       column : (%, Integer) -> JLComplexF64Vector
 columnSpace : % -> List(JLComplexF64Vector)            convert : % -> String
 copy : % -> %                                          copy! : (%, %) -> %
 count : (JLComplexF64, %) -> NonNegativeInteger        determinant : % -> JLComplexF64
 diagonal? : % -> Boolean                               diagonalMatrix : JLComplexF64Vector -> %
 diagonalMatrix : List(%) -> %                          diagonalMatrix : List(JLComplexF64) -> %
 elt : (%, Integer, List(Integer)) -> %                 elt : (%, Integer, List(Segment(Integer))) -> %
 elt : (%, List(Integer), Integer) -> %                 elt : (%, List(Integer), List(Integer)) -> %
 elt : (%, List(Integer), Segment(Integer)) -> %        elt : (%, List(Segment(Integer)), Integer) -> %
 elt : (%, Segment(Integer), List(Integer)) -> %        elt : (%, Integer, Integer) -> JLComplexF64
 elt : (%, NonNegativeInteger) -> JLComplexF64          empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 every? : ((JLComplexF64 -> Boolean), %) -> Boolean     ?exquo? : (%, JLComplexF64) -> Union(%,"failed")
 fill! : (%, JLComplexF64) -> %                         hermitian? : % -> Boolean
 hermitianPart : % -> %                                 hermitianPart! : % -> %
 horizConcat : (%, %) -> %                              horizConcat : List(%) -> %
 horizSplit : (%, PositiveInteger) -> List(%)           imag : % -> JLFloat64Matrix
 inverse : % -> Union(%,"failed")                       jlApply : (String, %) -> JLFloat64
 jlApply : (String, %, JLFloat64) -> JLFloat64          jlApply : (String, %) -> Void
 jlApprox? : (%, %) -> Boolean                          jlDisplay : % -> Void
 jlVector : % -> JLComplexF64Vector                     juliaCMPrint : Boolean -> Boolean
 kroneckerProduct : (%, %) -> %                         kroneckerProduct : List(%) -> %
 kroneckerSum : (%, %) -> %                             kroneckerSum : List(%) -> %
 latex : % -> String                                    less? : (%, NonNegativeInteger) -> Boolean
 listOfLists : % -> List(List(JLComplexF64))            map : ((JLComplexF64 -> JLComplexF64), %) -> %
 map! : ((JLComplexF64 -> JLComplexF64), %) -> %        matrix : List(List(JLComplexF64)) -> %
 maxColIndex : % -> Integer                             maxRowIndex : % -> Integer
 member? : (JLComplexF64, %) -> Boolean                 members : % -> List(JLComplexF64)
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 minordet : % -> JLComplexF64                           more? : (%, NonNegativeInteger) -> Boolean
 ncols : % -> NonNegativeInteger                        nrand : (PositiveInteger, PositiveInteger) -> %
 nrows : % -> NonNegativeInteger                        nullSpace : % -> List(JLComplexF64Vector)
 nullity : % -> NonNegativeInteger                      parts : % -> List(JLComplexF64)
 positiveDefinite? : % -> Boolean                       positivePower : (%, Integer) -> %
 qelt : (%, Integer, Integer) -> JLComplexF64           qelt : (%, NonNegativeInteger) -> JLComplexF64
 rank : % -> NonNegativeInteger                         real : % -> JLFloat64Matrix
 row : (%, Integer) -> JLComplexF64Vector               rowEchelon : % -> %
 rowSlice : % -> Segment(Integer)                       sample : () -> %
 setRow! : (%, Integer, JLComplexF64Vector) -> %        setelt! : (%, Integer, List(Integer), %) -> %
 setelt! : (%, List(Integer), Integer, %) -> %          setsubMatrix! : (%, Integer, Integer, %) -> %
 size? : (%, NonNegativeInteger) -> Boolean             smaller? : (%, %) -> Boolean
 square? : % -> Boolean                                 squareTop : % -> %
 string : % -> String                                   swapColumns! : (%, Integer, Integer) -> %
 swapRows! : (%, Integer, Integer) -> %                 symmetric? : % -> Boolean
 transpose : % -> %                                     transpose : JLComplexF64Vector -> %
 transpose! : (%, %) -> %                               urand01 : (PositiveInteger, PositiveInteger) -> %
 vertConcat : (%, %) -> %                               vertConcat : List(%) -> %
 vertSplit : (%, PositiveInteger) -> List(%)            zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 ?*? : (%, JLComplexF64Vector) -> JLComplexF64Vector
 ?*? : (JLComplexF64Vector, %) -> JLComplexF64Vector
 blockSplit : (%, List(NonNegativeInteger), List(NonNegativeInteger)) -> List(List(%))
 blockSplit : (%, PositiveInteger, PositiveInteger) -> List(List(%))
 count : ((JLComplexF64 -> Boolean), %) -> NonNegativeInteger
 elt : (%, List(Segment(Integer)), List(Segment(Integer))) -> %
 elt : (%, List(Segment(Integer)), Segment(Integer)) -> %
 elt : (%, Segment(Integer), List(Segment(Integer))) -> %
 elt : (%, Segment(Integer), Segment(Integer)) -> %
 elt : (%, Integer, Integer, JLComplexF64) -> JLComplexF64
 horizSplit : (%, List(NonNegativeInteger)) -> List(%)
 kronecker_prod1 : (%, Integer, List(List(NonNegativeInteger)), List(%), NonNegativeInteger, NonNegativeInteger, Union(JLComplexF64,one)) -> Void
 map : (((JLComplexF64, JLComplexF64) -> JLComplexF64), %, %) -> %
 map : (((JLComplexF64, JLComplexF64) -> JLComplexF64), %, %, JLComplexF64) -> %
 matrix : (NonNegativeInteger, NonNegativeInteger, ((Integer, Integer) -> JLComplexF64)) -> %
 max : (((JLComplexF64, JLComplexF64) -> Boolean), %) -> JLComplexF64
 new : (NonNegativeInteger, NonNegativeInteger, JLComplexF64) -> %
 qnew : (NonNegativeInteger, NonNegativeInteger) -> %
 qsetelt! : (%, Integer, Integer, JLComplexF64) -> JLComplexF64
 qsetelt! : (%, NonNegativeInteger, JLComplexF64) -> JLComplexF64
 scalarMatrix : (NonNegativeInteger, JLComplexF64) -> %
 setColumn! : (%, Integer, JLComplexF64Vector) -> %
 setelt! : (%, Integer, List(Segment(Integer)), %) -> %
 setelt! : (%, List(Integer), List(Integer), %) -> %
 setelt! : (%, List(Integer), Segment(Integer), %) -> %
 setelt! : (%, List(Segment(Integer)), Integer, %) -> %
 setelt! : (%, List(Segment(Integer)), List(Segment(Integer)), %) -> %
 setelt! : (%, List(Segment(Integer)), Segment(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Segment(Integer)), %) -> %
 setelt! : (%, Segment(Integer), Segment(Integer), %) -> %
 setelt! : (%, Integer, Integer, JLComplexF64) -> JLComplexF64
 setelt! : (%, NonNegativeInteger, JLComplexF64) -> JLComplexF64
 subMatrix : (%, Integer, Integer, Integer, Integer) -> %
 vertSplit : (%, List(NonNegativeInteger)) -> List(%)
 zero : (NonNegativeInteger, NonNegativeInteger) -> %
```

## Operations added

### `adjoint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L825)\]

adjoint(m) returns the adjoint of m i.e. the conjugate transposition of m.

- **Signature**: `(%)->%`

### `adjoint!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L828)\]

adjoint!(a, m) stores in a the adjoint of m i.e. the conjugate transposition of m.

- **Signature**: `(%,%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L779)\]

coerce(m) coerces m to a Julia Complex Float 64 matrix.

- **Signature**: `(JLFloat64Matrix)->%`

### `copy!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L812)\]

copy!(b,a) efficiently copies a to b. No checks are performed on array dimensions.

- **Signature**: `(%,%)->%`

### `diagonalMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L815)\]

diagonalMatrix(v) returns a diagonal matrix with elements of v.

- **Signature**: `(JLComplexF64Vector)->%`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L767)\]

elt(m, i) returns the i-th element of m. Column major order.

- **Signature**: `(%,NonNegativeInteger)->JLComplexF64`

### `hermitian?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L797)\]

hermitian?(m) tests hermiticity of m.

- **Signature**: `(%)->Boolean`

### `hermitianPart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L807)\]

hermitianPart(m) returns the symmetric part of m (m + m')/2.

- **Signature**: `(%)->%`

### `hermitianPart!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L809)\]

hermitianPart!(m) overwrites m with the symmetric part of m (m + m')/2 to save memory space and returns m.

- **Signature**: `(%)->%`

### `imag` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L788)\]

imag(m) returns a JLFloat64 matrix with the imaginary part of the m elements.

- **Signature**: `(%)->JLFloat64Matrix`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L791)\]

jlApply(func, x) applies func to argument x.

- **Signature**: `(String,%)->JLFloat64`
- **Signature**: `(String,%)->Void`

jlApply(func, x, val) applies func to arguments x and val.

- **Signature**: `(String,%,JLFloat64)->JLFloat64`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L802)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L781)\]

jlDisplay(m) pretty prints m (à la Julia).

- **Signature**: `(%)->Void`

### `jlVector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L783)\]

jlVector(m) returns reference to the internal vector representation.

- **Signature**: `(%)->JLComplexF64Vector`

### `juliaCMPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L831)\]

juliaCMPrint(b) defines whether or not FriCAS uses the printing version of Julia for matrices instead of its OutputForm version. Returns previous value. By default it is the Julia version.

- **Signature**: `(Boolean)->Boolean`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L819)\]

nrand(m,n) returns a Julia matrix of size (m,n) with random elements from the normal distribution with mean=0See Julia documentation (randn) for this complex version used here.

- **Signature**: `(PositiveInteger,PositiveInteger)->%`

### `positiveDefinite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L799)\]

positiveDefinite?(m) tests whether or not m is Hermitian positive definite using a Cholesky factorisation.

- **Signature**: `(%)->Boolean`

### `qelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L770)\]

qelt(m, i) returns the i-th element of m. Column major order.

- **Signature**: `(%,NonNegativeInteger)->JLComplexF64`

### `qsetelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L776)\]

qsetelt!(m, i, r) sets the i-th element of m to r. Column major order.

- **Signature**: `(%,NonNegativeInteger,JLComplexF64)->JLComplexF64`

### `real` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L785)\]

real(m) returns a JLFloat64 matrix with the real part of the m elements.

- **Signature**: `(%)->JLFloat64Matrix`

### `setelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L773)\]

setelt!(m, i, r) sets the i-th element of m to r. Column major order.

- **Signature**: `(%,NonNegativeInteger,JLComplexF64)->JLComplexF64`

### `transpose!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L823)\]

transpose!(b, a) stores transposed a in b.

- **Signature**: `(%,%)->%`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L817)\]

urand01(m,n) returns a uniform(0..1) Julia matrix of size (m,n).

- **Signature**: `(PositiveInteger,PositiveInteger)->%`
---
[Back to Index](../index.md)
