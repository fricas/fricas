# JLComplexF32Matrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L681)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast JLComplexF32 matrix type with no bound checking on elt's. Minimum index is 1.

**JLComplexF32Matrix is a domain constructor.**  
**Abbreviation for JLComplexF32Matrix is JCF32MAT**  
**This constructor is not exposed in this frame.**  
**103 names for 155 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?*? : (%, %) -> %
 ?*? : (%, JLComplexF32) -> %                           ?*? : (Integer, %) -> %
 ?*? : (JLComplexF32, %) -> %                           ?+? : (%, %) -> %
 -? : % -> %                                            ?-? : (%, %) -> %
 ?/? : (%, JLComplexF32) -> %                           ?=? : (%, %) -> Boolean
 Pfaffian : % -> JLComplexF32                           ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     adjoint : % -> %
 adjoint! : (%, %) -> %                                 antisymmetric? : % -> Boolean
 any? : ((JLComplexF32 -> Boolean), %) -> Boolean       array2 : List(List(JLComplexF32)) -> %
 blockConcat : List(List(%)) -> %                       coerce : JLComplexF32Vector -> %
 coerce : JLFloat32Matrix -> %                          coerce : % -> OutputForm
 colSlice : % -> Segment(Integer)                       column : (%, Integer) -> JLComplexF32Vector
 columnSpace : % -> List(JLComplexF32Vector)            convert : % -> String
 copy : % -> %                                          copy! : (%, %) -> %
 count : (JLComplexF32, %) -> NonNegativeInteger        determinant : % -> JLComplexF32
 diagonal? : % -> Boolean                               diagonalMatrix : JLComplexF32Vector -> %
 diagonalMatrix : List(%) -> %                          diagonalMatrix : List(JLComplexF32) -> %
 elt : (%, Integer, List(Integer)) -> %                 elt : (%, Integer, List(Segment(Integer))) -> %
 elt : (%, List(Integer), Integer) -> %                 elt : (%, List(Integer), List(Integer)) -> %
 elt : (%, List(Integer), Segment(Integer)) -> %        elt : (%, List(Segment(Integer)), Integer) -> %
 elt : (%, Segment(Integer), List(Integer)) -> %        elt : (%, Integer, Integer) -> JLComplexF32
 elt : (%, NonNegativeInteger) -> JLComplexF32          empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 every? : ((JLComplexF32 -> Boolean), %) -> Boolean     ?exquo? : (%, JLComplexF32) -> Union(%,"failed")
 fill! : (%, JLComplexF32) -> %                         hermitian? : % -> Boolean
 hermitianPart : % -> %                                 hermitianPart! : % -> %
 horizConcat : (%, %) -> %                              horizConcat : List(%) -> %
 horizSplit : (%, PositiveInteger) -> List(%)           imag : % -> JLFloat32Matrix
 inverse : % -> Union(%,"failed")                       jlApply : (String, %) -> JLFloat32
 jlApply : (String, %, JLFloat32) -> JLFloat32          jlApply : (String, %) -> Void
 jlApprox? : (%, %) -> Boolean                          jlDisplay : % -> Void
 jlVector : % -> JLComplexF32Vector                     juliaCMPrint : Boolean -> Boolean
 kroneckerProduct : (%, %) -> %                         kroneckerProduct : List(%) -> %
 kroneckerSum : (%, %) -> %                             kroneckerSum : List(%) -> %
 latex : % -> String                                    less? : (%, NonNegativeInteger) -> Boolean
 listOfLists : % -> List(List(JLComplexF32))            map : ((JLComplexF32 -> JLComplexF32), %) -> %
 map! : ((JLComplexF32 -> JLComplexF32), %) -> %        matrix : List(List(JLComplexF32)) -> %
 maxColIndex : % -> Integer                             maxRowIndex : % -> Integer
 member? : (JLComplexF32, %) -> Boolean                 members : % -> List(JLComplexF32)
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 minordet : % -> JLComplexF32                           more? : (%, NonNegativeInteger) -> Boolean
 ncols : % -> NonNegativeInteger                        nrand : (PositiveInteger, PositiveInteger) -> %
 nrows : % -> NonNegativeInteger                        nullSpace : % -> List(JLComplexF32Vector)
 nullity : % -> NonNegativeInteger                      parts : % -> List(JLComplexF32)
 positiveDefinite? : % -> Boolean                       positivePower : (%, Integer) -> %
 qelt : (%, Integer, Integer) -> JLComplexF32           qelt : (%, NonNegativeInteger) -> JLComplexF32
 rank : % -> NonNegativeInteger                         real : % -> JLFloat32Matrix
 row : (%, Integer) -> JLComplexF32Vector               rowEchelon : % -> %
 rowSlice : % -> Segment(Integer)                       sample : () -> %
 setRow! : (%, Integer, JLComplexF32Vector) -> %        setelt! : (%, Integer, List(Integer), %) -> %
 setelt! : (%, List(Integer), Integer, %) -> %          setsubMatrix! : (%, Integer, Integer, %) -> %
 size? : (%, NonNegativeInteger) -> Boolean             smaller? : (%, %) -> Boolean
 square? : % -> Boolean                                 squareTop : % -> %
 string : % -> String                                   swapColumns! : (%, Integer, Integer) -> %
 swapRows! : (%, Integer, Integer) -> %                 symmetric? : % -> Boolean
 transpose : % -> %                                     transpose : JLComplexF32Vector -> %
 transpose! : (%, %) -> %                               urand01 : (PositiveInteger, PositiveInteger) -> %
 vertConcat : (%, %) -> %                               vertConcat : List(%) -> %
 vertSplit : (%, PositiveInteger) -> List(%)            zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 ?*? : (%, JLComplexF32Vector) -> JLComplexF32Vector
 ?*? : (JLComplexF32Vector, %) -> JLComplexF32Vector
 blockSplit : (%, List(NonNegativeInteger), List(NonNegativeInteger)) -> List(List(%))
 blockSplit : (%, PositiveInteger, PositiveInteger) -> List(List(%))
 count : ((JLComplexF32 -> Boolean), %) -> NonNegativeInteger
 elt : (%, List(Segment(Integer)), List(Segment(Integer))) -> %
 elt : (%, List(Segment(Integer)), Segment(Integer)) -> %
 elt : (%, Segment(Integer), List(Segment(Integer))) -> %
 elt : (%, Segment(Integer), Segment(Integer)) -> %
 elt : (%, Integer, Integer, JLComplexF32) -> JLComplexF32
 horizSplit : (%, List(NonNegativeInteger)) -> List(%)
 kronecker_prod1 : (%, Integer, List(List(NonNegativeInteger)), List(%), NonNegativeInteger, NonNegativeInteger, Union(JLComplexF32,one)) -> Void
 map : (((JLComplexF32, JLComplexF32) -> JLComplexF32), %, %) -> %
 map : (((JLComplexF32, JLComplexF32) -> JLComplexF32), %, %, JLComplexF32) -> %
 matrix : (NonNegativeInteger, NonNegativeInteger, ((Integer, Integer) -> JLComplexF32)) -> %
 max : (((JLComplexF32, JLComplexF32) -> Boolean), %) -> JLComplexF32
 new : (NonNegativeInteger, NonNegativeInteger, JLComplexF32) -> %
 qnew : (NonNegativeInteger, NonNegativeInteger) -> %
 qsetelt! : (%, Integer, Integer, JLComplexF32) -> JLComplexF32
 qsetelt! : (%, NonNegativeInteger, JLComplexF32) -> JLComplexF32
 scalarMatrix : (NonNegativeInteger, JLComplexF32) -> %
 setColumn! : (%, Integer, JLComplexF32Vector) -> %
 setelt! : (%, Integer, List(Segment(Integer)), %) -> %
 setelt! : (%, List(Integer), List(Integer), %) -> %
 setelt! : (%, List(Integer), Segment(Integer), %) -> %
 setelt! : (%, List(Segment(Integer)), Integer, %) -> %
 setelt! : (%, List(Segment(Integer)), List(Segment(Integer)), %) -> %
 setelt! : (%, List(Segment(Integer)), Segment(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Segment(Integer)), %) -> %
 setelt! : (%, Segment(Integer), Segment(Integer), %) -> %
 setelt! : (%, Integer, Integer, JLComplexF32) -> JLComplexF32
 setelt! : (%, NonNegativeInteger, JLComplexF32) -> JLComplexF32
 subMatrix : (%, Integer, Integer, Integer, Integer) -> %
 vertSplit : (%, List(NonNegativeInteger)) -> List(%)
 zero : (NonNegativeInteger, NonNegativeInteger) -> %
```

## Operations added

### `adjoint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L760)\]

adjoint(m) returns the adjoint of m i.e. the conjugate transposition of m.

- **Signature**: `(%)->%`

### `adjoint!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L763)\]

adjoint!(a, m) stores in a the adjoint of m i.e. the conjugate transposition of m.

- **Signature**: `(%,%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L712)\]

coerce(m) coerces m to a Julia Complex Float 32 matrix.

- **Signature**: `(JLFloat32Matrix)->%`

### `copy!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L745)\]

copy!(b,a) efficiently copies a to b. No checks are performed on array dimensions.

- **Signature**: `(%,%)->%`

### `diagonalMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L748)\]

diagonalMatrix(v) returns a diagonal matrix with elements of v.

- **Signature**: `(JLComplexF32Vector)->%`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L700)\]

elt(m, i) returns the i-th element of m. Column major order.

- **Signature**: `(%,NonNegativeInteger)->JLComplexF32`

### `hermitian?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L730)\]

hermitian?(m) tests hermiticity of m.

- **Signature**: `(%)->Boolean`

### `hermitianPart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L740)\]

hermitianPart(m) returns the symmetric part of m (m + m')/2.

- **Signature**: `(%)->%`

### `hermitianPart!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L742)\]

hermitianPart!(m) overwrites m with the symmetric part of m (m + m')/2 to save memory space and returns m.

- **Signature**: `(%)->%`

### `imag` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L721)\]

imag(m) returns a JLFloat32 matrix with the imaginary part of the m elements.

- **Signature**: `(%)->JLFloat32Matrix`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L724)\]

jlApply(func, x) applies func to argument x.

- **Signature**: `(String,%)->JLFloat32`
- **Signature**: `(String,%)->Void`

jlApply(func, x, val) applies func to arguments x and val.

- **Signature**: `(String,%,JLFloat32)->JLFloat32`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L735)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L714)\]

jlDisplay(m) pretty prints m (à la Julia).

- **Signature**: `(%)->Void`

### `jlVector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L716)\]

jlVector(m) returns reference to the internal vector representation.

- **Signature**: `(%)->JLComplexF32Vector`

### `juliaCMPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L766)\]

juliaCMPrint(b) defines whether or not FriCAS uses the printing version of Julia for matrices instead of its OutputForm version. Returns previous value. By default it is the Julia version.

- **Signature**: `(Boolean)->Boolean`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L753)\]

nrand(m,n) returns a Julia matrix of size (m,n) with random elements from the normal distribution with mean=0and standard deviation=1. See Julia documentation (randn) for this complex version used here.

- **Signature**: `(PositiveInteger,PositiveInteger)->%`

### `positiveDefinite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L732)\]

positiveDefinite?(m) tests whether or not m is Hermitian positive definite using a Cholesky factorisation.

- **Signature**: `(%)->Boolean`

### `qelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L703)\]

qelt(m, i) returns the i-th element of m. Column major order.

- **Signature**: `(%,NonNegativeInteger)->JLComplexF32`

### `qsetelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L709)\]

qsetelt!(m, i, r) sets the i-th element of m to r. Column major order.

- **Signature**: `(%,NonNegativeInteger,JLComplexF32)->JLComplexF32`

### `real` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L718)\]

real(m) returns a JLFloat32 matrix with the real part of the m elements.

- **Signature**: `(%)->JLFloat32Matrix`

### `setelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L706)\]

setelt!(m, i, r) sets the i-th element of m to r. Column major order.

- **Signature**: `(%,NonNegativeInteger,JLComplexF32)->JLComplexF32`

### `transpose!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L758)\]

transpose!(b, a) stores transposed a in b.

- **Signature**: `(%,%)->%`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L750)\]

urand01(m,n) returns a uniformly (0..1) distributed Julia matrix of size (m,n).

- **Signature**: `(PositiveInteger,PositiveInteger)->%`
---
[Back to Index](../index.md)
