# JLFloat32Matrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L223)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast JLFloat32 matrix type with no bound checking on elt's. Minimum index is 1.

**JLFloat32Matrix is a domain constructor.**  
**Abbreviation for JLFloat32Matrix is JF32MAT**  
**This constructor is not exposed in this frame.**  
**103 names for 156 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?*? : (%, %) -> %
 ?*? : (%, JLFloat32) -> %                              ?*? : (Integer, %) -> %
 ?*? : (JLFloat32, %) -> %                              ?*? : (%, JLFloat32Vector) -> JLFloat32Vector
 ?*? : (JLFloat32Vector, %) -> JLFloat32Vector          ?+? : (%, %) -> %
 -? : % -> %                                            ?-? : (%, %) -> %
 ?/? : (%, JLFloat32) -> %                              ?=? : (%, %) -> Boolean
 Pfaffian : % -> JLFloat32                              ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     antisymmetric? : % -> Boolean
 any? : ((JLFloat32 -> Boolean), %) -> Boolean          array2 : List(List(JLFloat32)) -> %
 blockConcat : List(List(%)) -> %                       coerce : JLFloat32Vector -> %
 coerce : % -> OutputForm                               colSlice : % -> Segment(Integer)
 column : (%, Integer) -> JLFloat32Vector               columnSpace : % -> List(JLFloat32Vector)
 convert : % -> String                                  copy : % -> %
 copy! : (%, %) -> %                                    count : (JLFloat32, %) -> NonNegativeInteger
 determinant : % -> JLFloat32                           diagonal? : % -> Boolean
 diagonalMatrix : JLFloat32Vector -> %                  diagonalMatrix : List(%) -> %
 diagonalMatrix : List(JLFloat32) -> %                  elt : (%, Integer, List(Integer)) -> %
 elt : (%, Integer, List(Segment(Integer))) -> %        elt : (%, List(Integer), Integer) -> %
 elt : (%, List(Integer), List(Integer)) -> %           elt : (%, List(Integer), Segment(Integer)) -> %
 elt : (%, List(Segment(Integer)), Integer) -> %        elt : (%, Segment(Integer), List(Integer)) -> %
 elt : (%, Integer, Integer) -> JLFloat32               elt : (%, NonNegativeInteger) -> JLFloat32
 empty : () -> %                                        empty? : % -> Boolean
 eq? : (%, %) -> Boolean                                every? : ((JLFloat32 -> Boolean), %) -> Boolean
 exprand : (PositiveInteger, PositiveInteger) -> %      ?exquo? : (%, JLFloat32) -> Union(%,"failed")
 fill! : (%, JLFloat32) -> %                            horizConcat : (%, %) -> %
 horizConcat : List(%) -> %                             horizSplit : (%, PositiveInteger) -> List(%)
 identity : PositiveInteger -> %                        inverse : % -> Union(%,"failed")
 jlApply : (String, %) -> %                             jlApply : (String, %) -> JLFloat32
 jlApply : (String, %, JLFloat32) -> JLFloat32          jlApply : (String, %) -> Void
 jlApprox? : (%, %) -> Boolean                          jlDisplay : % -> Void
 jlVector : % -> JLFloat32Vector                        juliaMPrint : Boolean -> Boolean
 kroneckerProduct : (%, %) -> %                         kroneckerProduct : List(%) -> %
 kroneckerSum : (%, %) -> %                             kroneckerSum : List(%) -> %
 latex : % -> String                                    less? : (%, NonNegativeInteger) -> Boolean
 listOfLists : % -> List(List(JLFloat32))               map : ((JLFloat32 -> JLFloat32), %) -> %
 map! : ((JLFloat32 -> JLFloat32), %) -> %              matrix : List(List(JLFloat32)) -> %
 max : % -> JLFloat32                                   maxColIndex : % -> Integer
 maxRowIndex : % -> Integer                             member? : (JLFloat32, %) -> Boolean
 members : % -> List(JLFloat32)                         min : % -> JLFloat32
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 minordet : % -> JLFloat32                              more? : (%, NonNegativeInteger) -> Boolean
 ncols : % -> NonNegativeInteger                        nrand : (PositiveInteger, PositiveInteger) -> %
 nrows : % -> NonNegativeInteger                        nullSpace : % -> List(JLFloat32Vector)
 nullity : % -> NonNegativeInteger                      parts : % -> List(JLFloat32)
 positiveDefinite? : % -> Boolean                       positivePower : (%, Integer) -> %
 qelt : (%, Integer, Integer) -> JLFloat32              qelt : (%, NonNegativeInteger) -> JLFloat32
 rank : % -> NonNegativeInteger                         row : (%, Integer) -> JLFloat32Vector
 rowEchelon : % -> %                                    rowSlice : % -> Segment(Integer)
 sample : () -> %                                       setColumn! : (%, Integer, JLFloat32Vector) -> %
 setRow! : (%, Integer, JLFloat32Vector) -> %           setelt! : (%, Integer, List(Integer), %) -> %
 setelt! : (%, List(Integer), Integer, %) -> %          setsubMatrix! : (%, Integer, Integer, %) -> %
 size? : (%, NonNegativeInteger) -> Boolean             skewPart : % -> %
 smaller? : (%, %) -> Boolean                           square? : % -> Boolean
 squareTop : % -> %                                     string : % -> String
 swapColumns! : (%, Integer, Integer) -> %              swapRows! : (%, Integer, Integer) -> %
 symmetric? : % -> Boolean                              symmetricPart : % -> %
 symmetricPart! : % -> %                                transpose : % -> %
 transpose : JLFloat32Vector -> %                       transpose! : (%, %) -> %
 urand01 : (PositiveInteger, PositiveInteger) -> %      vertConcat : (%, %) -> %
 vertConcat : List(%) -> %                              vertSplit : (%, PositiveInteger) -> List(%)
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 blockSplit : (%, List(NonNegativeInteger), List(NonNegativeInteger)) -> List(List(%))
 blockSplit : (%, PositiveInteger, PositiveInteger) -> List(List(%))
 count : ((JLFloat32 -> Boolean), %) -> NonNegativeInteger
 elt : (%, List(Segment(Integer)), List(Segment(Integer))) -> %
 elt : (%, List(Segment(Integer)), Segment(Integer)) -> %
 elt : (%, Segment(Integer), List(Segment(Integer))) -> %
 elt : (%, Segment(Integer), Segment(Integer)) -> %
 elt : (%, Integer, Integer, JLFloat32) -> JLFloat32
 horizSplit : (%, List(NonNegativeInteger)) -> List(%)
 kronecker_prod1 : (%, Integer, List(List(NonNegativeInteger)), List(%), NonNegativeInteger, NonNegativeInteger, Union(JLFloat32,one)) -> Void
 map : (((JLFloat32, JLFloat32) -> JLFloat32), %, %) -> %
 map : (((JLFloat32, JLFloat32) -> JLFloat32), %, %, JLFloat32) -> %
 matrix : (NonNegativeInteger, NonNegativeInteger, ((Integer, Integer) -> JLFloat32)) -> %
 max : (((JLFloat32, JLFloat32) -> Boolean), %) -> JLFloat32
 new : (NonNegativeInteger, NonNegativeInteger, JLFloat32) -> %
 qnew : (NonNegativeInteger, NonNegativeInteger) -> %
 qsetelt! : (%, Integer, Integer, JLFloat32) -> JLFloat32
 qsetelt! : (%, NonNegativeInteger, JLFloat32) -> JLFloat32
 scalarMatrix : (NonNegativeInteger, JLFloat32) -> %
 setelt! : (%, Integer, List(Segment(Integer)), %) -> %
 setelt! : (%, List(Integer), List(Integer), %) -> %
 setelt! : (%, List(Integer), Segment(Integer), %) -> %
 setelt! : (%, List(Segment(Integer)), Integer, %) -> %
 setelt! : (%, List(Segment(Integer)), List(Segment(Integer)), %) -> %
 setelt! : (%, List(Segment(Integer)), Segment(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Segment(Integer)), %) -> %
 setelt! : (%, Segment(Integer), Segment(Integer), %) -> %
 setelt! : (%, Integer, Integer, JLFloat32) -> JLFloat32
 setelt! : (%, NonNegativeInteger, JLFloat32) -> JLFloat32
 subMatrix : (%, Integer, Integer, Integer, Integer) -> %
 urand : (PositiveInteger, PositiveInteger, JLFloat32) -> %
 vertSplit : (%, List(NonNegativeInteger)) -> List(%)
 zero : (NonNegativeInteger, NonNegativeInteger) -> %
```

## Operations added

### `copy!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L271)\]

copy!(b,a) efficiently copies a to b. No checks are performed on array dimensions.

- **Signature**: `(%,%)->%`

### `diagonalMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L306)\]

diagonalMatrix(v) returns a diagonal matrix with elements of v.

- **Signature**: `(JLFloat32Vector)->%`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L242)\]

elt(m, i) returns the i-th element of m. Column major order.

- **Signature**: `(%,NonNegativeInteger)->JLFloat32`

### `exprand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L288)\]

exprand(m,n) returns a Julia matrix of size (m,n) with random elements from the exponential distribution withscale 1.

- **Signature**: `(PositiveInteger,PositiveInteger)->%`

### `identity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L274)\]

identity(n) returns a n by n identity matrix.

- **Signature**: `(PositiveInteger)->%`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L256)\]

jlApply(func, x) applies func to argument x.

- **Signature**: `(String,%)->%`
- **Signature**: `(String,%)->JLFloat32`
- **Signature**: `(String,%)->Void`

jlApply(func, x, val) applies func to arguments x and val.

- **Signature**: `(String,%,JLFloat32)->JLFloat32`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L294)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L264)\]

jlDisplay(m) pretty prints m (à la Julia).

- **Signature**: `(%)->Void`

### `jlVector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L254)\]

jlVector(m) returns reference to the internal vector representation.

- **Signature**: `(%)->JLFloat32Vector`

### `juliaMPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L266)\]

juliaMPrint(b) defines whether or not FriCAS uses the printing version of Julia for matrices instead of its OutputForm version. Returns previous value. By default it is the Julia version.

- **Signature**: `(Boolean)->Boolean`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L284)\]

nrand(m,n) returns a Julia matrix of size (m,n) with random elements from the normal distribution with mean=0and standard deviation=1.

- **Signature**: `(PositiveInteger,PositiveInteger)->%`

### `positiveDefinite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L291)\]

positiveDefinite?(m) tests whether or not m is positive definite. Use a Cholesky factorisation.

- **Signature**: `(%)->Boolean`

### `qelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L245)\]

qelt(m, i) returns the i-th element of m. Column major order.

- **Signature**: `(%,NonNegativeInteger)->JLFloat32`

### `qsetelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L251)\]

qsetelt!(m, i, r) sets the i-th element of m to r. Column major order.

- **Signature**: `(%,NonNegativeInteger,JLFloat32)->JLFloat32`

### `setelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L248)\]

setelt!(m, i, r) sets the i-th element of m to r. Column major order.

- **Signature**: `(%,NonNegativeInteger,JLFloat32)->JLFloat32`

### `skewPart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L304)\]

skewPart(m) returns the skew part of m (m - m')/2.

- **Signature**: `(%)->%`

### `symmetricPart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L299)\]

symmetricPart(m) returns the symmetric part of m (m + m')/2.

- **Signature**: `(%)->%`

### `symmetricPart!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L301)\]

symmetricPart!(m) overwrites m with the symmetric part of m (m + m')/2 to save memory space and returns m.

- **Signature**: `(%)->%`

### `transpose!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L308)\]

transpose!(b, a) stores transposed a in b.

- **Signature**: `(%,%)->%`

### `urand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L276)\]

urand(m,n,x) returns a uniformly (0..x) distributed Julia matrix of size (m,n). Use the underlying Common Lisp random number generator so jlSeed! will not affect it.

- **Signature**: `(PositiveInteger,PositiveInteger,JLFloat32)->%`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L281)\]

urand01(m,n) returns a uniformly (0..1) distributed Julia matrix of size (m,n).

- **Signature**: `(PositiveInteger,PositiveInteger)->%`
---
[Back to Index](../index.md)
