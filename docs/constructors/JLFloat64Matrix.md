# JLFloat64Matrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L293)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast JLFloat64 matrix type with no bound checking on elt's. Minimum index is 1.

**JLFloat64Matrix is a domain constructor.**  
**Abbreviation for JLFloat64Matrix is JF64MAT**  
**This constructor is exposed in this frame.**  
**103 names for 157 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?*? : (%, %) -> %
 ?*? : (%, JLFloat64) -> %                              ?*? : (Integer, %) -> %
 ?*? : (JLFloat64, %) -> %                              ?*? : (%, JLFloat64Vector) -> JLFloat64Vector
 ?*? : (JLFloat64Vector, %) -> JLFloat64Vector          ?+? : (%, %) -> %
 -? : % -> %                                            ?-? : (%, %) -> %
 ?/? : (%, JLFloat64) -> %                              ?=? : (%, %) -> Boolean
 Pfaffian : % -> JLFloat64                              ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     antisymmetric? : % -> Boolean
 any? : ((JLFloat64 -> Boolean), %) -> Boolean          array2 : List(List(JLFloat64)) -> %
 blockConcat : List(List(%)) -> %                       coerce : JLFloat64Vector -> %
 coerce : % -> Matrix(DoubleFloat)                      coerce : % -> OutputForm
 colSlice : % -> Segment(Integer)                       column : (%, Integer) -> JLFloat64Vector
 columnSpace : % -> List(JLFloat64Vector)               convert : % -> String
 copy : % -> %                                          copy! : (%, %) -> %
 count : (JLFloat64, %) -> NonNegativeInteger           determinant : % -> JLFloat64
 diagonal? : % -> Boolean                               diagonalMatrix : JLFloat64Vector -> %
 diagonalMatrix : List(%) -> %                          diagonalMatrix : List(JLFloat64) -> %
 elt : (%, Integer, List(Integer)) -> %                 elt : (%, Integer, List(Segment(Integer))) -> %
 elt : (%, List(Integer), Integer) -> %                 elt : (%, List(Integer), List(Integer)) -> %
 elt : (%, List(Integer), Segment(Integer)) -> %        elt : (%, List(Segment(Integer)), Integer) -> %
 elt : (%, Segment(Integer), List(Integer)) -> %        elt : (%, Integer, Integer) -> JLFloat64
 elt : (%, NonNegativeInteger) -> JLFloat64             empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 every? : ((JLFloat64 -> Boolean), %) -> Boolean        exprand : (PositiveInteger, PositiveInteger) -> %
 ?exquo? : (%, JLFloat64) -> Union(%,"failed")          fill! : (%, JLFloat64) -> %
 horizConcat : (%, %) -> %                              horizConcat : List(%) -> %
 horizSplit : (%, PositiveInteger) -> List(%)           identity : PositiveInteger -> %
 inverse : % -> Union(%,"failed")                       jlApply : (String, %) -> %
 jlApply : (String, %) -> JLFloat64                     jlApply : (String, %, JLFloat64) -> JLFloat64
 jlApply : (String, %) -> Void                          jlApprox? : (%, %) -> Boolean
 jlDisplay : % -> Void                                  jlVector : % -> JLFloat64Vector
 juliaMPrint : Boolean -> Boolean                       kroneckerProduct : (%, %) -> %
 kroneckerProduct : List(%) -> %                        kroneckerSum : (%, %) -> %
 kroneckerSum : List(%) -> %                            latex : % -> String
 less? : (%, NonNegativeInteger) -> Boolean             listOfLists : % -> List(List(JLFloat64))
 map : ((JLFloat64 -> JLFloat64), %) -> %               map! : ((JLFloat64 -> JLFloat64), %) -> %
 matrix : List(List(JLFloat64)) -> %                    max : % -> JLFloat64
 maxColIndex : % -> Integer                             maxRowIndex : % -> Integer
 member? : (JLFloat64, %) -> Boolean                    members : % -> List(JLFloat64)
 min : % -> JLFloat64                                   minColIndex : % -> Integer
 minRowIndex : % -> Integer                             minordet : % -> JLFloat64
 more? : (%, NonNegativeInteger) -> Boolean             ncols : % -> NonNegativeInteger
 nrand : (PositiveInteger, PositiveInteger) -> %        nrows : % -> NonNegativeInteger
 nullSpace : % -> List(JLFloat64Vector)                 nullity : % -> NonNegativeInteger
 parts : % -> List(JLFloat64)                           positiveDefinite? : % -> Boolean
 positivePower : (%, Integer) -> %                      qelt : (%, Integer, Integer) -> JLFloat64
 qelt : (%, NonNegativeInteger) -> JLFloat64            rank : % -> NonNegativeInteger
 row : (%, Integer) -> JLFloat64Vector                  rowEchelon : % -> %
 rowSlice : % -> Segment(Integer)                       sample : () -> %
 setColumn! : (%, Integer, JLFloat64Vector) -> %        setRow! : (%, Integer, JLFloat64Vector) -> %
 setelt! : (%, Integer, List(Integer), %) -> %          setelt! : (%, List(Integer), Integer, %) -> %
 setsubMatrix! : (%, Integer, Integer, %) -> %          size? : (%, NonNegativeInteger) -> Boolean
 skewPart : % -> %                                      smaller? : (%, %) -> Boolean
 square? : % -> Boolean                                 squareTop : % -> %
 string : % -> String                                   swapColumns! : (%, Integer, Integer) -> %
 swapRows! : (%, Integer, Integer) -> %                 symmetric? : % -> Boolean
 symmetricPart : % -> %                                 symmetricPart! : % -> %
 transpose : % -> %                                     transpose : JLFloat64Vector -> %
 transpose! : (%, %) -> %                               urand01 : (PositiveInteger, PositiveInteger) -> %
 vertConcat : (%, %) -> %                               vertConcat : List(%) -> %
 vertSplit : (%, PositiveInteger) -> List(%)            zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 blockSplit : (%, List(NonNegativeInteger), List(NonNegativeInteger)) -> List(List(%))
 blockSplit : (%, PositiveInteger, PositiveInteger) -> List(List(%))
 count : ((JLFloat64 -> Boolean), %) -> NonNegativeInteger
 elt : (%, List(Segment(Integer)), List(Segment(Integer))) -> %
 elt : (%, List(Segment(Integer)), Segment(Integer)) -> %
 elt : (%, Segment(Integer), List(Segment(Integer))) -> %
 elt : (%, Segment(Integer), Segment(Integer)) -> %
 elt : (%, Integer, Integer, JLFloat64) -> JLFloat64
 horizSplit : (%, List(NonNegativeInteger)) -> List(%)
 kronecker_prod1 : (%, Integer, List(List(NonNegativeInteger)), List(%), NonNegativeInteger, NonNegativeInteger, Union(JLFloat64,one)) -> Void
 map : (((JLFloat64, JLFloat64) -> JLFloat64), %, %) -> %
 map : (((JLFloat64, JLFloat64) -> JLFloat64), %, %, JLFloat64) -> %
 matrix : (NonNegativeInteger, NonNegativeInteger, ((Integer, Integer) -> JLFloat64)) -> %
 max : (((JLFloat64, JLFloat64) -> Boolean), %) -> JLFloat64
 new : (NonNegativeInteger, NonNegativeInteger, JLFloat64) -> %
 qnew : (NonNegativeInteger, NonNegativeInteger) -> %
 qsetelt! : (%, Integer, Integer, JLFloat64) -> JLFloat64
 qsetelt! : (%, NonNegativeInteger, JLFloat64) -> JLFloat64
 scalarMatrix : (NonNegativeInteger, JLFloat64) -> %
 setelt! : (%, Integer, List(Segment(Integer)), %) -> %
 setelt! : (%, List(Integer), List(Integer), %) -> %
 setelt! : (%, List(Integer), Segment(Integer), %) -> %
 setelt! : (%, List(Segment(Integer)), Integer, %) -> %
 setelt! : (%, List(Segment(Integer)), List(Segment(Integer)), %) -> %
 setelt! : (%, List(Segment(Integer)), Segment(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Segment(Integer)), %) -> %
 setelt! : (%, Segment(Integer), Segment(Integer), %) -> %
 setelt! : (%, Integer, Integer, JLFloat64) -> JLFloat64
 setelt! : (%, NonNegativeInteger, JLFloat64) -> JLFloat64
 subMatrix : (%, Integer, Integer, Integer, Integer) -> %
 urand : (PositiveInteger, PositiveInteger, JLFloat64) -> %
 vertSplit : (%, List(NonNegativeInteger)) -> List(%)
 zero : (NonNegativeInteger, NonNegativeInteger) -> %
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L321)\]

coerce(m) coerces a copy of m to a Matrix(DoubleFloat).

- **Signature**: `(%)->Matrix(DoubleFloat)`

### `copy!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L340)\]

copy!(b,a) efficiently copies a to b. No checks are performed on array dimensions.

- **Signature**: `(%,%)->%`

### `diagonalMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L373)\]

diagonalMatrix(v) returns a diagonal matrix with elements of v.

- **Signature**: `(JLFloat64Vector)->%`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L309)\]

elt(m, i) returns the i-th element of m. Column major order.

- **Signature**: `(%,NonNegativeInteger)->JLFloat64`

### `exprand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L355)\]

exprand(m,n) returns a Julia matrix of size (m,n) with random elements from the exponential distribution withscale 1.

- **Signature**: `(PositiveInteger,PositiveInteger)->%`

### `identity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L343)\]

identity(n) returns a n by n identity matrix.

- **Signature**: `(PositiveInteger)->%`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L325)\]

jlApply(func, x) applies func to argument x.

- **Signature**: `(String,%)->%`
- **Signature**: `(String,%)->JLFloat64`
- **Signature**: `(String,%)->Void`

jlApply(func, x, val) applies func to arguments x and val.

- **Signature**: `(String,%,JLFloat64)->JLFloat64`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L361)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L333)\]

jlDisplay(m) pretty prints m (à la Julia).

- **Signature**: `(%)->Void`

### `jlVector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L323)\]

jlVector(m) returns reference to the internal vector representation.

- **Signature**: `(%)->JLFloat64Vector`

### `juliaMPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L335)\]

juliaMPrint(b) defines whether or not FriCAS uses the printing version of Julia for matrices instead of its OutputForm version. Returns previous value. By default it is the Julia version.

- **Signature**: `(Boolean)->Boolean`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L351)\]

nrand(m,n) returns a Julia matrix of size (m,n) with random elements from the normal distribution with mean=0and standard deviation=1.

- **Signature**: `(PositiveInteger,PositiveInteger)->%`

### `positiveDefinite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L358)\]

positiveDefinite?(m) tests whether or not m is positive definite. Use a Cholesky factorisation.

- **Signature**: `(%)->Boolean`

### `qelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L312)\]

qelt(m, i) returns the i-th element of m. Column major order.

- **Signature**: `(%,NonNegativeInteger)->JLFloat64`

### `qsetelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L318)\]

qsetelt!(m, i, r) sets the i-th element of m to r. Column major order.

- **Signature**: `(%,NonNegativeInteger,JLFloat64)->JLFloat64`

### `setelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L315)\]

setelt!(m, i, r) sets the i-th element of m to r. Column major order.

- **Signature**: `(%,NonNegativeInteger,JLFloat64)->JLFloat64`

### `skewPart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L371)\]

skewPart(m) returns the skew part of m (m - m')/2.

- **Signature**: `(%)->%`

### `symmetricPart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L366)\]

symmetricPart(m) returns the symmetric part of m (m + m')/2.

- **Signature**: `(%)->%`

### `symmetricPart!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L368)\]

symmetricPart!(m) overwrites m with the symmetric part of m (m + m')/2 to save memory space and returns m.

- **Signature**: `(%)->%`

### `transpose!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L375)\]

transpose!(b, a) stores transposed a in b.

- **Signature**: `(%,%)->%`

### `urand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L345)\]

urand(m,n,x) returns a uniform(0..x) Julia matrix of size (m,n). Use the underlying Common Lisp random numbergenerator so jlSeed! will not affect it.

- **Signature**: `(PositiveInteger,PositiveInteger,JLFloat64)->%`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L349)\]

urand01(m,n) returns a uniform(0..1) Julia matrix of size (m,n).

- **Signature**: `(PositiveInteger,PositiveInteger)->%`
---
[Back to Index](../index.md)
