# JLComplexF64Vector

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L619)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast JLComplexF64 vector type with no bound checking on elt's. Minimum index is 1.

**JLComplexF64Vector is a domain constructor.**  
**Abbreviation for JLComplexF64Vector is JCF64VEC**  
**This constructor is not exposed in this frame.**  
**78 names for 99 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?*? : (%, JLComplexF64) -> %
 ?*? : (Integer, %) -> %                                ?*? : (JLComplexF64, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?=? : (%, %) -> Boolean
 any? : ((JLComplexF64 -> Boolean), %) -> Boolean       coerce : % -> OutputForm
 concat : (%, %) -> %                                   concat : (%, JLComplexF64) -> %
 concat : (JLComplexF64, %) -> %                        concat : List(%) -> %
 construct : List(JLComplexF64) -> %                    convert : % -> InputForm
 copy : % -> %                                          copy! : (%, %) -> %
 copyInto! : (%, %, Integer) -> %                       count : (JLComplexF64, %) -> NonNegativeInteger
 cross : (%, %) -> %                                    delete : (%, Integer) -> %
 delete : (%, UniversalSegment(Integer)) -> %           dot : (%, %) -> JLComplexF64
 elt : (%, UniversalSegment(Integer)) -> %              elt : (%, Integer) -> JLComplexF64
 elt : (%, Integer, JLComplexF64) -> JLComplexF64       empty : () -> %
 empty? : % -> Boolean                                  entries : % -> List(JLComplexF64)
 entry? : (JLComplexF64, %) -> Boolean                  eq? : (%, %) -> Boolean
 every? : ((JLComplexF64 -> Boolean), %) -> Boolean     fill! : (%, JLComplexF64) -> %
 first : (%, NonNegativeInteger) -> %                   first : % -> JLComplexF64
 imag : % -> JLFloat64Vector                            index? : (Integer, %) -> Boolean
 indices : % -> List(Integer)                           insert : (%, %, Integer) -> %
 insert : (JLComplexF64, %, Integer) -> %               jlApply : (String, %) -> JLFloat64
 jlApply : (String, %, JLFloat64) -> JLFloat64          jlApply : (String, %) -> Void
 jlApprox? : (%, %) -> Boolean                          jlDisplay : % -> Void
 juliaCVPrint : Boolean -> Boolean                      latex : % -> String
 leftTrim : (%, JLComplexF64) -> %                      length : % -> JLComplexF64
 less? : (%, NonNegativeInteger) -> Boolean             map : ((JLComplexF64 -> JLComplexF64), %) -> %
 map! : ((JLComplexF64 -> JLComplexF64), %) -> %        maxIndex : % -> Integer
 member? : (JLComplexF64, %) -> Boolean                 members : % -> List(JLComplexF64)
 minIndex : % -> Integer                                more? : (%, NonNegativeInteger) -> Boolean
 new : (NonNegativeInteger, JLComplexF64) -> %          nrand : PositiveInteger -> %
 outerProduct : (%, %) -> Matrix(JLComplexF64)          parts : % -> List(JLComplexF64)
 position : (JLComplexF64, %) -> Integer                position : (JLComplexF64, %, Integer) -> Integer
 qelt : (%, Integer) -> JLComplexF64                    qnew : NonNegativeInteger -> %
 real : % -> JLFloat64Vector                            remove : (JLComplexF64, %) -> %
 remove : ((JLComplexF64 -> Boolean), %) -> %           removeDuplicates : % -> %
 reverse : % -> %                                       reverse! : % -> %
 rightTrim : (%, JLComplexF64) -> %                     sample : () -> %
 select : ((JLComplexF64 -> Boolean), %) -> %           size? : (%, NonNegativeInteger) -> Boolean
 smaller? : (%, %) -> Boolean                           swap! : (%, Integer, Integer) -> Void
 trim : (%, JLComplexF64) -> %                          urand01 : PositiveInteger -> %
 vector : List(JLComplexF64) -> %                       zero : NonNegativeInteger -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 count : ((JLComplexF64 -> Boolean), %) -> NonNegativeInteger
 find : ((JLComplexF64 -> Boolean), %) -> Union(JLComplexF64,"failed")
 map : (((JLComplexF64, JLComplexF64) -> JLComplexF64), %, %) -> %
 max : (((JLComplexF64, JLComplexF64) -> Boolean), %) -> JLComplexF64
 merge : (((JLComplexF64, JLComplexF64) -> Boolean), %, %) -> %
 position : ((JLComplexF64 -> Boolean), %) -> Integer
 qsetelt! : (%, Integer, JLComplexF64) -> JLComplexF64
 reduce : (((JLComplexF64, JLComplexF64) -> JLComplexF64), %) -> JLComplexF64
 reduce : (((JLComplexF64, JLComplexF64) -> JLComplexF64), %, JLComplexF64) -> JLComplexF64
 reduce : (((JLComplexF64, JLComplexF64) -> JLComplexF64), %, JLComplexF64, JLComplexF64) -> JLComplexF64
 setelt! : (%, Integer, JLComplexF64) -> JLComplexF64
 setelt! : (%, UniversalSegment(Integer), JLComplexF64) -> JLComplexF64
 sort : (((JLComplexF64, JLComplexF64) -> Boolean), %) -> %
 sort! : (((JLComplexF64, JLComplexF64) -> Boolean), %) -> %
 sorted? : (((JLComplexF64, JLComplexF64) -> Boolean), %) -> Boolean
```

## Operations added

### `copy!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L641)\]

copy!(b,a) efficiently copies a to b. No checks are performed on array dimensions.

- **Signature**: `(%,%)->%`

### `imag` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L638)\]

imag(v) returns a JLFloat64 vector with the imaginary part of the v elements.

- **Signature**: `(%)->JLFloat64Vector`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L644)\]

jlApply(func, x) applies func to argument x.

- **Signature**: `(String,%)->JLFloat64`
- **Signature**: `(String,%)->Void`

jlApply(func, x, val) applies func to arguments x and val.

- **Signature**: `(String,%,JLFloat64)->JLFloat64`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L650)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L660)\]

jlDisplay(v) pretty prints v (à la Julia).

- **Signature**: `(%)->Void`

### `juliaCVPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L662)\]

juliaCVPrint(b) defines whether or not FriCAS uses the printing version of Julia for matrices instead of its OutputForm version. Returns previous value. By default it is the Julia version.

- **Signature**: `(Boolean)->Boolean`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L657)\]

nrand(n) returns a normally distributed Julia vector of size n. See Julia documentation (randn) for this complex version used here.

- **Signature**: `(PositiveInteger)->%`

### `qnew` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L631)\]

qnew(n) creates a new uninitialized vector of length n.

- **Signature**: `(NonNegativeInteger)->%`

### `real` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L635)\]

real(v) returns a JLFloat64 vector with the real part of the v elements.

- **Signature**: `(%)->JLFloat64Vector`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L655)\]

urand01(n) returns a uniform(0..1) Julia vector of size n.

- **Signature**: `(PositiveInteger)->%`

### `vector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L633)\]

vector(l) converts the list l to a vector.

- **Signature**: `(List(JLComplexF64))->%`
---
[Back to Index](../index.md)
