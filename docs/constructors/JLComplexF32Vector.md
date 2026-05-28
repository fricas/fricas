# JLComplexF32Vector

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L545)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast JLComplexF32 vector type with no bound checking on elt's. Minimum index is 1.

**JLComplexF32Vector is a domain constructor.**  
**Abbreviation for JLComplexF32Vector is JCF32VEC**  
**This constructor is not exposed in this frame.**  
**77 names for 98 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?*? : (%, JLComplexF32) -> %
 ?*? : (Integer, %) -> %                                ?*? : (JLComplexF32, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?=? : (%, %) -> Boolean
 any? : ((JLComplexF32 -> Boolean), %) -> Boolean       coerce : % -> OutputForm
 concat : (%, %) -> %                                   concat : (%, JLComplexF32) -> %
 concat : (JLComplexF32, %) -> %                        concat : List(%) -> %
 construct : List(JLComplexF32) -> %                    copy : % -> %
 copy! : (%, %) -> %                                    copyInto! : (%, %, Integer) -> %
 count : (JLComplexF32, %) -> NonNegativeInteger        cross : (%, %) -> %
 delete : (%, Integer) -> %                             delete : (%, UniversalSegment(Integer)) -> %
 dot : (%, %) -> JLComplexF32                           elt : (%, UniversalSegment(Integer)) -> %
 elt : (%, Integer) -> JLComplexF32                     elt : (%, Integer, JLComplexF32) -> JLComplexF32
 empty : () -> %                                        empty? : % -> Boolean
 entries : % -> List(JLComplexF32)                      entry? : (JLComplexF32, %) -> Boolean
 eq? : (%, %) -> Boolean                                every? : ((JLComplexF32 -> Boolean), %) -> Boolean
 fill! : (%, JLComplexF32) -> %                         first : (%, NonNegativeInteger) -> %
 first : % -> JLComplexF32                              imag : % -> JLFloat32Vector
 index? : (Integer, %) -> Boolean                       indices : % -> List(Integer)
 insert : (%, %, Integer) -> %                          insert : (JLComplexF32, %, Integer) -> %
 jlApply : (String, %) -> JLFloat32                     jlApply : (String, %, JLFloat32) -> JLFloat32
 jlApply : (String, %) -> Void                          jlApprox? : (%, %) -> Boolean
 jlDisplay : % -> Void                                  juliaCVPrint : Boolean -> Boolean
 latex : % -> String                                    leftTrim : (%, JLComplexF32) -> %
 length : % -> JLComplexF32                             less? : (%, NonNegativeInteger) -> Boolean
 map : ((JLComplexF32 -> JLComplexF32), %) -> %         map! : ((JLComplexF32 -> JLComplexF32), %) -> %
 maxIndex : % -> Integer                                member? : (JLComplexF32, %) -> Boolean
 members : % -> List(JLComplexF32)                      minIndex : % -> Integer
 more? : (%, NonNegativeInteger) -> Boolean             new : (NonNegativeInteger, JLComplexF32) -> %
 nrand : PositiveInteger -> %                           outerProduct : (%, %) -> Matrix(JLComplexF32)
 parts : % -> List(JLComplexF32)                        position : (JLComplexF32, %) -> Integer
 position : (JLComplexF32, %, Integer) -> Integer       qelt : (%, Integer) -> JLComplexF32
 qnew : NonNegativeInteger -> %                         real : % -> JLFloat32Vector
 remove : (JLComplexF32, %) -> %                        remove : ((JLComplexF32 -> Boolean), %) -> %
 removeDuplicates : % -> %                              reverse : % -> %
 reverse! : % -> %                                      rightTrim : (%, JLComplexF32) -> %
 sample : () -> %                                       select : ((JLComplexF32 -> Boolean), %) -> %
 size? : (%, NonNegativeInteger) -> Boolean             smaller? : (%, %) -> Boolean
 swap! : (%, Integer, Integer) -> Void                  trim : (%, JLComplexF32) -> %
 urand01 : PositiveInteger -> %                         vector : List(JLComplexF32) -> %
 zero : NonNegativeInteger -> %                         zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 count : ((JLComplexF32 -> Boolean), %) -> NonNegativeInteger
 find : ((JLComplexF32 -> Boolean), %) -> Union(JLComplexF32,"failed")
 map : (((JLComplexF32, JLComplexF32) -> JLComplexF32), %, %) -> %
 max : (((JLComplexF32, JLComplexF32) -> Boolean), %) -> JLComplexF32
 merge : (((JLComplexF32, JLComplexF32) -> Boolean), %, %) -> %
 position : ((JLComplexF32 -> Boolean), %) -> Integer
 qsetelt! : (%, Integer, JLComplexF32) -> JLComplexF32
 reduce : (((JLComplexF32, JLComplexF32) -> JLComplexF32), %) -> JLComplexF32
 reduce : (((JLComplexF32, JLComplexF32) -> JLComplexF32), %, JLComplexF32) -> JLComplexF32
 reduce : (((JLComplexF32, JLComplexF32) -> JLComplexF32), %, JLComplexF32, JLComplexF32) -> JLComplexF32
 setelt! : (%, Integer, JLComplexF32) -> JLComplexF32
 setelt! : (%, UniversalSegment(Integer), JLComplexF32) -> JLComplexF32
 sort : (((JLComplexF32, JLComplexF32) -> Boolean), %) -> %
 sort! : (((JLComplexF32, JLComplexF32) -> Boolean), %) -> %
 sorted? : (((JLComplexF32, JLComplexF32) -> Boolean), %) -> Boolean
```

## Operations added

### `copy!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L570)\]

copy!(b,a) efficiently copies a to b. No checks are performed on array dimensions.

- **Signature**: `(%,%)->%`

### `imag` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L567)\]

imag(v) returns a JLFloat32 vector with the imaginary part of the v elements.

- **Signature**: `(%)->JLFloat32Vector`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L573)\]

jlApply(func, x) applies func to argument x.

- **Signature**: `(String,%)->JLFloat32`
- **Signature**: `(String,%)->Void`

jlApply(func, x, val) applies func to arguments x and val.

- **Signature**: `(String,%,JLFloat32)->JLFloat32`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L579)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L591)\]

jlDisplay(v) pretty prints v (à la Julia).

- **Signature**: `(%)->Void`

### `juliaCVPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L593)\]

juliaCVPrint(b) defines whether or not FriCAS uses the printing version of Julia for matrices instead of its OutputForm version. Returns previous value. By default it is the Julia version.

- **Signature**: `(Boolean)->Boolean`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L587)\]

nrand(n) returns a normally distributed Julia vector of size n with mean=0 and standard deviation=1. See Julia documentation (randn) for this complex version used here.

- **Signature**: `(PositiveInteger)->%`

### `qnew` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L560)\]

qnew(n) creates a new uninitialized vector of length n.

- **Signature**: `(NonNegativeInteger)->%`

### `real` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L564)\]

real(v) returns a JLFloat32 vector with the real part of the v elements.

- **Signature**: `(%)->JLFloat32Vector`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L584)\]

urand01(n) returns a uniformly (0..1) distributed Julia vector of size n.

- **Signature**: `(PositiveInteger)->%`

### `vector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L562)\]

vector(l) converts the list l to a vector.

- **Signature**: `(List(JLComplexF32))->%`
---
[Back to Index](../index.md)
