# JLFloat32Vector

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast JLFloat32 vector type with no bound checking on elt's. Minimum index is 1.

**JLFloat32Vector is a domain constructor.**  
**Abbreviation for JLFloat32Vector is JF32VEC**  
**This constructor is not exposed in this frame.**  
**82 names for 113 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?*? : (%, JLFloat32) -> %
 ?*? : (Integer, %) -> %                                ?*? : (JLFloat32, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?<? : (%, %) -> Boolean
 ?<=? : (%, %) -> Boolean                               ?=? : (%, %) -> Boolean
 ?>? : (%, %) -> Boolean                                ?>=? : (%, %) -> Boolean
 any? : ((JLFloat32 -> Boolean), %) -> Boolean          coerce : % -> OutputForm
 concat : (%, %) -> %                                   concat : (%, JLFloat32) -> %
 concat : (JLFloat32, %) -> %                           concat : List(%) -> %
 construct : List(JLFloat32) -> %                       copy : % -> %
 copy! : (%, %) -> %                                    copyInto! : (%, %, Integer) -> %
 count : (JLFloat32, %) -> NonNegativeInteger           cross : (%, %) -> %
 delete : (%, Integer) -> %                             delete : (%, UniversalSegment(Integer)) -> %
 dot : (%, %) -> JLFloat32                              elt : (%, UniversalSegment(Integer)) -> %
 elt : (%, Integer) -> JLFloat32                        elt : (%, Integer, JLFloat32) -> JLFloat32
 empty : () -> %                                        empty? : % -> Boolean
 entries : % -> List(JLFloat32)                         entry? : (JLFloat32, %) -> Boolean
 eq? : (%, %) -> Boolean                                every? : ((JLFloat32 -> Boolean), %) -> Boolean
 exprand : PositiveInteger -> %                         fill! : (%, JLFloat32) -> %
 first : (%, NonNegativeInteger) -> %                   first : % -> JLFloat32
 index? : (Integer, %) -> Boolean                       indices : % -> List(Integer)
 insert : (%, %, Integer) -> %                          insert : (JLFloat32, %, Integer) -> %
 jlApply : (String, %) -> %                             jlApply : (String, %) -> JLFloat32
 jlApply : (String, %, JLFloat32) -> JLFloat32          jlApply : (String, %) -> Void
 jlApply : (String, %, %) -> Void                       jlApply : (String, %, %, %) -> Void
 jlApprox? : (%, %) -> Boolean                          jlDisplay : % -> Void
 juliaVPrint : Boolean -> Boolean                       latex : % -> String
 leftTrim : (%, JLFloat32) -> %                         length : % -> JLFloat32
 less? : (%, NonNegativeInteger) -> Boolean             map : ((JLFloat32 -> JLFloat32), %) -> %
 map! : ((JLFloat32 -> JLFloat32), %) -> %              max : (%, %) -> %
 max : % -> JLFloat32                                   maxIndex : % -> Integer
 member? : (JLFloat32, %) -> Boolean                    members : % -> List(JLFloat32)
 merge : (%, %) -> %                                    min : (%, %) -> %
 min : % -> JLFloat32                                   minIndex : % -> Integer
 more? : (%, NonNegativeInteger) -> Boolean             new : (NonNegativeInteger, JLFloat32) -> %
 nrand : PositiveInteger -> %                           outerProduct : (%, %) -> Matrix(JLFloat32)
 parts : % -> List(JLFloat32)                           position : (JLFloat32, %) -> Integer
 position : (JLFloat32, %, Integer) -> Integer          position : ((JLFloat32 -> Boolean), %) -> Integer
 qelt : (%, Integer) -> JLFloat32                       qnew : NonNegativeInteger -> %
 qsetelt! : (%, Integer, JLFloat32) -> JLFloat32        remove : (JLFloat32, %) -> %
 remove : ((JLFloat32 -> Boolean), %) -> %              removeDuplicates : % -> %
 reverse : % -> %                                       reverse! : % -> %
 rightTrim : (%, JLFloat32) -> %                        sample : () -> %
 select : ((JLFloat32 -> Boolean), %) -> %              setelt! : (%, Integer, JLFloat32) -> JLFloat32
 size? : (%, NonNegativeInteger) -> Boolean             smaller? : (%, %) -> Boolean
 sort : % -> %                                          sort! : % -> %
 sorted? : % -> Boolean                                 swap! : (%, Integer, Integer) -> Void
 trim : (%, JLFloat32) -> %                             urand : (PositiveInteger, JLFloat32) -> %
 urand01 : PositiveInteger -> %                         vector : List(JLFloat32) -> %
 zero : NonNegativeInteger -> %                         zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 count : ((JLFloat32 -> Boolean), %) -> NonNegativeInteger
 find : ((JLFloat32 -> Boolean), %) -> Union(JLFloat32,"failed")
 map : (((JLFloat32, JLFloat32) -> JLFloat32), %, %) -> %
 max : (((JLFloat32, JLFloat32) -> Boolean), %) -> JLFloat32
 merge : (((JLFloat32, JLFloat32) -> Boolean), %, %) -> %
 reduce : (((JLFloat32, JLFloat32) -> JLFloat32), %) -> JLFloat32
 reduce : (((JLFloat32, JLFloat32) -> JLFloat32), %, JLFloat32) -> JLFloat32
 reduce : (((JLFloat32, JLFloat32) -> JLFloat32), %, JLFloat32, JLFloat32) -> JLFloat32
 setelt! : (%, UniversalSegment(Integer), JLFloat32) -> JLFloat32
 sort : (((JLFloat32, JLFloat32) -> Boolean), %) -> %
 sort! : (((JLFloat32, JLFloat32) -> Boolean), %) -> %
 sorted? : (((JLFloat32, JLFloat32) -> Boolean), %) -> Boolean
```

## Operations added

### `copy!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L20)\]

copy!(b,a) efficiently copies a to b. No checks are performed on array dimensions.

- **Signature**: `(%,%)->%`

### `exprand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L55)\]

exprand(n) returns a vector of size n with random elements from the exponential distribution with scale 1.

- **Signature**: `(PositiveInteger)->%`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L23)\]

jlApply(func, v) applies func to argument v. and returns a JLFloat32Vector.

- **Signature**: `(String,%)->%`

jlApply(func, v) applies func to argument v. and returns a Julia Float32.

- **Signature**: `(String,%)->JLFloat32`

jlApply(func, v, val) applies func to arguments v and val and returns a Julia Float32.

- **Signature**: `(String,%,JLFloat32)->JLFloat32`

jlApply(func, v) applies func to argument v.

- **Signature**: `(String,%)->Void`

jlApply(func, v1, v2) applies func to arguments v1 and v2.

- **Signature**: `(String,%,%)->Void`

jlApply(func, v1, v2, v3) applies func to arguments v1, v2 and v3.

- **Signature**: `(String,%,%,%)->Void`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L39)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L58)\]

jlDisplay(v) pretty prints v (à la Julia).

- **Signature**: `(%)->Void`

### `juliaVPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L60)\]

juliaVPrint(b) defines whether or not FriCAS uses the printing version of Julia for vectors instead of its OutputForm version. Returns previous value. By default it is the Julia version.

- **Signature**: `(Boolean)->Boolean`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L52)\]

nrand(n) returns a normally distributed Julia vector of size n with mean=0 and standard deviation=1.

- **Signature**: `(PositiveInteger)->%`

### `qnew` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L18)\]

qnew(n) returns an uninitialized vector of dimension n.

- **Signature**: `(NonNegativeInteger)->%`

### `urand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L44)\]

urand(n, x) returns a uniformly (0..x) distributed Julia vector of size n. Use the underlying Common Lisp random number generator so jlSeed! will not affect it.

- **Signature**: `(PositiveInteger,JLFloat32)->%`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L49)\]

urand01(n) returns a uniformly (0..1) distributed Julia vector of size n.

- **Signature**: `(PositiveInteger)->%`

### `vector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L16)\]

vector(l) converts the list l to a vector.

- **Signature**: `(List(JLFloat32))->%`
---
[Back to Index](../index.md)
