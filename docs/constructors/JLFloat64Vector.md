# JLFloat64Vector

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L71)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast JLFloat64 vector type with no bound checking on elt's. Minimum index is 1.

**JLFloat64Vector is a domain constructor.**  
**Abbreviation for JLFloat64Vector is JF64VEC**  
**This constructor is exposed in this frame.**  
**84 names for 116 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?*? : (%, JLFloat64) -> %
 ?*? : (Integer, %) -> %                                ?*? : (JLFloat64, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?<? : (%, %) -> Boolean
 ?<=? : (%, %) -> Boolean                               ?=? : (%, %) -> Boolean
 ?>? : (%, %) -> Boolean                                ?>=? : (%, %) -> Boolean
 any? : ((JLFloat64 -> Boolean), %) -> Boolean          coerce : % -> DoubleFloatVector
 coerce : % -> OutputForm                               coerce! : % -> DoubleFloatVector
 concat : (%, %) -> %                                   concat : (%, JLFloat64) -> %
 concat : (JLFloat64, %) -> %                           concat : List(%) -> %
 construct : List(JLFloat64) -> %                       convert : % -> InputForm
 copy : % -> %                                          copy! : (%, %) -> %
 copyInto! : (%, %, Integer) -> %                       count : (JLFloat64, %) -> NonNegativeInteger
 cross : (%, %) -> %                                    delete : (%, Integer) -> %
 delete : (%, UniversalSegment(Integer)) -> %           dot : (%, %) -> JLFloat64
 elt : (%, UniversalSegment(Integer)) -> %              elt : (%, Integer) -> JLFloat64
 elt : (%, Integer, JLFloat64) -> JLFloat64             empty : () -> %
 empty? : % -> Boolean                                  entries : % -> List(JLFloat64)
 entry? : (JLFloat64, %) -> Boolean                     eq? : (%, %) -> Boolean
 every? : ((JLFloat64 -> Boolean), %) -> Boolean        exprand : PositiveInteger -> %
 fill! : (%, JLFloat64) -> %                            first : (%, NonNegativeInteger) -> %
 first : % -> JLFloat64                                 index? : (Integer, %) -> Boolean
 indices : % -> List(Integer)                           insert : (%, %, Integer) -> %
 insert : (JLFloat64, %, Integer) -> %                  jlApply : (String, %) -> %
 jlApply : (String, %) -> JLFloat64                     jlApply : (String, %, JLFloat64) -> JLFloat64
 jlApply : (String, %) -> Void                          jlApply : (String, %, %) -> Void
 jlApply : (String, %, %, %) -> Void                    jlApprox? : (%, %) -> Boolean
 jlDisplay : % -> Void                                  juliaVPrint : Boolean -> Boolean
 latex : % -> String                                    leftTrim : (%, JLFloat64) -> %
 length : % -> JLFloat64                                less? : (%, NonNegativeInteger) -> Boolean
 map : ((JLFloat64 -> JLFloat64), %) -> %               map! : ((JLFloat64 -> JLFloat64), %) -> %
 max : (%, %) -> %                                      max : % -> JLFloat64
 maxIndex : % -> Integer                                member? : (JLFloat64, %) -> Boolean
 members : % -> List(JLFloat64)                         merge : (%, %) -> %
 min : (%, %) -> %                                      min : % -> JLFloat64
 minIndex : % -> Integer                                more? : (%, NonNegativeInteger) -> Boolean
 new : (NonNegativeInteger, JLFloat64) -> %             nrand : PositiveInteger -> %
 outerProduct : (%, %) -> Matrix(JLFloat64)             parts : % -> List(JLFloat64)
 position : (JLFloat64, %) -> Integer                   position : (JLFloat64, %, Integer) -> Integer
 position : ((JLFloat64 -> Boolean), %) -> Integer      qelt : (%, Integer) -> JLFloat64
 qnew : NonNegativeInteger -> %                         qsetelt! : (%, Integer, JLFloat64) -> JLFloat64
 remove : (JLFloat64, %) -> %                           remove : ((JLFloat64 -> Boolean), %) -> %
 removeDuplicates : % -> %                              reverse : % -> %
 reverse! : % -> %                                      rightTrim : (%, JLFloat64) -> %
 sample : () -> %                                       select : ((JLFloat64 -> Boolean), %) -> %
 setelt! : (%, Integer, JLFloat64) -> JLFloat64         size? : (%, NonNegativeInteger) -> Boolean
 smaller? : (%, %) -> Boolean                           sort : % -> %
 sort! : % -> %                                         sorted? : % -> Boolean
 swap! : (%, Integer, Integer) -> Void                  trim : (%, JLFloat64) -> %
 urand : (PositiveInteger, JLFloat64) -> %              urand01 : PositiveInteger -> %
 vector : List(JLFloat64) -> %                          zero : NonNegativeInteger -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 count : ((JLFloat64 -> Boolean), %) -> NonNegativeInteger
 find : ((JLFloat64 -> Boolean), %) -> Union(JLFloat64,"failed")
 map : (((JLFloat64, JLFloat64) -> JLFloat64), %, %) -> %
 max : (((JLFloat64, JLFloat64) -> Boolean), %) -> JLFloat64
 merge : (((JLFloat64, JLFloat64) -> Boolean), %, %) -> %
 reduce : (((JLFloat64, JLFloat64) -> JLFloat64), %) -> JLFloat64
 reduce : (((JLFloat64, JLFloat64) -> JLFloat64), %, JLFloat64) -> JLFloat64
 reduce : (((JLFloat64, JLFloat64) -> JLFloat64), %, JLFloat64, JLFloat64) -> JLFloat64
 setelt! : (%, UniversalSegment(Integer), JLFloat64) -> JLFloat64
 sort : (((JLFloat64, JLFloat64) -> Boolean), %) -> %
 sort! : (((JLFloat64, JLFloat64) -> Boolean), %) -> %
 sorted? : (((JLFloat64, JLFloat64) -> Boolean), %) -> Boolean
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L83)\]

coerce(v) coerces a copy of v to a DoubleFloatVector.

- **Signature**: `(%)->DoubleFloatVector`

### `coerce!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L85)\]

coerce!(v) coerces v to a DoubleFloatVector.

- **Signature**: `(%)->DoubleFloatVector`

### `copy!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L91)\]

copy!(b,a) efficiently copies a to b. No checks are performed on array dimensions.

- **Signature**: `(%,%)->%`

### `exprand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L124)\]

exprand(n) returns a Julia vector of size n with random elements from the exponential distribution with scale1.

- **Signature**: `(PositiveInteger)->%`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L94)\]

jlApply(func, v) applies func to argument v and returns a JLFloat64Vector.

- **Signature**: `(String,%)->%`

jlApply(func, v) applies func to argument v and returns a Julia Float64.

- **Signature**: `(String,%)->JLFloat64`

jlApply(func, v, val) applies func to arguments v and val and returns a Julia Float64.

- **Signature**: `(String,%,JLFloat64)->JLFloat64`

jlApply(func, v) applies func to argument v.

- **Signature**: `(String,%)->Void`

jlApply(func, v1, v2) applies func to arguments v1 and v2.

- **Signature**: `(String,%,%)->Void`

jlApply(func, v1, v2, v3) applies func to arguments v1, v2 and v3.

- **Signature**: `(String,%,%,%)->Void`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L110)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds. Applied component-wise.

- **Signature**: `(%,%)->Boolean`

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L127)\]

jlDisplay(v) pretty prints v (à la Julia).

- **Signature**: `(%)->Void`

### `juliaVPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L129)\]

juliaVPrint(b) defines whether or not FriCAS uses the printing version of Julia for vectors instead of its OutputForm version. Returns previous value. By default it is the Julia version.

- **Signature**: `(Boolean)->Boolean`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L121)\]

nrand(n) returns a Julia vector of size n with random elements from the normal distribution with mean=0 and standard deviation=1.

- **Signature**: `(PositiveInteger)->%`

### `qnew` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L89)\]

qnew(n) returns an uninitialized vector of dimension n.

- **Signature**: `(NonNegativeInteger)->%`

### `urand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L115)\]

urand(n, x) returns a uniform(0..x) Julia vector of size n. Use the underlying Common Lisp random number generator so jlSeed! will not affect it.

- **Signature**: `(PositiveInteger,JLFloat64)->%`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L119)\]

urand01(n) returns a uniform(0..1) Julia vector of size n.

- **Signature**: `(PositiveInteger)->%`

### `vector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L87)\]

vector(l) converts the list l to a vector.

- **Signature**: `(List(JLFloat64))->%`
---
[Back to Index](../index.md)
