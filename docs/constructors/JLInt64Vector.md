# JLInt64Vector

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L12)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast JLInt64 vector type with no bound checking on elt's. Minimum index is 1. Convenience domain.

**JLInt64Vector is a domain constructor.**  
**Abbreviation for JLInt64Vector is JI64VEC**  
**This constructor is exposed in this frame.**  
**75 names for 101 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?*? : (%, JLInt64) -> %
 ?*? : (Integer, %) -> %                                ?*? : (JLInt64, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?<? : (%, %) -> Boolean
 ?<=? : (%, %) -> Boolean                               ?=? : (%, %) -> Boolean
 ?>? : (%, %) -> Boolean                                ?>=? : (%, %) -> Boolean
 any? : ((JLInt64 -> Boolean), %) -> Boolean            coerce : % -> OutputForm
 concat : (%, %) -> %                                   concat : (%, JLInt64) -> %
 concat : (JLInt64, %) -> %                             concat : List(%) -> %
 construct : List(JLInt64) -> %                         convert : % -> InputForm
 copy : % -> %                                          copyInto! : (%, %, Integer) -> %
 count : (JLInt64, %) -> NonNegativeInteger             cross : (%, %) -> %
 delete : (%, Integer) -> %                             delete : (%, UniversalSegment(Integer)) -> %
 dot : (%, %) -> JLInt64                                elt : (%, UniversalSegment(Integer)) -> %
 elt : (%, Integer) -> JLInt64                          elt : (%, Integer, JLInt64) -> JLInt64
 empty : () -> %                                        empty? : % -> Boolean
 entries : % -> List(JLInt64)                           entry? : (JLInt64, %) -> Boolean
 eq? : (%, %) -> Boolean                                every? : ((JLInt64 -> Boolean), %) -> Boolean
 fill! : (%, JLInt64) -> %                              first : (%, NonNegativeInteger) -> %
 first : % -> JLInt64                                   index? : (Integer, %) -> Boolean
 indices : % -> List(Integer)                           insert : (%, %, Integer) -> %
 insert : (JLInt64, %, Integer) -> %                    jlDisplay : % -> Void
 juliaIVPrint : Boolean -> Boolean                      latex : % -> String
 leftTrim : (%, JLInt64) -> %                           less? : (%, NonNegativeInteger) -> Boolean
 map : ((JLInt64 -> JLInt64), %) -> %                   map! : ((JLInt64 -> JLInt64), %) -> %
 max : (%, %) -> %                                      max : % -> JLInt64
 maxIndex : % -> Integer                                member? : (JLInt64, %) -> Boolean
 members : % -> List(JLInt64)                           merge : (%, %) -> %
 min : (%, %) -> %                                      min : % -> JLInt64
 minIndex : % -> Integer                                more? : (%, NonNegativeInteger) -> Boolean
 new : (NonNegativeInteger, JLInt64) -> %               outerProduct : (%, %) -> Matrix(JLInt64)
 parts : % -> List(JLInt64)                             position : (JLInt64, %) -> Integer
 position : (JLInt64, %, Integer) -> Integer            position : ((JLInt64 -> Boolean), %) -> Integer
 qelt : (%, Integer) -> JLInt64                         qnew : NonNegativeInteger -> %
 qsetelt! : (%, Integer, JLInt64) -> JLInt64            remove : (JLInt64, %) -> %
 remove : ((JLInt64 -> Boolean), %) -> %                removeDuplicates : % -> %
 reverse : % -> %                                       reverse! : % -> %
 rightTrim : (%, JLInt64) -> %                          sample : () -> %
 select : ((JLInt64 -> Boolean), %) -> %                setelt! : (%, Integer, JLInt64) -> JLInt64
 size? : (%, NonNegativeInteger) -> Boolean             smaller? : (%, %) -> Boolean
 sort : % -> %                                          sort : (((JLInt64, JLInt64) -> Boolean), %) -> %
 sort! : % -> %                                         sort! : (((JLInt64, JLInt64) -> Boolean), %) -> %
 sorted? : % -> Boolean                                 swap! : (%, Integer, Integer) -> Void
 trim : (%, JLInt64) -> %                               vector : List(JLInt64) -> %
 zero : NonNegativeInteger -> %                         zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 count : ((JLInt64 -> Boolean), %) -> NonNegativeInteger
 find : ((JLInt64 -> Boolean), %) -> Union(JLInt64,"failed")
 map : (((JLInt64, JLInt64) -> JLInt64), %, %) -> %
 max : (((JLInt64, JLInt64) -> Boolean), %) -> JLInt64
 merge : (((JLInt64, JLInt64) -> Boolean), %, %) -> %
 reduce : (((JLInt64, JLInt64) -> JLInt64), %) -> JLInt64
 reduce : (((JLInt64, JLInt64) -> JLInt64), %, JLInt64) -> JLInt64
 reduce : (((JLInt64, JLInt64) -> JLInt64), %, JLInt64, JLInt64) -> JLInt64
 setelt! : (%, UniversalSegment(Integer), JLInt64) -> JLInt64
 sorted? : (((JLInt64, JLInt64) -> Boolean), %) -> Boolean
```

## Operations added

### `jlDisplay` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L29)\]

jlDisplay(v) pretty prints v (à la Julia).

- **Signature**: `(%)->Void`

### `juliaIVPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L31)\]

juliaIVPrint(b) defines whether or not FriCAS uses the printing version of Julia for vectors instead of its OutputForm version. Returns previous value. By default it is the Julia version.

- **Signature**: `(Boolean)->Boolean`

### `qnew` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L25)\]

qnew(n) returns an uninitialized vector of dimension n.

- **Signature**: `(NonNegativeInteger)->%`

### `vector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L27)\]

vector(l) converts the list l to a vector.

- **Signature**: `(List(JLInt64))->%`
---
[Back to Index](../index.md)
