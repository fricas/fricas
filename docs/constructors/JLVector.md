# JLVector

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1442)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a JLVector type for JLObjectType. Minimum index is 1.

**JLVector(R: JLObjectType) is a domain constructor**  
**Abbreviation for JLVector is JVECTOR**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?*? : (Integer, %) -> % if R has ABELGRP
 ?*? : (R, %) -> % if R has SGROUP                      ?*? : (%, R) -> % if R has SGROUP
 ?+? : (%, %) -> % if R has ABELSG                      -? : % -> % if R has ABELGRP
 ?-? : (%, %) -> % if R has ABELGRP                     ?=? : (%, %) -> Boolean
 coerce : JLFloat64Vector -> JLVector(JLObjFloat64)     coerce : % -> JLObject
 coerce : % -> OutputForm                               concat : (%, R) -> %
 concat : (R, %) -> %                                   concat : (%, %) -> %
 concat : List(%) -> %                                  construct : List(R) -> %
 convert : % -> String                                  copy : % -> %
 cross : (%, %) -> % if R has RING                      delete : (%, Integer) -> %
 delete : (%, UniversalSegment(Integer)) -> %           elt : (%, PositiveInteger) -> R
 elt : (%, Integer, R) -> R                             elt : (%, Integer) -> R
 elt : (%, UniversalSegment(Integer)) -> %              elt : (%, Integer) -> JLObject
 elt : (%, JLSymbol) -> JLObject                        empty : () -> %
 empty? : % -> Boolean                                  entries : % -> List(R)
 eq? : (%, %) -> Boolean                                fill! : (%, R) -> % if % has ATSHMUT
 find : ((R -> Boolean), %) -> Union(R,"failed")        first : % -> R if Integer has ORDSET
 first : (%, NonNegativeInteger) -> %                   index? : (Integer, %) -> Boolean
 indices : % -> List(Integer)                           insert : (R, %, Integer) -> %
 insert : (%, %, Integer) -> %                          jlAbout : % -> Void
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %) -> JLObject
 jlApply : (String, %) -> JLObject                      jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jvector : String -> %
 latex : % -> String                                    less? : (%, NonNegativeInteger) -> Boolean
 map : ((R -> R), %) -> %                               map : (((R, R) -> R), %, %) -> %
 map! : ((R -> R), %) -> % if % has ATSHMUT             maxIndex : % -> Integer if Integer has ORDSET
 members : % -> List(R) if % has ATFINAG                minIndex : % -> Integer if Integer has ORDSET
 missing? : % -> Boolean                                more? : (%, NonNegativeInteger) -> Boolean
 mutable? : % -> Boolean                                new : (NonNegativeInteger, R) -> %
 nothing? : % -> Boolean                                parts : % -> List(R) if % has ATFINAG
 qelt : (%, PositiveInteger) -> R                       qelt : (%, Integer) -> R
 qelt : (%, Integer) -> JLObject                        qelt : (%, JLSymbol) -> JLObject
 reverse : % -> % if % has ATFINAG                      sample : () -> %
 size? : (%, NonNegativeInteger) -> Boolean             string : % -> String
 vector : List(R) -> %                                  zero? : % -> Boolean if R has ABELMON
 ?~=? : (%, %) -> Boolean
 ?<? : (%, %) -> Boolean if % has ATFINAG and R has ORDSET
 ?<=? : (%, %) -> Boolean if % has ATFINAG and R has ORDSET
 ?>? : (%, %) -> Boolean if % has ATFINAG and R has ORDSET
 ?>=? : (%, %) -> Boolean if % has ATFINAG and R has ORDSET
 any? : ((R -> Boolean), %) -> Boolean if % has ATFINAG
 coerce : JLComplexF64Vector -> JLVector(JLObjComplexF64)
 convert : % -> InputForm if R has KONVERT(INFORM)
 copyInto! : (%, %, Integer) -> % if % has ATFINAG and % has ATSHMUT
 count : ((R -> Boolean), %) -> NonNegativeInteger if % has ATFINAG
 count : (R, %) -> NonNegativeInteger if % has ATFINAG and R has BASTYPE
 dot : (%, %) -> R if R has ABELMON and R has SRNG
 entry? : (R, %) -> Boolean if % has ATFINAG and R has BASTYPE
 eval : (%, List(Equation(R))) -> % if R has EVALAB(R) and R has SETCAT
 eval : (%, Equation(R)) -> % if R has EVALAB(R) and R has SETCAT
 eval : (%, R, R) -> % if R has EVALAB(R) and R has SETCAT
 eval : (%, List(R), List(R)) -> % if R has EVALAB(R) and R has SETCAT
 every? : ((R -> Boolean), %) -> Boolean if % has ATFINAG
 exprand : PositiveInteger -> JLVector(JLFloat) if R has FPS and R has JOBRING and R has ATARBPR and not R has NTYPE
 hash : % -> SingleInteger if % has ATFINAG and R has HASHABL
 hashUpdate! : (HashState, %) -> HashState if % has ATFINAG and R has HASHABL
 kroneckerProduct : (%, %) -> % if not R has NTYPE
 leftTrim : (%, R) -> % if % has ATFINAG and R has BASTYPE
 length : % -> R if R has RADCAT and R has RING
 max : (((R, R) -> Boolean), %) -> R if % has ATFINAG
 max : % -> R if % has ATFINAG and R has ORDSET
 max : (%, %) -> % if % has ATFINAG and R has ORDSET
 member? : (R, %) -> Boolean if % has ATFINAG and R has BASTYPE
 merge : (((R, R) -> Boolean), %, %) -> % if % has ATFINAG
 merge : (%, %) -> % if % has ATFINAG and R has ORDSET
 min : % -> R if % has ATFINAG and R has ORDSET
 min : (%, %) -> % if % has ATFINAG and R has ORDSET
 nrand : PositiveInteger -> JLVector(JLComplexFloat) if R has COMPCAT(JFLOAT) and R has JOBRING and not R has NTYPE
 nrand : PositiveInteger -> JLVector(JLFloat) if R has FPS and R has JOBRING and R has ATARBPR and not R has NTYPE
 outerProduct : (%, %) -> Matrix(R) if R has RING
 position : ((R -> Boolean), %) -> Integer if % has ATFINAG
 position : (R, %) -> Integer if % has ATFINAG and R has BASTYPE
 position : (R, %, Integer) -> Integer if % has ATFINAG and R has BASTYPE
 qsetelt! : (%, Integer, R) -> R if % has ATSHMUT
 reduce : (((R, R) -> R), %, R, R) -> R if % has ATFINAG and R has BASTYPE
 reduce : (((R, R) -> R), %, R) -> R if % has ATFINAG
 reduce : (((R, R) -> R), %) -> R if % has ATFINAG
 remove : (R, %) -> % if % has ATFINAG and R has BASTYPE
 remove : ((R -> Boolean), %) -> % if % has ATFINAG
 removeDuplicates : % -> % if % has ATFINAG and R has BASTYPE
 reverse! : % -> % if % has ATFINAG and % has ATSHMUT
 rightTrim : (%, R) -> % if % has ATFINAG and R has BASTYPE
 select : ((R -> Boolean), %) -> % if % has ATFINAG
 setelt! : (%, Integer, R) -> R if % has ATSHMUT
 setelt! : (%, UniversalSegment(Integer), R) -> R if % has ATSHMUT
 smaller? : (%, %) -> Boolean if % has ATFINAG and R has COMPAR or % has ATFINAG and R has ORDSET
 sort : (((R, R) -> Boolean), %) -> % if % has ATFINAG
 sort : % -> % if % has ATFINAG and R has ORDSET
 sort! : (((R, R) -> Boolean), %) -> % if % has ATFINAG and % has ATSHMUT
 sort! : % -> % if % has ATFINAG and % has ATSHMUT and R has ORDSET
 sorted? : (((R, R) -> Boolean), %) -> Boolean if % has ATFINAG
 sorted? : % -> Boolean if % has ATFINAG and R has ORDSET
 swap! : (%, Integer, Integer) -> Void if % has ATSHMUT
 trim : (%, R) -> % if % has ATFINAG and R has BASTYPE
 urand01 : PositiveInteger -> JLVector(JLComplexFloat) if R has COMPCAT(JFLOAT) and R has JOBRING and not R has NTYPE
 urand01 : PositiveInteger -> JLVector(JLFloat) if R has FPS and R has JOBRING and R has ATARBPR and not R has NTYPE
 zero : NonNegativeInteger -> % if R has ABELMON
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1480)\]

coerce(x): convenience function.

- **Signature**: `(JLComplexF64Vector)->JLVector(JLObjComplexF64)`
- **Signature**: `(JLFloat64Vector)->JLVector(JLObjFloat64)`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1452)\]

elt(v,n) returns the element in v from the index n.

- **Signature**: `(%,PositiveInteger)->R`

### `exprand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1469)\]

exprand(n) returns a JLVector of size n with exponentially distributed random numbers.

- **Signature**: `(PositiveInteger)->JLVector(JLFloat)`

### `jvector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1484)\]

jvector(str) evaluates the string str and returns the generated vector. No checks are done at the FriCAS level.

- **Signature**: `(String)->%`

### `kroneckerProduct` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1459)\]

kroneckerProduct(v1,v2) returns the kronecker product of v1 and v2.

- **Signature**: `(%,%)->%`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1466)\]

nrand(n) returns a JLVector of size n with normally distributed random numbers.

- **Signature**: `(PositiveInteger)->JLVector(JLComplexFloat)`
- **Signature**: `(PositiveInteger)->JLVector(JLFloat)`

### `qelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1454)\]

qelt(v,n) returns the element in v from the index n. No checks are done at the FriCAS level, only at the Julia level

- **Signature**: `(%,PositiveInteger)->R`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1463)\]

urand01(n) returns a JLVector of size n with uniformly distributed random numbers contained in the unit disk.

- **Signature**: `(PositiveInteger)->JLVector(JLComplexFloat)`

urand01(n) returns a JLVector of size n with uniformly distributed random numbers contained in [0,1].

- **Signature**: `(PositiveInteger)->JLVector(JLFloat)`

### `vector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1450)\]

vector(l) converts the list l to a vector.

- **Signature**: `(List(R))->%`
---
[Back to Index](../index.md)
