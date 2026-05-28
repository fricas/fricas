# JLVectorCategory

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L8)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This category provides Julia vectors.

**JLVectorCategory(JT: JLType) is a category constructor**  
**Abbreviation for JLVectorCategory is JVECCAT**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?*? : (%, JT) -> % if JT has SGROUP
 ?*? : (JT, %) -> % if JT has SGROUP                    ?*? : (Integer, %) -> % if JT has ABELGRP
 ?+? : (%, %) -> % if JT has ABELSG                     ?-? : (%, %) -> % if JT has ABELGRP
 -? : % -> % if JT has ABELGRP                          concat : List(%) -> %
 concat : (%, %) -> %                                   concat : (JT, %) -> %
 concat : (%, JT) -> %                                  construct : List(JT) -> %
 copy : % -> %                                          cross : (%, %) -> % if JT has RING
 delete : (%, UniversalSegment(Integer)) -> %           delete : (%, Integer) -> %
 elt : (%, UniversalSegment(Integer)) -> %              elt : (%, Integer) -> JT
 elt : (%, Integer, JT) -> JT                           empty : () -> %
 empty? : % -> Boolean                                  entries : % -> List(JT)
 eq? : (%, %) -> Boolean                                fill! : (%, JT) -> % if % has ATSHMUT
 find : ((JT -> Boolean), %) -> Union(JT,"failed")      first : (%, NonNegativeInteger) -> %
 first : % -> JT if Integer has ORDSET                  index? : (Integer, %) -> Boolean
 indices : % -> List(Integer)                           insert : (%, %, Integer) -> %
 insert : (JT, %, Integer) -> %                         less? : (%, NonNegativeInteger) -> Boolean
 map : (((JT, JT) -> JT), %, %) -> %                    map : ((JT -> JT), %) -> %
 map! : ((JT -> JT), %) -> % if % has ATSHMUT           maxIndex : % -> Integer if Integer has ORDSET
 members : % -> List(JT) if % has ATFINAG               minIndex : % -> Integer if Integer has ORDSET
 more? : (%, NonNegativeInteger) -> Boolean             new : (NonNegativeInteger, JT) -> %
 parts : % -> List(JT) if % has ATFINAG                 qelt : (%, Integer) -> JT
 reverse : % -> % if % has ATFINAG                      sample : () -> %
 size? : (%, NonNegativeInteger) -> Boolean             zero? : % -> Boolean if JT has ABELMON
 ?<? : (%, %) -> Boolean if JT has ORDSET and % has ATFINAG
 ?<=? : (%, %) -> Boolean if JT has ORDSET and % has ATFINAG
 ?=? : (%, %) -> Boolean if JT has HASHABL and % has ATFINAG or JT has COMPAR and % has ATFINAG or JT has ORDSET and % has ATFINAG or JT has SETCAT or JT has BASTYPE and % has ATFINAG
 ?>? : (%, %) -> Boolean if JT has ORDSET and % has ATFINAG
 ?>=? : (%, %) -> Boolean if JT has ORDSET and % has ATFINAG
 any? : ((JT -> Boolean), %) -> Boolean if % has ATFINAG
 coerce : % -> OutputForm if JT has COMPAR and % has ATFINAG or JT has ORDSET and % has ATFINAG or JT has KOERCE(OUTFORM)
 convert : % -> InputForm if JT has KONVERT(INFORM)
 copyInto! : (%, %, Integer) -> % if % has ATSHMUT and % has ATFINAG
 count : (JT, %) -> NonNegativeInteger if JT has BASTYPE and % has ATFINAG
 count : ((JT -> Boolean), %) -> NonNegativeInteger if % has ATFINAG
 dot : (%, %) -> JT if JT has SRNG and JT has ABELMON
 entry? : (JT, %) -> Boolean if % has ATFINAG and JT has BASTYPE
 eval : (%, List(JT), List(JT)) -> % if JT has EVALAB(JT) and JT has SETCAT
 eval : (%, JT, JT) -> % if JT has EVALAB(JT) and JT has SETCAT
 eval : (%, Equation(JT)) -> % if JT has EVALAB(JT) and JT has SETCAT
 eval : (%, List(Equation(JT))) -> % if JT has EVALAB(JT) and JT has SETCAT
 every? : ((JT -> Boolean), %) -> Boolean if % has ATFINAG
 hash : % -> SingleInteger if JT has HASHABL and % has ATFINAG
 hashUpdate! : (HashState, %) -> HashState if JT has HASHABL and % has ATFINAG
 latex : % -> String if JT has COMPAR and % has ATFINAG or JT has ORDSET and % has ATFINAG or JT has SETCAT
 leftTrim : (%, JT) -> % if JT has BASTYPE and % has ATFINAG
 length : % -> JT if JT has RING and JT has RADCAT
 max : (%, %) -> % if JT has ORDSET and % has ATFINAG
 max : % -> JT if JT has ORDSET and % has ATFINAG
 max : (((JT, JT) -> Boolean), %) -> JT if % has ATFINAG
 member? : (JT, %) -> Boolean if JT has BASTYPE and % has ATFINAG
 merge : (%, %) -> % if JT has ORDSET and % has ATFINAG
 merge : (((JT, JT) -> Boolean), %, %) -> % if % has ATFINAG
 min : (%, %) -> % if JT has ORDSET and % has ATFINAG
 min : % -> JT if JT has ORDSET and % has ATFINAG
 outerProduct : (%, %) -> Matrix(JT) if JT has RING
 position : (JT, %, Integer) -> Integer if JT has BASTYPE and % has ATFINAG
 position : (JT, %) -> Integer if JT has BASTYPE and % has ATFINAG
 position : ((JT -> Boolean), %) -> Integer if % has ATFINAG
 qsetelt! : (%, Integer, JT) -> JT if % has ATSHMUT
 reduce : (((JT, JT) -> JT), %) -> JT if % has ATFINAG
 reduce : (((JT, JT) -> JT), %, JT) -> JT if % has ATFINAG
 reduce : (((JT, JT) -> JT), %, JT, JT) -> JT if JT has BASTYPE and % has ATFINAG
 remove : ((JT -> Boolean), %) -> % if % has ATFINAG
 remove : (JT, %) -> % if JT has BASTYPE and % has ATFINAG
 removeDuplicates : % -> % if JT has BASTYPE and % has ATFINAG
 reverse! : % -> % if % has ATSHMUT and % has ATFINAG
 rightTrim : (%, JT) -> % if JT has BASTYPE and % has ATFINAG
 select : ((JT -> Boolean), %) -> % if % has ATFINAG
 setelt! : (%, UniversalSegment(Integer), JT) -> JT if % has ATSHMUT
 setelt! : (%, Integer, JT) -> JT if % has ATSHMUT
 smaller? : (%, %) -> Boolean if JT has COMPAR and % has ATFINAG or JT has ORDSET and % has ATFINAG
 sort : % -> % if JT has ORDSET and % has ATFINAG
 sort : (((JT, JT) -> Boolean), %) -> % if % has ATFINAG
 sort! : % -> % if JT has ORDSET and % has ATSHMUT and % has ATFINAG
 sort! : (((JT, JT) -> Boolean), %) -> % if % has ATSHMUT and % has ATFINAG
 sorted? : % -> Boolean if JT has ORDSET and % has ATFINAG
 sorted? : (((JT, JT) -> Boolean), %) -> Boolean if % has ATFINAG
 swap! : (%, Integer, Integer) -> Void if % has ATSHMUT
 trim : (%, JT) -> % if JT has BASTYPE and % has ATFINAG
 zero : NonNegativeInteger -> % if JT has ABELMON
 ?~=? : (%, %) -> Boolean if JT has HASHABL and % has ATFINAG or JT has COMPAR and % has ATFINAG or JT has ORDSET and % has ATFINAG or JT has SETCAT or JT has BASTYPE and % has ATFINAG
```

## Operations added
---
[Back to Index](../index.md)
