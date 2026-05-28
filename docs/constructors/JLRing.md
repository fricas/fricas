# JLRing

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L22)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Parent category of Julia ring domains.

**JLRing is a category constructor**  
**Abbreviation for JLRing is JRING**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?+? : (%, %) -> %                                      ?-? : (%, %) -> %
 -? : % -> %                                            0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 coerce : Integer -> %                                  coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> String
 latex : % -> String                                    leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 recip : % -> Union(%,"failed")                         rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
```

## Operations added
---
[Back to Index](../index.md)
