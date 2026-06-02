# NMIntegerMod

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L559)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This domain allows the manipulation of Nemo integer mod n elements using the Nemo Julia package (FLINT based).

**NMIntegerMod(p: PositiveInteger) is a domain constructor**  
**Abbreviation for NMIntegerMod is NZMOD**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, NMInteger) -> %                              ?*? : (%, Integer) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 0 : () -> %                                            1 : () -> %
 ?=? : (%, %) -> Boolean                                ?^? : (%, PositiveInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              coerce : NMInteger -> %
 coerce : % -> %                                        coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> Integer
 convert : % -> InputForm                               convert : % -> String
 enumerate : () -> List(%)                              equal? : (%, %) -> Boolean
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 hash : % -> SingleInteger                              hashUpdate! : (HashState, %) -> HashState
 index : PositiveInteger -> %                           init : () -> %
 jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlId : % -> JLInt64                                    jlNMRing : () -> String
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jnzmod : NMInteger -> %
 jnzmod : Integer -> %                                  latex : % -> String
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     lookup : % -> PositiveInteger
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nextItem : % -> Union(%,"failed")                      nothing? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               random : () -> %
 recip : % -> Union(%,"failed")                         rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       size : () -> NonNegativeInteger
 smaller? : (%, %) -> Boolean                           string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            unit? : % -> Boolean
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L578)\]

coerce(i) coerces i.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `jnzmod` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L582)\]

jnzmod(i) returns i as a NMIntegerMod.

- **Signature**: `(Integer)->%`

jnzmod(i) returns i as a NMIntegerMod

- **Signature**: `(NMInteger)->%`
---
[Back to Index](../index.md)
