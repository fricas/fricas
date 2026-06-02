# WSRing

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L95)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic category for rings using the Wolfram Symbol Transport Protocol.

**WSRing is a category constructor**  
**Abbreviation for WSRing is WSRING**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 0 : () -> %                                            1 : () -> %
 ?=? : (%, %) -> Boolean                                ?^? : (%, PositiveInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              coerce : Integer -> %
 coerce : % -> WSExpression                             coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> String                                  jWSInterpret : (String, String, String) -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : String -> %
 jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlEval : % -> %                                        jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlHead : % -> WSSymbol
 jlId : % -> JLInt64                                    jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlSymbolic : % -> String                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    latex : % -> String
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     missing? : % -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 numeric : (%, PositiveInteger) -> WSExpression         numeric : % -> WSExpression
 numeric? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          recip : % -> Union(%,"failed")
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 toString : % -> String                                 zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
```

## Operations added
---
[Back to Index](../index.md)
