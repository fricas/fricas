# WSString

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L215)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic strings using the MathLink Julia package.

**WSString is a domain constructor.**  
**Abbreviation for WSString is WSSTR**  
**This constructor is exposed in this frame.**  
**35 names for 46 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                characters : % -> WSList(%)
 coerce : String -> %                                   coerce : % -> JLObject
 coerce : % -> OutputForm                               coerce : % -> String
 coerce : % -> WSExpression                             convert : % -> String
 elt : (%, Integer) -> %                                jWSInterpret : String -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : (String, String, String) -> %
 jWSString : String -> %                                jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlEval : % -> %
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlHead : % -> WSSymbol                                 jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlSymbolic : % -> String
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 latex : % -> String                                    missing? : % -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 numeric : % -> WSExpression                            numeric : (%, PositiveInteger) -> WSExpression
 numeric? : % -> Boolean                                string : % -> String
 toString : % -> String                                 ?~=? : (%, %) -> Boolean
```

## Operations added

### `characters` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L226)\]

characters(str) returns the characters of str as a WSList.

- **Signature**: `(%)->WSList(%)`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L230)\]

coerce(str) coerces str to a Julia WS string.

- **Signature**: `(String)->%`

coerce(str) coerces str to a FriCAS String. str must be a Julia WS string

- **Signature**: `(%)->String`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L228)\]

elt(str,i) returns the i-th character of str.

- **Signature**: `(%,Integer)->%`

### `jWSString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L235)\]

jWSString(str) constructs str as a WSString.

- **Signature**: `(String)->%`
---
[Back to Index](../index.md)
