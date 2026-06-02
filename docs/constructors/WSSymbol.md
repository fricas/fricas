# WSSymbol

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L115)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic symbols using the MathLink Julia package.

**WSSymbol is a domain constructor.**  
**Abbreviation for WSSymbol is WSSYM**  
**This constructor is exposed in this frame.**  
**47 names for 62 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                WComplex : () -> %
 WFailure : () -> %                                     WFalse : () -> %
 WFormat : () -> %                                      WInteger : () -> %
 WList : () -> %                                        WMissing : () -> %
 WNone : () -> %                                        WRational : () -> %
 WReal : () -> %                                        WRule : () -> %
 WString : () -> %                                      WSymbol : () -> %
 WTrue : () -> %                                        coerce : Boolean -> %
 coerce : String -> %                                   coerce : Symbol -> %
 coerce : % -> Boolean                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               coerce : % -> Symbol
 coerce : % -> WSExpression                             convert : % -> String
 jWSInterpret : String -> %                             jWSInterpret : (String, String) -> %
 jWSInterpret : (String, String, String) -> %           jWSSym : String -> %
 jWSSym : Symbol -> %                                   jlAbout : % -> Void
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

### `WComplex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L128)\]

WComplex() returns the WS Complex symbol.

- **Signature**: `()->%`

### `WFailure` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L138)\]

WFailure() returns the WS Failure symbol.

- **Signature**: `()->%`

### `WFalse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L136)\]

WFalse() returns the WS False symbol.

- **Signature**: `()->%`

### `WFormat` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L152)\]

WFormat() returns the WS Format symbol.

- **Signature**: `()->%`

### `WInteger` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L132)\]

WInteger() returns the WS Integer symbol.

- **Signature**: `()->%`

### `WList` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L142)\]

WList() returns the WS List symbol.

- **Signature**: `()->%`

### `WMissing` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L148)\]

WMissing() returns the WS Missing symbol.

- **Signature**: `()->%`

### `WNone` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L150)\]

WNone() returns the WS None symbol.

- **Signature**: `()->%`

### `WRational` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L130)\]

WRational() returns the WS Rational symbol.

- **Signature**: `()->%`

### `WReal` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L126)\]

WReal() returns the WS Real symbol.

- **Signature**: `()->%`

### `WRule` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L140)\]

WRule() returns the WS Rule symbol.

- **Signature**: `()->%`

### `WString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L144)\]

WString() returns the WS String symbol.

- **Signature**: `()->%`

### `WSymbol` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L146)\]

WSymbol() returns the WS Symbol symbol.

- **Signature**: `()->%`

### `WTrue` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L134)\]

WTrue() returns the WS True symbol.

- **Signature**: `()->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L154)\]

coerce(bool) coerces bool to a Julia WS boolean.

- **Signature**: `(Boolean)->%`

coerce(str) is a convenience function to coerce a String.

- **Signature**: `(String)->%`

coerce(sym) is a convenience function to coerce a Symbol.

- **Signature**: `(Symbol)->%`

coerce(sym) coerces sym to a FriCAS Boolean. Sym must be a Julia WS boolean

- **Signature**: `(%)->Boolean`

coerce(sym) is a convenience function to coerce sym to a FriCAS Symbol.

- **Signature**: `(%)->Symbol`

### `jWSSym` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L166)\]

jWSSym(str) constructs str as a WSSymbol.

- **Signature**: `(String)->%`

jWSSym(sym) constructs sym as a WSSymbol.

- **Signature**: `(Symbol)->%`
---
[Back to Index](../index.md)
