# JLSymbol

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L108)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain allows the manipulation of Julia symbols.

**JLSymbol is a domain constructor.**  
**Abbreviation for JLSymbol is JSYM**  
**This constructor is exposed in this frame.**  
**7 names for 10 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : String -> %
 coerce : Symbol -> %                                   coerce : % -> OutputForm
 convert : % -> String                                  jsym : String -> %
 jsym : Symbol -> %                                     latex : % -> String
 string : % -> String                                   ?~=? : (%, %) -> Boolean
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L117)\]

coerce(str) coerces str.

- **Signature**: `(String)->%`

coerce(sym) coerces sym.

- **Signature**: `(Symbol)->%`

### `jsym` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L121)\]

jsym(str) converts str to a Julia symbol.

- **Signature**: `(String)->%`

jsym(sym) converts sym to a Julia symbol.

- **Signature**: `(Symbol)->%`
---
[Back to Index](../index.md)
