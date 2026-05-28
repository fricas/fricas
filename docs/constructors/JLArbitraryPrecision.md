# JLArbitraryPrecision

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L249)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Approximate numbers for which the user can get the precision for subsequent calculations.

**JLArbitraryPrecision is a category constructor**  
**Abbreviation for JLArbitraryPrecision is JARBPR**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : % -> OutputForm
 convert : % -> String                                  latex : % -> String
 precision : () -> PositiveInteger                      string : % -> String
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L255)\]

precision() returns the precision defined at instantiation time or later if its modification is allowed.

- **Signature**: `()->PositiveInteger`
---
[Back to Index](../index.md)
