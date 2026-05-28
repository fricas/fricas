# JLType

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Parent category of Julia domains.

**JLType is a category constructor**  
**Abbreviation for JLType is JTYPE**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : % -> OutputForm
 convert : % -> String                                  latex : % -> String
 string : % -> String                                   ?~=? : (%, %) -> Boolean
```

## Operations added

### `string` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/julia.spad#L9)\]

string(jt) returns the string representation of jt.

- **Signature**: `(%)->String`
---
[Back to Index](../index.md)
