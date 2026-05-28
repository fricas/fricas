# JLDataFrame

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1342)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Julia DataFrames support

**JLDataFrame is a domain constructor.**  
**Abbreviation for JLDataFrame is JDFRAME**  
**This constructor is exposed in this frame.**  
**46 names for 55 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger                           ?=? : (%, %) -> Boolean
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  copy : % -> %
 elt : (%, Integer) -> JLObject                         elt : (%, JLSymbol) -> JLObject
 empty : () -> %                                        empty? : % -> Boolean
 eq? : (%, %) -> Boolean                                first : (%, NonNegativeInteger) -> %
 head : (%, NonNegativeInteger) -> %                    jdframe : JLFloat64Matrix -> %
 jdframe : (JLFloat64Matrix, List(JLSymbol)) -> %       jdframe : JLMatrix(JLObjFloat64) -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %, %, %) -> JLObject
 jlDescribe : % -> %                                    jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    last : (%, NonNegativeInteger) -> %
 latex : % -> String                                    less? : (%, NonNegativeInteger) -> Boolean
 matrix : % -> JLObject                                 missing? : % -> Boolean
 more? : (%, NonNegativeInteger) -> Boolean             mutable? : % -> Boolean
 names : % -> List(String)                              ncols : % -> NonNegativeInteger
 nothing? : % -> Boolean                                nrows : % -> NonNegativeInteger
 qelt : (%, Integer) -> JLObject                        qelt : (%, JLSymbol) -> JLObject
 sample : () -> %                                       size? : (%, NonNegativeInteger) -> Boolean
 string : % -> String                                   tail : (%, NonNegativeInteger) -> %
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `first` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1384)\]

first(df, n) returns the first n rows.

- **Signature**: `(%,NonNegativeInteger)->%`

### `head` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1380)\]

head(df, n) returns the first n rows.

- **Signature**: `(%,NonNegativeInteger)->%`

### `jdframe` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1364)\]

jdframe(array) returns an automatically created data frame.

- **Signature**: `(JLFloat64Matrix)->%`
- **Signature**: `(JLMatrix(JLObjFloat64))->%`

jdframe(a, ls) returns a data frame from data a and column names ls. For example: exampledf:=jdframe(nrand(7,3),[a,b,jsym c]) exampledf.b

- **Signature**: `(JLFloat64Matrix,List(JLSymbol))->%`

### `jlDescribe` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1360)\]

jlDescribe(df) returns basic statistics about df.

- **Signature**: `(%)->%`

### `last` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1386)\]

last(df, n) returns the last n rows.

- **Signature**: `(%,NonNegativeInteger)->%`

### `matrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1362)\]

matrix(df) returns a copy of the internal DataFrame matrix.

- **Signature**: `(%)->JLObject`

### `names` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1378)\]

names(df) returns the column names as a list of strings.

- **Signature**: `(%)->List(String)`

### `ncols` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1376)\]

ncols(df) returns the number of columns.

- **Signature**: `(%)->NonNegativeInteger`

### `nrows` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1374)\]

nrows(df) returns the number of rows.

- **Signature**: `(%)->NonNegativeInteger`

### `tail` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L1382)\]

tail(df, n) returns the last n rows.

- **Signature**: `(%,NonNegativeInteger)->%`
---
[Back to Index](../index.md)
