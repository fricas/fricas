# WSList

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L207)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic lists using the MathLink Julia package.

**WSList(E: WSObject) is a domain constructor**  
**Abbreviation for WSList is WSLIST**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> WSInteger                                    ?=? : (%, %) -> Boolean
 accumulate : % -> % if E has WSNUM                     append : (%, E) -> %
 coerce : % -> List(E)                                  coerce : List(E) -> %
 coerce : % -> WSExpression                             coerce : % -> JLObject
 coerce : % -> OutputForm                               convert : % -> String
 delete : (%, WSList(WSInteger)) -> %                   differences : % -> % if E has WSNUM
 dimensions : % -> WSList(WSInteger)                    elt : (%, Integer) -> E
 extract : (%, WSExpression) -> %                       first : % -> E
 insert : (%, E, WSInteger) -> %                        intersection : (%, %) -> %
 jWSAggregate : List(E) -> %                            jWSInterpret : (String, String, String) -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : String -> %
 jWSList : String -> %                                  jWSList : List(E) -> %
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
 jlref : String -> %                                    join : (%, %) -> %
 last : % -> E                                          latex : % -> String
 length : % -> WSInteger                                missing? : % -> Boolean
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 numeric : (%, PositiveInteger) -> WSExpression         numeric : % -> WSExpression
 numeric? : % -> Boolean                                part : (%, WSInteger) -> E
 prepend : (%, E) -> %                                  qelt : (%, Integer) -> E
 qsetelt : (%, Integer, E) -> %                         qsetelt! : (%, Integer, E) -> %
 removeDuplicates : % -> %                              replacePart : (%, %) -> %
 rest : % -> %                                          reverse : (%, WSList(WSInteger)) -> %
 reverse : (%, WSInteger) -> %                          reverse : % -> %
 riffle : (%, %, %) -> %                                riffle : (%, %) -> %
 setIntersection : (%, %) -> %                          setelt : (%, Integer, E) -> %
 setelt! : (%, Integer, E) -> %                         sort : % -> %
 sorted? : % -> Boolean                                 string : % -> String
 take : (%, WSList(WSInteger)) -> %                     take : (%, Integer) -> %
 toString : % -> String                                 total : % -> E if E has WSNUM
 union : (%, %) -> %                                    ?~=? : (%, %) -> Boolean
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L217)\]

coerce(l) coerces l to a FriCAS List.

- **Signature**: `(%)->List(E)`

### `jWSList` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L219)\]

jWSList(list) constructs list as a WSList.

- **Signature**: `(List(E))->%`

jWSList(str) constructs str as a WSList. str must be in the WS language.

- **Signature**: `(String)->%`
---
[Back to Index](../index.md)
