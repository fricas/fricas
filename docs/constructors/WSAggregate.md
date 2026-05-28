# WSAggregate

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L1)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic aggregate (WS list) using the MathLink Julia package.

**WSAggregate(E: WSObject) is a category constructor**  
**Abbreviation for WSAggregate is WSAGG**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> WSInteger                                    ?=? : (%, %) -> Boolean
 accumulate : % -> % if E has WSNUM                     append : (%, E) -> %
 coerce : List(E) -> %                                  coerce : % -> WSExpression
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  delete : (%, WSList(WSInteger)) -> %
 differences : % -> % if E has WSNUM                    dimensions : % -> WSList(WSInteger)
 elt : (%, Integer) -> E                                extract : (%, WSExpression) -> %
 first : % -> E                                         insert : (%, E, WSInteger) -> %
 intersection : (%, %) -> %                             jWSAggregate : List(E) -> %
 jWSInterpret : (String, String, String) -> %           jWSInterpret : (String, String) -> %
 jWSInterpret : String -> %                             jlAbout : % -> Void
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %) -> JLObject
 jlApply : (String, %) -> JLObject                      jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlEval : % -> %
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlHead : % -> WSSymbol                                 jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlSymbolic : % -> String
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 join : (%, %) -> %                                     last : % -> E
 latex : % -> String                                    length : % -> WSInteger
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nothing? : % -> Boolean                                numeric : (%, PositiveInteger) -> WSExpression
 numeric : % -> WSExpression                            numeric? : % -> Boolean
 part : (%, WSInteger) -> E                             prepend : (%, E) -> %
 qelt : (%, Integer) -> E                               qsetelt : (%, Integer, E) -> %
 qsetelt! : (%, Integer, E) -> %                        removeDuplicates : % -> %
 replacePart : (%, %) -> %                              rest : % -> %
 reverse : (%, WSList(WSInteger)) -> %                  reverse : (%, WSInteger) -> %
 reverse : % -> %                                       riffle : (%, %, %) -> %
 riffle : (%, %) -> %                                   setIntersection : (%, %) -> %
 setelt : (%, Integer, E) -> %                          setelt! : (%, Integer, E) -> %
 sort : % -> %                                          sorted? : % -> Boolean
 string : % -> String                                   take : (%, WSList(WSInteger)) -> %
 take : (%, Integer) -> %                               toString : % -> String
 total : % -> E if E has WSNUM                          union : (%, %) -> %
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `accumulate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L77)\]

accumulate(agg) returns the partial sums of elements in agg.

- **Signature**: `(%)->%`

### `append` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L34)\]

append(l, elt) appends elt to the list l.

- **Signature**: `(%,E)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L100)\]

coerce(list) tries to coerce list to a WSAggregate.

- **Signature**: `(List(E))->%`

### `delete` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L47)\]

delete(l, inds) deletes element(s) of the list l at index(es) inds.

- **Signature**: `(%,WSList(WSInteger))->%`

### `differences` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L74)\]

differences(agg) returns the differences between adjacent elements in agg.

- **Signature**: `(%)->%`

### `dimensions` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L18)\]

dimensions(agg) returns dimensions of agg.

- **Signature**: `(%)->WSList(WSInteger)`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L82)\]

elt(agg,i) returns the i-th element.

- **Signature**: `(%,Integer)->E`

### `extract` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L26)\]

extract(agg, parts) extracts parts of agg.

- **Signature**: `(%,WSExpression)->%`

### `first` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L28)\]

first(agg) returns the first element of agg.

- **Signature**: `(%)->E`

### `insert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L45)\]

insert(l, elt, ind) inserts elt to the list l at index ind.

- **Signature**: `(%,E,WSInteger)->%`

### `intersection` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L53)\]

intersection(agg1, agg2) is the intersection operator.

- **Signature**: `(%,%)->%`

### `jWSAggregate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L102)\]

jWSAggregate(list) constructs agg to a WSAggregate.

- **Signature**: `(List(E))->%`

### `join` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L49)\]

join(agg1, agg2) joins the two aggregates agg1 and agg2.

- **Signature**: `(%,%)->%`

### `last` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L30)\]

last(agg) returns the last element of agg.

- **Signature**: `(%)->E`

### `length` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L15)\]

length(agg) returns the length of agg. 0 if it is a scalar, an index for example.

- **Signature**: `(%)->WSInteger`

### `part` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L20)\]

part(agg,i) returns the i-th element.

- **Signature**: `(%,WSInteger)->E`

### `prepend` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L40)\]

prepend(l, elt) prepends elt to the list l.

- **Signature**: `(%,E)->%`

### `qelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L84)\]

qelt(agg,i) returns the i-th element. No checks are done at the FriCAS level.

- **Signature**: `(%,Integer)->E`

### `qsetelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L90)\]

qsetelt(l,i,elem) returns a copy of l with i-th element replaced by the element elem. No checks are done at the FriCAS level.

- **Signature**: `(%,Integer,E)->%`

### `qsetelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L95)\]

qsetelt!(l,i,elem) returns l with i-th element replaced by elem. No checks are done at the FriCAS level.

- **Signature**: `(%,Integer,E)->%`

### `removeDuplicates` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L62)\]

removeDuplicates(agg) removes duplicate elements.

- **Signature**: `(%)->%`

### `replacePart` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L98)\]

replacePart(expr, part) replaces expr using rule(s) expressing position(s).

- **Signature**: `(%,%)->%`

### `rest` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L32)\]

rest(agg) returns agg without the first element.

- **Signature**: `(%)->%`

### `reverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L64)\]

reverse(agg) reverses the elements of agg.

- **Signature**: `(%)->%`

reverse(agg, lev) reverses the elements of agg at level lev.

- **Signature**: `(%,WSInteger)->%`

reverse(agg, levels) reverses the elements of agg using levels.

- **Signature**: `(%,WSList(WSInteger))->%`

### `riffle` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L57)\]

riffle(agg, elts) riffles (interleaves) element(s) elts i.e. inserts them.

- **Signature**: `(%,%)->%`

riffle(agg, elts, inds) riffles (interleaves) element(s) elts i.e. inserts them. If inds is min, max, n inserts at position min, min+n, min+2n..max.

- **Signature**: `(%,%,%)->%`

### `setIntersection` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L55)\]

setIntersection(agg1, agg2) is the intersection set operator.

- **Signature**: `(%,%)->%`

### `setelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L87)\]

setelt(l,i,elem) returns a copy of l with i-th element replaced by the element elem.

- **Signature**: `(%,Integer,E)->%`

### `setelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L93)\]

setelt!(l,i,elem) returns l with i-th element replaced by elem.

- **Signature**: `(%,Integer,E)->%`

### `sort` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L68)\]

sort(agg) returns agg in sorted order. For complex numbers, sorts them by their real part first.

- **Signature**: `(%)->%`

### `sorted?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L10)\]

sorted?(agg) checks whether agg is sorted or not.

- **Signature**: `(%)->Boolean`

### `take` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L22)\]

take(l,i) returns the first i elements.

- **Signature**: `(%,Integer)->%`
- **Signature**: `(%,WSList(WSInteger))->%`

### `total` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L72)\]

total(agg) returns the sum of the elements in agg.

- **Signature**: `(%)->E`

### `union` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L51)\]

union(agg1, agg2) is the union set operator. Elements are sorted on output.

- **Signature**: `(%,%)->%`
---
[Back to Index](../index.md)
