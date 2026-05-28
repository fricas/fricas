# JLF64SquareMatrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L583)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast Julia Float64 square matrix type with no bound checking on elt's. Minimum index is 1.

**JLF64SquareMatrix(n: NonNegativeInteger) is a domain constructor**  
**Abbreviation for JLF64SquareMatrix is JF64SMAT**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?*? : (PositiveInteger, %) -> %
 ?*? : (%, %) -> %                                      ?*? : (NonNegativeInteger, %) -> %
 ?*? : (%, JLFloat64) -> %                              ?*? : (JLFloat64, %) -> %
 ?+? : (%, %) -> %                                      0 : () -> %
 1 : () -> % if JLFloat64 has SRING                     ?=? : (%, %) -> Boolean
 ?^? : (%, PositiveInteger) -> %                        antiCommutator : (%, %) -> %
 coerce : % -> JLFloat64Matrix                          coerce : JLFloat64 -> %
 coerce : % -> OutputForm                               convert : % -> String
 copy : % -> %                                          diagonal : % -> DirectProduct(n,JLFloat64)
 diagonalMatrix : List(JLFloat64) -> %                  diagonalProduct : % -> JLFloat64
 elt : (%, Integer, Integer) -> JLFloat64               empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 latex : % -> String                                    leftPower : (%, PositiveInteger) -> %
 less? : (%, NonNegativeInteger) -> Boolean             listOfLists : % -> List(List(JLFloat64))
 map : ((JLFloat64 -> JLFloat64), %) -> %               matrix : List(List(JLFloat64)) -> %
 maxColIndex : % -> Integer                             maxRowIndex : % -> Integer
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 more? : (%, NonNegativeInteger) -> Boolean             ncols : % -> NonNegativeInteger
 nrows : % -> NonNegativeInteger                        one? : % -> Boolean if JLFloat64 has SRING
 opposite? : (%, %) -> Boolean                          parts : % -> List(JLFloat64) if % has ATFINAG
 qcoerce : JLFloat64Matrix -> %                         qelt : (%, Integer, Integer) -> JLFloat64
 random : () -> % if JLFloat64 has FINITE               retract : % -> JLFloat64
 retractIfCan : % -> Union(JLFloat64,"failed")          rightPower : (%, PositiveInteger) -> %
 row : (%, Integer) -> DirectProduct(n,JLFloat64)       rowEchelon : % -> % if JLFloat64 has EUCDOM
 sample : () -> %                                       scalarMatrix : JLFloat64 -> %
 size? : (%, NonNegativeInteger) -> Boolean             square? : % -> Boolean
 squareMatrix : JLFloat64Matrix -> %                    string : % -> String
 symmetric? : % -> Boolean                              trace : % -> JLFloat64
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (%, DirectProduct(n,JLFloat64)) -> DirectProduct(n,JLFloat64)
 ?*? : (DirectProduct(n,JLFloat64), %) -> DirectProduct(n,JLFloat64)
 ?*? : (%, Integer) -> % if JLFloat64 has LINEXP(INT) and JLFloat64 has RING
 ?*? : (Integer, %) -> % if % has ABELGRP or Integer has ABELGRP and JLFloat64 has LINEXP(INT) and JLFloat64 has RING or JLFloat64 has ABELGRP
 ?-? : (%, %) -> % if % has ABELGRP or Integer has ABELGRP and JLFloat64 has LINEXP(INT) and JLFloat64 has RING or JLFloat64 has ABELGRP
 -? : % -> % if % has ABELGRP or Integer has ABELGRP and JLFloat64 has LINEXP(INT) and JLFloat64 has RING or JLFloat64 has ABELGRP
 ?/? : (%, JLFloat64) -> % if JLFloat64 has FIELD
 D : % -> % if JLFloat64 has DIFRING and JLFloat64 has RING
 D : (%, NonNegativeInteger) -> % if JLFloat64 has DIFRING and JLFloat64 has RING
 D : (%, Symbol) -> % if JLFloat64 has PDRING(SYMBOL) and JLFloat64 has RING
 D : (%, List(Symbol)) -> % if JLFloat64 has PDRING(SYMBOL) and JLFloat64 has RING
 D : (%, Symbol, NonNegativeInteger) -> % if JLFloat64 has PDRING(SYMBOL) and JLFloat64 has RING
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if JLFloat64 has PDRING(SYMBOL) and JLFloat64 has RING
 D : (%, (JLFloat64 -> JLFloat64), NonNegativeInteger) -> % if JLFloat64 has RING
 D : (%, (JLFloat64 -> JLFloat64)) -> % if JLFloat64 has RING
 Pfaffian : % -> JLFloat64 if JLFloat64 has COMRING
 ?^? : (%, Integer) -> % if JLFloat64 has FIELD
 ?^? : (%, NonNegativeInteger) -> % if JLFloat64 has SRING
 annihilate? : (%, %) -> Boolean if JLFloat64 has RING
 antisymmetric? : % -> Boolean if JLFloat64 has ABELGRP
 any? : ((JLFloat64 -> Boolean), %) -> Boolean if % has ATFINAG
 associator : (%, %, %) -> % if JLFloat64 has RING
 characteristic : () -> NonNegativeInteger if JLFloat64 has RING
 coerce : Fraction(Integer) -> % if JLFloat64 has RETRACT(FRAC(INT))
 coerce : Integer -> % if JLFloat64 has RETRACT(INT) or JLFloat64 has RING
 column : (%, Integer) -> DirectProduct(n,JLFloat64)
 columnSpace : % -> List(DirectProduct(n,JLFloat64)) if JLFloat64 has EUCDOM
 commutator : (%, %) -> % if JLFloat64 has RING
 convert : % -> InputForm if JLFloat64 has FINITE
 count : ((JLFloat64 -> Boolean), %) -> NonNegativeInteger if % has ATFINAG
 count : (JLFloat64, %) -> NonNegativeInteger if % has ATFINAG and JLFloat64 has BASTYPE
 determinant : % -> JLFloat64 if JLFloat64 has COMRING
 diagonal? : % -> Boolean if JLFloat64 has ABELMON
 differentiate : % -> % if JLFloat64 has DIFRING and JLFloat64 has RING
 differentiate : (%, NonNegativeInteger) -> % if JLFloat64 has DIFRING and JLFloat64 has RING
 differentiate : (%, Symbol) -> % if JLFloat64 has PDRING(SYMBOL) and JLFloat64 has RING
 differentiate : (%, List(Symbol)) -> % if JLFloat64 has PDRING(SYMBOL) and JLFloat64 has RING
 differentiate : (%, Symbol, NonNegativeInteger) -> % if JLFloat64 has PDRING(SYMBOL) and JLFloat64 has RING
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if JLFloat64 has PDRING(SYMBOL) and JLFloat64 has RING
 differentiate : (%, (JLFloat64 -> JLFloat64), NonNegativeInteger) -> % if JLFloat64 has RING
 differentiate : (%, (JLFloat64 -> JLFloat64)) -> % if JLFloat64 has RING
 elt : (%, Integer, Integer, JLFloat64) -> JLFloat64
 enumerate : () -> List(%) if JLFloat64 has FINITE
 eval : (%, List(Equation(JLFloat64))) -> % if JLFloat64 has EVALAB(JF64) and JLFloat64 has SETCAT
 eval : (%, Equation(JLFloat64)) -> % if JLFloat64 has EVALAB(JF64) and JLFloat64 has SETCAT
 eval : (%, JLFloat64, JLFloat64) -> % if JLFloat64 has EVALAB(JF64) and JLFloat64 has SETCAT
 eval : (%, List(JLFloat64), List(JLFloat64)) -> % if JLFloat64 has EVALAB(JF64) and JLFloat64 has SETCAT
 every? : ((JLFloat64 -> Boolean), %) -> Boolean if % has ATFINAG
 ?exquo? : (%, JLFloat64) -> Union(%,"failed") if JLFloat64 has INTDOM
 hash : % -> SingleInteger if JLFloat64 has FINITE
 hashUpdate! : (HashState, %) -> HashState if JLFloat64 has FINITE
 index : PositiveInteger -> % if JLFloat64 has FINITE
 inverse : % -> Union(%,"failed") if JLFloat64 has FIELD
 leftPower : (%, NonNegativeInteger) -> % if JLFloat64 has SRING
 leftRecip : % -> Union(%,"failed") if JLFloat64 has SRING
 lookup : % -> PositiveInteger if JLFloat64 has FINITE
 map : (((JLFloat64, JLFloat64) -> JLFloat64), %, %) -> %
 map! : ((JLFloat64 -> JLFloat64), %) -> % if % has ATSHMUT
 max : (((JLFloat64, JLFloat64) -> Boolean), %) -> JLFloat64 if % has ATFINAG
 max : % -> JLFloat64 if % has ATFINAG and JLFloat64 has ORDSET
 member? : (JLFloat64, %) -> Boolean if % has ATFINAG and JLFloat64 has BASTYPE
 members : % -> List(JLFloat64) if % has ATFINAG
 min : % -> JLFloat64 if % has ATFINAG and JLFloat64 has ORDSET
 minordet : % -> JLFloat64 if JLFloat64 has COMRING
 nullSpace : % -> List(DirectProduct(n,JLFloat64)) if JLFloat64 has INTDOM
 nullity : % -> NonNegativeInteger if JLFloat64 has INTDOM
 plenaryPower : (%, PositiveInteger) -> % if JLFloat64 has COMRING
 rank : % -> NonNegativeInteger if JLFloat64 has INTDOM
 recip : % -> Union(%,"failed") if JLFloat64 has SRING
 reducedSystem : Matrix(%) -> Matrix(Integer) if JLFloat64 has LINEXP(INT) and JLFloat64 has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if JLFloat64 has LINEXP(INT) and JLFloat64 has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(JLFloat64),vec: Vector(JLFloat64)) if JLFloat64 has RING
 reducedSystem : Matrix(%) -> Matrix(JLFloat64) if JLFloat64 has RING
 retract : % -> Integer if JLFloat64 has RETRACT(INT)
 retract : % -> Fraction(Integer) if JLFloat64 has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Integer,"failed") if JLFloat64 has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if JLFloat64 has RETRACT(FRAC(INT))
 rightPower : (%, NonNegativeInteger) -> % if JLFloat64 has SRING
 rightRecip : % -> Union(%,"failed") if JLFloat64 has SRING
 size : () -> NonNegativeInteger if JLFloat64 has FINITE
 smaller? : (%, %) -> Boolean if JLFloat64 has FINITE
 subtractIfCan : (%, %) -> Union(%,"failed") if % has ABELGRP or Integer has ABELGRP and JLFloat64 has LINEXP(INT) and JLFloat64 has RING or JLFloat64 has ABELGRP
```

## Operations added

### `qcoerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L591)\]

qcoerce(m) coerces m to JLF64SquareMatrix trusting that m is square.

- **Signature**: `(JLFloat64Matrix)->%`

### `squareMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L594)\]

squareMatrix(m) returns a copy of m as a JLF64SquareMatrix.

- **Signature**: `(JLFloat64Matrix)->%`
---
[Back to Index](../index.md)
