# JLF32SquareMatrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L505)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast Julia Float32 square matrix type with no bound checking on elt's. Minimum index is 1.

**JLF32SquareMatrix(n: NonNegativeInteger) is a domain constructor**  
**Abbreviation for JLF32SquareMatrix is JF32SMAT**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?*? : (PositiveInteger, %) -> %
 ?*? : (%, %) -> %                                      ?*? : (NonNegativeInteger, %) -> %
 ?*? : (%, JLFloat32) -> %                              ?*? : (JLFloat32, %) -> %
 ?+? : (%, %) -> %                                      0 : () -> %
 1 : () -> % if JLFloat32 has SRING                     ?=? : (%, %) -> Boolean
 ?^? : (%, PositiveInteger) -> %                        antiCommutator : (%, %) -> %
 coerce : % -> JLFloat32Matrix                          coerce : JLFloat32 -> %
 coerce : % -> OutputForm                               convert : % -> String
 copy : % -> %                                          diagonal : % -> DirectProduct(n,JLFloat32)
 diagonalMatrix : List(JLFloat32) -> %                  diagonalProduct : % -> JLFloat32
 elt : (%, Integer, Integer) -> JLFloat32               empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 latex : % -> String                                    leftPower : (%, PositiveInteger) -> %
 less? : (%, NonNegativeInteger) -> Boolean             listOfLists : % -> List(List(JLFloat32))
 map : ((JLFloat32 -> JLFloat32), %) -> %               matrix : List(List(JLFloat32)) -> %
 maxColIndex : % -> Integer                             maxRowIndex : % -> Integer
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 more? : (%, NonNegativeInteger) -> Boolean             ncols : % -> NonNegativeInteger
 nrows : % -> NonNegativeInteger                        one? : % -> Boolean if JLFloat32 has SRING
 opposite? : (%, %) -> Boolean                          parts : % -> List(JLFloat32) if % has ATFINAG
 qcoerce : JLFloat32Matrix -> %                         qelt : (%, Integer, Integer) -> JLFloat32
 random : () -> % if JLFloat32 has FINITE               retract : % -> JLFloat32
 retractIfCan : % -> Union(JLFloat32,"failed")          rightPower : (%, PositiveInteger) -> %
 row : (%, Integer) -> DirectProduct(n,JLFloat32)       rowEchelon : % -> % if JLFloat32 has EUCDOM
 sample : () -> %                                       scalarMatrix : JLFloat32 -> %
 size? : (%, NonNegativeInteger) -> Boolean             square? : % -> Boolean
 squareMatrix : JLFloat32Matrix -> %                    string : % -> String
 symmetric? : % -> Boolean                              trace : % -> JLFloat32
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (%, DirectProduct(n,JLFloat32)) -> DirectProduct(n,JLFloat32)
 ?*? : (DirectProduct(n,JLFloat32), %) -> DirectProduct(n,JLFloat32)
 ?*? : (%, Integer) -> % if JLFloat32 has LINEXP(INT) and JLFloat32 has RING
 ?*? : (Integer, %) -> % if % has ABELGRP or Integer has ABELGRP and JLFloat32 has LINEXP(INT) and JLFloat32 has RING or JLFloat32 has ABELGRP
 ?-? : (%, %) -> % if % has ABELGRP or Integer has ABELGRP and JLFloat32 has LINEXP(INT) and JLFloat32 has RING or JLFloat32 has ABELGRP
 -? : % -> % if % has ABELGRP or Integer has ABELGRP and JLFloat32 has LINEXP(INT) and JLFloat32 has RING or JLFloat32 has ABELGRP
 ?/? : (%, JLFloat32) -> % if JLFloat32 has FIELD
 D : % -> % if JLFloat32 has DIFRING and JLFloat32 has RING
 D : (%, NonNegativeInteger) -> % if JLFloat32 has DIFRING and JLFloat32 has RING
 D : (%, Symbol) -> % if JLFloat32 has PDRING(SYMBOL) and JLFloat32 has RING
 D : (%, List(Symbol)) -> % if JLFloat32 has PDRING(SYMBOL) and JLFloat32 has RING
 D : (%, Symbol, NonNegativeInteger) -> % if JLFloat32 has PDRING(SYMBOL) and JLFloat32 has RING
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if JLFloat32 has PDRING(SYMBOL) and JLFloat32 has RING
 D : (%, (JLFloat32 -> JLFloat32), NonNegativeInteger) -> % if JLFloat32 has RING
 D : (%, (JLFloat32 -> JLFloat32)) -> % if JLFloat32 has RING
 Pfaffian : % -> JLFloat32 if JLFloat32 has COMRING
 ?^? : (%, Integer) -> % if JLFloat32 has FIELD
 ?^? : (%, NonNegativeInteger) -> % if JLFloat32 has SRING
 annihilate? : (%, %) -> Boolean if JLFloat32 has RING
 antisymmetric? : % -> Boolean if JLFloat32 has ABELGRP
 any? : ((JLFloat32 -> Boolean), %) -> Boolean if % has ATFINAG
 associator : (%, %, %) -> % if JLFloat32 has RING
 characteristic : () -> NonNegativeInteger if JLFloat32 has RING
 coerce : Fraction(Integer) -> % if JLFloat32 has RETRACT(FRAC(INT))
 coerce : Integer -> % if JLFloat32 has RETRACT(INT) or JLFloat32 has RING
 column : (%, Integer) -> DirectProduct(n,JLFloat32)
 columnSpace : % -> List(DirectProduct(n,JLFloat32)) if JLFloat32 has EUCDOM
 commutator : (%, %) -> % if JLFloat32 has RING
 convert : % -> InputForm if JLFloat32 has FINITE
 count : ((JLFloat32 -> Boolean), %) -> NonNegativeInteger if % has ATFINAG
 count : (JLFloat32, %) -> NonNegativeInteger if % has ATFINAG and JLFloat32 has BASTYPE
 determinant : % -> JLFloat32 if JLFloat32 has COMRING
 diagonal? : % -> Boolean if JLFloat32 has ABELMON
 differentiate : % -> % if JLFloat32 has DIFRING and JLFloat32 has RING
 differentiate : (%, NonNegativeInteger) -> % if JLFloat32 has DIFRING and JLFloat32 has RING
 differentiate : (%, Symbol) -> % if JLFloat32 has PDRING(SYMBOL) and JLFloat32 has RING
 differentiate : (%, List(Symbol)) -> % if JLFloat32 has PDRING(SYMBOL) and JLFloat32 has RING
 differentiate : (%, Symbol, NonNegativeInteger) -> % if JLFloat32 has PDRING(SYMBOL) and JLFloat32 has RING
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if JLFloat32 has PDRING(SYMBOL) and JLFloat32 has RING
 differentiate : (%, (JLFloat32 -> JLFloat32), NonNegativeInteger) -> % if JLFloat32 has RING
 differentiate : (%, (JLFloat32 -> JLFloat32)) -> % if JLFloat32 has RING
 elt : (%, Integer, Integer, JLFloat32) -> JLFloat32
 enumerate : () -> List(%) if JLFloat32 has FINITE
 eval : (%, List(Equation(JLFloat32))) -> % if JLFloat32 has EVALAB(JF32) and JLFloat32 has SETCAT
 eval : (%, Equation(JLFloat32)) -> % if JLFloat32 has EVALAB(JF32) and JLFloat32 has SETCAT
 eval : (%, JLFloat32, JLFloat32) -> % if JLFloat32 has EVALAB(JF32) and JLFloat32 has SETCAT
 eval : (%, List(JLFloat32), List(JLFloat32)) -> % if JLFloat32 has EVALAB(JF32) and JLFloat32 has SETCAT
 every? : ((JLFloat32 -> Boolean), %) -> Boolean if % has ATFINAG
 ?exquo? : (%, JLFloat32) -> Union(%,"failed") if JLFloat32 has INTDOM
 hash : % -> SingleInteger if JLFloat32 has FINITE
 hashUpdate! : (HashState, %) -> HashState if JLFloat32 has FINITE
 index : PositiveInteger -> % if JLFloat32 has FINITE
 inverse : % -> Union(%,"failed") if JLFloat32 has FIELD
 leftPower : (%, NonNegativeInteger) -> % if JLFloat32 has SRING
 leftRecip : % -> Union(%,"failed") if JLFloat32 has SRING
 lookup : % -> PositiveInteger if JLFloat32 has FINITE
 map : (((JLFloat32, JLFloat32) -> JLFloat32), %, %) -> %
 map! : ((JLFloat32 -> JLFloat32), %) -> % if % has ATSHMUT
 max : (((JLFloat32, JLFloat32) -> Boolean), %) -> JLFloat32 if % has ATFINAG
 max : % -> JLFloat32 if % has ATFINAG and JLFloat32 has ORDSET
 member? : (JLFloat32, %) -> Boolean if % has ATFINAG and JLFloat32 has BASTYPE
 members : % -> List(JLFloat32) if % has ATFINAG
 min : % -> JLFloat32 if % has ATFINAG and JLFloat32 has ORDSET
 minordet : % -> JLFloat32 if JLFloat32 has COMRING
 nullSpace : % -> List(DirectProduct(n,JLFloat32)) if JLFloat32 has INTDOM
 nullity : % -> NonNegativeInteger if JLFloat32 has INTDOM
 plenaryPower : (%, PositiveInteger) -> % if JLFloat32 has COMRING
 rank : % -> NonNegativeInteger if JLFloat32 has INTDOM
 recip : % -> Union(%,"failed") if JLFloat32 has SRING
 reducedSystem : Matrix(%) -> Matrix(Integer) if JLFloat32 has LINEXP(INT) and JLFloat32 has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if JLFloat32 has LINEXP(INT) and JLFloat32 has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(JLFloat32),vec: Vector(JLFloat32)) if JLFloat32 has RING
 reducedSystem : Matrix(%) -> Matrix(JLFloat32) if JLFloat32 has RING
 retract : % -> Integer if JLFloat32 has RETRACT(INT)
 retract : % -> Fraction(Integer) if JLFloat32 has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Integer,"failed") if JLFloat32 has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if JLFloat32 has RETRACT(FRAC(INT))
 rightPower : (%, NonNegativeInteger) -> % if JLFloat32 has SRING
 rightRecip : % -> Union(%,"failed") if JLFloat32 has SRING
 size : () -> NonNegativeInteger if JLFloat32 has FINITE
 smaller? : (%, %) -> Boolean if JLFloat32 has FINITE
 subtractIfCan : (%, %) -> Union(%,"failed") if % has ABELGRP or Integer has ABELGRP and JLFloat32 has LINEXP(INT) and JLFloat32 has RING or JLFloat32 has ABELGRP
```

## Operations added

### `qcoerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L516)\]

qcoerce(m) coerces m to JLF32SquareMatrix trusting that m is square.

- **Signature**: `(JLFloat32Matrix)->%`

### `squareMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L519)\]

squareMatrix(m) returns a copy of m as a JLF32SquareMatrix.

- **Signature**: `(JLFloat32Matrix)->%`
---
[Back to Index](../index.md)
