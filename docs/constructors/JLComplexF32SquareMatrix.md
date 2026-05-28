# JLComplexF32SquareMatrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L950)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast Julia Float32 square matrix type with no bound checking on elt's. Minimum index is 1.

**JLComplexF32SquareMatrix(n: NonNegativeInteger) is a domain constructor**  
**Abbreviation for JLComplexF32SquareMatrix is JCF32SMA**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?*? : (PositiveInteger, %) -> %
 ?*? : (%, %) -> %                                      ?*? : (NonNegativeInteger, %) -> %
 ?*? : (%, JLComplexF32) -> %                           ?*? : (JLComplexF32, %) -> %
 ?+? : (%, %) -> %                                      0 : () -> %
 1 : () -> % if JLComplexF32 has SRING                  ?=? : (%, %) -> Boolean
 ?^? : (%, PositiveInteger) -> %                        antiCommutator : (%, %) -> %
 coerce : % -> JLComplexF32Matrix                       coerce : JLComplexF32 -> %
 coerce : % -> OutputForm                               convert : % -> String
 copy : % -> %                                          diagonal : % -> DirectProduct(n,JLComplexF32)
 diagonalMatrix : List(JLComplexF32) -> %               diagonalProduct : % -> JLComplexF32
 elt : (%, Integer, Integer) -> JLComplexF32            empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 latex : % -> String                                    leftPower : (%, PositiveInteger) -> %
 less? : (%, NonNegativeInteger) -> Boolean             listOfLists : % -> List(List(JLComplexF32))
 map : ((JLComplexF32 -> JLComplexF32), %) -> %         matrix : List(List(JLComplexF32)) -> %
 maxColIndex : % -> Integer                             maxRowIndex : % -> Integer
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 more? : (%, NonNegativeInteger) -> Boolean             ncols : % -> NonNegativeInteger
 nrows : % -> NonNegativeInteger                        one? : % -> Boolean if JLComplexF32 has SRING
 opposite? : (%, %) -> Boolean                          qcoerce : JLComplexF32Matrix -> %
 qelt : (%, Integer, Integer) -> JLComplexF32           random : () -> % if JLComplexF32 has FINITE
 retract : % -> JLComplexF32                            retractIfCan : % -> Union(JLComplexF32,"failed")
 rightPower : (%, PositiveInteger) -> %                 sample : () -> %
 scalarMatrix : JLComplexF32 -> %                       size? : (%, NonNegativeInteger) -> Boolean
 square? : % -> Boolean                                 squareMatrix : JLComplexF32Matrix -> %
 string : % -> String                                   symmetric? : % -> Boolean
 trace : % -> JLComplexF32                              zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 ?*? : (%, DirectProduct(n,JLComplexF32)) -> DirectProduct(n,JLComplexF32)
 ?*? : (DirectProduct(n,JLComplexF32), %) -> DirectProduct(n,JLComplexF32)
 ?*? : (%, Integer) -> % if JLComplexF32 has LINEXP(INT) and JLComplexF32 has RING
 ?*? : (Integer, %) -> % if % has ABELGRP or Integer has ABELGRP and JLComplexF32 has LINEXP(INT) and JLComplexF32 has RING or JLComplexF32 has ABELGRP
 ?-? : (%, %) -> % if % has ABELGRP or Integer has ABELGRP and JLComplexF32 has LINEXP(INT) and JLComplexF32 has RING or JLComplexF32 has ABELGRP
 -? : % -> % if % has ABELGRP or Integer has ABELGRP and JLComplexF32 has LINEXP(INT) and JLComplexF32 has RING or JLComplexF32 has ABELGRP
 ?/? : (%, JLComplexF32) -> % if JLComplexF32 has FIELD
 D : % -> % if JLComplexF32 has DIFRING and JLComplexF32 has RING
 D : (%, NonNegativeInteger) -> % if JLComplexF32 has DIFRING and JLComplexF32 has RING
 D : (%, Symbol) -> % if JLComplexF32 has PDRING(SYMBOL) and JLComplexF32 has RING
 D : (%, List(Symbol)) -> % if JLComplexF32 has PDRING(SYMBOL) and JLComplexF32 has RING
 D : (%, Symbol, NonNegativeInteger) -> % if JLComplexF32 has PDRING(SYMBOL) and JLComplexF32 has RING
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if JLComplexF32 has PDRING(SYMBOL) and JLComplexF32 has RING
 D : (%, (JLComplexF32 -> JLComplexF32), NonNegativeInteger) -> % if JLComplexF32 has RING
 D : (%, (JLComplexF32 -> JLComplexF32)) -> % if JLComplexF32 has RING
 Pfaffian : % -> JLComplexF32 if JLComplexF32 has COMRING
 ?^? : (%, Integer) -> % if JLComplexF32 has FIELD
 ?^? : (%, NonNegativeInteger) -> % if JLComplexF32 has SRING
 annihilate? : (%, %) -> Boolean if JLComplexF32 has RING
 antisymmetric? : % -> Boolean if JLComplexF32 has ABELGRP
 any? : ((JLComplexF32 -> Boolean), %) -> Boolean if % has ATFINAG
 associator : (%, %, %) -> % if JLComplexF32 has RING
 characteristic : () -> NonNegativeInteger if JLComplexF32 has RING
 coerce : Fraction(Integer) -> % if JLComplexF32 has RETRACT(FRAC(INT))
 coerce : Integer -> % if JLComplexF32 has RETRACT(INT) or JLComplexF32 has RING
 column : (%, Integer) -> DirectProduct(n,JLComplexF32)
 columnSpace : % -> List(DirectProduct(n,JLComplexF32)) if JLComplexF32 has EUCDOM
 commutator : (%, %) -> % if JLComplexF32 has RING
 convert : % -> InputForm if JLComplexF32 has FINITE
 count : ((JLComplexF32 -> Boolean), %) -> NonNegativeInteger if % has ATFINAG
 count : (JLComplexF32, %) -> NonNegativeInteger if % has ATFINAG and JLComplexF32 has BASTYPE
 determinant : % -> JLComplexF32 if JLComplexF32 has COMRING
 diagonal? : % -> Boolean if JLComplexF32 has ABELMON
 differentiate : % -> % if JLComplexF32 has DIFRING and JLComplexF32 has RING
 differentiate : (%, NonNegativeInteger) -> % if JLComplexF32 has DIFRING and JLComplexF32 has RING
 differentiate : (%, Symbol) -> % if JLComplexF32 has PDRING(SYMBOL) and JLComplexF32 has RING
 differentiate : (%, List(Symbol)) -> % if JLComplexF32 has PDRING(SYMBOL) and JLComplexF32 has RING
 differentiate : (%, Symbol, NonNegativeInteger) -> % if JLComplexF32 has PDRING(SYMBOL) and JLComplexF32 has RING
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if JLComplexF32 has PDRING(SYMBOL) and JLComplexF32 has RING
 differentiate : (%, (JLComplexF32 -> JLComplexF32), NonNegativeInteger) -> % if JLComplexF32 has RING
 differentiate : (%, (JLComplexF32 -> JLComplexF32)) -> % if JLComplexF32 has RING
 elt : (%, Integer, Integer, JLComplexF32) -> JLComplexF32
 enumerate : () -> List(%) if JLComplexF32 has FINITE
 eval : (%, List(Equation(JLComplexF32))) -> % if JLComplexF32 has EVALAB(JCF32) and JLComplexF32 has SETCAT
 eval : (%, Equation(JLComplexF32)) -> % if JLComplexF32 has EVALAB(JCF32) and JLComplexF32 has SETCAT
 eval : (%, JLComplexF32, JLComplexF32) -> % if JLComplexF32 has EVALAB(JCF32) and JLComplexF32 has SETCAT
 eval : (%, List(JLComplexF32), List(JLComplexF32)) -> % if JLComplexF32 has EVALAB(JCF32) and JLComplexF32 has SETCAT
 every? : ((JLComplexF32 -> Boolean), %) -> Boolean if % has ATFINAG
 ?exquo? : (%, JLComplexF32) -> Union(%,"failed") if JLComplexF32 has INTDOM
 hash : % -> SingleInteger if JLComplexF32 has FINITE
 hashUpdate! : (HashState, %) -> HashState if JLComplexF32 has FINITE
 index : PositiveInteger -> % if JLComplexF32 has FINITE
 inverse : % -> Union(%,"failed") if JLComplexF32 has FIELD
 leftPower : (%, NonNegativeInteger) -> % if JLComplexF32 has SRING
 leftRecip : % -> Union(%,"failed") if JLComplexF32 has SRING
 lookup : % -> PositiveInteger if JLComplexF32 has FINITE
 map : (((JLComplexF32, JLComplexF32) -> JLComplexF32), %, %) -> %
 map! : ((JLComplexF32 -> JLComplexF32), %) -> % if % has ATSHMUT
 max : (((JLComplexF32, JLComplexF32) -> Boolean), %) -> JLComplexF32 if % has ATFINAG
 max : % -> JLComplexF32 if % has ATFINAG and JLComplexF32 has ORDSET
 member? : (JLComplexF32, %) -> Boolean if % has ATFINAG and JLComplexF32 has BASTYPE
 members : % -> List(JLComplexF32) if % has ATFINAG
 min : % -> JLComplexF32 if % has ATFINAG and JLComplexF32 has ORDSET
 minordet : % -> JLComplexF32 if JLComplexF32 has COMRING
 nullSpace : % -> List(DirectProduct(n,JLComplexF32)) if JLComplexF32 has INTDOM
 nullity : % -> NonNegativeInteger if JLComplexF32 has INTDOM
 parts : % -> List(JLComplexF32) if % has ATFINAG
 plenaryPower : (%, PositiveInteger) -> % if JLComplexF32 has COMRING
 rank : % -> NonNegativeInteger if JLComplexF32 has INTDOM
 recip : % -> Union(%,"failed") if JLComplexF32 has SRING
 reducedSystem : Matrix(%) -> Matrix(Integer) if JLComplexF32 has LINEXP(INT) and JLComplexF32 has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if JLComplexF32 has LINEXP(INT) and JLComplexF32 has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(JLComplexF32),vec: Vector(JLComplexF32)) if JLComplexF32 has RING
 reducedSystem : Matrix(%) -> Matrix(JLComplexF32) if JLComplexF32 has RING
 retract : % -> Integer if JLComplexF32 has RETRACT(INT)
 retract : % -> Fraction(Integer) if JLComplexF32 has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Integer,"failed") if JLComplexF32 has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if JLComplexF32 has RETRACT(FRAC(INT))
 rightPower : (%, NonNegativeInteger) -> % if JLComplexF32 has SRING
 rightRecip : % -> Union(%,"failed") if JLComplexF32 has SRING
 row : (%, Integer) -> DirectProduct(n,JLComplexF32)
 rowEchelon : % -> % if JLComplexF32 has EUCDOM
 size : () -> NonNegativeInteger if JLComplexF32 has FINITE
 smaller? : (%, %) -> Boolean if JLComplexF32 has FINITE
 subtractIfCan : (%, %) -> Union(%,"failed") if % has ABELGRP or Integer has ABELGRP and JLComplexF32 has LINEXP(INT) and JLComplexF32 has RING or JLComplexF32 has ABELGRP
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L964)\]

coerce(m) coerces m to JLComplexF32Matrix

- **Signature**: `(%)->JLComplexF32Matrix`

### `qcoerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L961)\]

qcoerce(m) coerces m to JLComplexF32SquareMatrix trusting that m is square.

- **Signature**: `(JLComplexF32Matrix)->%`

### `squareMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray32.spad#L966)\]

squareMatrix(m) returns a copy of m as a JLComplexF32SquareMatrix.

- **Signature**: `(JLComplexF32Matrix)->%`
---
[Back to Index](../index.md)
