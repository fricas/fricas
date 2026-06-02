# JLComplexF64SquareMatrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L1039)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This domain provides a fast Julia Float64 square matrix type with no bound checking on elt's. Minimum index is 1.

**JLComplexF64SquareMatrix(n: NonNegativeInteger) is a domain constructor**  
**Abbreviation for JLComplexF64SquareMatrix is JCF64SMA**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?*? : (PositiveInteger, %) -> %
 ?*? : (%, %) -> %                                      ?*? : (NonNegativeInteger, %) -> %
 ?*? : (%, JLComplexF64) -> %                           ?*? : (JLComplexF64, %) -> %
 ?+? : (%, %) -> %                                      0 : () -> %
 1 : () -> % if JLComplexF64 has SRING                  ?=? : (%, %) -> Boolean
 ?^? : (%, PositiveInteger) -> %                        antiCommutator : (%, %) -> %
 coerce : % -> JLComplexF64Matrix                       coerce : JLComplexF64 -> %
 coerce : % -> OutputForm                               convert : % -> String
 copy : % -> %                                          diagonal : % -> DirectProduct(n,JLComplexF64)
 diagonalMatrix : List(JLComplexF64) -> %               diagonalProduct : % -> JLComplexF64
 elt : (%, Integer, Integer) -> JLComplexF64            empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 latex : % -> String                                    leftPower : (%, PositiveInteger) -> %
 less? : (%, NonNegativeInteger) -> Boolean             listOfLists : % -> List(List(JLComplexF64))
 map : ((JLComplexF64 -> JLComplexF64), %) -> %         matrix : List(List(JLComplexF64)) -> %
 maxColIndex : % -> Integer                             maxRowIndex : % -> Integer
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 more? : (%, NonNegativeInteger) -> Boolean             ncols : % -> NonNegativeInteger
 nrows : % -> NonNegativeInteger                        one? : % -> Boolean if JLComplexF64 has SRING
 opposite? : (%, %) -> Boolean                          qcoerce : JLComplexF64Matrix -> %
 qelt : (%, Integer, Integer) -> JLComplexF64           random : () -> % if JLComplexF64 has FINITE
 retract : % -> JLComplexF64                            retractIfCan : % -> Union(JLComplexF64,"failed")
 rightPower : (%, PositiveInteger) -> %                 sample : () -> %
 scalarMatrix : JLComplexF64 -> %                       size? : (%, NonNegativeInteger) -> Boolean
 square? : % -> Boolean                                 squareMatrix : JLComplexF64Matrix -> %
 string : % -> String                                   symmetric? : % -> Boolean
 trace : % -> JLComplexF64                              zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 ?*? : (%, DirectProduct(n,JLComplexF64)) -> DirectProduct(n,JLComplexF64)
 ?*? : (DirectProduct(n,JLComplexF64), %) -> DirectProduct(n,JLComplexF64)
 ?*? : (%, Integer) -> % if JLComplexF64 has LINEXP(INT) and JLComplexF64 has RING
 ?*? : (Integer, %) -> % if % has ABELGRP or Integer has ABELGRP and JLComplexF64 has LINEXP(INT) and JLComplexF64 has RING or JLComplexF64 has ABELGRP
 ?-? : (%, %) -> % if % has ABELGRP or Integer has ABELGRP and JLComplexF64 has LINEXP(INT) and JLComplexF64 has RING or JLComplexF64 has ABELGRP
 -? : % -> % if % has ABELGRP or Integer has ABELGRP and JLComplexF64 has LINEXP(INT) and JLComplexF64 has RING or JLComplexF64 has ABELGRP
 ?/? : (%, JLComplexF64) -> % if JLComplexF64 has FIELD
 D : % -> % if JLComplexF64 has DIFRING and JLComplexF64 has RING
 D : (%, NonNegativeInteger) -> % if JLComplexF64 has DIFRING and JLComplexF64 has RING
 D : (%, Symbol) -> % if JLComplexF64 has PDRING(SYMBOL) and JLComplexF64 has RING
 D : (%, List(Symbol)) -> % if JLComplexF64 has PDRING(SYMBOL) and JLComplexF64 has RING
 D : (%, Symbol, NonNegativeInteger) -> % if JLComplexF64 has PDRING(SYMBOL) and JLComplexF64 has RING
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if JLComplexF64 has PDRING(SYMBOL) and JLComplexF64 has RING
 D : (%, (JLComplexF64 -> JLComplexF64), NonNegativeInteger) -> % if JLComplexF64 has RING
 D : (%, (JLComplexF64 -> JLComplexF64)) -> % if JLComplexF64 has RING
 Pfaffian : % -> JLComplexF64 if JLComplexF64 has COMRING
 ?^? : (%, Integer) -> % if JLComplexF64 has FIELD
 ?^? : (%, NonNegativeInteger) -> % if JLComplexF64 has SRING
 annihilate? : (%, %) -> Boolean if JLComplexF64 has RING
 antisymmetric? : % -> Boolean if JLComplexF64 has ABELGRP
 any? : ((JLComplexF64 -> Boolean), %) -> Boolean if % has ATFINAG
 associator : (%, %, %) -> % if JLComplexF64 has RING
 characteristic : () -> NonNegativeInteger if JLComplexF64 has RING
 coerce : Fraction(Integer) -> % if JLComplexF64 has RETRACT(FRAC(INT))
 coerce : Integer -> % if JLComplexF64 has RETRACT(INT) or JLComplexF64 has RING
 column : (%, Integer) -> DirectProduct(n,JLComplexF64)
 columnSpace : % -> List(DirectProduct(n,JLComplexF64)) if JLComplexF64 has EUCDOM
 commutator : (%, %) -> % if JLComplexF64 has RING
 convert : % -> InputForm if JLComplexF64 has FINITE
 count : ((JLComplexF64 -> Boolean), %) -> NonNegativeInteger if % has ATFINAG
 count : (JLComplexF64, %) -> NonNegativeInteger if % has ATFINAG and JLComplexF64 has BASTYPE
 determinant : % -> JLComplexF64 if JLComplexF64 has COMRING
 diagonal? : % -> Boolean if JLComplexF64 has ABELMON
 differentiate : % -> % if JLComplexF64 has DIFRING and JLComplexF64 has RING
 differentiate : (%, NonNegativeInteger) -> % if JLComplexF64 has DIFRING and JLComplexF64 has RING
 differentiate : (%, Symbol) -> % if JLComplexF64 has PDRING(SYMBOL) and JLComplexF64 has RING
 differentiate : (%, List(Symbol)) -> % if JLComplexF64 has PDRING(SYMBOL) and JLComplexF64 has RING
 differentiate : (%, Symbol, NonNegativeInteger) -> % if JLComplexF64 has PDRING(SYMBOL) and JLComplexF64 has RING
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if JLComplexF64 has PDRING(SYMBOL) and JLComplexF64 has RING
 differentiate : (%, (JLComplexF64 -> JLComplexF64), NonNegativeInteger) -> % if JLComplexF64 has RING
 differentiate : (%, (JLComplexF64 -> JLComplexF64)) -> % if JLComplexF64 has RING
 elt : (%, Integer, Integer, JLComplexF64) -> JLComplexF64
 enumerate : () -> List(%) if JLComplexF64 has FINITE
 eval : (%, List(Equation(JLComplexF64))) -> % if JLComplexF64 has EVALAB(JCF64) and JLComplexF64 has SETCAT
 eval : (%, Equation(JLComplexF64)) -> % if JLComplexF64 has EVALAB(JCF64) and JLComplexF64 has SETCAT
 eval : (%, JLComplexF64, JLComplexF64) -> % if JLComplexF64 has EVALAB(JCF64) and JLComplexF64 has SETCAT
 eval : (%, List(JLComplexF64), List(JLComplexF64)) -> % if JLComplexF64 has EVALAB(JCF64) and JLComplexF64 has SETCAT
 every? : ((JLComplexF64 -> Boolean), %) -> Boolean if % has ATFINAG
 ?exquo? : (%, JLComplexF64) -> Union(%,"failed") if JLComplexF64 has INTDOM
 hash : % -> SingleInteger if JLComplexF64 has FINITE
 hashUpdate! : (HashState, %) -> HashState if JLComplexF64 has FINITE
 index : PositiveInteger -> % if JLComplexF64 has FINITE
 inverse : % -> Union(%,"failed") if JLComplexF64 has FIELD
 leftPower : (%, NonNegativeInteger) -> % if JLComplexF64 has SRING
 leftRecip : % -> Union(%,"failed") if JLComplexF64 has SRING
 lookup : % -> PositiveInteger if JLComplexF64 has FINITE
 map : (((JLComplexF64, JLComplexF64) -> JLComplexF64), %, %) -> %
 map! : ((JLComplexF64 -> JLComplexF64), %) -> % if % has ATSHMUT
 max : (((JLComplexF64, JLComplexF64) -> Boolean), %) -> JLComplexF64 if % has ATFINAG
 max : % -> JLComplexF64 if % has ATFINAG and JLComplexF64 has ORDSET
 member? : (JLComplexF64, %) -> Boolean if % has ATFINAG and JLComplexF64 has BASTYPE
 members : % -> List(JLComplexF64) if % has ATFINAG
 min : % -> JLComplexF64 if % has ATFINAG and JLComplexF64 has ORDSET
 minordet : % -> JLComplexF64 if JLComplexF64 has COMRING
 nullSpace : % -> List(DirectProduct(n,JLComplexF64)) if JLComplexF64 has INTDOM
 nullity : % -> NonNegativeInteger if JLComplexF64 has INTDOM
 parts : % -> List(JLComplexF64) if % has ATFINAG
 plenaryPower : (%, PositiveInteger) -> % if JLComplexF64 has COMRING
 rank : % -> NonNegativeInteger if JLComplexF64 has INTDOM
 recip : % -> Union(%,"failed") if JLComplexF64 has SRING
 reducedSystem : Matrix(%) -> Matrix(Integer) if JLComplexF64 has LINEXP(INT) and JLComplexF64 has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if JLComplexF64 has LINEXP(INT) and JLComplexF64 has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(JLComplexF64),vec: Vector(JLComplexF64)) if JLComplexF64 has RING
 reducedSystem : Matrix(%) -> Matrix(JLComplexF64) if JLComplexF64 has RING
 retract : % -> Integer if JLComplexF64 has RETRACT(INT)
 retract : % -> Fraction(Integer) if JLComplexF64 has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Integer,"failed") if JLComplexF64 has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if JLComplexF64 has RETRACT(FRAC(INT))
 rightPower : (%, NonNegativeInteger) -> % if JLComplexF64 has SRING
 rightRecip : % -> Union(%,"failed") if JLComplexF64 has SRING
 row : (%, Integer) -> DirectProduct(n,JLComplexF64)
 rowEchelon : % -> % if JLComplexF64 has EUCDOM
 size : () -> NonNegativeInteger if JLComplexF64 has FINITE
 smaller? : (%, %) -> Boolean if JLComplexF64 has FINITE
 subtractIfCan : (%, %) -> Union(%,"failed") if % has ABELGRP or Integer has ABELGRP and JLComplexF64 has LINEXP(INT) and JLComplexF64 has RING or JLComplexF64 has ABELGRP
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L1050)\]

coerce(m) coerces m to JLComplexF64Matrix

- **Signature**: `(%)->JLComplexF64Matrix`

### `qcoerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L1047)\]

qcoerce(m) coerces m to JLComplexF64SquareMatrix trusting that m is square.

- **Signature**: `(JLComplexF64Matrix)->%`

### `squareMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L1052)\]

squareMatrix(m) returns a copy of m as a JLComplexF64SquareMatrix.

- **Signature**: `(JLComplexF64Matrix)->%`
---
[Back to Index](../index.md)
