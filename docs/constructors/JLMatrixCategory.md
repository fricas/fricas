# JLMatrixCategory

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jarray64.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This category provides Julia matrices.

**JLMatrixCategory(R: Join(JLType,AbelianMonoid),Row: FiniteLinearAggregate(t#1),Col: FiniteLinearAggregate(t#1)) is a category constructor**  
**Abbreviation for JLMatrixCategory is JMATCAT**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          ?*? : (Integer, %) -> % if R has ABELGRP
 ?*? : (%, %) -> % if R has SRNG                        ?*? : (R, %) -> % if R has SRNG
 ?*? : (%, R) -> % if R has SRNG                        ?*? : (%, Col) -> Col if R has SRNG
 ?*? : (Row, %) -> Row if R has SRNG                    ?+? : (%, %) -> % if R has ABELMON
 ?-? : (%, %) -> % if R has ABELGRP                     -? : % -> % if R has ABELGRP
 ?/? : (%, R) -> % if R has FIELD                       ?=? : (%, %) -> Boolean
 Pfaffian : % -> R if R has COMRING                     ?^? : (%, Integer) -> % if R has FIELD
 antisymmetric? : % -> Boolean if R has ABELGRP         array2 : List(List(R)) -> %
 blockConcat : List(List(%)) -> %                       coerce : Col -> %
 coerce : % -> OutputForm                               colSlice : % -> Segment(Integer)
 column : (%, Integer) -> Col                           columnSpace : % -> List(Col) if R has EUCDOM
 convert : % -> String                                  copy : % -> %
 determinant : % -> R if R has COMRING                  diagonal? : % -> Boolean if R has ABELMON
 diagonalMatrix : List(R) -> %                          diagonalMatrix : List(%) -> %
 elt : (%, Integer, Integer) -> R                       elt : (%, Integer, Integer, R) -> R
 elt : (%, Integer, List(Integer)) -> %                 elt : (%, List(Integer), Integer) -> %
 elt : (%, List(Integer), List(Integer)) -> %           elt : (%, List(Integer), Segment(Integer)) -> %
 elt : (%, Segment(Integer), List(Integer)) -> %        elt : (%, Integer, List(Segment(Integer))) -> %
 elt : (%, List(Segment(Integer)), Integer) -> %        empty : () -> %
 empty? : % -> Boolean                                  eq? : (%, %) -> Boolean
 fill! : (%, R) -> %                                    hash : % -> SingleInteger if R has HASHABL
 horizConcat : (%, %) -> %                              horizConcat : List(%) -> %
 horizSplit : (%, PositiveInteger) -> List(%)           kroneckerProduct : (%, %) -> % if R has SRNG
 kroneckerProduct : List(%) -> % if R has SRNG          kroneckerSum : (%, %) -> %
 kroneckerSum : List(%) -> %                            latex : % -> String
 less? : (%, NonNegativeInteger) -> Boolean             listOfLists : % -> List(List(R))
 map : ((R -> R), %) -> %                               map : (((R, R) -> R), %, %) -> %
 map : (((R, R) -> R), %, %, R) -> %                    map! : ((R -> R), %) -> %
 matrix : List(List(R)) -> %                            maxColIndex : % -> Integer
 maxRowIndex : % -> Integer                             members : % -> List(R) if % has ATFINAG
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 minordet : % -> R if R has COMRING                     more? : (%, NonNegativeInteger) -> Boolean
 ncols : % -> NonNegativeInteger                        nrows : % -> NonNegativeInteger
 nullSpace : % -> List(Col) if R has INTDOM             parts : % -> List(R)
 qelt : (%, Integer, Integer) -> R                      qsetelt! : (%, Integer, Integer, R) -> R
 rank : % -> NonNegativeInteger if R has INTDOM         row : (%, Integer) -> Row
 rowEchelon : % -> % if R has EUCDOM                    rowSlice : % -> Segment(Integer)
 sample : () -> %                                       scalarMatrix : (NonNegativeInteger, R) -> %
 setColumn! : (%, Integer, Col) -> %                    setRow! : (%, Integer, Row) -> %
 setelt! : (%, Integer, Integer, R) -> R                setelt! : (%, Integer, List(Integer), %) -> %
 setelt! : (%, List(Integer), Integer, %) -> %          setsubMatrix! : (%, Integer, Integer, %) -> %
 size? : (%, NonNegativeInteger) -> Boolean             smaller? : (%, %) -> Boolean if R has COMPAR
 square? : % -> Boolean                                 squareTop : % -> %
 string : % -> String                                   swapColumns! : (%, Integer, Integer) -> %
 swapRows! : (%, Integer, Integer) -> %                 symmetric? : % -> Boolean
 transpose : % -> %                                     transpose : Row -> %
 vertConcat : (%, %) -> %                               vertConcat : List(%) -> %
 vertSplit : (%, PositiveInteger) -> List(%)            zero? : % -> Boolean if R has ABELMON
 ?~=? : (%, %) -> Boolean
 ?^? : (%, NonNegativeInteger) -> % if R has MONOID and R has SRNG
 any? : ((R -> Boolean), %) -> Boolean if % has ATFINAG
 blockSplit : (%, PositiveInteger, PositiveInteger) -> List(List(%))
 blockSplit : (%, List(NonNegativeInteger), List(NonNegativeInteger)) -> List(List(%))
 count : ((R -> Boolean), %) -> NonNegativeInteger if % has ATFINAG
 count : (R, %) -> NonNegativeInteger if R has BASTYPE and % has ATFINAG
 elt : (%, Segment(Integer), Segment(Integer)) -> %
 elt : (%, Segment(Integer), List(Segment(Integer))) -> %
 elt : (%, List(Segment(Integer)), Segment(Integer)) -> %
 elt : (%, List(Segment(Integer)), List(Segment(Integer))) -> %
 eval : (%, List(Equation(R))) -> % if R has EVALAB(R) and R has SETCAT
 eval : (%, Equation(R)) -> % if R has EVALAB(R) and R has SETCAT
 eval : (%, R, R) -> % if R has EVALAB(R) and R has SETCAT
 eval : (%, List(R), List(R)) -> % if R has EVALAB(R) and R has SETCAT
 every? : ((R -> Boolean), %) -> Boolean if % has ATFINAG
 ?exquo? : (%, R) -> Union(%,"failed") if R has INTDOM
 hashUpdate! : (HashState, %) -> HashState if R has HASHABL
 horizSplit : (%, List(NonNegativeInteger)) -> List(%)
 inverse : % -> Union(%,"failed") if R has FIELD
 kronecker_prod1 : (%, Integer, List(List(NonNegativeInteger)), List(%), NonNegativeInteger, NonNegativeInteger, Union(R,one)) -> Void
 matrix : (NonNegativeInteger, NonNegativeInteger, ((Integer, Integer) -> R)) -> %
 max : (((R, R) -> Boolean), %) -> R if % has ATFINAG
 max : % -> R if R has ORDSET and % has ATFINAG
 member? : (R, %) -> Boolean if R has BASTYPE and % has ATFINAG
 min : % -> R if R has ORDSET and % has ATFINAG
 new : (NonNegativeInteger, NonNegativeInteger, R) -> %
 nullity : % -> NonNegativeInteger if R has INTDOM
 positivePower : (%, Integer) -> % if R has SRNG
 qnew : (NonNegativeInteger, NonNegativeInteger) -> %
 setelt! : (%, Integer, List(Segment(Integer)), %) -> %
 setelt! : (%, List(Segment(Integer)), Integer, %) -> %
 setelt! : (%, List(Integer), List(Integer), %) -> %
 setelt! : (%, Segment(Integer), Segment(Integer), %) -> %
 setelt! : (%, List(Integer), Segment(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Segment(Integer)), %) -> %
 setelt! : (%, List(Segment(Integer)), Segment(Integer), %) -> %
 setelt! : (%, List(Segment(Integer)), List(Segment(Integer)), %) -> %
 subMatrix : (%, Integer, Integer, Integer, Integer) -> %
 vertSplit : (%, List(NonNegativeInteger)) -> List(%)
 zero : (NonNegativeInteger, NonNegativeInteger) -> %
```

## Operations added
---
[Back to Index](../index.md)
