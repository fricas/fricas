# NMField

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L56)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

Parent category of Nemo commutative field domains.

**NMField is a category constructor**  
**Abbreviation for NMField is NFIELD**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 ?/? : (%, %) -> %                                      0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 ?^? : (%, Integer) -> %                                ?^? : (%, PositiveInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 coerce : % -> %                                        coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> String
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 inv : % -> %                                           jlAbout : % -> Void
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %) -> JLObject
 jlApply : (String, %) -> JLObject                      jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlNMRing : () -> String                                jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nothing? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          plenaryPower : (%, PositiveInteger) -> %
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sizeLess? : (%, %) -> Boolean                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            unit? : % -> Boolean
 unitCanonical : % -> %                                 zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ
 coerce : Fraction(Integer) -> % if % has CHARZ
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added
---
[Back to Index](../index.md)
