# WSRational

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L388)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic rational numbers using the MathLink Julia package.

**WSRational is a domain constructor.**  
**Abbreviation for WSRational is WSRAT**  
**This constructor is exposed in this frame.**  
**107 names for 153 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, WSInteger) -> %                              ?*? : (Fraction(Integer), %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (WSInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 -? : % -> %                                            ?-? : (%, %) -> %
 ?/? : (%, %) -> %                                      ?/? : (WSInteger, WSInteger) -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               D : % -> %
 D : (%, (WSInteger -> WSInteger)) -> %                 D : (%, NonNegativeInteger) -> %
 ?^? : (%, Integer) -> %                                ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        abs : % -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 ceiling : % -> WSInteger                               characteristic : () -> NonNegativeInteger
 coerce : % -> %                                        coerce : Fraction(Integer) -> %
 coerce : Integer -> %                                  coerce : WSInteger -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 coerce : % -> WSExpression                             commutator : (%, %) -> %
 convert : % -> DoubleFloat                             convert : % -> Float
 convert : % -> Fraction(Integer)                       convert : % -> InputForm
 convert : % -> Pattern(Integer)                        convert : % -> String
 denom : % -> WSInteger                                 denominator : % -> %
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 euclideanSize : % -> NonNegativeInteger                ?exquo? : (%, %) -> Union(%,"failed")
 factor : % -> Factored(%)                              floor : % -> WSInteger
 fractionPart : % -> %                                  gcd : (%, %) -> %
 gcd : List(%) -> %                                     init : () -> %
 inv : % -> %                                           jWSInterpret : String -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : (String, String, String) -> %
 jWSRational : Fraction(Integer) -> %                   jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlEval : % -> %
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlHead : % -> WSSymbol                                 jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlSymbolic : % -> String
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 map : ((WSInteger -> WSInteger), %) -> %               max : (%, %) -> %
 min : (%, %) -> %                                      missing? : % -> Boolean
 mutable? : % -> Boolean                                negative? : % -> Boolean
 nextItem : % -> Union(%,"failed")                      nothing? : % -> Boolean
 numer : % -> WSInteger                                 numerator : % -> %
 numeric : % -> WSExpression                            numeric : (%, PositiveInteger) -> WSExpression
 numeric? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          plenaryPower : (%, PositiveInteger) -> %
 positive? : % -> Boolean                               prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    recip : % -> Union(%,"failed")
 reducedSystem : Matrix(%) -> Matrix(WSInteger)         ?rem? : (%, %) -> %
 retract : % -> Fraction(Integer)                       retract : % -> Integer
 retract : % -> WSInteger                               retractIfCan : % -> Union(Integer,"failed")
 retractIfCan : % -> Union(WSInteger,"failed")          rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sign : % -> Integer
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 toString : % -> String                                 unit? : % -> Boolean
 unitCanonical : % -> %                                 wholePart : % -> WSInteger
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 D : (%, (WSInteger -> WSInteger), NonNegativeInteger) -> %
 differentiate : (%, (WSInteger -> WSInteger)) -> %
 differentiate : (%, (WSInteger -> WSInteger), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(WSInteger),vec: Vector(WSInteger))
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L401)\]

coerce(z) coerces z. Convenience function.

- **Signature**: `(Integer)->%`

### `convert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L407)\]

convert(q) returns q as a Fraction(Integer).

- **Signature**: `(%)->Fraction(Integer)`

### `jWSRational` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L409)\]

jWSRational(q) constructs q as a WSRational.

- **Signature**: `(Fraction(Integer))->%`
---
[Back to Index](../index.md)
