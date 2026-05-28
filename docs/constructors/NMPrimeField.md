# NMPrimeField

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L632)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This domain allows the manipulation of Nemo prime field elements using the Nemo Julia package (FLINT based).

**NMPrimeField(p: PositiveInteger) is a domain constructor**  
**Abbreviation for NMPrimeField is NPF**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, Integer) -> %                                ?*? : (%, NMInteger) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 ?/? : (%, %) -> %                                      0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 D : (%, NonNegativeInteger) -> %                       D : % -> %
 Frobenius : % -> % if % has FINITE                     GF : NMInteger -> %
 GF : Integer -> %                                      ?^? : (%, Integer) -> %
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 algebraic? : % -> Boolean                              annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            basis : () -> Vector(%)
 characteristic : () -> NonNegativeInteger              charthRoot : % -> %
 coerce : NMInteger -> %                                coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> Integer                                 convert : % -> Vector(%)
 convert : Vector(%) -> %                               convert : % -> InputForm
 convert : % -> String                                  coordinates : % -> Vector(%)
 coordinates : Vector(%) -> Matrix(%)                   coordinates : (%, Vector(%)) -> Vector(%)
 createNormalElement : () -> % if % has FINITE          createPrimitiveElement : () -> %
 degree : % -> OnePointCompletion(PositiveInteger)      degree : % -> PositiveInteger
 differentiate : (%, NonNegativeInteger) -> %           differentiate : % -> %
 discreteLog : % -> NonNegativeInteger                  discriminant : () -> %
 discriminant : Vector(%) -> %                          enumerate : () -> List(%)
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  extensionDegree : () -> PositiveInteger
 factor : % -> Factored(%)                              gcd : (%, %) -> %
 gcd : List(%) -> %                                     generator : () -> % if % has FINITE
 generator? : % -> Boolean                              hash : % -> SingleInteger
 hashUpdate! : (HashState, %) -> HashState              inGroundField? : % -> Boolean
 index : PositiveInteger -> %                           init : () -> %
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
 jnpf : NMInteger -> %                                  jnpf : Integer -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 lookup : % -> PositiveInteger                          missing? : % -> Boolean
 mutable? : % -> Boolean                                nextItem : % -> Union(%,"failed")
 norm : % -> %                                          normal? : % -> Boolean if % has FINITE
 normalElement : () -> % if % has FINITE                nothing? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 order : () -> NonNegativeInteger                       order : % -> OnePointCompletion(PositiveInteger)
 order : % -> PositiveInteger                           plenaryPower : (%, PositiveInteger) -> %
 prime? : % -> Boolean                                  primeFrobenius : % -> %
 primeFrobenius : (%, NonNegativeInteger) -> %          primitive? : % -> Boolean
 primitiveElement : () -> %                             quadraticNonResidue : () -> %
 ?quo? : (%, %) -> %                                    random : () -> %
 rank : () -> PositiveInteger                           recip : % -> Union(%,"failed")
 regularRepresentation : % -> Matrix(%)                 ?rem? : (%, %) -> %
 represents : Vector(%) -> %                            represents : (Vector(%), Vector(%)) -> %
 retract : % -> %                                       retractIfCan : % -> Union(%,"failed")
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 size : () -> NonNegativeInteger                        sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 trace : % -> %                                         traceMatrix : () -> Matrix(%)
 traceMatrix : Vector(%) -> Matrix(%)                   transcendenceDegree : () -> NonNegativeInteger
 transcendent? : % -> Boolean                           unit? : % -> Boolean
 unitCanonical : % -> %                                 zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ
 Frobenius : (%, NonNegativeInteger) -> % if % has FINITE
 basis : PositiveInteger -> Vector(%) if % has FINITE
 characteristicPolynomial : % -> SparseUnivariatePolynomial(%)
 charthRoot : % -> Union(%,"failed") if % has CHARNZ
 coerce : Fraction(Integer) -> % if % has CHARZ
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ
 coordinates : (Vector(%), Vector(%)) -> Matrix(%)
 definingPolynomial : () -> SparseUnivariatePolynomial(%)
 discreteLog : (%, %) -> Union(NonNegativeInteger,"failed")
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 extensionDegree : () -> OnePointCompletion(PositiveInteger)
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 factorsOfCyclicGroupSize : () -> List(Record(factor: Integer,exponent: NonNegativeInteger))
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 linearAssociatedExp : (%, SparseUnivariatePolynomial(%)) -> % if % has FINITE
 linearAssociatedLog : % -> SparseUnivariatePolynomial(%) if % has FINITE
 linearAssociatedLog : (%, %) -> Union(SparseUnivariatePolynomial(%),"failed") if % has FINITE
 linearAssociatedOrder : % -> SparseUnivariatePolynomial(%) if % has FINITE
 minimalPolynomial : % -> SparseUnivariatePolynomial(%) if % has FIELD
 minimalPolynomial : (%, PositiveInteger) -> SparseUnivariatePolynomial(%) if % has FINITE
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 norm : (%, PositiveInteger) -> % if % has FINITE
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 regularRepresentation : (%, Vector(%)) -> Matrix(%)
 representationType : () -> Union("prime",polynomial,normal,cyclic)
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed")
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 tableForDiscreteLogarithm : Integer -> Table(PositiveInteger,NonNegativeInteger)
 trace : (%, PositiveInteger) -> % if % has FINITE
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `GF` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L673)\]

GF(i) returns i as a NMPrimeField element. Convenience function.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L665)\]

coerce(i) coerces i as a NMPrimeField element.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `generator?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L651)\]

generator?(x) checks whether or not x is a generator of the NMPrimeField.

- **Signature**: `(%)->Boolean`

### `jnpf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L669)\]

jnpf(i) returns i as a NMPrimeField element.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `order` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L653)\]

order() returns the order of the NMPrimeField.

- **Signature**: `()->NonNegativeInteger`

### `quadraticNonResidue` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L662)\]

quadraticNonResidue() computes the smallest non negative integer, which represents a quadratic non residue.

- **Signature**: `()->%`

### `sqrt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L659)\]

sqrt(x) returns square root of x. Throws a Julia error if there is no square root and returns 0.

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
