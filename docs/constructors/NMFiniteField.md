# NMFiniteField

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L792)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This domain allows the manipulation of Nemo Galois field of p^n elements using the Nemo Julia package (FLINT based).

**NMFiniteField(p: PositiveInteger,n: PositiveInteger) is a domain constructor**  
**Abbreviation for NMFiniteField is NFF**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, Integer) -> %                                ?*? : (%, NMInteger) -> %
 ?*? : (%, NMPrimeField(p)) -> %                        ?*? : (NMPrimeField(p), %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 ?/? : (%, NMPrimeField(p)) -> %                        ?/? : (%, %) -> %
 0 : () -> %                                            1 : () -> %
 ?=? : (%, %) -> Boolean                                D : (%, NonNegativeInteger) -> %
 D : % -> %                                             ?^? : (%, Integer) -> %
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 absolute? : () -> Boolean                              absoluteNorm : % -> NMPrimeField(p)
 absoluteTrace : % -> NMPrimeField(p)                   algebraic? : % -> Boolean
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 basis : () -> Vector(%)                                characteristic : () -> NonNegativeInteger
 charthRoot : % -> %                                    coerce : NMInteger -> %
 coerce : NMPrimeField(p) -> %                          coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> Integer                                 convert : % -> Vector(NMPrimeField(p))
 convert : Vector(NMPrimeField(p)) -> %                 convert : % -> InputForm
 convert : % -> String                                  coordinates : % -> Vector(NMPrimeField(p))
 coordinates : Vector(%) -> Matrix(NMPrimeField(p))     createPrimitiveElement : () -> %
 degree : % -> OnePointCompletion(PositiveInteger)      degree : % -> PositiveInteger
 differentiate : (%, NonNegativeInteger) -> %           differentiate : % -> %
 discreteLog : % -> NonNegativeInteger                  discriminant : () -> NMPrimeField(p)
 discriminant : Vector(%) -> NMPrimeField(p)            enumerate : () -> List(%)
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  extensionDegree : () -> PositiveInteger
 factor : % -> Factored(%)                              gcd : (%, %) -> %
 gcd : List(%) -> %                                     generator? : % -> Boolean
 hash : % -> SingleInteger                              hashUpdate! : (HashState, %) -> HashState
 inGroundField? : % -> Boolean                          index : PositiveInteger -> %
 init : () -> %                                         inv : % -> %
 jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlId : % -> JLInt64                                    jlNMRing : () -> String
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jnff : NMInteger -> %
 jnff : Integer -> %                                    latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     lookup : % -> PositiveInteger
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nextItem : % -> Union(%,"failed")                      norm : % -> NMPrimeField(p)
 nothing? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : () -> NonNegativeInteger
 order : % -> OnePointCompletion(PositiveInteger)       order : % -> PositiveInteger
 plenaryPower : (%, PositiveInteger) -> %               prime? : % -> Boolean
 primeFrobenius : % -> %                                primeFrobenius : (%, NonNegativeInteger) -> %
 primitive? : % -> Boolean                              primitiveElement : () -> %
 ?quo? : (%, %) -> %                                    random : () -> %
 rank : () -> PositiveInteger                           recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    represents : Vector(NMPrimeField(p)) -> %
 retract : % -> NMPrimeField(p)                         rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       size : () -> NonNegativeInteger
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            trace : % -> NMPrimeField(p)
 traceMatrix : () -> Matrix(NMPrimeField(p))            traceMatrix : Vector(%) -> Matrix(NMPrimeField(p))
 transcendenceDegree : () -> NonNegativeInteger         transcendent? : % -> Boolean
 unit? : % -> Boolean                                   unitCanonical : % -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ
 Frobenius : % -> % if NMPrimeField(p) has FINITE
 Frobenius : (%, NonNegativeInteger) -> % if NMPrimeField(p) has FINITE
 basis : PositiveInteger -> Vector(%) if NMPrimeField(p) has FINITE
 characteristicPolynomial : % -> SparseUnivariatePolynomial(NMPrimeField(p))
 charthRoot : % -> Union(%,"failed") if % has CHARNZ
 coerce : Fraction(Integer) -> % if % has CHARZ
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ
 coordinates : (Vector(%), Vector(%)) -> Matrix(NMPrimeField(p))
 coordinates : (%, Vector(%)) -> Vector(NMPrimeField(p))
 createNormalElement : () -> % if NMPrimeField(p) has FINITE
 definingPolynomial : () -> SparseUnivariatePolynomial(NMPrimeField(p))
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
 generator : () -> % if NMPrimeField(p) has FINITE
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 linearAssociatedExp : (%, SparseUnivariatePolynomial(NMPrimeField(p))) -> % if NMPrimeField(p) has FINITE
 linearAssociatedLog : % -> SparseUnivariatePolynomial(NMPrimeField(p)) if NMPrimeField(p) has FINITE
 linearAssociatedLog : (%, %) -> Union(SparseUnivariatePolynomial(NMPrimeField(p)),"failed") if NMPrimeField(p) has FINITE
 linearAssociatedOrder : % -> SparseUnivariatePolynomial(NMPrimeField(p)) if NMPrimeField(p) has FINITE
 minimalPolynomial : % -> SparseUnivariatePolynomial(NMPrimeField(p)) if NMPrimeField(p) has FIELD
 minimalPolynomial : (%, PositiveInteger) -> SparseUnivariatePolynomial(%) if NMPrimeField(p) has FINITE
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 norm : (%, PositiveInteger) -> % if NMPrimeField(p) has FINITE
 normal? : % -> Boolean if NMPrimeField(p) has FINITE
 normalElement : () -> % if NMPrimeField(p) has FINITE
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 regularRepresentation : % -> Matrix(NMPrimeField(p))
 regularRepresentation : (%, Vector(%)) -> Matrix(NMPrimeField(p))
 representationType : () -> Union("prime",polynomial,normal,cyclic)
 represents : (Vector(NMPrimeField(p)), Vector(%)) -> %
 retractIfCan : % -> Union(NMPrimeField(p),"failed")
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed")
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 tableForDiscreteLogarithm : Integer -> Table(PositiveInteger,NonNegativeInteger)
 trace : (%, PositiveInteger) -> % if NMPrimeField(p) has FINITE
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `absolute?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L813)\]

absolute?(x) checks whether or not x is the base field is a prime field.

- **Signature**: `()->Boolean`

### `absoluteNorm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L822)\]

absoluteNorm(x) returns the absolute norm of x.

- **Signature**: `(%)->NMPrimeField(p)`

### `absoluteTrace` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L818)\]

absoluteTrace(x) returns the absolute trace of x.

- **Signature**: `(%)->NMPrimeField(p)`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L835)\]

coerce(i) coerces i.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `generator?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L810)\]

generator?(x) checks whether or not x is a generator of the finite field.

- **Signature**: `(%)->Boolean`

### `jnff` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L839)\]

jnff(i) returns i as a NMFiniteField element.

- **Signature**: `(Integer)->%`

jnff(i) returns i, a NMInteger, as a NMFiniteField element.

- **Signature**: `(NMInteger)->%`

### `norm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L820)\]

norm(x) returns the norm of x.

- **Signature**: `(%)->NMPrimeField(p)`

### `order` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L824)\]

order() returns the order of the finite field.

- **Signature**: `()->NonNegativeInteger`

### `rank` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L826)\]

rank() returns the degree of the extension.

- **Signature**: `()->PositiveInteger`

### `sqrt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L832)\]

sqrt(x) returns a square root of x. Throws a Julia error if there is no square root and returns 0.

- **Signature**: `(%)->%`

### `trace` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L816)\]

trace(x) returns the trace of x.

- **Signature**: `(%)->NMPrimeField(p)`
---
[Back to Index](../index.md)
