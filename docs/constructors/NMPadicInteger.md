# NMPadicInteger

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L223)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

Implementation of Zp: p-adic numbers are represented as sum(i = 0.., a[i] * p^i), where the a[i] lie in 0, 1,..., (p - 1) using the Nemo Julia package.

**NMPadicInteger(p: Integer) is a domain constructor**  
**Abbreviation for NMPadicInteger is NPADICZ**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, NMInteger) -> %                              ?*? : (NMInteger, %) -> %
 ?*? : (NMFraction(NMInteger), %) -> %                  ?*? : (%, NMFraction(NMInteger)) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (NMFraction(NMInteger), %) -> %
 ?+? : (%, NMFraction(NMInteger)) -> %                  ?+? : (%, NMInteger) -> %
 ?+? : (NMInteger, %) -> %                              ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 0 : () -> %                                            1 : () -> %
 ?=? : (%, %) -> Boolean                                O : () -> %
 ?^? : (%, %) -> %                                      ?^? : (%, Fraction(Integer)) -> %
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 approximate : (%, Integer) -> Integer                  associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 coerce : NMInteger -> %                                coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 complete : % -> %                                      convert : % -> String
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 exp : % -> %                                           ?exquo? : (%, %) -> Union(%,"failed")
 extend : (%, Integer) -> %                             gcd : (%, %) -> %
 gcd : List(%) -> %                                     jlAbout : % -> Void
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
 jnpadic : NMInteger -> %                               jnpadic : Integer -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 liftQ : % -> NMFraction(NMInteger)                     liftZ : % -> NMInteger
 log : % -> %                                           missing? : % -> Boolean
 moduloP : % -> Integer                                 modulus : () -> Integer
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> NonNegativeInteger
 plenaryPower : (%, PositiveInteger) -> %               precision : % -> Integer
 prime : % -> Integer                                   ?quo? : (%, %) -> %
 quotientByP : % -> %                                   recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sizeLess? : (%, %) -> Boolean
 sqrt : % -> %                                          string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            teichmuller : % -> %
 unit? : % -> Boolean                                   unitCanonical : % -> %
 valuation : % -> %                                     zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
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

### `O` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L304)\]

- **Signature**: `()->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L298)\]

- **Signature**: `(NMInteger)->%`

### `jnpadic` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L300)\]

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `liftQ`

- **Signature**: `(%)->NMFraction(NMInteger)`

### `liftZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L290)\]

- **Signature**: `(%)->NMInteger`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L309)\]

- **Signature**: `(%)->Integer`

### `prime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L306)\]

- **Signature**: `(%)->Integer`

### `teichmuller` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L292)\]

- **Signature**: `(%)->%`

### `valuation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L288)\]

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
