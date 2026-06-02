# NMExtendedPadic

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L1)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This domain implements the field of p-adic numbers $Q_p$ using the Nemo Julia package.

**NMExtendedPadic(p: Integer,prec: Integer) is a domain constructor**  
**Abbreviation for NMExtendedPadic is NXPADIC**  
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
 ?/? : (%, %) -> %                                      0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 O : () -> %                                            ?^? : (%, %) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, Integer) -> %
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 approximate : (%, Integer) -> NMInteger                associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 coerce : NMInteger -> %                                coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> String                                  equal? : (%, %) -> Boolean
 euclideanSize : % -> NonNegativeInteger                exact? : % -> Boolean
 exactDivide : (%, %) -> %                              exp : % -> %
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
 jnpadic : NMInteger -> %                               jnpadic : Integer -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 liftQ : % -> NMFraction(NMInteger)                     liftZ : % -> NMInteger
 log : % -> %                                           missing? : % -> Boolean
 moduloP : % -> NMInteger                               modulus : () -> Integer
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> Integer
 plenaryPower : (%, PositiveInteger) -> %               precision : % -> Integer
 prime : % -> Integer                                   prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    quotientByP : % -> %
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sizeLess? : (%, %) -> Boolean                          sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 teichmuller : % -> %                                   unit? : % -> Boolean
 unitCanonical : % -> %                                 valuation : % -> JLObjInt64
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
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

### `O` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L66)\]

O() returns the default Big-oh from domain parameters.

- **Signature**: `()->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L64)\]

coerce(x) returns x as the p-adic completion of the Nemo Integer.

- **Signature**: `(NMInteger)->%`

### `jnpadic` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L68)\]

jnpadic(x) returns x as the p-adic completion of the Nemo Integer.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `liftQ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L48)\]

liftQ(x) lift x to a Nemo Fraction Nemo Integer.

- **Signature**: `(%)->NMFraction(NMInteger)`

### `liftZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L46)\]

liftZ(x) lift x to a Nemo Integer.

- **Signature**: `(%)->NMInteger`

### `moduloP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L50)\]

moduloP(x) returns a, where x = a + b p.

- **Signature**: `(%)->NMInteger`

### `modulus` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L57)\]

modulus() returns the value of p.

- **Signature**: `()->Integer`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L62)\]

precision(x) returns the precision used for x.

- **Signature**: `(%)->Integer`

### `prime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L59)\]

prime(x) returns the modulus used for x. Convenience function.

- **Signature**: `(%)->Integer`

### `quotientByP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L52)\]

quotientByP(x) returns b, where x = a + b p.

- **Signature**: `(%)->%`

### `teichmuller` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L54)\]

teichmuller(x) computes the Teichmuller lift of x. The valuation of x must be non negative.

- **Signature**: `(%)->%`

### `valuation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXPADIC.spad#L44)\]

valuation(x) is the valuation of x.

- **Signature**: `(%)->JLObjInt64`
---
[Back to Index](../index.md)
