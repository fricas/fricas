# NMPadicNumberCategory

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L1)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This category provides representations of the p-adic numbers using the Nemo Julia package.

**NMPadicNumberCategory(p: Integer) is a category constructor**  
**Abbreviation for NMPadicNumberCategory is NPADICC**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 0 : () -> %                                            1 : () -> %
 ?=? : (%, %) -> Boolean                                ?^? : (%, %) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, PositiveInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           approximate : (%, Integer) -> Integer
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              coerce : % -> %
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
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 log : % -> %                                           missing? : % -> Boolean
 moduloP : % -> Integer                                 modulus : () -> Integer
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> NonNegativeInteger
 plenaryPower : (%, PositiveInteger) -> %               ?quo? : (%, %) -> %
 quotientByP : % -> %                                   recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sizeLess? : (%, %) -> Boolean
 sqrt : % -> %                                          string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            unit? : % -> Boolean
 unitCanonical : % -> %                                 zero? : % -> Boolean
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

### `approximate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L35)\]

approximate(x, n) returns an integer y such that y = x (mod p^n) when n is positive, and 0 otherwise.

- **Signature**: `(%,Integer)->Integer`

### `complete` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L27)\]

complete(x) forces the computation of all digits.

- **Signature**: `(%)->%`

### `extend` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L25)\]

extend(x, n) forces the computation of digits up to order n.

- **Signature**: `(%,Integer)->%`

### `moduloP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L31)\]

modulo(x) returns a, where x = a + b p.

- **Signature**: `(%)->Integer`

### `modulus` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L29)\]

modulus() returns the value of p.

- **Signature**: `()->Integer`

### `order` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L22)\]

order(x) returns the exponent of the highest power of p dividing x.

- **Signature**: `(%)->NonNegativeInteger`

### `quotientByP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L33)\]

quotientByP(x) returns b, where x = a + b p.

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
