# NMExtendedQadic

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L1)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This is a domain for Qp.

**NMExtendedQadic(p: NMInteger,deg: Integer,prec: Integer) is a domain constructor**  
**Abbreviation for NMExtendedQadic is NXQADIC**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, NMInteger) -> %                              ?*? : (NMInteger, %) -> %
 ?*? : (NMFraction(NMInteger), %) -> %                  ?*? : (%, NMFraction(NMInteger)) -> %
 ?*? : (%, NMExtendedPadic(p,prec)) -> %                ?*? : (NMExtendedPadic(p,prec), %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (NMFraction(NMInteger), %) -> %
 ?+? : (%, NMFraction(NMInteger)) -> %                  ?+? : (%, NMInteger) -> %
 ?+? : (NMInteger, %) -> %                              ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 ?/? : (%, %) -> %                                      0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 O : () -> %                                            ?^? : (%, Integer) -> %
 ?^? : (%, %) -> %                                      ?^? : (%, Fraction(Integer)) -> %
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 approximate : (%, Integer) -> Integer                  associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 coerce : NMInteger -> %                                coerce : NMExtendedPadic(p,prec) -> %
 coerce : % -> %                                        coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> String
 denom : % -> NMExtendedPadic(p,prec)                   denominator : % -> %
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 exp : % -> %                                           ?exquo? : (%, %) -> Union(%,"failed")
 factor : % -> Factored(%)                              frobenius : (%, Integer) -> %
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
 liftZ : % -> NMInteger                                 log : % -> %
 missing? : % -> Boolean                                moduloP : % -> Integer
 modulus : () -> NMInteger                              modulus : () -> Integer
 mutable? : % -> Boolean                                nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            numer : % -> NMExtendedPadic(p,prec)
 numerator : % -> %                                     one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> NonNegativeInteger
 plenaryPower : (%, PositiveInteger) -> %               precision : % -> Integer
 prime : % -> NMInteger                                 prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    quotientByP : % -> %
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 retract : % -> NMExtendedPadic(p,prec)                 rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sizeLess? : (%, %) -> Boolean
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            teichmuller : % -> %
 unit? : % -> Boolean                                   unitCanonical : % -> %
 valuation : % -> %                                     zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ
 ?*? : (%, Integer) -> % if NMExtendedPadic(p,prec) has LINEXP(INT)
 ?/? : (NMExtendedPadic(p,prec), NMExtendedPadic(p,prec)) -> %
 ?<? : (%, %) -> Boolean if NMExtendedPadic(p,prec) has ORDSET
 ?<=? : (%, %) -> Boolean if NMExtendedPadic(p,prec) has ORDSET
 ?>? : (%, %) -> Boolean if NMExtendedPadic(p,prec) has ORDSET
 ?>=? : (%, %) -> Boolean if NMExtendedPadic(p,prec) has ORDSET
 D : % -> % if NMExtendedPadic(p,prec) has DIFRING
 D : (%, NonNegativeInteger) -> % if NMExtendedPadic(p,prec) has DIFRING
 D : (%, Symbol) -> % if NMExtendedPadic(p,prec) has PDRING(SYMBOL)
 D : (%, List(Symbol)) -> % if NMExtendedPadic(p,prec) has PDRING(SYMBOL)
 D : (%, Symbol, NonNegativeInteger) -> % if NMExtendedPadic(p,prec) has PDRING(SYMBOL)
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMExtendedPadic(p,prec) has PDRING(SYMBOL)
 D : (%, (NMExtendedPadic(p,prec) -> NMExtendedPadic(p,prec)), NonNegativeInteger) -> %
 D : (%, (NMExtendedPadic(p,prec) -> NMExtendedPadic(p,prec))) -> %
 abs : % -> % if NMExtendedPadic(p,prec) has OINTDOM
 ceiling : % -> NMExtendedPadic(p,prec) if NMExtendedPadic(p,prec) has INS
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and NMExtendedPadic(p,prec) has PFECAT
 coerce : Symbol -> % if NMExtendedPadic(p,prec) has RETRACT(SYMBOL)
 coerce : Fraction(Integer) -> % if % has CHARZ or NMExtendedPadic(p,prec) has RETRACT(INT)
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and NMExtendedPadic(p,prec) has PFECAT
 convert : % -> Pattern(Integer) if NMExtendedPadic(p,prec) has KONVERT(PATTERN(INT))
 convert : % -> Pattern(Float) if NMExtendedPadic(p,prec) has KONVERT(PATTERN(FLOAT))
 convert : % -> InputForm if NMExtendedPadic(p,prec) has KONVERT(INFORM)
 convert : % -> Float if NMExtendedPadic(p,prec) has REAL
 convert : % -> DoubleFloat if NMExtendedPadic(p,prec) has REAL
 differentiate : % -> % if NMExtendedPadic(p,prec) has DIFRING
 differentiate : (%, NonNegativeInteger) -> % if NMExtendedPadic(p,prec) has DIFRING
 differentiate : (%, Symbol) -> % if NMExtendedPadic(p,prec) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if NMExtendedPadic(p,prec) has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if NMExtendedPadic(p,prec) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMExtendedPadic(p,prec) has PDRING(SYMBOL)
 differentiate : (%, (NMExtendedPadic(p,prec) -> NMExtendedPadic(p,prec)), NonNegativeInteger) -> %
 differentiate : (%, (NMExtendedPadic(p,prec) -> NMExtendedPadic(p,prec))) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 elt : (%, NMExtendedPadic(p,prec)) -> % if NMExtendedPadic(p,prec) has ELTAB(NXPADIC(p,prec),NXPADIC(p,prec))
 eval : (%, List(NMExtendedPadic(p,prec)), List(NMExtendedPadic(p,prec))) -> % if NMExtendedPadic(p,prec) has EVALAB(NXPADIC(p,prec))
 eval : (%, NMExtendedPadic(p,prec), NMExtendedPadic(p,prec)) -> % if NMExtendedPadic(p,prec) has EVALAB(NXPADIC(p,prec))
 eval : (%, Equation(NMExtendedPadic(p,prec))) -> % if NMExtendedPadic(p,prec) has EVALAB(NXPADIC(p,prec))
 eval : (%, List(Equation(NMExtendedPadic(p,prec)))) -> % if NMExtendedPadic(p,prec) has EVALAB(NXPADIC(p,prec))
 eval : (%, List(Symbol), List(NMExtendedPadic(p,prec))) -> % if NMExtendedPadic(p,prec) has IEVALAB(SYMBOL,NXPADIC(p,prec))
 eval : (%, Symbol, NMExtendedPadic(p,prec)) -> % if NMExtendedPadic(p,prec) has IEVALAB(SYMBOL,NXPADIC(p,prec))
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadic(p,prec) has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadic(p,prec) has PFECAT
 floor : % -> NMExtendedPadic(p,prec) if NMExtendedPadic(p,prec) has INS
 fractionPart : % -> % if NMExtendedPadic(p,prec) has EUCDOM
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 init : () -> % if NMExtendedPadic(p,prec) has STEP
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 map : ((NMExtendedPadic(p,prec) -> NMExtendedPadic(p,prec)), %) -> %
 max : (%, %) -> % if NMExtendedPadic(p,prec) has ORDSET
 min : (%, %) -> % if NMExtendedPadic(p,prec) has ORDSET
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 negative? : % -> Boolean if NMExtendedPadic(p,prec) has OINTDOM
 nextItem : % -> Union(%,"failed") if NMExtendedPadic(p,prec) has STEP
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if NMExtendedPadic(p,prec) has PATMAB(INT)
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if NMExtendedPadic(p,prec) has PATMAB(FLOAT)
 positive? : % -> Boolean if NMExtendedPadic(p,prec) has OINTDOM
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reducedSystem : Matrix(%) -> Matrix(Integer) if NMExtendedPadic(p,prec) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if NMExtendedPadic(p,prec) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(NMExtendedPadic(p,prec)),vec: Vector(NMExtendedPadic(p,prec)))
 reducedSystem : Matrix(%) -> Matrix(NMExtendedPadic(p,prec))
 retract : % -> Symbol if NMExtendedPadic(p,prec) has RETRACT(SYMBOL)
 retract : % -> Fraction(Integer) if NMExtendedPadic(p,prec) has RETRACT(INT)
 retract : % -> Integer if NMExtendedPadic(p,prec) has RETRACT(INT)
 retractIfCan : % -> Union(NMExtendedPadic(p,prec),"failed")
 retractIfCan : % -> Union(Symbol,"failed") if NMExtendedPadic(p,prec) has RETRACT(SYMBOL)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if NMExtendedPadic(p,prec) has RETRACT(INT)
 retractIfCan : % -> Union(Integer,"failed") if NMExtendedPadic(p,prec) has RETRACT(INT)
 sign : % -> Integer if NMExtendedPadic(p,prec) has OINTDOM
 smaller? : (%, %) -> Boolean if NMExtendedPadic(p,prec) has COMPAR
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if NMExtendedPadic(p,prec) has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadic(p,prec) has PFECAT
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
 wholePart : % -> NMExtendedPadic(p,prec) if NMExtendedPadic(p,prec) has EUCDOM
```

## Operations added

### `O` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L63)\]

O() returns the default Big-oh from domain parameters.

- **Signature**: `()->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L57)\]

coerce(x) returns x as the q-adic completion of the Nemo Integer.

- **Signature**: `(NMInteger)->%`

### `frobenius` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L54)\]

frobenius(x,i) returns the image of the i-th power of Frobenius of x.

- **Signature**: `(%,Integer)->%`

### `jnpadic` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L59)\]

jnpadic(x) returns x as the q-adic completion of the Nemo Integer.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `liftZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L49)\]

liftZ(x) lifts x to a Nemo Integer if possible.

- **Signature**: `(%)->NMInteger`

### `modulus` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L70)\]

modulus() returns the value of p.

- **Signature**: `()->NMInteger`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L68)\]

precision(x) returns the precision used for x.

- **Signature**: `(%)->Integer`

### `prime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L65)\]

prime(x) returns the modulus used for x. Convenience function.

- **Signature**: `(%)->NMInteger`

### `teichmuller` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L51)\]

teichmuller(x) computes the Teichmuller lift of x. The valuation of x must be non negative.

- **Signature**: `(%)->%`

### `valuation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADIC.spad#L47)\]

valuation(x) is the valuation of x.

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
