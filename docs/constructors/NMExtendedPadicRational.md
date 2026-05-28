# NMExtendedPadicRational

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L239)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This is a domain for Qp.

**NMExtendedPadicRational(p: Integer,deg: Integer,prec: Integer) is a domain constructor**  
**Abbreviation for NMExtendedPadicRational is NXPADICR**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, NMInteger) -> %                              ?*? : (NMInteger, %) -> %
 ?*? : (NMFraction(NMInteger), %) -> %                  ?*? : (%, NMFraction(NMInteger)) -> %
 ?*? : (%, NMExtendedPadicInteger(p,prec)) -> %         ?*? : (NMExtendedPadicInteger(p,prec), %) -> %
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
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              coerce : NMInteger -> %
 coerce : NMExtendedPadicInteger(p,prec) -> %           coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> String                                  denom : % -> NMExtendedPadicInteger(p,prec)
 denominator : % -> %                                   equal? : (%, %) -> Boolean
 euclideanSize : % -> NonNegativeInteger                exact? : % -> Boolean
 exactDivide : (%, %) -> %                              exp : % -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 frobenius : (%, Integer) -> %                          gcd : (%, %) -> %
 gcd : List(%) -> %                                     inv : % -> %
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
 jlref : String -> %                                    jnpadic : NMInteger -> %
 jnpadic : Integer -> %                                 latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     liftZ : % -> NMInteger
 log : % -> %                                           missing? : % -> Boolean
 modulus : () -> Integer                                mutable? : % -> Boolean
 nothing? : % -> Boolean                                nthRoot : (%, Integer) -> %
 numer : % -> NMExtendedPadicInteger(p,prec)            numerator : % -> %
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               precision : % -> Integer
 prime : % -> Integer                                   prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    retract : % -> NMExtendedPadicInteger(p,prec)
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sizeLess? : (%, %) -> Boolean                          sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 teichmuller : % -> %                                   unit? : % -> Boolean
 unitCanonical : % -> %                                 valuation : % -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ
 ?*? : (%, Integer) -> % if NMExtendedPadicInteger(p,prec) has LINEXP(INT)
 ?/? : (NMExtendedPadicInteger(p,prec), NMExtendedPadicInteger(p,prec)) -> %
 ?<? : (%, %) -> Boolean if NMExtendedPadicInteger(p,prec) has ORDSET
 ?<=? : (%, %) -> Boolean if NMExtendedPadicInteger(p,prec) has ORDSET
 ?>? : (%, %) -> Boolean if NMExtendedPadicInteger(p,prec) has ORDSET
 ?>=? : (%, %) -> Boolean if NMExtendedPadicInteger(p,prec) has ORDSET
 D : % -> % if NMExtendedPadicInteger(p,prec) has DIFRING
 D : (%, NonNegativeInteger) -> % if NMExtendedPadicInteger(p,prec) has DIFRING
 D : (%, Symbol) -> % if NMExtendedPadicInteger(p,prec) has PDRING(SYMBOL)
 D : (%, List(Symbol)) -> % if NMExtendedPadicInteger(p,prec) has PDRING(SYMBOL)
 D : (%, Symbol, NonNegativeInteger) -> % if NMExtendedPadicInteger(p,prec) has PDRING(SYMBOL)
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMExtendedPadicInteger(p,prec) has PDRING(SYMBOL)
 D : (%, (NMExtendedPadicInteger(p,prec) -> NMExtendedPadicInteger(p,prec)), NonNegativeInteger) -> %
 D : (%, (NMExtendedPadicInteger(p,prec) -> NMExtendedPadicInteger(p,prec))) -> %
 abs : % -> % if NMExtendedPadicInteger(p,prec) has OINTDOM
 ceiling : % -> NMExtendedPadicInteger(p,prec) if NMExtendedPadicInteger(p,prec) has INS
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and NMExtendedPadicInteger(p,prec) has PFECAT
 coerce : Symbol -> % if NMExtendedPadicInteger(p,prec) has RETRACT(SYMBOL)
 coerce : Fraction(Integer) -> % if % has CHARZ or NMExtendedPadicInteger(p,prec) has RETRACT(INT)
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and NMExtendedPadicInteger(p,prec) has PFECAT
 convert : % -> Pattern(Integer) if NMExtendedPadicInteger(p,prec) has KONVERT(PATTERN(INT))
 convert : % -> Pattern(Float) if NMExtendedPadicInteger(p,prec) has KONVERT(PATTERN(FLOAT))
 convert : % -> InputForm if NMExtendedPadicInteger(p,prec) has KONVERT(INFORM)
 convert : % -> Float if NMExtendedPadicInteger(p,prec) has REAL
 convert : % -> DoubleFloat if NMExtendedPadicInteger(p,prec) has REAL
 differentiate : % -> % if NMExtendedPadicInteger(p,prec) has DIFRING
 differentiate : (%, NonNegativeInteger) -> % if NMExtendedPadicInteger(p,prec) has DIFRING
 differentiate : (%, Symbol) -> % if NMExtendedPadicInteger(p,prec) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if NMExtendedPadicInteger(p,prec) has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if NMExtendedPadicInteger(p,prec) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMExtendedPadicInteger(p,prec) has PDRING(SYMBOL)
 differentiate : (%, (NMExtendedPadicInteger(p,prec) -> NMExtendedPadicInteger(p,prec)), NonNegativeInteger) -> %
 differentiate : (%, (NMExtendedPadicInteger(p,prec) -> NMExtendedPadicInteger(p,prec))) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 elt : (%, NMExtendedPadicInteger(p,prec)) -> % if NMExtendedPadicInteger(p,prec) has ELTAB(NXPADICZ(p,prec),NXPADICZ(p,prec))
 eval : (%, List(NMExtendedPadicInteger(p,prec)), List(NMExtendedPadicInteger(p,prec))) -> % if NMExtendedPadicInteger(p,prec) has EVALAB(NXPADICZ(p,prec))
 eval : (%, NMExtendedPadicInteger(p,prec), NMExtendedPadicInteger(p,prec)) -> % if NMExtendedPadicInteger(p,prec) has EVALAB(NXPADICZ(p,prec))
 eval : (%, Equation(NMExtendedPadicInteger(p,prec))) -> % if NMExtendedPadicInteger(p,prec) has EVALAB(NXPADICZ(p,prec))
 eval : (%, List(Equation(NMExtendedPadicInteger(p,prec)))) -> % if NMExtendedPadicInteger(p,prec) has EVALAB(NXPADICZ(p,prec))
 eval : (%, List(Symbol), List(NMExtendedPadicInteger(p,prec))) -> % if NMExtendedPadicInteger(p,prec) has IEVALAB(SYMBOL,NXPADICZ(p,prec))
 eval : (%, Symbol, NMExtendedPadicInteger(p,prec)) -> % if NMExtendedPadicInteger(p,prec) has IEVALAB(SYMBOL,NXPADICZ(p,prec))
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadicInteger(p,prec) has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadicInteger(p,prec) has PFECAT
 floor : % -> NMExtendedPadicInteger(p,prec) if NMExtendedPadicInteger(p,prec) has INS
 fractionPart : % -> % if NMExtendedPadicInteger(p,prec) has EUCDOM
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 init : () -> % if NMExtendedPadicInteger(p,prec) has STEP
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 map : ((NMExtendedPadicInteger(p,prec) -> NMExtendedPadicInteger(p,prec)), %) -> %
 max : (%, %) -> % if NMExtendedPadicInteger(p,prec) has ORDSET
 min : (%, %) -> % if NMExtendedPadicInteger(p,prec) has ORDSET
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 negative? : % -> Boolean if NMExtendedPadicInteger(p,prec) has OINTDOM
 nextItem : % -> Union(%,"failed") if NMExtendedPadicInteger(p,prec) has STEP
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if NMExtendedPadicInteger(p,prec) has PATMAB(INT)
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if NMExtendedPadicInteger(p,prec) has PATMAB(FLOAT)
 positive? : % -> Boolean if NMExtendedPadicInteger(p,prec) has OINTDOM
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reducedSystem : Matrix(%) -> Matrix(Integer) if NMExtendedPadicInteger(p,prec) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if NMExtendedPadicInteger(p,prec) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(NMExtendedPadicInteger(p,prec)),vec: Vector(NMExtendedPadicInteger(p,prec)))
 reducedSystem : Matrix(%) -> Matrix(NMExtendedPadicInteger(p,prec))
 retract : % -> Symbol if NMExtendedPadicInteger(p,prec) has RETRACT(SYMBOL)
 retract : % -> Fraction(Integer) if NMExtendedPadicInteger(p,prec) has RETRACT(INT)
 retract : % -> Integer if NMExtendedPadicInteger(p,prec) has RETRACT(INT)
 retractIfCan : % -> Union(NMExtendedPadicInteger(p,prec),"failed")
 retractIfCan : % -> Union(Symbol,"failed") if NMExtendedPadicInteger(p,prec) has RETRACT(SYMBOL)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if NMExtendedPadicInteger(p,prec) has RETRACT(INT)
 retractIfCan : % -> Union(Integer,"failed") if NMExtendedPadicInteger(p,prec) has RETRACT(INT)
 sign : % -> Integer if NMExtendedPadicInteger(p,prec) has OINTDOM
 smaller? : (%, %) -> Boolean if NMExtendedPadicInteger(p,prec) has COMPAR
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if NMExtendedPadicInteger(p,prec) has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadicInteger(p,prec) has PFECAT
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
 wholePart : % -> NMExtendedPadicInteger(p,prec) if NMExtendedPadicInteger(p,prec) has EUCDOM
```

## Operations added

### `O` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L304)\]

O() returns the default Big-oh from domain parameters.

- **Signature**: `()->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L298)\]

coerce(x) returns x as the q-adic completion of the Nemo Integer.

- **Signature**: `(NMInteger)->%`

### `frobenius` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L295)\]

frobenius(x,i) returns the image of the i-th power of Frobenius of x.

- **Signature**: `(%,Integer)->%`

### `jnpadic` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L300)\]

jnpadic(x) returns x as the q-adic completion of the Nemo Integer.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `liftZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L290)\]

liftZ(x) lifts x to a Nemo Integer if possible.

- **Signature**: `(%)->NMInteger`

### `modulus` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L311)\]

modulus() returns the value of p.

- **Signature**: `()->Integer`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L309)\]

precision(x) returns the precision used for x.

- **Signature**: `(%)->Integer`

### `prime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L306)\]

prime(x) returns the modulus used for x. Convenience function.

- **Signature**: `(%)->Integer`

### `teichmuller` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L292)\]

teichmuller(x) computes the Teichmuller lift of x. The valuation of x must be non negative.

- **Signature**: `(%)->%`

### `valuation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L288)\]

valuation(x) is the valuation of x.

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
