# NMPadicRational

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpadic.spad#L425)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This is a domain for Qp.

**NMPadicRational(p: Integer) is a domain constructor**  
**Abbreviation for NMPadicRational is NPADICR**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, NMInteger) -> %                              ?*? : (NMInteger, %) -> %
 ?*? : (NMFraction(NMInteger), %) -> %                  ?*? : (%, NMFraction(NMInteger)) -> %
 ?*? : (%, NMExtendedPadicInteger(p,64)) -> %           ?*? : (NMExtendedPadicInteger(p,64), %) -> %
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
 coerce : NMExtendedPadicInteger(p,64) -> %             coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 convert : % -> String                                  denom : % -> NMExtendedPadicInteger(p,64)
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
 numer : % -> NMExtendedPadicInteger(p,64)              numerator : % -> %
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               precision : % -> Integer
 prime : % -> Integer                                   prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    retract : % -> NMExtendedPadicInteger(p,64)
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
 ?*? : (%, Integer) -> % if NMExtendedPadicInteger(p,64) has LINEXP(INT)
 ?/? : (NMExtendedPadicInteger(p,64), NMExtendedPadicInteger(p,64)) -> %
 ?<? : (%, %) -> Boolean if NMExtendedPadicInteger(p,64) has ORDSET
 ?<=? : (%, %) -> Boolean if NMExtendedPadicInteger(p,64) has ORDSET
 ?>? : (%, %) -> Boolean if NMExtendedPadicInteger(p,64) has ORDSET
 ?>=? : (%, %) -> Boolean if NMExtendedPadicInteger(p,64) has ORDSET
 D : % -> % if NMExtendedPadicInteger(p,64) has DIFRING
 D : (%, NonNegativeInteger) -> % if NMExtendedPadicInteger(p,64) has DIFRING
 D : (%, Symbol) -> % if NMExtendedPadicInteger(p,64) has PDRING(SYMBOL)
 D : (%, List(Symbol)) -> % if NMExtendedPadicInteger(p,64) has PDRING(SYMBOL)
 D : (%, Symbol, NonNegativeInteger) -> % if NMExtendedPadicInteger(p,64) has PDRING(SYMBOL)
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMExtendedPadicInteger(p,64) has PDRING(SYMBOL)
 D : (%, (NMExtendedPadicInteger(p,64) -> NMExtendedPadicInteger(p,64)), NonNegativeInteger) -> %
 D : (%, (NMExtendedPadicInteger(p,64) -> NMExtendedPadicInteger(p,64))) -> %
 abs : % -> % if NMExtendedPadicInteger(p,64) has OINTDOM
 ceiling : % -> NMExtendedPadicInteger(p,64) if NMExtendedPadicInteger(p,64) has INS
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and NMExtendedPadicInteger(p,64) has PFECAT
 coerce : Symbol -> % if NMExtendedPadicInteger(p,64) has RETRACT(SYMBOL)
 coerce : Fraction(Integer) -> % if % has CHARZ or NMExtendedPadicInteger(p,64) has RETRACT(INT)
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and NMExtendedPadicInteger(p,64) has PFECAT
 convert : % -> Pattern(Integer) if NMExtendedPadicInteger(p,64) has KONVERT(PATTERN(INT))
 convert : % -> Pattern(Float) if NMExtendedPadicInteger(p,64) has KONVERT(PATTERN(FLOAT))
 convert : % -> InputForm if NMExtendedPadicInteger(p,64) has KONVERT(INFORM)
 convert : % -> Float if NMExtendedPadicInteger(p,64) has REAL
 convert : % -> DoubleFloat if NMExtendedPadicInteger(p,64) has REAL
 differentiate : % -> % if NMExtendedPadicInteger(p,64) has DIFRING
 differentiate : (%, NonNegativeInteger) -> % if NMExtendedPadicInteger(p,64) has DIFRING
 differentiate : (%, Symbol) -> % if NMExtendedPadicInteger(p,64) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if NMExtendedPadicInteger(p,64) has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if NMExtendedPadicInteger(p,64) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMExtendedPadicInteger(p,64) has PDRING(SYMBOL)
 differentiate : (%, (NMExtendedPadicInteger(p,64) -> NMExtendedPadicInteger(p,64)), NonNegativeInteger) -> %
 differentiate : (%, (NMExtendedPadicInteger(p,64) -> NMExtendedPadicInteger(p,64))) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 elt : (%, NMExtendedPadicInteger(p,64)) -> % if NMExtendedPadicInteger(p,64) has ELTAB(NXPADICZ(p,64),NXPADICZ(p,64))
 eval : (%, List(NMExtendedPadicInteger(p,64)), List(NMExtendedPadicInteger(p,64))) -> % if NMExtendedPadicInteger(p,64) has EVALAB(NXPADICZ(p,64))
 eval : (%, NMExtendedPadicInteger(p,64), NMExtendedPadicInteger(p,64)) -> % if NMExtendedPadicInteger(p,64) has EVALAB(NXPADICZ(p,64))
 eval : (%, Equation(NMExtendedPadicInteger(p,64))) -> % if NMExtendedPadicInteger(p,64) has EVALAB(NXPADICZ(p,64))
 eval : (%, List(Equation(NMExtendedPadicInteger(p,64)))) -> % if NMExtendedPadicInteger(p,64) has EVALAB(NXPADICZ(p,64))
 eval : (%, List(Symbol), List(NMExtendedPadicInteger(p,64))) -> % if NMExtendedPadicInteger(p,64) has IEVALAB(SYMBOL,NXPADICZ(p,64))
 eval : (%, Symbol, NMExtendedPadicInteger(p,64)) -> % if NMExtendedPadicInteger(p,64) has IEVALAB(SYMBOL,NXPADICZ(p,64))
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadicInteger(p,64) has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadicInteger(p,64) has PFECAT
 floor : % -> NMExtendedPadicInteger(p,64) if NMExtendedPadicInteger(p,64) has INS
 fractionPart : % -> % if NMExtendedPadicInteger(p,64) has EUCDOM
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 init : () -> % if NMExtendedPadicInteger(p,64) has STEP
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 map : ((NMExtendedPadicInteger(p,64) -> NMExtendedPadicInteger(p,64)), %) -> %
 max : (%, %) -> % if NMExtendedPadicInteger(p,64) has ORDSET
 min : (%, %) -> % if NMExtendedPadicInteger(p,64) has ORDSET
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 negative? : % -> Boolean if NMExtendedPadicInteger(p,64) has OINTDOM
 nextItem : % -> Union(%,"failed") if NMExtendedPadicInteger(p,64) has STEP
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if NMExtendedPadicInteger(p,64) has PATMAB(INT)
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if NMExtendedPadicInteger(p,64) has PATMAB(FLOAT)
 positive? : % -> Boolean if NMExtendedPadicInteger(p,64) has OINTDOM
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reducedSystem : Matrix(%) -> Matrix(Integer) if NMExtendedPadicInteger(p,64) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if NMExtendedPadicInteger(p,64) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(NMExtendedPadicInteger(p,64)),vec: Vector(NMExtendedPadicInteger(p,64)))
 reducedSystem : Matrix(%) -> Matrix(NMExtendedPadicInteger(p,64))
 retract : % -> Symbol if NMExtendedPadicInteger(p,64) has RETRACT(SYMBOL)
 retract : % -> Fraction(Integer) if NMExtendedPadicInteger(p,64) has RETRACT(INT)
 retract : % -> Integer if NMExtendedPadicInteger(p,64) has RETRACT(INT)
 retractIfCan : % -> Union(NMExtendedPadicInteger(p,64),"failed")
 retractIfCan : % -> Union(Symbol,"failed") if NMExtendedPadicInteger(p,64) has RETRACT(SYMBOL)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if NMExtendedPadicInteger(p,64) has RETRACT(INT)
 retractIfCan : % -> Union(Integer,"failed") if NMExtendedPadicInteger(p,64) has RETRACT(INT)
 sign : % -> Integer if NMExtendedPadicInteger(p,64) has OINTDOM
 smaller? : (%, %) -> Boolean if NMExtendedPadicInteger(p,64) has COMPAR
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if NMExtendedPadicInteger(p,64) has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadicInteger(p,64) has PFECAT
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
 wholePart : % -> NMExtendedPadicInteger(p,64) if NMExtendedPadicInteger(p,64) has EUCDOM
```

## Operations added

### `O`

- **Signature**: `()->%`

### `coerce`

- **Signature**: `(NMInteger)->%`

### `frobenius`

- **Signature**: `(%,Integer)->%`

### `jnpadic`

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `liftZ`

- **Signature**: `(%)->NMInteger`

### `modulus`

- **Signature**: `()->Integer`

### `precision`

- **Signature**: `(%)->Integer`

### `prime`

- **Signature**: `(%)->Integer`

### `teichmuller`

- **Signature**: `(%)->%`

### `valuation`

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
