# NMQadic

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NQADIC.spad#L1)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This is a domain for Qp.

**NMQadic(p: NMInteger) is a domain constructor**  
**Abbreviation for NMQadic is NQADIC**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, NMInteger) -> %                              ?*? : (NMInteger, %) -> %
 ?*? : (NMFraction(NMInteger), %) -> %                  ?*? : (%, NMFraction(NMInteger)) -> %
 ?*? : (%, NMExtendedPadic(p,64)) -> %                  ?*? : (NMExtendedPadic(p,64), %) -> %
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
 coerce : NMInteger -> %                                coerce : NMExtendedPadic(p,64) -> %
 coerce : % -> %                                        coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> String
 denom : % -> NMExtendedPadic(p,64)                     denominator : % -> %
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
 nthRoot : (%, Integer) -> %                            numer : % -> NMExtendedPadic(p,64)
 numerator : % -> %                                     one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> NonNegativeInteger
 plenaryPower : (%, PositiveInteger) -> %               precision : % -> Integer
 prime : % -> NMInteger                                 prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    quotientByP : % -> %
 recip : % -> Union(%,"failed")                         ?rem? : (%, %) -> %
 retract : % -> NMExtendedPadic(p,64)                   rightPower : (%, PositiveInteger) -> %
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
 ?*? : (%, Integer) -> % if NMExtendedPadic(p,64) has LINEXP(INT)
 ?/? : (NMExtendedPadic(p,64), NMExtendedPadic(p,64)) -> %
 ?<? : (%, %) -> Boolean if NMExtendedPadic(p,64) has ORDSET
 ?<=? : (%, %) -> Boolean if NMExtendedPadic(p,64) has ORDSET
 ?>? : (%, %) -> Boolean if NMExtendedPadic(p,64) has ORDSET
 ?>=? : (%, %) -> Boolean if NMExtendedPadic(p,64) has ORDSET
 D : % -> % if NMExtendedPadic(p,64) has DIFRING
 D : (%, NonNegativeInteger) -> % if NMExtendedPadic(p,64) has DIFRING
 D : (%, Symbol) -> % if NMExtendedPadic(p,64) has PDRING(SYMBOL)
 D : (%, List(Symbol)) -> % if NMExtendedPadic(p,64) has PDRING(SYMBOL)
 D : (%, Symbol, NonNegativeInteger) -> % if NMExtendedPadic(p,64) has PDRING(SYMBOL)
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMExtendedPadic(p,64) has PDRING(SYMBOL)
 D : (%, (NMExtendedPadic(p,64) -> NMExtendedPadic(p,64)), NonNegativeInteger) -> %
 D : (%, (NMExtendedPadic(p,64) -> NMExtendedPadic(p,64))) -> %
 abs : % -> % if NMExtendedPadic(p,64) has OINTDOM
 ceiling : % -> NMExtendedPadic(p,64) if NMExtendedPadic(p,64) has INS
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and NMExtendedPadic(p,64) has PFECAT
 coerce : Symbol -> % if NMExtendedPadic(p,64) has RETRACT(SYMBOL)
 coerce : Fraction(Integer) -> % if % has CHARZ or NMExtendedPadic(p,64) has RETRACT(INT)
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and NMExtendedPadic(p,64) has PFECAT
 convert : % -> Pattern(Integer) if NMExtendedPadic(p,64) has KONVERT(PATTERN(INT))
 convert : % -> Pattern(Float) if NMExtendedPadic(p,64) has KONVERT(PATTERN(FLOAT))
 convert : % -> InputForm if NMExtendedPadic(p,64) has KONVERT(INFORM)
 convert : % -> Float if NMExtendedPadic(p,64) has REAL
 convert : % -> DoubleFloat if NMExtendedPadic(p,64) has REAL
 differentiate : % -> % if NMExtendedPadic(p,64) has DIFRING
 differentiate : (%, NonNegativeInteger) -> % if NMExtendedPadic(p,64) has DIFRING
 differentiate : (%, Symbol) -> % if NMExtendedPadic(p,64) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if NMExtendedPadic(p,64) has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if NMExtendedPadic(p,64) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMExtendedPadic(p,64) has PDRING(SYMBOL)
 differentiate : (%, (NMExtendedPadic(p,64) -> NMExtendedPadic(p,64)), NonNegativeInteger) -> %
 differentiate : (%, (NMExtendedPadic(p,64) -> NMExtendedPadic(p,64))) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 elt : (%, NMExtendedPadic(p,64)) -> % if NMExtendedPadic(p,64) has ELTAB(NXPADIC(p,64),NXPADIC(p,64))
 eval : (%, List(NMExtendedPadic(p,64)), List(NMExtendedPadic(p,64))) -> % if NMExtendedPadic(p,64) has EVALAB(NXPADIC(p,64))
 eval : (%, NMExtendedPadic(p,64), NMExtendedPadic(p,64)) -> % if NMExtendedPadic(p,64) has EVALAB(NXPADIC(p,64))
 eval : (%, Equation(NMExtendedPadic(p,64))) -> % if NMExtendedPadic(p,64) has EVALAB(NXPADIC(p,64))
 eval : (%, List(Equation(NMExtendedPadic(p,64)))) -> % if NMExtendedPadic(p,64) has EVALAB(NXPADIC(p,64))
 eval : (%, List(Symbol), List(NMExtendedPadic(p,64))) -> % if NMExtendedPadic(p,64) has IEVALAB(SYMBOL,NXPADIC(p,64))
 eval : (%, Symbol, NMExtendedPadic(p,64)) -> % if NMExtendedPadic(p,64) has IEVALAB(SYMBOL,NXPADIC(p,64))
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadic(p,64) has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadic(p,64) has PFECAT
 floor : % -> NMExtendedPadic(p,64) if NMExtendedPadic(p,64) has INS
 fractionPart : % -> % if NMExtendedPadic(p,64) has EUCDOM
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 init : () -> % if NMExtendedPadic(p,64) has STEP
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 map : ((NMExtendedPadic(p,64) -> NMExtendedPadic(p,64)), %) -> %
 max : (%, %) -> % if NMExtendedPadic(p,64) has ORDSET
 min : (%, %) -> % if NMExtendedPadic(p,64) has ORDSET
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 negative? : % -> Boolean if NMExtendedPadic(p,64) has OINTDOM
 nextItem : % -> Union(%,"failed") if NMExtendedPadic(p,64) has STEP
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if NMExtendedPadic(p,64) has PATMAB(INT)
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if NMExtendedPadic(p,64) has PATMAB(FLOAT)
 positive? : % -> Boolean if NMExtendedPadic(p,64) has OINTDOM
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reducedSystem : Matrix(%) -> Matrix(Integer) if NMExtendedPadic(p,64) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if NMExtendedPadic(p,64) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(NMExtendedPadic(p,64)),vec: Vector(NMExtendedPadic(p,64)))
 reducedSystem : Matrix(%) -> Matrix(NMExtendedPadic(p,64))
 retract : % -> Symbol if NMExtendedPadic(p,64) has RETRACT(SYMBOL)
 retract : % -> Fraction(Integer) if NMExtendedPadic(p,64) has RETRACT(INT)
 retract : % -> Integer if NMExtendedPadic(p,64) has RETRACT(INT)
 retractIfCan : % -> Union(NMExtendedPadic(p,64),"failed")
 retractIfCan : % -> Union(Symbol,"failed") if NMExtendedPadic(p,64) has RETRACT(SYMBOL)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if NMExtendedPadic(p,64) has RETRACT(INT)
 retractIfCan : % -> Union(Integer,"failed") if NMExtendedPadic(p,64) has RETRACT(INT)
 sign : % -> Integer if NMExtendedPadic(p,64) has OINTDOM
 smaller? : (%, %) -> Boolean if NMExtendedPadic(p,64) has COMPAR
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if NMExtendedPadic(p,64) has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMExtendedPadic(p,64) has PFECAT
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
 wholePart : % -> NMExtendedPadic(p,64) if NMExtendedPadic(p,64) has EUCDOM
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

- **Signature**: `()->NMInteger`

### `precision`

- **Signature**: `(%)->Integer`

### `prime`

- **Signature**: `(%)->NMInteger`

### `teichmuller`

- **Signature**: `(%)->%`

### `valuation`

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
