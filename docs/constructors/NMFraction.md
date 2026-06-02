# NMFraction

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1751)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

Nemo Fractions over an integral domain.

**NMFraction(R: Join(NMCommutativeRing,IntegralDomain)) is a domain constructor**  
**Abbreviation for NMFraction is NFRAC**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (NMInteger, %) -> %                              ?*? : (%, NMInteger) -> %
 ?*? : (%, R) -> %                                      ?*? : (R, %) -> %
 ?*? : (%, Integer) -> % if R has LINEXP(INT)           ?*? : (PositiveInteger, %) -> %
 ?*? : (%, %) -> %                                      ?*? : (Integer, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      ?-? : (%, %) -> %
 -? : % -> %                                            ?/? : (R, %) -> %
 ?/? : (R, R) -> %                                      ?/? : (%, %) -> %
 0 : () -> %                                            1 : () -> %
 ?<? : (%, %) -> Boolean if R has ORDSET                ?<=? : (%, %) -> Boolean if R has ORDSET
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean if R has ORDSET
 ?>=? : (%, %) -> Boolean if R has ORDSET               D : % -> % if R has DIFRING
 D : (%, Symbol) -> % if R has PDRING(SYMBOL)           D : (%, (R -> R), NonNegativeInteger) -> %
 D : (%, (R -> R)) -> %                                 ?^? : (%, Integer) -> %
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 abs : % -> % if R has OINTDOM                          abs2 : % -> % if R has INS
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 ceiling : % -> R if R has INS                          characteristic : () -> NonNegativeInteger
 coerce : R -> %                                        coerce : Symbol -> % if R has RETRACT(SYMBOL)
 coerce : % -> %                                        coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> Float if R has REAL
 convert : % -> DoubleFloat if R has REAL               convert : % -> String
 denom : % -> R                                         denominator : % -> %
 differentiate : % -> % if R has DIFRING                differentiate : (%, (R -> R)) -> %
 elt : (%, R) -> % if R has ELTAB(R,R)                  equal? : (%, %) -> Boolean
 euclideanSize : % -> NonNegativeInteger                eval : (%, R, R) -> % if R has EVALAB(R)
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 floor : % -> R if R has INS                            fractionPart : % -> % if R has EUCDOM
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 init : () -> % if R has STEP                           inv : % -> %
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
 jlref : String -> %                                    latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     map : ((R -> R), %) -> %
 max : (%, %) -> % if R has ORDSET                      min : (%, %) -> % if R has ORDSET
 missing? : % -> Boolean                                mutable? : % -> Boolean
 negative? : % -> Boolean if R has OINTDOM              nothing? : % -> Boolean
 numer : % -> R                                         numerator : % -> %
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               positive? : % -> Boolean if R has OINTDOM
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 random : (Segment(Integer), Segment(Integer)) -> %     random : Segment(Integer) -> %
 random : PositiveInteger -> % if R has INS             recip : % -> Union(%,"failed")
 reducedSystem : Matrix(%) -> Matrix(R)                 ?rem? : (%, %) -> %
 retract : % -> R                                       retract : % -> Symbol if R has RETRACT(SYMBOL)
 retract : % -> Integer if R has RETRACT(INT)           retractIfCan : % -> Union(R,"failed")
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sign : % -> Integer if R has OINTDOM                   sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean if R has COMPAR           squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            unit? : % -> Boolean
 unitCanonical : % -> %                                 wholePart : % -> R if R has EUCDOM
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ
 D : (%, NonNegativeInteger) -> % if R has DIFRING
 D : (%, List(Symbol)) -> % if R has PDRING(SYMBOL)
 D : (%, Symbol, NonNegativeInteger) -> % if R has PDRING(SYMBOL)
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if R has PDRING(SYMBOL)
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and R has PFECAT
 coerce : Fraction(Integer) -> % if % has CHARZ or R has RETRACT(INT)
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and R has PFECAT
 convert : % -> Pattern(Integer) if R has KONVERT(PATTERN(INT))
 convert : % -> Pattern(Float) if R has KONVERT(PATTERN(FLOAT))
 convert : % -> InputForm if R has KONVERT(INFORM)
 differentiate : (%, (NMInteger -> NMInteger)) -> % if R has INS
 differentiate : (%, NonNegativeInteger) -> % if R has DIFRING
 differentiate : (%, Symbol) -> % if R has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if R has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if R has PDRING(SYMBOL)
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if R has PDRING(SYMBOL)
 differentiate : (%, (R -> R), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 eval : (%, List(R), List(R)) -> % if R has EVALAB(R)
 eval : (%, Equation(R)) -> % if R has EVALAB(R)
 eval : (%, List(Equation(R))) -> % if R has EVALAB(R)
 eval : (%, List(Symbol), List(R)) -> % if R has IEVALAB(SYMBOL,R)
 eval : (%, Symbol, R) -> % if R has IEVALAB(SYMBOL,R)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorFraction : % -> Fraction(Factored(R)) if R has UFD
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 nextItem : % -> Union(%,"failed") if R has STEP
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if R has PATMAB(INT)
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if R has PATMAB(FLOAT)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reducedSystem : Matrix(%) -> Matrix(Integer) if R has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if R has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(R),vec: Vector(R))
 retract : % -> Fraction(Integer) if R has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if R has RETRACT(INT)
 retractIfCan : % -> Union(Symbol,"failed") if R has RETRACT(SYMBOL)
 retractIfCan : % -> Union(Integer,"failed") if R has RETRACT(INT)
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if R has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1779)\]

abs2(x) returns the square of the absolute value of x.

- **Signature**: `(%)->%`

### `differentiate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1784)\]

differentiate(x, f) differentiates x extending the derivation f on NMFraction(R).

- **Signature**: `(%,(NMInteger)->NMInteger)->%`

### `factorFraction` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1767)\]

factorFraction(r) factors the numerator and the denominator of the fraction r.

- **Signature**: `(%)->Fraction(Factored(R))`

### `random` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1781)\]

random(p) returns a random rational with numerator and denominator having p bits before canonicalisation.

- **Signature**: `(PositiveInteger)->%`

random(seg) returns a random fraction depending on the base ring.

- **Signature**: `(Segment(Integer))->%`

random(seg1, seg2) returns a random fraction depending on the base ring. For example:

**Example**:
```fricas
FRPRing:=NFRAC (NUP(NINT,'x))
```

**Example**:
```fricas
random(1..5,-10..10)$FRPRing
```

- **Signature**: `(Segment(Integer),Segment(Integer))->%`
---
[Back to Index](../index.md)
