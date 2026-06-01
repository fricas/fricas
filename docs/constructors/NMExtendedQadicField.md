# NMExtendedQadicField

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L1)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This is a domain for Qp.

**NMExtendedQadicField(R: Join(NMCommutativeRing,Finite),P: NMUnivariatePolynomialCategory(R),poly: P,prec: Integer) is a domain constructor**  
**Abbreviation for NMExtendedQadicField is NXQADICF**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, NMInteger) -> %                              ?*? : (NMInteger, %) -> %
 ?*? : (NMFraction(NMInteger), %) -> %                  ?*? : (%, NMFraction(NMInteger)) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?*? : (NonNegativeInteger, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (%, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, Integer) -> % if P has LINEXP(INT)
 ?*? : (P, %) -> %                                      ?*? : (%, P) -> %
 ?+? : (NMFraction(NMInteger), %) -> %                  ?+? : (%, NMFraction(NMInteger)) -> %
 ?+? : (%, NMInteger) -> %                              ?+? : (NMInteger, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (P, P) -> %
 ?/? : (%, %) -> %                                      0 : () -> %
 1 : () -> %                                            ?<? : (%, %) -> Boolean if P has ORDSET
 ?<=? : (%, %) -> Boolean if P has ORDSET               ?=? : (%, %) -> Boolean
 ?>? : (%, %) -> Boolean if P has ORDSET                ?>=? : (%, %) -> Boolean if P has ORDSET
 D : (%, (P -> P)) -> %                                 D : (%, (P -> P), NonNegativeInteger) -> %
 D : (%, Symbol) -> % if P has PDRING(SYMBOL)           D : % -> % if P has DIFRING
 O : () -> %                                            ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, %) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, Integer) -> %
 abs : % -> % if P has OINTDOM                          annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           approximate : (%, Integer) -> Integer
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 ceiling : % -> P if P has INS                          characteristic : () -> NonNegativeInteger
 coerce : NMInteger -> %                                coerce : % -> OutputForm
 coerce : % -> JLObject                                 coerce : Integer -> %
 coerce : % -> %                                        coerce : Symbol -> % if P has RETRACT(SYMBOL)
 coerce : P -> %                                        commutator : (%, %) -> %
 convert : % -> String                                  convert : % -> DoubleFloat if P has REAL
 convert : % -> Float if P has REAL                     denom : % -> P
 denominator : % -> %                                   differentiate : (%, (P -> P)) -> %
 differentiate : % -> % if P has DIFRING                elt : (%, P) -> % if P has ELTAB(P,P)
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 eval : (%, P, P) -> % if P has EVALAB(P)               exact? : % -> Boolean
 exactDivide : (%, %) -> %                              exp : % -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 floor : % -> P if P has INS                            fractionPart : % -> % if P has EUCDOM
 frobenius : (%, Integer) -> %                          gcd : (%, %) -> %
 gcd : List(%) -> %                                     init : () -> % if P has STEP
 inv : % -> %                                           jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlNMRing : () -> String                                jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 jnpadic : NMInteger -> %                               jnpadic : Integer -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 liftZ : % -> NMInteger                                 log : % -> %
 map : ((P -> P), %) -> %                               max : (%, %) -> % if P has ORDSET
 min : (%, %) -> % if P has ORDSET                      missing? : % -> Boolean
 moduloP : % -> Integer                                 modulus : () -> JLObjUInt64
 modulus : () -> Integer                                mutable? : % -> Boolean
 negative? : % -> Boolean if P has OINTDOM              nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            numer : % -> P
 numerator : % -> %                                     one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> NonNegativeInteger
 plenaryPower : (%, PositiveInteger) -> %               positive? : % -> Boolean if P has OINTDOM
 precision : % -> Integer                               prime : % -> NMInteger
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 quotientByP : % -> %                                   recip : % -> Union(%,"failed")
 reducedSystem : Matrix(%) -> Matrix(P)                 ?rem? : (%, %) -> %
 retract : % -> Integer if P has RETRACT(INT)           retract : % -> Symbol if P has RETRACT(SYMBOL)
 retract : % -> P                                       retractIfCan : % -> Union(P,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sign : % -> Integer if P has OINTDOM                   sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean if P has COMPAR           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 teichmuller : % -> %                                   unit? : % -> Boolean
 unitCanonical : % -> %                                 valuation : % -> %
 wholePart : % -> P if P has EUCDOM                     zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if P has PDRING(SYMBOL)
 D : (%, Symbol, NonNegativeInteger) -> % if P has PDRING(SYMBOL)
 D : (%, List(Symbol)) -> % if P has PDRING(SYMBOL)
 D : (%, NonNegativeInteger) -> % if P has DIFRING
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and P has PFECAT
 coerce : Fraction(Integer) -> % if % has CHARZ or P has RETRACT(INT)
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and P has PFECAT
 convert : % -> InputForm if P has KONVERT(INFORM)
 convert : % -> Pattern(Float) if P has KONVERT(PATTERN(FLOAT))
 convert : % -> Pattern(Integer) if P has KONVERT(PATTERN(INT))
 differentiate : (%, (P -> P), NonNegativeInteger) -> %
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if P has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if P has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if P has PDRING(SYMBOL)
 differentiate : (%, Symbol) -> % if P has PDRING(SYMBOL)
 differentiate : (%, NonNegativeInteger) -> % if P has DIFRING
 divide : (%, %) -> Record(quotient: %,remainder: %)
 eval : (%, Symbol, P) -> % if P has IEVALAB(SYMBOL,P)
 eval : (%, List(Symbol), List(P)) -> % if P has IEVALAB(SYMBOL,P)
 eval : (%, List(Equation(P))) -> % if P has EVALAB(P)
 eval : (%, Equation(P)) -> % if P has EVALAB(P)
 eval : (%, List(P), List(P)) -> % if P has EVALAB(P)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if P has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if P has PFECAT
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 nextItem : % -> Union(%,"failed") if P has STEP
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if P has PATMAB(FLOAT)
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if P has PATMAB(INT)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(P),vec: Vector(P))
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if P has LINEXP(INT)
 reducedSystem : Matrix(%) -> Matrix(Integer) if P has LINEXP(INT)
 retract : % -> Fraction(Integer) if P has RETRACT(INT)
 retractIfCan : % -> Union(Integer,"failed") if P has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if P has RETRACT(INT)
 retractIfCan : % -> Union(Symbol,"failed") if P has RETRACT(SYMBOL)
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if P has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if P has PFECAT
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `O` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L63)\]

O() returns the default Big-oh from domain parameters.

- **Signature**: `()->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L57)\]

coerce(x) returns x as the q-adic completion of the Nemo Integer.

- **Signature**: `(NMInteger)->%`

### `frobenius` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L54)\]

frobenius(x,i) returns the image of the i-th power of Frobenius of x.

- **Signature**: `(%,Integer)->%`

### `jnpadic` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L59)\]

jnpadic(x) returns x as the q-adic completion of the Nemo Integer.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `liftZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L49)\]

liftZ(x) lifts x to a Nemo Integer if possible.

- **Signature**: `(%)->NMInteger`

### `modulus` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L70)\]

modulus() returns the modulus.

- **Signature**: `()->JLObjUInt64`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L68)\]

precision(x) returns the precision used for x.

- **Signature**: `(%)->Integer`

### `prime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L65)\]

prime(x) returns the modulus used for x. Convenience function.

- **Signature**: `(%)->NMInteger`

### `teichmuller` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L51)\]

teichmuller(x) computes the Teichmuller lift of x. The valuation of x must be non negative.

- **Signature**: `(%)->%`

### `valuation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NXQADICF.spad#L47)\]

valuation(x) is the valuation of x.

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
