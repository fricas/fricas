# NMPolynomial

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L973)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This type is a basic representation of generic sparse distributed multivariable polynomial types using the Julia package Nemo (AbstractAlgebra). It is parameterized by the coefficient ring. Experimental domain.

**NMPolynomial(R: NMRing) is a domain constructor**  
**Abbreviation for NMPolynomial is NPOLY**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, NonNegativeInteger) -> %                     ?*? : (%, PositiveInteger) -> %
 ?*? : (%, R) -> %                                      ?*? : (R, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 ?/? : (%, R) -> % if R has FIELD                       0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 D : (%, List(Symbol)) -> % if R has RING               D : (%, Symbol) -> % if R has RING
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 coefficient : (%, IndexedExponents(Symbol)) -> R       coefficients : % -> JLVector(R)
 coefficients : % -> List(R)                            coerce : R -> %
 coerce : Symbol -> % if R has SRING                    coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 constant? : % -> Boolean                               constantCoefficient : % -> R
 content : % -> R if R has GCDDOM                       content : (%, Symbol) -> % if R has GCDDOM
 convert : % -> String                                  degree : % -> IndexedExponents(Symbol)
 degree : (%, Symbol) -> NonNegativeInteger             equal? : (%, %) -> Boolean
 eval : (%, Symbol, R) -> %                             eval : (%, List(Symbol), List(R)) -> %
 eval : (%, Symbol, %) -> %                             eval : (%, List(Symbol), List(%)) -> %
 eval : (%, Equation(%)) -> % if R has SRING            eval : (%, %, %) -> % if R has SRING
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 factor : % -> NMFactored(%)                            factor : % -> Factored(%) if R has PFECAT
 gcd : List(%) -> % if R has GCDDOM                     gcd : (%, %) -> % if R has GCDDOM
 ground : % -> R                                        ground? : % -> Boolean
 internalOrdering : () -> JLObject                      isPlus : % -> Union(List(%),"failed")
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
 lcm : List(%) -> % if R has GCDDOM                     lcm : (%, %) -> % if R has GCDDOM
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     length : % -> NonNegativeInteger
 mainVariable : % -> Union(Symbol,"failed")             map : ((R -> R), %) -> %
 minimumDegree : % -> IndexedExponents(Symbol)          minimumDegree : (%, Symbol) -> NonNegativeInteger
 missing? : % -> Boolean                                monomial : (R, IndexedExponents(Symbol)) -> %
 monomial : (%, Symbol, NonNegativeInteger) -> %        monomial? : % -> Boolean
 monomialRecursive? : % -> Boolean                      monomials : % -> JLVector(%)
 monomials : % -> List(%)                               mutable? : % -> Boolean
 nemoPPrint : Boolean -> Boolean                        nothing? : % -> Boolean
 numberOfMonomials : % -> NonNegativeInteger            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          plenaryPower : (%, PositiveInteger) -> %
 prime? : % -> Boolean if R has PFECAT                  primitiveMonomials : % -> JLVector(%)
 primitivePart : % -> % if R has GCDDOM                 recip : % -> Union(%,"failed")
 retract : % -> Integer if R has RETRACT(INT)           retract : % -> R
 retract : % -> Symbol if R has SRING                   retractIfCan : % -> Union(R,"failed")
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 smaller? : (%, %) -> Boolean if R has COMPAR           square? : % -> Boolean
 squareFree : % -> Factored(%) if R has GCDDOM          squareFreePart : % -> % if R has GCDDOM
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 support : % -> List(IndexedExponents(Symbol))          term? : % -> Boolean
 termRecursive? : % -> Boolean                          totalDegree : % -> NonNegativeInteger
 trailingCoefficient : % -> R                           unit? : % -> Boolean
 univariate : % -> SparseUnivariatePolynomial(R)        univariate? : % -> Boolean
 variables : % -> List(Symbol)                          vectorOfTerms : % -> JLVector(%)
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if R has ALGEBRA(FRAC(INT)) or R has ALGEBRA(NFRAC(NINT))
 ?*? : (%, Fraction(Integer)) -> % if R has ALGEBRA(FRAC(INT)) or R has ALGEBRA(NFRAC(NINT))
 ?*? : (%, Integer) -> % if R has LINEXP(INT) and R has RING
 ?+? : (Fraction(Integer), %) -> % if R has ALGEBRA(NFRAC(NINT))
 ?+? : (%, Fraction(Integer)) -> % if R has ALGEBRA(NFRAC(NINT))
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if R has RING
 D : (%, Symbol, NonNegativeInteger) -> % if R has RING
 associates? : (%, %) -> Boolean if % has ATVCWC and R has INTDOM or R has ENTIRER
 binomThmExpt : (%, %, NonNegativeInteger) -> % if % has COMRING
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and R has PFECAT
 coefficient : (%, Symbol, NonNegativeInteger) -> %
 coefficient : (%, List(Symbol), List(NonNegativeInteger)) -> %
 coerce : Fraction(Integer) -> % if R has ALGEBRA(FRAC(INT)) or R has RETRACT(FRAC(INT))
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and R has PFECAT
 construct : List(Record(k: IndexedExponents(Symbol),c: R)) -> %
 constructOrdered : List(Record(k: IndexedExponents(Symbol),c: R)) -> % if IndexedExponents(Symbol) has COMPAR
 convert : % -> Pattern(Float) if R has KONVERT(PATTERN(FLOAT)) and R has RING and Symbol has KONVERT(PATTERN(FLOAT))
 convert : % -> Pattern(Integer) if R has KONVERT(PATTERN(INT)) and R has RING and Symbol has KONVERT(PATTERN(INT))
 convert : % -> InputForm if R has KONVERT(INFORM) and Symbol has KONVERT(INFORM)
 degree : (%, PositiveInteger) -> NonNegativeInteger
 degree : (%, List(Symbol)) -> List(NonNegativeInteger)
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if R has RING
 differentiate : (%, Symbol, NonNegativeInteger) -> % if R has RING
 differentiate : (%, List(Symbol)) -> % if R has RING
 differentiate : (%, Symbol) -> % if R has RING
 discriminant : (%, Symbol) -> % if R has COMRING
 eval : (%, List(Equation(%))) -> % if R has SRING
 eval : (%, List(%), List(%)) -> % if R has SRING
 ?exquo? : (%, R) -> Union(%,"failed") if R has ENTIRER
 ?exquo? : (%, %) -> Union(%,"failed") if % has ATVCWC and R has INTDOM or R has ENTIRER
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 fmecg : (%, IndexedExponents(Symbol), R, %) -> % if R has RING
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%) if R has GCDDOM
 hash : % -> SingleInteger if R has HASHABL and Symbol has HASHABL
 hashUpdate! : (HashState, %) -> HashState if R has HASHABL and Symbol has HASHABL
 isExpt : % -> Union(Record(var: Symbol,exponent: NonNegativeInteger),"failed") if R has SRING
 isTimes : % -> Union(List(%),"failed") if R has SRING
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %) if R has GCDDOM
 leadingCoefficient : % -> R if IndexedExponents(Symbol) has COMPAR
 leadingMonomial : % -> % if IndexedExponents(Symbol) has COMPAR
 leadingSupport : % -> IndexedExponents(Symbol) if IndexedExponents(Symbol) has COMPAR
 leadingTerm : % -> Record(k: IndexedExponents(Symbol),c: R) if IndexedExponents(Symbol) has COMPAR
 linearExtend : ((IndexedExponents(Symbol) -> R), %) -> R if R has COMRING
 listOfTerms : % -> List(Record(k: IndexedExponents(Symbol),c: R))
 mapExponents : ((IndexedExponents(Symbol) -> IndexedExponents(Symbol)), %) -> %
 minimumDegree : (%, List(Symbol)) -> List(NonNegativeInteger)
 monicDivide : (%, %, Symbol) -> Record(quotient: %,remainder: %) if R has RING
 monomial : (%, List(Symbol), List(NonNegativeInteger)) -> %
 multivariate : (SparseUnivariatePolynomial(R), Symbol) -> %
 multivariate : (SparseUnivariatePolynomial(%), Symbol) -> %
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if R has PATMAB(FLOAT) and R has RING and Symbol has PATMAB(FLOAT)
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if R has PATMAB(INT) and R has RING and Symbol has PATMAB(INT)
 pomopo! : (%, R, IndexedExponents(Symbol), %) -> %
 primitiveMonomials : % -> List(%) if R has SRING
 primitivePart : (%, Symbol) -> % if R has GCDDOM
 reducedSystem : Matrix(%) -> Matrix(Integer) if R has LINEXP(INT) and R has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if R has LINEXP(INT) and R has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(R),vec: Vector(R)) if R has RING
 reducedSystem : Matrix(%) -> Matrix(R) if R has RING
 reductum : % -> % if IndexedExponents(Symbol) has COMPAR
 resultant : (%, %, Symbol) -> % if R has COMRING
 retract : % -> Fraction(Integer) if R has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Integer,"failed") if R has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if R has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Symbol,"failed") if R has SRING
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if R has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 totalDegree : (%, List(Symbol)) -> NonNegativeInteger
 totalDegreeSorted : (%, List(Symbol)) -> NonNegativeInteger
 unitCanonical : % -> % if % has ATVCWC and R has INTDOM or R has ENTIRER
 unitNormal : % -> Record(unit: %,canonical: %,associate: %) if % has ATVCWC and R has INTDOM or R has ENTIRER
 univariate : (%, Symbol) -> SparseUnivariatePolynomial(%)
```

## Operations added

### `coefficients` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1088)\]

- **Signature**: `(%)->JLVector(R)`

### `constant?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1036)\]

- **Signature**: `(%)->Boolean`

### `constantCoefficient` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1098)\]

- **Signature**: `(%)->R`

### `degree` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1084)\]

- **Signature**: `(%,PositiveInteger)->NonNegativeInteger`

### `exactDivide` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1069)\]

- **Signature**: `(%,%)->%`

### `factor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1082)\]

- **Signature**: `(%)->NMFactored(%)`

### `internalOrdering` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1100)\]

- **Signature**: `()->JLObject`

### `length` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1086)\]

- **Signature**: `(%)->NonNegativeInteger`

### `monomial?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1038)\]

- **Signature**: `(%)->Boolean`

### `monomialRecursive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1040)\]

- **Signature**: `(%)->Boolean`

### `monomials` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1090)\]

- **Signature**: `(%)->JLVector(%)`

### `nemoPPrint`

- **Signature**: `(Boolean)->Boolean`

### `primitiveMonomials` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1093)\]

- **Signature**: `(%)->JLVector(%)`

### `square?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1050)\]

- **Signature**: `(%)->Boolean`

### `term?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1043)\]

- **Signature**: `(%)->Boolean`

### `termRecursive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1045)\]

- **Signature**: `(%)->Boolean`

### `trailingCoefficient` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1096)\]

- **Signature**: `(%)->R`

### `univariate?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1048)\]

- **Signature**: `(%)->Boolean`

### `vectorOfTerms` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1103)\]

- **Signature**: `(%)->JLVector(%)`
---
[Back to Index](../index.md)
