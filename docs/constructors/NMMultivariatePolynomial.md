# NMMultivariatePolynomial

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1398)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This type is a basic representation of sparse, distributed multivariate polynomials using the Julia package Nemo. It is parameterized by the coefficient ring. The coefficient ring may be non-commutative, but the variables are assumed to commute. The monomial ordering used for internal storage is lexicographic. For example: VarSet: List Symbol:=[x,y,z] V := OrderedVariableList(VarSet) -- eventually, for later use -- See SparseMultivariatePolynomial -- E := IndexedExponents V PRing := NMP(NINT,VarSet) x := x::V::PRing y := y::V::PRing z := z:f:V::PRing p:=x*2+3*y^2+17*z^13 p^7

**NMMultivariatePolynomial(R: NMRing,VarSet: List(Symbol)) is a domain constructor**  
**Abbreviation for NMMultivariatePolynomial is NMP**  
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
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 coefficients : % -> JLVector(R)                        coefficients : % -> List(R)
 coerce : R -> %                                        coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 constant? : % -> Boolean                               constantCoefficient : % -> R
 content : % -> R if R has GCDDOM                       convert : % -> String
 equal? : (%, %) -> Boolean                             eval : (%, OrderedVariableList(VarSet), R) -> %
 eval : (%, OrderedVariableList(VarSet), %) -> %        eval : (%, Equation(%)) -> % if R has SRING
 eval : (%, %, %) -> % if R has SRING                   exact? : % -> Boolean
 exactDivide : (%, %) -> %                              factor : % -> NMFactored(%)
 factor : % -> Factored(%) if R has PFECAT              gcd : List(%) -> % if R has GCDDOM
 gcd : (%, %) -> % if R has GCDDOM                      ground : % -> R
 ground? : % -> Boolean                                 internalOrdering : () -> JLObject
 isPlus : % -> Union(List(%),"failed")                  jlAbout : % -> Void
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
 jmp2nmp : MultivariatePolynomial(VarSet,R) -> %        latex : % -> String
 lcm : List(%) -> % if R has GCDDOM                     lcm : (%, %) -> % if R has GCDDOM
 leading_monomial : % -> %                              leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 length : % -> NonNegativeInteger                       map : ((R -> R), %) -> %
 missing? : % -> Boolean                                monomial? : % -> Boolean
 monomialRecursive? : % -> Boolean                      monomials : % -> JLVector(%)
 monomials : % -> List(%)                               mutable? : % -> Boolean
 nemoMPPrint : Boolean -> Boolean                       nothing? : % -> Boolean
 numberOfMonomials : % -> NonNegativeInteger            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          plenaryPower : (%, PositiveInteger) -> %
 prime? : % -> Boolean if R has PFECAT                  primitiveMonomials : % -> JLVector(%)
 primitivePart : % -> % if R has GCDDOM                 recip : % -> Union(%,"failed")
 retract : % -> Integer if R has RETRACT(INT)           retract : % -> R
 retractIfCan : % -> Union(R,"failed")                  rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       shiftLeft : (%, NonNegativeInteger) -> %
 shiftLeft! : (%, NonNegativeInteger) -> %              shiftRight : (%, NonNegativeInteger) -> %
 shiftRight! : (%, NonNegativeInteger) -> %             smaller? : (%, %) -> Boolean if R has COMPAR
 square? : % -> Boolean                                 squareFree : % -> Factored(%) if R has GCDDOM
 squareFreePart : % -> % if R has GCDDOM                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            term? : % -> Boolean
 termRecursive? : % -> Boolean                          totalDegree : % -> NonNegativeInteger
 trailingCoefficient : % -> R                           unit? : % -> Boolean
 univariate : % -> SparseUnivariatePolynomial(R)        univariate? : % -> Boolean
 variables : % -> List(OrderedVariableList(VarSet))     vectorOfTerms : % -> JLVector(%)
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if R has ALGEBRA(FRAC(INT)) or R has ALGEBRA(NFRAC(NINT))
 ?*? : (%, Fraction(Integer)) -> % if R has ALGEBRA(FRAC(INT)) or R has ALGEBRA(NFRAC(NINT))
 ?*? : (%, Integer) -> % if R has LINEXP(INT) and R has RING
 ?+? : (Fraction(Integer), %) -> % if R has ALGEBRA(NFRAC(NINT))
 ?+? : (%, Fraction(Integer)) -> % if R has ALGEBRA(NFRAC(NINT))
 D : (%, List(OrderedVariableList(VarSet)), List(NonNegativeInteger)) -> % if R has RING
 D : (%, OrderedVariableList(VarSet), NonNegativeInteger) -> % if R has RING
 D : (%, List(OrderedVariableList(VarSet))) -> % if R has RING
 D : (%, OrderedVariableList(VarSet)) -> % if R has RING
 associates? : (%, %) -> Boolean if % has ATVCWC and R has INTDOM or R has ENTIRER
 binomThmExpt : (%, %, NonNegativeInteger) -> % if % has COMRING
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and R has PFECAT
 coefficient : (%, IndexedExponents(OrderedVariableList(VarSet))) -> R
 coefficient : (%, OrderedVariableList(VarSet), NonNegativeInteger) -> %
 coefficient : (%, List(OrderedVariableList(VarSet)), List(NonNegativeInteger)) -> %
 coerce : Fraction(Integer) -> % if R has ALGEBRA(FRAC(INT)) or R has RETRACT(FRAC(INT))
 coerce : OrderedVariableList(VarSet) -> % if R has SRING
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and R has PFECAT
 construct : List(Record(k: IndexedExponents(OrderedVariableList(VarSet)),c: R)) -> %
 constructOrdered : List(Record(k: IndexedExponents(OrderedVariableList(VarSet)),c: R)) -> % if IndexedExponents(OrderedVariableList(VarSet)) has COMPAR
 content : (%, OrderedVariableList(VarSet)) -> % if R has GCDDOM
 convert : % -> Pattern(Float) if R has KONVERT(PATTERN(FLOAT)) and R has RING and OrderedVariableList(VarSet) has KONVERT(PATTERN(FLOAT))
 convert : % -> Pattern(Integer) if R has KONVERT(PATTERN(INT)) and R has RING and OrderedVariableList(VarSet) has KONVERT(PATTERN(INT))
 convert : % -> InputForm if R has KONVERT(INFORM) and OrderedVariableList(VarSet) has KONVERT(INFORM)
 degree : (%, PositiveInteger) -> NonNegativeInteger
 degree : % -> IndexedExponents(OrderedVariableList(VarSet))
 degree : (%, OrderedVariableList(VarSet)) -> NonNegativeInteger
 degree : (%, List(OrderedVariableList(VarSet))) -> List(NonNegativeInteger)
 differentiate : (%, List(OrderedVariableList(VarSet)), List(NonNegativeInteger)) -> % if R has RING
 differentiate : (%, OrderedVariableList(VarSet), NonNegativeInteger) -> % if R has RING
 differentiate : (%, List(OrderedVariableList(VarSet))) -> % if R has RING
 differentiate : (%, OrderedVariableList(VarSet)) -> % if R has RING
 discriminant : (%, OrderedVariableList(VarSet)) -> % if R has COMRING
 eval : (%, List(OrderedVariableList(VarSet)), List(R)) -> %
 eval : (%, List(OrderedVariableList(VarSet)), List(%)) -> %
 eval : (%, List(Equation(%))) -> % if R has SRING
 eval : (%, List(%), List(%)) -> % if R has SRING
 exponent_vectors : % -> JLVector(JLVector(JLObjInt64))
 ?exquo? : (%, R) -> Union(%,"failed") if R has ENTIRER
 ?exquo? : (%, %) -> Union(%,"failed") if % has ATVCWC and R has INTDOM or R has ENTIRER
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 fmecg : (%, IndexedExponents(OrderedVariableList(VarSet)), R, %) -> % if R has RING
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%) if R has GCDDOM
 hash : % -> SingleInteger if R has HASHABL and OrderedVariableList(VarSet) has HASHABL
 hashUpdate! : (HashState, %) -> HashState if R has HASHABL and OrderedVariableList(VarSet) has HASHABL
 isExpt : % -> Union(Record(var: OrderedVariableList(VarSet),exponent: NonNegativeInteger),"failed") if R has SRING
 isTimes : % -> Union(List(%),"failed") if R has SRING
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %) if R has GCDDOM
 leadingCoefficient : % -> R if IndexedExponents(OrderedVariableList(VarSet)) has COMPAR
 leadingMonomial : % -> % if IndexedExponents(OrderedVariableList(VarSet)) has COMPAR
 leadingSupport : % -> IndexedExponents(OrderedVariableList(VarSet)) if IndexedExponents(OrderedVariableList(VarSet)) has COMPAR
 leadingTerm : % -> Record(k: IndexedExponents(OrderedVariableList(VarSet)),c: R) if IndexedExponents(OrderedVariableList(VarSet)) has COMPAR
 leading_exponent_vector : % -> JLVector(JLObjInt64)
 linearExtend : ((IndexedExponents(OrderedVariableList(VarSet)) -> R), %) -> R if R has COMRING
 listOfTerms : % -> List(Record(k: IndexedExponents(OrderedVariableList(VarSet)),c: R))
 mainVariable : % -> Union(OrderedVariableList(VarSet),"failed")
 mapExponents : ((IndexedExponents(OrderedVariableList(VarSet)) -> IndexedExponents(OrderedVariableList(VarSet))), %) -> %
 minimumDegree : % -> IndexedExponents(OrderedVariableList(VarSet))
 minimumDegree : (%, OrderedVariableList(VarSet)) -> NonNegativeInteger
 minimumDegree : (%, List(OrderedVariableList(VarSet))) -> List(NonNegativeInteger)
 monicDivide : (%, %, OrderedVariableList(VarSet)) -> Record(quotient: %,remainder: %) if R has RING
 monomial : (R, IndexedExponents(OrderedVariableList(VarSet))) -> %
 monomial : (%, OrderedVariableList(VarSet), NonNegativeInteger) -> %
 monomial : (%, List(OrderedVariableList(VarSet)), List(NonNegativeInteger)) -> %
 multivariate : (SparseUnivariatePolynomial(R), OrderedVariableList(VarSet)) -> %
 multivariate : (SparseUnivariatePolynomial(%), OrderedVariableList(VarSet)) -> %
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if R has PATMAB(FLOAT) and R has RING and OrderedVariableList(VarSet) has PATMAB(FLOAT)
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if R has PATMAB(INT) and R has RING and OrderedVariableList(VarSet) has PATMAB(INT)
 pomopo! : (%, R, IndexedExponents(OrderedVariableList(VarSet)), %) -> %
 primitiveMonomials : % -> List(%) if R has SRING
 primitivePart : (%, OrderedVariableList(VarSet)) -> % if R has GCDDOM
 reducedSystem : Matrix(%) -> Matrix(Integer) if R has LINEXP(INT) and R has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if R has LINEXP(INT) and R has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(R),vec: Vector(R)) if R has RING
 reducedSystem : Matrix(%) -> Matrix(R) if R has RING
 reductum : % -> % if IndexedExponents(OrderedVariableList(VarSet)) has COMPAR
 resultant : (%, %, OrderedVariableList(VarSet)) -> % if R has COMRING
 retract : % -> Fraction(Integer) if R has RETRACT(FRAC(INT))
 retract : % -> OrderedVariableList(VarSet) if R has SRING
 retractIfCan : % -> Union(Integer,"failed") if R has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if R has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(OrderedVariableList(VarSet),"failed") if R has SRING
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if R has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 support : % -> List(IndexedExponents(OrderedVariableList(VarSet)))
 totalDegree : (%, List(OrderedVariableList(VarSet))) -> NonNegativeInteger
 totalDegreeSorted : (%, List(OrderedVariableList(VarSet))) -> NonNegativeInteger
 unitCanonical : % -> % if % has ATVCWC and R has INTDOM or R has ENTIRER
 unitNormal : % -> Record(unit: %,canonical: %,associate: %) if % has ATVCWC and R has INTDOM or R has ENTIRER
 univariate : (%, OrderedVariableList(VarSet)) -> SparseUnivariatePolynomial(%)
```

## Operations added

### `coefficients`

- **Signature**: `(%)->JLVector(R)`

### `constant?`

- **Signature**: `(%)->Boolean`

### `constantCoefficient`

- **Signature**: `(%)->R`

### `degree`

- **Signature**: `(%,PositiveInteger)->NonNegativeInteger`

### `exactDivide`

- **Signature**: `(%,%)->%`

### `exponent_vectors`

- **Signature**: `(%)->JLVector(JLVector(JLObjInt64))`

### `factor`

- **Signature**: `(%)->NMFactored(%)`

### `internalOrdering`

- **Signature**: `()->JLObject`

### `jmp2nmp`

- **Signature**: `(MultivariatePolynomial(VarSet,R))->%`

### `leading_exponent_vector`

- **Signature**: `(%)->JLVector(JLObjInt64)`

### `leading_monomial`

- **Signature**: `(%)->%`

### `length`

- **Signature**: `(%)->NonNegativeInteger`

### `monomial?`

- **Signature**: `(%)->Boolean`

### `monomialRecursive?`

- **Signature**: `(%)->Boolean`

### `monomials`

- **Signature**: `(%)->JLVector(%)`

### `nemoMPPrint`

- **Signature**: `(Boolean)->Boolean`

### `primitiveMonomials`

- **Signature**: `(%)->JLVector(%)`

### `shiftLeft`

- **Signature**: `(%,NonNegativeInteger)->%`

### `shiftLeft!`

- **Signature**: `(%,NonNegativeInteger)->%`

### `shiftRight`

- **Signature**: `(%,NonNegativeInteger)->%`

### `shiftRight!`

- **Signature**: `(%,NonNegativeInteger)->%`

### `square?`

- **Signature**: `(%)->Boolean`

### `term?`

- **Signature**: `(%)->Boolean`

### `termRecursive?`

- **Signature**: `(%)->Boolean`

### `trailingCoefficient`

- **Signature**: `(%)->R`

### `univariate?`

- **Signature**: `(%)->Boolean`

### `vectorOfTerms`

- **Signature**: `(%)->JLVector(%)`
---
[Back to Index](../index.md)
