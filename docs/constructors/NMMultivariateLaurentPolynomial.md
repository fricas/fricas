# NMMultivariateLaurentPolynomial

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1432)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This type is a basic representation of sparse, distributed multivariate Laurent polynomials using the Julia package Nemo. It is parameterized by the coefficient ring. The coefficient ring may be non-commutative, but thevariables are assumed to commute. The monomial ordering used for internal storage can be one of :lex, :deglexor :degrevlex. For example: VarSet: List Symbol:=[x,y,z] V := OrderedVariableList(VarSet) -- eventually, for later use -- See SparseMultivariatePolynomial -- E := IndexedExponents V LPRing := NMLP(NINT,VarSet) x := x::V::LPRing y := y::V::LPRing z := z::V::LPRing p:=x*2+3*y^2+17*z^-13 p^7

**NMMultivariateLaurentPolynomial(R: NMRing,VarSet: List(Symbol)) is a domain constructor**  
**Abbreviation for NMMultivariateLaurentPolynomial is NMLP**  
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
 ?^? : (%, Integer) -> %                                ?^? : (%, PositiveInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           associator : (%, %, %) -> %
 characteristic : () -> NonNegativeInteger              coefficients : % -> JLVector(R)
 coefficients : % -> List(R)                            coerce : R -> %
 coerce : % -> %                                        coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               constantCoefficient : % -> R
 content : % -> R if R has GCDDOM                       convert : % -> String
 equal? : (%, %) -> Boolean                             eval : (%, OrderedVariableList(VarSet), R) -> %
 eval : (%, OrderedVariableList(VarSet), %) -> %        eval : (%, Equation(%)) -> % if R has SRING
 eval : (%, %, %) -> % if R has SRING                   exact? : % -> Boolean
 exactDivide : (%, %) -> %                              factor : % -> NMFactored(%)
 factor : % -> Factored(%) if R has PFECAT              gcd : List(%) -> % if R has GCDDOM
 gcd : (%, %) -> % if R has GCDDOM                      gen : Integer -> %
 gen? : % -> Boolean                                    gens : () -> JLVector(%)
 ground : % -> R                                        ground? : % -> Boolean
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
 leading_monomial : % -> %                              leading_term : % -> %
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     length : % -> NonNegativeInteger
 map : ((R -> R), %) -> %                               mapCoefficients : (JLObject, %) -> %
 missing? : % -> Boolean                                monomial? : % -> Boolean
 monomialRecursive? : % -> Boolean                      monomials : % -> JLVector(%)
 monomials : % -> List(%)                               mutable? : % -> Boolean
 nemoMLPPrint : Boolean -> Boolean                      nothing? : % -> Boolean
 numberOfMonomials : % -> NonNegativeInteger            numberOfVariables : () -> JLObject
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               prime? : % -> Boolean if R has PFECAT
 primitiveMonomials : % -> JLVector(%)                  primitivePart : % -> % if R has GCDDOM
 recip : % -> Union(%,"failed")                         retract : % -> Integer if R has RETRACT(INT)
 retract : % -> R                                       retractIfCan : % -> Union(R,"failed")
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 smaller? : (%, %) -> Boolean if R has COMPAR           squareFree : % -> Factored(%) if R has GCDDOM
 squareFreePart : % -> % if R has GCDDOM                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            symbols : () -> JLVector(JLObject)
 termRecursive? : % -> Boolean                          totalDegree : % -> NonNegativeInteger
 unit? : % -> Boolean                                   univariate : % -> SparseUnivariatePolynomial(R)
 variableIndex : % -> JLObjInt64                        variables : % -> List(OrderedVariableList(VarSet))
 vectorOfTerms : % -> JLVector(%)                       zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
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
 coerce : % -> NMMultivariateLaurentPolynomial(NMComplexField,VarSet)
 coerce : % -> NMMultivariateLaurentPolynomial(NMRealField,VarSet)
 coerce : % -> NMMultivariateLaurentPolynomial(NMFraction(NMInteger),VarSet)
 coerce : % -> NMMultivariateLaurentPolynomial(NMInteger,VarSet)
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

### `coefficients` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1545)\]

coefficients(p) returns a Julia vector of coefficients of p.

- **Signature**: `(%)->JLVector(R)`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1558)\]

coerce(lp) coerces lp.

- **Signature**: `(%)->NMMultivariateLaurentPolynomial(NMComplexField,VarSet)`
- **Signature**: `(%)->NMMultivariateLaurentPolynomial(NMFraction(NMInteger),VarSet)`
- **Signature**: `(%)->NMMultivariateLaurentPolynomial(NMInteger,VarSet)`
- **Signature**: `(%)->NMMultivariateLaurentPolynomial(NMRealField,VarSet)`

### `constantCoefficient` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1530)\]

constantCoefficient(p) returns the constant of p.

- **Signature**: `(%)->R`

### `degree` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1518)\]

degree(p,i) returns the degree of the i-th variable.

- **Signature**: `(%,PositiveInteger)->NonNegativeInteger`

### `exactDivide` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1511)\]

exactDivide(p,q) divides p by q if the division is exact.

- **Signature**: `(%,%)->%`

### `exponent_vectors` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1549)\]

exponent_vectors(lp) returns the exponent vectors.

- **Signature**: `(%)->JLVector(JLVector(JLObjInt64))`

### `factor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1516)\]

factor(p) returns the factorization of p.

- **Signature**: `(%)->NMFactored(%)`

### `gen` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1538)\]

gen(i) returns the i-th generator.

- **Signature**: `(Integer)->%`

### `gen?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1540)\]

gen?(lp) checks whether or not lp is a generator.

- **Signature**: `(%)->Boolean`

### `gens` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1536)\]

gens() returns a vector of the generators (variables).

- **Signature**: `()->JLVector(%)`

### `jmp2nmp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1513)\]

jmp2nmp(p) converts the multivariate polynomial p to a Nemo multivariate polynomial.

- **Signature**: `(MultivariatePolynomial(VarSet,R))->%`

### `leading_exponent_vector` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1555)\]

leading_exponent_vector(lp) returns the of leading exponent.

- **Signature**: `(%)->JLVector(JLObjInt64)`

### `leading_monomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1551)\]

leading_monomial(lp) returns the leading monomial.

- **Signature**: `(%)->%`

### `leading_term` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1553)\]

leading_term(lp) returns the leading term.

- **Signature**: `(%)->%`

### `length` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1520)\]

length(p) retruns the number of terms of p.

- **Signature**: `(%)->NonNegativeInteger`

### `mapCoefficients` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1571)\]

mapCoefficients(jmap, p) applies the Julia map jmap to the coefficients of p an returns the modified Laurent polynomial. Use coerce to change the base ring instead of mapCoefficients.

- **Signature**: `(JLObject,%)->%`

### `monomialRecursive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1480)\]

monomialRecursive?(p) checks whether or not p is monomial recurisvely (all scalar types).

- **Signature**: `(%)->Boolean`

### `monomials` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1522)\]

monomials(p) returns the list of non-zero monomials of p includind its coefficients.

- **Signature**: `(%)->JLVector(%)`

### `nemoMLPPrint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1581)\]

nemoMLPPrint(bool) set the output mode. If set to false, use the FriCAS output mode if possible. Otherwise use the Nemo one (default).

- **Signature**: `(Boolean)->Boolean`

### `numberOfVariables` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1534)\]

numberOfVariables() returns the number of variables.

- **Signature**: `()->JLObject`

### `primitiveMonomials` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1525)\]

primitiveMonomials(p) returns the list of non-zero primitive monomials of p, i.e. not including its coefficients.

- **Signature**: `(%)->JLVector(%)`

### `symbols` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1532)\]

symbols() returns a Julia vector of variables (Julia symbols).

- **Signature**: `()->JLVector(JLObject)`

### `termRecursive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1485)\]

termRecursive?(p) checks whether or not p as one term polynomial recursively on all scalar types.

- **Signature**: `(%)->Boolean`

### `variableIndex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1542)\]

variableIndex(var) returns the index of the variable var. Julia raises an error if var is not

- **Signature**: `(%)->JLObjInt64`

### `vectorOfTerms` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L1547)\]

terms(lp) returns the vector of terms.

- **Signature**: `(%)->JLVector(%)`
---
[Back to Index](../index.md)
