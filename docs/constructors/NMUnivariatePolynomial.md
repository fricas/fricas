# NMUnivariatePolynomial

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L9)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

Univariate polynomial domain using the Nemo Julia package Author: G. Vanuxem Date Created: March, 2024 Nestedpolynomials Fatemans's benchmark: 

**Example**:
```fricas
R:=NUP(NINT,'x)
```

**Example**:
```fricas
x:=x::R
```

**Example**:
```fricas
RR := NUP(R,'y)
```

**Example**:
```fricas
y:=y::RR
```

**Example**:
```fricas
RRR := NUP(RR,'z)
```

**Example**:
```fricas
z:=z::RRR
```

**Example**:
```fricas
T:=NUP(RRR,'t)
```

**Example**:
```fricas
t:=t::T
```

**Example**:
```fricas
f := 1 + x + y + z + t
```

**Example**:
```fricas
p := f^30;
```

**Example**:
```fricas
)set message time on
```

**Example**:
```fricas
p*(p + 1);
```

**NMUnivariatePolynomial(R: NMRing,x: Symbol) is a domain constructor**  
**Abbreviation for NMUnivariatePolynomial is NUP**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, PositiveInteger) -> %                        ?*? : (%, NonNegativeInteger) -> %
 ?*? : (%, R) -> %                                      ?*? : (R, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (%, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (NMInteger, %) -> JLObject                       ?+? : (%, %) -> %
 ?-? : (%, %) -> %                                      -? : % -> %
 ?/? : (%, R) -> % if R has FIELD                       0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 D : (%, (R -> R)) -> % if R has RING                   D : % -> % if R has RING
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 coefficient : (%, NonNegativeInteger) -> R             coefficients : % -> List(R)
 coerce : Variable(x) -> %                              coerce : R -> %
 coerce : % -> %                                        coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               compose : (%, %) -> %
 constant? : % -> Boolean                               constantCoefficient : % -> R
 content : % -> R if R has GCDDOM                       convert : % -> String
 degree : % -> NonNegativeInteger                       differentiate : % -> % if R has RING
 discriminant : % -> R if R has COMRING                 elt : (%, R) -> R
 elt : (%, %) -> %                                      elt : (Fraction(%), R) -> R if R has FIELD
 equal? : (%, %) -> Boolean                             eval : (%, SingletonAsOrderedSet, R) -> %
 eval : (%, SingletonAsOrderedSet, %) -> %              eval : (%, Equation(%)) -> % if R has SRING
 eval : (%, %, %) -> % if R has SRING                   exact? : % -> Boolean
 exactDivide : (%, %) -> %                              factor : % -> NMFactored(%)
 factor : % -> Factored(%) if R has PFECAT              gcd : List(%) -> % if R has GCDDOM
 gcd : (%, %) -> % if R has GCDDOM                      ground : % -> R
 ground? : % -> Boolean                                 init : () -> % if R has STEP
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
 jnup : UnivariatePolynomial(x,R) -> %                  latex : % -> String
 lcm : List(%) -> % if R has GCDDOM                     lcm : (%, %) -> % if R has GCDDOM
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     length : % -> NonNegativeInteger
 makeSUP : % -> SparseUnivariatePolynomial(R)           map : ((R -> R), %) -> %
 minimumDegree : % -> NonNegativeInteger                missing? : % -> Boolean
 monic? : % -> Boolean                                  monomial : (R, NonNegativeInteger) -> %
 monomial? : % -> Boolean                               monomialRecursive? : % -> Boolean
 monomials : % -> List(%)                               mullow : (%, %, NonNegativeInteger) -> %
 multiplyExponents : (%, NonNegativeInteger) -> %       mutable? : % -> Boolean
 nothing? : % -> Boolean                                numberOfMonomials : % -> NonNegativeInteger
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 plenaryPower : (%, PositiveInteger) -> %               pomopo! : (%, R, NonNegativeInteger, %) -> %
 prime? : % -> Boolean if R has PFECAT                  primitivePart : % -> % if R has GCDDOM
 pseudoQuotient : (%, %) -> % if R has INTDOM           pseudoRemainder : (%, %) -> % if R has RING
 ?quo? : (%, %) -> %                                    recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    resultant : (%, %) -> R if R has COMRING
 retract : % -> Integer if R has RETRACT(INT)           retract : % -> R
 retractIfCan : % -> Union(R,"failed")                  reverse : (%, NonNegativeInteger) -> %
 reverse : % -> %                                       rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       shiftLeft : (%, NonNegativeInteger) -> %
 shiftLeft! : (%, NonNegativeInteger) -> %              shiftRight : (%, NonNegativeInteger) -> %
 shiftRight! : (%, NonNegativeInteger) -> %             sizeLess? : (%, %) -> Boolean if R has FIELD
 smaller? : (%, %) -> Boolean if R has COMPAR           square? : % -> Boolean
 squareFree : % -> Factored(%) if R has GCDDOM          squareFreePart : % -> % if R has GCDDOM
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 support : % -> List(NonNegativeInteger)                term? : % -> Boolean
 termRecursive? : % -> Boolean                          totalDegree : % -> NonNegativeInteger
 trailingCoefficient : % -> R                           truncate : (%, NonNegativeInteger) -> %
 unit? : % -> Boolean                                   univariate : % -> SparseUnivariatePolynomial(R)
 unmakeSUP : SparseUnivariatePolynomial(R) -> %         unvectorise : Vector(R) -> %
 variables : % -> List(SingletonAsOrderedSet)           vectorise : (%, NonNegativeInteger) -> Vector(R)
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (Fraction(Integer), %) -> % if R has ALGEBRA(FRAC(INT)) or R has ALGEBRA(NFRAC(NINT))
 ?*? : (%, Fraction(Integer)) -> % if R has ALGEBRA(FRAC(INT)) or R has ALGEBRA(NFRAC(NINT))
 ?*? : (%, Integer) -> % if R has LINEXP(INT) and R has RING
 ?+? : (Fraction(Integer), %) -> % if R has ALGEBRA(NFRAC(NINT))
 ?+? : (%, Fraction(Integer)) -> % if R has ALGEBRA(NFRAC(NINT))
 D : (%, List(SingletonAsOrderedSet), List(NonNegativeInteger)) -> % if R has RING
 D : (%, SingletonAsOrderedSet, NonNegativeInteger) -> % if R has RING
 D : (%, List(SingletonAsOrderedSet)) -> % if R has RING
 D : (%, SingletonAsOrderedSet) -> % if R has RING
 D : (%, Symbol) -> % if R has PDRING(SYMBOL) and R has RING
 D : (%, List(Symbol)) -> % if R has PDRING(SYMBOL) and R has RING
 D : (%, Symbol, NonNegativeInteger) -> % if R has PDRING(SYMBOL) and R has RING
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if R has PDRING(SYMBOL) and R has RING
 D : (%, (R -> R), NonNegativeInteger) -> % if R has RING
 D : (%, NonNegativeInteger) -> % if R has RING
 associates? : (%, %) -> Boolean if % has ATVCWC and R has INTDOM or R has ENTIRER
 binomThmExpt : (%, %, NonNegativeInteger) -> % if % has COMRING
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and R has PFECAT
 coefficient : (%, SingletonAsOrderedSet, NonNegativeInteger) -> %
 coefficient : (%, List(SingletonAsOrderedSet), List(NonNegativeInteger)) -> %
 coerce : Fraction(Integer) -> % if R has ALGEBRA(FRAC(INT)) or R has RETRACT(FRAC(INT))
 coerce : SingletonAsOrderedSet -> % if R has SRING
 composite : (%, %) -> Union(%,"failed") if R has INTDOM
 composite : (Fraction(%), %) -> Union(Fraction(%),"failed") if R has INTDOM
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and R has PFECAT
 construct : List(Record(k: NonNegativeInteger,c: R)) -> %
 constructOrdered : List(Record(k: NonNegativeInteger,c: R)) -> % if NonNegativeInteger has COMPAR
 content : (%, SingletonAsOrderedSet) -> % if R has GCDDOM
 convert : % -> Pattern(Float) if R has KONVERT(PATTERN(FLOAT)) and R has RING and SingletonAsOrderedSet has KONVERT(PATTERN(FLOAT))
 convert : % -> Pattern(Integer) if R has KONVERT(PATTERN(INT)) and R has RING and SingletonAsOrderedSet has KONVERT(PATTERN(INT))
 convert : % -> InputForm if R has KONVERT(INFORM) and SingletonAsOrderedSet has KONVERT(INFORM)
 cosMinimalPolynomial : (NonNegativeInteger, %) -> % if R has INS
 cyclotomicPolynomial : (NonNegativeInteger, %) -> % if R has INS
 degree : (%, SingletonAsOrderedSet) -> NonNegativeInteger
 degree : (%, List(SingletonAsOrderedSet)) -> List(NonNegativeInteger)
 differentiate : (%, List(SingletonAsOrderedSet), List(NonNegativeInteger)) -> % if R has RING
 differentiate : (%, SingletonAsOrderedSet, NonNegativeInteger) -> % if R has RING
 differentiate : (%, List(SingletonAsOrderedSet)) -> % if R has RING
 differentiate : (%, SingletonAsOrderedSet) -> % if R has RING
 differentiate : (%, (R -> R), %) -> % if R has RING
 differentiate : (%, Symbol) -> % if R has PDRING(SYMBOL) and R has RING
 differentiate : (%, List(Symbol)) -> % if R has PDRING(SYMBOL) and R has RING
 differentiate : (%, Symbol, NonNegativeInteger) -> % if R has PDRING(SYMBOL) and R has RING
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if R has PDRING(SYMBOL) and R has RING
 differentiate : (%, (R -> R), NonNegativeInteger) -> % if R has RING
 differentiate : (%, (R -> R)) -> % if R has RING
 differentiate : (%, NonNegativeInteger) -> % if R has RING
 discriminant : (%, SingletonAsOrderedSet) -> % if R has COMRING
 divide : (%, %) -> Record(quotient: %,remainder: %)
 divideExponents : (%, NonNegativeInteger) -> Union(%,"failed")
 elt : (Fraction(%), Fraction(%)) -> Fraction(%) if R has INTDOM
 elt : (%, Fraction(%)) -> Fraction(%) if R has INTDOM
 etaQExp : (Integer, PositiveInteger, %) -> % if R has INS
 euclideanSize : % -> NonNegativeInteger if R has FIELD
 eval : (%, List(SingletonAsOrderedSet), List(R)) -> %
 eval : (%, List(SingletonAsOrderedSet), List(%)) -> %
 eval : (%, List(Equation(%))) -> % if R has SRING
 eval : (%, List(%), List(%)) -> % if R has SRING
 expressIdealMember : (List(%), %) -> Union(List(%),"failed") if R has FIELD
 ?exquo? : (%, R) -> Union(%,"failed") if R has ENTIRER
 ?exquo? : (%, %) -> Union(%,"failed") if % has ATVCWC and R has INTDOM or R has ENTIRER
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed") if R has FIELD
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %) if R has FIELD
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 fmecg : (%, NonNegativeInteger, R, %) -> % if R has RING
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%) if R has GCDDOM
 hash : % -> SingleInteger if R has HASHABL and SingletonAsOrderedSet has HASHABL
 hashUpdate! : (HashState, %) -> HashState if R has HASHABL and SingletonAsOrderedSet has HASHABL
 integrate : % -> % if R has ALGEBRA(FRAC(INT))
 isExpt : % -> Union(Record(var: SingletonAsOrderedSet,exponent: NonNegativeInteger),"failed") if R has SRING
 isTimes : % -> Union(List(%),"failed") if R has SRING
 karatsubaDivide : (%, NonNegativeInteger) -> Record(quotient: %,remainder: %) if R has RING
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %) if R has GCDDOM
 leadingCoefficient : % -> R if NonNegativeInteger has COMPAR
 leadingMonomial : % -> % if NonNegativeInteger has COMPAR
 leadingSupport : % -> NonNegativeInteger if NonNegativeInteger has COMPAR
 leadingTerm : % -> Record(k: NonNegativeInteger,c: R) if NonNegativeInteger has COMPAR
 linearExtend : ((NonNegativeInteger -> R), %) -> R if R has COMRING
 listOfTerms : % -> List(Record(k: NonNegativeInteger,c: R))
 mainVariable : % -> Union(SingletonAsOrderedSet,"failed")
 mapExponents : ((NonNegativeInteger -> NonNegativeInteger), %) -> %
 minimalPolynomial : NMAlgebraicNumber -> % if R has ALGEBRA(NFRAC(NINT)) or R has INS
 minimumDegree : (%, SingletonAsOrderedSet) -> NonNegativeInteger
 minimumDegree : (%, List(SingletonAsOrderedSet)) -> List(NonNegativeInteger)
 monicDivide : (%, %, SingletonAsOrderedSet) -> Record(quotient: %,remainder: %) if R has RING
 monicDivide : (%, %) -> Record(quotient: %,remainder: %) if R has RING
 monomial : (%, SingletonAsOrderedSet, NonNegativeInteger) -> %
 monomial : (%, List(SingletonAsOrderedSet), List(NonNegativeInteger)) -> %
 multiEuclidean : (List(%), %) -> Union(List(%),"failed") if R has FIELD
 multivariate : (SparseUnivariatePolynomial(R), SingletonAsOrderedSet) -> %
 multivariate : (SparseUnivariatePolynomial(%), SingletonAsOrderedSet) -> %
 nextItem : % -> Union(%,"failed") if R has STEP
 order : (%, %) -> NonNegativeInteger if R has INTDOM
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if R has PATMAB(FLOAT) and R has RING and SingletonAsOrderedSet has PATMAB(FLOAT)
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if R has PATMAB(INT) and R has RING and SingletonAsOrderedSet has PATMAB(INT)
 primitiveMonomials : % -> List(%) if R has SRING
 primitivePart : (%, SingletonAsOrderedSet) -> % if R has GCDDOM
 principalIdeal : List(%) -> Record(coef: List(%),generator: %) if R has FIELD
 pseudoDivide : (%, %) -> Record(quotient: %,remainder: %)
 pseudoDivide : (%, %) -> Record(coef: R,quotient: %,remainder: %) if R has INTDOM
 reducedSystem : Matrix(%) -> Matrix(Integer) if R has LINEXP(INT) and R has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if R has LINEXP(INT) and R has RING
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(R),vec: Vector(R)) if R has RING
 reducedSystem : Matrix(%) -> Matrix(R) if R has RING
 reductum : % -> % if NonNegativeInteger has COMPAR
 resultant : (%, %, SingletonAsOrderedSet) -> % if R has COMRING
 retract : % -> Fraction(Integer) if R has RETRACT(FRAC(INT))
 retract : % -> SingletonAsOrderedSet if R has SRING
 retractIfCan : % -> Union(Integer,"failed") if R has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if R has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(SingletonAsOrderedSet,"failed") if R has SRING
 roots : % -> JLVector(NMAlgebraicNumber) if R has ALGEBRA(NFRAC(NINT)) or R has INS
 separate : (%, %) -> Record(primePart: %,commonPart: %) if R has GCDDOM
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if R has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if R has PFECAT
 subResultantGcd : (%, %) -> % if R has INTDOM
 swinnertonDyerPolynomial : (NonNegativeInteger, %) -> % if R has INS
 thetaQExp : (Integer, PositiveInteger, %) -> % if R has INS
 totalDegree : (%, List(SingletonAsOrderedSet)) -> NonNegativeInteger
 totalDegreeSorted : (%, List(SingletonAsOrderedSet)) -> NonNegativeInteger
 unitCanonical : % -> % if % has ATVCWC and R has INTDOM or R has ENTIRER
 unitNormal : % -> Record(unit: %,canonical: %,associate: %) if % has ATVCWC and R has INTDOM or R has ENTIRER
 univariate : (%, SingletonAsOrderedSet) -> SparseUnivariatePolynomial(%)
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L148)\]

coerce(x) converts the variable x to a Nemo univariate polynomial.

- **Signature**: `(Variable(x))->%`

### `compose` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L115)\]

compose(p, q) returns the composition p(q(x)).

- **Signature**: `(%,%)->%`

### `constant?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L38)\]

constant?(p) checks whether or not p is a constant polynomial.

- **Signature**: `(%)->Boolean`

### `constantCoefficient` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L125)\]

constantCoefficient(p) returns the constant of p.

- **Signature**: `(%)->R`

### `cosMinimalPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L74)\]

cosMinimalPolynomial(n,p) returns the minimal polynomial of 2*cos(2*π/n).

- **Signature**: `(NonNegativeInteger,%)->%`

### `cyclotomicPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L63)\]

cyclotomicPolynomial(n,p) returns the n-th cyclotomic polynomial Φn. For example:

**Example**:
```fricas
x:=x::NUP(NINT,'x)
```

**Example**:
```fricas
cyclotomicPolynomial(15,x)
```

- **Signature**: `(NonNegativeInteger,%)->%`

### `divide` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L99)\]

divide(p,q) returns a record (quotient, remainder) of the the Euclidean division of p by q.

- **Signature**: `(%,%)->Record(quotient:%,remainder:%)`

### `etaQExp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L80)\]

q^(1/24)) raised to the power r, (q^(−1/24)*η(q))^r. etaQExp(r,n,p) returns the q-expansion to length n of the Dedekind eta function (without the leading factor

- **Signature**: `(Integer,PositiveInteger,%)->%`

### `exactDivide` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L107)\]

exactDivide(p,q) divides p by q if the division is exact.

- **Signature**: `(%,%)->%`

### `factor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L117)\]

factor(p) returns the factorization of p using the NMFactored domain.

- **Signature**: `(%)->NMFactored(%)`

### `jnup` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L112)\]

jnup(p) converts the univariate polynomial p to a Nemo univariate polynomial.

- **Signature**: `(UnivariatePolynomial(x,R))->%`

### `length` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L120)\]

length(p) returns the number of coefficients in its dense representation. It includes zero coefficients.

- **Signature**: `(%)->NonNegativeInteger`

### `minimalPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L56)\]

minimalPolynomial(algn) returns the minimal polynomial of algn. Convenience function.

- **Signature**: `(NMAlgebraicNumber)->%`

### `monic?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L50)\]

monic?(p) checks whether or not p monic.

- **Signature**: `(%)->Boolean`

### `monomial?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L40)\]

monomial?(p) checks whether or not p is a monomial.

- **Signature**: `(%)->Boolean`

### `monomialRecursive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L42)\]

monomialRecursive?(p) checks whether or not p is monomial recursively (all scalar types).

- **Signature**: `(%)->Boolean`

### `mullow` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L130)\]

mullow(p1,p2,n) is the truncated multiplication of p1 and p2 by n.

- **Signature**: `(%,%,NonNegativeInteger)->%`

### `pseudoDivide` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L109)\]

pseudoDivide(p1,p2) returns pseudo-quotient and pseudo-remainder of the pseudo-division of p1 by p2.

- **Signature**: `(%,%)->Record(quotient:%,remainder:%)`

### `reverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L132)\]

reverse the coefficients of p (the leading becomes the trailing) and normalise the resulting polynomial.

- **Signature**: `(%)->%`

reverse the coefficients of p (the leading becomes the trailing) and normalise the resulting polynomial. Adjust the length to n so the resulting polynomial is truncated or padded with zeroes before the leading term if necessary

- **Signature**: `(%,NonNegativeInteger)->%`

### `roots` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L59)\]

roots(p) returns the roots of p. Convenience function.

- **Signature**: `(%)->JLVector(NMAlgebraicNumber)`

### `shiftLeft` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L140)\]

shiftLeft(p) shifts p left.

- **Signature**: `(%,NonNegativeInteger)->%`

### `shiftLeft!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L144)\]

shiftLeft(p) left shift p.

- **Signature**: `(%,NonNegativeInteger)->%`

### `shiftRight` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L142)\]

shiftRight(p) shifts p right.

- **Signature**: `(%,NonNegativeInteger)->%`

### `shiftRight!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L146)\]

shiftRight(p) right shift p.

- **Signature**: `(%,NonNegativeInteger)->%`

### `square?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L52)\]

square?(p) checks whether or not p is a perfect square.

- **Signature**: `(%)->Boolean`

### `swinnertonDyerPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L69)\]

swinnertonDyerPolynomial(n, p) returns the Swinnerton-Dyer polynomial Sn. 

**Example**:
```fricas
x:=x::NUP(NINT,'x)
```

Examp le: swinnertonDyerPolynomial(4,x)

- **Signature**: `(NonNegativeInteger,%)->%`

### `term?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L45)\]

term?(p) checks whether or not p is a one term polynomial.

- **Signature**: `(%)->Boolean`

### `termRecursive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L47)\]

termRecursive?(p) checks whether or not p as one term, recursively (all scalar types).

- **Signature**: `(%)->Boolean`

### `thetaQExp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L77)\]

thetaQExp(r,n,p) returns the q-expansion to length n of the Jacobi theta function raised to the power r, ϑ(q)^r.

- **Signature**: `(Integer,PositiveInteger,%)->%`

### `trailingCoefficient` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L123)\]

trailingCoefficient(p) returns the trailing coefficient of p.

- **Signature**: `(%)->R`

### `truncate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnpoly.spad#L127)\]

truncate(p, n) returns p truncated to n terms. It's the remainder of the division by x^n.

- **Signature**: `(%,NonNegativeInteger)->%`
---
[Back to Index](../index.md)
