# NMAcbField

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L351)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

NMAcbField implements fixed precision complex ball arithmetic using the Nemo Julia package - based on the Arblibrary.

**NMAcbField(p: PositiveInteger) is a domain constructor**  
**Abbreviation for NMAcbField is NACB**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, Integer) -> %                                ?*? : (NMArbField(p), %) -> %
 ?*? : (%, NMArbField(p)) -> %                          ?*? : (PositiveInteger, %) -> %
 ?*? : (%, %) -> %                                      ?*? : (Integer, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      ?-? : (%, %) -> %
 -? : % -> %                                            ?/? : (Integer, %) -> %
 ?/? : (%, %) -> % if NMArbField(p) has FIELD           0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 D : % -> %                                             D : (%, NonNegativeInteger) -> %
 D : (%, (NMArbField(p) -> NMArbField(p))) -> %         Gamma : (%, %) -> %
 Gamma : % -> %                                         ?^? : (%, PositiveInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     abs : % -> % if NMArbField(p) has RNS
 abs2 : % -> %                                          accuracyBits : % -> JLInt64
 acos : % -> % if NMArbField(p) has TRANFUN             acosh : % -> % if NMArbField(p) has TRANFUN
 acot : % -> % if NMArbField(p) has TRANFUN             acoth : % -> % if NMArbField(p) has TRANFUN
 acsc : % -> % if NMArbField(p) has TRANFUN             acsch : % -> % if NMArbField(p) has TRANFUN
 airyAi : R -> R                                        airyAiPrime : R -> R
 airyBi : R -> R                                        airyBiPrime : R -> R
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 asec : % -> % if NMArbField(p) has TRANFUN             asech : % -> % if NMArbField(p) has TRANFUN
 asin : % -> % if NMArbField(p) has TRANFUN             asinh : % -> % if NMArbField(p) has TRANFUN
 associator : (%, %, %) -> %                            atan : % -> % if NMArbField(p) has TRANFUN
 atanh : % -> % if NMArbField(p) has TRANFUN            basis : () -> Vector(%)
 besselI : (%, %) -> %                                  besselJ : (%, %) -> %
 besselK : (%, %) -> %                                  besselY : (%, %) -> %
 bits : % -> JLInt64                                    characteristic : () -> NonNegativeInteger
 coerce : Float -> %                                    coerce : Complex(Integer) -> %
 coerce : NMArbField(p) -> %                            coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 complex : (NMArbField(p), NMArbField(p)) -> %          conjugate : % -> %
 contains? : (%, NMFraction(NMInteger)) -> Boolean      contains? : (%, NMInteger) -> Boolean
 contains? : (%, %) -> Boolean                          containsZero? : % -> Boolean
 convert : Vector(NMArbField(p)) -> %                   convert : % -> Vector(NMArbField(p))
 convert : % -> String                                  coordinates : Vector(%) -> Matrix(NMArbField(p))
 coordinates : % -> Vector(NMArbField(p))               cos : % -> % if NMArbField(p) has TRANFUN
 cosh : % -> % if NMArbField(p) has TRANFUN             cot : % -> % if NMArbField(p) has TRANFUN
 coth : % -> % if NMArbField(p) has TRANFUN             csc : % -> % if NMArbField(p) has TRANFUN
 csch : % -> % if NMArbField(p) has TRANFUN             differentiate : % -> %
 differentiate : (%, NonNegativeInteger) -> %           discriminant : Vector(%) -> NMArbField(p)
 discriminant : () -> NMArbField(p)                     ellipticE : % -> %
 ellipticK : % -> %                                     equal? : (%, %) -> Boolean
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 exp : () -> %                                          exp : % -> % if NMArbField(p) has TRANFUN
 exp1 : () -> %                                         expm1 : % -> %
 finite? : % -> Boolean                                 generator : () -> %
 hurwitzZeta : (%, %) -> %                              hypergeometric1F1 : (%, %, %) -> %
 hypergeometric1F1Regularized : (%, %, %) -> %          hypergeometricU : (%, %, %) -> %
 imag : % -> NMArbField(p)                              imaginary : () -> %
 init : () -> % if NMArbField(p) has FFIELDC            integer? : % -> Boolean
 inv : % -> % if NMArbField(p) has FIELD                jlAbout : % -> Void
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
 jncb : (Float, Float) -> %                             jncb : Float -> %
 jncb : (Integer, Integer) -> %                         jncb : Integer -> %
 jncb : (String, String) -> %                           jncb : String -> %
 jncb : NMExactCalciumField -> %                        jncb : NMAlgebraicNumber -> %
 latex : % -> String                                    ldexp : (%, NMInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftPower : (%, NonNegativeInteger) -> %
 leftRecip : % -> Union(%,"failed")                     log : % -> % if NMArbField(p) has TRANFUN
 log1p : % -> %                                         map : ((NMArbField(p) -> NMArbField(p)), %) -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 norm : % -> NMArbField(p)                              nothing? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 overlaps? : (%, %) -> Boolean                          pi : () -> %
 plenaryPower : (%, PositiveInteger) -> %               polyLog : (%, %) -> %
 polygamma : (%, %) -> %                                precision : () -> PositiveInteger
 random : () -> % if NMArbField(p) has FINITE           randtest : JLSymbol -> %
 rank : () -> PositiveInteger                           real : % -> NMArbField(p)
 recip : % -> Union(%,"failed")                         reducedSystem : Matrix(%) -> Matrix(NMArbField(p))
 regularRepresentation : % -> Matrix(NMArbField(p))     represents : Vector(NMArbField(p)) -> %
 retract : % -> NMArbField(p)                           retractIfCan : % -> Union(NMArbField(p),"failed")
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    rootOfUnity : NonNegativeInteger -> %
 sample : () -> %                                       sec : % -> % if NMArbField(p) has TRANFUN
 sech : % -> % if NMArbField(p) has TRANFUN             sin : % -> % if NMArbField(p) has TRANFUN
 sinh : % -> % if NMArbField(p) has TRANFUN             string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tan : % -> % if NMArbField(p) has TRANFUN
 tanh : % -> % if NMArbField(p) has TRANFUN             trace : % -> NMArbField(p)
 traceMatrix : Vector(%) -> Matrix(NMArbField(p))       traceMatrix : () -> Matrix(NMArbField(p))
 trim : % -> %                                          uniqueInteger : % -> Union(NMInteger,"failed")
 unit? : % -> Boolean                                   urand01 : () -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ and NMArbField(p) has FIELD or % has CHARZ and NMArbField(p) has FFIELDC
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ and NMArbField(p) has FIELD or % has CHARZ and NMArbField(p) has FFIELDC
 D : (%, Symbol) -> % if NMArbField(p) has PDRING(SYMBOL)
 D : (%, List(Symbol)) -> % if NMArbField(p) has PDRING(SYMBOL)
 D : (%, Symbol, NonNegativeInteger) -> % if NMArbField(p) has PDRING(SYMBOL)
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMArbField(p) has PDRING(SYMBOL)
 D : (%, (NMArbField(p) -> NMArbField(p)), NonNegativeInteger) -> %
 ?^? : (%, Integer) -> % if NMArbField(p) has FIELD
 ?^? : (%, Fraction(Integer)) -> % if NMArbField(p) has RADCAT and NMArbField(p) has TRANFUN
 ?^? : (%, %) -> % if NMArbField(p) has TRANFUN
 argument : % -> NMArbField(p) if NMArbField(p) has TRANFUN
 associates? : (%, %) -> Boolean if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has INTDOM
 characteristicPolynomial : % -> SparseUnivariatePolynomial(NMArbField(p))
 charthRoot : % -> % if NMArbField(p) has FFIELDC
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or % has CHARNZ and NMArbField(p) has FFIELDC
 coerce : Fraction(Integer) -> % if % has CHARZ and NMArbField(p) has FIELD or % has CHARZ and NMArbField(p) has FFIELDC or NMArbField(p) has RETRACT(FRAC(INT))
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or % has CHARNZ and NMArbField(p) has FFIELDC
 convert : % -> SparseUnivariatePolynomial(NMArbField(p))
 convert : SparseUnivariatePolynomial(NMArbField(p)) -> %
 convert : % -> Pattern(Integer) if NMArbField(p) has KONVERT(PATTERN(INT))
 convert : % -> Pattern(Float) if NMArbField(p) has KONVERT(PATTERN(FLOAT))
 convert : % -> Complex(Float) if NMArbField(p) has REAL
 convert : % -> Complex(DoubleFloat) if NMArbField(p) has REAL
 convert : % -> InputForm if NMArbField(p) has KONVERT(INFORM)
 coordinates : (%, Vector(%)) -> Vector(NMArbField(p))
 coordinates : (Vector(%), Vector(%)) -> Matrix(NMArbField(p))
 createPrimitiveElement : () -> % if NMArbField(p) has FFIELDC
 definingPolynomial : () -> SparseUnivariatePolynomial(NMArbField(p))
 derivationCoordinates : (Vector(%), (NMArbField(p) -> NMArbField(p))) -> Matrix(NMArbField(p)) if NMArbField(p) has FIELD
 differentiate : (%, Symbol) -> % if NMArbField(p) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if NMArbField(p) has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if NMArbField(p) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if NMArbField(p) has PDRING(SYMBOL)
 differentiate : (%, (NMArbField(p) -> NMArbField(p)), NonNegativeInteger) -> %
 differentiate : (%, (NMArbField(p) -> NMArbField(p))) -> %
 discreteLog : % -> NonNegativeInteger if NMArbField(p) has FFIELDC
 discreteLog : (%, %) -> Union(NonNegativeInteger,"failed") if NMArbField(p) has FFIELDC
 divide : (%, %) -> Record(quotient: %,remainder: %) if NMArbField(p) has FIELD or NMArbField(p) has INS
 elt : (%, NMArbField(p)) -> % if NMArbField(p) has ELTAB(NARB(p),NARB(p))
 enumerate : () -> List(%) if NMArbField(p) has FINITE
 euclideanSize : % -> NonNegativeInteger if NMArbField(p) has FIELD or NMArbField(p) has INS
 eval : (%, List(NMArbField(p)), List(NMArbField(p))) -> % if NMArbField(p) has EVALAB(NARB(p))
 eval : (%, NMArbField(p), NMArbField(p)) -> % if NMArbField(p) has EVALAB(NARB(p))
 eval : (%, Equation(NMArbField(p))) -> % if NMArbField(p) has EVALAB(NARB(p))
 eval : (%, List(Equation(NMArbField(p)))) -> % if NMArbField(p) has EVALAB(NARB(p))
 eval : (%, List(Symbol), List(NMArbField(p))) -> % if NMArbField(p) has IEVALAB(SYMBOL,NARB(p))
 eval : (%, Symbol, NMArbField(p)) -> % if NMArbField(p) has IEVALAB(SYMBOL,NARB(p))
 expressIdealMember : (List(%), %) -> Union(List(%),"failed") if NMArbField(p) has FIELD or NMArbField(p) has INS
 ?exquo? : (%, NMArbField(p)) -> Union(%,"failed") if NMArbField(p) has INTDOM
 ?exquo? : (%, %) -> Union(%,"failed") if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has INTDOM
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed") if NMArbField(p) has FIELD or NMArbField(p) has INS
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %) if NMArbField(p) has FIELD or NMArbField(p) has INS
 factor : % -> Factored(%) if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FFIELDC
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FFIELDC
 factorsOfCyclicGroupSize : () -> List(Record(factor: Integer,exponent: NonNegativeInteger)) if NMArbField(p) has FFIELDC
 gcd : List(%) -> % if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 gcd : (%, %) -> % if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%) if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 guess : (%, NonNegativeInteger) -> NMAlgebraicNumber
 hash : % -> SingleInteger if NMArbField(p) has HASHABL
 hashUpdate! : (HashState, %) -> HashState if NMArbField(p) has HASHABL
 index : PositiveInteger -> % if NMArbField(p) has FINITE
 lcm : List(%) -> % if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 lcm : (%, %) -> % if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %) if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 lift : % -> SparseUnivariatePolynomial(NMArbField(p))
 lookup : % -> PositiveInteger if NMArbField(p) has FINITE
 minimalPolynomial : % -> SparseUnivariatePolynomial(NMArbField(p)) if NMArbField(p) has FIELD
 multiEuclidean : (List(%), %) -> Union(List(%),"failed") if NMArbField(p) has FIELD or NMArbField(p) has INS
 nextItem : % -> Union(%,"failed") if NMArbField(p) has FFIELDC
 nthRoot : (%, Integer) -> % if NMArbField(p) has RADCAT and NMArbField(p) has TRANFUN
 order : % -> PositiveInteger if NMArbField(p) has FFIELDC
 order : % -> OnePointCompletion(PositiveInteger) if NMArbField(p) has FFIELDC
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if NMArbField(p) has PATMAB(INT)
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if NMArbField(p) has PATMAB(FLOAT)
 polarCoordinates : % -> Record(r: NMArbField(p),phi: NMArbField(p)) if NMArbField(p) has RNS and NMArbField(p) has TRANFUN
 prime? : % -> Boolean if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 primeFrobenius : (%, NonNegativeInteger) -> % if NMArbField(p) has FFIELDC
 primeFrobenius : % -> % if NMArbField(p) has FFIELDC
 primitive? : % -> Boolean if NMArbField(p) has FFIELDC
 primitiveElement : () -> % if NMArbField(p) has FFIELDC
 principalIdeal : List(%) -> Record(coef: List(%),generator: %) if NMArbField(p) has FIELD or NMArbField(p) has INS
 ?quo? : (%, %) -> % if NMArbField(p) has FIELD or NMArbField(p) has INS
 rational : % -> Fraction(Integer) if NMArbField(p) has INS
 rational? : % -> Boolean if NMArbField(p) has INS
 rationalIfCan : % -> Union(Fraction(Integer),"failed") if NMArbField(p) has INS
 reduce : SparseUnivariatePolynomial(NMArbField(p)) -> %
 reduce : Fraction(SparseUnivariatePolynomial(NMArbField(p))) -> Union(%,"failed") if NMArbField(p) has FIELD
 reducedSystem : Matrix(%) -> Matrix(Integer) if NMArbField(p) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if NMArbField(p) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(NMArbField(p)),vec: Vector(NMArbField(p)))
 regularRepresentation : (%, Vector(%)) -> Matrix(NMArbField(p))
 ?rem? : (%, %) -> % if NMArbField(p) has FIELD or NMArbField(p) has INS
 representationType : () -> Union("prime",polynomial,normal,cyclic) if NMArbField(p) has FFIELDC
 represents : (Vector(NMArbField(p)), Vector(%)) -> %
 retract : % -> Integer if NMArbField(p) has RETRACT(INT)
 retract : % -> Fraction(Integer) if NMArbField(p) has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Integer,"failed") if NMArbField(p) has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if NMArbField(p) has RETRACT(FRAC(INT))
 size : () -> NonNegativeInteger if NMArbField(p) has FINITE
 sizeLess? : (%, %) -> Boolean if NMArbField(p) has FIELD or NMArbField(p) has INS
 smaller? : (%, %) -> Boolean if NMArbField(p) has COMPAR
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FFIELDC
 sqrt : % -> % if NMArbField(p) has FIELD and NMArbField(p) has ORDRING and NMArbField(p) has RADCAT or NMArbField(p) has RADCAT and NMArbField(p) has TRANFUN
 squareFree : % -> Factored(%) if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 squareFreePart : % -> % if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FIELD or NMArbField(p) has INS
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has FFIELDC
 tableForDiscreteLogarithm : Integer -> Table(PositiveInteger,NonNegativeInteger) if NMArbField(p) has FFIELDC
 unitCanonical : % -> % if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has INTDOM
 unitNormal : % -> Record(unit: %,canonical: %,associate: %) if NMArbField(p) has EUCDOM and NMArbField(p) has PFECAT or NMArbField(p) has INTDOM
```

## Operations added

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L422)\]

Gamma(x) is the Euler Gamma function evaluated at x.

- **Signature**: `(%)->%`

Gamma(x,y) is the incomplete Gamma function.

- **Signature**: `(%,%)->%`

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L402)\]

abs2(x) returns the square of the absolute value of real part of x and imaginary part of x.

- **Signature**: `(%)->%`

### `accuracyBits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L466)\]

accuracyBits(x) returns the relative accuracy of x in bits.

- **Signature**: `(%)->JLInt64`

### `airyAi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L430)\]

airyAi(z) is the Airy function Ai(z).

- **Signature**: `(R)->R`

### `airyAiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L432)\]

airyAiPrime(z) is the derivative of the Airy function Ai(z).

- **Signature**: `(R)->R`

### `airyBi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L434)\]

airyBi(z) is the Airy function Bi(z).

- **Signature**: `(R)->R`

### `airyBiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L436)\]

airyBiPrime(z) is the derivative of the Airy function Bi(z).

- **Signature**: `(R)->R`

### `besselI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L442)\]

besselI(nu,x) is the Bessel I function.

- **Signature**: `(%,%)->%`

### `besselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L444)\]

besselJ(nu,x) is the Bessel J function.

- **Signature**: `(%,%)->%`

### `besselK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L446)\]

besselK(nu,x) is the Bessel K function.

- **Signature**: `(%,%)->%`

### `besselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L448)\]

besselY(nu,x) is the Bessel Y function.

- **Signature**: `(%,%)->%`

### `bits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L458)\]

bits(x) returns the bit length of the mantissa of x. For a result computed at prec bits of precision this canbe anywhere in the range 0 <= b <= prec. For example 0 has 0 bits, 0.75 has 2 bits, and 3.7 has 126 bits after rounding to prec = 128 (with the default rounding mode) because the two least significant bits are zero and thus get discarded. Source of documentation: flint-devel@googlegroups.com

- **Signature**: `(%)->JLInt64`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L480)\]

coerce(z) coerces z. Convenience function.

- **Signature**: `(Complex(Integer))->%`

coerce(r) coerces the floating point number r.

- **Signature**: `(Float)->%`

### `contains?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L378)\]

contains?(x,y) checks whether or not y is contained in x.

- **Signature**: `(%,%)->Boolean`
- **Signature**: `(%,NMFraction(NMInteger))->Boolean`
- **Signature**: `(%,NMInteger)->Boolean`

### `containsZero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L384)\]

containsZero?(x) checks whether or not 0 is contained in x.

- **Signature**: `(%)->Boolean`

### `ellipticE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L438)\]

ellipticE(x) is the complete elliptic integral of the second kind.

- **Signature**: `(%)->%`

### `ellipticK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L440)\]

ellipticK(x) is the complete elliptic integral of the first kind.

- **Signature**: `(%)->%`

### `exact?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L374)\]

exact?(x) checks whether x is exact i.e. with 0 radius.

- **Signature**: `(%)->Boolean`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L414)\]

exp() returns the NMAcbField ℯ (exp(1)).

- **Signature**: `()->%`

### `exp1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L416)\]

exp1() returns the NMAcbField ℯ (exp(1)).

- **Signature**: `()->%`

### `expm1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L409)\]

expm1(x) computes accurately e^x-1. It avoids the loss of precision involved in the direct evaluation of exp(x)-1 for small values of x.

- **Signature**: `(%)->%`

### `finite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L371)\]

finite?(x) checks whether or not x is finite, not an infinity for example.

- **Signature**: `(%)->Boolean`

### `guess` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L477)\]

guess(a, deg) returns the reconstructed algebraic number found if it succeeds. Up to degree deg.

- **Signature**: `(%,NonNegativeInteger)->NMAlgebraicNumber`

### `hurwitzZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L420)\]

hurwitzZeta(s,a) returns the Hurwitz zeta function of s and a.

- **Signature**: `(%,%)->%`

### `hypergeometric1F1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L450)\]

hypergeometric1F1(a,b,z) is the confluent hypergeometric function 1F1.

- **Signature**: `(%,%,%)->%`

### `hypergeometric1F1Regularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L453)\]

hypergeometric1F1Regularized(a,b,z) is the regularized confluent hypergeometric function 1F1.

- **Signature**: `(%,%,%)->%`

### `hypergeometricU` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L456)\]

hypergeometricU(a,b,x) is the confluent hypergeometric function U.

- **Signature**: `(%,%,%)->%`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L369)\]

integer?(x) checks whether or not x is an integer.

- **Signature**: `(%)->Boolean`

### `jncb` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L484)\]

jncb(r) returns r as a complex complex Arb ball.

- **Signature**: `(Float)->%`
- **Signature**: `(Integer)->%`

jncb(r,i) returns r and i as a complex Arb ball using real and imaginary part.

- **Signature**: `(Float,Float)->%`
- **Signature**: `(Integer,Integer)->%`

jncb(an) evaluates numerically an by converting it to a complex Arb field.

- **Signature**: `(NMAlgebraicNumber)->%`

jncb(necf) evaluates numerically necf by converting it to a complex Arb field.

- **Signature**: `(NMExactCalciumField)->%`

jncb(str) evaluates str to a complex Arb field.

- **Signature**: `(String)->%`

jncb(strr, stri) evaluates strr and stri to a complex Arb field. using real and imaginary part.

- **Signature**: `(String,String)->%`

### `ldexp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L418)\]

ldexp(x, n) returns x * 2^n.

- **Signature**: `(%,NMInteger)->%`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L412)\]

log1p(x) logarithm of 1+x computed accurately.

- **Signature**: `(%)->%`

### `overlaps?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L376)\]

overlaps?(x,y) checks whether or not any part of x and y balls overlaps.

- **Signature**: `(%,%)->Boolean`

### `pi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L386)\]

pi() returns the JLFloat representation of π.

- **Signature**: `()->%`

### `polyLog` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L428)\]

polyLog(x,y) returns the polyLog function applied to x and y.

- **Signature**: `(%,%)->%`

### `polygamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L426)\]

polygamma(x,y) returns the polygamma function applied to x and y.

- **Signature**: `(%,%)->%`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L475)\]

precision() returns precision in bits used.

- **Signature**: `()->PositiveInteger`

### `randtest` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L391)\]

randtest(randtype) returns a random number depending on the Julia symbol randtype. :urandom an uniformly distributed random number contained in the unit disk. To test corner cases: :randtest with separate real and imaginary parts, :randtest_precise with precise real and imaginary parts, :randtest_special with separate real and imaginary parts but eventually infinities and NaNs, :randtest_param with very high probability of returning integers or half-integers.

- **Signature**: `(JLSymbol)->%`

### `rootOfUnity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L473)\]

rootOfUnity(n)Return the root of unity exp(2*%pi*%i/n).

- **Signature**: `(NonNegativeInteger)->%`

### `trim` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L468)\]

trim(x) rounds off insignificant bits from the midpoint.

- **Signature**: `(%)->%`

### `uniqueInteger` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L470)\]

uniqueInteger(x) returns a NMInteger if there is a unique integer in the interval x, "failed" otherwise.

- **Signature**: `(%)->Union(NMInteger,"failed")`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L388)\]

urand01() returns an uniformly distributed random number contained in the unit disk.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
