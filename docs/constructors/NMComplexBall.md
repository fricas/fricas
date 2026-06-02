# NMComplexBall

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L668)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

convenience domain that reflects Nemo AcbField(256), i.e. without parameters.

**NMComplexBall is a domain constructor.**  
**Abbreviation for NMComplexBall is NCB**  
**This constructor is not exposed in this frame.**  
**173 names for 242 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, Integer) -> %                                ?*? : (%, NMArbField(256)) -> %
 ?*? : (Fraction(Integer), %) -> %                      ?*? : (Integer, %) -> %
 ?*? : (NMArbField(256), %) -> %                        ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?/? : (Integer, %) -> %                                ?=? : (%, %) -> Boolean
 D : % -> %                                             D : (%, NonNegativeInteger) -> %
 Gamma : % -> %                                         Gamma : (%, %) -> %
 ?^? : (%, %) -> %                                      ?^? : (%, Fraction(Integer)) -> %
 ?^? : (%, Integer) -> %                                ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        abs : % -> %
 abs2 : % -> %                                          accuracyBits : % -> JLInt64
 acos : % -> %                                          acosh : % -> %
 acot : % -> %                                          acoth : % -> %
 acsc : % -> %                                          acsch : % -> %
 airyAi : R -> R                                        airyAiPrime : R -> R
 airyBi : R -> R                                        airyBiPrime : R -> R
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 argument : % -> NMArbField(256)                        asec : % -> %
 asech : % -> %                                         asin : % -> %
 asinh : % -> %                                         associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            atan : % -> %
 atanh : % -> %                                         basis : () -> Vector(%)
 besselI : (%, %) -> %                                  besselJ : (%, %) -> %
 besselK : (%, %) -> %                                  besselY : (%, %) -> %
 bits : % -> JLInt64                                    characteristic : () -> NonNegativeInteger
 coerce : % -> %                                        coerce : Complex(Integer) -> %
 coerce : Float -> %                                    coerce : Fraction(Integer) -> %
 coerce : Integer -> %                                  coerce : NMArbField(256) -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               complex : (NMArbField(256), NMArbField(256)) -> %
 conjugate : % -> %                                     contains? : (%, %) -> Boolean
 contains? : (%, NMFraction(NMInteger)) -> Boolean      contains? : (%, NMInteger) -> Boolean
 containsZero? : % -> Boolean                           convert : Vector(NMArbField(256)) -> %
 convert : % -> Complex(DoubleFloat)                    convert : % -> Complex(Float)
 convert : % -> Pattern(Float)                          convert : % -> String
 convert : % -> Vector(NMArbField(256))                 coordinates : Vector(%) -> Matrix(NMArbField(256))
 coordinates : % -> Vector(NMArbField(256))             cos : % -> %
 cosh : % -> %                                          cot : % -> %
 coth : % -> %                                          csc : % -> %
 csch : % -> %                                          differentiate : % -> %
 differentiate : (%, NonNegativeInteger) -> %           discriminant : () -> NMArbField(256)
 discriminant : Vector(%) -> NMArbField(256)            ellipticE : % -> %
 ellipticK : % -> %                                     equal? : (%, %) -> Boolean
 euclideanSize : % -> NonNegativeInteger                exact? : % -> Boolean
 exactDivide : (%, %) -> %                              exp : () -> %
 exp : % -> %                                           exp1 : () -> %
 expm1 : % -> %                                         ?exquo? : (%, %) -> Union(%,"failed")
 factor : % -> Factored(%)                              finite? : % -> Boolean
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 generator : () -> %                                    hurwitzZeta : (%, %) -> %
 hypergeometric1F1 : (%, %, %) -> %                     hypergeometric1F1Regularized : (%, %, %) -> %
 hypergeometricU : (%, %, %) -> %                       imag : % -> NMArbField(256)
 imaginary : () -> %                                    integer? : % -> Boolean
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
 jncb : Float -> %                                      jncb : (Float, Float) -> %
 jncb : Integer -> %                                    jncb : (Integer, Integer) -> %
 jncb : NMAlgebraicNumber -> %                          jncb : NMExactCalciumField -> %
 jncb : String -> %                                     jncb : (String, String) -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     ldexp : (%, NMInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     log : % -> %
 log1p : % -> %                                         missing? : % -> Boolean
 mutable? : % -> Boolean                                norm : % -> NMArbField(256)
 nothing? : % -> Boolean                                nthRoot : (%, Integer) -> %
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 overlaps? : (%, %) -> Boolean                          pi : () -> %
 plenaryPower : (%, PositiveInteger) -> %               polyLog : (%, %) -> %
 polygamma : (%, %) -> %                                precision : () -> PositiveInteger
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 randtest : JLSymbol -> %                               rank : () -> PositiveInteger
 real : % -> NMArbField(256)                            recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    represents : Vector(NMArbField(256)) -> %
 retract : % -> Fraction(Integer)                       retract : % -> Integer
 retract : % -> NMArbField(256)                         retractIfCan : % -> Union(Integer,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    rootOfUnity : NonNegativeInteger -> %
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          sin : % -> %
 sinh : % -> %                                          sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 tan : % -> %                                           tanh : % -> %
 trace : % -> NMArbField(256)                           traceMatrix : () -> Matrix(NMArbField(256))
 traceMatrix : Vector(%) -> Matrix(NMArbField(256))     trim : % -> %
 uniqueInteger : % -> Union(NMInteger,"failed")         unit? : % -> Boolean
 unitCanonical : % -> %                                 urand01 : () -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 D : (%, (NMArbField(256) -> NMArbField(256))) -> %
 D : (%, (NMArbField(256) -> NMArbField(256)), NonNegativeInteger) -> %
 characteristicPolynomial : % -> SparseUnivariatePolynomial(NMArbField(256))
 convert : SparseUnivariatePolynomial(NMArbField(256)) -> %
 convert : % -> SparseUnivariatePolynomial(NMArbField(256))
 coordinates : (Vector(%), Vector(%)) -> Matrix(NMArbField(256))
 coordinates : (%, Vector(%)) -> Vector(NMArbField(256))
 definingPolynomial : () -> SparseUnivariatePolynomial(NMArbField(256))
 derivationCoordinates : (Vector(%), (NMArbField(256) -> NMArbField(256))) -> Matrix(NMArbField(256))
 differentiate : (%, (NMArbField(256) -> NMArbField(256))) -> %
 differentiate : (%, (NMArbField(256) -> NMArbField(256)), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 ?exquo? : (%, NMArbField(256)) -> Union(%,"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 guess : (%, NonNegativeInteger) -> NMAlgebraicNumber
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 lift : % -> SparseUnivariatePolynomial(NMArbField(256))
 map : ((NMArbField(256) -> NMArbField(256)), %) -> %
 minimalPolynomial : % -> SparseUnivariatePolynomial(NMArbField(256))
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 polarCoordinates : % -> Record(r: NMArbField(256),phi: NMArbField(256))
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reduce : SparseUnivariatePolynomial(NMArbField(256)) -> %
 reduce : Fraction(SparseUnivariatePolynomial(NMArbField(256))) -> Union(%,"failed")
 reducedSystem : Matrix(%) -> Matrix(NMArbField(256))
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(NMArbField(256)),vec: Vector(NMArbField(256)))
 regularRepresentation : % -> Matrix(NMArbField(256))
 regularRepresentation : (%, Vector(%)) -> Matrix(NMArbField(256))
 represents : (Vector(NMArbField(256)), Vector(%)) -> %
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 retractIfCan : % -> Union(NMArbField(256),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
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
