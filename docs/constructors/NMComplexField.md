# NMComplexField

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1006)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

NMComplexField implements arbitrary precision ball arithmetic using the Nemo Julia package.

**NMComplexField is a domain constructor.**  
**Abbreviation for NMComplexField is NCF**  
**This constructor is not exposed in this frame.**  
**172 names for 244 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, Integer) -> %                                ?*? : (%, NMRealField) -> %
 ?*? : (Fraction(Integer), %) -> %                      ?*? : (Integer, %) -> %
 ?*? : (NMRealField, %) -> %                            ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?/? : (%, Integer) -> %                                ?/? : (Integer, %) -> %
 ?=? : (%, %) -> Boolean                                D : % -> %
 D : (%, (NMRealField -> NMRealField)) -> %             D : (%, NonNegativeInteger) -> %
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
 argument : % -> NMRealField                            asec : % -> %
 asech : % -> %                                         asin : % -> %
 asinh : % -> %                                         associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            atan : % -> %
 atanh : % -> %                                         basis : () -> Vector(%)
 besselI : (%, %) -> %                                  besselJ : (%, %) -> %
 besselK : (%, %) -> %                                  besselY : (%, %) -> %
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Complex(Integer) -> %                         coerce : Float -> %
 coerce : Fraction(Integer) -> %                        coerce : Integer -> %
 coerce : JLFloat64 -> %                                coerce : NMRealField -> %
 coerce : String -> %                                   coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 complex : (NMRealField, NMRealField) -> %              conjugate : % -> %
 contains? : (%, %) -> Boolean                          contains? : (%, NMFraction(NMInteger)) -> Boolean
 contains? : (%, NMInteger) -> Boolean                  containsZero? : % -> Boolean
 convert : Vector(NMRealField) -> %                     convert : % -> Complex(DoubleFloat)
 convert : % -> Complex(Float)                          convert : % -> Pattern(Float)
 convert : % -> String                                  convert : % -> Vector(NMRealField)
 coordinates : Vector(%) -> Matrix(NMRealField)         coordinates : % -> Vector(NMRealField)
 cos : % -> %                                           cosh : % -> %
 cot : % -> %                                           coth : % -> %
 csc : % -> %                                           csch : % -> %
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 discriminant : () -> NMRealField                       discriminant : Vector(%) -> NMRealField
 ellipticE : % -> %                                     ellipticK : % -> %
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 exp : () -> %                                          exp : % -> %
 exp1 : () -> %                                         expm1 : % -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  ?exquo? : (%, NMRealField) -> Union(%,"failed")
 factor : % -> Factored(%)                              finite? : % -> Boolean
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 generator : () -> %                                    hurwitzZeta : (%, %) -> %
 hypergeometric1F1 : (%, %, %) -> %                     hypergeometric1F1Regularized : (%, %, %) -> %
 hypergeometricU : (%, %, %) -> %                       imag : % -> NMRealField
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
 jncf : Float -> %                                      jncf : (Float, Float) -> %
 jncf : Integer -> %                                    jncf : (Integer, Integer) -> %
 jncf : String -> %                                     jncf : (String, String) -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     ldexp : (%, NMInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     log : % -> %
 log1p : % -> %                                         map : ((NMRealField -> NMRealField), %) -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 norm : % -> NMRealField                                nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          overlaps? : (%, %) -> Boolean
 pi : () -> %                                           plenaryPower : (%, PositiveInteger) -> %
 polyLog : (%, %) -> %                                  polygamma : (%, %) -> %
 precision : () -> PositiveInteger                      precision : PositiveInteger -> PositiveInteger
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 randtest : JLSymbol -> %                               rank : () -> PositiveInteger
 real : % -> NMRealField                                recip : % -> Union(%,"failed")
 reducedSystem : Matrix(%) -> Matrix(NMRealField)       regularRepresentation : % -> Matrix(NMRealField)
 ?rem? : (%, %) -> %                                    represents : Vector(NMRealField) -> %
 retract : % -> Fraction(Integer)                       retract : % -> Integer
 retract : % -> NMRealField                             retractIfCan : % -> Union(Integer,"failed")
 retractIfCan : % -> Union(NMRealField,"failed")        rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 rootOfUnity : NonNegativeInteger -> %                  rootOfUnity : (NonNegativeInteger, Integer) -> %
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          sin : % -> %
 sinh : % -> %                                          sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 tan : % -> %                                           tanh : % -> %
 trace : % -> NMRealField                               traceMatrix : () -> Matrix(NMRealField)
 traceMatrix : Vector(%) -> Matrix(NMRealField)         trim : % -> %
 uniqueInteger : % -> Union(NMInteger,"failed")         unit? : % -> Boolean
 unitCanonical : % -> %                                 urand01 : () -> %
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 D : (%, (NMRealField -> NMRealField), NonNegativeInteger) -> %
 characteristicPolynomial : % -> SparseUnivariatePolynomial(NMRealField)
 convert : SparseUnivariatePolynomial(NMRealField) -> %
 convert : % -> SparseUnivariatePolynomial(NMRealField)
 coordinates : (Vector(%), Vector(%)) -> Matrix(NMRealField)
 coordinates : (%, Vector(%)) -> Vector(NMRealField)
 definingPolynomial : () -> SparseUnivariatePolynomial(NMRealField)
 derivationCoordinates : (Vector(%), (NMRealField -> NMRealField)) -> Matrix(NMRealField)
 differentiate : (%, (NMRealField -> NMRealField)) -> %
 differentiate : (%, (NMRealField -> NMRealField), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 guess : (%, NonNegativeInteger) -> NMAlgebraicNumber
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 lift : % -> SparseUnivariatePolynomial(NMRealField)
 minimalPolynomial : % -> SparseUnivariatePolynomial(NMRealField)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 polarCoordinates : % -> Record(r: NMRealField,phi: NMRealField)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reduce : SparseUnivariatePolynomial(NMRealField) -> %
 reduce : Fraction(SparseUnivariatePolynomial(NMRealField)) -> Union(%,"failed")
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(NMRealField),vec: Vector(NMRealField))
 regularRepresentation : (%, Vector(%)) -> Matrix(NMRealField)
 represents : (Vector(NMRealField), Vector(%)) -> %
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1068)\]

Gamma(x) is the Euler Gamma function evaluated at x.

- **Signature**: `(%)->%`

Gamma(x,y) is the incomplete Gamma function.

- **Signature**: `(%,%)->%`

### `abs` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1042)\]

abs(x) returns the absolute value of x.

- **Signature**: `(%)->%`

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1044)\]

abs2(x) returns the square of the absolute value of real part of x and imaginary part of x.

- **Signature**: `(%)->%`

### `accuracyBits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1030)\]

accuracyBits(x) returns the relative accuracy of x in bits.

- **Signature**: `(%)->JLInt64`

### `airyAi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1076)\]

airyAi(z) is the Airy function Ai(z).

- **Signature**: `(R)->R`

### `airyAiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1078)\]

airyAiPrime(z) is the derivative of the Airy function Ai(z).

- **Signature**: `(R)->R`

### `airyBi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1080)\]

airyBi(z) is the Airy function Bi(z).

- **Signature**: `(R)->R`

### `airyBiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1082)\]

airyBiPrime(z) is the derivative of the Airy function Bi(z).

- **Signature**: `(R)->R`

### `besselI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1088)\]

besselI(nu,x) is the Bessel I function.

- **Signature**: `(%,%)->%`

### `besselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1090)\]

besselJ(nu,x) is the Bessel J function.

- **Signature**: `(%,%)->%`

### `besselK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1092)\]

besselK(nu,x) is the Bessel K function.

- **Signature**: `(%,%)->%`

### `besselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1094)\]

besselY(nu,x) is the Bessel Y function.

- **Signature**: `(%,%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1138)\]

coerce(z) coerces z. Convenience function.

- **Signature**: `(Complex(Integer))->%`

coerce(x) coerces x.

- **Signature**: `(Float)->%`
- **Signature**: `(JLFloat64)->%`

coerce(str) returns a NMComplexField element from evaluation in Julia of str.

- **Signature**: `(String)->%`

### `contains?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1034)\]

contains?(x,y) checks whether or not y is contained in x.

- **Signature**: `(%,%)->Boolean`
- **Signature**: `(%,NMFraction(NMInteger))->Boolean`
- **Signature**: `(%,NMInteger)->Boolean`

### `containsZero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1040)\]

containsZero?(x) checks whether or not 0 is contained in x.

- **Signature**: `(%)->Boolean`

### `ellipticE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1084)\]

ellipticE(x) is the complete elliptic integral of the second kind.

- **Signature**: `(%)->%`

### `ellipticK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1086)\]

ellipticK(x) is the complete elliptic integral of the first kind.

- **Signature**: `(%)->%`

### `exact?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1028)\]

exact?(x) checks whether x is exact i.e. with 0 radius.

- **Signature**: `(%)->Boolean`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1052)\]

exp() returns the NMComplexField ℯ (exp(1)).

- **Signature**: `()->%`

### `exp1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1054)\]

exp1() returns the NMComplexField ℯ (exp(1)).

- **Signature**: `()->%`

### `expm1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1047)\]

expm1(x) computes accurately e^x-1. It avoids the loss of precision involved in the direct evaluation of exp(x)-1 for small values of x.

- **Signature**: `(%)->%`

### `finite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1025)\]

finite?(x) checks whether or not x is finite, not an infinity for example.

- **Signature**: `(%)->Boolean`

### `guess` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1130)\]

guess(a, deg) returns the reconstructed algebraic number found if it succeeds. Up to degree deg.

- **Signature**: `(%,NonNegativeInteger)->NMAlgebraicNumber`

### `hurwitzZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1066)\]

hurwitzZeta(s,a) returns the Hurwitz zeta function of s and a.

- **Signature**: `(%,%)->%`

### `hypergeometric1F1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1096)\]

hypergeometric1F1(a,b,z) is the confluent hypergeometric function 1F1.

- **Signature**: `(%,%,%)->%`

### `hypergeometric1F1Regularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1099)\]

hypergeometric1F1Regularized(a,b,z) is the regularized confluent hypergeometric function 1F1.

- **Signature**: `(%,%,%)->%`

### `hypergeometricU` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1102)\]

hypergeometricU(a,b,x) is the confluent hypergeometric function U.

- **Signature**: `(%,%,%)->%`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1023)\]

integer?(x) checks whether or not x is an integer.

- **Signature**: `(%)->Boolean`

### `jncf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1147)\]

jncf(x) returns x as a NMComplexField element.

- **Signature**: `(Float)->%`

jncf(r,i) returns r as real part and i as imaginary part as a NMComplexField element.

- **Signature**: `(Float,Float)->%`
- **Signature**: `(Integer,Integer)->%`

jncf(i)returns i as NMComplexField element.

- **Signature**: `(Integer)->%`

jncf(str) returns a NMComplexField element from evaluation in Julia of str.

- **Signature**: `(String)->%`

jncf(r,i) returns a NMComplexField element from evaluation in Julia of r and i. r is the real part, i the imaginary one.

- **Signature**: `(String,String)->%`

### `ldexp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1056)\]

ldexp(x, n) returns x * 2^n.

- **Signature**: `(%,NMInteger)->%`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1050)\]

log1p(x) logarithm of 1+x computed accurately.

- **Signature**: `(%)->%`

### `overlaps?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1032)\]

overlaps?(x,y) checks whether or not any part of x and y balls overlaps.

- **Signature**: `(%,%)->Boolean`

### `pi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1058)\]

pi() returns the JLFloat representation of π.

- **Signature**: `()->%`

### `polyLog` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1074)\]

polyLog(x,y) returns the polyLog function applied to x and y.

- **Signature**: `(%,%)->%`

### `polygamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1072)\]

polygamma(x,y) returns the polygamma function applied to x and y.

- **Signature**: `(%,%)->%`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1125)\]

precision() returns the actual default precision used when creating a new element.

- **Signature**: `()->PositiveInteger`

precision(p) changes the precision used for creating elements.

- **Signature**: `(PositiveInteger)->PositiveInteger`

### `randtest` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1114)\]

randtest(randtype) returns a random number depending on the Julia symbol randtype. :urandom an uniformly distributed random number contained in the unit disk. To test corner cases: :randtest with separate real and imaginary parts, :randtest_precise with precise real and imaginary parts, :randtest_special with separate real and imaginary parts but eventually infinities and NaNs, :randtest_param with very high probability of returning integers or half-integers.

- **Signature**: `(JLSymbol)->%`

### `rootOfUnity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1107)\]

rootOfUnity(n)Return the root of unity exp(2*%pi*%i/n).

- **Signature**: `(NonNegativeInteger)->%`

rootOfUnity(n,k)Return the root of unity exp(2*%pi*%i*k/n).

- **Signature**: `(NonNegativeInteger,Integer)->%`

### `trim` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1133)\]

trim(x) rounds off insignificant bits from the midpoint.

- **Signature**: `(%)->%`

### `uniqueInteger` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1135)\]

uniqueInteger(x) returns a NMInteger if there is a unique integer in the interval x, "failed" otherwise.

- **Signature**: `(%)->Union(NMInteger,"failed")`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1111)\]

urand01() returns an uniformly distributed random number contained in the unit disk.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
