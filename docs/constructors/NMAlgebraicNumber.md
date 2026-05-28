# NMAlgebraicNumber

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L1)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

This domain allows the manipulation of Nemo algebraic numbers, i.e. algebraic closure of rational field, represented by minimal polynomials using the Nemo Julia package (Calcium library based). https://fredrikj.net/calcium/

**NMAlgebraicNumber is a domain constructor.**  
**Abbreviation for NMAlgebraicNumber is NAN**  
**This constructor is exposed in this frame.**  
**126 names for 182 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, Integer) -> %                                ?*? : (%, NMFraction(NMInteger)) -> %
 ?*? : (%, NMInteger) -> %                              ?*? : (Fraction(Integer), %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?=? : (%, %) -> Boolean                                D : % -> %
 D : (%, NonNegativeInteger) -> %                       QQbar : Fraction(Integer) -> %
 QQbar : Integer -> %                                   QQbar : NMFraction(NMInteger) -> %
 QQbar : NMInteger -> %                                 ?^? : (%, %) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 abs : % -> %                                           abs2 : % -> %
 acospi : % -> %                                        algebraicInteger? : % -> Boolean
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 asinpi : % -> %                                        associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            atanpi : % -> %
 ceiling : % -> %                                       characteristic : () -> NonNegativeInteger
 coerce : % -> %                                        coerce : Fraction(Integer) -> %
 coerce : Integer -> %                                  coerce : NMFraction(NMInteger) -> %
 coerce : NMInteger -> %                                coerce : % -> AlgebraicNumber
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               conjugate : % -> %
 conjugates : % -> JLVector(%)                          convert : % -> NMComplexField
 convert : % -> String                                  cospi : % -> %
 csign : % -> %                                         degree : % -> JLInt64
 denominator : % -> NMInteger                           differentiate : % -> %
 differentiate : (%, NonNegativeInteger) -> %           equal? : (%, %) -> Boolean
 euclideanSize : % -> NonNegativeInteger                exact? : % -> Boolean
 exactDivide : (%, %) -> %                              expPiI : % -> %
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 floor : % -> %                                         gcd : (%, %) -> %
 gcd : List(%) -> %                                     height : % -> NonNegativeInteger
 heightBits : % -> NonNegativeInteger                   imag : % -> %
 integer? : % -> Boolean                                inv : % -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %, %, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlId : % -> JLInt64                                    jlNMRing : () -> String
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jnanumber : Fraction(Integer) -> %
 jnanumber : Integer -> %                               jnanumber : NMFraction(NMInteger) -> %
 jnanumber : NMInteger -> %                             jnanumber : String -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 logPiI : % -> %                                        missing? : % -> Boolean
 mutable? : % -> Boolean                                norm : % -> %
 nothing? : % -> Boolean                                nthRoot : (%, Integer) -> %
 numerator : % -> %                                     one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          plenaryPower : (%, PositiveInteger) -> %
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 rational? : % -> Boolean                               real : % -> %
 real? : % -> Boolean                                   recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    retract : % -> Fraction(Integer)
 retract : % -> Integer                                 retract : % -> NMFraction(NMInteger)
 retract : % -> NMInteger                               retractIfCan : % -> Union(Integer,"failed")
 retractIfCan : % -> Union(NMInteger,"failed")          rightPower : (%, NonNegativeInteger) -> %
 rightPower : (%, PositiveInteger) -> %                 rightRecip : % -> Union(%,"failed")
 rootOf : Polynomial(%) -> %                            rootOf : SparseUnivariatePolynomial(%) -> %
 rootOfUnity : NonNegativeInteger -> %                  rootOfUnity : (NonNegativeInteger, Integer) -> %
 rootOfUnity? : % -> Boolean                            rootsOf : Polynomial(%) -> List(%)
 sample : () -> %                                       sign : % -> %
 signImag : % -> %                                      signReal : % -> %
 sinpi : % -> %                                         sizeLess? : (%, %) -> Boolean
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tanpi : % -> %
 unit? : % -> Boolean                                   unitCanonical : % -> %
 zero? : % -> Boolean                                   zeroOf : Polynomial(%) -> %
 zeroOf : SparseUnivariatePolynomial(%) -> %            zerosOf : Polynomial(%) -> List(%)
 ?~=? : (%, %) -> Boolean
 crandom : (NonNegativeInteger, NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 minimalPolynomial : % -> SparseUnivariatePolynomial(%)
 minimalPolynomial : % -> SparseUnivariatePolynomial(Integer)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 random : (NonNegativeInteger, NonNegativeInteger) -> %
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 retractIfCan : % -> Union(NMFraction(NMInteger),"failed")
 rootOf : (SparseUnivariatePolynomial(%), Symbol) -> %
 rootsOf : SparseUnivariatePolynomial(%) -> List(%)
 rootsOf : (SparseUnivariatePolynomial(%), Symbol) -> List(%)
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed")
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%))
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
 zeroOf : (SparseUnivariatePolynomial(%), Symbol) -> %
 zerosOf : SparseUnivariatePolynomial(%) -> List(%)
 zerosOf : (SparseUnivariatePolynomial(%), Symbol) -> List(%)
```

## Operations added

### `QQbar` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L130)\]

jnanumber(q) returns q as a NMAlgebraicNumber.

- **Signature**: `(Fraction(Integer))->%`
- **Signature**: `(NMFraction(NMInteger))->%`

jnanumber(x) returns x as a NMAlgebraicNumber.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `abs` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L96)\]

abs(a) returns the absolute value of a.

- **Signature**: `(%)->%`

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L98)\]

abs2(a) returns the squared absolute value of a.

- **Signature**: `(%)->%`

### `acospi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L73)\]

acospi(x) returns acos(x)/%pi

- **Signature**: `(%)->%`

### `algebraicInteger?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L41)\]

algebraicInteger?(a) tests whether or not a is an algebraic integer.

- **Signature**: `(%)->Boolean`

### `asinpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L71)\]

asinpi(x) returns asin(x)/%pi

- **Signature**: `(%)->%`

### `atanpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L75)\]

atanpi(x) returns atan(x)/%pi

- **Signature**: `(%)->%`

### `ceiling` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L104)\]

ceiling(a) returns the smallest integer above or equal to a.

- **Signature**: `(%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L117)\]

coerce(nan) coerces nan to AlgebraicNumber using the root of the minimal polynomial.

- **Signature**: `(%)->AlgebraicNumber`

### `conjugate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L100)\]

conjugate(a) returns the complex conjugate of a.

- **Signature**: `(%)->%`

### `conjugates` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L77)\]

conjugates(a) returns all the roots of a.

- **Signature**: `(%)->JLVector(%)`

### `cospi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L67)\]

cospi(x) returns cos(%pi*x).

- **Signature**: `(%)->%`

### `crandom` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L53)\]

crandom(deg, bits) returns a random algebraic number (complex) of degree up to deg and coefficients size up to bits. Requires at least degree 2.

- **Signature**: `(NonNegativeInteger,NonNegativeInteger)->%`

### `csign` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L112)\]

csign(a) returns an extension of the real sign function equivalent to a/sqrt(a^2).

- **Signature**: `(%)->%`

### `degree` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L79)\]

degree(a) returns the degree of the minimal polynomial of a.

- **Signature**: `(%)->JLInt64`

### `denominator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L84)\]

denominator(anum) returns the denominator of anum, i.e. the leading coefficient of the minimal polynomial of a.

- **Signature**: `(%)->NMInteger`

### `expPiI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L57)\]

expPiI(a) returns exp(%pi*%i*a).

- **Signature**: `(%)->%`

### `floor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L102)\]

floor(a) returns the largest integer below or equal to a.

- **Signature**: `(%)->%`

### `height` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L92)\]

height(a) returns the height of a.

- **Signature**: `(%)->NonNegativeInteger`

### `heightBits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L94)\]

heightBits(a) returns the height of a as a number of bits.

- **Signature**: `(%)->NonNegativeInteger`

### `imag` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L46)\]

imag(x) returns imaginary part of x.

- **Signature**: `(%)->%`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L35)\]

integer?(x) tests whether or not x is an integer.

- **Signature**: `(%)->Boolean`

### `jnanumber` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L120)\]

jnanumber(q) returns q as a NMAlgebraicNumber.

- **Signature**: `(Fraction(Integer))->%`
- **Signature**: `(NMFraction(NMInteger))->%`

jnanumber(x) returns x as a NMAlgebraicNumber.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

jnanumber(str) evaluates str in Julia that returns a NMAlgebraicNumber.

- **Signature**: `(String)->%`

### `logPiI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L59)\]

logPiI(a) returns log(a)/(%pi*%i).

- **Signature**: `(%)->%`

### `minimalPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L87)\]

minimalPolynomial(an) returns the minimal polynomial of an over algebraic numbers.

- **Signature**: `(%)->SparseUnivariatePolynomial(%)`

minimalPolynomial(an) returns the minimal polynomial of an.

- **Signature**: `(%)->SparseUnivariatePolynomial(Integer)`

### `norm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L115)\]

norm(a) returns the norm of a.

- **Signature**: `(%)->%`

### `numerator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L81)\]

numerator(anum) returns anum multiplied by its denominator i.e. an algebraic integer.

- **Signature**: `(%)->%`

### `random` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L50)\]

random(deg, bits) returns a random algebraic number (real) of degree up to deg and coefficients size up to bits.

- **Signature**: `(NonNegativeInteger,NonNegativeInteger)->%`

### `rational?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L37)\]

rational?(x) tests whether or not x is a rational number.

- **Signature**: `(%)->Boolean`

### `real` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L44)\]

real(x) returns real part of x.

- **Signature**: `(%)->%`

### `real?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L48)\]

real?(x) tests whether or not x is a real number.

- **Signature**: `(%)->Boolean`

### `rootOfUnity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L61)\]

rootOfUnity(n) returns the root of unity exp(2*%pi*%i/n).

- **Signature**: `(NonNegativeInteger)->%`

rootOfUnity(n,k) returns the root of unity exp(2*%pi*%i*k/n).

- **Signature**: `(NonNegativeInteger,Integer)->%`

### `rootOfUnity?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L39)\]

rootOfUnity?(x) tests whether or not x is a root of unity.

- **Signature**: `(%)->Boolean`

### `sign` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L106)\]

sign(a) returns the complex sign of a.

- **Signature**: `(%)->%`

### `signImag` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L110)\]

signImag(a) returns the sign of the imaginary part.

- **Signature**: `(%)->%`

### `signReal` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L108)\]

signReal(a) returns the sign of the real part.

- **Signature**: `(%)->%`

### `sinpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L65)\]

sinpi(x) returns sin(%pi*x).

- **Signature**: `(%)->%`

### `tanpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/NAN.spad#L69)\]

tanpi(x) returns tan(%pi*x).

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
