# NMAlgebraicNumber

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L202)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

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

### `QQbar` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L331)\]

jnanumber(q) returns q as a NMAlgebraicNumber.

- **Signature**: `(Fraction(Integer))->%`
- **Signature**: `(NMFraction(NMInteger))->%`

jnanumber(x) returns x as a NMAlgebraicNumber.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

### `abs` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L297)\]

abs(a) returns the absolute value of a.

- **Signature**: `(%)->%`

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L299)\]

abs2(a) returns the squared absolute value of a.

- **Signature**: `(%)->%`

### `acospi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L274)\]

acospi(x) returns acos(x)/%pi

- **Signature**: `(%)->%`

### `algebraicInteger?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L242)\]

algebraicInteger?(a) tests whether or not a is an algebraic integer.

- **Signature**: `(%)->Boolean`

### `asinpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L272)\]

asinpi(x) returns asin(x)/%pi

- **Signature**: `(%)->%`

### `atanpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L276)\]

atanpi(x) returns atan(x)/%pi

- **Signature**: `(%)->%`

### `ceiling` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L305)\]

ceiling(a) returns the smallest integer above or equal to a.

- **Signature**: `(%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L318)\]

coerce(nan) coerces nan to AlgebraicNumber using the root of the minimal polynomial.

- **Signature**: `(%)->AlgebraicNumber`

### `conjugate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L301)\]

conjugate(a) returns the complex conjugate of a.

- **Signature**: `(%)->%`

### `conjugates` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L278)\]

conjugates(a) returns all the roots of a.

- **Signature**: `(%)->JLVector(%)`

### `cospi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L268)\]

cospi(x) returns cos(%pi*x).

- **Signature**: `(%)->%`

### `crandom` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L254)\]

crandom(deg, bits) returns a random algebraic number (complex) of degree up to deg and coefficients size up to bits. Requires at least degree 2.

- **Signature**: `(NonNegativeInteger,NonNegativeInteger)->%`

### `csign` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L313)\]

csign(a) returns an extension of the real sign function equivalent to a/sqrt(a^2).

- **Signature**: `(%)->%`

### `degree` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L280)\]

degree(a) returns the degree of the minimal polynomial of a.

- **Signature**: `(%)->JLInt64`

### `denominator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L285)\]

denominator(anum) returns the denominator of anum, i.e. the leading coefficient of the minimal polynomial of a.

- **Signature**: `(%)->NMInteger`

### `expPiI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L258)\]

expPiI(a) returns exp(%pi*%i*a).

- **Signature**: `(%)->%`

### `floor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L303)\]

floor(a) returns the largest integer below or equal to a.

- **Signature**: `(%)->%`

### `height` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L293)\]

height(a) returns the height of a.

- **Signature**: `(%)->NonNegativeInteger`

### `heightBits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L295)\]

heightBits(a) returns the height of a as a number of bits.

- **Signature**: `(%)->NonNegativeInteger`

### `imag` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L247)\]

imag(x) returns imaginary part of x.

- **Signature**: `(%)->%`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L236)\]

integer?(x) tests whether or not x is an integer.

- **Signature**: `(%)->Boolean`

### `jnanumber` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L321)\]

jnanumber(q) returns q as a NMAlgebraicNumber.

- **Signature**: `(Fraction(Integer))->%`
- **Signature**: `(NMFraction(NMInteger))->%`

jnanumber(x) returns x as a NMAlgebraicNumber.

- **Signature**: `(Integer)->%`
- **Signature**: `(NMInteger)->%`

jnanumber(str) evaluates str in Julia that returns a NMAlgebraicNumber.

- **Signature**: `(String)->%`

### `logPiI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L260)\]

logPiI(a) returns log(a)/(%pi*%i).

- **Signature**: `(%)->%`

### `minimalPolynomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L288)\]

minimalPolynomial(an) returns the minimal polynomial of an over algebraic numbers.

- **Signature**: `(%)->SparseUnivariatePolynomial(%)`

minimalPolynomial(an) returns the minimal polynomial of an.

- **Signature**: `(%)->SparseUnivariatePolynomial(Integer)`

### `norm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L316)\]

norm(a) returns the norm of a.

- **Signature**: `(%)->%`

### `numerator` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L282)\]

numerator(anum) returns anum multiplied by its denominator i.e. an algebraic integer.

- **Signature**: `(%)->%`

### `random` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L251)\]

random(deg, bits) returns a random algebraic number (real) of degree up to deg and coefficients size up to bits.

- **Signature**: `(NonNegativeInteger,NonNegativeInteger)->%`

### `rational?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L238)\]

rational?(x) tests whether or not x is a rational number.

- **Signature**: `(%)->Boolean`

### `real` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L245)\]

real(x) returns real part of x.

- **Signature**: `(%)->%`

### `real?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L249)\]

real?(x) tests whether or not x is a real number.

- **Signature**: `(%)->Boolean`

### `rootOfUnity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L262)\]

rootOfUnity(n) returns the root of unity exp(2*%pi*%i/n).

- **Signature**: `(NonNegativeInteger)->%`

rootOfUnity(n,k) returns the root of unity exp(2*%pi*%i*k/n).

- **Signature**: `(NonNegativeInteger,Integer)->%`

### `rootOfUnity?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L240)\]

rootOfUnity?(x) tests whether or not x is a root of unity.

- **Signature**: `(%)->Boolean`

### `sign` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L307)\]

sign(a) returns the complex sign of a.

- **Signature**: `(%)->%`

### `signImag` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L311)\]

signImag(a) returns the sign of the imaginary part.

- **Signature**: `(%)->%`

### `signReal` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L309)\]

signReal(a) returns the sign of the real part.

- **Signature**: `(%)->%`

### `sinpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L266)\]

sinpi(x) returns sin(%pi*x).

- **Signature**: `(%)->%`

### `tanpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L270)\]

tanpi(x) returns tan(%pi*x).

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
