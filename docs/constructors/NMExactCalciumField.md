# NMExactCalciumField

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1346)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

NMExactCalciumField implements exact real/complex field arithmetic using the Nemo Julia package.

**NMExactCalciumField is a domain constructor.**  
**Abbreviation for NMExactCalciumField is NECF**  
**This constructor is exposed in this frame.**  
**176 names for 239 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Integer) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?/? : (%, Integer) -> %                                ?/? : (Integer, %) -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               D : (%, (% -> %)) -> %
 D : (%, (% -> %), NonNegativeInteger) -> %             Gamma : % -> %
 ?^? : (%, %) -> %                                      ?^? : (%, Fraction(Integer)) -> %
 ?^? : (%, Integer) -> %                                ?^? : (%, NonNegativeInteger) -> %
 ?^? : (%, PositiveInteger) -> %                        abs : % -> %
 abs2 : % -> %                                          acos : % -> %
 acos : (%, JLSymbol) -> %                              acosh : % -> %
 acot : % -> %                                          acoth : % -> %
 acsc : % -> %                                          acsch : % -> %
 algebraic? : % -> Boolean                              annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           argument : % -> %
 asec : % -> %                                          asech : % -> %
 asin : % -> %                                          asin : (%, JLSymbol) -> %
 asinh : % -> %                                         associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            atan : % -> %
 atan : (%, JLSymbol) -> %                              atanh : % -> %
 basis : () -> Vector(%)                                ceiling : % -> %
 characteristic : () -> NonNegativeInteger              coerce : % -> %
 coerce : Complex(Integer) -> %                         coerce : Fraction(Integer) -> %
 coerce : Integer -> %                                  coerce : NMAlgebraicNumber -> %
 coerce : NMFraction(NMInteger) -> %                    coerce : PositiveInteger -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               complex : (%, %) -> %
 complexNormalForm : % -> %                             conjugate : % -> %
 conjugate : (%, JLSymbol) -> %                         convert : SparseUnivariatePolynomial(%) -> %
 convert : Vector(%) -> %                               convert : % -> SparseUnivariatePolynomial(%)
 convert : % -> String                                  convert : % -> Vector(%)
 coordinates : Vector(%) -> Matrix(%)                   coordinates : % -> Vector(%)
 coordinates : (%, Vector(%)) -> Vector(%)              cos : % -> %
 cos : (%, JLSymbol) -> %                               cosh : % -> %
 cot : % -> %                                           coth : % -> %
 csc : % -> %                                           csch : % -> %
 csign : % -> %                                         differentiate : (%, (% -> %)) -> %
 discriminant : () -> %                                 discriminant : Vector(%) -> %
 equal? : (%, %) -> Boolean                             erf : % -> %
 erfc : % -> %                                          erfi : % -> %
 euclideanSize : % -> NonNegativeInteger                eulerGamma : () -> %
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 exp : () -> %                                          exp : % -> %
 exp1 : () -> %                                         ?exquo? : (%, %) -> Union(%,"failed")
 factor : % -> Factored(%)                              floor : % -> %
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 generator : () -> %                                    imag : % -> %
 imaginary : () -> %                                    imaginary? : % -> Boolean
 infinity : () -> %                                     infinity : % -> %
 infinity? : % -> Boolean                               integer? : % -> Boolean
 inv : % -> %                                           jlAbout : % -> Void
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlNMRing : () -> String                                jlObject : () -> String
 jlOptions : % -> JLObjDict                             jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    jnecf : Fraction(Integer) -> %
 jnecf : NMAlgebraicNumber -> %                         jnecf : NMFraction(NMInteger) -> %
 latex : % -> String                                    lcm : (%, %) -> %
 lcm : List(%) -> %                                     leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 lift : % -> SparseUnivariatePolynomial(%)              log : % -> %
 map : ((% -> %), %) -> %                               max : (%, %) -> %
 min : (%, %) -> %                                      missing? : % -> Boolean
 mutable? : % -> Boolean                                negativeInfinity : () -> %
 norm : % -> %                                          nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            number? : % -> Boolean
 one? : % -> Boolean                                    opposite? : (%, %) -> Boolean
 pi : () -> %                                           plenaryPower : (%, PositiveInteger) -> %
 positiveInfinity : () -> %                             pow : (%, Integer, JLSymbol) -> %
 prime? : % -> Boolean                                  ?quo? : (%, %) -> %
 random : (Integer, Integer) -> %                       random : (Integer, Integer, JLSymbol) -> %
 rank : () -> PositiveInteger                           rational? : % -> Boolean
 real : % -> %                                          real? : % -> Boolean
 recip : % -> Union(%,"failed")                         reduce : SparseUnivariatePolynomial(%) -> %
 reducedSystem : Matrix(%) -> Matrix(%)                 regularRepresentation : % -> Matrix(%)
 ?rem? : (%, %) -> %                                    represents : Vector(%) -> %
 represents : (Vector(%), Vector(%)) -> %               retract : % -> %
 retract : % -> NMComplexBall                           retractIfCan : % -> Union(%,"failed")
 retractIfCan : % -> Union(NMInteger,"failed")          retractIfCan : % -> Union(NMRealBall,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sec : % -> %                                           sech : % -> %
 sign : % -> %                                          signedInfinity? : % -> Boolean
 sin : % -> %                                           sin : (%, JLSymbol) -> %
 sinh : % -> %                                          sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 tan : % -> %                                           tan : (%, JLSymbol) -> %
 tanh : % -> %                                          trace : % -> %
 traceMatrix : () -> Matrix(%)                          traceMatrix : Vector(%) -> Matrix(%)
 undefined : () -> %                                    undefined? : % -> Boolean
 unit? : % -> Boolean                                   unitCanonical : % -> %
 unknown : () -> %                                      unknown? : % -> Boolean
 unsignedInfinity? : % -> Boolean                       zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 characteristicPolynomial : % -> SparseUnivariatePolynomial(%)
 coordinates : (Vector(%), Vector(%)) -> Matrix(%)
 definingPolynomial : () -> SparseUnivariatePolynomial(%)
 derivationCoordinates : (Vector(%), (% -> %)) -> Matrix(%)
 differentiate : (%, (% -> %), NonNegativeInteger) -> %
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if % has PFECAT
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if % has PFECAT
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 jnecf : (Fraction(Integer), Fraction(Integer)) -> %
 jnecf : (NMFraction(NMInteger), NMFraction(NMInteger)) -> %
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 minimalPolynomial : % -> SparseUnivariatePolynomial(%)
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 reduce : Fraction(SparseUnivariatePolynomial(%)) -> Union(%,"failed")
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(%),vec: Vector(%))
 regularRepresentation : (%, Vector(%)) -> Matrix(%)
 retractIfCan : % -> Union(NMAlgebraicNumber,"failed")
 retractIfCan : % -> Union(NMFraction(NMInteger),"failed")
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if % has PFECAT
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if % has PFECAT
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1464)\]

Gamma(x) is the Euler Gamma function evaluated at x.

- **Signature**: `(%)->%`

### `abs` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1412)\]

abs(x) returns the absolute value of x.

- **Signature**: `(%)->%`

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1414)\]

abs2(x) returns the square of the absolute value of x.

- **Signature**: `(%)->%`

### `acos` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1458)\]

acos(x,repr) returns acos(x) using one of the repr representations: :default (since 'default' is a FriCAS keyword use jsym(String) to coerce it) :logarithm :direct

- **Signature**: `(%,JLSymbol)->%`

### `algebraic?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1368)\]

algebraic?(x) checks whether or not x is algebraic.

- **Signature**: `(%)->Boolean`

### `asin` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1453)\]

asin(x, repr) returns asin(x) using one of the repr representations: :default (since 'default' is a FriCAS keyword use jsym(String) to coerce it) :logarithm :direct

- **Signature**: `(%,JLSymbol)->%`

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1448)\]

atan(x, repr) returns atan(x) using one of the repr representations: :default (since 'default' is a FriCAS keyword use jsym(String) to coerce it) :logarithm :direct or :arctangent

- **Signature**: `(%,JLSymbol)->%`

### `ceiling` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1423)\]

ceiling(z) returns the smallest integer above or equal to z.

- **Signature**: `(%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1508)\]

coerce(z) coerces z. Convenience function.

- **Signature**: `(Complex(Integer))->%`

coerce(q) coerces q. Convenience function.

- **Signature**: `(Fraction(Integer))->%`
- **Signature**: `(NMFraction(NMInteger))->%`

coerce(qbar) coerces qbar. Convenience function.

- **Signature**: `(NMAlgebraicNumber)->%`

coerce(pi) coerces pi. Convenience function.

- **Signature**: `(PositiveInteger)->%`

### `complexNormalForm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1472)\]

complexNormalForm(x) returns x rewritten using standard transformations. See the Nemo.jl documentation for more information.

- **Signature**: `(%)->%`

### `conjugate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1407)\]

conjugate(x, repr) returns conjugate(x) using one of the repr representations: :default (since 'default' is aFriCAS keyword use jsym(String) to coerce it), :deep (recursively), :shallow (a new extension, ā, is used if there is no express simplification).

- **Signature**: `(%,JLSymbol)->%`

### `cos` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1436)\]

cos(x, repr) returns cos(x) using one of the repr representations: :default (since 'default' is a FriCAS keyword use jsym(String) to coerce it) :exponential :tangent :direct or :sine_cosine

- **Signature**: `(%,JLSymbol)->%`

### `csign` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1418)\]

csign(x) is an extension of the real sign function, x/sqrt(x^2) unless x is 0 (0 in this case).

- **Signature**: `(%)->%`

### `erf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1466)\]

erf(x) is the error function evaluated at x.

- **Signature**: `(%)->%`

### `erfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1470)\]

erfc(x) is the complementary error function evaluated at x.

- **Signature**: `(%)->%`

### `erfi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1468)\]

erfi(x) is the imaginary error function evaluated at x.

- **Signature**: `(%)->%`

### `eulerGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1391)\]

eulerGamma() returns the Euler's constant gamma (γ).

- **Signature**: `()->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1387)\]

exp() returns ℯ (exp(1)).

- **Signature**: `()->%`

### `exp1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1389)\]

exp1() returns ℯ (exp(1)).

- **Signature**: `()->%`

### `floor` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1421)\]

floor(z) returns the largest integer below or equal to z.

- **Signature**: `(%)->%`

### `imaginary?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1370)\]

imaginary?(x) checks whether or not x is imaginary.

- **Signature**: `(%)->Boolean`

### `infinity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1395)\]

infinity() returns unsigned infinity.

- **Signature**: `()->%`

infinity(x) returns signed infinity depending on x sign.

- **Signature**: `(%)->%`

### `infinity?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1381)\]

infinity?(x) checks whether or not x is an infinity.

- **Signature**: `(%)->Boolean`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1366)\]

integer?(z) checks whether or not z is an integer.

- **Signature**: `(%)->Boolean`

### `jlOptions` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1496)\]

jlOptions(x) returns the options used during computations.

- **Signature**: `(%)->JLObjDict`

### `jnecf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1518)\]

jnecf(fi) coerces fi to a Nemo real exact number.

- **Signature**: `(Fraction(Integer))->%`

jnecf(real,cplx) coerces real and cplx to a Nemo exact complex number where real is the real part and cplx the complex part.

- **Signature**: `(Fraction(Integer),Fraction(Integer))->%`

jnecf(qbar) coerces qbar to a Nemo exact real or complex number.

- **Signature**: `(NMAlgebraicNumber)->%`

jnecf(nr) coerces nr to a Nemo exact real number.

- **Signature**: `(NMFraction(NMInteger))->%`

jnecf(real,cplx) coerces real and cplx to a Nemo exact where real is the real part and cplx the complex part.

- **Signature**: `(NMFraction(NMInteger),NMFraction(NMInteger))->%`

### `negativeInfinity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1399)\]

negativeInfinity() returns negative infinity.

- **Signature**: `()->%`

### `number?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1374)\]

number?(x) checks whether or not x is a number, i.e. not an infinity or an undefined value.

- **Signature**: `(%)->Boolean`

### `pi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1393)\]

pi() returns π.

- **Signature**: `()->%`

### `positiveInfinity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1397)\]

positiveInfinity() returns positive infinity.

- **Signature**: `()->%`

### `pow` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1425)\]

pow(x, i, repr) x raised to power i using one of the repr representations: :default (since 'default' is a FriCAS keyword use jsym(String) to coerce it) :arithmetic See the Nemo documentation for more information.

- **Signature**: `(%,Integer,JLSymbol)->%`

### `random` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1475)\]

random(depth, bits) returns a random number with size of coefficients up to bits. If depth is nonzero, apply a random arithmetic operation/function to operands produced using recursive calls with depth - 1.

- **Signature**: `(Integer,Integer)->%`

random(depth, bits, type) returns a random number with size of coefficients up to bits. If depth is nonzero, apply a random arithmetic operation/function to operands produced using recursive calls with depth - 1. depth is not used for rationals. type can be one of: :rational (returns a rational) :null (returns value with default settings) :special (returns a special value of a number - can throw an error if it isn't a number).

- **Signature**: `(Integer,Integer,JLSymbol)->%`

### `rational?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1372)\]

rational?(x) checks whether or not x is a rational number.

- **Signature**: `(%)->Boolean`

### `real?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1364)\]

real?(z) checks whether or not z is a real number.

- **Signature**: `(%)->Boolean`

### `retract` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1506)\]

retract(x) retracts x.

- **Signature**: `(%)->NMComplexBall`

### `retractIfCan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1498)\]

retractIfCan(z) retracts if possible z to a NMAlgebraicNumber.

- **Signature**: `(%)->Union(NMAlgebraicNumber,"failed")`

retractIfCan(z) retracts if possible z to a NMFraction(NMInteger).

- **Signature**: `(%)->Union(NMFraction(NMInteger),"failed")`

retractIfCan(z) retracts if possible z to a Integer.

- **Signature**: `(%)->Union(NMInteger,"failed")`

retractIfCan(x) retracts x.

- **Signature**: `(%)->Union(NMRealBall,"failed")`

### `sign` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1416)\]

sign(x) returns sign of x.

- **Signature**: `(%)->%`

### `signedInfinity?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1383)\]

signedInfinity?(x) checks whether or not x is a signed infinity.

- **Signature**: `(%)->Boolean`

### `sin` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1430)\]

sin(x, repr) returns sin(x) using one of the repr representations: :default (since 'default' is a FriCAS keyword use jsym(String) to coerce it) :exponential :tangent :direct

- **Signature**: `(%,JLSymbol)->%`

### `tan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1442)\]

tan(x, repr) returns tan(x) using one of the repr representations: :default (since 'default' is a FriCAS keyword use jsym(String) to coerce it) :exponential :direct or :tangent :sine_cosine

- **Signature**: `(%,JLSymbol)->%`

### `undefined` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1405)\]

undefined() returns the undefined special value.

- **Signature**: `()->%`

### `undefined?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1379)\]

undefined?(x) checks whether or not x is undefined.

- **Signature**: `(%)->Boolean`

### `unknown` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1403)\]

unknown() returns the unknown special value.

- **Signature**: `()->%`

### `unknown?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1377)\]

unknown?(x) checks whether or not x is unknown.

- **Signature**: `(%)->Boolean`

### `unsignedInfinity?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1385)\]

unsignedInfinity?(x) checks whether or not x is an unsigned infinity.

- **Signature**: `(%)->Boolean`
---
[Back to Index](../index.md)
