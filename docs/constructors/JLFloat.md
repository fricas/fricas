# JLFloat

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L770)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

JLFloat implements arbitrary precision floating point arithmetic using Julia BigFloats. Use the MPFR library:https://www.mpfr.org/

**JLFloat is a domain constructor.**  
**Abbreviation for JLFloat is JFLOAT**  
**This constructor is exposed in this frame.**  
**153 names for 209 operations in this domain.**

## Signatures

```fricas
--- Operations ---

 0 : () -> %                                            1 : () -> %
 ?*? : (%, %) -> %                                      ?*? : (%, Fraction(Integer)) -> %
 ?*? : (%, Integer) -> %                                ?*? : (Fraction(Integer), %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (NonNegativeInteger, %) -> %
 ?*? : (PositiveInteger, %) -> %                        ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, %) -> %
 ?/? : (%, Integer) -> %                                ?/? : (Integer, %) -> %
 ?<? : (%, %) -> Boolean                                ?<=? : (%, %) -> Boolean
 ?=? : (%, %) -> Boolean                                ?>? : (%, %) -> Boolean
 ?>=? : (%, %) -> Boolean                               D : % -> %
 D : (%, NonNegativeInteger) -> %                       ?^? : (%, %) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 abs : % -> %                                           acos : % -> %
 acosh : % -> %                                         acot : % -> %
 acoth : % -> %                                         acsc : % -> %
 acsch : % -> %                                         annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           asec : % -> %
 asech : % -> %                                         asin : % -> %
 asinh : % -> %                                         associates? : (%, %) -> Boolean
 associator : (%, %, %) -> %                            atan : % -> %
 atan : (%, %) -> %                                     atanh : % -> %
 base : () -> PositiveInteger                           bits : () -> PositiveInteger
 bits : PositiveInteger -> PositiveInteger              ceiling : % -> %
 characteristic : () -> NonNegativeInteger              cis : % -> JLComplexFloat
 cispi : % -> JLComplexFloat                            coerce : % -> %
 coerce : Float -> %                                    coerce : Fraction(Integer) -> %
 coerce : Integer -> %                                  coerce : JLFloat64 -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               convert : % -> DoubleFloat
 convert : % -> Float                                   convert : % -> Pattern(Float)
 convert : % -> String                                  cos : % -> %
 cosh : % -> %                                          cot : % -> %
 coth : % -> %                                          csc : % -> %
 csch : % -> %                                          decreasePrecision : Integer -> PositiveInteger
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 digits : () -> PositiveInteger                         digits : PositiveInteger -> PositiveInteger
 euclideanSize : % -> NonNegativeInteger                exp : () -> %
 exp : % -> %                                           exp1 : () -> %
 exp10 : % -> %                                         exp2 : % -> %
 expm1 : % -> %                                         exponent : % -> Integer
 exprand : () -> %                                      ?exquo? : (%, %) -> Union(%,"failed")
 factor : % -> Factored(%)                              finite? : % -> Boolean
 float : (Integer, Integer) -> %                        float : (Integer, Integer, PositiveInteger) -> %
 floor : % -> %                                         fractionPart : % -> %
 gcd : (%, %) -> %                                      gcd : List(%) -> %
 increasePrecision : Integer -> PositiveInteger         integer? : % -> Boolean
 inv : % -> %                                           jfloat : Float -> %
 jfloat : Fraction(Integer) -> %                        jfloat : Integer -> %
 jfloat : String -> %                                   jinf : () -> %
 jlAbout : % -> Void                                    jlApply : (String, %) -> %
 jlApply : (String, %, %) -> %                          jlApply : (String, %, %, %) -> %
 jlApply : (String, %) -> JLObject                      jlApply : (String, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApprox? : (%, %) -> Boolean
 jlApprox? : (%, %, %) -> Boolean                       jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlId : % -> JLInt64
 jlMPFRApply : (JLObject, %) -> %                       jlMPFRApply : (JLObject, %, %) -> %
 jlMPFRApply : (JLObject, %, %, %) -> %                 jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 jnan : () -> %                                         latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftPower : (%, PositiveInteger) -> %
 leftRecip : % -> Union(%,"failed")                     log : % -> %
 log10 : % -> %                                         log1p : % -> %
 log2 : % -> %                                          mantissa : % -> Integer
 max : (%, %) -> %                                      min : (%, %) -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 negative? : % -> Boolean                               norm : % -> %
 nothing? : % -> Boolean                                nrand : () -> %
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> Integer
 pi : () -> %                                           plenaryPower : (%, PositiveInteger) -> %
 positive? : % -> Boolean                               precision : (%, PositiveInteger) -> %
 precision : () -> PositiveInteger                      precision : % -> PositiveInteger
 precision : PositiveInteger -> PositiveInteger         prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    retract : % -> Fraction(Integer)
 retract : % -> Integer                                 retractIfCan : % -> Union(Integer,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    round : % -> %
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          sign : % -> Integer
 sin : % -> %                                           sinh : % -> %
 sizeLess? : (%, %) -> Boolean                          smaller? : (%, %) -> Boolean
 sqrt : % -> %                                          squareFree : % -> Factored(%)
 squareFreePart : % -> %                                string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tan : % -> %
 tanh : % -> %                                          toString : % -> String
 toString : (%, NonNegativeInteger) -> String           truncate : % -> %
 unit? : % -> Boolean                                   unitCanonical : % -> %
 urand01 : () -> %                                      wholePart : % -> Integer
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %)
 max : () -> % if not % has ATARBEX and not % has ATARBPR
 min : () -> % if not % has ATARBEX and not % has ATARBPR
 multiEuclidean : (List(%), %) -> Union(List(%),"failed")
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%)
 principalIdeal : List(%) -> Record(coef: List(%),generator: %)
 retractIfCan : % -> Union(Fraction(Integer),"failed")
 unitNormal : % -> Record(unit: %,canonical: %,associate: %)
```

## Operations added

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L882)\]

atan(x, y) computes the inverse tangent of x/y.

- **Signature**: `(%,%)->%`

### `cis` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L871)\]

cis(x) returns exp(%i*x) computed efficiently.

- **Signature**: `(%)->JLComplexFloat`

### `cispi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L873)\]

cispi(x) returns cis(%pi*x) computed efficiently.

- **Signature**: `(%)->JLComplexFloat`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L890)\]

coerce(x) coerces x to JLFLoat.

- **Signature**: `(Float)->%`

coerce(x) coerces x to a JLFloat. convenience function.

- **Signature**: `(JLFloat64)->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L867)\]

exp() returns the JLFloat ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `exp1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L869)\]

exp1() returns the JLFloat ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `exp10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L859)\]

exp10(x) computes the base 10 exponential of x.

- **Signature**: `(%)->%`

### `exp2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L857)\]

exp2(x) computes the base 2 exponential of x.

- **Signature**: `(%)->%`

### `expm1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L854)\]

expm1(x) computes accurately ℯ^x-1. It avoids the loss of precision involved in the direct evaluation of exp(x)-1 for small values of x.

- **Signature**: `(%)->%`

### `exprand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L880)\]

exprand() returns an exponentially distributed random number.

- **Signature**: `()->%`

### `finite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L804)\]

finite?(x) tests whether or not x is finite.

- **Signature**: `(%)->Boolean`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L806)\]

integer?(x) tests whether or not x is an integer.

- **Signature**: `(%)->Boolean`

### `jfloat` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L895)\]

jfloat(x) returns x as a JFLoat.

- **Signature**: `(Float)->%`

jfloat(q) returns q as a JFLoat.

- **Signature**: `(Fraction(Integer))->%`

jfloat(z) returns z as a JFLoat. For example: 

**Example**:
```fricas
sqrt(jfloat(2)^2+7.0^2+9.^2)
```

- **Signature**: `(Integer)->%`

jfloat(z) returns z as a JFLoat. For example where automatic coercions is also used:

**Example**:
```fricas
sqrt(jfloat(2.0 )^2+7.0^2+9.^2)
```

- **Signature**: `(String)->%`

### `jinf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L792)\]

jinf() returns the Julia Infinity object.

- **Signature**: `()->%`

### `jlApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L812)\]

jlApply(func, x) applies the function func with x as parameter.

- **Signature**: `(String,%)->%`

jlApply(func, x, y) applies the function func to x and y.

- **Signature**: `(String,%,%)->%`

jlApply(func, x, y, z) applies the function func to x, y and z.

- **Signature**: `(String,%,%,%)->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L794)\]

jlApprox?(x,y) computes inexact equality comparison with default parameters. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%)->Boolean`

jlApprox?(x,y,atol) computes inexact equality comparison with absolute tolerance atol. Two numbers compare equal if their relative distance or their absolute distance is within tolerance bounds.

- **Signature**: `(%,%,%)->Boolean`

### `jlMPFRApply` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L819)\]

jlMPFRApply(func, x) applies the function pointer func to x. It is expected that the C function modifies the first parameters for the returned value (provided by FriCAS and returned here) and and as a last parameter, the rounding mode. See MPFR. Uses Julia rounding mode, default to nearest. 

**Example**:
```fricas
mpfr:=jlDlOpen "libmpfr"
```

E xample: sym:= jlDlSym(mpfr,jsym(mpfr_gamma))

**Example**:
```fricas
val:=jfloat("0.7");
```

**Example**:
```fricas
jlMPFRApply(sym,val) = > 1.29805533264755778568117117915281161778...
```

**Example**:
```fricas
jlDlClose(mpfr)
```

- **Signature**: `(JLObject,%)->%`

jlMPFRApply(func, x, y) applies the function pointer func to x and y. It is expected that the C function modifies the first parameters for the returned value (provided by FriCAS and returned here) and and as a last parameter, the rounding mode. See MPFR. Uses Julia rounding mode, default to nearest.

**Example**:
```fricas
mpfr:=j lDlOpen "li bmpfr"
```

**Example**:
```fricas
sym:= jlDlSym(mpfr,jsym(mpfr_pow))
```

**Example**:
```fricas
val:=jfloat("0.7");
```

**Example**:
```fricas
jlMPFRApply(sym ,val, jfloat(2))
```

**Example**:
```fricas
jlDlClose(mpfr)
```

- **Signature**: `(JLObject,%,%)->%`

jlMPFRApply(func, x, y, z) applies the function pointer func to x, y and z. It is expected that the C function modifies the first parameters for the returned value (provided by FriCAS and returned here) and as a last parameter, the rounding mode (the default mode is used here). See MPFR. Uses Julia rounding mode, default to nearest. 

**Example**:
```fricas
mpfr:=j lDlOpen "libmpfr"
```

**Example**:
```fricas
sym:= jlDlSym(mpfr,jsym(mpfr_fma))
```

**Example**:
```fricas
val:=jfloat( "0.7");
```

**Example**:
```fricas
jlMPFRApply(sym,val, jfloat(2), jfloat(5))
```

**Example**:
```fricas
jlDlClose(mpfr)
```

- **Signature**: `(JLObject,%,%,%)->%`

### `jnan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L790)\]

jnan() returns the Julia NaN (Not a Number) object.

- **Signature**: `()->%`

### `log10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L865)\]

log10(x) computes the base 10 logarithm of x.

- **Signature**: `(%)->%`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L861)\]

log1p(x) computes accurately log(1+x)

- **Signature**: `(%)->%`

### `log2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L863)\]

log2(x) computes the base 2 logarithm of x.

- **Signature**: `(%)->%`

### `nrand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L878)\]

nrand() returns an normally distributed random number.

- **Signature**: `()->%`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L808)\]

precision(x, p) returns a copy of x with precision p.

- **Signature**: `(%,PositiveInteger)->%`

precision(x) returns the precision of x.

- **Signature**: `(%)->PositiveInteger`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad#L875)\]

urand01() returns an uniformly distributed random number contained in [0,1].

- **Signature**: `()->%`
---
[Back to Index](../index.md)
