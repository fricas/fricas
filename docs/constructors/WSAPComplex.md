# WSAPComplex

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1427)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic arbitrary precision complex numbers using Wolfram Symbolic Transport Protocol.

**WSAPComplex(prec: PositiveInteger) is a domain constructor**  
**Abbreviation for WSAPComplex is WSAPCPX**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (WSInteger, %) -> %                              ?*? : (WSAPReal(prec), %) -> %
 ?*? : (%, WSAPReal(prec)) -> %                         ?*? : (PositiveInteger, %) -> %
 ?*? : (%, %) -> %                                      ?*? : (Integer, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      ?-? : (%, %) -> %
 -? : % -> %                                            0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 Chi : % -> %                                           Ci : % -> %
 D : % -> % if WSAPReal(prec) has DIFRING               D : (%, (WSAPReal(prec) -> WSAPReal(prec))) -> %
 Ei : % -> %                                            Shi : % -> %
 Si : % -> %                                            ?^? : (%, %) -> %
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 abs : % -> % if WSAPReal(prec) has RNS                 acos : % -> %
 acosh : % -> %                                         acot : % -> %
 acoth : % -> %                                         acsc : % -> %
 acsch : % -> %                                         annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           asec : % -> %
 asech : % -> %                                         asin : % -> %
 asinh : % -> %                                         associator : (%, %, %) -> %
 atan : (%, %) -> %                                     atan : % -> %
 atanh : % -> %                                         basis : () -> Vector(%)
 characteristic : () -> NonNegativeInteger              coerce : WSInteger -> %
 coerce : Complex(Integer) -> %                         coerce : WSAPReal(prec) -> %
 coerce : % -> %                                        coerce : Integer -> %
 coerce : % -> WSExpression                             coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 complex : (WSAPReal(prec), WSAPReal(prec)) -> %        conjugate : % -> %
 convert : Vector(WSAPReal(prec)) -> %                  convert : % -> Vector(WSAPReal(prec))
 convert : % -> String                                  coordinates : Vector(%) -> Matrix(WSAPReal(prec))
 coordinates : % -> Vector(WSAPReal(prec))              cos : % -> %
 cosh : % -> %                                          cot : % -> %
 coth : % -> %                                          csc : % -> %
 csch : % -> %                                          dilog : % -> %
 discriminant : Vector(%) -> WSAPReal(prec)             discriminant : () -> WSAPReal(prec)
 erf : (%, %) -> %                                      erf : % -> %
 erfc : % -> %                                          erfi : % -> %
 exp : () -> %                                          exp : % -> %
 fresnelC : % -> %                                      fresnelS : % -> %
 generator : () -> %                                    imag : % -> WSAPReal(prec)
 imaginary : () -> %                                    init : () -> % if WSAPReal(prec) has FFIELDC
 integral : (%, SegmentBinding(%)) -> %                 integral : (%, Symbol) -> %
 inv : % -> % if WSAPReal(prec) has FIELD               jWSComplex : (WSAPReal(prec), WSAPReal(prec)) -> %
 jWSComplex : WSAPReal(prec) -> %                       jWSInterpret : (String, String, String) -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : String -> %
 jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObject
 jlApprox? : (%, %) -> Boolean                          jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlEval : % -> %
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlHead : % -> WSSymbol                                 jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlSymbolic : % -> String
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 latex : % -> String                                    leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 li : % -> %                                            log : % -> %
 log10 : % -> %                                         log2 : % -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 norm : % -> WSAPReal(prec)                             nothing? : % -> Boolean
 numeric : (%, PositiveInteger) -> WSExpression         numeric : % -> WSExpression
 numeric? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          pi : () -> %
 plenaryPower : (%, PositiveInteger) -> %               random : () -> % if WSAPReal(prec) has FINITE
 rank : () -> PositiveInteger                           real : % -> WSAPReal(prec)
 recip : % -> Union(%,"failed")                         represents : Vector(WSAPReal(prec)) -> %
 retract : % -> WSAPReal(prec)                          retractIfCan : % -> Union(WSAPReal(prec),"failed")
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sec : % -> %                                           sech : % -> %
 sin : % -> %                                           sinc : % -> %
 sinh : % -> %                                          string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tan : % -> %
 tanh : % -> %                                          toString : (%, WSExpression) -> String
 toString : % -> String                                 trace : % -> WSAPReal(prec)
 traceMatrix : Vector(%) -> Matrix(WSAPReal(prec))      traceMatrix : () -> Matrix(WSAPReal(prec))
 urand01 : () -> %                                      zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 ?*? : (%, Integer) -> % if WSAPReal(prec) has LINEXP(INT)
 ?*? : (%, Fraction(Integer)) -> % if % has CHARZ and WSAPReal(prec) has FIELD or % has CHARZ and WSAPReal(prec) has FFIELDC
 ?*? : (Fraction(Integer), %) -> % if % has CHARZ and WSAPReal(prec) has FIELD or % has CHARZ and WSAPReal(prec) has FFIELDC
 ?/? : (%, %) -> % if WSAPReal(prec) has FIELD
 D : (%, NonNegativeInteger) -> % if WSAPReal(prec) has DIFRING
 D : (%, Symbol) -> % if WSAPReal(prec) has PDRING(SYMBOL)
 D : (%, List(Symbol)) -> % if WSAPReal(prec) has PDRING(SYMBOL)
 D : (%, Symbol, NonNegativeInteger) -> % if WSAPReal(prec) has PDRING(SYMBOL)
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if WSAPReal(prec) has PDRING(SYMBOL)
 D : (%, (WSAPReal(prec) -> WSAPReal(prec)), NonNegativeInteger) -> %
 ?^? : (%, Integer) -> % if WSAPReal(prec) has FIELD
 ?^? : (%, Fraction(Integer)) -> % if WSAPReal(prec) has RADCAT and WSAPReal(prec) has TRANFUN
 argument : % -> WSAPReal(prec) if WSAPReal(prec) has TRANFUN
 associates? : (%, %) -> Boolean if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has INTDOM
 characteristicPolynomial : % -> SparseUnivariatePolynomial(WSAPReal(prec))
 charthRoot : % -> % if WSAPReal(prec) has FFIELDC
 charthRoot : % -> Union(%,"failed") if % has CHARNZ and WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or % has CHARNZ and WSAPReal(prec) has FFIELDC
 coerce : Fraction(Integer) -> % if % has CHARZ and WSAPReal(prec) has FIELD or % has CHARZ and WSAPReal(prec) has FFIELDC or WSAPReal(prec) has RETRACT(FRAC(INT))
 conditionP : Matrix(%) -> Union(Vector(%),"failed") if % has CHARNZ and WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or % has CHARNZ and WSAPReal(prec) has FFIELDC
 convert : % -> SparseUnivariatePolynomial(WSAPReal(prec))
 convert : SparseUnivariatePolynomial(WSAPReal(prec)) -> %
 convert : % -> Pattern(Integer) if WSAPReal(prec) has KONVERT(PATTERN(INT))
 convert : % -> Pattern(Float) if WSAPReal(prec) has KONVERT(PATTERN(FLOAT))
 convert : % -> Complex(Float) if WSAPReal(prec) has REAL
 convert : % -> Complex(DoubleFloat) if WSAPReal(prec) has REAL
 convert : % -> InputForm if WSAPReal(prec) has KONVERT(INFORM)
 coordinates : (%, Vector(%)) -> Vector(WSAPReal(prec))
 coordinates : (Vector(%), Vector(%)) -> Matrix(WSAPReal(prec))
 createPrimitiveElement : () -> % if WSAPReal(prec) has FFIELDC
 definingPolynomial : () -> SparseUnivariatePolynomial(WSAPReal(prec))
 derivationCoordinates : (Vector(%), (WSAPReal(prec) -> WSAPReal(prec))) -> Matrix(WSAPReal(prec)) if WSAPReal(prec) has FIELD
 differentiate : % -> % if WSAPReal(prec) has DIFRING
 differentiate : (%, NonNegativeInteger) -> % if WSAPReal(prec) has DIFRING
 differentiate : (%, Symbol) -> % if WSAPReal(prec) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if WSAPReal(prec) has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if WSAPReal(prec) has PDRING(SYMBOL)
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if WSAPReal(prec) has PDRING(SYMBOL)
 differentiate : (%, (WSAPReal(prec) -> WSAPReal(prec)), NonNegativeInteger) -> %
 differentiate : (%, (WSAPReal(prec) -> WSAPReal(prec))) -> %
 discreteLog : % -> NonNegativeInteger if WSAPReal(prec) has FFIELDC
 discreteLog : (%, %) -> Union(NonNegativeInteger,"failed") if WSAPReal(prec) has FFIELDC
 divide : (%, %) -> Record(quotient: %,remainder: %) if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 elt : (%, WSAPReal(prec)) -> % if WSAPReal(prec) has ELTAB(WSAPR(prec),WSAPR(prec))
 enumerate : () -> List(%) if WSAPReal(prec) has FINITE
 euclideanSize : % -> NonNegativeInteger if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 eval : (%, List(WSAPReal(prec)), List(WSAPReal(prec))) -> % if WSAPReal(prec) has EVALAB(WSAPR(prec))
 eval : (%, WSAPReal(prec), WSAPReal(prec)) -> % if WSAPReal(prec) has EVALAB(WSAPR(prec))
 eval : (%, Equation(WSAPReal(prec))) -> % if WSAPReal(prec) has EVALAB(WSAPR(prec))
 eval : (%, List(Equation(WSAPReal(prec)))) -> % if WSAPReal(prec) has EVALAB(WSAPR(prec))
 eval : (%, List(Symbol), List(WSAPReal(prec))) -> % if WSAPReal(prec) has IEVALAB(SYMBOL,WSAPR(prec))
 eval : (%, Symbol, WSAPReal(prec)) -> % if WSAPReal(prec) has IEVALAB(SYMBOL,WSAPR(prec))
 expressIdealMember : (List(%), %) -> Union(List(%),"failed") if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 ?exquo? : (%, WSAPReal(prec)) -> Union(%,"failed") if WSAPReal(prec) has INTDOM
 ?exquo? : (%, %) -> Union(%,"failed") if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has INTDOM
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed") if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %) if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 factor : % -> Factored(%) if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 factorPolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FFIELDC
 factorSquareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FFIELDC
 factorsOfCyclicGroupSize : () -> List(Record(factor: Integer,exponent: NonNegativeInteger)) if WSAPReal(prec) has FFIELDC
 gcd : List(%) -> % if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 gcd : (%, %) -> % if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%) if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 hash : % -> SingleInteger if WSAPReal(prec) has HASHABL
 hashUpdate! : (HashState, %) -> HashState if WSAPReal(prec) has HASHABL
 index : PositiveInteger -> % if WSAPReal(prec) has FINITE
 lcm : List(%) -> % if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 lcm : (%, %) -> % if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 lcmCoef : (%, %) -> Record(llcm_res: %,coeff1: %,coeff2: %) if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 lift : % -> SparseUnivariatePolynomial(WSAPReal(prec))
 lookup : % -> PositiveInteger if WSAPReal(prec) has FINITE
 map : ((WSAPReal(prec) -> WSAPReal(prec)), %) -> %
 minimalPolynomial : % -> SparseUnivariatePolynomial(WSAPReal(prec)) if WSAPReal(prec) has FIELD
 multiEuclidean : (List(%), %) -> Union(List(%),"failed") if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 nextItem : % -> Union(%,"failed") if WSAPReal(prec) has FFIELDC
 nthRoot : (%, Integer) -> % if WSAPReal(prec) has RADCAT and WSAPReal(prec) has TRANFUN
 order : % -> PositiveInteger if WSAPReal(prec) has FFIELDC
 order : % -> OnePointCompletion(PositiveInteger) if WSAPReal(prec) has FFIELDC
 patternMatch : (%, Pattern(Integer), PatternMatchResult(Integer,%)) -> PatternMatchResult(Integer,%) if WSAPReal(prec) has PATMAB(INT)
 patternMatch : (%, Pattern(Float), PatternMatchResult(Float,%)) -> PatternMatchResult(Float,%) if WSAPReal(prec) has PATMAB(FLOAT)
 polarCoordinates : % -> Record(r: WSAPReal(prec),phi: WSAPReal(prec)) if WSAPReal(prec) has RNS and WSAPReal(prec) has TRANFUN
 prime? : % -> Boolean if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 primeFrobenius : (%, NonNegativeInteger) -> % if WSAPReal(prec) has FFIELDC
 primeFrobenius : % -> % if WSAPReal(prec) has FFIELDC
 primitive? : % -> Boolean if WSAPReal(prec) has FFIELDC
 primitiveElement : () -> % if WSAPReal(prec) has FFIELDC
 principalIdeal : List(%) -> Record(coef: List(%),generator: %) if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 ?quo? : (%, %) -> % if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 rational : % -> Fraction(Integer) if WSAPReal(prec) has INS
 rational? : % -> Boolean if WSAPReal(prec) has INS
 rationalIfCan : % -> Union(Fraction(Integer),"failed") if WSAPReal(prec) has INS
 reduce : SparseUnivariatePolynomial(WSAPReal(prec)) -> %
 reduce : Fraction(SparseUnivariatePolynomial(WSAPReal(prec))) -> Union(%,"failed") if WSAPReal(prec) has FIELD
 reducedSystem : Matrix(%) -> Matrix(Integer) if WSAPReal(prec) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(Integer),vec: Vector(Integer)) if WSAPReal(prec) has LINEXP(INT)
 reducedSystem : (Matrix(%), Vector(%)) -> Record(mat: Matrix(WSAPReal(prec)),vec: Vector(WSAPReal(prec)))
 reducedSystem : Matrix(%) -> Matrix(WSAPReal(prec))
 regularRepresentation : (%, Vector(%)) -> Matrix(WSAPReal(prec))
 regularRepresentation : % -> Matrix(WSAPReal(prec))
 ?rem? : (%, %) -> % if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 representationType : () -> Union("prime",polynomial,normal,cyclic) if WSAPReal(prec) has FFIELDC
 represents : (Vector(WSAPReal(prec)), Vector(%)) -> %
 retract : % -> Integer if WSAPReal(prec) has RETRACT(INT)
 retract : % -> Fraction(Integer) if WSAPReal(prec) has RETRACT(FRAC(INT))
 retractIfCan : % -> Union(Integer,"failed") if WSAPReal(prec) has RETRACT(INT)
 retractIfCan : % -> Union(Fraction(Integer),"failed") if WSAPReal(prec) has RETRACT(FRAC(INT))
 size : () -> NonNegativeInteger if WSAPReal(prec) has FINITE
 sizeLess? : (%, %) -> Boolean if WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 smaller? : (%, %) -> Boolean if WSAPReal(prec) has COMPAR
 solveLinearPolynomialEquation : (List(SparseUnivariatePolynomial(%)), SparseUnivariatePolynomial(%)) -> Union(List(SparseUnivariatePolynomial(%)),"failed") if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FFIELDC
 sqrt : % -> % if WSAPReal(prec) has FIELD and WSAPReal(prec) has ORDRING and WSAPReal(prec) has RADCAT or WSAPReal(prec) has RADCAT and WSAPReal(prec) has TRANFUN
 squareFree : % -> Factored(%) if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 squareFreePart : % -> % if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FIELD or WSAPReal(prec) has INS
 squareFreePolynomial : SparseUnivariatePolynomial(%) -> Factored(SparseUnivariatePolynomial(%)) if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has FFIELDC
 tableForDiscreteLogarithm : Integer -> Table(PositiveInteger,NonNegativeInteger) if WSAPReal(prec) has FFIELDC
 unit? : % -> Boolean if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has INTDOM
 unitCanonical : % -> % if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has INTDOM
 unitNormal : % -> Record(unit: %,canonical: %,associate: %) if WSAPReal(prec) has EUCDOM and WSAPReal(prec) has PFECAT or WSAPReal(prec) has INTDOM
```

## Operations added

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1456)\]

atan(z1,z2) computes the arc tangent of z2/z1.

- **Signature**: `(%,%)->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1465)\]

coerce(gi) coerces gi. Convenience function.

- **Signature**: `(Complex(Integer))->%`

coerce(i): convenience function.

- **Signature**: `(Integer)->%`

coerce(int): coerces int. Convenience function.

- **Signature**: `(WSInteger)->%`

### `complex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1471)\]

complex(re, im) returns the complex number from real part re and imaginary part im.

- **Signature**: `(WSAPReal(prec),WSAPReal(prec))->%`

### `erf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1458)\]

erf(z) computes the error function of z.

- **Signature**: `(%,%)->%`

### `erfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1460)\]

erfc(z) returns the complementary error function of z.

- **Signature**: `(%)->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1448)\]

exp() returns the WSAPReal ℯ (%e or exp(1)).

- **Signature**: `()->%`

### `jWSComplex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1474)\]

jWSComplex(re) constructs a WSComplex with real part re.

- **Signature**: `(WSAPReal(prec))->%`

jWSComplex(re, im) constructs a WSComplex from real part re and imaginary part im.

- **Signature**: `(WSAPReal(prec),WSAPReal(prec))->%`

### `jlApprox?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1443)\]

jlApprox?(x,y) computes inexact equality comparison with WS default parameters (Equal).

- **Signature**: `(%,%)->Boolean`

### `log10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1454)\]

log10(z) computes the logarithm of z in base 10.

- **Signature**: `(%)->%`

### `log2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1452)\]

log2(z) computes the logarithm of z in base 2.

- **Signature**: `(%)->%`

### `sinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1450)\]

sinc(z) computes the unnormalized sinc of z, sin(z)/z and 0 if z = 0.

- **Signature**: `(%)->%`

### `toString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1462)\]

toString(expr, form) returns the string representation of expr with WS language format form.

- **Signature**: `(%,WSExpression)->String`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1441)\]

urand01() returns a unit square random complex number.

- **Signature**: `()->%`
---
[Back to Index](../index.md)
