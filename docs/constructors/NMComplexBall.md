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

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L747)\]

- **Signature**: `(%)->%`
- **Signature**: `(%,%)->%`

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L732)\]

- **Signature**: `(%)->%`

### `accuracyBits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L762)\]

- **Signature**: `(%)->JLInt64`

### `airyAi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1076)\]

- **Signature**: `(R)->R`

### `airyAiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1078)\]

- **Signature**: `(R)->R`

### `airyBi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1080)\]

- **Signature**: `(R)->R`

### `airyBiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1082)\]

- **Signature**: `(R)->R`

### `besselI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1088)\]

- **Signature**: `(%,%)->%`

### `besselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1090)\]

- **Signature**: `(%,%)->%`

### `besselK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1092)\]

- **Signature**: `(%,%)->%`

### `besselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1094)\]

- **Signature**: `(%,%)->%`

### `bits`

- **Signature**: `(%)->JLInt64`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L804)\]

- **Signature**: `(Complex(Integer))->%`
- **Signature**: `(Float)->%`

### `contains?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L773)\]

- **Signature**: `(%,%)->Boolean`
- **Signature**: `(%,NMFraction(NMInteger))->Boolean`
- **Signature**: `(%,NMInteger)->Boolean`

### `containsZero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L781)\]

- **Signature**: `(%)->Boolean`

### `ellipticE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1084)\]

- **Signature**: `(%)->%`

### `ellipticK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1086)\]

- **Signature**: `(%)->%`

### `exact?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L696)\]

- **Signature**: `(%)->Boolean`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L704)\]

- **Signature**: `()->%`

### `exp1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L706)\]

- **Signature**: `()->%`

### `expm1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L738)\]

- **Signature**: `(%)->%`

### `finite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L693)\]

- **Signature**: `(%)->Boolean`

### `guess` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L795)\]

- **Signature**: `(%,NonNegativeInteger)->NMAlgebraicNumber`

### `hurwitzZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L745)\]

- **Signature**: `(%,%)->%`

### `hypergeometric1F1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1096)\]

- **Signature**: `(%,%,%)->%`

### `hypergeometric1F1Regularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1099)\]

- **Signature**: `(%,%,%)->%`

### `hypergeometricU` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1102)\]

- **Signature**: `(%,%,%)->%`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L691)\]

- **Signature**: `(%)->Boolean`

### `jncb`

- **Signature**: `(Float)->%`
- **Signature**: `(Float,Float)->%`
- **Signature**: `(Integer)->%`
- **Signature**: `(Integer,Integer)->%`
- **Signature**: `(NMAlgebraicNumber)->%`
- **Signature**: `(NMExactCalciumField)->%`
- **Signature**: `(String)->%`
- **Signature**: `(String,String)->%`

### `ldexp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L743)\]

- **Signature**: `(%,NMInteger)->%`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L741)\]

- **Signature**: `(%)->%`

### `overlaps?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L771)\]

- **Signature**: `(%,%)->Boolean`

### `pi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1058)\]

- **Signature**: `()->%`

### `polyLog` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L751)\]

- **Signature**: `(%,%)->%`

### `polygamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1072)\]

- **Signature**: `(%,%)->%`

### `precision` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1125)\]

- **Signature**: `()->PositiveInteger`

### `randtest` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L719)\]

- **Signature**: `(JLSymbol)->%`

### `rootOfUnity` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L1107)\]

- **Signature**: `(NonNegativeInteger)->%`

### `trim` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L764)\]

- **Signature**: `(%)->%`

### `uniqueInteger` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L766)\]

- **Signature**: `(%)->Union(NMInteger,"failed")`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L716)\]

- **Signature**: `()->%`
---
[Back to Index](../index.md)
