# NMRealBall

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L346)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

convenience domain to reflect Nemo ArbField(256), i.e. without parameters.

**NMRealBall is a domain constructor.**  
**Abbreviation for NMRealBall is NRB**  
**This constructor is exposed in this frame.**  
**174 names for 231 operations in this domain.**

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
 D : (%, NonNegativeInteger) -> %                       Gamma : % -> %
 Gamma : (%, %) -> %                                    ?^? : (%, %) -> %
 ?^? : (%, Fraction(Integer)) -> %                      ?^? : (%, Integer) -> %
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 abs : % -> %                                           abs2 : % -> %
 accuracyBits : % -> JLInt64                            acos : % -> %
 acosh : % -> %                                         acot : % -> %
 acoth : % -> %                                         acsc : % -> %
 acsch : % -> %                                         addError! : (%, %) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 asec : % -> %                                          asech : % -> %
 asin : % -> %                                          asinh : % -> %
 associates? : (%, %) -> Boolean                        associator : (%, %, %) -> %
 atan : % -> %                                          atan : (%, %) -> %
 atanh : % -> %                                         base : () -> PositiveInteger
 bits : % -> JLInt64                                    bits : () -> PositiveInteger
 bits : PositiveInteger -> PositiveInteger              catalan : () -> %
 ceiling : % -> %                                       characteristic : () -> NonNegativeInteger
 coerce : % -> %                                        coerce : Float -> %
 coerce : Fraction(Integer) -> %                        coerce : Integer -> %
 coerce : JLFloat64 -> %                                coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 contains? : (%, %) -> Boolean                          contains? : (%, JLFloat) -> Boolean
 contains? : (%, NMFraction(NMInteger)) -> Boolean      contains? : (%, NMInteger) -> Boolean
 containsNegative? : % -> Boolean                       containsNonNegative? : % -> Boolean
 containsNonPositive? : % -> Boolean                    containsPositive? : % -> Boolean
 containsZero? : % -> Boolean                           convert : % -> DoubleFloat
 convert : % -> Float                                   convert : % -> JLFloat
 convert : % -> NMFraction(NMInteger)                   convert : % -> Pattern(Float)
 convert : % -> String                                  cos : % -> %
 cosh : % -> %                                          cot : % -> %
 coth : % -> %                                          csc : % -> %
 csch : % -> %                                          decreasePrecision : Integer -> PositiveInteger
 differentiate : % -> %                                 differentiate : (%, NonNegativeInteger) -> %
 digits : () -> PositiveInteger                         digits : PositiveInteger -> PositiveInteger
 equal? : (%, %) -> Boolean                             euclideanSize : % -> NonNegativeInteger
 eulerGamma : () -> %                                   exact? : % -> Boolean
 exactDivide : (%, %) -> %                              exp : () -> %
 exp : % -> %                                           exp1 : () -> %
 expm1 : % -> %                                         exponent : % -> Integer
 ?exquo? : (%, %) -> Union(%,"failed")                  factor : % -> Factored(%)
 finite? : % -> Boolean                                 float : (Integer, Integer) -> %
 float : (Integer, Integer, PositiveInteger) -> %       floor : % -> %
 fractionPart : % -> %                                  gcd : (%, %) -> %
 gcd : List(%) -> %                                     glaisher : () -> %
 hurwitzZeta : (%, %) -> %                              increasePrecision : Integer -> PositiveInteger
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
 jlref : String -> %                                    jnball : (%, %) -> %
 jnrb : Float -> %                                      jnrb : Fraction(Integer) -> %
 jnrb : Integer -> %                                    jnrb : NMAlgebraicNumber -> %
 jnrb : NMExactCalciumField -> %                        jnrb : String -> %
 khinchin : () -> %                                     latex : % -> String
 lcm : (%, %) -> %                                      lcm : List(%) -> %
 ldexp : (%, NMInteger) -> %                            leftPower : (%, NonNegativeInteger) -> %
 leftPower : (%, PositiveInteger) -> %                  leftRecip : % -> Union(%,"failed")
 log : % -> %                                           log1p : % -> %
 mantissa : % -> Integer                                max : (%, %) -> %
 midpoint : % -> %                                      min : (%, %) -> %
 missing? : % -> Boolean                                mutable? : % -> Boolean
 negative? : % -> Boolean                               nonNegative? : % -> Boolean
 nonPositive? : % -> Boolean                            nonZero? : % -> Boolean
 norm : % -> %                                          nothing? : % -> Boolean
 nthRoot : (%, Integer) -> %                            one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          order : % -> Integer
 overlaps? : (%, %) -> Boolean                          pi : () -> %
 plenaryPower : (%, PositiveInteger) -> %               polyLog : (%, %) -> %
 positive? : % -> Boolean                               precision : () -> PositiveInteger
 precision : PositiveInteger -> PositiveInteger         prime? : % -> Boolean
 ?quo? : (%, %) -> %                                    radius : % -> %
 randtest : JLSymbol -> %                               recip : % -> Union(%,"failed")
 ?rem? : (%, %) -> %                                    retract : % -> Fraction(Integer)
 retract : % -> Integer                                 retractIfCan : % -> Union(Integer,"failed")
 rightPower : (%, NonNegativeInteger) -> %              rightPower : (%, PositiveInteger) -> %
 rightRecip : % -> Union(%,"failed")                    round : % -> %
 sample : () -> %                                       sec : % -> %
 sech : % -> %                                          setUnion : (%, %) -> %
 sign : % -> Integer                                    sin : % -> %
 sinh : % -> %                                          sizeLess? : (%, %) -> Boolean
 smaller? : (%, %) -> Boolean                           sqrt : % -> %
 squareFree : % -> Factored(%)                          squareFreePart : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 tan : % -> %                                           tanh : % -> %
 toString : % -> String                                 toString : (%, NonNegativeInteger) -> String
 trim : % -> %                                          truncate : % -> %
 uniqueInteger : % -> Union(NMInteger,"failed")         unit? : % -> Boolean
 unitCanonical : % -> %                                 urand01 : () -> %
 wholePart : % -> Integer                               zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
 divide : (%, %) -> Record(quotient: %,remainder: %)
 expressIdealMember : (List(%), %) -> Union(List(%),"failed")
 extendedEuclidean : (%, %) -> Record(coef1: %,coef2: %,generator: %)
 extendedEuclidean : (%, %, %) -> Union(Record(coef1: %,coef2: %),"failed")
 gcdPolynomial : (SparseUnivariatePolynomial(%), SparseUnivariatePolynomial(%)) -> SparseUnivariatePolynomial(%)
 guess : (%, NonNegativeInteger) -> NMAlgebraicNumber
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

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L422)\]

- **Signature**: `(%)->%`
- **Signature**: `(%,%)->%`

### `abs2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L402)\]

- **Signature**: `(%)->%`

### `accuracyBits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L466)\]

- **Signature**: `(%)->JLInt64`

### `addError!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L759)\]

- **Signature**: `(%,%)->%`

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L753)\]

- **Signature**: `(%,%)->%`

### `bits` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L458)\]

- **Signature**: `(%)->JLInt64`

### `catalan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L710)\]

- **Signature**: `()->%`

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L480)\]

- **Signature**: `(Float)->%`
- **Signature**: `(JLFloat64)->%`

### `contains?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L378)\]

- **Signature**: `(%,%)->Boolean`
- **Signature**: `(%,JLFloat)->Boolean`
- **Signature**: `(%,NMFraction(NMInteger))->Boolean`
- **Signature**: `(%,NMInteger)->Boolean`

### `containsNegative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L786)\]

- **Signature**: `(%)->Boolean`

### `containsNonNegative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L789)\]

- **Signature**: `(%)->Boolean`

### `containsNonPositive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L792)\]

- **Signature**: `(%)->Boolean`

### `containsPositive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L783)\]

- **Signature**: `(%)->Boolean`

### `containsZero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L384)\]

- **Signature**: `(%)->Boolean`

### `convert` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L800)\]

- **Signature**: `(%)->JLFloat`
- **Signature**: `(%)->NMFraction(NMInteger)`

### `eulerGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L708)\]

- **Signature**: `()->%`

### `exact?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L374)\]

- **Signature**: `(%)->Boolean`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L414)\]

- **Signature**: `()->%`

### `exp1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L416)\]

- **Signature**: `()->%`

### `expm1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L409)\]

- **Signature**: `(%)->%`

### `finite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L371)\]

- **Signature**: `(%)->Boolean`

### `glaisher` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L714)\]

- **Signature**: `()->%`

### `guess` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L477)\]

- **Signature**: `(%,NonNegativeInteger)->NMAlgebraicNumber`

### `hurwitzZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L420)\]

- **Signature**: `(%,%)->%`

### `integer?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L369)\]

- **Signature**: `(%)->Boolean`

### `jnball` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L810)\]

- **Signature**: `(%,%)->%`

### `jnrb`

- **Signature**: `(Float)->%`
- **Signature**: `(Fraction(Integer))->%`
- **Signature**: `(Integer)->%`
- **Signature**: `(NMAlgebraicNumber)->%`
- **Signature**: `(NMExactCalciumField)->%`
- **Signature**: `(String)->%`

### `khinchin` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L712)\]

- **Signature**: `()->%`

### `ldexp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L418)\]

- **Signature**: `(%,NMInteger)->%`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L412)\]

- **Signature**: `(%)->%`

### `midpoint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L755)\]

- **Signature**: `(%)->%`

### `nonNegative?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L700)\]

- **Signature**: `(%)->Boolean`

### `nonPositive?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L702)\]

- **Signature**: `(%)->Boolean`

### `nonZero?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L698)\]

- **Signature**: `(%)->Boolean`

### `overlaps?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L376)\]

- **Signature**: `(%,%)->Boolean`

### `polyLog` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L428)\]

- **Signature**: `(%,%)->%`

### `radius` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L757)\]

- **Signature**: `(%)->%`

### `randtest` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L391)\]

- **Signature**: `(JLSymbol)->%`

### `setUnion` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L769)\]

- **Signature**: `(%,%)->%`

### `trim` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L468)\]

- **Signature**: `(%)->%`

### `uniqueInteger` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L470)\]

- **Signature**: `(%)->Union(NMInteger,"failed")`

### `urand01` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnball.spad#L388)\]

- **Signature**: `()->%`
---
[Back to Index](../index.md)
