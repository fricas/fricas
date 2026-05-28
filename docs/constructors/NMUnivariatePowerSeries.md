# NMUnivariatePowerSeries

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L1)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

NMUnivariatePowerSeries is the Nemo univariate power series using Julia. prec determines the precision used which can be absolute or relative (:capped_absolute or :capped_relative). x := x::NUPS(NFRAC(NINT),'x,30,"capped_relative") sin(x)

**NMUnivariatePowerSeries(R: NMRing,x: Symbol,prec: PositiveInteger,abs: JLSymbol) is a domain constructor**  
**Abbreviation for NMUnivariatePowerSeries is NUPS**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?*? : (%, PositiveInteger) -> %                        ?*? : (%, NonNegativeInteger) -> %
 ?*? : (%, Integer) -> %                                ?*? : (%, R) -> %
 ?*? : (R, %) -> %                                      ?*? : (PositiveInteger, %) -> %
 ?*? : (%, %) -> %                                      ?*? : (Integer, %) -> %
 ?*? : (NonNegativeInteger, %) -> %                     ?*? : (NMInteger, %) -> JLObject
 ?+? : (%, %) -> %                                      ?-? : (%, %) -> %
 -? : % -> %                                            0 : () -> %
 1 : () -> %                                            ?=? : (%, %) -> Boolean
 D : (%, Symbol) -> % if R has PDRING(SYMBOL)           D : % -> %
 D : (%, NonNegativeInteger) -> %                       ?^? : (%, PositiveInteger) -> %
 ?^? : (%, NonNegativeInteger) -> %                     acos : % -> % if R has ALGEBRA(NFRAC(NINT))
 acosh : % -> % if R has ALGEBRA(NFRAC(NINT))           acot : % -> % if R has ALGEBRA(NFRAC(NINT))
 acoth : % -> % if R has ALGEBRA(NFRAC(NINT))           acsc : % -> % if R has ALGEBRA(NFRAC(NINT))
 acsch : % -> % if R has ALGEBRA(NFRAC(NINT))           annihilate? : (%, %) -> Boolean
 antiCommutator : (%, %) -> %                           asec : % -> % if R has ALGEBRA(NFRAC(NINT))
 asech : % -> % if R has ALGEBRA(NFRAC(NINT))           asin : % -> % if R has ALGEBRA(NFRAC(NINT))
 asinh : % -> % if R has ALGEBRA(NFRAC(NINT))           associator : (%, %, %) -> %
 atan : % -> % if R has ALGEBRA(NFRAC(NINT))            atanh : % -> % if R has ALGEBRA(NFRAC(NINT))
 characteristic : () -> NonNegativeInteger              coefficient : (%, NonNegativeInteger) -> R
 coerce : Variable(x) -> %                              coerce : % -> %
 coerce : Integer -> %                                  coerce : % -> JLObject
 coerce : % -> OutputForm                               commutator : (%, %) -> %
 complete : % -> %                                      convert : % -> String
 cos : % -> % if R has ALGEBRA(NFRAC(NINT))             cosh : % -> % if R has ALGEBRA(NFRAC(NINT))
 cot : % -> % if R has ALGEBRA(NFRAC(NINT))             coth : % -> % if R has ALGEBRA(NFRAC(NINT))
 csc : % -> % if R has ALGEBRA(NFRAC(NINT))             csch : % -> % if R has ALGEBRA(NFRAC(NINT))
 degree : % -> NonNegativeInteger                       differentiate : % -> %
 differentiate : (%, NonNegativeInteger) -> %           equal? : (%, %) -> Boolean
 exact? : % -> Boolean                                  exactDivide : (%, %) -> %
 exp : % -> % if R has ALGEBRA(NFRAC(NINT))             integrate : % -> %
 inverse : % -> %                                       jlAbout : % -> Void
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
 latex : % -> String                                    leadingCoefficient : % -> R
 leadingMonomial : % -> %                               leftPower : (%, PositiveInteger) -> %
 leftPower : (%, NonNegativeInteger) -> %               leftRecip : % -> Union(%,"failed")
 log : % -> % if R has ALGEBRA(NFRAC(NINT))             map : ((R -> R), %) -> %
 missing? : % -> Boolean                                monomial : (R, NonNegativeInteger) -> %
 monomial? : % -> Boolean                               mutable? : % -> Boolean
 nothing? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          plenaryPower : (%, PositiveInteger) -> %
 pole? : % -> Boolean                                   recip : % -> Union(%,"failed")
 rightPower : (%, PositiveInteger) -> %                 rightPower : (%, NonNegativeInteger) -> %
 rightRecip : % -> Union(%,"failed")                    sample : () -> %
 sec : % -> % if R has ALGEBRA(NFRAC(NINT))             sech : % -> % if R has ALGEBRA(NFRAC(NINT))
 sin : % -> % if R has ALGEBRA(NFRAC(NINT))             sinh : % -> % if R has ALGEBRA(NFRAC(NINT))
 sqrt : % -> %                                          string : % -> String
 subtractIfCan : (%, %) -> Union(%,"failed")            tan : % -> % if R has ALGEBRA(NFRAC(NINT))
 tanh : % -> % if R has ALGEBRA(NFRAC(NINT))            unit? : % -> Boolean
 valuation : % -> NonNegativeInteger                    variable : % -> Symbol
 zero? : % -> Boolean                                   ?~=? : (%, %) -> Boolean
 ?*? : (%, Fraction(Integer)) -> % if R has ALGEBRA(FRAC(INT))
 ?*? : (Fraction(Integer), %) -> % if R has ALGEBRA(FRAC(INT))
 ?+? : (%, Fraction(Integer)) -> % if R has ALGEBRA(NFRAC(NINT))
 ?-? : (%, Fraction(Integer)) -> % if R has ALGEBRA(NFRAC(NINT))
 ?/? : (%, R) -> % if R has ALGEBRA(NFRAC(NINT)) or R has FIELD
 ?/? : (%, NMInteger) -> % if R has ALGEBRA(NFRAC(NINT))
 ?/? : (%, Integer) -> % if R has ALGEBRA(NFRAC(NINT))
 D : (%, List(Symbol), List(NonNegativeInteger)) -> % if R has PDRING(SYMBOL)
 D : (%, Symbol, NonNegativeInteger) -> % if R has PDRING(SYMBOL)
 D : (%, List(Symbol)) -> % if R has PDRING(SYMBOL)
 associates? : (%, %) -> Boolean if % has ATVCWC and R has INTDOM
 coerce : Fraction(Integer) -> % if R has ALGEBRA(FRAC(INT))
 coerce : R -> % if % has ATVCWC and R has COMRING
 construct : List(Record(k: NonNegativeInteger,c: R)) -> %
 constructOrdered : List(Record(k: NonNegativeInteger,c: R)) -> % if NonNegativeInteger has COMPAR
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if R has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if R has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if R has PDRING(SYMBOL)
 differentiate : (%, Symbol) -> % if R has PDRING(SYMBOL)
 ?exquo? : (%, %) -> Union(%,"failed") if % has ATVCWC and R has INTDOM
 leadingSupport : % -> NonNegativeInteger if NonNegativeInteger has COMPAR
 leadingTerm : % -> Record(k: NonNegativeInteger,c: R) if NonNegativeInteger has COMPAR
 reductum : % -> % if NonNegativeInteger has COMPAR
 unitCanonical : % -> % if % has ATVCWC and R has INTDOM
 unitNormal : % -> Record(unit: %,canonical: %,associate: %) if % has ATVCWC and R has INTDOM
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L73)\]

coerce(x) converts the variable x to a Nemo univariate power series.

- **Signature**: `(Variable(x))->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L57)\]

exp(s) returns the exponential of the power series s.

- **Signature**: `(%)->%`

### `integrate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L49)\]

integrate(f(x)) returns an anti-derivative of the power series f(x) with constant coefficient 0.

- **Signature**: `(%)->%`

### `inverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L39)\]

inverse(s) returns the inverse of s. Throws a Julia error if no such inverse exists.

- **Signature**: `(%)->%`

### `log` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L59)\]

log(s) returns the logarithm of the power series s.

- **Signature**: `(%)->%`

### `monomial?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L45)\]

monomial?(f) tests if f is a single monomial.

- **Signature**: `(%)->Boolean`

### `sqrt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L42)\]

sqrt(f) returns the square root of f if it exists, a Julia error is thrown if f has no square root.

- **Signature**: `(%)->%`

### `valuation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L36)\]

valuation(s) returns the valuation of the given power series, the degree of the first nonzero term

- **Signature**: `(%)->NonNegativeInteger`

### `variable` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L33)\]

variable(f) returns the (unique) power series variable of the power series f.

- **Signature**: `(%)->Symbol`
---
[Back to Index](../index.md)
