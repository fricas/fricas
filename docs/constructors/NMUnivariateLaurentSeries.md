# NMUnivariateLaurentSeries

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L195)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

NMUnivariateLaurentSeries is the Nemo univariate Laurent series using Julia. x := x::NULS(NFRAC(NINT),'x,30) x^(-1)

**NMUnivariateLaurentSeries(R: NMRing,x: Symbol,prec: PositiveInteger) is a domain constructor**  
**Abbreviation for NMUnivariateLaurentSeries is NULS**  
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
 D : (%, Symbol) -> % if R has PDRING(SYMBOL)           D : (%, NonNegativeInteger) -> %
 D : % -> %                                             ?^? : (%, Integer) -> %
 ?^? : (%, PositiveInteger) -> %                        ?^? : (%, NonNegativeInteger) -> %
 annihilate? : (%, %) -> Boolean                        antiCommutator : (%, %) -> %
 associator : (%, %, %) -> %                            characteristic : () -> NonNegativeInteger
 coefficient : (%, Integer) -> R                        coerce : Variable(x) -> %
 coerce : % -> %                                        coerce : Integer -> %
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 commutator : (%, %) -> %                               complete : % -> %
 construct : List(Record(k: Integer,c: R)) -> %         convert : % -> String
 degree : % -> Integer                                  differentiate : (%, NonNegativeInteger) -> %
 differentiate : % -> %                                 equal? : (%, %) -> Boolean
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
 missing? : % -> Boolean                                monomial : (R, Integer) -> %
 monomial? : % -> Boolean                               mutable? : % -> Boolean
 nothing? : % -> Boolean                                one? : % -> Boolean
 opposite? : (%, %) -> Boolean                          plenaryPower : (%, PositiveInteger) -> %
 pole? : % -> Boolean                                   recip : % -> Union(%,"failed")
 reductum : % -> % if Integer has COMPAR                rightPower : (%, PositiveInteger) -> %
 rightPower : (%, NonNegativeInteger) -> %              rightRecip : % -> Union(%,"failed")
 sample : () -> %                                       sqrt : % -> %
 string : % -> String                                   subtractIfCan : (%, %) -> Union(%,"failed")
 unit? : % -> Boolean                                   valuation : % -> NonNegativeInteger
 variable : % -> Symbol                                 zero? : % -> Boolean
 ?~=? : (%, %) -> Boolean
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
 constructOrdered : List(Record(k: Integer,c: R)) -> % if Integer has COMPAR
 differentiate : (%, List(Symbol), List(NonNegativeInteger)) -> % if R has PDRING(SYMBOL)
 differentiate : (%, Symbol, NonNegativeInteger) -> % if R has PDRING(SYMBOL)
 differentiate : (%, List(Symbol)) -> % if R has PDRING(SYMBOL)
 differentiate : (%, Symbol) -> % if R has PDRING(SYMBOL)
 ?exquo? : (%, %) -> Union(%,"failed") if % has ATVCWC and R has INTDOM
 leadingSupport : % -> Integer if Integer has COMPAR
 leadingTerm : % -> Record(k: Integer,c: R) if Integer has COMPAR
 unitCanonical : % -> % if % has ATVCWC and R has INTDOM
 unitNormal : % -> Record(unit: %,canonical: %,associate: %) if % has ATVCWC and R has INTDOM
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L263)\]

coerce(x) converts the variable x to a Nemo univariate power series.

- **Signature**: `(Variable(x))->%`

### `exp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L228)\]

exp(s) returns the exponential of the Laurent series s.

- **Signature**: `(%)->%`

### `integrate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L260)\]

integrate(f(x)) returns an anti-derivative of the power series f(x) with constant coefficient 0.

- **Signature**: `(%)->%`

### `inverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L250)\]

inverse(s) returns the inverse of s. Throws a Julia error if no such inverse exists.

- **Signature**: `(%)->%`

### `log` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L230)\]

log(s) returns the logarithm of the Laurent series s.

- **Signature**: `(%)->%`

### `monomial?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L256)\]

monomial?(f) tests if f is a single monomial.

- **Signature**: `(%)->Boolean`

### `sqrt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L253)\]

sqrt(f) returns the square root of f if it exists, a Julia error is thrown if f has no square root.

- **Signature**: `(%)->%`

### `valuation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L247)\]

valuation(s) returns the valuation of the given power series, the degree of the first nonzero term

- **Signature**: `(%)->NonNegativeInteger`

### `variable` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnseries.spad#L244)\]

variable(f) returns the (unique) Laurent series variable of the power series f.

- **Signature**: `(%)->Symbol`
---
[Back to Index](../index.md)
