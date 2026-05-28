# JLFloatSpecialFunctions2

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Special functions computed using Julia's Ecosystem. They are here essentially for "completeness" purpose withJLFloat.

**JLFloatSpecialFunctions2 is a package constructor**  
**Abbreviation for JLFloatSpecialFunctions2 is JFSF2**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 Beta : (JLFloat, JLFloat) -> JLFloat                   Ei : JLFloat -> JLFloat
 Gamma : JLFloat -> JLFloat                             Gamma : (JLFloat, JLFloat) -> JLFloat
 airyAi : JLFloat -> JLFloat                            besselJ0 : JLFloat -> JLFloat
 besselJ1 : JLFloat -> JLFloat                          besselY0 : JLFloat -> JLFloat
 besselY1 : JLFloat -> JLFloat                          digamma : JLFloat -> JLFloat
 erf : JLFloat -> JLFloat                               erf : (JLFloat, JLFloat) -> JLFloat
 erfc : JLFloat -> JLFloat                              erfcx : JLFloat -> JLFloat
 eta : JLFloat -> JLFloat                               expint : (JLFloat, JLFloat) -> JLFloat
 inverseErf : JLFloat -> JLFloat                        inverseErfc : JLFloat -> JLFloat
 jinc : JLFloat -> JLFloat                              logBeta : (JLFloat, JLFloat) -> JLFloat
 logGamma : JLFloat -> JLFloat                          logGamma : (JLFloat, JLFloat) -> JLFloat
 logerfc : JLFloat -> JLFloat                           logerfcx : JLFloat -> JLFloat
 riemannZeta : JLFloat -> JLFloat
```

## Operations added

### `Beta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L21)\]

Beta(x,y) computes beta function at x,y.

- **Signature**: `(JLFloat,JLFloat)->JLFloat`

### `Ei` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L30)\]

Ei(x) computes exponential integral Ei(x).

- **Signature**: `(JLFloat)->JLFloat`

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L11)\]

Gamma(z) computes gamma function (z).

- **Signature**: `(JLFloat)->JLFloat`

Gamma(a,z) computes upper incomplete gamma function (a,z).

- **Signature**: `(JLFloat,JLFloat)->JLFloat`

### `airyAi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L58)\]

airyAi(z) computes Airy Ai function at z

- **Signature**: `(JLFloat)->JLFloat`

### `besselJ0` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L63)\]

besselJ0(z) computes besselj(0,z).

- **Signature**: `(JLFloat)->JLFloat`

### `besselJ1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L65)\]

besselJ1(z) computes besselj(1,z).

- **Signature**: `(JLFloat)->JLFloat`

### `besselY0` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L67)\]

besselY0(z) computes bessely(0,z).

- **Signature**: `(JLFloat)->JLFloat`

### `besselY1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L69)\]

besselY1(z) computes bessely(1,z).

- **Signature**: `(JLFloat)->JLFloat`

### `digamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L15)\]

digamma(x) computes digamma function (i.e. the derivative of logGamma at x).

- **Signature**: `(JLFloat)->JLFloat`

### `erf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L35)\]

erf(x) computes error function at x.

- **Signature**: `(JLFloat)->JLFloat`

erf(x,y) computes accurate version of erf(y) - erf(x).

- **Signature**: `(JLFloat,JLFloat)->JLFloat`

### `erfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L39)\]

erfc(x) computes complementary error function, i.e. the accurate version of 1-erf(x) for large x.

- **Signature**: `(JLFloat)->JLFloat`

### `erfcx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L44)\]

erfcx(x) computes scaled complementary error function, i.e. accurate e^x^2 erfc(x) for large x.

- **Signature**: `(JLFloat)->JLFloat`

### `eta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L77)\]

eta(x) computes Dirichlet eta function at x.

- **Signature**: `(JLFloat)->JLFloat`

### `expint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L28)\]

expint(nu, z) computes exponential integral E(z).

- **Signature**: `(JLFloat,JLFloat)->JLFloat`

### `inverseErf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L53)\]

inverseErf(x) computes inverse function of erf.

- **Signature**: `(JLFloat)->JLFloat`

### `inverseErfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L42)\]

inverseErfc(x) computes the inverse function of erfc.

- **Signature**: `(JLFloat)->JLFloat`

### `jinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L71)\]

jinc(x) computes scaled Bessel function of the first kind divided by x. A.k.a. sombrero or besinc.

- **Signature**: `(JLFloat)->JLFloat`

### `logBeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L23)\]

logBeta(x,y) computes accurate log(beta(x,y)) for large x or y.

- **Signature**: `(JLFloat,JLFloat)->JLFloat`

### `logGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L13)\]

logGamma(x) computes accurate log(gamma(x)) for large x.

- **Signature**: `(JLFloat)->JLFloat`

logGamma(a,z) computes accurate log(gamma(a,x)) for large arguments.

- **Signature**: `(JLFloat,JLFloat)->JLFloat`

### `logerfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L47)\]

logerfc(x) computes log of the complementary error function, i.e. accurate ln(erfc(x)) for large x.

- **Signature**: `(JLFloat)->JLFloat`

### `logerfcx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L50)\]

logerfcx(x) computes log of the scaled complementary error function, i.e. accurate ln(erfcx(x)) for large negative x.

- **Signature**: `(JLFloat)->JLFloat`

### `riemannZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf2.spad#L79)\]

riemannZeta(x) computes Riemann zeta function at x.

- **Signature**: `(JLFloat)->JLFloat`
---
[Back to Index](../index.md)
