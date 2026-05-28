# JLFloat64SpecialFunctions2

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Special functions computed using Julia's Ecosystem. They are here essentially for "completeness" purpose withJLFloat64. You should use the DoubleFloat's special functions if available, calling Julia functions is costly.

**JLFloat64SpecialFunctions2 is a package constructor**  
**Abbreviation for JLFloat64SpecialFunctions2 is JF64SF2**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 Beta : (JLFloat64, JLFloat64) -> JLFloat64             Ci : JLFloat64 -> JLFloat64
 Ei : JLFloat64 -> JLFloat64                            Gamma : JLFloat64 -> JLFloat64
 Gamma : (JLFloat64, JLFloat64) -> JLFloat64            Si : JLFloat64 -> JLFloat64
 airyAi : JLFloat64 -> JLFloat64                        airyAiPrime : JLFloat64 -> JLFloat64
 airyAiPrimex : JLFloat64 -> JLFloat64                  airyAix : JLFloat64 -> JLFloat64
 airyBi : JLFloat64 -> JLFloat64                        airyBiPrime : JLFloat64 -> JLFloat64
 airyBiPrimex : JLFloat64 -> JLFloat64                  airyBix : JLFloat64 -> JLFloat64
 besselI : (JLFloat64, JLFloat64) -> JLFloat64          besselIx : (JLFloat64, JLFloat64) -> JLFloat64
 besselJ : (JLFloat64, JLFloat64) -> JLFloat64          besselJ0 : JLFloat64 -> JLFloat64
 besselJ1 : JLFloat64 -> JLFloat64                      besselJx : (JLFloat64, JLFloat64) -> JLFloat64
 besselK : (JLFloat64, JLFloat64) -> JLFloat64          besselKx : (JLFloat64, JLFloat64) -> JLFloat64
 besselY : (JLFloat64, JLFloat64) -> JLFloat64          besselY0 : JLFloat64 -> JLFloat64
 besselY1 : JLFloat64 -> JLFloat64                      besselYx : (JLFloat64, JLFloat64) -> JLFloat64
 dawson : JLFloat64 -> JLFloat64                        digamma : JLFloat64 -> JLFloat64
 ellipticE : JLFloat64 -> JLFloat64                     ellipticK : JLFloat64 -> JLFloat64
 erf : JLFloat64 -> JLFloat64                           erf : (JLFloat64, JLFloat64) -> JLFloat64
 erfc : JLFloat64 -> JLFloat64                          erfcx : JLFloat64 -> JLFloat64
 erfi : JLFloat64 -> JLFloat64                          eta : JLFloat64 -> JLFloat64
 expint : (JLFloat64, JLFloat64) -> JLFloat64           expintx : JLFloat64 -> JLFloat64
 hankelH1 : (JLFloat64, JLFloat64) -> JLFloat64         hankelH1x : (JLFloat64, JLFloat64) -> JLFloat64
 hankelH2 : (JLFloat64, JLFloat64) -> JLFloat64         hankelH2x : (JLFloat64, JLFloat64) -> JLFloat64
 invdigamma : JLFloat64 -> JLFloat64                    inverseErf : JLFloat64 -> JLFloat64
 inverseErfc : JLFloat64 -> JLFloat64                   jinc : JLFloat64 -> JLFloat64
 logBeta : (JLFloat64, JLFloat64) -> JLFloat64          logGamma : JLFloat64 -> JLFloat64
 logGamma : (JLFloat64, JLFloat64) -> JLFloat64         logabsbeta : (JLFloat64, JLFloat64) -> JLFloat64
 logabsgamma : JLFloat64 -> JLFloat64                   logerfc : JLFloat64 -> JLFloat64
 logerfcx : JLFloat64 -> JLFloat64                      polygamma : (JLInt64, JLFloat64) -> JLFloat64
 riemannZeta : JLFloat64 -> JLFloat64                   trigamma : JLFloat64 -> JLFloat64
 gamma_inc_inv : (JLFloat64, JLFloat64, JLFloat64) -> JLFloat64
 sphericalBesselJ : (JLFloat64, JLFloat64) -> JLFloat64
 sphericalBesselY : (JLFloat64, JLFloat64) -> JLFloat64
```

## Operations added

### `Beta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L40)\]

Beta(x,y) computes beta function at x,y

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `Ci` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L59)\]

Ci(x) computes cosine integral Ci(x)

- **Signature**: `(JLFloat64)->JLFloat64`

### `Ei` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L53)\]

Ei(x) computes exponential integral Ei(x)

- **Signature**: `(JLFloat64)->JLFloat64`

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L13)\]

Gamma(z) computes Gamma function (z)

- **Signature**: `(JLFloat64)->JLFloat64`

Gamma(a,z) computes upper incomplete gamma function (a,z)

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `Si` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L57)\]

Si(x) computes sine integral Si(x)

- **Signature**: `(JLFloat64)->JLFloat64`

### `airyAi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L87)\]

airyAi(z) computes Airy Ai function at z

- **Signature**: `(JLFloat64)->JLFloat64`

### `airyAiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L89)\]

airyAiPrime(z) computes derivative of the Airy Ai function at z

- **Signature**: `(JLFloat64)->JLFloat64`

### `airyAiPrimex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L97)\]

airyAiPrimex(z) computes scaled derivative of the Airy Ai function at z

- **Signature**: `(JLFloat64)->JLFloat64`

### `airyAix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L95)\]

airyAix(z) computes scaled Airy Ai function and kth derivatives at z

- **Signature**: `(JLFloat64)->JLFloat64`

### `airyBi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L91)\]

airyBi(z) computes Airy Bi function at z

- **Signature**: `(JLFloat64)->JLFloat64`

### `airyBiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L93)\]

airyBiPrime(z) computes derivative of the Airy Bi function at z

- **Signature**: `(JLFloat64)->JLFloat64`

### `airyBiPrimex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L101)\]

airyBiPrimex(z) computes scaled derivative of the Airy Bi function at z

- **Signature**: `(JLFloat64)->JLFloat64`

### `airyBix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L99)\]

airyBix(z) computes scaled Airy Bi function at z

- **Signature**: `(JLFloat64)->JLFloat64`

### `besselI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L136)\]

besselI(nu,z) computes modified Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `besselIx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L138)\]

besselIx(nu,z) computes scaled modified Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `besselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L106)\]

besselJ(nu,z) computes Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `besselJ0` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L108)\]

besselJ0(z) computes besselj(0,z)

- **Signature**: `(JLFloat64)->JLFloat64`

### `besselJ1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L110)\]

besselJ1(z) computes besselj(1,z)

- **Signature**: `(JLFloat64)->JLFloat64`

### `besselJx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L112)\]

besselJx(nu,z) computes scaled Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `besselK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L140)\]

besselK(nu,z) computes modified Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `besselKx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L142)\]

besselKx(nu,z) computes scaled modified Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `besselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L116)\]

besselY(nu,z) computes Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `besselY0` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L118)\]

besselY0(z) computes bessely(0,z)

- **Signature**: `(JLFloat64)->JLFloat64`

### `besselY1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L120)\]

besselY1(z) computes bessely(1,z)

- **Signature**: `(JLFloat64)->JLFloat64`

### `besselYx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L122)\]

besselYx(nu,z) computes scaled Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `dawson` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L82)\]

dawson(x) computes scaled imaginary error function, a.k.a. Dawson function.

- **Signature**: `(JLFloat64)->JLFloat64`

### `digamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L21)\]

digamma(x) computes digamma function (i.e. the derivative of loggamma at x)

- **Signature**: `(JLFloat64)->JLFloat64`

### `ellipticE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L151)\]

ellipticE(m) computes complete elliptic integral of 2nd kind E(m)

- **Signature**: `(JLFloat64)->JLFloat64`

### `ellipticK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L149)\]

ellipticK(m) computes complete elliptic integral of 1st kind K(m)

- **Signature**: `(JLFloat64)->JLFloat64`

### `erf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L64)\]

erf(x) computes error function at x

- **Signature**: `(JLFloat64)->JLFloat64`

erf(x,y) computes accurate version of erf(y) - erf(x)

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `erfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L68)\]

erfc(x) computes complementary error function, i.e. the accurate version of 1-erf(x) for large x

- **Signature**: `(JLFloat64)->JLFloat64`

### `erfcx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L72)\]

erfcx(x) computes scaled complementary error function, i.e. accurate e^(x^2) erfc(x) for large x

- **Signature**: `(JLFloat64)->JLFloat64`

### `erfi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L78)\]

erfi(x) computes imaginary error function defined as -i erf(ix)

- **Signature**: `(JLFloat64)->JLFloat64`

### `eta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L156)\]

eta(x) computes Dirichlet eta function at x

- **Signature**: `(JLFloat64)->JLFloat64`

### `expint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L51)\]

expint(nu, z) computes exponential integral function

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `expintx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L55)\]

expintx(x) computes scaled exponential integral function

- **Signature**: `(JLFloat64)->JLFloat64`

### `hankelH1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L128)\]

hankelH1(nu,z) computes besselh(nu, 1, z)

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `hankelH1x` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L130)\]

hankelH1x(nu,z) computes scaled besselh(nu, 1, z)

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `hankelH2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L132)\]

hankelH2(nu,z) computes besselh(nu, 2, z)

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `hankelH2x` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L134)\]

hankelH2x(nu,z) computes scaled besselh(nu, 2, z)

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `invdigamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L23)\]

invdigamma(x) computes invdigamma function (i.e. inverse of digamma function at x using fixed-point iterationalgorithm)

- **Signature**: `(JLFloat64)->JLFloat64`

### `inverseErf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L80)\]

inverseErf(x) computes inverse function of erf()

- **Signature**: `(JLFloat64)->JLFloat64`

### `inverseErfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L70)\]

inverseErfc(x) computes inverse function of erfc.

- **Signature**: `(JLFloat64)->JLFloat64`

### `jinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L144)\]

jinc(x) computes scaled Bessel function of the first kind divided by x. A.k.a. sombrero or besinc

- **Signature**: `(JLFloat64)->JLFloat64`

### `logBeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L42)\]

logBeta(x,y) computes accurate log(beta(x,y)) for large x or y

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `logGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L15)\]

logGamma(x) computes accurate log(gamma(x)) for large x

- **Signature**: `(JLFloat64)->JLFloat64`

logGamma(a,z) computes accurate log(gamma(a,x)) for large arguments

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `logabsbeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L44)\]

logabsbeta(x,y) computes accurate log(abs(beta(x,y))) for large x or y

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `logabsgamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L17)\]

logabsgamma(x) computes accurate log(abs(gamma(x))) for large x

- **Signature**: `(JLFloat64)->JLFloat64`

### `logerfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L74)\]

logerfc(x) computes log of the complementary error function, i.e. accurate ln(erfc(x)) for large x

- **Signature**: `(JLFloat64)->JLFloat64`

### `logerfcx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L76)\]

logerfcx(x) computes log of the scaled complementary error function, i.e. accurate ln(erfcx(x)) for large negative x

- **Signature**: `(JLFloat64)->JLFloat64`

### `polygamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L27)\]

polygamma(m,x) computes polygamma function (i.e the (m+1)-th derivative of the loggamma function at x)

- **Signature**: `(JLInt64,JLFloat64)->JLFloat64`

### `riemannZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L158)\]

riemannZeta(x) computes Riemann zeta function at x

- **Signature**: `(JLFloat64)->JLFloat64`

### `sphericalBesselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L114)\]

sphericalBesselJ(nu,z) computes Spherical Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `sphericalBesselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L124)\]

sphericalBesselY(nu,z) computes Spherical Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `trigamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf2.spad#L25)\]

trigamma(x) computes trigamma function (i.e the logarithmic second derivative of gamma at x)

- **Signature**: `(JLFloat64)->JLFloat64`
---
[Back to Index](../index.md)
