# JLFloat32SpecialFunctions2

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Special functions computed using JL's Ecosystem. They are here essentially for "completeness" purpose with JLFloat32. You should use the DoubleFloat's special functions if available, calling JL functions is costly.

**JLFloat32SpecialFunctions2 is a package constructor**  
**Abbreviation for JLFloat32SpecialFunctions2 is JF32SF2**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 Beta : (JLFloat32, JLFloat32) -> JLFloat32             Ci : JLFloat32 -> JLFloat32
 Ei : JLFloat32 -> JLFloat32                            Gamma : JLFloat32 -> JLFloat32
 Gamma : (JLFloat32, JLFloat32) -> JLFloat32            Si : JLFloat32 -> JLFloat32
 airyAi : JLFloat32 -> JLFloat32                        airyAiPrime : JLFloat32 -> JLFloat32
 airyAiPrimex : JLFloat32 -> JLFloat32                  airyAix : JLFloat32 -> JLFloat32
 airyBi : JLFloat32 -> JLFloat32                        airyBiPrime : JLFloat32 -> JLFloat32
 airyBiPrimex : JLFloat32 -> JLFloat32                  airyBix : JLFloat32 -> JLFloat32
 besselI : (JLFloat32, JLFloat32) -> JLFloat32          besselIx : (JLFloat32, JLFloat32) -> JLFloat32
 besselJ : (JLFloat32, JLFloat32) -> JLFloat32          besselJ0 : JLFloat32 -> JLFloat32
 besselJ1 : JLFloat32 -> JLFloat32                      besselJx : (JLFloat32, JLFloat32) -> JLFloat32
 besselK : (JLFloat32, JLFloat32) -> JLFloat32          besselKx : (JLFloat32, JLFloat32) -> JLFloat32
 besselY : (JLFloat32, JLFloat32) -> JLFloat32          besselY0 : JLFloat32 -> JLFloat32
 besselY1 : JLFloat32 -> JLFloat32                      besselYx : (JLFloat32, JLFloat32) -> JLFloat32
 dawson : JLFloat32 -> JLFloat32                        digamma : JLFloat32 -> JLFloat32
 ellipticE : JLFloat32 -> JLFloat32                     ellipticK : JLFloat32 -> JLFloat32
 erf : JLFloat32 -> JLFloat32                           erf : (JLFloat32, JLFloat32) -> JLFloat32
 erfc : JLFloat32 -> JLFloat32                          erfcx : JLFloat32 -> JLFloat32
 erfi : JLFloat32 -> JLFloat32                          eta : JLFloat32 -> JLFloat32
 expint : (JLFloat32, JLFloat32) -> JLFloat32           expintx : JLFloat32 -> JLFloat32
 hankelH1 : (JLFloat32, JLFloat32) -> JLFloat32         hankelH1x : (JLFloat32, JLFloat32) -> JLFloat32
 hankelH2 : (JLFloat32, JLFloat32) -> JLFloat32         hankelH2x : (JLFloat32, JLFloat32) -> JLFloat32
 invdigamma : JLFloat32 -> JLFloat32                    inverseErf : JLFloat32 -> JLFloat32
 inverseErfc : JLFloat32 -> JLFloat32                   jinc : JLFloat32 -> JLFloat32
 logBeta : (JLFloat32, JLFloat32) -> JLFloat32          logGamma : JLFloat32 -> JLFloat32
 logGamma : (JLFloat32, JLFloat32) -> JLFloat32         logabsbeta : (JLFloat32, JLFloat32) -> JLFloat32
 logabsgamma : JLFloat32 -> JLFloat32                   logerfc : JLFloat32 -> JLFloat32
 logerfcx : JLFloat32 -> JLFloat32                      polygamma : (JLInt64, JLFloat32) -> JLFloat32
 riemannZeta : JLFloat32 -> JLFloat32                   trigamma : JLFloat32 -> JLFloat32
 gamma_inc_inv : (JLFloat32, JLFloat32, JLFloat32) -> JLFloat32
 sphericalBesselJ : (JLFloat32, JLFloat32) -> JLFloat32
 sphericalBesselY : (JLFloat32, JLFloat32) -> JLFloat32
```

## Operations added

### `Beta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L40)\]

Beta(x,y) computes beta function at x,y

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `Ci` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L59)\]

Ci(x) computes cosine integral Ci(x)

- **Signature**: `(JLFloat32)->JLFloat32`

### `Ei` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L53)\]

Ei(x) computes exponential integral Ei(x)

- **Signature**: `(JLFloat32)->JLFloat32`

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L13)\]

Gamma(z) computes Gamma function (z)

- **Signature**: `(JLFloat32)->JLFloat32`

Gamma(a,z) computes upper incomplete gamma function (a,z)

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `Si` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L57)\]

Si(x) computes sine integral Si(x)

- **Signature**: `(JLFloat32)->JLFloat32`

### `airyAi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L87)\]

airyAi(z) computes Airy Ai function at z

- **Signature**: `(JLFloat32)->JLFloat32`

### `airyAiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L89)\]

airyAiPrime(z) computes derivative of the Airy Ai function at z

- **Signature**: `(JLFloat32)->JLFloat32`

### `airyAiPrimex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L97)\]

airyAiPrimex(z) computes scaled derivative of the Airy Ai function at z

- **Signature**: `(JLFloat32)->JLFloat32`

### `airyAix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L95)\]

airyAix(z) computes scaled Airy Ai function and kth derivatives at z

- **Signature**: `(JLFloat32)->JLFloat32`

### `airyBi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L91)\]

airyBi(z) computes Airy Bi function at z

- **Signature**: `(JLFloat32)->JLFloat32`

### `airyBiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L93)\]

airyBiPrime(z) computes derivative of the Airy Bi function at z

- **Signature**: `(JLFloat32)->JLFloat32`

### `airyBiPrimex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L101)\]

airyBiPrimex(z) computes scaled derivative of the Airy Bi function at z

- **Signature**: `(JLFloat32)->JLFloat32`

### `airyBix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L99)\]

airyBix(z) computes scaled Airy Bi function at z

- **Signature**: `(JLFloat32)->JLFloat32`

### `besselI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L136)\]

besselI(nu,z) computes modified Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `besselIx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L138)\]

besselIx(nu,z) computes scaled modified Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `besselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L106)\]

besselJ(nu,z) computes Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `besselJ0` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L108)\]

besselJ0(z) computes besselj(0,z)

- **Signature**: `(JLFloat32)->JLFloat32`

### `besselJ1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L110)\]

besselJ1(z) computes besselj(1,z)

- **Signature**: `(JLFloat32)->JLFloat32`

### `besselJx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L112)\]

besselJx(nu,z) computes scaled Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `besselK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L140)\]

besselK(nu,z) computes modified Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `besselKx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L142)\]

besselKx(nu,z) computes scaled modified Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `besselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L116)\]

besselY(nu,z) computes Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `besselY0` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L118)\]

besselY0(z) computes bessely(0,z)

- **Signature**: `(JLFloat32)->JLFloat32`

### `besselY1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L120)\]

besselY1(z) computes bessely(1,z)

- **Signature**: `(JLFloat32)->JLFloat32`

### `besselYx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L122)\]

besselYx(nu,z) computes scaled Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `dawson` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L82)\]

dawson(x) computes scaled imaginary error function, a.k.a. Dawson function.

- **Signature**: `(JLFloat32)->JLFloat32`

### `digamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L21)\]

digamma(x) computes digamma function (i.e. the derivative of loggamma at x)

- **Signature**: `(JLFloat32)->JLFloat32`

### `ellipticE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L151)\]

ellipticE(m) computes complete elliptic integral of 2nd kind E(m)

- **Signature**: `(JLFloat32)->JLFloat32`

### `ellipticK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L149)\]

ellipticK(m) computes complete elliptic integral of 1st kind K(m)

- **Signature**: `(JLFloat32)->JLFloat32`

### `erf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L64)\]

erf(x) computes error function at x

- **Signature**: `(JLFloat32)->JLFloat32`

erf(x,y) computes accurate version of erf(y) - erf(x)

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `erfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L68)\]

erfc(x) computes complementary error function, i.e. the accurate version of 1-erf(x) for large x

- **Signature**: `(JLFloat32)->JLFloat32`

### `erfcx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L72)\]

erfcx(x) computes scaled complementary error function, i.e. accurate e^(x^2) erfc(x) for large x

- **Signature**: `(JLFloat32)->JLFloat32`

### `erfi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L78)\]

erfi(x) computes imaginary error function defined as -i erf(ix)

- **Signature**: `(JLFloat32)->JLFloat32`

### `eta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L156)\]

eta(x) computes Dirichlet eta function at x

- **Signature**: `(JLFloat32)->JLFloat32`

### `expint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L51)\]

expint(nu, z) computes exponential integral function

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `expintx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L55)\]

expintx(x) computes scaled exponential integral function

- **Signature**: `(JLFloat32)->JLFloat32`

### `gamma_inc_inv` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L38)\]

gamma_inc_inv(a,p,q) computes inverse of incomplete gamma function ratio P(a,x) and Q(a,x) (i.e evaluates x given P(a,x)=p and Q(a,x)=q)

- **Signature**: `(JLFloat32,JLFloat32,JLFloat32)->JLFloat32`

### `hankelH1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L128)\]

hankelH1(nu,z) computes besselh(nu, 1, z)

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `hankelH1x` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L130)\]

hankelH1x(nu,z) computes scaled besselh(nu, 1, z)

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `hankelH2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L132)\]

hankelH2(nu,z) computes besselh(nu, 2, z)

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `hankelH2x` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L134)\]

hankelH2x(nu,z) computes scaled besselh(nu, 2, z)

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `invdigamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L23)\]

invdigamma(x) computes invdigamma function (i.e. inverse of digamma function at x using fixed-point iterationalgorithm)

- **Signature**: `(JLFloat32)->JLFloat32`

### `inverseErf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L80)\]

inverseErf(x) computes inverse function of erf()

- **Signature**: `(JLFloat32)->JLFloat32`

### `inverseErfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L70)\]

inverseErfc(x) computes inverse function of erfc.

- **Signature**: `(JLFloat32)->JLFloat32`

### `jinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L144)\]

jinc(x) computes scaled Bessel function of the first kind divided by x. A.k.a. sombrero or besinc

- **Signature**: `(JLFloat32)->JLFloat32`

### `logBeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L42)\]

logBeta(x,y) computes accurate log(beta(x,y)) for large x or y

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `logGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L15)\]

logGamma(x) computes accurate log(gamma(x)) for large x

- **Signature**: `(JLFloat32)->JLFloat32`

logGamma(a,z) computes accurate log(gamma(a,x)) for large arguments

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `logabsbeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L44)\]

logabsbeta(x,y) computes accurate log(abs(beta(x,y))) for large x or y

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `logabsgamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L17)\]

logabsgamma(x) computes accurate log(abs(gamma(x))) for large x

- **Signature**: `(JLFloat32)->JLFloat32`

### `logerfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L74)\]

logerfc(x) computes log of the complementary error function, i.e. accurate ln(erfc(x)) for large x

- **Signature**: `(JLFloat32)->JLFloat32`

### `logerfcx` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L76)\]

logerfcx(x) computes log of the scaled complementary error function, i.e. accurate ln(erfcx(x)) for large negative x

- **Signature**: `(JLFloat32)->JLFloat32`

### `polygamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L27)\]

polygamma(m,x) computes polygamma function (i.e the (m+1)-th derivative of the loggamma function at x)

- **Signature**: `(JLInt64,JLFloat32)->JLFloat32`

### `riemannZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L158)\]

riemannZeta(x) computes Riemann zeta function at x

- **Signature**: `(JLFloat32)->JLFloat32`

### `sphericalBesselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L114)\]

sphericalBesselJ(nu,z) computes Spherical Bessel function of the first kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `sphericalBesselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L124)\]

sphericalBesselY(nu,z) computes Spherical Bessel function of the second kind of order nu at z

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `trigamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf2.spad#L25)\]

trigamma(x) computes trigamma function (i.e the logarithmic second derivative of gamma at x)

- **Signature**: `(JLFloat32)->JLFloat32`
---
[Back to Index](../index.md)
