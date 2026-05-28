# WSNumericalSpecialFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L1)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic numerical special functions using the MathLink Julia package.

**WSNumericalSpecialFunctions(R: WSNumber) is a package constructor**  
**Abbreviation for WSNumericalSpecialFunctions is WSNSF**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 Beta : (R, R) -> R                                     Beta : (R, R, R) -> R
 EiEn : (WSInteger, R) -> R                             Gamma : R -> R
 Gamma : (R, R) -> R                                    Gamma : (R, R, R) -> R
 airyAi : R -> R                                        airyAiPrime : R -> R
 airyAiZero : R -> R                                    airyAiZero : (R, R) -> R
 airyBi : R -> R                                        airyBiPrime : R -> R
 airyBiZero : R -> R                                    airyBiZero : (R, R) -> R
 angerJ : (R, R) -> R                                   angerJ : (R, R, R) -> R
 barnesG : R -> R                                       besselI : (R, R) -> R
 besselJ : (R, R) -> R                                  besselJZero : (R, R) -> R
 besselK : (R, R) -> R                                  besselY : (R, R) -> R
 besselYZero : (R, R) -> R                              betaRegularized : (R, R, R) -> R
 charlierC : (R, R, R) -> R                             chebyshevT : (R, R) -> R
 chebyshevU : (R, R) -> R                               conjugate : R -> R
 coulombF : (R, R, R) -> R                              coulombG : (R, R, R) -> R
 coulombH1 : (R, R, R) -> R                             coulombH2 : (R, R, R) -> R
 dawson : R -> R                                        dedekindEta : R -> R
 digamma : R -> R                                       digamma : (R, R) -> R
 diracDelta : R -> WSExpression                         dirichletEta : R -> R
 dirichletL : (R, R, R) -> R                            ellipticE : R -> R
 ellipticE : (R, R) -> R                                ellipticF : (R, R) -> R
 ellipticK : R -> R                                     ellipticPi : (R, R) -> R
 ellipticPi : (R, R, R) -> R                            ellipticTheta : (R, R, R) -> R
 ellipticThetaPrime : (R, R, R) -> R                    fibonacci : (WSInteger, R) -> R
 gammaRegularized : (R, R) -> R                         gegenbauerC : (WSInteger, R) -> R
 gegenbauerC : (WSInteger, R, R) -> R                   gudermannian : R -> R
 hankelH1 : (R, R) -> R                                 hankelH2 : (R, R) -> R
 haversine : R -> R                                     hermiteH : (R, R) -> R
 hurwitzLerchPhi : (R, R, R) -> R                       hurwitzZeta : (R, R) -> R
 hyperFactorial : R -> R                                hypergeometric0F1 : (R, R) -> R
 hypergeometric0F1Regularized : (R, R) -> R             hypergeometric1F1 : (R, R, R) -> R
 hypergeometric1F1Regularized : (R, R, R) -> R          hypergeometricU : (R, R, R) -> R
 inverseBetaRegularized : (R, R, R) -> R                inverseErf : R -> R
 inverseErfc : R -> R                                   inverseGammaRegularized : (R, R) -> R
 inverseGudermannian : R -> R                           inverseHaversine : R -> R
 inverseJacobiCn : (R, R) -> R                          inverseJacobiSn : (R, R) -> R
 jacobiAmplitude : (R, R) -> R                          jacobiCn : (R, R) -> R
 jacobiDn : (R, R) -> R                                 jacobiP : (R, R, R, R) -> R
 jacobiSn : (R, R) -> R                                 jacobiTheta : (WSInteger, R, R) -> R
 jacobiZeta : (R, R) -> R                               kelvinBei : (R, R) -> R
 kelvinBer : (R, R) -> R                                kelvinKei : (R, R) -> R
 kelvinKer : (R, R) -> R                                kleinInvariantJ : R -> R
 laguerreL : (R, R) -> R                                laguerreL : (R, R, R) -> R
 lambertW : R -> R                                      lambertW : (WSInteger, R) -> R
 legendreP : (R, R, R) -> R                             legendreP : (R, R) -> R
 legendreQ : (R, R, R) -> R                             legendreQ : (R, R) -> R
 lerchPhi : (R, R, R) -> R                              logBarnesG : R -> R
 logGamma : R -> R                                      lommelS1 : (R, R, R) -> R
 lommelS2 : (R, R, R) -> R                              mathieuC : (R, R, R) -> R
 mathieuCPrime : (R, R, R) -> R                         mathieuCharacteristicA : (R, R) -> R
 mathieuCharacteristicB : (R, R) -> R                   mathieuCharacteristicExponent : (R, R) -> R
 mathieuS : (R, R, R) -> R                              mathieuSPrime : (R, R, R) -> R
 modularLambda : R -> R                                 parabolicCylinderD : (R, R) -> R
 pochhammer : (R, R) -> R                               polygamma : (R, R) -> R
 polylog : (R, R) -> R                                  polylog : (R, R, R) -> R
 qBinomial : (R, R, R) -> R                             qFactorial : (R, R) -> R
 qGamma : (R, R) -> R                                   qPochhammer : (R, R) -> R
 qPochhammer : (R, R, R) -> R                           qPolyGamma : (R, R) -> R
 qPolyGamma : (R, R, R) -> R                            ramanujanTau : R -> R
 ramanujanTauL : R -> R                                 ramanujanTauTheta : R -> R
 ramanujanTauZ : R -> R                                 riemannSiegelTheta : R -> R
 riemannSiegelZ : R -> R                                riemannZeta : R -> R
 riemannZeta : (R, R) -> R                              sign : R -> R
 sphericalBesselJ : (R, R) -> R                         sphericalBesselY : (R, R) -> R
 sphericalHankelH1 : (R, R) -> R                        sphericalHankelH2 : (R, R) -> R
 sphericalHarmonicY : (R, R, R, R) -> R                 stieltjesGamma : WSInteger -> R
 stieltjesGamma : (WSInteger, R) -> R                   struveH : (R, R) -> R
 struveL : (R, R) -> R                                  unitStep : R -> WSExpression
 weberE : (R, R) -> R                                   weberE : (R, R, R) -> R
 weierstrassP : (R, R, R) -> R                          weierstrassPInverse : (R, R, R) -> R
 weierstrassPPrime : (R, R, R) -> R                     weierstrassSigma : (R, R, R) -> R
 weierstrassZeta : (R, R, R) -> R                       whittakerM : (R, R, R) -> R
 whittakerW : (R, R, R) -> R                            zernikeR : (R, R, R) -> R
```

## Operations added

### `Beta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L18)\]

Beta(x, y) is Gamma(x) * Gamma(y)/Gamma(x+y).

- **Signature**: `(R,R)->R`

Beta(z, a, b) is the incomplete Beta function.

- **Signature**: `(R,R,R)->R`

### `EiEn` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L168)\]

EiEn(n,z) returns the exponential integral En of z.

- **Signature**: `(WSInteger,R)->R`

### `Gamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L16)\]

Gamma(z) is the Euler Gamma function.

- **Signature**: `(R)->R`

Gamma(a, z) is the incomplete Gamma function.

- **Signature**: `(R,R)->R`

Gamma(a,z1,z2) computes the generalized incomplete Gamma function.

- **Signature**: `(R,R,R)->R`

### `airyAi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L44)\]

airyAi(z) is the Airy function Ai(z).

- **Signature**: `(R)->R`

### `airyAiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L46)\]

airyAiPrime(z) is the derivative of the Airy function Ai(z).

- **Signature**: `(R)->R`

### `airyAiZero` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L48)\]

airyAiZero(n) is the n-th zero function of the Airy function Ai(z).

- **Signature**: `(R)->R`

airyAiZero(n,x) is the n-th zero function of the Airy function Ai(z) smaller than x.

- **Signature**: `(R,R)->R`

### `airyBi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L53)\]

airyBi(z) is the Airy function Bi(z).

- **Signature**: `(R)->R`

### `airyBiPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L60)\]

airyBiPrime(z) is the derivative of the Airy function Bi(z).

- **Signature**: `(R)->R`

### `airyBiZero` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L55)\]

airyBiZero(n) is the n-th zero function of the Airy function Bi(z).

- **Signature**: `(R)->R`

airyBiZero(n,x) is the n-th zero function of the Airy function Bi(z) smaller than x.

- **Signature**: `(R,R)->R`

### `angerJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L62)\]

angerJ(v, z) is the Anger J function.

- **Signature**: `(R,R)->R`
- **Signature**: `(R,R)->R`

angerJ(v, n, z) is the associated Anger J function.

- **Signature**: `(R,R,R)->R`

### `barnesG` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L246)\]

barnesG(z) computes the Barnes G-function of z.

- **Signature**: `(R)->R`

### `besselI` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L40)\]

besselI(v, z) is the modified Bessel function of the first kind.

- **Signature**: `(R,R)->R`

### `besselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L30)\]

besselJ(v, z) is the Bessel function of the first kind.

- **Signature**: `(R,R)->R`

### `besselJZero` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L32)\]

besselJZero(n,x) returns the n-th zero of the Bessel J n-th function.

- **Signature**: `(R,R)->R`

### `besselK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L42)\]

besselK(v, z) is the modified Bessel function of the second kind.

- **Signature**: `(R,R)->R`

### `besselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L35)\]

besselY(v, z) is the Bessel function of the second kind.

- **Signature**: `(R,R)->R`

### `besselYZero` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L37)\]

besselYZero(n,x) returns the n-th zero of the Bessel Y n-th function.

- **Signature**: `(R,R)->R`

### `betaRegularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L230)\]

betaRegularized(z,a,b) computes the regularized incomplete Beta function.

- **Signature**: `(R,R,R)->R`

### `charlierC` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L157)\]

charlierC(n, a, z) is the Charlier polynomial

- **Signature**: `(R,R,R)->R`

### `chebyshevT` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L271)\]

chebyshevT(n, z) evaluates the Chebyshev polynomial of the first kind at z.

- **Signature**: `(R,R)->R`

### `chebyshevU` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L274)\]

chebyshevU(n, expr) evaluates the Chebyshev polynomial of the second kind at z.

- **Signature**: `(R,R)->R`

### `conjugate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L66)\]

conjugate(z) is the conjugate of the complex number z.

- **Signature**: `(R)->R`

### `coulombF` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L305)\]

coulombF(l,eta,ro) is the regular Coulomb wave function.

- **Signature**: `(R,R,R)->R`

### `coulombG` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L307)\]

coulombG(l,eta,ro) is the irregular Coulomb wave function.

- **Signature**: `(R,R,R)->R`

### `coulombH1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L309)\]

coulombH1(l,eta,ro) is the incoming irregular Coulomb wave function H^(+).

- **Signature**: `(R,R,R)->R`

### `coulombH2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L311)\]

coulombH2(l,eta,ro) is the incoming irregular Coulomb wave function H^(-).

- **Signature**: `(R,R,R)->R`

### `dawson` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L68)\]

dawson(x) computes the Dawson integral of x.

- **Signature**: `(R)->R`

### `dedekindEta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L381)\]

dedekindEta(tau) computes the Dedekind eta modular function.

- **Signature**: `(R)->R`

### `digamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L22)\]

digamma(z) is the logarithmic derivative of Gamma(z) (often written psi(z) in the literature).

- **Signature**: `(R)->R`

digamma(n,z) is the n-th derivative of the digamma function.

- **Signature**: `(R,R)->R`

### `diracDelta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L196)\]

diracDelta(x) returns the Dirac delta function of x.

- **Signature**: `(R)->WSExpression`

### `dirichletEta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L383)\]

dirichletEta(z) computes the Dirichlet eta function of z.

- **Signature**: `(R)->R`

### `dirichletL` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L194)\]

dirichletL(k,j,s) returns Dirichlet L-function of s, modulus k, index j.

- **Signature**: `(R,R,R)->R`

### `ellipticE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L369)\]

ellipticE(z) computes the complete elliptic integral of the second kind.

- **Signature**: `(R)->R`

ellipticE(phi,m) computes the elliptic integral of the second kind.

- **Signature**: `(R,R)->R`

### `ellipticF` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L367)\]

ellipticF(phi,m) computes the elliptic integral of the first kind.

- **Signature**: `(R,R)->R`

### `ellipticK` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L364)\]

ellipticK(m) computes the complete elliptic integral of the first kind.

- **Signature**: `(R)->R`

### `ellipticPi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L373)\]

ellipticPi(n,m) computes the complete elliptic integral of the third kind.

- **Signature**: `(R,R)->R`

ellipticPi(n,phi,m) computes the elliptic integral of the third kind.

- **Signature**: `(R,R,R)->R`

### `ellipticTheta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L355)\]

ellipticTheta(a, u, q) computes the theta function, a ranges from 1 to 4.

- **Signature**: `(R,R,R)->R`

### `ellipticThetaPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L357)\]

ellipticThetaPrime(a, u, q) computes the derivative of the theta function, a ranges from 1 to 4.

- **Signature**: `(R,R,R)->R`

### `fibonacci` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L250)\]

fibonacci(n, z) evaluates the Fibonacci polynomial at z.

- **Signature**: `(WSInteger,R)->R`

### `gammaRegularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L232)\]

gammaRegularized(a,z) computes the regularized incomplete Gamma function.

- **Signature**: `(R,R)->R`

### `gegenbauerC` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L266)\]

gegenbauerC(n,z) evaluates the renormalized form of the Gegenbauer polynomial at z.

- **Signature**: `(WSInteger,R)->R`

gegenbauerC(n,lambda,z) evaluates the Gegenbauer polynomial at z.

- **Signature**: `(WSInteger,R,R)->R`

### `gudermannian` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L176)\]

gudermannian(z) computes the Gudermannian of z.

- **Signature**: `(R)->R`

### `hankelH1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L103)\]

hankelH1(v, z) is the first Hankel function (Bessel function of the third kind).

- **Signature**: `(R,R)->R`

### `hankelH2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L106)\]

hankelH2(v, z) is the second Hankel function (Bessel function of the third kind).

- **Signature**: `(R,R)->R`

### `haversine` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L172)\]

haversine(z) computes the Haversine of z.

- **Signature**: `(R)->R`

### `hermiteH` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L277)\]

hermiteH(n, z) evaluates the Hermite polynomial at z.

- **Signature**: `(R,R)->R`

### `hurwitzLerchPhi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L221)\]

hurwitzLerchPhi(z,s,a) computes the Hurwitz-Lerch transcendent phi function.

- **Signature**: `(R,R,R)->R`

### `hurwitzZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L219)\]

hurwitzZeta(s,a) computes the Hurwitz zeta.

- **Signature**: `(R,R)->R`

### `hyperFactorial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L248)\]

hyperFactorial(n) computes the hyperfactorial of n.

- **Signature**: `(R)->R`

### `hypergeometric0F1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L287)\]

hypergeometric0F1(a,z) is the hypergeometric 0F1.

- **Signature**: `(R,R)->R`

### `hypergeometric0F1Regularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L289)\]

hypergeometric0F1Regularized(a,z) is the regularized hypergeometric 0F1.

- **Signature**: `(R,R)->R`

### `hypergeometric1F1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L291)\]

hypergeometric1F1(a,b,z) is the Kummer confluent hypergeometric function 1F1.

- **Signature**: `(R,R,R)->R`

### `hypergeometric1F1Regularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L294)\]

hypergeometric1F1Regularized(a,b,z) is the regularized confluent hypergeometric function 1F1.

- **Signature**: `(R,R,R)->R`

### `hypergeometricU` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L297)\]

hypergeometricU(a,b,z) is the confluent hypergeometric function U.

- **Signature**: `(R,R,R)->R`

### `inverseBetaRegularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L236)\]

inverseBetaRegularized(s,a,b) computes the Beta inverse.

- **Signature**: `(R,R,R)->R`

### `inverseErf` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L225)\]

inverseErf(z) computes the inverse error function of z.

- **Signature**: `(R)->R`

### `inverseErfc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L227)\]

inverseErfc(z) computes the inverse complementary error function of z.

- **Signature**: `(R)->R`

### `inverseGammaRegularized` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L238)\]

inverseGammaRegularized(a,s) computes the Gamma inverse.

- **Signature**: `(R,R)->R`

### `inverseGudermannian` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L178)\]

inverseGudermannian(z) computes the inverse Gudermannian of z.

- **Signature**: `(R)->R`

### `inverseHaversine` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L174)\]

inverseHaversine(z) computes the inverse Haversine of z.

- **Signature**: `(R)->R`

### `inverseJacobiCn` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L353)\]

inverseJacobiCn(nu, m) computes the inverse Jacobi's CN elliptic function.

- **Signature**: `(R,R)->R`

### `inverseJacobiSn` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L351)\]

inverseJacobiSn(nu, m) computes the inverse Jacobi's SN elliptic function.

- **Signature**: `(R,R)->R`

### `jacobiAmplitude` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L346)\]

jacobiAmplitude(u,m) computes the amplitude function am.

- **Signature**: `(R,R)->R`

### `jacobiCn` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L138)\]

jacobiCn(z, m) is the Jacobi elliptic cn function, defined by jacobiCn(z, m)^2 + jacobiSn(z, m)^2 = 1 and jacobiCn(0, m) = 1.

- **Signature**: `(R,R)->R`

### `jacobiDn` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L142)\]

jacobiDn(z, m) is the Jacobi elliptic dn function, defined by jacobiDn(z, m)^2 + m*jacobiSn(z, m)^2 = 1 and jacobiDn(0, m) = 1.

- **Signature**: `(R,R)->R`

### `jacobiP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L285)\]

jacobiP(n, a, b, z) evaluates the Jacobi polynomial at z.

- **Signature**: `(R,R,R,R)->R`

### `jacobiSn` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L135)\]

jacobiSn(z, m) is the Jacobi elliptic sn function, defined by the formula jacobiSn(ellipticF(z, m), m) = z.

- **Signature**: `(R,R)->R`

### `jacobiTheta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L151)\]

jacobiTheta(n, z, m) are the Jacobi theta functions.

- **Signature**: `(WSInteger,R,R)->R`

### `jacobiZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L146)\]

jacobiZeta(phi,m) computes the Jacobi Zeta function.

- **Signature**: `(R,R)->R`
- **Signature**: `(R,R)->R`

### `kelvinBei` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L117)\]

kelvinBei(v, z) is the Kelvin bei function defined by equality kelvinBei(v, z) = imag(besselJ(v, exp(3*Rpi*Ri/4)*z)) for z and v real.

- **Signature**: `(R,R)->R`

### `kelvinBer` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L121)\]

kelvinBer(v, z) is the Kelvin ber function defined by equality kelvinBer(v, z) = real(besselJ(v, exp(3*Rpi*Ri/4)*z)) for z and v real.

- **Signature**: `(R,R)->R`

### `kelvinKei` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L125)\]

kelvinKei(v, z) is the Kelvin kei function defined by equality kelvinKei(v, z) = imag(exp(-v*Rpi*Ri/2)*besselK(v, exp(Rpi*Ri/4)*z)) for z and v real.

- **Signature**: `(R,R)->R`

### `kelvinKer` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L130)\]

kelvinKer(v, z) is the Kelvin ker function defined by equality kelvinKer(v, z) = real(exp(-v*Rpi*Ri/2)*besselK(v, exp(Rpi*Ri/4)*z)) for z and v real.

- **Signature**: `(R,R)->R`

### `kleinInvariantJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L385)\]

kleinInvariantJ(tau) computes the Klein's absolute invariant.

- **Signature**: `(R)->R`

### `laguerreL` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L279)\]

laguerreL(n, z) evaluates the Laguerre polynomial at z.

- **Signature**: `(R,R)->R`

laguerreL(n, a, z) evaluates the generalized Laguerre polynomial a z.

- **Signature**: `(R,R,R)->R`

### `lambertW` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L70)\]

lambertW(z) = w is the principal branch of the solution to the equation we^w = z.

- **Signature**: `(R)->R`

lambertW(k,z) returns the k-th solution to LambertW function.

- **Signature**: `(WSInteger,R)->R`

### `legendreP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L113)\]

legendreP(n, z) evaluates the Legendre polynomial of the first kind at z.

- **Signature**: `(R,R)->R`

legendreP(n, m, z) evaluates the associated Legendre polynomial of the first type at z.

- **Signature**: `(R,R,R)->R`
- **Signature**: `(R,R,R)->R`

### `legendreQ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L115)\]

legendreQ(n, z) returns the Legendre function of the second kind.

- **Signature**: `(R,R)->R`

legendreQ(n, m, z) evaluates the associated Legendre function of the second kind at z.

- **Signature**: `(R,R,R)->R`
- **Signature**: `(R,R,R)->R`

### `lerchPhi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L153)\]

lerchPhi(z,s,a) returns Lerch's transcendent phi of arguments.

- **Signature**: `(R,R,R)->R`
- **Signature**: `(R,R,R)->R`

### `logBarnesG` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L244)\]

logBarnesG(z) is the logarithm of Barnes-G.

- **Signature**: `(R)->R`

### `logGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L242)\]

logGamma(z) returns the log-Gamma of z.

- **Signature**: `(R)->R`

### `lommelS1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L109)\]

lommelS1(mu, nu, z) is the Lommel s function.

- **Signature**: `(R,R,R)->R`

### `lommelS2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L111)\]

lommelS2(mu, nu, z) is the Lommel S function.

- **Signature**: `(R,R,R)->R`

### `mathieuC` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L327)\]

mathieuC(a,q,z) is the even Mathieu function with characteristic a and parameter q.

- **Signature**: `(R,R,R)->R`

### `mathieuCPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L333)\]

mathieuCPrime(a,q,z) derivative of the even Mathieu function.

- **Signature**: `(R,R,R)->R`

### `mathieuCharacteristicA` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L337)\]

mathieuCharacteristicA(r,q) returns the characteristic for even Mathieu function.

- **Signature**: `(R,R)->R`

### `mathieuCharacteristicB` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L340)\]

mathieuCharacteristicB(r,q) returns the characteristic for odd Mathieu function.

- **Signature**: `(R,R)->R`

### `mathieuCharacteristicExponent` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L343)\]

mathieuCharacteristicExponent(a,q) returns the characteristic exponent of the Mathieu function.

- **Signature**: `(R,R)->R`

### `mathieuS` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L330)\]

mathieuS(b,q,z) is the odd Mathieu function with characteristic b and parameter q.

- **Signature**: `(R,R,R)->R`

### `mathieuSPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L335)\]

mathieuSPrime(b,q,z) derivative of the odd Mathieu function.

- **Signature**: `(R,R,R)->R`

### `modularLambda` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L387)\]

modularLambda(tau) computes the lambda modular function.

- **Signature**: `(R)->R`

### `parabolicCylinderD` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L303)\]

parabolicCylinderD(nu,z) computes the parabolic cylinder function D of z.

- **Signature**: `(R,R)->R`

### `pochhammer` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L240)\]

pochhammer(a,n) returns the Pochhammer symbol.

- **Signature**: `(R,R)->R`

### `polygamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L25)\]

polygamma(k, z) is the k-th derivative of digamma(z), (often written psi(k, z) in the literature).

- **Signature**: `(R,R)->R`

### `polylog` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L73)\]

polylog(s, z) is the polylogarithm of order s at z.

- **Signature**: `(R,R)->R`

polylog(n,p,z) is the Nielsen generalized polylogarithm function.

- **Signature**: `(R,R,R)->R`

### `qBinomial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L319)\]

qBinomial(n,m,q) returns the q-analog of binomial coefficient.

- **Signature**: `(R,R,R)->R`

### `qFactorial` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L317)\]

qFactorial(z,q) returns the q-analog of factorial of z.

- **Signature**: `(R,R)->R`

### `qGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L321)\]

qGamma(z,q) returns the q-analog of Euler Gamma of z.

- **Signature**: `(R,R)->R`

### `qPochhammer` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L313)\]

qPochhammer(z,q) returns the q-Pochhammer symbol of z.

- **Signature**: `(R,R)->R`

qPochhammer(z,q,n) returns the q-Pochhammer symbol of z.

- **Signature**: `(R,R,R)->R`

### `qPolyGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L323)\]

qPolyGamma(z,q) returns the q-digamma of z.

- **Signature**: `(R,R)->R`

qPolyGamma(n,z,q) returns the n-th derivative of the q-digamma function of z.

- **Signature**: `(R,R,R)->R`

### `ramanujanTau` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L202)\]

ramanujanTau(n) returns the Ramanujan tau of n.

- **Signature**: `(R)->R`

### `ramanujanTauL` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L204)\]

ramanujanTauL(s) computes the Ramanujan tau Dirichlet L-function of s.

- **Signature**: `(R)->R`

### `ramanujanTauTheta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L206)\]

ramanujanTauTheta(z) returns the Ramanujan tau theta of z.

- **Signature**: `(R)->R`

### `ramanujanTauZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L208)\]

ramanujanTauZ(t) computes the Ramanujan tau Z-function of t.

- **Signature**: `(R)->R`

### `riemannSiegelTheta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L210)\]

riemannSiegelTheta(t) returns the Riemann-Siegel theta function of t.

- **Signature**: `(R)->R`

### `riemannSiegelZ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L212)\]

riemannSiegelZ(t) computes the Riemann-Siegel Z function of t.

- **Signature**: `(R)->R`

### `riemannZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L155)\]

riemannZeta(z) is the Riemann Zeta function.

- **Signature**: `(R)->R`

riemannZeta(s,a) is the generalized Riemann Zeta function.

- **Signature**: `(R,R)->R`

### `sign` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L14)\]

sign(x) is 1 if x is positive, -1 if x is negative, 0 if x equals 0.

- **Signature**: `(R)->R`

### `sphericalBesselJ` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L182)\]

sphericalBesselJ(n,z) returns the spherical Bessel of the first kind of z.

- **Signature**: `(R,R)->R`

### `sphericalBesselY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L185)\]

sphericalBesselY(n,z) returns the spherical Bessel of the second kind of z.

- **Signature**: `(R,R)->R`

### `sphericalHankelH1` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L188)\]

sphericalHankelH1(n,z) returns the spherical Hankel of the first kind of z.

- **Signature**: `(R,R)->R`

### `sphericalHankelH2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L191)\]

sphericalHankelH2(n,z) computes the spherical Hankel of the second kind of z.

- **Signature**: `(R,R)->R`

### `sphericalHarmonicY` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L263)\]

sphericalHarmonicY(l, m, theta, phi) returns the spherical harmonic Y or evaluates it.

- **Signature**: `(R,R,R,R)->R`

### `stieltjesGamma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L215)\]

stieltjesGamma(n) returns the n-th Stieltjes constant.

- **Signature**: `(WSInteger)->R`

stieltjesGamma(n,a) returns the generalized n-th Stieltjes constant.

- **Signature**: `(WSInteger,R)->R`

### `struveH` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L98)\]

struveH(v, z) is the Struve H function.

- **Signature**: `(R,R)->R`

### `struveL` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L100)\]

struveL(v, z) is the Struve L function defined by the formula struveL(v, z) = -Ri^exp(-v*Rpi*Ri/2)*struveH(v,Ri*z).

- **Signature**: `(R,R)->R`

### `unitStep` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L348)\]

unitStep(x) returns the unit step function i.e. 0 for x less than 0, 1 for x greater or equal to 0.

- **Signature**: `(R)->WSExpression`

### `weberE` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L94)\]

weberE(v, z) is the Weber E function.

- **Signature**: `(R,R)->R`

weberE(v, n, z) is the associated Weber E function.

- **Signature**: `(R,R,R)->R`

### `weierstrassP` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L75)\]

weierstrassP(g2, g3, z) is the Weierstrass P function.

- **Signature**: `(R,R,R)->R`

### `weierstrassPInverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L84)\]

weierstrassPInverse(g2, g3, z) is the inverse of Weierstrass P function, defined by the formula weierstrassP(g2, g3, weierstrassPInverse(g2, g3, z)) = z.

- **Signature**: `(R,R,R)->R`

### `weierstrassPPrime` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L77)\]

weierstrassPPrime(g2, g3, z) is the derivative of Weierstrass P function.

- **Signature**: `(R,R,R)->R`

### `weierstrassSigma` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L80)\]

weierstrassSigma(g2, g3, z) is the Weierstrass sigma function.

- **Signature**: `(R,R,R)->R`

### `weierstrassZeta` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L82)\]

weierstrassZeta(g2, g3, z) is the Weierstrass Zeta function.

- **Signature**: `(R,R,R)->R`

### `whittakerM` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L88)\]

whittakerM(k,m,z) computes the Whittaker function M of z.

- **Signature**: `(R,R,R)->R`
- **Signature**: `(R,R,R)->R`

### `whittakerW` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L90)\]

whittakerW(k,m,z) computes the Whittaker function W of z.

- **Signature**: `(R,R,R)->R`
- **Signature**: `(R,R,R)->R`

### `zernikeR` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsnsf.spad#L283)\]

zernikeR(n, m, z) evaluates the Zernike radial polynomial at z.

- **Signature**: `(R,R,R)->R`
---
[Back to Index](../index.md)
