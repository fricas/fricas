# JLFloatSpecialFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Special functions computed using Julia's ecosystem.

**JLFloatSpecialFunctions is a package constructor**  
**Abbreviation for JLFloatSpecialFunctions is JFSF**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 acosd : JLFloat -> JLFloat                             acotd : JLFloat -> JLFloat
 acscd : JLFloat -> JLFloat                             asecd : JLFloat -> JLFloat
 asind : JLFloat -> JLFloat                             atand : JLFloat -> JLFloat
 atand : (JLFloat, JLFloat) -> JLFloat                  cosc : JLFloat -> JLFloat
 cosd : JLFloat -> JLFloat                              cospi : JLFloat -> JLFloat
 cotd : JLFloat -> JLFloat                              cscd : JLFloat -> JLFloat
 deg2Rad : JLFloat -> JLFloat                           hypot : (JLFloat, JLFloat) -> JLFloat
 ldexp : (JLFloat, Integer) -> JLFloat                  rad2Deg : JLFloat -> JLFloat
 secd : JLFloat -> JLFloat                              sinc : JLFloat -> JLFloat
 sind : JLFloat -> JLFloat                              sinpi : JLFloat -> JLFloat
 tand : JLFloat -> JLFloat                              tanpi : JLFloat -> JLFloat
```

## Operations added

### `acosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L40)\]

acosd(x) computes the inverse cosine of x, where output is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `acotd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L46)\]

acotd(x) computes the inverse cotangent of x, where output is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `acscd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L50)\]

acscd(x) computes the inverse cosecant of x, where output is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `asecd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L48)\]

asecd(x) computes the inverse secant of x, where output is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `asind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L38)\]

asind(x) computes the inverse sine of x, where output is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `atand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L42)\]

atand(x) computes the inverse tangent of x, where output is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

atand(x, y) computes the inverse tangent of x/y, where output is in degrees.

- **Signature**: `(JLFloat,JLFloat)->JLFloat`

### `cosc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L22)\]

cosc(x) computes cos(pi*x)/x−sin(pi*x)/(pi*x^2) if x~=0, and 0 if x=0 i.e. the derivative of sinc(x).

- **Signature**: `(JLFloat)->JLFloat`

### `cosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L18)\]

cosd(x) computes cosine of x, where x is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `cospi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L20)\]

cospi(x) computes cos(pi*x) more accurately.

- **Signature**: `(JLFloat)->JLFloat`

### `cotd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L29)\]

cotd(x) computes cotangent of x, where x is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `cscd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L33)\]

cscd(x) computes cosecant of x, where x is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `deg2Rad` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L54)\]

deg2Rad(x) converts x to radians, where x is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `hypot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L35)\]

hypot(x,y) computes the hypotenuse avoiding overflow and underflow.

- **Signature**: `(JLFloat,JLFloat)->JLFloat`

### `ldexp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L8)\]

ldexp(x,n) computes x*2^n

- **Signature**: `(JLFloat,Integer)->JLFloat`

### `rad2Deg` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L52)\]

rad2Deg(x) converts x to degrees, where x is in radians.

- **Signature**: `(JLFloat)->JLFloat`

### `secd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L31)\]

secd(x) computes secant of x, where x is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `sinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L15)\]

sinc(x) computes the normalized sinc, i.e. sin(pi*x)/(pi*x) if x~=0, and 1 if x=0.

- **Signature**: `(JLFloat)->JLFloat`

### `sind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L11)\]

sind(x) computes sine of x, where x is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `sinpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L13)\]

sinpi(x) computes sin(pi*x) more accurately.

- **Signature**: `(JLFloat)->JLFloat`

### `tand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L25)\]

tand(x) computes tangent of x, where x is in degrees.

- **Signature**: `(JLFloat)->JLFloat`

### `tanpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jfsf.spad#L27)\]

tanpi(x) computes tan(pi*x) more accurately.

- **Signature**: `(JLFloat)->JLFloat`
---
[Back to Index](../index.md)
