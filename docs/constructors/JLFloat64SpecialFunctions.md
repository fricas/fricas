# JLFloat64SpecialFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Special functions computed using Julia's ecosystem. They are here essentially for "completeness" purpose withJLFloat64. You should use the DoubleFloat's special functions if available, calling Julia functions is costly.

**JLFloat64SpecialFunctions is a package constructor**  
**Abbreviation for JLFloat64SpecialFunctions is JF64SF**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 acosd : JLFloat64 -> JLFloat64                         acotd : JLFloat64 -> JLFloat64
 acscd : JLFloat64 -> JLFloat64                         asecd : JLFloat64 -> JLFloat64
 asind : JLFloat64 -> JLFloat64                         atand : JLFloat64 -> JLFloat64
 atand : (JLFloat64, JLFloat64) -> JLFloat64            cosc : JLFloat64 -> JLFloat64
 cosd : JLFloat64 -> JLFloat64                          cospi : JLFloat64 -> JLFloat64
 cotd : JLFloat64 -> JLFloat64                          cscd : JLFloat64 -> JLFloat64
 deg2rad : JLFloat64 -> JLFloat64                       exp10 : JLFloat64 -> JLFloat64
 exp2 : JLFloat64 -> JLFloat64                          hypot : (JLFloat64, JLFloat64) -> JLFloat64
 ldexp : (JLFloat64, JLInt64) -> JLFloat64              log1p : JLFloat64 -> JLFloat64
 rad2deg : JLFloat64 -> JLFloat64                       secd : JLFloat64 -> JLFloat64
 sinc : JLFloat64 -> JLFloat64                          sind : JLFloat64 -> JLFloat64
 sinpi : JLFloat64 -> JLFloat64                         tand : JLFloat64 -> JLFloat64
 tanpi : JLFloat64 -> JLFloat64
```

## Operations added

### `acosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L47)\]

acosd(x) computes the inverse cosine of x, where output is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `acotd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L53)\]

acotd(x) computes the inverse cotangent of x, where output is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `acscd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L57)\]

acscd(x) computes the inverse cosecant of x, where output is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `asecd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L55)\]

asecd(x) computes the inverse secant of x, where output is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `asind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L45)\]

asind(x) computes the inverse sine of x, where output is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `atand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L49)\]

atand(x) computes the inverse tangent of x, where output is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

atand(x, y) computes the inverse tangent of x/y, where output is in degrees.

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `cosc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L29)\]

cosc(x) computes cos(pi*x)/x−sin(pi*x)/(pi*x^2) if x~=0, and 0 if x=0 i.e. the derivative of sinc(x).

- **Signature**: `(JLFloat64)->JLFloat64`

### `cosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L25)\]

cosd(x) computes cosine of x, where x is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `cospi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L27)\]

cospi(x) computes cos(pi*x) more accurately.

- **Signature**: `(JLFloat64)->JLFloat64`

### `cotd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L36)\]

cotd(x) computes cotangent of x, where x is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `cscd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L40)\]

cscd(x) computes cosecant of x, where x is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `deg2rad` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L61)\]

deg2rad(x) converts x to radians, where x is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `exp10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L14)\]

exp10(x) computes the base 10 exponential of x.

- **Signature**: `(JLFloat64)->JLFloat64`

### `exp2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L12)\]

exp2(x) computes the base 2 exponential of x.

- **Signature**: `(JLFloat64)->JLFloat64`

### `hypot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L42)\]

hypot(x,y) computes the hypotenuse avoiding overflow and underflow.

- **Signature**: `(JLFloat64,JLFloat64)->JLFloat64`

### `ldexp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L10)\]

ldexp(x,n) computes x*2^n

- **Signature**: `(JLFloat64,JLInt64)->JLFloat64`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L16)\]

log1p(x) computes accurately natural logarithm of 1+x.

- **Signature**: `(JLFloat64)->JLFloat64`

### `rad2deg` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L59)\]

rad2deg(x) converts x to degrees, where x is in radians.

- **Signature**: `(JLFloat64)->JLFloat64`

### `secd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L38)\]

secd(x) computes secant of x, where x is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `sinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L23)\]

sinc(x) computes sin(pi*x)/(pi*x) if x~=0, and 1 if x=0.

- **Signature**: `(JLFloat64)->JLFloat64`

### `sind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L19)\]

sind(x) computes sine of x, where x is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `sinpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L21)\]

sinpi(x) computes sin(pi*x) more accurately.

- **Signature**: `(JLFloat64)->JLFloat64`

### `tand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L32)\]

tand(x) computes tangent of x, where x is in degrees.

- **Signature**: `(JLFloat64)->JLFloat64`

### `tanpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf64sf.spad#L34)\]

tanpi(x) computes tan(pi*x) more accurately.

- **Signature**: `(JLFloat64)->JLFloat64`
---
[Back to Index](../index.md)
