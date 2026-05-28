# JLFloat32SpecialFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Special functions computed using Julia's ecosystem. They are here essentially for "completeness" purpose withJLFloat32. You should use the DoubleFloat's special functions if available, calling Julia functions is costly.

**JLFloat32SpecialFunctions is a package constructor**  
**Abbreviation for JLFloat32SpecialFunctions is JF32SF**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 acosd : JLFloat32 -> JLFloat32                         acotd : JLFloat32 -> JLFloat32
 acscd : JLFloat32 -> JLFloat32                         asecd : JLFloat32 -> JLFloat32
 asind : JLFloat32 -> JLFloat32                         atand : JLFloat32 -> JLFloat32
 atand : (JLFloat32, JLFloat32) -> JLFloat32            cosc : JLFloat32 -> JLFloat32
 cosd : JLFloat32 -> JLFloat32                          cospi : JLFloat32 -> JLFloat32
 cotd : JLFloat32 -> JLFloat32                          cscd : JLFloat32 -> JLFloat32
 deg2rad : JLFloat32 -> JLFloat32                       exp10 : JLFloat32 -> JLFloat32
 exp2 : JLFloat32 -> JLFloat32                          hypot : (JLFloat32, JLFloat32) -> JLFloat32
 ldexp : (JLFloat32, JLInt64) -> JLFloat32              log1p : JLFloat32 -> JLFloat32
 rad2deg : JLFloat32 -> JLFloat32                       secd : JLFloat32 -> JLFloat32
 sinc : JLFloat32 -> JLFloat32                          sind : JLFloat32 -> JLFloat32
 sinpi : JLFloat32 -> JLFloat32                         tand : JLFloat32 -> JLFloat32
 tanpi : JLFloat32 -> JLFloat32
```

## Operations added

### `acosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L47)\]

acosd(x) computes the inverse cosine of x, where output is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `acotd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L53)\]

acotd(x) computes the inverse cotangent of x, where output is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `acscd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L57)\]

acscd(x) computes the inverse cosecant of x, where output is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `asecd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L55)\]

asecd(x) computes the inverse secant of x, where output is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `asind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L45)\]

asind(x) computes the inverse sine of x, where output is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `atand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L49)\]

atand(x) computes the inverse tangent of x, where output is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

atand(x, y) computes the inverse tangent of x/y, where output is in degrees.

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `cosc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L29)\]

cosc(x) computes cos(pi*x)/x−sin(pi*x)/(pi*x^2) if x~=0, and 0 if x=0 i.e. the derivative of sinc(x).

- **Signature**: `(JLFloat32)->JLFloat32`

### `cosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L25)\]

cosd(x) computes cosine of x, where x is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `cospi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L27)\]

cospi(x) computes cos(pi*x) more accurately.

- **Signature**: `(JLFloat32)->JLFloat32`

### `cotd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L36)\]

cotd(x) computes cotangent of x, where x is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `cscd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L40)\]

cscd(x) computes cosecant of x, where x is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `deg2rad` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L61)\]

deg2rad(x) converts x to radians, where x is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `exp10` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L14)\]

exp10(x) computes the base 10 exponential of x.

- **Signature**: `(JLFloat32)->JLFloat32`

### `exp2` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L12)\]

exp2(x) computes the base 2 exponential of x.

- **Signature**: `(JLFloat32)->JLFloat32`

### `hypot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L42)\]

hypot(x,y) computes the hypotenuse avoiding overflow and underflow.

- **Signature**: `(JLFloat32,JLFloat32)->JLFloat32`

### `ldexp` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L10)\]

ldexp(x,n) computes x*2^n

- **Signature**: `(JLFloat32,JLInt64)->JLFloat32`

### `log1p` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L16)\]

log1p(x) computes accurately natural logarithm of 1+x.

- **Signature**: `(JLFloat32)->JLFloat32`

### `rad2deg` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L59)\]

rad2deg(x) converts x to degrees, where x is in radians.

- **Signature**: `(JLFloat32)->JLFloat32`

### `secd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L38)\]

secd(x) computes secant of x, where x is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `sinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L23)\]

sinc(x) computes sin(pi*x)/(pi*x) if x~=0, and 1 if x=0.

- **Signature**: `(JLFloat32)->JLFloat32`

### `sind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L19)\]

sind(x) computes sine of x, where x is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `sinpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L21)\]

sinpi(x) computes sin(pi*x) more accurately.

- **Signature**: `(JLFloat32)->JLFloat32`

### `tand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L32)\]

tand(x) computes tangent of x, where x is in degrees.

- **Signature**: `(JLFloat32)->JLFloat32`

### `tanpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jf32sf.spad#L34)\]

tanpi(x) computes tan(pi*x) more accurately.

- **Signature**: `(JLFloat32)->JLFloat32`
---
[Back to Index](../index.md)
