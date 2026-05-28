# JLComplexFloatSpecialFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L1)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Special functions computed using Julia's ecosystem.

**JLComplexFloatSpecialFunctions is a package constructor**  
**Abbreviation for JLComplexFloatSpecialFunctions is JCFSF**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 acosd : JLComplexFloat -> JLComplexFloat               acotd : JLComplexFloat -> JLComplexFloat
 acscd : JLComplexFloat -> JLComplexFloat               asecd : JLComplexFloat -> JLComplexFloat
 asind : JLComplexFloat -> JLComplexFloat               atand : JLComplexFloat -> JLComplexFloat
 cosc : JLComplexFloat -> JLComplexFloat                cosd : JLComplexFloat -> JLComplexFloat
 cospi : JLComplexFloat -> JLComplexFloat               cotd : JLComplexFloat -> JLComplexFloat
 cscd : JLComplexFloat -> JLComplexFloat                deg2Rad : JLComplexFloat -> JLComplexFloat
 rad2Deg : JLComplexFloat -> JLComplexFloat             secd : JLComplexFloat -> JLComplexFloat
 sinc : JLComplexFloat -> JLComplexFloat                sind : JLComplexFloat -> JLComplexFloat
 sinpi : JLComplexFloat -> JLComplexFloat               tand : JLComplexFloat -> JLComplexFloat
 tanpi : JLComplexFloat -> JLComplexFloat
 atand : (JLComplexFloat, JLComplexFloat) -> JLComplexFloat
 hypot : (JLComplexFloat, JLComplexFloat) -> JLComplexFloat
```

## Operations added

### `acosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L34)\]

acosd(x) computes the inverse cosine of x, where output is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `acotd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L40)\]

acotd(x) computes the inverse cotangent of x, where output is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `acscd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L44)\]

acscd(x) computes the inverse cosecant of x, where output is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `asecd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L42)\]

asecd(x) computes the inverse secant of x, where output is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `asind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L32)\]

asind(x) computes the inverse sine of x, where output is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `atand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L36)\]

atand(x) computes the inverse tangent of x, where output is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

atand(x, y) computes the inverse tangent of x/y, where output is in degrees.

- **Signature**: `(JLComplexFloat,JLComplexFloat)->JLComplexFloat`

### `cosc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L16)\]

cosc(x) computes cos(pi*x)/x−sin(pi*x)/(pi*x^2) if x~=0, and 0 if x=0 i.e. the derivative of sinc(x).

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `cosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L12)\]

cosd(x) computes cosine of x, where x is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `cospi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L14)\]

cospi(x) computes cos(pi*x) more accurately.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `cotd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L23)\]

cotd(x) computes cotangent of x, where x is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `cscd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L27)\]

cscd(x) computes cosecant of x, where x is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `deg2Rad` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L48)\]

deg2Rad(x) converts x to radians, where x is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `hypot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L29)\]

hypot(x,y) computes the hypotenuse avoiding overflow and underflow.

- **Signature**: `(JLComplexFloat,JLComplexFloat)->JLComplexFloat`

### `rad2Deg` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L46)\]

rad2Deg(x) converts x to degrees, where x is in radians.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `secd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L25)\]

secd(x) computes secant of x, where x is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `sinc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L10)\]

sinc(x) computes sin(pi*x)/(pi*x) if x~=0, and 1 if x=0.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `sind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L6)\]

sind(x) computes sine of x, where x is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `sinpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L8)\]

sinpi(x) computes sin(pi*x) more accurately.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `tand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L19)\]

tand(x) computes tangent of x, where x is in degrees.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`

### `tanpi` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jcfsf.spad#L21)\]

tanpi(x) computes tan(pi*x) more accurately.

- **Signature**: `(JLComplexFloat)->JLComplexFloat`
---
[Back to Index](../index.md)
