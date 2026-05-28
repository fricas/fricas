# JLComplexF64MatrixTranscendentalFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L998)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

Complex matrix transcendental functions computed using Julia and its algorithms. 64 bits version.

**JLComplexF64MatrixTranscendentalFunctions is a package constructor**  
**Abbreviation for JLComplexF64MatrixTranscendentalFunctions is JCF64MTF**  
**This constructor is not exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 acos : JLComplexF64Matrix -> JLComplexF64Matrix        acosd : JLComplexF64Matrix -> JLComplexF64Matrix
 acosh : JLComplexF64Matrix -> JLComplexF64Matrix       acot : JLComplexF64Matrix -> JLComplexF64Matrix
 acotd : JLComplexF64Matrix -> JLComplexF64Matrix       acoth : JLComplexF64Matrix -> JLComplexF64Matrix
 acsc : JLComplexF64Matrix -> JLComplexF64Matrix        acscd : JLComplexF64Matrix -> JLComplexF64Matrix
 acsch : JLComplexF64Matrix -> JLComplexF64Matrix       asec : JLComplexF64Matrix -> JLComplexF64Matrix
 asecd : JLComplexF64Matrix -> JLComplexF64Matrix       asech : JLComplexF64Matrix -> JLComplexF64Matrix
 asin : JLComplexF64Matrix -> JLComplexF64Matrix        asind : JLComplexF64Matrix -> JLComplexF64Matrix
 asinh : JLComplexF64Matrix -> JLComplexF64Matrix       atan : JLComplexF64Matrix -> JLComplexF64Matrix
 atand : JLComplexF64Matrix -> JLComplexF64Matrix       atanh : JLComplexF64Matrix -> JLComplexF64Matrix
 cis : JLComplexF64Matrix -> JLComplexF64Matrix         cos : JLComplexF64Matrix -> JLComplexF64Matrix
 cosd : JLComplexF64Matrix -> JLComplexF64Matrix        cosh : JLComplexF64Matrix -> JLComplexF64Matrix
 cot : JLComplexF64Matrix -> JLComplexF64Matrix         coth : JLComplexF64Matrix -> JLComplexF64Matrix
 csc : JLComplexF64Matrix -> JLComplexF64Matrix         csch : JLComplexF64Matrix -> JLComplexF64Matrix
 sec : JLComplexF64Matrix -> JLComplexF64Matrix         sech : JLComplexF64Matrix -> JLComplexF64Matrix
 sin : JLComplexF64Matrix -> JLComplexF64Matrix         sind : JLComplexF64Matrix -> JLComplexF64Matrix
 sinh : JLComplexF64Matrix -> JLComplexF64Matrix        tan : JLComplexF64Matrix -> JLComplexF64Matrix
 tand : JLComplexF64Matrix -> JLComplexF64Matrix        tanh : JLComplexF64Matrix -> JLComplexF64Matrix
```

## Operations added

### `acos` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1041)\]

acos(m) computes the inverse matrix cosine of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `acosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1043)\]

acosd(m) computes the inverse matrix cosine of m. Output is in degrees.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `acosh` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1063)\]

acosh(m) computes the inverse matrix hyperbolic cosine of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `acot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1057)\]

acot(m) computes the inverse matrix cotangent of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `acotd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1059)\]

acotd(m) computes the inverse matrix cotangent of m. Output is in degrees.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `acoth` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1071)\]

acoth(m) computes the inverse matrix hyperbolic cotangent of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `acsc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1053)\]

acsc(m) computes the inverse matrix cosecant of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `acscd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1055)\]

acscd(m) computes the inverse matrix cosecant of m. Output is in degrees.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `acsch` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1069)\]

acsch(m) computes the inverse matrix hyperbolic cosecant of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `asec` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1049)\]

asec(m) computes the inverse matrix secant of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `asecd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1051)\]

asecd(m) computes the inverse matrix secant of m. Output is in degrees.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `asech` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1067)\]

asech(m) computes the inverse matrix hyperbolic secant of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `asin` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1037)\]

asin(m) computes the inverse matrix sine of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `asind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1039)\]

asind(m) computes the inverse matrix sine of m. Output is in degrees.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `asinh` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1061)\]

asinh(m) computes the inverse matrix hyperbolic sine of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `atan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1045)\]

atan(m) computes the inverse matrix tangent of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `atand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1047)\]

atand(m) computes the inverse matrix tangent of m. Output is in degrees.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `atanh` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1065)\]

atanh(m) computes the inverse matrix hyperbolic tangent of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `cis` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1073)\]

cis(m) returns exp(%i*m) computed efficiently.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `cos` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1011)\]

cos(m) computes the matrix cosine of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `cosd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1013)\]

cosd(m) computes the matrix cosine of m, where m is in degrees.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `cosh` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1027)\]

cosh(m) computes the matrix hyperbolic cosine of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `cot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1023)\]

cot(m) computes the matrix cotangent of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `coth` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1035)\]

coth(m) computes the matrix hyperbolic cotangent of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `csc` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1021)\]

csc(m) computes the matrix cosecant of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `csch` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1033)\]

csch(m) computes the matrix hyperbolic cosecant of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `sec` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1019)\]

sec(m) computes the matrix secant of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `sech` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1031)\]

sech(m) computes the matrix hyperbolic secant of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `sin` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1007)\]

sin(m) computes the matrix sine of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `sind` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1009)\]

sind(m) computes the matrix sine of m, where m is in degrees.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `sinh` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1025)\]

sinh(m) computes the matrix hyperbolic sine of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `tan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1015)\]

tan(m) computes the matrix tangent of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `tand` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1017)\]

tand(m) computes the matrix tangent of m, where m is in degrees.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`

### `tanh` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jla64.spad#L1029)\]

tanh(m) computes the matrix hyperbolic tangent of m.

- **Signature**: `(JLComplexF64Matrix)->JLComplexF64Matrix`
---
[Back to Index](../index.md)
