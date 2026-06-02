# WSUtilityFunctions

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L1)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic utility functions using the MathLink Julia package.

**WSUtilityFunctions is a package constructor**  
**Abbreviation for WSUtilityFunctions is WSUF**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 gaussianMatrix : WSReal -> WSMatrix(WSReal)            jWSDateObject : () -> WSExpression
 jWSDateObject : WSExpression -> WSExpression           jWSRange : WSInteger -> WSList(WSInteger)
 jlWSDateString : () -> WSExpression                    jlWSDateString : WSExpression -> WSExpression
 jlWSDocumentation : Symbol -> WSExpression             jlWSExport : (String, WSExpression) -> WSExpression
 jlWSFileFormat : WSString -> WSExpression              jlWSImport : WSString -> WSExpression
 jlWSImport : (WSString, WSString) -> WSExpression      jlWSSeedRandom! : WSInteger -> WSExpression
 urand01Real : PositiveInteger -> WSVector(WSReal)
 fourierMatrix : PositiveInteger -> WSMatrix(WSExpression)
 hankelMatrix : PositiveInteger -> WSMatrix(WSInteger)
 hilbertMatrix : PositiveInteger -> WSMatrix(WSRational)
 identityMatrix : PositiveInteger -> WSMatrix(WSInteger)
 jWSDateObject : (WSExpression, WSExpression) -> WSExpression
 jWSRange : (WSInteger, WSInteger) -> WSList(WSInteger)
 jWSRange : (WSInteger, WSInteger, WSInteger) -> WSList(WSInteger)
 jlWSDateString : (WSExpression, WSExpression) -> WSExpression
 jlWSExportString : (WSExpression, WSString) -> WSExpression
 jlWSFileFormat : (WSString, WSExpression) -> WSExpression
 jlWSPlot : (WSExpression, WSExpression) -> WSExpression
 jlWSSnippet : (WSExpression, WSInteger) -> WSExpression
 urand01Complex : PositiveInteger -> WSVector(WSComplex)
 urand01Complex : (PositiveInteger, PositiveInteger) -> WSMatrix(WSComplex)
 urand01Real : (PositiveInteger, PositiveInteger) -> WSMatrix(WSReal)
 vandermondeMatrix : WSList(WSExpression) -> WSMatrix(WSExpression)
 vandermondeMatrix : (WSList(WSExpression), WSInteger) -> WSMatrix(WSExpression)
 zeroMatrix : (PositiveInteger, PositiveInteger) -> WSMatrix(WSInteger)
```

## Operations added

### `fourierMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L36)\]

fourierMatrix(n) returns the n x n Fourier matrix.

- **Signature**: `(PositiveInteger)->WSMatrix(WSExpression)`

### `gaussianMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L38)\]

gaussianMatrix(r) returns the Gaussian matrix with radius r. For example: 

**Example**:
```fricas
gaussianMatrix "2.2"
```

- **Signature**: `(WSReal)->WSMatrix(WSReal)`

### `hankelMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L31)\]

hankelMatrix(n) returns the square Hankel matrix with integer coefficients.

- **Signature**: `(PositiveInteger)->WSMatrix(WSInteger)`

### `hilbertMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L34)\]

hilbertMatrix(n) returns the square Hilbert matrix.

- **Signature**: `(PositiveInteger)->WSMatrix(WSRational)`

### `identityMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L49)\]

identityMatrix(n) returns the identity matrix of size n.

- **Signature**: `(PositiveInteger)->WSMatrix(WSInteger)`

### `jWSDateObject` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L63)\]

jWSDateObject() returns the WSExpression object of the local date and time.

- **Signature**: `()->WSExpression`

jWSDateObject(expr) returns the WSExpression object of the date from expr.

- **Signature**: `(WSExpression)->WSExpression`

jWSDateObject(date,type) returns the WSExpression object of the date and type, for example "Month".

- **Signature**: `(WSExpression,WSExpression)->WSExpression`

### `jWSRange` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L54)\]

jWSRange(n) returns a WSList that ranges from 1 to n. For example: 

**Example**:
```fricas
jWSRange(10)@WSLIST(WSINT)
```

- **Signature**: `(WSInteger)->WSList(WSInteger)`

jWSRange(n,m) returns a WSList that ranges from n to m.

- **Signature**: `(WSInteger,WSInteger)->WSList(WSInteger)`

jWSRange(n,m,d) returns a WSList that ranges from n to m with step d.

- **Signature**: `(WSInteger,WSInteger,WSInteger)->WSList(WSInteger)`

### `jlWSDateString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L72)\]

jlWSDateString() returns the WSExpression string of the local date and time. For example:

**Example**:
```fricas
toString j lWSDateString()
```

- **Signature**: `()->WSExpression`

jlWSDateString(expr) returns the WSExpression string of the date from a WS date object. For example:

**Example**:
```fricas
jlWSDateString jWSExpr "Tomorrow"
```

- **Signature**: `(WSExpression)->WSExpression`

jlWSDateString(expr, form) returns WSExpression string of the date from a WS date object with WS format form.For example: 

**Example**:
```fricas
jlWSDateString(jWSExpr("Now"), jWSExpr("Entity[_"Language_", _"French::367gk_"]"))
```

- **Signature**: `(WSExpression,WSExpression)->WSExpression`

### `jlWSDocumentation` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L86)\]

jlWSDocumentation(sym) prints WS information about the symbol sym. Note that the WS language is preferable. Trivial implementation. For example: 

**Example**:
```fricas
jlWSDocumentation sin
```

**Example**:
```fricas
jlWSDocumentation ArcSin
```

- **Signature**: `(Symbol)->WSExpression`

### `jlWSExport` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L115)\]

jlWSExport(file.ext, obj) exports the object obj to the file file.ext. The extension ext will determine the saved format. A WS expression for example can be exported in an image file, it will be saved in the WS 'StandardForm' whereas in FriCAS it is displayed in WS 'OutputForm': 

**Example**:
```fricas
x:=jWSExpr(x);jlWSExport("legendreP.png" , legendreP(7, x))
```

- **Signature**: `(String,WSExpression)->WSExpression`

### `jlWSExportString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L125)\]

jlWSExportString(expr, form) returns the string representation of expr in the specified format. Use toString or string to obtain the FriCAS String. For example: 

**Example**:
```fricas
jWSExpr "Probability[x < 0, x \[Distributed] Norm alDistribution[]]"
```

**Example**:
```fricas
jWSExpr "Probability[x < 1, x \[Distributed] NormalDistribution[]]"
```

**Example**:
```fricas
s tring jlWSExportString(%,jWSString "TeX")
```

- **Signature**: `(WSExpression,WSString)->WSExpression`

### `jlWSFileFormat` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L101)\]

jlWSFileFormat(src) tries to determine the format of the source src from its content. Can be used with jWSImport. For example: 

**Example**:
```fricas
jlWSFileFormat("examples/customers-100.csv")
```

- **Signature**: `(WSString)->WSExpression`

jlWSFileFormat(src, list(form)) tries to determine the format of the source src from its content using the list of formats form. Returns WS None if no format is found. Can be used with jWSImport. For example: jlWSFileFormat("examples/customers-100.csv",jWSExpr "_"CSV_",_"TSV_"")

- **Signature**: `(WSString,WSExpression)->WSExpression`

### `jlWSImport` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L92)\]

jlWSImport(src) imports the source src. For example: 

**Example**:
```fricas
file:=jlWSImport jWSString "examples/customers- 100.csv";
```

**Example**:
```fricas
jlWSDateString(file.2.11)
```

- **Signature**: `(WSString)->WSExpression`

jlWSImport(myfile, form) imports the file myfile with format form. For example:

**Example**:
```fricas
file:=jlWSImport(jWS String("examples/customers-100.csv"), jWSString "CSV");
```

- **Signature**: `(WSString,WSString)->WSExpression`

### `jlWSPlot` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L133)\]

jlWSPlot(expr, options) is the WS plot function. Since it should be run in a Wolfram notebook or any other supported graphical interfaces, the Wolfram Jupyter "plugin" for example, it is left to the user for testing purposes. The following uses the jlWSExport function: 

**Example**:
```fricas
x:=jWSExpr(x);opt:=jWSList [x,-5,5]
```

**Example**:
```fricas
jlW SExport("sin.png", jlWSPlot(sin(x),opt))
```

- **Signature**: `(WSExpression,WSExpression)->WSExpression`

### `jlWSSeedRandom!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L13)\]

jlWSSeedRandom!(n) reseeds the random number generator with n as seed. Returns the random generator state.

- **Signature**: `(WSInteger)->WSExpression`

### `jlWSSnippet` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L111)\]

jlWSSnippet(expr, n) returns the first (or last) n snippets of the WSExpression expr. For example:

**Example**:
```fricas
j lWSSnippet(jlWSImport jWSString "http://www.fricas.org/",-2)
```

- **Signature**: `(WSExpression,WSInteger)->WSExpression`

### `urand01Complex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L22)\]

urand01Complex(rows,cols) returns a Julia WS matrix with uniformly distributed random complex elements in theunit square. Convenience function.

- **Signature**: `(PositiveInteger,PositiveInteger)->WSMatrix(WSComplex)`

urand01Complex(n) returns a Julia WS vector with uniformly distributed random complex elements in the unit square. Convenience function.

- **Signature**: `(PositiveInteger)->WSVector(WSComplex)`

### `urand01Real` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L16)\]

urand01Real(rows,cols) returns a Julia WS matrix with uniformly distributed random elements in the range 0..1. Convenience function.

- **Signature**: `(PositiveInteger,PositiveInteger)->WSMatrix(WSReal)`

urand01Real(n) returns a Julia WS vector with uniformly distributed random elements in the range 0..1. Convenience function.

- **Signature**: `(PositiveInteger)->WSVector(WSReal)`

### `vandermondeMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L42)\]

vandermondeMatrix(lvars) returns a Vandermonde matrix with nodes from lvars.

- **Signature**: `(WSList(WSExpression))->WSMatrix(WSExpression)`

vandermondeMatrix(lvars, m) returns a Vandermonde matrix with nodes from lvars and m columns.

- **Signature**: `(WSList(WSExpression),WSInteger)->WSMatrix(WSExpression)`

### `zeroMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsutils.spad#L51)\]

zeroMatrix(m, n) returns an m-by-n zero matrix.

- **Signature**: `(PositiveInteger,PositiveInteger)->WSMatrix(WSInteger)`
---
[Back to Index](../index.md)
