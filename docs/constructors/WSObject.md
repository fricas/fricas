# WSObject

> **Kind**: Category &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L1)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic Object category using the Wolfram Symbol Transport Protocol.

**WSObject is a category constructor**  
**Abbreviation for WSObject is WSO**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 ?=? : (%, %) -> Boolean                                coerce : % -> WSExpression
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 convert : % -> String                                  jWSInterpret : (String, String, String) -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : String -> %
 jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObject
 jlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObject
 jlDisplay : % -> Void                                  jlDump : JLObject -> Void
 jlEval : % -> %                                        jlFieldNames : % -> JLObject
 jlGetField : (%, JLSymbol) -> JLObject                 jlGetJuliaIndex : % -> String
 jlGetProperty : (%, JLSymbol) -> JLObject              jlHead : % -> WSSymbol
 jlId : % -> JLInt64                                    jlObject : () -> String
 jlPropertyNames : % -> JLObject                        jlRef : % -> SExpression
 jlSymbolic : % -> String                               jlText : (%, String) -> List(String)
 jlType : % -> Symbol                                   jlimref : String -> %
 jlref : String -> %                                    latex : % -> String
 missing? : % -> Boolean                                mutable? : % -> Boolean
 nothing? : % -> Boolean                                numeric : (%, PositiveInteger) -> WSExpression
 numeric : % -> WSExpression                            numeric? : % -> Boolean
 string : % -> String                                   toString : % -> String
 ?~=? : (%, %) -> Boolean
```

## Operations added

### `coerce` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L41)\]

coerce(expr) returns the WSExpression representation of expr.

- **Signature**: `(%)->WSExpression`

### `jWSInterpret` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L10)\]

jWSInterpret(form) interprets form.

- **Signature**: `(String)->%`

jWSInterpret(Type|Form, Expr) interprets Expr to be of type Type or Form using the Wolfram Symbolic Language.For example: 

**Example**:
```fricas
jWSInterpret("Number","3.2")@WSREAL
```

**Example**:
```fricas
jWSInterpret("Real","3.2")@WSREAL
```

Exam ple: jWSInterpret("Location","Lille")$WSEXPR See jWSExpr "$InterpreterTypes"

- **Signature**: `(String,String)->%`

jWSInterpret(form1,form2,form3) interprets forms.

- **Signature**: `(String,String,String)->%`

### `jlEval` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L23)\]

jlEval(expr) evaluates expr using the Wolfram Symbolic Transport Protocol.

- **Signature**: `(%)->%`

### `jlHead` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L34)\]

jlHead(expr) returns the Head type of expr.

- **Signature**: `(%)->WSSymbol`

### `jlSymbolic` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L25)\]

jlSymbolic(expr) returns the symbolic 'FullForm'.

- **Signature**: `(%)->String`

### `numeric` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L27)\]

numeric(expr) returns numerical expr if possible.

- **Signature**: `(%)->WSExpression`

numeric(expr, prec) returns the numerical expr with prec digits of precision if possible.

- **Signature**: `(%,PositiveInteger)->WSExpression`

### `numeric?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L32)\]

numeric?(x) checks whether or not x is or would yield a number.

- **Signature**: `(%)->Boolean`

### `string` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L38)\]

string(obj) returns the string representation of obj in the Julia WS format.

- **Signature**: `(%)->String`

### `toString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jws.spad#L36)\]

toString(obj) returns the string representation of obj.

- **Signature**: `(%)->String`
---
[Back to Index](../index.md)
