# JLStringUtilities

> **Kind**: Package &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L164)\] &nbsp;|&nbsp; **Group**: JL — Native Julia

## Description

This package provides Julia facilities to work with strings.

**JLStringUtilities is a package constructor**  
**Abbreviation for JLStringUtilities is JSTRU**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 jlLatex : String -> String                             jlMessageStyled : String -> Void
 jlMessageStyled : (String, JLSymbol) -> Void           jlRegex : String -> String
 jlRegexMatch : (String, String) -> String              jlRegexMatch : (String, String) -> List(String)
 jlRegexMatch? : (String, String) -> Boolean            jlRegexReplace : (String, String, String) -> String
 jlSplit : (String, String) -> List(String)             jlString : String -> String
 jlStyled : String -> String                            jlSubstitute : String -> String
 jlToLatex : String -> String
 jlRegexMatch : (String, String, PositiveInteger) -> String
 jlRegexMatch : (String, String, PositiveInteger) -> List(String)
 jlRegexMatch : (String, String) -> StringTable(String)
 jlRegexMatch : (String, String, PositiveInteger) -> StringTable(String)
 jlSplit : (String, String, NonNegativeInteger) -> List(String)
```

## Operations added

### `jlLatex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L205)\]

jlLatex(str) returns the String corresponding to a Julia LaTeX expression, that is, "L"str"" if your Julia installation supports it (LaTeXStrings package). Convenience function.

- **Signature**: `(String)->String`

### `jlMessageStyled` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L176)\]

jlMessageStyled(message) prints the message with formatted style within braces (I am red:here). For example:

**Example**:
```fricas
jlMessageStyled("red:■ green:■ yellow:■ blue:■ magenta:■ cyan:■")
```

- **Signature**: `(String)->Void`

jlMessageStyled(message, color) prints the message with formatted `color` (english color). For example:Examp le: jlMessageStyled("I AM HERE!!!", "red")

- **Signature**: `(String,JLSymbol)->Void`

### `jlRegex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L199)\]

jlRegex(str) returns the String corresponding to a Julia regular expression, that is, "r_"str_"". Conveniencefunction.

- **Signature**: `(String)->String`

### `jlRegexMatch` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L217)\]

jlRegexMatch(s, regex) returns the matched regular expressions in s.

- **Signature**: `(String,String)->List(String)`
- **Signature**: `(String,String)->StringTable(String)`

jlRegexMatch(s, regex, i) returns the matched regular expressions in s, starting at index i.

- **Signature**: `(String,String,PositiveInteger)->List(String)`
- **Signature**: `(String,String,PositiveInteger)->StringTable(String)`

jlRegexMatch(s, regex) returns the matched regular expression in s.

- **Signature**: `(String,String)->String`

jlRegexMatch(s, regex, i) returns the matched regular expression in s, starting at index i.

- **Signature**: `(String,String,PositiveInteger)->String`

### `jlRegexMatch?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L215)\]

jlRegexMatch?(s, regex) checks if the regular expression matches in s.

- **Signature**: `(String,String)->Boolean`

### `jlRegexReplace` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L232)\]

jlRegexReplace(str, regex, subst) will replace captured regular expressions in str with subst. More information in String documentation of Julia. For example: jlRegexReplace("hey", jlRegex("$"), jlSubstitute(" you!"))

- **Signature**: `(String,String,String)->String`

### `jlSplit` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L191)\]

jlSplit(str, delims) splits str with delimiter(s) delims (a string or a regular expression). Equivalent to jlSplit(str, delims, 0).

- **Signature**: `(String,String)->List(String)`

jlSplit(str, delims, n) splits str with delimiter(s) delims (a string or a regular expression). Returns a list of size n. Parses entirely str if n = 0.

- **Signature**: `(String,String,NonNegativeInteger)->List(String)`

### `jlString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L213)\]

jlString(str) constructs an escaped string usable by the Julia parser.

- **Signature**: `(String)->String`

### `jlStyled` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L209)\]

jlStyled(str) returns the String corresponding to a Julia styled expression, that is, "styled_"str_"" if yourJulia supports it. (StyledStrings is an official package). Convenience function.

- **Signature**: `(String)->String`

### `jlSubstitute` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L202)\]

jlSubstitute(str) returns the String corresponding to a Julia substitute expression, that is, "s_"str_"". Convenience function.

- **Signature**: `(String)->String`

### `jlToLatex` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jutils.spad#L186)\]

jlToLatex(str) converts the string str to LaTeX for supported expressions. The Julia package Latexify needs to be installed. jlToLatex("sin(sqrt(2))") => "$( 2 )$"

- **Signature**: `(String)->String`
---
[Back to Index](../index.md)
