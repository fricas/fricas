# NMFactored

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jnemo.spad#L1909)\] &nbsp;|&nbsp; **Group**: NM — Nemo (FLINT)

## Description

Nemo factored objects.NMFactored(R: Join(NMRing,IntegralDomain)) is a domain constructor Abbreviation for NMFactored is NFR This constructor is exposed in this frame. ------------------------------------------------ Operations ------------------------------------------------

?=? : (%, %) -> Boolean                                coerce : % -> JLObjectcoerce : % -> OutputForm                               convert : % -> Stringfactor : R -> Factored(R)                              factor : R -> % factorSquareFree : R -> Factored(R)                    factorSquareFree : R -> %jlAbout : % -> Void                                    jlApply : (String, %, %, %, %, %) -> JLObjectjlApply : (String, %, %, %, %) -> JLObject             jlApply : (String, %, %, %) -> JLObjectjlApply : (String, %, %) -> JLObject                   jlApply : (String, %) -> JLObjectjlDisplay : % -> Void                                  jlDump : JLObject -> VoidjlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObjectjlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObjectjlId : % -> JLInt64                                    jlObject : () -> StringjlPropertyNames : % -> JLObject                        jlRef : % -> SExpressionjlText : (%, String) -> List(String)                   jlType : % -> Symbol jlimref : String -> %                                  jlref : String -> % latex : % -> String                                    missing? : % -> Booleanmutable? : % -> Boolean                                nothing? : % -> Booleanstring : % -> String                                   unit : % -> R ?~=? : (%, %) -> Boolean

---
[Back to Index](../index.md)
