# jlFriCAS Documentation

**jlFriCAS** is a high-performance interface connecting [FriCAS](http://fricas.github.io) to [Julia](https://julialang.org), enabling seamless numerical computing, [Nemo](https://nemocas.github.io/Nemo.jl/stable/) algebraic power, and, optionally, integrate some [Wolfram®](https://www.wolfram.com/) capabilities. Generic Julia interfaces to [Python®](https://www.python.org) and [R](https://www.r-project.org/) via [PyCall](https://github.com/JuliaPy/PyCall.jl) and [RCall](https://github.com/JuliaInterop/RCall.jl) are provided as well.

---

## The Two Interfaces of jlFriCAS

jlFriCAS provides two complementary ways to interact with Julia:

### 1. Low-level CL-Julia Interface

This interface sends/shares "pure" Lisp objects directly between FriCAS and Julia with minimal overhead. It is the foundation on which higher-level domains are built and supports:

- **Scalars** — integers, floats, booleans.
- **Strings** — textual data evaluated inside Julia.
- **Arrays** — native Lisp arrays mapped to Julia arrays and vice versa.

All of this is handled transparently at the Lisp level by the `JLUtilityFunctions` generic package. See below for other domains and packages that use this CL-Julia interface.

### 2. High-level `JLObjectType` Interface

Defined in [jobject.spad](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jobject.spad), this interface manages Julia objects as opaque *reference handles* (`JLObject`) inside FriCAS.

- Julia objects are stored in an indexed dictionary inside Julia; FriCAS holds the integer key.
- Any Julia value — functions, DataFrames, Nemo rings, Wolfram expressions — can be manipulated uniformly from FriCAS Spad code.
- All `JLObj*`, `NM*`, and `WS*` domains inherit from `JLObjectType`.
---

## Architectural Design

The first interface is implemented using the underlying CL Foreign
Function Interface (FFI). Only SBCL and Clozure CL are supported actually.
For data, JuliaInt64 uses (signed-byte 64) for arrays or the same
implementation than SingeInteger in FriCAS for scalars.
JuliaFloat32 and JuliaFloat64 use Common LISP (CL) single-float and
double-float types. A wrapper in C interfaces the CL FFI to use directly
some specific operations in Julia.


For scalars data are passed by values, for arrays, via the CL FFI from the
underlying CL implementation, a C pointer is passed to Julia to use
libjulia available operations. The principal goal is efficiency, so
even if some special scalar functions are added to FriCAS using Julia,
the main part is operating on arrays (vectors or matrices). Usual
linear algebra operations are provided using BLAS and LAPACK.


A <code>)show JuliaF64LinearAlgebra</code> will show you some operations
and signatures for 64bit arrays. Julia uses internally the hardware
optimized OpenBlas library which historically comes from the Goto BLAS
library (autotuned at startup contrary to Atlas which used to tune
the library at build time depending on your processor). This library can
be changed at run time, for example on an Intel processor you can use
the Julia package [MKL](https://github.com/JuliaLinearAlgebra/MKL.jl)
(called Intel® oneAPI) with <code>jlUsing "MKL"</code>.

The second interface implements a simple CL class, **jlref**, where every
object used in Julia is referenced by an object of this class. The
actual data — such as arrays, for example, but practically everything in
Julia — resides in the Julia process (multithreaded).

A MCP server in Common Lisp is also added. For details on the [Model Context Protocol](https://modelcontextprotocol.io) server (stdio and socket modes) that can be used by AI models or [fricas-vscode](https://github.com/gvanuxem/fricas-vscode), see:

- [MCP Server Design](MCP_SERVER_DESIGN.md)
---

## Constructor Index

### JL — Native Julia Integration

These constructors expose Julia's built-in types, linear algebra, special functions, plotting, and general utilities directly from FriCAS.

#### Categories

| Constructor | Description |
|-------------|-------------|
| [JLArbitraryPrecision](constructors/JLArbitraryPrecision.md) | Category for Julia arbitrary-precision types |
| [JLCommutativeRing](constructors/JLCommutativeRing.md) | Category for Julia commutative rings |
| [JLMachineFloat](constructors/JLMachineFloat.md) | Category for Julia machine floating-point types |
| [JLMachineType](constructors/JLMachineType.md) | Category for Julia machine number types |
| [JLMatrixCategory](constructors/JLMatrixCategory.md) | Category for Julia matrix types |
| [JLObjectAggregate](constructors/JLObjectAggregate.md) | Category for Julia aggregate (indexable) objects |
| [JLObjectRing](constructors/JLObjectRing.md) | Category for Julia ring objects |
| [JLObjectType](constructors/JLObjectType.md) | **Root category** for all Julia objects managed via reference handles |
| [JLRing](constructors/JLRing.md) | Category for Julia ring types |
| [JLType](constructors/JLType.md) | Base category for all Julia types |
| [JLVectorCategory](constructors/JLVectorCategory.md) | Category for Julia 1D array types |

#### Domain — Symbol

| Constructor | Description |
|-------------|-------------|
| [JLSymbol](constructors/JLSymbol.md) | CL-Julia `Symbol` |

#### Domains — Floating Point

| Constructor | Description |
|-------------|-------------|
| [JLFloat](constructors/JLFloat.md) | Julia `BigFloat` (arbitrary precision) |
| [JLFloat32](constructors/JLFloat32.md) | CL-Julia `Float32` |
| [JLFloat64](constructors/JLFloat64.md) | CL-Julia `Float64` |
| [JLComplexFloat](constructors/JLComplexFloat.md) | Julia `Complex{BigFloat}` |
| [JLComplexF32](constructors/JLComplexF32.md) | CL-Julia `Complex{Float32}` |
| [JLComplexF64](constructors/JLComplexF64.md) | CL-Julia `Complex{Float64}` |

#### Domains — Integer

| Constructor | Description |
|-------------|-------------|
| [JLInt64](constructors/JLInt64.md) | CL-Julia `Int64` |

#### Domains — Vectors

| Constructor | Description |
|-------------|-------------|
| [JLVector](constructors/JLVector.md) | Generic parameterised Julia 1D array |
| [JLFloat32Vector](constructors/JLFloat32Vector.md) | CL-Julia `Vector{Float32}` |
| [JLFloat64Vector](constructors/JLFloat64Vector.md) | CL-Julia `Vector{Float64}` |
| [JLComplexF32Vector](constructors/JLComplexF32Vector.md) | CL-Julia `Vector{Complex{Float32}}` |
| [JLComplexF64Vector](constructors/JLComplexF64Vector.md) | CL-Julia `Vector{Complex{Float64}}` |
| [JLInt64Vector](constructors/JLInt64Vector.md) | CL-Julia `Vector{Int64}` |

#### Domains — Matrices

| Constructor | Description |
|-------------|-------------|
| [JLMatrix](constructors/JLMatrix.md) | Generic parameterised Julia 2D array |
| [JLFloat32Matrix](constructors/JLFloat32Matrix.md) | CL-Julia `Matrix{Float32}` |
| [JLFloat64Matrix](constructors/JLFloat64Matrix.md) | CL-Julia `Matrix{Float64}` |
| [JLComplexF32Matrix](constructors/JLComplexF32Matrix.md) | CL-Julia `Matrix{Complex{Float32}}` |
| [JLComplexF64Matrix](constructors/JLComplexF64Matrix.md) | CL-Julia `Matrix{Complex{Float64}}` |
| [JLF32SquareMatrix](constructors/JLF32SquareMatrix.md) | CL-Julia Square matrix over `Float32` |
| [JLF64SquareMatrix](constructors/JLF64SquareMatrix.md) | CL-Julia Square matrix over `Float64` |
| [JLComplexF32SquareMatrix](constructors/JLComplexF32SquareMatrix.md) | CL-Julia Square matrix over `Complex{Float32}` |
| [JLComplexF64SquareMatrix](constructors/JLComplexF64SquareMatrix.md) | CL-Julia Square matrix over `Complex{Float64}` |

#### Domains — Generic Julia Objects

| Constructor | Description |
|-------------|-------------|
| [JLObject](constructors/JLObject.md) | Generic Julia object (reference handle) |
| [JLDataFrame](constructors/JLDataFrame.md) | Julia `DataFrame` (DataFrames.jl) |
| [JLObjAnonymousFunction](constructors/JLObjAnonymousFunction.md) | Julia anonymous function |
| [JLObjBigInt](constructors/JLObjBigInt.md) | Julia `BigInt` |
| [JLObjBool](constructors/JLObjBool.md) | Julia `Bool` |
| [JLObjComplexF32](constructors/JLObjComplexF32.md) | Julia `Complex{Float32}` object |
| [JLObjComplexF64](constructors/JLObjComplexF64.md) | Julia `Complex{Float64}` object |
| [JLObjDict](constructors/JLObjDict.md) | Julia `Dict` |
| [JLObjDynamicLinker](constructors/JLObjDynamicLinker.md) | Julia dynamic linker object |
| [JLObjFloat32](constructors/JLObjFloat32.md) | Julia `Float32` object |
| [JLObjFloat64](constructors/JLObjFloat64.md) | Julia `Float64` object |
| [JLObjFunction](constructors/JLObjFunction.md) | Julia named function |
| [JLObjInt64](constructors/JLObjInt64.md) | Julia `Int64` object |
| [JLObjNamedTuple](constructors/JLObjNamedTuple.md) | Julia `NamedTuple` |
| [JLObjPair](constructors/JLObjPair.md) | Julia `Pair` |
| [JLObjPy](constructors/JLObjPy.md) | Julia Python object (PythonCall.jl) |
| [JLObjR](constructors/JLObjR.md) | Julia R object (RCall.jl) |
| [JLObjRational](constructors/JLObjRational.md) | Julia `Rational` |
| [JLObjTuple](constructors/JLObjTuple.md) | Julia `Tuple` |
| [JLObjUInt64](constructors/JLObjUInt64.md) | Julia `UInt64` object |

#### Packages

| Constructor | Description |
|-------------|-------------|
| [JLUtilityFunctions](constructors/JLUtilityFunctions.md) | Core Julia interface: `jlEvalString`, `jlUsing`, `jlInitialize`, … |
| [JLStringUtilities](constructors/JLStringUtilities.md) | Julia string and regex helpers |
| [JLDrawFunctions](constructors/JLDrawFunctions.md) | 2D/3D plotting via Julia |
| [JLPlotFunctions](constructors/JLPlotFunctions.md) | Additional plot utilities |
| [JLF32ArrayFunctions](constructors/JLF32ArrayFunctions.md) | `Float32` array operations |
| [JLF64ArrayFunctions](constructors/JLF64ArrayFunctions.md) | `Float64` array operations |
| [JLF32LinearAlgebra](constructors/JLF32LinearAlgebra.md) | `Float32` linear algebra |
| [JLF64LinearAlgebra](constructors/JLF64LinearAlgebra.md) | `Float64` linear algebra |
| [JLCF32LinearAlgebra](constructors/JLCF32LinearAlgebra.md) | `Complex{Float32}` linear algebra |
| [JLCF64LinearAlgebra](constructors/JLCF64LinearAlgebra.md) | `Complex{Float64}` linear algebra |
| [JLF64MatrixTranscendentalFunctions](constructors/JLF64MatrixTranscendentalFunctions.md) | Matrix transcendental functions (`Float64`) |
| [JLComplexF64MatrixTranscendentalFunctions](constructors/JLComplexF64MatrixTranscendentalFunctions.md) | Matrix transcendental functions (`Complex{Float64}`) |
| [JLFloat32SpecialFunctions](constructors/JLFloat32SpecialFunctions.md) | Special functions for `Float32` |
| [JLFloat32SpecialFunctions2](constructors/JLFloat32SpecialFunctions2.md) | Extended special functions for `Float32` |
| [JLFloat64SpecialFunctions](constructors/JLFloat64SpecialFunctions.md) | Special functions for `Float64` |
| [JLFloat64SpecialFunctions2](constructors/JLFloat64SpecialFunctions2.md) | Extended special functions for `Float64` |
| [JLFloatSpecialFunctions](constructors/JLFloatSpecialFunctions.md) | Special functions for `BigFloat` |
| [JLFloatSpecialFunctions2](constructors/JLFloatSpecialFunctions2.md) | Extended special functions for `BigFloat` |
| [JLComplexFloatSpecialFunctions](constructors/JLComplexFloatSpecialFunctions.md) | Special functions for complex `BigFloat` |
| [JLFloat32VectorFunctions2](constructors/JLFloat32VectorFunctions2.md) | Vector functions for `Float32` |
| [JLFloat64VectorFunctions2](constructors/JLFloat64VectorFunctions2.md) | Vector functions for `Float64` |
| [JLVectorFunctions2](constructors/JLVectorFunctions2.md) | Generic vector functions |
---

### NM — Nemo / FLINT Integration

These domains wrap the [Nemo.jl](https://nemocas.github.io/Nemo.jl/stable/) library, giving access to FLINT-powered exact arithmetic: integers, rationals, finite fields, number fields, p-adic numbers, and polynomial rings.

#### Categories

| Constructor | Description |
|-------------|-------------|
| [NMCommutativeRing](constructors/NMCommutativeRing.md) | Category for Nemo commutative rings |
| [NMField](constructors/NMField.md) | Category for Nemo fields |
| [NMPadicNumberCategory](constructors/NMPadicNumberCategory.md) | Category for Nemo p-adic numbers |
| [NMRing](constructors/NMRing.md) | Base category for all Nemo rings |
| [NMType](constructors/NMType.md) | Base category for all Nemo types |

#### Domains — Integers and Rationals

| Constructor | Description |
|-------------|-------------|
| [NMInteger](constructors/NMInteger.md) | Nemo `ZZRingElem` (FLINT big integers) |
| [NMFraction](constructors/NMFraction.md) | Nemo fraction field |
| [NMIntegerMod](constructors/NMIntegerMod.md) | Integers modulo *n* |

#### Domains — Fields

| Constructor | Description |
|-------------|-------------|
| [NMAcbField](constructors/NMAcbField.md) | Arb complex ball field |
| [NMArbField](constructors/NMArbField.md) | Arb real ball field |
| [NMAlgebraicNumber](constructors/NMAlgebraicNumber.md) | Algebraic numbers (`QQBar`) |
| [NMComplexBall](constructors/NMComplexBall.md) | Complex ball arithmetic |
| [NMComplexField](constructors/NMComplexField.md) | Complex field |
| [NMExactCalciumField](constructors/NMExactCalciumField.md) | Exact real/complex Calcium field |
| [NMFiniteField](constructors/NMFiniteField.md) | Finite field GF(p^n) |
| [NMPrimeField](constructors/NMPrimeField.md) | Prime field GF(p) |
| [NMRealBall](constructors/NMRealBall.md) | Real ball arithmetic |
| [NMRealField](constructors/NMRealField.md) | Real field |

#### Domains — p-adic and q-adic Numbers

| Constructor | Description |
|-------------|-------------|
| [NMExtendedPadic](constructors/NMExtendedPadic.md) | p-adic completion of the integers |
| [NMPadic](constructors/NMPadic.md) | p-adic numbers (fixed precision) |
| [NMExtendedQadic](constructors/NMExtendedQadic.md) | q-adic completion field |
| [NMQadic](constructors/NMQadic.md) | q-adic numbers (fixed precision) |
| [NMExtendedQadicField](constructors/NMExtendedQadicField.md) | q-adic completion field using polynomial |

#### Domains — Polynomials and Series

| Constructor | Description |
|-------------|-------------|
| [NMPolynomial](constructors/NMPolynomial.md) | Generic Nemo polynomial (universal polynomial ring) |
| [NMUnivariatePolynomial](constructors/NMUnivariatePolynomial.md) | Univariate polynomial over a Nemo ring |
| [NMUnivariateLaurentPolynomial](constructors/NMUnivariateLaurentPolynomial.md) | Univariate Laurent polynomial |
| [NMMultivariatePolynomial](constructors/NMMultivariatePolynomial.md) | Multivariate polynomial over a Nemo ring |
| [NMMultivariateLaurentPolynomial](constructors/NMMultivariateLaurentPolynomial.md) | Multivariate Laurent polynomial |
| [NMUnivariatePowerSeries](constructors/NMUnivariatePowerSeries.md) | Univariate power series |
| [NMUnivariateLaurentSeries](constructors/NMUnivariateLaurentSeries.md) | Univariate Laurent series |
| [NMUnivariatePuiseuxSeries](constructors/NMUnivariatePuiseuxSeries.md) | Univariate Puiseux series |

#### Domains — Other

| Constructor | Description |
|-------------|-------------|
| [NMFactored](constructors/NMFactored.md) | Factored elements (Nemo factorisation) |
---

### WS — Wolfram / MathLink Integration

These domains provide access to the [Wolfram Language](https://www.wolfram.com/language/) via the [MathLink.jl](https://github.com/JuliaInterop/MathLink.jl) Julia package. A licensed Wolfram Engine or Mathematica installation is required.

#### Categories

| Constructor | Description |
|-------------|-------------|
| [WSAggregate](constructors/WSAggregate.md) | Category for Wolfram aggregate (list-like) types |
| [WSNumber](constructors/WSNumber.md) | Category for Wolfram numeric types |
| [WSObject](constructors/WSObject.md) | **Root category** for all Wolfram objects |
| [WSRing](constructors/WSRing.md) | Category for Wolfram ring types |

#### Domains

| Constructor | Description |
|-------------|-------------|
| [WSExpression](constructors/WSExpression.md) | Generic Wolfram symbolic expression |
| [WSSymbol](constructors/WSSymbol.md) | Wolfram `Symbol` |
| [WSString](constructors/WSString.md) | Wolfram `String` |
| [WSInteger](constructors/WSInteger.md) | Wolfram `Integer` |
| [WSRational](constructors/WSRational.md) | Wolfram `Rational` |
| [WSReal](constructors/WSReal.md) | Wolfram machine-precision `Real` |
| [WSAPReal](constructors/WSAPReal.md) | Wolfram arbitrary-precision `Real` |
| [WSComplex](constructors/WSComplex.md) | Wolfram machine-precision `Complex` |
| [WSAPComplex](constructors/WSAPComplex.md) | Wolfram arbitrary-precision `Complex` |
| [WSGaussianInteger](constructors/WSGaussianInteger.md) | Wolfram Gaussian integer |
| [WSList](constructors/WSList.md) | Wolfram `List` |
| [WSVector](constructors/WSVector.md) | Wolfram 1D array |
| [WSMatrix](constructors/WSMatrix.md) | Wolfram 2D array |

#### Packages

| Constructor | Description |
|-------------|-------------|
| [WSNumericalSpecialFunctions](constructors/WSNumericalSpecialFunctions.md) | Numerical special functions via Wolfram |
| [WSUtilityFunctions](constructors/WSUtilityFunctions.md) | Wolfram utility helpers |

---

> **Trademark notice**  
> Wolfram is a registered trademark of Wolfram Research, Inc.  
> Python is a registered trademark of the Python Software Foundation.  
> Intel is a registered trademark of Intel Corporation.  