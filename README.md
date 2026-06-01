# jlFriCAS

[![FriCAS CI on x64|arm64 Linux (with Julia support - SBCL based)](https://github.com/gvanuxem/jlfricas/actions/workflows/linuxJulia_sbcl.yml/badge.svg)](https://github.com/gvanuxem/jlfricas/actions/workflows/linuxJulia_sbcl.yml)\
[![FriCAS CI on x64 Windows (with Julia support - SBCL based)](https://github.com/gvanuxem/jlfricas/actions/workflows/windowsJulia_sbcl.yml/badge.svg)](https://github.com/gvanuxem/jlfricas/actions/workflows/windowsJulia_sbcl.yml)\
[![FriCAS CI on arm64|intel macOS (with Julia support - SBCL based)](https://github.com/gvanuxem/jlfricas/actions/workflows/macOSJulia_sbcl.yml/badge.svg)](https://github.com/gvanuxem/jlfricas/actions/workflows/macOS_Julia_sbcl.yml)

⚠️ WARNING: This extension to FriCAS is under active development! ⚠️

[FriCAS](https://fricas.github.io) is a general purpose computer algebra
system (CAS).

In this work-in-progress repository, a C wrapper using libjulia is embedded in [FriCAS](https://fricas.github.io/) to call [Julia](https://julialang.org) optimized numerical routines such as **BLAS** and **LAPACK**, and more generally to manipulate Julia objects from within FriCAS.

The build process supports Clozure CL (x86-64) and SBCL (x86-64 and arm64). For SBCL, only Julia 1.10.0, Julia 1.11.* and higher are supported, see [Caveats](#caveat-sbcl). It must not be considered production-ready. An overview of what is added to FriCAS is available [here](https://gvanuxem.github.io/jlfricas/). [Binary snapshots](https://github.com/gvanuxem/jlfricas/releases) are available.

A [MCP server](https://github.com/gvanuxem/jlfricas/blob/master/docs/MCP_SERVER_DESIGN.md) is also available in jlFriCAS on Linux, supporting both SBCL and Clozure CL. [Here](https://github.com/gvanuxem/jlfricas/blob/master/examples/Solving_Polynomial.md) is an example of a session. At configure time use the option <code>--enable-mcp</code>. The Yason Common Lisp library is required. For Clozure CL, install it with quicklisp for example. With SBCL on Debian based systems, `sudo apt install cl-yason` should do the trick, it will be used automatically through ASDF.

The MCP server is also used by [fricas-vscode](https://github.com/gvanuxem/fricas-vscode) (the Julia interface is not necessary nor the use of a LLM).

A full source of information can also be found in [HTML format](https://gvanuxem.github.io/jlfricas.documentation/).
Take into account that this is absolutely not the official documentation even though it is highly based on the official FriCAS web site which can be built from the FriCAS source code (thanks to Ralf Hemmecke and Kurt Pagani for their amazing work).


![Capture1](https://github.com/user-attachments/assets/f5a6d7ec-0dea-482d-805a-0074cea8be72)


Julia Plots and HyperDoc:

Beware, actually, on WSL you may need to export:
``
export QT_WAYLAND_SHELL_INTEGRATION=xdg-shell
``
in your shell first.

![Capture2](https://github.com/user-attachments/assets/97f35ad1-a6bc-41f9-986f-47e8d3281ad6)

Julia Plots on Windows/cmd.exe:

![Capture3](https://github.com/user-attachments/assets/4c6bec2c-6344-470c-b467-d3571794de51)


Linear Algebra:
![Capture4](https://github.com/user-attachments/assets/21ca03f1-0cff-407f-b8f6-339e38abe4ed)



## Building and Installing

For general installation instructions see INSTALL. For general documentation
consult <https://fricas.github.io>.

To build FriCAS with Julia support, the <code>julia</code> executable needs to be available in your PATH, and a simple <code>./configure --enable-julia && make && sudo make install</code> should do the trick. We require Julia 1.10 or higher. Please see https://julialang.org/downloads/ for instructions on how to obtain Julia for your system. The required Julia packages are (see the Julia package manager or if you install them later using `jlAddPackage` restart jlFriCAS):
 - [Nemo](https://nemocas.github.io/Nemo.jl/stable/) (must be installed)
 - [Suppressor](https://github.com/JuliaIO/Suppressor.jl) (must be installed)
 - [SpecialFunctions](https://specialfunctions.juliamath.org/stable/) 
 - [StyledStrings](https://julialang.github.io/StyledStrings.jl/dev/) (if it is not already installed in Julia by default)

 And optionnally:
  - [MathLink](https://github.com/JuliaInterop/MathLink.jl) (use the option `--enable-mathlink` at configure time)
  - [Plots](https://docs.juliaplots.org/stable/)
  - [UnicodePlots](https://juliaplots.org/UnicodePlots.jl/stable/)
  - [Latexify](https://github.com/korsbo/Latexify.jl)
  - [LaTeXStrings](https://github.com/JuliaStrings/LaTeXStrings.jl)
  - [DataFrames](https://dataframes.juliadata.org/stable/) and [Statistics](https://juliastats.org/Statistics.jl/dev/) (needed for tests)
  - [PythonCall](https://juliapy.github.io/PythonCall.jl/stable/) (and CondaPkg if you want to install some Python® packages, use Julia for this)
  - [RCall](https://juliainterop.github.io/RCall.jl/stable/) (install packages in Julia, example R"install.packages('boot')")

As of now with Clozure CL [queues](https://github.com/oconnore/queues) is required. Use installed [quicklisp](https://www.quicklisp.org/beta/) with `queues` installed and at configure time use the `--with-quicklisp` option, see the `quicklisp` documentation for how to install it. Another possibility, easier, is to use [roswell](https://roswell.github.io/) with added `ccl-bin` and `queues`.

If you want to visualize your data using Julia, small support is provided using `Plots` and eventually `LaTeXStrings` Julia packages.

If you want to use [jFriCAS](https://jfricas.readthedocs.io/en/latest/) i.e. Jupyter support for FriCAS built with SBCL, make sure [hunchentoot](https://edicl.github.io/hunchentoot/) is installed. On a Debian like system you can add `hunchentoot` with <code>sudo apt install cl-hunchentoot</code> and issue, for example, <code>./configure --enable-gmp --enable-julia --enable-hunchentoot</code>.

If you want to build and install the HTML documentation,
you need to install [Sphinx](https://www.sphinx-doc.org/en/master/). On a Debian like system, to add it, issue in a
terminal <code>sudo apt install python3-sphinx</code>.
After building jlFriCAS, and before the installation, issue in your terminal <code>make htmldoc</code>.

## Description

The basic goal of [FriCAS](https://fricas.github.io) is to create a free
advanced world-class CAS. In 2007 [FriCAS](https://fricas.github.io)
forked from [Axiom](http://axiom-developer.org). Currently the
[FriCAS](https://fricas.github.io) algebra library is one of the largest
and most advanced free general purpose computer algebra systems \-- this
gives a good foundation to build on. Additionally, the
[FriCAS](https://fricas.github.io) algebra library is written in a high
level strongly typed language (Spad), which allows natural expression of
mathematical algorithms. This makes [FriCAS](https://fricas.github.io)
easier to understand and extend.

[FriCAS](https://fricas.github.io) uses lightweight development
methodology. Compared to [Axiom](http://axiom-developer.org),
[FriCAS](https://fricas.github.io) is significantly restructured \-- it
is more portable and fixed several defects.
[FriCAS](https://fricas.github.io) removed rather large unused parts
(without removing functionality).

## Goals

Current development goals:

-   continue structural improvements
-   add new mathematical algorithms
-   develop better user interface
-   develop improved Spad compiler
-   make it easier for external programs to interface with FriCAS
-   support for using external mathematical routines from Spad

## Caveat: SBCL

Julia support for jlFriCAS built with SBCL is/was erratic, depending on the Julia version used and the loaded libraries used by Julia. The 1.10.0 version seems to have solved some issues related to memory management interactions with SBCL, but with Julia 1.10.1 and 1.10.2 some problems occur again. Note that with Julia 1.11.* and later, FriCAS seems to work fine again. More work needs to be done in this regard. So, if you use SBCL to build FriCAS, imperatively use a version of Julia that is known to be compatible. Additionnaly, if you use WS domains/packages through MathLink,
SBCL is known to crash if you start HyperDoc after loading WS domains/packages. If you need to use
HyperDoc, start it first with the system command `)hd`.
