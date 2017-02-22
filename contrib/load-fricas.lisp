#|
To use FriCAS as a Lisp library build FriCAS as usual and
keep the build tree.  Change path below to point to build
tree.  After that doing:

 (load "load-fricas.lisp")

will load and initialize FriCAS.

Note: this does not work with ECL.  Using ECL you need to
create shared library, see 'mk_shlib.lisp'.

Examples:
Using FriCAS command interpreter:

 (in-package "BOOT")
 (|parseAndInterpret| "x^2")

          2
   (2)  x
                                                    Type: Polynomial Integer
 ((Polynomial (Integer)) WRAPPED 1 x (2 0 . 1))

Directly calling math functions:

 (defvar  *mult-i-sup*
     (|getFunctionFromDomain|
           '*
           '(|SparseUnivariatePolynomial| (|Integer|))
           '((|SparseUnivariatePolynomial| (|Integer|))
               (|SparseUnivariatePolynomial| (|Integer|)))))
 *MULT-I-SUP*

 (SPADCALL '((2 . 1) (0 . 1)) '((5 . 1) (0 . 1)) *mult-i-sup*)
 ((7 . 1) (5 . 1) (2 . 1) (0 . 1))


Notes:
- all interesting functionality is in package called "BOOT".
- at mathematical level FriCAS is case-sensitive, so at Lisp level one
  has to use bars.
- the simplest interface is |parseAndInterpret| which takes a string
  as input and produces a Lisp form representing printed output.  As
  side effect |parseAndInterpret| prints the result.
- at deeper lever FriCAS functions are overloaded, so to call correct
  function one has to first use |getFunctionFromDomain| to get
  function which matches to given argument types.  Above I want to
  multiply two sparse univariate polynomials with integer coefficients.
  Since lookup may be expensive the caller is advised to cache result
  of the lookup.
- FriCAS functions use special calling convention, so one has to use
  SPADCALL macro to call them.  Actually, |getFunctionFromDomain|
  returns a pair of a function and an extra argument.
  SPADCALL takes care of decomposing the pair and appending the
  extra argument to the argument list.

Currently FriCAS sets a few system (global) variables, for example
*read-default-float-format* is set to 'double-float -- in principle
FriCAS settings may interfere with other programs.

|#
(let ((*default-pathname-defaults*
       #P"/full_path_to_FriCAS_build_directory/src/interp/"))
     (load "../lisp/fricas-package.lisp")
     (load "../lisp/fricas-config.lisp")
     (load "../lisp/fricas-lisp")
     (load "../lisp/primitives.lisp")
     (load "makeint.lisp"))
(in-package "BOOT")
(fricas-init)
