Features
========

The capabilities of |FriCAS| cover areas such as

- linear, polynomial and differential system solving
- integration (most complete implementation of the
  `Risch algorithm <https://en.wikipedia.org/wiki/Risch_algorithm>`_)
- vector, matrix, and tensor calculus, eigenvalus, eigenvectors,
- symbolic expression manipulation,
- non-commutative polynomials
- limit calculation,
- arithmetic with Taylor, Laurent, and Puiseux series,
- combinatorics, symmetric polynomials, special functions, number
  theory,
- arbitrary precision numerical computations,
- modular arithmetic,
- continued fractions, partial fraction decomposition,
- integer factorization, polynomial factorization,
- polynomial ideals, Gröbner bases, Gröbner factorization,
  triangular sets, regular chains,
- guessing of formulas for sequences,
- computation with cardinal numbers.

A distinguished feature of |FriCAS| is its type system. It
allows to create and compute with domains such as matrices of
polynomials over finite fields or polynomials having square matrices
with rational entries as their coefficient ring.

FriCAS provides structures such as

- complex numbers, quaternions, octonions,
- groups, rings, fields, algebraic extension rings,
- modules, (graded) algebras, finite fields,
- non-commutative and non-associative algebras,
- Clifford algebras, exterior algebras,

The type system of |FriCAS| is based on its statically typed
programming lanuguage (SPAD_).

|FriCAS| can be extend by programs written in SPAD_ that are compiled
to machine code in the same way as the algebra library that comes with
|FriCAS|.

In a |FriCAS| session the type system is somewhat relaxed.
Although underneath everything is computed with a distingused type,
the interpreter tries to figure out in many cases what the best way is
to convert the user input into an appropriate type. Should the
interpreter fail to guess correctly, users can always specify the
types that they want.
