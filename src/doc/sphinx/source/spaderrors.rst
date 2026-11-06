Spad Compiler Error Messages
============================

This page lists common mistakes in Spad code, the message the compiler
gives for each of them, and what to do about it.

Reading a message
-----------------

When the compilation of a function fails, the compiler prints one
message of the form::

   >> Apparent user error:
   file.spad:11: in function g: there is no operation named foo

The message gives the file, the line and the function, and then says
what went wrong. Other problems are collected as ``Semantic Errors``
or ``Warnings`` with the same kind of position.

Compiler developers can get the internal state of the compiler
instead, with ``)boot $comp_fail_dump := true``.


Syntax errors
-------------

A missing parenthesis, for example::

    f(x) ==
        y := (x + 1
        y * 2

gives::

    ******** Spad syntax error detected ********
    Expected: |)|
    The prior line was:

        6>         y := (x + 1

    The current line is:

        7>         y * 2

The error is usually at the end of the prior line or somewhere before
it, not in the current line. Look for a missing ``)`` or ``]``, or for
wrong indentation.

A function header followed by its body on the next line, without
``==``::

    pos?(x : %) : Boolean
        rep(x) > 0

gives::

    missing == after pos?(x: %)?

The parser reads the indented body as part of the type ``Boolean``.
Write ``pos?(x : %) : Boolean ==``.

A reserved word used as a name, for example ``f(mod : Integer) == mod +
1``, gives a syntax error with the note::

    mod is a reserved word; to use it as a name, write _mod

Reserved words include ``mod``, ``rem``, ``quo``, ``exquo``, ``has``,
``is``, ``case``, ``by`` and ``where``. Write ``_mod`` or choose another
name.


Names
-----

``there is no operation named foo``
   ``foo`` is neither a local function nor an operation of any domain or
   package in scope. Check the spelling, and check that the domain or
   package that defines ``foo`` is imported or used in a declaration.

``S has no operation named bar``
   An operation is called from a domain with ``$``, as in ``bar()$S`` or
   ``1$S``, but the domain (here the parameter ``S``) does not have it::

       Foo(S : with One : () -> %) : with ... == add
           foo() : S == 1$S

   Check the exports of the domain or of the category of the parameter.

``there is no operation named per; per and rep are not predefined ...``
   ``per`` and ``rep``, which convert between ``%`` and ``Rep``, are
   not built in. Define them in the capsule::

       per(r : Rep) : % == r pretend %
       rep(x : %) : Rep == x pretend Rep

   or use ``pretend`` directly.

``w is not declared``
   ``w`` is used but has no value and no declaration::

       f(x) == x + w

   Declare and assign it first, or check the spelling. The message also
   comes when a variable is assigned only inside a condition, for
   example in ``while zero?(c := first l) repeat ...``, and used
   after the loop. Declare it before the loop: ``c : Integer``.

``v is declared as Integer but has not been given a value`` (warning)
   ``v`` is declared and used, but never assigned. At runtime the use
   fails.

``there is no field kerr``
   A record has no field of that name::

       r : Record(ker : Integer, val : Integer) := [x, x]
       r.kerr

   Check the field names in the ``Record`` type.


Operations and arguments
------------------------

``cannot find an operation named + with argument types String, Integer and result type String; candidates are: ...``
   There are operations named ``+``, but none takes arguments of these
   types and gives the expected result type::

       s : String := "a"
       s + x

   The message lists the operations of that name in the types in
   scope that take as many arguments as the call; if one of them gives
   the expected result type, only those that do. The ones for which
   the most arguments fit come first, at most 5. Often an argument has
   to be converted (``concat(s, string(x))``), or the arguments have
   to be swapped. For example, in a ``Ring`` ``R`` there is ``* :
   (PositiveInteger, R) -> R`` but no ``* : (R, PositiveInteger) ->
   R``, so ``y * 2`` with ``y : R`` gives::

       cannot find an operation named * with argument types R, PositiveInteger and result type R; candidates are:
          * : (R, R) -> R from R
          * : (PositiveInteger, R) -> R from R
          * : (Integer, R) -> R from R
          * : (NonNegativeInteger, R) -> R from R

   and ``2 * y`` works. If there are more than 5 candidates, the last
   line says how many are not shown; ``)display operation +`` lists
   the operations of the whole library.

``gcd takes 1 or 2 arguments, not 3``
   The operation exists, but not with this number of arguments. Here
   ``gcd(gcd(y, x), 2)`` is meant.

``"positive" has type String, but Integer is expected``
   An expression has a different type than the one required at that
   place, here a function declared to return ``Integer``::

       f(x) ==
           x > 0 => "positive"
           x

   Return a value of the declared type, or change the declaration.

``+-> has 2 arguments, but 1 are expected``
   An anonymous function has a different number of arguments than the
   function type it must have, for example ``(x, y) +-> x + n`` where
   ``Integer -> Integer`` is expected.


Loops
-----

``n has type Integer, but for ... in needs a list``
   ``for i in n`` needs a list or a segment. For a range write
   ``for i in 1..n``.

``the end value "a" of the loop is not an integer``
   The start value, the step and the end value of ``for i in a..b by
   s`` must be integers.

``the condition n after while is not a Boolean``
   Conditions after ``while``, ``if`` and ``|`` must have type
   ``Boolean``. Write for example ``while n > 0``.

``a collection [... for ...] cannot have type Integer``
   ``[i for i in 1..n]`` builds a list (or a vector), but the place
   where it is used needs another type, for example the result of a
   function declared to return ``Integer``.


Types and categories
--------------------

``Intger is not a valid type``
   A type name is misspelled, or the domain is not known to the
   compiler. For a misspelled type in the exports, for example
   ``f : Integer -> Intger``, the message gives the line of that
   declaration.

``Foo is not a valid category``
   The category in the declaration of a constructor (``ErrM :
   Join(SetCategory, Foo) with ...`` or ``ErrN : Foo == ...``) is
   unknown. Check the spelling, or compile the file that defines
   ``Foo`` first.

``cannot compile ErrCat == ...: ErrCat is not declared``
   A constructor is defined without its declaration. A category needs
   ``ErrCat : Category == SetCategory with ...``; a domain or package
   needs a declaration with its exports, ``ErrD : with ... == add ...``.

``cannot compile Foo(n) == ...: the right-hand side is not a domain or an add form``
   The constructor is declared, but its definition is neither a domain
   nor an ``add`` form, for example ``Foo(n : Integer) : Ring == (n > 2
   => Fraction(Integer); Integer)``. A conditional choice of the
   representation works when it is followed by ``add``:
   ``(n > 2 => Fraction(Integer); Integer) add ...``.
