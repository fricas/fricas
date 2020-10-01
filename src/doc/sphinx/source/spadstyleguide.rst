SPAD Style Guide
================

Maximum line length
-------------------

No firm rule, but use about 70 characters unless that causes problems.
Try to avoid lines longer than 78 characters.


File splitting during compilation
---------------------------------

There is a script :viewsource:`src/scripts/unpack_file` that splits
``.spad`` source files at ``)abbrev`` boundaries. As a result,
everything before the first ``)abbrev`` will be discarded.

In particular, it means that there cannot be a global macro definition
for the whole ``.spad`` file.


Literate documentation
----------------------

SPAD_ files can contain LaTeX parts.
Such documentation must be included into blocks of the form::

  )if LiterateDoc
  ...
  )endif

These parts must form a proper LaTeX document. The code parts are
included and typeset via the
`listings package <https://www.ctan.org/pkg/listings/>`_.

To support a weak form of literate programming, the two files

* :viewsource:`src/doc/literatedoc.sty`
* :viewsource:`src/doc/literatedoc.awk`

are provided.

The file :viewcode:`fmt.spad` can be taken as an example.

::

  cd fricas
  awk -f src/doc/literatedoc.awk src/algebra/fmt.spad > fmt.tex
  TEXINPUTS=:src/doc pdflatex fmt.tex


Whitespace
----------

Do not use tabs, but rather explicit spaces.


Block indentation
`````````````````

4 spaces (no tabs)


Whitespaces around punctuation
``````````````````````````````

- ":" space before and after

- "," space after (but not before)

- ";" space after (but not before)

- '+' spaces optional for most arithmetic infix operations

- "-" no space after for unary operations


Continuation lines
------------------

Use explicit ``_`` (underscore) even when not required


Function signature
------------------

Use explicit types, that is::

  func(a : Integer) : Integer ==

rather than::

  func(a) ==


Empty list
----------

use []$T rather than empty()$T

=> (early exit)
---------------

The ternary operator ``cond ? a : b`` known from the C programming
language can be encoded in SPAD as follows.

Either::

  e := (v > 0 => 1; -1)

or::

  e := if v > 0 then 1 else -1


Dangling ``else``
-----------------

If there is space then the best option is all on one line

::

  if .... then .... else ....

Otherwise it should be like this

::

      if ... then
        ...
      else
        ...



elt vs. qelt
------------

We encourage better error messages over speed, i.e., use ``elt``.
``qelt`` should only be used in cases where it is clear from the
context that no index error can happen.

_+ vs. "+"
----------

Prefer the escaped version of an operator instead of letting it look
like a string, i.e. use

::

  _+(a : %, b : %) : Boolean ==

instead of

::

  "+"(a : %, b : %) : Boolean ==


Boolean valued functions
------------------------

Functions that return boolean values have names that end in ``?``.
Define::

  positive? : Integer -> Boolean

instead of::

  isPositive : Integer -> Boolean


Destructive operations
----------------------

Identifiers of functions that modify their arguments should be ended
with an exclamation mark (``!``) to remind other programmers that they
should be very careful in using such functions.

Compare ``reverse`` with ``reverse!``.


Non-public constructors
-----------------------

Constructors starting with "Inner" are meant for library developers
but not for end-users.


Calling unary functions
-----------------------

Use

::

  foo arg

instead of

::

  foo(arg)

for unary functions if the argument is simple.
