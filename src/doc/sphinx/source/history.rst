History
=======

The origin of FriCAS is in IBM laboratories. About 1965 J. Griesmer
from IBM started a project to develop a computer algebra system. In
1970 the project was named Scratchpad. In 1976 the Scratchpad team
decided to develop a new system with a novel architecture under name
Scratchpad II. Around 1982 the Spad language took shape quite similar
to current form. During 1980-s Scratchpad II grow as internal IBM
project. Scratchpad II was a big system and due to memory requirements
could run only on biggest machines.

In 1992 IBM decided to sell the system to NAG and NAG changed name to
Axiom and begun marketing it. NAG ported Axiom to run on top of
Codemist Common Lisp (CCL), which dramatically reduced memory
requirements, so that Axiom could run on PC class computers (but 16 Mb
required memory meant that it was limited to largest PC).

For various reasons Axiom did not satisfy NAG hopes and around 1998
system development stopped and in 2001 Axiom was withdrawn from the
market. Fortunately NAG decided to release Axiom to the public under
open source licence. In 2002 Tim Daly received Axiom sources from NAG
and started open-source Axiom project. Development of Axiom moved
slowely, concentrating on build system. Build system was a nontrivial
problem as during commercial times Axiom/Scratchpad II required having
running system for recompilation -- open source version dropped this
requirement.

In 2007 after serious disagreement about development strategy Waldek
Hebisch forked Axiom creating FriCAS. Shortly thereafter Gabriel Dos
Reis forked Axiom second time creating Open Axiom. Tim Daly continues
his work keeping Axiom name. FriCAS had several releases each bringing
some incremental improvement. In 2013, majority of FriCAS code is from
period before 1998. In the src/algebra subdirectory, which hosts
mathematical functionality about 25% of code was added during FriCAS
time (part of the added code was created earlier, but was not included
in Axiom).

Old Scratchpad Newsletters
--------------------------

The following Scratchpad newsletters are provided in the
`Axiom <http://axiom.axiom-developer.org>`_
`code repository <https://github.com/daly/axiom>`_.

* `June 1984
  <https://github.com/daly/axiom/raw/master/books/Newsletter.June84.pdf>`_
* `September 1985
  <https://github.com/daly/axiom/raw/master/books/Newsletter.September85.pdf>`_
* `May 1986
  <https://github.com/daly/axiom/raw/master/books/Newsletter.May86.pdf>`_
* `January 1986
  <https://github.com/daly/axiom/raw/master/books/Newsletter.January86.pdf>`_
* `November 1987
  <https://github.com/daly/axiom/raw/master/books/Newsletter.November87.pdf>`_
