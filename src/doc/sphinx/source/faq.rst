FriCAS FAQ
==========

.. contents::
  :local:
  :backlinks: none
  :depth: 2



General Questions
-----------------

What is FriCAS?
^^^^^^^^^^^^^^^

FriCAS is an advanced computer algebra system. Its capabilities range
from calculus (integration and differentiation) to abstract algebra.
It can plot functions and has an integrated help system.


What is the relation between FriCAS and Axiom?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

FriCAS forked from Axiom in 2007. FriCAS seeks different development
methodology and after fork removed several unused parts (without
removing functionality). FriCAS fixed a lot of bugs and added new
functionality. As of April 2013, in the src/algebra subdirectory,
which hosts mathematical functionality about 25% of code was added
after the fork.


What does the FriCAS name mean?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The prefix *Fri* is a deliberate misspelling of Free -- FriCAS sounds
like Free CAS.

There is also a second meaning: the Polish word "frykas" (which sounds
similar to FriCAS) denotes generally tasty food (dainty); the French
word "fricassee" and the German "Frikassee" name a particular food.


Where can I find online information about FriCAS?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The main entrance point to documentation about FriCAS is
http://fricas.github.io.

The mailing list
https://groups.google.com/group/fricas-devel?hl=en
is intended both for users and developers.
Please sign up before posting a message.


Where can I find documentation?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The main source of information about FriCAS is the "Axiom book" by
Jenks and Sutor in its adaptation for FriCAS. FriCAS continues to
update that contents of that book along with its development and calls
it |PACKAGE_BOOK|. Its content is shown by the integrated HyperDoc
help system. You can also access the book in .pdf form at
http://fricas.github.io/book.pdf.
An older version is available in in .xhtml form at
http://wiki.fricas.org/JenksSutorInXhtml.


Where should I report bugs?
^^^^^^^^^^^^^^^^^^^^^^^^^^^

FriCAS moved to GitHub in March 2020. Issues should be reported to
https://github.com/fricas/fricas/issues.

The older bugtrackers at
`SourceForge <http://sourceforge.net/tracker/?atid=972421&group_id=200168>`_
and
the `FriCAS wiki <`http://wiki.fricas.org/IssueTracker>`_
should not be used anymore.


How one can use FriCAS in a pipe or in batch mode?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently trying to run plain ``fricas`` command in a pipe hangs
(this is a bug, but fix requires substantial change).
Instead, one needs to pass ``-nosman`` option.

There several ways to let FriCAS process commands from a file.
::

   fricas -nosman < tst.input > tst.output
   cat tst.input | fricas -nosman > tst.output
   echo ')read tst.input' | fricas -nosman > tst.output

Whereas the first two versions do not show the input in ``tst.ouput``,
the last command does. However, in the latter case it is hard to
distinguish input from output.

Note: Similarly to the commandline, pipe mode requires each function
to be defined in a single line.


How to find where an exported function is implemented?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In Hyperdoc 'Browse' window enter the constructor name or its
abbreviation and click on 'Constructors'. In the resulting form
specify all parameters. Then click on 'Operations' and finally on
'Implementations'. If you are interested in a single operation, after
clicking on 'Operations' click on an operation name and then click on
'Implementations'.


Clef does not work.  Is there an alternative?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have GNU Readline and ``rlwrap`` installed, the ``-rl`` option
will use GNU Readline editing, tab completion, and history, i.e. call
FriCAS like this::

  fricas -rl


Default HyperDoc window is too small.  How to enlarge it?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Put the lines like below in file called ``~/.Xresources``.
::

    FriCAS.hyperdoc.FormGeometry: =1200x700+40+40
    FriCAS.hyperdoc.Geometry: =1200x700+40+40

They should take effect the next time you log in. If not
you may have to manually run
::

   xrdb -m ~/.Xresources

If you work with a 4K-display and want to make the font bigger, you
can add the following to your ``.Xresources`` file.
::

   FriCAS.hyperdoc.Geometry: =1500x800+0+0
   FriCAS.hyperdoc.RmFont: -adobe-courier-bold-r-*-*-34-*-*-*-*-*-*-*
   FriCAS.hyperdoc.RmColor: black
   FriCAS.hyperdoc.ActiveFont: -adobe-courier-bold-r-*-*-34-*-*-*-*-*-*-*
   FriCAS.hyperdoc.ActiveColor: blue
   FriCAS.hyperdoc.FriCASFont: -adobe-courier-bold-r-*-*-34-*-*-*-*-*-*-*
   FriCAS.hyperdoc.FriCASColor: #008000
   FriCAS.hyperdoc.BoldFont: -adobe-courier-bold-r-*-*-34-*-*-*-*-*-*-*
   FriCAS.hyperdoc.BoldColor: red
   FriCAS.hyperdoc.TtFont: -adobe-courier-bold-r-*-*-34-*-*-*-*-*-*-*
   FriCAS.hyperdoc.TtColor: black
   FriCAS.hyperdoc.EmphasizeFont: -adobe-courier-bold-r-*-*-34-*-*-*-*-*-*-*
   FriCAS.hyperdoc.EmphasizeColor: #800080
   FriCAS.hyperdoc.InputBackground: #FFFF80
   FriCAS.hyperdoc.InputForeground: black
   FriCAS.hyperdoc.BorderColor: black
   FriCAS.hyperdoc.Background: white

Of course, you might want to choose other fonts that are available on
your computer. Use ``xfontsel`` to find out.


Why does FriCAS behave differently after loading new code?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When FriCAS loads new code, current variables become invalid.
This happens when you use
::

   )lib SOMELIB

or automatically when you compile a file via
::

   )compile foo.spad

You have to load and compile code before you start your computation.



Old Axiom FAQ
-------------

This list has been slightly adapted to match the new name FriCAS.


The fricas command fails.
^^^^^^^^^^^^^^^^^^^^^^^^^

This is likely one of two problems.

1. FriCAS uses clef as its command line editor. This has functionality
similar to GNU Readline but was written independently. The fricas
command uses
::

   clef -e $FRICAS/bin/FRICASsys

Clef attempts to create new terminals and this might fail.
The first thing to check is the permission bits on /dev/pty.

2. It is possible to run the fricas image, called ``FRICASsys``,
directly. Just type FRICASsys. It won't have command recall or command
line editing but everything else is there.
A direct call to ``FRICASsys`` (with the ``FRICAS`` environment
variable set appropriately) can be done by calling
::

   fricas -nosman


How can I create and access Lisp functions from FriCAS?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

SExpression is the domain that handles raw lisp objects.
It is possible to create SExpression elements directly.
::

   m:=[1::SEX, 2::SEX]
     [1,2]
                             Type: List SExpression
   n:=m::SEX
     (1 2)
                                Type: SExpression
   car(n)
     1
                                Type: SExpression

You can access lisp functions directly as in
::

   GENSYM()$Lisp

Lisp is the domain, known to the interpreter and compiler, that
contains lisp functions and symbols.

Notice that FriCAS is case-sensitive and that generally lisp symbols
are upper case.

You can also create and call lisp functions.
::

   )lisp (defun foo () (print "it works"))
       Value = FOO

   FOO()$Lisp
    "it works"

       it works
                                Type: SExpression

While accessing and writing functions in Lisp is possible it is
discouraged. FriCAS contains a programming language that should be
able to achieve almost everything you need.

**Use Lisp only when you cannot achieve your goal otherwise!**


How can I see what the interpreter is trying to do?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The command
::

   )set message bottomup on

will tell you the signatures that the interpreter is trying to use.

Another method is to do
::

   )boot $monitorNewWorld := t

and you can view database calls with
::

   )lisp (setq *miss* t)


How can I record console output?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use ``)spool``.
::

   )spool filename

starts sending output to the file called ``filename`` and
::

   )spool )off

stops sending output to the file.


Graphics doesn't work or sman fails to start ?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First try running ``sman`` as
::

   sman -debug -noclef -noht

If graphics still doesn't work or sman fails to start then look at the
error messages.


What is the purpose of the domain HACKPI?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

HACKPI is a hack provided for the benefit of the FriCAS interpreter.
As a mathematical type, it is the simple transcendental extension
:math:`Q(\pi)` of the rational numbers. This type allows interactive users to
use the name ``%pi`` without a type both where a numerical value is
expected as in
::

   draw(sin x,x=-%pi..%pi)

or when the exact symbolic value is meant. The interpreter defaults a
typeless ``%pi`` to HACKPI and then uses the various conversions to cast
it further as required by the context.

One could argue that it is unfair to single ``%pi`` out from other
constants, but it occurs frequently enough in school examples
(specially for graphs) so it was worth a special hack. In a
non-interactive environment (library), HACKPI would not exist.


Why do .fricas.input defined functions fail in fricas?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You write this in your ``.fricas.input`` file:
::

   mrd(x:Integer,v:Integer):Integer == x+y

You can't see this function even though it appears to be defined. That's
because FriCAS is working in a new frame.

When you start ``FRICASsys`` you are running the interpreter talking
directly to the terminal. So the ``.input`` file is actually talking
to a frame at the top level. Your function is defined.

The ``.fricas.input`` file is read in a "frame" called "initial".
``FRICASsys`` only uses the "initial" frame (although you can define and
use new ones). A frame contains its own variables and function definitions.

The ``fricas`` command does several things that ``FRICASsys`` does
not. In particular the ``fricas`` shell script starts up the ``sman``
process which starts ``FRICASsys`` (which reads the ``.fricas.input``
file) and then ``sman`` creates a new frame (usually a random lisp gensym
name). In this new frame (created after ``.fricas.input`` is read) your
mrandom function is not defined.

To see this do
::

   fricas -nosman   -- This starts FRICASsys directly.
   mrandom(3,3,3)   -- compiles and runs the function
   )quit

Now do
::

   fricas
   mrandom(3,3,3)   -- undefined function
   )frame next
   mrandom(3,3,3)   -- compiles and runs the function
   )frame names     -- shows you all of the defined frames
   )quit


So with the ``fricas`` shell script the process is
::

   fricas
     start sman                    (done by fricas shell script)
       sman starts FRICASsys        (done by sman)
         create frame "initial"    (done by FRICASsys)
           read .fricas.input       (define your function here)
         create frame "G00234"     (done by sman)
           put up a command prompt (in frame G00234, no functions defined)
         )frame next               (done by you)
                         .... and now you're back in frame initial
                         .... and your function is there

So your function was read and it is defined. However the function got
defined in the "initial" frame (because you defined it in the
``.fricas.input`` file) and is not known in the frame created by
``sman``. The ")frame next" command will move you around the ring of
frames. (See the ``)frame`` command in the appendix of the
|PACKAGE_BOOK|).


How can I debug algebra code?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

FriCAS contains some powerful commands to help with testing and
debugging library modules written in Spad and also the FriCAS system
itself. The most important of these commands is ``)trace``.

This command is used to trace the execution of functions that make
up the FriCAS system, functions defined by users, and functions from
the system library. Almost all options are available for each type
of function but exceptions will be noted below.

To list all functions, constructors, domains and packages that are
traced, simply issue
::

   )trace

To untrace everything that is traced, issue
::

   )trace )off

When a function is traced, the default system action is to display the
arguments to the function and the return value when the function is
exited. Other information can be displayed or collected when a
function is traced and this is controlled by the various options. If a
domain or package is traced, the default action is to trace all
functions exported. Individual interpreter, lisp or boot functions can
be traced by listing their names after ``)trace``. Any options that are
present must follow the functions to be traced. For example
::

   )trace f

traces the function f. To untrace f, issue
::

   )trace f )off

Note that if a function name contains a special character, it will
be necessary to escape the character with an underscore
::

   )trace _/D_,1

To trace all domains or packages that are or will be created from a
particular constructor, give the constructor name or abbreviation
after ``)trace``
::

   )trace MATRIX
   )trace List Integer

The first command traces all domains currently instantiated with
``Matrix``. If additional domains are instantiated with this constructor
(for example, if you have used ``Matrix(Integer)`` and ``Matrix(Float)``),
they will be automatically traced. The second command traces
``List(Integer)``.

The following are the general options for the ``)trace`` command.

``)break after`` -- causes a Common Lisp break loop to be entered after
exiting the traced function.

``)break before`` -- causes a Common Lisp break loop to be entered before
entering the traced function.

``)break`` -- is the same as ``)break before``.

``)count`` -- causes the system to keep a count of the number of times the
traced function is entered.

The total can be displayed with::

   )trace )stats

and cleared with::

   )trace )stats reset

``)count n`` -- causes information about the traced function to be displayed
for the first n executions. After the n-th execution, the function is
untraced.

``)depth n`` -- causes trace information to be shown for only n levels of
recursion of the traced function.

The command::

  )trace fib )depth 10

will cause the display of only 10 levels of trace information for the
recursive execution of a user function fib.

``)math`` causes -- the function arguments and return value to be displayed
in the FriCAS monospace two-dimensional math format.

``)nonquietly`` -- causes the display of additional messages when a function
is traced.

``)nt`` -- This suppresses all normal trace information. This option is useful
if the ``)count`` or ``)timer`` options are used and you are interested in the
statistics but not the function calling information.

``)off`` -- causes untracing of all or specific functions. Without an argument,
all functions, constructors, domains and packages are untraced. Otherwise,
the given functions and other objects are untraced.

To immediately retrace the untraced functions, issue::

  )trace )restore

``)only listOfDataToDisplay`` -- causes only specific trace information to be
shown.

``)restore`` -- causes the last untraced functions to be retraced. If
additional options are present, they are added to those previously in
effect.

``)stats`` -- causes the display of statistics collected by the use of the
``)count`` and ``)timer`` options.

``)stats reset`` -- resets to 0 the statistics collected by the use of the
``)count`` and ``)timer`` options.

``)timer`` -- causes the system to keep a count of execution times for the
traced function. The total can be displayed with ``)trace )stats`` and
cleared with ``)trace )stats reset``.

``)varbreak var1 ... varN`` -- causes a Common Lisp break loop to be
entered after the assignment to any of the listed variables in the
traced function.

``)vars`` -- causes the display of the value of any variable after it is
assigned in the traced function. Note that library code must have been
compiled using the ``)vartrace`` option in order to support this option.

``)vars var1  ... varN`` -- causes the display of the value of any of
the specified variables after they are assigned in the traced function.
Note that library code must have been compiled using the ``)vartrace``
option in order to support this option.

``)within executingFunction`` -- causes the display of trace information
only if the traced function is called when the given executingFunction
is running.

The following are the options for tracing constructors, domains and
packages.

``)local op1 ... opN`` -- causes local functions of the constructor to
be traced. Note that to untrace an individual local function, you must
use the fully qualified internal name, using the escape character before
the semicolon. For example::

  )trace FRAC )local
  )trace FRAC_;cancelGcd )off

``)ops op1 ... opN`` -- By default, all operations from a domain or package
are traced when the domain or package is traced. This option allows you
to specify that only particular operations should be traced.

The command::

  )trace Integer )ops min max _+ _-

traces four operations from the domain Integer. Since + and - are special
characters, it is necessary to escape them with an underscore.

Also See: ``)boot``, ``)lisp`` , and ``)ltrace``. Please refer to the
FriCAS Book section "FriCAS System Commands" for more detailed information.


How can I access lisp code from the FriCAS command line?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To run a lisp command from the command line use )lisp:
::

   --> )lisp (+ 2 3)

If you want to run a lot of lisp commands from the command line do
::

   --> )lisp (setq $dalymode t)
   --> (+ 2 3)
   --> (defun foo (x y) (+ x y))
   --> (foo 2 3)
   --> 2 + 3

$dalymode says::

  If the first character is a '('
     then it is lisp
     else it is fricas

to disable it do
::

   --> (setq $dalymode nil)

Tim Daly wrote this change to the interpreter because he used lisp a lot
during maintenance. It breaks some syntax but you can work around that.

If you really want to "drop" into lisp do
::

   --> )fin
   BOOT> (+ 2 3)

and now you are talking only to lisp at a lisp command prompt in
the BOOT package. To restart FriCAS type
::

   BOOT>(restart)

Note: ')fin' does not work when using Clozure CL or ECL.  Due to
this do not use it in partable scripts.
