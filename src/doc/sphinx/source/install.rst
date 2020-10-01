Installation Guide
==================

.. contents:: Table of Contents
   :local:


Quick installation
------------------

FriCAS now tries to support standard GNU build/installation
conventions.  So if you have sources and all prerequisites, then
::

   configure && make && sudo make install

should work.  The above will install FriCAS files into
``/usr/local/lib/fricas/``  and put the ``fricas`` command into
``/usr/local/bin/``.
You can give arguments to ``configure`` to change those locations.



Prerequisites
-------------

Standard build tools
^^^^^^^^^^^^^^^^^^^^

To *build* FriCAS you need standard build tools like C compiler and
make.

Lisp
^^^^

To *build* FriCAS you need *one* of the following Lisp variants:

- `SBCL`_ 1.0.7 or later (preferred)

  http://sbcl.sourceforge.net/platform-table.html

- `Clozure CL`_ (former openmcl), starting from openmcl 1.1 prerelease
  070512

  https://ccl.clozure.com/download.html

- ECL_ 0.9l or later

  https://common-lisp.net/project/ecl

- CLISP_ 2.41 or later
- CMUCL_

- FriCAS builds also using GCL_, at least build using released version
  2.6.12 works.  Build using newer GCL versions from git repository
  fails.


All Lisp implementations should give essentially the same
functionality, however performance (speed) may differ quite a lot.  ATM
CMU CL port should be considered experimental, it received only little
testing.  Also CMU CL seem to have problems on some machines.  By
default FriCAS tries to use SBCL, since it is fast and reliable.  On
64-bit AMD64 on average SBCL is the fastest one (9 times faster than
CLISP), Clozure CL the second (about 1.5 times slower than SBCL), than
GCL and ECL (about 3 times slower than SBCL) and CLISP is the slowest
one.  Note: very old versions of ECL were much (about 4 times) slower, you
should use reasonably new version if you care about speed.

Some computation work much faster on 64-bit machines, especially
when using SBCL.


X libraries (optional, but needed for graphics and HyperDoc)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On Debian (or Ubuntu) install the following packages.
::

   sudo apt install libx11-dev libxt-dev libice-dev \
                    libsm-dev libxau-dev libxdmcp-dev libxpm-dev


xvfb (optional, but highly recommended)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you compile FriCAS from the |git repository|, and ``configure``
does not detect the ``xvfb-run`` program, then graphic examples will
not be built.  See Section `HyperDoc and graphics`_ for more detail.
::

   sudo apt install xvfb


GMP (optional)
^^^^^^^^^^^^^^

You you use SBCL or Clozure CL the ``--enable-gmp`` configure option
is available only if the development version of GMP is installed.
Note: using GMP should work on all SBCL and Clozure CL platforms
except for Clozure CL on Power PC.
::

   sudo apt install libgmp-dev


LaTeX (optional)
^^^^^^^^^^^^^^^^

If you run FriCAS in Emacs_ (efricas) you can enable
::

   )set output tex on

to show rendered TeX output.  For that to work, you need the following.
::

   sudo apt install texlive auctex dvipng

In order to build the |PACKAGE_BOOK|, you also need the following
LaTeX packages (available from CTAN_).
::

   amsmath
   breqn
   tensor
   mleftright
   epsf
   verbatim
   hyperref
   color
   listings
   makeidx
   xparse
   tikz


SphinxDoc (optional)
^^^^^^^^^^^^^^^^^^^^

The documentation is built via Sphinx_.
::

   sudo apt install python3 python3-pip
   pip3 install -U Sphinx


Aldor (optional)
^^^^^^^^^^^^^^^^

If you want to use Aldor_ to extend the FriCAS library, you must, of
course, hava Aldor_ installed, and add ``--enable-aldor`` to your
configure options when you compile FriCAS.


Extra libraries needed by ECL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This only applies if you use Debian ECL.
::

   sudo apt install libffi-dev



Detailed installations instructions
-----------------------------------

We assume that you have installed all necessary prerequisittes.

0. Change to a directory with enough (0.8 GB) free space.

1. Fetch sources.
   ::

      git clone --depth 1 https://github.com/fricas/fricas

   Remove the ``--depth 1`` option for access to the change history.

2. Create build directory and change to it
   ::

      mkdir fr-build
      cd fr-build

3. Configure.  Assuming that you want fricas files to be installed in
   ``/tmp/usr``.
   ::

      ../fricas/configure --with-lisp=/path/to/your/lisp --prefix=/tmp/usr

   where ``/path/to/your/lisp`` is name of your Lisp.  For example,
   type
   ::

      ../fricas/configure --with-lisp="sbcl --dynamic-space-size 4096" --prefix=/tmp/usr --enable-gmp --enable-aldor

   to build with SBCL and 4 GiB dynamic space, use GMP, and enable the
   build of the Aldor library ``libfricas.al``.

   Type
   ::

      ../fricas/configure --help

   to see all possible options.

4. Build and install
   ::

      make
      make install

   Optionaly, to gain confidence that your build works, you can
   run tests
   ::

      make check


Extra information
^^^^^^^^^^^^^^^^^

The preferred way to build FriCAS is to use an already installed Lisp.
Also, it is preferable to use a separate build directory.  Assuming
that the source tree is in ``$HOME/fricas``, you build in
``$HOME/fricas-build`` subdirectory and your Lisp is called
``sbcl`` the following should just work.
::

   cd $HOME/fricas-build
   $HOME/fricas/configure --with-lisp=sbcl && make && sudo make install

Currently ``--with-lisp`` option accepts all supported lisp variants,
namely SBCL, CLISP, ECL, GCL and Clozure CL (openmcl).  Note: the
argument is just a command to invoke the respective Lisp variant.
Build machinery will automatically detect which Lisp is in use and
adjust as needed.



HyperDoc and graphics
^^^^^^^^^^^^^^^^^^^^^

If you compile FriCAS from the |git repository|, and ``configure``
does not detect the ``xvfb-run`` program, then graphic examples will
not be built.  This results in broken HyperDoc pages -- all graphic
examples will be missing (and trying to access them will crash
hypertex).

The get working graphic examples login into X and replace ``make``
above by the following
::

   make MAYBE_VIEWPORTS=viewports

Alternatively, after ``make`` finishes use
::

   make viewports

*Important*: building graphic examples accesses the X server, so it
will not work on text console.  During build drawings will temporarily
appear on the screen.  Redirecting X via ``ssh`` should work fine, but
may be slow.

It is possible to use the ``xvfb-run`` program, replacing
``make viewports`` above by
::

   xvfb-run -a -n 0 -s '-screen 0 1024x768x24' make viewports


Algebra optimization
^^^^^^^^^^^^^^^^^^^^

When writing/compiling programs there is always tradeoff between speed
and safety.  Programs may include many checks to detect errors early
(and allow recovery).  Such programs are safe but checks take time so
the program is slower.  Or a program may just blindly goes forward
hoping that everything goes well.  Typically the second program will be
faster, but in case of problems it may crash without any hint why and
take user data with it.

Safety checks may be written by programmers, but another possibility
is to have a compiler which automatically inserts various checks.
FriCAS is compiled by a Lisp compiler and Lisp compilers may insert
safety checks.  How many checks are inserted may be controlled by the
user.  By default FriCAS tries to strike good balance between speed and
safety.  However, some FriCAS users want different tradeoff.  The
::

   --enable-algebra-optimization=S

option to configure allows changing this setting: S is a Lisp
expression specifying speed/safety tradeoff used by Lisp compiler.  For
example::

  --enable-algebra-optimization="((speed 3) (safety 0))"

chooses fastest (but unsafe) variant, while
::

  --enable-algebra-optimization="((speed 2) (safety 3))"

should be very safe (but possibly slow).

Note: this setting affects only algebra (that is mathematical code).
The rest of FriCAS always uses default setting.  Rationale for this is
that mathematical code is unlikely to contain errors which can crash
the whole system.



Using GMP with SBCL or Clozure CL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently on average FriCAS is fastest when compiled using SBCL_.
However, SBCL normally uses its own routines for computations with
large numbers and those routines are slower than GMP_.  FriCAS now has
special support to replace sbcl arithmetic routines by GMP.  To use
this support install GMP including header files (development package
if you install via a package manager).  Currently there are two
available GMP_ versions, version 5 is much faster than version 4.  Then
configure FriCAS adding ``--enable-gmp`` option to the ``configure``
arguments.

FriCAS also has support for using GMP_ with `Clozure CL`_.  Currently
Clozure CL with GMP works on 32/64 bit Intel/AMD processors and ARM
(using Clozure CL with GMP is not supported on Power PC processors).

When you have GMP installed in a non-standard location (this usually
means anything other than ``/usr`` or ``/usr/local``) then you can
specify the location with
::

   configure`--with-gmp=PATH

This supposes that the include file is in ``PATH/include`` and libgmp
is in ``PATH/lib``.  If you have a different setup, then you can
specify
::

   --with-gmp-include=INCLUDEPATH --with-gmp-lib=LIBPATH

(specify the directories where the include files and libgmp are found,
respectively).

These options also implicitly set ``--enable-gmp``.  However, if
``--enable-gmp=no`` is given, then ``--with-gmp=...``,
``--with-gmp-include=...`` and ``--with-gmp-lib=...`` is ignored.



Building documentation
^^^^^^^^^^^^^^^^^^^^^^

After a build of FriCAS, (suppose your build directory is under
``$BUILD``), the |home page| can be built via
::

   cd $BUILD/src/doc
   make doc

This builds the full content of the |home page| including the
|PACKAGE_BOOK| (also known as the FriCAS User Guide) into the
directory ``src/doc/html`` from which it can be committed to the
``gh-pages`` branch of the official |git repository|.

Most links also work fine if you start
::

   firefox src/doc/html/index.html

but some links point to the web.  If you want the links referring only
to the data on your computer, you call the compilation like this
::

   cd $BUILD/src/doc
   make localdoc

This will have broken references to the
`FriCAS Demos and Tutorials <https://fricas.github.io/fricas-notebooks/>`_
as they live in a separate repository.  Do the following to get a local
copy and thus have working references.
::

   cd $BUILD/src/doc/html
   git clone -b gh-pages https://github.com/fricas/fricas-notebooks


For more control on the generation of the FriCAS website content,
you can set various variables (see ``src/doc/Makefile.in``)
in the |git repository|.
For example, if you like to push to your forked FriCAS repository and
refer to branch ``foo`` instead of ``master`` then do as follows
(replace ``hemmecke`` by your account name).
::

   make PACKAGE_SOURCE=https://github.com/hemmecke/fricas \
        BRANCH=foo \
        PACKAGE_URL=https://hemmecke.github.io/fricas \
        doc

If you want to change the version information provided by default
through ``configure.ac``, you can add a variable assignment like this
to the above command.
::

   PACKAGE_VERSION=$(git log -1 --pretty=%H)
   PACKAGE_VERSION="1.3.6+ `date +'%Y-%m-%d %H:%M'`"

Then, checkout the ``gh-pages`` branch and put the data from
``$BUILD/src/doc/html`` into your ``gh-pages`` branch.
::

   git clone git@github.com:hemmecke/fricas.git
   git checkout gh-pages
   git rm -rf .
   rm '.gitignore'
   echo 'https://help.github.com/articles/using-jekyll-with-pages' > .nojekyll
   cp -a $BUILD/src/doc/html/* .
   rm -r _sources/api/
   git add .
   git commit -m "$PACKAGE_VERSION"
   git push origin gh-pages

Of course, leave out the ``--orphan`` switch, if you already have an
appropriate ``gh-pages`` branch.


Optional: If you add
::

   text/x-spad       spad

to ``/etc/mime.types`` and in firefox associate ``text/x-spad`` with
your editor, then clicking on a ``.spad`` file opens the `.spad` file
in in this editor.



Aldor library libfricas.al
^^^^^^^^^^^^^^^^^^^^^^^^^^

You can not only extend the FriCAS library by ``.spad`` files (SPAD
programs), but also by ``.as`` files (Aldor_ programs).  For the latter
to work FriCAS needs a library ``libfricas.al``.

This library can be build as follows.
(An Aldor compiler is of course a prerequisite.)
::

   configure --enable-aldor --prefix=/tmp/usr
   ( cd src/aldor &&  make )
   make install

After that you should be able to compile and use the program below in
a FriCAS session via
::

   )compile sieve.as
   sieve 10

The program ``sieve.as`` is::

  --
  -- sieve.as: A prime number sieve to count primes <= n.
  --
  #include "fricas"

  N ==> NonNegativeInteger;
  import from Boolean, N, Integer;

  sieve(n: N): N  == {
      isprime: PrimitiveArray Boolean := new(n+1, true);
      np: N := 0;
      two: N := 2;
      for p in two..n | isprime(p::Integer) repeat {
          np := np + 1;
          for i in two*p..n by p::Integer repeat {
              isprime(i::Integer) := false;
          }
      }
      np
  }



Known problems
--------------

- currently on when using case insensitive filesystem (typically on
  Mac OSX and Windows), the git version can be only build in a
  separate directory (in-tree build will fail).  This does not affect
  release tarball.

- In general, any error when generating documentation will cause build
  to hang.

- 32-bit sbcl from 1.5.9 to 2.1.3 may miscompile floating point
  comparisons.   Due to this most plots wil fail.   The problem is
  fixed in developement version of sbcl.   Alternatively, use older
  version of sbcl.   64-bit sbcl works OK.

- by default sbcl 1.0.54 and newer limits memory use to 1GB, which is
  too small for heavy use.  To work around this one can pass
  ``--dynamic-space-size`` argument during sbcl build to increase
  default limit.
  We recommend limit slightly smaller than amount of
  available RAM (in this way FriCAS will be able to use almost all
  RAM, but limit should prevent thrashing).

- Some Linux versions, notably SuSE, by default seem to have very
  small limit on virtual memory.  This causes build failure when using
  sbcl or Clozure CL.  Also if limit on virtual memory is too small
  sbcl-based or Clozure CL-based FriCAS binary will silently fail at
  startup.  The simplest workaround is to increase limit, in the shell
  typing
  ::

     ulimit -v unlimited

  Alternatively for sbcl one can use ``--dynamic-space-size`` argument
  to decrease use of virtual memory.

- On new Linux kernel build using Clisp may take very long time.  This
  is caused by frequent calls to ``fsync`` performed without need by
  Clisp.

- on some systems (notably MAC OSX) when using sbcl default limit of
  open files may be too low.  To workaround increase limit (experiments
  suggest that 512 open files is enough).  This should not be needed in
  FriCAS 1.1.7.

- sbcl from 1.3.1 to 1.3.4 runs out of memory when compiling FriCAS.
  This is fixed in newer versions of sbcl.

- using sbcl from 1.0.47 to 1.0.49 compilation is very slow (few hours
  on fast machine).  This is fixed in newer versions of sbcl.

- sbcl-1.0.29 has a bug in the ``directory`` function which causes
  build failure.  This problem is fixed in 1.0.29.54.rc1.

- 1.0.29.54.rc1 has broken complex ``tanh`` function -- you will get
  wrong results when applying ``tanh`` to ``Complex DoubleFloat``.

- in sbcl 1.0.35 and up Control-C handling did not work.  This should
  be fixed in current FriCAS.

- prerelease gcl from gcl git repository is incompatible with FriCAS
  and build will fail.

- older gcl had serious problems on Macs and Windows.

- released gcl-2.6.9 has a bug which causes failure of FriCAS build.
  This problem is fixed in 2.6.10 and later but but there is a
  different one.  Namely, FriCAS builds but apparently on some machines
  is miscompiled using released 2.6.10 or 2.6.11 or 2.6.12.

- On Gentoo system installed gcl probably will not work, one need to
  build own one.

- Older version of gcl are incompatible with Fedora "exec-shield" and
  strong address space randomization (setting randomize_va_space to
  2).  Newest CVS version of 2.6.8 branch of gcl fixes this problem.

- gcl needs bfd library.  Many Linux systems include version of bfd
  library which is incompatible with gcl.  In the past we advised to
  use in such case the following configure line
  ::

     configure --disable-xgcl --disable-dynsysbfd --disable-statsysbfd --enable-locbfd

- Boehm garbage collector included in old ECL (version 6.8) is incompatible
  with Fedora strong address space randomization (setting randomize_va_space
  to 2).   Using newer version of Boehm garbage collector (7.0 or 7.1) or
  newer ECL should solve this problem.

- Striping FriCAS binaries is likely to break them.  In particular
  Clisp based FriCAS may crash with message
  ::

     module 'syscalls' requires package OS.

  while sbcl will show only loader prompt.

- On Mac OSX Tiger some users reported problems with pseudoterminals,
  build stopped with the message
  ::

     fork_Axiom: Failed to reopen server: No such file or directory

  This problem is believed to be fixed in FriCAS-1.0.5 (and later).

- ECL 9.6.2 (and probably also 9.6.1 and 9.6.0) has a bug with
  handling string constants which causes build based on this version
  to fail.  This bugs is fixed in newer versions.  ECL 9.7.1 generates
  wrong C code, so that build fails.  This is fixed in newer versions.

- Unicode-enabled ECL before 9.8.4 is unable to build FriCAS.

- ECL up to version 0.9l may segfault at exit.  This is usually
  harmless, but may cause build to hang (for example when generating
  ``ug13.pht``).

- Clozure CL 1.10 apparently miscompiles some operations on U32Matrix.
  Version 1.11 works OK.

- Clozure CL 1.7 and 1.6 apparently miscompiles FriCAS.  Versions 1.8
  and newer and 1.5 and earlier work OK.

- Clozure CL earlier than release 1.2 (former Openmcl) has a bug in
  Lisp printer.  This bug causes incorrect printing of FriCAS types.
  Also, Clozure CL earlier than release 1.2 has bug in complex cosine
  function.  Those bugs are fixed in release 1.2.  If you want to use
  earlier version you can work around the bugs applying the
  ``contib/omcl.diff`` patch and recompiling the compiler (see the
  patch or Clozure CL documentation for instructions).

- Older versions of Clisp may fail to build FriCAS complaining about
  opening already opened file -- this is error is spurious, the file
  in question in fact is closed, but for some reason Clisp got
  confused.


.. _Aldor: https://github.com/aldorlang/aldor
.. _CLISP: http://clisp.cons.org
.. _Clozure CL: http://ccl.clozure.com/manual/chapter2.2.html
.. _CMUCL: https://www.cons.org/cmucl/
.. _CTAN: https://www.ctan.org/
.. _ECL: http://ecls.sourceforge.net
.. _Emacs: https://www.gnu.org/software/emacs/
.. _GCL: https://www.gnu.org/software/gcl
.. _GMP: https://gmplib.org
.. _SBCL: http://sbcl.sourceforge.net/platform-table.html
.. _Sphinx: https://www.sphinx-doc.org
