Installation Guide
==================

.. contents:: Table of Contents
   :local:
   :depth: 2


Note: this text is mostly about installation from sources.
If you fetched compiled binaries skip to section about
binary distribution.

Quick installation
------------------

FriCAS now tries to support standard GNU build/installation
conventions.  So if you have sources and all prerequisites, then
::

   ./configure && make && sudo make install

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
  2.6.14 works.  Build using older GCL versions no longer works.
  Note that with default setting build is likely to fail.
  Look at GCL_MEM_MULTIPLE note in Known problems section
  for possible workaround.


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


jFriCAS (optional)
^^^^^^^^^^^^^^^^^^

jFriCAS_ is an interface for running FriCAS_ in a Jupyter_ notebook.
It should be installed **after** FriCAS_ has been installed.

**Note:** It currently only works with an SBCL_ image that has the
Hunchentoot_ webserver included.  See next section.


Hunchentoot (optional)
^^^^^^^^^^^^^^^^^^^^^^

The jFriCAS_ interface needs a web server built into FRICASsys binary.
This can be done by using Lisp (currently only SBCL_) containing
the Hunchentoot_ web server.  You can provide your own Lisp with
preloaded Hunchentoot_.  Or you can fetch the ``hsbcl-1.3.9.tar``
tarball from FriCAS distribution area.  Then do
::

    tar -xf hsbcl-1.3.9.tar
    cd hsbcl
    ./build_hsbcl > build_hsbcl.log 2>&1

This assumes that the base Lisp to use is SBCL_ and creates executable
binary ``hsbcl`` which contains Hunchentoot_. If your SBCL_ is started
in different way (say via full pathname), then edit ``build_hsbcl`` to
match. After creating ``hsbcl`` one can then configure FriCAS like
::

    ../fricas-1.3.9/configure --with-lisp=/path/to/hsbcl --enable-gmp

FriCAS build in this way will contain Hunchentoot_ and can be used
by jFriCAS_.


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
   pip3 install -U Sphinx==5.3.0

**WARNING**: Currently, Sphinx_ 6 and higher will fail building the
``.html`` pages.


Aldor (optional)
^^^^^^^^^^^^^^^^

If you want to use Aldor_ to extend the FriCAS library, you must, of
course, have Aldor_ installed, and add ``--enable-aldor`` to your
configure options when you compile FriCAS.


Extra libraries needed by ECL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This only applies if you use Debian ECL.
::

   sudo apt install libffi-dev



Detailed installation instructions
----------------------------------

We assume that you have installed all necessary prerequisites.

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

   to build with SBCL_ and 4 GiB dynamic space, use GMP_, and enable the
   build of the Aldor_ library ``libfricas.al``.

   Use
   ::

      --with-lisp="/path/to/hsbcl"

   to include the Hunchentoot_ webserver if you later want to install
   jFriCAS_.

   Type
   ::

      ../fricas/configure --help

   to see all possible options.

4. Build and install
   ::

      make
      make install

   Optionally, to gain confidence that your build works, you can
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

Note that jFriCAS_ has currently only been tested to work with SBCL_.


HyperDoc and graphics
^^^^^^^^^^^^^^^^^^^^^

If you compile FriCAS from the |git repository|, and ``configure``
does not detect the ``xvfb-run`` program, then graphic examples will
not be built.  This results in broken HyperDoc pages -- all graphic
examples will be missing (and trying to access them will crash
hypertex).

To get working graphic examples login into X and replace ``make``
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

   xvfb-run -a -s '-screen 0 1024x768x24' make viewports


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

   configure --with-gmp=PATH

This means that the header files are in ``PATH/include`` and libgmp
is in ``PATH/lib``.  If you have a different setup, then you can
specify
::

   --with-gmp-include=INCLUDEPATH --with-gmp-lib=LIBPATH

(specify the directories where the header files and libgmp are found,
respectively).

These options also implicitly set ``--enable-gmp``.  However, if
``--enable-gmp=no`` is given, then ``--with-gmp=...``,
``--with-gmp-include=...`` and ``--with-gmp-lib=...`` is ignored.



Post-compilation steps (optional)
---------------------------------


Build extra documentation (book and website)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


After a build of FriCAS, (suppose your build directory is under
``$BUILD``), you can build the documentation provided at
the |home page| on your local installation.

To build the extra documentation you need a working ``convert`` program
from ImageMagick_.  Note that several Linux distribution currently disable
the ability to create ``.ps`` files via ``convert``.  If your distribution
is doing this, then the build of extra documentation will fail.

In Ubuntu you can allow the creation of ``.ps`` files by editing
``/etc/ImageMagick-6/policy.xml`` as ``root`` and changing the
respective line to
::

   <policy domain="coder" rights="read|write" pattern="PS" />


Currently building ``.html`` pages does not work with Sphinx 6.
You must install Sphinx 5.3.0 (or smaller) or only build the
|PACKAGE_BOOK| via
::

   cd $BUILD/src/doc
   make book.pdf

The |home page| can be built via
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
   PACKAGE_VERSION="1.3.9+ `date +'%Y-%m-%d %H:%M'`"

Then, checkout the ``gh-pages`` branch and put the data from
``$BUILD/src/doc/html`` into your ``gh-pages`` branch.
::

   git clone git@github.com:hemmecke/fricas.git
   cd fricas
   git checkout gh-pages
   git rm -rf .
   rm '.gitignore'
   echo 'https://help.github.com/articles/using-jekyll-with-pages' > .nojekyll
   cp -a $BUILD/src/doc/html/* .
   rm -r _sources/api/
   git add .
   git commit -m "$PACKAGE_VERSION"
   git push origin gh-pages

You must use ``git checkout --orphan gh-pages`` if you do not yet have
a ``gh-pages`` branch.


Optional: If you add
::

   text/x-spad       spad

to ``/etc/mime.types`` and in firefox associate ``text/x-spad`` with
your editor, then clicking on a ``.spad`` file opens the ``.spad``
file in this editor.



Build FriCAS-Aldor interface (libfricas.al)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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



Install jFriCAS
^^^^^^^^^^^^^^^

There are a couple of things to install.

#. Jupyter
#. jFriCAS

Except for the file ``$HOME/.jupyter/jupyter_notebook_config.py`` that
maybe necessary to create, the following description will put most of
the things (in particular the git repositories) under the directory
``$FDIR``.
We assume that FriCAS will be installed into ``$FRICASINSTALL``.
jFriCAS_ and Jupyter_ will go into ``$JFRICASINSTALL``
You can change any of these paths or even install without a python
virtual environment, but there is no description (yet) for an
installation without venv.
::

   FDIR=$HOME/fricas
   GITREPOS=$FDIR
   FRICASINSTALL=$FDIR/install
   export PATH=$FRICASINSTALL/bin:$PATH
   VENV=$FDIR/venv
   JFRICASINSTALL=$VENV/jfricas
   mkdir -p $FDIR $GITREPOS $FRICASINSTALL $JFRICASINSTALL


jFriCAS installation
""""""""""""""""""""

jFriCAS_ is the Jupyter_ notebook interface to FriCAS_. Of course,
jFriCAS_ needs Jupyter_ in a reasonably recent version (at least 4).

Install prerequisites if not yet available (needs root access, but it
may already be installed on your system).
::

   sudo apt install python3-pip python3-venv

Prepare directories and download jFriCAS_.
::

   cd $GITREPOS
   git clone https://github.com/fricas/jfricas

Install prerequisites, Jupyter_ and jFriCAS_.

**WARNING**: Do not install jfricas 1.0.0 from PyPI, as that will
not work. If you have it installed, then uninstall it first.
::

   python3 -m venv $JFRICASINSTALL
   source $JFRICASINSTALL/bin/activate
   pip3 install wheel jupyter
   cd $GITREPOS/jfricas
   pip3 install .
   jupyter kernelspec list

The output of the last command should show something similar to the
following.
::

    Available kernels:
      jfricas    /home/hemmecke/fricas/venv/jfricas/share/jupyter/kernels/jfricas
      python3    /home/hemmecke/fricas/venv/jfricas/share/jupyter/kernels/python3

Create the script ``jfricas``.
::

   cat > $FRICASINSTALL/bin/jfricas <<EOF
   source $JFRICASINSTALL/bin/activate
   jupyter notebook \$1
   EOF
   chmod +x $FRICASINSTALL/bin/jfricas

Start a new terminal or set the ``PATH`` on the command line or inside
your ``.bashrc`` file and start ``jfricas`` from any directory (after
you have installed FriCAS_).
::

   export PATH=$FRICASINSTALL/bin:$PATH

Note that inside jupyter the place from where you start
``jfricas`` is the place where your notebooks will be stored.

You can start a new FriCAS session by selecting ``FriCAS`` from the
``New`` drop down menu.
If you want to enjoy nice looking output, then type the following
inside a notebook cell.
::

   )set output algebra off
   setFormat!(FormatMathJax)$JFriCASSupport

You can go back to standard 2D ASCII output as follows.
::

   )set output formatted off
   )set output algebra on



(optional) Install JupyText
"""""""""""""""""""""""""""

Ordinary Jupyter notebooks use a special format in order to store
their content. They have the file extension ``.ipynb``. It is an
incredible feature to be able to load and store notebooks as ordinary
FriCAS ``.input`` files. You can even synchronize between the
``.ipynb`` and ``.input`` formats.

There are two types of cells in Jupyter_: Markdown documentation
cells and execution cells. With the help of JupyText_, Markdown
cells will appear inside an ``.input`` file as FriCAS_
comments and execution cells appear without the ``"-- "``
comment prefix.
::

   source $JFRICASINSTALL/bin/activate
   pip3 install jupytext

Enable the spad language and set the respective parameters.
::

   cd $HOME
   J=$(find $JFRICASINSTALL -type d | grep '/site-packages/jupytext$')
   emacs $J/languages.py

Edit the file ``$J/languages.py`` and change appropriately.
::

   # Jupyter magic commands that are also languages
   _JUPYTER_LANGUAGES = ["spad", "R", ...]

   # Supported file extensions (and languages)
   # Please add more languages here (and add a few tests) - see CONTRIBUTING.md
   _SCRIPT_EXTENSIONS = {
      ".py": {"language": "python", "comment": "#"},
       ".input": {"language": "spad", "comment": "--"},
       ".input-test": {"language": "spad", "comment": "--"},
       ...
   }


Make Jupytext available
"""""""""""""""""""""""

In Ubuntu 22.04 you need not do run the commands from this section.
It seemingly works without having to change something in the
configuration file. There were even reports that jFriCAS_ stopped
working if ``c.NotebookApp.contents_manager_class`` was set.  However,
for older versions of JupyText_ and/or Jupyter_, the following had to be
configured.

If ``$HOME/.jupyter/jupyter_notebook_config.py`` does not yet exist,
generate it.
*Note that this is outside the* ``$FDIR`` *directory.*
::

   jupyter notebook --generate-config


For the following see
https://jupyter-notebook.readthedocs.io/en/stable/config.html .
::

   sed -i 's|^# *c.NotebookApp.use_redirect_file = .*|c.NotebookApp.use_redirect_file = False|' $HOME/.jupyter/jupyter_notebook_config.py


The following enables JupyText_.
::

   sed -i 's|^# *c.NotebookApp.contents_manager_class =.*|c.NotebookApp.contents_manager_class = "jupytext.TextFileContentsManager"|' $HOME/.jupyter/jupyter_notebook_config.py





Put the following input into the file ``$FDIR/foo.input``.
::

   -- # FriCAS demo notebook

   )set output algebra off
   setFormat!(FormatMathJax)$JFriCASSupport

   -- Here we compute $\frac{d^2}{dx^2} sin(x^3)$.

   D(sin(x^3),x,2)

   -- We compute the indefinite integral $\int \sin x \cdot e^x dx$.

   integrate(exp(x)*sin(x), x)


Then start via ``jfricas``, load ``foo.input`` and enjoy.
::

   cd $FDIR
   jfricas

If something does not work then look at the end of ``fricaskernel.py``
and experiment with different versions of how to start FriCAS.
::

   FRICASKERNEL=$(find $JFRICASINSTALL -type f | grep 'fricaskernel\.py$')
   emacs $FRICASKERNEL

You can also download or clone the demo notebooks from
https://github.com/fricas/fricas-notebooks/ and compare them with what
you see at
`FriCAS Demos and Tutorials <https://fricas.github.io/fricas-notebooks/index.html>`_.


Install frimacs
^^^^^^^^^^^^^^^

frimacs_ is an Emacs_ mode for FriCAS with special features to
edit ``.input`` and ``.spad`` files as well as executing a FriCAS_
session inside an Emacs_ buffer.

Install as follows.
::

   cd $GITREPOS
   git clone https://github.com/pdo/frimacs.git

If your ``GITREPOS=/home/hemmecke/fricas``, then add the line
::

   (load-file "/home/hemmecke/fricas/frimacs/frimacs.el")

to your ``.emacs`` or ``.emacs.d/init.el`` file.

To start a FriCAS_ session use
::

   M-x run-fricas




Creation of distribution tarballs
---------------------------------

The source distribution can be created as follows.  Fetch and
build sources, taking care to build Hyperdoc pages and graphic
examples.  Make sure that text of help pages is available in some
directory (they are **not** part of source tree, some are generated,
but the rest is copied to tarball).  Assuming that you build FriCAS
in ``fr-build`` and ``$SRC`` point to FriCAS source tree do
::

   cd fr-build
   $SRC/src/scripts/mkdist.sh --copy_lisp --copy_phts \
     --copy_help=/full/path/to/help/files
   mv dist ../fricas-X.Y.Z
   cd ..
   tar -cjf fricas-X.Y.Z.tar.bz2 fricas-X.Y.Z

Note: FriCAS source distributions are created from a branch which
differs from trunk, namely release branch has version number, trunk
instead gives date of last update to ``configure.ac``.  If you
wish you can create distribution tarballs from trunk.

The binary distribution can be created as follows.  First fetch and
unpack source tarball in work directory.  Then in work directory
::
   mkdir fr-build
   ../fricas-X.Y.Z/configure --enable--gmp --with-lisp=/path/to/hsbcl
   make -j 7 > makelog 2>&1
   make DESTDIR=/full/path/to/auxiliary/dir install
   cd /full/path/to/auxiliary/dir
   tar -cjf fricas-x.y.z.amd64.tar.bz2 usr


Installation from binary distribution
-------------------------------------

You can download the latest release as a ``.tar.bz2`` from
https://github.com/fricas/fricas/releases and install as follows (of
course, you can set ``FDIR`` to anything you like).
::

   FDIR=$HOME/fricas
   mkdir -p $FDIR
   cd $FDIR
   tar xjf fricas-x.y.z.amd64.tar.bz2

If before running ``tar`` you change to the root directory and do
this command as ``root``, then you will get ready to run FriCAS in
the ``/usr/local`` subtree of the filesystem.  This puts FriCAS files
in the same places as running ``install`` after build from source
using default settings.

Alternatively, you can put FriCAS files anywhere in your file system,
which is useful if you want to install FriCAS without administrator
rights.

For this to work you need to adapt the ``fricas`` and ``efricas`` scripts
to point to the right paths.  This is explained in

http://fricas.sourceforge.net/doc/INSTALL-bin.txt

After installation you can start FriCAS with full path name
like one of the following commands.
::

   $FDIR/usr/local/bin/fricas
   $FDIR/usr/local/bin/efricas

Of course, you must have Emacs_ installed for the ``efricas``
script to work correctly.

You might have to install
::

   sudo apt install xfonts-75dpi xfonts-100dpi

and restart the X server (log out and log in again) in case the font
in HyperDoc does not look pretty.

That is, however, not necessary, if you do not intend to use HyperDoc
a lot and rather look at the FriCAS_ homepage in order to find
relevant information.

Optionally, set the PATH in ``$HOME/.bashrc``:

Edit the file ``$HOME/.bashrc`` (or whatever your shell initialization
resource is) and put in something like the following in order to make
all fricas scripts available.
::

   FDIR=$HOME/fricas
   export PATH=$FDIR/usr/local/bin:$PATH



Known problems
--------------

- currently when using case insensitive filesystem (typically on
  macOS and Windows), the git version can only be built in a
  separate directory (in-tree build will fail).  This does not affect
  release tarball.

- In general, any error when generating documentation will cause build
  to hang.

- 32-bit sbcl from 1.5.9 to 2.1.3 may miscompile floating point
  comparisons.   Due to this most plots will fail.   The problem is
  fixed in newer versions of sbcl.   Alternatively, use older
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

- CLISP built with threads support may fail to compile FriCAS.

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

- gcl-2.6.14 by default tries to use large fraction of available
  memory.  However with default settings, it can only load code
  into first 2Gb of memory.  If more than 2Gb of memory are
  available this is likely to lead to error when loading compiled
  code after longer computation.  Due to this, FriCAS build is
  likely to fail.  One possible workaround is to limit amount of
  memory available to gcl.  This can be done by setting environment
  variable GCL_MEM_MULTIPLE.  Set it to floating point value which
  multiplied by total memory gives about 2Gb.  For example, on
  32Gb machine set GCL_MEM_MULTIPLE to 0.07.

- On Gentoo system installed gcl probably will not work, one need to
  build own one.

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
.. _frimacs: https://github.com/pdo/frimacs
.. _GCL: https://www.gnu.org/software/gcl
.. _GMP: https://gmplib.org
.. _Hunchentoot: https://edicl.github.io/hunchentoot/
.. _ImageMagick: https://imagemagick.org/
.. _jFriCAS: https://jfricas.readthedocs.io
.. _Jupyter: https://jupyter.org
.. _JupyText: https://jupytext.readthedocs.io
.. _SBCL: http://sbcl.sourceforge.net/platform-table.html
.. _Sphinx: https://www.sphinx-doc.org
