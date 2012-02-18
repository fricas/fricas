sinclude(config/fricas.m4)
AC_INIT([FriCAS], [1.1.6],
        [fricas-devel@googlegroups.com])

AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
AC_CONFIG_HEADERS([config/axiom-c-macros.h])
# AM_INIT_AUTOMAKE([foreign])
AC_PREREQ([2.59])

AC_CONFIG_SRCDIR(src/Makefile.in)

# FRICAS_CHECK_PROGS and FRICAS_CHECK_PROG signal error if the
# program is not found
AC_DEFUN([FRICAS_CHECK_PROGS],
   [AC_CHECK_PROGS([$1], [$2])
   test -n "$$1" || AC_MSG_ERROR([$3])])

AC_DEFUN([FRICAS_CHECK_PROG],
   [AC_CHECK_PROG([$1], [$2], [$2])
   test -n "$$1" || AC_MSG_ERROR([$3])])

fricas_src_subdirs="lib lisp boot interp share algebra input etc"
AC_SUBST(fricas_src_subdirs)

AC_CANONICAL_SYSTEM

case $build in
  *mingw*)
    fricas_pwd=`pwd -W`
    fricas_top_srcdir=`cd "$srcdir" && pwd -W`
     ;;
  *)
    fricas_pwd=`pwd`
    fricas_top_srcdir=`cd "$srcdir" && pwd`
     ;;
esac

AC_SUBST(fricas_top_srcdir)

if echo "$fricas_pwd" | grep ' ' ; then
    AC_MSG_WARN([Please make sure that name of source directory])
    AC_MSG_WARN([and name of build directory does not contain spaces.])
    AC_MSG_ERROR([Name of build directory contains space.])
fi
if echo "$srcdir" | grep ' ' || echo "fricas_top_srcdir" | grep ' ' ; then
    AC_MSG_WARN([Please make sure that name of source directory])
    AC_MSG_WARN([and name of build directory does not contain spaces.])
    AC_MSG_ERROR([Name of source directory contains space.])
fi

## Where tools for the build machine are built
fricas_builddir=$fricas_pwd/build/$build
AC_SUBST(fricas_builddir)
fricas_build_bindir=$fricas_builddir/bin
AC_SUBST(fricas_build_bindir)

## Prefix for the staging target inistallation directory
fricas_targetdir=$fricas_pwd/target/$target
AC_SUBST(fricas_targetdir)
if test $build != $target; then
   AC_MSG_WARN([Cross build is not supported.])
   AC_MSG_WARN([Please notify fricas-devel@googlegroups.com if you succeed.])
fi

## Accumulate list of utils needed for the build platform
## It is vital that noweb is present in the build environement.
fricas_all_prerequisites=

case $build in
    *linux*)
        # GNU/Linux systems come equipped with GNU Make, called `make'
        FRICAS_CHECK_PROGS([MAKE], [make], [Make utility missing.])
        ;;
    *)
        # Other systems tend to spell it `gmake' and such
        FRICAS_CHECK_PROGS([MAKE], [gmake make],
                       [Make utility missing.])
        if ! $MAKE --version | grep 'GNU' 2>/dev/null; then
            AC_MSG_ERROR([FriCAS build system needs GNU Make.])
        fi
        ;;
esac

## Make sure the C compiler is from GCC
AC_PROG_CC
if test x$GCC != xyes; then
    AC_MSG_ERROR([We need a C compiler from GCC])
fi
# fricas_cflags="-O2 -Wall -D_GNU_SOURCE"
# AC_SUBST(fricas_cflags)

## What is the extension of object files on this platform?
AC_OBJEXT

AC_PROG_INSTALL
# AC_PROG_LN_S
FRICAS_CHECK_PROG([TOUCH], [touch], ['touch' program is missing.])
AC_CHECK_PROGS([MKTEMP], [mktemp])

case $build in
     *-solaris9)
        FRICAS_CHECK_PROG([AWK], [gawk], [FriCAS needs gawk])

        FRICAS_CHECK_PROG([TAR], [gtar], [FriCAS needs gtar])
        ;;

      *)
        AC_PROG_AWK

        FRICAS_CHECK_PROGS([TAR], [gtar tar], [FriCAS needs a tar program])

        ;;
esac

AC_PROG_RANLIB
FRICAS_CHECK_PROG([AR], [ar], [program 'ar' is missing])

AC_PATH_PROG([LATEX], [latex])
AC_CHECK_PROGS([MAKEINDEX], [makeindex])

if test -z $AWK_FOR_TARGET ; then
   AWK_FOR_TARGET=`which $AWK`
fi

AC_SUBST(AWK_FOR_TARGET)

# Shell needed to run src/algebra/boot-sum.sh
case $build in
     *-solaris*)
        AC_PATH_PROG([AUTO_GOOD_SHELL], [bash])
        test -n "$AUTO_GOOD_SHELL" || AC_MSG_ERROR([FriCAS needs bash])
        ;;
     *)
        # For now we assume that /bin/sh always exists
        AUTO_GOOD_SHELL=/bin/sh
        ;;
esac

AC_SUBST(AUTO_GOOD_SHELL)

## -------------------------
## -- Which noweb to use? --
## -------------------------
##

## The logic is as follows.
## No "--with-noweb-tarball" option:
##    Check for "notangle" and "noweave" in the PATH.
##    If not present, but $fricas_top_srcdir/zips/noweb-2.10a.tgz is
##    configure to build the noweb binaries.
##    Error, if neither noweb binaries or .tgz could not be found.
## "--without-noweb-tarball" or "--with-noweb-tarball=no":
##    notangle and noweave must be found in the PATH
##    Error if missing.
## "--with-noweb-tarball" or "--with-noweb-tarball=yes":
##    Look for $fricas_top_srcdir/zips/noweb-2.10a.tgz and extract
##    and compile noweb.
##    Error, if .tgz cannot be found.
## "--with-noweb-tarball=/path/to/noweb-2.10a.tgz":
##    Like above, but with an explicit path to the noweb tarball.
##    Error, if the .tgz is not present.

internal_noweb="$fricas_top_srcdir/zips/noweb-2.10a.tgz"
AC_ARG_WITH([noweb-tarball],
    [AS_HELP_STRING([--with-noweb-tarball],
                    [specify place of the noweb-2.10a.tgz tarball])],
    # Action if --with-noweb-tarball is given.
    [case $withval in
        yes) if test -f "$internal_noweb" ; then
                 fricas_build_noweb=yes
                 noweb_tarball="$internal_noweb"
             else
                 AC_MSG_ERROR([--with-noweb-tarball specified, but noweb tarball is not present])
             fi  ;;
        no)  fricas_build_noweb=no ;;
        *)   if test -f "$withval"; then
                 fricas_build_noweb=yes
                 noweb_tarball="$withval"
             else
                 AC_MSG_ERROR([Could not find the file "$withval".])
             fi ;;
    esac],
    # Action if --with-noweb-tarball is not given.
    [noweb_tarball=; fricas_build_noweb=]
)

## Check for notangle and noweb if we are not explicitly told
## to build noweb from FriCAS sources.
if test x$fricas_build_noweb != xyes ; then
    AC_CHECK_PROGS([NOTANGLE], [notangle])
    AC_CHECK_PROGS([NOWEAVE], [noweave])

    ## In case noweb is missing we need to build our own.
    if test -z $NOTANGLE ; then
        if test x$fricas_build_noweb = xno ; then
           AC_MSG_ERROR([noweb utils are missing but --without-noweb is specified])
        elif test -f "$internal_noweb" ; then
           fricas_build_noweb=yes
           noweb_tarball="$internal_noweb"
        else
           AC_MSG_ERROR([noweb utils and noweb sources missing])
        fi
    fi
fi

if test x$fricas_build_noweb = xyes ; then
    fricas_all_prerequisites="$fricas_all_prerequisites all-noweb"
    echo "fricas_build_bindir = $fricas_build_bindir"
    NOTANGLE=$fricas_build_bindir/notangle
    NOWEAVE=$fricas_build_bindir/noweave
fi
AC_SUBST(NOTANGLE)
AC_SUBST(NOWEAVE)
AC_SUBST(noweb_tarball)


## ------------------------
## -- Which Lisp to use? --
## ------------------------
##
## We will default to SBCL later, if no lisp implementation is specified.
fricas_lisp=
fricas_lisp_flavor=unknown
AC_ARG_WITH([lisp],
            [AS_HELP_STRING([--with-lisp=L], [use L as Lisp platform])],
            [fricas_lisp=$withval])
## Default to SBCL if no lisp given
if test -z $fricas_lisp ; then
    AC_PATH_PROG([FRICAS_LISP], [sbcl])
    if test -z $FRICAS_LISP ; then
        AC_MSG_ERROR([sbcl not found and no Lisp specified.
                      Install supported Lisp implementation and
                      use --with-lisp option to tell FriCAS
                      how to invoke your Lisp])
    fi
    fricas_lisp=$FRICAS_LISP
else
    ## Honor use of Lisp image specified on command line
    FRICAS_LISP=$fricas_lisp
    AC_SUBST(FRICAS_LISP)
    :
fi

AC_ARG_WITH([lisp-flavor],
            [AS_HELP_STRING([--with-lisp-flavor=F],
                           [obsolete and ignored])],
            [AC_MSG_WARN([--with-lisp-flavor is ignored now])]
           )

AC_MSG_CHECKING([Lisp implementation])
rm -f config_cl.out
echo '(load "'"$fricas_top_srcdir"'/config.lisp")' | $fricas_lisp

if test x$fricas_lisp_flavor = xunknown ; then
      fricas_lisp_flavor=`sed -n -e 's/fricas_lisp_flavor=//p' config_cl.out`
fi

if test -z "$fricas_lisp_flavor" ; then
    AC_MSG_ERROR([Unable to determine Lisp flavor])
fi

AC_MSG_RESULT([$fricas_lisp_flavor])

fricas_fasl_type=`sed -n -e 's/fricas_fasl_type=//p' config_cl.out`
fricas_lisp_version=`sed -n -e 's/fricas_lisp_version=//p' config_cl.out`

if test -z "$fricas_lisp_version" ; then
    AC_MSG_WARN([Unable to determine Lisp version.])
else
   case $fricas_lisp_flavor in
     clisp)
        if echo $fricas_lisp_version | grep '^[[0-1]]\.' > /dev/null || \
           echo $fricas_lisp_version | grep '^2\.[[0-3]]' > /dev/null ; then
             AC_MSG_ERROR([clisp $fricas_lisp_version is too old])
        fi
        ;;
     ecl)
        case $fricas_lisp_version in
           0.*)
              case $fricas_lisp_version in
                 0.9j)
                    AC_MSG_WARN([You use old ecl, please consider upgrade])
                   ;;
                 0.9l)
                   ;;
                 *)
                    AC_MSG_ERROR([ecl $fricas_lisp_version is too old])
                   ;;
               esac
             ;;
         esac
        ;;
     gcl)
        case $fricas_lisp_version in
           2.6.7|2.6.8)
             ;;
           2.7.0)
             AC_MSG_WARN([FriCAS needs porting to gcl $fricas_lisp_version
                          please proceed with caution])
             ;;
           *)
             AC_MSG_ERROR([gcl $fricas_lisp_version is too old])
             ;;
        esac
        ;;
     openmcl)
        if echo $fricas_lisp_version | grep '^0\.' > /dev/null || \
           echo $fricas_lisp_version | grep '^1\.[[01]]' > /dev/null ; then
             AC_MSG_ERROR([openmcl $fricas_lisp_version is too old])
        fi
        ;;
     sbcl)
        if echo $fricas_lisp_version | grep '^0\.' > /dev/null || \
           echo $fricas_lisp_version | grep '^1\.0.[[0-6]]$' > /dev/null ; then
             AC_MSG_ERROR([sbcl $fricas_lisp_version is too old])
        fi
        case $fricas_lisp_version in
           1.0.29)
             AC_MSG_ERROR([sbcl $fricas_lisp_version has a bug which does not allow building FriCAS.
Please use different sbcl version])
                  ;;
        esac
        ;;
   esac
fi

AC_SUBST(fricas_lisp_flavor)

## Check if we need shared libspad library.
case $fricas_lisp_flavor in
    sbcl|cmucl|clisp|openmcl|lispworks)
       LIBSPAD_SO_TARGET="libspad_so"
        ;;
    *)
       LIBSPAD_SO_TARGET=""
       ;;
esac
case $target in
    *darwin*)
       SOLIB_FLAGS="-dynamiclib -single_module"
         ;;
    *)
       SOLIB_FLAGS="-shared"
         ;;
esac
AC_SUBST(LIBSPAD_SO_TARGET)
AC_SUBST(SOLIB_FLAGS)

## In case our underlying lisp is SBCL, we provide a wrapper to enable
## GMP bignums in SBCL.
GMP_WRAP_SO_TARGET=""
AC_ARG_ENABLE([gmp],
            [AS_HELP_STRING([--enable-gmp], [use gmp for sbcl bignums])],
            [case $enableval in
               yes)
                 if test $fricas_lisp_flavor = sbcl ; then
                    case $(uname -m) in
                      i386|i486|i586|i686|x86_64)
                       GMP_WRAP_SO_TARGET=gmp_wrap_so
                         ;;
                      *)
                       AC_MSG_ERROR([--enable-gmp is valid only when using sbcl on AMD/Intel processors])
                         ;;
                    esac
                 elif test $fricas_lisp_flavor = openmcl ; then
                    case $(uname -m) in x86_64)
                      GMP_WRAP_SO_TARGET=gmp_wrap_so
                        ;;
                      *)
                       AC_MSG_ERROR([--enable-gmp is valid only when using Closure CL in 64-bit AMD/Intel processors])
                    esac
                 else
                    AC_MSG_ERROR([--enable-gmp is valid only when using sbcl on AMD/Intel processors])

                 fi ;;
               no) ;;
               *) AC_MSG_ERROR([--enable-gmp only allows yes or no as an argument]) ;;
             esac])
AC_SUBST(GMP_WRAP_SO_TARGET)
## If it is possible to enable the gmp wrapper, we have to check
## whether gmp is actually installed.
AS_IF([test -n "$GMP_WRAP_SO_TARGET"],[
  # temporarily save LDFLAGS
  fricas_save_LDFLAGS=$LDFLAGS
  # ask for shared library in AC_CHECK_LIB
  LDFLAGS="$LDFLAGS -fPIC $SOLIB_FLAGS"
  AC_CHECK_LIB(gmp, __gmpz_init, [], [AC_MSG_ERROR(
    [GNU MP not found, see http://swox.com/gmp])])
  AC_CHECK_HEADERS([gmp.h], [], [AC_MSG_ERROR(
    [gmp.h not found, see http://swox.com/gmp])])
  # reset LDFLAGS
  LDFLAGS=$fricas_save_LDFLAGS
  GMP_LIB=-lgmp
])
AC_SUBST(GMP_LIB)
## The following is a horrible to arrange for GCL to successfully
## rebuild symbol tables with "rsym" on Windows platform.  It should
## go away as soon as GCL upstream is fixed.
case $fricas_lisp_flavor,$target in
    gcl,*mingw*)
        fricas_gcl_rsym_hack='d=`echo "(format nil \"~a\" si::*system-directory*)" | $(FRICAS_LISP) | grep "/gcl.*/" | sed -e "s,\",,g"`; cp $$d/rsym$(EXEEXT) .'
        ;;
    *)
        fricas_gcl_rsym_hack=':'
        ;;
esac
AC_SUBST(fricas_gcl_rsym_hack)
## How are we supposed to tell the Lisp system to eval an expression
## in batch mode?  What is the extension of a compiled Lisp file?
case $fricas_lisp_flavor in
    gcl)
       fricas_quiet_flags='-batch'
       fricas_eval_flags='-eval'
       ;;
    ecl)
       fricas_quiet_flags='-norc'
       fricas_eval_flags='-eval'
       ;;
    sbcl)
       fricas_quiet_flags='--noinform --noprint --no-userinit'
       fricas_eval_flags='--eval'
       ;;
    cmucl)
       fricas_quiet_flags='-batch -noinit'
       fricas_eval_flags='-eval'
       ;;
    clisp)
       fricas_quiet_flags='--quiet -norc'
       fricas_eval_flags='-x'
       ;;
    openmcl)
       fricas_quiet_flags=
       fricas_eval_flags='--eval'
       ;;
    lispworks)
       fricas_quiet_flags=
       fricas_eval_flags='-init - -siteinit - -eval'
       ;;
    *) AC_MSG_ERROR([We do not know how to build FriCAS this $fricas_lisp]) ;;
esac
AC_SUBST(fricas_quiet_flags)

if test -z $fricas_fasl_type; then
   AC_MSG_ERROR([Could not determine extension for compiled Lisp files])
fi
AC_MSG_RESULT([$fricas_fasl_type])

fricas_fasl_type2=$fricas_fasl_type
case $fricas_lisp_flavor in
    ecl)
       fricas_fasl_type="o"
     ;;
    *)
     ;;
esac
AC_SUBST(fricas_fasl_type)
AC_SUBST(fricas_fasl_type2)

fricas_algebra_optimization=nil

AC_ARG_ENABLE([algebra-optimization],
            [AS_HELP_STRING([--enable-algebra-optimization=S],
                            [use S as Lisp optimize declaration for compiling
                             the algebra])],
            [fricas_algebra_optimization=$enableval])

AC_SUBST(fricas_algebra_optimization)

AC_SUBST(fricas_all_prerequisites)

# FIXME: Move this out of here.
# The core runtime is always built.
fricas_c_runtime=core
AC_SUBST(fricas_c_runtime)

AC_CHECK_HEADERS([signal.h],
                 [],
                 [AC_MSG_ERROR([FriCAS needs signal support.])])
AC_CHECK_DECLS([sigaction], [], [],
               [#include <signal.h>])
AC_CHECK_HEADERS([sys/stat.h],
                 [],
                 [AC_MSG_ERROR([FriCAS needs <sys/stat.h>.])])
AC_CHECK_HEADERS([unistd.h], [],
                 [AC_MSG_ERROR([FriCAS needs <unistd.h>])])
AC_CHECK_DECLS([getuid, geteuid, getgid, getegid], [], [],
               [#include <unistd.h>])

AC_CHECK_DECLS([kill], [], [],
               [#include <signal.h>])
case $host in
    *mingw*)
        AC_CHECK_HEADERS([winsock2.h],
                        [fricas_host_has_socket=yes],
                        [])
        fricas_c_runtime_extra="-lwsock32"
        ;;
    *)
        AC_CHECK_HEADERS([sys/socket.h],
                         [fricas_host_has_socket=yes],
                         [])
        ;;
esac

AC_TRY_COMPILE([
#include <sys/socket.h>
   ], [
int flag = MSG_NOSIGNAL;
   ], 
   [AC_DEFINE([HAVE_MSG_NOSIGNAL], [1], [Host has MSG_NOSIGNAL])],[])

AC_TRY_COMPILE([
#include <errno.h>
#include <sys/socket.h>
   ], [
int code = EPIPE;
   ],
   [AC_DEFINE([HAVE_EPIPE], [1], [Host has EPIPE])],[])


if test x$fricas_host_has_socket != xyes; then \
    AC_MSG_ERROR([FriCAS needs suport for sockets.])
fi

## solaris-based systems tend to hide the socket library.
case $host in
    *solaris*)
        AC_SEARCH_LIBS([accept], [socket],
            [fricas_c_runtime_extra="-lsocket"],
            [AC_MSG_ERROR([socket library not found])])
        ;;
    *) ;;
esac

AC_EGREP_CPP([has_af_local],
                [
#if HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#else
#  include <winsock2.h>
#endif
#ifdef AF_LOCAL
   has_af_local
#endif
                 ],
                 [AC_DEFINE([HAVE_AF_LOCAL], [1], [Host has AF_LOCAL])])


AC_EGREP_CPP([has_af_unix],
                [
#if HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#else
#  include <winsock2.h>
#endif
#ifdef AF_UNIX
   has_af_unix
#endif
                 ],
                 [AC_DEFINE([HAVE_AF_UNIX], [1], [Host has AF_UNIX])])


AC_CHECK_HEADERS([util.h],
                   [],
                   [AC_CHECK_HEADERS([pty.h], [], [])
                   ]) # HAVE_UTIL_H or HAVE_PTY_H

AC_CHECK_DECL([openpty],
                 [AC_DEFINE([HAVE_OPENPTY_DECL], [1],
                              [openpty is declared])], [],
                 [
#if defined(HAVE_UTIL_H)
# include <util.h>
#elif defined(HAVE_PTY_H)
# include <pty.h>
#endif
                 ]) # HAVE_OPENPTY_DECL

AC_CHECK_LIB([c], [openpty],
       [AC_DEFINE([HAVE_OPENPTY], [1],
	          [openpty is available])
       ],
       AC_CHECK_LIB([util], [openpty],
                 [AC_DEFINE([HAVE_OPENPTY], [1],
                            [openpty is available])
                   fricas_c_runtime_extra="${fricas_c_runtime_extra} -lutil"
                 ],
                 AC_CHECK_LIB([bsd], [openpty],
                    [AC_DEFINE([HAVE_OPENPTY], [1],
                               [openpty is available])
                      fricas_c_runtime_extra="${fricas_c_runtime_extra} -lbsd"
                    ])
             )) # HAVE_OPENPTY

AC_SUBST(fricas_c_runtime_extra)

AC_CHECK_HEADERS([sys/wait.h])

if test x"$ac_cv_header_sys_wait_h" = xyes; then \
    AC_CHECK_DECLS([wait],
                   [],
                   [],
                   [#include <sys/wait.h>])
fi

AC_CHECK_DECLS([fork],
               [],
               [],
               [#include <unistd.h>])

if test x"$ac_cv_have_decl_fork" = xyes \
     -a x"$ac_cv_have_decl_wait" = xyes; then \
    fricas_c_runtime="$fricas_c_runtime terminal_io"
    fricas_src_all="$fricas_src_all all-sman all-clef"
    fricas_src_subdirs="$fricas_src_subdirs clef sman"
    FRICAS_MAKEFILE([src/clef/Makefile])
    FRICAS_MAKEFILE([src/sman/Makefile])
else
    AC_MSG_NOTICE([Superman component is disabled.])
fi

fricas_src_all="all-axiomsys $fricas_src_all"
AC_PATH_XTRA
## Output directives for the C compiler
AC_SUBST(X_CLFAGS)
## Output directives for the linker
AC_SUBST(X_LIBS)
## Output any extra libraries required by X11
AC_SUBST(X_EXTRA_LIBS)

## Finally, output the list of libraries that need to appear before -lX11
## Some part of FriCAS depends on Xpm.  That library has kind uncertain
## future.  At some point in the past, it was deprecated, to be
## replaced by xpm-nox; then came back again.  So, its support may
## vary from system to system.  For the moment, we do a quick sanity
## check and disable X11 if Xpm is not present.  Long term, FriCAS should
## get rid of dependence on Xpm.  Another possibility is to (optionally)
## bundle Xpm source tarball and build Xpm if needed.

if test -z $no_x; then
  AC_CHECK_LIB([Xpm], [XpmReadFileToImage],
              [fricas_has_xpm=yes],
              [fricas_has_xpm=no],
              [$X_LIBS -lX11])
else
  fricas_has_xpm=no
fi

X_PRE_LIBS="-lXpm $X_PRE_LIBS"
AC_SUBST(X_PRE_LIBS)

## If the system supports X11, then build graphics and hyperdoc
if test x"$fricas_has_xpm" = xyes; then
    fricas_c_runtime="$fricas_c_runtime graphics"
    fricas_src_all="$fricas_src_all all-graph"
    fricas_src_subdirs="$fricas_src_subdirs graph"
    FRICAS_MAKEFILE([src/graph/Makefile])
    FRICAS_MAKEFILE([src/graph/Gdraws/Makefile])
    FRICAS_MAKEFILE([src/graph/view2D/Makefile])
    FRICAS_MAKEFILE([src/graph/view3D/Makefile])
    FRICAS_MAKEFILE([src/graph/viewAlone/Makefile])
    FRICAS_MAKEFILE([src/graph/viewman/Makefile])
else
    AC_MSG_NOTICE([The Graphics and HyperDoc components are disabled.])
fi
AC_SUBST(fricas_src_all)
if test x"$fricas_has_xpm" = xyes; then
   AC_CHECK_HEADER([regex.h],
                   [fricas_src_all="$fricas_src_all all-hyper all-paste"
                    fricas_src_subdirs="$fricas_src_subdirs hyper paste"
                    FRICAS_MAKEFILE([src/paste/Makefile])],
                   [AC_MSG_NOTICE([HyperDoc is disabled.])])
fi
# We need the Makefile (for util.ht) even if we do not build HyperDoc.
FRICAS_MAKEFILE([src/hyper/Makefile])

BASE=""
AC_SUBST(BASE)
AXIOM=${BASE}${fricas_targetdir}
AC_SUBST(AXIOM)

CCF="-O2 -Wall -D_GNU_SOURCE"
LDF=

case $target in
    *bsd*|*dragonfly*)
        AC_DEFINE([BSDplatform], [], [BSD flavour])
        CCF="-O2 -pipe -Wall -D_GNU_SOURCE -I/usr/local/include"
        LDF="-L/usr/local/lib"
        ;;
    windows)
        AC_DEFINE([MSYSplatform], [], [MS])
        SRCDIRS=bootdir interpdir sharedir algebradir etcdir docdir inputdir
        ;;
    *linux*)
        AC_DEFINE([LINUXplatform], [], [Linux flavour])
        ;;
    *cygwin*)
        AC_DEFINE([CYGWINplatform], [], [Cygwin flavour])
        ;;
    *solaris*)
        AC_DEFINE([SUNplatform], [], [SunOS flavour])
        ;;
    *darwin*)
        AC_DEFINE([MACOSXplatform], [], [MACOX flavour])
        CCF="-O2 -Wall -D_GNU_SOURCE \
            -I/usr/include -I/usr/include/sys"
        ;;
esac

AC_SUBST(CCF)
AC_SUBST(LDF)

FRICAS_MAKEFILE([Makefile])
FRICAS_MAKEFILE([src/Makefile])
FRICAS_MAKEFILE([src/lib/Makefile])
FRICAS_MAKEFILE([src/lisp/Makefile])
FRICAS_MAKEFILE([src/boot/Makefile])
FRICAS_MAKEFILE([src/interp/Makefile])
FRICAS_MAKEFILE([src/share/Makefile])
FRICAS_MAKEFILE([src/algebra/Makefile])
FRICAS_MAKEFILE([src/input/Makefile])
FRICAS_MAKEFILE([src/etc/Makefile])


###################################################################
## The following section deals with the generation of the
## aldor interface.
##

## This handles the cases if one of the following options is given on the
## command line. If none is given, then we will skip aldor interface.
## --enable-aldor, --enable-aldor=yes:
##   User requires to build the aldor interface. In this case,
##   any missing prerequisite counts as error (not just warning).
## --enable-aldor=no, --disable-aldor:
##   User asks to skip the build of the aldor interface.
## --enable-aldor=SOMETHING DIFFERENT FROM yes/no
##   The value is interpreted as a directory (ALDORSOURCE) of a local
##   checkout of the aldor sources (revision 23).
##   As a sanity check we test for the file
##   "$(ALDORSOURCE)/aldor/lib/libax0/axiom.as".
##   If the file is not there, then the user made a mistake (which is an error).
BUILD_ALDOR_INTERFACE=no
## Default is to fetch the sources at make time from the web.
ALDORSOURCE=https://svn.origo.ethz.ch/algebraist/trunk
AC_ARG_ENABLE(aldor,
  [AS_HELP_STRING([--enable-aldor],
                  [build an interface to the Aldor compiler, (yes/no/directory of trunk checkout)])],
  [case $enableval in
    yes|no) BUILD_ALDOR_INTERFACE=$enableval ;;
    *) AS_IF([test -r "$enableval/aldor/lib/libax0/axiom.as"],
           [ALDORSOURCE=$enableval
            BUILD_ALDOR_INTERFACE=yes],
           [AC_MSG_ERROR([only yes/no or a directory allowed as value for --enable-aldor])]);;
   esac])
AC_SUBST(ALDORSOURCE)

###################################################################
## If one of the Aldor related tests fail we should warn and
## not build the Aldor interface.

if test x"$BUILD_ALDOR_INTERFACE" = xyes ; then
  ## check whether Aldor is available
  AC_CHECK_PROGS([ALDOR], [aldor])
  if test -z $ALDOR; then
    AC_MSG_WARN([Aldor not found.])
    AC_MSG_ERROR([prerequisites for Aldor interface missing])
    BUILD_ALDOR_INTERFACE=no
  fi

  # The following is only necessary to get some files that do not yet
  # have a free license.
  # Not having svn, curl, or wget is not a show stopper, because the files
  # can be obtained manually via a webbrowser.
  AC_CHECK_PROGS([SVN], [svn])
  AC_CHECK_PROGS([CURL], [curl])
  AC_CHECK_PROGS([WGET], [wget])

  ## check for $ALDORROOT/include/aldor.conf
  if test -z "$ALDORROOT" || ! test -f "$ALDORROOT/include/aldor.conf"; then
    AC_MSG_WARN([The environment variable ALDORROOT is not set.])
    AC_MSG_ERROR([prerequisites for Aldor interface missing])
    BUILD_ALDOR_INTERFACE=no
  fi

  ## check for $ALDORROOT/lib/libfoam.al (needed for aldor interface)
  if ! test -f "$ALDORROOT/lib/libfoam.al"; then
    AC_MSG_WARN([Couldn't find libfoam.al.])
    AC_MSG_ERROR([prerequisites for Aldor interface missing])
    BUILD_ALDOR_INTERFACE=no
  fi
fi

###################################################################
# The installation is currently not done by recursively calling
# install in the src subdirectories, so we have to hardcode the
# intallation of the aldor interface in Makefile.in and let a
# variable decide whether or not the interface stuff should be
# installed.
AC_SUBST(BUILD_ALDOR_INTERFACE)

if test x"$BUILD_ALDOR_INTERFACE" = xyes ; then
  AC_MSG_NOTICE([Aldor interface will be built.])
  fricas_src_subdirs="$fricas_src_subdirs aldor"
  fricas_src_all="$fricas_src_all all-aldor"
else
  AC_MSG_WARN([Aldor interface will not be built.])
fi
FRICAS_MAKEFILE([src/aldor/Makefile])
FRICAS_MAKEFILE([src/aldor/Makefile2])
FRICAS_MAKEFILE([src/aldor/Makefile3])
###################################################################


###################################################################
## The following section deals with running fricas inside emacs.
## The sources are expected in ${srcdir}/contrib/emacs.
## The shell script efricas will be copied to ${bindir}.
## The files fricas*.el should probably go to
#    emacsdatadir=$datadir/emacs/site-lisp
## but will currently be put into
emacsdatadir=$libdir/fricas/emacs
AC_SUBST(emacsdatadir)
FRICAS_MAKEFILE([contrib/emacs/Makefile])
###################################################################




## We now generate the "document" script and support files at configure time.
## We put them in the build directory because they are intended to be
## build support utils only.
AC_CONFIG_FILES(build/scripts/document:$srcdir/src/scripts/document.in, \
                [chmod +x build/scripts/document])

AC_OUTPUT

## Generate rules to extrad SPAD type definitions from pamphlets.
echo "extracting list of SPAD type definitions"
(cd $srcdir/src/algebra;
   . ../scripts/find-algebra-files) > src/algebra/tmp-extract-spad.mk
$srcdir/config/move-if-change \
    src/algebra/tmp-extract-spad.mk src/algebra/extract-spad.mk

## Configure the use of cached files.
if test -f $srcdir/src/algebra/use_lisp ; then
    # Sanity check
    if test -f $srcdir/src/algebra/A1AGG.lsp ; then
        touch src/algebra/use_lisp
    else
        AC_MSG_ERROR([Sources look corrupted])
    fi
fi

if test -f $srcdir/src/paste/copy_nphts ; then
    # Sanity check
    if test -f $srcdir/src/paste/SYMBOL.pht ; then
        touch src/paste/copy_nphts
    fi
fi

if test -f $srcdir/src/paste/copy_gphts ; then
    # Sanity check
    if test -d $srcdir/src/paste/mobius.VIEW ; then
        touch src/paste/copy_gphts
    else
        AC_MSG_ERROR([Sources look corrupted])
    fi
fi

echo "Type '${MAKE}' (without quotes) to build FriCAS"
