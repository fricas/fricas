sinclude(config/axiom.m4)
AC_INIT([FriCAS], [2009-09-17], 
        [fricas-devel@googlegroups.com])
axiom_top_srcdir=`cd "$srcdir" && pwd`
if pwd | grep ' ' ; then
    AC_MSG_WARN([Please make sure that name of source directory])
    AC_MSG_WARN([and name of build directory does not contain spaces.])
    AC_MSG_ERROR([Name of build directory contains space.])
fi
if echo "$srcdir" | grep ' ' || echo "axiom_top_srcdir" | grep ' ' ; then
    AC_MSG_WARN([Please make sure that name of source directory])
    AC_MSG_WARN([and name of build directory does not contain spaces.])
    AC_MSG_ERROR([Name of source directory contains space.])
fi
AC_SUBST(axiom_top_srcdir)
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

axiom_src_subdirs="lib lisp boot interp share algebra input etc"
AC_SUBST(axiom_src_subdirs)

AC_CANONICAL_SYSTEM

## Where tools for the build machine are built
axiom_builddir=`pwd`/build/$build
AC_SUBST(axiom_builddir)
axiom_build_bindir=$axiom_builddir/bin
AC_SUBST(axiom_build_bindir)

## Prefix for the staging target inistallation directory
axiom_targetdir=`pwd`/target/$target
AC_SUBST(axiom_targetdir)
if test $build != $target; then
   AC_MSG_WARN([Cross build is not supported.])
   AC_MSG_WARN([Please notify fricas-devel@googlegroups.com if you succeed.])
fi
## Where tools for the build platform are sequestered
axiom_build_sharedir=$axiom_builddir/share

## Accumulate list of utils needed for the build platform
## It is vital that noweb is present in the build environement.
axiom_all_prerequisites=

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
axiom_cflags="-O2 -fno-strength-reduce -Wall -D_GNU_SOURCE"
AC_SUBST(axiom_cflags)

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

axiom_build_noweb=
AC_ARG_WITH([included-noweb],
            [  --with-included-noweb   build noweb from included sources],
            [case $withval in
                yes) if test -f "$axiom_top_srcdir/zips/noweb-2.10a.tgz" ; then
                         axiom_build_noweb=yes
                     else
                         AC_MSG_ERROR([--with-included-noweb specified, but no noweb sources is present])
                     fi  ;;
                no) axiom_build_noweb=no ;;
                *) AC_MSG_ERROR([erroneous value for --with-included-noweb]) ;;
             esac])
## Check for notangle and noweb if we are not explicitly told
## to build noweb from FriCAS sources.
if test x$axiom_build_noweb != xyes ; then
    AC_CHECK_PROGS([NOTANGLE], [notangle])
    AC_CHECK_PROGS([NOWEAVE], [noweave])

    ## In case noweb is missing we need to build our own.
    if test -z $NOTANGLE -o -z $NOWEAVE ; then
        if test x$axiom_build_noweb = xno ; then
           AC_MSG_ERROR([noweb utils are missing but --without-included-noweb is specified])
        elif test -f "$axiom_top_srcdir/zips/noweb-2.10a.tgz" ; then
           axiom_build_noweb=yes
        else
           AC_MSG_ERROR([noweb utils and noweb sources missing])
        fi
    fi
fi

if test x$axiom_build_noweb = xyes ; then
    axiom_all_prerequisites="$axiom_all_prerequisites all-noweb"
    echo "axiom_build_bindir = $axiom_build_bindir"
    NOTANGLE=$axiom_build_bindir/notangle
    NOWEAVE=$axiom_build_bindir/noweave
fi
AC_SUBST(NOTANGLE)
AC_SUBST(NOWEAVE)


## ------------------------
## -- Which Lisp to use? --
## ------------------------
##
## We will default to GCL later, if no lisp implementation is specified.
fricas_lisp=
fricas_lisp_flavor=unknown
AC_ARG_WITH([lisp], 
            [  --with-lisp=L           use L as Lisp platform],
              [fricas_lisp=$withval])
## If --enable-gcl is specified, we need to check for coonsistency
axiom_include_gcl=
if test -z $fricas_lisp ; then
    AC_ARG_ENABLE([gcl], 
                  [  --enable-gcl            build GCL from FriCAS source],
                  [case $enableval in
                       yes|no) axiom_include_gcl=$enableval ;;
                       *) AC_MSG_ERROR([erroneous value for --enable-gcl]) ;;
                   esac])
fi
## We need to build our own GCL is none is avalaible
if test x"$axiom_include_gcl" != xyes ; then
  if test -z $fricas_lisp ; then
    AC_PATH_PROG([FRICAS_LISP], [sbcl])
    fricas_lisp=$FRICAS_LISP
  else
    ## Honor use of Lisp image specified on command line
    FRICAS_LISP=$fricas_lisp
    AC_SUBST(FRICAS_LISP)
    :
  fi
fi
## Coherence check for GCL inclusion.
case $axiom_include_gcl,$fricas_lisp in
    ,|no,|yes*)
       ## It doesn't make sense not to include GCL when no Lisp image
       ## is available.  Give up.
       if test $axiom_include_gcl,$FRICAS_LISP = no,; then
           AC_MSG_ERROR([--disable-gcl specified but no GCL image found])
       fi

       ## No Lisp image was specified and none was available from
       ## the build environment; build GCL from FriCAS source.
       if test -f $axiom_top_srcdir/gcl/configure.in ; then
         FRICAS_LISP='$(axiom_build_bindir)/gcl'
         axiom_all_prerequisites="$axiom_all_prerequisites all-gcl"
         axiom_include_gcl=yes
         fricas_lisp_flavor=gcl
         fricas_fasl_type=o
       else
          AC_MSG_ERROR([GCL and GCL sources missing, see INSTALL])
       fi
       ;;
    yes,*)
       AC_MSG_ERROR([--with-lisp=$fricas_lisp conflicts with --enable-gcl])
       ;;
esac

AC_ARG_WITH([lisp-flavor],
            [  --with-lisp-flavor=F    your Lisp is brand F
                          where F if one F gcl clisp sbcl ecl openmcl
                          should be autodetected],
            [case $withval in
            gcl|clisp|sbcl|ecl|openmcl)
                   fricas_lisp_flavor=$withval
                   ;;
            *)
              AC_MSG_ERROR([--with-lisp-flavor requires one of gcl clisp sbcl ecl openmcl]);;
            esac])
if test x"$axiom_include_gcl" != xyes ; then
  AC_MSG_CHECKING([Lisp implementation])
  rm -f config_cl.out
  echo '(load "'"$axiom_top_srcdir"'/config.lisp")' | $fricas_lisp

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
fi
AC_SUBST(axiom_include_gcl)
AC_SUBST(fricas_lisp_flavor)

## Check if we need shared libspad library.
case $fricas_lisp_flavor in
    sbcl|clisp|openmcl)
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

## The following is a horrible to arrange for GCL to successfully
## rebuild symbol tables with "rsym" on Windows platform.  It should
## go away as soon as GCL upstream is fixed.
case $fricas_lisp_flavor,$target in
    gcl,*mingw*)
        axiom_gcl_rsym_hack='d=`echo "(format nil \"~a\" si::*system-directory*)" | $(FRICAS_LISP) | grep "/gcl.*/" | sed -e "s,\",,g"`; cp $$d/rsym$(EXEEXT) .'
        ;;
    *) 
        axiom_gcl_rsym_hack=':'
        ;;
esac
AC_SUBST(axiom_gcl_rsym_hack)
## How are we supposed to tell the Lisp system to eval an expression
## in batch mode?  What is the extension of a compiled Lisp file?
case $fricas_lisp_flavor in
    gcl) 
       fricas_quiet_flags='-batch'
       axiom_eval_flags='-eval'
       ;;
    ecl) 
       fricas_quiet_flags='-norc'
       axiom_eval_flags='-eval'
       ;;
    sbcl) 
       fricas_quiet_flags='--noinform --noprint --no-userinit'
       axiom_eval_flags='--eval'
       ;;
    clisp) 
       fricas_quiet_flags='--quiet -norc'
       axiom_eval_flags='-x'
       ;;
    openmcl)
       fricas_quiet_flags=
       axiom_eval_flags='--eval'
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

AC_ARG_WITH([algebra-optimization],
            [  --with-algebra-optimization=S
                          use S as Lisp optimize declaration for compiling the 
                          algebra],
            [fricas_algebra_optimization=$withval])

AC_SUBST(fricas_algebra_optimization)

AC_SUBST(axiom_all_prerequisites)

# FIXME: Move this out of here.  
# The core runtime is always built.
axiom_c_runtime=core
AC_SUBST(axiom_c_runtime)

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
                        [axiom_host_has_socket=yes],
                        [])
        axiom_c_runtime_extra="-lwsock32"
        ;;
    *)
        AC_CHECK_HEADERS([sys/socket.h], 
                         [axiom_host_has_socket=yes],
                         [])
        ;;
esac
if test x$axiom_host_has_socket != xyes; then \
    AC_MSG_ERROR([FriCAS needs suport for sockets.])
fi

## solaris-based systems tend to hide the socket library.
case $host in
    *solaris*)
        AC_SEARCH_LIBS([accept], [socket],
            [axiom_c_runtime_extra="-lsocket"],
            [AC_MSG_ERROR([socket library not found])])
        ;;
    *) ;;
esac

AC_EGREP_HEADER([has_af_local],
                [#if HAVE_SYS_SOCKET_H
                 #  include <sys/socket.h>
                 #else
                 #  include <winsock2.h>
                 #endif
                 #ifdef AF_LOCAL
                   has_af_local
                 #endif],
                 [AC_DEFINE([HAVE_AF_LOCAL], [1], [Host has AF_LOCAL])])


AC_EGREP_HEADER([has_af_unix],
                [#if HAVE_SYS_SOCKET_H
                 #  include <sys/socket.h>
                 #else
                 #  include <winsock2.h>
                 #endif
                 #ifdef AF_UNIX
                   has_af_unix
                 #endif],
                 [AC_DEFINE([HAVE_AF_UNIX], [1], [Host has AF_UNIX])])


AC_CHECK_HEADERS([util.h],
                   [],
                   [AC_CHECK_HEADERS([pty.h], [], [])
                   ]) # HAVE_UTIL_H or HAVE_PTY_H

AC_CHECK_DECL([openpty],
                 [AC_DEFINE([HAVE_OPENPTY_DECL], [1], 
                              [openpty is declared])], [],
                 [#if defined(HAVE_UTIL_H)
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
                   axiom_c_runtime_extra="${axiom_c_runtime_extra} -lutil"
                 ],
                 AC_CHECK_LIB([bsd], [openpty],
                    [AC_DEFINE([HAVE_OPENPTY], [1],
                               [openpty is available])
                      axiom_c_runtime_extra="${axiom_c_runtime_extra} -lbsd"
                    ])
             )) # HAVE_OPENPTY

AC_SUBST(axiom_c_runtime_extra)

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
    axiom_c_runtime="$axiom_c_runtime terminal_io"
    axiom_src_all="$axiom_src_all all-sman all-clef"
    axiom_src_subdirs="$axiom_src_subdirs clef sman"
    AXIOM_MAKEFILE([src/clef/Makefile])
    AXIOM_MAKEFILE([src/sman/Makefile])
else
    AC_MSG_NOTICE([Superman component is disabled.])
fi

axiom_src_all="all-axiomsys $axiom_src_all"
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
              [axiom_has_xpm=yes],
              [axiom_has_xpm=no],
              [$X_LIBS -lX11])
else
  axiom_has_xpm=no
fi

X_PRE_LIBS="-lXpm $X_PRE_LIBS"
AC_SUBST(X_PRE_LIBS)

## If the system supports X11, then build graphics and hyperdoc
if test x"$axiom_has_xpm" = xyes; then
    axiom_c_runtime="$axiom_c_runtime graphics"
    axiom_src_all="$axiom_src_all all-graph"
    axiom_src_subdirs="$axiom_src_subdirs graph"
    AXIOM_MAKEFILE([src/graph/Makefile])
    AXIOM_MAKEFILE([src/graph/Gdraws/Makefile])
    AXIOM_MAKEFILE([src/graph/view2D/Makefile])
    AXIOM_MAKEFILE([src/graph/view3D/Makefile])
    AXIOM_MAKEFILE([src/graph/viewAlone/Makefile])
    AXIOM_MAKEFILE([src/graph/viewman/Makefile])
else
    AC_MSG_NOTICE([The Gaphics and HyperDoc components are disabled.])
fi
AC_SUBST(axiom_src_all)
if test x"$axiom_has_xpm" = xyes; then
   AC_CHECK_HEADER([regex.h], 
                   [axiom_src_all="$axiom_src_all all-hyper all-paste"
                    axiom_src_subdirs="$axiom_src_subdirs hyper paste"
                    AXIOM_MAKEFILE([src/paste/Makefile])],
                   [AC_MSG_NOTICE([HyperDoc is disabled.])])
fi
# We need the Makefile (for util.ht) even if we do not build HyperDoc.
AXIOM_MAKEFILE([src/hyper/Makefile])

case $host in
    *mingw*)
        BASE=`(cd /;pwd -W)`
        ;;
    *)
        BASE=""
        ;;
esac
AC_SUBST(BASE)
AXIOM=${BASE}${axiom_targetdir}
AC_SUBST(AXIOM)

# axiom_host_has_libbfd=
# AC_CHECK_HEADER([bfd.h])
# AC_HAVE_LIBRARY([bfd], [axiom_host_has_libbfd=yes])

# axiom_gcl_bfd_option=
# if test x"$ac_cv_header_bfd_h" = xyes \
#    && test x"$axiom_host_has_libbfd" = xyes; then
#    axiom_gcl_bfd_option="--disable-dynsysbfd"
# else
#     axiom_gcl_bfd_option="--disable-statsysbfd --enable-locbfd"
#fi

# Modern system seem to have bfd which is incompatible with gcl
axiom_gcl_bfd_option="--disable-dynsysbfd --disable-statsysbfd --enable-locbfd"

# axiom_gcl_mm_option="--enable-maxpage=256*1024"
axiom_gcl_x_option="--disable-tkconfig --disable-x --disable-xgcl"

PFL=
CCF="-O2 -fno-strength-reduce -Wall -D_GNU_SOURCE"
LDF=
LISP=lsp

case $target in
    *bsd*|*dragonfly*)
        AC_DEFINE([BSDplatform], [], [BSD flavour])
        CCF="-O2 -pipe -fno-strength-reduce -Wall -D_GNU_SOURCE -I/usr/local/include"
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
        CCF="-O2 -fno-strength-reduce -Wall -D_GNU_SOURCE \
            -I/usr/include -I/usr/include/sys"
        axiom_gcl_bfd_option="--disable-statsysbfd \
                                --enable-machine=powerpc-macosx"
        axiom_gcl_mm_option="--enable-vssize=65536*2"
        ;;
esac

GCLOPTS="$axiom_gcl_bfd_option $axiom_gcl_mm_option $axiom_gcl_x_option"

AC_SUBST(PLF)
AC_SUBST(CCF)
AC_SUBST(LDF)
AC_SUBST(LISP)
AC_SUBST(GCLOPTS)

AXIOM_MAKEFILE([Makefile])
AXIOM_MAKEFILE([src/Makefile])
AXIOM_MAKEFILE([src/lib/Makefile])
AXIOM_MAKEFILE([src/lisp/Makefile])
AXIOM_MAKEFILE([src/boot/Makefile])
AXIOM_MAKEFILE([src/interp/Makefile])
AXIOM_MAKEFILE([src/share/Makefile])
AXIOM_MAKEFILE([src/algebra/Makefile])
AXIOM_MAKEFILE([src/input/Makefile])
AXIOM_MAKEFILE([src/etc/Makefile])


###################################################################
## The following section deals with the generation of the
## aldor interface.
##
## If one of the Aldor related tests fail we should warn and
## not build the Aldor interface. Default is to build it.
## The test only fails if --enable-aldor is given explicitly
## and some prerequisite is not available.
BUILD_ALDOR_INTERFACE=build-aldor-interface

## check whether Aldor is available
AC_CHECK_PROGS([ALDOR], [aldor])
if test -z $ALDOR; then
  AC_MSG_WARN([Aldor not found.])
  BUILD_ALDOR_INTERFACE=
fi

# The following is only necessary to get some files that do not yet
# have a free license.
# Not having svn, curl, or wget is not a show stopper, because the files
# can be obtained manually via a webbrowser.
AC_CHECK_PROGS([SVN], [svn])
AC_CHECK_PROGS([CURL], [curl])
AC_CHECK_PROGS([WGET], [wget])

## check for $ALDORROOT/include/aldor.conf
if test -z $ALDORROOT || ! test -f $ALDORROOT/include/aldor.conf; then
  AC_MSG_WARN([The environment variable ALDORROOT is not set.])
  BUILD_ALDOR_INTERFACE=
fi

## check for $ALDORROOT/lib/libfoam.al (needed for aldor interface)
if ! test -f $ALDORROOT/lib/libfoam.al; then
  AC_MSG_WARN([Couldn't find libfoam.al.])
  BUILD_ALDOR_INTERFACE=
fi

## This handles the cases if one of the following options is given on the
## command line. If none is given, then the aldor interface will be built
## if and only if all prerequisites are available.
## --enable-aldor, --enable-aldor=yes:
##   User requires to build the aldor interface. In this case,
##   any missing prerequisite counts as error (not just warning).
## --enable-aldor=no, --disable-aldor:
##   User asks to skip the build of the aldor interface.
## --enable-aldor=SOMETHING DIFFERENT FROM yes/no
##   User made a mistake. This is an error.
AC_ARG_ENABLE(aldor,
  [AC_HELP_STRING([--enable-aldor],
                  [build an interface to the Aldor compiler])],
  [case $enableval in
    yes) if test -z "$BUILD_ALDOR_INTERFACE"; then
           AC_MSG_ERROR([prerequisites for Aldor interface missing])
         fi;;
    no)  BUILD_ALDOR_INTERFACE=;;
    *)   AC_MSG_ERROR([only yes/no allowed as value for --enable-aldor]);;
   esac])

###################################################################
# The installation is currently not done by recursively calling
# install in the src subdirectories, so we have to hardcode the
# intallation of the aldor interface in src/Makefile.in and let a
# variable decide whether or not the interface stuff should be
# installed.
AC_SUBST(BUILD_ALDOR_INTERFACE)

if test -z "$BUILD_ALDOR_INTERFACE"; then
  AC_MSG_WARN([Aldor interface will not be built.])
else
  AC_MSG_NOTICE([Aldor interface will be built.])
  axiom_src_subdirs="$axiom_src_subdirs aldor"
  axiom_src_all="$axiom_src_all all-aldor"
  AXIOM_MAKEFILE([src/aldor/Makefile])
  AXIOM_MAKEFILE([src/aldor/Makefile2])
fi
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
AXIOM_MAKEFILE([contrib/emacs/Makefile])
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
