#! /bin/sh

NOTANGLE=${NOTANGLE:-notangle}

error() {
    echo "$1"
    exit 1
}

# set -x

$NOTANGLE ./configure.ac.pamphlet > ./configure.acp \
      && mv configure.acp configure.ac \
   || error "could not extract configure.ac from pamphlet file"

autoheader || error "could not re-generate config/axiom-c-macros.h"
autoconf || error "could not re-generate configure"

## subdirectoris that contain Makefile pamphlets of interest
SUBDIRS=". src                           \
                  src/lib                \
		  src/lisp               \
		  src/boot               \
		  src/interp             \
		  src/share              \
		  src/algebra            \
		  src/etc                \
		  src/clef               \
		  src/graph              \
                     src/graph/Gdraws    \
                     src/graph/view2D    \
                     src/graph/view3D    \
                     src/graph/viewAlone \
                     src/graph/viewman   \
		  src/sman               \
		  src/hyper              \
		  src/input              \
		  src/paste              \
"


for d in $SUBDIRS; do
    $NOTANGLE -t8 $d/Makefile.pamphlet > $d/Makefile.inp \
          && mv $d/Makefile.inp $d/Makefile.in \
       || error "could not extract $d/Makefile.in from pamphlet file"
done

# set +x
