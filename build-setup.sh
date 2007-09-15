#! /bin/sh

error() {
    echo "$1"
    exit 1
}

# set -x

autoheader || error "could not re-generate config/axiom-c-macros.h"
autoconf || error "could not re-generate configure"

# set +x
