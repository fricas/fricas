#!/bin/sh
# Compile every constructor of a built algebra library once more, with the
# check for missing exported functions enabled, and write one log file per
# constructor.  The check needs the final databases of a finished build
# (during the library build itself it is switched off, since the bootstrap
# databases do not record which operations default packages implement).
#
#   tools/missing-survey.sh [BUILDDIR [OUTDIR [JOBS]]]
#
# BUILDDIR defaults to ./build (an out-of-tree build), OUTDIR to
# BUILDDIR/missing-survey, JOBS to 4.  Summarize the result with
#
#   tools/missing-report.py OUTDIR/logs

set -e

if [ "$1" = --one ]; then
    # internal: compile constructor $2, called via xargs below
    printf '%s\n%s\n' ")set compiler missing warn" ")compile $ALG/$2.spad" |
        DAASE="$TARGET" FRICAS="$TARGET" FRICAS_INITFILE='' "$INTERPSYS" \
        > "logs/$2.log" 2>&1 || true
    exit 0
fi

top=$(cd "$(dirname "$0")/.." && pwd)
build=$(cd "${1:-$top/build}" && pwd)
out=${2:-$build/missing-survey}
jobs=${3:-4}

INTERPSYS=$(ls "$build"/build/*/bin/interpsys | head -1)
TARGET=$(ls -d "$build"/target/* | head -1)
ALG="$build/src/algebra"
export INTERPSYS TARGET ALG
[ -x "$INTERPSYS" ] || { echo "no interpsys in $build" >&2; exit 1; }

mkdir -p "$out/logs"
cd "$out"
self="$top/tools/missing-survey.sh"
ls "$ALG"/*.spad | sed 's|.*/||; s|\.spad$||' |
    xargs -P "$jobs" -n 1 "$self" --one
echo "logs written to $out/logs"
