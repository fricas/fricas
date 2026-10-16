#!/bin/sh
# Regression check for compiler changes: compare the Lisp generated for the
# algebra library (out-of-tree build in ./build) with a saved baseline.
#
#   tools/compare-lsp.sh save    copy build/src/algebra/*.NRLIB/*.lsp to
#                                build/baseline-lsp/
#   tools/compare-lsp.sh         compare the current build with the baseline
#
# Exit status 0 if all files are identical, 1 otherwise.

set -e
top=$(cd "$(dirname "$0")/.." && pwd)
alg="$top/build/src/algebra"
base="$top/build/baseline-lsp"

if [ "$1" = save ]; then
    mkdir -p "$base"
    rm -f "$base"/*.lsp
    for d in "$alg"/*.NRLIB; do
        n=$(basename "$d" .NRLIB)
        [ -f "$d/$n.lsp" ] && cp "$d/$n.lsp" "$base/"
    done
    echo "saved $(ls "$base" | wc -l | tr -d ' ') files to $base"
    exit 0
fi

changed=0
missing=0
for f in "$base"/*.lsp; do
    n=$(basename "$f" .lsp)
    new="$alg/$n.NRLIB/$n.lsp"
    if [ ! -f "$new" ]; then
        echo "missing: $n"
        missing=$((missing + 1))
    elif ! cmp -s "$f" "$new"; then
        echo "changed: $n"
        changed=$((changed + 1))
    fi
done
total=$(ls "$base" | wc -l | tr -d ' ')
echo "$total files: $changed changed, $missing missing"
[ $changed -eq 0 ] && [ $missing -eq 0 ]
