#!/bin/bash

make-deps-rec () {
    deps="$(grep '"*.scm"' $1 | sed -E 's/.*"([a-z]+)\.scm".*/\1/')"
    for dep in $deps; do deps="$deps $(make-deps-rec $dep.scm)"; done
    echo $deps
}

for f in $*; do make-deps-rec $f | tr -s ' ' '\n' | sort | uniq; done
