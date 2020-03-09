#!/bin/sh

ONLY_GENERATE_IMAGES=1 dune runtest --force &&
    sleep 1 &&
    for i in `find _build/default/test -maxdepth 1 -name *.png`
    do
        NAME=`basename $i`
        mv $i test/tests/$NAME
    done
