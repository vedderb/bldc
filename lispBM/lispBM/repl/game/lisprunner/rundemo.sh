#!/bin/bash


if [ -f ../../repl ]; then
    ../../repl -M $((1024*1024)) -H 32000 -s demo.lisp
else
    echo "Go to directory ../../ and run 'make sdl_old' to build the repl with SDL support"
fi
