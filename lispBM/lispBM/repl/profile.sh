#!/bin/bash

src=$1

valgrind --toggle-collect=lbm_run_eval --tool=callgrind --callgrind-out-file=cg.out ./repl --terminate -s $src

callgrind_annotate cg.out | head -5

gprof2dot  --node-thres=0.01 --edge-thres=0.01 -f callgrind cg.out -o cg.dot

dot -Tpdf cg.dot -o cg.pdf

rm cg.out
rm cg.dot
