#!/bin/sh

make clean
scan-build-10 -o ./static_analysis make -j4
