#!/bin/sh

make clean
scan-build-14 -o ./static_analysis make -j4
