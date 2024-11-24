#!/bin/sh

make clean
if command -v scan-build-14 
then
    scan-build-14 -o ./static_analysis make -j4
else
    if command -v scan-build-10 
    then
        scan-build-10 -o ./static_analysis make -j4
    fi
fi
