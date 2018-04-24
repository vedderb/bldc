#!/bin/bash

mkdir -p ci/deploy

for f in `find ./build_all -name '*bin'`
do
   filename=`echo $f | sed 's/\.\/build_all\///; s/\/VESC//;'`
   cp $f ci/deploy/$filename
done