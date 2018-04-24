#!/bin/bash

mkdir deploy

for f in `find . -name '*bin'`
do
   filename=`echo $f | awk -F'/' '{SL = NF-1; TL = NF-2; print $SL  "_" $NF}'`
   cp $f deploy/$filename
done