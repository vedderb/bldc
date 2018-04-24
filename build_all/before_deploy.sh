#!/bin/bash

mkdir deploy

for f in `find . -name '*bin'`
do
   filename=`echo $f | sed 's/\.\///; s/\/VESC//;'
   cp $f deploy/$filename
done