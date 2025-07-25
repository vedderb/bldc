#!/bin/bash

versions=$(ls -d version*)

mkdir -p html

for ver in $versions;
do
    echo Processing: $ver
    cp -r $ver ./html
    cd html/$ver
    tree -H . > index.html
    cd ..
    cd ..
done

cp -r html ../../lispbm.com/testlogs
