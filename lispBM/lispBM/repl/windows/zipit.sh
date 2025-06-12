#!/bin/bash

rm -rf winlbm
rm -f  winlbm.zip

cp -r build winlbm

zip -r winlbm.zip winlbm

