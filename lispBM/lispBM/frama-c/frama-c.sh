#!/bin/bash

## run bear -- make 

frama-c -json-compilation-database compile_commands.json main.c ../src/*.c ../platform/linux/src/*.c -save parse.sav

## frama-c -load parse.sav -eva -eva-precision 3 -save eva.sav

## ivette -load eva.sav 
