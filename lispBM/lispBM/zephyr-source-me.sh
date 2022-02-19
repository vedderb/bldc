#!/bin/bash

# Tweak this line to reflect your setup
export ZEPHYR_TOOLCHAIN_VARIANT=cross-compile
export CROSS_COMPILE=$HOME/opt/gcc-arm-none-eabi-9-2019-q4-major/bin/arm-none-eabi-

source ../zephyrproject/zephyr/zephyr-env.sh 
