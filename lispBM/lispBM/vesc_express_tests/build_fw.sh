#!/bin/bash

IDF_DIR=$HOME/esp-idf-v5.0.2
VESC_TOOL="vesc_tool_6.05"

if [ -d "vesc_express" ]; then
    cd vesc_express
    git reset --hard origin/main
    git pull
else
    git clone git@github.com:vedderb/vesc_express
    cd vesc_express
fi

echo -e "\nidf_build_set_property(COMPILE_OPTIONS \"-Wno-maybe-uninitialized\" APPEND)" >> CMakeLists.txt

rm -rf build
cd main
rm -rf lispBM


git clone git@github.com:svenssonjoel/lispBM

cd ..

source $IDF_DIR/export.sh

idf.py build

if [ -f "vesc_express/build/vesc_express.bin" ]; then
    $VESC_TOOL --uploadFirmware vesc_express/build/vesc_express.bin
else
    echo "Building firmware failed"
fi


