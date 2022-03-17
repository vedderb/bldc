#!/bin/bash

if [ -d "repl-zephyr_build" ]; then
    echo "Build directory exists!"
else
    mkdir repl-zephyr_build
    cd repl-zephyr_build
    cmake -G "Eclipse CDT4 - Unix Makefiles" -DBOARD=nrf52840_pca10056 ../repl-zephyr
fi    
