
# LBM REPL

## Dependencies:

        64Bit       | 32Bit
        ---------------------------
                    | gcc-multilib
        libreadline | libreadline:i386
        libhistory  | libhistory:i386

Ubuntu example for obtaining 32bit dependencies
'sudo apt-get install gcc-multilib libreadline-dev lib32readline-dev'

Additionally for SDL and png support the following libraries are
needed.

        64Bit              | 32Bit
        ---------------------------
        libsdl2-dev        |
        libsdl2-image-dev  |
        libpng-dev         | libpng-dev:i386

**Note** that installing libsdl2-dev:i386/libsdl2-image-dev:i386 on UBUNTU 24.04 seems to brick the
entire OS! So I cannot recommend trying that... But if anyone know what is going
on there, please let me know!

To generate dot graphs from LBM, also install graphviz:

```
sudo apt install graphviz
```

Additionally, for MIDI and sound the following dependencies are needed:

        64Bit              | 32Bit
        ---------------------------
        libasound2-dev     | libasound2-dev:i386

And finally for offline rendering of fonts using freetype:

        64Bit              | 32Bit
        ---------------------------
        libfreetype-dev    | libfreetype-dev:i386



### Dependencies on MACOS

In order to build the repl on Macos, you need to install libpng using brew as below.

'brew install libpng readline'

You need to build the `all64` target, and set `PLATFORM=macos-arm` when building.

## Build

The REPL can be built with different feature-sets. features are selected as:
```
make FEATURES="alsa sdl"
```
which is an example of adding features for sound and graphics.

The total list of features is:

* alsa     - Sound on Linux.
* sdl      - Graphics on Linux.
* freetype - Use libfreetype for font prepropressing.
* 64       - 64Bit build.
* coverage - Build with coverage collection.

To build the default target (32 bit LispBM repl) just issue the command:

```
make
```

## install as lbm

After building do:

```
make install
```

to install the repl as `lbm` under ~/.local/bin




