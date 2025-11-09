
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

To build the default target (32 bit LispBM repl) just issue the command:

```
make
```

Additionally there are numerous alternative targets:

- `make all` - Build 32-bit REPL (default target)
- `make all64` - Build 64-bit REPL
- `make cov` - Build 32-bit REPL with coverage instrumentation
- `make debug` - Build with debug symbols
- `make sdl` - Build 32-bit REPL with SDL graphics support
- `make sdl64` - Build 64-bit REPL with SDL graphics support
- `make freetype` - Build 32-bit REPL with FreeType font backend
- `make freetype64` - Build 64-bit REPL with FreeType font backend
- `make sdl_freetype` - Build 32-bit REPL with SDL and FreeType support
- `make sdl_freetype64` - Build 64-bit REPL with SDL and FreeType support
- `make alsa` - Build 32-bit REPL with ALSA MIDI and sound support
- `make pirepl` - Build for Raspberry Pi (32-bit)
- `make pirepl64` - Build for Raspberry Pi (64-bit)
- `make improved_closures` - Build with experimental closure cleanup
- `make clean` - Remove built binaries and coverage data

For more advanced build instructions, including conditional build flags and code size optimizations, see the [Building LispBM documentation](https://www.lispbm.com/cdocs/html/Building.html).

## install as lbm

After building do:

```
make install
```

to install the repl as `lbm` under ~/.local/bin




