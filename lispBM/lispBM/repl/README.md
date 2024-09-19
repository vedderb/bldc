
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


## Build

```
make
```

## Build improved closures versions

```
make
make clean
make improved_closures
```

## install as lbm

After building do:

```
make install
```

to install the repl as `lbm` under ~/.local/bin




