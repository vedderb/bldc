
# LBM REPL

## Dependencies:

        64Bit       | 32Bit
        ---------------------------
                    | gcc-multilib
        libreadline | lib32readline
        libhistory  | lib32history

Ubuntu example for obtaining 32bit dependencies
'sudo apt-get install gcc-multilib libreadline-dev lib32readline-dev'


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




