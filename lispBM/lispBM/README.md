# lispBM (LBM)

A concurrent lisp-like language with message-passing and
pattern-matching implemented in C for 32 bit and 64 bit platforms.

![LispBM mascot](https://github.com/svenssonjoel/lispBM/blob/master/mascot/lispbm_llama_small.png)

All programming languages need a mascot, so here is the LispBM llama by [PixiLady](https://www.instagram.com/pixiladyart/).

## Information

From version 0.6.0 LispBM implements round-robin scheduling and is
no-longer cooperatively concurrent.  The documentation is not yet
updated to reflect this change. If it turns out that the new approach
to scheduling makes sense over time, the documentation will be
updated.

This is not a semantics preserving update. Using the cooperative
scheduler one could assume totally exclusive access to the runtime
system until a process yields. Between yields one could assume that
all reads and writes to globals were not interspersed by reads and
writes of other tasks, this is now false. An atomic operation has been
added in case one wants to be sure that evaluation of an expression is
done in one go, with no interruption.

For example: 
```lisp
(atomic
  (progn
     a
     b
     c))
```

## Documentation
 - Work in progress [LispBM language reference](./doc/lbmref.md).
 - Work in progress [LispBM programming manual](./doc/manual)
 - C code documentation can be found [here](http://svenssonjoel.github.io/lbmdoc/html/index.html).
 - LispBM's internals are documented as a series of [blog posts](http://svenssonjoel.github.io).
 - There are [demonstrations on YouTube](https://youtube.com/playlist?list=PLtf_3TaqZoDOQqZcB9Yj-R1zS2DWDZ9q9).


## Purpose
1. Have fun.
2. Learn about lisp.
3. Learn about microcontrollers.
4. An interactive REPL for devboards.
5. ...

## Features
1. heap consisting of cons-cells with mark and sweep garbage collection.
2. Built-in functions: cons, car, cdr, eval, list, +, -, >, <, = and more.
3. Some special forms: Lambdas, closures, lets (letrecs), define and quote.
4. 28-Bit signed/unsigned integers and boxed 32-Bit Float, 32-Bit signed/unsigned values.
5. Arrays (in progress), string is an array.
6. Compiles for, and runs on linux-x86.
7. Compiles for, and runs on Zynq 7000.
8. Compiles for, and runs on STM32f4.
9. Compiles for, and runs on NRF52840.
10. Compiles for, and runs on ESP32 (ARM - WROOM).
11. Compiles for, and runs on ESP32C3 (RISC-V).
12. Compiles for, and runs on Raspberry PI (Tested on 32bit Raspbian OS)
13. Quasiquotation.
14. Concurrency.
15. Message-passing.
16. Pattern-matching.

## Want to get involved and help out?
1. Are you interested in microcontrollers and programming languages?
2. You find it fun to mess around in C code with close to zero comments?
3. Then join in the fun. Lots to do, so little time!
4. Poke me by mail bo(dot)joel(dot)svensson(whirly-a)gmail(dot)com

## Vague or continuosly ongoing todos
1. Doxygen?
2. Tutorials?
3. Be much more stringent on checking of error conditions etc.
4. More built in arithmetic.
5. More built in comparisons.
6. Make uniform how to return success or failure. It is sometimes bool and sometimes int right now. 

## Compile a 32bit binary for linux (Requires 32bit libraries. May need something like "multilib" on a 64bit linux)

1. Build the repl: `cd repl-cps` and then `make`

2. Run the repl: `./repl`

## Compile a 64bit binary for linux

1. Build the repl: `cd repl-cps` and then `make all64`

2. Run the repl: `./repl`

## Compile on Raspberry Pi

To build the library exeute the following command in the lispbm folder:

```
PLATFORM=pi make
```

To build the `repl-cps` example repl do:

```
cd repl-cps
make pirepl
```

Then start it up using `./repl`
Building the library is not a prerequisite for building the repl anymore.

## SDL and LispBM

In the `sdlrepl` directory there is a start of a set of SDL bindings for LispBM.

To build this repl you need the following dependencies:

1. libsdl2-dev - `sudo apt-get install libsdl2-dev`
2. libsdl2-image-dev - `sudo apt-get install libsdl2-image-dev`

Then compile the repl using the command `make`
