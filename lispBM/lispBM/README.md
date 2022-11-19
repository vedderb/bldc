# lispBM (LBM)

LispBM is a lisp or scheme like programming language for
microcontrollers.  LispBM also borrows a couple of ideas from Erlang
when it comes to concurrency, message passing, pattern matching and
process monitoring.  The LispBM runtime system can be compiled for
either 32 or 64 bit platforms and runs on a wide range of hardware
such as for example STM32, NRF52, ESP32 or X86.  When running the
LispBM runtime system on a microcontroller it can be built on top of
ChibiOS, FreeRTOS or ZephyrOS or, if you are adventurous, on bare-metal.
LispBM can also be built to run on topof a regular linux. 

![LispBM mascot](https://github.com/svenssonjoel/lispBM/blob/master/mascot/lispbm_llama_small.png)

The LispBM mascot, Lispy the llama, was created by
[PixiLadyArt](https://www.instagram.com/pixiladyart/). Thank you, Pixi! 

## Want to get involved and help out?

There are lots of interesting things to code on in and around the
LispBM runtime system.

1. Are you interested in microcontrollers and programming languages?
2. You find it fun to mess around in C code with close to zero comments?
3. Then join in the fun. Lots to do, so little time!
4. Poke me by mail bo(dot)joel(dot)svensson(whirly-a)gmail(dot)com.

## Documentation
 - Work in progress [LispBM language reference](./doc/lbmref.md).
 - Work in progress [LispBM programming manual](./doc/manual)
 - C code documentation can be found [here](http://svenssonjoel.github.io/lbmdoc/html/index.html).
 - LispBM's internals are documented as a series of [blog posts](http://svenssonjoel.github.io).
 - There are [demonstrations on YouTube](https://youtube.com/playlist?list=PLtf_3TaqZoDOQqZcB9Yj-R1zS2DWDZ9q9).

## Features
1. heap consisting of cons-cells with mark and sweep garbage collection.
2. Built-in functions: cons, car, cdr, eval, list, +, -, >, <, = and more.
3. Some special forms: Lambdas, closures, lets (letrecs), define and quote.
4. 28-Bit signed/unsigned integers and boxed 32-Bit Float, 32-Bit signed/unsigned values.
5. Arrays (in progress), string is an array.
6. Quasiquotation.
7. Concurrency.
8. Message-passing.
9. Pattern-matching.

## Vague or continuosly ongoing todos
1. Doxygen?
2. Tutorials?
3. Be much more stringent on checking of error conditions etc.
4. Make uniform how to return success or failure. It is sometimes bool and sometimes int right now. 

## Compile a 64bit binary for linux

1. Build the repl: `cd repl` and then `make all64`

2. Run the repl: `./repl`

## Compile a 32bit binary for linux (Requires 32bit libraries. May need something like "multilib" on a 64bit linux)

1. Build the repl: `cd repl` and then `make`

2. Run the repl: `./repl`

## Compile on Raspberry Pi

To build the library exeute the following command in the lispbm folder:

```
PLATFORM=pi make
```

To build the `repl` example repl do:

```
cd repl
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
