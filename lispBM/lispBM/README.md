# lispBM (Lisp Black Magic)

A concurrent lisp-like language with message-passing and
pattern-matching implemented in C for 32 bit and 64 bit platforms.

![LispBM mascot](https://github.com/svenssonjoel/lispBM/blob/master/mascot/lispbm_llama_small.png)

All programming languages need a mascot, so here is the LispBM llama by [PixiLady](https://www.instagram.com/pixiladyart/).

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

## TODOs
1. [x] Write some tests that stresses the Garbage collector.
2. [x] Implement some "reference to X type", for uint32, int32.
3. [x] Write a small library of useful hofs.
4. [x] Improve handling of arguments in eval-cps.
5. [x] Code improvements with simplicity, clarity  and readability in mind.
6. [x] Implement a small dedicated lisp reader/parser to replace MPC. MPC eats way to much memory for small platforms.
7. [x] Port to STM32f4 - 128K ram platform (will need big changes). (surely there will be some more bugs)
8. [x] Add STM32f4 example code (repl implementation)
9. [x] Port to nrf52840_pca10056 - 256k ram platform (same changes as above).
10. [x] Reduce size of builtins.c and put platform specific built in functions elsewhere. (Builtins.c will be removed an replaced by fundamentals.c) 
11. [x] Implement 'progn' facility.
12. [x] Remove the "gensym" functionality havent found a use for it so far and it only complicates things.
13. [x] Add NRF52 example repl to repository
14. [x] Update all example REPLs after adding quasiquotation
15. [x] The parser allocates heap memory, but there is no interfacing with the GC there.
16. [x] The parser uses a lot of stack memory, fix by making tail recursive and accumulating lists onto heap directly. 
17. [x] Rename files with names that may conflict with common stuff (memory.h, memory.c). 
18. [x] It should be possible to reset the runtime system.
19. [x] Add messages to lisp process mailbox from C to unlock blocked proc.
20. [x] Spawn closures specifically instead of expressions in general.
21. [x] Implement some looping structure for speed or just ease of use. 
        See the loop macros implemented by Benjamin in [bldc](https://github.com/vedderb/bldc/blob/master/lispBM/lispif_vesc_dynamic_loader.c#L103).
        

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
