# lispBM (LBM)

LispBM is a lisp or scheme like programming language for
microcontrollers.  LispBM also borrows a couple of ideas from Erlang
when it comes to concurrency, message passing, pattern matching and
process monitoring.  The LispBM runtime system can be compiled for
either 32 or 64 bit platforms and runs on a wide range of hardware
such as for example STM32, NRF52, ESP32 or X86.  When running the
LispBM runtime system on a microcontroller it can be built on top of
ChibiOS, FreeRTOS or ZephyrOS or, if you are adventurous, on bare-metal.
LispBM can also be built to run on top of a regular linux. 

![LispBM mascot](https://github.com/svenssonjoel/lispBM/blob/master/mascot/lispbm_llama_small.png)

The LispBM mascot, Lispy the llama, was created by
[PixiLadyArt](https://www.instagram.com/pixiladyart/). Thank you, Pixi! 

## Want to get involved and help out?

 - Check out the [lispbm.com](http://www.lispbm.com) website.
 - Join the [discord server](https://discord.gg/urtJUUMnwQ).

## Getting started 

The easiest way to get started with LispBM programming is to use
[VESC-Tool](https://vesc-project.com/vesc_tool) together with the
[VESC EXPRESS Firmware](https://github.com/vedderb/vesc_express) on an [esp32c3](https://docs.espressif.com/projects/esp-idf/en/latest/esp32c3/hw-reference/esp32c3/user-guide-devkitm-1.html).

VESC-Tool gives you access to a very capable [IDE](https://vesc-project.com/sites/default/files/lisp%20overview.png) for lisp programming on embedded systems
and the VESC Express firmware provides built in functionality to interface with CAN, WIFI, BLE and UART
as well as GPIO and display drivers.

If you are looking for a more bare-bones way to add scripting capabilities to your
embedded system, LispBM is quite platform independent and can be compiled for many
different MCUs and systems. LispBM can tie into the rest of your application via
what we call "extensions" which are C functions that can be called from your LispBM scripts. 

### Documentation

 - Work in progress [LispBM language reference](./doc/lbmref.md).
 - Work in prograss [LispBM display library reference](./doc/displayref.md).
 - [LispBM runtime system library reference](./doc/runtimeref.md).
 - Gotchas and caveats [Gotchas and caveats](./doc/gotchas.md).
 - !OUTDATED! [LispBM programming manual](./doc/manual).
 - C code documentation can be found [here](http://svenssonjoel.github.io/lbmdoc/html/index.html).
 - LispBM's internals are documented as a series of (now quite outdated) [blog posts](http://svenssonjoel.github.io).
 - There are [demonstrations on YouTube](https://youtube.com/playlist?list=PLtf_3TaqZoDOQqZcB9Yj-R1zS2DWDZ9q9).

## The LBM REPL

There is an example REPL implementation that runs on X86 32 or 64bit in the `repl` directory.
The REPL depends on libreadline.


**REPL Dependencies for 32Bit executable on 64Bit linux:**
* libreadline
* lib32readline
* gcc-multilib
* libpng

On Ubunty you can obtain the depencies by:
```shell
sudo apt-get install gcc-multilib libreadline-dev lib32readline-dev libpng-dev libpng-dev:i386
```

Then issue command `make` in the repl directory.

There is also a Nix flake (see details below). You can build and run the repl using `nix run`. This will build the 32 bit version. For the 64 bit repl you instead run `nix run .#repl64`.

### Editor support

* [vesc_tool](https://vesc-project.com/vesc_tool)
* [vscode support](https://marketplace.visualstudio.com/items?itemName=rasmus-soderhielm.lispbm-language-support)

## Development

There is a Nix flake to help you build the repl and documentation. It outputs packages for the 32 and 64 bit repl: `repl` and `repl64`, the documentation: `doc`, and the doxygen source code documentation: `c-doc`. Assuming you have Nix installed, you can build these using
```shell
nix build .#<package>
```

The generated output is then placed in the `result/` directory. However `doc` and `c-doc` packages might not make much sense to build in this way because they are supposed to generate files inside the repository tree. Therefore, it makes more sense to instead enter their respective development environment with `nix develop .#<package>`. This places you in a new shell with the required dependencies installed where you can build them with `make`. 
