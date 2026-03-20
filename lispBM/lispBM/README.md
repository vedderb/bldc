[![Website](https://img.shields.io/badge/Website-lispbm.com-blue)](https://www.lispbm.com)
[![Documentation](https://img.shields.io/badge/docs-latest-brightgreen)](https://www.lispbm.com/#documentation)
[![Discord](https://img.shields.io/badge/Discord-Join%20Server-7289da?logo=discord&logoColor=white)](https://discord.gg/urtJUUMnwQ)
[![Gallery](https://img.shields.io/badge/Gallery-Community%20Projects-green?logo=image)](https://www.lispbm.com/gallery.html)
[![Contributors](https://img.shields.io/badge/Contributors-Meet%20Our%20Team-blue?logo=github)](https://www.lispbm.com/contributors.html)

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

LispBM is an embeddable scripting language meant to be integrated into
a larger application. Our goal is that LispBM is a suitably sandboxed scripting
language to be used in just about any application domain.

![LispBM mascot](https://github.com/svenssonjoel/lispBM/blob/master/mascot/lispbm_llama_small.png)

The LispBM mascot, Lispy the llama, was created by
[PixiLadyArt](https://www.instagram.com/pixiladyart/). Thank you, Pixi! 

## Contribute to LispBM!
LispBM is an open source project and welcomes collaboration and contribution.
Do you want to get involved and help out?

 - Check out the [lispbm.com](http://www.lispbm.com) website.
 - Join the [discord server](https://discord.gg/urtJUUMnwQ).
 
### Documentation

 - [Searchable reference manual](https://www.lispbm.com/lispbm-reference-manual/html/)
 - [More documentation available from the website](https://www.lispbm.com/)
 - There are [demonstrations on YouTube](https://youtube.com/playlist?list=PLtf_3TaqZoDOQqZcB9Yj-R1zS2DWDZ9q9).

## Getting started

[Try LispBM in the browser](https://www.lispbm.com/repl-wasm/index.html)

The easiest way to get started with LispBM programming on actual embedded systems is to use
[VESC-Tool](https://vesc-project.com/vesc_tool) together with the
[VESC EXPRESS Firmware](https://github.com/vedderb/vesc_express) on an
[esp32c3](https://docs.espressif.com/projects/esp-idf/en/latest/esp32c3/hw-reference/esp32c3/user-guide-devkitm-1.html).

VESC-Tool gives you access to a very capable [IDE](https://vesc-project.com/sites/default/files/lisp%20overview.png) for lisp programming on embedded systems
and the VESC Express firmware provides built in functionality to interface with CAN, WIFI, BLE and UART
as well as GPIO and display drivers.

Many other ways to get started with LispBM is described on our [getting started page](https://www.lispbm.com/getting-started.html).
There you can find information about how get going on a selection of platforms.