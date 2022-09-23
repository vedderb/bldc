# Black Magic Probe

This is the black magic probe integration in the VESC firmware. You can read more about the black magic probe at  
[https://github.com/blacksphere/blackmagic/wiki](https://github.com/blacksphere/blackmagic/wiki)

The black magic probe software allows the VESC to be used as a SWD programmer and, to some extent, debugger. Currently only a subset of the targets and functionality of the black magic probe is included, in order to not use too much flash. If more targets are needed later it is easy to add them.

Targets that are supported:

  * NRF51 series
  * NRF52 series
  * STM32F0
  * STM32F1
  * STM32F3
  * STM32F2
  * STM32F4
  * STM32F7
  * STM32L0
  * STM32L1
  * STM32L4

Note that not all of the targets can be programmed by VESC Tool at the moment, but if the need arises they can easily be added to **bm_if.c** and **pageswdprog.c** in VESC Tool.

The VESC terminal also has some commands that provide additional functionality over the GUI page, such as reading and manipulating the option bytes and reading the device ID. The terminal commands also support some operations on targets not added to the GUI.
