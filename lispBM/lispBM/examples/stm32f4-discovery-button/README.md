
# LispBM REPL in Chibios for stm32f407-discovery boards

The purpose of this example is to get people who want to try/use 
LispBM on an STM32 based system.

The example is targeting ChibiOS 21.11.3.
The dev-board used during development is the stm32f407-disc1 but
it can probably quite easily be portet to other stm32f4 boards.
The example uses USB-CDC to provide a REPL. If your board does not
have this kind of USB wired up, you need to rewrite the example to
use an UART for interaction.

The example also shows how to set up image-storage on flash. When up
and running you interact with the image via the extensions:

1. `(image-clear)` - clear the image and reboot the system
2. `(image-save)`  - Save the environemnt into the image.
3. `(reset)` - resets the board in an image-safe way.

Saving the image requires that there is a main function. The main function
is launched automatically upon reboot.

Note that LBM images are not portable across changes to the FW!
So if you have an image stored on the flash, but you uploaded an updated firmware,
then LBM will (possibly/likely) not be able to load the image. Run the
`erase-image.sh` script to clear the image and be able to boot again.

# Requirements:

1. ChibiOS 21.11.3 located at `../../../ChibiOS_21.11.3` relative to this example (or modify the Makefile)
2. arm-none-eabi-gcc compiler suite. 

# Building

run:

```
make
```

# Flashing onto discovery board

run:

```
./flash.sh
```

