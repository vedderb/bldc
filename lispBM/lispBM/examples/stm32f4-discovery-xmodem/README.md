
# LispBM REPL in Chibios for stm32f407-discovery boards

The purpose of this example is to give an example how to use
the xmodem utility for uploading of code to a running RTS.

The example is targeting ChibiOS 21.11.3.
The dev-board used during development is the stm32f407-disc1 but
it can probably quite easily be ported to other stm32f4 boards.
The example uses USB-CDC to provide a REPL. If your board does not
have this kind of USB wired up, you need to rewrite the example to
use an UART for interaction.

# Requirements:

1. ChibiOS 21.11.3 located at `../../../ChibiOS_21.11.3` relative to this example (or modify the Makefile)
2. arm-none-eabi-gcc compiler suite.
3. minicom (or other serial terminal)
4. sx

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

# Upload program to running LispBM

Connect to your devboard with a serial terminal.

```
minicom -D /dev/ttyACM1
``` 

(Could be ttyACM0, ttyACM1 or any other number depending on your setup).

Then issue the command `:xmodem` in the repl

```
# :xmodem                                                                       
XMODEM ready - exit terminal and run sx.
CC
```

Now in another linux terminal window upload a file using sx.

```
$sx test.lisp < /dev/ttyACM0 > /dev/ttyACM0 
Sending test.lisp, 1 blocks: Give your local XMODEM receive command now.
Bytes Sent:    256   BPS:4784                            

Transfer complete
```

The `test.lisp` program should now have run. Note that output is disabled
while the xmodem transfer is in progress. The serial link will be "hijacked"
by the sx program for the duration of the transfer. 

