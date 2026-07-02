
# LispBM REPL in FreeRTOS for esp32c3

The purpose of this example is to get people who want to try/use LispBM
started. This example sets up a thread for running LispBM programs
concurrently to the main application written in C. 

This example was developed using ESP-IDF v5.5.3
Are you using a newer ESP-IDF and this example is no longer compiling, let me know.

This code runs on the esp32c3-devkit-mini1 but likely runs fine on similar
development kits.

This code interfaces with a display using a ST7789 driver over SPI. The display
used has a 240x240 resolution and is using a driver borrowed from the vesc express
code but updated so that the `disp-load-st7789` function takes extra arguments specifying
the display resolution.

# Building

The example uses a flash partition for image storage.

run:

```
idf.py partition-table
```

to process the partitions.csv file

Then run:

```
idf.py build
```

followed by:

```
idf.py flash
```

Now connect to the development kit using a serial terminal (such as  minicom) or run:

```
idf.py monitor
```

You can now interact with a simple REPL on your esp32c3.

# Pleasant use

Use rlwrap with picocom for a better experience. Disable character echoing
in the main.c file if going for this approach.

```
rlwrap picocom /dev/ttyUSB0 -b 115200
```

Exit by pressing CTRL-a followed by CTRL-x

# Display

(disp-load-st7789 6 4 7 8 5 40 240 240)
(disp-reset)
(disp-clear 0xff0000)


