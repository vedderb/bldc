
# LispBM REPL in FreeRTOS for esp32c3

The purpose of this example is to get people who want to try/use LispBM
started. This example sets up a thread for running LispBM programs
concurrently to the main application written in C. 

This example was developed using ESP-IDF v6.0-dev-1489-g4e036983a7.
Are you using a newer ESP-IDF and this example is no longer compiling, let me know.

This code runs on the esp32c3-devkit-mini1 but likely runs fine on similar
development kits.

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


