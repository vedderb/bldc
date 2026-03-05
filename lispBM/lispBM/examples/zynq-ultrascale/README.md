# LispBM REPL in FreeRTOS for Zynq UltraScale+

The purpose of this example is to get people who want to try/use
LispBM started. This example sets up a thread for running LispBM
programs concurrently to the main application written in C.

This example has been tested on AMD/Xiling Vitis 2025.2 on a TE0802-02
development board from Trenz Electronic.

# Requirements

This example requires a Zynq UltraScale+ platform (A53 64Bit processor).

This example requires a BSP platform and that platform must have
`freertos_support_static_allocation` enabled in the `Configuration for Os: freertos`
section of the bsp platform settings.

# Building

 1. In vitis click examples (left hand side of gui) and select `FreeRTOS Hello World`.
 2. Click `Create application Component from Template`.
 3. Decide a name for the application.
 4. Select hardware platform. This must be a 64bit zynq ultrascale+ platform.
 5. Domain should be freertos based (probably prefilled).
 6. Finish.

Now you should have a application project to modify.
 7. Remove the `freertos_hello_world.c` file.
 8. Go into settings under your application project and click `vitis_comp.json`.
 9. Click `Navigate to BSP Settings" and locate the freertos settings. 
 10. Set `freertos_support_static_allocation` to true.

Now the BSP is set up for LispBM integration!
 11. Copy `main.c` from this example directory into your application's `src/` directory.
 12. Create a file called `lispbm.cmake` in your application's `src/` directory
     with the following content (adjust `LISPBM_DIR` to your local clone):
```cmake
set(LISPBM_DIR "/path/to/lispbm")
include("${LISPBM_DIR}/examples/zynq-ultrascale/lispbm_sources.cmake")

list(APPEND USER_COMPILE_SOURCES "main.c" ${LISPBM_SOURCES})
list(APPEND USER_INCLUDE_DIRECTORIES ${LISPBM_INCLUDE_DIRS})
list(APPEND USER_COMPILE_DEFINITIONS "LBM64")
list(APPEND USER_LINK_LIBRARIES "m")
```
 13. In your application's `src/CMakeLists.txt`, add this line after the
     `UserConfig.cmake` include:
```cmake
include(${CMAKE_CURRENT_SOURCE_DIR}/lispbm.cmake)
```
 14. Build and connect over UART to see the LispBM REPL prompt.



# A Note on Images and Powerful Platforms. 

The A53 is a very capable 64bit processor and systems based on it
usually comes with lots of memory (Several Megabytes!). On such a
system it may be more suitable to keep the "Lisp Image" in RAM. On
small embedded systems we like to write the image to FLASH
incrementally but when lots of ram is available it feels suitable to
keep the image in RAM. If one is after the persistance and the bootup
speed improvements of images then I think the entire image should be
written to flash/eMMC/SDCard in a single go. This is something to look
at more in the future.