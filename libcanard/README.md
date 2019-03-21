# Libcanard
[![Forum](https://img.shields.io/discourse/https/forum.uavcan.org/users.svg)](https://forum.uavcan.org)
[![Build Status](https://travis-ci.org/UAVCAN/libcanard.svg?branch=master)](https://travis-ci.org/UAVCAN/libcanard)
[![Coverity Scan](https://scan.coverity.com/projects/uavcan-libcanard/badge.svg)](https://scan.coverity.com/projects/uavcan-libcanard)

Minimal implementation of the UAVCAN protocol stack in C for resource constrained applications.

Get help on the **[UAVCAN Forum](https://forum.uavcan.org)**.

## Usage

If you're not using Git, you can just copy the entire library into your project tree.
If you're using Git, it is recommended to add Libcanard to your project as a Git submodule,
like this:

```bash
git submodule add https://github.com/UAVCAN/libcanard
```

The entire library is contained in three files:

- `canard.c` - the only translation unit; add it to your build or compile it into a separate static library;
- `canard.h` - the API header; include it in your application;
- `canard_internals.h` - internal definitions of the library;
keep this file in the same directory with `canard.c`.

Add `canard.c` to your application build, add `libcanard` directory to the include paths,
and you're ready to roll.

Also you may want to use one of the available drivers for various CAN backends
that are distributed with Libcanard - check out the `drivers/` directory to find out more.

If you wish to override some of the default options, e.g., assert macros' definition,
define the macro `CANARD_ENABLE_CUSTOM_BUILD_CONFIG` as a non-zero value
(e.g. for GCC or Clang: `-DCANARD_ENABLE_CUSTOM_BUILD_CONFIG=1`)
and provide your implementation in a file named `canard_build_config.h`.

Example for Make:

```make
# Adding the library.
INCLUDE += libcanard
CSRC += libcanard/canard.c

# Adding drivers, unless you want to use your own.
# In this example we're using Linux SocketCAN drivers.
INCLUDE += libcanard/drivers/socketcan
CSRC += libcanard/drivers/socketcan/socketcan.c
```

There is no dedicated documentation for the library API, because it is simple enough to be self-documenting.
Please check out the explanations provided in the comments in the header file to learn the basics.
Most importantly, check out the demo application under `tests/demo.c`.
Also use [code search to find real life usage examples](https://github.com/search?q=libcanard&type=Code&utf8=%E2%9C%93).

At the moment the library does not provide means to automate (de)serialization of UAVCAN data structures,
like other implementations (e.g. libuavcan for C++ or pyuavcan for Python) do.
Therefore, data structures need to be parsed and assembled manually.
The necessary examples are provided in the demo application.

## Library Development

This section is intended only for library developers and contributors.

The library design document can be found in [DESIGN.md](DESIGN.md)

Contributors, please follow the [Zubax C++ Coding Conventions](https://kb.zubax.com/x/84Ah).

### Building and Running Tests

```bash
mkdir build && cd build
cmake ../libcanard/tests    # Adjust path if necessary
make
./run_tests
```

### Submitting a Coverity Scan Build

First, [get the Coverity build tool](https://scan.coverity.com/download?tab=cxx).
Then build the tests with it:

```bash
export PATH=$PATH:<coverity-build-tool-directory>/bin/
mkdir build && cd build
cmake ../libcanard/tests -DCMAKE_BUILD_TYPE=Debug   # Adjust path if necessary
cov-build --dir cov-int make -j8
tar czvf libcanard.tgz cov-int
```

Then upload the resulting archive to Coverity.
