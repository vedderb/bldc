# VESC firmware

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Travis CI Status](https://travis-ci.com/vedderb/bldc.svg?branch=master)](https://travis-ci.com/vedderb/bldc)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/75e90ffbd46841a3a7be2a9f7a94c242)](https://www.codacy.com/app/vedderb/bldc?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=vedderb/bldc&amp;utm_campaign=Badge_Grade)
[![Contributors](https://img.shields.io/github/contributors/vedderb/bldc.svg)](https://github.com/vedderb/bldc/graphs/contributors)
[![Watchers](https://img.shields.io/github/watchers/vedderb/bldc.svg)](https://github.com/vedderb/bldc/watchers)
[![Stars](https://img.shields.io/github/stars/vedderb/bldc.svg)](https://github.com/vedderb/bldc/stargazers)
[![Forks](https://img.shields.io/github/forks/vedderb/bldc.svg)](https://github.com/vedderb/bldc/network/members)

An open source motor controller firmware.

This is the source code for the VESC DC/BLDC/FOC controller. Read more at
[https://vesc-project.com/](https://vesc-project.com/)

## Supported boards

All  of them!

Make sure you select your board in [conf_general.h](conf_general.h)


```c
//#define HW_SOURCE "hw_40.c"
//#define HW_HEADER "hw_40.h"

//#define HW_SOURCE "hw_45.c"
//#define HW_HEADER "hw_45.h"

//#define HW_SOURCE "hw_46.c" // Also for 4.7
//#define HW_HEADER "hw_46.h" // Also for 4.7

//#define HW_SOURCE "hw_48.c"
//#define HW_HEADER "hw_48.h"

//#define HW_SOURCE "hw_49.c"
//#define HW_HEADER "hw_49.h"

//#define HW_SOURCE "hw_410.c" // Also for 4.11 and 4.12
//#define HW_HEADER "hw_410.h" // Also for 4.11 and 4.12

// Benjamins first HW60 PCB with PB5 and PB6 swapped
//#define HW60_VEDDER_FIRST_PCB

// Mark3 version of HW60 with power switch and separate NRF UART.
//#define HW60_IS_MK3

#define HW_SOURCE "hw_60.c"
#define HW_HEADER "hw_60.h"

//#define HW_SOURCE "hw_r2.c"
//#define HW_HEADER "hw_r2.h"

//#define HW_SOURCE "hw_victor_r1a.c"
//#define HW_HEADER "hw_victor_r1a.h"

//#define HW_SOURCE "hw_das_rs.c"
//#define HW_HEADER "hw_das_rs.h"

//#define HW_SOURCE "hw_axiom.c"
//#define HW_HEADER "hw_axiom.h"

//#define HW_SOURCE "hw_luna_bbshd.c"
//#define HW_HEADER "hw_luna_bbshd.h"

//#define HW_SOURCE "hw_rh.c"
//#define HW_HEADER "hw_rh.h"

//#define HW_SOURCE "hw_tp.c"
//#define HW_HEADER "hw_tp.h"

// Benjamins first HW75_300 PCB with different LED pins and motor temp error
//#define HW75_300_VEDDER_FIRST_PCB

// Second revision with separate UART for NRF51
#define HW75_300_REV_2

//#define HW_SOURCE "hw_75_300.c"
//#define HW_HEADER "hw_75_300.h"

//#define HW_SOURCE "hw_mini4.c"
//#define HW_HEADER "hw_mini4.h"

//#define HW_SOURCE "hw_das_mini.c"
//#define HW_HEADER "hw_das_mini.h"

//#define HW_SOURCE "hw_uavc_qcube.c"
//#define HW_HEADER "hw_uavc_qcube.h"

//#define HW_SOURCE "hw_uavc_omega.c"
//#define HW_HEADER "hw_uavc_omega.h"

//#define HW_SOURCE "hw_binar_v1.c"
//#define HW_HEADER "hw_binar_v1.h"

//#define HW_SOURCE "hw_hd.c"
//#define HW_HEADER "hw_hd.h"

//#define HW_SOURCE "hw_a200s_v2.c"
//#define HW_HEADER "hw_a200s_v2.h"

//#define HW_SOURCE "hw_rd2.c"
//#define HW_HEADER "hw_rd2.h"

//#define HW_SOURCE "hw_100_250.c"
//#define HW_HEADER "hw_100_250.h"

```
There are also many other options that can be changed in conf_general.h


## Prerequisites

### On Ubuntu

Install the gcc-arm-embedded toolchain


```bash
sudo add-apt-repository ppa:team-gcc-arm-embedded/ppa
sudo apt update
sudo apt install gcc-arm-embedded
```

Add udev rules to use the stlink v2 programmer without being root


```bash
wget vedder.se/Temp/49-stlinkv2.rules
sudo mv 49-stlinkv2.rules /etc/udev/rules.d/
sudo udevadm trigger
```

### On MacOS

Go to the [GNU ARM embedded toolchain downloads Website](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-rm/downloads) and select the mac version, download it and extract it to your user directory.

Append the bin directory to your **$PATH**. For example:


```bash
export PATH="$PATH:/Users/your-name/gcc-arm-none-eabi-8-2019-q3-update/bin/"
```

Install stlink and openocd


```bash
brew install stlink
brew install openocd
```

## Build

Build and flash the [bootloader](https://github.com/vedderb/bldc-bootloader) first

Clone and build the firmware

```bash
git clone https://github.com/vedderb/bldc.git vesc_firmware
cd vesc_firmware
make
```

Flash it using an STLink SWD debugger

```bash
make upload
```

## Contribute

Head to the [forums](https://vesc-project.com/forum) to get involved and improve this project.


## License

The software is released under the GNU General Public License version 3.0
