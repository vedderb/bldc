# VESC firmware

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Travis CI Status](https://travis-ci.com/paltatech/bldc.svg?branch=powerdesigns-dev)](https://travis-ci.com/paltatech/bldc)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/92eefb23a0c24b3cbed011b14ca0ffc9)](https://www.codacy.com/app/nitrousnrg/bldc?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=paltatech/bldc&amp;utm_campaign=Badge_Grade)
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
//#define HW_VERSION_40
//#define HW_VERSION_45
//#define HW_VERSION_46 // Also for 4.7
//#define HW_VERSION_48
//#define HW_VERSION_49
//#define HW_VERSION_410 // Also for 4.11 and 4.12
#define HW_VERSION_60
//#define HW_VERSION_R2
//#define HW_VERSION_VICTOR_R1A
//#define HW_VERSION_DAS_RS
//#define HW_VERSION_PALTA
//#define HW_VERSION_RH
//#define HW_VERSION_TP
//#define HW_VERSION_75_300
//#define HW_VERSION_MINI4
//#define HW_VERSION_DAS_MINI
```
There are also many other options that can be changed in conf_general.h


## Prerequisites

On an Ubuntu machine, install the gcc-arm-embedded toolchain


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

Build and flash the [bootloader](https://github.com/vedderb/bldc-bootloader)


## Build

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
