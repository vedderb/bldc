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

//#define HW_SOURCE "hw_rh.c"
//#define HW_HEADER "hw_rh.h"

//#define HW_SOURCE "hw_tp.c"
//#define HW_HEADER "hw_tp.h"

//#define HW_SOURCE "hw_75_300.c"
//#define HW_HEADER "hw_75_300.h"

//#define HW_SOURCE "hw_mini4.c"
//#define HW_HEADER "hw_mini4.h"

//#define HW_SOURCE "hw_das_mini.c"
//#define HW_HEADER "hw_das_mini.h"

//#define HW_SOURCE "hw_uavc_qcube.c"
//#define HW_HEADER "hw_uavc_qcube.h"

//#define HW_SOURCE "hw_uavc_basic.c"
//#define HW_HEADER "hw_uavc_basic.h"
```
There are also many other options that can be changed in conf_general.h


## Prerequisites

### On Ubuntu

Install the gcc-arm-embedded toolchain. Recommended version ```gcc-arm-none-eabi-7-2018-q2```  

**Method 1 - Through Official GNU Arm Embedded Toolchain Downloads**
1. Go to [GNU Arm Embedded Toolchain Downloads](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-rm/downloads)
2. Locate and Download version **gcc-arm-none-eabi-7-2018-q2** for your machine  
   ```GNU Arm Embedded Toolchain: 7-2018-q2-update     June 27, 2018```  
   Linux 64-bit version can be downloaded from [here](https://developer.arm.com/-/media/Files/downloads/gnu-rm/7-2018q2/gcc-arm-none-eabi-7-2018-q2-update-linux.tar.bz2?revision=bc2c96c0-14b5-4bb4-9f18-bceb4050fee7?product=GNU%20Arm%20Embedded%20Toolchain,64-bit,,Linux,7-2018-q2-update)  
3. Unpack the archive in the file manager by right-clicking on it and select "extract here"
4. Change directory to the unpacked folder, unpack it in /usr/local by execute the following command
   ```
   cd gcc-arm-none-eabi-7-2018-q2-update-linux  
   sudo cp -RT gcc-arm-none-eabi-7-2018-q2-update/ /usr/local  
   ```

**Method 2 - Through apt install**
```bash
sudo add-apt-repository ppa:team-gcc-arm-embedded/ppa
sudo apt update
sudo apt install gcc-arm-embedded
```


**Optional - Add udev rules to use the stlink v2 programmer without being root**
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
Clone and build the firmware

```bash
git clone https://github.com/vedderb/bldc.git vesc_firmware
cd vesc_firmware
make
```


## Upload to VESC
### Method 1 - Flash it using an STLink SWD debugger

Build and flash the [bootloader](https://github.com/vedderb/bldc-bootloader) first

```bash
make upload
```
### Method 2 - Upload Firmware via VESC tool through USB

1. Clone and build the firmware in **.bin** format

Reminder : Remember to select your board in [**conf_general.h**]

```bash
git clone https://github.com/vedderb/bldc.git vesc_firmware
cd vesc_firmware
#make build/your_firmware_name_here.bin
make build/VESC_fw.bin 
```
In VESC tool

2. Connect to the VESC
3. Navigate to the Firmware tab on the left side menu 
4. Click on Custom file tab
5. Click on the folder icon to select the built firmware in .bin format (e.g. VESC_fw.bin)  

##### [ Reminder : It is normal to see VESC disconnects during the firmware upload process ]  
#####  **[ Warning : DO NOT DISCONNECT POWER/USB to VESC during the upload process, or you will risk bricking your VESC ]**  
#####  **[ Warning : ONLY DISCONNECT your VESC 10s after the upload loading bar completed and "FW Upload DONE" ]**

6. Press the upload firmware button (downward arrow) on the bottom right to start upload the selected firmware.
7. Wait for **10s** after the loading bar completed (Warning: unplug sooner will risk bricking your VESC)
8. The VESC will disconnect itself after new firmware is uploaded.

## In case you bricked your VESC
you will need to upload a new working firmware to the VESC.  
However, to upload a firmware to a bricked VESC, you have to use a SWD Debugger.


## Contribute

Head to the [forums](https://vesc-project.com/forum) to get involved and improve this project.


## License

The software is released under the GNU General Public License version 3.0
