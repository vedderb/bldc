*****************************************************************************
*** Files Organization                                                    ***
*****************************************************************************

--{root}                  - ChibiOS/RT directory.
  +--readme.txt           - This file.
  +--documentation.html   - Shortcut to the web documentation page.
  +--license.txt          - GPL license text.
  +--exception.txt        - GPL exception text (stable releases only).
  +--demos/               - Demo projects, one directory per platform.
  +--docs/                - Documentation.
  |  +--common/           - Documentation common build resources.
  |  +--hal/              - Builders for HAL.
  |  |  +--Doxyfile_*     - Doxygen project files (required for rebuild).
  |  |  +--html/          - Local HTML documentation (after rebuild).
  |  |  +--reports/       - Test reports.
  |  |  +--rsc/           - Documentation resource files (required for rebuild).
  |  |  +--src/           - Documentation source files (required for rebuild).
  |  |  +--Doxyfile_*     - Doxygen project files (required for rebuild).
  |  |  +--index.html     - Local documentation access (after rebuild).
  |  +--nil/              - Builders for NIL.
  |  |  +--Doxyfile_*     - Doxygen project files (required for rebuild).
  |  |  +--html/          - Local HTML documentation (after rebuild).
  |  |  +--reports/       - Test reports.
  |  |  +--rsc/           - Documentation resource files (required for rebuild).
  |  |  +--src/           - Documentation source files (required for rebuild).
  |  |  +--Doxyfile_*     - Doxygen project files (required for rebuild).
  |  |  +--index.html     - Local documentation access (after rebuild).
  |  +--rt/               - Builders for RT.
  |  |  +--html/          - Local HTML documentation (after rebuild).
  |  |  +--reports/       - Test reports.
  |  |  +--rsc/           - Documentation resource files (required for rebuild).
  |  |  +--src/           - Documentation source files (required for rebuild).
  |  |  +--Doxyfile_*     - Doxygen project files (required for rebuild).
  |  |  +--index.html     - Local documentation access (after rebuild).
  +--ext/                 - External libraries, not part of ChibiOS/RT.
  +--os/                  - ChibiOS components.
  |  +--hal/              - HAL component.
  |  |  +--boards/        - HAL board support files.
  |  |  +--dox/           - HAL documentation resources.
  |  |  +--include/       - HAL high level headers.
  |  |  +--lib/           - HAL libraries.
  |  |  +--osal/          - HAL OSAL implementations.
  |  |  +--src/           - HAL high level source.
  |  |  +--ports/         - HAL ports.
  |  |  +--templates/     - HAL driver template files.
  |  |     +--osal/       - HAL OSAL templates.
  |  +--nil/              - NIL RTOS component.
  |  |  +--dox/           - NIL documentation resources.
  |  |  +--include/       - NIL high level headers.
  |  |  +--src/           - NIL high level source.
  |  |  +--ports/         - NIL ports.
  |  |  +--templates/     - NIL port template files.
  |  +--rt/               - RT RTOS component.
  |  |  +--dox/           - RT documentation resources.
  |  |  +--include/       - RT high level headers.
  |  |  +--src/           - RT high level source.
  |  |  +--ports/         - RT ports.
  |  |  +--templates/     - RT port template files.
  |  +--various/          - Various portable support files.
  +--test/                - Kernel test suite source code.
  |  +--lib/              - Portable test engine.
  |  +--hal/              - HAL test suites.
  |  |  +--testbuild/     - HAL uild test and MISRA check.
  |  +--nil/              - NIL test suites.
  |  |  +--testbuild/     - NIL nuild test and MISRA check.
  |  +--rt/               - RT test suites.
  |  |  +--testbuild/     - RT build test and MISRA check.
  |  |  +--coverage/      - RT code coverage project.
  +--testhal/             - HAL integration test demos.

*****************************************************************************
*** Releases and Change Log                                               ***
*****************************************************************************

*** 3.0.5 ***
- VAR: Fixed missing time convesion in lwIP arch module (bug #696).
- NIL: NIL_CFG_USE_EVENTS not properly checked in NIL (bug #694).
- RT:  Fixed ISR statistics are not updated from a critical zone in RT
       (bug #693).
- NIL: Fixed NIL test suite calls I and S functions outside critical zone
       (bug #692).
- NIL: Fixed protocol violation in NIL OSAL (bug #691).
- HAL: Fixed STM32 CAN driver always broadcasts tx complete for mailbox 1
       (bug #690).
- RT:  Fixed tm_stop - best case bug (bug #688).
- RT:  Fixed ARM port enforcing THUMB mode (bug #687).
- HAL: Fixed HAL drivers still calling RT functions (bug #686).
- HAL: Fixed Serial_USB improperly resets queues on stop (bug #685).
- HAL: Fixed chprintf() still calling RT functions (bug #684).
- ALL: Several minor documentation/formatting-related fixes.

*** 3.0.4 ***
- HAL: Fixed STM32 ICU driver uses chSysLock and chSysUnlock (bug #681).
- HAL: Fixed wrong DMA priority assigned to STM32F3 ADC3&4 (bug #680).
- HAL: Fixed invalid DMA settings in STM32 DACv1 driver in dual mode
      (bug #677).
- HAL: Fixed usbStop() hangs in STM32 OTGv1 driver (bug #674).
- HAL: Fixed STM32 I2Cv2 driver fails on transfers greater than 255 bytes
       (bug #673).
- HAL: Fixed STM32 I2Cv2 DMA conflict (bug #671).
- HAL: Fixed invalid macro in STM32F0xx registry (bug #668).
- HAL: Fixed I2S clock selection not working in STM32F4xx HAL (bug #667).
- HAL: Fixed STM32 USBv1 broken isochronous endpoints (bug #662).
- HAL: Fixed STM32 USBv1 wrong multiplier when calculating descriptor address
       in BTABLE (bug #661).
- HAL: Fixed STM32 USBv1 does not make use of BTABLE_ADDR define(bug #660).

*** 3.0.3 ***
- HAL: Fixed differences in STM32F3 ADC macro definitions (bug #665).
- HAL: Fixed RTC module loses day of week when converting (bug #664).
- HAL: Fixed invalid class type for sdPutWouldBlock() and sdGetWouldBlock()
       functions (bug #659).
- HAL: Fixed STM32L1xx HAL errors in comments (bug #657).
- HAL: Fixed STM32 USBv1 wrong buffer alignment (bug #656).
- HAL: Fixed Wrong vector name for STM32F3xx EXTI33 (bug #655).
- HAL: Fixed nvicEnableVector broken for Cortex-M0 (bug #654).
- HAL: Fixed missing RCC and ISR definitions for STM32F0xx timers (bug #651).
- HAL: Fixed incorrect compiler check in STM32 RTCv1 driver (bug #650).
- HAL: Fixed STM32F3xx HAL checking for non-existing macros (bug #648).
- HAL: Fixed error in STM32F030 EXT driver (bug #647).
- RT:  Fixed problem with chTimeIsWithin() (bug #646).
- VAR: Fixed _sbrk_r with incr == 0 should be valid (bug #645).
- RT:  Fixed issues in CMSIS RTOS interface (bug #644).
- HAL: Fixed rtcConvertDateTimeToFAT() incorrect conversion (bug #615).

*** 3.0.2 ***
- HAL: Fixed RT dependency in STM32 SDCv1 driver (bug #643).
- VAR: Fixed incorrect working area size in LwIP creation in demos (bug #642).
- HAL: Fixed error in hal_lld_f100.h checks (bug #641).
- HAL: Fixed volatile variable issue in I/O queues, both RT and HAL (bug #640).
- HAL: Fixed wrong DMA assignment for I2C1 in STM32F302xC registry (bug #637).
- HAL: Fixed missing timers 5, 6, 7, 10 & 11 from STM32L1 HAL port (bug #636).
- VAR: Fixed CRT0_CALL_DESTRUCTORS not utilized in crt0_v7m.s (bug #635).
- DEM: Fixed wrong ld file in STM32F072xB USB CDC demo (bug #634).
- NIL: Fixed wrong assertion in NIL chSemResetI() and NIL OSAL
       osalThreadDequeueAllI() (bug #633).
- RT:  Fixed problem with RT mutexes involving priority inheritance (bug #632).
- HAL: Fixed HAL to RT dependency in STM32 DAC driver (bug #631).
- HAL: Fixed problem with STM32 I2S driver restart (bug #630).
- NIL: Added polled delays required to fix bug #629.
- HAL: Fixed STM32F3xx ADC driver uses US2RTC directly (bug #629).

*** 3.0.1 ***
- HAL: Fixed CEC clock cannot be disabled on STM32F0xx (bug #628).
- VAR: Fixed lwIP arch code breaks with a 16-bit systick timer (bug #627).
- HAL: Fixed broken MAC driver for STM32F107 (bug #626).
- NIL: Fixed missing configuration options from NIL PPC port (bug #625).
- HAL: Fixed wrong offset in STM32 DAC driver (bug #624).
- HAL: Fixed crash on STM32F030x4/6 devices (bug #623).
- HAL: Fixed duplicated doxygen tag in STM32F4xx hal_lld.h file (bug #621).
- HAL: Fixed STM32F042 registry error (bug #620).
- HAL: Fixed wrong check in canReceive() (bug #619).
- HAL: Fixed wrong EXTI[18] vector number on STM32F373 (bug #618).
- HAL: Fixed wrong check on STM32_LSE_ENABLED definition in STM32L1xx HAL port
       (bug #617).

*** 3.0.0 ***
- NEW: Added an initialization function to the lwIP bindings, now it is
       sufficient to call lwipInit(NULL); in order to start the subsystem.
       Demo updated.
- RT:  Fixed compilation error in RT when registry is disabled (bug #614).
- NIL: Fixed OSAL_ST_MODE not defined in AVR port (bug #613).
- NIL: Fixed nilrtos redefinition of systime_t (bug #611).
- HAL: Fixed TIM2 wrongly classified as 32bits in STM32F1xx devices
       (bug #610).

*** 3.0.0p6 ***
- HAL: Removed call to localtime_r() function for non-GNU compilers in
       STM32F1xx RTC driver.
- DEM: Fixed the FatFS demo timeout, now it is expressed in milliseconds.
- DEM: Added -Wundef to all the demos and test programs in order to find
       common error cases.
- NIL: Added INTC priorities check to the e200z port.
- RT:  Added INTC priorities check to the e200z port.
- HAL: Added support for CAN in STM32F042/72 devices.
- HAL: Added support for extra DMA channels in STM32F072 devices.
- HAL: Modified the STM32 CAN driver to support unified IRQs.
- RT:  SPE-related issue in e200z ports (bug #607).
- NIL: SPE-related issue in e200z ports (bug #607).
- HAL: Fixed dependency between STM32 MAC driver and RT (bug #606).
- HAL: Fixed wrong macro names in STM32F0xx HAL driver (bug #605).
- HAL: Fixed wrong check on ADC3 in STM32F3xx ADC driver (bug #604).
- HAL: Fixed wrong macro names in STM32F3xx HAL driver (bug #603).
- HAL: Fixed errors in STM32 OTGv1 driver (bug #601).
- DEM: Fixed missing paths in e200z demos (bug #600).
- HAL: Fixed error in platform_f105_f107.mk file (bug #599).
- HAL: Fixed issue in DMA drivers when channels share ISRs (bug #597).

*** 3.0.0p5 ***
- HAL: Added no-DMA mode to the STM32 I2Cv2 driver.
- HAL: Added DAC support to all STM32 sub-platforms, added another demo for
       the STM32F3xx.
- HAL: Fixed STM32 USARTv1: incorrect txend2_cb callback behavior (bug #596).
- DEM: Fixed wrong comment in ARMCM4-STM32F401RE-NUCLEO demo (bug #595).
- HAL: Fixed STM32 SDC LLD driver initialization with Asserts disabled
       (bug #594).

*** 3.0.0p4 ***
- NEW: Added no-DMA mode to STM32 I2Cv2 driver.
- BLD: New "smart build" mode added to makefiles, now only used files are
       compiled.
- HAL: Change to the Serial_USB driver, now the INT endpoint is no more
       mandatory.
- HAL: New DAC driver implementation for STM32F4xx.
- HAL: Fixed SDC STM32 driver broken in 50MHz mode (bug #592).
- HAL: Fixed STM32 RTC SSR Register Counts Down (bug #591).
- HAL: Fixed STM32 RTC PRER Register not being set in init (bug #590).
- HAL: Fixed STM32F334 does not have an EXT18 interrupt (bug #588).
- HAL: Fixed STM32L1xx USB is missing disconnect/connect macros (bug #587).
- HAL: Fixed wrong vector number for STM32L1xx USB (bug #586).
- HAL: Fixed spurious TC interrupt in STM32 UART (v1 and v2) driver (bug #584).
- HAL: Fixed invalid checks on STM32L1xx LSI and LSE clocks (bug #583).
- HAL: Fixed RCC CAN2 macros missing in STM32F1xx platform (bug #582).
- HAL: Fixed STM32 I2Cv2 driver issue (bug 581).
- BLD: Fixed ules.mk: adding "PRE_MAKE_ALL_RULE_HOOK" (bug #580).
- BLD: Fixed rules.mk should not explicitly depend on $(BUILDDIR) (bug #579).

*** 3.0.0p3 ***
- RT:  Fixed tickless mode instability in RT (bug 577).

*** 3.0.0p2 ***
- HAL: Fixed instances of RT API in HAL drivers (bug 574).
- RT:  Fixed system time overflow issue in tickless mode (bug 573).
- RT:  Improvements to the IRQ_STORM applications.

*** 3.0.0p1 ***
- First 3.0.0 release, see release note 3.0.0.
