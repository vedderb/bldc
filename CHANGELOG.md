### 6.05
#### TBD
* LispBM:
	* Support for var in progn.
	* Support for curly brackets instead of progn.
	* Added set and setq.
	* Added defunret.
	* Added support for detaching only one of the ADCs.
	* Added set-encoder.
	* Support for moving code and data to flash.
	* Incremental read of uploaded code.
	* Removed array types other than byte arrays.
	* Added more position extensions.
	* Use timeout when app_adc is detached.
	* Added extension conf-restore-mc.
	* Added extension conf-restore-app.
	* Added extension conf-dc-cal.
	* Added access to several config parameters.
	* Many improvements and bug fixes.
	* Better error descriptions.
	* Added thread profiler.
	* Added flat value support.
	* Added recv-to with a timeout argument.
	* Added remote message extensions.
	* Added phase-hall extension.
	* Added loopwhile-thd.
	* Added can-recv-sid.
	* Added can-recv-eid.
	* Added app-adc-range-ok.
	* Much faster return on error for i2c-tx-rx.
	* Added shutdown-hold.
	* Added crc32.
	* Support for only polling variables starting with VT.
	* Added rand.
	* Built-in sort function that is much faster and can sort much larger lists.
	* Added foc-play-tone and foc-play-stop.
	* Added foc-play-samples.
	* Added can-msg-age.
	* Massive performance boost for string functions and 64-bit operations due to much faster memory allocation.
* Hall sensors improvements:
	* Smooth transition to sensorless.
	* Bug fix in interpolation.
	* Use less noisy speed estimator for interpolation.
	* Adjusted rate limit.
* Added soft regen cutoff. See https://github.com/vedderb/vesc_tool/pull/310
* Attempt at limiting the input current when using MTPA and field weakening.
* Removed built-in balance app. The balance-package can be used instead, which is where new development is done.
* Added option to select PID-controller speed source.
* Try to make odometer persistent even without shutdown switch. See https://github.com/vedderb/bldc/pull/624
* Dual motor flux linkage measurement current spike bug fix.
* Fix possible runaway after faults during openloop measurements (flux linkage, encoder).
* Added more current sampling modes.
* Added FOC interpolation mode.
* Allow passive flux linkage measurement.
* Fixed some braking glitches.
* Configurable HFI error truncation to reject noise.
* Removed GPDrive.
* FOC: Option to short phases on 0 duty.
* FOC: Added MXV observer.

### 6.02
#### 2023-03-12
* LispBM:
	* Bug fixes.
	* Added floor, ceil, round.
	* Added resistance, inductance and flux linkage estimator functions.
	* Added adc_ctrl_type setting.
	* Added app_is_disabled.
	* Added ICU-driver.
	* Inductance measurement support.
	* Rewrite of the event system.
	* Added canget-vin
	* Added hw-type sysinfo.
	* Added crc16-extension.
* IMU filtering changes and new defaults.
* Increased hall sensor switch hysteresis.
* Added and updated hardware configs.
* Systime overflow fix.
* Some can-command fixes.

---

### 6.00
#### Released 2022-12-08
* Added stack checks.
* Release motor fix when cc_min_current is 0.
* Added support for NTC and PTC temperature sensors with custom resistance and base temperature.
* Added sandboxed lisp scripting using https://github.com/svenssonjoel/lispBM.
* Encoder driver split and rewrite to make it easier to support more encoders.
* Flash writes now work on lower MCU voltages too.
* Added fwinfo terminal command.
* CAN status messages are now configured using bitfields.
* There are two rates at which CAN status messages can be sent now.
* New silent HFI mode with much better performance at high load.
* Added app ADC min and max voltage limits for throttle fault detection.
* Another new silent HFI mode that is less sensitive to getting the inductance correct.
* Added openloop current boost parameter.
* Added openloop current max parameter.
* Added support for dynamic loading of C libraries.
* Added more observers:
	* mxlemming
	* ortega with flux linkage tracking
	* mxlemming with flux linkage tracking
* AS5x47 encoder support: https://github.com/vedderb/bldc/pull/511
* Disable BMS limit options.
* Added PT1000 temperature sensor support.
* APP ADC button bitfield and CC disable support.
* BissC encoder support: https://github.com/vedderb/bldc/pull/536
* Better detection failt handling and reporting: https://github.com/vedderb/bldc/pull/533
* TLE5012-support: https://github.com/vedderb/bldc/pull/551

---

### 5.03
#### Released 2022-01-16
* Fixed inductance measurement bug.
* Speed tracker windup protection.
* Phase filter support.
* Phase voltage offset calibration.
* Better current offset calibration.
* Added power switch commands.
* Synchronize observer state when running in open loop.
* Force oberver state magnitude above 50% of flux linkage. This prevents the motor from getting stuck and 'screaming'.
* Observer global convergence update. Helps tracking the motor through 0 speed.
* Added HFI start sensor mode.
* Added TEMP_SENSOR_KTY84_130.
* Major UAVCAN update. See: https://github.com/vedderb/bldc/pull/269
* Avoid numerical instability when mapping is done over a narrow range. See: https://github.com/vedderb/bldc/issues/262
* App Balance updates.
* Added servo_out_enable appconf option, so that the PPM port can be used to control servos with the default firmware.
* Better current controller windup protection.
* Field weakening support (experimental, be careful and use at your own risk).
* Use filtered current for axis decoupling.
* Odometer is now persistent after firmware updates.
* Added hw runtime counter (see hw_status in terminal)
* Added custom UI support.
* Limit hall sensor angle rate of change based on ERPM.
* Added p_pid_gain_dec_angle parameter.
* Low pass filter input voltage.
* Dual hardware CAN-scan fix.
* Dynamic QML-script write support.
* Use fast speed tracker for current controller.
* Disable motor for 5 seconds after flash operations.
* Added kill switch support.
* Added process derivative term to position controller.
* Added position PID-controller angle offset.
* Configurable PID controller rate.
* Added several AUX port modes.
* Added configurable safe start modes.
* Added fusion IMU filter.
* Added constant torque PAS mode.
* Correct scaling for resistance and inductance.
* Fixed inductance measurement bug with f_sw > 30k.
* Corrected inductance measurement algorithm.
* Fixed max power loss calculation.
* Better input current estimation.
* Added raw sampled data mode.
* Compensate inductance for motor saliency in observer.
* Added MTPA mode based on measured current.
* Faster overvoltage protection.
* Added statistics counters.
* Added configurable observer offset.

---

### 5.02
#### Released 2021-01-11
* IMU calibration improvement.
* Added COMM_GET_MCCONF_TEMP command.
* Added bidirectional current command to VESC remote.
* Fixed motor temperature reading on hw with ADC mux.
* Added speed PID input ramping option.
* Added LSM6DS3 IMU support.
* Added MTPA support. See See: https://github.com/vedderb/bldc/pull/179
* Added HW_HD75 support.
* NRF52 UICR write fix.
* App PPM rework. See https://github.com/vedderb/bldc/pull/192
* Added bm_reset terminal command.
* Added bm support for STM32F30x and STM32L47x.
* App Balance updates. See https://github.com/vedderb/bldc/pull/193
* Motor current now based on magnitude of both axes.
* Initial VESC BMS support.
* Hall sensor interpolation improvement.
* Made hall sensor filter configurable.
* Added locking time and ramp up time parameters to sensorless startup.
* Removed D axis current injection.
* Initial VESC IO-board support.
* Added hall sensor interpolation ERPM config option.
* Use fast speed estimator for RPM limit.
* Avoid accumulated rounding error when using PID position angle division.
* Added UAVCAN raw throttle drive mode (current or duty cycle control).
* Added MT6816 encoder support. See https://github.com/vedderb/bldc/pull/238
* Added modulation-based D axis current controller gain scaling. Addresses https://github.com/vedderb/bldc/pull/220
* Added PAS app. See: https://github.com/vedderb/bldc/pull/243
* Added 100k CAN-baudrate.
* Added 100k NTC temperature sensor support.

---

### 5.01
#### Released 2020-04-27
* Fixed PPM bug in previous release.

---

### 5.00
#### Released 2020-04-27
* Dual motor support. VESC-based controllers such as the focbox unity will now work.
* Fixed bug in cross BEMF decoupling.
* Disable CC decoupling during flux linkage measurement.
* Balance app updates. See: https://github.com/vedderb/bldc/pull/141
* Observer gain calculation update.
* Better observer gain scaling. This has a large impact on some motors.
* Added test build flag, that is transmitted with the FW Version command.
* Detect all bug fix.
* Added COMM_SET_BATTERY_CUT command.
* Added CAN_PACKET_SHUTDOWN CAN-command.
* GPDRIVE output sample fix.
* Some ISR optimization.
* Updated ChibiOS to version 3.0.5.
* Increased USB thread priority to avoid freeze during connect/disconnect on windows.
* Smooth current ramping during resistance measurement.
* Moved fault stop to thread, and added SPI mutexes to DRV drivers.
* Smooth current ramping in flux linkage measurement and sensor detection.
* Added hall_analyze terminal command.
* Motor temperature filtering bug fix.
* Inductance measurement scaling fix.
* Better flux linkage measurement.
* Improved battery level and range estimation.
* Use fast speed estimator for encoder sensorless transition.
* Signigicantly improved hall sensor transitions and interpolation.
* More filtering on the motor temperature.
* Added IMU orientation calibration support: https://github.com/vedderb/bldc/pull/158
* Added mcconf_l_duty_start so that the current can be limited smoothly when reaching max speed.
* Allow throttle in opposite direction even after passing speed limit for PPM and VESC Remote apps.

---

### 4.02
#### Released 2020-03-06
* Position PID fix (most notable on multiturn encoders).
* App balance updates. See https://github.com/vedderb/bldc/pull/138.
* Changed FOC time constant back to 1000 us.
* Do not count AS5047 all ones as fault.
* Improved axis decoupling and integrator windup protection. Should prevent wobbles.

---

### 4.01
#### Released 2020-02-01
* Leave debug mode on NRF5x after disconnect to avoid excess power consumption before power cycle.
* Added encoder_clear_errors and encoder_clear_multiturn terminal commands.
* Initialize current offsets to 2048 to avoid a fault code to be logged at boot.
* Added 10K, 20K, 50K and 75K CAN baud rates.
* Added very basic TS5700N8501 multiturn mode.

---

### 4.00
#### Released 2020-01-28
* Added support for HFI to track motor position at 0 speed without sensors. This is the main new feature of FW 4.
* Fixed CAN-bug in VESC Remote.
* Reset current integrator when leaving duty cycle control mode. Fixes braking issue https://github.com/vedderb/bldc/issues/125.
* More accurate and faster inductance measurement.
* Ability to measyre ld - lq. Useful for MTPA in future firmwares.
* Reset shutdown when uploading FW data.
* Added CAN-bride and COMM_CAN_FWD_FRAME.
* Added CAN_PACKET_POLL_TS5700N8501_STATUS to poll most relevant data last received from the TS5700N8501 encoder.
* Added TS57N8501 ABM, SF and ALMC to encoder terminal command.

---

### 3.66
#### Released 2020-01-12
* Added support for HW 100/250.
* Added uptime terminal command.
* Added some delays to DRV8323s SPI driver.
* Added SWD support for NRF52840 with idcode 0x015B.
	* TODO: Have a look at https://github.com/blacksphere/blackmagic/commit/302ff20a6d5b806c09e0ca7e996beab3ef3596f4.
* Fixed INVERTED_SHUNT_POLARITY for BLDC.
* Added decoupling to FOC current controller.
* Better motor tracking at high ERPM and low Fsw.
* Made uart and permanent uart more independent.
* Do not write to USB if cable has not been connected.
* Added timeout to USB write.
* Better FOC current control integrator windup protection.
* Added FOC observer type selection options.
* Print TS5700N8501 position in encoder terminal command.

---

### 3.65
#### Released 2019-12-22
* Added support for PTC motor temperature sensor (e.g. KTY84)
* APP_PPM sleep fix. Should solve CAN issues.

---

### 3.64
#### Released 2019-12-19
* Added support for HW60_MK3
* Disable shutdown sampling when the watchdog runs slowly.
* Added COMM_SET_CURRENT_REL.
* Added support for boot/ok signal.
* Added unused mode for aux port.
* Fixed positive ramping time setting.
* Changed PPM timeout handling.
* IRQ priority fix: SYSTICK < UART < MCPWM. Possibly related to http://www.chibios.com/forum/viewtopic.php?f=3&t=4665.

---

### 3.63
#### Released 2019-12-05
* NRF remote power meter is now unaffected by temperature decrease and speed limits.
* Added LZO compression support to firmware upload, making firmware updates 30% - 50% faster.
* Added LZO compression support to SWD upload.
* Made serial interrupts priority higher than PWM so that higher speed UART is possible.
* Added support for TS5700N8501 encoder (via COMM port).
* Better observer gain calculation.

---

### 3.62
#### Released 2019-09-27
* Added COMM_BM_MEM_READ.
* Merged EUC app (experimental).
* Fixed NRF remote reverse bug.
* Do not stop FOC on configuration updates if not needed.

---

### 3.61
#### Released 2019-09-09
* Added PPM_CTRL_TYPE_CURRENT_SMART_REV mode.

---

### 3.60
#### Released 2019-09-08
* Fixed IMU9x50 bug.
* Unrigester ICM20948 terminal callbacks when unused.
* Added experiment plot functions.
* Added D and Q axis voltage to RT data.
* Added smart reverse function to nunchuk app.

---

### 3.59
#### Released 2019-09-03
* Added more data to MOTE_PACKET_ALIVE.
* Added app template.
* Added function to unregister terminal callbacks.
* Added BMI160 support.
* Added support for the VESC HD.
* Added support for SWD programming permanent NRF.
* Encoder SW SPI fix.
* Slightly faster boot.
* Moved custom HW and APP configurations to conf_general.h.
* Added support for passing HW and APP default configuration as make arguments.
* Added SW shutdown support.
* Added command to erase bootloader.
* Added function to stop IMU threads, so that IMUs can be switched during runtime.
* Only generate encoder fault when the ERPM is low enough to use the encoder.
* Added many IMU and AHRS settings to appconf.
* Re-initialize IMU when appconf is written.
* Added imu_gyro_info terminal command.

---

### 3.58
#### Released 2019-07-01
* Set motor to FOC mode after successful FOC detection instead of the default type for the hardware.
* APP_ADC: Do not send brake command over CAN if config.multi_esc is not set.
* APP_PPM: Make pulses invalid if they are above 150 % instead of 120 %.
* Introduced a new control mode that allows reverse with hysteria (@ackmaniac port)

---

### 3.57
#### Released 2019-05-16
* Added CAN status message 5 with input voltage and tachometer data.
* Fix github issue https://github.com/vedderb/bldc/issues/94.
* Use default F_SW for HW after autodetect FOC.

---

### 3.56
#### Released 2019-05-03
* Fixed current offset fault bug in non-FOC mode.
* Multiple IMU support.
* Added support for the ICM-20948 IMU.
* Decreased ERPM cut in open loop flux linkage measurement.

---

### 3.55
#### Released 2019-04-26
* Initial sin/cos encoder support.
* New ADC control mode.
* Virtual motor support.
* Disable chuk cruise control on dropouts.
* Fix multiple VESCs over CAN duty cycle mode.
* Added boot and runtime flash memory CRC integrity check.
* Updated chuk RPM filter and moved it to the beginning of the output thread.
* Decreased chuk output thread rate from 1 kHz to 200 Hz.
* Added mpu_read_reg terminal command.
* Fixed DRV8301 fault readout bug.
* Added unbalanced current detection.
* Added high current offset detection.

---

### 3.54
#### Released 2019-03-31
* Added mcpwm_foc_set_openloop_duty and mcpwm_foc_set_openloop_duty_phase.
* Added blackmagic probe SWD output to program other MCUs.
	* Can be used to flash bricked VESCs from a working one.
	* Can be used to make a custom NRF5x module.

---

### 3.53
#### Released 2019-03-20
* Limit foc_current_filter_const range to prevent damage due to bad configuration.
* Set default NRF speed to 1 Mbit/s.
* Use lower switching frequency when detecting resistance to reduce deat-time distortion.
* Don't enable temperature compensation in auto detection by default.

---

### 3.52
#### Released 2019-03-10
* Added support for second revision of HW75/300 with separate UART for NRF51.
* Added COMM_TERMINAL_CMD_SYNC, which does not drop commands when busy.
* Added option to disable permanent UART.
* Moved TIM5 to own file, and use it from other places.
* Removed need for TIM2 in mcpwm.
* Added utilization percentage to threads terminal command.
* Added IMU interface.
* Added support for the MPU9150 and MPU9250.
* Added COMM_GET_IMU_DATA.

---

### 3.51
#### Released 2019-03-04
* Fixed AS5047 error rate bug at position 0.
* Increased threshold for AS5047 fault to 5 %.
* Set correct V_REG value for HW 75/300.
* Better command processing.
* Proper scaling when setting relative currents and acceleration and braking currents are different.

---

### 3.50
#### Released 2019-03-01
* AS5047 parity check and fault code on error rates > 1 %.
* Signature on mc and app configuration.
* FOC loop frequency truncation on all hardwares.

---

### 3.49
#### Released 2019-03-01
* New watchdog implementation.
* HW updates.
* Fixed DC motor current sampling issue.
* Deadtime in nanoseconds instead of register value.
* Use fastest ramping time when throttle is applied.

---

### 3.48
#### Released 2019-02-18
* Added pairing flag to appconf.
* Decreased CAN TX timeout.

---

### 3.47
#### No official release
* Current percentage limits.
* Mcconf_temp based on current scale instead of absolute current.
* Removed battery current from mcconf_temp.
* Added current scale parameter.
* Different braking behavior: prefer cogging over locking the brakes.

---

### 3.46
#### No official release
* DC motor RPM measurement and RPM control when using encoder.
* Support for configurable current low pass filter.
* Much better recovery when failing to decode packets.
* Run all detect functions in separate thread.
* Fixed bug introduced when adding support for dual UARTs.
* Added support for reverse state on NRF remote.
* Support to disable app output for a specified time.

---

### 3.45
#### No official release
* Default CAN ID from UUID, and hook to define it in hwconf.
* CAN ping support.
* Simultaneous firmware update over CAN-bus.
* Fully automated motor detection, based on maximum motor power losses.
* Sensor autodetection and configuration support.
* Softer encoder detection.
* Better NRF_EXT support.
* New more reliable flux linkage measurement.
* Simpler to add hardware versions to build system.
* More DAS hardware support.
* DRV8323s support.
* Initial UAVCAN support.
* Moved from uart to serial driver to avoid DMA conflicts.
* Support for permanent UART.

---

### 3.44
#### No official release
* NRF_EXT commands support.
	* Use NRF51822 with ESB remotes.
* Different radio channel for NRF pairing.

---

### 3.43
#### No official release
* Added battery ah to setup info.
* Changed tacho values in COMM_GET_VALUES_SETUP to meters.
* Added battery wh COMM_GET_VALUES_SETUP.
* Better remaining battery capacity calculation.

---

### 3.42
#### No official release
* Added setup info parameters:
	* Motor Poles
	* Gear Ratio
	* Wheel Diameter
	* Battery Type
	* Battery Cells
* Added more CAN status messages.
* Updated speed PID to start properly when braking is disabled.
* Added COMM_GET_VALUES_SETUP.
* Added COMM_SET_MCCONF_TEMP.
* Added COMM_SET_MCCONF_TEMP_SETUP.
* Added COMM_GET_VALUES_SELECTIVE.
* Added COMM_GET_VALUES_SETUP_SELECTIVE.

---

### 3.41
#### No official release
* First general purpose DC output implementation.

---

### 3.40
#### Released 2018-07-23
* Added motor controller ID to COMM_GET_VALUES.

---

### 3.39
#### Released 2018-07-06
* Updated HW75_300.
* Added AUX output support.

---

### 3.38
#### Released 2018-04-22
* Fixed temperature limit bug when the acceleration and brake current limits are different in magnitude.

---

### 3.37
#### Released 2018-03-24
* Temperature compensation on KI in addition to the observer resistance.
* Configurable FOC current filter (useful for slow abs max current setting).

---

### 3.36
#### No official release
* Added handbrake current commands to the simple CAN interface.
* Added D-term filter to position and speed controllers.

---

### 3.35
#### Released 2018-02-17
* Added option to disable nRF transmission (option in Transmit Power parameter).
* Fixed servo output driver for all hardwares and removed software servo driver.

---

### 3.34
#### Released 2018-01-24
* Added motor PID position to COMM_GET_VALUES.
* Inverted direction angle normalization in mc_interface.
* Use relative current mode in APP_ADC to support multiple VESCs with different current limits.

---

### 3.33
#### Released 2017-11-08
* Fixed CAN-bus baud rate update.

---

### 3.32
#### Released 2017-11-08
* Added CAN-bus baud rate setting.

---

### 3.31
#### Released 2017-10-27
* Option to decrease temperature limits during acceleration to still have braking torque left.
* Added PID speed control mode to ADC app.

---

### 3.30
#### Released 2017-10-20
* Activated iterative observer for better operation at high ERPM.
* Check for NAN and truncate some FOC variables.
* Speed controller windup protection improvement.

---

### 3.29
#### Released 2017-09-21
* Disabled throttle limit scaling for now.
* Increased packet timeout.

---

### 3.28
#### Released 2017-09-06
* DC_CAL timeout.
* Added board configuration file to avoid braking at boot.
* Shorter default fault stop time.
* Lower default PPM ramping time.
* Configurable beta value for motor thermistor.
* Individual throttle curves for acceleration and braking.

---

### 3.27
#### Released 2017-09-04
* Watt hour reset bug fix
* Changed the way custom applications are implemented.
* FOC: high current sampling mode.

---

### 3.26
#### No official release
* Current limit bug fix. It is now possible to apply break past the RPM limits.
* Openloop RPM calculation bug fix.

---

### 3.25
#### No official release
* APP multi-VESC PID control: send current instead of duty cycle for better load sharing.
* Added relative current commands to mc_interface and comm_can.
* APP ADC: added mode ADC_CTRL_TYPE_CURRENT_REV_BUTTON_BRAKE_ADC.
* APP ADC: changed behavior when throttle and brake ADC channels are used simultaneously for smoother combination.
* APP ADC: ramping support.
* Flux linkage measurement: Added extra try with high integrator value.

---

### 3.24
#### No official release
* Changed back inductance calculation since that seems to work much better in practise. (TODO: Have a closer look at why)

---

### 3.23
#### No official release
* Improved inductance measurement (bug fix).
* Multiple tries with different settings on flux linkage measurement.
* Observer improvements for high speed operation and better performance across the whole speed range.
* Compile time option to disable override limits.

---

### 3.22
#### No official release
* Added hardware-specific limits to configuration parameters.
* Permanent NRF bug fix.

---

### 3.21
#### No official release
* Fixed regression in PID speed controller.

---

### 3.20
#### No official release
* PID speed control: Set prev_error to error when the PID is off to make the start smoother.
* Improved spinup algorithm for flux linkage and bldc parameter measurement.
* APP ADC: Configurable center voltage for channel 1.
* APP_UARTCOMM: Keep the processing thread running when stopping the app in case the configuration is made from the UART port itself.
* Commands: Return results of long running commands to the port they came from even if commands come in between and change the last port.

---

### 3.19
#### No official release
* Added terminal plugin hook implementation. Inspired by https://github.com/vedderb/bldc/pull/28
* Moved sampling buffers to CCM to free some RAM.
* Added hardware info terminal command.
* NRF init SPI check fix.
* Sampled data is now transmitted in floating point with scaling done at the VESC. This avoids hard-coded scaling in VESC Tool.

---

### 3.18
#### No official release
* NRF init SPI check.
* Permanent NRF: reconfigure NRF pins to SPI pins on init failure in case the permanent NRF is not mounted and behave as if there is no permanent NRF.

---

### 3.17
#### No official release
* Temperature filtering.
* FOC: temperature resistance compensation.

---

### 3.16
#### No official release
* FOC: stator saturation compensation parameter.
* FOC: Another update for the fix for throttle limits to prevent loosing range at high speed when the battery current limit is lower than the motor current limit.
* DRV8301: over current protection settings added to configuration.
* DRV8301: Reset command andded to terminal.
* DRV8301: Log fault code read over SPI.
* DRV: Check for faults in ADC interrupt to catch the state as fast as possible.
* Send phase samples as well with the sampling function.
* New sampling modes for debugging.
* BLDC and DC: Configurable switching frequency
* FOC detect: increase minimum switching frequency for motor spinup to make it possible to detect high kv motors at high voltage.
* FOC: observer gain scaling parameter for low modulation.

---

### 3.15
#### No official release
* FOC: added the option for FOC sampling in both V0 and V7 to mcconf, so that it can be changed without recompiling the firmware.
* FOC: tweaked repetition counter and preload to get cleaner waveforms with low latency.
* FOC: Input voltage filterting and vd/vq filtering while undriven for more stable performance.

---

### 3.14
#### No official release
* Different throttle curve modes
* Improved FOC sensorless startup.

---

### 3.13
#### No official release
* Throttle curve for PPM, ADC and Nunchuk.
* Updated fix for throttle limits to prevent loosing range at high speed when the battery current limit is lower than the motor current limit.
* APP PPM ramping.
* APP ADC and PPM current range bug fix for some control modes.

---

### 3.12
#### No official release
* APP PPM throttle center setting.

---

### 3.11
#### No official release
* BLDC detect: disable direction inversion before detecting parameters.
* FOC speed control: remove supply voltage scaling since that does not make any sense in current control mode.
* BLDC speed control: added current-based speed controller option.
* BLDC: heavier RPM filtering.
* Speed control: option to disable braking in speed control mode.
* Added wattage limits. Useful for following laws for electric vehicles in some regions.
* Use override current limits to scale throttle inputs in apps. Will prevent the throttle from loosing rage at speed if e.g. the battery current limits are lower than the motor current limits.

---

### 3.10
#### Released 2016-11-06
* BLDC: removed cycles_running variable.
* BLDC: update ADC sampling in correct order to avoid corrupt samples when the switching frequency changes a lot at once.
* Terminal: print fault duty cycle state with one extra decimal.

---

### 3.09
#### Released 2016-11-06
* Configuration option for inverting the motor direction.
* STM32 96-bit unique ID readout.

---

### 3.08
#### No official release
* Communication protocol update for floating point variables. This breaks almost all compatibility with old firmwares.

---

### 3.07
#### Released 2016-11-04
* Delay after app and motor conf write.
	* Fixes NRF bug.
	* Fixes glitches if throttle is given while updating the configurations.
* Lock mc_interface while storing configuration.
* Nunchuk app local timeout.
	* Prevents the output thread from blocking other outputs after being used before.
* Lock MC interface while storing configurations to flash.

---

### 3.06
#### No official release
* spi_sw for NRF stop bug fix.

---

### 3.05
#### No official release
* App NRF pairing.
* App nunchuk chuk error restore bug fix.

---

### 3.04
#### No official release
* HW version built into firmware.
	* Allows VESC Tool to only list firmwares compatible with the hardware.

---

### 3.02
#### No official release
* hw_60 support.
* hw_das support.
* DRV8301 support.
	* SPI implementation.
	* Some terminal commands.
* DRV8313 support.
* 3 shunt support.
* Phase shunt support.
* Global RPM limit for both BLDC and FOC.
* Hall sensor software filtering.
* SPI software filtering.
* The software filters remove the need for hardware filtering on the sensor port, making it work for all different sensors without modification.
* Handbrake function for FOC (open loop braking).
* FOC updates and fixes.
	* Current control signs.
	* Control loop integrator fixes.
	* Phase delay compensation and minimization.
	* More consistent flux linkage detection.
	* Resistance and inductance measurement bug fix that could cause a reboot.
	* Timer sampling improvement and cleanup.
	* Support for sampling in V0 and V7 when using phase shunts.
* Fix reboot on over temperature fault code.
* Motor temperature measurement and soft backoff.
* Terminal command for rotating magnet field generation (ACIM experimentation).
* Prevent motor start command during initialization.
* Hardware specific default configuration support.
* Stop functionality for apps so that reboots are not required anymore when changing app.
* EEPROM emulation bug fix: https://github.com/vedderb/bldc/issues/27

---

### 3.00
#### Released 2016-06-27
* HW60 support
* 3 low/high side shunt support
* permanent NRF option

