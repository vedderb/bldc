# LispBM

This is the VESC-integration of [lispBM](https://github.com/svenssonjoel/lispBM) written by Joel Svensson. It allows the VESC to run lisp-programs in a sandboxed environment.

### Feature Overview

* Development and testing in VESC Tool with live variable monitoring and plotting as well as CPU and memory monitoring.
* There is a REPL in VESC Tool where code can be executed and tested live. You even have full access to the functions and bindings in the program you have uploaded.
* Sandboxed environment, meaning that the Lisp code (hopefully) cannot freeze or crash the rest of the VESC code when it gets stuck or runs out of heap or stack memory.
* The application runs on the VESC itself without the need for having VESC Tool connected and is stored in flash memory.
* When a lisp-application is written to the VESC it is automatically started on each boot.

## Language Reference

[LispBM Language Reference](lispBM/doc/lbmref.md)

## Programming Manual

This is the work-in-progress programming manual for LispBM. Note that the examples in the manual use the REPL quite a lot. All of them also work in the VESC Tool REPL (which is below the console below the code editor) when you are connected to a VESC and will be executed on the VESC itself. The results of the commands will be printed in the console. From the VESC Tool REPL you also have access to all functions and variables in the program that you have uploaded.

[Chapter 1: Introduction to programming in LispBM](lispBM/doc/manual/ch1_introduction.md)  
[Chapter 2: List Processing](lispBM/doc/manual/ch2_list_processing.md)  
[Chapter 3: Concurrency](lispBM/doc/manual/ch3_concurrency.md)

## VESC Express Libraries

The VESC Express has some extra libraries that are documented in separate documents.

**VESC Express Display Driver**  

[VESC Express Display Driver](https://github.com/vedderb/vesc_express/blob/main/main/display/README.md)

The display driver allows driving many common displays using SPI, such as the ST7789, ST7735, ILI9341, ILI9488, SH8501, SSD1306 and SSD1351. There are accelerated rendering and font extensions that can be used from LispBM.  

**VESC Express Wifi and TCP**  

[VESC Express Wifi and TCP](https://github.com/vedderb/vesc_express/blob/main/main/wifi/README.md)

The WiFi and TCP library allows scanning for and connecting to networks as well as creating and managing TCP connections.  

**VESC Express BLE**  

[VESC Express BLE](https://github.com/vedderb/vesc_express/blob/main/main/ble/README.md)

This library allows creating a custom BLE-server with custom characteristics.  

## VESC-Specific Commands and Extensions

The VESC-specific extensions are documented below. If you are reading this on GitHub there is an index in the upper right corner that can be used to navigate this document. It follows you as you scroll around and also includes a search function that filters all the titles in this document.

Note that VESC Tool includes a collection of examples that can be used as a starting point for using LispBM on the VESC.

---

### Various Commands

---

#### print

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(print arg1 ... argN)
```

Print to the VESC Tool Lisp console. Example:

```clj
(print "Hello World")
> "Hello World"
```

Should work for all types. If multiple arguments are provided, each one will be
printed on a separate line.

---

#### puts

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(puts str)
```

Similar to `print`, but only takes one string as an argument and prints it without
quotes. The string extensions can be used to format the output.

**Note:**  
This extension can print longer strings than `print`. `print` will trim any
output over 256 bytes, while this extension only trims strings over 400 bytes.

Example:

```clj
(puts "Hello World")
> Hello World
```

---

#### set-print-prefix

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.06+ |

```clj
(set-print-prefix str)
```

Adds a prefix to prints that are sent to the repl in VESC Tool. Useful when
multiple devices print to the repl over CAN at the same time for determining
which print comes from which device. The prefix can't be longer than 50 bytes
(including the null byte), and `str` will be trimmed if longer. The value is
remembered until the next power cycle.

**Note:**  
This is applied to all text sent to the repl, not just text created by
calling `print` or `puts`.


Example:

```clj
(set-print-prefix "dev-1| ")
(puts "This message has\na clear origin!")
```
which outputs
```
dev-1| This message has
dev-1| a clear origin!
```

---

#### timeout-reset

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(timeout-reset)
```

Reset the timeout that stops the motor. This has to be run on at least every second to keep the motor running. The timeout time can be configured in App Settings->General. The [Motor Set Commands](#motor-set-commands) will also reset the timeout when they are called.

---

#### get-ppm

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-ppm)
```

Read the decoded value on the PPM input, range -1.0 to 1.0. If the PPM-decoder is not running it will be initialized and the PPM-pin will be reconfigured, so make sure that nothing else is using that pin. Example:

```clj
(print (str-from-n (get-ppm) "PPM Value: %.2f"))
```

Note that control type can be set to Off in the PPM app to get the input without running the motor automatically, which is useful when running the motor from lisp.

---

#### get-ppm-age

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-ppm-age)
```

Get the age of the last PPM update in seconds. Can be used to determine if there is any valid PPM-signal.

---

#### set-servo

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-servo value)
```

Set servo output to value. Range 0 to 1. Note that the servo output has to be enabled in App Settings -> General.

---

#### get-vin

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-vin)
```

Get input voltage.

---

#### select-motor

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(select-motor motor)
```

Select which motor to control on dual-motor hardware. Options are 1 for motor 1 and 2 for motor 2.

---

#### get-selected-motor

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-selected-motor)
```

Get currently selected motor on dual motor hardware.

---

#### get-bms-val

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(get-bms-val val optValArg)
```

Get value from BMS. Examples:

```clj
(get-bms-val 'bms-v-tot) ; Total voltage
(get-bms-val 'bms-v-charge) ; Charge input voltage
(get-bms-val 'bms-i-in-ic) ; Measured current (negative means charging)
(get-bms-val 'bms-ah-cnt) ; Amp hour counter
(get-bms-val 'bms-wh-cnt) ; Watt hour counter
(get-bms-val 'bms-cell-num) ; Number of cells in series
(get-bms-val 'bms-v-cell 2) ; Cell 3 voltage (index starts from 0)
(get-bms-val 'bms-bal-state 2) ; Cell 3 balancing state. 0: not balancing, 1: balancing
(get-bms-val 'bms-temp-adc-num) ; Temperature sensor count
(get-bms-val 'bms-temps-adc 2) ; Get sensor 3 temperature (index starts from 0)
(get-bms-val 'bms-temp-ic) ; Balance IC temperature
(get-bms-val 'bms-temp-hum) ; Humidity sensor temperature
(get-bms-val 'bms-hum) ; Humidity
(get-bms-val 'bms-pres) ; Pressure in PA (Added in 6.05)
(get-bms-val 'bms-temp-cell-max) ; Maximum cell temperature
(get-bms-val 'bms-soc) ; State of charge (0.0 to 1.0)
(get-bms-val 'bms-can-id) ; CAN ID of BMS
(get-bms-val 'bms-ah-cnt-chg-total) ; Total ah charged
(get-bms-val 'bms-wh-cnt-chg-total) ; Total wh charged
(get-bms-val 'bms-ah-cnt-dis-total) ; Total ah discharged
(get-bms-val 'bms-wh-cnt-dis-total) ; Total wh discharged
(get-bms-val 'bms-msg-age) ; Age of last message from BMS in seconds
(get-bms-val 'bms-chg-allowed) ; Charging allowed (Added in 6.05, Express only)
```

---

#### set-bms-val

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(set-bms-val val optValArg new-value)
```

This lets you set BMS-values so that VESC Tool and the BMS limits can see and use them. The same values as described in [get-bms-val](#get-bms-val) can be updated, but with an argument at the end with what the value should be set to.

This is useful if you want to implement communication with a custom BMS and have it show up in VESC Tool.

Example:

```clj
(set-bms-val 'bms-cell-num 12) ; It is a 12s pack
(set-bms-val 'bms-v-cell 2 3.92) ; Set cell 2 voltage to 3.92V
```

---

#### send-bms-can

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(send-bms-can)
```

Send BMS-values on CAN-bus. This his useful if a custom BMS-driver is implemented using [set-bms-val](#set-bms-val) in order to make devices on the CAN-bus aware of the BMS-state using the VESC protocol.

---

#### set-bms-chg-allowed

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(set-bms-chg-allowed allow)
```

Enable or disable charging. 1 means enable and 0 means disable.

---

#### bms-force-balance

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(bms-force-balance balance)
```

Start or stop balancing. 1 means start and 0 means stop.

---

#### bms-zero-offset

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(bms-zero-offset)
```

Zero current measurement offset on BMS. Has to be done while no current (or charge-current on charge-only BMS) is flowing. Will be sent to every BMS on the CAN-bus.

---

#### get-adc

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(get-adc ch)
```

Get ADC voltage on channel ch (0, 1, 2 or 3). The channels are the following:

**Channel 0:**  
ADC1 on the COMM-port

**Channel 1:**  
ADC2 on the COMM-port

**Channel 2:**  
ADC3 on the COMM-port. Note that some hardware does not have this channel - then the voltage of ADC1 is returned instead.

**Channel 3:**  
This is the ADC-channel that the motor temperature sensor goes to. Note: if you want to use this channel for something else you have to disable the motor temperature sensor in General -> Advanced. Otherwise the input might generate overtemperature faults.

**Channel 4+:**  
Some hardware has additional ADC-channels which also can be read with this function. If they are missing the voltage on ADC1 is returned instead.

---

#### override-temp-motor

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(override-temp-motor temp)
```

Override motor temperature. This can be used to implement custom motor temperature sensors if the sensor you have is not supported. Note: Motor Temperature Sensor Type has to be set to Disabled in General -> Advanced for the override to work.

---

#### get-adc-decoded

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-adc-decoded ch)
```

Get decoded ADC value on channel ch (0 or 1). Decoded means that the voltage is mapped to the range 0 to 1 according to the configuration in the ADC app. Note that the ADC app must be running for this function to work. No throttle curve is applied to this value, but you can use the [throttle-curve](#throttle-curve) function to apply one if desired.

---

#### systime

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(systime)
```

Get system time in ticks since boot.

---

#### secs-since

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(secs-since timestamp)
```

Get seconds elapsed since systime timestamp.

---

#### set-aux

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-aux ch state)
```

Set AUX output ch (1 or 2) to state. Example:

```clj
(set-aux 1 1) ; Set AUX1 to ON.
```

Note: The AUX output mode must be set to Unused in Motor Settings->General->Advanced. Otherwise the firmware will change the AUX state directly after it is set using this function.

---

#### get-imu-rpy

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(get-imu-rpy)
```
Get roll, pitch and yaw from the IMU in radians.

The function (ix list ind) can be used to get an element from the list. Example:
```clj
(ix (get-imu-rpy) 0) ; Get roll (index 0)
```

---

#### get-imu-quat

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(get-imu-quat)
```
Get a list of quaternions from the IMU (q0, q1, q2 and q3).

---

#### get-imu-acc

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(get-imu-acc)
```
Get a list of the x, y and z acceleration from the IMU in G.

---

#### get-imu-gyro

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(get-imu-gyro)
```
Get a list of the x, y and z angular rate from the IMU in degrees/s.

---

#### get-imu-mag

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(get-imu-mag)
```
Get a list of the x, y and z magnetic field strength from the IMU in uT. Note that most IMUs do not have a magnetometer.

---

#### get-imu-acc-derot

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

Same as get-imu-acc, but derotates the result first. This means that the acceleration will be relative to the horizon and not the IMU chip.

---

#### get-imu-gyro-derot

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

Same as get-imu-gyro, but derotates the result first. This means that the angular rates will be relative to the horizon and not the IMU chip.

---

#### send-data

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(send-data dataList optInterface optCanId)
```
Send a list of custom app data on the commands-interface of the type COMM_CUSTOM_APP_DATA. This can be read from a Qml script in VESC Tool for example or from another node on the CAN-bus.

Example of sending the numbers 1, 2, 3 and 4:

```clj
(send-data (list 1 2 3 4))
```

*dataList* can be a list or a [byte array](#byte-arrays).

The optional argument optInterface (introduced in FW 6.05) can be used to specify which interface to use and the optional argument optCanId can be used to specify which CAN-device to send the message to when using the CAN interface. The available interfaces are:

| Number | Interface Type |
|---|---|
| 0 | The last interface data was received on (default) |
| 1 | USB |
| 2 | CAN (requires the argument optCanId) |
| 3 | UART on comm-header |
| 4 | UART builtin (depending on hardware) |
| 5 | UART extra (depending on hardware) |
| 6 | WiFi local (express) |
| 7 | WiFi TCP Hub (express) |
| 8 | BLE (express) |

---

#### recv-data

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(recv-data optTimeout)
```

Block current thread until data from VESC Tool arrives. The optional argument optTimeout can be used to specify a timeout in seconds.

If a timeout occurs the symbol timeout will be returned, otherwise a byte array with the data will arrive.

Usage example:

```clj
(print (recv-data))
> [1 2 3]
```

---

#### sleep

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(sleep seconds)
```

Sleep for *seconds* seconds. Example:

```clj
(sleep 0.05) ; Sleep for 0.05 seconds (50 ms)
```

---

#### get-remote-state

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-remote-state)
```

Get button and joystick state of connected remote. Note that a remote app such as the VESC remote or nunchuk must be configured and running for this to work. Returns the following list:

```clj
(js-y js-x bt-c bt-z is-rev update-age)
; Where
; js-y : Joystick Y axis, range -1.0 to 1.0
; js-x : Joystick X axis, range -1.0 to 1.0
; bt-c : C button pressed state, 0 or 1
; bt-z : Z button pressed state, 0 or 1
; is-rev : Reverse active, 0 or 1
; update-age : Age of last update from the remote in seconds
```

---

#### sysinfo

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(sysinfo param)
```

Read system info parameter param. Example:

```clj
(sysinfo 'hw-name) ; Hardware name, e.g 60
(sysinfo 'fw-ver) ; Firmware version as list (Major Minor BetaNum)
(sysinfo 'has-phase-filters) ; t if hardware has phase filters. ESC only.
(sysinfo 'uuid) ; STM32 UUID. ESC only.
(sysinfo 'runtime) ; Total runtime in seconds. ESC only.
(sysinfo 'odometer) ; Total odometer in meters. ESC only. Added in 6.06.
(sysinfo 'git-branch) ; Git branch name.
(sysinfo 'git-hash) ; Git hash of current commit.
(sysinfo 'compiler) ; GCC version, e.g. 7.3.1. ESC only.
(sysinfo 'hw-type) ; Hardware type, e.g. hw-express. Added in 6.02.
(sysinfo 'part-running) ; Running partition name. Express only.
```

---

#### stats

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(stats param)
```

Get statistics about the selected motor since boot (or since stats-reset). The following example shows which stats are available:

```clj
(stats 'stat-speed-avg) ; Average speed in m/s
(stats 'stat-speed-max) ; Maximum speed in m/s
(stats 'stat-power-avg) ; Average power in W
(stats 'stat-power-max) ; Maximum power in W
(stats 'stat-current-avg) ; Average current in A
(stats 'stat-current-max) ; Maximum current in A
(stats 'stat-temp-mosfet-avg) ; Average MOSFET temp in degC
(stats 'stat-temp-mosfet-max) ; Maximum MOSFET temp in degC
(stats 'stat-temp-motor-avg) ; Average motor temp in degC
(stats 'stat-temp-motor-max) ; Maximum motor temp in degC
(stats 'stat-count-time) ; Time since start of stat collection in seconds
```

---

#### set-odometer

| Platforms | Firmware |
|---|---|
| ESC | 6.06+ |

```clj
(set-odometer meters)
```

Set persistent odometer counter to meters.

---

#### stats-reset

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(stats-reset)
```

Reset stat counters to 0.

---

#### crc16

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.02+ |

```clj
(crc16 array optLen)
```

Calculate the 16-bit crc of array. optLen is an optional argument for how many elements to include, if it is left out the entire array will be used. The crc uses the polynomial 0x11021 and initial value 0, which is the same as the VESC packets use. Added in FW 6.02.

---

#### crc32

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(crc32 array init optLen)
```

Calculate the 32-bit crc of array. optLen is an optional argument for how many elements to include, if it is left out the entire array will be used. The crc uses the polynomial 0x4C11DB7 (ethernet) and initial value init.

---

#### main-init-done

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(main-init-done)
```

Returns true when the main-function is done with all initialization.

---

#### shutdown-hold

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(shutdown-hold hold)
```

Hold shutdown. When hold is true hardware shutdown will be delayed until hold is set to false again. Can be used when catching a shutdown-event if more time is needed for cleanup.

---

#### const-heap-erase

| Platforms | Firmware |
|---|---|
| ESC | 6.06+ |

```clj
(const-heap-erase)
```

Erase constant heap. This can be used in the beginning of the application to erase the constant memory of the application. That is useful for applications that do not always write the same things to constant memory in the same order after starting. Situations where that can occur is when the application takes different execution paths depending on external events or when writing variables to constant memory that differ based on external events.

Running this command in the beginning of the application should prevent any write-to-flash errors, but the command takes a few seconds to execute and puts some wear on the flash memory. The reader will become slightly slower in const blocks after running this command compared to an application where constant memory already has been written in previous runs.

---

#### reboot

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(reboot)
```

Reboot CPU. Required on the express when e.g. changing wifi-settings.

---

### App Override Commands

Several app-inputs can be detached from the external interfaces and overridden from lisp. This is useful to take advantage of existing throttle curves and control modes from the apps while providing a custom input source.

**Note:** Detach does *not* mean that the app output is disabled, it means that you can provide the input for the app instead of having it read the external peripheral. So if you e.g. detach the app and override the input with 0 the app will keep sending the corresponding command to the motor as usual, even if that command is a stop command. If you want to disable the app output you can have a look at [app-disable-output](#app-disable-output).

---

#### app-adc-detach

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(app-adc-detach mode state)
; Where
; mode : Select periperial to detach from APP
;        - 0 All peripherial attached (no second argument)
;        - 1 ADC1/2
;        - 2 Buttons
;        - 3 ADC1/2 + Buttons
; state : Only used when mode is not 0. State 1, 2 or 3 detaches the peripheral and
;         state 0 attaches peripheral. For the ADC, 1 means detach both ADCs, 2 means
;         detach ADC1 only and 3 means detach ADC2 only. For the buttons state
;         1, 2 and 3 mean the same thing, namely detach.
```

Detaches a peripherial from the APP ADC

**Note:** Since firmware 6.05 the ADC-app will no longer reset the timeout by itself when detached. That means that the override-commands have to be sent at a rate higher than the timeout for the output to stay enabled. This is a safety feature that prevents the motor from running after the timeout time if e.g. your script crashes.

---

#### app-adc-override

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(app-adc-override mode value)
; Where
; mode : Select peripheral to override
;        - 0 ADC1
;        - 1 ADC2
;        - 2 Reverse button
;        - 3 Cruise control button
; value : 0.0 to 3.3 (button pressed is > 0.0)
; Note that throttle mapping also is applied to the override value.
```

Sets the override value

---

#### app-adc-range-ok

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(app-adc-range-ok)
```

Returns true when the throttle voltage is within range and false (nil) otherwise.

---

#### app-ppm-detach

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(app-ppm-detach state)
```

Detaches the decoded ppm signal from APP PPM. 1 means detach, 0 means attach.

---

#### app-ppm-override

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(app-ppm-override value)
```

Sets the override value. Range -1.0 to 1.0.

---

#### set-remote-state

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-remote-state js-y js-x bt-c bt-z is-rev)
; Where
; js-y : Joystick Y axis, range -1.0 to 1.0, used for throttle position
; js-x : Joystick X axis, range -1.0 to 1.0, unused by the app
; bt-c : C button pressed state, 0 or 1, used for cruise control
; bt-z : Z button pressed state, 0 or 1, unused by the app
; is-rev : Reverse active, 0 or 1, reverse mode active
```

Send input to the VESC Remote app. Unlike the ADC and PPM apps, input can be sent to this app at any time without detaching it and it will be treated the same as a packet from a VESC Remote. That means the timeout as well as all VESC Remote settings will be used.

---

#### app-disable-output

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(app-disable-output ms)
```

Disable app output for ms milliseconds. 0 means enable now and -1 means disable forever. This can be used to override the control of apps temporarily.

---

#### app-is-output-disabled

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(app-is-output-disabled)
```

Check if app output is disabled. VESC Tool will disable app output during some detection routines, so when running a custom control script it might be useful to check this and disable the output when this value is true.

---

#### app-pas-get-rpm

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(app-pas-get-rpm)
```

Returns the pedal RPM measured by the PAS-app. If you want to implement your own PAS-control based on this RPM you can use [app-disable-output](#app-disable-output) to disable the output of the PAS-app.

---

### Motor Set Commands

---

#### set-current

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-current current optOffDelay)
```

Set motor current in amperes.

The optional optOffDelay argument (in seconds) will delay turning off the modulation when setting 0 current. This is useful when running e.g. a control loop that will end up setting 0 current in some circumstances when turning off the modulation would make the control less smooth. The delay value should be longer than the rate at which the control loop runs.

---

#### set-current-rel

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-current-rel current optOffDelay)
```

Set motor current relative to the maximum current. Range -1 to 1. For example, if the maximum current is set to 50A, (set-current-rel 0.5) will set the current to 25A.

See [set-current](#set-current) for details on what the optional argument optOffDelay does.

---

#### set-duty

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-duty dutycycle)
```

Set duty cycle. Range -1.0 to 1.0.

---

#### set-brake

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-brake current)
```

Set braking current.

---

#### set-brake-rel

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-brake-rel current)
```

Set braking current relative to the maximum current, range 0.0 to 1.0.

---

#### set-handbrake

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-handbrake current)
```

Set handbrake current. This sets an open loop current that allows to hold the motor still even at 0 speed at the cost of efficient.

---

#### set-handbrake-rel

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-handbrake-rel current)
```

Same as set-handbrake, but with a current relative to the maximum current in the range 0.0 to 1.0.

---

#### set-rpm

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-rpm rpm)
```

Set RPM speed control.

---

#### set-pos

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(set-pos pos)
```

Position control. Set motor position in degrees, range 0.0 to 360.0.

---

#### foc-openloop

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(foc-openloop current rpm)
```

Run FOC in open loop. Useful to test thermal properties of motors and power stages.

---

#### foc-beep

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(foc-beep freq time voltage)
```

Use the motor to play a beep sound at frequency freq for time seconds using voltage excitation voltage. The frequency can be set between 100 Hz and 7500 Hz.

---

#### foc-play-tone

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(foc-play-tone channel freq voltage)
```

Use the motor to play a tone at frequency freq with modulation voltage. Channel can be 0, 1, 2 or 3 and all channels can play tones simultaneously (polyphonic audio). Unlike foc-beep, foc-play-tone also works while the motor is running.

---

#### foc-play-samples

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(foc-play-samples samples freq voltage)
```

Use the motor to play sampled audio. Works while the motor is running. Samples is a byte-array with samples, where each sample has the range -128 to 127. Freq is the sampling frequency and voltage is the voltage amplitude the samples will be played at.

The caller is responsible for making sure that the sample buffer stays valid until it is consumed. Internally this function has two buffers and when both buffers are full the function will block until a buffer is free. For smooth playback, it is important to keep feeding this function with buffers faster than it consumes the samples.

---

#### foc-play-stop

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(foc-play-stop)
```

Stop playing tones on all channels.

---

### Motor Get Commands

**Note:**  
If the optional optFilter-argument is 1 in the commands below the result will be the average since that function was called the last time. That is also how the plots are filtered in VESC Tool. Polling realtime data in VESC Tool at the same time will affect the averaging as it uses the same integrator. This function was added in firmware 6.05.

---

#### get-current

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-current optFilter)
```

Get motor current. Positive means that current is flowing into the motor and negative means that current is flowing out of the motor (regenerative braking).

---

#### get-current-dir

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-current-dir)
```

Get directional current. Positive for torque in the forward direction and negative for torque in the reverse direction.

---

#### get-current-in

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-current-in optFilter)
```

---

#### get-id

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-id optFilter)
```

Get FOC d-axis current.

---

#### get-iq

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-iq optFilter)
```

Get FOC q-axis current.

---

#### get-id-set

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(get-id-set)
```

Get the set FOC d-axis current. This is the raw requested current.

---

#### get-iq-set

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(get-iq-set)
```

Get the set FOC q-axis current. This is the raw requested current.

---

#### get-vd

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-vd optFilter)
```

Get FOC d-axis voltage.

---

#### get-vq

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-vq optFilter)
```

Get FOC q-axis voltage.

---

#### get-est-lambda

| Platforms | Firmware |
|---|---|
| ESC | 6.02+ |

```clj
(get-est-lambda)
```

Get FOC estimated flux linkage in Weber. Requires that one of the observers with flux linkage tracking is used. Added in FW 6.02.

---

#### get-est-res

| Platforms | Firmware |
|---|---|
| ESC | 6.02+ |

```clj
(get-est-res)
```

Get FOC estimated motor resistance in Ohm. This value is only accurate when the RPM is low and current is high. Added in FW 6.02.

---

#### get-est-ind

| Platforms | Firmware |
|---|---|
| ESC | 6.02+ |

```clj
(get-est-ind)
```

Get FOC estimated motor inductance Henry. Only works while the first HFI is running (not 45 Deg and not Coupled HFI). Added in FW 6.02.

---

#### get-hfi-res

| Platforms | Firmware |
|---|---|
| ESC | 6.06+ |

```clj
(get-hfi-res)
```

Get HFI result. Only valid when using single or double pulse ambiguity resolution mode. Returns a list with the following valies:

```clj
(i1 i2 diff)
```

Where i1 is the delta current for the first voltage, i2 is the response current for the second voltage and diff is the difference between them. This can be used to determine how much ambiguity resolution current is required for a given motor.

---

#### get-duty

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-duty)
```

Get duty cycle. Range -1.0 to 1.0.

---

#### get-rpm

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-rpm)
```

Get motor ERPM. Negative values mean that the motor spins in the reverse direction.

---

#### get-rpm-fast

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(get-rpm-fast)
```

Same as get-rpm, but with less filtering. This give the RPM-estimation lower latency at the cost of more noise.

---

#### get-rpm-faster

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(get-rpm-faster)
```

Same as get-rpm-fast, but with even less filtering. This give the RPM-estimation even lower latency at the cost of even more noise.

---

#### get-rpm-set

| Platforms | Firmware |
|---|---|
| ESC | 6.06+ |

```clj
(get-rpm-set)
```

Get motor ERPM setpoint for PID speed controller.

---

#### get-pos

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-pos)
```

Get motor position. Returns the motor PID pos (taking into account the Position Angle Division)

---

#### get-temp-fet

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-temp-fet optFet)
```

Get MOSFET temperature. The argument optFet can be used to select senor 1 to 3. If it is left out or 0 the highest temperature is returned. If the hardware only has one sensor 0 is returned for sensors 1 to 3.

---

#### get-temp-mot

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-temp-mot)
```

Get motor temperature.

---

#### get-speed

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-speed)
```

Get speed in meters per second. Requires that the number of motor poles, wheel diameter and gear ratio are set up correctly.

---

#### get-speed-set

| Platforms | Firmware |
|---|---|
| ESC | 6.06+ |

```clj
(get-speed-set)
```

Get PID speed setpoint in meters per second. Requires that the number of motor poles, wheel diameter and gear ratio are set up correctly.

---

#### get-dist

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-dist)
```

Get the distance traveled since start in meters. As with (get-speed) this requires that the number of motor poles, wheel diameter and gear ratio are set up correctly. When the motor spins forwards this counter counts up and when it spins backwards it counts down.

---

#### get-dist-abs

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-dist-abs)
```

Same as get-dist, but will count up when the motors spins in both directions.

---

#### get-batt

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-batt)
```

Get the battery level, range 0.0 to 1.0. Requires that the battery type and number of cells is set up correctly.

---

#### get-fault

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-fault)
```

Get fault code.

---

#### get-ah

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-ah)
```

Get the number of amp hours consumed since start.

---

#### get-wh

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-wh)
```

Get the number of watt hours consumed since start.

---

#### get-ah-chg

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-ah-chg)
```

Get the number of amp hours charged since start.

---

#### get-wh-chg

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-wh-chg)
```

Get the number of watt hours charged since start.

---

### Positions

There are several position sources and many ways to interpret them. The following extensions can be used to get most interpretations of most position sources.

---

#### get-encoder

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-encoder)
```

Get angle from selected encoder in degrees.

---

#### set-encoder

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(set-encoder degrees)
```

Set the encoder position in degrees. This command only has an effect in the ABI and custom encoder modes. In ABI mode the encoder position is updated and the index is set to found. In custom encoder mode the encoder position is updated (unless a native library provides custom encoder support).

When using an ABI-encoder this is useful if a position can be derived before the index pulse is found.

---

#### get-encoder-error-rate

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(get-encoder-error-rate)
```

Returns the error rate for the selected encoder, range 0.0 to 1.0. If the selected encoder does not provide any error rate -1.0 is returned. If the selected encoder has multiple error rates the highest one is returned.

---

#### pos-pid-now

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(pos-pid-now)
```

Returns the current position of the PID-controller, including compensation for the angle division and offset. Unit: Degrees.

---

#### pos-pid-set

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(pos-pid-set)
```

Returns the set position of the PID-controller, including compensation for the angle division and offset. Unit: Degrees.

---

#### pos-pid-error

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(pos-pid-error)
```

Returns the difference between the current and the set PID position. Unit: Degrees.

---

#### phase-motor

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(phase-motor)
```

Returns the electrical position of the motor that is used for FOC now. Unit: Degrees.

---

#### phase-encoder

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(phase-encoder)
```

Returns the encoder position mapped to the electrical position of the motor. Unit: Degrees.

---

#### phase-hall

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(phase-hall)
```

Returns the hall sensor position of the motor. Unit: Degrees.

---

#### phase-observer

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(phase-observer)
```

Returns the FOC observer position. Unit: Degrees.

---

#### observer-error

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(observer-error)
```

Returns the difference between the observer position and the encoder position mapped to the electrical position of the motor. Unit: Degrees.

---

### Setup Values

These commands return the accumulated values from all VESC-based motor controllers on the CAN-bus. Note that the corresponding CAN status messages must be activated for these commands to work.

---

#### setup-ah

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(setup-ah)
```

Get the number of amp hours consumed since start.

---

#### setup-ah-chg

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(setup-ah-chg)
```

Get the number of amp hours charged since start.

---

#### setup-wh

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(setup-wh)
```

Get the number of watt hours consumed since start.

---

#### setup-wh-chg

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(setup-wh-chg)
```

Get the number of watt hours charged since start.

---

#### setup-current

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(setup-current)
```

Get total motor current. Positive means that current is flowing into the motor and negative means that current is flowing out of the motor (regenerative braking).

---

#### setup-current-in

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(setup-current-in)
```

---

#### setup-num-vescs

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(setup-num-vescs)
```

Get the number of VESC-based motor controllers the setup values are accumulated from.

---

### CAN-Commands

Notice that all canget-commands rely on the status messages being active on the VESCs on the CAN-bus. That can be done from App Settings->General->Can status message mode.

---

#### can-msg-age

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(can-msg-age id msg)
```

Get age of can-message msg from device with can-id id. Can be used to determine how up-to-date the values are from the different messages. nil is returned if the given message has never been received from the given id.

---

#### canset-current

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canset-current id current optOffDelay)
```

Set current over CAN-bus on VESC with id. Example for setting 25A on VESC with id 115:

```clj
(canset-current 115 25)
```

See [set-current](#set-current) for details on what the optional argument optOffDelay does.

---

#### canset-current-rel

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canset-current-rel id current optOffDelay)
```

Same as above, but relative current in the range -1.0 to 1.0. See (set-current) for details on what relative current means.

See [set-current](#set-current) for details on what the optional argument optOffDelay does.

---

#### canset-duty

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canset-duty id duty)
```

Set duty cycle over CAN-bus on VESC with id. Range -1.0 to 1.0.

---

#### canset-brake

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canset-brake id current)
```

Set braking current over CAN-bus.

---

#### canset-brake-rel

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canset-brake-rel id current)
```

Set relative braking current over CAN-bus. Range 0.0 to 1.0.

---

#### canset-rpm

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canset-rpm id rpm)
```

Set rpm over CAN-bus.

---

#### canset-pos

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canset-pos id pos)
```

Set position control in degrees over CAN-bus. Range 0.0 to 1.0.

---

#### canget-current

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-current id)
```

Get current over CAN-bus on VESC with id.

---

#### canget-current-dir

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-current-dir id)
```

Get directional current over CAN-bus on VESC with id. See (get-current-dir) for what directional means.

---

#### canget-current-in

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-current-in id)
```

Get input current over CAN-bus on VESC with id.

---

#### canget-duty

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-duty id)
```

Get duty cycle over CAN-bus on VESC with id.

---

#### canget-rpm

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-rpm id)
```

Get RPM over CAN-bus on VESC with id.

---

#### canget-temp-fet

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-temp-fet id)
```

Get MOSFET temperature over CAN-bus on VESC with id.

---

#### canget-temp-motor

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-temp-motor id)
```

Get motor temperature over CAN-bus on VESC with id.

---

#### canget-speed

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-speed id)
```

Get speed in meters per second over CAN-bus on VESC with id. The gearing, wheel diameter and number of motor poles from the local configuration will be used for converting the RPM to meters per second.

---

#### canget-dist

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-dist id)
```

Get distance traveled in meters over CAN-bus on VESC with id. As with (canget-speed id), the local configuration will be used to convert the tachometer value to meters.

---

#### canget-ppm

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-ppm id)
```

Get PPM-input from the VESC with id on the CAN-bus. Note that CAN status message 6 as well as the PPM-app must be active on that VESC for this function to work.

---

#### canget-adc

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(canget-adc id ch)
```

Get ADC channel ch from the VESC with id on the CAN-bus. Note that CAN status message 6 must be active on that VESC for this function to work.

---

#### canget-vin

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.02+ |

```clj
(canget-vin id)
```

Get input voltage on ESC with id on the CAN-bus. Note that CAN status message 5 must be active on that ESC for this function to work.

---

#### can-list-devs

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(can-list-devs)
```

List CAN-devices that have been heard on the CAN-bus since boot. This function is fast as it does not actively scan the CAN-bus, but it relies on the devices sending status message 1.

---

#### can-scan

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(can-scan)
```

Actively scan the CAN-bus and return a list with devices that responded. This function takes several seconds to run, but also finds devices that do not actively send messages and only respond to a ping message.

---

#### can-local-id

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(can-local-id)
```

Get local CAN ID.

---

#### can-send-sid

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(can-send-sid id data)
```

Send standard ID CAN-frame with id and data. Data is a list with bytes, and the length of the list (max 8) decides how many data bytes are sent. Example:

```clj
(can-send-sid 0x11FF11 (list 0xAA 0x11 0x15))
```

*data* can be a list or a [byte array](#byte-arrays).

---

#### can-send-eid

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(can-send-eid id data)
```

Same as (can-send-sid), but sends extended ID frame.

---

#### can-recv-sid

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(can-recv-sid optTimeout)
```

Block current thread until a standard-id can-frame arrives. The optional argument optTimeout can be used to specify a timeout in seconds.

If a timeout occurs the symbol timeout will be returned, otherwise a list with the following format will be returned:

```clj
(id data)
```

Usage example:

```clj
(print (can-recv-sid))
> (2i32 [1 2 3])
```

---

#### can-recv-eid

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(can-recv-eid optTimeout)
```

Same as (can-recv-sid), but waits for extended ID frame.

---

#### can-cmd

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(can-cmd id cmd)
```

Execute command cmd on CAN-device with ID id. The command cmd is sent as a string and will be parsed and evaluated by the receiver.

This is useful to execute arbitrary code on a CAN-device that is not covered by the other [CAN-Commands](#can-commands). This function has more overhead than other CAN-Commands, so if possible they should be used instead.

Example:

```clj
; Configuration update on ID54:
(can-cmd 54 "(conf-set max-speed 10.0)")

; The string-functions can be used for setting something from a variable
(def max-speed-kmh 25.0)
(can-cmd 54 (str-from-n (/ max-speed-kmh 3.6) "(conf-set 'max-speed %.3f)"))
```

---

#### can-start

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(can-start optPinTx optPinRx)
```

Start or restart CAN-driver. The optional arguments optPinTx and optPinRx can be used to specify which pins to use as CAN TX and CAN RX. If they are omitted the default pins will be used. If the hardware does not have default CAN pins and no pins are specified an error is returned. This function can also be used while the CAN-driver is running for changing CAN-pins.

---

#### can-stop

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(can-stop)
```

Stop CAN-bus driver.

---

#### can-use-vesc

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(can-use-vesc use-vesc)
```

Enable or disable decoding of the VESC Protocol on the CAN-bus. By default it is enabled. The VESC Protocol is used for communication with VESC Tool, the VESC BMS and VESC motor controllers on the CAN-bus. Disabling the VESC Protocol is useful for using the express as a generic CAN sniffer and debugger.

---

## CAN Messages

---

CAN messages are byte arrays of up to 500 bytes that can be sent between devices over CAN-bus. Together with flat values they are useful for e.g. remote code execution. Each CAN-device has 5 different slots to send messages to.

---

#### canmsg-recv

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(canmsg-recv slot timeout)
```

Wait for message on slot with timeout seconds. Returns a byte array with the received message on success or timeout if nothing is received before the timeout has passed. A negative timeout means wait forever.

---

#### canmsg-send

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(canmsg-send can-id slot msg)
```

Send msg over CAN-bus to slot on can-id. msg is a byte array.

---

### Math Functions

---

#### sin

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(sin angle)
```

Get the sine of angle. Unit: Radians.

---

#### cos

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(cos angle)
```

Get the cosine of angle. Unit: Radians.

---

#### tan

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(tan angle)
```

Get the tangent of angle. Unit: Radians.

---

#### asin

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(asin x)
```

Get the arc sine of x. Unit: Radians.

---

#### acos

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(acos x)
```

Get the arc cosine of x. Unit: Radians.

---

#### atan

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(atan x)
```

Get the arc tangent of x. Unit: Radians.

---

#### atan2

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(atan2 y x)
```

Get the arc tangent of y / x. Unit: Radians. This version uses the signs of y and x to determine the quadrant.

---

#### pow

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(pow base power)
```

Get base raised to power.

---

#### sqrt

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(sqrt x)
```

Get the square root of x.

---

#### log

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(log x)
```

Get the base-e logarithm of x.

---

#### log10

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(log10 x)
```

Get the base-10 logarithm of x.

---

#### floor

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.02+ |

```clj
(floor x)
```

Round x down to the closest integer. Added in FW 6.02.

---

#### ceil

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.02+ |

```clj
(ceil x)
```

Round x up to the closest integer. Added in FW 6.02.

---

#### round

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.02+ |

```clj
(round x)
```

Round x to the closest integer. Added in FW 6.02.

---

#### deg2rad

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(deg2rad x)
```

Converts x from degrees to radians.

---

#### rad2deg

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(rad2deg x)
```

Converts x from radians to degrees.

---

#### abs

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(abs x)
```

Get the absolute value of x.

---

#### throttle-curve

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(throttle-curve value accel brake mode)
```

Apply throttle curve on value. accel (range -1 to 1) is the curve constant for acceleration (when value is greater than 0) and brake (range -1 to 1) is the curve constant for braking (when value is less than 0). mode (0, 1 or 2) is the throttle curve mode. Negative curve constants mean that the throttle will be gentler in the beginning and more aggressive with towards the end and positive curve constants mean the opposite. The modes are 0: Exponential, 1: Natural and 2: Polynomial. You can have a look at the throttle curves in VESC Tool for the PPM, ADC or VESC Remote app and experiment with the mode and curve constants to see a plot of the response.

---

#### rand

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(rand optSeed)
```

Generate random number in the range 0 to (rand-max). Example:

```clj
; Generate integer in the range 0 to 99
(mod (rand) 100)

; Generate number in the range 0.0 to 1.0
(/ (to-float (rand)) (rand-max))
```

---

#### rand-max

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(rand-max)
```

Returns the maximum number that rand can return.

---

### Bit Operations

---

#### bits-enc-int

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(bits-enc-int initial offset number bits)
```

Put bits of number in initial at offset and return the result. For example, if the bits initial are aaaaaaaa, number is bbb, offset is 2 and bits is 3 the result is aaabbbaa. For reference, the corresponding operation in C is:

```c
initial &= ~((0xFFFFFFFF >> (32 - bits)) << offset);
initial |= (number << (32 - bits)) >> (32 - bits - offset);
```

---

#### bits-dec-int

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(bits-dec-int value offset size)
```

Return size bits of value at offset. For example if the bits of value are abcdefgh, offset is 3 and size it 3 a number with the bits cde is returned. The corresponding operation in C is:

```c
val >>= offset;
val &= 0xFFFFFFFF >> (32 - bits);
```

---

### Raw Commands

Raw data commands useful for debugging hardware issues.

---

#### raw-adc-current

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(raw-adc-current motor phase useRaw)
```

Get raw current measurements. Motor is the motor index (1 or 2), phase is the phase (1, 2 or 3) and useRaw is whether to convert the measurements to currents or to use raw ADC values.

**Note:**  
These samples can come from either V0 or V7 depending on when the function is called (although most likely V7 as less other computations happen then), so when the motor is running this is most likely not going to look good, especially if the hardware does not have phase shunts. This function is intended for debugging hardware and returns just was goes into the ADC without any processing.

Example for reading phase B on motor 1 as raw ADC values:

```clj
(raw-adc-current 1 2 1)
```

---

#### raw-adc-voltage

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(raw-adc-voltage motor phase useRaw)
```

Same as (raw-adc-current), but measures phase voltages instead.

---

#### raw-mod-alpha

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(raw-mod-alpha)
```

Get alpha modulation. Range -1.0 to 1.0 (almost).

---

#### raw-mod-beta

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(raw-mod-beta)
```

Get beta modulation. Range -1.0 to 1.0 (almost).

---

#### raw-mod-alpha-measured

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(raw-mod-alpha-measured)
```

Same as (raw-mod-alpha), but derives the modulation from the phase voltage reading and/or dead-time compensation.

---

#### raw-mod-beta-measured

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(raw-mod-beta-measured)
```
Same as (raw-mod-beta), but derives the modulation from the phase voltage reading and/or dead-time compensation.

---

#### raw-hall

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(raw-hall motor optSamples)
```
Read hall sensors for motor (1 or 2) and return their states in a list. The optional argument optSamples (max 20) can be used to set how many times the hall sensors are sampled; if it is not supplied the number of samples from the motor configuration will be used.

The function (ix list ind) can be used to get an element from the list. Example:
```clj
(ix (raw-hall 1) 0) ; Get hall sensor 1 state (index 0)
```

---

### UART

---

#### uart-start

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(uart-start baudrate optHd)
```

Start the UART driver at baudrate on the COMM-port. optHd is an optional argument that can be set to 'half-duplex to use half-duplex mode. In half-duplex mode only the tx-pin is used. If any app is using the UART pins it will be stopped first. Example:

```clj
(uart-start 115200) ; Start UART at 115200 baud in full duplex mode
(uart-start 115200 'half-duplex) ; Start UART at 115200 baud in half duplex mode
```

---

#### uart-start

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(uart-start uart-num rx-pin tx-pin baudrate)
```

This version of uart-start is for the Express-platform. Start the UART uart-num on the pins rx-pin and tx-pin with baudrate. Uart-num can be 0 or 1 and any pin can be used. Note that GNSS uses uart 0, so when a gnss-receiver is connected uart 1 must be used. Example:

```clj
; Start UART 1 with pin 20 for RX, pin 21 for TX and 115200 baudrate.
(uart-start 1 20 21 115200)
```

---

#### uart-stop

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(uart-stop)
```

Stop the UART-driver and free the resources it uses.

---

#### uart-write

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(uart-write array)
```

Write array (see [byte array](#byte-arrays) for details) to the UART. The array-argument can also be a list. Examples:

```clj
(uart-write "Hello World!") ; Write the string hello world!
```

```clj
(define arr (array-create 6)) ; Create a 6 byte long array
(bufset-i16 arr 0 1123) ; Set byte 0 and 1 to 1123
(bufset-i32 arr 2 424242) ; Set byte 2 to 5 to 424242 
(uart-write arr) ; Write arr to the uart
```

---

#### uart-read

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(uart-read array num optOffset optStopAt optTimeout)
```

Read num bytes into array at offset optOffset. Stop reading if the character optStopAt is received or optTimeout time has passed. The last three arguments are optional. Setting optTimeout to 0 (or omitting the argument) makes this function return immediately, even when there is nothing to read. The return value is the number of bytes read. The optional arguments can also be set to nil in case not all of them are used. Example:

```clj
; Read at most 10 characters into arr and return if it takes more than
; 0.5 seconds to receive anything.
(def arr (bufcreate 10))
(var res (uart-read arr 10 nil nil 0.5))
(print (list res arr))
```

---

#### uart-read-bytes

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(uart-read-bytes array num offset)
```

Read num bytes into buffer at offset. This function is blocking, so it will not return until the specified amount of bytes is read.

---

#### uart-read-until

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(uart-read-until array num offset end)
```

Same as uart-read-bytes, but will return when the byte end is read.

---

#### uartcomm-start

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(uartcomm-start uart-num rx-pin tx-pin baudrate)
```

Start VESC Tool-compatible packet handler over UART. That allows VESC Tool to connect to those pins using an USB-SERIAL adapter. Uart-num can be 0 or 1 and any pins can be used. Multiple UARTs can be used simultaneously.

---

#### uartcomm-stop

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(uartcomm-stop uart-num)
```

Stop UARTCOMM on uart-num.

---

### I2C

---

#### i2c-start

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(i2c-start optRate optPinSda optPinScl)
```

Start the I2C driver on the COMM-port on the VESC. If any app is using the I2C pins it will be stopped first. optRate is an optional argument for the I2C bitrate. optPinSda and optPinScl are optional arguments for using different SDA and SCL pins. Example:

```clj
(i2c-start 'rate-400k) ; 400 kbps and the default SDA and SDC pins
(i2c-start 'rate-100k 'pin-swdio 'pin-swclk) ; 100 kbps and SWDIO and SWCLK as SDA and SCL

; Available bitrates
'rate-100k
'rate-200k
'rate-400k
'rate-700k

; Available pins
'pin-rx
'pin-tx
'pin-swdio
'pin-swclk
'pin-hall1
'pin-hall2
'pin-hall3

; Note: On express the pins are a number
```

---

#### i2c-tx-rx

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(i2c-tx-rx addr arrTx optArrRx)
```

Send array (or list) arrTx to the I2C-device with address addr. Optionally receive a response to opArrRx. Example:

```clj
; Create 14 byte long array
(define arr (array-create 14))

; Send 0x3B to device 0x68 and receive 14 bytes into arr
(i2c-tx-rx 0x68 (list 0x3B) arr)
```

---

#### i2c-restore

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(i2c-restore)
```

Sends a sequence of bits in an attempt to restore the i2c-bus. Can be used if an i2c-device hangs and refuses to respond.

---

#### i2c-detect-addr

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(i2c-detect-addr address)
```

Test if address is present on the bus by writing to it and checking the nack-bit. Returns true if the address is present, nil otherwise. Can be used to detect if I2C-devices are plugged in and powered correctly.

---

#### imu-start-lsm6

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(imu-start-lsm6 optRate optPinSda optPinScl)
```

Start LSM6DSx IMU driver over i2c. Takes the same arguments as i2c-start and shares the same i2c-pins. Example:

```clj
(imu-start-lsm6 'rate-400k 21 20)
```

---

#### imu-stop

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(imu-stop)
```

Stop IMU-driver and put IMU in sleep mode.

---

### GPIO

These functions allow using GPIO-pins from lispBM. The UART and SWD pins can currently be used. NOTE: If you are using the SWD-pins a SWD-programmer won't work after that until the next reset. If you are using the hall sensor pins make sure that sensor port mode is not set to anything that will communicate with encoders using those pins. Leaving the sensor port in hall sensor mode should be fine.

The gpio-extension are also available on the express-platform. There the pins are a number (e.g. 1, 2) instead of a symbol.

---

#### gpio-configure

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(gpio-configure pin mode)
```

Configure GPIO pin to mode. Example:

```clj
(gpio-configure 'pin-rx 'pin-mode-out) ; Set pin RX to output

; Available pins
'pin-rx     ; RX-pin on the COMM-port
'pin-tx     ; TX-pin on the COMM-port
'pin-swdio  ; IO-pin on the SWD-port
'pin-swclk  ; CLK-pin on the SWD-port
'pin-hall1  ; Sensor port hall1
'pin-hall2  ; Sensor port hall2
'pin-hall3  ; Sensor port hall3
'pin-adc1   ; ADC1-pin on COMM-port
'pin-adc2   ; ADC2-pin on COMM-port
'pin-ppm    ; Signal-pin on PPM-port

; On express the pins are a number and not a symbol.

; Available modes
'pin-mode-out    ; Output
'pin-mode-od     ; Open drain output
'pin-mode-od-pu  ; Open drain output with pull-up
'pin-mode-od-pd  ; Open drain output with pull-down
'pin-mode-in     ; Input
'pin-mode-in-pu  ; Input with pull-up
'pin-mode-in-pd  ; Input with pull-down
'pin-mode-analog ; Analog (NOTE: only works on the ADC-pins)
```

---

#### gpio-write

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(gpio-write pin state)
```

Write state to pin. If the pin is set to an output 1 will set it to VCC and 0 to GND. If the pin is open drain 1 will set it floating and 0 will set it to GND. Example:

```clj
(gpio-write 'pin-rx 1) ; Set pin rx to 1
```

---

#### gpio-read

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(gpio-read pin)
```

Read state of pin. Returns 1 if the pin is high, 0 otherwise.

---

#### gpio-hold

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(gpio-hold pin state)
```

It state is 1, the state of pin will be locked to its state at the moment of the call. If state is 0 the pin will be unlocked. The state persists through reset, but not through deep sleep.

---

#### gpio-hold-deepsleep

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(gpio-hold-deepsleep state)
```

It state is 1, all digital pads will hold their state during deep sleep. If it is set to 0 they will reset to their initial state when entering deep sleep.

---

### Pulse-Width Modulation (PWM)

A logic-level PWM-signal can be created on GPIO-pins for controlling various accessories. The Express-platform supports up to 4 PWM-channels on any pins and the ESC-platform supports only one PWM-channel on the servo-pin. The arguments prefixed with express in the following functions are ignored on the ESC-platform.

---

#### pwm-start

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(pwm-start freq duty expressChannel expressPin optExpressBits)
```

Start PWM-generation at frequency freq Hz and duty-cycle duty. The arguments expressChannel, expressPin and optExpressBits are only used on the Express-platform and control which channel and pin to use and optionally how many bits of resolution to use (2 - 14 bits, 10 bits default). On the ESC-platform the PPM/Servo-pin is always used. This function returns the actual frequency that will be used as some rounding can be done when setting up the timer. Example:

```clj
; Express: 2 kHz PWM with 20% duty, channel 0 pin 20, 12 bits resolution
(pwm-start 2000 0.2 0 20 12)

; ESC: 2 kHz PWM with 20% duty on the Servo/PPM-pin
(pwm-start 2000 0.2)
```

---

#### pwm-stop

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(pwm-stop expressChannel)
```

Stop PWM-output. On the express-platform the channel must be specified.

---

#### pwm-set-duty

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(pwm-set-duty dutyCycle expressChannel)
```

Set PWM duty cycle. Range 0.0 to 1.0. On the express-platform the channel must be specified.

---

### Input Capture (ICU)

Input capture can be used to measure pulse lengths and periods on the PPM input pin. This can be used to measure the frequency and duty cycle of PWM-signals. The ICU driver was added in FW 6.02.

---

#### icu-start

| Platforms | Firmware |
|---|---|
| ESC | 6.02+ |

```clj
(icu-start freqHz pol)
```

Start input capture on the PPM-pin with timer frequency freqHz hertz and polarity pol. Polarity = 1 means that the high part of the pulse is measured and polarity = 0 means that the low part of the pulse is measured. Every time a pulse is received an [event](#events) is generated. Example:

```clj
; Run timer at 1 MHz and capture the positive part of the pulse
(icu-start 1000000 1)

(def cb-cnt 0)

(defun proc-icu (width period) {
        (def icu-w width)
        (def icu-p period)
        (def cb-cnt (+ cb-cnt 1))
})

(defun event-handler ()
    (loopwhile t
        (recv
            ((event-icu-period . ((? width) . (? period))) (proc-icu width period))
            (_ nil)
)))

; Spawn event handler
(event-register-handler (spawn event-handler))

; Enable the period event, which is generated at the end of the period.
(event-enable 'event-icu-period)
```

---

#### icu-width

| Platforms | Firmware |
|---|---|
| ESC | 6.02+ |

```clj
(icu-width)
```

Get the width of the last captured pulse. The unit is timer ticks, which depends on the freqHz you pick when running icu-start. E.g. if freqHz is 1000000 (1 MHz) the unit will be one microsecond.

---

#### icu-period

| Platforms | Firmware |
|---|---|
| ESC | 6.02+ |

```clj
(icu-period)
```

Get the period of the last captured pulse. The unit is timer ticks, which depends on the freqHz you pick when running icu-start. E.g. if freqHz is 1000000 (1 MHz) the unit will be one microsecond.

---

### AS504x Encoder

The AS504x (AS5047, AS5048) encoder can be used on any pins using software SPI.

---

#### as5047x-init

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(as5047x-init pin-miso pin-sck pin-cs)
```

Initialize the AS504x-driver on the specified pins.

---

#### as5047x-deinit

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(as5047x-deinit)
```

De-initialize the AS504x-driver and restore the pins.

---

#### as5047x-angle

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(as5047x-angle)
```

Read angle from the AS504x-encoder in degrees.

---

### Configuration

The following selection of app and motor parameters can be read and set from LispBM:

```clj
'l-current-min          ; Minimum current in A (a negative value)
'l-current-max          ; Maximum current in A
'l-current-min-scale    ; Scaled minimum current, 0.0 to 1.0
'l-current-max-scale    ; Scaled maximum current, 0.0 to 1.0
'l-in-current-min       ; Minimum input current in A (a negative value)
'l-in-current-max       ; Maximum input current in A
'l-abs-current-max      ; Abs max current in A
'l-min-erpm             ; Minimum ERPM (a negative value)
'l-max-erpm             ; Maximum ERPM
'l-erpm-start           ; Start limiting current at this fraction of max ERPM
'l-min-vin              ; Minimum input voltage
'l-max-vin              ; Maximum input voltage
'l-min-duty             ; Minimum duty cycle
'l-max-duty             ; Maximum duty cycle
'l-watt-min             ; Minimum power regen in W (a negative value)
'l-watt-max             ; Maximum power regen in W
'l-battery-cut-start    ; Voltage where current starts to get reduced
'l-battery-cut-end      ; Voltage below which current is not allowed
'l-temp-motor-start     ; Temperature where motor current starts to get reduced
'l-temp-motor-end       ; Temperature above which motor current is not allowed
'l-temp-accel-dec       ; Decrease temp limits this much during acceleration

; BMS Settings (Added in FW 6.05)
'bms-limit-mode         ; BMS limit mode bitfield
                        ; Bit 0: Enable temperature limit
                        ; Bit 1: Enable SOC limit
                        ; Bit 2: Enable VCell min limit
                        ; Bit 3: Enable VCell max limit
'bms-t-limit-start      ; Temperature where current starts to get reduced
'bms-t-limit-end        ; Temperature above which current is not allowed
'bms-vmin-limit-start   ; VCell where current starts to get reduced
'bms-vmin-limit-end     ; VCell below which current draw is not allowed
'bms-vmax-limit-start   ; VCell where regen current starts to get reduced
'bms-vmax-limit-end     ; VCell above which regen current is not allowed

'motor-type             ; Motor Type
                        ;    0: BLDC (6-step commutation)
                        ;    1: DC (DC motor on phase A and C)
                        ;    2: FOC (Field Oriented Control)
                        ;    3: GPD (General Purpose Drive)
'm-invert-direction     ; Invert motor direction, 0 or 1
'm-out-aux-mode         ; AUX-pin output mode. Options:
                        ;    0:  OUT_AUX_MODE_OFF
                        ;    1:  OUT_AUX_MODE_ON_AFTER_2S
                        ;    2:  OUT_AUX_MODE_ON_AFTER_5S
                        ;    3:  OUT_AUX_MODE_ON_AFTER_10S
                        ;    4:  OUT_AUX_MODE_UNUSED
                        ;    5:  OUT_AUX_MODE_ON_WHEN_RUNNING
                        ;    6:  OUT_AUX_MODE_ON_WHEN_NOT_RUNNING
                        ;    7:  OUT_AUX_MODE_MOTOR_50
                        ;    8:  OUT_AUX_MODE_MOSFET_50
                        ;    9:  OUT_AUX_MODE_MOTOR_70
                        ;    10: OUT_AUX_MODE_MOSFET_70
                        ;    11: OUT_AUX_MODE_MOTOR_MOSFET_50
                        ;    12: OUT_AUX_MODE_MOTOR_MOSFET_70
'm-motor-temp-sens-type ; Temperature sensor type (Added in FW 6.05). Options:
                        ;    0:  TEMP_SENSOR_NTC_10K_25C
                        ;    1:  TEMP_SENSOR_PTC_1K_100C
                        ;    2:  TEMP_SENSOR_KTY83_122
                        ;    3:  TEMP_SENSOR_NTC_100K_25C
                        ;    4:  TEMP_SENSOR_KTY84_130
                        ;    5:  TEMP_SENSOR_NTCX
                        ;    6:  TEMP_SENSOR_PTCX
                        ;    7:  TEMP_SENSOR_PT1000
                        ;    8:  TEMP_SENSOR_DISABLED
'foc-sensor-mode        ; FOC sensor mode
                        ;    0: FOC_SENSOR_MODE_SENSORLESS
                        ;    1: FOC_SENSOR_MODE_ENCODER
                        ;    2: FOC_SENSOR_MODE_HALL
                        ;    3: FOC_SENSOR_MODE_HFI
                        ;    4: FOC_SENSOR_MODE_HFI_START
                        ;    5: FOC_SENSOR_MODE_HFI_V2
                        ;    6: FOC_SENSOR_MODE_HFI_V3
                        ;    7: FOC_SENSOR_MODE_HFI_V4
                        ;    8: FOC_SENSOR_MODE_HFI_V5
'm-ntc-motor-beta       ; Beta Value for Motor Thermistor
'si-motor-poles         ; Number of motor poles, must be multiple of 2
'si-gear-ratio          ; Gear ratio (Added in FW 6.05)
'si-wheel-diameter      ; Wheel diameter in meters (Added in FW 6.05)
'si-battery-cells       ; Number of battery cells in series (Added in FW 6.05)
'si-battery-ah          ; Battery amp hours (Added in FW 6.05)
'foc-current-kp         ; FOC current controller KP
'foc-current-ki         ; FOC current controller KI
'foc-f-zv               ; Zero Vector Frequency in Hz (Added in FW 6.05)
'foc-motor-l            ; Motor inductance in microHenry
'foc-motor-ld-lq-diff   ; D and Q axis inductance difference in microHenry
'foc-motor-r            ; Motor resistance in milliOhm
'foc-motor-flux-linkage ; Motor flux linkage in milliWeber
'foc-observer-gain      ; Observer gain x1M
'foc-hfi-amb-mode       ; HFI Ambiguity Resolve Mode (FW 6.06)
                        ; 0 : Six Vector
                        ; 1 : ID Single Pulse
                        ; 2 : ID Double Pulse
'foc-hfi-amb-current    ; HFI Ambiguity Resolve Current (FW 6.06)
'foc-hfi-amb-tres       ; HFI Ambiguity Resolve Threshold (FW 6.06)
'foc-hfi-voltage-start  ; HFI start voltage (V) (for resolving ambiguity)
'foc-hfi-voltage-run    ; HFI voltage (V) HFI voltage at min current
'foc-hfi-voltage-max    ; HFI voltage (V) at max current
'foc-sl-erpm            ; Full sensorless control (Added in FW 6.05)
'foc-sl-erpm-start      ; Start sensorless transition here (Added in FW 6.05)
'foc-hall-t0            ; Hall table index 0 (Added in FW 6.05)
'foc-hall-t1            ; Hall table index 1 (Added in FW 6.05)
'foc-hall-t2            ; Hall table index 2 (Added in FW 6.05)
'foc-hall-t3            ; Hall table index 3 (Added in FW 6.05)
'foc-hall-t4            ; Hall table index 4 (Added in FW 6.05)
'foc-hall-t5            ; Hall table index 5 (Added in FW 6.05)
'foc-hall-t6            ; Hall table index 6 (Added in FW 6.05)
'foc-hall-t7            ; Hall table index 7 (Added in FW 6.05)
'foc-sl-erpm-hfi        ; ERPM where to move to sensorless in HFI mode
'foc-openloop-rpm       ; Use openloop commutation below this ERPM
'foc-openloop-rpm-low   ; Openloop ERPM and minimum current
'foc-sl-openloop-time-lock ; Locking time at the start of openloop
'foc-sl-openloop-time-ramp ; Time to ramp up to the openloop speed
'foc-sl-openloop-time   ; Stay in openloop for this amount of time
'foc-temp-comp          ; Use observer temperature compensation
'foc-temp-comp-base-temp ; Temperature at which parameters were measured
'foc-offsets-cal-on-boot ; Measure offsets at boot (FW 6.05 only)
'foc-offsets-cal-mode   ; Offset Calibration Mode (Added in FW 6.06)
                        ; Bit 0: Calibrate on Boot
                        ; Bit 1: Enable Write from VESC Tool
                        ; Bit 2: Auto-calibrate when undriven
'foc-fw-current-max     ; Maximum field weakening current (Added in FW 6.05)
'foc-fw-duty-start      ; Duty where field weakening starts (Added in FW 6.05)
'foc-short-ls-on-zero-duty ; Short low-side FETs on 0 duty (Added in FW 6.05)
'min-speed              ; Minimum speed in meters per second (a negative value)
'max-speed              ; Maximum speed in meters per second
'app-to-use             ; App to use
                        ;    0: APP_NONE
                        ;    1: APP_PPM
                        ;    2: APP_ADC
                        ;    3: APP_UART
                        ;    4: APP_PPM_UART
                        ;    5: APP_ADC_UART
                        ;    6: APP_NUNCHUK
                        ;    7: APP_NRF
                        ;    8: APP_CUSTOM
                        ;    9: APP_PAS
                        ;    10: APP_ADC_PAS
'controller-id          ; VESC CAN ID
'timeout-msec           ; Motor timeout in milliseconds (Added in FW 6.06)
'can-baud-rate          ; CAN-bus baud rate (Added in FW 6.05)
                        ; 0: 125K
                        ; 1: 250K
                        ; 2: 500K
                        ; 3: 1M
                        ; 4: 10K
                        ; 5: 20K
                        ; 6: 50K
                        ; 7: 75K
                        ; 8: 100K
'can-status-rate-1      ; CAN status rate 1 in Hz (Added in FW 6.05)
'can-status-msgs-r1     ; Bitfield with the status messages (Added in FW 6.05)
                        ; Bit0: Status 1 (RPM, Current, Duty)
                        ; Bit1: Status 2 (Ah Used, Ah Charged)
                        ; Bit2: Status 3 (Wh Used, Wh Charged)
                        ; Bit3: Status 4 (T FET, T Mot, I In, PID pos)
                        ; Bit4: Status 5 (V In, Tacho)
                        ; Bit5: Status 6 (ADC1, ADC2, ADC3, PPM)
'can-status-rate-2      ; CAN status rate 2 in Hz (Added in FW 6.05)
'can-status-msgs-r2     ; See can-status-msgs-r1 (Added in FW 6.05)
'ppm-ctrl-type          ; PPM Control Type
                        ;    0:  PPM_CTRL_TYPE_NONE
                        ;    1:  PPM_CTRL_TYPE_CURRENT
                        ;    2:  PPM_CTRL_TYPE_CURRENT_NOREV
                        ;    3:  PPM_CTRL_TYPE_CURRENT_NOREV_BRAKE
                        ;    4:  PPM_CTRL_TYPE_DUTY
                        ;    5:  PPM_CTRL_TYPE_DUTY_NOREV
                        ;    6:  PPM_CTRL_TYPE_PID
                        ;    7:  PPM_CTRL_TYPE_PID_NOREV
                        ;    8:  PPM_CTRL_TYPE_CURRENT_BRAKE_REV_HYST
                        ;    9:  PPM_CTRL_TYPE_CURRENT_SMART_REV
                        ;    10: PPM_CTRL_TYPE_PID_POSITION_180
                        ;    11: PPM_CTRL_TYPE_PID_POSITION_360
'ppm-pulse-start        ; Shortest PPM pulse in ms
'ppm-pulse-end          ; Longest PPM pulse in ms
'ppm-pulse-center       ; Pulse corresponding to center throttle in ms
'ppm-ramp-time-pos      ; Positive ramping time in seconds
'ppm-ramp-time-neg      ; Negative ramping time in seconds
'adc-ctrl-type          ; ADC Control Type (Added in FW 6.02)
                        ;    0:  ADC_CTRL_TYPE_NONE
                        ;    1:  ADC_CTRL_TYPE_CURRENT
                        ;    2:  ADC_CTRL_TYPE_CURRENT_REV_CENTER
                        ;    3:  ADC_CTRL_TYPE_CURRENT_REV_BUTTON
                        ;    4:  ADC_CTRL_TYPE_CURRENT_REV_BUTTON_BRAKE_ADC
                        ;    5:  ADC_CTRL_TYPE_CURRENT_REV_BUTTON_BRAKE_CENTER
                        ;    6:  ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_CENTER
                        ;    7:  ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_BUTTON
                        ;    8:  ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_ADC
                        ;    9:  ADC_CTRL_TYPE_DUTY
                        ;    10: ADC_CTRL_TYPE_DUTY_REV_CENTER
                        ;    11: ADC_CTRL_TYPE_DUTY_REV_BUTTON
                        ;    12: ADC_CTRL_TYPE_PID
                        ;    13: ADC_CTRL_TYPE_PID_REV_CENTER
                        ;    14: ADC_CTRL_TYPE_PID_REV_BUTTON
'adc-ramp-time-pos      ; Positive ramping time in seconds (Added in FW 6.05)
'adc-ramp-time-neg      ; Negative ramping time in seconds (Added in FW 6.05)
'adc-thr-hyst           ; Throttle hysteresis, range 0 to 1 (Added in FW 6.05)
'adc-v1-start           ; Throttle 1 start voltage (Added in FW 6.05)
'adc-v1-end             ; Throttle 1 end voltage (Added in FW 6.05)
'adc-v1-min             ; Throttle 1 low fault voltage (Added in FW 6.05)
'adc-v1-max             ; Throttle 1 high fault voltage (Added in FW 6.05)
'pas-current-scaling    ; PAS current scaling (Added in FW 6.05)

; Express settings (Added in 6.05)
'controller-id          ; VESC CAN ID
'can-baud-rate          ; CAN-bus baud rate
                        ; 0: 125K
                        ; 1: 250K
                        ; 2: 500K
                        ; 3: 1M
                        ; 4: 10K
                        ; 5: 20K
                        ; 6: 50K
'can-status-rate-hz     ; CAN status message rate
'wifi-mode              ; Wifi mode
                        ; 0: Disabled
                        ; 1: Station
                        ; 2: Access Point
'wifi-sta-ssid          ; Wifi station SSID
'wifi-sta-key           ; Wifi station Key
'wifi-ap-ssid           ; Wifi access point SSID
'wifi-ap-key            ; Wifi access point key
'use-tcp-local          ; Use local TCP server
'use-tcp-hub            ; Connecto to TCP hub
'tcp-hub-url            ; TCP hub URL
'tcp-hub-port           ; TCP hub port
'tcp-hub-id             ; TCP hub connection ID
'tcp-hub-pass           ; TCP hub password
'ble-mode               ; BLE mode
                        ; 0: Disabled
                        ; 1: Enabled
                        ; 2: Enabled and encrypted with pin
                        ; 3: Enabled with scripting
'ble-name               ; Device name (also the name that shows up in VESC Tool)
'ble-pin                ; BLE pin code
```

---

#### conf-set

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(conf-set param value)
```

Set param to value. This can be done while the motor is running and it will be applied instantly. Note that the parameter won't be stored in flash, so it will be back to the old value on the next boot. To store all parameters that have been changed you can use [conf-store](#conf-store). Example:

```clj
(conf-set 'max-speed (/ 25 3.6)) ; Set the maximum speed to 25 km/h
```

---

#### conf-get

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(conf-get param optDefLim)
```

Get the value of param. optDefLim is an optional argument that can be set to 1 or 2; 1 means get the default value and 2 means get the limit value. Example:

```clj
(conf-get 'foc-motor-r) ; Get the motor resistance in milliOhm
(conf-get 'controller-id 1) ; Get the default CAN ID of this VESC
(conf-get 'l-current-max 2) ; Get the maximum allowed current on this hardware
```

---

#### conf-store

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(conf-store)
```

Store the current configuration to flash. This will stop the motor. Note: On the express most settings require a reboot to be applied. Remember to use conf-store before rebooting.

---

#### conf-detect-foc

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(conf-detect-foc canFwd maxLoss minCurrIn maxCurrIn openloopErpm slErpm)
```

Run the same autodetection as the wizard in VESC Tool does. This function will block the current lispBM-thread until in finishes (other threads will continue running). Arguments:

```clj
canFwd       ; Scan CAN-bus and detect on all VESCs found on CAN-bus
maxLoss      ; Maximum power loss in W to derive current limit from
minCurrIn    ; Minimum input current in A (negative value)
maxCurrIn    ; Maximum input current in A
openLoopErpm ; Openlopp ERPM setting
slErpm       ; Sensorless ERPM setting
```

Result:

```clj
  0 ; OK and no sensors found
  1 ; OK and hall sensors found
  2 ; OK and AS5047 found
 -1 ; Fault code during sensor detection
-10 ; Flux linkage detection failed
-50 ; CAN-detection failed
-51 ; CAN-detection timed out
```

Example:

```clj
; No can detection, 50w losses max, -20 A to 50 A input current, 800 ERPM openloop and 2500 erpm for sensorless in case sensors are found
(print (conf-detect-foc 0 50 -20 50 800 2500))
; Print the result when done
```

---

#### conf-set-pid-offset

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(conf-set-pid-offset offset optStore)
```

Set the PID controller offset such that the current angle becomes offset. This can be used in position control applications when e.g. homing against a limit switch. The optional argument optStore can be set to true to store the offset persistently (although that requires stopping the motor).

---

#### conf-measure-res

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(conf-measure-res current optSamples)
```

Measure motor resistance with current. The optional argument optSamples sets the number of samples to use (default 100).

This command is useful to update the configuration before starting the motor as the resistance is the most important when it comes to sensorless low-speed performance. It is also useful to sanity check if the motor is shorted out or if a connector is loose. Such faults cause a relatively significant change in resistance. Changes with more than 50% compared to the detected value are most likely faults.

**NOTE:** Phase filters are required to get accurate resistance measurements, so resistance-based fault detection is not as useful on hardware without phase filters.

---

#### conf-measure-ind

| Platforms | Firmware |
|---|---|
| ESC | 6.02+ |

```clj
(conf-measure-ind target-current optSamples)
```

Measure motor inductance with target-current. The optional argument optSamples sets the number of samples to use (default 100).

returns: ({ld_lq_avg} {ld_lq_diff} {actual_measurement_current} fault-code)

Can not be used when the motor is running. The measurement current is not gaurenteed to reach the target, and the actual_measurement_current parameter should be used to verify the actual current used.

Useful for finding the saturation inductance curve of a motor.

---

#### conf-restore-mc

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(conf-restore-mc)
```

Restore motor configuration to the default values on the selected motor. The current and voltage offsets are kept from the old configuration.

---

#### conf-restore-app

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(conf-restore-app)
```

Restore app configuration to the default values.

---

#### conf-dc-cal

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(conf-dc-cal calUndriven)
```

Run FOC DC offset calibration. calUndriven can be set to true for including the undriven voltages in the calibration, which requires that the motor stands still. If the calibration fails nil is returned and on success a list with the new offsets in the following format is returned:

```clj
(i0 i1 i2 v0 v1 v2 v0-undriven v1-undriven v2-undriven)
```

---

#### conf-get-limits

| Platforms | Firmware |
|---|---|
| ESC | 6.05+ |

```clj
(conf-get-limits)
```

Get all overridden current limits from speed, temperature, voltage, wattage etc. Return the following list:

```clj
(motor-min motor-max input-min input-max)
```

---

### EEPROM (Nonvolatile Storage)

Up to 128 variables (int32 or float) can be stored in a nonvolatile memory reserved for LispBM. These variables persist between power cycles and configuration changes, but not between firmware updates. Keep in mind that the motor will be stopped briefly when writing them and that they only can be written a limited number of times (about 100 000 writes) before wear on the flash memory starts to become an issue.

---

#### eeprom-store-f

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(eeprom-store-f addr number)
```

Store float number on emulated eeprom at address addr. Addr range: 0 to 127. Note that this will stop the motor briefly as writing to the flash memory cannot be done at the same time as the motor is running.

---

#### eeprom-read-f

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(eeprom-read-f addr)
```

Read float number on emulated eeprom at address addr. Addr range: 0 to 127. If nothing was stored on that address this function returns nil.

---

#### eeprom-store-i

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(eeprom-store-i addr number)
```

Same as eeprom-store-f, but store number as i32 instead of float.

---

#### eeprom-read-i

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(eeprom-read-i addr)
```

Same as eeprom-read-i, but read number as i32 instead of float.

---

### Loops

---

#### loopfor

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(loopfor it start cond update body)
```

For-loop. it is the iterator, start is what it is initialized to, cond is the condition that has the be true for the loop to continue running, update is how to update the iterator after each iteration and body is the code to execute each iteration. The iterator can be accessed from within body. Example:

```clj
(loopfor i 0 (< i 5) (+ i 1)
    (print i)
)

Output:
0
1
2
3
4

; Remember that multiple statements in the loop require {} or progn:
(loopfor i 0 (< i 5) (+ i 1) {
        (print i)
        (sleep 0.5)
})
```

---

#### loopwhile

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(loopwhile cond body)
```

While-loop. cond is the condition that has the be true for the loop to continue running and body is the code to execute each iteration. Example:

```clj
(define i 0)

(loopwhile (< i 5) {
        (print i)
        (define i (+ i 1))
})

Output:
0
1
2
3
4
```

Another example that prints "Hello World" every two seconds:

```clj
(loopwhile t {
        (print "Hello World")
        (sleep 2)
})
```

---

#### looprange

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(looprange it start end body)
```

Range-loop. Iterate it from start to end and evaluate body for each iteration. The iterator it can be accessed from within body. Example:

```clj
(looprange i 0 5
    (print i)
)

Output:
0
1
2
3
4

; As with the other loops, multiple statements require {} or progn
(looprange i 0 5 {
        (print i)
        (sleep 0.5)
})
```

---

#### loopforeach

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(loopforeach it lst body)
```

ForEach-loop. Iterate over every element in the list lst and evaluate body for each iteration. The iterator it can be accessed from within body. Example:

```clj
(loopforeach i '("AB" "C" "dE" "f")
    (print i)
)

Output:
AB
C
dE
f

; As with the other loops, multiple statements require {} or progn
(loopforeach i '("AB" "C" "dE" "f") {
        (print i)
        (sleep 0.5)
})

```

---

#### loopwhile-thd

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(loopwhile-thd stack cond body)
```

While-loop that starts in a new thread. The argument stack is the stack-size of the thread, cond is the condition that has the be true for the loop to continue running and body is the code to execute each iteration. The difference from the regular loopwhile is that the evaluator will continue running the code after this one before this one finishes, as this loop is evaluated in a new thread.

Example that forever prints "Hello World" every two seconds:

```clj
(loopwhile-thd 100 t {
        (print "Hello World")
        (sleep 2)
})

; The above is equivalent to the following code

(spawn 100 (fn () (loopwhile t {
                (print "Hello World")
                (sleep 2)
})))
```

From firmware 6.06 it is possible to give the thread a name and/or a stack size. That gives the following combinations of possibilities:

```clj
; No name and default stack size
(loopwhile-thd () t {
        (print "Hello World1")
        (sleep 2)
})

; No name and stack size 100
(loopwhile-thd 100 t {
        (print "Hello Worl2")
        (sleep 2)
})

; Name ThdTest and default stack size
(loopwhile-thd "ThdTest" t {
        (print "Hello World3")
        (sleep 2)
})

; Name ThdTest2 and stack size 100
(loopwhile-thd ("ThdTest2" 100) t {
        (print "Hello World4")
        (sleep 2)
})
```

---

#### break

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(break retval)
```

break can be used to break out of a loop and return retval (the result of the loop will be retval, otherwise the result of the loop will be the result of the last expression in it). break works in all of the loops above. Example:

```clj
; Below we make a function to determine if
; the list lst contains number num

(defun contains (num lst)
    (loopforeach it lst
        (if (= it num)
            (break t)
            nil
)))

(contains 346 '(12 33 452 11 22 346 99 12))
> t

(contains 347 '(12 33 452 11 22 346 99 12))
> nil
```

---

### Useful Lisp Functions

There are a number of lisp functions that can be used from lispBM in the VESC firmware. They will be loaded to the environment the first time they are used, so they do not use up memory before the first use.

---

#### defun

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(defun (args) body)
```

Shorthand macro for defining a function. Example:

```clj
; Create function f with argument x that prints x
(defun f (x)
    (print x)
)

; The above is equivalent to
(define f (lambda (x)
    (print x)
))
```

---

#### defunret

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(defunret (args) body)
```

Same as defun, but allows returning at any point. This one has a bit more overhead than defun as it uses call-cc internally, which is why both exist.

```clj
(defunret test (a b) {
        (if (> a b)
            (return (+ a 5))
        )
        
        (+ a b)
})

(test 2 2)
> 4

(test 3 2)
> 8
```

---

#### map

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(map f lst)
```

Apply function f to every element in list lst. Example:

```clj
(map (fn (x) (* x 5)) '(1 2 3 4))
> (5 10 15 20)
```

This example creates an anonymous function that takes one argument and returns that argument multiplied by 5. Map then applies it to every element in the list (1 2 3 4), which yields the list (5 10 15 20). In the later version of lispBM map is a "native" function, meaning that it is quite fast and efficient.

---

#### range

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(range optStart end)
```

Create a list from start to end, excluding end. Range also works with just one argument, which is takes as end assuming start is 0. Example:

```clj
(range 2 8)
> (2 3 4 5 6 7)

(range 5)
> (0 1 2 3 4)
```

---

#### foldl

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(foldl f init lst)
```

Apply the function f to pairs of init and each element of the list lst and accumulate the result. Example:

```clj
(foldl + 0 '(1 2 3 4 5))
> 15
```

---

#### foldr

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

Same as foldl, but start from the right side of lst. Note that foldl is tail recursive but foldr is not, so if possible use foldl.

---

#### reverse

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(reverse lst)
```

Returns the list lst in reverse. Example:

```clj
(reverse '(1 2 3 4 5))
> (5 4 3 2 1)
```

---

#### length

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(length lst)
```

Returns the length of list lst. Example:

```clj
(length '(1 2 3))
> 3
```

---

#### apply

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(apply f lst)
```

Use the elements in list lst as arguments to function f. Example:

```clj
(apply + '(1 2 3))
> 6
```

---

#### zipwith

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(zipwith f x y)
```

Apply the function f to pairs between the elements in list x and list y. Example:

```clj
(zipwith * '(1 2 3) '(3 4 5))
> (3 8 15)
```

---

#### filter

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(filter f lst)
```

Filter list by keeping the elements on which f returns true. Example:

```clj
(filter (fn (x) (< x 5)) '(3 9 5 8 2 4 7))
> (3 2 4)
```

---

#### sort

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(sort f lst)
```

Sort list lst using comparison function f. Example:

```clj
(sort < '(5 6 2 1 5 63 33 7 7 8))
> (1 2 5 5 6 7 7 8 33 63)

(sort > '(5 6 2 1 5 63 33 7 7 8))
> (63 33 8 7 7 6 5 5 2 1)

; Split sentence to words and sort them in ascending order
(sort str-cmp-asc (str-split "this is a string" " "))
> ("a" "is" "string" "this")
```

Note: Sort is quite slow the way it is implemented now. If sorting becomes a bottleneck in your application you can open an issue on github and hopefully someone will look into that and make a fast implementation. **Update**: Since firmware 6.05 sort is a built-in function that uses the merge sort algorithm. That makes it fast and it works well on large lists.

---

### String Manipulation

---

#### str-from-n

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-from-n n optFormat)
```

Create a string from the number n. Also takes an optional format argument optFormat that works in the same way as the printf-function in C. The optFormat argument can also be used together with other characters as long as the resulting output string is shorter than 100 characters. Example:

```clj
(str-from-n 10)
> "10"

(str-from-n 2.5)
> "2.500000"

(str-from-n 2.5 "%.1f")
> "2.5"

(str-from-n 10 "0x%04X") ; Here we also append 0x in front of optFormat
> "0x000A"

(str-from-n 0.023e3)
> "2.500000"
```

---

#### str-join

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(str-join strings opt-delim)
```

Joins a list of strings into one, with an optional delimeter string placed
between each.

Note: Passing in an empty list returns `""`.

Examples:
```clj
(str-join '("a" "b" "c"))
> "abc"

(str-join '("I" "love" "lispbm!") " ")
> "I love lispbm!"

(str-join '() " ")
> ""
```

---

#### str-merge

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-merge str1 str2 ...)
```

Merge a number of strings into one. Example:

```clj
(str-merge "A" "bC" "D")
> "AbCD"

(str-merge "Num1: " (str-from-n 10) " Num2: " (str-from-n 2.1 "%.1f"))
> "Num1: 10 Num2: 2.1"
```

---

#### str-to-i

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-to-i str optBase)
```

Convert string to integer. By default the base is chosen automatically, but it can also be specified. Example:

```clj
(str-to-i "123")
> {123}

(str-to-i "a" 16)
> {10}

(str-to-i "0xa") ; Automatic base16 if str starts with 0x
> {10}
```

---

#### str-to-f

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-to-f str)
```

Convert string to floating point number. Example:

```clj
(str-to-f "2.5")
> {2.500000}

; Also supports scientific notation
(str-to-f "0.0025e3")
> {2.500000}
```

---

#### str-part

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-part str start optN)
```

Take part of string str starting at start for optN characters. If optN is omitted the rest of str will be taken. Example:

```clj
(str-part "Hello World!" 6)
> "World!"

(str-part "Hello World!" 6 2)
> "Wo"

(str-part "Hello World!" 0 2)
> "He"
```

---

#### str-split

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-split str delim)
```

Split string str into tokens using delimiter delim. If delim is a number str will be split into tokens the size of that number. Example:

```clj
(str-split "This is a test" " ")
> ("This" "is" "a" "test")

(str-split "this_o_is_o_a_o_test" "_o_")
> ("This" "is" "a" "test")

(str-split "This is a test" 3)
> ("Thi" "s i" "s a" " te" "st")

(str-split "This is a test" 1)
> ("T" "h" "i" "s" " " "i" "s" " " "a" " " "t" "e" "s" "t")
```

---

#### str-replace

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-replace str rep optWith)
```

Replace every occurrence of rep in str with optWith. If optWith is omitted every rep will be removed. Example:

```clj
(str-replace "Hello World!" "World" "LispBM")
> "Hello LispBM!"

(str-replace "Hello World!" " World")
> "Hello!"
```

---

#### str-to-upper

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-to-upper str)
```

Convert string str to upper case. Example:

```clj
(str-to-upper "TesTt")
> "TESTT"
```

---

#### str-to-lower

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-to-lower str)
```

Convert string str to lower case. Example:

```clj
(str-to-lower "TesTt")
> "testt"
```

---

#### str-cmp

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-cmp str1 str1 optN)
```

Compare strings str1 and str2. Works in the same way as the strcmp-function in C, meaning that equal strings return 0 and different strings return their difference according how they would be sorted.

The optional argument optN can be used to specify how many characters to compare (like strncmp in C). If it is left out all characters will be compared.

Example:

```clj
(str-cmp "Hello" "Hello")
> 0

(str-cmp "Hello" "World")
> -15

(str-cmp "World" "Hello")
> 15

(str-cmp "ab" "abcd" 2) ; Compare only the first two characters
> 0
```

---

#### str-cmp-asc

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-cmp-asc str1 str1)
```

Return true if str1 comes before str2, nil otherwise. Useful for sorting strings using the [sort](#sort) function in ascending order.

---

#### str-cmp-dsc

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-cmp-dsc str1 str1)
```

Return true if str2 comes before str1, nil otherwise. Useful for sorting strings using the [sort](#sort) function in descending order.

---

#### str-len

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(str-len str)
```

Calculate length of string str excluding the null termination. Example:

```clj
(str-len "Hello")
> 5
```

---

#### str-find

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(str-find str substr [start] [occurrence] [direction])
```

Finds the index of the first character of a substring `substr` in the string `str`,
returning this index, or `-1` if it doesn't exist. `substr` can also be a list
of strings, in which case a certain positon only needs to be equal to one of
these substrings to be considered a match. If this list is empty, `-1` is always
returned, and searching for an empty string returns the starting index.

**Optional arguments:**  
- `start`: Overrides which index the search starts at. May be negative, in which
  case it specifies which index to start at from the end, with `-1` resulting in
  starting at the last character of the string (See one of the examples below).
  Defaults to 0 if searching to the right, otherwise to the end of the string
  minus the length of `substr` if searching to the left. If it helps, the
  formula is then `(- (+ (str-len str) 1) (str-len substr))`, if `substr` is a
  list, the shortest string length is used.
- `occurrence`: When given, this specifies how many matches to skip before
  an index is returned, otherwise the index of the first is returned by default.
- `direction`: The direction to perform the search in. One of the symbols `left`
  or `right`. Defaults to `right`, starting from the first character. Can be passed
  in an earlier position if `occurrence` and/or `start` are left out.

It is valid if the resulting start index is outside the range of `str` (i.e.
being smaller than 0 or larger than the size of `str`), in which case the
expected behaviour is performed by either immediatly returning no match (`-1`)
or skipping the index forward to the first potentially valid match (depending on
the direction).

**Note about byte arrays**  
`str` is actually considered to be a byte array, meaning that the final
terminating null byte is considered when searching. The final byte of `substr`
is on the other hand removed before it is searched for. If you want to search
for an abitrary byte sequence you must first increase the size of `substr` by
one, for instance with `(buf-resize substr 1 'copy)`.
Note however that since specifying `start` to be `-1` starts the search at the
last character (which is the second to last byte in a null-terminated string),
**it is impossible to specify that the search should start at the last byte**!
(If you need that functionality, you'll unfortunately need to implement that
yourself and only specify positive values for `start`.)  
This function actually replaces `buf-find` which existed in earlier versions of
v6.05. This version is almost backwards compatible, so you should be able to
just search and replace `buf-find` with `str-find` for most cases. The only
change is that the third argument `occurrence` has been moved to the fourth
position (with `start` taking the third). So you'll need to replace
`(buf-find buf seq x)` with `(str-find buf seq 0 x)` in this case.

Examples:
```clj
(str-find "-str-" "str")
> 1

(str-find "-str-str-" "str" 0 1)
> 5

(str-find "-str-str-" "str" 2)
> 5

(str-find "-str-str-" "str" 'left)
> 5

(str-find "-ab-ab-" "ab" 5 'left)
> 4

(str-find "-ab-ba-" '("ba" "ab") 0 1)
> 4

(str-find "a--a" "a" -1 'left)
> 3
```

---

#### to-str

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(to-str arg1 ... argN)
```

Convert LBM-types to their string representation and return that string. Example:

```clj
(to-str '(1 2 3))
> "(1 2 3)"

(to-str "aAa" 4 '(a 2 3) 2 3 "Hello")
> "aAa 4 (a 2 3) 2 3 Hello"
```

---

#### to-str-delim

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(to-str-delim delimiter arg1 ... argN)
```

Same as [to-str](#to-str), but with a custom delimiter. Example:

```clj
(to-str-delim "::" "aAa" 4 '(a 2 3) 2 3 "Hello")
> "aAa::4::(a 2 3)::2::3::Hello"
```

---

## Events

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

Events can be used to execute code for certain events, such as when CAN-frames are received. To use events you must first register an event handler, then enable the events you want to receive. As the event handler blocks until the event arrives it is useful to spawn a thread to handle events so that other things can be done in the main thread at the same time.

The following example shows how to spawn a thread that handles SID (standard-id) CAN-frames and custom app data:

```clj
(defun proc-sid (id data)
    (print (list id data))
)

(defun proc-data (data)
    (print data)
)

(defun event-handler ()
    (loopwhile t
        (recv
            ((event-can-sid . ((? id) . (? data))) (proc-sid id data))
            ((event-data-rx . (? data)) (proc-data data))
            (_ nil) ; Ignore other events
)))

; Spawn the event handler thread and pass the ID it returns to C
(event-register-handler (spawn event-handler))

; Enable the CAN event for standard ID (SID) frames
(event-enable 'event-can-sid)

; Enable the custom app data event
(event-enable 'event-data-rx)
```

Possible events to register are

```clj
(event-enable 'event-can-sid)  ; -> (event-can-sid . (id . data)), where id is U32 and data is a byte array
(event-enable 'event-can-eid)  ; -> (event-can-eid . (id . data)), where id is U32 and data is a byte array
(event-enable 'event-data-rx)  ; -> (event-data-rx . data), where data is a byte array
(event-enable 'event-shutdown) ; -> event-shutdown
(event-enable 'event-icu-width) ; -> (event-icu-width . (width . period))
(event-enable 'event-icu-period) ; -> (event-icu-period . (width . period))

; BMS events (currently express only)
(event-enable 'event-bms-chg-allow) ; -> (event-bms-chg-allow allow)
(event-enable 'event-bms-bal-ovr) ; -> (event-bms-bal-ovr ch bal)
(event-enable 'event-bms-reset-cnt) ; -> event-bms-reset-cnt
(event-enable 'event-bms-force-bal) ; -> (event-bms-force-bal force)
(event-enable 'event-bms-zero-ofs) ; -> event-bms-zero-ofs

; Other express only events
(event-enable 'event-ble-rx) ; -> (event-ble-rx handle data)
(event-enable 'event-wifi-disconnect) ; -> ('event-wifi-disconnect reason from-extension)
```

The CAN-frames arrive whenever data is received on the CAN-bus and data-rx is received for example when data is sent from a Qml-script in VESC Tool.

### Event Description

**event-can-sid**  
This event is sent when standard id CAN-frames are received.

**event-can-eid**  
This event is sent when extended id CAN-frames are received.

**event-data-rx**  
This event is sent when custom app data is sent from VESC Tool or other connected devices. This works using all communication ports including USB, UART and CAN-bus.

**event-shutdown**  
This event is sent when the VESC is about to shut down. Note that this event currently only works on hardware with a power switch. If that is not the case you could try to, for example, monitor the input voltage and simulate this event when it drops below a set level.

**event-icu-width**  
This event is sent when the input capture unit captures a pulse. Both the pulse width and the period of the last pulse are provided.

**event-icu-period**  
This event is sent when the input capture unit ends a period and the next pulse starts. Both the pulse width and the period are provided.

**event-ble-rx** (Express exclusive)  
This event is sent when a client connected to the VESC writes a value to a
characteristic or descriptor. Read the
[BLE docs](https://github.com/vedderb/vesc_express/tree/main/main/ble#events)
for details.

**event-wifi-disconnect** (Express exclusive)  
This event is sent when the VESC disconnects from the currently connected
network for any reason. Read the
[Wi-Fi docs](https://github.com/vedderb/vesc_express/tree/main/main/wifi#events)
for details.

---

## Byte Arrays

Byte arrays (and text strings) are allocated in memory as consecutive arrays of bytes (not linked lists). They can be shared with C and are more space and performance efficient than linked lists. Several of the extensions also take byte arrays as input as an alternative to lists and some of the events return byte arrays.

To allocate a byte array with 20 bytes and bind the symbol `arr` to it you can use

```clj
(define arr (array-create 20))
```

---

#### buflen

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

The length of a byte array can be read with

```clj
(buflen arr)
```

Which will return 20 for the array `arr` above.

---

#### bufclear

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

To clear a byte array the function bufclear can be used:

```clj
(bufclear arr optByte optStart optLen)
```

Where `arr` is the byte array to clear, `optByte` is the optional argument of what to clear with (default 0), `optStart` is the optional argument of which position to start clearing (default 0) and `optLen` is the optional argument of how many bytes to clear after start (default the entire array). Example:

```clj
(bufclear arr) ; Clear all of arr
(bufclear arr 0xFF) ; Fill arr with 0xFF
(bufclear arr 0 5) ; Clear from index 5 to the end
(bufclear arr 0 5 10) ; Clear 10 bytes starting from index 5
(bufclear arr 0xAA 5 10) ; Set 10 bytes to 0xAA starting from index 5
```

---

#### bufget-\[x\]

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

To read data from the byte array you can use

```clj
(bufget-[x] arr index)
```

Where \[x\] is i8, u8, i16, u16, i32, u32 or f32. Index is the position in the array to start reading from, starting at 0. Here are some examples

```clj
(bufget-i8 arr 0) ; read byte 0 as int8
(bufget-i16 arr 0) ; read byte 0 and 1 as int16
(bufget-i32 arr 0) ; read byte 0 to 3 as i32
(bufget-u8 arr 0) ; read byte 0 as uint8
(bufget-u16 arr 0) ; read byte 0 and 1 as uint16
(bufget-u24 arr 0) ; read byte 0, 1 and 2 as uint24
(bufget-u32 arr 0) ; read byte 0 to 3 as uint32
(bufget-f32 arr 0) ; read byte 0 to 3 as float32 (IEEE 754)
```

By default the byte order is big endian. The byte order can also be specified as an extra argument. E.g. to read 4 bytes as int32 from position 6 in little endian you can use

```clj
(bufget-i32 arr 6 'little-endian)
```

---

#### bufset-\[x\]

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

Writing to the array can be done in a similar way

```clj
(bufset-[x] arr index value)
```

Here are some examples

```clj
(bufset-i8 arr 0 12) ; write 12 to byte 0 as int8
(bufset-i16 arr 0 -5621) ; write -5621 to byte 0 and 1 as int16
(bufset-i32 arr 0 2441) ; write 2441 to byte 0 to 3 as i32
(bufset-u8 arr 0 12) ; write 12 to byte 0 as uint8
(bufset-u16 arr 0 420) ; write 420 to byte 0 and 1 as uint16
(bufset-u24 arr 0 420) ; write 420 to byte 0, 1 and 2 as uint24
(bufset-u32 arr 0 119) ; write 119 to byte 0 to 3 as uint32
(bufset-f32 arr 0 3.14) ; write 3.14 to byte 0 to 3 as float32 (IEEE 754)
(bufset-bit arr 14 1) ; Set bit 14 to 1 (note that this is a bitindex)
```

As with bufget big endian is the default byte order and little-endian can be passed as the last argument to use little-endian byte order instead.

---

#### bufcpy

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

Copy part of one array into another array.

```clj
(bufcpy arr1 ind1 arr2 ind2 len)
```

Copy len bytes from `arr2` starting at `ind2` to `arr1` starting at `ind1`. `len` will be truncated to ensure that nothing is read or written outside of the arrays.

---

#### free

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

Byte arrays will be de-allocated by the garbage collector on a regular basis, but can still use a lot of memory until then and large byte arrays cause a risk of running out of memory. It is possible to manually de-allocate the byte arrays when done with them by calling free

```clj
(free arr)
```

This will clear the allocated memory for `arr`.

**Note:**  
Strings in lispBM are treated the same as byte arrays, so all of the above can be done to the characters in strings too.

---

#### buf-resize

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(buf-resize arr delta-size opt-absolute-size opt-copy-symbol)
```

Change the length of array `arr` in bytes. A reference to `arr` is returned.
This extension can be used in two modes:
1. Relative: In this mode you set `delta-size` to the amount of bytes the length
   should be changed by. Negative numbers makes the array smaller.
   `opt-absolute-size` should not be passed in this mode (it is therefore an
   optional argument).
2. Absolute: In this mode you set `delta-size` to be `nil` and set
   `opt-absolute-size` to the new length in bytes.

Passing `nil` to `delta-size` while not passing any value for
`opt-absolute-size` will result in an `eval_error`.

You can optionally pass the symbol `'copy` to `opt-copy-symbol` to specify that
`arr` should be left unchanged and that a copy should instead be made. Don't
worry about the exact position of the argument, the only important part is that
`opt-copy-symbol` is last. So you can give a value for `opt-copy-symbol` even
when `opt-absolute-size` isn't passed. You can also for completeness pass the
symbol `'mut` to specify that the standard behaviour of modifying `arr` in place
should remain in effect.

When growing the length of the array a new range will be allocated and the old
data copied over. The new bytes will be initialised to zero. If the new length
of the array is smaller than the previous the allocated range will simply be
marked as smaller in an efficient manner which avoids any new allocations.

It is possible to shrink an array to a length of zero.

**Note:**  
The array will be resized in place. The returned reference to `arr` is just for
convenience. (Unless `opt-copy-symbol` is `'copy` of course.)

Example where we remove the terminating null byte from a string buffer:
```clj
(buf-resize "hello" -1)
> [104 101 108 108 111]
```

Example where we increase the length of `buf` to 5:
```clj
(def buf [1 2 3 4])
(buf-resize buf nil 5)
(bufset-u8 buf 4 5) ; we set it to avoid LBM printing the array as a string
(print buf)
> [1 2 3 4 5]
```

Example where we create a copy of `name` with the terminating null byte
removed.
```clj
(def name "name")
(def name-array (buf-resize name -1 'copy))

(print name)
> "name"
(print name-array)
> [110 97 109 101]
```

---

## Import Files

Import is a special command that is mostly handled by VESC Tool. When VESC Tool sees a line that imports a file it will open and read that file and attach it as binary data to the end of the uploaded code. VESC Tool also generates a table of the imported files that will be allocated as arrays and passed to LispBM at start and bound to bindings.

Every imported file behaves as a byte array that is read-only (so do not try to modify it). These byte arrays can be used as usual from the lisp code to, for example, load native libraries or to load more lisp code at runtime. As they are stored in flash in raw binary format there is significantly more space available than when using e.g. the array syntax. The lisp script and the imported files can use up to 120 KB together.

---

#### import

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(import filename binding)
```

Load filename as a byte array and bind it to binding. Note that import must be on its own line and that every line can only have one import.

**Example: Load native library**

```clj
(import "ws2812.bin" 'ws2812) ; Import the native ws2812 library. This creates the byte array ws2812 that can be used usual.
(load-native-lib ws2812); Load it to get the extensions it provides 
```

---

### Import Paths

Paths for import can be relative or absolute. Absolute paths are always looked up from the root of the file system, but relative paths need to be resolved. If the lisp-file is saved (e.g. there is a path at the bottom of the editor) paths are looked up relative to the location of that lisp file. If they are not found there they are looked up relative to the location where VESC Tool is started. If the file never has been saved (e.g. you just opened a new tab and started typing) the file path is unknown until save as is used, so only the path relative to VESC Tool is looked up then.

---

### Special Paths

It is also possible to import files from [VESC Packages](https://github.com/vedderb/vesc_pkg) using a special path syntax. If you want to import the lisp-file from a VESC Package you can use the format

```clj
(import "pkg@path/to/package.vescpkg" 'import-name)
```

This will import the lisp-file from that VESC Package. It is also possible to import the imports from a lisp-file in a VESC Package. This is a bit confusing as the package itself does not contain the paths anymore, so instead of referring to the path you have to refer to the label. Here is an example of that:

Suppose you have a VESC Package that has a lisp-file that imports a file like this:

```clj
(import "c_lib/ws2812/ws2812.bin" 'ws2812)
```

To import the file that is imported as ws2812 in the lisp-file of that vesc package you can use

```clj
(import "pkg::ws2812@path/to/package.vescpkg" 'ws2812)
```

This means import the import ws2812 from the lisp file in the VESC Package located at path/to/package.vescpkg.

### Import Official VESC Packages

There is a [github repository](https://github.com/vedderb/vesc_pkg) where official VESC Packages are located. All of the lisp-files and their imports from the packages in this repository can be imported using the special base path **://vesc_packages**. For example, to import the ws2812 import from lib_ws2812 the following import can be used:

```clj
(import "pkg::ws2812@://vesc_packages/lib_ws2812/ws2812.vescpkg" 'ws2812)
```

This should work for all VESC Packages in that repository. Most examples there use this pattern.

**NOTE:** The path above does not download the repository on demand, but relies on the cached local version. This cached version is not updated automatically, so you have to run the update manually when you want to get the latest version of the repository. To update the cache you can use the **Update Archive**-button from the VESC Packages-page in VESC Tool.

---

## Native Libraries

Native libraries can be used when more performance is needed. They can be created by compiling position-independent C code and loaded/unloaded with the functions below. More care has to be taken when developing native libraries as they have far less sandboxing than lispBM-code, so access to a SWD-programmer is recommended while developing them.

Up to 10 native libraries can be loaded simultaneously and the recommended way to configure and interact with them is by providing LispBM-extensions from them.

Currently the documentation for native libraries is limited, but there are some examples [in this diretory](c_libs/examples). The interface that can be used in native libraries can be found [in this file](c_libs/vesc_c_if.h).

### Features

Native libraries get a list of function pointers that can be used to interact with the rest of the VESC code. The following features are currently supported:

* Register LispBM-extensions.
* Os-functions like sleep, print, malloc, free, system time.
* Create one or more threads.
* GPIO-control (ST and abstract).
* The ST standard peripheral library can be used.
* Send and receive CAN-frames and control other VESCs over CAN-bus.
* Motor control using almost everything from mc_interface.

### Cleanup

Every time lispBM is restarted or when new code is uploaded the native libraries are closed and reloaded, so it is important to do proper cleanup in lib_info->stop_fun when resources such as threads are allocated.

---

#### load-native-lib

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(load-native-lib lib)
```

Load the native library lib. lib is a byte array with the compiled binary that is created after running make on the native library.

---

#### unload-native-lib

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(unload-native-lib lib)
```

Unload the native library lib. This is done automatically when lispBM is stopped or restarted, so there is no need to do it explicitly. This function is provided in case native libraries need to be explicitly loaded and unloaded while the same program is running.

---

### Native Library Example

This example creates an extension called ext-test that takes a number as an argument and returns the number multiplied by 3. The code for it can be found [in this diretory](c_libs/examples/extension).

```clj
; When running make in the example directory a file called example.lisp
; with this array is created.

(def example [
0x00 0x00 0x00 0x00 0x08 0xb5 0x07 0x4b 0x07 0x49 0x08 0x48 0x7b 0x44 0x79 0x44 0x1b 0x68 0x03 0x4b
0x78 0x44 0x1b 0x68 0x98 0x47 0x01 0x20 0x08 0xbd 0x00 0xbf 0x00 0xf8 0x00 0x10 0xf0 0xff 0xff 0xff
0x2b 0x00 0x00 0x00 0x18 0x00 0x00 0x00 0x65 0x78 0x74 0x2d 0x74 0x65 0x73 0x74 0x00 0x00 0x00 0x00
0x01 0x29 0x70 0xb5 0x05 0x46 0x09 0x4c 0x0d 0xd1 0xe3 0x6f 0x00 0x68 0x98 0x47 0x48 0xb1 0x63 0x6e
0x28 0x68 0x26 0x6c 0x98 0x47 0x33 0x46 0x00 0xeb 0x40 0x00 0xbd 0xe8 0x70 0x40 0x18 0x47 0xd4 0xf8
0x94 0x00 0x70 0xbd 0x00 0xf8 0x00 0x10
])

; The array can be loaded like this

(load-native-lib example)

; Now an extension called ext-test is available. Here we use it
; with the argument 4 and print the result

(print (ext-test 4)) ; Should print 12
```

---

## UAVCAN

---

#### uavcan-last-rawcmd

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(uavcan-last-rawcmd canInterface)
```

Get the last raw uavcan-command and its age. Returns a list where the first element is the value and the second element is the age. canInterface is the interface, which can be 1 or 2. Interface 2 is only valid if the hardware has dual CAN-buses. Example:

```clj
(print (ix (uavcan-last-rawcmd 1) 0)) ; Print the value
(print (ix (uavcan-last-rawcmd 1) 1)) ; Print the age in seconds
```

---

#### uavcan-last-rpmcmd

| Platforms | Firmware |
|---|---|
| ESC | 6.00+ |

```clj
(uavcan-last-rpmcmd canInterface)
```

Same as uavcan-last-rawcmd, but for the last rpm-command.

---

## LispBM

---

#### lbm-set-quota

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(lbm-set-quota quota)
```

Set how many evaluation steps to run each thread between context switches. Default is 50. A lower value will alter between threads more often, reducing latency between context switches at the cost of overall performance. The default value of 50 has relatively low performance overhead. Setting the quota to the lowest possible value of 1, meaning that each thread gets to run one step at a time, roughly halves the performance.

Lowering this value is useful if there are one or more timing-critical threads (that e.g. read encoders) that cannot wait too long between iterations.

---

#### lbm-set-gc-stack-size

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.05+ |

```clj
(lbm-set-gc-stack-size new-size)
```

Change the stack size for the garbage collector. If the GC stack is too small the program can crash during garbage collection and print a message stating that it ran out of GC stack. If that happens increasing the size from the default of 160 can help. Note that the GC stack is on LBM memory and increasing its size leaves less memory available for other things.

---

## Plotting

VESC Tool can be used for plotting data. The easiest way to plot a variable is to just select it in the binding tab while "Poll Status" is active and go to the binding plot below the editor. The plot commands in this section make plots that show up in the "Experiment Plot" tab below the editor and allow more control over the plotting.

The following code adds two graphs and plots them in the experiment plot:

```clj
(plot-init "x-name" "y-name")
(plot-add-graph "sin")
(plot-add-graph "cos")

(plot-set-graph 0)
(looprange i 0 200
    (plot-send-points (/ i 10.0) (sin (/ i 10.0)))
)

(plot-set-graph 1)
(looprange i 0 200
    (plot-send-points (/ i 10.0) (cos (/ i 10.0)))
)
```

---

#### plot-init

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(plot-init namex namey)
```

Start a new plot with namex as the x axis name and namey as the u axis name.

---

#### plot-add-graph

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(plot-add-graph name)
```

Add a graph to the current plot that will be called name. Every added graph gets a new index, starting from 0.

---

#### plot-set-graph

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(plot-set-graph ind)
```

Set graph index to which data points are sent.

---

#### plot-send-points

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(plot-send-points x y)
```

Send a xy-point to the selected graph in the plot.

---

## IO Boards

CAN-connected IO-boards can be interfaced using the functions in this section.

---

#### ioboard-get-adc

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(ioboard-get-adc can-id channel)
```

Read ADC-input channel from IO-board with can-id. Channel range: 1 to 8. If the IO-board with can-id is not present on the CAN-bus or if the channel is missing -1 will be returned.

---

#### ioboard-get-digital

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(ioboard-get-digital can-id channel)
```

Read  digital input channel from IO-board with can-id. Channel range: 1 to 64. If the IO-board with can-id is not present on the CAN-bus or if the channel is missing -1 will be returned.

---

#### ioboard-set-digital

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(ioboard-set-digital can-id channel state)
```

Write digital output channel to IO-board with can-id. State can be 1 or 0.

---

#### ioboard-set-pwm

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(ioboard-set-pwm can-id channel duty)
```

Write PWM-output channel to IO-board with can-id. The value duty can be 0.0 to 1.0.

---

## Logging

It is possible to log arbitrary data to log-devices such as the VESC Express, which can be connected on the CAN-bus.

Every log field consists of a keyword, name, unit, precision and some flags. Most of these fields control how VESC Tool renders the log. In the csv-file for the log they only show up in the first line as headers for each column.

VESC Tool will look for some special keywords to handle statistics and rendering on the map. If these keywords are missing the log can still be shown, but some of the functionality and statistics in VESC Tool will not be available. The following are the special keywords:

**t_day**  
Time of day in seconds. Used for statistics. This field is created by the log module when append-time is set to 1.

**gnss_h_acc**  
GNSS horizontal accuracy. Used for outlier filtering. This field is created by the log module when apped-gnss is set to 1.

**gnss_lat**  
GNSS latitude. Used for plotting traces on the map and for calculating statistics. This field is created by the log module when apped-gnss is set to 1.

**gnss_lon**  
GNSS longitude. Used for plotting traces on the map and for calculating statistics. This field is created by the log module when apped-gnss is set to 1.

**gnss_alt**  
GNSS altitude. Used for plotting traces on the map and for calculating statistics. This field is created by the log module when apped-gnss is set to 1.

**trip_vesc**  
VESC trip counter in meters. Used for calculating statistics.

**trip_vesc_abs**  
Absolute VESC trip counter in meters. Used for calculating statistics.

**trip_gnss**  
GNSS trip counter in meters. Used for calculating statistics. This field is automatically generated by VESC Tool from gnss_lat, gnss_lon and gnss_alt if it is missing.

**cnt_wh**  
Watt hour counter. Used for calculating statistics.

**cnt_wh_chg**  
Watt hour counter charging. Used for calculating statistics.

**cnt_ah**  
Amp hour counter. Used for calculating statistics.

**cnt_ah_chg**  
Amp hour counter charging. Used for calculating statistics.

**roll**  
IMU roll in degrees. Used for the IMU 3D plot.

**pitch**  
IMU pitch in degrees. Used for the IMU 3D plot.

**yaw**  
IMU yaw in degrees. Used for the IMU 3D plot.

**fault**  
Fault code. Converted to a fault string in the VESC Tool log analysis tool.

---

#### log-config-field

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(log-config-field
    can-id
    field-ind
    key
    name
    unit
    precision
    is-relative
    is-timestamp
)
```

Configure log field on log device. Parameters:

**can-id**  
ID on the CAN-bus. Setting the id to -1 will send the data to VESC Tool. On the express platform -2 will log to the local SD-card.

**field-ind**  
Field index in the log.

**key**  
Keyword string.

**name**  
Name string.

**unit**  
Unit string.

**precision**  
Number of decimals.

**is-relative**  
Relative fields are displayed relative to the start value of the log.

**is-timestamp**  
Timestamp fields are displayed with the format hh:mm:ss.

---

#### log-start

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(log-start
    can-id
    field-num
    rate-hz
    append-time
    apped-gnss
)
```

Start logging. Before starting to log all fields should be configured with [log-config-field](#log-config-field).

**can-id**  
ID on the CAN-bus. Setting the id to -1 will send the data to VESC Tool. On the express platform -2 will log to the local SD-card.

**field-num**  
Number of log fields.

**rate-hz**  
Log rate in Hz.

**append-time**  
If set to true the log device will append a timestamp to each sample.

**append-gnss**  
If set to true the log device will append a GNSS-position to each sample. This requires a GNSS-receiver on the log device and the log will not start until a valid position is available.

---

#### log-stop

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(log-stop can-id)
```

Stop logging data on log device with can-id. The id -1 refers to VESC Tool and id -2 refers to the local express.

---

#### log-send-f32

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(log-send-f32 can-id from-field-ind sample1 ... sampleN)
```

Send log samples to log device with can-id. This function takes 1 to 100 samples as arguments which will be applied to the log fields starting from from-field-ind. The samples can be numbers or lists of numbers. Setting the id to -1 will send the data to VESC Tool and setting id to -2 will store it locally on the express.

---

#### log-send-f64

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(log-send-f64 can-id from-field-ind sample1 ... sampleN)
```

Same as [log-send-f32](#log-send-f32) but uses 64-bit values for higher precision and takes up to 50 samples. Useful for e.g. gnss-positions where 32-bit floats do not give enough precision due to the size of the earth.

---

## GNSS

If a GNSS-receiver such as the VESC Express is connected on the CAN-bus, the position, speed, time and precision data from it can be read from LBM.

---

#### gnss-lat-lon

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(gnss-lat-lon)
```

Returns the latitude and longitude of the position as a list with two elements.

---

#### gnss-height

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(gnss-height)
```

Returns the height of the position in meters.

---

#### gnss-speed

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(gnss-speed)
```

Returns the speed on meters per second.

---

#### gnss-hdop

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(gnss-hdop)
```

Returns the hdop-value of the position. Lower values mean that the precision is better.

---

#### gnss-date-time

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(gnss-date-time)
```

Returns date and time of the last position sample as a list with the format (year month day hours minutes seconds milliseconds).

---

#### gnss-age

| Platforms | Firmware |
|---|---|
| ESC, Express | 6.00+ |

```clj
(gnss-age)
```

Returns the age of the last gnss-sample in seconds.

---

#### ublox-init

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(ublox-init optRateMs optUartNum optPinRx optPinTx)
```

Re-initializes the ublox gnss-module. Returns true on success and nil on failure. Nil most likely means that something is wrong with the connection.

The optional argument optRateMs can be used to set the navigation rate in milliseconds. By default 500 ms us used. Not any navigation rate is possible, it depends on the ublox module in use. Common rates that can work are 100, 200, 500, 1000 and 2000 ms.

The optional arguments optUartNum, optPinRx and optPinTx can be used to specify the UART peripheral and pins. optUartNum can be 0 or 1 and the pins can be any valid ESP-pins.

---

## ESP-NOW

The VESC Express has full support for ESP-NOW. It can be used in any combination of bluetooth and wifi, the only limitation is that it must use the same channel as the wifi. That is mainly an issue in station mode as there is no way to control the channel that the access point the express connects to uses.

---

#### esp-now-start

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(esp-now-start)
```

Start ESP-NOW. This must be run before further ESP-NOW operations.

---

#### esp-now-add-peer

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(esp-now-add-peer peer)
```

Add peer. The argument is a list with the mac address of the peer to add. This must be run before esp-now-send as it only is possible to send data to peers that have been added. Example:

```clj
(esp-now-add-peer '(255 255 255 255 255 255)) ; Add broadcast address as peer
```

---

#### esp-now-del-peer

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(esp-now-del-peer peer)
```

Delete peer. There is a limit to how many peers can be added, so if adding another peer does not work old peers can be deleted to free up resources.

```clj
(esp-now-del-peer '(255 255 255 255 255 255)) ; Remove broadcast peer
```

---

#### esp-now-send

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(esp-now-send peer data)
```

Send the array data to peer. The peer argument is a list with the mac address of the peer to send to. If peer is not the broadcast address this function will make a few retries and returns true if the packet arrived at the receiver and false if it was not received. For broadcast only one try is made and it should always return true.

Example:

```clj
(esp-now-send '(255 255 255 255 255 255) "Hello!") ; Broadcast the string "Hello!"
```

---

#### esp-now-recv

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(esp-now-recv optTimeout)
```

Block current thread until esp-now data arrives. The optional argument optTimeout can be used to specify a timeout in seconds.

If a timeout occurs the symbol timeout will be returned, otherwise a list with the following format will be returned:

```clj
(src-mac-addr dest-mac-addr payload-array rssi-db)
```

Usage example:

```clj
(esp-now-start)
(print (esp-now-recv))
> ((112 4 29 15 194 105) (16 145 168 52 203 121) "Test" -40)
```

---

#### get-mac-addr

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(get-mac-addr)
```

Get the mac address of the device as a list. This address is what peer refers to in the functions above. Example:

```clj
(get-mac-addr)
> (112 4 29 15 194 105)
```

---

#### wifi-get-chan

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(wifi-get-chan)
```

Get the current wifi-channel. For ESP-NOW to work the communicating devices need to be on the same wifi-channel. By default the channel is 1, but when station mode is used on the wifi the channel will be the same as the access point and cannot be changed.

---

#### wifi-set-chan

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(wifi-set-chan channel)
```

Set the current wifi-channel. This function cannot be used when wifi is connected.

---

#### wifi-get-bw

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(wifi-get-bw)
```

Get current wifi bandwidth in MHz.

---

#### wifi-set-bw

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(wifi-set-bw)
```

Set wifi bandwidth in MHz. This function is experimental and should only be used if you know what you are doing.

---

#### wifi-stop

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(wifi-stop)
```

Stop wifi-driver. Reduces power consumption.

---

#### wifi-start

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(wifi-start)
```

Start wifi-driver. Re-connects to networks that were connected before. Can be used to restart wifi after sleep-light.

---

### Receiving Data

Events can be used to receive ESP-NOW data. This is best described with an example:

```clj
; Here src is the mac-address of the sender, des is the mac-address
; of the receiver and data is an array with the sent data. If des is
; the broadcast address (255 255 255 255 255 255) it means that this
; was a broadcast packet.

(defun proc-data (src des data rssi)
    (print (list src des data rssi))
)

(defun event-handler ()
    (loopwhile t
        (recv
            ((event-esp-now-rx (? src) (? des) (? data) (? rssi)) (proc-data src des data rssi))
            (_ nil)
)))

(event-register-handler (spawn event-handler))
(event-enable 'event-esp-now-rx)
```

NOTE: The RSSI was added in firmware 6.05 and should be left out in earlier firmwares.

---

## File System (SD Card)

When a SD-card is present in the VESC Express files can be listed, read, written and removed. Directories can also be created and removed.

---

#### f-connect

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-connect pin-mosi pin-miso pin-sck pin-cs optSpiSpeed)
```

Connect SD-card on pin-mosi, pin-miso, pin-sck and pin-cs. The optional argument optSpiSpeed can be used to specify the SPI speed (default 20000 Hz). Returns true on success, nil otherwise.

---

#### f-connect-nand

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-connect-nand pin-mosi pin-miso pin-sck pin-cs optSpiSpeed)
```

Connect NAND-flash memory on pin-mosi, pin-miso, pin-sck and pin-cs. The optional argument optSpiSpeed can be used to specify the SPI speed (default 20000 Hz). Returns true on success, nil otherwise.

---

#### f-disconnect

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-disconnect)
```

Disconnect SD-card or NAND-Flash.

---

#### f-open

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-open path mode)
```

Open a file in open-mode mode (r, w or rw). Returns a number that can be used as a handle to the file on success or nil if the file could not be opened. Example:

```clj
(def f (f-open "test.txt" "w")) ; Open test.txt in write-only mode and store the handle as f.
```

---

#### f-close

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-close file)
```

Close file. The argument file is the handle returned by f-open.

---

#### f-read

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-read file size)
```

Read up to size bytes from file. Returns an array with the read data. Successive calls to f-read will move the file position forwards. If the returned array is smaller than the size-argument it means that the end of the file was reached. When calling read after reaching the end if the file nil is returned.

---

#### f-readline

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-readline file maxlen)
```

Read one line from file. At most maxlen bytes will be read. If maxlen is set too high this function will fail as it needs to pre-allocate enough memory to fit maxlen. This function advances the file position, so successive calls to it can be used to read the file line-by-line. When the end of the file is reached nil is returned.

---

#### f-write

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-write file buf)
```

Write array buf to file. This will advance the file position by the size of buf. 

---

#### f-tell

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-tell file)
```

Get the current position in the file.

---

#### f-seek

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-seek file pos)
```

Seek to position pos in file. If pos is a negative number the seek is done from the end of the file.

---

#### f-mkdir

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-mkdir path)
```

Make directory on path.

---

#### f-rm

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-rm path)
```

Remove path recursively. If path is a file just the file is removed and if it is a directory that directory and all its content will be removed recursively.

---

#### f-ls

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-ls path)
```

List all files and directories in path. Returns a list with the entries; each entry is a list where the first element is the name, the second element is true for directories and nil for files and the third element is the size. For directories the size says how many entries that directory has and for files it says what size the file has in bytes.

Example:

```clj
(f-ls "")
> ("testsize.bin" nil 100) ("test.txt" nil 7) ("old_logs" t 47))
```

---

#### f-size

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-size file)
```

Get the size of a file in bytes. File can be a path (e.g. "test.txt") or a file pointer opened with f-open.

---

#### f-rename

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-rename oldname newname)
```

Rename file (same as moving a file). Returns true on success, nil otherwise.

---

#### f-fatinfo

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(f-fatinfo)
```

Returns a list where the first element is the free space on the file system and the second element is the total size of the file system. Unit: MB. Example:

```clj
(f-fatinfo)
> (30298 30417)
```

---

## Firmware Update

The firmware can be updated locally and on CAN-devices. This requires that the firmware-file is pre-processed using VESC Tool with the cli-command --packFirmware.

---

#### fw-erase

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(fw-erase size optCanId)
```

Erase firmware-buffer. This is required before writing the firmware. Returns true on success or nil/timeout on failure. If the optional argument optCanId is omitted or set to -1 the command is performed locally, otherwise it is performed on the CAN-device with id optCanId.

---

#### fw-write

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(fw-write offset data optCanId)
```

Write data to firmware-buffer at offset. Returns true on success or nil/timeout on failure. If the optional argument optCanId is omitted or set to -1 the command is performed locally, otherwise it is performed on the CAN-device with id optCanId.

---

#### fw-reboot

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(fw-reboot optCanId)
```

Reboot and attempt to load the new firmware from the firmware-buffer using the bootloader. This function always returns true as there is no easy way to get the response from the bootloader. If the optional argument optCanId is omitted or set to -1 the command is performed locally, otherwise it is performed on the CAN-device with id optCanId.

---

#### fw-data

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(fw-data optOffset optLen)
```

Get the firmware data partition as an array. The optional argument optOffset can be used to specify an offset in the partition and the optional argument optLen can be used to specify the length. By default the offset is 0 and the length is the entire firmware buffer (around 1.5 MB).

Note that this array is read-only, so do not try to write directly to it! It is however possible to write to it using fw-write-raw if fw-erase has been performed first. After the erase each byte can be written to once, in any order.

Example:
```clj
; An array with the first 10 bytes of the firmware buffer
(def fwd (fw-data 0 10))

; Erase at least 100 bytes
(fw-erase 100)

; Write 1 to 10 to the beginning of the firmware buffer.
(fw-write-raw 0 [1 2 3 4 5 6 7 8 9 10])

; Print the array
(print fwd)
```

---

#### fw-write-raw

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(fw-write-raw offset data)
```

Write data to firmware-buffer at offset. Returns true on success or nil on failure. Unlike fw-write, this function writes directly to the firmware buffer without an offset shift and it supports writing more than 500 bytes at a time.

---

## Lisp and Qml Upload

Lisp and QML-scripts can be updated locally and on CAN-devices. This requires that the file is pre-processed using VESC Tool with the cli-command --packLisp and --packQml.

---

#### lbm-erase

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(lbm-erase optCanId)
```

Erase lisp-code. This is required before writing new code. Returns true on success or nil/timeout on failure. If the optional argument optCanId is omitted or set to -1 the command is performed locally, otherwise it is performed on the CAN-device with id optCanId.

---

#### qml-erase

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(qml-erase optCanId)
```

Erase qml-code. This is required before writing new code. Returns true on success or nil/timeout on failure. If the optional argument optCanId is omitted or set to -1 the command is performed locally, otherwise it is performed on the CAN-device with id optCanId.

---

#### lbm-write

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(lbm-write offset data optCanId)
```

Write data to lbm-buffer at offset. Returns true on success or nil/timeout on failure. If the optional argument optCanId is omitted or set to -1 the command is performed locally, otherwise it is performed on the CAN-device with id optCanId.

---

#### qml-write

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(qml-write offset data optCanId)
```

Write data to qml-buffer at offset. Returns true on success or nil/timeout on failure. If the optional argument optCanId is omitted or set to -1 the command is performed locally, otherwise it is performed on the CAN-device with id optCanId.

---

#### lbm-run

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(lbm-run running optCanId)
```

Run or stop the lbm-code (run if running is 1, stop otherwise). Returns true on success or nil/timeout on failure. If the optional argument optCanId is omitted or set to -1 the command is performed locally, otherwise it is performed on the CAN-device with id optCanId.

---

## RGB LED (e.g. WS2812)

The express can use the remote peripheral to drive addressable LEDs on any pin. The LED on the DevKitM-1 is actually and addressable LED connected to pin 8, so this driver is required to use it.

---

#### rgbled-init

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(rgbled-init pin)
```

Initialize the rgbled-driver on pin. If the driver already is initialized it will be de-initialized first.

---

#### rgbled-deinit

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(rgbled-deinit)
```

De-initialize the rgbled-driver and release the resources it used. If data is currently sent to the LEDs this function will block until that is done.

---

#### rgbled-buffer

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(rgbled-buffer num-leds optLedType optGammaCorr)
```

Creates an LED-buffer for num-leds leds. The optional argument optLedType specifies the type of LEDs. If it is omitted type 0 (GRB) is used. The available types are:

| Number | LED Type |
|---|---|
| 0 | GRB |
| 1 | RGB |
| 2 | GRBW |
| 3 | RGBW |
| 4 | WRGB |

The optional argument optGammaCorr can be set to enable gamma correction for this LED-buffer. Gamma correction makes the brightness non-linear matching the response of human eyes. Generally that makes colors look better and it is something that all monitors do. The downside is that fewer distinct colors are available as not all bits can be used.

Example:

```clj
; Create a buffer for 10 GRBW-LEDs
(def strip1 (rgbled-buffer 10 2))

; Create a buffer for 10 GRB-LEDs with gamma-correction enabled
(def strip2 (rgbled-buffer 10 0 1))
```

---

#### rgbled-color

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(rgbled-color buffer led-num color optBrightness)
```

Set LED led-num to color in buffer. The color is a number in WRGB8888 format, alternatively a list of numbers in WRGB8888 format. When a list of numbers is used all colors after led-num will be set to the corresponding colors in the list. When the white color is used the type must be u32 as all 32 bits are needed then. Buffer must be created with rgbled-buffer. Example:

```clj
; Create led-buffer and bind it to strip1
(def strip1 (rgbled-buffer 10 2))

; Set the first LED to red
(rgbled-color strip1 0 0x00FF0000u32)

; Set the second LED to white
(rgbled-color strip1 1 0xFF000000u32)

; Set the second LED to white with 30 % brightness
(rgbled-color strip1 1 0xFF000000u32 0.3)

; Set LED 4 to white, LED 5 to red and LED 6 to green, all at 40 % brightness
(rgbled-color strip1 4 '(0xFF000000u32 0x00FF0000u32 0x0000FF00u32) 0.4)
```

Note: This function only updates the color in the buffer. To show the new color the function rgbled-update must be used.

---

#### rgbled-update

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(rgbled-update buffer)
```

Send buffer to the LEDs.

### Multiple LED-strips

It is possible to use multiple pins to drive different LED-strips. Even different types of LEDs can be used on the different pins. To do that more than one buffer can be used and rgbled-init can be used to move the driver to a different pin. Example:

```clj
; In this example a strip with 10 GRBW-LEDs is connected to
; pin 20 and another strip with 10 GRB-LEDs is conneced to pin 21.

; Buffer for 10 GRBW-LEDs
(def strip1 (rgbled-buffer 10 2))
; Buffer for 10 GRB-LEDs
(def strip2 (rgbled-buffer 10 0))

; Write some colors to the buffers of the
; two strips

(rgbled-color strip1 0 0x330000)
(rgbled-color strip1 1 0x22000000u32)
(rgbled-color strip1 9 0x003311)

(rgbled-color strip2 0 0x330000)
(rgbled-color strip2 1 0x22000000u32)
(rgbled-color strip2 6 0x330011)

; Start the driver on pin 20 and send the first buffer
(rgbled-init 20)
(rgbled-update strip1)

; Now move the driver to pin 21 and send the other buffer. This
; function will block until the previous update has finished.
(rgbled-init 21)
(rgbled-update strip2)
```

---

## Color Manipulation

The following functions are useful for manipulating colors and lists of colors. The color format used here is RGB888 (or WRGB8888), which is compatible with both the display and the RGB LED extensions. This format is just a u32-number and it is also compatible with common hexadecimal color codes found in most color pickers and HTML color tables.

---

#### color-make

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(color-make red green blue optWhite)
```

Make color from individual color components. The components can be expressed in 0 to 255 or as floating point 0.0 to 1.0 (in any combination). optWhite is an optional argument for the white component, which often is used in RGBW LED-strips. Example:

```clj
(color-make 37 150 90) ; Creates the color 0x2596be
(color-make 0.146 0.59 0.747) ; Creates the color 0x2596be
```

---

#### color-split

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(color-split color optMode)
```

Split color into a list of components. The optional argument optMode can be used to define how the color is split. The options are:

| Number | Mode |
|---|---|
| 0 | (red green blue) |
| 1 | (red green blue white) |
| 2 | (red-float green-float blue-float) |
| 3 | (red-float green-float blue-float white-float) |

Example:

```clj
(color-split 0x123456)
> (18 52 86)

(color-split 0x123456 1)
> (18 52 86 0)

(color-split 0x50123456u32 1)
> (18 52 86 80)

(color-split 0x123456 2)
> (0.070588f32 0.203922f32 0.337255f32)

(color-split 0x123456 3)
> (0.070588f32 0.203922f32 0.337255f32 0.000000f32)

(color-split (color-make 123 250 99))
> (123 250 99)
```

---

#### color-mix

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(color-mix color1 color2 ratio)
```

Mix color1 with color2 with mix-ratio ratio. Ratio has a range of 0.0 to 1.0. Ratio 0.0 will result in color1 and ratio 1.0 will result in color2. The argument color1 can be a single color or a list of colors. If it is a list of colors a list with the mixed colors will be returned.

Example:

```clj
; Helper function to print colors as components
(defun print-color (c)
    (if (eq (type-of c) type-list)
        (print (map (fn (x) (color-split x 1)) c))
        (print (color-split c 1))
    )
)

(print-color (color-mix (color-make 123 250 99) (color-make 12 25 9) 0.3))
> (89 182 71 0)

(print-color (color-mix '(0x224488 0x111111 0x222222) 0x446699 0.5))
> ((51 85 144 0) (42 59 85 0) (51 68 93 0))
```

---

#### color-add

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(color-add color1 color2)
```

Add color2 to color1. The components are truncated at 255. As with color-mix, the argument color1 can be a single color or a list with colors. Example:

```clj
; Helper function to print colors as components
(defun print-color (c)
    (if (eq (type-of c) type-list)
        (print (map (fn (x) (color-split x 1)) c))
        (print (color-split c 1))
    )
)

(print-color (color-add (color-make 123 250 99) (color-make 12 25 9)))
> (135 255 108 0)

(print-color (color-add '(0x224488 0x111111 0x222222) 0x446699))
> ((102 170 255 0) (85 119 170 0) (102 136 187 0))
```

#### color-sub

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(color-sub color1 color2)
```

Subtract color2 from color1. The components are truncated at 0. As with color-mix, the argument color1 can be a single color or a list with colors.

---

#### color-scale

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(color-sub color factor)
```

Scale color with factor. The result is truncated between 0 to 255. As with color-mix, the argument color can be a single color or a list with colors. The argument factor is a scaling factor. Example:

```clj
; Helper function to print colors as components
(defun print-color (c)
    (if (eq (type-of c) type-list)
        (print (map (fn (x) (color-split x 1)) c))
        (print (color-split c 1))
    )
)

(print-color (color-scale (color-make 123 250 99) 2.0))
> (246 255 198 0)

(print-color (color-scale '(0x224488 0x111111 0x222222) 0.6))
> ((20 40 81 0) (10 10 10 0) (20 20 20 0))
```

---

## File (De-)Compression

---

#### unzip

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(unzip input fileInZip optOutputFile)
```

Unzip input. Input can be a byte array or a file handle on the SD-card. FileInZip is the file in the zip-archive; if it is a string the filename will be used and if it is an integer its sequence number will be used. The optional argument optOutputFile can be used to specify a file to write the output to - if it is not provided the decompressed file is returned as a byte array.

**Note:**   
When optOutputFile is used the firmware buffer is used to temporally store the output file in order to make the operation as fast as possible. That means if anything is in the firmware buffer it will be destroyed. It also means that the maximum size of the output file is 1.5 MB.

Examples:

```clj
; Unzip the first file of the archive test.zip (on the SD-card) to the byte array uz 
(def f (f-open "test.zip" "r"))
(def uz (unzip f 0))
(f-close f)
```

```clj
; Unzip the file hello.txt of the archive test.zip (on the SD-card) to the byte array uz 
(def f (f-open "test.zip" "r"))
(def uz (unzip f "hello.txt"))
(f-close f)
```

```clj
; Unzip the first file of the imported archive test.zip to the byte array uz 
(import "test.zip" 'test)
(def uz (unzip test 0))
```

```clj
; Unzip the file hello.txt of the imported archive test.zip to the byte array uz 
(import "test.zip" 'test)
(def uz (unzip test "hello.txt"))
```

```clj
; Unzip the file hello.txt of the archive test.zip (on the SD-card) to the file hello.txt on the SD-card 
(def f (f-open "test.zip" "r"))
(def f-out (f-open "hello.txt" "w"))
(unzip f "hello.txt" f-out)
(f-close f)
(f-close f-out)
```

```clj
; Unzip the file hello.txt of the archive imported test.zip to the file hello.txt on the SD-card 
(import "test.zip" 'test)
(def f-out (f-open "hello.txt" "w"))
(unzip test "hello.txt" f-out)
(f-close f-out)
```

---

#### zip-ls

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(zip-ls input)
```

List all files in the archive input. Input can be a byte array or a file handle on the SD-card. Returns a list with the entries; each entry is a list where the first element is the name and the second element is the size when uncompressed.

Example:

```clj
; List all files in the archive test.zip on the SD-card
(def f (f-open "test.zip" "r"))
(print (zip-ls f))
(f-close f)

> (("test.txt" 7) ("fw.bin" 291871))
```

```clj
; List all files in the imported archive test.zip
(import "test.zip" 'test)
(print (zip-ls test))

> (("test.txt" 7) ("fw.bin" 291871))
```

---

## Crypto

---

#### aes-ctr-crypt

| Platforms | Firmware |
|---|---|
| Express | 6.06+ |

```clj
(aes-ctr-crypt key counter data start-offset length)
```

Performs in-place AES-CTR (128/192/256) encryption/decryption. Counter will also be updated.

Example:

```clj
(def data [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]) 
(def key [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]) 
(def counter [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15])
(def start-offset 5)
(def len 6)
; Encrypt data
(aes-ctr-crypt key counter data start-offset len)
(print data)
> [0 1 2 3 4 15 146 12 189 72 100 11 12 13 14 15]
(print counter)
> [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 16]
; Reset counter
(setq counter [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15])
; Decrypt data
(aes-ctr-crypt key counter data start-offset len)
(print data)
> [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]
```

---

## Sleep Modes

---

#### sleep-deep

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(sleep-deep time)
```

Put the CPU in deep sleep mode for time seconds. If time is negative the CPU will sleep forever, or until a wakeup pin triggers a wakeup. Waking up from sleep-deep results in a complete reset.

---

#### sleep-light

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(sleep-light time)
```

Put the CPU in light sleep mode for time seconds. Waking up will return to the point where sleep-light was called without a complete reset, but the power draw is much higher than for sleep-deep. Note that wifi and bluetooth are disabled when using this function, so they have to be enabled again when waking up.

---

#### sleep-config-wakeup-pin

| Platforms | Firmware |
|---|---|
| Express | 6.02+ |

```clj
(sleep-config-wakeup-pin pin state)
```

Configure pin to wake up the CPU from sleep mode. The available pins are 0 to 5 and state can be 0 or 1. 0 means that a low state wakes up the CPU and 1 means that a high state wakes up the CPU.

---

#### rtc-data

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(rtc-data)
```

Returns a 4k byte array from RTC memory that can be used as a general purpose array. What is special about it is that it is retained in deep sleep mode, which is useful for storing state while the CPU is in deep sleep mode.

---

## Connection Checks

---

#### connected-wifi

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(connected-wifi)
```

Check if any client (e.g. VESC Tool) is connected over wifi. Returns true when connected, nil otherwise.

---

#### connected-hub

| Platforms | Firmware |
|---|---|
| Express | 6.06+ |

```clj
(connected-hub)
```

Check if we are connected to the TCP hub. Returns true when connected, nil otherwise. Notice that this does not tell if someone is connected to us using the hub, it only tells that we are connected to the hub and that others can connect to us.

---

#### connected-ble

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(connected-ble)
```

Check if any client (e.g. VESC Tool) is connected over ble. Returns true when connected, nil otherwise.

---

#### connected-usb

| Platforms | Firmware |
|---|---|
| Express | 6.05+ |

```clj
(connected-usb)
```

Check if any client (e.g. VESC Tool) is connected over usb. Returns true when connected, nil otherwise.

---

## How to update

To update from remote repository:

```bash
git remote add lispBM git@github.com:svenssonjoel/lispBM.git
git subtree pull --squash --prefix=lispBM/lispBM/ lispBM master
```

The first command might fail if it already is added, but the second one should still work. If there are uncomitted changes you can run **git stash** before the commands and **git stash pop** after them.
