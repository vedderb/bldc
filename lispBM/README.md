# LispBM

This is the VESC-integration of [lispBM](https://github.com/svenssonjoel/lispBM) written by Joel Svensson. It allows the VESC to run lisp-programs in a sandboxed environment.

### Feature Overview

* Development and testing in VESC Tool with variable live monitoring and plotting as well as CPU and memory monitoring.
* There is a REPL in VESC Tool where code can be executed and tested live. You even have full access to the functions and bindings in the program you have uploaded.
* Sandboxed environment, meaning that the Lisp code (hopefully) cannot freeze or crash the rest of the VESC code when it gets stuck or runs out of heap or stack memory.
* The application runs on the VESC itself without the need for having VESC Tool connected and is stored in flash memory.
* When a lisp-application is written to the VESC it is automatically started on each boot.

## Language Reference

[LispBM Language Reference](lispBM/doc/lbmref.md)

## Programming Manual

This is the work-in-progress programming manual for LispBM. Note that the examples in the manual use the REPL quite a lot. All of them also work in the VESC Tool REPL (which is below the console below the code editor) when you are connected to a VESC and will be executed on the VESC itself. The results of the commands will be printed in the console. From the VESC Tool REPL you also have access to all functions and variables in the program that you have uploaded to the VESC.

[Chapter 1: Introduction to programming in LispBM](lispBM/doc/manual/ch1_introduction.md)  
[Chapter 2: List Processing](lispBM/doc/manual/ch2_list_processing.md)  
[Chapter 3: Concurrency](lispBM/doc/manual/ch3_concurrency.md)

## VESC-Specific Commands and Extensions

The VESC-specific extensions are documented below. If you are reading this on GitHub there is an index in the upper left corner that can be used to navigate this document. It follows you as you scroll around and also includes a search function that filters all the titles in this document.

Note that VESC Tool includes a collection of examples that can be used as a starting point for using LispBM on the VESC.

### Various Commands

#### print

```clj
(print arg1 ... argN)
```

Print to the VESC Tool Lisp console. Example:

```clj
(print "Hello World")
```

Should work for all types.

#### timeout-reset

```clj
(timeout-reset)
```

Reset the timeout that stops the motor. This has to be run on at least every second to keep the motor running. The timeout time can be configured in App Settings->General. The [Motor Set Commands](#motor-set-commands) will also reset the timeout when they are called.

#### get-ppm

```clj
(get-ppm)
```

Read the decoded value on the PPM input, range -1.0 to 1.0. If the PPM-decoder is not running it will be initialized and the PPM-pin will be reconfigured, so make sure that nothing else is using that pin. Example:

```clj
(print (str-from-n (get-ppm) "PPM Value: %.2f"))
```

Note that control type can be set to Off in the PPM app to get the input without running the motor automatically, which is useful when running the motor from lisp.

#### get-ppm-age

```clj
(get-ppm-age)
```

Get the age of the last PPM update in seconds. Can be used to determine if there is any valid PPM-signal.

#### get-encoder

```clj
(get-encoder)
```

Get angle from selected encoder in degrees.

#### get-encoder-error-rate

```clj
(get-encoder-error-rate)
```

Returns the error rate for the selected encoder, range 0.0 to 1.0. If the selected encoder does not provide any error rate -1.0 is returned. If the selected encoder has multiple error rates the highest one is returned.

#### set-servo

```clj
(set-servo value)
```

Set servo output to value. Range 0 to 1. Note that the servo output has to be enabled in App Settings -> General.

#### get-vin

```clj
(get-vin)
```

Get input voltage.

#### select-motor

```clj
(select-motor motor)
```

Select which motor to control on dual-motor hardware. Options are 1 for motor 1 and 2 for motor 2.

#### get-selected-motor

```clj
(get-selected-motor)
```

Get currently selected motor on dual motor hardware.

#### get-bms-val

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
(get-bms-val 'bms-temp-cell-max) ; Maximum cell temperature
(get-bms-val 'bms-soc) ; State of charge (0.0 to 1.0)
(get-bms-val 'bms-can-id) ; CAN ID of BMS
(get-bms-val 'bms-ah-cnt-chg-total) ; Total ah charged
(get-bms-val 'bms-wh-cnt-chg-total) ; Total wh charged
(get-bms-val 'bms-ah-cnt-dis-total) ; Total ah discharged
(get-bms-val 'bms-wh-cnt-dis-total) ; Total wh discharged
(get-bms-val 'bms-msg-age) ; Age of last message from BMS in seconds
```

#### set-bms-val

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

#### send-bms-can

```clj
(send-bms-can)
```

Send BMS-values on CAN-bus. This his useful if a custom BMS-driver is implemented using [set-bms-val](#set-bms-val) in order to make devices on the CAN-bus aware of the BMS-state using the VESC protocol.

#### get-adc

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

#### override-temp-motor

```clj
(override-temp-motor temp)
```

Override motor temperature. This can be used to implement custom motor temperature sensors if the sensor you have is not supported. Note: Motor Temperature Sensor Type has to be set to Disabled in General -> Advanced for the override to work.

#### get-adc-decoded

```clj
(get-adc-decoded ch)
```

Get decoded ADC value on channel ch (0 or 1). Decoded means that the voltage is mapped to the range 0 to 1 according to the configuration in the ADC app. Note that the ADC app must be running for this function to work. No throttle curve is applied to this value, but you can use the [throttle-curve](#throttle-curve) function to apply one if desired.

#### systime

```clj
(systime)
```

Get system time in ticks since boot. Every tick is 0.1 ms.

#### secs-since

```clj
(secs-since timestamp)
```

Get seconds elapsed since systime timestamp.

#### set-aux

```clj
(set-aux ch state)
```

Set AUX output ch (1 or 2) to state. Example:

```clj
(set-aux 1 1) ; Set AUX1 to ON.
```

Note: The AUX output mode must be set to Unused in Motor Settings->General->Advanced. Otherwise the firmware will change the AUX state directly after it is set using this function.

#### get-imu-rpy
```clj
(get-imu-rpy)
```
Get roll, pitch and yaw from the IMU in radians.

The function (ix list ind) can be used to get an element from the list. Example:
```clj
(ix (get-imu-rpy) 0) ; Get roll (index 0)
```

#### get-imu-quat
```clj
(get-imu-quat)
```
Get a list of quaternions from the IMU (q0, q1, q2 and q3).

#### get-imu-acc
```clj
(get-imu-acc)
```
Get a list of the x, y and z acceleration from the IMU in G.

#### get-imu-gyro
```clj
(get-imu-gyro)
```
Get a list of the x, y and z angular rate from the IMU in degrees/s.

#### get-imu-mag
```clj
(get-imu-mag)
```
Get a list of the x, y and z magnetic field strength from the IMU in uT. Note that most IMUs do not have a magnetometer.

#### get-imu-acc-derot

Same as get-imu-acc, but derotates the result first. This means that the acceleration will be relative to the horizon and not the IMU chip.

#### get-imu-gyro-derot

Same as get-imu-gyro, but derotates the result first. This means that the angular rates will be relative to the horizon and not the IMU chip.

#### send-data
```clj
(send-data dataList)
```
Send a list of custom app data to VESC Tool. This can be read from a Qml script for example.

Example of sending the numbers 1, 2, 3 and 4:

```clj
(send-data (list 1 2 3 4))
```

*dataList* can be a list or a [byte array](#byte-arrays).

#### sleep

```clj
(sleep seconds)
```

Sleep for *seconds* seconds. Example:

```clj
(sleep 0.05) ; Sleep for 0.05 seconds (50 ms)
```

#### get-remote-state

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

#### sysinfo

```clj
(sysinfo param)
```

Read system info parameter param. Example:

```clj
(sysinfo 'hw-name) ; Hardware name, e.g 60
(sysinfo 'fw-ver) ; Firmware version as list (Major Minor BetaNum)
(sysinfo 'has-phase-filters) ; t if hardware has phase filters
(sysinfo 'uuid) ; STM32 UUID
(sysinfo 'runtime) ; Total runtime in seconds
(sysinfo 'git-branch) ; Git branch name
(sysinfo 'git-hash) ; Git hash of current commit
(sysinfo 'compiler) ; GCC version, e.g. 7.3.1
```

#### stats

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

#### stats-reset

```clj
(stats-reset)
```

Reset stat counters to 0.

### App Override Commands

Several app-inputs can be detached from the external interfaces and overridden from lisp. This is useful to take advantage of existing throttle curves and control modes from the apps while providing a custom input source.

**Note:** Detach does *not* mean that the app output is disabled, it means that you can provide the input for the app instead of having it read the external peripheral. So if you e.g. detach the app and override the input with 0 the app will keep sending the corresponding command to the motor as usual, even if that command is a stop command. If you want to disable the app output you can have a look at [app-disable-output](#app-disable-output).

#### app-adc-detach

```clj
(app-adc-detach mode state)
; Where
; mode : Select periperial to detach from APP
;        - 0 All peripherial attached (no second argument)
;        - 1 ADC1/2
;        - 2 Buttons
;        - 3 ADC1/2 + Buttons
; state : Only when mode 1/2/3/4 - 1 detaches periperial from APP, 0 attaches peripherial to APP 
```

Detaches a peripherial from the APP ADC

#### app-adc-override

```clj
(app-adc-override mode value)
; Where
; mode : Select periperial to override
;        - 0 ADC1
;        - 1 ADC2
;        - 2 Reverse button
;        - 3 Cruise control button
; val : 0.0 to 1.0 (button pressed is > 0.0)
```

Sets the override value

#### app-ppm-detach

```clj
(app-ppm-detach state)
```

Detaches the decoded ppm signal from APP PPM. 1 means detach, 0 means attach.

#### app-ppm-override

```clj
(app-ppm-override value)
```

Sets the override value. Range -1.0 to 1.0.

#### set-remote-state

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

#### app-disable-output

```clj
(app-disable-output ms)
```

Disable app output for ms milliseconds. 0 means enable now and -1 means disable forever. This can be used to override the control of apps temporarily.

#### app-is-output-disabled

```clj
(app-is-output-disabled)
```

Check if app output is disabled. VESC Tool will disable app output during some detection routines, so when running a custom control script it might be useful to check this and disable the output when this value is true.

#### app-pas-get-rpm

```clj
(app-pas-get-rpm)
```

Returns the pedal RPM measured by the PAS-app. If you want to implement your own PAS-control based on this RPM you can use [app-disable-output](#app-disable-output) to disable the output of the PAS-app.

### Motor Set Commands

#### set-current

```clj
(set-current current optOffDelay)
```

Set motor current in amperes.

The optional optOffDelay argument (in seconds) will delay turning off the modulation when setting 0 current. This is useful when running e.g. a control loop that will end up setting 0 current in some circumstances when turning off the modulation would make the control less smooth. The delay value should be longer than the rate at which the control loop runs.

#### set-current-rel

```clj
(set-current-rel current optOffDelay)
```

Set motor current relative to the maximum current. Range -1 to 1. For example, if the maximum current is set to 50A, (set-current-rel 0.5) will set the current to 25A.

See [set-current](#set-current) for details on what the optional argument optOffDelay does.

#### set-duty
```clj
(set-duty dutycycle)
```

Set duty cycle. Range -1.0 to 1.0.

#### set-brake
```clj
(set-brake current)
```

Set braking current.

#### set-brake-rel
```clj
(set-brake-rel current)
```

Set braking current relative to the maximum current, range 0.0 to 1.0.

#### set-handbrake
```clj
(set-handbrake current)
```

Set handbrake current. This sets an open loop current that allows to hold the motor still even at 0 speed at the cost of efficient.

#### set-handbrake-rel
```clj
(set-handbrake-rel current)
```

Same as set-handbrake, but with a current relative to the maximum current in the range 0.0 to 1.0.

#### set-rpm
```clj
(set-rpm rpm)
```

Set RPM speed control.

#### set-pos
```clj
(set-pos pos)
```

Position control. Set motor position in degrees, range 0.0 to 360.0.

#### foc-openloop
```clj
(foc-openloop current rpm)
```

Run FOC in open loop. Useful to test thermal properties of motors and power stages.

#### foc-beep
```clj
(foc-beep freq time voltage)
```

Use the motor to play a beep sound at frequency freq for time seconds using voltage excitation voltage. The frequency can be set between 100 Hz and 7500 Hz.

### Motor Get Commands

#### get-current
```clj
(get-current)
```

Get motor current. Positive means that current is flowing into the motor and negative means that current is flowing out of the motor (regenerative braking).

#### get-current-dir
```clj
(get-current-dir)
```

Get directional current. Positive for torque in the forward direction and negative for torque in the reverse direction.

#### get-current-in
```clj
(get-current-in)
```

#### get-id
```clj
(get-id)
```

Get FOC d-axis current.

#### get-iq
```clj
(get-iq)
```

Get FOC q-axis current.

#### get-vd
```clj
(get-vd)
```

Get FOC d-axis voltage.

#### get-vq
```clj
(get-vq)
```

Get FOC q-axis voltage.

#### get-est-lambda
```clj
(get-est-lambda)
```

Get FOC estimated flux linkage in Weber. Requires that one of the observers with flux linkage tracking is used. Added in FW 6.02.

#### get-est-res
```clj
(get-est-res)
```

Get FOC estimated motor resistance in Ohm. This value is only accurate when the RPM is low and current is high. Added in FW 6.02.

#### get-est-ind
```clj
(get-est-ind)
```

Get FOC estimated motor inductance Henry. Only works while the first HFI is running (not 45 Deg and not Coupled HFI). Added in FW 6.02.

#### get-duty
```clj
(get-duty)
```

Get duty cycle. Range -1.0 to 1.0.

#### get-rpm
```clj
(get-rpm)
```

Get motor RPM. Negative values mean that the motor spins in the reverse direction.

#### get-pos
```clj
(get-pos)
```

Get motor position. Returns the motor PID pos (taking into account the Position Angle Division)

#### get-temp-fet
```clj
(get-temp-fet optFet)
```

Get MOSFET temperature. The argument optFet can be used to select senor 1 to 3. If it is left out or 0 the highest temperature is returned. If the hardware only has one sensor 0 is returned for sensors 1 to 3.

#### get-temp-mot
```clj
(get-temp-mot)
```

Get motor temperature.

#### get-speed
```clj
(get-speed)
```

Get speed in meters per second. Requires that the number of motor poles, wheel diameter and gear ratio are set up correctly.

#### get-dist
```clj
(get-dist)
```

Get the distance traveled since start in meters. As with (get-speed) this requires that the number of motor poles, wheel diameter and gear ratio are set up correctly. When the motor spins forwards this counter counts up and when it spins backwards it counts down.

#### get-dist-abs
```clj
(get-dist-abs)
```

Same as get-dist, but will count up when the motors spins in both directions.

#### get-batt
```clj
(get-batt)
```

Get the battery level, range 0.0 to 1.0. Requires that the battery type and number of cells is set up correctly.

#### get-fault
```clj
(get-fault)
```

Get fault code.

#### get-ah
```clj
(get-ah)
```

Get the number of amp hours consumed since start.

#### get-wh
```clj
(get-wh)
```

Get the number of watt hours consumed since start.

#### get-ah-chg
```clj
(get-ah-chg)
```

Get the number of amp hours charged since start.

#### get-wh-chg
```clj
(get-wh-chg)
```

Get the number of watt hours charged since start.

### Setup Values

These commands return the accumulated values from all VESC-based motor controllers on the CAN-bus. Note that the corresponding CAN status messages must be activated for these commands to work.

#### setup-ah
```clj
(setup-ah)
```

Get the number of amp hours consumed since start.

#### setup-ah-chg
```clj
(setup-ah-chg)
```

Get the number of amp hours charged since start.

#### setup-wh
```clj
(setup-wh)
```

Get the number of watt hours consumed since start.

#### setup-wh-chg
```clj
(setup-wh-chg)
```

Get the number of watt hours charged since start.

#### setup-current
```clj
(setup-current)
```

Get total motor current. Positive means that current is flowing into the motor and negative means that current is flowing out of the motor (regenerative braking).

#### setup-current-in
```clj
(setup-current-in)
```

#### setup-num-vescs
```clj
(setup-num-vescs)
```

Get the number of VESC-based motor controllers the setup values are accumulated from.

### CAN-Commands

Notice that all canget-commands rely on the status messages being active on the VESCs on the CAN-bus. That can be done from App Settings->General->Can status message mode.

#### canset-current
```clj
(canset-current id current optOffDelay)
```

Set current over CAN-bus on VESC with id. Example for setting 25A on VESC with id 115:

```clj
(canset-current 115 25)
```

See [set-current](#set-current) for details on what the optional argument optOffDelay does.

#### canset-current-rel
```clj
(canset-current-rel id current optOffDelay)
```

Same as above, but relative current in the range -1.0 to 1.0. See (set-current) for details on what relative current means.

See [set-current](#set-current) for details on what the optional argument optOffDelay does.

#### canset-duty
```clj
(canset-duty id duty)
```

Set duty cycle over CAN-bus on VESC with id. Range -1.0 to 1.0.

#### canset-brake
```clj
(canset-brake id current)
```

Set braking current over CAN-bus.

#### canset-brake-rel
```clj
(canset-brake-rel id current)
```

Set relative braking current over CAN-bus. Range 0.0 to 1.0.

#### canset-rpm
```clj
(canset-rpm id rpm)
```

Set rpm over CAN-bus.

#### canset-pos
```clj
(canset-pos id pos)
```

Set position control in degrees over CAN-bus. Range 0.0 to 1.0.

#### canget-current
```clj
(canget-current id)
```

Get current over CAN-bus on VESC with id.

#### canget-current-dir
```clj
(canget-current-dir id)
```

Get directional current over CAN-bus on VESC with id. See (get-current-dir) for what directional means.

#### canget-current-in
```clj
(canget-current-in id)
```

Get input current over CAN-bus on VESC with id.

#### canget-duty
```clj
(canget-duty id)
```

Get duty cycle over CAN-bus on VESC with id.

#### canget-rpm
```clj
(canget-rpm id)
```

Get RPM over CAN-bus on VESC with id.

#### canget-temp-fet
```clj
(canget-temp-fet id)
```

Get MOSFET temperature over CAN-bus on VESC with id.

#### canget-temp-motor
```clj
(canget-temp-motor id)
```

Get motor temperature over CAN-bus on VESC with id.

#### canget-speed
```clj
(canget-speed id)
```

Get speed in meters per second over CAN-bus on VESC with id. The gearing, wheel diameter and number of motor poles from the local configuration will be used for converting the RPM to meters per second.

#### canget-dist
```clj
(canget-dist id)
```

Get distance traveled in meters over CAN-bus on VESC with id. As with (canget-speed id), the local configuration will be used to convert the tachometer value to meters.

#### canget-ppm
```clj
(canget-ppm id)
```

Get PPM-input from the VESC with id on the CAN-bus. Note that CAN status message 6 as well as the PPM-app must be active on that VESC for this function to work.

#### canget-adc
```clj
(canget-adc id ch)
```

Get ADC channel ch from the VESC with id on the CAN-bus. Note that CAN status message 6 must be active on that VESC for this function to work.

#### can-list-devs
```clj
(can-list-devs)
```

List CAN-devices that have been heard on the CAN-bus since boot. This function is fast as it does not actively scan the CAN-bus, but it relies on the devices sending status message 1.

#### can-scan
```clj
(can-scan)
```

Actively scan the CAN-bus and return a list with devices that responded. This function takes several seconds to run, but also finds devices that do not actively send messages and only respond to a ping message.

#### can-send-sid
```clj
(can-send-sid id data)
```

Send standard ID CAN-frame with id and data. Data is a list with bytes, and the length of the list (max 8) decides how many data bytes are sent. Example:

```clj
(can-send-sid 0x11FF11 (list 0xAA 0x11 0x15))
```

*data* can be a list or a [byte array](#byte-arrays).

#### can-send-eid
```clj
(can-send-eid id data)
```

Same as (can-send-sid), but sends extended ID frame.

#### can-cmd

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

### Math Functions

#### sin
```clj
(sin angle)
```

Get the sine of angle. Unit: Radians.

#### cos
```clj
(cos angle)
```

Get the cosine of angle. Unit: Radians.

#### tan
```clj
(tan angle)
```

Get the tangent of angle. Unit: Radians.

#### asin
```clj
(asin x)
```

Get the arc sine of x. Unit: Radians.

#### acos
```clj
(acos x)
```

Get the arc cosine of x. Unit: Radians.

#### atan
```clj
(atan x)
```

Get the arc tangent of x. Unit: Radians.

#### atan2
```clj
(atan2 y x)
```

Get the arc tangent of y / x. Unit: Radians. This version uses the signs of y and x to determine the quadrant.

#### pow
```clj
(pow base power)
```

Get base raised to power.

#### sqrt
```clj
(sqrt x)
```

Get the square root of x.

#### log
```clj
(log x)
```

Get the base-e logarithm of x.

#### log10
```clj
(log10 x)
```

Get the base-10 logarithm of x.

#### floor
```clj
(floor x)
```

Round x down to the closest integer. Added in FW 6.02.

#### ceil
```clj
(ceil x)
```

Round x up to the closest integer. Added in FW 6.02.

#### round
```clj
(round x)
```

Round x to the closest integer. Added in FW 6.02.

#### deg2rad
```clj
(deg2rad x)
```

Converts x from degrees to radians.

#### rad2deg
```clj
(rad2deg x)
```

Converts x from radians to degrees.

#### vec3-rot
```clj
(vec3-rot x1 x2 x3 roll pitch yaw optRev)
```

Rotate vector x1,x2,x3 around roll, pitch and yaw. optRev (1 or 0) will apply the rotation in reverse (apply the inverse of the rotation matrix) if set to 1.

#### abs
```clj
(abs x)
```

Get the absolute value of x.

#### throttle-curve
```clj
(throttle-curve value accel brake mode)
```

Apply throttle curve on value. accel (range -1 to 1) is the curve constant for acceleration (when value is greater than 0) and brake (range -1 to 1) is the curve constant for braking (when value is less than 0). mode (0, 1 or 2) is the throttle curve mode. Negative curve constants mean that the throttle will be gentler in the beginning and more aggressive with towards the end and positive curve constants mean the opposite. The modes are 0: Exponential, 1: Natural and 2: Polynomial. You can have a look at the throttle curves in VESC Tool for the PPM, ADC or VESC Remote app and experiment with the mode and curve constants to see a plot of the response.

### Bit Operations

#### bits-enc-int
```clj
(bits-enc-int initial offset number bits)
```

Put bits of number in initial at offset and return the result. For example, if the bits initial are aaaaaaaa, number is bbb, offset is 2 and bits is 3 the result is aaabbbaa. For reference, the corresponding operation in C is:

```c
initial &= ~((0xFFFFFFFF >> (32 - bits)) << offset);
initial |= (number << (32 - bits)) >> (32 - bits - offset);
```

#### bits-dec-int
```clj
(bits-dec-int value offset size)
```

Return size bits of value at offset. For example if the bits of value are abcdefgh, offset is 3 and size it 3 a number with the bits cde is returned. The corresponding operation in C is:

```c
val >>= offset;
val &= 0xFFFFFFFF >> (32 - bits);
```

### Raw Commands

Raw data commands useful for debugging hardware issues.

#### raw-adc-current
```clj
(raw-adc-current motor phase useRaw)
```

Get raw current measurements. Motor is the motor index (1 or 2), phase is the phase (1, 2 or 3) and useRaw is whether to convert the measurements to currents or to use raw ADC values.

**NOTE**  
These samples can come from either V0 or V7 depending on when the function is called (although most likely V7 as less other computations happen then), so when the motor is running this is most likely not going to look good, especially if the hardware does not have phase shunts. This function is intended for debugging hardware and returns just was goes into the ADC without any processing.

Example for reading phase B on motor 1 as raw ADC values:

```clj
(raw-adc-current 1 2 1)
```

#### raw-adc-voltage
```clj
(raw-adc-voltage motor phase useRaw)
```

Same as (raw-adc-current), but measures phase voltages instead.

#### raw-mod-alpha
```clj
(raw-mod-alpha)
```

Get alpha modulation. Range -1.0 to 1.0 (almost).

#### raw-mod-beta
```clj
(raw-mod-beta)
```

Get beta modulation. Range -1.0 to 1.0 (almost).

#### raw-mod-alpha-measured
```clj
(raw-mod-alpha-measured)
```

Same as (raw-mod-alpha), but derives the modulation from the phase voltage reading and/or dead-time compensation.

#### raw-mod-beta-measured
```clj
(raw-mod-beta-measured)
```
Same as (raw-mod-beta), but derives the modulation from the phase voltage reading and/or dead-time compensation.

#### raw-hall
```clj
(raw-hall motor optSamples)
```
Read hall sensors for motor (1 or 2) and return their states in a list. The optional argument optSamples (max 20) can be used to set how many times the hall sensors are sampled; if it is not supplied the number of samples from the motor configuration will be used.

The function (ix list ind) can be used to get an element from the list. Example:
```clj
(ix (raw-hall 1) 0) ; Get hall sensor 1 state (index 0)
```

### UART

#### uart-start

```clj
(uart-start baudrate optHd)
```

Start the UART driver at baudrate on the COMM-port on the VESC. optHd is an optional argument that can be set to 'half-duplex to use half-duplex mode. In half-duplex mode only the tx-pin is used. If any app is using the UART pins it will be stopped first. Example:

```clj
(uart-start 115200) ; Start UART at 115200 baud in full duplex mode
(uart-start 115200 'half-duplex) ; Start UART at 115200 baud in half duplex mode
```

#### uart-write

```clj
(uart-write array)
```

Write array (see [byte array](#byte-arrays) for details) to the UART. Examples:

```clj
(uart-write "Hello World!") ; Write the string hello world!
```

```clj
(define arr (array-create 6)) ; Create a 6 byte long array
(bufset-i16 arr 0 1123) ; Set byte 0 and 1 to 1123
(bufset-i32 arr 2 424242) ; Set byte 2 to 5 to 424242 
(uart-write arr) ; Write arr to the uart
```

#### uart-read

```clj
(uart-read array num optOffset optStopAt)
```

Read num bytes into array at offset optOffset. Stop reading if the character optStopAt is received. The last two arguments are optional. Note that this function returns immediately if there is nothing to be read, so it is not blocking. The return value is the number of bytes read.

#### uart-read-bytes

```clj
(uart-read-bytes array num offset)
```

Read num bytes into buffer at offset. This function is blocking, so it will not return until the specified amount of bytes is read.

#### uart-read-until

```clj
(uart-read-until array num offset end)
```

Same as uart-read-bytes, but will return when the byte end is read.

### I2C

#### i2c-start

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
```

#### i2c-tx-rx

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

#### i2c-restore

```clj
(i2c-restore)
```

Sends a sequence of bits in an attempt to restore the i2c-bus. Can be used if an i2c-device hangs and refuses to respond.

### GPIO

These functions allow using GPIO-pins from lispBM. The UART and SWD pins can currently be used. NOTE: If you are using the SWD-pins a SWD-programmer won't work after that until the next reset. If you are using the hall sensor pins make sure that sensor port mode is not set to anything that will communicate with encoders using those pins. Leaving the sensor port in hall sensor mode should be fine.

#### gpio-configure

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

#### gpio-write

```clj
(gpio-write pin state)
```

Write state to pin. If the pin is set to an output 1 will set it to VCC and 0 to GND. If the pin is open drain 1 will set it floating and 0 will set it to GND. Example:

```clj
(gpio-write 'pin-rx 1) ; Set pin rx to 1
```

#### gpio-read

```clj
(gpio-read pin)
```

Read state of pin. Returns 1 if the pin is high, 0 otherwise.

### Input Capture (ICU)

Input capture can be used to measure pulse lengths and periods on the PPM input pin. This can be used to measure the frequency and duty cycle of PWM-signals. The ICU driver was added in FW 6.02.

#### icu-start

```clj
(icu-start freqHz pol)
```

Start input capture on the PPM-pin with timer frequency freqHz hertz and polarity pol. Polarity = 1 means that the high part of the pulse is measured and polarity = 0 means that the low part of the pulse is measured. Every time a pulse is received an [event](#events) is generated. Example:

```clj
; Run timer at 1 MHz and capture the positive part of the pulse
(icu-start 1000000 1)

(def cb-cnt 0)

(defun proc-icu (width period)
    (progn
        (def icu-w width)
        (def icu-p period)
        (def cb-cnt (+ cb-cnt 1))
))

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

#### icu-width

```clj
(icu-width)
```

Get the width of the last captured pulse.

#### icu-period

```clj
(icu-period)
```

Get the period of the last captured pulse.

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
'motor-type             ; Motor Type
                        ;    0: BLDC (6-step commutation)
                        ;    1: DC (DC motor on phase A and C)
                        ;    2: FOC (Field Oriented Control)
                        ;    3: GPD (General Purpose Drive)
'l-watt-max             ; Maximum power regen in W
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
'si-motor-poles         ; Number of motor poles, must be multiple of 2
'foc-current-kp         ; FOC current controller KP
'foc-current-ki         ; FOC current controller KI
'foc-motor-l            ; Motor inductance in microHenry
'foc-motor-ld-lq-diff   ; D and Q axis inductance difference in microHenry
'foc-motor-r            ; Motor resistance in milliOhm
'foc-motor-flux-linkage ; Motor flux linkage in milliWeber
'foc-observer-gain      ; Observer gain x1M
'foc-hfi-voltage-start  ; HFI start voltage (V) (for resolving ambiguity)
'foc-hfi-voltage-run    ; HFI voltage (V) HFI voltage at min current
'foc-hfi-voltage-max    ; HFI voltage (V) at max current
'foc-sl-erpm-hfi        ; ERPM where to move to sensorless in HFI mode
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
                        ;    9: APP_BALANCE
                        ;    10: APP_PAS
                        ;    11: APP_ADC_PAS
'controller-id          ; VESC CAN ID
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
```

#### conf-set

```clj
(conf-set param value)
```

Set param to value. This can be done while the motor is running and it will be applied instantly. Note that the parameter won't be stored in flash, so it will be back to the old value on the next boot. To store all parameters that have been changed you can use [conf-store](#conf-store). Example:

```clj
(conf-set 'max-speed (/ 25 3.6)) ; Set the maximum speed to 25 km/h
```

#### conf-get

```clj
(conf-get param optDefLim)
```

Get the value of param. optDefLim is an optional argument that can be set to 1 or 2; 1 means get the default value and 2 means get the limit value. Example:

```clj
(conf-get 'foc-motor-r) ; Get the motor resistance in milliOhm
(conf-get 'controller-id 1) ; Get the default CAN ID of this VESC
(conf-get 'l-current-max 2) ; Get the maximum allowed current on this hardware
```

#### conf-store

```clj
(conf-store)
```

Store the current configuration to flash. This will stop the motor.

#### conf-detect-foc

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

#### conf-set-pid-offset

```clj
(conf-set-pid-offset offset optStore)
```

Set the PID controller offset such that the current angle becomes offset. This can be used in position control applications when e.g. homing against a limit switch. The optional argument optStore can be set to true to store the offset persistently (although that requires stopping the motor).

#### conf-measure-res

```clj
(conf-measure-res current optSamples)
```

Measure motor resistance with current. The optional argument optSamples sets the number of samples to use (default 100).

This command is useful to update the configuration before starting the motor as the resistance is the most important when it comes to sensorless low-speed performance. It is also useful to sanity check if the motor is shorted out or if a connector is loose. Such faults cause a relatively significant change in resistance. Changes with more than 50% compared to the detected value are most likely faults.

**NOTE:** Phase filters are required to get accurate resistance measurements, so resistance-based fault detection is not as useful on hardware without phase filters.

### EEPROM (Nonvolatile Storage)

Up to 128 variables (int32 or float) can be stored in a nonvolatile memory reserved for LispBM. These variables persist between power cycles and configuration changes, but not between firmware updates. Keep in mind that the motor will be stopped briefly when writing them and that they only can be written a limited number of times (about 100 000 writes) before wear on the flash memory starts to become an issue.

#### eeprom-store-f

```clj
(eeprom-store-f addr number)
```

Store float number on emulated eeprom at address addr. Addr range: 0 to 127. Note that this will stop the motor briefly as writing to the flash memory cannot be done at the same time as the motor is running.

#### eeprom-read-f

```clj
(eeprom-read-f addr)
```

Read float number on emulated eeprom at address addr. Addr range: 0 to 127. If nothing was stored on that address this function returns nil.

#### eeprom-store-i

```clj
(eeprom-store-i addr number)
```

Same as eeprom-store-f, but store number as i32 instead of float.

#### eeprom-read-i

```clj
(eeprom-read-i addr)
```

Same as eeprom-read-i, but read number as i32 instead of float.

### Loops

#### loopfor

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

; Remember that multiple statements in the loop require a progn:
(loopfor i 0 (< i 5) (+ i 1)
    (progn
        (print i)
        (sleep 0.5)
))
```

#### loopwhile

```clj
(loopwhile cond body)
```

While-loop. cond is the condition that has the be true for the loop to continue running and body is the code to execute each iteration. Example:

```clj
(define i 0)

(loopwhile (< i 5)
    (progn
        (print i)
        (define i (+ i 1))
))

Output:
0
1
2
3
4
```

Another example that prints "Hello World" every two seconds:

```clj
(loopwhile t
    (progn
        (print "Hello World")
        (sleep 2)
))
```

#### looprange

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

; As with the other loops, multiple statements require a progn
(looprange i 0 5
    (progn
        (print i)
        (sleep 0.5)
))
```

#### loopforeach

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

; As with the other loops, multiple statements require a progn
(loopforeach i '("AB" "C" "dE" "f")
    (progn
        (print i)
        (sleep 0.5)
))

```

#### break

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

### Useful Lisp Functions

There are a number of lisp functions that can be used from lispBM in the VESC firmware. They will be loaded to the environment the first time they are used, so they do not use up memory before the first use.

#### defun

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

#### map

```clj
(map f lst)
```

Apply function f to every element in list lst. Example:

```clj
(map (lambda (x) (* x 5)) '(1 2 3 4))
> (5 10 15 20)
```

This example creates an anonymous function that takes one argument and returns that argument multiplied by 5. Map then applies it to every element in the list (1 2 3 4), which yields the list (5 10 15 20).

#### range

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

#### foldl

```clj
(foldl f init lst)
```

Apply the function f to pairs of init and each element of the list lst and accumulate the result. Example:

```clj
(foldl + 0 '(1 2 3 4 5))
> 15
```

#### foldr

Same as foldl, but start from the right side of lst.

#### reverse

```clj
(reverse lst)
```

Returns the list lst in reverse. Example:

```clj
(reverse '(1 2 3 4 5))
> (5 4 3 2 1)
```

#### length

```clj
(length lst)
```

Returns the length of list lst. Example:

```clj
(length '(1 2 3))
> 3
```

#### apply

```clj
(apply f lst)
```

Use the elements in list lst as arguments to function f. Example:

```clj
(apply + '(1 2 3))
> 6
```

#### zipwith

```clj
(zipwith f x y)
```

Apply the function f to pairs between the elements in list x and list y. Example:

```clj
(zipwith * '(1 2 3) '(3 4 5))
> (3 8 15)
```

#### filter

```clj
(filter f lst)
```

Filter list by keeping the elements on which f returns true. Example:

```clj
(filter (lambda (x) (< x 5)) '(3 9 5 8 2 4 7))
> (3 2 4)
```

#### sort

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

### String Manipulation

#### str-from-n

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

#### str-merge

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

#### str-to-i

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

#### str-to-f

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

#### str-part

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

#### str-split

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

#### str-replace

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

#### str-to-upper

```clj
(str-to-upper str)
```

Convert string str to upper case. Example:

```clj
(str-to-upper "TesTt")
> "TESTT"
```

#### str-to-lower

```clj
(str-to-lower str)
```

Convert string str to lower case. Example:

```clj
(str-to-lower "TesTt")
> "testt"
```

#### str-cmp

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

#### str-cmp-asc

```clj
(str-cmp-asc str1 str1)
```

Return true if str1 comes before str2, nil otherwise. Useful for sorting strings using the [sort](#sort) function in ascending order.

#### str-cmp-dsc

```clj
(str-cmp-dsc str1 str1)
```

Return true if str2 comes before str1, nil otherwise. Useful for sorting strings using the [sort](#sort) function in descending order.

#### str-len

```clj
(str-len str)
```

Calculate length of string str excluding the null termination. Example:

```clj
(str-len "Hello")
> 5
```

#### to-str

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

#### to-str-delim

```clj
(to-str-delim delimiter arg1 ... argN)
```

Same as [to-str](#to-str), but with a custom delimiter. Example:

```clj
(to-str-delim "::" "aAa" 4 '(a 2 3) 2 3 "Hello")
> "aAa::4::(a 2 3)::2::3::Hello"
```

## Events

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
(event-enable 'event-can-sid)  ; Sends (signal-can-sid . (id . data)), where id is U32 and data is a byte array
(event-enable 'event-can-eid)  ; Sends (signal-can-eid . (id . data)), where id is U32 and data is a byte array
(event-enable 'event-data-rx)  ; Sends (signal-data-rx . data), where data is a byte array
(event-enable 'event-shutdown) ; Sends signal-shutdown
(event-enable 'event-icu-width) ; Sends (event-icu-width . (width . period))
(event-enable 'event-icu-period) ; Sends (event-icu-period . (width . period))
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

## Byte Arrays

Byte arrays (and text strings) are allocated in memory as consecutive arrays of bytes (not linked lists). They can be shared with C and are more space and performance efficient than linked lists. Several of the extensions also take byte arrays as input as an alternative to lists and some of the events return byte arrays.

To allocate a byte array with 20 bytes and bind the symbol arr to it you can use

```clj
(define arr (array-create 20))
```

#### buflen

The length of a byte array can be read with

```clj
(buflen arr)
```

Which will return 20 for the array arr above.

#### bufclear

To clear a byte array the function bufclear can be used:

```clj
(bufclear arr optByte optStart optLen)
```

Where arr is the byte array to clear, optByte is the optional argument of what to clear with (default 0), optStart is the optional argument of which position to start clearing (default 0) and optLen is the optional argument of how many bytes to clear after start (default the entire array). Example:

```clj
(bufclear arr) ; Clear all of arr
(bufclear arr 0xFF) ; Fill arr with 0xFF
(bufclear arr 0 5) ; Clear from index 5 to the end
(bufclear arr 0 5 10) ; Clear 10 bytes starting from index 5
(bufclear arr 0xAA 5 10) ; Set 10 bytes to 0xAA starting from index 5
```

#### bufget-\[x\]

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

#### bufset-\[x\]

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

#### bufcpy

Copy part of one array into another array.

```clj
(bufcpy arr1 ind1 arr2 ind2 len)
```

Copy len bytes from arr2 starting at ind2 to arr1 starting at ind1. Len will be truncated to ensure that nothing is read or written outside of the arrays.

#### free

Byte arrays will be de-allocated by the garbage collector on a regular basis, but can still use a lot of memory until then and large byte arrays cause a risk of running out of memory. It is possible to manually de-allocate the byte arrays when done with them by calling free

```clj
(free arr)
```

This will clear the allocated memory for arr.

**Note**  
Strings in lispBM are treated the same as byte arrays, so all of the above can be done to the characters in strings too.

## Import Files

Import is a special command that is mostly handled by VESC Tool. When VESC Tool sees a line that imports a file it will open and read that file and attach it as binary data to the end of the uploaded code. VESC Tool also generates a table of the imported files that will be allocated as arrays and passed to LispBM at start and bound to bindings.

Every imported file behaves as a byte array that is read-only (so do not try to modify it). These byte arrays can be used as usual from the lisp code to, for example, load native libraries or to load more lisp code at runtime. As they are stored in flash in raw binary format there is significantly more space available than when using e.g. the array syntax. The lisp script and the imported files can use up to 120 KB together.

#### import

```clj
(import filename binding)
```

Load filename as a byte array and bind it to binding. Note that import must be on its own line and that every line can only have one import.

**Example: Load native library**

```clj
(import "ws2812.bin" 'ws2812) ; Import the native ws2812 library. This creates the byte array ws2812 that can be used usual.
(load-native-lib ws2812); Load it to get the extensions it provides 
```

### Import Paths

Paths for import can be relative or absolute. Absolute paths are always looked up from the root of the file system, but relative paths need to be resolved. If the lisp-file is saved (e.g. there is a path at the bottom of the editor) paths are looked up relative to the location of that lisp file. If they are not found there they are looked up relative to the location where VESC Tool is started. If the file never has been saved (e.g. you just opened a new tab and started typing) the file path is unknown until save as is used, so only the path relative to VESC Tool is looked up then.

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

#### load-native-lib

```clj
(load-native-lib lib)
```

Load the native library lib. lib is a byte array with the compiled binary that is created after running make on the native library.

#### unload-native-lib

```clj
(unload-native-lib lib)
```

Unload the native library lib. This is done automatically when lispBM is stopped or restarted, so there is no need to do it explicitly. This function is provided in case native libraries need to be explicitly loaded and unloaded while the same program is running.

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

## UAVCAN

#### uavcan-last-rawcmd

```clj
(uavcan-last-rawcmd canInterface)
```

Get the last raw uavcan-command and its age. Returns a list where the first element is the value and the second element is the age. canInterface is the interface, which can be 1 or 2. Interface 2 is only valid if the hardware has dual CAN-buses. Example:

```clj
(print (ix (uavcan-last-rawcmd 1) 0)) ; Print the value
(print (ix (uavcan-last-rawcmd 1) 1)) ; Print the age in seconds
```

#### uavcan-last-rpmcmd

```clj
(uavcan-last-rpmcmd canInterface)
```

Same as uavcan-last-rawcmd, but for the last rpm-command.

## LispBM

#### lbm-set-quota

```clj
(lbm-set-quota quota)
```

Set how many evaluation steps to run each thread between context switches. Default is 50. A lower value will alter between threads more often, reducing latency between context switches at the cost of overall performance. The default value of 50 has relatively low performance overhead. Setting the quota to the lowest possible value of 1, meaning that each thread gets to run one step at a time, roughly halves the performance.

Lowering this value is useful if there are one or more timing-critical threads (that e.g. read encoders) that cannot wait too long between iterations.

## Plotting

VESC Tool can be used for plotting data using the Realtime Data->Experiment page. The following commands are used to set up a plot and send data.

#### plot-init

```clj
(plot-init namex namey)
```

Start a new plot with namex as the x axis name and namey as the u axis name.

#### plot-add-graph

```clj
(plot-add-graph name)
```

Add a graph to the current plot that will be called name. Every added graph gets a new index, starting from 0.

#### plot-set-graph

```clj
(plot-set-graph ind)
```

Set graph index to which data points are sent.

#### plot-send-points

```clj
(plot-send-points x y)
```

Send a xy-point to the selected graph in the plot.

## IO Boards

CAN-connected IO-boards can be interfaced using the functions in this section.

#### ioboard-get-adc

```clj
(ioboard-get-adc can-id channel)
```

Read ADC-input channel from IO-board with can-id. Channel range: 1 to 8. If the IO-board with can-id is not present on the CAN-bus or if the channel is missing -1 will be returned.

#### ioboard-get-digital

```clj
(ioboard-get-digital can-id channel)
```

Read  digital input channel from IO-board with can-id. Channel range: 1 to 64. If the IO-board with can-id is not present on the CAN-bus or if the channel is missing -1 will be returned.

#### ioboard-set-digital

```clj
(ioboard-set-digital can-id channel state)
```

Write digital output channel to IO-board with can-id. State can be 1 or 0.

#### ioboard-set-pwm

```clj
(ioboard-set-pwm can-id channel duty)
```

Write PWM-output channel to IO-board with can-id. The value duty can be 0.0 to 1.0.

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

#### log-config-field

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
ID on the CAN-bus. Setting the id to -1 will send the data to VESC Tool.

**field-ind**  
Field index in the log.

**key**  
Keyword string.

**name**  
Name string.

**unit**  
Unit string.

**is-relative**  
Relative fields are displayed relative to the start value of the log.

**is-timestamp**  
Timestamp fields are displayed with the format hh:mm:ss.

#### log-start

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
ID on the CAN-bus. Setting the id to -1 will send the data to VESC Tool.

**field-num**  
Number of log fields.

**rate-hz**  
Log rate in Hz.

**append-time**  
If set to true the log device will append a timestamp to each sample.

**append-gnss**  
If set to true the log device will append a GNSS-position to each sample. This requires a GNSS-receiver on the log device and the log will not start until a valid position is available.

#### log-stop

```clj
(log-stop can-id)
```

Stop logging data on log device with can-id. Setting the id to -1 will send the data to VESC Tool.

#### log-send-f32

```clj
(log-send-f32 can-id from-field-ind sample1 ... sampleN)
```

Send log samples to log device with can-id. This function takes 1 to 100 samples as arguments which will be applied to the log fields starting from from-field-ind. The samples can be numbers or lists of numbers. Setting the id to -1 will send the data to VESC Tool.

#### log-send-f64

```clj
(log-send-f64 can-id from-field-ind sample1 ... sampleN)
```

Same as [log-send-f32](#log-send-f32) but uses 64-bit values for higher precision and takes up to 50 samples. Useful for e.g. gnss-positions where 32-bit floats do not give enough precision due to the size of the earth.

## GNSS

If a GNSS-receiver such as the VESC Express is connected on the CAN-bus, the position, speed, time and precision data from it can be read from LBM.

#### gnss-lat-lon

```clj
(gnss-lat-lon)
```

Returns the latitude and longitude of the position as a list with two elements.

#### gnss-height

```clj
(gnss-height)
```

Returns the height of the position in meters.

#### gnss-speed

```clj
(gnss-speed)
```

Returns the speed on meters per second.

#### gnss-hdop

```clj
(gnss-hdop)
```

Returns the hdop-value of the position. Lower values mean that the precision is better.

#### gnss-date-time

```clj
(gnss-date-time)
```

Returns date and time of the last position sample as a list with the format (year month day hours minutes seconds milliseconds).

#### gnss-age

```clj
(gnss-age)
```

Returns the age of the last gnss-sample in seconds.

## How to update

To update from remote repository:

```bash
git remote add lispBM git@github.com:svenssonjoel/lispBM.git
git subtree pull --squash --prefix=lispBM/lispBM/ lispBM master
```

The first command might fail if it already is added, but the second one should still work. If there are uncomitted changes you can run **git stash** before the commands and **git stash pop** after them.
