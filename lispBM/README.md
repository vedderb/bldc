# LispBM

This is the VESC-integration of [lispBM](https://github.com/svenssonjoel/lispBM) written by Joel Svensson. It allows the VESC to run lisp-programs in a sandboxed environment.

### Feature Overview

* Development and testing in VESC Tool with variable live monitoring and plotting as well as CPU and memory monitoring.
* Sandboxed environment, meaning that the Lisp code (hopefully) cannot freeze or crash the rest of the VESC code when it gets stuck or runs out of heap or stack memory.
* The application runs on the VESC itself without the need for having VESC Tool connected and is stored in flash memory.
* When a lisp-application is written to the VESC it is automatically started on each boot.

## Documentation

Basics about LispBM are documented [here](http://svenssonjoel.github.io/lbmdoc/html/lbmref.html). The VESC-specific extensions are documented in this section. Note that VESC Tool includes a collection of examples that can be used as a starting point for using lisp on the VESC.

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

Reset the timeout that stops the motor. This has to be run on at least every second to keep the motor running. The timeout time can be configured in App Settings->General.

#### get-ppm

```clj
(get-ppm)
```

Read the decoded value on the PPM input and returns 0.0 to 1.0. Note that the PPM app has to be configured and running. Example:

```clj
(print (list "PPM Value: " (get-ppm)))
```

Note that control type can be set to Off in the PPM app to get the input without running the motor automatically, which is useful when running the motor from lisp.

#### get-encoder

```clj
(get-encoder)
```

Get angle from selected encoder in degrees.

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
(get-bms-val "v_tot") ; Total voltage
(get-bms-val "v_charge") ; Charge input voltage
(get-bms-val "i_in_ic") ; Measured current (negative means charging)
(get-bms-val "ah_cnt") ; Amp hour counter
(get-bms-val "wh_cnt") ; Watt hour counter
(get-bms-val "cell_num") ; Number of cells in series
(get-bms-val "v_cell" 2) ; Cell 3 voltage (index starts from 0)
(get-bms-val "bal_state 2") ; Cell 3 balancing state. 0: not balancing, 1: balancing
(get-bms-val "temp_adc_num") ; Temperature sensor count
(get-bms-val "temps_adc" 2) ; Get sensor 3 temperature (index starts from 0)
(get-bms-val "temp_ic") ; Balance IC temperature
(get-bms-val "temp_hum") ; Humidity sensor temperature
(get-bms-val "hum") ; Humidity
(get-bms-val "temp_cell_max") ; Maximum cell temperature
(get-bms-val "soc") ; State of charge (0.0 to 1.0)
(get-bms-val "can_id") ; CAN ID of BMS
(get-bms-val "ah_cnt_chg_total") ; Total ah charged
(get-bms-val "wh_cnt_chg_total") ; Total wh charged
(get-bms-val "ah_cnt_dis_total") ; Total ah discharged
(get-bms-val "wh_cnt_dis_total") ; Total wh discharged
(get-bms-val "msg_age") ; Age of last message from BMS in seconds
```

#### get-adc

```clj
(get-adc ch)
```

Get ADC voltage on channel ch (0, 1 or 2).

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

The function (ix ind list) can be used to get an element from the list. Example:
```clj
(ix 0 (get-imu-rpy)) ; Get roll (index 0)
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

#### send-data
```clj
(send-data dataList)
```
Send a list of custom app data to VESC Tool. This can be read from a Qml script for example.

Example of sending the numbers 1, 2, 3 and 4:

```clj
(send-data (list 1 2 3 4))
```

### Motor Set Commands

#### set-current

```clj
(set-current current)
```

Set motor current in amperes.

#### set-current-rel

```clj
(set-current-rel current)
```

Set motor current relative to the maximum current. Range -1 to 1. For example, if the maximum current is set to 50A, (set-current-rel 0.5) will set the current to 25A.

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

Get input current. Will always be lower than the motor current. The closer the motor spins to full speed the closer the input current is to the motor current.

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

#### get-temp-fet
```clj
(get-temp-fet)
```

Get MOSFET temperature.

#### get-temp-motor
```clj
(get-temp-motor)
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

Get the distance traveled since start in meters. As with (get-speed) this requires that the number of motor poles, wheel diameter and gear ratio are set up correctly.

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

### CAN-Commands

#### canset-current
```clj
(canset-current id current)
```

Set current over CAN-bus on VESC with id. Example for setting 25A on VESC with id 115:

```clj
(canset-current 115 25)
```

#### canset-current-rel
```clj
(canset-current-rel id current)
```

Same as above, but relative current in the range -1.0 to 1.0. See (set-current) for details on what relative current means.

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

#### can-send-sid
```clj
(can-send-sid id data)
```

Send standard ID CAN-frame with id and data. Data is a list with bytes, and the length of the list (max 8) decides how many data bytes are sent. Example:

```clj
(can-send-sid 0x11FF11 (list 0xAA 0x11 0x15))
```

#### can-send-eid
```clj
(can-send-eid id data)
```

Same as (can-send-sid), but sends extended ID frame.

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

#### atan
```clj
(atan x)
```

Get the arctangent of x. Unit: Radians.

#### atan2
```clj
(atan2 y x)
```

Get the arctangent of y / x. Unit: Radians. This is the version that uses two arguments.

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

### Bit Operations

#### bits-enc-int
```clj
(bits-enc-int initial number offset bits)
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

The function (ix ind list) can be used to get an element from the list. Example:
```clj
(ix 0 (raw-hall 1)) ; Get hall sensor 1 state (index 0)
```

## Events

Events can be used to execute code for certain events, such as when CAN-frames are received. To use events you must first register an event handler, then enable the events you want to receive. As the event handler blocks until the event arrives it is useful to spawn a thread to handle events so that other things can be done in the main thread at the same time.

The following example shows how to spawn a thread that handles SID (standard-id) CAN-frames and custom app data:

```clj
(define proc-sid (lambda (id data)
    (print (list id data)) ; Print the ID and data
))

(define proc-data (lambda (data)
    (progn
        (print data)
)))

(define event-handler (lambda ()
    (progn
        (recv ((signal-can-sid (? id) . (? data)) (proc-sid id data))
        (recv ((signal-data-rx ? data) (proc-data data))
              (_ nil)) ; Ignore other events
        (event-handler) ; Call self again to make this a loop
)))

; Spawn the event handler thread and pass the ID it returns to C
(event-register-handler (spawn '(event-handler)))

; Enable the CAN event for standard ID (SID) frames
(event-enable "event-can-sid")

; Enable the custom app data event
(event-enable "event-data-rx")
```

Possible events to register are

```clj
(event-enable "event-can-sid") ; Sends (signal-can-sid id data), where id is U32 and data is a list of i28
(event-enable "event-can-eid") ; Sends (signal-can-eid id data), where id is U32 and data is a list of i28
(event-enable "event-data-rx") ; Sends (signal-data-rx data), where data is a list of i28
```

The CAN-frames arrive whenever data is received on the CAN-bus and data-rx is received for example when data is sent from a Qml-script in VESC Tool.

## How to update

To update from remote repository:

```
git remote add lispBM git@github.com:svenssonjoel/lispBM.git
git subtree pull --squash --prefix=lispBM/lispBM/ lispBM master
```

The first command might fail if it already is added, but the second one should still work. If there are uncomitted changes you can run **git stash** before the commands and **git stash pop** after them.
