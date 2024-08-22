# VESC CAN-Bus Communication

The VESC Firmware supports 4 different CAN-modes, which can be selected from **App Settings -> General -> CAN Mode** in VESC Tool. The modes are:

| **Mode** | **Description** |
|------|-------------|
| VESC | Default VESC CAN-bus. Required for CAN forwarding and configuring multiple VESC-based devices using VESC Tool. |
| UAVCAN | Basic implementation of uavcan.equipment.esc. See https://dronecan.github.io for more info. |
| Comm Brigde | Bridge CAN-bus to commands. Useful for using VESC Tool as a generic CAN interface and debugger. |
| Unused | CAN-frames are not processed and just ignored. Custom applications and scripts can still process CAN-frames. This is similar to Comm Bridge, but the received frames are not forwarded using commands. |

The default and recommended mode CAN-mode is **VESC**.

## Timeout

By default there is a timeout that stops the motor when nothing has been received on the CAN-bus for more than 0.5 seconds. The timeout value can be changed in VESC Tool under App Settings -> General. Settings it to 0 disables the timeout, but it is strongly recommended to not disable it in case something goes wrong with the communication link. There is also a timeout brake current that can be configured on that page that can be used to brake the motor when timeout occurs. By default it is 0, which just releases the motor.

To prevent timeouts it is recommended to keep sending the desired commands over and over at a fixed rate, e.g. 50 Hz.

## VESC Mode

The VESC CAN-mode supports basic commands that fit in a single CAN-frame as well as everything in https://github.com/vedderb/bldc/blob/master/comm/commands.c by splitting up the commands into multiple frames. The basic commands are described in this document. For a full and precise description of the protocol it is best to look at the source code https://github.com/vedderb/bldc/blob/master/comm/comm_can.c.

### Frame Format

The VESC CAN-frames all use 29-bit extended IDs. The ID of the receiver as well as the command ID is embedded in the extended ID of the CAN-frame as:

| **B28 - B16** | **B15 - B8** | **B7 - B0** |
|-----------|----------|---------|
| Unused | Command ID | VESC ID |

The data bytes in the CAN-frame depend on the command. The VESC CAN ID can be set in VESC Tool under **App Settings -> General -> VESC ID**. Each VESC-based device will only accept commands if VESC ID is set to its ID in the CAN-frame.

### Single-Frame (simple) Commands

All simple CAN-commands have 4 data bytes which represent the argument for the commands as a 32-bit big endian signed number with scaling.

Example: the command CAN_PACKET_SET_CURRENT has command id 1 and scaling 1000. The CAN-frame for setting the current to 51 A on VESC ID 23 would look like the following:

| **ID** | **B0** | **B1** | **B2** | **B3** |
|----|----|----|----|----|
| 0x0117 | 0x00 | 0x00 | 0xC7 | 0x38 |

The following simple commands are available:

| **Command Name** | **Command ID** | **Scaling** | **Unit** | **Description** | **Range** |
|--------------|------------|---------|------|-------------|-------|
| CAN_PACKET_SET_DUTY | 0 | 100000 | % / 100 | Duty Cycle | \-1.0 to 1.0 |
| CAN_PACKET_SET_CURRENT | 1 | 1000 | A | Motor Current | \-MOTOR_MAX to MOTOR_MAX |
| CAN_PACKET_SET_CURRENT_BRAKE | 2 | 1000 | A | Braking Current | \-MOTOR_MAX to MOTOR_MAX |
| CAN_PACKET_SET_RPM | 3 | 1 | RPM | RPM | \-MAX_RPM to MAX_RPM |
| CAN_PACKET_SET_POS | 4 | 1000000 | Degrees |  | 0 to 360 |
| CAN_PACKET_SET_CURRENT_REL | 10 | 100000 | % / 100 |  | \-1.0 to 1.0 |
| CAN_PACKET_SET_CURRENT_BRAKE_REL | 11 | 100000 | % / 100 |  | \-1.0 to 1.0 |
| CAN_PACKET_SET_CURRENT_HANDBRAKE | 12 | 1000 | A |  | \-MOTOR_MAX to MOTOR_MAX |
| CAN_PACKET_SET_CURRENT_HANDBRAKE_REL | 13 | 100000 | % / 100 |  | \-1.0 to 1.0 |

#### C Code

The following C Code can be used to create and send the commands:

```c
#include <stdint.h>

// Implementation for sending extended ID CAN-frames
void can_transmit_eid(uint32_t id, const uint8_t *data, uint8_t len) {
    ...
}

typedef enum {
	CAN_PACKET_SET_DUTY = 0,
	CAN_PACKET_SET_CURRENT,
	CAN_PACKET_SET_CURRENT_BRAKE,
	CAN_PACKET_SET_RPM,
	CAN_PACKET_SET_POS,
	CAN_PACKET_SET_CURRENT_REL = 10,
	CAN_PACKET_SET_CURRENT_BRAKE_REL,
	CAN_PACKET_SET_CURRENT_HANDBRAKE,
	CAN_PACKET_SET_CURRENT_HANDBRAKE_REL,
	CAN_PACKET_MAKE_ENUM_32_BITS = 0xFFFFFFFF,
} CAN_PACKET_ID;

void buffer_append_int16(uint8_t* buffer, int16_t number, int32_t *index) {
	buffer[(*index)++] = number >> 8;
	buffer[(*index)++] = number;
}

void buffer_append_int32(uint8_t* buffer, int32_t number, int32_t *index) {
	buffer[(*index)++] = number >> 24;
	buffer[(*index)++] = number >> 16;
	buffer[(*index)++] = number >> 8;
	buffer[(*index)++] = number;
}

void buffer_append_float16(uint8_t* buffer, float number, float scale, int32_t *index) {
    buffer_append_int16(buffer, (int16_t)(number * scale), index);
}

void buffer_append_float32(uint8_t* buffer, float number, float scale, int32_t *index) {
    buffer_append_int32(buffer, (int32_t)(number * scale), index);
}

void comm_can_set_duty(uint8_t controller_id, float duty) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)(duty * 100000.0), &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_DUTY << 8), buffer, send_index);
}

void comm_can_set_current(uint8_t controller_id, float current) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)(current * 1000.0), &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_CURRENT << 8), buffer, send_index);
}

void comm_can_set_current_off_delay(uint8_t controller_id, float current, float off_delay) {
	int32_t send_index = 0;
	uint8_t buffer[6];
	buffer_append_int32(buffer, (int32_t)(current * 1000.0), &send_index);
	buffer_append_float16(buffer, off_delay, 1e3, &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_CURRENT << 8), buffer, send_index);
}

void comm_can_set_current_brake(uint8_t controller_id, float current) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)(current * 1000.0), &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_CURRENT_BRAKE << 8), buffer, send_index);
}

void comm_can_set_rpm(uint8_t controller_id, float rpm) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)rpm, &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_RPM << 8), buffer, send_index);
}

void comm_can_set_pos(uint8_t controller_id, float pos) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_int32(buffer, (int32_t)(pos * 1000000.0), &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_POS << 8), buffer, send_index);
}

void comm_can_set_current_rel(uint8_t controller_id, float current_rel) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_float32(buffer, current_rel, 1e5, &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_CURRENT_REL << 8), buffer, send_index);
}

/**
 * Same as above, but also sets the off delay. Note that this command uses 6 bytes now. The off delay is useful to set to keep the current controller running for a while even after setting currents below the minimum current.
 */
void comm_can_set_current_rel_off_delay(uint8_t controller_id, float current_rel, float off_delay) {
	int32_t send_index = 0;
	uint8_t buffer[6];
	buffer_append_float32(buffer, current_rel, 1e5, &send_index);
	buffer_append_float16(buffer, off_delay, 1e3, &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_CURRENT_REL << 8), buffer, send_index);
}

void comm_can_set_current_brake_rel(uint8_t controller_id, float current_rel) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_float32(buffer, current_rel, 1e5, &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_CURRENT_BRAKE_REL << 8), buffer, send_index);
}

void comm_can_set_handbrake(uint8_t controller_id, float current) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_float32(buffer, current, 1e3, &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_CURRENT_HANDBRAKE << 8), buffer, send_index);
}

void comm_can_set_handbrake_rel(uint8_t controller_id, float current_rel) {
	int32_t send_index = 0;
	uint8_t buffer[4];
	buffer_append_float32(buffer, current_rel, 1e5, &send_index);
	can_transmit_eid(controller_id |
			((uint32_t)CAN_PACKET_SET_CURRENT_HANDBRAKE_REL << 8), buffer, send_index);
}
```

### Status Commands

To receive motor status (RPM, Voltage, Current etc.) status commands can be activated from VESC Tool under **App Settings -> General -> CAN Status Messages Rate x**. Two sets of messages can be enabled where each set is transmitted at a specified rate. Each set can contain any combination of status messages. Using two rates is useful for not congesting the bus with messages that are not needed as often, which can be sent at a lower rate.

There are 6 different status messages available with the following data:

| **Command Name** | **Command Id** | **Content** |
|--------------|------------|---------|
| CAN_PACKET_STATUS | 9 | ERPM, Current, Duty Cycle |
| CAN_PACKET_STATUS_2 | 14 | Ah Used, Ah Charged |
| CAN_PACKET_STATUS_3 | 15 | Wh Used, Wh Charged |
| CAN_PACKET_STATUS_4 | 16 | Temp Fet, Temp Motor, Current In, PID position |
| CAN_PACKET_STATUS_5 | 27 | Tachometer, Voltage In |
| CAN_PACKET_STATUS_6 | 28 | ADC1, ADC2, ADC3, PPM |

The content of the status messages is encoded as follows:

**CAN_PACKET_STATUS**

| **Byte** | **Data** | **Unit** | **Scale** |
|------|------|------|-------|
| B0 - B3 | ERPM | RPM | 1 |
| B4 - B5 | Current | A | 10 |
| B6 - B7 | Duty Cycle | % / 100 | 1000 |

**CAN_PACKET_STATUS_2**

| **Byte** | **Data** | **Unit** | **Scale** |
|------|------|------|-------|
| B0 - B3 | Amp Hours | Ah | 10000 |
| B4 - B7 | Amp Hours Chg | Ah | 10000 |

**CAN_PACKET_STATUS_3**

| **Byte** | **Data** | **Unit** | **Scale** |
|------|------|------|-------|
| B0 - B3 | Watt Hours | Wh | 10000 |
| B4 - B7 | Watt Hours Chg | Wh | 10000 |

**CAN_PACKET_STATUS_4**

| **Byte** | **Data** | **Unit** | **Scale** |
|------|------|------|-------|
| B0 - B1 | Temp FET | DegC | 10 |
| B2 - B3 | Temp Motor | DegC | 10 |
| B4 - B5 | Current In | A | 10 |
| B6 - B7 | PID Pos | Deg | 50 |

**CAN_PACKET_STATUS_5**

| **Byte** | **Data** | **Unit** | **Scale** |
|------|------|------|-------|
| B0 - B3 | Tachometer | EREV | 6 |
| B4 - B5 | Volts In | V | 10 |

**CAN_PACKET_STATUS_6**

| **Byte** | **Data** | **Unit** | **Scale** |
|------|------|------|-------|
| B0 - B1 | ADC1 | V | 1000 |
| B2 - B3 | ADC2 | V | 1000 |
| B4 - B5 | ADC3 | V | 1000 |
| B6 - B7 | PPM | % / 100 | 1000 |

## Frequently Asked Questions (FAQ)

**What happends when sending commands outside of range?**  
- When sending commands outside of range they will be truncated at the range limit. For example, if the maximum braking current is set to 50A and CAN_PACKET_SET_CURRENT_BRAKE is sent with 60A the value will be truncated and 50A will be used.
