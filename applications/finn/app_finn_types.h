#ifndef APPLICATIONS_FINN_APP_FINN_TYPES_H_
#define APPLICATIONS_FINN_APP_FINN_TYPES_H_

#include <stdint.h>
#include <stdbool.h>
#include "ch.h"
#include "hal.h"

/** ***************************************************************************
    \struct CAN_HEADER_UNION
    \brief CAN message header

    extid is 0 -> 0x1FFFFFFF. last field in struct is first byte
    first byte bits, mask 0x1F000000 -> prio0 = 0x10, prio1 = 0x08, prio2 = 0x04, res0 = 0x02, res1 = 0x01.
    second byte is packetType, mask 0x00FF0000.
    third byte is sourceIndex, mask 0x0000FF00.
    fourth byte is sourceType, mask 0x000000FF. 0-0xFF;

    extid = prio0, prio1, prio2, res0, res1, packetType, sourceIndex, sourceType.
    word/extid -> 0x000055AA -> sourcetype=0xAA, sourceIndex=0x55
******************************************************************************/

//typedef union CAN_HEADER_UNION {
//    struct fields {                        ///< Use this when accessing data
//        uint8_t sourceType;                ///< Last byte in header (sending device type)
//        uint8_t sourceIndex;            ///< Sometimes from DIP-switches val 0-3
//        uint8_t packetType;                ///< CAN_PACKET_TYPE (ex CAN_MESSAGE_THRUSTER_REQUEST)
//
//        union meta {
//            struct bits {                ///< (bus bits 28-24)
//                uint8_t reserved1:1;            ///< Set reserved bits to 0b11 for normal packets
//                uint8_t reserved0:1;            ///< Set reserved bits to 0b11 for normal packets
//                uint8_t priority2:1;            ///< Bit 2, third bit on the bus (bus bit 26)
//                uint8_t priority1:1;            ///< Bit 3, second bit on the bus (bus bit 27)
//                uint8_t priority0:1;            ///< Bit 4, first bit on the bus (bus bit 28)
//                uint8_t unused2:1;            ///< Bit 5, unused (not on bus)
//                uint8_t unused1:1;            ///< Bit 6, unused (not on bus)
//                uint8_t unused0:1;            ///< Bit 7, MSB, unused (not on bus)
//            };
//            uint8_t byte;
//        } meta;
//    };
//
//    uint32_t word;                        ///< Used when stuffing data from the CAN read function
//    uint8_t bytes[4];                    ///< Addressable with CAN_HEADER_BYTES
//} CAN_HEADER_UNION;

/** ***************************************************************************
    \enum CAN_HEADER_PRIORITIES
    \brief Packet priority field values
    \details The priority bits work are read from MSB to LSB:
    0b111    Lowest priority
    0b110    Medium Priority
    0b100    High priority
    0b000    Extra high priority

    Actual bits used in the meta byte:
    0x1C = 0b00011100
******************************************************************************/

enum CAN_HEADER_PRIORITIES {
    CAN_HEADER_PRIORITY_MASK =        0x1C,
    CAN_HEADER_PRIORITY_LOW =        0x1C,
    CAN_HEADER_PRIORITY_MID =        0x18,
    CAN_HEADER_PRIORITY_HIGH =        0x10,
    CAN_HEADER_PRIORITY_EXTRA =        0x00,
};

typedef struct CAN_PACKET_POD_REQ_FOUR {
    int16_t req_angle[4];
} CAN_PACKET_POD_REQ_FOUR;

typedef struct __attribute__((packed)) CAN_PACKET_POD_REQUEST {
    int16_t req_angle;
    uint8_t pod_id:3;
    uint8_t use_req_angle:1;

    uint8_t byte4;
    uint8_t byte5;
    uint8_t byte6;
    uint8_t byte7;
    uint8_t byte8;
} CAN_PACKET_POD_REQUEST;

typedef struct __attribute__((packed)) CAN_PACKET_POD_STATUS {
    // Byte 1-4
    int16_t actual_angle;            // -5000 to +5000
    int16_t accepted_angle;

    // Byte 5
    uint8_t pod_id_lo:2;                // pod id low bits
    uint8_t ready:1;
    uint8_t error:1;
    uint8_t stepdriver_alarm:1;
    uint8_t calib_running:1;
    uint8_t calib_error:1;
    uint8_t pod_id_hi:1;                // pod id hi bit

    // Byte 6
    uint8_t config_mode:1;
    uint8_t limitswitch:1;
    uint8_t fake_ready:1;

    uint8_t byte7;
    uint8_t byte8;
} CAN_PACKET_POD_STATUS;

typedef enum {
	CAN_MESSAGE_POD_REQUEST =           0x54,        // Request to POD (use POD_REQ_FOUR normally)
	CAN_MESSAGE_POD_STATUS =            0x55,        // Status from POD (actual angle, ready, errors)
	CAN_MESSAGE_POD_REQ_FOUR =            0x58,        // Request to up to four PODS in same packet 0-3
	CAN_MESSAGE_POD_REQ_FOUR_HI =        0x63,        // Request to up to four PODS in same packet 4-7
} CAN_MESSAGE_POD;

enum TIMING_CAN {
	TIMING_CAN_SEND_NORMAL =     2000,      ///< CAN send interval
	TIMING_CAN_SEND_UPDATE =     50,        ///< CAN send interval when changing status
	TIMING_CAN_RECEIVE_TIMEOUT = 2500,      ///< Timeout for received message, go to idle
};

enum POD_POS_ENUM {
    POD_POS_MAX = 4,
    POD_POS_REAR = 0,
    POD_POS_FRONT = 1,
    POD_POS_LEFT_REAR = 0,
    POD_POS_LEFT_FRONT = 1,
    POD_POS_RIGHT_REAR = 2,
    POD_POS_RIGHT_FRONT = 3,
};

typedef union {
    uint8_t bytes[8];
//    CAN_PACKET_SLEEP_REQUEST sleepRequest;
    CAN_PACKET_POD_REQ_FOUR podReqFour;
    CAN_PACKET_POD_REQUEST podRequest;
    CAN_PACKET_POD_STATUS podStatus;
//    CAN_PACKET_JOY_POS_RAW joyposraw;
} CAN_PAYLOAD_UNION;

typedef struct {
	int pod_id;

	float req_angle;
	float actual_angle;

	float angle_offset;
	float angle_home;

	bool homing_done;
	bool homing_error;

	bool wait_start;
	bool wait_data;

	float homing_angle_now;
	float homing_back_time;

	bool btn_limit_pressed;
	bool btn_left_pressed;
	bool btn_right_pressed;
	systime_t last_update;
} POD_STATE;

#endif /* APPLICATIONS_FINN_APP_FINN_TYPES_H_ */
