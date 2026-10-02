#ifndef HAL_H
#define HAL_H

#include <stdint.h>

#define CAN_IDE_STD		0
#define CAN_IDE_EXT		1

typedef struct {
	uint8_t DLC;
	uint8_t RTR;
	uint8_t IDE;
	uint32_t SID;
	uint32_t EID;
	uint8_t data8[8];
} CANRxFrame;

extern int test_reset_count;
#define NVIC_SystemReset()	(test_reset_count++)

#endif // HAL_H
