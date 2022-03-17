/*
 * TMC6200.c
 *
 *  Created on: 14.03.2018
 *      Author: ED
 */

#include "TMC6200.h"

// => SPI wrapper
extern uint8_t tmc6200_readwriteByte(uint8_t motor, uint8_t data, uint8_t lastTransfer);
// <= SPI wrapper

// spi access
int32_t tmc6200_readInt(uint8_t motor, uint8_t address)
{
	// clear write bit
	address &= 0x7F;

	// write address
	tmc6200_readwriteByte(motor, address, 0);

	// read data
	int value = tmc6200_readwriteByte(motor, 0, 0);
	value <<= 8;
	value |= tmc6200_readwriteByte(motor, 0, 0);
	value <<= 8;
	value |= tmc6200_readwriteByte(motor, 0, 0);
	value <<= 8;
	value |= tmc6200_readwriteByte(motor, 0, 1);

	return value;
}

void tmc6200_writeInt(uint8_t motor, uint8_t address, int32_t value)
{
	// write address
	tmc6200_readwriteByte(motor, address | TMC6200_WRITE_BIT, 0);

	// write value
	tmc6200_readwriteByte(motor, 0xFF & (value>>24), 0);
	tmc6200_readwriteByte(motor, 0xFF & (value>>16), 0);
	tmc6200_readwriteByte(motor, 0xFF & (value>>8), 0);
	tmc6200_readwriteByte(motor, 0xFF & (value>>0), 1);
}
