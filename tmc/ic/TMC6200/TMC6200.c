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
	address = TMC_ADDRESS(address);

	// write address
	tmc6200_readwriteByte(motor, address, false);

	// read data
	int value = tmc6200_readwriteByte(motor, 0, false);
	value <<= 8;
	value |= tmc6200_readwriteByte(motor, 0, false);
	value <<= 8;
	value |= tmc6200_readwriteByte(motor, 0, false);
	value <<= 8;
	value |= tmc6200_readwriteByte(motor, 0, true);

	return value;
}

void tmc6200_writeInt(uint8_t motor, uint8_t address, int32_t value)
{
	// write address
	tmc6200_readwriteByte(motor, address | TMC6200_WRITE_BIT, false);

	// write value
	tmc6200_readwriteByte(motor, 0xFF & (value>>24), false);
	tmc6200_readwriteByte(motor, 0xFF & (value>>16), false);
	tmc6200_readwriteByte(motor, 0xFF & (value>>8), false);
	tmc6200_readwriteByte(motor, 0xFF & (value>>0), true);
}
