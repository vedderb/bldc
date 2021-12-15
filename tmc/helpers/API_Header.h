/*
 * tmc_header.h
 *
 *  Created on: 29.09.2016
 *      Author: ed
 */

#ifndef TMC_API_HEADER_H_
#define TMC_API_HEADER_H_

#include "Config.h"
#include "Macros.h"
#include "Constants.h"
#include "Bits.h"
#include "CRC.h"
#include "RegisterAccess.h"
#include <stdlib.h>
#include "Types.h"

// TODO: Restructure these.
/*
 * Goal: Just give these values here as status back to the IDE when used with EvalSystem.
 * Currently, this is obtained by just leaving out implementation specific error bits here.
 */
typedef enum {
	TMC_ERROR_NONE      = 0x00,
	TMC_ERROR_GENERIC   = 0x01,
	TMC_ERROR_FUNCTION  = 0x02,
	TMC_ERROR_MOTOR     = 0x08,
	TMC_ERROR_VALUE     = 0x10,
	TMC_ERROR_CHIP      = 0x40
} TMCError;

typedef enum {
	TMC_COMM_DEFAULT,
	TMC_COMM_SPI,
	TMC_COMM_UART
} TMC_Comm_Mode;

#endif /* TMC_API_HEADER_H_ */
