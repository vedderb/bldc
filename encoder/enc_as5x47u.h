/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Zach O'Brien

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef ENC_AS5x47U_H_
#define ENC_AS5x47U_H_

#include "encoder/encoder_datatype.h"

// Functions
/**
 * @brief Initalized the SPI peripheral used to communicate with the AS5x47U encoder.
 */
bool enc_as5x47u_init(AS5x47U_config_t *AS504x_config);

/**
 * @brief Resets the SPI peripheral used to communicate with the AS5x47U encoder.
 */
void enc_as5x47u_deinit(AS5x47U_config_t *cfg);

/**
 * @brief Starts a new SPI transaction to get the current position from the position sensor.
 * 
 * Every call of enc_as5x47u_routine ends up doing two SPI transactions. The first one
 * always receives position, but will request one of MAGN, AGC, DIAG, or ERRFL. The next
 * transaction receives the requested register while requesting pos for the next 
 * transaction.
 * @param cfg A pointer to the encoder config struct.
 */
void enc_as5x47u_routine(AS5x47U_config_t *cfg);

// Macros
#define AS5x47U_LAST_ANGLE(cfg)		((cfg)->state.last_enc_angle)

#endif /* ENC_AS5x47U_H_ */
