/*
	Copyright 2023 Benjamin Vedder	benjamin@vedder.se
	Copyright 2023 Joel Svensson     svenssonjoel@yahoo.se

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

#ifndef MAIN_HWSPI_H_
#define MAIN_HWSPI_H_

#include <stdint.h>

/*
 * Triple buffering: Write to one buffer while one is in the queue for sending and one
 * is being sent. That means when the transaction finishes the next buffer is already
 * in the queue and there is no delay for queuing it before the next transaction can
 * start.
 *
 * NOTE: Making the buffer any larger seems to cause some data loss. No idea why that
 * happens, looks like some ESP-issue...
 */
#define HWSPI_DATA_BUFFER_SIZE 	1024
#define HWSPI_BUFFERS			3

// Global variables
extern uint8_t *hwspi_buffer_pointer;
extern int *hwspi_buffer_pos;

// Functions
void hwspi_init(int clk_mhz, int mode,
		int pin_miso, int pin_mosi, int pin_clk, int pin_cs);
void hwspi_begin(void);
void hwspi_end(void);
void hwspi_swap_buffer(void);
void hwspi_send_data(const uint8_t *data, int len);

void hwspi_data_stream_start(void);
static inline void hwspi_data_stream_write(uint8_t byte) {
	if (*hwspi_buffer_pos == HWSPI_DATA_BUFFER_SIZE) {
		hwspi_swap_buffer();
	}
	hwspi_buffer_pointer[(*hwspi_buffer_pos)++] = byte;
}
void hwspi_data_stream_finish(void);

#endif /* MAIN_HWSPI_H_ */
