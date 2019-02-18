/*
	Copyright 2016 - 2019 Benjamin Vedder	benjamin@vedder.se

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

#ifndef NRF_NRF_DRIVER_H_
#define NRF_NRF_DRIVER_H_

#include <stdbool.h>

// Functions
bool nrf_driver_init(void);
void nrf_driver_init_ext_nrf(void);
void nrf_driver_stop(void);
void nrf_driver_start_pairing(int ms);
void nrf_driver_send_buffer(unsigned char *data, unsigned int len);
void nrf_driver_process_packet(unsigned char *buf, unsigned char len);
bool nrf_driver_is_pairing(void);
bool nrf_driver_ext_nrf_running(void);
void nrf_driver_pause(int ms);

#endif /* NRF_NRF_DRIVER_H_ */
