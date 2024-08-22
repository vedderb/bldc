/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#ifndef RFHELP_H_
#define RFHELP_H_

#include <stdbool.h>
#include "datatypes.h"

// Functions
bool rfhelp_init(void);
void rfhelp_stop(void);
void rfhelp_update_conf(nrf_config *conf);
void rfhelp_restart(void);
int rfhelp_send_data(char *data, int len, bool ack);
int rfhelp_send_data_crc(char *data, int len);
int rfhelp_read_rx_data(char *data, int *len, int *pipe);
int rfhelp_read_rx_data_crc(char *data, int *len, int *pipe);
int rfhelp_rf_status(void);
void rfhelp_set_tx_addr(const char *addr, int addr_len);
void rfhelp_set_rx_addr(int pipe, const char *addr, int addr_len);
void rfhelp_power_down(void);
void rfhelp_power_up(void);

#endif /* RFHELP_H_ */
