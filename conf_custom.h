/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

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

#ifndef CONF_CUSTOM_H_
#define CONF_CUSTOM_H_

#include <stdint.h>
#include <stdbool.h>

// Functions
void conf_custom_add_config(
		int (*get_cfg)(uint8_t *data, bool is_default),
		bool (*set_cfg)(uint8_t *data),
		int (*get_cfg_xml)(uint8_t **data));
void conf_custom_clear_configs(void);
int conf_custom_cfg_num(void);
void conf_custom_process_cmd(unsigned char *data, unsigned int len,
		void(*reply_func)(unsigned char *data, unsigned int len));

#endif /* CONF_CUSTOM_H_ */
