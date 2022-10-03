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

#include "vesc_c_if.h"

#include "conf/datatypes.h"
#include "conf/confparser.h"
#include "conf/confxml.h"

#include <string.h>

HEADER

typedef struct {
	balance_config balance_conf;
} data;

static int get_cfg(uint8_t *buffer, bool is_default) {
	data *d = (data*)ARG;
	balance_config cfg = d->balance_conf;
	
	if (is_default) {
		confparser_set_defaults_balance_config(&cfg);
	}
	
	return confparser_serialize_balance_config(buffer, &cfg);
}

static bool set_cfg(uint8_t *buffer) {
	data *d = (data*)ARG;
	bool res = confparser_deserialize_balance_config(buffer, &(d->balance_conf));
	
	// Store to EEPROM
	if (res) {
		uint32_t ints = sizeof(balance_config) / 4 + 1;
		uint32_t buffer[ints];
		bool write_ok = true;
		memcpy(buffer, &(d->balance_conf), sizeof(balance_config));
		for (uint32_t i = 0;i < ints;i++) {
			eeprom_var v;
			v.as_u32 = buffer[i];
			if (!VESC_IF->store_eeprom_var(&v, i + 1)) {
				write_ok = false;
				break;
			}
		}
		
		if (write_ok) {
			eeprom_var v;
			v.as_u32 = BALANCE_CONFIG_SIGNATURE;
			VESC_IF->store_eeprom_var(&v, 0);
		}
	}
	
	return res;
}

static int get_cfg_xml(uint8_t **buffer) {
	// Note: As the address of data_balance_config_ is not known
	// at compile time it will be relative to where it is in the
	// linked binary. Therefore we add PROG_ADDR to it so that it
	// points to where it ends up on the STM32.
	*buffer = data_balance_config_ + PROG_ADDR;
	return DATA_BALANCE_CONFIG__SIZE;
}

// Called when code is stopped
static void stop(void *arg) {
	(void)arg;
	VESC_IF->conf_custom_clear_configs();
}

INIT_FUN(lib_info *info) {
	INIT_START

	data *d = VESC_IF->malloc(sizeof(data));
	memset(d, 0, sizeof(data));
	
	// Read config from EEPROM if signature is correct
	eeprom_var v;
	uint32_t ints = sizeof(balance_config) / 4 + 1;
	uint32_t buffer[ints];
	bool read_ok = VESC_IF->read_eeprom_var(&v, 0);
	if (read_ok && v.as_u32 == BALANCE_CONFIG_SIGNATURE) {
		for (uint32_t i = 0;i < ints;i++) {
			if (!VESC_IF->read_eeprom_var(&v, i + 1)) {
				read_ok = false;
				break;
			}
			buffer[i] = v.as_u32;
		}
	} else {
		read_ok = false;
	}
	
	if (read_ok) {
		memcpy(&(d->balance_conf), buffer, sizeof(balance_config));
	} else {
		confparser_set_defaults_balance_config(&(d->balance_conf));
	}
	
	info->stop_fun = stop;	
	info->arg = d;
	
	VESC_IF->conf_custom_add_config(get_cfg, set_cfg, get_cfg_xml);

	return true;
}

