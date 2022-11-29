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

#include "conf_custom.h"
#include "datatypes.h"
#include "packet.h"
#include "mempools.h"
#include "buffer.h"
#include "utils_sys.h"

#include <string.h>

// Function pointers
static int (*m_get_cfg)(uint8_t *data, bool is_default) = 0;
static bool (*m_set_cfg)(uint8_t *data) = 0;
static int (*m_get_cfg_xml)(uint8_t **data) = 0;

void conf_custom_add_config(
		int (*get_cfg)(uint8_t *data, bool is_default),
		bool (*set_cfg)(uint8_t *data),
		int (*get_cfg_xml)(uint8_t **data)) {

	if (utils_is_func_valid(get_cfg) &&
			utils_is_func_valid(set_cfg) &&
			utils_is_func_valid(get_cfg_xml)) {
		m_get_cfg = get_cfg;
		m_set_cfg = set_cfg;
		m_get_cfg_xml = get_cfg_xml;
	}
}

void conf_custom_clear_configs(void) {
	m_get_cfg = 0;
	m_set_cfg = 0;
	m_get_cfg_xml = 0;
}

int conf_custom_cfg_num(void) {
	int res = 0;

	if (m_get_cfg_xml) {
		uint8_t *xml_data = 0;
		m_get_cfg_xml(&xml_data);

		if (utils_is_func_valid(xml_data)) {
			res = 1;
		}
	}

	return res;
}

void conf_custom_process_cmd(unsigned char *data, unsigned int len,
		void(*reply_func)(unsigned char *data, unsigned int len)) {
	COMM_PACKET_ID packet_id;

	packet_id = data[0];
	data++;
	len--;

	switch (packet_id) {

	case COMM_GET_CUSTOM_CONFIG:
	case COMM_GET_CUSTOM_CONFIG_DEFAULT: {
		int conf_ind = data[0];
		if (m_get_cfg && conf_ind == 0) {
			uint8_t *send_buffer = mempools_get_packet_buffer();
			int32_t ind = 0;
			send_buffer[ind++] = packet_id;
			send_buffer[ind++] = conf_ind;
			int32_t len_cfg = m_get_cfg(send_buffer + ind, packet_id == COMM_GET_CUSTOM_CONFIG_DEFAULT);
			ind += len_cfg;
			reply_func(send_buffer, ind);
			mempools_free_packet_buffer(send_buffer);
		}
	} break;

	case COMM_SET_CUSTOM_CONFIG: {
		int conf_ind = data[0];
		if (m_set_cfg && conf_ind == 0) {
			m_set_cfg(data + 1);
			int32_t ind = 0;
			uint8_t send_buffer[50];
			send_buffer[ind++] = packet_id;
			reply_func(send_buffer, ind);
		}
	} break;

	case COMM_GET_CUSTOM_CONFIG_XML: {
		int32_t ind = 0;

		int conf_ind = data[ind++];

		if (conf_ind != 0 || m_get_cfg_xml == 0) {
			break;
		}

		int32_t len_conf = buffer_get_int32(data, &ind);
		int32_t ofs_conf = buffer_get_int32(data, &ind);

		uint8_t *xml_data = 0;
		int xml_len = m_get_cfg_xml(&xml_data);

		if ((len_conf + ofs_conf) > xml_len || len_conf > (PACKET_MAX_PL_LEN - 10)) {
			break;
		}

		uint8_t *send_buffer = mempools_get_packet_buffer();
		ind = 0;
		send_buffer[ind++] = packet_id;
		send_buffer[ind++] = conf_ind;
		buffer_append_int32(send_buffer, xml_len, &ind);
		buffer_append_int32(send_buffer, ofs_conf, &ind);
		memcpy(send_buffer + ind, xml_data + ofs_conf, len_conf);
		ind += len_conf;
		reply_func(send_buffer, ind);

		mempools_free_packet_buffer(send_buffer);
	} break;

	default:
		break;
	}
}
