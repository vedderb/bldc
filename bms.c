/*
	Copyright 2020 Benjamin Vedder	benjamin@vedder.se

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

/**
 * This is the BMS module of the VESC firmware. It mainly supports the VESC BMS, but
 * the intention is to have it extendible to other BMSs too. The first step is
 * to add the BMS you want to support to the BMS_TYPE enum, and then you need to update
 * this module to interpret CAN-messages from it properly.
 */

#include "bms.h"
#include "buffer.h"
#include "utils.h"
#include "datatypes.h"
#include "comm_can.h"
#include <string.h>
#include <math.h>

// Settings
#define MAX_CAN_AGE_SEC				2.0

// Private variables
static volatile bms_config m_conf;
static volatile bms_values m_values;
static volatile bms_soc_soh_temp_stat m_stat_temp_max;
static volatile bms_soc_soh_temp_stat m_stat_soc_min;
static volatile bms_soc_soh_temp_stat m_stat_soc_max;

void bms_init(bms_config *conf) {
	m_conf = *conf;
	memset((void*)&m_values, 0, sizeof(m_values));
	memset((void*)&m_stat_temp_max, 0, sizeof(m_stat_temp_max));
	memset((void*)&m_stat_soc_min, 0, sizeof(m_stat_soc_min));
	memset((void*)&m_stat_soc_max, 0, sizeof(m_stat_soc_max));
	memset((void*)&m_values, 0, sizeof(m_values));
	m_values.can_id = -1;
	m_stat_temp_max.id = -1;
	m_stat_soc_min.id = -1;
	m_stat_soc_max.id = -1;
}

bool bms_process_can_frame(uint32_t can_id, uint8_t *data8, int len, bool is_ext) {
	bool used_data = false;

	if (m_conf.type == BMS_TYPE_VESC) {
		if (is_ext) {
			uint8_t id = can_id & 0xFF;
			CAN_PACKET_ID cmd = can_id >> 8;

			switch (cmd) {
			case CAN_PACKET_BMS_SOC_SOH_TEMP_STAT: {
				used_data = true;

				int32_t ind = 0;
				bms_soc_soh_temp_stat msg;
				msg.id = id;
				msg.rx_time = chVTGetSystemTime();
				msg.v_cell_min = buffer_get_float16(data8, 1e3, &ind);
				msg.v_cell_max = buffer_get_float16(data8, 1e3, &ind);
				msg.soc = ((float)((uint8_t)data8[ind++])) / 255.0;
				msg.soh = ((float)((uint8_t)data8[ind++])) / 255.0;
				msg.t_cell_max = (float)((int8_t)data8[ind++]);
				uint8_t stat = data8[ind++];
				msg.is_charging = (stat >> 0) & 1;
				msg.is_balancing = (stat >> 1) & 1;
				msg.is_charge_allowed = (stat >> 2) & 1;

				if (id == m_values.can_id || UTILS_AGE_S(m_values.update_time) > MAX_CAN_AGE_SEC) {
					m_values.can_id = id;
					m_values.update_time = chVTGetSystemTimeX();
					m_values.soc = msg.soc;
					m_values.soh = msg.soh;
					m_values.temp_max_cell = msg.t_cell_max;
				}

				// In case there is more than one BMS, keep track of the limiting
				// values for all of them.

				if (m_stat_temp_max.id < 0 ||
						UTILS_AGE_S(m_stat_temp_max.rx_time) > MAX_CAN_AGE_SEC ||
						m_stat_temp_max.t_cell_max < msg.t_cell_max) {
					m_stat_temp_max = msg;
				} else if (m_stat_temp_max.id == msg.id) {
					m_stat_temp_max = msg;
				}

				if (m_stat_soc_min.id < 0 ||
						UTILS_AGE_S(m_stat_soc_min.rx_time) > MAX_CAN_AGE_SEC ||
						m_stat_soc_min.soc > msg.soc) {
					m_stat_soc_min = msg;
				} else if (m_stat_soc_min.id == msg.id) {
					m_stat_soc_min = msg;
				}

				if (m_stat_soc_max.id < 0 ||
						UTILS_AGE_S(m_stat_soc_max.rx_time) > MAX_CAN_AGE_SEC ||
						m_stat_soc_max.soc < msg.soc) {
					m_stat_soc_max = msg;
				} else if (m_stat_soc_max.id == msg.id) {
					m_stat_soc_max = msg;
				}
			} break;

			case CAN_PACKET_BMS_V_TOT: {
				used_data = true;

				if (id == m_values.can_id || UTILS_AGE_S(m_values.update_time) > MAX_CAN_AGE_SEC) {
					int32_t ind = 0;
					m_values.can_id = id;
					m_values.update_time = chVTGetSystemTimeX();
					m_values.v_tot = buffer_get_float32_auto(data8, &ind);
					m_values.v_charge = buffer_get_float32_auto(data8, &ind);
				}
			} break;

			case CAN_PACKET_BMS_I: {
				used_data = true;

				if (id == m_values.can_id || UTILS_AGE_S(m_values.update_time) > MAX_CAN_AGE_SEC) {
					int32_t ind = 0;
					m_values.can_id = id;
					m_values.update_time = chVTGetSystemTimeX();
					m_values.i_in = buffer_get_float32_auto(data8, &ind);
					m_values.i_in_ic = buffer_get_float32_auto(data8, &ind);
				}
			} break;

			case CAN_PACKET_BMS_AH_WH: {
				used_data = true;

				if (id == m_values.can_id || UTILS_AGE_S(m_values.update_time) > MAX_CAN_AGE_SEC) {
					int32_t ind = 0;
					m_values.can_id = id;
					m_values.update_time = chVTGetSystemTimeX();
					m_values.ah_cnt = buffer_get_float32_auto(data8, &ind);
					m_values.wh_cnt = buffer_get_float32_auto(data8, &ind);
				}
			} break;

			case CAN_PACKET_BMS_V_CELL: {
				used_data = true;

				if (id == m_values.can_id || UTILS_AGE_S(m_values.update_time) > MAX_CAN_AGE_SEC) {
					int32_t ind = 0;
					m_values.can_id = id;
					m_values.update_time = chVTGetSystemTimeX();
					unsigned int ofs = data8[ind++];
					m_values.cell_num = data8[ind++];

					while(ind < len) {
						if (ofs >= (sizeof(m_values.v_cell) / sizeof(float))) {
							// Out of buffer space
							break;
						}

						m_values.v_cell[ofs++] = buffer_get_float16(data8, 1e3, &ind);
					}
				}
			} break;

			case CAN_PACKET_BMS_BAL: {
				used_data = true;

				if (id == m_values.can_id || UTILS_AGE_S(m_values.update_time) > MAX_CAN_AGE_SEC) {
					int32_t ind = 0;
					m_values.can_id = id;
					m_values.update_time = chVTGetSystemTimeX();

					int cell_num = data8[0];
					uint64_t bal_state_0 = buffer_get_uint32(data8, &ind);
					bal_state_0 &= 0x00FFFFFF;
					uint64_t bal_state_1 = buffer_get_uint32(data8, &ind);
					uint64_t bal_state = bal_state_0 << 32 | bal_state_1;
					ind = 0;

					while (ind < (int)(sizeof(m_values.bal_state) / sizeof(bool)) && ind < cell_num) {
						m_values.bal_state[ind] = (bal_state >> ind) & 1;
						ind++;
					}
				}
			} break;

			case CAN_PACKET_BMS_TEMPS: {
				used_data = true;

				if (id == m_values.can_id || UTILS_AGE_S(m_values.update_time) > MAX_CAN_AGE_SEC) {
					int32_t ind = 0;
					m_values.can_id = id;
					m_values.update_time = chVTGetSystemTimeX();
					unsigned int ofs = data8[ind++];
					m_values.temp_adc_num = data8[ind++];

					while(ind < len) {
						if (ofs >= (sizeof(m_values.temps_adc) / sizeof(float))) {
							// Out of buffer space
							break;
						}

						m_values.temps_adc[ofs++] = buffer_get_float16(data8, 1e2, &ind);
					}
				}
			} break;

			case CAN_PACKET_BMS_HUM: {
				used_data = true;

				if (id == m_values.can_id || UTILS_AGE_S(m_values.update_time) > MAX_CAN_AGE_SEC) {
					int32_t ind = 0;
					m_values.can_id = id;
					m_values.update_time = chVTGetSystemTimeX();
					m_values.temp_hum = buffer_get_float16(data8, 1e2, &ind);
					m_values.hum = buffer_get_float16(data8, 1e2, &ind);
					m_values.temp_ic = buffer_get_float16(data8, 1e2, &ind);
				}
			} break;

			default:
				break;
			}
		}
	}

	return used_data;
}

void bms_update_limits(float *i_in_min, float *i_in_max,
		float i_in_min_conf, float i_in_max_conf) {
	float i_in_min_bms = i_in_min_conf;
	float i_in_max_bms = i_in_max_conf;

	// Temperature
	if (UTILS_AGE_S(m_stat_temp_max.rx_time) < MAX_CAN_AGE_SEC) {
		float temp = m_stat_temp_max.t_cell_max;

		if (temp < m_conf.t_limit_start) {
			// OK
		} else if (temp > m_conf.t_limit_end) {
			i_in_min_bms = 0.0;
			i_in_max_bms = 0.0;
			// Maybe add fault code?
//			mc_interface_fault_stop(FAULT_CODE_OVER_TEMP_FET, false, false);
		} else {
			float maxc = fabsf(i_in_max_conf);
			if (fabsf(i_in_min_conf) > maxc) {
				maxc = fabsf(i_in_min_conf);
			}

			maxc = utils_map(temp, m_conf.t_limit_start, m_conf.t_limit_end, maxc, 0.0);

			if (fabsf(i_in_min_bms) > maxc) {
				i_in_min_bms = SIGN(i_in_min_bms) * maxc;
			}

			if (fabsf(i_in_max_bms) > maxc) {
				i_in_max_bms = SIGN(i_in_max_bms) * maxc;
			}
		}
	}

	// TODO: add support for conf->l_temp_accel_dec to still have braking.

	// SOC
	if (UTILS_AGE_S(m_stat_soc_min.rx_time) < MAX_CAN_AGE_SEC) {
		float soc = m_stat_soc_min.soc;

		if (soc > m_conf.soc_limit_start) {
			// OK
		} else if (soc < m_conf.soc_limit_end) {
			i_in_max_bms = 0.0;
		} else {
			i_in_max_bms = utils_map(soc, m_conf.soc_limit_start,
					m_conf.soc_limit_end, i_in_max_conf, 0.0);
		}
	}

	if (fabsf(i_in_min_bms) < fabsf(*i_in_min)) {
		*i_in_min = i_in_min_bms;
	}

	if (fabsf(i_in_max_bms) < fabsf(*i_in_max)) {
		*i_in_max = i_in_max_bms;
	}
}

void bms_process_cmd(unsigned char *data, unsigned int len,
		void(*reply_func)(unsigned char *data, unsigned int len)) {
	COMM_PACKET_ID packet_id;

	packet_id = data[0];
	data++;
	len--;

	switch (packet_id) {
	case COMM_BMS_GET_VALUES: {
		int32_t ind = 0;
		uint8_t send_buffer[128];

		send_buffer[ind++] = packet_id;

		buffer_append_float32(send_buffer, m_values.v_tot, 1e6, &ind);
		buffer_append_float32(send_buffer, m_values.v_charge, 1e6, &ind);
		buffer_append_float32(send_buffer, m_values.i_in, 1e6, &ind);
		buffer_append_float32(send_buffer, m_values.i_in_ic, 1e6, &ind);
		buffer_append_float32(send_buffer, m_values.ah_cnt, 1e3, &ind);
		buffer_append_float32(send_buffer, m_values.wh_cnt, 1e3, &ind);

		// Cell voltages
		send_buffer[ind++] = m_values.cell_num;
		for (int i = 0;i < m_values.cell_num;i++) {
			buffer_append_float16(send_buffer, m_values.v_cell[i], 1e3, &ind);
		}

		// Balancing state
		for (int i = 0;i < m_values.cell_num;i++) {
			send_buffer[ind++] = m_values.bal_state[i];
		}

		// Temperatures
		send_buffer[ind++] = m_values.temp_adc_num;
		for (int i = 0;i < m_values.temp_adc_num;i++) {
			buffer_append_float16(send_buffer, m_values.temps_adc[i], 1e2, &ind);
		}
		buffer_append_float16(send_buffer, m_values.temp_ic, 1e2, &ind);

		// Humidity
		buffer_append_float16(send_buffer, m_values.temp_hum, 1e2, &ind);
		buffer_append_float16(send_buffer, m_values.hum, 1e2, &ind);

		// Highest cell temperature
		buffer_append_float16(send_buffer, m_values.temp_max_cell, 1e2, &ind);

		// State of charge and state of health
		buffer_append_float16(send_buffer, m_values.soc, 1e3, &ind);
		buffer_append_float16(send_buffer, m_values.soh, 1e3, &ind);

		reply_func(send_buffer, ind);
	} break;

	default:
		break;
	}

	if (m_conf.type == BMS_TYPE_VESC && UTILS_AGE_S(m_values.update_time) < MAX_CAN_AGE_SEC) {
		switch (packet_id) {
		case COMM_BMS_SET_CHARGE_ALLOWED:
		case COMM_BMS_SET_BALANCE_OVERRIDE:
		case COMM_BMS_RESET_COUNTERS:
		case COMM_BMS_FORCE_BALANCE:
		case COMM_BMS_ZERO_CURRENT_OFFSET: {
			comm_can_send_buffer(m_values.can_id, data - 1, len + 1, 0);
		} break;

		default:
			break;
		}
	}

}

bms_values *bms_get_values(void) {
	return (bms_values*)&m_values;
}
