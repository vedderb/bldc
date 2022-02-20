/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Jakub Tomczak

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

#ifndef ENC_TS5700N8501_H_
#define ENC_TS5700N8501_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

bool enc_ts5700n8501_init(TS5700N8501_config_t *cfg);
void enc_ts5700n8501_deinit(TS5700N8501_config_t *cfg);

inline float enc_ts5700n8501_read_deg(TS5700N8501_config_t *cfg) {
	return cfg->state.last_enc_angle;
}

inline uint8_t* enc_ts5700n8501_get_raw_status(TS5700N8501_config_t *cfg) {
	return (uint8_t*)cfg->state.raw_status;
}

inline int16_t enc_ts5700n8501_get_abm(TS5700N8501_config_t *cfg) {
	return (uint16_t) cfg->state.raw_status[4]
			| ((uint16_t) cfg->state.raw_status[5] << 8);
}

inline void enc_ts5700n8501_reset_errors(TS5700N8501_config_t *cfg) {
	cfg->state.reset_errors = true;
}

inline void enc_ts5700n8501_reset_multiturn(TS5700N8501_config_t *cfg) {
	cfg->state.reset_multiturn = true;
}

#endif /* ENC_TS5700N8501_H_ */
