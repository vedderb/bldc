/*
	Copyright 2018 Benjamin Vedder	benjamin@vedder.se

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

#ifndef GPDRIVE_H_
#define GPDRIVE_H_

#include "conf_general.h"

// Functions
void gpdrive_init(volatile mc_configuration *configuration);
void gpdrive_deinit(void);
bool gpdrive_init_done(void);
bool gpdrive_is_dccal_done(void);
float gpdrive_get_switching_frequency_now(void);
void gpdrive_set_configuration(volatile mc_configuration *configuration);
void gpdrive_output_sample(float sample);
void gpdrive_fill_buffer(float *samples, int sample_num);
void gpdrive_add_buffer_sample(float sample);
void gpdrive_add_buffer_sample_int(int sample);
void gpdrive_set_buffer_int_scale(float scale);
void gpdrive_set_switching_frequency(float freq);
int gpdrive_buffer_size_left(void);
void gpdrive_set_mode(gpd_output_mode mode);
float gpdrive_get_current(void);
float gpdrive_get_current_filtered(void);
float gpdrive_get_modulation(void);
float gpdrive_get_last_adc_isr_duration(void);

#endif /* GPDRIVE_H_ */
