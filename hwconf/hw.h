/*
	Copyright 2012-2017 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

/*
 * hw.h
 *
 *  Created on: 12 apr 2014
 *      Author: benjamin
 */

#ifndef HW_H_
#define HW_H_

#include "conf_general.h"
#include "stm32f4xx_conf.h"

#ifdef HW_VERSION_40
#include "hw_40.h"
#elif defined HW_VERSION_45
#include "hw_45.h"
#elif defined HW_VERSION_46
#include "hw_46.h"
#elif defined HW_VERSION_48
#include "hw_48.h"
#elif defined HW_VERSION_49
#include "hw_49.h"
#elif defined HW_VERSION_410
#include "hw_410.h"
#elif defined HW_VERSION_60
#include "hw_60.h"
#elif defined HW_VERSION_R2
#include "hw_r2.h"
#elif defined HW_VERSION_VICTOR_R1A
#include "hw_victor_r1a.h"
#elif defined HW_VERSION_DAS_RS
#include "hw_das_rs.h"
#elif defined HW_VERSION_PALTA
#include "hw_palta.h"
#elif defined HW_VERSION_RH
#include "hw_rh.h"
#elif defined HW_VERSION_TP
#include "hw_tp.h"
#elif defined HW_VERSION_75_300
#include "hw_75_300.h"
#elif defined HW_VERSION_MINI4
#include "hw_mini4.h"
#elif defined HW_VERSION_DAS_MINI
#include "hw_das_mini.h"
#else
#error "No hardware version defined"
#endif

// Default empty macros in case there is no hardware support
#ifndef ENABLE_GATE
#define ENABLE_GATE()
#endif
#ifndef DISABLE_GATE
#define DISABLE_GATE()
#endif
#ifndef DCCAL_ON
#define DCCAL_ON()
#endif
#ifndef DCCAL_OFF
#define DCCAL_OFF()
#endif
#ifndef IS_DRV_FAULT
#define IS_DRV_FAULT()			0
#endif
#ifndef AUX_ON
#define AUX_ON()
#endif
#ifndef AUX_OFF
#define AUX_OFF()
#endif

// Functions
void hw_init_gpio(void);
void hw_setup_adc_channels(void);
void hw_start_i2c(void);
void hw_stop_i2c(void);
void hw_try_restore_i2c(void);

#endif /* HW_H_ */
