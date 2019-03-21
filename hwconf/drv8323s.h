/*
	Copyright 2017 Benjamin Vedder	benjamin@vedder.se

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

#ifndef HWCONF_DRV8323S_H_
#define HWCONF_DRV8323S_H_

#include "datatypes.h"

// Functions
void drv8323s_init(void);
void drv8323s_set_oc_adj(int val);
void drv8323s_set_oc_mode(drv8301_oc_mode mode);
void drv8323s_set_current_amp_gain(int gain);
void drv8323s_dccal_on(void);
void drv8323s_dccal_off(void);
unsigned long drv8323s_read_faults(void);
void drv8323s_reset_faults(void);
char* drv8323s_faults_to_string(unsigned long faults);
unsigned int drv8323s_read_reg(int reg);
void drv8323s_write_reg(int reg, int data);

// Defines
#define DRV8323S_FAULT_FET_LC_OC		(1 << 0)
#define DRV8323S_FAULT_FET_HC_OC		(1 << 1)
#define DRV8323S_FAULT_FET_LB_OC		(1 << 2)
#define DRV8323S_FAULT_FET_HB_OC		(1 << 3)
#define DRV8323S_FAULT_FET_LA_OC		(1 << 4)
#define DRV8323S_FAULT_FET_HA_OC		(1 << 5)
#define DRV8323S_FAULT_OTSD			    (1 << 6)
#define DRV8323S_FAULT_UVLO			    (1 << 7)
#define DRV8323S_FAULT_GDF   		    (1 << 8)
#define DRV8323S_FAULT_VDS_OCP		    (1 << 9)
#define DRV8323S_FAULT_FAULT			(1 << 10)

#define DRV8323S_FAULT_VGS_LC        (1 << 16)
#define DRV8323S_FAULT_VGS_HC        (1 << 17)
#define DRV8323S_FAULT_VGS_LB        (1 << 18)
#define DRV8323S_FAULT_VGS_HB        (1 << 19)
#define DRV8323S_FAULT_VGS_LA        (1 << 20)
#define DRV8323S_FAULT_VGS_HA        (1 << 21)
#define DRV8323S_FAULT_CPUV          (1 << 22)
#define DRV8323S_FAULT_OTW           (1 << 23)
#define DRV8323S_FAULT_SC_OC         (1 << 24)
#define DRV8323S_FAULT_SB_OC         (1 << 25)
#define DRV8323S_FAULT_SA_OC         (1 << 26)

#endif /* HWCONF_DRV8323S_H_ */