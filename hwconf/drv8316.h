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

#ifndef HWCONF_DRV8316_H_
#define HWCONF_DRV8316_H_

#include "datatypes.h"

// Functions
void drv8316_init(void);
void drv8316_set_oc_adj(int val);
void drv8316_set_oc_mode(drv8301_oc_mode mode);
void drv8316_set_current_amp_gain(int gain);
void drv8316_dccal_on(void);
void drv8316_dccal_off(void);
uint32_t drv8316_read_faults(void);
void drv8316_reset_faults(void);
char* drv8316_faults_to_string(unsigned long faults);
unsigned int drv8316_read_reg(int reg);
void drv8316_write_reg(uint8_t reg, uint8_t data);

#define HW_RESET_DRV_FAULTS()			drv8316_reset_faults()

#ifndef MCCONF_M_DRV8316_OC_MODE
#define MCCONF_M_DRV8316_OC_MODE		DRV8316_OC_LATCH_SHUTDOWN
#endif

// Register Definitions
#define DRV8316_IC_STATUS_REG       0x00
#define DRV8316_STATUS_REG_1        0x01
#define DRV8316_STATUS_REG_2        0x02
#define DRV8316_CTRL_REG_1		    0x03
#define DRV8316_CTRL_REG_2		    0x04
#define DRV8316_CTRL_REG_3		    0x05
#define DRV8316_CTRL_REG_4		    0x06
#define DRV8316_CTRL_REG_5		    0x07
#define DRV8316_CTRL_REG_6		    0x08

// IC Faults
#define DRV8316_IC_FAULT_BUCK_FLT           (1 << 6)
#define DRV8316_IC_FAULT_SPI_FLT            (1 << 5)
#define DRV8316_IC_FAULT_OCP                (1 << 4)
#define DRV8316_IC_FAULT_NPOR               (1 << 3)
#define DRV8316_IC_FAULT_OVP                (1 << 2)
#define DRV8316_IC_FAULT_OT                 (1 << 1)
#define DRV8316_IC_FAULT_DEVICE_FLT         (1 << 0)

// Status Register 1 Faults
#define DRV8316_FAULT_STS_1_OTW             (1 << 7)
#define DRV8316_FAULT_STS_1_OTS             (1 << 6)
#define DRV8316_FAULT_STS_1_OCP_HC          (1 << 5)
#define DRV8316_FAULT_STS_1_OCP_LC	        (1 << 4)
#define DRV8316_FAULT_STS_1_OCP_HB          (1 << 3)
#define DRV8316_FAULT_STS_1_OCP_LB          (1 << 2)
#define DRV8316_FAULT_STS_1_OCP_HA          (1 << 1)
#define DRV8316_FAULT_STS_1_OCP_LA 	        (1 << 0)

// Status Register 2 Faults
#define DRV8316_FAULT_STS_2_OTP_ERR         (1 << 6)
#define DRV8316_FAULT_STS_2_BUCK_OCP        (1 << 5)
#define DRV8316_FAULT_STS_2_BUCK_UV	        (1 << 4)
#define DRV8316_FAULT_STS_2_VCP_UV 		    (1 << 3)
#define DRV8316_FAULT_STS_2_SPI_PARITY      (1 << 2)
#define DRV8316_FAULT_STS_2_SPI_SCLK_FLT    (1 << 1)
#define DRV8316_FAULT_STS_2_SPI_ADDR_FLT 	(1 << 0)

#endif /* HWCONF_DRV8316_H_ */
