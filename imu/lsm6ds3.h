/*
	Copyright 2020 Mitch Lustig

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


	**Note** Most of the constants were copied from...

	Marshall Taylor @ SparkFun Electronics
	May 20, 2015
	https://github.com/sparkfun/LSM6DS3_Breakout
	https://github.com/sparkfun/SparkFun_LSM6DS3_Arduino_Library

	That code is released under the [MIT License](http://opensource.org/licenses/MIT).
	Please review the LICENSE.md file included with that example. If you have any questions
	or concerns with licensing, please contact techsupport@sparkfun.com.
	Distributed as-is; no warranty is given.
	*/

#ifndef LSM6DS3_H_
#define LSM6DS3_H_

#include "ch.h"
#include "hal.h"

void lsm6ds3_set_rate_hz(int hz);
void lsm6ds3_init(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin,
		stkalign_t *work_area, size_t work_area_size);
void lsm6ds3_set_read_callback(void(*func)(float *accel, float *gyro, float *mag));
void lsm6ds3_stop(void);


#define LSM6DS3_ACC_GYRO_ADDR_A  				0X6A
#define LSM6DS3_ACC_GYRO_ADDR_B 				0X6B

/************** Device Register  *******************/
#define LSM6DS3_ACC_GYRO_TEST_PAGE  			0X00
#define LSM6DS3_ACC_GYRO_RAM_ACCESS  			0X01
#define LSM6DS3_ACC_GYRO_SENSOR_SYNC_TIME  		0X04
#define LSM6DS3_ACC_GYRO_SENSOR_SYNC_EN  		0X05
#define LSM6DS3_ACC_GYRO_FIFO_CTRL1  			0X06
#define LSM6DS3_ACC_GYRO_FIFO_CTRL2  			0X07
#define LSM6DS3_ACC_GYRO_FIFO_CTRL3  			0X08
#define LSM6DS3_ACC_GYRO_FIFO_CTRL4  			0X09
#define LSM6DS3_ACC_GYRO_FIFO_CTRL5  			0X0A
#define LSM6DS3_ACC_GYRO_ORIENT_CFG_G  			0X0B
#define LSM6DS3_ACC_GYRO_REFERENCE_G  			0X0C
#define LSM6DS3_ACC_GYRO_INT1_CTRL  			0X0D
#define LSM6DS3_ACC_GYRO_INT2_CTRL  			0X0E
#define LSM6DS3_ACC_GYRO_WHO_AM_I_REG  			0X0F
#define LSM6DS3_ACC_GYRO_CTRL1_XL  			0X10
#define LSM6DS3_ACC_GYRO_CTRL2_G  			0X11
#define LSM6DS3_ACC_GYRO_CTRL3_C  			0X12
#define LSM6DS3_ACC_GYRO_CTRL4_C  			0X13
#define LSM6DS3_ACC_GYRO_CTRL5_C  			0X14
#define LSM6DS3_ACC_GYRO_CTRL6_G  			0X15
#define LSM6DS3_ACC_GYRO_CTRL7_G  			0X16
#define LSM6DS3_ACC_GYRO_CTRL8_XL  			0X17
#define LSM6DS3_ACC_GYRO_CTRL9_XL  			0X18
#define LSM6DS3_ACC_GYRO_CTRL10_C  			0X19
#define LSM6DS3_ACC_GYRO_MASTER_CONFIG  		0X1A
#define LSM6DS3_ACC_GYRO_WAKE_UP_SRC  			0X1B
#define LSM6DS3_ACC_GYRO_TAP_SRC  			0X1C
#define LSM6DS3_ACC_GYRO_D6D_SRC  			0X1D
#define LSM6DS3_ACC_GYRO_STATUS_REG  			0X1E
#define LSM6DS3_ACC_GYRO_OUT_TEMP_L  			0X20
#define LSM6DS3_ACC_GYRO_OUT_TEMP_H  			0X21
#define LSM6DS3_ACC_GYRO_OUTX_L_G  			0X22
#define LSM6DS3_ACC_GYRO_OUTX_H_G  			0X23
#define LSM6DS3_ACC_GYRO_OUTY_L_G  			0X24
#define LSM6DS3_ACC_GYRO_OUTY_H_G  			0X25
#define LSM6DS3_ACC_GYRO_OUTZ_L_G  			0X26
#define LSM6DS3_ACC_GYRO_OUTZ_H_G  			0X27
#define LSM6DS3_ACC_GYRO_OUTX_L_XL  			0X28
#define LSM6DS3_ACC_GYRO_OUTX_H_XL  			0X29
#define LSM6DS3_ACC_GYRO_OUTY_L_XL  			0X2A
#define LSM6DS3_ACC_GYRO_OUTY_H_XL  			0X2B
#define LSM6DS3_ACC_GYRO_OUTZ_L_XL  			0X2C
#define LSM6DS3_ACC_GYRO_OUTZ_H_XL  			0X2D
#define LSM6DS3_ACC_GYRO_SENSORHUB1_REG  		0X2E
#define LSM6DS3_ACC_GYRO_SENSORHUB2_REG  		0X2F
#define LSM6DS3_ACC_GYRO_SENSORHUB3_REG  		0X30
#define LSM6DS3_ACC_GYRO_SENSORHUB4_REG  		0X31
#define LSM6DS3_ACC_GYRO_SENSORHUB5_REG  		0X32
#define LSM6DS3_ACC_GYRO_SENSORHUB6_REG  		0X33
#define LSM6DS3_ACC_GYRO_SENSORHUB7_REG  		0X34
#define LSM6DS3_ACC_GYRO_SENSORHUB8_REG  		0X35
#define LSM6DS3_ACC_GYRO_SENSORHUB9_REG  		0X36
#define LSM6DS3_ACC_GYRO_SENSORHUB10_REG  		0X37
#define LSM6DS3_ACC_GYRO_SENSORHUB11_REG  		0X38
#define LSM6DS3_ACC_GYRO_SENSORHUB12_REG  		0X39
#define LSM6DS3_ACC_GYRO_FIFO_STATUS1  			0X3A
#define LSM6DS3_ACC_GYRO_FIFO_STATUS2  			0X3B
#define LSM6DS3_ACC_GYRO_FIFO_STATUS3  			0X3C
#define LSM6DS3_ACC_GYRO_FIFO_STATUS4  			0X3D
#define LSM6DS3_ACC_GYRO_FIFO_DATA_OUT_L  		0X3E
#define LSM6DS3_ACC_GYRO_FIFO_DATA_OUT_H  		0X3F
#define LSM6DS3_ACC_GYRO_TIMESTAMP0_REG  		0X40
#define LSM6DS3_ACC_GYRO_TIMESTAMP1_REG  		0X41
#define LSM6DS3_ACC_GYRO_TIMESTAMP2_REG  		0X42
#define LSM6DS3_ACC_GYRO_STEP_COUNTER_L  		0X4B
#define LSM6DS3_ACC_GYRO_STEP_COUNTER_H  		0X4C
#define LSM6DS3_ACC_GYRO_FUNC_SRC  			0X53
#define LSM6DS3_ACC_GYRO_TAP_CFG1  			0X58
#define LSM6DS3_ACC_GYRO_TAP_THS_6D  			0X59
#define LSM6DS3_ACC_GYRO_INT_DUR2  			0X5A
#define LSM6DS3_ACC_GYRO_WAKE_UP_THS  			0X5B
#define LSM6DS3_ACC_GYRO_WAKE_UP_DUR  			0X5C
#define LSM6DS3_ACC_GYRO_FREE_FALL  			0X5D
#define LSM6DS3_ACC_GYRO_MD1_CFG  			0X5E
#define LSM6DS3_ACC_GYRO_MD2_CFG  			0X5F

/************** Access Device RAM  *******************/
#define LSM6DS3_ACC_GYRO_ADDR0_TO_RW_RAM         0x62
#define LSM6DS3_ACC_GYRO_ADDR1_TO_RW_RAM         0x63
#define LSM6DS3_ACC_GYRO_DATA_TO_WR_RAM          0x64
#define LSM6DS3_ACC_GYRO_DATA_RD_FROM_RAM        0x65

#define LSM6DS3_ACC_GYRO_RAM_SIZE                4096

/************** Embedded functions register mapping  *******************/
#define LSM6DS3_ACC_GYRO_SLV0_ADD                     0x02
#define LSM6DS3_ACC_GYRO_SLV0_SUBADD                  0x03
#define LSM6DS3_ACC_GYRO_SLAVE0_CONFIG                0x04
#define LSM6DS3_ACC_GYRO_SLV1_ADD                     0x05
#define LSM6DS3_ACC_GYRO_SLV1_SUBADD                  0x06
#define LSM6DS3_ACC_GYRO_SLAVE1_CONFIG                0x07
#define LSM6DS3_ACC_GYRO_SLV2_ADD                     0x08
#define LSM6DS3_ACC_GYRO_SLV2_SUBADD                  0x09
#define LSM6DS3_ACC_GYRO_SLAVE2_CONFIG                0x0A
#define LSM6DS3_ACC_GYRO_SLV3_ADD                     0x0B
#define LSM6DS3_ACC_GYRO_SLV3_SUBADD                  0x0C
#define LSM6DS3_ACC_GYRO_SLAVE3_CONFIG                0x0D
#define LSM6DS3_ACC_GYRO_DATAWRITE_SRC_MODE_SUB_SLV0  0x0E
#define LSM6DS3_ACC_GYRO_CONFIG_PEDO_THS_MIN          0x0F
#define LSM6DS3_ACC_GYRO_CONFIG_TILT_IIR              0x10
#define LSM6DS3_ACC_GYRO_CONFIG_TILT_ACOS             0x11
#define LSM6DS3_ACC_GYRO_CONFIG_TILT_WTIME            0x12
#define LSM6DS3_ACC_GYRO_SM_STEP_THS                  0x13
#define LSM6DS3_ACC_GYRO_MAG_SI_XX                    0x24
#define LSM6DS3_ACC_GYRO_MAG_SI_XY                    0x25
#define LSM6DS3_ACC_GYRO_MAG_SI_XZ                    0x26
#define LSM6DS3_ACC_GYRO_MAG_SI_YX                    0x27
#define LSM6DS3_ACC_GYRO_MAG_SI_YY                    0x28
#define LSM6DS3_ACC_GYRO_MAG_SI_YZ                    0x29
#define LSM6DS3_ACC_GYRO_MAG_SI_ZX                    0x2A
#define LSM6DS3_ACC_GYRO_MAG_SI_ZY                    0x2B
#define LSM6DS3_ACC_GYRO_MAG_SI_ZZ                    0x2C
#define LSM6DS3_ACC_GYRO_MAG_OFFX_L                   0x2D
#define LSM6DS3_ACC_GYRO_MAG_OFFX_H                   0x2E
#define LSM6DS3_ACC_GYRO_MAG_OFFY_L                   0x2F
#define LSM6DS3_ACC_GYRO_MAG_OFFY_H                   0x30
#define LSM6DS3_ACC_GYRO_MAG_OFFZ_L                   0x31
#define LSM6DS3_ACC_GYRO_MAG_OFFZ_H                   0x32

/*******************************************************************************
* Register      : TEST_PAGE
* Address       : 0X00
* Bit Group Name: FLASH_PAGE
* Permission    : RW
*******************************************************************************/
#define LSM6DS3_FLASH_PAGE    0x40

/*******************************************************************************
* Register      : RAM_ACCESS
* Address       : 0X01
* Bit Group Name: PROG_RAM1
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_PROG_RAM1_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_PROG_RAM1_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_PROG_RAM1_t;

/*******************************************************************************
* Register      : RAM_ACCESS
* Address       : 0X01
* Bit Group Name: CUSTOMROM1
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_CUSTOMROM1_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_CUSTOMROM1_ENABLED 		 = 0x04,
} LSM6DS3_ACC_GYRO_CUSTOMROM1_t;

/*******************************************************************************
* Register      : RAM_ACCESS
* Address       : 0X01
* Bit Group Name: RAM_PAGE
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_RAM_PAGE_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_RAM_PAGE_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_RAM_PAGE_t;

/*******************************************************************************
* Register      : SENSOR_SYNC_TIME
* Address       : 0X04
* Bit Group Name: TPH
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_TPH_MASK  	0xFF
#define  	LSM6DS3_ACC_GYRO_TPH_POSITION  	0

/*******************************************************************************
* Register      : SENSOR_SYNC_EN
* Address       : 0X05
* Bit Group Name: SYNC_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SYNC_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_SYNC_EN_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_SYNC_EN_t;

/*******************************************************************************
* Register      : SENSOR_SYNC_EN
* Address       : 0X05
* Bit Group Name: HP_RST
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_HP_RST_RST_OFF 		 = 0x00,
	LSM6DS3_ACC_GYRO_HP_RST_RST_ON 		 = 0x02,
} LSM6DS3_ACC_GYRO_HP_RST_t;

/*******************************************************************************
* Register      : FIFO_CTRL1
* Address       : 0X06
* Bit Group Name: WTM_FIFO
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_WTM_FIFO_CTRL1_MASK  	0xFF
#define  	LSM6DS3_ACC_GYRO_WTM_FIFO_CTRL1_POSITION  	0
#define  	LSM6DS3_ACC_GYRO_WTM_FIFO_CTRL2_MASK  	0x0F
#define  	LSM6DS3_ACC_GYRO_WTM_FIFO_CTRL2_POSITION  	0

/*******************************************************************************
* Register      : FIFO_CTRL2
* Address       : 0X07
* Bit Group Name: TIM_PEDO_FIFO_DRDY
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TIM_PEDO_FIFO_DRDY_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_TIM_PEDO_FIFO_DRDY_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_TIM_PEDO_FIFO_DRDY_t;

/*******************************************************************************
* Register      : FIFO_CTRL2
* Address       : 0X07
* Bit Group Name: TIM_PEDO_FIFO_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TIM_PEDO_FIFO_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_TIM_PEDO_FIFO_EN_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_TIM_PEDO_FIFO_EN_t;

/*******************************************************************************
* Register      : FIFO_CTRL3
* Address       : 0X08
* Bit Group Name: DEC_FIFO_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DEC_FIFO_XL_DATA_NOT_IN_FIFO 		 = 0x00,
	LSM6DS3_ACC_GYRO_DEC_FIFO_XL_NO_DECIMATION 		 = 0x01,
	LSM6DS3_ACC_GYRO_DEC_FIFO_XL_DECIMATION_BY_2 		 = 0x02,
	LSM6DS3_ACC_GYRO_DEC_FIFO_XL_DECIMATION_BY_3 		 = 0x03,
	LSM6DS3_ACC_GYRO_DEC_FIFO_XL_DECIMATION_BY_4 		 = 0x04,
	LSM6DS3_ACC_GYRO_DEC_FIFO_XL_DECIMATION_BY_8 		 = 0x05,
	LSM6DS3_ACC_GYRO_DEC_FIFO_XL_DECIMATION_BY_16 		 = 0x06,
	LSM6DS3_ACC_GYRO_DEC_FIFO_XL_DECIMATION_BY_32 		 = 0x07,
} LSM6DS3_ACC_GYRO_DEC_FIFO_XL_t;

/*******************************************************************************
* Register      : FIFO_CTRL3
* Address       : 0X08
* Bit Group Name: DEC_FIFO_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DEC_FIFO_G_DATA_NOT_IN_FIFO 		 = 0x00,
	LSM6DS3_ACC_GYRO_DEC_FIFO_G_NO_DECIMATION 		 = 0x08,
	LSM6DS3_ACC_GYRO_DEC_FIFO_G_DECIMATION_BY_2 		 = 0x10,
	LSM6DS3_ACC_GYRO_DEC_FIFO_G_DECIMATION_BY_3 		 = 0x18,
	LSM6DS3_ACC_GYRO_DEC_FIFO_G_DECIMATION_BY_4 		 = 0x20,
	LSM6DS3_ACC_GYRO_DEC_FIFO_G_DECIMATION_BY_8 		 = 0x28,
	LSM6DS3_ACC_GYRO_DEC_FIFO_G_DECIMATION_BY_16 		 = 0x30,
	LSM6DS3_ACC_GYRO_DEC_FIFO_G_DECIMATION_BY_32 		 = 0x38,
} LSM6DS3_ACC_GYRO_DEC_FIFO_G_t;

/*******************************************************************************
* Register      : FIFO_CTRL4
* Address       : 0X09
* Bit Group Name: DEC_FIFO_SLV0
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV0_DATA_NOT_IN_FIFO 		 = 0x00,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV0_NO_DECIMATION 		 = 0x01,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV0_DECIMATION_BY_2 		 = 0x02,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV0_DECIMATION_BY_3 		 = 0x03,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV0_DECIMATION_BY_4 		 = 0x04,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV0_DECIMATION_BY_8 		 = 0x05,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV0_DECIMATION_BY_16 		 = 0x06,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV0_DECIMATION_BY_32 		 = 0x07,
} LSM6DS3_ACC_GYRO_DEC_FIFO_SLV0_t;

/*******************************************************************************
* Register      : FIFO_CTRL4
* Address       : 0X09
* Bit Group Name: DEC_FIFO_SLV1
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV1_DATA_NOT_IN_FIFO 		 = 0x00,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV1_NO_DECIMATION 		 = 0x08,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV1_DECIMATION_BY_2 		 = 0x10,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV1_DECIMATION_BY_3 		 = 0x18,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV1_DECIMATION_BY_4 		 = 0x20,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV1_DECIMATION_BY_8 		 = 0x28,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV1_DECIMATION_BY_16 		 = 0x30,
	LSM6DS3_ACC_GYRO_DEC_FIFO_SLV1_DECIMATION_BY_32 		 = 0x38,
} LSM6DS3_ACC_GYRO_DEC_FIFO_SLV1_t;

/*******************************************************************************
* Register      : FIFO_CTRL4
* Address       : 0X09
* Bit Group Name: HI_DATA_ONLY
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_HI_DATA_ONLY_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_HI_DATA_ONLY_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_HI_DATA_ONLY_t;

/*******************************************************************************
* Register      : FIFO_CTRL5
* Address       : 0X0A
* Bit Group Name: FIFO_MODE
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FIFO_MODE_BYPASS 		 = 0x00,
	LSM6DS3_ACC_GYRO_FIFO_MODE_FIFO 		 = 0x01,
	LSM6DS3_ACC_GYRO_FIFO_MODE_STREAM 		 = 0x02,
	LSM6DS3_ACC_GYRO_FIFO_MODE_STF 		 = 0x03,
	LSM6DS3_ACC_GYRO_FIFO_MODE_BTS 		 = 0x04,
	LSM6DS3_ACC_GYRO_FIFO_MODE_DYN_STREAM 		 = 0x05,
	LSM6DS3_ACC_GYRO_FIFO_MODE_DYN_STREAM_2 		 = 0x06,
	LSM6DS3_ACC_GYRO_FIFO_MODE_BTF 		 = 0x07,
} LSM6DS3_ACC_GYRO_FIFO_MODE_t;

/*******************************************************************************
* Register      : FIFO_CTRL5
* Address       : 0X0A
* Bit Group Name: ODR_FIFO
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_ODR_FIFO_10Hz 		 = 0x08,
	LSM6DS3_ACC_GYRO_ODR_FIFO_25Hz 		 = 0x10,
	LSM6DS3_ACC_GYRO_ODR_FIFO_50Hz 		 = 0x18,
	LSM6DS3_ACC_GYRO_ODR_FIFO_100Hz 		 = 0x20,
	LSM6DS3_ACC_GYRO_ODR_FIFO_200Hz 		 = 0x28,
	LSM6DS3_ACC_GYRO_ODR_FIFO_400Hz 		 = 0x30,
	LSM6DS3_ACC_GYRO_ODR_FIFO_800Hz 		 = 0x38,
	LSM6DS3_ACC_GYRO_ODR_FIFO_1600Hz 		 = 0x40,
	LSM6DS3_ACC_GYRO_ODR_FIFO_3300Hz 		 = 0x48,
	LSM6DS3_ACC_GYRO_ODR_FIFO_6600Hz 		 = 0x50,
	LSM6DS3_ACC_GYRO_ODR_FIFO_13300Hz 		 = 0x58,
} LSM6DS3_ACC_GYRO_ODR_FIFO_t;

/*******************************************************************************
* Register      : ORIENT_CFG_G
* Address       : 0X0B
* Bit Group Name: ORIENT
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_ORIENT_XYZ 		 = 0x00,
	LSM6DS3_ACC_GYRO_ORIENT_XZY 		 = 0x01,
	LSM6DS3_ACC_GYRO_ORIENT_YXZ 		 = 0x02,
	LSM6DS3_ACC_GYRO_ORIENT_YZX 		 = 0x03,
	LSM6DS3_ACC_GYRO_ORIENT_ZXY 		 = 0x04,
	LSM6DS3_ACC_GYRO_ORIENT_ZYX 		 = 0x05,
} LSM6DS3_ACC_GYRO_ORIENT_t;

/*******************************************************************************
* Register      : ORIENT_CFG_G
* Address       : 0X0B
* Bit Group Name: SIGN_Z_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SIGN_Z_G_POSITIVE 		 = 0x00,
	LSM6DS3_ACC_GYRO_SIGN_Z_G_NEGATIVE 		 = 0x08,
} LSM6DS3_ACC_GYRO_SIGN_Z_G_t;

/*******************************************************************************
* Register      : ORIENT_CFG_G
* Address       : 0X0B
* Bit Group Name: SIGN_Y_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SIGN_Y_G_POSITIVE 		 = 0x00,
	LSM6DS3_ACC_GYRO_SIGN_Y_G_NEGATIVE 		 = 0x10,
} LSM6DS3_ACC_GYRO_SIGN_Y_G_t;

/*******************************************************************************
* Register      : ORIENT_CFG_G
* Address       : 0X0B
* Bit Group Name: SIGN_X_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SIGN_X_G_POSITIVE 		 = 0x00,
	LSM6DS3_ACC_GYRO_SIGN_X_G_NEGATIVE 		 = 0x20,
} LSM6DS3_ACC_GYRO_SIGN_X_G_t;

/*******************************************************************************
* Register      : REFERENCE_G
* Address       : 0X0C
* Bit Group Name: REF_G
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_REF_G_MASK  	0xFF
#define  	LSM6DS3_ACC_GYRO_REF_G_POSITION  	0

/*******************************************************************************
* Register      : INT1_CTRL
* Address       : 0X0D
* Bit Group Name: INT1_DRDY_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_DRDY_XL_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_DRDY_XL_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_INT1_DRDY_XL_t;

/*******************************************************************************
* Register      : INT1_CTRL
* Address       : 0X0D
* Bit Group Name: INT1_DRDY_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_DRDY_G_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_DRDY_G_ENABLED 		 = 0x02,
} LSM6DS3_ACC_GYRO_INT1_DRDY_G_t;

/*******************************************************************************
* Register      : INT1_CTRL
* Address       : 0X0D
* Bit Group Name: INT1_BOOT
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_BOOT_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_BOOT_ENABLED 		 = 0x04,
} LSM6DS3_ACC_GYRO_INT1_BOOT_t;

/*******************************************************************************
* Register      : INT1_CTRL
* Address       : 0X0D
* Bit Group Name: INT1_FTH
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_FTH_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_FTH_ENABLED 		 = 0x08,
} LSM6DS3_ACC_GYRO_INT1_FTH_t;

/*******************************************************************************
* Register      : INT1_CTRL
* Address       : 0X0D
* Bit Group Name: INT1_OVR
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_OVR_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_OVR_ENABLED 		 = 0x10,
} LSM6DS3_ACC_GYRO_INT1_OVR_t;

/*******************************************************************************
* Register      : INT1_CTRL
* Address       : 0X0D
* Bit Group Name: INT1_FSS5
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_FSS5_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_FSS5_ENABLED 		 = 0x20,
} LSM6DS3_ACC_GYRO_INT1_FSS5_t;

/*******************************************************************************
* Register      : INT1_CTRL
* Address       : 0X0D
* Bit Group Name: INT1_SIGN_MOT
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_SIGN_MOT_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_SIGN_MOT_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_INT1_SIGN_MOT_t;

/*******************************************************************************
* Register      : INT1_CTRL
* Address       : 0X0D
* Bit Group Name: INT1_PEDO
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_PEDO_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_PEDO_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_INT1_PEDO_t;

/*******************************************************************************
* Register      : INT2_CTRL
* Address       : 0X0E
* Bit Group Name: INT2_DRDY_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_DRDY_XL_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_DRDY_XL_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_INT2_DRDY_XL_t;

/*******************************************************************************
* Register      : INT2_CTRL
* Address       : 0X0E
* Bit Group Name: INT2_DRDY_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_DRDY_G_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_DRDY_G_ENABLED 		 = 0x02,
} LSM6DS3_ACC_GYRO_INT2_DRDY_G_t;

/*******************************************************************************
* Register      : INT2_CTRL
* Address       : 0X0E
* Bit Group Name: INT2_FTH
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_FTH_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_FTH_ENABLED 		 = 0x08,
} LSM6DS3_ACC_GYRO_INT2_FTH_t;

/*******************************************************************************
* Register      : INT2_CTRL
* Address       : 0X0E
* Bit Group Name: INT2_OVR
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_OVR_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_OVR_ENABLED 		 = 0x10,
} LSM6DS3_ACC_GYRO_INT2_OVR_t;

/*******************************************************************************
* Register      : INT2_CTRL
* Address       : 0X0E
* Bit Group Name: INT2_FSS5
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_FSS5_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_FSS5_ENABLED 		 = 0x20,
} LSM6DS3_ACC_GYRO_INT2_FSS5_t;

/*******************************************************************************
* Register      : INT2_CTRL
* Address       : 0X0E
* Bit Group Name: INT2_SIGN_MOT
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_SIGN_MOT_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_SIGN_MOT_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_INT2_SIGN_MOT_t;

/*******************************************************************************
* Register      : INT2_CTRL
* Address       : 0X0E
* Bit Group Name: INT2_PEDO
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_PEDO_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_PEDO_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_INT2_PEDO_t;

/*******************************************************************************
* Register      : WHO_AM_I
* Address       : 0X0F
* Bit Group Name: WHO_AM_I_BIT
* Permission    : RO
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_WHO_AM_I_BIT_MASK  	0xFF
#define  	LSM6DS3_ACC_GYRO_WHO_AM_I_BIT_POSITION  	0

/*******************************************************************************
* Register      : CTRL1_XL
* Address       : 0X10
* Bit Group Name: BW_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_BW_XL_400Hz 		 = 0x00,
	LSM6DS3_ACC_GYRO_BW_XL_200Hz 		 = 0x01,
	LSM6DS3_ACC_GYRO_BW_XL_100Hz 		 = 0x02,
	LSM6DS3_ACC_GYRO_BW_XL_50Hz 		 = 0x03,
} LSM6DS3_ACC_GYRO_BW_XL_t;

/*******************************************************************************
* Register      : CTRL1_XL
* Address       : 0X10
* Bit Group Name: FS_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FS_XL_2g 		 = 0x00,
	LSM6DS3_ACC_GYRO_FS_XL_16g 		 = 0x04,
	LSM6DS3_ACC_GYRO_FS_XL_4g 		 = 0x08,
	LSM6DS3_ACC_GYRO_FS_XL_8g 		 = 0x0C,
} LSM6DS3_ACC_GYRO_FS_XL_t;

/*******************************************************************************
* Register      : CTRL1_XL
* Address       : 0X10
* Bit Group Name: ODR_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_ODR_XL_POWER_DOWN 		 = 0x00,
	LSM6DS3_ACC_GYRO_ODR_XL_13Hz 		         = 0x10,
	LSM6DS3_ACC_GYRO_ODR_XL_26Hz 		         = 0x20,
	LSM6DS3_ACC_GYRO_ODR_XL_52Hz 		         = 0x30,
	LSM6DS3_ACC_GYRO_ODR_XL_104Hz 		 = 0x40,
	LSM6DS3_ACC_GYRO_ODR_XL_208Hz 		 = 0x50,
	LSM6DS3_ACC_GYRO_ODR_XL_416Hz 		 = 0x60,
	LSM6DS3_ACC_GYRO_ODR_XL_833Hz 		 = 0x70,
	LSM6DS3_ACC_GYRO_ODR_XL_1660Hz 		 = 0x80,
	LSM6DS3_ACC_GYRO_ODR_XL_3330Hz 		 = 0x90,
	LSM6DS3_ACC_GYRO_ODR_XL_6660Hz 		 = 0xA0,
	LSM6DS3_ACC_GYRO_ODR_XL_13330Hz 		 = 0xB0,
} LSM6DS3_ACC_GYRO_ODR_XL_t;

/*******************************************************************************
* Register      : CTRL2_G
* Address       : 0X11
* Bit Group Name: FS_125
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FS_125_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_FS_125_ENABLED 		 = 0x02,
} LSM6DS3_ACC_GYRO_FS_125_t;

/*******************************************************************************
* Register      : CTRL2_G
* Address       : 0X11
* Bit Group Name: FS_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FS_G_245dps 		 = 0x00,
	LSM6DS3_ACC_GYRO_FS_G_500dps 		 = 0x04,
	LSM6DS3_ACC_GYRO_FS_G_1000dps 		 = 0x08,
	LSM6DS3_ACC_GYRO_FS_G_2000dps 		 = 0x0C,
} LSM6DS3_ACC_GYRO_FS_G_t;

/*******************************************************************************
* Register      : CTRL2_G
* Address       : 0X11
* Bit Group Name: ODR_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_ODR_G_POWER_DOWN 		 = 0x00,
	LSM6DS3_ACC_GYRO_ODR_G_13Hz 		 = 0x10,
	LSM6DS3_ACC_GYRO_ODR_G_26Hz 		 = 0x20,
	LSM6DS3_ACC_GYRO_ODR_G_52Hz 		 = 0x30,
	LSM6DS3_ACC_GYRO_ODR_G_104Hz 		 = 0x40,
	LSM6DS3_ACC_GYRO_ODR_G_208Hz 		 = 0x50,
	LSM6DS3_ACC_GYRO_ODR_G_416Hz 		 = 0x60,
	LSM6DS3_ACC_GYRO_ODR_G_833Hz 		 = 0x70,
	LSM6DS3_ACC_GYRO_ODR_G_1660Hz 		 = 0x80,
} LSM6DS3_ACC_GYRO_ODR_G_t;

/*******************************************************************************
* Register      : CTRL3_C
* Address       : 0X12
* Bit Group Name: SW_RESET
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SW_RESET_NORMAL_MODE 		 = 0x00,
	LSM6DS3_ACC_GYRO_SW_RESET_RESET_DEVICE 		 = 0x01,
} LSM6DS3_ACC_GYRO_SW_RESET_t;

/*******************************************************************************
* Register      : CTRL3_C
* Address       : 0X12
* Bit Group Name: BLE
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_BLE_LSB 		 = 0x00,
	LSM6DS3_ACC_GYRO_BLE_MSB 		 = 0x02,
} LSM6DS3_ACC_GYRO_BLE_t;

/*******************************************************************************
* Register      : CTRL3_C
* Address       : 0X12
* Bit Group Name: IF_INC
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_IF_INC_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_IF_INC_ENABLED 		 = 0x04,
} LSM6DS3_ACC_GYRO_IF_INC_t;

/*******************************************************************************
* Register      : CTRL3_C
* Address       : 0X12
* Bit Group Name: SIM
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SIM_4_WIRE 		 = 0x00,
	LSM6DS3_ACC_GYRO_SIM_3_WIRE 		 = 0x08,
} LSM6DS3_ACC_GYRO_SIM_t;

/*******************************************************************************
* Register      : CTRL3_C
* Address       : 0X12
* Bit Group Name: PP_OD
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_PP_OD_PUSH_PULL 		 = 0x00,
	LSM6DS3_ACC_GYRO_PP_OD_OPEN_DRAIN 		 = 0x10,
} LSM6DS3_ACC_GYRO_PP_OD_t;

/*******************************************************************************
* Register      : CTRL3_C
* Address       : 0X12
* Bit Group Name: INT_ACT_LEVEL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT_ACT_LEVEL_ACTIVE_HI 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT_ACT_LEVEL_ACTIVE_LO 		 = 0x20,
} LSM6DS3_ACC_GYRO_INT_ACT_LEVEL_t;

/*******************************************************************************
* Register      : CTRL3_C
* Address       : 0X12
* Bit Group Name: BDU
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_BDU_CONTINUOS 		 = 0x00,
	LSM6DS3_ACC_GYRO_BDU_BLOCK_UPDATE 		 = 0x40,
} LSM6DS3_ACC_GYRO_BDU_t;

/*******************************************************************************
* Register      : CTRL3_C
* Address       : 0X12
* Bit Group Name: BOOT
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_BOOT_NORMAL_MODE 		 = 0x00,
	LSM6DS3_ACC_GYRO_BOOT_REBOOT_MODE 		 = 0x80,
} LSM6DS3_ACC_GYRO_BOOT_t;

/*******************************************************************************
* Register      : CTRL4_C
* Address       : 0X13
* Bit Group Name: STOP_ON_FTH
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_STOP_ON_FTH_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_STOP_ON_FTH_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_STOP_ON_FTH_t;

/*******************************************************************************
* Register      : CTRL4_C
* Address       : 0X13
* Bit Group Name: MODE3_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_MODE3_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_MODE3_EN_ENABLED 		 = 0x02,
} LSM6DS3_ACC_GYRO_MODE3_EN_t;

/*******************************************************************************
* Register      : CTRL4_C
* Address       : 0X13
* Bit Group Name: I2C_DISABLE
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_I2C_DISABLE_I2C_AND_SPI 		 = 0x00,
	LSM6DS3_ACC_GYRO_I2C_DISABLE_SPI_ONLY 		 = 0x04,
} LSM6DS3_ACC_GYRO_I2C_DISABLE_t;

/*******************************************************************************
* Register      : CTRL4_C
* Address       : 0X13
* Bit Group Name: DRDY_MSK
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DRDY_MSK_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DRDY_MSK_ENABLED 		 = 0x08,
} LSM6DS3_ACC_GYRO_DRDY_MSK_t;

/*******************************************************************************
* Register      : CTRL4_C
* Address       : 0X13
* Bit Group Name: FIFO_TEMP_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FIFO_TEMP_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_FIFO_TEMP_EN_ENABLED 		 = 0x10,
} LSM6DS3_ACC_GYRO_FIFO_TEMP_EN_t;

/*******************************************************************************
* Register      : CTRL4_C
* Address       : 0X13
* Bit Group Name: INT2_ON_INT1
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_ON_INT1_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_ON_INT1_ENABLED 		 = 0x20,
} LSM6DS3_ACC_GYRO_INT2_ON_INT1_t;

/*******************************************************************************
* Register      : CTRL4_C
* Address       : 0X13
* Bit Group Name: SLEEP_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SLEEP_G_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_SLEEP_G_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_SLEEP_G_t;

/*******************************************************************************
* Register      : CTRL4_C
* Address       : 0X13
* Bit Group Name: BW_SCAL_ODR
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_BW_SCAL_ODR_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_BW_SCAL_ODR_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_BW_SCAL_ODR_t;

/*******************************************************************************
* Register      : CTRL5_C
* Address       : 0X14
* Bit Group Name: ST_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_ST_XL_NORMAL_MODE 		 = 0x00,
	LSM6DS3_ACC_GYRO_ST_XL_POS_SIGN_TEST 		 = 0x01,
	LSM6DS3_ACC_GYRO_ST_XL_NEG_SIGN_TEST 		 = 0x02,
	LSM6DS3_ACC_GYRO_ST_XL_NA 		 = 0x03,
} LSM6DS3_ACC_GYRO_ST_XL_t;

/*******************************************************************************
* Register      : CTRL5_C
* Address       : 0X14
* Bit Group Name: ST_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_ST_G_NORMAL_MODE 		 = 0x00,
	LSM6DS3_ACC_GYRO_ST_G_POS_SIGN_TEST 		 = 0x04,
	LSM6DS3_ACC_GYRO_ST_G_NA 		 = 0x08,
	LSM6DS3_ACC_GYRO_ST_G_NEG_SIGN_TEST 		 = 0x0C,
} LSM6DS3_ACC_GYRO_ST_G_t;

/*******************************************************************************
* Register      : CTRL6_G
* Address       : 0X15
* Bit Group Name: LP_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_LP_XL_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_LP_XL_ENABLED 		 = 0x10,
} LSM6DS3_ACC_GYRO_LP_XL_t;

/*******************************************************************************
* Register      : CTRL6_G
* Address       : 0X15
* Bit Group Name: DEN_LVL2_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DEN_LVL2_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DEN_LVL2_EN_ENABLED 		 = 0x20,
} LSM6DS3_ACC_GYRO_DEN_LVL2_EN_t;

/*******************************************************************************
* Register      : CTRL6_G
* Address       : 0X15
* Bit Group Name: DEN_LVL_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DEN_LVL_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DEN_LVL_EN_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_DEN_LVL_EN_t;

/*******************************************************************************
* Register      : CTRL6_G
* Address       : 0X15
* Bit Group Name: DEN_EDGE_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DEN_EDGE_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DEN_EDGE_EN_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_DEN_EDGE_EN_t;

/*******************************************************************************
* Register      : CTRL7_G
* Address       : 0X16
* Bit Group Name: HPM_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_HPM_G_NORMAL_MODE 		 = 0x00,
	LSM6DS3_ACC_GYRO_HPM_G_REF_SIGNAL 		 = 0x10,
	LSM6DS3_ACC_GYRO_HPM_G_NORMAL_MODE_2 		 = 0x20,
	LSM6DS3_ACC_GYRO_HPM_G_AUTO_RESET_ON_INT 		 = 0x30,
} LSM6DS3_ACC_GYRO_HPM_G_t;

/*******************************************************************************
* Register      : CTRL7_G
* Address       : 0X16
* Bit Group Name: HP_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_HP_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_HP_EN_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_HP_EN_t;

/*******************************************************************************
* Register      : CTRL7_G
* Address       : 0X16
* Bit Group Name: LP_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_LP_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_LP_EN_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_LP_EN_t;

/*******************************************************************************
* Register      : CTRL8_XL
* Address       : 0X17
* Bit Group Name: FDS
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FDS_FILTER_OFF 		 = 0x00,
	LSM6DS3_ACC_GYRO_FDS_FILTER_ON 		 = 0x04,
} LSM6DS3_ACC_GYRO_FDS_t;

/*******************************************************************************
* Register      : CTRL9_XL
* Address       : 0X18
* Bit Group Name: XEN_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_XEN_XL_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_XEN_XL_ENABLED 		 = 0x08,
} LSM6DS3_ACC_GYRO_XEN_XL_t;

/*******************************************************************************
* Register      : CTRL9_XL
* Address       : 0X18
* Bit Group Name: YEN_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_YEN_XL_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_YEN_XL_ENABLED 		 = 0x10,
} LSM6DS3_ACC_GYRO_YEN_XL_t;

/*******************************************************************************
* Register      : CTRL9_XL
* Address       : 0X18
* Bit Group Name: ZEN_XL
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_ZEN_XL_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_ZEN_XL_ENABLED 		 = 0x20,
} LSM6DS3_ACC_GYRO_ZEN_XL_t;

/*******************************************************************************
* Register      : CTRL10_C
* Address       : 0X19
* Bit Group Name: SIGN_MOTION_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SIGN_MOTION_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_SIGN_MOTION_EN_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_SIGN_MOTION_EN_t;

/*******************************************************************************
* Register      : CTRL10_C
* Address       : 0X19
* Bit Group Name: PEDO_RST_STEP
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_PEDO_RST_STEP_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_PEDO_RST_STEP_ENABLED 		 = 0x02,
} LSM6DS3_ACC_GYRO_PEDO_RST_STEP_t;

/*******************************************************************************
* Register      : CTRL10_C
* Address       : 0X19
* Bit Group Name: XEN_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_XEN_G_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_XEN_G_ENABLED 		 = 0x08,
} LSM6DS3_ACC_GYRO_XEN_G_t;

/*******************************************************************************
* Register      : CTRL10_C
* Address       : 0X19
* Bit Group Name: YEN_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_YEN_G_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_YEN_G_ENABLED 		 = 0x10,
} LSM6DS3_ACC_GYRO_YEN_G_t;

/*******************************************************************************
* Register      : CTRL10_C
* Address       : 0X19
* Bit Group Name: ZEN_G
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_ZEN_G_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_ZEN_G_ENABLED 		 = 0x20,
} LSM6DS3_ACC_GYRO_ZEN_G_t;

/*******************************************************************************
* Register      : CTRL10_C
* Address       : 0X19
* Bit Group Name: FUNC_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FUNC_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_FUNC_EN_ENABLED 		 = 0x04,
} LSM6DS3_ACC_GYRO_FUNC_EN_t;

/*******************************************************************************
* Register      : MASTER_CONFIG
* Address       : 0X1A
* Bit Group Name: MASTER_ON
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_MASTER_ON_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_MASTER_ON_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_MASTER_ON_t;

/*******************************************************************************
* Register      : MASTER_CONFIG
* Address       : 0X1A
* Bit Group Name: IRON_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_IRON_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_IRON_EN_ENABLED 		 = 0x02,
} LSM6DS3_ACC_GYRO_IRON_EN_t;

/*******************************************************************************
* Register      : MASTER_CONFIG
* Address       : 0X1A
* Bit Group Name: PASS_THRU_MODE
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_PASS_THRU_MODE_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_PASS_THRU_MODE_ENABLED 		 = 0x04,
} LSM6DS3_ACC_GYRO_PASS_THRU_MODE_t;

/*******************************************************************************
* Register      : MASTER_CONFIG
* Address       : 0X1A
* Bit Group Name: PULL_UP_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_PULL_UP_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_PULL_UP_EN_ENABLED 		 = 0x08,
} LSM6DS3_ACC_GYRO_PULL_UP_EN_t;

/*******************************************************************************
* Register      : MASTER_CONFIG
* Address       : 0X1A
* Bit Group Name: START_CONFIG
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_START_CONFIG_XL_G_DRDY 		 = 0x00,
	LSM6DS3_ACC_GYRO_START_CONFIG_EXT_INT2 		 = 0x10,
} LSM6DS3_ACC_GYRO_START_CONFIG_t;

/*******************************************************************************
* Register      : MASTER_CONFIG
* Address       : 0X1A
* Bit Group Name: DATA_VAL_SEL_FIFO
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DATA_VAL_SEL_FIFO_XL_G_DRDY 		 = 0x00,
	LSM6DS3_ACC_GYRO_DATA_VAL_SEL_FIFO_SHUB_DRDY 		 = 0x40,
} LSM6DS3_ACC_GYRO_DATA_VAL_SEL_FIFO_t;

/*******************************************************************************
* Register      : MASTER_CONFIG
* Address       : 0X1A
* Bit Group Name: DRDY_ON_INT1
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DRDY_ON_INT1_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DRDY_ON_INT1_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_DRDY_ON_INT1_t;

/*******************************************************************************
* Register      : WAKE_UP_SRC
* Address       : 0X1B
* Bit Group Name: Z_WU
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_Z_WU_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_Z_WU_DETECTED 		 = 0x01,
} LSM6DS3_ACC_GYRO_Z_WU_t;

/*******************************************************************************
* Register      : WAKE_UP_SRC
* Address       : 0X1B
* Bit Group Name: Y_WU
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_Y_WU_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_Y_WU_DETECTED 		 = 0x02,
} LSM6DS3_ACC_GYRO_Y_WU_t;

/*******************************************************************************
* Register      : WAKE_UP_SRC
* Address       : 0X1B
* Bit Group Name: X_WU
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_X_WU_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_X_WU_DETECTED 		 = 0x04,
} LSM6DS3_ACC_GYRO_X_WU_t;

/*******************************************************************************
* Register      : WAKE_UP_SRC
* Address       : 0X1B
* Bit Group Name: WU_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_WU_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_WU_EV_STATUS_DETECTED 		 = 0x08,
} LSM6DS3_ACC_GYRO_WU_EV_STATUS_t;

/*******************************************************************************
* Register      : WAKE_UP_SRC
* Address       : 0X1B
* Bit Group Name: SLEEP_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SLEEP_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_SLEEP_EV_STATUS_DETECTED 		 = 0x10,
} LSM6DS3_ACC_GYRO_SLEEP_EV_STATUS_t;

/*******************************************************************************
* Register      : WAKE_UP_SRC
* Address       : 0X1B
* Bit Group Name: FF_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FF_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_FF_EV_STATUS_DETECTED 		 = 0x20,
} LSM6DS3_ACC_GYRO_FF_EV_STATUS_t;

/*******************************************************************************
* Register      : TAP_SRC
* Address       : 0X1C
* Bit Group Name: Z_TAP
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_Z_TAP_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_Z_TAP_DETECTED 		 = 0x01,
} LSM6DS3_ACC_GYRO_Z_TAP_t;

/*******************************************************************************
* Register      : TAP_SRC
* Address       : 0X1C
* Bit Group Name: Y_TAP
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_Y_TAP_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_Y_TAP_DETECTED 		 = 0x02,
} LSM6DS3_ACC_GYRO_Y_TAP_t;

/*******************************************************************************
* Register      : TAP_SRC
* Address       : 0X1C
* Bit Group Name: X_TAP
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_X_TAP_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_X_TAP_DETECTED 		 = 0x04,
} LSM6DS3_ACC_GYRO_X_TAP_t;

/*******************************************************************************
* Register      : TAP_SRC
* Address       : 0X1C
* Bit Group Name: TAP_SIGN
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TAP_SIGN_POS_SIGN 		 = 0x00,
	LSM6DS3_ACC_GYRO_TAP_SIGN_NEG_SIGN 		 = 0x08,
} LSM6DS3_ACC_GYRO_TAP_SIGN_t;

/*******************************************************************************
* Register      : TAP_SRC
* Address       : 0X1C
* Bit Group Name: DOUBLE_TAP_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DOUBLE_TAP_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DOUBLE_TAP_EV_STATUS_DETECTED 		 = 0x10,
} LSM6DS3_ACC_GYRO_DOUBLE_TAP_EV_STATUS_t;

/*******************************************************************************
* Register      : TAP_SRC
* Address       : 0X1C
* Bit Group Name: SINGLE_TAP_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SINGLE_TAP_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_SINGLE_TAP_EV_STATUS_DETECTED 		 = 0x20,
} LSM6DS3_ACC_GYRO_SINGLE_TAP_EV_STATUS_t;

/*******************************************************************************
* Register      : TAP_SRC
* Address       : 0X1C
* Bit Group Name: TAP_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TAP_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_TAP_EV_STATUS_DETECTED 		 = 0x40,
} LSM6DS3_ACC_GYRO_TAP_EV_STATUS_t;

/*******************************************************************************
* Register      : D6D_SRC
* Address       : 0X1D
* Bit Group Name: DSD_XL
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DSD_XL_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DSD_XL_DETECTED 		 = 0x01,
} LSM6DS3_ACC_GYRO_DSD_XL_t;

/*******************************************************************************
* Register      : D6D_SRC
* Address       : 0X1D
* Bit Group Name: DSD_XH
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DSD_XH_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DSD_XH_DETECTED 		 = 0x02,
} LSM6DS3_ACC_GYRO_DSD_XH_t;

/*******************************************************************************
* Register      : D6D_SRC
* Address       : 0X1D
* Bit Group Name: DSD_YL
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DSD_YL_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DSD_YL_DETECTED 		 = 0x04,
} LSM6DS3_ACC_GYRO_DSD_YL_t;

/*******************************************************************************
* Register      : D6D_SRC
* Address       : 0X1D
* Bit Group Name: DSD_YH
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DSD_YH_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DSD_YH_DETECTED 		 = 0x08,
} LSM6DS3_ACC_GYRO_DSD_YH_t;

/*******************************************************************************
* Register      : D6D_SRC
* Address       : 0X1D
* Bit Group Name: DSD_ZL
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DSD_ZL_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DSD_ZL_DETECTED 		 = 0x10,
} LSM6DS3_ACC_GYRO_DSD_ZL_t;

/*******************************************************************************
* Register      : D6D_SRC
* Address       : 0X1D
* Bit Group Name: DSD_ZH
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_DSD_ZH_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_DSD_ZH_DETECTED 		 = 0x20,
} LSM6DS3_ACC_GYRO_DSD_ZH_t;

/*******************************************************************************
* Register      : D6D_SRC
* Address       : 0X1D
* Bit Group Name: D6D_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_D6D_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_D6D_EV_STATUS_DETECTED 		 = 0x40,
} LSM6DS3_ACC_GYRO_D6D_EV_STATUS_t;

/*******************************************************************************
* Register      : STATUS_REG
* Address       : 0X1E
* Bit Group Name: XLDA
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_XLDA_NO_DATA_AVAIL 		 = 0x00,
	LSM6DS3_ACC_GYRO_XLDA_DATA_AVAIL 		 = 0x01,
} LSM6DS3_ACC_GYRO_XLDA_t;

/*******************************************************************************
* Register      : STATUS_REG
* Address       : 0X1E
* Bit Group Name: GDA
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_GDA_NO_DATA_AVAIL 		 = 0x00,
	LSM6DS3_ACC_GYRO_GDA_DATA_AVAIL 		 = 0x02,
} LSM6DS3_ACC_GYRO_GDA_t;

/*******************************************************************************
* Register      : STATUS_REG
* Address       : 0X1E
* Bit Group Name: EV_BOOT
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_EV_BOOT_NO_BOOT_RUNNING 		 = 0x00,
	LSM6DS3_ACC_GYRO_EV_BOOT_BOOT_IS_RUNNING 		 = 0x08,
} LSM6DS3_ACC_GYRO_EV_BOOT_t;

/*******************************************************************************
* Register      : FIFO_STATUS1
* Address       : 0X3A
* Bit Group Name: DIFF_FIFO
* Permission    : RO
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_DIFF_FIFO_STATUS1_MASK  	0xFF
#define  	LSM6DS3_ACC_GYRO_DIFF_FIFO_STATUS1_POSITION  	0
#define  	LSM6DS3_ACC_GYRO_DIFF_FIFO_STATUS2_MASK  0xF
#define  	LSM6DS3_ACC_GYRO_DIFF_FIFO_STATUS2_POSITION  	0

/*******************************************************************************
* Register      : FIFO_STATUS2
* Address       : 0X3B
* Bit Group Name: FIFO_EMPTY
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FIFO_EMPTY_FIFO_NOT_EMPTY 		 = 0x00,
	LSM6DS3_ACC_GYRO_FIFO_EMPTY_FIFO_EMPTY 		 = 0x10,
} LSM6DS3_ACC_GYRO_FIFO_EMPTY_t;

/*******************************************************************************
* Register      : FIFO_STATUS2
* Address       : 0X3B
* Bit Group Name: FIFO_FULL
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FIFO_FULL_FIFO_NOT_FULL 		 = 0x00,
	LSM6DS3_ACC_GYRO_FIFO_FULL_FIFO_FULL 		 = 0x20,
} LSM6DS3_ACC_GYRO_FIFO_FULL_t;

/*******************************************************************************
* Register      : FIFO_STATUS2
* Address       : 0X3B
* Bit Group Name: OVERRUN
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_OVERRUN_NO_OVERRUN 		 = 0x00,
	LSM6DS3_ACC_GYRO_OVERRUN_OVERRUN 		 = 0x40,
} LSM6DS3_ACC_GYRO_OVERRUN_t;

/*******************************************************************************
* Register      : FIFO_STATUS2
* Address       : 0X3B
* Bit Group Name: WTM
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_WTM_BELOW_WTM 		 = 0x00,
	LSM6DS3_ACC_GYRO_WTM_ABOVE_OR_EQUAL_WTM 		 = 0x80,
} LSM6DS3_ACC_GYRO_WTM_t;

/*******************************************************************************
* Register      : FIFO_STATUS3
* Address       : 0X3C
* Bit Group Name: FIFO_PATTERN
* Permission    : RO
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_FIFO_STATUS3_PATTERN_MASK  	0xFF
#define  	LSM6DS3_ACC_GYRO_FIFO_STATUS3_PATTERN_POSITION  	0
#define  	LSM6DS3_ACC_GYRO_FIFO_STATUS4_PATTERN_MASK  	0x03
#define  	LSM6DS3_ACC_GYRO_FIFO_STATUS4_PATTERN_POSITION  	0

/*******************************************************************************
* Register      : FUNC_SRC
* Address       : 0X53
* Bit Group Name: SENS_HUB_END
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SENS_HUB_END_STILL_ONGOING 		 = 0x00,
	LSM6DS3_ACC_GYRO_SENS_HUB_END_OP_COMPLETED 		 = 0x01,
} LSM6DS3_ACC_GYRO_SENS_HUB_END_t;

/*******************************************************************************
* Register      : FUNC_SRC
* Address       : 0X53
* Bit Group Name: SOFT_IRON_END
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SOFT_IRON_END_NOT_COMPLETED 		 = 0x00,
	LSM6DS3_ACC_GYRO_SOFT_IRON_END_COMPLETED 		 = 0x02,
} LSM6DS3_ACC_GYRO_SOFT_IRON_END_t;

/*******************************************************************************
* Register      : FUNC_SRC
* Address       : 0X53
* Bit Group Name: PEDO_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_PEDO_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_PEDO_EV_STATUS_DETECTED 		 = 0x10,
} LSM6DS3_ACC_GYRO_PEDO_EV_STATUS_t;

/*******************************************************************************
* Register      : FUNC_SRC
* Address       : 0X53
* Bit Group Name: TILT_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TILT_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_TILT_EV_STATUS_DETECTED 		 = 0x20,
} LSM6DS3_ACC_GYRO_TILT_EV_STATUS_t;

/*******************************************************************************
* Register      : FUNC_SRC
* Address       : 0X53
* Bit Group Name: SIGN_MOT_EV_STATUS
* Permission    : RO
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SIGN_MOT_EV_STATUS_NOT_DETECTED 		 = 0x00,
	LSM6DS3_ACC_GYRO_SIGN_MOT_EV_STATUS_DETECTED 		 = 0x40,
} LSM6DS3_ACC_GYRO_SIGN_MOT_EV_STATUS_t;

/*******************************************************************************
* Register      : TAP_CFG1
* Address       : 0X58
* Bit Group Name: LIR
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_LIR_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_LIR_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_LIR_t;

/*******************************************************************************
* Register      : TAP_CFG1
* Address       : 0X58
* Bit Group Name: TAP_Z_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TAP_Z_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_TAP_Z_EN_ENABLED 		 = 0x02,
} LSM6DS3_ACC_GYRO_TAP_Z_EN_t;

/*******************************************************************************
* Register      : TAP_CFG1
* Address       : 0X58
* Bit Group Name: TAP_Y_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TAP_Y_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_TAP_Y_EN_ENABLED 		 = 0x04,
} LSM6DS3_ACC_GYRO_TAP_Y_EN_t;

/*******************************************************************************
* Register      : TAP_CFG1
* Address       : 0X58
* Bit Group Name: TAP_X_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TAP_X_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_TAP_X_EN_ENABLED 		 = 0x08,
} LSM6DS3_ACC_GYRO_TAP_X_EN_t;

/*******************************************************************************
* Register      : TAP_CFG1
* Address       : 0X58
* Bit Group Name: TILT_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TILT_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_TILT_EN_ENABLED 		 = 0x20,
} LSM6DS3_ACC_GYRO_TILT_EN_t;

/*******************************************************************************
* Register      : TAP_CFG1
* Address       : 0X58
* Bit Group Name: PEDO_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_PEDO_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_PEDO_EN_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_PEDO_EN_t;

/*******************************************************************************
* Register      : TAP_CFG1
* Address       : 0X58
* Bit Group Name: TIMER_EN
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TIMER_EN_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_TIMER_EN_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_TIMER_EN_t;

/*******************************************************************************
* Register      : TAP_THS_6D
* Address       : 0X59
* Bit Group Name: TAP_THS
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_TAP_THS_MASK  	0x1F
#define  	LSM6DS3_ACC_GYRO_TAP_THS_POSITION  	0

/*******************************************************************************
* Register      : TAP_THS_6D
* Address       : 0X59
* Bit Group Name: SIXD_THS
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SIXD_THS_80_degree 		 = 0x00,
	LSM6DS3_ACC_GYRO_SIXD_THS_70_degree 		 = 0x20,
	LSM6DS3_ACC_GYRO_SIXD_THS_60_degree 		 = 0x40,
	LSM6DS3_ACC_GYRO_SIXD_THS_50_degree 		 = 0x60,
} LSM6DS3_ACC_GYRO_SIXD_THS_t;

/*******************************************************************************
* Register      : INT_DUR2
* Address       : 0X5A
* Bit Group Name: SHOCK
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_SHOCK_MASK  	0x03
#define  	LSM6DS3_ACC_GYRO_SHOCK_POSITION  	0

/*******************************************************************************
* Register      : INT_DUR2
* Address       : 0X5A
* Bit Group Name: QUIET
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_QUIET_MASK  	0x0C
#define  	LSM6DS3_ACC_GYRO_QUIET_POSITION  	2

/*******************************************************************************
* Register      : INT_DUR2
* Address       : 0X5A
* Bit Group Name: DUR
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_DUR_MASK  	0xF0
#define  	LSM6DS3_ACC_GYRO_DUR_POSITION  	4

/*******************************************************************************
* Register      : WAKE_UP_THS
* Address       : 0X5B
* Bit Group Name: WK_THS
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_WK_THS_MASK  	0x3F
#define  	LSM6DS3_ACC_GYRO_WK_THS_POSITION  	0

/*******************************************************************************
* Register      : WAKE_UP_THS
* Address       : 0X5B
* Bit Group Name: INACTIVITY_ON
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INACTIVITY_ON_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INACTIVITY_ON_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_INACTIVITY_ON_t;

/*******************************************************************************
* Register      : WAKE_UP_THS
* Address       : 0X5B
* Bit Group Name: SINGLE_DOUBLE_TAP
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_SINGLE_DOUBLE_TAP_DOUBLE_TAP 		 = 0x00,
	LSM6DS3_ACC_GYRO_SINGLE_DOUBLE_TAP_SINGLE_TAP 		 = 0x80,
} LSM6DS3_ACC_GYRO_SINGLE_DOUBLE_TAP_t;

/*******************************************************************************
* Register      : WAKE_UP_DUR
* Address       : 0X5C
* Bit Group Name: SLEEP_DUR
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_SLEEP_DUR_MASK  	0x0F
#define  	LSM6DS3_ACC_GYRO_SLEEP_DUR_POSITION  	0

/*******************************************************************************
* Register      : WAKE_UP_DUR
* Address       : 0X5C
* Bit Group Name: TIMER_HR
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_TIMER_HR_6_4ms 		 = 0x00,
	LSM6DS3_ACC_GYRO_TIMER_HR_25us 		 = 0x10,
} LSM6DS3_ACC_GYRO_TIMER_HR_t;

/*******************************************************************************
* Register      : WAKE_UP_DUR
* Address       : 0X5C
* Bit Group Name: WAKE_DUR
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_WAKE_DUR_MASK  	0x60
#define  	LSM6DS3_ACC_GYRO_WAKE_DUR_POSITION  	5

/*******************************************************************************
* Register      : FREE_FALL
* Address       : 0X5D
* Bit Group Name: FF_DUR
* Permission    : RW
*******************************************************************************/
#define  	LSM6DS3_ACC_GYRO_FF_FREE_FALL_DUR_MASK  	0xF8
#define  	LSM6DS3_ACC_GYRO_FF_FREE_FALL_DUR_POSITION  	3
#define  	LSM6DS3_ACC_GYRO_FF_WAKE_UP_DUR_MASK  	0x80
#define  	LSM6DS3_ACC_GYRO_FF_WAKE_UP_DUR_POSITION  	7


/*******************************************************************************
* Register      : FREE_FALL
* Address       : 0X5D
* Bit Group Name: FF_THS
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_FF_THS_5 		 = 0x00,
	LSM6DS3_ACC_GYRO_FF_THS_7 		 = 0x01,
	LSM6DS3_ACC_GYRO_FF_THS_8 		 = 0x02,
	LSM6DS3_ACC_GYRO_FF_THS_10 		 = 0x03,
	LSM6DS3_ACC_GYRO_FF_THS_11 		 = 0x04,
	LSM6DS3_ACC_GYRO_FF_THS_13 		 = 0x05,
	LSM6DS3_ACC_GYRO_FF_THS_15 		 = 0x06,
	LSM6DS3_ACC_GYRO_FF_THS_16 		 = 0x07,
} LSM6DS3_ACC_GYRO_FF_THS_t;

/*******************************************************************************
* Register      : MD1_CFG
* Address       : 0X5E
* Bit Group Name: INT1_TIMER
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_TIMER_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_TIMER_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_INT1_TIMER_t;

/*******************************************************************************
* Register      : MD1_CFG
* Address       : 0X5E
* Bit Group Name: INT1_TILT
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_TILT_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_TILT_ENABLED 		 = 0x02,
} LSM6DS3_ACC_GYRO_INT1_TILT_t;

/*******************************************************************************
* Register      : MD1_CFG
* Address       : 0X5E
* Bit Group Name: INT1_6D
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_6D_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_6D_ENABLED 		 = 0x04,
} LSM6DS3_ACC_GYRO_INT1_6D_t;

/*******************************************************************************
* Register      : MD1_CFG
* Address       : 0X5E
* Bit Group Name: INT1_TAP
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_TAP_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_TAP_ENABLED 		 = 0x08,
} LSM6DS3_ACC_GYRO_INT1_TAP_t;

/*******************************************************************************
* Register      : MD1_CFG
* Address       : 0X5E
* Bit Group Name: INT1_FF
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_FF_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_FF_ENABLED 		 = 0x10,
} LSM6DS3_ACC_GYRO_INT1_FF_t;

/*******************************************************************************
* Register      : MD1_CFG
* Address       : 0X5E
* Bit Group Name: INT1_WU
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_WU_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_WU_ENABLED 		 = 0x20,
} LSM6DS3_ACC_GYRO_INT1_WU_t;

/*******************************************************************************
* Register      : MD1_CFG
* Address       : 0X5E
* Bit Group Name: INT1_SINGLE_TAP
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_SINGLE_TAP_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_SINGLE_TAP_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_INT1_SINGLE_TAP_t;

/*******************************************************************************
* Register      : MD1_CFG
* Address       : 0X5E
* Bit Group Name: INT1_SLEEP
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT1_SLEEP_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT1_SLEEP_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_INT1_SLEEP_t;

/*******************************************************************************
* Register      : MD2_CFG
* Address       : 0X5F
* Bit Group Name: INT2_TIMER
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_TIMER_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_TIMER_ENABLED 		 = 0x01,
} LSM6DS3_ACC_GYRO_INT2_TIMER_t;

/*******************************************************************************
* Register      : MD2_CFG
* Address       : 0X5F
* Bit Group Name: INT2_TILT
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_TILT_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_TILT_ENABLED 		 = 0x02,
} LSM6DS3_ACC_GYRO_INT2_TILT_t;

/*******************************************************************************
* Register      : MD2_CFG
* Address       : 0X5F
* Bit Group Name: INT2_6D
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_6D_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_6D_ENABLED 		 = 0x04,
} LSM6DS3_ACC_GYRO_INT2_6D_t;

/*******************************************************************************
* Register      : MD2_CFG
* Address       : 0X5F
* Bit Group Name: INT2_TAP
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_TAP_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_TAP_ENABLED 		 = 0x08,
} LSM6DS3_ACC_GYRO_INT2_TAP_t;

/*******************************************************************************
* Register      : MD2_CFG
* Address       : 0X5F
* Bit Group Name: INT2_FF
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_FF_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_FF_ENABLED 		 = 0x10,
} LSM6DS3_ACC_GYRO_INT2_FF_t;

/*******************************************************************************
* Register      : MD2_CFG
* Address       : 0X5F
* Bit Group Name: INT2_WU
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_WU_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_WU_ENABLED 		 = 0x20,
} LSM6DS3_ACC_GYRO_INT2_WU_t;

/*******************************************************************************
* Register      : MD2_CFG
* Address       : 0X5F
* Bit Group Name: INT2_SINGLE_TAP
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_SINGLE_TAP_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_SINGLE_TAP_ENABLED 		 = 0x40,
} LSM6DS3_ACC_GYRO_INT2_SINGLE_TAP_t;

/*******************************************************************************
* Register      : MD2_CFG
* Address       : 0X5F
* Bit Group Name: INT2_SLEEP
* Permission    : RW
*******************************************************************************/
typedef enum {
	LSM6DS3_ACC_GYRO_INT2_SLEEP_DISABLED 		 = 0x00,
	LSM6DS3_ACC_GYRO_INT2_SLEEP_ENABLED 		 = 0x80,
} LSM6DS3_ACC_GYRO_INT2_SLEEP_t;


#endif /* LSM6DS3_H_ */
