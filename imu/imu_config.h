/*
	Copyright 2026 Lukas Hrazky

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

#ifndef IMU_IMU_CONFIG_H_
#define IMU_IMU_CONFIG_H_

#include "hw.h"

// A board describes its internal IMU with a device (IMU model) selector, a
// transport selector and a set of pins. Example:
//
//   #define IMU_DEV   IMU_DEV_LSM6DS3
//   #define IMU_COM   IMU_COM_SPI_HW
//   #define IMU_SPI_NSS_GPIO/_PIN, _SCK_, _MOSI_, _MISO_   (SPI transports)
//   #define IMU_SPI_DEV / IMU_SPI_AF                       (hardware SPI only)
//   #define IMU_I2C_SDA_GPIO/_PIN, _SCL_                   (I2C transport)
//   #define IMU_BUS_SPEED_HZ                               (optional bus clock in Hz; 0/unset = transport default)
//
// A board may also declare a fallback transport that imu_init switches to when the
// IMU does not answer on the primary bus (e.g. SPI on new hardware, I2C on old). It
// mirrors the primary selectors with an IMU_FALLBACK_ prefix:
//
//   #define IMU_FALLBACK_COM   IMU_COM_I2C_BB              (only IMU_COM_I2C_BB is supported)
//   #define IMU_FALLBACK_I2C_SDA_GPIO/_PIN, _SCL_          (fallback I2C pins)
//   #define IMU_FALLBACK_BUS_SPEED_HZ 700000               (optional; 0/unset = transport default)

// IMU model:
#define IMU_DEV_NONE		0
#define IMU_DEV_MPU9X50		1
#define IMU_DEV_ICM20948	2
#define IMU_DEV_BMI160		3
#define IMU_DEV_LSM6DS3		4

// IMU transport:
#define IMU_COM_NONE		0
#define IMU_COM_I2C_BB		1 // bit-bang I2C
#define IMU_COM_SPI_BB		2 // bit-bang SPI
#define IMU_COM_SPI_HW		3 // hardware SPI (DMA)
#define IMU_COM_I2C_BB_SPI	4 // bit-bang I2C over the SPI-labelled pins (NSS parked high, MISO low)

// ---- Legacy IMU_* config macro support ----
#if !defined(IMU_DEV)

#if defined(MPU9X50_SDA_GPIO)
#define IMU_DEV				IMU_DEV_MPU9X50
#define IMU_COM				IMU_COM_I2C_BB
#define IMU_I2C_SDA_GPIO	MPU9X50_SDA_GPIO
#define IMU_I2C_SDA_PIN		MPU9X50_SDA_PIN
#define IMU_I2C_SCL_GPIO	MPU9X50_SCL_GPIO
#define IMU_I2C_SCL_PIN		MPU9X50_SCL_PIN

#elif defined(ICM20948_SDA_GPIO)
#define IMU_DEV				IMU_DEV_ICM20948
#define IMU_COM				IMU_COM_I2C_BB
#define IMU_I2C_SDA_GPIO	ICM20948_SDA_GPIO
#define IMU_I2C_SDA_PIN		ICM20948_SDA_PIN
#define IMU_I2C_SCL_GPIO	ICM20948_SCL_GPIO
#define IMU_I2C_SCL_PIN		ICM20948_SCL_PIN

#elif defined(BMI160_SDA_GPIO)
#define IMU_DEV				IMU_DEV_BMI160
#define IMU_COM				IMU_COM_I2C_BB
#define IMU_I2C_SDA_GPIO	BMI160_SDA_GPIO
#define IMU_I2C_SDA_PIN		BMI160_SDA_PIN
#define IMU_I2C_SCL_GPIO	BMI160_SCL_GPIO
#define IMU_I2C_SCL_PIN		BMI160_SCL_PIN

#elif defined(BMI160_SPI_PORT_NSS)
#define IMU_DEV				IMU_DEV_BMI160
#define IMU_COM				IMU_COM_SPI_BB
#define IMU_SPI_NSS_GPIO	BMI160_SPI_PORT_NSS
#define IMU_SPI_NSS_PIN		BMI160_SPI_PIN_NSS
#define IMU_SPI_SCK_GPIO	BMI160_SPI_PORT_SCK
#define IMU_SPI_SCK_PIN		BMI160_SPI_PIN_SCK
#define IMU_SPI_MOSI_GPIO	BMI160_SPI_PORT_MOSI
#define IMU_SPI_MOSI_PIN	BMI160_SPI_PIN_MOSI
#define IMU_SPI_MISO_GPIO	BMI160_SPI_PORT_MISO
#define IMU_SPI_MISO_PIN	BMI160_SPI_PIN_MISO

#elif defined(LSM6DS3_SDA_GPIO)
#define IMU_DEV				IMU_DEV_LSM6DS3
#define IMU_COM				IMU_COM_I2C_BB
#define IMU_I2C_SDA_GPIO	LSM6DS3_SDA_GPIO
#define IMU_I2C_SDA_PIN		LSM6DS3_SDA_PIN
#define IMU_I2C_SCL_GPIO	LSM6DS3_SCL_GPIO
#define IMU_I2C_SCL_PIN		LSM6DS3_SCL_PIN

#elif defined(LSM6DS3_USE_SPI)
#define IMU_DEV				IMU_DEV_LSM6DS3
#if defined(LSM6DS3_HWSPI_DEV)
#define IMU_COM				IMU_COM_SPI_HW
#define IMU_SPI_DEV			LSM6DS3_HWSPI_DEV
#define IMU_SPI_AF			LSM6DS3_HWSPI_AF
#else
#define IMU_COM				IMU_COM_SPI_BB
#endif
#define IMU_SPI_NSS_GPIO	LSM6DS3_NSS_GPIO
#define IMU_SPI_NSS_PIN		LSM6DS3_NSS_PIN
#define IMU_SPI_SCK_GPIO	LSM6DS3_SCK_GPIO
#define IMU_SPI_SCK_PIN		LSM6DS3_SCK_PIN
#define IMU_SPI_MOSI_GPIO	LSM6DS3_MOSI_GPIO
#define IMU_SPI_MOSI_PIN	LSM6DS3_MOSI_PIN
#define IMU_SPI_MISO_GPIO	LSM6DS3_MISO_GPIO
#define IMU_SPI_MISO_PIN	LSM6DS3_MISO_PIN

#elif defined(LSM6DS3_NSS_GPIO)
// LSM6DS3 on the SPI pins but driven as bit-bang I2C (the legacy fallback).
#define IMU_DEV				IMU_DEV_LSM6DS3
#define IMU_COM				IMU_COM_I2C_BB_SPI
#define IMU_SPI_NSS_GPIO	LSM6DS3_NSS_GPIO
#define IMU_SPI_NSS_PIN		LSM6DS3_NSS_PIN
#define IMU_SPI_SCK_GPIO	LSM6DS3_SCK_GPIO
#define IMU_SPI_SCK_PIN		LSM6DS3_SCK_PIN
#define IMU_SPI_MOSI_GPIO	LSM6DS3_MOSI_GPIO
#define IMU_SPI_MOSI_PIN	LSM6DS3_MOSI_PIN
#define IMU_SPI_MISO_GPIO	LSM6DS3_MISO_GPIO
#define IMU_SPI_MISO_PIN	LSM6DS3_MISO_PIN

#endif

#if defined(LSM6DS3_SPEED_700KHZ)
#define IMU_BUS_SPEED_HZ	700000
#endif

#ifdef IMU_DEV
#warning The LSM6DS3_*/MPU9X50_*/BMI160_*/ICM20948_* are deprecated. Please migrate to IMU_DEV / IMU_COM / IMU_SPI_* / IMU_I2C_* (see the example at the top of imu/imu_config.h or existing configs in hwconf/).
#endif

#endif // !defined(IMU_DEV)
// ---- End of legacy IMU_* config macro support ----

// ---- Defaults ----
#ifndef IMU_DEV
#define IMU_DEV				IMU_DEV_NONE
#endif
#ifndef IMU_COM
#define IMU_COM				IMU_COM_NONE
#endif
// 0 lets each transport pick its own default bus clock.
#ifndef IMU_BUS_SPEED_HZ
#define IMU_BUS_SPEED_HZ	0
#endif
#ifdef IMU_FALLBACK_COM
#ifndef IMU_FALLBACK_BUS_SPEED_HZ
#define IMU_FALLBACK_BUS_SPEED_HZ	0
#endif
#endif

#endif /* IMU_IMU_CONFIG_H_ */
