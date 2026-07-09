/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se
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

#include "bmi160_wrapper.h"
#include "ch.h"

/*
 * Anti-alias 3 dB cutoffs (poll mode only, DRDY interrupt support not implemented). The ODR
 * is pinned at max (accel 1600 Hz, gyro 3200 Hz) and IMU_FILTER selects the oversampling ratio
 * (acc_bwp/gyr_bwp); with the ODR fixed this acts as a fixed-cutoff selector. Datasheet table 12
 * (accel) and the gyroscope cutoff table.
 *
 * poll:
 *   ODR: accel 1600 Hz, gyro 3200 Hz
 *
 *   filter          accel          gyro
 *   LOW    (normal)  684 Hz         890 Hz
 *   MEDIUM (OSR2)    342 Hz         445 Hz
 *   HIGH   (OSR4)    171 Hz         222 Hz
 */

static struct bmi160_dev m_sensor;

// Bosch's read/write callbacks carry no context pointer, so the active transport is held
// here (single BMI instance at a time).
static transport_t *m_transport;

static int8_t bmi_read(uint8_t dev_addr, uint8_t reg_addr, uint8_t *data, uint16_t len) {
	return transport_read_reg(m_transport, dev_addr, reg_addr, data, len) ? BMI160_OK : BMI160_E_COM_FAIL;
}

static int8_t bmi_write(uint8_t dev_addr, uint8_t reg_addr, uint8_t *data, uint16_t len) {
	return transport_write_reg(m_transport, dev_addr, reg_addr, data, len) ? BMI160_OK : BMI160_E_COM_FAIL;
}

static void user_delay_ms(uint32_t ms) {
	chThdSleepMilliseconds(ms);
}

static bool reset_init_bmi(struct bmi160_dev *sensor, IMU_FILTER filter) {
	sensor->delay_ms = user_delay_ms;

	bmi160_init(sensor);

	// Poll scheme (mirrors the LSM6DS3 non-DRDY path): run both sensors at their max ODR so the
	// asynchronous poll always reads the freshest sample.
	// Unlike the LSM6DS3 the BMI160 has no absolute-Hz anti-alias filter: acc_bwp/gyr_bwp only pick
	// an oversampling ratio whose 3dB cutoff is a fixed fraction of the ODR (normal power mode).
	// With the ODR pinned at max, that ratio acts as a fixed-cutoff selector, so IMU_FILTER maps
	// straight onto it. The narrowest cutoff is tied to the max ODR (accel 1600 Hz is the binding
	// constraint), so a sample rate below ~350 Hz cannot be fully anti-aliased (a limit of using
	// this approach).
	sensor->accel_cfg.range = BMI160_ACCEL_RANGE_16G;
	sensor->accel_cfg.power = BMI160_ACCEL_NORMAL_MODE;
	sensor->accel_cfg.odr = BMI160_ACCEL_ODR_1600HZ;

	sensor->gyro_cfg.range = BMI160_GYRO_RANGE_2000_DPS;
	sensor->gyro_cfg.power = BMI160_GYRO_NORMAL_MODE;
	sensor->gyro_cfg.odr = BMI160_GYRO_ODR_3200HZ;

	if (filter == IMU_FILTER_HIGH) {
		sensor->accel_cfg.bw = BMI160_ACCEL_BW_OSR4_AVG1;
		sensor->gyro_cfg.bw = BMI160_GYRO_BW_OSR4_MODE;
	} else if (filter == IMU_FILTER_MEDIUM) {
		sensor->accel_cfg.bw = BMI160_ACCEL_BW_OSR2_AVG2;
		sensor->gyro_cfg.bw = BMI160_GYRO_BW_OSR2_MODE;
	} else { // IMU_FILTER_LOW
		sensor->accel_cfg.bw = BMI160_ACCEL_BW_NORMAL_AVG4;
		sensor->gyro_cfg.bw = BMI160_GYRO_BW_NORMAL_MODE;
	}

	chThdSleepMilliseconds(50);
	int8_t res = bmi160_set_sens_conf(sensor);
	chThdSleepMilliseconds(50);

	return res == BMI160_OK;
}

static bool configure(imu_device_t *dev, IMU_FILTER filter, bool use_mag) {
	(void)use_mag;

	struct bmi160_dev *sensor = dev->priv;
	m_transport = dev->transport;

	// The bus is encoded in dev_addr by the factory: an I2C slave address, or 0 for SPI.
	sensor->interface = dev->dev_addr ? BMI160_I2C_INTF : BMI160_SPI_INTF;
	sensor->id = dev->dev_addr;
	sensor->read = bmi_read;
	sensor->write = bmi_write;

	return reset_init_bmi(sensor, filter);
}

static bool read_sample(imu_device_t *dev, float accel[3], float gyro[3], float mag[3]) {
	struct bmi160_dev *sensor = dev->priv;
	struct bmi160_sensor_data a, g;

	if (bmi160_get_sensor_data(BMI160_ACCEL_SEL | BMI160_GYRO_SEL, &a, &g, sensor) != BMI160_OK) {
		return false;
	}

	accel[0] = (float)a.x * 16.0 / 32768.0;
	accel[1] = (float)a.y * 16.0 / 32768.0;
	accel[2] = (float)a.z * 16.0 / 32768.0;

	gyro[0] = (float)g.x * 2000.0 / 32768.0;
	gyro[1] = (float)g.y * 2000.0 / 32768.0;
	gyro[2] = (float)g.z * 2000.0 / 32768.0;

	mag[0] = 0.0;
	mag[1] = 0.0;
	mag[2] = 0.0;

	return true;
}

static void on_read_fail(imu_device_t *dev) {
	(void)dev;
	// The Bosch driver recovers on its own; just back off before the next read.
	chThdSleepMilliseconds(5);
}

static const imu_device_interface_t bmi160_interface = {
	.name = "BMI160",
	.configure = configure,
	.read_sample = read_sample,
	.on_read_fail = on_read_fail,
};

imu_device_t bmi160_device(transport_t *transport, uint8_t interface) {
	return (imu_device_t){
		.interface = &bmi160_interface,
		.transport = transport,
		.priv = &m_sensor,
		.dev_addr = (interface == BMI160_I2C_INTF) ? BMI160_I2C_ADDR : 0,
	};
}
