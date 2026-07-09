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

#ifndef IMU_THREAD_H_
#define IMU_THREAD_H_

#include "device.h"

// Bind the active IMU device at the given sample rate and register the debug terminal
// commands. The rate is stored on the device, where its configure() may override it.
void imu_thread_set_device(imu_device_t *dev, uint16_t rate_hz);

// Start a thread that reads samples from the IMU device (at its sample_rate_hz) and hands
// them to cb(accel, gyro, mag). A device must be set with imu_thread_set_device first.
void imu_thread_start(void (*cb)(float *accel, float *gyro, float *mag));

// Stop the IMU thread and wait for it to exit.
void imu_thread_stop(void);

#endif /* IMU_THREAD_H_ */
