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

#ifndef EMA_H_
#define EMA_H_

#include <math.h>

/**
 * Calculate the constant of an exponential moving average filter from its cutoff frequency.
 *
 * This is a second order Taylor series approximation of 1 - e^-omega. Omega is limited to 0.5,
 * above which the approximation is no longer accurate, so the result never exceeds 0.375.
 *
 * @param cutoff_hz The cutoff frequency in Hz.
 * @param dt The time between filter updates in seconds.
 *
 * @return The filter constant.
 */
static inline float ema_alpha(float cutoff_hz, float dt) {
	float omega = 2.0f * M_PI * cutoff_hz * dt;
	if (omega > 0.5f) {
		omega = 0.5f;
	}
	return omega - 0.5f * omega * omega;
}

/**
 * Calculate the constant of an exponential moving average filter from its time constant.
 *
 * @param time_constant The time constant in seconds.
 * @param dt The time between filter updates in seconds.
 *
 * @return The filter constant.
 */
static inline float ema_alpha_tau(float time_constant, float dt) {
	return ema_alpha(1.0f / (2.0f * M_PI * time_constant), dt);
}

/**
 * Calculate the next value of an exponential moving average filter. Use this for volatile values,
 * which ema_step does not accept.
 *
 * @param value The current filtered value.
 * @param sample The next sample.
 * @param alpha The filter constant, range 0.0 to 1.0, where 1.0 gives the unfiltered value.
 *
 * @return The next filtered value.
 */
static inline float ema_next(float value, float sample, float alpha) {
	return value - alpha * (value - sample);
}

/**
 * Update the value of an exponential moving average filter in place.
 *
 * @param value The filtered value to update.
 * @param sample The next sample.
 * @param alpha The filter constant, range 0.0 to 1.0, where 1.0 gives the unfiltered value.
 */
static inline void ema_step(float *value, float sample, float alpha) {
	*value = ema_next(*value, sample, alpha);
}

#endif /* EMA_H_ */
