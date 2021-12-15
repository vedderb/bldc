/*
 * Ramp.h
 *
 *  Created on: 09.11.2018
 *      Author: LK
 */

#ifndef TMC_RAMP_RAMP_H_
#define TMC_RAMP_RAMP_H_

#include "LinearRamp1.h"

typedef enum {
	TMC_RAMP_TYPE_LINEAR
} TMC_RampType;

// Initializes ramp parameters for given type
void tmc_ramp_init(void *ramp, TMC_RampType type);

// Computes new ramp state after delta ticks have passed
// Note: To call this function periodically with a fixed delta-time, use delta = 1 and
// define the units of acceleration as v/delta-time. If you want to specify a different unit,
// change delta to your preference.
// Returns the position difference of the calculation.
int32_t tmc_ramp_compute(void *ramp, TMC_RampType type, uint32_t delta);

// Returns the current ramp velocity computed by the given ramp
int32_t tmc_ramp_get_rampVelocity(void *ramp, TMC_RampType type);

// Returns the current ramp position computed by the given ramp
int32_t tmc_ramp_get_rampPosition(void *ramp, TMC_RampType type);

// Enable/disable ramps
bool tmc_ramp_get_enabled(void *ramp, TMC_RampType type);
void tmc_ramp_set_enabled(void *ramp, TMC_RampType type, bool enabled);
void tmc_ramp_toggle_enabled(void *ramp, TMC_RampType type);

#endif /* TMC_RAMP_RAMP_H_ */
