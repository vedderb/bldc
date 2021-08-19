/**
 * @file FusionBias.h
 * @author Seb Madgwick
 * @brief The gyroscope bias correction algorithm achieves run-time calibration
 * of the gyroscope bias.  The algorithm will detect when the gyroscope is
 * stationary for a set period of time and then begin to sample gyroscope
 * measurements to calculate the bias as an average.
 */

#ifndef FUSION_BIAS_H
#define FUSION_BIAS_H

//------------------------------------------------------------------------------
// Includes

#include "FusionTypes.h"
#include <stdbool.h>

//------------------------------------------------------------------------------
// Definitions

/**
 * @brief Gyroscope bias correction algorithm structure.  Structure members are
 * used internally and should not be used by the user application.
 */
typedef struct {
    float threshold;
    float filterCoefficient;
    float stationaryTimer;
    FusionVector3 gyroscopeBias;
} FusionBias;

//------------------------------------------------------------------------------
// Function prototypes

void FusionBiasInitialise(FusionBias * const fusionBias, const float threshold, const float samplePeriod);
FusionVector3 FusionBiasUpdate(FusionBias * const fusionBias, FusionVector3 gyroscope, float dt);
bool FusionBiasIsActive(FusionBias * const fusionBias);

#endif

//------------------------------------------------------------------------------
// End of file
