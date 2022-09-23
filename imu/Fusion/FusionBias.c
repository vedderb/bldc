/**
 * @file FusionBias.c
 * @author Seb Madgwick
 * @brief The gyroscope bias correction algorithm achieves run-time calibration
 * of the gyroscope bias.  The algorithm will detect when the gyroscope is
 * stationary for a set period of time and then begin to sample gyroscope
 * measurements to calculate the bias as an average.
 */

//------------------------------------------------------------------------------
// Includes

#include "FusionBias.h"
#include "math.h" // fabs

//------------------------------------------------------------------------------
// Definitions

/**
 * @brief Minimum stationary period (in seconds) after which the the algorithm
 * becomes active and begins sampling gyroscope measurements.
 */
#define STATIONARY_PERIOD (5.0f)

/**
 * @brief Corner frequency (in Hz) of the high-pass filter used to sample the
 * gyroscope bias.
 */
#define CORNER_FREQUENCY (0.02f)

//------------------------------------------------------------------------------
// Functions

/**
 * @brief Initialises the gyroscope bias correction algorithm.
 * @param fusionBias FusionBias structure.
 * @param threshold Gyroscope threshold (in degrees per second) below which the
 * gyroscope is detected stationary.
 * @param samplePeriod Nominal sample period (in seconds) corresponding the rate
 * at which the application will update the algorithm.
 */
void FusionBiasInitialise(FusionBias * const fusionBias, const float threshold, const float samplePeriod) {
    fusionBias->threshold = threshold;
    fusionBias->filterCoefficient = (2.0f * M_PI * CORNER_FREQUENCY) * samplePeriod;
    fusionBias->stationaryTimer = 0.0f;
    fusionBias->gyroscopeBias = FUSION_VECTOR3_ZERO;
}

/**
 * @brief Updates the gyroscope bias correction algorithm and returns the
 * corrected gyroscope measurement.
 * @param fusionBias FusionBias structure.
 * @param gyroscope Gyroscope measurement in degrees per second.
 * @param dt Nominal sample period (in seconds) corresponding the rate
 * @return Corrected gyroscope measurement in degrees per second.
 */
FusionVector3 FusionBiasUpdate(FusionBias * const fusionBias, FusionVector3 gyroscope, float dt) {

    // Subtract bias from gyroscope measurement
    gyroscope = FusionVectorSubtract(gyroscope, fusionBias->gyroscopeBias);

    // Reset stationary timer if gyroscope not stationary
    if ((fabsf(gyroscope.axis.x) > fusionBias->threshold) || (fabsf(gyroscope.axis.y) > fusionBias->threshold) || (fabsf(gyroscope.axis.z) > fusionBias->threshold)) {
        fusionBias->stationaryTimer = 0.0f;
        return gyroscope;
    }

    // Increment stationary timer while gyroscope stationary
    if (fusionBias->stationaryTimer < STATIONARY_PERIOD) {
        fusionBias->stationaryTimer += dt;
        return gyroscope;
    }

    // Adjust bias if stationary timer has elapsed
    fusionBias->gyroscopeBias = FusionVectorAdd(fusionBias->gyroscopeBias, FusionVectorMultiplyScalar(gyroscope, fusionBias->filterCoefficient));
    return gyroscope;
}

/**
 * @brief Returns true if the gyroscope bias correction algorithm is active.
 * @param fusionBias FusionBias structure.
 * @return True if the gyroscope bias correction algorithm is active.
 */
bool FusionBiasIsActive(FusionBias * const fusionBias) {
    return fusionBias->stationaryTimer >= STATIONARY_PERIOD;
}

//------------------------------------------------------------------------------
// End of file
