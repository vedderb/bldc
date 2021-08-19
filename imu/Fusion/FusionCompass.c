/**
 * @file FusionCompass.c
 * @author Seb Madgwick
 * @brief The tilt-compensated compass calculates an angular heading relative to
 * magnetic north using accelerometer and magnetometer measurements (NWU
 * convention).
 */

//------------------------------------------------------------------------------
// Includes

#include "FusionCompass.h"
#include <math.h> // atan2f

//------------------------------------------------------------------------------
// Functions

/**
 * @brief Calculates the tilt-compensated heading relative to magnetic north.
 * @param accelerometer Accelerometer measurement in any calibrated units.
 * @param magnetometer Magnetometer measurement in any calibrated units.
 * @return Heading angle in degrees.
 */
float FusionCompassCalculateHeading(const FusionVector3 accelerometer, const FusionVector3 magnetometer) {

    // Compute direction of 'magnetic west' (Earth's y axis)
    const FusionVector3 magneticWest = FusionVectorFastNormalise(FusionVectorCrossProduct(accelerometer, magnetometer));

    // Compute direction of magnetic north (Earth's x axis)
    const FusionVector3 magneticNorth = FusionVectorFastNormalise(FusionVectorCrossProduct(magneticWest, accelerometer));

    // Calculate angular heading relative to magnetic north
    return FusionRadiansToDegrees(atan2f(magneticWest.axis.x, magneticNorth.axis.x));
}

//------------------------------------------------------------------------------
// End of file
