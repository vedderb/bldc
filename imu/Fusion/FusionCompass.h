/**
 * @file FusionCompass.h
 * @author Seb Madgwick
 * @brief The tilt-compensated compass calculates an angular heading relative to
 * magnetic north using accelerometer and magnetometer measurements (NWU
 * convention).
 */

#ifndef FUSION_COMPASS_H
#define FUSION_COMPASS_H

//------------------------------------------------------------------------------
// Includes

#include "FusionTypes.h"

//------------------------------------------------------------------------------
// Function prototypes

float FusionCompassCalculateHeading(const FusionVector3 accelerometer, const FusionVector3 magnetometer);

#endif

//------------------------------------------------------------------------------
// End of file
