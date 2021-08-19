/**
 * @file FusionCalibration.h
 * @author Seb Madgwick
 * @brief Gyroscope, accelerometer, and magnetometer calibration model.
 *
 * Static inline implementations are used to optimise for increased execution
 * speed.
 */

#ifndef FUSION_CALIBRATION_H
#define FUSION_CALIBRATION_H

//------------------------------------------------------------------------------
// Includes

#include "FusionTypes.h"

//------------------------------------------------------------------------------
// Inline functions

/**
 * @brief Gyroscope and accelerometer calibration model.
 * @param uncalibrated Uncalibrated gyroscope or accelerometer measurement in
 * lsb.
 * @param misalignment Misalignment matrix (may not be a true rotation matrix).
 * @param sensitivity Sensitivity in g per lsb for an accelerometer and degrees
 * per second per lsb for a gyroscope.
 * @param bias Bias in lsb.
 * @return Calibrated gyroscope or accelerometer measurement.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionCalibrationInertial(const FusionVector3 uncalibrated, const FusionRotationMatrix misalignment, const FusionVector3 sensitivity, const FusionVector3 bias) {
    return FusionRotationMatrixMultiplyVector(misalignment, FusionVectorHadamardProduct(FusionVectorSubtract(uncalibrated, bias), sensitivity));
}

/**
 * @brief Magnetometer calibration model.
 * @param magnetometer Uncalibrated magnetometer measurement in uT.
 * @param softIronMatrix Soft-iron matrix (may not be a true rotation matrix).
 * @param hardIronBias Hard-iron bias in uT.
 * @return Calibrated magnetometer measurement.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionCalibrationMagnetic(const FusionVector3 uncalibrated, const FusionRotationMatrix softIronMatrix, const FusionVector3 hardIronBias) {
    return FusionVectorSubtract(FusionRotationMatrixMultiplyVector(softIronMatrix, uncalibrated), hardIronBias);
}

#endif

//------------------------------------------------------------------------------
// End of file
