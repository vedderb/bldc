#include "../Fusion.h"
#include <stdio.h>

FusionVector3 accelerometerSensitivity = {
    .axis.x = 1.0f,
    .axis.y = 1.0f,
    .axis.z = 1.0f,
}; // replace these values with actual sensitivity in g per lsb as specified in accelerometer datasheet

FusionVector3 hardIronBias = {
    .axis.x = 0.0f,
    .axis.y = 0.0f,
    .axis.z = 0.0f,
}; // replace these values with actual hard-iron bias in uT if known

int main() {

    // The contents of this do while loop should be called for each time new sensor measurements are available
    do {

        // Calibrate accelerometer
        FusionVector3 uncalibratedAccelerometer = {
            .axis.x = 0.0f, /* replace this value with actual accelerometer x axis measurement in lsb */
            .axis.y = 0.0f, /* replace this value with actual accelerometer y axis measurement in lsb */
            .axis.z = 1.0f, /* replace this value with actual accelerometer z axis measurement in lsb */
        };
        FusionVector3 calibratedAccelerometer = FusionCalibrationInertial(uncalibratedAccelerometer, FUSION_ROTATION_MATRIX_IDENTITY, accelerometerSensitivity, FUSION_VECTOR3_ZERO);

        // Calibrate magnetometer
        FusionVector3 uncalibratedMagnetometer = {
            .axis.x = 0.5f, /* replace this value with actual magnetometer x axis measurement in uT */
            .axis.y = 0.0f, /* replace this value with actual magnetometer y axis measurement in uT */
            .axis.z = 0.0f, /* replace this value with actual magnetometer z axis measurement in uT */
        };
        FusionVector3 calibratedMagnetometer = FusionCalibrationMagnetic(uncalibratedMagnetometer, FUSION_ROTATION_MATRIX_IDENTITY, hardIronBias);

        // Calculate heading
        float heading = FusionCompassCalculateHeading(calibratedAccelerometer, calibratedMagnetometer);

        // Print heading
        printf("Heading = %0.1f\r\n", heading);

    } while (false);
}
