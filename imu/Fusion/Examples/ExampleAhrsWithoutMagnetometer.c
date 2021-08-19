#include "../Fusion.h"
#include <stdio.h>

FusionBias fusionBias;
FusionAhrs fusionAhrs;

float samplePeriod = 0.01f; // replace this value with actual sample period in seconds

FusionVector3 gyroscopeSensitivity = {
    .axis.x = 1.0f,
    .axis.y = 1.0f,
    .axis.z = 1.0f,
}; // replace these values with actual sensitivity in degrees per second per lsb as specified in gyroscope datasheet

FusionVector3 accelerometerSensitivity = {
    .axis.x = 1.0f,
    .axis.y = 1.0f,
    .axis.z = 1.0f,
}; // replace these values with actual sensitivity in g per lsb as specified in accelerometer datasheet

int main() {

    // Initialise gyroscope bias correction algorithm
    FusionBiasInitialise(&fusionBias, 0.5f, samplePeriod); // stationary threshold = 0.5 degrees per second

    // Initialise AHRS algorithm
    FusionAhrsInitialise(&fusionAhrs, 0.5f); // gain = 0.5

    // The contents of this do while loop should be called for each time new sensor measurements are available
    do {

        // Calibrate gyroscope
        FusionVector3 uncalibratedGyroscope = {
            .axis.x = 0.0f, /* replace this value with actual gyroscope x axis measurement in lsb */
            .axis.y = 0.0f, /* replace this value with actual gyroscope y axis measurement in lsb */
            .axis.z = 0.0f, /* replace this value with actual gyroscope z axis measurement in lsb */
        };
        FusionVector3 calibratedGyroscope = FusionCalibrationInertial(uncalibratedGyroscope, FUSION_ROTATION_MATRIX_IDENTITY, gyroscopeSensitivity, FUSION_VECTOR3_ZERO);

        // Calibrate accelerometer
        FusionVector3 uncalibratedAccelerometer = {
            .axis.x = 0.0f, /* replace this value with actual accelerometer x axis measurement in lsb */
            .axis.y = 0.0f, /* replace this value with actual accelerometer y axis measurement in lsb */
            .axis.z = 1.0f, /* replace this value with actual accelerometer z axis measurement in lsb */
        };
        FusionVector3 calibratedAccelerometer = FusionCalibrationInertial(uncalibratedAccelerometer, FUSION_ROTATION_MATRIX_IDENTITY, accelerometerSensitivity, FUSION_VECTOR3_ZERO);

        // Update gyroscope bias correction algorithm
        calibratedGyroscope = FusionBiasUpdate(&fusionBias, calibratedGyroscope);

        // Update AHRS algorithm
        FusionAhrsUpdateWithoutMagnetometer(&fusionAhrs, calibratedGyroscope, calibratedAccelerometer, samplePeriod);

        // Print Euler angles
        FusionEulerAngles eulerAngles = FusionQuaternionToEulerAngles(FusionAhrsGetQuaternion(&fusionAhrs));
        printf("Roll = %0.1f, Pitch = %0.1f, Yaw = %0.1f\r\n", eulerAngles.angle.roll, eulerAngles.angle.pitch, eulerAngles.angle.yaw);

    } while (false);
}
