//=====================================================================================================
// MahonyAHRS.h
//=====================================================================================================
//
// Madgwick's implementation of Mayhony's AHRS algorithm.
// See: http://www.x-io.co.uk/node/8#open_source_ahrs_and_imu_algorithms
//
// Date			Author			Notes
// 29/09/2011	SOH Madgwick    Initial release
// 02/10/2011	SOH Madgwick	Optimised for reduced CPU load
//
//=====================================================================================================
#ifndef AHRS_h
#define AHRS_h

#include "conf_general.h"

// Function declarations
void ahrs_init_attitude_info(ATTITUDE_INFO *att);
void ahrs_update_all_parameters(ATTITUDE_INFO *att, float confidence_decay, float kp, float ki, float beta);
void ahrs_update_initial_orientation(float *accelXYZ, float *magXYZ, ATTITUDE_INFO *att);

void ahrs_update_mahony_imu(float *gyroXYZ, float *accelXYZ, float dt, ATTITUDE_INFO *att);
void ahrs_update_madgwick_imu(float *gyroXYZ, float *accelXYZ, float dt, ATTITUDE_INFO *att);

float ahrs_get_roll(ATTITUDE_INFO *att);
float ahrs_get_pitch(ATTITUDE_INFO *att);
float ahrs_get_yaw(ATTITUDE_INFO *att);
void ahrs_get_roll_pitch_yaw(float *rpy, ATTITUDE_INFO *att);

#endif
