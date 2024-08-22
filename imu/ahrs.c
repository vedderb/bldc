//=====================================================================================================
// MahonyAHRS.c
//=====================================================================================================
//
// Madgwick's implementation of Mayhony's AHRS algorithm.
// See: http://www.x-io.co.uk/node/8#open_source_ahrs_and_imu_algorithms
//
// Date			Author			Notes
// 29/09/2011	SOH Madgwick    Initial release
// 02/10/2011	SOH Madgwick	Optimised for reduced CPU load
// 26/01/2014	Benjamin V		Adaption to our platform
// 20/02/2017	Benjamin V		Added Madgwick algorithm and refactoring
//
//=====================================================================================================

// Header files
#include <ahrs.h>
#include "utils_math.h"
#include <math.h>

// Private functions
static float invSqrt(float x);
static float calculateAccConfidence(float accMag, float *accMagP, float acc_confidence_decay);

static float calculateAccConfidence(float accMag, float *accMagP, float acc_confidence_decay) {
	// G.K. Egan (C) computes confidence in accelerometers when
	// aircraft is being accelerated over and above that due to gravity

	float confidence;

	accMag = *accMagP * 0.9f + accMag * 0.1f;
	*accMagP = accMag;

	confidence = 1.0 - (acc_confidence_decay * sqrtf(fabsf(accMag - 1.0f)));
	utils_truncate_number(&confidence, 0.0, 1.0);

	return confidence;
}

void ahrs_update_all_parameters(ATTITUDE_INFO *att, float confidence_decay, float kp, float ki, float beta) {
	att->acc_confidence_decay = confidence_decay;
	att->kp = kp;
	att->ki = ki;
	att->beta = beta;
}

void ahrs_init_attitude_info(ATTITUDE_INFO *att) {
	att->q0 = 1.0;
	att->q1 = 0.0;
	att->q2 = 0.0;
	att->q3 = 0.0;
	att->integralFBx = 0.0;
	att->integralFBy = 0.0;
	att->integralFBz = 0.0;
	att->accMagP = 1.0;
	att->initialUpdateDone = 0;
}

void ahrs_update_initial_orientation(float *accelXYZ, float *magXYZ, ATTITUDE_INFO *att) {
	// See https://cache.freescale.com/files/sensors/doc/app_note/AN4248.pdf
	// and http://sedris.org/wg8home/Documents/WG80485.pdf

	float ax = accelXYZ[0];
	float ay = accelXYZ[1];
	float az = accelXYZ[2];

	float mx = -magXYZ[0];
	float my = magXYZ[1];
	float mz = magXYZ[2];

	float roll = atan2f(-ay, az);
	float sr = sinf(roll);
	float cr = cosf(roll);

	float pitch = atanf(-ax / (-ay * sr + az * cr));
	float sp = sinf(pitch);
	float cp = cosf(pitch);

	float c_mx = mx * cp + my * sr * sp + mz * sp * cr;
	float c_my = my * cr - mz * sr;
	float yaw = atan2f(-c_my, c_mx) - M_PI / 2.0;
	utils_norm_angle_rad(&yaw);

	cr = cosf(-roll * 0.5f);
	sr = sinf(-roll * 0.5f);
	cp = cosf(pitch * 0.5f);
	sp = sinf(pitch * 0.5f);
	float cy = cosf(-yaw * 0.5f);
	float sy = sinf(-yaw * 0.5f);

	att->q0 = cr * cp * cy + sr * sp * sy;
	att->q1 = sr * cp * cy - cr * sp * sy;
	att->q2 = cr * sp * cy + sr * cp * sy;
	att->q3 = cr * cp * sy - sr * sp * cy;
}

void ahrs_update_mahony_imu(float *gyroXYZ, float *accelXYZ, float dt, ATTITUDE_INFO *att) {
	float accelNorm, recipNorm;
	float qa, qb, qc;

	float gx = gyroXYZ[0];
	float gy = gyroXYZ[1];
	float gz = gyroXYZ[2];

	float ax = accelXYZ[0];
	float ay = accelXYZ[1];
	float az = accelXYZ[2];

	accelNorm = sqrtf(ax * ax + ay * ay + az * az);

	// Compute feedback only if accelerometer abs(vector)is not too small to avoid a division
	// by a small number
	if (accelNorm > 0.01) {
		float halfvx, halfvy, halfvz;
		float halfex, halfey, halfez;
		float accelConfidence;

		volatile float twoKp = 2.0 * att->kp;
		volatile float twoKi = 2.0 * att->ki;

		accelConfidence = calculateAccConfidence(accelNorm, &att->accMagP, att->acc_confidence_decay);
		twoKp *= accelConfidence;
		twoKi *= accelConfidence;

		// Normalise accelerometer measurement
		recipNorm = invSqrt(ax * ax + ay * ay + az * az);
		ax *= recipNorm;
		ay *= recipNorm;
		az *= recipNorm;

		// Estimated direction of gravity and vector perpendicular to magnetic flux
		halfvx = att->q1 * att->q3 - att->q0 * att->q2;
		halfvy = att->q0 * att->q1 + att->q2 * att->q3;
		halfvz = att->q0 * att->q0 - 0.5f + att->q3 * att->q3;

		// Error is sum of cross product between estimated and measured direction of gravity
		halfex = (ay * halfvz - az * halfvy);
		halfey = (az * halfvx - ax * halfvz);
		halfez = (ax * halfvy - ay * halfvx);

		// Compute and apply integral feedback if enabled
		if(twoKi > 0.0f) {
			att->integralFBx += twoKi * halfex * dt;	// integral error scaled by Ki
			att->integralFBy += twoKi * halfey * dt;
			att->integralFBz += twoKi * halfez * dt;
			gx += att->integralFBx;	// apply integral feedback
			gy += att->integralFBy;
			gz += att->integralFBz;
		} else {
			att->integralFBx = 0.0f;	// prevent integral windup
			att->integralFBy = 0.0f;
			att->integralFBz = 0.0f;
		}

		// Apply proportional feedback
		gx += twoKp * halfex;
		gy += twoKp * halfey;
		gz += twoKp * halfez;
	}

	// Integrate rate of change of quaternion
	gx *= (0.5f * dt);		// pre-multiply common factors
	gy *= (0.5f * dt);
	gz *= (0.5f * dt);
	qa = att->q0;
	qb = att->q1;
	qc = att->q2;
	att->q0 += (-qb * gx - qc * gy - att->q3 * gz);
	att->q1 += (qa * gx + qc * gz - att->q3 * gy);
	att->q2 += (qa * gy - qb * gz + att->q3 * gx);
	att->q3 += (qa * gz + qb * gy - qc * gx);

	// Normalize quaternion
	recipNorm = invSqrt(att->q0 * att->q0 + att->q1 * att->q1 + att->q2 * att->q2 + att->q3 * att->q3);
	att->q0 *= recipNorm;
	att->q1 *= recipNorm;
	att->q2 *= recipNorm;
	att->q3 *= recipNorm;
}

void ahrs_update_madgwick_imu(float *gyroXYZ, float *accelXYZ, float dt, ATTITUDE_INFO *att) {
	float accelNorm, recipNorm;
	float qDot1, qDot2, qDot3, qDot4;

	float q0 = att->q0;
	float q1 = att->q1;
	float q2 = att->q2;
	float q3 = att->q3;

	float gx = gyroXYZ[0];
	float gy = gyroXYZ[1];
	float gz = gyroXYZ[2];

	float ax = accelXYZ[0];
	float ay = accelXYZ[1];
	float az = accelXYZ[2];

	// Rate of change of quaternion from gyroscope
	qDot1 = 0.5f * (-q1 * gx - q2 * gy - q3 * gz);
	qDot2 = 0.5f * (q0 * gx + q2 * gz - q3 * gy);
	qDot3 = 0.5f * (q0 * gy - q1 * gz + q3 * gx);
	qDot4 = 0.5f * (q0 * gz + q1 * gy - q2 * gx);

	accelNorm = sqrtf(ax * ax + ay * ay + az * az);

	// Compute feedback only if accelerometer abs(vector)is not too small to avoid a division
	// by a small number
	if (accelNorm > 0.01) {
		float _2q0, _2q1, _2q2, _2q3, _4q0, _4q1, _4q2 ,_8q1, _8q2, q0q0, q1q1, q2q2, q3q3;
		float s0, s1, s2, s3;
		float accelConfidence;

		// Normalise accelerometer measurement
		recipNorm = invSqrt(ax * ax + ay * ay + az * az);
		ax *= recipNorm;
		ay *= recipNorm;
		az *= recipNorm;

		// Auxiliary variables to avoid repeated arithmetic
		_2q0 = 2.0f * q0;
		_2q1 = 2.0f * q1;
		_2q2 = 2.0f * q2;
		_2q3 = 2.0f * q3;
		_4q0 = 4.0f * q0;
		_4q1 = 4.0f * q1;
		_4q2 = 4.0f * q2;
		_8q1 = 8.0f * q1;
		_8q2 = 8.0f * q2;
		q0q0 = q0 * q0;
		q1q1 = q1 * q1;
		q2q2 = q2 * q2;
		q3q3 = q3 * q3;

		// Gradient decent algorithm corrective step
		s0 = _4q0 * q2q2 + _2q2 * ax + _4q0 * q1q1 - _2q1 * ay;
		s1 = _4q1 * q3q3 - _2q3 * ax + 4.0f * q0q0 * q1 - _2q0 * ay - _4q1 + _8q1 * q1q1 + _8q1 * q2q2 + _4q1 * az;
		s2 = 4.0f * q0q0 * q2 + _2q0 * ax + _4q2 * q3q3 - _2q3 * ay - _4q2 + _8q2 * q1q1 + _8q2 * q2q2 + _4q2 * az;
		s3 = 4.0f * q1q1 * q3 - _2q1 * ax + 4.0f * q2q2 * q3 - _2q2 * ay;
		recipNorm = invSqrt(s0 * s0 + s1 * s1 + s2 * s2 + s3 * s3); // normalise step magnitude
		s0 *= recipNorm;
		s1 *= recipNorm;
		s2 *= recipNorm;
		s3 *= recipNorm;

		// Apply feedback step
		accelConfidence = calculateAccConfidence(accelNorm, &att->accMagP, att->acc_confidence_decay);
		qDot1 -= att->beta * s0 * accelConfidence;
		qDot2 -= att->beta * s1 * accelConfidence;
		qDot3 -= att->beta * s2 * accelConfidence;
		qDot4 -= att->beta * s3 * accelConfidence;
	}

	// Integrate rate of change of quaternion to yield quaternion
	q0 += qDot1 * dt;
	q1 += qDot2 * dt;
	q2 += qDot3 * dt;
	q3 += qDot4 * dt;

	// Normalise quaternion
	recipNorm = invSqrt(q0 * q0 + q1 * q1 + q2 * q2 + q3 * q3);
	q0 *= recipNorm;
	q1 *= recipNorm;
	q2 *= recipNorm;
	q3 *= recipNorm;

	att->q0 = q0;
	att->q1 = q1;
	att->q2 = q2;
	att->q3 = q3;
}

float ahrs_get_roll(ATTITUDE_INFO *att) {
	const float q0 = att->q0;
	const float q1 = att->q1;
	const float q2 = att->q2;
	const float q3 = att->q3;

	return -atan2f(q0 * q1 + q2 * q3, 0.5 - (q1 * q1 + q2 * q2));
}

float ahrs_get_pitch(ATTITUDE_INFO *att) {
	const float q0 = att->q0;
	const float q1 = att->q1;
	const float q2 = att->q2;
	const float q3 = att->q3;

	return asinf(-2.0 * (q1 * q3 - q0 * q2));
}

float ahrs_get_yaw(ATTITUDE_INFO *att) {
	const float q0 = att->q0;
	const float q1 = att->q1;
	const float q2 = att->q2;
	const float q3 = att->q3;

	return -atan2f(q0 * q3 + q1 * q2, 0.5 - (q2 * q2 + q3 * q3));
}

void ahrs_get_roll_pitch_yaw(float *rpy, ATTITUDE_INFO *att) {
	// See http://math.stackexchange.com/questions/687964/getting-euler-tait-bryan-angles-from-quaternion-representation
	const float q0 = att->q0;
	const float q1 = att->q1;
	const float q2 = att->q2;
	const float q3 = att->q3;

	rpy[0] = -atan2f(q0 * q1 + q2 * q3, 0.5 - (q1 * q1 + q2 * q2));
	rpy[1] = asinf(-2.0 * (q1 * q3 - q0 * q2));
	rpy[2] = -atan2f(q0 * q3 + q1 * q2, 0.5 - (q2 * q2 + q3 * q3));
}

static float invSqrt(float x) {
	// Fast inverse square-root
	// See: http://en.wikipedia.org/wiki/Fast_inverse_square_root
	//	union {
	//		float as_float;
	//		long as_int;
	//	} un;
	//
	//	float xhalf = 0.5f*x;
	//	un.as_float = x;
	//	un.as_int = 0x5f3759df - (un.as_int >> 1);
	//	un.as_float = un.as_float * (1.5f - xhalf * un.as_float * un.as_float);
	//	return un.as_float;

	// Use normal inverse square root.
	// http://diydrones.com/forum/topics/madgwick-imu-ahrs-and-fast-inverse-square-root
	return 1.0 / sqrtf(x);
}
