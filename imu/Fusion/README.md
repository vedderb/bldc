# Fusion

Fusion is an ANSI C99 compliment sensor fusion library for sensor arrays of gyroscopes, accelerometers, and magnetometers.  Fusion was specifically developed for use with embedded systems and has been optimised for execution speed.  The library includes modules for: attitude and heading reference system (AHRS) sensor fusion, gyroscope bias correction, and a tilt-compensated compass.

## AHRS sensor fusion algorithm

The AHRS sensor fusion algorithm to combines gyroscope, accelerometer, and magnetometer measurements into a single measurement of orientation relative to the Earth (NWU convention).

The algorithm behaviour is governed by a gain.  A low gain will decrease the influence of the accelerometer and magnetometer so that the algorithm will better reject disturbances causes by translational motion and temporary magnetic distortions.  However, a low gain will also increase the risk of drift due to gyroscope calibration errors.  A typical gain value suitable for most applications is 0.5.

The algorithm allows the application to define a minimum and maximum valid magnetic field magnitude.  The algorithm will ignore magnetic measurements that fall outside of this range.  This allows the algorithm to reject magnetic measurements that do not represent the direction of magnetic North.  The typical magnitude of the Earth's magnetic field is between 20 uT and 70 uT.

The algorithm can be used without a magnetometer.  Measurements of orientation obtained using only gyroscope and accelerometer measurements can be expected to drift in the yaw component of orientation only.  The application can reset the drift in yaw by setting the yaw to a specified angle at any time.

The algorithm provides the measurement of orientation as a quaternion.  The library includes functions for converting this quaternion to a rotation matrix and Euler angles.

The algorithm also provides a measurement of linear acceleration and Earth acceleration.  Linear acceleration is equal to the accelerometer  measurement with the 1 g of gravity removed.  Earth acceleration is a measurement of linear acceleration in the Earth coordinate frame.

## Gyroscope bias correction algorithm

The gyroscope bias correction algorithm achieves run-time calibration of the gyroscope bias.  The algorithm will detect when the gyroscope is stationary for a set period of time and then begin to sample gyroscope measurements to calculate the bias as an average.

## Tilt-compensated compass

The tilt-compensated compass calculates an angular heading relative to magnetic north using accelerometer and magnetometer measurements (NWU convention).
