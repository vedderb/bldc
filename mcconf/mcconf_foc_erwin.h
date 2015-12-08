/*
 * mcconf_foc_erwin.h
 *
 *  Created on: 25 okt 2015
 *      Author: benjamin
 */

#ifndef MCCONF_FOC_ERWIN_H_
#define MCCONF_FOC_ERWIN_H_

#define MCCONF_DEFAULT_MOTOR_TYPE		MOTOR_TYPE_FOC

#define MCCONF_FOC_CURRENT_KP			0.05
#define MCCONF_FOC_CURRENT_KI			50.0
#define MCCONF_FOC_F_SW					50000.0
#define MCCONF_FOC_ENCODER_INVERTED		true
#define MCCONF_FOC_ENCODER_OFFSET		92.0
#define MCCONF_FOC_ENCODER_RATIO		7.0
#define MCCONF_FOC_SENSOR_MODE			FOC_SENSOR_MODE_ENCODER
#define MCCONF_FOC_PLL_KP				40.0
#define MCCONF_FOC_PLL_KI				40000.0
#define MCCONF_FOC_MOTOR_L				0.000064
#define MCCONF_FOC_MOTOR_R				0.038
#define MCCONF_FOC_MOTOR_FLUX_LINKAGE	0.0085
#define MCCONF_FOC_OBSERVER_GAIN		9e6		// Can be something like 600 / L

#define MCCONF_S_PID_KP					0.0001
#define MCCONF_S_PID_KI					0.0005
#define MCCONF_S_PID_KD					0.0
#define MCCONF_S_PID_MIN_RPM			100.0

#define MCCONF_P_PID_KP					0.01
#define MCCONF_P_PID_KI					0.0
#define MCCONF_P_PID_KD					0.0006

#endif /* MCCONF_FOC_ERWIN_H_ */
