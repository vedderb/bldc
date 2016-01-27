/*
	Copyright 2015 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * mcpwm_foc.c
 *
 *  Created on: 10 okt 2015
 *      Author: benjamin
 */

#include "mcpwm_foc.h"
#include "mc_interface.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "digital_filter.h"
#include "utils.h"
#include "ledpwm.h"
#include "hw.h"
#include "terminal.h"
#include "encoder.h"
#include "commands.h"
#include "timeout.h"
#include <math.h>
#include <string.h>
#include <stdlib.h>

// Private types
typedef struct {
	float id_target;
	float iq_target;
	float max_duty;
	float duty_now;
	float phase;
	float i_alpha;
	float i_beta;
	float i_abs;
	float i_abs_filter;
	float i_bus;
	float v_bus;
	float v_alpha;
	float v_beta;
	float mod_d;
	float mod_q;
	float id;
	float iq;
	float id_filter;
	float iq_filter;
	float vd;
	float vq;
} motor_state_t;

typedef struct {
	int sample_num;
	float avg_current_tot;
	float avg_voltage_tot;
	bool measure_inductance_now;
	float measure_inductance_duty;
} mc_sample_t;

// Private variables
static volatile mc_configuration *m_conf;
static volatile mc_state m_state;
static volatile mc_control_mode m_control_mode;
static volatile motor_state_t m_motor_state;
static volatile int m_curr0_sum;
static volatile int m_curr1_sum;
static volatile int m_curr_samples;
static volatile int m_curr0_offset;
static volatile int m_curr1_offset;
static volatile bool m_phase_override;
static volatile float m_phase_now_override;
static volatile float m_duty_cycle_set;
static volatile float m_id_set;
static volatile float m_iq_set;
static volatile bool m_dccal_done;
static volatile bool m_output_on;
static volatile float m_pos_pid_set_pos;
static volatile float m_speed_pid_set_rpm;
static volatile float m_phase_now_observer;
static volatile float m_phase_now_observer_override;
static volatile bool m_phase_observer_override;
static volatile float m_phase_now_encoder;
static volatile float m_phase_now_encoder_no_index;
static volatile float m_observer_x1;
static volatile float m_observer_x2;
static volatile float m_pll_phase;
static volatile float m_pll_speed;
static volatile mc_sample_t m_samples;
static volatile int m_tachometer;
static volatile int m_tachometer_abs;
static volatile float last_inj_adc_isr_duration;

// Private functions
static void do_dc_cal(void);
void observer_update(float v_alpha, float v_beta, float i_alpha, float i_beta,
		float dt, volatile float *x1, volatile float *x2, volatile float *phase);
static void pll_run(float phase, float dt, volatile float *phase_var,
		volatile float *speed_var);
static void control_current(volatile motor_state_t *state_m, float dt);
static void svm(float alpha, float beta, uint32_t PWMHalfPeriod,
		uint32_t* tAout, uint32_t* tBout, uint32_t* tCout);
static void run_pid_control_pos(float dt);
static void run_pid_control_speed(float dt);
static void stop_pwm_hw(void);
static void start_pwm_hw(void);
static int read_hall(void);
static float correct_hall(float angle, float speed, float dt);

// Threads
static THD_WORKING_AREA(timer_thread_wa, 2048);
static THD_FUNCTION(timer_thread, arg);
static volatile bool timer_thd_stop;

// Constants
#define ONE_BY_SQRT3			(0.57735026919)
#define TWO_BY_SQRT3			(2.0f * 0.57735026919)
#define SQRT3_BY_2				(0.86602540378)

// Macros
#define TIMER_UPDATE_DUTY(duty1, duty2, duty3) \
		TIM1->CR1 |= TIM_CR1_UDIS; \
		TIM1->CCR1 = duty1; \
		TIM1->CCR2 = duty3; \
		TIM1->CCR3 = duty2; \
		TIM1->CR1 &= ~TIM_CR1_UDIS;

#define TIMER_UPDATE_SAMP(current_samp, voltage_samp) \
		TIM1->CR1 |= TIM_CR1_UDIS; \
		TIM8->CR1 |= TIM_CR1_UDIS; \
		TIM8->CCR1 = voltage_samp; \
		TIM1->CCR4 = current_samp; \
		TIM8->CCR2 = current_samp; \
		TIM1->CR1 &= ~TIM_CR1_UDIS; \
		TIM8->CR1 &= ~TIM_CR1_UDIS;

#define TIMER_UPDATE_SAMP_TOP(current_samp, voltage_samp, top) \
		TIM1->CR1 |= TIM_CR1_UDIS; \
		TIM8->CR1 |= TIM_CR1_UDIS; \
		TIM1->ARR = top; \
		TIM8->ARR = top; \
		TIM8->CCR1 = voltage_samp; \
		TIM1->CCR4 = current_samp; \
		TIM8->CCR2 = current_samp; \
		TIM1->CR1 &= ~TIM_CR1_UDIS; \
		TIM8->CR1 &= ~TIM_CR1_UDIS;

#define TIMER_UPDATE_DUTY_CURRENTSAMP(duty1, duty2, duty3, current_samp) \
		TIM1->CR1 |= TIM_CR1_UDIS; \
		TIM8->CR1 |= TIM_CR1_UDIS; \
		TIM1->CCR1 = duty1; \
		TIM1->CCR2 = duty3; \
		TIM1->CCR3 = duty2; \
		TIM1->CCR4 = current_samp; \
		TIM8->CCR2 = current_samp; \
		TIM1->CR1 &= ~TIM_CR1_UDIS; \
		TIM8->CR1 &= ~TIM_CR1_UDIS;

#define TIMER_DISABLE_PRELOAD_DUTY1()	TIM1->CCMR1 &= (uint16_t)(~TIM_CCMR1_OC1PE)
#define TIMER_DISABLE_PRELOAD_DUTY2()	TIM1->CCMR2 &= (uint16_t)(~TIM_CCMR2_OC3PE)
#define TIMER_DISABLE_PRELOAD_DUTY3()	TIM1->CCMR1 &= (uint16_t)(~TIM_CCMR1_OC2PE)

#define TIMER_ENABLE_PRELOAD_DUTY1()	TIM1->CCMR1 |= (uint16_t)(TIM_CCMR1_OC1PE)
#define TIMER_ENABLE_PRELOAD_DUTY2()	TIM1->CCMR2 |= (uint16_t)(TIM_CCMR2_OC3PE)
#define TIMER_ENABLE_PRELOAD_DUTY3()	TIM1->CCMR1 |= (uint16_t)(TIM_CCMR1_OC2PE)

void mcpwm_foc_init(volatile mc_configuration *configuration) {
	utils_sys_lock_cnt();

	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	TIM_BDTRInitTypeDef TIM_BDTRInitStructure;

	m_conf = configuration;

	// Initialize variables
	m_conf = configuration;
	m_state = MC_STATE_OFF;
	m_control_mode = CONTROL_MODE_NONE;
	m_curr0_sum = 0;
	m_curr1_sum = 0;
	m_curr_samples = 0;
	m_dccal_done = false;
	m_phase_override = false;
	m_phase_now_override = 0.0;
	m_duty_cycle_set = 0.0;
	m_id_set = 0.0;
	m_iq_set = 0.0;
	m_output_on = false;
	m_pos_pid_set_pos = 0.0;
	m_speed_pid_set_rpm = 0.0;
	m_phase_now_observer = 0.0;
	m_phase_now_observer_override = 0.0;
	m_phase_observer_override = false;
	m_phase_now_encoder = 0.0;
	m_phase_now_encoder_no_index = 0.0;
	m_observer_x1 = 0.0;
	m_observer_x2 = 0.0;
	m_pll_phase = 0.0;
	m_pll_speed = 0.0;
	m_tachometer = 0;
	m_tachometer_abs = 0;
	last_inj_adc_isr_duration = 0;
	memset((void*)&m_motor_state, 0, sizeof(motor_state_t));
	memset((void*)&m_samples, 0, sizeof(mc_sample_t));

	TIM_DeInit(TIM1);
	TIM_DeInit(TIM8);
	TIM1->CNT = 0;
	TIM8->CNT = 0;

	// TIM1 clock enable
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, ENABLE);

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_CenterAligned2; // compare flag when upcounting
	TIM_TimeBaseStructure.TIM_Period = SYSTEM_CORE_CLOCK / (int)m_conf->foc_f_sw;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 1; // Only generate update event on underflow

	TIM_TimeBaseInit(TIM1, &TIM_TimeBaseStructure);

	// Channel 1, 2 and 3 Configuration in PWM mode
	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_OutputNState = TIM_OutputNState_Enable;
	TIM_OCInitStructure.TIM_Pulse = TIM1->ARR / 2;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_High;
	TIM_OCInitStructure.TIM_OCIdleState = TIM_OCIdleState_Set;
	TIM_OCInitStructure.TIM_OCNIdleState = TIM_OCNIdleState_Set;

	TIM_OC1Init(TIM1, &TIM_OCInitStructure);
	TIM_OC2Init(TIM1, &TIM_OCInitStructure);
	TIM_OC3Init(TIM1, &TIM_OCInitStructure);
	TIM_OC4Init(TIM1, &TIM_OCInitStructure);

	TIM_OC1PreloadConfig(TIM1, TIM_OCPreload_Enable);
	TIM_OC2PreloadConfig(TIM1, TIM_OCPreload_Enable);
	TIM_OC3PreloadConfig(TIM1, TIM_OCPreload_Enable);
	TIM_OC4PreloadConfig(TIM1, TIM_OCPreload_Enable);

	// Automatic Output enable, Break, dead time and lock configuration
	TIM_BDTRInitStructure.TIM_OSSRState = TIM_OSSRState_Enable;
	TIM_BDTRInitStructure.TIM_OSSIState = TIM_OSSIState_Enable;
	TIM_BDTRInitStructure.TIM_LOCKLevel = TIM_LOCKLevel_OFF;
	TIM_BDTRInitStructure.TIM_DeadTime = MCPWM_DEAD_TIME_CYCLES;
	TIM_BDTRInitStructure.TIM_Break = TIM_Break_Disable;
	TIM_BDTRInitStructure.TIM_BreakPolarity = TIM_BreakPolarity_High;
	TIM_BDTRInitStructure.TIM_AutomaticOutput = TIM_AutomaticOutput_Disable;

	TIM_BDTRConfig(TIM1, &TIM_BDTRInitStructure);
	TIM_CCPreloadControl(TIM1, ENABLE);
	TIM_ARRPreloadConfig(TIM1, ENABLE);

	/*
	 * ADC!
	 */
	ADC_CommonInitTypeDef ADC_CommonInitStructure;
	DMA_InitTypeDef DMA_InitStructure;
	ADC_InitTypeDef ADC_InitStructure;

	// Clock
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_DMA2 | RCC_AHB1Periph_GPIOA | RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_ADC1 | RCC_APB2Periph_ADC2 | RCC_APB2Periph_ADC3, ENABLE);

	// DMA for the ADC
	DMA_InitStructure.DMA_Channel = DMA_Channel_0;
	DMA_InitStructure.DMA_Memory0BaseAddr = (uint32_t)&ADC_Value;
	DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&ADC->CDR;
	DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralToMemory;
	DMA_InitStructure.DMA_BufferSize = HW_ADC_CHANNELS;
	DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
	DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
	DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;
	DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;
	DMA_InitStructure.DMA_Mode = DMA_Mode_Circular;
	DMA_InitStructure.DMA_Priority = DMA_Priority_High;
	DMA_InitStructure.DMA_FIFOMode = DMA_FIFOMode_Disable;
	DMA_InitStructure.DMA_FIFOThreshold = DMA_FIFOThreshold_1QuarterFull;
	DMA_InitStructure.DMA_MemoryBurst = DMA_MemoryBurst_Single;
	DMA_InitStructure.DMA_PeripheralBurst = DMA_PeripheralBurst_Single;
	DMA_Init(DMA2_Stream4, &DMA_InitStructure);

	// DMA2_Stream0 enable
	DMA_Cmd(DMA2_Stream4, ENABLE);

	// ADC Common Init
	// Note that the ADC is running at 42MHz, which is higher than the
	// specified 36MHz in the data sheet, but it works.
	ADC_CommonInitStructure.ADC_Mode = ADC_TripleMode_RegSimult;
	ADC_CommonInitStructure.ADC_Prescaler = ADC_Prescaler_Div2;
	ADC_CommonInitStructure.ADC_DMAAccessMode = ADC_DMAAccessMode_1;
	ADC_CommonInitStructure.ADC_TwoSamplingDelay = ADC_TwoSamplingDelay_5Cycles;
	ADC_CommonInit(&ADC_CommonInitStructure);

	// Channel-specific settings
	ADC_InitStructure.ADC_Resolution = ADC_Resolution_12b;
	ADC_InitStructure.ADC_ScanConvMode = ENABLE;
	ADC_InitStructure.ADC_ContinuousConvMode = DISABLE;
	ADC_InitStructure.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_Falling;
	ADC_InitStructure.ADC_ExternalTrigConv = ADC_ExternalTrigConv_T8_CC1;
	ADC_InitStructure.ADC_DataAlign = ADC_DataAlign_Right;
	ADC_InitStructure.ADC_NbrOfConversion = HW_ADC_NBR_CONV;

	ADC_Init(ADC1, &ADC_InitStructure);
	ADC_InitStructure.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_None;
	ADC_InitStructure.ADC_ExternalTrigConv = 0;
	ADC_Init(ADC2, &ADC_InitStructure);
	ADC_Init(ADC3, &ADC_InitStructure);

	// Enable Vrefint channel
	ADC_TempSensorVrefintCmd(ENABLE);

	// Enable DMA request after last transfer (Multi-ADC mode)
	ADC_MultiModeDMARequestAfterLastTransferCmd(ENABLE);

	// Injected channels for current measurement at end of cycle
	ADC_ExternalTrigInjectedConvConfig(ADC1, ADC_ExternalTrigInjecConv_T1_CC4);
	ADC_ExternalTrigInjectedConvConfig(ADC2, ADC_ExternalTrigInjecConv_T8_CC2);
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC1, ADC_ExternalTrigInjecConvEdge_Falling);
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC2, ADC_ExternalTrigInjecConvEdge_Falling);
	ADC_InjectedSequencerLengthConfig(ADC1, 2);
	ADC_InjectedSequencerLengthConfig(ADC2, 2);

	hw_setup_adc_channels();

	// Interrupt
	ADC_ITConfig(ADC1, ADC_IT_JEOC, ENABLE);
	nvicEnableVector(ADC_IRQn, 4);

	// Enable ADC1
	ADC_Cmd(ADC1, ENABLE);

	// Enable ADC2
	ADC_Cmd(ADC2, ENABLE);

	// Enable ADC3
	ADC_Cmd(ADC3, ENABLE);

	// ------------- Timer8 for ADC sampling ------------- //
	// Time Base configuration
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM8, ENABLE);

	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_CenterAligned2;
	TIM_TimeBaseStructure.TIM_Period = SYSTEM_CORE_CLOCK / (int)m_conf->foc_f_sw;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 1;
	TIM_TimeBaseInit(TIM8, &TIM_TimeBaseStructure);

	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_Pulse = 500;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_High;
	TIM_OCInitStructure.TIM_OCIdleState = TIM_OCIdleState_Set;
	TIM_OCInitStructure.TIM_OCNIdleState = TIM_OCNIdleState_Set;
	TIM_OC1Init(TIM8, &TIM_OCInitStructure);
	TIM_OC1PreloadConfig(TIM8, TIM_OCPreload_Enable);
	TIM_OC2Init(TIM8, &TIM_OCInitStructure);
	TIM_OC2PreloadConfig(TIM8, TIM_OCPreload_Enable);

	TIM_ARRPreloadConfig(TIM8, ENABLE);
	TIM_CCPreloadControl(TIM8, ENABLE);

	// PWM outputs have to be enabled in order to trigger ADC on CCx
	TIM_CtrlPWMOutputs(TIM8, ENABLE);

	// TIM1 Master and TIM8 slave
	TIM_SelectOutputTrigger(TIM1, TIM_TRGOSource_Update);
	TIM_SelectMasterSlaveMode(TIM1, TIM_MasterSlaveMode_Enable);
	TIM_SelectInputTrigger(TIM8, TIM_TS_ITR0);
	TIM_SelectSlaveMode(TIM8, TIM_SlaveMode_Reset);

	// Enable TIM1 and TIM8
	TIM_Cmd(TIM1, ENABLE);
	TIM_Cmd(TIM8, ENABLE);

	// Main Output Enable
	TIM_CtrlPWMOutputs(TIM1, ENABLE);

	// ADC sampling locations
	stop_pwm_hw();

	// Sample intervals. For now they are fixed with voltage samples in the center of V7
	// and current samples in the center of V0
	TIMER_UPDATE_SAMP(TIM1->ARR - 2, 5);

	utils_sys_unlock_cnt();

	// Calibrate current offset
	ENABLE_GATE();
	DCCAL_OFF();
	do_dc_cal();

	// Various time measurements
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM12, ENABLE);

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFFFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = (uint16_t)(((SYSTEM_CORE_CLOCK / 2) / 10000000) - 1);
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM12, &TIM_TimeBaseStructure);

	TIM_Cmd(TIM12, ENABLE);

	// Start threads
	timer_thd_stop = false;
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);

	// WWDG configuration
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_WWDG, ENABLE);
	WWDG_SetPrescaler(WWDG_Prescaler_1);
	WWDG_SetWindowValue(255);
	WWDG_Enable(100);
}

void mcpwm_foc_deinit(void) {
	WWDG_DeInit();

	timer_thd_stop = true;

	while (timer_thd_stop) {
		chThdSleepMilliseconds(1);
	}

	TIM_DeInit(TIM1);
	TIM_DeInit(TIM8);
	TIM_DeInit(TIM12);
	ADC_DeInit();
	DMA_DeInit(DMA2_Stream4);
	nvicDisableVector(ADC_IRQn);
	dmaStreamRelease(STM32_DMA_STREAM(STM32_DMA_STREAM_ID(2, 4)));
}

void mcpwm_foc_set_configuration(volatile mc_configuration *configuration) {
	m_conf = configuration;

	m_control_mode = CONTROL_MODE_NONE;
	m_state = MC_STATE_OFF;
	stop_pwm_hw();
	uint32_t top = SYSTEM_CORE_CLOCK / (int)m_conf->foc_f_sw;
	TIMER_UPDATE_SAMP_TOP(top - 2, 5, top);
}

mc_state mcpwm_foc_get_state(void) {
	return m_state;
}

bool mcpwm_foc_is_dccal_done(void) {
	return m_dccal_done;
}

/**
 * Switch off all FETs.TIM12->CNT = 0;
 */
void mcpwm_foc_stop_pwm(void) {
	mcpwm_foc_set_current(0.0);
}

/**
 * Use duty cycle control. Absolute values less than MCPWM_MIN_DUTY_CYCLE will
 * stop the motor.
 *
 * @param dutyCycle
 * The duty cycle to use.
 */
void mcpwm_foc_set_duty(float dutyCycle) {
	m_control_mode = CONTROL_MODE_DUTY;
	m_duty_cycle_set = dutyCycle;

	if (m_state != MC_STATE_RUNNING) {
		m_state = MC_STATE_RUNNING;
	}
}

/**
 * Use duty cycle control. Absolute values less than MCPWM_MIN_DUTY_CYCLE will
 * stop the motor.
 *
 * WARNING: This function does not use ramping. A too large step with a large motor
 * can destroy hardware.
 *
 * @param dutyCycle
 * The duty cycle to use.
 */
void mcpwm_foc_set_duty_noramp(float dutyCycle) {
	// TODO: Actually do this without ramping
	mcpwm_foc_set_duty(dutyCycle);
}

/**
 * Use PID rpm control. Note that this value has to be multiplied by half of
 * the number of motor poles.
 *
 * @param rpm
 * The electrical RPM goal value to use.
 */
void mcpwm_foc_set_pid_speed(float rpm) {
	m_control_mode = CONTROL_MODE_SPEED;
	m_speed_pid_set_rpm = rpm;

	if (m_state != MC_STATE_RUNNING && fabsf(rpm) > m_conf->s_pid_min_erpm) {
		m_state = MC_STATE_RUNNING;
	}
}

/**
 * Use PID position control. Note that this only works when encoder support
 * is enabled.
 *
 * @param pos
 * The desired position of the motor in degrees.
 */
void mcpwm_foc_set_pid_pos(float pos) {
	m_control_mode = CONTROL_MODE_POS;
	m_pos_pid_set_pos = pos;

	if (m_state != MC_STATE_RUNNING) {
		m_state = MC_STATE_RUNNING;
	}
}

/**
 * Use current control and specify a goal current to use. The sign determines
 * the direction of the torque. Absolute values less than
 * conf->cc_min_current will release the motor.
 *
 * @param current
 * The current to use.
 */
void mcpwm_foc_set_current(float current) {
	if (fabsf(current) < m_conf->cc_min_current) {
		m_control_mode = CONTROL_MODE_NONE;
		m_state = MC_STATE_OFF;
		stop_pwm_hw();
		return;
	}

	utils_truncate_number(&current, m_conf->lo_current_min, m_conf->lo_current_max);

	m_control_mode = CONTROL_MODE_CURRENT;
	m_iq_set = current;

	if (m_state != MC_STATE_RUNNING) {
		m_state = MC_STATE_RUNNING;
	}
}

/**
 * Brake the motor with a desired current. Absolute values less than
 * conf->cc_min_current will release the motor.
 *
 * @param current
 * The current to use. Positive and negative values give the same effect.
 */
void mcpwm_foc_set_brake_current(float current) {
	if (fabsf(current) < m_conf->cc_min_current) {
		m_control_mode = CONTROL_MODE_NONE;
		m_state = MC_STATE_OFF;
		stop_pwm_hw();
		return;
	}

	utils_truncate_number(&current, m_conf->lo_current_min, m_conf->lo_current_max);

	m_control_mode = CONTROL_MODE_CURRENT_BRAKE;
	m_iq_set = current;

	if (m_state != MC_STATE_RUNNING) {
		m_state = MC_STATE_RUNNING;
	}
}

float mcpwm_foc_get_duty_cycle_set(void) {
	return m_duty_cycle_set;
}

float mcpwm_foc_get_duty_cycle_now(void) {
	return m_motor_state.duty_now;
}

/**
 * Get the current switching frequency.
 *
 * @return
 * The switching frequency in Hz.
 */
float mcpwm_foc_get_switching_frequency_now(void) {
	return m_conf->foc_f_sw;
}

/**
 * Calculate the current RPM of the motor. This is a signed value and the sign
 * depends on the direction the motor is rotating in. Note that this value has
 * to be divided by half the number of motor poles.
 *
 * @return
 * The RPM value.
 */
float mcpwm_foc_get_rpm(void) {
	return m_pll_speed / ((2.0 * M_PI) / 60.0);
}

/**
 * Get the motor current. The sign of this value will
 * represent whether the motor is drawing (positive) or generating
 * (negative) current. This is the q-axis current which produces torque.
 *
 * @return
 * The motor current.
 */
float mcpwm_foc_get_tot_current(void) {
	return SIGN(m_motor_state.vq) * m_motor_state.iq;
}

/**
 * Get the filtered motor current. The sign of this value will
 * represent whether the motor is drawing (positive) or generating
 * (negative) current. This is the q-axis current which produces torque.
 *
 * @return
 * The filtered motor current.
 */
float mcpwm_foc_get_tot_current_filtered(void) {
	return SIGN(m_motor_state.vq) * m_motor_state.iq_filter;
}

/**
 * Get the magnitude of the motor current, which includes both the
 * D and Q axis.
 *
 * @return
 * The magnitude of the motor current.
 */
float mcpwm_foc_get_abs_motor_current(void) {
	return m_motor_state.i_abs;
}

/**
 * Get the filtered magnitude of the motor current, which includes both the
 * D and Q axis.
 *
 * @return
 * The magnitude of the motor current.
 */
float mcpwm_foc_get_abs_motor_current_filtered(void) {
	return m_motor_state.i_abs_filter;
}

/**
 * Get the motor current. The sign of this value represents the direction
 * in which the motor generates torque.
 *
 * @return
 * The motor current.
 */
float mcpwm_foc_get_tot_current_directional(void) {
	return m_motor_state.iq;
}

/**
 * Get the filtered motor current. The sign of this value represents the
 * direction in which the motor generates torque.
 *
 * @return
 * The filtered motor current.
 */
float mcpwm_foc_get_tot_current_directional_filtered(void) {
	return m_motor_state.iq_filter;
}

/**
 * Get the input current to the motor controller.
 *
 * @return
 * The input current.
 */
float mcpwm_foc_get_tot_current_in(void) {
	return m_motor_state.i_bus;
}

/**
 * Get the filtered input current to the motor controller.
 *
 * @return
 * The filtered input current.
 */
float mcpwm_foc_get_tot_current_in_filtered(void) {
	return m_motor_state.i_bus; // TODO: Calculate filtered current?
}

/**
 * Read the number of steps the motor has rotated. This number is signed and
 * will return a negative number when the motor is rotating backwards.
 *
 * @param reset
 * If true, the tachometer counter will be reset after this call.
 *
 * @return
 * The tachometer value in motor steps. The number of motor revolutions will
 * be this number divided by (3 * MOTOR_POLE_NUMBER).
 */
int mcpwm_foc_get_tachometer_value(bool reset) {
	int val = m_tachometer;

	if (reset) {
		m_tachometer = 0;
	}

	return val;
}

/**
 * Read the absolute number of steps the motor has rotated.
 *
 * @param reset
 * If true, the tachometer counter will be reset after this call.
 *
 * @return
 * The tachometer value in motor steps. The number of motor revolutions will
 * be this number divided by (3 * MOTOR_POLE_NUMBER).
 */
int mcpwm_foc_get_tachometer_abs_value(bool reset) {
	int val = m_tachometer_abs;

	if (reset) {
		m_tachometer_abs = 0;
	}

	return val;
}

/**
 * Read the motor phase.
 *
 * @return
 * The phase angle in degrees.
 */
float mcpwm_foc_get_phase(void) {
	float angle = m_motor_state.phase * (180.0 / M_PI);
	utils_norm_angle(&angle);
	return angle;
}

/**
 * Read the phase that the observer has calculated.
 *
 * @return
 * The phase angle in degrees.
 */
float mcpwm_foc_get_phase_observer(void) {
	float angle = m_phase_now_observer * (180.0 / M_PI);
	utils_norm_angle(&angle);
	return angle;
}

/**
 * Read the phase from based on the encoder.
 *
 * @return
 * The phase angle in degrees.
 */
float mcpwm_foc_get_phase_encoder(void) {
	float angle = m_phase_now_encoder * (180.0 / M_PI);
	utils_norm_angle(&angle);
	return angle;
}

float mcpwm_foc_get_vd(void) {
	return m_motor_state.vd;
}

float mcpwm_foc_get_vq(void) {
	return m_motor_state.vq;
}

/**
 * Measure encoder offset and direction.
 *
 * @param current
 * The locking open loop current for the motor.
 *
 * @param offset
 * The detected offset.
 *
 * @param ratio
 * The ratio between electrical and mechanical revolutions
 *
 * @param direction
 * The detected direction.
 */
void mcpwm_foc_encoder_detect(float current, bool print, float *offset, float *ratio, bool *inverted) {
	m_phase_override = true;
	m_id_set = current;
	m_iq_set = 0.0;
	m_control_mode = CONTROL_MODE_CURRENT;
	m_state = MC_STATE_RUNNING;

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_configure(60000, 0.0);

	// Save configuration
	float offset_old = m_conf->foc_encoder_offset;
	float inverted_old = m_conf->foc_encoder_inverted;
	float ratio_old = m_conf->foc_encoder_ratio;

	m_conf->foc_encoder_offset = 0.0;
	m_conf->foc_encoder_inverted = false;
	m_conf->foc_encoder_ratio = 1.0;

	// Find index
	int cnt = 0;
	while(!encoder_index_found()) {
		for (float i = 0.0;i < 2.0 * M_PI;i += (2.0 * M_PI) / 500.0) {
			m_phase_now_override = i;
			chThdSleepMilliseconds(1);
		}

		cnt++;
		if (cnt > 30) {
			// Give up
			break;
		}
	}

	if (print) {
		commands_printf("Index found");
	}

	// Rotate
	for (float i = 0.0;i < 2.0 * M_PI;i += (2.0 * M_PI) / 500.0) {
		m_phase_now_override = i;
		chThdSleepMilliseconds(1);
	}

	if (print) {
		commands_printf("Rotated for sync");
	}

	// Inverted and ratio
	chThdSleepMilliseconds(1000);

	float s_sum = 0.0;
	float c_sum = 0.0;
	for (int i = 0; i < 10; i++) {
		float phase_old = m_phase_now_encoder;
		float phase_ovr_tmp = m_phase_now_override;
		for (float i = phase_ovr_tmp; i < phase_ovr_tmp + 0.8 * M_PI;
				i += (2.0 * M_PI) / 500.0) {
			m_phase_now_override = i;
			chThdSleepMilliseconds(2);
		}
		chThdSleepMilliseconds(1000);
		float diff = utils_angle_difference(
				m_phase_now_encoder * (180.0 / M_PI),
				phase_old * (180.0 / M_PI));

		float s, c;
		sincosf(diff * M_PI / 180.0, &s, &c);
		s_sum += s;
		c_sum += c;

		if (print) {
			commands_printf("%.2f", (double)diff);
		}
	}

	float diff = atan2f(s_sum, c_sum) * 180.0 / M_PI;
	*inverted = diff < 0.0;
	*ratio = roundf((0.8 * 180.0) /
			fabsf(diff));

	m_conf->foc_encoder_inverted = *inverted;
	m_conf->foc_encoder_ratio = *ratio;

	if (print) {
		commands_printf("Inversion and ratio detected");
	}

	// Rotate
	for (float i = m_phase_now_override;i < 2.0 * M_PI;i += (2.0 * M_PI) / 500.0) {
		m_phase_now_override = i;
		chThdSleepMilliseconds(2);
	}

	if (print) {
		commands_printf("Rotated for sync");
		commands_printf("Enc: %.2f", (double)encoder_read_deg());
	}

	s_sum = 0.0;
	c_sum = 0.0;
	for (int i = 0;i < 5;i++) {
		m_phase_now_override = (float)i * M_PI / 5.0;
		chThdSleepMilliseconds(1000);

		float s, c;
		sincosf(m_phase_now_encoder - m_phase_now_override, &s, &c);
		s_sum += s;
		c_sum += c;

		if (print) {
			commands_printf("%.2f", (double)((m_phase_now_encoder - m_phase_now_override) * 180.0 / M_PI));
		}
	}

	for (int i = 3;i >= -1;i--) {
		m_phase_now_override = (float)i * M_PI / 5.0;
		chThdSleepMilliseconds(1000);

		float s, c;
		sincosf(m_phase_now_encoder - m_phase_now_override, &s, &c);
		s_sum += s;
		c_sum += c;

		if (print) {
			commands_printf("%.2f", (double)((m_phase_now_encoder - m_phase_now_override) * 180.0 / M_PI));
		}
	}

	*offset = atan2f(s_sum, c_sum) * 180.0 / M_PI;

	if (print) {
		commands_printf("Avg: %.2f", (double)*offset);
	}

	utils_norm_angle(offset);

	if (print) {
		commands_printf("Offset detected");
	}

	m_id_set = 0.0;
	m_iq_set = 0.0;
	m_phase_override = false;
	m_control_mode = CONTROL_MODE_NONE;
	m_state = MC_STATE_OFF;
	stop_pwm_hw();

	// Restore configuration
	m_conf->foc_encoder_inverted = inverted_old;
	m_conf->foc_encoder_offset = offset_old;
	m_conf->foc_encoder_ratio = ratio_old;

	// Enable timeout
	timeout_configure(tout, tout_c);
}

/**
 * Lock the motor with a current and sample the voiltage and current to
 * calculate the motor resistance.
 *
 * @param current
 * The locking current.
 *
 * @param samples
 * The number of samples to take.
 *
 * @return
 * The calculated motor resistance.
 */
float mcpwm_foc_measure_resistance(float current, int samples) {
	mc_interface_lock();

	m_phase_override = true;
	m_phase_now_override = 0.0;
	m_id_set = 0.0;
	m_iq_set = current;
	m_control_mode = CONTROL_MODE_CURRENT;
	m_state = MC_STATE_RUNNING;

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_configure(60000, 0.0);

	// Wait for the current to rise and the motor to lock.
	chThdSleepMilliseconds(1000);

	// Sample
	m_samples.avg_current_tot = 0.0;
	m_samples.avg_voltage_tot = 0.0;
	m_samples.sample_num = 0;

	int cnt = 0;
	while (m_samples.sample_num < samples) {
		chThdSleepMilliseconds(1);
		cnt++;
		// Timeout
		if (cnt > 10000) {
			break;
		}
	}

	const float current_avg = m_samples.avg_current_tot / (float)m_samples.sample_num;
	const float voltage_avg = m_samples.avg_voltage_tot / (float)m_samples.sample_num;

	// Stop
	m_id_set = 0.0;
	m_iq_set = 0.0;
	m_phase_override = false;
	m_control_mode = CONTROL_MODE_NONE;
	m_state = MC_STATE_OFF;
	stop_pwm_hw();

	// Enable timeout
	timeout_configure(tout, tout_c);

	mc_interface_unlock();

	return (voltage_avg / current_avg) * (2.0 / 3.0);
}

/**
 * Measure the motor inductance with short voltage pulses.
 *
 * @param duty
 * The duty cycle to use in the pulses.
 *
 * @param samples
 * The number of samples to average over.
 *
 * @param
 * The current that was used for this measurement.
 *
 * @return
 * The average d and q axis inductance in microhenry.
 */
float mcpwm_foc_measure_inductance(float duty, int samples, float *curr) {
	m_samples.avg_current_tot = 0.0;
	m_samples.avg_voltage_tot = 0.0;
	m_samples.sample_num = 0;
	m_samples.measure_inductance_duty = duty;

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	int to_cnt = 0;
	for (int i = 0;i < samples;i++) {
		m_samples.measure_inductance_now = true;

		while (m_samples.measure_inductance_now) {
			chThdSleepMilliseconds(1);
			to_cnt++;
			if (to_cnt > 5000) {
				break;
			}
		}

		if (to_cnt > 5000) {
			break;
		}
	}

	// Enable timeout
	timeout_configure(tout, tout_c);

	mc_interface_unlock();

	float avg_current = m_samples.avg_current_tot / (float)m_samples.sample_num;
	float avg_voltage = m_samples.avg_voltage_tot / (float)m_samples.sample_num;
	float t = (float)TIM1->ARR * m_samples.measure_inductance_duty / (float)SYSTEM_CORE_CLOCK -
			(float)(MCPWM_FOC_INDUCTANCE_SAMPLE_CNT_OFFSET + MCPWM_FOC_INDUCTANCE_SAMPLE_RISE_COMP) / (float)SYSTEM_CORE_CLOCK;

	if (curr) {
		*curr = avg_current;
	}

	return ((avg_voltage * t) / avg_current) * 1e6 * (2.0 /  3.0);
}

/**
 * Automatically measure the resistance and inductance of the motor with small steps.
 *
 * @param res
 * The measured resistance in ohm.
 *
 * @param ind
 * The measured inductance in microhenry.
 *
 * @return
 * True if the measurement succeeded, false otherwise.
 */
bool mcpwm_foc_measure_res_ind(float *res, float *ind) {
	const float f_sw_old = m_conf->foc_f_sw;
	const float kp_old = m_conf->foc_current_kp;
	const float ki_old = m_conf->foc_current_ki;

	m_conf->foc_f_sw = 10000.0;
	m_conf->foc_current_kp = 0.01;
	m_conf->foc_current_ki = 10.0;

	uint32_t top = SYSTEM_CORE_CLOCK / (int)m_conf->foc_f_sw;
	TIMER_UPDATE_SAMP_TOP(top - 2, 5, top);

	float res_tmp = 0.0;
	float i_last = 0.0;
	for (float i = 2.0;i < 35.0;i *= 1.5) {
		res_tmp = mcpwm_foc_measure_resistance(i, 20);

		if (i > (0.5 / res_tmp)) {
			i_last = i;
			break;
		}
	}

	if (i_last < 0.01) {
		i_last = 35.0;
	}

	*res = mcpwm_foc_measure_resistance(i_last, 500);

	m_conf->foc_f_sw = 3000.0;
	top = SYSTEM_CORE_CLOCK / (int)m_conf->foc_f_sw;
	TIMER_UPDATE_SAMP_TOP(top - 2, 5, top);

	float duty_last = 0.0;
	for (float i = 0.02;i < 0.9;i *= 1.5) {
		float i_tmp;
		mcpwm_foc_measure_inductance(i, 20, &i_tmp);

		if (i_tmp >= i_last) {
			duty_last = i;
			break;
		}
	}

	*ind = mcpwm_foc_measure_inductance(duty_last, 500, 0);

	m_conf->foc_f_sw = f_sw_old;
	m_conf->foc_current_kp = kp_old;
	m_conf->foc_current_ki = ki_old;

	top = SYSTEM_CORE_CLOCK / (int)m_conf->foc_f_sw;
	TIMER_UPDATE_SAMP_TOP(top - 2, 5, top);

	return true;
}

/**
 * Run the motor in open loop and figure out at which angles the hall sensors are.
 *
 * @param current
 * Current to use.
 *
 * @param hall_table
 * Table to store the result to.
 *
 * @return
 * true: Success
 * false: Something went wrong
 */
bool mcpwm_foc_hall_detect(float current, uint8_t *hall_table) {
	m_phase_override = true;
	m_id_set = current;
	m_iq_set = 0.0;
	m_control_mode = CONTROL_MODE_CURRENT;
	m_state = MC_STATE_RUNNING;

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_configure(60000, 0.0);

	// Lock the motor
	m_phase_now_override = 0;
	chThdSleepMilliseconds(1000);

	float sin_hall[8];
	float cos_hall[8];
	int hall_iterations[8];
	memset(sin_hall, 0, sizeof(sin_hall));
	memset(cos_hall, 0, sizeof(cos_hall));
	memset(hall_iterations, 0, sizeof(hall_iterations));

	// Forwards
	for (int i = 0;i < 3;i++) {
		for (int i = 0;i < 360;i++) {
			m_phase_now_override = (float)i * M_PI / 180.0;
			chThdSleepMilliseconds(5);

			int hall = read_hall();
			float s, c;
			sincosf(m_phase_now_override, &s, &c);
			sin_hall[hall] += s;
			cos_hall[hall] += c;
			hall_iterations[hall]++;
		}
	}

	// Reverse
	for (int i = 0;i < 3;i++) {
		for (int i = 360;i >= 0;i--) {
			m_phase_now_override = (float)i * M_PI / 180.0;
			chThdSleepMilliseconds(5);

			int hall = read_hall();
			float s, c;
			sincosf(m_phase_now_override, &s, &c);
			sin_hall[hall] += s;
			cos_hall[hall] += c;
			hall_iterations[hall]++;
		}
	}

	m_id_set = 0.0;
	m_iq_set = 0.0;
	m_phase_override = false;
	m_control_mode = CONTROL_MODE_NONE;
	m_state = MC_STATE_OFF;
	stop_pwm_hw();

	// Enable timeout
	timeout_configure(tout, tout_c);

	int fails = 0;
	for(int i = 0;i < 8;i++) {
		if (hall_iterations[i] > 30) {
			float ang = atan2f(sin_hall[i], cos_hall[i]) * 180.0 / M_PI;
			utils_norm_angle(&ang);
			hall_table[i] = (uint8_t)(ang * 200.0 / 360.0);
		} else {
			hall_table[i] = 255;
			fails++;
		}
	}

	return fails == 2;
}

void mcpwm_foc_print_state(void) {
	commands_printf("Mod d:        %.2f", (double)m_motor_state.mod_d);
	commands_printf("Mod q:        %.2f", (double)m_motor_state.mod_q);
	commands_printf("Duty:         %.2f", (double)m_motor_state.duty_now);
	commands_printf("Vd:           %.2f", (double)m_motor_state.vd);
	commands_printf("Vq:           %.2f", (double)m_motor_state.vq);
	commands_printf("Phase:        %.2f", (double)m_motor_state.phase);
	commands_printf("V_alpha:      %.2f", (double)m_motor_state.v_alpha);
	commands_printf("V_beta:       %.2f", (double)m_motor_state.v_beta);
	commands_printf("id:           %.2f", (double)m_motor_state.id);
	commands_printf("iq:           %.2f", (double)m_motor_state.iq);
	commands_printf("id_filter:    %.2f", (double)m_motor_state.id_filter);
	commands_printf("iq_filter:    %.2f", (double)m_motor_state.iq_filter);
	commands_printf("id_target:    %.2f", (double)m_motor_state.id_target);
	commands_printf("iq_target:    %.2f", (double)m_motor_state.iq_target);
	commands_printf("i_abs:        %.2f", (double)m_motor_state.i_abs);
	commands_printf("i_abs_filter: %.2f", (double)m_motor_state.i_abs_filter);
	commands_printf("Obs_x1:       %.2f", (double)m_observer_x1);
	commands_printf("Obs_x2:       %.2f", (double)m_observer_x2);
}

float mcpwm_foc_get_last_inj_adc_isr_duration(void) {
	return last_inj_adc_isr_duration;
}

void mcpwm_foc_adc_inj_int_handler(void) {
	TIM12->CNT = 0;

	// Reset the watchdog
	WWDG_SetCounter(100);

	const float dt = 1.0 / (m_conf->foc_f_sw / 2.0);
	int curr0 = ADC_GetInjectedConversionValue(ADC1, ADC_InjectedChannel_1);
	int curr1 = ADC_GetInjectedConversionValue(ADC2, ADC_InjectedChannel_1);

	m_curr0_sum += curr0;
	m_curr1_sum += curr1;
	m_curr_samples++;

	curr0 -= m_curr0_offset;
	curr1 -= m_curr1_offset;

	// Subtract the current sampled in V7
//	curr0 -= ADC_Value[ADC_IND_CURR1];
//	curr1 -= ADC_Value[ADC_IND_CURR2];

	ADC_curr_norm_value[0] = curr0;
	ADC_curr_norm_value[1] = curr1;
	ADC_curr_norm_value[2] = -(ADC_curr_norm_value[0] + ADC_curr_norm_value[1]);

	float ia = ADC_curr_norm_value[0] * (V_REG / 4095.0) / (CURRENT_SHUNT_RES * CURRENT_AMP_GAIN);
	float ib = ADC_curr_norm_value[1] * (V_REG / 4095.0) / (CURRENT_SHUNT_RES * CURRENT_AMP_GAIN);
//	float ic = -(ia + ib);

	if (m_samples.measure_inductance_now) {
		static int inductance_state = 0;
		const uint32_t duty_cnt = (uint32_t)((float)TIM1->ARR * m_samples.measure_inductance_duty);
		const uint32_t samp_time = duty_cnt - MCPWM_FOC_INDUCTANCE_SAMPLE_CNT_OFFSET;

		if (inductance_state == 0) {
			TIMER_UPDATE_DUTY_CURRENTSAMP(0, 0, 0, samp_time);
			start_pwm_hw();
		} else if (inductance_state == 2) {
			TIMER_UPDATE_DUTY(duty_cnt,	0, duty_cnt);
		} else if (inductance_state == 3) {
			m_samples.avg_current_tot += -ib;
			m_samples.avg_voltage_tot += GET_INPUT_VOLTAGE();
			m_samples.sample_num++;
			TIMER_DISABLE_PRELOAD_DUTY1();
			TIMER_UPDATE_DUTY(0, 0, 0);
			TIMER_ENABLE_PRELOAD_DUTY1();
		} else if (inductance_state == 5) {
			TIMER_UPDATE_DUTY(0, duty_cnt, duty_cnt);
		} else if (inductance_state == 6) {
			m_samples.avg_current_tot += -ia;
			m_samples.avg_voltage_tot += GET_INPUT_VOLTAGE();
			m_samples.sample_num++;
			TIMER_DISABLE_PRELOAD_DUTY2();
			TIMER_UPDATE_DUTY(0, 0, 0);
			TIMER_ENABLE_PRELOAD_DUTY2();
		} else if (inductance_state == 8) {
			TIMER_UPDATE_DUTY(0, 0, duty_cnt);
		} else if (inductance_state == 9) {
			m_samples.avg_current_tot += -(ia + ib);
			m_samples.avg_voltage_tot += GET_INPUT_VOLTAGE();
			m_samples.sample_num++;
			stop_pwm_hw();

			TIMER_UPDATE_SAMP(TIM1->ARR - 2, 5);
			inductance_state = 0;
			m_samples.measure_inductance_now = false;
			return;
		}

		inductance_state++;
		return;
	}

	m_motor_state.v_bus = GET_INPUT_VOLTAGE();

#if ENCODER_ENABLE
	float phase_tmp = encoder_read_deg();
	if (m_conf->foc_encoder_inverted) {
		phase_tmp = 360.0 - phase_tmp;
	}
	phase_tmp *= m_conf->foc_encoder_ratio;
	phase_tmp -= m_conf->foc_encoder_offset;
	utils_norm_angle((float*)&phase_tmp);
	m_phase_now_encoder = phase_tmp * (M_PI / 180.0);
#endif

	static float phase_before = 0.0;
	const float phase_diff = utils_angle_difference_rad(m_motor_state.phase, phase_before);
	phase_before = m_motor_state.phase;

	if (m_state == MC_STATE_RUNNING) {
		// Clarke transform assuming balanced currents
		m_motor_state.i_alpha = ia;
		m_motor_state.i_beta = ONE_BY_SQRT3 * ia + TWO_BY_SQRT3 * ib;

		// Full Clarke transform in case there are current offsets
//		m_motor_state.i_alpha = (2.0 / 3.0) * ia - (1.0 / 3.0) * ib - (1.0 / 3.0) * ic;
//		m_motor_state.i_beta = ONE_BY_SQRT3 * ib - ONE_BY_SQRT3 * ic;

		const float duty_abs = fabsf(m_motor_state.duty_now);
		float id_set_tmp = m_id_set;
		float iq_set_tmp = m_iq_set;
		m_motor_state.max_duty = m_conf->l_max_duty;

		static float duty_filtered = 0.0;
		UTILS_LP_FAST(duty_filtered, m_motor_state.duty_now, 0.1);
		utils_truncate_number(&duty_filtered, -1.0, 1.0);

		float duty_set = m_duty_cycle_set;
		bool control_duty = m_control_mode == CONTROL_MODE_DUTY;

		// When the filtered duty cycle in sensorless mode becomes low in brake mode, the
		// observer has lost tracking. Use duty cycle control with the lowest duty cycle
		// to get as smooth braking as possible.
		if (m_control_mode == CONTROL_MODE_CURRENT_BRAKE
				&& (m_conf->foc_sensor_mode != FOC_SENSOR_MODE_ENCODER)
				&& fabsf(duty_filtered) < 0.03) {
			control_duty = true;
			duty_set = 0.0;
		}

		if (control_duty) {
			// Duty cycle control
			static float duty_i_term = 0.0;
			if (fabsf(duty_set) < (duty_abs - 0.05) ||
					(SIGN(m_motor_state.vq) * m_motor_state.iq) < m_conf->lo_current_min) {
				// Truncating the duty cycle here would be dangerous, so run a PID controller.

				// Compensation for supply voltage variations
				float scale = 1.0 / GET_INPUT_VOLTAGE();

				// Compute error
				float error = duty_set - m_motor_state.duty_now;

				// Compute parameters
				float p_term = error * m_conf->foc_duty_dowmramp_kp * scale;
				duty_i_term += error * (m_conf->foc_duty_dowmramp_ki * dt) * scale;

				// I-term wind-up protection
				utils_truncate_number(&duty_i_term, -1.0, 1.0);

				// Calculate output
				float output = p_term + duty_i_term;
				utils_truncate_number(&output, -1.0, 1.0);
				iq_set_tmp = output * m_conf->lo_current_max;
			} else {
				// If the duty cycle is less than or equal to the set duty cycle just limit
				// the modulation and use the maximum allowed current.
				duty_i_term = 0.0;
				m_motor_state.max_duty = duty_set;
				if (duty_set > 0.0) {
					iq_set_tmp = m_conf->lo_current_max;
				} else {
					iq_set_tmp = -m_conf->lo_current_max;
				}
			}
		} else if (m_control_mode == CONTROL_MODE_CURRENT_BRAKE) {
			// Braking
			iq_set_tmp = fabsf(iq_set_tmp);

			if (phase_diff > 0.0) {
				iq_set_tmp = -iq_set_tmp;
			} else if (phase_diff == 0.0) {
				iq_set_tmp = 0.0;
			}
		}

		// Run observer
		if (!m_phase_override) {
			observer_update(m_motor_state.v_alpha, m_motor_state.v_beta,
					m_motor_state.i_alpha, m_motor_state.i_beta, dt,
					&m_observer_x1, &m_observer_x2, &m_phase_now_observer);
		}

		switch (m_conf->foc_sensor_mode) {
		case FOC_SENSOR_MODE_ENCODER:
			if (encoder_index_found()) {
				m_motor_state.phase = m_phase_now_encoder;
			} else {
				// Rotate the motor in open loop if the index isn't found.
				m_motor_state.phase = m_phase_now_encoder_no_index;
			}

			if (!m_phase_override) {
				id_set_tmp = 0.0;
			}
			break;
		case FOC_SENSOR_MODE_HALL:
			m_phase_now_observer = correct_hall(m_phase_now_observer, m_pll_speed, dt);
			m_motor_state.phase = m_phase_now_observer;
			break;
		case FOC_SENSOR_MODE_SENSORLESS:
			if (m_phase_observer_override) {
				m_motor_state.phase = m_phase_now_observer_override;
			} else {
				m_motor_state.phase = m_phase_now_observer;
			}

			// Inject D axis current at low speed to make the observer track
			// better. This does not seem to be necessary with dead time
			// compensation.
			// Note: this is done at high rate prevent noise.
			if (!m_phase_override) {
				if (duty_abs < m_conf->foc_sl_d_current_duty) {
					id_set_tmp = utils_map(duty_abs, 0.0, m_conf->foc_sl_d_current_duty,
							fabsf(m_motor_state.iq_target) * m_conf->foc_sl_d_current_factor, 0.0);
				} else {
					id_set_tmp = 0.0;
				}
			}
			break;
		}

		if (m_phase_override) {
			m_motor_state.phase = m_phase_now_override;
		}

		// Apply current limits
		const float mod_q = m_motor_state.mod_q;
		utils_truncate_number(&iq_set_tmp, m_conf->lo_current_min, m_conf->lo_current_max);
		utils_saturate_vector_2d(&id_set_tmp, &iq_set_tmp, m_conf->lo_current_max);
		if (mod_q > 0.001) {
			utils_truncate_number(&iq_set_tmp, m_conf->lo_in_current_min / mod_q, m_conf->lo_in_current_max / mod_q);
		} else if (mod_q < -0.001) {
			utils_truncate_number(&iq_set_tmp, m_conf->lo_in_current_max / mod_q, m_conf->lo_in_current_min / mod_q);
		}

		m_motor_state.id_target = id_set_tmp;
		m_motor_state.iq_target = iq_set_tmp;

		control_current(&m_motor_state, dt);
		run_pid_control_pos(dt);
	} else {
		// Track back emf
		float Va = ADC_VOLTS(ADC_IND_SENS1) * ((VIN_R1 + VIN_R2) / VIN_R2);
		float Vb = ADC_VOLTS(ADC_IND_SENS3) * ((VIN_R1 + VIN_R2) / VIN_R2);
		float Vc = ADC_VOLTS(ADC_IND_SENS2) * ((VIN_R1 + VIN_R2) / VIN_R2);

		// Clarke transform
		m_motor_state.v_alpha = (2.0 / 3.0) * Va - (1.0 / 3.0) * Vb - (1.0 / 3.0) * Vc;
		m_motor_state.v_beta = ONE_BY_SQRT3 * Vb - ONE_BY_SQRT3 * Vc;

		// Park transform
		float c,s;
		utils_fast_sincos_better(m_motor_state.phase, &s, &c);
		m_motor_state.vd = c * m_motor_state.v_alpha + s * m_motor_state.v_beta;
		m_motor_state.vq = c * m_motor_state.v_beta  - s * m_motor_state.v_alpha;

		// Update corresponding modulation
		m_motor_state.mod_d = m_motor_state.vd / ((2.0 / 3.0) * m_motor_state.v_bus);
		m_motor_state.mod_q = m_motor_state.vq / ((2.0 / 3.0) * m_motor_state.v_bus);

		// The current is 0 when the motor is undriven
		m_motor_state.i_alpha = 0.0;
		m_motor_state.i_beta = 0.0;
		m_motor_state.id = 0.0;
		m_motor_state.iq = 0.0;
		m_motor_state.id_filter = 0.0;
		m_motor_state.iq_filter = 0.0;
		m_motor_state.i_bus = 0.0;
		m_motor_state.i_abs = 0.0;
		m_motor_state.i_abs_filter = 0.0;

		// Run observer
		observer_update(m_motor_state.v_alpha, m_motor_state.v_beta,
				m_motor_state.i_alpha, m_motor_state.i_beta, dt, &m_observer_x1,
				&m_observer_x2, &m_phase_now_observer);

		switch (m_conf->foc_sensor_mode) {
		case FOC_SENSOR_MODE_ENCODER:
			m_motor_state.phase = m_phase_now_encoder;
			break;
		case FOC_SENSOR_MODE_HALL:
			m_phase_now_observer = correct_hall(m_phase_now_observer, m_pll_speed, dt);
			m_motor_state.phase = m_phase_now_observer;
			break;
		case FOC_SENSOR_MODE_SENSORLESS:
			m_motor_state.phase = m_phase_now_observer;
			break;
		}
	}

	// Calculate duty cycle
	m_motor_state.duty_now = SIGN(m_motor_state.vq) *
			sqrtf(m_motor_state.mod_d * m_motor_state.mod_d +
					m_motor_state.mod_q * m_motor_state.mod_q) / SQRT3_BY_2;

	// Run PLL for speed estimation
	pll_run(m_motor_state.phase, dt, &m_pll_phase, &m_pll_speed);

	// Update tachometer
	static float phase_last = 0.0;
	int diff = (int)((utils_angle_difference_rad(m_motor_state.phase, phase_last) * (180.0 / M_PI)) / 60.0);
	if (diff != 0) {
		m_tachometer += diff;
		m_tachometer_abs += abs(diff);
		phase_last = m_motor_state.phase;
	}

	// MCIF handler
	mc_interface_mc_timer_isr();

	last_inj_adc_isr_duration = (float) TIM12->CNT / 10000000.0;
}

// Private functions

static THD_FUNCTION(timer_thread, arg) {
	(void)arg;

	chRegSetThreadName("mcpwm_foc timer");

	for(;;) {
		if (timer_thd_stop) {
			timer_thd_stop = false;
			return;
		}

		const float dt = 0.001;
		const float min_rads = (m_conf->foc_openloop_rpm * 2.0 * M_PI) / 60.0;
		static float min_rpm_hyst_timer = 0.0;
		static float min_rpm_timer = 0.0;

		float add_min_speed = 0.0;
		if (m_motor_state.duty_now > 0.0) {
			add_min_speed = min_rads * dt;
		} else {
			add_min_speed = -min_rads * dt;
		}

		// Open loop encoder angle for when the index is not found
		m_phase_now_encoder_no_index += add_min_speed;
		utils_norm_angle_rad((float*)&m_phase_now_encoder_no_index);

		// Output a minimum speed from the observer
		if (fabsf(m_pll_speed) < min_rads) {
			min_rpm_hyst_timer += dt;
		} else if (min_rpm_hyst_timer > 0.0) {
			min_rpm_hyst_timer -= dt;
		}

		// Don't use this in brake mode.
		if (m_control_mode == CONTROL_MODE_CURRENT_BRAKE) {
			min_rpm_hyst_timer = 0.0;
			m_phase_observer_override = false;
		}

		if (min_rpm_hyst_timer > m_conf->foc_sl_openloop_hyst && min_rpm_timer <= 0.0001) {
			min_rpm_timer = m_conf->foc_sl_openloop_time;
		}

		if (min_rpm_timer > 0.0) {
			m_phase_now_observer_override += add_min_speed;
			utils_norm_angle_rad((float*)&m_phase_now_observer_override);
			m_phase_observer_override = true;
			min_rpm_timer -= dt;
			min_rpm_hyst_timer = 0.0;
		} else {
			m_phase_now_observer_override = m_phase_now_observer;
			m_phase_observer_override = false;
		}

		// Samples
		if (m_state == MC_STATE_RUNNING) {
			const volatile float vd_tmp = m_motor_state.vd;
			const volatile float vq_tmp = m_motor_state.vq;
			const volatile float id_tmp = m_motor_state.id;
			const volatile float iq_tmp = m_motor_state.iq;

			m_samples.avg_current_tot += sqrtf(id_tmp * id_tmp + iq_tmp * iq_tmp);
			m_samples.avg_voltage_tot += sqrtf(vd_tmp * vd_tmp + vq_tmp * vq_tmp);
			m_samples.sample_num++;
		}

		run_pid_control_speed(dt);
		chThdSleepMilliseconds(1);
	}

}

static void do_dc_cal(void) {
	DCCAL_ON();
	while(IS_DRV_FAULT()){};
	chThdSleepMilliseconds(1000);
	m_curr0_sum = 0;
	m_curr1_sum = 0;
	m_curr_samples = 0;
	while(m_curr_samples < 4000) {};
	m_curr0_offset = m_curr0_sum / m_curr_samples;
	m_curr1_offset = m_curr1_sum / m_curr_samples;
	DCCAL_OFF();
	m_dccal_done = true;
}

// See http://cas.ensmp.fr/~praly/Telechargement/Journaux/2010-IEEE_TPEL-Lee-Hong-Nam-Ortega-Praly-Astolfi.pdf
void observer_update(float v_alpha, float v_beta, float i_alpha, float i_beta,
		float dt, volatile float *x1, volatile float *x2, volatile float *phase) {

	const float L = (3.0 / 2.0) * m_conf->foc_motor_l;
	const float R = (3.0 / 2.0) * m_conf->foc_motor_r;
	const float gamma = m_conf->foc_observer_gain;
	const float linkage = m_conf->foc_motor_flux_linkage;

	const float Lia = L * i_alpha;
	const float Lib = L * i_beta;

	float k1 = (linkage * linkage) - ((*x1 - Lia) * (*x1 - Lia) + (*x2 - Lib) * (*x2 - Lib));
	float x1_dot = 0.0;
	float x2_dot = 0.0;

	x1_dot = -R * i_alpha + v_alpha + ((gamma / 2.0) * (*x1 - Lia)) * k1;
	x2_dot = -R * i_beta + v_beta + ((gamma / 2.0) * (*x2 - Lib)) * k1;
	*x1 += x1_dot * dt;
	*x2 += x2_dot * dt;

	if (fabsf(*x1) > 1e20 || UTILS_IS_NAN(*x1)) {
		*x1 = 0.0;
	}

	if (fabsf(*x2) > 1e20 || UTILS_IS_NAN(*x2)) {
		*x2 = 0.0;
	}

	*phase = utils_fast_atan2(*x2 - L * i_beta, *x1 - L * i_alpha);
}

static void pll_run(float phase, float dt, volatile float *phase_var,
		volatile float *speed_var) {
	float delta_theta = phase - *phase_var;
	utils_norm_angle_rad(&delta_theta);
	*phase_var += (*speed_var + m_conf->foc_pll_kp * delta_theta) * dt;
	utils_norm_angle_rad((float*)phase_var);
	*speed_var += m_conf->foc_pll_ki * delta_theta * dt;
}

/**
 * Run the current control loop.
 *
 * @param state_m
 * The motor state.
 *
 * Parameters that shall be set before calling this function:
 * id_target
 * iq_target
 * max_duty
 * phase
 * i_alpha
 * i_beta
 * v_bus
 *
 * Parameters that will be updated in this function:
 * i_bus
 * i_abs
 * i_abs_filter
 * v_alpha
 * v_beta
 * mod_d
 * mod_q
 * id
 * iq
 * id_filter
 * iq_filter
 * vd
 * vq
 *
 * @param dt
 * The time step in seconds.
 */
static void control_current(volatile motor_state_t *state_m, float dt) {
	float c,s;
	utils_fast_sincos_better(state_m->phase, &s, &c);

	state_m->id = c * state_m->i_alpha + s * state_m->i_beta;
	state_m->iq = c * state_m->i_beta  - s * state_m->i_alpha;
	UTILS_LP_FAST(state_m->id_filter, state_m->id, MCPWM_FOC_I_FILTER_CONST);
	UTILS_LP_FAST(state_m->iq_filter, state_m->iq, MCPWM_FOC_I_FILTER_CONST);

	float Ierr_d = state_m->id_target - state_m->id;
	float Ierr_q = state_m->iq_target - state_m->iq;

	float Vd = state_m->vd + Ierr_d * m_conf->foc_current_kp;
	float Vq = state_m->vq + Ierr_q * m_conf->foc_current_kp;
	state_m->vd += Ierr_d * (m_conf->foc_current_ki * dt);
	state_m->vq += Ierr_q * (m_conf->foc_current_ki * dt);

	float max_duty = fabsf(state_m->max_duty);
	utils_truncate_number(&max_duty, 0.0, m_conf->l_max_duty);

	state_m->mod_d = Vd / ((2.0 / 3.0) * state_m->v_bus);
	state_m->mod_q = Vq / ((2.0 / 3.0) * state_m->v_bus);

	// Windup protection and saturation
	utils_saturate_vector_2d((float*)&state_m->mod_d, (float*)&state_m->mod_q,
			SQRT3_BY_2 * max_duty);
	utils_saturate_vector_2d((float*)&state_m->vd, (float*)&state_m->vq,
			(2.0 / 3.0) * max_duty * SQRT3_BY_2 * state_m->v_bus);

	// TODO: Have a look at this?
	state_m->i_bus = state_m->mod_d * state_m->id + state_m->mod_q * state_m->iq;
	state_m->i_abs = sqrtf(state_m->id * state_m->id + state_m->iq * state_m->iq);
	state_m->i_abs_filter = sqrtf(state_m->id_filter * state_m->id_filter + state_m->iq_filter * state_m->iq_filter);

	float mod_alpha = c * state_m->mod_d - s * state_m->mod_q;
	float mod_beta  = c * state_m->mod_q + s * state_m->mod_d;

	state_m->v_alpha = mod_alpha * (2.0 / 3.0) * state_m->v_bus;
	state_m->v_beta = mod_beta * (2.0 / 3.0) * state_m->v_bus;

	// Set output (HW Dependent)
	uint32_t duty1, duty2, duty3, top;
	top = TIM1->ARR;

	// Deadtime compensation
	const float i_alpha_set = c * state_m->id_target - s * state_m->iq_target;
	const float i_beta_set = c * state_m->iq_target + s * state_m->id_target;
	const float ia_set = i_alpha_set;
	const float ib_set = -0.5 * i_alpha_set + SQRT3_BY_2 * i_beta_set;
	const float ic_set = -0.5 * i_alpha_set - SQRT3_BY_2 * i_beta_set;
	const float mod_alpha_set_sgn = (2.0 / 3.0) * SIGN(ia_set) - (1.0 / 3.0) * SIGN(ib_set) - (1.0 / 3.0) * SIGN(ic_set);
	const float mod_beta_set_sgn = ONE_BY_SQRT3 * SIGN(ib_set) - ONE_BY_SQRT3 * SIGN(ic_set);

	mod_alpha += mod_alpha_set_sgn * m_conf->foc_dt_us * 1e-6 * m_conf->foc_f_sw;
	mod_beta += mod_beta_set_sgn * m_conf->foc_dt_us * 1e-6 * m_conf->foc_f_sw;

	svm(-mod_alpha, -mod_beta, top, &duty1, &duty2, &duty3);
	TIMER_UPDATE_DUTY(duty1, duty2, duty3);

	if (!m_output_on) {
		start_pwm_hw();
	}
}

// Magnitude must not be larger than sqrt(3)/2, or 0.866
static void svm(float alpha, float beta, uint32_t PWMHalfPeriod,
		uint32_t* tAout, uint32_t* tBout, uint32_t* tCout) {
	uint32_t sector;

	if (beta >= 0.0f) {
		if (alpha >= 0.0f) {
			//quadrant I
			if (ONE_BY_SQRT3 * beta > alpha)
				sector = 2;
			else
				sector = 1;
		} else {
			//quadrant II
			if (-ONE_BY_SQRT3 * beta > alpha)
				sector = 3;
			else
				sector = 2;
		}
	} else {
		if (alpha >= 0.0f) {
			//quadrant IV5
			if (-ONE_BY_SQRT3 * beta > alpha)
				sector = 5;
			else
				sector = 6;
		} else {
			//quadrant III
			if (ONE_BY_SQRT3 * beta > alpha)
				sector = 4;
			else
				sector = 5;
		}
	}

	// PWM timings
	uint32_t tA, tB, tC;

	switch (sector) {

	// sector 1-2
	case 1: {
		// Vector on-times
		uint32_t t1 = (alpha - ONE_BY_SQRT3 * beta) * PWMHalfPeriod;
		uint32_t t2 = (TWO_BY_SQRT3 * beta) * PWMHalfPeriod;

		// PWM timings
		tA = (PWMHalfPeriod - t1 - t2) / 2;
		tB = tA + t1;
		tC = tB + t2;

		break;
	}

	// sector 2-3
	case 2: {
		// Vector on-times
		uint32_t t2 = (alpha + ONE_BY_SQRT3 * beta) * PWMHalfPeriod;
		uint32_t t3 = (-alpha + ONE_BY_SQRT3 * beta) * PWMHalfPeriod;

		// PWM timings
		tB = (PWMHalfPeriod - t2 - t3) / 2;
		tA = tB + t3;
		tC = tA + t2;

		break;
	}

	// sector 3-4
	case 3: {
		// Vector on-times
		uint32_t t3 = (TWO_BY_SQRT3 * beta) * PWMHalfPeriod;
		uint32_t t4 = (-alpha - ONE_BY_SQRT3 * beta) * PWMHalfPeriod;

		// PWM timings
		tB = (PWMHalfPeriod - t3 - t4) / 2;
		tC = tB + t3;
		tA = tC + t4;

		break;
	}

	// sector 4-5
	case 4: {
		// Vector on-times
		uint32_t t4 = (-alpha + ONE_BY_SQRT3 * beta) * PWMHalfPeriod;
		uint32_t t5 = (-TWO_BY_SQRT3 * beta) * PWMHalfPeriod;

		// PWM timings
		tC = (PWMHalfPeriod - t4 - t5) / 2;
		tB = tC + t5;
		tA = tB + t4;

		break;
	}

	// sector 5-6
	case 5: {
		// Vector on-times
		uint32_t t5 = (-alpha - ONE_BY_SQRT3 * beta) * PWMHalfPeriod;
		uint32_t t6 = (alpha - ONE_BY_SQRT3 * beta) * PWMHalfPeriod;

		// PWM timings
		tC = (PWMHalfPeriod - t5 - t6) / 2;
		tA = tC + t5;
		tB = tA + t6;

		break;
	}

	// sector 6-1
	case 6: {
		// Vector on-times
		uint32_t t6 = (-TWO_BY_SQRT3 * beta) * PWMHalfPeriod;
		uint32_t t1 = (alpha + ONE_BY_SQRT3 * beta) * PWMHalfPeriod;

		// PWM timings
		tA = (PWMHalfPeriod - t6 - t1) / 2;
		tC = tA + t1;
		tB = tC + t6;

		break;
	}
	}

	*tAout = tA;
	*tBout = tB;
	*tCout = tC;
}

static void run_pid_control_pos(float dt) {
	static float i_term = 0;
	static float prev_error = 0;
	float p_term;
	float d_term;

	// PID is off. Return.
	if (m_control_mode != CONTROL_MODE_POS) {
		i_term = 0;
		prev_error = 0;
		return;
	}

	// Compute error
	float angle = encoder_read_deg();
	float error = utils_angle_difference(m_pos_pid_set_pos, angle);

	if (m_conf->foc_encoder_inverted) {
		error = -error;
	}


	// Compute parameters
	p_term = error * m_conf->p_pid_kp;
	i_term += error * (m_conf->p_pid_ki * dt);
	d_term = (error - prev_error) * (m_conf->p_pid_kd / dt);

	// I-term wind-up protection
	utils_truncate_number(&i_term, -1.0, 1.0);

	// Store previous error
	prev_error = error;

	// Calculate output
	float output = p_term + i_term + d_term;
	utils_truncate_number(&output, -1.0, 1.0);

	if (encoder_index_found()) {
		m_iq_set = output * m_conf->lo_current_max;
	} else {
		// Rotate the motor with 40 % power until the encoder index is found.
		m_iq_set = 0.4 * m_conf->lo_current_max;
	}
}

static void run_pid_control_speed(float dt) {
	static float i_term = 0.0;
	static float prev_error = 0.0;
	float p_term;
	float d_term;

	// PID is off. Return.
	if (m_control_mode != CONTROL_MODE_SPEED) {
		i_term = 0.0;
		prev_error = 0.0;
		return;
	}

	// Too low RPM set. Stop and return.
	if (fabsf(m_speed_pid_set_rpm) < m_conf->s_pid_min_erpm) {
		i_term = 0.0;
		prev_error = 0;
		m_iq_set = 0.0;
		m_state = MC_STATE_OFF;
		if (m_output_on) {
			stop_pwm_hw();
		}
		return;
	}

	// Compensation for supply voltage variations
	float scale = 1.0 / GET_INPUT_VOLTAGE();

	// Compute error
	float error = m_speed_pid_set_rpm - mcpwm_foc_get_rpm();

	// Compute parameters
	p_term = error * m_conf->s_pid_kp * scale;
	i_term += error * (m_conf->s_pid_ki * dt) * scale;
	d_term = (error - prev_error) * (m_conf->s_pid_kd / dt) * scale;

	// I-term wind-up protection
	utils_truncate_number(&i_term, -1.0, 1.0);

	// Store previous error
	prev_error = error;

	// Calculate output
	float output = p_term + i_term + d_term;
	utils_truncate_number(&output, -1.0, 1.0);

	m_iq_set = output * m_conf->lo_current_max;
}

static void stop_pwm_hw(void) {
	TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

	TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

	TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);

	m_output_on = false;
}

static void start_pwm_hw(void) {
	TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_PWM1);
	TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_PWM1);
	TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_PWM1);
	TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Enable);

	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);

	m_output_on = true;
}

static int read_hall(void) {
	return READ_HALL1() | (READ_HALL2() << 1) | (READ_HALL3() << 2);
}

static float correct_hall(float angle, float speed, float dt) {
	static int ang_hall_int_prev = -1;
	float rpm_abs = fabsf(speed / ((2.0 * M_PI) / 60.0));
	static bool using_hall = true;

	// Hysteresis of 100 rpm
	if (using_hall) {
		if (rpm_abs > (m_conf->foc_hall_sl_erpm + 100)) {
			using_hall = false;
		}
	} else {
		if (rpm_abs < (m_conf->foc_hall_sl_erpm - 100)) {
			using_hall = true;
		}
	}

	if (using_hall) {
		int ang_hall_int = m_conf->foc_hall_table[read_hall()];

		// Only override the observer if the hall sensor value is valid.
		if (ang_hall_int < 201) {
			static float ang_hall = 0.0;
			float ang_hall_now = (((float)ang_hall_int / 200.0) * 360.0) * M_PI / 180.0;

			if (ang_hall_int_prev < 0) {
				// Previous angle not valid
				ang_hall_int_prev = ang_hall_int;

				if (ang_hall_int_prev == -2) {
					// Before was sensorless, initialize with the provided angle
					ang_hall = angle;
				} else {
					// A boot or error has occurred. Use center of hall sensor angle.
					ang_hall = ((ang_hall_int / 200.0) * 360.0) * M_PI / 180.0;
				}
			} else if (ang_hall_int != ang_hall_int_prev) {
				// A transition was just made. The angle is in the middle of the new and old angle.
				int ang_avg = abs(ang_hall_int - ang_hall_int_prev);
				if (ang_avg < 100) {
					ang_avg = (ang_hall_int + ang_hall_int_prev) / 2;
				} else if (ang_avg != 100) {
					ang_avg = (ang_hall_int + ang_hall_int_prev) / 2 + 100;
				}
				ang_avg %= 200;
				ang_hall = (((float)ang_avg / 200.0) * 360.0) * M_PI / 180.0;
			}

			ang_hall_int_prev = ang_hall_int;

			if (rpm_abs < 100) {
				// Don't interpolate on very low speed, just use the closest hall sensor
				ang_hall = ang_hall_now;
			} else {
				// Interpolate
				float diff = utils_angle_difference_rad(ang_hall, ang_hall_now);
				if (fabsf(diff) < ((2.0 * M_PI) / 12.0)) {
					// Do interpolation
					ang_hall += speed * dt;
				} else {
					// We are too far away with the interpolation
					ang_hall -= diff / 100.0;
				}
			}

			utils_norm_angle_rad(&ang_hall);

			angle = ang_hall;
		} else {
			// Invalid hall reading. Don't update angle.
			ang_hall_int_prev = -1;

			// Also allow open loop in order to behave like normal sensorless
			// operation. Then the motor works even if the hall sensor cable
			// gets disconnected (when the sensor spacing is 120 degrees).
			if (m_phase_observer_override && m_state == MC_STATE_RUNNING) {
				angle = m_phase_now_observer_override;
			}
		}
	} else {
		// We are running sensorless.
		ang_hall_int_prev = -2;
	}

	return angle;
}
