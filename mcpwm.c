/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

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
 * mcpwm.c
 *
 *  Created on: 13 okt 2012
 *      Author: benjamin
 */

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "main.h"
#include "mcpwm.h"
#include "digital_filter.h"
#include "utils.h"
#include "ledpwm.h"
#include "comm.h"
#include "hw.h"

// Structs
typedef struct {
	volatile unsigned int top;
	volatile unsigned int duty;
	volatile unsigned int val_sample;
	volatile unsigned int curr1_sample;
	volatile unsigned int curr2_sample;
} mc_timer_struct;

// Private variables
static volatile int comm_step;
static volatile int direction;
static volatile int last_comm_time;
static volatile float dutycycle_set;
static volatile float dutycycle_now;
static volatile float rpm_now;
static volatile float speed_pid_set_rpm;
static volatile float current_set;
static volatile int tachometer;
static volatile int tachometer_for_direction;
static volatile int curr0_sum;
static volatile int curr1_sum;
static volatile int curr_start_samples;
static volatile int curr0_offset;
static volatile int curr1_offset;
static volatile mc_state state;
static volatile mc_fault_code fault_now;
static volatile int detect_now;
static volatile int detect_inc;
static volatile int detect_do_step;
static volatile mc_pwm_mode pwm_mode;
static volatile mc_control_mode control_mode;
static volatile float last_current_sample;
static volatile float last_current_sample_filtered;
static volatile float motor_current_sum;
static volatile float input_current_sum;
static volatile float motor_current_iterations;
static volatile float input_current_iterations;
static volatile float mcpwm_detect_currents_avg[6];
static volatile float mcpwm_detect_currents_avg_samples[6];
static volatile int switching_frequency_now;
static volatile int fault_iterations;
static volatile mc_timer_struct timer_struct;
static volatile int timer_struct_updated;
static volatile int curr_samp_volt; // Use the voltage-synchronized samples for this current sample

#if MCPWM_IS_SENSORLESS
static volatile float cycle_integrator;
static volatile float pwm_cycles_sum;
static volatile float last_pwm_cycles_sum;
static volatile float last_pwm_cycles_sums[6];
#endif

// KV FIR filter
#define KV_FIR_TAPS_BITS		7
#define KV_FIR_LEN				(1 << KV_FIR_TAPS_BITS)
#define KV_FIR_FCUT				0.02
static volatile float kv_fir_coeffs[KV_FIR_LEN];
static volatile float kv_fir_samples[KV_FIR_LEN];
static volatile int kv_fir_index = 0;

// Amplitude FIR filter
#define AMP_FIR_TAPS_BITS		7
#define AMP_FIR_LEN				(1 << KV_FIR_TAPS_BITS)
#define AMP_FIR_FCUT			0.02
static volatile float amp_fir_coeffs[KV_FIR_LEN];
static volatile float amp_fir_samples[KV_FIR_LEN];
static volatile int amp_fir_index = 0;

// Current FIR filter
#define CURR_FIR_TAPS_BITS		4
#define CURR_FIR_LEN			(1 << CURR_FIR_TAPS_BITS)
#define CURR_FIR_FCUT			0.15
static volatile float current_fir_coeffs[CURR_FIR_LEN];
static volatile float current_fir_samples[CURR_FIR_LEN];
static volatile int current_fir_index = 0;

// Hall sensor shift table
const unsigned int mc_shift_table[] = {
	// 0
	0b000,	// 000
	0b001,	// 001
	0b010,	// 010
	0b011,	// 011
	0b100,	// 100
	0b101,	// 101
	0b110,	// 110
	0b111,	// 111

	// 1
	0b000,	// 000
	0b001,	// 001
	0b100,	// 010
	0b101,	// 011
	0b010,	// 100
	0b011,	// 101
	0b110,	// 110
	0b111,	// 111

	// 2
	0b000,	// 000
	0b010,	// 001
	0b001,	// 010
	0b011,	// 011
	0b100,	// 100
	0b110,	// 101
	0b101,	// 110
	0b111,	// 111

	// 3
	0b000,	// 000
	0b100,	// 001
	0b010,	// 010
	0b110,	// 011
	0b001,	// 100
	0b101,	// 101
	0b011,	// 110
	0b111,	// 111

	// 4
	0b000,	// 000
	0b010,	// 001
	0b100,	// 010
	0b110,	// 011
	0b001,	// 100
	0b011,	// 101
	0b101,	// 110
	0b111,	// 111

	// 5
	0b000,	// 000
	0b100,	// 001
	0b001,	// 010
	0b101,	// 011
	0b010,	// 100
	0b110,	// 101
	0b011,	// 110
	0b111	// 111
};

static volatile unsigned int hall_sensor_order;
static volatile float last_adc_isr_duration;
static volatile float last_inj_adc_isr_duration;

// Global variables
volatile uint16_t ADC_Value[HW_ADC_CHANNELS];
volatile int ADC_curr_norm_value[3];
volatile float mcpwm_detect_currents[6];
volatile int mcpwm_vzero;

// Private functions
static void set_duty_cycle_hl(float dutyCycle);
static void set_duty_cycle_ll(float dutyCycle);
static void set_duty_cycle_hw(float dutyCycle);
static void stop_pwm(void);
static void fault_stop(mc_fault_code fault);
static void run_pid_controller(void);
static void set_next_comm_step(int next_step);
static void update_rpm_tacho(void);
static void update_adc_sample_pos(mc_timer_struct *timer_tmp);
static void commutate(void);
static void set_next_timer_settings(mc_timer_struct *settings);
static void set_switching_frequency(int frequency);
static void full_brake(void);

#if MCPWM_IS_SENSORLESS
static int integrate_cycle(float v_diff);
#endif

// Defines
#define ADC_CDR_ADDRESS			((uint32_t)0x40012308)
#define CYCLE_INT_START			(0)

// Threads
static WORKING_AREA(timer_thread_wa, 1024);
static msg_t timer_thread(void *arg);

void mcpwm_init(void) {
	chSysLock();
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	TIM_BDTRInitTypeDef TIM_BDTRInitStructure;
	NVIC_InitTypeDef NVIC_InitStructure;

	// Initialize variables
	comm_step = 1;
	direction = 1;
	rpm_now = 0;
	last_comm_time = 0;
	dutycycle_set = 0.0;
	dutycycle_now = 0.0;
	speed_pid_set_rpm = 0.0;
	current_set = 0.0;
	tachometer = 0;
	tachometer_for_direction = 0;
	state = MC_STATE_OFF;
	fault_now = FAULT_CODE_NONE;
	detect_now = 0;
	detect_inc = 0;
	detect_do_step = 0;
	hall_sensor_order = MCPWM_HALL_SENSOR_ORDER;
	pwm_mode = MCPWM_PWM_MODE;
	control_mode = CONTROL_MODE_NONE;
	last_current_sample = 0.0;
	last_current_sample_filtered = 0.0;
	motor_current_sum = 0.0;
	input_current_sum = 0.0;
	motor_current_iterations = 0.0;
	input_current_iterations = 0.0;
	switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MAX;
	fault_iterations = 0;
	timer_struct_updated = 0;
	curr_samp_volt = 0;

#if MCPWM_IS_SENSORLESS
	cycle_integrator = CYCLE_INT_START;
	pwm_cycles_sum = 0.0;
	last_pwm_cycles_sum = 0.0;
	memset((float*)last_pwm_cycles_sums, 0, sizeof(last_pwm_cycles_sums));
#endif

	// Create KV FIR filter
	filter_create_fir_lowpass((float*)kv_fir_coeffs, KV_FIR_FCUT, KV_FIR_TAPS_BITS, 1);

	// Create amplitude FIR filter
	filter_create_fir_lowpass((float*)amp_fir_coeffs, AMP_FIR_FCUT, AMP_FIR_TAPS_BITS, 1);

	// Create current FIR filter
	filter_create_fir_lowpass((float*)current_fir_coeffs, CURR_FIR_FCUT, CURR_FIR_TAPS_BITS, 1);

	// TIM1 clock enable
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, ENABLE);

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = 168000000 / switching_frequency_now;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;

	TIM_TimeBaseInit(TIM1, &TIM_TimeBaseStructure);

	// Channel 1, 2 and 3 Configuration in PWM mode
	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_OutputNState = TIM_OutputState_Enable;
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
	TIM_BDTRInitStructure.TIM_OSSIState = TIM_OSSRState_Enable;
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

	dmaStreamAllocate(STM32_DMA_STREAM(STM32_DMA_STREAM_ID(2, 4)),
			3,
			(stm32_dmaisr_t)mcpwm_adc_int_handler,
			(void *)0);

	// DMA for the ADC
	DMA_InitStructure.DMA_Channel = DMA_Channel_0;
	DMA_InitStructure.DMA_Memory0BaseAddr = (uint32_t)&ADC_Value;
	DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)ADC_CDR_ADDRESS;
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

	// Enable transfer complete interrupt
	DMA_ITConfig(DMA2_Stream4, DMA_IT_TC, ENABLE);

	// ADC Common Init
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

	hw_setup_adc_channels();

	// Enable DMA request after last transfer (Multi-ADC mode)
	ADC_MultiModeDMARequestAfterLastTransferCmd(ENABLE);

	// Injected channels for current measurement at end of cycle
	ADC_ExternalTrigInjectedConvConfig(ADC1, ADC_ExternalTrigInjecConv_T1_CC4);
	ADC_ExternalTrigInjectedConvConfig(ADC2, ADC_ExternalTrigInjecConv_T8_CC2);
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC1, ADC_ExternalTrigInjecConvEdge_Falling);
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC2, ADC_ExternalTrigInjecConvEdge_Falling);
	ADC_InjectedSequencerLengthConfig(ADC1, 1);
	ADC_InjectedSequencerLengthConfig(ADC2, 1);

	// Interrupt
	ADC_ITConfig(ADC1, ADC_IT_JEOC, ENABLE);
	NVIC_InitStructure.NVIC_IRQChannel = ADC_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

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
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = 168000000 / switching_frequency_now;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
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
	TIM_SelectOutputTrigger(TIM1, TIM_TRGOSource_Enable);
	TIM_SelectMasterSlaveMode(TIM1, TIM_MasterSlaveMode_Enable);
	TIM_SelectMasterSlaveMode(TIM8, TIM_MasterSlaveMode_Enable);
	TIM_SelectInputTrigger(TIM8, TIM_TS_ITR0);
	TIM_SelectSlaveMode(TIM8, TIM_SlaveMode_Gated);

	// Update interrupt
	TIM_ITConfig(TIM1, TIM_IT_Update, ENABLE);
	NVIC_InitStructure.NVIC_IRQChannel = TIM1_UP_TIM10_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	// Enable TIM8 first to make sure timers are in sync
	TIM_Cmd(TIM8, ENABLE);
	TIM_Cmd(TIM1, ENABLE);

	// Main Output Enable
	TIM_CtrlPWMOutputs(TIM1, ENABLE);

	// 32-bit timer for RPM measurement
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM2, ENABLE);
	uint16_t PrescalerValue = (uint16_t) ((168000000 / 2) / 1000000) - 1;

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFFFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = PrescalerValue;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM2, &TIM_TimeBaseStructure);

	// TIM2 enable counter
	TIM_Cmd(TIM2, ENABLE);

	// ADC sampling locations
	stop_pwm();
	timer_struct.top = TIM1->ARR;
	timer_struct.duty = TIM1->ARR / 2;
	update_adc_sample_pos((mc_timer_struct*)&timer_struct);
	timer_struct_updated = 1;

	chSysUnlock();

	// Calibrate current offset
	ENABLE_GATE();
	DCCAL_ON();
	chThdSleepMilliseconds(500);
	curr0_sum = 0;
	curr1_sum = 0;
	curr_start_samples = 0;
	while(curr_start_samples < 2000) {};
	curr0_offset = curr0_sum / curr_start_samples;
	curr1_offset = curr1_sum / curr_start_samples;
	DCCAL_OFF();

	// Various time measurements
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE);
	PrescalerValue = (uint16_t) ((168000000 / 2) / 10000000) - 1;

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFFFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = PrescalerValue;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM4, &TIM_TimeBaseStructure);

	// TIM3 enable counter
	TIM_Cmd(TIM4, ENABLE);

	// Start the timer thread
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);
}

/**
 * Use duty cycle control. Absolute values less than MCPWM_MIN_DUTY_CYCLE will
 * stop the motor.
 *
 * @param dutyCycle
 * The duty cycle to use.
 */
void mcpwm_set_duty(float dutyCycle) {
	if (fault_now != FAULT_CODE_NONE) {
		return;
	}

	control_mode = CONTROL_MODE_DUTY;

	set_duty_cycle_hl(dutyCycle);
}

/**
 * Use PID rpm control. Note that this value has to be multiplied by half of
 * the number of motor poles.
 *
 * @param rpm
 * The electrical RPM goal value to use.
 */
void mcpwm_set_pid_speed(float rpm) {
	if (fault_now != FAULT_CODE_NONE) {
		return;
	}

	control_mode = CONTROL_MODE_SPEED;
	speed_pid_set_rpm = rpm;
}

/**
 * Use current control and specify a goal current to use. The sign determines
 * the direction of the torque. Absolute values less than
 * MCPWM_CURRENT_CONTROL_MIN will stop the motor.
 *
 * @param current
 * The current to use.
 */
void mcpwm_set_current(float current) {
	if (fault_now != FAULT_CODE_NONE) {
		return;
	}

	if (fabsf(current) < MCPWM_CURRENT_CONTROL_MIN) {
		state = MC_STATE_OFF;
		control_mode = CONTROL_MODE_NONE;
		stop_pwm();
		return;
	}

	utils_truncate_number(&current, -MCPWM_CURRENT_MAX, MCPWM_CURRENT_MAX);

	control_mode = CONTROL_MODE_CURRENT;
	current_set = current;

	if (state != MC_STATE_RUNNING) {
		if (current > 0) {
			set_duty_cycle_hl(MCPWM_MIN_DUTY_CYCLE + 0.001);
		} else {
			set_duty_cycle_hl(-(MCPWM_MIN_DUTY_CYCLE + 0.001));
		}
	}
}

/**
 * Stop the motor and use braking.
 */
void mcpwm_brake_now(void) {
	mcpwm_set_duty(0.0);
}

/**
 * Disconnect the motor and let it turn freely.
 */
void mcpwm_release_motor(void) {
	mcpwm_set_current(0.0);
}

/**
 * Get the electrical position (or commutation step) of the motor.
 *
 * @return
 * The current commutation step. Range [1 6]
 */
int mcpwm_get_comm_step(void) {
	return comm_step;
}

float mcpwm_get_duty_cycle_set(void) {
	return dutycycle_set;
}

float mcpwm_get_duty_cycle_now(void) {
	return dutycycle_now;
}

/**
 * Calculate the current RPM of the motor. This is a signed value and the sign
 * depends on the direction the motor is rotating in. Note that this value has
 * to be divided by half the number of motor poles.
 *
 * @return
 * The RPM value.
 */
float mcpwm_get_rpm(void) {
	return direction ? rpm_now : -rpm_now;
}

mc_state mcpwm_get_state(void) {
	return state;
}

mc_fault_code mcpwm_get_fault(void) {
	return fault_now;
}

/**
 * Calculate the KV (RPM per volt) value for the motor. This function has to
 * be used while the motor is moving. Note that the return value has to be
 * divided by half the number of motor poles.
 *
 * @return
 * The KV value.
 */
float mcpwm_get_kv(void) {
	return rpm_now / (GET_INPUT_VOLTAGE() * fabsf(dutycycle_now));
}

/**
 * Calculate the FIR-filtered KV (RPM per volt) value for the motor. This
 * function has to be used while the motor is moving. Note that the return
 * value has to be divided by half the number of motor poles.
 *
 * @return
 * The filtered KV value.
 */
float mcpwm_get_kv_filtered(void) {
	float value = filter_run_fir_iteration((float*)kv_fir_samples,
			(float*)kv_fir_coeffs, KV_FIR_TAPS_BITS, kv_fir_index);

	return value;
}

/**
 * Get the motor current.
 *
 * @return
 * The motor current.
 */
float mcpwm_get_tot_current(void) {
	return last_current_sample * (3.3 / 4095.0) / (0.001 * 10.0);
}

/**
 * Get the FIR-filtered motor current.
 *
 * @return
 * The filtered motor current.
 */
float mcpwm_get_tot_current_filtered(void) {
	return last_current_sample_filtered * (3.3 / 4095.0) / (0.001 * 10.0);
}

/**
 * Get the input current to the motor controller.
 *
 * @return
 * The input current.
 */
float mcpwm_get_tot_current_in(void) {
	return mcpwm_get_tot_current() * fabsf(dutycycle_now);
}

/**
 * Get the FIR-filtered input current to the motor controller.
 *
 * @return
 * The filtered input current.
 */
float mcpwm_get_tot_current_in_filtered(void) {
	return mcpwm_get_tot_current_filtered() * fabsf(dutycycle_now);
}

/**
 * Read the number of steps the motor has rotated. This number is signed and
 * will return a negative number when the motor is rotating backwards.
 *
 * @param reset
 * If true (!= 0), the tachometer counter will be reset after this call.
 *
 * @return
 * The tachometer value in motor steps. The number of motor revolutions will
 * be this number divided by (3 * MOTOR_POLE_NUMBER).
 */
int mcpwm_get_tachometer_value(int reset) {
	int val = tachometer;

	if (reset) {
		tachometer = 0;
	}

	return val;
}

static void stop_pwm(void) {
	dutycycle_set = 0;

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

	set_switching_frequency(MCPWM_SWITCH_FREQUENCY_MAX);
}

static void fault_stop(mc_fault_code fault) {
	fault_iterations = MCPWM_FAULT_STOP_TIME;
	control_mode = CONTROL_MODE_NONE;
	state = MC_STATE_OFF;
	stop_pwm();
	fault_now = fault;
}

static void full_brake(void) {
	if (fault_now != FAULT_CODE_NONE) {
		return;
	}

	state = MC_STATE_FULL_BRAKE;

	dutycycle_set = 0;

	TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Enable);

	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);

	set_switching_frequency(MCPWM_SWITCH_FREQUENCY_MAX);
}

/**
 * High-level duty cycle setter. Will set the ramping goal of the duty cycle.
 * If motor is not running, it will be started in different ways depending on
 * whether it is moving or not.
 *
 * @param dutyCycle
 * The duty cycle in the range [-MCPWM_MAX_DUTY_CYCLE MCPWM_MAX_DUTY_CYCLE]
 * If the absolute value of the duty cycle is less than MCPWM_MIN_DUTY_CYCLE,
 * the motor phases will be shorted to brake the motor.
 */
static void set_duty_cycle_hl(float dutyCycle) {
	utils_truncate_number(&dutyCycle, -MCPWM_MAX_DUTY_CYCLE, MCPWM_MAX_DUTY_CYCLE);

	if (state == MC_STATE_DETECTING) {
		state = MC_STATE_OFF;
		stop_pwm();
		return;
	}

	dutycycle_set = dutyCycle;

	if (state != MC_STATE_RUNNING) {
		if (fabsf(dutyCycle) > MCPWM_MIN_DUTY_CYCLE) {
			if (fabsf(dutycycle_now) < MCPWM_MIN_DUTY_CYCLE) {
				if (dutyCycle > 0.0) {
					dutycycle_now = (MCPWM_MIN_DUTY_CYCLE + 0.001);
				} else {
					dutycycle_now = -(MCPWM_MIN_DUTY_CYCLE + 0.001);
				}
			}

			set_duty_cycle_ll(dutycycle_now);
		} else {
			if (state == MC_STATE_OFF) {
				// In case the motor is already spinning, set the state to running
				// so that it can be ramped down before the full brake is applied.
				state = MC_STATE_RUNNING;
			} else {
				state = MC_STATE_OFF;
				stop_pwm();
			}
		}
	}
}

/**
 * Low-level duty cycle setter. Will update the state of the application
 * and the motor direction accordingly.
 *
 * This function should be used with care. Ramping together with current
 * limiting should be used.
 *
 * @param dutyCycle
 * The duty cycle in the range [-MCPWM_MAX_DUTY_CYCLE MCPWM_MAX_DUTY_CYCLE]
 * If the absolute value of the duty cycle is less than MCPWM_MIN_DUTY_CYCLE,
 * the motor will be switched off.
 */
static void set_duty_cycle_ll(float dutyCycle) {
	if (dutyCycle > MCPWM_MIN_DUTY_CYCLE) {
		direction = 1;
	} else if (dutyCycle < -MCPWM_MIN_DUTY_CYCLE) {
		dutyCycle = -dutyCycle;
		direction = 0;
	}

	if (dutyCycle < MCPWM_MIN_DUTY_CYCLE) {
		switch (state) {
		case MC_STATE_RUNNING:
			state = MC_STATE_OFF;
			full_brake();
			break;

		case MC_STATE_DETECTING:
			state = MC_STATE_OFF;
			stop_pwm();
			break;

		default:
			break;
		}

		dutycycle_set = direction ? dutyCycle : -dutyCycle;
		return;
	} else if (dutyCycle > MCPWM_MAX_DUTY_CYCLE) {
		dutyCycle = MCPWM_MAX_DUTY_CYCLE;
	}

	set_duty_cycle_hw(dutyCycle);

#if MCPWM_IS_SENSORLESS
	if (state != MC_STATE_RUNNING) {
		state = MC_STATE_RUNNING;
		if (rpm_now < MCPWM_MIN_RPM) {
			set_next_comm_step(comm_step);
			commutate();
		}
	}
#else
	if (state != MC_STATE_RUNNING) {
		state = MC_STATE_RUNNING;
		set_next_comm_step(mcpwm_read_hall_phase());
		commutate();
	}
#endif
}

/**
 * Lowest level (hardware) dyty cycle setter. Will set the hardware timer to
 * the specified duty cycle and update the ADC sampling positions.
 *
 * @param dutyCycle
 * The dutycycle in the range [MCPWM_MIN_DUTY_CYCLE  MCPWM_MAX_DUTY_CYCLE]
 * (Only positive)
 */
static void set_duty_cycle_hw(float dutyCycle) {
	mc_timer_struct timer_tmp;
	memcpy(&timer_tmp, (void*)&timer_struct, sizeof(mc_timer_struct));

	utils_truncate_number(&dutyCycle, MCPWM_MIN_DUTY_CYCLE, MCPWM_MAX_DUTY_CYCLE);

	if (pwm_mode == PWM_MODE_BIPOLAR && state != MC_STATE_DETECTING) {
		timer_tmp.duty = (uint16_t) (((float) timer_tmp.top / 2.0) * dutyCycle
				+ ((float) timer_tmp.top / 2.0));
	} else {
		timer_tmp.duty = (uint16_t)((float)timer_tmp.top * dutyCycle);
	}

	if (state == MC_STATE_DETECTING || pwm_mode == PWM_MODE_BIPOLAR) {
		switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MAX;
	} else {
		switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MIN * (1.0 - fabsf(dutyCycle)) +
				MCPWM_SWITCH_FREQUENCY_MAX * fabsf(dutyCycle);
	}

	timer_tmp.top = 168000000 / switching_frequency_now;
	update_adc_sample_pos(&timer_tmp);
	set_next_timer_settings(&timer_tmp);
}

static void run_pid_controller(void) {
	static float i_term = 0;
	static float prev_error = 0;
	float p_term;
	float d_term;

	// PID is off. Return.
	if (control_mode != CONTROL_MODE_SPEED) {
		i_term = 0;
		prev_error = 0;
		return;
	}

	// Too low RPM set. Stop and return.
	if (fabsf(speed_pid_set_rpm) < MCPWM_PID_MIN_RPM) {
		i_term = 0;
		prev_error = 0;
		set_duty_cycle_hl(0.0);
		return;
	}

	// Compensation for supply voltage variations
	float scale = 1.0 / GET_INPUT_VOLTAGE();

	// Compute error
	float error = speed_pid_set_rpm - mcpwm_get_rpm();

	// Compute parameters
	p_term = error * MCPWM_PID_KP * scale;
	i_term += error * (MCPWM_PID_KI * MCPWM_PID_TIME_K) * scale;
	d_term = (error - prev_error) * (MCPWM_PID_KD / MCPWM_PID_TIME_K) * scale;

	// I-term wind-up protection
	utils_truncate_number(&i_term, -1.0, 1.0);

	// Store previous error
	prev_error = error;

	// Calculate output
	float output = p_term + i_term + d_term;

	// Make sure that at least minimum output is used
	if (fabsf(output) < MCPWM_MIN_DUTY_CYCLE) {
		if (output > 0.0) {
			output = MCPWM_MIN_DUTY_CYCLE;
		} else {
			output = -MCPWM_MIN_DUTY_CYCLE;
		}
	}

	// Do not output in reverse direction to oppose too high rpm
	if (speed_pid_set_rpm > 0.0 && output < 0.0) {
		output = MCPWM_MIN_DUTY_CYCLE + 0.001;
		i_term = 0.0;
	} else if (speed_pid_set_rpm < 0.0 && output > 0.0) {
		output = -(MCPWM_MIN_DUTY_CYCLE + 0.001);
		i_term = 0.0;
	}

	set_duty_cycle_hl(output);
}

static msg_t timer_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("mcpwm timer");

	float amp;

#if MCPWM_IS_SENSORLESS
	float min_s;
	float max_s;
#endif

	for(;;) {
		// Update RPM in case it has slowed down
		uint32_t tim_val = TIM2->CNT;
		uint32_t tim_diff = tim_val - last_comm_time;

		if (tim_diff > 0) {
			float rpm_tmp = ((float)MCPWM_AVG_COM_RPM * 1000000.0 * 60.0) /
					((float)tim_diff *  6.0);

			// Re-calculate RPM between commutations
			// This will end up being used when slowing down
			if (rpm_tmp < rpm_now) {
				rpm_now = rpm_tmp;
			}
		}

		if (state != MC_STATE_OFF) {
			tachometer_for_direction = 0;
		}

		switch (state) {
		case MC_STATE_OFF:
			// Track the motor back-emf and follow it with dutycycle_now. Also track
			// the direction of the motor.
			amp = filter_run_fir_iteration((float*)amp_fir_samples,
					(float*)amp_fir_coeffs, AMP_FIR_TAPS_BITS, amp_fir_index);

			// Direction tracking
#if MCPWM_IS_SENSORLESS
			min_s = 9999999999999.0;
			max_s = 0.0;

			for (int i = 0;i < 6;i++) {
				if (last_pwm_cycles_sums[i] < min_s) {
					min_s = last_pwm_cycles_sums[i];
				}

				if (last_pwm_cycles_sums[i] > max_s) {
					max_s = last_pwm_cycles_sums[i];
				}
			}

			// If the relative difference between the longest and shortest commutation is
			// too large, we probably got the direction wrong. In that case, try the other
			// direction.
			//
			// The tachometer_for_direction value is used to make sure that the samples
			// have enough time after a direction change to get stable before trying to
			// change direction again.

			if ((max_s - min_s) / ((max_s + min_s) / 2.0) > 1.2) {
				if (tachometer_for_direction > 12) {
					if (direction == 1) {
						direction = 0;
					} else {
						direction = 1;
					}
					tachometer_for_direction = 0;
				}
			} else {
				tachometer_for_direction = 0;
			}
#else
			// If the direction tachometer is counting backwards, the motor is
			// not moving in the direction we think it is.
			if (tachometer_for_direction < -3) {
				if (direction == 1) {
					direction = 0;
				} else {
					direction = 1;
				}
				tachometer_for_direction = 0;
			} else if (tachometer_for_direction > 0) {
				tachometer_for_direction = 0;
			}
#endif

			if (direction == 1) {
				dutycycle_now = amp / (float)ADC_Value[ADC_IND_VIN_SENS] * sqrtf(3.0);
			} else {
				dutycycle_now = -amp / (float)ADC_Value[ADC_IND_VIN_SENS] * sqrtf(3.0);
			}
			utils_truncate_number((float*)&dutycycle_now, -MCPWM_MAX_DUTY_CYCLE, MCPWM_MAX_DUTY_CYCLE);
			break;

		case MC_STATE_DETECTING:
			detect_do_step = 1;
			dutycycle_now = 0;
			break;

		case MC_STATE_RUNNING:
			break;

		case MC_STATE_FULL_BRAKE:
			break;

		default:
			break;
		}

		run_pid_controller();

		// Fill KV filter vector at 100Hz
		static int cnt_tmp = 0;
		cnt_tmp++;
		if (cnt_tmp >= 10) {
			cnt_tmp = 0;
			if (state == MC_STATE_RUNNING) {
				filter_add_sample((float*)kv_fir_samples, mcpwm_get_kv(),
						KV_FIR_TAPS_BITS, (uint32_t*)&kv_fir_index);
			} else if (state == MC_STATE_OFF) {
				if (dutycycle_now > MCPWM_MIN_DUTY_CYCLE) {
					filter_add_sample((float*)kv_fir_samples, mcpwm_get_kv(),
							KV_FIR_TAPS_BITS, (uint32_t*)&kv_fir_index);
				}
			}
		}

		// Check if the DRV8302 indicates any fault
		if (IS_DRV_FAULT()) {
			fault_stop(FAULT_CODE_DRV8302);
		}

		// Decrease fault iterations
		if (fault_iterations > 0) {
			fault_iterations--;
		} else {
			if (!IS_DRV_FAULT()) {
				fault_now = FAULT_CODE_NONE;
			}
		}

		chThdSleepMilliseconds(1);
	}

	return 0;
}

void mcpwm_update_int_handler(void) {
	if (timer_struct_updated) {
		TIM1->ARR = timer_struct.top;
		TIM8->ARR = timer_struct.top;
		TIM1->CCR1 = timer_struct.duty;
		TIM1->CCR2 = timer_struct.duty;
		TIM1->CCR3 = timer_struct.duty;
		TIM8->CCR1 = timer_struct.val_sample;
		TIM1->CCR4 = timer_struct.curr1_sample;
		TIM8->CCR2 = timer_struct.curr2_sample;
		timer_struct_updated = 0;
	}
}

void mcpwm_adc_inj_int_handler(void) {
	TIM4->CNT = 0;

	int curr0 = ADC_GetInjectedConversionValue(ADC1, ADC_InjectedChannel_1);
	int curr1 = ADC_GetInjectedConversionValue(ADC2, ADC_InjectedChannel_1);

	if (curr_samp_volt == 1) {
		curr0 = ADC_Value[ADC_IND_CURR1];
	} else if (curr_samp_volt == 2) {
		curr1 = ADC_Value[ADC_IND_CURR2];
	}

	curr0_sum += curr0;
	curr1_sum += curr1;
	curr_start_samples++;

	ADC_curr_norm_value[0] = curr0 - curr0_offset;
	ADC_curr_norm_value[1] = curr1 - curr1_offset;
	ADC_curr_norm_value[2] = -(ADC_curr_norm_value[0] + ADC_curr_norm_value[1]);

	float curr_tot_sample = 0;

	switch (comm_step) {
	case 1:
	case 6:
		if (direction) {
			curr_tot_sample = (float)ADC_curr_norm_value[2];
		} else {
			curr_tot_sample = (float)ADC_curr_norm_value[1];
		}
		break;

	case 2:
	case 3:
		curr_tot_sample = (float)ADC_curr_norm_value[0];
		break;

	case 4:
	case 5:
		if (direction) {
			curr_tot_sample = (float)ADC_curr_norm_value[1];
		} else {
			curr_tot_sample = (float)ADC_curr_norm_value[2];
		}
		break;
	}

	if (detect_now == 1) {
		float a = fabsf(ADC_curr_norm_value[0]);
		float b = fabsf(ADC_curr_norm_value[1]);

		if (a > b) {
			mcpwm_detect_currents[comm_step - 1] = a;
		} else {
			mcpwm_detect_currents[comm_step - 1] = b;
		}

		mcpwm_detect_currents_avg[comm_step - 1] += mcpwm_detect_currents[comm_step - 1];
		mcpwm_detect_currents_avg_samples[comm_step - 1]++;

		stop_pwm();
	}

	if (detect_now) {
		detect_now--;
	}

	if (detect_do_step) {
		detect_now = 2;

		set_duty_cycle_hw(0.5);

		direction = 1;
		comm_step++;
		if (comm_step > 6) {
			comm_step = 1;
		}

		set_next_comm_step(comm_step);
		TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
		detect_do_step = 0;
	}

	last_current_sample = curr_tot_sample;
	filter_add_sample((float*) current_fir_samples, curr_tot_sample,
			CURR_FIR_TAPS_BITS, (uint32_t*) &current_fir_index);
	last_current_sample_filtered = filter_run_fir_iteration(
			(float*) current_fir_samples, (float*) current_fir_coeffs,
			CURR_FIR_TAPS_BITS, current_fir_index);

	last_inj_adc_isr_duration = (float) TIM4 ->CNT / 10000000;
}

/*
 * New ADC samples ready. Do commutation!
 */
void mcpwm_adc_int_handler(void *p, uint32_t flags) {
	(void)p;
	(void)flags;

	TIM4->CNT = 0;

	const float input_voltage = GET_INPUT_VOLTAGE();
	volatile int ph1, ph2, ph3;

	/*
	 * Changing the duty cycle too rapidly after a stop or after a direction change
	 * seems to cause problems for some reason. Therefore, slow down the ramping
	 * after such events.
	 *
	 * TODO: Figure out what the real problem is...
	 */
	static volatile unsigned int cycles_running = 0;
	static volatile int direction_before = 1;
	if (state == MC_STATE_RUNNING && direction == direction_before) {
		cycles_running++;
	} else {
		cycles_running = 0;
	}
	direction_before = direction;

	// Check for faults that should stop the motor
	static float wrong_voltage_iterations = 0;
	if (input_voltage < MCPWM_MIN_VOLTAGE ||
			input_voltage > MCPWM_MAX_VOLTAGE) {
		wrong_voltage_iterations++;

		if ((wrong_voltage_iterations >= 3)) {
			fault_stop(input_voltage < MCPWM_MIN_VOLTAGE ?
					FAULT_CODE_UNDER_VOLTAGE : FAULT_CODE_OVER_VOLTAGE);
		}
	} else {
		wrong_voltage_iterations = 0;
	}

	mcpwm_vzero = (ADC_V_L1 + ADC_V_L2 + ADC_V_L3) / 3;

	if (direction) {
		ph1 = ADC_V_L1 - mcpwm_vzero;
		ph2 = ADC_V_L2 - mcpwm_vzero;
		ph3 = ADC_V_L3 - mcpwm_vzero;
	} else {
		ph1 = ADC_V_L1 - mcpwm_vzero;
		ph2 = ADC_V_L3 - mcpwm_vzero;
		ph3 = ADC_V_L2 - mcpwm_vzero;
	}

	float amp = sqrtf(((float)(ph1*ph1 + ph2*ph2 + ph3*ph3)) / 1.5);

	// Fill the amplitude FIR filter
	filter_add_sample((float*)amp_fir_samples, amp,
			AMP_FIR_TAPS_BITS, (uint32_t*)&amp_fir_index);

#if MCPWM_IS_SENSORLESS
	// Compute the theoretical commutation time at the current RPM
	const float comm_time_sum = ((float)MCPWM_SWITCH_FREQUENCY_MAX) /
			(((float)MCPWM_MIN_RPM / 60.0) * 6.0);

	if (pwm_cycles_sum >= comm_time_sum) {
		if (state == MC_STATE_RUNNING) {
			commutate();
			cycle_integrator = CYCLE_INT_START;
		}
	}

	if (pwm_cycles_sum > last_pwm_cycles_sum / 3 || state != MC_STATE_RUNNING) {
		int inc_step = 0;
		int v_diff = 0;

		switch (comm_step) {
		case 1:
			if (ph1 > 0) {
				inc_step = 1;
			}
			v_diff = ph1;
			break;

		case 2:
			if (ph2 < 0) {
				inc_step = 1;
			}
			v_diff = -ph2;
			break;

		case 3:
			if (ph3 > 0) {
				inc_step = 1;
			}
			v_diff = ph3;
			break;

		case 4:
			if (ph1 < 0) {
				inc_step = 1;
			}
			v_diff = -ph1;
			break;

		case 5:
			if (ph2 > 0) {
				inc_step = 1;
			}
			v_diff = ph2;
			break;

		case 6:
			if (ph3 < 0) {
				inc_step = 1;
			}
			v_diff = -ph3;
			break;

		default:
			break;
		}

		if (inc_step) {
			if (state == MC_STATE_RUNNING || state == MC_STATE_OFF) {
				integrate_cycle((float)v_diff);
			}
		} else {
			cycle_integrator = CYCLE_INT_START;
		}
	}

	pwm_cycles_sum += (float)MCPWM_SWITCH_FREQUENCY_MAX / (float)switching_frequency_now;
#else
	int hall_phase = mcpwm_read_hall_phase();
	if (comm_step != hall_phase) {
		comm_step = hall_phase;

		update_rpm_tacho();

		if (state == MC_STATE_RUNNING) {
			set_next_comm_step(comm_step);
			commutate();
		}
	}
#endif

	if (state == MC_STATE_RUNNING) {
		// Compensation for supply voltage variations
		const float voltage_scale = 20.0 / input_voltage;
		float ramp_step = MCPWM_RAMP_STEP / (switching_frequency_now / 1000.0);
		const float current = mcpwm_get_tot_current_filtered();
		const float current_in = current * fabsf(dutycycle_now);
		const float rpm = mcpwm_get_rpm();

		if (fabsf(dutycycle_now) > MCPWM_MIN_DUTY_CYCLE) {
			ramp_step *= fabsf(dutycycle_now);
		}

		motor_current_sum += current;
		input_current_sum += current_in;
		motor_current_iterations++;
		input_current_iterations++;

		float dutycycle_now_tmp = dutycycle_now;

		if (control_mode == CONTROL_MODE_CURRENT) {
			// Compute error
			float error = current_set - (direction ? current : -current);
			float step = error * MCPWM_CURRENT_CONTROL_GAIN * voltage_scale;

			if (cycles_running < 1000) {
				utils_truncate_number(&step, -ramp_step, ramp_step);
			}

			dutycycle_now_tmp += step;

			if (fabsf(dutycycle_now_tmp) < MCPWM_MIN_DUTY_CYCLE) {
				if (dutycycle_now_tmp < 0.0 && current_set > 0.0) {
					dutycycle_now_tmp = MCPWM_MIN_DUTY_CYCLE + 0.001;
				} else if (dutycycle_now_tmp > 0.0 && current_set < 0.0) {
					dutycycle_now_tmp = -(MCPWM_MIN_DUTY_CYCLE + 0.001);
				}
			}

			utils_truncate_number((float*)&dutycycle_now_tmp, -MCPWM_MAX_DUTY_CYCLE, MCPWM_MAX_DUTY_CYCLE);

			// The set dutycycle should be in the correct direction in case the output is lower
			// than the minimum duty cycle and the mechanism below gets activated.
			dutycycle_set = dutycycle_now_tmp > 0 ? MCPWM_MIN_DUTY_CYCLE + 0.001 : -(MCPWM_MIN_DUTY_CYCLE + 0.001);
		} else {
			step_towards((float*)&dutycycle_now_tmp, dutycycle_set, ramp_step);
		}

		// Apply limits in priority order
		if (current > MCPWM_CURRENT_MAX) {
			step_towards((float*) &dutycycle_now, 0.0,
					ramp_step * fabsf(current - MCPWM_CURRENT_MAX) * MCPWM_CURRENT_LIMIT_GAIN);
		} else if (current < MCPWM_CURRENT_MIN) {
			step_towards((float*) &dutycycle_now,
					direction ? MCPWM_MAX_DUTY_CYCLE : -MCPWM_MAX_DUTY_CYCLE,
					ramp_step * fabsf(current - MCPWM_CURRENT_MAX) * MCPWM_CURRENT_LIMIT_GAIN);
		} else if (current_in > MCPWM_IN_CURRENT_MAX) {
			step_towards((float*) &dutycycle_now, 0.0,
					ramp_step * fabsf(current_in - MCPWM_IN_CURRENT_MAX) * MCPWM_CURRENT_LIMIT_GAIN);
		} else if (current_in < MCPWM_IN_CURRENT_MIN) {
			step_towards((float*) &dutycycle_now,
					direction ? MCPWM_MAX_DUTY_CYCLE : -MCPWM_MAX_DUTY_CYCLE,
					ramp_step * fabsf(current_in - MCPWM_IN_CURRENT_MIN) * MCPWM_CURRENT_LIMIT_GAIN);
		} else if (rpm > MCPWM_RPM_MAX) {
			step_towards((float*) &dutycycle_now, 0.0, ramp_step);
			cycles_running = 0;
		} else if (rpm < MCPWM_RPM_MIN) {
			step_towards((float*) &dutycycle_now, 0.0, ramp_step);
			cycles_running = 0;
		} else {
			dutycycle_now = dutycycle_now_tmp;
		}

		// When the set duty cycle is in the opposite direction, make sure that the motor
		// starts again after stopping completely
		if (fabsf(dutycycle_now) <= MCPWM_MIN_DUTY_CYCLE) {
			if (dutycycle_set > MCPWM_MIN_DUTY_CYCLE) {
				dutycycle_now = (MCPWM_MIN_DUTY_CYCLE + 0.001);
			} else if (dutycycle_set < -MCPWM_MIN_DUTY_CYCLE) {
				dutycycle_now = -(MCPWM_MIN_DUTY_CYCLE + 0.001);
			}
		}

		set_duty_cycle_ll(dutycycle_now);
	}

	main_dma_adc_handler();

	last_adc_isr_duration = (float)TIM4->CNT / 10000000;
}

#if MCPWM_IS_SENSORLESS
static int integrate_cycle(float v_diff) {
	cycle_integrator += v_diff / (float)switching_frequency_now;

	const float rpm_fac = rpm_now / 50000.0;
	const float cycle_int_limit = MCPWM_CYCLE_INT_LIMIT_LOW * (1.0 - rpm_fac) +
			MCPWM_CYCLE_INT_LIMIT_HIGH * rpm_fac;
	const float limit = (cycle_int_limit * 0.0005);

	if (cycle_integrator >= limit) {
		commutate();
		cycle_integrator = CYCLE_INT_START;
		return 1;
	}

	return 0;
}
#endif

void mcpwm_set_detect(void) {
	if (fault_now != FAULT_CODE_NONE) {
		return;
	}

	control_mode = CONTROL_MODE_NONE;
	stop_pwm();

	set_switching_frequency(MCPWM_SWITCH_FREQUENCY_MAX);

	for(int i = 0;i < 6;i++) {
		mcpwm_detect_currents[i] = 0;
		mcpwm_detect_currents_avg[i] = 0;
		mcpwm_detect_currents_avg_samples[i] = 0;
	}

	state = MC_STATE_DETECTING;
}

float mcpwm_get_detect_pos(void) {
	float v[6];
	v[0] = mcpwm_detect_currents_avg[0] / mcpwm_detect_currents_avg_samples[0];
	v[1] = mcpwm_detect_currents_avg[1] / mcpwm_detect_currents_avg_samples[1];
	v[2] = mcpwm_detect_currents_avg[2] / mcpwm_detect_currents_avg_samples[2];
	v[3] = mcpwm_detect_currents_avg[3] / mcpwm_detect_currents_avg_samples[3];
	v[4] = mcpwm_detect_currents_avg[4] / mcpwm_detect_currents_avg_samples[4];
	v[5] = mcpwm_detect_currents_avg[5] / mcpwm_detect_currents_avg_samples[5];

	for(int i = 0;i < 6;i++) {
		mcpwm_detect_currents_avg[i] = 0;
		mcpwm_detect_currents_avg_samples[i] = 0;
	}

	float v0 = v[0] + v[3];
	float v1 = v[1] + v[4];
	float v2 = v[2] + v[5];

	float offset = (v0 + v1 + v2) / 3.0;
	v0 -= offset;
	v1 -= offset;
	v2 -= offset;

	float amp = sqrtf((v0*v0 + v1*v1 + v2*v2) / 1.5);
	v0 /= amp;
	v1 /= amp;
	v2 /= amp;

	float ph[1];
	ph[0] = asinf(v0) * 180.0 / M_PI;

	float res = ph[0];
	if (v1 < v2) {
		res = 180 - ph[0];
	}

	utils_norm_angle(&res);

	return res;
}

float mcpwm_read_reset_avg_motor_current(void) {
	float res = motor_current_sum / motor_current_iterations;
	motor_current_sum = 0;
	motor_current_iterations = 0;
	return res;
}

float mcpwm_read_reset_avg_input_current(void) {
	float res = input_current_sum / input_current_iterations;
	input_current_sum = 0;
	input_current_iterations = 0;
	return res;
}

float mcpwm_get_last_adc_isr_duration(void) {
	return last_adc_isr_duration;
}

float mcpwm_get_last_inj_adc_isr_duration(void) {
	return last_inj_adc_isr_duration;
}

/**
 * Read the current phase of the motor using hall effect sensors
 * @return
 * The phase read.
 */
signed int mcpwm_read_hall_phase(void) {
	int hall = READ_HALL1() | (READ_HALL2() << 1) | (READ_HALL3() << 2);

	signed int tmp_phase = -1;
	int shift = mc_shift_table[hall + (hall_sensor_order << 3)];

	switch (shift) {
	case 0b101:
		tmp_phase = 1;
		break;

	case 0b001:
		tmp_phase = 2;
		break;

	case 0b011:
		tmp_phase = 3;
		break;

	case 0b010:
		tmp_phase = 4;
		break;

	case 0b110:
		tmp_phase = 5;
		break;

	case 0b100:
		tmp_phase = 6;
		break;
	case 0b000:
	case 0b111:
		tmp_phase = -1;
		break;
	}

	// TODO: Gurgalof-fix
	tmp_phase--;
	if (tmp_phase == 0) {
		tmp_phase = 6;
	}

	// This is NOT a proper way to solve this...
	if (!direction && tmp_phase > 0) {
		signed int p_tmp = tmp_phase;
		p_tmp += 4;
		if (p_tmp > 5) {
			p_tmp -= 6;
		}
		tmp_phase = 6 - p_tmp;
	}

	return tmp_phase;
}

/*
 * Commutation Steps FORWARDS
 * STEP		BR1		BR2		BR3
 * 1		0		+		-
 * 2		+		0		-
 * 3		+		-		0
 * 4		0		-		+
 * 5		-		0		+
 * 6		-		+		0
 *
 * Commutation Steps REVERSE (switch phase 2 and 3)
 * STEP		BR1		BR2		BR3
 * 1		0		-		+
 * 2		+		-		0
 * 3		+		0		-
 * 4		0		+		-
 * 5		-		+		0
 * 6		-		0		+
 */

static void update_adc_sample_pos(mc_timer_struct *timer_tmp) {
	volatile uint32_t duty = timer_tmp->duty;
	volatile uint32_t top = timer_tmp->top;
	volatile uint32_t val_sample = timer_tmp->val_sample;
	volatile uint32_t curr1_sample = timer_tmp->curr1_sample;
	volatile uint32_t curr2_sample = timer_tmp->curr2_sample;

	// Sample the ADC at an appropriate time during the pwm cycle
	if (state == MC_STATE_DETECTING) {
		// Voltage samples
		val_sample = 200;

		// Current samples
		curr1_sample = (top - duty) / 2 + duty;
		curr2_sample = (top - duty) / 2 + duty;
	} else {
		if (pwm_mode == PWM_MODE_BIPOLAR) {
			uint32_t samp_neg = top - 2;
			uint32_t samp_pos = duty + (top - duty) / 2;
			uint32_t samp_zero = top - 2;

			// Voltage and other sampling
			val_sample = duty / 2;
			curr_samp_volt = 0;

			// Current sampling
			switch (comm_step) {
			case 1:
				if (direction) {
					curr1_sample = samp_zero;
					curr2_sample = samp_neg;
					curr_samp_volt = 2;
				} else {
					curr1_sample = samp_zero;
					curr2_sample = samp_pos;
				}
				break;

			case 2:
				if (direction) {
					curr1_sample = samp_pos;
					curr2_sample = samp_neg;
					curr_samp_volt = 2;
				} else {
					curr1_sample = samp_pos;
					curr2_sample = samp_zero;
				}
				break;

			case 3:
				if (direction) {
					curr1_sample = samp_pos;
					curr2_sample = samp_zero;
				} else {
					curr1_sample = samp_pos;
					curr2_sample = samp_neg;
					curr_samp_volt = 2;
				}
				break;

			case 4:
				if (direction) {
					curr1_sample = samp_zero;
					curr2_sample = samp_pos;
				} else {
					curr1_sample = samp_zero;
					curr2_sample = samp_neg;
					curr_samp_volt = 2;
				}
				break;

			case 5:
				if (direction) {
					curr1_sample = samp_neg;
					curr2_sample = samp_pos;
					curr_samp_volt = 1;
				} else {
					curr1_sample = samp_neg;
					curr2_sample = samp_zero;
					curr_samp_volt = 1;
				}
				break;

			case 6:
				if (direction) {
					curr1_sample = samp_neg;
					curr2_sample = samp_zero;
					curr_samp_volt = 1;
				} else {
					curr1_sample = samp_neg;
					curr2_sample = samp_pos;
					curr_samp_volt = 1;
				}
				break;
			}
		} else {
			// Voltage samples
			val_sample = duty / 2;

			// Current samples
			curr1_sample = duty + (top - duty) / 2;
			curr2_sample = duty + (top - duty) / 2;
		}
	}

	timer_tmp->val_sample = val_sample;
	timer_tmp->curr1_sample = curr1_sample;
	timer_tmp->curr2_sample = curr2_sample;
}

static void update_rpm_tacho(void) {
	static uint32_t comm_counter = 0;
	comm_counter++;

	if (comm_counter == MCPWM_AVG_COM_RPM) {
		comm_counter = 0;
		uint32_t tim_val = TIM2->CNT;
		uint32_t tim_diff = tim_val - last_comm_time;
		last_comm_time = tim_val;

		if (tim_diff > 0) {
			rpm_now = ((float)MCPWM_AVG_COM_RPM * 1000000.0 * 60.0) /
					((float)tim_diff *  6.0);
		}
	}

	static int last_step = 0;
	int tacho_diff = 0;

	if (comm_step == 1 && last_step == 6) {
		tacho_diff++;
	} else if (comm_step == 6 && last_step == 1) {
		tacho_diff--;
	} else {
		tacho_diff += comm_step - last_step;
	}

	last_step = comm_step;

	// Tachometers
	tachometer_for_direction += tacho_diff;

	if (direction) {
		tachometer += tacho_diff;
	} else {
		tachometer -= tacho_diff;
	}
}

static void commutate(void) {
	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);

#if MCPWM_IS_SENSORLESS
	last_pwm_cycles_sum = pwm_cycles_sum;
	last_pwm_cycles_sums[comm_step - 1] = pwm_cycles_sum;
	pwm_cycles_sum = 0;

	update_rpm_tacho();

	comm_step++;
	if (comm_step > 6) {
		comm_step = 1;
	}

	int next_step = comm_step + 1;
	if (next_step > 6) {
		next_step = 1;
	}

	if (!(state == MC_STATE_RUNNING)) {
		return;
	}

	set_next_comm_step(next_step);
#endif
	mc_timer_struct timer_tmp;
	memcpy(&timer_tmp, (void*)&timer_struct, sizeof(mc_timer_struct));
	update_adc_sample_pos(&timer_tmp);
	set_next_timer_settings(&timer_tmp);
}

static void set_next_timer_settings(mc_timer_struct *settings) {
	chSysLock();
	memcpy((void*)&timer_struct, settings, sizeof(mc_timer_struct));

	int cnt = TIM1->CNT;
	int top = TIM1->ARR;

	// If there is enough time to update all values at once during this cycle,
	// do it here. Otherwise, schedule the update for the next cycle.
	if ((top - cnt) > 400) {
		TIM1->ARR = timer_struct.top;
		TIM8->ARR = timer_struct.top;
		TIM1->CCR1 = timer_struct.duty;
		TIM1->CCR2 = timer_struct.duty;
		TIM1->CCR3 = timer_struct.duty;
		TIM8->CCR1 = timer_struct.val_sample;
		TIM1->CCR4 = timer_struct.curr1_sample;
		TIM8->CCR2 = timer_struct.curr2_sample;
	} else {
		timer_struct_updated = 1;
	}

	chSysUnlock();
}

static void set_switching_frequency(int frequency) {
	switching_frequency_now = frequency;
	mc_timer_struct timer_tmp;
	memcpy(&timer_tmp, (void*)&timer_struct, sizeof(mc_timer_struct));
	timer_tmp.top = 168000000 / switching_frequency_now;
	update_adc_sample_pos(&timer_tmp);
	set_next_timer_settings(&timer_tmp);
}

static void set_next_comm_step(int next_step) {
	uint16_t positive_oc_mode = TIM_OCMode_PWM1;
	uint16_t negative_oc_mode = TIM_OCMode_Inactive;

	uint16_t positive_highside = TIM_CCx_Enable;
	uint16_t positive_lowside = TIM_CCxN_Enable;

	uint16_t negative_highside = TIM_CCx_Enable;
	uint16_t negative_lowside = TIM_CCxN_Enable;

	if (state != MC_STATE_DETECTING) {
		switch (pwm_mode) {
		case PWM_MODE_NONSYNCHRONOUS_HISW:
			positive_lowside = TIM_CCxN_Disable;
			break;

		case PWM_MODE_SYNCHRONOUS:
			break;

		case PWM_MODE_BIPOLAR:
			negative_oc_mode = TIM_OCMode_PWM2;
			break;
		}
	}

	if (next_step == 1) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_2, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_3, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_3, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_2, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, negative_lowside);
		}
	} else if (next_step == 2) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_1, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_3, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_1, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_2, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, negative_lowside);
		}
	} else if (next_step == 3) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_1, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_2, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_1, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_3, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, negative_lowside);
		}
	} else if (next_step == 4) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_3, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_2, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_2, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_3, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, negative_lowside);
		}
	} else if (next_step == 5) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_3, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_1, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_2, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_1, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, negative_lowside);
		}
	} else if (next_step == 6) {
		if (direction) {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_2, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_2, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_1, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, negative_lowside);
		} else {
			// 0
			TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_3, positive_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_3, positive_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, positive_lowside);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_1, negative_oc_mode);
			TIM_CCxCmd(TIM1, TIM_Channel_1, negative_highside);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, negative_lowside);
		}
	} else if (next_step == 32) {
		// NOTE: This means we are going to use sine modulation. Switch on all phases!
		TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_PWM1);
		TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Enable);

		TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_PWM1);
		TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Enable);

		TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_PWM1);
		TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Enable);
	} else {
		// Invalid phase.. stop PWM!
		TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_ForcedAction_InActive);
		TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

		TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_ForcedAction_InActive);
		TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

		TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_ForcedAction_InActive);
		TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);
	}
}
