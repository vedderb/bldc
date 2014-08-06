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

typedef struct {
	volatile float cycle_int_limit;
	volatile float cycle_int_limit_running;
	volatile uint32_t comms;
	volatile uint32_t time_at_comm;
} rpm_dep_struct;

// Private variables
static volatile int comm_step; // Range [1 6]
static volatile int detect_step; // Range [0 5]
static volatile int direction;
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
static volatile float switching_frequency_now;
static volatile int ignore_iterations;
static volatile mc_timer_struct timer_struct;
static volatile int timer_struct_updated;
static volatile int curr_samp_volt; // Use the voltage-synchronized samples for this current sample
static int hall_to_phase_table[16];
static volatile unsigned int cycles_running;
static volatile unsigned int slow_ramping_cycles;
static volatile int has_commutated;
static volatile rpm_dep_struct rpm_dep;

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

static volatile float last_adc_isr_duration;
static volatile float last_inj_adc_isr_duration;

// Global variables
volatile uint16_t ADC_Value[HW_ADC_CHANNELS];
volatile int ADC_curr_norm_value[3];
volatile float mcpwm_detect_currents[6];
volatile float mcpwm_detect_currents_diff[6];
volatile int mcpwm_vzero;

// Private functions
static void set_duty_cycle_hl(float dutyCycle);
static void set_duty_cycle_ll(float dutyCycle);
static void set_duty_cycle_hw(float dutyCycle);
static void stop_pwm_ll(void);
static void stop_pwm_hw(void);
static void full_brake_ll(void);
static void full_brake_hw(void);
static void fault_stop(mc_fault_code fault);
static void run_pid_controller(void);
static void set_next_comm_step(int next_step);
static void update_rpm_tacho(void);
static void update_adc_sample_pos(mc_timer_struct *timer_tmp);
static void commutate(void);
static void set_next_timer_settings(mc_timer_struct *settings);
static void set_switching_frequency(float frequency);
static int try_input(void);
static void do_dc_cal(void);

// Defines
#define CYCLE_INT_START			(0.0)
#define IS_DETECTING()			(state == MC_STATE_DETECTING)

// Threads
static WORKING_AREA(timer_thread_wa, 1024);
static msg_t timer_thread(void *arg);
static WORKING_AREA(rpm_thread_wa, 1024);
static msg_t rpm_thread(void *arg);

void mcpwm_init(void) {
	chSysLock();
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	TIM_BDTRInitTypeDef TIM_BDTRInitStructure;
	NVIC_InitTypeDef NVIC_InitStructure;

	// Initialize variables
	comm_step = 1;
	detect_step = 0;
	direction = 1;
	rpm_now = 0;
	dutycycle_set = 0.0;
	dutycycle_now = 0.0;
	speed_pid_set_rpm = 0.0;
	current_set = 0.0;
	tachometer = 0;
	tachometer_for_direction = 0;
	state = MC_STATE_OFF;
	fault_now = FAULT_CODE_NONE;
	pwm_mode = MCPWM_PWM_MODE;
	control_mode = CONTROL_MODE_NONE;
	last_current_sample = 0.0;
	last_current_sample_filtered = 0.0;
	motor_current_sum = 0.0;
	input_current_sum = 0.0;
	motor_current_iterations = 0.0;
	input_current_iterations = 0.0;
	switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MAX;
	ignore_iterations = 0;
	timer_struct_updated = 0;
	curr_samp_volt = 0;
	cycles_running = 0;
	slow_ramping_cycles = 0;
	has_commutated = 0;
	memset((void*)&rpm_dep, 0, sizeof(rpm_dep));

#if MCPWM_IS_SENSORLESS
	cycle_integrator = CYCLE_INT_START;
	pwm_cycles_sum = 0.0;
	last_pwm_cycles_sum = 0.0;
	memset((float*)last_pwm_cycles_sums, 0, sizeof(last_pwm_cycles_sums));
#endif

	mcpwm_init_hall_table(MCPWM_HALL_DIR, MCPWM_HALL_FWD_ADD, MCPWM_HALL_REV_ADD);

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
	TIM_TimeBaseStructure.TIM_Period = SYSTEM_CORE_CLOCK / (int)switching_frequency_now;
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
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 3;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 3;
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
	TIM_TimeBaseStructure.TIM_Period = SYSTEM_CORE_CLOCK / (int)switching_frequency_now;
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
	uint16_t PrescalerValue = (uint16_t) ((SYSTEM_CORE_CLOCK / 2) / MCPWM_RPM_TIMER_FREQ) - 1;

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFFFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = PrescalerValue;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM2, &TIM_TimeBaseStructure);

	// TIM2 enable counter
	TIM_Cmd(TIM2, ENABLE);

	// ADC sampling locations
	stop_pwm_hw();
	timer_struct.top = TIM1->ARR;
	timer_struct.duty = TIM1->ARR / 2;
	update_adc_sample_pos((mc_timer_struct*)&timer_struct);
	timer_struct_updated = 1;

	chSysUnlock();

	// Calibrate current offset
	ENABLE_GATE();
	DCCAL_OFF();
	do_dc_cal();

	// Various time measurements
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE);
	PrescalerValue = (uint16_t) ((SYSTEM_CORE_CLOCK / 2) / 10000000) - 1;

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFFFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = PrescalerValue;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM4, &TIM_TimeBaseStructure);

	// TIM3 enable counter
	TIM_Cmd(TIM4, ENABLE);

	// Start threads
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);
	chThdCreateStatic(rpm_thread_wa, sizeof(rpm_thread_wa), NORMALPRIO, rpm_thread, NULL);

	// WWDG configuration
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_WWDG, ENABLE);
	WWDG_SetPrescaler(WWDG_Prescaler_1);
	WWDG_SetWindowValue(255);
	WWDG_Enable(100);
}

/**
 * Initialize the hall sensor lookup table
 *
 * @param dir
 * Invert the direction
 *
 * @param fwd_add
 * Offset to add when the motor is spinning forwards
 *
 * @param rev_add
 * Offset to add when the motor is spinning reverse
 */
void mcpwm_init_hall_table(int dir, int fwd_add, int rev_add) {
	const int comms1[8] = {-1,1,3,2,5,6,4,-1};
	const int comms2[8] = {-1,1,5,6,3,2,4,-1};

	memcpy(hall_to_phase_table, dir ? comms1 : comms2, sizeof(int[8]));
	memcpy(hall_to_phase_table + 8, dir ? comms2 : comms1, sizeof(int[8]));

	for (int i = 1;i < 7;i++) {
		hall_to_phase_table[i    ] = ((hall_to_phase_table[i    ] + rev_add) % 6) + 1;
		hall_to_phase_table[8 + i] = ((hall_to_phase_table[8 + i] + fwd_add) % 6) + 1;
	}
}

static void do_dc_cal(void) {
	DCCAL_ON();
	while(IS_DRV_FAULT()){};
	chThdSleepMilliseconds(1000);
	curr0_sum = 0;
	curr1_sum = 0;
	curr_start_samples = 0;
	while(curr_start_samples < 4000) {};
	curr0_offset = curr0_sum / curr_start_samples;
	curr1_offset = curr1_sum / curr_start_samples;
	DCCAL_OFF();
}

/**
 * Use duty cycle control. Absolute values less than MCPWM_MIN_DUTY_CYCLE will
 * stop the motor.
 *
 * @param dutyCycle
 * The duty cycle to use.
 */
void mcpwm_set_duty(float dutyCycle) {
	if (try_input()) {
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
	if (try_input()) {
		return;
	}

	control_mode = CONTROL_MODE_SPEED;
	speed_pid_set_rpm = rpm;
}

/**
 * Use current control and specify a goal current to use. The sign determines
 * the direction of the torque. Absolute values less than
 * MCPWM_CURRENT_CONTROL_MIN will release the motor.
 *
 * @param current
 * The current to use.
 */
void mcpwm_set_current(float current) {
	if (try_input()) {
		return;
	}

	if (fabsf(current) < MCPWM_CURRENT_CONTROL_MIN) {
		control_mode = CONTROL_MODE_NONE;
		stop_pwm_ll();
		return;
	}

	utils_truncate_number(&current, MCPWM_CURRENT_MIN, MCPWM_CURRENT_MAX);

	control_mode = CONTROL_MODE_CURRENT;
	current_set = current;

	if (state != MC_STATE_RUNNING) {
		set_duty_cycle_hl(SIGN(current) * MCPWM_MIN_DUTY_CYCLE);
	}
}

/**
 * Brake the motor with a desired current. Absolute values less than
 * MCPWM_CURRENT_CONTROL_MIN will release the motor.
 *
 * @param current
 * The current to use. Positive and negative values give the same effect.
 */
void mcpwm_set_brake_current(float current) {
	if (try_input()) {
		return;
	}

	if (fabsf(current) < MCPWM_CURRENT_CONTROL_MIN) {
		control_mode = CONTROL_MODE_NONE;
		stop_pwm_ll();
		return;
	}

	utils_truncate_number(&current, -fabsf(MCPWM_CURRENT_MIN), fabsf(MCPWM_CURRENT_MIN));

	control_mode = CONTROL_MODE_CURRENT_BRAKE;
	current_set = current;

	if (state != MC_STATE_RUNNING) {
		// In case the motor is already spinning, set the state to running
		// so that it can be ramped down before the full brake is applied.
		// TODO: The number 500 is a hack...
		if (fabsf(rpm_now) > 500) {
			state = MC_STATE_RUNNING;
		} else {
			full_brake_ll();
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
 * Get the current switching frequency.
 *
 * @return
 * The switching frequency in Hz.
 */
float mcpwm_get_switching_frequency_now(void) {
	return switching_frequency_now;
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
	return last_current_sample * (V_REG / 4095.0) / (CURRENT_SHUNT_RES * CURRENT_AMP_GAIN);
}

/**
 * Get the FIR-filtered motor current.
 *
 * @return
 * The filtered motor current.
 */
float mcpwm_get_tot_current_filtered(void) {
	return last_current_sample_filtered * (V_REG / 4095.0) / (CURRENT_SHUNT_RES * CURRENT_AMP_GAIN);
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

static void stop_pwm_ll(void) {
	state = MC_STATE_OFF;
	ignore_iterations = MCPWM_CMD_STOP_TIME;
	stop_pwm_hw();
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

	set_switching_frequency(MCPWM_SWITCH_FREQUENCY_MAX);
}

static void full_brake_ll(void) {
	state = MC_STATE_FULL_BRAKE;
	ignore_iterations = MCPWM_CMD_STOP_TIME;
	full_brake_hw();
}

static void full_brake_hw(void) {
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

static void fault_stop(mc_fault_code fault) {
	ignore_iterations = MCPWM_FAULT_STOP_TIME;
	control_mode = CONTROL_MODE_NONE;
	state = MC_STATE_OFF;
	stop_pwm_hw();
	fault_now = fault;
}

/**
 * A helper function that should be called before sending commands to control
 * the motor. If the state is detecting, the detection will be stopped.
 *
 * @return
 * The amount if milliseconds left until user commands are allowed again.
 *
 */
static int try_input(void) {
	if (state == MC_STATE_DETECTING) {
		state = MC_STATE_OFF;
		stop_pwm_hw();
		ignore_iterations = MCPWM_DETECT_STOP_TIME;
	}

	return ignore_iterations;
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
		stop_pwm_ll();
		return;
	}

	dutycycle_set = dutyCycle;

	if (state != MC_STATE_RUNNING) {
		if (fabsf(dutyCycle) >= MCPWM_MIN_DUTY_CYCLE) {
			// dutycycle_now is updated by the back-emf detection. If the motor already
			// is spinning, it will be non-zero.
			if (fabsf(dutycycle_now) < MCPWM_MIN_DUTY_CYCLE) {
				dutycycle_now = SIGN(dutyCycle) * MCPWM_MIN_DUTY_CYCLE;
			}

			set_duty_cycle_ll(dutycycle_now);
		} else {
			// In case the motor is already spinning, set the state to running
			// so that it can be ramped down before the full brake is applied.
			if (fabsf(rpm_now) > MCPWM_MIN_RPM) {
				state = MC_STATE_RUNNING;
			} else {
				full_brake_ll();
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
	if (dutyCycle >= MCPWM_MIN_DUTY_CYCLE) {
		direction = 1;
	} else if (dutyCycle <= -MCPWM_MIN_DUTY_CYCLE) {
		dutyCycle = -dutyCycle;
		direction = 0;
	}

	if (dutyCycle < MCPWM_MIN_DUTY_CYCLE) {
		switch (state) {
		case MC_STATE_RUNNING:
			full_brake_ll();
			break;

		case MC_STATE_DETECTING:
			stop_pwm_ll();
			break;

		default:
			break;
		}
		return;
	} else if (dutyCycle > MCPWM_MAX_DUTY_CYCLE) {
		dutyCycle = MCPWM_MAX_DUTY_CYCLE;
	}

	set_duty_cycle_hw(dutyCycle);

#if MCPWM_IS_SENSORLESS
	if (state != MC_STATE_RUNNING) {
		state = MC_STATE_RUNNING;
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

	if (pwm_mode == PWM_MODE_BIPOLAR && !IS_DETECTING()) {
		timer_tmp.duty = (uint16_t) (((float) timer_tmp.top / 2.0) * dutyCycle
				+ ((float) timer_tmp.top / 2.0));
	} else {
		timer_tmp.duty = (uint16_t)((float)timer_tmp.top * dutyCycle);
	}

	if (IS_DETECTING() || pwm_mode == PWM_MODE_BIPOLAR) {
		switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MAX;
	} else {
		switching_frequency_now = (float)MCPWM_SWITCH_FREQUENCY_MIN * (1.0 - fabsf(dutyCycle)) +
				(float)MCPWM_SWITCH_FREQUENCY_MAX * fabsf(dutyCycle);
	}

	timer_tmp.top = SYSTEM_CORE_CLOCK / (int)switching_frequency_now;
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
		mcpwm_set_duty(0.0);
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
		output = SIGN(output) * MCPWM_MIN_DUTY_CYCLE;
	}

	// Do not output in reverse direction to oppose too high rpm
	if (speed_pid_set_rpm > 0.0 && output < 0.0) {
		output = MCPWM_MIN_DUTY_CYCLE;
		i_term = 0.0;
	} else if (speed_pid_set_rpm < 0.0 && output > 0.0) {
		output = -MCPWM_MIN_DUTY_CYCLE;
		i_term = 0.0;
	}

	set_duty_cycle_hl(output);
}

static msg_t rpm_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("rpm timer");

	for (;;) {
		if (rpm_dep.comms > 0.0) {
			chSysLock();
			const float comms = (float) rpm_dep.comms;
			const float time_at_comm = (float) rpm_dep.time_at_comm;
			rpm_dep.comms = 0;
			rpm_dep.time_at_comm = 0;
			chSysUnlock();

			rpm_now = (comms * MCPWM_RPM_TIMER_FREQ * 60.0)
					/ (time_at_comm * 6.0);
		} else {
			// In case we have slowed down
			float rpm_tmp = (MCPWM_RPM_TIMER_FREQ * 60.0)
					/ ((float) TIM2 ->CNT * 6.0);

			if (rpm_tmp < rpm_now) {
				rpm_now = rpm_tmp;
			}
		}

		// Some low-pass filtering
		static float rpm_p1 = 0.0;
		static float rpm_p2 = 0.0;
		rpm_now = (rpm_now + rpm_p1 + rpm_p2) / 3;
		rpm_p2 = rpm_p1;
		rpm_p1 = rpm_now;

		// Update the cycle integrator limit
		rpm_dep.cycle_int_limit = utils_map(rpm_now,
				MCPWM_CYCLE_INT_START_RPM_BR, 80000.0,
				MCPWM_CYCLE_INT_LIMIT_LOW, MCPWM_CYCLE_INT_LIMIT_HIGH);

		if (rpm_now < MCPWM_CYCLE_INT_START_RPM_BR) {
			rpm_dep.cycle_int_limit_running = utils_map(rpm_now, 0,
					MCPWM_CYCLE_INT_START_RPM_BR, MCPWM_CYCLE_INT_LIMIT_START,
					MCPWM_CYCLE_INT_LIMIT_LOW);
		} else {
			rpm_dep.cycle_int_limit_running = rpm_dep.cycle_int_limit;
		}

		chThdSleepMilliseconds(1);
	}

	return 0;
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
				dutycycle_now = amp / (float)ADC_Value[ADC_IND_VIN_SENS];
			} else {
				dutycycle_now = -amp / (float)ADC_Value[ADC_IND_VIN_SENS];
			}
			utils_truncate_number((float*)&dutycycle_now, -MCPWM_MAX_DUTY_CYCLE, MCPWM_MAX_DUTY_CYCLE);
			break;

		case MC_STATE_DETECTING:
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
				if (dutycycle_now >= MCPWM_MIN_DUTY_CYCLE) {
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
		if (ignore_iterations > 0) {
			ignore_iterations--;
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
	chSysLockFromIsr();

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

	chSysUnlockFromIsr();
}

void mcpwm_adc_inj_int_handler(void) {
	chSysLockFromIsr();

	TIM4->CNT = 0;

	static int detect_now = 0;

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

	if (detect_now == 4) {
		float a = fabsf(ADC_curr_norm_value[0]);
		float b = fabsf(ADC_curr_norm_value[1]);

		if (a > b) {
			mcpwm_detect_currents[detect_step] = a;
		} else {
			mcpwm_detect_currents[detect_step] = b;
		}

		if (detect_step > 0) {
			mcpwm_detect_currents_diff[detect_step] =
					mcpwm_detect_currents[detect_step - 1] - mcpwm_detect_currents[detect_step];
		} else {
			mcpwm_detect_currents_diff[detect_step] =
					mcpwm_detect_currents[5] - mcpwm_detect_currents[detect_step];
		}

		mcpwm_detect_currents_avg[detect_step] += mcpwm_detect_currents[detect_step];
		mcpwm_detect_currents_avg_samples[detect_step]++;

		if (detect_now > 1) {
			stop_pwm_hw();
		}
	}

	if (detect_now) {
		detect_now--;
	}

	if (IS_DETECTING() && detect_now == 0) {
		detect_now = 5;

		set_duty_cycle_hw(0.2);

		detect_step++;
		if (detect_step > 5) {
			detect_step = 0;
		}

		comm_step = detect_step + 1;

		set_next_comm_step(detect_step + 1);
		TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
	}

	last_current_sample = curr_tot_sample;
	filter_add_sample((float*) current_fir_samples, curr_tot_sample,
			CURR_FIR_TAPS_BITS, (uint32_t*) &current_fir_index);
	last_current_sample_filtered = filter_run_fir_iteration(
			(float*) current_fir_samples, (float*) current_fir_coeffs,
			CURR_FIR_TAPS_BITS, current_fir_index);

	last_inj_adc_isr_duration = (float) TIM4->CNT / 10000000;

	chSysUnlockFromIsr();
}

/*
 * New ADC samples ready. Do commutation!
 */
void mcpwm_adc_int_handler(void *p, uint32_t flags) {
	(void)p;
	(void)flags;

	chSysLockFromIsr();

	TIM4->CNT = 0;

	// Reset the watchdog
	WWDG_SetCounter(100);

	const float input_voltage = GET_INPUT_VOLTAGE();
	int ph1, ph2, ph3;

	static int direction_before = 1;
	if (state == MC_STATE_RUNNING && direction == direction_before) {
		cycles_running++;
	} else {
		cycles_running = 0;
		has_commutated = 0;
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

	/*
	 * If the motor has been running for a while use half the input voltage
	 * as the zero reference. Otherwise, calculate the zero reference manually.
	 */
	if (has_commutated) {
		mcpwm_vzero = ADC_V_ZERO;
	} else {
		mcpwm_vzero = (ADC_V_L1 + ADC_V_L2 + ADC_V_L3) / 3;
	}

	if (direction) {
		ph1 = ADC_V_L1 - mcpwm_vzero;
		ph2 = ADC_V_L2 - mcpwm_vzero;
		ph3 = ADC_V_L3 - mcpwm_vzero;
	} else {
		ph1 = ADC_V_L1 - mcpwm_vzero;
		ph2 = ADC_V_L3 - mcpwm_vzero;
		ph3 = ADC_V_L2 - mcpwm_vzero;
	}

	float amp = 0.0;

	if (has_commutated) {
		amp = fabsf(dutycycle_now) * (float)ADC_Value[ADC_IND_VIN_SENS];
	} else {
		amp = sqrtf((float)(ph1*ph1 + ph2*ph2 + ph3*ph3)) * sqrtf(2.0);
	}

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

	if ((state == MC_STATE_RUNNING && pwm_cycles_sum >= 2.0) || state == MC_STATE_OFF) {
		int v_diff = 0;
		switch (comm_step) {
		case 1:
			v_diff = ph1;
			break;
		case 2:
			v_diff = -ph2;
			break;
		case 3:
			v_diff = ph3;
			break;
		case 4:
			v_diff = -ph1;
			break;
		case 5:
			v_diff = ph2;
			break;
		case 6:
			v_diff = -ph3;
			break;
		default:
			break;
		}

		if (v_diff > 0) {
			cycle_integrator += (float)v_diff / switching_frequency_now;

			float limit;
			if (has_commutated) {
				limit = rpm_dep.cycle_int_limit_running * 0.0005;
			} else {
				limit = rpm_dep.cycle_int_limit * 0.0005;
			}

			if ((cycle_integrator >= MCPWM_CYCLE_INT_LIMIT_START * 0.0005 || pwm_cycles_sum > last_pwm_cycles_sum / 3.0 || !has_commutated)
					&& cycle_integrator >= limit) {
				commutate();
				cycle_integrator = CYCLE_INT_START;
			}
		} else {
			cycle_integrator = CYCLE_INT_START;
		}
	} else {
		cycle_integrator = CYCLE_INT_START;
	}

	pwm_cycles_sum += (float)MCPWM_SWITCH_FREQUENCY_MAX / switching_frequency_now;
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

	const float current = mcpwm_get_tot_current_filtered();
	const float current_in = current * fabsf(dutycycle_now);
	const float current_nofilter = mcpwm_get_tot_current();
	const float current_in_nofilter = current_nofilter * fabsf(dutycycle_now);
	motor_current_sum += current;
	input_current_sum += current_in;
	motor_current_iterations++;
	input_current_iterations++;

	if (fabsf(current_nofilter) > MCPWM_MAX_ABS_CURRENT) {
		fault_stop(FAULT_CODE_ABS_OVER_CURRENT);
	}

	if (state == MC_STATE_RUNNING && has_commutated) {
		// Compensation for supply voltage variations
		const float voltage_scale = 20.0 / input_voltage;
		float ramp_step = MCPWM_RAMP_STEP / (switching_frequency_now / 1000.0);
		float ramp_step_no_lim = ramp_step;
		const float rpm = mcpwm_get_rpm();

		if (slow_ramping_cycles) {
			slow_ramping_cycles--;
			ramp_step *= 0.1;
		}

		float dutycycle_now_tmp = dutycycle_now;

		if (control_mode == CONTROL_MODE_CURRENT) {
			// Compute error
			const float error = current_set - (direction ? current_nofilter : -current_nofilter);
			float step = error * MCPWM_CURRENT_CONTROL_GAIN * voltage_scale;
			const float start_boost = MCPWM_CURRENT_STARTUP_BOOST / voltage_scale;

			// Do not ramp too much
			utils_truncate_number(&step, -MCPWM_RAMP_STEP_CURRENT_MAX,
					MCPWM_RAMP_STEP_CURRENT_MAX);

			// Switching frequency correction
			step /= switching_frequency_now / 1000.0;

			if (slow_ramping_cycles) {
				slow_ramping_cycles--;
				step *= 0.1;
			}

			// Optionally apply startup boost.
			if (fabsf(dutycycle_now_tmp) < start_boost && (direction ? dutycycle_now > 0.0 : dutycycle_now < 0.0)) {
				utils_step_towards(&dutycycle_now_tmp,
						current_set > 0.0 ?
								start_boost :
								-start_boost, ramp_step);
			} else {
				dutycycle_now_tmp += step;
			}

			// Upper truncation
			utils_truncate_number((float*)&dutycycle_now_tmp, -MCPWM_MAX_DUTY_CYCLE, MCPWM_MAX_DUTY_CYCLE);

			// Lower truncation
			if (fabsf(dutycycle_now_tmp) < MCPWM_MIN_DUTY_CYCLE) {
				if (dutycycle_now_tmp < 0.0 && current_set > 0.0) {
					dutycycle_now_tmp = MCPWM_MIN_DUTY_CYCLE;
				} else if (dutycycle_now_tmp > 0.0 && current_set < 0.0) {
					dutycycle_now_tmp = -MCPWM_MIN_DUTY_CYCLE;
				}
			}

			// The set dutycycle should be in the correct direction in case the output is lower
			// than the minimum duty cycle and the mechanism below gets activated.
			dutycycle_set = dutycycle_now_tmp >= 0.0 ? MCPWM_MIN_DUTY_CYCLE : -MCPWM_MIN_DUTY_CYCLE;
		} else if (control_mode == CONTROL_MODE_CURRENT_BRAKE) {
			// Compute error
			const float error = -fabsf(current_set) - current_nofilter;
			float step = error * MCPWM_CURRENT_CONTROL_GAIN * voltage_scale;

			// Do not ramp too much
			utils_truncate_number(&step, -MCPWM_RAMP_STEP_CURRENT_MAX,
					MCPWM_RAMP_STEP_CURRENT_MAX);

			// Switching frequency correction
			step /= switching_frequency_now / 1000.0;

			if (slow_ramping_cycles) {
				slow_ramping_cycles--;
				step *= 0.1;
			}

			dutycycle_now_tmp += SIGN(dutycycle_now_tmp) * step;

			// Upper truncation
			utils_truncate_number((float*)&dutycycle_now_tmp, -MCPWM_MAX_DUTY_CYCLE, MCPWM_MAX_DUTY_CYCLE);

			// Lower truncation
			if (fabsf(dutycycle_now_tmp) < MCPWM_MIN_DUTY_CYCLE) {
				if (fabsf(rpm_now) < MCPWM_CURR_MIN_RPM_FBRAKE) {
					dutycycle_now_tmp = 0.0;
					dutycycle_set = dutycycle_now_tmp;
				} else {
					dutycycle_now_tmp = SIGN(dutycycle_now_tmp) * MCPWM_MIN_DUTY_CYCLE;
					dutycycle_set = dutycycle_now_tmp;
				}
			}
		} else {
			utils_step_towards((float*)&dutycycle_now_tmp, dutycycle_set, ramp_step);
		}

		static int limit_delay = 0;

		// Apply limits in priority order
		if (current_nofilter > MCPWM_CURRENT_MAX) {
			utils_step_towards((float*) &dutycycle_now, 0.0,
					ramp_step_no_lim * fabsf(current_nofilter - MCPWM_CURRENT_MAX) * MCPWM_CURRENT_LIMIT_GAIN);
			limit_delay = 1;
		} else if (current_nofilter < MCPWM_CURRENT_MIN) {
			utils_step_towards((float*) &dutycycle_now,
					direction ? MCPWM_MAX_DUTY_CYCLE : -MCPWM_MAX_DUTY_CYCLE, ramp_step_no_lim);
			limit_delay = 1;
		} else if (current_in_nofilter > MCPWM_IN_CURRENT_MAX) {
			utils_step_towards((float*) &dutycycle_now, 0.0,
					ramp_step_no_lim * fabsf(current_in_nofilter - MCPWM_IN_CURRENT_MAX) * MCPWM_CURRENT_LIMIT_GAIN);
			limit_delay = 1;
		} else if (current_in_nofilter < MCPWM_IN_CURRENT_MIN) {
			utils_step_towards((float*) &dutycycle_now,
					direction ? MCPWM_MAX_DUTY_CYCLE : -MCPWM_MAX_DUTY_CYCLE, ramp_step_no_lim);
			limit_delay = 1;
		} else if (rpm > MCPWM_RPM_MAX) {
			if ((MCPWM_RPM_LIMIT_NEG_TORQUE || current > -1.0) && dutycycle_now <= dutycycle_now_tmp) {
				utils_step_towards((float*) &dutycycle_now, 0.0, MCPWM_RAMP_STEP_RPM_LIMIT);
				limit_delay = 1;
				slow_ramping_cycles = 500;
			}
		} else if (rpm < MCPWM_RPM_MIN) {
			if ((MCPWM_RPM_LIMIT_NEG_TORQUE || current > -1.0) && dutycycle_now >= dutycycle_now_tmp) {
				utils_step_towards((float*) &dutycycle_now, 0.0, MCPWM_RAMP_STEP_RPM_LIMIT);
				limit_delay = 1;
				slow_ramping_cycles = 500;
			}
		}

		if (limit_delay > 0) {
			limit_delay--;
		} else {
			dutycycle_now = dutycycle_now_tmp;
		}

		// When the set duty cycle is in the opposite direction, make sure that the motor
		// starts again after stopping completely
		if (fabsf(dutycycle_now) < MCPWM_MIN_DUTY_CYCLE) {
			if (dutycycle_set >= MCPWM_MIN_DUTY_CYCLE) {
				dutycycle_now = MCPWM_MIN_DUTY_CYCLE;
			} else if (dutycycle_set <= -MCPWM_MIN_DUTY_CYCLE) {
				dutycycle_now = -MCPWM_MIN_DUTY_CYCLE;
			}
		}

		set_duty_cycle_ll(dutycycle_now);
	}

	main_dma_adc_handler();

	last_adc_isr_duration = (float)TIM4->CNT / 10000000;
	chSysUnlockFromIsr();
}

void mcpwm_set_detect(void) {
	if (try_input()) {
		return;
	}

	control_mode = CONTROL_MODE_NONE;
	stop_pwm_hw();

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
	return hall_to_phase_table[hall + (direction ? 8 : 0)];
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
	if (IS_DETECTING()) {
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
			val_sample = top / 4;
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
	rpm_dep.comms++;
	rpm_dep.time_at_comm += TIM2->CNT;
	TIM2->CNT = 0;

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
#if MCPWM_IS_SENSORLESS
	last_pwm_cycles_sum = pwm_cycles_sum;
	last_pwm_cycles_sums[comm_step - 1] = pwm_cycles_sum;
	pwm_cycles_sum = 0;

	update_rpm_tacho();

	comm_step++;
	if (comm_step > 6) {
		comm_step = 1;
	}

	if (!(state == MC_STATE_RUNNING)) {
		return;
	}

	set_next_comm_step(comm_step);
#endif
	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
	has_commutated = 1;

	mc_timer_struct timer_tmp;
	memcpy(&timer_tmp, (void*)&timer_struct, sizeof(mc_timer_struct));
	update_adc_sample_pos(&timer_tmp);
	set_next_timer_settings(&timer_tmp);
}

static void set_next_timer_settings(mc_timer_struct *settings) {
	memcpy((void*)&timer_struct, settings, sizeof(mc_timer_struct));

	chSysLock();

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

static void set_switching_frequency(float frequency) {
	switching_frequency_now = frequency;
	mc_timer_struct timer_tmp;
	memcpy(&timer_tmp, (void*)&timer_struct, sizeof(mc_timer_struct));
	timer_tmp.top = SYSTEM_CORE_CLOCK / (int)switching_frequency_now;
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

	if (!IS_DETECTING()) {
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
