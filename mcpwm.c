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
#include "hw.h"
#include "terminal.h"
#include "encoder.h"

// Structs
typedef struct {
	volatile bool updated;
	volatile unsigned int top;
	volatile unsigned int duty;
	volatile unsigned int val_sample;
	volatile unsigned int curr1_sample;
	volatile unsigned int curr2_sample;
} mc_timer_struct;

// Private variables
static volatile int comm_step; // Range [1 6]
static volatile int detect_step; // Range [0 5]
static volatile int direction;
static volatile float dutycycle_set;
static volatile float dutycycle_now;
static volatile float rpm_now;
static volatile float speed_pid_set_rpm;
static volatile float pos_pid_set_pos;
static volatile float current_set;
static volatile int tachometer;
static volatile int tachometer_abs;
static volatile int tachometer_for_direction;
static volatile int curr0_sum;
static volatile int curr1_sum;
static volatile int curr_start_samples;
static volatile int curr0_offset;
static volatile int curr1_offset;
static volatile mc_state state;
static volatile mc_fault_code fault_now;
static volatile mc_control_mode control_mode;
static volatile float last_current_sample;
static volatile float last_current_sample_filtered;
static volatile float motor_current_sum;
static volatile float input_current_sum;
static volatile float motor_current_iterations;
static volatile float input_current_iterations;
static volatile float mcpwm_detect_currents_avg[6];
static volatile float mcpwm_detect_avg_samples[6];
static volatile float switching_frequency_now;
static volatile int ignore_iterations;
static volatile mc_timer_struct timer_struct;
static volatile int curr_samp_volt; // Use the voltage-synchronized samples for this current sample
static int hall_to_phase_table[16];
static volatile unsigned int cycles_running;
static volatile unsigned int slow_ramping_cycles;
static volatile int has_commutated;
static volatile mc_rpm_dep_struct rpm_dep;
static volatile float cycle_integrator_sum;
static volatile float cycle_integrator_iterations;
static volatile mc_configuration conf;
static volatile float pwm_cycles_sum;
static volatile float last_pwm_cycles_sum;
static volatile float last_pwm_cycles_sums[6];
static volatile float amp_seconds;
static volatile float amp_seconds_charged;
static volatile float watt_seconds;
static volatile float watt_seconds_charged;
static volatile bool dccal_done;
static volatile bool lock_enabled;
static volatile bool lock_override_once;
static volatile bool sensorless_now;
static volatile int hall_detect_table[8][7];

// KV FIR filter
#define KV_FIR_TAPS_BITS		7
#define KV_FIR_LEN				(1 << KV_FIR_TAPS_BITS)
#define KV_FIR_FCUT				0.02
static volatile float kv_fir_coeffs[KV_FIR_LEN];
static volatile float kv_fir_samples[KV_FIR_LEN];
static volatile int kv_fir_index = 0;

// Amplitude FIR filter
#define AMP_FIR_TAPS_BITS		7
#define AMP_FIR_LEN				(1 << AMP_FIR_TAPS_BITS)
#define AMP_FIR_FCUT			0.02
static volatile float amp_fir_coeffs[AMP_FIR_LEN];
static volatile float amp_fir_samples[AMP_FIR_LEN];
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
volatile float mcpwm_detect_voltages[6];
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
static void run_pid_control_speed(void);
static void run_pid_control_pos(float dt);
static void set_next_comm_step(int next_step);
static void update_rpm_tacho(void);
static void update_sensor_mode(void);
static int read_hall(void);
static void update_adc_sample_pos(mc_timer_struct *timer_tmp);
static void commutate(int steps);
static void set_next_timer_settings(mc_timer_struct *settings);
static void update_timer_attempt(void);
static void set_switching_frequency(float frequency);
static int try_input(void);
static void do_dc_cal(void);
static void update_override_limits(volatile mc_configuration *conf);

// Defines
#define IS_DETECTING()			(state == MC_STATE_DETECTING)

// Threads
static WORKING_AREA(timer_thread_wa, 2048);
static msg_t timer_thread(void *arg);
static WORKING_AREA(rpm_thread_wa, 1024);
static msg_t rpm_thread(void *arg);

void mcpwm_init(mc_configuration *configuration) {
	utils_sys_lock_cnt();

	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	TIM_BDTRInitTypeDef TIM_BDTRInitStructure;
	NVIC_InitTypeDef NVIC_InitStructure;

	conf = *configuration;

	// Initialize variables
	comm_step = 1;
	detect_step = 0;
	direction = 1;
	rpm_now = 0.0;
	dutycycle_set = 0.0;
	dutycycle_now = 0.0;
	speed_pid_set_rpm = 0.0;
	pos_pid_set_pos = 0.0;
	current_set = 0.0;
	tachometer = 0;
	tachometer_abs = 0;
	tachometer_for_direction = 0;
	state = MC_STATE_OFF;
	fault_now = FAULT_CODE_NONE;
	control_mode = CONTROL_MODE_NONE;
	last_current_sample = 0.0;
	last_current_sample_filtered = 0.0;
	motor_current_sum = 0.0;
	input_current_sum = 0.0;
	motor_current_iterations = 0.0;
	input_current_iterations = 0.0;
	switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MAX;
	ignore_iterations = 0;
	curr_samp_volt = 0;
	cycles_running = 0;
	slow_ramping_cycles = 0;
	has_commutated = 0;
	memset((void*)&rpm_dep, 0, sizeof(rpm_dep));
	cycle_integrator_sum = 0.0;
	cycle_integrator_iterations = 0.0;
	pwm_cycles_sum = 0.0;
	last_pwm_cycles_sum = 0.0;
	memset((float*)last_pwm_cycles_sums, 0, sizeof(last_pwm_cycles_sums));
	amp_seconds = 0.0;
	amp_seconds_charged = 0.0;
	watt_seconds = 0.0;
	watt_seconds_charged = 0.0;
	dccal_done = false;
	lock_enabled = false;
	lock_override_once = false;
	memset(hall_detect_table, 0, sizeof(hall_detect_table[0][0]) * 8 * 7);
	update_sensor_mode();

	mcpwm_init_hall_table((int8_t*)conf.hall_table);

	// Create KV FIR filter
	filter_create_fir_lowpass((float*)kv_fir_coeffs, KV_FIR_FCUT, KV_FIR_TAPS_BITS, 1);

	// Create amplitude FIR filter
	filter_create_fir_lowpass((float*)amp_fir_coeffs, AMP_FIR_FCUT, AMP_FIR_TAPS_BITS, 1);

	// Create current FIR filter
	filter_create_fir_lowpass((float*)current_fir_coeffs, CURR_FIR_FCUT, CURR_FIR_TAPS_BITS, 1);

	TIM_DeInit(TIM1);
	TIM_DeInit(TIM8);
	TIM1->CNT = 0;
	TIM8->CNT = 0;

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
	TIM_TimeBaseStructure.TIM_Period = 0xFFFF;
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
	TIM_SelectOutputTrigger(TIM1, TIM_TRGOSource_Update);
	TIM_SelectMasterSlaveMode(TIM1, TIM_MasterSlaveMode_Enable);
	TIM_SelectInputTrigger(TIM8, TIM_TS_ITR0);
	TIM_SelectSlaveMode(TIM8, TIM_SlaveMode_Reset);

	// Enable TIM1 and TIM8
	TIM_Cmd(TIM1, ENABLE);
	TIM_Cmd(TIM8, ENABLE);

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
	mc_timer_struct timer_tmp;
	timer_tmp.top = TIM1->ARR;
	timer_tmp.duty = TIM1->ARR / 2;
	update_adc_sample_pos(&timer_tmp);
	set_next_timer_settings(&timer_tmp);

	utils_sys_unlock_cnt();

	// Calibrate current offset
	ENABLE_GATE();
	DCCAL_OFF();
	do_dc_cal();

	// Various time measurements
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM12, ENABLE);
	PrescalerValue = (uint16_t) ((SYSTEM_CORE_CLOCK / 2) / 10000000) - 1;

	// Time base configuration
	TIM_TimeBaseStructure.TIM_Period = 0xFFFFFFFF;
	TIM_TimeBaseStructure.TIM_Prescaler = PrescalerValue;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM12, &TIM_TimeBaseStructure);

	// TIM3 enable counter
	TIM_Cmd(TIM12, ENABLE);

	// Start threads
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);
	chThdCreateStatic(rpm_thread_wa, sizeof(rpm_thread_wa), NORMALPRIO, rpm_thread, NULL);

	// WWDG configuration
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_WWDG, ENABLE);
	WWDG_SetPrescaler(WWDG_Prescaler_1);
	WWDG_SetWindowValue(255);
	WWDG_Enable(100);

	// Reset tachometers again
	tachometer = 0;
	tachometer_abs = 0;
}

const volatile mc_configuration* mcpwm_get_configuration(void) {
	return &conf;
}

void mcpwm_set_configuration(mc_configuration *configuration) {
	// Stop everything first to be safe
	control_mode = CONTROL_MODE_NONE;
	stop_pwm_ll();

	utils_sys_lock_cnt();
	conf = *configuration;
	update_override_limits(&conf);
	mcpwm_init_hall_table((int8_t*)conf.hall_table);
	update_sensor_mode();
	utils_sys_unlock_cnt();
}

/**
 * Initialize the hall sensor lookup table
 *
 * @param table
 * The commutations corresponding to the hall sensor states in the forward direction-
 */
void mcpwm_init_hall_table(int8_t *table) {
	const int fwd_to_rev[7] = {-1,4,3,2,1,6,5};

	for (int i = 0;i < 8;i++) {
		hall_to_phase_table[8 + i] = table[i];
		int ind_now = hall_to_phase_table[8 + i];

		if (ind_now < 1) {
			hall_to_phase_table[i] = ind_now;
			continue;
		}

		ind_now += 2;
		if (ind_now > 6) {
			ind_now -= 6;
		}
		ind_now = fwd_to_rev[ind_now];

		hall_to_phase_table[i] = ind_now;
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
	dccal_done = true;
}

/**
 * Update the override limits for a configuration based on MOSFET temperature etc.
 *
 * @param conf
 * The configaration to update.
 */
static void update_override_limits(volatile mc_configuration *conf) {
	float temp = NTC_TEMP(ADC_IND_TEMP_MOS1);

	if (temp < conf->l_temp_fet_start) {
		conf->lo_current_min = conf->l_current_min;
		conf->lo_current_max = conf->l_current_max;
	} else if (temp > conf->l_temp_fet_end) {
		conf->lo_current_min = 0.0;
		conf->lo_current_max = 0.0;
		fault_stop(FAULT_CODE_OVER_TEMP_FET);
	} else {
		float maxc = fabsf(conf->l_current_max);
		if (fabsf(conf->l_current_min) > maxc) {
			maxc = fabsf(conf->l_current_min);
		}

		maxc = utils_map(temp, conf->l_temp_fet_start, conf->l_temp_fet_end, maxc, 0.0);

		if (fabsf(conf->l_current_max) > maxc) {
			conf->lo_current_max = SIGN(conf->l_current_max) * maxc;
		}

		if (fabsf(conf->l_current_min) > maxc) {
			conf->lo_current_min = SIGN(conf->l_current_min) * maxc;
		}
	}

	conf->lo_in_current_max = conf->l_in_current_max;
	conf->lo_in_current_min = conf->l_in_current_min;
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
 * Use PID position control. Note that this only works when encoder support
 * is enabled.
 *
 * @param pos
 * The desired position of the motor in degrees.
 */
void mcpwm_set_pid_pos(float pos) {
	if (try_input() || !ENCODER_ENABLE) {
		return;
	}

	control_mode = CONTROL_MODE_POS;
	pos_pid_set_pos = pos;

	if (state != MC_STATE_RUNNING) {
		set_duty_cycle_hl(conf.l_min_duty);
	}
}

/**
 * Use current control and specify a goal current to use. The sign determines
 * the direction of the torque. Absolute values less than
 * conf.cc_min_current will release the motor.
 *
 * @param current
 * The current to use.
 */
void mcpwm_set_current(float current) {
	if (try_input()) {
		return;
	}

	if (fabsf(current) < conf.cc_min_current) {
		control_mode = CONTROL_MODE_NONE;
		stop_pwm_ll();
		return;
	}

	utils_truncate_number(&current, conf.lo_current_min, conf.lo_current_max);

	control_mode = CONTROL_MODE_CURRENT;
	current_set = current;

	if (state != MC_STATE_RUNNING) {
		set_duty_cycle_hl(SIGN(current) * conf.l_min_duty);
	}
}

/**
 * Brake the motor with a desired current. Absolute values less than
 * conf.cc_min_current will release the motor.
 *
 * @param current
 * The current to use. Positive and negative values give the same effect.
 */
void mcpwm_set_brake_current(float current) {
	if (try_input()) {
		return;
	}

	if (fabsf(current) < conf.cc_min_current) {
		control_mode = CONTROL_MODE_NONE;
		stop_pwm_ll();
		return;
	}

	utils_truncate_number(&current, -fabsf(conf.lo_current_min), fabsf(conf.lo_current_min));

	control_mode = CONTROL_MODE_CURRENT_BRAKE;
	current_set = current;

	if (state != MC_STATE_RUNNING && state != MC_STATE_FULL_BRAKE) {
		// In case the motor is already spinning, set the state to running
		// so that it can be ramped down before the full brake is applied.
		if (conf.motor_type == MOTOR_TYPE_DC) {
			if (fabsf(dutycycle_now) > 0.1) {
				state = MC_STATE_RUNNING;
			} else {
				full_brake_ll();
			}
		} else {
			if (fabsf(rpm_now) > conf.l_max_erpm_fbrake) {
				state = MC_STATE_RUNNING;
			} else {
				full_brake_ll();
			}
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
 * Lock the control by disabling all control commands.
 */
void mcpwm_lock(void) {
	lock_enabled = true;
}

/**
 * Unlock all control commands.
 */
void mcpwm_unlock(void) {
	lock_enabled = false;
}

/**
 * Allow just one motor control command in the locked state.
 */
void mcpwm_lock_override_once(void) {
	lock_override_once = true;
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

const char* mcpwm_fault_to_string(mc_fault_code fault) {
	switch (fault) {
	case FAULT_CODE_NONE: return "FAULT_CODE_NONE"; break;
	case FAULT_CODE_OVER_VOLTAGE: return "FAULT_CODE_OVER_VOLTAGE"; break;
	case FAULT_CODE_UNDER_VOLTAGE: return "FAULT_CODE_UNDER_VOLTAGE"; break;
	case FAULT_CODE_DRV8302: return "FAULT_CODE_DRV8302"; break;
	case FAULT_CODE_ABS_OVER_CURRENT: return "FAULT_CODE_ABS_OVER_CURRENT"; break;
	case FAULT_CODE_OVER_TEMP_FET: return "FAULT_CODE_OVER_TEMP_FET"; break;
	case FAULT_CODE_OVER_TEMP_MOTOR: return "FAULT_CODE_OVER_TEMP_MOTOR"; break;
	default: return "FAULT_UNKNOWN"; break;
	}
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
 * Get the motor current. The sign of this value will
 * represent whether the motor is drawing (positive) or generating
 * (negative) current.
 *
 * @return
 * The motor current.
 */
float mcpwm_get_tot_current(void) {
	return last_current_sample * (V_REG / 4095.0) / (CURRENT_SHUNT_RES * CURRENT_AMP_GAIN);
}

/**
 * Get the FIR-filtered motor current. The sign of this value will
 * represent whether the motor is drawing (positive) or generating
 * (negative) current.
 *
 * @return
 * The filtered motor current.
 */
float mcpwm_get_tot_current_filtered(void) {
	return last_current_sample_filtered * (V_REG / 4095.0) / (CURRENT_SHUNT_RES * CURRENT_AMP_GAIN);
}

/**
 * Get the motor current. The sign of this value represents the direction
 * in which the motor generates torque.
 *
 * @return
 * The motor current.
 */
float mcpwm_get_tot_current_directional(void) {
	const float retval = mcpwm_get_tot_current();
	return dutycycle_now > 0.0 ? retval : -retval;
}

/**
 * Get the filtered motor current. The sign of this value represents the
 * direction in which the motor generates torque.
 *
 * @return
 * The filtered motor current.
 */
float mcpwm_get_tot_current_directional_filtered(void) {
	const float retval = mcpwm_get_tot_current_filtered();
	return dutycycle_now > 0.0 ? retval : -retval;
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
 * If true, the tachometer counter will be reset after this call.
 *
 * @return
 * The tachometer value in motor steps. The number of motor revolutions will
 * be this number divided by (3 * MOTOR_POLE_NUMBER).
 */
int mcpwm_get_tachometer_value(bool reset) {
	int val = tachometer;

	if (reset) {
		tachometer = 0;
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
int mcpwm_get_tachometer_abs_value(bool reset) {
	int val = tachometer_abs;

	if (reset) {
		tachometer_abs = 0;
	}

	return val;
}

/**
 * Get the amount of amp hours drawn from the input source.
 *
 * @param reset
 * If true, the counter will be reset after this call.
 *
 * @return
 * The amount of amp hours drawn.
 */
float mcpwm_get_amp_hours(bool reset) {
	float val = amp_seconds / 3600;

	if (reset) {
		amp_seconds = 0.0;
	}

	return val;
}

/**
 * Get the amount of amp hours fed back into the input source.
 *
 * @param reset
 * If true, the counter will be reset after this call.
 *
 * @return
 * The amount of amp hours fed back.
 */
float mcpwm_get_amp_hours_charged(bool reset) {
	float val = amp_seconds_charged / 3600;

	if (reset) {
		amp_seconds_charged = 0.0;
	}

	return val;
}

/**
 * Get the amount of watt hours drawn from the input source.
 *
 * @param reset
 * If true, the counter will be reset after this call.
 *
 * @return
 * The amount of watt hours drawn.
 */
float mcpwm_get_watt_hours(bool reset) {
	float val = watt_seconds / 3600;

	if (reset) {
		amp_seconds = 0.0;
	}

	return val;
}

/**
 * Get the amount of watt hours fed back into the input source.
 *
 * @param reset
 * If true, the counter will be reset after this call.
 *
 * @return
 * The amount of watt hours fed back.
 */
float mcpwm_get_watt_hours_charged(bool reset) {
	float val = watt_seconds_charged / 3600;

	if (reset) {
		watt_seconds_charged = 0.0;
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
	if (dccal_done && fault_now == FAULT_CODE_NONE) {
		// Sent to terminal fault logger so that all faults and their conditions
		// can be printed for debugging.
		chSysLock();
		volatile int t1_cnt = TIM1->CNT;
		volatile int t8_cnt = TIM8->CNT;
		chSysUnlock();

		fault_data fdata;
		fdata.fault = fault;
		fdata.current = mcpwm_get_tot_current();
		fdata.current_filtered = mcpwm_get_tot_current_filtered();
		fdata.voltage = GET_INPUT_VOLTAGE();
		fdata.duty = dutycycle_now;
		fdata.rpm = mcpwm_get_rpm();
		fdata.tacho = mcpwm_get_tachometer_value(false);
		fdata.tim_pwm_cnt = t1_cnt;
		fdata.tim_samp_cnt = t8_cnt;
		fdata.comm_step = comm_step;
		fdata.temperature = NTC_TEMP(ADC_IND_TEMP_MOS1);
		terminal_add_fault_data(&fdata);
	}

	ignore_iterations = conf.m_fault_stop_time_ms;
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

	int retval = ignore_iterations;

	if (!ignore_iterations && lock_enabled) {
		if (!lock_override_once) {
			retval = 1;
		} else {
			lock_override_once = false;
		}
	}

	return retval;
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
	utils_truncate_number(&dutyCycle, -conf.l_max_duty, conf.l_max_duty);

	if (state == MC_STATE_DETECTING) {
		stop_pwm_ll();
		return;
	}

	dutycycle_set = dutyCycle;

	if (state != MC_STATE_RUNNING) {
		if (fabsf(dutyCycle) >= conf.l_min_duty) {
			// dutycycle_now is updated by the back-emf detection. If the motor already
			// is spinning, it will be non-zero.
			if (fabsf(dutycycle_now) < conf.l_min_duty) {
				dutycycle_now = SIGN(dutyCycle) * conf.l_min_duty;
			}

			set_duty_cycle_ll(dutycycle_now);
		} else {
			// In case the motor is already spinning, set the state to running
			// so that it can be ramped down before the full brake is applied.
			if (conf.motor_type == MOTOR_TYPE_DC) {
				if (fabsf(dutycycle_now) > 0.1) {
					state = MC_STATE_RUNNING;
				} else {
					full_brake_ll();
				}
			} else {
				if (fabsf(rpm_now) > conf.l_max_erpm_fbrake) {
					state = MC_STATE_RUNNING;
				} else {
					full_brake_ll();
				}
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
	if (dutyCycle >= conf.l_min_duty) {
		direction = 1;
	} else if (dutyCycle <= -conf.l_min_duty) {
		dutyCycle = -dutyCycle;
		direction = 0;
	}

	if (dutyCycle < conf.l_min_duty) {
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
	} else if (dutyCycle > conf.l_max_duty) {
		dutyCycle = conf.l_max_duty;
	}

	set_duty_cycle_hw(dutyCycle);

	if (conf.motor_type == MOTOR_TYPE_DC) {
		state = MC_STATE_RUNNING;
		set_next_comm_step(comm_step);
		commutate(1);
	} else {
		if (sensorless_now) {
			if (state != MC_STATE_RUNNING) {
				if (state == MC_STATE_OFF) {
					state = MC_STATE_RUNNING;

					if (fabsf(rpm_now) < conf.sl_min_erpm) {
						commutate(1);
					}
				} else if (state == MC_STATE_FULL_BRAKE) {
					if (fabsf(rpm_now) < conf.sl_min_erpm && mcpwm_get_tot_current_filtered() < conf.sl_max_fullbreak_current_dir_change) {
						state = MC_STATE_RUNNING;
						commutate(1);
					}
				}
			}
		} else {
			if (state != MC_STATE_RUNNING) {
				state = MC_STATE_RUNNING;
				comm_step = mcpwm_read_hall_phase();
				set_next_comm_step(comm_step);
				commutate(1);
			}
		}
	}
}

/**
 * Lowest level (hardware) duty cycle setter. Will set the hardware timer to
 * the specified duty cycle and update the ADC sampling positions.
 *
 * @param dutyCycle
 * The duty cycle in the range [MCPWM_MIN_DUTY_CYCLE  MCPWM_MAX_DUTY_CYCLE]
 * (Only positive)
 */
static void set_duty_cycle_hw(float dutyCycle) {
	mc_timer_struct timer_tmp;

	utils_sys_lock_cnt();
	timer_tmp = timer_struct;
	utils_sys_unlock_cnt();

	utils_truncate_number(&dutyCycle, conf.l_min_duty, conf.l_max_duty);

	if (conf.motor_type == MOTOR_TYPE_BLDC && conf.pwm_mode == PWM_MODE_BIPOLAR && !IS_DETECTING()) {
		timer_tmp.duty = (uint16_t) (((float) timer_tmp.top / 2.0) * dutyCycle
				+ ((float) timer_tmp.top / 2.0));
	} else {
		timer_tmp.duty = (uint16_t)((float)timer_tmp.top * dutyCycle);
	}

	if (conf.motor_type == MOTOR_TYPE_DC) {
		switching_frequency_now = MCPWM_SWITCH_FREQUENCY_DC_MOTOR;
	} else {
		if (IS_DETECTING() || conf.pwm_mode == PWM_MODE_BIPOLAR) {
			switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MAX;
		} else {
			switching_frequency_now = (float)MCPWM_SWITCH_FREQUENCY_MIN * (1.0 - fabsf(dutyCycle)) +
					(float)MCPWM_SWITCH_FREQUENCY_MAX * fabsf(dutyCycle);
		}
	}

	timer_tmp.top = SYSTEM_CORE_CLOCK / (int)switching_frequency_now;
	update_adc_sample_pos(&timer_tmp);
	set_next_timer_settings(&timer_tmp);
}

static void run_pid_control_speed(void) {
	static float i_term = 0;
	static float prev_error = 0;
	float p_term;
	float d_term;

	// PID is off. Return.
	if (control_mode != CONTROL_MODE_SPEED) {
		i_term = dutycycle_now;
		prev_error = 0;
		return;
	}

	// Too low RPM set. Stop and return.
	if (fabsf(speed_pid_set_rpm) < conf.s_pid_min_rpm) {
		i_term = dutycycle_now;
		prev_error = 0;
		mcpwm_set_duty(0.0);
		return;
	}

	// Compensation for supply voltage variations
	float scale = 1.0 / GET_INPUT_VOLTAGE();

	// Compute error
	float error = speed_pid_set_rpm - mcpwm_get_rpm();

	// Compute parameters
	p_term = error * conf.s_pid_kp * scale;
	i_term += error * (conf.s_pid_ki * MCPWM_PID_TIME_K) * scale;
	d_term = (error - prev_error) * (conf.s_pid_kd / MCPWM_PID_TIME_K) * scale;

	// I-term wind-up protection
	utils_truncate_number(&i_term, -1.0, 1.0);

	// Store previous error
	prev_error = error;

	// Calculate output
	float output = p_term + i_term + d_term;

	// Make sure that at least minimum output is used
	if (fabsf(output) < conf.l_min_duty) {
		output = SIGN(output) * conf.l_min_duty;
	}

	// Do not output in reverse direction to oppose too high rpm
	if (speed_pid_set_rpm > 0.0 && output < 0.0) {
		output = conf.l_min_duty;
		i_term = 0.0;
	} else if (speed_pid_set_rpm < 0.0 && output > 0.0) {
		output = -conf.l_min_duty;
		i_term = 0.0;
	}

	set_duty_cycle_hl(output);
}

static void run_pid_control_pos(float dt) {
	static float i_term = 0;
	static float prev_error = 0;
	float p_term;
	float d_term;

	// PID is off. Return.
	if (control_mode != CONTROL_MODE_POS) {
		i_term = 0;
		prev_error = 0;
		return;
	}

	// Compute error
	float error = utils_angle_difference(encoder_read_deg(), pos_pid_set_pos);

	// Compute parameters
	p_term = error * conf.p_pid_kp;
	i_term += error * (conf.p_pid_ki * dt);
	d_term = (error - prev_error) * (conf.p_pid_kd / dt);

	// I-term wind-up protection
	utils_truncate_number(&i_term, -1.0, 1.0);

	// Store previous error
	prev_error = error;

	// Calculate output
	float output = p_term + i_term + d_term;
	utils_truncate_number(&output, -1.0, 1.0);

	current_set = output * conf.lo_current_max;
}

static msg_t rpm_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("rpm timer");

	for (;;) {
		if (rpm_dep.comms != 0) {
			utils_sys_lock_cnt();
			const float comms = (float)rpm_dep.comms;
			const float time_at_comm = (float)rpm_dep.time_at_comm;
			rpm_dep.comms = 0;
			rpm_dep.time_at_comm = 0;
			utils_sys_unlock_cnt();

			rpm_now = (comms * MCPWM_RPM_TIMER_FREQ * 60.0) / (time_at_comm * 6.0);
		} else {
			// In case we have slowed down
			float rpm_tmp = (MCPWM_RPM_TIMER_FREQ * 60.0) / ((float) TIM2 ->CNT * 6.0);

			if (fabsf(rpm_tmp) < fabsf(rpm_now)) {
				rpm_now = rpm_tmp;
			}
		}

		// Some low-pass filtering
		static float rpm_p1 = 0.0;
		rpm_now = (rpm_now + rpm_p1) / 2;
		rpm_p1 = rpm_now;
		const float rpm_abs = fabsf(rpm_now);

		// Update the cycle integrator limit
		rpm_dep.cycle_int_limit = conf.sl_cycle_int_limit;
		rpm_dep.cycle_int_limit_running = rpm_dep.cycle_int_limit + (float)ADC_Value[ADC_IND_VIN_SENS] *
				conf.sl_bemf_coupling_k / (rpm_abs > conf.sl_min_erpm ? rpm_abs : conf.sl_min_erpm);
		rpm_dep.cycle_int_limit_running = utils_map(rpm_abs, 0,
				conf.sl_cycle_int_rpm_br, rpm_dep.cycle_int_limit_running,
				rpm_dep.cycle_int_limit_running * conf.sl_phase_advance_at_br);
		rpm_dep.cycle_int_limit_max = rpm_dep.cycle_int_limit + (float)ADC_Value[ADC_IND_VIN_SENS] *
				conf.sl_bemf_coupling_k / conf.sl_min_erpm_cycle_int_limit;

		if (rpm_dep.cycle_int_limit_running < 1.0) {
			rpm_dep.cycle_int_limit_running = 1.0;
		}

		if (rpm_dep.cycle_int_limit_running > rpm_dep.cycle_int_limit_max) {
			rpm_dep.cycle_int_limit_running = rpm_dep.cycle_int_limit_max;
		}

		rpm_dep.comm_time_sum = ((float) MCPWM_SWITCH_FREQUENCY_MAX) / ((rpm_abs / 60.0) * 6.0);
		rpm_dep.comm_time_sum_min_rpm = ((float) MCPWM_SWITCH_FREQUENCY_MAX) / ((conf.sl_min_erpm / 60.0) * 6.0);

		run_pid_control_speed();

		chThdSleepMilliseconds(1);
	}

	return 0;
}

static msg_t timer_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("mcpwm timer");

	float amp;
	float min_s;
	float max_s;

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
			if (conf.motor_type == MOTOR_TYPE_DC) {
				if (amp > 0) {
					direction = 0;
				} else {
					direction = 1;
					amp = -amp;
				}
			} else {
				if (sensorless_now) {
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
				} else {
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
				}
			}

			if (direction == 1) {
				dutycycle_now = amp / (float)ADC_Value[ADC_IND_VIN_SENS];
			} else {
				dutycycle_now = -amp / (float)ADC_Value[ADC_IND_VIN_SENS];
			}
			utils_truncate_number((float*)&dutycycle_now, -conf.l_max_duty, conf.l_max_duty);
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

		// Fill KV filter vector at 100Hz
		static int cnt_tmp = 0;
		cnt_tmp++;
		if (cnt_tmp >= 10) {
			cnt_tmp = 0;
			if (state == MC_STATE_RUNNING) {
				filter_add_sample((float*)kv_fir_samples, mcpwm_get_kv(),
						KV_FIR_TAPS_BITS, (uint32_t*)&kv_fir_index);
			} else if (state == MC_STATE_OFF) {
				if (dutycycle_now >= conf.l_min_duty) {
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

		update_override_limits(&conf);

		chThdSleepMilliseconds(1);
	}

	return 0;
}

void mcpwm_adc_inj_int_handler(void) {
	TIM12->CNT = 0;

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

	if (conf.motor_type == MOTOR_TYPE_DC) {
		if (direction) {
			curr_tot_sample = -(float)(ADC_Value[ADC_IND_CURR2] - curr1_offset);
		} else {
			curr_tot_sample = -(float)(ADC_Value[ADC_IND_CURR1] - curr0_offset);
		}
	} else {
		static int detect_now = 0;

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

		if (state == MC_STATE_FULL_BRAKE) {
			float c0 = (float)ADC_curr_norm_value[0];
			float c1 = (float)ADC_curr_norm_value[1];
			float c2 = (float)ADC_curr_norm_value[2];
			curr_tot_sample = sqrtf((c0*c0 + c1*c1 + c2*c2) / 1.5);
		} else {
			switch (comm_step) {
			case 1:
			case 6:
				if (direction) {
					if (comm_step == 1) {
						curr_tot_sample = -(float)ADC_curr_norm_value[1];
					} else {
						curr_tot_sample = -(float)ADC_curr_norm_value[0];
					}
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
					if (comm_step == 4) {
						curr_tot_sample = -(float)ADC_curr_norm_value[1];
					} else {
						curr_tot_sample = -(float)ADC_curr_norm_value[0];
					}
				}
				break;
			}
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

			int vzero = ADC_V_ZERO;
//			int vzero = (ADC_V_L1 + ADC_V_L2 + ADC_V_L3) / 3;

			switch (comm_step) {
			case 1:
			case 4:
				mcpwm_detect_voltages[detect_step] = ADC_V_L1 - vzero;
				break;

			case 2:
			case 5:
				mcpwm_detect_voltages[detect_step] = ADC_V_L2 - vzero;
				break;

			case 3:
			case 6:
				mcpwm_detect_voltages[detect_step] = ADC_V_L3 - vzero;
				break;

			default:
				break;
			}

			mcpwm_detect_currents_avg[detect_step] += mcpwm_detect_currents[detect_step];
			mcpwm_detect_avg_samples[detect_step]++;

			stop_pwm_hw();
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

			set_next_comm_step(comm_step);
			TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
		}
	}

	last_current_sample = curr_tot_sample;
	filter_add_sample((float*) current_fir_samples, curr_tot_sample,
			CURR_FIR_TAPS_BITS, (uint32_t*) &current_fir_index);
	last_current_sample_filtered = filter_run_fir_iteration(
			(float*) current_fir_samples, (float*) current_fir_coeffs,
			CURR_FIR_TAPS_BITS, current_fir_index);

	last_inj_adc_isr_duration = (float) TIM12->CNT / 10000000.0;
}

/*
 * New ADC samples ready. Do commutation!
 */
void mcpwm_adc_int_handler(void *p, uint32_t flags) {
	(void)p;
	(void)flags;

	TIM12->CNT = 0;

	// Set the next timer settings if an update is far enough away
	update_timer_attempt();

	// Reset the watchdog
	WWDG_SetCounter(100);

	const float input_voltage = GET_INPUT_VOLTAGE();
	int ph1, ph2, ph3;
	int ph1_raw, ph2_raw, ph3_raw;

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
	if (input_voltage < conf.l_min_vin ||
			input_voltage > conf.l_max_vin) {
		wrong_voltage_iterations++;

		if ((wrong_voltage_iterations >= 8)) {
			fault_stop(input_voltage < conf.l_min_vin ?
					FAULT_CODE_UNDER_VOLTAGE : FAULT_CODE_OVER_VOLTAGE);
		}
	} else {
		wrong_voltage_iterations = 0;
	}

	if (conf.motor_type == MOTOR_TYPE_BLDC) {

		/*
		 * Calculate the virtual ground, depending on the state.
		 */
		if (has_commutated && fabsf(dutycycle_now) > 0.2) {
			mcpwm_vzero = ADC_V_ZERO;
		} else {
			mcpwm_vzero = (ADC_V_L1 + ADC_V_L2 + ADC_V_L3) / 3;
		}

		if (direction) {
			ph1 = ADC_V_L1 - mcpwm_vzero;
			ph2 = ADC_V_L2 - mcpwm_vzero;
			ph3 = ADC_V_L3 - mcpwm_vzero;
			ph1_raw = ADC_V_L1;
			ph2_raw = ADC_V_L2;
			ph3_raw = ADC_V_L3;
		} else {
			ph1 = ADC_V_L1 - mcpwm_vzero;
			ph2 = ADC_V_L3 - mcpwm_vzero;
			ph3 = ADC_V_L2 - mcpwm_vzero;
			ph1_raw = ADC_V_L1;
			ph2_raw = ADC_V_L3;
			ph3_raw = ADC_V_L2;
		}

		update_timer_attempt();

		float amp = 0.0;

		if (has_commutated) {
			amp = fabsf(dutycycle_now) * (float)ADC_Value[ADC_IND_VIN_SENS];
		} else {
			amp = sqrtf((float)(ph1*ph1 + ph2*ph2 + ph3*ph3)) * sqrtf(2.0);
		}

		// Fill the amplitude FIR filter
		filter_add_sample((float*)amp_fir_samples, amp,
				AMP_FIR_TAPS_BITS, (uint32_t*)&amp_fir_index);

		if (sensorless_now) {
			static float cycle_integrator = 0;

			if (pwm_cycles_sum >= rpm_dep.comm_time_sum_min_rpm) {
				if (state == MC_STATE_RUNNING) {
					if (conf.comm_mode == COMM_MODE_INTEGRATE) {
						// This means that the motor is stuck. If this commutation does not
						// produce any torque because of misalignment at start, two
						// commutations ahead should produce full torque.
						commutate(2);
					} else if (conf.comm_mode == COMM_MODE_DELAY) {
						commutate(1);
					}

					cycle_integrator = 0.0;
				}
			}

			if ((state == MC_STATE_RUNNING && pwm_cycles_sum >= 2.0) || state == MC_STATE_OFF) {
				int v_diff = 0;
				int ph_now_raw = 0;

				switch (comm_step) {
				case 1:
					v_diff = ph1;
					ph_now_raw = ph1_raw;
					break;
				case 2:
					v_diff = -ph2;
					ph_now_raw = ph2_raw;
					break;
				case 3:
					v_diff = ph3;
					ph_now_raw = ph3_raw;
					break;
				case 4:
					v_diff = -ph1;
					ph_now_raw = ph1_raw;
					break;
				case 5:
					v_diff = ph2;
					ph_now_raw = ph2_raw;
					break;
				case 6:
					v_diff = -ph3;
					ph_now_raw = ph3_raw;
					break;
				default:
					break;
				}

				// Collect hall sensor samples in the first half of the commutation cycle. This is
				// because positive timing is much better than negative timing in case they are
				// mis-aligned.
				if (v_diff < 50) {
					hall_detect_table[read_hall()][comm_step]++;
				}

				// Don't commutate while the motor is standing still and the signal only consists
				// of weak noise.
				if (abs(v_diff) < 10) {
					v_diff = 0;
				}

				if (v_diff > 0) {
					if (pwm_cycles_sum > (last_pwm_cycles_sum / 2.0) ||
							!has_commutated || (ph_now_raw > 100 && ph_now_raw < (ADC_Value[ADC_IND_VIN_SENS] - 100))) {
						cycle_integrator += (float)v_diff / switching_frequency_now;
					}
				}

				if (conf.comm_mode == COMM_MODE_INTEGRATE) {
					float limit;
					if (has_commutated) {
						limit = rpm_dep.cycle_int_limit_running * (0.0005 * VDIV_CORR);
					} else {
						limit = rpm_dep.cycle_int_limit * (0.0005 * VDIV_CORR);
					}

					if (cycle_integrator >= (rpm_dep.cycle_int_limit_max * (0.0005 * VDIV_CORR)) ||
							cycle_integrator >= limit) {
						commutate(1);
						cycle_integrator = 0.0;
					}
				} else if (conf.comm_mode == COMM_MODE_DELAY) {
					static float cycle_sum = 0.0;
					if (v_diff > 0) {
						cycle_sum += (float)MCPWM_SWITCH_FREQUENCY_MAX / switching_frequency_now;

						if (cycle_sum >= utils_map(fabsf(rpm_now), 0,
								conf.sl_cycle_int_rpm_br, rpm_dep.comm_time_sum / 2.0,
								(rpm_dep.comm_time_sum / 2.0) * conf.sl_phase_advance_at_br)) {
							commutate(1);
							cycle_integrator_sum += cycle_integrator * (1.0 / (0.0005 * VDIV_CORR));
							cycle_integrator_iterations += 1.0;
							cycle_integrator = 0.0;
							cycle_sum = 0.0;
						}
					} else {
						cycle_integrator = 0.0;
						cycle_sum = 0.0;
					}
				}
			} else {
				cycle_integrator = 0.0;
			}

			pwm_cycles_sum += (float)MCPWM_SWITCH_FREQUENCY_MAX / switching_frequency_now;
		} else {
			int hall_phase = mcpwm_read_hall_phase();
			if (comm_step != hall_phase) {
				comm_step = hall_phase;

				update_rpm_tacho();

				if (state == MC_STATE_RUNNING) {
					set_next_comm_step(comm_step);
					commutate(0);
				}
			} else if (state == MC_STATE_RUNNING && !has_commutated) {
				set_next_comm_step(comm_step);
				commutate(0);
			}
		}
	} else {
		float amp = 0.0;

		if (has_commutated) {
			amp = dutycycle_now * (float)ADC_Value[ADC_IND_VIN_SENS];
		} else {
			amp = ADC_V_L3 - ADC_V_L1;
		}

		// Fill the amplitude FIR filter
		filter_add_sample((float*)amp_fir_samples, amp,
				AMP_FIR_TAPS_BITS, (uint32_t*)&amp_fir_index);

		if (state == MC_STATE_RUNNING && !has_commutated) {
			set_next_comm_step(comm_step);
			commutate(0);
		}
	}

	const float current = mcpwm_get_tot_current_filtered();
	const float current_in = current * fabsf(dutycycle_now);
	const float current_nofilter = mcpwm_get_tot_current();
	const float current_in_nofilter = current_nofilter * fabsf(dutycycle_now);
	motor_current_sum += current;
	input_current_sum += current_in;
	motor_current_iterations++;
	input_current_iterations++;

	if (conf.l_slow_abs_current) {
		if (fabsf(current) > conf.l_abs_current_max) {
			fault_stop(FAULT_CODE_ABS_OVER_CURRENT);
		}
	} else {
		if (fabsf(current_nofilter) > conf.l_abs_current_max) {
			fault_stop(FAULT_CODE_ABS_OVER_CURRENT);
		}
	}

	if (fabsf(current) > 1.0) {
		// Some extra filtering
		static float curr_diff_sum = 0.0;
		static float curr_diff_samples = 0;

		curr_diff_sum += current_in / switching_frequency_now;
		curr_diff_samples += 1.0 / switching_frequency_now;

		if (curr_diff_samples >= 0.01) {
			if (curr_diff_sum > 0.0) {
				amp_seconds += curr_diff_sum;
				watt_seconds += curr_diff_sum * input_voltage;
			} else {
				amp_seconds_charged -= curr_diff_sum;
				watt_seconds_charged -= curr_diff_sum * input_voltage;
			}

			curr_diff_samples = 0.0;
			curr_diff_sum = 0.0;
		}
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

		if (control_mode == CONTROL_MODE_CURRENT || control_mode == CONTROL_MODE_POS) {
			// Compute error
			const float error = current_set - (direction ? current_nofilter : -current_nofilter);
			float step = error * conf.cc_gain * voltage_scale;
			const float start_boost = conf.cc_startup_boost_duty * voltage_scale;

			// Do not ramp too much
			utils_truncate_number(&step, -conf.cc_ramp_step_max, conf.cc_ramp_step_max);

			// Switching frequency correction
			step /= switching_frequency_now / 1000.0;

			if (slow_ramping_cycles) {
				slow_ramping_cycles--;
				step *= 0.1;
			}

			// Optionally apply startup boost.
			if (fabsf(dutycycle_now_tmp) < start_boost) {
				utils_step_towards(&dutycycle_now_tmp,
						current_set > 0.0 ?
								start_boost :
								-start_boost, ramp_step);
			} else {
				dutycycle_now_tmp += step;
			}

			// Upper truncation
			utils_truncate_number((float*)&dutycycle_now_tmp, -conf.l_max_duty, conf.l_max_duty);

			// Lower truncation
			if (fabsf(dutycycle_now_tmp) < conf.l_min_duty) {
				if (dutycycle_now_tmp < 0.0 && current_set > 0.0) {
					dutycycle_now_tmp = conf.l_min_duty;
				} else if (dutycycle_now_tmp > 0.0 && current_set < 0.0) {
					dutycycle_now_tmp = -conf.l_min_duty;
				}
			}

			// The set dutycycle should be in the correct direction in case the output is lower
			// than the minimum duty cycle and the mechanism below gets activated.
			dutycycle_set = dutycycle_now_tmp >= 0.0 ? conf.l_min_duty : -conf.l_min_duty;
		} else if (control_mode == CONTROL_MODE_CURRENT_BRAKE) {
			// Compute error
			const float error = -fabsf(current_set) - current_nofilter;
			float step = error * conf.cc_gain * voltage_scale;

			// Do not ramp too much
			utils_truncate_number(&step, -conf.cc_ramp_step_max, conf.cc_ramp_step_max);

			// Switching frequency correction
			step /= switching_frequency_now / 1000.0;

			if (slow_ramping_cycles) {
				slow_ramping_cycles--;
				step *= 0.1;
			}

			dutycycle_now_tmp += SIGN(dutycycle_now_tmp) * step;

			// Upper truncation
			utils_truncate_number((float*)&dutycycle_now_tmp, -conf.l_max_duty, conf.l_max_duty);

			// Lower truncation
			if (fabsf(dutycycle_now_tmp) < conf.l_min_duty) {
				if (fabsf(rpm_now) < conf.l_max_erpm_fbrake_cc) {
					dutycycle_now_tmp = 0.0;
					dutycycle_set = dutycycle_now_tmp;
				} else {
					dutycycle_now_tmp = SIGN(dutycycle_now_tmp) * conf.l_min_duty;
					dutycycle_set = dutycycle_now_tmp;
				}
			}
		} else {
			utils_step_towards((float*)&dutycycle_now_tmp, dutycycle_set, ramp_step);
		}

		static int limit_delay = 0;

		// Apply limits in priority order
		if (current_nofilter > conf.lo_current_max) {
			utils_step_towards((float*) &dutycycle_now, 0.0,
					ramp_step_no_lim * fabsf(current_nofilter - conf.lo_current_max) * MCPWM_CURRENT_LIMIT_GAIN);
			limit_delay = 1;
		} else if (current_nofilter < conf.lo_current_min) {
			utils_step_towards((float*) &dutycycle_now, direction ? conf.l_max_duty : -conf.l_max_duty,
					ramp_step_no_lim * fabsf(current_nofilter - conf.lo_current_min) * MCPWM_CURRENT_LIMIT_GAIN);
			limit_delay = 1;
		} else if (current_in_nofilter > conf.lo_in_current_max) {
			utils_step_towards((float*) &dutycycle_now, 0.0,
					ramp_step_no_lim * fabsf(current_in_nofilter - conf.lo_in_current_max) * MCPWM_CURRENT_LIMIT_GAIN);
			limit_delay = 1;
		} else if (current_in_nofilter < conf.lo_in_current_min) {
			utils_step_towards((float*) &dutycycle_now, direction ? conf.l_max_duty : -conf.l_max_duty,
					ramp_step_no_lim * fabsf(current_in_nofilter - conf.lo_in_current_min) * MCPWM_CURRENT_LIMIT_GAIN);
			limit_delay = 1;
		} else if (rpm > conf.l_max_erpm) {
			if ((conf.l_rpm_lim_neg_torque || current > -1.0) && dutycycle_now <= dutycycle_now_tmp) {
				utils_step_towards((float*) &dutycycle_now, 0.0, MCPWM_RAMP_STEP_RPM_LIMIT);
				limit_delay = 1;
				slow_ramping_cycles = 500;
			}
		} else if (rpm < conf.l_min_erpm) {
			if ((conf.l_rpm_lim_neg_torque || current > -1.0) && dutycycle_now >= dutycycle_now_tmp) {
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
		if (fabsf(dutycycle_now) < conf.l_min_duty) {
			if (dutycycle_set >= conf.l_min_duty) {
				dutycycle_now = conf.l_min_duty;
			} else if (dutycycle_set <= -conf.l_min_duty) {
				dutycycle_now = -conf.l_min_duty;
			}
		}

		// Don't start in the opposite direction when the RPM is too high even if the current is low enough.
		if (dutycycle_now >= conf.l_min_duty && rpm < -conf.l_max_erpm_fbrake) {
			dutycycle_now = -conf.l_min_duty;
		} else if (dutycycle_now <= -conf.l_min_duty && rpm > conf.l_max_erpm_fbrake) {
			dutycycle_now = conf.l_min_duty;
		}

		set_duty_cycle_ll(dutycycle_now);
	}

	main_dma_adc_handler();

	if (ENCODER_ENABLE) {
		run_pid_control_pos(1.0 / switching_frequency_now);
	}

	last_adc_isr_duration = (float)TIM12->CNT / 10000000.0;
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
		mcpwm_detect_avg_samples[i] = 0;
	}

	state = MC_STATE_DETECTING;
}

float mcpwm_get_detect_pos(void) {
	float v[6];
	v[0] = mcpwm_detect_currents_avg[0] / mcpwm_detect_avg_samples[0];
	v[1] = mcpwm_detect_currents_avg[1] / mcpwm_detect_avg_samples[1];
	v[2] = mcpwm_detect_currents_avg[2] / mcpwm_detect_avg_samples[2];
	v[3] = mcpwm_detect_currents_avg[3] / mcpwm_detect_avg_samples[3];
	v[4] = mcpwm_detect_currents_avg[4] / mcpwm_detect_avg_samples[4];
	v[5] = mcpwm_detect_currents_avg[5] / mcpwm_detect_avg_samples[5];

	for(int i = 0;i < 6;i++) {
		mcpwm_detect_currents_avg[i] = 0;
		mcpwm_detect_avg_samples[i] = 0;
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

float mcpwm_read_reset_avg_cycle_integrator(void) {
	float res = cycle_integrator_sum / cycle_integrator_iterations;
	cycle_integrator_sum = 0;
	cycle_integrator_iterations = 0;
	return res;
}

/**
 * Set the minimum allowed RPM in sensorless mode. This will affect startup
 * performance. WARNING: Setting this too high can break stuff.
 *
 * @param rpm
 * The minimum allowed RPM.
 */
void mcpwm_set_min_rpm(float rpm) {
	conf.sl_min_erpm = rpm;
}

/**
 * Get the minimum allowed RPM in sensorless mode.
 *
 * @return
 * The minimum allowed RPM.
 */
float mcpwm_get_min_rpm(void) {
	return conf.sl_min_erpm;
}

/**
 * Set the commutation mode for sensorless commutation.
 *
 * @param mode
 * COMM_MODE_INTEGRATE: More robust, but requires many parameters.
 * COMM_MODE_DELAY: Like most hobby ESCs. Requires less parameters,
 * but has worse startup and is less robust.
 *
 */
void mcpwm_set_comm_mode(mc_comm_mode mode) {
	conf.comm_mode = mode;
}

mc_comm_mode mcpwm_get_comm_mode(void) {
	return conf.comm_mode;
}

float mcpwm_get_last_adc_isr_duration(void) {
	return last_adc_isr_duration;
}

float mcpwm_get_last_inj_adc_isr_duration(void) {
	return last_inj_adc_isr_duration;
}

mc_rpm_dep_struct mcpwm_get_rpm_dep(void) {
	return rpm_dep;
}

/**
 * Reset the hall sensor detection table
 */
void mcpwm_reset_hall_detect_table(void) {
	memset(hall_detect_table, 0, sizeof(hall_detect_table[0][0]) * 8 * 7);
}

/**
 * Get the current detected hall sensor table
 *
 * @param table
 * Pointer to a table where the result should be stored
 *
 * @return
 * 0: OK
 * -1: Invalid hall sensor output
 * -2: WS2811 enabled
 * -3: Encoder enabled
 */
int mcpwm_get_hall_detect_result(int8_t *table) {
	if (WS2811_ENABLE) {
		return -2;
	} else if (ENCODER_ENABLE) {
		return -3;
	}

	for (int i = 0;i < 8;i++) {
		int samples = 0;
		int res = -1;
		for (int j = 1;j < 7;j++) {
			if (hall_detect_table[i][j] > samples) {
				samples = hall_detect_table[i][j];
				if (samples > 15) {
					res = j;
				}
			}
			table[i] = res;
		}
	}

	int invalid_samp_num = 0;
	int nums[7] = {0, 0, 0, 0, 0, 0, 0};
	int tot_nums = 0;
	for (int i = 0;i < 8;i++) {
		if (table[i] == -1) {
			invalid_samp_num++;
		} else {
			if (!nums[table[i]]) {
				nums[table[i]] = 1;
				tot_nums++;
			}
		}
	}

	if (invalid_samp_num == 2 && tot_nums == 6) {
		return 0;
	} else {
		return -1;
	}
}

/**
 * Read the current phase of the motor using hall effect sensors
 * @return
 * The phase read.
 */
int mcpwm_read_hall_phase(void) {
	return hall_to_phase_table[read_hall() + (direction ? 8 : 0)];
}

static int read_hall(void) {
	return READ_HALL1() | (READ_HALL2() << 1) | (READ_HALL3() << 2);
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

	curr_samp_volt = 0;

	if (conf.motor_type == MOTOR_TYPE_DC) {
		curr1_sample = top - 10; // Not used anyway
		curr2_sample = top - 10;

		if (duty > 1000) {
			val_sample = duty / 2;
		} else {
			val_sample = duty + 800;
			curr_samp_volt = 1;
		}

//		if (duty < (top / 2)) {
//			val_sample = (top - duty) / 2 + duty;
//		} else {
//			val_sample = duty / 2;
//		}
	} else {
		// Sample the ADC at an appropriate time during the pwm cycle
		if (IS_DETECTING()) {
			// Voltage samples
			val_sample = duty / 2;

			// Current samples
			curr1_sample = (top - duty) / 2 + duty;
			curr2_sample = (top - duty) / 2 + duty;
		} else {
			if (conf.pwm_mode == PWM_MODE_BIPOLAR) {
				uint32_t samp_neg = top - 2;
				uint32_t samp_pos = duty + (top - duty) / 2;
				uint32_t samp_zero = top - 2;

				// Voltage and other sampling
				val_sample = top / 4;

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
				curr1_sample = duty + (top - duty) / 2 + 1000;
				if (curr1_sample > (top - 20)) {
					curr1_sample = top - 20;
				}

				//			curr1_sample = duty + 1500;
				//			curr1_sample = duty + (top - duty) / 2;
				//			curr1_sample = duty + 2 * (top - duty) / 3;
				//			curr1_sample = top - 20;

				curr2_sample = curr1_sample;
			}
		}
	}

	timer_tmp->val_sample = val_sample;
	timer_tmp->curr1_sample = curr1_sample;
	timer_tmp->curr2_sample = curr2_sample;
}

static void update_rpm_tacho(void) {
	int step = comm_step - 1;
	static int last_step = 0;
	int tacho_diff = (step - last_step) % 6;
	last_step = step;

	if (tacho_diff > 3) {
		tacho_diff -= 6;
	} else if (tacho_diff < -2) {
		tacho_diff += 6;
	}

	if (tacho_diff != 0) {
		rpm_dep.comms += tacho_diff;
		rpm_dep.time_at_comm += TIM2->CNT;
		TIM2->CNT = 0;
	}

	// Tachometers
	tachometer_for_direction += tacho_diff;
	tachometer_abs += tacho_diff;

	if (direction) {
		tachometer += tacho_diff;
	} else {
		tachometer -= tacho_diff;
	}
}

static void update_sensor_mode(void) {
	if (conf.sensor_mode == SENSOR_MODE_SENSORLESS ||
			(conf.sensor_mode == SENSOR_MODE_HYBRID &&
					fabsf(mcpwm_get_rpm()) > conf.hall_sl_erpm)) {
		sensorless_now = true;
	} else {
		sensorless_now = false;
	}
}

static void commutate(int steps) {
	last_pwm_cycles_sum = pwm_cycles_sum;
	last_pwm_cycles_sums[comm_step - 1] = pwm_cycles_sum;
	pwm_cycles_sum = 0;

	if (conf.motor_type == MOTOR_TYPE_BLDC && sensorless_now) {
		comm_step += steps;
		while (comm_step > 6) {
			comm_step -= 6;
		}
		while (comm_step < 1) {
			comm_step += 6;
		}

		update_rpm_tacho();

		if (!(state == MC_STATE_RUNNING)) {
			update_sensor_mode();
			return;
		}

		set_next_comm_step(comm_step);
	}

	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
	has_commutated = 1;

	mc_timer_struct timer_tmp;

	utils_sys_lock_cnt();
	timer_tmp = timer_struct;
	utils_sys_unlock_cnt();

	update_adc_sample_pos(&timer_tmp);
	set_next_timer_settings(&timer_tmp);
	update_sensor_mode();
}

static void set_next_timer_settings(mc_timer_struct *settings) {
	utils_sys_lock_cnt();
	timer_struct = *settings;
	timer_struct.updated = false;
	utils_sys_unlock_cnt();

	update_timer_attempt();
}

/**
 * Try to apply the new timer settings. This is really not an elegant solution, but for now it is
 * the best I can come up with.
 */
static void update_timer_attempt(void) {
	utils_sys_lock_cnt();

	// Set the next timer settings if an update is far enough away
	if (!timer_struct.updated && TIM1->CNT > 10 && TIM1->CNT < (TIM1->ARR - 500)) {
		// Disable preload register updates
		TIM1->CR1 |= TIM_CR1_UDIS;
		TIM8->CR1 |= TIM_CR1_UDIS;

		// Set the new configuration
		TIM1->ARR = timer_struct.top;
		TIM1->CCR1 = timer_struct.duty;
		TIM1->CCR2 = timer_struct.duty;
		TIM1->CCR3 = timer_struct.duty;
		TIM8->CCR1 = timer_struct.val_sample;
		TIM1->CCR4 = timer_struct.curr1_sample;
		TIM8->CCR2 = timer_struct.curr2_sample;

		// Enables preload register updates
		TIM1->CR1 &= ~TIM_CR1_UDIS;
		TIM8->CR1 &= ~TIM_CR1_UDIS;
		timer_struct.updated = true;
	}

	utils_sys_unlock_cnt();
}

static void set_switching_frequency(float frequency) {
	switching_frequency_now = frequency;
	mc_timer_struct timer_tmp;

	utils_sys_lock_cnt();
	timer_tmp = timer_struct;
	utils_sys_unlock_cnt();

	timer_tmp.top = SYSTEM_CORE_CLOCK / (int)switching_frequency_now;
	update_adc_sample_pos(&timer_tmp);
	set_next_timer_settings(&timer_tmp);
}

static void set_next_comm_step(int next_step) {
	if (conf.motor_type == MOTOR_TYPE_DC) {
		// 0
		TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
		TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Disable);

		if (direction) {
			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_PWM1);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Enable);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Enable);
		} else {
			// +
			TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_PWM1);
			TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Enable);

			// -
			TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_Inactive);
			TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
			TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Enable);
		}

		return;
	}

	uint16_t positive_oc_mode = TIM_OCMode_PWM1;
	uint16_t negative_oc_mode = TIM_OCMode_Inactive;

	uint16_t positive_highside = TIM_CCx_Enable;
	uint16_t positive_lowside = TIM_CCxN_Enable;

	uint16_t negative_highside = TIM_CCx_Enable;
	uint16_t negative_lowside = TIM_CCxN_Enable;

	if (!IS_DETECTING()) {
		switch (conf.pwm_mode) {
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
