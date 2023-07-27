/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "mcpwm.h"
#include "mc_interface.h"
#include "digital_filter.h"
#include "utils_math.h"
#include "utils_sys.h"
#include "ledpwm.h"
#include "terminal.h"
#include "timeout.h"
#include "encoder/encoder.h"
#include "timer.h"

// Structs
typedef struct {
	volatile bool updated;
	volatile unsigned int top;
	volatile unsigned int duty;
	volatile unsigned int val_sample;
	volatile unsigned int curr1_sample;
	volatile unsigned int curr2_sample;
#ifdef HW_HAS_3_SHUNTS
	volatile unsigned int curr3_sample;
#endif
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
static volatile mc_control_mode control_mode;
static volatile float last_current_sample;
static volatile float last_current_sample_filtered;
static volatile float mcpwm_detect_currents_avg[6];
static volatile float mcpwm_detect_avg_samples[6];
static volatile float switching_frequency_now;
static volatile int ignore_iterations;
static volatile mc_timer_struct timer_struct;
static volatile int curr_samp_volt; // Use the voltage-synchronized samples for this current sample
static int hall_to_phase_table[16];
static volatile unsigned int slow_ramping_cycles;
static volatile int has_commutated;
static volatile mc_rpm_dep_struct rpm_dep;
static volatile float cycle_integrator_sum;
static volatile float cycle_integrator_iterations;
static volatile mc_configuration *conf;
static volatile float pwm_cycles_sum;
static volatile int pwm_cycles;
static volatile float last_pwm_cycles_sum;
static volatile float last_pwm_cycles_sums[6];
static volatile bool dccal_done;
static volatile bool sensorless_now;
static volatile int hall_detect_table[8][7];
static volatile bool init_done = false;
static volatile mc_comm_mode comm_mode_next;
static volatile float m_pll_phase;
static volatile float m_pll_speed;
static volatile uint32_t rpm_timer_start;

#ifdef HW_HAS_3_SHUNTS
static volatile int curr2_sum;
static volatile int curr2_offset;
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
static void run_pid_control_speed(void);
static void run_pid_control_pos(float dt, float pos_now);
static void set_next_comm_step(int next_step);
static void update_rpm_tacho(void);
static void update_sensor_mode(void);
static int read_hall(void);
static void update_adc_sample_pos(mc_timer_struct *timer_tmp);
static void commutate(int steps);
static void set_next_timer_settings(mc_timer_struct *settings);
static void update_timer_attempt(void);
static void set_switching_frequency(float frequency);
static void do_dc_cal(void);
static void pll_run(float phase, float dt, volatile float *phase_var,
		volatile float *speed_var);

// Defines
#define IS_DETECTING()			(state == MC_STATE_DETECTING)

// Threads
static THD_WORKING_AREA(timer_thread_wa, 512);
static THD_FUNCTION(timer_thread, arg);
static THD_WORKING_AREA(rpm_thread_wa, 512);
static THD_FUNCTION(rpm_thread, arg);
static volatile bool timer_thd_stop;
static volatile bool rpm_thd_stop;

void mcpwm_init(volatile mc_configuration *configuration) {
	utils_sys_lock_cnt();

	init_done= false;

	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	TIM_BDTRInitTypeDef TIM_BDTRInitStructure;

	conf = configuration;

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
	control_mode = CONTROL_MODE_NONE;
	last_current_sample = 0.0;
	last_current_sample_filtered = 0.0;
	switching_frequency_now = conf->m_bldc_f_sw_max;
	ignore_iterations = 0;
	curr_samp_volt = 0;
	slow_ramping_cycles = 0;
	has_commutated = 0;
	memset((void*)&rpm_dep, 0, sizeof(rpm_dep));
	cycle_integrator_sum = 0.0;
	cycle_integrator_iterations = 0.0;
	pwm_cycles_sum = 0.0;
	pwm_cycles = 0;
	last_pwm_cycles_sum = 0.0;
	memset((float*)last_pwm_cycles_sums, 0, sizeof(last_pwm_cycles_sums));
	dccal_done = false;
	memset((void*)hall_detect_table, 0, sizeof(hall_detect_table[0][0]) * 8 * 7);
	update_sensor_mode();
	comm_mode_next = conf->comm_mode;
	m_pll_phase = 0.0;
	m_pll_speed = 0.0;
	rpm_timer_start = 0;

	mcpwm_init_hall_table((int8_t*)conf->hall_table);

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

	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, ENABLE);

	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = SYSTEM_CORE_CLOCK / (int)switching_frequency_now;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;

	TIM_TimeBaseInit(TIM1, &TIM_TimeBaseStructure);

	// Channel 1, 2 and 3 Configuration in PWM mode
	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_OutputNState = TIM_OutputNState_Enable;
	TIM_OCInitStructure.TIM_Pulse = TIM1->ARR / 2;

#ifndef INVERTED_TOP_DRIVER_INPUT
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High; // gpio high = top fets on
#else
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_Low;
#endif
	TIM_OCInitStructure.TIM_OCIdleState = TIM_OCIdleState_Set;

#ifndef INVERTED_BOTTOM_DRIVER_INPUT
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_High;  // gpio high = bottom fets on
#else
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_Low;
#endif
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
	TIM_BDTRInitStructure.TIM_DeadTime = conf_general_calculate_deadtime(HW_DEAD_TIME_NSEC, SYSTEM_CORE_CLOCK);
	TIM_BDTRInitStructure.TIM_Break = TIM_Break_Disable;
	TIM_BDTRInitStructure.TIM_BreakPolarity = TIM_BreakPolarity_High;
	TIM_BDTRInitStructure.TIM_AutomaticOutput = TIM_AutomaticOutput_Disable;

	TIM_BDTRConfig(TIM1, &TIM_BDTRInitStructure);
	TIM_CCPreloadControl(TIM1, ENABLE);
	TIM_ARRPreloadConfig(TIM1, ENABLE);

	ADC_CommonInitTypeDef ADC_CommonInitStructure;
	DMA_InitTypeDef DMA_InitStructure;
	ADC_InitTypeDef ADC_InitStructure;

	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_DMA2 | RCC_AHB1Periph_GPIOA | RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_ADC1 | RCC_APB2Periph_ADC2 | RCC_APB2Periph_ADC3, ENABLE);

	dmaStreamAllocate(STM32_DMA_STREAM(STM32_DMA_STREAM_ID(2, 4)),
			5,
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

	DMA_Cmd(DMA2_Stream4, ENABLE);

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

	ADC_TempSensorVrefintCmd(ENABLE);

	// Enable DMA request after last transfer (Multi-ADC mode)
	ADC_MultiModeDMARequestAfterLastTransferCmd(ENABLE);

	// Injected channels for current measurement at end of cycle
	ADC_ExternalTrigInjectedConvConfig(ADC1, ADC_ExternalTrigInjecConv_T1_CC4);
	ADC_ExternalTrigInjectedConvConfig(ADC2, ADC_ExternalTrigInjecConv_T8_CC2);
#ifdef HW_HAS_3_SHUNTS
	ADC_ExternalTrigInjectedConvConfig(ADC3, ADC_ExternalTrigInjecConv_T8_CC3);
#endif
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC1, ADC_ExternalTrigInjecConvEdge_Falling);
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC2, ADC_ExternalTrigInjecConvEdge_Falling);
#ifdef HW_HAS_3_SHUNTS
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC3, ADC_ExternalTrigInjecConvEdge_Falling);
#endif
	ADC_InjectedSequencerLengthConfig(ADC1, HW_ADC_INJ_CHANNELS);
	ADC_InjectedSequencerLengthConfig(ADC2, HW_ADC_INJ_CHANNELS);
#ifdef HW_HAS_3_SHUNTS
	ADC_InjectedSequencerLengthConfig(ADC3, HW_ADC_INJ_CHANNELS);
#endif

	hw_setup_adc_channels();

	ADC_ITConfig(ADC1, ADC_IT_JEOC, ENABLE);
	nvicEnableVector(ADC_IRQn, 6);

	ADC_Cmd(ADC1, ENABLE);
	ADC_Cmd(ADC2, ENABLE);
	ADC_Cmd(ADC3, ENABLE);

	// Timer8 for ADC sampling
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
	TIM_OC3Init(TIM8, &TIM_OCInitStructure);
	TIM_OC3PreloadConfig(TIM8, TIM_OCPreload_Enable);

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
	mc_timer_struct timer_tmp;
	timer_tmp.top = TIM1->ARR;
	timer_tmp.duty = TIM1->ARR / 2;
	update_adc_sample_pos(&timer_tmp);
	set_next_timer_settings(&timer_tmp);

	utils_sys_unlock_cnt();

	CURRENT_FILTER_ON();
	CURRENT_FILTER_ON_M2();

	// Calibrate current offset
	ENABLE_GATE();
	DCCAL_OFF();
	do_dc_cal();

	// Start threads
	timer_thd_stop = false;
	rpm_thd_stop = false;
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);
	chThdCreateStatic(rpm_thread_wa, sizeof(rpm_thread_wa), NORMALPRIO, rpm_thread, NULL);

	// Check if the system has resumed from IWDG reset
	if (timeout_had_IWDG_reset()) {
		mc_interface_fault_stop(FAULT_CODE_BOOTING_FROM_WATCHDOG_RESET, false, false);
	}

	// Reset tachometers again
	tachometer = 0;
	tachometer_abs = 0;
	init_done = true;
}

void mcpwm_deinit(void) {
	if (!init_done) {
		return;
	}

	init_done = false;

	timer_thd_stop = true;
	rpm_thd_stop = true;

	while (timer_thd_stop || rpm_thd_stop) {
		chThdSleepMilliseconds(1);
	}

	TIM_DeInit(TIM1);
	TIM_DeInit(TIM8);
	ADC_DeInit();
	DMA_DeInit(DMA2_Stream4);
	nvicDisableVector(ADC_IRQn);
	dmaStreamRelease(STM32_DMA_STREAM(STM32_DMA_STREAM_ID(2, 4)));
}

bool mcpwm_init_done(void) {
	return init_done;
}

void mcpwm_set_configuration(volatile mc_configuration *configuration) {
	// Stop everything first to be safe
	control_mode = CONTROL_MODE_NONE;
	stop_pwm_ll();

	utils_sys_lock_cnt();
	conf = configuration;
	comm_mode_next = conf->comm_mode;
	mcpwm_init_hall_table((int8_t*)conf->hall_table);
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
	const int fwd_to_rev[7] = {-1,1,6,5,4,3,2};

	for (int i = 0;i < 8;i++) {
		hall_to_phase_table[8 + i] = table[i];
		int ind_now = hall_to_phase_table[8 + i];

		if (ind_now < 1) {
			hall_to_phase_table[i] = ind_now;
			continue;
		}

		hall_to_phase_table[i] = fwd_to_rev[ind_now];
	}
}

static void do_dc_cal(void) {
	DCCAL_ON();

	// Wait max 5 seconds
	int cnt = 0;
	while(IS_DRV_FAULT()){
		chThdSleepMilliseconds(1);
		cnt++;
		if (cnt > 5000) {
			break;
		}
	};

	chThdSleepMilliseconds(1000);
	curr0_sum = 0;
	curr1_sum = 0;

#ifdef HW_HAS_3_SHUNTS
	curr2_sum = 0;
#endif

	curr_start_samples = 0;
	while(curr_start_samples < 4000) {};
	curr0_offset = curr0_sum / curr_start_samples;
	curr1_offset = curr1_sum / curr_start_samples;

#ifdef HW_HAS_3_SHUNTS
	curr2_offset = curr2_sum / curr_start_samples;
#endif

	DCCAL_OFF();
	dccal_done = true;
}

static void pll_run(float phase, float dt, volatile float *phase_var,
		volatile float *speed_var) {
	UTILS_NAN_ZERO(*phase_var);
	float delta_theta = phase - *phase_var;
	utils_norm_angle_rad(&delta_theta);
	UTILS_NAN_ZERO(*speed_var);
	*phase_var += (*speed_var + conf->foc_pll_kp * delta_theta) * dt;
	utils_norm_angle_rad((float*)phase_var);
	*speed_var += conf->foc_pll_ki * delta_theta * dt;
}

/**
 * Use duty cycle control. Absolute values less than MCPWM_MIN_DUTY_CYCLE will
 * stop the motor.
 *
 * @param dutyCycle
 * The duty cycle to use.
 */
void mcpwm_set_duty(float dutyCycle) {
	control_mode = CONTROL_MODE_DUTY;
	set_duty_cycle_hl(dutyCycle);
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
void mcpwm_set_duty_noramp(float dutyCycle) {
	control_mode = CONTROL_MODE_DUTY;

	if (state != MC_STATE_RUNNING) {
		set_duty_cycle_hl(dutyCycle);
	} else {
		dutycycle_set = dutyCycle;
		dutycycle_now = dutyCycle;
		set_duty_cycle_ll(dutyCycle);
	}
}

/**
 * Use PID rpm control. Note that this value has to be multiplied by half of
 * the number of motor poles.
 *
 * @param rpm
 * The electrical RPM goal value to use.
 */
void mcpwm_set_pid_speed(float rpm) {
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
	control_mode = CONTROL_MODE_POS;
	pos_pid_set_pos = pos;

	if (state != MC_STATE_RUNNING) {
		set_duty_cycle_hl(conf->l_min_duty);
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
void mcpwm_set_current(float current) {
	if (fabsf(current) < conf->cc_min_current) {
		control_mode = CONTROL_MODE_NONE;
		stop_pwm_ll();
		return;
	}

	utils_truncate_number(&current, -conf->l_current_max * conf->l_current_max_scale,
			conf->l_current_max * conf->l_current_max_scale);

	control_mode = CONTROL_MODE_CURRENT;
	current_set = current;

	if (state != MC_STATE_RUNNING) {
		set_duty_cycle_hl(SIGN(current) * conf->l_min_duty);
	}
}

void mcpwm_release_motor(void) {
	current_set = 0.0;
	control_mode = CONTROL_MODE_NONE;
	stop_pwm_ll();
}

/**
 * Brake the motor with a desired current. Absolute values less than
 * conf->cc_min_current will release the motor.
 *
 * @param current
 * The current to use. Positive and negative values give the same effect.
 */
void mcpwm_set_brake_current(float current) {
	if (fabsf(current) < conf->cc_min_current) {
		control_mode = CONTROL_MODE_NONE;
		stop_pwm_ll();
		return;
	}

	utils_truncate_number(&current, -fabsf(conf->lo_current_min), fabsf(conf->lo_current_min));

	control_mode = CONTROL_MODE_CURRENT_BRAKE;
	current_set = current;

	if (state != MC_STATE_RUNNING && state != MC_STATE_FULL_BRAKE) {
		// In case the motor is already spinning, set the state to running
		// so that it can be ramped down before the full brake is applied.
		if (conf->motor_type == MOTOR_TYPE_DC) {
			if (fabsf(dutycycle_now) > 0.1) {
				state = MC_STATE_RUNNING;
			} else {
				full_brake_ll();
			}
		} else {
			if (fabsf(rpm_now) > conf->l_max_erpm_fbrake) {
				state = MC_STATE_RUNNING;
			} else {
				full_brake_ll();
			}
		}
	}
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
	if (conf->motor_type == MOTOR_TYPE_DC) {
		return RADPS2RPM_f(m_pll_speed);
	} else {
		return direction ? rpm_now : -rpm_now;
	}
}

mc_state mcpwm_get_state(void) {
	return state;
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
	return rpm_now / (mc_interface_get_input_voltage_filtered() * fabsf(dutycycle_now));
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
	return last_current_sample;
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
	return last_current_sample_filtered;
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
 * Set the number of steps the motor has rotated. This number is signed and
 * becomes a negative when the motor is rotating backwards.
 *
 * @param steps
 * New number of motor steps will be set after this call.
 *
 * @return
 * The previous tachometer value in motor steps. The number of motor revolutions will
 * be this number divided by (3 * MOTOR_POLE_NUMBER).
 */
int mcpwm_set_tachometer_value(int steps)
{
	int val = tachometer;

	tachometer = steps;

	return val;
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
 * Switch off all FETs.
 */
void mcpwm_stop_pwm(void) {
	control_mode = CONTROL_MODE_NONE;
	stop_pwm_ll();
}

static void stop_pwm_ll(void) {
	state = MC_STATE_OFF;
	ignore_iterations = MCPWM_CMD_STOP_TIME;
	stop_pwm_hw();
}

static void stop_pwm_hw(void) {
#ifdef HW_HAS_DRV8313
	DISABLE_BR();
#endif

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

	set_switching_frequency(conf->m_bldc_f_sw_max);
}

static void full_brake_ll(void) {
	state = MC_STATE_FULL_BRAKE;
	ignore_iterations = MCPWM_CMD_STOP_TIME;
	full_brake_hw();
}

static void full_brake_hw(void) {
#ifdef HW_HAS_DRV8313
	ENABLE_BR();
#endif

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

	set_switching_frequency(conf->m_bldc_f_sw_max);
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
	utils_truncate_number(&dutyCycle, -conf->l_max_duty, conf->l_max_duty);

	if (state == MC_STATE_DETECTING) {
		stop_pwm_ll();
		return;
	}

	dutycycle_set = dutyCycle;

	if (state != MC_STATE_RUNNING) {
		if (fabsf(dutyCycle) >= conf->l_min_duty) {
			// dutycycle_now is updated by the back-emf detection. If the motor already
			// is spinning, it will be non-zero.
			if (fabsf(dutycycle_now) < conf->l_min_duty) {
				dutycycle_now = SIGN(dutyCycle) * conf->l_min_duty;
			}

			set_duty_cycle_ll(dutycycle_now);
		} else {
			// In case the motor is already spinning, set the state to running
			// so that it can be ramped down before the full brake is applied.
			if (conf->motor_type == MOTOR_TYPE_DC) {
				if (fabsf(dutycycle_now) > 0.1) {
					state = MC_STATE_RUNNING;
				} else {
					full_brake_ll();
				}
			} else {
				if (fabsf(rpm_now) > conf->l_max_erpm_fbrake) {
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
	if (dutyCycle >= conf->l_min_duty) {
		direction = 1;
	} else if (dutyCycle <= -conf->l_min_duty) {
		dutyCycle = -dutyCycle;
		direction = 0;
	}

	if (dutyCycle < conf->l_min_duty) {
		float max_erpm_fbrake;
#if BLDC_SPEED_CONTROL_CURRENT
		if (control_mode == CONTROL_MODE_CURRENT ||
				control_mode == CONTROL_MODE_CURRENT_BRAKE ||
				control_mode == CONTROL_MODE_SPEED) {
#else
		if (control_mode == CONTROL_MODE_CURRENT || control_mode == CONTROL_MODE_CURRENT_BRAKE) {
#endif
			max_erpm_fbrake = conf->l_max_erpm_fbrake_cc;
		} else {
			max_erpm_fbrake = conf->l_max_erpm_fbrake;
		}

		switch (state) {
		case MC_STATE_RUNNING:
			// TODO!!!
			if (fabsf(rpm_now) > max_erpm_fbrake) {
				dutyCycle = conf->l_min_duty;
			} else {
				full_brake_ll();
				return;
			}
			break;

		case MC_STATE_DETECTING:
			stop_pwm_ll();
			return;
			break;

		default:
			return;
		}
	} else if (dutyCycle > conf->l_max_duty) {
		dutyCycle = conf->l_max_duty;
	}

	set_duty_cycle_hw(dutyCycle);

	if (conf->motor_type == MOTOR_TYPE_DC) {
		state = MC_STATE_RUNNING;
		set_next_comm_step(comm_step);
		commutate(1);
	} else {
		if (sensorless_now) {
			if (state != MC_STATE_RUNNING) {
				if (state == MC_STATE_OFF) {
					state = MC_STATE_RUNNING;

					if (fabsf(rpm_now) < conf->sl_min_erpm) {
						commutate(1);
					}
				} else if (state == MC_STATE_FULL_BRAKE) {
					if (fabsf(rpm_now) < conf->sl_min_erpm && mcpwm_get_tot_current_filtered() < conf->sl_max_fullbreak_current_dir_change) {
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

	utils_truncate_number(&dutyCycle, conf->l_min_duty, conf->l_max_duty);

	if (conf->motor_type == MOTOR_TYPE_DC) {
		switching_frequency_now = conf->m_dc_f_sw;
	} else {
		if (IS_DETECTING() || conf->pwm_mode == PWM_MODE_BIPOLAR) {
			switching_frequency_now = conf->m_bldc_f_sw_max;
		} else {
			switching_frequency_now = (float)conf->m_bldc_f_sw_min * (1.0 - fabsf(dutyCycle)) +
					conf->m_bldc_f_sw_max * fabsf(dutyCycle);
		}
	}

	timer_tmp.top = SYSTEM_CORE_CLOCK / (int)switching_frequency_now;

	if (conf->motor_type == MOTOR_TYPE_BLDC && conf->pwm_mode == PWM_MODE_BIPOLAR && !IS_DETECTING()) {
		timer_tmp.duty = (uint16_t) (((float) timer_tmp.top / 2.0) * dutyCycle
				+ ((float) timer_tmp.top / 2.0));
	} else {
		timer_tmp.duty = (uint16_t)((float)timer_tmp.top * dutyCycle);
	}

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
#if BLDC_SPEED_CONTROL_CURRENT
		i_term = 0.0;
#else
		i_term = dutycycle_now;
#endif
		prev_error = 0.0;
		return;
	}

	const float rpm = mcpwm_get_rpm();
	float error = speed_pid_set_rpm - rpm;

	// Too low RPM set. Stop and return.
	if (fabsf(speed_pid_set_rpm) < conf->s_pid_min_erpm) {
		i_term = dutycycle_now;
		prev_error = error;
		mcpwm_set_duty(0.0);
		return;
	}

#if BLDC_SPEED_CONTROL_CURRENT
	// Compute parameters
	p_term = error * conf->s_pid_kp * (1.0 / 20.0);
	i_term += error * (conf->s_pid_ki * MCPWM_PID_TIME_K) * (1.0 / 20.0);
	d_term = (error - prev_error) * (conf->s_pid_kd / MCPWM_PID_TIME_K) * (1.0 / 20.0);

	// Filter D
	static float d_filter = 0.0;
	UTILS_LP_FAST(d_filter, d_term, conf->p_pid_kd_filter);
	d_term = d_filter;

	// I-term wind-up protection
	utils_truncate_number(&i_term, -1.0, 1.0);

	// Store previous error
	prev_error = error;

	// Calculate output
	float output = p_term + i_term + d_term;
	utils_truncate_number(&output, -1.0, 1.0);

	// Optionally disable braking
	if (!conf->s_pid_allow_braking) {
		if (rpm > 0.0 && output < 0.0) {
			output = 0.0;
		}

		if (rpm < 0.0 && output > 0.0) {
			output = 0.0;
		}
	}

	current_set = output * conf->lo_current_max;

	if (state != MC_STATE_RUNNING) {
		set_duty_cycle_hl(SIGN(output) * conf->l_min_duty);
	}
#else
	// Compensation for supply voltage variations
	float scale = 1.0 / mc_interface_get_input_voltage_filtered();

	// Compute parameters
	p_term = error * conf->s_pid_kp * scale;
	i_term += error * (conf->s_pid_ki * MCPWM_PID_TIME_K) * scale;
	d_term = (error - prev_error) * (conf->s_pid_kd / MCPWM_PID_TIME_K) * scale;

	// Filter D
	static float d_filter = 0.0;
	UTILS_LP_FAST(d_filter, d_term, conf->s_pid_kd_filter);
	d_term = d_filter;

	// I-term wind-up protection
	utils_truncate_number(&i_term, -1.0, 1.0);

	// Store previous error
	prev_error = error;

	// Calculate output
	float output = p_term + i_term + d_term;

	// Make sure that at least minimum output is used
	if (fabsf(output) < conf->l_min_duty) {
		output = SIGN(output) * conf->l_min_duty;
	}

	// Do not output in reverse direction to oppose too high rpm
	if (speed_pid_set_rpm > 0.0 && output < 0.0) {
		output = conf->l_min_duty;
		i_term = 0.0;
	} else if (speed_pid_set_rpm < 0.0 && output > 0.0) {
		output = -conf->l_min_duty;
		i_term = 0.0;
	}

	set_duty_cycle_hl(output);
#endif
}

static void run_pid_control_pos(float dt, float pos_now) {
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
	float error = utils_angle_difference(pos_now, pos_pid_set_pos);

	// Compute parameters
	p_term = error * conf->p_pid_kp;
	i_term += error * (conf->p_pid_ki * dt);
	d_term = (error - prev_error) * (conf->p_pid_kd / dt);

	// Filter D
	static float d_filter = 0.0;
	UTILS_LP_FAST(d_filter, d_term, conf->p_pid_kd_filter);
	d_term = d_filter;

	// I-term wind-up protection
	utils_truncate_number(&i_term, -1.0, 1.0);

	// Store previous error
	prev_error = error;

	// Calculate output
	float output = p_term + i_term + d_term;
	utils_truncate_number(&output, -1.0, 1.0);

	current_set = output * conf->lo_current_max;
}

static THD_FUNCTION(rpm_thread, arg) {
	(void)arg;

	chRegSetThreadName("rpm timer");

	for (;;) {
		if (rpm_thd_stop) {
			rpm_thd_stop = false;
			return;
		}

		if (rpm_dep.comms != 0) {
			utils_sys_lock_cnt();
			const float comms = (float)rpm_dep.comms;
			const float time_at_comm = rpm_dep.time_at_comm;
			rpm_dep.comms = 0;
			rpm_dep.time_at_comm = 0.0;
			utils_sys_unlock_cnt();

			rpm_now = (comms * 60.0) / (time_at_comm * 6.0);
		} else {
			// In case we have slowed down
			float rpm_tmp = 60.0 / (timer_seconds_elapsed_since(rpm_timer_start) * 6.0);

			if (fabsf(rpm_tmp) < fabsf(rpm_now)) {
				rpm_now = rpm_tmp;
			}
		}

		// Some low-pass filtering
		static float rpm_filtered = 0.0;
		UTILS_LP_FAST(rpm_filtered, rpm_now, 0.1);
		rpm_now = rpm_filtered;
		const float rpm_abs = fabsf(rpm_now);

		// Update the cycle integrator limit
		rpm_dep.cycle_int_limit = conf->sl_cycle_int_limit;
		rpm_dep.cycle_int_limit_running = rpm_dep.cycle_int_limit + (float)ADC_Value[ADC_IND_VIN_SENS] *
				conf->sl_bemf_coupling_k / (rpm_abs > conf->sl_min_erpm ? rpm_abs : conf->sl_min_erpm);
		rpm_dep.cycle_int_limit_running = utils_map(rpm_abs, 0,
				conf->sl_cycle_int_rpm_br, rpm_dep.cycle_int_limit_running,
				rpm_dep.cycle_int_limit_running * conf->sl_phase_advance_at_br);
		rpm_dep.cycle_int_limit_max = rpm_dep.cycle_int_limit + (float)ADC_Value[ADC_IND_VIN_SENS] *
				conf->sl_bemf_coupling_k / conf->sl_min_erpm_cycle_int_limit;

		if (rpm_dep.cycle_int_limit_running < 1.0) {
			rpm_dep.cycle_int_limit_running = 1.0;
		}

		if (rpm_dep.cycle_int_limit_running > rpm_dep.cycle_int_limit_max) {
			rpm_dep.cycle_int_limit_running = rpm_dep.cycle_int_limit_max;
		}

		rpm_dep.comm_time_sum = conf->m_bldc_f_sw_max / ((rpm_abs / 60.0) * 6.0);
		rpm_dep.comm_time_sum_min_rpm = conf->m_bldc_f_sw_max / ((conf->sl_min_erpm / 60.0) * 6.0);

		run_pid_control_speed();

		chThdSleepMilliseconds(1);
	}
}

static THD_FUNCTION(timer_thread, arg) {
	(void)arg;

	chRegSetThreadName("mcpwm timer");

	float amp;
	float min_s;
	float max_s;

	for(;;) {
		if (timer_thd_stop) {
			timer_thd_stop = false;
			return;
		}

		if (state == MC_STATE_OFF) {
			// Track the motor back-emf and follow it with dutycycle_now. Also track
			// the direction of the motor.
			amp = filter_run_fir_iteration((float*)amp_fir_samples,
					(float*)amp_fir_coeffs, AMP_FIR_TAPS_BITS, amp_fir_index);

			// Direction tracking
			if (conf->motor_type == MOTOR_TYPE_DC) {
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
			utils_truncate_number((float*)&dutycycle_now, -conf->l_max_duty, conf->l_max_duty);
		} else {
			tachometer_for_direction = 0;
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
				if (dutycycle_now >= conf->l_min_duty) {
					filter_add_sample((float*)kv_fir_samples, mcpwm_get_kv(),
							KV_FIR_TAPS_BITS, (uint32_t*)&kv_fir_index);
				}
			}
		}

		chThdSleepMilliseconds(1);
	}
}

void mcpwm_adc_inj_int_handler(void) {
	uint32_t t_start = timer_time_now();

	int curr0 = HW_GET_INJ_CURR1();
	int curr1 = HW_GET_INJ_CURR2();

	int curr0_2 = HW_GET_INJ_CURR1_S2();
	int curr1_2 = HW_GET_INJ_CURR1_S2();

#ifdef HW_HAS_3_SHUNTS
	int curr2 = HW_GET_INJ_CURR3();
#endif

#ifdef INVERTED_SHUNT_POLARITY
	curr0 = 4095 - curr0;
	curr1 = 4095 - curr1;

	curr0_2 = 4095 - curr0_2;
	curr1_2 = 4095 - curr1_2;
#ifdef HW_HAS_3_SHUNTS
	curr2 = 4095 - curr2;
#endif
#endif

	float curr0_currsamp = curr0;
	float curr1_currsamp = curr1;
#ifdef HW_HAS_3_SHUNTS
	float curr2_currsamp = curr2;
#endif

	if (curr_samp_volt & (1 << 0)) {
		curr0 = GET_CURRENT1();
	}

	if (curr_samp_volt & (1 << 1)) {
		curr1 = GET_CURRENT2();
	}

#ifdef HW_HAS_3_SHUNTS
	if (curr_samp_volt & (1 << 2)) {
		curr2 = GET_CURRENT3();
	}
#endif

	// DCCal every other cycle
	//	static bool sample_ofs = true;
	//	if (sample_ofs) {
	//		sample_ofs = false;
	//		curr0_offset = curr0;
	//		curr1_offset = curr1;
	//		DCCAL_OFF();
	//		return;
	//	} else {
	//		sample_ofs = true;
	//		DCCAL_ON();
	//	}

	curr0_sum += curr0;
	curr1_sum += curr1;
#ifdef HW_HAS_3_SHUNTS
	curr2_sum += curr2;
#endif

	curr_start_samples++;

	curr0_currsamp -= curr0_offset;
	curr1_currsamp -= curr1_offset;
	curr0 -= curr0_offset;
	curr1 -= curr1_offset;
	curr0_2 -= curr0_offset;
	curr1_2 -= curr1_offset;

#ifdef HW_HAS_3_SHUNTS
	curr2_currsamp -= curr2_offset;
	curr2 -= curr2_offset;
#endif

#if CURR1_DOUBLE_SAMPLE || CURR2_DOUBLE_SAMPLE
	if (conf->pwm_mode != PWM_MODE_BIPOLAR && conf->motor_type == MOTOR_TYPE_BLDC) {
		if (direction) {
			if (CURR1_DOUBLE_SAMPLE && comm_step == 3) {
				curr0 = (curr0 + curr0_2) / 2.0;
			} else if (CURR2_DOUBLE_SAMPLE && comm_step == 4) {
				curr1 = (curr1 + curr1_2) / 2.0;
			}
		} else {
			if (CURR1_DOUBLE_SAMPLE && comm_step == 2) {
				curr0 = (curr0 + curr0_2) / 2.0;
			} else if (CURR2_DOUBLE_SAMPLE && comm_step == 1) {
				curr1 = (curr1 + curr1_2) / 2.0;
			}
		}
	}
#endif

	ADC_curr_norm_value[0] = curr0;
	ADC_curr_norm_value[1] = curr1;

#ifdef HW_HAS_3_SHUNTS
	ADC_curr_norm_value[2] = curr2;
#else
	ADC_curr_norm_value[2] = -(ADC_curr_norm_value[0] + ADC_curr_norm_value[1]);
#endif

	float curr_tot_sample = 0;
	if (conf->motor_type == MOTOR_TYPE_DC) {
		if (direction) {
#ifdef HW_HAS_3_SHUNTS
			curr_tot_sample = -(GET_CURRENT3() - curr2_offset);
#else
			curr_tot_sample = -(GET_CURRENT2() - curr1_offset);
#endif
		} else {
			curr_tot_sample = -(GET_CURRENT1() - curr0_offset);
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
#ifdef HW_HAS_3_SHUNTS
			if (direction) {
				switch (comm_step) {
				case 1: curr_tot_sample = -(float)ADC_curr_norm_value[2]; break;
				case 2: curr_tot_sample = -(float)ADC_curr_norm_value[2]; break;
				case 3: curr_tot_sample = -(float)ADC_curr_norm_value[1]; break;
				case 4: curr_tot_sample = -(float)ADC_curr_norm_value[1]; break;
				case 5: curr_tot_sample = -(float)ADC_curr_norm_value[0]; break;
				case 6: curr_tot_sample = -(float)ADC_curr_norm_value[0]; break;
				default: break;
				}
			} else {
				switch (comm_step) {
				case 1: curr_tot_sample = -(float)ADC_curr_norm_value[1]; break;
				case 2: curr_tot_sample = -(float)ADC_curr_norm_value[1]; break;
				case 3: curr_tot_sample = -(float)ADC_curr_norm_value[2]; break;
				case 4: curr_tot_sample = -(float)ADC_curr_norm_value[2]; break;
				case 5: curr_tot_sample = -(float)ADC_curr_norm_value[0]; break;
				case 6: curr_tot_sample = -(float)ADC_curr_norm_value[0]; break;
				default: break;
				}
			}
#else
			if (direction) {
				switch (comm_step) {
				case 1: curr_tot_sample = -(float)ADC_curr_norm_value[1]; break;
				case 2: curr_tot_sample = -(float)ADC_curr_norm_value[1]; break;
				case 3: curr_tot_sample = (float)ADC_curr_norm_value[0]; break;
				case 4: curr_tot_sample = (float)ADC_curr_norm_value[1]; break;
				case 5: curr_tot_sample = -(float)ADC_curr_norm_value[0]; break;
				case 6: curr_tot_sample = -(float)ADC_curr_norm_value[0]; break;
				default: break;
				}
			} else {
				switch (comm_step) {
				case 1: curr_tot_sample = (float)ADC_curr_norm_value[1]; break;
				case 2: curr_tot_sample = (float)ADC_curr_norm_value[0]; break;
				case 3: curr_tot_sample = -(float)ADC_curr_norm_value[1]; break;
				case 4: curr_tot_sample = -(float)ADC_curr_norm_value[1]; break;
				case 5: curr_tot_sample = -(float)ADC_curr_norm_value[0]; break;
				case 6: curr_tot_sample = -(float)ADC_curr_norm_value[0]; break;
				default: break;
				}
			}
#endif

			const float tot_sample_tmp = curr_tot_sample;
			static int comm_step_prev = 1;
			static float prev_tot_sample = 0.0;
			if (comm_step != comm_step_prev) {
				curr_tot_sample = prev_tot_sample;
			}
			comm_step_prev = comm_step;
			prev_tot_sample = tot_sample_tmp;
		}

		if (detect_now == 4) {
			const float a = fabsf(ADC_curr_norm_value[0]);
			const float b = fabsf(ADC_curr_norm_value[1]);

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

			const int vzero = ADC_V_ZERO;
			//			const int vzero = (ADC_V_L1 + ADC_V_L2 + ADC_V_L3) / 3;

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

	last_current_sample = curr_tot_sample * FAC_CURRENT;

	// Filter out outliers
	if (fabsf(last_current_sample) > (conf->l_abs_current_max * 1.2)) {
		last_current_sample = SIGN(last_current_sample) * conf->l_abs_current_max * 1.2;
	}

	filter_add_sample((float*) current_fir_samples, last_current_sample,
			CURR_FIR_TAPS_BITS, (uint32_t*) &current_fir_index);
	last_current_sample_filtered = filter_run_fir_iteration(
			(float*) current_fir_samples, (float*) current_fir_coeffs,
			CURR_FIR_TAPS_BITS, current_fir_index);

	last_inj_adc_isr_duration = timer_seconds_elapsed_since(t_start);
}

/*
 * New ADC samples ready. Do commutation!
 */
void mcpwm_adc_int_handler(void *p, uint32_t flags) {
	(void)p;
	(void)flags;

	uint32_t t_start = timer_time_now();

	// Set the next timer settings if an update is far enough away
	update_timer_attempt();

	// Reset the watchdog
	timeout_feed_WDT(THREAD_MCPWM);

	const float input_voltage = GET_INPUT_VOLTAGE();
	int ph1, ph2, ph3;

	static int direction_before = 1;
	if (!(state == MC_STATE_RUNNING && direction == direction_before)) {
		has_commutated = 0;
	}
	direction_before = direction;

	if (conf->motor_type == MOTOR_TYPE_BLDC) {
		int ph1_raw, ph2_raw, ph3_raw;

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
					if (conf->comm_mode == COMM_MODE_INTEGRATE) {
						// This means that the motor is stuck. If this commutation does not
						// produce any torque because of misalignment at start, two
						// commutations ahead should produce full torque.
						commutate(2);
					} else if (conf->comm_mode == COMM_MODE_DELAY) {
						commutate(1);
					}

					cycle_integrator = 0.0;
				}
			}

			if ((state == MC_STATE_RUNNING && pwm_cycles >= 2) || state == MC_STATE_OFF) {
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
					// TODO!
					//					const int min = 100;
					int min = (int)((1.0 - fabsf(dutycycle_now)) * (float)ADC_Value[ADC_IND_VIN_SENS] * 0.3);
					if (min > ADC_Value[ADC_IND_VIN_SENS] / 4) {
						min = ADC_Value[ADC_IND_VIN_SENS] / 4;
					}

					if (pwm_cycles_sum > (last_pwm_cycles_sum / 2.0) ||
							!has_commutated || (ph_now_raw > min && ph_now_raw < (ADC_Value[ADC_IND_VIN_SENS] - min))) {
						cycle_integrator += (float)v_diff / switching_frequency_now;
					}
				}

				static float cycle_sum = 0.0;
				if (conf->comm_mode == COMM_MODE_INTEGRATE) {
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
						cycle_sum = 0.0;
					}
				} else if (conf->comm_mode == COMM_MODE_DELAY) {
					if (v_diff > 0) {
						cycle_sum += conf->m_bldc_f_sw_max / switching_frequency_now;

						if (cycle_sum >= utils_map(fabsf(rpm_now), 0,
								conf->sl_cycle_int_rpm_br, rpm_dep.comm_time_sum / 2.0,
								(rpm_dep.comm_time_sum / 2.0) * conf->sl_phase_advance_at_br)) {
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

			pwm_cycles_sum += conf->m_bldc_f_sw_max / switching_frequency_now;
			pwm_cycles++;
		} else {
			const int hall_phase = mcpwm_read_hall_phase();
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

	const float current_nofilter = mcpwm_get_tot_current();
	const float current_in_nofilter = current_nofilter * fabsf(dutycycle_now);

	if (state == MC_STATE_RUNNING && has_commutated) {
		// Compensation for supply voltage variations
		const float voltage_scale = 20.0 / input_voltage;
		float ramp_step = conf->m_duty_ramp_step / (switching_frequency_now / 1000.0);
		float ramp_step_no_lim = ramp_step;

		if (slow_ramping_cycles) {
			slow_ramping_cycles--;
			ramp_step *= 0.1;
		}

		float dutycycle_now_tmp = dutycycle_now;

#if BLDC_SPEED_CONTROL_CURRENT
		if (control_mode == CONTROL_MODE_CURRENT ||
				control_mode == CONTROL_MODE_POS ||
				control_mode == CONTROL_MODE_SPEED) {
#else
		if (control_mode == CONTROL_MODE_CURRENT || control_mode == CONTROL_MODE_POS) {
#endif
			// Compute error
			const float error = current_set - (direction ? current_nofilter : -current_nofilter);
			float step = error * conf->cc_gain * voltage_scale;
			const float start_boost = conf->cc_startup_boost_duty * voltage_scale;

			// Do not ramp too much
			utils_truncate_number(&step, -conf->cc_ramp_step_max, conf->cc_ramp_step_max);

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
			utils_truncate_number((float*)&dutycycle_now_tmp, -conf->l_max_duty, conf->l_max_duty);

			// Lower truncation
			if (fabsf(dutycycle_now_tmp) < conf->l_min_duty) {
				if (dutycycle_now_tmp < 0.0 && current_set > 0.0) {
					dutycycle_now_tmp = conf->l_min_duty;
				} else if (dutycycle_now_tmp > 0.0 && current_set < 0.0) {
					dutycycle_now_tmp = -conf->l_min_duty;
				}
			}

			// The set dutycycle should be in the correct direction in case the output is lower
			// than the minimum duty cycle and the mechanism below gets activated.
			dutycycle_set = dutycycle_now_tmp >= 0.0 ? conf->l_min_duty : -conf->l_min_duty;
		} else if (control_mode == CONTROL_MODE_CURRENT_BRAKE) {
			// Compute error
			const float error = -fabsf(current_set) - current_nofilter;
			float step = error * conf->cc_gain * voltage_scale;

			// Do not ramp too much
			utils_truncate_number(&step, -conf->cc_ramp_step_max, conf->cc_ramp_step_max);

			// Switching frequency correction
			step /= switching_frequency_now / 1000.0;

			if (slow_ramping_cycles) {
				slow_ramping_cycles--;
				step *= 0.1;
			}

			dutycycle_now_tmp += SIGN(dutycycle_now_tmp) * step;

			// Upper truncation
			utils_truncate_number((float*)&dutycycle_now_tmp, -conf->l_max_duty, conf->l_max_duty);

			// Lower truncation
			if (fabsf(dutycycle_now_tmp) < conf->l_min_duty) {
				if (fabsf(rpm_now) < conf->l_max_erpm_fbrake_cc) {
					dutycycle_now_tmp = 0.0;
					dutycycle_set = dutycycle_now_tmp;
				} else {
					dutycycle_now_tmp = SIGN(dutycycle_now_tmp) * conf->l_min_duty;
					dutycycle_set = dutycycle_now_tmp;
				}
			}
		} else {
			utils_step_towards((float*)&dutycycle_now_tmp, dutycycle_set, ramp_step);
		}

		static int limit_delay = 0;

		// Apply limits in priority order
		if (current_nofilter > conf->lo_current_max) {
			utils_step_towards((float*) &dutycycle_now, 0.0,
					ramp_step_no_lim * fabsf(current_nofilter - conf->lo_current_max) * conf->m_current_backoff_gain);
			limit_delay = 1;
		} else if (current_nofilter < conf->lo_current_min) {
			utils_step_towards((float*) &dutycycle_now, direction ? conf->l_max_duty : -conf->l_max_duty,
					ramp_step_no_lim * fabsf(current_nofilter - conf->lo_current_min) * conf->m_current_backoff_gain);
			limit_delay = 1;
		} else if (current_in_nofilter > conf->lo_in_current_max) {
			utils_step_towards((float*) &dutycycle_now, 0.0,
					ramp_step_no_lim * fabsf(current_in_nofilter - conf->lo_in_current_max) * conf->m_current_backoff_gain);
			limit_delay = 1;
		} else if (current_in_nofilter < conf->lo_in_current_min) {
			utils_step_towards((float*) &dutycycle_now, direction ? conf->l_max_duty : -conf->l_max_duty,
					ramp_step_no_lim * fabsf(current_in_nofilter - conf->lo_in_current_min) * conf->m_current_backoff_gain);
			limit_delay = 1;
		}

		if (limit_delay > 0) {
			limit_delay--;
		} else {
			dutycycle_now = dutycycle_now_tmp;
		}

		// When the set duty cycle is in the opposite direction, make sure that the motor
		// starts again after stopping completely
		if (fabsf(dutycycle_now) < conf->l_min_duty) {
			if (dutycycle_set >= conf->l_min_duty) {
				dutycycle_now = conf->l_min_duty;
			} else if (dutycycle_set <= -conf->l_min_duty) {
				dutycycle_now = -conf->l_min_duty;
			}
		}

		// Don't start in the opposite direction when the RPM is too high even if the current is low enough.
		if (conf->motor_type != MOTOR_TYPE_DC) {
			const float rpm = mcpwm_get_rpm();
			if (dutycycle_now >= conf->l_min_duty && rpm < -conf->l_max_erpm_fbrake) {
				dutycycle_now = -conf->l_min_duty;
			} else if (dutycycle_now <= -conf->l_min_duty && rpm > conf->l_max_erpm_fbrake) {
				dutycycle_now = conf->l_min_duty;
			}
		}

		set_duty_cycle_ll(dutycycle_now);
	}

	mc_interface_mc_timer_isr(false);

	if (encoder_is_configured()) {
		float pos = encoder_read_deg();
		run_pid_control_pos(1.0 / switching_frequency_now, pos);
		pll_run(-DEG2RAD_f(pos), 1.0 / switching_frequency_now, &m_pll_phase, &m_pll_speed);
	}

	last_adc_isr_duration = timer_seconds_elapsed_since(t_start);
}

void mcpwm_set_detect(void) {
	if (mc_interface_try_input()) {
		return;
	}

	control_mode = CONTROL_MODE_NONE;
	stop_pwm_hw();

	set_switching_frequency(conf->m_bldc_f_sw_max);

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
	ph[0] = RAD2DEG_f(asinf(v0));

	float res = ph[0];
	if (v1 < v2) {
		res = 180 - ph[0];
	}

	utils_norm_angle(&res);

	return res;
}

float mcpwm_read_reset_avg_cycle_integrator(void) {
	float res = cycle_integrator_sum / cycle_integrator_iterations;
	cycle_integrator_sum = 0;
	cycle_integrator_iterations = 0;
	return res;
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
	conf->comm_mode = mode;
}

mc_comm_mode mcpwm_get_comm_mode(void) {
	return conf->comm_mode;
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

bool mcpwm_is_dccal_done(void) {
	return dccal_done;
}

void mcpwm_switch_comm_mode(mc_comm_mode next) {
	comm_mode_next = next;
}

/**
 * Reset the hall sensor detection table
 */
void mcpwm_reset_hall_detect_table(void) {
	memset((void*)hall_detect_table, 0, sizeof(hall_detect_table[0][0]) * 8 * 7);
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
 * -3: Encoder enabled
 */
int mcpwm_get_hall_detect_result(int8_t *table) {
	if (conf->m_sensor_port_mode != SENSOR_PORT_MODE_HALL) {
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

#ifdef HW_HAS_3_SHUNTS
	volatile uint32_t curr3_sample = timer_tmp->curr3_sample;
#endif

	if (duty > (uint32_t)((float)top * conf->l_max_duty)) {
		duty = (uint32_t)((float)top * conf->l_max_duty);
	}

	curr_samp_volt = 0;

	if (conf->motor_type == MOTOR_TYPE_DC) {
		curr1_sample = top - 10; // Not used anyway
		curr2_sample = top - 10;
#ifdef HW_HAS_3_SHUNTS
		curr3_sample = top - 10;
#endif

		if (duty > 1000) {
			val_sample = duty / 2;
		} else {
			val_sample = duty + 800;
			curr_samp_volt = (1 << 0) | (1 << 1) | (1 << 2);
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
#ifdef HW_HAS_3_SHUNTS
			curr3_sample = (top - duty) / 2 + duty;
#endif
		} else {
			if (conf->pwm_mode == PWM_MODE_BIPOLAR) {
				uint32_t samp_neg = top - 2;
				uint32_t samp_pos = duty + (top - duty) / 2;
				uint32_t samp_zero = top - 2;

				// Voltage and other sampling
				val_sample = top / 4;

				// Current sampling
				// TODO: Adapt for 3 shunts
#ifdef HW_HAS_3_SHUNTS
				curr3_sample = samp_zero;
#endif

				switch (comm_step) {
				case 1:
					if (direction) {
						curr1_sample = samp_zero;
						curr2_sample = samp_neg;
						curr_samp_volt = (1 << 1);
					} else {
						curr1_sample = samp_zero;
						curr2_sample = samp_pos;
					}
					break;

				case 2:
					if (direction) {
						curr1_sample = samp_pos;
						curr2_sample = samp_neg;
						curr_samp_volt = (1 << 1);
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
						curr_samp_volt = (1 << 1);
					}
					break;

				case 4:
					if (direction) {
						curr1_sample = samp_zero;
						curr2_sample = samp_pos;
					} else {
						curr1_sample = samp_zero;
						curr2_sample = samp_neg;
						curr_samp_volt = (1 << 1);
					}
					break;

				case 5:
					if (direction) {
						curr1_sample = samp_neg;
						curr2_sample = samp_pos;
						curr_samp_volt = (1 << 0);
					} else {
						curr1_sample = samp_neg;
						curr2_sample = samp_zero;
						curr_samp_volt = (1 << 0);
					}
					break;

				case 6:
					if (direction) {
						curr1_sample = samp_neg;
						curr2_sample = samp_zero;
						curr_samp_volt = (1 << 0);
					} else {
						curr1_sample = samp_neg;
						curr2_sample = samp_pos;
						curr_samp_volt = (1 << 0);
					}
					break;
				}
			} else {
				// Voltage samples
				val_sample = duty / 2;

				// Current samples
				curr1_sample = duty + (top - duty) / 2;
				if (curr1_sample > (top - 70)) {
					curr1_sample = top - 70;
				}

				curr2_sample = curr1_sample;
#ifdef HW_HAS_3_SHUNTS
				curr3_sample = curr1_sample;
#endif

				// The off sampling time is short, so use the on sampling time
				// where possible
				if (duty > (top / 2)) {
#if CURR1_DOUBLE_SAMPLE
					if (comm_step == 2 || comm_step == 3) {
						curr1_sample = duty + 90;
						curr2_sample = top - 230;
					}
#endif

#if CURR2_DOUBLE_SAMPLE
					if (direction) {
						if (comm_step == 4 || comm_step == 5) {
							curr1_sample = duty + 90;
							curr2_sample = top - 230;
						}
					} else {
						if (comm_step == 1 || comm_step == 6) {
							curr1_sample = duty + 90;
							curr2_sample = top - 230;
						}
					}
#endif

#ifdef HW_HAS_3_SHUNTS
					if (direction) {
						switch (comm_step) {
						case 1: curr_samp_volt = (1 << 0) || (1 << 2); break;
						case 2: curr_samp_volt = (1 << 1) || (1 << 2); break;
						case 3: curr_samp_volt = (1 << 1) || (1 << 2); break;
						case 4: curr_samp_volt = (1 << 0) || (1 << 1); break;
						case 5: curr_samp_volt = (1 << 0) || (1 << 1); break;
						case 6: curr_samp_volt = (1 << 0) || (1 << 2); break;
						default: break;
						}
					} else {
						switch (comm_step) {
						case 1: curr_samp_volt = (1 << 0) || (1 << 1); break;
						case 2: curr_samp_volt = (1 << 1) || (1 << 2); break;
						case 3: curr_samp_volt = (1 << 1) || (1 << 2); break;
						case 4: curr_samp_volt = (1 << 0) || (1 << 2); break;
						case 5: curr_samp_volt = (1 << 0) || (1 << 2); break;
						case 6: curr_samp_volt = (1 << 0) || (1 << 1); break;
						default: break;
						}
					}
#else
					if (direction) {
						switch (comm_step) {
						case 1: curr_samp_volt = (1 << 0) || (1 << 1); break;
						case 2: curr_samp_volt = (1 << 1); break;
						case 3: curr_samp_volt = (1 << 1); break;
						case 4: curr_samp_volt = (1 << 0); break;
						case 5: curr_samp_volt = (1 << 0); break;
						case 6: curr_samp_volt = (1 << 0) || (1 << 1); break;
						default: break;
						}
					} else {
						switch (comm_step) {
						case 1: curr_samp_volt = (1 << 0); break;
						case 2: curr_samp_volt = (1 << 1); break;
						case 3: curr_samp_volt = (1 << 1); break;
						case 4: curr_samp_volt = (1 << 0) || (1 << 1); break;
						case 5: curr_samp_volt = (1 << 0) || (1 << 1); break;
						case 6: curr_samp_volt = (1 << 0); break;
						default: break;
						}
					}
#endif
				}
			}
		}
	}

	timer_tmp->val_sample = val_sample;
	timer_tmp->curr1_sample = curr1_sample;
	timer_tmp->curr2_sample = curr2_sample;
#ifdef HW_HAS_3_SHUNTS
	timer_tmp->curr3_sample = curr3_sample;
#endif
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
		rpm_dep.time_at_comm += timer_seconds_elapsed_since(rpm_timer_start);
		rpm_timer_start = timer_time_now();
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
	if (conf->sensor_mode == SENSOR_MODE_SENSORLESS ||
			(conf->sensor_mode == SENSOR_MODE_HYBRID &&
					fabsf(mcpwm_get_rpm()) > conf->hall_sl_erpm)) {
		sensorless_now = true;
	} else {
		sensorless_now = false;
	}
}

static void commutate(int steps) {
	last_pwm_cycles_sum = pwm_cycles_sum;
	last_pwm_cycles_sums[comm_step - 1] = pwm_cycles_sum;
	pwm_cycles_sum = 0;
	pwm_cycles = 0;

	if (conf->motor_type == MOTOR_TYPE_BLDC && sensorless_now) {
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
	conf->comm_mode = comm_mode_next;
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
#ifdef HW_HAS_3_SHUNTS
		TIM8->CCR3 = timer_struct.curr3_sample;
#endif

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
	if (conf->motor_type == MOTOR_TYPE_DC) {
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
		switch (conf->pwm_mode) {
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR1();
			ENABLE_BR2();
			ENABLE_BR3();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR1();
			ENABLE_BR3();
			ENABLE_BR2();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR2();
			ENABLE_BR1();
			ENABLE_BR3();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR3();
			ENABLE_BR1();
			ENABLE_BR2();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR3();
			ENABLE_BR1();
			ENABLE_BR2();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR2();
			ENABLE_BR1();
			ENABLE_BR3();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR1();
			ENABLE_BR3();
			ENABLE_BR2();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR1();
			ENABLE_BR2();
			ENABLE_BR3();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR2();
			ENABLE_BR3();
			ENABLE_BR1();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR3();
			ENABLE_BR2();
			ENABLE_BR1();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR3();
			ENABLE_BR2();
			ENABLE_BR1();
#endif
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
#ifdef HW_HAS_DRV8313
			DISABLE_BR2();
			ENABLE_BR3();
			ENABLE_BR1();
#endif
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
#ifdef HW_HAS_DRV8313
		DISABLE_BR1();
		DISABLE_BR2();
		DISABLE_BR3();
#endif
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
