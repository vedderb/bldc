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
#include "main.h"
#include "mcpwm.h"
#include "digital_filter.h"
#include "utils.h"
#include "ledpwm.h"
#include "comm.h"

// Private variables
static volatile int comm_step;
static volatile int direction;
static volatile int last_comm_time;
static volatile float dutycycle_set;
static volatile float dutycycle_now;
static volatile float rpm_now;
static volatile int is_using_pid;
static volatile float pid_set_rpm;
static volatile int tachometer;
static volatile int pwm_adc_cycles;
static volatile int curr0_sum;
static volatile int curr1_sum;
static volatile int curr_start_samples;
static volatile int curr0_offset;
static volatile int curr1_offset;
static volatile mc_state state;
static volatile int detect_now;
static volatile int detect_inc;
static volatile int detect_do_step;
static volatile mc_pwm_mode pwm_mode;
static volatile float last_current_sample;
static volatile float motor_current_sum;
static volatile float input_current_sum;
static volatile float motor_current_iterations;
static volatile float input_current_iterations;
static volatile float mcpwm_detect_currents_avg[6];
static volatile float mcpwm_detect_currents_avg_samples[6];
static volatile int switching_frequency_now;

#if MCPWM_IS_SENSORLESS
static volatile int start_pulses;
static volatile int closed_cycles;
static volatile float cycle_integrator;
static volatile int start_time_ms_now;
#endif

// KV FIR filter
#define KV_FIR_TAPS_BITS		7
#define KV_FIR_LEN				(1 << KV_FIR_TAPS_BITS)
#define KV_FIR_FCUT				0.02
static volatile float kv_fir_coeffs[KV_FIR_LEN];
static volatile float kv_fir_samples[KV_FIR_LEN];
static volatile int kv_fir_index = 0;

// Current FIR filter
#define CURR_FIR_TAPS_BITS		4
#define CURR_FIR_LEN			(1 << CURR_FIR_TAPS_BITS)
#define CURR_FIR_FCUT			0.15
static volatile float current_fir_coeffs[CURR_FIR_LEN];
static volatile float current_fir_samples[CURR_FIR_LEN];
static volatile int current_fir_index = 0;

// Sine table (to be generated in init)
#define SINE_TABLE_LEN		3600
static volatile float sine_table[SINE_TABLE_LEN];

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
volatile uint16_t ADC_Value[MCPWM_ADC_CHANNELS];
volatile int ADC_curr_norm_value[3];
volatile float mcpwm_detect_currents[6];
volatile int mcpwm_vzero;

// Private functions
static void set_duty_cycle(float dutyCycle);
static void set_duty_cycle_hw(float dutyCycle);
static void stop_pwm(void);
static void run_pid_controller(void);
static void set_next_comm_step(int next_step);
static void update_rpm_tacho(void);
static void update_adc_sample_pos(void);
static void set_switch_frequency_hw(int freq);

#if MCPWM_IS_SENSORLESS
static void set_open_loop(void);
static int integrate_cycle(float v_diff);
static float get_start_duty(void);
#endif

// Defines
#define ADC_CDR_ADDRESS			((uint32_t)0x40012308)
#define ENABLE_GATE()			palSetPad(GPIOC, 10)
#define DISABLE_GATE()			palClearPad(GPIOC, 10)
#define DCCAL_ON()				palSetPad(GPIOB, 12)
#define DCCAL_OFF()				palClearPad(GPIOB, 12)
#define IS_FAULT()				(!palReadPad(GPIOC, 12))
#define CYCLE_INT_START			(0)

// Threads
static WORKING_AREA(timer_thread_wa, 1024);
static msg_t timer_thread(void *arg);

void mcpwm_init(void) {
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
	is_using_pid = 0;
	pid_set_rpm = 0.0;
	tachometer = 0;
	pwm_adc_cycles = 0;
	state = MC_STATE_OFF;
	detect_now = 0;
	detect_inc = 0;
	detect_do_step = 0;
	hall_sensor_order = MCPWM_HALL_SENSOR_ORDER;
	pwm_mode = MCPWM_PWM_MODE;
	last_current_sample = 0.0;
	motor_current_sum = 0.0;
	input_current_sum = 0.0;
	motor_current_iterations = 0.0;
	input_current_iterations = 0.0;
	switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MIN;

#if MCPWM_IS_SENSORLESS
	start_pulses = 0;
	closed_cycles = 0;
	cycle_integrator = CYCLE_INT_START;
	start_time_ms_now = MCPWM_START_COMM_TIME_MS_L;
#endif

	// Create KV FIR filter
	filter_create_fir((float*)kv_fir_coeffs, KV_FIR_FCUT, KV_FIR_TAPS_BITS, 1);

	// Create current FIR filter
	filter_create_fir((float*)current_fir_coeffs, CURR_FIR_FCUT, CURR_FIR_TAPS_BITS, 1);

	// Generate sine table (Range: 0.0 - 1.0)
	for (int i = 0;i < SINE_TABLE_LEN;i++) {
		sine_table[i] = (sinf((2.0 * M_PI * (float)i) / SINE_TABLE_LEN) + 1.0) / 2.0;
	}

	// Start the timer thread
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);

	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);

	// GPIOC (ENABLE_GATE)
	palSetPadMode(GPIOC, 10,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	DISABLE_GATE();

	// GPIOB (DCCAL)
	palSetPadMode(GPIOB, 12,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

	// GPIOB (hall sensors)
	palSetPadMode(GPIOB, 6, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(GPIOB, 7, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(GPIOB, 8, PAL_MODE_INPUT_PULLUP);

	// Fault pin
	palSetPadMode(GPIOC, 12, PAL_MODE_INPUT_PULLUP);

	// TIM1 clock enable
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, ENABLE);

	// GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
	palSetPadMode(GPIOA, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 9, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 10, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOB, 13, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 15, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	// Enable the TIM1 Trigger and commutation interrupt
	NVIC_InitStructure.NVIC_IRQChannel = TIM1_TRG_COM_TIM11_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

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
	TIM_OCInitStructure.TIM_Pulse = 0;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_High;
	TIM_OCInitStructure.TIM_OCIdleState = TIM_OCIdleState_Set;
	TIM_OCInitStructure.TIM_OCNIdleState = TIM_OCNIdleState_Set;

	TIM_OC1Init(TIM1, &TIM_OCInitStructure);
	TIM_OC2Init(TIM1, &TIM_OCInitStructure);
	TIM_OC3Init(TIM1, &TIM_OCInitStructure);
	TIM_OC4Init(TIM1, &TIM_OCInitStructure);

	TIM_OC1PreloadConfig(TIM1, TIM_OCPreload_Disable);
	TIM_OC2PreloadConfig(TIM1, TIM_OCPreload_Disable);
	TIM_OC3PreloadConfig(TIM1, TIM_OCPreload_Disable);
	TIM_OC4PreloadConfig(TIM1, TIM_OCPreload_Disable);

	// Automatic Output enable, Break, dead time and lock configuration
	TIM_BDTRInitStructure.TIM_OSSRState = TIM_OSSRState_Enable;
	TIM_BDTRInitStructure.TIM_OSSIState = TIM_OSSRState_Enable;
	TIM_BDTRInitStructure.TIM_LOCKLevel = TIM_LOCKLevel_OFF;
	TIM_BDTRInitStructure.TIM_DeadTime = MCPWM_DEAD_TIME_CYCLES;
	TIM_BDTRInitStructure.TIM_Break = TIM_Break_Disable;
	TIM_BDTRInitStructure.TIM_BreakPolarity = TIM_BreakPolarity_High;
	TIM_BDTRInitStructure.TIM_AutomaticOutput = TIM_AutomaticOutput_Enable;

	TIM_BDTRConfig(TIM1, &TIM_BDTRInitStructure);
	TIM_CCPreloadControl(TIM1, ENABLE);
	TIM_ITConfig(TIM1, TIM_IT_COM, ENABLE);

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

	// DMA
	DMA_InitStructure.DMA_Channel = DMA_Channel_0;
	DMA_InitStructure.DMA_Memory0BaseAddr = (uint32_t)&ADC_Value;
	DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)ADC_CDR_ADDRESS;
	DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralToMemory;
	DMA_InitStructure.DMA_BufferSize = MCPWM_ADC_CHANNELS;
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

	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 4, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOB, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOB, 1, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 5, PAL_MODE_INPUT_ANALOG);

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
	ADC_InitStructure.ADC_NbrOfConversion = 4;

	ADC_Init(ADC1, &ADC_InitStructure);
	ADC_InitStructure.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_None;
	ADC_InitStructure.ADC_ExternalTrigConv = 0;
	ADC_Init(ADC2, &ADC_InitStructure);
	ADC_Init(ADC3, &ADC_InitStructure);

	// ADC1 regular channels 0, 5, 10, 13
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 1, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5, 2, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 3, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_13, 4, ADC_SampleTime_3Cycles);

	// ADC2 regular channels 1, 6, 11, 15
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 1, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 2, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 3, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 4, ADC_SampleTime_3Cycles);

	// ADC3 regular channels 2, 3, 12, 3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 1, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 2, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 3, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 4, ADC_SampleTime_3Cycles);

	// Enable DMA request after last transfer (Multi-ADC mode)
	ADC_MultiModeDMARequestAfterLastTransferCmd(ENABLE);

	// Injected channels for current measurement at end of cycle
	ADC_ExternalTrigInjectedConvConfig(ADC1, ADC_ExternalTrigInjecConv_T1_CC4);
	ADC_ExternalTrigInjectedConvConfig(ADC2, ADC_ExternalTrigInjecConv_T8_CC2);
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC1, ADC_ExternalTrigInjecConvEdge_Falling);
	ADC_ExternalTrigInjectedConvEdgeConfig(ADC2, ADC_ExternalTrigInjecConvEdge_Falling);
	ADC_InjectedSequencerLengthConfig(ADC1, 1);
	ADC_InjectedSequencerLengthConfig(ADC2, 1);
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_6, 1, ADC_SampleTime_3Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_5, 1, ADC_SampleTime_3Cycles);

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
	TIM_OCInitStructure.TIM_Pulse = 100;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_High;
	TIM_OCInitStructure.TIM_OCIdleState = TIM_OCIdleState_Set;
	TIM_OCInitStructure.TIM_OCNIdleState = TIM_OCNIdleState_Set;
	TIM_OC1Init(TIM8, &TIM_OCInitStructure);
	TIM_OC1PreloadConfig(TIM8, TIM_OCPreload_Disable);
	TIM_OC2Init(TIM8, &TIM_OCInitStructure);
	TIM_OC2PreloadConfig(TIM8, TIM_OCPreload_Disable);

	// PWM outputs have to be enabled in order to trigger ADC on CCx
	TIM_CtrlPWMOutputs(TIM8, ENABLE);

	// TIM1 Master and TIM8 slave
	TIM_SelectOutputTrigger(TIM1, TIM_TRGOSource_Enable);
	TIM_SelectMasterSlaveMode(TIM1, TIM_MasterSlaveMode_Enable);
	TIM_SelectMasterSlaveMode(TIM8, TIM_MasterSlaveMode_Enable);
	TIM_SelectInputTrigger(TIM8, TIM_TS_ITR0);
	TIM_SelectSlaveMode(TIM8, TIM_SlaveMode_Gated);

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

	// Sample injected channels at end of PWM cycle
	TIM1->CCR4 = TIM1->ARR - 300;
	TIM8->CCR2 = TIM1->ARR - 300;

	// Calibrate current offset
	ENABLE_GATE();
	chThdSleepMilliseconds(100);
	DCCAL_ON();
	curr0_sum = 0;
	curr1_sum = 0;
	curr_start_samples = 0;
	while(curr_start_samples < 5000) {};
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
}

void mcpwm_set_duty(float dutyCycle) {
	if (dutyCycle > MCPWM_MAX_DUTY_CYCLE) {
		dutyCycle = MCPWM_MAX_DUTY_CYCLE;
	} else if (dutyCycle < -MCPWM_MAX_DUTY_CYCLE) {
		dutyCycle = -MCPWM_MAX_DUTY_CYCLE;
	}

	if (state == MC_STATE_DETECTING) {
		state = MC_STATE_OFF;
		stop_pwm();
	}

	dutycycle_set = dutyCycle;

	if (state != MC_STATE_RUNNING && fabsf(dutyCycle) > MCPWM_MIN_DUTY_CYCLE) {
		if (dutyCycle > 0.0) {
			dutycycle_now = (MCPWM_MIN_DUTY_CYCLE + 0.01);
		} else {
			dutycycle_now = -(MCPWM_MIN_DUTY_CYCLE + 0.01);
		}
		set_duty_cycle(dutycycle_now);
	}
}

void mcpwm_use_pid(int use_pid) {
	if (use_pid != is_using_pid && state == MC_STATE_DETECTING) {
		state = MC_STATE_OFF;
		stop_pwm();
	}

	is_using_pid = use_pid;
}

void mcpwm_set_pid_speed(float rpm) {
	pid_set_rpm = rpm;
}

int mcpwm_get_comm_step(void) {
	return comm_step;
}

float mcpwm_get_duty_cycle(void) {
	return dutycycle_set;
}

float mcpwm_get_rpm(void) {
	return direction ? rpm_now : -rpm_now;
}

float mcpwm_get_kv(void) {
	return rpm_now / (GET_INPUT_VOLTAGE * mcpwm_get_duty_cycle());
}

mc_state mcpwm_get_state(void) {
	return state;
}

float mcpwm_get_kv_filtered(void) {
	float value = filter_run_fir_iteration((float*)kv_fir_samples,
			(float*)kv_fir_coeffs, KV_FIR_TAPS_BITS, kv_fir_index);

	return value;
}

float mcpwm_get_tot_current_filtered(void) {
	float value = filter_run_fir_iteration((float*)current_fir_samples,
			(float*)current_fir_coeffs, CURR_FIR_TAPS_BITS, current_fir_index);

	value *= (3.3 / 4095.0) / (0.001 * 10.0);
	return value;
}

float mcpwm_get_tot_current(void) {
	return last_current_sample * (3.3 / 4095.0) / (0.001 * 10.0);
}

float mcpwm_get_tot_current_in(void) {
	return mcpwm_get_tot_current() * dutycycle_now;
}

int mcpwm_get_tachometer_value(int reset) {
	int val = tachometer;

	if (reset) {
		tachometer = 0;
	}

	return val;
}

static void stop_pwm(void) {
	dutycycle_set = 0;
	dutycycle_now = 0;

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
}

void mcpwm_full_brake(void) {
	state = MC_STATE_FULL_BRAKE;

	dutycycle_set = 0;
	dutycycle_now = 0;

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
}

#if MCPWM_IS_SENSORLESS
static float get_start_duty(void) {
	float ret = 0.0;
	float ratio = utils_calc_ratio(MCPWM_START_COMM_TIME_MS_L,
			MCPWM_START_COMM_TIME_MS_H, start_time_ms_now);

	if (direction) {
		ret = MCPWM_START_DUTY_CYCLE_L * (1 - ratio)
				+ MCPWM_START_DUTY_CYCLE_H * ratio;
	} else {
		ret = -(MCPWM_START_DUTY_CYCLE_REV_L * (1 - ratio) + MCPWM_START_DUTY_CYCLE_REV_H * ratio);
	}

	ret *= 20.0 / GET_BRIDGE_VOLTAGE;

	return ret;
}

static void set_open_loop(void) {
	start_pulses = 0;
	state = MC_STATE_STARTING;
	closed_cycles = 0;
	cycle_integrator = CYCLE_INT_START;
	start_time_ms_now = MCPWM_START_COMM_TIME_MS_L;

	dutycycle_now = get_start_duty();
	set_duty_cycle(dutycycle_now);
}
#endif

static void set_duty_cycle(float dutyCycle) {
	if (dutyCycle > MCPWM_MIN_DUTY_CYCLE) {
		direction = 1;
	} else if (dutyCycle < -MCPWM_MIN_DUTY_CYCLE) {
		dutyCycle = -dutyCycle;
		direction = 0;
	}

	if (dutyCycle < MCPWM_MIN_DUTY_CYCLE) {
		switch (state) {
		case MC_STATE_STARTING:
		case MC_STATE_RUNNING:
			state = MC_STATE_OFF;
			if (MCPWM_FULL_BRAKE_AT_STOP) {
				mcpwm_full_brake();
			} else {
				stop_pwm();
			}
			break;

		case MC_STATE_DETECTING:
			state = MC_STATE_OFF;
			stop_pwm(); // TODO: Full break?
			break;

		default:
			break;
		}

		dutycycle_set = dutyCycle;
		return;
	} else if (dutyCycle > MCPWM_MAX_DUTY_CYCLE) {
		dutyCycle = MCPWM_MAX_DUTY_CYCLE;
	}

#if MCPWM_IS_SENSORLESS
	if (state != MC_STATE_RUNNING && state != MC_STATE_STARTING) {
		set_open_loop();
	}
#else
	if (state != MC_STATE_RUNNING) {
		state = MC_STATE_RUNNING;
		set_next_comm_step(mcpwm_read_hall_phase());
		TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
	}
#endif

	set_duty_cycle_hw(dutyCycle);
}

static void set_duty_cycle_hw(float dutyCycle) {
	if (dutyCycle < MCPWM_MIN_DUTY_CYCLE) {
		dutyCycle = MCPWM_MIN_DUTY_CYCLE;
	} else if (dutyCycle > MCPWM_MAX_DUTY_CYCLE) {
		dutyCycle = MCPWM_MAX_DUTY_CYCLE;
	}

	uint16_t period;
	if (pwm_mode == PWM_MODE_BIPOLAR && state != MC_STATE_DETECTING) {
		period = (uint16_t)(((float)TIM1->ARR / 2.0) * dutyCycle + ((float)TIM1->ARR / 2.0));
	} else {
		period = (uint16_t)((float)TIM1->ARR * dutyCycle);
	}

	TIM1->CCR1 = period;
	TIM1->CCR2 = period;
	TIM1->CCR3 = period;

	if (state == MC_STATE_DETECTING) {
		switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MAX;
	} else {
		switching_frequency_now = MCPWM_SWITCH_FREQUENCY_MIN * (1.0 - fabsf(dutyCycle)) +
				MCPWM_SWITCH_FREQUENCY_MAX * fabsf(dutyCycle);
	}

	set_switch_frequency_hw(switching_frequency_now);
}

static void set_switch_frequency_hw(int freq) {
	TIM_Cmd(TIM1, DISABLE);
	TIM1->ARR = 168000000 / freq;
	TIM8->ARR = 168000000 / freq;
	TIM_Cmd(TIM1, ENABLE);
	update_adc_sample_pos();
}

static void run_pid_controller(void) {
	static float i_term = 0;
	static float prev_error = 0;
	float p_term;
	float d_term;

	// PID is off. Return.
	if (!is_using_pid) {
		i_term = 0;
		prev_error = 0;
		return;
	}

	// Too low RPM set. Stop and return.
	if (fabsf(pid_set_rpm) < MCPWM_PID_MIN_RPM) {
		i_term = 0;
		prev_error = 0;
		mcpwm_set_duty(0.0);
		return;
	}

#if MCPWM_IS_SENSORLESS
	// Start sequence running. Return.
	if (state == MC_STATE_STARTING || closed_cycles < MCPWM_CLOSED_STARTPWM_COMMS) {
		i_term = 0;
		prev_error = 0;
		return;
	}
#endif

	// Compensation for supply voltage variations
	float scale = 1.0 / GET_INPUT_VOLTAGE;

	// Compute error
	float error = pid_set_rpm - mcpwm_get_rpm();

	// Compute parameters
	p_term = error * MCPWM_PID_KP * scale;
	i_term += error * (MCPWM_PID_KI * MCPWM_PID_TIME_K) * scale;
	d_term = (error - prev_error) * (MCPWM_PID_KD / MCPWM_PID_TIME_K) * scale;

	// I-term wind-up protection
	if (i_term > 1.0) {
		i_term = 1.0;
	} else if (i_term < -1.0) {
		i_term = -1.0;
	}

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
	if (pid_set_rpm > 0.0 && output < 0.0) {
		output = MCPWM_MIN_DUTY_CYCLE;
		i_term = 0.0;
	} else if (pid_set_rpm < 0.0 && output > 0.0) {
		output = -MCPWM_MIN_DUTY_CYCLE;
		i_term = 0.0;
	}

	mcpwm_set_duty(output);
}

static msg_t timer_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("mcpwm timer");

	for(;;) {
		// Update RPM in case it has slowed down
		uint32_t tim_val = TIM2->CNT;
		uint32_t tim_diff = tim_val - last_comm_time;

#if MCPWM_IS_SENSORLESS
		static int start_time_cnt = 0;
#endif

		if (tim_diff > 0) {
			float rpm_tmp = ((float)MCPWM_AVG_COM_RPM * 1000000.0 * 60.0) /
					((float)tim_diff *  (float)MCPWM_NUM_POLES * 3.0);

			// Re-calculate RPM between commutations
			// This will end up being used when slowing down
			if (rpm_tmp < rpm_now) {
				rpm_now = rpm_tmp;
			}

#if MCPWM_IS_SENSORLESS
			if (rpm_now < MCPWM_MIN_CLOSED_RPM && state == MC_STATE_RUNNING && closed_cycles > 40) {
				set_open_loop();
			}
#endif
		}

#if MCPWM_IS_SENSORLESS
		// Duty-cycle, detect and startup
		static int start_time = 0;

		if (state != MC_STATE_STARTING) {
			if (state == MC_STATE_RUNNING) {
				if (closed_cycles >= MCPWM_CLOSED_STARTPWM_COMMS) {
					start_time_ms_now = MCPWM_START_COMM_TIME_MS_L;
				}
			} else {
				start_time_ms_now = MCPWM_START_COMM_TIME_MS_L;
			}
		}
#endif

		switch (state) {
		case MC_STATE_OFF:
			stop_pwm();
			break;

		case MC_STATE_DETECTING:
			detect_do_step = 1;
			break;

#if MCPWM_IS_SENSORLESS
		case MC_STATE_STARTING:
			start_time++;

			if (start_time >= start_time_ms_now) {
				start_time = 0;
				start_pulses++;
				TIM_GenerateEvent(TIM1, TIM_EventSource_COM);

				start_time_cnt++;
				if (start_time_cnt > 6) {
					start_time_cnt = 0;
					start_time_ms_now++;
					if (start_time_ms_now > MCPWM_START_COMM_TIME_MS_H) {
						start_time_ms_now = MCPWM_START_COMM_TIME_MS_L;
					}

					dutycycle_now = get_start_duty();
					set_duty_cycle(dutycycle_now);
				}
			}
			break;
#endif

		case MC_STATE_RUNNING:
#if MCPWM_IS_SENSORLESS
			start_time = 0;
#endif
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
			}
		}

		chThdSleepMilliseconds(1);
	}

	return 0;
}

void mcpwm_adc_inj_int_handler(void) {
	TIM4->CNT = 0;

	int curr0 = ADC_GetInjectedConversionValue(ADC1, ADC_InjectedChannel_1);
	int curr1 = ADC_GetInjectedConversionValue(ADC2, ADC_InjectedChannel_1);

	curr0_sum += curr0;
	curr1_sum += curr1;
	curr_start_samples++;

	ADC_curr_norm_value[0] = curr0 - curr0_offset;
	ADC_curr_norm_value[1] = curr1 - curr1_offset;
	ADC_curr_norm_value[2] = -(ADC_curr_norm_value[0] + ADC_curr_norm_value[1]);

	// Run current FIR filter
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

	filter_add_sample((float*)current_fir_samples, curr_tot_sample,
			CURR_FIR_TAPS_BITS, (uint32_t*)&current_fir_index);

	last_inj_adc_isr_duration = (float)TIM4->CNT / 10000000;
}

/*
 * New ADC samples ready. Do commutation!
 */
void mcpwm_adc_int_handler(void *p, uint32_t flags) {
	(void)p;
	(void)flags;

	TIM4->CNT = 0;

	mcpwm_vzero = (ADC_V_L1 + ADC_V_L2 + ADC_V_L3) / 3;

#if MCPWM_IS_SENSORLESS
	// See if current RPM is large enough to consider it updated,
	// otherwise use low enough RPM value
	float div_rpm = rpm_now;

	if (div_rpm < (float)MCPWM_MIN_CLOSED_RPM) {
		div_rpm = (float)MCPWM_MIN_CLOSED_RPM;
	}

	// Compute the theoretical commutation time at the current RPM
	float comm_time = ((float)switching_frequency_now) /
			((div_rpm / 60.0) * (float)MCPWM_NUM_POLES * 3.0);

	if (pwm_adc_cycles >= (int)(comm_time * (1.0 / MCPWM_COMM_RPM_FACTOR))) {
		if (state == MC_STATE_RUNNING) {
			TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
			closed_cycles++;
			cycle_integrator = CYCLE_INT_START;
		}
	}

	if (pwm_mode == PWM_MODE_BIPOLAR) {
		comm_time = 3.0 / MCPWM_COMM_RPM_FACTOR;
	}

	if (pwm_adc_cycles >= (int)(comm_time * MCPWM_COMM_RPM_FACTOR)) {
		int inc_step = 0;
		int ph1, ph2, ph3;
		int v_diff = 0;

		if (direction) {
			ph1 = ADC_V_L1 - mcpwm_vzero;
			ph2 = ADC_V_L2 - mcpwm_vzero;
			ph3 = ADC_V_L3 - mcpwm_vzero;
		} else {
			ph1 = ADC_V_L1 - mcpwm_vzero;
			ph2 = ADC_V_L3 - mcpwm_vzero;
			ph3 = ADC_V_L2 - mcpwm_vzero;
		}

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
			const int ratio = (100 * v_diff) / mcpwm_vzero;

			if (state == MC_STATE_STARTING && ratio < MCPWM_MAX_COMM_START_DIFF
					&& start_pulses > MCPWM_MIN_START_STEPS) {
				// We think we are running in closed loop. Stop start sequence!
				state = MC_STATE_RUNNING;
				cycle_integrator = CYCLE_INT_START;
			} else if (state == MC_STATE_RUNNING || state == MC_STATE_OFF) {
				integrate_cycle((float)v_diff);
			}
		} else {
			cycle_integrator = CYCLE_INT_START;
		}
	}

	pwm_adc_cycles++;
#else
	int hall_phase = mcpwm_read_hall_phase();
	if (comm_step != hall_phase) {
		comm_step = hall_phase;

		update_rpm_tacho();

		if (state == MC_STATE_RUNNING) {
			set_next_comm_step(hall_phase);
			TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
		}
	}
#endif

	if (state == MC_STATE_RUNNING) {
		const float ramp_step = MCPWM_RAMP_STEP / (switching_frequency_now / 1000.0);
		const float current = mcpwm_get_tot_current();
		const float current_in = mcpwm_get_tot_current_in();

		float dutycycle_set_tmp = dutycycle_set;

#if MCPWM_IS_SENSORLESS
		if (closed_cycles < MCPWM_CLOSED_STARTPWM_COMMS) {
			dutycycle_set_tmp = get_start_duty();
		}
#endif

		motor_current_sum += current;
		input_current_sum += current_in;
		motor_current_iterations++;
		input_current_iterations++;

		if (current > MCPWM_CURRENT_MAX) {
			step_towards((float*) &dutycycle_now, 0.0,
					ramp_step * fabsf(current - MCPWM_CURRENT_MAX));
		} else if (current < MCPWM_CURRENT_MIN) {
			step_towards((float*) &dutycycle_now, direction ? MCPWM_MAX_DUTY_CYCLE : -MCPWM_MAX_DUTY_CYCLE, ramp_step * 0.5);
		} else if (fabsf(current_in) > MCPWM_IN_CURRENT_LIMIT) {
			step_towards((float*) &dutycycle_now, 0.0,
					ramp_step * fabsf(current_in - MCPWM_IN_CURRENT_LIMIT));
		} else {
			step_towards((float*)&dutycycle_now, dutycycle_set_tmp, ramp_step);
		}

		// When the set duty cycle is in the opposite direction, make sure that the motor
		// starts again after stopping completely
		if (fabsf(dutycycle_now) <= MCPWM_MIN_DUTY_CYCLE) {
			if (dutycycle_set_tmp > MCPWM_MIN_DUTY_CYCLE) {
				dutycycle_now = (MCPWM_MIN_DUTY_CYCLE + 0.001);
#if MCPWM_IS_SENSORLESS
				direction = 1;
				set_open_loop();
#endif
			} else if (dutycycle_set_tmp < -MCPWM_MIN_DUTY_CYCLE) {
				dutycycle_now = -(MCPWM_MIN_DUTY_CYCLE + 0.001);
#if MCPWM_IS_SENSORLESS
				direction = 0;
				set_open_loop();
#endif
			}
		}

		set_duty_cycle(dutycycle_now);
	}

	main_dma_adc_handler();

	last_adc_isr_duration = (float)TIM4->CNT / 10000000;
}

void mcpwm_set_detect(void) {
	is_using_pid = 0;
	stop_pwm();
	set_switch_frequency_hw(MCPWM_SWITCH_FREQUENCY_MAX);

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

float mcpwm_get_dutycycle_now(void) {
	return dutycycle_now;
}

float mcpwm_get_last_adc_isr_duration(void) {
	return last_adc_isr_duration;
}

float mcpwm_get_last_inj_adc_isr_duration(void) {
	return last_inj_adc_isr_duration;
}

#if MCPWM_IS_SENSORLESS
static int integrate_cycle(float v_diff) {
	cycle_integrator += v_diff;
	const float limit = (MCPWM_CYCLE_INT_LIMIT * 0.0005) * (float)switching_frequency_now;

	if (cycle_integrator >= limit) {
		TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
		closed_cycles++;
		cycle_integrator = CYCLE_INT_START;
		return 1;
	}

	return 0;
}
#endif

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
	if (!direction && tmp_phase >= 0) {
		signed int p_tmp = tmp_phase;
		p_tmp += 4;
		if (p_tmp < 0) {
			p_tmp += 6;
		} else if (p_tmp > 5) {
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

static void update_adc_sample_pos(void) {
	uint32_t period = TIM1->CCR1;

	// Sample the ADC at an appropriate time during the pwm cycle
	if (state == MC_STATE_DETECTING) {
		// Voltage samples
		TIM8->CCR1 = 200;

		// Current samples
		TIM1->CCR4 = (TIM1->ARR - period) / 2 + period;
		TIM8->CCR2 = (TIM1->ARR - period) / 2 + period;
	} else {
		if (pwm_mode == PWM_MODE_BIPOLAR) {
			uint32_t samp_neg = period - (TIM1->ARR - period) / 2;
			uint32_t samp_pos = period + (TIM1->ARR - period) / 2;
			uint32_t samp_zero = TIM1->ARR - 2;

			// Voltage and other sampling
			TIM8->CCR1 = 200;

			// Current sampling
			switch (comm_step) {
			case 1:
				if (direction) {
					TIM1->CCR4 = samp_zero;
					TIM8->CCR2 = samp_neg;
				} else {
					TIM1->CCR4 = samp_zero;
					TIM8->CCR2 = samp_pos;
				}
				break;

			case 2:
				if (direction) {
					TIM1->CCR4 = samp_pos;
					TIM8->CCR2 = samp_neg;
				} else {
					TIM1->CCR4 = samp_pos;
					TIM8->CCR2 = samp_zero;
				}
				break;

			case 3:
				if (direction) {
					TIM1->CCR4 = samp_pos;
					TIM8->CCR2 = samp_zero;
				} else {
					TIM1->CCR4 = samp_pos;
					TIM8->CCR2 = samp_neg;
				}
				break;

			case 4:
				if (direction) {
					TIM1->CCR4 = samp_zero;
					TIM8->CCR2 = samp_pos;
				} else {
					TIM1->CCR4 = samp_zero;
					TIM8->CCR2 = samp_neg;
				}
				break;

			case 5:
				if (direction) {
					TIM1->CCR4 = samp_neg;
					TIM8->CCR2 = samp_pos;
				} else {
					TIM1->CCR4 = samp_neg;
					TIM8->CCR2 = samp_zero;
				}
				break;

			case 6:
				if (direction) {
					TIM1->CCR4 = samp_neg;
					TIM8->CCR2 = samp_zero;
				} else {
					TIM1->CCR4 = samp_neg;
					TIM8->CCR2 = samp_pos;
				}
				break;
			}
		} else {
			TIM8->CCR1 = period / 2;

			// TODO: WTF??
//			const uint32_t low_samp = 215;
//			const uint32_t norm_samp = period / 2;
//			const uint32_t low = TIM1->ARR / 12;
//			const uint32_t high = TIM1->ARR / 8;
//
//			if (period <= low) {
//				TIM8->CCR1 = low_samp;
//			} else if (period < high) {
//				float ratio = utils_calc_ratio(low, high, period);
//				TIM8->CCR1 = (uint32_t) ((float) low_samp * (1.0 - ratio)
//						+ (float) norm_samp * ratio);
//			} else {
//				TIM8->CCR1 = norm_samp;
//			}

			// Current samples
			TIM1->CCR4 = period + (TIM1->ARR - period) / 2;
			TIM8->CCR2 = period + (TIM1->ARR - period) / 2;
		}
	}
}

static void update_rpm_tacho(void) {
	pwm_adc_cycles = 0;

	static uint32_t comm_counter = 0;
	comm_counter++;

	if (comm_counter == MCPWM_AVG_COM_RPM) {
		comm_counter = 0;
		uint32_t tim_val = TIM2->CNT;
		uint32_t tim_diff = tim_val - last_comm_time;
		last_comm_time = tim_val;

		if (tim_diff > 0) {
			rpm_now = ((float)MCPWM_AVG_COM_RPM * 1000000.0 * 60.0) /
					((float)tim_diff *  (float)MCPWM_NUM_POLES * 3.0);
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

	// Tachometer
	if (direction) {
		tachometer += tacho_diff;
	} else {
		tachometer -= tacho_diff;
	}
}

void mcpwm_comm_int_handler(void) {
#if MCPWM_IS_SENSORLESS
	// PWM commutation in advance for next step.

	if (!(state == MC_STATE_STARTING || state == MC_STATE_RUNNING)) {
		return;
	}

	update_rpm_tacho();

	comm_step++;
	if (comm_step > 6) {
		comm_step = 1;
	}

	int next_step = comm_step + 1;
	if (next_step > 6) {
		next_step = 1;
	}

	set_next_comm_step(next_step);
#endif
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

	update_adc_sample_pos();
}
