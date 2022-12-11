/*
	Copyright 2018 Benjamin Vedder	benjamin@vedder.se

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

#include "gpdrive.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "digital_filter.h"
#include "utils_math.h"
#include "utils_sys.h"
#include "ledpwm.h"
#include "terminal.h"
#include "commands.h"
#include "timeout.h"
#include "mc_interface.h"
#include "timer.h"
#include <math.h>
#include <string.h>
#include <stdlib.h>

// Settings
#define SAMPLE_BUFFER_SIZE			1024

// Private types
typedef struct {
	float current_set;
	float voltage_now;
	float voltage_int;
} cc_state;

typedef struct {
	float buffer[SAMPLE_BUFFER_SIZE];
	int read;
	int write;
} sample_buffer;

// Private variables
static volatile mc_configuration *m_conf;
static volatile float m_fsw_now;
static volatile float m_mod_now;
static volatile float m_current_now;
static volatile float m_current_now_filtered;
static volatile bool m_init_done = false;
static volatile gpd_output_mode m_output_mode;
static volatile sample_buffer m_sample_buffer;
static volatile float m_buffer_int_scale;
static volatile bool m_is_running;
static volatile float m_output_now;
static volatile bool m_dccal_done = false;
static volatile int m_curr_samples;
static volatile int m_curr0_sum;
static volatile int m_curr1_sum;
static volatile int m_curr0_offset;
static volatile int m_curr1_offset;
#ifdef HW_HAS_3_SHUNTS
static volatile int m_curr2_sum;
static volatile int m_curr2_offset;
#endif
static volatile float m_last_adc_isr_duration;
static volatile cc_state m_current_state;

// Private functions
static void stop_pwm_hw(void);
static void adc_int_handler(void *p, uint32_t flags);
static void set_modulation(float mod);
static void do_dc_cal(void);

// Threads
static THD_WORKING_AREA(timer_thread_wa, 512);
static THD_FUNCTION(timer_thread, arg);
static volatile bool timer_thd_stop;

void gpdrive_init(volatile mc_configuration *configuration) {
	utils_sys_lock_cnt();

	m_init_done = false;

	// Restore timers
	TIM_DeInit(TIM1);
	TIM1->CNT = 0;

	// Disable channel 2 pins
	palSetPadMode(GPIOA, 9, PAL_MODE_OUTPUT_PUSHPULL);
	palClearPad(GPIOA, 9);
	palSetPadMode(GPIOB, 14, PAL_MODE_OUTPUT_PUSHPULL);
	palClearPad(GPIOB, 14);

	m_conf = configuration;
	m_fsw_now = 40000;
	m_mod_now = 0.0;
	m_current_now = 0.0;
	m_current_now_filtered = 0.0;
	m_output_mode = GPD_OUTPUT_MODE_NONE;
	memset((void*)&m_sample_buffer, 0, sizeof(m_sample_buffer));
	m_buffer_int_scale = 1.0 / 128.0;
	m_is_running = false;
	m_output_now = 0.0;
	m_curr0_sum = 0;
	m_curr1_sum = 0;
	m_curr_samples = 0;
	m_curr0_offset = 0;
	m_curr1_offset = 0;
	m_dccal_done = false;
#ifdef HW_HAS_3_SHUNTS
	m_curr2_sum = 0;
	m_curr2_offset = 0;
#endif
	m_last_adc_isr_duration = 0;
	memset((void*)&m_current_state, 0, sizeof(m_current_state));

	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	TIM_BDTRInitTypeDef TIM_BDTRInitStructure;

	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, ENABLE);

	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = SYSTEM_CORE_CLOCK / (int)m_fsw_now;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(TIM1, &TIM_TimeBaseStructure);

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
			(stm32_dmaisr_t)adc_int_handler,
			(void *)0);

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

	// Note that the ADC is running at 42MHz, which is higher than the
	// specified 36MHz in the data sheet, but it works.
	ADC_CommonInitStructure.ADC_Mode = ADC_TripleMode_RegSimult;
	ADC_CommonInitStructure.ADC_Prescaler = ADC_Prescaler_Div2;
	ADC_CommonInitStructure.ADC_DMAAccessMode = ADC_DMAAccessMode_1;
	ADC_CommonInitStructure.ADC_TwoSamplingDelay = ADC_TwoSamplingDelay_5Cycles;
	ADC_CommonInit(&ADC_CommonInitStructure);

	ADC_InitStructure.ADC_Resolution = ADC_Resolution_12b;
	ADC_InitStructure.ADC_ScanConvMode = ENABLE;
	ADC_InitStructure.ADC_ContinuousConvMode = DISABLE;
	ADC_InitStructure.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_Falling;
	ADC_InitStructure.ADC_ExternalTrigConv = ADC_ExternalTrigConv_T1_CC2;
	ADC_InitStructure.ADC_DataAlign = ADC_DataAlign_Right;
	ADC_InitStructure.ADC_NbrOfConversion = HW_ADC_NBR_CONV;

	ADC_Init(ADC1, &ADC_InitStructure);
	ADC_InitStructure.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_None;
	ADC_InitStructure.ADC_ExternalTrigConv = 0;
	ADC_Init(ADC2, &ADC_InitStructure);
	ADC_Init(ADC3, &ADC_InitStructure);

	ADC_TempSensorVrefintCmd(ENABLE);

	ADC_MultiModeDMARequestAfterLastTransferCmd(ENABLE);

	hw_setup_adc_channels();

	ADC_Cmd(ADC1, ENABLE);
	ADC_Cmd(ADC2, ENABLE);
	ADC_Cmd(ADC3, ENABLE);

	TIM_Cmd(TIM1, ENABLE);
	TIM_CtrlPWMOutputs(TIM1, ENABLE);

	// Always sample ADC in the beginning of the PWM cycle
	TIM1->CCR2 = 200;

	utils_sys_unlock_cnt();

	ENABLE_GATE();
	DCCAL_OFF();
	do_dc_cal();

	// Start threads
	timer_thd_stop = false;
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);

	stop_pwm_hw();

	// Check if the system has resumed from IWDG reset
	if (timeout_had_IWDG_reset()) {
		mc_interface_fault_stop(FAULT_CODE_BOOTING_FROM_WATCHDOG_RESET, false, false);
	}

	m_init_done = true;
}

void gpdrive_deinit(void) {
	if (!m_init_done) {
		return;
	}

	m_init_done = false;

	timer_thd_stop = true;

	while (timer_thd_stop) {
		chThdSleepMilliseconds(1);
	}

	TIM_DeInit(TIM1);
	TIM_DeInit(TIM12);
	ADC_DeInit();
	DMA_DeInit(DMA2_Stream4);
	nvicDisableVector(ADC_IRQn);
	dmaStreamRelease(STM32_DMA_STREAM(STM32_DMA_STREAM_ID(2, 4)));

	// Restore pins
	palSetPadMode(GPIOA, 9, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
}

bool gpdrive_init_done(void) {
	return m_init_done;
}

bool gpdrive_is_dccal_done(void) {
	return m_dccal_done;
}

float gpdrive_get_switching_frequency_now(void) {
	return m_fsw_now;
}

void gpdrive_set_configuration(volatile mc_configuration *configuration) {
	// Stop everything first to be safe
	m_output_mode = GPD_OUTPUT_MODE_NONE;
	stop_pwm_hw();

	utils_sys_lock_cnt();
	m_conf = configuration;
	utils_sys_unlock_cnt();
}

void gpdrive_output_sample(float sample) {
	m_output_now = sample;

	switch (m_output_mode) {
	case GPD_OUTPUT_MODE_MODULATION:
		set_modulation(sample);
		break;

	case GPD_OUTPUT_MODE_VOLTAGE:
		set_modulation(sample / GET_INPUT_VOLTAGE());
		break;

	case GPD_OUTPUT_MODE_CURRENT:
		m_current_state.current_set = sample;
		m_is_running = true;
		break;

	default:
		break;
	}
}

void gpdrive_fill_buffer(float *samples, int sample_num) {
	for (int i = 0;i < sample_num;i++) {
		m_sample_buffer.buffer[m_sample_buffer.write++] = samples[i];
		m_sample_buffer.write %= SAMPLE_BUFFER_SIZE;
	}
}

void gpdrive_add_buffer_sample(float sample) {
	m_sample_buffer.buffer[m_sample_buffer.write++] = sample;
	m_sample_buffer.write %= SAMPLE_BUFFER_SIZE;
}

void gpdrive_add_buffer_sample_int(int sample) {
	m_sample_buffer.buffer[m_sample_buffer.write++] = (float)sample * m_buffer_int_scale;
	m_sample_buffer.write %= SAMPLE_BUFFER_SIZE;
}

void gpdrive_set_buffer_int_scale(float scale) {
	m_buffer_int_scale = scale;
}

void gpdrive_set_switching_frequency(float freq) {
	m_fsw_now = freq;
	TIM1->ARR = SYSTEM_CORE_CLOCK / (int)m_fsw_now;
	set_modulation(m_mod_now);
}

int gpdrive_buffer_size_left(void) {
	return (m_sample_buffer.write > m_sample_buffer.read)
                ? m_sample_buffer.write - m_sample_buffer.read
                : SAMPLE_BUFFER_SIZE - m_sample_buffer.read +m_sample_buffer.write;
}

void gpdrive_set_mode(gpd_output_mode mode) {
	m_output_mode = mode;

	if (m_output_mode == GPD_OUTPUT_MODE_NONE) {
		stop_pwm_hw();
	}
}

float gpdrive_get_current(void) {
	return m_current_now;
}

float gpdrive_get_current_filtered(void) {
	return m_current_now_filtered;
}

float gpdrive_get_modulation(void) {
	return m_mod_now;
}

float gpdrive_get_last_adc_isr_duration(void) {
	return m_last_adc_isr_duration;
}

// Private functions

static void set_modulation(float mod) {
	utils_truncate_number_abs(&mod, m_conf->l_max_duty);
	m_mod_now = mod;

	if (m_output_mode == GPD_OUTPUT_MODE_NONE || mc_interface_get_fault() != FAULT_CODE_NONE) {
		return;
	}

	if (m_conf->pwm_mode == PWM_MODE_BIPOLAR) {
		uint32_t duty = (uint32_t) (((float)TIM1->ARR / 2.0) * mod + ((float)TIM1->ARR / 2.0));
		TIM1->CCR1 = duty;
		TIM1->CCR3 = duty;

		// +
		TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_PWM1);
		TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Enable);

		// -
		TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_PWM2);
		TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
		TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Enable);
	} else {
		uint32_t duty = (uint32_t)((float)TIM1->ARR * fabsf(mod));
		TIM1->CCR1 = duty;
		TIM1->CCR3 = duty;

		if (mod >= 0) {
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
	}

	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);

	m_is_running = true;
}

static void stop_pwm_hw(void) {
	m_is_running = false;
	m_sample_buffer.write = 0;
	m_sample_buffer.read = 0;

#ifdef HW_HAS_DRV8313
	DISABLE_BR();
#endif

	TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Disable);

	TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_ForcedAction_InActive);
	TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Disable);

	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);
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
	m_curr0_sum = 0;
	m_curr1_sum = 0;
#ifdef HW_HAS_3_SHUNTS
	m_curr2_sum = 0;
#endif
	m_curr_samples = 0;
	while(m_curr_samples < 4000) {};
	m_curr0_offset = m_curr0_sum / m_curr_samples;
	m_curr1_offset = m_curr1_sum / m_curr_samples;
#ifdef HW_HAS_3_SHUNTS
	m_curr2_offset = m_curr2_sum / m_curr_samples;
#endif
	DCCAL_OFF();
	m_dccal_done = true;
}

static void adc_int_handler(void *p, uint32_t flags) {
	(void)p;
	(void)flags;

	uint32_t t_start = timer_time_now();

	// Reset the watchdog
	timeout_feed_WDT(THREAD_MCPWM);

	int curr0 = GET_CURRENT1();
	int curr1 = GET_CURRENT2();

#ifdef HW_HAS_3_SHUNTS
	int curr2 = GET_CURRENT3();
#endif

	m_curr0_sum += curr0;
	m_curr1_sum += curr1;
#ifdef HW_HAS_3_SHUNTS
	m_curr2_sum += curr2;
#endif

	curr0 -= m_curr0_offset;
	curr1 -= m_curr1_offset;
#ifdef HW_HAS_3_SHUNTS
	curr2 -= m_curr2_offset;
#endif

	m_curr_samples++;

	// Update current
#ifdef HW_HAS_3_SHUNTS
	float i1 = -(float)curr2;
#else
	float i1 = -(float)curr1;
#endif
	float i2 = (float)curr0;

	m_current_now = utils_max_abs(i1, i2) * FAC_CURRENT;
	UTILS_LP_FAST(m_current_now_filtered, m_current_now, m_conf->gpd_current_filter_const);

	// Check for most critical faults here, as doing it in mc_interface can be too slow
	// for high switching frequencies.

	const float input_voltage = GET_INPUT_VOLTAGE();

	static int wrong_voltage_iterations = 0;
	if (input_voltage < m_conf->l_min_vin ||
			input_voltage > m_conf->l_max_vin) {
		wrong_voltage_iterations++;

		if ((wrong_voltage_iterations >= 8)) {
			mc_interface_fault_stop(input_voltage < m_conf->l_min_vin ?
					FAULT_CODE_UNDER_VOLTAGE : FAULT_CODE_OVER_VOLTAGE, false, true);
		}
	} else {
		wrong_voltage_iterations = 0;
	}

	if (m_conf->l_slow_abs_current) {
		if (fabsf(m_current_now) > m_conf->l_abs_current_max) {
			mc_interface_fault_stop(FAULT_CODE_ABS_OVER_CURRENT, false, true);
		}
	} else {
		if (fabsf(m_current_now_filtered) > m_conf->l_abs_current_max) {
			mc_interface_fault_stop(FAULT_CODE_ABS_OVER_CURRENT, false, true);
		}
	}

	// Buffer handling
	static bool buffer_was_empty = true;
	static int interpol = 0;
	static float buffer_last = 0.0;
	static float buffer_next = 0.0;

	interpol++;

	if (interpol > m_conf->gpd_buffer_interpol) {
		interpol = 0;
		if (m_sample_buffer.read != m_sample_buffer.write) {
			buffer_last = buffer_next;
			buffer_next = m_sample_buffer.buffer[m_sample_buffer.read++];
			m_sample_buffer.read %= SAMPLE_BUFFER_SIZE;

			m_output_now = buffer_last;
			m_is_running = true;

			buffer_was_empty = false;
		} else {
			if (!buffer_was_empty) {
				stop_pwm_hw();
			}

			buffer_was_empty = true;
		}
	} else if (!buffer_was_empty) {
		m_output_now = utils_map((float)interpol,
				0.0, (float)m_conf->gpd_buffer_interpol + 1.0,
			buffer_last, buffer_next);
		m_is_running = true;
	}

	if (m_is_running) {
		gpdrive_output_sample(m_output_now);

		if (m_output_mode == GPD_OUTPUT_MODE_CURRENT) {
			float v_in = GET_INPUT_VOLTAGE();
			float err = m_current_state.current_set - m_current_now_filtered;
			m_current_state.voltage_now = m_current_state.voltage_int + err * m_conf->gpd_current_kp;
			m_current_state.voltage_int += err * m_conf->gpd_current_ki * (1.0 / m_fsw_now);
			utils_truncate_number_abs((float*)&m_current_state.voltage_int, v_in);
			set_modulation(m_current_state.voltage_now / v_in);
		}
	}

	ledpwm_update_pwm();

	m_last_adc_isr_duration = timer_seconds_elapsed_since(t_start);
}

static THD_FUNCTION(timer_thread, arg) {
	(void)arg;

	chRegSetThreadName("gpdrive timer");

	for(;;) {
		if (timer_thd_stop) {
			timer_thd_stop = false;
			return;
		}

		static bool buffer_empty_before = true;
		if (gpdrive_buffer_size_left() > 0 &&
				gpdrive_buffer_size_left() < m_conf->gpd_buffer_notify_left) {
			if (!buffer_empty_before) {
				commands_send_gpd_buffer_notify();
			}
			buffer_empty_before = true;
		} else {
			buffer_empty_before = false;
		}

		chThdSleepMilliseconds(1);
	}
}
