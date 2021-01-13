/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#include "encoder.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>

// Defines
#define AS5047P_READ_ANGLECOM		(0x3FFF | 0x4000 | 0x8000) // This is just ones
#define AS5047_SAMPLE_RATE_HZ		20000
#define AD2S1205_SAMPLE_RATE_HZ		20000		//25MHz max spi clk
#define MT6816_SAMPLE_RATE_HZ		20000
#define MT6816_NO_MAGNET_ERROR_MASK	0x0002
#define SINCOS_SAMPLE_RATE_HZ		20000
#define SINCOS_MIN_AMPLITUDE		1.0			// sqrt(sin^2 + cos^2) has to be larger than this
#define SINCOS_MAX_AMPLITUDE		1.65		// sqrt(sin^2 + cos^2) has to be smaller than this

#if (AS5047_USE_HW_SPI_PINS) || (MT6816_USE_HW_SPI_PINS) || (AD2S1205_USE_HW_SPI_PINS)
#ifdef HW_SPI_DEV
#define SPI_SW_MISO_GPIO			HW_SPI_PORT_MISO
#define SPI_SW_MISO_PIN				HW_SPI_PIN_MISO
#define SPI_SW_MOSI_GPIO			HW_SPI_PORT_MOSI
#define SPI_SW_MOSI_PIN				HW_SPI_PIN_MOSI
#define SPI_SW_SCK_GPIO				HW_SPI_PORT_SCK
#define SPI_SW_SCK_PIN				HW_SPI_PIN_SCK
#define SPI_SW_CS_GPIO				HW_SPI_PORT_NSS
#define SPI_SW_CS_PIN				HW_SPI_PIN_NSS
#else
// Note: These values are hardcoded.
#define SPI_SW_MISO_GPIO			GPIOB
#define SPI_SW_MISO_PIN				4
#define SPI_SW_MOSI_GPIO			GPIOB
#define SPI_SW_MOSI_PIN				5
#define SPI_SW_SCK_GPIO				GPIOB
#define SPI_SW_SCK_PIN				3
#define SPI_SW_CS_GPIO				GPIOB
#define SPI_SW_CS_PIN				0
#endif
#else
#define SPI_SW_MISO_GPIO			HW_HALL_ENC_GPIO2
#define SPI_SW_MISO_PIN				HW_HALL_ENC_PIN2
#define SPI_SW_SCK_GPIO				HW_HALL_ENC_GPIO1
#define SPI_SW_SCK_PIN				HW_HALL_ENC_PIN1
#define SPI_SW_CS_GPIO				HW_HALL_ENC_GPIO3
#define SPI_SW_CS_PIN				HW_HALL_ENC_PIN3
#endif

// Private types
typedef enum {
	ENCODER_MODE_NONE = 0,
	ENCODER_MODE_ABI,
	ENCODER_MODE_AS5047P_SPI,
	RESOLVER_MODE_AD2S1205,
	ENCODER_MODE_SINCOS,
	ENCODER_MODE_TS5700N8501,
	ENCODER_MODE_MT6816_SPI
} encoder_mode;

// Private variables
static bool index_found = false;
static uint32_t enc_counts = 10000;
static encoder_mode mode = ENCODER_MODE_NONE;
static float last_enc_angle = 0.0;
static uint32_t spi_val = 0;
static uint32_t spi_error_cnt = 0;
static uint32_t encoder_no_magnet_error_cnt = 0;
static float spi_error_rate = 0.0;
static float encoder_no_magnet_error_rate = 0.0;
static float resolver_loss_of_tracking_error_rate = 0.0;
static float resolver_degradation_of_signal_error_rate = 0.0;
static float resolver_loss_of_signal_error_rate = 0.0;
static uint32_t resolver_loss_of_tracking_error_cnt = 0;
static uint32_t resolver_degradation_of_signal_error_cnt = 0;
static uint32_t resolver_loss_of_signal_error_cnt = 0;

static float sin_gain = 0.0;
static float sin_offset = 0.0;
static float cos_gain = 0.0;
static float cos_offset = 0.0;
static float sincos_filter_constant = 0.0;
static uint32_t sincos_signal_below_min_error_cnt = 0;
static uint32_t sincos_signal_above_max_error_cnt = 0;
static float sincos_signal_low_error_rate = 0.0;
static float sincos_signal_above_max_error_rate = 0.0;

static SerialConfig TS5700N8501_uart_cfg = {
		2500000,
		0,
		USART_CR2_LINEN,
		0
};

//                                                             SPI1        SPI2/3
#define SPI_BaudRatePrescaler_2         ((uint16_t)0x0000) //  42 MHz      21 MHZ
#define SPI_BaudRatePrescaler_4         ((uint16_t)0x0008) //  21 MHz      10.5 MHz
#define SPI_BaudRatePrescaler_8         ((uint16_t)0x0010) //  10.5 MHz    5.25 MHz
#define SPI_BaudRatePrescaler_16        ((uint16_t)0x0018) //  5.25 MHz    2.626 MHz
#define SPI_BaudRatePrescaler_32        ((uint16_t)0x0020) //  2.626 MHz   1.3125 MHz
#define SPI_BaudRatePrescaler_64        ((uint16_t)0x0028) //  1.3125 MHz  656.25 KHz
#define SPI_BaudRatePrescaler_128       ((uint16_t)0x0030) //  656.25 KHz  328.125 KHz
#define SPI_BaudRatePrescaler_256       ((uint16_t)0x0038) //  328.125 KHz 164.06 KHz
#define SPI_DATASIZE_16BIT				SPI_CR1_DFF

#ifdef HW_SPI_DEV
//MT6816 max clk freq: 15.625MHz
static const SPIConfig mt6816_spi_cfg = {
		NULL,
		SPI_SW_CS_GPIO,
		SPI_SW_CS_PIN,
		SPI_BaudRatePrescaler_4 | SPI_CR1_CPOL | SPI_CR1_CPHA | SPI_DATASIZE_16BIT};
#endif

static THD_FUNCTION(ts5700n8501_thread, arg);
static THD_WORKING_AREA(ts5700n8501_thread_wa, 512);
static volatile bool ts5700n8501_stop_now = true;
static volatile bool ts5700n8501_is_running = false;
static volatile uint8_t ts5700n8501_raw_status[8] = {0};
static volatile bool ts5700n8501_reset_errors = false;
static volatile bool ts5700n8501_reset_multiturn = false;

// Private functions
static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length);
static void spi_begin(void);
static void spi_end(void);
static void spi_delay(void);
static void TS5700N8501_send_byte(uint8_t b);

uint32_t encoder_spi_get_error_cnt(void) {
	return spi_error_cnt;
}

uint32_t encoder_spi_get_val(void) {
	return spi_val;
}

float encoder_spi_get_error_rate(void) {
	return spi_error_rate;
}

uint32_t encoder_get_no_magnet_error_cnt(void) {
	return encoder_no_magnet_error_cnt;
}

float encoder_get_no_magnet_error_rate(void) {
	return encoder_no_magnet_error_rate;
}

float encoder_resolver_loss_of_tracking_error_rate(void) {
	return resolver_loss_of_tracking_error_rate;
}

float encoder_resolver_degradation_of_signal_error_rate(void) {
	return resolver_degradation_of_signal_error_rate;
}

float encoder_resolver_loss_of_signal_error_rate(void) {
	return resolver_loss_of_signal_error_rate;
}

uint32_t encoder_resolver_loss_of_tracking_error_cnt(void) {
	return resolver_loss_of_tracking_error_cnt;
}

uint32_t encoder_resolver_degradation_of_signal_error_cnt(void) {
	return resolver_degradation_of_signal_error_cnt;
}

uint32_t encoder_resolver_loss_of_signal_error_cnt(void) {
	return resolver_loss_of_signal_error_cnt;
}

uint32_t encoder_sincos_get_signal_below_min_error_cnt(void) {
	return sincos_signal_below_min_error_cnt;
}

uint32_t encoder_sincos_get_signal_above_max_error_cnt(void) {
	return sincos_signal_above_max_error_cnt;
}

float encoder_sincos_get_signal_below_min_error_rate(void) {
	return sincos_signal_low_error_rate;
}

float encoder_sincos_get_signal_above_max_error_rate(void) {
	return sincos_signal_above_max_error_rate;
}

uint8_t* encoder_ts5700n8501_get_raw_status(void) {
	return (uint8_t*)ts5700n8501_raw_status;
}

int16_t encoder_ts57n8501_get_abm(void) {
	return (uint16_t)ts5700n8501_raw_status[4] |
			((uint16_t)ts5700n8501_raw_status[5] << 8);
}

void encoder_ts57n8501_reset_errors(void) {
	ts5700n8501_reset_errors = true;
}

void encoder_ts57n8501_reset_multiturn(void) {
	ts5700n8501_reset_multiturn = true;
}

void encoder_deinit(void) {
	nvicDisableVector(HW_ENC_EXTI_CH);
	nvicDisableVector(HW_ENC_TIM_ISR_CH);

	TIM_DeInit(HW_ENC_TIM);

	palSetPadMode(SPI_SW_MISO_GPIO, SPI_SW_MISO_PIN, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(SPI_SW_SCK_GPIO, SPI_SW_SCK_PIN, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(SPI_SW_CS_GPIO, SPI_SW_CS_PIN, PAL_MODE_INPUT_PULLUP);

#ifdef HW_SPI_DEV
	spiStop(&HW_SPI_DEV);
#endif

	palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_INPUT_PULLUP);

	if (mode == ENCODER_MODE_TS5700N8501) {
		ts5700n8501_stop_now = true;
		while (ts5700n8501_is_running) {
			chThdSleepMilliseconds(1);
		}

		palSetPadMode(HW_UART_TX_PORT, HW_UART_TX_PIN, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(HW_UART_RX_PORT, HW_UART_RX_PIN, PAL_MODE_INPUT_PULLUP);
#ifdef HW_ADC_EXT_GPIO
		palSetPadMode(HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN, PAL_MODE_INPUT_ANALOG);
#endif
	}

	index_found = false;
	mode = ENCODER_MODE_NONE;
	last_enc_angle = 0.0;
	spi_error_rate = 0.0;
	sincos_signal_low_error_rate = 0.0;
	sincos_signal_above_max_error_rate = 0.0;
}

void encoder_init_abi(uint32_t counts) {
	EXTI_InitTypeDef   EXTI_InitStructure;

	// Initialize variables
	index_found = false;
	enc_counts = counts;

	palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_ALTERNATE(HW_ENC_TIM_AF));
	palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_ALTERNATE(HW_ENC_TIM_AF));
//	palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_ALTERNATE(HW_ENC_TIM_AF));

	// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Enable SYSCFG clock
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_SYSCFG, ENABLE);

	TIM_EncoderInterfaceConfig (HW_ENC_TIM, TIM_EncoderMode_TI12,
			TIM_ICPolarity_Rising,
			TIM_ICPolarity_Rising);
	TIM_SetAutoreload(HW_ENC_TIM, enc_counts - 1);

	// Filter
	HW_ENC_TIM->CCMR1 |= 6 << 12 | 6 << 4;
	HW_ENC_TIM->CCMR2 |= 6 << 4;

	TIM_Cmd(HW_ENC_TIM, ENABLE);

	// Interrupt on index pulse

	// Connect EXTI Line to pin
	SYSCFG_EXTILineConfig(HW_ENC_EXTI_PORTSRC, HW_ENC_EXTI_PINSRC);

	// Configure EXTI Line
	EXTI_InitStructure.EXTI_Line = HW_ENC_EXTI_LINE;
	EXTI_InitStructure.EXTI_Mode = EXTI_Mode_Interrupt;
	EXTI_InitStructure.EXTI_Trigger = EXTI_Trigger_Rising;
	EXTI_InitStructure.EXTI_LineCmd = ENABLE;
	EXTI_Init(&EXTI_InitStructure);

	// Enable and set EXTI Line Interrupt to the highest priority
	nvicEnableVector(HW_ENC_EXTI_CH, 0);

	mode = ENCODER_MODE_ABI;
}

void encoder_init_as5047p_spi(void) {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;

	palSetPadMode(SPI_SW_MISO_GPIO, SPI_SW_MISO_PIN, PAL_MODE_INPUT);
	palSetPadMode(SPI_SW_SCK_GPIO, SPI_SW_SCK_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SPI_SW_CS_GPIO, SPI_SW_CS_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	// Set MOSI to 1
#if (AS5047_USE_HW_SPI_PINS || AD2S1205_USE_HW_SPI_PINS)
	palSetPadMode(SPI_SW_MOSI_GPIO, SPI_SW_MOSI_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(SPI_SW_MOSI_GPIO, SPI_SW_MOSI_PIN);
#endif

	// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = ((168000000 / 2 / AS5047_SAMPLE_RATE_HZ) - 1);
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(HW_ENC_TIM, &TIM_TimeBaseStructure);

	// Enable overflow interrupt
	TIM_ITConfig(HW_ENC_TIM, TIM_IT_Update, ENABLE);

	// Enable timer
	TIM_Cmd(HW_ENC_TIM, ENABLE);

	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);

	mode = ENCODER_MODE_AS5047P_SPI;
	index_found = true;
	spi_error_rate = 0.0;

}

void encoder_init_mt6816_spi(void) {
#ifdef HW_SPI_DEV
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;

	palSetPadMode(SPI_SW_SCK_GPIO, SPI_SW_SCK_PIN, PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SPI_SW_MISO_GPIO, SPI_SW_MISO_PIN, PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SPI_SW_CS_GPIO, SPI_SW_CS_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

#if (MT6816_USE_HW_SPI_PINS)
	palSetPadMode(SPI_SW_MOSI_GPIO, SPI_SW_MOSI_PIN, PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);
#endif

	//Start driver with MT6816 SPI settings
	spiStart(&HW_SPI_DEV, &mt6816_spi_cfg);

	// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = ((168000000 / 2 / MT6816_SAMPLE_RATE_HZ) - 1);
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(HW_ENC_TIM, &TIM_TimeBaseStructure);

	// Enable overflow interrupt
	TIM_ITConfig(HW_ENC_TIM, TIM_IT_Update, ENABLE);

	// Enable timer
	TIM_Cmd(HW_ENC_TIM, ENABLE);

	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);

	mode = ENCODER_MODE_MT6816_SPI;
	index_found = true;
	spi_error_rate = 0.0;
	encoder_no_magnet_error_rate = 0.0;
#endif
}

void encoder_init_ad2s1205_spi(void) {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;

	resolver_loss_of_tracking_error_rate = 0.0;
	resolver_degradation_of_signal_error_rate = 0.0;
	resolver_loss_of_signal_error_rate = 0.0;
	resolver_loss_of_tracking_error_cnt = 0;
	resolver_loss_of_signal_error_cnt = 0;

	palSetPadMode(SPI_SW_MISO_GPIO, SPI_SW_MISO_PIN, PAL_MODE_INPUT);
	palSetPadMode(SPI_SW_SCK_GPIO, SPI_SW_SCK_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SPI_SW_CS_GPIO, SPI_SW_CS_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	// Set MOSI to 1
#if (AS5047_USE_HW_SPI_PINS || AD2S1205_USE_HW_SPI_PINS)
	palSetPadMode(SPI_SW_MOSI_GPIO, SPI_SW_MOSI_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(SPI_SW_MOSI_GPIO, SPI_SW_MOSI_PIN);
#endif

	// TODO: Choose pins on comm port when these are not defined
#if defined(AD2S1205_SAMPLE_GPIO)
	palSetPadMode(AD2S1205_SAMPLE_GPIO, AD2S1205_SAMPLE_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(AD2S1205_SAMPLE_GPIO, AD2S1205_SAMPLE_PIN);	// Prepare for a falling edge SAMPLE assertion
#endif
#if defined(AD2S1205_RDVEL_GPIO)
	palSetPadMode(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN);		// Will always read position
#endif


	// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = ((168000000 / 2 / AD2S1205_SAMPLE_RATE_HZ) - 1);
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(HW_ENC_TIM, &TIM_TimeBaseStructure);

	// Enable overflow interrupt
	TIM_ITConfig(HW_ENC_TIM, TIM_IT_Update, ENABLE);

	// Enable timer
	TIM_Cmd(HW_ENC_TIM, ENABLE);

	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);

	mode = RESOLVER_MODE_AD2S1205;
	index_found = true;
}

void encoder_init_sincos(float s_gain, float s_offset,
						 float c_gain, float c_offset, float filter_constant) {
	//ADC inputs are already initialized in hw_init_gpio()
	sin_gain = s_gain;
	sin_offset = s_offset;
	cos_gain = c_gain;
	cos_offset = c_offset;
	sincos_filter_constant = filter_constant;

	sincos_signal_below_min_error_cnt = 0;
	sincos_signal_above_max_error_cnt = 0;
	sincos_signal_low_error_rate = 0.0;
	sincos_signal_above_max_error_rate = 0.0;

	// ADC measurements needs to be in sync with motor PWM
#ifdef HW_HAS_SIN_COS_ENCODER
	mode = ENCODER_MODE_SINCOS;
	index_found = true;
#else
	mode = ENCODER_MODE_NONE;
	index_found = false;
#endif
}

void encoder_init_ts5700n8501(void) {
	mode = ENCODER_MODE_TS5700N8501;
	index_found = true;
	spi_error_rate = 0.0;
	spi_error_cnt = 0;
	ts5700n8501_is_running = true;
	ts5700n8501_stop_now = false;

	chThdCreateStatic(ts5700n8501_thread_wa, sizeof(ts5700n8501_thread_wa),
			NORMALPRIO - 10, ts5700n8501_thread, NULL);
}

bool encoder_is_configured(void) {
	return mode != ENCODER_MODE_NONE;
}

/**
 * Read angle from configured encoder.
 *
 * @return
 * The current encoder angle in degrees.
 */
float encoder_read_deg(void) {
	static float angle = 0.0;

	switch (mode) {
	case ENCODER_MODE_ABI:
		angle = ((float)HW_ENC_TIM->CNT * 360.0) / (float)enc_counts;
		break;

	case ENCODER_MODE_AS5047P_SPI:
	case ENCODER_MODE_MT6816_SPI:
	case RESOLVER_MODE_AD2S1205:
	case ENCODER_MODE_TS5700N8501:
		angle = last_enc_angle;
		break;

#ifdef HW_HAS_SIN_COS_ENCODER
	case ENCODER_MODE_SINCOS: {
		float sin = ENCODER_SIN_VOLTS * sin_gain - sin_offset;
		float cos = ENCODER_COS_VOLTS * cos_gain - cos_offset;

		float module = SQ(sin) + SQ(cos);

		if (module > SQ(SINCOS_MAX_AMPLITUDE) )	{
			// signals vector outside of the valid area. Increase error count and discard measurement
			++sincos_signal_above_max_error_cnt;
			UTILS_LP_FAST(sincos_signal_above_max_error_rate, 1.0, 1./SINCOS_SAMPLE_RATE_HZ);
			angle = last_enc_angle;
		}
		else {
			if (module < SQ(SINCOS_MIN_AMPLITUDE)) {
				++sincos_signal_below_min_error_cnt;
				UTILS_LP_FAST(sincos_signal_low_error_rate, 1.0, 1./SINCOS_SAMPLE_RATE_HZ);
				angle = last_enc_angle;
			}
			else {
				UTILS_LP_FAST(sincos_signal_above_max_error_rate, 0.0, 1./SINCOS_SAMPLE_RATE_HZ);
				UTILS_LP_FAST(sincos_signal_low_error_rate, 0.0, 1./SINCOS_SAMPLE_RATE_HZ);

				float angle_tmp = utils_fast_atan2(sin, cos) * 180.0 / M_PI;
				UTILS_LP_FAST(angle, angle_tmp, sincos_filter_constant);
				last_enc_angle = angle;
			}
		}
		break;
	}
#endif

	default:
		break;
	}

	return angle;
}

/*
 * Note: This is not a good solution and needs a proper implementation later...
 */
float encoder_read_deg_multiturn(void) {
	if (mode == ENCODER_MODE_TS5700N8501) {
		encoder_ts57n8501_get_abm();
		float ts_mt = (float)encoder_ts57n8501_get_abm();
		if (fabsf(ts_mt) > 5000.0) {
			ts_mt = 0;
			encoder_ts57n8501_reset_multiturn();
		}

		ts_mt += 5000;

		return encoder_read_deg() / 10000.0 + (360 * ts_mt) / 10000.0;
	} else {
		return encoder_read_deg();
	}
}

/**
 * Reset the encoder counter. Should be called from the index interrupt.
 */
void encoder_reset(void) {
	// Only reset if the pin is still high to avoid too short pulses, which
	// most likely are noise.
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	if (palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)) {
		const unsigned int cnt = HW_ENC_TIM->CNT;
		static int bad_pulses = 0;
		const unsigned int lim = enc_counts / 20;

		if (index_found) {
			// Some plausibility filtering.
			if (cnt > (enc_counts - lim) || cnt < lim) {
				HW_ENC_TIM->CNT = 0;
				bad_pulses = 0;
			} else {
				bad_pulses++;

				if (bad_pulses > 5) {
					index_found = 0;
				}
			}
		} else {
			HW_ENC_TIM->CNT = 0;
			index_found = true;
			bad_pulses = 0;
		}
	}
}

// returns true for even number of ones (no parity error according to AS5047 datasheet
bool spi_check_parity(uint16_t x) {
	x ^= x >> 8;
	x ^= x >> 4;
	x ^= x >> 2;
	x ^= x >> 1;
	return (~x) & 1;
}

/**
 * Timer interrupt
 */
void encoder_tim_isr(void) {
	uint16_t pos;

	if(mode == ENCODER_MODE_AS5047P_SPI) {
		spi_begin();
		spi_transfer(&pos, 0, 1);
		spi_end();

		spi_val = pos;
		if(spi_check_parity(pos)) {
			pos &= 0x3FFF;
			last_enc_angle = ((float)pos * 360.0) / 16384.0;
			UTILS_LP_FAST(spi_error_rate, 0.0, 1./AS5047_SAMPLE_RATE_HZ);
		} else {
			++spi_error_cnt;
			UTILS_LP_FAST(spi_error_rate, 1.0, 1./AS5047_SAMPLE_RATE_HZ);
		}		
	}

#ifdef HW_SPI_DEV
	if(mode == ENCODER_MODE_MT6816_SPI) {
		uint16_t reg_data_03;
		uint16_t reg_data_04;
		uint16_t reg_addr_03 = 0x8300;
		uint16_t reg_addr_04 = 0x8400;

		spi_begin();
		reg_data_03 = spiPolledExchange(&HW_SPI_DEV, reg_addr_03);
		spi_end();
		spi_delay();
		spi_begin();
		reg_data_04 = spiPolledExchange(&HW_SPI_DEV, reg_addr_04);
		spi_end();

		pos = (reg_data_03 << 8) | reg_data_04;
		spi_val = pos;

		if( spi_check_parity(pos) ) {
			if (pos & MT6816_NO_MAGNET_ERROR_MASK) {
				++encoder_no_magnet_error_cnt;
				UTILS_LP_FAST(encoder_no_magnet_error_rate, 1.0, 1./MT6816_SAMPLE_RATE_HZ);
			}
			else {
				pos = pos >> 2;
				last_enc_angle = ((float)pos * 360.0) / 16384.0;
				UTILS_LP_FAST(spi_error_rate, 0.0, 1./MT6816_SAMPLE_RATE_HZ);
				UTILS_LP_FAST(encoder_no_magnet_error_rate, 0.0, 1./MT6816_SAMPLE_RATE_HZ);
			}
		} else {
			++spi_error_cnt;
			UTILS_LP_FAST(spi_error_rate, 1.0, 1./MT6816_SAMPLE_RATE_HZ);
		}
	}
#endif

	if(mode == RESOLVER_MODE_AD2S1205) {
		// SAMPLE signal should have been be asserted in sync with ADC sampling
#ifdef AD2S1205_RDVEL_GPIO
		palSetPad(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN);	// Always read position
#endif

		palSetPad(SPI_SW_SCK_GPIO, SPI_SW_SCK_PIN);
		spi_delay();
		spi_begin(); // CS uses the same mcu pin as AS5047
		spi_delay();

		spi_transfer(&pos, 0, 1);
		spi_end();

		spi_val = pos;

		uint16_t RDVEL = pos & 0x0008; // 1 means a position read

		if((RDVEL != 0)){

			bool DOS = ((pos & 0x04) == 0);
			bool LOT = ((pos & 0x02) == 0);
			bool LOS = DOS && LOT;
			bool parity_error = spi_check_parity(pos);	//16 bit frame has odd parity
			bool angle_is_correct = true;

			if(LOS) {
				LOT = DOS = 0;
			}

			if(!parity_error) {
				UTILS_LP_FAST(spi_error_rate, 0.0, 1./AD2S1205_SAMPLE_RATE_HZ);
			} else {
				angle_is_correct = false;
				++spi_error_cnt;
				UTILS_LP_FAST(spi_error_rate, 1.0, 1./AD2S1205_SAMPLE_RATE_HZ);
			}

			pos &= 0xFFF0;
			pos = pos >> 4;
			pos &= 0x0FFF;

			if(LOT) {
				angle_is_correct = false;
				++resolver_loss_of_tracking_error_cnt;
				UTILS_LP_FAST(resolver_loss_of_tracking_error_rate, 1.0, 1./AD2S1205_SAMPLE_RATE_HZ);
			} else {
				UTILS_LP_FAST(resolver_loss_of_tracking_error_rate, 0.0, 1./AD2S1205_SAMPLE_RATE_HZ);
			}

			if(DOS) {
				angle_is_correct = false;
				++resolver_degradation_of_signal_error_cnt;
				UTILS_LP_FAST(resolver_degradation_of_signal_error_rate, 1.0, 1./AD2S1205_SAMPLE_RATE_HZ);
			} else {
				UTILS_LP_FAST(resolver_degradation_of_signal_error_rate, 0.0, 1./AD2S1205_SAMPLE_RATE_HZ);
			}

			if(LOS) {
				angle_is_correct = false;
				++resolver_loss_of_signal_error_cnt;
				UTILS_LP_FAST(resolver_loss_of_signal_error_rate, 1.0, 1./AD2S1205_SAMPLE_RATE_HZ);
			} else {
				UTILS_LP_FAST(resolver_loss_of_signal_error_rate, 0.0, 1./AD2S1205_SAMPLE_RATE_HZ);
			}

			if(angle_is_correct)
			{
				last_enc_angle = ((float)pos * 360.0) / 4096.0;
			}
		}
	}
}

/**
 * Set the number of encoder counts.
 *
 * @param counts
 * The number of encoder counts
 */
void encoder_set_counts(uint32_t counts) {
	if (counts != enc_counts) {
		enc_counts = counts;
		TIM_SetAutoreload(HW_ENC_TIM, enc_counts - 1);
		index_found = false;
	}
}

/**
 * Check if the index pulse is found.
 *
 * @return
 * True if the index is found, false otherwise.
 */
bool encoder_index_found(void) {
	return index_found;
}

// Software SPI
static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length) {
	for (int i = 0;i < length;i++) {
		uint16_t send = out_buf ? out_buf[i] : 0xFFFF;
		uint16_t receive = 0;

		for (int bit = 0;bit < 16;bit++) {
			//palWritePad(HW_SPI_PORT_MOSI, HW_SPI_PIN_MOSI, send >> 15);
			send <<= 1;

			palSetPad(SPI_SW_SCK_GPIO, SPI_SW_SCK_PIN);
			spi_delay();

			int samples = 0;
			samples += palReadPad(SPI_SW_MISO_GPIO, SPI_SW_MISO_PIN);
			__NOP();
			samples += palReadPad(SPI_SW_MISO_GPIO, SPI_SW_MISO_PIN);
			__NOP();
			samples += palReadPad(SPI_SW_MISO_GPIO, SPI_SW_MISO_PIN);
			__NOP();
			samples += palReadPad(SPI_SW_MISO_GPIO, SPI_SW_MISO_PIN);
			__NOP();
			samples += palReadPad(SPI_SW_MISO_GPIO, SPI_SW_MISO_PIN);

			receive <<= 1;
			if (samples > 2) {
				receive |= 1;
			}

			palClearPad(SPI_SW_SCK_GPIO, SPI_SW_SCK_PIN);
			spi_delay();
		}

		if (in_buf) {
			in_buf[i] = receive;
		}
	}
}

static void spi_begin(void) {
	palClearPad(SPI_SW_CS_GPIO, SPI_SW_CS_PIN);
}

static void spi_end(void) {
	palSetPad(SPI_SW_CS_GPIO, SPI_SW_CS_PIN);
}

static void spi_delay(void) {
	__NOP();
	__NOP();
	__NOP();
	__NOP();
}

#pragma GCC push_options
#pragma GCC optimize ("O0")

void TS5700N8501_delay_uart(void) {
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP();
}

/*
 * It is important to switch to receive mode immediately after sending the readout command,
 * as the TS5700N8501 starts sending the reply after 3 microseconds. Therefore use software
 * UART on TX so that the enable signal can be controlled manually. This function runs while
 * the system is locked, but it should finish fast enough to not cause problems for other
 * things due to the high baud rate.
 */
static void TS5700N8501_send_byte(uint8_t b) {
	utils_sys_lock_cnt();
#ifdef HW_ADC_EXT_GPIO
	palSetPad(HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN);
#endif
	TS5700N8501_delay_uart();
	palWritePad(HW_UART_TX_PORT, HW_UART_TX_PIN, 0);
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	for (int i = 0;i < 8;i++) {
		palWritePad(HW_UART_TX_PORT, HW_UART_TX_PIN,
				(b & (0x80 >> i)) ? PAL_HIGH : PAL_LOW);
		TS5700N8501_delay_uart();
	}
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	palWritePad(HW_UART_TX_PORT, HW_UART_TX_PIN, 1);
	TS5700N8501_delay_uart();
#ifdef HW_ADC_EXT_GPIO
	palClearPad(HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN);
#endif
	utils_sys_unlock_cnt();
}

#pragma GCC pop_options

static THD_FUNCTION(ts5700n8501_thread, arg) {
	(void)arg;

	chRegSetThreadName("TS5700N8501");

	sdStart(&HW_UART_DEV, &TS5700N8501_uart_cfg);
	palSetPadMode(HW_UART_TX_PORT, HW_UART_TX_PIN, PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);
	palSetPadMode(HW_UART_RX_PORT, HW_UART_RX_PIN, PAL_MODE_ALTERNATE(HW_UART_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);
#ifdef HW_ADC_EXT_GPIO
	palSetPadMode(HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN, PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);
#endif

	for(;;) {
		// Check if it is time to stop.
		if (ts5700n8501_stop_now) {
			ts5700n8501_is_running = false;
			return;
		}

		if (ts5700n8501_reset_errors) {
			for (int i = 0;i < 20;i++) {
				TS5700N8501_send_byte(0b01011101);
				chThdSleep(2);
			}

			ts5700n8501_reset_errors = false;
		}

		if (ts5700n8501_reset_multiturn) {
			for (int i = 0;i < 20;i++) {
				TS5700N8501_send_byte(0b01000110);
				chThdSleep(2);
			}

			ts5700n8501_reset_multiturn = false;
		}

		TS5700N8501_send_byte(0b01011000);

		chThdSleep(2);

		uint8_t reply[11];
		int reply_ind = 0;

		msg_t res = sdGetTimeout(&HW_UART_DEV, TIME_IMMEDIATE);
		while (res != MSG_TIMEOUT) {
			if (reply_ind < (int)sizeof(reply)) {
				reply[reply_ind++] = res;
			}
			res = sdGetTimeout(&HW_UART_DEV, TIME_IMMEDIATE);
		}

		uint8_t crc = 0;
		for (int i = 0;i < (reply_ind - 1);i++) {
			crc = (reply[i] ^ crc);
		}

		if (reply_ind == 11 && crc == reply[reply_ind - 1]) {
			uint32_t pos = (uint32_t)reply[2] + ((uint32_t)reply[3] << 8) + ((uint32_t)reply[4] << 16);
			spi_val = pos;
			last_enc_angle = (float)pos / 131072.0 * 360.0;
			UTILS_LP_FAST(spi_error_rate, 0.0, 1.0 / AS5047_SAMPLE_RATE_HZ);

			ts5700n8501_raw_status[0] = reply[1]; // SF
			ts5700n8501_raw_status[1] = reply[2]; // ABS0
			ts5700n8501_raw_status[2] = reply[3]; // ABS1
			ts5700n8501_raw_status[3] = reply[4]; // ABS2
			ts5700n8501_raw_status[4] = reply[6]; // ABM0
			ts5700n8501_raw_status[5] = reply[7]; // ABM1
			ts5700n8501_raw_status[6] = reply[8]; // ABM2
			ts5700n8501_raw_status[7] = reply[9]; // ALMC
		} else {
			++spi_error_cnt;
			UTILS_LP_FAST(spi_error_rate, 1.0, 1.0 / AS5047_SAMPLE_RATE_HZ);
		}
	}
}

