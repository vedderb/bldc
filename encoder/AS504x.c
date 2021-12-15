
#include "encoder/AS504x.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>

//Private variables
#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
static uint16_t AS504x_diag_fetch_now_count = 0;
#endif
#if !(AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS)
static uint32_t AS504x_data_last_invalid_counter = 0;
#endif
static uint32_t AS504x_spi_communication_error_count = 0;
static AS504x_config_t AS504x_config_now = { 0 };
static uint8_t spi_data_err_raised = 0;
static AS504x_diag AS504x_sensor_diag = { 0 };
static uint16_t spi_val = 0;
static float last_enc_angle = 0.0;
static uint32_t spi_error_cnt = 0;
static float spi_error_rate = 0.0;

//Private functions
static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length);
static void spi_begin(void);
static void spi_end(void);
static void spi_delay(void);
static void spi_AS5047_cs_delay(void);
static bool spi_check_parity(uint16_t x);

#if (AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS)
static uint8_t AS504x_fetch_diag(void);
static uint8_t AS504x_verify_serial(void);
static void AS504x_deserialize_diag(void);
static void AS504x_fetch_clear_err_diag(void);
static uint8_t AS504x_spi_transfer_err_check(uint16_t *in_buf,
		const uint16_t *out_buf, int length);
#endif
static void AS504x_determinate_if_connected(bool was_last_valid);

void AS504x_routine(void) {

	uint16_t pos;
// if MOSI is defined, use diagnostics
#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
	spi_begin();
	spi_transfer(0, 0, 1);
	spi_end();

	spi_AS5047_cs_delay();

	spi_begin();
	spi_data_err_raised = AS504x_spi_transfer_err_check(&pos, 0, 1);
	spi_end();
	spi_val = pos;

	// get diagnostic every AS504x_REFRESH_DIAG_AFTER_NSAMPLES
	AS504x_diag_fetch_now_count++;
	if (AS504x_diag_fetch_now_count >= AS504x_REFRESH_DIAG_AFTER_NSAMPLES
			|| spi_data_err_raised) {
		// clear error flags before getting new diagnostics data
		AS504x_fetch_clear_err_diag();

		if (!AS504x_fetch_diag()) {
			if (!AS504x_verify_serial()) {
				AS504x_deserialize_diag();
				AS504x_determinate_if_connected(true);
			} else {
				AS504x_determinate_if_connected(false);
			}
		} else {
			AS504x_determinate_if_connected(false);
		}
		AS504x_diag_fetch_now_count = 0;
	}
#else
        spi_begin();
        spi_transfer(&pos, 0, 1);
        spi_end();
        spi_val = pos;

        if(0x0000 == pos || 0xFFFF == pos) {
            AS504x_data_last_invalid_counter++;
        } else {
            AS504x_data_last_invalid_counter = 0;
            AS504x_determinate_if_connected(true);
        }

        if (AS504x_data_last_invalid_counter >= AS504x_DATA_INVALID_THRESHOLD) {
            AS504x_determinate_if_connected(false);
            AS504x_data_last_invalid_counter = AS504x_DATA_INVALID_THRESHOLD;
        }
#endif

	if (spi_check_parity(pos) && !spi_data_err_raised) {
		pos &= 0x3FFF;
		last_enc_angle = ((float) pos * 360.0) / 16384.0;
		UTILS_LP_FAST(spi_error_rate, 0.0,
				1. / AS504x_config_now.refresh_rate_hz);
	} else {
		++spi_error_cnt;
		UTILS_LP_FAST(spi_error_rate, 1.0,
				1. / AS504x_config_now.refresh_rate_hz);
	}
}

static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length) {
	const encoders_gpio_t gpio_miso = AS504x_config_now.spi_config.gpio_miso;
#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
	const encoders_gpio_t gpio_mosi = AS504x_config_now.spi_config.gpio_mosi;
#endif
	const encoders_gpio_t gpio_sck = AS504x_config_now.spi_config.gpio_sck;
	for (int i = 0; i < length; i++) {

#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
		uint16_t send = out_buf ? out_buf[i] : 0xFFFF;
#else
		(void)out_buf;
#endif

		uint16_t receive = 0;

		for (int bit = 0; bit < 16; bit++) {
#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
			palWritePad(gpio_mosi.port, gpio_mosi.pin, send >> (15 - bit));
#endif

			palSetPad(gpio_sck.port, gpio_sck.pin);
			spi_delay();

			int samples = 0;
			samples += palReadPad(gpio_miso.port, gpio_miso.pin);
			__NOP();
			samples += palReadPad(gpio_miso.port, gpio_miso.pin);
			__NOP();
			samples += palReadPad(gpio_miso.port, gpio_miso.pin);
			__NOP();
			samples += palReadPad(gpio_miso.port, gpio_miso.pin);
			__NOP();
			samples += palReadPad(gpio_miso.port, gpio_miso.pin);

			receive <<= 1;
			if (samples > 2) {
				receive |= 1;
			}

			palClearPad(gpio_sck.port, gpio_sck.pin);
			spi_delay();
		}

		if (in_buf) {
			in_buf[i] = receive;
		}
	}
}

encoders_ret_t AS504x_init(AS504x_config_t *AS504x_config) {
	TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;
	encoders_spi_config_t AS504x_spi_config = AS504x_config->spi_config;

	palSetPadMode(AS504x_spi_config.gpio_miso.port,
			AS504x_spi_config.gpio_miso.pin, PAL_MODE_INPUT);
	palSetPadMode(AS504x_spi_config.gpio_sck.port,
			AS504x_spi_config.gpio_sck.pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(AS504x_spi_config.gpio_nss.port,
			AS504x_spi_config.gpio_nss.pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	// Set MOSI to 1
#if AS504x_USE_SW_MOSI_PIN
	palSetPadMode(AS504x_spi_config.gpio_mosi.port,
			AS504x_spi_config.gpio_mosi.pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(AS504x_spi_config.gpio_mosi.port,
			AS504x_spi_config.gpio_mosi.pin);
#endif

	AS504x_config_now = *AS504x_config;

	HW_ENC_TIM_CLK_EN();

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = ((168000000 / 2
			/ AS504x_config->refresh_rate_hz) - 1);
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(HW_ENC_TIM, &TIM_TimeBaseStructure);

	// Enable overflow interrupt
	TIM_ITConfig(HW_ENC_TIM, TIM_IT_Update, ENABLE);
	// Enable timer
	TIM_Cmd(HW_ENC_TIM, ENABLE);
	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);

	spi_error_rate = 0.0;

	AS504x_config_now.is_init = 1;
	AS504x_config->is_init = 1;
	return ENCODERS_OK;
}
#if (AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS)
static uint8_t AS504x_fetch_diag(void) {
	uint16_t recf[2], senf[2] = { AS504x_SPI_READ_DIAG_MSG,
			AS504x_SPI_READ_MAGN_MSG };
	uint8_t ret = 0;

	spi_begin();
	spi_transfer(0, senf, 1);
	spi_end();

	spi_AS5047_cs_delay();

	spi_begin();
	ret |= AS504x_spi_transfer_err_check(recf, senf + 1, 1);
	spi_end();

	spi_AS5047_cs_delay();

	spi_begin();
	ret |= AS504x_spi_transfer_err_check(recf + 1, 0, 1);
	spi_end();

	if (!ret) {
		if (spi_check_parity(recf[0]) && spi_check_parity(recf[1])) {
			AS504x_sensor_diag.serial_diag_flgs = recf[0];
			AS504x_sensor_diag.serial_magnitude = recf[1];
		}
	}

	return ret;
}

static uint8_t AS504x_verify_serial() {
	uint16_t serial_diag_flgs, serial_magnitude, test_magnitude;
	uint8_t test_AGC_value, test_is_Comp_high, test_is_Comp_low;

	serial_magnitude = AS504x_get_diag().serial_magnitude;
	serial_diag_flgs = AS504x_get_diag().serial_diag_flgs;

	test_magnitude = serial_magnitude
			& AS504x_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK;
	test_AGC_value = serial_diag_flgs;
	test_is_Comp_low = (serial_diag_flgs >> AS504x_SPI_DIAG_COMP_LOW_BIT_POS)
			& 1;
	test_is_Comp_high = (serial_diag_flgs >> AS504x_SPI_DIAG_COMP_HIGH_BIT_POS)
			& 1;

	if (test_is_Comp_high && test_is_Comp_low) {
		return 1;
	}
	if ((uint32_t) test_magnitude + (uint32_t) test_AGC_value == 0) {
		return 1;
	}

	return 0;
}

static void AS504x_deserialize_diag() {
	AS504x_sensor_diag.AGC_value = AS504x_sensor_diag.serial_diag_flgs;
	AS504x_sensor_diag.is_OCF = (AS504x_sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_OCF_BIT_POS) & 1;
	AS504x_sensor_diag.is_COF = (AS504x_sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_COF_BIT_POS) & 1;
	AS504x_sensor_diag.is_Comp_low = (AS504x_sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_COMP_LOW_BIT_POS) & 1;
	AS504x_sensor_diag.is_Comp_high = (AS504x_sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_COMP_HIGH_BIT_POS) & 1;
	AS504x_sensor_diag.magnitude = AS504x_sensor_diag.serial_magnitude
			& AS504x_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK;
}

static void AS504x_fetch_clear_err_diag() {
	uint16_t recf, senf = AS504x_SPI_READ_CLEAR_ERROR_MSG;

	spi_begin();
	spi_transfer(0, &senf, 1);
	spi_end();

	spi_AS5047_cs_delay();

	spi_begin();
	spi_transfer(&recf, 0, 1);
	spi_end();

	AS504x_sensor_diag.serial_error_flags = recf;
}

static uint8_t AS504x_spi_transfer_err_check(uint16_t *in_buf,
		const uint16_t *out_buf, int length) {
	spi_transfer(in_buf, out_buf, length);

	for (int len_count = 0; len_count < length; len_count++) {
		if (((in_buf[len_count]) >> 14) & 0b01) {
			return 1;
		}
	}

	return 0;
}
#endif

static void AS504x_determinate_if_connected(bool was_last_valid) {
	if (!was_last_valid) {
		AS504x_spi_communication_error_count++;

		if (AS504x_spi_communication_error_count
				>= AS504x_CONNECTION_DETERMINATOR_ERROR_THRESHOLD) {
			AS504x_spi_communication_error_count =
					AS504x_CONNECTION_DETERMINATOR_ERROR_THRESHOLD;
			AS504x_sensor_diag.is_connected = 0;
		}
	} else {
		if (AS504x_spi_communication_error_count) {
			AS504x_spi_communication_error_count--;
		} else {
			AS504x_sensor_diag.is_connected = 1;
		}
	}
}

AS504x_diag AS504x_get_diag(void) {
	return AS504x_sensor_diag;
}

float AS504x_read_deg(void) {
	return last_enc_angle;
}

uint32_t AS504x_spi_get_val(void) {
	return spi_val;
}

uint32_t AS504x_spi_get_error_cnt(void) {
	return spi_error_cnt;
}

float AS504x_spi_get_error_rate(void) {
	return spi_error_rate;
}

void AS504x_deinit(void) {
	nvicDisableVector(HW_ENC_EXTI_CH);
	nvicDisableVector(HW_ENC_TIM_ISR_CH);

	TIM_DeInit(HW_ENC_TIM);

	palSetPadMode(AS504x_config_now.spi_config.gpio_miso.port,
			AS504x_config_now.spi_config.gpio_miso.pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(AS504x_config_now.spi_config.gpio_sck.port,
			AS504x_config_now.spi_config.gpio_sck.pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(AS504x_config_now.spi_config.gpio_nss.port,
			AS504x_config_now.spi_config.gpio_nss.pin, PAL_MODE_INPUT_PULLUP);

#if AS504x_USE_SW_MOSI_PIN
	palSetPadMode(AS504x_config_now.spi_config.gpio_mosi.port,
			AS504x_config_now.spi_config.gpio_mosi.pin, PAL_MODE_INPUT_PULLUP);
#endif

#ifdef HW_SPI_DEV
	spiStop(&HW_SPI_DEV);
#endif

	palSetPadMode(AS504x_config_now.spi_config.gpio_miso.port,
			AS504x_config_now.spi_config.gpio_miso.pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(AS504x_config_now.spi_config.gpio_sck.port,
			AS504x_config_now.spi_config.gpio_sck.pin, PAL_MODE_INPUT_PULLUP);

	AS504x_config_now.is_init = 0;
	last_enc_angle = 0.0;
	spi_error_rate = 0.0;
}

#pragma GCC push_options
#pragma GCC optimize ("O0")

static void spi_begin(void) {
	palClearPad(AS504x_config_now.spi_config.gpio_nss.port,
			AS504x_config_now.spi_config.gpio_nss.pin);
	spi_AS5047_cs_delay();
}

static void spi_end(void) {
	palSetPad(AS504x_config_now.spi_config.gpio_nss.port,
			AS504x_config_now.spi_config.gpio_nss.pin);
	spi_AS5047_cs_delay();
}

static void spi_delay(void) {
	__NOP();
	__NOP();
	__NOP();
	__NOP();
}

static void spi_AS5047_cs_delay(void) {
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
}

static bool spi_check_parity(uint16_t x) {
	x ^= x >> 8;
	x ^= x >> 4;
	x ^= x >> 2;
	x ^= x >> 1;
	return (~x) & 1;
}
