
#include "encoder/AD2S1205.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>
#include "spi_bb.h"

static AD2S1205_config_t AD2S1205_config_now = { 0 };
static spi_bb_state software_spi_now = { 0 };

static uint16_t spi_val = 0;
static float resolver_loss_of_tracking_error_rate = 0.0;
static float resolver_degradation_of_signal_error_rate = 0.0;
static float resolver_loss_of_signal_error_rate = 0.0;
static uint32_t resolver_loss_of_tracking_error_cnt = 0;
static uint32_t resolver_degradation_of_signal_error_cnt = 0;
static uint32_t resolver_loss_of_signal_error_cnt = 0;
static uint32_t spi_error_cnt = 0;
static float spi_error_rate = 0.0;
static float last_enc_angle = 0.0;

void AD2S1205_deinit(void) {
	nvicDisableVector(HW_ENC_EXTI_CH);
	nvicDisableVector(HW_ENC_TIM_ISR_CH);

	TIM_DeInit(HW_ENC_TIM);

	spi_bb_deinit(&software_spi_now);

#ifdef HW_SPI_DEV
	spiStop(&HW_SPI_DEV);
#endif

	// TODO: (TO BE TESTED!!) DEINITIALIZE ALSO SAMPLE AND RDVEL
#if defined(AD2S1205_SAMPLE_GPIO)
	palSetPadMode(AD2S1205_SAMPLE_GPIO, AD2S1205_SAMPLE_PIN, PAL_MODE_INPUT_PULLUP);	// Prepare for a falling edge SAMPLE assertion
#endif
#if defined(AD2S1205_RDVEL_GPIO)
	palSetPadMode(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN, PAL_MODE_INPUT_PULLUP);	// Will always read position
#endif

	AD2S1205_config_now.is_init = 0;
}

encoder_ret_t AD2S1205_init(AD2S1205_config_t *AD2S1205_config) {

	TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;
	encoder_spi_config_t AD2S1205_spi_config = AD2S1205_config->spi_config;

	software_spi_now.miso_gpio = AD2S1205_spi_config.gpio_miso.port;
	software_spi_now.miso_pin = AD2S1205_spi_config.gpio_miso.pin;
	software_spi_now.nss_gpio = AD2S1205_spi_config.gpio_nss.port;
	software_spi_now.nss_pin = AD2S1205_spi_config.gpio_nss.pin;
	software_spi_now.sck_gpio = AD2S1205_spi_config.gpio_sck.port;
	software_spi_now.sck_pin = AD2S1205_spi_config.gpio_sck.pin;
	software_spi_now.mosi_gpio = AD2S1205_spi_config.gpio_mosi.port;
	software_spi_now.mosi_pin = AD2S1205_spi_config.gpio_mosi.pin;

	spi_bb_init(&software_spi_now);

	resolver_loss_of_tracking_error_rate = 0.0;
	resolver_degradation_of_signal_error_rate = 0.0;
	resolver_loss_of_signal_error_rate = 0.0;
	resolver_loss_of_tracking_error_cnt = 0;
	resolver_loss_of_signal_error_cnt = 0;



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
	TIM_TimeBaseStructure.TIM_Period = ((168000000 / 2
			/ AD2S1205_config->refresh_rate_hz) - 1);
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(HW_ENC_TIM, &TIM_TimeBaseStructure);

	// Enable overflow interrupt
	TIM_ITConfig(HW_ENC_TIM, TIM_IT_Update, ENABLE);
	// Enable timer
	TIM_Cmd(HW_ENC_TIM, ENABLE);

	AD2S1205_config->is_init = 1;
	AD2S1205_config_now = *AD2S1205_config;

	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);

	return ENCODER_OK;
}

float AD2S1205_read_deg(void) {
	return last_enc_angle;
}

void AD2S1205_routine(void) {
	uint16_t pos;
	// SAMPLE signal should have been be asserted in sync with ADC sampling
#ifdef AD2S1205_RDVEL_GPIO
	palSetPad(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN);	// Always read position
#endif

	palSetPad(AD2S1205_config_now.spi_config.gpio_sck.port,
			AD2S1205_config_now.spi_config.gpio_sck.pin);
	spi_bb_delay();
	spi_bb_begin(&software_spi_now); // CS uses the same mcu pin as AS5047
	spi_bb_delay();

	spi_bb_transfer_16(&software_spi_now, &pos, 0, 1);
	spi_bb_end(&software_spi_now);

	spi_val = pos;

	uint16_t RDVEL = pos & 0x0008; // 1 means a position read

	if ((RDVEL != 0)) {

		bool DOS = ((pos & 0x04) == 0);
		bool LOT = ((pos & 0x02) == 0);
		bool LOS = DOS && LOT;
		bool parity_error = spi_bb_check_parity(pos);//16 bit frame has odd parity
		bool angle_is_correct = true;

		if (LOS) {
			LOT = DOS = 0;
		}

		if (!parity_error) {
			UTILS_LP_FAST(spi_error_rate, 0.0,
					1. / AD2S1205_config_now.refresh_rate_hz);
		} else {
			angle_is_correct = false;
			++spi_error_cnt;
			UTILS_LP_FAST(spi_error_rate, 1.0,
					1. / AD2S1205_config_now.refresh_rate_hz);
		}

		pos &= 0xFFF0;
		pos = pos >> 4;
		pos &= 0x0FFF;

		if (LOT) {
			angle_is_correct = false;
			++resolver_loss_of_tracking_error_cnt;
			UTILS_LP_FAST(resolver_loss_of_tracking_error_rate, 1.0,
					1. / AD2S1205_config_now.refresh_rate_hz);
		} else {
			UTILS_LP_FAST(resolver_loss_of_tracking_error_rate, 0.0,
					1. / AD2S1205_config_now.refresh_rate_hz);
		}

		if (DOS) {
			angle_is_correct = false;
			++resolver_degradation_of_signal_error_cnt;
			UTILS_LP_FAST(resolver_degradation_of_signal_error_rate, 1.0,
					1. / AD2S1205_config_now.refresh_rate_hz);
		} else {
			UTILS_LP_FAST(resolver_degradation_of_signal_error_rate, 0.0,
					1. / AD2S1205_config_now.refresh_rate_hz);
		}

		if (LOS) {
			angle_is_correct = false;
			++resolver_loss_of_signal_error_cnt;
			UTILS_LP_FAST(resolver_loss_of_signal_error_rate, 1.0,
					1. / AD2S1205_config_now.refresh_rate_hz);
		} else {
			UTILS_LP_FAST(resolver_loss_of_signal_error_rate, 0.0,
					1. / AD2S1205_config_now.refresh_rate_hz);
		}

		if (angle_is_correct) {
			last_enc_angle = ((float) pos * 360.0) / 4096.0;
		}
	}

}

float AD2S1205_resolver_loss_of_tracking_error_rate(void) {
	return resolver_loss_of_tracking_error_rate;
}

float AD2S1205_resolver_degradation_of_signal_error_rate(void) {
	return resolver_degradation_of_signal_error_rate;
}

float AD2S1205_resolver_loss_of_signal_error_rate(void) {
	return resolver_loss_of_signal_error_rate;
}

uint32_t AD2S1205_resolver_loss_of_tracking_error_cnt(void) {
	return resolver_loss_of_tracking_error_cnt;
}

uint32_t AD2S1205_resolver_degradation_of_signal_error_cnt(void) {
	return resolver_degradation_of_signal_error_cnt;
}

uint32_t AD2S1205_resolver_loss_of_signal_error_cnt(void) {
	return resolver_loss_of_signal_error_cnt;
}

uint32_t AD2S1205_spi_get_error_cnt(void) {
	return spi_error_cnt;
}
