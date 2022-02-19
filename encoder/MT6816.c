#include "encoder/MT6816.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include "spi_bb.h"
#include <math.h>

#define MT6816_NO_MAGNET_ERROR_MASK		0x0002

static MT6816_config_t MT6816_config_now = { 0 };

static float spi_error_rate = 0.0;
static float encoder_no_magnet_error_rate = 0.0;
static float encoder_no_magnet_error_cnt = 0.0;
static float last_enc_angle = 0.0;
static uint32_t spi_error_cnt = 0;
static uint32_t spi_val = 0;

void MT6816_deinit(void) {

	nvicDisableVector(HW_ENC_EXTI_CH);
	nvicDisableVector(HW_ENC_TIM_ISR_CH);

	TIM_DeInit(HW_ENC_TIM);

	palSetPadMode(MT6816_config_now.sw_spi.miso_gpio,
			MT6816_config_now.sw_spi.miso_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(MT6816_config_now.sw_spi.sck_gpio,
			MT6816_config_now.sw_spi.sck_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(MT6816_config_now.sw_spi.nss_gpio,
			MT6816_config_now.sw_spi.nss_pin, PAL_MODE_INPUT_PULLUP);

#if (MT6816_USE_HW_SPI_PINS)
	palSetPadMode(MT6816_config_now.sw_spi.mosi_gpio, MT6816_config_now.sw_spi.mosi_pin, PAL_MODE_INPUT_PULLUP);
#endif

#ifdef HW_SPI_DEV
	spiStop(&HW_SPI_DEV);
#endif

	palSetPadMode(MT6816_config_now.sw_spi.miso_gpio,
			MT6816_config_now.sw_spi.miso_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(MT6816_config_now.sw_spi.sck_gpio,
			MT6816_config_now.sw_spi.sck_pin, PAL_MODE_INPUT_PULLUP);

	MT6816_config_now.is_init = 0;
	last_enc_angle = 0.0;
	spi_error_rate = 0.0;
}

encoder_ret_t MT6816_init(MT6816_config_t *mt6816_config) {
#ifdef HW_SPI_DEV
	TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;

	MT6816_config_now = *mt6816_config;

	palSetPadMode(MT6816_config_now.sw_spi.sck_gpio,
			MT6816_config_now.sw_spi.sck_pin,
			PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(MT6816_config_now.sw_spi.miso_gpio,
			MT6816_config_now.sw_spi.miso_pin,
			PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);

	spi_bb_nss_init(&(MT6816_config_now.sw_spi));

#if (MT6816_USE_HW_SPI_PINS)
	palSetPadMode(MT6816_config_now.sw_spi.mosi_gpio, MT6816_config_now.sw_spi.mosi_pin, PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);
#endif

	//Start driver with MT6816 SPI settings

	spiStart(&HW_SPI_DEV, &(MT6816_config_now.hw_spi_cfg));

	// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = ((168000000 / 2
			/ mt6816_config->refresh_rate_hz) - 1);
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(HW_ENC_TIM, &TIM_TimeBaseStructure);

	// Enable overflow interrupt
	TIM_ITConfig(HW_ENC_TIM, TIM_IT_Update, ENABLE);

	// Enable timer
	TIM_Cmd(HW_ENC_TIM, ENABLE);

	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);
	spi_error_rate = 0.0;
	encoder_no_magnet_error_rate = 0.0;

	MT6816_config_now.is_init = 1;
	mt6816_config->is_init = 1;
#endif
	return ENCODER_OK;
}

float MT6816_read_deg(void) {
	return last_enc_angle;
}

void MT6816_routine(void) {
	uint16_t pos;
	uint16_t reg_data_03;
	uint16_t reg_data_04;
	uint16_t reg_addr_03 = 0x8300;
	uint16_t reg_addr_04 = 0x8400;

	spi_bb_begin(&(MT6816_config_now.sw_spi));
	reg_data_03 = spiPolledExchange(&HW_SPI_DEV, reg_addr_03);
	spi_bb_end(&(MT6816_config_now.sw_spi));
	spi_bb_delay();
	spi_bb_begin(&(MT6816_config_now.sw_spi));
	reg_data_04 = spiPolledExchange(&HW_SPI_DEV, reg_addr_04);
	spi_bb_end(&(MT6816_config_now.sw_spi));

	pos = (reg_data_03 << 8) | reg_data_04;
	spi_val = pos;

	if (spi_bb_check_parity(pos)) {
		if (pos & MT6816_NO_MAGNET_ERROR_MASK) {
			++encoder_no_magnet_error_cnt;
			UTILS_LP_FAST(encoder_no_magnet_error_rate, 1.0,
					1. / MT6816_config_now.refresh_rate_hz);
		} else {
			pos = pos >> 2;
			last_enc_angle = ((float) pos * 360.0) / 16384.0;
			UTILS_LP_FAST(spi_error_rate, 0.0,
					1. / MT6816_config_now.refresh_rate_hz);
			UTILS_LP_FAST(encoder_no_magnet_error_rate, 0.0,
					1. / MT6816_config_now.refresh_rate_hz);
		}
	} else {
		++spi_error_cnt;
		UTILS_LP_FAST(spi_error_rate, 1.0,
				1. / MT6816_config_now.refresh_rate_hz);
	}

}

uint32_t MT6816_spi_get_val(void) {
	return spi_val;
}

uint32_t MT6816_spi_get_error_cnt(void) {
	return spi_error_cnt;
}

uint32_t MT6816_get_no_magnet_error_cnt(void) {
	return encoder_no_magnet_error_cnt;
}

uint32_t MT6816_get_no_magnet_error_rate(void) {
	return encoder_no_magnet_error_rate;
}
