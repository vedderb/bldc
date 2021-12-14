

#include "encoder/MT6816.h"


#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>

#define SPI_BaudRatePrescaler_2         ((uint16_t)0x0000) //  42 MHz      21 MHZ
#define SPI_BaudRatePrescaler_4         ((uint16_t)0x0008) //  21 MHz      10.5 MHz
#define SPI_BaudRatePrescaler_8         ((uint16_t)0x0010) //  10.5 MHz    5.25 MHz
#define SPI_BaudRatePrescaler_16        ((uint16_t)0x0018) //  5.25 MHz    2.626 MHz
#define SPI_BaudRatePrescaler_32        ((uint16_t)0x0020) //  2.626 MHz   1.3125 MHz
#define SPI_BaudRatePrescaler_64        ((uint16_t)0x0028) //  1.3125 MHz  656.25 KHz
#define SPI_BaudRatePrescaler_128       ((uint16_t)0x0030) //  656.25 KHz  328.125 KHz
#define SPI_BaudRatePrescaler_256       ((uint16_t)0x0038) //  328.125 KHz 164.06 KHz
#define SPI_DATASIZE_16BIT				SPI_CR1_DFF

#define MT6816_NO_MAGNET_ERROR_MASK		0x0002

static void spi_begin(void);
static void spi_end(void);
static void spi_delay(void);
static void spi_AS5047_cs_delay(void);
static bool spi_check_parity(uint16_t x);

#ifdef HW_SPI_DEV
static SPIConfig mt6816_spi_cfg = {0};
#endif

static MT6816_config_t mt6816_config_now = {0};

static float spi_error_rate = 0.0;
static float encoder_no_magnet_error_rate = 0.0;
static float encoder_no_magnet_error_cnt = 0.0;
static float last_enc_angle = 0.0;
static uint32_t spi_error_cnt = 0;
static uint32_t spi_val = 0;

void MT6816_deinit(void)
{

	nvicDisableVector(HW_ENC_EXTI_CH);
	nvicDisableVector(HW_ENC_TIM_ISR_CH);

	TIM_DeInit(HW_ENC_TIM);

	palSetPadMode(mt6816_config_now.spi_config.gpio_miso.port, mt6816_config_now.spi_config.gpio_miso.pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(mt6816_config_now.spi_config.gpio_sck.port, mt6816_config_now.spi_config.gpio_sck.pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(mt6816_config_now.spi_config.gpio_nss.port, mt6816_config_now.spi_config.gpio_nss.pin, PAL_MODE_INPUT_PULLUP);

#if (MT6816_USE_HW_SPI_PINS)
	palSetPadMode(mt6816_config_now.spi_config.gpio_mosi.port, mt6816_config_now.spi_config.gpio_mosi.pin, PAL_MODE_INPUT_PULLUP);
#endif

#ifdef HW_SPI_DEV
	spiStop(&HW_SPI_DEV);
#endif

	//palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_INPUT_PULLUP); //TODO: is this necessary
	//palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_INPUT_PULLUP); //TODO: is this necessary

	palSetPadMode(mt6816_config_now.spi_config.gpio_miso.port, mt6816_config_now.spi_config.gpio_miso.pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(mt6816_config_now.spi_config.gpio_sck.port, mt6816_config_now.spi_config.gpio_sck.pin, PAL_MODE_INPUT_PULLUP);

	mt6816_config_now.is_init = 0;
	last_enc_angle = 0.0;
	spi_error_rate = 0.0;
}

encoders_ret_t MT6816_init(MT6816_config_t* mt6816_config)
{
#ifdef HW_SPI_DEV
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	encoders_spi_config_t mt6816_spi_config = mt6816_config->spi_config;

	palSetPadMode(mt6816_spi_config.gpio_sck.port, mt6816_spi_config.gpio_sck.pin, PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(mt6816_spi_config.gpio_miso.port, mt6816_spi_config.gpio_miso.pin, PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(mt6816_spi_config.gpio_nss.port, mt6816_spi_config.gpio_nss.pin, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

#if (MT6816_USE_HW_SPI_PINS)
	palSetPadMode(mt6816_spi_config.gpio_mosi.port, mt6816_spi_config.gpio_mosi.pin, PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);
#endif

	mt6816_config_now = *mt6816_config;

	mt6816_spi_cfg.end_cb = NULL;
	mt6816_spi_cfg.ssport = mt6816_spi_config.gpio_nss.port;
	mt6816_spi_cfg.sspad =mt6816_spi_config.gpio_nss.pin;
	mt6816_spi_cfg.cr1 = SPI_BaudRatePrescaler_4 | SPI_CR1_CPOL | SPI_CR1_CPHA | SPI_DATASIZE_16BIT;

	//Start driver with MT6816 SPI settings
		spiStart(&HW_SPI_DEV, &mt6816_spi_cfg);

		// Enable timer clock
		HW_ENC_TIM_CLK_EN();

		// Time Base configuration
		TIM_TimeBaseStructure.TIM_Prescaler = 0;
		TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
		TIM_TimeBaseStructure.TIM_Period = ((168000000 / 2 / mt6816_config->refresh_rate_hz) - 1);
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

		mt6816_config_now.is_init = 1;
		mt6816_config->is_init = 1;
#endif
		return ENCODERS_OK;
}
void MT6816_routine(void)
{
			uint16_t pos;
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
					UTILS_LP_FAST(encoder_no_magnet_error_rate, 1.0, 1./mt6816_config_now.refresh_rate_hz);
				} else {
					pos = pos >> 2;
					last_enc_angle = ((float)pos * 360.0) / 16384.0;
					UTILS_LP_FAST(spi_error_rate, 0.0, 1./mt6816_config_now.refresh_rate_hz);
					UTILS_LP_FAST(encoder_no_magnet_error_rate, 0.0, 1./mt6816_config_now.refresh_rate_hz);
				}
			} else {
				++spi_error_cnt;
				UTILS_LP_FAST(spi_error_rate, 1.0, 1./mt6816_config_now.refresh_rate_hz);
			}

}

uint32_t MT6816_spi_get_val(void)
{
	return spi_val;
}

uint32_t MT6816_spi_get_error_cnt(void)
{
	return spi_error_cnt;
}

uint32_t MT6816_get_no_magnet_error_cnt(void)
{
  return encoder_no_magnet_error_cnt;
}

uint32_t MT6816_get_no_magnet_error_rate(void)
{
  return encoder_no_magnet_error_rate;
}

float MT6816_read_deg(void)
{
 return last_enc_angle;
}

static void spi_begin(void) {
	palClearPad(mt6816_config_now.spi_config.gpio_nss.port, mt6816_config_now.spi_config.gpio_nss.pin);
	spi_AS5047_cs_delay();
}

static void spi_end(void) {
	palSetPad(mt6816_config_now.spi_config.gpio_nss.port, mt6816_config_now.spi_config.gpio_nss.pin);
	spi_AS5047_cs_delay();
}

static void spi_delay(void) {
	__NOP();
	__NOP();
	__NOP();
	__NOP();
}

static void spi_AS5047_cs_delay(void) {
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();__NOP();__NOP();
	__NOP();
}

static bool spi_check_parity(uint16_t x) {
	x ^= x >> 8;
	x ^= x >> 4;
	x ^= x >> 2;
	x ^= x >> 1;
	return (~x) & 1;
}
