


#include "encoder/AD2S1205.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>

static AD2S1205_config_t AD2S1205_config_now = {0};

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

static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length);
static void spi_begin(void);
static void spi_end(void);
static void spi_delay(void);
static void spi_AS5047_cs_delay(void);
static bool spi_check_parity(uint16_t x);

void AD2S1205_deinit(void)
{
		nvicDisableVector(HW_ENC_EXTI_CH);
		nvicDisableVector(HW_ENC_TIM_ISR_CH);

		TIM_DeInit(HW_ENC_TIM);

		palSetPadMode(AD2S1205_config_now.spi_config.gpio_miso.port, AD2S1205_config_now.spi_config.gpio_miso.pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(AD2S1205_config_now.spi_config.gpio_sck.port, AD2S1205_config_now.spi_config.gpio_sck.pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(AD2S1205_config_now.spi_config.gpio_nss.port, AD2S1205_config_now.spi_config.gpio_nss.pin, PAL_MODE_INPUT_PULLUP);

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

}

encoders_ret_t AD2S1205_init(AD2S1205_config_t* AD2S1205_config)
{

	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	encoders_spi_config_t AD2S1205_spi_config = AD2S1205_config->spi_config;

	resolver_loss_of_tracking_error_rate = 0.0;
	resolver_degradation_of_signal_error_rate = 0.0;
	resolver_loss_of_signal_error_rate = 0.0;
	resolver_loss_of_tracking_error_cnt = 0;
	resolver_loss_of_signal_error_cnt = 0;

	palSetPadMode(AD2S1205_spi_config.gpio_miso.port, AD2S1205_spi_config.gpio_miso.pin, PAL_MODE_INPUT);
	palSetPadMode(AD2S1205_spi_config.gpio_sck.port, AD2S1205_spi_config.gpio_sck.pin, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(AD2S1205_spi_config.gpio_nss.port, AD2S1205_spi_config.gpio_nss.pin, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	// Set MOSI to 1
#if AD2S1205_USE_HW_SPI_PINS
	palSetPadMode(AD2S1205_spi_config.gpio_mosi.port, AD2S1205_spi_config.gpio_mosi.pin, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(AD2S1205_spi_config.gpio_mosi.port, AD2S1205_spi_config.gpio_mosi.pin);
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

	AD2S1205_config_now = *AD2S1205_config;

	// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = ((168000000 / 2 / AD2S1205_config->refresh_rate_hz) - 1);
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(HW_ENC_TIM, &TIM_TimeBaseStructure);

	// Enable overflow interrupt
	TIM_ITConfig(HW_ENC_TIM, TIM_IT_Update, ENABLE);

	// Enable timer
	TIM_Cmd(HW_ENC_TIM, ENABLE);

	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);

	AD2S1205_config_now.is_init = 1;
	AD2S1205_config->is_init = 1;

	return ENCODERS_OK;
}

void AD2S1205_routine(void)
{
	uint16_t pos;
	// SAMPLE signal should have been be asserted in sync with ADC sampling
#ifdef AD2S1205_RDVEL_GPIO
	palSetPad(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN);	// Always read position
#endif

	palSetPad(AD2S1205_config_now.spi_config.gpio_sck.port, AD2S1205_config_now.spi_config.gpio_sck.pin);
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
			UTILS_LP_FAST(spi_error_rate, 0.0, 1./AD2S1205_config_now.refresh_rate_hz);
		} else {
			angle_is_correct = false;
			++spi_error_cnt;
			UTILS_LP_FAST(spi_error_rate, 1.0, 1./AD2S1205_config_now.refresh_rate_hz);
		}

		pos &= 0xFFF0;
		pos = pos >> 4;
		pos &= 0x0FFF;

		if(LOT) {
			angle_is_correct = false;
			++resolver_loss_of_tracking_error_cnt;
			UTILS_LP_FAST(resolver_loss_of_tracking_error_rate, 1.0, 1./AD2S1205_config_now.refresh_rate_hz);
		} else {
			UTILS_LP_FAST(resolver_loss_of_tracking_error_rate, 0.0, 1./AD2S1205_config_now.refresh_rate_hz);
		}

		if(DOS) {
			angle_is_correct = false;
			++resolver_degradation_of_signal_error_cnt;
			UTILS_LP_FAST(resolver_degradation_of_signal_error_rate, 1.0, 1./AD2S1205_config_now.refresh_rate_hz);
		} else {
			UTILS_LP_FAST(resolver_degradation_of_signal_error_rate, 0.0, 1./AD2S1205_config_now.refresh_rate_hz);
		}

		if(LOS) {
			angle_is_correct = false;
			++resolver_loss_of_signal_error_cnt;
			UTILS_LP_FAST(resolver_loss_of_signal_error_rate, 1.0, 1./AD2S1205_config_now.refresh_rate_hz);
		} else {
			UTILS_LP_FAST(resolver_loss_of_signal_error_rate, 0.0, 1./AD2S1205_config_now.refresh_rate_hz);
		}

		if(angle_is_correct)
		{
			last_enc_angle = ((float)pos * 360.0) / 4096.0;
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

uint32_t AD2S1205_spi_get_error_cnt(void)
{
	return spi_error_cnt;
}

float AD2S1205_read_deg(void)
{
	return last_enc_angle;
}

static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length) {
	const encoders_gpio_t gpio_miso = AD2S1205_config_now.spi_config.gpio_miso;
#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
	const encoders_gpio_t gpio_mosi = AD2S1205_config_now.spi_config.gpio_mosi;
#endif
	const encoders_gpio_t gpio_sck = AD2S1205_config_now.spi_config.gpio_sck;
	for (int i = 0;i < length;i++) {

#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
		uint16_t send = out_buf ? out_buf[i] : 0xFFFF;
#else
		(void)out_buf;
#endif

		uint16_t receive = 0;

		for (int bit = 0;bit < 16;bit++) {
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

static void spi_begin(void) {
	palClearPad(AD2S1205_config_now.spi_config.gpio_nss.port, AD2S1205_config_now.spi_config.gpio_nss.pin);
	spi_AS5047_cs_delay();
}

static void spi_end(void) {
	palSetPad(AD2S1205_config_now.spi_config.gpio_nss.port, AD2S1205_config_now.spi_config.gpio_nss.pin);
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
