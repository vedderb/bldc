
#include "encoders.h"

static encoders_type_t encoder_type_now = ENCODERS_TYPE_NONE; // TODO: CHANGE TO encoder_config_now
static uint32_t enc_counts = 10000;
static bool index_found = false;

encoders_ret_t encoders_init(encoders_config_t *encoder_config)
{

	//TODO: ASSERTS

	if(encoder_type_now != ENCODERS_TYPE_NONE)
	{
		return ENCODERS_ERROR;
	}

	if(encoder_config->encoder_type == ENCODERS_TYPE_AS504x)
	{
		AS504x_config_t as504x_config;
		encoders_ret_t encoder_ret;

		as504x_config.spi_config = encoder_config->spi_config;
		as504x_config.refresh_rate_hz = encoder_config->refresh_rate_hz;
		encoder_ret = AS504x_init(&as504x_config);

		if(ENCODERS_OK != encoder_ret || !as504x_config.is_init)
		{
			encoder_type_now = ENCODERS_TYPE_NONE; // TODO: maybe should be deleted
			index_found = false;
			return ENCODERS_ERROR;
		}
		encoder_type_now = ENCODERS_TYPE_AS504x;
		index_found = true;
		return ENCODERS_OK;
	}
	else if(encoder_config->encoder_type == ENCODERS_TYPE_MT6816)
	{
		MT6816_config_t mt6816_config;
		encoders_ret_t encoder_ret;

		mt6816_config.spi_config = encoder_config->spi_config;
		mt6816_config.refresh_rate_hz = encoder_config->refresh_rate_hz;

		encoder_ret = MT6816_init(&mt6816_config);

		if(ENCODERS_OK != encoder_ret || !mt6816_config.is_init)
		{
			encoder_type_now = ENCODERS_TYPE_NONE;
			index_found = false;
			return ENCODERS_ERROR;
		}
		encoder_type_now = ENCODERS_TYPE_MT6816;
		index_found = true;
		return ENCODERS_OK;
	}
	else if(encoder_config->encoder_type == ENCODERS_TYPE_AD2S1205_SPI)
	{
		AD2S1205_config_t AD2S1205_config;
		encoders_ret_t encoder_ret;

		AD2S1205_config.spi_config = encoder_config->spi_config;
		AD2S1205_config.refresh_rate_hz = encoder_config->refresh_rate_hz;

		encoder_ret = AD2S1205_init(&AD2S1205_config);

		if(ENCODERS_OK != encoder_ret || !AD2S1205_config.is_init)
		{
			encoder_type_now = ENCODERS_TYPE_NONE;
			index_found = false;
			return ENCODERS_ERROR;
		}
		encoder_type_now = ENCODERS_TYPE_AD2S1205_SPI;
		index_found = true;
		return ENCODERS_OK;
	}
	else
	{
		encoder_type_now = ENCODERS_TYPE_NONE;
		index_found = false;
	}
	return ENCODERS_NONE;
}


float encoders_read_deg(void)
{
	if(encoder_type_now == ENCODERS_TYPE_AS504x)
	{
		return AS504x_read_deg();
	}
	else if(encoder_type_now == ENCODERS_TYPE_MT6816)
	{
		return MT6816_read_deg();
	}
	else if(encoder_type_now == ENCODERS_TYPE_AD2S1205_SPI)
	{
		return AD2S1205_read_deg();
	}
	return 0.0;
}

encoders_type_t encoders_is_configured(void)
{
	return encoder_type_now;
}

bool encoders_index_found(void)
{
  return index_found;
}

void encoders_deinit(void)
{
	if(encoder_type_now == ENCODERS_TYPE_AS504x)
	{
		AS504x_deinit();
	}
	else if(encoder_type_now == ENCODERS_TYPE_MT6816)
	{
		MT6816_deinit();
	}
	else if(encoder_type_now == ENCODERS_TYPE_AD2S1205_SPI)
	{
		AD2S1205_deinit();
	}
}

float encoders_spi_get_error_rate(void) {
	if(encoder_type_now == ENCODERS_TYPE_AS504x)
	{
		return AS504x_spi_get_error_rate();
	}
	return 0.0;
}

uint32_t encoders_spi_get_error_cnt(void)
{
	if(encoder_type_now == ENCODERS_TYPE_AS504x)
	{
		return AS504x_spi_get_error_cnt();
	}
	else if(encoder_type_now == ENCODERS_TYPE_MT6816)
	{
		return MT6816_spi_get_error_cnt();
	}
	return 0;
}

uint32_t encoders_get_no_magnet_error_cnt(void)
{
  if(encoder_type_now == ENCODERS_TYPE_MT6816)
  {
    return MT6816_get_no_magnet_error_cnt();
  }
  return 0;
}

float encoders_get_no_magnet_error_rate(void)
{
  if(encoder_type_now == ENCODERS_TYPE_MT6816)
  {
    return MT6816_get_no_magnet_error_rate();
  }
  return 0.0;
}
float encoders_resolver_loss_of_tracking_error_rate(void) {
	return AD2S1205_resolver_loss_of_tracking_error_rate();
}

float encoders_resolver_degradation_of_signal_error_rate(void) {
	return AD2S1205_resolver_degradation_of_signal_error_rate();
}

float encoders_resolver_loss_of_signal_error_rate(void) {
	return AD2S1205_resolver_loss_of_signal_error_rate();
}

uint32_t encoders_resolver_loss_of_tracking_error_cnt(void) {
	return AD2S1205_resolver_loss_of_tracking_error_cnt();
}

uint32_t encoders_resolver_degradation_of_signal_error_cnt(void) {
	return AD2S1205_resolver_degradation_of_signal_error_cnt();
}

uint32_t encoders_resolver_loss_of_signal_error_cnt(void) {
	return AD2S1205_resolver_loss_of_signal_error_cnt();
}

uint32_t encoders_spi_get_val(void)
{
	if(encoder_type_now == ENCODERS_TYPE_AS504x)
	{
		return AS504x_spi_get_val();
	}
	else if(encoder_type_now == ENCODERS_TYPE_MT6816)
	{
		return MT6816_spi_get_val();
	}
	return 0;
}

void encoders_reset(void) {
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

        if (encoders_index_found()) {
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

void encoders_tim_isr(void)
{
	if(encoder_type_now == ENCODERS_TYPE_AS504x)
	{
		AS504x_routine();
	}
	else if(encoder_type_now == ENCODERS_TYPE_MT6816)
	{
		return MT6816_routine();
	}
	else if(encoder_type_now == ENCODERS_TYPE_AD2S1205_SPI)
	{
		return AD2S1205_routine();
	}
}

