#include "encoders.h"

#include "encoder/AS504x.h"
#include "encoder/MT6816.h"
#include "encoder/AD2S1205.h"
#include "encoder/encoder_hwconf.h"
#include "encoder/ABI.h"
#include "encoder/ENC_SINCOS.h"
#include "encoder/TS5700N8501.h"

#include "math.h"

static bool encoders_is_uart_defined(encoders_config_t *encoder_config);
static bool encoders_is_spi_defined(encoders_config_t *encoder_config);
static bool encoders_is_incremental_defined(encoders_config_t *encoder_config);

static encoders_type_t encoder_type_now = ENCODERS_TYPE_NONE;
static uint32_t enc_counts = 10000;
static bool index_found = false;

void encoders_deinit(void) {
	if (encoder_type_now == ENCODERS_TYPE_AS504x) {
		AS504x_deinit();
	} else if (encoder_type_now == ENCODERS_TYPE_MT6816) {
		MT6816_deinit();
	} else if (encoder_type_now == ENCODERS_TYPE_AD2S1205_SPI) {
		AD2S1205_deinit();
	} else if (encoder_type_now == ENCODERS_TYPE_ABI) {
		ABI_deinit();
	} else if (encoder_type_now == ENCODERS_TYPE_SINCOS) {
		ENC_SINCOS_deinit();
	} else if (encoder_type_now == ENCODERS_TYPE_TS5700N8501) {
		TS5700N8501_deinit();
	}

	encoder_type_now = ENCODERS_TYPE_NONE;
}

encoders_ret_t encoders_init(encoders_config_t *encoder_config) {

	if (encoder_type_now != ENCODERS_TYPE_NONE) {
		return ENCODERS_ERROR;
	}

	if (encoder_config->encoder_type == ENCODERS_TYPE_AS504x) {
		AS504x_config_t *as504x_config;
		encoders_ret_t encoder_ret;

		if(!encoders_is_spi_defined(encoder_config))
		{
			return ENCODERS_ERROR;
		}

		as504x_config = &(encoder_config->encspi);
		as504x_config->is_init = 0;

		encoder_ret = AS504x_init(as504x_config);

		if (ENCODERS_OK != encoder_ret || !as504x_config->is_init) {
			encoder_type_now = ENCODERS_TYPE_NONE; // TODO: maybe should be deleted
			index_found = false;
			return ENCODERS_ERROR;
		}
		encoder_type_now = ENCODERS_TYPE_AS504x;
		index_found = true;
		return ENCODERS_OK;
	} else if (encoder_config->encoder_type == ENCODERS_TYPE_MT6816) {
		MT6816_config_t *mt6816_config;
		encoders_ret_t encoder_ret;

		if(!encoders_is_spi_defined(encoder_config))
		{
			return ENCODERS_ERROR;
		}

		mt6816_config = &(encoder_config->encspi);
		mt6816_config->is_init = 0;

		encoder_ret = MT6816_init(mt6816_config);

		if (ENCODERS_OK != encoder_ret || !mt6816_config->is_init) {
			encoder_type_now = ENCODERS_TYPE_NONE;
			index_found = false;
			return ENCODERS_ERROR;
		}
		encoder_type_now = ENCODERS_TYPE_MT6816;
		index_found = true;
		return ENCODERS_OK;
	} else if (encoder_config->encoder_type == ENCODERS_TYPE_AD2S1205_SPI) {
		AD2S1205_config_t *ad2s1205_config;
		encoders_ret_t encoder_ret;

		if(!encoders_is_spi_defined(encoder_config))
		{
			return ENCODERS_ERROR;
		}

		ad2s1205_config = &(encoder_config->encspi);
		ad2s1205_config->is_init = 0;

		encoder_ret = AD2S1205_init(ad2s1205_config);

		if (ENCODERS_OK != encoder_ret || !ad2s1205_config->is_init) {
			encoder_type_now = ENCODERS_TYPE_NONE;
			index_found = false;
			return ENCODERS_ERROR;
		}
		encoder_type_now = ENCODERS_TYPE_AD2S1205_SPI;
		index_found = true;
		return ENCODERS_OK;
	} else if (encoder_config->encoder_type == ENCODERS_TYPE_ABI) {
		ABI_config_t *abi_config;
		encoders_ret_t encoder_ret;

		if(!encoders_is_incremental_defined(encoder_config))
		{
			return ENCODERS_ERROR;
		}

		abi_config = &(encoder_config->abi);
		abi_config->is_init = 0;

		encoder_ret = ABI_init(abi_config);

		if (ENCODERS_OK != encoder_ret || !abi_config->is_init) {
			encoder_type_now = ENCODERS_TYPE_NONE;
			index_found = false;
			return ENCODERS_ERROR;
		}
		encoder_type_now = ENCODERS_TYPE_ABI;
		index_found = true;
		return ENCODERS_OK;
	} else if (encoder_type_now == ENCODERS_TYPE_SINCOS) {
		ENCSINCOS_config_t *enc_sincos_config;
		encoders_ret_t encoder_ret;

		enc_sincos_config = &(encoder_config->encsincos);
		enc_sincos_config->is_init = 0;

		encoder_ret = ENC_SINCOS_init(enc_sincos_config);

		if (ENCODERS_OK != encoder_ret || !enc_sincos_config->is_init) {
			encoder_type_now = ENCODERS_TYPE_NONE;
			index_found = false;
			return ENCODERS_ERROR;
		}
		encoder_type_now = ENCODERS_TYPE_SINCOS;
		index_found = true;
		return ENCODERS_OK;
	} else if (encoder_type_now == ENCODERS_TYPE_TS5700N8501) {
		TS5700N8501_config_t *ts5700N8501_config;
		encoders_ret_t encoder_ret;

		if(!encoders_is_uart_defined(encoder_config))
		{
			return ENCODERS_ERROR;
		}

		ts5700N8501_config = &(encoder_config->ts5700n8501);

		encoder_ret = TS5700N8501_init(ts5700N8501_config);

		if (ENCODERS_OK != encoder_ret || !ts5700N8501_config->is_init) {
			encoder_type_now = ENCODERS_TYPE_NONE;
			index_found = false;
			return ENCODERS_ERROR;
		}
		encoder_type_now = ENCODERS_TYPE_TS5700N8501;
		index_found = true;
		return ENCODERS_OK;
	} else {
		encoder_type_now = ENCODERS_TYPE_NONE;
		index_found = false;
	}
	return ENCODERS_NONE;
}

float encoders_read_deg(void) {
	if (encoder_type_now == ENCODERS_TYPE_AS504x) {
		return AS504x_read_deg();
	} else if (encoder_type_now == ENCODERS_TYPE_MT6816) {
		return MT6816_read_deg();
	} else if (encoder_type_now == ENCODERS_TYPE_AD2S1205_SPI) {
		return AD2S1205_read_deg();
	} else if (encoder_type_now == ENCODERS_TYPE_ABI) {
		return ABI_read_deg();
	} else if (encoder_type_now == ENCODERS_TYPE_SINCOS) {
		return ENC_SINCOS_read_deg();
	} else if (encoder_type_now == ENCODERS_TYPE_TS5700N8501) {
		return TS5700N8501_read_deg();
	}
	return 0.0;
}

float encoders_read_deg_multiturn(void) {
	if (encoder_type_now == ENCODERS_TYPE_TS5700N8501) {
		encoders_get_abm();
		float ts_mt = (float)encoders_get_abm();
		if (fabsf(ts_mt) > 5000.0) {
			ts_mt = 0;
			encoders_reset_multiturn();
		}

		ts_mt += 5000;

		return encoders_read_deg() / 10000.0 + (360 * ts_mt) / 10000.0;
	} else {
		return encoders_read_deg();
	}
}

encoders_type_t encoders_is_configured(void) {
	return encoder_type_now;
}

bool encoders_index_found(void) {
	return index_found;
}

float encoders_spi_get_error_rate(void) {
	if (encoder_type_now == ENCODERS_TYPE_AS504x) {
		return AS504x_spi_get_error_rate();
	}
	return 0.0;
}

uint32_t encoders_spi_get_error_cnt(void) {
	if (encoder_type_now == ENCODERS_TYPE_AS504x) {
		return AS504x_spi_get_error_cnt();
	} else if (encoder_type_now == ENCODERS_TYPE_MT6816) {
		return MT6816_spi_get_error_cnt();
	}
	return 0;
}

uint32_t encoders_get_no_magnet_error_cnt(void) {
	if (encoder_type_now == ENCODERS_TYPE_MT6816) {
		return MT6816_get_no_magnet_error_cnt();
	}
	return 0;
}

float encoders_get_no_magnet_error_rate(void) {
	if (encoder_type_now == ENCODERS_TYPE_MT6816) {
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

void encoders_set_counts(uint32_t counts) {
	if (encoder_type_now == ENCODERS_TYPE_ABI)
	{
		ABI_set_counts(counts);
		index_found = false;
	}
}

uint32_t encoders_spi_get_val(void) {
	if (encoder_type_now == ENCODERS_TYPE_AS504x) {
		return AS504x_spi_get_val();
	} else if (encoder_type_now == ENCODERS_TYPE_MT6816) {
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

AS504x_diag encoders_get_diag(void) {
	AS504x_diag diag = {0};
	if (encoder_type_now == ENCODERS_TYPE_AS504x) {
		diag = AS504x_get_diag();
	}
	return diag;
}

uint8_t* encoders_get_raw_status(void) {
	if (encoder_type_now == ENCODERS_TYPE_TS5700N8501) {
		return TS5700N8501_get_raw_status();
	}
	return NULL;
}

int16_t encoders_get_abm(void) {
	if (encoder_type_now == ENCODERS_TYPE_TS5700N8501) {
		return TS5700N8501_get_abm();
	}
	return 0;
}

void encoders_reset_errors(void) {
	if (encoder_type_now == ENCODERS_TYPE_TS5700N8501) {
		return TS5700N8501_reset_errors();
	}
}

void encoders_reset_multiturn(void) {
	if (encoder_type_now == ENCODERS_TYPE_TS5700N8501) {
		return TS5700N8501_reset_multiturn();
	}
}

uint32_t encoders_get_signal_below_min_error_cnt(void) {
	if (encoder_type_now == ENCODERS_TYPE_SINCOS) {
		return ENC_SINCOS_get_signal_below_min_error_cnt();
	}
	return 0;
}
uint32_t encoders_get_signal_above_max_error_cnt(void) {
	if (encoder_type_now == ENCODERS_TYPE_SINCOS) {
		return ENC_SINCOS_get_signal_above_max_error_cnt();
	}
	return 0;
}
float encoders_get_signal_below_min_error_rate(void) {
	if (encoder_type_now == ENCODERS_TYPE_SINCOS) {
		return ENC_SINCOS_get_signal_below_min_error_rate();
	}
	return 0.0;
}
float encoders_get_signal_above_max_error_rate(void) {
	if (encoder_type_now == ENCODERS_TYPE_SINCOS) {
		return ENC_SINCOS_get_signal_above_max_error_rate();
	}
	return 0.0;
}

void encoders_tim_isr(void) {
	if (encoder_type_now == ENCODERS_TYPE_AS504x) {
		AS504x_routine();
	} else if (encoder_type_now == ENCODERS_TYPE_MT6816) {
		MT6816_routine();
	} else if (encoder_type_now == ENCODERS_TYPE_AD2S1205_SPI) {
		AD2S1205_routine();
	}
}

static bool encoders_is_uart_defined(encoders_config_t *encoder_config)
{
	if (!encoder_config->ts5700n8501.uart_config.gpio_RX.port
			|| !encoder_config->ts5700n8501.uart_config.gpio_TX.port)
	{
		return false;
	}
	return true;
}

static bool encoders_is_spi_defined(encoders_config_t *encoder_config)
{
	if (!encoder_config->encspi.spi_config.gpio_miso.port
			|| !encoder_config->encspi.spi_config.gpio_nss.port
			|| !encoder_config->encspi.spi_config.gpio_sck.port)
	{
		return false;
	}
	return true;
}

static bool encoders_is_incremental_defined(encoders_config_t *encoder_config)
{
	if (!encoder_config->abi.incremental_config.gpio_A.port
			|| !encoder_config->abi.incremental_config.gpio_B.port)
	{
		return false;
	}
	return true;
}

