#include "encoder/AS504x.h"
#include "encoder/MT6816.h"
#include "encoder/AD2S1205.h"
#include "encoder/encoder_hwconf.h"
#include "encoder/ABI.h"
#include "encoder/ENC_SINCOS.h"
#include "encoder/TS5700N8501.h"
#include "encoder.h"

static bool encoder_is_uart_defined(void);
static bool encoder_is_spi_defined(void);
static bool encoder_is_incremental_defined(void);

static encoder_type_t encoder_type_now = ENCODER_TYPE_NONE;
static uint32_t enc_counts = 10000;
static bool index_found = false;

void encoder_deinit(void) {
	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		AS504x_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_MT6816) {
		MT6816_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_AD2S1205_SPI) {
		AD2S1205_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_ABI) {
		ABI_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_SINCOS) {
		ENC_SINCOS_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		TS5700N8501_deinit();
	}

	encoder_type_now = ENCODER_TYPE_NONE;
}

encoder_ret_t encoder_init(encoder_type_t encoder_type) {

	if (encoder_type_now != ENCODER_TYPE_NONE) {
		return ENCODER_ERROR;
	}

	if (encoder_type == ENCODER_TYPE_AS504x) {
		encoder_ret_t encoder_ret;

		if (encoder_is_spi_defined() == false) {
			return ENCODER_ERROR;
		}

		encoders_conf_ENCSPI.is_init = 0;

		encoder_ret = AS504x_init(&encoders_conf_ENCSPI);

		if (ENCODER_OK != encoder_ret || !encoders_conf_ENCSPI.is_init) {
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_AS504x;
		index_found = true;
		return ENCODER_OK;
	} else if (encoder_type == ENCODER_TYPE_MT6816) {
		encoder_ret_t encoder_ret;

		if (encoder_is_spi_defined() == false) {
			return ENCODER_ERROR;
		}

		encoders_conf_ENCSPI.is_init = 0;

		encoder_ret = MT6816_init(&encoders_conf_ENCSPI);

		if (ENCODER_OK != encoder_ret || !encoders_conf_ENCSPI.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_MT6816;
		index_found = true;
		return ENCODER_OK;
	} else if (encoder_type == ENCODER_TYPE_AD2S1205_SPI) {
		encoder_ret_t encoder_ret;

		if (encoder_is_spi_defined() == false) {
			return ENCODER_ERROR;
		}

		encoders_conf_ENCSPI.is_init = 0;

		encoder_ret = AD2S1205_init(&encoders_conf_ENCSPI);

		if (ENCODER_OK != encoder_ret || !encoders_conf_ENCSPI.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_AD2S1205_SPI;
		index_found = true;
		return ENCODER_OK;
	} else if (encoder_type == ENCODER_TYPE_ABI) {
		encoder_ret_t encoder_ret;

		if (encoder_is_incremental_defined() == false) {
			return ENCODER_ERROR;
		}

		encoders_conf_ABI.is_init = 0;

		encoder_ret = ABI_init(&encoders_conf_ABI);

		if (ENCODER_OK != encoder_ret || !encoders_conf_ABI.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_ABI;
		index_found = true;
		return ENCODER_OK;
	} else if (encoder_type == ENCODER_TYPE_SINCOS) {
		encoder_ret_t encoder_ret;

		encoders_conf_ENCSINCOS.is_init = 0;

		encoder_ret = ENC_SINCOS_init(&encoders_conf_ENCSINCOS);

		if (ENCODER_OK != encoder_ret || !encoders_conf_ENCSINCOS.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_SINCOS;
		index_found = true;
		return ENCODER_OK;
	} else if (encoder_type == ENCODER_TYPE_TS5700N8501) {
		encoder_ret_t encoder_ret;

		if (encoder_is_uart_defined() == false) {
			return ENCODER_ERROR;
		}

		encoders_conf_TS5700N8501.is_init = 0;

		encoder_ret = TS5700N8501_init(&encoders_conf_TS5700N8501);

		if (ENCODER_OK != encoder_ret || !encoders_conf_TS5700N8501.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_TS5700N8501;
		index_found = true;
		return ENCODER_OK;
	} else {
		encoder_type_now = ENCODER_TYPE_NONE;
		index_found = false;
	}
	return ENCODER_NONE;
}

float encoder_read_deg(void) {
	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		return AS504x_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_MT6816) {
		return MT6816_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_AD2S1205_SPI) {
		return AD2S1205_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_ABI) {
		return ABI_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_SINCOS) {
		return ENC_SINCOS_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		return TS5700N8501_read_deg();
	}
	return 0.0;
}

float encoder_read_deg_multiturn(void) {
	if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		encoder_get_abm();
		float ts_mt = (float) encoder_get_abm();
		if (fabsf(ts_mt) > 5000.0) {
			ts_mt = 0;
			encoder_reset_multiturn();
		}

		ts_mt += 5000;

		return encoder_read_deg() / 10000.0 + (360 * ts_mt) / 10000.0;
	} else {
		return encoder_read_deg();
	}
}

encoder_type_t encoder_is_configured(void) {
	return encoder_type_now;
}

bool encoder_index_found(void) {
	return index_found;
}

float encoder_spi_get_error_rate(void) {
	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		return AS504x_spi_get_error_rate();
	}
	return 0.0;
}

uint32_t encoder_spi_get_error_cnt(void) {
	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		return AS504x_spi_get_error_cnt();
	} else if (encoder_type_now == ENCODER_TYPE_MT6816) {
		return MT6816_spi_get_error_cnt();
	}
	return 0;
}

uint32_t encoder_get_no_magnet_error_cnt(void) {
	if (encoder_type_now == ENCODER_TYPE_MT6816) {
		return MT6816_get_no_magnet_error_cnt();
	}
	return 0;
}

float encoder_get_no_magnet_error_rate(void) {
	if (encoder_type_now == ENCODER_TYPE_MT6816) {
		return MT6816_get_no_magnet_error_rate();
	}
	return 0.0;
}
float encoder_resolver_loss_of_tracking_error_rate(void) {
	return AD2S1205_resolver_loss_of_tracking_error_rate();
}

float encoder_resolver_degradation_of_signal_error_rate(void) {
	return AD2S1205_resolver_degradation_of_signal_error_rate();
}

float encoder_resolver_loss_of_signal_error_rate(void) {
	return AD2S1205_resolver_loss_of_signal_error_rate();
}

uint32_t encoder_resolver_loss_of_tracking_error_cnt(void) {
	return AD2S1205_resolver_loss_of_tracking_error_cnt();
}

uint32_t encoder_resolver_degradation_of_signal_error_cnt(void) {
	return AD2S1205_resolver_degradation_of_signal_error_cnt();
}

uint32_t encoder_resolver_loss_of_signal_error_cnt(void) {
	return AD2S1205_resolver_loss_of_signal_error_cnt();
}

// ABI
void encoder_set_counts(uint32_t counts) {
	if (encoder_type_now == ENCODER_TYPE_ABI) {
		ABI_set_counts(counts);
		index_found = false;
	}
}

uint32_t encoder_spi_get_val(void) {
	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		return AS504x_spi_get_val();
	} else if (encoder_type_now == ENCODER_TYPE_MT6816) {
		return MT6816_spi_get_val();
	}
	return 0;
}

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

		if (encoder_index_found()) {
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

AS504x_diag encoder_get_diag(void) {
	AS504x_diag diag = { 0 };
	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		diag = AS504x_get_diag();
	}
	return diag;
}

uint8_t* encoder_get_raw_status(void) {
	if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		return TS5700N8501_get_raw_status();
	}
	return NULL;
}

int16_t encoder_get_abm(void) {
	if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		return TS5700N8501_get_abm();
	}
	return 0;
}

void encoder_reset_errors(void) {
	if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		return TS5700N8501_reset_errors();
	}
}

void encoder_reset_multiturn(void) {
	if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		return TS5700N8501_reset_multiturn();
	}
}

// SINCOS TODO labels
uint32_t encoder_get_signal_below_min_error_cnt(void) {
	if (encoder_type_now == ENCODER_TYPE_SINCOS) {
		return ENC_SINCOS_get_signal_below_min_error_cnt();
	}
	return 0;
}
uint32_t encoder_get_signal_above_max_error_cnt(void) {
	if (encoder_type_now == ENCODER_TYPE_SINCOS) {
		return ENC_SINCOS_get_signal_above_max_error_cnt();
	}
	return 0;
}
float encoder_get_signal_below_min_error_rate(void) {
	if (encoder_type_now == ENCODER_TYPE_SINCOS) {
		return ENC_SINCOS_get_signal_below_min_error_rate();
	}
	return 0.0;
}
float encoder_get_signal_above_max_error_rate(void) {
	if (encoder_type_now == ENCODER_TYPE_SINCOS) {
		return ENC_SINCOS_get_signal_above_max_error_rate();
	}
	return 0.0;
}

void encoder_sincos_conf_set(ENCSINCOS_config_t *sincos_config) {
	encoders_conf_ENCSINCOS.s_gain = sincos_config->s_gain;
	encoders_conf_ENCSINCOS.s_offset = sincos_config->s_offset;
	encoders_conf_ENCSINCOS.c_gain = sincos_config->c_gain;
	encoders_conf_ENCSINCOS.c_offset = sincos_config->c_offset;
	encoders_conf_ENCSINCOS.filter_constant = sincos_config->filter_constant;
}

void encoder_tim_isr(void) {
	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		AS504x_routine();
	} else if (encoder_type_now == ENCODER_TYPE_MT6816) {
		MT6816_routine();
	} else if (encoder_type_now == ENCODER_TYPE_AD2S1205_SPI) {
		AD2S1205_routine();
	}
}

static bool encoder_is_uart_defined(void) {
	if (!encoders_conf_TS5700N8501.uart_config.gpio_RX.port
			|| !encoders_conf_TS5700N8501.uart_config.gpio_TX.port) {
		return false;
	}
	return true;
}

static bool encoder_is_spi_defined(void) {
	if (!encoders_conf_ENCSPI.spi_config.gpio_miso.port
			|| !encoders_conf_ENCSPI.spi_config.gpio_nss.port
			|| !encoders_conf_ENCSPI.spi_config.gpio_sck.port) {
		return false;
	}
	return true;
}

static bool encoder_is_incremental_defined(void) {
	if (!encoders_conf_ABI.incremental_config.gpio_A.port
			|| !encoders_conf_ABI.incremental_config.gpio_B.port) {
		return false;
	}
	return true;
}

