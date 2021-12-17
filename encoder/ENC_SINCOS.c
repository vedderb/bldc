
#include "encoder/ENC_SINCOS.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>

ENCSINCOS_config_t enc_sincos_config_now = { 0 };

static float sin_gain = 0.0;
static float sin_offset = 0.0;
static float cos_gain = 0.0;
static float cos_offset = 0.0;
static float sincos_filter_constant = 0.0;
static uint32_t sincos_signal_below_min_error_cnt = 0;
static uint32_t sincos_signal_above_max_error_cnt = 0;
static float sincos_signal_low_error_rate = 0.0;
static float sincos_signal_above_max_error_rate = 0.0;

static float last_enc_angle = 0.0;

void ENC_SINCOS_deinit(void) {
	last_enc_angle = 0.0;

	sincos_signal_low_error_rate = 0.0;
	sincos_signal_above_max_error_rate = 0.0;
	enc_sincos_config_now.is_init = 0;
}

encoders_ret_t ENC_SINCOS_init(ENCSINCOS_config_t *enc_sincos_config) {
	//ADC inputs are already initialized in hw_init_gpio()
	sin_gain = enc_sincos_config->s_gain;
	sin_offset = enc_sincos_config->s_offset;
	cos_gain = enc_sincos_config->c_gain;
	cos_offset = enc_sincos_config->c_offset;
	sincos_filter_constant = enc_sincos_config->filter_constant;

	sincos_signal_below_min_error_cnt = 0;
	sincos_signal_above_max_error_cnt = 0;
	sincos_signal_low_error_rate = 0.0;
	sincos_signal_above_max_error_rate = 0.0;
	last_enc_angle = 0.0;

	// ADC measurements needs to be in sync with motor PWM
#ifdef HW_HAS_SIN_COS_ENCODER
	enc_sincos_config->is_init = 1;
	return ENCODERS_OK;
#else
	enc_sincos_config->is_init = 0;
	return ENCODERS_ERROR;
#endif
}

float ENC_SINCOS_read_deg(void) {
#ifdef HW_HAS_SIN_COS_ENCODER
	float angle;
	float sin = ENCODER_SIN_VOLTS * sin_gain - sin_offset;
	float cos = ENCODER_COS_VOLTS * cos_gain - cos_offset;

	float module = SQ(sin) + SQ(cos);

	if (module > SQ(SINCOS_MAX_AMPLITUDE) )	{
		// signals vector outside of the valid area. Increase error count and discard measurement
		++sincos_signal_above_max_error_cnt;
		UTILS_LP_FAST(sincos_signal_above_max_error_rate, 1.0, 1./SINCOS_SAMPLE_RATE_HZ);
		angle = last_enc_angle;
	} else {
		if (module < SQ(SINCOS_MIN_AMPLITUDE)) {
			++sincos_signal_below_min_error_cnt;
			UTILS_LP_FAST(sincos_signal_low_error_rate, 1.0, 1./SINCOS_SAMPLE_RATE_HZ);
			angle = last_enc_angle;
		} else {
			UTILS_LP_FAST(sincos_signal_above_max_error_rate, 0.0, 1./SINCOS_SAMPLE_RATE_HZ);
			UTILS_LP_FAST(sincos_signal_low_error_rate, 0.0, 1./SINCOS_SAMPLE_RATE_HZ);

			float angle_tmp = RAD2DEG_f(utils_fast_atan2(sin, cos));
			UTILS_LP_FAST(angle, angle_tmp, sincos_filter_constant);
			last_enc_angle = angle;
		}
	}
#endif
	return last_enc_angle;
}
