
#ifndef ENCODER_ENC_SINCOS_H_
#define ENCODER_ENC_SINCOS_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void ENC_SINCOS_deinit(void);
encoder_ret_t ENC_SINCOS_init(ENCSINCOS_config_t *enc_sincos_config);

float ENC_SINCOS_read_deg(void);

uint32_t ENC_SINCOS_get_signal_below_min_error_cnt(void);
uint32_t ENC_SINCOS_get_signal_above_max_error_cnt(void);
float ENC_SINCOS_get_signal_below_min_error_rate(void);
float ENC_SINCOS_get_signal_above_max_error_rate(void);

#endif /* ENCODER_ENC_SINCOS_H_ */
