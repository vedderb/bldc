/*
 * ENC_SINCOS.h
 *
 *  Created on: Dec 15, 2021
 *      Author: jtuxv
 */

#ifndef ENCODER_ENC_SINCOS_H_
#define ENCODER_ENC_SINCOS_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void ENC_SINCOS_deinit(void);
encoders_ret_t ENC_SINCOS_init(ENC_SINCOS_config_t *enc_sincos_config);

float ENC_SINCOS_read_deg(void);

#endif /* ENCODER_ENC_SINCOS_H_ */
