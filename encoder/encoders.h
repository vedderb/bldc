

#ifndef ENCODER_ENCODERS_H_
#define ENCODER_ENCODERS_H_

#include "encoder_datatype.h"
#include "encoder/AS504x.h"
#include "encoder/MT6816.h"
#include "encoder/AD2S1205.h"
#include "encoder/encoder_hwconf.h"
#include "hal.h"
// GENERIC GLOBAL
void encoders_deinit(void);
encoders_ret_t encoders_init(encoders_config_t *encoder_config);
float encoders_read_deg(void);
encoders_type_t encoders_is_configured(void);

// SPECIFIC GLOBAL
bool encoders_index_found(void);
float encoders_spi_get_error_rate(void);
uint32_t encoders_spi_get_error_cnt(void);
uint32_t encoders_get_no_magnet_error_cnt(void);

float encoders_resolver_loss_of_tracking_error_rate(void);
float encoders_resolver_degradation_of_signal_error_rate(void);
float encoders_resolver_loss_of_signal_error_rate(void);
uint32_t encoders_resolver_loss_of_tracking_error_cnt(void);
uint32_t encoders_resolver_degradation_of_signal_error_cnt(void);
uint32_t encoders_resolver_loss_of_signal_error_cnt(void);
float encoders_get_no_magnet_error_rate(void);
uint32_t encoders_spi_get_val(void);


void encoders_tim_isr(void);
void encoders_reset(void);


#endif /* ENCODER_ENCODERS_H_ */
