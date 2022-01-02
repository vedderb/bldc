#ifndef ENCODER_ENCODERS_H_
#define ENCODER_ENCODERS_H_

#include "encoder_datatype.h"
#include "hal.h"

// GENERIC GLOBAL
void encoders_deinit(void);
encoders_ret_t encoders_init(encoders_type_t encoder_config);

float encoders_read_deg(void);
float encoders_read_deg_multiturn(void);
encoders_type_t encoders_is_configured(void);
bool encoders_index_found(void);
void encoders_reset_multiturn(void);

// SPECIFIC GLOBAL
//AD2S1205
float encoders_resolver_loss_of_tracking_error_rate(void);
float encoders_resolver_degradation_of_signal_error_rate(void);
float encoders_resolver_loss_of_signal_error_rate(void);
uint32_t encoders_resolver_loss_of_tracking_error_cnt(void);
uint32_t encoders_resolver_degradation_of_signal_error_cnt(void);
uint32_t encoders_resolver_loss_of_signal_error_cnt(void);

//ABI
void encoders_set_counts(uint32_t counts);

//MT6816
float encoders_get_no_magnet_error_rate(void);
uint32_t encoders_get_no_magnet_error_cnt(void);

//AS504x
AS504x_diag encoders_get_diag(void);

//SINCOS
uint32_t encoders_get_signal_below_min_error_cnt(void);
uint32_t encoders_get_signal_above_max_error_cnt(void);
float encoders_get_signal_below_min_error_rate(void);
float encoders_get_signal_above_max_error_rate(void);
void encoders_sincos_conf_set(ENCSINCOS_config_t *sincos_config);

//TS5700N8501
uint8_t* encoders_get_raw_status(void);
int16_t encoders_get_abm(void);
void encoders_reset_errors(void);

//SPI ENCODERS
uint32_t encoders_spi_get_val(void);
float encoders_spi_get_error_rate(void);
uint32_t encoders_spi_get_error_cnt(void);

void encoders_reset(void);
void encoders_tim_isr(void);

#endif /* ENCODER_ENCODERS_H_ */
