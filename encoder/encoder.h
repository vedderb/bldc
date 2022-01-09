#ifndef ENCODER_ENCODER_H_
#define ENCODER_ENCODER_H_

#include "encoder_datatype.h"
#include "hal.h"

// GENERIC GLOBAL
void encoder_deinit(void);
encoder_ret_t encoder_init(encoder_type_t encoder_config);

float encoder_read_deg(void);
float encoder_read_deg_multiturn(void);
encoder_type_t encoder_is_configured(void);
bool encoder_index_found(void);
void encoder_reset_multiturn(void);

// SPECIFIC GLOBAL
//AD2S1205
float encoder_resolver_loss_of_tracking_error_rate(void);
float encoder_resolver_degradation_of_signal_error_rate(void);
float encoder_resolver_loss_of_signal_error_rate(void);
uint32_t encoder_resolver_loss_of_tracking_error_cnt(void);
uint32_t encoder_resolver_degradation_of_signal_error_cnt(void);
uint32_t encoder_resolver_loss_of_signal_error_cnt(void);

//ABI
void encoder_set_counts(uint32_t counts);

//MT6816
float encoder_get_no_magnet_error_rate(void);
uint32_t encoder_get_no_magnet_error_cnt(void);

//AS504x
AS504x_diag encoder_get_diag(void);

//SINCOS
uint32_t encoder_get_signal_below_min_error_cnt(void);
uint32_t encoder_get_signal_above_max_error_cnt(void);
float encoder_get_signal_below_min_error_rate(void);
float encoder_get_signal_above_max_error_rate(void);
void encoder_sincos_conf_set(ENCSINCOS_config_t *sincos_config);

//TS5700N8501
uint8_t* encoder_get_raw_status(void);
int16_t encoder_get_abm(void);
void encoder_reset_errors(void);

//SPI ENCODERS
uint32_t encoder_spi_get_val(void);
float encoder_spi_get_error_rate(void);
uint32_t encoder_spi_get_error_cnt(void);

void encoder_reset(void);
void encoder_tim_isr(void);

#endif /* ENCODER_ENCODER_H_ */
