
#ifndef ENCODER_AD2S1205_H_
#define ENCODER_AD2S1205_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void AD2S1205_deinit(void);
encoders_ret_t AD2S1205_init(AD2S1205_config_t *AD2S1205_config);

float AD2S1205_read_deg(void);
void AD2S1205_routine(void);

bool AD2S1205_index_found(void);
float AD2S1205_resolver_loss_of_tracking_error_rate(void);
float AD2S1205_resolver_degradation_of_signal_error_rate(void);
float AD2S1205_resolver_loss_of_signal_error_rate(void);
uint32_t AD2S1205_resolver_loss_of_tracking_error_cnt(void);
uint32_t AD2S1205_resolver_degradation_of_signal_error_cnt(void);
uint32_t AD2S1205_resolver_loss_of_signal_error_cnt(void);

#endif /* ENCODER_AD2S1205_H_ */
