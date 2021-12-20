
#ifndef ENCODER_MT6816_H_
#define ENCODER_MT6816_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void MT6816_deinit(void);
encoders_ret_t MT6816_init(MT6816_config_t *mt6816_config);

float MT6816_read_deg(void);
void MT6816_routine(void);

uint32_t MT6816_spi_get_val(void);
uint32_t MT6816_spi_get_error_cnt(void);
uint32_t MT6816_get_no_magnet_error_cnt(void);
uint32_t MT6816_get_no_magnet_error_rate(void);

#endif /* ENCODER_MT6816_H_ */
