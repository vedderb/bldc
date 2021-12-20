
#ifndef ENCODER_AS504X_H_
#define ENCODER_AS504X_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void AS504x_deinit(void);
encoders_ret_t AS504x_init(AS504x_config_t *AS504x_config);

float AS504x_read_deg(void);
void AS504x_routine(void);

AS504x_diag AS504x_get_diag(void);
float AS504x_spi_get_error_rate(void);
uint32_t AS504x_spi_get_val(void);
uint32_t AS504x_spi_get_error_cnt(void);
#endif /* ENCODER_AS504X_H_ */
