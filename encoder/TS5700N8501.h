#ifndef ENCODER_TS5700N8501_H_
#define ENCODER_TS5700N8501_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void TS5700N8501_deinit(void);
encoder_ret_t TS5700N8501_init(TS5700N8501_config_t *ts5700n8501_config);

float TS5700N8501_read_deg(void);

uint8_t* TS5700N8501_get_raw_status(void);
int16_t TS5700N8501_get_abm(void);
void TS5700N8501_reset_errors(void);
void TS5700N8501_reset_multiturn(void);

#endif /* ENCODER_TS5700N8501_H_ */
