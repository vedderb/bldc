
#ifndef ENCODER_ABI_H_
#define ENCODER_ABI_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void ABI_deinit(void);
encoder_ret_t ABI_init(ABI_config_t *abi_config);

float ABI_read_deg(void);

#endif /* ENCODER_ABI_H_ */
