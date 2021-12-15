/*
 * Functions.h
 *
 *  Created on: 23.07.2018
 *      Author: ed
 */

#ifndef TMC_FUNCTIONS_H_
#define TMC_FUNCTIONS_H_

#include "API_Header.h"

int32_t tmc_limitInt(int32_t value, int32_t min, int32_t max);
int64_t tmc_limitS64(int64_t value, int64_t min, int64_t max);
int32_t tmc_sqrti(int32_t x);
int32_t tmc_filterPT1(int64_t *akku, int32_t newValue, int32_t lastValue, uint8_t actualFilter, uint8_t maxFilter);

#endif /* TMC_FUNCTIONS_H_ */
