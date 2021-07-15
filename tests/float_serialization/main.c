#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#include "buffer.h"

void test_read(unsigned char *data) {
	int32_t ind = 0;
	union {
		float f;
		uint32_t i;
	} flt;
	
	float res = buffer_get_float32_auto(data, &ind);
	printf("Read buffer: %g\r\n", res);
	
	ind = 0;
	flt.i = buffer_get_uint32(data, &ind);
	printf("Read typecast: %g\r\n", flt.f);
}

int main(void) {
	unsigned char data[4];
	int32_t ind = 0;
	
	float number = 0.5e-38;
	
	printf("Test with subnormal number: %g\r\n", number);
	
	union {
		float f;
		uint32_t i;
	} flt;
	
	printf("=== Add using buffer ===\r\n");
	buffer_append_float32_auto(data, number, &ind);
	ind = 0;
	
	test_read(data);
	
	printf("=== Add using typecast ===\r\n");
	flt.f = number;
	ind = 0;
	buffer_append_uint32(data, flt.i, &ind);
	test_read(data);
	
	return 0;
}

