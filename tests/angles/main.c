#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "utils_math.h"

static float interpolate_angles_rad(float a1, float a2, float weight_a1) {
	float weight_a2 = 1.0 - weight_a1;

	float sin_a1 = sinf(a1);
	float cos_a1 = cosf(a1);
	float sin_a2 = sinf(a2);
	float cos_a2 = cosf(a2);

	float sin_mapped = sin_a1 * weight_a1 + sin_a2 * weight_a2;
	float cos_mapped = cos_a1 * weight_a1 + cos_a2 * weight_a2;
	return atan2f(sin_mapped, cos_mapped);
}

// Range 0.0 - 1.0
float rand01 (void) {
	return ((float)rand() / (float)RAND_MAX);
}

// Range -1.0 to 1.0
float rand11 (void) {
	return 2.0 * rand01() - 1.0;
}

int main(void) {	
	srand(time(NULL));
	
	int testnum = 100000;
	
	bool ok = true;
	for (int i = 0;i < testnum;i++) {
		float a1 = rand11() * 370.0;
		float a1_tmp = a1;
		utils_norm_angle(&a1_tmp);
		if (a1_tmp > 180.0) {
			a1_tmp -= 360.0;
		}
		float a2 = a1_tmp + rand11() * 60.0;
		float weight = rand01();
		
	
		float a_correct = interpolate_angles_rad(DEG2RAD_f(a1), DEG2RAD_f(a2), weight);
		float a_util = utils_interpolate_angles_rad(DEG2RAD_f(a1), DEG2RAD_f(a2), weight);
		
		float error = a_correct - a_util;
		utils_norm_angle_rad(&error);
		
		a_correct = RAD2DEG_f(a_correct);
		a_util = RAD2DEG_f(a_util);
		error = RAD2DEG_f(error);
		
		// Allow for 1.5 degrees difference as interpolation between sin/cos is not the same as
		// interpolation between the angles.
		if (fabsf(error) > 1.5) {
			ok = false;
			printf("Error occurred after %d tests\r\n", i + 1);
			printf("a1: %.1f, a2: %.1f, weight_a1: %.1f => Correct: %.1f Util: %.1f Error: %.1f\r\n",
				a1, a2, weight, a_correct, a_util, error);
			break;
		}
	}
	
	if (ok) {
		printf("All %d tests passed!\r\n", testnum);
	}	
	
	return 0;
}
