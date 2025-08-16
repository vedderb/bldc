

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include "../../include/buffer.h"

#define TEST_BUFFER_SIZE 256

int test_int16_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  int16_t test_values[] = {0, 1, -1, 32767, -32768, 255, -255, 1000, -1000};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    index = 0;
    buffer_append_int16(buffer, test_values[i], &index);
    index = 0;
    int16_t result = buffer_get_int16(buffer, &index);
    if (result != test_values[i] || index != 2) {
      return 0;
    }
  }
  return 1;
}

int test_uint16_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  uint16_t test_values[] = {0, 1, 255, 256, 32767, 32768, 65535};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    index = 0;
    buffer_append_uint16(buffer, test_values[i], &index);
    index = 0;
    uint16_t result = buffer_get_uint16(buffer, &index);
    if (result != test_values[i] || index != 2) {
      return 0;
    }
  }
  return 1;
}

int test_int32_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  int32_t test_values[] = {0, 1, -1, 2147483647, -2147483648, 1000000, -1000000};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    index = 0;
    buffer_append_int32(buffer, test_values[i], &index);
    index = 0;
    int32_t result = buffer_get_int32(buffer, &index);
    if (result != test_values[i] || index != 4) {
      return 0;
    }
  }
  return 1;
}

int test_uint32_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  uint32_t test_values[] = {0, 1, 255, 256, 65535, 65536, 2147483647U, 2147483648U, 4294967295U};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    index = 0;
    buffer_append_uint32(buffer, test_values[i], &index);
    index = 0;
    uint32_t result = buffer_get_uint32(buffer, &index);
    if (result != test_values[i] || index != 4) {
      return 0;
    }
  }
  return 1;
}

int test_int64_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  int64_t test_values[] = {0, 1, -1, 9223372036854775807LL, (-9223372036854775807LL - 1), 1000000000000LL, -1000000000000LL};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    index = 0;
    buffer_append_int64(buffer, test_values[i], &index);
    index = 0;
    int64_t result = buffer_get_int64(buffer, &index);
    if (result != test_values[i] || index != 8) {
      return 0;
    }
  }
  return 1;
}

int test_uint64_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  uint64_t test_values[] = {0, 1, 255, 256, 65535, 65536, 9223372036854775807ULL, 9223372036854775808ULL, 18446744073709551615ULL};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    index = 0;
    buffer_append_uint64(buffer, test_values[i], &index);
    index = 0;
    uint64_t result = buffer_get_uint64(buffer, &index);
    if (result != test_values[i] || index != 8) {
      return 0;
    }
  }
  return 1;
}

int test_float16_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  struct {
    float value;
    float scale;
  } test_cases[] = {
    {0.0f, 1.0f}, {1.0f, 1.0f}, {-1.0f, 1.0f},
    {3.14159f, 1.0f}, {-3.14159f, 1.0f},
    {1.0f, 10.0f}, {-1.0f, 10.0f}, {3.14159f, 10.0f},
    {1.0f, 100.0f}, {-1.0f, 100.0f}, {3.14159f, 100.0f},
    {1.0f, 1000.0f}, {-1.0f, 1000.0f}, {3.14159f, 1000.0f},
    {10.5f, 100.0f}, {-10.5f, 100.0f},
    {32.0f, 1000.0f}, {-32.0f, 1000.0f}
  };
  int num_tests = sizeof(test_cases) / sizeof(test_cases[0]);
  
  for (int i = 0; i < num_tests; i++) {
    float scaled_value = test_cases[i].value * test_cases[i].scale;
    if (scaled_value > 32767.0f || scaled_value < -32768.0f) {
      continue;
    }
    
    index = 0;
    buffer_append_float16(buffer, test_cases[i].value, test_cases[i].scale, &index);
    index = 0;
    float result = buffer_get_float16(buffer, test_cases[i].scale, &index);
    
    float tolerance = fabsf(test_cases[i].value) * 0.01f + 1.0f / test_cases[i].scale;
    if (fabsf(result - test_cases[i].value) > tolerance || index != 2) {
      return 0;
    }
  }
  return 1;
}

int test_float32_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  float test_values[] = {0.0f, 1.0f, -1.0f, 3.14159f, -3.14159f, 1000000.5f, -1000000.5f};
  float scales[] = {1.0f, 10.0f, 100.0f, 1000.0f};
  int num_values = sizeof(test_values) / sizeof(test_values[0]);
  int num_scales = sizeof(scales) / sizeof(scales[0]);
  
  for (int i = 0; i < num_values; i++) {
    for (int j = 0; j < num_scales; j++) {
      index = 0;
      buffer_append_float32(buffer, test_values[i], scales[j], &index);
      index = 0;
      float result = buffer_get_float32(buffer, scales[j], &index);
      float tolerance = fabsf(test_values[i]) * 0.0001f + 1.0f / scales[j];
      if (fabsf(result - test_values[i]) > tolerance || index != 4) {
        return 0;
      }
    }
  }
  return 1;
}

// test designed to hit all cases in if (e != 0 || sig_i != 0)
// The read back floats are not that interesting.
int test_uint32_to_float32_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  uint32_t test_values[] = {0, 1 << 30, 0xFFFFFFFF, 0x7FFFFF, 0xFFFFFF, 2 << 24, 0x2 };
  int num_values = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_values; i++) {
    index = 0;
    buffer_append_uint32(buffer, test_values[i], &index);
    index = 0;
    buffer_get_float32_auto(buffer, &index);
  }
  return 1;
}

int test_double64_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  double test_values[] = {0.0, 1.0, -1.0, 3.14159265359, -3.14159265359, 1000000000.5, -1000000000.5};
  double scales[] = {1.0, 10.0, 100.0, 1000.0};
  int num_values = sizeof(test_values) / sizeof(test_values[0]);
  int num_scales = sizeof(scales) / sizeof(scales[0]);
  
  for (int i = 0; i < num_values; i++) {
    for (int j = 0; j < num_scales; j++) {
      index = 0;
      buffer_append_double64(buffer, test_values[i], scales[j], &index);
      index = 0;
      double result = buffer_get_double64(buffer, scales[j], &index);
      double tolerance = fabs(test_values[i]) * 0.000001 + 1.0 / scales[j];
      if (fabs(result - test_values[i]) > tolerance || index != 8) {
        return 0;
      }
    }
  }
  return 1;
}

int test_float32_auto_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  float test_values[] = {0.0f, 1.0f, -1.0f, 3.14159f, -3.14159f, 1.5e-38f, -1.5e-38f, 1e10f, -1e10f, 0.123456789f, -0.0f, 8388608.0f, -8388608.0f};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    index = 0;
    buffer_append_float32_auto(buffer, test_values[i], &index);
    index = 0;
    float result = buffer_get_float32_auto(buffer, &index);
    float tolerance = fabsf(test_values[i]) * 1e-6f + 1e-38f;
    if (fabsf(result - test_values[i]) > tolerance || index != 4) {
      return 0;
    }
  }
  return 1;
}

int test_float64_auto_round_trip() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  double test_values[] = {0.0, 1.0, -1.0, 3.14159265359, -3.14159265359, 1e10, -1e10, 0.123456789012345};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    index = 0;
    buffer_append_float64_auto(buffer, test_values[i], &index);
    index = 0;
    double result = buffer_get_float64_auto(buffer, &index);
    double tolerance = fabs(test_values[i]) * 1e-5 + 1e-38;
    if (fabs(result - test_values[i]) > tolerance || index != 8) {
      return 0;
    }
  }
  return 1;
}

int test_endianness() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  buffer_append_uint16(buffer, 0x1234, &index);
  if (buffer[0] != 0x12 || buffer[1] != 0x34) {
    return 0;
  }
  
  index = 0;
  buffer_append_uint32(buffer, 0x12345678, &index);
  if (buffer[0] != 0x12 || buffer[1] != 0x34 || buffer[2] != 0x56 || buffer[3] != 0x78) {
    return 0;
  }
  
  return 1;
}

int test_sequential_operations() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  buffer_append_int16(buffer, 0x1234, &index);
  buffer_append_uint32(buffer, 0x56789ABC, &index);
  buffer_append_int64(buffer, 0x123456789ABCDEFLL, &index);
  
  if (index != 14) {
    return 0;
  }
  
  index = 0;
  int16_t val1 = buffer_get_int16(buffer, &index);
  uint32_t val2 = buffer_get_uint32(buffer, &index);
  int64_t val3 = buffer_get_int64(buffer, &index);
  
  if (val1 != 0x1234 || val2 != 0x56789ABC || val3 != 0x123456789ABCDEFLL || index != 14) {
    return 0;
  }
  
  return 1;
}

int test_subnormal_float_handling() {
  uint8_t buffer[TEST_BUFFER_SIZE];
  int32_t index = 0;
  
  float subnormal = 1.0e-39f;
  buffer_append_float32_auto(buffer, subnormal, &index);
  index = 0;
  float result = buffer_get_float32_auto(buffer, &index);
  
  if (result != 0.0f) {
    return 0;
  }
  
  return 1;
}

int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  total_tests++; if (test_int16_round_trip()) tests_passed++;
  total_tests++; if (test_uint16_round_trip()) tests_passed++;
  total_tests++; if (test_int32_round_trip()) tests_passed++;
  total_tests++; if (test_uint32_round_trip()) tests_passed++;
  total_tests++; if (test_int64_round_trip()) tests_passed++;
  total_tests++; if (test_uint64_round_trip()) tests_passed++;
  total_tests++; if (test_float16_round_trip()) tests_passed++;
  total_tests++; if (test_float32_round_trip()) tests_passed++;
  total_tests++; if (test_double64_round_trip()) tests_passed++;
  total_tests++; if (test_float32_auto_round_trip()) tests_passed++;
  total_tests++; if (test_float64_auto_round_trip()) tests_passed++;
  total_tests++; if (test_endianness()) tests_passed++;
  total_tests++; if (test_sequential_operations()) tests_passed++;
  total_tests++; if (test_subnormal_float_handling()) tests_passed++;
  total_tests++; if (test_uint32_to_float32_round_trip()) tests_passed++;
  
  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
} 
