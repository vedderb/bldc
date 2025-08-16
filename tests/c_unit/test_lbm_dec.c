

#define _GNU_SOURCE // MAP_ANON
#define _POSIX_C_SOURCE 200809L // nanosleep?
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <unistd.h>
#include <math.h>

#include "lispbm.h"
#include "lbm_image.h"
#include "lbm_channel.h"


#include "init/start_lispbm.c"

static int test_init(void) {
  return start_lispbm_for_tests();
}

// Test lbm_dec_float with valid values
int test_lbm_dec_float_valid() {
  if (!test_init()) return 0;
  
  float test_values[] = {0.0f, 1.0f, -1.0f, 3.14159f, -2.71828f, 
                         1e6f, -1e6f, 1e-6f, -1e-6f, INFINITY, -INFINITY};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    lbm_value enc_val = lbm_enc_float(test_values[i]);
    if (lbm_type_of(enc_val) != LBM_TYPE_FLOAT) {
      printf("Failed to encode float %f\n", test_values[i]);
      continue;
    }
    
    float dec_val = lbm_dec_float(enc_val);
    
    // Handle special values
    if (isinf(test_values[i]) && isinf(dec_val) && 
        signbit(test_values[i]) == signbit(dec_val)) {
      continue; // Pass
    }
    if (isnan(test_values[i]) && isnan(dec_val)) {
      continue; // Pass  
    }
    
    if (fabsf(dec_val - test_values[i]) > 1e-6f) {
      printf("lbm_dec_float failed: expected %f, got %f\n", test_values[i], dec_val);
      return 0;
    }
  }
  return 1;
}

// Test lbm_dec_double with valid values
int test_lbm_dec_double_valid() {
  if (!test_init()) return 0;
  
  double test_values[] = {0.0, 1.0, -1.0, 3.141592653589793, -2.718281828459045,
                          1e15, -1e15, 1e-15, -1e-15, INFINITY, -INFINITY};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    lbm_value enc_val = lbm_enc_double(test_values[i]);
    if (lbm_type_of(enc_val) != LBM_TYPE_DOUBLE) {
      printf("Failed to encode double %f\n", test_values[i]);
      continue;
    }
    
    double dec_val = lbm_dec_double(enc_val);
    
    // Handle special values
    if (isinf(test_values[i]) && isinf(dec_val) && 
        signbit(test_values[i]) == signbit(dec_val)) {
      continue; // Pass
    }
    if (isnan(test_values[i]) && isnan(dec_val)) {
      continue; // Pass
    }
    
    if (fabs(dec_val - test_values[i]) > 1e-15) {
      printf("lbm_dec_double failed: expected %f, got %f\n", test_values[i], dec_val);
      return 0;
    }
  }
  return 1;
}

// Test lbm_dec_u64 with valid values
int test_lbm_dec_u64_valid() {
  if (!test_init()) return 0;
  
  uint64_t test_values[] = {0, 1, 255, 256, 65535, 65536, 
                            0xFFFFFFFF, 0x100000000ULL, 0xFFFFFFFFFFFFFFFFULL};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    lbm_value enc_val = lbm_enc_u64(test_values[i]);
    if (lbm_type_of(enc_val) != LBM_TYPE_U64) {
      printf("Failed to encode u64 %" PRIu64 "\n", test_values[i]);
      continue;
    }
    
    uint64_t dec_val = lbm_dec_u64(enc_val);
    if (dec_val != test_values[i]) {
      printf("lbm_dec_u64 failed: expected %" PRIu64 ", got %" PRIu64 "\n", 
             test_values[i], dec_val);
      return 0;
    }
  }
  return 1;
}

// Test lbm_dec_i64 with valid values
int test_lbm_dec_i64_valid() {
  if (!test_init()) return 0;
  
  int64_t test_values[] = {0, 1, -1, 127, -128, 32767, -32768,
                           2147483647, -2147483648LL, 
                           9223372036854775807LL, -9223372036854775808ULL};
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    lbm_value enc_val = lbm_enc_i64(test_values[i]);
    if (lbm_type_of(enc_val) != LBM_TYPE_I64) {
      printf("Failed to encode i64 %" PRId64 "\n", test_values[i]);
      continue;
    }
    
    int64_t dec_val = lbm_dec_i64(enc_val);
    if (dec_val != test_values[i]) {
      printf("lbm_dec_i64 failed: expected %" PRId64 ", got %" PRId64 "\n", 
             test_values[i], dec_val);
      return 0;
    }
  }
  return 1;
}

// Test lbm_dec_str with valid values
int test_lbm_dec_str_valid() {
  if (!test_init()) return 0;
  
  char *test_strings[] = {"", "hello", "world", "test string", "a", 
                          "0123456789", "special chars: !@#$%^&*()"};
  int num_tests = sizeof(test_strings) / sizeof(test_strings[0]);
  
  for (int i = 0; i < num_tests; i++) {
    lbm_value str_val;
    lbm_heap_allocate_array(&str_val, 
                            strlen(test_strings[i]) + 1);
    if (lbm_type_of(str_val) != LBM_TYPE_ARRAY) {
      printf("Failed to allocate string array\n");
      continue;
    }
    
    lbm_array_header_t *arr = lbm_dec_array_rw(str_val);
    if (!arr) {
      printf("Failed to get array header\n");
      continue;  
    }
    
    strcpy((char*)arr->data, test_strings[i]);
    
    char *dec_str = lbm_dec_str(str_val);
    if (!dec_str) {
      printf("lbm_dec_str returned NULL for valid string\n");
      return 0;
    }
    
    if (strcmp(dec_str, test_strings[i]) != 0) {
      printf("lbm_dec_str failed: expected '%s', got '%s'\n", 
             test_strings[i], dec_str);
      return 0;
    }
  }
  return 1;
}

// Test conversion functions with mixed types
int test_lbm_dec_as_conversions() {
  if (!test_init()) return 0;
  
  // Test various input types being converted
  lbm_value test_values[] = {
    lbm_enc_char(65),    // 'A'
    lbm_enc_i(42),       
    lbm_enc_u(100),
    lbm_enc_i32(-1000),
    lbm_enc_u32(2000),
    lbm_enc_float(3.14f),
    lbm_enc_i64(-999999999LL),
    lbm_enc_u64(888888888ULL),
    lbm_enc_double(2.71828)
  };
  
  int num_tests = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    // These functions should not crash - they may return converted values
    uint8_t as_char = lbm_dec_as_char(test_values[i]);
    uint32_t as_u32 = lbm_dec_as_u32(test_values[i]);
    int32_t as_i32 = lbm_dec_as_i32(test_values[i]);
    int64_t as_i64 = lbm_dec_as_i64(test_values[i]);
    uint64_t as_u64 = lbm_dec_as_u64(test_values[i]);
    float as_float = lbm_dec_as_float(test_values[i]);
    double as_double = lbm_dec_as_double(test_values[i]);
    
    // Just test that we got some values without crashing
    (void)as_char; (void)as_u32; (void)as_i32; (void)as_i64; 
    (void)as_u64; (void)as_float; (void)as_double;
  }
  
  return 1;
}

// Test edge cases and boundary values
int test_lbm_dec_edge_cases() {
  if (!test_init()) return 0;
  
  // Test with values that have specific bit patterns
  lbm_value edge_values[] = {
    0x00000001,        // Minimal value
    0x00000002,        // Next minimal
    0xFFFFFFFE,        // Near max
    0x80000001,        // Sign bit + 1
    0x40000000,        // Middle bit
    0x20000000,        // Another middle bit
    0x10000000,        // Type bits area
    0x08000000,        // More type bits
  };
  
  int num_tests = sizeof(edge_values) / sizeof(edge_values[0]);
  
  for (int i = 0; i < num_tests; i++) {
    lbm_value val = edge_values[i];
    
    // Test all decoder functions - focus on not crashing
    uint8_t as_char = lbm_dec_as_char(val);
    uint32_t as_u32 = lbm_dec_as_u32(val);
    int32_t as_i32 = lbm_dec_as_i32(val);
    int64_t as_i64 = lbm_dec_as_i64(val);
    uint64_t as_u64 = lbm_dec_as_u64(val);
    float as_float = lbm_dec_as_float(val);
    double as_double = lbm_dec_as_double(val);

    // This is too abusive. we cannot protect against malicious
    // creation of values that are valid for pointer types but
    // does not point towards the correct data. These situation should
    // never arise from running any lisp code (even incorrect such).
    
    // Test pointer decoders with edge values
    //char *str_result = lbm_dec_str(val);
    //lbm_array_header_t *arr_r = lbm_dec_array_r(val);
    //lbm_array_header_t *arr_rw = lbm_dec_array_rw(val);
    //lbm_char_channel_t *chan = lbm_dec_channel(val);
    //lbm_uint custom = lbm_dec_custom(val);
    
    // If we reach here, no crash occurred
    (void)as_char; (void)as_u32; (void)as_i32; (void)as_i64; 
    (void)as_u64; (void)as_float; (void)as_double;
    //(void)str_result; (void)arr_r; (void)arr_rw; (void)chan; (void)custom;
  }
  
  return 1;
}

int main(int argc, char **argv) {
  (void)argc;
  (void)argv;
  
  printf("Testing lbm_dec_* functions\n");
  
  if (!test_lbm_dec_float_valid()) {
    printf("FAILED: test_lbm_dec_float_valid\n");
    return 1;
  }
  printf("PASSED: test_lbm_dec_float_valid\n");
  
  if (!test_lbm_dec_double_valid()) {
    printf("FAILED: test_lbm_dec_double_valid\n");
    return 1;
  }
  printf("PASSED: test_lbm_dec_double_valid\n");
  
  if (!test_lbm_dec_u64_valid()) {
    printf("FAILED: test_lbm_dec_u64_valid\n");
    return 1;
  }
  printf("PASSED: test_lbm_dec_u64_valid\n");
  
  if (!test_lbm_dec_i64_valid()) {
    printf("FAILED: test_lbm_dec_i64_valid\n");
    return 1;
  }
  printf("PASSED: test_lbm_dec_i64_valid\n");
  
  if (!test_lbm_dec_str_valid()) {
    printf("FAILED: test_lbm_dec_str_valid\n");
    return 1;
  }
  printf("PASSED: test_lbm_dec_str_valid\n");
  
  if (!test_lbm_dec_as_conversions()) {
    printf("FAILED: test_lbm_dec_as_conversions\n");
    return 1;
  }
  printf("PASSED: test_lbm_dec_as_conversions\n");
    
  if (!test_lbm_dec_edge_cases()) {
    printf("FAILED: test_lbm_dec_edge_cases\n");
    return 1;
  }
  printf("PASSED: test_lbm_dec_edge_cases\n");
  
  printf("SUCCESS\n");
  return 0;
}
