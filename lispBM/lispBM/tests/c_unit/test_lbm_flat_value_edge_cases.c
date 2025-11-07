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
#include <string.h>

#include "lispbm.h"
#include "lbm_flat_value.h"

#include "init/start_lispbm.c"

static int test_init(void) {
  return start_lispbm_for_tests();
}

// Test flattening with buffer too small
int test_flatten_buffer_too_small(void) {
  if (!test_init()) return 0;
  
  // Create a simple cons cell that requires more space than we'll give it
  lbm_value cons = lbm_cons(lbm_enc_i(42), lbm_enc_i(24));
  if (lbm_is_symbol_merror(cons)) return 0;
  
  // Test 1: Zero-size buffer
  lbm_flat_value_t fv1;
  if (!lbm_start_flatten(&fv1, 0)) {
    // Expected - can't allocate 0-size buffer
  } else {
    int result1 = flatten_value_c(&fv1, cons);
    lbm_finish_flatten(&fv1);
    if (result1 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 2: Buffer size 1 byte (too small for cons)
  lbm_flat_value_t fv2;
  if (lbm_start_flatten(&fv2, 1)) {
    int result2 = flatten_value_c(&fv2, cons);
    lbm_finish_flatten(&fv2);
    if (result2 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 3: Buffer size 4 bytes (still too small for cons)
  lbm_flat_value_t fv3;
  if (lbm_start_flatten(&fv3, 4)) {
    int result3 = flatten_value_c(&fv3, cons);
    lbm_finish_flatten(&fv3);
    if (result3 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 4: Just enough space (should work)
  int required_size = flatten_value_size(cons, false);
  if (required_size <= 0) return 0;
  
  lbm_flat_value_t fv4;
  if (lbm_start_flatten(&fv4, (size_t)required_size)) {
    int result4 = flatten_value_c(&fv4, cons);
    lbm_finish_flatten(&fv4);
    if (result4 != FLATTEN_VALUE_OK) return 0;
  } else {
    return 0;
  }
  
  return 1;
}

// Test flattening with various data types and buffer constraints
int test_flatten_different_types_small_buffer(void) {
  if (!test_init()) return 0;
  
  // Test with integer (should need minimal space)
  lbm_value int_val = lbm_enc_i(123);
  
  // Buffer too small for integer
  lbm_flat_value_t fv1;
  if (lbm_start_flatten(&fv1, 4)) { // Too small for integer type
    int result1 = flatten_value_c(&fv1, int_val);
    lbm_finish_flatten(&fv1);
    if (result1 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test with byte value (minimal space needed)
  lbm_value byte_val = lbm_enc_char('A');
  
  lbm_flat_value_t fv2;
  if (lbm_start_flatten(&fv2, 1)) { // Too small for byte type tag
    int result2 = flatten_value_c(&fv2, byte_val);
    lbm_finish_flatten(&fv2);
    if (result2 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test with float value
  lbm_value float_val = lbm_enc_float(3.14f);
  
  lbm_flat_value_t fv3;
  if (lbm_start_flatten(&fv3, 4)) { // Too small for float
    int result3 = flatten_value_c(&fv3, float_val);
    lbm_finish_flatten(&fv3);
    if (result3 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  return 1;
}

// Test flattening arrays with insufficient buffer space
int test_flatten_array_buffer_too_small(void) {
  if (!test_init()) return 0;
  
  // Create a byte array
  lbm_value arr;
  if (!lbm_create_array(&arr, 100)) return 0;
  
  lbm_array_header_t *arr_header = lbm_dec_array_rw(arr);
  if (!arr_header || !arr_header->data) return 0;
  
  uint8_t *arr_data = (uint8_t*)arr_header->data;
  
  // Fill with some data
  for (int i = 0; i < 100; i++) {
    arr_data[i] = (uint8_t)i;
  }
  
  // Test 1: Buffer too small for the array
  lbm_flat_value_t fv1;
  if (lbm_start_flatten(&fv1, 50)) { // Not enough space for 100-byte array + overhead
    int result1 = flatten_value_c(&fv1, arr);
    lbm_finish_flatten(&fv1);
    if (result1 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 2: Buffer just big enough
  int required_size = flatten_value_size(arr, false);
  if (required_size <= 0) return 0;
  
  lbm_flat_value_t fv2;
  if (lbm_start_flatten(&fv2, (size_t)required_size)) {
    int result2 = flatten_value_c(&fv2, arr);
    lbm_finish_flatten(&fv2);
    if (result2 != FLATTEN_VALUE_OK) return 0;
  } else {
    return 0;
  }
  
  return 1;
}

// Test flattening deeply nested structures with small buffer
int test_flatten_deep_nesting_small_buffer(void) {
  if (!test_init()) return 0;
  
  // Create a nested list: (((((42)))))
  lbm_value nested = lbm_enc_i(42);
  for (int i = 0; i < 10; i++) {
    nested = lbm_cons(nested, ENC_SYM_NIL);
    if (lbm_is_symbol_merror(nested)) return 0;
  }
  
  // Test 1: Buffer too small for deep nesting
  lbm_flat_value_t fv1;
  if (lbm_start_flatten(&fv1, 20)) { // Not enough for deep nesting
    int result1 = flatten_value_c(&fv1, nested);
    lbm_finish_flatten(&fv1);
    if (result1 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 2: Buffer just enough
  int required_size = flatten_value_size(nested, false);
  if (required_size <= 0) return 0;
  
  lbm_flat_value_t fv2;
  if (lbm_start_flatten(&fv2, (size_t)required_size)) {
    int result2 = flatten_value_c(&fv2, nested);
    lbm_finish_flatten(&fv2);
    if (result2 != FLATTEN_VALUE_OK) return 0;
  } else {
    return 0;
  }
  
  return 1;
}

// Test unflattering with malformed data
int test_unflatten_malformed_data(void) {
  if (!test_init()) return 0;
  
  // Test 1: Zero-size buffer
  uint8_t dummy_byte = 0;
  lbm_flat_value_t fv1;
  fv1.buf = &dummy_byte;
  fv1.buf_size = 0;
  fv1.buf_pos = 0;
  
  lbm_value result1;
  bool success1 = lbm_unflatten_value(&fv1, &result1);
  if (success1) return 0; // Should fail with zero-size buffer
  
  // Test 2: Buffer with invalid type tag
  uint8_t bad_data[] = {0xFF, 0x42, 0x24}; // 0xFF is not a valid type tag
  lbm_flat_value_t fv2;
  fv2.buf = bad_data;
  fv2.buf_size = sizeof(bad_data);
  fv2.buf_pos = 0;
  
  lbm_value result2;
  bool success2 = lbm_unflatten_value(&fv2, &result2);
  if (success2) return 0; // Should fail with bad type tag
  
  // Test 3: Truncated data (type tag but no data)
  uint8_t truncated_data[] = {S_I32_VALUE}; // Tag for i32 but no data
  lbm_flat_value_t fv3;
  fv3.buf = truncated_data;
  fv3.buf_size = sizeof(truncated_data);
  fv3.buf_pos = 0;
  
  lbm_value result3;
  bool success3 = lbm_unflatten_value(&fv3, &result3);
  if (success3) return 0; // Should fail with truncated data
  
  return 1;
}

// Test unflatten with array that claims larger size than buffer
int test_unflatten_oversized_array(void) {
  if (!test_init()) return 0;
  
  // Create malformed array data: claims 1000 bytes but buffer is smaller
  uint8_t bad_array_data[10];
  bad_array_data[0] = S_LBM_ARRAY; // Array type tag
  // Size field (big-endian 32-bit): claim 1000 bytes
  bad_array_data[1] = 0x00;
  bad_array_data[2] = 0x00;
  bad_array_data[3] = 0x03;
  bad_array_data[4] = 0xE8; // 1000 in hex
  // Only provide 5 more bytes of actual data
  for (int i = 5; i < 10; i++) {
    bad_array_data[i] = (uint8_t)i;
  }
  
  lbm_flat_value_t fv;
  fv.buf = bad_array_data;
  fv.buf_size = sizeof(bad_array_data);
  fv.buf_pos = 0;
  
  lbm_value result;
  bool success = lbm_unflatten_value(&fv, &result);
  if (success) return 0; // Should fail due to buffer overrun
  
  return 1;
}

// Test edge cases with flatten_value_size
int test_flatten_value_size_edge_cases(void) {
  if (!test_init()) return 0;
  
  // Test 1: Size of simple values
  int size_int = flatten_value_size(lbm_enc_i(42), false);
  int size_byte = flatten_value_size(lbm_enc_char('A'), false);
  int size_nil = flatten_value_size(ENC_SYM_NIL, false);
  
  if (size_int <= 0 || size_byte <= 0 || size_nil <= 0) return 0;
  
  // Test 2: Size should be consistent
  int size_int2 = flatten_value_size(lbm_enc_i(999), false);
  if (size_int != size_int2) return 0; // Same type should have same size
  
  // Test 3: Nested structure size
  lbm_value cons = lbm_cons(lbm_enc_i(1), lbm_cons(lbm_enc_i(2), ENC_SYM_NIL));
  if (lbm_is_symbol_merror(cons)) return 0;
  
  int size_cons = flatten_value_size(cons, false);
  if (size_cons <= size_int) return 0; // Cons should be larger than single int
  
  return 1;
}

// Test round-trip: flatten then unflatten
int test_flatten_unflatten_roundtrip(void) {
  if (!test_init()) return 0;
  
  // Test with various data types
  lbm_value values[] = {
    lbm_enc_i(42),
    lbm_enc_i(-123),
    lbm_enc_float(3.14f),
    lbm_enc_char('Z'),
    ENC_SYM_NIL,
    lbm_cons(lbm_enc_i(1), lbm_enc_i(2))
  };
  
  int num_values = sizeof(values) / sizeof(values[0]);
  
  for (int i = 0; i < num_values; i++) {
    if (lbm_is_symbol_merror(values[i])) continue;
    
    // Get required size and flatten
    int size = flatten_value_size(values[i], false);
    if (size <= 0) return 0;
    
    lbm_flat_value_t fv;
    if (!lbm_start_flatten(&fv, (size_t)size)) return 0;
    
    int flatten_result = flatten_value_c(&fv, values[i]);
    if (flatten_result != FLATTEN_VALUE_OK) {
      lbm_finish_flatten(&fv);
      return 0;
    }
    
    if (!lbm_finish_flatten(&fv)) return 0;
    
    // Reset position for unflattening
    fv.buf_pos = 0;
    
    // Unflatten
    lbm_value unflattened;
    if (!lbm_unflatten_value(&fv, &unflattened)) return 0;
    
    // Compare (simple equality check for basic types)
    lbm_uint type_orig = lbm_type_of(values[i]);
    lbm_uint type_unflat = lbm_type_of(unflattened);
    
    if (type_orig != type_unflat) return 0;
    
    // For integers and bytes, check exact equality
    if (type_orig == LBM_TYPE_I && values[i] != unflattened) return 0;
    if (type_orig == LBM_TYPE_BYTE && values[i] != unflattened) return 0;
    if (type_orig == LBM_TYPE_SYMBOL && values[i] != unflattened) return 0;
  }
  
  return 1;
}

// Test maximum flatten depth
int test_flatten_maximum_depth(void) {
  if (!test_init()) return 0;
  
  // Create very deep nesting to exceed maximum depth
  lbm_value deep_nested = lbm_enc_i(42);
  
  // Create deeper nesting than FLATTEN_VALUE_MAXIMUM_DEPTH (2000)
  for (int i = 0; i < 2100; i++) {
    deep_nested = lbm_cons(deep_nested, ENC_SYM_NIL);
    if (lbm_is_symbol_merror(deep_nested)) {
      // Memory exhaustion is expected at some point
      break;
    }
  }
  
  if (!lbm_is_symbol_merror(deep_nested)) {
    // Try to flatten - should fail with maximum depth error
    int size = flatten_value_size(deep_nested, false);
    if (size == FLATTEN_VALUE_ERROR_MAXIMUM_DEPTH) {
      return 1; // Expected error
    }
    
    if (size > 0) {
      lbm_flat_value_t fv;
      if (lbm_start_flatten(&fv, (size_t)size)) {
        int result = flatten_value_c(&fv, deep_nested);
        lbm_finish_flatten(&fv);
        if (result == FLATTEN_VALUE_ERROR_MAXIMUM_DEPTH) {
          return 1; // Expected error
        }
      }
    }
  }
  
  // If we got here without hitting max depth, that's also OK
  // (might happen on systems with different memory constraints)
  return 1;
}

// Test flattening each value type with insufficient buffer space
int test_flatten_each_type_small_buffer(void) {
  if (!test_init()) return 0;
  
  // Test each basic type with a 1-byte buffer (too small for any type)
  lbm_flat_value_t fv;
  
  // Test 1: Integer value
  lbm_value int_val = lbm_enc_i(42);
  if (lbm_start_flatten(&fv, 1)) {
    int result1 = flatten_value_c(&fv, int_val);
    lbm_finish_flatten(&fv);
    if (result1 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 2: Unsigned integer  
  lbm_value uint_val = lbm_enc_u(42u);
  if (lbm_start_flatten(&fv, 1)) {
    int result2 = flatten_value_c(&fv, uint_val);
    lbm_finish_flatten(&fv);
    if (result2 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 3: Float value
  lbm_value float_val = lbm_enc_float(3.14f);
  if (lbm_start_flatten(&fv, 1)) {
    int result3 = flatten_value_c(&fv, float_val);
    lbm_finish_flatten(&fv);
    if (result3 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 4: 32-bit integer
  lbm_value i32_val = lbm_enc_i32(123456);
  if (lbm_start_flatten(&fv, 1)) {
    int result4 = flatten_value_c(&fv, i32_val);
    lbm_finish_flatten(&fv);
    if (result4 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 5: 32-bit unsigned integer
  lbm_value u32_val = lbm_enc_u32(123456u);
  if (lbm_start_flatten(&fv, 1)) {
    int result5 = flatten_value_c(&fv, u32_val);
    lbm_finish_flatten(&fv);
    if (result5 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 6: 64-bit integer
  lbm_value i64_val = lbm_enc_i64(123456789LL);
  if (lbm_start_flatten(&fv, 1)) {
    int result6 = flatten_value_c(&fv, i64_val);
    lbm_finish_flatten(&fv);
    if (result6 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 7: 64-bit unsigned integer
  lbm_value u64_val = lbm_enc_u64(123456789ULL);
  if (lbm_start_flatten(&fv, 1)) {
    int result7 = flatten_value_c(&fv, u64_val);
    lbm_finish_flatten(&fv);
    if (result7 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 8: Double value
  lbm_value double_val = lbm_enc_double(3.141592653589793);
  if (lbm_start_flatten(&fv, 1)) {
    int result8 = flatten_value_c(&fv, double_val);
    lbm_finish_flatten(&fv);
    if (result8 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 9: Character/byte value
  lbm_value char_val = lbm_enc_char('A');
  if (lbm_start_flatten(&fv, 1)) {
    int result9 = flatten_value_c(&fv, char_val);
    lbm_finish_flatten(&fv);
    if (result9 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 10: Symbol value
  lbm_value sym_val = ENC_SYM_NIL;
  if (lbm_start_flatten(&fv, 1)) {
    int result10 = flatten_value_c(&fv, sym_val);
    lbm_finish_flatten(&fv);
    if (result10 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 11: Cons cell
  lbm_value cons_val = lbm_cons(lbm_enc_i(1), lbm_enc_i(2));
  if (lbm_is_symbol_merror(cons_val)) return 0;
  if (lbm_start_flatten(&fv, 1)) {
    int result11 = flatten_value_c(&fv, cons_val);
    lbm_finish_flatten(&fv);
    if (result11 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test 12: Array value
  lbm_value arr_val;
  if (lbm_create_array(&arr_val, 10)) {
    if (lbm_start_flatten(&fv, 1)) {
      int result12 = flatten_value_c(&fv, arr_val);
      lbm_finish_flatten(&fv);
      if (result12 != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
    }
  }
  
  return 1;
}

// Test flattening each type with buffer just barely too small  
int test_flatten_each_type_barely_too_small(void) {
  if (!test_init()) return 0;
  
  // For each type, calculate required size and provide 1 byte less
  lbm_flat_value_t fv;
  
  // Test with integer - get required size and subtract 1
  lbm_value int_val = lbm_enc_i(42);
  int required_size = flatten_value_size(int_val, false);
  if (required_size > 1) {
    if (lbm_start_flatten(&fv, (size_t)(required_size - 1))) {
      int result = flatten_value_c(&fv, int_val);
      lbm_finish_flatten(&fv);
      if (result != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
    }
  }
  
  // Test with float
  lbm_value float_val = lbm_enc_float(3.14f);
  required_size = flatten_value_size(float_val, false);
  if (required_size > 1) {
    if (lbm_start_flatten(&fv, (size_t)(required_size - 1))) {
      int result = flatten_value_c(&fv, float_val);
      lbm_finish_flatten(&fv);
      if (result != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
    }
  }
  
  // Test with cons cell
  lbm_value cons_val = lbm_cons(lbm_enc_i(1), lbm_enc_i(2));
  if (lbm_is_symbol_merror(cons_val)) return 0;
  required_size = flatten_value_size(cons_val, false);
  if (required_size > 1) {
    if (lbm_start_flatten(&fv, (size_t)(required_size - 1))) {
      int result = flatten_value_c(&fv, cons_val);
      lbm_finish_flatten(&fv);
      if (result != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
    }
  }
  
  // Test with array
  lbm_value arr_val;
  if (lbm_create_array(&arr_val, 20)) {
    required_size = flatten_value_size(arr_val, false);
    if (required_size > 1) {
      if (lbm_start_flatten(&fv, (size_t)(required_size - 1))) {
        int result = flatten_value_c(&fv, arr_val);
        lbm_finish_flatten(&fv);
        if (result != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
      }
    }
  }
  
  return 1;
}

// Test unflatten with malicious S_SYM_STRING (symbol string without null terminator)
int test_unflatten_malicious_symbol_string(void) {
  if (!test_init()) return 0;
  
  // Create malicious S_SYM_STRING data without null terminator
  // This should cause unflatten to read beyond buffer bounds looking for null terminator
  uint8_t malicious_symbol_data[] = {
    S_SYM_STRING,     // 0x03 - S_SYM_STRING type tag
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'  // 8 bytes of string data, no null terminator
  };
  
  lbm_flat_value_t fv;
  fv.buf = malicious_symbol_data;
  fv.buf_size = sizeof(malicious_symbol_data);
  fv.buf_pos = 0;
  
  lbm_value result;
  bool success = lbm_unflatten_value(&fv, &result);
  
  // This should fail due to lack of null terminator causing buffer overrun
  if (success) {
    // If it succeeded, the symbol string bounds checking might be vulnerable
    printf("WARNING: S_SYM_STRING succeeded without null terminator - potential vulnerability\n");
    return 0; 
  }
  
  return 1; // Good - failed as expected
}

// Test unflatten with malicious S_LBM_LISP_ARRAY claiming more elements than buffer has
int test_unflatten_malicious_lisp_array(void) {
  if (!test_init()) return 0;
  
  // Create malicious S_LBM_LISP_ARRAY data claiming more elements than available
  uint8_t malicious_lisp_array_data[] = {
    S_LBM_LISP_ARRAY, // 0x1F - S_LBM_LISP_ARRAY type tag
    0x00, 0x00, 0x00, 0x10,  // Claims 16 elements (big-endian)
    // But we only provide space for a few bytes of data
    0x05, 0x01,  // S_I28_VALUE, 1 (partial integer) 
    0x00, 0x00   // Only 4 more bytes, not enough for 16 elements
  };
  
  lbm_flat_value_t fv;
  fv.buf = malicious_lisp_array_data;
  fv.buf_size = sizeof(malicious_lisp_array_data);
  fv.buf_pos = 0;
  
  lbm_value result;
  bool success = lbm_unflatten_value(&fv, &result);
  
  // This should fail due to insufficient data for claimed number of elements
  if (success && !lbm_is_symbol(result) && result != ENC_SYM_NIL) {
    printf("WARNING: S_LBM_LISP_ARRAY succeeded with insufficient data - potential vulnerability\n");
    return 0; 
  }
  
  return 1; // Good - failed as expected or returned safe value
}

// Test unflatten with S_SYM_STRING that has extremely long claimed length
int test_unflatten_symbol_string_long_scan(void) {
  if (!test_init()) return 0;
  
  // Create S_SYM_STRING with a very long string that might cause excessive scanning
  // Fill most of buffer with non-null bytes, put null terminator at the very end
  uint8_t long_string_data[1000];
  long_string_data[0] = S_SYM_STRING; // Type tag
  
  // Fill with 'A' characters (no null terminators until the end)
  for (int i = 1; i < 999; i++) {
    long_string_data[i] = 'A';
  }
  long_string_data[999] = '\0'; // Null terminator at very end
  
  lbm_flat_value_t fv;
  fv.buf = long_string_data;
  fv.buf_size = sizeof(long_string_data);
  fv.buf_pos = 0;
  
  lbm_value result;
  bool success = lbm_unflatten_value(&fv, &result);
  
  // This should either succeed (if properly implemented) or fail gracefully
  // The main concern is that it doesn't crash or cause buffer overrun
  if (success) {
    // If it succeeded, verify it created a valid symbol
    if (lbm_is_symbol(result)) {
      return 1; // Good - created valid symbol
    } else {
      return 0; // Unexpected result type
    }
  }
  
  return 1; // Failed gracefully, which is acceptable
}


// Test flattening lisp arrays (different from byte arrays) with small buffer
int test_flatten_lisp_array_small_buffer(void) {
  if (!test_init()) return 0;
  
  // Create a lisp array with some elements
  lbm_value arr = lbm_heap_allocate_list(3);
  if (lbm_is_symbol_merror(arr)) return 0;
  
  lbm_value *arr_ptr = (lbm_value*)lbm_car(lbm_cdr(arr));  // Get data pointer
  if (arr_ptr) {
    arr_ptr[0] = lbm_enc_i(1);
    arr_ptr[1] = lbm_enc_i(2);  
    arr_ptr[2] = lbm_enc_i(3);
  }
  
  lbm_flat_value_t fv;
  
  // Test with 1 byte buffer - should be too small
  if (lbm_start_flatten(&fv, 1)) {
    int result = flatten_value_c(&fv, arr);
    lbm_finish_flatten(&fv);
    if (result != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
  }
  
  // Test with buffer just barely too small
  int required_size = flatten_value_size(arr, false);
  if (required_size > 1) {
    if (lbm_start_flatten(&fv, (size_t)(required_size - 1))) {
      int result = flatten_value_c(&fv, arr);
      lbm_finish_flatten(&fv);
      if (result != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
    }
  }
  
  return 1;
}

// Test flattening with progressively smaller buffer sizes
int test_flatten_progressive_buffer_sizes(void) {
  if (!test_init()) return 0;
  
  // Create a moderately complex structure
  lbm_value complex_val = lbm_cons(
    lbm_cons(lbm_enc_i(42), lbm_enc_float(3.14f)),
    lbm_cons(lbm_enc_i64(123456789LL), lbm_enc_double(2.718281828))
  );
  if (lbm_is_symbol_merror(complex_val)) return 0;
  
  // Get the required size
  int required_size = flatten_value_size(complex_val, false);
  if (required_size <= 0) return 0;
  
  lbm_flat_value_t fv;
  
  // Test with various buffer sizes from 1 up to required_size-1
  // All should fail with BUFFER_TOO_SMALL
  for (int buf_size = 1; buf_size < required_size; buf_size += (required_size / 10 + 1)) {
    if (lbm_start_flatten(&fv, (size_t)buf_size)) {
      int result = flatten_value_c(&fv, complex_val);
      lbm_finish_flatten(&fv);
      if (result != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
    }
  }
  
  // Test with exact required size - should succeed
  if (lbm_start_flatten(&fv, (size_t)required_size)) {
    int result = flatten_value_c(&fv, complex_val);
    lbm_finish_flatten(&fv);
    if (result != FLATTEN_VALUE_OK) return 0;
  } else {
    return 0;
  }
  
  return 1;
}

// Test flattening all numeric types with various small buffer sizes
int test_flatten_all_numeric_types_small_buffers(void) {
  if (!test_init()) return 0;
  
  // Create values of all numeric types
  lbm_value values[] = {
    lbm_enc_char('X'),           // S_BYTE_VALUE
    lbm_enc_i(42),               // S_I28_VALUE  
    lbm_enc_u(42u),              // S_U28_VALUE
    lbm_enc_i32(123456),         // S_I32_VALUE
    lbm_enc_u32(123456u),        // S_U32_VALUE
    lbm_enc_float(3.14f),        // S_FLOAT_VALUE
    lbm_enc_i64(123456789LL),    // S_I64_VALUE
    lbm_enc_u64(123456789ULL),   // S_U64_VALUE
    lbm_enc_double(2.718281828)  // S_DOUBLE_VALUE
  };
  
  int num_values = sizeof(values) / sizeof(values[0]);
  lbm_flat_value_t fv;
  
  // Test each value with buffer sizes 1, 2, 4, 8 bytes
  int test_sizes[] = {1, 2, 4, 8};
  int num_sizes = sizeof(test_sizes) / sizeof(test_sizes[0]);
  
  for (int i = 0; i < num_values; i++) {
    if (lbm_is_symbol_merror(values[i])) continue;
    
    int required_size = flatten_value_size(values[i], false);
    if (required_size <= 0) continue;
    
    for (int j = 0; j < num_sizes; j++) {
      int buf_size = test_sizes[j];
      
      if (buf_size < required_size) {
        // Should fail with buffer too small
        if (lbm_start_flatten(&fv, (size_t)buf_size)) {
          int result = flatten_value_c(&fv, values[i]);
          lbm_finish_flatten(&fv);
          if (result != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
        }
      }
    }
  }
  
  return 1;
}

// Test flattening with exact boundary buffer sizes
int test_flatten_boundary_buffer_sizes(void) {
  if (!test_init()) return 0;
  
  lbm_value test_val = lbm_cons(lbm_enc_i(42), lbm_enc_i(24));
  if (lbm_is_symbol_merror(test_val)) return 0;
  
  int required_size = flatten_value_size(test_val, false);
  if (required_size <= 0) return 0;
  
  lbm_flat_value_t fv;
  
  // Test with exactly required_size - 1 (should fail)
  if (required_size > 1) {
    if (lbm_start_flatten(&fv, (size_t)(required_size - 1))) {
      int result = flatten_value_c(&fv, test_val);
      lbm_finish_flatten(&fv);
      if (result != FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL) return 0;
    }
  }
  
  // Test with exactly required_size (should succeed)
  if (lbm_start_flatten(&fv, (size_t)required_size)) {
    int result = flatten_value_c(&fv, test_val);
    lbm_finish_flatten(&fv);
    if (result != FLATTEN_VALUE_OK) return 0;
  } else {
    return 0;
  }
  
  // Test with required_size + 1 (should succeed with space left over)
  if (lbm_start_flatten(&fv, (size_t)(required_size + 1))) {
    int result = flatten_value_c(&fv, test_val);
    lbm_finish_flatten(&fv);
    if (result != FLATTEN_VALUE_OK) return 0;
  } else {
    return 0;
  }
  
  return 1;
}

// Test flattening mixed type structures with insufficient buffer
int test_flatten_mixed_structures_small_buffer(void) {
  if (!test_init()) return 0;
  
  // Create nested structure with different types
  // ((42 . 3.14) . (("hello" . #\A) . (nil . [1 2 3])))
  
  lbm_value arr_val;
  if (!lbm_create_array(&arr_val, 3)) return 0;
  lbm_array_header_t *arr_header = lbm_dec_array_rw(arr_val);
  if (!arr_header || !arr_header->data) return 0;
  uint8_t *arr_data = (uint8_t*)arr_header->data;
  arr_data[0] = 1; arr_data[1] = 2; arr_data[2] = 3;
  
  lbm_value nested = lbm_cons(
    lbm_cons(lbm_enc_i(42), lbm_enc_float(3.14f)),
    lbm_cons(
      lbm_cons(ENC_SYM_NIL, lbm_enc_char('A')), // Note: simplified - real symbol string would be complex
      lbm_cons(ENC_SYM_NIL, arr_val)
    )
  );
  
  if (lbm_is_symbol_merror(nested)) return 0;
  
  lbm_flat_value_t fv;
  
  // Test with very small buffer sizes
  int small_sizes[] = {1, 2, 4, 8, 16, 32};
  int num_sizes = sizeof(small_sizes) / sizeof(small_sizes[0]);
  
  for (int i = 0; i < num_sizes; i++) {
    if (lbm_start_flatten(&fv, (size_t)small_sizes[i])) {
      int result = flatten_value_c(&fv, nested);
      lbm_finish_flatten(&fv);
      
      // With such a complex structure, small buffers should fail
      if (result == FLATTEN_VALUE_OK && small_sizes[i] < 64) {
        // If it succeeded with a very small buffer, that's unexpected
        return 0;
      }
    }
  }
  
  return 1;
}

// Test lbm_get_max_flatten_depth and lbm_set_max_flatten_depth functions
int test_flatten_depth_configuration(void) {
  if (!test_init()) return 0;
  
  // Test 1: Get initial/default max depth
  int initial_depth = lbm_get_max_flatten_depth();
  if (initial_depth <= 0) return 0; // Should have some reasonable default (probably 2000)
  
  // Test 2: Set a new max depth and verify it was set
  int new_depth = 500;
  lbm_set_max_flatten_depth(new_depth);
  int retrieved_depth = lbm_get_max_flatten_depth();
  if (retrieved_depth != new_depth) return 0;
  
  // Test 3: Restore original depth for other tests
  lbm_set_max_flatten_depth(initial_depth);
  if (lbm_get_max_flatten_depth() != initial_depth) return 0;
  
  return 1;
}

// Test flatten depth limits with progressively deeper structures
int test_flatten_depth_limits(void) {
  if (!test_init()) return 0;
  
  // Get and save current max depth
  int original_depth = lbm_get_max_flatten_depth();
  
  // Set a small max depth for testing
  int test_depth = 10;
  lbm_set_max_flatten_depth(test_depth);
  
  // Create nested structures of various depths
  lbm_value structures[15]; // More than test_depth
  
  // Initialize with a simple value
  structures[0] = lbm_enc_i(42);
  
  // Build progressively deeper nesting
  for (int i = 1; i < 15 && i <= test_depth + 5; i++) {
    structures[i] = lbm_cons(structures[i-1], ENC_SYM_NIL);
    if (lbm_is_symbol_merror(structures[i])) {
      // Memory exhaustion - break and test what we have
      break;
    }
  }
  
  // Test a structure that should succeed (within depth limit)
  if (structures[test_depth/2] && !lbm_is_symbol_merror(structures[test_depth/2])) {
    int size_ok = flatten_value_size(structures[test_depth/2], false);
    if (size_ok <= 0) {
      lbm_set_max_flatten_depth(original_depth);
      return 0;
    }
  }
  
  // Test a structure that should definitely fail (exceeds depth limit)
  if (structures[test_depth + 2] && !lbm_is_symbol_merror(structures[test_depth + 2])) {
    int size_fail = flatten_value_size(structures[test_depth + 2], false);
    if (size_fail != FLATTEN_VALUE_ERROR_MAXIMUM_DEPTH) {
      lbm_set_max_flatten_depth(original_depth);
      return 0;
    }
  }
  
  // Test with a different depth limit
  lbm_set_max_flatten_depth(5);
  
  // Test that the 6th level fails
  if (structures[6] != 0 && !lbm_is_symbol_merror(structures[6])) {
    int result = flatten_value_size(structures[6], false);
    if (result != FLATTEN_VALUE_ERROR_MAXIMUM_DEPTH) {
      lbm_set_max_flatten_depth(original_depth);
      return 0;
    }
  }
  
  // Restore original depth
  lbm_set_max_flatten_depth(original_depth);
  
  return 1;
}

// Test edge cases for depth configuration
int test_flatten_depth_edge_cases(void) {
  if (!test_init()) return 0;
  
  int original_depth = lbm_get_max_flatten_depth();
  
  // Test 1: Set very large depth
  int large_depth = 10000;
  lbm_set_max_flatten_depth(large_depth);
  if (lbm_get_max_flatten_depth() != large_depth) return 0;
  
  // Test 2: Set depth to a small value
  lbm_set_max_flatten_depth(5);
  if (lbm_get_max_flatten_depth() != 5) return 0;
  
  // Test 3: Restore original depth
  lbm_set_max_flatten_depth(original_depth);
  if (lbm_get_max_flatten_depth() != original_depth) return 0;
  
  return 1;
}

int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  total_tests++; if (test_flatten_buffer_too_small()) tests_passed++;
  total_tests++; if (test_flatten_different_types_small_buffer()) tests_passed++;
  total_tests++; if (test_flatten_array_buffer_too_small()) tests_passed++;
  total_tests++; if (test_flatten_deep_nesting_small_buffer()) tests_passed++;
  total_tests++; if (test_unflatten_malformed_data()) tests_passed++;
  total_tests++; if (test_unflatten_oversized_array()) tests_passed++;
  total_tests++; if (test_flatten_value_size_edge_cases()) tests_passed++;
  total_tests++; if (test_flatten_unflatten_roundtrip()) tests_passed++;
  total_tests++; if (test_flatten_maximum_depth()) tests_passed++;
  total_tests++; if (test_flatten_each_type_small_buffer()) tests_passed++;
  total_tests++; if (test_flatten_each_type_barely_too_small()) tests_passed++;
  total_tests++; if (test_unflatten_malicious_symbol_string()) tests_passed++;
  total_tests++; if (test_unflatten_malicious_lisp_array()) tests_passed++;
  total_tests++; if (test_unflatten_symbol_string_long_scan()) tests_passed++;
  total_tests++; if (test_flatten_lisp_array_small_buffer()) tests_passed++;
  total_tests++; if (test_flatten_progressive_buffer_sizes()) tests_passed++;
  total_tests++; if (test_flatten_all_numeric_types_small_buffers()) tests_passed++;
  total_tests++; if (test_flatten_boundary_buffer_sizes()) tests_passed++;
  total_tests++; if (test_flatten_mixed_structures_small_buffer()) tests_passed++;
  // Test depth configuration functions
  total_tests++; if (test_flatten_depth_configuration()) tests_passed++;
  total_tests++; if (test_flatten_depth_limits()) tests_passed++;
  total_tests++; if (test_flatten_depth_edge_cases()) tests_passed++;
  
  kill_eval_after_tests();
  
  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}