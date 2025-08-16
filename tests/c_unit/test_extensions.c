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

#include "lispbm.h"
#include "extensions.h"

#include "init/start_lispbm.c"

// Test extension functions
lbm_value test_extension_1(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return lbm_enc_i(42);
}

lbm_value test_extension_2(lbm_value *args, lbm_uint argn) {
  if (argn == 1 && lbm_is_number(args[0])) {
    return lbm_enc_i(lbm_dec_i(args[0]) * 2);
  }
  return ENC_SYM_EERROR;
}

lbm_value test_extension_number_check(lbm_value *args, lbm_uint argn) {
  if (!lbm_check_number_all(args, argn)) {
    return ENC_SYM_EERROR;
  }
  return ENC_SYM_TRUE;
}

lbm_value test_extension_argn_check(lbm_value *args, lbm_uint argn) {
  (void)args;
  if (!lbm_check_argn(argn, 2)) {
    return ENC_SYM_EERROR;
  }
  return ENC_SYM_TRUE;
}

lbm_value test_extension_argn_number_check(lbm_value *args, lbm_uint argn) {
  if (!lbm_check_argn_number(args, argn, 2)) {
    return ENC_SYM_EERROR;
  }
  return ENC_SYM_TRUE;
}

int test_lbm_extensions_init(void) {
  // Test 1: Initialize with valid parameters
  lbm_extension_t test_storage[10];
  bool result1 = lbm_extensions_init(test_storage, 10);
  
  // Test 2: Check that max extensions is set correctly
  lbm_uint max_ext = lbm_get_max_extensions();
  
  // Test 3: Check that num extensions starts at 0
  lbm_uint num_ext = lbm_get_num_extensions();
  
  // Test 4: Try to initialize with NULL storage (should fail)
  bool result2 = lbm_extensions_init(NULL, 10);
  
  // Test 5: Try to initialize with 0 size (should fail)
  bool result3 = lbm_extensions_init(test_storage, 0);
  
  if (!result1 || max_ext != 10 || num_ext != 0 || result2 || result3) {
    return 0;
  }
  
  return 1;
}

int test_lbm_extensions_set_next(void) {
  lbm_extension_t test_storage[10];
  if (!lbm_extensions_init(test_storage, 10)) return 0;
  
  // Test 1: Set next extension index
  lbm_extensions_set_next(5);
  lbm_uint num_ext = lbm_get_num_extensions();
  
  // Test 2: Reset to 0
  lbm_extensions_set_next(0);
  lbm_uint num_ext2 = lbm_get_num_extensions();
  
  if (num_ext != 5 || num_ext2 != 0) {
    return 0;
  }
  
  return 1;
}

int test_lbm_extensions_default(void) {
  // Test: Default extension should return EERROR
  lbm_value args[2] = {lbm_enc_i(1), lbm_enc_i(2)};
  lbm_value result = lbm_extensions_default(args, 2);
  
  if (result != ENC_SYM_EERROR) {
    return 0;
  }
  
  return 1;
}

int test_lbm_add_extension(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_extension_t test_storage[10];
  if (!lbm_extensions_init(test_storage, 10)) return 0;
  
  // Test 1: Add first extension
  bool result1 = lbm_add_extension("test-ext-1", test_extension_1);
  
  // Test 2: Add second extension
  bool result2 = lbm_add_extension("test-ext-2", test_extension_2);
  
  // Test 3: Try to update existing extension
  bool result3 = lbm_add_extension("test-ext-1", test_extension_2);
  
  // Test 4: Check number of extensions increased
  lbm_uint num_ext = lbm_get_num_extensions();
  
  if (!result1 || !result2 || !result3 || num_ext != 2) {
    return 0;
  }
  
  return 1;
}

int test_lbm_add_extension_not_extension(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_extension_t test_storage[10];
  if (!lbm_extensions_init(test_storage, 10)) return 0;
  
  // Test 1: Add first extension
  bool result1 = lbm_add_extension("+", test_extension_1);
  
  // Test 2: Add second extension
  bool result2 = lbm_add_extension("car", test_extension_2);
  
  // Test 3: Try to update existing extension
  bool result3 = lbm_add_extension("eval_error", test_extension_2);
  
  // Test 4: Check number of extensions increased
  lbm_uint num_ext = lbm_get_num_extensions();
  
  if (result1 || result2 || result3 || num_ext != 0) {
    return 0;
  }
  
  return 1;
}


int test_lbm_lookup_extension_id(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_extension_t test_storage[10];
  if (!lbm_extensions_init(test_storage, 10)) return 0;
  
  // Add an extension first
  if (!lbm_add_extension("lookup-test", test_extension_1)) return 0;
  
  // Test 1: Look up existing extension
  lbm_uint id1;
  bool result1 = lbm_lookup_extension_id("lookup-test", &id1);
  
  // Test 2: Try to look up non-existent extension
  lbm_uint id2;
  bool result2 = lbm_lookup_extension_id("non-existent", &id2);
  
  if (!result1 || result2 || id1 != EXTENSION_SYMBOLS_START) {
    return 0;
  }
  
  return 1;
}

int test_lbm_get_extension(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_extension_t test_storage[10];
  if (!lbm_extensions_init(test_storage, 10)) return 0;
  
  // Add an extension first
  if (!lbm_add_extension("get-test", test_extension_1)) return 0;
  
  // Test 1: Get extension function pointer
  extension_fptr fptr1 = lbm_get_extension(EXTENSION_SYMBOLS_START);
  
  // Test 2: Try to get extension with invalid symbol
  extension_fptr fptr2 = lbm_get_extension(EXTENSION_SYMBOLS_START + 100);
  
  // Test 3: Call the retrieved extension
  lbm_value args[1] = {lbm_enc_i(1)};
  lbm_value result = fptr1(args, 1);
  
  if (fptr1 != test_extension_1 || fptr2 != lbm_extensions_default || result != lbm_enc_i(42)) {
    return 0;
  }
  
  return 1;
}

int test_lbm_clr_extension(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_extension_t test_storage[10];
  if (!lbm_extensions_init(test_storage, 10)) return 0;
  
  // Add an extension first
  if (!lbm_add_extension("clear-test", test_extension_1)) return 0;
  
  // Test 1: Clear existing extension
  bool result1 = lbm_clr_extension(EXTENSION_SYMBOLS_START);
  
  // Test 2: Verify extension is cleared (should return default)
  extension_fptr fptr = lbm_get_extension(EXTENSION_SYMBOLS_START);
  
  // Test 3: Try to clear non-existent extension
  bool result2 = lbm_clr_extension(EXTENSION_SYMBOLS_START + 100);
  
  if (!result1 || fptr != lbm_extensions_default || result2) {
    return 0;
  }
  
  return 1;
}

int test_lbm_check_number_all(void) {
  // Test 1: All numbers
  lbm_value args1[3] = {lbm_enc_i(1), lbm_enc_float(2.5f), lbm_enc_i(3)};
  bool result1 = lbm_check_number_all(args1, 3);
  
  // Test 2: Mixed with non-number
  lbm_value args2[3] = {lbm_enc_i(1), ENC_SYM_NIL, lbm_enc_i(3)};
  bool result2 = lbm_check_number_all(args2, 3);
  
  // Test 3: Empty array
  bool result3 = lbm_check_number_all(NULL, 0);
  
  if (!result1 || result2 || !result3) {
    return 0;
  }
  
  return 1;
}

int test_lbm_check_argn(void) {
  // Test 1: Correct number of arguments
  bool result1 = lbm_check_argn(3, 3);
  
  // Test 2: Wrong number of arguments
  bool result2 = lbm_check_argn(2, 3);
  
  // Test 3: Zero arguments
  bool result3 = lbm_check_argn(0, 0);
  
  if (!result1 || result2 || !result3) {
    return 0;
  }
  
  return 1;
}

int test_lbm_check_argn_number(void) {
  // Test 1: Correct number of number arguments
  lbm_value args1[2] = {lbm_enc_i(1), lbm_enc_float(2.5f)};
  bool result1 = lbm_check_argn_number(args1, 2, 2);
  
  // Test 2: Wrong number of arguments
  lbm_value args2[1] = {lbm_enc_i(1)};
  bool result2 = lbm_check_argn_number(args2, 1, 2);
  
  // Test 3: Right number but non-number argument
  lbm_value args3[2] = {lbm_enc_i(1), ENC_SYM_NIL};
  bool result3 = lbm_check_argn_number(args3, 2, 2);
  
  if (!result1 || result2 || result3) {
    return 0;
  }
  
  return 1;
}

int test_make_list(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  // Test 1: Make list with 3 elements
  lbm_value list1 = make_list(3, lbm_enc_i(1), lbm_enc_i(2), lbm_enc_i(3));
  
  // Test 2: Make empty list
  lbm_value list2 = make_list(0);
  
  // Test 3: Verify first list structure
  if (!lbm_is_cons(list1)) return 0;
  lbm_value car1 = lbm_car(list1);
  lbm_value cdr1 = lbm_cdr(list1);
  
  if (!lbm_is_cons(cdr1)) return 0;
  lbm_value car2 = lbm_car(cdr1);
  lbm_value cdr2 = lbm_cdr(cdr1);
  
  if (!lbm_is_cons(cdr2)) return 0;
  lbm_value car3 = lbm_car(cdr2);
  lbm_value cdr3 = lbm_cdr(cdr2);
  
  if (car1 != lbm_enc_i(1) || car2 != lbm_enc_i(2) || car3 != lbm_enc_i(3) || 
      cdr3 != ENC_SYM_NIL || list2 != ENC_SYM_NIL) {
    return 0;
  }
  
  return 1;
}

int test_strmatch(void) {
  // Test 1: Matching strings with space separator
  bool result1 = strmatch("test", "test more content");
  
  // Test 2: Non-matching strings
  bool result2 = strmatch("test", "testing more content");
  
  // Test 3: String without space separator
  bool result3 = strmatch("test", "testmore");
  
  // Test 4: Empty string match
  bool result4 = strmatch("", " content");
  
  if (!result1 || result2 || result3 || !result4) {
    return 0;
  }
  
  return 1;
}

int test_extension_helpers(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_extension_t test_storage[10];
  if (!lbm_extensions_init(test_storage, 10)) return 0;
  
  // Add test extensions
  if (!lbm_add_extension("test-number-check", test_extension_number_check) ||
      !lbm_add_extension("test-argn-check", test_extension_argn_check) ||
      !lbm_add_extension("test-argn-number-check", test_extension_argn_number_check)) {
    return 0;
  }
  
  // Test number check extension
  lbm_value args1[2] = {lbm_enc_i(1), lbm_enc_i(2)};
  lbm_value result1 = test_extension_number_check(args1, 2);
  
  lbm_value args2[2] = {lbm_enc_i(1), ENC_SYM_NIL};
  lbm_value result2 = test_extension_number_check(args2, 2);
  
  // Test argn check extension
  lbm_value result3 = test_extension_argn_check(args1, 2);
  lbm_value result4 = test_extension_argn_check(args1, 1);
  
  // Test argn number check extension
  lbm_value result5 = test_extension_argn_number_check(args1, 2);
  lbm_value result6 = test_extension_argn_number_check(args2, 2);
  
  if (result1 != ENC_SYM_TRUE || result2 != ENC_SYM_EERROR ||
      result3 != ENC_SYM_TRUE || result4 != ENC_SYM_EERROR ||
      result5 != ENC_SYM_TRUE || result6 != ENC_SYM_EERROR) {
    return 0;
  }
  
  return 1;
}

int test_extension_table_full(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  // Test with very small extension table
  lbm_extension_t test_storage[2];
  if (!lbm_extensions_init(test_storage, 2)) return 0;
  
  // Fill up the extension table
  bool result1 = lbm_add_extension("ext1", test_extension_1);
  bool result2 = lbm_add_extension("ext2", test_extension_2);
  
  // Try to add one more (should fail)
  bool result3 = lbm_add_extension("ext3", test_extension_1);
  
  if (!result1 || !result2 || result3) {
    return 0;
  }
  
  return 1;
}



int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  printf("Starting extensions tests...\n");
  
  total_tests++; if (test_lbm_extensions_init()) {
    tests_passed++;
    printf("test_lbm_extensions_init passed\n");
  } else {
    printf("test_lbm_extensions_init FAILED\n");
  }
  
  total_tests++; if (test_lbm_extensions_set_next()) {
    tests_passed++;
    printf("test_lbm_extensions_set_next passed\n");
  } else {
    printf("test_lbm_extensions_set_next FAILED\n");
  }
  
  total_tests++; if (test_lbm_extensions_default()) {
    tests_passed++;
    printf("test_lbm_extensions_default passed\n");
  } else {
    printf("test_lbm_extensions_default FAILED\n");
  }
  
  total_tests++; if (test_lbm_add_extension()) {
    tests_passed++;
    printf("test_lbm_add_extension passed\n");
  } else {
    printf("test_lbm_add_extension FAILED\n");
  }
  
  total_tests++; if (test_lbm_lookup_extension_id()) {
    tests_passed++;
    printf("test_lbm_lookup_extension_id passed\n");
  } else {
    printf("test_lbm_lookup_extension_id FAILED\n");
  }
  
  total_tests++; if (test_lbm_get_extension()) {
    tests_passed++;
    printf("test_lbm_get_extension passed\n");
  } else {
    printf("test_lbm_get_extension FAILED\n");
  }
  
  total_tests++; if (test_lbm_clr_extension()) {
    tests_passed++;
    printf("test_lbm_clr_extension passed\n");
  } else {
    printf("test_lbm_clr_extension FAILED\n");
  }
  
  total_tests++; if (test_lbm_check_number_all()) {
    tests_passed++;
    printf("test_lbm_check_number_all passed\n");
  } else {
    printf("test_lbm_check_number_all FAILED\n");
  }
  
  total_tests++; if (test_lbm_check_argn()) {
    tests_passed++;
    printf("test_lbm_check_argn passed\n");
  } else {
    printf("test_lbm_check_argn FAILED\n");
  }
  
  total_tests++; if (test_lbm_check_argn_number()) {
    tests_passed++;
    printf("test_lbm_check_argn_number passed\n");
  } else {
    printf("test_lbm_check_argn_number FAILED\n");
  }
  
  total_tests++; if (test_make_list()) {
    tests_passed++;
    printf("test_make_list passed\n");
  } else {
    printf("test_make_list FAILED\n");
  }
  
  total_tests++; if (test_strmatch()) {
    tests_passed++;
    printf("test_strmatch passed\n");
  } else {
    printf("test_strmatch FAILED\n");
  }
  
  total_tests++; if (test_extension_helpers()) {
    tests_passed++;
    printf("test_extension_helpers passed\n");
  } else {
    printf("test_extension_helpers FAILED\n");
  }
  
  total_tests++; if (test_extension_table_full()) {
    tests_passed++;
    printf("test_extension_table_full passed\n");
  } else {
    printf("test_extension_table_full FAILED\n");
  }

  total_tests++; if ( test_lbm_add_extension_not_extension()) {
    tests_passed++;
    printf("test_lbm_add_extension_not_extension passed\n");
  } else {
    printf("test_lbm_add_extension_not_extension FAILED\n");
  } 

  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
