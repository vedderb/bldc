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
#include "symrepr.h"

#include "init/start_lispbm.c"

int test_lbm_symrepr_init(void) {
  // Test 1: Initialize symbol representation
  bool result1 = lbm_symrepr_init();
  
  // Test 2: Check that next_symbol_id is set to RUNTIME_SYMBOLS_START
  lbm_uint next_id = lbm_symrepr_get_next_id();
  
  if (!result1 || next_id != RUNTIME_SYMBOLS_START) {
    return 0;
  }
  
  return 1;
}

int test_lbm_symrepr_next_id(void) {
  if (!lbm_symrepr_init()) return 0;
  
  // Test 1: Get initial next id
  lbm_uint initial_id = lbm_symrepr_get_next_id();
  
  // Test 2: Set a new next id
  lbm_uint new_id = initial_id + 100;
  lbm_symrepr_set_next_id(new_id);
  
  // Test 3: Verify the id was set correctly
  lbm_uint retrieved_id = lbm_symrepr_get_next_id();
  
  if (initial_id != RUNTIME_SYMBOLS_START || retrieved_id != new_id) {
    return 0;
  }
  
  return 1;
}

int test_lbm_get_symbol_by_name_special(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  // Test 1: Look up special symbol "nil"
  lbm_uint nil_id;
  int result1 = lbm_get_symbol_by_name("nil", &nil_id);
  
  // Test 2: Look up special symbol "+"
  lbm_uint add_id;
  int result2 = lbm_get_symbol_by_name("+", &add_id);
  
  // Test 3: Look up special symbol "cons"
  lbm_uint cons_id;
  int result3 = lbm_get_symbol_by_name("cons", &cons_id);
  
  // Test 4: Try to look up non-existent symbol
  lbm_uint nonexistent_id;
  int result4 = lbm_get_symbol_by_name("non-existent-symbol", &nonexistent_id);
  
  if (!result1 || !result2 || !result3 || result4) {
    return 0;
  }
  
  // Verify we got expected symbol IDs
  if (nil_id != SYM_NIL || add_id != SYM_ADD || cons_id != SYM_CONS) {
    return 0;
  }
  
  return 1;
}

int test_lbm_get_name_by_symbol(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  // Test 1: Look up name for "nil" symbol
  const char *nil_name = lbm_get_name_by_symbol(SYM_NIL);
  
  // Test 2: Look up name for "+" symbol
  const char *add_name = lbm_get_name_by_symbol(SYM_ADD);
  
  // Test 3: Look up name for "cons" symbol
  const char *cons_name = lbm_get_name_by_symbol(SYM_CONS);
  
  // Test 4: Try to look up invalid symbol ID
  const char *invalid_name = lbm_get_name_by_symbol(0xFFFFFFFF);
  
  if (!nil_name || !add_name || !cons_name || invalid_name) {
    return 0;
  }
  
  // Verify we got expected names
  if (strcmp(nil_name, "nil") != 0 || 
      strcmp(add_name, "+") != 0 || 
      strcmp(cons_name, "cons") != 0) {
    return 0;
  }
  
  return 1;
}


// When adding symbols from a separate task, it makes sense to pause!
int test_lbm_add_symbol(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_pause_eval();
  int timeout = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }
  if (timeout >= 5) return 0;
  
  // Test 1: Add a new symbol
  lbm_uint symbol1_id;
  int result1 = lbm_add_symbol("test-symbol-1", &symbol1_id);
  
  // Test 2: Add another new symbol
  lbm_uint symbol2_id;
  int result2 = lbm_add_symbol("test-symbol-2", &symbol2_id);
  
  // Test 3: Try to add an existing symbol (should return existing ID)
  lbm_uint symbol1_id_again;
  int result3 = lbm_add_symbol("test-symbol-1", &symbol1_id_again);
  
  // Test 4: Verify we can look up the added symbols
  lbm_uint lookup_id;
  int result4 = lbm_get_symbol_by_name("test-symbol-1", &lookup_id);
  
  if (!result1 || !result2 || !result3 || !result4) {
    return 0;
  }
  
  if (symbol1_id != symbol1_id_again || lookup_id != symbol1_id) {
    return 0;
  }
  
  // Symbols should have different IDs
  if (symbol1_id == symbol2_id) {
    return 0;
  }
  
  return 1;
}

int test_lbm_add_symbol_const(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_pause_eval();
  int timeout = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }
  if (timeout >= 5) return 0;
  
  // Test 1: Add a new const symbol
  lbm_uint symbol1_id;
  int result1 = lbm_add_symbol_const("test-const-symbol-1", &symbol1_id);
  
  // Test 2: Add another const symbol
  lbm_uint symbol2_id;
  int result2 = lbm_add_symbol_const("test-const-symbol-2", &symbol2_id);
  
  // Test 3: Try to add an existing const symbol (should return existing ID)
  lbm_uint symbol1_id_again;
  int result3 = lbm_add_symbol_const("test-const-symbol-1", &symbol1_id_again);
  
  if (!result1 || !result2 || !result3) {
    return 0;
  }
  
  if (symbol1_id != symbol1_id_again) {
    return 0;
  }
  
  // Symbols should have different IDs
  if (symbol1_id == symbol2_id) {
    return 0;
  }
  
  return 1;
}

int test_lbm_str_to_symbol(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_pause_eval();
  int timeout = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }
  if (timeout >= 5) return 0;
  
  // Test 1: Convert existing special symbol name to symbol
  lbm_uint nil_id;
  int result1 = lbm_str_to_symbol("nil", &nil_id);
  
  // Test 2: Convert new symbol name to symbol (should create it)
  lbm_uint new_symbol_id;
  int result2 = lbm_str_to_symbol("str-to-symbol-test", &new_symbol_id);
  
  // Test 3: Convert the same new symbol name again (should return same ID)
  lbm_uint same_symbol_id;
  int result3 = lbm_str_to_symbol("str-to-symbol-test", &same_symbol_id);
  
  if (!result1 || !result2 || !result3) {
    return 0;
  }
  
  if (nil_id != SYM_NIL || new_symbol_id != same_symbol_id) {
    return 0;
  }
  
  return 1;
}

// It is also a good idea to pause when traversing the symbol list from a separate task
int test_lbm_get_symbol_list_entry_by_name(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_pause_eval();
  int timeout = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }
  if (timeout >= 5) return 0;
  
  // First add a symbol
  lbm_uint symbol_id;
  if (!lbm_add_symbol("list-entry-test", &symbol_id)) {
    return 0;
  }
  
  // Test 1: Get list entry for the symbol we just added
  lbm_uint *entry1 = lbm_get_symbol_list_entry_by_name("list-entry-test");
  
  // Test 2: Try to get list entry for non-existent symbol
  lbm_uint *entry2 = lbm_get_symbol_list_entry_by_name("non-existent-list-entry");
  
  // Test 3: Get list entry for special symbol (should return NULL since they're not in the list)
  lbm_uint *entry3 = lbm_get_symbol_list_entry_by_name("nil");
  
  if (!entry1 || entry2 || entry3) {
    return 0;
  }
  
  return 1;
}

// TODO: Only symbol_table_size_strings_flash is ever incremented
//       in symrepr.c. This is probably an oversight coming from
//       the implementation of images when all symbol storage moved to flash.
//       Since the images were introduced all storage, list and names
//       associated with the symrepr is in "flash".
//       (What would be flash on an embedded platform. Simulated on desktop builds)
int test_lbm_get_symbol_table_sizes(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  // Test 1: Get initial sizes
  lbm_uint size1 = lbm_get_symbol_table_size();
  lbm_uint flash_size1 = lbm_get_symbol_table_size_flash();
  lbm_uint names_size1 = lbm_get_symbol_table_size_names();
  lbm_uint names_flash_size1 = lbm_get_symbol_table_size_names_flash();
  
  lbm_pause_eval();
  int timeout = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }
  if (timeout >= 5) return 0;
  
  // Add a symbol to change the sizes
  lbm_uint symbol_id;
  if (!lbm_add_symbol("size-test-symbol", &symbol_id)) {
    return 0;
  }
  
  // Test 2: Get sizes after adding symbol
  lbm_uint size2 = lbm_get_symbol_table_size();
  lbm_uint flash_size2 = lbm_get_symbol_table_size_flash();
  lbm_uint names_size2 = lbm_get_symbol_table_size_names();
  lbm_uint names_flash_size2 = lbm_get_symbol_table_size_names_flash();
  
  // Flash sizes should increase after adding a symbol
  // (Non-flash sizes may stay the same since lbm_add_symbol stores names in flash)
  if (flash_size2 <= flash_size1 || names_flash_size2 <= names_flash_size1) {
    return 0;
  }
  
  // All size functions should return non-negative values
  (void)size1; (void)size2; (void)names_size1; (void)names_size2; // May be 0, that's ok
  
  return 1;
}

int test_lbm_symbol_in_flash(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_pause_eval();
  int timeout = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }
  if (timeout >= 5) return 0;
  
  // Add a symbol
  lbm_uint symbol_id;
  if (!lbm_add_symbol("flash-test-symbol", &symbol_id)) {
    return 0;
  }
  
  // Test 1: Check if symbol name is in flash
  bool in_flash1 = lbm_symbol_in_flash("flash-test-symbol");
  
  // Test 2: Check if symbol list entry is in flash  
  bool entry_in_flash1 = lbm_symbol_list_entry_in_flash("flash-test-symbol");
  
  // Test 3: Check with non-existent symbol
  bool entry_in_flash2 = lbm_symbol_list_entry_in_flash("non-existent-flash-symbol");
  
  // For symbols added with lbm_add_symbol, they should be in flash
  if (!in_flash1 || !entry_in_flash1 || !entry_in_flash2) {
    return 0;
  }
  
  return 1;
}

static int iterator_call_count = 0;
static void test_iterator_function(const char *name) {
  (void)name; // Suppress unused parameter warning
  iterator_call_count++;
}

int test_lbm_symrepr_name_iterator(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_pause_eval();
  int timeout = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }
  if (timeout >= 5) return 0;
  
  // Add some symbols first
  lbm_uint id1, id2;
  if (!lbm_add_symbol("iterator-test-1", &id1) ||
      !lbm_add_symbol("iterator-test-2", &id2)) {
    return 0;
  }
  
  // Test: Iterate over symbol names
  iterator_call_count = 0;
  lbm_symrepr_name_iterator(test_iterator_function);
  
  // Should have called the iterator function at least twice (for our added symbols)
  if (iterator_call_count < 2) {
    return 0;
  }
  
  return 1;
}

int test_lbm_add_symbol_base(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_pause_eval();
  int timeout = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }
  if (timeout >= 5) return 0;
  
  // Test 1: Add a symbol using base function
  lbm_uint symbol_id;
  int result1 = lbm_add_symbol_base("base-test-symbol", &symbol_id);
  
  // Test 2: Verify we can look it up
  lbm_uint lookup_id;
  int result2 = lbm_get_symbol_by_name("base-test-symbol", &lookup_id);
  
  // Test 3: Try to add empty symbol name (should fail)
  lbm_uint empty_id;
  int result3 = lbm_add_symbol_base("", &empty_id);
  
  if (!result1 || !result2 || result3) {
    return 0;
  }
  
  if (symbol_id != lookup_id) {
    return 0;
  }
  
  return 1;
}

int test_lbm_add_symbol_const_base(void) {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_pause_eval();
  int timeout = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }
  if (timeout >= 5) return 0;
  
  // Test 1: Add a const symbol using base function without linking
  lbm_uint symbol1_id;
  int result1 = lbm_add_symbol_const_base("const-base-test-1", &symbol1_id, false);
  
  // Test 2: Add a const symbol using base function with linking
  lbm_uint symbol2_id;
  int result2 = lbm_add_symbol_const_base("const-base-test-2", &symbol2_id, true);
  
  // Test 3: Verify we can look up both symbols
  lbm_uint lookup1_id, lookup2_id;
  int result3 = lbm_get_symbol_by_name("const-base-test-1", &lookup1_id);
  int result4 = lbm_get_symbol_by_name("const-base-test-2", &lookup2_id);
  
  if (!result1 || !result2 || !result3 || !result4) {
    return 0;
  }
  
  if (symbol1_id != lookup1_id || symbol2_id != lookup2_id) {
    return 0;
  }
  
  return 1;
}

int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  printf("Starting symrepr tests...\n");
  
  total_tests++; if (test_lbm_symrepr_init()) {
    tests_passed++;
    printf("test_lbm_symrepr_init passed\n");
  } else {
    printf("test_lbm_symrepr_init FAILED\n");
  }
  
  total_tests++; if (test_lbm_symrepr_next_id()) {
    tests_passed++;
    printf("test_lbm_symrepr_next_id passed\n");
  } else {
    printf("test_lbm_symrepr_next_id FAILED\n");
  }
  
  total_tests++; if (test_lbm_get_symbol_by_name_special()) {
    tests_passed++;
    printf("test_lbm_get_symbol_by_name_special passed\n");
  } else {
    printf("test_lbm_get_symbol_by_name_special FAILED\n");
  }
  
  total_tests++; if (test_lbm_get_name_by_symbol()) {
    tests_passed++;
    printf("test_lbm_get_name_by_symbol passed\n");
  } else {
    printf("test_lbm_get_name_by_symbol FAILED\n");
  }
  
  total_tests++; if (test_lbm_add_symbol()) {
    tests_passed++;
    printf("test_lbm_add_symbol passed\n");
  } else {
    printf("test_lbm_add_symbol FAILED\n");
  }
  
  total_tests++; if (test_lbm_add_symbol_const()) {
    tests_passed++;
    printf("test_lbm_add_symbol_const passed\n");
  } else {
    printf("test_lbm_add_symbol_const FAILED\n");
  }
  
  total_tests++; if (test_lbm_str_to_symbol()) {
    tests_passed++;
    printf("test_lbm_str_to_symbol passed\n");
  } else {
    printf("test_lbm_str_to_symbol FAILED\n");
  }
  
  total_tests++; if (test_lbm_get_symbol_list_entry_by_name()) {
    tests_passed++;
    printf("test_lbm_get_symbol_list_entry_by_name passed\n");
  } else {
    printf("test_lbm_get_symbol_list_entry_by_name FAILED\n");
  }
  
  total_tests++; if (test_lbm_get_symbol_table_sizes()) {
    tests_passed++;
    printf("test_lbm_get_symbol_table_sizes passed\n");
  } else {
    printf("test_lbm_get_symbol_table_sizes FAILED\n");
  }
  
  total_tests++; if (test_lbm_symbol_in_flash()) {
    tests_passed++;
    printf("test_lbm_symbol_in_flash passed\n");
  } else {
    printf("test_lbm_symbol_in_flash FAILED\n");
  }
  
  total_tests++; if (test_lbm_symrepr_name_iterator()) {
    tests_passed++;
    printf("test_lbm_symrepr_name_iterator passed\n");
  } else {
    printf("test_lbm_symrepr_name_iterator FAILED\n");
  }
  
  total_tests++; if (test_lbm_add_symbol_base()) {
    tests_passed++;
    printf("test_lbm_add_symbol_base passed\n");
  } else {
    printf("test_lbm_add_symbol_base FAILED\n");
  }
  
  total_tests++; if (test_lbm_add_symbol_const_base()) {
    tests_passed++;
    printf("test_lbm_add_symbol_const_base passed\n");
  } else {
    printf("test_lbm_add_symbol_const_base FAILED\n");
  }
  
  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
