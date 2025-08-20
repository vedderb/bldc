#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <stdarg.h>
#include <time.h>

#include "../../include/lispbm.h"
#include "../../include/eval_cps.h"
#include "../../include/lbm_flat_value.h"
#include "../../include/extensions.h"

// Include the start_lispbm initialization helper
#include "init/start_lispbm.c"

// Test lbm_reset_eval enters paused state
int test_lbm_reset_eval_enters_paused() {
  if (!start_lispbm_for_tests()) return 0;
  
  // Let the evaluator run briefly to ensure it's running
  sleep_callback(10000);
  
  // Reset the evaluator - should enter paused state
  lbm_reset_eval();
  
  // Give it time to process the reset
  sleep_callback(10000);
  
  // After reset, evaluator remain in state RESET until continue
  uint32_t state = lbm_get_eval_state();
  return (state == EVAL_CPS_STATE_RESET);
}

// Test lbm_reset_eval followed by lbm_continue_eval
int test_lbm_reset_then_continue() {
  if (!start_lispbm_for_tests()) return 0;
  
  // Let the evaluator run briefly
  sleep_callback(10000);
  
  // Reset the evaluator - should enter paused state
  lbm_reset_eval();
  sleep_callback(10000);
  
  // Verify it's paused
  uint32_t state = lbm_get_eval_state();
  if (state != EVAL_CPS_STATE_RESET) return 0;
  
  // Continue the evaluator - should enter running state
  lbm_continue_eval();
  sleep_callback(10000);
  
  // Verify it's running
  state = lbm_get_eval_state();
  return (state == EVAL_CPS_STATE_RUNNING);
}

// Test multiple reset-continue cycles
int test_lbm_reset_continue_cycles() {
  if (!start_lispbm_for_tests()) return 0;
  
  for (int i = 0; i < 3; i++) {
    // Reset
    lbm_reset_eval();
    sleep_callback(5000);
    
    // Should be paused
    uint32_t state = lbm_get_eval_state();
    if (state != EVAL_CPS_STATE_RESET) return 0;
    
    // Continue
    lbm_continue_eval();
    sleep_callback(5000);
    
    // Should be running
    state = lbm_get_eval_state();
    if (state != EVAL_CPS_STATE_RUNNING) return 0;
  }
  
  return 1;
}

// Test lbm_toggle_verbose function
int test_lbm_toggle_verbose_basic() {
  // Note: lbm_verbose is static in eval_cps.c, so we can't directly check its value
  // But we can call the function and ensure it doesn't crash
  lbm_toggle_verbose();
  lbm_toggle_verbose();
  lbm_toggle_verbose();
  
  return 1; // Success if no crash
}

// Test lbm_surrender_quota function
int test_lbm_surrender_quota_basic() {
  if (!start_lispbm_for_tests()) return 0;
  
  // Call surrender quota
  lbm_surrender_quota();
  
  // Should not crash
  sleep_callback(1000);
  
  return 1;
}

// Test lbm_surrender_quota multiple times
int test_lbm_surrender_quota_multiple() {
  if (!start_lispbm_for_tests()) return 0;
  
  for (int i = 0; i < 5; i++) {
    lbm_surrender_quota();
    sleep_callback(100);
  }
  
  // Should not crash
  return 1;
}

int test_lbm_eval_init_events(void) {
  if (!start_lispbm_for_tests()) return 0;

  if (!lbm_eval_init_events(20)) return 0;
  return 1;
}

// Test lbm_event_define function with proper event handler
int test_lbm_event_define_basic() {
  if (!start_lispbm_for_tests()) return 0;

  if (!lbm_eval_init_events(20)) return 0;
  
  char *handler_code = "(let ((running t)) (loopwhile running (recv ((? msg) msg))))";
  lbm_string_channel_state_t st;
  lbm_char_channel_t chan;
  lbm_create_string_char_channel(&st, &chan, handler_code);
  lbm_cid handler_cid = lbm_load_and_eval_expression(&chan);

  if (handler_cid < 0) return 0;
  
  lbm_set_event_handler_pid(handler_cid);
  
  // Verify event handler exists
  if (!lbm_event_handler_exists()) return 0;
  
  // Create a simple flat value
  lbm_flat_value_t fv;
  
  bool flatten_success = lbm_start_flatten(&fv, 1000);
  if (!flatten_success) return 0;
  
  flatten_success = lbm_finish_flatten(&fv);
  if (!flatten_success) return 0;
  
  // Test event define with proper handler
  lbm_uint sym_id;
  lbm_add_symbol("test-key", &sym_id);
  lbm_value key = lbm_enc_sym(sym_id);
  bool result = lbm_event_define(key, &fv);
  
  // Give time for event processing
  sleep_callback(5000);

  // Clean up
  //if (fv.buf) free(fv.buf);
  // The flatvalue is freed on the receiving side.

  kill_eval_after_tests();
  // With proper handler, should succeed
  return result ? 1 : 0;
}

// Test lbm_event_define with different key types
int test_lbm_event_define_different_keys() {
  if (!start_lispbm_for_tests()) return 0;

  if (!lbm_eval_init_events(20)) return 0;
  
  // Create and register an event handler
  
  char *handler_code = "(let ((running t)) (loopwhile running (recv ((? msg) msg))))";
  lbm_string_channel_state_t st;
  lbm_char_channel_t chan;
  lbm_create_string_char_channel(&st, &chan, handler_code);
  lbm_cid handler_cid = lbm_load_and_eval_expression(&chan);
  
  lbm_set_event_handler_pid(handler_cid);
  
  // Verify event handler exists
  if (!lbm_event_handler_exists()) return 0;
  
  lbm_flat_value_t fv;
  bool flatten_success = lbm_start_flatten(&fv, 100);
  if (!flatten_success) return 0;
  
  flatten_success = lbm_finish_flatten(&fv);
  if (!flatten_success) return 0;
  
  // Test with different key types
  lbm_uint sym_id;
  lbm_add_symbol("symbol-key", &sym_id);
  lbm_value keys[] = {
    lbm_enc_sym(sym_id),
    lbm_enc_i(42),
    lbm_enc_u(123),
    lbm_enc_char('A')
  };
  
  for (size_t i = 0; i < sizeof(keys)/sizeof(keys[0]); i++) {
    lbm_event_define(keys[i], &fv);
    sleep_callback(1000); // Give time for processing
  }
  
  //if (fv.buf) free(fv.buf);
  kill_eval_after_tests();
  return 1;
}

// Test lbm_event_handler_exists function
int test_lbm_event_handler_exists() {
  if (!start_lispbm_for_tests()) return 0;
  
  if (!lbm_eval_init_events(20)) return 0;
  
  // Initially no event handler should exist
  if (lbm_event_handler_exists()) return 0;
  
  // Create and register an event handler
  char *handler_code = "(let ((running t)) (loopwhile running (recv ((? msg) msg))))";
  lbm_string_channel_state_t st;
  lbm_char_channel_t chan;
  lbm_create_string_char_channel(&st, &chan, handler_code);
  lbm_cid handler_cid = lbm_load_and_eval_expression(&chan);

  if (handler_cid < 0) return 0;
  
  lbm_set_event_handler_pid(handler_cid);
  
  // Now event handler should exist
  if (!lbm_event_handler_exists()) return 0;
  
  // Verify we can get the handler PID
  if (lbm_get_event_handler_pid() != handler_cid) return 0;
  kill_eval_after_tests();
  return 1;
}

// Test lbm_event_unboxed with valid unboxed types and proper event handler
int test_lbm_event_unboxed_valid_types() {
  if (!start_lispbm_for_tests()) return 0;
  
  if (!lbm_eval_init_events(20)) return 0;
  
  // Create and register an event handler
  char *handler_code = "(let ((running t)) (loopwhile running (recv ((? msg) msg))))";
  lbm_string_channel_state_t st;
  lbm_char_channel_t chan;
  lbm_create_string_char_channel(&st, &chan, handler_code);
  lbm_cid handler_cid = lbm_load_and_eval_expression(&chan);

  if (handler_cid < 0) return 0;
  
  lbm_set_event_handler_pid(handler_cid);
  
  // Verify event handler exists
  if (!lbm_event_handler_exists()) return 0;
  
  // Test with valid unboxed types that lbm_event_unboxed should accept
  lbm_uint sym_id;
  lbm_add_symbol("test-symbol", &sym_id);
  
  lbm_value valid_values[] = {
    lbm_enc_sym(sym_id),    // Symbol
    lbm_enc_i(42),          // Integer
    lbm_enc_u(123),         // Unsigned integer
    lbm_enc_char('A')       // Character
  };
  
  for (size_t i = 0; i < sizeof(valid_values)/sizeof(valid_values[0]); i++) {
    // With a proper event handler, these should succeed
    lbm_event_unboxed(valid_values[i]);
    // Give some time for event processing
    sleep_callback(1000);
  }

  kill_eval_after_tests();
  return 1; // Success if no crash
}

// Test lbm_event_unboxed with invalid types
int test_lbm_event_unboxed_invalid_types() {
  if (!start_lispbm_for_tests()) return 0;
  
  if (!lbm_eval_init_events(20)) return 0;
  
  // Test with types that lbm_event_unboxed should reject
  lbm_value invalid_values[] = {
    lbm_enc_float(3.14f),   // Float (boxed type)
    ENC_SYM_NIL,            // Special case - nil
    ENC_SYM_TRUE            // Special case - true
  };
  
  for (size_t i = 0; i < sizeof(invalid_values)/sizeof(invalid_values[0]); i++) {
    // These should return false but not crash
    bool result = lbm_event_unboxed(invalid_values[i]);
    if (result) return 0; // Should return false for invalid types
  }
  
  return 1;
}

// Test lbm_event_unboxed behavior without event handler
int test_lbm_event_unboxed_no_handler() {
  if (!start_lispbm_for_tests()) return 0;
  
  // Don't initialize events - no event handler should be set
  
  lbm_value test_value = lbm_enc_i(42);
  bool result = lbm_event_unboxed(test_value);
  
  // Should return false when no event handler is set
  return (result == false);
}

// Test lbm_event_unboxed with multiple valid calls
int test_lbm_event_unboxed_multiple_calls() {
  if (!start_lispbm_for_tests()) return 0;
  
  if (!lbm_eval_init_events(20)) return 0;
  
  // Create and register an event handler
  char *handler_code = "(let ((running t)) (loopwhile running (recv ((? msg) msg))))";
  lbm_string_channel_state_t st;
  lbm_char_channel_t chan;
  lbm_create_string_char_channel(&st, &chan, handler_code);
  lbm_cid handler_cid = lbm_load_and_eval_expression(&chan);

  if (handler_cid < 0) return 0;
  
  lbm_set_event_handler_pid(handler_cid);
  
  // Verify event handler exists
  if (!lbm_event_handler_exists()) return 0;
  
  // Call multiple times with different values
  for (int i = 0; i < 5; i++) {
    lbm_event_unboxed(lbm_enc_i(i));
    lbm_event_unboxed(lbm_enc_u((unsigned int)i));
    lbm_event_unboxed(lbm_enc_char((char)('A' + i)));
    sleep_callback(100); // Brief pause between events
  }

  kill_eval_after_tests();
  return 1; // Success if no crash
}

// Test lbm_event_queue_is_empty function
int test_lbm_event_queue_is_empty() {
  if (!start_lispbm_for_tests()) return 0;
  
  if (!lbm_eval_init_events(20)) return 0;
  
  // Initially, event queue should be empty
  if (!lbm_event_queue_is_empty()) return 0;
  
  // Create and register an event handler
  char *handler_code = "(let ((running t)) (loopwhile running (recv ((? msg) msg))))";
  lbm_string_channel_state_t st;
  lbm_char_channel_t chan;
  lbm_create_string_char_channel(&st, &chan, handler_code);
  lbm_cid handler_cid = lbm_load_and_eval_expression(&chan);

  if (handler_cid < 0) return 0;
  
  lbm_set_event_handler_pid(handler_cid);
  
  // Verify event handler exists
  if (!lbm_event_handler_exists()) return 0;
  
  // Queue should still be empty before adding events
  if (!lbm_event_queue_is_empty()) return 0;
  
  // Pause the evaluator so events won't be processed
  lbm_pause_eval();
  sleep_callback(5000); // Give time for pause to take effect
  
  // Verify evaluator is paused
  uint32_t state = lbm_get_eval_state();
  if (state != EVAL_CPS_STATE_PAUSED) return 0;
  
  // Add some events to the queue while paused
  lbm_event_unboxed(lbm_enc_i(42));
  lbm_event_unboxed(lbm_enc_u(123));
  lbm_event_unboxed(lbm_enc_char('A'));
  
  // Since evaluator is paused, events should remain in queue
  if (lbm_event_queue_is_empty()) return 0; // Should NOT be empty now
  
  // Continue the evaluator to let events process
  lbm_continue_eval();
  sleep_callback(5000); // Give time for events to process
  
  // After processing, queue should be empty again
  if (!lbm_event_queue_is_empty()) return 0;
  
  kill_eval_after_tests();
  return 1;
}

// Test interaction between reset and surrender quota
int test_reset_and_surrender_interaction() {
  if (!start_lispbm_for_tests()) return 0;
  
  // Reset, then surrender quota while paused
  lbm_reset_eval();
  sleep_callback(5000);
  
  // Should be paused
  uint32_t state = lbm_get_eval_state();
  if (state != EVAL_CPS_STATE_RESET) return 0;
  
  // Surrender quota while paused
  lbm_surrender_quota();
  sleep_callback(1000);
  
  // Should still be paused
  state = lbm_get_eval_state();
  if (state != EVAL_CPS_STATE_RESET) return 0;
  
  // Continue and test surrender quota while running
  lbm_continue_eval();
  sleep_callback(5000);
  lbm_surrender_quota();
  sleep_callback(1000);
  
  return 1;
}

// Test verbose toggle with reset/continue cycle
int test_verbose_with_reset_continue() {
  if (!start_lispbm_for_tests()) return 0;
  
  lbm_toggle_verbose();
  lbm_reset_eval();
  sleep_callback(5000);
  
  // Should be paused
  uint32_t state = lbm_get_eval_state();
  if (state != EVAL_CPS_STATE_RESET) return 0;
  
  lbm_toggle_verbose();
  lbm_continue_eval();
  sleep_callback(5000);
  
  // Should be running
  state = lbm_get_eval_state();
  if (state != EVAL_CPS_STATE_RUNNING) return 0;
  
  lbm_toggle_verbose();
  
  return 1;
}

// Test event queue full condition
int test_lbm_event_queue_full() {
  if (!start_lispbm_for_tests()) return 0;
  
  // Initialize events with a small queue size to make it easier to fill
  if (!lbm_eval_init_events(5)) return 0;
  
  // Pause the evaluator so events won't be processed immediately
  lbm_pause_eval();
  sleep_callback(5000); // Give time for pause to take effect
  
  // Verify evaluator is paused
  uint32_t state = lbm_get_eval_state();
  if (state != EVAL_CPS_STATE_PAUSED) return 0;
  
  // Create and register an event handler (but since evaluator is paused, 
  // it won't process events from the queue)
  char *handler_code = "(let ((running t)) (loopwhile running (recv ((? msg) msg))))";
  lbm_string_channel_state_t st;
  lbm_char_channel_t chan;
  lbm_create_string_char_channel(&st, &chan, handler_code);
  lbm_cid handler_cid = lbm_load_and_eval_expression(&chan);

  if (handler_cid < 0) return 0;
  
  lbm_set_event_handler_pid(handler_cid);
  
  // Verify event handler exists
  if (!lbm_event_handler_exists()) return 0;
  
  // Fill the event queue (size 5) by adding more events than capacity
  bool results[10];
  for (int i = 0; i < 10; i++) {
    results[i] = lbm_event_unboxed(lbm_enc_i(i));
  }
  
  // First 5 events should succeed (fill the queue)
  for (int i = 0; i < 5; i++) {
    if (!results[i]) return 0; // Should succeed
  }
  
  // Remaining events should fail (queue is full)
  for (int i = 5; i < 10; i++) {
    if (results[i]) return 0; // Should fail when queue is full
  }
  
  // Queue should not be empty (it's full)
  if (lbm_event_queue_is_empty()) return 0;
  
  // Continue the evaluator to process events
  lbm_continue_eval();
  sleep_callback(5000); // Give time for events to process
  
  // After processing, queue should be empty again
  if (!lbm_event_queue_is_empty()) return 0;
  
  kill_eval_after_tests();
  return 1;
}

// Test all functions in proper sequence
int test_all_functions_sequence() {
  if (!start_lispbm_for_tests()) return 0;
  
  // Call all functions respecting the reset->paused->continue->running cycle
  lbm_toggle_verbose();
  lbm_surrender_quota();
  
  lbm_reset_eval();
  sleep_callback(5000);
  
  // Should be paused
  uint32_t state = lbm_get_eval_state();
  if (state != EVAL_CPS_STATE_RESET) return 0;
  
  lbm_toggle_verbose();
  lbm_continue_eval();
  sleep_callback(5000);
  
  // Should be running
  state = lbm_get_eval_state();
  if (state != EVAL_CPS_STATE_RUNNING) return 0;
  
  lbm_surrender_quota();
  lbm_toggle_verbose();
  
  return 1;
}

// Test callback setters with NULL - only testing that they don't crash
int test_callback_setters_null() {
  // Test lbm_set_critical_error_callback with NULL
  lbm_set_critical_error_callback(NULL);
  
  // Test lbm_set_usleep_callback with NULL
  lbm_set_usleep_callback(NULL);
  
  // Test lbm_set_timestamp_us_callback with NULL
  lbm_set_timestamp_us_callback(NULL);
  
  // Test lbm_set_ctx_done_callback with NULL
  lbm_set_ctx_done_callback(NULL);
  
  // Test lbm_set_printf_callback with NULL
  lbm_set_printf_callback(NULL);
  
  // Test lbm_set_dynamic_load_callback with NULL
  lbm_set_dynamic_load_callback(NULL);
  
  // If we get here without crashing, the test passes
  return 1;
}

int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  printf("Running eval_cps.c function tests...\n");

  total_tests++; if (test_lbm_reset_eval_enters_paused()) { printf("✓ test_lbm_reset_eval_enters_paused\n"); tests_passed++; } else { printf("✗ test_lbm_reset_eval_enters_paused\n"); }
  total_tests++; if (test_lbm_reset_then_continue()) { printf("✓ test_lbm_reset_then_continue\n"); tests_passed++; } else { printf("✗ test_lbm_reset_then_continue\n"); }
  total_tests++; if (test_lbm_reset_continue_cycles()) { printf("✓ test_lbm_reset_continue_cycles\n"); tests_passed++; } else { printf("✗ test_lbm_reset_continue_cycles\n"); }
  total_tests++; if (test_lbm_toggle_verbose_basic()) { printf("✓ test_lbm_toggle_verbose_basic\n"); tests_passed++; } else { printf("✗ test_lbm_toggle_verbose_basic\n"); }
  total_tests++; if (test_lbm_surrender_quota_basic()) { printf("✓ test_lbm_surrender_quota_basic\n"); tests_passed++; } else { printf("✗ test_lbm_surrender_quota_basic\n"); }
  total_tests++; if (test_lbm_surrender_quota_multiple()) { printf("✓ test_lbm_surrender_quota_multiple\n"); tests_passed++; } else { printf("✗ test_lbm_surrender_quota_multiple\n"); }
  total_tests++; if (test_lbm_eval_init_events()) { printf("✓ test_lbm_eval_init_events\n"); tests_passed++; } else { printf("✗ test_lbm_eval_init_events\n"); }
  total_tests++; if (test_lbm_event_handler_exists()) { printf("✓ test_lbm_event_handler_exists\n"); tests_passed++; } else { printf("✗ test_lbm_event_handler_exists\n"); }
  total_tests++; if (test_lbm_event_define_basic()) { printf("✓ test_lbm_event_define_basic\n"); tests_passed++; } else { printf("✗ test_lbm_event_define_basic\n"); }
  total_tests++; if (test_lbm_event_define_different_keys()) { printf("✓ test_lbm_event_define_different_keys\n"); tests_passed++; } else { printf("✗ test_lbm_event_define_different_keys\n"); }
  total_tests++; if (test_lbm_event_unboxed_valid_types()) { printf("✓ test_lbm_event_unboxed_valid_types\n"); tests_passed++; } else { printf("✗ test_lbm_event_unboxed_valid_types\n"); }
  total_tests++; if (test_lbm_event_unboxed_invalid_types()) { printf("✓ test_lbm_event_unboxed_invalid_types\n"); tests_passed++; } else { printf("✗ test_lbm_event_unboxed_invalid_types\n"); }
  total_tests++; if (test_lbm_event_unboxed_no_handler()) { printf("✓ test_lbm_event_unboxed_no_handler\n"); tests_passed++; } else { printf("✗ test_lbm_event_unboxed_no_handler\n"); }
  total_tests++; if (test_lbm_event_unboxed_multiple_calls()) { printf("✓ test_lbm_event_unboxed_multiple_calls\n"); tests_passed++; } else { printf("✗ test_lbm_event_unboxed_multiple_calls\n"); }
  total_tests++; if (test_lbm_event_queue_is_empty()) { printf("✓ test_lbm_event_queue_is_empty\n"); tests_passed++; } else { printf("✗ test_lbm_event_queue_is_empty\n"); }
  total_tests++; if (test_lbm_event_queue_full()) { printf("✓ test_lbm_event_queue_full\n"); tests_passed++; } else { printf("✗ test_lbm_event_queue_full\n"); }
  total_tests++; if (test_reset_and_surrender_interaction()) { printf("✓ test_reset_and_surrender_interaction\n"); tests_passed++; } else { printf("✗ test_reset_and_surrender_interaction\n"); }
  total_tests++; if (test_verbose_with_reset_continue()) { printf("✓ test_verbose_with_reset_continue\n"); tests_passed++; } else { printf("✗ test_verbose_with_reset_continue\n"); }
  total_tests++; if (test_all_functions_sequence()) { printf("✓ test_all_functions_sequence\n"); tests_passed++; } else { printf("✗ test_all_functions_sequence\n"); }
  total_tests++; if (test_callback_setters_null()) { printf("✓ test_callback_setters_null\n"); tests_passed++; } else { printf("✗ test_callback_setters_null\n"); }

  printf("\n");
  if (tests_passed == total_tests) {
    printf("SUCCESS: All %d tests passed\n", total_tests);
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
