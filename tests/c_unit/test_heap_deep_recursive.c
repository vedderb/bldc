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
#include "heap.h"

#include "init/start_lispbm.c"

static int test_init(void) {
  return start_lispbm_for_tests();
}

int test_deep_recursive_gc_stack_overflow(void) {
  if (!test_init()) return 0;
  
  // The GC stack size is 256 (from init/start_lispbm.c)
  // Create a structure deeper than this to trigger GC stack overflow
  const int depth = 512; // Twice the GC stack size
  
  printf("Creating deeply car-recursive structure with depth %d...\n", depth);
  
  lbm_value result = lbm_enc_i(42); // Base value at the bottom
  
  // Build the structure: (cons (cons (cons ... (cons 42 nil) nil) nil) nil)
  // Each level nests deeper in the car position

  int timeout = 0;
  
  lbm_pause_eval();
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout < 5) {
    sleep_callback(1000);
    timeout++;
  }

  // the cdr also has to be a cons for the stack usage to grow.
  lbm_value cdr_val = lbm_cons(lbm_enc_i(42),lbm_enc_i(33));
  
  for (int i = 0; i < depth; i++) {
    result = lbm_cons(result, cdr_val);
    if (lbm_is_symbol_merror(result)) {
      printf("Memory allocation failed at depth %d\n", i);
      return 0;
    }
  }

  if (!lbm_define("deep", result)) return 0;

  printf("Structure created successfully. Current GC stack size: %u\n", lbm_get_gc_stack_size());
  printf("Current GC stack max usage: %u\n", lbm_get_gc_stack_max());
  
  // Force garbage collection which will attempt to traverse the deep structure
  printf("Triggering garbage collection...\n");
  // lbm_gc_mark_phase(result); // This is illegal and would trigger longjmp undefined behavior.
  lbm_request_gc(); // this is fine, GC called from evaluator thread.
  lbm_continue_eval();

  int thread_r;
  // Test hangs if evaluator thread does not exit. 
  pthread_join(lispbm_thd, (void*)&thread_r); // The evaluator dies but there should be no crash.
    
  printf("Garbage collection completed without crash\n");
  printf("Post-GC stack max usage: %u\n", lbm_get_gc_stack_max());
  
  return 1;
}

int main(void) {
  printf("Testing deep recursive structure that exceeds GC stack size...\n");
  
  if (!test_deep_recursive_gc_stack_overflow()) {
    printf("FAILURE: GC stack overflow test failed\n");
    return 1;
  }
  
  printf("SUCCESS\n");
  return 0;
}
