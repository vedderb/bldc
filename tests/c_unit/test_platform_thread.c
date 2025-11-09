/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

#include "lispbm.h"
#include "platform_thread.h"

// Memory for lbm_memory system
static lbm_uint memory[LBM_MEMORY_SIZE_8K];
static lbm_uint bitmap[LBM_MEMORY_BITMAP_SIZE_8K];

static bool memory_initialized = false;

static void init_memory(void) {
  if (!memory_initialized) {
    lbm_memory_init(memory, LBM_MEMORY_SIZE_8K,
                    bitmap, LBM_MEMORY_BITMAP_SIZE_8K);
    memory_initialized = true;
  }
}

// Test data structures
typedef struct {
  int *counter;
  int increment;
} thread_data_t;

// Simple thread function that increments a counter
void counter_thread(void *arg) {
  thread_data_t *data = (thread_data_t *)arg;
  for (int i = 0; i < data->increment; i++) {
    (*data->counter)++;
  }
}

// Thread function that sleeps
void sleep_thread(void *arg) {
  uint32_t *sleep_us = (uint32_t *)arg;
  lbm_thread_sleep_us(*sleep_us);
}

// Thread function that yields
void yield_thread(void *arg) {
  int *count = (int *)arg;
  for (int i = 0; i < *count; i++) {
    lbm_thread_yield();
  }
}

// Test 1: Basic thread creation and execution
int test_thread_create_execute(void) {
  int counter = 0;
  thread_data_t data = {
    .counter = &counter,
    .increment = 100
  };

  lbm_thread_t thread = {0};
  if (!lbm_thread_create(&thread, "test_thread", counter_thread, &data,
                         LBM_THREAD_PRIO_NORMAL, 0)) {
    return 0;
  }

  lbm_thread_destroy(&thread);

  if (counter != 100) {
    printf("Expected counter = 100, got %d\n", counter);
    return 0;
  }

  return 1;
}

// Test 2: Thread parameter passing
int test_thread_parameter_passing(void) {
  int counter1 = 0;
  int counter2 = 0;

  thread_data_t data1 = {.counter = &counter1, .increment = 50};
  thread_data_t data2 = {.counter = &counter2, .increment = 75};

  lbm_thread_t thread1 = {0};
  lbm_thread_t thread2 = {0};

  if (!lbm_thread_create(&thread1, "thread1", counter_thread, &data1,
                         LBM_THREAD_PRIO_NORMAL, 0)) {
    return 0;
  }

  if (!lbm_thread_create(&thread2, "thread2", counter_thread, &data2,
                         LBM_THREAD_PRIO_NORMAL, 0)) {
    lbm_thread_destroy(&thread1);
    return 0;
  }

  lbm_thread_destroy(&thread1);
  lbm_thread_destroy(&thread2);

  if (counter1 != 50 || counter2 != 75) {
    printf("Expected counter1=50, counter2=75, got %d, %d\n", counter1, counter2);
    return 0;
  }

  return 1;
}

// Test 3: Thread sleep functionality
int test_thread_sleep(void) {
  uint32_t sleep_duration = 10000; // 10ms

  struct timespec start, end;
  clock_gettime(CLOCK_MONOTONIC, &start);

  lbm_thread_t thread = {0};
  if (!lbm_thread_create(&thread, "sleep_test", sleep_thread, &sleep_duration,
                         LBM_THREAD_PRIO_NORMAL, 0)) {
    return 0;
  }

  lbm_thread_destroy(&thread);

  clock_gettime(CLOCK_MONOTONIC, &end);

  long elapsed_us = (end.tv_sec - start.tv_sec) * 1000000L +
                    (end.tv_nsec - start.tv_nsec) / 1000L;

  // Check that we slept at least 10ms (allow some overhead)
  if (elapsed_us < 10000) {
    printf("Sleep was too short: %ld us\n", elapsed_us);
    return 0;
  }

  return 1;
}

// Test 4: Thread yield functionality
int test_thread_yield(void) {
  int yield_count = 100;

  lbm_thread_t thread = {0};
  if (!lbm_thread_create(&thread, "yield_test", yield_thread, &yield_count,
                         LBM_THREAD_PRIO_NORMAL, 0)) {
    return 0;
  }

  lbm_thread_destroy(&thread);

  return 1;
}

// Test 5: Thread cleanup with zero-initialized thread
int test_thread_null_handling(void) {
  lbm_thread_t thread = {0};

  // Destroying zero-initialized thread should not crash
  lbm_thread_destroy(&thread);

  return 1;
}

// Test 6: Thread with different priorities
int test_thread_priorities(void) {
  int counter = 0;
  thread_data_t data = {.counter = &counter, .increment = 10};

  lbm_thread_prio_t priorities[] = {
    LBM_THREAD_PRIO_LOW,
    LBM_THREAD_PRIO_NORMAL,
    LBM_THREAD_PRIO_HIGH
    // Note: LBM_THREAD_PRIO_REALTIME may require elevated privileges
  };

  for (size_t i = 0; i < sizeof(priorities) / sizeof(priorities[0]); i++) {
    counter = 0;

    lbm_thread_t thread = {0};
    if (!lbm_thread_create(&thread, "prio_test", counter_thread, &data,
                           priorities[i], 0)) {
      printf("Failed to create thread with priority %d\n", (int)priorities[i]);
      return 0;
    }

    lbm_thread_destroy(&thread);

    if (counter != 10) {
      printf("Thread with priority %d failed: counter=%d\n",
             (int)priorities[i], counter);
      return 0;
    }
  }

  return 1;
}

int main(void) {
  int tests_passed = 0;
  int tests_total = 6;

  init_memory();

  printf("Running platform_thread tests...\n");

  if (test_thread_create_execute()) {
    printf("Test 1 passed: thread_create_execute\n");
    tests_passed++;
  } else {
    printf("Test 1 FAILED: thread_create_execute\n");
  }

  if (test_thread_parameter_passing()) {
    printf("Test 2 passed: thread_parameter_passing\n");
    tests_passed++;
  } else {
    printf("Test 2 FAILED: thread_parameter_passing\n");
  }

  if (test_thread_sleep()) {
    printf("Test 3 passed: thread_sleep\n");
    tests_passed++;
  } else {
    printf("Test 3 FAILED: thread_sleep\n");
  }

  if (test_thread_yield()) {
    printf("Test 4 passed: thread_yield\n");
    tests_passed++;
  } else {
    printf("Test 4 FAILED: thread_yield\n");
  }

  if (test_thread_null_handling()) {
    printf("Test 5 passed: thread_null_handling\n");
    tests_passed++;
  } else {
    printf("Test 5 FAILED: thread_null_handling\n");
  }

  if (test_thread_priorities()) {
    printf("Test 6 passed: thread_priorities\n");
    tests_passed++;
  } else {
    printf("Test 6 FAILED: thread_priorities\n");
  }

  printf("\n%d/%d tests passed\n", tests_passed, tests_total);

  if (tests_passed == tests_total) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILURE\n");
    return 1;
  }
}
