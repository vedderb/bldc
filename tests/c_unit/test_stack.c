#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "../../include/stack.h"
#include "../../include/lbm_memory.h"
#include "../../include/lbm_types.h"

#define TEST_MEMORY_SIZE 1024
#define TEST_BITMAP_SIZE 64

static lbm_uint test_memory[TEST_MEMORY_SIZE];
static lbm_uint test_bitmap[TEST_BITMAP_SIZE];

int setup_memory() {
  return lbm_memory_init(test_memory, TEST_MEMORY_SIZE, test_bitmap, TEST_BITMAP_SIZE);
}

int test_stack_allocate() {
  if (!setup_memory()) return 0;
  
  lbm_stack_t stack;
  int result = lbm_stack_allocate(&stack, 32);
  
  if (!result) return 0;
  if (!stack.data) return 0;
  if (stack.sp != 0) return 0;
  if (stack.size != 32) return 0;
  
  lbm_stack_free(&stack);
  return 1;
}

int test_stack_create() {
  lbm_uint static_data[16];
  lbm_stack_t stack;
  
  int result = lbm_stack_create(&stack, static_data, 16);
  
  if (!result) return 0;
  if (stack.data != static_data) return 0;
  if (stack.sp != 0) return 0;
  if (stack.size != 16) return 0;
  
  return 1;
}

int test_push_pop_single() {
  lbm_uint static_data[16];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 16);
  
  int push_result = lbm_push(&stack, 0x12345678);
  if (!push_result) return 0;
  if (stack.sp != 1) return 0;
  
  lbm_uint value;
  int pop_result = lbm_pop(&stack, &value);
  if (!pop_result) return 0;
  if (value != 0x12345678) return 0;
  if (stack.sp != 0) return 0;
  
  return 1;
}

int test_push_pop_multiple() {
  lbm_uint static_data[16];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 16);
  
  lbm_uint test_values[] = {0x11111111, 0x22222222, 0x33333333, 0x44444444};
  int num_values = sizeof(test_values) / sizeof(test_values[0]);
  
  for (int i = 0; i < num_values; i++) {
    if (!lbm_push(&stack, test_values[i])) return 0;
    if (stack.sp != (lbm_uint)(i + 1)) return 0;
  }
  
  for (int i = num_values - 1; i >= 0; i--) {
    lbm_uint value;
    if (!lbm_pop(&stack, &value)) return 0;
    if (value != test_values[i]) return 0;
    if (stack.sp != (lbm_uint)i) return 0;
  }
  
  return 1;
}

int test_stack_overflow() {
  lbm_uint static_data[4];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 4);
  
  for (int i = 0; i < 4; i++) {
    if (!lbm_push(&stack, (lbm_uint)i)) return 0;
  }
  
  int overflow_result = lbm_push(&stack, 0xDEADBEEF);
  if (overflow_result != 0) return 0;
  if (stack.sp != 4) return 0;
  
  return 1;
}

int test_pop_2() {
  lbm_uint static_data[16];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 16);
  
  lbm_push(&stack, 0x11111111);
  lbm_push(&stack, 0x22222222);
  
  lbm_uint r0, r1;
  int result = lbm_pop_2(&stack, &r0, &r1);
  
  if (!result) return 0;
  if (r0 != 0x22222222) return 0;
  if (r1 != 0x11111111) return 0;
  if (stack.sp != 0) return 0;
  
  return 1;
}

int test_pop_3() {
  lbm_uint static_data[16];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 16);
  
  lbm_push(&stack, 0x11111111);
  lbm_push(&stack, 0x22222222);
  lbm_push(&stack, 0x33333333);
  
  lbm_uint r0, r1, r2;
  int result = lbm_pop_3(&stack, &r0, &r1, &r2);
  
  if (!result) return 0;
  if (r0 != 0x33333333) return 0;
  if (r1 != 0x22222222) return 0;
  if (r2 != 0x11111111) return 0;
  if (stack.sp != 0) return 0;
  
  return 1;
}

int test_stack_clear() {
  lbm_uint static_data[16];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 16);
  
  lbm_push(&stack, 0x11111111);
  lbm_push(&stack, 0x22222222);
  lbm_push(&stack, 0x33333333);
  
  if (stack.sp != 3) return 0;
  
  lbm_stack_clear(&stack);
  
  if (stack.sp != 0) return 0;
  
  return 1;
}

int test_stack_drop() {
  lbm_uint static_data[16];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 16);
  
  for (int i = 0; i < 5; i++) {
    lbm_push(&stack, (lbm_uint)i);
  }
  
  int result1 = lbm_stack_drop(&stack, 2);
  if (!result1) return 0;
  if (stack.sp != 3) return 0;
  
  int result2 = lbm_stack_drop(&stack, 3);
  if (!result2) return 0;
  if (stack.sp != 0) return 0;
  
  int result3 = lbm_stack_drop(&stack, 1);
  if (result3 != 0) return 0;
  if (stack.sp != 0) return 0;
  
  return 1;
}

int test_stack_is_empty() {
  lbm_uint static_data[16];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 16);
  
  if (!lbm_stack_is_empty(&stack)) return 0;
  
  lbm_push(&stack, 0x12345678);
  if (lbm_stack_is_empty(&stack)) return 0;
  
  lbm_uint value;
  lbm_pop(&stack, &value);
  if (!lbm_stack_is_empty(&stack)) return 0;
  
  return 1;
}

int test_get_max_stack() {
  lbm_uint static_data[16];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 16);
  
  lbm_uint max_sp = lbm_get_max_stack(&stack);
  if (max_sp != 0) return 0;
  
  lbm_push(&stack, 0x11111111);
  lbm_push(&stack, 0x22222222);
  lbm_push(&stack, 0x33333333);
  
  max_sp = lbm_get_max_stack(&stack);
  if (max_sp != 3) return 0;
  
  lbm_uint value;
  lbm_pop(&stack, &value);
  
  max_sp = lbm_get_max_stack(&stack);
  if (max_sp != 3) return 0;
  
  return 1;
}

int test_stack_allocate_failure() {
  lbm_stack_t stack;
  int result = lbm_stack_allocate(&stack, 100000);
  
  if (result != 0) return 0;
  
  return 1;
}

int test_stack_free() {
  if (!setup_memory()) return 0;
  
  lbm_stack_t stack;
  int alloc_result = lbm_stack_allocate(&stack, 32);
  if (!alloc_result) return 0;
  
  
  lbm_stack_free(&stack);
  
  lbm_stack_t stack2;
  int alloc_result2 = lbm_stack_allocate(&stack2, 32);
  if (!alloc_result2) return 0;
  
  lbm_stack_free(&stack2);
  return 1;
}

int test_stack_free_2() { 
  lbm_stack_t stack;
  stack.data = NULL;
  
  lbm_stack_free(&stack);
  return 1;
}

int test_edge_cases() {
  lbm_uint static_data[1];
  lbm_stack_t stack;
  lbm_stack_create(&stack, static_data, 1);
  
  if (!lbm_push(&stack, 0x12345678)) return 0;
  if (lbm_push(&stack, 0x87654321)) return 0;
  
  lbm_uint value;
  if (!lbm_pop(&stack, &value)) return 0;
  if (value != 0x12345678) return 0;
  
  if (stack.sp != 0) return 0;
  
  return 1;
}

int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  total_tests++; if (test_stack_allocate()) tests_passed++;
  total_tests++; if (test_stack_create()) tests_passed++;
  total_tests++; if (test_push_pop_single()) tests_passed++;
  total_tests++; if (test_push_pop_multiple()) tests_passed++;
  total_tests++; if (test_stack_overflow()) tests_passed++;
  total_tests++; if (test_pop_2()) tests_passed++;
  total_tests++; if (test_pop_3()) tests_passed++;
  total_tests++; if (test_stack_clear()) tests_passed++;
  total_tests++; if (test_stack_drop()) tests_passed++;
  total_tests++; if (test_stack_is_empty()) tests_passed++;
  total_tests++; if (test_get_max_stack()) tests_passed++;
  total_tests++; if (test_stack_allocate_failure()) tests_passed++;
  total_tests++; if (test_stack_free()) tests_passed++;
  total_tests++; if (test_stack_free_2()) tests_passed++;
  total_tests++; if (test_edge_cases()) tests_passed++;

  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
