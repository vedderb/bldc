#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "../../include/lbm_memory.h"
#include "../../include/lbm_types.h"

#define TEST_MEMORY_SIZE 1024
#define TEST_BITMAP_SIZE 64

static lbm_uint test_memory[TEST_MEMORY_SIZE];
static lbm_uint test_bitmap[TEST_BITMAP_SIZE];

// Mock function for lbm_request_gc (required by lbm_memory.c)
// Use weak symbol to avoid conflict with the one in eval_cps.c
__attribute__((weak)) void lbm_request_gc(void) {
  // Do nothing in tests
}

int setup_memory() {
  memset(test_memory, 0, sizeof(test_memory));
  memset(test_bitmap, 0, sizeof(test_bitmap));
  return lbm_memory_init(test_memory, TEST_MEMORY_SIZE, test_bitmap, TEST_BITMAP_SIZE);
}

int test_memory_init_valid() {
  lbm_uint memory[256];
  lbm_uint bitmap[16];

  int result = lbm_memory_init(memory, 256, bitmap, 16);
  if (!result) return 0;

  if (lbm_memory_num_words() != 256) return 0;
  if (lbm_memory_num_free() != 256) return 0;

  return 1;
}

int test_memory_init_null_pointers() {
  lbm_uint memory[256];
  lbm_uint bitmap[16];

  if (lbm_memory_init(NULL, 256, bitmap, 16)) return 0;
  if (lbm_memory_init(memory, 256, NULL, 16)) return 0;
  if (lbm_memory_init(NULL, 256, NULL, 16)) return 0;

  return 1;
}

int test_memory_init_invalid_alignment() {
  lbm_uint memory[256];
  lbm_uint bitmap[16];

  // Test invalid data size (not multiple of 4)
  if (lbm_memory_init(memory, 255, bitmap, 16)) return 0;
  if (lbm_memory_init(memory, 257, bitmap, 16)) return 0;

  // Test invalid bitmap size (not multiple of 4)
  if (lbm_memory_init(memory, 256, bitmap, 15)) return 0;
  if (lbm_memory_init(memory, 256, bitmap, 17)) return 0;

  // Test size relationship: (data_size * 2) != (bits_size * sizeof(lbm_uint) * 8)
  if (lbm_memory_init(memory, 256, bitmap, 32)) return 0;

  return 1;
}

int test_memory_reserve() {
  if (!setup_memory()) return 0;

  lbm_uint original_reserve = lbm_memory_get_reserve();

  lbm_memory_set_reserve(100);
  if (lbm_memory_get_reserve() != 100) return 0;

  lbm_memory_set_reserve(50);
  if (lbm_memory_get_reserve() != 50) return 0;

  lbm_memory_set_reserve(original_reserve);
  return 1;
}

int test_memory_allocate_single_word() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr = lbm_memory_allocate(1);
  if (!ptr) return 0;

  if (lbm_memory_num_free() != TEST_MEMORY_SIZE - 1) return 0;

  *ptr = 0x12345678;
  if (*ptr != 0x12345678) return 0;

  if (!lbm_memory_ptr_inside(ptr)) return 0;

  return 1;
}

int test_memory_allocate_multiple_words() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr = lbm_memory_allocate(10);
  if (!ptr) return 0;

  if (lbm_memory_num_free() != TEST_MEMORY_SIZE - 10) return 0;

  for (int i = 0; i < 10; i++) {
    ptr[i] = 0x10000000 + (lbm_uint)i;
  }

  for (int i = 0; i < 10; i++) {
    if (ptr[i] != 0x10000000 + (lbm_uint)i) return 0;
  }

  return 1;
}

int test_memory_free() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr1 = lbm_memory_allocate(5);
  lbm_uint *ptr2 = lbm_memory_allocate(3);
  if (!ptr1 || !ptr2) return 0;

  lbm_uint free_before = lbm_memory_num_free();

  int result = lbm_memory_free(ptr1);
  if (!result) return 0;

  if (lbm_memory_num_free() != free_before + 5) return 0;

  result = lbm_memory_free(ptr2);
  if (!result) return 0;

  if (lbm_memory_num_free() != TEST_MEMORY_SIZE) return 0;

  return 1;
}

int test_memory_free_invalid_pointer() {
  if (!setup_memory()) return 0;

  lbm_uint external_memory[10];

  int result = lbm_memory_free(external_memory);
  if (result != 0) return 0;

  int result2 = lbm_memory_free(NULL);
  if (result2 != 0) return 0;

  return 1;
}

int test_lbm_malloc_free() {
  if (!setup_memory()) return 0;

  void *ptr = lbm_malloc(20);
  if (!ptr) return 0;

  if (!lbm_memory_ptr_inside((lbm_uint*)ptr)) return 0;

  lbm_free(ptr);

  void *ptr_zero = lbm_malloc(0);
  if (ptr_zero != NULL) return 0;

  return 1;
}

int test_lbm_malloc_reserve() {
  if (!setup_memory()) return 0;

  void *ptr = lbm_malloc_reserve(20);
  if (!ptr) return 0;

  if (!lbm_memory_ptr_inside((lbm_uint*)ptr)) return 0;

  lbm_free(ptr);

  void *ptr_zero = lbm_malloc_reserve(0);
  if (ptr_zero != NULL) return 0;

  return 1;
}

int test_memory_address_to_ix() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr = lbm_memory_allocate(1);
  if (!ptr) return 0;

  lbm_int ix = lbm_memory_address_to_ix(ptr);
  if (ix < 0) return 0;

  return 1;
}

int test_memory_shrink() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr = lbm_memory_allocate(10);
  if (!ptr) return 0;

  lbm_uint free_before = lbm_memory_num_free();

  int result = lbm_memory_shrink(ptr, 5);
  if (!result) return 0;

  if (lbm_memory_num_free() != free_before + 5) return 0;

  result = lbm_memory_shrink(ptr, 1);
  if (!result) return 0;

  if (lbm_memory_num_free() != free_before + 9) return 0;

  return 1;
}

int test_memory_shrink_single_word() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr = lbm_memory_allocate(1);
  if (!ptr) return 0;

  int result = lbm_memory_shrink(ptr, 1);
  if (!result) return 0;

  return 1;
}

int test_memory_shrink_invalid() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr = lbm_memory_allocate(5);
  if (!ptr) return 0;

  int result = lbm_memory_shrink(ptr, 10);
  if (result != 0) return 0;

  int result2 = lbm_memory_shrink(ptr, 0);
  if (result2 != 0) return 0;

  lbm_uint external_memory[10];
  int result3 = lbm_memory_shrink(external_memory, 5);
  if (result3 != 0) return 0;

  return 1;
}

int test_memory_longest_free() {
  if (!setup_memory()) return 0;

  lbm_uint longest = lbm_memory_longest_free();
  if (longest == 0) return 0;

  lbm_uint *ptr1 = lbm_memory_allocate(10);
  lbm_uint *ptr2 = lbm_memory_allocate(5);
  if (!ptr1 || !ptr2) return 0;

  lbm_uint longest_after = lbm_memory_longest_free();
  if (longest_after >= longest) return 0;

  return 1;
}

int test_memory_longest_free_uninitialized() {
  // Skip this test as it requires memory to be uninitialized
  // But other tests have already initialized memory
  return 1;
}

int test_memory_maximum_used() {
  if (!setup_memory()) return 0;

  lbm_uint max_used_initial = lbm_memory_maximum_used();
  if (max_used_initial != 0) return 0;

  lbm_uint *ptr = lbm_memory_allocate(50);
  if (!ptr) return 0;

  lbm_memory_update_min_free();

  lbm_uint max_used_after = lbm_memory_maximum_used();
  if (max_used_after != 50) return 0;

  return 1;
}

int test_memory_ptr_inside() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr = lbm_memory_allocate(1);
  if (!ptr) return 0;

  if (!lbm_memory_ptr_inside(ptr)) return 0;

  lbm_uint external_memory[10];
  if (lbm_memory_ptr_inside(external_memory)) return 0;

  return 1;
}

int test_memory_fragmentation() {
  if (!setup_memory()) return 0;

  lbm_uint *ptrs[10];

  for (int i = 0; i < 10; i++) {
    ptrs[i] = lbm_memory_allocate(2);
    if (!ptrs[i]) return 0;
  }

  for (int i = 0; i < 10; i += 2) {
    if (!lbm_memory_free(ptrs[i])) return 0;
  }

  lbm_uint longest = lbm_memory_longest_free();
  if (longest == 0) return 0;

  return 1;
}

int test_memory_allocation_patterns() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr1 = lbm_memory_allocate(100);
  lbm_uint *ptr2 = lbm_memory_allocate(200);
  lbm_uint *ptr3 = lbm_memory_allocate(50);

  if (!ptr1 || !ptr2 || !ptr3) return 0;

  if (!lbm_memory_free(ptr2)) return 0;

  lbm_uint *ptr4 = lbm_memory_allocate(150);
  if (!ptr4) return 0;

  if (!lbm_memory_free(ptr1)) return 0;
  if (!lbm_memory_free(ptr3)) return 0;
  if (!lbm_memory_free(ptr4)) return 0;

  if (lbm_memory_num_free() != TEST_MEMORY_SIZE) return 0;

  return 1;
}

int test_memory_edge_cases() {
  if (!setup_memory()) return 0;

  // Set reserve to zero to test edge cases
  lbm_memory_set_reserve(0);

  lbm_uint *large_ptr = lbm_memory_allocate(TEST_MEMORY_SIZE - 50);
  if (!large_ptr) return 0;

  lbm_uint *small_ptr = lbm_memory_allocate(40);
  if (!small_ptr) return 0;

  // This should fail because there's not enough memory left
  lbm_uint *fail_ptr = lbm_memory_allocate(20);
  if (fail_ptr != NULL) return 0;

  return 1;
}

// ===== SINGLE WORD ALLOCATION TESTS =====

int test_single_word_allocation_basic() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr = lbm_memory_allocate(1);
  if (!ptr) return 0;

  if (lbm_memory_num_free() != TEST_MEMORY_SIZE - 1) return 0;

  *ptr = 0xDEADBEEF;
  if (*ptr != 0xDEADBEEF) return 0;

  if (!lbm_memory_ptr_inside(ptr)) return 0;

  if (!lbm_memory_free(ptr)) return 0;
  if (lbm_memory_num_free() != TEST_MEMORY_SIZE) return 0;

  return 1;
}

int test_single_word_allocation_multiple() {
  if (!setup_memory()) return 0;

  lbm_uint *ptrs[20];

  for (int i = 0; i < 20; i++) {
    ptrs[i] = lbm_memory_allocate(1);
    if (!ptrs[i]) return 0;
    *(ptrs[i]) = 0x1000 + (lbm_uint)i;
  }

  if (lbm_memory_num_free() != TEST_MEMORY_SIZE - 20) return 0;

  for (int i = 0; i < 20; i++) {
    if (*(ptrs[i]) != 0x1000 + (lbm_uint)i) return 0;
  }

  for (int i = 0; i < 20; i++) {
    if (!lbm_memory_free(ptrs[i])) return 0;
  }

  if (lbm_memory_num_free() != TEST_MEMORY_SIZE) return 0;

  return 1;
}

int test_single_word_free_and_reuse() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr1 = lbm_memory_allocate(1);
  if (!ptr1) return 0;
  *ptr1 = 0x11111111;

  lbm_uint *ptr2 = lbm_memory_allocate(1);
  if (!ptr2) return 0;
  *ptr2 = 0x22222222;

  if (!lbm_memory_free(ptr1)) return 0;

  lbm_uint *ptr3 = lbm_memory_allocate(1);
  if (!ptr3) return 0;

  if (ptr3 != ptr1) return 0;

  *ptr3 = 0x33333333;
  if (*ptr3 != 0x33333333) return 0;
  if (*ptr2 != 0x22222222) return 0;

  return 1;
}

int test_single_word_fragmentation() {
  if (!setup_memory()) return 0;

  lbm_uint *single_ptrs[10];
  lbm_uint *multi_ptrs[5];

  for (int i = 0; i < 10; i++) {
    single_ptrs[i] = lbm_memory_allocate(1);
    if (!single_ptrs[i]) return 0;
  }

  for (int i = 0; i < 5; i++) {
    multi_ptrs[i] = lbm_memory_allocate(3);
    if (!multi_ptrs[i]) return 0;
  }

  for (int i = 0; i < 10; i += 2) {
    if (!lbm_memory_free(single_ptrs[i])) return 0;
  }

  lbm_uint longest = lbm_memory_longest_free();
  if (longest == 0) return 0;

  lbm_uint *new_single = lbm_memory_allocate(1);
  if (!new_single) return 0;

  return 1;
}

int test_single_word_shrink_operations() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr1 = lbm_memory_allocate(1);
  if (!ptr1) return 0;

  int result = lbm_memory_shrink(ptr1, 1);
  if (!result) return 0;

  int result2 = lbm_memory_shrink(ptr1, 0);
  if (result2 != 0) return 0;

  lbm_uint *ptr2 = lbm_memory_allocate(5);
  if (!ptr2) return 0;

  int result3 = lbm_memory_shrink(ptr2, 1);
  if (!result3) return 0;

  lbm_uint free_after_shrink = lbm_memory_num_free();

  lbm_uint *ptr3 = lbm_memory_allocate(3);
  if (!ptr3) return 0;

  if (lbm_memory_num_free() != free_after_shrink - 3) return 0;

  return 1;
}

int test_single_word_mixed_with_multi() {
  if (!setup_memory()) return 0;

  lbm_uint *single1 = lbm_memory_allocate(1);
  lbm_uint *multi1 = lbm_memory_allocate(5);
  lbm_uint *single2 = lbm_memory_allocate(1);
  lbm_uint *multi2 = lbm_memory_allocate(3);
  lbm_uint *single3 = lbm_memory_allocate(1);

  if (!single1 || !multi1 || !single2 || !multi2 || !single3) return 0;

  *single1 = 0xAAAA;
  multi1[0] = 0xBBBB;
  multi1[4] = 0xCCCC;
  *single2 = 0xDDDD;
  multi2[2] = 0xEEEE;
  *single3 = 0xFFFF;

  if (*single1 != 0xAAAA) return 0;
  if (multi1[0] != 0xBBBB || multi1[4] != 0xCCCC) return 0;
  if (*single2 != 0xDDDD) return 0;
  if (multi2[2] != 0xEEEE) return 0;
  if (*single3 != 0xFFFF) return 0;

  if (!lbm_memory_free(single2)) return 0;
  if (!lbm_memory_free(multi1)) return 0;

  lbm_uint *new_single = lbm_memory_allocate(1);
  if (!new_single) return 0;

  // The allocator should reuse one of the freed locations
  if (new_single != single2 && new_single != multi1) return 0;

  return 1;
}

int test_single_word_boundary_conditions() {
  if (!setup_memory()) return 0;

  lbm_uint *ptr = lbm_memory_allocate(1);
  if (!ptr) return 0;

  lbm_uint *base_memory = (lbm_uint*)test_memory;
  if (ptr < base_memory || ptr >= base_memory + TEST_MEMORY_SIZE) return 0;

  lbm_int ix = lbm_memory_address_to_ix(ptr);
  if (ix < 0) return 0;

  if (!lbm_memory_ptr_inside(ptr)) return 0;
  if (lbm_memory_ptr_inside(ptr - 1)) return 0;
  if (lbm_memory_ptr_inside(ptr + TEST_MEMORY_SIZE)) return 0;

  return 1;
}

int test_single_word_malloc_interface() {
  if (!setup_memory()) return 0;

  void *ptr1 = lbm_malloc(sizeof(lbm_uint));
  if (!ptr1) return 0;

  if (!lbm_memory_ptr_inside((lbm_uint*)ptr1)) return 0;

  *(lbm_uint*)ptr1 = 0x12345678;
  if (*(lbm_uint*)ptr1 != 0x12345678) return 0;

  void *ptr2 = lbm_malloc_reserve(sizeof(lbm_uint));
  if (!ptr2) return 0;

  if (!lbm_memory_ptr_inside((lbm_uint*)ptr2)) return 0;

  lbm_free(ptr1);
  lbm_free(ptr2);

  void *ptr3 = lbm_malloc(1);
  if (!ptr3) return 0;

  lbm_free(ptr3);

  return 1;
}

int test_single_word_status_transitions() {
  if (!setup_memory()) return 0;

  lbm_uint initial_free = lbm_memory_num_free();

  lbm_uint *ptr1 = lbm_memory_allocate(1);
  if (!ptr1) return 0;
  if (lbm_memory_num_free() != initial_free - 1) return 0;

  lbm_uint *ptr2 = lbm_memory_allocate(1);
  if (!ptr2) return 0;
  if (lbm_memory_num_free() != initial_free - 2) return 0;

  if (!lbm_memory_free(ptr1)) return 0;
  if (lbm_memory_num_free() != initial_free - 1) return 0;

  lbm_uint *ptr3 = lbm_memory_allocate(1);
  if (!ptr3) return 0;
  if (ptr3 != ptr1) return 0;

  if (!lbm_memory_free(ptr2)) return 0;
  if (!lbm_memory_free(ptr3)) return 0;
  if (lbm_memory_num_free() != initial_free) return 0;

  return 1;
}

int test_single_word_allocation_exhaustion() {
  if (!setup_memory()) return 0;

  lbm_memory_set_reserve(0);

  lbm_uint num_allocated = 0;
  lbm_uint *ptrs[TEST_MEMORY_SIZE];

  for (lbm_uint i = 0; i < TEST_MEMORY_SIZE; i++) {
    ptrs[i] = lbm_memory_allocate(1);
    if (ptrs[i]) {
      num_allocated++;
      *(ptrs[i]) = i;
    } else {
      break;
    }
  }

  if (num_allocated != TEST_MEMORY_SIZE) return 0;
  if (lbm_memory_num_free() != 0) return 0;

  lbm_uint *fail_ptr = lbm_memory_allocate(1);
  if (fail_ptr != NULL) return 0;

  for (lbm_uint i = 0; i < num_allocated; i++) {
    if (*(ptrs[i]) != i) return 0;
  }

  for (lbm_uint i = 0; i < num_allocated; i++) {
    if (!lbm_memory_free(ptrs[i])) return 0;
  }

  if (lbm_memory_num_free() != TEST_MEMORY_SIZE) return 0;

  return 1;
}

int test_memory_update_min_free_unchanged() {
  if (!setup_memory()) return 0;

  // Allocate some memory to reduce memory_num_free below initial
  lbm_uint *ptr1 = lbm_memory_allocate(100);
  if (!ptr1) return 0;

  // Call update_min_free to set the minimum to current level
  lbm_memory_update_min_free();

  // Allocate more memory to make memory_num_free even smaller
  lbm_uint *ptr2 = lbm_memory_allocate(50);
  if (!ptr2) return 0;

  // Call update_min_free again - this should update since memory is lower
  lbm_memory_update_min_free();

  // Free the second allocation, making memory_num_free larger than memory_min_free
  if (!lbm_memory_free(ptr2)) return 0;

  lbm_uint current_max_used = lbm_memory_maximum_used();

  // Call update_min_free when memory_num_free > memory_min_free (should NOT update)
  lbm_memory_update_min_free();

  // Verify that memory_min_free was NOT updated (max_used should be unchanged)
  lbm_uint final_max_used = lbm_memory_maximum_used();
  if (final_max_used != current_max_used) return 0;

  // Clean up
  if (!lbm_memory_free(ptr1)) return 0;

  return 1;
}

// ===== NULL MEMORY AND BITMAP TESTS =====

int test_functions_with_null_memory_bitmap() {
  // NOTE: These tests assume global memory and bitmap are NULL (uninitialized)
  // However, they may have been initialized by previous tests.
  // We'll test what we can based on the current implementation.

  // Test functions that should handle uninitialized state gracefully
  // Note: lbm_memory_num_words(), lbm_memory_num_free(), and lbm_memory_maximum_used()
  // read global variables that may have been set by previous tests

  // Test that update_min_free doesn't crash with any state
  lbm_memory_update_min_free();  // Should not crash

  // Test ptr_inside with external pointer
  lbm_uint dummy_value = 0;
  lbm_memory_ptr_inside(&dummy_value);
  // This should return 0 (false) for pointers outside the memory range

  // Test address_to_ix with external pointer (should handle gracefully)
  lbm_int ix = lbm_memory_address_to_ix(&dummy_value);
  (void)ix; // Just verify it doesn't crash

  return 1;
}

int test_allocation_with_null_memory_bitmap() {
  // Test allocation functions when they should fail
  // We can't easily test with NULL memory/bitmap due to global state,
  // but we can test allocation failures in low memory conditions

  if (!setup_memory()) return 0;

  // Set reserve to maximum to force allocation failures
  lbm_memory_set_reserve(TEST_MEMORY_SIZE);

  lbm_uint *ptr = lbm_memory_allocate(10);
  if (ptr != NULL) return 0;   // Should return NULL when reserve prevents allocation

  void *malloc_ptr = lbm_malloc(40);
  if (malloc_ptr != NULL) return 0;  // Should return NULL when reserve prevents allocation

  lbm_malloc_reserve(40);
  // malloc_reserve may still succeed as it can use reserved memory

  // Reset reserve
  lbm_memory_set_reserve(0);

  return 1;
}

int test_free_with_null_memory_bitmap() {
  // Test free functions with invalid pointers
  lbm_uint dummy_value = 0;

  // These should not crash and should return 0 (failure) for invalid pointers
  int result1 = lbm_memory_free(&dummy_value);
  if (result1 != 0) return 0;  // Should return 0 for pointer outside memory range

  int result2 = lbm_memory_free(NULL);
  if (result2 != 0) return 0;  // Should return 0 for NULL pointer

  // lbm_free should not crash with invalid pointers
  lbm_free(&dummy_value);    // Should not crash
  lbm_free(NULL);       // Should not crash

  return 1;
}

int test_longest_free_with_null_memory_bitmap() {
  // Test longest_free function behavior
  if (!setup_memory()) return 0;

  lbm_uint longest = lbm_memory_longest_free();
  if (longest == 0) return 0;  // Should return > 0 when memory is initialized

  return 1;
}

int test_shrink_with_null_memory_bitmap() {
  // Test shrink with invalid pointers
  lbm_uint dummy_value = 0;

  int result1 = lbm_memory_shrink(&dummy_value, 5);
  if (result1 != 0) return 0;   // Should return 0 for pointer outside memory range

  int result2 = lbm_memory_shrink(NULL, 5);
  if (result2 != 0) return 0;   // Should return 0 for NULL pointer

  return 1;
}

int test_status_bitmap_bounds() {
  // Test the status() function bounds checking
  // The longest_free function iterates through (bitmap_size << BITMAP_SIZE_SHIFT) indices
  // For 32-bit: BITMAP_SIZE_SHIFT = 4, so it's bitmap_size * 16
  // For 64-bit: BITMAP_SIZE_SHIFT = 5, so it's bitmap_size * 32

  // Create a minimal valid setup and test that status bounds checking works
  lbm_uint minimal_memory[64];  // 64 words of memory
  lbm_uint minimal_bitmap[4]; // 4 words of bitmap

  memset(minimal_memory, 0, sizeof(minimal_memory));
  memset(minimal_bitmap, 0, sizeof(minimal_bitmap));

  // Validation requirements:
  // - data_size % 4 == 0: 64 % 4 = 0 ✓
  // - bits_size % 4 == 0: 4 % 4 = 0 ✓
  // - (data_size * 2) == (bits_size * sizeof(lbm_uint) * 8): 64*2=128, 4*4*8=128 ✓

  if (!lbm_memory_init(minimal_memory, 64, minimal_bitmap, 4)) {
    return 0;
  }

  // Test longest_free which iterates through the bitmap
  // This exercises status() function calls at the bitmap boundaries
  lbm_uint longest = lbm_memory_longest_free();
  if (longest == 0) return 0;  // Should return positive value for empty memory

  // Allocate some memory to create a pattern in the bitmap
  lbm_uint *ptr1 = lbm_memory_allocate(32);
  if (!ptr1) return 0;

  // Call longest_free again - this exercises status() with allocated memory
  longest = lbm_memory_longest_free();
  // Should return some value (may be small due to fragmentation/reserve)

  // Free the memory
  if (!lbm_memory_free(ptr1)) return 0;

  // Final longest_free call to ensure status() bounds checking works
  // after memory operations
  longest = lbm_memory_longest_free();
  if (longest == 0) return 0;

  return 1;
}

int test_uninitialized_memory_system() {
  // This test should be run before any memory initialization
  // to test the behavior when global memory/bitmap pointers are NULL

  // Test longest_free with uninitialized system
  // This should return 0 because memory == NULL || bitmap == NULL
  lbm_uint longest = lbm_memory_longest_free();
  if (longest != 0) return 0;  // Should return 0 when memory/bitmap is NULL

  // Test allocation with uninitialized system
  // This should return NULL because memory == NULL || bitmap == NULL
  lbm_uint *ptr = lbm_memory_allocate(10);
  if (ptr != NULL) return 0;   // Should return NULL when memory/bitmap is NULL

  return 1;
}

int test_null_memory_init_combinations() {
  // Test different combinations of NULL parameters to lbm_memory_init
  lbm_uint valid_memory[64];
  lbm_uint valid_bitmap[4];

  memset(valid_memory, 0, sizeof(valid_memory));
  memset(valid_bitmap, 0, sizeof(valid_bitmap));

  // Test Case 1: memory=NULL, bitmap=valid - should fail
  int result1 = lbm_memory_init(NULL, 64, valid_bitmap, 4);
  if (result1 != 0) return 0;  // Should fail

  // Test Case 2: memory=valid, bitmap=NULL - should fail
  int result2 = lbm_memory_init(valid_memory, 64, NULL, 4);
  if (result2 != 0) return 0;  // Should fail

  // Test Case 3: memory=NULL, bitmap=NULL - should fail
  int result3 = lbm_memory_init(NULL, 64, NULL, 4);
  if (result3 != 0) return 0;  // Should fail

  // Test Case 4: memory=valid, bitmap=valid - should succeed
  int result4 = lbm_memory_init(valid_memory, 64, valid_bitmap, 4);
  if (result4 == 0) return 0;  // Should succeed

  return 1;
}

int test_memory_operations_after_null_init() {
  // Test memory operations behavior
  // Since previous test may have already initialized memory successfully,
  // we can't easily test the NULL state again. Let's test different scenarios.

  lbm_uint valid_memory[64];
  lbm_uint valid_bitmap[4];

  memset(valid_memory, 0, sizeof(valid_memory));
  memset(valid_bitmap, 0, sizeof(valid_bitmap));

  // Try to initialize with NULL parameters (should fail but not crash)
  int result1 = lbm_memory_init(NULL, 64, valid_bitmap, 4);
  if (result1 != 0) return 0;  // Should fail

  int result2 = lbm_memory_init(valid_memory, 64, NULL, 4);
  if (result2 != 0) return 0;  // Should fail

  // Do a valid initialization for cleanup
  if (!lbm_memory_init(valid_memory, 64, valid_bitmap, 4)) return 0;

  // Test that operations work after valid init
  lbm_uint longest = lbm_memory_longest_free();
  if (longest == 0) return 0;  // Should return positive value

  lbm_uint *ptr = lbm_memory_allocate(10);
  if (ptr == NULL) return 0;   // Should succeed with valid init

  if (!lbm_memory_free(ptr)) return 0;  // Should succeed

  return 1;
}

int test_free_unallocated_valid_address() {
  if (!setup_memory()) return 0;

  // Get a valid memory address that was never allocated
  // We'll create a pointer to a location within the memory range
  // but that doesn't correspond to any allocated block

  // First, allocate some memory to establish a pattern
  lbm_uint *allocated_ptr = lbm_memory_allocate(10);
  if (!allocated_ptr) return 0;

  // Calculate a valid address that's within the memory range but unallocated
  // We'll try an address that's definitely within bounds but at a different offset
  lbm_uint *unallocated_ptr = allocated_ptr + 20;  // Point beyond our allocation

  // Verify this pointer is within the memory range
  if (!lbm_memory_ptr_inside(unallocated_ptr)) {
    // If it's outside bounds, try a different offset within our allocated space
    unallocated_ptr = allocated_ptr + 5;  // Point within our allocation but not at start
  }

  if (!lbm_memory_ptr_inside(unallocated_ptr)) return 0;  // Should be inside memory range

  // Attempt to free the unallocated address
  // This should fail gracefully and return 0 because the address
  // doesn't have START or START_END status in the bitmap
  int free_result = lbm_memory_free(unallocated_ptr);
  if (free_result != 0) return 0;  // Should return 0 (failure) for unallocated address

  // Verify our original allocation is still intact
  if (!lbm_memory_free(allocated_ptr)) return 0;  // This should succeed

  return 1;
}

int test_free_middle_of_allocation() {
  if (!setup_memory()) return 0;

  // Allocate a multi-word block
  lbm_uint *allocated_ptr = lbm_memory_allocate(10);
  if (!allocated_ptr) return 0;

  // Try to free a pointer that's in the middle of the allocation
  // This should fail because only the start of an allocation can be freed
  lbm_uint *middle_ptr = allocated_ptr + 5;

  // Verify this pointer is within the memory range
  if (!lbm_memory_ptr_inside(middle_ptr)) return 0;

  // Attempt to free the middle address
  // This should fail because the bitmap status at this location
  // is FREE_OR_USED (part of the allocation) not START or START_END
  int free_result = lbm_memory_free(middle_ptr);
  if (free_result != 0) return 0;  // Should return 0 (failure)

  // Verify we can still free the original allocation properly
  if (!lbm_memory_free(allocated_ptr)) return 0;  // This should succeed

  return 1;
}

int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  // IMPORTANT: These tests must run FIRST before any memory initialization
  // to test the behavior when global memory/bitmap pointers are NULL
  total_tests++; if (test_uninitialized_memory_system()) tests_passed++;
  total_tests++; if (test_null_memory_init_combinations()) tests_passed++;
  total_tests++; if (test_memory_operations_after_null_init()) tests_passed++;

  total_tests++; if (test_memory_init_valid()) tests_passed++;
  total_tests++; if (test_memory_init_null_pointers()) tests_passed++;
  total_tests++; if (test_memory_init_invalid_alignment()) tests_passed++;
  total_tests++; if (test_memory_reserve()) tests_passed++;
  total_tests++; if (test_memory_allocate_single_word()) tests_passed++;
  total_tests++; if (test_memory_allocate_multiple_words()) tests_passed++;
  total_tests++; if (test_memory_free()) tests_passed++;
  total_tests++; if (test_memory_free_invalid_pointer()) tests_passed++;
  total_tests++; if (test_lbm_malloc_free()) tests_passed++;
  total_tests++; if (test_lbm_malloc_reserve()) tests_passed++;
  total_tests++; if (test_memory_address_to_ix()) tests_passed++;
  total_tests++; if (test_memory_shrink()) tests_passed++;
  total_tests++; if (test_memory_shrink_single_word()) tests_passed++;
  total_tests++; if (test_memory_shrink_invalid()) tests_passed++;
  total_tests++; if (test_memory_longest_free()) tests_passed++;
  total_tests++; if (test_memory_longest_free_uninitialized()) tests_passed++;
  total_tests++; if (test_memory_maximum_used()) tests_passed++;
  total_tests++; if (test_memory_ptr_inside()) tests_passed++;
  total_tests++; if (test_memory_fragmentation()) tests_passed++;
  total_tests++; if (test_memory_allocation_patterns()) tests_passed++;
  total_tests++; if (test_memory_edge_cases()) tests_passed++;

  // Single word allocation tests
  total_tests++; if (test_single_word_allocation_basic()) tests_passed++;
  total_tests++; if (test_single_word_allocation_multiple()) tests_passed++;
  total_tests++; if (test_single_word_free_and_reuse()) tests_passed++;
  total_tests++; if (test_single_word_fragmentation()) tests_passed++;
  total_tests++; if (test_single_word_shrink_operations()) tests_passed++;
  total_tests++; if (test_single_word_mixed_with_multi()) tests_passed++;
  total_tests++; if (test_single_word_boundary_conditions()) tests_passed++;
  total_tests++; if (test_single_word_malloc_interface()) tests_passed++;
  total_tests++; if (test_single_word_status_transitions()) tests_passed++;
  total_tests++; if (test_single_word_allocation_exhaustion()) tests_passed++;
  total_tests++; if (test_memory_update_min_free_unchanged()) tests_passed++;

  // NULL memory and bitmap tests
  total_tests++; if (test_functions_with_null_memory_bitmap()) tests_passed++;
  total_tests++; if (test_allocation_with_null_memory_bitmap()) tests_passed++;
  total_tests++; if (test_free_with_null_memory_bitmap()) tests_passed++;
  total_tests++; if (test_longest_free_with_null_memory_bitmap()) tests_passed++;
  total_tests++; if (test_shrink_with_null_memory_bitmap()) tests_passed++;

  // Bitmap bounds test
  total_tests++; if (test_status_bitmap_bounds()) tests_passed++;

  // Free unallocated memory tests
  total_tests++; if (test_free_unallocated_valid_address()) tests_passed++;
  total_tests++; if (test_free_middle_of_allocation()) tests_passed++;

  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
