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
#include "lbm_image.h"

#include "init/start_lispbm.c"

static int test_init(void) {
  return start_lispbm_for_tests();
}

// Drive lbm_allocate_const_raw(1, ...) to exhaustion and report how many
// single-word allocations succeed before the const heap reports full.
static lbm_uint drain_with_raw_words(void) {
  lbm_uint count = 0;
  lbm_uint addr;
  while (lbm_allocate_const_raw(1, &addr) == LBM_FLASH_WRITE_OK) {
    count++;
  }
  return count;
}

// Drive lbm_allocate_const_cell(...) (2 words/cell) to exhaustion and report
// how many words were consumed in total before the const heap reports full.
static lbm_uint drain_with_cells(void) {
  lbm_uint count_cells = 0;
  lbm_value cell;
  while (lbm_allocate_const_cell(&cell) == LBM_FLASH_WRITE_OK) {
    count_cells++;
  }
  return count_cells * 2;
}

// heap.c's lbm_allocate_const_cell and lbm_allocate_const_raw both allocate
// words from the same const/flash heap, bounded by the same
// lbm_image_get_write_index() ceiling. Since both start from a freshly
// booted image (const heap next == 0), they should be able to consume the
// exact same number of words from that shared budget before reporting
// full. If they consume different amounts, the two boundary checks in
// heap.c disagree about how much space is actually usable.
int test_heap_const_raw_vs_cell_boundary_consistency(void) {
  if (!test_init()) return 0;
  lbm_uint words_via_raw = drain_with_raw_words();

  if (!test_init()) return 0; // fresh image / fresh const heap
  lbm_uint words_via_cells = drain_with_cells();

  printf("words usable via lbm_allocate_const_raw(1,...) repeatedly: %lu\n",
         (unsigned long)words_via_raw);
  printf("words usable via lbm_allocate_const_cell() repeatedly (2w/cell): %lu\n",
         (unsigned long)words_via_cells);

  if (words_via_raw != words_via_cells) {
    printf("MISMATCH: raw-word allocator and cell allocator disagree on "
           "usable capacity by %ld word(s)\n",
           (long)words_via_cells - (long)words_via_raw);
    return 0;
  }
  return 1;
}

// A single lbm_allocate_const_raw call requesting exactly the number of
// words that lbm_allocate_const_cell calls proved fit, should also
// succeed, since it is asking for no more than what was already shown to
// be available.
int test_heap_const_raw_single_call_matches_cell_capacity(void) {
  if (!test_init()) return 0;
  lbm_uint capacity_via_cells = drain_with_cells();

  if (!test_init()) return 0; // fresh image / fresh const heap
  lbm_uint addr;
  lbm_flash_status s = lbm_allocate_const_raw(capacity_via_cells, &addr);

  printf("single lbm_allocate_const_raw request for %lu words "
         "(the capacity lbm_allocate_const_cell demonstrated): %s\n",
         (unsigned long)capacity_via_cells,
         s == LBM_FLASH_WRITE_OK ? "OK" : "FULL");

  if (s != LBM_FLASH_WRITE_OK) return 0;
  return 1;
}

int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  total_tests++; if (test_heap_const_raw_vs_cell_boundary_consistency()) tests_passed++;
  printf("%d\n", tests_passed);
  total_tests++; if (test_heap_const_raw_single_call_matches_cell_capacity()) tests_passed++;
  printf("%d\n", tests_passed);

  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
