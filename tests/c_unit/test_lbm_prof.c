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

#include "lbm_prof.h"
#include "init/start_lispbm.c"


int test_lbm_prof_init(void) {

  lbm_prof_t prof_data_buf[100];

  if (!start_lispbm_for_tests()) return 0;
  
  if (lbm_prof_init(prof_data_buf, 100)) return 1;
 
  return 0;
}


int test_lbm_prof_sample_100(void) {
 lbm_prof_t prof_data_buf[100];

  if (!start_lispbm_for_tests()) return 0;
  
  if (!lbm_prof_init(prof_data_buf, 100)) return 0;


  char *prog1 = "(define f (lambda () {(sleep 1) (f)})) (spawn f)";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, prog1);
  lbm_cid cid1 = lbm_load_and_eval_program(&chan1, "thread-1");

  if (cid1 < 0) return 0;
  
  // run sampling a number of times
  for (int i = 0; i < 100; i ++) {
    lbm_prof_sample();
    sleep_callback(100);
  }
  
  lbm_uint num = lbm_prof_get_num_samples();

  if (num == 100) return 1;
  return 0;
}


int test_lbm_prof_measure(void) {
 lbm_prof_t prof_data_buf[100];

  if (!start_lispbm_for_tests()) return 0;
  
  if (!lbm_prof_init(prof_data_buf, 100)) return 0;


  char *prog1 = "(define f (lambda () {(+ 1 2 3 4 5) (sleep 0.01) (f)})) (spawn f)";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, prog1);
  lbm_cid cid1 = lbm_load_and_eval_program(&chan1, "thread-1");

  // cid1 is the cid of the task that spawns the f. Not the f task.
  if (cid1 < 0) return 0;
  
  // run sampling a number of times
  for (int i = 0; i < 1000; i ++) {
    lbm_prof_sample();
    sleep_callback(1000);
  }
  
  lbm_uint num = lbm_prof_get_num_samples();

  lbm_uint sys = lbm_prof_get_num_system_samples();
  lbm_uint sle = lbm_prof_get_num_sleep_samples();

  bool has_cid = false;
  for (int i = 0; i < 100; i ++) {
    if ( prof_data_buf[i].cid != -1) has_cid = true;
  }
  
  // The numbers in sys and sle vary and can be 0 or more.
  // There is no right or wrong number that can be determined without
  // more rigorous testing.
  printf("Num sys: %d\n", sys);
  printf("Num sleep: %d\n", sle);
  printf("%s\n", has_cid ? "HAS CID" : "NO CID");
  
  if (num == 1000) return 1;
  return 0;
}

int test_lbm_prof_measure2(void) {
 lbm_prof_t prof_data_buf[100];

  if (!start_lispbm_for_tests()) return 0;
  
  if (!lbm_prof_init(prof_data_buf, 100)) return 0;


  char *prog1 = "(define f (lambda () {(+ 1 2 3 4 5) (f)})) (spawn \"name\" f)";
  lbm_string_channel_state_t st1;
  lbm_char_channel_t chan1;
  lbm_create_string_char_channel(&st1, &chan1, prog1);
  lbm_cid cid1 = lbm_load_and_eval_program(&chan1, "thread-1");

  char *prog2 = "(define g (lambda () {(+ 1 2 3 4 5) (g)})) (spawn g)";
  lbm_string_channel_state_t st2;
  lbm_char_channel_t chan2;
  lbm_create_string_char_channel(&st2, &chan2, prog2);
  lbm_cid cid2 = lbm_load_and_eval_program(&chan2, "thread-2");

  // cid1 is the cid of the task that spawns the f. Not the f task.
  if (cid1 < 0 || cid2 < 0) return 0;

  // run sampling a number of times
  for (int i = 0; i < 1000; i ++) {
    lbm_prof_sample();
    sleep_callback(1000);
  }
  
  lbm_uint num = lbm_prof_get_num_samples();

  lbm_uint sys = lbm_prof_get_num_system_samples();
  lbm_uint sle = lbm_prof_get_num_sleep_samples();

  int num_cid = 0;
  for (int i = 0; i < 100; i ++) {
    if ( prof_data_buf[i].cid != -1) num_cid++;
  }
  
  // The numbers in sys and sle vary and can be 0 or more.
  // There is no right or wrong number that can be determined without
  // more rigorous testing.
  printf("Num sys: %d\n", sys);
  printf("Num sleep: %d\n", sle);
  printf("Num cid: %d\n", num_cid);
  printf("Num: %d\n", num);

  if (num == 1000) return 1;
  return 0;
}

int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  total_tests++; if (test_lbm_prof_init()) tests_passed++;
  total_tests++; if (test_lbm_prof_sample_100()) tests_passed++;
  total_tests++; if (test_lbm_prof_measure()) tests_passed++;
  total_tests++; if (test_lbm_prof_measure2()) tests_passed++;
  
  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
