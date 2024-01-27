/*
    Copyright 2018, 2021, 2022 Joel Svensson	svenssonjoel@yahoo.se

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
#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <getopt.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>

#include "lispbm.h"

#define EVAL_CPS_STACK_SIZE 256
#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256
#define EXTENSION_STORAGE_SIZE 256

#define WAIT_TIMEOUT 2500

lbm_extension_t extension_storage[EXTENSION_STORAGE_SIZE];

/* Tokenizer state for strings */
static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t string_tok;


bool dyn_load(const char *str, const char **code) {

  bool res = false;
  if (strlen(str) == 5 && strncmp(str, "defun", 5) == 0) {
    *code = "(define defun (macro (name args body) `(define ,name (lambda ,args ,body))))";
    res = true;
  } else if (strlen(str) == 7 && strncmp(str, "reverse", 7) == 0) {
    *code = "(define reverse (lambda (xs)"
            "(let ((revacc (lambda (acc xs)"
	    "(if (eq nil xs) acc"
	    "(revacc (cons (car xs) acc) (cdr xs))))))"
            "(revacc nil xs))))";
    res = true;
  } else if (strlen(str) == 4 && strncmp(str, "iota", 4) == 0) {
    *code = "(define iota (lambda (n)"
            "(let ((iacc (lambda (acc i)"
            "(if (< i 0) acc"
            "(iacc (cons i acc) (- i 1))))))"
            "(iacc nil n))))";
    res = true;
  } else if (strlen(str) == 6 && strncmp(str, "length", 6) == 0) {
    *code = "(define length (lambda (xs)"
	    "(let ((len (lambda (l xs)"
	    "(if (eq xs nil) l"
	    "(len (+ l 1) (cdr xs))))))"
            "(len 0 xs))))";
    res = true;
  } else if (strlen(str) == 4 && strncmp(str, "take", 4) == 0) {
    *code = "(define take (lambda (n xs)"
	    "(let ((take-tail (lambda (acc n xs)"
	    "(if (= n 0) acc"
	    "(take-tail (cons (car xs) acc) (- n 1) (cdr xs))))))"
            "(reverse (take-tail nil n xs)))))";
    res = true;
  } else if (strlen(str) == 4 && strncmp(str, "drop", 4) == 0) {
    *code = "(define drop (lambda (n xs)"
	    "(if (= n 0) xs"
	    "(if (eq xs nil) nil"
            "(drop (- n 1) (cdr xs))))))";
    res = true;
  } else if (strlen(str) == 3 && strncmp(str, "zip", 3) == 0) {
    *code = "(define zip (lambda (xs ys)"
	    "(if (eq xs nil) nil"
	    "(if (eq ys nil) nil"
            "(cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))))";
    res = true;
  } else if (strlen(str) == 3 && strncmp(str, "map", 3) == 0) {
    *code = "(define map (lambda (f xs)"
	    "(if (eq xs nil) nil"
            "(cons (f (car xs)) (map f (cdr xs))))))";
    res = true;
  } else if (strlen(str) == 6 && strncmp(str, "lookup", 6) == 0) {
    *code = "(define lookup (lambda (x xs)"
	    "(if (eq xs nil) nil"
	    "(if (eq (car (car xs)) x)"
	    "(car (cdr (car xs)))"
            "(lookup x (cdr xs))))))";
    res = true;
  } else if (strlen(str) == 5 && strncmp(str, "foldr", 5) == 0) {
    *code = "(define foldr (lambda (f i xs)"
	    "(if (eq xs nil) i"
            "(f (car xs) (foldr f i (cdr xs))))))";
    res = true;
  } else if (strlen(str) == 5 && strncmp(str, "foldl", 5) == 0) {
    *code = "(define foldl (lambda (f i xs)"
            "(if (eq xs nil) i (foldl f (f i (car xs)) (cdr xs)))))";
    res = true;
  }
  return res;
}


uint32_t timestamp_callback() {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (uint32_t)(tv.tv_sec * 1000000 + tv.tv_usec);
}

void sleep_callback(uint32_t us) {
  struct timespec s;
  struct timespec r;
  s.tv_sec = 0;
  s.tv_nsec = (long)us * 1000;
  nanosleep(&s, &r);
}

void done_callback(eval_context_t *ctx) {

  char output[1024];

  lbm_cid cid = ctx->id;
  lbm_value t = ctx->r;

  int print_ret = lbm_print_value(output, 1024, t);

  if (print_ret >= 0) {
    printf("<< Context %"PRI_INT" finished with value %s >>\n", cid, output);
  } else {
    printf("<< Context %"PRI_INT" finished with value %s >>\n", cid, output);
  }
  printf("stack max:  %"PRI_UINT"\n", ctx->K.max_sp);
  printf("stack size: %"PRI_UINT"\n", ctx->K.size);
  printf("stack sp:   %"PRI_INT"\n", ctx->K.sp);

  //  if (!eval_cps_remove_done_ctx(cid, &t)) {
  //   printf("Error: done context (%d)  not in list\n", cid);
  //}
  fflush(stdout);
}

void *eval_thd_wrapper(void *v) {
  lbm_run_eval();
  return NULL;
}

int main(int argc, char **argv) {

  unsigned int heap_size = 8 * 1024 * 1024;  // 8 Megabytes is standard
  pthread_t lispbm_thd;

  int c;
  opterr = 1;

  while (( c = getopt(argc, argv, "gh:")) != -1) {
    switch (c) {
    case 'h':
      heap_size = (unsigned int)atoi((char *)optarg);
      break;
    case '?':
      break;
    default:
      break;
    }
  }
  printf("------------------------------------------------------------\n");
  printf("Heap size: %u\n", heap_size);
  printf("------------------------------------------------------------\n");

  if (argc - optind < 1) {
    printf("Incorrect arguments\n");
    return 0;
  }

  printf("Opening file: %s\n", argv[optind]);

  FILE* fp = fopen(argv[optind], "r");

  if (fp == NULL) {
    printf("Error opening file\n");
    return 0;
  }

  fseek(fp, 0, SEEK_END);
  long size = ftell(fp);
  if (size <= 0) {
    printf("Error file empty %s\n", argv[1]);
    return 0;
  }
  fseek(fp, 0, SEEK_SET);
  char *code_buffer = malloc((unsigned long)size * sizeof(char) + 1);
  size_t r = fread (code_buffer, 1, (unsigned int)size, fp);

  if (r == 0) {
    printf("Error empty file?\n");
    return 0;
  }

  uint32_t *memory = (uint32_t*)malloc(4 * LBM_MEMORY_SIZE_16K);
  uint32_t *bitmap = (uint32_t*)malloc(4 * LBM_MEMORY_BITMAP_SIZE_16K);
  if (memory == NULL || bitmap == NULL) return 0;

  lbm_cons_t *heap_storage = (lbm_cons_t*)malloc(sizeof(lbm_cons_t) * heap_size);
  if (heap_storage == NULL) {
    return 0;
  }

  lbm_init(heap_storage, heap_size,
           memory, LBM_MEMORY_SIZE_16K,
           bitmap, LBM_MEMORY_BITMAP_SIZE_16K,
           GC_STACK_SIZE,
           PRINT_STACK_SIZE,
           extension_storage, EXTENSION_STORAGE_SIZE);

  lbm_set_ctx_done_callback(done_callback);
  lbm_set_timestamp_us_callback(timestamp_callback);
  lbm_set_usleep_callback(sleep_callback);
  lbm_set_dynamic_load_callback(dyn_load);
  lbm_set_printf_callback(printf);

  if (pthread_create(&lispbm_thd, NULL, eval_thd_wrapper, NULL)) {
    printf("Error creating evaluation thread\n");
    return 1;
  }

  lbm_cid cid;

  lbm_create_string_char_channel(&string_tok_state,
                                 &string_tok,
                                 code_buffer);

  lbm_pause_eval();
  while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
    sleep_callback(10);
  }

  cid = lbm_load_and_eval_program(&string_tok,NULL);

  lbm_continue_eval();

  lbm_wait_ctx(cid, WAIT_TIMEOUT);

  return 0;
}
