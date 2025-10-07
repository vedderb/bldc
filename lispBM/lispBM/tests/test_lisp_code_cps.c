/*
    Copyright 2018, 2020, 2023 Joel Svensson   svenssonjoel@yahoo.se

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
#define _GNU_SOURCE // MAP_ANON
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <getopt.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/mman.h>

#include "lispbm.h"
#include "extensions/array_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/runtime_extensions.h"
#include "extensions/random_extensions.h"
#include "extensions/set_extensions.h"
#include "extensions/mutex_extensions.h"
#include "extensions/lbm_dyn_lib.h"
#include "lbm_channel.h"
#include "lbm_flat_value.h"
#include "lbm_image.h"
#include "platform_timestamp.h"

#define WAIT_TIMEOUT 2500

#define GC_STACK_SIZE 96
#define PRINT_STACK_SIZE 256
#define EXTENSION_STORAGE_SIZE 200
#define CONSTANT_MEMORY_SIZE 4*1024 // in words


#define FAIL 0
#define SUCCESS 1

lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];

#define IMAGE_STORAGE_SIZE              (128 * 1024)
#ifdef LBM64
// Cannot map address above 2^48 so use same as for 32 bit...
#define IMAGE_FIXED_VIRTUAL_ADDRESS     (void*)0xA0000000
#else
#define IMAGE_FIXED_VIRTUAL_ADDRESS     (void*)0xA0000000
#endif

static size_t   image_storage_size = IMAGE_STORAGE_SIZE;
static uint32_t *image_storage = NULL;

#ifndef LONGER_DELAY
static uint32_t timeout = 10;
#else
static uint32_t timeout = 30;
#endif

bool image_write(uint32_t w, int32_t ix, bool const_heap) {
  (void) const_heap;
  if (image_storage[ix] == 0xffffffff) {
    image_storage[ix] = w;
    return true;
  } else if (image_storage[ix] == w) {
    return true;
  } else {
    printf("image_storage[%u] = %x\n", ix, image_storage[ix]);
    printf("when trying to write %x\n", w);
  }
  return false;
}

bool image_clear(void) {
  memset(image_storage, 0xff, image_storage_size);
  return true;
}


/* Tokenizer state for strings */
//static lbm_tokenizer_string_state_t string_tok_state;

/* Tokenizer statefor compressed data */
//static tokenizer_compressed_state_t comp_tok_state;

/* shared tokenizer */
//static lbm_tokenizer_char_stream_t string_tok;

static lbm_char_channel_t string_tok;
static lbm_string_channel_state_t string_tok_state;
static lbm_buffered_channel_state_t buffered_tok_state;

void *eval_thd_wrapper(void *v) {
  (void)v;
  lbm_run_eval();
  return NULL;
}

void sleep_callback(uint32_t us) {
  struct timespec s;
  struct timespec r;
  s.tv_sec = 0;
  s.tv_nsec = (long)us * 1000;
  nanosleep(&s, &r);
}
volatile bool experiment_success = false;
volatile bool experiment_done = false;

lbm_cid test_cid = -1;

void context_done_callback(eval_context_t *ctx) {
  char output[128];
  lbm_value t = ctx->r;

  (void)lbm_print_value(output, 128, t);

  printf("Thread %d finished: %s\n", (int32_t)ctx->id, output);
}

void critical_error(void) {
  printf("Critical error\n");
  exit(EXIT_FAILURE);
}


// Dynamic loader

bool dyn_load(const char *str, const char **code) {

  size_t len = strlen(str);
  if (len == 5 && strncmp(str, "defun", 5) == 0) {
    *code = "(define defun (macro (name args body) `(define ,name (lambda ,args ,body))))";
    return true;
  }  else if (len == 4 && strncmp(str, "iota", 4) == 0) {
    *code = "(define iota (lambda (n)"
            "(range 0 n)))";
    return true;
  } else if (len == 4 && strncmp(str, "take", 4) == 0) {
    *code = "(define take (lambda (n xs)"
            "(let ((take-tail (lambda (acc n xs)"
            "(if (= n 0) acc"
            "(take-tail (cons (car xs) acc) (- n 1) (cdr xs))))))"
            "(reverse (take-tail nil n xs)))))";
    return true;
  } else if (len == 4 && strncmp(str, "drop", 4) == 0) {
    *code = "(define drop (lambda (n xs)"
            "(if (= n 0) xs"
            "(if (eq xs nil) nil"
            "(drop (- n 1) (cdr xs))))))";
    return true;
  } else if (len == 3 && strncmp(str, "zip", 3) == 0) {
    *code = "(define zip (lambda (xs ys)"
            "(if (eq xs nil) nil"
            "(if (eq ys nil) nil"
            "(cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))))";
    return true;
  } else if (len == 6 && strncmp(str, "lookup", 6) == 0) {
    *code = "(define lookup (lambda (x xs)"
            "(if (eq xs nil) nil"
            "(if (eq (car (car xs)) x)"
            "(car (cdr (car xs)))"
            "(lookup x (cdr xs))))))";
    return true;
  } else if (len == 5 && strncmp(str, "foldr", 5) == 0) {
    *code = "(define foldr (lambda (f i xs)"
            "(if (eq xs nil) i"
            "(f (car xs) (foldr f i (cdr xs))))))";
    return true;
  } else if (len == 5 && strncmp(str, "foldl", 5) == 0) {
    *code = "(define foldl (lambda (f i xs)"
            "(if (eq xs nil) i (foldl f (f i (car xs)) (cdr xs)))))";
    return true;
  }

  return lbm_dyn_lib_find(str,code);
}

  //lbm_value ext_even(lbm_value *args, lbm_uint argn) {
LBM_EXTENSION(ext_even, args, argn){
  if (argn < 1) return lbm_enc_sym(SYM_NIL);

  lbm_value v = args[0];

  if (lbm_type_of(v) == LBM_TYPE_I ||
      lbm_type_of(v) == LBM_TYPE_U) {
    if (lbm_dec_i(v) % 2 == 0)
      return lbm_enc_sym(SYM_TRUE);
  }

  return lbm_enc_sym(SYM_NIL);
}

LBM_EXTENSION(ext_odd, args, argn){
  //lbm_value ext_odd(lbm_value *args, lbm_uint argn) {

  if (argn < 1) return lbm_enc_sym(SYM_NIL);

  lbm_value v = args[0];

  if (lbm_type_of(v) == LBM_TYPE_I ||
      lbm_type_of(v) == LBM_TYPE_U) {
    if (lbm_dec_i(v) % 2 == 1)
      return lbm_enc_sym(SYM_TRUE);
  }

  return lbm_enc_sym(SYM_NIL);
}

LBM_EXTENSION(ext_numbers, args, argn) {

  bool b = true;

  for (unsigned int i = 0; i < argn; i ++) {
    if (!lbm_is_number(args[i])) {
      b = false;
      break;
    }
  }
  return lbm_enc_sym(b ? SYM_TRUE : SYM_NIL);
}


LBM_EXTENSION(ext_event_sym, args, argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn == 1 && lbm_is_symbol(args[0])) {
    lbm_flat_value_t v;
    if (lbm_start_flatten(&v, 1 + sizeof(lbm_uint) + 20)) {
      f_sym(&v, lbm_dec_sym(args[0]));
      lbm_finish_flatten(&v);
      lbm_event(&v);
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

LBM_EXTENSION(ext_event_float, args, argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    float f = lbm_dec_as_float(args[0]);
    lbm_flat_value_t v;
    if (lbm_start_flatten(&v, 1 + sizeof(float) + 20)) {
      f_float(&v, f);
      lbm_finish_flatten(&v);
      lbm_event(&v);
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

LBM_EXTENSION(ext_event_list_of_float, args, argn) {
  LBM_CHECK_NUMBER_ALL();
  lbm_value res = ENC_SYM_EERROR;
  if (argn >= 2) {
    lbm_flat_value_t v;
    if (lbm_start_flatten(&v, 8 + ((1 + sizeof(lbm_uint) * argn) + (1 + sizeof(lbm_uint))))) {
      for (unsigned int i = 0; i < argn; i ++) {
        f_cons(&v);
        float f = lbm_dec_as_float(args[i]);
        f_float(&v, f);
      }
      f_sym(&v, SYM_NIL);
      lbm_finish_flatten(&v);
      lbm_event(&v);
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

LBM_EXTENSION(ext_event_array, args, argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn == 1 && lbm_is_symbol(args[0])) {
    char *hello = "hello world";
    lbm_flat_value_t v;
    if (lbm_start_flatten(&v, 100)) {
      f_cons(&v);
      f_sym(&v,lbm_dec_sym(args[0]));
      f_lbm_array(&v, 12, (uint8_t*)hello);
      lbm_finish_flatten(&v);
      lbm_event(&v);
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

LBM_EXTENSION(ext_block, args, argn) {
  (void) args;
  (void) argn;

  lbm_block_ctx_from_extension();
  return ENC_SYM_NIL; //ignored
}

LBM_EXTENSION(ext_unblock, args, argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_cid c = lbm_dec_as_i32(args[0]);
    lbm_flat_value_t v;
    if (lbm_start_flatten(&v, 1 + sizeof(lbm_uint))) {
      f_sym_string(&v, "t");
      lbm_finish_flatten(&v);
      lbm_unblock_ctx(c,&v);
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

LBM_EXTENSION(ext_block_rmbr, args, argn) {
  (void) args;
  (void) argn;

  lbm_value array;
  if (lbm_heap_allocate_array(&array, 4)) {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(array);

    uint8_t *data = (uint8_t *)header->data;
    for (int i = 0; i < 4; i ++) {
      data[i] = (uint8_t)( 1 + i);
    }

    lbm_block_ctx_from_extension();
    return array; // remembered
  }
  return ENC_SYM_MERROR;
}

LBM_EXTENSION(ext_unblock_rmbr, args, argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_cid c = lbm_dec_as_i32(args[0]);
    // Glob array will be protected until cid is allowed to run again.
    // Should keep array safe from GC.
    lbm_unblock_ctx_r(c);
    return ENC_SYM_TRUE;
  }
  return res;
}

LBM_EXTENSION(ext_unblock_error, args, argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_cid c = lbm_dec_as_i32(args[0]);
    lbm_flat_value_t v;
    if (lbm_start_flatten(&v, 1 + sizeof(lbm_uint))) {
      f_sym(&v, SYM_EERROR);
      lbm_finish_flatten(&v);
      lbm_unblock_ctx(c,&v);
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

int checks = 0;
LBM_EXTENSION(ext_check, args, argn) {

  if (argn != 1 && argn != 2) return ENC_SYM_NIL;

  char output[128];
  lbm_value t = args[0];

  if (argn == 2) {
    checks ++;
  } else {
    checks = 2;
  }

  int res = lbm_print_value(output, 128, t);
  printf("Checking result value: %s\n", output);

  if (checks == 2) {
    experiment_done = true;
    if (res && lbm_type_of(t) == LBM_TYPE_SYMBOL && lbm_dec_sym(t) == SYM_TRUE){ // structural_equality(car(rest),car(cdr(rest)))) {
      experiment_success = true;
      printf("Test: OK!\n");
      printf("Result: %s\n", output);
    } else {
      printf("Test: Failed!\n");
      printf("Result: %s\n", output);
    }
  }
  return ENC_SYM_TRUE;
}

char *const_prg = "(define a 10) (+ a 1)";

LBM_EXTENSION(ext_flash_prg, args, argn) {
  (void) args;
  (void) argn;
  lbm_value v = ENC_SYM_NIL;

  if (!lbm_share_array_const(&v, const_prg, strlen(const_prg)+1))
    return ENC_SYM_NIL;
  return v;
}

LBM_EXTENSION(ext_inc_i, args, argn) {
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_int i = lbm_dec_as_i32(args[0]);
    return lbm_enc_i(i + 1);
  }
  return ENC_SYM_EERROR;
}

LBM_EXTENSION(ext_load_inc_i, args, argn) {
  (void) args;
  (void) argn;

  lbm_add_extension("ext-inc-i", ext_inc_i);
  return ENC_SYM_TRUE;
}


LBM_EXTENSION(ext_flatten_depth, args, argn) {
  lbm_value res = ENC_SYM_NIL;
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_int i = lbm_dec_as_i32(args[0]);
    lbm_set_max_flatten_depth(i);
    res = ENC_SYM_TRUE;
  }
  return res;
}

int main(int argc, char **argv) {

  int res = 0;

  image_storage = mmap(IMAGE_FIXED_VIRTUAL_ADDRESS,
                       IMAGE_STORAGE_SIZE,
                       PROT_READ | PROT_WRITE,
                       MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
  if (((int)image_storage) == -1) {
    printf("error mapping fixed location for flash emulation\n");
    return 0;
  }

  unsigned int heap_size = 8 * 1024;  // 8k cells 

  bool stream_source = false;
  bool incremental = false;

  static pthread_t timestamp_thread = 0;
  pthread_t lispbm_thd;
  lbm_cons_t *heap_storage = NULL;

  pthread_create(&timestamp_thread, NULL, timestamp_cacher, NULL);

  int c;
  opterr = 1;

  while (( c = getopt(argc, argv, "igsch:t:")) != -1) {
    switch (c) {
    case 't':
      timeout = (uint32_t)atoi((char *)optarg);
      break;
    case 'h':
      heap_size = (unsigned int)atoi((char *)optarg);
      break;
    case 'i':
      incremental = true;
      break;
      //    case 'c':
      //compress_decompress = true;
      //break;
    case 's':
      stream_source = true;
    case '?':
      break;
    default:
      break;
    }
  }
  printf("------------------------------------------------------------\n");
  printf("Heap size: %u\n", heap_size);
  printf("Streaming source: %s\n", stream_source ? "yes" : "no");
  printf("Incremental read: %s\n", incremental ? "yes" : "no");
  printf("------------------------------------------------------------\n");

  if (argc - optind < 1) {
    printf("Incorrect arguments\n");
    return FAIL;
  }

  printf("Opening file: %s\n", argv[optind]);

  FILE* fp = fopen(argv[optind], "r");

  if (fp == NULL) {
    printf("Error opening file\n");
    return FAIL;
  }

  fseek(fp, 0, SEEK_END);
  long size = ftell(fp);
  if (size <= 0) {
    printf("Error file empty %s\n", argv[1]);
    return FAIL;
  }
  fseek(fp, 0, SEEK_SET);
  char *code_buffer = malloc((unsigned long)size * sizeof(char) + 1);
  if (!code_buffer) return 0;
  memset(code_buffer, 0, (unsigned long)size * sizeof(char) + 1);
  size_t r = fread (code_buffer, 1, (unsigned int)size, fp);

  if (r == 0) {
    printf("Error empty file?\n");
    return FAIL;
  }

  lbm_uint *memory = NULL;
  lbm_uint *bitmap = NULL;
  if (sizeof(lbm_uint) == 4) {
    memory = malloc(sizeof(lbm_uint) * LBM_MEMORY_SIZE_16K);
    if (memory == NULL) return 0;
    bitmap = malloc(sizeof(lbm_uint) * LBM_MEMORY_BITMAP_SIZE_16K);
    if (bitmap == NULL) return 0;
    res = lbm_memory_init(memory, LBM_MEMORY_SIZE_16K,
                          bitmap, LBM_MEMORY_BITMAP_SIZE_16K);
  } else {
    memory = malloc(sizeof(lbm_uint) * LBM_MEMORY_SIZE_1M);
    if (memory == NULL) return 0;
    bitmap = malloc(sizeof(lbm_uint) * LBM_MEMORY_BITMAP_SIZE_1M);
    if (bitmap == NULL) return 0;
    res = lbm_memory_init(memory, LBM_MEMORY_SIZE_1M,
                          bitmap, LBM_MEMORY_BITMAP_SIZE_1M);
  }

  if (res)
    printf("Memory initialized.\n");
  else {
    printf("Error initializing memory!\n");
    return FAIL;
  }

  heap_storage = (lbm_cons_t*)malloc(sizeof(lbm_cons_t) * heap_size);
  if (heap_storage == NULL) {
    return FAIL;
  }

  if (lbm_init(heap_storage, heap_size,
               memory, LBM_MEMORY_SIZE_16K,
               bitmap, LBM_MEMORY_BITMAP_SIZE_16K,
               GC_STACK_SIZE,
               PRINT_STACK_SIZE,
               extensions,
               EXTENSION_STORAGE_SIZE)
              ) {
    printf ("LBM Initialized\n");
  } else {
    printf ("FAILED to initialize LBM\n");
    return FAIL;
  }

  lbm_image_init(image_storage,
                 image_storage_size / sizeof(lbm_uint),
                 image_write);

  image_clear();
  lbm_image_create("test-image");

  if (!lbm_image_boot()) {
    printf("Error booting image\n");
    return 0;
  }
  lbm_add_eval_symbols();

  res = lbm_eval_init_events(20);
  if (res)
    printf("Events initialized.\n");
  else {
    printf("Error initializing events.\n");
    return FAIL;
  }

  lbm_array_extensions_init();
  lbm_math_extensions_init();
  lbm_string_extensions_init();
  lbm_runtime_extensions_init();
  lbm_random_extensions_init();
  lbm_mutex_extensions_init();
  lbm_set_extensions_init();
  lbm_dyn_lib_init();

  lbm_add_extension("ext-even", ext_even);
  lbm_add_extension("ext-odd", ext_odd);
  lbm_add_extension("ext-numbers", ext_numbers);
  lbm_add_extension("event-sym", ext_event_sym);
  lbm_add_extension("event-float", ext_event_float);
  lbm_add_extension("event-list-of-float", ext_event_list_of_float);
  lbm_add_extension("event-array", ext_event_array);
  lbm_add_extension("block", ext_block);
  lbm_add_extension("unblock", ext_unblock);
  lbm_add_extension("block-rmbr", ext_block_rmbr);

  lbm_add_extension("unblock-rmbr", ext_unblock_rmbr);
  lbm_add_extension("unblock-error", ext_unblock_error);
  lbm_add_extension("flash-prg", ext_flash_prg);
  lbm_add_extension("check", ext_check);
  lbm_add_extension("load-inc-i", ext_load_inc_i);
  lbm_add_extension("flatten-depth", ext_flatten_depth);

  if (lbm_get_num_extensions() < lbm_get_max_extensions()) {
    printf("Extensions loaded successfully\n");
  } else {
    printf("Failed to load Extensions\n");
    return FAIL;
  }

  lbm_set_dynamic_load_callback(dyn_load);
  lbm_set_usleep_callback(sleep_callback);
  lbm_set_printf_callback(printf);
  lbm_set_critical_error_callback(critical_error);

  lbm_set_verbose(true);

  printf("LBM memory free: %"PRI_UINT" words, %"PRI_UINT" bytes \n", lbm_memory_num_free(), lbm_memory_num_free() * sizeof(lbm_uint));

  if (pthread_create(&lispbm_thd, NULL, eval_thd_wrapper, NULL)) {
    printf("Error creating evaluation thread\n");
    return FAIL;
  }
  sleep_callback(50);
  lbm_cid cid;

  lbm_pause_eval_with_gc(20);
  int wait_count = 0;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
    if (wait_count >= 100) {
      printf("Could not pause the evaluator\n");
      return FAIL;
    }
    printf("Wait for pause init\n");
    sleep_callback(100);
    wait_count++;
  }
  if (stream_source) {
    lbm_create_buffered_char_channel(&buffered_tok_state,
                                     &string_tok);
  } else {
    lbm_create_string_char_channel(&string_tok_state,
                                   &string_tok,
                                   code_buffer);
  }
  //}

  lbm_set_ctx_done_callback(context_done_callback);
  if (incremental) {
    cid = lbm_load_and_eval_program_incremental(&string_tok, NULL);
  } else {
    cid = lbm_load_and_eval_program(&string_tok, NULL);
  }

  if (cid == -1) {
    printf("Failed to load and evaluate the test program\n");
    return FAIL;
  }

  test_cid = cid; // the result which is important for success or failure of test.
  printf("test_cid = %"PRI_INT"\n", test_cid);

  lbm_continue_eval();
  uint32_t stream_i = 0;

  if (stream_source) {
    int stuck_count = 0;
    int i = 0;
    while (true) {
      if (code_buffer[i] == 0) {
        lbm_channel_writer_close(&string_tok);
        break;
      }
      int ch_res = lbm_channel_write(&string_tok, code_buffer[i]);

      if (ch_res == CHANNEL_SUCCESS) {
        //printf("wrote: %c\n", code_buffer[i]);
        i ++;
      } else if (ch_res == CHANNEL_READER_CLOSED) {
        break;
      } else {
        if ((stream_i % 100) == 99) {
          printf("stuck streaming\n");
          stuck_count ++;
          if (stuck_count == 10) return 0;
        }
        stream_i ++;
        sleep_callback(2);
      }
    }
  }
  printf("Program loaded\n");
  uint32_t i = 0;
  bool timed_out = false;
  while (!experiment_done) {
    if (i >= timeout * 1000) {
      timed_out = true;
      break;
    }
    sleep_callback(1000);
    i ++;
  }

  if (timed_out) {
    printf ("experiment failed due to taking longer than %u seconds\n", timeout);
    experiment_success = false;
    return FAIL;
  }

  lbm_pause_eval();
  uint32_t pause_i = 0;
  while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
    if ((pause_i % 100) == 99) {
      printf("Waiting for pause\n");
    }
    pause_i ++;
    sleep_callback(2);
  }

  free(heap_storage);

  printf("Experiment done: ");
  printf("Check was executed %u times\n", checks);
  if (experiment_success) {
    printf("SUCCESS\n");
    return 1;
  }
  printf("FAILURE\n");
  return 0;
}
