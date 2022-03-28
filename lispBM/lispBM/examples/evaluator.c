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
#define VARIABLE_STORAGE_SIZE 256

#define WAIT_TIMEOUT 2500

uint32_t gc_stack_storage[GC_STACK_SIZE];
uint32_t print_stack_storage[PRINT_STACK_SIZE];
extension_fptr extension_storage[EXTENSION_STORAGE_SIZE];
lbm_value variable_storage[VARIABLE_STORAGE_SIZE];


/* Tokenizer state for strings */
static lbm_tokenizer_string_state_t string_tok_state;
/* Tokenizer statefor compressed data */
static tokenizer_compressed_state_t comp_tok_state;
/* shared tokenizer */
static lbm_tokenizer_char_stream_t string_tok;


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

void *eval_thd_wrapper(void *v) {
  lbm_run_eval();
  return NULL;
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

int main(int argc, char **argv) {

  unsigned int heap_size = 8 * 1024 * 1024;  // 8 Megabytes is standard
  bool compress_decompress = false;
  pthread_t lispbm_thd;

  int c;
  opterr = 1;

  while (( c = getopt(argc, argv, "gch:")) != -1) {
    switch (c) {
    case 'h':
      heap_size = (unsigned int)atoi((char *)optarg);
      break;
    case 'c':
      compress_decompress = true;
      break;
    case '?':
      break;
    default:
      break;
    }
  }
  printf("------------------------------------------------------------\n");
  printf("Heap size: %u\n", heap_size);
  printf("Compression: %s\n", compress_decompress ? "yes" : "no");
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
           gc_stack_storage, GC_STACK_SIZE,
           memory, LBM_MEMORY_SIZE_16K,
           bitmap, LBM_MEMORY_BITMAP_SIZE_16K,
           print_stack_storage, PRINT_STACK_SIZE,
           extension_storage, EXTENSION_STORAGE_SIZE);

  lbm_set_ctx_done_callback(done_callback);
  lbm_set_timestamp_us_callback(timestamp_callback);
  lbm_set_usleep_callback(sleep_callback);

  if (pthread_create(&lispbm_thd, NULL, eval_thd_wrapper, NULL)) {
    printf("Error creating evaluation thread\n");
    return 1;
  }

  prelude_load(&string_tok_state,
               &string_tok);
  lbm_cid cid = lbm_load_and_eval_program(&string_tok);

  lbm_wait_ctx(cid, WAIT_TIMEOUT);

  lbm_value t;
  char *compressed_code;
  char decompress_code[8192];

  if (compress_decompress) {
    uint32_t compressed_size = 0;
    compressed_code = lbm_compress(code_buffer, &compressed_size);
    if (!compressed_code) {
      printf("Error compressing code\n");
      return 0;
    }
    lbm_decompress(decompress_code, 8192, compressed_code);
    printf("\n\nDECOMPRESS TEST: %s\n\n", decompress_code);
    lbm_create_char_stream_from_compressed(&comp_tok_state,
                                           &string_tok,
                                           compressed_code);
  } else {
     lbm_create_char_stream_from_string(&string_tok_state,
                                        &string_tok,
                                        code_buffer);
  }

  lbm_pause_eval();
  while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
    sleep_callback(10);
  }

  cid = lbm_load_and_eval_program(&string_tok);

  lbm_continue_eval();

  t = lbm_wait_ctx(cid, WAIT_TIMEOUT);

  char output[1024];

  if (compress_decompress) {
    free(compressed_code);
  }

  return 0;
}
