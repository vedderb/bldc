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
#include <math.h>

#include "lispbm.h"
#include "lbm_image.h"
#include "lbm_channel.h"

#include "extensions/lbm_dyn_lib.h"

#define IMAGE_STORAGE_SIZE              (128 * 1024) // bytes:
#define IMAGE_FIXED_VIRTUAL_ADDRESS     (void*)0xA0000000
static uint32_t *image_storage = NULL;
static size_t   image_storage_size = IMAGE_STORAGE_SIZE;

#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256
#define EXTENSION_STORAGE_SIZE 1024
#define HEAP_SIZE 4096

lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];
static size_t lbm_memory_size = LBM_MEMORY_SIZE_10K;
static size_t lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_10K;
static lbm_cons_t heap_storage[HEAP_SIZE];

static lbm_uint *memory=NULL;
static lbm_uint *bitmap=NULL;

bool image_write(uint32_t w, int32_t ix, bool const_heap) { // ix >= 0 and ix <= image_size
  (void) const_heap;
  if (image_storage[ix] == 0xffffffff) {
    image_storage[ix] = w;
    return true;
  } else if (image_storage[ix] == w) {
    return true;
  }
  return false;
}

bool image_clear(void) {
  memset(image_storage, 0xff, image_storage_size);
  return true;
}

void *eval_thd_wrapper(void *v) {
  (void) v;
  lbm_run_eval();
  return NULL;
}

void critical(void) {
  printf("CRITICAL ERROR\n");
}

uint32_t lbm_timestamp(void) {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (uint32_t)(tv.tv_sec * 1000000 + tv.tv_usec);
}

typedef struct done_cid_s {
  lbm_cid id;
  lbm_value r;
  struct done_cid_s *next;
} done_cid_t;

done_cid_t *done_list = NULL;

void sleep_callback(uint32_t us); // Forward declaration

lbm_value wait_cid(lbm_cid id) {

  if (id < 0) return ENC_SYM_NIL;
  
  while (true) {

    done_cid_t *prev = NULL;
    done_cid_t *curr = done_list;
    while (curr) {
      if (curr->id == id) {
        if (prev != NULL) {
          prev->next = curr->next;
        } else {
          done_list = curr->next;
        }
        lbm_value result = curr->r;
        free(curr);
        return result; // only still valid if no GC has happened.
      }
      prev = curr;
      curr = curr->next;
    }
    sleep_callback(100);
  }
}

void done_callback(eval_context_t *ctx) {

  //char output[1024];
  //lbm_value t = ctx->r;
  //lbm_print_value(output, 1024, t);
  //printf("done: %d, %s\n", ctx->id, output);

  done_cid_t *new = malloc(sizeof(done_cid_t));
  new->id = ctx->id;
  new->r = ctx->r;
  new->next = done_list;

  done_list = new;
  
  //printf("ctx %d exits with value: %s\n", ctx->id, output);
}

int error_print(const char *format, ...) {
  va_list args;
  va_start (args, format);
  int n = vprintf(format, args);
  va_end(args);
  return n;
}

void sleep_callback(uint32_t us) {
  struct timespec s;
  struct timespec r;
  s.tv_sec = 0;
  s.tv_nsec = (long)us * 1000;
  nanosleep(&s, &r);
}

bool dynamic_loader(const char *str, const char **code) {
  return lbm_dyn_lib_find(str, code);
}

int main(void) {

  // Frama-c does not seem to understand a mmapped address as valid.
  // using malloc until I understand frama-c enough.
  image_storage = malloc(image_storage_size * sizeof(lbm_uint));
  
  memory = (lbm_uint*)malloc(lbm_memory_size * sizeof(lbm_uint));
  bitmap = (lbm_uint*)malloc(lbm_memory_bitmap_size * sizeof(lbm_uint));

  if (memory == NULL || bitmap == NULL) return 0;

  if (!lbm_init(heap_storage, HEAP_SIZE,
                memory, lbm_memory_size,
                bitmap, lbm_memory_bitmap_size,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    return 0;
  }

  lbm_set_critical_error_callback(critical);
  lbm_set_ctx_done_callback(done_callback);
  lbm_set_timestamp_us_callback(timestamp);
  lbm_set_usleep_callback(sleep_callback);
  lbm_set_printf_callback(error_print);
  lbm_set_dynamic_load_callback(dynamic_loader);
  
  lbm_image_init(image_storage,
                 image_storage_size / sizeof(uint32_t), //sizeof(lbm_uint),
                 image_write);
  image_clear();
  lbm_image_create("bepa_1");
  lbm_image_boot();
  
  lbm_add_eval_symbols();
  lbm_dyn_lib_init();
  
  // GO into evaluation loop.
  eval_thd_wrapper(NULL);
  
  return 0;
}
  
