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

#include <stdio.h>
#include <string.h>

#include "FreeRTOS.h"
#include "task.h"

#include "lispbm.h"
#include "lbm_image.h"
#include "platform_thread.h"

// ////////////////////////////////////////////////////////////
// LispBM configuration

#define GC_STACK_SIZE          256
#define PRINT_STACK_SIZE       256
#define HEAP_SIZE              8192
#define EXTENSION_STORAGE_SIZE 256

static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));
static lbm_uint memory_array[LBM_MEMORY_SIZE_16K];
static lbm_uint bitmap_array[LBM_MEMORY_BITMAP_SIZE_16K];
static lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];

#define IMAGE_SIZE 32768
static uint32_t image_buffer[IMAGE_SIZE];

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t string_tok;

static lbm_thread_t eval_thread;

// ////////////////////////////////////////////////////////////
// Callbacks

static void done_callback(eval_context_t *ctx) {
  char buf[256];
  lbm_print_value(buf, 256, ctx->r);
  printf("# %s\n", buf);
}

static void usleep_callback(uint32_t us) {
  TickType_t t = us / (portTICK_PERIOD_MS * 1000);
  if (t == 0) t = 1;
  vTaskDelay(t);
}

// ////////////////////////////////////////////////////////////
// Image (RAM-backed, not persistent across power cycles)

static bool image_write(uint32_t data, int32_t index, bool const_heap) {
  (void)const_heap;
  image_buffer[index] = data;
  return true;
}

// ////////////////////////////////////////////////////////////
// Eval thread

static void eval_thread_func(void *arg) {
  (void)arg;
  lbm_run_eval();
  vTaskDelete(NULL);
}

// ////////////////////////////////////////////////////////////
// LispBM initialization

static bool startup_lbm(void) {

  if (!lbm_init(heap, HEAP_SIZE,
                memory_array, LBM_MEMORY_SIZE_16K,
                bitmap_array, LBM_MEMORY_BITMAP_SIZE_16K,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    printf("Failed to initialize LispBM\n");
    return false;
  }

  lbm_set_usleep_callback(usleep_callback);
  lbm_set_ctx_done_callback(done_callback);
  lbm_set_printf_callback(printf);
  lbm_set_verbose(true);

  lbm_image_init(image_buffer, IMAGE_SIZE, image_write);

  if (!lbm_image_exists()) {
    printf("Image does not exist - creating\n");
    lbm_image_create("v01");
  }
  if (!lbm_image_boot()) {
    printf("Unable to boot image\n");
    return false;
  }
  printf("Image booted\n");

  lbm_add_eval_symbols();

  if (!lbm_thread_create(&eval_thread,
                         "lbm_eval",
                         eval_thread_func,
                         NULL,
                         LBM_THREAD_PRIO_NORMAL,
                         4096)) {
    printf("Failed to create eval thread\n");
    return false;
  }

  return true;
}

// ////////////////////////////////////////////////////////////
// REPL input handling

#define INPUT_BUFFER_SIZE 512

static void repl_task(void *pvParameters) {
  (void)pvParameters;

  char input_buffer[INPUT_BUFFER_SIZE];
  int pos = 0;

  printf("\nLispBM REPL on Zynq UltraScale+\n");
  printf("> "); fflush(stdout);

  while (1) {
    char c = inbyte();

    if (c == '\r' || c == '\n') {
      printf("\n");
      if (pos > 0) {
        input_buffer[pos] = '\0';

        lbm_pause_eval();
        while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
          vTaskDelay(1);
        }

        lbm_create_string_char_channel(&string_tok_state,
                                       &string_tok,
                                       input_buffer);
        lbm_load_and_eval_expression(&string_tok);
        lbm_continue_eval();

        vTaskDelay(100 / portTICK_PERIOD_MS);

        pos = 0;
      }
      printf("> "); fflush(stdout);
    } else if (c == 127 || c == '\b') {
      if (pos > 0) {
        pos--;
        outbyte('\b');
        outbyte(' ');
        outbyte('\b');
      }
    } else if (pos < INPUT_BUFFER_SIZE - 1) {
      input_buffer[pos++] = c;
      outbyte(c);
    }
  }
}

// ////////////////////////////////////////////////////////////
// Main

int main(void) {

  printf("LispBM REPL example starting...\n");

  if (!startup_lbm()) {
    printf("LispBM startup failed.\n");
    for (;;);
  }

  xTaskCreate(repl_task,
              "repl",
              configMINIMAL_STACK_SIZE * 4,
              NULL,
              tskIDLE_PRIORITY + 1,
              NULL);

  vTaskStartScheduler();

  for (;;);
  return 0;
}
