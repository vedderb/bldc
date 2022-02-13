 /*
    Copyright 2019 - 2022 Joel Svensson	svenssonjoel@yahoo.se

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

#include <device.h>
#include <drivers/uart.h>
#include <zephyr.h>
#include <sys/ring_buffer.h>
#include <stdint.h>

#include "lispbm.h"

#include "usb_cdc.h"

#define LISPBM_HEAP_SIZE 2048
#define LISPBM_OUTPUT_BUFFER_SIZE 1024
#define LISPBM_INPUT_BUFFER_SIZE  1024
#define EVAL_CPS_STACK_SIZE 256
#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256

static char str[LISPBM_INPUT_BUFFER_SIZE];
static char outbuf[LISPBM_OUTPUT_BUFFER_SIZE];

static uint32_t memory[LBM_MEMORY_SIZE_8K];
static uint32_t bitmap[LBM_MEMORY_BITMAP_SIZE_8K];

static lbm_cons_t heap[LISPBM_HEAP_SIZE];
static uint32_t gc_stack_storage[GC_STACK_SIZE];
static uint32_t print_stack_storage[PRINT_STACK_SIZE];


static lbm_tokenizer_string_state_t string_tok_state;
static lbm_tokenizer_char_stream_t string_tok;


void done_callback(eval_context_t *ctx) {

  static char print_output[1024];
  
  lbm_cid cid = ctx->id;
  lbm_value t = ctx->r;

  int print_ret = lbm_print_value(print_output, 1024, t);

  if (print_ret >= 0) {
    usb_printf("<< Context %d finished with value %s >>\r\n# ", cid, print_output);
  } else {
    usb_printf("<< Context %d finished with value %s >>\r\n# ", cid, print_output);
  }
}


uint32_t timestamp_callback(void) {
  return (1000000 / sys_clock_hw_cycles_per_sec()) *  k_cycle_get_32();
}

void sleep_callback(uint32_t us) {
  k_sleep(K_USEC(us));
}


void main(void)
{
  int res = 0;
  start_usb_cdc_thread();
  
  k_sleep(K_SECONDS(5));

  lbm_heap_state_t heap_state;

  lbm_init(heap, LISPBM_HEAP_SIZE,
           gc_stack_storage, GC_STACK_SIZE,
           memory, LBM_MEMORY_SIZE_8K,
           bitmap, LBM_MEMORY_BITMAP_SIZE_8K,
           print_stack_storage, PRINT_STACK_SIZE
           );

  lbm_set_ctx_done_callback(done_callback);
  lbm_set_timestamp_us_callback(timestamp_callback);
  lbm_set_usleep_callback(sleep_callback);


  lbm_pause_eval();
  while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
    k_sleep(K_MSEC(1));
  }
  prelude_load(&string_tok_state,
               &string_tok);

  lbm_cid cid = lbm_load_and_eval_program(&string_tok);
  
  lbm_continue_eval();
  lbm_wait_ctx((lbm_cid)cid);

  usb_printf("Lisp REPL started (ZephyrOS)!\r\n");
	
  while (1) {
    k_sleep(K_MSEC(100));
    usb_printf("# ");
    memset(str,0,LISPBM_INPUT_BUFFER_SIZE);
    memset(outbuf,0, LISPBM_OUTPUT_BUFFER_SIZE);

    /* While loop handles empty lines */
    while ( usb_readl(str, LISPBM_INPUT_BUFFER_SIZE) == 0) {
      k_sleep(K_MSEC(100));
    }
    
    usb_printf("\r\n");

    if (strncmp(str, ":info", 5) == 0) {
      usb_printf("##(REPL - ZephyrOS)#########################################\r\n");
      usb_printf("Used cons cells: %lu \r\n", LISPBM_HEAP_SIZE - lbm_heap_num_free());
      res = lbm_print_value(outbuf, LISPBM_OUTPUT_BUFFER_SIZE, lbm_get_env());
      if (res >= 0) {
	usb_printf("ENV: %s \r\n", outbuf);
      } else {
	usb_printf("%s\n", outbuf);
      }
      lbm_get_heap_state(&heap_state);
      usb_printf("GC counter: %lu\r\n", heap_state.gc_num);
      usb_printf("Recovered: %lu\r\n", heap_state.gc_recovered);
      usb_printf("Marked: %lu\r\n", heap_state.gc_marked);
      usb_printf("Free cons cells: %lu\r\n", lbm_heap_num_free());
      usb_printf("############################################################\r\n");
      memset(outbuf,0, LISPBM_OUTPUT_BUFFER_SIZE);
    } else if (strncmp(str, ":quit", 5) == 0) {
      break;
    } else {

      if (strlen(str) == 0) {
        continue;
      }

      lbm_pause_eval();
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        k_sleep(K_MSEC(1));
      }
      lbm_create_char_stream_from_string(&string_tok_state,
                                         &string_tok,
                                         str);
      
      lbm_cid cid = lbm_load_and_eval_expression(&string_tok);
      
      lbm_continue_eval();

      lbm_wait_ctx((lbm_cid)cid);
    }
  }
}
