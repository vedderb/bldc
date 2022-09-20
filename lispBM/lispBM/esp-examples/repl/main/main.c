/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se

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
#include <stdarg.h>
#include <ctype.h>
#include "sdkconfig.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_chip_info.h"
#include "esp_spi_flash.h"

#include "lispbm.h"
#include "lbm_llama_ascii.h"
#include "lbm_version.h"


#include "extensions/array_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"

#include "lbm_custom_type.h"


int inputline(char *buffer, int size) {
  int n = 0;
  unsigned char c;

  for (n = 0; n < size - 1; n++) {

    c = getchar();
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      putchar(0x8); /* output backspace character */
      putchar(' ');
      putchar(0x8);
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        putchar(c);
        buffer[n] = c;
      } else {
    	  // Yield or get a complaint from the watchdog.
    	  // No character received or unprintable.
    	  vTaskDelay(1);
    	  n -= 1;
      }
      break;
    }
  }
  buffer[size - 1] = 0;
  return 0; // Filled up buffer without reading a linebreak
}

#define EVAL_CPS_STACK_SIZE 256
#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256
#define EXTENSION_STORAGE_SIZE 256
#define VARIABLE_STORAGE_SIZE 256
#define WAIT_TIMEOUT 2500
#define STR_SIZE 1024
#define HEAP_SIZE 2048
#define PRINT_SIZE 1024

lbm_uint gc_stack_storage[GC_STACK_SIZE];
lbm_uint print_stack_storage[PRINT_STACK_SIZE];
extension_fptr extension_storage[EXTENSION_STORAGE_SIZE];
lbm_value variable_storage[VARIABLE_STORAGE_SIZE];

static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));

static lbm_uint memory[LBM_MEMORY_SIZE_8K];
static lbm_uint bitmap[LBM_MEMORY_BITMAP_SIZE_8K];

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t string_tok;

static char print_output[PRINT_SIZE];

void eval_thd_wrapper(void *v) {
  lbm_run_eval();
}

void done_callback(eval_context_t *ctx) {

	char *output = print_output;

	lbm_cid cid = ctx->id;
	lbm_value t = ctx->r;

	int print_ret = lbm_print_value(output, PRINT_SIZE, t);

	if (print_ret >= 0) {
		printf("<< Context %d finished with value %s >>\r\n", cid, output);
	} else {
		printf("<< Context %d finished with value %s >>\r\n", cid, output);
	}
}

// On FreeRTOS the systick freq can be set to at most 1KHz.
// At 1KHz, 1 tick is 1000 us.
uint32_t timestamp_callback(void) {
	TickType_t t = xTaskGetTickCount();
	return (uint32_t) (1000 * t);
}

void sleep_callback(uint32_t us) {
	uint32_t ticks = us / 1000;
	if (ticks == 0) vTaskDelay(1);//taskYIELD();
	else vTaskDelay(ticks);
}

lbm_value ext_print(lbm_value *args, lbm_uint argn) {

  char *output = print_output;

  for (lbm_uint i = 0; i < argn; i ++) {
    lbm_value t = args[i];

    if (lbm_is_ptr(t) && lbm_type_of(t) == LBM_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
      switch (array->elt_type){
      case LBM_TYPE_CHAR:
        printf("%s", (char*)array->data);
        break;
      default:
        return lbm_enc_sym(SYM_NIL);
        break;
      }
    } else if (lbm_type_of(t) == LBM_TYPE_CHAR) {
      if (lbm_dec_char(t) =='\n') {
        printf("\r\n");
      } else {
        printf("%c", lbm_dec_char(t));
      }
    }  else {
      lbm_print_value(output, 1024, t);
      printf("%s", output);
    }
  }
  return lbm_enc_sym(SYM_TRUE);
}

static char outbuf[1024];

void print_ctx_info(eval_context_t *ctx, void *arg1, void *arg2) {
  (void)arg2;
  lbm_print_value(outbuf, 1024, ctx->r);
  printf("%s %x %u %u %s\r\n", (char*)arg1, (uint32_t)ctx, ctx->id, ctx->K.sp, outbuf);
}

void ctx_exists(eval_context_t *ctx, void *arg1, void *arg2) {

  lbm_cid id = *(lbm_cid*)arg1;
  bool *exists = (bool*)arg2;

  if (ctx->id == id) {
    *exists = true;
  }
}

static char str[1024];
static char outbuf[1024];

void app_main(void)
{

	int res = 0;
	lbm_heap_state_t heap_state;

	vTaskDelay(1000);

	if (!lbm_init(heap, HEAP_SIZE,
			gc_stack_storage, GC_STACK_SIZE,
			memory, LBM_MEMORY_SIZE_8K,
			bitmap, LBM_MEMORY_BITMAP_SIZE_8K,
			print_stack_storage, PRINT_STACK_SIZE,
			extension_storage, EXTENSION_STORAGE_SIZE)) {
		printf("LispBM Init failed.\r\n");
		return;
	}
	printf("LispBM Initialized\r\n");

	lbm_set_ctx_done_callback(done_callback);
	lbm_set_timestamp_us_callback(timestamp_callback);
	lbm_set_usleep_callback(sleep_callback);

	res = lbm_add_extension("print", ext_print);
	if (res)
		printf("Extension added.\r\n");
	else
		printf("Error adding extension.\r\n");


	TaskHandle_t eval_thd = NULL;
	BaseType_t status = xTaskCreate(eval_thd_wrapper,
			"eval",
			4096,
			NULL,
			2,
			&eval_thd
	);

	if( status == pdPASS ) {
		printf("Evaluator thread started\r\n");
	}

	printf("LispBM Version %d.%d.%d\r\n", LBM_MAJOR_VERSION, LBM_MINOR_VERSION, LBM_PATCH_VERSION);
	printf("Lisp REPL started (ESP32C3)\r\n");

	while (1) {
		printf("# ");
		memset(str,0,1024);
		memset(outbuf,0, 1024);
		inputline(str, 1024);
		printf("\r\n");
		if (strncmp(str, ":info", 5) == 0) {
			printf("------------------------------------------------------------\r\n");
			printf("Used cons cells: %u \r\n", HEAP_SIZE - lbm_heap_num_free());
			printf("Free cons cells: %u\r\n", lbm_heap_num_free());
			lbm_get_heap_state(&heap_state);
			printf("GC counter: %u\r\n", heap_state.gc_num);
			printf("Recovered: %u\r\n", heap_state.gc_recovered);
			printf("Marked: %u\r\n", heap_state.gc_marked);

			printf("Array and symbol string memory:\r\n");
			printf("  Size: %u 32Bit words\r\n", lbm_memory_num_words());
			printf("  Free: %u 32Bit words\r\n", lbm_memory_num_free());
			printf("------------------------------------------------------------\r\n");
			memset(outbuf,0, 1024);
		} else if (strncmp(str, ":env", 4) == 0) {
			lbm_value curr = *lbm_get_env_ptr();
			printf("Environment:\r\n");
			while (lbm_type_of(curr) == LBM_TYPE_CONS) {
				res = lbm_print_value(outbuf,1024, lbm_car(curr));
				curr = lbm_cdr(curr);

				printf("  %s \r\n", outbuf);
			}
		} else if (strncmp(str, ":ctxs", 5) == 0) {
			lbm_running_iterator(print_ctx_info, "RUNNABLE", NULL);
			lbm_blocked_iterator(print_ctx_info, "BLOCKED", NULL);
			lbm_done_iterator   (print_ctx_info, "DONE", NULL);
		} else if (strncmp(str, ":wait", 5) == 0) {
			int id = atoi(str + 5);
			bool exists = false;
			lbm_done_iterator(ctx_exists, (void*)&id, (void*)&exists);
			if (exists) {
				lbm_wait_ctx((lbm_cid)id, WAIT_TIMEOUT);
			}
		} else if (strncmp(str, ":pause", 6) == 0) {
			lbm_pause_eval();
			while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
				sleep_callback(10);
			}
			printf("Evaluator paused\r\nEnter command :continue to unpause or :step to perform single stepping\r\n");
		} else if (strncmp(str, ":continue", 9) == 0) {
			lbm_continue_eval();
		} else if (strncmp(str, ":step", 5) == 0) {
			lbm_step_eval();
			while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
				sleep_callback(1);
			}
			printf("Evaluator paused\r\nEnter command :continue to unpause or :step to perform single stepping\r\n");
		} else if (strncmp(str, ":reset", 6) == 0) {
			lbm_pause_eval();
			while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
				sleep_callback(1);
			}

			if (!lbm_init(heap, HEAP_SIZE,
					gc_stack_storage, GC_STACK_SIZE,
					memory, LBM_MEMORY_SIZE_8K,
					bitmap, LBM_MEMORY_BITMAP_SIZE_8K,
					print_stack_storage, PRINT_STACK_SIZE,
					extension_storage, EXTENSION_STORAGE_SIZE)) {
				printf("LispBM Init failed.\r\n");
				return;
			}
			lbm_add_extension("print", ext_print);

		} else if (strncmp(str, ":quit", 5) == 0) {

			break;
		} else {

			if (strlen(str) == 0) {
				continue;
			}
			/* Get exclusive access to the heap */
			lbm_pause_eval();
			while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
				sleep_callback(10);
			}

			lbm_create_string_char_channel(&string_tok_state,
                                                       &string_tok,
                                                       str);

			lbm_cid cid = lbm_load_and_eval_expression(&string_tok);

			lbm_continue_eval();

			printf("started ctx: %u\r\n", cid);
			lbm_wait_ctx((lbm_cid)cid, WAIT_TIMEOUT);
		}
	}
}
