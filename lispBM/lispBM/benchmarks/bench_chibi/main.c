/*
    Copyright 2019, 2022  Joel Svensson        svenssonjoel@yahoo.se
                    2022  Benjamin Vedder

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
#include "ctype.h"

#include "ch.h"
#include "hal.h"
#include "chvt.h"
#include "chtime.h"

#include "usbcfg.h"
#include "chprintf.h"

#include "lispbm.h"

/*
   (float)chVTTimeElapsedSinceX(x) / (float)CH_CFG_ST_FREQUENCY)

   systime_t x = chVTGetSystemTimeX()
*/

#define WAIT_TIMEOUT 2500

#define EVAL_WA_SIZE THD_WORKING_AREA_SIZE(1024)
#define EVAL_CPS_STACK_SIZE 256
#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256
#define HEAP_SIZE 4096
#define EXTENSION_STORAGE_SIZE 256

lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];
uint32_t print_stack_storage[PRINT_STACK_SIZE];

static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));

static uint32_t memory_array[LBM_MEMORY_SIZE_8K];
static uint32_t bitmap_array[LBM_MEMORY_BITMAP_SIZE_8K];

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t string_tok;

BaseSequentialStream *chp = NULL;

int inputline(BaseSequentialStream *chp, char *buffer, int size) {
  int n = 0;
  unsigned char c;
  for (n = 0; n < size - 1; n++) {

    c = streamGet(chp);
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      streamPut(chp,0x8); /* output backspace character */
      streamPut(chp,' ');
      streamPut(chp,0x8);
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        streamPut(chp,c);
        buffer[n] = c;
      } else {
        n -= 1;
      }
      break;
    }
  }
  buffer[size - 1] = 0;
  return 0; // Filled up buffer without reading a linebreak
}

static char print_output[1024];

void done_callback(eval_context_t *ctx) {

  char *output = print_output;

  lbm_cid cid = ctx->id;
  lbm_value t = ctx->r;

  int print_ret = lbm_print_value(output, 1024, t);

  if (print_ret >= 0) {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, output);
  } else {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, output);
  }
}

uint32_t timestamp_callback(void) {
  systime_t t = chVTGetSystemTime();
  uint32_t ts = (uint32_t) ((1000000 / CH_CFG_ST_FREQUENCY) * t);
  //chprintf(chp,"timestamp %d\r\n ", ts);
  return ts;
}

void sleep_callback(uint32_t us) {
  chThdSleepMicroseconds(us);
}

static THD_FUNCTION(eval, arg) {
  (void) arg;
  lbm_run_eval();
}

/* ext_print is atomic from the point of view of the lisp RTS */
lbm_value ext_print(lbm_value *args, lbm_uint argn) {

  char *output = print_output;

  for (lbm_uint i = 0; i < argn; i ++) {
    lbm_value t = args[i];

    if (lbm_is_ptr(t) && lbm_type_of(t) == LBM_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
      chprintf(chp,"%s", (char*)array + 8);
    } else if (lbm_type_of(t) == LBM_TYPE_CHAR) {
      if (lbm_dec_char(t) =='\n') {
        chprintf(chp, "\r\n");
      } else {
        chprintf(chp,"%c", lbm_dec_char(t));
      }
    }  else {
      lbm_print_value(output, 1024, t);
      chprintf(chp,"%s", output);
    }
  }
  return lbm_enc_sym(SYM_TRUE);
}

static char str[1024];
static char outbuf[1024];
#define FILE_LEN 8192
static char file_buffer[FILE_LEN];

void print_ctx_info(eval_context_t *ctx, void *arg1, void *arg2) {
  (void)arg2;
  lbm_print_value(outbuf, 1024, ctx->r);
  chprintf(chp, "%s %x %u %u %s\r\n", (char*)arg1, (uint32_t)ctx, ctx->id, ctx->K.sp, outbuf);
}

void ctx_exists(eval_context_t *ctx, void *arg1, void *arg2) {

  lbm_cid id = *(lbm_cid*)arg1;
  bool *exists = (bool*)arg2;

  if (ctx->id == id) {
    *exists = true;
  }
}
int main(void) {
  halInit();
  chSysInit();

  sduObjectInit(&SDU1);
  sduStart(&SDU1, &serusbcfg);

  /*
   * Activates the USB driver and then the USB bus pull-up on D+.
   * Note, a delay is inserted in order to not have to disconnect the cable
   * after a reset.
   */
  usbDisconnectBus(serusbcfg.usbp);
  chThdSleepMilliseconds(1500);
  usbStart(serusbcfg.usbp, &usbcfg);
  usbConnectBus(serusbcfg.usbp);

  chp = (BaseSequentialStream*)&SDU1;

  size_t len = 1024;

  int res = 0;

  lbm_heap_state_t heap_state;

  chThdSleepMilliseconds(2000);

  if (!lbm_init(heap, HEAP_SIZE,
                memory_array, LBM_MEMORY_SIZE_8K,
                bitmap_array, LBM_MEMORY_BITMAP_SIZE_8K,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {

    chprintf(chp,"LispBM Init failed.\r\n");
    return 0;
  }

  lbm_set_ctx_done_callback(done_callback);
  lbm_set_timestamp_us_callback(timestamp_callback);
  lbm_set_usleep_callback(sleep_callback);

  res = lbm_add_extension("print", ext_print);
  if (res)
    chprintf(chp,"Extension added.\r\n");
  else
    chprintf(chp,"Error adding extension.\r\n");

  thread_t *t = chThdCreateFromHeap(NULL, EVAL_WA_SIZE,
                                    "eval", NORMALPRIO+1,
                                    eval, (void *)NULL);

  if (!t) {
    chprintf(chp,"Error starting evaluator thread.\r\n");
    return 0;
  }
  chprintf(chp,"Lisp REPL started (ChibiOS)!\r\n");

  while (1) {
    chprintf(chp,"# ");
    memset(str,0,len);
    memset(outbuf,0, 1024);
    inputline(chp,str, len);
    chprintf(chp,"\r\n");

    if (strncmp(str, ":info", 5) == 0) {
      chprintf(chp,"------------------------------------------------------------\r\n");
      chprintf(chp,"Used cons cells: %lu \r\n", HEAP_SIZE - lbm_heap_num_free());
      chprintf(chp,"Free cons cells: %lu\r\n", lbm_heap_num_free());
      lbm_get_heap_state(&heap_state);
      chprintf(chp,"GC counter: %lu\r\n", heap_state.gc_num);
      chprintf(chp,"Recovered: %lu\r\n", heap_state.gc_recovered);
      chprintf(chp,"Marked: %lu\r\n", heap_state.gc_marked);

      chprintf(chp,"Array and symbol string memory:\r\n");
      chprintf(chp,"  Size: %u 32Bit words\r\n", lbm_memory_num_words());
      chprintf(chp,"  Free: %u 32Bit words\r\n", lbm_memory_num_free());
      chprintf(chp,"------------------------------------------------------------\r\n");
      memset(outbuf,0, 1024);
    } else if (strncmp(str, ":env", 4) == 0) {
      lbm_value *glob_env = lbm_get_global_env();
      for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
        lbm_value curr = glob_env[i];
        chprintf(chp,"Global Environment [%d]:\r\n", i);
        while (lbm_type_of(curr) == LBM_TYPE_CONS) {
          res = lbm_print_value(outbuf,1024, lbm_car(curr));
          curr = lbm_cdr(curr);

          chprintf(chp,"  %s \r\n", outbuf);
        }
      }
    } else if (strncmp(str, ":threads", 8) == 0) {
      thread_t *tp;
      static const char *states[] = {CH_STATE_NAMES};
      chprintf(chp, "    addr prio refs     state           name time    \r\n");
      chprintf(chp, "-------------------------------------------------------------------\r\n");
      tp = chRegFirstThread();
      do {
        chprintf(chp, "%.8lx %4lu %4lu %9s %14s %lu (%.1f %%)\r\n",
                 (uint32_t)tp,
                 (uint32_t)tp->prio, (uint32_t)(tp->refs - 1),
                 states[tp->state], tp->name, (uint32_t)(tp->time - tp->time_last),
                 (double)(100.0 * (float)(tp->time - tp->time_last) / (float)(chVTGetSystemTimeX() - tp->time_last)));
        tp->time_last = tp->time;
        tp = chRegNextThread(tp);
      } while (tp != NULL);
    } else if (strncmp(str, ":mem", 4) == 0) {
      size_t n, size, sizel;
      n = chHeapStatus(NULL, &size, &sizel);
      chprintf(chp, "core free memory  : %u bytes\r\n", chCoreGetStatusX());
      chprintf(chp, "heap fragments    : %u\r\n", n);
      chprintf(chp, "heap free largest : %u bytes\r\n", sizel);
      chprintf(chp, "heap free total   : %u bytes\n\r\n", size);
    } else if (strncmp(str, ":ctxs", 5) == 0) {
      lbm_running_iterator(print_ctx_info, "RUNNABLE", NULL);
      lbm_blocked_iterator(print_ctx_info, "BLOCKED", NULL);
    }  else if (strncmp(str, ":pause", 6) == 0) {
      lbm_pause_eval();
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }
      chprintf(chp, "Evaluator paused\r\nEnter command :continue to unpause\r\n");
    } else if (strncmp(str, ":continue", 9) == 0) {
      lbm_continue_eval();
    } else if (strncmp(str, ":reset", 6) == 0) {
      lbm_pause_eval();
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        chThdSleepMilliseconds(1);
      }

      lbm_init(heap, HEAP_SIZE,
               memory_array, LBM_MEMORY_SIZE_8K,
               bitmap_array, LBM_MEMORY_BITMAP_SIZE_8K,
               GC_STACK_SIZE,
               PRINT_STACK_SIZE,
               extensions,
               EXTENSION_STORAGE_SIZE);

      lbm_add_extension("print", ext_print);

    } else if (strncmp(str, ":quit", 5) == 0) {

      break;
    } else if (strncmp(str, ":read", 5) == 0) {
      memset(file_buffer, 0, FILE_LEN);
      bool done = false;
      int c;

      for (int i = 0; i < FILE_LEN; i ++) {
        c = streamGet(chp);

        if (c == 4 || c == 26 || c == STM_RESET) {
          done = true;
          break;
        }
        file_buffer[i] = (char)c;
      }

      chprintf(chp, "%s\r\n", file_buffer);
      chprintf(chp, "received %d bytes\r\n", strlen(file_buffer));

      if (done) {

        /* Get exclusive access to the heap */
        lbm_pause_eval();
        while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
          sleep_callback(10);
        }
        lbm_create_string_char_channel(&string_tok_state,
                                       &string_tok,
                                       file_buffer);

        systime_t t_load = chVTGetSystemTimeX();

        lbm_cid cid = lbm_load_and_define_program(&string_tok, "prg");

        lbm_continue_eval();
        lbm_wait_ctx((lbm_cid)cid, WAIT_TIMEOUT);

        systime_t elapsed_load = chVTTimeElapsedSinceX(t_load);

        lbm_pause_eval();
        while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
          sleep_callback(10);
        }

        //systime_t t_eval = chVTGetSystemTimeX();
        cid = lbm_eval_defined_program("prg");

        lbm_continue_eval();
        lbm_wait_ctx((lbm_cid)cid,WAIT_TIMEOUT);

        systime_t elapsed_eval = chVTTimeElapsedSinceX(t_load);

        chprintf(chp, "time eval: %fs\r\n", (float)elapsed_eval / (float)CH_CFG_ST_FREQUENCY);
        chprintf(chp, "time load: %fs\r\n", (float)elapsed_load / (float)CH_CFG_ST_FREQUENCY);

        lbm_get_heap_state(&heap_state);

        chprintf(chp, "gc invocations: %d\r\n", heap_state.gc_num);
        chprintf(chp, "gc time avg: %f\r\n", 0.0);
        chprintf(chp, "gc min time: %u\r\n", 0);
        chprintf(chp, "gc max time: %u\r\n", 0);
        chprintf(chp, "gc least free: %u\r\n", heap_state.gc_least_free);
      }
    } else {

      if (strlen(str) == 0) {
        continue;
      }

      //lbm_value t;

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

      //printf("started ctx: %ld\n", cid);
      lbm_wait_ctx((lbm_cid)cid, WAIT_TIMEOUT);
    }
  }
}

