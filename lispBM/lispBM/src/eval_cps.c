/*
    Copyright 2018, 2020, 2021, 2022 Joel Svensson    svenssonjoel@yahoo.se

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

#include <lbm_memory.h>
#include <lbm_types.h>
#include "symrepr.h"
#include "heap.h"
#include "env.h"
#include "eval_cps.h"
#include "stack.h"
#include "fundamental.h"
#include "extensions.h"
#include "exp_kind.h"
#include "tokpar.h"
#include "qq_expand.h"
#include "lbm_variables.h"
#include "lbm_channel.h"
#include "print.h"
#include "platform_mutex.h"

#ifdef VISUALIZE_HEAP
#include "heap_vis.h"
#endif

#define ILLEGAL_CONT      ((0 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define DONE              ((1 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define SET_GLOBAL_ENV    ((2 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define BIND_TO_KEY_REST  ((3 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define IF                ((4 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define PROGN_REST        ((5 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define APPLICATION       ((6 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define APPLICATION_ARGS  ((7 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define AND               ((8 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define OR                ((9 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define WAIT              ((10 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define MATCH             ((11 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define MATCH_MANY        ((12 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ              ((13 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define APPLICATION_START ((14 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define EVAL_R            ((15 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define SET_VARIABLE      ((16 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define RESUME            ((17 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define EXPAND_MACRO      ((18 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define CLOSURE_ARGS      ((19 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define CLOSURE_APP       ((20 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define NAMESPACE_POP     ((21 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define TERMINATE_COLON   ((22 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define EXIT_ATOMIC       ((23 << LBM_VAL_SHIFT) | LBM_TYPE_U)

#define READ_NEXT_TOKEN       ((31 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_APPEND_CONTINUE  ((32 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_EXPECT_CLOSEPAR  ((33 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_DOT_TERMINATE    ((34 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_DONE             ((36 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_QUOTE_RESULT     ((37 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_BACKQUOTE_RESULT ((38 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_COMMAAT_RESULT   ((39 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_COMMA_RESULT     ((40 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_TERMINATE_COLON  ((41 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_START_ARRAY      ((42 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_APPEND_ARRAY     ((43 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_CHECK_COLON      ((44 << LBM_VAL_SHIFT) | LBM_TYPE_U)


static const char* parse_error_eof = "End of parse stream";
static const char* parse_error_token = "Malformed token";
static const char* parse_error_dot = "Incorrect usage of '.'";
static const char* parse_error_close = "Expected closing parenthesis";

#define CHECK_STACK(x)                          \
  if (!(x)) {                                   \
    error_ctx(ENC_SYM_STACK_ERROR);             \
    return;                                     \
  }

#define WITH_GC(y, x, remember1,remember2)      \
  (y) = (x);                                    \
  if (lbm_is_symbol_merror((y))) {              \
    gc(remember1, remember2);                   \
    (y) = (x);                                  \
    if (lbm_is_symbol_merror((y))) {            \
      ctx_running->done = true;                 \
      error_ctx(ENC_SYM_MERROR);       \
      return;                                   \
    }                                           \
    /* continue executing statements below */   \
  }

#define PRELIMINARY_GC_MEASURE 30

static int gc(lbm_value, lbm_value);
static void error_ctx(lbm_value);
static eval_context_t *ctx_running = NULL;

static inline lbm_value cons_with_gc(lbm_value head, lbm_value tail, lbm_value remember) {
  lbm_value res = lbm_cons(head, tail);
  if (lbm_is_symbol_merror(res)) {
    gc(remember, ENC_SYM_NIL);
    res = lbm_cons(head, tail);
    if (lbm_is_symbol_merror(res)) {
        ctx_running->done = true;
        error_ctx(ENC_SYM_MERROR);
    }
  }
  return res;
}

#define CONS_WITH_GC(res, h, t, r)              \
  (res) = cons_with_gc(h,t,r);                  \
  if (lbm_is_symbol_merror(res)) {              \
    return;                                     \
  }

#define DEFAULT_SLEEP_US  1000

#define EVAL_CPS_DEFAULT_STACK_SIZE 256

/* 768 us -> ~128000 "ticks" at 168MHz I assume this means also roughly 128000 instructions */
#define EVAL_CPS_QUANTA_US 768
#define EVAL_CPS_WAIT_US   1536
#define EVAL_CPS_MIN_SLEEP 200

#define EVAL_STEPS_QUOTA   10
static volatile uint32_t eval_steps_refill = EVAL_STEPS_QUOTA;
static uint32_t eval_steps_quota = EVAL_STEPS_QUOTA;

void lbm_set_eval_step_quota(uint32_t quota) {
  eval_steps_refill = quota;
}

static uint32_t eval_cps_run_state = EVAL_CPS_STATE_INIT;
static volatile uint32_t eval_cps_next_state = EVAL_CPS_STATE_INIT;
static volatile uint32_t eval_cps_next_state_arg = 0;

/*
   On ChibiOs the CH_CFG_ST_FREQUENCY setting in chconf.h sets the
   resolution of the timer used for sleep operations.  If this is set
   to 10KHz the resolution is 100us.

   The CH_CFG_ST_TIMEDELTA specifies the minimum number of ticks that
   can be safely specified in a timeout directive (wonder if that
   means sleep-period). The timedelta is set to 2.

   If I have understood these correctly it means that the minimum
   sleep duration possible is 2 * 100us = 200us.
*/

static bool          eval_running = false;
static volatile bool blocking_extension = false;
static bool          is_atomic = false;

typedef struct {
  eval_context_t *first;
  eval_context_t *last;
} eval_context_queue_t;


/* Callbacks and task queue */
static eval_context_queue_t blocked  = {NULL, NULL};
static eval_context_queue_t sleeping = {NULL, NULL};
static eval_context_queue_t queue    = {NULL, NULL};
static eval_context_queue_t done     = {NULL, NULL};

/* one mutex for all queue operations */
mutex_t qmutex;

static void usleep_nonsense(uint32_t us) {
  (void) us;
}

static void (*usleep_callback)(uint32_t) = usleep_nonsense;
static uint32_t (*timestamp_us_callback)(void) = NULL;
static void (*ctx_done_callback)(eval_context_t *) = NULL;
static int (*printf_callback)(const char *, ...) = NULL;
static bool (*dynamic_load_callback)(const char *, const char **) = NULL;
static void (*reader_done_callback)(lbm_cid cid) = NULL;

static bool lbm_verbose = false;

void lbm_toggle_verbose(void) {
  lbm_verbose = !lbm_verbose;
}

void lbm_set_verbose(bool verbose) {
  lbm_verbose = verbose;
}

void lbm_set_usleep_callback(void (*fptr)(uint32_t)) {
  usleep_callback = fptr;
}

void lbm_set_timestamp_us_callback(uint32_t (*fptr)(void)) {
  timestamp_us_callback = fptr;
}

void lbm_set_ctx_done_callback(void (*fptr)(eval_context_t *)) {
  ctx_done_callback = fptr;
}

void lbm_set_printf_callback(int (*fptr)(const char*, ...)){
  printf_callback = fptr;
}

void lbm_set_dynamic_load_callback(bool (*fptr)(const char *, const char **)) {
  dynamic_load_callback = fptr;
}

void lbm_set_reader_done_callback(void (*fptr)(lbm_cid)) {
  reader_done_callback = fptr;
}

lbm_cid lbm_get_current_cid(void) {
  if (ctx_running)
    return ctx_running->id;
  else
    return -1;
}

void done_reading(lbm_cid cid) {
  if (reader_done_callback != NULL) {
    reader_done_callback(cid);
  }
}

/****************************************************/
/* Error message creation                           */

#define ERROR_MESSAGE_BUFFER_SIZE_BYTES 256

void print_environments(char *buf, unsigned int size) {

  lbm_value curr_g = lbm_get_env();
  lbm_value curr_l = ctx_running->curr_env;

  printf_callback("\tCurrent local environment:\n");
  while (lbm_type_of(curr_l) == LBM_TYPE_CONS) {

    lbm_print_value(buf, (size/2) - 1,lbm_car(lbm_car(curr_l)));
    lbm_print_value(buf + (size/2),size/2, lbm_cdr(lbm_car(curr_l)));
    printf_callback("\t%s = %s\n", buf, buf+(size/2));
    curr_l = lbm_cdr(curr_l);
  }

  printf_callback("\n\n");
  printf_callback("\tCurrent global environment:\n");
  while (lbm_type_of(curr_g) == LBM_TYPE_CONS) {

    lbm_print_value(buf, (size/2) - 1,lbm_car(lbm_car(curr_g)));
    lbm_print_value(buf + (size/2),size/2, lbm_cdr(lbm_car(curr_g)));
    printf_callback("\t%s = %s\n", buf, buf+(size/2));
    curr_g = lbm_cdr(curr_g);
  }
}


void print_error_message(lbm_value error, unsigned int row, unsigned int col) {
  if (!printf_callback) return;

  /* try to allocate a lbm_print_value buffer on the lbm_memory */
  lbm_uint* buf32 = lbm_memory_allocate(ERROR_MESSAGE_BUFFER_SIZE_BYTES / (sizeof(lbm_uint)));
  if (!buf32) {
    printf_callback("Error: Not enough free memory to create a human readable error message\n");
    return;
  }
  char *buf = (char*)buf32;

  lbm_print_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES, error);
  printf_callback("***\tError:\t%s\n", buf);

  if (lbm_is_symbol(error) &&
      error == ENC_SYM_RERROR) {
    printf_callback("***\t\tLine: %u\n", row);
    printf_callback("***\t\tColumn: %u\n", col);
  }

  printf_callback("\n");

  if (ctx_running->error_reason) {
    printf_callback("Reason:\n\t%s\n\n", ctx_running->error_reason);
  }
  if (lbm_verbose) {
    lbm_print_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES, ctx_running->curr_exp);
    printf_callback("\tWhile evaluating: %s\n", buf);
    printf_callback("\tIn context: %d\n", ctx_running->id);
    printf_callback("\tCurrent intermediate result: %s\n\n", buf);

    print_environments(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES);
    printf_callback("\n\n");

    printf_callback("\tStack:\n");
    for (unsigned int i = 0; i < ctx_running->K.sp; i ++) {
      lbm_print_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES, ctx_running->K.data[i]);
      printf_callback("\t\t%s\n", buf);
    }
  }
  lbm_memory_free(buf32);
}

/****************************************************/
/* Tokenizing and parsing                           */

bool create_string_channel(char *str, lbm_value *res) {

  lbm_char_channel_t *chan = NULL;
  lbm_string_channel_state_t *st = NULL;

  st = (lbm_string_channel_state_t*)lbm_memory_allocate(sizeof(lbm_string_channel_state_t) / sizeof(lbm_uint) +1);
  if (st == NULL) {
    return false;
  }
  chan = (lbm_char_channel_t*)lbm_memory_allocate(sizeof(lbm_char_channel_t) / sizeof(lbm_uint) + 1);
  if (chan == NULL) {
    lbm_memory_free((lbm_uint*)st);
    return false;
  }

  lbm_value cell = lbm_heap_allocate_cell(LBM_TYPE_CONS);
  if (lbm_type_of(cell) == LBM_TYPE_SYMBOL) {
    lbm_memory_free((lbm_uint*)st);
    lbm_memory_free((lbm_uint*)chan);
    return false;
  }

  lbm_create_string_char_channel(st, chan, str);

  lbm_set_car(cell, (lbm_uint)chan);
  lbm_set_cdr(cell, lbm_enc_sym(SYM_CHANNEL_TYPE));
  cell = lbm_set_ptr_type(cell, LBM_TYPE_CHANNEL);
  *res = cell;
  return true;
}

bool lift_char_channel(lbm_char_channel_t *chan , lbm_value *res) {
 lbm_value cell = lbm_heap_allocate_cell(LBM_TYPE_CONS);
  if (lbm_type_of(cell) == LBM_TYPE_SYMBOL) {
    return false;
  }

  lbm_set_car(cell, (lbm_uint)chan);
  lbm_set_cdr(cell, lbm_enc_sym(SYM_CHANNEL_TYPE));
  cell = lbm_set_ptr_type(cell, LBM_TYPE_CHANNEL);
  *res = cell;
  return true;
}




/****************************************************/
/* Queue functions                                  */

static void queue_iterator(eval_context_queue_t *q, ctx_fun f, void *arg1, void *arg2) {
  mutex_lock(&qmutex);
  eval_context_t *curr;
  curr = q->first;

  while (curr != NULL) {
    f(curr, arg1, arg2);
    curr = curr->next;
  }
  mutex_unlock(&qmutex);
}

void lbm_running_iterator(ctx_fun f, void *arg1, void *arg2){
  queue_iterator(&queue, f, arg1, arg2);
}

void lbm_blocked_iterator(ctx_fun f, void *arg1, void *arg2){
  queue_iterator(&blocked, f, arg1, arg2);
}

void lbm_done_iterator(ctx_fun f, void *arg1, void *arg2){
  queue_iterator(&done, f, arg1, arg2);
}

void lbm_sleeping_iterator(ctx_fun f, void *arg1, void *arg2){
  queue_iterator(&sleeping, f, arg1, arg2);
}

static void enqueue_ctx(eval_context_queue_t *q, eval_context_t *ctx) {
  mutex_lock(&qmutex);
  if (q->last == NULL) {
    ctx->prev = NULL;
    ctx->next = NULL;
    q->first = ctx;
    q->last  = ctx;
  } else {
    ctx->prev = q->last;
    ctx->next = NULL;
    q->last->next = ctx;
    q->last = ctx;
  }
  mutex_unlock(&qmutex);
}

static eval_context_t *enqueue_dequeue_ctx(eval_context_queue_t *q, eval_context_t *ctx) {
  mutex_lock(&qmutex);
  if (q->last == NULL) { // queue is empty, dequeue the enqueue
    mutex_unlock(&qmutex);
    return ctx;
  }

  eval_context_t *res = q->first;

  if (q->first == q->last) { // nothing in q or 1 thing
    q->first = ctx;
    q->last  = ctx;
  } else {
    q->first = q->first->next;
    q->first->prev = NULL;
    if (ctx != NULL) {
      q->last->next = ctx;
      ctx->prev = q->last;
      q->last = ctx;
    }
  }
  res->prev = NULL;
  res->next = NULL;
  mutex_unlock(&qmutex);
  return res;
}

static eval_context_t *lookup_ctx(eval_context_queue_t *q, lbm_cid cid) {

  mutex_lock(&qmutex);
  eval_context_t *curr;

  curr = q->first;
  while (curr != NULL) {
    if (curr->id == cid) {
      mutex_unlock(&qmutex);
      return curr;
    }
    curr = curr->next;
  }
  mutex_unlock(&qmutex);
  return NULL;
}

static void drop_ctx(eval_context_queue_t *q, eval_context_t *ctx) {

  mutex_lock(&qmutex);

  if (q->first == NULL || q->last == NULL) {
    if (!(q->last == NULL && q->first == NULL)) {
      /* error state that should not happen */
      mutex_unlock(&qmutex);
      return;
    }
    /* Queue is empty */
    mutex_unlock(&qmutex);
    return;
  }

  eval_context_t *curr = q->first;
  while (curr) {
    if (curr->id == ctx->id) {
      eval_context_t *tmp = curr->next;
      if (curr->prev == NULL) {
        if (curr->next == NULL) {
          q->last = NULL;
          q->first = NULL;
        } else {
          q->first = tmp;
          tmp->prev = NULL;
        }
      } else { /* curr->prev != NULL */
        if (curr->next == NULL) {
          q->last = curr->prev;
          q->last->next = NULL;
        } else {
          curr->prev->next = tmp;
          tmp->prev = curr->prev;
        }
      }
      break;
    }
    curr = curr->next;
  }
  mutex_unlock(&qmutex);
}

/* End execution of the running context and add it to the
   list of finished contexts. */
static void finish_ctx(void) {

  if (!ctx_running) {
    return;
  }

  /* Drop the continuation stack immediately to free up lbm_memory */
  lbm_stack_free(&ctx_running->K);

  if (ctx_done_callback) {
    ctx_done_callback(ctx_running);
  }
  if (lbm_memory_ptr_inside((lbm_uint*)ctx_running->error_reason)) {
    lbm_memory_free((lbm_uint*)ctx_running->error_reason);
  }

  lbm_memory_free((lbm_uint*)ctx_running);
  ctx_running = NULL;
}

static void context_exists(eval_context_t *ctx, void *cid, void *b) {
  if (ctx->id == *(lbm_cid*)cid) {
    *(bool*)b = true;
  }
}

bool lbm_wait_ctx(lbm_cid cid, lbm_uint timeout_ms) {

  bool exists;
  uint32_t i = 0;

  do {
    exists = false;
    lbm_blocked_iterator(context_exists, &cid, &exists);
    lbm_running_iterator(context_exists, &cid, &exists);
    lbm_sleeping_iterator(context_exists, &cid, &exists);

    if (ctx_running &&
        ctx_running->id == cid) {
      exists = true;
    }

    if (exists) {
       if (usleep_callback) {
         usleep_callback(1000);
       }
       if (timeout_ms > 0) i ++;
    }
  } while (exists && i < timeout_ms);

  if (exists) return false;
  return true;
}

int lbm_set_error_reason(char *error_str) {
  int r = 0;
  if (ctx_running) {
    ctx_running->error_reason = error_str;
    r = 1;
  }
  return r;
}

static void error_ctx(lbm_value err_val) {
  print_error_message(err_val, 0, 0);
  ctx_running->r = err_val;
  finish_ctx();
}

static void read_error_ctx(unsigned int row, unsigned int column) {
  print_error_message(ENC_SYM_RERROR, row, column);
  ctx_running->r = ENC_SYM_RERROR;
  finish_ctx();
}

static eval_context_t *dequeue_ctx(eval_context_queue_t *q, uint32_t *us) {
  lbm_uint min_us = DEFAULT_SLEEP_US;
  lbm_uint t_now;

  mutex_lock(&qmutex);

  if (timestamp_us_callback) {
    t_now = timestamp_us_callback();
  } else {
    t_now = 0;
  }

  eval_context_t *curr = q->first; //ctx_queue;

  while (curr != NULL) {
    lbm_uint t_diff;
    if ( curr->timestamp > t_now) {
      /* There was an overflow on the counter */
      #ifndef LBM64
      t_diff = (0xFFFFFFFF - curr->timestamp) + t_now;
      #else
      t_diff = (0xFFFFFFFFFFFFFFFF - curr->timestamp) + t_now;
      #endif
    } else {
      t_diff = t_now - curr->timestamp;
    }

    if (t_diff >= curr->sleep_us) {
      eval_context_t *result = curr;
      if (curr == q->last) {
        if (curr->prev) {
          q->last = curr->prev;
          q->last->next = NULL;
        } else {
          q->first = NULL;
          q->last = NULL;
        }
      } else if (curr->prev == NULL) {
        q->first = curr->next;
        if (q->first) {
          q->first->prev = NULL;
        }
      } else {
        curr->prev->next = curr->next;
        if (curr->next) {
          curr->next->prev = curr->prev;
        }
      }
      mutex_unlock(&qmutex);
      return result;
    }
    if (min_us > t_diff) min_us = t_diff;
    curr = curr->next;
  }
  /* ChibiOS does not like a sleep time of 0 */
  /* TODO: Make sure that does not happen. */
  *us = EVAL_CPS_MIN_SLEEP;
  mutex_unlock(&qmutex);
  return NULL;
}

static void yield_ctx(lbm_uint sleep_us) {
  if (timestamp_us_callback) {
    ctx_running->timestamp = timestamp_us_callback();
    ctx_running->sleep_us = sleep_us;
  } else {
    ctx_running->timestamp = 0;
    ctx_running->sleep_us = 0;
  }
  ctx_running->r = ENC_SYM_TRUE;
  ctx_running->app_cont = true;
  enqueue_ctx(&sleeping,ctx_running);
  ctx_running = NULL;
}

lbm_cid lbm_create_ctx(lbm_value program, lbm_value env, lbm_uint stack_size) {

  if (lbm_type_of(program) != LBM_TYPE_CONS) return -1;

  eval_context_t *ctx = NULL;
  ctx = (eval_context_t*)lbm_memory_allocate(sizeof(eval_context_t) / (sizeof(lbm_uint)));
  if (ctx == NULL) {
    gc(program,env);
    ctx = (eval_context_t*)lbm_memory_allocate(sizeof(eval_context_t) / (sizeof(lbm_uint)));
  }
  if (ctx == NULL) return -1;

  if (!lbm_stack_allocate(&ctx->K, stack_size)) {
    gc(program,env);
    if (!lbm_stack_allocate(&ctx->K, stack_size)) {
      lbm_memory_free((lbm_uint*)ctx);
      return -1;
    }
  }

  lbm_int cid = lbm_memory_address_to_ix((lbm_uint*)ctx);

  ctx->program = lbm_cdr(program);
  ctx->curr_exp = lbm_car(program);
  ctx->curr_env = env;
  ctx->r = ENC_SYM_NIL;
  ctx->error_reason = NULL;
  ctx->mailbox = ENC_SYM_NIL;
  ctx->done = false;
  ctx->app_cont = false;
  ctx->timestamp = 0;
  ctx->sleep_us = 0;
  ctx->prev = NULL;
  ctx->next = NULL;

  ctx->id = cid;

  if (!lbm_push(&ctx->K, DONE)) {
    lbm_stack_free(&ctx->K);
    lbm_memory_free((lbm_uint*)ctx);
    return -1;
  }

  enqueue_ctx(&queue,ctx);

  return ctx->id;
}

/* Advance execution to the next expression in the program */
static void advance_ctx(void) {

  if (lbm_type_of(ctx_running->program) == LBM_TYPE_CONS) {
    lbm_push(&ctx_running->K, DONE);
    ctx_running->curr_exp = lbm_car(ctx_running->program);
    ctx_running->curr_env = ENC_SYM_NIL;
    ctx_running->program = lbm_cdr(ctx_running->program);
    ctx_running->r = ENC_SYM_NIL;
    ctx_running->app_cont = false;

  } else {
    ctx_running->done = true;
    finish_ctx();
  }
}

bool lbm_unblock_ctx(lbm_cid cid, lbm_value result) {
 eval_context_t *found = NULL;

  found = lookup_ctx(&blocked, cid);

  if (found == NULL)
    return false;

  drop_ctx(&blocked,found);
  found->r = result;
  enqueue_ctx(&queue,found);
  return true;
}

void lbm_block_ctx_from_extension(void) {
  blocking_extension = true;
}

lbm_value lbm_find_receiver_and_send(lbm_cid cid, lbm_value msg) {
  eval_context_t *found = NULL;
  bool found_blocked = false;

  found = lookup_ctx(&blocked, cid);
  if (found) found_blocked = true;

  if (found == NULL) {
    found = lookup_ctx(&queue, cid);
  }

  if (found == NULL) {
    found = lookup_ctx(&sleeping, cid);
  }

  if (found) {
    lbm_value new_mailbox = lbm_cons(msg, found->mailbox);

    if (lbm_type_of(new_mailbox) == LBM_TYPE_SYMBOL) {
      return new_mailbox; /* An error symbol */
    }

    found->mailbox = new_mailbox;

    if (found_blocked){
      drop_ctx(&blocked,found);
      drop_ctx(&queue,found);

      enqueue_ctx(&queue,found);
    }
    return ENC_SYM_TRUE;
  }

  /* check the current context */
  if (ctx_running && ctx_running->id == cid) {
    lbm_value new_mailbox = lbm_cons(msg, ctx_running->mailbox);

    if (lbm_type_of(new_mailbox) == LBM_TYPE_SYMBOL) {
      return new_mailbox; /* An error symbol */
    }
    ctx_running->mailbox = new_mailbox;
    return ENC_SYM_TRUE;
  }

  return ENC_SYM_NIL;
}

static lbm_value remove_from_list(int n, lbm_value list) {
  int c = 0;
  lbm_value res;
  lbm_value curr = list;

  lbm_value tmp = ENC_SYM_NIL;

  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    if (n == c) {
      curr = lbm_cdr(curr);
      break;
    }
    tmp = lbm_cons(lbm_car(curr), tmp);
    if (lbm_type_of(tmp) == LBM_TYPE_SYMBOL) {
      res = tmp;
      return res;
    }
    curr = lbm_cdr(curr);
    c++;
  }

  res = curr; /*res is the tail */
  curr = tmp;
  if ( c != 0) {
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      res = lbm_cons(lbm_car(curr),res);
      if (lbm_type_of(res) == LBM_TYPE_SYMBOL) {
        return res;
      }
      curr = lbm_cdr(curr);
    }
  }
  return res;
}

/* Pattern matching is currently implemented as a recursive
   function and make use of stack relative to the size of
   expressions that are being matched. */
static bool match(lbm_value p, lbm_value e, lbm_value *env, bool *gc) {

  lbm_value binding;

  if (lbm_is_match_binder(p)) {
    lbm_value var = lbm_cadr(p);
    lbm_value bindertype = lbm_car(p);

    if (!lbm_is_symbol(var)) return false;

    switch (lbm_dec_sym(bindertype)) {
    case SYM_MATCH_ANY:
      if (lbm_dec_sym(var) == SYM_DONTCARE) {
        return true;
      }
      break;
    case SYM_MATCH_I:
      if (lbm_type_of(e) == LBM_TYPE_I) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_U:
      if (lbm_type_of(e) == LBM_TYPE_U) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_I32:
      if (lbm_type_of(e) == LBM_TYPE_I32) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_U32:
      if (lbm_type_of(e) == LBM_TYPE_U32) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;

    case SYM_MATCH_FLOAT:
      if (lbm_type_of(e) == LBM_TYPE_FLOAT) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_U64:
      if (lbm_type_of(e) == LBM_TYPE_U64) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_I64:
      if (lbm_type_of(e) == LBM_TYPE_I64) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_DOUBLE:
      if (lbm_type_of(e) == LBM_TYPE_DOUBLE) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_CONS:
      if (lbm_type_of(e) == LBM_TYPE_CONS) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    default: /* this should be an error case */
      return false;
    }
    binding = lbm_cons(var, e);
    if ( lbm_type_of(binding) == LBM_TYPE_SYMBOL ) {
      *gc = true;
      return false;
    }
    *env = lbm_cons(binding, *env);
    if ( lbm_type_of(*env) == LBM_TYPE_SYMBOL ) {
      *gc = true;
      return false;
    }
    return true;
  }

  /* Comma-qualification experiment. */
  if (lbm_is_comma_qualified_symbol(p)) {
    lbm_value sym = lbm_car(lbm_cdr(p));
    lbm_value val = lbm_env_lookup(sym, *env);
    if (lbm_is_symbol(SYM_NOT_FOUND)) {
      return false;
    }
    return (val == e);
  }

  if (lbm_is_symbol(p)) {
    if (lbm_dec_sym(p) == SYM_DONTCARE) return true;
    return (p == e);
  }

  if (lbm_type_of(p) == LBM_TYPE_CONS &&
      lbm_type_of(e) == LBM_TYPE_CONS) {

    lbm_value headp = lbm_car(p);
    lbm_value heade = lbm_car(e);
    if (!match(headp, heade, env, gc)) {
      return false;
    }
    return match (lbm_cdr(p), lbm_cdr(e), env, gc);
  } else if (lbm_type_of(p) == LBM_TYPE_FLOAT &&
             lbm_type_of(e) == LBM_TYPE_FLOAT &&
             lbm_dec_float(p) == lbm_dec_float(e)) {
    return true;
  } else if (lbm_type_of(p) == LBM_TYPE_DOUBLE &&
             lbm_type_of(e) == LBM_TYPE_DOUBLE &&
             lbm_dec_double(p) == lbm_dec_double(e)) {
    return true;
  } else if (lbm_type_of(p) == LBM_TYPE_U32 &&
             lbm_type_of(e) == LBM_TYPE_U32 &&
             lbm_dec_u32(p) == lbm_dec_u32(e)) {
    return true;
  } else if (lbm_type_of(p) == LBM_TYPE_U64 &&
             lbm_type_of(e) == LBM_TYPE_U64 &&
             lbm_dec_u64(p) == lbm_dec_u64(e)) {
    return true;
  } else if (lbm_type_of(p) == LBM_TYPE_I32 &&
             lbm_type_of(e) == LBM_TYPE_I32 &&
             lbm_dec_i32(p) == lbm_dec_i32(e)) {
    return true;
  } else if (lbm_type_of(p) == LBM_TYPE_I64 &&
             lbm_type_of(e) == LBM_TYPE_I64 &&
             lbm_dec_i64(p) == lbm_dec_i64(e)) {
    return true;
  } else if (p == e) {
    return true;
  }
  return false;
}

static int find_match(lbm_value plist, lbm_value elist, lbm_value *e, lbm_value *env, bool *gc) {

  lbm_value curr_p = plist;
  lbm_value curr_e = elist;
  int n = 0;
  while (lbm_type_of(curr_e) == LBM_TYPE_CONS) {
    while (lbm_type_of(curr_p) == LBM_TYPE_CONS) {
      if (match(lbm_car(lbm_car(curr_p)), lbm_car(curr_e), env, gc)) {
        if (*gc) return -1;
        *e = lbm_cadr(lbm_car(curr_p));
        return n;
      }
      curr_p = lbm_cdr(curr_p);
    }
    curr_p = plist;       /* search all patterns against next exp */
    curr_e = lbm_cdr(curr_e);
    n ++;
  }

  return -1;
}

/****************************************************/
/* Garbage collection                               */
static int gc(lbm_value remember1, lbm_value remember2) {

  lbm_uint tstart = 0;
  lbm_uint tend = 0;

  if (timestamp_us_callback) {
    tstart = timestamp_us_callback();
  }

  lbm_gc_state_inc();

  lbm_value *variables = lbm_get_variable_table();
  if (variables) {
    for (int i = 0; i < lbm_get_num_variables(); i ++) {
      lbm_gc_mark_phase(variables[i]);
    }
  }

  lbm_gc_mark_freelist();
  lbm_gc_mark_phase(*lbm_get_env_ptr());
  lbm_gc_mark_phase(remember1);
  lbm_gc_mark_phase(remember2);

  eval_context_t *curr = queue.first;
  while (curr) {
    lbm_gc_mark_phase(curr->curr_env);
    lbm_gc_mark_phase(curr->curr_exp);
    lbm_gc_mark_phase(curr->program);
    lbm_gc_mark_phase(curr->r);
    lbm_gc_mark_phase(curr->mailbox);
    lbm_gc_mark_aux(curr->K.data, curr->K.sp);
    curr = curr->next;
  }

  curr = sleeping.first;
  while (curr) {
    lbm_gc_mark_phase(curr->curr_env);
    lbm_gc_mark_phase(curr->curr_exp);
    lbm_gc_mark_phase(curr->program);
    lbm_gc_mark_phase(curr->r);
    lbm_gc_mark_phase(curr->mailbox);
    lbm_gc_mark_aux(curr->K.data, curr->K.sp);
    curr = curr->next;
  }

  curr = blocked.first;
  while (curr) {
    lbm_gc_mark_phase(curr->curr_env);
    lbm_gc_mark_phase(curr->curr_exp);
    lbm_gc_mark_phase(curr->program);
    lbm_gc_mark_phase(curr->r);
    lbm_gc_mark_phase(curr->mailbox);
    lbm_gc_mark_aux(curr->K.data, curr->K.sp);
    curr = curr->next;
  }

  if (ctx_running) {
    lbm_gc_mark_phase(ctx_running->curr_env);
    lbm_gc_mark_phase(ctx_running->curr_exp);
    lbm_gc_mark_phase(ctx_running->program);
    lbm_gc_mark_phase(ctx_running->r);
    lbm_gc_mark_phase(ctx_running->mailbox);
    lbm_gc_mark_aux(ctx_running->K.data, ctx_running->K.sp);
  }

#ifdef VISUALIZE_HEAP
  heap_vis_gen_image();
#endif

  int r = lbm_gc_sweep_phase();

  if (timestamp_us_callback) {
    tend = timestamp_us_callback();
  }

  lbm_uint dur = 0;
  if (tend >= tstart) {
    dur = tend - tstart;
  }

  lbm_heap_new_gc_time(dur);

  lbm_heap_new_freelist_length();

  return r;
}

int lbm_perform_gc(void) {
  return gc(ENC_SYM_NIL,ENC_SYM_NIL);
}

/****************************************************/
/* Evaluation functions                             */

static inline bool eval_symbol(eval_context_t *ctx, lbm_value *value) {

  lbm_uint s = lbm_dec_sym(ctx->curr_exp);
  if (s < SPECIAL_SYMBOLS_END ||
      (lbm_get_extension(lbm_dec_sym(ctx->curr_exp)) != NULL)) {
    // Special symbols and extension symbols evaluate to themselves
    *value = ctx->curr_exp;
    return true;
  }

  if (s >= VARIABLE_SYMBOLS_START &&
             s < VARIABLE_SYMBOLS_END) {
    *value = lbm_get_var(s);
    return true;
  }

  if (lbm_env_lookup_b(value, ctx->curr_exp, ctx->curr_env)) {
    return true;
  } else {
    return lbm_env_lookup_b(value, ctx->curr_exp, *lbm_get_env_ptr());
  }
}

static inline void dynamic_load(eval_context_t *ctx) {

  const char *sym_str = lbm_get_name_by_symbol(lbm_dec_sym(ctx->curr_exp));
  const char *code_str = NULL;
  if (! dynamic_load_callback(sym_str, &code_str)) {
    error_ctx(ENC_SYM_NOT_FOUND);
    return;
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, ctx->curr_exp, RESUME));

    lbm_value chan;
    if (!create_string_channel((char *)code_str, &chan)) {
      gc(ENC_SYM_NIL, ENC_SYM_NIL);
      if (!create_string_channel((char *)code_str, &chan)) {
        error_ctx(ENC_SYM_MERROR);
        return;
      }
    }
    lbm_value loader = ENC_SYM_NIL;
    CONS_WITH_GC(loader, chan, loader, chan);
    CONS_WITH_GC(loader, ENC_SYM_READ, loader, loader);
    lbm_value evaluator = ENC_SYM_NIL;
    CONS_WITH_GC(evaluator, loader, evaluator, loader);
    CONS_WITH_GC(evaluator, ENC_SYM_EVAL, evaluator, evaluator);
    ctx->curr_exp = evaluator;
    return;
  }
}


static inline void eval_quote(eval_context_t *ctx) {
  ctx->r = lbm_cadr(ctx->curr_exp);
  ctx->app_cont = true;
}

static inline void eval_selfevaluating(eval_context_t *ctx) {
  ctx->r = ctx->curr_exp;
  ctx->app_cont = true;
}


static inline void eval_atomic(eval_context_t *ctx) {
  if (is_atomic) {
    lbm_set_error_reason("Atomic blocks cannot be nested!");
    error_ctx(ENC_SYM_EERROR);
    return;
  }

  CHECK_STACK(lbm_push(&ctx->K, EXIT_ATOMIC));
  is_atomic = true;
  ctx->curr_exp = lbm_cadr(ctx->curr_exp);
  /*NOTE:  ctx->app_cont = false; */
}


static inline void eval_callcc(eval_context_t *ctx) {

  lbm_value cont_array;
#ifndef LBM64
  if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U32)) {
    gc(ENC_SYM_NIL,ENC_SYM_NIL);
    if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U32)) {
      error_ctx(ENC_SYM_MERROR);
      return;
    }
  }
#else
  if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U64)) {
    gc(ENC_SYM_NIL,ENC_SYM_NIL);
    if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U64)) {
      error_ctx(ENC_SYM_MERROR);
      return;
    }
  }
#endif

  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(cont_array);
  memcpy(arr->data, ctx->K.data, ctx->K.sp * sizeof(lbm_uint));

  lbm_value acont;
  CONS_WITH_GC(acont, ENC_SYM_CONT, cont_array, cont_array);

  /* Create an application */
  lbm_value fun_arg = lbm_cadr(ctx->curr_exp);
  lbm_value app = ENC_SYM_NIL;
  CONS_WITH_GC(app, acont, app, acont);
  CONS_WITH_GC(app, fun_arg, app, app);

  ctx->curr_exp = app;
  ctx->app_cont = false;
}

static inline void eval_namespace(eval_context_t *ctx) {

  lbm_value arg = lbm_cadr(ctx->curr_exp);
  lbm_value body = lbm_car(lbm_cdr(lbm_cdr(ctx->curr_exp)));

  if (lbm_is_symbol(arg) && !lbm_is_symbol_nil(arg)) {
    lbm_value namespace_env = ENC_SYM_NIL;
    if (lbm_env_lookup_b(&namespace_env, arg, lbm_get_env())) {
      /* namespace exists, enter it */
    } else {
      lbm_value new_env;
      WITH_GC(new_env, lbm_env_set(*lbm_get_env_ptr(),arg,namespace_env), ENC_SYM_NIL, ENC_SYM_NIL);
      *lbm_get_env_ptr() = new_env;
    }

    CHECK_STACK(lbm_push_3(&ctx->K, arg, lbm_get_env(), NAMESPACE_POP));

    *lbm_get_env_ptr() = namespace_env;

    ctx->curr_exp = body;
    ctx->app_cont = false;
  } else {
    error_ctx(ENC_SYM_EERROR);
  }
}

static inline void eval_define(eval_context_t *ctx) {
  lbm_value args = lbm_cdr(ctx->curr_exp);
  lbm_value key = lbm_car(args);
  lbm_value rest_args = lbm_cdr(args);
  lbm_value val_exp = lbm_car(rest_args);

  lbm_uint *sptr = lbm_stack_reserve(&ctx->K, 2);
  if (!sptr) {
    error_ctx(ENC_SYM_STACK_ERROR);
    return;
  }

  if (lbm_is_symbol(key) && lbm_is_symbol_nil(lbm_cdr(rest_args))) {
    lbm_uint sym_val = lbm_dec_sym(key);

    sptr[0] = key;

    if ((sym_val >= VARIABLE_SYMBOLS_START) &&
        (sym_val <  VARIABLE_SYMBOLS_END)) {
      sptr[1] = SET_VARIABLE;
      ctx->curr_exp = val_exp;
      return;
    } else if (sym_val >= RUNTIME_SYMBOLS_START) {
      sptr[1] = SET_GLOBAL_ENV;
      ctx->curr_exp = val_exp;
      return;
    }
  }
  error_ctx(ENC_SYM_EERROR);
  return;
}

static inline void eval_progn(eval_context_t *ctx) {
  lbm_value exps = lbm_cdr(ctx->curr_exp);
  lbm_value env  = ctx->curr_env;

  if (lbm_is_list(exps)) {
    lbm_uint *sptr = lbm_stack_reserve(&ctx->K, 3);
    if (!sptr) {
      error_ctx(ENC_SYM_STACK_ERROR);
      return;
    }
    sptr[0] = env;
    sptr[1] = lbm_cdr(exps);
    sptr[2] = PROGN_REST;
    ctx->curr_exp = lbm_car(exps);
    ctx->curr_env = env;
    if (lbm_is_symbol(sptr[1])) /* The only symbol it can be is nil */
      lbm_stack_drop(&ctx->K, 3);
  } else if (lbm_is_symbol_nil(exps)) {
    ctx->r = ENC_SYM_NIL;
    ctx->app_cont = true;
  } else {
    error_ctx(ENC_SYM_EERROR);
  }
}

static inline lbm_value mk_closure(lbm_value env, lbm_value body, lbm_value params) {
  lbm_value env_end = cons_with_gc( env, ENC_SYM_NIL, env);
  lbm_value exp = cons_with_gc(body, env_end, env_end);
  lbm_value par = cons_with_gc(params, exp, exp);
  return cons_with_gc(ENC_SYM_CLOSURE, par, par);
}

static inline void eval_lambda(eval_context_t *ctx) {
  lbm_value closure = mk_closure(ctx->curr_env, lbm_cadr(lbm_cdr(ctx->curr_exp)),  lbm_cadr(ctx->curr_exp));
  ctx->app_cont = true;
  ctx->r = closure;
  return;
}

static inline void eval_if(eval_context_t *ctx) {

  lbm_value cddr = lbm_cdr(lbm_cdr(ctx->curr_exp));
  lbm_value then_branch = lbm_car(cddr);
  lbm_value else_branch = lbm_cadr(cddr);

  lbm_uint *sptr = lbm_stack_reserve(&ctx->K, 4);
  if (!sptr) {
    error_ctx(ENC_SYM_STACK_ERROR);
    return;
  }
  sptr[0] = else_branch;
  sptr[1] = then_branch;
  sptr[2] = ctx->curr_env;
  sptr[3] = IF;
  ctx->curr_exp = lbm_cadr(ctx->curr_exp);
}

static inline void eval_let(eval_context_t *ctx) {
  lbm_value orig_env = ctx->curr_env;
  lbm_value binds    = lbm_cadr(ctx->curr_exp); // key value pairs.
  lbm_value exp      = lbm_cadr(lbm_cdr(ctx->curr_exp)); // exp to evaluate in the new env.

  lbm_value curr = binds;
  lbm_value new_env = orig_env;

  if (lbm_type_of(binds) != LBM_TYPE_CONS) {
    // binds better be nil or there is a programmer error.
    ctx->curr_exp = exp;
    return;
  }

  // Implements letrec by "preallocating" the key parts
  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    lbm_value key = lbm_car(lbm_car(curr));
    lbm_value val = ENC_SYM_NIL;
    lbm_value binding;
    lbm_value new_env_tmp;
    WITH_GC(binding, lbm_cons(key, val), new_env, ENC_SYM_NIL);
    WITH_GC(new_env_tmp, lbm_cons(binding, new_env), new_env, binding);
    new_env = new_env_tmp;
    curr = lbm_cdr(curr);
  }

  lbm_value key0 = lbm_car(lbm_car(binds));
  lbm_value val0_exp = lbm_cadr(lbm_car(binds));

  lbm_uint *sptr = lbm_stack_reserve(&ctx->K, 5);
  if (!sptr) {
    error_ctx(ENC_SYM_STACK_ERROR);
    return;
  }
  sptr[0] = exp;
  sptr[1] = lbm_cdr(binds);
  sptr[2] = new_env;
  sptr[3] = key0;
  sptr[4] = BIND_TO_KEY_REST;
  ctx->curr_exp = val0_exp;
  ctx->curr_env = new_env;
  return;
}

static inline void eval_and(eval_context_t *ctx) {
  lbm_value rest = lbm_cdr(ctx->curr_exp);
  if (lbm_is_symbol_nil(rest)) {
    ctx->app_cont = true;
    ctx->r = ENC_SYM_TRUE;
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(rest), AND));
    ctx->curr_exp = lbm_car(rest);
  }
}

static inline void eval_or(eval_context_t *ctx) {
  lbm_value rest = lbm_cdr(ctx->curr_exp);
  if (lbm_is_symbol_nil(rest)) {
    ctx->app_cont = true;
    ctx->r = ENC_SYM_NIL;
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(rest), OR));
    ctx->curr_exp = lbm_car(rest);
  }
}

/* pattern matching experiment */
/* format:                     */
/* (match e (pattern body)     */
/*          (pattern body)     */
/*          ...  )             */
static inline void eval_match(eval_context_t *ctx) {

  lbm_value rest = lbm_cdr(ctx->curr_exp);
  if (lbm_type_of(rest) == LBM_TYPE_SYMBOL &&
      rest == ENC_SYM_NIL) {
    /* Someone wrote the program (match) */
    ctx->app_cont = true;
    ctx->r = ENC_SYM_NIL;
    return;
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(rest), MATCH));
    ctx->curr_exp = lbm_car(rest); /* Evaluate e next*/
  }
}

static inline void eval_receive(eval_context_t *ctx) {

  if (is_atomic) {
    lbm_set_error_reason("Cannot receive inside of an atomic block");
    error_ctx(ENC_SYM_EERROR);
    return;
  }

  if (lbm_is_symbol_nil(ctx->mailbox)) {
    /*nothing in the mailbox: block the context*/
    ctx->timestamp = timestamp_us_callback();
    ctx->sleep_us = 0;
    enqueue_ctx(&blocked,ctx);
    ctx_running = NULL;
  } else {
    lbm_value pats = ctx->curr_exp;
    lbm_value msgs = ctx->mailbox;

    if (lbm_is_symbol_nil(pats)) {
      /* A receive statement without any patterns */
      ctx->app_cont = true;
      ctx->r = ENC_SYM_NIL;
    } else {
      /* The common case */
      lbm_value e;
      lbm_value new_env = ctx->curr_env;
      bool do_gc = false;
      int n = find_match(lbm_cdr(pats), msgs, &e, &new_env, &do_gc);
      if (do_gc) {
        gc(ENC_SYM_NIL, ENC_SYM_NIL);
        do_gc = false;
        n = find_match(lbm_cdr(pats), msgs, &e, &new_env, &do_gc);
        if (do_gc) {
          ctx_running->done = true;
          error_ctx(ENC_SYM_MERROR);
          return;
        }
      }
      if (n >= 0 ) { /* Match */
        lbm_value new_mailbox;
        WITH_GC(new_mailbox, remove_from_list(n, msgs), ENC_SYM_NIL, ENC_SYM_NIL);

        ctx->mailbox = new_mailbox;
        ctx->curr_env = new_env;
        ctx->curr_exp = e;
      } else { /* No match  go back to sleep */
        ctx->timestamp = timestamp_us_callback();
        ctx->sleep_us = 0;
        enqueue_ctx(&blocked,ctx);
        ctx_running = NULL;
        ctx->r = ENC_SYM_NO_MATCH;
      }
    }
  }
  return;
}

/*********************************************************/
/*  Continuation functions                               */

static inline void cont_set_global_env(eval_context_t *ctx){

  lbm_value key;
  lbm_value val = ctx->r;

  lbm_pop(&ctx->K, &key);
  lbm_value new_env;
  WITH_GC(new_env, lbm_env_set(*lbm_get_env_ptr(),key,val), key, ENC_SYM_NIL);

  *lbm_get_env_ptr() = new_env;
  ctx->r = key;

  ctx->app_cont = true;

  return;
}

static inline void cont_set_var(eval_context_t *ctx) {
  lbm_value key;
  lbm_value val = ctx->r;
  lbm_pop(&ctx->K, &key);

  ctx->r = lbm_set_var(lbm_dec_sym(key), val);
  ctx->app_cont = true;
  return;
}

static inline void cont_resume(eval_context_t *ctx) {
  lbm_value exp;
  lbm_pop(&ctx->K, &exp);
  ctx->curr_exp = exp;
}

static inline void cont_expand_macro(eval_context_t *ctx) {

  lbm_uint* sptr = lbm_get_stack_ptr(&ctx->K, 2);
  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }
  lbm_value env = (lbm_value)sptr[0];
  lbm_value args = (lbm_value)sptr[1];

  if (lbm_is_macro(ctx->r)) {

    lbm_value m = ctx->r;
    lbm_value curr_param = lbm_cadr(m);
    lbm_value curr_arg = args;
    lbm_value expand_env = env;
    while (lbm_type_of(curr_param) == LBM_TYPE_CONS &&
           lbm_type_of(curr_arg)   == LBM_TYPE_CONS) {

      lbm_value entry;
      WITH_GC(entry,lbm_cons(lbm_car(curr_param),lbm_car(curr_arg)), expand_env,ENC_SYM_NIL);

      lbm_value aug_env;
      WITH_GC(aug_env,lbm_cons(entry, expand_env),expand_env,entry);
      expand_env = aug_env;

      curr_param = lbm_cdr(curr_param);
      curr_arg   = lbm_cdr(curr_arg);
    }
    lbm_stack_drop(&ctx->K, 2);
    ctx->curr_exp = lbm_cadr(lbm_cdr(m));
    ctx->curr_env = expand_env;
    ctx->app_cont = false;
    return;
  }
  error_ctx(ENC_SYM_EERROR);
}

static inline void cont_progn_rest(eval_context_t *ctx) {
  lbm_value rest;
  lbm_value env;

  lbm_value *sptr = (lbm_value*)lbm_get_stack_ptr(&ctx->K, 2);
  if (sptr == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  rest = sptr[1];
  env  = sptr[0];

  if (lbm_is_symbol_nil(rest)) {
    lbm_stack_drop(&ctx->K, 2);
    ctx->app_cont = true;
    return;
  }
  // allow for tail recursion
  if (lbm_is_symbol_nil(lbm_cdr(rest))) {
    ctx->curr_exp = lbm_car(rest);
    ctx->curr_env = env;
    lbm_stack_drop(&ctx->K, 2);
  } else {
    sptr[1] = lbm_cdr(rest);
    CHECK_STACK(lbm_push(&ctx->K, PROGN_REST));
    ctx->curr_exp = lbm_car(rest);
    ctx->curr_env = env;
  }
}

static inline void cont_wait(eval_context_t *ctx) {

  lbm_value cid_val;
  lbm_pop(&ctx->K, &cid_val);
  lbm_cid cid = (lbm_cid)lbm_dec_i(cid_val);

  bool exists = false;

  lbm_blocked_iterator(context_exists, &cid, &exists);
  lbm_running_iterator(context_exists, &cid, &exists);

  if (ctx_running->id == cid) {
    exists = true;
  }

  if (exists) {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_enc_i(cid), WAIT));
    ctx->r = ENC_SYM_TRUE;
    ctx->app_cont = true;
    yield_ctx(50000);
  } else {
    ctx->r = ENC_SYM_TRUE;
    ctx->app_cont = true;
  }
}

static inline void apply_setvar(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
 if (nargs == 2 && lbm_is_symbol(args[1])) {
   lbm_uint s = lbm_dec_sym(args[1]);
   if (s >= VARIABLE_SYMBOLS_START &&
       s <  VARIABLE_SYMBOLS_END) {
     /* #var case ignores local/global if present */
     ctx->r = lbm_set_var(s, args[2]);
   } else if (s >= RUNTIME_SYMBOLS_START) {
     lbm_value new_env = lbm_env_modify_binding(ctx->curr_env, args[1], args[2]);
     if (lbm_type_of(new_env) == LBM_TYPE_SYMBOL &&
         new_env == ENC_SYM_NOT_FOUND) {
       new_env = lbm_env_modify_binding(lbm_get_env(), args[1], args[2]);
     }
     if (lbm_type_of(new_env) == LBM_TYPE_SYMBOL &&
         new_env == ENC_SYM_NOT_FOUND) {
       new_env = lbm_env_set(lbm_get_env(),  args[1], args[2]);
       if (lbm_is_error(new_env)) {
         gc(ENC_SYM_NIL,ENC_SYM_NIL);
         new_env = lbm_env_set(lbm_get_env(),  args[1], args[2]);
         if (lbm_is_error(new_env)) {
           error_ctx(new_env);
           return;
         }
       }
       *lbm_get_env_ptr() = new_env;
     } else {
       ctx->r = args[2];
     }
   } else {
     error_ctx(ENC_SYM_EERROR);
     return;
   }
 } else {
   error_ctx(ENC_SYM_EERROR);
   return;
 }
 ctx->r = args[2];
 lbm_stack_drop(&ctx->K, nargs+1);
 ctx->app_cont = true;
}

static inline void apply_read_program(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value chan = ENC_SYM_NIL;
    if (lbm_type_of(args[1]) == LBM_TYPE_ARRAY) {
      if (!create_string_channel(lbm_dec_str(args[1]), &chan)) {
        gc(ENC_SYM_NIL, ENC_SYM_NIL);
        if (!create_string_channel(lbm_dec_str(args[1]), &chan)) {
          error_ctx(ENC_SYM_MERROR);
          return;
        }
      }
    } else if (lbm_type_of(args[1]) == LBM_TYPE_CHANNEL) {
      chan = args[1];
    } else {
      error_ctx(SYM_EERROR);
      return;
    }
    lbm_value fun = args[0];
    lbm_stack_drop(&ctx->K, nargs+1);
    CHECK_STACK(lbm_push_3(&ctx->K, chan, fun, READ));
    ctx->r = ENC_SYM_NIL;
    ctx->app_cont = true;
  }
}

static inline void apply_spawn(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {

  lbm_uint stack_size = EVAL_CPS_DEFAULT_STACK_SIZE;
  lbm_uint closure_pos = 1;

  if (nargs >= 2 &&
      lbm_is_number(args[1]) &&
      lbm_is_closure(args[2])) {
    stack_size = lbm_dec_as_u32(args[1]);
    closure_pos = 2;
  }

  if (!lbm_is_closure(args[closure_pos]) ||
      nargs < 1) {
    error_ctx(ENC_SYM_EERROR);
    return;
  }

  lbm_value cdr_fun = lbm_cdr(args[closure_pos]);
  lbm_value cddr_fun = lbm_cdr(cdr_fun);
  lbm_value cdddr_fun = lbm_cdr(cddr_fun);
  lbm_value params  = lbm_car(cdr_fun);
  lbm_value exp     = lbm_car(cddr_fun);
  lbm_value clo_env = lbm_car(cdddr_fun);

  lbm_value curr_param = params;
  lbm_uint i = closure_pos + 1;
  while (lbm_type_of(curr_param) == LBM_TYPE_CONS &&
         i <= nargs) {

    lbm_value entry;
    WITH_GC(entry,lbm_cons(lbm_car(curr_param),args[i]), clo_env,ENC_SYM_NIL);

    lbm_value aug_env;
    WITH_GC(aug_env,lbm_cons(entry, clo_env),clo_env,entry);
    clo_env = aug_env;
    curr_param = lbm_cdr(curr_param);
    i ++;
  }

  lbm_stack_drop(&ctx->K, nargs+1);

  lbm_value program =  ENC_SYM_NIL;
  CONS_WITH_GC(program, exp, program, clo_env);


  lbm_cid cid = lbm_create_ctx(program,
                               clo_env,
                               stack_size);
  ctx->r = lbm_enc_i(cid);
  ctx->app_cont = true;
}

static inline void apply_yield(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (is_atomic) {
    lbm_set_error_reason("Cannot yield inside of an atomic block");
    error_ctx(ENC_SYM_EERROR);
    return;
  }
  if (nargs == 1 && lbm_is_number(args[1])) {
    lbm_uint ts = lbm_dec_as_u32(args[1]);
    lbm_stack_drop(&ctx->K, nargs+1);
    yield_ctx(ts);
  } else {
    error_ctx(ENC_SYM_EERROR);
  }
}

static inline void apply_wait(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (lbm_type_of(args[1]) == LBM_TYPE_I) {
    lbm_cid cid = (lbm_cid)lbm_dec_i(args[1]);
    lbm_stack_drop(&ctx->K, nargs+1);
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_enc_i(cid), WAIT));
    ctx->r = ENC_SYM_TRUE;
    ctx->app_cont = true;
    yield_ctx(50000);
  } else {
    error_ctx(ENC_SYM_EERROR);
  }
}

static inline void apply_eval(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  ctx->curr_exp = args[1];
  lbm_stack_drop(&ctx->K, nargs+1);
}

static inline void apply_eval_program(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {

  lbm_value prg = args[1];
  prg = lbm_list_append(prg, ctx->program);

  lbm_stack_drop(&ctx->K, nargs+1);

  if (lbm_type_of(prg) != LBM_TYPE_CONS) {
    error_ctx(ENC_SYM_EERROR);
    return;
  }

  ctx->program = lbm_cdr(prg);
  ctx->curr_exp = lbm_car(prg);
}

static inline void apply_send(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value status = ENC_SYM_EERROR;
  if (nargs == 2) {

    if (lbm_type_of(args[1]) == LBM_TYPE_I) {
      lbm_cid cid = (lbm_cid)lbm_dec_i(args[1]);
      lbm_value msg = args[2];

      WITH_GC(status, lbm_find_receiver_and_send(cid, msg), ENC_SYM_NIL, ENC_SYM_NIL);
    }
  }
  /* return the status */
  lbm_stack_drop(&ctx->K, nargs+1);
  ctx->r = status;
  ctx->app_cont = true;
}

static inline void apply_fundamental(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value res;
  WITH_GC(res, lbm_fundamental(&args[1], nargs, args[0]), ENC_SYM_NIL, ENC_SYM_NIL);
  if (lbm_is_error(res)) {
    error_ctx(res);
    return;
  }
  lbm_stack_drop(&ctx->K, nargs+1);
  ctx->app_cont = true;
  ctx->r = res;
}

static inline void apply_extension(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  extension_fptr f = lbm_get_extension(lbm_dec_sym(args[0]));
  if (f == NULL) {
    error_ctx(ENC_SYM_EERROR);
    return;
  }

  lbm_value ext_res;
  WITH_GC(ext_res, f(&args[1] , nargs), ENC_SYM_NIL, ENC_SYM_NIL);
  if (lbm_is_error(ext_res)) {
    error_ctx(ext_res);
    return;
  }
  lbm_stack_drop(&ctx->K, nargs + 1);

  if (blocking_extension) {
    blocking_extension = false;
    ctx->timestamp = timestamp_us_callback();
    ctx->sleep_us = 0;
    ctx->app_cont = true;
    enqueue_ctx(&blocked,ctx);
    ctx_running = NULL;
  } else {
    ctx->app_cont = true;
    ctx->r = ext_res;
  }
}

static inline void cont_application(eval_context_t *ctx) {
  lbm_value count;
  lbm_pop(&ctx->K, &count);

  lbm_uint arg_count = lbm_dec_u(count);

  lbm_uint *fun_args = lbm_get_stack_ptr(&ctx->K, arg_count+1);

  if (fun_args == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }
  lbm_value fun = fun_args[0];
  if (lbm_is_continuation(fun)) {

    lbm_value c = lbm_cdr(fun); /* should be the continuation */

    if (!lbm_is_array(c)) {
      error_ctx(ENC_SYM_FATAL_ERROR);
      return;
    }

    lbm_value arg = ENC_SYM_NIL;
    if (arg_count == 1) {
      arg = fun_args[1];
    } else if (arg_count > 1) {
      lbm_set_error_reason("A continuation created by call-cc was applied to too many arguments (>1)");
      error_ctx(ENC_SYM_EERROR);
      return;
    }
    // zero argument continuation application is fine! (defaults to a nil arg)
    lbm_stack_clear(&ctx->K);

    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(c);

    ctx->K.sp = arr->size;
    memcpy(ctx->K.data, arr->data, arr->size * sizeof(lbm_uint));

    ctx->r = arg;
    ctx->app_cont = true;
  } else if (lbm_type_of(fun) == LBM_TYPE_SYMBOL) {

    /* eval_cps specific operations */
    //lbm_uint dfun = lbm_dec_sym(fun);

    switch(fun) {
    case ENC_SYM_SETVAR: apply_setvar(fun_args, lbm_dec_u(count), ctx); break;
    case ENC_SYM_READ: /* fall through */
    case ENC_SYM_READ_PROGRAM: apply_read_program(fun_args, lbm_dec_u(count), ctx); break;
    case ENC_SYM_SPAWN: apply_spawn(fun_args, lbm_dec_u(count), ctx); break;
    case ENC_SYM_YIELD: apply_yield(fun_args, lbm_dec_u(count), ctx); break;
    case ENC_SYM_WAIT: apply_wait(fun_args, lbm_dec_u(count), ctx); break;
    case ENC_SYM_EVAL: apply_eval(fun_args, lbm_dec_u(count), ctx); break;
    case ENC_SYM_EVAL_PROGRAM: apply_eval_program(fun_args, lbm_dec_u(count), ctx); break;
    case ENC_SYM_SEND: apply_send(fun_args, lbm_dec_u(count), ctx); break;
    default:
      if (lbm_is_fundamental(fun)) {
        // If it is not a eval_cps specific function, it may be a fundamental operation
        apply_fundamental(fun_args, lbm_dec_u(count), ctx);
      } else {
        // It may be an extension
        apply_extension(fun_args, lbm_dec_u(count), ctx);
      }
    }
  } else {
    error_ctx(ENC_SYM_EERROR);
  }
}

static inline void cont_closure_application_args(eval_context_t *ctx) {
  lbm_uint* sptr = lbm_get_stack_ptr(&ctx->K, 5);

  if (sptr == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }
  lbm_value arg_env = (lbm_value)sptr[0];
  lbm_value exp     = (lbm_value)sptr[1];
  lbm_value clo_env = (lbm_value)sptr[2];
  lbm_value params  = (lbm_value)sptr[3];
  lbm_value args    = (lbm_value)sptr[4];

  if (lbm_is_list(params)) {
    lbm_value entry;
    WITH_GC(entry,lbm_cons(lbm_car(params),ctx->r), ENC_SYM_NIL, ENC_SYM_NIL);

    lbm_value aug_env;
    WITH_GC(aug_env,lbm_cons(entry, clo_env),entry,ENC_SYM_NIL);
    clo_env = aug_env;
  }

  bool a_nil = lbm_is_symbol_nil(args);
  bool p_nil = lbm_is_symbol_nil(lbm_cdr(params));

  if (a_nil && p_nil) {
    lbm_stack_drop(&ctx->K, 5);
    ctx->curr_env = clo_env;
    ctx->curr_exp = exp;
    ctx->app_cont = false;
  } else if (!a_nil && p_nil) {
    lbm_set_error_reason("Too many arguments.");
    error_ctx(ENC_SYM_EERROR);
  } else if (a_nil && !p_nil) {
    lbm_value new_env = lbm_list_append(arg_env,clo_env);
    lbm_value closure = mk_closure(new_env, exp, lbm_cdr(params));
    lbm_stack_drop(&ctx->K, 5);
    ctx->app_cont = true;
    ctx->r = closure;
  } else {
   sptr[2] = clo_env;
   sptr[3] = lbm_cdr(params);
   sptr[4] = lbm_cdr(args);
   CHECK_STACK(lbm_push(&ctx->K, CLOSURE_ARGS));
   ctx->curr_exp = lbm_car(args);
   ctx->curr_env = arg_env;
  }
}

static inline void cont_application_args(eval_context_t *ctx) {

  lbm_uint *sptr = lbm_get_stack_ptr(&ctx->K,3);

  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_value env = sptr[0];
  lbm_value count = sptr[1];
  lbm_value rest = sptr[2];
  lbm_value arg = ctx->r;

  sptr[0] = arg;
  if (lbm_is_symbol_nil(rest)) {
    // no arguments
    sptr[1] = count;
    lbm_stack_drop(&ctx->K, 1);
    cont_application(ctx);
  } else if (lbm_is_list(rest)) {
    sptr[1] = env;
    sptr[2] = lbm_enc_u(lbm_dec_u(count) + 1);
    CHECK_STACK(lbm_push_2(&ctx->K,lbm_cdr(rest), APPLICATION_ARGS));
    ctx->curr_exp = lbm_car(rest);
    ctx->curr_env = env;
  } else {
    error_ctx(ENC_SYM_EERROR);
  }
}

static inline void cont_and(eval_context_t *ctx) {
  lbm_value rest;
  lbm_value arg = ctx->r;
  lbm_pop(&ctx->K, &rest);
  if (lbm_is_symbol_nil(arg)) {
    ctx->app_cont = true;
    ctx->r = ENC_SYM_NIL;
  } else if (lbm_is_symbol_nil(rest)) {
    ctx->app_cont = true;
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(rest), AND));
    ctx->curr_exp = lbm_car(rest);
  }
}

static inline void cont_or(eval_context_t *ctx) {
  lbm_value rest;
  lbm_value arg = ctx->r;
  lbm_pop(&ctx->K, &rest);
  if (!lbm_is_symbol_nil(arg)) {
    ctx->app_cont = true;
  } else if (lbm_is_symbol_nil(rest)) {
    ctx->app_cont = true;
    ctx->r = ENC_SYM_NIL;
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(rest), OR));
    ctx->curr_exp = lbm_car(rest);
  }
}

static inline void cont_bind_to_key_rest(eval_context_t *ctx) {
  lbm_value arg = ctx->r;

  lbm_value *sptr = lbm_get_stack_ptr(&ctx->K, 4);
  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }
  lbm_value rest = sptr[1];
  lbm_value env  = sptr[2];
  lbm_value key  = sptr[3];

  lbm_type keyt = lbm_type_of(key);

  if ((keyt != LBM_TYPE_SYMBOL) ||
      ((keyt == LBM_TYPE_SYMBOL) && lbm_dec_sym(key) < RUNTIME_SYMBOLS_START)) {
    lbm_set_error_reason("Incorrect type of name/key in let-binding");
    error_ctx(ENC_SYM_EERROR);
    return;
  }

  lbm_env_modify_binding(env, key, arg);

  if (lbm_is_list(rest)) {
    lbm_value keyn = lbm_car(lbm_car(rest));
    lbm_value valn_exp = lbm_cadr(lbm_car(rest));

    sptr[1] = lbm_cdr(rest);
    sptr[3] = keyn;
    CHECK_STACK(lbm_push(&ctx->K, BIND_TO_KEY_REST));
    ctx->curr_exp = valn_exp;
    ctx->curr_env = env;
  } else {
    // Otherwise evaluate the expression in the populated env
    ctx->curr_exp = sptr[0];
    ctx->curr_env = env;
    lbm_stack_drop(&ctx->K, 4);
  }
}

static inline void cont_if(eval_context_t *ctx) {

  lbm_value arg = ctx->r;

  lbm_value *sptr = lbm_get_stack_ptr(&ctx->K, 3);
  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  ctx->curr_env = sptr[2];
  if (lbm_is_symbol_nil(arg)) {
    ctx->curr_exp = sptr[0]; // else branch
  } else {
    ctx->curr_exp = sptr[1]; // then branch
  }
  lbm_stack_drop(&ctx->K, 3);
}

static inline void cont_match_many(eval_context_t *ctx) {

  lbm_value r = ctx->r;

  lbm_value rest_msgs;
  lbm_value pats;
  lbm_value exp;

  lbm_pop_3(&ctx->K, &rest_msgs, &pats, &exp);

  if (lbm_type_of(r) == LBM_TYPE_SYMBOL &&
      (lbm_dec_sym(r) == SYM_NO_MATCH)) {

    if (lbm_type_of(rest_msgs) == LBM_TYPE_SYMBOL &&
        lbm_dec_sym(rest_msgs) == SYM_NIL) {

      ctx->curr_exp = exp;

    } else {
      /* try match the next one */
      CHECK_STACK(lbm_push_4(&ctx->K, exp, pats, lbm_cdr(rest_msgs), MATCH_MANY));
      CHECK_STACK(lbm_push_2(&ctx->K, lbm_car(pats), MATCH));
      ctx->r = lbm_car(rest_msgs);
      ctx->app_cont = true;
    }
  } else {
  /* I think the else branch will be "do nothing" here. */
  /* We should just continue executing with the result in ctx->r already*/
    ctx->app_cont = true;
  }
}

static inline void cont_match(eval_context_t *ctx) {
  lbm_value e = ctx->r;
  lbm_value patterns;
  lbm_value new_env = ctx->curr_env;
  bool  do_gc = false;

  lbm_pop(&ctx->K, &patterns);

  if (lbm_is_symbol_nil(patterns)) {
    /* no more patterns */
    ctx->r = ENC_SYM_NO_MATCH;
    ctx->app_cont = true;
  } else if (lbm_is_list(patterns)) {
    lbm_value pattern = lbm_car(lbm_car(patterns));
    lbm_value body    = lbm_cadr(lbm_car(patterns));

    if (match(pattern, e, &new_env, &do_gc)) {
      ctx->curr_env = new_env;
      ctx->curr_exp = body;
    } else if (do_gc) {
      gc(patterns,e);
      do_gc = false;
      new_env = ctx->curr_env;
      match(pattern, e, &new_env, &do_gc);
      if (do_gc) {
        ctx_running->done = true;
        error_ctx(ENC_SYM_MERROR);
        return;
      }
      ctx->curr_env = new_env;
      ctx->curr_exp = body;
    } else {
      /* set up for checking of next pattern */
      CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(patterns), MATCH));
      /* leave r unaltered */
      ctx->app_cont = true;
    }
  } else {
    error_ctx(ENC_SYM_TERROR);
  }
}


static inline void cont_namespace_pop(eval_context_t *ctx) {

  lbm_value env;
  lbm_value leaving_space;
  lbm_pop_2(&ctx->K, &env, &leaving_space);
  lbm_value leaving_env = lbm_get_env();
  *lbm_get_env_ptr() = env;
  lbm_value new_env;
  WITH_GC(new_env, lbm_env_set(*lbm_get_env_ptr(),leaving_space,leaving_env), leaving_space, leaving_env);
  ctx->app_cont = true;
}

static inline void cont_exit_atomic(eval_context_t *ctx) {
  is_atomic = false;
  ctx->app_cont = true;
}


/****************************************************/
/*   READER                                         */

static inline void cont_read(eval_context_t *ctx) {

  gc(ENC_SYM_NIL,ENC_SYM_NIL);

  lbm_value stream = ENC_SYM_NIL;
  lbm_value prg_val = ENC_SYM_NIL;
  lbm_pop_2(&ctx->K, &prg_val, &stream);

  bool program = false;

  if (lbm_type_of(prg_val) == LBM_TYPE_SYMBOL) {
    if (lbm_dec_sym(prg_val) == SYM_READ) program = false;
    else if (lbm_dec_sym(prg_val) == SYM_READ_PROGRAM) program = true;
  } else {
    error_ctx(ENC_SYM_FATAL_ERROR);
    done_reading(ctx->id);
    return;
  }

  lbm_value prg_tag = lbm_enc_u(program ? 1 : 0);

  CHECK_STACK(lbm_push_3(&ctx->K, prg_tag, stream, READ_DONE));

  if (program) {
    CHECK_STACK(lbm_push_4(&ctx->K, ENC_SYM_NIL, ENC_SYM_NIL, stream, READ_APPEND_CONTINUE));
  }
  CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));

  ctx->app_cont = true;
}

static void read_process_token(eval_context_t *ctx, lbm_value stream, lbm_value tok) {

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  if (lbm_type_of(tok) == LBM_TYPE_SYMBOL) {
    switch (lbm_dec_sym(tok)) {
    case SYM_RERROR:
      lbm_channel_reader_close(str);
      lbm_set_error_reason((char*)parse_error_token);
      read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
      done_reading(ctx->id);
      return;
    case SYM_MERROR:
      error_ctx(ENC_SYM_MERROR);
      done_reading(ctx->id);
      return;
    case SYM_TOKENIZER_WAIT:
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      yield_ctx(EVAL_CPS_MIN_SLEEP);
      return;
    case SYM_TOKENIZER_DONE: {
      /* Tokenizer reached "end of file"
         The parser could be in a state where it needs
         more tokens to correctly finish an expression.

         Three cases
         1. The program / expression is malformed and the context should die.
         2. We are finished reading a program and should close off the
            internl representation with a closing parenthesis. Then
            apply continuation.
         3. We are finished reading an expression and should
            apply the continuation.


         In case 3, we should find the READ_DONE at sp - 1.
         In case 2, we should find the READ_DONE at sp - 5.

      */

      if (ctx->K.data[ctx->K.sp-1] == READ_DONE &&
          lbm_dec_u(ctx->K.data[ctx->K.sp-3]) == 0) {
        /* successfully finished reading an expression  (CASE 3) */
        ctx->app_cont = true;
      } else if (ctx->K.sp > 5 && ctx->K.data[ctx->K.sp - 5] == READ_DONE &&
                 lbm_dec_u(ctx->K.data[ctx->K.sp-7]) == 1) {
        /* successfully finished reading a program  (CASE 2) */
        ctx->r = ENC_SYM_CLOSEPAR;
        ctx->app_cont = true;
      } else {
        /* Parsing failed */
        lbm_set_error_reason((char*)parse_error_eof);
        read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
        done_reading(ctx->id);
      }
    } break;
    case SYM_CLOSEPAR:
      ctx->r = tok;
      ctx->app_cont = true;
      break;
    case SYM_OPENPAR:
      CHECK_STACK(lbm_push_4(&ctx->K,
                             ENC_SYM_NIL, ENC_SYM_NIL,
                             stream,
                             READ_APPEND_CONTINUE));
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      ctx->app_cont = true;
      break;
    case SYM_CLOSEBRACK:
      ctx->r = tok;
      ctx->app_cont = true;
      break;
    case SYM_OPENBRACK:
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_START_ARRAY));
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      ctx->app_cont = true;
      break;
    case SYM_QUOTE_IT:
      CHECK_STACK(lbm_push(&ctx->K, READ_QUOTE_RESULT));
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      ctx->app_cont = true;
      break;
    case SYM_BACKQUOTE:
      CHECK_STACK(lbm_push(&ctx->K, READ_BACKQUOTE_RESULT));
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      ctx->app_cont = true;
      break;
    case SYM_COMMAAT:
      CHECK_STACK(lbm_push(&ctx->K, READ_COMMAAT_RESULT));
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      ctx->app_cont = true;
      break;
    case SYM_COMMA:
      CHECK_STACK(lbm_push(&ctx->K, READ_COMMA_RESULT));
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      ctx->app_cont = true;
      break;
    default: // arbitrary symbol form
      CHECK_STACK(lbm_push_3(&ctx->K, stream, tok, READ_CHECK_COLON));
      ctx->app_cont = true;
      break;
    }
  } else { // arbitrary value form
    ctx->r = tok;
    ctx->app_cont = true;
  }
}

static inline void cont_read_next_token(eval_context_t *ctx) {

  lbm_value stream;
  lbm_pop(&ctx->K, &stream);

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_value tok = lbm_get_next_token(str, false);

  read_process_token(ctx, stream, tok);
}

static inline void cont_read_start_array(eval_context_t *ctx) {

  lbm_value stream;

  lbm_pop(&ctx->K, &stream);

  lbm_uint num_free = lbm_memory_longest_free();
  lbm_uint initial_size = (lbm_uint)((float)num_free * 0.9);
  if (initial_size == 0) {
    gc(ENC_SYM_NIL, ENC_SYM_NIL);
    num_free = lbm_memory_longest_free();
    initial_size = (lbm_uint)((float)num_free * 0.9);
    if (initial_size == 0) {
      error_ctx(ENC_SYM_MERROR);
      return;
    }
  }

  if ((lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL) &&
      ((ctx->r == ENC_SYM_TYPE_I32) ||
       (ctx->r == ENC_SYM_TYPE_U32) ||
       (ctx->r == ENC_SYM_TYPE_FLOAT) ||
       (ctx->r == ENC_SYM_TYPE_CHAR))) {

    lbm_type t;
    switch (ctx->r) {
    case ENC_SYM_TYPE_I32: t = LBM_TYPE_I32; break;
    case ENC_SYM_TYPE_U32: t = LBM_TYPE_U32; break;
    case ENC_SYM_TYPE_FLOAT: t = LBM_TYPE_FLOAT; break;
    case ENC_SYM_TYPE_CHAR: t = LBM_TYPE_CHAR; break;
    default:
      error_ctx(ENC_SYM_TERROR);
      return;
    }

    if (ctx->r == ENC_SYM_TYPE_CHAR) {
      initial_size = sizeof(lbm_uint) * initial_size;
    }

    lbm_value array;
    if (!lbm_heap_allocate_array(&array, initial_size, t)) {
      error_ctx(ENC_SYM_FATAL_ERROR);
      return;
    }

    CHECK_STACK(lbm_push_5(&ctx->K, array, lbm_enc_u(initial_size), lbm_enc_u(0), ctx->r, stream));
    CHECK_STACK(lbm_push_3(&ctx->K, READ_APPEND_ARRAY, stream, READ_NEXT_TOKEN));
    ctx->app_cont = true;
  } else if (lbm_is_number(ctx->r)) {
    lbm_value array;
    initial_size = sizeof(lbm_uint) * initial_size;
    if (!lbm_heap_allocate_array(&array, initial_size, LBM_TYPE_CHAR)) {
      error_ctx(ENC_SYM_FATAL_ERROR);
      return;
    }

    CHECK_STACK(lbm_push_5(&ctx->K, array, lbm_enc_u(initial_size), lbm_enc_u(0), ENC_SYM_TYPE_CHAR, stream));
    CHECK_STACK(lbm_push(&ctx->K, READ_APPEND_ARRAY));
    ctx->app_cont = true;
  } else {
    lbm_char_channel_t *str = lbm_dec_channel(stream);
    lbm_channel_reader_close(str);
    read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
  }
}

static inline void cont_read_append_array(eval_context_t *ctx) {

  lbm_uint *sptr = lbm_get_stack_ptr(&ctx->K, 5);
  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_value array  = sptr[0];
  lbm_value size   = lbm_dec_as_u32(sptr[1]);
  lbm_value ix     = lbm_dec_as_u32(sptr[2]);
  lbm_value type   = sptr[3];
  lbm_value stream = sptr[4];

  if (ix >= (size - 1)) {
    error_ctx(ENC_SYM_MERROR);
    return;
  }

  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(array); // TODO: Check

  if (lbm_is_number(ctx->r)) {
    switch(type) {
    case ENC_SYM_TYPE_CHAR:
      ((uint8_t*)arr->data)[ix] = (uint8_t)lbm_dec_as_u32(ctx->r);
      break;
    case ENC_SYM_TYPE_I32:
      ((lbm_int*)arr->data)[ix] = lbm_dec_as_i32(ctx->r);
      break;
    case ENC_SYM_TYPE_U32:
      ((lbm_uint*)arr->data)[ix] = lbm_dec_as_u32(ctx->r);
      break;
    case ENC_SYM_TYPE_FLOAT: {
      float f = lbm_dec_as_float(ctx->r);
      memcpy(&arr->data[ix], (uint32_t*)&f, sizeof(float));
    } break;
    default:
      error_ctx(ENC_SYM_TERROR);
      return;
    }
    sptr[2] = lbm_enc_u(ix + 1);
    CHECK_STACK(lbm_push_3(&ctx->K, READ_APPEND_ARRAY, stream, READ_NEXT_TOKEN));
    ctx->app_cont = true;
  } else if (lbm_is_symbol(ctx->r) && lbm_dec_sym(ctx->r) == SYM_CLOSEBRACK) {
    lbm_uint array_size = ix;
    if (type == ENC_SYM_TYPE_CHAR) {
      if (array_size % 4) {
        array_size = (array_size / 4) + 1;
      } else {
        array_size = array_size / 4;
      }
    }
    lbm_memory_shrink((lbm_uint*)arr->data, array_size);
    arr->size = ix;
    lbm_stack_drop(&ctx->K, 5);
    ctx->r = array;
    ctx->app_cont = true;
  } else {
    error_ctx(ENC_SYM_TERROR);
  }
}

static inline void cont_read_append_continue(eval_context_t *ctx) {

  lbm_value *sptr = lbm_get_stack_ptr(&ctx->K, 3);
  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_value first_cell = sptr[0];
  lbm_value last_cell  = sptr[1];
  lbm_value stream     = sptr[2];

  if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL) {

    switch(lbm_dec_sym(ctx->r)) {
    case SYM_CLOSEPAR:
      if (lbm_type_of(last_cell) == LBM_TYPE_CONS) {
        lbm_set_cdr(last_cell, ENC_SYM_NIL); // terminate the list
        ctx->r = first_cell;
      } else {
        ctx->r = ENC_SYM_NIL;
      }
      lbm_stack_drop(&ctx->K, 3);
      /* Skip reading another token and apply the continuation */
      ctx->app_cont = true;
      return;
    case SYM_DOT:
      CHECK_STACK(lbm_push(&ctx->K, READ_DOT_TERMINATE));
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      ctx->app_cont = true;
      return;
    }
  }
  lbm_value new_cell;
  CONS_WITH_GC(new_cell, ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
  if (lbm_type_of(last_cell) == LBM_TYPE_CONS) {
    lbm_set_cdr(last_cell, new_cell);
    last_cell = new_cell;
  } else {
    first_cell = last_cell = new_cell;
  }
  sptr[0] = first_cell;
  sptr[1] = last_cell;
  CHECK_STACK(lbm_push(&ctx->K, READ_APPEND_CONTINUE));
  CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
}

static inline void cont_read_expect_closepar(eval_context_t *ctx) {

  lbm_value res;
  lbm_value stream;

  lbm_pop_2(&ctx->K, &res, &stream);

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL &&
      lbm_dec_sym(ctx->r) == SYM_CLOSEPAR) {
    ctx->r = res;
    ctx->app_cont = true;
  } else {
    lbm_set_error_reason((char*)parse_error_close);
    read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
    done_reading(ctx->id);
  }
}

static inline void cont_read_dot_terminate(eval_context_t *ctx) {

  lbm_value *sptr = lbm_get_stack_ptr(&ctx->K, 3);
  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_value first_cell = sptr[0];
  lbm_value last_cell  = sptr[1];
  lbm_value stream = sptr[2];

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_stack_drop(&ctx->K ,3);

  if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL &&
      (lbm_dec_sym(ctx->r) == SYM_CLOSEPAR ||
       lbm_dec_sym(ctx->r) == SYM_DOT)) {
    lbm_set_error_reason((char*)parse_error_dot);
    read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
    done_reading(ctx->id);
    return;
  } else {
    if (lbm_type_of(last_cell) == LBM_TYPE_CONS) {
      lbm_set_cdr(last_cell, ctx->r);
      ctx->r = first_cell;
      CHECK_STACK(lbm_push_3(&ctx->K,
                             stream,
                             ctx->r,
                             READ_EXPECT_CLOSEPAR));
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      ctx->app_cont = true;
    } else {
      lbm_set_error_reason((char*)parse_error_dot);
      read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
      done_reading(ctx->id);
      return;
    }
  }
}

static inline void cont_read_done(eval_context_t *ctx) {

  lbm_value stream;
  lbm_value prg_tag;

  lbm_pop_2(&ctx->K, &stream, &prg_tag);

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_value tok = lbm_get_next_token(str, false);
  /* May not be absolutely required that we check to
     see if the tokenizer feels it is done here. */
  lbm_channel_reader_close(str);
  if (tok != ENC_SYM_TOKENIZER_DONE) {
    lbm_set_error_reason((char*)parse_error_eof);
    read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
  } else {
    ctx->app_cont = true;
  }
  done_reading(ctx->id);
}

static inline void cont_read_quote_result(eval_context_t *ctx) {
  lbm_value cell1;
  lbm_value cell2;
  CONS_WITH_GC(cell2, ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
  CONS_WITH_GC(cell1, ENC_SYM_QUOTE, cell2, ENC_SYM_NIL);
  ctx->r = cell1;
  ctx->app_cont = true;
}

static inline void cont_read_backquote_result(eval_context_t *ctx) {
  lbm_value expanded = lbm_qq_expand(ctx->r);
  ctx->r = expanded;
  ctx->app_cont = true;
}

static inline void cont_read_commaat_result(eval_context_t *ctx) {
  lbm_value cell1;
  lbm_value cell2;
  CONS_WITH_GC(cell2, ctx->r,ENC_SYM_NIL, ENC_SYM_NIL);
  CONS_WITH_GC(cell1, ENC_SYM_COMMAAT, cell2, ENC_SYM_NIL);
  ctx->r = cell1;
  ctx->app_cont = true;
}

static inline void cont_read_comma_result(eval_context_t *ctx) {
  lbm_value cell1;
  lbm_value cell2;
  CONS_WITH_GC(cell2, ctx->r,ENC_SYM_NIL, ENC_SYM_NIL);
  CONS_WITH_GC(cell1, ENC_SYM_COMMA, cell2, ENC_SYM_NIL);
  ctx->r = cell1;
  ctx->app_cont = true;
}

static inline void cont_read_terminate_colon(eval_context_t *ctx) {
  lbm_value sym;
  lbm_value term = ENC_SYM_NIL;
  lbm_value t1;
  lbm_value t2;
  lbm_value t3;
  lbm_pop(&ctx->K, &sym);

  CONS_WITH_GC(t1, ctx->r, term, sym);
  CONS_WITH_GC(t2, sym, t1, t1);
  CONS_WITH_GC(t3, ENC_SYM_NAMESPACE, t2, t2);
  ctx->r = t3;
  ctx->app_cont = true;
}

static inline void cont_read_check_colon(eval_context_t *ctx) {

  lbm_value r;
  lbm_value stream;
  lbm_pop_2(&ctx->K, &r, &stream);

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_value tok = lbm_get_next_token(str, true);
  if (lbm_type_of(tok) == LBM_TYPE_SYMBOL) {
    switch (lbm_dec_sym(tok)) {
    case SYM_COLON:
      lbm_get_next_token(str,false); //drop the colon;
      CHECK_STACK(lbm_push_2(&ctx->K, r, READ_TERMINATE_COLON));
      CHECK_STACK(lbm_push_2(&ctx->K, stream, READ_NEXT_TOKEN));
      ctx->app_cont = true;
      break;
    default:
      ctx->r = r;
      ctx->app_cont = true;
    }
  } else {
    ctx->r = r;
    ctx->app_cont = true;
  }
}


static inline void cont_application_start(eval_context_t *ctx) {

  lbm_uint *sptr = lbm_get_stack_ptr(&ctx->K, 2);
  if (sptr == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }
  lbm_value args = (lbm_value)sptr[1];

  if (lbm_type_of(ctx->r) == LBM_TYPE_CONS) {
    switch (lbm_car(ctx->r)) {
    case ENC_SYM_MACRO_EXPAND:
      /* (macro-expand (args + (list 1 2 3))) */
      sptr[1] = lbm_cdr(args);
      CHECK_STACK(lbm_push(&ctx->K,
                           EXPAND_MACRO));
      ctx->curr_exp = lbm_car(lbm_car(args));
      break;
    case ENC_SYM_MACRO:{
      /*
       * Perform macro expansion.
       * Macro expansion is really just evaluation in an
       * environment augmented with the unevaluated expressions passed
       * as arguments.
       */
      lbm_value env = (lbm_value)sptr[0];

      lbm_value curr_param = lbm_cadr(ctx->r);
      lbm_value curr_arg = args;
      lbm_value expand_env = env;
      while (lbm_type_of(curr_param) == LBM_TYPE_CONS &&
             lbm_type_of(curr_arg)   == LBM_TYPE_CONS) {

        lbm_value entry;
        WITH_GC(entry,lbm_cons(lbm_car(curr_param),lbm_car(curr_arg)), expand_env,ENC_SYM_NIL);

        lbm_value aug_env;
        WITH_GC(aug_env,lbm_cons(entry, expand_env),expand_env,entry);
        expand_env = aug_env;

        curr_param = lbm_cdr(curr_param);
        curr_arg   = lbm_cdr(curr_arg);
      }
      /* Two rounds of evaluation is performed.
       * First to instantiate the arguments into the macro body.
       * Second to evaluate the resulting program.
       */
      sptr[1] = EVAL_R;
      /* CHECK_STACK(lbm_push_u32_2(&ctx->K, */
      /*                            env, */
      /*                            EVAL_R)); */
      lbm_value exp = lbm_cadr(lbm_cdr(ctx->r));
      ctx->curr_exp = exp;
      ctx->curr_env = expand_env;
      ctx->app_cont = false;
    } break;
    case ENC_SYM_CLOSURE: {
      lbm_value cdr_fun = lbm_cdr(ctx->r);
      lbm_value cddr_fun = lbm_cdr(cdr_fun);
      lbm_value cdddr_fun = lbm_cdr(cddr_fun);
      lbm_value params  = lbm_car(cdr_fun);
      lbm_value exp     = lbm_car(cddr_fun);
      lbm_value clo_env = lbm_car(cdddr_fun);
      lbm_value arg_env = (lbm_value)sptr[0];
      sptr[1] = exp;
      lbm_value *reserved = lbm_stack_reserve(&ctx->K, 4);
      if (!reserved) {
        error_ctx(ENC_SYM_STACK_ERROR);
        return;
      }
      reserved[0] = clo_env;
      reserved[1] = params;
      reserved[2] = lbm_cdr(args);
      reserved[3] = CLOSURE_ARGS;
      ctx->curr_exp = lbm_car(args);
      ctx->curr_env = arg_env;
      ctx->app_cont = false;
    } break;
    default:
      sptr[1] = lbm_enc_u(0);
      CHECK_STACK(lbm_push(&ctx->K,
                           args));
      cont_application_args(ctx);
      break;
    }
  } else {
    sptr[1] = lbm_enc_u(0);
    CHECK_STACK(lbm_push(&ctx->K,
                         args));
    cont_application_args(ctx);
  }
}

static inline void cont_eval_r(eval_context_t* ctx) {

  lbm_value env;
  lbm_pop(&ctx->K, &env);
  ctx->curr_exp = ctx->r;
  ctx->curr_env = env;
  ctx->app_cont = false;
}

/*********************************************************/
/* Evaluator step function                               */

static void evaluation_step(void){
  eval_context_t *ctx = ctx_running;

#ifdef VISUALIZE_HEAP
  heap_vis_gen_image();
#endif

  if (ctx->app_cont) {
    lbm_value k;
    lbm_pop(&ctx->K, &k);
    ctx->app_cont = false;

    switch(k) {
    case DONE:              advance_ctx(); return;
    case SET_GLOBAL_ENV:    cont_set_global_env(ctx); return;
    case PROGN_REST:        cont_progn_rest(ctx); return;
    case WAIT:              cont_wait(ctx); return;
    case APPLICATION_ARGS:  cont_application_args(ctx); return;
    case AND:               cont_and(ctx); return;
    case OR:                cont_or(ctx); return;
    case BIND_TO_KEY_REST:  cont_bind_to_key_rest(ctx); return;
    case IF:                cont_if(ctx); return;
    case MATCH:             cont_match(ctx); return;
    case MATCH_MANY:        cont_match_many(ctx); return;
    case READ:              cont_read(ctx); return;
    case APPLICATION_START: cont_application_start(ctx); return;
    case EVAL_R:            cont_eval_r(ctx); return;
    case SET_VARIABLE:      cont_set_var(ctx); return;
    case RESUME:            cont_resume(ctx); return;
    case EXPAND_MACRO:      cont_expand_macro(ctx); return;
    case CLOSURE_ARGS:      cont_closure_application_args(ctx); return;
    case NAMESPACE_POP:     cont_namespace_pop(ctx); return;
    case EXIT_ATOMIC:       cont_exit_atomic(ctx); return;
    case READ_NEXT_TOKEN:       cont_read_next_token(ctx); return;
    case READ_APPEND_CONTINUE:  cont_read_append_continue(ctx); return;
    case READ_EXPECT_CLOSEPAR:  cont_read_expect_closepar(ctx); return;
    case READ_DOT_TERMINATE:    cont_read_dot_terminate(ctx); return;
    case READ_DONE:             cont_read_done(ctx); return;
    case READ_QUOTE_RESULT:     cont_read_quote_result(ctx); return;
    case READ_BACKQUOTE_RESULT: cont_read_backquote_result(ctx); return;
    case READ_COMMAAT_RESULT:   cont_read_commaat_result(ctx); return;
    case READ_COMMA_RESULT:     cont_read_comma_result(ctx); return;
    case READ_TERMINATE_COLON:  cont_read_terminate_colon(ctx); return;
    case READ_START_ARRAY:      cont_read_start_array(ctx); return;
    case READ_APPEND_ARRAY:     cont_read_append_array(ctx); return;
    case READ_CHECK_COLON:      cont_read_check_colon(ctx); return;
    default:
      error_ctx(ENC_SYM_EERROR);
       return;
    }
  }

  lbm_value head;

  switch (lbm_type_of(ctx->curr_exp)) {

  case LBM_TYPE_SYMBOL: {
    lbm_value s;
    if (ctx->curr_exp == ENC_SYM_NIL) {
      ctx->app_cont = true;
      ctx->r = ENC_SYM_NIL;
      return;
    }

    if (eval_symbol(ctx, &s)) {
      ctx->app_cont = true;
      ctx->r = s;
      return;
    }

    if (dynamic_load_callback) {
      dynamic_load(ctx);
    } else {
      error_ctx(ENC_SYM_NOT_FOUND);
    }
    return;
  }
  case LBM_TYPE_FLOAT: /* fall through */
  case LBM_TYPE_DOUBLE:
  case LBM_TYPE_U32:
  case LBM_TYPE_U64:
  case LBM_TYPE_I32:
  case LBM_TYPE_I64:
  case LBM_TYPE_I:
  case LBM_TYPE_U:
  case LBM_TYPE_CHAR:
  case LBM_TYPE_ARRAY:
  case LBM_TYPE_REF:
  case LBM_TYPE_CHANNEL: eval_selfevaluating(ctx);  return;

  case LBM_TYPE_CONS: {
    head = lbm_car(ctx->curr_exp);

    if (lbm_type_of(head) == LBM_TYPE_SYMBOL) {

      lbm_uint sym_id = lbm_dec_sym(head);

      switch(sym_id) {
      case SYM_QUOTE:   eval_quote(ctx); return;
      case SYM_DEFINE:  eval_define(ctx); return;
      case SYM_PROGN:   eval_progn(ctx); return;
      case SYM_LAMBDA:  eval_lambda(ctx); return;
      case SYM_IF:      eval_if(ctx); return;
      case SYM_LET:     eval_let(ctx); return;
      case SYM_AND:     eval_and(ctx); return;
      case SYM_OR:      eval_or(ctx); return;
      case SYM_MATCH:   eval_match(ctx); return;
      case SYM_RECEIVE: eval_receive(ctx); return;
      case SYM_CALLCC:  eval_callcc(ctx); return;
      case SYM_NAMESPACE: eval_namespace(ctx); return;
      case SYM_ATOMIC:  eval_atomic(ctx); return;

      case SYM_MACRO:   /* fall through */
      case SYM_CONT:
      case SYM_CLOSURE: eval_selfevaluating(ctx); return;

      default: break; /* May be general application form. Checked below*/
      }
    } // If head is symbol
    /*
     * At this point head can be a closure, fundamental, extension or a macro.
     * Anything else would be an error.
     */
    lbm_value *reserved = lbm_stack_reserve(&ctx->K, 3);
    if (!reserved) {
      error_ctx(ENC_SYM_STACK_ERROR);
      return;
    }
    reserved[0] = ctx->curr_env;
    reserved[1] = lbm_cdr(ctx->curr_exp);
    reserved[2] = APPLICATION_START;

    ctx->curr_exp = head; // evaluate the function
  } break;
  default:
    // BUG No applicable case!
    error_ctx(ENC_SYM_EERROR);
    break;
  }
  return;
}

void lbm_pause_eval(void ) {
  eval_cps_next_state_arg = 0;
  eval_cps_next_state = EVAL_CPS_STATE_PAUSED;
}

void lbm_pause_eval_with_gc(uint32_t num_free) {
  eval_cps_next_state_arg = num_free;
  eval_cps_next_state = EVAL_CPS_STATE_PAUSED;
}

void lbm_step_eval(void) {
  eval_cps_next_state_arg = 1;
  eval_cps_next_state = EVAL_CPS_STATE_STEP;
}

void lbm_step_n_eval(uint32_t n) {
  eval_cps_next_state_arg = n;
  eval_cps_next_state = EVAL_CPS_STATE_STEP;
}

void lbm_continue_eval(void) {
  eval_cps_next_state = EVAL_CPS_STATE_RUNNING;
}

void lbm_kill_eval(void) {
  eval_cps_next_state = EVAL_CPS_STATE_KILL;
}

uint32_t lbm_get_eval_state(void) {
  return eval_cps_run_state;
}

/* eval_cps_run can be paused
   I think it would be better use a mailbox for
   communication between other threads and the run_eval
   but for now a set of variables will be used. */
void lbm_run_eval(void){

  while (eval_running) {

    switch (eval_cps_next_state) {
    case EVAL_CPS_STATE_INIT:
      eval_cps_run_state = EVAL_CPS_STATE_RUNNING;
      break;
    case EVAL_CPS_STATE_STEP:
      if (eval_cps_next_state_arg > 1) {
        eval_cps_next_state = EVAL_CPS_STATE_STEP;
        eval_cps_next_state_arg --;
      } else {
        eval_cps_next_state = EVAL_CPS_STATE_PAUSED;
      }
      break;
    case EVAL_CPS_STATE_PAUSED:
      if (eval_cps_run_state != EVAL_CPS_STATE_PAUSED) {
        if (lbm_heap_num_free() < eval_cps_next_state_arg) {
          gc(ENC_SYM_NIL, ENC_SYM_NIL);
        }
        eval_cps_next_state_arg = 0;
      }
      eval_cps_run_state = EVAL_CPS_STATE_PAUSED;
      usleep_callback(EVAL_CPS_MIN_SLEEP);
      continue; /* jump back to start of eval_running loop */
    case EVAL_CPS_STATE_KILL:
      eval_running = false;
      continue;
    default:
      eval_cps_run_state = eval_cps_next_state;
      break;
    }

    eval_context_t *next_to_run = NULL;
    if (eval_steps_quota <= 0 || !ctx_running) {
      uint32_t us = EVAL_CPS_MIN_SLEEP;

      if (is_atomic) {
        if (ctx_running) {
          next_to_run = ctx_running;
          ctx_running = NULL;
        } else {
          is_atomic = false;
          // This is not right!
          // but there is no context available to
          // report an error in.
        }
      } else {
        next_to_run = dequeue_ctx(&sleeping, &us);
      }

      if (!next_to_run) {
        next_to_run = enqueue_dequeue_ctx(&queue, ctx_running);
      } else if (ctx_running) {
        enqueue_ctx(&queue, ctx_running);
      }

      eval_steps_quota = eval_steps_refill;
      ctx_running = next_to_run;

      if (!ctx_running) {
        usleep_callback(us);
        continue;
      }
    }

    eval_steps_quota--;
    evaluation_step();
  }
}

lbm_cid lbm_eval_program(lbm_value lisp) {
  return lbm_create_ctx(lisp, ENC_SYM_NIL, 256);
}

lbm_cid lbm_eval_program_ext(lbm_value lisp, unsigned int stack_size) {
  return lbm_create_ctx(lisp, ENC_SYM_NIL, stack_size);
}

int lbm_eval_init() {
  int res = 1;

  blocked.first = NULL;
  blocked.last = NULL;
  sleeping.first = NULL;
  sleeping.last = NULL;
  queue.first = NULL;
  queue.last = NULL;
  done.first = NULL;
  done.last = NULL;
  ctx_running = NULL;

  eval_cps_run_state = EVAL_CPS_STATE_INIT;

  mutex_init(&qmutex);

  *lbm_get_env_ptr() = ENC_SYM_NIL;
  eval_running = true;

  return res;
}

