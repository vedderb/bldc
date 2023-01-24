/*
    Copyright 2018, 2020, 2021, 2022, 2023 Joel Svensson    svenssonjoel@yahoo.se

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

#define DONE                  ((0 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define SET_GLOBAL_ENV        ((1 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define BIND_TO_KEY_REST      ((2 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define IF                    ((3 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define PROGN_REST            ((4 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define APPLICATION_ARGS      ((5 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define AND                   ((6 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define OR                    ((7 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define WAIT                  ((8 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define MATCH                 ((9 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define MATCH_MANY            ((10 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ                  ((11 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define APPLICATION_START     ((12 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define EVAL_R                ((13 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define SET_VARIABLE          ((14 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define RESUME                ((15 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define CLOSURE_ARGS          ((16 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define EXIT_ATOMIC           ((17 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_NEXT_TOKEN       ((18 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_APPEND_CONTINUE  ((19 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_EXPECT_CLOSEPAR  ((20 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_DOT_TERMINATE    ((21 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_DONE             ((22 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_QUOTE_RESULT     ((23 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_BACKQUOTE_RESULT ((24 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_COMMAAT_RESULT   ((25 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_COMMA_RESULT     ((26 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_START_ARRAY      ((27 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_APPEND_ARRAY     ((28 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define MAP_FIRST             ((29 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define MAP_REST              ((30 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define MATCH_GUARD           ((31 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define NUM_CONTINUATIONS     32

#define FM_NEED_GC       -1
#define FM_NO_MATCH      -2
#define FM_PATTERN_ERROR -3

const char* lbm_error_str_parse_eof = "End of parse stream.";
const char* lbm_error_str_parse_token = "Malformed token.";
const char* lbm_error_str_parse_dot = "Incorrect usage of '.'.";
const char* lbm_error_str_parse_close = "Expected closing parenthesis.";
const char* lbm_error_str_num_args = "Incorrect number of arguments.";
const char* lbm_error_str_forbidden_in_atomic = "Operation is forbidden in an atomic block.";
const char* lbm_error_str_no_number = "Argument(s) must be a number.";
const char* lbm_error_str_not_a_boolean = "Argument must be t or nil (true or false).";
const char* lbm_error_str_incorrect_arg = "Incorrect argument.";

#define CHECK_STACK(x)                          \
  if (!(x)) {                                   \
    error_ctx(ENC_SYM_STACK_ERROR);             \
    return;                                     \
  }

#define WITH_GC(y, x)                           \
  (y) = (x);                                    \
  if (lbm_is_symbol_merror((y))) {              \
    gc();                                       \
    (y) = (x);                                  \
    if (lbm_is_symbol_merror((y))) {            \
      ctx_running->done = true;                 \
      error_ctx(ENC_SYM_MERROR);                \
      return;                                   \
    }                                           \
    /* continue executing statements below */   \
  }
#define WITH_GC_RMBR(y, x, n, ...)              \
  (y) = (x);                                    \
  if (lbm_is_symbol_merror((y))) {              \
    lbm_gc_mark_phase((n), __VA_ARGS__);        \
    gc();                                       \
    (y) = (x);                                  \
    if (lbm_is_symbol_merror((y))) {            \
      ctx_running->done = true;                 \
      error_ctx(ENC_SYM_MERROR);                \
      return;                                   \
    }                                           \
    /* continue executing statements below */   \
  }

#define PRELIMINARY_GC_MEASURE 30

static int gc(void);
static void error_ctx(lbm_value);
static eval_context_t *ctx_running = NULL;

static lbm_value cons_with_gc(lbm_value head, lbm_value tail, lbm_value remember) {
  lbm_value res = lbm_cons(head, tail);
  if (lbm_is_symbol_merror(res)) {
    lbm_gc_mark_phase(1, remember);
    gc();
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

static uint32_t          eval_cps_run_state = EVAL_CPS_STATE_RUNNING;
static volatile uint32_t eval_cps_next_state = EVAL_CPS_STATE_RUNNING;
static volatile uint32_t eval_cps_next_state_arg = 0;
static volatile bool     eval_cps_state_changed = false;

static volatile lbm_event_t *lbm_events = NULL;
static unsigned int lbm_events_head = 0;
static unsigned int lbm_events_tail = 0;
static unsigned int lbm_events_max  = 0;
static bool         lbm_events_full = false;
static mutex_t      lbm_events_mutex;
static bool         lbm_events_mutex_initialized = false;
static volatile lbm_cid  lbm_event_handler_pid = -1;

lbm_cid lbm_get_event_handler_pid(void) {
  return lbm_event_handler_pid;
}

void lbm_set_event_handler_pid(lbm_cid pid) {
  lbm_event_handler_pid = pid;
}

bool lbm_event(lbm_event_t event, uint8_t* opt_array, int opt_array_len) {

  if (lbm_event_handler_pid == -1 || !lbm_events) {
    return false;
  }
  mutex_lock(&lbm_events_mutex);
  if (lbm_events_full) return false;
  if (opt_array != NULL) {
    event.array = lbm_malloc((size_t)opt_array_len);
    event.array_len = opt_array_len;
    if (event.array == NULL) return false;
    memcpy(event.array, opt_array, (size_t)opt_array_len);
  }
  lbm_events[lbm_events_head] = event;

  lbm_events_head = (lbm_events_head + 1) % lbm_events_max;
  mutex_unlock(&lbm_events_mutex);
  return true;
}

static bool lbm_event_pop(lbm_event_t *event) {
  mutex_lock(&lbm_events_mutex);
  if (lbm_events_head == lbm_events_tail && !lbm_events_full) {
    mutex_unlock(&lbm_events_mutex);
    return false;
  }
  *event = lbm_events[lbm_events_tail];
  lbm_events_tail = (lbm_events_tail + 1) % lbm_events_max;
  lbm_events_full = false;
  mutex_unlock(&lbm_events_mutex);
  return true;
}

static unsigned int lbm_event_num(void) {
  mutex_lock(&lbm_events_mutex);
  unsigned int res = lbm_events_max;
  if (!lbm_events_full) {
    if (lbm_events_head >= lbm_events_tail) res = lbm_events_head - lbm_events_tail;
    else res = lbm_events_max - lbm_events_tail + lbm_events_head;
  }
  mutex_unlock(&lbm_events_mutex);
  return res;
}

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


/* Process queues */
static eval_context_queue_t blocked  = {NULL, NULL};
static eval_context_queue_t sleeping = {NULL, NULL};
static eval_context_queue_t queue    = {NULL, NULL};

/* one mutex for all queue operations */
mutex_t qmutex;
bool    qmutex_initialized = false;

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

eval_context_t *lbm_get_current_context(void) {
  return ctx_running;
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
      error == ENC_SYM_NOT_FOUND) {
    lbm_print_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES, ctx_running->curr_exp);
    printf_callback("***\t\t%s\n",buf);
  } else {
    lbm_print_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES, ctx_running->curr_exp);
    printf_callback("***\tAt:\t%s\n",buf);
  }

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
  lbm_memory_free((lbm_uint*)ctx_running->mailbox);
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

// Not possible to CONS_WITH_GC in error_ctx_base (potential loop)
static void error_ctx_base(lbm_value err_val, unsigned int row, unsigned int column) {
  ctx_running->r = err_val;

  if (ctx_running->flags & EVAL_CPS_CONTEXT_FLAG_TRAP) {
    if (lbm_heap_num_free() < 3) {
      gc();
    }

    if (lbm_heap_num_free() >= 3) {
      lbm_value msg = lbm_cons(err_val, ENC_SYM_NIL);
      msg = lbm_cons(lbm_enc_i(ctx_running->id), msg);
      msg = lbm_cons(ENC_SYM_EXIT_ERROR, msg);
      if (lbm_is_symbol_merror(msg)) {
        // If this happens something is pretty seriously wrong.
        print_error_message(err_val, row, column);
      } else {
        lbm_find_receiver_and_send(ctx_running->parent, msg);
      }
    }
  } else {
    print_error_message(err_val, row, column);
  }
  finish_ctx();
}

static void error_ctx(lbm_value err_val) {
  error_ctx_base(err_val, 0, 0);
}

static void read_error_ctx(unsigned int row, unsigned int column) {
  error_ctx_base(ENC_SYM_RERROR, row, column);
}

// successfully finish a context
static void ok_ctx(void) {
  if (ctx_running->flags & EVAL_CPS_CONTEXT_FLAG_TRAP) {
    lbm_value msg;
    CONS_WITH_GC(msg, ctx_running->r, ENC_SYM_NIL, ENC_SYM_NIL);
    CONS_WITH_GC(msg, lbm_enc_i(ctx_running->id), msg, msg);
    CONS_WITH_GC(msg, ENC_SYM_EXIT_OK, msg, msg);
    lbm_find_receiver_and_send(ctx_running->parent, msg);
  }
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

static lbm_cid lbm_create_ctx_parent(lbm_value program, lbm_value env, lbm_uint stack_size, lbm_cid parent, uint32_t context_flags) {

  if (lbm_type_of(program) != LBM_TYPE_CONS) return -1;

  eval_context_t *ctx = NULL;
  ctx = (eval_context_t*)lbm_memory_allocate(sizeof(eval_context_t) / (sizeof(lbm_uint)));
  if (ctx == NULL) {
    lbm_gc_mark_phase(2, program, env);
    gc();
    ctx = (eval_context_t*)lbm_memory_allocate(sizeof(eval_context_t) / (sizeof(lbm_uint)));
  }
  if (ctx == NULL) return -1;

  if (!lbm_stack_allocate(&ctx->K, stack_size)) {
    lbm_gc_mark_phase(2, program, env);
    gc();
    if (!lbm_stack_allocate(&ctx->K, stack_size)) {
      lbm_memory_free((lbm_uint*)ctx);
      return -1;
    }
  }

  lbm_value *mailbox = NULL;
  mailbox = (lbm_value*)lbm_memory_allocate(EVAL_CPS_DEFAULT_MAILBOX_SIZE);
  if (mailbox == NULL) {
    lbm_gc_mark_phase(2, program, env);
    gc();
    mailbox = (lbm_value *)lbm_memory_allocate(EVAL_CPS_DEFAULT_MAILBOX_SIZE);
  }
  if (mailbox == NULL) {
    lbm_stack_free(&ctx->K);
    lbm_memory_free((lbm_uint*)ctx);
    return -1;
  }

  lbm_int cid = lbm_memory_address_to_ix((lbm_uint*)ctx);

  ctx->program = lbm_cdr(program);
  ctx->curr_exp = lbm_car(program);
  ctx->curr_env = env;
  ctx->r = ENC_SYM_NIL;
  ctx->error_reason = NULL;
  ctx->mailbox = mailbox;
  ctx->mailbox_size = EVAL_CPS_DEFAULT_MAILBOX_SIZE;
  ctx->flags = context_flags;
  ctx->num_mail = 0;
  ctx->done = false;
  ctx->app_cont = false;
  ctx->timestamp = 0;
  ctx->sleep_us = 0;
  ctx->prev = NULL;
  ctx->next = NULL;

  ctx->id = cid;
  ctx->parent = parent;

  if (!lbm_push(&ctx->K, DONE)) {
    lbm_memory_free((lbm_uint*)ctx->mailbox);
    lbm_stack_free(&ctx->K);
    lbm_memory_free((lbm_uint*)ctx);
    return -1;
  }

  enqueue_ctx(&queue,ctx);

  return ctx->id;
}

lbm_cid lbm_create_ctx(lbm_value program, lbm_value env, lbm_uint stack_size) {
  // Creates a parentless context.
  return lbm_create_ctx_parent(program,
                               env,
                               stack_size,
                               -1,
                               EVAL_CPS_CONTEXT_FLAG_NOTHING);
}

bool lbm_mailbox_change_size(eval_context_t *ctx, lbm_uint new_size) {

  lbm_value *mailbox = NULL;
  mailbox = (lbm_value*)lbm_memory_allocate(new_size);
  if (mailbox == NULL) {
    gc();
    mailbox = (lbm_value *)lbm_memory_allocate(new_size);
  }
  if (mailbox == NULL) {
    return false;
  }

  for (lbm_uint i = 0; i < ctx->num_mail; i ++ ) {
    mailbox[i] = ctx->mailbox[i];
  }
  lbm_memory_free(ctx->mailbox);
  ctx->mailbox = mailbox;
  ctx->mailbox_size = new_size;
  return true;
}

static void mailbox_remove_mail(eval_context_t *ctx, lbm_uint ix) {

  for (lbm_uint i = ix; i < ctx->num_mail-1; i ++) {
    ctx->mailbox[i] = ctx->mailbox[i+1];
  }
  ctx->num_mail --;
}

static bool mailbox_add_mail(eval_context_t *ctx, lbm_value mail) {

  if (ctx->num_mail >= ctx->mailbox_size) {
    mailbox_remove_mail(ctx, 0);
  }

  ctx->mailbox[ctx->num_mail] = mail;
  ctx->num_mail ++;
  return true;
}

/* Advance execution to the next expression in the program */
static void advance_ctx(eval_context_t *ctx) {

  if (lbm_type_of(ctx->program) == LBM_TYPE_CONS) {
    lbm_push(&ctx->K, DONE);
    ctx->curr_exp = lbm_car(ctx->program);
    ctx->curr_env = ENC_SYM_NIL;
    ctx->program = lbm_cdr(ctx->program);
    ctx->app_cont = false;
  } else {
    ctx->done = true;
    if (ctx_running == ctx) {  // This should always be the case because of odd historical reasons.
      ok_ctx();
    }
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
    if (!mailbox_add_mail(found, msg)) {
      return ENC_SYM_NIL;
    }

    if (found_blocked){
      drop_ctx(&blocked,found);
      drop_ctx(&queue,found);

      enqueue_ctx(&queue,found);
    }
    return ENC_SYM_TRUE;
  }

  /* check the current context */
  if (ctx_running && ctx_running->id == cid) {
    if (!mailbox_add_mail(ctx_running, msg)) {
      return ENC_SYM_NIL;
    }
    return ENC_SYM_TRUE;
  }

  return ENC_SYM_NIL;
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
  }
  return struct_eq(p, e);
}

static int find_match(lbm_value plist, lbm_value *earr, lbm_uint num, lbm_value *e, lbm_value *env) {

  lbm_value curr_p = plist;
  int n = 0;
  bool gc = false;
  for (int i = 0; i < (int)num; i ++ ) {
    lbm_value curr_e = earr[i];
    while (lbm_type_of(curr_p) == LBM_TYPE_CONS) {
      lbm_value me = lbm_car(curr_p);
      if (match(lbm_car(me), curr_e, env, &gc)) {
        if (gc) return FM_NEED_GC;
        *e = lbm_cadr(me);

        if (!lbm_is_symbol_nil(lbm_cadr(lbm_cdr(me)))) {
          return FM_PATTERN_ERROR;
        }
        return n;
      }
      curr_p = lbm_cdr(curr_p);
    }
    curr_p = plist;       /* search all patterns against next exp */
    n ++;
  }

  return FM_NO_MATCH;
}

/****************************************************/
/* Garbage collection                               */
static int gc(void) {

  lbm_uint tstart = 0;
  lbm_uint tend = 0;

  if (timestamp_us_callback) {
    tstart = timestamp_us_callback();
  }

  lbm_gc_state_inc();

  lbm_value *variables = lbm_get_variable_table();
  if (variables) {
    for (int i = 0; i < lbm_get_num_variables(); i ++) {
      lbm_gc_mark_phase(1, variables[i]);
    }
  }
  // The freelist should generally be NIL when GC runs.
  lbm_nil_freelist();
  lbm_gc_mark_phase(1, *lbm_get_env_ptr());

  eval_context_t *curr = queue.first;
  while (curr) {
    lbm_gc_mark_phase(4,
                      curr->curr_env,
                      curr->curr_exp,
                      curr->program,
                      curr->r);
    lbm_gc_mark_aux(curr->mailbox, curr->num_mail);
    lbm_gc_mark_aux(curr->K.data, curr->K.sp);
    curr = curr->next;
  }

  curr = sleeping.first;
  while (curr) {
    lbm_gc_mark_phase(4,
                      curr->curr_env,
                      curr->curr_exp,
                      curr->program,
                      curr->r);
    lbm_gc_mark_aux(curr->mailbox, curr->num_mail);
    lbm_gc_mark_aux(curr->K.data, curr->K.sp);
    curr = curr->next;
  }

  curr = blocked.first;
  while (curr) {
    lbm_gc_mark_phase(4,
                      curr->curr_env,
                      curr->curr_exp,
                      curr->program,
                      curr->r);
    lbm_gc_mark_aux(curr->mailbox, curr->num_mail);
    lbm_gc_mark_aux(curr->K.data, curr->K.sp);
    curr = curr->next;
  }

  if (ctx_running) {
    lbm_gc_mark_phase(4,
                      ctx_running->curr_env,
                      ctx_running->curr_exp,
                      ctx_running->program,
                      ctx_running->r);
    lbm_gc_mark_aux(ctx_running->mailbox, ctx_running->num_mail);
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
  return gc();
}

/****************************************************/
/* Evaluation functions                             */

static bool eval_symbol(eval_context_t *ctx, lbm_value *value) {
  lbm_uint s = lbm_dec_sym(ctx->curr_exp);
  if (s < SPECIAL_SYMBOLS_END) {
    *value = ctx->curr_exp;
    return true;
  }

  if (s >= EXTENSION_SYMBOLS_START &&
      s <  EXTENSION_SYMBOLS_END) {
    if (lbm_get_extension(lbm_dec_sym(ctx->curr_exp)) != NULL) {
      *value = ctx->curr_exp;
      return true;
    }
    return false;
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

static void dynamic_load(eval_context_t *ctx) {
  const char *sym_str = lbm_get_name_by_symbol(lbm_dec_sym(ctx->curr_exp));
  const char *code_str = NULL;
  if (! dynamic_load_callback(sym_str, &code_str)) {
    error_ctx(ENC_SYM_NOT_FOUND);
    return;
  } else {
    CHECK_STACK(lbm_push_3(&ctx->K, ctx->curr_env, ctx->curr_exp, RESUME));

    lbm_value chan;
    if (!create_string_channel((char *)code_str, &chan)) {
      gc();
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
    ctx->curr_env = ENC_SYM_NIL; // dynamics should be evaluable in empty local env
    return;
  }
}


static void eval_quote(eval_context_t *ctx) {
  ctx->r = lbm_cadr(ctx->curr_exp);
  ctx->app_cont = true;
}

static void eval_selfevaluating(eval_context_t *ctx) {
  ctx->r = ctx->curr_exp;
  ctx->app_cont = true;
}

static void eval_progn(eval_context_t *ctx) {
  lbm_value exps = lbm_cdr(ctx->curr_exp);
  lbm_value env  = ctx->curr_env;

  if (lbm_is_cons(exps)) {
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

static void eval_atomic(eval_context_t *ctx) {
  if (is_atomic) {
    lbm_set_error_reason("Atomic blocks cannot be nested!");
    error_ctx(ENC_SYM_EERROR);
    return;
  }

  CHECK_STACK(lbm_push(&ctx->K, EXIT_ATOMIC));
  is_atomic = true;
  eval_progn(ctx);
}


static void eval_callcc(eval_context_t *ctx) {

  lbm_value cont_array;
#ifndef LBM64
  if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U32)) {
    gc();
    if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U32)) {
      error_ctx(ENC_SYM_MERROR);
      return;
    }
  }
#else
  if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U64)) {
    gc();
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

static void eval_define(eval_context_t *ctx) {
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

// (closure params body env)
static bool mk_closure(lbm_value *closure, lbm_value env, lbm_value body, lbm_value params) {

  bool ret = lbm_heap_allocate_list_init(closure,
                                         4,
                                         ENC_SYM_CLOSURE,
                                         params,
                                         body,
                                         env);
  if (!ret) {
    lbm_gc_mark_phase(3, env, body, params);
    gc();
    ret = lbm_heap_allocate_list_init(closure,
                                      4,
                                      ENC_SYM_CLOSURE,
                                      params,
                                      body,
                                      env);
  }
  return ret;
}

static void eval_lambda(eval_context_t *ctx) {
  lbm_value closure;

  if (mk_closure(&closure,
                  ctx->curr_env,
                  lbm_cadr(lbm_cdr(ctx->curr_exp)),
                  lbm_cadr(ctx->curr_exp))) {
    ctx->app_cont = true;
    ctx->r = closure;
  } else {
    error_ctx(ENC_SYM_MERROR);
  }
}

static void eval_if(eval_context_t *ctx) {

  lbm_value cddr = lbm_cddr(ctx->curr_exp);
  lbm_value then_branch = lbm_car(cddr);
  lbm_value else_branch = lbm_cadr(cddr);

  lbm_uint *sptr = lbm_stack_reserve(&ctx->K, 4);
  if (sptr) {
    sptr[0] = else_branch;
    sptr[1] = then_branch;
    sptr[2] = ctx->curr_env;
    sptr[3] = IF;
    ctx->curr_exp = lbm_cadr(ctx->curr_exp);
  } else {
    error_ctx(ENC_SYM_STACK_ERROR);
  }
}

static void eval_cond(eval_context_t *ctx) {
  lbm_value cond1 = lbm_cadr(ctx->curr_exp);

  if (lbm_is_symbol_nil(cond1)) {
    ctx->r = ENC_SYM_NIL;
    ctx->app_cont = true;
  } else {
    uint32_t len = lbm_list_length(cond1);
    if (len != 2) {
      lbm_set_error_reason("Incorrect syntax in cond");
      error_ctx(ENC_SYM_EERROR);
    }
    lbm_value condition = lbm_car(cond1);
    lbm_value body = lbm_cadr(cond1);
    lbm_value rest;
    WITH_GC(rest, lbm_cons(ENC_SYM_COND, lbm_cdr(lbm_cdr(ctx->curr_exp))));
    lbm_uint *sptr = lbm_stack_reserve(&ctx->K, 4);
    if (sptr) {
      sptr[0] = rest;
      sptr[1] = body;
      sptr[2] = ctx->curr_env;
      sptr[3] = IF;
      ctx->curr_exp = condition;
    } else {
      error_ctx(ENC_SYM_STACK_ERROR);
    }
  }
}

static void eval_app_cont(eval_context_t *ctx) {
  lbm_value tmp;
  lbm_pop(&ctx->K, &tmp);
  ctx->app_cont = true;
}

static void eval_let(eval_context_t *ctx) {
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
    WITH_GC_RMBR(binding, lbm_cons(key, val), 1, new_env);
    WITH_GC_RMBR(new_env_tmp, lbm_cons(binding, new_env), 2, new_env, binding);
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

static void eval_and(eval_context_t *ctx) {
  lbm_value rest = lbm_cdr(ctx->curr_exp);
  if (lbm_is_symbol_nil(rest)) {
    ctx->app_cont = true;
    ctx->r = ENC_SYM_TRUE;
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(rest), AND));
    ctx->curr_exp = lbm_car(rest);
  }
}

static void eval_or(eval_context_t *ctx) {
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
static void eval_match(eval_context_t *ctx) {

  lbm_value rest = lbm_cdr(ctx->curr_exp);
  if (lbm_type_of(rest) == LBM_TYPE_SYMBOL &&
      rest == ENC_SYM_NIL) {
    /* Someone wrote the program (match) */
    ctx->app_cont = true;
    ctx->r = ENC_SYM_NIL;
    return;
  } else {
    CHECK_STACK(lbm_push_3(&ctx->K, lbm_cdr(rest), ctx->curr_env, MATCH));
    ctx->curr_exp = lbm_car(rest); /* Evaluate e next*/
  }
}

static void eval_receive(eval_context_t *ctx) {

  if (is_atomic) {
    lbm_set_error_reason((char*)lbm_error_str_forbidden_in_atomic);
    error_ctx(ENC_SYM_EERROR);
    return;
  }

  if (ctx->num_mail == 0) {
    ctx->timestamp = timestamp_us_callback();
    ctx->sleep_us = 0;
    enqueue_ctx(&blocked,ctx);
    ctx_running = NULL;
  } else {
    lbm_value pats = ctx->curr_exp;
    lbm_value *msgs = ctx->mailbox;
    lbm_uint  num   = ctx->num_mail;

    if (lbm_is_symbol_nil(pats)) {
      /* A receive statement without any patterns */
      ctx->app_cont = true;
      ctx->r = ENC_SYM_NIL;
    } else {
      /* The common case */
      lbm_value e;
      lbm_value new_env = ctx->curr_env;
      int n = find_match(lbm_cdr(pats), msgs, num, &e, &new_env);
      if (n == FM_NEED_GC) {
        gc();
        new_env = ctx->curr_env;
        n = find_match(lbm_cdr(pats), msgs, num, &e, &new_env);
        if (n == FM_NEED_GC) {
          ctx_running->done = true;
          error_ctx(ENC_SYM_MERROR);
          return;
        }
      }
      if (n == FM_PATTERN_ERROR) {
        lbm_set_error_reason("Incorrect pattern format for recv");
        error_ctx(ENC_SYM_EERROR);
      } else if (n >= 0 ) { /* Match */
        mailbox_remove_mail(ctx, (lbm_uint)n);
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

static void cont_set_global_env(eval_context_t *ctx){

  lbm_value key;
  lbm_value val = ctx->r;

  lbm_pop(&ctx->K, &key);
  lbm_value new_env;
  // A key is a symbol and should not need to be remembered.
  WITH_GC(new_env, lbm_env_set(*lbm_get_env_ptr(),key,val));

  *lbm_get_env_ptr() = new_env;
  ctx->r = key;

  ctx->app_cont = true;

  return;
}

static void cont_set_var(eval_context_t *ctx) {
  lbm_value key;
  lbm_value val = ctx->r;
  lbm_pop(&ctx->K, &key);

  ctx->r = lbm_set_var(lbm_dec_sym(key), val);
  ctx->app_cont = true;
  return;
}

static void cont_resume(eval_context_t *ctx) {
  lbm_value exp;
  lbm_value env;
  lbm_pop_2(&ctx->K, &exp, &env);
  ctx->curr_exp = exp;
  ctx->curr_env = env;
}

static void cont_progn_rest(eval_context_t *ctx) {
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

static void cont_wait(eval_context_t *ctx) {

  lbm_value cid_val;
  lbm_pop(&ctx->K, &cid_val);
  lbm_cid cid = (lbm_cid)lbm_dec_i(cid_val);

  bool exists = false;

  lbm_blocked_iterator(context_exists, &cid, &exists);
  lbm_running_iterator(context_exists, &cid, &exists);
  lbm_sleeping_iterator(context_exists, &cid, &exists);

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

static void apply_setvar(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
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
         gc();
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

static void apply_read_program(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value chan = ENC_SYM_NIL;
    if (lbm_type_of(args[1]) == LBM_TYPE_ARRAY) {
      if (!create_string_channel(lbm_dec_str(args[1]), &chan)) {
        gc();
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
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    error_ctx(ENC_SYM_EERROR);
  }
}

static void apply_spawn_base(lbm_value *args, lbm_uint nargs, eval_context_t *ctx, uint32_t context_flags) {

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
    WITH_GC_RMBR(entry,lbm_cons(lbm_car(curr_param),args[i]), 1, clo_env);

    lbm_value aug_env;
    WITH_GC_RMBR(aug_env,lbm_cons(entry, clo_env),2, clo_env,entry);
    clo_env = aug_env;
    curr_param = lbm_cdr(curr_param);
    i ++;
  }

  lbm_stack_drop(&ctx->K, nargs+1);

  lbm_value program =  ENC_SYM_NIL;
  CONS_WITH_GC(program, exp, program, clo_env);


  lbm_cid cid = lbm_create_ctx_parent(program,
                                      clo_env,
                                      stack_size,
                                      lbm_get_current_cid(),
                                      context_flags);
  ctx->r = lbm_enc_i(cid);
  ctx->app_cont = true;
}

static void apply_spawn(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  apply_spawn_base(args,nargs,ctx, EVAL_CPS_CONTEXT_FLAG_NOTHING);
}

static void apply_spawn_trap(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  apply_spawn_base(args,nargs,ctx, EVAL_CPS_CONTEXT_FLAG_TRAP);
}

static void apply_yield(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (is_atomic) {
    lbm_set_error_reason((char*)lbm_error_str_forbidden_in_atomic);
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

static void apply_wait(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
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

static void apply_eval(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if ( nargs == 1) {
    ctx->curr_exp = args[1];
    lbm_stack_drop(&ctx->K, nargs+1);
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    error_ctx(ENC_SYM_EERROR);
  }
}

static void apply_eval_program(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value prg = args[1];
    lbm_value app_cont;
    lbm_value app_cont_prg;
    lbm_value new_prg;

    lbm_value prg_copy;

    WITH_GC(prg_copy, lbm_list_copy(prg));
    lbm_stack_drop(&ctx->K, nargs+1);

    if (ctx->K.sp > nargs+2) { // if there is a continuation
      WITH_GC_RMBR(app_cont, lbm_cons(ENC_SYM_APP_CONT, ENC_SYM_NIL), 1, prg_copy);
      WITH_GC_RMBR(app_cont_prg, lbm_cons(app_cont, ENC_SYM_NIL), 2, app_cont, prg_copy);
      new_prg = lbm_list_append(app_cont_prg, ctx->program);
      new_prg = lbm_list_append(prg_copy, new_prg);
    } else {
      new_prg = lbm_list_append(prg_copy, ctx->program);
    }
    if (!lbm_is_list(new_prg)) {
      error_ctx(ENC_SYM_EERROR);
      return;
    }
    CHECK_STACK(lbm_push(&ctx->K, DONE));
    ctx->program = lbm_cdr(new_prg);
    ctx->curr_exp = lbm_car(new_prg);
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    error_ctx(ENC_SYM_EERROR);
  }
}

static void apply_send(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2) {
    lbm_value status = ENC_SYM_TERROR;
    if (lbm_type_of(args[1]) == LBM_TYPE_I) {
      lbm_cid cid = (lbm_cid)lbm_dec_i(args[1]);
      lbm_value msg = args[2];

      WITH_GC(status, lbm_find_receiver_and_send(cid, msg));
    }
    /* return the status */
    lbm_stack_drop(&ctx->K, nargs+1);
    ctx->r = status;
    ctx->app_cont = true;
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    error_ctx(ENC_SYM_EERROR);
  }
}

static void apply_ok(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value ok_val = ENC_SYM_TRUE;
  if (nargs >= 1) {
    ok_val = args[1];
  }
  ctx->r = ok_val;
  ok_ctx();
}

static void apply_error(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value err_val = ENC_SYM_EERROR;
  if (nargs >= 1) {
    err_val = args[1];
  }
  error_ctx(err_val);
}

static void apply_map(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 && lbm_is_list(args[2])) {
    if (lbm_is_symbol_nil(args[2])) {
      lbm_stack_drop(&ctx->K, 3);
      ctx->r = ENC_SYM_NIL;
      ctx->app_cont = true;
      return;
    }
    lbm_value *sptr = lbm_get_stack_ptr(&ctx->K, 3);
    if (!sptr) {
      error_ctx(ENC_SYM_FATAL_ERROR);
      return;
    }

    lbm_value f = args[1];
    lbm_value h = lbm_car(args[2]);
    lbm_value t = lbm_cdr(args[2]);

    CHECK_STACK(lbm_push(&ctx->K, ENC_SYM_NIL));
    lbm_value appli_0;
    WITH_GC(appli_0, lbm_cons(h, ENC_SYM_NIL));
    lbm_value appli_1;
    WITH_GC_RMBR(appli_1, lbm_cons(ENC_SYM_QUOTE, appli_0), 1, appli_0);
    lbm_value appli_2;
    WITH_GC_RMBR(appli_2, lbm_cons(appli_1,ENC_SYM_NIL), 1, appli_1);
    lbm_value appli;
    WITH_GC_RMBR(appli, lbm_cons(f, appli_2), 1, appli_2);
    // cache the function application on the stack for faster
    // successive calls.
    CHECK_STACK(lbm_push_3(&ctx->K, appli, appli_0, MAP_FIRST));
    sptr[0] = t;     // reuse stack space
    sptr[1] = ctx->curr_env;
    sptr[2] = ENC_SYM_NIL;
    ctx->curr_exp = appli;
  } else if (nargs == 1) {
    // Partial application, create a closure.
    lbm_uint sym;
    if (lbm_str_to_symbol("x", &sym)) {
      lbm_value params;
      WITH_GC(params, lbm_cons(lbm_enc_sym(sym), ENC_SYM_NIL));
      lbm_value body_0;
      WITH_GC(body_0, lbm_cons(lbm_enc_sym(sym), ENC_SYM_NIL));
      lbm_value body_1;
      WITH_GC_RMBR(body_1, lbm_cons(args[1], body_0), 1, body_0);
      lbm_value body;
      WITH_GC_RMBR(body, lbm_cons(args[0], body_1), 1, body_0);
      lbm_value closure; ;
      if (mk_closure(&closure, ENC_SYM_NIL, body, params)) {
        ctx->r = closure;
        lbm_stack_drop(&ctx->K, 2);
        ctx->app_cont = true;
      } else {
        error_ctx(ENC_SYM_MERROR);
      }
    } else {
      error_ctx(ENC_SYM_FATAL_ERROR);
    }
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    error_ctx(ENC_SYM_EERROR);
  }
}

static void apply_reverse(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1 && lbm_is_list(args[1])) {
    lbm_value curr = args[1];

    lbm_value new_list = ENC_SYM_NIL;
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      lbm_value tmp;
      WITH_GC_RMBR(tmp, lbm_cons(lbm_car(curr), new_list), 1, new_list);
      new_list = tmp;
      curr = lbm_cdr(curr);
    }
    lbm_stack_drop(&ctx->K, 2);
    ctx->r = new_list;
    ctx->app_cont = true;
  } else {
    lbm_set_error_reason("Reverse requires a list argument");
    error_ctx(ENC_SYM_EERROR);
  }
}

/***************************************************/
/* Application lookup table                        */

typedef void (*apply_fun)(lbm_value *, lbm_uint, eval_context_t *);
static const apply_fun fun_table[] =
  {
   apply_setvar,
   apply_read_program,
   apply_read_program, // Duplication intended
   apply_spawn,
   apply_spawn_trap,
   apply_yield,
   apply_wait,
   apply_eval,
   apply_eval_program,
   apply_send,
   apply_ok,
   apply_error,
   apply_map,
   apply_reverse
  };

/***************************************************/
/* Application of function that takes arguments    */
/* passed over the stack.                          */

static void application(eval_context_t *ctx, lbm_value *fun_args, lbm_uint arg_count) {
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
      lbm_set_error_reason((char*)lbm_error_str_num_args);
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
    lbm_uint sym_val = lbm_dec_sym(fun) - APPLY_FUNS_START;

    if (sym_val <= APPLY_FUNS_END) {
      fun_table[sym_val](fun_args, arg_count, ctx);
    } else if (lbm_is_fundamental(fun)) {
      lbm_uint fund_ix = lbm_dec_sym(fun) -  FUNDAMENTALS_START;

      lbm_value res;
      WITH_GC(res, fundamental_table[fund_ix](&fun_args[1], arg_count, ctx));
      if (lbm_is_error(res)) { //Error other than merror.
        error_ctx(res);
        return;
      }
      lbm_stack_drop(&ctx->K, arg_count+1);
      ctx->app_cont = true;
      ctx->r = res;
    } else {
      // It may be an extension
      extension_fptr f = lbm_get_extension(lbm_dec_sym(fun));
      if (f == NULL) {
        error_ctx(ENC_SYM_EERROR);
        return;
      }

      lbm_value ext_res;
      WITH_GC(ext_res, f(&fun_args[1], arg_count));
      if (lbm_is_error(ext_res)) { //Error other than merror
        error_ctx(ext_res);
        return;
      }
      lbm_stack_drop(&ctx->K, arg_count + 1);

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
  } else {
    error_ctx(ENC_SYM_EERROR);
  }
}

// Caveat: Application of a closure to 0 arguments if
// the same as applying it to NIL.
static void cont_closure_application_args(eval_context_t *ctx) {
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

  if (lbm_is_cons(params)) {
    lbm_value entry;
    WITH_GC(entry,lbm_cons(lbm_car(params),ctx->r));

    lbm_value aug_env;
    WITH_GC_RMBR(aug_env,lbm_cons(entry, clo_env), 1, entry);
    clo_env = aug_env;
  }

  bool a_nil = lbm_is_symbol_nil(args);
  bool p_nil = lbm_is_symbol_nil(lbm_cdr(params));

  if (a_nil && p_nil) {
    // Arguments and parameters match up in number
    lbm_stack_drop(&ctx->K, 5);
    ctx->curr_env = clo_env;
    ctx->curr_exp = exp;
    ctx->app_cont = false;
  } else if (!a_nil && p_nil) {
    // Application with extra arguments
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    error_ctx(ENC_SYM_EERROR);
  } else if (a_nil && !p_nil) {
    // Ran out of arguments, but there are still parameters.
    lbm_value new_env = lbm_list_append(arg_env,clo_env);
    lbm_value closure;
    if (mk_closure(&closure, new_env, exp, lbm_cdr(params))) {
      lbm_stack_drop(&ctx->K, 5);
      ctx->app_cont = true;
      ctx->r = closure;
    } else {
      error_ctx(ENC_SYM_MERROR);
    }
  } else {
    // evaluate the next argument.
    sptr[2] = clo_env;
    sptr[3] = lbm_cdr(params);
    sptr[4] = lbm_cdr(args);
    CHECK_STACK(lbm_push(&ctx->K, CLOSURE_ARGS));
    ctx->curr_exp = lbm_car(args);
    ctx->curr_env = arg_env;
  }
}

static void cont_application_args(eval_context_t *ctx) {

  lbm_uint *sptr = lbm_get_stack_ptr(&ctx->K,3);

  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_value env = sptr[0];
  lbm_value count = sptr[1];
  lbm_value rest = sptr[2];
  lbm_value arg = ctx->r;

  ctx->curr_env = env;
  sptr[0] = arg;
  if (lbm_is_symbol_nil(rest)) {
    // No more arguments
    lbm_stack_drop(&ctx->K, 2);
    lbm_uint nargs = lbm_dec_u(count);
    lbm_value *args = lbm_get_stack_ptr(&ctx->K, nargs + 1);
    if (args) {
      application(ctx,args, nargs);
    } else {
      error_ctx(ENC_SYM_FATAL_ERROR);
      return;
    }
  } else if (lbm_is_cons(rest)) {
    sptr[1] = env;
    sptr[2] = lbm_enc_u(lbm_dec_u(count) + 1);
    CHECK_STACK(lbm_push_2(&ctx->K,lbm_cdr(rest), APPLICATION_ARGS));
    ctx->curr_exp = lbm_car(rest);
  } else {
    error_ctx(ENC_SYM_EERROR);
  }
}

static void cont_and(eval_context_t *ctx) {
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

static void cont_or(eval_context_t *ctx) {
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

static void cont_bind_to_key_rest(eval_context_t *ctx) {
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

  if (lbm_is_cons(rest)) {
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

static void cont_if(eval_context_t *ctx) {

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

static void cont_match_many(eval_context_t *ctx) {

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

static void cont_match(eval_context_t *ctx) {
  lbm_value e = ctx->r;
  lbm_value patterns;
  lbm_value new_env;
  bool  do_gc = false;
  lbm_pop(&ctx->K, &new_env); // restore enclosing environment
  lbm_pop(&ctx->K, &patterns);
  ctx->curr_env = new_env;

  if (lbm_is_symbol_nil(patterns)) {
    /* no more patterns */
    ctx->r = ENC_SYM_NO_MATCH;
    ctx->app_cont = true;
  } else if (lbm_is_cons(patterns)) {
    lbm_value match_case = lbm_car(patterns);
    lbm_value pattern = lbm_car(match_case);
    lbm_value n1      = lbm_cadr(match_case);
    lbm_value n2      = lbm_cadr(lbm_cdr(match_case));
    lbm_value body;
    bool check_guard = false;
    if (lbm_is_symbol_nil(n2)) {
      body = n1;
    } else {
      body = n2;
      check_guard = true;
    }
    if (match(pattern, e, &new_env, &do_gc)) {
      if (check_guard) {
        CHECK_STACK(lbm_push_3(&ctx->K, lbm_cdr(patterns), ctx->curr_env, MATCH));
        CHECK_STACK(lbm_push_4(&ctx->K, new_env, body, e, MATCH_GUARD));
        ctx->curr_env = new_env;
        ctx->curr_exp = n1; // The guard
      } else {
        ctx->curr_env = new_env;
        ctx->curr_exp = body;
      }
    } else if (do_gc) {
      lbm_gc_mark_phase(2, patterns, e);
      gc();
      do_gc = false;
      new_env = ctx->curr_env;
      match(pattern, e, &new_env, &do_gc);
      if (do_gc) {
        ctx_running->done = true;
        error_ctx(ENC_SYM_MERROR);
        return;
      }
      if (check_guard) {
        CHECK_STACK(lbm_push_3(&ctx->K, lbm_cdr(patterns), ctx->curr_env, MATCH));
        CHECK_STACK(lbm_push_4(&ctx->K, new_env, body, e, MATCH_GUARD));
        ctx->curr_env = new_env;
        ctx->curr_exp = n1; // The guard
      } else {
        ctx->curr_env = new_env;
        ctx->curr_exp = body;
      }
    } else {
      /* set up for checking of next pattern */
      CHECK_STACK(lbm_push_3(&ctx->K, lbm_cdr(patterns),ctx->curr_env, MATCH));
      /* leave r unaltered */
      ctx->app_cont = true;
    }
  } else {
    error_ctx(ENC_SYM_TERROR);
  }
}

static void cont_exit_atomic(eval_context_t *ctx) {
  is_atomic = false;
  ctx->app_cont = true;
}

static void cont_map_first(eval_context_t *ctx) {

  lbm_value *sptr = lbm_get_stack_ptr(&ctx->K, 6);
  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }
  lbm_value ls  = sptr[0];
  lbm_value env = sptr[1];

  lbm_value elt;
  CONS_WITH_GC(elt, ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
  sptr[2] = elt; // head of result list
  sptr[3] = elt; // tail of result list
  if (lbm_is_cons(ls)) {
    lbm_value rest = lbm_cdr(ls);
    lbm_value next = lbm_car(ls);
    sptr[0] = rest;
    CHECK_STACK(lbm_push(&ctx->K, MAP_REST));
    lbm_set_car(sptr[5], next); // new arguments
    ctx->curr_exp = sptr[4];
    ctx->curr_env = env;
  } else {
    ctx->r = sptr[2];
    ctx->curr_env = env;
    lbm_stack_drop(&ctx->K, 6);
    ctx->app_cont = true;
  }
}

static void cont_map_rest(eval_context_t *ctx) {
  lbm_value *sptr = lbm_get_stack_ptr(&ctx->K, 6);
  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }
  lbm_value ls  = sptr[0];
  lbm_value env = sptr[1];
  lbm_value t   = sptr[3];

  lbm_value elt;
  CONS_WITH_GC(elt, ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
  lbm_set_cdr(t, elt);
  sptr[3] = elt; // update tail of result list.
  if (lbm_is_cons(ls)) {
    lbm_value rest = lbm_cdr(ls);
    lbm_value next = lbm_car(ls);
    sptr[0] = rest;
    CHECK_STACK(lbm_push(&ctx->K, MAP_REST));
    lbm_set_car(sptr[5], next); // new arguments
    ctx->curr_exp = sptr[4];
    ctx->curr_env = env;
  } else {
    ctx->r = sptr[2]; //head of result list
    ctx->curr_env = env;
    lbm_stack_drop(&ctx->K, 6);
    ctx->app_cont = true;
  }
}

static void cont_match_guard(eval_context_t *ctx) {
  if (lbm_is_symbol_nil(ctx->r)) {
    lbm_value e;
    lbm_pop(&ctx->K, &e);
    lbm_stack_drop(&ctx->K, 2);
    ctx->r = e;
    ctx->app_cont = true;
  } else {
    lbm_value body;
    lbm_value env;
    lbm_stack_drop(&ctx->K, 1);
    lbm_pop_2(&ctx->K, &body, &env);
    lbm_stack_drop(&ctx->K, 3);
    ctx->curr_env = env;
    ctx->curr_exp = body;
  }
}


/****************************************************/
/*   READER                                         */

static void cont_read(eval_context_t *ctx) {

  gc(); // TODO: This should not be necessary

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
    case SYM_TOKENIZER_RERROR:
      lbm_channel_reader_close(str);
      lbm_set_error_reason((char*)lbm_error_str_parse_token);
      read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
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
            internal representation with a closing parenthesis. Then
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
        lbm_channel_reader_close(str);
        lbm_set_error_reason((char*)lbm_error_str_parse_eof);
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
      ctx->r = tok;
      ctx->app_cont = true;
      break;
    }
  } else { // arbitrary value form
    ctx->r = tok;
    ctx->app_cont = true;
  }
}

static void cont_read_next_token(eval_context_t *ctx) {

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

static void cont_read_start_array(eval_context_t *ctx) {

  lbm_value stream;

  lbm_pop(&ctx->K, &stream);

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_uint num_free = lbm_memory_longest_free();
  lbm_uint initial_size = (lbm_uint)((float)num_free * 0.9);
  if (initial_size == 0) {
    gc();
    num_free = lbm_memory_longest_free();
    initial_size = (lbm_uint)((float)num_free * 0.9);
    if (initial_size == 0) {
      lbm_channel_reader_close(str);
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
      lbm_channel_reader_close(str);
      error_ctx(ENC_SYM_TERROR);
      return;
    }

    if (ctx->r == ENC_SYM_TYPE_CHAR) {
      initial_size = sizeof(lbm_uint) * initial_size;
    }

    lbm_value array;
    if (!lbm_heap_allocate_array(&array, initial_size, t)) {
      lbm_channel_reader_close(str);
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
      lbm_channel_reader_close(str);
      error_ctx(ENC_SYM_FATAL_ERROR);
      return;
    }

    CHECK_STACK(lbm_push_5(&ctx->K, array, lbm_enc_u(initial_size), lbm_enc_u(0), ENC_SYM_TYPE_CHAR, stream));
    CHECK_STACK(lbm_push(&ctx->K, READ_APPEND_ARRAY));
    ctx->app_cont = true;
  } else {
    lbm_channel_reader_close(str);
    read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
  }
}

static void cont_read_append_array(eval_context_t *ctx) {

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

static void cont_read_append_continue(eval_context_t *ctx) {

  lbm_value *sptr = lbm_get_stack_ptr(&ctx->K, 3);
  if (!sptr) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

  lbm_value first_cell = sptr[0];
  lbm_value last_cell  = sptr[1];
  lbm_value stream     = sptr[2];

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }

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
  lbm_value new_cell = cons_with_gc(ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
  if (lbm_is_symbol_merror(new_cell)) {
    lbm_channel_reader_close(str);
    return;
  }

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

static void cont_read_expect_closepar(eval_context_t *ctx) {

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
    lbm_channel_reader_close(str);
    lbm_set_error_reason((char*)lbm_error_str_parse_close);
    read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
    done_reading(ctx->id);
  }
}

static void cont_read_dot_terminate(eval_context_t *ctx) {

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
    lbm_channel_reader_close(str);
    lbm_set_error_reason((char*)lbm_error_str_parse_dot);
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
      lbm_channel_reader_close(str);
      lbm_set_error_reason((char*)lbm_error_str_parse_dot);
      read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
      done_reading(ctx->id);
      return;
    }
  }
}

static void cont_read_done(eval_context_t *ctx) {

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
    lbm_channel_reader_close(str);
    lbm_set_error_reason((char*)lbm_error_str_parse_eof);
    read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
  } else {
    ctx->app_cont = true;
  }
  done_reading(ctx->id);
}

static void cont_read_quote_result(eval_context_t *ctx) {
  lbm_value cell1;
  lbm_value cell2;
  CONS_WITH_GC(cell2, ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
  CONS_WITH_GC(cell1, ENC_SYM_QUOTE, cell2, ENC_SYM_NIL);
  ctx->r = cell1;
  ctx->app_cont = true;
}

static void cont_read_backquote_result(eval_context_t *ctx) {
  // The entire expression is in ctx->r
  // and is thus protected from GC
  lbm_value expanded = lbm_qq_expand(ctx->r);
  if (lbm_is_error(expanded)) {
    error_ctx(expanded);
    return;
  }
  ctx->r = expanded;
  ctx->app_cont = true;
}

static void cont_read_commaat_result(eval_context_t *ctx) {
  lbm_value cell1;
  lbm_value cell2;
  CONS_WITH_GC(cell2, ctx->r,ENC_SYM_NIL, ENC_SYM_NIL);
  CONS_WITH_GC(cell1, ENC_SYM_COMMAAT, cell2, ENC_SYM_NIL);
  ctx->r = cell1;
  ctx->app_cont = true;
}

static void cont_read_comma_result(eval_context_t *ctx) {
  lbm_value cell1;
  lbm_value cell2;
  CONS_WITH_GC(cell2, ctx->r,ENC_SYM_NIL, ENC_SYM_NIL);
  CONS_WITH_GC(cell1, ENC_SYM_COMMA, cell2, ENC_SYM_NIL);
  ctx->r = cell1;
  ctx->app_cont = true;
}

static void cont_application_start(eval_context_t *ctx) {

  lbm_uint *sptr = lbm_get_stack_ptr(&ctx->K, 2);
  if (sptr == NULL) {
    error_ctx(ENC_SYM_FATAL_ERROR);
    return;
  }
  lbm_value args = (lbm_value)sptr[1];

  if (lbm_type_of(ctx->r) == LBM_TYPE_CONS) {
    switch (lbm_car(ctx->r)) {
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
        WITH_GC_RMBR(entry,lbm_cons(lbm_car(curr_param),lbm_car(curr_arg)), 1, expand_env);

        lbm_value aug_env;
        WITH_GC_RMBR(aug_env,lbm_cons(entry, expand_env), 2, expand_env, entry);
        expand_env = aug_env;

        curr_param = lbm_cdr(curr_param);
        curr_arg   = lbm_cdr(curr_arg);
      }
      /* Two rounds of evaluation is performed.
       * First to instantiate the arguments into the macro body.
       * Second to evaluate the resulting program.
       */
      sptr[1] = EVAL_R;
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

static void cont_eval_r(eval_context_t* ctx) {
  lbm_value env;
  lbm_pop(&ctx->K, &env);
  ctx->curr_exp = ctx->r;
  ctx->curr_env = env;
  ctx->app_cont = false;
}

/*********************************************************/
/* Continuations table                                   */
typedef void (*cont_fun)(eval_context_t *);

static const cont_fun continuations[NUM_CONTINUATIONS] =
  { advance_ctx,
    cont_set_global_env,
    cont_bind_to_key_rest,
    cont_if,
    cont_progn_rest,
    cont_application_args,
    cont_and,
    cont_or,
    cont_wait,
    cont_match,
    cont_match_many,
    cont_read,
    cont_application_start,
    cont_eval_r,
    cont_set_var,
    cont_resume,
    cont_closure_application_args,
    cont_exit_atomic,
    cont_read_next_token,
    cont_read_append_continue,
    cont_read_expect_closepar,
    cont_read_dot_terminate,
    cont_read_done,
    cont_read_quote_result,
    cont_read_backquote_result,
    cont_read_commaat_result,
    cont_read_comma_result,
    cont_read_start_array,
    cont_read_append_array,
    cont_map_first,
    cont_map_rest,
    cont_match_guard,
  };

/*********************************************************/
/* Evaluators lookup table (special forms)               */
typedef void (*evaluator_fun)(eval_context_t *);

static const evaluator_fun evaluators[] =
  {
   eval_quote,
   eval_define,
   eval_progn,
   eval_lambda,
   eval_if,
   eval_let,
   eval_and,
   eval_or,
   eval_match,
   eval_receive,
   eval_callcc,
   eval_atomic,
   eval_selfevaluating, // macro
   eval_selfevaluating, // cont
   eval_selfevaluating, // closure
   eval_cond,
   eval_app_cont,
  };


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

    lbm_uint decoded_k = lbm_dec_u(k);

    if (decoded_k < NUM_CONTINUATIONS) {
      continuations[decoded_k](ctx);
    } else {
      error_ctx(ENC_SYM_FATAL_ERROR);
    }
    return;
  }

  lbm_uint exp_type = lbm_type_of(ctx->curr_exp);
  if (exp_type == LBM_TYPE_SYMBOL) {
     lbm_value s;

    if (eval_symbol(ctx, &s)) {
      ctx->app_cont = true;
      ctx->r = s;
    } else if (dynamic_load_callback) {
      dynamic_load(ctx);
    } else {
      error_ctx(ENC_SYM_NOT_FOUND);
    }
    return;
  }
  if (exp_type == LBM_TYPE_CONS) {
    lbm_value head = lbm_ref_cell(ctx->curr_exp)->car;

    if (lbm_type_of(head) == LBM_TYPE_SYMBOL) {

      lbm_value eval_index = lbm_dec_sym(head) - SPECIAL_FORMS_START;

      if (eval_index <= (SPECIAL_FORMS_END - SPECIAL_FORMS_START)) {
        evaluators[eval_index](ctx);
        return;
      }
    }
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
    reserved[1] = lbm_ref_cell(ctx->curr_exp)->cdr;
    reserved[2] = APPLICATION_START;

    ctx->curr_exp = head; // evaluate the function
    return;
  }

  eval_selfevaluating(ctx);
  return;
}

void lbm_pause_eval(void ) {
  eval_cps_next_state_arg = 0;
  eval_cps_next_state = EVAL_CPS_STATE_PAUSED;
  if (eval_cps_next_state != eval_cps_run_state) eval_cps_state_changed = true;
}

void lbm_pause_eval_with_gc(uint32_t num_free) {
  eval_cps_next_state_arg = num_free;
  eval_cps_next_state = EVAL_CPS_STATE_PAUSED;
  if (eval_cps_next_state != eval_cps_run_state) eval_cps_state_changed = true;
}

void lbm_continue_eval(void) {
  eval_cps_next_state = EVAL_CPS_STATE_RUNNING;
  if (eval_cps_next_state != eval_cps_run_state) eval_cps_state_changed = true;
}

void lbm_kill_eval(void) {
  eval_cps_next_state = EVAL_CPS_STATE_KILL;
  if (eval_cps_next_state != eval_cps_run_state) eval_cps_state_changed = true;
}

uint32_t lbm_get_eval_state(void) {
  return eval_cps_run_state;
}

static void process_events(void) {

  if (!lbm_events) return;

  if (lbm_event_handler_pid < 0) {
    lbm_events_head = 0;
    lbm_events_tail = 0;
    lbm_events_full = false;
    return;
  }

  unsigned int event_cnt = lbm_event_num();

  lbm_event_t e;

  if (event_cnt > 0) {
    while (lbm_event_pop(&e) && lbm_event_handler_pid >= 0) {
      if (e.type == LBM_EVENT_SYM) {
        lbm_find_receiver_and_send(lbm_event_handler_pid, lbm_enc_sym(e.sym));
      } else if (e.type == LBM_EVENT_SYM_INT) {
        lbm_value msg = lbm_cons(lbm_enc_sym(e.sym), lbm_enc_i(e.i));
        if (lbm_is_symbol_merror(msg)) {
          gc();
          msg = lbm_cons(lbm_enc_sym(e.sym), lbm_enc_i(e.i));
        }
        if (lbm_is_ptr(msg)) {
          lbm_find_receiver_and_send(lbm_event_handler_pid, msg);
        }
      } else if (e.type == LBM_EVENT_SYM_INT_INT) {
        lbm_value ints = lbm_cons(lbm_enc_i(e.i), lbm_enc_i(e.i2));
        if (lbm_is_symbol_merror(ints)) {
          gc();
          ints = lbm_cons(lbm_enc_i(e.i), lbm_enc_i(e.i2));
        }
        lbm_value msg = lbm_cons(lbm_enc_sym(e.sym), ints);
        if (lbm_is_symbol_merror(msg)) {
          lbm_gc_mark_phase(1,ints);
          gc();
          msg = lbm_cons(lbm_enc_sym(e.sym), ints);
        }
        if (lbm_is_ptr(ints) && lbm_is_ptr(msg)) {
          lbm_find_receiver_and_send(lbm_event_handler_pid, msg);
        }
      } else if (e.type == LBM_EVENT_SYM_ARRAY) {
        lbm_value val;
        if (!lbm_lift_array(&val, e.array, LBM_TYPE_BYTE, (size_t)e.array_len)) {
          gc();
          lbm_lift_array(&val, e.array, LBM_TYPE_BYTE, (size_t)e.array_len);
        }
        if (lbm_is_array(val)) {
          lbm_value msg;
          msg = lbm_cons(lbm_enc_sym(e.sym), val);
          if (lbm_is_symbol_merror(msg)) {
            lbm_gc_mark_phase(1, val);
            gc();
            msg = lbm_cons(lbm_enc_sym(e.sym), val);
          }
          if (!lbm_is_symbol_merror(msg)) {
            lbm_find_receiver_and_send(lbm_event_handler_pid, msg);
          } else {
            lbm_heap_explicit_free_array(val);
          }
        }
      } else if (e.type == LBM_EVENT_SYM_INT_ARRAY) {
        lbm_value val;
        if (!lbm_lift_array(&val, e.array,  LBM_TYPE_BYTE, (size_t)e.array_len)) {
          gc();
          lbm_lift_array(&val, e.array,  LBM_TYPE_BYTE, (size_t)e.array_len);
        }
        if (lbm_is_array(val)) {
          lbm_value msg_data;
          msg_data = lbm_cons(lbm_enc_i32(e.i),val);
          if (lbm_is_symbol_merror(msg_data)) {
            lbm_gc_mark_phase(1,val);
            gc();
            msg_data = lbm_cons(lbm_enc_i32(e.i), val);
          }
          if (!lbm_is_symbol_merror(msg_data)) {
            lbm_value msg;
            msg = lbm_cons(lbm_enc_sym(e.sym), msg_data);
            if (lbm_is_symbol_merror(msg)) {
              lbm_gc_mark_phase(1, msg_data);
              gc();
              msg = lbm_cons(lbm_enc_sym(e.sym), msg_data);
            }
            if (!lbm_is_symbol_merror(msg)) {
              lbm_find_receiver_and_send(lbm_event_handler_pid, msg);
            } else {
              lbm_heap_explicit_free_array(val);
            }
          }
        }
      }
    }
  }
}

/* eval_cps_run can be paused
   I think it would be better use a mailbox for
   communication between other threads and the run_eval
   but for now a set of variables will be used. */
void lbm_run_eval(void){

  while (eval_running) {
    eval_cps_state_changed = false;
    switch (eval_cps_next_state) {
    case EVAL_CPS_STATE_PAUSED:
      if (eval_cps_run_state != EVAL_CPS_STATE_PAUSED) {
        if (lbm_heap_num_free() < eval_cps_next_state_arg) {
          gc();
        }
        eval_cps_next_state_arg = 0;
      }
      eval_cps_run_state = EVAL_CPS_STATE_PAUSED;
      usleep_callback(EVAL_CPS_MIN_SLEEP);
      continue; /* jump back to start of eval_running loop */
    case EVAL_CPS_STATE_KILL:
      eval_running = false;
      continue;
    default: // running state
      eval_cps_run_state = eval_cps_next_state;
      break;
    }

    while (true) {
      eval_context_t *next_to_run = NULL;
      if (eval_steps_quota && ctx_running) {
        eval_steps_quota--;
        evaluation_step();
      } else {
        if (eval_cps_state_changed) break;
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
          process_events();
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
    }
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

  if (!qmutex_initialized) {
    mutex_init(&qmutex);
  }
  if (!lbm_events_mutex_initialized) {
    mutex_init(&lbm_events_mutex);
  }

  mutex_lock(&qmutex);
  mutex_lock(&lbm_events_mutex);

  blocked.first = NULL;
  blocked.last = NULL;
  sleeping.first = NULL;
  sleeping.last = NULL;
  queue.first = NULL;
  queue.last = NULL;
  ctx_running = NULL;

  eval_cps_run_state = EVAL_CPS_STATE_RUNNING;

  mutex_unlock(&qmutex);
  mutex_unlock(&lbm_events_mutex);

  *lbm_get_env_ptr() = ENC_SYM_NIL;
  eval_running = true;

  return res;
}

bool lbm_eval_init_events(unsigned int num_events) {

  mutex_lock(&lbm_events_mutex);
  lbm_events = (lbm_event_t*)lbm_malloc(num_events * sizeof(lbm_event_t));

  if (!lbm_events) return false;
  lbm_events_max = num_events;
  lbm_events_head = 0;
  lbm_events_tail = 0;
  lbm_events_full = false;
  lbm_event_handler_pid = -1;
  mutex_unlock(&lbm_events_mutex);
  return true;
}
