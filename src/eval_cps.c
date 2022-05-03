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
#include "streams.h"
#include "tokpar.h"
#include "qq_expand.h"
#include "lbm_variables.h"
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
#define QUOTE_RESULT      ((19 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define BACKQUOTE_RESULT  ((20 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define COMMAAT_RESULT    ((21 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define COMMA_RESULT      ((22 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define DOT_TERMINATE     ((23 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define EXPECT_CLOSEPAR   ((24 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define APPEND_CONTINUE   ((25 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READ_DONE         ((26 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define CLOSURE_ARGS      ((27 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define CLOSURE_APP       ((28 << LBM_VAL_SHIFT) | LBM_TYPE_U)

#define CHECK_STACK(x)                          \
  if (!(x)) {                                   \
    error_ctx(lbm_enc_sym(SYM_STACK_ERROR));    \
    return;                                     \
  }

#define WITH_GC(y, x, remember1,remember2)      \
  (y) = (x);                                    \
  if (lbm_is_symbol_merror((y))) {              \
    gc(remember1, remember2);                   \
    (y) = (x);                                  \
    if (lbm_is_symbol_merror((y))) {            \
      ctx_running->done = true;                 \
      error_ctx(lbm_enc_sym(SYM_MERROR));       \
      return;                                   \
    }                                           \
    /* continue executing statements below */   \
  }

#define PRELIMINARY_GC_MEASURE 30

static int gc(lbm_value, lbm_value);
static lbm_value NIL;
static lbm_value NONSENSE;
static void error_ctx(lbm_value);
static eval_context_t *ctx_running = NULL;

static inline lbm_value cons_with_gc(lbm_value head, lbm_value tail, lbm_value remember) {
  lbm_value res = lbm_cons(head, tail);
  if (lbm_is_symbol_merror(res)) {
    gc(remember, NIL);
    res = lbm_cons(head, tail);
    if (lbm_is_symbol_merror(res)) {
        ctx_running->done = true;
        error_ctx(lbm_enc_sym(SYM_MERROR));
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

static uint32_t eval_cps_run_state = EVAL_CPS_STATE_INIT;
volatile uint32_t eval_cps_next_state = EVAL_CPS_STATE_INIT;
volatile uint32_t eval_cps_next_state_arg = 0;

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

static bool     eval_running = false;
static uint32_t next_ctx_id = 1;

static volatile bool     blocking_extension = false;

typedef struct {
  eval_context_t *first;
  eval_context_t *last;
} eval_context_queue_t;


/* Callbacks and task queue */
static eval_context_queue_t blocked = {NULL, NULL};
static eval_context_queue_t queue   = {NULL, NULL};
static eval_context_queue_t done    = {NULL, NULL};

/* one mutex for all queue operations */
mutex_t qmutex;

static void (*usleep_callback)(uint32_t) = NULL;
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


void print_error_message(lbm_value error) {
  if (!printf_callback) return;

  /* try to allocate a lbm_print_value buffer on the lbm_memory */
  lbm_uint* buf32 = lbm_memory_allocate(ERROR_MESSAGE_BUFFER_SIZE_BYTES / (sizeof(lbm_uint)));
  if (!buf32) {
    printf_callback("Error: Not enough free memory to create a human readable error message\n");
    return;
  }
  char *buf = (char*)buf32;

  lbm_print_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES, error);
  printf_callback("***\tError: %s\n\n", buf);

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

static lbm_value token_stream_more(lbm_stream_t *str) {
  (void) str;
  return lbm_enc_sym(SYM_NIL);
}

static lbm_value token_stream_get(lbm_stream_t *str){
  return lbm_get_next_token((lbm_tokenizer_char_stream_t*)str->state);
}

static lbm_value token_stream_peek(lbm_stream_t *str, lbm_value n){
  (void) str;
  (void) n;
  return lbm_enc_sym(SYM_NIL);
}

static lbm_value token_stream_drop(lbm_stream_t *str, lbm_value n){
  (void) str;
  (void) n;
  return lbm_enc_sym(SYM_NIL);
}

static lbm_value token_stream_put(lbm_stream_t *str, lbm_value v){
  (void) str;
  (void) v;
  return lbm_enc_sym(SYM_NIL);
}

lbm_value lbm_create_token_stream(lbm_tokenizer_char_stream_t *str) {

  lbm_stream_t *stream;
  stream = (lbm_stream_t*)lbm_memory_allocate(1+ sizeof(lbm_stream_t) / (sizeof(lbm_uint)));

  if (stream == NULL) {
    return lbm_enc_sym(SYM_MERROR);
  }

  /* gc can check if the state pointer is inside of the
     lispbm_memory and free it if that is that case.
     Otherwise we assume it is a statically created tokenizer */
  stream->state = (void*)str;
  stream->more = token_stream_more;
  stream->get  = token_stream_get;
  stream->peek = token_stream_peek;
  stream->drop = token_stream_drop;
  stream->put  = token_stream_put;

  return lbm_stream_create(stream);
}

int lbm_explicit_free_token_stream(lbm_value stream) {
  int r = 0;
  if (lbm_is_stream(stream)) {

    lbm_stream_t *str = (lbm_stream_t*)lbm_car(stream);

    lbm_memory_free((lbm_uint*)str);
    stream = lbm_set_ptr_type(stream, LBM_TYPE_CONS);
    lbm_set_car(stream, lbm_enc_sym(SYM_NIL));
    lbm_set_cdr(stream, lbm_enc_sym(SYM_NIL));

    r = 1;
  }
  return r;
}

lbm_value token_stream_from_string_value(lbm_value s) {
  char *str = lbm_dec_str(s);

  lbm_stream_t *stream = NULL;
  lbm_tokenizer_string_state_t *tok_stream_state = NULL;
  lbm_tokenizer_char_stream_t *tok_stream = NULL;

  stream = (lbm_stream_t*)lbm_memory_allocate(sizeof(lbm_stream_t) / (sizeof(lbm_uint)));
  if (stream == NULL) {
    return lbm_enc_sym(SYM_MERROR);
  }

  tok_stream_state = (lbm_tokenizer_string_state_t*)lbm_memory_allocate(1 + (sizeof(lbm_tokenizer_string_state_t) / (sizeof(lbm_uint))));
  if (tok_stream_state == NULL) {
    lbm_memory_free((lbm_uint*)stream);
    return lbm_enc_sym(SYM_MERROR);
  }

  tok_stream = (lbm_tokenizer_char_stream_t*)lbm_memory_allocate(sizeof(lbm_tokenizer_char_stream_t) / (sizeof(lbm_uint)));
  if (tok_stream == NULL) {
    lbm_memory_free((lbm_uint*)stream);
    lbm_memory_free((lbm_uint*)tok_stream_state);
    return lbm_enc_sym(SYM_MERROR);
  }

  lbm_create_char_stream_from_string(tok_stream_state,
                                     tok_stream,
                                     str);

  stream->state = (void*)tok_stream;
  stream->more = token_stream_more;
  stream->get  = token_stream_get;
  stream->peek = token_stream_peek;
  stream->drop = token_stream_drop;
  stream->put  = token_stream_put;

  return lbm_stream_create(stream);
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
  print_error_message(err_val);
  ctx_running->r = err_val;
  finish_ctx();
}

static eval_context_t *dequeue_ctx(uint32_t *us) {
  lbm_uint min_us = DEFAULT_SLEEP_US;
  lbm_uint t_now;

  mutex_lock(&qmutex);

  if (timestamp_us_callback) {
    t_now = timestamp_us_callback();
  } else {
    t_now = 0;
  }

  eval_context_t *curr = queue.first; //ctx_queue;

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
      if (curr == queue.last) {
        if (curr->prev) {
          queue.last = curr->prev;
          queue.last->next = NULL;
        } else {
          queue.first = NULL;
          queue.last = NULL;
        }
      } else if (curr->prev == NULL) {
        queue.first = curr->next;
        if (queue.first) {
          queue.first->prev = NULL;
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
  *us = DEFAULT_SLEEP_US;
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
  ctx_running->r = lbm_enc_sym(SYM_TRUE);
  ctx_running->app_cont = true;
  enqueue_ctx(&queue,ctx_running);
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
  ctx->r = lbm_enc_sym(SYM_NIL);
  ctx->error_reason = NULL;
  ctx->mailbox = lbm_enc_sym(SYM_NIL);
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
    ctx_running->curr_env = lbm_enc_sym(SYM_NIL);
    ctx_running->program = lbm_cdr(ctx_running->program);
    ctx_running->r = NIL;
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

  found = lookup_ctx(&blocked, cid);

  if (found == NULL) {
    found = lookup_ctx(&queue, cid);
  }

  if (found) {
    lbm_value new_mailbox = lbm_cons(msg, found->mailbox);

    if (lbm_type_of(new_mailbox) == LBM_TYPE_SYMBOL) {
      return new_mailbox; /* An error symbol */
    }

    found->mailbox = new_mailbox;

    drop_ctx(&blocked,found);
    drop_ctx(&queue,found);

    enqueue_ctx(&queue,found);
    return lbm_enc_sym(SYM_TRUE);
  }

  /* check the current context */
  if (ctx_running && ctx_running->id == cid) {
    lbm_value new_mailbox = lbm_cons(msg, ctx_running->mailbox);

    if (lbm_type_of(new_mailbox) == LBM_TYPE_SYMBOL) {
      return new_mailbox; /* An error symbol */
    }
    ctx_running->mailbox = new_mailbox;
    return lbm_enc_sym(SYM_TRUE);
  }

  return lbm_enc_sym(SYM_NIL);
}

static lbm_value remove_from_list(int n, lbm_value list) {
  int c = 0;
  lbm_value res;
  lbm_value curr = list;

  lbm_value tmp = lbm_enc_sym(SYM_NIL);

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
  } else if (lbm_type_of(p) == LBM_TYPE_U32 &&
             lbm_type_of(e) == LBM_TYPE_U32 &&
             lbm_dec_u32(p) == lbm_dec_u32(e)) {
    return true;
  } else if (lbm_type_of(p) == LBM_TYPE_I32 &&
      lbm_type_of(e) == LBM_TYPE_I32 &&
      lbm_dec_i32(p) == lbm_dec_i32(e)) {
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
  return gc(NIL,NIL);
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
  } else if (lbm_env_lookup_b(value, ctx->curr_exp, *lbm_get_env_ptr())) {
    return true;
  }
  return false;
}

static inline void dynamic_load(eval_context_t *ctx) {

  const char *sym_str = lbm_get_name_by_symbol(lbm_dec_sym(ctx->curr_exp));
  const char *code_str = NULL;
  if (! dynamic_load_callback(sym_str, &code_str)) {
    error_ctx(lbm_enc_sym(SYM_NOT_FOUND));
    return;
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, ctx->curr_exp, RESUME));

    lbm_value cell = lbm_heap_allocate_cell(LBM_TYPE_CONS);

    if (lbm_type_of(cell) == LBM_TYPE_SYMBOL) {
      gc(NIL,NIL);
      cell = lbm_heap_allocate_cell(LBM_TYPE_CONS);
      if (lbm_type_of(cell) == LBM_TYPE_SYMBOL) {
        error_ctx(cell);
        return;
      }
    }

    lbm_array_header_t *array = (lbm_array_header_t*)lbm_memory_allocate(sizeof(lbm_array_header_t) / (sizeof(lbm_uint)));

    if (array == NULL) {
      gc(cell,NIL);
      array = (lbm_array_header_t*)lbm_memory_allocate(sizeof(lbm_array_header_t) / (sizeof(lbm_uint)));
      if (array == NULL) {
        error_ctx(lbm_enc_sym(SYM_MERROR));
        return;
      }
    }

    array->data = (lbm_uint*)code_str;
    array->elt_type = LBM_TYPE_CHAR;
    array->size = strlen(code_str);

    lbm_set_car(cell, (lbm_uint)array);
    lbm_set_cdr(cell, lbm_enc_sym(SYM_ARRAY_TYPE));

    cell = cell | LBM_TYPE_ARRAY;

    lbm_value stream = token_stream_from_string_value(cell);
    if (lbm_type_of(stream) == LBM_TYPE_SYMBOL) {
      gc(cell,NIL);
      stream = token_stream_from_string_value(cell);
      if (lbm_type_of(stream) == LBM_TYPE_SYMBOL) {
        error_ctx(stream);
        return;
      }
    }

    lbm_value loader = NIL;
    CONS_WITH_GC(loader, stream, loader, stream);
    CONS_WITH_GC(loader, lbm_enc_sym(SYM_READ), loader, loader);
    lbm_value evaluator = NIL;
    CONS_WITH_GC(evaluator, loader, evaluator, loader);
    CONS_WITH_GC(evaluator, lbm_enc_sym(SYM_EVAL), evaluator, evaluator);
    ctx->curr_exp = evaluator;
    return;
  }
}

static inline void eval_selfevaluating(eval_context_t *ctx) {
  ctx->r = ctx->curr_exp;
  ctx->app_cont = true;
}

static inline void eval_quote(eval_context_t *ctx) {
  ctx->r = lbm_cadr(ctx->curr_exp);
  ctx->app_cont = true;
}

static inline void eval_macro(eval_context_t *ctx) {
  ctx->r = ctx->curr_exp;
  ctx->app_cont = true;
}

static inline void eval_closure(eval_context_t *ctx) {
  ctx->r = ctx->curr_exp;
  ctx->app_cont = true;
}

static inline void eval_callcc(eval_context_t *ctx) {

  //lbm_value continuation = NIL;

  lbm_value cont_array;
#ifndef LBM64
  if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U32)) {
    gc(NIL,NIL);
    if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U32)) {
      error_ctx(lbm_enc_sym(SYM_MERROR));
      return;
    }
  }
#else
  if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U64)) {
    gc(NIL,NIL);
    if (!lbm_heap_allocate_array(&cont_array, ctx->K.sp, LBM_TYPE_U32)) {
      error_ctx(lbm_enc_sym(SYM_MERROR));
      return;
    }
  }
#endif

  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(cont_array);
  memcpy(arr->data, ctx->K.data, ctx->K.sp * sizeof(lbm_uint));

  lbm_value acont;
  CONS_WITH_GC(acont, lbm_enc_sym(SYM_CONT), cont_array, cont_array);

  /* Create an application */
  lbm_value fun_arg = lbm_cadr(ctx->curr_exp);
  lbm_value app = NIL;
  CONS_WITH_GC(app, acont, app, acont);
  CONS_WITH_GC(app, fun_arg, app, app);

  //ctx->r = NIL;
  ctx->curr_exp = app;
  ctx->app_cont = false;
}

static inline void eval_continuation(eval_context_t *ctx) {
  ctx->r = ctx->curr_exp;
  ctx->app_cont = true;
}

static inline void eval_define(eval_context_t *ctx) {
  lbm_value args = lbm_cdr(ctx->curr_exp);
  lbm_value key = lbm_car(args);
  lbm_value rest_args = lbm_cdr(args);
  lbm_value val_exp = lbm_car(rest_args);

  lbm_uint *sptr = lbm_stack_reserve(&ctx->K, 2);
  if (!sptr) {
    error_ctx(lbm_enc_sym(SYM_STACK_ERROR));
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
  error_ctx(lbm_enc_sym(SYM_EERROR));
  return;
}


static inline void eval_progn(eval_context_t *ctx) {
  lbm_value exps = lbm_cdr(ctx->curr_exp);
  lbm_value env  = ctx->curr_env;

  if (lbm_is_list(exps)) {
    lbm_uint *sptr = lbm_stack_reserve(&ctx->K, 3);
    if (!sptr) {
      error_ctx(lbm_enc_sym(SYM_STACK_ERROR));
      return;
    }
    sptr[0] = env;
    sptr[1] = lbm_cdr(exps);
    sptr[2] = PROGN_REST;
    ctx->curr_exp = lbm_car(exps);
    ctx->curr_env = env;
  } else if (lbm_is_symbol_nil(exps)) {
    ctx->r = NIL;
    ctx->app_cont = true;
  } else {
    error_ctx(lbm_enc_sym(SYM_EERROR));
  }
}

static inline void eval_lambda(eval_context_t *ctx) {

  lbm_value env_cpy = ctx->curr_env;
  lbm_value env_end;
  lbm_value body;
  lbm_value params;
  lbm_value closure;
  CONS_WITH_GC(env_end, env_cpy, NIL, env_cpy);
  CONS_WITH_GC(body, lbm_cadr(lbm_cdr(ctx->curr_exp)), env_end, env_end);
  CONS_WITH_GC(params, lbm_cadr(ctx->curr_exp), body, body);
  CONS_WITH_GC(closure, lbm_enc_sym(SYM_CLOSURE), params, params);

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
    error_ctx(lbm_enc_sym(SYM_STACK_ERROR));
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
    lbm_value val = NIL;
    lbm_value binding;
    lbm_value new_env_tmp;
    WITH_GC(binding, lbm_cons(key, val), new_env, NIL);
    WITH_GC(new_env_tmp, lbm_cons(binding, new_env), new_env, binding);
    new_env = new_env_tmp;
    curr = lbm_cdr(curr);
  }

  lbm_value key0 = lbm_car(lbm_car(binds));
  lbm_value val0_exp = lbm_cadr(lbm_car(binds));

  lbm_uint *sptr = lbm_stack_reserve(&ctx->K, 5);
  if (!sptr) {
    error_ctx(lbm_enc_sym(SYM_STACK_ERROR));
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
    ctx->r = lbm_enc_sym(SYM_TRUE);
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(rest), AND));
    ctx->curr_exp = lbm_car(rest);
  }
}

static inline void eval_or(eval_context_t *ctx) {
  lbm_value rest = lbm_cdr(ctx->curr_exp);
  if (lbm_is_symbol_nil(rest)) {
    ctx->app_cont = true;
    ctx->r = lbm_enc_sym(SYM_NIL);
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
      rest == NIL) {
    /* Someone wrote the program (match) */
    ctx->app_cont = true;
    ctx->r = lbm_enc_sym(SYM_NIL);
    return;
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(rest), MATCH));
    ctx->curr_exp = lbm_car(rest); /* Evaluate e next*/
  }
}

static inline void eval_receive(eval_context_t *ctx) {

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
      ctx->r = lbm_enc_sym(SYM_NIL);
    } else {
      /* The common case */
      lbm_value e;
      lbm_value new_env = ctx->curr_env;
      bool do_gc = false;
      int n = find_match(lbm_cdr(pats), msgs, &e, &new_env, &do_gc);
      if (do_gc) {
        gc(NIL, NIL);
        do_gc = false;
        n = find_match(lbm_cdr(pats), msgs, &e, &new_env, &do_gc);
        if (do_gc) {
          ctx_running->done = true;
          error_ctx(lbm_enc_sym(SYM_MERROR));
          return;
        }
      }
      if (n >= 0 ) { /* Match */
        lbm_value new_mailbox;
        WITH_GC(new_mailbox, remove_from_list(n, msgs), NIL, NIL);

        ctx->mailbox = new_mailbox;
        ctx->curr_env = new_env;
        ctx->curr_exp = e;
      } else { /* No match  go back to sleep */
        ctx->timestamp = timestamp_us_callback();
        ctx->sleep_us = 0;
        enqueue_ctx(&blocked,ctx);
        ctx_running = NULL;
        ctx->r = lbm_enc_sym(SYM_NO_MATCH);
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
  WITH_GC(new_env, lbm_env_set(*lbm_get_env_ptr(),key,val), key, NIL);

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
    error_ctx(lbm_enc_sym(SYM_TERROR));
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
      WITH_GC(entry,lbm_cons(lbm_car(curr_param),lbm_car(curr_arg)), expand_env,NIL);

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
  error_ctx(lbm_enc_sym(SYM_EERROR));
}

static inline void cont_progn_rest(eval_context_t *ctx) {
  lbm_value rest;
  lbm_value env;

  lbm_value *sptr = (lbm_value*)lbm_get_stack_ptr(&ctx->K, 2);
  if (sptr == NULL) {
    error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
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
    ctx->r = lbm_enc_sym(SYM_TRUE);
    ctx->app_cont = true;
    yield_ctx(50000);
  } else {
    ctx->r = lbm_enc_sym(SYM_TRUE);
    ctx->app_cont = true;
  }
}

static inline void cont_application(eval_context_t *ctx) {
  lbm_value count;
  lbm_pop(&ctx->K, &count);

  lbm_uint arg_count = lbm_dec_u(count);

  lbm_uint *fun_args = lbm_get_stack_ptr(&ctx->K, arg_count+1);

  if (fun_args == NULL) {
    ctx->r = lbm_enc_sym(SYM_FATAL_ERROR);
    return;
  }
  lbm_value fun = fun_args[0];
  if (lbm_is_continuation(fun)) {

    lbm_value c = lbm_cdr(fun); /* should be the continuation */

    if (!lbm_is_array(c)) {
      error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
      return;
    }

    lbm_value arg = NIL;
    if (arg_count == 1) {
      arg = fun_args[1];
    } else if (arg_count > 1) {
      lbm_set_error_reason("A continuation created by call-cc was applied to too many arguments (>1)");
      error_ctx(lbm_enc_sym(SYM_EERROR));
      return;
    }
    lbm_stack_clear(&ctx->K);

    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(c);

    ctx->K.sp = arr->size;
    memcpy(ctx->K.data, arr->data, arr->size * sizeof(lbm_uint));

    ctx->r = arg;
    ctx->app_cont = true;
    return;
  } else if (lbm_type_of(fun) == LBM_TYPE_SYMBOL) {

    /* eval_cps specific operations */
    lbm_uint dfun = lbm_dec_sym(fun);

    switch(dfun) {
    case SYM_SETVAR: {
      lbm_uint cnt = lbm_dec_u(count);

      if (cnt == 2 && lbm_is_symbol(fun_args[1])) {

        lbm_uint s = lbm_dec_sym(fun_args[1]);
        if (s >= VARIABLE_SYMBOLS_START &&
            s <  VARIABLE_SYMBOLS_END) {
          /* #var case ignores local/global if present */
          ctx->r = lbm_set_var(s, fun_args[2]);
        } else if (s >= RUNTIME_SYMBOLS_START) {
          lbm_value new_env = lbm_env_modify_binding(ctx->curr_env, fun_args[1], fun_args[2]);
          if (lbm_type_of(new_env) == LBM_TYPE_SYMBOL &&
              lbm_dec_sym(new_env) == SYM_NOT_FOUND) {
            new_env = lbm_env_modify_binding(lbm_get_env(), fun_args[1], fun_args[2]);
          }
          if (lbm_type_of(new_env) == LBM_TYPE_SYMBOL &&
              lbm_dec_sym(new_env) == SYM_NOT_FOUND) {
            new_env = lbm_env_set(lbm_get_env(),  fun_args[1], fun_args[2]);
            if (lbm_is_error(new_env)) {
              gc(NIL,NIL);
              new_env = lbm_env_set(lbm_get_env(),  fun_args[1], fun_args[2]);
              if (lbm_is_error(new_env)) {
                error_ctx(new_env);
                return;
              }
            }
            *lbm_get_env_ptr() = new_env;
          } else {
            ctx->r = fun_args[2];
          }
        } else {
          error_ctx(lbm_enc_sym(SYM_EERROR));
          return;
        }
      } else {
        error_ctx(lbm_enc_sym(SYM_EERROR));
        return;
      }
      ctx->r = fun_args[2];
      lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
      ctx->app_cont = true;
    } break;
    case SYM_READ: /* fall through */
    case SYM_READ_PROGRAM:
      if (lbm_dec_u(count) == 1) {
        lbm_value stream = NIL;
        if (lbm_type_of(fun_args[1]) == LBM_TYPE_ARRAY) {
          lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(fun_args[1]);
          if(array->elt_type == LBM_TYPE_CHAR) {
            stream = token_stream_from_string_value(fun_args[1]);
            if (lbm_type_of(stream) == LBM_TYPE_SYMBOL) {
              gc(NIL,NIL);
              stream = token_stream_from_string_value(fun_args[1]);
              if (lbm_type_of(stream) == LBM_TYPE_SYMBOL) {
                error_ctx(stream);
                return;
              }
            }
          }
        } else if (lbm_type_of(fun_args[1]) == LBM_TYPE_STREAM) {
          stream = fun_args[1];
        } else {
          error_ctx(lbm_enc_sym(SYM_EERROR));
          break;
        }
        lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
        CHECK_STACK(lbm_push_3(&ctx->K, stream, fun, READ));
        ctx->r = NIL;
        ctx->app_cont = true;
        break;
    case SYM_SPAWN: {

      lbm_uint stack_size = EVAL_CPS_DEFAULT_STACK_SIZE;
      lbm_uint closure_pos = 1;

      if (lbm_dec_u(count) >= 2 &&
          lbm_is_number(fun_args[1]) &&
          lbm_is_closure(fun_args[2])) {
        stack_size = lbm_dec_as_u32(fun_args[1]);
        closure_pos = 2;
      }

      if (!lbm_is_closure(fun_args[closure_pos]) ||
          lbm_dec_u(count) < 1) {
        error_ctx(lbm_enc_sym(SYM_EERROR));
        return;
      }

      lbm_value cdr_fun = lbm_cdr(fun_args[closure_pos]);
      lbm_value cddr_fun = lbm_cdr(cdr_fun);
      lbm_value cdddr_fun = lbm_cdr(cddr_fun);
      lbm_value params  = lbm_car(cdr_fun);
      lbm_value exp     = lbm_car(cddr_fun);
      lbm_value clo_env = lbm_car(cdddr_fun);

      lbm_value curr_param = params;
      lbm_uint i = closure_pos + 1;
      while (lbm_type_of(curr_param) == LBM_TYPE_CONS &&
             i <= lbm_dec_u(count)) {

        lbm_value entry;
        WITH_GC(entry,lbm_cons(lbm_car(curr_param),fun_args[i]), clo_env,NIL);

        lbm_value aug_env;
        WITH_GC(aug_env,lbm_cons(entry, clo_env),clo_env,entry);
        clo_env = aug_env;
        curr_param = lbm_cdr(curr_param);
        i ++;
      }

      lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);

      lbm_value program =  NIL;
      CONS_WITH_GC(program, exp, program, clo_env);


      lbm_cid cid = lbm_create_ctx(program,
                                   clo_env,
                                   stack_size);
      ctx->r = lbm_enc_i(cid);
      ctx->app_cont = true;
    } break;
    case SYM_YIELD:
      if (lbm_dec_u(count) == 1 && lbm_is_number(fun_args[1])) {
        lbm_uint ts = lbm_dec_as_u32(fun_args[1]);
        lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
        yield_ctx(ts);
      } else {
        error_ctx(lbm_enc_sym(SYM_EERROR));
      }
      break;
    case SYM_WAIT:
      if (lbm_type_of(fun_args[1]) == LBM_TYPE_I) {
        lbm_cid cid = (lbm_cid)lbm_dec_i(fun_args[1]);
        lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
        CHECK_STACK(lbm_push_2(&ctx->K, lbm_enc_i(cid), WAIT));
        ctx->r = lbm_enc_sym(SYM_TRUE);
        ctx->app_cont = true;
        yield_ctx(50000);
      } else {
        error_ctx(lbm_enc_sym(SYM_EERROR));
      }
      break;
    case SYM_EVAL:
      ctx->curr_exp = fun_args[1];
      lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
      break;
    case SYM_EVAL_PROGRAM: {
      lbm_value prg = fun_args[1];
      prg = lbm_list_append(prg, ctx->program);

      lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);

      if (lbm_type_of(prg) != LBM_TYPE_CONS) {
        error_ctx(lbm_enc_sym(SYM_EERROR));
        return;
      }

      ctx->program = lbm_cdr(prg);
      ctx->curr_exp = lbm_car(prg);
    } break;
    case SYM_SEND: {
      lbm_value status = lbm_enc_sym(SYM_EERROR);
      if (lbm_dec_u(count) == 2) {

        if (lbm_type_of(fun_args[1]) == LBM_TYPE_I) {
          lbm_cid cid = (lbm_cid)lbm_dec_i(fun_args[1]);
          lbm_value msg = fun_args[2];

          WITH_GC(status, lbm_find_receiver_and_send(cid, msg), NIL, NIL);
        }
      }
      /* return the status */
      lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
      ctx->r = status;
      ctx->app_cont = true;
    } break;
    default:
      if (lbm_is_fundamental(fun)) {
        /* If it is not a eval_cps specific function, it may be a fundamental operation */
        lbm_value res;
        WITH_GC(res, lbm_fundamental(&fun_args[1], lbm_dec_u(count), fun), NIL, NIL);
        if (lbm_is_error(res)) {
          error_ctx(res);
          return;
        }
        lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
        ctx->app_cont = true;
        ctx->r = res;
        break;
      } else {
        // It may be an extension
        extension_fptr f = lbm_get_extension(lbm_dec_sym(fun));
        if (f == NULL) {
          error_ctx(lbm_enc_sym(SYM_EERROR));
          return;
        }

        lbm_value ext_res;
        WITH_GC(ext_res, f(&fun_args[1] , lbm_dec_u(count)), NIL, NIL);
        if (lbm_is_error(ext_res)) {
          error_ctx(ext_res);
          return;
        }
        lbm_stack_drop(&ctx->K, lbm_dec_u(count) + 1);

        if (blocking_extension) {
          blocking_extension = false;
          ctx->timestamp = timestamp_us_callback();
          ctx->sleep_us = 0;
          ctx->app_cont = true;
          enqueue_ctx(&blocked,ctx);
          ctx_running = NULL;
          break;
        }
        ctx->app_cont = true;
        ctx->r = ext_res;
        break;
      }
    }
    }
  } else {
    error_ctx(lbm_enc_sym(SYM_EERROR));
  }
}

static inline void cont_closure_application_args(eval_context_t *ctx) {
  lbm_uint* sptr = lbm_get_stack_ptr(&ctx->K, 5);

  if (sptr == NULL) {
    error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
    return;
  }
  lbm_value arg_env = (lbm_value)sptr[0];
  lbm_value exp     = (lbm_value)sptr[1];
  lbm_value clo_env = (lbm_value)sptr[2];
  lbm_value params  = (lbm_value)sptr[3];
  lbm_value args    = (lbm_value)sptr[4];

  if (lbm_is_list(params)) {
    lbm_value entry;
    WITH_GC(entry,lbm_cons(lbm_car(params),ctx->r), NIL, NIL);

    lbm_value aug_env;
    WITH_GC(aug_env,lbm_cons(entry, clo_env),entry,NIL);
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
    error_ctx(lbm_enc_sym(SYM_EERROR));
  } else if (a_nil && !p_nil) {
    lbm_set_error_reason("Too few arguments.");
    error_ctx(lbm_enc_sym(SYM_EERROR));
  } else {
   sptr[2] = clo_env;
   sptr[3] = lbm_cdr(params);
   sptr[4] = lbm_cdr(args);
   lbm_push(&ctx->K, CLOSURE_ARGS);
   ctx->curr_exp = lbm_car(args);
   ctx->curr_env = arg_env;
  }
}

static inline void cont_application_args(eval_context_t *ctx) {

  lbm_uint *sptr = lbm_get_stack_ptr(&ctx->K,3);

  if (!sptr) {
    error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
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
    error_ctx(lbm_enc_sym(SYM_EERROR));
  }
}

static inline void cont_and(eval_context_t *ctx) {
  lbm_value rest;
  lbm_value arg = ctx->r;
  lbm_pop(&ctx->K, &rest);
  if (lbm_is_symbol_nil(arg)) {
    ctx->app_cont = true;
    ctx->r = lbm_enc_sym(SYM_NIL);
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
    ctx->r = lbm_enc_sym(SYM_NIL);
  } else {
    CHECK_STACK(lbm_push_2(&ctx->K, lbm_cdr(rest), OR));
    ctx->curr_exp = lbm_car(rest);
  }
}

static inline void cont_bind_to_key_rest(eval_context_t *ctx) {
  lbm_value key;
  lbm_value env;
  lbm_value rest;
  lbm_value arg = ctx->r;
  lbm_pop_3(&ctx->K, &key, &env, &rest);

  lbm_env_modify_binding(env, key, arg);

  if (lbm_is_list(rest)) {
    lbm_value keyn = lbm_car(lbm_car(rest));
    lbm_value valn_exp = lbm_cadr(lbm_car(rest));

    CHECK_STACK(lbm_push_4(&ctx->K, lbm_cdr(rest), env, keyn, BIND_TO_KEY_REST));

    ctx->curr_exp = valn_exp;
    ctx->curr_env = env;
  } else {
    // Otherwise evaluate the expression in the populated env
    lbm_value exp;
    lbm_pop(&ctx->K, &exp);
    ctx->curr_exp = exp;
    ctx->curr_env = env;
  }
}

static inline void cont_if(eval_context_t *ctx) {

  lbm_value arg = ctx->r;

  lbm_value *sptr = lbm_get_stack_ptr(&ctx->K, 3);
  if (!sptr) {
    error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
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
    ctx->r = lbm_enc_sym(SYM_NO_MATCH);
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
        error_ctx(lbm_enc_sym(SYM_MERROR));
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
    error_ctx(lbm_enc_sym(SYM_TERROR));
  }
}

/****************************************************/
/*   READER                                         */

static inline void cont_read(eval_context_t *ctx) {

  gc(NIL,NIL);

  lbm_value stream = NIL;
  lbm_value prg_val = NIL;
  lbm_pop_2(&ctx->K, &prg_val, &stream);

  lbm_stream_t *str = lbm_dec_stream(stream);
  lbm_value tok;
  bool read_done = false;
  bool app_cont = false;
  bool program = false;

  lbm_uint sp_start = ctx->K.sp;
  lbm_cid cid = ctx->id;

  if (lbm_type_of(prg_val) == LBM_TYPE_SYMBOL) {
    if (lbm_dec_sym(prg_val) == SYM_READ) program = false;
    else if (lbm_dec_sym(prg_val) == SYM_READ_PROGRAM) program = true;
  } else {
    error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
    done_reading(cid);
    return;
  }

  CHECK_STACK(lbm_push(&ctx->K, READ_DONE));

  if (program) {
    CHECK_STACK(lbm_push_3(&ctx->K, NIL, NIL, APPEND_CONTINUE));
  }

  while (!read_done) {

    if (app_cont) {
      lbm_value cont = NIL;
      lbm_pop(&ctx->K, &cont);
      app_cont = false; // false unless explicitly set

      switch(cont) {

      case APPEND_CONTINUE: {
        lbm_value first_cell = NIL;
        lbm_value last_cell  = NIL;
        lbm_pop_2(&ctx->K, &last_cell, &first_cell);

        if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL &&
            lbm_dec_sym(ctx->r) == SYM_CLOSEPAR) {
          if (lbm_type_of(last_cell) == LBM_TYPE_CONS) {
            lbm_set_cdr(last_cell, NIL); // terminate the list
            ctx->r = first_cell;
          } else {
            // empty list case
            ctx->r = NIL;
          }
          app_cont = true;
        } else if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL &&
                   lbm_dec_sym(ctx->r) == SYM_DOT) {
          CHECK_STACK(lbm_push_3(&ctx->K,
                                 first_cell, last_cell,
                                 DOT_TERMINATE));
        } else {
          lbm_value new_cell;
          CONS_WITH_GC(new_cell, ctx->r, NIL, stream);
          if (lbm_type_of(last_cell) == LBM_TYPE_CONS) {
            lbm_set_cdr(last_cell, new_cell);
            last_cell = new_cell;
          } else {
            first_cell = last_cell = new_cell;
          }
          CHECK_STACK(lbm_push_3(&ctx->K,
                                 first_cell, last_cell,
                                 APPEND_CONTINUE));
        }
      } break;
      case EXPECT_CLOSEPAR: {
        if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL &&
            lbm_dec_sym(ctx->r) == SYM_CLOSEPAR) {
          lbm_value res = NIL;
          lbm_pop(&ctx->K, &res);
          ctx->r = res;
          app_cont = true;
        } else {
          error_ctx(lbm_enc_sym(SYM_RERROR));
          done_reading(cid);
          return;
        }
      } break;
      case DOT_TERMINATE: {
        lbm_value first_cell = NIL;
        lbm_value last_cell  = NIL;
        lbm_pop_2(&ctx->K, &last_cell, &first_cell);

        if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL &&
            (lbm_dec_sym(ctx->r) == SYM_CLOSEPAR ||
             lbm_dec_sym(ctx->r) == SYM_DOT)) {
          error_ctx(lbm_enc_sym(SYM_RERROR));
          done_reading(cid);
          return;
        } else {
          if (lbm_type_of(last_cell) == LBM_TYPE_CONS) {
            lbm_set_cdr(last_cell, ctx->r);
            ctx->r = first_cell;
            CHECK_STACK(lbm_push_2(&ctx->K,
                                   ctx->r,
                                   EXPECT_CLOSEPAR));
          } else {
            error_ctx(lbm_enc_sym(SYM_RERROR));
            done_reading(cid);
            return;
          }
        }
      } break;
      case READ_DONE:
        tok = token_stream_get(str);
        if (tok != lbm_enc_sym(SYM_TOKENIZER_DONE)) {
          error_ctx(lbm_enc_sym(SYM_RERROR));
          done_reading(cid);
          return;
        }
        /* Go back to outer eval loop and apply continuation */
        ctx->app_cont = true;
        read_done = true;
        continue;
      case QUOTE_RESULT: {
        lbm_value cell1;
        lbm_value cell2;
        CONS_WITH_GC(cell2, ctx->r, NIL, stream);
        CONS_WITH_GC(cell1, lbm_enc_sym(SYM_QUOTE), cell2, stream);
        ctx->r = cell1;
        app_cont = true;
      } break;
      case BACKQUOTE_RESULT: {
        lbm_value expanded = lbm_qq_expand(ctx->r);
        ctx->r = expanded;
        app_cont = true;
      } break;
      case COMMAAT_RESULT: {
        lbm_value cell1;
        lbm_value cell2;
        CONS_WITH_GC(cell2, ctx->r,NIL, stream);
        CONS_WITH_GC(cell1, lbm_enc_sym(SYM_COMMAAT), cell2, stream);
        ctx->r = cell1;
        app_cont = true;
      } break;
      case COMMA_RESULT: {
        lbm_value cell1;
        lbm_value cell2;
        CONS_WITH_GC(cell2, ctx->r,NIL, stream);
        CONS_WITH_GC(cell1, lbm_enc_sym(SYM_COMMA), cell2, stream);
        ctx->r = cell1;
        app_cont = true;
      } break;
      }
    } else {
      tok = token_stream_get(str);

      if (lbm_type_of(tok) == LBM_TYPE_SYMBOL) {
        switch (lbm_dec_sym(tok)) {
        case SYM_RERROR:
          error_ctx(lbm_enc_sym(SYM_RERROR));
          done_reading(cid);
          return;
        case SYM_MERROR:
          error_ctx(lbm_enc_sym(SYM_MERROR));
          done_reading(cid);
          return;
        case SYM_TOKENIZER_DONE:
          if (program) {

            if (ctx->K.sp == sp_start + 4) {
              ctx->r = lbm_enc_sym(SYM_CLOSEPAR);
              app_cont = true;
            } else if (ctx->K.data[sp_start] == READ_DONE &&
                       ctx->K.data[sp_start+3] == APPEND_CONTINUE) {
              // Parsing failed but stack seems to not be corrupted.
              error_ctx(lbm_enc_sym(SYM_RERROR));
              done_reading(cid);
              return;
            } else {
              // parsing failed and left a corrupted stack.
              error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
              done_reading(cid);
              return; // there is no context to keep working in so return.
            }
          } else {
            if (ctx->K.sp > sp_start &&
                ctx->K.data[sp_start] == READ_DONE) {
              error_ctx(lbm_enc_sym(SYM_RERROR));
              done_reading(cid);
              return;
            } else if (ctx->K.sp < sp_start) {
              /*the stack is broken */
              error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
              done_reading(cid);
              return; // there is no context to keep working in so return.
            } else {
              app_cont = true;
            }
          }
          break;
        case SYM_CLOSEPAR:
          ctx->r = tok;
          app_cont = true;
          break;
        case SYM_OPENPAR:
          CHECK_STACK(lbm_push_3(&ctx->K,
                                 NIL, NIL,
                                 APPEND_CONTINUE));
          app_cont = false;
          break;
        case SYM_QUOTE_IT:
          CHECK_STACK(lbm_push(&ctx->K,
                               QUOTE_RESULT));
          app_cont = false;
          break;
        case SYM_BACKQUOTE:
          CHECK_STACK(lbm_push(&ctx->K,
                               BACKQUOTE_RESULT));
          app_cont = false;
          break;
        case SYM_COMMAAT:
          CHECK_STACK(lbm_push(&ctx->K,
                               COMMAAT_RESULT));
          app_cont = false;
          break;
        case SYM_COMMA:
          CHECK_STACK(lbm_push(&ctx->K,
                               COMMA_RESULT));
          app_cont = false;
          break;
        default:
          ctx->r = tok;
          app_cont = true;
          break;
        }
      } else { // arbitrary value form
        ctx->r = tok;
        app_cont = true;
      }
    }
  }

  // TODO: See if there are better things to do here.
  if (ctx->K.sp != sp_start) {
    error_ctx(lbm_enc_sym(SYM_EERROR));
  }

  done_reading(cid);
}

#define OTHER_APPLY   0
#define MACRO_APPLY   1
#define MACRO_EXPAND  2
#define CLOSURE_APPLY 3

static inline int application_kind(lbm_value v) {
  if (lbm_type_of(v) == LBM_TYPE_CONS) {
    lbm_value fun_kind_identifier = lbm_car(v);
    if (lbm_type_of(fun_kind_identifier) == LBM_TYPE_SYMBOL) {
      if (lbm_dec_sym(fun_kind_identifier) == SYM_CLOSURE) return CLOSURE_APPLY;
      if (lbm_dec_sym(fun_kind_identifier) == SYM_MACRO) return MACRO_APPLY;
      if (lbm_dec_sym(fun_kind_identifier) == SYM_MACRO_EXPAND) return MACRO_EXPAND;
    }
  }
  return OTHER_APPLY;
}

static inline void cont_application_start(eval_context_t *ctx) {

  lbm_uint *sptr = lbm_get_stack_ptr(&ctx->K, 2);
  if (sptr == NULL) {
    error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
    return;
  }
  lbm_value args = (lbm_value)sptr[1];

  switch (application_kind(ctx->r)) {
  case MACRO_EXPAND:
    /* (macro-expand (args + (list 1 2 3))) */
    sptr[1] = lbm_cdr(args);
    CHECK_STACK(lbm_push(&ctx->K,
                         EXPAND_MACRO));
    ctx->curr_exp = lbm_car(lbm_car(args));
    break;
  case MACRO_APPLY:{
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
      WITH_GC(entry,lbm_cons(lbm_car(curr_param),lbm_car(curr_arg)), expand_env,NIL);

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
  case CLOSURE_APPLY: {
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
      error_ctx(lbm_enc_sym(SYM_STACK_ERROR));
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
      //    case APPLICATION:       cont_application(ctx); return;
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
    default:
      error_ctx(lbm_enc_sym(SYM_EERROR));
      return;
    }
  }

  lbm_value head;

  switch (lbm_type_of(ctx->curr_exp)) {

  case LBM_TYPE_SYMBOL: {
    lbm_value s;
    if (ctx->curr_exp == NIL) {
      ctx->app_cont = true;
      ctx->r = NIL;
      return;
    }

    if (eval_symbol(ctx, &s)) {
      ctx->app_cont = true;
      ctx->r = s;
      return;
    }

    if (dynamic_load_callback) {
      dynamic_load(ctx);
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
  case LBM_TYPE_STREAM: eval_selfevaluating(ctx);  return;

  case LBM_TYPE_CONS:
    head = lbm_car(ctx->curr_exp);

    if (lbm_type_of(head) == LBM_TYPE_SYMBOL) {

      lbm_uint sym_id = lbm_dec_sym(head);

      switch(sym_id) {
      case SYM_QUOTE:   eval_quote(ctx); return;
      case SYM_DEFINE:  eval_define(ctx); return;
      case SYM_PROGN:   eval_progn(ctx); return;
      case SYM_LAMBDA:  eval_lambda(ctx); return;
      case SYM_CLOSURE: eval_closure(ctx); return;
      case SYM_IF:      eval_if(ctx); return;
      case SYM_LET:     eval_let(ctx); return;
      case SYM_AND:     eval_and(ctx); return;
      case SYM_OR:      eval_or(ctx); return;
      case SYM_MATCH:   eval_match(ctx); return;
        /* message passing primitives */
      case SYM_RECEIVE: eval_receive(ctx); return;
      case SYM_MACRO:   eval_macro(ctx); return;
      case SYM_CALLCC:  eval_callcc(ctx); return;
      case SYM_CONT:    eval_continuation(ctx); return;

      default: break; /* May be general application form. Checked below*/
      }
    } // If head is symbol
    /*
     * At this point head can be a closure, fundamental, extension or a macro.
     * Anything else would be an error.
     */
    lbm_value *reserved = lbm_stack_reserve(&ctx->K, 3);
    if (!reserved) {
      error_ctx(lbm_enc_sym(SYM_STACK_ERROR));
      return;
    }
    reserved[0] = ctx->curr_env;
    reserved[1] = lbm_cdr(ctx->curr_exp);
    reserved[2] = APPLICATION_START;

    ctx->curr_exp = head; // evaluate the function
    break;
  default:
    // BUG No applicable case!
    error_ctx(lbm_enc_sym(SYM_EERROR));
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
          gc(NIL, NIL);
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

    /* TODO: Logic for sleeping in case the evaluator has been using a lot of CPU
       should go here */

    if (!ctx_running) {
      uint32_t us;
      ctx_running = dequeue_ctx(&us);
      if (!ctx_running) {
        if (usleep_callback) {
          usleep_callback(us);
        }
        continue;
      }
    }

    evaluation_step();
  }
}

lbm_cid lbm_eval_program(lbm_value lisp) {
  return lbm_create_ctx(lisp, NIL, 256);
}

lbm_cid lbm_eval_program_ext(lbm_value lisp, unsigned int stack_size) {
  return lbm_create_ctx(lisp, NIL, stack_size);
}

int lbm_eval_init() {
  int res = 1;

  blocked.first = NULL;
  blocked.last = NULL;
  queue.first = NULL;
  queue.last = NULL;
  done.first = NULL;
  done.last = NULL;
  ctx_running = NULL;
  next_ctx_id = 1;

  eval_cps_run_state = EVAL_CPS_STATE_INIT;

  mutex_init(&qmutex);

  NIL = lbm_enc_sym(SYM_NIL);
  NONSENSE = lbm_enc_sym(SYM_NONSENSE);

  *lbm_get_env_ptr() = NIL;
  eval_running = true;

  return res;
}

