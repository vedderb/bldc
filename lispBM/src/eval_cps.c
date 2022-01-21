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

#include "symrepr.h"
#include "heap.h"
#include "env.h"
#include "eval_cps.h"
#include "stack.h"
#include "fundamental.h"
#include "extensions.h"
#include "lispbm_types.h"
#include "exp_kind.h"
#include "streams.h"
#include "lispbm_memory.h"
#include "tokpar.h"
#include "qq_expand.h"

#include "platform_mutex.h"

#ifdef VISUALIZE_HEAP
#include "heap_vis.h"
#endif

#define DONE              1
#define SET_GLOBAL_ENV    2
#define BIND_TO_KEY_REST  3
#define IF                4
#define PROGN_REST        5
#define APPLICATION       6
#define APPLICATION_ARGS  7
#define AND               8
#define OR                9
#define WAIT              10
#define SPAWN_ALL         11
#define MATCH             12
#define MATCH_MANY        13
#define READ              14

#define CHECK_STACK(x)                          \
  if (!(x)) {                                   \
    ctx->done=true;                             \
    ctx->r = lbm_enc_sym(SYM_STACK_ERROR);          \
    finish_ctx();                               \
    return;                                     \
  }

#define WITH_GC(y, x, remember1,remember2)      \
  (y) = (x);                                    \
  if (lbm_is_symbol_merror((y))) {                  \
    gc(remember1, remember2);                   \
    (y) = (x);                                  \
    if (lbm_is_symbol_merror((y))) {                \
      ctx_running->done = true;                 \
      error_ctx(lbm_enc_sym(SYM_MERROR));           \
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

static lbm_value cons_with_gc(lbm_value head, lbm_value tail, lbm_value remember) {
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
  if (lbm_is_symbol_merror(res)) {                  \
    return;                                     \
  }

#define ERROR
//#define ERROR printf("Line: %d\n", __LINE__);

#define DEFAULT_SLEEP_US  1000

#define EVAL_CPS_DEFAULT_STACK_SIZE 256
#define EVAL_CPS_DEFAULT_STACK_GROW_POLICY false

/* 768 us -> ~128000 "ticks" at 168MHz I assume this means also roughly 128000 instructions */
#define EVAL_CPS_QUANTA_US 768
#define EVAL_CPS_WAIT_US   1536
#define EVAL_CPS_MIN_SLEEP 200

static uint32_t eval_cps_run_state = EVAL_CPS_STATE_INIT;
volatile uint32_t eval_cps_next_state = EVAL_CPS_STATE_INIT;

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

//static eval_context_t *ctx_done = NULL;


static eval_context_t ctx_non_concurrent;

static void (*usleep_callback)(uint32_t) = NULL;
static uint32_t (*timestamp_us_callback)(void) = NULL;
static void (*ctx_done_callback)(eval_context_t *) = NULL;

void lbm_set_usleep_callback(void (*fptr)(uint32_t)) {
  usleep_callback = fptr;
}

void lbm_set_timestamp_us_callback(uint32_t (*fptr)(void)) {
  timestamp_us_callback = fptr;
}

void lbm_set_ctx_done_callback(void (*fptr)(eval_context_t *)) {
  ctx_done_callback = fptr;
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

lbm_value eval_cps_create_token_stream(lbm_tokenizer_char_stream_t *str) {

  lbm_stream_t *stream;

  stream = (lbm_stream_t*)lbm_memory_allocate(sizeof(lbm_stream_t) / 4);

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

lbm_value token_stream_from_string_value(lbm_value s) {
  char *str = lbm_dec_str(s);

  lbm_stream_t *stream = NULL;
  lbm_tokenizer_string_state_t *tok_stream_state = NULL;
  lbm_tokenizer_char_stream_t *tok_stream = NULL;

  stream = (lbm_stream_t*)lbm_memory_allocate(sizeof(lbm_stream_t) / 4);
  if (stream == NULL) {
    return lbm_enc_sym(SYM_MERROR);
  }

  tok_stream_state = (lbm_tokenizer_string_state_t*)lbm_memory_allocate(1 + (sizeof(lbm_tokenizer_string_state_t) / 4));
  if (tok_stream_state == NULL) {
    lbm_memory_free((uint32_t*)stream);
    return lbm_enc_sym(SYM_MERROR);
  }

  tok_stream = (lbm_tokenizer_char_stream_t*)lbm_memory_allocate(sizeof(lbm_tokenizer_char_stream_t) / 4);
  if (tok_stream == NULL) {
    lbm_memory_free((uint32_t*)stream);
    lbm_memory_free((uint32_t*)tok_stream_state);
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

  enqueue_ctx(&done, ctx_running);

  if (ctx_done_callback) {
    ctx_done_callback(ctx_running);
  }
  ctx_running = NULL;
}

bool lbm_remove_done_ctx(lbm_cid cid, lbm_value *v) {

  eval_context_t *ctx = lookup_ctx(&done, cid);

  if (ctx) {
    drop_ctx(&done, ctx);

    *v = ctx->r;
    lbm_stack_free(&ctx->K);
    lbm_memory_free((uint32_t*)ctx);
    return true;
  }
  return false;
}

/* Dangerous function that will lock up if called
   with the incorrect cid
   TODO: replace with less dangerous alternatives
*/
lbm_value lbm_wait_ctx(lbm_cid cid) {

  eval_context_t *ctx = NULL;
  lbm_value r = lbm_enc_sym(SYM_NIL);

  while (!ctx) {
    ctx = lookup_ctx(&done, cid);
    if (ctx) {
      lbm_remove_done_ctx(cid, &r);
      return r;
    }
    usleep_callback(1000);
  }
  return r;
}

static void error_ctx(lbm_value err_val) {
  ctx_running->r = err_val;
  finish_ctx();
}

static eval_context_t *dequeue_ctx(uint32_t *us) {
  uint32_t min_us = DEFAULT_SLEEP_US;
  uint32_t t_now;

  mutex_lock(&qmutex);

  if (timestamp_us_callback) {
    t_now = timestamp_us_callback();
  } else {
    t_now = 0;
  }

  eval_context_t *curr = queue.first; //ctx_queue;

  while (curr != NULL) {
    uint32_t t_diff;
    if ( curr->timestamp > t_now) {
      /* There was an overflow on the counter */
      t_diff = (0xFFFFFFFF - curr->timestamp) + t_now;
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

static void yield_ctx(uint32_t sleep_us) {
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

static lbm_cid create_ctx(lbm_value program, lbm_value env, uint32_t stack_size) {

  if (next_ctx_id == 0) return 0; // overflow of CIDs

  if (lbm_type_of(program) != LBM_PTR_TYPE_CONS) return 0;

  eval_context_t *ctx = NULL;
  ctx = (eval_context_t*)lbm_memory_allocate(sizeof(eval_context_t) / 4);
  if (ctx == NULL) return 0;

  ctx->program = lbm_cdr(program);
  ctx->curr_exp = lbm_car(program);
  ctx->curr_env = env;
  ctx->mailbox = lbm_enc_sym(SYM_NIL);
  ctx->done = false;
  ctx->app_cont = false;
  ctx->timestamp = 0;
  ctx->sleep_us = 0;
  ctx->prev = NULL;
  ctx->next = NULL;
  if (next_ctx_id > CID_MAX) {
    lbm_memory_free((uint32_t*)ctx);
    return 0;
  }

  ctx->id = (uint16_t)next_ctx_id++;
  if (!lbm_stack_allocate(&ctx->K, stack_size)) {
    lbm_memory_free((uint32_t*)ctx);
    return 0;
  }
  if (!lbm_push_u32(&ctx->K, lbm_enc_u(DONE))) {
    lbm_stack_free(&ctx->K);
    lbm_memory_free((uint32_t*)ctx);
    return 0;
  }

  enqueue_ctx(&queue,ctx);

  return ctx->id;
}

/* Advance execution to the next expression in the program */
static void advance_ctx(void) {

  if (lbm_type_of(ctx_running->program) == LBM_PTR_TYPE_CONS) {
    lbm_push_u32(&ctx_running->K, lbm_enc_u(DONE));
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

static lbm_value find_receiver_and_send(lbm_cid cid, lbm_value msg) {
  eval_context_t *found = NULL;

  found = lookup_ctx(&blocked, cid);

  if (found == NULL) {
    found = lookup_ctx(&queue, cid);
  }

  if (found) {
    lbm_value new_mailbox = lbm_cons(msg, found->mailbox);

    if (lbm_type_of(new_mailbox) == LBM_VAL_TYPE_SYMBOL) {
      return new_mailbox; /* An error symbol */
    }

    found->mailbox = new_mailbox;

    drop_ctx(&blocked,found);
    drop_ctx(&queue,found);

    enqueue_ctx(&queue,found);
    return lbm_enc_sym(SYM_TRUE);
  }

  /* check the current context */
  if (ctx_running->id == cid) {
    lbm_value new_mailbox = lbm_cons(msg, ctx_running->mailbox);

    if (lbm_type_of(new_mailbox) == LBM_VAL_TYPE_SYMBOL) {
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

  while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
    if (n == c) {
      curr = lbm_cdr(curr);
      break;
    }
    tmp = lbm_cons(lbm_car(curr), tmp);
    if (lbm_type_of(tmp) == LBM_VAL_TYPE_SYMBOL) {
      res = tmp;
      return res;
    }
    curr = lbm_cdr(curr);
    c++;
  }

  res = curr; /*res is the tail */
  curr = tmp;
  if ( c != 0) {
    while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
      res = lbm_cons(lbm_car(curr),res);
      if (lbm_type_of(res) == LBM_VAL_TYPE_SYMBOL) {
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
    lbm_value var = lbm_car(lbm_cdr(p));
    lbm_value bindertype = lbm_car(p);

    if (!lbm_is_symbol(var)) return false;

    switch (lbm_dec_sym(bindertype)) {
    case SYM_MATCH_ANY:
      if (lbm_dec_sym(var) == SYM_DONTCARE) {
        return true;
      } else {
        break;
      }
      return false;
    case SYM_MATCH_I28:
      if (lbm_type_of(e) == LBM_VAL_TYPE_I) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_U28:
      if (lbm_type_of(e) == LBM_VAL_TYPE_U) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_FLOAT:
      if (lbm_type_of(e) == LBM_PTR_TYPE_BOXED_F) {
        if (lbm_dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_CONS:
      if (lbm_type_of(e) == LBM_PTR_TYPE_CONS) {
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
    *env = lbm_cons(binding, *env);
    if (lbm_type_of(binding) == LBM_VAL_TYPE_SYMBOL ||
        lbm_type_of(*env) == LBM_VAL_TYPE_SYMBOL) {
      *gc = true;
      return false;
    }
    return true;
  }

  if (lbm_is_symbol(p)) {
    if (lbm_dec_sym(p) == SYM_DONTCARE) return true;
    return (p == e);
  }

  if (lbm_type_of(p) == LBM_PTR_TYPE_CONS &&
      lbm_type_of(e) == LBM_PTR_TYPE_CONS) {

    lbm_value headp = lbm_car(p);
    lbm_value heade = lbm_car(e);
    if (!match(headp, heade, env, gc)) {
      return false;
    }
    return match (lbm_cdr(p), lbm_cdr(e), env, gc);
  } else if (p == e) {
    return true;
  }
  return false;
}

static int find_match(lbm_value plist, lbm_value elist, lbm_value *e, lbm_value *env, bool *gc) {

  lbm_value curr_p = plist;
  lbm_value curr_e = elist;
  int n = 0;
  while (lbm_type_of(curr_e) == LBM_PTR_TYPE_CONS) {
    while (lbm_type_of(curr_p) == LBM_PTR_TYPE_CONS) {
      if (match(lbm_car(lbm_car(curr_p)), lbm_car(curr_e), env, gc)) {
        if (*gc) return -1;
        *e = lbm_car(lbm_cdr(lbm_car(curr_p)));
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

  lbm_gc_state_inc();
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

  curr = done.first;
  while (curr) {
    lbm_gc_mark_phase(curr->r);
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

  return lbm_gc_sweep_phase();
}


/****************************************************/
/* Evaluation functions                             */

static inline void eval_symbol(eval_context_t *ctx) {
  lbm_value value;

  if (lbm_is_special(ctx->curr_exp) ||
      (lbm_get_extension(lbm_dec_sym(ctx->curr_exp)) != NULL)) {
    // Special symbols and extension symbols evaluate to themself
    value = ctx->curr_exp;
  } else {
    // If not special, check if there is a binding in the environments
    value = lbm_env_lookup(ctx->curr_exp, ctx->curr_env);
    if (lbm_type_of(value) == LBM_VAL_TYPE_SYMBOL &&
        lbm_dec_sym(value) == SYM_NOT_FOUND) {

      value = lbm_env_lookup(ctx->curr_exp, *lbm_get_env_ptr());
    }
  }

  ctx->app_cont = true;
  ctx->r = value;
}


static inline void eval_selfevaluating(eval_context_t *ctx) {
  ctx->app_cont = true;
  ctx->r = ctx->curr_exp;
}

static inline void eval_quote(eval_context_t *ctx) {
  ctx->r = lbm_car(lbm_cdr(ctx->curr_exp));
  ctx->app_cont = true;
}

static inline void eval_define(eval_context_t *ctx) {
  lbm_value key = lbm_car(lbm_cdr(ctx->curr_exp));
  lbm_value val_exp = lbm_car(lbm_cdr(lbm_cdr(ctx->curr_exp)));

  if (lbm_type_of(key) != LBM_VAL_TYPE_SYMBOL ||
      key == NIL) {
    ERROR
      error_ctx(lbm_enc_sym(SYM_EERROR));
    return;
  }

  CHECK_STACK(lbm_push_u32_2(&ctx->K, key, lbm_enc_u(SET_GLOBAL_ENV)));
  ctx->curr_exp = val_exp;
}


static inline void eval_progn(eval_context_t *ctx) {
  lbm_value exps = lbm_cdr(ctx->curr_exp);
  lbm_value env  = ctx->curr_env;

  if (lbm_type_of(exps) == LBM_VAL_TYPE_SYMBOL && exps == NIL) {
    ctx->r = NIL;
    ctx->app_cont = true;
    return;
  }

  if (lbm_is_error(exps)) {
    ERROR
      error_ctx(exps);
    return;
  }
  CHECK_STACK(lbm_push_u32_3(&ctx->K, env, lbm_cdr(exps), lbm_enc_u(PROGN_REST)));
  ctx->curr_exp = lbm_car(exps);
  ctx->curr_env = env;
}

static inline void eval_spawn(eval_context_t *ctx) {
  lbm_value prgs = lbm_cdr(ctx->curr_exp);
  lbm_value env = ctx->curr_env;

  if (lbm_type_of(prgs) == LBM_VAL_TYPE_SYMBOL && prgs == NIL) {
    ctx->r = NIL;
    ctx->app_cont = true;
    return;
  }

  lbm_value cid_list = NIL;
  CHECK_STACK(lbm_push_u32_3(&ctx->K, env, prgs, lbm_enc_u(SPAWN_ALL)));
  ctx->r = cid_list;
  ctx->app_cont = true;
}


static inline void eval_lambda(eval_context_t *ctx) {

  lbm_value env_cpy = lbm_env_copy_shallow(ctx->curr_env);

  if (lbm_is_symbol_merror(env_cpy)) {
    gc(NIL, NIL);
    env_cpy = lbm_env_copy_shallow(ctx->curr_env);

    if (lbm_is_symbol_merror(env_cpy)) {
       ctx_running->done = true;
       error_ctx(lbm_enc_sym(SYM_MERROR));
       return;
    }
  }

  lbm_value env_end;
  lbm_value body;
  lbm_value params;
  lbm_value closure;
  CONS_WITH_GC(env_end, env_cpy, NIL, env_cpy);
  CONS_WITH_GC(body, lbm_car(lbm_cdr(lbm_cdr(ctx->curr_exp))), env_end, env_end);
  CONS_WITH_GC(params, lbm_car(lbm_cdr(ctx->curr_exp)), body, body);
  CONS_WITH_GC(closure, lbm_enc_sym(SYM_CLOSURE), params, params);

  ctx->app_cont = true;
  ctx->r = closure;
  return;
}

static inline void eval_if(eval_context_t *ctx) {

  CHECK_STACK(lbm_push_u32_3(&ctx->K,
                         lbm_car(lbm_cdr(lbm_cdr(lbm_cdr(ctx->curr_exp)))), // Else branch
                         lbm_car(lbm_cdr(lbm_cdr(ctx->curr_exp))),      // Then branch
                         lbm_enc_u(IF)));
  ctx->curr_exp = lbm_car(lbm_cdr(ctx->curr_exp));
}

static inline void eval_let(eval_context_t *ctx) {
  lbm_value orig_env = ctx->curr_env;
  lbm_value binds    = lbm_car(lbm_cdr(ctx->curr_exp)); // key value pairs.
  lbm_value exp      = lbm_car(lbm_cdr(lbm_cdr(ctx->curr_exp))); // exp to evaluate in the new env.

  lbm_value curr = binds;
  lbm_value new_env = orig_env;

  if (lbm_type_of(binds) != LBM_PTR_TYPE_CONS) {
    // binds better be nil or there is a programmer error.
    ctx->curr_exp = exp;
    return;
  }

  // Implements letrec by "preallocating" the key parts
  while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
    lbm_value key = lbm_car(lbm_car(curr));
    lbm_value val = NIL;
    lbm_value binding;
    CONS_WITH_GC(binding, key, val, NIL);
    CONS_WITH_GC(new_env, binding, new_env, new_env);

    curr = lbm_cdr(curr);
  }

  lbm_value key0 = lbm_car(lbm_car(binds));
  lbm_value val0_exp = lbm_car(lbm_cdr(lbm_car(binds)));

  CHECK_STACK(lbm_push_u32_5(&ctx->K, exp, lbm_cdr(binds), new_env,
                         key0, lbm_enc_u(BIND_TO_KEY_REST)));
  ctx->curr_exp = val0_exp;
  ctx->curr_env = new_env;
  return;
}

static inline void eval_and(eval_context_t *ctx) {
  lbm_value rest = lbm_cdr(ctx->curr_exp);
  if (lbm_type_of(rest) == LBM_VAL_TYPE_SYMBOL &&
      rest == NIL) {
    ctx->app_cont = true;
    ctx->r = lbm_enc_sym(SYM_TRUE);
  } else {
    CHECK_STACK(lbm_push_u32_2(&ctx->K, lbm_cdr(rest), lbm_enc_u(AND)));
    ctx->curr_exp = lbm_car(rest);
  }
}

static inline void eval_or(eval_context_t *ctx) {
  lbm_value rest = lbm_cdr(ctx->curr_exp);
  if (lbm_type_of(rest) == LBM_VAL_TYPE_SYMBOL &&
      rest == NIL) {
    ctx->app_cont = true;
    ctx->r = lbm_enc_sym(SYM_NIL);
    return;
  } else {
    CHECK_STACK(lbm_push_u32_2(&ctx->K, lbm_cdr(rest), lbm_enc_u(OR)));
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
  if (lbm_type_of(rest) == LBM_VAL_TYPE_SYMBOL &&
      rest == NIL) {
    /* Someone wrote the program (match) */
    ctx->app_cont = true;
    ctx->r = lbm_enc_sym(SYM_NIL); /* make up new specific symbol? */
    return;
  } else {
    CHECK_STACK(lbm_push_u32_2(&ctx->K, lbm_cdr(rest), lbm_enc_u(MATCH)));
    ctx->curr_exp = lbm_car(rest); /* Evaluate e next*/
  }
}

static inline void eval_receive(eval_context_t *ctx) {

  if (lbm_type_of(ctx->mailbox) == LBM_VAL_TYPE_SYMBOL &&
      lbm_dec_sym(ctx->mailbox) == SYM_NIL) {
    /*nothing in the mailbox: block the context*/
    ctx->timestamp = timestamp_us_callback();
    ctx->sleep_us = 0;
    enqueue_ctx(&blocked,ctx);
    ctx_running = NULL;
  } else {
    lbm_value pats = ctx->curr_exp;
    lbm_value msgs = ctx->mailbox;

    if (lbm_type_of(pats) == LBM_VAL_TYPE_SYMBOL &&
        pats == NIL) {
      /* A receive statement without any patterns */
      ctx->app_cont = true;
      ctx->r = lbm_enc_sym(SYM_NIL);
    } else {
      /* The common case */
      lbm_value e;
      lbm_value new_env = ctx->curr_env;
      bool do_gc = false;;
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

      /* Match messages on mailbox against the patterns */
      /* FATAL_ON_FAIL(ctx->done, push_u32_4(&ctx->K, ctx->curr_exp, car(cdr(pats)), cdr(msgs), enc_u(MATCH_MANY))); */
      /* FATAL_ON_FAIL(ctx->done, push_u32_2(&ctx->K, car(cdr(pats)), enc_u(MATCH))); */
      /* ctx->r = car(msgs); */
      /* ctx->app_cont = true; */
    }
  }
  return;
}

/*********************************************************/
/*  Continuation functions                               */

static inline void cont_set_global_env(eval_context_t *ctx){

  lbm_value key;
  lbm_value val = ctx->r;

  lbm_pop_u32(&ctx->K, &key);
  lbm_value new_env;
  WITH_GC(new_env, lbm_env_set(*lbm_get_env_ptr(),key,val), key, NIL);

  *lbm_get_env_ptr() = new_env;
  ctx->r = key;

  if (!ctx->done)
    ctx->app_cont = true;

  return;
}

static inline void cont_progn_rest(eval_context_t *ctx) {
  lbm_value rest;
  lbm_value env;
  lbm_pop_u32_2(&ctx->K, &rest, &env);
  if (lbm_type_of(rest) == LBM_VAL_TYPE_SYMBOL && rest == NIL) {
    ctx->app_cont = true;
    return;
  }

  if (lbm_is_error(rest)) {
    ERROR
      error_ctx(rest);
    return;
  }
  // allow for tail recursion
  if (lbm_type_of(lbm_cdr(rest)) == LBM_VAL_TYPE_SYMBOL &&
      lbm_cdr(rest) == NIL) {
    ctx->curr_exp = lbm_car(rest);
    return;
  }
  // Else create a continuation
  CHECK_STACK(lbm_push_u32_3(&ctx->K, env, lbm_cdr(rest), lbm_enc_u(PROGN_REST)));
  ctx->curr_exp = lbm_car(rest);
  ctx->curr_env = env;
}

static inline void cont_spawn_all(eval_context_t *ctx) {
  lbm_value rest;
  lbm_value env;
  lbm_pop_u32_2(&ctx->K, &rest, &env);
  if (lbm_type_of(rest) == LBM_VAL_TYPE_SYMBOL && rest == NIL) {
    ctx->app_cont = true;
    return;
  }

  lbm_value cid_val = lbm_enc_u((lbm_uint)next_ctx_id); /* CIDS range from 0 - 65535, so this is fine */
  lbm_value cid_list;
  WITH_GC(cid_list, lbm_cons(cid_val, ctx->r), rest, env);

  lbm_cid cid = create_ctx(lbm_car(rest),
                       env,
                       EVAL_CPS_DEFAULT_STACK_SIZE);
  if (!cid) {
    lbm_set_car(cid_list, lbm_enc_sym(SYM_NIL));
  }
  CHECK_STACK(lbm_push_u32_3(&ctx->K, env, lbm_cdr(rest), lbm_enc_u(SPAWN_ALL)));
  ctx->r = cid_list;
  ctx->app_cont = true;
}

static inline void cont_wait(eval_context_t *ctx) {

  lbm_value cid_val;
  lbm_pop_u32(&ctx->K, &cid_val);
  lbm_cid cid = (lbm_cid)lbm_dec_u(cid_val);

  lbm_value r;

  if (lbm_remove_done_ctx(cid, &r)) {
    ctx->r = r;
    ctx->app_cont = true;
  } else {
    CHECK_STACK(lbm_push_u32_2(&ctx->K, lbm_enc_u(cid), lbm_enc_u(WAIT)));
    ctx->r = lbm_enc_sym(SYM_TRUE);
    ctx->app_cont = true;
    yield_ctx(50000);
  }
  return;
}

static inline void cont_application(eval_context_t *ctx) {
  lbm_value count;
  lbm_pop_u32(&ctx->K, &count);

  lbm_uint *fun_args = lbm_get_stack_ptr(&ctx->K, lbm_dec_u(count)+1);

  if (fun_args == NULL) {
    ctx->r = lbm_enc_sym(SYM_FATAL_ERROR);
    return;
  }
  lbm_value fun = fun_args[0];

  if (lbm_type_of(fun) == LBM_PTR_TYPE_CONS) { // a closure (it better be)
    lbm_value args = NIL;
    for (lbm_uint i = lbm_dec_u(count); i > 0; i --) {
      CONS_WITH_GC(args, fun_args[i], args, args);
    }

    lbm_value params  = lbm_car(lbm_cdr(fun));
    lbm_value exp     = lbm_car(lbm_cdr(lbm_cdr(fun)));
    lbm_value clo_env = lbm_car(lbm_cdr(lbm_cdr(lbm_cdr(fun))));

    if (lbm_list_length(params) != lbm_list_length(args)) { // programmer error
      ERROR
        error_ctx(lbm_enc_sym(SYM_EERROR));
      return;
    }

    lbm_value local_env;
    WITH_GC(local_env, lbm_env_build_params_args(params, args, clo_env), args, NIL);

    if (lbm_dec_sym(local_env) == SYM_FATAL_ERROR) {
      ctx->r = local_env;
      return;
    }

    /* ************************************************************
       Odd area!  It feels like the callers environment should be
       explicitly restored after an application of a closure.
       However, if the callers environment is pushed onto the stack
       here, it will make the stack grow proportional to the call
       depth.

       I am very unsure about the correctness here.
       ************************************************************ */
    lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
    ctx->curr_exp = exp;
    ctx->curr_env = local_env;
    return;
  } else if (lbm_type_of(fun) == LBM_VAL_TYPE_SYMBOL) {

    lbm_value res;

    /* eval_cps specific operations */
    lbm_uint dfun = lbm_dec_sym(fun);
    if (dfun == SYM_READ || dfun == SYM_READ_PROGRAM) {
      if (lbm_dec_u(count) == 1) {
        lbm_value stream = NIL;
        if (lbm_type_of(fun_args[1]) == LBM_PTR_TYPE_ARRAY) {
          lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(fun_args[1]);
          if(array->elt_type == LBM_VAL_TYPE_CHAR) {
            stream = token_stream_from_string_value(fun_args[1]);
          }
        } else if (lbm_type_of(fun_args[1]) == LBM_PTR_TYPE_STREAM) {
          stream = fun_args[1];
        } else {
          error_ctx(lbm_enc_sym(SYM_EERROR));
          return;
        }

        lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
        CHECK_STACK(lbm_push_u32_3(&ctx->K, stream, fun, lbm_enc_u(READ)));
        ctx->r = NIL;
        ctx->app_cont = true;
      } else {
        error_ctx(lbm_enc_sym(SYM_EERROR));
      }
      return;
    } else if (dfun == SYM_YIELD) {
      if (lbm_dec_u(count) == 1 && lbm_is_number(fun_args[1])) {
        lbm_uint ts = lbm_dec_as_u(fun_args[1]);
        lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
        yield_ctx(ts);
      } else {
         error_ctx(lbm_enc_sym(SYM_EERROR));
      }
      return;
    } else if (dfun == SYM_WAIT) {
      if (lbm_type_of(fun_args[1]) == LBM_VAL_TYPE_I) {
        lbm_cid cid = (lbm_cid)lbm_dec_u(fun_args[1]);
        lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
        CHECK_STACK(lbm_push_u32_2(&ctx->K, lbm_enc_u(cid), lbm_enc_u(WAIT)));
        ctx->r = lbm_enc_sym(SYM_TRUE);
        ctx->app_cont = true;
        yield_ctx(50000);
      } else {
        ERROR
        error_ctx(lbm_enc_sym(SYM_EERROR));
      }
      return;
    } else if (dfun == SYM_EVAL) {
      ctx->curr_exp = fun_args[1];
      lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
      return;
    } else if (dfun == SYM_EVAL_PROGRAM) {
      lbm_value prg = fun_args[1];
      prg = lbm_list_append(prg, ctx->program);

      lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);

      if (lbm_type_of(prg) != LBM_PTR_TYPE_CONS) {
        ctx->r = lbm_enc_sym(SYM_EERROR);
        ctx->app_cont = true;
        return;
      }

      ctx->program = lbm_cdr(prg);
      ctx->curr_exp = lbm_car(prg);
      return;
    } else if (dfun == SYM_SEND) {
      lbm_value status = lbm_enc_sym(SYM_EERROR);

      if (lbm_dec_u(count) == 2) {

        if (lbm_type_of(fun_args[1]) == LBM_VAL_TYPE_U) { /* CID is of U type */
          lbm_cid cid = (lbm_cid)lbm_dec_u(fun_args[1]);
          lbm_value msg = fun_args[2];

          WITH_GC(status, find_receiver_and_send(cid, msg), NIL, NIL);
        }
      }
      /* return the status */
      lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
      ctx->r = status;
      ctx->app_cont = true;
      return;
    }
    else if (lbm_is_fundamental(fun)) {
      /* If it is not a eval_cps specific function, it may be a fundamental operation */
      WITH_GC(res, lbm_fundamental(&fun_args[1], lbm_dec_u(count), fun), NIL, NIL);
      if (lbm_type_of(res) == LBM_VAL_TYPE_SYMBOL &&
          lbm_dec_sym(res) == SYM_EERROR) {
        ERROR
          error_ctx(res);
      }  else {
        lbm_stack_drop(&ctx->K, lbm_dec_u(count)+1);
        ctx->app_cont = true;
        ctx->r = res;
      }
      return;
    }
  }
  // It may be an extension
  extension_fptr f = lbm_get_extension(lbm_dec_sym(fun));
  if (f == NULL) {
    ERROR
      error_ctx(lbm_enc_sym(SYM_EERROR));
    return;
  }

  lbm_value ext_res;
  WITH_GC(ext_res, f(&fun_args[1] , lbm_dec_u(count)), NIL, NIL);

  lbm_stack_drop(&ctx->K, lbm_dec_u(count) + 1);

  ctx->app_cont = true;
  ctx->r = ext_res;
  return;
}

static inline void cont_application_args(eval_context_t *ctx) {
  lbm_value count;
  lbm_value env;
  lbm_value rest;
  lbm_value arg = ctx->r;
  lbm_pop_u32_3(&ctx->K, &rest, &count, &env);

  CHECK_STACK(lbm_push_u32(&ctx->K, arg));
  /* Deal with general fundamentals */
  if (lbm_type_of(rest) == LBM_VAL_TYPE_SYMBOL &&
      rest == NIL) {
    // no arguments
    CHECK_STACK(lbm_push_u32_2(&ctx->K, count, lbm_enc_u(APPLICATION)));
    ctx->app_cont = true;
  } else if (lbm_type_of(rest) == LBM_PTR_TYPE_CONS) {
    CHECK_STACK(lbm_push_u32_4(&ctx->K, env, lbm_enc_u(lbm_dec_u(count) + 1), lbm_cdr(rest), lbm_enc_u(APPLICATION_ARGS)));
    ctx->curr_exp = lbm_car(rest);
    ctx->curr_env = env;
  } else {
    /* TODO: Should pop count elements from the stack here as this application is an error */
    ctx->curr_exp = lbm_enc_sym(SYM_EERROR);
    ctx->curr_env = env;
  }
  return;
}

static inline void cont_and(eval_context_t *ctx) {
  lbm_value rest;
  lbm_value arg = ctx->r;
  lbm_pop_u32(&ctx->K, &rest);
  if (lbm_type_of(arg) == LBM_VAL_TYPE_SYMBOL &&
      lbm_dec_sym(arg) == SYM_NIL) {
    ctx->app_cont = true;
    ctx->r = lbm_enc_sym(SYM_NIL);
  } else if (lbm_type_of(rest) == LBM_VAL_TYPE_SYMBOL &&
             rest == NIL) {
    ctx->app_cont = true;
  } else {
    CHECK_STACK(lbm_push_u32_2(&ctx->K, lbm_cdr(rest), lbm_enc_u(AND)));
    ctx->curr_exp = lbm_car(rest);
  }
}

static inline void cont_or(eval_context_t *ctx) {
  lbm_value rest;
  lbm_value arg = ctx->r;
  lbm_pop_u32(&ctx->K, &rest);
  if (lbm_type_of(arg) != LBM_VAL_TYPE_SYMBOL ||
      lbm_dec_sym(arg) != SYM_NIL) {
    ctx->app_cont = true;
  } else  if (lbm_type_of(rest) == LBM_VAL_TYPE_SYMBOL &&
              rest == NIL) {
    ctx->app_cont = true;
    ctx->r = lbm_enc_sym(SYM_NIL);
  } else {
    CHECK_STACK(lbm_push_u32_2(&ctx->K, lbm_cdr(rest), lbm_enc_u(OR)));
    ctx->curr_exp = lbm_car(rest);
  }
}

static inline void cont_bind_to_key_rest(eval_context_t *ctx) {
  lbm_value key;
  lbm_value env;
  lbm_value rest;
  lbm_value arg = ctx->r;
  lbm_pop_u32_3(&ctx->K, &key, &env, &rest);

  lbm_env_modify_binding(env, key, arg);

  if ( lbm_type_of(rest) == LBM_PTR_TYPE_CONS ){
    lbm_value keyn = lbm_car(lbm_car(rest));
    lbm_value valn_exp = lbm_car(lbm_cdr(lbm_car(rest)));

    CHECK_STACK(lbm_push_u32_4(&ctx->K, lbm_cdr(rest), env, keyn, lbm_enc_u(BIND_TO_KEY_REST)));

    ctx->curr_exp = valn_exp;
    ctx->curr_env = env;
  } else {
    // Otherwise evaluate the expression in the populated env
    lbm_value exp;
    lbm_pop_u32(&ctx->K, &exp);
    ctx->curr_exp = exp;
    ctx->curr_env = env;
  }
}

static inline void cont_if(eval_context_t *ctx) {
  lbm_value then_branch;
  lbm_value else_branch;
  lbm_value arg = ctx->r;

  lbm_pop_u32_2(&ctx->K, &then_branch, &else_branch);

  if (lbm_type_of(arg) == LBM_VAL_TYPE_SYMBOL && lbm_dec_sym(arg) == SYM_TRUE) {
    ctx->curr_exp = then_branch;
  } else {
    ctx->curr_exp = else_branch;
  }
}

static inline void cont_match_many(eval_context_t *ctx) {

  lbm_value r = ctx->r;

  lbm_value rest_msgs;
  lbm_value pats;
  lbm_value exp;

  lbm_pop_u32_3(&ctx->K, &rest_msgs, &pats, &exp);

  if (lbm_type_of(r) == LBM_VAL_TYPE_SYMBOL &&
      (lbm_dec_sym(r) == SYM_NO_MATCH)) {

    if (lbm_type_of(rest_msgs) == LBM_VAL_TYPE_SYMBOL &&
        lbm_dec_sym(rest_msgs) == SYM_NIL) {

      ctx->curr_exp = exp;

    } else {
      /* try match the next one */
      CHECK_STACK(lbm_push_u32_4(&ctx->K, exp, pats, lbm_cdr(rest_msgs), lbm_enc_u(MATCH_MANY)));
      CHECK_STACK(lbm_push_u32_2(&ctx->K, lbm_car(pats), lbm_enc_u(MATCH)));
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

  lbm_pop_u32(&ctx->K, &patterns);

  if (lbm_type_of(patterns) == LBM_VAL_TYPE_SYMBOL && lbm_dec_sym(patterns) == SYM_NIL) {
    /* no more patterns */
    ctx->r = lbm_enc_sym(SYM_NO_MATCH);
    ctx->app_cont = true;
  } else if (lbm_type_of(patterns) == LBM_PTR_TYPE_CONS) {
    lbm_value pattern = lbm_car(lbm_car(patterns));
    lbm_value body    = lbm_car(lbm_cdr(lbm_car(patterns)));

    if (match(pattern, e, &new_env, &do_gc)) {
      ctx->curr_env = new_env;
      ctx->curr_exp = body;
    } else if (do_gc) {
      gc(NIL,NIL);
      do_gc = false;
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
      CHECK_STACK(lbm_push_u32_2(&ctx->K, lbm_cdr(patterns), lbm_enc_u(MATCH)));
      /* leave r unaltered */
      ctx->app_cont = true;
    }
  } else {
    /* TODO: return type error */
    ctx->r = lbm_enc_sym(SYM_TERROR);
    ctx->done = true;
  }
}


//#define PARSE_PROGRAM 1
#define PARSE            1
#define PARSE_LIST       2
#define QUOTE_RESULT     3
#define BACKQUOTE_RESULT 4
#define COMMAAT_RESULT   5
#define COMMA_RESULT     6
#define DOT_TERMINATE    7
#define EXPECT_CLOSEPAR  8

#define APPEND_CONTINUE 100
#define READ_DONE       110

static inline void cont_read(eval_context_t *ctx) {

  lbm_value stream = NIL;
  lbm_value prg_val = NIL;
  lbm_pop_u32_2(&ctx->K, &prg_val, &stream);

  lbm_stream_t *str = lbm_dec_stream(stream);

  lbm_value tok = ctx->r;
  bool read_done = false;

  bool app_cont = false;
  bool program = false;

  unsigned int sp_start = ctx->K.sp;

  if (lbm_type_of(prg_val) == LBM_VAL_TYPE_SYMBOL) {
    if (lbm_dec_sym(prg_val) == SYM_READ) program = false;
    else if (lbm_dec_sym(prg_val) == SYM_READ_PROGRAM) program = true;
  } else {
    ctx->r = lbm_enc_sym(SYM_FATAL_ERROR);
    return;
  }

  CHECK_STACK(lbm_push_u32(&ctx->K, lbm_enc_u(READ_DONE)));

  if (program) {
    CHECK_STACK(lbm_push_u32_3(&ctx->K, NIL, NIL, lbm_enc_u(APPEND_CONTINUE)));
  }

  while (!read_done) {

    if (app_cont) {
      lbm_value cont = NIL;
      lbm_pop_u32(&ctx->K, &cont);
      app_cont = false; // false unless explicitly set

      switch(lbm_dec_u(cont)) {

      case APPEND_CONTINUE: {
        lbm_value first_cell = NIL;
        lbm_value last_cell  = NIL;
        lbm_pop_u32_2(&ctx->K, &last_cell, &first_cell);

        if (lbm_type_of(ctx->r) == LBM_VAL_TYPE_SYMBOL &&
            lbm_dec_sym(ctx->r) == SYM_CLOSEPAR) {
          if (lbm_type_of(last_cell) == LBM_PTR_TYPE_CONS) {
            lbm_set_cdr(last_cell, NIL); // terminate the list
            ctx->r = first_cell;
          } else {
            // empty list case
            ctx->r = NIL;
          }
          app_cont = true;
        } else if (lbm_type_of(ctx->r) == LBM_VAL_TYPE_SYMBOL &&
                   lbm_dec_sym(ctx->r) == SYM_DOT) {
          CHECK_STACK(lbm_push_u32_3(&ctx->K,
                                 first_cell, last_cell,
                                 lbm_enc_u(DOT_TERMINATE)));
        } else {
          lbm_value new_cell = NIL;
          CONS_WITH_GC(new_cell, ctx->r, NIL, stream);
          if (lbm_type_of(last_cell) == LBM_PTR_TYPE_CONS) {
            lbm_set_cdr(last_cell, new_cell);
            last_cell = new_cell;
          } else {
            first_cell = last_cell = new_cell;
          }
          CHECK_STACK(lbm_push_u32_3(&ctx->K,
                                 first_cell, last_cell,
                                 lbm_enc_u(APPEND_CONTINUE)));
        }
      } break;
      case EXPECT_CLOSEPAR: {
        if (lbm_type_of(ctx->r) == LBM_VAL_TYPE_SYMBOL &&
            lbm_dec_sym(ctx->r) == SYM_CLOSEPAR) {
          lbm_value res = NIL;
          lbm_pop_u32(&ctx->K, &res);
          ctx->r = res;
          app_cont = true;
        } else {
          ctx->r = lbm_enc_sym(SYM_RERROR);
          read_done = true;
        }
      } break;
      case DOT_TERMINATE: {
        lbm_value first_cell = NIL;
        lbm_value last_cell  = NIL;
        lbm_pop_u32_2(&ctx->K, &last_cell, &first_cell);

        if (lbm_type_of(ctx->r) == LBM_VAL_TYPE_SYMBOL &&
            (lbm_dec_sym(ctx->r) == SYM_CLOSEPAR ||
             lbm_dec_sym(ctx->r) == SYM_DOT)) {
          ctx->r = lbm_enc_sym(SYM_RERROR);
          read_done = true;
        } else {
          if (lbm_type_of(last_cell) == LBM_PTR_TYPE_CONS) {
            lbm_set_cdr(last_cell, ctx->r);
            ctx->r = first_cell;
            CHECK_STACK(lbm_push_u32_2(&ctx->K,
                                   ctx->r,
                                   lbm_enc_u(EXPECT_CLOSEPAR)));
          } else {
            ctx->r = lbm_enc_sym(SYM_RERROR);
            read_done = true;
          }
        }
      } break;
      case READ_DONE:
        tok = token_stream_get(str);
        if (tok != lbm_enc_sym(SYM_TOKENIZER_DONE)) {
          ctx->r = lbm_enc_sym(SYM_RERROR);
        }
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

      if (lbm_type_of(tok) == LBM_VAL_TYPE_SYMBOL) {
        switch (lbm_dec_sym(tok)) {
        case SYM_RERROR:
        case SYM_MERROR:
          // check if stack is corrupted
          if (program) {
            if (ctx->K.sp >= sp_start+4 &&
                ctx->K.data[sp_start] == lbm_enc_u(READ_DONE) &&
                ctx->K.data[sp_start+3] == lbm_enc_u(APPEND_CONTINUE)) {
              // stack seems fine 
              ctx->K.sp = sp_start; 
              ctx->r = lbm_enc_sym(SYM_RERROR);
              ctx->app_cont = true;
              read_done = true;
              // as the continuation will run, the result will
              // be an eval error rather than an READ_ERROR.
            } else {
              error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
              read_done = true;
            }
          } else {
            // This may be dead code as the expression case does
            // not seem to end up here.  
            if (ctx->K.sp >= sp_start &&
                ctx->K.data[sp_start] == lbm_enc_u(READ_DONE)) {
              ctx->K.sp = sp_start; 
              ctx->r = lbm_enc_sym(SYM_RERROR);
              ctx->app_cont = true;
              read_done = true;
            } else {
              error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
              read_done = true;
            }
          }
          break;
        case SYM_TOKENIZER_DONE:
          if (program) {

            if (ctx->K.sp == sp_start + 4) {
              ctx->r = lbm_enc_sym(SYM_CLOSEPAR);
              app_cont = true;
            } else if (ctx->K.data[sp_start] == lbm_enc_u(READ_DONE) &&
                       ctx->K.data[sp_start+3] == lbm_enc_u(APPEND_CONTINUE)) {
              // Parsing failed but stack seems to not be corrupted.
              ctx->K.sp = sp_start;
              ctx->r = lbm_enc_sym(SYM_RERROR);
              ctx->app_cont = true;
              read_done = true;
            } else {
              // parsing failed and left a corrupted stack.
              error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
              read_done = true;
            }
          } else {
            if (ctx->K.sp > sp_start &&
                ctx->K.data[sp_start] == lbm_enc_u(READ_DONE)) {
              ctx->K.sp = sp_start;
              ctx->r = lbm_enc_sym(SYM_RERROR);
              ctx->app_cont = true;
              read_done = true;
            } else if (ctx->K.sp < sp_start) {
              /*the stack is broken */
              error_ctx(lbm_enc_sym(SYM_FATAL_ERROR));
              read_done = true;
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
          CHECK_STACK(lbm_push_u32_3(&ctx->K,
                                 NIL, NIL,
                                 lbm_enc_u(APPEND_CONTINUE)));
          app_cont = false;
          break;
        case SYM_QUOTE:
          CHECK_STACK(lbm_push_u32(&ctx->K,
                               lbm_enc_u(QUOTE_RESULT)));
          app_cont = false;
          break;
        case SYM_BACKQUOTE:
          CHECK_STACK(lbm_push_u32(&ctx->K,
                               lbm_enc_u(BACKQUOTE_RESULT)));
          app_cont = false;
          break;
        case SYM_COMMAAT:
          CHECK_STACK(lbm_push_u32(&ctx->K,
                               lbm_enc_u(COMMAAT_RESULT)));
          app_cont = false;
          break;
        case SYM_COMMA:
          CHECK_STACK(lbm_push_u32(&ctx->K,
                               lbm_enc_u(COMMA_RESULT)));
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
    lbm_pop_u32(&ctx->K, &k);
    ctx->app_cont = false;

    switch(lbm_dec_u(k)) {
    case DONE: advance_ctx(); return;
    case SET_GLOBAL_ENV:   cont_set_global_env(ctx); return;
    case PROGN_REST:       cont_progn_rest(ctx); return;
    case SPAWN_ALL:        cont_spawn_all(ctx); return;
    case WAIT:             cont_wait(ctx); return;
    case APPLICATION:      cont_application(ctx); return;
    case APPLICATION_ARGS: cont_application_args(ctx); return;
    case AND:              cont_and(ctx); return;
    case OR:               cont_or(ctx); return;
    case BIND_TO_KEY_REST: cont_bind_to_key_rest(ctx); return;
    case IF:               cont_if(ctx); return;
    case MATCH:            cont_match(ctx); return;
    case MATCH_MANY:       cont_match_many(ctx); return;
    case READ:             cont_read(ctx); return;
    default:
      ERROR
      error_ctx(lbm_enc_sym(SYM_EERROR));
      return;
    }
  }

  lbm_value head;

  switch (lbm_type_of(ctx->curr_exp)) {

  case LBM_VAL_TYPE_SYMBOL: eval_symbol(ctx); return;
  case LBM_PTR_TYPE_BOXED_F: /* fall through */
  case LBM_PTR_TYPE_BOXED_U:
  case LBM_PTR_TYPE_BOXED_I:
  case LBM_VAL_TYPE_I:
  case LBM_VAL_TYPE_U:
  case LBM_VAL_TYPE_CHAR:
  case LBM_PTR_TYPE_ARRAY:
  case LBM_PTR_TYPE_REF:
  case LBM_PTR_TYPE_STREAM: eval_selfevaluating(ctx);  return;

  case LBM_PTR_TYPE_CONS:
    head = lbm_car(ctx->curr_exp);

    if (lbm_type_of(head) == LBM_VAL_TYPE_SYMBOL) {

      lbm_uint sym_id = lbm_dec_sym(head);

      switch(sym_id) {
      case SYM_QUOTE:   eval_quote(ctx); return;
      case SYM_DEFINE:  eval_define(ctx); return;
      case SYM_PROGN:   eval_progn(ctx); return;
      case SYM_SPAWN:   eval_spawn(ctx); return;
      case SYM_LAMBDA:  eval_lambda(ctx); return;
      case SYM_IF:      eval_if(ctx); return;
      case SYM_LET:     eval_let(ctx); return;
      case SYM_AND:     eval_and(ctx); return;
      case SYM_OR:      eval_or(ctx); return;
      case SYM_MATCH:   eval_match(ctx); return;
        /* message passing primitives */
      case SYM_RECEIVE: eval_receive(ctx); return;

      default: break; /* May be general application form. Checked below*/
      }
    } // If head is symbol
    CHECK_STACK(lbm_push_u32_4(&ctx->K,
                           ctx->curr_env,
                           lbm_enc_u(0),
                           lbm_cdr(ctx->curr_exp),
                           lbm_enc_u(APPLICATION_ARGS)));

    ctx->curr_exp = head; // evaluate the function
    return;
  default:
    // BUG No applicable case!
    ERROR
    error_ctx(lbm_enc_sym(SYM_EERROR));
    break;
  }
  return;
}

void lbm_pause_eval(void) {
  eval_cps_next_state = EVAL_CPS_STATE_PAUSED;
}

void lbm_step_eval(void) {
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

    eval_cps_run_state = eval_cps_next_state;

    switch (eval_cps_run_state) {
    case EVAL_CPS_STATE_INIT:
      eval_cps_next_state = EVAL_CPS_STATE_RUNNING;
      break;
    case EVAL_CPS_STATE_STEP:
      eval_cps_next_state = EVAL_CPS_STATE_PAUSED;
      break;
    case EVAL_CPS_STATE_PAUSED:
      eval_cps_next_state = EVAL_CPS_STATE_PAUSED;
      usleep_callback(EVAL_CPS_MIN_SLEEP);
      continue; /* jump back to start of eval_running loop */
    case EVAL_CPS_STATE_KILL:
      eval_running = false;
      continue;
    default:
      break;
    }

    //if (heap_size() - heap_num_allocated() < PRELIMINARY_GC_MEASURE) {
    //  gc(NIL, NIL);
    //}
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

lbm_value evaluate_non_concurrent(void) {

  lbm_cid cid = ctx_running->id;

  while (ctx_running) {
    evaluation_step();
  }

  eval_context_t *ctx = lookup_ctx(&done, cid);
  if (ctx) {
    drop_ctx(&done, ctx);
  } else {
    return lbm_enc_sym(SYM_FATAL_ERROR);
  }

  return ctx_non_concurrent.r;
}

lbm_cid lbm_eval_program(lbm_value lisp) {
  return create_ctx(lisp, NIL, 256);
}

lbm_cid lbm_eval_program_ext(lbm_value lisp, unsigned int stack_size) {
  return create_ctx(lisp, NIL, stack_size);
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

  lbm_value nil_entry = lbm_cons(NIL, NIL);
  *lbm_get_env_ptr() = lbm_cons(nil_entry, *lbm_get_env_ptr());

  if (lbm_type_of(nil_entry) == LBM_VAL_TYPE_SYMBOL ||
      lbm_type_of(*lbm_get_env_ptr()) == LBM_VAL_TYPE_SYMBOL) res = 0;

  eval_running = true;

  return res;
}

/****************************************************/
/* Interface for loading and running programs and   */
/* expressions                                      */

static lbm_cid eval_cps_load_and_eval(lbm_tokenizer_char_stream_t *tokenizer, bool program) {

  lbm_stream_t *stream = NULL;

  stream = (lbm_stream_t*)lbm_memory_allocate(sizeof(lbm_stream_t) / 4);
  if (stream == NULL) {
    return 0; // No valid CID is 0
  }

  stream->state = (void*)tokenizer;
  stream->more = token_stream_more;
  stream->get  = token_stream_get;
  stream->peek = token_stream_peek;
  stream->drop = token_stream_drop;
  stream->put  = token_stream_put;

  lbm_value lisp_stream = lbm_stream_create(stream);

  if (lbm_type_of(lisp_stream) == LBM_VAL_TYPE_SYMBOL) {
    lbm_memory_free((uint32_t*)stream);
    return 0;
  }

  /* LISP ZONE */

  lbm_value launcher = NIL;
  launcher = lbm_cons(lisp_stream, NIL);
  launcher = lbm_cons(lbm_enc_sym(program ? SYM_READ_PROGRAM : SYM_READ), launcher);
  lbm_value evaluator = lbm_cons(launcher, NIL);
  evaluator = lbm_cons(lbm_enc_sym(program ? SYM_EVAL_PROGRAM : SYM_EVAL), evaluator);
  lbm_value start_prg = lbm_cons(evaluator, NIL);

  /* LISP ZONE ENDS */

  if (lbm_type_of(launcher) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(evaluator) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(start_prg) != LBM_PTR_TYPE_CONS ) {
    lbm_memory_free((uint32_t*)stream);
    return 0;
  }
  return create_ctx(start_prg, NIL, 256);
}

lbm_cid lbm_load_and_eval_expression(lbm_tokenizer_char_stream_t *tokenizer) {
  return eval_cps_load_and_eval(tokenizer, false);
}

static lbm_cid eval_cps_load_and_define(lbm_tokenizer_char_stream_t *tokenizer, char *symbol, bool program) {

  lbm_stream_t *stream = NULL;

  stream = (lbm_stream_t*)lbm_memory_allocate(sizeof(lbm_stream_t) / 4);
  if (stream == NULL) {
    return 0; // No valid CID is 0
  }

  stream->state = (void*)tokenizer;
  stream->more = token_stream_more;
  stream->get  = token_stream_get;
  stream->peek = token_stream_peek;
  stream->drop = token_stream_drop;
  stream->put  = token_stream_put;

  lbm_value lisp_stream = lbm_stream_create(stream);

  if (lbm_type_of(lisp_stream) == LBM_VAL_TYPE_SYMBOL) {
    lbm_memory_free((uint32_t*)stream);
    return 0;
  }

  lbm_uint sym_id;

  if (!lbm_get_symbol_by_name(symbol, &sym_id)) {
    if (!lbm_add_symbol(symbol, &sym_id)) {
      lbm_memory_free((uint32_t*)stream);
      return 0;
    }
  }

  /* LISP ZONE */

  lbm_value launcher = NIL;
  launcher = lbm_cons(lisp_stream, NIL);
  launcher = lbm_cons(lbm_enc_sym(program ? SYM_READ_PROGRAM : SYM_READ), launcher);
  lbm_value binding = lbm_cons(launcher, NIL);
  binding = lbm_cons(lbm_enc_sym(sym_id), binding);
  lbm_value definer = lbm_cons(binding, NIL);
  definer = lbm_cons(lbm_enc_sym(SYM_DEFINE), binding);
  definer  = lbm_cons(definer, NIL);
  /* LISP ZONE ENDS */

  if (lbm_type_of(launcher) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(binding) != LBM_PTR_TYPE_CONS ||
      lbm_type_of(definer) != LBM_PTR_TYPE_CONS ) {
    lbm_memory_free((uint32_t*)stream);
    return 0;
  }
  return create_ctx(definer, NIL, 256);
}


lbm_cid lbm_load_and_define_program(lbm_tokenizer_char_stream_t *tokenizer, char *symbol) {
  return eval_cps_load_and_define(tokenizer, symbol, true);
}

lbm_cid lbm_load_and_define_expression(lbm_tokenizer_char_stream_t *tokenizer, char *symbol) {
  return eval_cps_load_and_define(tokenizer, symbol, false);
}

lbm_cid lbm_load_and_eval_program(lbm_tokenizer_char_stream_t *tokenizer) {
  return eval_cps_load_and_eval(tokenizer, true);
}
