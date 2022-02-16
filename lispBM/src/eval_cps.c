/*
    Copyright 2018, 2020, 2021 Joel Svensson    svenssonjoel@yahoo.se

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

#define FOF(done, x)                            \
  if (!(x)) {                                   \
    (done)=true;                                \
    ctx->r = enc_sym(SYM_FATAL_ERROR);          \
    return;                                     \
  }

#define WITH_GC(y, x, remember1,remember2)      \
  (y) = (x);                                    \
  if (is_symbol_merror((y))) {                  \
    gc(remember1, remember2);                   \
    (y) = (x);                                  \
    if (is_symbol_merror((y))) {                \
      ctx_running->done = true;                 \
      error_ctx(enc_sym(SYM_MERROR));           \
      return;                                   \
    }                                           \
    /* continue executing statements below */   \
  }

#define PRELIMINARY_GC_MEASURE 30

static int gc(VALUE, VALUE);
static VALUE NIL;
static VALUE NONSENSE;
static void error_ctx(VALUE);
static eval_context_t *ctx_running = NULL;

static VALUE cons_with_gc(VALUE head, VALUE tail, VALUE remember) {
  VALUE res = cons(head, tail);
  if (is_symbol_merror(res)) {
    gc(remember, NIL);
    res = cons(head, tail);
    if (is_symbol_merror(res)) {
        ctx_running->done = true;
        error_ctx(enc_sym(SYM_MERROR));
    }
  }
  return res;
}

#define CONS_WITH_GC(res, h, t, r)              \
  (res) = cons_with_gc(h,t,r);                  \
  if (is_symbol_merror(res)) {                  \
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

void eval_cps_set_usleep_callback(void (*fptr)(uint32_t)) {
  usleep_callback = fptr;
}

void eval_cps_set_timestamp_us_callback(uint32_t (*fptr)(void)) {
  timestamp_us_callback = fptr;
}

void eval_cps_set_ctx_done_callback(void (*fptr)(eval_context_t *)) {
  ctx_done_callback = fptr;
}


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

void eval_cps_running_iterator(ctx_fun f, void *arg1, void *arg2){
  queue_iterator(&queue, f, arg1, arg2);
}

void eval_cps_blocked_iterator(ctx_fun f, void *arg1, void *arg2){
  queue_iterator(&blocked, f, arg1, arg2);
}

void eval_cps_done_iterator(ctx_fun f, void *arg1, void *arg2){
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

static eval_context_t *lookup_ctx(eval_context_queue_t *q, CID cid) {

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

bool eval_cps_remove_done_ctx(CID cid, VALUE *v) {

  eval_context_t *ctx = lookup_ctx(&done, cid);

  if (ctx) {
    drop_ctx(&done, ctx);

    *v = ctx->r;
    stack_free(&ctx->K);
    memory_free((uint32_t*)ctx);
    return true;
  }
  return false;
}

/* Dangerous function that will lock up if called
   with the incorrect cid
   TODO: replace with less dangerous alternatives
*/
VALUE eval_cps_wait_ctx(CID cid) {

  eval_context_t *ctx = NULL;
  VALUE r = enc_sym(SYM_NIL);

  while (!ctx) {
    ctx = lookup_ctx(&done, cid);
    if (ctx) {
      eval_cps_remove_done_ctx(cid, &r);
      return r;
    }
    usleep_callback(1000);
  }
  return r;
}

static void error_ctx(VALUE err_val) {
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
  ctx_running->r = enc_sym(SYM_TRUE);
  ctx_running->app_cont = true;
  enqueue_ctx(&queue,ctx_running);
  ctx_running = NULL;
}

static CID create_ctx(VALUE program, VALUE env, uint32_t stack_size, bool grow_stack) {

  if (next_ctx_id == 0) return 0; // overflow of CIDs

  if (type_of(program) != PTR_TYPE_CONS) return 0;

  eval_context_t *ctx = NULL;
  ctx = (eval_context_t*)memory_allocate(sizeof(eval_context_t) / 4);
  if (ctx == NULL) return 0;

  ctx->program = cdr(program);
  ctx->curr_exp = car(program);
  ctx->curr_env = env;
  ctx->mailbox = enc_sym(SYM_NIL);
  ctx->done = false;
  ctx->app_cont = false;
  ctx->timestamp = 0;
  ctx->sleep_us = 0;
  ctx->prev = NULL;
  ctx->next = NULL;
  if (next_ctx_id > CID_MAX) {
    memory_free((uint32_t*)ctx);
    return 0;
  }

  ctx->id = (uint16_t)next_ctx_id++;
  if (!stack_allocate(&ctx->K, stack_size, grow_stack)) {
    memory_free((uint32_t*)ctx);
    return 0;
  }
  if (!push_u32(&ctx->K, enc_u(DONE))) {
    stack_free(&ctx->K);
    memory_free((uint32_t*)ctx);
    return 0;
  }

  enqueue_ctx(&queue,ctx);

  return ctx->id;
}

/* Advance execution to the next expression in the program */
static void advance_ctx(void) {

  if (type_of(ctx_running->program) == PTR_TYPE_CONS) {
    push_u32(&ctx_running->K, enc_u(DONE));
    ctx_running->curr_exp = car(ctx_running->program);
    ctx_running->curr_env = enc_sym(SYM_NIL);
    ctx_running->program = cdr(ctx_running->program);
    ctx_running->r = NIL;
    ctx_running->app_cont = false;

  } else {
    ctx_running->done = true;
    finish_ctx();
  }
}



static VALUE find_receiver_and_send(CID cid, VALUE msg) {
  eval_context_t *found = NULL;

  found = lookup_ctx(&blocked, cid);

  if (found == NULL) {
    found = lookup_ctx(&queue, cid);
  }

  if (found) {
    VALUE new_mailbox = cons(msg, found->mailbox);

    if (type_of(new_mailbox) == VAL_TYPE_SYMBOL) {
      return new_mailbox; /* An error symbol */
    }

    found->mailbox = new_mailbox;

    drop_ctx(&blocked,found);
    drop_ctx(&queue,found);

    enqueue_ctx(&queue,found);
    return enc_sym(SYM_TRUE);
  }

  /* check the current context */
  if (ctx_running->id == cid) {
    VALUE new_mailbox = cons(msg, ctx_running->mailbox);

    if (type_of(new_mailbox) == VAL_TYPE_SYMBOL) {
      return new_mailbox; /* An error symbol */
    }
    ctx_running->mailbox = new_mailbox;
    return enc_sym(SYM_TRUE);
  }

  return enc_sym(SYM_NIL);
}

static VALUE remove_from_list(int n, VALUE list) {
  int c = 0;
  VALUE res;
  VALUE curr = list;

  VALUE tmp = enc_sym(SYM_NIL);

  while (type_of(curr) == PTR_TYPE_CONS) {
    if (n == c) {
      curr = cdr(curr);
      break;
    }
    tmp = cons(car(curr), tmp);
    if (type_of(tmp) == VAL_TYPE_SYMBOL) {
      res = tmp;
      return res;
    }
    curr = cdr(curr);
    c++;
  }

  res = curr; /*res is the tail */
  curr = tmp;
  if ( c != 0) {
    while (type_of(curr) == PTR_TYPE_CONS) {
      res = cons(car(curr),res);
      if (type_of(res) == VAL_TYPE_SYMBOL) {
        return res;
      }
      curr = cdr(curr);
    }
  }
  return res;
}

/* Pattern matching is currently implemented as a recursive
   function and make use of stack relative to the size of
   expressions that are being matched. */
static bool match(VALUE p, VALUE e, VALUE *env, bool *gc) {

  VALUE binding;

  if (is_match_binder(p)) {
    VALUE var = car(cdr(p));
    VALUE bindertype = car(p);

    if (!is_symbol(var)) return false;

    switch (dec_sym(bindertype)) {
    case SYM_MATCH_ANY:
      if (dec_sym(var) == SYM_DONTCARE) {
        return true;
      } else {
        break;
      }
      return false;
    case SYM_MATCH_I28:
      if (type_of(e) == VAL_TYPE_I) {
        if (dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_U28:
      if (type_of(e) == VAL_TYPE_U) {
        if (dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_FLOAT:
      if (type_of(e) == PTR_TYPE_BOXED_F) {
        if (dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    case SYM_MATCH_CONS:
      if (type_of(e) == PTR_TYPE_CONS) {
        if (dec_sym(var) == SYM_DONTCARE) {
          return true;
        } else {
          break;
        }
      }
      return false;
    default: /* this should be an error case */
      return false;
    }
    binding = cons(var, e);
    *env = cons(binding, *env);
    if (type_of(binding) == VAL_TYPE_SYMBOL ||
        type_of(*env) == VAL_TYPE_SYMBOL) {
      *gc = true;
      return false;
    }
    return true;
  }

  if (is_symbol(p)) {
    if (dec_sym(p) == SYM_DONTCARE) return true;
    return (p == e);
  }

  if (type_of(p) == PTR_TYPE_CONS &&
      type_of(e) == PTR_TYPE_CONS) {

    VALUE headp = car(p);
    VALUE heade = car(e);
    if (!match(headp, heade, env, gc)) {
      return false;
    }
    return match (cdr(p), cdr(e), env, gc);
  } else if (p == e) {
    return true;
  }
  return false;
}

static int find_match(VALUE plist, VALUE elist, VALUE *e, VALUE *env, bool *gc) {

  VALUE curr_p = plist;
  VALUE curr_e = elist;
  int n = 0;
  while (type_of(curr_e) == PTR_TYPE_CONS) {
    while (type_of(curr_p) == PTR_TYPE_CONS) {
      if (match(car(car(curr_p)), car(curr_e), env, gc)) {
        if (*gc) return -1;
        *e = car(cdr(car(curr_p)));
        return n;
      }
      curr_p = cdr(curr_p);
    }
    curr_p = plist;       /* search all patterns against next exp */
    curr_e = cdr(curr_e);
    n ++;
  }

  return -1;
}

/****************************************************/
/* Garbage collection                               */
static int gc(VALUE remember1, VALUE remember2) {

  gc_state_inc();
  gc_mark_freelist();
  gc_mark_phase(*env_get_global_ptr());
  gc_mark_phase(remember1);
  gc_mark_phase(remember2);

  eval_context_t *curr = queue.first;
  while (curr) {
    gc_mark_phase(curr->curr_env);
    gc_mark_phase(curr->curr_exp);
    gc_mark_phase(curr->program);
    gc_mark_phase(curr->r);
    gc_mark_phase(curr->mailbox);
    gc_mark_aux(curr->K.data, curr->K.sp);
    curr = curr->next;
  }

  curr = done.first;
  while (curr) {
    gc_mark_phase(curr->r);
    curr = curr->next;
  }


  curr = blocked.first;
  while (curr) {
    gc_mark_phase(curr->curr_env);
    gc_mark_phase(curr->curr_exp);
    gc_mark_phase(curr->program);
    gc_mark_phase(curr->r);
    gc_mark_phase(curr->mailbox);
    gc_mark_aux(curr->K.data, curr->K.sp);
    curr = curr->next;
  }

  if (ctx_running) {
    gc_mark_phase(ctx_running->curr_env);
    gc_mark_phase(ctx_running->curr_exp);
    gc_mark_phase(ctx_running->program);
    gc_mark_phase(ctx_running->r);
    gc_mark_phase(ctx_running->mailbox);
    gc_mark_aux(ctx_running->K.data, ctx_running->K.sp);
  }

#ifdef VISUALIZE_HEAP
  heap_vis_gen_image();
#endif

  return gc_sweep_phase();

}


/****************************************************/
/* Evaluation functions                             */

static inline void eval_symbol(eval_context_t *ctx) {
  VALUE value;

  if (is_special(ctx->curr_exp) ||
      (extensions_lookup(dec_sym(ctx->curr_exp)) != NULL)) {
    // Special symbols and extension symbols evaluate to themself
    value = ctx->curr_exp;
  } else {
    // If not special, check if there is a binding in the environments
    value = env_lookup(ctx->curr_exp, ctx->curr_env);
    if (type_of(value) == VAL_TYPE_SYMBOL &&
        dec_sym(value) == SYM_NOT_FOUND) {

      value = env_lookup(ctx->curr_exp, *env_get_global_ptr());
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
  ctx->r = car(cdr(ctx->curr_exp));
  ctx->app_cont = true;
}

static inline void eval_define(eval_context_t *ctx) {
  VALUE key = car(cdr(ctx->curr_exp));
  VALUE val_exp = car(cdr(cdr(ctx->curr_exp)));

  if (type_of(key) != VAL_TYPE_SYMBOL ||
      key == NIL) {
    ERROR
      error_ctx(enc_sym(SYM_EERROR));
    return;
  }

  FOF(ctx->done, push_u32_2(&ctx->K, key, enc_u(SET_GLOBAL_ENV)));
  ctx->curr_exp = val_exp;
}


static inline void eval_progn(eval_context_t *ctx) {
  VALUE exps = cdr(ctx->curr_exp);
  VALUE env  = ctx->curr_env;

  if (type_of(exps) == VAL_TYPE_SYMBOL && exps == NIL) {
    ctx->r = NIL;
    ctx->app_cont = true;
    return;
  }

  if (symrepr_is_error(exps)) {
    ERROR
      error_ctx(exps);
    return;
  }
  FOF(ctx->done, push_u32_3(&ctx->K, env, cdr(exps), enc_u(PROGN_REST)));
  ctx->curr_exp = car(exps);
  ctx->curr_env = env;
}

static inline void eval_spawn(eval_context_t *ctx) {
  VALUE prgs = cdr(ctx->curr_exp);
  VALUE env = ctx->curr_env;

  if (type_of(prgs) == VAL_TYPE_SYMBOL && prgs == NIL) {
    ctx->r = NIL;
    ctx->app_cont = true;
    return;
  }

  VALUE cid_list = NIL;
  FOF(ctx->done, push_u32_3(&ctx->K, env, prgs, enc_u(SPAWN_ALL)));
  ctx->r = cid_list;
  ctx->app_cont = true;
}


static inline void eval_lambda(eval_context_t *ctx) {

  VALUE env_cpy = env_copy_shallow(ctx->curr_env);

  if (is_symbol_merror(env_cpy)) {
    gc(NIL, NIL);
    env_cpy = env_copy_shallow(ctx->curr_env);

    if (is_symbol_merror(env_cpy)) {
       ctx_running->done = true;
       error_ctx(enc_sym(SYM_MERROR));
       return;
    }
  }

  VALUE env_end;
  VALUE body;
  VALUE params;
  VALUE closure;
  CONS_WITH_GC(env_end, env_cpy, NIL, env_cpy);
  CONS_WITH_GC(body, car(cdr(cdr(ctx->curr_exp))), env_end, env_end);
  CONS_WITH_GC(params, car(cdr(ctx->curr_exp)), body, body);
  CONS_WITH_GC(closure, enc_sym(SYM_CLOSURE), params, params);

  ctx->app_cont = true;
  ctx->r = closure;
  return;
}

static inline void eval_if(eval_context_t *ctx) {

  FOF(ctx->done, push_u32_3(&ctx->K,
                 car(cdr(cdr(cdr(ctx->curr_exp)))), // Else branch
                 car(cdr(cdr(ctx->curr_exp))),      // Then branch
                 enc_u(IF)));
  ctx->curr_exp = car(cdr(ctx->curr_exp));
}

static inline void eval_let(eval_context_t *ctx) {
  VALUE orig_env = ctx->curr_env;
  VALUE binds    = car(cdr(ctx->curr_exp)); // key value pairs.
  VALUE exp      = car(cdr(cdr(ctx->curr_exp))); // exp to evaluate in the new env.

  VALUE curr = binds;
  VALUE new_env = orig_env;

  if (type_of(binds) != PTR_TYPE_CONS) {
    // binds better be nil or there is a programmer error.
    ctx->curr_exp = exp;
    return;
  }

  // Implements letrec by "preallocating" the key parts
  while (type_of(curr) == PTR_TYPE_CONS) {
    VALUE key = car(car(curr));
    VALUE val = NIL;
    VALUE binding;
    CONS_WITH_GC(binding, key, val, NIL);
    CONS_WITH_GC(new_env, binding, new_env, new_env);

    curr = cdr(curr);
  }

  VALUE key0 = car(car(binds));
  VALUE val0_exp = car(cdr(car(binds)));

  FOF(ctx->done, push_u32_5(&ctx->K, exp, cdr(binds), new_env,
                 key0, enc_u(BIND_TO_KEY_REST)));
  ctx->curr_exp = val0_exp;
  ctx->curr_env = new_env;
  return;
}

static inline void eval_and(eval_context_t *ctx) {
  VALUE rest = cdr(ctx->curr_exp);
  if (type_of(rest) == VAL_TYPE_SYMBOL &&
      rest == NIL) {
    ctx->app_cont = true;
    ctx->r = enc_sym(SYM_TRUE);
  } else {
    FOF(ctx->done, push_u32_2(&ctx->K, cdr(rest), enc_u(AND)));
    ctx->curr_exp = car(rest);
  }
}

static inline void eval_or(eval_context_t *ctx) {
  VALUE rest = cdr(ctx->curr_exp);
  if (type_of(rest) == VAL_TYPE_SYMBOL &&
      rest == NIL) {
    ctx->app_cont = true;
    ctx->r = enc_sym(SYM_NIL);
    return;
  } else {
    FOF(ctx->done, push_u32_2(&ctx->K, cdr(rest), enc_u(OR)));
    ctx->curr_exp = car(rest);
  }
}

/* pattern matching experiment */
/* format:                     */
/* (match e (pattern body)     */
/*          (pattern body)     */
/*          ...  )             */
static inline void eval_match(eval_context_t *ctx) {

  VALUE rest = cdr(ctx->curr_exp);
  if (type_of(rest) == VAL_TYPE_SYMBOL &&
      rest == NIL) {
    /* Someone wrote the program (match) */
    ctx->app_cont = true;
    ctx->r = enc_sym(SYM_NIL); /* make up new specific symbol? */
    return;
  } else {
    FOF(ctx->done, push_u32_2(&ctx->K, cdr(rest), enc_u(MATCH)));
    ctx->curr_exp = car(rest); /* Evaluate e next*/
  }
}

static inline void eval_receive(eval_context_t *ctx) {

  if (type_of(ctx->mailbox) == VAL_TYPE_SYMBOL &&
      dec_sym(ctx->mailbox) == SYM_NIL) {
    /*nothing in the mailbox: block the context*/
    ctx->timestamp = timestamp_us_callback();
    ctx->sleep_us = 0;
    enqueue_ctx(&blocked,ctx);
    ctx_running = NULL;
  } else {
    VALUE pats = ctx->curr_exp;
    VALUE msgs = ctx->mailbox;

    if (type_of(pats) == VAL_TYPE_SYMBOL &&
        pats == NIL) {
      /* A receive statement without any patterns */
      ctx->app_cont = true;
      ctx->r = enc_sym(SYM_NIL);
    } else {
      /* The common case */
      VALUE e;
      VALUE new_env = ctx->curr_env;
      bool do_gc = false;;
      int n = find_match(cdr(pats), msgs, &e, &new_env, &do_gc);
      if (do_gc) {
        gc(NIL, NIL);
        do_gc = false;
        n = find_match(cdr(pats), msgs, &e, &new_env, &do_gc);
        if (do_gc) {
          ctx_running->done = true;
          error_ctx(enc_sym(SYM_MERROR));
          return;
        }
      }
      if (n >= 0 ) { /* Match */
        VALUE new_mailbox;
        WITH_GC(new_mailbox, remove_from_list(n, msgs), NIL, NIL);

        ctx->mailbox = new_mailbox;
        ctx->curr_env = new_env;
        ctx->curr_exp = e;
      } else { /* No match  go back to sleep */
        ctx->timestamp = timestamp_us_callback();
        ctx->sleep_us = 0;
        enqueue_ctx(&blocked,ctx);
        ctx_running = NULL;
        ctx->r = enc_sym(SYM_NO_MATCH);
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

  VALUE key;
  VALUE val = ctx->r;

  pop_u32(&ctx->K, &key);
  VALUE new_env;
  WITH_GC(new_env, env_set(*env_get_global_ptr(),key,val), key, NIL);

  *env_get_global_ptr() = new_env;
  ctx->r = key;

  if (!ctx->done)
    ctx->app_cont = true;

  return;
}

static inline void cont_progn_rest(eval_context_t *ctx) {
  VALUE rest;
  VALUE env;
  pop_u32_2(&ctx->K, &rest, &env);
  if (type_of(rest) == VAL_TYPE_SYMBOL && rest == NIL) {
    ctx->app_cont = true;
    return;
  }

  if (symrepr_is_error(rest)) {
    ERROR
      error_ctx(rest);
    return;
  }
  // allow for tail recursion
  if (type_of(cdr(rest)) == VAL_TYPE_SYMBOL &&
      cdr(rest) == NIL) {
    ctx->curr_exp = car(rest);
    return;
  }
  // Else create a continuation
  FOF(ctx->done, push_u32_3(&ctx->K, env, cdr(rest), enc_u(PROGN_REST)));
  ctx->curr_exp = car(rest);
  ctx->curr_env = env;
}

static inline void cont_spawn_all(eval_context_t *ctx) {
  VALUE rest;
  VALUE env;
  pop_u32_2(&ctx->K, &rest, &env);
  if (type_of(rest) == VAL_TYPE_SYMBOL && rest == NIL) {
    ctx->app_cont = true;
    return;
  }

  VALUE cid_val = enc_u((UINT)next_ctx_id); /* CIDS range from 0 - 65535, so this is fine */
  VALUE cid_list;
  WITH_GC(cid_list, cons(cid_val, ctx->r), rest, env);

  CID cid = create_ctx(car(rest),
                       env,
                       EVAL_CPS_DEFAULT_STACK_SIZE,
                       EVAL_CPS_DEFAULT_STACK_GROW_POLICY);
  if (!cid) {
    set_car(cid_list, enc_sym(SYM_NIL));
  }
  FOF(ctx->done, push_u32_3(&ctx->K, env, cdr(rest), enc_u(SPAWN_ALL)));
  ctx->r = cid_list;
  ctx->app_cont = true;
}

static inline void cont_wait(eval_context_t *ctx) {

  VALUE cid_val;
  pop_u32(&ctx->K, &cid_val);
  CID cid = (CID)dec_u(cid_val);

  VALUE r;

  if (eval_cps_remove_done_ctx(cid, &r)) {
    ctx->r = r;
    ctx->app_cont = true;
  } else {
    FOF(ctx->done, push_u32_2(&ctx->K, enc_u(cid), enc_u(WAIT)));
    ctx->r = enc_sym(SYM_TRUE);
    ctx->app_cont = true;
    yield_ctx(50000);
  }
  return;
}

static inline void cont_application(eval_context_t *ctx) {
  VALUE count;
  pop_u32(&ctx->K, &count);

  UINT *fun_args = stack_ptr(&ctx->K, dec_u(count)+1);

  if (fun_args == NULL) {
    ctx->r = enc_sym(SYM_FATAL_ERROR);
    return;
  }
  VALUE fun = fun_args[0];

  if (type_of(fun) == PTR_TYPE_CONS) { // a closure (it better be)
    VALUE args = NIL;
    for (UINT i = dec_u(count); i > 0; i --) {
      CONS_WITH_GC(args, fun_args[i], args, args);
    }

    VALUE params  = car(cdr(fun));
    VALUE exp     = car(cdr(cdr(fun)));
    VALUE clo_env = car(cdr(cdr(cdr(fun))));

    if (length(params) != length(args)) { // programmer error
      ERROR
        error_ctx(enc_sym(SYM_EERROR));
      return;
    }

    VALUE local_env;
    WITH_GC(local_env, env_build_params_args(params, args, clo_env), args, NIL);

    if (dec_sym(local_env) == SYM_FATAL_ERROR) {
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

    stack_drop(&ctx->K, dec_u(count)+1);
    ctx->curr_exp = exp;
    ctx->curr_env = local_env;
    return;
  } else if (type_of(fun) == VAL_TYPE_SYMBOL) {

     VALUE res;

    /* eval_cps specific operations */
    UINT dfun = dec_sym(fun);
    if (dfun == SYM_YIELD) {
      if (dec_u(count) == 1 && is_number(fun_args[1])) {
        UINT ts = dec_as_u(fun_args[1]);
        stack_drop(&ctx->K, dec_u(count)+1);
        yield_ctx(ts);
      } else {
        error_ctx(enc_sym(SYM_EERROR));
      }
      return;
    } else if (dfun == SYM_WAIT) {
      if (type_of(fun_args[1]) == VAL_TYPE_I) {
        CID cid = (CID)dec_u(fun_args[1]);
        stack_drop(&ctx->K, dec_u(count)+1);
        FOF(ctx->done, push_u32_2(&ctx->K, enc_u(cid), enc_u(WAIT)));
        ctx->r = enc_sym(SYM_TRUE);
        ctx->app_cont = true;
        yield_ctx(50000);
      } else {
        ERROR
        error_ctx(enc_sym(SYM_EERROR));
      }
      return;
    } else if (dfun == SYM_EVAL) {
      ctx->curr_exp = fun_args[1];
      stack_drop(&ctx->K, dec_u(count)+1);
      return;
    } else if (dfun == SYM_SEND) {
      VALUE status = enc_sym(SYM_EERROR);

      if (dec_u(count) == 2) {

        if (type_of(fun_args[1]) == VAL_TYPE_U) { /* CID is of U type */
          CID cid = (CID)dec_u(fun_args[1]);
          VALUE msg = fun_args[2];

          WITH_GC(status, find_receiver_and_send(cid, msg), NIL, NIL);
        }
      }
      /* return the status */
      stack_drop(&ctx->K, dec_u(count)+1);
      ctx->r = status;
      ctx->app_cont = true;
      return;
    }
    else if (is_fundamental(fun)) {
      /* If it is not a eval_cps specific function, it may be a fundamental operation */
      WITH_GC(res, fundamental_exec(&fun_args[1], dec_u(count), fun), NIL, NIL);
      if (type_of(res) == VAL_TYPE_SYMBOL &&
          dec_sym(res) == SYM_EERROR) {
        ERROR
          error_ctx(res);
      }  else {
        stack_drop(&ctx->K, dec_u(count)+1);
        ctx->app_cont = true;
        ctx->r = res;
      }
      return;
    }
  }

  // It may be an extension. Run GC first so that the extension has to worry less
  // about running out of memory.
  if (heap_size() - heap_num_allocated() < PRELIMINARY_GC_MEASURE) {
    gc(NIL, NIL);
  }

  extension_fptr f = extensions_lookup(dec_sym(fun));
  if (f == NULL) {
    ERROR
      error_ctx(enc_sym(SYM_EERROR));
    return;
  }

  VALUE ext_res;
  WITH_GC(ext_res, f(&fun_args[1] , dec_u(count)), NIL, NIL);

  stack_drop(&ctx->K, dec_u(count) + 1);

  ctx->app_cont = true;
  ctx->r = ext_res;
  return;
}

static inline void cont_application_args(eval_context_t *ctx) {
  VALUE count;
  VALUE env;
  VALUE rest;
  VALUE arg = ctx->r;
  pop_u32_3(&ctx->K, &rest, &count, &env);

  FOF(ctx->done, push_u32(&ctx->K, arg));
  /* Deal with general fundamentals */
  if (type_of(rest) == VAL_TYPE_SYMBOL &&
      rest == NIL) {
    // no arguments
    FOF(ctx->done, push_u32_2(&ctx->K, count, enc_u(APPLICATION)));
    ctx->app_cont = true;
  } else if (type_of(rest) == PTR_TYPE_CONS) {
    FOF(ctx->done, push_u32_4(&ctx->K, env, enc_u(dec_u(count) + 1), cdr(rest), enc_u(APPLICATION_ARGS)));
    ctx->curr_exp = car(rest);
    ctx->curr_env = env;
  } else {
    /* TODO: Should pop count elements from the stack here as this application is an error */
    ctx->curr_exp = enc_sym(SYM_EERROR);
    ctx->curr_env = env;
  }
  return;
}

static inline void cont_and(eval_context_t *ctx) {
  VALUE rest;
  VALUE arg = ctx->r;
  pop_u32(&ctx->K, &rest);
  if (type_of(arg) == VAL_TYPE_SYMBOL &&
      dec_sym(arg) == SYM_NIL) {
    ctx->app_cont = true;
    ctx->r = enc_sym(SYM_NIL);
  } else if (type_of(rest) == VAL_TYPE_SYMBOL &&
             rest == NIL) {
    ctx->app_cont = true;
  } else {
    FOF(ctx->done, push_u32_2(&ctx->K, cdr(rest), enc_u(AND)));
    ctx->curr_exp = car(rest);
  }
}

static inline void cont_or(eval_context_t *ctx) {
  VALUE rest;
  VALUE arg = ctx->r;
  pop_u32(&ctx->K, &rest);
  if (type_of(arg) != VAL_TYPE_SYMBOL ||
      dec_sym(arg) != SYM_NIL) {
    ctx->app_cont = true;
  } else  if (type_of(rest) == VAL_TYPE_SYMBOL &&
              rest == NIL) {
    ctx->app_cont = true;
    ctx->r = enc_sym(SYM_NIL);
  } else {
    FOF(ctx->done, push_u32_2(&ctx->K, cdr(rest), enc_u(OR)));
    ctx->curr_exp = car(rest);
  }
}

static inline void cont_bind_to_key_rest(eval_context_t *ctx) {
  VALUE key;
  VALUE env;
  VALUE rest;
  VALUE arg = ctx->r;
  pop_u32_3(&ctx->K, &key, &env, &rest);

  env_modify_binding(env, key, arg);

  if ( type_of(rest) == PTR_TYPE_CONS ){
    VALUE keyn = car(car(rest));
    VALUE valn_exp = car(cdr(car(rest)));

    FOF(ctx->done, push_u32_4(&ctx->K, cdr(rest), env, keyn, enc_u(BIND_TO_KEY_REST)));

    ctx->curr_exp = valn_exp;
    ctx->curr_env = env;
  } else {
    // Otherwise evaluate the expression in the populated env
    VALUE exp;
    pop_u32(&ctx->K, &exp);
    ctx->curr_exp = exp;
    ctx->curr_env = env;
  }
}

static inline void cont_if(eval_context_t *ctx) {
  VALUE then_branch;
  VALUE else_branch;
  VALUE arg = ctx->r;

  pop_u32_2(&ctx->K, &then_branch, &else_branch);

  if (type_of(arg) == VAL_TYPE_SYMBOL && dec_sym(arg) == SYM_TRUE) {
    ctx->curr_exp = then_branch;
  } else {
    ctx->curr_exp = else_branch;
  }
}

static inline void cont_match_many(eval_context_t *ctx) {

  VALUE r = ctx->r;

  VALUE rest_msgs;
  VALUE pats;
  VALUE exp;

  pop_u32_3(&ctx->K, &rest_msgs, &pats, &exp);

  if (type_of(r) == VAL_TYPE_SYMBOL &&
      (dec_sym(r) == SYM_NO_MATCH)) {

    if (type_of(rest_msgs) == VAL_TYPE_SYMBOL &&
        dec_sym(rest_msgs) == SYM_NIL) {

      ctx->curr_exp = exp;

    } else {
      /* try match the next one */
      FOF(ctx->done, push_u32_4(&ctx->K, exp, pats, cdr(rest_msgs), enc_u(MATCH_MANY)));
      FOF(ctx->done, push_u32_2(&ctx->K, car(pats), enc_u(MATCH)));
      ctx->r = car(rest_msgs);
      ctx->app_cont = true;
    }
  } else {
  /* I think the else branch will be "do nothing" here. */
  /* We should just continue executing with the result in ctx->r already*/
    ctx->app_cont = true;
  }
}

static inline void cont_match(eval_context_t *ctx) {
  VALUE e = ctx->r;
  VALUE patterns;
  VALUE new_env = ctx->curr_env;
  bool  do_gc = false;

  pop_u32(&ctx->K, &patterns);

  if (type_of(patterns) == VAL_TYPE_SYMBOL && dec_sym(patterns) == SYM_NIL) {
    /* no more patterns */
    ctx->r = enc_sym(SYM_NO_MATCH);
    ctx->app_cont = true;
  } else if (type_of(patterns) == PTR_TYPE_CONS) {
    VALUE pattern = car(car(patterns));
    VALUE body    = car(cdr(car(patterns)));

    if (match(pattern, e, &new_env, &do_gc)) {
      ctx->curr_env = new_env;
      ctx->curr_exp = body;
    } else if (do_gc) {
      gc(NIL,NIL);
      do_gc = false;
      match(pattern, e, &new_env, &do_gc);
      if (do_gc) {
        ctx_running->done = true;
        error_ctx(enc_sym(SYM_MERROR));
        return;
      }
      ctx->curr_env = new_env;
      ctx->curr_exp = body;
    } else {
      /* set up for checking of next pattern */
      FOF(ctx->done, push_u32_2(&ctx->K, cdr(patterns), enc_u(MATCH)));
      /* leave r unaltered */
      ctx->app_cont = true;
    }
  } else {
    /* TODO: return type error */
    ctx->r = enc_sym(SYM_TERROR);
    ctx->done = true;
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
    VALUE k;
    pop_u32(&ctx->K, &k);
    ctx->app_cont = false;

    switch(dec_u(k)) {
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
    default:
      ERROR
      error_ctx(enc_sym(SYM_EERROR));
      return;
    }
  }

  VALUE head;

  switch (type_of(ctx->curr_exp)) {

  case VAL_TYPE_SYMBOL: eval_symbol(ctx); return;
  case PTR_TYPE_BOXED_F: /* fall through */
  case PTR_TYPE_BOXED_U:
  case PTR_TYPE_BOXED_I:
  case VAL_TYPE_I:
  case VAL_TYPE_U:
  case VAL_TYPE_CHAR:
  case PTR_TYPE_ARRAY:
  case PTR_TYPE_REF:
  case PTR_TYPE_STREAM: eval_selfevaluating(ctx);  return;

  case PTR_TYPE_CONS:
    head = car(ctx->curr_exp);

    if (type_of(head) == VAL_TYPE_SYMBOL) {

      UINT sym_id = dec_sym(head);

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
    FOF(ctx->done, push_u32_4(&ctx->K,
                   ctx->curr_env,
                   enc_u(0),
                   cdr(ctx->curr_exp),
                   enc_u(APPLICATION_ARGS)));

    ctx->curr_exp = head; // evaluate the function
    return;
  default:
    // BUG No applicable case!
    ERROR
    error_ctx(enc_sym(SYM_EERROR));
    break;
  }
  return;
}

void eval_cps_pause_eval(void) {
  eval_cps_next_state = EVAL_CPS_STATE_PAUSED;
}

void eval_cps_step_eval(void) {
  eval_cps_next_state = EVAL_CPS_STATE_STEP;
}

void eval_cps_continue_eval(void) {
  eval_cps_next_state = EVAL_CPS_STATE_RUNNING;
}

void eval_cps_kill_eval(void) {
  eval_cps_next_state = EVAL_CPS_STATE_KILL;
}

uint32_t eval_cps_current_state(void) {
  return eval_cps_run_state;
}

/* eval_cps_run can be paused
   I think it would be better use a mailbox for
   communication between other threads and the run_eval
   but for now a set of variables will be used. */
void eval_cps_run_eval(void){

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

    if (heap_size() - heap_num_allocated() < PRELIMINARY_GC_MEASURE) {
      gc(NIL, NIL);
    }

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

VALUE evaluate_non_concurrent(void) {

  CID cid = ctx_running->id;

  while (ctx_running) {
    evaluation_step();
  }

  eval_context_t *ctx = lookup_ctx(&done, cid);
  if (ctx) {
    drop_ctx(&done, ctx);
  } else {
    return enc_sym(SYM_FATAL_ERROR);
  }

  return ctx_non_concurrent.r;
}

CID eval_cps_program(VALUE lisp) {
  return create_ctx(lisp, NIL, 256, false);
}

CID eval_cps_program_ext(VALUE lisp, unsigned int stack_size, bool grow_stack) {
  return create_ctx(lisp, NIL, stack_size, grow_stack);
}

VALUE eval_cps_program_nc(VALUE lisp) {

  if (type_of(lisp) != PTR_TYPE_CONS)
    return enc_sym(SYM_EERROR);
  ctx_non_concurrent.program = cdr(lisp);
  ctx_non_concurrent.curr_exp = car(lisp);
  ctx_non_concurrent.curr_env = NIL;
  ctx_non_concurrent.done = false;
  ctx_non_concurrent.app_cont = false;
  ctx_non_concurrent.timestamp = 0;
  ctx_non_concurrent.sleep_us = 0;
  ctx_non_concurrent.id = 0;

  stack_clear(&ctx_non_concurrent.K);

  if (!push_u32(&ctx_non_concurrent.K, enc_u(DONE)))
    return enc_sym(SYM_MERROR);

  ctx_running = &ctx_non_concurrent;

  return evaluate_non_concurrent();
}

int eval_cps_init_nc(unsigned int stack_size, bool grow_stack) {

  NIL = enc_sym(SYM_NIL);
  NONSENSE = enc_sym(SYM_NONSENSE);

  mutex_init(&qmutex);

  VALUE nil_entry = cons(NIL, NIL);
  *env_get_global_ptr() = cons(nil_entry, *env_get_global_ptr());

  if (type_of(nil_entry) == VAL_TYPE_SYMBOL ||
      type_of(*env_get_global_ptr()) == VAL_TYPE_SYMBOL)
    return 0;

  if (!stack_allocate(&ctx_non_concurrent.K, stack_size, grow_stack))
    return 0;

  return 1;
}

int eval_cps_init() {
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

  NIL = enc_sym(SYM_NIL);
  NONSENSE = enc_sym(SYM_NONSENSE);

  VALUE nil_entry = cons(NIL, NIL);
  *env_get_global_ptr() = cons(nil_entry, *env_get_global_ptr());

  if (type_of(nil_entry) == VAL_TYPE_SYMBOL ||
      type_of(*env_get_global_ptr()) == VAL_TYPE_SYMBOL) res = 0;

  eval_running = true;

  return res;
}

void eval_cps_del_nc(void) {
  stack_free(&ctx_non_concurrent.K);
}
