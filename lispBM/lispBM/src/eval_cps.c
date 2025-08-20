/*
    Copyright 2018, 2020 - 2025 Joel Svensson    svenssonjoel@yahoo.se
              2025 Rasmus Söderhielm rasmus.soderhielm@gmail.com

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
#include "tokpar.h"
#include "lbm_channel.h"
#include "print.h"
#include "platform_mutex.h"
#include "lbm_flat_value.h"

#include <setjmp.h>
#include <stdarg.h>
#include <stdnoreturn.h>

#if __STDC_VERSION__ < 201112L
// Lower than C11
#undef noreturn
#define noreturn __attribute__ ((__noreturn__))
#endif

static jmp_buf error_jmp_buf;
static jmp_buf critical_error_jmp_buf;

#define S_TO_US(X) (lbm_uint)((X) * 1000000)

#define DEC_CONTINUATION(x) (((x) & ~LBM_CONTINUATION_INTERNAL) >> LBM_ADDRESS_SHIFT)
#define IS_CONTINUATION(x) (((x) & LBM_CONTINUATION_INTERNAL) == LBM_CONTINUATION_INTERNAL)
#define CONTINUATION(x) (((x) << LBM_ADDRESS_SHIFT) | LBM_CONTINUATION_INTERNAL)

#define DONE                       CONTINUATION(0)
#define SET_GLOBAL_ENV             CONTINUATION(1)
#define BIND_TO_KEY_REST           CONTINUATION(2)
#define IF                         CONTINUATION(3)
#define PROGN_REST                 CONTINUATION(4)
#define APPLICATION_ARGS           CONTINUATION(5)
#define AND                        CONTINUATION(6)
#define OR                         CONTINUATION(7)
#define WAIT                       CONTINUATION(8)
#define MATCH                      CONTINUATION(9)
#define APPLICATION_START          CONTINUATION(10)
#define EVAL_R                     CONTINUATION(11)
#define RESUME                     CONTINUATION(12)
#define CLOSURE_ARGS               CONTINUATION(13)
#define EXIT_ATOMIC                CONTINUATION(14)
#define READ_NEXT_TOKEN            CONTINUATION(15)
#define READ_APPEND_CONTINUE       CONTINUATION(16)
#define READ_EVAL_CONTINUE         CONTINUATION(17)
#define READ_EXPECT_CLOSEPAR       CONTINUATION(18)
#define READ_DOT_TERMINATE         CONTINUATION(19)
#define READ_DONE                  CONTINUATION(20)
#define READ_START_BYTEARRAY       CONTINUATION(21)
#define READ_APPEND_BYTEARRAY      CONTINUATION(22)
#define MAP                        CONTINUATION(23)
#define MATCH_GUARD                CONTINUATION(24)
#define TERMINATE                  CONTINUATION(25)
#define PROGN_VAR                  CONTINUATION(26)
#define SETQ                       CONTINUATION(27)
#define MOVE_TO_FLASH              CONTINUATION(28)
#define MOVE_VAL_TO_FLASH_DISPATCH CONTINUATION(29)
#define MOVE_LIST_TO_FLASH         CONTINUATION(30)
#define CLOSE_LIST_IN_FLASH        CONTINUATION(31)
#define QQ_EXPAND_START            CONTINUATION(32)
#define QQ_EXPAND                  CONTINUATION(33)
#define QQ_APPEND                  CONTINUATION(34)
#define QQ_EXPAND_LIST             CONTINUATION(35)
#define QQ_LIST                    CONTINUATION(36)
#define KILL                       CONTINUATION(37)
#define LOOP                       CONTINUATION(38)
#define LOOP_CONDITION             CONTINUATION(39)
#define MERGE_REST                 CONTINUATION(40)
#define MERGE_LAYER                CONTINUATION(41)
#define CLOSURE_ARGS_REST          CONTINUATION(42)
#define MOVE_ARRAY_ELTS_TO_FLASH   CONTINUATION(43)
#define POP_READER_FLAGS           CONTINUATION(44)
#define EXCEPTION_HANDLER          CONTINUATION(45)
#define RECV_TO                    CONTINUATION(46)
#define WRAP_RESULT                CONTINUATION(47)
#define RECV_TO_RETRY              CONTINUATION(48)
#define READ_START_ARRAY           CONTINUATION(49)
#define READ_APPEND_ARRAY          CONTINUATION(50)
#define LOOP_ENV_PREP              CONTINUATION(51)
#define NUM_CONTINUATIONS          52

#define FM_NEED_GC       -1
#define FM_NO_MATCH      -2
#define FM_PATTERN_ERROR -3

typedef enum {
  BL_OK = 0,
  BL_NO_MEMORY,
  BL_INCORRECT_KEY
} binding_location_status;

#define FB_OK             0
#define FB_TYPE_ERROR    -1

#ifdef LBM_USE_ERROR_LINENO
#define ERROR_AT_CTX(err_val, at) error_at_ctx(err_val, at, __LINE__)
#define ERROR_CTX(err_val) error_ctx(err_val, __LINE__)
#define READ_ERROR_CTX(row, col) read_error_ctx(row, col, __LINE__)
#else
#define ERROR_AT_CTX(err_val, at) error_at_ctx(err_val, at)
#define ERROR_CTX(err_val) error_ctx(err_val)
#define READ_ERROR_CTX(row, col) read_error_ctx(row, col)
#endif

// ////////////////////////////////////////////////////////////
// Local variables used in sort and merge
lbm_value symbol_x = ENC_SYM_NIL;
lbm_value symbol_y = ENC_SYM_NIL;
#ifdef CLEAN_UP_CLOSURES
static lbm_value clean_cl_env_symbol = ENC_SYM_NIL;
#endif

// ////////////////////////////////////////////////////////////
// Error strings
const char* lbm_error_str_parse_eof = "End of parse stream.";
const char* lbm_error_str_parse_dot = "Incorrect usage of '.'.";
const char* lbm_error_str_parse_close = "Expected closing parenthesis.";
const char* lbm_error_str_num_args = "Incorrect number of arguments.";
const char* lbm_error_str_forbidden_in_atomic = "Operation is forbidden in an atomic block.";
const char* lbm_error_str_no_number = "Argument(s) must be a number.";
const char* lbm_error_str_not_a_boolean = "Argument must be t or nil (true or false).";
const char* lbm_error_str_incorrect_arg = "Incorrect argument.";
const char* lbm_error_str_var_outside_progn = "Usage of var outside of progn.";
const char* lbm_error_str_flash_not_possible = "Value cannot be written to flash.";
const char* lbm_error_str_flash_error = "Error writing to flash.";
const char* lbm_error_str_flash_full = "Flash memory is full.";
const char* lbm_error_str_variable_not_bound = "Variable not bound.";
const char* lbm_error_str_read_no_mem = "Out of memory while reading.";
const char* lbm_error_str_qq_expand = "Quasiquotation expansion error.";
const char* lbm_error_str_not_applicable = "Value is not applicable.";
const char* lbm_error_str_built_in = "Cannot redefine built-in.";

static lbm_value lbm_error_suspect;
static bool lbm_error_has_suspect = false;


// ////////////////////////////////////////////////////////////
// Prototypes for locally used functions (static)
static uint32_t lbm_mailbox_free_space_for_cid(lbm_cid cid);
static void apply_apply(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
static int gc(void);
#ifdef LBM_USE_ERROR_LINENO
static void error_ctx(lbm_value, int line_no);
static void error_at_ctx(lbm_value err_val, lbm_value at, int line_no);
#else
static void error_ctx(lbm_value);
static void error_at_ctx(lbm_value err_val, lbm_value at);
#endif
static void mailbox_add_mail(eval_context_t *ctx, lbm_value mail);

// TODO: Optimize, In a large number of cases
// where WITH_GC is used, it is not really required to check is_symbol_merror.
// Just checking is_symbol should be enough.
// Given the number of calls to WITH_GC this could save some code
// space and potentially also be a slight speedup.
// TODO: profile.
#ifdef LBM_ALWAYS_GC
#define WITH_GC(y, x)                           \
  gc();                                         \
  (y) = (x);                                    \
  if (lbm_is_symbol_merror((y))) {              \
    ERROR_CTX(ENC_SYM_MERROR);                  \
  }

#define WITH_GC_RMBR_1(y, x, r)                 \
  lbm_gc_mark_phase(r);                         \
  gc();                                         \
  (y) = (x);                                    \
  if (lbm_is_symbol_merror((y))) {              \
    ERROR_CTX(ENC_SYM_MERROR);                  \
  }

#else

#define WITH_GC(y, x)                           \
  (y) = (x);                                    \
  if (lbm_is_symbol_merror((y))) {              \
    gc();                                       \
    (y) = (x);                                  \
    if (lbm_is_symbol_merror((y))) {            \
      ERROR_CTX(ENC_SYM_MERROR);                \
    }                                           \
    /* continue executing statements below */   \
  }
#define WITH_GC_RMBR_1(y, x, r)                 \
  (y) = (x);                                    \
  if (lbm_is_symbol_merror((y))) {              \
    lbm_gc_mark_phase(r);                       \
    gc();                                       \
    (y) = (x);                                  \
    if (lbm_is_symbol_merror((y))) {            \
      ERROR_CTX(ENC_SYM_MERROR);                \
    }                                           \
    /* continue executing statements below */   \
  }
#endif

// ////////////////////////////////////////////////////////////
// Context queues
typedef struct {
  eval_context_t *first;
  eval_context_t *last;
} eval_context_queue_t;

static eval_context_queue_t blocked  = {NULL, NULL};
static eval_context_queue_t queue    = {NULL, NULL};

mutex_t qmutex;
bool    qmutex_initialized = false;

static void enqueue_ctx(eval_context_queue_t *q, eval_context_t *ctx);

// The currently executing context.
eval_context_t *ctx_running = NULL;
volatile bool  lbm_system_sleeping = false;

static volatile bool gc_requested = false;
void lbm_request_gc(void) {
  gc_requested = true;
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

#define EVAL_CPS_DEFAULT_STACK_SIZE 256
#define EVAL_TIME_QUOTA 400 // time in used, if time quota
#define EVAL_CPS_MIN_SLEEP 200
#define EVAL_STEPS_QUOTA   10

#ifdef LBM_USE_TIME_QUOTA
static volatile uint32_t eval_time_refill = EVAL_TIME_QUOTA;
static uint32_t eval_time_quota = EVAL_TIME_QUOTA;
static uint32_t eval_current_quota = 0;
void lbm_set_eval_time_quota(uint32_t quota) {
  eval_time_refill = quota;
}
#else
static volatile uint32_t eval_steps_refill = EVAL_STEPS_QUOTA;
static uint32_t eval_steps_quota = EVAL_STEPS_QUOTA;
void lbm_set_eval_step_quota(uint32_t quota) {
  eval_steps_refill = quota;
}
#endif

static uint32_t          eval_cps_run_state = EVAL_CPS_STATE_DEAD;
static volatile uint32_t eval_cps_next_state = EVAL_CPS_STATE_NONE;
static volatile uint32_t eval_cps_next_state_arg = 0;
static volatile bool     eval_cps_state_changed = false;

static void usleep_nonsense(uint32_t us) {
  (void) us;
}

static bool dynamic_load_nonsense(const char *sym, const char **code) {
  (void) sym;
  (void) code;
  return false;
}

static uint32_t timestamp_nonsense(void) {
  return 0;
}

static int printf_nonsense(const char *fmt, ...) {
  (void) fmt;
  return 0;
}

static void ctx_done_nonsense(eval_context_t *ctx) {
  (void) ctx;
}

static void critical_nonsense(void) {
  return;
}

static void (*critical_error_callback)(void) = critical_nonsense;
static void (*usleep_callback)(uint32_t) = usleep_nonsense;
static uint32_t (*timestamp_us_callback)(void) = timestamp_nonsense;
static void (*ctx_done_callback)(eval_context_t *) = ctx_done_nonsense;
int (*lbm_printf_callback)(const char *, ...) = printf_nonsense;
static bool (*dynamic_load_callback)(const char *, const char **) = dynamic_load_nonsense;

void lbm_set_critical_error_callback(void (*fptr)(void)) {
  if (fptr == NULL) critical_error_callback = critical_nonsense;
  else critical_error_callback = fptr;
}

void lbm_set_usleep_callback(void (*fptr)(uint32_t)) {
  if (fptr == NULL) usleep_callback = usleep_nonsense;
  else usleep_callback = fptr;
}

void lbm_set_timestamp_us_callback(uint32_t (*fptr)(void)) {
  if (fptr == NULL) timestamp_us_callback = timestamp_nonsense;
  else timestamp_us_callback = fptr;
}

void lbm_set_ctx_done_callback(void (*fptr)(eval_context_t *)) {
  if (fptr == NULL) ctx_done_callback = ctx_done_nonsense;
  else ctx_done_callback = fptr;
}

void lbm_set_printf_callback(int (*fptr)(const char*, ...)){
  if (fptr == NULL) lbm_printf_callback = printf_nonsense;
  else lbm_printf_callback = fptr;
}

void lbm_set_dynamic_load_callback(bool (*fptr)(const char *, const char **)) {
  if (fptr == NULL) dynamic_load_callback = dynamic_load_nonsense;
  else  dynamic_load_callback = fptr;
}

static volatile lbm_event_t *lbm_events = NULL;
static unsigned int lbm_events_head = 0;
static unsigned int lbm_events_tail = 0;
static unsigned int lbm_events_max  = 0;
static bool         lbm_events_full = false;
static mutex_t      lbm_events_mutex;
static bool         lbm_events_mutex_initialized = false;
static volatile lbm_cid  lbm_event_handler_pid = -1;

static unsigned int lbm_event_queue_item_count(void) {
  unsigned int res = lbm_events_max;
  if (!lbm_events_full) {
    if (lbm_events_head >= lbm_events_tail) {
      res = lbm_events_head - lbm_events_tail;
    } else {
      res = lbm_events_max - lbm_events_tail + lbm_events_head;
    }
  }
  return res;
}

lbm_cid lbm_get_event_handler_pid(void) {
  return lbm_event_handler_pid;
}

void lbm_set_event_handler_pid(lbm_cid pid) {
  lbm_event_handler_pid = pid;
}

bool lbm_event_handler_exists(void) {
  return(lbm_event_handler_pid > 0);
}

static bool event_internal(lbm_event_type_t event_type, lbm_uint parameter, lbm_uint buf_ptr, lbm_uint buf_len) {
  bool r = false;
  if (lbm_events) {
    mutex_lock(&lbm_events_mutex);
    if (!lbm_events_full) {
      lbm_event_t event;
      event.type = event_type;
      event.parameter = parameter;
      event.buf_ptr = buf_ptr;
      event.buf_len = buf_len;
      lbm_events[lbm_events_head] = event;
      lbm_events_head = (lbm_events_head + 1) % lbm_events_max;
      lbm_events_full = lbm_events_head == lbm_events_tail;
      r = true;
    }
    mutex_unlock(&lbm_events_mutex);
  }
  return r;
}

bool lbm_event_define(lbm_value key, lbm_flat_value_t *fv) {
  return event_internal(LBM_EVENT_DEFINE, key, (lbm_uint)fv->buf, fv->buf_size);
}

bool lbm_event_unboxed(lbm_value unboxed) {
  lbm_uint t = lbm_type_of(unboxed);
  if (t == LBM_TYPE_SYMBOL ||
      t == LBM_TYPE_I ||
      t == LBM_TYPE_U ||
      t == LBM_TYPE_CHAR) {
    if (lbm_event_handler_pid > 0) {
      if (lbm_mailbox_free_space_for_cid(lbm_event_handler_pid) <= lbm_event_queue_item_count()) {
        return false;
      }
      return event_internal(LBM_EVENT_FOR_HANDLER, 0, (lbm_uint)unboxed, 0);
    }
  }
  return false;
}

bool lbm_event(lbm_flat_value_t *fv) {
  if (lbm_event_handler_pid > 0) {
    if (lbm_mailbox_free_space_for_cid(lbm_event_handler_pid) <= lbm_event_queue_item_count()) {
      return false;
    }
    return event_internal(LBM_EVENT_FOR_HANDLER, 0, (lbm_uint)fv->buf, fv->buf_size);
  }
  return false;
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

bool lbm_event_queue_is_empty(void) {
  mutex_lock(&lbm_events_mutex);
  bool empty = false;
  if (lbm_events_head == lbm_events_tail && !lbm_events_full) {
    empty = true;
  }
  mutex_unlock(&lbm_events_mutex);
  return empty;
}

static bool              eval_running = false;
static volatile bool     blocking_extension = false;
static mutex_t           blocking_extension_mutex;
static bool              blocking_extension_mutex_initialized = false;
static lbm_uint          blocking_extension_timeout_us = 0;
static bool              blocking_extension_timeout = false;

static bool              is_atomic = false;

// MODES
static volatile bool lbm_verbose = false;
static volatile bool lbm_hide_trapped_error = false;

void lbm_toggle_verbose(void) {
  lbm_verbose = !lbm_verbose;
}

void lbm_set_verbose(bool verbose) {
  lbm_verbose = verbose;
}

void lbm_set_hide_trapped_error(bool hide) {
  lbm_hide_trapped_error = hide;
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

#ifdef LBM_USE_TIME_QUOTA
void lbm_surrender_quota(void) {
  // dummy;
}
#else
void lbm_surrender_quota(void) {
  eval_steps_quota = 0;
}
#endif

/****************************************************/
/* Utilities used locally in this file              */

static inline lbm_array_header_t *assume_array(lbm_value a){
  return (lbm_array_header_t*)lbm_ref_cell(a)->car;
}

static lbm_value cons_with_gc(lbm_value head, lbm_value tail, lbm_value remember) {
#ifdef LBM_ALWAYS_GC
  lbm_value always_gc_roots[3] = {head, tail, remember};
  lbm_gc_mark_roots(always_gc_roots,3);
  gc();
#endif
  lbm_value res = lbm_heap_state.freelist;
  if (lbm_is_symbol_nil(res)) {
    lbm_value roots[3] = {head, tail, remember};
    lbm_gc_mark_roots(roots,3);
    gc();
    res = lbm_heap_state.freelist;
    if (lbm_is_symbol_nil(res)) {
      ERROR_CTX(ENC_SYM_MERROR);
    }
  }
  lbm_uint heap_ix = lbm_dec_ptr(res);
  lbm_heap_state.freelist = lbm_heap_state.heap[heap_ix].cdr;
  lbm_heap_state.num_alloc++;
  lbm_heap_state.heap[heap_ix].car = head;
  lbm_heap_state.heap[heap_ix].cdr = tail;
  res = lbm_set_ptr_type(res, LBM_TYPE_CONS);
  return res;
}

// ////////////////////////////////////////////////////////////
// Stack and CPS

// The CPS system is dependent on a diciplined use of a continuation
// stack! Continuations are pushed and popped from this stack and
// between the point in time when a continuation is pushed and when it is popped
// significant stack "traffic" may have occurred.
//
// In the evaluator, functions called things line "eval_X" usually sets up
// a stack-frame for a future exection of a "cont_X" continuation.
// Some kind of proof is needed that Things that pushed in a continuation setup-phase
// are all popped (no more, no less) in a continuation execution phase.
// continuations "cont_X" also may set up more continuations and in that
// case they may modify the stack in place or reuse in part or full the
// stack-frame associated with "cont_X"
//
// TODO: for each "eval_X" and "cont_X" pair write an argument or some kind
// of test that illustrates correct usage of the stack discipline.
//
// For now the argument of stack discipline being honored is that all tests
// pass and no tests end up in stack underflow.
//
// Stack overflow is easy to end up in, just write a non-tail-recursive but recursive funtions
// and run it for a suitably large input.
//
// * Potentially tricky situation is "trap" used on a call to a non-tail-recursive function that
// ends out exhausting stack.
// # Trap rewinds the stack to the point of trap so it should be perfectly safe to trap even in this situation.

// get_stack_ptr and pop_stack_ptr does in no tests reach
// the error condition. The check n <= sp is not really needed.
static inline lbm_uint *get_stack_ptr(eval_context_t *ctx, unsigned int n) {
  lbm_uint index = ctx->K.sp - n;
  return &ctx->K.data[index];
}

// pop_stack_ptr is safe when no GC is performed and
// the values of the stack will be dropped.
static inline lbm_uint *pop_stack_ptr(eval_context_t *ctx, unsigned int n) {
  ctx->K.sp -= n;
  return &ctx->K.data[ctx->K.sp];
}

static inline lbm_uint *stack_reserve(eval_context_t *ctx, unsigned int n) {
  if (ctx->K.sp + n < ctx->K.size) {
    lbm_uint *ptr = &ctx->K.data[ctx->K.sp];
    ctx->K.sp += n;
    return ptr;
  }
  ERROR_CTX(ENC_SYM_STACK_ERROR);
}

static void handle_flash_status(lbm_flash_status s) {
  if ( s == LBM_FLASH_FULL) {
    lbm_set_error_reason((char*)lbm_error_str_flash_full);
    ERROR_CTX(ENC_SYM_EERROR);
  }
  if (s == LBM_FLASH_WRITE_ERROR) {
    lbm_set_error_reason((char*)lbm_error_str_flash_error);
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  }
}

static void lift_array_flash(lbm_value flash_cell, bool bytearray,  char *data, lbm_uint num_elt) {

  lbm_array_header_t flash_array_header;
  flash_array_header.size = num_elt;
  flash_array_header.data = (lbm_uint*)data;
  lbm_uint flash_array_header_ptr = 0;
  handle_flash_status(lbm_write_const_raw((lbm_uint*)&flash_array_header,
                                          sizeof(lbm_array_header_t) / sizeof(lbm_uint),
                                          &flash_array_header_ptr));
  handle_flash_status(write_const_car(flash_cell, flash_array_header_ptr));
  lbm_uint t = bytearray ? ENC_SYM_ARRAY_TYPE : ENC_SYM_LISPARRAY_TYPE;
  handle_flash_status(write_const_cdr(flash_cell, t));
}

// ////////////////////////////////////////////////////////////
// get_car and lbm_car

// lbm_car is a lower level operation that extracts a car field from a cons cell
// without any consideration of any additional type-tags associated with the cell.
// get_car is for list cons-cells only.

static inline void get_car_and_cdr(lbm_value a, lbm_value *a_car, lbm_value *a_cdr) {
  if (lbm_is_cons(a)) {
    lbm_cons_t *cell = lbm_ref_cell(a);
    *a_car = cell->car;
    *a_cdr = cell->cdr;
  } else if (lbm_is_symbol_nil(a)) {
    *a_car = *a_cdr = ENC_SYM_NIL;
  } else {
    ERROR_CTX(ENC_SYM_TERROR);
  }
}

/* car cdr caar cadr replacements that are evaluator safe. */
static inline lbm_value get_car(lbm_value a) {
  if (lbm_is_cons(a)) {
    lbm_cons_t *cell = lbm_ref_cell(a);
    return cell->car;
  } else if (lbm_is_symbol_nil(a)) {
    return a;
  }
  ERROR_CTX(ENC_SYM_TERROR);
}

static inline lbm_value get_cdr(lbm_value a) {
  if (lbm_is_cons(a)) {
    lbm_cons_t *cell = lbm_ref_cell(a);
    return cell->cdr;
  } else if (lbm_is_symbol_nil(a)) {
    return a;
  }
  ERROR_CTX(ENC_SYM_TERROR);
}

static inline lbm_value get_cadr(lbm_value a) {
  if (lbm_is_cons(a)) {
    lbm_cons_t *cell = lbm_ref_cell(a);
    lbm_value tmp = cell->cdr;
    if (lbm_is_cons(tmp)) {
      return lbm_ref_cell(tmp)->car;
    } else if (lbm_is_symbol_nil(tmp)) {
      return tmp;
    }
  } else if (lbm_is_symbol_nil(a)) {
    return a;
  }
  ERROR_CTX(ENC_SYM_TERROR);
}

// Allocate a binding and attach it to a list (if so desired)
static lbm_value allocate_binding(lbm_value key, lbm_value val, lbm_value the_cdr) {
#ifdef LBM_ALWAYS_GC
  lbm_gc_mark_phase(key);
  lbm_gc_mark_phase(val);
  lbm_gc_mark_phase(the_cdr);
  gc();
  if (lbm_heap_num_free() < 2) {
    ERROR_CTX(ENC_SYM_MERROR);
  }
#else
  if (lbm_heap_num_free() < 2) {
    lbm_gc_mark_phase(key);
    lbm_gc_mark_phase(val);
    lbm_gc_mark_phase(the_cdr);
    gc();
    if (lbm_heap_num_free() < 2) {
      ERROR_CTX(ENC_SYM_MERROR);
    }
  }
#endif
  // If num_free is calculated correctly, freelist is definitely a cons-cell.
  lbm_cons_t* heap = lbm_heap_state.heap;
  lbm_value binding_cell = lbm_heap_state.freelist;
  lbm_uint binding_cell_ix = lbm_dec_ptr(binding_cell);
  lbm_value list_cell = heap[binding_cell_ix].cdr;
  lbm_uint list_cell_ix = lbm_dec_ptr(list_cell);
  lbm_heap_state.freelist = heap[list_cell_ix].cdr;
  lbm_heap_state.num_alloc += 2;
  heap[binding_cell_ix].car = key;
  heap[binding_cell_ix].cdr = val;
  heap[list_cell_ix].car = binding_cell;
  heap[list_cell_ix].cdr = the_cdr;
  return list_cell;
}

#define CLO_PARAMS 0
#define CLO_BODY   1
#define CLO_ENV    2
#define LOOP_BINDS 0
#define LOOP_COND  1
#define LOOP_BODY  2

// TODO: extract_n could be a good place to do some error checking.
//       extract_n is often used to extract components of a list that
//       makes up a special form application. If there are not n items
//       present that could be an indication of a syntax error in the
//       special form application.
// (a b c) -> [a b c]
static lbm_value extract_n(lbm_value curr, lbm_value *res, unsigned int n) {
  for (unsigned int i = 0; i < n; i ++) {
    if (lbm_is_ptr(curr)) {
      lbm_cons_t *cell = lbm_ref_cell(curr);
      res[i] = cell->car;
      curr = cell->cdr;
    } else {
      res[i] = ENC_SYM_NIL;
    }
  }
  return curr; // Rest of list is returned here.
}

static void call_fundamental(lbm_uint fundamental, lbm_value *args, lbm_uint arg_count, eval_context_t *ctx) {
  lbm_value res;
#ifdef LBM_ALWAYS_GC
  gc();
#endif
  res = fundamental_table[fundamental](args, arg_count, ctx);
  if (lbm_is_error(res)) {
    if (lbm_is_symbol_merror(res)) {
      gc();
      res = fundamental_table[fundamental](args, arg_count, ctx);
    }
    if (lbm_is_error(res)) {
      ERROR_AT_CTX(res, lbm_enc_sym(FUNDAMENTAL_SYMBOLS_START | fundamental));
    }
  }
  lbm_stack_drop(&ctx->K, arg_count+1);
  ctx->app_cont = true;
  ctx->r = res;
}

static void atomic_error(void) {
  is_atomic = false;
  lbm_set_error_reason((char*)lbm_error_str_forbidden_in_atomic);
  ERROR_CTX(ENC_SYM_EERROR);
}

// block_current_ctx blocks a context until it is
// woken up externally or a timeout period of time passes.
// Blocking while in an atomic block would have bad consequences.
static void block_current_ctx(uint32_t state, lbm_uint sleep_us,  bool do_cont) {
  if (is_atomic) atomic_error();
  ctx_running->timestamp = timestamp_us_callback();
  ctx_running->sleep_us = sleep_us;
  ctx_running->state  = state;
  ctx_running->app_cont = do_cont;
  enqueue_ctx(&blocked, ctx_running);
  ctx_running = NULL;
}

// reblock an essentially already blocked context.
// Same as block but sets no new timestamp or sleep_us.
static void reblock_current_ctx(uint32_t state, bool do_cont) {
  if (is_atomic) atomic_error();
  ctx_running->state  = state;
  ctx_running->app_cont = do_cont;
  enqueue_ctx(&blocked, ctx_running);
  ctx_running = NULL;
}


lbm_flash_status lbm_write_const_array_padded(uint8_t *data, lbm_uint n, lbm_uint *res) {
  lbm_uint full_words = n / sizeof(lbm_uint);
  lbm_uint n_mod = n % sizeof(lbm_uint);

  if (n_mod == 0) { // perfect fit.
    return lbm_write_const_raw((lbm_uint*)data, full_words, res);
  } else {
    lbm_uint last_word = 0;
    memcpy(&last_word, &data[full_words * sizeof(lbm_uint)], n_mod);
    if (full_words >= 1) {
      lbm_flash_status s = lbm_write_const_raw((lbm_uint*)data, full_words, res);
      if ( s == LBM_FLASH_WRITE_OK) {
        lbm_uint dummy;
        s = lbm_write_const_raw(&last_word, 1, &dummy);
      }
      return s;
    } else {
      return lbm_write_const_raw(&last_word, 1, res);
    }
  }
}

/****************************************************/
/* Error message creation                           */

#define ERROR_MESSAGE_BUFFER_SIZE_BYTES 256

void print_environments(char *buf, unsigned int size) {

  lbm_value curr_l = ctx_running->curr_env;
  lbm_printf_callback("\tCurrent local environment:\n");
  while (lbm_type_of(curr_l) == LBM_TYPE_CONS) {
    lbm_print_value(buf, (size/2) - 1, lbm_caar(curr_l));
    lbm_print_value(buf + (size/2),size/2, lbm_cdr(lbm_car(curr_l)));
    lbm_printf_callback("\t%s = %s\n", buf, buf+(size/2));
    curr_l = lbm_cdr(curr_l);
  }
  lbm_printf_callback("\n\n");
  lbm_printf_callback("\tCurrent global environment:\n");
  lbm_value *glob_env = lbm_get_global_env();

  for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
    lbm_value curr_g = glob_env[i];;
    while (lbm_type_of(curr_g) == LBM_TYPE_CONS) {

      lbm_print_value(buf, (size/2) - 1, lbm_caar(curr_g));
      lbm_print_value(buf + (size/2),size/2, lbm_cdr(lbm_car(curr_g)));
      lbm_printf_callback("\t%s = %s\n", buf, buf+(size/2));
      curr_g = lbm_cdr(curr_g);
    }
  }
}

void print_error_value(char *buf, uint32_t bufsize, char *pre, lbm_value v, bool lookup) {

  lbm_print_value(buf, bufsize, v);
  lbm_printf_callback("%s %s\n",pre, buf);
  if (lookup) {
    if (lbm_is_symbol(v)) {
      if (lbm_dec_sym(v) >= RUNTIME_SYMBOLS_START) {
        lbm_value res = ENC_SYM_NIL;
        if (lbm_env_lookup_b(&res, v, ctx_running->curr_env) ||
            lbm_global_env_lookup(&res, v)) {
          lbm_print_value(buf, bufsize, res);
          lbm_printf_callback("      bound to: %s\n", buf);
        } else {
          lbm_printf_callback("      UNDEFINED\n");
        }
      }
    }
  }
}

static void print_error_message(lbm_value error,
                                bool has_at,
                                lbm_value at,
                                unsigned int row,
                                unsigned int col,
                                lbm_int row0,
                                lbm_int row1,
                                lbm_int cid,
                                char *name,
                                bool trapped) {
  /* try to allocate a lbm_print_value buffer on the lbm_memory */
  char *buf = lbm_malloc_reserve(ERROR_MESSAGE_BUFFER_SIZE_BYTES);
  if (!buf) {
    lbm_printf_callback("Error: Not enough memory to show a human readable error message\n");
    return;
  }
  if (trapped) {
    print_error_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES,"   Error (trapped):", error, false);
  } else {
    print_error_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES,"   Error:", error, false);
  }
  if (lbm_is_symbol_merror(error)) {
    lbm_printf_callback("\n   Heap cells free:  %d\n", lbm_heap_state.heap_size - lbm_heap_state.num_alloc);
    lbm_printf_callback("   Mem longest free: %d\n\n", lbm_memory_longest_free());
  }
  if (name) {
    lbm_printf_callback(  "   CTX: %d \"%s\"\n", cid, name);
  } else {
    lbm_printf_callback(  "   CTX: %d\n", cid);
  }
  print_error_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES,"   Current:", ctx_running->curr_exp, true);
  // An error can have both a set suspect that can be more detailed than the "at"
  // show both if present!
  if (lbm_error_has_suspect) {
      print_error_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES,"   At:", lbm_error_suspect, true);
      lbm_error_has_suspect = false;
  }
  // TODO: Should perhaps be called has_in and be meant to capture a bit
  // of the surrounding of where the error happened.
  if (has_at) {
    print_error_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES,"   In:", at, true);
  }

  lbm_printf_callback("\n");

  if (lbm_is_symbol(error) &&
      error == ENC_SYM_RERROR) {
    lbm_printf_callback("   Line:   %u\n", row);
    lbm_printf_callback("   Column: %u\n", col);
  } else if (row0 >= 0) {
    if (row1 < 0) lbm_printf_callback("   Starting at row: %d\n", row0);
    else lbm_printf_callback("   Between row %d and %d\n", row0, row1);
  }

  lbm_printf_callback("\n");

  if (ctx_running->error_reason) {
    lbm_printf_callback("   Reason: %s\n\n", ctx_running->error_reason);
  }
  if (lbm_verbose) {
    lbm_print_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES, ctx_running->r);
    lbm_printf_callback("   Current intermediate result: %s\n\n", buf);

    print_environments(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES);

    lbm_printf_callback("\n   Mailbox:\n");
    for (unsigned int i = 0; i < ctx_running->num_mail; i ++) {
      lbm_print_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES, ctx_running->mailbox[i]);
      lbm_printf_callback("     %s\n", buf);
    }
    lbm_printf_callback("\n   Stack:\n");
    for (unsigned int i = 0; i < ctx_running->K.sp; i ++) {
      lbm_print_value(buf, ERROR_MESSAGE_BUFFER_SIZE_BYTES, ctx_running->K.data[i]);
      lbm_printf_callback("     %s\n", buf);
    }
  }
  lbm_free(buf);
}

/****************************************************/
/* Tokenizing and parsing                           */

static bool create_string_channel(char *str, lbm_value *res, lbm_value dep) {

  lbm_char_channel_t *chan = NULL;
  lbm_string_channel_state_t *st = NULL;

  st = (lbm_string_channel_state_t*)lbm_malloc(sizeof(lbm_string_channel_state_t));
  if (st == NULL) {
    return false;
  }
  chan = (lbm_char_channel_t*)lbm_malloc(sizeof(lbm_char_channel_t));
  if (chan == NULL) {
    lbm_free(st);
    return false;
  }

  lbm_create_string_char_channel(st, chan, str);
  lbm_value cell = lbm_heap_allocate_cell(LBM_TYPE_CHANNEL, (lbm_uint) chan, ENC_SYM_CHANNEL_TYPE);
  if (cell == ENC_SYM_MERROR) {
    lbm_free(st);
    lbm_free(chan);
    return false;
  }

  lbm_char_channel_set_dependency(chan, dep);

  *res = cell;
  return true;
}

/****************************************************/
/* Queue functions                                  */

static void queue_iterator_nm(eval_context_queue_t *q, ctx_fun f, void *arg1, void *arg2) {
  eval_context_t *curr;
  curr = q->first;

  while (curr != NULL) {
    f(curr, arg1, arg2);
    curr = curr->next;
  }
}

void lbm_all_ctxs_iterator(ctx_fun f, void *arg1, void *arg2) {
  mutex_lock(&qmutex);
  queue_iterator_nm(&blocked, f, arg1, arg2);
  queue_iterator_nm(&queue, f, arg1, arg2);
  if (ctx_running) f(ctx_running, arg1, arg2);
  mutex_unlock(&qmutex);
}

void lbm_running_iterator(ctx_fun f, void *arg1, void *arg2){
  mutex_lock(&qmutex);
  queue_iterator_nm(&queue, f, arg1, arg2);
  mutex_unlock(&qmutex);
}

void lbm_blocked_iterator(ctx_fun f, void *arg1, void *arg2){
  mutex_lock(&qmutex);
  queue_iterator_nm(&blocked, f, arg1, arg2);
  mutex_unlock(&qmutex);
}

static void enqueue_ctx_nm(eval_context_queue_t *q, eval_context_t *ctx) {
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
}

static void enqueue_ctx(eval_context_queue_t *q, eval_context_t *ctx) {
  mutex_lock(&qmutex);
  enqueue_ctx_nm(q,ctx);
  mutex_unlock(&qmutex);
}

static eval_context_t *lookup_ctx_nm(eval_context_queue_t *q, lbm_cid cid) {
  eval_context_t *curr;
  curr = q->first;
  while (curr != NULL) {
    if (curr->id == cid) {
      return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

static bool drop_ctx_nm(eval_context_queue_t *q, eval_context_t *ctx) {

  bool res = false;
  if (q->first == NULL || q->last == NULL) {
    if (!(q->last == NULL && q->first == NULL)) {
      /* error state that should not happen */
      return res;
    }
    /* Queue is empty */
    return res;
  }

  eval_context_t *curr = q->first;
  while (curr) {
    if (curr->id == ctx->id) {
      res = true;
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
  return res;
}

/* End execution of the running context. */
static void finish_ctx(void) {

  if (!ctx_running) {
    return;
  }
  if (ctx_running->id == lbm_event_handler_pid) {
    lbm_event_handler_pid = -1;
  }
  /* Drop the continuation stack immediately to free up lbm_memory */
  lbm_stack_free(&ctx_running->K);
  ctx_done_callback(ctx_running);

  lbm_free(ctx_running->name); //free name if in LBM_MEM

  lbm_memory_free((lbm_uint*)ctx_running->error_reason); //free error_reason if in LBM_MEM

  lbm_memory_free((lbm_uint*)ctx_running->mailbox);
  lbm_memory_free((lbm_uint*)ctx_running);
  ctx_running = NULL;
}

static void context_exists(eval_context_t *ctx, void *cid, void *b) {
  if (ctx->id == *(lbm_cid*)cid) {
    *(bool*)b = true;
  }
}

void lbm_set_error_suspect(lbm_value suspect) {
  lbm_error_suspect = suspect;
  lbm_error_has_suspect = true;
}

void lbm_set_error_reason(const char *error_str) {
  if (ctx_running != NULL) {
    ctx_running->error_reason = error_str;
  }
}

// Not possible to CONS_WITH_GC in error_ctx_base (potential loop)
#ifdef LBM_USE_ERROR_LINENO
static noreturn void error_ctx_base(lbm_value err_val, bool has_at, lbm_value at, unsigned int row, unsigned int column, int line_no) {
#else
static noreturn void error_ctx_base(lbm_value err_val, bool has_at, lbm_value at, unsigned int row, unsigned int column) {
#endif
  bool print_trapped = !lbm_hide_trapped_error && (ctx_running->flags & EVAL_CPS_CONTEXT_FLAG_TRAP_UNROLL_RETURN);

  if (!(lbm_hide_trapped_error &&
        (ctx_running->flags & EVAL_CPS_CONTEXT_FLAG_TRAP_UNROLL_RETURN))) {
    print_error_message(err_val,
                        has_at,
                        at,
                        row,
                        column,
                        ctx_running->row0,
                        ctx_running->row1,
                        ctx_running->id,
                        ctx_running->name,
                        print_trapped
                        );
  }
#ifdef LBM_USE_ERROR_LINENO
  if (!lbm_hide_trapped_error) {
    lbm_printf_callback("eval_cps.c line number: %d\n", line_no);
  }
#endif
  if (ctx_running->flags & EVAL_CPS_CONTEXT_FLAG_TRAP) {
    if (lbm_heap_num_free() < 3) {
      gc();
    }

    if (lbm_heap_num_free() >= 3) {
      lbm_value msg = lbm_cons(err_val, ENC_SYM_NIL);
      msg = lbm_cons(lbm_enc_i(ctx_running->id), msg);
      msg = lbm_cons(ENC_SYM_EXIT_ERROR, msg);
      if (!lbm_is_symbol_merror(msg)) {
        lbm_find_receiver_and_send(ctx_running->parent, msg);
      }
    }
    // context dies.
  } else if ((ctx_running->flags & EVAL_CPS_CONTEXT_FLAG_TRAP_UNROLL_RETURN) &&
      (err_val != ENC_SYM_FATAL_ERROR)) {
    while (ctx_running->K.sp > 0) {
      lbm_uint v = ctx_running->K.data[--ctx_running->K.sp];
      if (v == EXCEPTION_HANDLER) { // context continues executing.
        lbm_value *sptr = get_stack_ptr(ctx_running, 2);
        lbm_set_car(sptr[0], ENC_SYM_EXIT_ERROR);
        stack_reserve(ctx_running, 1)[0] = EXCEPTION_HANDLER;
        ctx_running->app_cont = true;
        ctx_running->r = err_val;
        longjmp(error_jmp_buf, 1);
      }
    }
    err_val = ENC_SYM_FATAL_ERROR;
  }
  ctx_running->r = err_val;
  finish_ctx();
  longjmp(error_jmp_buf, 1);
}

#ifdef LBM_USE_ERROR_LINENO
static noreturn void error_at_ctx(lbm_value err_val, lbm_value at, int line_no) {
  error_ctx_base(err_val, true, at, 0, 0, line_no);
}

static noreturn void error_ctx(lbm_value err_val, int line_no) {
  error_ctx_base(err_val, false, 0, 0, 0, line_no);
}

static noreturn void read_error_ctx(unsigned int row, unsigned int column, int line_no) {
  error_ctx_base(ENC_SYM_RERROR, false, 0, row, column, line_no);
}
#else
static noreturn void error_at_ctx(lbm_value err_val, lbm_value at) {
  error_ctx_base(err_val, true, at, 0, 0);
}

static noreturn void error_ctx(lbm_value err_val) {
  error_ctx_base(err_val, false, 0, 0, 0);
}

static noreturn void read_error_ctx(unsigned int row, unsigned int column) {
  error_ctx_base(ENC_SYM_RERROR, false, 0, row, column);
}
#endif

void lbm_critical_error(void) {
  longjmp(critical_error_jmp_buf, 1);
}

// successfully finish a context
static void ok_ctx(void) {
  if (ctx_running->flags & EVAL_CPS_CONTEXT_FLAG_TRAP) {
    lbm_value msg;
    WITH_GC(msg, lbm_heap_allocate_list_init(3,
                                             ENC_SYM_EXIT_OK,
                                             lbm_enc_i(ctx_running->id),
                                             ctx_running->r));
    lbm_find_receiver_and_send(ctx_running->parent, msg);
  }
  finish_ctx();
}

static eval_context_t *dequeue_ctx_nm(eval_context_queue_t *q) {
  if (q->last == NULL) {
    return NULL;
  }
  // q->first should only be NULL if q->last is.
  eval_context_t *res = q->first;

  if (q->first == q->last) { // One thing in queue
    q->first = NULL;
    q->last  = NULL;
   } else {
    q->first = q->first->next;
    q->first->prev = NULL;
  }
  res->prev = NULL;
  res->next = NULL;
  return res;
}

static void wake_up_ctxs_nm(void) {
  lbm_uint t_now;

  if (timestamp_us_callback) {
    t_now = timestamp_us_callback();
  } else {
    t_now = 0;
  }

  eval_context_queue_t *q = &blocked;
  eval_context_t *curr = q->first;

  while (curr != NULL) {
    lbm_uint t_diff;
    eval_context_t *next = curr->next;
    if (LBM_IS_STATE_WAKE_UP_WAKABLE(curr->state)) {
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
        eval_context_t *wake_ctx = curr;
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
          q->first->prev = NULL;
        } else {
          curr->prev->next = curr->next;
          if (curr->next) {
            curr->next->prev = curr->prev;
          }
        }
        wake_ctx->next = NULL;
        wake_ctx->prev = NULL;
        if (LBM_IS_STATE_TIMEOUT(curr->state)) {
          mailbox_add_mail(wake_ctx, ENC_SYM_TIMEOUT);
          wake_ctx->r = ENC_SYM_TIMEOUT;
        }
        wake_ctx->state = LBM_THREAD_STATE_READY;
        enqueue_ctx_nm(&queue, wake_ctx);
      }
    }
    curr = next;
  }
}

static void yield_ctx(lbm_uint sleep_us) {
  if (is_atomic) atomic_error();
  if (timestamp_us_callback) {
    ctx_running->timestamp = timestamp_us_callback();
    ctx_running->sleep_us = sleep_us;
    ctx_running->state = LBM_THREAD_STATE_SLEEPING;
  } else {
    ctx_running->timestamp = 0;
    ctx_running->sleep_us = 0;
    ctx_running->state = LBM_THREAD_STATE_SLEEPING;
  }
  ctx_running->r = ENC_SYM_TRUE;
  ctx_running->app_cont = true;
  enqueue_ctx(&blocked,ctx_running);
  ctx_running = NULL;
}

static lbm_cid lbm_create_ctx_parent(lbm_value program, lbm_value env, lbm_uint stack_size, lbm_cid parent, uint32_t context_flags, char *name) {

  if (!lbm_is_cons(program)) return -1;

  eval_context_t *ctx = NULL;
#ifdef LBM_ALWAYS_GC
  {
    lbm_uint roots[2] = {program, env};
    lbm_gc_mark_roots(roots, 2);
    gc();
  }
#endif
  ctx = (eval_context_t*)lbm_malloc(sizeof(eval_context_t));
  if (ctx == NULL) {
    lbm_uint roots[2] = {program, env};
    lbm_gc_mark_roots(roots, 2);
    gc();
    ctx = (eval_context_t*)lbm_malloc(sizeof(eval_context_t));
  }
  if (ctx == NULL) return -1;
#ifdef LBM_ALWAYS_GC
  {
    lbm_uint roots[2] = {program, env};
    lbm_gc_mark_roots(roots, 2);
    gc();
  }
#endif
  if (!lbm_stack_allocate(&ctx->K, stack_size)) {
    lbm_uint roots[2] = {program, env};
    lbm_gc_mark_roots(roots, 2);
    gc();
    if (!lbm_stack_allocate(&ctx->K, stack_size)) {
      lbm_memory_free((lbm_uint*)ctx);
      return -1;
    }
  }

  lbm_value *mailbox = NULL;
#ifdef LBM_ALWAYS_GC
  {
    lbm_uint roots[2] = {program, env};
    lbm_gc_mark_roots(roots, 2);
    gc();
  }
#endif
  mailbox = (lbm_value*)lbm_memory_allocate(EVAL_CPS_DEFAULT_MAILBOX_SIZE);
  if (mailbox == NULL) {
    lbm_value roots[2] = {program, env};
    lbm_gc_mark_roots(roots,2);
    gc();
    mailbox = (lbm_value *)lbm_memory_allocate(EVAL_CPS_DEFAULT_MAILBOX_SIZE);
  }
  if (mailbox == NULL) {
    lbm_stack_free(&ctx->K);
    lbm_memory_free((lbm_uint*)ctx);
    return -1;
  }

  // TODO: Limit names to 19 chars + 1 char for 0? (or something similar).
  if (name) {
    lbm_uint name_len = strlen(name) + 1;
#ifdef LBM_ALWAYS_GC
    {
      lbm_uint roots[2] = {program, env};
      lbm_gc_mark_roots(roots, 2);
      gc();
    }
#endif
    ctx->name = lbm_malloc(name_len);
    if (ctx->name == NULL) {
      lbm_value roots[2] = {program, env};
      lbm_gc_mark_roots(roots, 2);
      gc();
      ctx->name = lbm_malloc(name_len);
    }
    if (ctx->name == NULL) {
      lbm_stack_free(&ctx->K);
      lbm_memory_free((lbm_uint*)mailbox);
      lbm_memory_free((lbm_uint*)ctx);
      return -1;
    }
    memcpy(ctx->name, name, name_len);
  } else {
     ctx->name = NULL;
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
  ctx->app_cont = false;
  ctx->timestamp = 0;
  ctx->sleep_us = 0;
  ctx->state = LBM_THREAD_STATE_READY;
  ctx->prev = NULL;
  ctx->next = NULL;

  ctx->row0 = -1;
  ctx->row1 = -1;

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

lbm_cid lbm_create_ctx(lbm_value program, lbm_value env, lbm_uint stack_size, char *name) {
  // Creates a parentless context.
  return lbm_create_ctx_parent(program,
                               env,
                               stack_size,
                               -1,
                               EVAL_CPS_CONTEXT_FLAG_NOTHING,
                               name);
}

bool lbm_mailbox_change_size(eval_context_t *ctx, lbm_uint new_size) {

  lbm_value *mailbox = NULL;
#ifdef LBM_ALWAYS_GC
  gc();
#endif
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
  ctx->mailbox_size = (uint32_t)new_size;
  return true;
}

static void mailbox_remove_mail(eval_context_t *ctx, lbm_uint ix) {

  for (lbm_uint i = ix; i < ctx->num_mail-1; i ++) {
    ctx->mailbox[i] = ctx->mailbox[i+1];
  }
  ctx->num_mail --;
}

static void mailbox_add_mail(eval_context_t *ctx, lbm_value mail) {

  if (ctx->num_mail >= ctx->mailbox_size) {
    mailbox_remove_mail(ctx, 0);
  }

  ctx->mailbox[ctx->num_mail] = mail;
  ctx->num_mail ++;
}

/**************************************************************
 * Advance execution to the next expression in the program.
 * Assumes programs are not malformed. Apply_eval_program
 * ensures programs are lists ending in nil. The reader
 * ensures this likewise.
 *************************************************************/
static void advance_ctx(eval_context_t *ctx) {
  if (ctx->program) { // fast not-nil check,  assume cons if not nil.
    stack_reserve(ctx, 1)[0] = DONE;
    lbm_cons_t *cell = lbm_ref_cell(ctx->program);
    ctx->curr_exp = cell->car;
    ctx->program = cell->cdr;
    ctx->curr_env = ENC_SYM_NIL;
  } else {
    if (ctx_running == ctx) {  // This should always be the case because of odd historical reasons.
      ok_ctx();
    }
  }
}

bool lbm_unblock_ctx(lbm_cid cid, lbm_flat_value_t *fv) {
  return event_internal(LBM_EVENT_UNBLOCK_CTX, (lbm_uint)cid, (lbm_uint)fv->buf, fv->buf_size);
}

bool lbm_unblock_ctx_r(lbm_cid cid) {
  mutex_lock(&blocking_extension_mutex);
  bool r = false;
  eval_context_t *found = NULL;
  mutex_lock(&qmutex);
  found = lookup_ctx_nm(&blocked, cid);
  if (found && (LBM_IS_STATE_UNBLOCKABLE(found->state))) {
    drop_ctx_nm(&blocked,found);
    found->state = LBM_THREAD_STATE_READY;
    enqueue_ctx_nm(&queue,found);
    r = true;
  }
  mutex_unlock(&qmutex);
  mutex_unlock(&blocking_extension_mutex);
  return r;
}

// unblock unboxed is also safe for rmbr:ed things.
// TODO: What happens if we unblock and the value is "merror"
bool lbm_unblock_ctx_unboxed(lbm_cid cid, lbm_value unboxed) {
  mutex_lock(&blocking_extension_mutex);
  bool r = false;
  eval_context_t *found = NULL;
  mutex_lock(&qmutex);
  found = lookup_ctx_nm(&blocked, cid);
  if (found && (LBM_IS_STATE_UNBLOCKABLE(found->state))) {
    drop_ctx_nm(&blocked,found);
    found->r = unboxed;
    if (lbm_is_error(unboxed)) {
      get_stack_ptr(found, 1)[0] = TERMINATE; // replace TOS
      found->app_cont = true;
    }
    found->state = LBM_THREAD_STATE_READY;
    enqueue_ctx_nm(&queue,found);
    r = true;
  }
  mutex_unlock(&qmutex);
  mutex_unlock(&blocking_extension_mutex);
  return r;
}

static bool lbm_block_ctx_base(bool timeout, float t_s) {
  mutex_lock(&blocking_extension_mutex);
  blocking_extension = true;
  if (timeout) {
    blocking_extension_timeout_us = S_TO_US(t_s);
    blocking_extension_timeout = true;
  } else {
    blocking_extension_timeout = false;
  }
  return true;
}

void lbm_block_ctx_from_extension_timeout(float s) {
  lbm_block_ctx_base(true, s);
}

void lbm_block_ctx_from_extension(void) {
  lbm_block_ctx_base(false, 0);
}

// todo: May need to pop rmbrs from stack, if present.
// Suspect that the letting the discard cont run is really not a problem.
// Either way will be quite confusing what happens to allocated things when undoing block.
void lbm_undo_block_ctx_from_extension(void) {
  blocking_extension = false;
  blocking_extension_timeout_us = 0;
  blocking_extension_timeout = false;
  mutex_unlock(&blocking_extension_mutex);
}

// TODO: very similar iteration patterns.
//       Try to break out common part from free_space and from find_and_send
/** mailbox_free_space_for_cid is used to get the available
 * space in a given context's mailbox.
 */
static uint32_t lbm_mailbox_free_space_for_cid(lbm_cid cid) {
  eval_context_t *found = NULL;
  uint32_t res = 0;

  mutex_lock(&qmutex);

  found = lookup_ctx_nm(&blocked, cid);
  if (!found) {
    found = lookup_ctx_nm(&queue, cid);
  }
  if (!found && ctx_running && ctx_running->id == cid) {
    found = ctx_running;
  }

  if (found) {
    res = found->mailbox_size - found->num_mail;
  }

  mutex_unlock(&qmutex);

  return res;
}

/** find_receiver_and_send is used for message passing where
 * the semantics is that the oldest message is dropped if the
 * receiver mailbox is full.
 */
bool lbm_find_receiver_and_send(lbm_cid cid, lbm_value msg) {
  mutex_lock(&qmutex);
  eval_context_t *found = NULL;
  int res = true;

  found = lookup_ctx_nm(&blocked, cid);
  if (found) {
    if (LBM_IS_STATE_RECV(found->state)) { // only if unblock receivers here.
      drop_ctx_nm(&blocked,found);
      found->state = LBM_THREAD_STATE_READY;
      enqueue_ctx_nm(&queue,found);
    }
    mailbox_add_mail(found, msg);
    goto find_receiver_end;
  }

  found = lookup_ctx_nm(&queue, cid);
  if (found) {
    mailbox_add_mail(found, msg);
    goto find_receiver_end;
  }

  /* check the current context */
  if (ctx_running && ctx_running->id == cid) {
    mailbox_add_mail(ctx_running, msg);
    goto find_receiver_end;
  }
  res = false;
 find_receiver_end:
  mutex_unlock(&qmutex);
  return res;
}

// a match binder looks like (? x) or (? _) for example.
// It is a list of two elements where the first is a ? and the second is a symbol.
static inline lbm_value get_match_binder_variable(lbm_value exp) {
  lbm_value var = ENC_SYM_NIL; // 0 false
  if (lbm_is_cons(exp)) {
    lbm_cons_t *e_cell = lbm_ref_cell(exp);
    lbm_value bt = e_cell->car;
    if (bt == ENC_SYM_MATCH_ANY && lbm_is_cons(e_cell->cdr)) {
      var = lbm_ref_cell(e_cell->cdr)->car;
    }
  }
  return var;
}

/* Pattern matching is currently implemented as a recursive
   function and make use of stack relative to the size of
   expressions that are being matched. */
static bool match(lbm_value p, lbm_value e, lbm_value *env) {
  bool r = false;
  lbm_value var = get_match_binder_variable(p);
  if (var) {
#ifdef LBM_ALWAYS_GC
    lbm_gc_mark_phase(*env);
    gc();
#endif
    lbm_value ls = lbm_heap_allocate_list_init(2, var, ENC_SYM_NIL);
    if (!lbm_is_ptr(ls)) {
      lbm_gc_mark_phase(*env);
      gc();
      ls = lbm_heap_allocate_list_init(2, var, ENC_SYM_NIL);
      if (!lbm_is_ptr(ls)) {
        ERROR_CTX(ls);
      }
    }
    lbm_value c1 = ls;
    lbm_value c2 = lbm_cdr(ls);
    lbm_set_cdr(c1, e);
    lbm_set_car_and_cdr(c2, c1, *env);
    *env = c2;
    r = true;
  } else  if (lbm_is_symbol(p)) {
    if (p == ENC_SYM_DONTCARE) r = true;
    else r = (p == e);
  } else if (lbm_is_cons(p) && lbm_is_cons(e) ) {
    lbm_cons_t *p_cell = lbm_ref_cell(p);
    lbm_cons_t *e_cell = lbm_ref_cell(e);
    lbm_value headp = p_cell->car;
    lbm_value tailp = p_cell->cdr;
    lbm_value heade = e_cell->car;
    lbm_value taile = e_cell->cdr;
    r = match(headp, heade, env);
    r = r && match (tailp, taile, env);
  } else {
    r = struct_eq(p, e);
  }
  return r;
}

// Find match is not very picky about syntax.
// A completely malformed recv form is most likely to
// just return no_match.
static int find_match(lbm_value plist, lbm_value *earr, lbm_uint num, lbm_value *e, lbm_value *env) {
  // A pattern list is a list of pattern, expression lists.
  // ( (p1 e1) (p2 e2) ... (pn en))
  lbm_value curr_p = plist;
  int n = 0;
  for (int i = 0; i < (int)num; i ++ ) {
    lbm_value curr_e = earr[i];
    while (lbm_is_cons(curr_p)) {
      lbm_value p[3];
      lbm_value curr = lbm_ref_cell(curr_p)->car;
      extract_n(curr, p, 3);
      if (!lbm_is_symbol_nil(p[2])) { // A rare syntax check. maybe drop?
        lbm_set_error_reason("Incorrect pattern format for recv");
        ERROR_AT_CTX(ENC_SYM_EERROR,curr);
      }
      if (match(p[0], curr_e, env)) {
        *e = p[1];
        return n;
      }
      curr_p = lbm_ref_cell(curr_p)->cdr;
    }
    curr_p = plist;       /* search all patterns against next exp */
    n ++;
  }
  return FM_NO_MATCH;
}

/****************************************************/
/* Garbage collection                               */

static void mark_context(eval_context_t *ctx, void *arg1, void *arg2) {
  (void) arg1;
  (void) arg2;
  lbm_value roots[3] = {ctx->curr_exp, ctx->program, ctx->r};
  lbm_gc_mark_env(ctx->curr_env);
  lbm_gc_mark_roots(roots, 3);
  lbm_gc_mark_roots(ctx->mailbox, ctx->num_mail);
  lbm_gc_mark_aux(ctx->K.data, ctx->K.sp);
}

static int gc(void) {
  if (ctx_running) {
    ctx_running->state = ctx_running->state | LBM_THREAD_STATE_GC_BIT;
  }

  gc_requested = false;
  lbm_gc_state_inc();

  // The freelist should generally be NIL when GC runs.
  lbm_nil_freelist();
  lbm_value *env = lbm_get_global_env();
  for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
    lbm_gc_mark_env(env[i]);
  }

  mutex_lock(&qmutex); // Lock the queues.
                       // Any concurrent messing with the queues
                       // while doing GC cannot possibly be good.
  queue_iterator_nm(&queue, mark_context, NULL, NULL);
  queue_iterator_nm(&blocked, mark_context, NULL, NULL);

  if (ctx_running) {
    mark_context(ctx_running, NULL, NULL);
  }
  mutex_unlock(&qmutex);

  int r = lbm_gc_sweep_phase();
  lbm_heap_new_freelist_length();
  lbm_memory_update_min_free();

  if (ctx_running) {
    ctx_running->state = ctx_running->state & ~LBM_THREAD_STATE_GC_BIT;
  }
  return r;
}

int lbm_perform_gc(void) {
  return gc();
}

/****************************************************/
/* Evaluation functions                             */


static void eval_symbol(eval_context_t *ctx) {
  lbm_uint s = lbm_dec_sym(ctx->curr_exp);
  if (s >= RUNTIME_SYMBOLS_START) {
    lbm_value res = ENC_SYM_NIL;
    if (lbm_env_lookup_b(&res, ctx->curr_exp, ctx->curr_env) ||
        lbm_global_env_lookup(&res, ctx->curr_exp)) {
      ctx->r =  res;
      ctx->app_cont = true;
      return;
    }
    // Dynamic load attempt
    // Only symbols of kind RUNTIME can be dynamically loaded.
    const char *sym_str = lbm_get_name_by_symbol(s);
    const char *code_str = NULL;
    if (!dynamic_load_callback(sym_str, &code_str)) {
      ERROR_AT_CTX(ENC_SYM_NOT_FOUND, ctx->curr_exp);
    }
    lbm_value *sptr = stack_reserve(ctx, 3);
    sptr[0] = ctx->curr_exp;
    sptr[1] = ctx->curr_env;
    sptr[2] = RESUME;

    lbm_value chan = ENC_SYM_NIL;
#ifdef LBM_ALWAYS_GC
    gc();
#endif
    if (!create_string_channel((char *)code_str, &chan, ENC_SYM_NIL)) {
      gc();
      if (!create_string_channel((char *)code_str, &chan, ENC_SYM_NIL)) {
        ERROR_CTX(ENC_SYM_MERROR);
      }
    }

    // Here, chan has either been assigned or execution has terminated.

    lbm_value loader;
    WITH_GC_RMBR_1(loader, lbm_heap_allocate_list_init(2,
                                                       ENC_SYM_READ,
                                                       chan), chan);
    lbm_value evaluator;
    WITH_GC_RMBR_1(evaluator, lbm_heap_allocate_list_init(2,
                                                          ENC_SYM_EVAL,
                                                          loader), loader);
    ctx->curr_exp = evaluator;
    ctx->curr_env = ENC_SYM_NIL; // dynamics should be evaluable in empty local env
  } else {
    //special symbols and extensions can be handled the same way.
    ctx->r = ctx->curr_exp;
    ctx->app_cont = true;
  }
}

// (quote e) => e
static void eval_quote(eval_context_t *ctx) {
  ctx->r = get_cadr(ctx->curr_exp);
  ctx->app_cont = true;
}

// a => a
static void eval_selfevaluating(eval_context_t *ctx) {
  ctx->r = ctx->curr_exp;
  ctx->app_cont = true;
}

// (progn e1 ... en)
static void eval_progn(eval_context_t *ctx) {
  lbm_value exps = get_cdr(ctx->curr_exp);

  if (lbm_is_cons(exps)) {
    lbm_cons_t *cell = lbm_ref_cell(exps); // already checked that it's cons.
    ctx->curr_exp = cell->car;
    if (lbm_is_cons(cell->cdr)) { // malformed progn not ending in nil is tolerated
      lbm_uint *sptr = stack_reserve(ctx, 4);
      sptr[0] = ctx->curr_env; // env to restore between expressions in progn
      sptr[1] = lbm_enc_u(0);  // Has env been copied (needed for progn local bindings)
      sptr[2] = cell->cdr;     // Requirement: sptr[2] is a cons.
      sptr[3] = PROGN_REST;
    }
  } else if (lbm_is_symbol_nil(exps)) { // Empty progn is nil
    ctx->r = ENC_SYM_NIL;
    ctx->app_cont = true;
  } else {
    ERROR_CTX(ENC_SYM_EERROR);
  }
}

// (atomic e1 ... en)
static void eval_atomic(eval_context_t *ctx) {
  if (is_atomic) atomic_error();
  stack_reserve(ctx, 1)[0] = EXIT_ATOMIC;
  is_atomic = true;
  eval_progn(ctx);
}

// (call-cc (lambda (k) .... ))
static void eval_callcc(eval_context_t *ctx) {
  lbm_value cont_array;
  lbm_uint *sptr0 = stack_reserve(ctx, 1);
  sptr0[0] = is_atomic ? ENC_SYM_TRUE : ENC_SYM_NIL;
#ifdef LBM_ALWAYS_GC
  gc();
#endif
  if (!lbm_heap_allocate_lisp_array(&cont_array, ctx->K.sp)) {
    gc();
    lbm_heap_allocate_lisp_array(&cont_array, ctx->K.sp);
  }
  if (lbm_is_ptr(cont_array)) {
    lbm_array_header_t *arr = assume_array(cont_array);
    memcpy(arr->data, ctx->K.data, ctx->K.sp * sizeof(lbm_uint));
    // The stored stack contains the is_atomic flag.
    // This flag is overwritten in the following execution path.

    lbm_value acont = cons_with_gc(ENC_SYM_CONT, cont_array, ENC_SYM_NIL);
    lbm_value arg_list = cons_with_gc(acont, ENC_SYM_NIL, ENC_SYM_NIL);
    // Go directly into application evaluation without passing go
    lbm_uint *sptr = stack_reserve(ctx, 2);
    sptr0[0] = ctx->curr_env;
    sptr[0] = arg_list;
    sptr[1] = APPLICATION_START;
    ctx->curr_exp = get_cadr(ctx->curr_exp);
  } else {
    // failed to create continuation array.
    ERROR_CTX(ENC_SYM_MERROR);
  }
}

// (call-cc-unsafe (lambda (k) ... ))
// cc-unsafe: continuation should not be bound to any global directly or indirectly.
// invoking the continuation must check that target SP holds a continuation that
// can be applied using app_cont, otherwise error. The continuation need not be correct
// in case user globally bound the continuation, but it may rule out disastrous failure.
static void eval_call_cc_unsafe(eval_context_t *ctx) {
  lbm_uint sp = ctx->K.sp;
  // The stored stack contains the is_atomic flag.
  // This flag is overwritten in the following execution path.
  lbm_value acont;
  WITH_GC(acont, lbm_heap_allocate_list_init(3,
                                             ENC_SYM_CONT_SP,
                                             lbm_enc_i((int32_t)sp),
                                             is_atomic ? ENC_SYM_TRUE : ENC_SYM_NIL, ENC_SYM_NIL));
  lbm_value arg_list = cons_with_gc(acont, ENC_SYM_NIL, ENC_SYM_NIL);
  // Go directly into application evaluation without passing go
  lbm_uint *sptr = stack_reserve(ctx, 3);
  sptr[0] = ctx->curr_env;
  sptr[1] = arg_list;
  sptr[2] = APPLICATION_START;
  ctx->curr_exp = get_cadr(ctx->curr_exp);
}

// (define sym exp)
#define KEY 1
#define VAL 2
static void eval_define(eval_context_t *ctx) {
  lbm_value parts[3];
  lbm_value rest = extract_n(ctx->curr_exp, parts, 3);
  lbm_uint *sptr = stack_reserve(ctx, 2);
  if (lbm_is_symbol(parts[KEY]) && lbm_is_symbol_nil(rest)) {
    lbm_uint sym_val = lbm_dec_sym(parts[KEY]);
    sptr[0] = parts[KEY];
    if (sym_val >= RUNTIME_SYMBOLS_START) {
      sptr[1] = SET_GLOBAL_ENV;
      if (ctx->flags & EVAL_CPS_CONTEXT_FLAG_CONST) {
        stack_reserve(ctx, 1)[0] = MOVE_VAL_TO_FLASH_DISPATCH;
      }
      ctx->curr_exp = parts[VAL];
      return;
    } else {
      lbm_set_error_reason((char*)lbm_error_str_built_in);
    }
  }
  ERROR_AT_CTX(ENC_SYM_EERROR, ctx->curr_exp);
}

#if false
/* Allocate closure is only used in eval_lambda currently.
   Inlining it should use no extra storage.
 */
static inline lbm_value allocate_closure(lbm_value params, lbm_value body, lbm_value env) {

#ifdef LBM_ALWAYS_GC
  gc();
  if (lbm_heap_num_free() < 4) {
    ERROR_CTX(ENC_SYM_MERROR);
  }
#else
  if (lbm_heap_num_free() < 4) {
    gc();
    if (lbm_heap_num_free() < 4) {
      ERROR_CTX(ENC_SYM_MERROR);
    }
  }
#endif
  // The freelist will always contain just plain heap-cells.
  // So dec_ptr is sufficient.
  lbm_value res = lbm_heap_state.freelist;
  // CONS check is not needed. If num_free is correct, then freelist is a cons-cell.
  lbm_cons_t *heap = lbm_heap_state.heap;
  lbm_uint ix = lbm_dec_ptr(res);
  heap[ix].car = ENC_SYM_CLOSURE;
  ix = lbm_dec_ptr(heap[ix].cdr);
  heap[ix].car = params;
  ix = lbm_dec_ptr(heap[ix].cdr);
  heap[ix].car = body;
  ix = lbm_dec_ptr(heap[ix].cdr);
  heap[ix].car = env;
  lbm_heap_state.freelist = heap[ix].cdr;
  heap[ix].cdr = ENC_SYM_NIL;
  lbm_heap_state.num_alloc+=4;
  return res;
}

/* Eval lambda is cheating, a lot! It does this
   for performance reasons. The cheats are that
   1. When  closure is created, a reference to the local env
   in which the lambda was evaluated is added to the closure.
   Ideally it should have created a list of free variables in the function
   and then looked up the values of these creating a new environment.
   2. The global env is considered global constant. As there is no copying
   of environment bindings into the closure, undefine may break closures.

   some obscure programs such as test_setq_local_closure.lisp does not
   work properly due to this cheating.
 */
// (lambda param-list body-exp) -> (closure param-list body-exp env)


static void eval_lambda(eval_context_t *ctx) {
  lbm_value vals[3];
  extract_n(ctx->curr_exp, vals, 3);
  ctx->r = allocate_closure(vals[1],vals[2], ctx->curr_env);
#ifdef CLEAN_UP_CLOSURES
  lbm_uint sym_id  = 0;
  if (clean_cl_env_symbol) {
    lbm_value tail = cons_with_gc(ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
    lbm_value app = cons_with_gc(clean_cl_env_symbol, tail, tail);
    ctx->curr_exp = app;
  } else if (lbm_get_symbol_by_name("clean-cl-env", &sym_id)) {
    clean_cl_env_symbol = lbm_enc_sym(sym_id);
    lbm_value tail = cons_with_gc(ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
    lbm_value app = cons_with_gc(clean_cl_env_symbol, tail, tail);
    ctx->curr_exp = app;
  } else {
    ctx->app_cont = true;
  }
#else
  ctx->app_cont = true;
#endif
}
#else
static void eval_lambda(eval_context_t *ctx) {
#ifdef LBM_ALWAYS_GC
  gc();
#endif
  for (int retry = 0; retry < 2; retry ++) {
    if (lbm_heap_num_free() >= 4) {
      lbm_value clo = lbm_heap_state.freelist;
      lbm_value lam = get_cdr(ctx->curr_exp);
      lbm_uint ix = lbm_dec_ptr(clo);
      lbm_cons_t *heap = lbm_heap_state.heap;
      heap[ix].car = ENC_SYM_CLOSURE;
      ix = lbm_dec_ptr(heap[ix].cdr);
      get_car_and_cdr(lam, &heap[ix].car, &lam); // params
      ix = lbm_dec_ptr(heap[ix].cdr);
      get_car_and_cdr(lam, &heap[ix].car, &lam); // body
      ix = lbm_dec_ptr(heap[ix].cdr);
      heap[ix].car = ctx->curr_env;
      lbm_heap_state.freelist = heap[ix].cdr;
      heap[ix].cdr = ENC_SYM_NIL;
      lbm_heap_state.num_alloc+=4;
      ctx->r = clo;
#ifdef CLEAN_UP_CLOSURES
      lbm_uint sym_id  = 0;
      if (clean_cl_env_symbol) {
        lbm_value tail = cons_with_gc(ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
        lbm_value app = cons_with_gc(clean_cl_env_symbol, tail, tail);
        ctx->curr_exp = app;
      } else if (lbm_get_symbol_by_name("clean-cl-env", &sym_id)) {
        clean_cl_env_symbol = lbm_enc_sym(sym_id);
        lbm_value tail = cons_with_gc(ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
        lbm_value app = cons_with_gc(clean_cl_env_symbol, tail, tail);
        ctx->curr_exp = app;
      } else {
        ctx->app_cont = true;
      }
#else
      ctx->app_cont = true;
#endif
      return;
    } else {
      gc();
    }
  }
  ERROR_CTX(ENC_SYM_MERROR);
}
#endif

// (if cond-expr then-expr else-expr)
static void eval_if(eval_context_t *ctx) {
  lbm_value cdr = get_cdr(ctx->curr_exp);
  lbm_value *sptr = stack_reserve(ctx, 3);
  get_car_and_cdr(cdr, &ctx->curr_exp, &sptr[0]);
  sptr[1] = ctx->curr_env;
  sptr[2] = IF;
}

// (cond (cond-expr-1 expr-1)
//         ...
//       (cond-expr-N expr-N))
static void eval_cond(eval_context_t *ctx) {
  lbm_value cond1[2];
  lbm_value rest_conds = extract_n(ctx->curr_exp, cond1, 2);

  // end recursion at (cond )
  if (lbm_is_symbol_nil(cond1[1])) {
    ctx->r = ENC_SYM_NIL;
    ctx->app_cont = true;
  } else {
    // Cond is one of the few places where a bit of syntax checking takes place at runtime..
    // Maybe dont bother?
    lbm_uint len = lbm_list_length(cond1[1]);
    if (len != 2) {
      lbm_set_error_reason("Incorrect syntax in cond");
      ERROR_CTX(ENC_SYM_EERROR);
    }
    lbm_value cond_expr[2];
    extract_n(cond1[1], cond_expr, 2);
    lbm_value rest;
    WITH_GC(rest, lbm_heap_allocate_list_init(2,
                                              cond_expr[1], // Then branch
                                              cons_with_gc(ENC_SYM_COND, rest_conds , ENC_SYM_NIL)));
    lbm_value *sptr = stack_reserve(ctx, 3);
    sptr[0] = rest;
    sptr[1] = ctx->curr_env;
    sptr[2] = IF;
    ctx->curr_exp = cond_expr[0]; //condition;
  }
}

static void eval_app_cont(eval_context_t *ctx) {
  lbm_stack_drop(&ctx->K, 1);
  ctx->app_cont = true;
}

// Create a named location in an environment to later receive a value.
// Protects env from GC, other data is the obligation of the caller.
static void create_binding_location(lbm_value key, lbm_value *env) {
  if (lbm_is_symbol(key)) { // default case
    if (key == ENC_SYM_NIL || key == ENC_SYM_DONTCARE) return;
#ifdef LBM_ALWAYS_GC
    lbm_gc_mark_phase(*env);
    gc();
#endif
    lbm_value ls = lbm_heap_allocate_list_init(2,
                                               key,
                                               ENC_SYM_NIL);
    if (!lbm_is_ptr(ls)) {
      lbm_gc_mark_phase(*env);
      gc();
      ls = lbm_heap_allocate_list_init(2,
                                       key,
                                       ENC_SYM_NIL);
      if (!lbm_is_ptr(ls)) ERROR_CTX(ENC_SYM_MERROR);
    }
    lbm_value binding = ls;
    lbm_cons_t *ls_ref = lbm_ref_cell(ls);
    lbm_value new_env = ls_ref->cdr;
    ls_ref->cdr = ENC_SYM_PLACEHOLDER; // known cons
    //lbm_set_cdr(binding, ENC_SYM_PLACEHOLDER);
    lbm_cons_t *new_env_ref = lbm_ref_cell(new_env); //known cons
    new_env_ref->car = binding;
    new_env_ref->cdr = *env;
    //lbm_set_car_and_cdr(new_env,binding, *env);
    *env = new_env;
  } else if (lbm_is_cons(key)) { // deconstruct case
    create_binding_location(lbm_ref_cell(key)->car, env);
    create_binding_location(lbm_ref_cell(key)->cdr, env);
  } else {
    ERROR_CTX(ENC_SYM_EERROR);
  }
}

static void let_bind_values_eval(lbm_value binds, lbm_value exp, lbm_value env, eval_context_t *ctx) {
  if (lbm_is_cons(binds)) {
      // Preallocate binding locations.
      lbm_value curr = binds;
      while (lbm_is_cons(curr)) {
        lbm_value new_env_tmp = env;
        lbm_cons_t *cell = lbm_ref_cell(curr); // already checked that cons.
        lbm_value car_curr = cell->car;
        lbm_value cdr_curr = cell->cdr;
        lbm_value key = get_car(car_curr);
        create_binding_location(key, &new_env_tmp);
        env = new_env_tmp;
        curr = cdr_curr;
      }

      lbm_cons_t *cell = lbm_ref_cell(binds); // already checked that cons.
      lbm_value car_binds = cell->car;
      lbm_value cdr_binds = cell->cdr;
      lbm_value key_val[2];
      extract_n(car_binds, key_val, 2);

      lbm_uint *sptr = stack_reserve(ctx, 5);
      sptr[0] = exp;
      sptr[1] = cdr_binds;
      sptr[2] = env;
      sptr[3] = key_val[0];
      sptr[4] = BIND_TO_KEY_REST;
      ctx->curr_exp = key_val[1];
      ctx->curr_env = env;
    } else {
      ctx->curr_exp = exp;
    }
}

// (var x (...)) - local binding inside of an progn
// var has to take, place root-level nesting within progn.
// (progn ... (var a 10) ...) OK!
// (progn ... (something (var a 10)) ... ) NOT OK!
/* progn stack
   sp-4 : env
   sp-3 : 0
   sp-2 : rest
   sp-1 : PROGN_REST
*/
static void eval_var(eval_context_t *ctx) {
  if (ctx->K.sp >= 4) { // Possibly in progn
    lbm_value sv = ctx->K.data[ctx->K.sp - 1];
    if (IS_CONTINUATION(sv) && (sv == PROGN_REST)) {
      lbm_uint sp = ctx->K.sp;
      uint32_t is_copied = lbm_dec_as_u32(ctx->K.data[sp-3]);
      if (is_copied == 0) {
        lbm_value env;
        WITH_GC(env, lbm_env_copy_spine(ctx->K.data[sp-4]));
        ctx->K.data[sp-3] = lbm_enc_u(1);
        ctx->K.data[sp-4] = env;
      }
      lbm_value new_env = ctx->K.data[sp-4];
      lbm_value args = get_cdr(ctx->curr_exp);
      lbm_value key = get_car(args);
      create_binding_location(key, &new_env);

      ctx->K.data[sp-4] = new_env;

      lbm_value v_exp = get_cadr(args);
      lbm_value *sptr = stack_reserve(ctx, 3);
      sptr[0] = new_env;
      sptr[1] = key;
      sptr[2] = PROGN_VAR;
      // Activating the new environment before the evaluation of the value to be bound.
      // This would normally shadow the existing value, but create_binding_location sets
      // the binding to be $placeholder, which is ignored when looking up the value.
      // The way closures work, the var-variable needs to be in scope during val
      // evaluation for a recursive closure to be possible.
      ctx->curr_env = new_env;
      ctx->curr_exp = v_exp;
      return;
    }
  }
  lbm_set_error_reason((char*)lbm_error_str_var_outside_progn);
  ERROR_CTX(ENC_SYM_EERROR);
}

// (setq x (...)) - same as (set 'x (...)) or (setvar 'x (...))
// does not error when given incorrect number of arguments.
static void eval_setq(eval_context_t *ctx) {
  lbm_value parts[3];
  extract_n(ctx->curr_exp, parts, 3);
  lbm_value *sptr = stack_reserve(ctx, 3);
  sptr[0] = ctx->curr_env;
  sptr[1] = parts[1];
  sptr[2] = SETQ;
  ctx->curr_exp = parts[2];
}

static void eval_move_to_flash(eval_context_t *ctx) {
  lbm_value args = get_cdr(ctx->curr_exp);
  lbm_value *sptr = stack_reserve(ctx,2);
  sptr[0] = args;
  sptr[1] = MOVE_TO_FLASH;
  ctx->app_cont = true;
}

// (loop list-of-local-bindings
//       condition-exp
//       body-exp)
static void eval_loop(eval_context_t *ctx) {
  lbm_value env              = ctx->curr_env;
  lbm_value parts[3];
  extract_n(get_cdr(ctx->curr_exp), parts, 3);
  lbm_value *sptr = stack_reserve(ctx, 4);
  sptr[0] = parts[LOOP_BODY];
  sptr[1] = parts[LOOP_COND];
  sptr[2] = ENC_SYM_NIL;
  sptr[3] = LOOP_ENV_PREP;
  let_bind_values_eval(parts[LOOP_BINDS], ENC_SYM_NIL, env, ctx);
}

/* (trap expression)
 *
 * suggested use:
 * (match (trap expression)
 *   ((exit-error (? err)) (error-handler err))
 *   ((exit-ok    (? v))   (value-handler v)))
 */
static void eval_trap(eval_context_t *ctx) {

  lbm_value expr = get_cadr(ctx->curr_exp);
  lbm_value retval;
  WITH_GC(retval, lbm_heap_allocate_list(2));
  lbm_ref_cell(retval)->car = ENC_SYM_EXIT_OK;
  // lbm_set_car(retval, ENC_SYM_EXIT_OK); // Assume things will go well.
  lbm_uint *sptr = stack_reserve(ctx,3);
  sptr[0] = retval;
  sptr[1] = ctx->flags;
  sptr[2] = EXCEPTION_HANDLER;
  ctx->flags |= EVAL_CPS_CONTEXT_FLAG_TRAP_UNROLL_RETURN;
  ctx->curr_exp = expr;
}

// (let list-of-binding s
//      body-exp)
static void eval_let(eval_context_t *ctx) {
  lbm_value env      = ctx->curr_env;
  lbm_value parts[3];
  extract_n(ctx->curr_exp, parts, 3);
  let_bind_values_eval(parts[1], parts[2], env, ctx);
}

// (and exp0 ... expN)
static void eval_and(eval_context_t *ctx) {
  lbm_value rest = get_cdr(ctx->curr_exp);
  if (lbm_is_symbol_nil(rest)) {
    ctx->app_cont = true;
    ctx->r = ENC_SYM_TRUE;
  } else {
    lbm_value *sptr = stack_reserve(ctx, 3);
    get_car_and_cdr(rest, &ctx->curr_exp, &sptr[1]);
    sptr[0] = ctx->curr_env;
    sptr[2] = AND;
  }
}

// (or exp0 ... expN)
static void eval_or(eval_context_t *ctx) {
  lbm_value rest = get_cdr(ctx->curr_exp);
  if (lbm_is_symbol_nil(rest)) {
    ctx->app_cont = true;
    ctx->r = ENC_SYM_NIL;
  } else {
    lbm_value *sptr = stack_reserve(ctx, 3);
    get_car_and_cdr(rest, &ctx->curr_exp, &sptr[1]);
    sptr[0] = ctx->curr_env;
    sptr[2] = OR;
  }
}

// Pattern matching
// format:
// (match e (pattern body)
//          (pattern body)
//          ...  )
//
// There can be an optional pattern guard:
// (match e (pattern guard body)
//          ... )
// a guard is a boolean expression.
// Guards make match, pattern matching more complicated
// than the recv pattern matching and requires staged execution
// via the continuation system rather than a while loop over a list.
static void eval_match(eval_context_t *ctx) {

  lbm_value rest = get_cdr(ctx->curr_exp);
  if (lbm_is_cons(rest)) {
    lbm_cons_t *cell = lbm_ref_cell(rest);
    lbm_value cdr_rest = cell->cdr;
    ctx->curr_exp = cell->car;
    lbm_value *sptr = stack_reserve(ctx, 3);
    sptr[0] = cdr_rest;
    sptr[1] = ctx->curr_env;
    sptr[2] = MATCH;
  } else {
    // someone wrote the program (match)
    ERROR_CTX(ENC_SYM_EERROR);
  }
}

// Receive-timeout
// (recv-to timeout (pattern expr)
//                  (pattern expr))
static void eval_receive_timeout(eval_context_t *ctx) {
  if (is_atomic) atomic_error();
  lbm_value timeout_val = get_cadr(ctx->curr_exp);
  lbm_value pats = get_cdr(get_cdr(ctx->curr_exp));
  if (lbm_is_symbol_nil(pats)) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    ERROR_AT_CTX(ENC_SYM_EERROR, ctx->curr_exp);
  } else {
    lbm_value *sptr = stack_reserve(ctx, 2);
    sptr[0] = pats;
    sptr[1] = RECV_TO;
    ctx->curr_exp = timeout_val;
  }
}

// Receive
// (recv (pattern expr)
//       (pattern expr))
static void eval_receive(eval_context_t *ctx) {
  if (is_atomic) atomic_error();
  lbm_value pats = get_cdr(ctx->curr_exp);
  if (pats) { // non-nil check
    if (ctx->num_mail == 0) {
      block_current_ctx(LBM_THREAD_STATE_RECV_BL,0,false);
    } else {
      lbm_value *msgs = ctx->mailbox;
      lbm_uint  num   = ctx->num_mail;

      lbm_value e;
      lbm_value new_env = ctx->curr_env;
      int n = find_match(pats, msgs, num, &e, &new_env);
      if (n >= 0 ) { /* Match */
        mailbox_remove_mail(ctx, (lbm_uint)n);
        ctx->curr_env = new_env;
        ctx->curr_exp = e;
      } else { /* No match  go back to sleep */
        ctx->r = ENC_SYM_NO_MATCH;
        block_current_ctx(LBM_THREAD_STATE_RECV_BL, 0,false);
      }
    }
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    ERROR_AT_CTX(ENC_SYM_EERROR,ctx->curr_exp);
  }
}
 
/*********************************************************/
/*  Continuation functions                               */

// cont_set_global_env:
//
//   s[sp-1] = Key-symbol
//
//   ctx->r = Value
static void cont_set_global_env(eval_context_t *ctx){

  lbm_value val = ctx->r;

  lbm_value key = ctx->K.data[--ctx->K.sp];
  lbm_uint dec_key = lbm_dec_sym(key);
  lbm_uint ix_key  = dec_key & GLOBAL_ENV_MASK;
  lbm_value *global_env = lbm_get_global_env();
  lbm_uint orig_env = global_env[ix_key];
  lbm_value new_env;
  // A key is a symbol and should not need to be remembered.
  WITH_GC(new_env, lbm_env_set(orig_env,key,val));

  global_env[ix_key] = new_env;
  ctx->r = val;

  ctx->app_cont = true;
}

// cont_resume:
//
// s[sp-2] = Expression
// s[sp-1] = Environment
//
// ctx->r = Irrelevant.
static void cont_resume(eval_context_t *ctx) {
  ctx->curr_env = ctx->K.data[--ctx->K.sp];
  ctx->curr_exp = ctx->K.data[--ctx->K.sp];
}

// cont_progn_rest:
//
// s[sp-3] = Environment to evaluate each expression in.
// s[sp-2] = Flag indicating if env has been copied.
// s[sp-1] = list of expressions to evaluate.
//
// ctx->r = Result of last evaluated expression.
static void cont_progn_rest(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 3);

  lbm_value env  = sptr[0];
  // eval_progn and cont_progn_rest both ensure that sptr[2] is a list
  // whenever cont_progn_rest is called.

  lbm_cons_t *rest_cell = lbm_ref_cell(sptr[2]);
  lbm_value rest_cdr = rest_cell->cdr;
  ctx->curr_exp = rest_cell->car;;
  ctx->curr_env = env;
  if (lbm_is_cons(rest_cdr)) {
    sptr[2] = rest_cdr; // Requirement: rest_cdr is a cons
    stack_reserve(ctx, 1)[0] = PROGN_REST;
  } else {
    // Nothing is pushed to stack for final element in progn. (tail-call req)
    lbm_stack_drop(&ctx->K, 3);
  }
}

// cont_wait
//
// s[sp-1] = cid
static void cont_wait(eval_context_t *ctx) {

  lbm_value cid_val = ctx->K.data[--ctx->K.sp];
  lbm_cid cid = (lbm_cid)lbm_dec_i(cid_val);

  bool exists = false;

  lbm_blocked_iterator(context_exists, &cid, &exists);
  lbm_running_iterator(context_exists, &cid, &exists);

  if (ctx_running->id == cid) {
    exists = true;
  }

  if (exists) {
    lbm_value *sptr = stack_reserve(ctx, 2);
    sptr[0] = lbm_enc_i(cid);
    sptr[1] = WAIT;
    ctx->r = ENC_SYM_TRUE;
    ctx->app_cont = true;
    yield_ctx(50000);
  } else {
    ctx->r = ENC_SYM_TRUE;
    ctx->app_cont = true;
  }
}

/***************************************************/
/* Application helper functions.                   */


/**
 * @brief Setup application of cont object (created by call-cc)
 * 
 * The "function" form, e.g. `(SYM_CONT . cont-array)`, is expected to be stored
 * in `ctx->r`.
 * 
 * @param args List of the arguments to apply with.
 * @return lbm_value The resulting argument value which should either be
 *   evaluated or passed on directly depending on how you use this.
 */
static lbm_value setup_cont(eval_context_t *ctx, lbm_value args) {
  /* Continuation created using call-cc.
   * ((SYM_CONT . cont-array) arg0 )
   */
  lbm_value c = get_cdr(ctx->r); /* should be the continuation array*/

  if (!lbm_is_lisp_array_r(c)) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  }
  
  lbm_value arg;
  lbm_uint arg_count = lbm_list_length(args);
  switch (arg_count) {
  case 0:
    arg = ENC_SYM_NIL;
    break;
  case 1:
    arg = get_car(args);
    break;
  default:
    lbm_set_error_reason(lbm_error_str_num_args);
    ERROR_CTX(ENC_SYM_EERROR);
  }

  lbm_stack_clear(&ctx->K);

  lbm_array_header_t *arr = assume_array(c);
  ctx->K.sp = arr->size / sizeof(lbm_uint);
  memcpy(ctx->K.data, arr->data, arr->size);

  lbm_value atomic = ctx->K.data[--ctx->K.sp];
  is_atomic = atomic ? 1 : 0;
  
  return arg;
}

/**
 * @brief Setup application of cont sp object (created by call-cc-unsafe)
 * 
 * The "function" form, e.g. `(SYM_CONT_SP . stack_ptr)` is expected to be
 * stored in `ctx->r`.
 * 
 * @param args List of the arguments to apply with.
 * @return lbm_value The resulting argument value which should either be
 *   evaluated or passed on directly depending on how you use this.
 */
static lbm_value setup_cont_sp(eval_context_t *ctx, lbm_value args) {
  // continuation created using call-cc-unsafe
  // ((SYM_CONT_SP . stack_ptr) arg0 )
  lbm_value c = get_cadr(ctx->r); /* should be the stack_ptr*/
  lbm_value atomic = get_cadr(get_cdr(ctx->r));

  if (!lbm_is_number(c)) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  }

  lbm_uint sp = (lbm_uint)lbm_dec_i(c);

  lbm_value arg;
  lbm_uint arg_count = lbm_list_length(args);
  switch (arg_count) {
  case 0:
    arg = ENC_SYM_NIL;
    break;
  case 1:
    arg = get_car(args);
    break;
  default:
    lbm_set_error_reason(lbm_error_str_num_args);
    ERROR_CTX(ENC_SYM_EERROR);
  }
  
  if (sp > 0 && sp <= ctx->K.sp && IS_CONTINUATION(ctx->K.data[sp-1])) {
    is_atomic = atomic ? 1 : 0; // works fine with nil/true
    ctx->K.sp = sp;
    return arg;
  } else {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  }
}

/**
 * @brief Setup application of macro
 * 
 * The macro form, e.g. `(macro (...) ...)`, is expected to be stored in
 * `ctx->r`.
 * 
 * @param args List of the arguments to apply the macro with.
 * @param curr_env The environment to re-evaluate the result of the macro
 *   experssion in. 
 */
static inline __attribute__ ((always_inline)) void setup_macro(eval_context_t *ctx, lbm_value args, lbm_value curr_env) {
  /*
   * Perform macro expansion.
   * Macro expansion is really just evaluation in an
   * environment augmented with the unevaluated expressions passed
   * as arguments.
   */

  lbm_uint *sptr = stack_reserve(ctx, 2);
  // For EVAL_R, placed here already to protect from GC
  sptr[0] = curr_env;
  // Placed here only to protect from GC, will be overriden.
  sptr[1] = args;

  lbm_value curr_param = get_cadr(ctx->r);
  lbm_value curr_arg = args;
  lbm_value expand_env = curr_env;
  while (lbm_is_cons(curr_param) &&
          lbm_is_cons(curr_arg)) {
    lbm_cons_t *param_cell = lbm_ref_cell(curr_param); // already checked that cons.
    lbm_cons_t *arg_cell = lbm_ref_cell(curr_arg);
    lbm_value car_curr_param = param_cell->car;
    lbm_value cdr_curr_param = param_cell->cdr;
    lbm_value car_curr_arg = arg_cell->car;
    lbm_value cdr_curr_arg = arg_cell->cdr;

    lbm_value entry = cons_with_gc(car_curr_param, car_curr_arg, expand_env);
    lbm_value aug_env = cons_with_gc(entry, expand_env,ENC_SYM_NIL);
    expand_env = aug_env;

    curr_param = cdr_curr_param;
    curr_arg   = cdr_curr_arg;
  }
  /* Two rounds of evaluation is performed.
   * First to instantiate the arguments into the macro body.
   * Second to evaluate the resulting program.
   */
  
  sptr[1] = EVAL_R;
  lbm_value exp = get_cadr(get_cdr(ctx->r));
  ctx->curr_exp = exp;
  ctx->curr_env = expand_env;
}

static lbm_value perform_setvar(lbm_value key, lbm_value val, lbm_value env) {

  lbm_uint s = lbm_dec_sym(key);
  if (s >= RUNTIME_SYMBOLS_START) {
    lbm_value new_env = lbm_env_modify_binding(env, key, val);
    if (lbm_is_symbol(new_env) && new_env == ENC_SYM_NOT_FOUND) {
      lbm_uint ix_key = lbm_dec_sym(key) & GLOBAL_ENV_MASK;
      lbm_value *glob_env = lbm_get_global_env();
      new_env = lbm_env_modify_binding(glob_env[ix_key], key, val);
      if (new_env != ENC_SYM_NOT_FOUND) {
        glob_env[ix_key] = new_env;
      }
    }
    if (lbm_is_symbol(new_env) && new_env == ENC_SYM_NOT_FOUND) {
      lbm_set_error_reason((char*)lbm_error_str_variable_not_bound);
      ERROR_AT_CTX(ENC_SYM_NOT_FOUND, key);
    }
    return val;
  }
  ERROR_AT_CTX(ENC_SYM_EERROR, ENC_SYM_SETVAR);
  return ENC_SYM_NIL; // unreachable
}

static void apply_setvar(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 && lbm_is_symbol(args[0])) {
    lbm_value res;
    WITH_GC(res, perform_setvar(args[0], args[1], ctx->curr_env));
    ctx->r = args[1];
    lbm_stack_drop(&ctx->K, nargs+1);
    ctx->app_cont = true;
  } else {
    if (nargs == 2) lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
    else lbm_set_error_reason((char*)lbm_error_str_num_args);
    ERROR_AT_CTX(ENC_SYM_EERROR, ENC_SYM_SETVAR);
  }
}


#define READING_EXPRESSION             ((0 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READING_PROGRAM                ((1 << LBM_VAL_SHIFT) | LBM_TYPE_U)
#define READING_PROGRAM_INCREMENTALLY  ((2 << LBM_VAL_SHIFT) | LBM_TYPE_U)

static void apply_read_base(lbm_value *args, lbm_uint nargs, eval_context_t *ctx, bool program, bool incremental) {
  if (nargs == 1) {
    lbm_value chan = ENC_SYM_NIL;
    if (lbm_type_of_functional(args[0]) == LBM_TYPE_ARRAY) {
      char *str = lbm_dec_str(args[0]);
      if (str) {
#ifdef LBM_ALWAYS_GC
        gc();
#endif
        if (!create_string_channel(lbm_dec_str(args[0]), &chan, args[0])) {
          gc();
          if (!create_string_channel(lbm_dec_str(args[0]), &chan, args[0])) {
            ERROR_CTX(ENC_SYM_MERROR);
          }
        }
      } else {
        ERROR_CTX(ENC_SYM_EERROR);
      }
    } else if (lbm_type_of(args[0]) == LBM_TYPE_CHANNEL) {
      chan = args[0];
      // Streaming transfers can freeze the evaluator if the stream is cut while
      // the reader is reading inside of an atomic block.
      // It is generally not advisable to read in an atomic block but now it is also
      // enforced in the case where it can cause problems.
      if (lbm_channel_may_block(lbm_dec_channel(chan)) && is_atomic) {
       lbm_set_error_reason((char*)lbm_error_str_forbidden_in_atomic);
       is_atomic = false;
       ERROR_CTX(ENC_SYM_EERROR);
      }
    } else {
      ERROR_CTX(ENC_SYM_EERROR);
    }
    lbm_value *sptr = get_stack_ptr(ctx, 2);

    // If we are inside a reader, its settings are stored.
    sptr[0] = lbm_enc_u(ctx->flags);  // flags stored.
    sptr[1] = chan;
    lbm_value  *rptr = stack_reserve(ctx,2);
    if (!program && !incremental) {
      rptr[0] = READING_EXPRESSION;
    } else if (program && !incremental) {
      rptr[0] = READING_PROGRAM;
    } else if (program && incremental) {
      rptr[0] = READING_PROGRAM_INCREMENTALLY;
    }  // the last combo is illegal
    rptr[1] = READ_DONE;

    // Each reader starts in a fresh situation
    ctx->flags &= ~EVAL_CPS_CONTEXT_READER_FLAGS_MASK;
    ctx->r = ENC_SYM_NIL; // set r to a known state.

    if (program) {
      if (incremental) {
        ctx->flags |= EVAL_CPS_CONTEXT_FLAG_INCREMENTAL_READ;
        lbm_value  *rptr1 = stack_reserve(ctx,3);
        rptr1[0] = chan;
        rptr1[1] = ctx->curr_env;
        rptr1[2] = READ_EVAL_CONTINUE;
      } else {
        ctx->flags &= ~EVAL_CPS_CONTEXT_FLAG_INCREMENTAL_READ;
        lbm_value  *rptr1 = stack_reserve(ctx,4);
        rptr1[0] = ENC_SYM_NIL;
        rptr1[1] = ENC_SYM_NIL;
        rptr1[2] = chan;
        rptr1[3] = READ_APPEND_CONTINUE;
      }
    }
    rptr = stack_reserve(ctx,3); // reuse of variable rptr
    rptr[0] = chan;
    rptr[1] = lbm_enc_u(1);
    rptr[2] = READ_NEXT_TOKEN;
    ctx->app_cont = true;
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    ERROR_CTX(ENC_SYM_EERROR);
  }
}

static void apply_read_program(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  apply_read_base(args,nargs,ctx,true,false);
}

static void apply_read_eval_program(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  apply_read_base(args,nargs,ctx,true,true);
}

static void apply_read(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  apply_read_base(args,nargs,ctx,false,false);
}

static void apply_spawn_base(lbm_value *args, lbm_uint nargs, eval_context_t *ctx, uint32_t context_flags) {

  lbm_uint stack_size = EVAL_CPS_DEFAULT_STACK_SIZE;
  lbm_uint closure_pos = 0;
  char *name = NULL;
  // allowed arguments:
  // (spawn opt-name opt-stack-size closure arg1 ... argN)

  if (nargs >= 1 &&
      lbm_is_closure(args[0])) {
    closure_pos = 0;
  } else if (nargs >= 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_closure(args[1])) {
    stack_size = lbm_dec_as_u32(args[0]);
    closure_pos = 1;
  } else if (nargs >= 2 &&
             lbm_is_array_r(args[0]) &&
             lbm_is_closure(args[1])) {
    name = lbm_dec_str(args[0]);
    closure_pos = 1;
  } else if (nargs >= 3 &&
             lbm_is_array_r(args[0]) &&
             lbm_is_number(args[1]) &&
             lbm_is_closure(args[2])) {
    stack_size = lbm_dec_as_u32(args[1]);
    closure_pos = 2;
    name = lbm_dec_str(args[0]);
  } else {
    if (context_flags & EVAL_CPS_CONTEXT_FLAG_TRAP)
      ERROR_AT_CTX(ENC_SYM_TERROR,ENC_SYM_SPAWN_TRAP);
    else
      ERROR_AT_CTX(ENC_SYM_TERROR,ENC_SYM_SPAWN);
  }

  lbm_value cl[3];
  extract_n(get_cdr(args[closure_pos]), cl, 3);
  lbm_value curr_param = cl[CLO_PARAMS];
  lbm_value clo_env    = cl[CLO_ENV];
  lbm_uint i = closure_pos + 1;
  while (lbm_is_cons(curr_param) && i <= nargs) {
    lbm_value entry = cons_with_gc(lbm_ref_cell(curr_param)->car, args[i], clo_env);
    lbm_value aug_env = cons_with_gc(entry, clo_env,ENC_SYM_NIL);
    clo_env = aug_env;
    curr_param = lbm_ref_cell(curr_param)->cdr;
    i ++;
  }

  lbm_stack_drop(&ctx->K, nargs+1);

  lbm_value program = cons_with_gc(cl[CLO_BODY], ENC_SYM_NIL, clo_env);

  lbm_cid cid = lbm_create_ctx_parent(program,
                                      clo_env,
                                      stack_size,
                                      lbm_get_current_cid(),
                                      context_flags,
                                      name);
  ctx->r = lbm_enc_i(cid);
  ctx->app_cont = true;
  if (cid == -1) ERROR_CTX(ENC_SYM_MERROR); // Kill parent and signal out of memory.
}

static void apply_spawn(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  apply_spawn_base(args,nargs,ctx, EVAL_CPS_CONTEXT_FLAG_NOTHING);
}

static void apply_spawn_trap(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  apply_spawn_base(args,nargs,ctx, EVAL_CPS_CONTEXT_FLAG_TRAP);
}

static void apply_yield(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1 && lbm_is_number(args[0])) {
    lbm_uint ts = lbm_dec_as_u32(args[0]);
    lbm_stack_drop(&ctx->K, nargs+1);
    yield_ctx(ts);
  } else {
    lbm_set_error_reason((char*)lbm_error_str_no_number);
    ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_YIELD);
  }
}

static void apply_sleep(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1 && lbm_is_number(args[0])) {
    lbm_uint ts = (lbm_uint)(1000000.0f * lbm_dec_as_float(args[0]));
    lbm_stack_drop(&ctx->K, nargs+1);
    yield_ctx(ts);
  } else {
    lbm_set_error_reason((char*)lbm_error_str_no_number);
    ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_SLEEP);
  }
}

static void apply_wait(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1 && lbm_type_of(args[0]) == LBM_TYPE_I) {
    lbm_cid cid = (lbm_cid)lbm_dec_i(args[0]);
    lbm_value *sptr = get_stack_ptr(ctx, 2);
    sptr[0] = lbm_enc_i(cid);
    sptr[1] = WAIT;
    ctx->r = ENC_SYM_TRUE;
    ctx->app_cont = true;
    yield_ctx(50000);
  } else {
    ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_WAIT);
  }
}

/* (eval expr)
   (eval env expr) */
static void apply_eval(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if ( nargs == 1) {
    ctx->curr_exp = args[0];
  } else if (nargs == 2) {
    ctx->curr_exp = args[1];
    ctx->curr_env = args[0];
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    ERROR_AT_CTX(ENC_SYM_EERROR, ENC_SYM_EVAL);
  }
  lbm_stack_drop(&ctx->K, nargs+1);
}

static void apply_eval_program(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value prg = args[0]; // No check that this is a program.
    lbm_value app_cont;
    lbm_value app_cont_prg;
    lbm_value new_prg;
    lbm_value prg_copy;

    int len = -1;
    WITH_GC(prg_copy, lbm_list_copy(&len, prg));
    lbm_stack_drop(&ctx->K, nargs+1);
    // There is always a continuation (DONE).
    // If ctx->program is nil, the stack should contain DONE.
    // after adding an intermediate done for prg, stack becomes DONE, DONE.
    app_cont = cons_with_gc(ENC_SYM_APP_CONT, ENC_SYM_NIL, prg_copy);
    app_cont_prg = cons_with_gc(app_cont, ENC_SYM_NIL, prg_copy);
    new_prg = lbm_list_append(app_cont_prg, ctx->program);
    new_prg = lbm_list_append(prg_copy, new_prg);
    // new_prg is guaranteed to be a cons cell or nil
    // even if the eval-program application is syntactically broken.
    stack_reserve(ctx, 1)[0] = DONE;
    ctx->program = get_cdr(new_prg);
    ctx->curr_exp = get_car(new_prg);
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    ERROR_AT_CTX(ENC_SYM_EERROR, ENC_SYM_EVAL_PROGRAM);
  }
}

static void apply_send(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2) {
    if (lbm_type_of(args[0]) == LBM_TYPE_I) {
      lbm_cid cid = (lbm_cid)lbm_dec_i(args[0]);
      lbm_value msg = args[1];
      bool r = lbm_find_receiver_and_send(cid, msg);
      /* return the status */
      lbm_stack_drop(&ctx->K, nargs+1);
      ctx->r = r ? ENC_SYM_TRUE : ENC_SYM_NIL;
      ctx->app_cont = true;
    } else {
      ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_SEND);
    }
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    ERROR_AT_CTX(ENC_SYM_EERROR, ENC_SYM_SEND);
  }
}

static void apply_ok(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value ok_val = ENC_SYM_TRUE;
  if (nargs >= 1) {
    ok_val = args[0];
  }
  is_atomic = false;
  ctx->r = ok_val;
  ok_ctx();
}

static void apply_error(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value err_val = ENC_SYM_EERROR;
  if (nargs >= 1) {
    err_val = args[0];
  }
  is_atomic = false;
  ERROR_AT_CTX(err_val, ENC_SYM_EXIT_ERROR);
}

// ////////////////////////////////////////////////////////////
// Map takes a function f and a list ls as arguments.
// The function f is applied to each element of ls.
//
// Normally when applying a function to an argument this happens:
//   1. the function is evaluated
//   2. the argument is evaluated
//   3. the result of evaluating the function is applied to the result of evaluating
//      the argument.
//
// When doing (map f arg-list) I assume one means to apply f to each element of arg-list
// exactly as those elements are. That is, no evaluation of the argument.
// The implementation of map below makes sure that the elements of the arg-list are not
// evaluated by wrapping them each in a `quote`.
//
// Map creates a structure in memory that looks like this (f (quote dummy . nil) . nil).
// Then, for each element from arg-list (example a1 ... aN) the object
// (f (quote aM . nil) . nil) is created by substituting dummy for an element of the list.
// after this substitution the evaluator is fired up to evaluate the entire (f (quote aM . nil) . nil)
// structure resulting in an element for the result list.
//
// Here comes the fun part, if you (map quote arg-list), then the object
// (quote (quote aM . nil) . nil) is created and evaluated. Now note that quote just gives back
// exactly what you give to it when evaluated.
// So (quote (quote aM . nil) . nil) gives you as result (quote aM . nil) and now also note that
// this is a list, and a list is really just an address on the heap!
// This leads to the very fun behavior that:
//
// # (map quote '(1 2 3 4))
// > ((quote 4) (quote 4) (quote 4) (quote 4))
//
// A potential fix is to instead of creating the object (f (quote aM . nil) . nil)
// we create the object (f var) for some unique var and then extend the environment
// for each round of evaluation with a binding var => aM.

// (map f arg-list)
static void apply_map(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 && lbm_is_cons(args[1])) {
    lbm_value *sptr = get_stack_ptr(ctx, 3);

    lbm_value f = args[0];
    lbm_cons_t *args1_cell = lbm_ref_cell(args[1]);
    lbm_value h = args1_cell->car;
    lbm_value t = args1_cell->cdr;

    lbm_value appli_1;
    lbm_value appli;
    WITH_GC(appli_1, lbm_heap_allocate_list(2));
    WITH_GC_RMBR_1(appli, lbm_heap_allocate_list(2), appli_1);

    // appli_1 is a list of length 2 here.
    lbm_value appli_0 = lbm_ref_cell(appli_1)->cdr;

    // appli is a list of length 2 here, so a cons
    lbm_cons_t *cell = lbm_ref_cell(appli_0);
    cell->car = h;
    cell->cdr = ENC_SYM_NIL;
    //lbm_set_car_and_cdr(appli_0, h, ENC_SYM_NIL);
    cell = lbm_ref_cell(appli_1);
    cell->car = ENC_SYM_QUOTE;
    //lbm_set_car(appli_1, ENC_SYM_QUOTE);
    lbm_cons_t *appli_cell = lbm_ref_cell(appli);
    cell = lbm_ref_cell(appli_cell->cdr);
    cell->car = appli_1;
    cell->cdr = ENC_SYM_NIL;
    //lbm_set_car_and_cdr(get_cdr(appli), appli_1, ENC_SYM_NIL);
    appli_cell->car = f;
    //lbm_set_car(appli, f);

    lbm_value elt = cons_with_gc(ctx->r, ENC_SYM_NIL, appli);
    sptr[0] = t;     // reuse stack space
    sptr[1] = ctx->curr_env;
    sptr[2] = elt;
    lbm_value *rptr = stack_reserve(ctx,4);
    rptr[0] = elt;
    rptr[1] = appli;
    rptr[2] = appli_0;
    rptr[3] = MAP;
    ctx->curr_exp = appli;
  } else if (nargs == 2 && lbm_is_symbol_nil(args[1])) {
    lbm_stack_drop(&ctx->K, 3);
    ctx->r = ENC_SYM_NIL;
    ctx->app_cont = true;
  } else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    ERROR_AT_CTX(ENC_SYM_EERROR, ENC_SYM_MAP);
  }
}

static void apply_reverse(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1 && lbm_is_list(args[0])) {
    lbm_value curr = args[0];

    lbm_value new_list = ENC_SYM_NIL;
    while (lbm_is_cons(curr)) {
      lbm_cons_t *curr_cell = lbm_ref_cell(curr); // known cons.
      lbm_value tmp = cons_with_gc(curr_cell->car, new_list, ENC_SYM_NIL);
      new_list = tmp;
      curr = curr_cell->cdr;
    }
    lbm_stack_drop(&ctx->K, 2);
    ctx->r = new_list;
    ctx->app_cont = true;
  } else {
    lbm_set_error_reason("Reverse requires a list argument");
    ERROR_AT_CTX(ENC_SYM_EERROR, ENC_SYM_REVERSE);
  }
}

static void apply_flatten(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
#ifdef LBM_ALWAYS_GC
    gc();
#endif
    lbm_value v = flatten_value(args[0]);
    if ( v == ENC_SYM_MERROR) {
      gc();
      v = flatten_value(args[0]);
    }

    if (lbm_is_symbol(v)) {
      ERROR_AT_CTX(v, ENC_SYM_FLATTEN);
    } else {
      lbm_stack_drop(&ctx->K, 2);
      ctx->r = v;
      ctx->app_cont = true;
    }
    return;
  }
  ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_FLATTEN);
}

static void apply_unflatten(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_array_header_t *array;
  if(nargs == 1 && (array = lbm_dec_array_r(args[0]))) {
    lbm_flat_value_t fv;
    fv.buf = (uint8_t*)array->data;
    fv.buf_size = array->size;
    fv.buf_pos = 0;

    lbm_value res;

    ctx->r = ENC_SYM_NIL;
    if (lbm_unflatten_value(&fv, &res)) {
      ctx->r =  res;
    }
    lbm_stack_drop(&ctx->K, 2);
    ctx->app_cont = true;
    return;
  }
  ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_UNFLATTEN);
}

static void apply_kill(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 && lbm_is_number(args[0])) {
    lbm_cid cid = lbm_dec_as_i32(args[0]);

    if (ctx->id == cid) {
      ctx->r = args[1];
      finish_ctx();
      return;
    }
    mutex_lock(&qmutex);
    eval_context_t *found = NULL;
    found = lookup_ctx_nm(&blocked, cid);
    if (found)
      drop_ctx_nm(&blocked, found);
    else
      found = lookup_ctx_nm(&queue, cid);
    if (found)
      drop_ctx_nm(&queue, found);

    if (found) {
      found->K.data[found->K.sp - 1] = KILL;
      found->r = args[1];
      found->app_cont = true;
      found->state = LBM_THREAD_STATE_READY;
      enqueue_ctx_nm(&queue,found);
      ctx->r = ENC_SYM_TRUE;
    } else {
      ctx->r = ENC_SYM_NIL;
    }
    lbm_stack_drop(&ctx->K, 3);
    ctx->app_cont = true;
    mutex_unlock(&qmutex);
    return;
  }
  ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_KILL);
}

static lbm_value cmp_to_clo(lbm_value cmp) {
  lbm_value closure;
  WITH_GC(closure, lbm_heap_allocate_list(4));
  lbm_set_car(closure, ENC_SYM_CLOSURE);
  lbm_value cl1 = lbm_cdr(closure);
  lbm_value par;
  WITH_GC_RMBR_1(par, lbm_heap_allocate_list_init(2, symbol_x, symbol_y), closure);
  lbm_set_car(cl1, par);
  lbm_value cl2 = lbm_cdr(cl1);
  lbm_value body;
  WITH_GC_RMBR_1(body, lbm_heap_allocate_list_init(3, cmp, symbol_x, symbol_y), closure);
  lbm_set_car(cl2, body);
  lbm_value cl3 = lbm_cdr(cl2);
  lbm_set_car(cl3, ENC_SYM_NIL);
  return closure;
}

// (merge comparator list1 list2)
static void apply_merge(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 3 && lbm_is_list(args[1]) && lbm_is_list(args[2])) {

    if (!lbm_is_closure(args[0])) {
      args[0] = cmp_to_clo(args[0]);
    }

    // Copy input lists for functional behaviour at top-level
    // merge itself is in-place in the copied lists.
    lbm_value a;
    lbm_value b;
    int len_a = -1;
    int len_b = -1;
    WITH_GC(a, lbm_list_copy(&len_a, args[1]));
    WITH_GC_RMBR_1(b, lbm_list_copy(&len_b, args[2]), a);

    if (len_a == 0) {
      ctx->r = b;
      lbm_stack_drop(&ctx->K, 4);
      ctx->app_cont = true;
      return;
    }
    if (len_b == 0) {
      ctx->r = a;
      lbm_stack_drop(&ctx->K, 4);
      ctx->app_cont = true;
      return;
    }

    args[1] = a; // keep safe by replacing the original on stack.
    args[2] = b;

    lbm_value cl[3]; // Comparator closure
    extract_n(lbm_cdr(args[0]), cl, 3);
    lbm_value cmp_env = cl[CLO_ENV];
    lbm_uint len = lbm_list_length(cl[CLO_PARAMS]);
    if (len == 2) {
      lbm_value a_1 = a;
      lbm_value b_1 = b;
      lbm_value a_rest = lbm_cdr(a);
      lbm_value b_rest = lbm_cdr(b);
      lbm_value par1 = get_car(cl[CLO_PARAMS]);
      lbm_value par2 = get_cadr(cl[CLO_PARAMS]);
      lbm_value new_env0;
      lbm_value new_env;
      WITH_GC(new_env0, lbm_env_set(cmp_env, par1, lbm_car(a_1)));
      WITH_GC_RMBR_1(new_env, lbm_env_set(new_env0, par2, lbm_car(b_1)),new_env0);
      cmp_env = new_env;
      lbm_set_cdr(a_1, b_1);
      lbm_set_cdr(b_1, ENC_SYM_NIL);
      lbm_value cmp = cl[CLO_BODY];

      lbm_stack_drop(&ctx->K, 4); // TODO: Optimize drop 4 alloc 10 into alloc 6
      lbm_uint *sptr = stack_reserve(ctx, 10);
      sptr[0] = ENC_SYM_NIL; // head of merged list
      sptr[1] = ENC_SYM_NIL; // last of merged list
      sptr[2] = a_1;
      sptr[3] = a_rest;
      sptr[4] = b_rest;
      sptr[5] = cmp;
      sptr[6] = cmp_env;
      sptr[7] = par1;
      sptr[8] = par2;
      sptr[9] = MERGE_REST;
      ctx->curr_exp = cl[CLO_BODY];
      ctx->curr_env = cmp_env;
      return;
    }
  }
  ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_MERGE);
}

// (sort comparator list)
static void apply_sort(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 && lbm_is_list(args[1])) {

    if (!lbm_is_closure(args[0])) {
      args[0] = cmp_to_clo(args[0]);
    }

    int len = -1;
    lbm_value list_copy;
    WITH_GC(list_copy, lbm_list_copy(&len, args[1]));
    if (len <= 1) {
      lbm_stack_drop(&ctx->K, 3);
      ctx->r = list_copy;
      ctx->app_cont = true;
      return;
    }

    args[1] = list_copy; // Keep safe, original replaced on stack.

    // Take the headmost 2, 1-element sublists.
    lbm_value a = list_copy;
    lbm_value b = lbm_cdr(a);
    lbm_value rest = lbm_cdr(b);
    // Do not terminate b. keep rest of list safe from GC in the following
    // closure extraction.
    //lbm_set_cdr(a, b); // This is void

    lbm_value cl[3]; // Comparator closure
    extract_n(lbm_cdr(args[0]), cl, 3);
    lbm_value cmp_env = cl[CLO_ENV];

    lbm_uint cl_len = lbm_list_length(cl[CLO_PARAMS]);
    if (cl_len == 2) {
      lbm_value par1 = get_car(cl[CLO_PARAMS]);
      lbm_value par2 = get_cadr(cl[CLO_PARAMS]);
      lbm_value new_env0;
      lbm_value new_env;
      WITH_GC(new_env0, lbm_env_set(cmp_env, par1, lbm_car(a)));
      WITH_GC_RMBR_1(new_env, lbm_env_set(new_env0, par2, lbm_car(b)), new_env0);
      cmp_env = new_env;

      lbm_value cmp = cl[CLO_BODY];

      // Terminate the comparator argument list.
      lbm_set_cdr(b, ENC_SYM_NIL);

      lbm_stack_drop(&ctx->K, 3);  //TODO: optimize drop 3, alloc 20 into alloc 17
      lbm_uint *sptr = stack_reserve(ctx, 20);
      sptr[0] = cmp;
      sptr[1] = cmp_env;
      sptr[2] = par1;
      sptr[3] = par2;
      sptr[4] = ENC_SYM_NIL; // head of merged accumulation of sublists
      sptr[5] = ENC_SYM_NIL; // last of merged accumulation of sublists
      sptr[6] = rest;
      sptr[7] = lbm_enc_i(1);
      sptr[8] = lbm_enc_i(len); //TODO: 28 bit i vs 32 bit i
      sptr[9] = MERGE_LAYER;
      sptr[10] = ENC_SYM_NIL; // head of merged sublist
      sptr[11] = ENC_SYM_NIL; // last of merged sublist
      sptr[12] = a;
      sptr[13] = ENC_SYM_NIL; // no a_rest, 1 element lists in layer 1.
      sptr[14] = ENC_SYM_NIL; // no b_rest, 1 element lists in layer 1.
      sptr[15] = cmp;
      sptr[16] = cmp_env;
      sptr[17] = par1;
      sptr[18] = par2;
      sptr[19] = MERGE_REST;
      ctx->curr_exp = cmp;
      ctx->curr_env = cmp_env;
      return;
    }
  }
  ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_SORT);
}

static void apply_rest_args(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value res = ENC_SYM_NIL; //TODO: lbm_env_lookup does not set res in all cases.
  if (lbm_env_lookup_b(&res, ENC_SYM_REST_ARGS, ctx->curr_env)) {
    if (nargs == 1 && lbm_is_number(args[0])) {
      int32_t ix = lbm_dec_as_i32(args[0]);
      res = lbm_index_list(res, ix);
    }
    ctx->r = res;
  } else {
    ctx->r = ENC_SYM_NIL;
  }
  lbm_stack_drop(&ctx->K, nargs+1);
  ctx->app_cont = true;
}

/* (rotate list-expr dist/dir-expr) */
static void apply_rotate(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 && lbm_is_list(args[0]) && lbm_is_number(args[1])) {
    int len = -1;
    lbm_value ls;
    WITH_GC(ls, lbm_list_copy(&len, args[0]));
    int dist = lbm_dec_as_i32(args[1]);
    if (len > 0 && dist != 0) {
      int d = dist;
      if (dist > 0) {
        ls = lbm_list_destructive_reverse(ls);
      } else {
        d = -dist;
      }

      lbm_value start = ls;
      lbm_value end = ENC_SYM_NIL;
      lbm_value curr = start;
      while (lbm_is_cons(curr)) {
        end = curr;
        curr = lbm_ref_cell(curr)->cdr;
      }

      for (int i = 0; i < d; i ++) {
        lbm_value a = start;
        start = lbm_cdr(start);
        lbm_set_cdr(a, ENC_SYM_NIL);
        lbm_set_cdr(end, a);
        end = a;
      }
      ls = start;
      if (dist > 0) {
        ls = lbm_list_destructive_reverse(ls);
      }
    }
    lbm_stack_drop(&ctx->K, nargs+1);
    ctx->app_cont = true;
    ctx->r = ls;
    return;
  }
  ERROR_CTX(ENC_SYM_EERROR);
}

/***************************************************/
/* Application lookup table                        */

typedef void (*apply_fun)(lbm_value *, lbm_uint, eval_context_t *);
static const apply_fun fun_table[] =
  {
   apply_setvar,
   apply_read,
   apply_read_program,
   apply_read_eval_program,
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
   apply_reverse,
   apply_flatten,
   apply_unflatten,
   apply_kill,
   apply_sleep,
   apply_merge,
   apply_sort,
   apply_rest_args,
   apply_rotate,
   apply_apply,
  };

/***************************************************/
/* Application of function that takes arguments    */
/* passed over the stack.                          */

static void application(eval_context_t *ctx, lbm_value *fun_args, lbm_uint arg_count) {
  /* If arriving here, we know that the fun is a symbol.
   *  and can be a built in operation or an extension.
   */
  lbm_value fun = fun_args[0];

  lbm_uint fun_val = lbm_dec_sym(fun);
  lbm_uint fun_kind = SYMBOL_KIND(fun_val);

  switch (fun_kind) {
  case SYMBOL_KIND_EXTENSION: {
    extension_fptr f = extension_table[SYMBOL_IX(fun_val)].fptr;

    lbm_value ext_res;
    WITH_GC(ext_res, f(&fun_args[1], arg_count));
    if (lbm_is_error(ext_res)) { //Error other than merror
      ERROR_AT_CTX(ext_res, fun);
    }
    lbm_stack_drop(&ctx->K, arg_count + 1);

    ctx->app_cont = true;
    ctx->r = ext_res;

    if (blocking_extension) {
      if (is_atomic) {
        // Check atomic_error explicitly so that the mutex
        // can be released if there is an error.
        blocking_extension = false;
        mutex_unlock(&blocking_extension_mutex);
        atomic_error();
      }
      blocking_extension = false;
      if (blocking_extension_timeout) {
        blocking_extension_timeout = false;
        block_current_ctx(LBM_THREAD_STATE_TIMEOUT, blocking_extension_timeout_us,true);
      } else {
        block_current_ctx(LBM_THREAD_STATE_BLOCKED, 0,true);
      }
      mutex_unlock(&blocking_extension_mutex);
    }
  }  break;
  case SYMBOL_KIND_FUNDAMENTAL:
    call_fundamental(SYMBOL_IX(fun_val), &fun_args[1], arg_count, ctx);
    break;
  case SYMBOL_KIND_APPFUN:
    fun_table[SYMBOL_IX(fun_val)](&fun_args[1], arg_count, ctx);
    break;
  default:
    // Symbols that are "special" but not in the way caught above
    // ends up here.
    lbm_set_error_reason("Symbol does not represent a function");
    ERROR_AT_CTX(ENC_SYM_EERROR,fun_args[0]);
    break;
  }
}

// cont_cloure_application_args
//
// s[sp-5]  = environment to evaluate the args in.
// s[sp-4]  = body
// s[sp-3]  = closure environment
// s[sp-2]  = parameter list
// s[sp-1]  = args list
//
// ctx->r  = evaluated argument.
static void cont_closure_application_args(eval_context_t *ctx) {
  lbm_uint* sptr = get_stack_ptr(ctx, 5);

  lbm_value arg_env = (lbm_value)sptr[0];
  lbm_value exp     = (lbm_value)sptr[1];
  lbm_value clo_env = (lbm_value)sptr[2];
  lbm_value params  = (lbm_value)sptr[3];
  lbm_value args    = (lbm_value)sptr[4];

  lbm_value car_params, cdr_params;
  get_car_and_cdr(params, &car_params, &cdr_params);

  bool a_nil = lbm_is_symbol_nil(args);
  bool p_nil = lbm_is_symbol_nil(cdr_params);

  lbm_value binder = allocate_binding(car_params, ctx->r, clo_env);

  if (!a_nil && !p_nil) {
    lbm_value car_args, cdr_args;
    get_car_and_cdr(args, &car_args, &cdr_args);
    sptr[2] = binder;
    sptr[3] = cdr_params;
    sptr[4] = cdr_args;
    stack_reserve(ctx,1)[0] = CLOSURE_ARGS;
    ctx->curr_exp = car_args;
    ctx->curr_env = arg_env;
  } else if (a_nil && p_nil) {
    // Arguments and parameters match up in number
    lbm_stack_drop(&ctx->K, 5);
    ctx->curr_env = binder;
    ctx->curr_exp = exp;
  } else if (p_nil) {
    lbm_value rest_binder = allocate_binding(ENC_SYM_REST_ARGS, ENC_SYM_NIL, binder);
    sptr[2] = rest_binder;
    sptr[3] = get_cdr(args);
    sptr[4] = get_car(rest_binder); // last element of rest_args so far
    stack_reserve(ctx,1)[0] = CLOSURE_ARGS_REST;
    ctx->curr_exp = get_car(args);
    ctx->curr_env = arg_env;
  }  else {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    ERROR_CTX(ENC_SYM_EERROR);
  }
}

// cont_closure_args_rest
//
// s[sp-5] = environment to evaluate args in
// s[sp-4] = body
// s[sp-3] = closure environment
// s[sp-2] = argument list 
// s[sp-1] = last cell in rest-args list so far.
static void cont_closure_args_rest(eval_context_t *ctx) {
  lbm_uint* sptr = get_stack_ptr(ctx, 5);
  lbm_value arg_env = (lbm_value)sptr[0];
  lbm_value exp     = (lbm_value)sptr[1];
  lbm_value clo_env = (lbm_value)sptr[2];
  lbm_value args    = (lbm_value)sptr[3];
  lbm_value last    = (lbm_value)sptr[4];
  lbm_cons_t* heap = lbm_heap_state.heap;
#ifdef LBM_ALWAYS_GC
  gc();
#endif
  lbm_value binding = lbm_heap_state.freelist;
  if (binding == ENC_SYM_NIL) {
    gc();
    binding = lbm_heap_state.freelist;
    if (binding == ENC_SYM_NIL) ERROR_CTX(ENC_SYM_MERROR);
  }
  lbm_uint binding_ix = lbm_dec_ptr(binding);
  lbm_heap_state.freelist = heap[binding_ix].cdr;
  lbm_heap_state.num_alloc += 1;
  heap[binding_ix].car = ctx->r;
  heap[binding_ix].cdr = ENC_SYM_NIL;

  lbm_set_cdr(last, binding);
  sptr[4] = binding;

  if (args == ENC_SYM_NIL) {
    lbm_stack_drop(&ctx->K, 5);
    ctx->curr_env = clo_env;
    ctx->curr_exp = exp;
  } else {
    stack_reserve(ctx,1)[0] = CLOSURE_ARGS_REST;
    sptr[3] = get_cdr(args);
    ctx->curr_exp = get_car(args);
    ctx->curr_env = arg_env;
  }
}


// cont_application_args
//  Functions that take arguments passed on the stack, fundamental and apply_f.
//
// s[sp-3] = environment to evaluate arguments in.
// s[sp-2] = argument list (user input syntax)
// s[sp-1] = count
//
// ctx->r  = function
static void cont_application_args(eval_context_t *ctx) {
  lbm_uint *sptr = get_stack_ptr(ctx, 3);

  lbm_value env = sptr[0];
  lbm_value rest = sptr[1];
  lbm_value count = sptr[2];

  ctx->curr_env = env;
  sptr[0] = ctx->r; // Function 1st then Arguments
  if (lbm_is_cons(rest)) { // rest is user input syntax, expensive check needed
    lbm_cons_t *cell = lbm_ref_cell(rest);
    sptr[1] = env;
    sptr[2] = cell->cdr;
    lbm_value *rptr = stack_reserve(ctx,2);
    rptr[0] = count + (1 << LBM_VAL_SHIFT); // arithmetic on encoded value
    rptr[1] = APPLICATION_ARGS;
    ctx->curr_exp = cell->car;
  } else { // tollerant for incorrect list termination.
    // No more arguments
    lbm_stack_drop(&ctx->K, 2);
    lbm_uint nargs = lbm_dec_u(count);
    lbm_value *args = get_stack_ptr(ctx, (uint32_t)(nargs + 1));
    application(ctx,args, nargs);
  }
}

// cont_and
//
// s[sp-2] = environment to evaluate args in.
// s[sp-1] = rest of argument list (user input syntax)
static void cont_and(eval_context_t *ctx) {
  lbm_value rest = ctx->K.data[--ctx->K.sp];
  lbm_value env = ctx->K.data[--ctx->K.sp];
  if (lbm_is_symbol_nil(ctx->r)) {
    ctx->app_cont = true;
  } else if (lbm_is_cons(rest)) {
    lbm_cons_t *r_cell = lbm_ref_cell(rest);
    lbm_value *sptr = stack_reserve(ctx, 3);
    sptr[0] = env;
    sptr[1] = r_cell->cdr;
    sptr[2] = AND;
    ctx->curr_env = env;
    ctx->curr_exp = r_cell->car;
  } else {
    ctx->app_cont = true;
  }
}

// cont_or
//
// s[sp-2] = environment to evaluate args in.
// s[sp-1] = rest of argument list (user input syntax)
static void cont_or(eval_context_t *ctx) {
  lbm_value rest = ctx->K.data[--ctx->K.sp];
  lbm_value env = ctx->K.data[--ctx->K.sp];
  if (!lbm_is_symbol_nil(ctx->r)) {
    ctx->app_cont = true;
  } else if (lbm_is_cons(rest)) {
    lbm_value *sptr = stack_reserve(ctx, 3);
    lbm_cons_t *r_cell = lbm_ref_cell(rest);
    sptr[0] = env;
    sptr[1] = r_cell->cdr;
    sptr[2] = OR;
    ctx->curr_exp = r_cell->car;
    ctx->curr_env = env;
  } else {
    // if we end up here we have traversed all arguments
    // and seen no non-nil. (see top case).
    ctx->app_cont = true;
  }
}

static void fill_binding_location(lbm_value key, lbm_value value, lbm_value env) {
  if (lbm_type_of(key) == LBM_TYPE_SYMBOL) {
    // NILs dual role makes it hard to detect the difference
    // between the end of a structural key or an attempt to use NIL as the key
    // or as part of big key.
    // NIL has been given the same role as dont care.
    if (lbm_dec_sym(key) >= RUNTIME_SYMBOLS_START) {
      lbm_env_modify_binding(env,key,value);
    } else {
      if (key == ENC_SYM_DONTCARE || key == ENC_SYM_NIL) return;
      lbm_set_error_reason((char*)lbm_error_str_built_in);
      ERROR_AT_CTX(ENC_SYM_EERROR, key);
    }
  } else if (lbm_is_cons(key) &&
             lbm_is_cons(value)) {
    fill_binding_location(lbm_ref_cell(key)->car, lbm_ref_cell(value)->car, env);
    fill_binding_location(lbm_ref_cell(key)->cdr, lbm_ref_cell(value)->cdr, env);
  } else {
    lbm_set_error_reason("Incorrect type of key in binding");
    ERROR_AT_CTX(ENC_SYM_TERROR, key);
  }
}

// cont_bind_to_key_rest
//
// s[sp-4] = expression to evaluate in final env
// s[sp-3] = rest of list of bindings
// s[sp-2] = env to evaluate values in (Modified along the way)
// s[sp-1] = key
//
// ctx->r  = evaluated value to bind to key
static void cont_bind_to_key_rest(eval_context_t *ctx) {

  lbm_value *sptr = get_stack_ptr(ctx, 4);

  lbm_value rest = sptr[1];
  lbm_value env  = sptr[2];
  lbm_value key  = sptr[3];

  fill_binding_location(key, ctx->r, env);

  if (lbm_is_cons(rest)) {
    lbm_value car_rest = lbm_ref_cell(rest)->car;
    lbm_value key_val[2];
    extract_n(car_rest, key_val, 2);

    sptr[1] = lbm_ref_cell(rest)->cdr;
    sptr[3] = key_val[0];
    stack_reserve(ctx,1)[0] = BIND_TO_KEY_REST;
    ctx->curr_exp = key_val[1];
    ctx->curr_env = env;
  } else {
    // Otherwise evaluate the expression in the populated env
    ctx->curr_exp = sptr[0];
    ctx->curr_env = env;
    lbm_stack_drop(&ctx->K, 4);
  }
}

// cont_if
//
// s[sp-2] = then/else list (user input syntax)
// s[sp-1] = environment
//
// ctx->r = evaluated condition
static void cont_if(eval_context_t *ctx) {

  lbm_value arg = ctx->r;

  lbm_value *sptr = pop_stack_ptr(ctx, 2);

  ctx->curr_env = sptr[1];
  if (lbm_is_symbol_nil(arg)) {
    ctx->curr_exp = get_cadr(sptr[0]); // else branch
  } else {
    ctx->curr_exp = get_car(sptr[0]); // then branch
  }
}

// cont_match
//
// s[sp-1] = patterns (user input syntax)
// s[sp-1] = orig_env
//
// ctx->r = expression to match against patterns
static void cont_match(eval_context_t *ctx) {
  lbm_value e = ctx->r;

  lbm_uint *sptr = get_stack_ptr(ctx, 2);
  lbm_value patterns = (lbm_value)sptr[0];
  lbm_value orig_env = (lbm_value)sptr[1]; // restore enclosing environment.
  lbm_value new_env = orig_env;

  if (lbm_is_symbol_nil(patterns)) {
    // no more patterns
    lbm_stack_drop(&ctx->K, 2);
    ctx->r = ENC_SYM_NO_MATCH;
    ctx->app_cont = true;
  } else if (lbm_is_cons(patterns)) {
    lbm_value match_case = lbm_ref_cell(patterns)->car;
    lbm_value pattern = get_car(match_case);
    lbm_value n1      = get_cadr(match_case);
    lbm_value n2      = get_cdr(get_cdr(match_case));
    lbm_value body;
    bool check_guard = false;
    if (lbm_is_symbol_nil(n2)) { // TODO: Not a very robust check.
      body = n1;
    } else {
      body = get_car(n2);
      check_guard = true;
    }
    bool is_match = match(pattern, e, &new_env);
    if (is_match) {
      if (check_guard) {
        lbm_value *rptr = stack_reserve(ctx,5);
        sptr[0] = lbm_ref_cell(patterns)->cdr;
        sptr[1] = ctx->curr_env;
        rptr[0] = MATCH;
        rptr[1] = new_env;
        rptr[2] = body;
        rptr[3] = e;
        rptr[4] = MATCH_GUARD;
        ctx->curr_env = new_env;
        ctx->curr_exp = n1; // The guard
      } else {
        lbm_stack_drop(&ctx->K, 2);
        ctx->curr_env = new_env;
        ctx->curr_exp = body;
      }
    } else {
      // set up for checking of next pattern
      sptr[0] = get_cdr(patterns);
      sptr[1] = orig_env;
      stack_reserve(ctx,1)[0] = MATCH;
      // leave r unaltered
      ctx->app_cont = true;
    }
  } else {
    ERROR_AT_CTX(ENC_SYM_TERROR, ENC_SYM_MATCH);
  }
}

static void cont_exit_atomic(eval_context_t *ctx) {
  is_atomic = false; // atomic blocks cannot nest!
  ctx->app_cont = true;
}

// cont_map:
//
// sptr[0]: s[sp-6] = Rest of the input list.
// sptr[1]: s[sp-5] = Environment to restore for the eval of each application.
// sptr[2]: s[sp-4] = Result list.
// sptr[3]: s[sp-3] = Cell that goes into result list after being populated with application result.
// sptr[4]: s[sp-2] = Ref to application.
// sptr[5]: s[sp-1] = Ref to application argument.
//
// ctx->r  = eval result of previous application.
static void cont_map(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 6);
  lbm_value ls  = sptr[0];
  lbm_value env = sptr[1];
  lbm_value t   = sptr[3]; // known cons!
  lbm_ref_cell(t)->car = ctx->r;
  //lbm_set_car(t, ctx->r); // update car field tailmost position.
  if (lbm_is_cons(ls)) {
    lbm_cons_t *cell = lbm_ref_cell(ls); // already checked that cons.
    lbm_value next = cell->car;
    lbm_value rest = cell->cdr;
    sptr[0] = rest;
    stack_reserve(ctx,1)[0] = MAP;
    lbm_ref_cell(sptr[5])->car = next; // update known cons
    //lbm_set_car(sptr[5], next); // new arguments

    lbm_value elt = cons_with_gc(ENC_SYM_NIL, ENC_SYM_NIL, ENC_SYM_NIL);
    lbm_ref_cell(t)->cdr = elt;
    //lbm_set_cdr(t, elt);
    sptr[3] = elt;  // (r1 ... rN . (nil . nil))
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
    lbm_value e = ctx->K.data[--ctx->K.sp];
    lbm_stack_drop(&ctx->K, 2);
    ctx->r = e;
    ctx->app_cont = true;
  } else {
    lbm_stack_drop(&ctx->K, 1);
    lbm_value body = ctx->K.data[--ctx->K.sp];
    lbm_value env = ctx->K.data[--ctx->K.sp];
    lbm_stack_drop(&ctx->K, 3);
    ctx->curr_env = env;
    ctx->curr_exp = body;
  }
}

static void cont_terminate(eval_context_t *ctx) {
  ERROR_CTX(ctx->r);
}

static void cont_loop(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 3);
  stack_reserve(ctx,1)[0] = LOOP_CONDITION;
  ctx->curr_env = sptr[2];
  ctx->curr_exp = sptr[1];
}

static void cont_loop_condition(eval_context_t *ctx) {
  if (lbm_is_symbol_nil(ctx->r)) {
    lbm_stack_drop(&ctx->K, 3);
    ctx->app_cont = true;  // A loop returns nil? Makes sense to me... but in general?
    return;
  }
  lbm_value *sptr = get_stack_ptr(ctx, 3);
  stack_reserve(ctx,1)[0] = LOOP;
  ctx->curr_env = sptr[2];
  ctx->curr_exp = sptr[0];
}

static void cont_loop_env_prep(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 3);
  sptr[2] = ctx->curr_env;
  stack_reserve(ctx,1)[0] = LOOP_CONDITION;
  ctx->curr_exp = sptr[1];
}
 
static void cont_merge_rest(eval_context_t *ctx) {
  lbm_uint *sptr = get_stack_ptr(ctx, 9);

  // If comparator returns true (result is in ctx->r):
  //   "a" should be moved to the last element position in merged list.
  //   A new element from "a_rest" should be moved into comparator argument 1 pos.
  // else
  //   "b" should be moved to last element position in merged list.
  //   A new element from "b_rest" should be moved into comparator argument 2 pos.
  //
  // If a_rest or b_rest is NIL:
  //   we are done, the remaining elements of
  //   non_nil list should be appended to merged list.
  // else
  //   Set up for a new comparator evaluation and recurse.
  lbm_value a = sptr[2];
  lbm_value b = lbm_cdr(a);
  lbm_set_cdr(a, ENC_SYM_NIL); // terminate 1 element list

  if (ctx->r == ENC_SYM_NIL) { // Comparison false

    if (sptr[0] == ENC_SYM_NIL) {
      sptr[0] = b;
      sptr[1] = b;
    } else {
      lbm_set_cdr(sptr[1], b);
      sptr[1] = b;
    }
    if (sptr[4] == ENC_SYM_NIL) {
      lbm_set_cdr(a, sptr[3]);
      lbm_set_cdr(sptr[1], a);
      ctx->r = sptr[0];
      lbm_stack_drop(&ctx->K, 9);
      ctx->app_cont = true;
      return;
    } else {
      b = sptr[4];
      sptr[4] = lbm_cdr(sptr[4]);
      lbm_set_cdr(b, ENC_SYM_NIL);
    }
  } else {
    if (sptr[0] == ENC_SYM_NIL) {
      sptr[0] = a;
      sptr[1] = a;
    } else {
      lbm_set_cdr(sptr[1], a);
      sptr[1] = a;
    }

    if (sptr[3] == ENC_SYM_NIL) {
      lbm_set_cdr(b, sptr[4]);
      lbm_set_cdr(sptr[1], b);
      ctx->r = sptr[0];
      lbm_stack_drop(&ctx->K, 9);
      ctx->app_cont = true;
      return;
    } else {
      a = sptr[3];
      sptr[3] = lbm_cdr(sptr[3]);
      lbm_set_cdr(a, ENC_SYM_NIL);
    }
  }
  lbm_set_cdr(a, b);
  sptr[2] = a;

  lbm_value par1 = sptr[7];
  lbm_value par2 = sptr[8];
  lbm_value cmp_body = sptr[5];
  lbm_value cmp_env = sptr[6];
  // Environment should be preallocated already at this point
  // and the operations below should never need GC.
  lbm_value new_env0 = lbm_env_set(cmp_env, par1, lbm_car(a));
  lbm_value new_env = lbm_env_set(new_env0, par2, lbm_car(b));
  if (lbm_is_symbol(new_env0) || lbm_is_symbol(new_env)) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  }
  cmp_env = new_env;

  stack_reserve(ctx,1)[0] = MERGE_REST;
  ctx->curr_exp = cmp_body;
  ctx->curr_env = cmp_env;
}

// merge_layer stack contents
// s[sp-9] = cmp
// s[sp-8] = cmp_env
// s[sp-7] = par1
// s[sp-6] = par2
// s[sp-5] = acc - first cell
// s[sp-4] = acc - last cell
// s[sp-3] = rest;
// s[sp-2] = layer
// s[sp-1] = length or original list
//
// ctx->r merged sublist
static void cont_merge_layer(eval_context_t *ctx) {
  lbm_uint *sptr = get_stack_ptr(ctx, 9);
  lbm_int layer = lbm_dec_i(sptr[7]);
  lbm_int len = lbm_dec_i(sptr[8]);

  lbm_value r_curr = ctx->r;
  while (lbm_is_cons(r_curr)) {
    lbm_value next = lbm_ref_cell(r_curr)->cdr;
    if (next == ENC_SYM_NIL) {
      break;
    }
    r_curr = next;
  }

  if (sptr[4] == ENC_SYM_NIL) {
    sptr[4] = ctx->r;
    sptr[5] = r_curr;
  } else {
    lbm_set_cdr(sptr[5], ctx->r); // accumulate merged sublists.
    sptr[5] = r_curr;
  }

  lbm_value layer_rest = sptr[6];
  // switch layer or done ?
  if (layer_rest == ENC_SYM_NIL) {
    if (layer * 2 >= len) {
      ctx->r = sptr[4];
      ctx->app_cont = true;
      lbm_stack_drop(&ctx->K, 9);
      return;
    } else {
      // Setup for merges of the next layer
      layer = layer * 2;
      sptr[7] = lbm_enc_i(layer);
      layer_rest = sptr[4]; // continue on the accumulation of all sublists.
      sptr[5] = ENC_SYM_NIL;
      sptr[4] = ENC_SYM_NIL;
    }
  }
  // merge another sublist based on current layer.
  lbm_value a_list = layer_rest;
  // build sublist a
  lbm_value curr = layer_rest;
  for (int i = 0; i < layer-1; i ++) {
    if (lbm_is_cons(curr)) {
      curr = lbm_ref_cell(curr)->cdr;
    } else {
      break;
    }
  }
  layer_rest = lbm_cdr(curr);
  lbm_set_cdr(curr, ENC_SYM_NIL); //terminate sublist.

  lbm_value b_list = layer_rest;
  // build sublist b
  curr = layer_rest;
  for (int i = 0; i < layer-1; i ++) {
    if (lbm_is_cons(curr)) {
      curr = lbm_ref_cell(curr)->cdr;
    } else {
      break;
    }
  }
  layer_rest = lbm_cdr(curr);
  lbm_set_cdr(curr, ENC_SYM_NIL); //terminate sublist.

  sptr[6] = layer_rest;

  if (b_list == ENC_SYM_NIL) {
    stack_reserve(ctx,1)[0] = MERGE_LAYER;
    ctx->r = a_list;
    ctx->app_cont = true;
    return;
  }
  // Set up for a merge of sublists.

  lbm_value a_rest = lbm_cdr(a_list);
  lbm_value b_rest = lbm_cdr(b_list);
  lbm_value a = a_list;
  lbm_value b = b_list;
  lbm_set_cdr(a, b);
  // Terminating the b list would be incorrect here
  // if there was any chance that the environment update below
  // performs GC.
  lbm_set_cdr(b, ENC_SYM_NIL);

  lbm_value cmp_body = sptr[0];
  lbm_value cmp_env = sptr[1];
  lbm_value par1 = sptr[2];
  lbm_value par2 = sptr[3];
  // Environment should be preallocated already at this point
  // and the operations below should never need GC.
  lbm_value new_env0 = lbm_env_set(cmp_env, par1, lbm_car(a));
  lbm_value new_env = lbm_env_set(cmp_env, par2, lbm_car(b));
  if (lbm_is_symbol(new_env0) || lbm_is_symbol(new_env)) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  }
  cmp_env = new_env;

  lbm_uint *merge_cont = stack_reserve(ctx, 11);
  merge_cont[0] = MERGE_LAYER;
  merge_cont[1] = ENC_SYM_NIL;
  merge_cont[2] = ENC_SYM_NIL;
  merge_cont[3] = a;
  merge_cont[4] = a_rest;
  merge_cont[5] = b_rest;
  merge_cont[6] = cmp_body;
  merge_cont[7] = cmp_env;
  merge_cont[8] = par1;
  merge_cont[9] = par2;
  merge_cont[10] = MERGE_REST;
  ctx->curr_exp = cmp_body;
  ctx->curr_env = cmp_env;
  return;
}

/****************************************************/
/*   READER                                         */

static void read_finish(lbm_char_channel_t *str, eval_context_t *ctx) {

  /* Tokenizer reached "end of file"
     The parser could be in a state where it needs
     more tokens to correctly finish an expression.

     Four cases
     1. The program / expression is malformed and the context should die.
     2. We are finished reading a program and should close off the
     internal representation with a closing parenthesis. Then
     apply continuation.
     3. We are finished reading an expression and should
     apply the continuation
     4. We are finished read-and-evaluating

     In case 2, we should find the READ_DONE at sp - 5.
     In case 3, we should find the READ_DONE at sp - 1.
     In case 4, we should find the READ_DONE at sp - 4.

     case 3 should not end up here, but rather end up in
     cont_read_done.
  */

  if (lbm_is_symbol(ctx->r)) {
    lbm_uint sym_val = lbm_dec_sym(ctx->r);
    if (sym_val >= TOKENIZER_SYMBOLS_START &&
        sym_val <= TOKENIZER_SYMBOLS_END) {
      READ_ERROR_CTX(lbm_channel_row(str), lbm_channel_column(str));
    }
  }

  if (ctx->K.sp > 4  && (ctx->K.data[ctx->K.sp - 4] == READ_DONE) &&
      (ctx->K.data[ctx->K.sp - 5] == READING_PROGRAM_INCREMENTALLY)) {
    /* read and evaluate is done */
    --ctx->K.sp; // Pop but do not use
    lbm_value env = ctx->K.data[--ctx->K.sp];
    --ctx->K.sp; // Pop but do not use
    ctx->curr_env = env;
    ctx->app_cont = true; // Program evaluated and result is in ctx->r.
  } else if (ctx->K.sp > 5 && (ctx->K.data[ctx->K.sp - 5] == READ_DONE) &&
             (ctx->K.data[ctx->K.sp - 6] == READING_PROGRAM)) {
    /* successfully finished reading a program  (CASE 2) */
    ctx->r = ENC_SYM_CLOSEPAR;
    ctx->app_cont = true;
  } else {
    if (lbm_channel_row(str) == 1 && lbm_channel_column(str) == 1) {
      // (read "") evaluates to nil.
      ctx->r = ENC_SYM_NIL;
      ctx->app_cont = true;
    } else {
      lbm_channel_reader_close(str);
      lbm_set_error_reason((char*)lbm_error_str_parse_eof);
      READ_ERROR_CTX(lbm_channel_row(str), lbm_channel_column(str));
    }
  }
}

/* cont_read_next_token
   sp-2 : Stream
   sp-1 : Grab row
*/
static void cont_read_next_token(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 2);
  lbm_value stream = sptr[0];
  lbm_value grab_row0 = sptr[1];

  lbm_char_channel_t *chan = lbm_dec_channel(stream);
  if (chan == NULL || chan->state == NULL) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
    return; // INFER does not understant that error_ctx longjmps
            // out of this function.
  }

  if (!lbm_channel_more(chan) && lbm_channel_is_empty(chan)) {
    lbm_stack_drop(&ctx->K, 2);
    read_finish(chan, ctx);
    return;
  }
  /* Eat whitespace and comments */
  if (!tok_clean_whitespace(chan)) {
    sptr[0] = stream;
    sptr[1] = lbm_enc_u(0);
    stack_reserve(ctx,1)[0] = READ_NEXT_TOKEN;
    yield_ctx(EVAL_CPS_MIN_SLEEP);
    return;
  }
  /* After eating whitespace we may be at end of file/stream */
  if (!lbm_channel_more(chan) && lbm_channel_is_empty(chan)) {
    lbm_stack_drop(&ctx->K, 2);
    read_finish(chan, ctx);
    return;
  }

  if (lbm_dec_u(grab_row0)) {
    ctx->row0 = (int32_t)lbm_channel_row(chan);
    ctx->row1 = -1; // a new start, end is unknown
  }

  /* Attempt to extract tokens from the character stream */
  int n = 0;
  lbm_value res = ENC_SYM_NIL;
  unsigned int string_len = 0;

  /*
   * SYNTAX
   */
  uint32_t tok_match;
  n = tok_syntax(chan, &tok_match);
  if (n > 0) {
    if (!lbm_channel_drop(chan, (unsigned int)n)) {
      ERROR_CTX(ENC_SYM_FATAL_ERROR);
    }
    lbm_value compound_read_start = READ_START_BYTEARRAY;
    lbm_value compound_value_opener = ENC_SYM_OPENBRACK;
    lbm_value compound_value_closer = ENC_SYM_CLOSEBRACK;
    ctx->app_cont = true;
    switch(tok_match) {
    case TOKOPENPAR: {
      sptr[0] = ENC_SYM_NIL;
      sptr[1] = ENC_SYM_NIL;
      lbm_value *rptr = stack_reserve(ctx,5);
      rptr[0] = stream;
      rptr[1] = READ_APPEND_CONTINUE;
      rptr[2] = stream;
      rptr[3] = lbm_enc_u(0);
      rptr[4] = READ_NEXT_TOKEN;
      ctx->r = ENC_SYM_OPENPAR;
    } return;
    case TOKCLOSEPAR: {
      lbm_stack_drop(&ctx->K, 2);
      ctx->r = ENC_SYM_CLOSEPAR;
    } return;
    case TOKOPENARRAY:
      compound_read_start = READ_START_ARRAY; // switch to array reader
      compound_value_opener = ENC_SYM_OPENARRAY; /* fall through */
    case TOKOPENBRACK: {
      sptr[0] = stream;
      sptr[1] = compound_read_start;
      lbm_value *rptr = stack_reserve(ctx, 3);
      rptr[0] = stream;
      rptr[1] = lbm_enc_u(0);
      rptr[2] = READ_NEXT_TOKEN;
      ctx->r = compound_value_opener;
    } return;
    case TOKCLOSEARRAY:
      compound_value_closer = ENC_SYM_CLOSEARRAY; /* fall through */
    case TOKCLOSEBRACK:
      lbm_stack_drop(&ctx->K, 2);
      ctx->r = compound_value_closer;
      return;
    case TOKDOT:
      lbm_stack_drop(&ctx->K, 2);
      ctx->r = ENC_SYM_DOT;
      return;
    case TOKDONTCARE:
      lbm_stack_drop(&ctx->K, 2);
      ctx->r = ENC_SYM_DONTCARE;
      return;
    case TOKQUOTE:
      sptr[0] = ENC_SYM_QUOTE;
      sptr[1] = WRAP_RESULT;
      break;
    case TOKBACKQUOTE: {
      sptr[0] = QQ_EXPAND_START;
      sptr[1] = stream;
      lbm_value *rptr = stack_reserve(ctx, 2);
      rptr[0] = lbm_enc_u(0);
      rptr[1] = READ_NEXT_TOKEN;
      ctx->app_cont = true;
    } return;
    case TOKCOMMAAT:
      sptr[0] = ENC_SYM_COMMAAT;
      sptr[1] = WRAP_RESULT;
      break;
    case TOKCOMMA:
      sptr[0] = ENC_SYM_COMMA;
      sptr[1] = WRAP_RESULT;
      break;
    case TOKMATCHANY:
      lbm_stack_drop(&ctx->K, 2);
      ctx->r = ENC_SYM_MATCH_ANY;
      return;
    case TOKOPENCURL: {
      sptr[0] = ENC_SYM_NIL;
      sptr[1] = ENC_SYM_NIL;
      lbm_value *rptr = stack_reserve(ctx,2);
      rptr[0] = stream;
      rptr[1] = READ_APPEND_CONTINUE;
      ctx->r = ENC_SYM_PROGN;
    } return;
    case TOKCLOSECURL:
      lbm_stack_drop(&ctx->K, 2);
      ctx->r = ENC_SYM_CLOSEPAR;
      return;
    case TOKCONSTSTART: /* fall through */
    case TOKCONSTEND: {
      if (tok_match == TOKCONSTSTART)  ctx->flags |= EVAL_CPS_CONTEXT_FLAG_CONST;
      if (tok_match == TOKCONSTEND)    ctx->flags &= ~EVAL_CPS_CONTEXT_FLAG_CONST;
      sptr[0] = stream;
      sptr[1] = lbm_enc_u(0);
      stack_reserve(ctx,1)[0] = READ_NEXT_TOKEN;
      ctx->app_cont = true;
    } return;
    default:
      READ_ERROR_CTX(lbm_channel_row(chan), lbm_channel_column(chan));
    }
    // read next token
    lbm_value *rptr = stack_reserve(ctx, 3);
    rptr[0] = stream;
    rptr[1] = lbm_enc_u(0);
    rptr[2] = READ_NEXT_TOKEN;
    ctx->app_cont = true;
    return;
  } else if (n < 0) goto retry_token;

  /*
   *  STRING
   */
  n = tok_string(chan, &string_len);
  if (n >= 2) {
    lbm_channel_drop(chan, (unsigned int)n);
#ifdef LBM_ALWAYS_GC
    gc();
#endif
    if (!lbm_heap_allocate_array(&res, (unsigned int)(string_len+1))) {
      gc();
      lbm_heap_allocate_array(&res, (unsigned int)(string_len+1));
    }
    if (lbm_is_ptr(res)) {
      lbm_array_header_t *arr = assume_array(res);
      char *data = (char*)arr->data;
      memset(data,0, string_len + 1);
      memcpy(data, tokpar_sym_str, string_len);
      lbm_stack_drop(&ctx->K, 2);
      ctx->r = res;
      ctx->app_cont = true;
      return;
    } else {
      ERROR_CTX(ENC_SYM_MERROR);
    }
  } else if (n < 0) goto retry_token;

  /*
   * FLOAT
   */
  token_float f_val;
  n = tok_double(chan, &f_val);
  if (n > 0) {
    lbm_channel_drop(chan, (unsigned int) n);
    switch(f_val.type) {
    case TOKTYPEF32:
      WITH_GC(res, lbm_enc_float((float)f_val.value));
      break;
    case TOKTYPEF64:
      res = lbm_enc_double(f_val.value);
      break;
    default:
      READ_ERROR_CTX(lbm_channel_row(chan), lbm_channel_column(chan));
    }
    lbm_stack_drop(&ctx->K, 2);
    ctx->r = res;
    ctx->app_cont = true;
    return;
  } else if (n < 0) goto retry_token;

  /*
   * INTEGER
   */
  token_int int_result;
  n = tok_integer(chan, &int_result);
  if (n > 0) {
    lbm_channel_drop(chan, (unsigned int)n);
    switch(int_result.type) {
    case TOKTYPEBYTE:
      res = lbm_enc_char((uint8_t)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOKTYPEI:
      res = lbm_enc_i((lbm_int)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOKTYPEU:
      res = lbm_enc_u((lbm_uint)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOKTYPEI32:
      WITH_GC(res, lbm_enc_i32((int32_t)(int_result.negative ? -int_result.value : int_result.value)));
      break;
    case TOKTYPEU32:
      WITH_GC(res,lbm_enc_u32((uint32_t)(int_result.negative ? -int_result.value : int_result.value)));
      break;
    case TOKTYPEI64:
      WITH_GC(res,lbm_enc_i64((int64_t)(int_result.negative ? -int_result.value : int_result.value)));
      break;
    case TOKTYPEU64:
      WITH_GC(res,lbm_enc_u64((uint64_t)(int_result.negative ? -int_result.value : int_result.value)));
      break;
    default:
      READ_ERROR_CTX(lbm_channel_row(chan), lbm_channel_column(chan));
    }
    lbm_stack_drop(&ctx->K, 2);
    ctx->r = res;
    ctx->app_cont = true;
    return;
  } else if (n < 0) goto retry_token;

  /*
   * SYMBOL
   */
  n = tok_symbol(chan);
  if (n > 0) {
    lbm_channel_drop(chan, (unsigned int) n);
    lbm_uint symbol_id;
    if (!lbm_get_symbol_by_name(tokpar_sym_str, &symbol_id)) {
      int r = 0;
      if (n > 4 &&
          tokpar_sym_str[0] == 'e' &&
          tokpar_sym_str[1] == 'x' &&
          tokpar_sym_str[2] == 't' &&
          tokpar_sym_str[3] == '-') {
        lbm_uint ext_id;
        lbm_uint ext_name_len = (lbm_uint)n + 1;
#ifdef LBM_ALWAYS_GC
        gc();
#endif
        char *ext_name = lbm_malloc(ext_name_len);
        if (!ext_name) {
          gc();
          ext_name = lbm_malloc(ext_name_len);
        }
        if (ext_name) {
          memcpy(ext_name, tokpar_sym_str, ext_name_len);
          r = lbm_add_extension(ext_name, lbm_extensions_default);
          if (!lbm_lookup_extension_id(ext_name, &ext_id)) {
            ERROR_CTX(ENC_SYM_FATAL_ERROR);
          }
          symbol_id = ext_id;
        } else {
          ERROR_CTX(ENC_SYM_MERROR);
        }
      } else {
        r = lbm_add_symbol_base(tokpar_sym_str, &symbol_id);
      }
      if (!r) {
        READ_ERROR_CTX(lbm_channel_row(chan), lbm_channel_column(chan));
      }
    }
    lbm_stack_drop(&ctx->K, 2);
    ctx->r = lbm_enc_sym(symbol_id);
    ctx->app_cont = true;
    return;
  } else if (n == TOKENIZER_NEED_MORE) {
    goto retry_token;
  } else if (n <= TOKENIZER_STRING_ERROR) {
    READ_ERROR_CTX(lbm_channel_row(chan), lbm_channel_column(chan));
  }

  /*
   * CHAR
   */
  char c_val;
  n = tok_char(chan, &c_val);
  if(n > 0) {
    lbm_channel_drop(chan,(unsigned int) n);
    lbm_stack_drop(&ctx->K, 2);
    ctx->r = lbm_enc_char((uint8_t)c_val);
    ctx->app_cont = true;
    return;
  }else if (n < 0) goto retry_token;

  READ_ERROR_CTX(lbm_channel_row(chan), lbm_channel_column(chan));

 retry_token:
  if (n == TOKENIZER_NEED_MORE) {
    sptr[0] = stream;
    sptr[1] = lbm_enc_u(0);
    stack_reserve(ctx,1)[0] = READ_NEXT_TOKEN;
    yield_ctx(EVAL_CPS_MIN_SLEEP);
    return;
  }
  READ_ERROR_CTX(lbm_channel_row(chan), lbm_channel_column(chan));
}

static void cont_read_start_bytearray(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 1);
  lbm_value stream = sptr[0];

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
    return; // INFER does not understand that error_ctx longjmps out
            // of this function here.
  }
  if (ctx->r == ENC_SYM_CLOSEBRACK) {
    lbm_value array;

    if (!lbm_heap_allocate_array(&array, 0)) {
      gc();
      if (!lbm_heap_allocate_array(&array, 0)) {
        lbm_set_error_reason((char*)lbm_error_str_read_no_mem);
        lbm_channel_reader_close(str);
        ERROR_CTX(ENC_SYM_FATAL_ERROR); // Terminates ctx
      }
    }
    lbm_stack_drop(&ctx->K, 1);
    ctx->r = array;
    ctx->app_cont = true;
  } else if (lbm_is_number(ctx->r)) {
#ifdef LBM_ALWAYS_GC
    gc();
#endif
    lbm_uint num_free = lbm_memory_longest_free();
    lbm_uint initial_size = (lbm_uint)((float)num_free * 0.9);
    if (initial_size == 0) {
      gc();
      num_free = lbm_memory_longest_free();
      initial_size = (lbm_uint)((float)num_free * 0.9);
      if (initial_size == 0) {
        lbm_channel_reader_close(str);
        ERROR_CTX(ENC_SYM_MERROR);
      }
    }
    lbm_value array;
    initial_size = sizeof(lbm_uint) * initial_size;

    // Keep in mind that this allocation can fail for both
    // lbm_memory and heap reasons.
    if (!lbm_heap_allocate_array(&array, initial_size)) {
      gc();
      if (!lbm_heap_allocate_array(&array, initial_size)) {
        lbm_set_error_reason((char*)lbm_error_str_read_no_mem);
        lbm_channel_reader_close(str);
        ERROR_CTX(ENC_SYM_FATAL_ERROR);
        // NOTE: If array is not created evaluation ends here.
        // Static analysis seems unaware.
      }
    }

    sptr[0] = array;
    lbm_value *rptr = stack_reserve(ctx, 4);
    rptr[0] = lbm_enc_u(initial_size);
    rptr[1] = lbm_enc_u(0);
    rptr[2] = stream;
    rptr[3] = READ_APPEND_BYTEARRAY;
    ctx->app_cont = true;
  } else {
    lbm_channel_reader_close(str);
    READ_ERROR_CTX(lbm_channel_row(str), lbm_channel_column(str));
  }
}

static void cont_read_append_bytearray(eval_context_t *ctx) {
  lbm_uint *sptr = get_stack_ptr(ctx, 4);

  lbm_value array  = sptr[0];
  lbm_value size   = lbm_dec_as_u32(sptr[1]);
  lbm_value ix     = lbm_dec_as_u32(sptr[2]);
  lbm_value stream = sptr[3];

  if (ix >= (size - 1)) {
    ERROR_CTX(ENC_SYM_MERROR);
  }

  // if sptr[0] is not an array something is very very wrong.
  // Not robust against a garbage on stack. But how would garbage get onto stack?
  lbm_array_header_t *arr = assume_array(array);
  if (lbm_is_number(ctx->r)) {
    ((uint8_t*)arr->data)[ix] = (uint8_t)lbm_dec_as_u32(ctx->r);

    sptr[2] = lbm_enc_u(ix + 1);
    lbm_value *rptr = stack_reserve(ctx, 4);
    rptr[0] = READ_APPEND_BYTEARRAY;
    rptr[1] = stream;
    rptr[2] = lbm_enc_u(0);
    rptr[3] = READ_NEXT_TOKEN;
    ctx->app_cont = true;
  } else if (lbm_is_symbol(ctx->r) && ctx->r == ENC_SYM_CLOSEBRACK) {
    lbm_uint array_size = ix / sizeof(lbm_uint);

    if (ix % sizeof(lbm_uint) != 0) {
      array_size = array_size + 1;
    }
    lbm_memory_shrink((lbm_uint*)arr->data, array_size);
    arr->size = ix;
    lbm_stack_drop(&ctx->K, 4);
    ctx->r = array;
    ctx->app_cont = true;
  } else {
    ERROR_CTX(ENC_SYM_TERROR);
  }
}

// Lisp array syntax reading ////////////////////////////////////////

static void cont_read_start_array(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 1);
  lbm_value stream = sptr[0];

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
    return; // INFER does not understand that error_ctx longjmps out
            // of this function here.
  }
  if (ctx->r == ENC_SYM_CLOSEARRAY) {
    lbm_value array;

    if (!lbm_heap_allocate_lisp_array(&array, 0)) {
      gc();
      if (!lbm_heap_allocate_lisp_array(&array, 0)) {
        lbm_set_error_reason((char*)lbm_error_str_read_no_mem);
        lbm_channel_reader_close(str);
        ERROR_CTX(ENC_SYM_FATAL_ERROR); // Terminates ctx
      }
    }
    lbm_stack_drop(&ctx->K, 1);
    ctx->r = array;
    ctx->app_cont = true;
  } else {
#ifdef LBM_ALWAYS_GC
    gc();
#endif
    lbm_uint num = ((lbm_uint)((float)lbm_memory_longest_free() * 0.9) / sizeof(lbm_uint)) ;
    lbm_uint initial_size = (lbm_uint)num;
    if (initial_size == 0) {
      gc();
      num = ((lbm_uint)((float)lbm_memory_longest_free() * 0.9) / sizeof(lbm_uint)) ;
      initial_size = (lbm_uint)num;
      if (initial_size == 0) {
        lbm_channel_reader_close(str);
        ERROR_CTX(ENC_SYM_MERROR);
      }
    }
    lbm_value array;
    initial_size = sizeof(lbm_uint) * initial_size;

    if (!lbm_heap_allocate_lisp_array(&array, initial_size)) {
      gc();
      if (!lbm_heap_allocate_lisp_array(&array, initial_size)) {
        lbm_set_error_reason((char*)lbm_error_str_read_no_mem);
        lbm_channel_reader_close(str);
        ERROR_CTX(ENC_SYM_FATAL_ERROR);
      }
    }

    sptr[0] = array;
    lbm_value *rptr = stack_reserve(ctx, 4);
    rptr[0] = lbm_enc_u(initial_size);
    rptr[1] = lbm_enc_u(0);
    rptr[2] = stream;
    rptr[3] = READ_APPEND_ARRAY;
    ctx->app_cont = true;
  }
}

static void cont_read_append_array(eval_context_t *ctx) {
  lbm_uint *sptr = get_stack_ptr(ctx, 4);

  lbm_value array  = sptr[0];
  lbm_value size   = lbm_dec_as_u32(sptr[1]);
  lbm_value ix     = lbm_dec_as_u32(sptr[2]);
  lbm_value stream = sptr[3];

  if (ix >= (size - 1)) {
    ERROR_CTX(ENC_SYM_MERROR);
  }

  // if sptr[0] is not an array something is very very wrong.
  // Not robust against a garbage on stack. But how would garbage get onto stack?
  lbm_array_header_t *arr = assume_array(array);
  if (lbm_is_symbol(ctx->r) && ctx->r == ENC_SYM_CLOSEARRAY) {
    lbm_uint array_size = ix;

    if (ix % sizeof(lbm_uint) != 0) {
      array_size = array_size + 1;
    }
    lbm_memory_shrink((lbm_uint*)arr->data, array_size);
    arr->size = ix * sizeof(lbm_uint);
    lbm_stack_drop(&ctx->K, 4);
    ctx->r = array;
    ctx->app_cont = true;
  } else {
    ((lbm_uint*)arr->data)[ix] = ctx->r;

    sptr[2] = lbm_enc_u(ix + 1);
    lbm_value *rptr = stack_reserve(ctx, 4);
    rptr[0] = READ_APPEND_ARRAY;
    rptr[1] = stream;
    rptr[2] = lbm_enc_u(0);
    rptr[3] = READ_NEXT_TOKEN;
    ctx->app_cont = true;
  }
}

// Lisp list syntax reading ////////////////////////////////////////

static void cont_read_append_continue(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 3);

  lbm_value first_cell = sptr[0];
  lbm_value last_cell  = sptr[1];
  lbm_value stream     = sptr[2];

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
    return; // INFER does not understand that execution
            // jumps out on error_ctx with a longjmp.
  }

  if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL) {

    switch(ctx->r) {
    case ENC_SYM_CLOSEPAR:
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
    case ENC_SYM_DOT: {
      lbm_value *rptr = stack_reserve(ctx, 4);
      rptr[0] = READ_DOT_TERMINATE;
      rptr[1] = stream;
      rptr[2] = lbm_enc_u(0);
      rptr[3] = READ_NEXT_TOKEN;
      ctx->app_cont = true;
    } return;
    }
  }
  lbm_value new_cell = cons_with_gc(ctx->r, ENC_SYM_NIL, ENC_SYM_NIL);
  // Does not return if merror. So we cannot get a read-error here
  // unless we write the a version of cons_with_gc here.
  //if (lbm_is_symbol_merror(new_cell)) {
  //  lbm_channel_reader_close(str);
  //  read_error_ctx(lbm_channel_row(str), lbm_channel_column(str));
  //  return;
  //}
  if (lbm_type_of(last_cell) == LBM_TYPE_CONS) {
    lbm_set_cdr(last_cell, new_cell);
    last_cell = new_cell;
  } else {
    first_cell = last_cell = new_cell;
  }
  sptr[0] = first_cell;
  sptr[1] = last_cell;
  //sptr[2] = stream;    // unchanged.
  lbm_value *rptr = stack_reserve(ctx, 4);
  rptr[0] = READ_APPEND_CONTINUE;
  rptr[1] = stream;
  rptr[2] = lbm_enc_u(0);
  rptr[3] = READ_NEXT_TOKEN;
  ctx->app_cont = true;
}

static void cont_read_eval_continue(eval_context_t *ctx) {
  lbm_value env;
  lbm_value stream;
  lbm_value *sptr = get_stack_ptr(ctx, 2);
  env = sptr[1];
  stream = sptr[0];
  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str && str->state) {
    ctx->row1 = (lbm_int)str->row(str);
    if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL) {
      switch(ctx->r) {
      case ENC_SYM_CLOSEPAR:
        lbm_stack_drop(&ctx->K, 2);
        ctx->app_cont = true;
        return;
      case ENC_SYM_DOT:
        // A dot here is a syntax error.
        lbm_set_error_reason((char*)lbm_error_str_parse_dot);
        READ_ERROR_CTX(lbm_channel_row(str),lbm_channel_column(str));
        return;
      }
    }
    lbm_value *rptr = stack_reserve(ctx, 6);
    rptr[0] = READ_EVAL_CONTINUE;
    rptr[1] = stream;
    rptr[2] = lbm_enc_u(1);
    rptr[3] = READ_NEXT_TOKEN;
    rptr[4] = lbm_enc_u(ctx->flags);
    rptr[5] = POP_READER_FLAGS;

    ctx->curr_env = env;
    ctx->curr_exp = ctx->r;
  } else {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  }
}

static void cont_read_expect_closepar(eval_context_t *ctx) {
  lbm_value res = ctx->K.data[--ctx->K.sp];
  lbm_value stream = ctx->K.data[--ctx->K.sp];

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) { // TODO: De Morgan these conditions.
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  } else {
    if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL &&
        ctx->r == ENC_SYM_CLOSEPAR) {
      ctx->r = res;
      ctx->app_cont = true;
    } else {
      lbm_channel_reader_close(str);
      lbm_set_error_reason((char*)lbm_error_str_parse_close);
      READ_ERROR_CTX(lbm_channel_row(str), lbm_channel_column(str));
    }
  }
}

static void cont_read_dot_terminate(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 3);

  lbm_value last_cell  = sptr[1];
  lbm_value stream = sptr[2];

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  } else if (lbm_type_of(ctx->r) == LBM_TYPE_SYMBOL &&
             (ctx->r == ENC_SYM_CLOSEPAR ||
              ctx->r == ENC_SYM_DOT)) {
    lbm_channel_reader_close(str);
    lbm_set_error_reason((char*)lbm_error_str_parse_dot);
    READ_ERROR_CTX(lbm_channel_row(str), lbm_channel_column(str));
  } else if (lbm_is_cons(last_cell)) {
    lbm_ref_cell(last_cell)->cdr = ctx->r;
    //lbm_set_cdr(last_cell, ctx->r); 
    ctx->r = sptr[0]; // first cell
    lbm_value *rptr = stack_reserve(ctx, 3);
    sptr[0] = stream;
    sptr[1] = ctx->r;
    sptr[2] = READ_EXPECT_CLOSEPAR;
    rptr[0] = stream;
    rptr[1] = lbm_enc_u(0);
    rptr[2] = READ_NEXT_TOKEN;
    ctx->app_cont = true;
  } else {
    lbm_channel_reader_close(str);
    lbm_set_error_reason((char*)lbm_error_str_parse_dot);
    READ_ERROR_CTX(lbm_channel_row(str), lbm_channel_column(str));
  }
}

static void cont_read_done(eval_context_t *ctx) {
  //lbm_value reader_mode = ctx->K.data[--ctx->K.sp];
  --ctx->K.sp;
  lbm_value stream = ctx->K.data[--ctx->K.sp];
  lbm_value f_val = ctx->K.data[--ctx->K.sp];

  uint32_t flags = lbm_dec_as_u32(f_val);
  ctx->flags &= ~EVAL_CPS_CONTEXT_READER_FLAGS_MASK;
  ctx->flags |= (flags & EVAL_CPS_CONTEXT_READER_FLAGS_MASK);

  lbm_char_channel_t *str = lbm_dec_channel(stream);
  if (str == NULL || str->state == NULL) {
    ERROR_CTX(ENC_SYM_FATAL_ERROR);
  } else {
    // the "else" is there to make INFER understand
    // that this only happens if str is non-null.
    // the "else" is unnecessary though as
    // error_ctx longjmps out.
    lbm_channel_reader_close(str);
    if (lbm_is_symbol(ctx->r)) {
      lbm_uint sym_val = lbm_dec_sym(ctx->r);
      if (sym_val >= TOKENIZER_SYMBOLS_START &&
          sym_val <= TOKENIZER_SYMBOLS_END) {
        READ_ERROR_CTX(lbm_channel_row(str), lbm_channel_column(str));
      }
    }
    ctx->row0 = -1;
    ctx->row1 = -1;
    ctx->app_cont = true;
  }
}

static void cont_wrap_result(eval_context_t *ctx) {
  lbm_value cell;
  lbm_value wrapper = ctx->K.data[--ctx->K.sp];
  WITH_GC(cell, lbm_heap_allocate_list_init(2,
                                            wrapper,
                                            ctx->r));
  ctx->r = cell;
  ctx->app_cont = true;
}

// cont_application_start
//
// sptr[0] = env
// sptr[1] = args
//
// ctx->r  = function
static void cont_application_start(eval_context_t *ctx) {
  if (lbm_is_symbol(ctx->r)) {
    stack_reserve(ctx,1)[0] = lbm_enc_u(0);
    cont_application_args(ctx);
  } else if (lbm_is_cons(ctx->r)) {
    lbm_uint *sptr = get_stack_ptr(ctx, 2);
    lbm_value args = (lbm_value)sptr[1];
    switch (lbm_ref_cell(ctx->r)->car) { // Already checked that is_cons
    case ENC_SYM_CLOSURE: {
      lbm_value cl[3];
      extract_n(get_cdr(ctx->r), cl, 3);
      lbm_value arg_env = (lbm_value)sptr[0];
      lbm_value arg0, arg_rest;
      get_car_and_cdr(args, &arg0, &arg_rest);
      sptr[1] = cl[CLO_BODY];
      bool a_nil = lbm_is_symbol_nil(args);
      bool p_nil = lbm_is_symbol_nil(cl[CLO_PARAMS]);
      lbm_value *reserved = stack_reserve(ctx, 4);

      if (!a_nil && !p_nil) {
        reserved[0] = cl[CLO_ENV];
        reserved[1] = cl[CLO_PARAMS];
        reserved[2] = arg_rest;
        reserved[3] = CLOSURE_ARGS;
        ctx->curr_exp = arg0;
        ctx->curr_env = arg_env;
      } else if (a_nil && p_nil) {
        // No params, No args
        lbm_stack_drop(&ctx->K, 6);
        ctx->curr_exp = cl[CLO_BODY];
        ctx->curr_env = cl[CLO_ENV];
      } else if (p_nil) {
        reserved[1] = get_cdr(args);      // protect cdr(args) from allocate_binding
        ctx->curr_exp = get_car(args);    // protect car(args) from allocate binding
        ctx->curr_env = arg_env;
        lbm_value rest_binder = allocate_binding(ENC_SYM_REST_ARGS, ENC_SYM_NIL, cl[CLO_ENV]);
        reserved[0] = rest_binder;
        reserved[2] = get_car(rest_binder);
        reserved[3] = CLOSURE_ARGS_REST;
      } else {
        lbm_set_error_reason((char*)lbm_error_str_num_args);
        ERROR_AT_CTX(ENC_SYM_EERROR, ctx->r);
      }
    } break;
    case ENC_SYM_CONT:{  
      ctx->curr_exp = setup_cont(ctx, args);
    } break;
    case ENC_SYM_CONT_SP: {
      ctx->curr_exp = setup_cont_sp(ctx, args);
      return;
    } break;
    case ENC_SYM_MACRO:{
      lbm_value env = (lbm_value)sptr[0];
      pop_stack_ptr(ctx, 2);
      setup_macro(ctx, args, env);
    } break;
    default:
      ERROR_CTX(ENC_SYM_EERROR);
    }
  } else {
    lbm_set_error_reason(lbm_error_str_not_applicable);
    ERROR_AT_CTX(ENC_SYM_EERROR, ctx->r);
  }
}

static void cont_eval_r(eval_context_t* ctx) {
  lbm_value env = ctx->K.data[--ctx->K.sp];
  ctx->curr_exp = ctx->r;
  ctx->curr_env = env;
}

static void cont_progn_var(eval_context_t* ctx) {

  lbm_value key = ctx->K.data[--ctx->K.sp];
  lbm_value env = ctx->K.data[--ctx->K.sp];
  fill_binding_location(key, ctx->r, env);

  ctx->curr_env = env; // evaluating value may build upon local env.
  ctx->app_cont = true;
}

static void cont_setq(eval_context_t *ctx) {
  lbm_value sym = ctx->K.data[--ctx->K.sp];
  lbm_value env = ctx->K.data[--ctx->K.sp];
  lbm_value res;
  WITH_GC(res, perform_setvar(sym, ctx->r, env));
  ctx->r = res;
  ctx->app_cont = true;
}

lbm_flash_status request_flash_storage_cell(lbm_value val, lbm_value *res) {

  lbm_value flash_cell;
  lbm_flash_status s = lbm_allocate_const_cell(&flash_cell);
  if (s != LBM_FLASH_WRITE_OK)
    return s;
  lbm_value new_val = val;
  new_val &= ~LBM_PTR_VAL_MASK; // clear the value part of the ptr
  new_val |= (flash_cell & LBM_PTR_VAL_MASK);
  new_val |= LBM_PTR_TO_CONSTANT_BIT;
  *res = new_val;
  return s;
}

static void cont_move_to_flash(eval_context_t *ctx) {

  lbm_value args = ctx->K.data[--ctx->K.sp];

  if (lbm_is_symbol_nil(args)) {
    // Done looping over arguments. return true.
    ctx->r = ENC_SYM_TRUE;
    ctx->app_cont = true;
    return;
  }

  lbm_value first_arg, rest;
  get_car_and_cdr(args, &first_arg, &rest);

  lbm_value val;
  if (lbm_is_symbol(first_arg) && lbm_global_env_lookup(&val, first_arg)) {
    // Prepare to copy the rest of the arguments when done with first.
    lbm_value *rptr = stack_reserve(ctx, 2);
    rptr[0] = rest;
    rptr[1] = MOVE_TO_FLASH;
    if (lbm_is_ptr(val) &&
        (!(val & LBM_PTR_TO_CONSTANT_BIT))) {
      lbm_value * rptr1 = stack_reserve(ctx, 3);
      rptr1[0] = first_arg;
      rptr1[1] = SET_GLOBAL_ENV;
      rptr1[2] = MOVE_VAL_TO_FLASH_DISPATCH;
      ctx->r = val;
    }
    ctx->app_cont = true;
    return;
  }
  ERROR_CTX(ENC_SYM_EERROR);
}

static void cont_move_val_to_flash_dispatch(eval_context_t *ctx) {

  lbm_value val = ctx->r;

  if (lbm_is_cons(val)) { // non-constant cons-cell
    lbm_value *rptr = stack_reserve(ctx, 5);
    rptr[0] = ENC_SYM_NIL; // fst cell of list
    rptr[1] = ENC_SYM_NIL; // last cell of list
    rptr[2] = get_cdr(val);
    rptr[3] = MOVE_LIST_TO_FLASH;
    rptr[4] = MOVE_VAL_TO_FLASH_DISPATCH;
    ctx->r = lbm_ref_cell(val)->car; // already checked is_cons
    ctx->app_cont = true;
    return;
  }

  if (lbm_is_ptr(val) && (val & LBM_PTR_TO_CONSTANT_BIT)) { // constant pointer cons or not.
    //ctx->r unchanged
    ctx->app_cont = true;
    return;
  }

  if (lbm_is_ptr(val)) { // something that is not a cons but still a ptr type.
    lbm_cons_t *ref = lbm_ref_cell(val);
    if (lbm_type_of(ref->cdr) == LBM_TYPE_SYMBOL) {
      switch (ref->cdr) {
      case ENC_SYM_RAW_I_TYPE: /* fall through */
      case ENC_SYM_RAW_U_TYPE:
      case ENC_SYM_RAW_F_TYPE: {
        lbm_value flash_cell = ENC_SYM_NIL;
        handle_flash_status(request_flash_storage_cell(val, &flash_cell));
        handle_flash_status(write_const_car(flash_cell, ref->car));
        handle_flash_status(write_const_cdr(flash_cell, ref->cdr));
        ctx->r = flash_cell;
      } break;
      case ENC_SYM_IND_I_TYPE: /* fall through */
      case ENC_SYM_IND_U_TYPE:
      case ENC_SYM_IND_F_TYPE: {
#ifndef LBM64
        /* 64 bit values are in lbm mem on 32bit platforms. */
        lbm_uint *lbm_mem_ptr = (lbm_uint*)ref->car;
        lbm_uint flash_ptr;

        handle_flash_status(lbm_write_const_raw(lbm_mem_ptr, 2, &flash_ptr));
        lbm_value flash_cell = ENC_SYM_NIL;
        handle_flash_status(request_flash_storage_cell(val, &flash_cell));
        handle_flash_status(write_const_car(flash_cell, flash_ptr));
        handle_flash_status(write_const_cdr(flash_cell, ref->cdr));
        ctx->r = flash_cell;
#else
        // There are no indirect types in LBM64
        ERROR_CTX(ENC_SYM_FATAL_ERROR);
#endif
      } break;
      case ENC_SYM_LISPARRAY_TYPE: {
        lbm_array_header_t *arr = (lbm_array_header_t*)ref->car;
        lbm_uint size = arr->size / sizeof(lbm_uint);
        lbm_uint flash_addr = 0;
        lbm_value *arrdata = (lbm_value *)arr->data;
        lbm_value flash_cell = ENC_SYM_NIL;
        handle_flash_status(request_flash_storage_cell(val, &flash_cell));
        handle_flash_status(lbm_allocate_const_raw(size, &flash_addr));
        lift_array_flash(flash_cell,
                         false,
                         (char *)flash_addr,
                         arr->size);
        // Move array contents to flash recursively
        lbm_value *rptr = stack_reserve(ctx, 5);
        rptr[0] = flash_cell;
        rptr[1] = lbm_enc_u(0);
        rptr[2] = val;
        rptr[3] = MOVE_ARRAY_ELTS_TO_FLASH;
        rptr[4] = MOVE_VAL_TO_FLASH_DISPATCH;
        ctx->r = arrdata[0];
        ctx->app_cont = true;
        return;
      }
      case ENC_SYM_ARRAY_TYPE: {
        lbm_array_header_t *arr = (lbm_array_header_t*)ref->car;
        // arbitrary address: flash_arr.
        lbm_uint flash_arr = 0;
        handle_flash_status(lbm_write_const_array_padded((uint8_t*)arr->data, arr->size, &flash_arr));
        lbm_value flash_cell = ENC_SYM_NIL;
        handle_flash_status(request_flash_storage_cell(val, &flash_cell));
        lift_array_flash(flash_cell,
                         true,
                         (char *)flash_arr,
                         arr->size);
        ctx->r = flash_cell;
      } break;
      case ENC_SYM_CHANNEL_TYPE: /* fall through */
      case ENC_SYM_CUSTOM_TYPE:
        lbm_set_error_reason((char *)lbm_error_str_flash_not_possible);
        ERROR_CTX(ENC_SYM_EERROR);
      }
    } else {
      ERROR_CTX(ENC_SYM_FATAL_ERROR);
    }
    ctx->app_cont = true;
    return;
  }

  // if no condition matches, nothing happens (id).
  ctx->r = val;
  ctx->app_cont = true;
}

static void cont_move_list_to_flash(eval_context_t *ctx) {

  // ctx->r holds the value that should go in car

  lbm_value *sptr = get_stack_ptr(ctx, 3);

  lbm_value fst = sptr[0];
  lbm_value lst = sptr[1];
  lbm_value val = sptr[2];


  lbm_value new_lst = ENC_SYM_NIL;
  // Allocate element ptr storage after storing the element to flash.
  handle_flash_status(request_flash_storage_cell(lbm_enc_cons_ptr(LBM_PTR_NULL), &new_lst));

  if (lbm_is_symbol_nil(fst)) {
    lst = new_lst;
    fst = new_lst;
    handle_flash_status(write_const_car(lst, ctx->r));
  } else {
    handle_flash_status(write_const_cdr(lst, new_lst)); // low before high
    handle_flash_status(write_const_car(new_lst, ctx->r));
    lst = new_lst;
  }

  if (lbm_is_cons(val)) {
    sptr[0] = fst;
    sptr[1] = lst;//rest_cell;
    sptr[2] = lbm_ref_cell(val)->cdr;
    lbm_value *rptr = stack_reserve(ctx, 2);
    rptr[0] = MOVE_LIST_TO_FLASH;
    rptr[1] = MOVE_VAL_TO_FLASH_DISPATCH;
    ctx->r = lbm_ref_cell(val)->car; // already checked is_cons
  } else {
    sptr[0] = fst;
    sptr[1] = lst;
    sptr[2] = CLOSE_LIST_IN_FLASH;
    stack_reserve(ctx, 1)[0] = MOVE_VAL_TO_FLASH_DISPATCH;
    ctx->r =  val;
  }
  ctx->app_cont = true;
}

static void cont_close_list_in_flash(eval_context_t *ctx) {
  lbm_value lst = ctx->K.data[--ctx->K.sp];
  lbm_value fst = ctx->K.data[--ctx->K.sp];
  lbm_value val = ctx->r;
  handle_flash_status(write_const_cdr(lst, val));
  ctx->r = fst;
  ctx->app_cont = true;
}

static void cont_move_array_elts_to_flash(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 3);
  // sptr[2] = source array in RAM
  // sptr[1] = current index
  // sptr[0] = target array in flash
  lbm_array_header_t *src_arr = assume_array(sptr[2]);
  lbm_uint size = src_arr->size / sizeof(lbm_uint);
  lbm_value *srcdata = (lbm_value *)src_arr->data;

  lbm_array_header_t *tgt_arr = assume_array(sptr[0]);
  lbm_uint *tgtdata = (lbm_value *)tgt_arr->data;
  lbm_uint ix = lbm_dec_as_u32(sptr[1]);
  handle_flash_status(lbm_const_write(&tgtdata[ix], ctx->r));
  if (ix >= size-1) {
    ctx->r = sptr[0];
    lbm_stack_drop(&ctx->K, 3);
    ctx->app_cont = true;
    return;
  }
  sptr[1] = lbm_enc_u(ix + 1);
  lbm_value *rptr = stack_reserve(ctx, 2);
  rptr[0] = MOVE_ARRAY_ELTS_TO_FLASH;
  rptr[1] = MOVE_VAL_TO_FLASH_DISPATCH;
  ctx->r = srcdata[ix+1];
  ctx->app_cont = true;
  return;
}

static void cont_qq_expand_start(eval_context_t *ctx) {
  lbm_value *rptr = stack_reserve(ctx, 2);
  rptr[0] = ctx->r;
  rptr[1] = QQ_EXPAND;
  ctx->r = ENC_SYM_NIL;
  ctx->app_cont = true;
}

lbm_value quote_it(lbm_value qquoted) {
  if (lbm_is_symbol(qquoted) &&
      lbm_is_special(qquoted)) return qquoted;

  lbm_value val = cons_with_gc(qquoted, ENC_SYM_NIL, ENC_SYM_NIL);
  return cons_with_gc(ENC_SYM_QUOTE, val, ENC_SYM_NIL);
}

bool is_append(lbm_value a) {
  return (lbm_is_cons(a) && 
          lbm_is_symbol(lbm_ref_cell(a)->car) &&
          (lbm_ref_cell(a)->car == ENC_SYM_APPEND));
}

lbm_value append(lbm_value front, lbm_value back) {
  if (lbm_is_symbol_nil(front)) return back;
  if (lbm_is_symbol_nil(back)) return front;

  if (lbm_is_quoted_list(front) &&
      lbm_is_quoted_list(back)) {
    lbm_value f = get_cadr(front);
    lbm_value b = get_cadr(back);
    return quote_it(lbm_list_append(f, b));
  }

  if (is_append(back) &&
      lbm_is_quoted_list(get_cadr(back)) &&
       lbm_is_quoted_list(front)) {
    lbm_value ql = get_cadr(back);
    lbm_value f = get_cadr(front);
    lbm_value b = get_cadr(ql);

    lbm_value v = lbm_list_append(f, b);
    lbm_set_car(get_cdr(ql), v);
    return back;
  }

  if (is_append(back)) {
    back  = get_cdr(back);
    lbm_value new = cons_with_gc(front, back, ENC_SYM_NIL);
    return cons_with_gc(ENC_SYM_APPEND, new, ENC_SYM_NIL);
  }

  lbm_value t0, t1;

  t0 = cons_with_gc(back, ENC_SYM_NIL, front);
  t1 = cons_with_gc(front, t0, ENC_SYM_NIL);
  return cons_with_gc(ENC_SYM_APPEND, t1, ENC_SYM_NIL);
}

// ////////////////////////////////////////////////////////////
// Quasiquotation expansion that takes place at read time
// and is based on the paper by Bawden "Quasiquotation in lisp".
// Bawden, Alan. "Quasiquotation in Lisp." PEPM. 1999.
//
// cont_qq_expand and cont_qq_expand_list corresponds (mostly) to
// qq-expand and qq-expand-list in the paper.
// One difference is that the case where a backquote is nested
// inside of a backqoute is handled via the recursion through the
// reader.

/* Bawden's qq-expand implementation
(define (qq-expand x)
  (cond ((tag-comma? x)
         (tag-data x))
        ((tag-comma-atsign? x)
         (error "Illegal"))
        ((tag-backquote? x)
         (qq-expand
          (qq-expand (tag-data x))))
        ((pair? x)
         `(append
           ,(qq-expand-list (car x))
           ,(qq-expand (cdr x))))
        (else `',x)))
 */
static void cont_qq_expand(eval_context_t *ctx) {
  lbm_value qquoted = ctx->K.data[--ctx->K.sp];

  switch(lbm_type_of(qquoted)) {
  case LBM_TYPE_CONS: {
    lbm_value car_val = get_car(qquoted);
    lbm_value cdr_val = get_cdr(qquoted);
    if (lbm_type_of(car_val) == LBM_TYPE_SYMBOL &&
        car_val == ENC_SYM_COMMA) {
      ctx->r = append(ctx->r, get_car(cdr_val));
      ctx->app_cont = true;
    } else if (lbm_type_of(car_val) == LBM_TYPE_SYMBOL &&
               car_val == ENC_SYM_COMMAAT) {
      lbm_set_error_reason((char*)lbm_error_str_qq_expand);
      ERROR_AT_CTX(ENC_SYM_RERROR, qquoted);
    } else {
      lbm_value *rptr = stack_reserve(ctx, 6);
      rptr[0] = ctx->r;
      rptr[1] = QQ_APPEND;
      rptr[2] = cdr_val;
      rptr[3] = QQ_EXPAND;
      rptr[4] = car_val;
      rptr[5] = QQ_EXPAND_LIST;
      ctx->app_cont = true;
      ctx->r = ENC_SYM_NIL;
    }

  } break;
  default: {
    lbm_value res = quote_it(qquoted);
    ctx->r = append(ctx->r, res);
    ctx->app_cont = true;
  }
  }
}

static void cont_qq_append(eval_context_t *ctx) {
  lbm_value head = ctx->K.data[--ctx->K.sp];
  ctx->r = append(head, ctx->r);
  ctx->app_cont = true;
}

/* Bawden's qq-expand-list implementation
(define (qq-expand-list x)
  (cond ((tag-comma? x)
         `(list ,(tag-data x)))
        ((tag-comma-atsign? x)
         (tag-data x))
        ((tag-backquote? x)
         (qq-expand-list
          (qq-expand (tag-data x))))
        ((pair? x)
         `(list
           (append
            ,(qq-expand-list (car x))
            ,(qq-expand (cdr x)))))
        (else `'(,x))))
*/

static void cont_qq_expand_list(eval_context_t* ctx) {
  lbm_value l = ctx->K.data[--ctx->K.sp];

  ctx->app_cont = true;
  switch(lbm_type_of(l)) {
  case LBM_TYPE_CONS: {
    lbm_value car_val = get_car(l);
    lbm_value cdr_val = get_cdr(l);
    if (lbm_type_of(car_val) == LBM_TYPE_SYMBOL &&
        car_val == ENC_SYM_COMMA) {
      lbm_value tl = cons_with_gc(get_car(cdr_val), ENC_SYM_NIL, ENC_SYM_NIL);
      lbm_value tmp = cons_with_gc(ENC_SYM_LIST, tl, ENC_SYM_NIL);
      ctx->r = append(ctx->r, tmp);
      return;
    } else if (lbm_type_of(car_val) == LBM_TYPE_SYMBOL &&
               car_val == ENC_SYM_COMMAAT) {
      lbm_value cadr_val = get_car(cdr_val);
      ctx->r = cadr_val;
      return;
    } else {
      lbm_value *rptr = stack_reserve(ctx, 7);
      rptr[0] = QQ_LIST;
      rptr[1] = ctx->r;
      rptr[2] = QQ_APPEND;
      rptr[3] = cdr_val;
      rptr[4] = QQ_EXPAND;
      rptr[5] = car_val;
      rptr[6] = QQ_EXPAND_LIST;
      ctx->r = ENC_SYM_NIL;
    }

  } break;
  default: {
    lbm_value a_list = cons_with_gc(l, ENC_SYM_NIL, ENC_SYM_NIL);
    lbm_value tl = cons_with_gc(a_list, ENC_SYM_NIL, ENC_SYM_NIL);
    lbm_value tmp = cons_with_gc(ENC_SYM_QUOTE, tl, ENC_SYM_NIL);
    ctx->r = append(ctx->r, tmp);
  }
  }
}

static void cont_qq_list(eval_context_t *ctx) {
  lbm_value val = ctx->r;
  lbm_value apnd_app = cons_with_gc(val, ENC_SYM_NIL, ENC_SYM_NIL);
  lbm_value tmp = cons_with_gc(ENC_SYM_LIST, apnd_app, ENC_SYM_NIL);
  ctx->r = tmp;
  ctx->app_cont = true;
}

static void cont_kill(eval_context_t *ctx) {
  (void) ctx;
  ok_ctx();
}

static void cont_pop_reader_flags(eval_context_t *ctx) {
  lbm_value flags = ctx->K.data[--ctx->K.sp];
  ctx->flags = ctx->flags & ~EVAL_CPS_CONTEXT_READER_FLAGS_MASK;
  ctx->flags |= (lbm_dec_as_u32(flags) & EVAL_CPS_CONTEXT_READER_FLAGS_MASK);
  // r is unchanged.
  ctx->app_cont = true;
}

// cont_exception_handler
//
// s[sp-2] retval  - a list of 2 elements created by eval_trap
// s[sp-1] flags   - context flags stored by eval_trap
// 
static void cont_exception_handler(eval_context_t *ctx) {
  lbm_value *sptr = pop_stack_ptr(ctx, 2);
  lbm_value retval = sptr[0];
  lbm_value flags = sptr[1];
  lbm_set_car(get_cdr(retval), ctx->r);
  ctx->flags = (uint32_t)flags;
  ctx->r = retval;
  ctx->app_cont = true;
}

// cont_recv_to:
//
// s[sp-1] = patterns
//
// ctx->r = timeout value
static void cont_recv_to(eval_context_t *ctx) {
  if (lbm_is_number(ctx->r)) {
    lbm_value *sptr = get_stack_ptr(ctx, 1); // patterns at sptr[0]
    float timeout_time = lbm_dec_as_float(ctx->r);
    if (timeout_time < 0.0) timeout_time = 0.0; // clamp.
    if (ctx->num_mail > 0) {
      lbm_value e;
      lbm_value new_env = ctx->curr_env;
      int n = find_match(sptr[0], ctx->mailbox, ctx->num_mail, &e, &new_env);
      if (n >= 0) { // match
        mailbox_remove_mail(ctx, (lbm_uint)n);
        ctx->curr_env = new_env;
        ctx->curr_exp = e;
        lbm_stack_drop(&ctx->K, 1);
        return;
      }
    }
    // If no mail or no match, go to sleep
    lbm_uint *rptr = stack_reserve(ctx,2);
    rptr[0] = ctx->r;
    rptr[1] = RECV_TO_RETRY;
    block_current_ctx(LBM_THREAD_STATE_RECV_TO,S_TO_US(timeout_time),true);
  } else {
    ERROR_CTX(ENC_SYM_TERROR);
  }
}

// cont_recv_to_retry
//
// s[sp-2] = patterns
// s[sp-1] = timeout value
//
// ctx->r = nonsense | timeout symbol
static void cont_recv_to_retry(eval_context_t *ctx) {
  lbm_value *sptr = get_stack_ptr(ctx, 2); //sptr[0] = patterns, sptr[1] = timeout

  if (ctx->num_mail > 0) {
    lbm_value e;
    lbm_value new_env = ctx->curr_env;
    int n = find_match(sptr[0], ctx->mailbox, ctx->num_mail, &e, &new_env);
    if (n >= 0) { // match
      mailbox_remove_mail(ctx, (lbm_uint)n);
      ctx->curr_env = new_env;
      ctx->curr_exp = e;
      lbm_stack_drop(&ctx->K, 2);
      return;
    }
  }

  // No message matched but the timeout was reached.
  // This is like having a recv-to with no case that matches
  // the timeout symbol.
  if (ctx->r == ENC_SYM_TIMEOUT) {
    lbm_stack_drop(&ctx->K, 2);
    ctx->app_cont = true;
    return;
  }

  stack_reserve(ctx,1)[0] = RECV_TO_RETRY;
  reblock_current_ctx(LBM_THREAD_STATE_RECV_TO,true);
}


/*********************************************************/
/* Continuations table                                   */
typedef void (*cont_fun)(eval_context_t *);

static const cont_fun continuations[NUM_CONTINUATIONS] =
  { advance_ctx,  // CONT_DONE
    cont_set_global_env,
    cont_bind_to_key_rest,
    cont_if,
    cont_progn_rest,
    cont_application_args,
    cont_and,
    cont_or,
    cont_wait,
    cont_match,
    cont_application_start,
    cont_eval_r,
    cont_resume,
    cont_closure_application_args,
    cont_exit_atomic,
    cont_read_next_token,
    cont_read_append_continue,
    cont_read_eval_continue,
    cont_read_expect_closepar,
    cont_read_dot_terminate,
    cont_read_done,
    cont_read_start_bytearray,
    cont_read_append_bytearray,
    cont_map,
    cont_match_guard,
    cont_terminate,
    cont_progn_var,
    cont_setq,
    cont_move_to_flash,
    cont_move_val_to_flash_dispatch,
    cont_move_list_to_flash,
    cont_close_list_in_flash,
    cont_qq_expand_start,
    cont_qq_expand,
    cont_qq_append,
    cont_qq_expand_list,
    cont_qq_list,
    cont_kill,
    cont_loop,
    cont_loop_condition,
    cont_merge_rest,
    cont_merge_layer,
    cont_closure_args_rest,
    cont_move_array_elts_to_flash,
    cont_pop_reader_flags,
    cont_exception_handler,
    cont_recv_to,
    cont_wrap_result,
    cont_recv_to_retry,
    cont_read_start_array,
    cont_read_append_array,
    cont_loop_env_prep,
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
   eval_receive_timeout,
   eval_callcc,
   eval_atomic,
   eval_selfevaluating, // macro
   eval_selfevaluating, // cont
   eval_selfevaluating, // closure
   eval_cond,
   eval_app_cont,
   eval_var,
   eval_setq,
   eval_move_to_flash,
   eval_loop,
   eval_trap,
   eval_call_cc_unsafe,
   eval_selfevaluating, // cont_sp
  };


/*********************************************************/
/* Evaluator step function                               */

static void evaluation_step(void){
  eval_context_t *ctx = ctx_running;

  if (ctx->app_cont) {
    lbm_value k = ctx->K.data[--ctx->K.sp];
    ctx->app_cont = false;

    lbm_uint decoded_k = DEC_CONTINUATION(k);
    // If app_cont is true, then top of stack must be a valid continuation!
    // If top of stack is not a valid continuation CRASH!
    continuations[decoded_k](ctx);
    return;
  }

  if (lbm_is_symbol(ctx->curr_exp)) {
    eval_symbol(ctx);
    return;
  }
  if (lbm_is_cons(ctx->curr_exp)) {
    lbm_cons_t *cell = lbm_ref_cell(ctx->curr_exp);
    lbm_value h = cell->car;
    if (lbm_is_symbol(h) && ((h & ENC_SPECIAL_FORMS_MASK) == ENC_SPECIAL_FORMS_BIT)) {
      lbm_uint eval_index = lbm_dec_sym(h) & SPECIAL_FORMS_INDEX_MASK;
      evaluators[eval_index](ctx);
      return;
    }
    /*
     * At this point head can be anything. It should evaluate
     * into a form that can be applied (closure, symbol, ...) though.
     */
    lbm_value *reserved = stack_reserve(ctx, 3);
    reserved[0] = ctx->curr_env; // INFER: stack_reserve aborts context if error.
    reserved[1] = cell->cdr;
    reserved[2] = APPLICATION_START;
    ctx->curr_exp = h; // evaluate the function
    return;
  }

  eval_selfevaluating(ctx);
  return;
}

// Placed down here since it depends on a lot of things.
// (apply fun arg-list)
static void apply_apply(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 || lbm_is_list(args[1])) {
    lbm_value fun = args[0];
    lbm_value arg_list = args[1];

    lbm_stack_drop(&ctx->K, nargs+1);

    if (lbm_is_symbol(fun)) {
      if ((fun & ENC_SPECIAL_FORMS_MASK) == ENC_SPECIAL_FORMS_BIT) {
        // Since the special form evaluators are responsible for conditionally
        // evaluating their arguments there is no easy way to prevent them
        // evaluating their arguments. Therefore we compromise and allow them to do
        // so, even if this isn't always how you would expect apply to work.
        // For instance, `(apply and '(a))` would try evaluating the symbol `a`,
        // instead of just returning the symbol `a` outright.
        
        // Evaluator functions expect the current expression to equal the special
        // form, i.e. including the function symbol.
        lbm_value fun_and_args = cons_with_gc(fun, arg_list, ENC_SYM_NIL);
        ctx->curr_exp = fun_and_args;
        lbm_uint eval_index = lbm_dec_sym(fun) & SPECIAL_FORMS_INDEX_MASK;
        evaluators[eval_index](ctx);
        return;
      } else { // lbm_is_symbol(fun)
        stack_reserve(ctx, 1)[0] = fun;
        size_t arg_count = 0;
        for (lbm_value current = arg_list; lbm_is_cons(current); current = lbm_ref_cell(current)->cdr) {
          stack_reserve(ctx, 1)[0] = lbm_ref_cell(current)->car;
          arg_count++;
        }
        lbm_value *fun_and_args = get_stack_ptr(ctx, arg_count + 1);
        application(ctx, fun_and_args, arg_count);
        return;
      }
    } else if (lbm_is_cons(fun)) {
      lbm_cons_t *fun_cell = lbm_ref_cell(fun);
      switch (fun_cell->car) {
        case ENC_SYM_CLOSURE: {          
          lbm_value closure[3];
          extract_n(fun_cell->cdr, closure, 3);
          
          // Only placed here to protect from GC. Will be overriden later.
          // ctx->r = arg_list; // Should already be placed there.
          ctx->curr_exp = fun;
          
          lbm_value env = closure[CLO_ENV];
          
          lbm_value current_params = closure[CLO_PARAMS];
          lbm_value current_args = arg_list;
          
          while (true) {
            bool more_params = lbm_is_cons(current_params);
            bool more_args = lbm_is_cons(current_args);
            if (more_params && more_args) {
              lbm_cons_t *p_cell = lbm_ref_cell(current_params);
              lbm_cons_t *a_cell = lbm_ref_cell(current_args);
              lbm_value car_params = p_cell->car;
              lbm_value car_args = a_cell->car;
              lbm_value cdr_params = p_cell->cdr;
              lbm_value cdr_args = a_cell->cdr;
              
              // More parameters to bind
              env = allocate_binding(
                car_params,
                car_args,
                env
              );
              
              current_params = cdr_params;
              current_args = cdr_args;
            } else if (!more_params && more_args) {
              // More arguments but all parameters have been bound
              env = allocate_binding(ENC_SYM_REST_ARGS, current_args, env);
              break;
            } else if (!more_params && !more_args) {
              // All parameters and arguments have been bound
              break;
            } else {
              // More parameters to bind but no arguments left
              lbm_set_error_reason(lbm_error_str_num_args);
              ERROR_AT_CTX(ENC_SYM_EERROR, fun);
            }
          }
          
          ctx->curr_env = env;
          ctx->curr_exp = closure[CLO_BODY];
          return;
        } break;
        case ENC_SYM_CONT:{
          ctx->r = fun;
          ctx->r = setup_cont(ctx, arg_list);
          ctx->app_cont = true;
          return;
        } break;
        case ENC_SYM_CONT_SP: {
          ctx->r = fun;
          ctx->r = setup_cont_sp(ctx, arg_list);
          ctx->app_cont = true;
          return;
        } break;
        case ENC_SYM_MACRO:{
          ctx->r = fun;
          setup_macro(ctx, arg_list, ctx->curr_env);
          return;
        } break;
        default: {
          lbm_set_error_reason(lbm_error_str_not_applicable);
          ERROR_AT_CTX(ENC_SYM_EERROR, fun);
        } break;
      }
    } else {
      lbm_set_error_reason(lbm_error_str_not_applicable);
      ERROR_AT_CTX(ENC_SYM_EERROR, fun);
    }
  } else {
    lbm_set_error_reason(lbm_error_str_incorrect_arg);
    ERROR_AT_CTX(ENC_SYM_EERROR, ENC_SYM_APPLY);
  }
}

// Reset has a built in pause.
// so after reset, continue.
void lbm_reset_eval(void) {
  eval_cps_next_state_arg = 0;
  eval_cps_next_state = EVAL_CPS_STATE_RESET;
  if (eval_cps_next_state != eval_cps_run_state) eval_cps_state_changed = true;
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

// Only unblocks threads that are unblockable.
// A sleeping thread is not unblockable.
static void handle_event_unblock_ctx(lbm_cid cid, lbm_value v) {
  eval_context_t *found = NULL;
  mutex_lock(&qmutex);

  found = lookup_ctx_nm(&blocked, cid);
  if (found && LBM_IS_STATE_UNBLOCKABLE(found->state)){
    drop_ctx_nm(&blocked,found);
    if (lbm_is_error(v)) {
      get_stack_ptr(found, 1)[0] = TERMINATE; // replace TOS
      found->app_cont = true;
    }
    found->r = v;
    found->state = LBM_THREAD_STATE_READY;
    enqueue_ctx_nm(&queue,found);
  }
  mutex_unlock(&qmutex);
}

static void handle_event_define(lbm_value key, lbm_value val) {
  lbm_uint dec_key = lbm_dec_sym(key);
  lbm_uint ix_key  = dec_key & GLOBAL_ENV_MASK;
  lbm_value *global_env = lbm_get_global_env();
  lbm_uint orig_env = global_env[ix_key];
  lbm_value new_env;
  // A key is a symbol and should not need to be remembered.
  WITH_GC(new_env, lbm_env_set(orig_env,key,val));

  global_env[ix_key] = new_env;
}

static lbm_value get_event_value(lbm_event_t *e) {
  lbm_value v;
  if (e->buf_len > 0) {
    lbm_flat_value_t fv;
    fv.buf = (uint8_t*)e->buf_ptr;
    fv.buf_size = e->buf_len;
    fv.buf_pos = 0;
    lbm_unflatten_value(&fv, &v);
    // Free the flat value buffer. GC is unaware of its existence.
    lbm_free(fv.buf);
  } else {
    v = (lbm_value)e->buf_ptr;
  }
  return v;
}

// In a scenario where C is enqueuing events and other LBM threads
// are sendind mail to event handler concurrently, old events will
// be dropped as the backpressure mechanism wont detect this scenario.
// TODO: Low prio pondering on robust solutions.
static void process_events(void) {

  if (!lbm_events) {
    return;
  }

  lbm_event_t e;
  while (lbm_event_pop(&e)) {
    lbm_value event_val = get_event_value(&e);
    switch(e.type) {
    case LBM_EVENT_UNBLOCK_CTX:
      handle_event_unblock_ctx((lbm_cid)e.parameter, event_val);
      break;
    case LBM_EVENT_DEFINE:
      handle_event_define((lbm_value)e.parameter, event_val);
      break;
    case LBM_EVENT_FOR_HANDLER:
      if (lbm_event_handler_pid >= 0) {
        //If multiple events for handler, this is wasteful!
        // TODO: Find the event_handler once and send all mails.
        // However, do it with as little new code as possible.
        lbm_find_receiver_and_send(lbm_event_handler_pid, event_val);
      }
      break;
    }
  }
}

void lbm_add_eval_symbols(void) {
  lbm_uint x = 0;
  lbm_uint y = 0;
  lbm_add_symbol("x", &x);
  lbm_add_symbol("y", &y);
  symbol_x = lbm_enc_sym(x);
  symbol_y = lbm_enc_sym(y);
}

/* eval_cps_run can be paused
   I think it would be better use a mailbox for
   communication between other threads and the run_eval
   but for now a set of variables will be used. */
void lbm_run_eval(void){
  if (setjmp(critical_error_jmp_buf) > 0) {
    lbm_printf_callback("GC stack overflow!\n");
    critical_error_callback();
    // terminate evaluation thread.
    return;
  }

  setjmp(error_jmp_buf);

  while (eval_running) {
    if (eval_cps_state_changed  || eval_cps_run_state == EVAL_CPS_STATE_PAUSED) {
      eval_cps_state_changed = false;
      switch (eval_cps_next_state) {
      case EVAL_CPS_STATE_RESET:
        if (eval_cps_run_state != EVAL_CPS_STATE_RESET) {
          is_atomic = false;
          blocked.first = NULL;
          blocked.last = NULL;
          queue.first = NULL;
          queue.last = NULL;
          ctx_running = NULL;
#ifdef LBM_USE_TIME_QUOTA
          eval_time_quota = 0; // maybe timestamp here ?
#else
          eval_steps_quota = eval_steps_refill;
#endif
          eval_cps_run_state = EVAL_CPS_STATE_RESET;
          if (blocking_extension) {
            blocking_extension = false;
            mutex_unlock(&blocking_extension_mutex);
          }
        }
        usleep_callback(EVAL_CPS_MIN_SLEEP);
        continue;
      case EVAL_CPS_STATE_PAUSED:
        if (eval_cps_run_state != EVAL_CPS_STATE_PAUSED) {
          if (lbm_heap_num_free() < eval_cps_next_state_arg) {
            gc();
          }
          eval_cps_next_state_arg = 0;
          eval_cps_run_state = EVAL_CPS_STATE_PAUSED;
        }
        usleep_callback(EVAL_CPS_MIN_SLEEP);
        continue;
      case EVAL_CPS_STATE_KILL:
        eval_cps_run_state = EVAL_CPS_STATE_DEAD;
        eval_running = false;
        continue;
      default: // running state
        eval_cps_run_state = eval_cps_next_state;
        break;
      }
    }
    while (true) {
#ifdef LBM_USE_TIME_QUOTA
      // Is "negative" (closer to max value) when the quota timestamp is "ahead
      // of" the current timestamp. It handles the quota being very large while
      // the current timestamp has overflowed back to being small, giving a
      // "positive" (closer to min value) result, meaning the context will be
      // switched.
      uint32_t unsigned_difference = timestamp_us_callback() - eval_current_quota;
      bool is_negative = unsigned_difference & (1u << 31); 
      if (is_negative && ctx_running) {
        evaluation_step();
      } else {
        if (eval_cps_state_changed) break;
        // On overflow of timer, task will get a no-quota.
        // Could lead to busy-wait here until timestamp and quota
        // are on same side of overflow.
        eval_current_quota = timestamp_us_callback() + eval_time_refill;
        if (!is_atomic) {
          if (gc_requested) {
            gc();
          }
          process_events();
          mutex_lock(&qmutex);
          if (ctx_running) {
            enqueue_ctx_nm(&queue, ctx_running);
            ctx_running = NULL;
          }
          wake_up_ctxs_nm();
          ctx_running = dequeue_ctx_nm(&queue);
          mutex_unlock(&qmutex);
          if (!ctx_running) {
            lbm_system_sleeping = true;
            //Fixed sleep interval to poll events regularly.
            usleep_callback(EVAL_CPS_MIN_SLEEP);
            lbm_system_sleeping = false;
          }
        }
      }
#else
      if (eval_steps_quota && ctx_running) {
        eval_steps_quota--;
        evaluation_step();
      } else {
        if (eval_cps_state_changed) break;
        eval_steps_quota = eval_steps_refill;
        if (!is_atomic) {
          if (gc_requested) {
            gc();
          }
          process_events();
          mutex_lock(&qmutex);
          if (ctx_running) {
            enqueue_ctx_nm(&queue, ctx_running);
            ctx_running = NULL;
          }
          wake_up_ctxs_nm();
          ctx_running = dequeue_ctx_nm(&queue);
          mutex_unlock(&qmutex);
          if (!ctx_running) {
            lbm_system_sleeping = true;
            //Fixed sleep interval to poll events regularly.
            usleep_callback(EVAL_CPS_MIN_SLEEP);
            lbm_system_sleeping = false;
          }
        }
      }
#endif
    }
  }
}

bool lbm_eval_init(void) {
  if (!qmutex_initialized) {
    mutex_init(&qmutex);
    qmutex_initialized = true;
  }
  if (!lbm_events_mutex_initialized) {
    mutex_init(&lbm_events_mutex);
    lbm_events_mutex_initialized = true;
  }
  if (!blocking_extension_mutex_initialized) {
    mutex_init(&blocking_extension_mutex);
    blocking_extension_mutex_initialized = true;
  }

  mutex_lock(&qmutex);
  mutex_lock(&lbm_events_mutex);

  blocked.first = NULL;
  blocked.last = NULL;
  queue.first = NULL;
  queue.last = NULL;
  ctx_running = NULL;

  eval_cps_run_state = EVAL_CPS_STATE_RUNNING;

  mutex_unlock(&lbm_events_mutex);
  mutex_unlock(&qmutex);

  if (!lbm_init_env()) return false;
  eval_running = true;
  return true;
}

bool lbm_eval_init_events(unsigned int num_events) {

  mutex_lock(&lbm_events_mutex);
  lbm_events = (lbm_event_t*)lbm_malloc(num_events * sizeof(lbm_event_t));
  bool r = false;
  if (lbm_events) {
    lbm_events_max = num_events;
    lbm_events_head = 0;
    lbm_events_tail = 0;
    lbm_events_full = false;
    lbm_event_handler_pid = -1;
    r = true;
  }
  mutex_unlock(&lbm_events_mutex);
  return r;
}
