/*
    Copyright 2018, 2020, 2021, 2022 Joel Svensson  svenssonjoel@yahoo.se

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
/** \file eval_cps.h */
#ifndef EVAL_CPS_H_
#define EVAL_CPS_H_

#include "lbm_types.h"
#include "stack.h"
#include "lbm_channel.h"
#include "lbm_flat_value.h"

#ifdef __cplusplus
extern "C" {
#endif

#define EVAL_CPS_STATE_INIT    0
#define EVAL_CPS_STATE_PAUSED  1
#define EVAL_CPS_STATE_RUNNING 2
#define EVAL_CPS_STATE_STEP    3
#define EVAL_CPS_STATE_KILL    4

#define EVAL_CPS_DEFAULT_MAILBOX_SIZE 10

#define EVAL_CPS_CONTEXT_FLAG_NOTHING               (uint32_t)0x0
#define EVAL_CPS_CONTEXT_FLAG_TRAP                  (uint32_t)0x1
#define EVAL_CPS_CONTEXT_FLAG_CONST                 (uint32_t)0x2
#define EVAL_CPS_CONTEXT_FLAG_CONST_SYMBOL_STRINGS  (uint32_t)0x4
#define EVAL_CPS_CONTEXT_FLAG_INCREMENTAL_READ      (uint32_t)0x8
  
/** The eval_context_t struct represents a lispbm process.
 *
 */
#define LBM_THREAD_STATE_READY     (uint32_t)0
#define LBM_THREAD_STATE_BLOCKED   (uint32_t)1
#define LBM_THREAD_STATE_TIMEOUT   (uint32_t)2
#define LBM_THREAD_STATE_SLEEPING  (uint32_t)3
#define LBM_THREAD_STATE_GC_BIT    (uint32_t)(1 << 31)
  
typedef struct eval_context_s{
  lbm_value program;
  lbm_value curr_exp;
  lbm_value curr_env;
  lbm_value *mailbox;    /* Message passing mailbox */
  uint32_t  mailbox_size;
  uint32_t  num_mail;    /* Number of messages in mailbox */
  uint32_t  flags;
  lbm_value r;
  char *error_reason;
  bool  app_cont;
  lbm_stack_t K;
  lbm_uint timestamp;
  lbm_uint sleep_us;
  uint32_t state;
  char *name;
  lbm_cid id;
  lbm_cid parent;
  /* while reading */
  lbm_int row0;
  lbm_int row1;
  /* List structure */
  struct eval_context_s *prev;
  struct eval_context_s *next;
} eval_context_t;

typedef enum {
  LBM_EVENT_FOR_HANDLER = 0,
  LBM_EVENT_UNBLOCK_CTX,
} lbm_event_type_t;

typedef struct {
  lbm_event_type_t type;
  lbm_uint parameter;
  lbm_uint buf_ptr;
  lbm_uint buf_len;
} lbm_event_t;

/** Fundamental operation type */
typedef lbm_value (*fundamental_fun)(lbm_value *, lbm_uint, eval_context_t*);

extern const fundamental_fun fundamental_table[];

/** A function pointer type to use together with the queue iterators.
 *
 * \param
 * \param
 * \param
 */
typedef void (*ctx_fun)(eval_context_t *, void*, void*);

/* Common interface */
/** Get the global environment
 *
 * \return global environment.
 */
lbm_value eval_cps_get_env(void);

/* Concurrent interface */
/** Initialize the evaluator.
 *
 * \return 1 on success and 0 on failure.
 */
int lbm_eval_init(void);
/**  Set a new value to use as step quota.
 *   This changes the scheduling interval.
 *   \param quota The new quota.
 */
void lbm_set_eval_step_quota(uint32_t quota);
/** Initialize events
 * \param num_events The maximum number of unprocessed events.
 * \return true on success, false otherwise.
 */
bool lbm_eval_init_events(unsigned int num_events);
/** Get the process ID for the current event handler.
 * \return process ID on success and -1 if no event handler is registered.
 */
lbm_cid lbm_get_event_handler_pid(void);
/** Set the event handler process ID.
 * \param pid The ID of the process to which events should be sent
 */
void lbm_set_event_handler_pid(lbm_cid pid);
/** Check if an event handler is registerd.
 * \return True if event handler exists, otherwise false.
 */
bool lbm_event_handler_exists(void);
/** Send an event to the registered event handler process.
 * If lbm_event returns false the C code will still be responsible for
 * the flat_value passed into lbm_event. If lbm_event returns true,
 * the LBM runtime system will take responsibility for the freeing
 * of the memory allocated in the flat_value.
 * \param event The event to send to the registered handler.
 * \param opt_array An optional array to pass to the event handler.
 * \param opt_array_len Length of array mandatory if array is passed in.
 * \return true if the event was successfully enqueued to be sent, false otherwise.
 */
bool lbm_event(lbm_flat_value_t *fv);
/** Send an unboxed value as an event to the event handler.
 * \param unboxed. An lbm_value (encoded) such as a symbol. int, uint, character.
 * \return true on success.
 */
bool lbm_event_unboxed(lbm_value unboxed);

/** Remove a context that has finished executing and free up its associated memory.
 *
 * \param cid Context id of context to free.
 * \param v Pointer to an lbm_value to store the result computed by the program into.
 * Note that for comples values, GC will free these up the next time it runs.
 * \return 1 if a context was successfully removed otherwise 0.
 */
int lbm_remove_done_ctx(lbm_cid cid, lbm_value *v);
/** Wait until a given cid is not present in any of the queues.
 *  If you have spawned this cid, you can conclude that it has
 *  run to completion or failure.
 *
 * \param cid Context id to wait for.
 * \param timeout_ms timeout in ms or 0 for no timeout.
 * \return Result computed by the program running in the context.
 */
bool lbm_wait_ctx(lbm_cid cid, lbm_uint timeout_ms);
/** Creates a context and initializes it with the provided program. The context
 * is added to the ready queue and will start executing when the evaluator is free.
 *
 * \param lisp Program to launch
 * \return a context id on success and 0 on failure to launch a context.
 */
lbm_cid lbm_eval_program(lbm_value lisp);
/** An extended version of lbm_eval_program that allows specification of stack size to use.
 *
 * \param lisp Program to launch.
 * \param stack_size Size of the continuation stack for this context.
 * \return a context id on success and 0 on failure to launch a context.
 */
lbm_cid lbm_eval_program_ext(lbm_value lisp, unsigned int stack_size);

/** This function executes the evaluation loop and does not return.
 *  lbm_run_eval should be started in a new thread provided by the underlying HAL or OS.
 */
void lbm_run_eval(void);

/** Indicate that the evaluator should pause at the next iteration.
 * You cannot assume that the evaluator has paused unless you call lbm_get_eval_state and get the
 * return value EVAL_CPS_STATE_PAUSED.
 */
void lbm_pause_eval(void);
/** Pause the evaluator and perform GC if needed.
 *
 * \param num_free Perform GC if there are less than this many elements free on the heap.
 */
void lbm_pause_eval_with_gc(uint32_t num_free);
/** Resume from being in EVAL_CPS_STATE_PAUSED.
 *
 */
void lbm_continue_eval(void);
/** This will kill the evaluator on the next iteration.
 *
 */
void lbm_kill_eval(void);
/** Get the current state of the evaluator.
 *
 * \return Current state of the evaluator.
 */
uint32_t lbm_get_eval_state(void);
/** Provide a description of an error as a string.
 *  Use when implementing for example extensions to
 *  report an error message to the programmer in case
 *  the extension is used incorrectly.
 *
 *  The error string can be allocated in lbm_memory
 *  and will in that case be freed when the context
 *  that errored is removed.
 * \param error_str
 */
void lbm_set_error_reason(char *error_str);
/** Provide the expression that is most suspicious
 *  in relation to the error at hand.
 * \param lbm_value
 */
void lbm_set_error_suspect(lbm_value suspect);
/** Terminate the runtime system in response to an
  *  error that it is not possible to recover from.
  */
void lbm_critical_error(void);
/** Set the critical error callback */
void lbm_set_critical_error_callback(void (*fptr)(void));
/** Create a context and enqueue it as runnable.
 *
 * \param program The program to evaluate in the context.
 * \param env An initial environment.
 * \param stack_size Stack size for the context.
 * \param name Name of thread or NULL.
 * \return
 */
lbm_cid lbm_create_ctx(lbm_value program, lbm_value env, lbm_uint stack_size, char *name);
/** Block a context from an extension
 */
void lbm_block_ctx_from_extension(void);
/** Block a context from an extension with a timeout.
 * \param s Timeout in seconds.
 */
void lbm_block_ctx_from_extension_timeout(float s);
/** Undo a previous call to lbm_block_ctx_from_extension.
 */
void lbm_undo_block_ctx_from_extension(void);
/** Unblock a context that has been blocked by a C extension
 *  Trying to unblock a context that is waiting on a message
 *  in a mailbox is not encouraged
 * \param cid Lisp process to wake up.
 * \param fv lbm_flat_value to give return as result from the unblocket process.
 */
bool lbm_unblock_ctx(lbm_cid cid, lbm_flat_value_t *fv);
/** Unblock a context bypassing the event-queue.
 *  Since the context will be unblocked in a separate tread it cannot
 *  take a composite return value. Only unboxed lbm_values are allowed.
 * \param cid Lisp process to inblock.
 * \param unboxed An unboxed lbm_value: char, i, u or symbol type.
 * \return True on successfully unblocking. False otherwise.
 */
bool lbm_unblock_ctx_unboxed(lbm_cid cid, lbm_value unboxed);
/**  Iterate over all ready contexts and apply function on each context.
 *
 * \param f Function to apply to each context.
 * \param arg1 Pointer argument that can be used to convey information back to user.
 * \param arg2 Same as above.
 */
void lbm_running_iterator(ctx_fun f, void*, void*);
/** Iterate over all blocked contexts and apply function on each context.
 *
 * \param f Function to apply to each context.
 * \param arg1 Pointer argument that can be used to convey information back to user.
 * \param arg2 Same as above
 */
void lbm_blocked_iterator(ctx_fun f, void*, void*);
/** toggle verbosity level of error messages
 */
void lbm_toggle_verbose(void);
/** Set verbosity level of lispbm error messages.
 *
 * \param verbose Boolean to turn verbose errors on or off.
 */
void lbm_set_verbose(bool verbose);
/** Set a usleep callback for use by the evaluator thread.
 *
 * \param fptr Pointer to a sleep function.
 */
void lbm_set_usleep_callback(void (*fptr)(uint32_t));
/** Set a timestamp callback for use by the evaluator thread.
 *
 * \param fptr Pointer to a timestamp generating function.
 */
void lbm_set_timestamp_us_callback(uint32_t (*fptr)(void));
/** Set a "done" callback function. This function will be called by
 * the evaluator when a context finishes execution.
 *
 * \param fptr Pointer to a "done" function.
 */
void lbm_set_ctx_done_callback(void (*fptr)(eval_context_t *));
/** Set a "printf" callback function. This function will be called by
 * the evaluator to report error strings back to the user.
 *
 * \param fptr Pointer to a "printf" function.
 */
void lbm_set_printf_callback(int (*prnt)(const char*, ...));
/** Set a callback for dynamically loading code associated with
 * an undefined symbol
 */
void lbm_set_dynamic_load_callback(bool (*fptr)(const char *, const char **));
/** Get the CID of the currently executing context.
 *  Should be called from an extension where there is
 *  a guarantee that a context is running
 */
lbm_cid lbm_get_current_cid(void);

/** Get the currently executing context.
 * Should be called from an extension where there is
 * a guarantee that a context is running
 */
eval_context_t *lbm_get_current_context(void);

/** Change the mailbox size for a given context.
 * \param ctx The context to change mailbox size for.
 * \param new_size The new size of the mailbox.
 * \return true on success and false otherwise.
 */
bool lbm_mailbox_change_size(eval_context_t *ctx, lbm_uint new_size);

bool create_string_channel(char *str, lbm_value *res);

bool lift_char_channel(lbm_char_channel_t *ch, lbm_value *res);

lbm_flash_status request_flash_storage_cell(lbm_value val, lbm_value *res);
  //bool lift_array_flash(lbm_value flash_cell, char *data, lbm_uint num_elt);

/** deliver a message
 *
 * \param cid Process to deliver to.
 * \param msg Message to deliver
 * \return lbm_enc_sym(SYM_NIL) on failure and lbm_enc_sym(SYM_TRUE) on success.
 */
lbm_value lbm_find_receiver_and_send(lbm_cid cid, lbm_value msg);
/** Perform garbage collection,
 * If this is called from another thread than the eval thread, evaluation must be
 * paused! Or there will be lots of trouble!
 *
 * \return 1 on success
 */
int lbm_perform_gc(void);

#ifdef __cplusplus
}
#endif
#endif
