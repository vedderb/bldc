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

#ifdef __cplusplus
extern "C" {
#endif

#define EVAL_CPS_STATE_INIT    0
#define EVAL_CPS_STATE_PAUSED  1
#define EVAL_CPS_STATE_RUNNING 2
#define EVAL_CPS_STATE_STEP    3
#define EVAL_CPS_STATE_KILL    4

#define EVAL_CPS_DEFAULT_MAILBOX_SIZE 10

#define EVAL_CPS_CONTEXT_FLAG_NOTHING       (uint32_t)0x0
#define EVAL_CPS_CONTEXT_FLAG_TRAP          (uint32_t)0x1

/** The eval_context_t struct represents a lispbm process.
 *
 */
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
  bool  done;
  bool  app_cont;
  lbm_stack_t K;
  lbm_uint timestamp;
  lbm_uint sleep_us;
  lbm_cid id;
  lbm_cid parent;
  /* List structure */
  struct eval_context_s *prev;
  struct eval_context_s *next;
} eval_context_t;

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
/** Perform a single step of evaluation.
 * The evaluator should be in EVAL_CPS_STATE_PAUSED before running this function.
 * After taking one step of evaluation, the evaluator will return to being in the
 * EVAL_CPS_STATE_PUASED state.
 */
void lbm_step_eval(void);
/** Perform multiple steps of evaluation.
 * \param n Number of eval steps to perform.
 */
void lbm_step_n_eval(uint32_t n);
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
 * \return 1 on success and 0 on failure.
 */
int lbm_set_error_reason(char *error_str);
/** Create a context and enqueue it as runnable.
 *
 * \param program The program to evaluate in the context.
 * \param env An initial environment.
 * \param stack_size Stack size for the context.
 * \return
 */
lbm_cid lbm_create_ctx(lbm_value program, lbm_value env, lbm_uint stack_size);
/** Block a context from an extension
 */
void lbm_block_ctx_from_extension(void);
/** Unblock a context that has been blocked by a C extension
 *  Trying to unblock a context that is waiting on a message
 *  in a mailbox is not encouraged
 * \param cid Lisp process to wake up.
 * \param result Value passed to the lisp process as the result from the blocking function.
 */
bool lbm_unblock_ctx(lbm_cid cid, lbm_value result);
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
/** Iterate over all done contexts and apply function on each context.
 *
 * \param f Function to apply to each context.
 * \param arg1 Pointer argument that can be used to convey information back to user.
 * \param arg2 Same as above
 */
void lbm_done_iterator(ctx_fun f, void*, void*);
/** Iterate over all sleeping contexts and apply function on each context.
 * 
 * \param f Function to apply to each context.
 * \param arg1 Pointer argument that can be used to convey information back to user.
 * \param arg2 Same as above. 
 */
void lbm_sleeping_iterator(ctx_fun f, void *, void *);  
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
/** Set a callback that is run when reading source is finishes
 *  within a context
 */
void lbm_set_reader_done_callback(void (*fptr)(lbm_cid));
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
