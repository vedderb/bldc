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

#define EVAL_CPS_STATE_INIT    0
#define EVAL_CPS_STATE_PAUSED  1
#define EVAL_CPS_STATE_RUNNING 2
#define EVAL_CPS_STATE_STEP    3
#define EVAL_CPS_STATE_KILL    4

/** The eval_context_t struct represents a lispbm process.
 *
 */
typedef struct eval_context_s{
  lbm_value program;
  lbm_value curr_exp;
  lbm_value curr_env;
  lbm_value mailbox;  /*massage passing mailbox */
  lbm_value r;
  char *error_reason;
  bool  done;
  bool  app_cont;
  lbm_stack_t K;
  /* Process control */
  lbm_uint timestamp;
  lbm_uint sleep_us;
  lbm_cid id;
  /* List structure */
  struct eval_context_s *prev;
  struct eval_context_s *next;
} eval_context_t;

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
extern lbm_value eval_cps_get_env(void);

/* Concurrent interface */
/** Initialize the evaluator.
 *
 * \return 1 on success and 0 on failure.
 */
extern int lbm_eval_init(void);
/** Remove a context that has finished executing and free up its associated memory.
 *
 * \param cid Context id of context to free.
 * \param v Pointer to an lbm_value to store the result computed by the program into.
 * Note that for comples values, GC will free these up the next time it runs.
 * \return 1 if a context was successfully removed otherwise 0.
 */
extern int lbm_remove_done_ctx(lbm_cid cid, lbm_value *v);
/** Wait until a given cid is not present in any of the queues.
 *  If you have spawned this cid, you can conclude that it has
 *  run to completion or failure.
 *
 * \param cid Context id to wait for.
 * \param timeout_ms timeout in ms or 0 for no timeout.
 * \return Result computed by the program running in the context.
 */
extern bool lbm_wait_ctx(lbm_cid cid, lbm_uint timeout_ms);


/** Creates a context and initializes it with the provided program. The context
 * is added to the ready queue and will start executing when the evaluator is free.
 *
 * \param lisp Program to launch
 * \return a context id on success and 0 on failure to launch a context.
 */
extern lbm_cid lbm_eval_program(lbm_value lisp);
/** An extended version of lbm_eval_program that allows specification of stack size to use.
 *
 * \param lisp Program to launch.
 * \param stack_size Size of the continuation stack for this context.
 * \return a context id on success and 0 on failure to launch a context.
 */
extern lbm_cid lbm_eval_program_ext(lbm_value lisp, unsigned int stack_size);

/** This function executes the evaluation loop and does not return.
 *  lbm_run_eval should be started in a new thread provided by the underlying HAL or OS.
 */
extern void lbm_run_eval(void);

/** Indicate that the evaluator should pause at the next iteration.
 * You cannot assume that the evaluator has paused unless you call lbm_get_eval_state and get the
 * return value EVAL_CPS_STATE_PAUSED.
 */
extern void lbm_pause_eval(void);
/** Pause the evaluator and perform GC if needed.
 *
 * \param num_free Perform GC if there are less than this many elements free on the heap.
 */
extern void lbm_pause_eval_with_gc(uint32_t num_free);
/** Perform a single step of evaluation.
 * The evaluator should be in EVAL_CPS_STATE_PAUSED before running this function.
 * After taking one step of evaluation, the evaluator will return to being in the
 * EVAL_CPS_STATE_PUASED state.
 */
extern void lbm_step_eval(void);
/** Perform multiple steps of evaluation.
 * \param n Number of eval steps to perform.
 */
extern void lbm_step_n_eval(uint32_t n);
/** Resume from being in EVAL_CPS_STATE_PAUSED.
 *
 */
extern void lbm_continue_eval(void);
/** This will kill the evaluator on the next iteration.
 *
 */
extern void lbm_kill_eval(void);
/** Get the current state of the evaluator.
 *
 * \return Current state of the evaluator.
 */
extern uint32_t lbm_get_eval_state(void);
/** Provide a description of an error as a string.
 *  Use when implementing for example extensions to
 *  report an error message to the programmer in case
 *  the extension is used incorrectly.
 *
 *  The error string can be allocates in lbm_memory
 *  and will in that case be freed when the context
 *  that errored is removed.
 * \param error_str
 * \return 1 on success and 0 on failure.
 */
extern int lbm_set_error_reason(char *error_str);
/** Create a context and enqueue it as runnable.
 *
 * \param program The program to evaluate in the context.
 * \param env An initial environment.
 * \param stack_size Stack size for the context.
 * \return
 */
extern lbm_cid lbm_create_ctx(lbm_value program, lbm_value env, lbm_uint stack_size);
/** Block a context from an extension
 */
extern void lbm_block_ctx_from_extension(void);
/** Unblock a context that has been blocked by a C extension
 *  Trying to unblock a context that is waiting on a message
 *  in a mailbox is not encouraged
 * \param cid Lisp process to wake up.
 * \param result Value passed to the lisp process as the result from the blocking function.
 */
extern bool lbm_unblock_ctx(lbm_cid cid, lbm_value result);
/**  Iterate over all ready contexts and apply function on each context.
 *
 * \param f Function to apply to each context.
 * \param arg1 Pointer argument that can be used to convey information back to user
 * \param arg2 Same as above.
 */
extern void lbm_running_iterator(ctx_fun f, void*, void*);
/** Iterate over all blocked contexts and apply function on each context.
 *
 * \param f Function to apply to each context.
 * \param arg1 Pointer argument that can be used to convey information back to user
 * \param arg2 Same as above
 */
extern void lbm_blocked_iterator(ctx_fun f, void*, void*);
/** Iterate over all done contexts and apply function on each context.
 *
 * \param f Function to apply to each context.
 * \param arg1 Pointer argument that can be used to convey information back to user
 * \param arg2 Same as above
 */
extern void lbm_done_iterator(ctx_fun f, void*, void*);
/** toggle verbosity level of error messages
 */
extern void lbm_toggle_verbose(void);
/** Set verbosity level of lispbm error messages.
 *
 * \param verbose Boolean to turn verbose errors on or off.
 */
extern void lbm_set_verbose(bool verbose);
/** Set a usleep callback for use by the evaluator thread.
 *
 * \param fptr Pointer to a sleep function.
 */
extern void lbm_set_usleep_callback(void (*fptr)(uint32_t));
/** Set a timestamp callback for use by the evaluator thread.
 *
 * \param fptr Pointer to a timestamp generating function.
 */
extern void lbm_set_timestamp_us_callback(uint32_t (*fptr)(void));
/** Set a "done" callback function. This function will be called by
 * the evaluator when a context finishes execution.
 *
 * \param fptr Pointer to a "done" function.
 */
extern void lbm_set_ctx_done_callback(void (*fptr)(eval_context_t *));
/** Set a "printf" callback function. This function will be called by
 * the evaluator to report error strings back to the user.
 *
 * \param fptr Pointer to a "printf" function.
 */
extern void lbm_set_printf_callback(int (*prnt)(const char*, ...));
/** Set a callback for dynamically loading code associated with
 * an undefined symbol
 */
extern void lbm_set_dynamic_load_callback(bool (*fptr)(const char *, const char **));
/** Set a callback that is run when reading source is finishes
 *  within a context
 */
extern void lbm_set_reader_done_callback(void (*fptr)(lbm_cid));
/** Get the CID of the currently executing context.
 *  Should be called from an extension where there is
 *  a guarantee that a context is running
 */
extern lbm_cid lbm_get_current_cid(void);

/** Create a token stream for parsing for code
 *
 * \param str character stream to convert into a token stream.
 * \return token stream.
 */
extern lbm_value lbm_create_token_stream(lbm_tokenizer_char_stream_t *str);
/** Explicitly free a stream (if something breaks while creating it)
 *  The stream must not have been made available to the program
 * \param stream The stream to free
 */
extern int lbm_explicit_free_token_stream(lbm_value stream);

/** deliver a message
 *
 * \param cid Process to deliver to.
 * \param msg Message to deliver
 * \return lbm_enc_sym(SYM_NIL) on failure and lbm_enc_sym(SYM_TRUE) on success.
 */
extern lbm_value lbm_find_receiver_and_send(lbm_cid cid, lbm_value msg);
/** Perform garbage collection,
 * If this is called from another thread than the eval thread, evaluation must be
 * paused! Or there will be lots of trouble!
 *
 * \return 1 on success
 */
extern int lbm_perform_gc(void);
#endif
