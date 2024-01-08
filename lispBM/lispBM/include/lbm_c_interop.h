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

/** \file lbm_c_interop.h */

#ifndef LBM_C_INTEROP_H_
#define LBM_C_INTEROP_H_

#include "env.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "heap.h"
#include "tokpar.h"
#include "lbm_memory.h"
#include "heap.h"
#include "lbm_types.h"
#include "lbm_channel.h"

#ifdef __cplusplus
extern "C" {
#endif

/** Load and schedule a program for execution.
 *
 * \param tokenizer The tokenizer to read the program from.
 * \param name Name of thread (or NULL) thread doing the load and eval.
 * \return A context id on success or 0 on failure.
 */
  lbm_cid lbm_load_and_eval_program(lbm_char_channel_t *tokenizer, char *name);
/** Load a program while evaluating incrementally.
 *
 * \param tokenizer The tokenizer to read the program from.
 * \param name Name of the thread (or NULL) that performs the incremental load.
 * \return A context id on success or 0 on failure.
 */
  lbm_cid lbm_load_and_eval_program_incremental(lbm_char_channel_t *tokenizer, char *name);
/** Load and schedule an expression for execution.
 *
 * \param tokenizer The tokenizer to read the expression from.
 * \return A context id on success or 0 on failure.
 */
lbm_cid lbm_load_and_eval_expression(lbm_char_channel_t *tokenizer);
/** Load a program and bind it to a symbol in the environment.
 *
 * \param tokenizer The tokenizer to read the program from.
 * \param symbol A string with the name you want the binding to have in the environment.
 * \return A context id on success or 0 on failure.
 */
lbm_cid lbm_load_and_define_program(lbm_char_channel_t *tokenizer, char *symbol);
/** Load an expression and bind it to a symbol in the environment.
 *
 * \param tokenizer The tokenizer to read the expression from.
 * \param symbol A string with the name you want the binding to have in the environment.
 * \return A context id on success or 0 on failure.
 */
lbm_cid lbm_load_and_define_expression(lbm_char_channel_t *tokenizer, char *symbol);

/* Evaluating a definition in a new context */
/** Create a context for a bound expression and schedule it for execution
 *
 * \param symbol The name of the binding to schedule for execution.
 * \return A context if on success or 0 on failure.
 */
lbm_cid lbm_eval_defined_expression(char *symbol);
/** Create a context for a bound program and schedule it for execution
 *
 * \param symbol The name of the binding to schedule for execution.
 * \return A context if on success or 0 on failure.
 */
lbm_cid lbm_eval_defined_program(char *symbol);

/** Send a message to a process running in the evaluator.
 *
 * \param cid Context id of the process to send a message to.
 * \param msg lbm_value that will be sent to the process.
 * \return 1 on success or 0 on failure.
 */
int lbm_send_message(lbm_cid cid, lbm_value msg);

/** Add a definition to the global environment
 *
 * \param symbol Name to bind the data to.
 * \param value The data.
 * \return 1 on success and 0 on failure.
 */
int lbm_define(char *symbol, lbm_value value);
/** Remove a definition from the global environment.
 *
 * \param symbol Name of symbol to undefine in the environment.
 * \return 1 if removed any bindings, 0 otherwise.
 */
int lbm_undefine(char *symbol);
/** Share a C array with LBM. The array should be created while the evaluator
 * is paused and the array should be bound to something before un-pausing. Send the array in
 * a message with \ref lbm_send_message or define it in the global with \ref lbm_define.
 * The data is stored in the array as C values (not encoded as lbm values).
 *
 * \param value Result array value.
 * \param data Pointer to the C array
 * \param num_elt Number of bytes in the array.
 */
int lbm_share_array(lbm_value *value, char *data, lbm_uint num_elt);

/** Share a C array stored in flash with LBM.
 *
 * \param value Pointer to a heap-cell allocated in flash.
 * \param data Pointer to the C array.
 * \param num_elt Number of bytes in the array.
 * \return 1 on success otherwise 0.
 */
int lbm_share_const_array(lbm_value *res, char *flash_ptr, lbm_uint num_elt);
/** Create an array to access from both LBM and C. This function should be called while the evaluator
 * is paused and the array should be bound to something before un-pausing. Send the array in
 * a message with \ref lbm_send_message or define it in the global with \ref lbm_define.
 * The data is stored in lbm_memory as C values (not encoded as lbm values).
 *
 * \param value Result array value.
 * \param num_elt Number of bytes in the array.
 */
int lbm_create_array(lbm_value *value,lbm_uint num_elt);

#ifdef __cplusplus
}
#endif
#endif
