/** \file env.h */
/*
    Copyright 2018 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef ENV_H_
#define ENV_H_

#include "lbm_types.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GLOBAL_ENV_ROOTS 32
#define GLOBAL_ENV_MASK  0x1F

//environment interface
/** Initialize the global environment. This sets the global environment to NIL
 *
 * \return 1
 */
int lbm_init_env(void);
/**
 *
 * \return the global environment
 */
lbm_value *lbm_get_global_env(void);
/** Copy the spine of an environment. The list structure is
 * recreated but the values themselves are not copied but rather
 * just referenced.
 *
 * \param env Environment to copy.
 * \return Copy of environment.
 */
lbm_value lbm_env_copy_spine(lbm_value env);
/** Lookup a value in an environment.
 * \param res Result stored here
 * \param sym The key to look for in the environment
 * \param env The environment to search for the key.
 * \return True on success or false otherwise.
 */
bool lbm_env_lookup_b(lbm_value *res, lbm_value sym, lbm_value env);
/** Lookup a value in the global environment.
 * \param res Result stored here
 * \param sym The key to look for in the environment
 * \param env The environment to search for the key.
 * \return True on success or false otherwise.
 */
bool lbm_global_env_lookup(lbm_value *res, lbm_value sym);
/** Lookup a value in from the global environment.
 *
 * \param sym The key to look for in the environment
 * \param env The environment to search for the key.
 * \return The value bound to key or lbm_enc_sym(SYM_NOT_FOUND).
 */
lbm_value lbm_env_lookup(lbm_value sym, lbm_value env);
/** Create a new binding on the environment or replace an old binding.
 *
 * \param env Environment to modify.
 * \param key A symbol to associate with a value.
 * \param val The value.
 * \return The modified environment or lbm_enc_sym(SYM_MERROR) if GC needs to be run.
 */
lbm_value lbm_env_set(lbm_value env, lbm_value key, lbm_value val);
/** Create a new binding on the environment without destroying the old value.
 *  If the old value is unused (the key-value pair) it will be freed by GC
 *  at next convenience.
 *
 * \param env Environment to modify.
 * \param key A symbol to associate with a value.
 * \param val The value.
 * \return The modified environment or lbm_enc_sym(SYM_MERROR) if GC needs to be run.
 */
lbm_value lbm_env_set_functional(lbm_value env, lbm_value key, lbm_value val);
/** Modifies an existing binding on the environment.
 *
 * \param env The environment to modify.
 * \param key The key.
 * \param val The new value to associate with the key.
 * \return The modified environment of Success and lbm_enc_sym(SYM_NOT_FOUND) if the key does not exist.
 */
lbm_value lbm_env_modify_binding(lbm_value env, lbm_value key, lbm_value val);
/** Removes a binding (destructively) from the input environment.
 * \param env Environment to modify.
 * \param key Key to remove from environment.
 * \return Updated environment or not_found symbol.
 */
lbm_value lbm_env_drop_binding(lbm_value env, lbm_value key);
// Internal use
/** Extend an environment given a list of keys and a list of values.
 *
 * \param params The list of keys.
 * \param args The list of values.
 * \param env0 An initial environment to extend
 * \return The extended environment on success and lbm_enc_sym(SYM_MERROR) if GC needs to be run.
 */
lbm_value lbm_env_build_params_args(lbm_value params,
                                    lbm_value args,
                                    lbm_value env0);

#ifdef __cplusplus
}
#endif
#endif
