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

//environment interface
/** Initialize the global environment. This sets the global environment to NIL
 *
 * \return 1
 */
extern int lbm_init_env(void);
/**
 * \deprecated There is no value in returning a pointer to the environment. Use lbm_get_env
 * \return A pointer to the global environment variable.
 */
extern lbm_value *lbm_get_env_ptr(void);
/**
 *
 * \return the global environment
 */
extern lbm_value lbm_get_env(void);
/** Performs a shallow copy of a proper list. A shallow copy does
 *  not recurse into the elements of the list to copy
 *  those as well. So if the list contains complex elements, the
 *  original list and the copy will share these elements on the heap.
 *
 * \param env List to copy.
 * \return Shallow copy of input list.
 */
extern lbm_value lbm_env_copy_shallow(lbm_value env);
/** Lookup a value in from the global environment.
 *
 * \param sym The key to look for in the environment
 * \param env The environment to search for the key.
 * \return The value bound to key or lbm_enc_sym(SYM_NOT_FOUND).
 */
extern bool lbm_env_lookup_b(lbm_value *res, lbm_value sym, lbm_value env);
/** Lookup a value in from the global environment.
 *
 * \param sym The key to look for in the environment
 * \param env The environment to search for the key.
 * \return The value bound to key or lbm_enc_sym(SYM_NOT_FOUND).
 */
extern lbm_value lbm_env_lookup(lbm_value sym, lbm_value env);
/** Create a new binding on the environment or replace an old binding.
 *
 * \param env Environment to modify.
 * \param key A symbol to associate with a value.
 * \param val The value.
 * \return The modified environment or lbm_enc_sym(SYM_MERROR) if GC needs to be run.
 */
extern lbm_value lbm_env_set(lbm_value env, lbm_value key, lbm_value val);
/** Modifies an existing binding on the environment.
 *
 * \param env The environment to modify.
 * \param key The key.
 * \param val The new value to associate with the key.
 * \return The modified environment of Success and lbm_enc_sym(SYM_NOT_FOUND) if the key does not exist.
 */
extern lbm_value lbm_env_modify_binding(lbm_value env, lbm_value key, lbm_value val);
// Internal use
/** Extend an environment given a list of keys and a list of values.
 *
 * \param params The list of keys.
 * \param args The list of values.
 * \param env0 An initial environment to extend
 * \return The extended environment on success and lbm_enc_sym(SYM_MERROR) if GC needs to be run.
 */
extern lbm_value lbm_env_build_params_args(lbm_value params,
                                           lbm_value args,
                                           lbm_value env0);

#endif
