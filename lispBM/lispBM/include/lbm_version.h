/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se

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

/** @file lbm_version.h */

#ifndef LBM_VERSION_H_
#define LBM_VERSION_H_

/** LBM major version */
#define LBM_MAJOR_VERSION 0
/** LBM minor version */
#define LBM_MINOR_VERSION 5
/** LBM patch revision */
#define LBM_PATCH_VERSION 2

/*! \page changelog Changelog

May 1 2022: Version 0.5.2
 - Added lbm_stack_reserve for allocating multiple words on stack
   in one function call (and one check on stack limits).

Apr 19 2022: Version 0.5.2
 - Added a reader_done_callback that is run when a context is done
   with a reading task.
 - Array-literal syntax.
 - Restructure symbol evaluation for efficiency and readability.
 - Rewrite progn to update stack in place when possible.
 - Removed a bunch of convertion back and forth from C and LBM representation
   of continuation identifiers in eval_cps. They are now compared in encoded
   form in the evaluator.
 - Added lbm_cadr and replaced lbm_car(lbm_cdr(x)) with lbm_cadr(x) in
   the evaluator.

Apr 10 2022: Version 0.5.1
 - Removed the prelude.lisp, prelude.xxd step of building LBM.
 - A continuation created by call-cc can be applied to 0 or 1 argument.
   If there are 0 arguments an implicit application to nil takes place.

Mar 26 2022: Version (0.5.0)
 - Optimized code-path for closure applications.
 - 64 and 32 bit support from a single source code
 - Added math extensions library from Benjamin Vedder
 - Added String manipulation extensions library from Benjamin Vedder

Mar 10 2022: Version (0.4.2)
 - Added the lbm_set_error_reason function.

Mar 02 2022: Version (0.4.2)
 - Bug fix in initialization of contexts.

Feb 28 2022: Version (0.4.2)
 - First go at human-readable error messages.
 - Finished contexts are immediately and completely removed.
 - Context ids are now set to the index into the lbm_memory
   where the context structure is stored.

Feb 21 2022: Version (0.4.1)
 - Bug fixes in gc related to arrays


Feb 20 2022: Version (0.4.0)
 - Adds support for macros.
 - Adds call-cc for escaping and abortive continuations.

Feb 17 2022: version 0.3.0
 - Added lbm_undefine to c_interop.
 - Added lbm_share_array to c_interop.
 - Added lbm_create_array to c_interop.
 - #var variables with more efficient storage and lookup.
   variables are set using `setvar`.
 - Spawn optionally takes a number argument before the closure argument
   to specify stack size.
 - Extensions are stored in an array and occupy a range of dedicated symbol values.

Feb 14 2022: version 0.2.0
 - Added GEQ >= and LEQ <= comparisons.

Feb 13 2022: version 0.1.1
 - Bug fix in handling of environments in progn.

Feb 11 2022: version 0.1.0
  - First state to be given a numbered version (0.1.0)
*/



#endif
