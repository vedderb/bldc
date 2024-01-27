/*
    Copyright 2022, 2023 Joel Svensson  svenssonjoel@yahoo.se

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

#ifdef __cplusplus
extern "C" {
#endif

/** LBM major version */
#define LBM_MAJOR_VERSION 0
/** LBM minor version */
#define LBM_MINOR_VERSION 22
/** LBM patch revision */
#define LBM_PATCH_VERSION 0

#define LBM_VERSION_STRING "0.22.0"

/*! \page changelog Changelog
DEC 26 2024: Version 0.22.0
  - Built-in sort operation on lists.
  - Built-in list-merge operation.
  - Bugfix in map.
  - Literal forms for special characters.


NOV 28 2024: Version 0.21.0
  - Removed partial evaluation.
  - Added a built-in loop.
  - Modification to built-in implementation of map.
  - Addition of pointer-reversal garbage collector. Not on by default.
  - Improved error messages.

NOV 1 2024: Version 0.20.0
  - Added lbm_set_error_suspect function to enable extension authors to point out in more detail what is wrong.
  - Improvement to error messages in some cases.
  - Changed behavior of set family on functions when variable is not already bound (now an error).
  - Fix of bug in flat_value handling.

OCT 8 2024: Version 0.19.0
  - Error message and callback on GC stack overflow.
  - Functions for gc stack size statistics added.
  - GC does not look at constant values.
  - Changes to environment handling during pattern matches.

AUG 26 2023: Version 0.18.0
  - Removed wait-for flags
  - Fix bug in unblock_unboxed when unblocking with error value.

JUL 29 2023: Version 0.17.0
  - Addition of a timeout functionality to blocked contexts.
  - recv-to special form added for receives with a timeout.
  - block_context_from_extension_timeout function added.
  - Unified sleeping and blocked queues.
  - Added a new optional argument to spawn and spawn-trap that can be used to provide a name for the thread.
  - Added profiler functionality.

JUL 16 2023: Version 0.16.0
  - Addition of flat values as a type in the language.
  - Addition of kill function for termination of threads.

JUN 29 2023: version 0.15.0
  - Bug fix in lift_array_flash.
  - Bug fix in map.
  - Bug fix in reader.
  - Bug fix in dynamic load.
  - Bug fix in quasiquotation expansion.
  - 

JUN 8 2023: Version 0.14.0
  - wait-for that blocks code unless a flag is set.
  - Bug fix in undefine.
  - Lots of cleaning and refactoring.

MAJ 5 2023: Version 0.13.0
  - Changed behavior of closure application to zero args. Used to be equivalent
    to application to nil.
  - Removed make-env and in-env.
  - Refactoring for readability. allocate_closure in eval_cps.

APR 30 2023: Version 0.12.0
  - added make-env and in-env for a kind of namespace management.
  - Deeply nested errors are resolved using longjmp.

Apr 4 2023: Version 0.11.0
  - Incremental read evaluates expressions as soon as possible while reading.
  - move-to-flash for storing constant parts of program in flash.
  - All arrays are now byte-arrays. [type-X 1 2 3]-syntax removed.

Mar 19 2023: Version 0.10.0
  - Added deconstructive let bindings with optional dont-care fields.
  - Added (var x (....)) for local bindings in progn.
  - Added setq
  - Curly brackets { .... } syntax as sugar over progn.

Feb 18 2023: Version 0.9.0
  - Arrays in flat_value are stored verbatim, not as ptr.
  - Mutex locking granularity changed in multiple places.

Feb 10 2023: Version 0.8.1
  - Flat representation of heap values added.
  - Added queue locking to GC
  - As an experiment blocked contexts are unblocked by the evaluator in a safe state.

Jan 28 2023: Version 0.8.0
  - Changed return value of define from being the bound symbol to
    being the value.
  - Many of the more general extensions from Benjamin's BLDC repository
    has moved into the LispBM source code.

Dec 11: Version 0.7.1
  - Changes to heap_allocate_cell for readability and perhaps performance.
  - Added heap_allocate_list for allocation of multiple cells at once.

Nov 9: Version 0.7.1
  - Bugfix: string literal lengths.
  - not-eq and != added.
  - Corrected behaviour for eval when applied to no argument.
  - lbm_memory operations are protected by mutex.
  - Fixes to eval-program.
  - Added multiple condition conditional function called cond.

Oct 31: Version 0.7.1
  - Added optional boolean guards to pattern matches.
  - Built in map and reverse.

Oct 16: Version 0.7.0
  - Refactoring for evaluation speed.
  - Removed possibility to step through code.
  - Oldest message is removed on mailbox full.
  - Added spawn-trap inspired by Erlang (but simplified).

Sep 25: Version 0.7.0
  - Removed namespaces (they were too restricted).
  - Mailboxes are now stored in arrays of default size 10 mails.
    Mailbox size can be changed using set-mailbox-size.

Sep 16 2022: Version 0.6.0
  - Source code can be streamed onto the heap. This means there is no need
    for a large buffer on the MCU (or area of flash) to parse source code
    from

Sep 5 2022: Version 0.6.0
  - Refactoring of array-reader. Array reading is nolonger done monolithically
    inside of the tokpar framework, but rather as a cooperation between the
    evaluator and the tokenizer.

Sep 3 2022: Version 0.6.0
  - Round-robin scheduling + Addition of an Atomic construct to use with care.

Aug 1 2022: Version 0.5.4
  - Easing use of the LBM library from C++ code.

Jul 25 2022: Version 0.5.4
  - lbm_define can now create variables (#var) in variable memory from
    the C side of things.
  - Simple namespaces.

Jul 18 2022: Version 0.5.4
  - Added pattern matching support for i64, u64 and double.
  - Fixed issue with pattern matching on i32, u32.

Jul 17 2022: Version 0.5.4
  - Refactoring with readability in focus.
  - Computing encodings of commonly used symbol constants (for eval_cps) at compile time
    rather then repeatedly at runtime.

Jul 13 2022: Version 0.5.4
  - Added function that lookups based on the second field in assoc structures.
    Called it "cossa" as it is like assoc but backwards.

Jul 4 2022: Version 0.5.4
  - Added possibility to partially apply closures. A partially applied closure
    is again a closure.

May 24 2022: Version 0.5.3
  - Fixed bug related to float-array literals not accepting whole numbers unless containing a decimal (0).

May 22 2022: Version 0.5.3
  - Fixed bug that could cause problems with call-cc on 64bit platforms.
  - bind_to_key_rest continuation refactoring to use indexing into stack.
  - Fix evaluator bug in progn that made tail-call not fire properly when there
    is only one expr in the progn sequence.

May 10 2022: Version 0.5.3
 - symbols starting with "ext-" will be allocated into the extensions-list
   and can on the VESC version of lispbm be dynamically bound to newly loaded
   extensions at runtime.

May 8 2022: Version 0.5.2
 - Added new macros for 10, 12 and 14K lbm_memory sizes.

May 5 2022: Version 0.5.2
 - Line and column numbers associated with read errors.
 - More explanatory descriptions in error messages related to read errors.

May 2 2022: Version 0.5.2
 - Performance tweaks to the evaluator. Small but positive effect.

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


#ifdef __cplusplus
}
#endif
#endif
