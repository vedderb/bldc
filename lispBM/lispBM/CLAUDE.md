# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

LispBM is an embeddable Lisp/Scheme-like programming language designed for microcontrollers and embedded systems. It features:
- Support for both 32-bit and 64-bit platforms
- Concurrent programming with message passing and process monitoring (inspired by Erlang)
- Multiple runtime environments: ChibiOS, FreeRTOS, ZephyrOS, bare-metal, and Linux
- Extensions system for integrating C functions
- Garbage collection with multiple strategies

## Build System

The project uses multiple Makefiles in different directories for various build targets:
- **`repl/Makefile`** - Builds the REPL application and interactive tools
- **`tests/Makefile`** - Builds test binaries for the main test suite
- **`tests/c_unit/Makefile`** - Builds C unit test executables

### REPL Build (repl/ directory)
- **32-bit REPL**: `make` or `make all`
- **64-bit REPL**: `make all64`
- **With SDL support**: `make sdl` or `make sdl64`
- **Coverage build**: `make cov`
- **Debug build**: `make debug`
- **Install**: `make install` (installs to ~/.local/bin/lbm)

Dependencies: libreadline, libhistory, libpng, optionally SDL2

## Testing

### Main Test Suite
Located in `tests/` directory:
- **All tests**: `./run_tests.sh` (comprehensive 32-bit test suite)
- **64-bit tests**: `./run_tests64.sh`
- **Coverage tests**: `./run_tests_cov.sh`
- **GC tests**: `./run_tests_gc.sh`
- **Reverse GC tests**: `./run_tests_gc_rev.sh`
- **Time quota tests**: `./run_tests_time.sh` and `./run_tests64_time.sh`
- **Image tests**: `./run_image_tests.sh`
- **REPL tests**: `./run_repl_tests.sh`
- **SDL tests**: `./run_sdl_tests.sh`

### Test Binaries
Built in `tests/` via Makefile:
- `test_lisp_code_cps` - Main test runner
- `test_lisp_code_cps_64` - 64-bit version
- `test_lisp_code_cps_gc` - With aggressive GC
- `test_lisp_code_cps_revgc` - With reverse GC
- `test_lisp_code_cps_cov` - Coverage instrumented

### Test Structure
- `tests/tests/` - Unit test files (.lisp)
- `tests/repl_tests/` - REPL-specific tests
- `tests/image_tests/` - Heap image serialization tests
- Test scripts automatically build required binaries before running

### Edge Case Tests
The REPL test suite includes comprehensive edge case tests for fundamental operations:
- **test_arithmetic_edge_cases.lisp** - Tests +, -, *, /, mod with wrong types and edge cases
- **test_comparison_edge_cases.lisp** - Tests =, !=, <, >, <=, >= with invalid arguments
- **test_list_operations_edge_cases.lisp** - Tests cons, car, cdr, list, append, length, ix edge cases
- **test_type_operations_edge_cases.lisp** - Tests type-of, list?, number?, conversions with edge cases
- **test_integer_division_float_args.lisp** - Tests integer division // with floating-point arguments
- **test_random_extensions_edge_cases.lisp** - Tests seed and random functions with invalid inputs
- **test_runtime_extensions_edge_cases.lisp** - Tests runtime introspection functions with edge cases

These tests ensure robust error handling and predictable behavior for embedded applications.

### Test Output Conventions
All test suites should print **"SUCCESS"** when all tests pass:
- **C unit tests** (`tests/c_unit/`) - Each test executable should print "SUCCESS" on completion
- **REPL tests** (`tests/repl_tests/`) - Each `.lisp` test file should print "SUCCESS" when passing
- **Image tests** (`tests/image_tests/`) - Should print "SUCCESS" when serialization/deserialization works
- **SDL tests** (`tests/sdl_tests/`) - Should print "SUCCESS" when graphics operations complete

### C Unit Test Development
When creating C unit tests in `tests/c_unit/`, use the standard initialization pattern:

```c
#define _GNU_SOURCE // MAP_ANON
#define _POSIX_C_SOURCE 200809L // nanosleep?
#include <stdlib.h>
#include <stdio.h>
// ... other standard includes

#include "lispbm.h"
#include "init/start_lispbm.c"

static int test_init(void) {
  return start_lispbm_for_tests();
}
```

The `start_lispbm_for_tests()` function:
- Handles complete LispBM system initialization
- Sets up heap, memory management, and all subsystems
- Provides a clean environment for testing LispBM C API functions
- Should be called at the beginning of each test function that requires LispBM
- Automatically manages cleanup and reinitialization between tests

**Important**: Use `kill_eval_after_tests()` at the end of C unit tests that start threads (such as evaluation threads) which are not explicitly killed or waited for during the test. This function ensures proper cleanup by:
- Killing the evaluator thread before test function scope ends
- Preventing use-after-free errors when evaluator threads access test function local variables
- Ensuring clean test isolation and preventing state leakage between tests

Example usage:
```c
int test_something_with_threads() {
  if (!test_init()) return 0;
  
  // Test code that may start evaluator threads
  // ...
  
  kill_eval_after_tests(); // Clean up before function returns
  return 1;
}
```

## Architecture

### Core Components
- **Evaluator** (`src/eval_cps.c`): Continuation-passing style evaluator
- **Heap** (`src/heap.c`): Memory management and garbage collection
- **Symbols** (`src/symrepr.c`): Symbol representation and interning
- **Parser** (`src/tokpar.c`): Tokenizer and parser
- **Environment** (`src/env.c`): Variable binding management
- **Extensions** (`src/extensions.c`): C function integration system

### Extensions System
Located in `src/extensions/`:
- Array operations, display functions, math, string manipulation
- Runtime system functions, TTF font rendering
- Custom extensions can be added via `lbm_add_extension()`

### Platform Support
- `platform/` directory contains platform-specific code
- `platform/linux/` for POSIX systems
- `platform/chibios/`, `platform/freertos/`, `platform/zephyr/` for RTOSes

### Memory Model
- Fixed-size heap of cons cells
- Separate memory pools for different object types
- Multiple GC strategies: copying GC, reverse pointer GC
- Configurable heap sizes and memory pools

## Key Features for Development

### Configuration Flags
- `DLBM64`: Enable 64-bit support
- `DLBM_ALWAYS_GC`: Force GC on every allocation (testing)
- `DLBM_USE_GC_PTR_REV`: Use reverse pointer GC
- `DFULL_RTS_LIB`: Enable full runtime library
- `DVISUALIZE_HEAP`: Enable heap visualization

### Important Files
- `include/lispbm.h`: Main API header
- `lispbm.mk`: Build system include file
- `doc/lbmref.md`: Language reference documentation
- `repl/examples/`: Example LispBM programs

### LispBM Language Features

#### Error Handling
- **`trap` function**: Returns a list with error handling results
  - Success: `'(exit-ok value)` where `value` is the result
  - Error: `'(exit-error error-symbol)` where `error-symbol` is the specific error type
  - Common error symbols: `eval_error`, `type_error`, `division_by_zero`, `out_of_memory`, etc.
  - Example: `(trap (sin "string"))` returns `'(exit-error eval_error)`
  - **Important**: `trap` always returns either `'(exit-ok something)` or `'(exit-error something)`
  - Some functions return error symbols directly (like `'type-symbol`) without needing `trap`

#### Expression Sequences
- **`progn`**: Traditional syntax for evaluating multiple expressions in sequence
  - `(progn expr1 expr2 expr3)` evaluates all expressions and returns the last value
  - Allows context switches between expressions (concurrent processes can interleave)
- **Brace syntax `{}`**: Alternative syntax for `progn`
  - `{ expr1 expr2 expr3 }` is equivalent to `(progn expr1 expr2 expr3)`
  - More concise and readable than traditional `progn`
- **`atomic`**: Like `progn` but prevents context switches
  - `(atomic expr1 expr2 expr3)` evaluates expressions atomically
  - Disallows context switches between expressions (critical sections)
  - Only 3 constructs in LispBM allow expression sequences: `progn`, `{}`, and `atomic`

#### Data Types
- **Arrays**: Two types available
  - Byte arrays: `[1 2 3]` syntax
  - Lisp arrays: `[| 1 2 3 |]` syntax
- **Variable Assignment**: Use `setq` instead of `set!`
- **Lists**: Standard Lisp lists with `'(...)` syntax
- **Type names**: Use correct LispBM type names in tests
  - `'type-i` for integers, `'type-u` for unsigned integers
  - `'type-array` for strings, `'type-list` for lists
  - `'type-symbol` for symbols

#### Function Behaviors
- **Single argument arithmetic**: `(+ 5)` returns `5`, `(- 5)` returns `-5`, `(* 7)` returns `7`
- **Single argument comparisons**: `(= 5)` and `(<= 5)` return `t`, others return `nil`
- **Random functions**: `random` returns unsigned integers (`type-u`), ignores all arguments
- **Integer division**: `//` properly converts floating-point arguments to integer results
- **Type conversions**: `to-i` and `to-float` return `0`/`0.0f32` for invalid types (no errors)
- **Runtime functions**: Most ignore arguments; `lbm-heap-state` returns `nil` for invalid symbols

#### Equality and Comparison Operators
- **`=` (numerical equality)**: Use for comparing numbers
  - Example: `(= 42 42)` returns `t`
  - Example: `(= 3.14 3.14)` returns `t`
  - Preferred for all numerical comparisons in tests and code
- **`eq` (identity equality)**: Use for comparing symbols, booleans, and object identity
  - Example: `(eq 'symbol 'symbol)` returns `t`
  - Example: `(eq t t)` returns `t`
  - Example: `(eq nil nil)` returns `t`
- **Rule**: Always use `=` for numbers and `eq` for non-numerical values

#### Advanced Color System
LispBM provides an advanced color system for use with `disp-render`:

- **`img-color`**: Creates color objects for advanced rendering
  - `'regular`: Simple 1-1 color mapping `(img-color 'regular 0xRRGGBB)`
  - `'gradient_x`/`'gradient_y`: Dynamic gradients calculated at render time
  - `'gradient_x_pre`/`'gradient_y_pre`: Pre-calculated gradient buffers (max width 512)
  - Gradients support `'repeat` or `'mirrored` repeat modes
  
- **`img-color-setpre`**: Modify colors in pre-calculated gradient buffers
  - `(img-color-setpre color pos new-color)`
  - Position is relative to gradient start (ignores offset)
  - Only works with `gradient_x_pre`/`gradient_y_pre` color objects
  
- **`img-color-getpre`**: Read colors from pre-calculated gradient buffers
  - `(img-color-getpre color pos)`
  - Useful for testing and validating gradient color mappings

**Important**: Advanced color objects can only be used with `disp-render`, not with regular drawing functions.

## LispBM Function Reference Index

This comprehensive index contains **106 distinct functions and operations** organized by category for quick reference:

### Arithmetic Operations
- **`+`** - Addition of arbitrary number of values - `(+ expr1 ... exprN)`
- **`-`** - Subtraction from a value - `(- expr1 ... exprN)`
- **`*`** - Multiplication of arbitrary number of values - `(* expr1 ... exprN)`
- **`/`** - Division - `(/ expr1 ... exprN)`
- **`mod`** - Modulo operation - `(mod expr1 expr2)`
- **`//`** - Integer division (floors floating point results) - `(// expr1 ... exprN)`

### Comparison Operations
- **`eq`** - Structural equality comparison - `(eq expr1 ... exprN)`
- **`not-eq`** - Structural inequality comparison - `(not-eq expr1 ... exprN)`
- **`=`** - Numerical equality comparison - `(= expr1 ... exprN)`
- **`>`** - Greater than comparison - `(> expr1 ... exprN)`
- **`<`** - Less than comparison - `(< expr1 ... exprN)`
- **`>=`** - Greater than or equal comparison - `(>= expr1 ... exprN)`
- **`<=`** - Less than or equal comparison - `(<= expr1 ... exprN)`

### Boolean Operations
- **`and`** - Logical AND with short-circuit evaluation - `(and expr1 ... exprN)`
- **`or`** - Logical OR with short-circuit evaluation - `(or expr1 ... exprN)`
- **`not`** - Logical negation - `(not expr)`

### Type Predicates
- **`list?`** - Test if value is a list - `(list? expr)`
- **`number?`** - Test if value is a number - `(number? expr)`

### Bit-level Operations
- **`shl`** - Shift left operation - `(shl value positions)`
- **`shr`** - Shift right operation - `(shr value positions)`
- **`bitwise-and`** - Bitwise AND - `(bitwise-and val1 val2)`
- **`bitwise-or`** - Bitwise OR - `(bitwise-or val1 val2)`
- **`bitwise-xor`** - Bitwise XOR - `(bitwise-xor val1 val2)`
- **`bitwise-not`** - Bitwise negation - `(bitwise-not value)`

### Constants and Literals
- **`nil`** - Empty list and false value
- **`t`** - True value
- **`true`** - Alias for `t`
- **`false`** - Alias for `nil`

### Quoting and Metaprogramming
- **`quote`** - Prevent evaluation of expression - `(quote expr)` or `'expr`
- **`` ` ``** - Quasiquotation for templating - `` `expr``
- **`,`** - Unquote within quasiquotation - `,expr`
- **`,@`** - Unquote-splicing within quasiquotation - `,@expr`

### Built-in Operations
- **`identity`** - Returns argument unchanged - `(identity expr)`
- **`rest-args`** - Access extra arguments in functions - `(rest-args)` or `(rest-args index)`
- **`set`** - Destructively update a binding - `(set symbol-expr value-expr)`
- **`setvar`** - Set variable value - `(setvar symbol-expr value-expr)`
- **`undefine`** - Remove global binding - `(undefine symbol)`
- **`eval`** - Evaluate expression - `(eval expr)`
- **`eval-program`** - Evaluate program with multiple expressions - `(eval-program expr-list)`
- **`apply`** - Apply function to argument list - `(apply func arg-list)`
- **`read`** - Parse string into expression - `(read string)`
- **`read-program`** - Parse string into program - `(read-program string)`
- **`read-eval-program`** - Parse and evaluate string - `(read-eval-program string)`
- **`type-of`** - Get type of value - `(type-of expr)`
- **`sym2str`** - Convert symbol to string - `(sym2str symbol)`
- **`str2sym`** - Convert string to symbol - `(str2sym string)`
- **`sym2u`** - Convert symbol to unsigned integer - `(sym2u symbol)`
- **`u2sym`** - Convert unsigned integer to symbol - `(u2sym uint)`
- **`gc`** - Trigger garbage collection - `(gc)`

### Special Forms
- **`if`** - Conditional expression - `(if condition then-expr else-expr)`
- **`cond`** - Multi-branch conditional - `(cond (test1 expr1) ... (testN exprN))`
- **`lambda`** - Create anonymous function - `(lambda params body)`
- **`closure`** - Function with captured environment - `(closure params body env)`
- **`let`** - Local variable binding - `(let bindings body)`
- **`loop`** - Iterative loop construct - `(loop bindings condition body)`
- **`define`** - Global variable definition - `(define symbol expr)`
- **`setq`** - Set variable (symbol not evaluated) - `(setq symbol expr)`
- **`progn`** - Sequential evaluation - `(progn expr1 ... exprN)`
- **`{`** - Shorthand for `progn` - `{ expr1 ... exprN }`
- **`}`** - Closing brace for `{`
- **`var`** - Local binding in progn - `(var symbol expr)`
- **`trap`** - Error handling - `(trap expr)`
- **`atomic`** - Uninterruptible execution - `(atomic expr1 ... exprN)`

### List Operations
- **`car`** - Get first element of list/pair - `(car list)`
- **`first`** - Alias for `car` - `(first list)`
- **`cdr`** - Get rest of list/pair - `(cdr list)`
- **`rest`** - Alias for `cdr` - `(rest list)`
- **`cons`** - Create cons cell - `(cons car cdr)`
- **`.`** - Dot notation for pairs - `(expr1 . expr2)`
- **`list`** - Create proper list - `(list expr1 ... exprN)`
- **`length`** - Get list length - `(length list)`
- **`range`** - Generate range of integers - `(range start end)`
- **`append`** - Concatenate lists - `(append list1 list2)`
- **`ix`** - Index into list - `(ix list index)`
- **`setix`** - Destructively update list element - `(setix list index value)`
- **`member`** - Test membership in list - `(member value list)`
- **`setcar`** - Destructively update car - `(setcar cons value)`
- **`setcdr`** - Destructively update cdr - `(setcdr cons value)`
- **`take`** - Take first n elements - `(take list n)`
- **`drop`** - Drop first n elements - `(drop list n)`
- **`reverse`** - Reverse list - `(reverse list)`
- **`rotate`** - Rotate list elements - `(rotate list distance)`
- **`merge`** - Merge sorted lists - `(merge comparator list1 list2)`
- **`sort`** - Sort list - `(sort comparator list)`

### Association Lists (alists)
- **`acons`** - Add key-value pair to alist - `(acons key value alist)`
- **`assoc`** - Look up value by key - `(assoc alist key)`
- **`cossa`** - Look up key by value - `(cossa alist value)`
- **`setassoc`** - Destructively update alist - `(setassoc alist key value)`

### Byte Buffers
- **`bufcreate`** - Create byte buffer - `(bufcreate size)`
- **`buflen`** - Get buffer length - `(buflen buffer)`
- **`bufget-[X]`** - Get value from buffer (various types) - `(bufget-u8 buffer index)`
- **`bufset-[X]`** - Set value in buffer (various types) - `(bufset-u8 buffer index value)`
- **`bufclear`** - Clear buffer - `(bufclear buffer value start end)`

### Arrays
- **`array`** - Create array from list - `(array type list)`
- **`mkarray`** - Create array with initial value - `(mkarray type size init-val)`

### Defragmentable Memory
- **`dm-create`** - Create defragmentable memory - `(dm-create size)`
- **`dm-alloc`** - Allocate from defragmentable memory - `(dm-alloc dm size)`

### Pattern Matching
- **`match`** - Pattern matching - `(match expr (pattern1 result1) ... (patternN resultN))`
- **`no_match`** - Default match case
- **`_`** - Wildcard pattern
- **`?`** - Variable binding pattern - `(? var)`

### Concurrency
- **`spawn`** - Create concurrent process - `(spawn closure arg1 ... argN)`
- **`spawn-trap`** - Spawn with exit trapping - `(spawn-trap closure arg1 ... argN)`
- **`self`** - Get current thread ID - `(self)`
- **`wait`** - Wait for process to finish - `(wait pid)`
- **`yield`** - Yield execution for microseconds - `(yield microseconds)`
- **`sleep`** - Sleep for seconds - `(sleep seconds)`
- **`exit-ok`** - Exit thread successfully - `(exit-ok value)`
- **`exit-error`** - Exit thread with error - `(exit-error error)`
- **`kill`** - Terminate another thread - `(kill thread-id value)`

### Message Passing
- **`send`** - Send message to process - `(send pid message)`
- **`recv`** - Receive message (blocking) - `(recv (pattern1 result1) ... (patternN resultN))`
- **`recv-to`** - Receive with timeout - `(recv-to timeout (pattern1 result1) ... (patternN resultN))`
- **`set-mailbox-size`** - Change mailbox size - `(set-mailbox-size size)`

### Serialization
- **`flatten`** - Serialize value to byte array - `(flatten value)`
- **`unflatten`** - Deserialize from byte array - `(unflatten flat-value)`

### Macros
- **`macro`** - Define macro - `(macro params body)`

### Call/CC
- **`call-cc`** - Call with current continuation - `(call-cc func)`
- **`call-cc-unsafe`** - Unsafe call with current continuation - `(call-cc-unsafe func)`

### Error Symbols
- **`read_error`** - Reading/parsing error
- **`type_error`** - Type mismatch error
- **`eval_error`** - Evaluation error
- **`out_of_memory`** - Memory exhaustion error
- **`fatal_error`** - Fatal system error
- **`out_of_stack`** - Stack overflow error
- **`division_by_zero`** - Division by zero error
- **`variable_not_bound`** - Unbound variable error

### Flash Memory
- **`@const-start`** - Start of constants section
- **`@const-end`** - End of constants section
- **`move-to-flash`** - Move value to flash memory - `(move-to-flash symbol ...)`

**Note**: Special forms (like `if`, `cond`, `lambda`, `let`, etc.) are evaluated differently than regular functions. Many functions support variable arguments. Destructive operations modify data in-place.

## Code Style Guidelines

### C Code Indentation Style
When writing or modifying C code in this project, use the following indentation style (consistent with eval_cps.c):

- **2-space indentation** for all code blocks (no tabs)
- **Consistent spacing** throughout all nesting levels
- **Compact style** suitable for deeply nested continuation-passing style code

**Examples:**
```c
static void function_name(params) {
  if (condition) {
    while (loop_condition) {
      if (nested_condition) {
        statement;
      }
    }
  }
}

switch (value) {
case CONSTANT:
  statement;
  break;
case OTHER:
  statement;
  break;
default:
  statement;
}
```

### Common Development Workflow
1. Modify core code in `src/`
2. Build library: `make clean && make`
3. Test changes: `cd tests && ./run_tests.sh`
4. For REPL testing: `cd repl && make && ./repl`
5. For coverage: `cd tests && ./run_tests_cov.sh`