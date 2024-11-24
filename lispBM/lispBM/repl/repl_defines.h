/*
    Copyright 2024 Joel Svensson  svenssonjoel@yahoo.se

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


#ifndef REPL_DEFINES_H_
#define REPL_DEFINES_H_


#define REPL_EXIT_SUCCESS 0
#define REPL_EXIT_OUT_OF_MEMORY_WHILE_FLATTENING 1
#define REPL_EXIT_FLATTENING_MAXIMUM_DEPTH 2
#define REPL_EXIT_CIRCULAR_VALUE 3
#define REPL_EXIT_FATAL_ERROR_WHILE_FLATTENING 4
#define REPL_EXIT_FLAT_VALUE_BUFFER_TOO_SMALL 5
#define REPL_EXIT_VALUE_CANNOT_BE_FLATTENED 6
#define REPL_EXIT_INVALID_KEY_IN_ENVIRONMENT 7
#define REPL_EXIT_LBM_EVENT_QUEUE_FULL 8
#define REPL_EXIT_INVALID_VALUE_IN_ENV_FILE 9
#define REPL_EXIT_OUT_OF_MEMORY_WHILE_PROCESSING_ENV_FILE 10
#define REPL_EXIT_INVALID_VALUE_SIZE_IN_ENV_FILE 11
#define REPL_EXIT_INVALID_KEY_IN_ENV_FILE 12
#define REPL_EXIT_ZERO_LENGTH_KEY_IN_ENV_FILE 13
#define REPL_EXIT_INVALID_ENV_FILE 14
#define REPL_EXIT_UNABLE_TO_OPEN_ENV_FILE 15
#define REPL_EXIT_UNABLE_TO_INIT_LBM 16
#define REPL_EXIT_ERROR_FLATTEN_NO_MEM 17
#define REPL_EXIT_UNABLE_TO_CREATE_SYMBOL 18
#define REPL_EXIT_INVALID_SOURCE_FILE 19
#define REPL_EXIT_UNABLE_TO_ACCESS_SYMBOL_STRING 20
#define REPL_EXIT_CRITICAL_ERROR 21
#define REPL_EXIT_SYMBOL_TABLE_TOO_BIG 22
#define REPL_EXIT_ENV_POPULATION_TIMEOUT 23
#define REPL_EXIT_INVALID_EXPRESSION 24

const char *repl_exit_message[25] =
  {"Goodbye.",
   "ERROR: Out of memory while flattening a value.",
   "ERROR: Maximum cons depth reached while flattening value.",
   "ERROR: Possibly circular value found while flattening.",
   "ERROR: Fatal error while flattening value.",
   "ERROR: Buffer for flat value is too small.",
   "ERROR: Value cannot be flattened.",
   "ERROR: An invalid key was found in the environment.",
   "ERROR: The event queue is full.",
   "ERROR: Invalid value found in environment file.",
   "ERROR: Out of memory while processing environment file.",
   "ERROR: Invalid value size found in environment file.",
   "ERROR: Invalid key found in environment file.",
   "ERROR: Zero length key found in environment file.",
   "ERROR: Invalid environment file.",
   "ERROR: Unable to open environment file.",
   "ERROR: Unable to initialize the LispBM runtime system.",
   "ERROR: Unable to allocate memory for flat value creation.",
   "ERROR: Unable to create a symbol.",
   "ERROR: Invalid source file.",
   "ERROR: Unable to access symbol string representation.",
   "ERROR: Critical Error",
   "ERROR: Symbol table too large",
   "ERROR: Populating the environment from an env file timed out"
   "ERROR: Invalid expression."
  };

#endif
