/*
    Copyright 2023 Joel Svensson        svenssonjoel@yahoo.se

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

#include <lbm_memory.h>
#include <heap.h>
#include <eval_cps.h>
#include <extensions.h>
#include <lbm_utils.h>
#include <lbm_version.h>
#include <env.h>

static lbm_uint sym_heap_size;
static lbm_uint sym_heap_bytes;
static lbm_uint sym_num_alloc_cells;
static lbm_uint sym_num_alloc_arrays;
static lbm_uint sym_num_gc;
static lbm_uint sym_num_gc_marked;
static lbm_uint sym_num_gc_recovered_cells;
static lbm_uint sym_num_gc_recovered_arrays;
static lbm_uint sym_num_least_free;
static lbm_uint sym_num_last_free;

lbm_value ext_eval_set_quota(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  uint32_t q = lbm_dec_as_u32(args[0]);
  lbm_set_eval_step_quota(q);
  return ENC_SYM_TRUE;
}

lbm_value ext_memory_num_free(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  lbm_uint n = lbm_memory_num_free();
  return lbm_enc_i((lbm_int)n);
}

lbm_value ext_memory_longest_free(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  lbm_uint n = lbm_memory_longest_free();
  return lbm_enc_i((lbm_int)n);
}

lbm_value ext_memory_size(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  lbm_uint n = lbm_memory_num_words();
  return lbm_enc_i((lbm_int)n);
}

lbm_value ext_memory_word_size(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  return lbm_enc_i((lbm_int)sizeof(lbm_uint));
}

lbm_value ext_lbm_version(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  lbm_value version = lbm_heap_allocate_list_init(3,
                                                  lbm_enc_i(LBM_MAJOR_VERSION),
                                                  lbm_enc_i(LBM_MINOR_VERSION),
                                                  lbm_enc_i(LBM_PATCH_VERSION));
  return version;
}

lbm_value ext_lbm_heap_state(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;

  lbm_heap_state_t hs;
  lbm_get_heap_state(&hs);

  if (argn == 1 &&
      lbm_is_symbol(args[0])) {
    lbm_uint s = lbm_dec_sym(args[0]);
    if (s == sym_heap_size) {
      res = lbm_enc_u(hs.heap_size);
    } else if (s == sym_heap_bytes) {
      res = lbm_enc_u(hs.heap_bytes);
    } else if (s == sym_num_alloc_cells) {
      res = lbm_enc_u(hs.num_alloc);
    } else if (s == sym_num_alloc_arrays) {
      res = lbm_enc_u(hs.num_alloc_arrays);
    } else if (s == sym_num_gc) {
      res = lbm_enc_u(hs.gc_num);
    } else if (s == sym_num_gc_marked) {
      res = lbm_enc_u(hs.gc_marked);
    } else if (s == sym_num_gc_recovered_cells) {
      res = lbm_enc_u(hs.gc_recovered);
    } else if (s == sym_num_gc_recovered_arrays) {
      res = lbm_enc_u(hs.gc_recovered_arrays);
    } else if (s == sym_num_least_free) {
      res = lbm_enc_u(hs.gc_least_free);
    } else if (s == sym_num_last_free) {
      res = lbm_enc_u(hs.gc_last_free);
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;
}

lbm_value ext_env_get(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_uint ix = lbm_dec_as_u32(args[0]) & GLOBAL_ENV_MASK;
    return lbm_get_global_env()[ix];
  }
  return ENC_SYM_TERROR;
}

lbm_value ext_env_set(lbm_value *args, lbm_uint argn) {
  if (argn == 2 && lbm_is_number(args[0])) {
    lbm_uint ix = lbm_dec_as_u32(args[0]) & GLOBAL_ENV_MASK;
    lbm_value *glob_env = lbm_get_global_env();
    glob_env[ix] = args[1];
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_NIL;
}

lbm_value ext_set_gc_stack_size(lbm_value *args, lbm_uint argn) {
  if (argn == 1) {
    if (lbm_is_number(args[0])) {
      uint32_t n = lbm_dec_as_u32(args[0]);
      lbm_uint *new_stack = lbm_malloc(n * sizeof(lbm_uint));
      if (new_stack) {
        lbm_free(lbm_heap_state.gc_stack.data);
        lbm_heap_state.gc_stack.data = new_stack;
        lbm_heap_state.gc_stack.size = n;
        lbm_heap_state.gc_stack.max_sp = 0;
        lbm_heap_state.gc_stack.sp = 0;  // should already be 0
        return ENC_SYM_TRUE;
      }
      return ENC_SYM_MERROR;
    }
  }
  return ENC_SYM_TERROR;
}
 
bool lbm_runtime_extensions_init(bool minimal) {

  if (!minimal) {
    lbm_add_symbol_const("get-heap-size", &sym_heap_size);
    lbm_add_symbol_const("get-heap-bytes", &sym_heap_bytes);
    lbm_add_symbol_const("get-num-alloc-cells", &sym_num_alloc_cells);
    lbm_add_symbol_const("get-num-alloc-arrays", &sym_num_alloc_arrays);
    lbm_add_symbol_const("get-gc-num", &sym_num_gc);
    lbm_add_symbol_const("get-gc-num-marked", &sym_num_gc_marked);
    lbm_add_symbol_const("get-gc-num-recovered-cells", &sym_num_gc_recovered_cells);
    lbm_add_symbol_const("get-gc-num-recovered-arrays", &sym_num_gc_recovered_arrays);
    lbm_add_symbol_const("get-gc-num-least-free", &sym_num_least_free);
    lbm_add_symbol_const("get-gc-num-last-free", &sym_num_last_free);
  }

  bool res = true;
  if (minimal) {
    res = res && lbm_add_extension("set-eval-quota", ext_eval_set_quota);
  } else {
    res = res && lbm_add_extension("set-eval-quota", ext_eval_set_quota);
    res = res && lbm_add_extension("mem-num-free", ext_memory_num_free);
    res = res && lbm_add_extension("mem-longest-free", ext_memory_longest_free);
    res = res && lbm_add_extension("mem-size", ext_memory_size);
    res = res && lbm_add_extension("word-size", ext_memory_word_size);
    res = res && lbm_add_extension("lbm-version", ext_lbm_version);
    res = res && lbm_add_extension("lbm-heap-state", ext_lbm_heap_state);
    res = res && lbm_add_extension("env-get", ext_env_get);
    res = res && lbm_add_extension("env-set", ext_env_set);
    res = res && lbm_add_extension("set-gc-stack-size", ext_set_gc_stack_size);
  }
  return res;
}
