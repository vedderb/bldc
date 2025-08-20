/*
    Copyright 2023, 2024, 2025 Joel Svensson        svenssonjoel@yahoo.se

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

#ifdef LBM_OPT_RUNTIME_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_RUNTIME_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif


#ifdef FULL_RTS_LIB
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
#endif

lbm_value ext_eval_set_quota(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(1);
  uint32_t q = lbm_dec_as_u32(args[0]);
#ifdef LBM_USE_TIME_QUOTA
  lbm_set_eval_time_quota(q);
#else
  lbm_set_eval_step_quota(q);
#endif
  return ENC_SYM_TRUE;
}

lbm_value ext_hide_trapped_error(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  lbm_set_hide_trapped_error(true);
  return ENC_SYM_TRUE;
}

lbm_value ext_show_trapped_error(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  lbm_set_hide_trapped_error(false);
  return ENC_SYM_TRUE;
}

#ifdef FULL_RTS_LIB
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
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_uint ix = lbm_dec_as_u32(args[0]) & GLOBAL_ENV_MASK;
    return lbm_get_global_env()[ix];
  }
  return ENC_SYM_TERROR;
}

lbm_value ext_local_env_get(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  eval_context_t *ctx = lbm_get_current_context();
  return ctx->curr_env;
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

lbm_value ext_env_drop(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_symbol(args[0])) {
    r = lbm_env_drop_binding(args[1], args[0]);
  }
  return r;
}

lbm_value ext_global_env_size(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn; // ignores any and all arguments.
  return lbm_enc_u(lbm_get_global_env_size());
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
        lbm_heap_state.gc_stack.sp = 0;  // should already be 0
        return ENC_SYM_TRUE;
      }
      return ENC_SYM_MERROR;
    }
  }
  return ENC_SYM_TERROR;
}

lbm_value ext_is_64bit(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  #ifndef LBM64
  return ENC_SYM_NIL;
  #else
  return ENC_SYM_TRUE;
  #endif
}

lbm_value ext_symbol_table_size(lbm_uint *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  return lbm_enc_u(lbm_get_symbol_table_size());
}

lbm_value ext_symbol_table_size_flash(lbm_uint *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  return lbm_enc_u(lbm_get_symbol_table_size_flash());
}

lbm_value ext_symbol_table_size_names(lbm_uint *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  return lbm_enc_u(lbm_get_symbol_table_size_names());
}

lbm_value ext_symbol_table_size_names_flash(lbm_uint *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  return lbm_enc_u(lbm_get_symbol_table_size_names_flash());
}

lbm_value ext_is_always_gc(lbm_uint *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  #ifdef LBM_ALWAYS_GC
  return ENC_SYM_TRUE;
  #else
  return ENC_SYM_NIL;
  #endif
}

#endif

#if defined(LBM_USE_EXT_MAILBOX_GET) || defined(FULL_RTS_LIB)

void find_cid(eval_context_t *ctx, void *arg1, void *arg2) {
  lbm_cid id = (lbm_cid)arg1;
  if (ctx->id == id) {
    *(eval_context_t**)arg2 = ctx;
  }
}


lbm_value ext_mailbox_get(lbm_uint *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  eval_context_t *ctx = NULL;

  if (argn == 1 && lbm_is_number(args[0])) {
    res = ENC_SYM_NIL;
    lbm_cid cid = lbm_dec_as_i32(args[0]);
    lbm_all_ctxs_iterator(find_cid, (void*)cid, (void*)&ctx);
    if (ctx) {
      uint32_t num_mail = ctx->num_mail;
      lbm_value ls = (lbm_heap_allocate_list(num_mail));
      res = ls;
      if (lbm_is_ptr(ls)) {
        lbm_value curr = ls;
        int i = 0;
        while (lbm_is_ptr(curr)) {
          lbm_set_car(curr, ctx->mailbox[i++]);
          curr = lbm_cdr(curr);
        }
      }
    }
  }
  return res;
}
#endif


void lbm_runtime_extensions_init(void) {

#ifdef FULL_RTS_LIB
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
#endif

#if defined(LBM_USE_EXT_MAILBOX_GET) || defined(FULL_RTS_LIB)
    lbm_add_extension("mailbox-get", ext_mailbox_get);
#endif
#ifndef FULL_RTS_LIB
    lbm_add_extension("set-eval-quota", ext_eval_set_quota);
    lbm_add_extension("hide-trapped-error", ext_hide_trapped_error);
    lbm_add_extension("show-trapped-error", ext_show_trapped_error);
#else
    lbm_add_extension("is-always-gc",ext_is_always_gc);
    lbm_add_extension("set-eval-quota", ext_eval_set_quota);
    lbm_add_extension("hide-trapped-error", ext_hide_trapped_error);
    lbm_add_extension("show-trapped-error", ext_show_trapped_error);
    lbm_add_extension("mem-num-free", ext_memory_num_free);
    lbm_add_extension("mem-longest-free", ext_memory_longest_free);
    lbm_add_extension("mem-size", ext_memory_size);
    lbm_add_extension("word-size", ext_memory_word_size);
    lbm_add_extension("lbm-version", ext_lbm_version);
    lbm_add_extension("lbm-heap-state", ext_lbm_heap_state);
    lbm_add_extension("env-get", ext_env_get);
    lbm_add_extension("env-set", ext_env_set);
    lbm_add_extension("env-drop", ext_env_drop);
    lbm_add_extension("local-env-get", ext_local_env_get);
    lbm_add_extension("global-env-size", ext_global_env_size);
    lbm_add_extension("set-gc-stack-size", ext_set_gc_stack_size);
    lbm_add_extension("is-64bit", ext_is_64bit);
    lbm_add_extension("symtab-size", ext_symbol_table_size);
    lbm_add_extension("symtab-size-flash", ext_symbol_table_size_flash);
    lbm_add_extension("symtab-size-names", ext_symbol_table_size_names);
    lbm_add_extension("symtab-size-names-flash", ext_symbol_table_size_names_flash);
#endif
}
