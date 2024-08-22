/*
  Copyright 2024 Joel Svensson  svenssonjoel@yahoo.se
            2022 Benjamin Vedder benjamin@vedder.se

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

#include "repl_exts.h"

#include <unistd.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/time.h>
#include <sys/wait.h>
#include "extensions/array_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/runtime_extensions.h"
#include "extensions/set_extensions.h"

// Macro expanders

static lbm_value make_list(int num, ...) {
  va_list arguments;
  va_start (arguments, num);
  lbm_value res = ENC_SYM_NIL;
  for (int i = 0; i < num; i++) {
    res = lbm_cons(va_arg(arguments, lbm_value), res);
  }
  va_end (arguments);
  return lbm_list_destructive_reverse(res);
}

static lbm_uint sym_res;
static lbm_uint sym_loop;
static lbm_uint sym_break;
static lbm_uint sym_brk;
static lbm_uint sym_rst;
static lbm_uint sym_return;

static lbm_value ext_me_defun(lbm_value *argsi, lbm_uint argn) {
  if (argn != 3) {
    return ENC_SYM_EERROR;
  }

  lbm_value name = argsi[0];
  lbm_value args = argsi[1];
  lbm_value body = argsi[2];

  // (define name (lambda args body))

  return make_list(3,
                   lbm_enc_sym(SYM_DEFINE),
                   name,
                   make_list(3,
                             lbm_enc_sym(SYM_LAMBDA),
                             args,
                             body));
}

static lbm_value ext_me_defunret(lbm_value *argsi, lbm_uint argn) {
  if (argn != 3) {
    return ENC_SYM_EERROR;
  }

  lbm_value name = argsi[0];
  lbm_value args = argsi[1];
  lbm_value body = argsi[2];

  // (def name (lambda args (call-cc (lambda (return) body))))

  return make_list(3,
                   lbm_enc_sym(SYM_DEFINE),
                   name,
                   make_list(3,
                             lbm_enc_sym(SYM_LAMBDA),
                             args,
                             make_list(2,
                                       lbm_enc_sym(SYM_CALLCC),
                                       make_list(3,
                                                 lbm_enc_sym(SYM_LAMBDA),
                                                 make_list(1, lbm_enc_sym(sym_return)),
                                                 body))));
}

static lbm_value ext_me_loopfor(lbm_value *args, lbm_uint argn) {
  if (argn != 5) {
    return ENC_SYM_EERROR;
  }

  lbm_value it = args[0];
  lbm_value start = args[1];
  lbm_value cond = args[2];
  lbm_value update = args[3];
  lbm_value body = args[4];

  // (let ((loop (lambda (it res break) (if cond (loop update body break) res)))) (call-cc (lambda (brk) (loop start nil brk))))

  return make_list(3,
                   lbm_enc_sym(SYM_LET),
                   make_list(1,
                             make_list(2,
                                       lbm_enc_sym(sym_loop),
                                       make_list(3,
                                                 lbm_enc_sym(SYM_LAMBDA),
                                                 make_list(3, it, lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
                                                 make_list(4,
                                                           lbm_enc_sym(SYM_IF),
                                                           cond,
                                                           make_list(4, lbm_enc_sym(sym_loop), update, body, lbm_enc_sym(sym_break)),
                                                           lbm_enc_sym(sym_res))))),
                   make_list(2,
                             lbm_enc_sym(SYM_CALLCC),
                             make_list(3,
                                       lbm_enc_sym(SYM_LAMBDA),
                                       make_list(1, lbm_enc_sym(sym_brk)),
                                       make_list(4, lbm_enc_sym(sym_loop), start, ENC_SYM_NIL, lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_me_loopwhile(lbm_value *args, lbm_uint argn) {
  if (argn != 2) {
    return ENC_SYM_EERROR;
  }

  lbm_value cond = args[0];
  lbm_value body = args[1];

  // (let ((loop (lambda (res break) (if cond (loop body break) res)))) (call-cc (lambda (brk) (loop nil brk))))

  return make_list(3,
                   lbm_enc_sym(SYM_LET),
                   make_list(1,
                             make_list(2,
                                       lbm_enc_sym(sym_loop),
                                       make_list(3,
                                                 lbm_enc_sym(SYM_LAMBDA),
                                                 make_list(2, lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
                                                 make_list(4,
                                                           lbm_enc_sym(SYM_IF),
                                                           cond,
                                                           make_list(3, lbm_enc_sym(sym_loop), body, lbm_enc_sym(sym_break)),
                                                           lbm_enc_sym(sym_res))))),
                   make_list(2,
                             lbm_enc_sym(SYM_CALLCC),
                             make_list(3,
                                       lbm_enc_sym(SYM_LAMBDA),
                                       make_list(1, lbm_enc_sym(sym_brk)),
                                       make_list(3, lbm_enc_sym(sym_loop), ENC_SYM_NIL, lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_me_looprange(lbm_value *args, lbm_uint argn) {
  if (argn != 4) {
    return ENC_SYM_EERROR;
  }

  lbm_value it = args[0];
  lbm_value start = args[1];
  lbm_value end = args[2];
  lbm_value body = args[3];

  // (let ((loop (lambda (it res break) (if (< it end) (loop (+ it 1) body break) res)))) (call-cc (lambda (brk) (loop start nil brk))))

  return make_list(3,
                   lbm_enc_sym(SYM_LET),
                   make_list(1,
                             make_list(2,
                                       lbm_enc_sym(sym_loop),
                                       make_list(3,
                                                 lbm_enc_sym(SYM_LAMBDA),
                                                 make_list(3, it, lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
                                                 make_list(4,
                                                           lbm_enc_sym(SYM_IF),
                                                           make_list(3, lbm_enc_sym(SYM_LT), it, end),
                                                           make_list(4, lbm_enc_sym(sym_loop), make_list(3, lbm_enc_sym(SYM_ADD), it, lbm_enc_i(1)), body, lbm_enc_sym(sym_break)),
                                                           lbm_enc_sym(sym_res))))),
                   make_list(2,
                             lbm_enc_sym(SYM_CALLCC),
                             make_list(3,
                                       lbm_enc_sym(SYM_LAMBDA),
                                       make_list(1, lbm_enc_sym(sym_brk)),
                                       make_list(4, lbm_enc_sym(sym_loop), start, ENC_SYM_NIL, lbm_enc_sym(sym_brk)))));
}

static lbm_value ext_me_loopforeach(lbm_value *args, lbm_uint argn) {
  if (argn != 3) {
    return ENC_SYM_EERROR;
  }

  lbm_value it = args[0];
  lbm_value lst = args[1];
  lbm_value body = args[2];

  // (let ((loop (lambda (it rst res break) (if (eq it nil) res (loop (car rst) (cdr rst) body break))))) (call-cc (lambda (brk) (loop (car lst) (cdr lst) nil brk))))

  return make_list(3,
                   lbm_enc_sym(SYM_LET),
                   make_list(1,
                             make_list(2,
                                       lbm_enc_sym(sym_loop),
                                       make_list(3,
                                                 lbm_enc_sym(SYM_LAMBDA),
                                                 make_list(4, it, lbm_enc_sym(sym_rst), lbm_enc_sym(sym_res), lbm_enc_sym(sym_break)),
                                                 make_list(4,
                                                           lbm_enc_sym(SYM_IF),
                                                           make_list(3, lbm_enc_sym(SYM_EQ), it, ENC_SYM_NIL),
                                                           lbm_enc_sym(sym_res),
                                                           make_list(5,
                                                                     lbm_enc_sym(sym_loop),
                                                                     make_list(2, lbm_enc_sym(SYM_CAR), lbm_enc_sym(sym_rst)),
                                                                     make_list(2, lbm_enc_sym(SYM_CDR), lbm_enc_sym(sym_rst)),
                                                                     body,
                                                                     lbm_enc_sym(sym_break))
                                                           )))),
                   make_list(2,
                             lbm_enc_sym(SYM_CALLCC),
                             make_list(3,
                                       lbm_enc_sym(SYM_LAMBDA),
                                       make_list(1, lbm_enc_sym(sym_brk)),
                                       make_list(5,
                                                 lbm_enc_sym(sym_loop),
                                                 make_list(2, lbm_enc_sym(SYM_CAR), lst),
                                                 make_list(2, lbm_enc_sym(SYM_CDR), lst),
                                                 ENC_SYM_NIL,
                                                 lbm_enc_sym(sym_brk)))));
}


// Math

static lbm_value ext_rand(lbm_value *args, lbm_uint argn) {
  if (argn != 0 && argn != 1) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_TERROR;
  }

  unsigned int seed = 0;
  bool seed_set = false;

  if (argn == 1) {
    if (!lbm_is_number(args[0])) {
      lbm_set_error_reason((char*)lbm_error_str_no_number);
      return ENC_SYM_TERROR;
    }

    seed = lbm_dec_as_u32(args[0]);
    seed_set = true;
  }

  if (seed_set) {
    srand(seed);
  }

  return lbm_enc_i32(rand());
}

static lbm_value ext_rand_max(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return lbm_enc_i32(RAND_MAX);
}


// BITS

/*
 * args[0]: Initial value
 * args[1]: Offset in initial value to modify
 * args[2]: Value to modify with
 * args[3]: Size in bits of value to modify with
 */
static lbm_value ext_bits_enc_int(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(4)
    uint32_t initial = lbm_dec_as_u32(args[0]);
  uint32_t offset = lbm_dec_as_u32(args[1]);
  uint32_t number = lbm_dec_as_u32(args[2]);
  uint32_t bits = lbm_dec_as_u32(args[3]);
  initial &= ~((0xFFFFFFFF >> (32 - bits)) << offset);
  initial |= (number << (32 - bits)) >> (32 - bits - offset);

  if (initial > ((1 << 27) - 1)) {
    return lbm_enc_i32((int32_t)initial);
  } else {
    return lbm_enc_i((int32_t)initial);
  }
}

/*
 * args[0]: Value
 * args[1]: Offset in initial value to get
 * args[2]: Size in bits of value to get
 */
static lbm_value ext_bits_dec_int(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(3)
    uint32_t val = lbm_dec_as_u32(args[0]);
  uint32_t offset = lbm_dec_as_u32(args[1]);
  uint32_t bits = lbm_dec_as_u32(args[2]);
  val >>= offset;
  val &= 0xFFFFFFFF >> (32 - bits);

  if (val > ((1 << 27) - 1)) {
    return lbm_enc_i32((int32_t)val);
  } else {
    return lbm_enc_i((int32_t)val);
  }
}



// TIME

uint32_t timestamp(void) {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (uint32_t)(tv.tv_sec * 1000000 + tv.tv_usec);
}

static lbm_value ext_systime(lbm_value *args, lbm_uint argn) {

  uint32_t time = timestamp();

  return lbm_enc_u32(time);
}

static lbm_value ext_secs_since(lbm_value *args, lbm_uint argn) {
  uint32_t t_now = timestamp();

  if (argn != 1 || !lbm_is_number(args[0])) return ENC_SYM_EERROR;

  uint32_t t_then = lbm_dec_as_u32(args[0]);
  uint32_t diff = t_now - t_then;
  return lbm_enc_float((float)diff / 1000000.0f);
}


static bool allow_print = true;
void set_allow_print(bool on) {
  allow_print = on;
}

lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  if (argn < 1) return lbm_enc_sym(SYM_NIL);

  if (!allow_print) return lbm_enc_sym(SYM_TRUE);

  char output[1024];

  for (unsigned int i = 0; i < argn; i ++) {
    lbm_value t = args[i];

    if (lbm_is_ptr(t) && lbm_type_of(t) == LBM_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
      char *data = (char*)array->data;
      printf("%s", data);
    } else {
      lbm_print_value(output, 1024, t);
      printf("%s", output);
    }
  }
  printf("\n");
  return lbm_enc_sym(SYM_TRUE);
}

// ------------------------------------------------------------
// File IO

static const char *lbm_file_handle_desc = "File-Handle";

typedef struct {
  FILE *fp;
} lbm_file_handle_t;

static bool file_handle_destructor(lbm_uint value) {

  lbm_file_handle_t *h = (lbm_file_handle_t *)value;
  if (h->fp) {
    fclose(h->fp);
  }
  return true;
}

static bool is_file_handle(lbm_value h) {
  return ((lbm_uint)lbm_get_custom_descriptor(h) == (lbm_uint)lbm_file_handle_desc);
}

static lbm_value ext_fopen(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    FILE *fp = NULL;

    char *filename = lbm_dec_str(args[0]);
    char *mode = lbm_dec_str(args[1]);

    fp = fopen(filename, mode);
    if (fp) {
      lbm_file_handle_t *mem = lbm_malloc(sizeof(lbm_file_handle_t));
      if (!mem) {
        fclose(fp);
        return ENC_SYM_MERROR;
      }
      mem->fp = fp;
      lbm_custom_type_create((lbm_uint)mem,
                             file_handle_destructor,
                             lbm_file_handle_desc,
                             &res);
    } else {
      return ENC_SYM_NIL;
    }
  }
  return res;
}

static lbm_value ext_fwrite(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      is_file_handle(args[0]) &&
      lbm_is_array_r(args[1])) {

    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
    if (array) {
      fwrite(array->data, 1, array->size, h->fp);
      fflush(h->fp);
      res = ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;

}

static lbm_value ext_fwrite_str(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      is_file_handle(args[0]) &&
      lbm_is_array_r(args[1])) {

    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
    if (array) {
      fwrite(array->data, 1, strlen((char*)array->data), h->fp);
      fflush(h->fp);
      res = ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;
}

static lbm_value ext_fwrite_value(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      is_file_handle(args[0])) {
    res = ENC_SYM_NIL;
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);

    lbm_set_max_flatten_depth(10000);
    int32_t fv_size = flatten_value_size(args[1], 0);
    if (fv_size > 0) {
      lbm_flat_value_t fv;
      fv.buf = malloc((uint32_t)fv_size);
      if (fv.buf) {
        fv.buf_size = (uint32_t)fv_size;
        fv.buf_pos = 0;
        if (flatten_value_c(&fv, args[1]) == FLATTEN_VALUE_OK) {
          fwrite(fv.buf, 1, (size_t)fv_size, h->fp);
          fflush(h->fp);
          res = ENC_SYM_TRUE;
        } else {
          printf("ALERT: Unable to flatten result value\n");
        }
      } else {
        printf("ALERT: Out of memory to allocate result buffer\n");
      }
    } else {
      printf("ALERT: Incorrect FV size: %d \n", fv_size);
    }
  }
  return res;
}


static bool all_arrays(lbm_value *args, lbm_uint argn) {
  bool r = true;
  for (uint32_t i = 0; i < argn; i ++) {
    r = r && lbm_is_array_r(args[i]);
  }
  return r;
}

static lbm_value ext_exec(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  int pid;

  if (all_arrays(args, argn) && argn >= 1) {
    char **strs = malloc(argn * sizeof(char*) + 1);
    for (uint32_t i = 0; i < argn; i ++) {
      strs[i] = lbm_dec_str(args[i]);
    }
    strs[argn] = NULL;
    fflush(stdout);
    int status = 0;
    pid = fork();
    if (pid == 0) {
      execvp(strs[0], &strs[1]);
      exit(0);
    } else {
      waitpid(pid, &status, 0);
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

static lbm_value ext_unsafe_call_system(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    int r = system(lbm_dec_str(args[0]));
    if (r == 0)  {
      res = ENC_SYM_TRUE;
    } else {
      res = lbm_enc_i(r);
    }
  }
  return res;
}

// ------------------------------------------------------------
// Init

int init_exts(void) {

  if (!lbm_array_extensions_init()) {
    return 0;
  }
  if (!lbm_string_extensions_init()) {
    return 0;
  }
  if (!lbm_math_extensions_init()) {
    return 0;
  }
  if (!lbm_runtime_extensions_init(false)) {
    return 0;
  }
  if (!lbm_set_extensions_init()) {
    return 0;
  }

  lbm_add_symbol_const("a01", &sym_res);
  lbm_add_symbol_const("a02", &sym_loop);
  lbm_add_symbol_const("break", &sym_break);
  lbm_add_symbol_const("a03", &sym_brk);
  lbm_add_symbol_const("a04", &sym_rst);
  lbm_add_symbol_const("return", &sym_return);

  lbm_add_extension("unsafe-call-system", ext_unsafe_call_system);
  lbm_add_extension("exec", ext_exec);
  lbm_add_extension("fopen", ext_fopen);
  lbm_add_extension("fwrite", ext_fwrite);
  lbm_add_extension("fwrite-str", ext_fwrite_str);
  lbm_add_extension("fwrite-value", ext_fwrite_value);
  lbm_add_extension("print", ext_print);
  lbm_add_extension("systime", ext_systime);
  lbm_add_extension("secs-since", ext_secs_since);

  // Math
  lbm_add_extension("rand", ext_rand);
  lbm_add_extension("rand-max", ext_rand_max);

  // Bit operations
  lbm_add_extension("bits-enc-int", ext_bits_enc_int);
  lbm_add_extension("bits-dec-int", ext_bits_dec_int);

  // Macro expanders
  lbm_add_extension("me-defun", ext_me_defun);
  lbm_add_extension("me-defunret", ext_me_defunret);
  lbm_add_extension("me-loopfor", ext_me_loopfor);
  lbm_add_extension("me-loopwhile", ext_me_loopwhile);
  lbm_add_extension("me-looprange", ext_me_looprange);
  lbm_add_extension("me-loopforeach", ext_me_loopforeach);

  return 1;
}


// Dynamic loader

static const char* functions[] = {
  "(defun iota (n) (range n))",

  "(defun foldl (f init lst)"
  "(if (eq lst nil) init (foldl f (f init (car lst)) (cdr lst))))",

  "(defun foldr (f init lst)"
  "(if (eq lst nil) init (f (car lst) (foldr f init (cdr lst)))))",

  "(defun apply (f lst) (eval (cons f lst)))",

  "(defun zipwith (f x y)"
  "(let ((map-rec (lambda (f res lst ys)"
  "(if (eq lst nil)"
  "(reverse res)"
  "(map-rec f (cons (f (car lst) (car ys)) res) (cdr lst) (cdr ys))))))"
  "(map-rec f nil x y)))",

  "(defun filter (f lst)"
  "(let ((filter-rec (lambda (f lst ys)"
  "(if (eq lst nil)"
  "(reverse ys)"
  "(if (f (car lst))"
  "(filter-rec f (cdr lst) (cons (car lst) ys))"
  "(filter-rec f (cdr lst) ys))))))"
  "(filter-rec f lst nil)"
  "))",

  "(defun str-cmp-asc (a b) (< (str-cmp a b) 0))",
  "(defun str-cmp-dsc (a b) (> (str-cmp a b) 0))",

  "(defun second (x) (car (cdr x)))",
  "(defun third (x) (car (cdr (cdr x))))",

  "(defun abs (x) (if (< x 0) (- x) x))",
};

static const char* macros[] = {
  "(define defun (macro (name args body) (me-defun name args body)))",
  "(define defunret (macro (name args body) (me-defunret name args body)))",
  "(define loopfor (macro (it start cnd update body) (me-loopfor it start cnd update body)))",
  "(define loopwhile (macro (cnd body) (me-loopwhile cnd body)))",
  "(define looprange (macro (it start end body) (me-looprange it start end body)))",
  "(define loopforeach (macro (it lst body) (me-loopforeach it lst body)))",
  "(define loopwhile-thd (macro (stk cnd body) `(spawn ,stk (fn () (loopwhile ,cnd ,body)))))",
};

static bool strmatch(const char *str1, const char *str2) {
  size_t len = strlen(str1);

  if (str2[len] != ' ') {
    return false;
  }

  bool same = true;
  for (unsigned int i = 0;i < len;i++) {
    if (str1[i] != str2[i]) {
      same = false;
      break;
    }
  }

  return same;
}

bool dynamic_loader(const char *str, const char **code) {
  for (unsigned int i = 0; i < (sizeof(macros) / sizeof(macros[0]));i++) {
    if (strmatch(str, macros[i] + 8)) {
      *code = macros[i];
      return true;
    }
  }

  for (unsigned int i = 0; i < (sizeof(functions) / sizeof(functions[0]));i++) {
    if (strmatch(str, functions[i] + 7)) {
      *code = functions[i];
      return true;
    }
  }

  return false;
}
