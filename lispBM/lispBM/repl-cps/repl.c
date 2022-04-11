/*
    Copyright 2018, 2021 Joel Svensson  svenssonjoel@yahoo.se

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

#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>
#include <termios.h>
#include <ctype.h>

#include "lispbm.h"
#include "extensions/array_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"

#define EVAL_CPS_STACK_SIZE 256
#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256
#define EXTENSION_STORAGE_SIZE 256
#define VARIABLE_STORAGE_SIZE 256

#define WAIT_TIMEOUT 2500

lbm_uint gc_stack_storage[GC_STACK_SIZE];
lbm_uint print_stack_storage[PRINT_STACK_SIZE];
extension_fptr extension_storage[EXTENSION_STORAGE_SIZE];
lbm_value variable_storage[VARIABLE_STORAGE_SIZE];

static volatile bool allow_print = true;

struct termios old_termios;
struct termios new_termios;

static lbm_tokenizer_string_state_t string_tok_state;
static lbm_tokenizer_char_stream_t string_tok;

void setup_terminal(void) {

  tcgetattr(0,&old_termios);
  new_termios = old_termios;
  //new_termios.c_iflag;                     // INPUT MODES
  //new_termios.c_oflag;                     // OUTPUT MODES
  //new_termios.c_cflag;                     // CONTROL MODES
  // LOCAL MODES
  new_termios.c_lflag &= (tcflag_t) ~(ICANON  | ISIG | ECHO);
  new_termios.c_cc[VMIN] = 0;
  new_termios.c_cc[VTIME] = 0;
  //new_termios.c_cc;                       // SPECIAL CHARACTERS

  // LOCAL MODES
  // Turn off:
  //  - canonical mode
  //  - Signal generation for certain characters (INTR, QUIT, SUSP, DSUSP)
  //  VMIN:  Minimal number of characters for noncanonical read.
  //  VTIME: Timeout in deciseconds for noncanonical read.

  tcsetattr(0, TCSANOW, &new_termios);

}

void restore_terminal(void) {
  tcsetattr(0, TCSANOW, &old_termios);
}


int inputline(char *buffer, unsigned int size) {
  int n = 0;
  int c;
  for (n = 0; n < size - 1; n++) {

    c = getchar(); // busy waiting.

    if (c < 0) {
      n--;
      struct timespec s;
      struct timespec r;
      s.tv_sec = 0;
      s.tv_nsec = (long)1000 * 1000;
      nanosleep(&s, &r);
      continue;
    }
    switch (c) {
    case 27:
      break;
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      //putchar(0x8); /* output backspace character */
      //putchar(' ');
      //putchar(0x8);
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        //putchar(c);
        buffer[n] = (char)c;
      } else {
        n -= 1;
      }
      break;
    }
  }
  buffer[size - 1] = 0;
  return 0; // Filled up buffer without reading a linebreak
}

void *eval_thd_wrapper(void *v) {

  lbm_run_eval();

  return NULL;
}

void done_callback(eval_context_t *ctx) {

  char output[1024];

  lbm_cid cid = ctx->id;
  lbm_value t = ctx->r;

  int print_ret = lbm_print_value(output, 1024, t);

  if (print_ret >= 0) {
    printf("<< Context %"PRI_INT" finished with value %s >>\n", cid, output);
  } else {
    printf("<< Context %"PRI_INT" finished with value %s >>\n", cid, output);
  }
  printf("stack max:  %"PRI_UINT"\n", ctx->K.max_sp);
  printf("stack size: %"PRI_UINT"\n", ctx->K.size);
  printf("stack sp:   %"PRI_INT"\n", ctx->K.sp);

  //  if (!eval_cps_remove_done_ctx(cid, &t)) {
  //   printf("Error: done context (%d)  not in list\n", cid);
  //}
  fflush(stdout);
}

uint32_t timestamp_callback() {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (uint32_t)(tv.tv_sec * 1000000 + tv.tv_usec);
}

void sleep_callback(uint32_t us) {
  struct timespec s;
  struct timespec r;
  s.tv_sec = 0;
  s.tv_nsec = (long)us * 1000;
  nanosleep(&s, &r);
}


bool dyn_load(const char *str, const char **code) {

  printf("dyn_load: %s\n", str);

  bool res = false;
  if (strncmp(str, "defun", 5) == 0) {
    *code = "(define defun (macro (name args body) `(define ,name (lambda ,args ,body))))";
    res = true;
  } else if (strncmp(str, "f", 1) == 0) {
    *code = "(defun f (x) (+ x 1))";
    res = true;
  } else if (strncmp(str, "g", 1) == 0) {
    *code = "(defun g (x) (+ 100 (f x)))";
    res = true;
  } else if (strncmp(str, "h", 1) == 0) {
    *code = "(defun h (x) (cons (g x) nil))";
    res = true;
  } else if (strncmp(str, "i", 1) == 0) {
    *code = "(defun i (x) (if (= x 0) 0 (+ (i (- x 1)) x)))";
    res = true;
  }
  return res;
}


lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  if (argn < 1) return lbm_enc_sym(SYM_NIL);

  if (!allow_print) return lbm_enc_sym(SYM_TRUE);

  char output[1024];

  for (int i = 0; i < argn; i ++) {
    lbm_value t = args[i];

    if (lbm_is_ptr(t) && lbm_type_of(t) == LBM_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
      switch (array->elt_type){
      case LBM_TYPE_CHAR: {
        char *data = (char*)array->data;
        printf("%s", data);
        break;
      }
      default:
        return lbm_enc_sym(SYM_NIL);
        break;
      }
    } else if (lbm_type_of(t) == LBM_TYPE_CHAR) {
      printf("%c", lbm_dec_char(t));
    } else {
      lbm_print_value(output, 1024, t);
      printf("%s", output);
    }
  }
  return lbm_enc_sym(SYM_TRUE);
}

char output[128];

static lbm_value ext_range(lbm_value *args, lbm_uint argn) {
        if (argn != 2 || lbm_type_of(args[0]) != LBM_TYPE_I || lbm_type_of(args[1]) != LBM_TYPE_I) {
                return lbm_enc_sym(SYM_EERROR);
        }

        lbm_int start = lbm_dec_i(args[0]);
        lbm_int end = lbm_dec_i(args[1]);

        if (start > end || (end - start) > 100) {
                return lbm_enc_sym(SYM_EERROR);
        }

        lbm_value res = lbm_enc_sym(SYM_NIL);

        for (lbm_int i = end;i >= start;i--) {
                res = lbm_cons(lbm_enc_i(i), res);
        }

        return res;
}



/* load a file, caller is responsible for freeing the returned string */
char * load_file(char *filename) {
  char *file_str = NULL;
  //size_t str_len = strlen(filename);
  //filename[str_len-1] = 0;
  int i = 0;
  while (filename[i] == ' ' && filename[i] != 0) {
    i ++;
  }
  FILE *fp;
  printf("filename: %s\n", &filename[i]);

  if (strlen(&filename[i]) > 0) {
    errno = 0;
    fp = fopen(&filename[i], "r");
    if (!fp) {
      return NULL;
    }
    long fsize_long;
    unsigned int fsize;
    fseek(fp, 0, SEEK_END);
    fsize_long = ftell(fp);
    if (fsize_long <= 0) {
      return NULL;
    }
    fsize = (unsigned int) fsize_long;
    fseek(fp, 0, SEEK_SET);
    file_str = malloc(fsize+1);
    memset(file_str, 0 , fsize+1);
    if (fread(file_str,1,fsize,fp) != fsize) {
      free(file_str);
      file_str = NULL;
    }
    fclose(fp);
  }
  return file_str;
}


void print_ctx_info(eval_context_t *ctx, void *arg1, void *arg2) {
  (void) arg1;
  (void) arg2;

  char output[1024];

  int print_ret = lbm_print_value(output, 1024, ctx->r);

  printf("--------------------------------\n");
  printf("ContextID: %"PRI_UINT"\n", ctx->id);
  printf("Stack SP: %"PRI_UINT"\n",  ctx->K.sp);
  printf("Stack SP max: %"PRI_UINT"\n", ctx->K.max_sp);
  if (print_ret) {
    printf("Value: %s\n", output);
  } else {
    printf("Error: %s\n", output);
  }
}

void ctx_exists(eval_context_t *ctx, void *arg1, void *arg2) {

  lbm_cid id = *(lbm_cid*)arg1;
  bool *exists = (bool*)arg2;

  if (ctx->id == id) {
    *exists = true;
  }
}


void sym_it(const char *str) {
  printf("%s\n", str);
}

static lbm_uint memory[LBM_MEMORY_SIZE_8K];
static lbm_uint bitmap[LBM_MEMORY_BITMAP_SIZE_8K];

char char_array[1024];
lbm_uint word_array[1024];


int main(int argc, char **argv) {
  char str[1024];
  unsigned int len = 1024;
  int res = 0;

  pthread_t lispbm_thd;

  lbm_heap_state_t heap_state;
  unsigned int heap_size = 2048;
  lbm_cons_t *heap_storage = NULL;

  for (int i = 0; i < 1024; i ++) {
    char_array[i] = (char)i;
    word_array[i] = (lbm_uint)i;
  }

  //setup_terminal();

  heap_storage = (lbm_cons_t*)malloc(sizeof(lbm_cons_t) * heap_size);
  if (heap_storage == NULL) {
    return 0;
  }

  if (!lbm_init(heap_storage, heap_size,
                gc_stack_storage, GC_STACK_SIZE,
                memory, LBM_MEMORY_SIZE_8K,
                bitmap, LBM_MEMORY_BITMAP_SIZE_8K,
                print_stack_storage, PRINT_STACK_SIZE,
                extension_storage, EXTENSION_STORAGE_SIZE)) {
    printf("Failed to initialize LispBM\n");
    return 0;
  }

  lbm_set_ctx_done_callback(done_callback);
  lbm_set_timestamp_us_callback(timestamp_callback);
  lbm_set_usleep_callback(sleep_callback);
  lbm_set_dynamic_load_callback(dyn_load);
  lbm_set_printf_callback(printf);

  lbm_variables_init(variable_storage, VARIABLE_STORAGE_SIZE);

  if (lbm_array_extensions_init()) {
    printf("Array extensions loaded\n");
  } else {
    printf("Loading array extensions failed\n");
  }
  
  if (lbm_string_extensions_init()) {
    printf("String extensions loaded\n");
  } else {
    printf("Loading string extensions failed\n");
  }

  if (lbm_math_extensions_init()) {
    printf("Math extensions loaded\n");
  } else {
    printf("Loading math extensions failed\n");
  }
  
  
  res = lbm_add_extension("print", ext_print);
  if (res)
    printf("Extension added.\n");
  else
    printf("Error adding extension.\n");

  res = lbm_add_extension("range", ext_range);
  if (res)
    printf("Extension added.\n");
  else
    printf("Error adding extension.\n");


  /* Start evaluator thread */
  if (pthread_create(&lispbm_thd, NULL, eval_thd_wrapper, NULL)) {
    printf("Error creating evaluation thread\n");
    return 1;
  }

  printf("Lisp REPL started!\n");
  printf("Type :quit to exit.\n");
  printf("     :info for statistics.\n");
  printf("     :load [filename] to load lisp source.\n");

  char output[1024];

  while (1) {
    fflush(stdin);
    printf("# ");
    memset(str, 0 ,len);

    ssize_t n = inputline(str,len);
    fflush(stdout);

    if (n >= 5 && strncmp(str, ":info", 5) == 0) {
      printf("--(LISP HEAP)-----------------------------------------------\n");
      lbm_get_heap_state(&heap_state);
      printf("Heap size: %u Bytes\n", heap_size * 8);
      printf("Used cons cells: %"PRI_INT"\n", heap_size - lbm_heap_num_free());
      printf("Free cons cells: %"PRI_INT"\n", lbm_heap_num_free());
      printf("GC counter: %"PRI_INT"\n", heap_state.gc_num);
      printf("Recovered: %"PRI_INT"\n", heap_state.gc_recovered);
      printf("Recovered arrays: %"PRI_UINT"\n", heap_state.gc_recovered_arrays);
      printf("Marked: %"PRI_INT"\n", heap_state.gc_marked);
      printf("--(Symbol and Array memory)---------------------------------\n");
      printf("Memory size: %"PRI_UINT" Words\n", lbm_memory_num_words());
      printf("Memory free: %"PRI_UINT" Words\n", lbm_memory_num_free());
      printf("Allocated arrays: %"PRI_UINT"\n", heap_state.num_alloc_arrays);
      printf("Symbol table size: %"PRI_UINT" Bytes\n", lbm_get_symbol_table_size());
    }  else if (strncmp(str, ":env", 4) == 0) {
      lbm_value curr = *lbm_get_env_ptr();
      printf("Environment:\r\n");
      while (lbm_type_of(curr) == LBM_TYPE_CONS) {
        res = lbm_print_value(output,1024, lbm_car(curr));
        curr = lbm_cdr(curr);
        printf("  %s\r\n",output);
      }
      printf("Variables:\r\n");
      for (int i = 0; i < lbm_get_num_variables(); i ++) {

        const char *name = lbm_get_variable_name_by_index(i);
        lbm_print_value(output,1024, lbm_get_variable_by_index(i));
        printf("  %s = %s\r\n", name ? name : "error", output);
      }

    }else if (n >= 5 && strncmp(str, ":load", 5) == 0) {
      char *file_str = load_file(&str[5]);
      if (file_str) {

        lbm_create_char_stream_from_string(&string_tok_state,
                                              &string_tok,
                                              file_str);

        /* Get exclusive access to the heap */
        lbm_pause_eval();
        while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
          sleep_callback(10);
        }

        lbm_cid cid = lbm_load_and_eval_program(&string_tok);

        lbm_continue_eval();

        printf("started ctx: %"PRI_UINT"\n", cid);
      }
    } else if (n >= 5 && strncmp(str, ":verb", 5) == 0) {
      lbm_toggle_verbose();
      continue;
    } else if (n >= 4 && strncmp(str, ":pon", 4) == 0) {
      allow_print = true;
      continue;
    } else if (n >= 5 && strncmp(str, ":poff", 5) == 0) {
      allow_print = false;
      continue;
    } else if (strncmp(str, ":ctxs", 5) == 0) {
      printf("****** Running contexts ******\n");
      lbm_running_iterator(print_ctx_info, NULL, NULL);
      printf("****** Blocked contexts ******\n");
      lbm_blocked_iterator(print_ctx_info, NULL, NULL);
      printf("****** Done contexts ******\n");
      lbm_done_iterator(print_ctx_info, NULL, NULL);
    } else if (strncmp(str, ":wait", 5) == 0) {
      int id = atoi(str + 5);
      bool exists = false;
      lbm_done_iterator(ctx_exists, (void*)&id, (void*)&exists);
      if (exists) {
        if (!lbm_wait_ctx((lbm_cid)id, WAIT_TIMEOUT)) {
          printf("Timout while waiting for context %d\n", id);
        }
      }
    } else if (n >= 5 && strncmp(str, ":quit", 5) == 0) {
            break;
    } else if (strncmp(str, ":symbols", 8) == 0) {
      lbm_symrepr_name_iterator(sym_it);
    } else if (strncmp(str, ":reset", 6) == 0) {
      lbm_pause_eval();
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }

      lbm_init(heap_storage, heap_size,
               gc_stack_storage, GC_STACK_SIZE,
               memory, LBM_MEMORY_SIZE_8K,
               bitmap, LBM_MEMORY_BITMAP_SIZE_8K,
               print_stack_storage, PRINT_STACK_SIZE,
               extension_storage, EXTENSION_STORAGE_SIZE);

      lbm_variables_init(variable_storage, VARIABLE_STORAGE_SIZE);

      if (lbm_array_extensions_init()) {
        printf("Array extensions loaded\n");
      } else {
        printf("Loading array extensions failed\n");
      }

      if (lbm_string_extensions_init()) {
        printf("String extensions loaded\n");
      } else {
        printf("Loading string extensions failed\n");
      }

      if (lbm_math_extensions_init()) {
        printf("Math extensions loaded\n");
      } else {
        printf("Loading math extensions failed\n");
      }
  
      
      lbm_add_extension("print", ext_print);
    } else if (strncmp(str, ":prelude", 8) == 0) {

      lbm_pause_eval();
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }

      prelude_load(&string_tok_state,
                   &string_tok);


      lbm_load_and_eval_program(&string_tok);

      lbm_continue_eval();
      /* Something better is needed.
         this sleep ís to ensure the string is alive until parsing
         is done */
      sleep_callback(10000);
    } else if (strncmp(str, ":send", 5) == 0) {

      int id;
      int i_val;

      if (sscanf(str + 5, "%d%d", &id, &i_val) == 2) {
        lbm_pause_eval();
        while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
          sleep_callback(10);
        }

        if (lbm_send_message((lbm_cid)id, lbm_enc_i(i_val)) == 0) {
          printf("Could not send message\n");
        }

        lbm_continue_eval();
      } else {
        printf("Incorrect arguments to send\n");
      }

    } else if (strncmp(str, ":pause", 6) == 0) {
      lbm_pause_eval_with_gc(30);
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }
      printf("Evaluator paused\n");

    } else if (strncmp(str, ":continue", 9) == 0) {
      lbm_continue_eval();
    } else if (strncmp(str, ":step", 5) == 0) {

      int num = atoi(str + 5);

      lbm_step_n_eval((uint32_t)num);
    } else if (strncmp(str, ":undef", 6) == 0) {
      lbm_pause_eval();
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }
      char *sym = str + 7;
      printf("undefining: %s\n", sym);
      printf("%s\n", lbm_undefine(sym) ? "Cleared bindings" : "No definition found");
      lbm_continue_eval();
    } else if (strncmp(str, ":array", 6) == 0) {
      lbm_pause_eval_with_gc(30);
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }
      printf("Evaluator paused\n");

      lbm_value arr_val;
      lbm_share_array(&arr_val, char_array, LBM_TYPE_CHAR,1024);
      lbm_define("c-arr", arr_val);

      lbm_share_array(&arr_val, (char *)word_array, LBM_TYPE_I32,1024);
      lbm_define("i-arr", arr_val);

      lbm_continue_eval();

    } else {
      /* Get exclusive access to the heap */
      lbm_pause_eval();
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }
      printf("loading: %s\n", str);
      lbm_create_char_stream_from_string(&string_tok_state,
                                         &string_tok,
                                         str);
      lbm_cid cid = lbm_load_and_eval_expression(&string_tok);

      lbm_continue_eval();

      printf("started ctx: %"PRI_UINT"\n", cid);
      /* Something better is needed.
         this sleep ís to ensure the string is alive until parsing
         is done */
      sleep_callback(250000);
    }
  }
  free(heap_storage);

  //restore_terminal();

  return 0;
}
