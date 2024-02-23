/*
    Copyright 2018, 2021, 2022, 2024 Joel Svensson  svenssonjoel@yahoo.se

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
#include <stdarg.h>
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>
#include <ctype.h>
#include <getopt.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "lispbm.h"
#include "lbm_flat_value.h"
#include "lbm_prof.h"
#include "extensions/array_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/runtime_extensions.h"

#include "lbm_custom_type.h"
#include "lbm_channel.h"
#include "lbm_version.h"

#include "repl_exts.h"

#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256
#define EXTENSION_STORAGE_SIZE 256
#define WAIT_TIMEOUT 2500
#define STR_SIZE 1024
#define CONSTANT_MEMORY_SIZE 32*1024
#define PROF_DATA_NUM 100

lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];
lbm_uint constants_memory[CONSTANT_MEMORY_SIZE];
lbm_prof_t prof_data[100];

char *env_input_file = NULL;
char *env_output_file = NULL;
volatile char *res_output_file = NULL;
bool terminate_after_startup = false;
volatile lbm_cid startup_cid = -1;
volatile lbm_cid store_result_cid = -1;
volatile bool silent_mode = false;

void shutdown_procedure(void);

bool const_heap_write(lbm_uint ix, lbm_uint w) {
  if (ix >= CONSTANT_MEMORY_SIZE) return false;
  if (constants_memory[ix] == 0xffffffff) {
    constants_memory[ix] = w;
    return true;
  } else if (constants_memory[ix] == w) {
    return true;
  }

  char buf[1024];
  lbm_print_value(buf, 1024, constants_memory[ix]);
  printf("prev: %x | %s\n", constants_memory[ix], buf);
  lbm_print_value(buf, 1024, w);
  printf("curr: %x | %s\n", w, buf);
  return false;
}

static volatile bool allow_print = true;

static lbm_char_channel_t string_tok;
static lbm_string_channel_state_t string_tok_state;

void new_prompt() {
  printf("\33[2K\r");
  printf("# ");
  fflush(stdout);
}

void erase() {
  printf("\33[2K\r");
  fflush(stdout);
}

void *eval_thd_wrapper(void *v) {
  if (!silent_mode) {
    printf("Lisp REPL started! (LBM Version: %u.%u.%u)\n", LBM_MAJOR_VERSION, LBM_MINOR_VERSION, LBM_PATCH_VERSION);
    printf("Type :quit to exit.\n");
    printf("     :info for statistics.\n");
    printf("     :load [filename] to load lisp source.\n");
  }
  lbm_run_eval();
  return NULL;
}

void critical(void) {
  printf("CRITICAL ERROR\n");
  exit(0);
}

void done_callback(eval_context_t *ctx) {

  // fails silently if unable to generate result file.
  // TODO: report failure in some way.
  if (res_output_file && store_result_cid == ctx->id) {
    store_result_cid = -1;
    int32_t fv_size = flatten_value_size(ctx->r, 0);
    if (fv_size > 0) {
      lbm_flat_value_t fv;
      fv.buf = malloc((uint32_t)fv_size);
      if (fv.buf) {
        fv.buf_size = (uint32_t)fv_size;
        fv.buf_pos = 0;
        if (flatten_value_c(&fv, ctx->r) == FLATTEN_VALUE_OK) {
          FILE *fp = fopen((const char *)res_output_file, "w");
          if (fp) {
            fwrite(&fv_size, 1, sizeof(int32_t), fp);
            fwrite(fv.buf, 1, (size_t)fv_size, fp);
            fclose(fp);
          } else {
            printf("ALERT: Cannot open result file\n");
          }
        } else {
          printf("ALERT: Unable to flatten result value\n");
        }
      } else {
        printf("ALERT: Out of memory to allocate result buffer\n");
      }
    } else {
      printf("ALERT: Unable to flatten result value\n");
    }
  }
  char output[1024];
  lbm_value t = ctx->r;
  lbm_print_value(output, 1024, t);
  erase();
  if (!silent_mode) {
    printf("> %s\n", output);
    new_prompt();
  } else {
    printf("%s\n", output);
  }

  if (startup_cid != -1) {
    if (ctx->id == startup_cid) {
      startup_cid = -1;
    }
  }
}

int error_print(const char *format, ...) {
  va_list args;
  va_start (args, format);
  erase();
  int n = vprintf(format, args);
  va_end(args);
  new_prompt();
  return n;
}

void sleep_callback(uint32_t us) {
  struct timespec s;
  struct timespec r;
  s.tv_sec = 0;
  s.tv_nsec = (long)us * 1000;
  nanosleep(&s, &r);
}

static bool prof_running = false;

void *prof_thd(void *v) {
  while (prof_running) {
    lbm_prof_sample();
    sleep_callback(200);
  }
  return NULL;
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

/* load a file, caller is responsible for freeing the returned string */
char * load_file(char *filename) {
  char *file_str = NULL;
  int i = 0;
  while (filename[i] == ' ' && filename[i] != 0) {
    i ++;
  }
  FILE *fp;
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
  if (!silent_mode) { 
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
}

void ctx_exists(eval_context_t *ctx, void *arg1, void *arg2) {

  lbm_cid id = *(lbm_cid*)arg1;
  bool *exists = (bool*)arg2;

  if (ctx->id == id) {
    *exists = true;
  }
}

void lookup_local(eval_context_t *ctx, void *arg1, void *arg2) {
  char output[1024];
  lbm_value res;
  if (lbm_env_lookup_b(&res, (lbm_value)arg1, ctx->curr_env)) {

    lbm_print_value(output, 1024, res);
    printf("CTX %d: %s = %s\n", (int32_t)ctx->id, (char *)arg2, output);
  } else {
    printf("not found\n");
  }

}


void sym_it(const char *str) {
  bool sym_name_flash = lbm_symbol_in_flash((char *)str);
  bool sym_entry_flash = lbm_symbol_list_entry_in_flash((char *)str);
  printf("[%s, %s]: %s\n",
         sym_name_flash ? "FLASH" : "LBM_MEM",
         sym_entry_flash ? "FLASH" : "LBM_MEM",
         str);
}

static lbm_uint memory[LBM_MEMORY_SIZE_1M];
static lbm_uint bitmap[LBM_MEMORY_BITMAP_SIZE_1M];

pthread_t lispbm_thd = 0;
unsigned int heap_size = 2048; // default
lbm_cons_t *heap_storage = NULL;
lbm_heap_state_t heap_state;
lbm_const_heap_t const_heap;


// OPTIONS

#define NO_SHORT_OPT         0x0400
#define LOAD_ENVIRONMENT     0x0401
#define STORE_ENVIRONMENT    0x0402
#define STORE_RESULT         0x0403
#define TERMINATE            0x0404
#define SILENT_MODE          0x0405

struct option options[] = {
  {"help", no_argument, NULL, 'h'},
  {"heap_size", required_argument, NULL, 'H'},
  {"src", required_argument, NULL, 's'},
  {"load_env", required_argument, NULL, LOAD_ENVIRONMENT},
  {"store_env", required_argument, NULL, STORE_ENVIRONMENT},
  {"store_res", required_argument, NULL, STORE_RESULT},
  {"terminate", no_argument, NULL, TERMINATE},
  {"silent", no_argument, NULL, SILENT_MODE},
  {0,0,0,0}};

typedef struct src_list_s {
  char *filename;
  struct src_list_s *next;
} src_list_t;

src_list_t *sources = NULL;

bool src_list_add(char *filename) {
  src_list_t *entry=malloc(sizeof(src_list_t));
  if (!entry) return false;

  entry->filename = filename;
  entry->next = NULL;

  if (!sources) {
    sources = entry;
    return true;
  }
  src_list_t *curr = sources;
  while(curr->next) {
    curr = curr->next;
  }
  curr->next = entry;
  return true;
}

int src_list_len(void) {
  int n = 0;
  src_list_t *curr = sources;
  while (curr) {
    curr = curr->next;
    n++;
  }
  return n;
}

void parse_opts(int argc, char **argv) {

  int c;
  opterr = 1;
  int opt_index = 0;
  while ((c = getopt_long(argc, argv, "H:hs:",options, &opt_index)) != -1) {
    switch (c) {
    case 'H':
      heap_size = (unsigned int)atoi((char*)optarg);
      break;
    case 'h':
      printf("Usage: %s [OPTION...]\n\n", argv[0]);
      printf("    -h, --help                    Prints help\n");
      printf("    -H SIZE, --heap_size=SIZE     Set heap_size to be SIZE number of\n"\
             "                                  cells.\n");
      printf("    -s FILEPATH, --src=FILEPATH   Load and evaluate lisp src\n");
      printf("\n");
      printf("    --load_env=FILEPATH           Load the global environment from a file at\n"\
             "                                  startup.\n");
      printf("    --store_env=FILEPATH          Store the global environment to a file upon\n"\
             "                                  exit.\n");
      printf("    --store_res=FILEPATH          Store the result of the last program\n"\
             "                                  specified with the --src/-s options.\n");
      printf("    --terminate                   Terminate the REPL after evaluating the\n"\
             "                                  source files specified with --src/-s\n");
      printf("    --silent                      The REPL will print as little as possible\n");
      printf("\n");
      printf("Multiple sourcefiles can be added with multiple uses of the --src/-s flag.\n" \
             "Multiple sources are evaluated in sequence in the order they are specified\n" \
             "on the command-line. Source file N will not start evaluating until after\n" \
             "source file (N-1) has terminated, for N larger than 1.\n");
      exit(EXIT_SUCCESS);
    case 's':
      if (!src_list_add((char*)optarg)) {
        printf("Error adding source file to source list\n");
        exit(EXIT_FAILURE);
      }
      break;
    case LOAD_ENVIRONMENT:
      env_input_file = (char*)optarg;
      break;
    case STORE_ENVIRONMENT:
      env_output_file = (char*)optarg;
      break;
    case STORE_RESULT:
      res_output_file = (char*)optarg;
      break;
    case TERMINATE:
      terminate_after_startup = true;
    case SILENT_MODE:
      silent_mode = true;
    default:
      break;
    }
  }
}

int init_repl() {

  if (lispbm_thd && lbm_get_eval_state() != EVAL_CPS_STATE_DEAD) {
    int thread_r = 0;
    lbm_kill_eval();
    pthread_join(lispbm_thd, (void*)&thread_r);
    lispbm_thd = 0;
  }

  if (heap_storage) {
    free(heap_storage);
    heap_storage = NULL;
  }

  heap_storage = (lbm_cons_t*)malloc(sizeof(lbm_cons_t) * heap_size);

  if (heap_storage == NULL) {
    return 0;
  }

  if (!lbm_init(heap_storage, heap_size,
                memory, LBM_MEMORY_SIZE_1M,
                bitmap, LBM_MEMORY_BITMAP_SIZE_1M,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    return 0;
  }

  if (!lbm_eval_init_events(20)) {
    return 0;
  }

  memset(constants_memory, 0xFF, CONSTANT_MEMORY_SIZE * sizeof(lbm_uint));
  if (!lbm_const_heap_init(const_heap_write,
                           &const_heap,constants_memory,
                           CONSTANT_MEMORY_SIZE)) {
    return 0;
  }

  lbm_set_critical_error_callback(critical);
  lbm_set_ctx_done_callback(done_callback);
  lbm_set_timestamp_us_callback(timestamp);
  lbm_set_usleep_callback(sleep_callback);
  lbm_set_dynamic_load_callback(dynamic_loader);
  lbm_set_printf_callback(error_print);

  init_exts();

  if (!lbm_add_extension("print", ext_print)) {
    return 0;
  }

  if (pthread_create(&lispbm_thd, NULL, eval_thd_wrapper, NULL)) {
    printf("Error creating evaluation thread\n");
    return 1;
  }
  return 1;
}

bool evaluate_sources(void) {

  src_list_t *curr = sources;
  char *file_str = NULL;
  while (curr) {
    if (file_str) free(file_str);
    file_str = load_file(curr->filename);
    lbm_create_string_char_channel(&string_tok_state,
                                   &string_tok,
                                   file_str);
    lbm_pause_eval_with_gc(50);
    while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
      sleep_callback(10);
    }

    startup_cid = lbm_load_and_eval_program_incremental(&string_tok, NULL);
    if (res_output_file) {
      store_result_cid = startup_cid;
    }
    lbm_continue_eval();

    int counter = 0;
    while (startup_cid != -1) {
      sleep_callback(10);
      counter++;
    }
    curr = curr->next;
  }
  return true;
}

#define NAME_BUF_SIZE 1024
void startup_procedure(void) {
  char name_buf[NAME_BUF_SIZE];

  if (env_input_file) {
    FILE *fp = fopen(env_input_file, "r");
    if (!fp) {
      printf("Error opening environment input file\n");
      goto startup_procedure_1;
    }
    while (true) {
      uint32_t name_len;
      size_t n = fread(&name_len, 1, sizeof(uint32_t), fp);
      if ( n < sizeof(uint32_t) ) {
        // We are successfully done if n == 0;
        if (n > 0) {
          printf("ALERT: Trailing data in env file\n");
        }
        break;
      }
      if (name_len ==  0) {
        printf("ALERT: Zero length name in env file\n");
        break;
      }
      memset(name_buf, 0, NAME_BUF_SIZE);
      n = fread(name_buf, 1, name_len, fp);
      if (n < name_len) {
        printf("ALERT: Malformed name in env file\n");
        break;
      }

      lbm_uint sym_id = 0;
      if (!lbm_get_symbol_by_name(name_buf, &sym_id)) {
        if (!lbm_add_symbol(name_buf, &sym_id)) {
          printf("ALERT: Unable to add symbol\n");
          break;
        }
      }

      char *sym = (char*)lbm_get_name_by_symbol(sym_id);
      if (!sym) {
        printf("ERROR\n");
        exit(EXIT_FAILURE);
      }

      lbm_value key = lbm_enc_sym(sym_id);
      uint32_t val_len;
      n = fread(&val_len, 1, sizeof(uint32_t), fp);
      if (n < sizeof(uint32_t) || val_len == 0) {
        printf("ALERT: Invalid value size in env file\n");
        break;
      }
      int retry = 0;
      uint8_t *data = NULL;
      while (retry < 100) {
        data = lbm_malloc_reserve(val_len);

        if (data == NULL) {
          sleep_callback(100);
          retry ++;
          continue;
        } else {
          break;
        }
      }
      if (retry >= 100) {
        printf("ALERT: Unable to allocate memory for flat value inside LBM\n");
        break;
      }
      n = fread(data, 1, val_len, fp);
      if (n < val_len) {
        lbm_free(data);
        printf("ALERT: Invalid value in env file\n");
        break;
      }

      // Nonintuitive that fv is reused before event is processed.
      // But ok as the fields of fv are copied inside lbm_event_define.
      // No this is not OK! that is not the case!
      // TODO - Better synchronization method needed.
      lbm_flat_value_t fv;
      fv.buf = data;
      fv.buf_size = val_len;
      fv.buf_pos = 0;

      retry = 0;
      while (retry < 100) {
        if (!lbm_event_define(key, &fv)) {
          sleep_callback(100);
          retry ++;
          continue;
        } else {
          break;
        }
      }
      if (retry >= 100) {
        printf("ALERT: Unable to create define-event inside LBM\n");
        break;
      }
      while (!lbm_event_queue_is_empty()) {
        sleep_callback(100);
      }
      sleep_callback(1000);
      // delay a little bit to allow all the events to be handled
    // and the environment be fully populated

    }
  }
 startup_procedure_1:
  if (sources) {
    evaluate_sources();
  }

  if(terminate_after_startup) {
    shutdown_procedure();
    exit(EXIT_SUCCESS);
  }
}

void shutdown_procedure(void) {

  if (env_output_file) {


    FILE *fp = fopen(env_output_file, "w");
    if (!fp) {
      printf("Error opening environment output file\n");
      goto shutdown_procedure_1;
    }
    lbm_value* env = lbm_get_global_env();
    for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
      lbm_value curr = env[i];
      while(lbm_is_cons(curr)) {
        lbm_value name_field = lbm_caar(curr);
        lbm_value val_field  = lbm_cdr(lbm_car(curr));
        char *name = (char*)lbm_get_name_by_symbol(lbm_dec_sym(name_field));
        if (!name) goto shutdown_procedure_1;
        int32_t fv_size = flatten_value_size(val_field, 0);
        if (fv_size > 0) {
          lbm_flat_value_t fv;
          fv.buf = malloc((uint32_t)fv_size);
          if (!fv.buf) {
            printf("Error allocating memory for flat value\n");
            exit(EXIT_FAILURE);
          }
          fv.buf_size = (uint32_t)fv_size;
          fv.buf_pos = 0;
          int r = flatten_value_c(&fv, val_field);
          if (r == FLATTEN_VALUE_OK) {
            uint32_t name_len = strlen(name);
            if (name_len > 0) {
              fwrite(&name_len, 1, sizeof(int32_t),fp);
              fwrite(name, 1, strlen(name), fp);
              fwrite(&fv_size, 1, sizeof(int32_t),fp);
              fwrite(fv.buf,1,(size_t)fv_size,fp);
            } else {
              printf("ALERT: Malformed key in environment\n");
            }
          } else {
            printf("ALERT: A value in the environment was not flattenable\n");
            switch (r) {
            case FLATTEN_VALUE_ERROR_CANNOT_BE_FLATTENED:
              printf("CANNOT_BE_FLATTENED Error!\n");
              break;
            case FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL:
              printf("Flat value buffer too small!\n");
              break;
            case FLATTEN_VALUE_ERROR_FATAL:
              printf("Fatal error during flattening\n");
              break;
            case FLATTEN_VALUE_ERROR_CIRCULAR:
              printf("Potentially circular value\n");
              break;
            case FLATTEN_VALUE_ERROR_MAXIMUM_DEPTH:
              printf("Maximum cons depth reached\n");
              break;
            case FLATTEN_VALUE_ERROR_NOT_ENOUGH_MEMORY:
              printf("Not enough memory while flattening\n");
              break;
            }
            char buf[1024];
            lbm_print_value(buf,1024, val_field);
            printf("Value: %s\n", buf);
          }
          free(fv.buf);
        }
        curr = lbm_cdr(curr);
      }
    }
    fclose(fp);
  }
  shutdown_procedure_1:
  return;
}

int main(int argc, char **argv) {
  parse_opts(argc, argv);

  using_history();

  if (!init_repl()) {
    printf ("Failed to initialize REPL\n");
    return -1;
  }
  startup_procedure();
  char output[1024];

  while (1) {
    erase();
    char *str;
    if (silent_mode) {
      str = readline("");
    } else {
      str = readline("# ");
    }
    if (str == NULL) exit(EXIT_SUCCESS);
    add_history(str);
    unsigned int n = strlen(str);

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
      printf("GC stack size: %"PRI_UINT"\n", lbm_get_gc_stack_size());
      printf("GC SP max: %"PRI_UINT"\n", lbm_get_gc_stack_max());
      printf("--(Symbol and Array memory)---------------------------------\n");
      printf("Memory size: %"PRI_UINT" Words\n", lbm_memory_num_words());
      printf("Memory free: %"PRI_UINT" Words\n", lbm_memory_num_free());
      printf("Allocated arrays: %"PRI_UINT"\n", heap_state.num_alloc_arrays);
      printf("Symbol table size RAM: %"PRI_UINT" Bytes\n", lbm_get_symbol_table_size());
      printf("Symbol names size RAM: %"PRI_UINT" Bytes\n", lbm_get_symbol_table_size_names());
      printf("Symbol table size FLASH: %"PRI_UINT" Bytes\n", lbm_get_symbol_table_size_flash());
      printf("Symbol names size FLASH: %"PRI_UINT" Bytes\n", lbm_get_symbol_table_size_names_flash());
      free(str);
    } else if (strncmp(str, ":prof start", 11) == 0) {
      lbm_prof_init(prof_data,
                    PROF_DATA_NUM);
      pthread_t thd; // just forget this id.
      prof_running = true;
      if (pthread_create(&thd, NULL, prof_thd, NULL)) {
        printf("Error creating profiler thread\n");
        free(str);
        continue;
      }
      printf("Profiler started\n");
      free(str);
    } else if (strncmp(str, ":prof stop", 10) == 0) {
      prof_running = false;
      printf("Profiler stopped. Issue command ':prof report' for statistics\n.");
      free(str);
    } else if (strncmp(str, ":prof report", 12) == 0) {
      lbm_uint num_sleep = lbm_prof_get_num_sleep_samples();
      lbm_uint num_system = lbm_prof_get_num_system_samples();
      lbm_uint tot_samples = lbm_prof_get_num_samples();
      lbm_uint tot_gc = 0;
      printf("CID\tName\tSamples\t%%Load\t%%GC\n");
      for (int i = 0; i < PROF_DATA_NUM; i ++) {
        if (prof_data[i].cid == -1) break;
        tot_gc += prof_data[i].gc_count;
        printf("%d\t%s\t%u\t%f\t%f\n",
               prof_data[i].cid,
               prof_data[i].name,
               prof_data[i].count,
               100.0 * ((float)prof_data[i].count) / (float) tot_samples,
               100.0 * ((float)prof_data[i].gc_count) / (float)prof_data[i].count);
      }
      printf("\n");
      printf("GC:\t%u\t%f%%\n", tot_gc, 100.0 * ((float)tot_gc / (float)tot_samples));
      printf("System:\t%u\t%f%%\n", num_system, 100.0 * ((float)num_system / (float)tot_samples));
      printf("Sleep:\t%u\t%f%%\n", num_sleep, 100.0 * ((float)num_sleep / (float)tot_samples));
      printf("Total:\t%u samples\n", tot_samples);
      free(str);
    } else if (strncmp(str, ":env", 4) == 0) {
      for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
        lbm_value *env = lbm_get_global_env();
        lbm_value curr = env[i];
        printf("Environment [%d]:\r\n", i);
        while (lbm_type_of(curr) == LBM_TYPE_CONS) {
          lbm_print_value(output,1024, lbm_car(curr));
          curr = lbm_cdr(curr);
          printf("  %s\r\n",output);
        }
      }
      free(str);
    } else if (strncmp(str, ":state", 6) == 0) {
      lbm_uint state = lbm_get_eval_state();
      switch (state) {
      case EVAL_CPS_STATE_DEAD:
        printf("DEAD\n");
        break;
      case EVAL_CPS_STATE_PAUSED:
        printf("PAUSED\n");
        break;
      case EVAL_CPS_STATE_NONE:
        printf("NO STATE\n");
        break;
      case EVAL_CPS_STATE_RUNNING:
        printf("RUNNING\n");
        break;
      case EVAL_CPS_STATE_KILL:
        printf("KILLING\n");
        break;
      }
      free(str);
    }
    else if (n >= 5 && strncmp(str, ":load", 5) == 0) {

      char *file_str = load_file(&str[5]);
      if (file_str) {

        lbm_create_string_char_channel(&string_tok_state,
                                       &string_tok,
                                       file_str);

        /* Get exclusive access to the heap */
        lbm_pause_eval_with_gc(50);
        while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
          sleep_callback(10);
        }

        (void)lbm_load_and_eval_program_incremental(&string_tok, NULL);
        lbm_continue_eval();

        //printf("started ctx: %"PRI_UINT"\n", cid);
      }
      free(str);
    } else if (n >= 5 && strncmp(str, ":verb", 5) == 0) {
      lbm_toggle_verbose();
      free(str);
      continue;
    } else if (n >= 4 && strncmp(str, ":pon", 4) == 0) {
      allow_print = true;
      free(str);
      continue;
    } else if (n >= 5 && strncmp(str, ":poff", 5) == 0) {
      allow_print = false;
      free(str);
      continue;
    } else if (strncmp(str, ":ctxs", 5) == 0) {
      printf("****** Running contexts ******\n");
      lbm_running_iterator(print_ctx_info, NULL, NULL);
      printf("****** Blocked contexts ******\n");
      lbm_blocked_iterator(print_ctx_info, NULL, NULL);
      free(str);
    }  else if (n >= 5 && strncmp(str, ":quit", 5) == 0) {
      shutdown_procedure();
      free(str);
      break;
    } else if (strncmp(str, ":symbols", 8) == 0) {
      lbm_symrepr_name_iterator(sym_it);
      free(str);
    } else if (strncmp(str, ":heap", 5) == 0) {
      int size = atoi(str + 5);
      if (size > 0) {
        heap_size = (unsigned int)size;
        if (!init_repl()) {
          printf("Failed to initialize REPL after heap resize\n");
          return -1;
        }
      }
      free(str);
    } else if (strncmp(str, ":reset", 6) == 0) {
      if (!init_repl()) {
        printf ("Failed to initialize REPL\n");
        return -1;
      }
      free(str);
    } else if (strncmp(str, ":send", 5) == 0) {
      int id;
      int i_val;

      if (sscanf(str + 5, "%d%d", &id, &i_val) == 2) {
        lbm_pause_eval_with_gc(50);
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
      free(str);
    } else if (strncmp(str, ":pause", 6) == 0) {
      lbm_pause_eval_with_gc(30);
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }
      printf("Evaluator paused\n");
      free(str);
    } else if (strncmp(str, ":continue", 9) == 0) {
      lbm_continue_eval();
      free(str);
    } else if (strncmp(str, ":inspect", 8) == 0) {

      int i = 8;
      if (strlen(str) >= 8) {
        while (str[i] == ' ') i++;
      }
      char *sym = str + i;
      lbm_uint sym_id = 0;
      if (lbm_get_symbol_by_name(sym, &sym_id)) {
        lbm_running_iterator(lookup_local, (void*)lbm_enc_sym(sym_id), (void*)sym);
        lbm_blocked_iterator(lookup_local, (void*)lbm_enc_sym(sym_id), (void*)sym);
      } else {
        printf("symbol does not exist\n");
      }
    } else if (strncmp(str, ":undef", 6) == 0) {
      lbm_pause_eval_with_gc(50);
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }
      char *sym = str + 7;
      printf("undefining: %s\n", sym);
      printf("%s\n", lbm_undefine(sym) ? "Cleared bindings" : "No definition found");
      lbm_continue_eval();
      free(str);
    } else {
      /* Get exclusive access to the heap */
      lbm_pause_eval_with_gc(50);
      while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }
      lbm_create_string_char_channel(&string_tok_state,
                                     &string_tok,
                                     str);
      (void)lbm_load_and_eval_expression(&string_tok);
      lbm_continue_eval();
    }
  }
  free(heap_storage);
  return 0;
}
