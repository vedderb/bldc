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

#define _POSIX_C_SOURCE 200809L // nanosleep?
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

//network
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <netdb.h>

//lispbm
#include "lispbm.h"
#include "lbm_flat_value.h"
#include "lbm_prof.h"

#include "lbm_custom_type.h"
#include "lbm_channel.h"
#include "lbm_version.h"

#include "repl_exts.h"
#include "repl_defines.h"
#ifdef CLEAN_UP_CLOSURES
#include "clean_cl.h"
#endif

#ifdef WITH_SDL
#include "lbm_sdl.h"
#endif

// things directly copied from VESC_EXPRESS
#include "packet.h"
#include "comm_packet_id.h"
#include "buffer.h"
#include "crc.h"

typedef void (*send_func_t)(unsigned char *, unsigned int);


// ////////////////////////////////////////////////////////////
// VESCTCP
#define DEFAULT_VESCIF_TCP_PORT 65107
#define DEFAULT_VESCIF_TCP_PROGRAM_FLASH_SIZE 1024 * 1024

static bool vesctcp = false;
static uint16_t vesctcp_port = (uint16_t)DEFAULT_VESCIF_TCP_PORT;
static volatile bool vesctcp_server_in_use = false;
static const char *vesctcp_in_use = "Error: Server is in use\n";
static uint8_t *vescif_program_flash = NULL;
static unsigned int vescif_program_flash_size = DEFAULT_VESCIF_TCP_PROGRAM_FLASH_SIZE;
static unsigned int vescif_program_flash_code_len = 0;

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t string_tok;
static lbm_buffered_channel_state_t buffered_tok_state;
static lbm_char_channel_t buffered_string_tok;


// ////////////////////////////////////////////////////////////
// LBM
#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256
#define EXTENSION_STORAGE_SIZE 1024
#define WAIT_TIMEOUT 2500
#define STR_SIZE 1024
#define PROF_DATA_NUM 100

lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];
lbm_prof_t prof_data[100];

static char *env_input_file = NULL;
static char *env_output_file = NULL;
static volatile char *res_output_file = NULL;
static bool terminate_after_startup = false;
static volatile lbm_cid startup_cid = -1;
static volatile lbm_cid store_result_cid = -1;
static volatile bool silent_mode = false;

static size_t lbm_memory_size = LBM_MEMORY_SIZE_10K;
static size_t lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_10K;
static size_t constants_memory_size = 4096;

static lbm_uint *constants_memory = NULL;
static lbm_uint *memory=NULL;
static lbm_uint *bitmap=NULL;

static pthread_t prof_thread;

struct read_state_s {
  char *str;   // String being read.
  lbm_cid cid;    // Reader thread id.
  struct read_state_s *next;
};

typedef struct read_state_s read_state_t;
read_state_t *readers = NULL; // ongoing list of readers.

void add_reader(char *str, lbm_cid cid) {
  read_state_t *new_reader = (read_state_t*)malloc(sizeof(read_state_t));
  new_reader->str = str;
  new_reader->cid = cid;
  new_reader->next = readers;
  readers = new_reader;
}

void clear_readers(void) {
  read_state_t *curr = readers;
  while (curr) {

    read_state_t *next = curr->next;
    if (curr->str) free(curr->str);
    free(curr);
    curr = next;
  }
  readers = NULL;
}

bool drop_reader(lbm_cid cid) {

  bool r = false;
  read_state_t *prev = NULL;
  read_state_t *curr = readers;

  while (curr) {
    if (curr->cid == cid) {
      if (prev) {
        prev->next = curr->next;
      } else {
        readers = curr->next;
      }

      if (curr->str) free(curr->str);
      free(curr);
      r = true;
      break;
    }
    prev = curr;
    curr = curr->next;
  }
  return r;
}


void shutdown_procedure(void);

void terminate_repl(int exit_code) {
  if (!silent_mode) {
    printf("%s\n", repl_exit_message[exit_code]);
  }
  exit(exit_code);
}

bool const_heap_write(lbm_uint ix, lbm_uint w) {
  if (ix >= constants_memory_size) return false;
  if (constants_memory[ix] == 0xffffffff) {
    constants_memory[ix] = w;
    return true;
  } else if (constants_memory[ix] == w) {
    return true;
  }
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
#ifdef WITH_SDL
    printf("With SDL extensions\n");
#endif
    printf("Type :quit to exit.\n");
    printf("     :info for statistics.\n");
    printf("     :load [filename] to load lisp source.\n");
  }
  lbm_run_eval();
  return NULL;
}

void critical(void) {
  printf("CRITICAL ERROR\n");
  terminate_repl(REPL_EXIT_CRITICAL_ERROR);
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

/* load a file, caller is responsible for freeing the returned string */
char * load_file(char *filename) {
  char *file_str = NULL;
  int i = 0;
  size_t len = strlen(filename);
  // leading whitespace
  while (filename[i] == ' ' && filename[i] != 0) {
    i ++;
  }
  //trailing whitespace
  while (len > 0) {
    if (filename[len-1] == ' ') {
      filename[len-1] = 0;
    }
    else break;
    len--;
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
    printf("Stack SP max: %"PRI_UINT"\n", lbm_get_max_stack(&ctx->K));
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
#define VESCTCP              0x0406
#define VESCTCP_PORT         0x0407
#define VESCTCP_PROGRAM_FLASH_SIZE   0x0408

struct option options[] = {
  {"help", no_argument, NULL, 'h'},
  {"heap_size", required_argument, NULL, 'H'},
  {"memory_size", required_argument, NULL, 'M'},
  {"const_memory_size", required_argument, NULL, 'C'},
  {"src", required_argument, NULL, 's'},
  {"eval", required_argument, NULL, 'e'},
  {"load_env", required_argument, NULL, LOAD_ENVIRONMENT},
  {"store_env", required_argument, NULL, STORE_ENVIRONMENT},
  {"store_res", required_argument, NULL, STORE_RESULT},
  {"terminate", no_argument, NULL, TERMINATE},
  {"silent", no_argument, NULL, SILENT_MODE},
  {"vesctcp",no_argument, NULL, VESCTCP},
  {"vesctcp_port",required_argument, NULL, VESCTCP_PORT},
  {"vesctcp_program_flash_size", required_argument, NULL, VESCTCP_PROGRAM_FLASH_SIZE},
  {0,0,0,0}};

typedef struct src_list_s {
  char *filename;
  struct src_list_s *next;
} src_list_t;

src_list_t *sources = NULL;

bool src_list_add(char *filename) {
  if (strlen(filename) == 0) return false;
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

typedef struct expr_list_s {
  char *expr;
  struct expr_list_s *next;
} expr_list_t;

expr_list_t *expressions = NULL;

bool expr_list_add(char *expr) {
  if (strlen(expr) == 0) return false;
  expr_list_t *entry=malloc(sizeof(expr_list_t));
  if (!entry) return false;

  entry->expr = expr;
  entry->next = NULL;

  if (!expressions) {
    expressions = entry;
    return true;
  }
  expr_list_t *curr = expressions;
  while(curr->next) {
    curr = curr->next;
  }
  curr->next = entry;
  return true;
}

void parse_opts(int argc, char **argv) {

  int c;
  opterr = 1;
  int opt_index = 0;
  while ((c = getopt_long(argc, argv, "H:M:C:hse:",options, &opt_index)) != -1) {
    switch (c) {
    case 'H':
      heap_size = (size_t)atoi((char*)optarg);
      break;
    case 'C':
      constants_memory_size = (size_t)atoi((char*)optarg);
      break;
    case 'M': {
      size_t ix = (size_t)atoi((char*)optarg);
      switch(ix) {
      case 1:
        lbm_memory_size = LBM_MEMORY_SIZE_512;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_512;
        break;
      case 2:
        lbm_memory_size = LBM_MEMORY_SIZE_1K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_1K;
        break;
      case 3:
        lbm_memory_size = LBM_MEMORY_SIZE_2K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_2K;
        break;
      case 4:
        lbm_memory_size = LBM_MEMORY_SIZE_4K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_4K;
        break;
      case 5:
        lbm_memory_size = LBM_MEMORY_SIZE_8K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_8K;
        break;
      case 6:
        lbm_memory_size = LBM_MEMORY_SIZE_10K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_10K;
        break;
      case 7:
        lbm_memory_size = LBM_MEMORY_SIZE_12K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_12K;
        break;
      case 8:
        lbm_memory_size = LBM_MEMORY_SIZE_14K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_14K;
        break;
      case 9:
        lbm_memory_size = LBM_MEMORY_SIZE_16K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_16K;
        break;
      case 10:
        lbm_memory_size = LBM_MEMORY_SIZE_32K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_32K;
        break;
      case 11:
        lbm_memory_size = LBM_MEMORY_SIZE_1M;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_1M;
        break;
      default:
        printf("WARNING: Incorrect lbm_memory_size index! Using default\n");
        lbm_memory_size = LBM_MEMORY_SIZE_10K;
        lbm_memory_bitmap_size = LBM_MEMORY_BITMAP_SIZE_10K;
        break;
      }
    } break;
    case 'h':
      printf("Usage: %s [OPTION...]\n\n", argv[0]);
      printf("    -h, --help                        Prints help\n");
      printf("    -H SIZE, --heap_size=SIZE         Set heap_size to be SIZE number of\n"\
             "                                      cells.\n");
      printf("    -M SIZE, --memory_size=SIZE       Set the arrays and symbols memory\n"\
             "                                      size to one memory-size-indices\n"\
             "                                      listed below.\n");
      printf("    -C SIZE, --const_memory_size=SIZE Set the size of the constants memory.\n"\
             "                                      This memory emulates a flash memory\n"\
             "                                      that can be written to once per location.\n");
      printf("    -s FILEPATH, --src=FILEPATH       Load and evaluate lisp src\n");
      printf("    -e EXPRESSION, --eval=EXPRESSION  Load and evaluate lisp src\n");
      printf("\n");
      printf("    --load_env=FILEPATH               Load the global environment from a file at\n"\
             "                                      startup.\n");
      printf("    --silent                          The REPL will print as little as possible\n");
      printf("    --store_env=FILEPATH              Store the global environment to a file\n"\
             "                                      upon exit.\n");
      printf("    --store_res=FILEPATH              Store the result of the last program\n"\
             "                                      specified with the --src/-s options.\n");
      printf("    --vesctcp                         Open a TCP server talking the VESC\n"\
             "                                      protocol on port %d\n", DEFAULT_VESCIF_TCP_PORT);
      printf("    --vesctcp_port=PORT               open the TCP server on this port instead.\n");
      printf("    --vesctcp_program_flash_size=SIZE Size of memory for program storage.\n");
      printf("    --terminate                       Terminate the REPL after evaluating the\n"\
             "                                      source files specified with --src/-s\n");
      printf("Memory-size-indices: \n"          \
             "Index | Words\n"                  \
             "  1   - %d\n"                     \
             "  2   - %d\n"                     \
             "  3   - %d\n"                     \
             "  4   - %d\n"                     \
             "  5   - %d\n"                     \
             "* 6   - %d\n"                     \
             "  7   - %d\n"                     \
             "  8   - %d\n"                     \
             "  9   - %d\n"                     \
             " 10   - %d\n"                     \
             " 11   - %d\n",
             LBM_MEMORY_SIZE_512,
             LBM_MEMORY_SIZE_1K,
             LBM_MEMORY_SIZE_2K,
             LBM_MEMORY_SIZE_4K,
             LBM_MEMORY_SIZE_8K,
             LBM_MEMORY_SIZE_10K,
             LBM_MEMORY_SIZE_12K,
             LBM_MEMORY_SIZE_14K,
             LBM_MEMORY_SIZE_16K,
             LBM_MEMORY_SIZE_32K,
             LBM_MEMORY_SIZE_1M
             );
      printf("Default is marked with a *.\n");
      printf("\n");
      printf("Multiple sourcefiles and expressions can be added with multiple uses\n" \
             "of the --src/-s and --eval/-e flags.\n" \
             "Sources and expressions are evaluated in sequence in the order they are\n" \
             "specified on the command-line, and all source files are evaluated before\n" \
             "expressions. Source file N will not start evaluating until after\n" \
             "source file (N-1) has terminated, for N larger than 1.\n");
      terminate_repl(REPL_EXIT_SUCCESS);
    case 's':
      if (!src_list_add((char*)optarg)) {
        printf("Error adding source file to source list\n");
        terminate_repl(REPL_EXIT_INVALID_SOURCE_FILE);
      }
      break;
    case 'e':
      if (!expr_list_add((char*)optarg)) {
        printf("Error adding expression to eval list\n");
        terminate_repl(REPL_EXIT_INVALID_EXPRESSION);
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
      break;
    case SILENT_MODE:
      silent_mode = true;
      break;
    case VESCTCP:
      vesctcp = true;
      break;
    case VESCTCP_PORT:
      vesctcp_port = (uint16_t)atoi((char *)optarg);
      vesctcp = true;
      break;
    case VESCTCP_PROGRAM_FLASH_SIZE:
      vescif_program_flash_size= (unsigned int)atoi((char *)optarg);
      break;
    default:
      break;
    }
  }
}

uint32_t read_word(unsigned char *data, unsigned int pos) {

  uint32_t res = 0;
  res |= (data[pos]);
  res |= ((uint32_t)(data[pos+1]) << 8);
  res |= ((uint32_t)(data[pos+2]) << 16);
  res |= ((uint32_t)(data[pos+3]) << 24);
  return res;
}

bool load_flat_library(unsigned char *lib, unsigned int size) {

  unsigned int pos = 0;

  while (pos < (size - 1) ) {
    uint32_t name_size = read_word(lib,pos); pos += 4;
    char *name = malloc(name_size+1);
    if (name == NULL) return false;
    memset(name, 0, name_size + 1);
    memcpy(name, lib + pos, name_size);
    pos += name_size;

    lbm_uint sym_id = 0;
    if (!lbm_get_symbol_by_name(name, &sym_id)) {
      if (!lbm_add_symbol(name, &sym_id)) {
        printf("unable to add symbol\n");
        return false;
      }
    }
    free(name);
    lbm_value sym = lbm_enc_sym(sym_id);

    uint32_t val_size = read_word(lib, pos); pos += 4;

    lbm_flat_value_t fv;
    fv.buf = &lib[pos];
    fv.buf_size = val_size;
    fv.buf_pos  = 0;

    lbm_value val = ENC_SYM_NIL;
    if (!lbm_unflatten_value(&fv, &val)) {
      printf("Unable to unflatten value\n");
      return false;
    }

    pos += val_size;

    lbm_uint ix_key  = sym_id & GLOBAL_ENV_MASK;
    lbm_value *global_env = lbm_get_global_env();
    lbm_uint orig_env = global_env[ix_key];
    lbm_value new_env;

    // All of this should just succeed with no GC needed.
    new_env = lbm_env_set(orig_env,sym,val);

    global_env[ix_key] = new_env;
  }
  return true;
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

  memory = (lbm_uint*)malloc(lbm_memory_size * sizeof(lbm_uint));
  bitmap = (lbm_uint*)malloc(lbm_memory_bitmap_size * sizeof(lbm_uint));

  if (memory == NULL || bitmap == NULL) return 0;

  if (!lbm_init(heap_storage, heap_size,
                memory, lbm_memory_size,
                bitmap, lbm_memory_bitmap_size,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    return 0;
  }

  if (!lbm_eval_init_events(20)) {
    return 0;
  }

  constants_memory = (lbm_uint*)malloc(constants_memory_size * sizeof(lbm_uint));
  memset(constants_memory, 0xFF, constants_memory_size * sizeof(lbm_uint));
  if (!lbm_const_heap_init(const_heap_write,
                           &const_heap,constants_memory,
                           constants_memory_size)) {
    return 0;
  }

  lbm_set_critical_error_callback(critical);
  lbm_set_ctx_done_callback(done_callback);
  lbm_set_timestamp_us_callback(timestamp);
  lbm_set_usleep_callback(sleep_callback);
  lbm_set_dynamic_load_callback(dynamic_loader);
  lbm_set_printf_callback(error_print);

  init_exts();

#ifdef WITH_SDL
  if (!lbm_sdl_init()) {
    return 0;
  }
#endif

  /* Load clean_cl library into heap */
#ifdef CLEAN_UP_CLOSURES
  if (!load_flat_library(clean_cl_env, clean_cl_env_len)) {
    printf("Error loading a flat library\n");
    return 1;
  }
#endif

  if (pthread_create(&lispbm_thd, NULL, eval_thd_wrapper, NULL)) {
    printf("Error creating evaluation thread\n");
    return 0;
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

bool evaluate_expressions(void) {
  expr_list_t *curr = expressions;
  char *expr = NULL;

  while (curr) {
    // if (expr) free(expr);
    expr = curr->expr;
    lbm_create_string_char_channel(&string_tok_state,
                                   &string_tok,
                                   expr);
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
      terminate_repl(REPL_EXIT_UNABLE_TO_OPEN_ENV_FILE);
    }
    uint32_t num_symbols = 0;
    while (true) {
      uint32_t name_len;
      size_t n = fread(&name_len, 1, sizeof(uint32_t), fp);
      if ( n < sizeof(uint32_t) ) {
        // We are successfully done if n == 0;
        if (n > 0) {
          terminate_repl(REPL_EXIT_INVALID_ENV_FILE);
        }
        break;
      }
      if (name_len ==  0) {
        terminate_repl(REPL_EXIT_ZERO_LENGTH_KEY_IN_ENV_FILE);
      }
      memset(name_buf, 0, NAME_BUF_SIZE);
      n = fread(name_buf, 1, name_len, fp);
      if (n < name_len) {
        terminate_repl(REPL_EXIT_INVALID_KEY_IN_ENV_FILE);
      }

      if (lbm_get_symbol_table_size() > (100 * 1024)) {
        terminate_repl(REPL_EXIT_SYMBOL_TABLE_TOO_BIG);
      }

      lbm_uint sym_id = 0;
      if (!lbm_get_symbol_by_name(name_buf, &sym_id)) {
        if (!lbm_add_symbol(name_buf, &sym_id)) {
          terminate_repl(REPL_EXIT_UNABLE_TO_CREATE_SYMBOL);
          //printf("sym: %s\n", name_buf);
          //printf("pos1: %u symbols added\n", num_symbols);
        }
      }

      char *sym = (char*)lbm_get_name_by_symbol(sym_id);
      if (!sym) {
        terminate_repl(REPL_EXIT_UNABLE_TO_CREATE_SYMBOL);
        //printf("sym: %s\n", name_buf);
        //printf("pos2: %u symbols added\n", num_symbols);
      }

      num_symbols ++;
      lbm_value key = lbm_enc_sym(sym_id);
      uint32_t val_len;
      n = fread(&val_len, 1, sizeof(uint32_t), fp);
      if (n < sizeof(uint32_t) || val_len == 0) {
        terminate_repl(REPL_EXIT_INVALID_VALUE_SIZE_IN_ENV_FILE);
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
        terminate_repl(REPL_EXIT_OUT_OF_MEMORY_WHILE_PROCESSING_ENV_FILE);
      }
      n = fread(data, 1, val_len, fp);
      if (n < val_len) {
        terminate_repl(REPL_EXIT_INVALID_VALUE_IN_ENV_FILE);
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
        terminate_repl(REPL_EXIT_LBM_EVENT_QUEUE_FULL);
      }
      while (!lbm_event_queue_is_empty()) {
        sleep_callback(100);
      }

      lbm_value binding;
      int count = 0;
      while (!lbm_global_env_lookup(&binding, key)) {
        // Wait up to one second for the binding to appear in the env.
        if (count > 10000) terminate_repl(REPL_EXIT_ENV_POPULATION_TIMEOUT);
        sleep_callback(100);
        count++;
      }
      // delay a little bit to allow all the events to be handled
      // and the environment be fully populated

    }
  }
  if (sources) {
    evaluate_sources();
  }
  if (expressions) {
    evaluate_expressions();
  }

  if(terminate_after_startup) {
    shutdown_procedure();
    terminate_repl(REPL_EXIT_SUCCESS);
  }
}


int store_env(char *filename) {
    FILE *fp = fopen(env_output_file, "w");
    if (!fp) {
      terminate_repl(REPL_EXIT_UNABLE_TO_OPEN_ENV_FILE);
    }
    lbm_value* env = lbm_get_global_env();
    for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
      lbm_value curr = env[i];
      while(lbm_is_cons(curr)) {
        lbm_value name_field = lbm_caar(curr);
        lbm_value val_field  = lbm_cdr(lbm_car(curr));
        char *name = (char*)lbm_get_name_by_symbol(lbm_dec_sym(name_field));
        if (!name) return REPL_EXIT_UNABLE_TO_ACCESS_SYMBOL_STRING;
        int32_t fv_size = flatten_value_size(val_field, 0);
        if (fv_size > 0) {
          lbm_flat_value_t fv;
          fv.buf = malloc((uint32_t)fv_size);
          if (!fv.buf) {
            return REPL_EXIT_ERROR_FLATTEN_NO_MEM;
          }
          fv.buf_size = (uint32_t)fv_size;
          fv.buf_pos = 0;
          int r = flatten_value_c(&fv, val_field);
          if (r == FLATTEN_VALUE_OK) {
            size_t name_len = strlen(name);
            if (name_len > 0) {
              fwrite(&name_len, 1, sizeof(int32_t),fp);
              fwrite(name, 1, strlen(name), fp);
              fwrite(&fv_size, 1, sizeof(int32_t),fp);
              fwrite(fv.buf,1,(size_t)fv_size,fp);
            } else {
              return REPL_EXIT_INVALID_KEY_IN_ENVIRONMENT;
            }
          } else {
            switch (r) {
            case FLATTEN_VALUE_ERROR_CANNOT_BE_FLATTENED:
              return REPL_EXIT_VALUE_CANNOT_BE_FLATTENED;
              break;
            case FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL:
              return REPL_EXIT_FLAT_VALUE_BUFFER_TOO_SMALL;
              break;
            case FLATTEN_VALUE_ERROR_FATAL:
              return REPL_EXIT_FATAL_ERROR_WHILE_FLATTENING;
              break;
            case FLATTEN_VALUE_ERROR_CIRCULAR:
              return REPL_EXIT_CIRCULAR_VALUE;
              break;
            case FLATTEN_VALUE_ERROR_MAXIMUM_DEPTH:
              return REPL_EXIT_FLATTENING_MAXIMUM_DEPTH;
              break;
            case FLATTEN_VALUE_ERROR_NOT_ENOUGH_MEMORY:
              return REPL_EXIT_OUT_OF_MEMORY_WHILE_FLATTENING;
              break;
            }
          }
          free(fv.buf);
        }
        curr = lbm_cdr(curr);
      }
    }
    fclose(fp);
    return REPL_EXIT_SUCCESS;
}

void shutdown_procedure(void) {

  if (env_output_file) {
    int r = store_env(env_output_file);
    if (r != REPL_EXIT_SUCCESS) terminate_repl(r);
  }
  return;
}

// ////////////////////////////////////////////////////////////
// Packet handling

#define PRINT_BUFFER_SIZE 1024
#define HW_NAME "lispbm"
#define FW_NAME "lispbm"
#define HW_TYPE_CUSTOM_MODULE 2
#define FW_TEST_VERSION_NUMBER 0
#define SEND_MAX_RETRY 10

static send_func_t send_func = 0;
static int connected_socket = 0;


char *lispif_print_prefix(void) {
  return "";
}

void commands_send_packet(unsigned char *data, unsigned int len) {
  if (send_func) {
    send_func(data, len);
  }
}


int commands_printf_lisp(const char* format, ...) {
  int len;

  char *print_buffer = malloc(PRINT_BUFFER_SIZE);
  if (!print_buffer) return 0;

  print_buffer[0] = (char)COMM_LISP_PRINT;
  int offset = 1;
  int prefix_len = sprintf(print_buffer + offset, lispif_print_prefix(), "%s");
  if (prefix_len < 0) {
    free(print_buffer);
    return prefix_len; // error.
  }
  offset += prefix_len;

  va_list arg;
  va_start(arg, format);
  
  len = vsnprintf(
                  print_buffer + offset, (size_t)(PRINT_BUFFER_SIZE - offset), format, arg
                  );
  va_end(arg);

  int len_to_print = (len < (PRINT_BUFFER_SIZE - offset)) ? len + offset : PRINT_BUFFER_SIZE;

  for (int i = 2; i < len_to_print; i++) {
    // TODO: Handle newline character in prefix?
    char chr = print_buffer[i - 1];
    if (chr == '\0') {
      break;
    }
    if (chr == '\n') {
      int remaining_len = len_to_print - i;
      if (remaining_len > (PRINT_BUFFER_SIZE - i - prefix_len)) {
        remaining_len = PRINT_BUFFER_SIZE - i - prefix_len;
      }
      if (remaining_len <= 0) {
        break;
      }
      memmove(print_buffer + i + prefix_len, print_buffer + i, (size_t)remaining_len);
      memmove(print_buffer + i, lispif_print_prefix(), (size_t)prefix_len);
      i += prefix_len;
      len_to_print += prefix_len;
    }

    if (len_to_print > PRINT_BUFFER_SIZE) {
      len_to_print = PRINT_BUFFER_SIZE;
    }
  }

  if (len > 0) {
    if (print_buffer[len_to_print - 1] == '\n') {
      len_to_print--;
    }

    commands_send_packet((unsigned char*)print_buffer, (unsigned int)len_to_print);
  }

  free(print_buffer);

  return len_to_print - 1;
}

#define UTILS_AGE_S(x)		((float)(timestamp() - x) / 1000.0f)
//static uint32_t repl_time = 0;

static void vesc_lbm_done_callback(eval_context_t *ctx) {
  lbm_cid cid = ctx->id;
  lbm_value t = ctx->r;

  if (drop_reader(cid)) {
    char output[128];
    lbm_print_value(output, sizeof(output), t);
    commands_printf_lisp("> %s", output);
  }
}

static lbm_value ext_vescif_print(lbm_value *args, lbm_uint argn) {
  const int str_len = 256;
  char *print_val_buffer = malloc(str_len);
  if (!print_val_buffer) {
    return ENC_SYM_MERROR;
  }

  for (lbm_uint i = 0; i < argn; i ++) {
    lbm_print_value(print_val_buffer, str_len, args[i]);
    commands_printf_lisp("%s", print_val_buffer);
  }

  lbm_free(print_val_buffer);

  return ENC_SYM_TRUE;
}

// a dummy to make imports do noting upon eval
static lbm_value ext_vescif_import(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  return ENC_SYM_NIL;
}

static void vescif_print_ctx_info(eval_context_t *ctx, void *arg1, void *arg2) {
  (void) arg1;
  (void) arg2;

  const char *state_string = "UNKNOWN (CONTACT JOEL)";

  switch (ctx->state & 0xFFFF) {
  case LBM_THREAD_STATE_READY: state_string = "READY"; break;
  case LBM_THREAD_STATE_BLOCKED: state_string = "BLOCKED"; break;
  case LBM_THREAD_STATE_TIMEOUT: state_string = "TIMEOUT"; break;
  case LBM_THREAD_STATE_SLEEPING: state_string = "SLEEPING"; break;
  case LBM_THREAD_STATE_RECV_BL: state_string = "RECV BLOCKING"; break;
  case LBM_THREAD_STATE_RECV_TO: state_string = "RECV TIMEOUT"; break;
  }

  char output[1024];
  int print_ret = lbm_print_value(output, 1024, ctx->r);
  commands_printf_lisp("--------------------------------");
  commands_printf_lisp("ContextID: %"PRI_UINT, ctx->id);
  commands_printf_lisp("State: %s\n", state_string);
  commands_printf_lisp("Stack SP: %"PRI_UINT,  ctx->K.sp);
  commands_printf_lisp("Stack SP max: %"PRI_UINT, lbm_get_max_stack(&ctx->K));
  if (print_ret) {
    commands_printf_lisp("Value: %s\n", output);
  } else {
    commands_printf_lisp("Error: %s\n", output);
  }
}


static void vescif_sym_it(const char *str) {
  bool sym_name_flash = lbm_symbol_in_flash((char *)str);
  bool sym_entry_flash = lbm_symbol_list_entry_in_flash((char *)str);
  commands_printf_lisp("[%s, %s]: %s\n",
         sym_name_flash ? "FLASH" : "LBM_MEM",
         sym_entry_flash ? "FLASH" : "LBM_MEM",
         str);
}


bool vescif_restart(bool print, bool load_code, bool load_imports) {
  bool res = false;
  if (prof_running) {
    prof_running = false;
    void *a;
    pthread_join(prof_thread, &a);
  }

  if (lispbm_thd) {
    int thread_r = 0;
    lbm_kill_eval();
    pthread_join(lispbm_thd, (void *)&thread_r);
    lispbm_thd = 0;
  }

  if (heap_storage) {
    free(heap_storage);
    heap_storage = NULL;
  }

  heap_storage = (lbm_cons_t*)malloc(sizeof(lbm_cons_t) * heap_size);
  if (heap_storage == NULL) return 0;

  if (!lbm_init(heap_storage, heap_size,
                memory, lbm_memory_size,
                bitmap, lbm_memory_bitmap_size,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    return 0;
  }


  if (!lbm_eval_init_events(20)) {
    return 0;
  }

  constants_memory = (lbm_uint*)malloc(constants_memory_size * sizeof(lbm_uint));
  memset(constants_memory, 0xFF, constants_memory_size * sizeof(lbm_uint));
  if (!lbm_const_heap_init(const_heap_write,
                           &const_heap,constants_memory,
                           constants_memory_size)) {
    return 0;
  }

  lbm_set_critical_error_callback(critical);
  lbm_set_ctx_done_callback(vesc_lbm_done_callback);
  lbm_set_timestamp_us_callback(timestamp);
  lbm_set_usleep_callback(sleep_callback);
  lbm_set_dynamic_load_callback(dynamic_loader);
  lbm_set_printf_callback(commands_printf_lisp);

  init_exts();
  lbm_add_extension("print", ext_vescif_print); // replace print
  lbm_add_extension("import", ext_vescif_import); // dummy import

#ifdef WITH_SDL
  if (!lbm_sdl_init()) {
    return 0;
  }
#endif

  /* Load clean_cl library into heap */
#ifdef CLEAN_UP_CLOSURES
  if (!load_flat_library(clean_cl_env, clean_cl_env_len)) {
    printf("Error loading a flat library\n");
    return 1;
  }
#endif

  if (pthread_create(&lispbm_thd, NULL, eval_thd_wrapper, NULL)) {
    printf("Error creating evaluation thread\n");
    return 0;
  }

  lbm_pause_eval();
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
    lbm_pause_eval();
    sleep_callback(1000);
  }

  /* lispif_load_vesc_extensions(); */
  /* for (int i = 0;i < EXT_LOAD_CALLBACK_LEN;i++) { */
  /*   if (ext_load_callbacks[i] == 0) { */
  /*     break; */
  /*   } */

  /*   ext_load_callbacks[i](); */
  /* } */

  /* lbm_set_dynamic_load_callback(lispif_vesc_dynamic_loader); */

  char *code_data = (char*)vescif_program_flash+8;
  size_t code_len = vescif_program_flash_code_len;

  size_t code_chars = 0;
  if (code_data) {
    code_chars = strnlen(code_data, code_len);
  }

  /* for (size_t i = 0; i < code_len; i ++)  { */
  /*   printf("%i %c\n",i, code_data[i]); */

  /* } */
  /* printf("\n"); */

  // Load imports
  if (load_imports) {
    if (code_len > code_chars + 3) {
      int32_t ind = (int32_t)code_chars + 1;
      uint16_t num_imports = buffer_get_uint16((uint8_t*)code_data, &ind);
      if (num_imports > 0 && num_imports < 500) {
        for (int i = 0;i < num_imports;i++) {
          char *name = code_data + ind;
          ind += (int32_t)(strlen(name) + 1);
          int32_t offset = buffer_get_int32((uint8_t*)code_data, &ind);
          int32_t len = buffer_get_int32((uint8_t*)code_data, &ind);

          lbm_value val;
          if (lbm_share_array(&val, code_data + offset, (lbm_uint)len)) {
            lbm_define(name, val);
          }
        }
      }
    }
  }

  /* if (code_data == 0) { */
  /*   code_data = (char*)flash_helper_code_data_raw(CODE_IND_LISP); */
  /* } */

  /* const_heap_max_ind = 0; */
  /* const_heap_ptr = (lbm_uint*)(code_data + code_len + 16); */
  /* const_heap_ptr = (lbm_uint*)((uint32_t)const_heap_ptr & 0xFFFFFFF4); */
  /* uint32_t const_heap_len = ((uint32_t)code_data + flash_helper_code_size_raw(CODE_IND_LISP)) - (uint32_t)const_heap_ptr; */
  /* lbm_const_heap_init(const_heap_write, &const_heap, (lbm_uint*)const_heap_ptr, const_heap_len); */

  code_data = (char*)vescif_program_flash+8;

  if (load_code) {
    if (print) {
      commands_printf_lisp("Parsing %d characters", code_chars);
    }

    lbm_create_string_char_channel(&string_tok_state, &string_tok, (char*)code_data);
    lbm_load_and_eval_program_incremental(&string_tok, "main-u");
  }

  lbm_continue_eval();

  res = true;

  return res;
}

unsigned int get_cpu_last_time = 1;
long unsigned int get_cpu_last_ticks = 1;

float get_cpu_usage(void) {

  int ticks_per_s = (int)sysconf(_SC_CLK_TCK);
  float cpu_usage = -1;

  char fname[200] ;
  snprintf(fname, sizeof(fname), "/proc/self/stat") ;
  FILE *fp = fopen(fname, "r") ;
  if ( fp ) {
    long unsigned int ucpu = 0, scpu=0, tot_cpu = 0 ;
    if ( fscanf(fp, "%*s %*s %*s %*s %*s %*s %*s %*s %*s %*s %*s %*s %*s  %lu %lu",
                &ucpu, &scpu) == 2 ) {
      tot_cpu = ucpu + scpu ;

      long unsigned int ticks = tot_cpu - get_cpu_last_ticks;
      unsigned int t_now = timestamp();
      unsigned int t_diff = t_now - get_cpu_last_time;

      // Not sure about this :)
      cpu_usage = 100.0f * (float)(((float)ticks / ((float)t_diff / 1000000.0f))  / (float)ticks_per_s);
      if (cpu_usage > 100) cpu_usage = 100;
      if (cpu_usage < 0) cpu_usage = 0;

      get_cpu_last_time = t_now;
      get_cpu_last_ticks = tot_cpu;
    }
    fclose(fp);
  }
  return cpu_usage ;
}



void repl_process_cmd(unsigned char *data, unsigned int len,
                      void(*reply_func)(unsigned char *data, unsigned int len)) {
  COMM_PACKET_ID packet_id;

  //send_func = reply_func;

  packet_id = data[0];
  data++;
  len--;

  switch (packet_id) {
  case COMM_FW_VERSION: {
    int32_t ind = 0;
    uint8_t send_buffer[65];
    send_buffer[ind++] = COMM_FW_VERSION;
    send_buffer[ind++] = 1;
    send_buffer[ind++] = 1;

    strcpy((char*)(send_buffer + ind), HW_NAME);
    ind += (int32_t)strlen(HW_NAME) + 1;

    //size_t size_bits = esp_efuse_get_field_size(ESP_EFUSE_MAC_FACTORY);
    //esp_efuse_read_field_blob(ESP_EFUSE_MAC_FACTORY, send_buffer + ind, size_bits);
    ind += 6;
    memset(send_buffer + ind, 0, 6);
    ind += 6;

    send_buffer[ind++] = 0;
    send_buffer[ind++] = FW_TEST_VERSION_NUMBER;

    send_buffer[ind++] = HW_TYPE_CUSTOM_MODULE;
    send_buffer[ind++] = 1; // One custom config

    send_buffer[ind++] = 0; // No phase filters
    send_buffer[ind++] = 0; // No HW QML

    //if (flash_helper_code_size(CODE_IND_QML) > 0) {
    //  send_buffer[ind++] = flash_helper_code_flags(CODE_IND_QML);
    //} else {
    send_buffer[ind++] = 0;
    //}

    send_buffer[ind++] = 0; // No NRF flags

    strcpy((char*)(send_buffer + ind), FW_NAME);
    ind += (int32_t)strlen(FW_NAME) + 1;

    buffer_append_uint32(send_buffer, 967, &ind); // main_calc_hw_crc()
    reply_func(send_buffer, (unsigned int)ind);
  } break;

  case COMM_LISP_SET_RUNNING: {
    // request to change the "running" state of the LBM evaluator.
    bool ok = false;
    bool running = data[0];
    if (!running) {
      if (lispbm_thd) {
        int timeout_cnt = 2000;
        lbm_pause_eval();
        while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
          sleep_callback(1000);
          timeout_cnt--;
        }
        ok = timeout_cnt > 0;
      }
    } else {
      // Vesc express does a restart here..
      ok = vescif_restart(true, true, true);
    }
    int32_t ind = 0;
    uint8_t send_buffer[50];
    send_buffer[ind++] = packet_id;
    send_buffer[ind++] = ok;
    reply_func(send_buffer, (unsigned int)ind);
  } break;

  case COMM_LISP_GET_STATS: {
    float cpu_use = get_cpu_usage();
    float heap_use = 0.0f;
    float mem_use = 0.0f;

    bool print_all = true;
    if (len > 0) {
      print_all = data[0];
    }

    lbm_gc_lock();
    if (lbm_heap_state.gc_num > 0) {
      heap_use = 100.0f * (float)(heap_size - lbm_heap_state.gc_last_free) / (float)heap_size;
    }

    mem_use = 100.0f * (float)(lbm_memory_num_words() - lbm_memory_num_free()) / (float)lbm_memory_num_words();

    uint8_t send_buffer_global[4096];
    int32_t ind = 0;

    send_buffer_global[ind++] = packet_id;
    buffer_append_float16(send_buffer_global, cpu_use, 1e2, &ind);
    buffer_append_float16(send_buffer_global, heap_use, 1e2, &ind);
    buffer_append_float16(send_buffer_global, mem_use, 1e2, &ind);

    // Stack. Currently unused
    buffer_append_float16(send_buffer_global, 0, 1e2, &ind);

    // Result. Currently unused.
    send_buffer_global[ind++] = '\0';

    lbm_value *glob_env = lbm_get_global_env();
    for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
      if (ind > 300) {
        break;
      }

      lbm_value curr = glob_env[i];
      while (lbm_type_of(curr) == LBM_TYPE_CONS) {
        lbm_value key_val = lbm_car(curr);
        if (lbm_type_of(lbm_car(key_val)) == LBM_TYPE_SYMBOL && lbm_is_number(lbm_cdr(key_val))) {
          const char *name = lbm_get_name_by_symbol(lbm_dec_sym(lbm_car(key_val)));

          if (print_all ||
              ((name[0] == 'v' || name[0] == 'V') &&
               (name[1] == 't' || name[1] == 'T'))) {
            strcpy((char*)(send_buffer_global + ind), name);
            ind += (int32_t)strlen(name) + 1;
            buffer_append_float32_auto(send_buffer_global, lbm_dec_as_float(lbm_cdr(key_val)), &ind);
          }
        }

        if (ind > 300) {
          break;
        }

        curr = lbm_cdr(curr);
      }
    }

    lbm_gc_unlock();

    reply_func(send_buffer_global, (unsigned int)ind);
  } break;

  case COMM_LISP_REPL_CMD: {
    if (!lispbm_thd) {
      vescif_restart(true, false, true);
    }

    if (lispbm_thd) {
      //lispif_lock_lbm();
      char *str = (char*)data;

      if (len <= 1) {
        commands_printf_lisp(">");
      } else if (len >= 5 && strncmp(str, ":help", 5) == 0) {
        commands_printf_lisp("== Special Commands ==");
        commands_printf_lisp(
                             ":help\n"
                             "  Print this help text");
        commands_printf_lisp(
                             ":info\n"
                             "  Print info about memory usage, allocated arrays and garbage collection");
        commands_printf_lisp(
                             ":prof start\n"
                             "  Start profiler");
        commands_printf_lisp(
                             ":prof stop\n"
                             "  Stop profiler");
        commands_printf_lisp(
                             ":prof report\n"
                             "  Print profiler report");
        commands_printf_lisp(
                             ":env\n"
                             "  Print current environment and variables");
        commands_printf_lisp(
                             ":ctxs\n"
                             "  Print context (threads) info");
        commands_printf_lisp(
                             ":symbols\n"
                             "  Print symbol names");
        commands_printf_lisp(
                             ":reset\n"
                             "  Reset LBM");
        commands_printf_lisp(
                             ":pause\n"
                             "  Pause LBM");
        commands_printf_lisp(
                             ":continue\n"
                             "  Continue running LBM");
        commands_printf_lisp(
                             ":undef <symbol_name>\n"
                             "  Undefine symbol");
        commands_printf_lisp(
                             ":verb\n"
                             "  Toggle verbose error messages");
        commands_printf_lisp(" ");
        commands_printf_lisp("Anything else will be evaluated as an expression in LBM.");
        commands_printf_lisp(" ");
      } else if (len >= 5 && strncmp(str, ":info", 5) == 0) {
        commands_printf_lisp("--(LISP HEAP)--\n");
        commands_printf_lisp("Heap size: %u Bytes\n", heap_size * 8);
        commands_printf_lisp("Used cons cells: %d\n", heap_size - lbm_heap_num_free());
        commands_printf_lisp("Free cons cells: %d\n", lbm_heap_num_free());
        commands_printf_lisp("GC counter: %d\n", lbm_heap_state.gc_num);
        commands_printf_lisp("Recovered: %d\n", lbm_heap_state.gc_recovered);
        commands_printf_lisp("Recovered arrays: %u\n", lbm_heap_state.gc_recovered_arrays);
        commands_printf_lisp("Marked: %d\n", lbm_heap_state.gc_marked);
        commands_printf_lisp("GC SP max: %u (size %u)\n", lbm_get_max_stack(&lbm_heap_state.gc_stack), lbm_heap_state.gc_stack.size);
        commands_printf_lisp("--(Symbol and Array memory)--\n");
        commands_printf_lisp("Memory size: %u bytes\n", lbm_memory_num_words() * 4);
        commands_printf_lisp("Memory free: %u bytes\n", lbm_memory_num_free() * 4);
        commands_printf_lisp("Longest block free: %u bytes\n", lbm_memory_longest_free() * 4);
	commands_printf_lisp("Maximum usage %f%%\n", 100.0  * ((float)lbm_memory_maximum_used() / (float)lbm_memory_num_words()));
        commands_printf_lisp("Allocated arrays: %u\n", lbm_heap_state.num_alloc_arrays);
        commands_printf_lisp("Symbol table size: %u Bytes\n", lbm_get_symbol_table_size());
        commands_printf_lisp("Symbol table size flash: %u Bytes\n", lbm_get_symbol_table_size_flash());
        commands_printf_lisp("Symbol name size: %u Bytes\n", lbm_get_symbol_table_size_names());
        commands_printf_lisp("Symbol name size flash: %u Bytes\n", lbm_get_symbol_table_size_names_flash());
        commands_printf_lisp("Extensions: %u, max %u\n", lbm_get_num_extensions(), lbm_get_max_extensions());
        commands_printf_lisp("--(Flash)--\n");
        commands_printf_lisp("Size: %u words", const_heap.size);
        commands_printf_lisp("Used words: %d\n", const_heap.next);
        commands_printf_lisp("Free words: %d\n", const_heap.size - const_heap.next);
        //flast_stats stats = flash_helper_stats();
        //commands_printf_lisp("Erase Cnt Tot: %d\n", stats.erase_cnt_tot);
        //commands_printf_lisp("Erase Cnt Max Sector: %d\n", stats.erase_cnt_max);
        //commands_printf_lisp("Num sectors erased: %d\n", stats.erased_sector_num);
      } else if (strncmp(str, ":prof start", 11) == 0) {
        if (prof_running) {
          prof_running = false;
          void *a;
          pthread_join(prof_thread,&a);
        }
        lbm_prof_init(prof_data, PROF_DATA_NUM);
        prof_running = true;
        if (pthread_create(&prof_thread, NULL, prof_thd, NULL)) {
          commands_printf_lisp("Error creating profiler thread\n");
        } else {
          commands_printf_lisp("Profiler started\n");
        }
      } else if (strncmp(str, ":prof stop", 10) == 0) {
        void *a;
        commands_printf_lisp("TODO :prof stop\n");
        if (prof_running) {
          prof_running = false;
          pthread_join(prof_thread,&a);
        }
        commands_printf_lisp("Profiler stopped. Issue command ':prof report' for statistics\n");
      } else if (strncmp(str, ":prof report", 12) == 0) {
        lbm_uint num_sleep = lbm_prof_get_num_sleep_samples();
        lbm_uint num_system = lbm_prof_get_num_system_samples();
        lbm_uint tot_samples = lbm_prof_get_num_samples();
        lbm_uint tot_gc = 0;
        commands_printf_lisp("CID\tName\tSamples\t%%Load\t%%GC");
        for (int i = 0; i < PROF_DATA_NUM; i ++) {
         if (prof_data[i].cid == -1) break;
         tot_gc += prof_data[i].gc_count;
         commands_printf_lisp("%d\t%s\t%u\t%.3f\t%.3f",
                              prof_data[i].cid,
                              prof_data[i].name,
                              prof_data[i].count,
                              (double)(100.0 * ((float)prof_data[i].count) / (float) tot_samples),
                              (double)(100.0 * ((float)prof_data[i].gc_count) / (float)prof_data[i].count));
        }
        commands_printf_lisp(" ");
        commands_printf_lisp("GC:\t%u\t%f%%\n", tot_gc, (double)(100.0 * ((float)tot_gc / (float)tot_samples)));
        commands_printf_lisp("System:\t%u\t%f%%\n", num_system, (double)(100.0 * ((float)num_system / (float)tot_samples)));
        commands_printf_lisp("Sleep:\t%u\t%f%%\n", num_sleep, (double)(100.0 * ((float)num_sleep / (float)tot_samples)));
        commands_printf_lisp("Total:\t%u samples\n", tot_samples);
      } else if (strncmp(str, ":env", 4) == 0) {
        lbm_value *glob_env = lbm_get_global_env();
        char output[128];
        for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
          lbm_value curr = glob_env[i];
          while (lbm_type_of(curr) == LBM_TYPE_CONS) {
            lbm_print_value(output, sizeof(output), lbm_car(curr));
            curr = lbm_cdr(curr);

            commands_printf_lisp("  %s", output);
          }
        }
      } else if (strncmp(str, ":ctxs", 5) == 0) {
        commands_printf_lisp("****** Contexts ******");
        lbm_all_ctxs_iterator(vescif_print_ctx_info, NULL,NULL);
      } else if (strncmp(str, ":symbols", 8) == 0) {
        lbm_symrepr_name_iterator(vescif_sym_it);
        commands_printf_lisp(" ");
      } else if (strncmp(str, ":reset", 6) == 0) {
        bool r = false;
        if (vescif_program_flash_code_len) {
          r = vescif_restart(true, true, true);
        } else {
          r = vescif_restart(true, false, true);
        }
        commands_printf_lisp(r ? "Reset OK\n\n" : "Reset Failed\n\n");
      } else if (strncmp(str, ":pause", 6) == 0) {
        lbm_pause_eval_with_gc(30);
        while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
          lbm_pause_eval();
          sleep_callback(1);
        }
        commands_printf_lisp("Evaluator paused\n");
      } else if (strncmp(str, ":continue", 9) == 0) {
        lbm_continue_eval();
      } else if (strncmp(str, ":undef", 6) == 0) {
        lbm_pause_eval();
        while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
          lbm_pause_eval();
          sleep_callback(1);
        }
        char *sym = str + 7;
        commands_printf_lisp("undefining: %s", sym);
        commands_printf_lisp("%s", lbm_undefine(sym) ? "Cleared bindings" : "No definition found");
        lbm_continue_eval();
      } else if (strncmp(str, ":verb", 5) == 0) {
        static bool verbose_now = false;
        verbose_now = !verbose_now;
        lbm_set_verbose(verbose_now);
        commands_printf_lisp("Verbose errors %s", verbose_now ? "Enabled" : "Disabled");
      } else {
        bool ok = true;
        int timeout_cnt = 1000;
        lbm_pause_eval_with_gc(30);
        while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
          sleep_callback(1000);
          timeout_cnt--;
        }
        ok = timeout_cnt > 0;

        if (ok) {
          char *buffer = malloc(len);
          if (buffer) {
            memcpy(buffer, data, len);
            lbm_create_string_char_channel(&string_tok_state, &string_tok, buffer);
            lbm_cid cid = lbm_load_and_eval_expression(&string_tok);
            if (cid >= 0) {
              add_reader(buffer, cid);
            } else {
              free(buffer);
              commands_printf_lisp("failed to spawn reader for REPL command\n");
            }
            lbm_continue_eval();
          } else {
            commands_printf_lisp("Not enough memory");
          }
        } else {
          commands_printf_lisp("Could not pause");
        }
      }
      //lispif_unlock_lbm();
    } else {
      commands_printf_lisp("LispBM is not running");
    }
  } break;

  case COMM_LISP_STREAM_CODE: {
    int32_t ind = 0;
    int32_t offset = buffer_get_int32(data, &ind);
    int32_t tot_len = buffer_get_int32(data, &ind);
    int8_t restart = (int8_t)data[ind++];

    static bool buffered_channel_created = false;
    static int32_t offset_last = -1;
    static int16_t result_last = -1;

    if (offset == 0) {
      if (!lispbm_thd) {
        vescif_restart(true, restart == 2 ? true : false, true);
        buffered_channel_created = false;
      } else if (restart == 1) {
        vescif_restart(true, false, true);
        buffered_channel_created = false;
      } else if (restart == 2) {
        vescif_restart(true, true, true);
        buffered_channel_created = false;
      }
    }

    int32_t send_ind = 0;
    uint8_t send_buffer[50];
    send_buffer[send_ind++] = packet_id;
    buffer_append_int32(send_buffer, offset, &send_ind);

    if (offset_last == offset) {
      buffer_append_int16(send_buffer, result_last, &send_ind);
      reply_func(send_buffer, (unsigned int)ind);
      break;
    }

    offset_last = offset;

    if (!lispbm_thd) {
      result_last = -1;
      offset_last = -1;
      buffer_append_int16(send_buffer, result_last, &send_ind);
      reply_func(send_buffer, (unsigned int)ind);
      break;
    }

    if (offset == 0) {
      if (buffered_channel_created) {
        int timeout = 1500;
        while (!buffered_tok_state.reader_closed) {
          lbm_channel_writer_close(&buffered_string_tok);
          sleep_callback(1000);
          timeout--;
          if (timeout == 0) {
            break;
          }
        }

        if (timeout == 0) {
          result_last = -2;
          offset_last = -1;
          buffer_append_int16(send_buffer, result_last, &send_ind);
          commands_printf_lisp("Reader not closing");
          reply_func(send_buffer, (unsigned int)ind);
          break;
        }
      }

      int timeout_cnt = 1000;
      //lispif_lock_lbm();
      lbm_pause_eval_with_gc(30);
      while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout_cnt > 0) {
        sleep_callback(1000);
        timeout_cnt--;
      }

      if (timeout_cnt == 0) {
        //lispif_unlock_lbm();
        result_last = -3;
        offset_last = -1;
        buffer_append_int16(send_buffer, result_last, &send_ind);
        commands_printf_lisp("Could not pause");
        reply_func(send_buffer, (unsigned int)ind);
        break;
      }

      lbm_create_buffered_char_channel(&buffered_tok_state, &buffered_string_tok);

      if (lbm_load_and_eval_program(&buffered_string_tok, "main-s") <= 0) {
        //lispif_unlock_lbm();
        result_last = -4;
        offset_last = -1;
        buffer_append_int16(send_buffer, result_last, &send_ind);
        commands_printf_lisp("Could not start eval");
        reply_func(send_buffer, (unsigned int)ind);
        break;
      }

      lbm_continue_eval();
      buffered_channel_created = true;
      //lispif_unlock_lbm();
    }

    int32_t written = 0;
    int timeout = 1500;
    while (ind < (int32_t)len) {
      int ch_res = lbm_channel_write(&buffered_string_tok, (char)data[ind]);

      if (ch_res == CHANNEL_SUCCESS) {
        ind++;
        written++;
        timeout = 0;
      } else if (ch_res == CHANNEL_READER_CLOSED) {
        break;
      } else {
        sleep_callback(1000);
        timeout--;
        if (timeout == 0) {
          break;
        }
      }
    }

    if (ind == (int32_t)len) {
      if ((offset + written) == tot_len) {
        lbm_channel_writer_close(&buffered_string_tok);
        offset_last = -1;
        commands_printf_lisp("Stream done, starting...");
      }

      result_last = 0;
      buffer_append_int16(send_buffer, result_last, &send_ind);
    } else {
      if (timeout == 0) {
        result_last = -5;
        offset_last = -1;
        buffer_append_int16(send_buffer, result_last, &send_ind);
        commands_printf_lisp("Stream timed out");
      } else {
        result_last = -6;
        offset_last = -1;
        buffer_append_int16(send_buffer, result_last, &send_ind);
        commands_printf_lisp("Stream closed");
      }
    }

    reply_func(send_buffer, (unsigned int)send_ind);
  } break;
  case COMM_LISP_WRITE_CODE: {
    int32_t ind = 0;
    int result = 0;
    uint32_t offset = buffer_get_uint32(data, &ind);

    size_t num = len - (size_t)ind; // length of data;
    if (num + offset < vescif_program_flash_size) {
      memcpy((uint8_t*)vescif_program_flash+offset, data+ind, num);
      vescif_program_flash_code_len = num;
      result = 1;
    }
    ind = 0;
    uint8_t send_buffer[50];
    send_buffer[ind++] = packet_id;
    send_buffer[ind++] = (uint8_t)result;
    buffer_append_uint32(send_buffer, offset, &ind);
    reply_func(send_buffer, (unsigned int)ind);
  } break;

  case COMM_LISP_READ_CODE: {
  }break;
  case COMM_LISP_ERASE_CODE: {
    memset(vescif_program_flash, 0, vescif_program_flash_size);
    vescif_program_flash_code_len = 0;
    int32_t ind = 0;
    uint8_t send_buffer[50];
    send_buffer[ind++] = packet_id;
    send_buffer[ind++] = 1;
    reply_func(send_buffer, (unsigned int)ind);
    break;
  }

  case COMM_LISP_RMSG: /* fall through */
  default:
    printf("Command %d is not supported by the LBM REPL VESC interface\n",packet_id);
    break;
  }

}



// ////////////////////////////////////////////////////////////
//

void *udp_broadcast_task(void *arg) {
  (void)arg;

  int sock = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);

  char hostbuffer[256];
  char *ip;
  gethostname(hostbuffer, sizeof(hostbuffer));
  struct hostent *host_entry;

  host_entry = gethostbyname(hostbuffer);

  ip = inet_ntoa(*((struct in_addr*)
                         host_entry->h_addr_list[0]));


  int bc = 1;
  setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &bc, sizeof(bc));

  struct sockaddr_in sDestAddr;
  memset(&sDestAddr, 0, sizeof(sDestAddr));
  sDestAddr.sin_family = AF_INET;
  sDestAddr.sin_addr.s_addr = htonl(INADDR_BROADCAST);
  sDestAddr.sin_port = htons(65109);

  char sendbuf[50];
  int ind = sprintf(sendbuf, "%s::%s::%u", "LBM-REPL",  ip, vesctcp_port) + 1;

  if (ind > 0) {
    for (;;) {
      sendto(sock, sendbuf, (size_t)ind, 0, (struct sockaddr *)&sDestAddr, sizeof(sDestAddr));
      sleep(2);
    }
  }
  return (void*)0;
}

void send_tcp_bytes(unsigned char *buffer, unsigned int len) {
  int to_write = (int)len;
  int error_cnt = 0;

  while (to_write > 0) {
    ssize_t written = write(connected_socket, buffer + ((int)len - to_write), (size_t)to_write);
    if (written < 0) {
      error_cnt ++;
      if (error_cnt > SEND_MAX_RETRY) {
        return;
      }
      sleep_callback(10);
    }
    to_write -= (int)written;
  }
}

PACKET_STATE_t packet;

void send_packet_local(unsigned char *data, unsigned int len) {
  packet_send_packet(data, len, &packet);
}

void process_packet_local(unsigned char *data, unsigned int len) {
  repl_process_cmd(data,len, send_packet_local);
}

void *vesctcp_client_handler(void *arg) {
  connected_socket = (int)arg;
  uint8_t buffer[1024];
  packet_init(send_tcp_bytes, process_packet_local,&packet);
  send_func = send_packet_local;
  ssize_t len;

  struct sockaddr_in addr;
  socklen_t addr_size = sizeof(struct sockaddr_in);
  getpeername(connected_socket, (struct sockaddr *)&addr, &addr_size);
  char ip[256];
  memset(ip,0,256);
  strncpy(ip, inet_ntoa(addr.sin_addr), 255);

  printf("Client %s connected\n",ip);

  vescif_restart(false,false,false);

  do {
    len = read(connected_socket, buffer, 1024);
    for (int i = 0; i < len; i ++) {
      packet_process_byte(buffer[i], &packet);
    }
  } while (len > 0);

  close(connected_socket);
  send_func = NULL;
  printf("Client %s disconnected\n",ip);
  vesctcp_server_in_use = false;
  return (void*)0;
}



// ////////////////////////////////////////////////////////////
//
int main(int argc, char **argv) {
  parse_opts(argc, argv);

  using_history();

  if (!init_repl()) {
    terminate_repl(REPL_EXIT_UNABLE_TO_INIT_LBM);
  }
  // TODO: Should the startup procedure work together with the VESC tcp serv?
  startup_procedure();

  if (vesctcp) {
    pthread_t broadcast_thread;
    pthread_t client_thread;
    pthread_create(&broadcast_thread, NULL, udp_broadcast_task, NULL);

    // initialize program flash
    vescif_program_flash_code_len = 0;
    vescif_program_flash=(uint8_t*)malloc(vescif_program_flash_size);
    if (vescif_program_flash == NULL) return 0;

    // Start tcp server
    struct sockaddr_in server_sockaddr_in;
    server_sockaddr_in.sin_family = AF_INET;
    server_sockaddr_in.sin_addr.s_addr = htonl(INADDR_ANY);
    server_sockaddr_in.sin_port = htons(vesctcp_port);

    int server_socket = socket(AF_INET, SOCK_STREAM, 0);

    bind(server_socket, (struct sockaddr *)&server_sockaddr_in, sizeof(server_sockaddr_in));

    listen(server_socket, 5);

    for (;;) {
      struct sockaddr_in client_sockaddr_in;
      socklen_t len = sizeof(client_sockaddr_in);

      int client_socket = accept(server_socket, (struct sockaddr *)&client_sockaddr_in, &len);

      if (client_socket >= 0 && !vesctcp_server_in_use ) {
        vesctcp_server_in_use = true;
        // TODO: is this cast really ok?
        pthread_create(&client_thread, NULL, vesctcp_client_handler, (void*)client_socket);

      } else if (client_socket >= 0) {
        char ip[256];
        memset(ip,0,256);
        strncpy(ip, inet_ntoa(client_sockaddr_in.sin_addr), 255);
        printf("Refusing connection from %s\n", ip);
        ssize_t r = write(client_socket, vesctcp_in_use, strlen(vesctcp_in_use));
        if (r < 0) {
          printf("Unable to write to refused client\n");
        }
      }
    }
  } else {

    char output[1024];

    while (1) {
      erase();
      char *str;
      if (silent_mode) {
        str = readline("");
      } else {
        str = readline("# ");
      }
      if (str == NULL) terminate_repl(REPL_EXIT_SUCCESS);
      add_history(str);
      size_t n = strlen(str);

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
	printf("Maximum usage %f%%\n", 100.0  * ((float)lbm_memory_maximum_used() / (float)lbm_memory_num_words()));
        printf("Allocated arrays: %"PRI_UINT"\n", heap_state.num_alloc_arrays);
        printf("Symbol table size RAM: %"PRI_UINT" Bytes\n", lbm_get_symbol_table_size());
        printf("Symbol names size RAM: %"PRI_UINT" Bytes\n", lbm_get_symbol_table_size_names());
        printf("Symbol table size FLASH: %"PRI_UINT" Bytes\n", lbm_get_symbol_table_size_flash());
        printf("Symbol names size FLASH: %"PRI_UINT" Bytes\n", lbm_get_symbol_table_size_names_flash());
        printf("--(Flash)--\n");
        printf("Size: %u words\n", const_heap.size);
        printf("Used words: %d\n", const_heap.next);
        printf("Free words: %d\n", const_heap.size - const_heap.next);
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
          printf("%"PRI_VALUE"\t%s\t%"PRI_UINT"\t%f\t%f\n",
                 prof_data[i].cid,
                 prof_data[i].name,
                 prof_data[i].count,
                 100.0 * ((float)prof_data[i].count) / (float) tot_samples,
                 100.0 * ((float)prof_data[i].gc_count) / (float)prof_data[i].count);
        }
        printf("\n");
        printf("GC:\t%"PRI_UINT"\t%f%%\n", tot_gc, 100.0 * ((float)tot_gc / (float)tot_samples));
        printf("System:\t%"PRI_UINT"\t%f%%\n", num_system, 100.0 * ((float)num_system / (float)tot_samples));
        printf("Sleep:\t%"PRI_UINT"\t%f%%\n", num_sleep, 100.0 * ((float)num_sleep / (float)tot_samples));
        printf("Total:\t%"PRI_UINT" samples\n", tot_samples);
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
          // TODO: Should free the file_str at some point!!
          // but it is hard to figure out when to do that if loading incrementally.
        } else {
          printf("Error loading file: %s\n",&str[5]);
        }
        free(str);
      } else if (n >= 5 && strncmp(str, ":verb", 5) == 0) {
        lbm_toggle_verbose();
        free(str);
        continue;
      } else if (n >= 4 && strncmp(str, ":pon", 4) == 0) {
        set_allow_print(true);
        free(str);
        continue;
      } else if (n >= 5 && strncmp(str, ":poff", 5) == 0) {
        set_allow_print(false);
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
            terminate_repl(REPL_EXIT_UNABLE_TO_INIT_LBM);
          }
        }
        free(str);
      } else if (strncmp(str, ":reset", 6) == 0) {
        if (!init_repl()) {
          printf ("Failed to initialize REPL\n");
          terminate_repl(REPL_EXIT_UNABLE_TO_INIT_LBM);
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
  }
  free(heap_storage);
  terminate_repl(REPL_EXIT_SUCCESS);
}
