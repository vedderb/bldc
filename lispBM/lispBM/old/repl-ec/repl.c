/*
    Copyright 2018, 2020 Joel Svensson	svenssonjoel@yahoo.se

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

#include "heap.h"
#include "symrepr.h"
#include "extensions.h"
#include "ec_eval.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "typedefs.h"
#include "memory.h"
#include "env.h"

#define EVAL_CPS_STACK_SIZE 256

lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  if (argn < 1) return lbm_enc_sym(symrepr_nil);

  char output[1024];
  char error[1024];

  for (int i = 0; i < argn; i ++) {
    lbm_value t = args[i];

    if (lbm_is_ptr(t) && ptr_type(t) == LBM_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
      switch (array->elt_type){
      case LBM_TYPE_CHAR: {
	char *data = (char *)array + 8;
	printf("%s", data);
	break;
      }
      default:
	return lbm_enc_sym(symrepr_nil);
	break;
      }
    } else if (val_type(t) == LBM_TYPE_CHAR) {
      printf("%c", lbm_dec_char(t));
    } else {
      int print_ret = lbm_print_value(output, 1024, error, 1024, t);

      if (print_ret >= 0) {
	printf("%s", output);
      } else {
	printf("%s", error);
      }
    }

  }
  return lbm_enc_sym(symrepr_true);
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

  if (strlen(&filename[i]) > 0) {
    errno = 0;
    fp = fopen(&filename[i], "r");
    if (!fp) {
      printf("cannot fopen file %s\n", &filename[i]);
      printf("filename length: %d\n", strlen(&filename[i]));
      printf("%d\n", errno);
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

int main(int argc, char **argv) {
  char *str = malloc(1024);;
  unsigned int len = 1024;
  int res = 0;

  lbm_heap_state_t heap_state;

  unsigned char *memory = malloc(MEMORY_SIZE_16K);
  unsigned char *bitmap = malloc(MEMORY_BITMAP_SIZE_16K);
  if (memory == NULL || bitmap == NULL) return 0;

  res = lbm_memory_init(memory, MEMORY_SIZE_16K,
		    bitmap, MEMORY_BITMAP_SIZE_16K);
  if (res)
    printf("Memory initialized. Memory size: %u Words. Free: %u Words.\n", lbm_memory_num_words(), lbm_memory_num_free());
  else {
    printf("Error initializing memory!\n");
    return 0;
  }

  res = lbm_symrepr_init();
  if (res)
    printf("Symrepr initialized.\n");
  else {
    printf("Error initializing symrepr!\n");
    return 0;
  }

  unsigned int heap_size = 8192;
  res = lbm_heap_init(heap_size);
  if (res)
    printf("Heap initialized. Heap size: %f MiB. Free cons cells: %d\n", lbm_heap_size_bytes() / 1024.0 / 1024.0, lbm_heap_num_free());
  else {
    printf("Error initializing heap!\n");
    return 0;
  }

  res = lbm_init_env();
  if (res)
    printf("Environment initialized.\n");
  else {
    printf("Error initializing environment!\n");
    return 0;
  }

  res = lbm_add_extension("print", ext_print);
  if (res)
    printf("Extension added.\n");
  else {
    printf("Error adding extension.\n");
    return 0;
  }

  lbm_value prelude = prelude_load();
  ec_eval_program(prelude);

  printf("Lisp REPL started!\n");
  printf("Type :quit to exit.\n");
  printf("     :info for statistics.\n");
  printf("     :load [filename] to load lisp source.\n");

  char output[1024];
  char error[1024];

  while (1) {
    fflush(stdin);
    printf("# ");
    memset(str, 0 ,len);

    ssize_t n = getline(&str,&len, stdin);
    printf("\n");

    if (n >= 5 && strncmp(str, ":info", 5) == 0) {
      printf("############################################################\n");
      printf("Used cons cells: %d\n", heap_size - lbm_heap_num_free());
      int r = lbm_print_value(output, 1024, error, 1024, *lbm_get_env_ptr());
      if (r >= 0) {
	printf("ENV: %s\n", output );
      } else {
	printf("%s\n", error);
      }
      int env_len = 0;
      lbm_value curr = *lbm_get_env_ptr();
      while (lbm_type_of(curr) == LBM_TYPE_CONS) {
	env_len ++;
	curr = lbm_cdr(curr);
      }
      printf("Global env num bindings: %d\n", env_len);
      lbm_get_heap_state(&heap_state);
      printf("Symbol table size: %u Bytes\n", lbm_get_symbol_table_size());
      printf("Heap size: %u Bytes\n", heap_size * 8);
      printf("Memory size: %u Words\n", lbm_memory_num_words());
      printf("Memory free: %u Words\n", lbm_memory_num_free());
      printf("Allocated arrays: %u\n", heap_state.num_alloc_arrays);
      printf("GC counter: %d\n", heap_state.gc_num);
      printf("Recovered: %d\n", heap_state.gc_recovered);
      printf("Recovered arrays: %u\n", heap_state.gc_recovered_arrays);
      printf("Marked: %d\n", heap_state.gc_marked);
      printf("Free cons cells: %d\n", lbm_heap_num_free());
      printf("############################################################\n");
    } else if (n >= 5 && strncmp(str, ":load", 5) == 0) {
      unsigned int fstr_len = strlen(&str[5]);
      str[5+fstr_len-1] = 0;
      char *file_str = load_file(&str[5]);
      if (file_str) {
	printf("Loading file %s\n", &str[5]);
	lbm_value f_exp = tokpar_parse(file_str);
	free(file_str);
	lbm_value l_r = ec_eval_program(f_exp);
	int r = lbm_print_value(output, 1024, error, 1024, l_r);
	if (r >= 0) {
	  printf("> %s\n", output );
	} else {
	  printf("%s\n", error);
	}
      } else {
	printf("Failed to load file %s\n", &str[5]);
      }


    }  else if (n >= 5 && strncmp(str, ":quit", 5) == 0) {
      break;
    } else {

      lbm_value t;
      t = tokpar_parse(str);

      lbm_value r_r = ec_eval_program(t);

      int r = lbm_print_value(output, 1024, error, 1024, r_r);
      if (r >= 0) {
	printf("> %s\n", output );
      } else {
	printf("%s\n", error);
      }

    }
  }

  symrepr_del();
  heap_del();

  return 0;
}
