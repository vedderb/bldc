/*
    Copyright 2019 Joel Svensson	svenssonjoel@yahoo.se

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

#include <stdio.h>
#include <stdlib.h>
#include "platform.h"
#include "xil_printf.h"


#include "heap.h"
#include "symrepr.h"
#include "builtin.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"

#define EVAL_CPS_STACK_SIZE

int inputline(char *buffer, int size) {
  int n = 0;
  char c;

  for (n = 0; n < size - 1; n++) {

    c = inbyte();
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      outbyte('\b'); /* output backspace character */
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        outbyte(c);
        buffer[n] = c;
      } else {
        n -= 1;
      }
      break;
    }
  }
  buffer[size - 1] = 0;
  return 0; // Filled up buffer without reading a linebreak
}


int main()
{
	init_platform();

	char *str = malloc(1024);;
	size_t len = 1024;
	int res = 0;

	lbm_heap_state_t heap_state;

	res = lbm_symrepr_init();
	if (res)
		printf("Symrepr initialized.\n");
	else {
		printf("Error initializing symrepr!\n");
		return 0;
	}
	int heap_size = 8 * 1024 * 1024;
	res = lbm_heap_init(heap_size);
	if (res)
		printf("Heap initialized. Heap size: %f MiB. Free cons cells: %lu\n", lbm_heap_size_bytes() / 1024.0 / 1024.0, lbm_heap_num_free());
	else {
		printf("Error initializing heap!\n");
		return 0;
	}

	res = builtin_init();
	if (res)
		printf("Built in functions initialized.\n");
	else {
		printf("Error initializing built in functions.\n");
		return 0;
	}

	res = eval_cps_init(EVAL_CPS_STACK_SIZE, true);
	if (res)
		printf("Evaluator initialized.\n");
	else {
		printf("Error initializing evaluator.\n");
	}

	printf("Lisp REPL started (ZYNQ)!\n");

	while (1) {
		printf("# "); fflush(stdout);
		memset(str,0,len);
		inputline(str, len);
		printf("\n");

		if (strncmp(str, ":info", 5) == 0) {
			printf("##(ZYNQ)####################################################\n");
			printf("Used cons cells: %lu \n", heap_size - lbm_heap_num_free());
			printf("ENV: "); simple_print(eval_cps_get_env()); printf("\n");
			//symrepr_print();
			//heap_perform_gc(eval_cps_get_env());
			lbm_get_heap_state(&heap_state);
			printf("GC counter: %lu\n", heap_state.gc_num);
			printf("Recovered: %lu\n", heap_state.gc_recovered);
			printf("Marked: %lu\n", heap_state.gc_marked);
			printf("Free cons cells: %lu\n", lbm_heap_num_free());
			printf("############################################################\n");
		} else {

			lbm_value t;
			t = tokpar_parse(str);

			t = lbm_eval_program(t);

			if (lbm_dec_sym(t) == symrepr_eerror) {
			  printf("Error\n");
			} else {
				printf("> "); simple_print(t); printf("\n");
			}
		}
	}

	symrepr_del();
	heap_del();

	cleanup_platform();
	return 0;
}
