/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

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
#include <stdio.h>
#include <stdlib.h>

#include <lispbm.h>

#define MAX_GNUPLOT_INSTANCES 5

FILE *gnuplot_instances[MAX_GNUPLOT_INSTANCES];

lbm_value ext_gnuplot_open(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;

  int ix = -1;

  for (int i = 0; i < MAX_GNUPLOT_INSTANCES; i ++) {
    if (gnuplot_instances[i] == NULL) {
      ix = i;
      break;
    }
  }

  if (ix >= 0) {
    FILE *gp = popen("gnuplot -persist", "w");
    if (gp) {
      gnuplot_instances[ix] = gp;
      return lbm_enc_i(ix);
    }
  }
  return ENC_SYM_NIL;
}

lbm_value ext_gnuplot_close(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_number(args[0])) {

    int32_t ix = lbm_dec_as_i32(args[0]);
    if (ix >= 0 && gnuplot_instances[ix]) {
      fclose(gnuplot_instances[ix]);
      gnuplot_instances[ix] = NULL;
      res = ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;
}

lbm_value ext_gnuplot_cmd(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_array_r(args[1])) {

    int32_t ix = lbm_dec_as_i32(args[0]);
    if (ix >= 0 && gnuplot_instances[ix]) {

      char *cmd = lbm_dec_str(args[1]);
      fprintf(gnuplot_instances[ix],"%s\n", cmd);
      fflush(gnuplot_instances[ix]);
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  return res;
}

lbm_value ext_gnuplot_data(lbm_value *args, lbm_uint argn) {
  // first deal with data passed as lists of:
  //   x, y value pairs
  //   y values
  
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 3 &&
      lbm_is_number(args[0]) && // gnuplot instance
      lbm_is_array_r(args[1]) &&
      lbm_is_list(args[2])) {
    int32_t ix = lbm_dec_as_i32(args[0]);
    if (ix >= 0 && gnuplot_instances[ix]) {
      char *name = lbm_dec_str(args[1]);
      fprintf(gnuplot_instances[ix], "$%s << EOD\n", name);
      lbm_value curr = args[2];
      while (lbm_is_cons(curr)) {
        lbm_value item = lbm_car(curr);
        if (lbm_is_cons(item)) {
          lbm_value x = lbm_car(item);
          lbm_value y = lbm_car(lbm_cdr(item));
          if (lbm_is_number(x) && lbm_is_number(y)) {
            fprintf(gnuplot_instances[ix], "%f %f\n", lbm_dec_as_float(x), lbm_dec_as_float(y));
          } else {
            goto gnuplot_data_done;
          }
        } else if (lbm_is_number(item)) {
          fprintf(gnuplot_instances[ix], "%f\n", lbm_dec_as_float(item));
        } else {
          goto gnuplot_data_done;
        }
        curr = lbm_cdr(curr);
      }
      fprintf(gnuplot_instances[ix], "EOD\n");
      fflush(gnuplot_instances[ix]);
      res = ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
 gnuplot_data_done:
  return res;
}


bool lbm_gnuplot_init(void) {

  for (int i = 0; i < MAX_GNUPLOT_INSTANCES; i ++) {
    gnuplot_instances[i] = NULL;
  }

  lbm_add_extension("gnuplot-open", ext_gnuplot_open);
  lbm_add_extension("gnuplot-close", ext_gnuplot_close);
  lbm_add_extension("gnuplot-cmd", ext_gnuplot_cmd);
  lbm_add_extension("gnuplot-data", ext_gnuplot_data);
  
  return lbm_get_num_extensions() < lbm_get_max_extensions();
}
