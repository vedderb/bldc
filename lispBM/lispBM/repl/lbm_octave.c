/*
    Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

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

#define MAX_OCTAVE_INSTANCES 5

FILE *octave_instances[MAX_OCTAVE_INSTANCES];

lbm_value ext_octave_open(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;

  lbm_value res = ENC_SYM_NIL;
  int ix = -1;
  
  for (int i = 0; i < MAX_OCTAVE_INSTANCES; i ++) {
    if (octave_instances[i] == NULL) {
      ix = i;
      break;
    }
  }

  if (ix >= 0) {
    FILE *o = popen("octave-cli --silent --no-init-file", "w");
    if (o) {
      octave_instances[ix] = o;
      res = lbm_enc_i(ix);
    }
  }
  return res;
}

lbm_value ext_octave_close(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_number(args[0])) {
    int32_t ix = lbm_dec_as_i32(args[0]);
    if (ix >= 0 && ix < MAX_OCTAVE_INSTANCES && octave_instances[ix]) {
      fclose(octave_instances[ix]);
      octave_instances[ix] = NULL;
      res = ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;
}

lbm_value ext_octave_cmd(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_array_r(args[1])) {

    int32_t ix = lbm_dec_as_i32(args[0]);
    if (ix >= 0 && ix < MAX_OCTAVE_INSTANCES && octave_instances[ix]) {
      char *cmd = lbm_dec_str(args[1]);
      fprintf(octave_instances[ix], "%s\n", cmd);
      fflush(octave_instances[ix]);
      res = ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;
}

bool lbm_octave_init(void) {
  for (int i = 0; i < MAX_OCTAVE_INSTANCES; i ++) {
    octave_instances[i] = NULL;
  }

  lbm_add_extension("octave-open", ext_octave_open);
  lbm_add_extension("octave-close", ext_octave_close);
  lbm_add_extension("octave-cmd", ext_octave_cmd);
  
  return lbm_get_num_extensions() < lbm_get_max_extensions();
}
