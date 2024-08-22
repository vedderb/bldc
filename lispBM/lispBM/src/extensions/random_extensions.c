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

#include <extensions.h>
#include <lbm_utils.h>

#define M 268435183 //(1 << 28)
#define A 268435043
#define C 268434949


static lbm_uint random_seed = 177739;

static lbm_value ext_seed(lbm_value *args, lbm_uint argn) {

  LBM_CHECK_ARGN_NUMBER(1);

  random_seed = lbm_dec_as_u32(args[0]);
  return ENC_SYM_TRUE;
}

static lbm_value ext_random(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  random_seed = (A * random_seed + C) % M;
  return lbm_enc_u(random_seed);
}

bool lbm_random_extensions_init(void) {

  bool res = true;
  res = res && lbm_add_extension("seed", ext_seed);
  res = res && lbm_add_extension("random", ext_random);
  return res;
}
