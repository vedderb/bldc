/*
    Copyright 2023 Joel Svensson  svenssonjoel@yahoo.se

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

#ifndef LBM_PROF_H_
#define LBM_PROF_H_

#include "heap.h"
#include "eval_cps.h"

#define LBM_PROF_MAX_NAME_SIZE 20

typedef struct {
  lbm_cid cid;
  bool has_name;
  char name[LBM_PROF_MAX_NAME_SIZE];
  lbm_uint count;
  lbm_uint gc_count;
} lbm_prof_t;

bool lbm_prof_init(lbm_prof_t *prof_data_buf,
                   lbm_uint    prof_data_buf_num);
lbm_uint lbm_prof_get_num_samples(void);
lbm_uint lbm_prof_get_num_system_samples(void);
lbm_uint lbm_prof_get_num_sleep_samples(void);
lbm_uint lbm_prof_stop(void);
void lbm_prof_sample(void);

#endif
