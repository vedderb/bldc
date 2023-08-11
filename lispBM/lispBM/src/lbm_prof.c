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

#include "lbm_prof.h"
#include "platform_mutex.h"

static lbm_uint num_samples = 0;
static lbm_uint num_system_samples = 0;
static lbm_uint num_sleep_samples = 0;
extern eval_context_t *ctx_running;
extern mutex_t qmutex;
extern bool    qmutex_initialized;
extern volatile bool lbm_system_sleeping;

static lbm_prof_t *prof_data;
static lbm_uint    prof_data_num;

#define TRUNC_SIZE(N) (((N) > LBM_PROF_MAX_NAME_SIZE -1) ? LBM_PROF_MAX_NAME_SIZE-1 : N)

bool lbm_prof_init(lbm_prof_t *prof_data_buf,
                   lbm_uint    prof_data_buf_num) {
  if (qmutex_initialized && prof_data_buf && prof_data_buf_num > 0) {
    num_samples = 0;
    num_system_samples = 0;
    num_sleep_samples = 0;
    prof_data_num = prof_data_buf_num;
    prof_data = prof_data_buf;
    for (lbm_uint i = 0; i < prof_data_num; i ++) {
      prof_data_buf[i].cid  = -1;
      prof_data[i].has_name = false;
      memset(&prof_data_buf[i].name, 0, LBM_PROF_MAX_NAME_SIZE);
      prof_data_buf[i].count = 0;
    }
  }
  return false;
}

lbm_uint lbm_prof_get_num_samples(void) {
  return num_samples;
}

lbm_uint lbm_prof_get_num_system_samples(void) {
  return num_system_samples;
}

lbm_uint lbm_prof_get_num_sleep_samples(void) {
  return num_sleep_samples;
}

void lbm_prof_sample(void) {
  num_samples ++;

  // Lock mutex so context cannot be destroyed until
  // we are done storing a sample.
  mutex_lock(&qmutex);
  eval_context_t *curr = ctx_running;
  if (curr != NULL) {
    lbm_cid id = curr->id;
    char *name = curr->name;
    lbm_uint name_len = 0;
    bool doing_gc = false;
    if (curr->state & LBM_THREAD_STATE_GC_BIT) {
      doing_gc = true;
    }
    if (name) name_len = strlen(name) + 1;
    for (lbm_uint i = 0; i < prof_data_num; i ++) {
      if (prof_data[i].cid == -1) {
        // add new sample:
        prof_data[i].cid = id;
        prof_data[i].count = 1;
        prof_data[i].gc_count = doing_gc ? 1 : 0;
        if (name) {
          memcpy(&prof_data[i].name, name, TRUNC_SIZE(name_len));
          prof_data[i].name[LBM_PROF_MAX_NAME_SIZE - 1] = 0;
          prof_data[i].has_name = true;
        }
        break;
      }
      if (prof_data[i].cid == id &&
          prof_data[i].has_name &&
          name != NULL &&
          strncmp(prof_data[i].name, name, TRUNC_SIZE(name_len)) == 0) {
        // found a named existing measurement.
        prof_data[i].count ++;
        prof_data[i].gc_count += doing_gc ? 1 : 0;
        break;
      }
      if (prof_data[i].cid == id &&
          !prof_data[i].has_name &&
          name == NULL) {
        prof_data[i].count ++;
        prof_data[i].gc_count += doing_gc ? 1 : 0;
        break;
      }
    }
  } else {
    if (lbm_system_sleeping) {
      num_sleep_samples ++;
    } else {
      num_system_samples ++;
    }
  }
  mutex_unlock(&qmutex);
}
