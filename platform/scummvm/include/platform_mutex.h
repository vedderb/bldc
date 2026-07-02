/*
  No-op mutex for single-threaded ScummVM builds.
  LispBM is driven by step-based evaluation from the game loop,
  so no real locking is needed.
*/

#ifndef PLATFORM_MUTEX_H_
#define PLATFORM_MUTEX_H_

#include <stdbool.h>

typedef int lbm_mutex_t;

static inline bool lbm_mutex_init(lbm_mutex_t *m)   { (void)m; return true; }
static inline void lbm_mutex_lock(lbm_mutex_t *m)    { (void)m; }
static inline void lbm_mutex_unlock(lbm_mutex_t *m)  { (void)m; }

#endif
