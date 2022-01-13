/*
    Copyright 2018, 2020, 2021 Joel Svensson  svenssonjoel@yahoo.se

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
#ifndef EVAL_CPS_H_
#define EVAL_CPS_H_

#include "stack.h"
#include "lispbm_types.h"

#define EVAL_CPS_STATE_INIT    0
#define EVAL_CPS_STATE_PAUSED  1
#define EVAL_CPS_STATE_RUNNING 2
#define EVAL_CPS_STATE_STEP    3
#define EVAL_CPS_STATE_KILL    4


typedef struct eval_context_s{
  VALUE program;
  VALUE curr_exp;
  VALUE curr_env;
  VALUE mailbox;  /*massage passing mailbox */
  VALUE r;
  bool  done;
  bool  app_cont;
  stack K;
  /* Process control */
  uint32_t timestamp;
  uint32_t sleep_us;
  CID id;
  /* List structure */
  struct eval_context_s *prev;
  struct eval_context_s *next;
} eval_context_t;

typedef void (*ctx_fun)(eval_context_t *, void*, void*);

/* Common interface */
extern VALUE eval_cps_get_env(void);

/* Concurrent interface */
extern int eval_cps_init(void);
extern bool eval_cps_remove_done_ctx(CID cid, VALUE *v);
extern VALUE eval_cps_wait_ctx(CID cid);
extern CID eval_cps_program(VALUE lisp);
extern CID eval_cps_program_ext(VALUE lisp, unsigned int stack_size, bool grow_stack);
extern void eval_cps_run_eval(void);

extern void eval_cps_pause_eval(void);
extern void eval_cps_step_eval(void);
extern void eval_cps_continue_eval(void); 
extern void eval_cps_kill_eval(void);
extern uint32_t eval_cps_current_state(void);

/* statistics interface */
extern void eval_cps_running_iterator(ctx_fun f, void*, void*);
extern void eval_cps_blocked_iterator(ctx_fun f, void*, void*);
extern void eval_cps_done_iterator(ctx_fun f, void*, void*);

/*
  Callback routines for sleeping and timestamp generation.
  Depending on target platform these will be implemented in different ways.
  Todo: It may become necessary to also add a mutex callback.
*/
extern void eval_cps_set_usleep_callback(void (*fptr)(uint32_t));
extern void eval_cps_set_timestamp_us_callback(uint32_t (*fptr)(void));
extern void eval_cps_set_ctx_done_callback(void (*fptr)(eval_context_t *));

/* Non concurrent interface: */
extern int eval_cps_init_nc(unsigned int stack_size, bool grow_stack);
extern void eval_cps_del_nc(void);
extern VALUE eval_cps_program_nc(VALUE lisp);
#endif
