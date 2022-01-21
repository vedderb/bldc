/*
    Copyright 2018, 2020, 2021, 2022 Joel Svensson  svenssonjoel@yahoo.se

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
  lbm_value program;
  lbm_value curr_exp;
  lbm_value curr_env;
  lbm_value mailbox;  /*massage passing mailbox */
  lbm_value r;
  bool  done;
  bool  app_cont;
  lbm_stack_t K;
  /* Process control */
  uint32_t timestamp;
  uint32_t sleep_us;
  lbm_cid id;
  /* List structure */
  struct eval_context_s *prev;
  struct eval_context_s *next;
} eval_context_t;

typedef void (*ctx_fun)(eval_context_t *, void*, void*);

/* Common interface */
extern lbm_value eval_cps_get_env(void);

/* Concurrent interface */
extern int lbm_eval_init(void);
extern bool lbm_remove_done_ctx(lbm_cid cid, lbm_value *v);
extern lbm_value lbm_wait_ctx(lbm_cid cid);
extern lbm_cid lbm_eval_program(lbm_value lisp);
extern lbm_cid lbm_eval_program_ext(lbm_value lisp, unsigned int stack_size);
extern void lbm_run_eval(void);

extern void lbm_pause_eval(void);
extern void lbm_step_eval(void);
extern void lbm_continue_eval(void);
extern void lbm_kill_eval(void);
extern uint32_t lbm_get_eval_state(void);

/* statistics interface */
extern void lbm_running_iterator(ctx_fun f, void*, void*);
extern void lbm_blocked_iterator(ctx_fun f, void*, void*);
extern void lbm_done_iterator(ctx_fun f, void*, void*);

/*
  Callback routines for sleeping and timestamp generation.
  Depending on target platform these will be implemented in different ways.
  Todo: It may become necessary to also add a mutex callback.
*/
extern void lbm_set_usleep_callback(void (*fptr)(uint32_t));
extern void lbm_set_timestamp_us_callback(uint32_t (*fptr)(void));
extern void lbm_set_ctx_done_callback(void (*fptr)(eval_context_t *));

/* loading of programs interface */
extern lbm_cid lbm_load_and_eval_program(lbm_tokenizer_char_stream_t *tokenizer);
extern lbm_cid lbm_load_and_eval_expression(lbm_tokenizer_char_stream_t *tokenizer);
extern lbm_cid lbm_load_and_define_program(lbm_tokenizer_char_stream_t *tokenizer, char *symbol);
extern lbm_cid lbm_load_and_define_expression(lbm_tokenizer_char_stream_t *tokenizer, char *symbol);

#endif
