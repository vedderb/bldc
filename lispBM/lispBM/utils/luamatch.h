/*
** Portable Lua pattern-matching engine, decoupled from lua_State.
**
** Derived from Lua 5.5's lstrlib.c (the MatchState/match()/classend()/
** singlematch()/matchclass()/matchbalance()/max_expand()/min_expand()/
** start_capture()/end_capture()/match_capture() core). Lua is distributed
** under the MIT license:
**
** Copyright (C) 1994-2026 Lua.org, PUC-Rio.
**
** Permission is hereby granted, free of charge, to any person obtaining
** a copy of this software and associated documentation files (the
** "Software"), to deal in the Software without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Software, and to
** permit persons to whom the Software is furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be
** included in all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**
** Adapted to LBM (lua_State removed, error signaling converted to a
** sticky-flag return-value scheme) by Joel Svensson in 2026.
*/

#ifndef LUAMATCH_H_
#define LUAMATCH_H_

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define LM_MAXCAPTURES   32     /* mirrors Lua's LUA_MAXCAPTURES */

#ifndef LM_MAXCCALLS
#define LM_MAXCCALLS     60     /* conservative vs Lua's default 200:
                                    tuned for small embedded C stacks.
                                    Bounds match()'s C-stack RECURSION
                                    DEPTH only (deeply nested patterns) --
                                    see LM_DEFAULT_MAX_ITERATIONS below for
                                    the separate, independent guard against
                                    exponential-time backtracking. */
#endif

#ifndef LM_DEFAULT_MAX_ITERATIONS
#define LM_DEFAULT_MAX_ITERATIONS 100000  /* default total-work budget (see
                                    lm_find's max_iterations parameter):
                                    caps the total number of lm_match()
                                    calls across one lm_find() attempt.
                                    Unlike LM_MAXCCALLS, this guards against
                                    catastrophic backtracking (e.g. chained
                                    ".*"/"a*" groups against a long
                                    non-matching string), which stays at
                                    roughly constant C-stack depth but does
                                    exponential total work -- a case
                                    LM_MAXCCALLS does NOT catch. This
                                    matters more here than in most
                                    contexts: an LBM extension call runs to
                                    completion as one atomic C call and
                                    cannot be preempted by LBM's scheduler
                                    mid-call, so a pathological pattern
                                    would otherwise freeze the whole
                                    runtime, not just one thread. */
#endif

#define LM_CAP_UNFINISHED (-1)
#define LM_CAP_POSITION   (-2)

typedef struct {
  const char *init;
  ptrdiff_t   len;      /* >=0 length, or LM_CAP_UNFINISHED / LM_CAP_POSITION */
} lm_capture_t;

typedef enum {
  LM_NOMATCH = 0,
  LM_MATCH   = 1,
  LM_ERROR   = 2
} lm_status_t;

typedef enum {
  LM_ERR_NONE = 0,
  LM_ERR_MALFORMED_PCT,           /* pattern ends with '%' */
  LM_ERR_MALFORMED_SET,           /* missing ']' in a [...] class */
  LM_ERR_MALFORMED_BALANCE,       /* missing args to '%b' */
  LM_ERR_MALFORMED_FRONTIER,      /* missing '[' after '%f' */
  LM_ERR_TOO_MANY_CAPTURES,
  LM_ERR_INVALID_PATTERN_CAPTURE, /* unmatched ')' */
  LM_ERR_INVALID_CAPTURE_INDEX,   /* bad or unfinished %N backreference */
  LM_ERR_TOO_COMPLEX,             /* matchdepth (recursion depth) exhausted */
  LM_ERR_ITERATION_LIMIT          /* max_iterations (total work) exhausted */
} lm_error_t;

typedef struct {
  const char   *src_init, *src_end, *p_end;
  int           matchdepth;
  size_t        iterations_left;            /* total-work budget, see lm_find */
  int           level;                      /* number of captures */
  lm_capture_t  capture[LM_MAXCAPTURES];
  lm_error_t    error;
} lm_match_state_t;

/*
** Scans text[start_offset..text_len) for the first match of
** pattern[0..pattern_len), honoring a leading '^' anchor (if present,
** only start_offset itself is tried, mirroring Lua's own find/match/gsub
** anchor behavior).
**
** `max_iterations` bounds the total number of lm_match() calls made
** during this one attempt (checked/decremented independently of, and in
** addition to, the LM_MAXCCALLS recursion-depth guard -- see the comment
** on LM_DEFAULT_MAX_ITERATIONS for why both are needed). Pass 0 to use
** LM_DEFAULT_MAX_ITERATIONS.
**
** On LM_MATCH: *out_start and *out_end are byte offsets into `text` spanning
** the whole match; ms->capture[0..ms->level-1] describe any sub-captures,
** as {pointer into `text`, length} pairs (or LM_CAP_POSITION for a
** position capture "()"). Captures remain valid only as long as `text`
** is not freed or mutated.
**
** On LM_ERROR: ms->error explains why -- malformed pattern, too many
** captures, pattern nested too deeply (LM_ERR_TOO_COMPLEX), or too much
** total backtracking work (LM_ERR_ITERATION_LIMIT).
**
** On LM_NOMATCH: no match was found starting at or after start_offset.
**
** Returns LM_NOMATCH immediately (without touching ms) if
** start_offset > text_len.
*/
lm_status_t lm_find(const char *text, size_t text_len,
                     const char *pattern, size_t pattern_len,
                     size_t start_offset, size_t max_iterations,
                     lm_match_state_t *ms,
                     size_t *out_start, size_t *out_end);

#ifdef __cplusplus
}
#endif
#endif
