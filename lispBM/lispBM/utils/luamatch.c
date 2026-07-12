/*
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
*/

/* This file containts the string pattern matching functionality
   from Lua. It has been extracted from lstrlib and lightly adapted
   for more standalone (from lua implementation specific idioms) portable
   application.
*/

#include "luamatch.h"
#include <ctype.h>
#include <string.h>

#define LM_ESC '%'

static int lm_match_class(int c, int cl) {
  int res;
  switch (tolower(cl)) {
    case 'a': res = isalpha(c); break;
    case 'c': res = iscntrl(c); break;
    case 'd': res = isdigit(c); break;
    case 'g': res = isgraph(c); break;
    case 'l': res = islower(c); break;
    case 'p': res = ispunct(c); break;
    case 's': res = isspace(c); break;
    case 'u': res = isupper(c); break;
    case 'w': res = isalnum(c); break;
    case 'x': res = isxdigit(c); break;
    case 'z': res = (c == 0); break; /* deprecated option */
    default: return (cl == c);
  }
  return (islower(cl) ? res : !res);
}

static int lm_matchbracketclass(int c, const char *p, const char *ec) {
  int sig = 1;
  if (*(p + 1) == '^') {
    sig = 0;
    p++;
  }
  while (++p < ec) {
    if (*p == LM_ESC) {
      p++;
      if (lm_match_class(c, (unsigned char)*p))
        return sig;
    } else if ((*(p + 1) == '-') && (p + 2 < ec)) {
      p += 2;
      if ((unsigned char)*(p - 2) <= c && c <= (unsigned char)*p)
        return sig;
    } else if ((unsigned char)*p == c)
      return sig;
  }
  return !sig;
}

static int lm_check_capture(lm_match_state_t *ms, int l) {
  l -= '1';
  if (l < 0 || l >= ms->level || ms->capture[l].len == LM_CAP_UNFINISHED) {
    ms->error = LM_ERR_INVALID_CAPTURE_INDEX;
    return -1;
  }
  return l;
}

static int lm_capture_to_close(lm_match_state_t *ms) {
  int level = ms->level;
  for (level--; level >= 0; level--)
    if (ms->capture[level].len == LM_CAP_UNFINISHED) return level;
  ms->error = LM_ERR_INVALID_PATTERN_CAPTURE;
  return -1;
}

static const char *lm_classend(lm_match_state_t *ms, const char *p) {
  switch (*p++) {
    case LM_ESC: {
      if (p == ms->p_end) {
        ms->error = LM_ERR_MALFORMED_PCT;
        return NULL;
      }
      return p + 1;
    }
    case '[': {
      if (*p == '^') p++;
      do {
        if (p == ms->p_end) {
          ms->error = LM_ERR_MALFORMED_SET;
          return NULL;
        }
        if (*(p++) == LM_ESC && p < ms->p_end) p++;
      } while (*p != ']');
      return p + 1;
    }
    default:
      return p;
  }
}

static int lm_singlematch(lm_match_state_t *ms, const char *s, const char *p,
                           const char *ep) {
  if (s >= ms->src_end) return 0;
  else {
    int c = (unsigned char)*s;
    switch (*p) {
      case '.': return 1;
      case LM_ESC: return lm_match_class(c, (unsigned char)*(p + 1));
      case '[': return lm_matchbracketclass(c, p, ep - 1);
      default: return ((unsigned char)*p == c);
    }
  }
}

static const char *lm_matchbalance(lm_match_state_t *ms, const char *s,
                                    const char *p) {
  if (p >= ms->p_end - 1) {
    ms->error = LM_ERR_MALFORMED_BALANCE;
    return NULL;
  }
  if (*s != *p) return NULL;
  else {
    int b = *p;
    int e = *(p + 1);
    int cont = 1;
    while (++s < ms->src_end) {
      if (*s == e) {
        if (--cont == 0) return s + 1;
      } else if (*s == b) cont++;
    }
  }
  return NULL; /* string ends out of balance */
}

/* forward declaration: match(), the expand functions, and the capture
   functions are all mutually recursive, exactly as in upstream lstrlib.c. */
static const char *lm_match(lm_match_state_t *ms, const char *s, const char *p);

static const char *lm_max_expand(lm_match_state_t *ms, const char *s,
                                  const char *p, const char *ep) {
  ptrdiff_t i = 0;
  while (lm_singlematch(ms, s + i, p, ep)) i++;
  while (i >= 0) {
    const char *res = lm_match(ms, (s + i), ep + 1);
    if (res) return res;
    if (ms->error != LM_ERR_NONE) return NULL;
    i--;
  }
  return NULL;
}

static const char *lm_min_expand(lm_match_state_t *ms, const char *s,
                                  const char *p, const char *ep) {
  for (;;) {
    const char *res = lm_match(ms, s, ep + 1);
    if (res != NULL) return res;
    /* Same fix as lm_max_expand: check error before treating NULL as
       "try one more repetition". */
    if (ms->error != LM_ERR_NONE) return NULL;
    if (lm_singlematch(ms, s, p, ep)) s++;
    else return NULL;
  }
}

static const char *lm_start_capture(lm_match_state_t *ms, const char *s,
                                     const char *p, int what) {
  const char *res;
  int level = ms->level;
  if (level >= LM_MAXCAPTURES) {
    ms->error = LM_ERR_TOO_MANY_CAPTURES;
    return NULL;
  }
  ms->capture[level].init = s;
  ms->capture[level].len = what;
  ms->level = level + 1;
  res = lm_match(ms, s, p);
  if (res == NULL) ms->level--; /* undo capture (also correct on error) */
  return res;
}

static const char *lm_end_capture(lm_match_state_t *ms, const char *s,
                                  const char *p) {
  int l = lm_capture_to_close(ms);
  const char *res;
  if (ms->error != LM_ERR_NONE) return NULL; /* l may be -1: see check above */
  ms->capture[l].len = s - ms->capture[l].init;
  res = lm_match(ms, s, p);
  if (res == NULL) ms->capture[l].len = LM_CAP_UNFINISHED;
  return res;
}

static const char *lm_match_capture(lm_match_state_t *ms, const char *s, int l) {
  size_t len;
  l = lm_check_capture(ms, l);
  if (ms->error != LM_ERR_NONE) return NULL; /* l may be -1: see check above */
  len = (size_t)ms->capture[l].len;
  if ((size_t)(ms->src_end - s) >= len &&
      memcmp(ms->capture[l].init, s, len) == 0)
    return s + len;
  else return NULL;
}

static const char *lm_match(lm_match_state_t *ms, const char *s, const char *p) {
  if (ms->matchdepth-- == 0) {
    ms->error = LM_ERR_TOO_COMPLEX;
    return NULL;
  }
  if (ms->iterations_left-- == 0) {
    // protection against non-termination.
    ms->error = LM_ERR_ITERATION_LIMIT;
    return NULL;
  }
init: /* using goto to optimize tail recursion, as upstream does */
  if (p != ms->p_end) {
    switch (*p) {
      case '(': {
        if (*(p + 1) == ')')
          s = lm_start_capture(ms, s, p + 2, LM_CAP_POSITION);
        else
          s = lm_start_capture(ms, s, p + 1, LM_CAP_UNFINISHED);
        break;
      }
      case ')': {
        s = lm_end_capture(ms, s, p + 1);
        break;
      }
      case '$': {
        if ((p + 1) != ms->p_end) goto dflt;
        s = (s == ms->src_end) ? s : NULL;
        break;
      }
      case LM_ESC: {
        switch (*(p + 1)) {
          case 'b': {
            s = lm_matchbalance(ms, s, p + 2);
            if (s != NULL) { p += 4; goto init; }
            break;
          }
          case 'f': {
            const char *ep;
            char previous;
            p += 2;
            if (*p != '[') {
              /* ERROR SITE 8: upstream's bare-statement luaL_error would
                 fall through into `classend(ms, p)` on a malformed
                 frontier pattern. Must bail immediately instead. */
              ms->error = LM_ERR_MALFORMED_FRONTIER;
              return NULL;
            }
            ep = lm_classend(ms, p);
            if (ms->error != LM_ERR_NONE) return NULL; /* classend may itself error */
            previous = (s == ms->src_init) ? '\0' : *(s - 1);
            if (!lm_matchbracketclass((unsigned char)previous, p, ep - 1) &&
                lm_matchbracketclass((unsigned char)*s, p, ep - 1)) {
              p = ep; goto init;
            }
            s = NULL;
            break;
          }
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9': {
            s = lm_match_capture(ms, s, (unsigned char)*(p + 1));
            if (s != NULL) { p += 2; goto init; }
            break;
          }
          default: goto dflt;
        }
        break;
      }
      default: dflt: {
        const char *ep = lm_classend(ms, p);
        if (ms->error != LM_ERR_NONE) return NULL; /* classend may itself error */
        if (!lm_singlematch(ms, s, p, ep)) {
          if (*ep == '*' || *ep == '?' || *ep == '-') { p = ep + 1; goto init; }
          else s = NULL;
        } else {
          switch (*ep) {
            case '?': {
              const char *res = lm_match(ms, s + 1, ep + 1);
              /* Fix vs. upstream: check error before treating a failed
                 optional-match as "fall back to zero occurrences". */
              if (ms->error != LM_ERR_NONE) return NULL;
              if (res != NULL) s = res;
              else { p = ep + 1; goto init; }
              break;
            }
            case '+':
              s++;
              /* FALLTHROUGH */
            case '*':
              s = lm_max_expand(ms, s, p, ep);
              break;
            case '-':
              s = lm_min_expand(ms, s, p, ep);
              break;
            default:
              s++; p = ep; goto init;
          }
        }
        break;
      }
    }
  }
  ms->matchdepth++;
  return s;
}

lm_status_t lm_find(const char *text, size_t text_len,
                     const char *pattern, size_t pattern_len,
                     size_t start_offset, size_t max_iterations,
                     lm_match_state_t *ms,
                     size_t *out_start, size_t *out_end) {
  const char *p = pattern;
  size_t lp = pattern_len;
  int anchor = (lp > 0 && *p == '^');
  const char *s1;

  if (anchor) { p++; lp--; }

  ms->src_init = text;
  ms->src_end = text + text_len;
  ms->p_end = p + lp;

  if (max_iterations == 0) max_iterations = LM_DEFAULT_MAX_ITERATIONS;

  if (start_offset > text_len) return LM_NOMATCH;

  s1 = text + start_offset;
  do {
    const char *res;
    ms->matchdepth = LM_MAXCCALLS;
    ms->iterations_left = max_iterations;
    ms->level = 0;
    ms->error = LM_ERR_NONE;
    res = lm_match(ms, s1, p);
    if (ms->error != LM_ERR_NONE) return LM_ERROR;
    if (res != NULL) {
      *out_start = (size_t)(s1 - text);
      *out_end = (size_t)(res - text);
      return LM_MATCH;
    }
  } while (s1++ < ms->src_end && !anchor);
  return LM_NOMATCH;
}
