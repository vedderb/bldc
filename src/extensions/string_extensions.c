/*
    Copyright 2022 Joel Svensson        svenssonjoel@yahoo.se
    Copyright 2022 Benjamin Vedder

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

#include "extensions.h"
#include "lbm_memory.h"
#include "heap.h"
#include "fundamental.h"
#include "lbm_c_interop.h"

#include <ctype.h>

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif


static lbm_value ext_str_from_n(lbm_value *args, lbm_uint argn) {
  if ((argn != 1 && argn != 2) || !lbm_is_number(args[0])) {
    return lbm_enc_sym(SYM_EERROR);
  }

  if (argn == 2 && lbm_type_of(args[1]) != LBM_TYPE_ARRAY) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *format = 0;
  if (argn == 2) {
    format = lbm_dec_str(args[1]);
  }

  char buffer[100];
  size_t len = 0;

  switch (lbm_type_of(args[0])) {
  case LBM_TYPE_FLOAT:
    if (!format) {
      format = "%f";
    }
    len = (size_t)snprintf(buffer, sizeof(buffer), format, (double)lbm_dec_as_float(args[0]));
    break;

  default:
    if (!format) {
      format = "%d";
    }
    len = (size_t)snprintf(buffer, sizeof(buffer), format, lbm_dec_as_i32(args[0]));
    break;
  }

  if (len > sizeof(buffer)) {
    len = sizeof(buffer);
  }

  lbm_value res;
  if (lbm_create_array(&res, LBM_TYPE_CHAR, len + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
    memcpy(arr->data, buffer, len);
    ((char*)(arr->data))[len] = '\0';
    return res;
  } else {
    return lbm_enc_sym(SYM_MERROR);
  }
}

static lbm_value ext_str_merge(lbm_value *args, lbm_uint argn) {
  int len_tot = 0;
  for (unsigned int i = 0;i < argn;i++) {
    char *str = lbm_dec_str(args[i]);
    if (str) {
      len_tot += (int)strlen(str);
    } else {
      return lbm_enc_sym(SYM_EERROR);
    }
  }

  lbm_value res;
  if (lbm_create_array(&res, LBM_TYPE_CHAR, (lbm_uint)len_tot + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
    unsigned int offset = 0;
    for (unsigned int i = 0;i < argn;i++) {
      offset += (unsigned int)sprintf((char*)arr->data + offset, "%s", lbm_dec_str(args[i]));
    }
    ((char*)(arr->data))[len_tot] = '\0';
    return res;
  } else {
    return lbm_enc_sym(SYM_MERROR);
  }
}

static lbm_value ext_str_to_i(lbm_value *args, lbm_uint argn) {
  if (argn != 1 && argn != 2) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *str = lbm_dec_str(args[0]);
  if (!str) {
    return lbm_enc_sym(SYM_EERROR);
  }

  int base = 0;
  if (argn == 2) {
    if (!lbm_is_number(args[1])) {
      return lbm_enc_sym(SYM_EERROR);
    }

    base = lbm_dec_as_i32(args[1]);
  }

  return lbm_enc_i(strtol(str, NULL, base));
}

static lbm_value ext_str_to_f(lbm_value *args, lbm_uint argn) {
  if (argn != 1) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *str = lbm_dec_str(args[0]);
  if (!str) {
    return lbm_enc_sym(SYM_EERROR);
  }

  return lbm_enc_float(strtof(str, NULL));
}

static lbm_value ext_str_part(lbm_value *args, lbm_uint argn) {
  if ((argn != 2 && argn != 3) || !lbm_is_number(args[1])) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *str = lbm_dec_str(args[0]);
  if (!str) {
    return lbm_enc_sym(SYM_EERROR);
  }

  size_t len = strlen(str);

  uint32_t start = lbm_dec_as_u32(args[1]);

  if (start >= len) {
    return lbm_enc_sym(SYM_EERROR);
  }

  uint32_t n = (uint32_t)len - start;
  if (argn == 3) {
    if (!lbm_is_number(args[2])) {
      return lbm_enc_sym(SYM_EERROR);
    }

    n = MIN(lbm_dec_as_u32(args[2]), n);
  }

  lbm_value res;
  if (lbm_create_array(&res, LBM_TYPE_CHAR, n + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
    memcpy(arr->data, str + start, n);
    ((char*)(arr->data))[n] = '\0';
    return res;
  } else {
    return lbm_enc_sym(SYM_MERROR);
  }
}

static lbm_value ext_str_split(lbm_value *args, lbm_uint argn) {
  if (argn != 2) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *str = lbm_dec_str(args[0]);
  if (!str) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *split = lbm_dec_str(args[1]);
  int step = 0;
  if (!split) {
    if (lbm_is_number(args[1])) {
      step = MAX(lbm_dec_as_i32(args[1]), 1);
    } else {
      return lbm_enc_sym(SYM_EERROR);
    }
  }

  if (step > 0) {
    lbm_value res = lbm_enc_sym(SYM_NIL);
    int len = (int)strlen(str);
    for (int i = len / step;i >= 0;i--) {
      int ind_now = i * step;
      if (ind_now >= len) {
        continue;
      }

      int step_now = step;
      while ((ind_now + step_now) > len) {
        step_now--;
      }

      lbm_value tok;
      if (lbm_create_array(&tok, LBM_TYPE_CHAR, (lbm_uint)step_now + 1)) {
        lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(tok);
        memcpy(arr->data, str + ind_now, (size_t)step_now);
        ((char*)(arr->data))[step_now] = '\0';
        res = lbm_cons(tok, res);
      } else {
        return lbm_enc_sym(SYM_MERROR);
      }
    }

    return res;
  } else {
    lbm_value res = lbm_enc_sym(SYM_NIL);
    const char *s = str;
    while (*(s += strspn(s, split)) != '\0') {
      size_t len = strcspn(s, split);

      lbm_value tok;
      if (lbm_create_array(&tok, LBM_TYPE_CHAR, len + 1)) {
        lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(tok);
        memcpy(arr->data, s, len);
        ((char*)(arr->data))[len] = '\0';
        res = lbm_cons(tok, res);
      } else {
        return lbm_enc_sym(SYM_MERROR);
      }

      s += len;
    }

    return lbm_list_destructive_reverse(res);
  }
}

static lbm_value ext_str_replace(lbm_value *args, lbm_uint argn) {
  if (argn != 2 && argn != 3) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *orig = lbm_dec_str(args[0]);
  if (!orig) {
    return lbm_enc_sym(SYM_TERROR);
  }

  char *rep = lbm_dec_str(args[1]);
  if (!rep) {
    return lbm_enc_sym(SYM_TERROR);
  }

  char *with = "";
  if (argn == 3) {
    with = lbm_dec_str(args[2]);
    if (!with) {
      return lbm_enc_sym(SYM_TERROR);
    }
  }

  // See https://stackoverflow.com/questions/779875/what-function-is-to-replace-a-substring-from-a-string-in-c
  char *result; // the return string
  char *ins;    // the next insert point
  char *tmp;    // varies
  int len_rep;  // length of rep (the string to remove)
  int len_with; // length of with (the string to replace rep with)
  int len_front; // distance between rep and end of last rep
  int count;    // number of replacements

  len_rep = (int)strlen(rep);
  if (len_rep == 0) {
    return args[0]; // empty rep causes infinite loop during count
  }

  len_with = (int)strlen(with);

  // count the number of replacements needed
  ins = orig;
  for (count = 0; (tmp = strstr(ins, rep)); ++count) {
    ins = tmp + len_rep;
  }
  
  size_t len_res = strlen(orig) + (size_t)((len_with - len_rep) * count + 1);
  lbm_value lbm_res;
  if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, len_res)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
    tmp = result = (char*)arr->data;
  } else {
    return lbm_enc_sym(SYM_MERROR);
  }

  // first time through the loop, all the variable are set correctly
  // from here on,
  //    tmp points to the end of the result string
  //    ins points to the next occurrence of rep in orig
  //    orig points to the remainder of orig after "end of rep"
  while (count--) {
    ins = strstr(orig, rep);
    len_front = (int)((lbm_uint)ins - (lbm_uint)orig);
    tmp = strncpy(tmp, orig, (size_t)len_front) + len_front;
    tmp = strcpy(tmp, with) + len_with;
    orig += len_front + len_rep; // move to next "end of rep"
  }
  strcpy(tmp, orig);

  return lbm_res;
}

static lbm_value ext_str_to_lower(lbm_value *args, lbm_uint argn) {
  if (argn != 1) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *orig = lbm_dec_str(args[0]);
  if (!orig) {
    return lbm_enc_sym(SYM_TERROR);
  }

  int len = (int)strlen(orig);
  lbm_value lbm_res;
  if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, (lbm_uint)len + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
    for (int i = 0;i < len;i++) {
      ((char*)(arr->data))[i] = (char)tolower(orig[i]);
    }
    ((char*)(arr->data))[len] = '\0';
    return lbm_res;
  } else {
    return lbm_enc_sym(SYM_MERROR);
  }
}

static lbm_value ext_str_to_upper(lbm_value *args, lbm_uint argn) {
  if (argn != 1) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *orig = lbm_dec_str(args[0]);
  if (!orig) {
    return lbm_enc_sym(SYM_TERROR);
  }

  int len = (int)strlen(orig);
  lbm_value lbm_res;
  if (lbm_create_array(&lbm_res, LBM_TYPE_CHAR, (lbm_uint)len + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
    for (int i = 0;i < len;i++) {
      ((char*)(arr->data))[i] = (char)toupper(orig[i]);
    }
    ((char*)(arr->data))[len] = '\0';
    return lbm_res;
  } else {
    return lbm_enc_sym(SYM_MERROR);
  }
}

static lbm_value ext_str_cmp(lbm_value *args, lbm_uint argn) {
  if (argn != 2) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *str1 = lbm_dec_str(args[0]);
  if (!str1) {
    return lbm_enc_sym(SYM_EERROR);
  }

  char *str2 = lbm_dec_str(args[1]);
  if (!str2) {
    return lbm_enc_sym(SYM_EERROR);
  }

  return lbm_enc_i(strcmp(str1, str2));
}

bool lbm_string_extensions_init(void) {

  bool res = true;
  res = res && lbm_add_extension("str-from-n", ext_str_from_n);
  res = res && lbm_add_extension("str-merge", ext_str_merge);
  res = res && lbm_add_extension("str-to-i", ext_str_to_i);
  res = res && lbm_add_extension("str-to-f", ext_str_to_f);
  res = res && lbm_add_extension("str-part", ext_str_part);
  res = res && lbm_add_extension("str-split", ext_str_split);
  res = res && lbm_add_extension("str-replace", ext_str_replace);
  res = res && lbm_add_extension("str-to-lower", ext_str_to_lower);
  res = res && lbm_add_extension("str-to-upper", ext_str_to_upper);
  res = res && lbm_add_extension("str-cmp", ext_str_cmp);
  return res;
}
