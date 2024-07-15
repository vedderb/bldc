/*
    Copyright 2022, 2023 Joel Svensson        svenssonjoel@yahoo.se
    Copyright 2022, 2023 Benjamin Vedder
    Copyright 2024       Rasmus Söderhielm    rasmus.soderhielm@gmail.com

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
#include "eval_cps.h"
#include "print.h"

#include <ctype.h>

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

static char print_val_buffer[256];

static lbm_uint sym_left;
static lbm_uint sym_right;

static size_t strlen_max(const char *s, size_t maxlen) {
  size_t i;
  for (i = 0; i < maxlen; i ++) {
    if (s[i] == 0) break;
  }
  return i;
}

static lbm_value ext_str_from_n(lbm_value *args, lbm_uint argn) {
  if (argn != 1 && argn != 2) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }
  if (!lbm_is_number(args[0])) {
    return ENC_SYM_TERROR;
  }

  if (argn == 2 && !lbm_is_array_r(args[1])) {
    return ENC_SYM_TERROR;
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
      format = "%g";
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
  if (lbm_create_array(&res, len + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
    memcpy(arr->data, buffer, len);
    ((char*)(arr->data))[len] = '\0';
    return res;
  } else {
    return ENC_SYM_MERROR;
  }
}

// signature: (str-join strings [delim]) -> str
static lbm_value ext_str_join(lbm_value *args, lbm_uint argn) {
  // This function does not check that the string arguments contain any
  // terminating null bytes.
  
  if (argn != 1 && argn != 2) {
    lbm_set_error_reason((char *)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  size_t str_len   = 0;
  size_t str_count = 0;
  if (!lbm_is_list(args[0])) {
    lbm_set_error_reason((char *)lbm_error_str_incorrect_arg);
    lbm_set_error_suspect(args[0]);
    return ENC_SYM_TERROR;
  }
  for (lbm_value current = args[0]; lbm_is_cons(current); current = lbm_cdr(current)) {
    char *str = lbm_dec_str(lbm_car(current));
    if (!str) {
      lbm_set_error_reason((char *)lbm_error_str_incorrect_arg);
      lbm_set_error_suspect(args[0]);
      return ENC_SYM_TERROR;
    }

    str_len   += (strlen(str));
    str_count += 1;
  }
  
  const char *delim = "";
  if (argn >= 2) {
    delim = lbm_dec_str(args[1]);
    if (!delim) {
      lbm_set_error_reason((char *)lbm_error_str_incorrect_arg);
      lbm_set_error_suspect(args[1]);
      return ENC_SYM_TERROR;
    }
  }

  size_t delim_len = strlen(delim);
  if (str_count > 0) {
    str_len += (str_count - 1) * delim_len;
  }
  
  lbm_value result;
  if (!lbm_create_array(&result, str_len + 1)) {
    return ENC_SYM_MERROR;
  }
  char *result_str = lbm_dec_str(result);

  size_t i      = 0;
  size_t offset = 0;
  for (lbm_value current = args[0]; lbm_is_cons(current); current = lbm_cdr(current)) {
    const char *str = lbm_dec_str(lbm_car(current));
    size_t len      = strlen(str);

    memcpy(result_str + offset, str, len);
    offset += len;

    if (i != str_count - 1) {
      memcpy(result_str + offset, delim, delim_len);
      offset += delim_len;
    }
    i++;
  }

  result_str[str_len] = '\0';

  return result;
}

static lbm_value ext_str_to_i(lbm_value *args, lbm_uint argn) {
  if (argn != 1 && argn != 2) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  char *str = lbm_dec_str(args[0]);
  if (!str) {
    return ENC_SYM_TERROR;
  }

  int base = 0;
  if (argn == 2) {
    if (!lbm_is_number(args[1])) {
      return ENC_SYM_TERROR;
    }

    base = (int)lbm_dec_as_u32(args[1]);
  }

  return lbm_enc_i32((int32_t)strtol(str, NULL, base));
}

static lbm_value ext_str_to_f(lbm_value *args, lbm_uint argn) {
  if (argn != 1) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  char *str = lbm_dec_str(args[0]);
  if (!str) {
    return ENC_SYM_TERROR;
  }

  return lbm_enc_float(strtof(str, NULL));
}

static lbm_value ext_str_part(lbm_value *args, lbm_uint argn) {
  if ((argn != 2 && argn != 3) || !lbm_is_number(args[1])) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_TERROR;
  }

  char *str = lbm_dec_str(args[0]);
  if (!str) {
    return ENC_SYM_TERROR;
  }

  uint32_t len = (uint32_t)strlen(str);

  uint32_t start = lbm_dec_as_u32(args[1]);

  if (start >= len) {
    return ENC_SYM_EERROR;
  }

  uint32_t n = len - start;
  if (argn == 3) {
    if (!lbm_is_number(args[2])) {
      return ENC_SYM_TERROR;
    }

    n = MIN(lbm_dec_as_u32(args[2]), n);
  }

  lbm_value res;
  if (lbm_create_array(&res, n + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
    memcpy(arr->data, str + start, n);
    ((char*)(arr->data))[n] = '\0';
    return res;
  } else {
    return ENC_SYM_MERROR;
  }
}

static lbm_value ext_str_split(lbm_value *args, lbm_uint argn) {
  if (argn != 2) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  char *str = lbm_dec_str(args[0]);
  if (!str) {
    return ENC_SYM_TERROR;
  }

  char *split = lbm_dec_str(args[1]);
  int step = 0;
  if (!split) {
    if (lbm_is_number(args[1])) {
      step = MAX(lbm_dec_as_i32(args[1]), 1);
      lbm_value res = ENC_SYM_NIL;
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
        if (lbm_create_array(&tok, (lbm_uint)step_now + 1)) {
          lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(tok);
          memcpy(arr->data, str + ind_now, (unsigned int)step_now);
          ((char*)(arr->data))[step_now] = '\0';
          res = lbm_cons(tok, res);
        } else {
          return ENC_SYM_MERROR;
        }
      }
      return res;
    } else {
      return ENC_SYM_TERROR;
    }
  } else {
     lbm_value res = ENC_SYM_NIL;
    const char *s = str;
    while (*(s += strspn(s, split)) != '\0') {
      size_t len = strcspn(s, split);

      lbm_value tok;
      if (lbm_create_array(&tok, len + 1)) {
        lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(tok);
        memcpy(arr->data, s, len);
        ((char*)(arr->data))[len] = '\0';
        res = lbm_cons(tok, res);
      } else {
        return ENC_SYM_MERROR;
      }
      s += len;
    }
    return lbm_list_destructive_reverse(res);
  }
}

// Todo: Clean this up for 64bit
static lbm_value ext_str_replace(lbm_value *args, lbm_uint argn) {
  if (argn != 2 && argn != 3) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  char *orig = lbm_dec_str(args[0]);
  if (!orig) {
    return ENC_SYM_TERROR;
  }

  char *rep = lbm_dec_str(args[1]);
  if (!rep) {
    return ENC_SYM_TERROR;
  }

  char *with = "";
  if (argn == 3) {
    with = lbm_dec_str(args[2]);
    if (!with) {
      return ENC_SYM_TERROR;
    }
  }

  // See https://stackoverflow.com/questions/779875/what-function-is-to-replace-a-substring-from-a-string-in-c
  //char *result; // the return string
  char *ins;    // the next insert point
  char *tmp;    // varies
  size_t len_rep;  // length of rep (the string to remove)
  size_t len_with; // length of with (the string to replace rep with)
  size_t len_front; // distance between rep and end of last rep
  int count;    // number of replacements

  len_rep = strlen(rep);
  if (len_rep == 0) {
    return args[0]; // empty rep causes infinite loop during count
  }

  len_with = strlen(with);

  // count the number of replacements needed
  ins = orig;
  for (count = 0; (tmp = strstr(ins, rep)); ++count) {
    ins = tmp + len_rep;
  }

  size_t len_res = strlen(orig) + (len_with - len_rep) * (unsigned int)count + 1;
  lbm_value lbm_res;
  if (lbm_create_array(&lbm_res, len_res)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
    tmp = (char*)arr->data;
  } else {
    return ENC_SYM_MERROR;
  }

  // first time through the loop, all the variable are set correctly
  // from here on,
  //    tmp points to the end of the result string
  //    ins points to the next occurrence of rep in orig
  //    orig points to the remainder of orig after "end of rep"
  while (count--) {
    ins = strstr(orig, rep);
    len_front = (size_t)ins - (size_t)orig;
    tmp = strncpy(tmp, orig, len_front) + len_front;
    tmp = strcpy(tmp, with) + len_with;
    orig += len_front + len_rep; // move to next "end of rep"
  }
  strcpy(tmp, orig);

  return lbm_res;
}

static lbm_value ext_str_to_lower(lbm_value *args, lbm_uint argn) {
  if (argn != 1) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  char *orig = lbm_dec_str(args[0]);
  if (!orig) {
    return ENC_SYM_TERROR;
  }

  size_t len = strlen(orig);
  lbm_value lbm_res;
  if (lbm_create_array(&lbm_res, len + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
    for (unsigned int i = 0;i < len;i++) {
      ((char*)(arr->data))[i] = (char)tolower(orig[i]);
    }
    ((char*)(arr->data))[len] = '\0';
    return lbm_res;
  } else {
    return ENC_SYM_MERROR;
  }
}

static lbm_value ext_str_to_upper(lbm_value *args, lbm_uint argn) {
  if (argn != 1) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  char *orig = lbm_dec_str(args[0]);
  if (!orig) {
    return ENC_SYM_TERROR;
  }

  int len = (int)strlen(orig);
  lbm_value lbm_res;
  if (lbm_create_array(&lbm_res, (lbm_uint)len + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
    for (int i = 0;i < len;i++) {
      ((char*)(arr->data))[i] = (char)toupper(orig[i]);
    }
    ((char*)(arr->data))[len] = '\0';
    return lbm_res;
  } else {
    return ENC_SYM_MERROR;
  }
}

static lbm_value ext_str_cmp(lbm_value *args, lbm_uint argn) {
  if (argn != 2 && argn != 3) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  char *str1 = lbm_dec_str(args[0]);
  if (!str1) {
    return ENC_SYM_TERROR;
  }

  char *str2 = lbm_dec_str(args[1]);
  if (!str2) {
    return ENC_SYM_TERROR;
  }

  int n = -1;
  if (argn == 3) {
    if (!lbm_is_number(args[2])) {
      return ENC_SYM_EERROR;
    }

    n = lbm_dec_as_i32(args[2]);
  }

  if (n > 0) {
    return lbm_enc_i(strncmp(str1, str2, (unsigned int)n));
  } else {
    return lbm_enc_i(strcmp(str1, str2));
  }
}



// TODO: This is very similar to ext-print. Maybe they can share code.
static lbm_value to_str(char *delimiter, lbm_value *args, lbm_uint argn) {
  const int str_len = 300;
  char *str = lbm_malloc((lbm_uint)str_len);
  if (!str) {
    return ENC_SYM_MERROR;
  }

  int str_ofs = 0;

  for (lbm_uint i = 0; i < argn; i ++) {
    lbm_value t = args[i];
    int max = str_len - str_ofs - 1;

    char *arr_str;
    int chars = 0;

    if (lbm_value_is_printable_string(t, &arr_str)) {
      if (str_ofs == 0) {
        chars = snprintf(str + str_ofs, (unsigned int)max, "%s", arr_str);
      } else {
        chars = snprintf(str + str_ofs, (unsigned int)max, "%s%s", delimiter, arr_str);
      }
    } else {
      lbm_print_value(print_val_buffer, 256, t);
      if (str_ofs == 0) {
        chars = snprintf(str + str_ofs, (unsigned int)max, "%s", print_val_buffer);
      } else {
        chars = snprintf(str + str_ofs, (unsigned int)max, "%s%s", delimiter, print_val_buffer);
      }
    }
    if (chars >= max) {
      str_ofs += max;
    } else {
      str_ofs += chars;
      }
  }

  lbm_value res;
  if (lbm_create_array(&res, (lbm_uint)str_ofs + 1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
    strncpy((char*)arr->data, str, (unsigned int)str_ofs + 1);
    lbm_free(str);
    return res;
  } else {
    lbm_free(str);
    return ENC_SYM_MERROR;
  }
}

static lbm_value ext_to_str(lbm_value *args, lbm_uint argn) {
  return to_str(" ", args, argn);
}

static lbm_value ext_to_str_delim(lbm_value *args, lbm_uint argn) {
  if (argn < 1) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  char *delim = lbm_dec_str(args[0]);
  if (!delim) {
    return ENC_SYM_TERROR;
  }

  return to_str(delim, args + 1, argn - 1);
}

static lbm_value ext_str_len(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN(1);

  char *str = lbm_dec_str(args[0]);
  if (!str) {
    return ENC_SYM_TERROR;
  }

  lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);

  return lbm_enc_i((int)strlen_max(str, array->size));
}

static lbm_value ext_str_replicate(lbm_value *args, lbm_uint argn) {
  if (argn != 2) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }

  lbm_value res = ENC_SYM_TERROR;

  if (lbm_is_number(args[0]) &&
      lbm_is_number(args[1])) {
    uint32_t len = lbm_dec_as_u32(args[0]);
    uint8_t c = lbm_dec_as_char(args[1]);

    lbm_value lbm_res;
    if (lbm_create_array(&lbm_res, len + 1)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(lbm_res);
      for (unsigned int i = 0;i < len;i++) {
        ((char*)(arr->data))[i] = (char)c;
      }
      ((char*)(arr->data))[len] = '\0';
      res = lbm_res;
    } else {
      res = ENC_SYM_MERROR;
    }
  }
  return res;
}

// signature: (str-find str:byte-array substr [start:int] [occurrence:int] [dir]) -> int
// where
//   seq = string|(..string)
//   dir = 'left|'right
static lbm_value ext_str_find(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || 5 < argn) {
    lbm_set_error_reason((char *)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }
  if (!lbm_is_array_r(args[0])) {
    lbm_set_error_suspect(args[0]);
    lbm_set_error_reason((char *)lbm_error_str_incorrect_arg);
    return ENC_SYM_TERROR;
  }

  lbm_array_header_t *str_header = (lbm_array_header_t *)lbm_car(args[0]);
  const char *str   = (const char *)str_header->data;
  lbm_int str_size = (lbm_int)str_header->size;

  // Guaranteed to be list containing strings.
  lbm_value substrings;
  lbm_int min_substr_len = LBM_INT_MAX;
  if (lbm_is_array_r(args[1])) {
    substrings = lbm_cons(args[1], ENC_SYM_NIL);
    if (substrings == ENC_SYM_MERROR) {
      return ENC_SYM_MERROR;
    }
    lbm_array_header_t *header =
        (lbm_array_header_t *)lbm_car(args[1]);
    if (!header) {
      // Should not be possible
      return ENC_SYM_FATAL_ERROR;
    }

    lbm_int len = (lbm_int)header->size - 1;
    if (len < 0) {
      // substr is zero length array
      return lbm_enc_i(-1);
    }
    min_substr_len = len;
  } else if (lbm_is_list(args[1])) {
    for (lbm_value current = args[1]; lbm_is_cons(current); current = lbm_cdr(current)) {
      if (!lbm_is_array_r(lbm_car(current))) {
        lbm_set_error_suspect(args[1]);
        lbm_set_error_reason((char *)lbm_error_str_incorrect_arg);
        return ENC_SYM_TERROR;
      }

      lbm_array_header_t *header =
          (lbm_array_header_t *)lbm_car(lbm_car(current));

      lbm_int len = (lbm_int)header->size - 1;
      if (len < 0) {
        // substr is zero length array
        continue;
      }
      if (len < min_substr_len) {
        min_substr_len = len;
      }
    }
    substrings = args[1];
  } else {
    lbm_set_error_suspect(args[1]);
    lbm_set_error_reason((char *)lbm_error_str_incorrect_arg);
    return ENC_SYM_TERROR;
  }

  bool to_right    = true;
  lbm_uint dir_index = 4;
  if (argn >= 3 && lbm_is_symbol(args[argn - 1])) {
    dir_index       = argn - 1;
    lbm_uint symbol = lbm_dec_sym(args[dir_index]);

    if (symbol == sym_left) {
      to_right = false;
    } else if (symbol != sym_right) {
      lbm_set_error_suspect(args[dir_index]);
      lbm_set_error_reason((char *)lbm_error_str_incorrect_arg);
      return ENC_SYM_TERROR;
    }
  }

  lbm_int start = to_right ? 0 : str_size - min_substr_len;
  if (argn >= 3 && dir_index != 2) {
    if (!lbm_is_number(args[2])) {
      lbm_set_error_reason((char *)lbm_error_str_no_number);
      lbm_set_error_suspect(args[2]);
      return ENC_SYM_TERROR;
    }

    start = lbm_dec_as_int(args[2]);
  }
  
  uint32_t occurrence = 0;
  if (argn >= 4 && dir_index != 3) {
    if (!lbm_is_number(args[3])) {
      lbm_set_error_reason((char *)lbm_error_str_no_number);
      lbm_set_error_suspect(args[3]);
      return ENC_SYM_TERROR;
    }

    occurrence = lbm_dec_as_u32(args[3]);
  }

  if (start < 0) {
    // start: -1 starts the search at the character index before the final null
    // byte index.
    start = str_size - 1 + start;
  }

  if (!to_right && (start > str_size - min_substr_len)) {
    start = str_size - min_substr_len;
  }
  else if (to_right && (start < 0)) {
    start = 0;
  }

  lbm_int dir = to_right ? 1 : -1;
  for (lbm_int i = start; to_right ? (i <= str_size - min_substr_len) : (i >= 0); i += dir) {
    for (lbm_value current = substrings; lbm_is_cons(current); current = lbm_cdr(current)) {
      lbm_array_header_t *header = (lbm_array_header_t *)lbm_car(lbm_car(current));
      lbm_int substr_len         = (lbm_int)header->size - 1;
      const char *substr         = (const char *)header->data;

      if (
        i > str_size - substr_len // substr length runs over str end.
        || substr_len < 0 // substr was zero bytes in size
      ) {
        continue;
      }
      
      if (memcmp(&str[i], substr, (size_t)substr_len) == 0) {
        if (occurrence == 0) {
          return lbm_enc_i(i);
        }
        occurrence -= 1;
      }
    }
  }

  return lbm_enc_i(-1);
}

bool lbm_string_extensions_init(void) {

  bool res = true;
  
  res = res && lbm_add_symbol_const("left", &sym_left);
  res = res && lbm_add_symbol_const("right", &sym_right);
  
  res = res && lbm_add_extension("str-from-n", ext_str_from_n);
  res = res && lbm_add_extension("str-join", ext_str_join);
  res = res && lbm_add_extension("str-to-i", ext_str_to_i);
  res = res && lbm_add_extension("str-to-f", ext_str_to_f);
  res = res && lbm_add_extension("str-part", ext_str_part);
  res = res && lbm_add_extension("str-split", ext_str_split);
  res = res && lbm_add_extension("str-replace", ext_str_replace);
  res = res && lbm_add_extension("str-to-lower", ext_str_to_lower);
  res = res && lbm_add_extension("str-to-upper", ext_str_to_upper);
  res = res && lbm_add_extension("str-cmp", ext_str_cmp);
  res = res && lbm_add_extension("to-str", ext_to_str);
  res = res && lbm_add_extension("to-str-delim", ext_to_str_delim);
  res = res && lbm_add_extension("str-len", ext_str_len);
  res = res && lbm_add_extension("str-replicate", ext_str_replicate);
  res = res && lbm_add_extension("str-find", ext_str_find);

  return res;
}
