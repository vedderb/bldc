/*
    Copyright 2026 Joel Svensson    svenssonjoel@yahoo.se

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

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include <emscripten.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>

#include "lispbm.h"
#include "lbm_flat_value.h"
#include "lbm_image.h"
#include "extensions/array_extensions.h"
#include "extensions/display_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/runtime_extensions.h"
#include "extensions/random_extensions.h"
#include "extensions/set_extensions.h"
#include "extensions/lbm_dyn_lib.h"
#include "extensions/crypto_extensions.h"
#include "extensions/dsp_extensions.h"
#include "extensions/ecc_extensions.h"
#include "extensions/ttf_extensions.h"

#include "lbm_custom_type.h"

#define HEAP_SIZE              (1 << 14)
#define GC_STACK_SIZE          256
#define PRINT_STACK_SIZE       256
#define EXTENSION_STORAGE_SIZE 512
#define LBM_MEMORY_BLOCKS      131072  // ~10K  (* (* 160 16) 4) 10240
                                       //  64K  (* (* 1024 16) 4) 65536
                                       // 256K  (* (* 4096 16) 4) 262144
                                       // 512K  (* (* 8192 16) 4) 655360
                                       //   1M  (* (* 16384 16) 4) 1048576
                                       //   8M  (* (* 131072 16) 4) 8388608
#define LBM_MEMORY_SIZE        LBM_MEMORY_SIZE_BLOCKS_TO_WORDS(LBM_MEMORY_BLOCKS)
#define LBM_BITMAP_SIZE        LBM_MEMORY_BITMAP_SIZE(LBM_MEMORY_BLOCKS)
#define OUTPUT_BUFFER_SIZE     65536
#define CTX_LIST_BUFFER_SIZE   4096
#define STATS_BUFFER_SIZE      512
#define IMAGE_STORAGE_SIZE     (128 * 1024)

static lbm_cons_t      heap[HEAP_SIZE];
static lbm_uint        lbm_memory[LBM_MEMORY_SIZE];
static lbm_uint        lbm_bitmap[LBM_BITMAP_SIZE];
static lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];

static uint32_t image_storage[IMAGE_STORAGE_SIZE / sizeof(uint32_t)];

static bool wasm_image_write(uint32_t w, int32_t ix, bool is_const_heap) {
  (void)is_const_heap;
  image_storage[ix] = w;
  return true;
}

static char output_buffer[OUTPUT_BUFFER_SIZE];
static int  output_pos = 0;

static char ctx_list_buffer[CTX_LIST_BUFFER_SIZE];
static char stats_buffer[STATS_BUFFER_SIZE];

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t         string_tok;

typedef struct reader_s {
  char            *str;
  lbm_cid          cid;
  struct reader_s *next;
} reader_t;

static reader_t *readers = NULL;

static void add_reader(char *str, lbm_cid cid) {
  reader_t *r = (reader_t*)malloc(sizeof(reader_t));
  if (!r) return;
  r->str  = str;
  r->cid  = cid;
  r->next = readers;
  readers = r;
}

static void drop_reader(lbm_cid cid) {
  reader_t *prev = NULL;
  reader_t *curr = readers;
  while (curr) {
    if (curr->cid == cid) {
      if (prev) prev->next = curr->next;
      else      readers    = curr->next;
      free(curr->str);
      free(curr);
      return;
    }
    prev = curr;
    curr = curr->next;
  }
}

// ////////////////////////////////////////////////////////////
//   Callbacks
//

static int print_callback(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int written = vsnprintf(output_buffer + output_pos,
                          OUTPUT_BUFFER_SIZE - output_pos - 1,
                          fmt, args);
  va_end(args);
  if (written > 0) {
    output_pos += written;
    if (output_pos >= OUTPUT_BUFFER_SIZE - 1)
      output_pos = OUTPUT_BUFFER_SIZE - 1;
    output_buffer[output_pos] = '\0';
  }
  return written;
}

static void critical_callback(void) {
  print_callback("CRITICAL ERROR\n");
}

// ////////////////////////////////////////////////////////////
// Extensions
//

static bool dynamic_loader(const char *sym, const char **code) {
  return lbm_dyn_lib_find(sym, code);
}

// Systime in whole number of ms.
// The type of emscripted_get_now() is a double but
// browsers may provide only 1ms resolution (a security measure).
static lbm_value ext_systime(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return lbm_enc_u32((uint32_t)emscripten_get_now());
}

static lbm_value ext_secs_since(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  uint32_t t0   = lbm_dec_as_u32(args[0]);
  uint32_t now  = (uint32_t)emscripten_get_now();
  uint32_t diff = now - t0;
  return lbm_enc_float((float)diff / 1000.0f);
}

static char print_prefix[256] = "";

static lbm_value ext_set_print_prefix(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  const char *str = lbm_dec_str(args[0]);
  if (!str) return ENC_SYM_TERROR;
  strncpy(print_prefix, str, sizeof(print_prefix) - 1);
  print_prefix[sizeof(print_prefix) - 1] = '\0';
  return ENC_SYM_TRUE;
}

static lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  char buf[256];
  if (print_prefix[0]) print_callback("%s", print_prefix);
  for (lbm_uint i = 0; i < argn; i++) {
    lbm_print_value(buf, sizeof(buf), args[i]);
    print_callback("%s", buf);
  }
  print_callback("\n");
  return ENC_SYM_TRUE;
}

static lbm_value ext_puts(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  const char *str = lbm_dec_str(args[0]);
  if (!str) return ENC_SYM_TERROR;
  if (print_prefix[0]) print_callback("%s", print_prefix);
  print_callback("%s\n", str);
  return ENC_SYM_TRUE;
}

/* EM_JS(void, js_plot_slot, (int slot, int nbytes, const char *title), { */
/*   if (typeof window.createPlotTab === 'function') { */
/*     window.createPlotTab(slot, nbytes, UTF8ToString(title)); */
/*   } */
/* }); */

EM_JS(int, js_plot_buf, (int tab_id, uint8_t *buffer, int nbytes, const char *title), {
  if (typeof window.addPlotToTab !== 'function') return -1;
  return window.addPlotToTab(tab_id, buffer, nbytes, UTF8ToString(title));
});

EM_JS(int, js_plot_bufs, (int tab_id, const char *bufs_json, const char *title), {
  if (typeof window.addMultiPlotToTab !== 'function') return -1;
  return window.addMultiPlotToTab(tab_id, UTF8ToString(bufs_json), UTF8ToString(title));
});

EM_JS(int, js_plot_xy, (int tab_id, uint8_t *xbuf, int xbytes, uint8_t *ybuf, int ybytes, const char *title), {
  if (typeof window.addXYPlotToTab !== 'function') return -1;
  return window.addXYPlotToTab(tab_id, xbuf, xbytes, ybuf, ybytes, UTF8ToString(title));
});


// Count how many open editor tabs have the given filename.
EM_JS(int, js_count_tab_matches, (const char *filename), {
  if (typeof window.countEditorTabMatches !== 'function') return 0;
  return window.countEditorTabMatches(UTF8ToString(filename));
});

// Check if an editor tab with the given filename is open; return its content.
EM_JS(char*, js_get_tab_content, (const char *filename), {
  if (typeof window.getEditorTabContent !== 'function') return 0;
  const content = window.getEditorTabContent(UTF8ToString(filename));
  if (content === null) return 0;
  const len = lengthBytesUTF8(content) + 1;
  const buf = _malloc(len);
  if (!buf) return 0;
  stringToUTF8(content, buf, len);
  return buf;
});

// Javascript function callable from C.
// defines:
//  char *js_import_lib(const char *filename);
//  which is the C "binding" for the javascript body below.


EM_JS(char*, js_resolve_import_url, (const char *filename), {
  const base = window.currentBaseUrl;
  if (!base) return 0;
  try {
    const resolved = new URL(UTF8ToString(filename), base).href;
    const len = lengthBytesUTF8(resolved) + 1;
    const buf = _malloc(len);
    if (!buf) return 0;
    stringToUTF8(resolved, buf, len);
    return buf;
  } catch(e) {
    return 0;
  }
});

EM_JS(uint8_t*, js_fetch_url_bytes, (const char *url, int *out_size), {
  const u = UTF8ToString(url);
  if (u.startsWith('memfs://')) {
    const path = u.slice('memfs://'.length);
    try {
      const data = FS.readFile(path);
      const len = data.length;
      const ptr = _malloc(len + 1);
      if (!ptr) return 0;
      HEAPU8.set(data, ptr);
      HEAPU8[ptr + len] = 0;
      setValue(out_size, len + 1, 'i32');
      return ptr;
    } catch(e) { return 0; }
  }
  const xhr = new XMLHttpRequest();
  xhr.open('GET', u, false);
  xhr.overrideMimeType('text/plain; charset=x-user-defined');
  try { xhr.send(null); } catch(e) {
    console.error('import fetch error:', e.message, 'url:', u);
    return 0;
  }
  if (xhr.status !== 200) {
    console.error('import fetch status:', xhr.status, 'url:', u);
    return 0;
  }
  const text = xhr.responseText;
  const len = text.length;
  const ptr = _malloc(len + 1);
  if (!ptr) return 0;
  for (let i = 0; i < len; i++) {
    HEAPU8[ptr + i] = text.charCodeAt(i) & 0xFF;
  }
  HEAPU8[ptr + len] = 0;
  setValue(out_size, len + 1, 'i32');
  return ptr;
});

// (wasm-add-plot tab-id buf "Title") -> plot-id
static lbm_value ext_wasm_add_plot(lbm_value *args, lbm_uint argn) {
  if (argn != 3 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]) || !lbm_is_array_r(args[2]))
    return ENC_SYM_TERROR;
  int tab_id = lbm_dec_as_i32(args[0]);
  const char *title = lbm_dec_str(args[2]);
  lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(args[1]);
  int pid = js_plot_buf(tab_id, (uint8_t*)array->data, (int)array->size, title);
  return lbm_enc_i(pid);
}

// (wasm-add-plot-multi tab-id '(buf1 buf2 ...) "Title") -> plot-id
static lbm_value ext_wasm_add_plot_multi(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_cons(args[1])) return ENC_SYM_TERROR;
  int tab_id = lbm_dec_as_i32(args[0]);
  char json[512];
  int pos = 0;
  pos += snprintf(json + pos, (int)sizeof(json) - pos, "[");
  lbm_value lst = args[1];
  int first = 1;
  while (lbm_is_cons(lst)) {
    lbm_value head = lbm_car(lst);
    if (!lbm_is_array_r(head)) return ENC_SYM_TERROR;
    lbm_array_header_t *hdr = (lbm_array_header_t*)lbm_car(head);
    if (!first) pos += snprintf(json + pos, (int)sizeof(json) - pos, ",");
    pos += snprintf(json + pos, (int)sizeof(json) - pos,
                   "{\"ptr\":%u,\"nbytes\":%u}", (unsigned int)(uintptr_t)hdr->data, (unsigned int)hdr->size);
    first = 0;
    lst = lbm_cdr(lst);
  }
  snprintf(json + pos, (int)sizeof(json) - pos, "]");
  const char *title = "";
  if (argn >= 3 && lbm_is_array_r(args[2])) title = lbm_dec_str(args[2]);
  int pid = js_plot_bufs(tab_id, json, title);
  return lbm_enc_i(pid);
}

// (wasm-add-plot-xy tab-id x-buf y-buf "Title") -> plot-id
static lbm_value ext_wasm_add_plot_xy(lbm_value *args, lbm_uint argn) {
  if (argn != 4 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]) ||
      !lbm_is_array_r(args[2]) || !lbm_is_array_r(args[3]))
    return ENC_SYM_TERROR;
  int tab_id = lbm_dec_as_i32(args[0]);
  const char *title = lbm_dec_str(args[3]);
  lbm_array_header_t *xarr = (lbm_array_header_t*)lbm_car(args[1]);
  lbm_array_header_t *yarr = (lbm_array_header_t*)lbm_car(args[2]);
  int pid = js_plot_xy(tab_id, (uint8_t*)xarr->data, (int)xarr->size,
                                (uint8_t*)yarr->data, (int)yarr->size, title);
  return lbm_enc_i(pid);
}

EM_JS(void, js_open_in_tab, (const char *filename, const char *content), {
  if (typeof window.openFileInTab === 'function')
    window.openFileInTab(UTF8ToString(filename), UTF8ToString(content));
});

static char *read_memfs_file(const char *path);

// ////////////////////////////////////////////////////////////
// BMS and Config simulation extensions
//

EM_JS(double, js_get_bms_val, (const char *key), {
  var e = window.bmsState && window.bmsState[UTF8ToString(key)];
  return e ? +e.val : 0;
});

EM_JS(double, js_conf_get_val, (const char *key), {
  var e = window.configState && window.configState[UTF8ToString(key)];
  return e ? +e.val : 0;
});

EM_JS(void, js_conf_set_val, (const char *key, double val), {
  var k = UTF8ToString(key);
  if (window.setConfVal) window.setConfVal(k, val);
  else if (window.configState) {
    if (window.configState[k]) window.configState[k].val = val;
    else window.configState[k] = {val: val, type: 'f64'};
  }
});

static inline const char *sim_state_name_to_js(const char *sn) { return sn; } // used in comments only

// Returns type code: 0=i 1=u 2=i32 3=u32 4=f32 5=f64 6=symbol 7=str 8=list
EM_JS(int, js_get_sim_type, (const char *state_name, const char *key), {
  var sn = UTF8ToString(state_name);
  var obj = sn==='bms' ? window.bmsState : sn==='gnss' ? window.gnssState : window.configState;
  var e = obj && obj[UTF8ToString(key)];
  if (!e || !e.type) return 5;
  var c = {i:0, u:1, i32:2, u32:3, f32:4, f64:5, symbol:6, str:7, list:8};
  return c[e.type] !== undefined ? c[e.type] : 5;
});

EM_JS(char*, js_get_sim_sym, (const char *state_name, const char *key), {
  var sn = UTF8ToString(state_name);
  var obj = sn==='bms' ? window.bmsState : sn==='gnss' ? window.gnssState : window.configState;
  var e = obj && obj[UTF8ToString(key)];
  var s = (e && e.type === 'symbol') ? String(e.val) : '';
  var len = lengthBytesUTF8(s) + 1;
  var buf = _malloc(len);
  stringToUTF8(s, buf, len);
  return buf;
});

EM_JS(char*, js_get_sim_str, (const char *state_name, const char *key), {
  var sn = UTF8ToString(state_name);
  var obj = sn==='bms' ? window.bmsState : sn==='gnss' ? window.gnssState : window.configState;
  var e = obj && obj[UTF8ToString(key)];
  var s = (e && e.type === 'str') ? String(e.val) : '';
  var len = lengthBytesUTF8(s) + 1;
  var buf = _malloc(len);
  stringToUTF8(s, buf, len);
  return buf;
});

EM_JS(int, js_get_list_len, (const char *state_name, const char *key), {
  var sn = UTF8ToString(state_name);
  var obj = sn==='bms' ? window.bmsState : sn==='gnss' ? window.gnssState : window.configState;
  var e = obj && obj[UTF8ToString(key)];
  return (e && e.type === 'list' && Array.isArray(e.val)) ? e.val.length : 0;
});

EM_JS(double, js_get_list_elem_val, (const char *state_name, const char *key, int idx), {
  var sn = UTF8ToString(state_name);
  var obj = sn==='bms' ? window.bmsState : sn==='gnss' ? window.gnssState : window.configState;
  var e = obj && obj[UTF8ToString(key)];
  if (!e || !Array.isArray(e.val) || idx >= e.val.length) return 0;
  return +e.val[idx].val;
});

EM_JS(int, js_get_list_elem_type, (const char *state_name, const char *key, int idx), {
  var sn = UTF8ToString(state_name);
  var obj = sn==='bms' ? window.bmsState : sn==='gnss' ? window.gnssState : window.configState;
  var e = obj && obj[UTF8ToString(key)];
  if (!e || !Array.isArray(e.val) || idx >= e.val.length) return 5;
  var c = {i:0, u:1, i32:2, u32:3, f32:4, f64:5, symbol:6, str:7};
  var t = e.val[idx].type;
  return c[t] !== undefined ? c[t] : 5;
});

EM_JS(char*, js_get_list_elem_sym, (const char *state_name, const char *key, int idx), {
  var sn = UTF8ToString(state_name);
  var obj = sn==='bms' ? window.bmsState : sn==='gnss' ? window.gnssState : window.configState;
  var e = obj && obj[UTF8ToString(key)];
  var s = (e && Array.isArray(e.val) && idx < e.val.length) ? String(e.val[idx].val || '') : '';
  var len = lengthBytesUTF8(s) + 1;
  var buf = _malloc(len);
  stringToUTF8(s, buf, len);
  return buf;
});

EM_JS(double, js_sim_get_val, (const char *state_name, const char *key), {
  var sn = UTF8ToString(state_name);
  var obj = sn==='bms' ? window.bmsState : sn==='gnss' ? window.gnssState : window.configState;
  var e = obj && obj[UTF8ToString(key)];
  return e ? +e.val : 0;
});

EM_JS(void, js_conf_set_str, (const char *key, const char *val), {
  var k = UTF8ToString(key);
  var v = UTF8ToString(val);
  if (window.configState) {
    if (window.configState[k]) window.configState[k].val = v;
    else window.configState[k] = {val: v, type: 'str'};
  }
  if (window.setConfVal) window.setConfVal(k, v);
});

static lbm_value sim_encode(const char *state_name, const char *key, double val) {
  int type = js_get_sim_type(state_name, key);
  switch (type) {
    case 0: return lbm_enc_i((lbm_int)val);
    case 1: return lbm_enc_u((lbm_uint)val);
    case 2: return lbm_enc_i32((int32_t)val);
    case 3: return lbm_enc_u32((uint32_t)val);
    case 4: return lbm_enc_float((float)val);
    case 6: {
      char *sname = js_get_sim_sym(state_name, key);
      lbm_uint sym = 0;
      bool ok = lbm_get_symbol_by_name(sname, &sym);
      if (!ok) ok = lbm_add_symbol(sname, &sym);
      free(sname);
      return ok ? lbm_enc_sym(sym) : ENC_SYM_NIL;
    }
    case 7: {
      char *s = js_get_sim_str(state_name, key);
      lbm_uint len = strlen(s);
      lbm_value result;
      bool ok = lbm_create_array(&result, len + 1);
      if (ok) memcpy(((lbm_array_header_t*)lbm_car(result))->data, s, len + 1);
      free(s);
      return ok ? result : ENC_SYM_MERROR;
    }
    case 8: {
      int len = js_get_list_len(state_name, key);
      lbm_value result = ENC_SYM_NIL;
      for (int i = len - 1; i >= 0; i--) {
        double ev = js_get_list_elem_val(state_name, key, i);
        int    et = js_get_list_elem_type(state_name, key, i);
        lbm_value elem;
        switch (et) {
          case 0: elem = lbm_enc_i((lbm_int)ev);       break;
          case 1: elem = lbm_enc_u((lbm_uint)ev);      break;
          case 2: elem = lbm_enc_i32((int32_t)ev);     break;
          case 3: elem = lbm_enc_u32((uint32_t)ev);    break;
          case 4: elem = lbm_enc_float((float)ev);     break;
          case 6: {
            char *sn = js_get_list_elem_sym(state_name, key, i);
            lbm_uint sym = 0;
            bool ok = lbm_get_symbol_by_name(sn, &sym);
            if (!ok) ok = lbm_add_symbol(sn, &sym);
            free(sn);
            elem = ok ? lbm_enc_sym(sym) : ENC_SYM_NIL;
            break;
          }
          default: elem = lbm_enc_double(ev);          break;
        }
        lbm_value cell = lbm_cons(elem, result);
        if (lbm_is_symbol_merror(cell)) return ENC_SYM_MERROR;
        result = cell;
      }
      return result;
    }
    default: return lbm_enc_double(val);
  }
}

static lbm_value ext_get_bms_val(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_symbol(args[0])) return ENC_SYM_TERROR;
  const char *key = lbm_get_name_by_symbol(lbm_dec_sym(args[0]));
  if (!key) return ENC_SYM_NIL;
  return sim_encode("bms", key, js_get_bms_val(key));
}

static lbm_value ext_conf_get(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_symbol(args[0])) return ENC_SYM_TERROR;
  const char *key = lbm_get_name_by_symbol(lbm_dec_sym(args[0]));
  if (!key) return ENC_SYM_NIL;
  return sim_encode("config", key, js_conf_get_val(key));
}

static lbm_value ext_conf_set(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_symbol(args[0])) return ENC_SYM_TERROR;
  const char *key = lbm_get_name_by_symbol(lbm_dec_sym(args[0]));
  if (!key) return ENC_SYM_NIL;
  if (lbm_is_array_r(args[1])) {
    const char *str = lbm_dec_str(args[1]);
    if (!str) return ENC_SYM_TERROR;
    js_conf_set_str(key, str);
  } else {
    js_conf_set_val(key, lbm_dec_as_double(args[1]));
  }
  return ENC_SYM_TRUE;
}

static lbm_value ext_conf_store(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_TRUE;
}

// GNSS simulation extensions

static lbm_value ext_gnss_lat_lon(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return sim_encode("gnss", "gnss-lat-lon", 0);
}
static lbm_value ext_gnss_height(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return sim_encode("gnss", "gnss-height", js_sim_get_val("gnss", "gnss-height"));
}
static lbm_value ext_gnss_speed(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return sim_encode("gnss", "gnss-speed", js_sim_get_val("gnss", "gnss-speed"));
}
static lbm_value ext_gnss_hdop(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return sim_encode("gnss", "gnss-hdop", js_sim_get_val("gnss", "gnss-hdop"));
}
static lbm_value ext_gnss_date_time(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return sim_encode("gnss", "gnss-date-time", 0);
}
static lbm_value ext_gnss_age(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return sim_encode("gnss", "gnss-age", js_sim_get_val("gnss", "gnss-age"));
}

// EEPROM simulation extensions

EM_JS(double, js_eeprom_read, (int addr), {
  var e = window.eepromState && window.eepromState[addr];
  return e ? +e.val : 0;
});

EM_JS(void, js_eeprom_store, (int addr, double val, int is_float), {
  if (!window.eepromState) window.eepromState = {};
  var type = is_float ? 'f32' : 'i32';
  if (window.eepromState[addr]) {
    window.eepromState[addr].val = val;
    window.eepromState[addr].type = type;
  } else {
    window.eepromState[addr] = { val: val, type: type };
  }
  if (window.eepromRefresh) window.eepromRefresh(addr, val, type);
});

static lbm_value ext_eeprom_read_i(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  return lbm_enc_i32((int32_t)js_eeprom_read(lbm_dec_as_i32(args[0])));
}

static lbm_value ext_eeprom_read_f(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  return lbm_enc_float((float)js_eeprom_read(lbm_dec_as_i32(args[0])));
}

static lbm_value ext_eeprom_store_i(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  js_eeprom_store(lbm_dec_as_i32(args[0]), lbm_dec_as_double(args[1]), 0);
  return ENC_SYM_TRUE;
}

static lbm_value ext_eeprom_store_f(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  js_eeprom_store(lbm_dec_as_i32(args[0]), lbm_dec_as_double(args[1]), 1);
  return ENC_SYM_TRUE;
}

// GPIO simulation extensions
//

EM_JS(void, js_gpio_configure, (const char *pin, const char *mode), {
  if (typeof window.gpioSetMode === 'function')
    window.gpioSetMode(UTF8ToString(pin), UTF8ToString(mode));
});

EM_JS(void, js_gpio_write_js, (const char *pin, int value), {
  if (typeof window.gpioWrite === 'function')
    window.gpioWrite(UTF8ToString(pin), value);
});

EM_JS(int, js_gpio_read_js, (const char *pin), {
  if (typeof window.gpioRead === 'function')
    return window.gpioRead(UTF8ToString(pin));
  return 0;
});

static void gpio_pin_str(lbm_value arg, char *buf, size_t bufsz) {
  if (lbm_is_symbol(arg)) {
    const char *name = lbm_get_name_by_symbol(lbm_dec_sym(arg));
    strncpy(buf, name ? name : "", bufsz - 1);
  } else if (lbm_is_number(arg)) {
    snprintf(buf, bufsz, "%d", lbm_dec_as_i32(arg));
  } else {
    buf[0] = '\0';
  }
  buf[bufsz - 1] = '\0';
}

static lbm_value ext_gpio_configure(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_symbol(args[1])) return ENC_SYM_TERROR;
  char pin[32];
  gpio_pin_str(args[0], pin, sizeof(pin));
  if (!pin[0]) return ENC_SYM_TERROR;
  const char *mode = lbm_get_name_by_symbol(lbm_dec_sym(args[1]));
  if (!mode) return ENC_SYM_TERROR;
  js_gpio_configure(pin, mode);
  return ENC_SYM_TRUE;
}

static lbm_value ext_gpio_write(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  char pin[32];
  gpio_pin_str(args[0], pin, sizeof(pin));
  if (!pin[0]) return ENC_SYM_TERROR;
  js_gpio_write_js(pin, lbm_dec_as_i32(args[1]));
  return ENC_SYM_TRUE;
}

static lbm_value ext_gpio_read(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  char pin[32];
  gpio_pin_str(args[0], pin, sizeof(pin));
  if (!pin[0]) return ENC_SYM_TERROR;
  return lbm_enc_i(js_gpio_read_js(pin));
}

EM_JS(void, js_sim_gpio_write, (const char *pin, int value), {
  if (typeof window.simGpioWrite === 'function')
    window.simGpioWrite(UTF8ToString(pin), value);
});

EM_JS(int, js_sim_gpio_read, (const char *pin), {
  if (typeof window.simGpioRead === 'function')
    return window.simGpioRead(UTF8ToString(pin));
  return 0;
});

EM_JS(void, js_sim_adc_set, (const char *pin, double value), {
  if (typeof window.simAdcSet === 'function')
    window.simAdcSet(UTF8ToString(pin), value);
});

EM_JS(double, js_sim_adc_get, (const char *pin), {
  if (typeof window.simAdcGet === 'function')
    return window.simAdcGet(UTF8ToString(pin));
  return 0.0;
});

static lbm_value ext_sim_gpio_write(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  char pin[32];
  gpio_pin_str(args[0], pin, sizeof(pin));
  if (!pin[0]) return ENC_SYM_TERROR;
  js_sim_gpio_write(pin, lbm_dec_as_i32(args[1]));
  return ENC_SYM_TRUE;
}

static lbm_value ext_sim_gpio_read(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  char pin[32];
  gpio_pin_str(args[0], pin, sizeof(pin));
  if (!pin[0]) return ENC_SYM_TERROR;
  return lbm_enc_i(js_sim_gpio_read(pin));
}

static lbm_value ext_sim_adc_set(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  char pin[32];
  gpio_pin_str(args[0], pin, sizeof(pin));
  if (!pin[0]) return ENC_SYM_TERROR;
  js_sim_adc_set(pin, lbm_dec_as_double(args[1]));
  return ENC_SYM_TRUE;
}

static lbm_value ext_sim_adc_get(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  char pin[32];
  gpio_pin_str(args[0], pin, sizeof(pin));
  if (!pin[0]) return ENC_SYM_TERROR;
  return lbm_enc_double(js_sim_adc_get(pin));
}

static lbm_value ext_image_save(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn; return ENC_SYM_TRUE;
}

static lbm_value ext_color_mix(lbm_value *args, lbm_uint argn) {
  if (argn != 3 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  uint32_t c1 = (uint32_t)lbm_dec_as_u32(args[0]);
  uint32_t c2 = (uint32_t)lbm_dec_as_u32(args[1]);
  float t = lbm_dec_as_float(args[2]);
  if (t < 0.0f) t = 0.0f;
  if (t > 1.0f) t = 1.0f;
  uint8_t r = (uint8_t)((float)((c1 >> 16) & 0xFF) * (1.0f - t) + (float)((c2 >> 16) & 0xFF) * t);
  uint8_t g = (uint8_t)((float)((c1 >>  8) & 0xFF) * (1.0f - t) + (float)((c2 >>  8) & 0xFF) * t);
  uint8_t b = (uint8_t)((float)((c1      ) & 0xFF) * (1.0f - t) + (float)((c2      ) & 0xFF) * t);
  return lbm_enc_u32(((uint32_t)r << 16) | ((uint32_t)g << 8) | (uint32_t)b);
}

static lbm_value ext_color_make(lbm_value *args, lbm_uint argn) {
  if (argn != 3 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  float r = lbm_dec_as_float(args[0]);
  float g = lbm_dec_as_float(args[1]);
  float b = lbm_dec_as_float(args[2]);
  if (r < 0.0f) r = 0.0f; if (r > 1.0f) r = 1.0f;
  if (g < 0.0f) g = 0.0f; if (g > 1.0f) g = 1.0f;
  if (b < 0.0f) b = 0.0f; if (b > 1.0f) b = 1.0f;
  return lbm_enc_u32(((uint32_t)(r * 255.0f) << 16) | ((uint32_t)(g * 255.0f) << 8) | (uint32_t)(b * 255.0f));
}

static lbm_value ext_bits_enc_int(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(4)
  uint32_t initial = lbm_dec_as_u32(args[0]);
  uint32_t offset  = lbm_dec_as_u32(args[1]);
  uint32_t number  = lbm_dec_as_u32(args[2]);
  uint32_t bits    = lbm_dec_as_u32(args[3]);
  initial &= ~((0xFFFFFFFF >> (32 - bits)) << offset);
  initial |= (number << (32 - bits)) >> (32 - bits - offset);
  if (initial > ((1u << 27) - 1)) return lbm_enc_i32((int32_t)initial);
  return lbm_enc_i((int32_t)initial);
}

static lbm_value ext_bits_dec_int(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(3)
  uint32_t val    = lbm_dec_as_u32(args[0]);
  uint32_t offset = lbm_dec_as_u32(args[1]);
  uint32_t bits   = lbm_dec_as_u32(args[2]);
  val >>= offset;
  val &= 0xFFFFFFFF >> (32 - bits);
  if (val > ((1u << 27) - 1)) return lbm_enc_i32((int32_t)val);
  return lbm_enc_i((int32_t)val);
}

static lbm_value ext_get_adc(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  char ch[32];
  gpio_pin_str(args[0], ch, sizeof(ch));
  if (!ch[0]) return ENC_SYM_TERROR;
  return lbm_enc_float((float)js_sim_adc_get(ch));
}

static lbm_value ext_get_adc_decoded(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  char ch[32];
  gpio_pin_str(args[0], ch, sizeof(ch));
  if (!ch[0]) return ENC_SYM_TERROR;
  float v = (float)js_sim_adc_get(ch);
  float norm = v / 3.3f;
  if (norm < 0.0f) norm = 0.0f;
  if (norm > 1.0f) norm = 1.0f;
  return lbm_enc_float(norm);
}


// ////////////////////////////////////////////////////////////
// Filesystem extensions (fs-*)
//

static lbm_value ext_fs_pwd(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  char buf[512];
  if (!getcwd(buf, sizeof(buf))) return ENC_SYM_NIL;
  lbm_uint len = strlen(buf);
  lbm_value result;
  if (!lbm_create_array(&result, len + 1)) return ENC_SYM_MERROR;
  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(result);
  memcpy(arr->data, buf, len + 1);
  return result;
}

static lbm_value ext_fs_cd(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  const char *path = lbm_dec_str(args[0]);
  if (!path) return ENC_SYM_TERROR;
  return chdir(path) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_fs_mkdir(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  const char *path = lbm_dec_str(args[0]);
  if (!path) return ENC_SYM_TERROR;
  return mkdir(path, 0777) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_fs_rm(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  const char *path = lbm_dec_str(args[0]);
  if (!path) return ENC_SYM_TERROR;
  return unlink(path) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_fs_mv(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_array_r(args[0]) || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  const char *src = lbm_dec_str(args[0]);
  const char *dst = lbm_dec_str(args[1]);
  if (!src || !dst) return ENC_SYM_TERROR;
  return rename(src, dst) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_fs_exists(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  const char *path = lbm_dec_str(args[0]);
  if (!path) return ENC_SYM_TERROR;
  struct stat st;
  return stat(path, &st) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

// (fs-stat path) -> (size is-dir)
static lbm_value ext_fs_stat(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  const char *path = lbm_dec_str(args[0]);
  if (!path) return ENC_SYM_TERROR;
  struct stat st;
  if (stat(path, &st) != 0) return ENC_SYM_NIL;
  lbm_value is_dir = S_ISDIR(st.st_mode) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  lbm_value result = lbm_cons(is_dir, ENC_SYM_NIL);
  if (lbm_is_symbol_merror(result)) return ENC_SYM_MERROR;
  result = lbm_cons(lbm_enc_i32((int32_t)st.st_size), result);
  if (lbm_is_symbol_merror(result)) return ENC_SYM_MERROR;
  return result;
}

// (fs-ls) or (fs-ls path) -> list of (name size is-dir)
static lbm_value ext_fs_ls(lbm_value *args, lbm_uint argn) {
  const char *path = ".";
  if (argn == 1 && lbm_is_array_r(args[0])) {
    path = lbm_dec_str(args[0]);
    if (!path) return ENC_SYM_TERROR;
  } else if (argn != 0) {
    return ENC_SYM_TERROR;
  }
  DIR *d = opendir(path);
  if (!d) return ENC_SYM_NIL;
  lbm_value result = ENC_SYM_NIL;
  struct dirent *entry;
  while ((entry = readdir(d)) != NULL) {
    if (entry->d_name[0] == '.') continue;
    lbm_value name;
    if (!lbm_create_array(&name, strlen(entry->d_name) + 1)) continue;
    lbm_array_header_t *hdr = (lbm_array_header_t*)lbm_car(name);
    memcpy(hdr->data, entry->d_name, strlen(entry->d_name) + 1);
    lbm_value cell = lbm_cons(name, result);
    if (lbm_is_symbol_merror(cell)) break;
    result = cell;
  }
  closedir(d);
  return result;
}

// (fs-open path) - read file from MEMFS and open in editor tab
static lbm_value ext_fs_open(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  const char *path = lbm_dec_str(args[0]);
  if (!path) return ENC_SYM_TERROR;
  char *content = read_memfs_file(path);
  if (!content) return ENC_SYM_NIL;
  const char *filename = strrchr(path, '/');
  filename = filename ? filename + 1 : path;
  js_open_in_tab(filename, content);
  free(content);
  return ENC_SYM_TRUE;
}

static char *read_memfs_file(const char *path) {
  FILE *fp = fopen(path, "rb");
  if (!fp) return NULL;
  fseek(fp, 0, SEEK_END);
  long size = ftell(fp);
  rewind(fp);
  if (size <= 0) { fclose(fp); return NULL; }
  char *buf = malloc((size_t)size + 1);
  if (!buf) { fclose(fp); return NULL; }
  fread(buf, 1, (size_t)size, fp);
  buf[size] = '\0';
  fclose(fp);
  return buf;
}

// (import library-file-name sym)
static lbm_value ext_import(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_array_r(args[0]) || !lbm_is_symbol(args[1])) return ENC_SYM_TERROR;
  const char *filename = lbm_dec_str(args[0]);
  if (!filename) return ENC_SYM_TERROR;
  const char *symname = lbm_get_name_by_symbol(lbm_dec_sym(args[1]));
  if (!symname) return ENC_SYM_TERROR;

  if (strncmp(filename, "pkg@://", 7) == 0) {
    print_callback("import: skipping pkg@:// package \"%s\" (not supported in WASM repl)\n", filename);
    lbm_value empty;
    if (!lbm_create_array(&empty, 1)) return ENC_SYM_MERROR;
    ((uint8_t*)((lbm_array_header_t*)lbm_car(empty))->data)[0] = 0;
    lbm_define((char*)symname, empty);
    return empty;
  }

  if (strncmp(filename, "http://", 7) == 0 || strncmp(filename, "https://", 8) == 0) {
    int size = 0;
    uint8_t *data = js_fetch_url_bytes(filename, &size);
    if (!data || size <= 0) {
      print_callback("import: failed to fetch \"%s\"\n", filename);
      return ENC_SYM_NIL;
    }
    lbm_value result;
    if (!lbm_create_array(&result, (lbm_uint)size)) {
      free(data);
      return ENC_SYM_MERROR;
    }
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(result);
    memcpy(arr->data, data, (size_t)size);
    free(data);
    lbm_define((char*)symname, result);
    return result;
  }

  // Relative path with a base URL set — resolve and fetch as URL
  bool is_relative = (strncmp(filename, "http://", 7) != 0 &&
                      strncmp(filename, "https://", 8) != 0 &&
                      filename[0] != '/');
  if (is_relative) {
    char *resolved = js_resolve_import_url(filename);
    if (resolved) {
      int size = 0;
      uint8_t *data = js_fetch_url_bytes(resolved, &size);
      free(resolved);
      if (data && size > 0) {
        lbm_value result;
        if (!lbm_create_array(&result, (lbm_uint)size)) { free(data); return ENC_SYM_MERROR; }
        lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(result);
        memcpy(arr->data, data, (size_t)size);
        free(data);
        lbm_define((char*)symname, result);
        return result;
      }
      print_callback("import: \"%s\" not found at URL, trying tabs and MEMFS\n", filename);
    }
  }

  int matches = js_count_tab_matches(filename);
  if (matches > 1) {
    print_callback("import: %d open tabs named \"%s\", using first match\n", matches, filename);
  }

  char *code = js_get_tab_content(filename);

  if (!code) code = read_memfs_file(filename);
  if (!code) {
    print_callback("import: \"%s\" not found in editor tabs or MEMFS\n", filename);
    return ENC_SYM_NIL;
  }

  lbm_uint len = (lbm_uint)strlen(code);
  lbm_value result;
  if (!lbm_create_array(&result, len + 1)) {
    free(code);
    return ENC_SYM_MERROR;
  }
  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(result);
  memcpy(arr->data, code, len + 1);
  free(code);
  lbm_define((char*)symname, result);
  return result;
}


EM_JS(int, js_create_tab, (const char *title), {
  if (typeof window.createTab !== 'function') return -1;
  return window.createTab(UTF8ToString(title));
});

EM_JS(int, js_add_canvas_to_tab, (int tab_id, int w, int h), {
  if (typeof window.addCanvasToTab !== 'function') return -1;
  return window.addCanvasToTab(tab_id, w, h);
});

EM_JS(int, js_add_button_to_tab, (int tab_id, const char *buttons_json), {
  if (typeof window.addButtonToTab !== 'function') return -1;
  return window.addButtonToTab(tab_id, UTF8ToString(buttons_json));
});

EM_JS(void, js_canvas_put_image, (int canvas_id, uint8_t *rgba, int w, int h, int x, int y), {
  if (typeof window.canvasPutImage === 'function') {
    window.canvasPutImage(canvas_id, rgba, w, h, x, y);
  }
});

EM_JS(void, js_canvas_clear_js, (int canvas_id, uint32_t color), {
  if (typeof window.canvasClear === 'function') {
    window.canvasClear(canvas_id, color);
  }
});

EM_JS(int, js_add_keyboard_control, (int tab_id, const char *label), {
  if (typeof window.addKeyboardControl !== 'function') return -1;
  return window.addKeyboardControl(tab_id, UTF8ToString(label));
});

EM_JS(void, js_keyboard_control_bind, (int kb_id, const char *key, const char *press, const char *release), {
  if (typeof window.keyboardControlBind === 'function')
    window.keyboardControlBind(kb_id, UTF8ToString(key), UTF8ToString(press), UTF8ToString(release));
});

static int active_canvas_id = -1;

// ////////////////////////////////////////////////////////////
// File extensions (backed by Emscripten MEMFS)
//

static const char *lbm_file_handle_desc = "File-Handle";

typedef struct {
  FILE *fp;
} lbm_file_handle_t;

static bool file_handle_destructor(lbm_uint value) {
  lbm_file_handle_t *h = (lbm_file_handle_t *)value;
  if (h->fp) fclose(h->fp);
  return true;
}

static bool is_file_handle(lbm_value arg) {
  if (lbm_is_custom(arg) && ((lbm_uint)lbm_get_custom_descriptor(arg) == (lbm_uint)lbm_file_handle_desc)) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(arg);
    if (h->fp) return true;
  }
  return false;
}

static lbm_value ext_fclose(lbm_value *args, lbm_uint argn) {
  if (argn == 1 && is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    fclose(h->fp);
    h->fp = NULL;
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_fopen(lbm_value *args, lbm_uint argn) {
  if (argn == 2 && lbm_is_array_r(args[0]) && lbm_is_array_r(args[1])) {
    char *filename = lbm_dec_str(args[0]);
    char *mode     = lbm_dec_str(args[1]);
    FILE *fp = fopen(filename, mode);
    if (fp) {
      lbm_file_handle_t *mem = lbm_malloc(sizeof(lbm_file_handle_t));
      if (!mem) { fclose(fp); return ENC_SYM_MERROR; }
      mem->fp = fp;
      lbm_value res;
      lbm_custom_type_create((lbm_uint)mem, file_handle_destructor, lbm_file_handle_desc, &res);
      return res;
    }
    return ENC_SYM_NIL;
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_load_file(lbm_value *args, lbm_uint argn) {
  if (argn == 1 && is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    if (fseek(h->fp, 0, SEEK_END) < 0) return ENC_SYM_EERROR;
    long int size = ftell(h->fp);
    rewind(h->fp);
    if (size <= 0) return ENC_SYM_NIL;
    uint8_t *data = lbm_malloc((size_t)size + 1);
    if (!data) return ENC_SYM_MERROR;
    memset(data, 0, (size_t)size + 1);
    lbm_value val;
    if (!lbm_lift_array(&val, (char*)data, (lbm_uint)size + 1)) { lbm_free(data); return ENC_SYM_MERROR; }
    if (fread(data, 1, (size_t)size, h->fp) > 0) return val;
    lbm_free(data);
    return ENC_SYM_NIL;
  }
  return ENC_SYM_TERROR;
}

static lbm_value sym_seek_set;
static lbm_value sym_seek_cur;
static lbm_value sym_seek_end;

static lbm_value ext_fseek(lbm_value *args, lbm_uint argn) {
  if ((argn == 2 || argn == 3) && is_file_handle(args[0]) && lbm_is_number(args[1])) {
    int whence = SEEK_SET;
    if (argn == 3) {
      if (!lbm_is_symbol(args[2])) return ENC_SYM_TERROR;
      if      (args[2] == sym_seek_set) whence = SEEK_SET;
      else if (args[2] == sym_seek_cur) whence = SEEK_CUR;
      else if (args[2] == sym_seek_end) whence = SEEK_END;
      else return ENC_SYM_TERROR;
    }
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    return (fseek(h->fp, (long)lbm_dec_as_i64(args[1]), whence) == 0) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_ftell(lbm_value *args, lbm_uint argn) {
  if (argn == 1 && is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    return lbm_enc_i64((int64_t)ftell(h->fp));
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_fread_byte(lbm_value *args, lbm_uint argn) {
  if (argn == 1 && is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    char c;
    return (fread(&c, 1, 1, h->fp) == 1) ? lbm_enc_char((uint8_t)c) : ENC_SYM_NIL;
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_fread(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !is_file_handle(args[0])) return ENC_SYM_TERROR;
  lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
  if (!h || !h->fp) return ENC_SYM_EERROR;
  if (lbm_is_number(args[1])) {
    lbm_uint n = (lbm_uint)lbm_dec_as_u32(args[1]);
    lbm_value result;
    if (!lbm_create_array(&result, n)) return ENC_SYM_MERROR;
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(result);
    size_t num = fread(arr->data, 1, n, h->fp);
    if (num == 0) return ENC_SYM_NIL;
    return result;
  }
  if (lbm_is_array_rw(args[1])) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(args[1]);
    size_t num = fread(arr->data, 1, arr->size, h->fp);
    return lbm_enc_u((lbm_uint)num);
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_fwrite(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !is_file_handle(args[0])) return ENC_SYM_TERROR;
  lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
  if (lbm_is_array_r(args[1])) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(args[1]);
    if (!arr) return ENC_SYM_NIL;
    fwrite(arr->data, 1, arr->size, h->fp);
    fflush(h->fp);
    return ENC_SYM_TRUE;
  }
  if (lbm_is_cons(args[1])) {
    lbm_value curr = args[1];
    while (lbm_is_cons(curr)) {
      lbm_value val = lbm_car(curr);
      if (!lbm_is_number(val)) return ENC_SYM_TERROR;
      uint8_t byte = (uint8_t)lbm_dec_as_u32(val);
      fwrite(&byte, 1, 1, h->fp);
      curr = lbm_cdr(curr);
    }
    fflush(h->fp);
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_fwrite_str(lbm_value *args, lbm_uint argn) {
  if (argn == 2 && is_file_handle(args[0]) && lbm_is_array_r(args[1])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(args[1]);
    if (arr) { fwrite(arr->data, 1, strlen((char*)arr->data), h->fp); fflush(h->fp); return ENC_SYM_TRUE; }
    return ENC_SYM_NIL;
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_fwrite_value(lbm_value *args, lbm_uint argn) {
  if (argn == 2 && is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    lbm_set_max_flatten_depth(10000);
    int32_t fv_size = flatten_value_size(args[1], 0);
    if (fv_size <= 0) return ENC_SYM_NIL;
    lbm_flat_value_t fv;
    fv.buf = malloc((uint32_t)fv_size);
    if (!fv.buf) return ENC_SYM_MERROR;
    fv.buf_size = (uint32_t)fv_size;
    fv.buf_pos  = 0;
    lbm_value res = ENC_SYM_NIL;
    if (flatten_value_c(&fv, args[1]) == FLATTEN_VALUE_OK) {
      fwrite(fv.buf, 1, (size_t)fv_size, h->fp);
      fflush(h->fp);
      res = ENC_SYM_TRUE;
    }
    free(fv.buf);
    return res;
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_file_list(lbm_value *args, lbm_uint argn) {
  const char *path = "/";
  if (argn == 1 && lbm_is_array_r(args[0])) {
    path = lbm_dec_str(args[0]);
    if (!path) return ENC_SYM_TERROR;
  } else if (argn != 0) {
    return ENC_SYM_TERROR;
  }
  DIR *d = opendir(path);
  if (!d) return ENC_SYM_NIL;
  lbm_value result = ENC_SYM_NIL;
  struct dirent *entry;
  while ((entry = readdir(d)) != NULL) {
    if (entry->d_name[0] == '.') continue;
    lbm_value name;
    if (!lbm_create_array(&name, strlen(entry->d_name) + 1)) continue;
    lbm_array_header_t *hdr = (lbm_array_header_t*)lbm_car(name);
    memcpy(hdr->data, entry->d_name, strlen(entry->d_name) + 1);
    lbm_value cell = lbm_cons(name, result);
    if (lbm_is_symbol_merror(cell)) break;
    result = cell;
  }
  closedir(d);
  return result;
}

static lbm_value ext_f_readline(lbm_value *args, lbm_uint argn) {
  if (argn == 1 && is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    if (!h || !h->fp) return ENC_SYM_EERROR;
    char buf[1024];
    if (!fgets(buf, sizeof(buf), h->fp)) return ENC_SYM_NIL;
    lbm_uint len = strlen(buf);
    lbm_value result;
    if (!lbm_create_array(&result, len + 1)) return ENC_SYM_MERROR;
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(result);
    memcpy(arr->data, buf, len + 1);
    return result;
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_f_size(lbm_value *args, lbm_uint argn) {
  if (argn == 1 && lbm_is_array_r(args[0])) {
    const char *path = lbm_dec_str(args[0]);
    if (!path) return ENC_SYM_TERROR;
    struct stat st;
    if (stat(path, &st) != 0) return ENC_SYM_NIL;
    return lbm_enc_i32((int32_t)st.st_size);
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_f_sync(lbm_value *args, lbm_uint argn) {
  if (argn == 1 && is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    if (!h || !h->fp) return ENC_SYM_EERROR;
    return (fflush(h->fp) == 0) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return ENC_SYM_TERROR;
}

static lbm_value ext_f_connect(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_TRUE;
}

static lbm_value ext_f_disconnect(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_TRUE;
}

static lbm_value ext_f_fatinfo(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return ENC_SYM_NIL;
}

// ////////////////////////////////////////////////////////////
// wasm-save-file: download a MEMFS file to the user's disk
//

EM_JS(void, js_save_memfs_file, (const char *path, const char *filename), {
  const p = UTF8ToString(path);
  const f = UTF8ToString(filename);
  try {
    const data = FS.readFile(p);
    const blob = new Blob([data], {type: 'application/octet-stream'});
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = f;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  } catch(e) {
    console.error('wasm-save-file:', e.message);
  }
});

// (wasm-save-file "path/in/memfs" "download-name")
// If only one arg, the download name matches the memfs filename.
static lbm_value ext_wasm_save_file(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || argn > 2 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  const char *path = lbm_dec_str(args[0]);
  if (!path) return ENC_SYM_TERROR;
  const char *filename = path;
  if (argn == 2 && lbm_is_array_r(args[1])) {
    filename = lbm_dec_str(args[1]);
    if (!filename) return ENC_SYM_TERROR;
  }
  // Strip leading path from filename for the download name if not overridden
  const char *base = strrchr(filename, '/');
  if (base) filename = base + 1;
  js_save_memfs_file(path, filename);
  return ENC_SYM_TRUE;
}


static void json_esc(const char *src, char *dst, size_t cap) {
  size_t j = 0;
  for (size_t i = 0; src[i] && j + 4 < cap; i++) {
    char c = src[i];
    if      (c == '"')  { dst[j++] = '\\'; dst[j++] = '"'; }
    else if (c == '\\') { dst[j++] = '\\'; dst[j++] = '\\'; }
    else if (c == '\n') { dst[j++] = '\\'; dst[j++] = 'n'; }
    else if (c == '\r') { dst[j++] = '\\'; dst[j++] = 'r'; }
    else                { dst[j++] = c; }
  }
  dst[j] = '\0';
}

// (wasm-add-button tab-id '(("label" "press-code" ["release-code"]) ...)) -> button-group-id
static lbm_value ext_wasm_add_button(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[0]) || !lbm_is_cons(args[1]))
    return ENC_SYM_TERROR;
  int tab_id = lbm_dec_as_i32(args[0]);

  char json[4096];
  int pos = snprintf(json, sizeof(json), "[");
  int first = 1;
  lbm_value lst = args[1];
  while (lbm_is_cons(lst)) {
    lbm_value spec = lbm_car(lst);
    if (!lbm_is_cons(spec)) return ENC_SYM_TERROR;

    lbm_value text_v  = lbm_car(spec); spec = lbm_cdr(spec);
    lbm_value press_v = lbm_is_cons(spec) ? lbm_car(spec) : ENC_SYM_NIL;
    spec              = lbm_is_cons(spec) ? lbm_cdr(spec) : ENC_SYM_NIL;
    lbm_value rel_v   = lbm_is_cons(spec) ? lbm_car(spec) : ENC_SYM_NIL;

    if (!lbm_is_array_r(text_v) || !lbm_is_array_r(press_v)) return ENC_SYM_TERROR;
    const char *text    = lbm_dec_str(text_v);  if (!text)  return ENC_SYM_TERROR;
    const char *press   = lbm_dec_str(press_v); if (!press) return ENC_SYM_TERROR;
    const char *release = lbm_is_array_r(rel_v) ? lbm_dec_str(rel_v) : "";
    if (!release) release = "";

    char et[256], ep[1024], er[1024];
    json_esc(text, et, sizeof(et));
    json_esc(press, ep, sizeof(ep));
    json_esc(release, er, sizeof(er));

    if (!first) pos += snprintf(json + pos, sizeof(json) - pos, ",");
    pos += snprintf(json + pos, sizeof(json) - pos,
                   "{\"text\":\"%s\",\"press\":\"%s\",\"release\":\"%s\"}",
                   et, ep, er);
    first = 0;
    lst = lbm_cdr(lst);
  }
  snprintf(json + pos, sizeof(json) - pos, "]");
  return lbm_enc_i(js_add_button_to_tab(tab_id, json));
}

// (wasm-add-keyboard-control tab-id "Label") -> kb-id
static lbm_value ext_wasm_add_keyboard_control(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  int tab_id = lbm_dec_as_i32(args[0]);
  const char *label = lbm_dec_str(args[1]);
  if (!label) return ENC_SYM_TERROR;
  return lbm_enc_i(js_add_keyboard_control(tab_id, label));
}

// (wasm-keyboard-control-bind kb-id "key" "press-code" "release-code") -> t
static lbm_value ext_wasm_keyboard_control_bind(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]) || !lbm_is_array_r(args[2]))
    return ENC_SYM_TERROR;
  int kb_id = lbm_dec_as_i32(args[0]);
  const char *key     = lbm_dec_str(args[1]); if (!key)   return ENC_SYM_TERROR;
  const char *press   = lbm_dec_str(args[2]); if (!press) return ENC_SYM_TERROR;
  const char *release = (argn >= 4 && lbm_is_array_r(args[3])) ? lbm_dec_str(args[3]) : "";
  if (!release) release = "";
  js_keyboard_control_bind(kb_id, key, press, release);
  return ENC_SYM_TRUE;
}

// (wasm-create-tab "Title") -> tab-id
static lbm_value ext_wasm_create_tab(lbm_value *args, lbm_uint argn) {
  const char *title = "";
  if (argn >= 1 && lbm_is_array_r(args[0])) {
    title = lbm_dec_str(args[0]);
    if (!title) title = "";
  }
  return lbm_enc_i(js_create_tab(title));
}

// (wasm-add-canvas tab-id w h) -> canvas-id, sets as active render target
static lbm_value ext_wasm_add_canvas(lbm_value *args, lbm_uint argn) {
  if (argn != 3 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  int tab_id = lbm_dec_as_i32(args[0]);
  int w      = lbm_dec_as_i32(args[1]);
  int h      = lbm_dec_as_i32(args[2]);
  int cid    = js_add_canvas_to_tab(tab_id, w, h);
  if (cid >= 0) active_canvas_id = cid;
  return lbm_enc_i(cid);
}

// (wasm-set-canvas canvas-id)
static lbm_value ext_wasm_set_canvas(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  active_canvas_id = lbm_dec_as_i32(args[0]);
  return ENC_SYM_TRUE;
}

static bool wasm_render_image(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors) {
  if (active_canvas_id < 0) return false;

  uint16_t w    = img->width;
  uint16_t h    = img->height;
  uint32_t npix = (uint32_t)w * (uint32_t)h;
  uint8_t *data = img->data;

  uint8_t *rgba = (uint8_t*)malloc(npix * 4);
  if (!rgba) return false;

  for (uint32_t i = 0; i < npix; i++) {
    uint32_t c  = 0;
    int      px = (int)(i % w);
    int      py = (int)(i / w);

    switch (img->fmt) {
    case indexed2: {
      int byte = (int)i >> 3;
      int bit  = 7 - ((int)i & 0x7);
      int ci   = (data[byte] & (1 << bit)) >> bit;
      c = COLOR_TO_RGB888(colors[ci], px, py);
      break;
    }
    case indexed4: {
      int byte = (int)i >> 2;
      int bit  = (3 - ((int)i & 0x03)) * 2;
      int ci   = (data[byte] & (0x03 << bit)) >> bit;
      c = COLOR_TO_RGB888(colors[ci], px, py);
      break;
    }
    case indexed16: {
      int byte = (int)i >> 1;
      int bit  = (1 - ((int)i & 0x01)) * 4;
      int ci   = (data[byte] & (0x0F << bit)) >> bit;
      c = COLOR_TO_RGB888(colors[ci], px, py);
      break;
    }
    case rgb332: {
      uint8_t  p = data[i];
      uint32_t r = (p >> 5) & 0x7;
      uint32_t g = (p >> 2) & 0x7;
      uint32_t b = p & 0x3;
      b = (b > 0) ? 2*b+1 : 0;
      r = (r == 7) ? 255 : r * 36;
      g = (g == 7) ? 255 : g * 36;
      b = (b == 7) ? 255 : b * 36;
      c = (r << 16) | (g << 8) | b;
      break;
    }
    case rgb565: {
      uint16_t p = (uint16_t)((data[2*i] << 8) | data[2*i+1]);
      uint32_t r = p >> 11;
      uint32_t g = (p >> 5) & 0x3F;
      uint32_t b = p & 0x1F;
      c = (r << 19) | (g << 10) | (b << 3);
      break;
    }
    case rgb888: {
      c = ((uint32_t)data[3*i] << 16) | ((uint32_t)data[3*i+1] << 8) | data[3*i+2];
      break;
    }
    default: break;
    }

    rgba[4*i + 0] = (c >> 16) & 0xFF;
    rgba[4*i + 1] = (c >>  8) & 0xFF;
    rgba[4*i + 2] =  c        & 0xFF;
    rgba[4*i + 3] = 255;
  }

  js_canvas_put_image(active_canvas_id, rgba, (int)w, (int)h, (int)x, (int)y);
  free(rgba);
  return true;
}

static void wasm_clear(uint32_t color) {
  if (active_canvas_id >= 0) js_canvas_clear_js(active_canvas_id, color);
}

static void wasm_reset(void) {
  if (active_canvas_id >= 0) js_canvas_clear_js(active_canvas_id, 0);
}

static const char *ctx_state_str(uint32_t state) {
  uint32_t s = state & ~LBM_THREAD_STATE_GC_BIT;
  if (s == LBM_THREAD_STATE_READY)   return "ready";
  if (s & LBM_THREAD_STATE_SLEEPING) return "sleeping";
  if (s & LBM_THREAD_STATE_RECV_BL)  return "recv-blocked";
  if (s & LBM_THREAD_STATE_RECV_TO)  return "recv-timeout";
  if (s & LBM_THREAD_STATE_TIMEOUT)  return "timeout";
  if (s & LBM_THREAD_STATE_BLOCKED)  return "blocked";
  return "unknown";
}

static void ctx_to_json(eval_context_t *ctx, void *arg1, void *arg2) {
  char *buf = (char*)arg1;
  int  *pos = (int*)arg2;
  const char *sep   = (*pos > 1) ? "," : "";
  const char *name  = ctx->name ? ctx->name : "";
  const char *state = ctx_state_str(ctx->state);
  int n = snprintf(buf + *pos, (size_t)(CTX_LIST_BUFFER_SIZE - *pos),
                   "%s{\"cid\":%d,\"name\":\"%s\",\"state\":\"%s\"}",
                   sep, (int)ctx->id, name, state);
  if (n > 0) *pos += n;
}

static void done_callback(eval_context_t *ctx) {
  char result[1024];
  lbm_print_value(result, sizeof(result), ctx->r);
  print_callback("> %s\n", result);
  drop_reader(ctx->id);
}

static void sleep_callback(uint32_t us) {
  (void)us;
}

// ////////////////////////////////////////////////////////////
//   Exported WASM API
//

EMSCRIPTEN_KEEPALIVE
int lbm_wasm_init(void) {
  if (!lbm_init(heap, HEAP_SIZE,
                lbm_memory, LBM_MEMORY_SIZE,
                lbm_bitmap, LBM_BITMAP_SIZE,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    return 0;
  }
  if (!lbm_eval_init_events(20)) {
    return 0;
  }
  lbm_set_critical_error_callback(critical_callback);
  lbm_set_ctx_done_callback(done_callback);
  lbm_set_usleep_callback(sleep_callback);
  lbm_set_dynamic_load_callback(dynamic_loader);
  lbm_set_printf_callback(print_callback);

  lbm_image_init(image_storage,
                 IMAGE_STORAGE_SIZE / sizeof(uint32_t),
                 wasm_image_write);
  memset(image_storage, 0, IMAGE_STORAGE_SIZE);
  lbm_image_create("wasm");
  if (!lbm_image_boot()) {
    return 0;
  }

  lbm_array_extensions_init();
  lbm_string_extensions_init();
  lbm_math_extensions_init();
  lbm_runtime_extensions_init();
  lbm_random_extensions_init();
  lbm_set_extensions_init();
  lbm_dyn_lib_init();
  lbm_crypto_extensions_init();
  lbm_dsp_extensions_init();
  lbm_ecc_extensions_init();
  lbm_display_extensions_init();
  lbm_display_extensions_set_callbacks(wasm_render_image, wasm_clear, wasm_reset);
  lbm_ttf_extensions_init();

  lbm_add_eval_symbols();

  lbm_add_extension("systime",             ext_systime);
  lbm_add_extension("secs-since",         ext_secs_since);
  lbm_add_extension("print",               ext_print);
  lbm_add_extension("puts",                ext_puts);
  lbm_add_extension("set-print-prefix",    ext_set_print_prefix);
  lbm_add_extension("wasm-add-button",              ext_wasm_add_button);
  lbm_add_extension("wasm-add-keyboard-control",    ext_wasm_add_keyboard_control);
  lbm_add_extension("wasm-keyboard-control-bind",   ext_wasm_keyboard_control_bind);
  lbm_add_extension("wasm-create-tab",              ext_wasm_create_tab);
  lbm_add_extension("wasm-add-canvas",      ext_wasm_add_canvas);
  lbm_add_extension("wasm-set-canvas",      ext_wasm_set_canvas);
  lbm_add_extension("wasm-add-plot",        ext_wasm_add_plot);
  lbm_add_extension("wasm-add-plot-multi",  ext_wasm_add_plot_multi);
  lbm_add_extension("wasm-add-plot-xy",     ext_wasm_add_plot_xy);
  lbm_add_extension("import",          ext_import);
  lbm_add_extension("f-open",          ext_fopen);
  lbm_add_extension("f-close",         ext_fclose);
  lbm_add_extension("load-file",       ext_load_file);
  lbm_add_extension("f-write",         ext_fwrite);
  lbm_add_extension("f-write-str",     ext_fwrite_str);
  lbm_add_extension("f-write-value",   ext_fwrite_value);
  lbm_add_extension("f-read",          ext_fread);
  lbm_add_extension("f-read-byte",     ext_fread_byte);
  lbm_add_extension("f-seek",          ext_fseek);
  lbm_add_extension("f-tell",          ext_ftell);
  lbm_add_extension("f-readline",      ext_f_readline);
  lbm_add_extension("f-size",          ext_f_size);
  lbm_add_extension("f-sync",          ext_f_sync);
  lbm_add_extension("f-connect",       ext_f_connect);
  lbm_add_extension("f-connect-nand",  ext_f_connect);
  lbm_add_extension("f-disconnect",    ext_f_disconnect);
  lbm_add_extension("f-fatinfo",       ext_f_fatinfo);
  lbm_add_extension("f-list",          ext_file_list);
  lbm_add_extension("wasm-save-file",  ext_wasm_save_file);
  lbm_add_extension("f-pwd",           ext_fs_pwd);
  lbm_add_extension("f-cd",            ext_fs_cd);
  lbm_add_extension("f-mkdir",         ext_fs_mkdir);
  lbm_add_extension("f-rm",            ext_fs_rm);
  lbm_add_extension("f-rename",        ext_fs_mv);
  lbm_add_extension("f-exists",        ext_fs_exists);
  lbm_add_extension("f-stat",          ext_fs_stat);
  lbm_add_extension("f-ls",            ext_fs_ls);
  lbm_add_extension("f-edit",          ext_fs_open);
  lbm_add_extension("get-bms-val",     ext_get_bms_val);
  lbm_add_extension("conf-get",        ext_conf_get);
  lbm_add_extension("conf-set",        ext_conf_set);
  lbm_add_extension("conf-store",      ext_conf_store);
  lbm_add_extension("gnss-lat-lon",   ext_gnss_lat_lon);
  lbm_add_extension("gnss-height",   ext_gnss_height);
  lbm_add_extension("gnss-speed",    ext_gnss_speed);
  lbm_add_extension("gnss-hdop",     ext_gnss_hdop);
  lbm_add_extension("gnss-date-time",ext_gnss_date_time);
  lbm_add_extension("gnss-age",      ext_gnss_age);
  lbm_add_extension("eeprom-read-i",  ext_eeprom_read_i);
  lbm_add_extension("eeprom-read-f",  ext_eeprom_read_f);
  lbm_add_extension("eeprom-store-i", ext_eeprom_store_i);
  lbm_add_extension("eeprom-store-f", ext_eeprom_store_f);
  lbm_add_extension("gpio-configure",  ext_gpio_configure);
  lbm_add_extension("gpio-write",      ext_gpio_write);
  lbm_add_extension("gpio-read",       ext_gpio_read);
  lbm_add_extension("sim-gpio-write",  ext_sim_gpio_write);
  lbm_add_extension("sim-gpio-read",   ext_sim_gpio_read);
  lbm_add_extension("sim-adc-set",      ext_sim_adc_set);
  lbm_add_extension("sim-adc-get",      ext_sim_adc_get);
  lbm_add_extension("get-adc",          ext_get_adc);
  lbm_add_extension("get-adc-decoded",  ext_get_adc_decoded);
  lbm_add_extension("image-save",       ext_image_save);
  lbm_add_extension("color-mix",        ext_color_mix);
  lbm_add_extension("color-make",       ext_color_make);
  lbm_add_extension("bits-enc-int",     ext_bits_enc_int);
  lbm_add_extension("bits-dec-int",     ext_bits_dec_int);

  lbm_uint seek_set = 0, seek_cur = 0, seek_end = 0;
  lbm_add_symbol("seek-set", &seek_set);
  lbm_add_symbol("seek-cur", &seek_cur);
  lbm_add_symbol("seek-end", &seek_end);
  sym_seek_set = lbm_enc_sym(seek_set);
  sym_seek_cur = lbm_enc_sym(seek_cur);
  sym_seek_end = lbm_enc_sym(seek_end);

  output_buffer[0] = '\0';

  print_callback("\nLispBM REPL on WASM\n");

  return 1;
}

EMSCRIPTEN_KEEPALIVE
int lbm_wasm_reset(void) {
  reader_t *r = readers;
  while (r) {
    reader_t *next = r->next;
    free(r->str);
    free(r);
    r = next;
  }
  readers = NULL;
  active_canvas_id = -1;
  print_prefix[0] = '\0';
  memset(heap,        0, sizeof(heap));
  memset(lbm_memory,  0, sizeof(lbm_memory));
  memset(lbm_bitmap,  0, sizeof(lbm_bitmap));
  memset(extensions,  0, sizeof(extensions));
  return lbm_wasm_init();
}

EMSCRIPTEN_KEEPALIVE
void lbm_wasm_eval(const char *str) {
  size_t len = strlen(str);
  char *buf = (char*)malloc(len + 1);
  if (!buf) return;
  memcpy(buf, str, len + 1);
  lbm_create_string_char_channel(&string_tok_state, &string_tok, buf);
  lbm_cid cid = lbm_load_and_eval_expression(&string_tok);
  if (cid >= 0) {
    add_reader(buf, cid);
  } else {
    free(buf);
    print_callback("Error: failed to spawn eval context\n");
  }
}

EMSCRIPTEN_KEEPALIVE
void lbm_wasm_eval_program(const char *str) {
  size_t len = strlen(str);
  char *buf = (char*)malloc(len + 1);
  if (!buf) return;
  memcpy(buf, str, len + 1);
  lbm_create_string_char_channel(&string_tok_state, &string_tok, buf);
  lbm_cid cid = lbm_load_and_eval_program(&string_tok, NULL);
  if (cid >= 0) {
    add_reader(buf, cid);
  } else {
    free(buf);
    print_callback("Error: failed to spawn eval program context\n");
  }
}

EMSCRIPTEN_KEEPALIVE
void lbm_wasm_step(void) {
  lbm_eval_step(1);
}

EMSCRIPTEN_KEEPALIVE
bool lbm_wasm_run(int steps) {
  return lbm_eval_step(steps);
}

EMSCRIPTEN_KEEPALIVE
const char *lbm_wasm_get_output(void) {
  return output_buffer;
}

EMSCRIPTEN_KEEPALIVE
void lbm_wasm_clear_output(void) {
  output_pos = 0;
  output_buffer[0] = '\0';
}

EMSCRIPTEN_KEEPALIVE
const char *lbm_wasm_get_stats(void) {
  lbm_heap_state_t hs;
  lbm_get_heap_state(&hs);

  lbm_uint heap_free    = lbm_heap_num_free();
  lbm_uint mem_words    = lbm_memory_num_words();
  lbm_uint mem_free     = lbm_memory_num_free();
  lbm_uint mem_longest  = lbm_memory_longest_free();
  lbm_uint mem_max_used = lbm_memory_maximum_used();
  float    mem_max_pct  = mem_words > 0
                          ? 100.0f * ((float)mem_max_used / (float)mem_words)
                          : 0.0f;

  snprintf(stats_buffer, STATS_BUFFER_SIZE,
           "{"
           "\"heap_size\":%u,\"heap_free\":%u,"
           "\"gc_num\":%u,\"gc_recovered\":%u,"
           "\"gc_recovered_arrays\":%u,\"gc_marked\":%u,"
           "\"gc_stack_max\":%u,\"gc_stack_size\":%u,"
           "\"mem_size\":%u,\"mem_free\":%u,"
           "\"mem_longest_free\":%u,\"mem_max_used_pct\":%.1f,"
           "\"num_alloc_arrays\":%u"
           "}",
           (unsigned)HEAP_SIZE,        (unsigned)heap_free,
           (unsigned)hs.gc_num,        (unsigned)hs.gc_recovered,
           (unsigned)hs.gc_recovered_arrays, (unsigned)hs.gc_marked,
           (unsigned)lbm_get_gc_stack_max(), (unsigned)lbm_get_gc_stack_size(),
           (unsigned)(mem_words * 4),  (unsigned)(mem_free * 4),
           (unsigned)(mem_longest * 4), mem_max_pct,
           (unsigned)hs.num_alloc_arrays);

  return stats_buffer;
}

EMSCRIPTEN_KEEPALIVE
int lbm_wasm_is_running(void) {
  return (int)(lbm_get_eval_state() != EVAL_CPS_STATE_DEAD);
}

EMSCRIPTEN_KEEPALIVE
const char *lbm_wasm_get_ctxs(void) {
  ctx_list_buffer[0] = '[';
  int pos = 1;
  lbm_all_ctxs_iterator(ctx_to_json, ctx_list_buffer, &pos);
  if (pos < CTX_LIST_BUFFER_SIZE - 2) {
    ctx_list_buffer[pos++] = ']';
    ctx_list_buffer[pos]   = '\0';
  }
  return ctx_list_buffer;
}

int main(void) {
  return 0;
}
