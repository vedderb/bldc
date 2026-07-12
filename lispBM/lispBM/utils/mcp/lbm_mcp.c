/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

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

#include "lbm_mcp.h"
#include "cJSON.h"

#include "lispbm.h"
#include "lbm_channel.h"
#include "lbm_c_interop.h"
#include "print.h"
#include "eval_cps.h"

#include "platform_mutex.h"
#include "platform_thread.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <ctype.h>

#define MCP_LINE_MAX       (64 * 1024)
#define MCP_RESULT_MAX     4096
#define MCP_OUTPUT_MAX     (8 * 1024)
#define DOC_PATH_DEFAULT   "../doc"
#define DOC_MAX_SIZE       (128 * 1024)
#define SEARCH_MAX_RESULTS 8
#define SEARCH_SECTION_MAX 2048
#define SEARCH_TOTAL_MAX   (16 * 1024)

// ------------------------------------------------------------------
// Reset / reinit callbacks (set by the host before lbm_mcp_run)
// ------------------------------------------------------------------

static bool (*reset_cb)(void)                              = NULL;
static bool (*reinit_cb)(uint32_t heap, uint32_t mem)      = NULL;

void lbm_mcp_set_reset_callback(bool (*cb)(void))                          { reset_cb  = cb; }
void lbm_mcp_set_reinit_callback(bool (*cb)(uint32_t h, uint32_t m))       { reinit_cb = cb; }

// ------------------------------------------------------------------
// Shared eval state
// ------------------------------------------------------------------

static lbm_mutex_t     eval_mutex;
static volatile bool   eval_done  = false;
static volatile lbm_cid wait_cid  = -1;
static char eval_result[MCP_RESULT_MAX];

static lbm_string_channel_state_t mcp_tok_state;
static lbm_char_channel_t         mcp_tok;

// Captures output from (print ...) and friends during eval
static char output_buf[MCP_OUTPUT_MAX];
static int  output_pos = 0;

// ------------------------------------------------------------------
// Callbacks installed on lbm_mcp_run entry
// ------------------------------------------------------------------

static int mcp_printf(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int remaining = MCP_OUTPUT_MAX - output_pos - 1;
  int n = 0;
  if (remaining > 0) {
    n = vsnprintf(output_buf + output_pos, (size_t)remaining, fmt, args);
    if (n > 0) output_pos += (n < remaining) ? n : remaining;
  }
  va_end(args);
  return n;
}

static void mcp_done_callback(eval_context_t *ctx) {
  lbm_mutex_lock(&eval_mutex);
  if (ctx->id == wait_cid) {
    lbm_print_value(eval_result, MCP_RESULT_MAX, ctx->r);
    eval_done = true;
  }
  lbm_mutex_unlock(&eval_mutex);
}

// ------------------------------------------------------------------
// JSON response helpers
// ------------------------------------------------------------------

static void send_json(cJSON *obj) {
  char *str = cJSON_PrintUnformatted(obj);
  if (str) {
    fputs(str, stdout);
    fputc('\n', stdout);
    fflush(stdout);
    free(str);
  }
}

static void send_error_response(cJSON *id, int code, const char *msg) {
  cJSON *resp  = cJSON_CreateObject();
  cJSON *error = cJSON_CreateObject();
  cJSON_AddStringToObject(resp, "jsonrpc", "2.0");
  cJSON_AddItemToObject(resp, "id", id ? cJSON_Duplicate(id, 0) : cJSON_CreateNull());
  cJSON_AddNumberToObject(error, "code", (double)code);
  cJSON_AddStringToObject(error, "message", msg);
  cJSON_AddItemToObject(resp, "error", error);
  send_json(resp);
  cJSON_Delete(resp);
}

static void send_tool_result(cJSON *id, const char *text) {
  cJSON *resp    = cJSON_CreateObject();
  cJSON *result  = cJSON_CreateObject();
  cJSON *content = cJSON_CreateArray();
  cJSON *item    = cJSON_CreateObject();
  cJSON_AddStringToObject(resp, "jsonrpc", "2.0");
  cJSON_AddItemToObject(resp, "id", cJSON_Duplicate(id, 0));
  cJSON_AddStringToObject(item, "type", "text");
  cJSON_AddStringToObject(item, "text", text);
  cJSON_AddItemToArray(content, item);
  cJSON_AddItemToObject(result, "content", content);
  cJSON_AddItemToObject(resp, "result", result);
  send_json(resp);
  cJSON_Delete(resp);
}

// ------------------------------------------------------------------
// LispBM eval helpers
// ------------------------------------------------------------------

static bool pause_and_wait(void) {
  lbm_pause_eval_with_gc(30);
  int timeout = 1000;
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout-- > 0) {
    lbm_thread_sleep_us(1000);
  }
  return timeout > 0;
}

// Combines captured print output with the eval result into out.
static void format_combined(char *out, size_t out_size) {
  if (output_pos > 0) {
    output_buf[output_pos] = '\0';
    snprintf(out, out_size, "%s\n=> %s", output_buf, eval_result);
  } else {
    snprintf(out, out_size, "%s", eval_result);
  }
}

static void run_eval(const char *src, bool is_program, char *out, size_t out_size) {
  size_t src_len = strlen(src);
  char *buf = malloc(src_len + 1);
  if (!buf) {
    snprintf(out, out_size, "error: out of memory");
    return;
  }
  memcpy(buf, src, src_len + 1);

  output_pos = 0;
  lbm_mutex_lock(&eval_mutex);
  eval_done = false;
  lbm_mutex_unlock(&eval_mutex);

  if (!pause_and_wait()) {
    free(buf);
    lbm_continue_eval();
    snprintf(out, out_size, "error: evaluator did not pause");
    return;
  }

  lbm_create_string_char_channel(&mcp_tok_state, &mcp_tok, buf);
  lbm_cid cid = is_program
    ? lbm_load_and_eval_program_incremental(&mcp_tok, NULL)
    : lbm_load_and_eval_expression(&mcp_tok);

  if (cid < 0) {
    free(buf);
    lbm_continue_eval();
    snprintf(out, out_size, "error: failed to load");
    return;
  }

  lbm_mutex_lock(&eval_mutex);
  wait_cid = cid;
  lbm_mutex_unlock(&eval_mutex);

  lbm_continue_eval();

  while (!eval_done) {
    lbm_thread_sleep_us(1000);
  }
  lbm_mutex_lock(&eval_mutex);
  format_combined(out, out_size);
  lbm_mutex_unlock(&eval_mutex);

  free(buf);
}

// ------------------------------------------------------------------
// Documentation tools
// ------------------------------------------------------------------

static const char *doc_path = DOC_PATH_DEFAULT;

void lbm_mcp_set_doc_path(const char *path) {
  doc_path = path;
}

typedef struct { const char *file; const char *desc; } doc_entry_t;

static const doc_entry_t doc_table[] = {
  { "lbmref.md",          "Core language: syntax, types, built-ins, control flow, environments, lists" },
  { "arrayref.md",        "Array extension functions" },
  { "stringref.md",       "String extension functions" },
  { "mathref.md",         "Math extensions (trigonometry, logarithms, etc.)" },
  { "runtimeref.md",      "Runtime and system extensions" },
  { "displayref.md",      "Display and graphics extensions" },
  { "dspref.md",          "DSP (digital signal processing) extensions" },
  { "cryptref.md",        "Cryptography extensions" },
  { "dynref.md",          "Dynamic library loading extensions" },
  { "mutexref.md",        "Mutex and synchronization extensions" },
  { "randomref.md",       "Random number extensions" },
  { "setref.md",          "Set data structure extensions" },
  { "ttfref.md",          "TrueType font extensions" },
  { "lbm-image-format.md","LispBM image file format specification" },
  { NULL, NULL }
};

static bool contains_icase(const char *hay, const char *needle) {
  size_t nlen = strlen(needle);
  size_t hlen = strlen(hay);
  if (nlen == 0) return true;
  if (nlen > hlen) return false;
  for (size_t i = 0; i <= hlen - nlen; i++) {
    size_t j;
    for (j = 0; j < nlen; j++) {
      if (tolower((unsigned char)hay[i + j]) != tolower((unsigned char)needle[j])) break;
    }
    if (j == nlen) return true;
  }
  return false;
}

static void handle_list_docs(cJSON *id) {
  char out[2048];
  int pos = 0;
  pos += snprintf(out + pos, sizeof(out) - (size_t)pos,
                  "Available LispBM documentation (doc path: %s):\n\n", doc_path);
  for (int i = 0; doc_table[i].file; i++) {
    pos += snprintf(out + pos, sizeof(out) - (size_t)pos,
                    "  %-26s %s\n", doc_table[i].file, doc_table[i].desc);
  }
  send_tool_result(id, out);
}

static void handle_get_doc(cJSON *id, cJSON *args) {
  cJSON *name_j = cJSON_GetObjectItem(args, "name");
  if (!cJSON_IsString(name_j)) { send_error_response(id, -32602, "missing name"); return; }

  const char *name = name_j->valuestring;
  char path[512];
  // accept both "lbmref" and "lbmref.md"
  if (strstr(name, ".md")) {
    snprintf(path, sizeof(path), "%s/%s", doc_path, name);
  } else {
    snprintf(path, sizeof(path), "%s/%s.md", doc_path, name);
  }

  FILE *fp = fopen(path, "r");
  if (!fp) { send_tool_result(id, "error: doc file not found"); return; }

  char *buf = malloc(DOC_MAX_SIZE + 64);
  if (!buf) { fclose(fp); send_tool_result(id, "error: out of memory"); return; }

  size_t n = fread(buf, 1, DOC_MAX_SIZE, fp);
  fclose(fp);
  buf[n] = '\0';
  if (n == DOC_MAX_SIZE) {
    strcpy(buf + DOC_MAX_SIZE - 64, "\n... [truncated]");
  }
  send_tool_result(id, buf);
  free(buf);
}

static void handle_search_docs(cJSON *id, cJSON *args) {
  cJSON *query_j = cJSON_GetObjectItem(args, "query");
  if (!cJSON_IsString(query_j)) { send_error_response(id, -32602, "missing query"); return; }
  const char *query = query_j->valuestring;

  char *out = malloc(SEARCH_TOTAL_MAX + 64);
  if (!out) { send_tool_result(id, "error: out of memory"); return; }
  int out_pos = 0;
  int results  = 0;

  for (int i = 0; doc_table[i].file && results < SEARCH_MAX_RESULTS; i++) {
    char path[512];
    snprintf(path, sizeof(path), "%s/%s", doc_path, doc_table[i].file);
    FILE *fp = fopen(path, "r");
    if (!fp) continue;

    char line[1024];
    char section_hdr[1024] = "";
    char section_body[SEARCH_SECTION_MAX] = "";
    int  body_pos = 0;

    while (fgets(line, sizeof(line), fp) && results < SEARCH_MAX_RESULTS) {
      if (line[0] == '#') {
        // flush previous section if it matches
        if (section_hdr[0] &&
            (contains_icase(section_hdr, query) || contains_icase(section_body, query))) {
          section_body[body_pos] = '\0';
          int added = snprintf(out + out_pos, (size_t)(SEARCH_TOTAL_MAX - out_pos),
                               "[%s] %s\n%s\n---\n", doc_table[i].file, section_hdr, section_body);
          if (added > 0) out_pos += added;
          results++;
        }
        snprintf(section_hdr, sizeof(section_hdr), "%s", line);
        section_hdr[strcspn(section_hdr, "\n")] = '\0';
        body_pos = 0;
        section_body[0] = '\0';
      } else {
        size_t llen = strlen(line);
        if (body_pos + (int)llen < SEARCH_SECTION_MAX - 1) {
          memcpy(section_body + body_pos, line, llen);
          body_pos += (int)llen;
        }
      }
    }
    // flush last section
    if (section_hdr[0] && results < SEARCH_MAX_RESULTS &&
        (contains_icase(section_hdr, query) || contains_icase(section_body, query))) {
      section_body[body_pos] = '\0';
      int added = snprintf(out + out_pos, (size_t)(SEARCH_TOTAL_MAX - out_pos),
                           "[%s] %s\n%s\n---\n", doc_table[i].file, section_hdr, section_body);
      if (added > 0) out_pos += added;
      results++;
    }
    fclose(fp);
  }

  if (results == 0) {
    snprintf(out, (size_t)SEARCH_TOTAL_MAX, "No documentation sections found matching \"%s\".", query);
  }
  send_tool_result(id, out);
  free(out);
}

// ------------------------------------------------------------------
// Reset / reinit handlers
// ------------------------------------------------------------------

static void restore_mcp_callbacks(void) {
  lbm_set_ctx_done_callback(mcp_done_callback);
  lbm_set_printf_callback(mcp_printf);
}

static void handle_reset(cJSON *id) {
  if (!reset_cb) {
    send_tool_result(id, "error: reset not available");
    return;
  }
  bool ok = reset_cb();
  if (ok) {
    restore_mcp_callbacks();
    lbm_continue_eval();
  }
  send_tool_result(id, ok ? "Reset OK" : "Reset failed");
}

static void handle_reinit(cJSON *id, cJSON *args) {
  if (!reinit_cb) {
    send_tool_result(id, "error: reinit not available");
    return;
  }
  uint32_t new_heap = 0;
  uint32_t new_mem  = 0;
  if (args) {
    cJSON *h = cJSON_GetObjectItem(args, "heap_cells");
    cJSON *m = cJSON_GetObjectItem(args, "memory_bytes");
    if (cJSON_IsNumber(h)) new_heap = (uint32_t)h->valuedouble;
    if (cJSON_IsNumber(m)) new_mem  = (uint32_t)m->valuedouble;
  }
  bool ok = reinit_cb(new_heap, new_mem);
  if (ok) {
    restore_mcp_callbacks();
    lbm_continue_eval();
  }
  char out[128];
  snprintf(out, sizeof(out), ok ? "Reinit OK" : "Reinit failed");
  send_tool_result(id, out);
}

// ------------------------------------------------------------------
// MCP message handlers
// ------------------------------------------------------------------

static void handle_initialize(cJSON *id) {
  cJSON *resp   = cJSON_CreateObject();
  cJSON *result = cJSON_CreateObject();
  cJSON *caps   = cJSON_CreateObject();
  cJSON *info   = cJSON_CreateObject();
  cJSON_AddStringToObject(resp, "jsonrpc", "2.0");
  cJSON_AddItemToObject(resp, "id", cJSON_Duplicate(id, 0));
  cJSON_AddStringToObject(result, "protocolVersion", "2024-11-05");
  cJSON_AddItemToObject(caps, "tools", cJSON_CreateObject());
  cJSON_AddItemToObject(result, "capabilities", caps);
  cJSON_AddStringToObject(info, "name", "lbm-mcp");
  cJSON_AddStringToObject(info, "version", "0.1.0");
  cJSON_AddItemToObject(result, "serverInfo", info);
  cJSON_AddItemToObject(resp, "result", result);
  send_json(resp);
  cJSON_Delete(resp);
}

static cJSON *make_tool(const char *name, const char *desc,
                        const char *arg_name, const char *arg_desc) {
  cJSON *tool     = cJSON_CreateObject();
  cJSON *schema   = cJSON_CreateObject();
  cJSON *props    = cJSON_CreateObject();
  cJSON *arg_prop = cJSON_CreateObject();
  cJSON *required = cJSON_CreateArray();
  cJSON_AddStringToObject(tool, "name", name);
  cJSON_AddStringToObject(tool, "description", desc);
  cJSON_AddStringToObject(schema, "type", "object");
  cJSON_AddStringToObject(arg_prop, "type", "string");
  cJSON_AddStringToObject(arg_prop, "description", arg_desc);
  cJSON_AddItemToObject(props, arg_name, arg_prop);
  cJSON_AddItemToObject(schema, "properties", props);
  cJSON_AddItemToArray(required, cJSON_CreateString(arg_name));
  cJSON_AddItemToObject(schema, "required", required);
  cJSON_AddItemToObject(tool, "inputSchema", schema);
  return tool;
}

static void handle_tools_list(cJSON *id) {
  cJSON *resp   = cJSON_CreateObject();
  cJSON *result = cJSON_CreateObject();
  cJSON *tools  = cJSON_CreateArray();
  cJSON_AddStringToObject(resp, "jsonrpc", "2.0");
  cJSON_AddItemToObject(resp, "id", cJSON_Duplicate(id, 0));
  cJSON_AddItemToArray(tools,
    make_tool("eval",
              "Evaluate a LispBM expression. Definitions (defun, def) persist for the session.",
              "expression", "LispBM expression to evaluate"));
  cJSON_AddItemToArray(tools,
    make_tool("load_file",
              "Load and evaluate a LispBM source file. All top-level definitions become available in the session.",
              "path", "Path to the .lbm source file"));

  // get_caveats — no arguments, prominent description so AI calls it first
  cJSON *cav_tool   = cJSON_CreateObject();
  cJSON *cav_schema = cJSON_CreateObject();
  cJSON_AddStringToObject(cav_tool, "name", "get_caveats");
  cJSON_AddStringToObject(cav_tool, "description",
    "IMPORTANT: Call this first when starting a LispBM session. "
    "Returns notes on how LispBM differs from Scheme and Common Lisp — "
    "things AI tools frequently get wrong.");
  cJSON_AddStringToObject(cav_schema, "type", "object");
  cJSON_AddItemToObject(cav_schema, "properties", cJSON_CreateObject());
  cJSON_AddItemToObject(cav_tool, "inputSchema", cav_schema);
  cJSON_AddItemToArray(tools, cav_tool);

  // list_docs has no required arguments
  cJSON *list_tool   = cJSON_CreateObject();
  cJSON *list_schema = cJSON_CreateObject();
  cJSON_AddStringToObject(list_tool, "name", "list_docs");
  cJSON_AddStringToObject(list_tool, "description", "List the available LispBM reference documentation files.");
  cJSON_AddStringToObject(list_schema, "type", "object");
  cJSON_AddItemToObject(list_schema, "properties", cJSON_CreateObject());
  cJSON_AddItemToObject(list_tool, "inputSchema", list_schema);
  cJSON_AddItemToArray(tools, list_tool);

  cJSON_AddItemToArray(tools,
    make_tool("get_doc",
              "Return the full contents of a LispBM reference document (e.g. \"lbmref\", \"arrayref\").",
              "name", "Document name with or without .md extension"));
  cJSON_AddItemToArray(tools,
    make_tool("search_docs",
              "Search all LispBM documentation by section. Returns sections whose heading or content matches the query.",
              "query", "Term or function name to search for"));

  // reset — no arguments
  cJSON *reset_tool   = cJSON_CreateObject();
  cJSON *reset_schema = cJSON_CreateObject();
  cJSON_AddStringToObject(reset_tool, "name", "reset");
  cJSON_AddStringToObject(reset_tool, "description",
    "Reset LispBM to a clean state, clearing all definitions and freeing the heap. "
    "Memory pool size is preserved. Call (gc) after reset before allocating large buffers.");
  cJSON_AddStringToObject(reset_schema, "type", "object");
  cJSON_AddItemToObject(reset_schema, "properties", cJSON_CreateObject());
  cJSON_AddItemToObject(reset_tool, "inputSchema", reset_schema);
  cJSON_AddItemToArray(tools, reset_tool);

  // reinit — two optional integer arguments
  cJSON *reinit_tool   = cJSON_CreateObject();
  cJSON *reinit_schema = cJSON_CreateObject();
  cJSON *reinit_props  = cJSON_CreateObject();
  cJSON *heap_prop     = cJSON_CreateObject();
  cJSON *mem_prop      = cJSON_CreateObject();
  cJSON_AddStringToObject(reinit_tool, "name", "reinit");
  cJSON_AddStringToObject(reinit_tool, "description",
    "Reinitialize LispBM, optionally with new resource sizes. "
    "heap_cells sets the heap size in cons cells; memory_bytes sets the flat memory size in bytes. "
    "Omit either parameter (or pass 0) to keep the current size. "
    "Call (gc) after reinit before allocating large buffers.");
  cJSON_AddStringToObject(reinit_schema, "type", "object");
  cJSON_AddStringToObject(heap_prop, "type", "integer");
  cJSON_AddStringToObject(heap_prop, "description", "New heap size in cons cells (0 or omit = keep current)");
  cJSON_AddStringToObject(mem_prop, "type", "integer");
  cJSON_AddStringToObject(mem_prop, "description", "New flat memory size in bytes (0 or omit = keep current)");
  cJSON_AddItemToObject(reinit_props, "heap_cells", heap_prop);
  cJSON_AddItemToObject(reinit_props, "memory_bytes", mem_prop);
  cJSON_AddItemToObject(reinit_schema, "properties", reinit_props);
  cJSON_AddItemToObject(reinit_tool, "inputSchema", reinit_schema);
  cJSON_AddItemToArray(tools, reinit_tool);

  cJSON_AddItemToObject(result, "tools", tools);
  cJSON_AddItemToObject(resp, "result", result);
  send_json(resp);
  cJSON_Delete(resp);
}

static void handle_tools_call(cJSON *id, cJSON *params) {
  cJSON *name_j = cJSON_GetObjectItem(params, "name");
  cJSON *args_j = cJSON_GetObjectItem(params, "arguments");

  if (!cJSON_IsString(name_j)) {
    send_error_response(id, -32602, "missing tool name");
    return;
  }
  const char *name = name_j->valuestring;

  if (strcmp(name, "eval") == 0) {
    cJSON *expr_j = cJSON_GetObjectItem(args_j, "expression");
    if (!cJSON_IsString(expr_j)) {
      send_error_response(id, -32602, "missing expression argument");
      return;
    }
    char out[MCP_OUTPUT_MAX + MCP_RESULT_MAX + 32];
    run_eval(expr_j->valuestring, false, out, sizeof(out));
    send_tool_result(id, out);

  } else if (strcmp(name, "load_file") == 0) {
    cJSON *path_j = cJSON_GetObjectItem(args_j, "path");
    if (!cJSON_IsString(path_j)) {
      send_error_response(id, -32602, "missing path argument");
      return;
    }
    FILE *fp = fopen(path_j->valuestring, "r");
    if (!fp) {
      send_tool_result(id, "error: file not found");
      return;
    }
    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    char *src = malloc((size_t)fsize + 1);
    if (!src) {
      fclose(fp);
      send_tool_result(id, "error: out of memory");
      return;
    }
    if (fread(src, 1, (size_t)fsize, fp) != (size_t)fsize) {
      free(src);
      fclose(fp);
      send_tool_result(id, "error: file read failed");
      return;
    }
    src[fsize] = '\0';
    fclose(fp);
    char out[MCP_OUTPUT_MAX + MCP_RESULT_MAX + 32];
    run_eval(src, true, out, sizeof(out));
    free(src);
    send_tool_result(id, out);

  } else if (strcmp(name, "get_caveats") == 0) {
    char path[512];
    snprintf(path, sizeof(path), "%s/lbm-ai-notes.md", doc_path);
    FILE *fp = fopen(path, "r");
    if (!fp) { send_tool_result(id, "error: lbm-ai-notes.md not found"); return; }
    char *buf = malloc(DOC_MAX_SIZE + 64);
    if (!buf) { fclose(fp); send_tool_result(id, "error: out of memory"); return; }
    size_t n = fread(buf, 1, DOC_MAX_SIZE, fp);
    fclose(fp);
    buf[n] = '\0';
    send_tool_result(id, buf);
    free(buf);

  } else if (strcmp(name, "list_docs") == 0) {
    handle_list_docs(id);

  } else if (strcmp(name, "get_doc") == 0) {
    if (args_j) handle_get_doc(id, args_j);
    else send_error_response(id, -32602, "missing arguments");

  } else if (strcmp(name, "search_docs") == 0) {
    if (args_j) handle_search_docs(id, args_j);
    else send_error_response(id, -32602, "missing arguments");

  } else if (strcmp(name, "reset") == 0) {
    handle_reset(id);

  } else if (strcmp(name, "reinit") == 0) {
    handle_reinit(id, args_j);

  } else {
    send_error_response(id, -32601, "unknown tool");
  }
}

// ------------------------------------------------------------------
// Main entry point
// ------------------------------------------------------------------

void lbm_mcp_run(void) {
  lbm_mutex_init(&eval_mutex);
  lbm_set_ctx_done_callback(mcp_done_callback);
  lbm_set_printf_callback(mcp_printf);

  char *line = malloc(MCP_LINE_MAX);
  if (!line) return;

  while (fgets(line, MCP_LINE_MAX, stdin)) {
    size_t len = strlen(line);
    while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r')) {
      line[--len] = '\0';
    }
    if (len == 0) continue;

    cJSON *msg = cJSON_Parse(line);
    if (!msg) continue;

    cJSON *method_j = cJSON_GetObjectItem(msg, "method");
    cJSON *id_j     = cJSON_GetObjectItem(msg, "id");
    cJSON *params_j = cJSON_GetObjectItem(msg, "params");

    if (cJSON_IsString(method_j)) {
      const char *method = method_j->valuestring;
      if (strcmp(method, "initialize") == 0) {
        handle_initialize(id_j);
      } else if (strcmp(method, "notifications/initialized") == 0) {
        /* no response for notifications */
      } else if (strcmp(method, "tools/list") == 0) {
        handle_tools_list(id_j);
      } else if (strcmp(method, "tools/call") == 0) {
        if (params_j) {
          handle_tools_call(id_j, params_j);
        } else {
          send_error_response(id_j, -32602, "missing params");
        }
      } else if (id_j) {
        send_error_response(id_j, -32601, "method not found");
      }
    }

    cJSON_Delete(msg);
  }

  free(line);
}
