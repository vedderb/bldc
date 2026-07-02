/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

  This file is part of LispBM.

  LispBM is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LispBM is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LispBM. If not, see <http://www.gnu.org/licenses/>.
*/

#include <string.h>
#include <stdio.h>
#include <stdarg.h>

#include "lbm_packet_interface.h"
#include "packet.h"
#include "buffer.h"
#include "lispbm.h"
#include "print.h"

// ////////////////////////////////////////////////////////////
// VESC packet command IDs (subset used here)

#define COMM_FW_VERSION        0
#define COMM_LISP_SET_RUNNING  133
#define COMM_LISP_GET_STATS    134
#define COMM_LISP_PRINT        135
#define COMM_LISP_REPL_CMD     138
#define COMM_LISP_STREAM_CODE  139

// ////////////////////////////////////////////////////////////
// Internal state

static lbm_packet_if_cfg_t if_cfg;
static PACKET_STATE_t       packet_state;

/* REPL context tracking */
static lbm_cid                    repl_cid = -1;
static char                      *repl_buffer = NULL;
static lbm_string_channel_state_t repl_tok_state;
static lbm_char_channel_t         repl_tok;

/* Streaming state */
static bool                          stream_open = false;
static int32_t                       stream_offset_last = -1;
static int16_t                       stream_result_last = -1;
static lbm_cid                       stream_cid = -1;
static lbm_buffered_channel_state_t  stream_tok_state;
static lbm_char_channel_t            stream_tok;

// ////////////////////////////////////////////////////////////
// Helpers

static bool pause_eval(void) {
  int timeout = 2000;
  lbm_pause_eval_with_gc(30);
  while (lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED && timeout > 0) {
    if (if_cfg.sleep_ms) {
      if_cfg.sleep_ms(1);
    }
    timeout--;
  }
  return timeout > 0;
}


// ////////////////////////////////////////////////////////////
// Done callback — prints the result of REPL evaluations back to the host

static void done_callback(eval_context_t *ctx) {
  if (repl_cid >= 0 && ctx->id == (lbm_uint)repl_cid) {
    char val_buf[256];
    lbm_print_value(val_buf, sizeof(val_buf), ctx->r);
    lbm_packet_if_printf("> %s", val_buf);
    repl_cid = -1;
    if (repl_buffer) {
      lbm_free(repl_buffer);
      repl_buffer = NULL;
    }
  }
  if (stream_cid >= 0 && ctx->id == (lbm_uint)stream_cid) {
    char val_buf[256];
    lbm_print_value(val_buf, sizeof(val_buf), ctx->r);
    lbm_packet_if_printf("Stream done: %s", val_buf);
    lbm_channel_reader_close(&stream_tok);
    stream_cid = -1;
    stream_open = false;
  }
  if (if_cfg.ctx_done) {
    if_cfg.ctx_done(ctx);
  }
}

// ////////////////////////////////////////////////////////////
// Command handlers

static void handle_fw_version(void) {
  uint8_t send_buf[80];
  int32_t ind = 0;

  send_buf[ind++] = COMM_FW_VERSION;
  send_buf[ind++] = if_cfg.fw_version_major;
  send_buf[ind++] = if_cfg.fw_version_minor;

  /* Hardware name */
  const char *hw = if_cfg.hw_name ? if_cfg.hw_name : "LBM-Device";
  size_t hw_len = strlen(hw);
  if (hw_len > 30) hw_len = 30;
  memcpy(send_buf + ind, hw, hw_len);
  ind += (int32_t)hw_len;
  send_buf[ind++] = 0;

  /* UUID: 12 bytes of zeros (no hardware UUID available generically) */
  memset(send_buf + ind, 0, 12);
  ind += 12;

  send_buf[ind++] = 0; /* no pairing done */
  send_buf[ind++] = 0; /* test version */
  send_buf[ind++] = 2; /* HW_TYPE_CUSTOM_MODULE */
  send_buf[ind++] = 0; /* no custom configs */
  send_buf[ind++] = 0; /* no phase filters */
  send_buf[ind++] = 0; /* no HW QML */
  send_buf[ind++] = 0; /* no QML app */
  send_buf[ind++] = 0; /* no NRF flags */

  /* Firmware name */
  const char *fw = if_cfg.fw_name ? if_cfg.fw_name : "LispBM";
  size_t fw_len = strlen(fw);
  if (fw_len > 20) fw_len = 20;
  memcpy(send_buf + ind, fw, fw_len);
  ind += (int32_t)fw_len;
  send_buf[ind++] = 0;

  /* HW CRC: zero (not applicable) */
  buffer_append_int32(send_buf, 0, &ind);

  packet_send_packet(send_buf, (unsigned int)ind, &packet_state);
}

static void handle_set_running(uint8_t *data, unsigned int len) {
  (void)len;
  uint8_t send_buf[3];
  int32_t ind = 0;
  bool running = data[0] != 0;
  bool ok = false;

  if (!running) {
    ok = pause_eval();
  } else {
    lbm_continue_eval();
    ok = true;
  }

  send_buf[ind++] = COMM_LISP_SET_RUNNING;
  send_buf[ind++] = ok ? 1 : 0;
  packet_send_packet(send_buf, (unsigned int)ind, &packet_state);
}

static void handle_get_stats(void) {
  uint8_t send_buf[32];
  int32_t ind = 0;

  lbm_uint heap_total = lbm_heap_size();
  lbm_uint heap_free  = lbm_heap_num_free();
  float heap_use = (heap_total > 0)
    ? 100.0f * (float)(heap_total - heap_free) / (float)heap_total
    : 0.0f;

  lbm_uint mem_total = lbm_memory_num_words();
  lbm_uint mem_free  = lbm_memory_num_free();
  float mem_use = (mem_total > 0)
    ? 100.0f * (float)(mem_total - mem_free) / (float)mem_total
    : 0.0f;

  send_buf[ind++] = COMM_LISP_GET_STATS;
  buffer_append_float32_auto(send_buf, heap_use, &ind);
  buffer_append_float32_auto(send_buf, mem_use, &ind);
  packet_send_packet(send_buf, (unsigned int)ind, &packet_state);
}

static void handle_repl_cmd(uint8_t *data, unsigned int len) {
  if (len <= 1) {
    lbm_packet_if_printf(">");
    return;
  }

  char *str = (char *)data;

  if (strncmp(str, ":reset", 6) == 0) {
    bool ok = pause_eval();
    if (ok && if_cfg.lbm_restart) {
      ok = if_cfg.lbm_restart(false);
    }
    lbm_packet_if_printf(ok ? "Reset OK" : "Reset failed");
  } else if (strncmp(str, ":pause", 6) == 0) {
    bool ok = pause_eval();
    lbm_packet_if_printf(ok ? "Evaluator paused" : "Could not pause");
  } else if (strncmp(str, ":continue", 9) == 0) {
    lbm_continue_eval();
    lbm_packet_if_printf("Evaluator running");
  } else if (strncmp(str, ":info", 5) == 0) {
    lbm_uint ht = lbm_heap_size();
    lbm_uint hf = lbm_heap_num_free();
    lbm_uint mt = lbm_memory_num_words();
    lbm_uint mf = lbm_memory_num_free();
    lbm_packet_if_printf(
      "Heap: %u used / %u total cells\n"
      "Mem:  %u used / %u total words",
      (unsigned)(ht - hf), (unsigned)ht,
      (unsigned)(mt - mf), (unsigned)mt);
  } else {
    /* Evaluate as a LispBM expression */
    if (repl_buffer) {
      lbm_packet_if_printf("REPL busy");
      return;
    }
    if (!pause_eval()) {
      lbm_packet_if_printf("Could not pause evaluator");
      return;
    }
    repl_buffer = lbm_malloc(len + 1);
    if (repl_buffer) {
      memcpy(repl_buffer, data, len);
      repl_buffer[len] = '\0';
      lbm_create_string_char_channel(&repl_tok_state, &repl_tok, repl_buffer);
      repl_cid = lbm_load_and_eval_expression(&repl_tok);
      lbm_continue_eval();
    } else {
      lbm_packet_if_printf("Not enough memory");
      lbm_continue_eval();
    }
  }
}

static void handle_stream_code(uint8_t *data, unsigned int len) {
  int32_t ind = 0;
  int32_t offset  = buffer_get_int32(data, &ind);
  int32_t tot_len = buffer_get_int32(data, &ind);
  uint8_t  mode    = (uint8_t)data[ind++]; /* 0=load on top, 1=reset&load, 2=reload flash&load */

  uint8_t send_buf[16];
  int32_t send_ind = 0;
  send_buf[send_ind++] = COMM_LISP_STREAM_CODE;
  buffer_append_int32(send_buf, offset, &send_ind);
  
  /* Deduplicate: if this packet was already handled, resend the last result */
  if (offset == stream_offset_last) {
    buffer_append_int16(send_buf, stream_result_last, &send_ind);
    packet_send_packet(send_buf, (unsigned int)send_ind, &packet_state);
    return;
  }
  stream_offset_last = offset;

  if (offset == 0) {
    /* Start of a new stream */
    if (stream_open) {
      lbm_channel_writer_close(&stream_tok);
      stream_open = false;
    }

    if (!pause_eval()) {
      stream_result_last = -3;
      buffer_append_int16(send_buf, stream_result_last, &send_ind);
      packet_send_packet(send_buf, (unsigned int)send_ind, &packet_state);
      lbm_packet_if_printf("Stream: could not pause evaluator");
      return;
    }

    if (if_cfg.lbm_restart) {
      /* Always restart to get a clean heap/memory/global-env before loading
         new code. mode==2 additionally reloads from flash. */
      if_cfg.lbm_restart(mode == 2);
      if (!pause_eval()) {
        stream_result_last = -3;
        buffer_append_int16(send_buf, stream_result_last, &send_ind);
        packet_send_packet(send_buf, (unsigned int)send_ind, &packet_state);
        lbm_packet_if_printf("Stream: could not re-pause after restart");
        return;
      }
    }

    lbm_create_buffered_char_channel(&stream_tok_state, &stream_tok);

    lbm_cid cid = lbm_load_and_eval_program_incremental(&stream_tok, "stream-main");
    if (cid <= 0) {
      lbm_continue_eval();
      stream_result_last = -4;
      buffer_append_int16(send_buf, stream_result_last, &send_ind);
      packet_send_packet(send_buf, (unsigned int)send_ind, &packet_state);
      lbm_packet_if_printf("Stream: could not start evaluator");
      return;
    }

    stream_cid = cid;
    lbm_continue_eval();
    stream_open = true;
  }

  if (!stream_open) {
    stream_result_last = -1;
    buffer_append_int16(send_buf, stream_result_last, &send_ind);
    packet_send_packet(send_buf, (unsigned int)send_ind, &packet_state);
    return;
  }

  /* Feed payload bytes into the buffered channel */
  int32_t written = 0;
  int timeout = 1500;
  while (ind < (int32_t)len) {
    int ch_res = lbm_channel_write(&stream_tok, (char)data[ind]);
    if (ch_res == CHANNEL_SUCCESS) {
      ind++;
      written++;
      timeout = 1500;
    } else if (ch_res == CHANNEL_READER_CLOSED) {
      break;
    } else { /* CHANNEL_FULL */
      if (if_cfg.sleep_ms) {
        if_cfg.sleep_ms(1);
      }
      timeout--;
      if (timeout == 0) {
        break;
      }
    }
  }

  if (ind < (int32_t)len) {
    /* Did not consume all bytes */
    stream_open = false;
    stream_offset_last = -1;
    if (timeout == 0) {
      stream_result_last = -5;
      lbm_packet_if_printf("Stream: timed out writing to channel");
    } else {
      stream_result_last = -6;
      lbm_packet_if_printf("Stream: channel closed by reader");
    }
  } else {
    if ((offset + written) >= tot_len) {
      /* All code delivered — close the write end so the reader sees EOF */
      lbm_channel_writer_close(&stream_tok);
      stream_open = false;
      stream_offset_last = -1;
    }
    stream_result_last = 0;
  }

  buffer_append_int16(send_buf, stream_result_last, &send_ind);
  packet_send_packet(send_buf, (unsigned int)send_ind, &packet_state);
}

// ////////////////////////////////////////////////////////////
// Packet receive callback — dispatches on command byte

static void process_packet(uint8_t *data, unsigned int len) {
  if (len == 0) return;
  uint8_t cmd = data[0];
  data++;
  len--;

  switch (cmd) {
  case COMM_FW_VERSION:
    handle_fw_version();
    break;
  case COMM_LISP_SET_RUNNING:
    handle_set_running(data, len);
    break;
  case COMM_LISP_GET_STATS:
    handle_get_stats();
    break;
  case COMM_LISP_REPL_CMD:
    handle_repl_cmd(data, len);
    break;
  case COMM_LISP_STREAM_CODE:
    handle_stream_code(data, len);
    break;
  default:
    break;
  }
}

// ////////////////////////////////////////////////////////////
// Public API

void lbm_packet_if_init(lbm_packet_if_cfg_t *cfg) {
  memcpy(&if_cfg, cfg, sizeof(lbm_packet_if_cfg_t));
  packet_init(cfg->send, process_packet, &packet_state);
  lbm_set_ctx_done_callback(done_callback);
  repl_cid = -1;
  repl_buffer = NULL;
  stream_open = false;
  stream_offset_last = -1;
  stream_result_last = -1;
  stream_cid = -1;
}

void lbm_packet_if_process_byte(uint8_t byte) {
  packet_process_byte(byte, &packet_state);
}

void lbm_packet_if_printf(const char *fmt, ...) {
  static uint8_t buf[PACKET_MAX_PL_LEN];
  buf[0] = COMM_LISP_PRINT;
  va_list args;
  va_start(args, fmt);
  int n = vsnprintf((char *)(buf + 1), PACKET_MAX_PL_LEN - 1, fmt, args);
  va_end(args);
  if (n < 0) n = 0;
  if (n >= PACKET_MAX_PL_LEN - 1) n = PACKET_MAX_PL_LEN - 2;
  buf[1 + n] = '\0';
  packet_send_packet(buf, (unsigned int)(n + 2), &packet_state);
}
