/*
    Copyright 2026 Joel Svensson        svenssonjoel@yahoo.se

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

#include "lbm_limesdr.h"
#include "extensions.h"
#include "lbm_c_interop.h"
#include "platform_thread.h"
#include "platform_mutex.h"
#include "eval_cps.h"

#include <lime/LimeSuite.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// ////////////////////////////////////////////////////////////
// Circular I/Q buffer — same pattern as lbm_rtlsdr.c,
// fully independent static state.

#define MAX_IQ_READERS  5
#define IQ_BUFFER_SIZE  262144
#define RX_CHUNK_SIZE   4096

typedef struct { uint32_t read_pos; } lime_iq_reader_t;

static float             *iq_buf_i        = NULL;
static float             *iq_buf_q        = NULL;
static uint32_t           iq_buf_size     = 0;
static uint32_t           iq_buf_write_pos = 0;
static lime_iq_reader_t  *iq_readers[MAX_IQ_READERS];
static lbm_mutex_t        iq_mutex;

static bool lime_iq_buffer_create(uint32_t size) {
  if (iq_buf_i) free(iq_buf_i);
  if (iq_buf_q) free(iq_buf_q);
  iq_buf_i = (float*)malloc(size * sizeof(float));
  iq_buf_q = (float*)malloc(size * sizeof(float));
  iq_buf_size      = size;
  iq_buf_write_pos = 0;
  for (int i = 0; i < MAX_IQ_READERS; i++) iq_readers[i] = NULL;
  if (!iq_buf_i || !iq_buf_q) {
    free(iq_buf_i); free(iq_buf_q);
    iq_buf_i = iq_buf_q = NULL;
    return false;
  }
  return true;
}

static bool lime_iq_buffer_add_reader(lime_iq_reader_t *r) {
  lbm_mutex_lock(&iq_mutex);
  int free_pos = -1;
  for (int i = 0; i < MAX_IQ_READERS; i++) {
    if (iq_readers[i] == r)  { lbm_mutex_unlock(&iq_mutex); return true; }
    if (!iq_readers[i])        free_pos = i;
  }
  if (free_pos >= 0) { iq_readers[free_pos] = r; lbm_mutex_unlock(&iq_mutex); return true; }
  lbm_mutex_unlock(&iq_mutex);
  return false;
}

static bool lime_iq_buffer_drop_reader(lime_iq_reader_t *r) {
  lbm_mutex_lock(&iq_mutex);
  for (int i = 0; i < MAX_IQ_READERS; i++) {
    if (iq_readers[i] == r) { iq_readers[i] = NULL; lbm_mutex_unlock(&iq_mutex); return true; }
  }
  lbm_mutex_unlock(&iq_mutex);
  return false;
}

static void lime_iq_buffer_write(float *i_data, float *q_data, uint32_t n) {
  if (n >= iq_buf_size) return;
  lbm_mutex_lock(&iq_mutex);
  uint32_t old = iq_buf_write_pos;
  if (iq_buf_write_pos + n >= iq_buf_size) {
    uint32_t s1 = iq_buf_size - iq_buf_write_pos;
    uint32_t s2 = n - s1;
    memcpy(&iq_buf_i[iq_buf_write_pos], i_data, s1 * sizeof(float));
    memcpy(&iq_buf_q[iq_buf_write_pos], q_data, s1 * sizeof(float));
    memcpy(iq_buf_i, &i_data[s1], s2 * sizeof(float));
    memcpy(iq_buf_q, &q_data[s1], s2 * sizeof(float));
    iq_buf_write_pos = s2;
  } else {
    memcpy(&iq_buf_i[iq_buf_write_pos], i_data, n * sizeof(float));
    memcpy(&iq_buf_q[iq_buf_write_pos], q_data, n * sizeof(float));
    iq_buf_write_pos = (iq_buf_write_pos + n) % iq_buf_size;
  }
  uint32_t new_wp = iq_buf_write_pos;
  for (int i = 0; i < MAX_IQ_READERS; i++) {
    if (!iq_readers[i]) continue;
    uint32_t rp = iq_readers[i]->read_pos;
    if ((old < new_wp && rp >= old && rp < new_wp) ||
        (old > new_wp && (rp < new_wp || rp >= old)))
      iq_readers[i]->read_pos = old;
  }
  lbm_mutex_unlock(&iq_mutex);
}

static bool lime_iq_buffer_read(float *i_data, float *q_data, uint32_t n, lime_iq_reader_t *r) {
  if (n >= iq_buf_size) return false;
  lbm_mutex_lock(&iq_mutex);
  uint32_t rp = r->read_pos;
  uint32_t wp = iq_buf_write_pos;
  bool res = false;
  if (wp >= rp) {
    if (wp - rp >= n) {
      memcpy(i_data, &iq_buf_i[rp], n * sizeof(float));
      memcpy(q_data, &iq_buf_q[rp], n * sizeof(float));
      r->read_pos = rp + n;
      res = true;
    }
  } else {
    uint32_t avail = (iq_buf_size - rp) + wp;
    if (avail >= n) {
      uint32_t l = iq_buf_size - rp;
      if (rp + n < iq_buf_size) {
        memcpy(i_data, &iq_buf_i[rp], n * sizeof(float));
        memcpy(q_data, &iq_buf_q[rp], n * sizeof(float));
        r->read_pos = rp + n;
      } else {
        uint32_t m = n - l;
        memcpy(i_data,      &iq_buf_i[rp], l * sizeof(float));
        memcpy(q_data,      &iq_buf_q[rp], l * sizeof(float));
        memcpy(&i_data[l],   iq_buf_i,     m * sizeof(float));
        memcpy(&q_data[l],   iq_buf_q,     m * sizeof(float));
        r->read_pos = m;
      }
      res = true;
    }
  }
  lbm_mutex_unlock(&iq_mutex);
  return res;
}

// ////////////////////////////////////////////////////////////
// Device and stream state

static lms_device_t  *dev            = NULL;
static lms_stream_t   rx_stream;
static volatile bool  rx_running     = false;
static lbm_thread_t   rx_thread;
static float          rx_signal_strength = 0.0f;
static lime_iq_reader_t gp_reader;

// RX blocking get-samples
static lbm_mutex_t    rx_wait_mutex;
static lbm_cid        rx_waiting_cid  = -1;
static uint32_t       rx_waiting_n    = 0;
static float         *rx_waiting_i    = NULL;
static float         *rx_waiting_q    = NULL;

// TX blocking send
static lbm_mutex_t    tx_mutex;
static lbm_cid        tx_waiting_cid  = -1;
static lbm_thread_t   tx_thread;
static float         *tx_interleaved  = NULL;
static uint32_t       tx_n_samples    = 0;

// ////////////////////////////////////////////////////////////
// RX thread — synchronous receive into circular buffer

static float rx_chunk_i[RX_CHUNK_SIZE];
static float rx_chunk_q[RX_CHUNK_SIZE];
static float rx_interleaved[RX_CHUNK_SIZE * 2];

static void rx_thd(void *arg) {
  (void)arg;
  lms_stream_meta_t meta;
  memset(&meta, 0, sizeof(meta));

  while (rx_running) {
    int n = LMS_RecvStream(&rx_stream, rx_interleaved, RX_CHUNK_SIZE, &meta, 1000);
    if (n <= 0) continue;

    float sum_sq = 0.0f;
    for (int i = 0; i < n; i++) {
      float iv = rx_interleaved[i * 2];
      float qv = rx_interleaved[i * 2 + 1];
      rx_chunk_i[i] = iv;
      rx_chunk_q[i] = qv;
      sum_sq += iv * iv + qv * qv;
    }
    rx_signal_strength = sqrtf(sum_sq / (float)n);
    lime_iq_buffer_write(rx_chunk_i, rx_chunk_q, (uint32_t)n);

    // Wake a blocked get-samples if enough data is now available.
    lbm_mutex_lock(&rx_wait_mutex);
    if (rx_waiting_cid >= 0) {
      if (lime_iq_buffer_read(rx_waiting_i, rx_waiting_q, rx_waiting_n, &gp_reader)) {
        lbm_cid cid = rx_waiting_cid;
        rx_waiting_cid = -1;
        lbm_mutex_unlock(&rx_wait_mutex);
        lbm_unblock_ctx_r(cid);
        continue;
      }
    }
    lbm_mutex_unlock(&rx_wait_mutex);
  }
}

// ////////////////////////////////////////////////////////////
// TX worker thread — interleave already done by ext, just stream

static void tx_thd(void *arg) {
  (void)arg;

  lms_stream_t tx_stream;
  memset(&tx_stream, 0, sizeof(tx_stream));
  tx_stream.channel               = 0;
  tx_stream.fifoSize              = 1024 * 1024;
  tx_stream.throughputVsLatency   = 0.5f;
  tx_stream.isTx                  = true;
  tx_stream.dataFmt               = LMS_FMT_F32;

  lbm_cid cid = tx_waiting_cid;
  bool ok = false;

  if (dev && LMS_SetupStream(dev, &tx_stream) == 0) {
    LMS_StartStream(&tx_stream);
    lms_stream_meta_t meta;
    memset(&meta, 0, sizeof(meta));
    meta.flushPartialPacket = true;
    int sent = LMS_SendStream(&tx_stream, tx_interleaved, tx_n_samples, &meta, 5000);
    ok = (sent > 0);
    LMS_StopStream(&tx_stream);
    LMS_DestroyStream(dev, &tx_stream);
  }

  free(tx_interleaved);
  tx_interleaved = NULL;

  lbm_unblock_ctx_unboxed(cid, ok ? ENC_SYM_TRUE : ENC_SYM_NIL);
}

// ////////////////////////////////////////////////////////////
// LBM extensions

static lbm_value ext_limesdr_get_device_count(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  lms_info_str_t list[16];
  int n = LMS_GetDeviceList(list);
  return lbm_enc_i(n < 0 ? 0 : n);
}

static lbm_value ext_limesdr_open(lbm_value *args, lbm_uint argn) {
  uint32_t index = 0;
  if (argn >= 1 && lbm_is_number(args[0]))
    index = lbm_dec_as_u32(args[0]);

  lms_info_str_t list[16];
  int n = LMS_GetDeviceList(list);
  if (n <= 0 || (int)index >= n) return ENC_SYM_NIL;

  if (LMS_Open(&dev, list[index], NULL) != 0) return ENC_SYM_NIL;
  if (LMS_Init(dev) != 0) { LMS_Close(dev); dev = NULL; return ENC_SYM_NIL; }
  return ENC_SYM_TRUE;
}

static lbm_value ext_limesdr_close(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  if (!dev) return ENC_SYM_NIL;
  if (rx_running) {
    rx_running = false;
    lbm_thread_destroy(&rx_thread);
    LMS_StopStream(&rx_stream);
    LMS_DestroyStream(dev, &rx_stream);
  }
  LMS_Close(dev);
  dev = NULL;
  return ENC_SYM_TRUE;
}

static lbm_value ext_limesdr_set_sample_rate(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  if (!dev) return ENC_SYM_EERROR;
  double rate = (double)lbm_dec_as_float(args[0]);
  return LMS_SetSampleRate(dev, rate, 0) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_limesdr_set_rx_freq(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  if (!dev) return ENC_SYM_EERROR;
  double freq = (double)lbm_dec_as_float(args[0]);
  return LMS_SetLOFrequency(dev, LMS_CH_RX, 0, freq) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_limesdr_set_tx_freq(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  if (!dev) return ENC_SYM_EERROR;
  double freq = (double)lbm_dec_as_float(args[0]);
  return LMS_SetLOFrequency(dev, LMS_CH_TX, 0, freq) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_limesdr_set_rx_gain(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  if (!dev) return ENC_SYM_EERROR;
  unsigned int gain = (unsigned int)lbm_dec_as_u32(args[0]);
  return LMS_SetGaindB(dev, LMS_CH_RX, 0, gain) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_limesdr_set_tx_gain(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  if (!dev) return ENC_SYM_EERROR;
  unsigned int gain = (unsigned int)lbm_dec_as_u32(args[0]);
  return LMS_SetGaindB(dev, LMS_CH_TX, 0, gain) == 0 ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_limesdr_start_rx(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  if (!dev || rx_running) return ENC_SYM_NIL;

  LMS_EnableChannel(dev, LMS_CH_RX, 0, true);

  memset(&rx_stream, 0, sizeof(rx_stream));
  rx_stream.channel             = 0;
  rx_stream.fifoSize            = 1024 * 1024;
  rx_stream.throughputVsLatency = 0.5f;
  rx_stream.isTx                = false;
  rx_stream.dataFmt             = LMS_FMT_F32;

  if (LMS_SetupStream(dev, &rx_stream) != 0) return ENC_SYM_NIL;
  LMS_StartStream(&rx_stream);

  rx_running = true;
  if (!lbm_thread_create(&rx_thread, "limesdr_rx", rx_thd, NULL, LBM_THREAD_PRIO_NORMAL, 8192)) {
    rx_running = false;
    LMS_StopStream(&rx_stream);
    LMS_DestroyStream(dev, &rx_stream);
    return ENC_SYM_NIL;
  }
  return ENC_SYM_TRUE;
}

static lbm_value ext_limesdr_stop_rx(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  if (!rx_running) return ENC_SYM_NIL;
  rx_running = false;
  lbm_thread_destroy(&rx_thread);
  LMS_StopStream(&rx_stream);
  LMS_DestroyStream(dev, &rx_stream);
  return ENC_SYM_TRUE;
}

static lbm_value ext_limesdr_signal_strength(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return lbm_enc_float(rx_running ? rx_signal_strength : -1.0f);
}

// Blocking: suspends until N samples are available in the circular buffer.
// Pre-allocates result arrays before blocking so they are GC-reachable
// via the blocked context's result register.
static lbm_value ext_limesdr_get_samples(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  uint32_t n = lbm_dec_as_u32(args[0]);
  if (n == 0 || n >= IQ_BUFFER_SIZE) return ENC_SYM_TERROR;

  lbm_value i_arr, q_arr;
  if (!lbm_heap_allocate_array(&i_arr, n * sizeof(float))) return ENC_SYM_MERROR;
  if (!lbm_heap_allocate_array(&q_arr, n * sizeof(float))) return ENC_SYM_MERROR;

  lbm_array_header_t *ih = (lbm_array_header_t*)lbm_car(i_arr);
  lbm_array_header_t *qh = (lbm_array_header_t*)lbm_car(q_arr);
  float *ibuf = (float*)ih->data;
  float *qbuf = (float*)qh->data;

  // Fast path: data already available.
  if (lime_iq_buffer_read(ibuf, qbuf, n, &gp_reader)) {
    return lbm_cons(i_arr, q_arr);
  }

  // Slow path: block until the RX thread has enough data.
  lbm_value result = lbm_cons(i_arr, q_arr);
  if (lbm_is_symbol_merror(result)) return ENC_SYM_MERROR;

  lbm_mutex_lock(&rx_wait_mutex);
  rx_waiting_cid = lbm_get_current_cid();
  rx_waiting_n   = n;
  rx_waiting_i   = ibuf;
  rx_waiting_q   = qbuf;
  lbm_mutex_unlock(&rx_wait_mutex);

  lbm_block_ctx_from_extension();
  return result; // goes into result register; unblock_ctx_r resumes with it
}

// Blocking: interleaves I/Q arrays, spawns TX worker thread, suspends until done.
static lbm_value ext_limesdr_tx_send(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_array_r(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  if (!dev) return ENC_SYM_EERROR;

  lbm_mutex_lock(&tx_mutex);
  if (tx_waiting_cid >= 0) {
    // A TX is already in progress.
    lbm_mutex_unlock(&tx_mutex);
    lbm_set_error_reason("limesdr-tx-send: TX already in progress");
    return ENC_SYM_EERROR;
  }

  lbm_array_header_t *ih = lbm_dec_array_r(args[0]);
  lbm_array_header_t *qh = lbm_dec_array_r(args[1]);
  if (!ih || !qh) { lbm_mutex_unlock(&tx_mutex); return ENC_SYM_TERROR; }

  uint32_t ni = (uint32_t)(ih->size / sizeof(float));
  uint32_t nq = (uint32_t)(qh->size / sizeof(float));
  uint32_t n  = ni < nq ? ni : nq;
  if (n == 0) { lbm_mutex_unlock(&tx_mutex); return ENC_SYM_TERROR; }

  // Interleave into a malloc'd buffer owned by the TX thread.
  float *interleaved = (float*)malloc(n * 2 * sizeof(float));
  if (!interleaved) { lbm_mutex_unlock(&tx_mutex); return ENC_SYM_MERROR; }

  float *isrc = (float*)ih->data;
  float *qsrc = (float*)qh->data;
  for (uint32_t i = 0; i < n; i++) {
    interleaved[i * 2]     = isrc[i];
    interleaved[i * 2 + 1] = qsrc[i];
  }

  tx_interleaved  = interleaved;
  tx_n_samples    = n;
  tx_waiting_cid  = lbm_get_current_cid();
  lbm_mutex_unlock(&tx_mutex);

  if (!lbm_thread_create(&tx_thread, "limesdr_tx", tx_thd, NULL, LBM_THREAD_PRIO_NORMAL, 8192)) {
    free(interleaved);
    tx_interleaved = NULL;
    tx_waiting_cid = -1;
    return ENC_SYM_EERROR;
  }

  lbm_block_ctx_from_extension();
  return ENC_SYM_TRUE;
}

// ////////////////////////////////////////////////////////////
// Init

void lbm_limesdr_init(void) {
  dev       = NULL;
  rx_running = false;

  lime_iq_buffer_create(IQ_BUFFER_SIZE);
  lime_iq_buffer_add_reader(&gp_reader);

  lbm_mutex_init(&iq_mutex);
  lbm_mutex_init(&rx_wait_mutex);
  lbm_mutex_init(&tx_mutex);

  rx_waiting_cid = -1;
  tx_waiting_cid = -1;
  tx_interleaved = NULL;

  lbm_add_extension("limesdr-get-device-count", ext_limesdr_get_device_count);
  lbm_add_extension("limesdr-open",             ext_limesdr_open);
  lbm_add_extension("limesdr-close",            ext_limesdr_close);
  lbm_add_extension("limesdr-set-sample-rate",  ext_limesdr_set_sample_rate);
  lbm_add_extension("limesdr-set-rx-freq",      ext_limesdr_set_rx_freq);
  lbm_add_extension("limesdr-set-tx-freq",      ext_limesdr_set_tx_freq);
  lbm_add_extension("limesdr-set-rx-gain-db",   ext_limesdr_set_rx_gain);
  lbm_add_extension("limesdr-set-tx-gain-db",   ext_limesdr_set_tx_gain);
  lbm_add_extension("limesdr-start-rx",         ext_limesdr_start_rx);
  lbm_add_extension("limesdr-stop-rx",          ext_limesdr_stop_rx);
  lbm_add_extension("limesdr-signal-strength",  ext_limesdr_signal_strength);
  lbm_add_extension("limesdr-get-samples",      ext_limesdr_get_samples);
  lbm_add_extension("limesdr-tx-send",          ext_limesdr_tx_send);
}
