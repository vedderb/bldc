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

#include "lbm_rtlsdr.h"
#include "extensions.h"
#include "lbm_c_interop.h"
#include "platform_thread.h"
#include "platform_timestamp.h"

#include <rtl-sdr.h>
#include "lbm_sound.h"
#include <math.h>


// Circular I/Q data buffer
#define MAX_IQ_READERS 5

typedef struct {
  uint32_t read_pos;
} iq_reader_t;

static float *iq_buffer_i;
static float *iq_buffer_q;
static uint32_t iq_buffer_size;
static uint32_t iq_buffer_write_pos;
static iq_reader_t *iq_buffer_readers[MAX_IQ_READERS];
static lbm_mutex_t iq_buffer_mutex;

static bool iq_buffer_create(uint32_t size) {
  if (iq_buffer_i) free(iq_buffer_i);
  if (iq_buffer_q) free(iq_buffer_q);
  iq_buffer_i = (float*)malloc(size * sizeof(float));
  iq_buffer_q = (float*)malloc(size * sizeof(float));
  iq_buffer_size = size;
  iq_buffer_write_pos = 0;

  for (int i = 0; i < MAX_IQ_READERS; i ++)  {
    iq_buffer_readers[i] = NULL;
  }

  bool res = true;
  if (!iq_buffer_i || !iq_buffer_q) {
    if (iq_buffer_i) free(iq_buffer_i);
    if (iq_buffer_q) free(iq_buffer_q);
    iq_buffer_i = NULL;
    iq_buffer_q = NULL;
    res = false;
  }
  return res;
}

static bool iq_buffer_add_reader(iq_reader_t *reader) {
  lbm_mutex_lock(&iq_buffer_mutex);
  bool res = false;
  int free_pos = -1;
  for (int i = 0; i < MAX_IQ_READERS; i ++) {
    if (iq_buffer_readers[i] == reader) {
      lbm_mutex_unlock(&iq_buffer_mutex);
      return true;
    }
    if (iq_buffer_readers[i] == NULL) {
      free_pos = i;
    }
  }
  if (free_pos >= 0) {
    iq_buffer_readers[free_pos] = reader;
    res = true;
  }
  lbm_mutex_unlock(&iq_buffer_mutex);
  return res;
}

static bool iq_buffer_drop_reader(iq_reader_t *reader) {
  lbm_mutex_lock(&iq_buffer_mutex);
  for (int i = 0; i < MAX_IQ_READERS; i ++) {
    if (iq_buffer_readers[i] == reader) {
      iq_buffer_readers[i] = NULL;
      lbm_mutex_unlock(&iq_buffer_mutex);
      return true;
    }
  }
  lbm_mutex_unlock(&iq_buffer_mutex);
  return false;
}


static void iq_buffer_write(float *i_data, float *q_data, uint32_t n) {
  if (n < iq_buffer_size) {
    lbm_mutex_lock(&iq_buffer_mutex);
    uint32_t old = iq_buffer_write_pos;

    if (iq_buffer_write_pos + n >= iq_buffer_size) { // do as 2 writes.
      uint32_t s1 = iq_buffer_size - iq_buffer_write_pos;
      uint32_t s2 = n - s1;
      // write from write_pos to end
      memcpy(&iq_buffer_i[iq_buffer_write_pos], i_data, s1 * sizeof(float));
      memcpy(&iq_buffer_q[iq_buffer_write_pos], q_data, s1 * sizeof(float));
      // write from 0 to s2
      memcpy(iq_buffer_i, &i_data[s1], s2 * sizeof(float));
      memcpy(iq_buffer_q, &q_data[s1], s2 * sizeof(float));
      iq_buffer_write_pos = s2;
    } else { // do as one write
      memcpy(&iq_buffer_i[iq_buffer_write_pos], i_data, n * sizeof(float));
      memcpy(&iq_buffer_q[iq_buffer_write_pos], q_data, n * sizeof(float));
      iq_buffer_write_pos = (iq_buffer_write_pos + n) % iq_buffer_size;
    }

    // if a reader's read position was in the range old_write_pos to new_write_pos
    // then it has been overwritten with new data.
    // The new data is written starting at position old_write_pos.
    // So to move the reader to known good data is to move it back to position old_write_pos
    // where we just wrote the fresh data.

    // there are a couple of cases
    // -------|-----|-----|----------- end
    //        old   r     new
    //
    // -------|-----|-----|----------- end
    //        new   r     old
    //
    // -------|-----|-----|----------- end
    //        new   old   r
    //
    // -------|-----|-----|----------- end
    //        old  new    r
    //
    // -------|-----|-----|----------- end
    //        r    new   old
    //
    // -------|-----|-----|----------- end
    //        r     old  new

    //  a couple of cases where r shoudl be bumpled
    //  1. if (old < new && r >= old && r < new)
    //  2. if (old > new && r < new)
    //  3. if (new < old && r >= old)
    //
    //  Combine 2 and 3 => if (old > new && (r < new || r >= old))
    //  Gives the following condition to bump a reader pos back to old
    //   if ((old < new && r >= old && r < new) ||
    //       (old > new && (r < new || r >= old)))
    //

    uint32_t new = iq_buffer_write_pos; // where to start writing next time.

    for (int i = 0; i < MAX_IQ_READERS; i ++) {
      if (iq_buffer_readers[i]) {
        uint32_t rp = iq_buffer_readers[i]->read_pos;
        if ((old < new && rp >= old && rp < new) ||
            (old > new && (rp < new || rp >= old))) {
          iq_buffer_readers[i]->read_pos = old;
        }
      }
    }
    lbm_mutex_unlock(&iq_buffer_mutex);
  }
}

static bool iq_buffer_read(float *i_data, float *q_data, int n, iq_reader_t *reader) {

  bool res = false;
  if (n < iq_buffer_size) {
    lbm_mutex_lock(&iq_buffer_mutex);
    uint32_t rp = reader->read_pos;
    uint32_t wp = iq_buffer_write_pos;

    if (wp >= rp) {
      uint32_t avail = wp - rp;
      if (avail >= n) {
        memcpy(i_data, &iq_buffer_i[rp], n * sizeof(float));
        memcpy(q_data, &iq_buffer_q[rp], n * sizeof(float));
        reader->read_pos = rp + n;
        res = true;
      }
    } else {
      uint32_t avail = (iq_buffer_size - rp) + wp;
      if (avail >= n) {
        if (rp + n < iq_buffer_size) {
          memcpy(i_data, &iq_buffer_i[rp], n * sizeof(float));
          memcpy(q_data, &iq_buffer_q[rp], n * sizeof(float));
          reader->read_pos = rp + n;
          res = true;
        } else {
          uint32_t l = iq_buffer_size - rp;
          uint32_t m = n - l;
          memcpy(i_data, &iq_buffer_i[rp], l * sizeof(float));
          memcpy(q_data, &iq_buffer_q[rp], l * sizeof(float));

          memcpy(&i_data[l], iq_buffer_i, m * sizeof(float));
          memcpy(&q_data[l], iq_buffer_q, m * sizeof(float));
          reader->read_pos = m;
          res = true;
        }
      }
    }
    lbm_mutex_unlock(&iq_buffer_mutex);
  }
  return res;
}

// supports having a single device open.
static rtlsdr_dev_t *dev = NULL;

static volatile bool radio_thread_running = false;
static lbm_thread_t radio_thread;

static lbm_uint sym_gain_auto;
static lbm_uint sym_gain_manual;
static lbm_uint sym_agc_on;
static lbm_uint sym_agc_off;

static lbm_value ext_rtlsdr_open(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t ix = lbm_dec_as_u32(args[0]);
    if (rtlsdr_open(&dev, ix) == 0) {
      r = ENC_SYM_TRUE;
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_close(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_NIL;
    if (dev) {
      // Stop radio thread first if running
      if (radio_thread_running) {
        radio_thread_running = false;
        // Thread will call rtlsdr_cancel_async and exit
        lbm_thread_destroy(&radio_thread);
      }
      rtlsdr_close(dev);
      dev = NULL;
      r = ENC_SYM_TRUE;
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_get_device_count(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;

  if (argn == 0) {
    uint32_t num_devices = rtlsdr_get_device_count();
    r = lbm_enc_i((int32_t)num_devices);
  }
  return r;
}


static lbm_value ext_rtlsdr_get_device_name(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;

  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t ix = lbm_dec_as_u32(args[0]);
    const char * device_name = rtlsdr_get_device_name(ix);
    if (device_name) {
      lbm_uint n = strlen(device_name);
      r = ENC_SYM_NIL;
      if (n > 0) {
        lbm_value res_arr;
        r = ENC_SYM_MERROR;
        if (lbm_create_array(&res_arr, n+1)) {
          lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res_arr);
          memcpy(arr->data, device_name, n);
          ((char*)(arr->data))[n] = '\0';
          r = res_arr;
        }
      }
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_set_center_freq(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t freq = lbm_dec_as_u32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (rtlsdr_set_center_freq(dev, freq) == 0) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_get_center_freq(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_EERROR;
    if (dev) {
      uint32_t freq = rtlsdr_get_center_freq(dev);
      r = lbm_enc_u32(freq);
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_set_sample_rate(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    uint32_t rate = lbm_dec_as_u32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (rtlsdr_set_sample_rate(dev, rate) == 0) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_get_sample_rate(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_EERROR;
    if (dev) {
      uint32_t rate = rtlsdr_get_sample_rate(dev);
      r = lbm_enc_u32(rate);
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_set_tuner_gain_mode(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_symbol(args[0])) {
    int32_t mode = lbm_dec_sym(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (mode == sym_gain_manual) {
        if (rtlsdr_set_tuner_gain_mode(dev, 1) == 0) {
          r = ENC_SYM_TRUE;
        }
      } else if (mode == sym_gain_auto) {
        if (rtlsdr_set_tuner_gain_mode(dev, 0) == 0) {
          r = ENC_SYM_TRUE;
        }
      }
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_set_tuner_gain(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    int32_t gain = lbm_dec_as_i32(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (rtlsdr_set_tuner_gain(dev, gain) == 0) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_get_tuner_gain(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_EERROR;
    if (dev) {
      int32_t gain = rtlsdr_get_tuner_gain(dev);
      r = lbm_enc_i32(gain);
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_set_agc_mode(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_symbol(args[0])) {
    int32_t mode = lbm_dec_sym(args[0]);
    r = ENC_SYM_EERROR;
    if (dev) {
      r = ENC_SYM_NIL;
      if (mode == sym_agc_on) {
        if (rtlsdr_set_agc_mode(dev, 1) == 0) {
          r = ENC_SYM_TRUE;
        }
      } else if (mode == sym_agc_off) {
        if (rtlsdr_set_agc_mode(dev, 0) == 0) {
          r = ENC_SYM_TRUE;
        }
      }
    }
  }
  return r;
}

// Towards FM demodulation

static float i_data[262144];
static float q_data[262144];
static float audio[262144];
static int16_t sound_samples[262144]; // way more than needed

static uint32_t sample_rate = 0;
static snd_pcm_uframes_t period_size = 0;
static snd_pcm_uframes_t buffer_size = 0;
static uint32_t downsample_ratio = 0;
static uint32_t last_callback_time = 0;

static float prev_i = 0.0f;
static float prev_q = 0.0f;

static void rtlsdr_callback(unsigned char *buf, uint32_t len, void *ctx) {
  (void)ctx;
  if (!radio_thread_running) {
    rtlsdr_cancel_async(dev);
    return;
  }

  if (sample_rate == 0) return; // abort if sample rate is not supported.

  uint32_t n = len / 2;

  // The FM audio we pick up from the air
  // consists of a carrier frequency (the station frequency 107.1MHz for example).
  // The audio wave is used to modulate (dynamically change) the carrier freq within
  // a range of +- 75kHz for example. The amplitude of the audio at time t
  // is used to modulate the transmitted frequency as carrier_freq + audio_normalized(t) * 75kHz.

  // When we tune the rtlsdr to 107.1MHz the
  // SDR downmixes the incomming signal with 2 107.1MHz signals 90 degree apart (cos/sin essentially).
  //
  //  modulated signal -> downmixing cos -> low pass filter ->  i-samples
  //                   \
  //                    -> downmixing sin -> low pass filter -> q-samples
  //
  // Note: Downmixing is a signal multiplication operation.

  // The demodulator or discriminator is still quite mysterious to me.
  // The difference in angle between <I(n), Q(n)> and <I(n+1), Q(n+1)> vectors
  // is computed via a 2D cross product, which is the name used for what is really
  // a 3D cross product where the z fields are 0, The result is a vector <0,0,z>
  // where the z is large for high frequency and low for low frequency.
  // (The difference between two consecutive measurements is larger if the frequency is high.)
  // The result of the cross_prod, z, is the audio (or at least it is proportional to the
  // audio) encoded on the fm wave.

  // More detail the cross_prod(prev,curr) = sin(theta) * |prev| * |curr|
  // and sin(theta) ~ theta for small theta
  // In the discriminator there is no need to use asin.
  //
  // There is a scaling of the theta with |prev| * |curr| that
  // could be normalized away if one wants..
  // A measurement of signal strength is sqrt(I^2 + Q^2) so the
  // scaling with |prev| * |curr| is in a way just a scaling related to signal strength.
  // So the effect of that is that we get more volume when signal is good.

  for (int i = 0; i < n; i ++) {
    float curr_i = ((float)buf[i * 2] - 127.5) / 128.0f;
    float curr_q = ((float)buf[(i * 2) + 1] - 127.5) / 128.0f;
    audio[i] = curr_q * prev_i - curr_i * prev_q;

    prev_i = curr_i;
    prev_q = curr_q;
  }

  // but we need a whole number of samples to give to alsa.
  // Maybe a float downsample_ratio is more correct
  // but would require some interpolation to make
  // correct amount of discrete samples.
  uint32_t n_samples =  n / downsample_ratio; // This one should be integer too, or similar situation to above.

  for (int i = 0; i < n_samples; i ++) {
    float sum = 0.0f;

    int start = i * downsample_ratio;
    int filter_len = downsample_ratio * 2;
    if (start + filter_len > n) filter_len = n - start;

    for (int j = 0; j < filter_len; j ++) {
      sum += audio[start + j];
    }
    float avg = sum / filter_len;
    float out = avg * 3.0f;

    if (out > 0.9f) out = 0.9f + 0.1f * (out - 0.9f) / (1.0f + fabsf(out - 0.9f));
    if (out < -0.9f) out = -0.9f + 0.1f * (out + 0.9f) / (1.0f + fabsf(out + 0.9f));

    sound_samples[i*2] = out * 32767.0;
    sound_samples[i*2+1] = sound_samples[i*2];
  }

  snd_pcm_t * pcmh = lbm_sound_pcm_handle();
  if (pcmh) {
    snd_pcm_sframes_t avail = snd_pcm_avail(pcmh);
    snd_pcm_state_t state = snd_pcm_state(pcmh);
    snd_pcm_sframes_t delay = 0;
    snd_pcm_delay(pcmh, &delay);

    snd_pcm_sframes_t frames = snd_pcm_writei(pcmh, sound_samples, n_samples);
    if (frames < 0) {
      if (frames == -EPIPE) {
        printf("Buffer underrun!\n");
        snd_pcm_prepare(pcmh);
        snd_pcm_writei(pcmh, sound_samples, n_samples);
      } else if (frames == -EAGAIN) {
        // Buffer full, drop this buffer to avoid blocking
        printf("ALSA buffer full, dropping samples\n");
      } else {
        printf("Write error: %s\n", snd_strerror(frames));
      }
    } else if (frames < n_samples) {
      printf("Partial write: %ld/%u samples\n", frames, n_samples);
    }
  }
}

static volatile bool rtlsdr_sampling_on = false;
static lbm_thread_t rtlsdr_sampling_thread;
static float rtlsdr_signal_strength = 0.0f;


static void rtlsdr_normalize_and_buffer_callback(unsigned char *buf, uint32_t len, void *ctx) {
  (void)ctx;
  if (!rtlsdr_sampling_on) {
    rtlsdr_cancel_async(dev);
    return;
  }
  int n = len / 2;

  float sum_sq = 0.0;
  
  for (int i = 0; i < n; i ++) {
    float i_val = ((float)buf[i * 2] - 127.5) / 128.0f;
    float q_val = ((float)buf[(i * 2) + 1] - 127.5) / 128.0f;
    i_data[i] = i_val;
    q_data[i] = q_val;
    sum_sq += i_val*i_val + q_val*q_val;
  }
  iq_buffer_write(i_data, q_data, n);
  rtlsdr_signal_strength = sqrt(sum_sq / n);
}

static void rtlsdr_sampling_thd(void *arg) {
  (void) arg;
  uint32_t rtlsdr_buffer_num = 8;
  uint32_t rtlsdr_buffer_size = 65536;
  // This thread dies if sampling is set to off.
  rtlsdr_sampling_on = true;

  rtlsdr_reset_buffer(dev);
  rtlsdr_read_async(dev,
                    rtlsdr_normalize_and_buffer_callback,
                    NULL,
                    rtlsdr_buffer_num,
                    rtlsdr_buffer_size);
}

static lbm_value ext_rtlsdr_start_sampling(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_NIL;
    if (dev && !rtlsdr_sampling_on) { // Starting thread requires a device
      if (lbm_thread_create(&rtlsdr_sampling_thread, "sampling_thd", rtlsdr_sampling_thd,
                            NULL, LBM_THREAD_PRIO_NORMAL, 8192)) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_stop_sampling(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_NIL;
    if (rtlsdr_sampling_on) {
      rtlsdr_sampling_on = false;
      lbm_thread_destroy(&rtlsdr_sampling_thread);
      r = ENC_SYM_TRUE;
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_signal_strength(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    if (rtlsdr_sampling_on) {
      r = lbm_enc_float(rtlsdr_signal_strength);
    } else {
      r = lbm_enc_float(-1.0f);
    }
  }
  return r;        
}

static volatile bool fm_playback_thread_running = false;
static lbm_thread_t fm_playback_thread;
static iq_reader_t fm_playback_reader;

static void fm_playback_thd(void *arg) {

  fm_playback_thread_running = true;

  iq_buffer_add_reader(&fm_playback_reader);

  // TODO: what do we do if the sample rate is changed while the playback thread is running?
  uint32_t sample_rate = rtlsdr_get_sample_rate(dev);
  uint32_t audio_sample_rate = 44100;
  uint32_t downsample  = sample_rate / audio_sample_rate;

  snd_pcm_t *pcmh = lbm_sound_pcm_handle();
  if (!pcmh) {
    fprintf(stderr, "FM playback: ALSA handle is NULL\n");
    fm_playback_thread_running = false;
    iq_buffer_drop_reader(&fm_playback_reader);
    return;
  }

  // Try to recover ALSA device if it's in a bad state
  snd_pcm_state_t state = snd_pcm_state(pcmh);
  if (state != SND_PCM_STATE_OPEN && state != SND_PCM_STATE_SETUP) {
    fprintf(stderr, "FM playback: ALSA in state %d, attempting drain/drop\n", state);
    snd_pcm_drop(pcmh);
  }

  int err = snd_pcm_set_params(pcmh,
                               SND_PCM_FORMAT_S16_LE,
                               SND_PCM_ACCESS_RW_INTERLEAVED,
                               2,       // channels
                               audio_sample_rate,
                               1,
                               300000); // latency
  if (err < 0) {
    fprintf(stderr, "FM playback: Cannot set audio parameters: %s\n", snd_strerror(err));
    fprintf(stderr, "FM playback: ALSA state was: %d\n", state);
    fm_playback_thread_running = false;
    iq_buffer_drop_reader(&fm_playback_reader);
    return;
  }

  // Set non-blocking mode
  err = snd_pcm_nonblock(pcmh, 1);
  if (err < 0) {
    fprintf(stderr, "FM playback: Cannot set non-blocking mode: %s\n", snd_strerror(err));
  } else {
    printf("FM playback: ALSA set to non-blocking mode\n");
  }

  float i_buf[8192];
  float q_buf[8192];
  float audio[8192];
  float prev_i = 0.0;
  float prev_q = 0.0;
  int n = 8192;

  int16_t sound_samples[8192];

  while (fm_playback_thread_running) {

    while (!iq_buffer_read(i_buf,q_buf, n, &fm_playback_reader)) {
      lbm_thread_sleep_us(100);
    }

    for (int i = 0; i < n; i ++) {
      float curr_q = q_buf[i];
      float curr_i = i_buf[i];
      audio[i] = curr_q * prev_i - curr_i * prev_q;

      prev_i = curr_i;
      prev_q = curr_q;
    }

    uint32_t n_samples =  n / downsample;
    for (int i = 0; i < n_samples; i ++) {
      float sum = 0.0f;
      int start = i * downsample;
      int filter_len = downsample * 2;
      if (start + filter_len > n) filter_len = n - start;

      for (int j = 0; j < filter_len; j ++) {
        sum += audio[start + j];
      }
      float avg = sum / filter_len;
      float out = avg * 3.0f;

      if (out > 0.9f) out = 0.9f + 0.1f * (out - 0.9f) / (1.0f + fabsf(out - 0.9f));
      if (out < -0.9f) out = -0.9f + 0.1f * (out + 0.9f) / (1.0f + fabsf(out + 0.9f));

      sound_samples[i*2] = out * 32767.0;
      sound_samples[i*2+1] = sound_samples[i*2];
    }
    if (pcmh) {
      snd_pcm_sframes_t frames = snd_pcm_writei(pcmh, sound_samples, n_samples);
      if (frames < 0) {
        if (frames == -EPIPE) {
          printf("Buffer underrun!\n");
          snd_pcm_prepare(pcmh);
          snd_pcm_writei(pcmh, sound_samples, n_samples);
        } else if (frames == -EAGAIN) {
          // Buffer full, drop this buffer to avoid blocking
          printf("ALSA buffer full, dropping samples\n");
        } else {
          printf("Write error: %s\n", snd_strerror(frames));
        }
      } else if (frames < n_samples) {
        printf("Partial write: %ld/%u samples\n", frames, n_samples);
      }
    }
  }

  iq_buffer_drop_reader(&fm_playback_reader);
}


static lbm_value ext_rtlsdr_start_fm_playback(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_NIL;
    if (dev && !fm_playback_thread_running) { // Starting thread requires a device
      if (lbm_thread_create(&fm_playback_thread, "fm_playback_thd", fm_playback_thd,
                            NULL, LBM_THREAD_PRIO_NORMAL, 8192)) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

static lbm_value ext_rtlsdr_stop_fm_playback(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_NIL;
    if (fm_playback_thread_running) {
      fm_playback_thread_running = false;
      lbm_thread_destroy(&fm_playback_thread);
      r = ENC_SYM_TRUE;
    }
  }
  return r;
}

// ------------------------------------------------------------
// General purpose reading

static iq_reader_t gp_reader;


static lbm_value ext_rtlsdr_get_samples(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_number(args[0])) {

    uint32_t n = lbm_dec_as_u32(args[0]);
    r = ENC_SYM_EERROR;
    if (n > 0) {
      lbm_value i_array;
      lbm_value q_array;
      if (lbm_heap_allocate_array(&i_array, (n * sizeof(float))) &&
          lbm_heap_allocate_array(&q_array, (n * sizeof(float)))) {
        lbm_array_header_t *i_array_h = (lbm_array_header_t*)lbm_car(i_array);
        lbm_array_header_t *q_array_h = (lbm_array_header_t*)lbm_car(q_array);
        float *i_buf = (float*)i_array_h->data;
        float *q_buf = (float*)q_array_h->data;
        iq_buffer_read(i_buf, q_buf, n, &gp_reader);
        r = lbm_cons(i_array, q_array);
      } else {
        r = ENC_SYM_MERROR;
      }
    }
  }
  return r;
}



// ------------------------------------------------------------
// Old radio thread
static void radio_thd(void *arg) {
  (void)arg;

  last_callback_time = 0;
  uint32_t rtlsdr_buffer_size = 131072;
  uint32_t rtlsdr_buffer_num = 8;

  sample_rate = rtlsdr_get_sample_rate(dev);
  printf("RTLSDR sample rate: %u\n", sample_rate);

  downsample_ratio = sample_rate / 44100;
  printf("Audio Downsample ratio: %u\n", downsample_ratio);

  printf("RTLSDR buffer size: %u\n", rtlsdr_buffer_size);
  printf("RTLSDR num buffers: %u\n", rtlsdr_buffer_num);

  snd_pcm_t *pcmh = lbm_sound_pcm_handle();

  int err = snd_pcm_set_params(pcmh,
                               SND_PCM_FORMAT_S16_LE,
                               SND_PCM_ACCESS_RW_INTERLEAVED,
                               2,       // channels
                               44100,   // sample rate
                               1,
                               300000); // latency
  if (err < 0) {
    fprintf(stderr, "Cannot set audio parameters: %s\n", snd_strerror(err));
    return;
  }


  // Set ALSA to non-blocking mode to prevent blocking callbacks
  snd_pcm_nonblock(pcmh, 1);
  printf("ALSA set to non-blocking mode\n");

  snd_pcm_hw_params_t *hw_params;
  snd_pcm_hw_params_malloc(&hw_params);
  snd_pcm_hw_params_current(pcmh, hw_params);
  snd_pcm_hw_params_get_period_size(hw_params, &period_size, NULL);
  snd_pcm_hw_params_get_buffer_size(hw_params, &buffer_size);
  printf("ALSA period size: %lu samples\n", period_size);
  printf("ALSA buffer size: %lu samples\n", buffer_size);

  rtlsdr_reset_buffer(dev);
  rtlsdr_read_async(dev, rtlsdr_callback, NULL, rtlsdr_buffer_num, rtlsdr_buffer_size);
}

lbm_value ext_rtlsdr_start_radio_thd(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_NIL;
    if (dev && !radio_thread_running) { // Starting thread requires a device
      radio_thread_running = true;
      if (lbm_thread_create(&radio_thread, "radio_thd", radio_thd,
                            NULL, LBM_THREAD_PRIO_NORMAL, 8192)) {
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

lbm_value ext_rtlsdr_stop_radio_thd(lbm_value *args, lbm_uint argn) {
  (void) args;
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 0) {
    r = ENC_SYM_NIL;
    if (radio_thread_running) {
      radio_thread_running = false;
      lbm_thread_destroy(&radio_thread);
      r = ENC_SYM_TRUE;
    }
  }
  return r;
}


void lbm_rtlsdr_init(void) {

  dev = NULL;

  iq_buffer_create(262144);
  iq_buffer_add_reader(&gp_reader);

  lbm_mutex_init(&iq_buffer_mutex);
  
  radio_thread_running = false;

  lbm_add_symbol_const("gain-mode-auto", &sym_gain_auto);
  lbm_add_symbol_const("gain-mode-manual", &sym_gain_manual);
  lbm_add_symbol_const("agc-on", &sym_agc_on);
  lbm_add_symbol_const("agc-off", &sym_agc_off);

  lbm_add_extension("rtlsdr-open", ext_rtlsdr_open);
  lbm_add_extension("rtlsdr-close", ext_rtlsdr_close);
  lbm_add_extension("rtlsdr-get-device-count", ext_rtlsdr_get_device_count);
  lbm_add_extension("rtlsdr-get-device-name", ext_rtlsdr_get_device_name);
  lbm_add_extension("rtlsdr-set-center-freq", ext_rtlsdr_set_center_freq);
  lbm_add_extension("rtlsdr-get-center-freq", ext_rtlsdr_get_center_freq);
  lbm_add_extension("rtlsdr-set-sample-rate", ext_rtlsdr_set_sample_rate);
  lbm_add_extension("rtlsdr-get-sample-rate", ext_rtlsdr_get_sample_rate);
  lbm_add_extension("rtlsdr-set-tuner-gain-mode", ext_rtlsdr_set_tuner_gain_mode);
  lbm_add_extension("rtlsdr-set-tuner-gain", ext_rtlsdr_set_tuner_gain);
  lbm_add_extension("rtlsdr-get-tuner-gain", ext_rtlsdr_get_tuner_gain);
  lbm_add_extension("rtlsdr-set-agc-mode", ext_rtlsdr_set_agc_mode);

  lbm_add_extension("rtlsdr-start-sampling", ext_rtlsdr_start_sampling);
  lbm_add_extension("rtlsdr-stop-sampling", ext_rtlsdr_stop_sampling);
  lbm_add_extension("rtlsdr-signal-strength", ext_rtlsdr_signal_strength);
  lbm_add_extension("rtlsdr-get-samples", ext_rtlsdr_get_samples);
  
  lbm_add_extension("rtlsdr-start-fm-playback", ext_rtlsdr_start_fm_playback);
  lbm_add_extension("rtlsdr-stop-fm-playback", ext_rtlsdr_stop_fm_playback);

  lbm_add_extension("rtlsdr-start-radio-thread", ext_rtlsdr_start_radio_thd);
  lbm_add_extension("rtlsdr-stop-radio-thread", ext_rtlsdr_stop_radio_thd);
}
