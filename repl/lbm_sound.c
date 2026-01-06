/*
    Copyright 2025    Joel Svensson  svenssonjoel@yahoo.se

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

/*
  Audio generation gotchas to be aware of:

  aliasing: a manifestation of a lower frequency that
            that happens when the sampled sound frequency is above half of the
            sampling frequency.
            - Remedied by selecting a sampling frequency such that it is at least 2x the audible spectrum.
            - For more info see: https://en.wikipedia.org/wiki/Aliasing

            Square waves contains all odd harmonics and will lead to
            aliasing in the audible range.
            - Remedied by generating the square wave at a much higher sampling
               frequency then doing a low-pass filter and a downsampling.
            - Remedied by a wave generation method called BLEP:
               https://www.martin-finke.de/articles/audio-plugins-018-polyblep-oscillator/
            - Remedied by approximating a square by adding together
               odd harmonics up to the Nyquist frequency.
               odd harmonics are found at 1x 3x 5x ... the note base frequency.

  beating: Interplay of multiple sine waves close in frequency (half-steps apart)
           where they are periodically amplifying each other causing a
           2-stroke engine (or hellicopter blade) like beating sound.
           The frequency of the emerging beat is |freq1 - freq2|.
           (For example C at 261.63Hz and C# at 277.18 pure sine waves
            result in a beat at 15.55Hz)
           - Remedied by not using entirely pure sine waves, introduce more harmonics.
           - For more details: https://en.wikipedia.org/wiki/Beat_(acoustics)

  phase synchronization:
           Multiple simultaneous voices all starting at phase 0.0
           will constructively interfere (amplify each other) potentially
           leading to clipping when they sum up above the range of the sample
           width (number of bits 16 in our case).
           - Remedied by randomizing phase at note-on.
 */

/* A list of possible TODOs
1. Filter cutoff modulation - change the filter cutoff freq dynamically
2. Oscillator detune
3. [X] Triangle/square oscillators
4. Filter resonance - 2 pole filters and above
5. BLEP square waves
6. Implement getter functions
7. Oscillator hard sync
8. Ring modulation - bell sounds
9. Fix stereo processing
10. Pan control
    [X] simple fixed pan per oscillator.
11. [X] Noise generator - Percussion and texture
12. Unison mode - Can be done in lisp!
*/


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include <math.h>

#include <extensions.h>
#include <platform_thread.h>
#include <platform_timestamp.h>
#include "lbm_sound.h"



static snd_pcm_t *pcm_handle = NULL;

static lbm_thread_t synth_thread;
static bool synth_thread_running = false;

static uint32_t voice_sequence_number = 0;

snd_pcm_t *lbm_sound_pcm_handle(void) {
  return pcm_handle;
}

// Default audio parameters
#define SAMPLE_RATE 44100
#define CHANNELS 2
#define BITS_PER_SAMPLE 16
#define LATENCY_US 30000
#define BUFFER_FRAMES 512

// frames = 128 => ~2.9ms per buffer at 44.1kHz

#define MAX_PATCHES 128
#define MAX_VOICES  16
#define NUM_MODULATORS 4

#define NUM_LFO 2
#define NUM_OSC 2

#define WRAP1(X) if ((X) >= 1.0f) (X) -= 1.0f

#ifndef M_PI
#define M_PI 3.14159265f
#endif

#define LEFT  0
#define RIGHT 1

// Oscilator types
typedef enum {
  OSC_NONE = 0,
  OSC_SINE,
  OSC_SAW,
  OSC_TRIANGLE,
  OSC_SQUARE,
  OSC_NOISE
} oscillator_type_t;

typedef enum {
  MOD_NONE = 0,
  MOD_LFO1,
  MOD_LFO2,
  MOD_ENV,
  MOD_VEL
} modulator_source_t;

typedef struct {
  modulator_source_t source;
  float amount;
} modulator_t;

typedef enum {
  FREQ_NOTE = 0,
  FREQ_FIXED,
  FREQ_CHIRP
} freq_source_t;

#define NUM_OSCILLATOR_PARAMETERS 3
#define OSC_PARAMETER_FREQ 0
#define OSC_PARAMETER_CHIRP_START_FREQ 0
#define OSC_PARAMETER_CHIRP_END_FREQ   1
#define OSC_PARAMETER_CHIRP_DURATION   2

// In every iteration, an oscillators frequency is computed as:
//  b + (m0 * a0) + (m1 * a1) .. + (m3 * a3)
// where b is base frequency from note value.
typedef struct {
  oscillator_type_t type;
  freq_source_t freq_source;
  float parameter[NUM_OSCILLATOR_PARAMETERS]; // Repurpose depending on freq source.
  modulator_t modulators[NUM_MODULATORS];
  float phase_offset;
  float vol;
  float pan;
} oscillator_t;

typedef enum {
  ENV_OFF = 0,
  ENV_ATTACK,
  ENV_DECAY,
  ENV_SUSTAIN,
  ENV_RELEASE
} env_adsr_state_t;

// Attack, decay, sustain, release
typedef struct {
  float attack_time;
  float decay_time;
  float sustain_level;
  float release_time;
} env_adsr_t;

// Filters
//
// My understanding of digital filters is quite shallow at the moment.
// The filters below are simple and cost-efficient.
// The 1-pole concept refers to them having 1 feedback term (remembers one old state).
// But it apparently also refers to "poles" in the z-plane in relation to the transfer function.
//
// see https://youtu.be/j0wJBEZdwLs?si=mXei1QBgdgjzKMmz for intuiting about the laplace transform
// which i think is related to the discrete Z-transform, which in turn is related to the concept
// of poles in digital filters.

// 1 pole low pass filter.
typedef struct {
  bool active;
  float alpha; // 1.0f - expf(-2.0f * M_PI * fc / SAMPLE_RATE);
               // fc : Cutoff frequency
               // The expf function is a model of how an analog RC filter would behave.
} filter_1_pole_t;

typedef struct {
  float y_old;
  float x_old;
} filter_1_pole_state_t;

// A synthesizer patch (Instrument)
typedef struct {
  oscillator_t lfo[NUM_LFO];
  oscillator_t osc[NUM_OSC];
  env_adsr_t   env;
  filter_1_pole_t lpf;
  filter_1_pole_t hpf;

} patch_t;

// ////////////////////////////////////////////////////////////
// Voice
typedef struct {
  uint32_t sequence_number; // Steal the oldest
  bool active;
  uint8_t patch;
  uint8_t note; // Midi note id.
  float   freq;
  float   vel;
  float   bend; // Pitch bend.

  uint32_t channel; // Associated midi channel if there is one.
                    // Several notes can be playing concurrently on a single channel.

  //dynamic state during playback;
  float osc_phase[NUM_OSC];
  float lfo_phase[NUM_LFO];
  float env_val;

  // Chirp state
  float osc_chirp_time[NUM_OSC];
  // Envelope statemachine
  env_adsr_state_t env_state;
  float env_time_in_state;
  float release_start_level; // Envelope level when release was triggered

  //filter state
  filter_1_pole_state_t lpf_state[2];
  filter_1_pole_state_t hpf_state[2];
} voice_t;

static patch_t patches[MAX_PATCHES];
static voice_t voices[MAX_VOICES];

// ////////////////////////////////////////////////////////////
// Filter operations

////////////////////
// Low pass filter.
// is a moving weighted average.
float lpf_1_pole(voice_t *v, int channel, float in) {
  float y_old = v->lpf_state[channel].y_old;
  filter_1_pole_t *filter = &patches[v->patch].lpf;
  float y = y_old + filter->alpha * (in - y_old);
  v->lpf_state[channel].y_old = y;
  return y;
}

////////////////////
// High pass filter.
// Measures the change in input between now and prev.
// The measure is decreased by the (1.0f - alpha) factor (a value between 0 and 1).
// alpha then is how strongly we suppress change....
// small changes in input dissappar.
// large changes in input remain.
float hpf_1_pole(voice_t *v, int channel, float in) {
  float y_old = v->hpf_state[channel].y_old;
  float x_old = v->hpf_state[channel].x_old;
  filter_1_pole_t *filter = &patches[v->patch].hpf;
  float y = (1.0f - filter->alpha) * (y_old + in - x_old);
  v->hpf_state[channel].y_old = y;
  v->hpf_state[channel].x_old = in;
  return y;
}


// ////////////////////////////////////////////////////////////
// LBM symbols

static lbm_uint sym_attack  = 0;
static lbm_uint sym_decay   = 0;
static lbm_uint sym_sustain = 0;
static lbm_uint sym_release = 0;

static lbm_uint sym_envelope = 0;

static lbm_uint sym_freq_src_note = 0;
static lbm_uint sym_freq_src_fixed = 0;
static lbm_uint sym_freq_src_chirp = 0;

static lbm_uint sym_osc_none     = 0;
static lbm_uint sym_osc_sine     = 0;
static lbm_uint sym_osc_saw      = 0;
static lbm_uint sym_osc_triangle = 0;
static lbm_uint sym_osc_square   = 0;
static lbm_uint sym_osc_noise    = 0;

static lbm_uint sym_osc1 = 0;
static lbm_uint sym_osc2 = 0;
static lbm_uint sym_lfo1 = 0;
static lbm_uint sym_lfo2 = 0;

static lbm_uint sym_mod_none = 0;
static lbm_uint sym_mod_lfo1 = 0;
static lbm_uint sym_mod_lfo2 = 0;
static lbm_uint sym_mod_env  = 0;
static lbm_uint sym_mod_vel  = 0;

static lbm_uint sym_mod1 = 0;
static lbm_uint sym_mod2 = 0;
static lbm_uint sym_mod3 = 0;
static lbm_uint sym_mod4 = 0;

static lbm_uint sym_simple_lpf = 0;
static lbm_uint sym_simple_hpf = 0;

lbm_value enc_osc_type(oscillator_type_t t) {
  switch(t) {
  case OSC_NONE:
    return lbm_enc_sym(sym_osc_none);
  case OSC_SINE:
    return lbm_enc_sym(sym_osc_sine);
  case OSC_SAW:
    return lbm_enc_sym(sym_osc_saw);
  case OSC_TRIANGLE:
    return lbm_enc_sym(sym_osc_triangle);
  case OSC_SQUARE:
    return lbm_enc_sym(sym_osc_square);
  case OSC_NOISE:
    return lbm_enc_sym(sym_osc_noise);
  }
  return ENC_SYM_NIL;
}

lbm_value enc_osc_freq_source(freq_source_t s) {
  switch(s) {
  case FREQ_NOTE:
    return lbm_enc_sym(sym_freq_src_note);
  case FREQ_FIXED:
    return lbm_enc_sym(sym_freq_src_fixed);
  case FREQ_CHIRP:
    return lbm_enc_sym(sym_freq_src_chirp);
  }
  return ENC_SYM_NIL;
}

// Update envelope state and return current envelope value
static float update_envelope(voice_t *voice, patch_t *patch) {
  float time_delta = 1.0f / SAMPLE_RATE;  // Time per sample
  voice->env_time_in_state += time_delta;

  switch (voice->env_state) {
  case ENV_OFF:
    voice->env_val = 0.0f;
    break;

  case ENV_ATTACK:
    if (patch->env.attack_time > 0.0f) {
      voice->env_val = voice->env_time_in_state / patch->env.attack_time;
      if (voice->env_val >= 1.0f) {
        voice->env_val = 1.0f;
        voice->env_state = ENV_DECAY;
        voice->env_time_in_state = 0.0f;
      }
    } else {
      // Instant attack
      voice->env_val = 1.0f;
      voice->env_state = ENV_DECAY;
      voice->env_time_in_state = 0.0f;
    }
    break;

  case ENV_DECAY:
    if (patch->env.decay_time > 0.0f) {
      float progress = voice->env_time_in_state / patch->env.decay_time;
      voice->env_val = 1.0f - (1.0f - patch->env.sustain_level) * progress;
      if (progress >= 1.0f) {
        voice->env_val = patch->env.sustain_level;
        voice->env_state = ENV_SUSTAIN;
        voice->env_time_in_state = 0.0f;
      }
    } else {
      // Instant decay
      voice->env_val = patch->env.sustain_level;
      voice->env_state = ENV_SUSTAIN;
      voice->env_time_in_state = 0.0f;
    }
    break;

  case ENV_SUSTAIN:
    voice->env_val = patch->env.sustain_level;
    break;

  case ENV_RELEASE:
    if (patch->env.release_time > 0.0f) {
      float progress = voice->env_time_in_state / patch->env.release_time;
      // Release from the level stored when release was triggered
      voice->env_val = voice->release_start_level * (1.0f - progress);

      if (progress >= 1.0f) {
        voice->env_val = 0.0f;
        voice->env_state = ENV_OFF;
        voice->active = false;
      }
    } else {
      // Instant release
      voice->env_val = 0.0f;
      voice->env_state = ENV_OFF;
      voice->active = false;
    }
    break;
  }

  return voice->env_val;
}

static float generate_chirp_freq(oscillator_t *osc, float time_in_chirp) {
  if (time_in_chirp >= osc->parameter[OSC_PARAMETER_CHIRP_DURATION]) {
    return osc->parameter[OSC_PARAMETER_CHIRP_END_FREQ];
  }

  float t = time_in_chirp / osc->parameter[OSC_PARAMETER_CHIRP_DURATION];

  float ratio = osc->parameter[OSC_PARAMETER_CHIRP_END_FREQ] / osc->parameter[OSC_PARAMETER_CHIRP_START_FREQ];
  return osc->parameter[OSC_PARAMETER_CHIRP_START_FREQ] * powf(ratio, t);
}


static void synth_thd(void *arg) {
  (void)arg;

  int16_t *buffer = (int16_t *)malloc(BUFFER_FRAMES * CHANNELS * sizeof(int16_t));
  if (!buffer) {
    fprintf(stderr, "Failed to allocate audio buffer\n");
    return;
  }

  while (synth_thread_running) {

    memset(buffer, 0, BUFFER_FRAMES * CHANNELS * sizeof(int16_t));

    for (int i = 0; i < BUFFER_FRAMES; i ++) {
      float s_left = 0.0;
      float s_right = 0.0;

      for (int v = 0; v < MAX_VOICES; v ++) {
        if (voices[v].active) {

          uint8_t patch = voices[v].patch;

          float base_freq = voices[v].freq;
          float vel = voices[v].vel * 24000.0f;
          float env_val = update_envelope(&voices[v], &patches[patch]);

          float lfo_val[NUM_LFO];

          // Modulation oscillators
          for (int lfo = 0; lfo < NUM_LFO; lfo ++) {
            lfo_val[lfo] = 0.0;
            oscillator_t *w = &patches[patch].lfo[lfo];
            float phase = voices[v].lfo_phase[lfo] + w->phase_offset;
            WRAP1(phase);
            float freq = base_freq; // For now.
            float osc = 0.0f;
            if (w->freq_source == FREQ_FIXED) {
              freq = w->parameter[OSC_PARAMETER_FREQ];
            }
            switch (w->type) {
            case OSC_SAW:
              osc = 2.0f * phase - 1.0f;
              break;
            case OSC_SINE:
              osc = sinf(2.0f * M_PI * phase);
              break;
            case OSC_SQUARE:
              osc = phase > 0.5 ? -1.0f : 1.0f;
              break;
            case OSC_TRIANGLE:
              osc = 1.0f - 4.0f * fabsf(phase - 0.5f);
              break;
            case OSC_NOISE:
              osc = (2.0f * (float)rand() / RAND_MAX) - 1.0f;
              break;
            default:
              break;
            }
            float phase_increment = freq / (float)SAMPLE_RATE;
            voices[v].lfo_phase[lfo] += phase_increment;
            WRAP1(voices[v].lfo_phase[lfo]);
            lfo_val[lfo] = osc;
          }
          // Tone oscillators
          float voice_r = 0.0;
          float voice_l = 0.0;
          for (int o = 0; o < NUM_OSC; o ++) {

            oscillator_t *w = &patches[patch].osc[o];
            float freq = base_freq;
            if (w->freq_source == FREQ_FIXED) {
              freq = w->parameter[OSC_PARAMETER_FREQ];
            }else if (w->freq_source == FREQ_CHIRP) {
              voices[v].osc_chirp_time[o] += (1.0f / SAMPLE_RATE);
              freq = generate_chirp_freq(w, voices[v].osc_chirp_time[o]);
            }
            float phase = voices[v].osc_phase[o] + w->phase_offset;
            WRAP1(phase);

            // compute modulator contribution to
            // the frequency of the tone.
            float mod_val = 0.0f;
            // mod_val = (m0 * a0) + (m1 * a1) ..
            // for all active modulators.
            for (int mod = 0; mod < NUM_MODULATORS; mod ++) {
              modulator_t *ent = &patches[patch].osc[o].modulators[mod];
              switch (ent->source) {
              case MOD_NONE:
                break;
              case MOD_LFO1:
                mod_val += lfo_val[0] * ent->amount;
                break;
              case MOD_LFO2:
                mod_val += lfo_val[1] * ent->amount;
                break;
              case MOD_ENV:
                mod_val += env_val * ent->amount;
                break;
              case MOD_VEL:
                mod_val += vel * ent->amount;
                break;
              }
            }

            float osc = 0.0f;
            switch (w->type) {
            case OSC_SAW:
              // The saw wave jumps from 1.0 to -1.0
              // instantaneoulsy => lots of harmonics => aliasing
              osc = 2.0f * phase - 1.0f;
              break;
            case OSC_SINE:
              osc = sinf(2.0f * M_PI * phase);
              break;
            case OSC_SQUARE:
              osc = phase > 0.5 ? -1.0f : 1.0f;
              break;
            case OSC_TRIANGLE:
              osc = 1.0f - 4.0f * fabsf(phase - 0.5f);
              break;
            case OSC_NOISE:
              osc = (2.0f * (float)rand() / RAND_MAX) - 1.0f;
              break;
            default:
              break;
            }

            float s = osc * env_val * vel * w->vol;
            // Phase increment makes no sense for Noise.
            float phase_increment = ((freq + mod_val) * voices[v].bend) / (float)SAMPLE_RATE;
            voices[v].osc_phase[o] += phase_increment;
            WRAP1(voices[v].osc_phase[o]);

            // If pan doesn't change dynamically these can be precomputed!
            float pan_left = cosf((w->pan + 1.0f) * M_PI / 4.0f);
            float pan_right = sinf((w->pan + 1.0f) * M_PI / 4.0f);

            voice_r += s * pan_right;
            voice_l += s * pan_left;
          }
          // Apply filtering.
          if (patches[patch].lpf.active) {
            voice_r = lpf_1_pole(&voices[v], RIGHT, voice_r);
            voice_l = lpf_1_pole(&voices[v], LEFT, voice_l);
          }

          if (patches[patch].hpf.active) {
            voice_r = hpf_1_pole(&voices[v], RIGHT,  voice_r);
            voice_l = hpf_1_pole(&voices[v], LEFT,  voice_l);
          }
          s_right += voice_r;
          s_left  += voice_l;
        }
      }

      // tanh based "soft clipping"
      // tanh(x) approaches 1.0 as x grows towards infinity
      float mixed_l = tanhf(s_left / 30000.0f);
      float mixed_r = tanhf(s_right / 30000.0f);

      buffer[i*2]   = (int16_t)(mixed_l * 32767.0);
      buffer[i*2+1] = (int16_t)(mixed_r * 32767.0);
    }

    // snd_pcm_writei blocks when the internal ALSA buffer is full
    // This is the only synchronization we will use in this thread.
    // Important to remember this when eventually writing and embedded
    // variant of this code!
    snd_pcm_sframes_t frames = snd_pcm_writei(pcm_handle, buffer, BUFFER_FRAMES);

    // Normally frames will be equal to BUFFER_FRAMES (or negative).
    // A partial write will not happen using the blocking snd_pcm_writei (normally).

    if (frames < 0) {
      frames = snd_pcm_recover(pcm_handle, frames, 0);
      if (frames < 0) {
        printf("snd_pcm_writei failed: %s\n", snd_strerror(frames));
        break;
      }
    }
  }

  free(buffer);
  printf("Audio generation thread stopped\n");
}


static void register_symbols(void) {
  lbm_add_symbol("attack", &sym_attack);
  lbm_add_symbol("decay" , &sym_decay);
  lbm_add_symbol("sustain", &sym_sustain);
  lbm_add_symbol("release", &sym_release);

  lbm_add_symbol("envelope", &sym_envelope);

  lbm_add_symbol("freq-src-note", &sym_freq_src_note);
  lbm_add_symbol("freq-src-fixed", &sym_freq_src_fixed);
  lbm_add_symbol("freq-src-chirp", &sym_freq_src_chirp);

  lbm_add_symbol("osc-none", &sym_osc_none);
  lbm_add_symbol("osc-sine", &sym_osc_sine);
  lbm_add_symbol("osc-saw", &sym_osc_saw);
  lbm_add_symbol("osc-triangle", &sym_osc_triangle);
  lbm_add_symbol("osc-square", &sym_osc_square);
  lbm_add_symbol("osc-noise", &sym_osc_noise);

  lbm_add_symbol("osc1", &sym_osc1);
  lbm_add_symbol("osc2", &sym_osc2);
  lbm_add_symbol("lfo1", &sym_lfo1);
  lbm_add_symbol("lfo2", &sym_lfo2);

  lbm_add_symbol("mod-none", &sym_mod_none);
  lbm_add_symbol("mod-lfo1", &sym_mod_lfo1);
  lbm_add_symbol("mod-lfo2", &sym_mod_lfo2);
  lbm_add_symbol("mod-env", &sym_mod_env);
  lbm_add_symbol("mod-vel", &sym_mod_vel);

  lbm_add_symbol("mod1", &sym_mod1);
  lbm_add_symbol("mod2", &sym_mod2);
  lbm_add_symbol("mod3", &sym_mod3);
  lbm_add_symbol("mod4", &sym_mod4);

  lbm_add_symbol("simple-lpf", &sym_simple_lpf);
  lbm_add_symbol("simple-hpf", &sym_simple_hpf);
}

// ////////////////////////////////////////////////////////////
// LBM extensions

// TODO: ext_patch_lfo_get and ext_patch_mod_get could use same
//       return value idiom as filter_get. That is:
//         not active -> nil
//             active -> a truthy value containing the config data.



lbm_value ext_patch_filter_set(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 3 &&
      lbm_is_number(args[0]) &&
      lbm_is_symbol(args[1]) &&
      lbm_is_number(args[2])) {  // In the future different filters could have more parameters here.
    uint8_t patch = lbm_dec_as_char(args[0]);
    if (patch < MAX_PATCHES) {
      lbm_uint sym = lbm_dec_sym(args[1]);
      if (sym == sym_simple_lpf) {

        float cutoff_freq = lbm_dec_as_float(args[2]);
        float alpha = 1.0f - expf(-2.0f * M_PI * cutoff_freq / SAMPLE_RATE);
        patches[patch].lpf.alpha = alpha;
        if (cutoff_freq > 0.0f) {
          patches[patch].lpf.active = true; // set last in case we want to make it an atomic operation.
        } else {
          patches[patch].lpf.active = false;
        }
        r = ENC_SYM_TRUE;
      } else if (sym == sym_simple_hpf) {
        float cutoff_freq = lbm_dec_as_float(args[2]);
        float alpha = 1.0f - expf(-2.0f * M_PI * cutoff_freq / SAMPLE_RATE);
        patches[patch].hpf.alpha = alpha;
        if (cutoff_freq > 0.0f) {
          patches[patch].hpf.active = true; // set last in case we want to make it an atomic operation.
        } else {
          patches[patch].hpf.active = false;
        }
        r = ENC_SYM_TRUE;
      } else {
        r = ENC_SYM_NIL;
      }
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}

lbm_value ext_patch_filter_get(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_symbol(args[1])) {
    uint8_t patch = lbm_dec_as_char(args[0]);
    if (patch < MAX_PATCHES) {
      lbm_uint sym = lbm_dec_sym(args[1]);
      if (sym == sym_simple_lpf) {
        float cutoff_freq = -logf(1.0f - patches[patch].lpf.alpha) * SAMPLE_RATE / (2.0f * M_PI);
        bool  active =  patches[patch].lpf.active;
        if (active) {
          r = lbm_enc_float(cutoff_freq);
        } else {
          r = ENC_SYM_NIL;
        }
      } else if (sym == sym_simple_hpf) {
        float cutoff_freq = -logf(1.0f - patches[patch].hpf.alpha) * SAMPLE_RATE / (2.0f * M_PI);
        bool  active =  patches[patch].hpf.active;
        if (active) {
          r = lbm_enc_float(cutoff_freq);
        } else {
          r = ENC_SYM_NIL;
        }
      } else {
        r = ENC_SYM_NIL;
      }
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}


//(patch-mod-set patch-no osc-no mod-no mod-source mod-amount)
lbm_value ext_patch_mod_set(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 5 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_number(args[2]) &&
      lbm_is_symbol(args[3]) &&
      lbm_is_number(args[4])) {
    uint8_t patch = lbm_dec_as_char(args[0]);
    uint32_t osc  = lbm_dec_as_u32(args[1]);
    uint32_t mod  = lbm_dec_as_u32(args[2]);
    lbm_uint src  = lbm_dec_sym(args[3]);
    float amount  = lbm_dec_as_float(args[4]);

    if (patch < MAX_PATCHES && osc < NUM_OSC && mod < NUM_MODULATORS) {
      modulator_source_t m;
      if (src == sym_mod_lfo1) {
        m = MOD_LFO1;
      } else if (src == sym_mod_lfo2) {
        m = MOD_LFO2;
      } else if (src == sym_mod_env) {
        m = MOD_ENV;
      } else if (src == sym_mod_vel) {
        m = MOD_VEL;
      } else {
        m = MOD_NONE;
      }
      modulator_t *ent = &patches[patch].osc[osc].modulators[mod];
      ent->source = m;
      ent->amount = amount;
      r = ENC_SYM_TRUE;
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}

//(patch-lfo-set patch-no lfo-num lfo-type lfo-frequency)
lbm_value ext_patch_lfo_set(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 4 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_symbol(args[2]) &&
      lbm_is_number(args[3])) {

    uint8_t patch = lbm_dec_as_char(args[0]);
    uint32_t lfo  = lbm_dec_as_u32(args[1]);
    lbm_uint osc_type = lbm_dec_sym(args[2]);
    float    freq = lbm_dec_as_float(args[3]);

    if (lfo < NUM_LFO && patch < MAX_PATCHES) {
      oscillator_type_t o;
      if (osc_type == sym_osc_sine) {
        o = OSC_SINE;
      } else if (osc_type == sym_osc_saw) {
        o = OSC_SAW;
      } else if (osc_type == sym_osc_triangle) {
        o = OSC_TRIANGLE;
      } else if (osc_type == sym_osc_square) {
        o = OSC_SQUARE;
      } else {
        o = OSC_NONE;
      }
      patch_t *p = &patches[patch];
      p->lfo[lfo].freq_source = FREQ_FIXED;
      p->lfo[lfo].parameter[OSC_PARAMETER_FREQ] = freq;
      p->lfo[lfo].phase_offset = 0.0;
      p->lfo[lfo].vol = 0.0;
      p->lfo[lfo].type = o;
      r = ENC_SYM_TRUE;
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}

//(patch-osc-chirp-set patch-no osc-no osc-type start-freq end-freq duration)
lbm_value ext_patch_osc_chirp_set(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 6 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_symbol(args[2]) &&
      lbm_is_number(args[3]) &&
      lbm_is_number(args[4]) &&
      lbm_is_number(args[5])) {

    uint8_t patch = lbm_dec_as_char(args[0]);
    uint32_t osc  = lbm_dec_as_u32(args[1]);
    lbm_uint osc_type = lbm_dec_sym(args[2]);
    float start_freq = lbm_dec_as_float(args[3]);
    float end_freq = lbm_dec_as_float(args[4]);
    float duration = lbm_dec_as_float(args[5]);

    if (osc < NUM_OSC && patch < MAX_PATCHES) {
      oscillator_type_t o;
      if (osc_type == sym_osc_sine) {
        o = OSC_SINE;
      } else if (osc_type == sym_osc_saw) {
        o = OSC_SAW;
      } else if (osc_type == sym_osc_triangle) {
        o = OSC_TRIANGLE;
      } else if (osc_type == sym_osc_square) {
        o = OSC_SQUARE;
      } else if (osc_type == sym_osc_noise) {
        o = OSC_NOISE;
      } else {
        o = OSC_NONE;
      }
      patches[patch].osc[osc].type = o;
      patches[patch].osc[osc].freq_source = FREQ_CHIRP;
      patches[patch].osc[osc].parameter[OSC_PARAMETER_CHIRP_START_FREQ] = start_freq;
      patches[patch].osc[osc].parameter[OSC_PARAMETER_CHIRP_END_FREQ] = end_freq;
      patches[patch].osc[osc].parameter[OSC_PARAMETER_CHIRP_DURATION] = duration;
      patches[patch].osc[osc].vol = 1.0f;  // Set default volume
      patches[patch].osc[osc].phase_offset = 0.0f;
      patches[patch].osc[osc].pan = 0.0f;  // Center pan
      r = ENC_SYM_TRUE;
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}


// (patch-osc-tvp-set-tvp p-num o-num o-type o-vol o-phase-offset)
lbm_value ext_patch_osc_tvp_get(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;

  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1])) {

    uint8_t patch = lbm_dec_as_char(args[0]);
    uint32_t osc = lbm_dec_as_u32(args[1]);

    if (osc < NUM_OSC && patch < MAX_PATCHES) {
      patch_t *p = &patches[patch];

      lbm_value t = enc_osc_type(p->osc[osc].type);

      lbm_value v = lbm_enc_float(p->osc[osc].vol);
      lbm_value ph = lbm_enc_float(p->osc[osc].phase_offset);
      if (v == ENC_SYM_MERROR ||
          ph == ENC_SYM_MERROR) return ENC_SYM_MERROR;
      r = lbm_heap_allocate_list_init(3, t, v, ph);
    }
  }
  return r;
}

lbm_value ext_patch_osc_tvp_set(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;

  if (argn == 5 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_symbol(args[2]) &&
      lbm_is_number(args[3]) &&
      lbm_is_number(args[4])) {

    uint8_t patch = lbm_dec_as_char(args[0]);
    float vol = lbm_dec_as_float(args[3]);
    float phase_offset = lbm_dec_as_float(args[4]);
    lbm_uint osc_type = lbm_dec_sym(args[2]);
    uint32_t osc = lbm_dec_as_u32(args[1]);

    if (osc < NUM_OSC && patch < MAX_PATCHES) {

      oscillator_type_t o;
      if (osc_type == sym_osc_sine) {
        o = OSC_SINE;
      } else if (osc_type == sym_osc_saw) {
        o = OSC_SAW;
      } else if (osc_type == sym_osc_triangle) {
        o = OSC_TRIANGLE;
      } else if (osc_type == sym_osc_square) {
        o = OSC_SQUARE;
      } else if (osc_type == sym_osc_noise) {
        o = OSC_NOISE;
      } else {
        o = OSC_NONE;
      }
      patches[patch].osc[osc].type = o;
      patches[patch].osc[osc].freq_source = FREQ_NOTE; // by default
      patches[patch].osc[osc].vol = vol;
      patches[patch].osc[osc].phase_offset = phase_offset;
      r = ENC_SYM_TRUE;
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}


lbm_value ext_patch_osc_pan_set(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 3 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_number(args[2])) {
    r = ENC_SYM_NIL;
    uint8_t patch = lbm_dec_as_char(args[0]);
    uint32_t osc  = lbm_dec_as_u32(args[1]);
    float pan = lbm_dec_as_float(args[2]);
    if (patch < MAX_PATCHES && osc < NUM_OSC) {
      patches[patch].osc[osc].pan = pan;
      r = ENC_SYM_TRUE;
    }
  }
  return r;
}

lbm_value ext_patch_osc_pan_get(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1])) {
    r = ENC_SYM_NIL;
    uint8_t patch = lbm_dec_as_char(args[0]);
    uint32_t osc  = lbm_dec_as_u32(args[1]);
    if (patch < MAX_PATCHES && osc < NUM_OSC) {
      r = lbm_enc_float(patches[patch].osc[osc].pan);
    }
  }
  return r;
}



lbm_value ext_patch_adsr_get(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_number(args[0])) {

    uint8_t patch = lbm_dec_as_char(args[0]);
    if (patch >= MAX_PATCHES) return ENC_SYM_NIL;
    lbm_value a = lbm_enc_float(patches[patch].env.attack_time);
    lbm_value d = lbm_enc_float(patches[patch].env.decay_time);
    lbm_value s = lbm_enc_float(patches[patch].env.sustain_level);
    lbm_value rel = lbm_enc_float(patches[patch].env.release_time);
    if (a == ENC_SYM_MERROR ||
        d == ENC_SYM_MERROR ||
        s == ENC_SYM_MERROR ||
        rel == ENC_SYM_MERROR) {
      r = ENC_SYM_MERROR;
    } else {
      r = lbm_heap_allocate_list_init(4, a, d, s ,rel);
    }
  }
  return r;
}

// (set-adsr patch-no attack decay sustain-level release)
lbm_value ext_patch_adsr_set(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 5 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_number(args[2]) &&
      lbm_is_number(args[3]) &&
      lbm_is_number(args[4])) {
    uint8_t patch = lbm_dec_as_char(args[0]);
    if (patch >= MAX_PATCHES) return ENC_SYM_NIL;
    float   attack = lbm_dec_as_float(args[1]);
    float   decay  = lbm_dec_as_float(args[2]);
    float   sustain = lbm_dec_as_float(args[3]);
    float   release = lbm_dec_as_float(args[4]);

    patches[patch].env.attack_time = attack;
    patches[patch].env.decay_time = decay;
    patches[patch].env.sustain_level = sustain;
    patches[patch].env.release_time = release;
    r = ENC_SYM_TRUE;
  }
  return r;
}


lbm_value ext_patch_clear(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_number(args[0])) {
    uint8_t patch = lbm_dec_as_char(args[0]);
    if (patch < MAX_PATCHES) {
      memset(&patches[patch], 0, sizeof(patch_t));
      r = ENC_SYM_TRUE;
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}

static uint32_t start_voice(voice_t *v, uint8_t patch, uint8_t note, float freq, float vel, uint32_t channel) {
  v->sequence_number = voice_sequence_number ++;
  v->channel = channel;
  v->patch = patch;
  v->note = note;
  v->freq = freq;
  v->vel = vel;
  v->bend = 1.0f;

  float phase = (float)rand() / RAND_MAX;
  v->osc_phase[0] = phase;
  v->osc_phase[1] = phase;
  v->osc_chirp_time[0] = 0.0f;
  v->osc_chirp_time[1] = 0.0f;
  v->lfo_phase[0] = 0.0f;
  v->lfo_phase[1] = 0.0f;
  v->env_val = 0.0f;
  v->env_state = ENV_ATTACK;
  v->env_time_in_state = 0.0f;
  v->release_start_level = 0.0f;

  v->lpf_state[LEFT].y_old = 0.0f;
  v->lpf_state[LEFT].x_old = 0.0f;
  v->hpf_state[LEFT].y_old = 0.0f;
  v->hpf_state[LEFT].x_old = 0.0f;
  v->lpf_state[RIGHT].y_old = 0.0f;
  v->lpf_state[RIGHT].x_old = 0.0f;
  v->hpf_state[RIGHT].y_old = 0.0f;
  v->hpf_state[RIGHT].x_old = 0.0f;

  // update active last.
  // possibly make this a synchronization using _Atomic bool.
  // But there is no real reason to synchronize as there are no pointers to chase
  // through the voice/patch, etc structs. We wont end up crashing, just potentially
  // glitch out the sound.
  //
  // atomic_store_explicit(&v->active, true, memory_order_release);
  // atomic_load_explicit(&voices[v].active, memory_order_acquire);
  // - memory_order_relaxed - No ordering guarantees (just atomicity)
  // - memory_order_acquire - Reads after this can't be reordered before it
  // - memory_order_release - Writes before this can't be reordered after it
  // - memory_order_seq_cst - Full sequential consistency
  v->active = true;
  return v->sequence_number;
}

lbm_value ext_ch_pitch_bend(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1])) {


    int32_t bend_val = lbm_dec_as_i32(args[0]);
    uint32_t ch = lbm_dec_as_u32(args[1]);

    float semitones = (bend_val / 8192.0f) * 2.0f;
    float cents = semitones * 100.0f;
    float bend_ratio = powf(2.0f, cents / 1200.0f);

    r = ENC_SYM_NIL;
    for (int v = 0; v < MAX_VOICES; v ++) {
      if (voices[v].channel == ch) {
        voices[v].bend = bend_ratio; // Apply bend to all channel notes.
        r = ENC_SYM_TRUE;
      }
    }
  }
  return r;
}

// (note-on patch-no note-id vel)
lbm_value ext_note_on(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 3 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_number(args[2])) {

    lbm_value result = lbm_enc_u32(0);
    if (lbm_is_symbol(result)) return result;

    uint8_t patch = lbm_dec_as_char(args[0]);
    uint8_t note = lbm_dec_as_char(args[1]);
    uint8_t vel = lbm_dec_as_char(args[2]);
    float vel_f = (float)vel / 127.0f;
    float freq = 440.0f * powf(2.0f, (note - 69) / 12.0f);

    // If no free voice, replace the oldest playing note (by seq nr).
    // Will steal out of sequence at point of overflow.
    uint32_t min_seq = UINT32_MAX;
    int      min_seq_ix = -1;
    int  slot_ix = -1;
    for (int i = 0; i < MAX_VOICES; i ++) {
      if (min_seq > voices[i].sequence_number) {
        min_seq = voices[i].sequence_number;
        min_seq_ix = i;
      }
      if (voices[i].active &&
          voices[i].patch == patch &&
          voices[i].note  == note) {
        slot_ix = i;
        break;
      }
      if (!voices[i].active) slot_ix = i;
    }
    if (slot_ix < 0) slot_ix = min_seq_ix;
    start_voice(&voices[slot_ix], patch, note, freq, vel_f, 0);
    r = ENC_SYM_TRUE;
  }
  return r;
}

lbm_value ext_ch_note_on(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 4 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_number(args[2]) &&
      lbm_is_number(args[3])) {

    lbm_value result = lbm_enc_u32(0);
    if (lbm_is_symbol(result)) return result;

    uint8_t patch = lbm_dec_as_char(args[0]);
    uint8_t note = lbm_dec_as_char(args[1]);
    uint8_t vel = lbm_dec_as_char(args[2]);
    uint32_t ch  = lbm_dec_as_u32(args[3]);
    float vel_f = (float)vel / 127.0f;
    float freq = 440.0f * powf(2.0f, (note - 69) / 12.0f);

    // If no free voice, replace the oldest playing note (by seq nr).
    // Will steal out of sequence at point of overflow.
    uint32_t min_seq = UINT32_MAX;
    int      min_seq_ix = -1;
    int  slot_ix = -1;
    for (int i = 0; i < MAX_VOICES; i ++) {
      if (min_seq > voices[i].sequence_number) {
        min_seq = voices[i].sequence_number;
        min_seq_ix = i;
      }
      if (voices[i].active &&
          voices[i].patch == patch &&
          voices[i].note  == note) {
        slot_ix = i;
        break;
      }
      if (!voices[i].active) slot_ix = i;
    }
    if (slot_ix < 0) slot_ix = min_seq_ix;
    start_voice(&voices[slot_ix], patch, note, freq, vel_f, ch);
    r = ENC_SYM_TRUE;
  }
  return r;
}


// (note-off patch-no node-id)
lbm_value ext_note_off(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1])) {
    uint8_t patch = lbm_dec_as_char(args[0]);
    uint8_t note = lbm_dec_as_char(args[1]);

    for (int i = 0; i < MAX_VOICES; i ++) {
      if (voices[i].active &&
          voices[i].patch == patch &&
          voices[i].note  == note) {
        voices[i].env_state = ENV_RELEASE;
        voices[i].env_time_in_state = 0.0f;
        voices[i].release_start_level = voices[i].env_val;
        break;
      }
    }
    r = ENC_SYM_TRUE;
  }
  return r;
}

// ////////////////////////////////////////////////////////////
// Init

bool lbm_sound_init(void) {
  int err;

  // plughw:0,0 requests direct to hardware control
  // This will fail if some other application is playing sound.
  //err = snd_pcm_open(&pcm_handle, "plughw:0,0", SND_PCM_STREAM_PLAYBACK, 0);
  err = snd_pcm_open(&pcm_handle, "default", SND_PCM_STREAM_PLAYBACK, 0);
  if (err < 0) {
    fprintf(stderr, "Cannot open audio device: %s\n", snd_strerror(err));
    return false;
  }

  err = snd_pcm_set_params(pcm_handle,
                           SND_PCM_FORMAT_S16_LE,
                           SND_PCM_ACCESS_RW_INTERLEAVED,
                           CHANNELS,
                           SAMPLE_RATE,
                           1,
                           LATENCY_US);
  if (err < 0) {
    fprintf(stderr, "Cannot set audio parameters: %s\n", snd_strerror(err));
    snd_pcm_close(pcm_handle);
    pcm_handle = NULL;
    return false;
  }

  printf("ALSA sound system initialized: %d Hz, %d channels, %d-bit, %dms latency\n",
         SAMPLE_RATE, CHANNELS, BITS_PER_SAMPLE, LATENCY_US / 1000);

  synth_thread_running = true;
  if (!lbm_thread_create(&synth_thread, "synth_thd", synth_thd,
                         NULL, LBM_THREAD_PRIO_HIGH, 32768)) {
    fprintf(stderr, "Failed to create synth thread\n");
    snd_pcm_close(pcm_handle);
    pcm_handle = NULL;
    return false;
  }

  for (int i = 0; i < MAX_VOICES; i ++) {
    voices[i].active = false;
  }

  for (int i = 0; i < MAX_PATCHES; i ++) {
    memset(&patches[i], 0, sizeof(patch_t));
  }

  register_symbols();

  // Extensions that take a channel
  lbm_add_extension("ch-pitch-bend", ext_ch_pitch_bend);
  lbm_add_extension("ch-note-on", ext_ch_note_on);

  // Notes
  lbm_add_extension("note-on", ext_note_on);
  lbm_add_extension("note-off", ext_note_off);

  // Patches
  lbm_add_extension("patch-clear", ext_patch_clear);
  lbm_add_extension("patch-mod-set", ext_patch_mod_set);
  lbm_add_extension("patch-lfo-set", ext_patch_lfo_set);
  lbm_add_extension("patch-osc-chirp-set", ext_patch_osc_chirp_set);
  lbm_add_extension("patch-osc-tvp-set", ext_patch_osc_tvp_set);
  lbm_add_extension("patch-osc-tvp-get", ext_patch_osc_tvp_get);
  lbm_add_extension("patch-osc-pan-set", ext_patch_osc_pan_set);
  lbm_add_extension("patch-osc-pan-get", ext_patch_osc_pan_get);
  lbm_add_extension("patch-adsr-set", ext_patch_adsr_set);
  lbm_add_extension("patch-adsr-get", ext_patch_adsr_get);
  lbm_add_extension("patch-filter-set", ext_patch_filter_set);
  lbm_add_extension("patch-filter-get", ext_patch_filter_get);
  return true;
}

void lbm_sound_cleanup(void) {
  if (synth_thread_running) {
    synth_thread_running = false;
    lbm_thread_destroy(&synth_thread);
  }

  if (pcm_handle) {
    snd_pcm_drain(pcm_handle);
    snd_pcm_close(pcm_handle);
    pcm_handle = NULL;
    printf("ALSA sound system cleanup complete\n");
  }
}

