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

#define _POSIX_C_SOURCE 200809L

#include "lbm_midi.h"
#include <alsa/asoundlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <alloca.h>
#include "lispbm.h"
#include "platform_thread.h"

static snd_seq_t *seq_handle = NULL;
static int seq_client = -1;
static int seq_port = -1;

// Only allow one blocked CID, so only start one midi reader lbm thread!
static volatile lbm_cid awaiting = -1;
static volatile lbm_value awaiting_result;

static volatile bool midi_thread_running = false;
static lbm_thread_t midi_thread;

static lbm_uint sym_controller = 0;
static lbm_uint sym_pitch_bend = 0;
static lbm_uint sym_note_on  = 0;
static lbm_uint sym_note_off = 0;
static lbm_uint sym_midi_unknown  = 0;
static lbm_uint sym_port_unsub = 0;

bool midi_read(lbm_value res) {
  snd_seq_event_t *ev = NULL;

  if (snd_seq_event_input(seq_handle, &ev) < 0) {
    return false;
  }

  if (ev == NULL) {
    return false;
  }

  switch (ev->type) {
  case SND_SEQ_EVENT_PORT_UNSUBSCRIBED: {
    lbm_value curr = res;
    lbm_set_car(curr, lbm_enc_sym(sym_port_unsub));
    lbm_set_cdr(curr, ENC_SYM_NIL);
  } break;
  case SND_SEQ_EVENT_CONTROLLER: {
    lbm_value curr = res;
    lbm_set_car(curr, lbm_enc_sym(sym_controller));
    curr = lbm_cdr(curr);
    lbm_set_car(curr, lbm_enc_i((int)ev->data.control.param));
    curr = lbm_cdr(curr);
    lbm_set_car(curr, lbm_enc_i(ev->data.control.value));
    lbm_set_cdr(curr, ENC_SYM_NIL);
  } break;
  case SND_SEQ_EVENT_PITCHBEND: {
    lbm_value curr = res;
    lbm_set_car(curr, lbm_enc_sym(sym_pitch_bend));
    curr = lbm_cdr(curr);
    lbm_set_car(curr, lbm_enc_i(ev->data.control.channel));
    curr = lbm_cdr(curr);
    lbm_set_car(curr, lbm_enc_i(ev->data.control.value));
    lbm_set_cdr(curr, ENC_SYM_NIL); // Cut off unused portion,
  } break;
  case SND_SEQ_EVENT_NOTEON:
    if (ev->data.note.velocity > 0) {
      lbm_value curr = res;
      lbm_set_car(curr, lbm_enc_sym(sym_note_on));
      curr = lbm_cdr(curr);
      lbm_set_car(curr, lbm_enc_i(ev->data.note.channel));
      curr = lbm_cdr(curr);
      lbm_set_car(curr, lbm_enc_i(ev->data.note.note));
      curr = lbm_cdr(curr);
      lbm_set_car(curr, lbm_enc_i(ev->data.note.velocity));
      lbm_set_cdr(curr, ENC_SYM_NIL);
      break;
    }
    __attribute__((fallthrough));
  case SND_SEQ_EVENT_NOTEOFF:
    {
      lbm_value curr = res;
      lbm_set_car(curr, lbm_enc_sym(sym_note_off));
      curr = lbm_cdr(curr);
      lbm_set_car(curr, lbm_enc_i(ev->data.note.channel));
      curr = lbm_cdr(curr);
      lbm_set_car(curr, lbm_enc_i(ev->data.note.note));
      lbm_set_cdr(curr, ENC_SYM_NIL);
    } break;
  default: {
    lbm_value curr = res;
    lbm_set_car(curr, lbm_enc_sym(sym_midi_unknown));
    lbm_set_cdr(curr, ENC_SYM_NIL);
  } break;
  }
  return true;
}

// MIDI polling thread - unblocks awaiting context when events arrive
static void midi_poll_thread(void *arg) {
  (void)arg;

  while (midi_thread_running) {
    if (awaiting != -1) {
      int pending = snd_seq_event_input_pending(seq_handle, 1);
      if (pending > 0) {
        if (midi_read(awaiting_result)) {
          lbm_unblock_ctx_r(awaiting);
        } else {
          lbm_unblock_ctx_unboxed(awaiting, ENC_SYM_NIL);
        }
        awaiting = -1;
      }
    }
    lbm_thread_sleep_us(1000);
  }
}

static lbm_value ext_midi_read(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;

  if (seq_handle == NULL) {
    return ENC_SYM_EERROR;
  }

  lbm_value result_list = lbm_heap_allocate_list(4);
  if (result_list == ENC_SYM_MERROR) return result_list;

  if (!snd_seq_event_input_pending(seq_handle, 0)) {
    awaiting_result = result_list;
    awaiting = lbm_get_current_cid();
    lbm_block_ctx_from_extension();
    return result_list;
  }
  midi_read(result_list);
  return result_list;
}

static lbm_value ext_midi_enumerate_devices(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;

  if (seq_handle == NULL) {
    return ENC_SYM_EERROR;
  }

  snd_seq_client_info_t *cinfo;
  snd_seq_port_info_t *pinfo;

  snd_seq_client_info_alloca(&cinfo);
  snd_seq_port_info_alloca(&pinfo);

  snd_seq_client_info_set_client(cinfo, -1);

  lbm_value res = ENC_SYM_NIL;

  while (snd_seq_query_next_client(seq_handle, cinfo) >= 0) {
    int client = snd_seq_client_info_get_client(cinfo);

    snd_seq_port_info_set_client(pinfo, client);
    snd_seq_port_info_set_port(pinfo, -1);

    while (snd_seq_query_next_port(seq_handle, pinfo) >= 0) {
      unsigned int caps = snd_seq_port_info_get_capability(pinfo);
      if ((caps & SND_SEQ_PORT_CAP_READ) &&
          (caps & SND_SEQ_PORT_CAP_SUBS_READ)) {

        int port = snd_seq_port_info_get_port(pinfo);
        const char *port_name = snd_seq_port_info_get_name(pinfo);

        lbm_value port_name_val;
        unsigned int len = (unsigned int)strlen(port_name) + 1;
        if (lbm_create_array(&port_name_val, len)) {
          lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(port_name_val);
          memcpy(arr->data, port_name, len);
        } else {
          return ENC_SYM_MERROR;
        }

        lbm_value l = lbm_heap_allocate_list_init(3,
                                                  lbm_enc_i(client),
                                                  lbm_enc_i(port),
                                                  port_name_val);
        if (l == ENC_SYM_MERROR) return ENC_SYM_MERROR;
        res = lbm_cons(l, res);
        if (res == ENC_SYM_MERROR) return ENC_SYM_MERROR;
      }
    }
  }
  return res;
}

// (midi-connect client port) -> t or error
static lbm_value ext_midi_connect(lbm_value *args, lbm_uint argn) {
  if (argn != 2) {
    return ENC_SYM_EERROR;
  }

  if (seq_handle == NULL || seq_port < 0) {
    return ENC_SYM_EERROR;
  }

  if (!lbm_is_number(args[0]) || !lbm_is_number(args[1])) {
    return ENC_SYM_TERROR;
  }

  int client = lbm_dec_as_i32(args[0]);
  int port = lbm_dec_as_i32(args[1]);

  int err = snd_seq_connect_from(seq_handle, seq_port, client, port);
  if (err < 0) {
    return ENC_SYM_EERROR;
  }

  return ENC_SYM_TRUE;
}

// (midi-disconnect client port) -> t or error
static lbm_value ext_midi_disconnect(lbm_value *args, lbm_uint argn) {
  if (argn != 2) {
    return ENC_SYM_EERROR;
  }

  if (seq_handle == NULL || seq_port < 0) {
    return ENC_SYM_EERROR;
  }

  if (!lbm_is_number(args[0]) || !lbm_is_number(args[1])) {
    return ENC_SYM_TERROR;
  }

  int client = lbm_dec_as_i32(args[0]);
  int port = lbm_dec_as_i32(args[1]);

  int err = snd_seq_disconnect_from(seq_handle, seq_port, client, port);
  if (err < 0) {
    return ENC_SYM_EERROR;
  }

  return ENC_SYM_TRUE;
}

static lbm_value ext_midi_client(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  return lbm_enc_i(seq_client);
}

static lbm_value ext_midi_in_port(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  return lbm_enc_i(seq_port);
}

bool lbm_midi_init(void) {
  int err;

  err = snd_seq_open(&seq_handle, "default", SND_SEQ_OPEN_INPUT, 0);
  if (err < 0) {
    return false;
  }

  snd_seq_set_client_name(seq_handle, "LispBM MIDI");

  // Client and port for midi input.
  seq_client = snd_seq_client_id(seq_handle);
  seq_port = snd_seq_create_simple_port(seq_handle, "LispBM MIDI In",
                                         SND_SEQ_PORT_CAP_WRITE | SND_SEQ_PORT_CAP_SUBS_WRITE,
                                         SND_SEQ_PORT_TYPE_MIDI_GENERIC | SND_SEQ_PORT_TYPE_APPLICATION);
  if (seq_port < 0) {
    snd_seq_close(seq_handle);
    seq_handle = NULL;
    return false;
  }

  midi_thread_running = true;
  if (!lbm_thread_create(&midi_thread, "midi_poll", midi_poll_thread, NULL,
                         LBM_THREAD_PRIO_NORMAL, 0)) {
    snd_seq_delete_simple_port(seq_handle, seq_port);
    snd_seq_close(seq_handle);
    seq_handle = NULL;
    seq_port = -1;
    return false;
  }

  lbm_add_symbol("controller", &sym_controller);
  lbm_add_symbol("pitch-bend", &sym_pitch_bend);
  lbm_add_symbol("note-on", &sym_note_on);
  lbm_add_symbol("note-off", &sym_note_off);
  lbm_add_symbol("midi-unknown", &sym_midi_unknown);
  lbm_add_symbol("port-unsubscribed", &sym_port_unsub);
  lbm_add_extension("midi-read", ext_midi_read);
  lbm_add_extension("midi-enumerate-devices", ext_midi_enumerate_devices);
  lbm_add_extension("midi-connect", ext_midi_connect);
  lbm_add_extension("midi-disconnect", ext_midi_disconnect);
  lbm_add_extension("midi-client", ext_midi_client);
  lbm_add_extension("midi-in-port", ext_midi_in_port);

  return true;
}

void lbm_midi_cleanup(void) {
  if (midi_thread_running) {
    midi_thread_running = false;
    lbm_thread_destroy(&midi_thread);
  }

  if (seq_handle != NULL) {
    if (seq_port >= 0) {
      snd_seq_delete_simple_port(seq_handle, seq_port);
      seq_port = -1;
    }
    snd_seq_close(seq_handle);
    seq_handle = NULL;
  }
}
