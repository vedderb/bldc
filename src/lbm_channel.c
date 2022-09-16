/*
    Copyright 2022 Joel Svensson        svenssonjoel@yahoo.se

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

#include <lbm_channel.h>
#include <string.h>

/* ------------------------------------------------------------
   Interface
   ------------------------------------------------------------ */
bool lbm_channel_more(lbm_char_channel_t *ch) {
  return ch->more(ch);
}

int lbm_channel_peek(lbm_char_channel_t *ch, unsigned int n, char *res) {
  return ch->peek(ch, n, res);
}

bool lbm_channel_read(lbm_char_channel_t *ch, char *res) {
  return ch->read(ch, res);
}

bool lbm_channel_drop(lbm_char_channel_t *ch, unsigned int n) {
  return ch->drop(ch, n);
}

bool lbm_channel_comment(lbm_char_channel_t *ch) {
  return ch->comment(ch);
}

void lbm_channel_set_comment(lbm_char_channel_t *ch, bool comment) {
  ch->set_comment(ch, comment);
}

bool lbm_channel_is_empty(lbm_char_channel_t *ch) {
  return ch->channel_is_empty(ch);
}

bool lbm_channel_is_full(lbm_char_channel_t *ch) {
  return ch->channel_is_full(ch);
}

int lbm_channel_write(lbm_char_channel_t *ch, char c) {
  return ch->write(ch, c);
}

void lbm_channel_writer_close(lbm_char_channel_t *ch) {
  ch->writer_close(ch);
}

void lbm_channel_reader_close(lbm_char_channel_t *ch) {
  ch->reader_close(ch);
}

unsigned int lbm_channel_row(lbm_char_channel_t *ch) {
  return ch->row(ch);
}

unsigned int lbm_channel_column(lbm_char_channel_t *ch) {
  return ch->column(ch);
}

/* ------------------------------------------------------------
   Implementation buffered channel
   ------------------------------------------------------------ */

bool buffered_more(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  return st->more;
}

void buffered_writer_close(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  st->more = false;
}

void buffered_reader_close(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  st->reader_closed = true;
}

int buffered_peek(lbm_char_channel_t *ch, unsigned int n, char *res) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  char *buffer = st->buffer;
  int ret = CHANNEL_MORE;
  mutex_lock(&st->lock);
  unsigned int peek_pos = (st->read_pos + n) % TOKENIZER_BUFFER_SIZE;
  bool in_data;

  if (st->write_pos >= st->read_pos) {
    in_data = peek_pos < st->write_pos && peek_pos >= st->read_pos;
  } else {
    in_data = !(peek_pos >= st->write_pos && peek_pos < st->read_pos);
  }

  if (in_data) {
    *res = buffer[peek_pos];
    ret = CHANNEL_SUCCESS;
  } else if (!buffered_more(ch)) {
    ret = CHANNEL_END;
  } else if (buffered_more(ch)) {
    ret = CHANNEL_MORE;
  }
  mutex_unlock(&st->lock);
  return ret;
}

bool buffered_channel_is_empty(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  if (st->read_pos == st->write_pos) {
    return true;
  }
  return false;
}

bool buffered_channel_is_full(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  if (st->write_pos == st->read_pos - 1 ||
      (st->read_pos == 0 &&
       st->write_pos == TOKENIZER_BUFFER_SIZE-1)) {
    return true;
  }
  return false;
}

bool buffered_read(lbm_char_channel_t *ch, char *res) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  char *buffer = st->buffer;
  bool ret = false;
  mutex_lock(&st->lock);
  if (!buffered_channel_is_empty(ch)) {
    *res = buffer[st->read_pos];
    st->read_pos = (st->read_pos + 1) % TOKENIZER_BUFFER_SIZE;
    ret = true;
  }
  mutex_unlock(&st->lock);
  return ret;
}

bool buffered_drop(lbm_char_channel_t *ch, unsigned int n) {
  bool r = true;
  char c;

  for (unsigned int i = 0; i < n; i ++) {
    r = buffered_read(ch, &c);
    if (r == false) break;
  }
  return r;
}

int buffered_write(lbm_char_channel_t *ch, char c) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  if (st->reader_closed) return CHANNEL_READER_CLOSED;
  int ret = CHANNEL_FULL;
  mutex_lock(&st->lock);
  char *buffer = st->buffer;
  if (!buffered_channel_is_full(ch)) {
    buffer[st->write_pos] = c;
    st->write_pos = (st->write_pos + 1) % TOKENIZER_BUFFER_SIZE;
    ret = CHANNEL_SUCCESS;
  }
  mutex_unlock(&st->lock);
  return ret;
}

unsigned int buffered_row(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  return st->row;
}

unsigned int buffered_column(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  return st->column;
}

bool buffered_comment(lbm_char_channel_t *ch) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  return st->comment;
}

void buffered_set_comment(lbm_char_channel_t *ch, bool comment) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)ch->state;
  st->comment = comment;
}

void lbm_create_buffered_char_channel(lbm_buffered_channel_state_t *st,
                                      lbm_char_channel_t *ch) {

  st->write_pos = 0;
  st->read_pos = 0;
  st->more = true;
  st->reader_closed = false;
  st->comment = false;
  st->row = 0;
  st->column = 0;

  mutex_init(&st->lock);

  ch->state = st;
  ch->more = buffered_more;
  ch->peek = buffered_peek;
  ch->read = buffered_read;
  ch->drop = buffered_drop;
  ch->comment = buffered_comment;
  ch->set_comment = buffered_set_comment;
  ch->channel_is_empty = buffered_channel_is_empty;
  ch->channel_is_full = buffered_channel_is_full;
  ch->write = buffered_write;
  ch->writer_close = buffered_writer_close;
  ch->reader_close = buffered_reader_close;
  ch->row = buffered_row;
  ch->column = buffered_column;
}

/* ------------------------------------------------------------
   Implementation string channel
   ------------------------------------------------------------ */

bool string_more(lbm_char_channel_t *ch) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  return st->more;
}

void string_writer_close(lbm_char_channel_t *ch) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  st->more = false;
}

void string_reader_close(lbm_char_channel_t *ch) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  st->reader_closed = true;
}

int string_peek(lbm_char_channel_t *ch, unsigned int n, char *res) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  char *str = st->str;

  unsigned int peek_pos = st->read_pos + n;

  if (peek_pos < strlen(str)) {
    *res = str[peek_pos];
    return CHANNEL_SUCCESS;
  }
  return CHANNEL_END;
}

bool string_channel_is_empty(lbm_char_channel_t *ch) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  if (st->read_pos == strlen(st->str)) {
    return true;
  }
  return false;
}

bool string_channel_is_full(lbm_char_channel_t *ch) {
  (void)ch;
  return true;
}

bool string_read(lbm_char_channel_t *ch, char *res) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  char *str = st->str;

  if (st->read_pos < strlen(str)) {
    *res = str[st->read_pos];
    if (*res == '\n') {
      st->row ++;
      st->column = 1;
    } else if (*res == 0) {
      st->more = false;
    } else {
      st->column++;
    }

    st->read_pos = st->read_pos + 1;
  } else {
    st->more = false;
  }
  return true;
}

bool string_drop(lbm_char_channel_t *ch, unsigned int n) {
  bool r = true;
  char c;

  if (n > 0) {
    do {
      r = string_read(ch, &c);
      n--;
    } while (n > 0 && r);
  }
  return r;
}

int string_write(lbm_char_channel_t *ch, char c) {
  (void) ch;
  (void) c;
  return CHANNEL_FULL;
}

unsigned int string_row(lbm_char_channel_t *ch) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  return st->row;
}

unsigned int string_column(lbm_char_channel_t *ch) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  return st->column;
}

bool string_comment(lbm_char_channel_t *ch) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  return st->comment;
}

void string_set_comment(lbm_char_channel_t *ch, bool comment) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)ch->state;
  st->comment = comment;
}

void lbm_create_string_char_channel(lbm_string_channel_state_t *st,
                                    lbm_char_channel_t *ch,
                                    char *str) {

  st->str = str;
  st->read_pos = 0;
  st->more = false;
  st->comment = false;
  st->row = 0;
  st->column = 0;

  ch->state = st;
  ch->more = string_more;
  ch->peek = string_peek;
  ch->read = string_read;
  ch->drop = string_drop;
  ch->comment = string_comment;
  ch->set_comment = string_set_comment;
  ch->channel_is_empty = string_channel_is_empty;
  ch->channel_is_full = string_channel_is_full;
  ch->write = string_write;
  ch->writer_close = string_writer_close;
  ch->reader_close = string_reader_close;
  ch->row = string_row;
  ch->column = string_column;
}
