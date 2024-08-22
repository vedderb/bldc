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
bool lbm_channel_more(lbm_char_channel_t *chan) {
  return chan->more(chan);
}

int lbm_channel_peek(lbm_char_channel_t *chan, unsigned int n, char *res) {
  return chan->peek(chan, n, res);
}

bool lbm_channel_read(lbm_char_channel_t *chan, char *res) {
  return chan->read(chan, res);
}

bool lbm_channel_drop(lbm_char_channel_t *chan, unsigned int n) {
  return chan->drop(chan, n);
}

bool lbm_channel_comment(lbm_char_channel_t *chan) {
  return chan->comment(chan);
}

void lbm_channel_set_comment(lbm_char_channel_t *chan, bool comment) {
  chan->set_comment(chan, comment);
}

bool lbm_channel_is_empty(lbm_char_channel_t *chan) {
  return chan->channel_is_empty(chan);
}

bool lbm_channel_is_full(lbm_char_channel_t *chan) {
  return chan->channel_is_full(chan);
}

int lbm_channel_write(lbm_char_channel_t *chan, char c) {
  return chan->write(chan, c);
}

void lbm_channel_writer_close(lbm_char_channel_t *chan) {
  chan->writer_close(chan);
}

void lbm_channel_reader_close(lbm_char_channel_t *chan) {
  chan->reader_close(chan);
}

bool lbm_channel_reader_is_closed(lbm_char_channel_t *chan) {
  return chan->reader_is_closed(chan);
}

unsigned int lbm_channel_row(lbm_char_channel_t *chan) {
  return chan->row(chan);
}

unsigned int lbm_channel_column(lbm_char_channel_t *chan) {
  return chan->column(chan);
}

/* ------------------------------------------------------------
   Implementation buffered channel
   ------------------------------------------------------------ */

bool buffered_more(lbm_char_channel_t *chan) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  return st->more;
}

void buffered_writer_close(lbm_char_channel_t *chan) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  st->more = false;
}

void buffered_reader_close(lbm_char_channel_t *chan) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  st->reader_closed = true;
}

bool buffered_reader_is_closed(lbm_char_channel_t *chan) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  return st->reader_closed;
}

int buffered_peek(lbm_char_channel_t *chan, unsigned int n, char *res) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
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
  } else if (!buffered_more(chan)) {
    ret = CHANNEL_END;
  } else if (buffered_more(chan)) {
    ret = CHANNEL_MORE;
  }
  mutex_unlock(&st->lock);
  return ret;
}

bool buffered_channel_is_empty(lbm_char_channel_t *chan) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  if (st->read_pos == st->write_pos) {
    return true;
  }
  return false;
}

bool buffered_channel_is_full(lbm_char_channel_t *chan) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  if (st->write_pos == st->read_pos - 1 ||
      (st->read_pos == 0 &&
       st->write_pos == TOKENIZER_BUFFER_SIZE-1)) {
    return true;
  }
  return false;
}

bool buffered_read(lbm_char_channel_t *chan, char *res) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  char *buffer = st->buffer;
  bool ret = false;
  mutex_lock(&st->lock);
  if (!buffered_channel_is_empty(chan)) {
    *res = buffer[st->read_pos];
    st->column++;
    if (*res == '\n') {
      st->column = 0;
      st->row ++;
    }
    st->read_pos = (st->read_pos + 1) % TOKENIZER_BUFFER_SIZE;
    ret = true;
  }
  mutex_unlock(&st->lock);
  return ret;
}

bool buffered_drop(lbm_char_channel_t *chan, unsigned int n) {
  bool r = true;
  char c;

  for (unsigned int i = 0; i < n; i ++) {
    r = buffered_read(chan, &c);
    if (r == false) break;
  }
  return r;
}

int buffered_write(lbm_char_channel_t *chan, char c) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  if (st->reader_closed) return CHANNEL_READER_CLOSED;
  int ret = CHANNEL_FULL;
  mutex_lock(&st->lock);
  char *buffer = st->buffer;
  if (!buffered_channel_is_full(chan)) {
    buffer[st->write_pos] = c;
    st->write_pos = (st->write_pos + 1) % TOKENIZER_BUFFER_SIZE;
    ret = CHANNEL_SUCCESS;
  }
  mutex_unlock(&st->lock);
  return ret;
}

unsigned int buffered_row(lbm_char_channel_t *chan) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  return st->row;
}

unsigned int buffered_column(lbm_char_channel_t *chan) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  return st->column;
}

bool buffered_comment(lbm_char_channel_t *chan) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  return st->comment;
}

void buffered_set_comment(lbm_char_channel_t *chan, bool comment) {
  lbm_buffered_channel_state_t *st = (lbm_buffered_channel_state_t*)chan->state;
  st->comment = comment;
}

void lbm_create_buffered_char_channel(lbm_buffered_channel_state_t *st,
                                      lbm_char_channel_t *chan) {

  st->write_pos = 0;
  st->read_pos = 0;
  st->more = true;
  st->reader_closed = false;
  st->comment = false;
  st->row = 1;
  st->column = 1;

  if (!st->mutex_initialized) {
    mutex_init(&st->lock);
    st->mutex_initialized = true;
  }

  chan->state = st;
  chan->more = buffered_more;
  chan->peek = buffered_peek;
  chan->read = buffered_read;
  chan->drop = buffered_drop;
  chan->comment = buffered_comment;
  chan->set_comment = buffered_set_comment;
  chan->channel_is_empty = buffered_channel_is_empty;
  chan->channel_is_full = buffered_channel_is_full;
  chan->write = buffered_write;
  chan->writer_close = buffered_writer_close;
  chan->reader_close = buffered_reader_close;
  chan->reader_is_closed = buffered_reader_is_closed;
  chan->row = buffered_row;
  chan->column = buffered_column;
}

/* ------------------------------------------------------------
   Implementation string channel
   ------------------------------------------------------------ */

bool string_more(lbm_char_channel_t *chan) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  return st->more;
}

void string_writer_close(lbm_char_channel_t *chan) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  st->more = false;
}

void string_reader_close(lbm_char_channel_t *chan) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  st->reader_closed = true;
}

bool string_reader_is_closed(lbm_char_channel_t *chan) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  return st->reader_closed;
}

int string_peek(lbm_char_channel_t *chan, unsigned int n, char *res) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  char *str = st->str;

  unsigned int peek_pos = st->read_pos + n;

  if (peek_pos < st->length) {
    *res = str[peek_pos];
    return CHANNEL_SUCCESS;
  }
  return CHANNEL_END;
}

bool string_channel_is_empty(lbm_char_channel_t *chan) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  if (st->read_pos == st->length) {
    return true;
  }
  return false;
}

bool string_channel_is_full(lbm_char_channel_t *chan) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  if (st->write_pos == st->length) {
    return true;
  }
  return false;
}

bool string_read(lbm_char_channel_t *chan, char *res) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  char *str = st->str;

  if (st->read_pos < st->length) {
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

bool string_drop(lbm_char_channel_t *chan, unsigned int n) {
  bool r = true;
  char c;

  if (n > 0) {
    do {
      r = string_read(chan, &c);
      n--;
    } while (n > 0 && r);
  }
  return r;
}

int string_write(lbm_char_channel_t *chan, char c) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  char *str = st->str;

  if (st->write_pos < st->length - 1) {
    str[st->write_pos] = c;
    st->write_pos = st->write_pos + 1;
  } else {
    return CHANNEL_FULL;
  }
  return CHANNEL_SUCCESS;
}

unsigned int string_row(lbm_char_channel_t *chan) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  return st->row;
}

unsigned int string_column(lbm_char_channel_t *chan) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  return st->column;
}

bool string_comment(lbm_char_channel_t *chan) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  return st->comment;
}

void string_set_comment(lbm_char_channel_t *chan, bool comment) {
  lbm_string_channel_state_t *st = (lbm_string_channel_state_t*)chan->state;
  st->comment = comment;
}

void lbm_create_string_char_channel(lbm_string_channel_state_t *st,
                                    lbm_char_channel_t *chan,
                                    char *str) {

  st->str = str;
  st->length = (unsigned int)strlen(str);
  st->read_pos = 0;
  st->write_pos = 0;
  st->more = false;
  st->comment = false;
  st->row = 1;
  st->column = 1;

  chan->state = st;
  chan->more = string_more;
  chan->peek = string_peek;
  chan->read = string_read;
  chan->drop = string_drop;
  chan->comment = string_comment;
  chan->set_comment = string_set_comment;
  chan->channel_is_empty = string_channel_is_empty;
  chan->channel_is_full = string_channel_is_full;
  chan->write = string_write;
  chan->writer_close = string_writer_close;
  chan->reader_close = string_reader_close;
  chan->reader_is_closed = string_reader_is_closed;
  chan->row = string_row;
  chan->column = string_column;
}

void lbm_create_string_char_channel_size(lbm_string_channel_state_t *st,
                                         lbm_char_channel_t *chan,
                                         char *str,
                                         unsigned int size) {
  st->str = str;
  st->length = size;
  st->read_pos = 0;
  st->write_pos = 0;
  st->more = false;
  st->comment = false;
  st->row = 1;
  st->column = 1;

  chan->state = st;
  chan->more = string_more;
  chan->peek = string_peek;
  chan->read = string_read;
  chan->drop = string_drop;
  chan->comment = string_comment;
  chan->set_comment = string_set_comment;
  chan->channel_is_empty = string_channel_is_empty;
  chan->channel_is_full = string_channel_is_full;
  chan->write = string_write;
  chan->writer_close = string_writer_close;
  chan->reader_close = string_reader_close;
  chan->reader_is_closed = string_reader_is_closed;
  chan->row = string_row;
  chan->column = string_column;
}
