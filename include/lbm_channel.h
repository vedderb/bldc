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

#ifndef LBM_CHANNEL_H_
#define LBM_CHANNEL_H_

#include <stdint.h>
#include <stdbool.h>
#include <platform_mutex.h>

#define TOKENIZER_BUFFER_SIZE 257

#define CHANNEL_SUCCESS    1 
#define CHANNEL_MORE       2
#define CHANNEL_END        3
#define CHANNEL_FULL       4

#define CHANNEL_READER_CLOSED 1000


typedef struct {
  char buffer[TOKENIZER_BUFFER_SIZE];
  unsigned int write_pos;
  unsigned int read_pos;
  bool more;
  bool comment;
  bool reader_closed; 
  mutex_t lock;
  // statistics
  unsigned int row;
  unsigned int column;
} lbm_buffered_channel_state_t;


typedef struct {
  char *str;
  unsigned int read_pos;
  bool more;
  bool comment;
  bool reader_closed;
  unsigned int row;
  unsigned int column;
} lbm_string_channel_state_t; 

typedef struct lbm_char_channel_s {

  void *state;
  bool (*more)(struct lbm_char_channel_s *chan);
  int  (*peek)(struct lbm_char_channel_s *chan, unsigned int n, char *res);
  bool (*read)(struct lbm_char_channel_s *chan, char *res);
  bool (*drop)(struct lbm_char_channel_s *chan, unsigned int n);
  bool (*comment)(struct lbm_char_channel_s *chan);
  void (*set_comment)(struct lbm_char_channel_s *chan, bool comment);
  void (*reader_close)(struct lbm_char_channel_s *chan);
  
  /* Either side */
  bool (*channel_is_empty)(struct lbm_char_channel_s *chan);
  bool (*channel_is_full)(struct lbm_char_channel_s *chan);
  bool (*reader_is_closed)(struct lbm_char_channel_s *chan);
  
  /* Write side */
  int (*write)(struct lbm_char_channel_s *chan, char c);
  void (*writer_close)(struct lbm_char_channel_s *chan);
  
  /* Statistics */
  unsigned int (*row)(struct lbm_char_channel_s *chan);
  unsigned int (*column)(struct lbm_char_channel_s *chan);
  
} lbm_char_channel_t;


/* Read side */
bool lbm_channel_more(lbm_char_channel_t *chan);
int lbm_channel_peek(lbm_char_channel_t *chan, unsigned int n, char *res);
bool lbm_channel_read(lbm_char_channel_t *chan, char *res);
bool lbm_channel_drop(lbm_char_channel_t *chan, unsigned int n);
bool lbm_channel_comment(lbm_char_channel_t *chan);
void lbm_channel_set_comment(lbm_char_channel_t *chan, bool comment);
void lbm_channel_reader_close(lbm_char_channel_t *chan);
bool lbm_channel_reader_is_closed(lbm_char_channel_t *chan);

/* Either side */
bool lbm_channel_is_empty(lbm_char_channel_t *chan);
bool lbm_channel_is_full(lbm_char_channel_t *chan);

/* Write side */
int  lbm_channel_write(lbm_char_channel_t *chan, char c);
void lbm_channel_writer_close(lbm_char_channel_t *chan);

/* Statistics */
unsigned int lbm_channel_row(lbm_char_channel_t *chan);
unsigned int lbm_channel_column(lbm_char_channel_t *chan);


/* Interface */
void lbm_create_string_char_channel(lbm_string_channel_state_t *st,
                                    lbm_char_channel_t *chan,
                                    char *str);

void lbm_create_buffered_char_channel(lbm_buffered_channel_state_t *st,
                                      lbm_char_channel_t *chan);


#endif
