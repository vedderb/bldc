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
/** \file lbm_channel.h */

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

/** Struct holding the state for a buffered character channel
 */
typedef struct {
  char buffer[TOKENIZER_BUFFER_SIZE];
  unsigned int write_pos;
  unsigned int read_pos;
  bool more;
  bool comment;
  bool reader_closed;
  mutex_t lock;
  bool mutex_initialized;
  // statistics
  unsigned int row;
  unsigned int column;
} lbm_buffered_channel_state_t;

/** Struct holding the state of a character channel
 *  backed by a string.
 */
typedef struct {
  char *str;
  unsigned int length;
  unsigned int read_pos;
  unsigned int write_pos;
  bool more;
  bool comment;
  bool reader_closed;
  unsigned int row;
  unsigned int column;
} lbm_string_channel_state_t;

/** Struct describing the interface to a character channel.
 */
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
/** Check if there are more characters comming on a character channel.
 *  This returns false if the sender has closed the send side of the
 *  channel.
 * \param chan The channel.
 * \return True if sender end is still open false if closed.
 */
bool lbm_channel_more(lbm_char_channel_t *chan);

/** Peek into a character channel.
 *  \param chan The channel to peek into.
 *  \param n The position to peek at.
 *  \param res Pointer to a character that will hold the result.
 *  \return
 *       - CHANNEL_SUCCESS: successfully peeked.
 *       - CHANNEL_MORE: The data you try to peek at has not yet arrived into the channel.
 *       - CHANNEL_END: The sender side is closed and you are peeking outside of valid data.
 */
int lbm_channel_peek(lbm_char_channel_t *chan, unsigned int n, char *res);

/** Read a character from the head of the channel.
 * \param chan The channel to read from.
 * \param res The resulting character is stored here.
 * \return true on success, otherwise false.
 */
bool lbm_channel_read(lbm_char_channel_t *chan, char *res);

/** Drop n characters from a channel.
 * \param chan The channel to drop characters from.
 * \param n The number of characters to drop.
 * \return true on successfully dropping n characters, false otherwise.
 */
bool lbm_channel_drop(lbm_char_channel_t *chan, unsigned int n);

/** Comment mode. Check if a channel is currently in comment mode.
 * \param chan The channel.
 * \return true if the channel is in comment mode, false otherwise.
 */
bool lbm_channel_comment(lbm_char_channel_t *chan);

/** Enter into or exit from comment mode.
 * \param chan Channel to change mode of.
 * \param comment True to enter into  comment mode and false to exit.
 */
void lbm_channel_set_comment(lbm_char_channel_t *chan, bool comment);

/** Close a channel reader side.
 * \param chan Channel to close the reader side of.
 */
void lbm_channel_reader_close(lbm_char_channel_t *chan);

/** Check if the reader side of channel is closed.
 *  \param chan Channel to query.
 *  \return True if closed, false otherwise.
 */
bool lbm_channel_reader_is_closed(lbm_char_channel_t *chan);

/* Either side */

/** Check if a channel is empty. Data cannot be read from an empty channel.
 * \param chan Channel to query.
 * \return True if empty, false otherwise.
 */
bool lbm_channel_is_empty(lbm_char_channel_t *chan);

/** Check if a channel is full. Data cannot be written into a full channel.
 * \param chan Channel to query.
 * \return True if channel is full, false otherwise.
 */
bool lbm_channel_is_full(lbm_char_channel_t *chan);

/* Write side */

/** Write a character onto the end of a channel.
 * \param chan Channel to write to.
 * \param c Character to place onto the channel.
 * \return
 *       - CHANNEL_SUCCESS: Successfully wrote.
 *       - CHANNEL_READER_CLOSED: The reader end is closed, you should abort writing.
 *       - CHANNEL_FULL: The channel is full and cannot accept the character. Try again later.
 */
int  lbm_channel_write(lbm_char_channel_t *chan, char c);

/** Close the writer side of a channel.
 * \ param chan The channel to close the writer side of.
 */
void lbm_channel_writer_close(lbm_char_channel_t *chan);

/* Statistics */

/** Obtain current row number from a channel.
 * \param chan The channel to query.
 * \return The current row as seen by the calls to "read".
 */
unsigned int lbm_channel_row(lbm_char_channel_t *chan);

/** Obtain current column from a channel.
 * \param chan The channel to query.
 */
unsigned int lbm_channel_column(lbm_char_channel_t *chan);


/* Interface */
/** Create a channel from a string. This channel can be read from but not
 *  written to.
 *  \param st Pointer to lbm_string_channel_state.
 *  \param chan Pointer to lbm_char_channel_t.
 *  \param str The string to base the channel contents upon.
 */
void lbm_create_string_char_channel(lbm_string_channel_state_t *st,
                                    lbm_char_channel_t *chan,
                                    char *str);

void lbm_create_string_char_channel_size(lbm_string_channel_state_t *st,
                                         lbm_char_channel_t *chan,
                                         char *str,
                                         unsigned int size);


/** Create a buffered channel that can be read from and written to.
 * \param st Pointer to lbm_buffered_channel_state_t.
 * \param chan Pointer to lbm_char_channel_t.
 */
void lbm_create_buffered_char_channel(lbm_buffered_channel_state_t *st,
                                      lbm_char_channel_t *chan);


#endif
