/*
    Copyright 2019, 2022 Joel Svensson        svenssonjoel@yahoo.se

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
/** \file tokpar.h */

#ifndef TOKPAR_H_
#define TOKPAR_H_

#include "lbm_types.h"

/**
 * State struct for the string tokenizer.
 */
typedef struct {
  const char *str;
  unsigned int pos;
  unsigned int row;
  unsigned int column;
} lbm_tokenizer_string_state_t;

/** Create an lbm_tokenizer_string_state_t, lbm_tokenizer_char_stream_t pair from a string.
 *
 * \param state string tokenizer state is initialized in this arg.
 * \param stream tokenizer stream is created in this arg.
 * \param str String to tokenize.
 */
extern void lbm_create_char_stream_from_string(lbm_tokenizer_string_state_t *state,
                                               lbm_tokenizer_char_stream_t *stream,
                                               const char *str);
/** Get the next token from a tokenizer stream (lbm_tokenizer_char_stream_t).
 *
 * \param str Tokenizer stream to get the next token from.
 * \return an lbm_value representing the token. This token is semi-parsed
 *  at this stage and values for example are already in proper value form.
 */
extern lbm_value lbm_get_next_token(lbm_tokenizer_char_stream_t *str);

#endif
