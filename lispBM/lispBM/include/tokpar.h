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
#include "lbm_channel.h"

#define TOKOPENPAR      1u      // "("
#define TOKCLOSEPAR     2u      // ")"
#define TOKOPENBRACK    3u      // "["
#define TOKCLOSEBRACK   4u      // "]"
#define TOKDOT          5u      // "."
#define TOKDONTCARE     6u      // "_"
#define TOKQUOTE        7u      // "'"
#define TOKBACKQUOTE    8u      // "`"
#define TOKCOMMAAT      9u      // ",@"
#define TOKCOMMA        10u     // ","
#define TOKMATCHANY     11u     // "?"
#define TOKOPENCURL     12u     // "{"
#define TOKCLOSECURL    13u     // "}"
#define TOKCONSTSTART   14u     // "@const-start"
#define TOKCONSTEND     15u     // "@const-end"
#define TOKCONSTSYMSTR  16u     // "@const-symbol-strings"

#define TOKTYPEBYTE     100u
#define TOKTYPEI        101u
#define TOKTYPEU        102u
#define TOKTYPEI32      103u
#define TOKTYPEU32      104u
#define TOKTYPEI64      105u
#define TOKTYPEU64      106u
#define TOKTYPEF32      107u
#define TOKTYPEF64      108u


#define TOKENIZER_ERROR 1024u
#define TOKENIZER_END   2048u

// Tokenizer return values
// > 0 : Successfully found token
// = 0 : Tokenizer can definitely not create a token
// = -1 : Tokenizer does not know if it can or cannot create a token yet.
// = -2 : Tokenizer was reading a string but ran out of space (for example).
//        This is an error!

#define TOKENIZER_NO_TOKEN   0
#define TOKENIZER_NEED_MORE -1
#define TOKENIZER_STRING_ERROR -2
#define TOKENIZER_CHAR_ERROR -3

#define TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH 256

// This is shared state between all ongoing read tasks. Maybe risky?
// Need to take care when dealing with this array in the reader.
extern char tokpar_sym_str[TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH];

#ifdef __cplusplus
extern "C" {
#endif
#if 0
}
#endif
/**to
 * State struct for the string tokenizer.
 */
typedef struct {
  const char *str;
  unsigned int pos;
  unsigned int row;
  unsigned int column;
} lbm_tokenizer_string_state_t;

typedef struct {
  uint32_t type;
  uint64_t value;
  bool negative;
} token_int;

typedef struct token_float {
  uint32_t type;
  double value;
  bool negative;
} token_float;

/** Attempt to read a "syntax" token from the character stream.
 *  A "syntax" token is for example '(', ')', '@' and so on.
 *  \param chan Character channel to read characters from.
 *  \param res Result token identifier.
 *  \return A positive value indicating number of characters used if successful. Otherwise a negative status indicator.
 */
int tok_syntax(lbm_char_channel_t *chan, uint32_t *res);
/** Attempt to read a symbol from the character stream.
 * If successful the resulting symbol string is found in tokpar_sym_str.
 * \param chan Character channel to read characters from.
 * \return A positive value indicating number of characters used if successful. Otherwise a negative status indicator.
 */
int tok_symbol(lbm_char_channel_t *chan);
/** Attempt to read a string from the character stream.
 * If successful the resulting string is found in tokpar_sym_str.
 * \param chan Character channel to read characters from.
 * \return A positive value indicating number of characters used if successful. Otherwise a negative status indicator.
 */
int tok_string(lbm_char_channel_t *chan, unsigned int *string_len);
/** Attempt to read a character from the character stream.
 * \param chan Character channel to read characters from.
 * \param res Result character if successful.
 * \return A positive value indicating number of characters used if successful. Otherwise a negative status indicator.
 */
int tok_char(lbm_char_channel_t *chan, char *res);
/** Attempt to read a floating point number from the character stream.
 * \param chan Character channel to read characters from.
 * \param res Result token_float-object if successful.
 * \return A positive value indicating number of characters used if successful. Otherwise a negative status indicator.
 */
int tok_double(lbm_char_channel_t *chan, token_float *result);
/** Attempt to read an integer value from the character stream.
 * \param chan Character channel to read characters from.
 * \param res Result token_int-object if successful.
 * \return A positive value indicating number of characters used if successful. Otherwise a negative status indicator.
 */
int tok_integer(lbm_char_channel_t *chan, token_int *result);
/** Clean off whitespace from head of the character stream
 * \return True if whitespace could be cleaned. False if stream has no more available characters at the moment.
 */
bool tok_clean_whitespace(lbm_char_channel_t *chan);

#ifdef __cplusplus
}
#endif
#endif
