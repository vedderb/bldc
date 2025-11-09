#define _GNU_SOURCE // MAP_ANON
#define _POSIX_C_SOURCE 200809L // nanosleep?
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

#include "lispbm.h"
#include "tokpar.h"
#include "lbm_channel.h"

#include "init/start_lispbm.c"

// Helper function to write string to buffered channel
static void write_string_to_channel(lbm_char_channel_t *chan, const char *str) {
  for (size_t i = 0; i < strlen(str); i++) {
    lbm_channel_write(chan, str[i]);
  }
}

// Helper function to setup channel with data
static void setup_channel_with_data(lbm_buffered_channel_state_t *bs,
                                   lbm_char_channel_t *chan,
                                   const char *data) {
  // Zero-initialize the buffered channel state to ensure mutex_initialized is false
  memset(bs, 0, sizeof(lbm_buffered_channel_state_t));
  lbm_create_buffered_char_channel(bs, chan);
  // Clear tokpar symbol string buffer
  memset(tokpar_sym_str, 0, TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH + 1);
  if (data) {
    write_string_to_channel(chan, data);
    // Close writer side to signal no more data is coming
    lbm_channel_writer_close(chan);
  }
}

// Helper function to setup empty closed channel
static void setup_empty_closed_channel(lbm_buffered_channel_state_t *bs,
                                      lbm_char_channel_t *chan) {
  // Zero-initialize the buffered channel state to ensure mutex_initialized is false
  memset(bs, 0, sizeof(lbm_buffered_channel_state_t));
  lbm_create_buffered_char_channel(bs, chan);
  // Clear tokpar symbol string buffer
  memset(tokpar_sym_str, 0, TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH + 1);
  // Close writer side immediately to signal no more data is coming
  lbm_channel_writer_close(chan);
}

// ////////////////////////////////////////////////////////////
// tok_syntax tests

int test_tok_syntax_no_data(void) {
  // LispBM already initialized in main()
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, NULL);
  
  uint32_t res;
  int r = tok_syntax(&chan, &res);
  
  // Should return TOKENIZER_NEED_MORE when no data available
  if (r != TOKENIZER_NEED_MORE) {
    printf("FAIL: test_tok_syntax_no_data - expected %d, got %d\n", TOKENIZER_NEED_MORE, r);
    return 0;
  }
  
  return 1;
}

int test_tok_syntax_open_paren(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "(");
  
  uint32_t res;
  int r = tok_syntax(&chan, &res);
  
  if (r != 1 || res != TOKOPENPAR) {
    printf("FAIL: test_tok_syntax_open_paren - expected r=1, res=%u, got r=%d, res=%u\n", TOKOPENPAR, r, res);
    return 0;
  }
  
  return 1;
}

int test_tok_syntax_close_paren(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, ")");
  
  uint32_t res;
  int r = tok_syntax(&chan, &res);
  
  if (r != 1 || res != TOKCLOSEPAR) {
    printf("FAIL: test_tok_syntax_close_paren - expected r=1, res=%u, got r=%d, res=%u\n", TOKCLOSEPAR, r, res);
    return 0;
  }
  
  return 1;
}

int test_tok_syntax_array_open(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "[|");
  
  uint32_t res;
  int r = tok_syntax(&chan, &res);
  
  if (r != 2 || res != TOKOPENARRAY) {
    printf("FAIL: test_tok_syntax_array_open - expected r=2, res=%u, got r=%d, res=%u\n", TOKOPENARRAY, r, res);
    return 0;
  }
  
  return 1;
}

int test_tok_syntax_invalid_data(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "abc");
  
  uint32_t res;
  int r = tok_syntax(&chan, &res);
  
  // Should return TOKENIZER_NO_TOKEN for non-syntax characters
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_syntax_invalid_data - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

// ////////////////////////////////////////////////////////////
// tok_symbol tests

int test_tok_symbol_no_data(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, NULL);
  
  int r = tok_symbol(&chan);
  
  if (r != TOKENIZER_NEED_MORE) {
    printf("FAIL: test_tok_symbol_no_data - expected %d, got %d\n", TOKENIZER_NEED_MORE, r);
    return 0;
  }
  
  return 1;
}

int test_tok_symbol_valid_simple_delimiter(void) {

  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  // Zero-initialize the buffered channel state to ensure mutex_initialized is false
  memset(&bs, 0, sizeof(lbm_buffered_channel_state_t));
  lbm_create_buffered_char_channel(&bs, &chan);
  memset(tokpar_sym_str, 0, TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH + 1);
  write_string_to_channel(&chan, "hello ");  // Space delimiter, don't close channel
  
  int r = tok_symbol(&chan);
  
  if (r <= 0) {
    printf("FAIL: test_tok_symbol_valid_simple_delimiter - expected positive return, got %d\n", r);
    return 0;
  }
  if (strcmp(tokpar_sym_str, "hello") != 0) {
    printf("FAIL: test_tok_symbol_valid_simple_delimiter - expected 'hello', got '%s'\n", tokpar_sym_str);
    return 0;
  }
  
  return 1;
}

int test_tok_symbol_valid_simple_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "hello");  // No delimiter, but channel closed
  
  int r = tok_symbol(&chan);
  
  if (r <= 0) {
    printf("FAIL: test_tok_symbol_valid_simple_closed - expected positive return, got %d\n", r);
    return 0;
  }
  if (strcmp(tokpar_sym_str, "hello") != 0) {
    printf("FAIL: test_tok_symbol_valid_simple_closed - expected 'hello', got '%s'\n", tokpar_sym_str);
    return 0;
  }
  
  return 1;
}

int test_tok_symbol_invalid_start_number(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "123abc");
  
  int r = tok_symbol(&chan);
  
  // Should return TOKENIZER_NO_TOKEN as symbols can't start with numbers
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_symbol_invalid_start_number - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

// ////////////////////////////////////////////////////////////
// tok_string tests

int test_tok_string_no_data(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, NULL);
  
  unsigned int string_len;
  int r = tok_string(&chan, &string_len);
  
  if (r != TOKENIZER_NEED_MORE) {
    printf("FAIL: test_tok_string_no_data - expected %d, got %d\n", TOKENIZER_NEED_MORE, r);
    return 0;
  }
  
  return 1;
}

int test_tok_string_valid_simple_delimiter(void) {

  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  // Zero-initialize the buffered channel state to ensure mutex_initialized is false
  memset(&bs, 0, sizeof(lbm_buffered_channel_state_t));
  lbm_create_buffered_char_channel(&bs, &chan);
  memset(tokpar_sym_str, 0, TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH + 1);
  write_string_to_channel(&chan, "\"hello\" ");  // Space delimiter after string
  
  unsigned int string_len;
  int r = tok_string(&chan, &string_len);
  
  if (r <= 0) {
    printf("FAIL: test_tok_string_valid_simple_delimiter - expected positive return, got %d\n", r);
    return 0;
  }
  if (strcmp(tokpar_sym_str, "hello") != 0) {
    printf("FAIL: test_tok_string_valid_simple_delimiter - expected 'hello', got '%s'\n", tokpar_sym_str);
    return 0;
  }
  if (string_len != 5) {
    printf("FAIL: test_tok_string_valid_simple_delimiter - expected length 5, got %u\n", string_len);
    return 0;
  }
  
  return 1;
}

int test_tok_string_valid_simple_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\"hello\"");  // No delimiter, channel closed
  
  unsigned int string_len;
  int r = tok_string(&chan, &string_len);
  
  if (r <= 0) {
    printf("FAIL: test_tok_string_valid_simple_closed - expected positive return, got %d\n", r);
    return 0;
  }
  if (strcmp(tokpar_sym_str, "hello") != 0) {
    printf("FAIL: test_tok_string_valid_simple_closed - expected 'hello', got '%s'\n", tokpar_sym_str);
    return 0;
  }
  if (string_len != 5) {
    printf("FAIL: test_tok_string_valid_simple_closed - expected length 5, got %u\n", string_len);
    return 0;
  }
  
  return 1;
}

int test_tok_string_invalid_start(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "hello");
  
  unsigned int string_len;
  int r = tok_string(&chan, &string_len);
  
  // Should return TOKENIZER_NO_TOKEN as strings must start with quote
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_string_invalid_start - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

// ////////////////////////////////////////////////////////////
// tok_char tests

int test_tok_char_no_data(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, NULL);
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r != TOKENIZER_NEED_MORE) {
    printf("FAIL: test_tok_char_no_data - expected %d, got %d\n", TOKENIZER_NEED_MORE, r);
    return 0;
  }
  
  return 1;
}

int test_tok_char_valid_simple_delimiter(void) {

  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  // Zero-initialize the buffered channel state to ensure mutex_initialized is false
  memset(&bs, 0, sizeof(lbm_buffered_channel_state_t));
  lbm_create_buffered_char_channel(&bs, &chan);
  memset(tokpar_sym_str, 0, TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH + 1);
  write_string_to_channel(&chan, "\\#a ");  // Space delimiter
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_valid_simple_delimiter - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != 'a') {
    printf("FAIL: test_tok_char_valid_simple_delimiter - expected 'a', got '%c'\n", res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_valid_simple_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#a");  // No delimiter, channel closed
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_valid_simple_closed - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != 'a') {
    printf("FAIL: test_tok_char_valid_simple_closed - expected 'a', got '%c'\n", res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_null(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\0");  // \0 - null character
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_null - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\0') {
    printf("FAIL: test_tok_char_escape_null - expected '\\0', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_bell(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\a");  // \a - bell character
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_bell - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\a') {
    printf("FAIL: test_tok_char_escape_bell - expected '\\a', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_backspace(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\b");  // \b - backspace
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_backspace - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\b') {
    printf("FAIL: test_tok_char_escape_backspace - expected '\\b', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_tab(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\t");  // \t - tab
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_tab - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\t') {
    printf("FAIL: test_tok_char_escape_tab - expected '\\t', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_newline(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\n");  // \n - newline
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_newline - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\n') {
    printf("FAIL: test_tok_char_escape_newline - expected '\\n', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_vtab(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\v");  // \v - vertical tab
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_vtab - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\v') {
    printf("FAIL: test_tok_char_escape_vtab - expected '\\v', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_formfeed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\f");  // \f - formfeed
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_formfeed - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\f') {
    printf("FAIL: test_tok_char_escape_formfeed - expected '\\f', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_carriage_return(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\r");  // \r - carriage return
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_carriage_return - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\r') {
    printf("FAIL: test_tok_char_escape_carriage_return - expected '\\r', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_escape(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\e");  // \e - escape character (27)
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_escape - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != 27) {
    printf("FAIL: test_tok_char_escape_escape - expected escape char (27), got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_space(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\s");  // \s - space character (32)
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_space - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != 32) {
    printf("FAIL: test_tok_char_escape_space - expected space char (32), got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_quote(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\\"");  // \" - double quote
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_quote - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\"') {
    printf("FAIL: test_tok_char_escape_quote - expected '\"', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_backslash(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\\\");  // \\ - backslash
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_backslash - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != '\\') {
    printf("FAIL: test_tok_char_escape_backslash - expected '\\', got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_delete(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\d");  // \d - delete character (127)
  
  char res;
  int r = tok_char(&chan, &res);
  
  if (r <= 0) {
    printf("FAIL: test_tok_char_escape_delete - expected positive return, got %d\n", r);
    return 0;
  }
  if (res != 127) {
    printf("FAIL: test_tok_char_escape_delete - expected delete char (127), got '%c' (%d)\n", res, (int)res);
    return 0;
  }
  
  return 1;
}

int test_tok_char_escape_invalid(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "\\#\\z");  // \z - invalid escape sequence
  
  char res;
  int r = tok_char(&chan, &res);
  
  // Should return an error for invalid escape sequence
  if (r >= 0) {
    printf("FAIL: test_tok_char_escape_invalid - expected error, got %d\n", r);
    return 0;
  }
  
  return 1;
}

int test_tok_char_invalid_start(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "abc");
  
  char res;
  int r = tok_char(&chan, &res);
  
  // Should return TOKENIZER_NO_TOKEN as chars must start with "#\"
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_char_invalid_start - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

// ////////////////////////////////////////////////////////////
// tok_integer tests

int test_tok_integer_no_data(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, NULL);
  
  token_int result;
  int r = tok_integer(&chan, &result);
  
  if (r != TOKENIZER_NEED_MORE) {
    printf("FAIL: test_tok_integer_no_data - expected %d, got %d\n", TOKENIZER_NEED_MORE, r);
    return 0;
  }
  
  return 1;
}

int test_tok_integer_positive(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "42");
  
  token_int result;
  int r = tok_integer(&chan, &result);
  
  if (r <= 0) {
    printf("FAIL: test_tok_integer_positive - expected positive return, got %d\n", r);
    return 0;
  }
  if (result.value != 42 || result.negative) {
    printf("FAIL: test_tok_integer_positive - expected value=42, negative=false, got value=%llu, negative=%d\n", (unsigned long long)result.value, result.negative);
    return 0;
  }
  
  return 1;
}

int test_tok_integer_invalid_start(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "abc");
  
  token_int result;
  int r = tok_integer(&chan, &result);
  
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_integer_invalid_start - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

// ////////////////////////////////////////////////////////////
// tok_double tests

int test_tok_double_no_data(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, NULL);
  
  token_float result;
  int r = tok_double(&chan, &result);
  
  if (r != TOKENIZER_NEED_MORE) {
    printf("FAIL: test_tok_double_no_data - expected %d, got %d\n", TOKENIZER_NEED_MORE, r);
    return 0;
  }
  
  return 1;
}

int test_tok_double_simple(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "3.14");
  
  token_float result;
  int r = tok_double(&chan, &result);
  
  if (r <= 0) {
    printf("FAIL: test_tok_double_simple - expected positive return, got %d\n", r);
    return 0;
  }
  if (result.value < 3.13 || result.value > 3.15) {
    printf("FAIL: test_tok_double_simple - expected ~3.14, got %f\n", result.value);
    return 0;
  }
  if (result.negative) {
    printf("FAIL: test_tok_double_simple - expected negative=false, got %d\n", result.negative);
    return 0;
  }
  
  return 1;
}

int test_tok_double_invalid_start(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "abc");
  
  token_float result;
  int r = tok_double(&chan, &result);
  
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_double_invalid_start - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

// ////////////////////////////////////////////////////////////
// tok_clean_whitespace tests

int test_tok_clean_whitespace_no_data(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, NULL);
  
  bool r = tok_clean_whitespace(&chan);
  
  // Should return false when no data available
  if (r) {
    printf("FAIL: test_tok_clean_whitespace_no_data - expected false when no data available\n");
    return 0;
  }
  
  return 1;
}

int test_tok_clean_whitespace_spaces(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "   hello");
  
  bool r = tok_clean_whitespace(&chan);
  
  if (!r) {
    printf("FAIL: test_tok_clean_whitespace_spaces - expected true\n");
    return 0;
  }
  
  // Check that 'h' is now the first character
  char next_char;
  if (!lbm_channel_read(&chan, &next_char)) {
    printf("FAIL: test_tok_clean_whitespace_spaces - failed to read next character\n");
    return 0;
  }
  if (next_char != 'h') {
    printf("FAIL: test_tok_clean_whitespace_spaces - expected 'h', got '%c'\n", next_char);
    return 0;
  }
  
  return 1;
}

// ////////////////////////////////////////////////////////////
// Empty closed channel tests

int test_tok_syntax_empty_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_empty_closed_channel(&bs, &chan);  // Empty and closed
  
  uint32_t res;
  int r = tok_syntax(&chan, &res);
  
  // Should return TOKENIZER_NO_TOKEN when channel is empty and closed
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_syntax_empty_closed - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

int test_tok_symbol_empty_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_empty_closed_channel(&bs, &chan);  // Empty and closed
  
  int r = tok_symbol(&chan);
  
  // Should return TOKENIZER_NO_TOKEN when channel is empty and closed
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_symbol_empty_closed - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

int test_tok_string_empty_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_empty_closed_channel(&bs, &chan);  // Empty and closed
  
  unsigned int string_len;
  int r = tok_string(&chan, &string_len);
  
  // Should return TOKENIZER_NO_TOKEN when channel is empty and closed
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_string_empty_closed - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

int test_tok_char_empty_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_empty_closed_channel(&bs, &chan);  // Empty and closed
  
  char res;
  int r = tok_char(&chan, &res);
  
  // Should return TOKENIZER_NO_TOKEN when channel is empty and closed
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_char_empty_closed - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

int test_tok_integer_empty_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_empty_closed_channel(&bs, &chan);  // Empty and closed
  
  token_int result;
  int r = tok_integer(&chan, &result);
  
  // Should return TOKENIZER_NO_TOKEN when channel is empty and closed
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_integer_empty_closed - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

int test_tok_double_empty_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_empty_closed_channel(&bs, &chan);  // Empty and closed
  
  token_float result;
  int r = tok_double(&chan, &result);
  
  // Should return TOKENIZER_NO_TOKEN when channel is empty and closed
  if (r != TOKENIZER_NO_TOKEN) {
    printf("FAIL: test_tok_double_empty_closed - expected %d, got %d\n", TOKENIZER_NO_TOKEN, r);
    return 0;
  }
  
  return 1;
}

int test_tok_clean_whitespace_empty_closed(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_empty_closed_channel(&bs, &chan);  // Empty and closed
  
  bool r = tok_clean_whitespace(&chan);
  
  // Should return true when channel is empty and closed (cleaning is complete)
  if (!r) {
    printf("FAIL: test_tok_clean_whitespace_empty_closed - expected true when channel is empty and closed\n");
    return 0;
  }
  
  return 1;
}

// ////////////////////////////////////////////////////////////
// Integration test

int test_tokenize_simple_expression(void) {
  
  lbm_buffered_channel_state_t bs;
  lbm_char_channel_t chan;
  setup_channel_with_data(&bs, &chan, "(+ 1 2)");
  int n = 0;
    
  // Should tokenize as: ( + 1 2 )
  uint32_t syntax_res;
  token_int int_res;
  
  // Test opening paren
  tok_clean_whitespace(&chan);
  n = tok_syntax(&chan, &syntax_res);
  if ( n <= 0 || syntax_res != TOKOPENPAR) {
    printf("FAIL: test_tokenize_simple_expression - step 1 (open paren)\n");
    return 0;
  }
  lbm_channel_drop(&chan, (unsigned int) n);
  
  // Test symbol +
  tok_clean_whitespace(&chan);
  n = tok_symbol(&chan);
  if (n <= 0 || strncmp(tokpar_sym_str, "+", 1) != 0) {
    printf("FAIL: test_tokenize_simple_expression - step 2 (+ symbol), got '%s'\n", tokpar_sym_str);
    return 0;
  }
  lbm_channel_drop(&chan, (unsigned int) n);
  
  // Test integer 1
  tok_clean_whitespace(&chan);
  n = tok_integer(&chan, &int_res);
  if (n <= 0 || int_res.value != 1) {
    printf("FAIL: test_tokenize_simple_expression - step 3 (integer 1), got %llu\n", (unsigned long long)int_res.value);
    return 0;
  }
  lbm_channel_drop(&chan, (unsigned int) n);
  
  // Test integer 2
  tok_clean_whitespace(&chan);
  n = tok_integer(&chan, &int_res);
  if (n <= 0 || int_res.value != 2) {
    printf("FAIL: test_tokenize_simple_expression - step 4 (integer 2), got %llu\n", (unsigned long long)int_res.value);
    return 0;
  }
  lbm_channel_drop(&chan, (unsigned int) n);
  
  // Test closing paren
  tok_clean_whitespace(&chan);
  n = tok_syntax(&chan, &syntax_res);
  if (n <= 0 || syntax_res != TOKCLOSEPAR) {
    printf("FAIL: test_tokenize_simple_expression - step 5 (close paren)\n");
    return 0;
  }
  lbm_channel_drop(&chan, (unsigned int) n);
  
  return 1;
}

// ////////////////////////////////////////////////////////////
// run the tests
int main(void) {
  int tests_passed = 0;
  int total_tests = 0;

  // Initialize LispBM once for all tests
  if (!start_lispbm_for_tests()) {
    printf("FAILED: Could not initialize LispBM\n");
    return 1;
  }

  // tok_syntax tests
  total_tests++; if (test_tok_syntax_no_data()) tests_passed++;
  total_tests++; if (test_tok_syntax_open_paren()) tests_passed++;
  total_tests++; if (test_tok_syntax_close_paren()) tests_passed++;
  total_tests++; if (test_tok_syntax_array_open()) tests_passed++;
  total_tests++; if (test_tok_syntax_invalid_data()) tests_passed++;

  // tok_symbol tests
  total_tests++; if (test_tok_symbol_no_data()) tests_passed++;
  total_tests++; if (test_tok_symbol_valid_simple_delimiter()) tests_passed++;
  total_tests++; if (test_tok_symbol_valid_simple_closed()) tests_passed++;
  total_tests++; if (test_tok_symbol_invalid_start_number()) tests_passed++;

  // tok_string tests
  total_tests++; if (test_tok_string_no_data()) tests_passed++;
  total_tests++; if (test_tok_string_valid_simple_delimiter()) tests_passed++;
  total_tests++; if (test_tok_string_valid_simple_closed()) tests_passed++;
  total_tests++; if (test_tok_string_invalid_start()) tests_passed++;

  // tok_char tests
  total_tests++; if (test_tok_char_no_data()) tests_passed++;
  total_tests++; if (test_tok_char_valid_simple_delimiter()) tests_passed++;
  total_tests++; if (test_tok_char_valid_simple_closed()) tests_passed++;
  total_tests++; if (test_tok_char_escape_null()) tests_passed++;
  total_tests++; if (test_tok_char_escape_bell()) tests_passed++;
  total_tests++; if (test_tok_char_escape_backspace()) tests_passed++;
  total_tests++; if (test_tok_char_escape_tab()) tests_passed++;
  total_tests++; if (test_tok_char_escape_newline()) tests_passed++;
  total_tests++; if (test_tok_char_escape_vtab()) tests_passed++;
  total_tests++; if (test_tok_char_escape_formfeed()) tests_passed++;
  total_tests++; if (test_tok_char_escape_carriage_return()) tests_passed++;
  total_tests++; if (test_tok_char_escape_escape()) tests_passed++;
  total_tests++; if (test_tok_char_escape_space()) tests_passed++;
  total_tests++; if (test_tok_char_escape_quote()) tests_passed++;
  total_tests++; if (test_tok_char_escape_backslash()) tests_passed++;
  total_tests++; if (test_tok_char_escape_delete()) tests_passed++;
  total_tests++; if (test_tok_char_escape_invalid()) tests_passed++;
  total_tests++; if (test_tok_char_invalid_start()) tests_passed++;

  // tok_integer tests
  total_tests++; if (test_tok_integer_no_data()) tests_passed++;
  total_tests++; if (test_tok_integer_positive()) tests_passed++;
  total_tests++; if (test_tok_integer_invalid_start()) tests_passed++;

  // tok_double tests
  total_tests++; if (test_tok_double_no_data()) tests_passed++;
  total_tests++; if (test_tok_double_simple()) tests_passed++;
  total_tests++; if (test_tok_double_invalid_start()) tests_passed++;

  // tok_clean_whitespace tests
  total_tests++; if (test_tok_clean_whitespace_no_data()) tests_passed++;
  total_tests++; if (test_tok_clean_whitespace_spaces()) tests_passed++;

  // Empty closed channel tests
  total_tests++; if (test_tok_syntax_empty_closed()) tests_passed++;
  total_tests++; if (test_tok_symbol_empty_closed()) tests_passed++;
  total_tests++; if (test_tok_string_empty_closed()) tests_passed++;
  total_tests++; if (test_tok_char_empty_closed()) tests_passed++;
  total_tests++; if (test_tok_integer_empty_closed()) tests_passed++;
  total_tests++; if (test_tok_double_empty_closed()) tests_passed++;
  total_tests++; if (test_tok_clean_whitespace_empty_closed()) tests_passed++;

  // Integration tests
  total_tests++; if (test_tokenize_simple_expression()) tests_passed++;

  // Clean up LispBM after all tests
  kill_eval_after_tests();

  if (tests_passed == total_tests) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILED: %d/%d tests passed\n", tests_passed, total_tests);
    return 1;
  }
}
