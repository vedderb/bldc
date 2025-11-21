/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

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

#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include "extensions/ttf_backend.h"


// Test 1: ASCII characters (1-byte UTF-8)
int test_ascii_characters(void) {
  uint8_t test_str[] = "Hello";
  uint32_t utf32;
  uint32_t next_ix;

  // Test 'H' = 0x48
  if (!get_utf32(test_str, &utf32, 0, &next_ix)) return 0;
  if (utf32 != 0x48 || next_ix != 1) {
    printf("ASCII 'H' failed: utf32=%u, next_ix=%u\n", utf32, next_ix);
    return 0;
  }

  // Test 'e' = 0x65
  if (!get_utf32(test_str, &utf32, 1, &next_ix)) return 0;
  if (utf32 != 0x65 || next_ix != 2) {
    printf("ASCII 'e' failed: utf32=%u, next_ix=%u\n", utf32, next_ix);
    return 0;
  }

  return 1;
}

// Test 2: 2-byte UTF-8 sequences
int test_2byte_utf8(void) {
  uint32_t utf32;
  uint32_t next_ix;

  // ñ (U+00F1) = 0xC3 0xB1
  uint8_t test_n[] = {0xC3, 0xB1, 0x00};
  if (!get_utf32(test_n, &utf32, 0, &next_ix)) {
    printf("2-byte UTF-8 'ñ' failed to decode\n");
    return 0;
  }
  if (utf32 != 0x00F1 || next_ix != 2) {
    printf("2-byte UTF-8 'ñ' failed: utf32=0x%X, next_ix=%u\n", utf32, next_ix);
    return 0;
  }

  // ö (U+00F6) = 0xC3 0xB6
  uint8_t test_o[] = {0xC3, 0xB6, 0x00};
  if (!get_utf32(test_o, &utf32, 0, &next_ix)) {
    printf("2-byte UTF-8 'ö' failed to decode\n");
    return 0;
  }
  if (utf32 != 0x00F6 || next_ix != 2) {
    printf("2-byte UTF-8 'ö' failed: utf32=0x%X, next_ix=%u\n", utf32, next_ix);
    return 0;
  }

  return 1;
}

// Test 3: 3-byte UTF-8 sequences
int test_3byte_utf8(void) {
  uint32_t utf32;
  uint32_t next_ix;

  // 中 (U+4E2D) = 0xE4 0xB8 0xAD
  uint8_t test_chinese[] = {0xE4, 0xB8, 0xAD, 0x00};
  if (!get_utf32(test_chinese, &utf32, 0, &next_ix)) {
    printf("3-byte UTF-8 '中' failed to decode\n");
    return 0;
  }
  if (utf32 != 0x4E2D || next_ix != 3) {
    printf("3-byte UTF-8 '中' failed: utf32=0x%X, next_ix=%u\n", utf32, next_ix);
    return 0;
  }

  // € (U+20AC) = 0xE2 0x82 0xAC
  uint8_t test_euro[] = {0xE2, 0x82, 0xAC, 0x00};
  if (!get_utf32(test_euro, &utf32, 0, &next_ix)) {
    printf("3-byte UTF-8 '€' failed to decode\n");
    return 0;
  }
  if (utf32 != 0x20AC || next_ix != 3) {
    printf("3-byte UTF-8 '€' failed: utf32=0x%X, next_ix=%u\n", utf32, next_ix);
    return 0;
  }

  return 1;
}

// Test 4: 4-byte UTF-8 sequences
int test_4byte_utf8(void) {
  uint32_t utf32;
  uint32_t next_ix;

  // 😀 (U+1F600) = 0xF0 0x9F 0x98 0x80
  uint8_t test_emoji[] = {0xF0, 0x9F, 0x98, 0x80, 0x00};
  if (!get_utf32(test_emoji, &utf32, 0, &next_ix)) {
    printf("4-byte UTF-8 '😀' failed to decode\n");
    return 0;
  }
  if (utf32 != 0x1F600 || next_ix != 4) {
    printf("4-byte UTF-8 '😀' failed: utf32=0x%X, next_ix=%u\n", utf32, next_ix);
    return 0;
  }

  // 🚀 (U+1F680) = 0xF0 0x9F 0x9A 0x80
  uint8_t test_rocket[] = {0xF0, 0x9F, 0x9A, 0x80, 0x00};
  if (!get_utf32(test_rocket, &utf32, 0, &next_ix)) {
    printf("4-byte UTF-8 '🚀' failed to decode\n");
    return 0;
  }
  if (utf32 != 0x1F680 || next_ix != 4) {
    printf("4-byte UTF-8 '🚀' failed: utf32=0x%X, next_ix=%u\n", utf32, next_ix);
    return 0;
  }

  return 1;
}

// Test 5: Null terminator
int test_null_terminator(void) {
  uint8_t test_str[] = {0x00};
  uint32_t utf32;
  uint32_t next_ix;

  if (get_utf32(test_str, &utf32, 0, &next_ix)) {
    printf("Null terminator should return false\n");
    return 0;
  }

  return 1;
}

// Test 6: Invalid 2-byte sequences
int test_invalid_2byte(void) {
  uint32_t utf32;
  uint32_t next_ix;

  // Invalid continuation byte (should be 0x80-0xBF)
  uint8_t invalid_continuation[] = {0xC3, 0x00, 0x00};
  if (get_utf32(invalid_continuation, &utf32, 0, &next_ix)) {
    printf("Invalid 2-byte continuation should return false\n");
    return 0;
  }

  // Invalid continuation byte (0xFF)
  uint8_t invalid_continuation2[] = {0xC3, 0xFF, 0x00};
  if (get_utf32(invalid_continuation2, &utf32, 0, &next_ix)) {
    printf("Invalid 2-byte continuation (0xFF) should return false\n");
    return 0;
  }

  return 1;
}

// Test 7: Invalid 3-byte sequences
int test_invalid_3byte(void) {
  uint32_t utf32;
  uint32_t next_ix;

  // Invalid 2nd byte
  uint8_t invalid_2nd[] = {0xE4, 0x00, 0xAD, 0x00};
  if (get_utf32(invalid_2nd, &utf32, 0, &next_ix)) {
    printf("Invalid 3-byte 2nd byte should return false\n");
    return 0;
  }

  // Invalid 3rd byte
  uint8_t invalid_3rd[] = {0xE4, 0xB8, 0x00, 0x00};
  if (get_utf32(invalid_3rd, &utf32, 0, &next_ix)) {
    printf("Invalid 3-byte 3rd byte should return false\n");
    return 0;
  }

  return 1;
}

// Test 8: Invalid 4-byte sequences
int test_invalid_4byte(void) {
  uint32_t utf32;
  uint32_t next_ix;

  // Invalid 2nd byte
  uint8_t invalid_2nd[] = {0xF0, 0x00, 0x98, 0x80, 0x00};
  if (get_utf32(invalid_2nd, &utf32, 0, &next_ix)) {
    printf("Invalid 4-byte 2nd byte should return false\n");
    return 0;
  }

  // Invalid 3rd byte
  uint8_t invalid_3rd[] = {0xF0, 0x9F, 0x00, 0x80, 0x00};
  if (get_utf32(invalid_3rd, &utf32, 0, &next_ix)) {
    printf("Invalid 4-byte 3rd byte should return false\n");
    return 0;
  }

  // Invalid 4th byte
  uint8_t invalid_4th[] = {0xF0, 0x9F, 0x98, 0x00, 0x00};
  if (get_utf32(invalid_4th, &utf32, 0, &next_ix)) {
    printf("Invalid 4-byte 4th byte should return false\n");
    return 0;
  }

  return 1;
}

// Test 9: Surrogate pair range in 4-byte sequences
int test_surrogate_pairs(void) {
  uint32_t utf32;
  uint32_t next_ix;

  // The surrogate check only applies to 4-byte sequences
  // Create a 4-byte sequence that decodes to the surrogate range
  // 0xF0 0x8D 0xA0 0x80 would decode to 0xD800 in the 4-byte handler
  uint8_t surrogate[] = {0xF0, 0x8D, 0xA0, 0x80, 0x00};
  if (get_utf32(surrogate, &utf32, 0, &next_ix)) {
    printf("4-byte sequence in surrogate range should return false\n");
    return 0;
  }

  return 1;
}

// Test 10: Invalid first byte
int test_invalid_first_byte(void) {
  uint32_t utf32;
  uint32_t next_ix;

  // 0xF8 is not a valid UTF-8 start byte (would be 5-byte sequence)
  uint8_t invalid_start[] = {0xF8, 0x80, 0x80, 0x80, 0x00};
  if (get_utf32(invalid_start, &utf32, 0, &next_ix)) {
    printf("Invalid first byte 0xF8 should return false\n");
    return 0;
  }

  // 0xFF is not a valid UTF-8 start byte
  uint8_t invalid_start2[] = {0xFF, 0x80, 0x80, 0x80, 0x00};
  if (get_utf32(invalid_start2, &utf32, 0, &next_ix)) {
    printf("Invalid first byte 0xFF should return false\n");
    return 0;
  }

  // Continuation byte as first byte
  uint8_t continuation_as_start[] = {0x80, 0x00};
  if (get_utf32(continuation_as_start, &utf32, 0, &next_ix)) {
    printf("Continuation byte as start should return false\n");
    return 0;
  }

  return 1;
}

int main(void) {
  int tests_passed = 0;
  int tests_total = 10;

  printf("Running UTF-8 decoder tests...\n");

  if (test_ascii_characters()) {
    printf("Test 1 passed: ascii_characters\n");
    tests_passed++;
  } else {
    printf("Test 1 FAILED: ascii_characters\n");
  }

  if (test_2byte_utf8()) {
    printf("Test 2 passed: 2byte_utf8\n");
    tests_passed++;
  } else {
    printf("Test 2 FAILED: 2byte_utf8\n");
  }

  if (test_3byte_utf8()) {
    printf("Test 3 passed: 3byte_utf8\n");
    tests_passed++;
  } else {
    printf("Test 3 FAILED: 3byte_utf8\n");
  }

  if (test_4byte_utf8()) {
    printf("Test 4 passed: 4byte_utf8\n");
    tests_passed++;
  } else {
    printf("Test 4 FAILED: 4byte_utf8\n");
  }

  if (test_null_terminator()) {
    printf("Test 5 passed: null_terminator\n");
    tests_passed++;
  } else {
    printf("Test 5 FAILED: null_terminator\n");
  }

  if (test_invalid_2byte()) {
    printf("Test 6 passed: invalid_2byte\n");
    tests_passed++;
  } else {
    printf("Test 6 FAILED: invalid_2byte\n");
  }

  if (test_invalid_3byte()) {
    printf("Test 7 passed: invalid_3byte\n");
    tests_passed++;
  } else {
    printf("Test 7 FAILED: invalid_3byte\n");
  }

  if (test_invalid_4byte()) {
    printf("Test 8 passed: invalid_4byte\n");
    tests_passed++;
  } else {
    printf("Test 8 FAILED: invalid_4byte\n");
  }

  if (test_surrogate_pairs()) {
    printf("Test 9 passed: surrogate_pairs\n");
    tests_passed++;
  } else {
    printf("Test 9 FAILED: surrogate_pairs\n");
  }

  if (test_invalid_first_byte()) {
    printf("Test 10 passed: invalid_first_byte\n");
    tests_passed++;
  } else {
    printf("Test 10 FAILED: invalid_first_byte\n");
  }

  printf("\n%d/%d tests passed\n", tests_passed, tests_total);

  if (tests_passed == tests_total) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILURE\n");
    return 1;
  }
}
