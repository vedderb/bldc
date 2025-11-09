/* This file is part of libschrift.
 *
 * © 2019-2022 Thomas Oltmann and contributors
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. */

/* Adapted to LBM by Joel Svensson in 2025 */

#ifndef SCHRIFT_H
#define SCHRIFT_H 1

#include <stddef.h> /* size_t */
#include <stdint.h> /* uint_fast32_t, uint_least32_t */
#include <extensions/display_extensions.h>
#include "ttf_backend.h"

#ifdef __cplusplus
extern "C" {
#endif

struct SFT_Font
{
	const uint8_t *memory;
	uint_fast32_t  size;
	uint_least16_t unitsPerEm;
	int_least16_t  locaFormat;
	uint_least16_t numLongHmtx;
        int            pairAdjustCoverageOffset;
        int            pairAdjustOffset;
};

#ifdef __cplusplus
}
#endif

#endif

