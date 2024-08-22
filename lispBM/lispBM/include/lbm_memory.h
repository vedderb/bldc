/** \file lbm_memory.h */
/*
    Copyright 2020, 2022 Joel Svensson        svenssonjoel@yahoo.se

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

/*
   Motivation: Memory manager for allocation of strings and arrays
   that will not be be located on the lisp-heap. These kinds of values
   have thus far been allocated using the "malloc" function provided
   on the platform. Using malloc is something I want to move away from
   doing within the guts of lispBM as I want it to be possible on
   running on the bare metal.

   ** This is already done!
   Later perhaps things such as the symbol table with symbol mappings
   should also be located on this managed memory area.  Symbols,
   however, are never freed after being created in lispBM. Currently I
   dont know if that is a good idea? or if it is possible to free
   unused symbols at all. To become entirely free from malloc, the entire
   symbol table management must be rewritten.
*/

/*
  Notes:
  64Bytes = 16 * 4Byte words
  4Bytes = 32Bits = 16 * 2Bit of status

  Status Bitpatterns:
  00 - FREE or USED 4Byte word
  01 - END of a sequence of allocated words
  10 - START of a sequence of allocated words
  11 - START and END of a sequence of allocated words (a 1 word allocation)

    0  1  2  3  4  5  6  7  8  9
  [11 00 00 00 00 10 01 11 00 00]

  Favours allocation of small amounts of data.

  Requirements:
   - Memory space is a multiple of 64Bytes.
   - Memory status bitmap is the same multiple of 4Bytes.

  Number of bits in an offset from the base_address
  MEMORY_SIZE_512  => 9
  MEMORY_SIZE_1K   => 10
  MEMORY_SIZE_2K   => 11
  MEMORY_SIZE_1M   => 20
  MEMORY_SIZE_16M  => 24
  MEMORY_SIZE_32M  => 25
  MEMORY_SIZE_64M  => 26
  MEMORY_SIZE_128M => 27
  MEMORY_SIZE_256M => 28

  However, due to alignment on a address multiple of 4, the 2 least
  significant bits are zeroes. So an offset into memory of size up to
  1GB should be possible to represent within a lispBM VALUE. This that
  using the offset into memory could be used as the identity of a
  symbol when it comes to replacing the symbol table.

*/
#ifndef _LISPBM_MEMORY_H_
#define _LISPBM_MEMORY_H_

#include "lbm_types.h"
#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define LBM_MEMORY_SIZE_64BYTES_TIMES_X(X) (16*(X))
#ifndef LBM64
#define LBM_MEMORY_BITMAP_SIZE(X) (X)
#else
#define LBM_MEMORY_BITMAP_SIZE(X) ((X)/2)
#endif

#define LBM_MEMORY_SIZE_512 LBM_MEMORY_SIZE_64BYTES_TIMES_X(8)
#define LBM_MEMORY_SIZE_1K LBM_MEMORY_SIZE_64BYTES_TIMES_X(16)
#define LBM_MEMORY_SIZE_2K LBM_MEMORY_SIZE_64BYTES_TIMES_X(32)
#define LBM_MEMORY_SIZE_4K LBM_MEMORY_SIZE_64BYTES_TIMES_X(64)
#define LBM_MEMORY_SIZE_8K LBM_MEMORY_SIZE_64BYTES_TIMES_X(128)
#define LBM_MEMORY_SIZE_10K LBM_MEMORY_SIZE_64BYTES_TIMES_X(160)
#define LBM_MEMORY_SIZE_12K LBM_MEMORY_SIZE_64BYTES_TIMES_X(192)
#define LBM_MEMORY_SIZE_14K LBM_MEMORY_SIZE_64BYTES_TIMES_X(224)
#define LBM_MEMORY_SIZE_16K LBM_MEMORY_SIZE_64BYTES_TIMES_X(256)
#define LBM_MEMORY_SIZE_32K LBM_MEMORY_SIZE_64BYTES_TIMES_X(512)
#define LBM_MEMORY_SIZE_1M LBM_MEMORY_SIZE_64BYTES_TIMES_X(16384)

#define LBM_MEMORY_BITMAP_SIZE_512 LBM_MEMORY_BITMAP_SIZE(8)
#define LBM_MEMORY_BITMAP_SIZE_1K  LBM_MEMORY_BITMAP_SIZE(16)
#define LBM_MEMORY_BITMAP_SIZE_2K  LBM_MEMORY_BITMAP_SIZE(32)
#define LBM_MEMORY_BITMAP_SIZE_4K  LBM_MEMORY_BITMAP_SIZE(64)
#define LBM_MEMORY_BITMAP_SIZE_8K  LBM_MEMORY_BITMAP_SIZE(128)
#define LBM_MEMORY_BITMAP_SIZE_10K LBM_MEMORY_BITMAP_SIZE(160)
#define LBM_MEMORY_BITMAP_SIZE_12K LBM_MEMORY_BITMAP_SIZE(192)
#define LBM_MEMORY_BITMAP_SIZE_14K LBM_MEMORY_BITMAP_SIZE(224)
#define LBM_MEMORY_BITMAP_SIZE_16K LBM_MEMORY_BITMAP_SIZE(256)
#define LBM_MEMORY_BITMAP_SIZE_32K LBM_MEMORY_BITMAP_SIZE(512)
#define LBM_MEMORY_BITMAP_SIZE_1M  LBM_MEMORY_BITMAP_SIZE(16384)

/** Initialize the symbols and arrays memory
 *
 * \param data Pointer to an array of uint32_t for data storage.
 * \param data_size The size of the data storage array in number of uint32_t elements.
 * \param bitmap Pointer to an array of uint32_t for memory allocator meta-data.
 * \param bitmap_size The size of the meta-data in number of uint32_t elements.
 * \return
 */
int lbm_memory_init(lbm_uint *data, lbm_uint data_size,
                           lbm_uint *bitmap, lbm_uint bitmap_size);

/** Set the size of the memory reserve in words.
 * \param num_words Number of words to treat as reserve.
 */
void lbm_memory_set_reserve(lbm_uint num_words);
  /** Get the number of words of memory that is treated as reserve.
   *\return Number of words that are reserved
   */
lbm_uint lbm_memory_get_reserve(void);
/** Size of of the symbols and arrays memory in uint32_t chunks.
 *
 * \return Number of uint32_t words.
 */
lbm_uint lbm_memory_num_words(void);
/**
 *
 * \return The number of free words in the symbols and arrays memory.
 */
lbm_uint lbm_memory_num_free(void);
/** Find the length of the longest run of consecutire free indices
 *  in the LBM memory.
 */
lbm_uint lbm_memory_longest_free(void);
/** Allocate a number of words from the symbols and arrays memory.
 *
 * \param num_words Number of words to allocate.
 * \return pointer to allocated array or NULL.
 */
lbm_uint *lbm_memory_allocate(lbm_uint num_words);
/** Free an allocated array int the symbols and arrays memory.
 *
 * \param ptr Pointer to array to free.
 * \return 1 on success and 0 on failure.
 */
int lbm_memory_free(lbm_uint *ptr);
/** Malloc like interface to lbm_memory
 * \param size Size in bytes of memory to allocate.
 * \return Pointer to array or NULL.
 */
void* lbm_malloc(size_t size);
/** Allocate memory potentially from the reserved memory.
 * \param size Size in bytes of memory to allocate.
 * \return Pointer to array or NULL.
 */
void* lbm_malloc_reserve(size_t size);
/** Free memory allocated with lbm_malloc
 * \param Pointer to array to free
 */
void lbm_free(void *ptr);
/** Shrink an allocated array.
 * \param ptr Pointer to array to shrink
 * \param n New smaller size of array
 * \return 1 on success and 0 on failure.
 */
int lbm_memory_shrink(lbm_uint *ptr, lbm_uint n);

/** Check if a pointer points into the lbm_memory
 *
 * \param ptr
 * \return 1 for yes and 0 for no.
 */
int lbm_memory_ptr_inside(lbm_uint *ptr);


lbm_int lbm_memory_address_to_ix(lbm_uint *ptr);

#ifdef __cplusplus
}
#endif
#endif
