/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2017 UAVCAN Team
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Contributors: https://github.com/UAVCAN/libcanard/contributors
 */

/*
 * This file holds function declarations that expose the library's internal definitions for unit testing.
 * It is NOT part of the library's API and should not even be looked at by the user.
 */

#ifndef CANARD_INTERNALS_H
#define CANARD_INTERNALS_H

#include "canard.h"

#ifdef __cplusplus
extern "C" {
#endif

/// This macro is needed only for testing and development. Do not redefine this in production.
#ifndef CANARD_INTERNAL
# define CANARD_INTERNAL static
#endif


CANARD_INTERNAL CanardRxState* traverseRxStates(CanardInstance* ins,
                                                uint32_t transfer_descriptor);

CANARD_INTERNAL CanardRxState* createRxState(CanardPoolAllocator* allocator,
                                             uint32_t transfer_descriptor);

CANARD_INTERNAL CanardRxState* prependRxState(CanardInstance* ins,
                                              uint32_t transfer_descriptor);

CANARD_INTERNAL CanardRxState* findRxState(CanardRxState* state,
                                           uint32_t transfer_descriptor);

CANARD_INTERNAL int16_t bufferBlockPushBytes(CanardPoolAllocator* allocator,
                                             CanardRxState* state,
                                             const uint8_t* data,
                                             uint8_t data_len);

CANARD_INTERNAL CanardBufferBlock* createBufferBlock(CanardPoolAllocator* allocator);

CANARD_INTERNAL CanardTransferType extractTransferType(uint32_t id);

CANARD_INTERNAL uint16_t extractDataType(uint32_t id);

CANARD_INTERNAL void pushTxQueue(CanardInstance* ins,
                                 CanardTxQueueItem* item);

CANARD_INTERNAL bool isPriorityHigher(uint32_t id,
                                      uint32_t rhs);

CANARD_INTERNAL CanardTxQueueItem* createTxItem(CanardPoolAllocator* allocator);

CANARD_INTERNAL void prepareForNextTransfer(CanardRxState* state);

CANARD_INTERNAL int16_t computeTransferIDForwardDistance(uint8_t a,
                                                         uint8_t b);

CANARD_INTERNAL void incrementTransferID(uint8_t* transfer_id);

CANARD_INTERNAL uint64_t releaseStatePayload(CanardInstance* ins,
                                             CanardRxState* rxstate);

/// Returns the number of frames enqueued
CANARD_INTERNAL int16_t enqueueTxFrames(CanardInstance* ins,
                                        uint32_t can_id,
                                        uint8_t* transfer_id,
                                        uint16_t crc,
                                        const uint8_t* payload,
                                        uint16_t payload_len);

CANARD_INTERNAL void copyBitArray(const uint8_t* src,
                                  uint32_t src_offset,
                                  uint32_t src_len,
                                  uint8_t* dst,
                                  uint32_t dst_offset);

/**
 * Moves specified bits from the scattered transfer storage to a specified contiguous buffer.
 * Returns the number of bits copied, or negated error code.
 */
CANARD_INTERNAL int16_t descatterTransferPayload(const CanardRxTransfer* transfer,
                                                 uint32_t bit_offset,
                                                 uint8_t bit_length,
                                                 void* output);

CANARD_INTERNAL bool isBigEndian(void);

CANARD_INTERNAL void swapByteOrder(void* data, unsigned size);

/*
 * Transfer CRC
 */
CANARD_INTERNAL uint16_t crcAddByte(uint16_t crc_val,
                                    uint8_t byte);

CANARD_INTERNAL uint16_t crcAddSignature(uint16_t crc_val,
                                         uint64_t data_type_signature);

CANARD_INTERNAL uint16_t crcAdd(uint16_t crc_val,
                                const uint8_t* bytes,
                                size_t len);

/**
 * Inits a memory allocator.
 *
 * @param [in] allocator The memory allocator to initialize.
 * @param [in] buf The buffer used by the memory allocator.
 * @param [in] buf_len The number of blocks in buf.
 */
CANARD_INTERNAL void initPoolAllocator(CanardPoolAllocator* allocator,
                                       CanardPoolAllocatorBlock* buf,
                                       uint16_t buf_len);

/**
 * Allocates a block from the given pool allocator.
 */
CANARD_INTERNAL void* allocateBlock(CanardPoolAllocator* allocator);

/**
 * Frees a memory block previously returned by canardAllocateBlock.
 */
CANARD_INTERNAL void freeBlock(CanardPoolAllocator* allocator,
                               void* p);


#ifdef __cplusplus
}
#endif
#endif
