/*
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
 *
 * Documentation: http://uavcan.org/Implementations/Libcanard
 */

#include "canard_internals.h"
#include <string.h>


#undef MIN
#undef MAX
#define MIN(a, b)   (((a) < (b)) ? (a) : (b))
#define MAX(a, b)   (((a) > (b)) ? (a) : (b))


#define TRANSFER_TIMEOUT_USEC                       2000000

#define TRANSFER_ID_BIT_LEN                         5U
#define ANON_MSG_DATA_TYPE_ID_BIT_LEN               2U

#define SOURCE_ID_FROM_ID(x)                        ((uint8_t) (((x) >> 0U)  & 0x7FU))
#define SERVICE_NOT_MSG_FROM_ID(x)                  ((bool)    (((x) >> 7U)  & 0x1U))
#define REQUEST_NOT_RESPONSE_FROM_ID(x)             ((bool)    (((x) >> 15U) & 0x1U))
#define DEST_ID_FROM_ID(x)                          ((uint8_t) (((x) >> 8U)  & 0x7FU))
#define PRIORITY_FROM_ID(x)                         ((uint8_t) (((x) >> 24U) & 0x1FU))
#define MSG_TYPE_FROM_ID(x)                         ((uint16_t)(((x) >> 8U)  & 0xFFFFU))
#define SRV_TYPE_FROM_ID(x)                         ((uint8_t) (((x) >> 16U) & 0xFFU))

#define MAKE_TRANSFER_DESCRIPTOR(data_type_id, transfer_type, src_node_id, dst_node_id)             \
    (((uint32_t)(data_type_id)) | (((uint32_t)(transfer_type)) << 16U) |                            \
    (((uint32_t)(src_node_id)) << 18U) | (((uint32_t)(dst_node_id)) << 25U))

#define TRANSFER_ID_FROM_TAIL_BYTE(x)               ((uint8_t)((x) & 0x1FU))

// The extra cast to unsigned is needed to squelch warnings from clang-tidy
#define IS_START_OF_TRANSFER(x)                     ((bool)(((uint32_t)(x) >> 7U) & 0x1U))
#define IS_END_OF_TRANSFER(x)                       ((bool)(((uint32_t)(x) >> 6U) & 0x1U))
#define TOGGLE_BIT(x)                               ((bool)(((uint32_t)(x) >> 5U) & 0x1U))


struct CanardTxQueueItem
{
    CanardTxQueueItem* next;
    CanardCANFrame frame;
};


/*
 * API functions
 */
void canardInit(CanardInstance* out_ins,
                void* mem_arena,
                size_t mem_arena_size,
                CanardOnTransferReception on_reception,
                CanardShouldAcceptTransfer should_accept,
                void* user_reference)
{
    CANARD_ASSERT(out_ins != NULL);

    /*
     * Checking memory layout.
     * This condition is supposed to be true for all 32-bit and smaller platforms.
     * If your application fails here, make sure it's not built in 64-bit mode.
     * Refer to the design documentation for more info.
     */
    CANARD_ASSERT(CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE >= 6);

    memset(out_ins, 0, sizeof(*out_ins));

    out_ins->node_id = CANARD_BROADCAST_NODE_ID;
    out_ins->on_reception = on_reception;
    out_ins->should_accept = should_accept;
    out_ins->rx_states = NULL;
    out_ins->tx_queue = NULL;
    out_ins->user_reference = user_reference;

    size_t pool_capacity = mem_arena_size / CANARD_MEM_BLOCK_SIZE;
    if (pool_capacity > 0xFFFFU)
    {
        pool_capacity = 0xFFFFU;
    }

    initPoolAllocator(&out_ins->allocator, mem_arena, (uint16_t)pool_capacity);
}

void* canardGetUserReference(CanardInstance* ins)
{
    CANARD_ASSERT(ins != NULL);
    return ins->user_reference;
}

void canardSetLocalNodeID(CanardInstance* ins, uint8_t self_node_id)
{
    CANARD_ASSERT(ins != NULL);

    if ((ins->node_id == CANARD_BROADCAST_NODE_ID) &&
        (self_node_id >= CANARD_MIN_NODE_ID) &&
        (self_node_id <= CANARD_MAX_NODE_ID))
    {
        ins->node_id = self_node_id;
    }
    else
    {
        CANARD_ASSERT(false);
    }
}

uint8_t canardGetLocalNodeID(const CanardInstance* ins)
{
    return ins->node_id;
}

int16_t canardBroadcast(CanardInstance* ins,
                        uint64_t data_type_signature,
                        uint16_t data_type_id,
                        uint8_t* inout_transfer_id,
                        uint8_t priority,
                        const void* payload,
                        uint16_t payload_len)
{
    if (payload == NULL && payload_len > 0)
    {
        return -CANARD_ERROR_INVALID_ARGUMENT;
    }
    if (priority > CANARD_TRANSFER_PRIORITY_LOWEST)
    {
        return -CANARD_ERROR_INVALID_ARGUMENT;
    }

    uint32_t can_id = 0;
    uint16_t crc = 0xFFFFU;

    if (canardGetLocalNodeID(ins) == 0)
    {
        if (payload_len > 7)
        {
            return -CANARD_ERROR_NODE_ID_NOT_SET;
        }

        static const uint16_t DTIDMask = (1U << ANON_MSG_DATA_TYPE_ID_BIT_LEN) - 1U;

        if ((data_type_id & DTIDMask) != data_type_id)
        {
            return -CANARD_ERROR_INVALID_ARGUMENT;
        }

        // anonymous transfer, random discriminator
        const uint16_t discriminator = (uint16_t)((crcAdd(0xFFFFU, payload, payload_len)) & 0x7FFEU);
        can_id = ((uint32_t) priority << 24U) | ((uint32_t) discriminator << 9U) |
                 ((uint32_t) (data_type_id & DTIDMask) << 8U) | (uint32_t) canardGetLocalNodeID(ins);
    }
    else
    {
        can_id = ((uint32_t) priority << 24U) | ((uint32_t) data_type_id << 8U) | (uint32_t) canardGetLocalNodeID(ins);

        if (payload_len > 7)
        {
            crc = crcAddSignature(crc, data_type_signature);
            crc = crcAdd(crc, payload, payload_len);
        }
    }

    const int16_t result = enqueueTxFrames(ins, can_id, inout_transfer_id, crc, payload, payload_len);

    incrementTransferID(inout_transfer_id);

    return result;
}

int16_t canardRequestOrRespond(CanardInstance* ins,
                               uint8_t destination_node_id,
                               uint64_t data_type_signature,
                               uint8_t data_type_id,
                               uint8_t* inout_transfer_id,
                               uint8_t priority,
                               CanardRequestResponse kind,
                               const void* payload,
                               uint16_t payload_len)
{
    if (payload == NULL && payload_len > 0)
    {
        return -CANARD_ERROR_INVALID_ARGUMENT;
    }
    if (priority > CANARD_TRANSFER_PRIORITY_LOWEST)
    {
        return -CANARD_ERROR_INVALID_ARGUMENT;
    }
    if (canardGetLocalNodeID(ins) == 0)
    {
        return -CANARD_ERROR_NODE_ID_NOT_SET;
    }

    const uint32_t can_id = ((uint32_t) priority << 24U) | ((uint32_t) data_type_id << 16U) |
                            ((uint32_t) kind << 15U) | ((uint32_t) destination_node_id << 8U) |
                            (1U << 7U) | (uint32_t) canardGetLocalNodeID(ins);
    uint16_t crc = 0xFFFFU;

    if (payload_len > 7)
    {
        crc = crcAddSignature(crc, data_type_signature);
        crc = crcAdd(crc, payload, payload_len);
    }

    const int16_t result = enqueueTxFrames(ins, can_id, inout_transfer_id, crc, payload, payload_len);

    if (kind == CanardRequest)                      // Response Transfer ID must not be altered
    {
        incrementTransferID(inout_transfer_id);
    }

    return result;
}

const CanardCANFrame* canardPeekTxQueue(const CanardInstance* ins)
{
    if (ins->tx_queue == NULL)
    {
        return NULL;
    }
    return &ins->tx_queue->frame;
}

void canardPopTxQueue(CanardInstance* ins)
{
    CanardTxQueueItem* item = ins->tx_queue;
    ins->tx_queue = item->next;
    freeBlock(&ins->allocator, item);
}

void canardHandleRxFrame(CanardInstance* ins, const CanardCANFrame* frame, uint64_t timestamp_usec)
{
    const CanardTransferType transfer_type = extractTransferType(frame->id);
    const uint8_t destination_node_id = (transfer_type == CanardTransferTypeBroadcast) ?
                                        (uint8_t)CANARD_BROADCAST_NODE_ID :
                                        DEST_ID_FROM_ID(frame->id);

    // TODO: This function should maintain statistics of transfer errors and such.

    if ((frame->id & CANARD_CAN_FRAME_EFF) == 0 ||
        (frame->id & CANARD_CAN_FRAME_RTR) != 0 ||
        (frame->id & CANARD_CAN_FRAME_ERR) != 0 ||
        (frame->data_len < 1))
    {
        return;     // Unsupported frame, not UAVCAN - ignore
    }

    if (transfer_type != CanardTransferTypeBroadcast &&
        destination_node_id != canardGetLocalNodeID(ins))
    {
        return;     // Address mismatch
    }

    const uint8_t priority = PRIORITY_FROM_ID(frame->id);
    const uint8_t source_node_id = SOURCE_ID_FROM_ID(frame->id);
    const uint16_t data_type_id = extractDataType(frame->id);
    const uint32_t transfer_descriptor =
            MAKE_TRANSFER_DESCRIPTOR(data_type_id, transfer_type, source_node_id, destination_node_id);

    const uint8_t tail_byte = frame->data[frame->data_len - 1];

    CanardRxState* rx_state = NULL;

    if (IS_START_OF_TRANSFER(tail_byte))
    {
        uint64_t data_type_signature = 0;

        if (ins->should_accept(ins, &data_type_signature, data_type_id, transfer_type, source_node_id))
        {
            rx_state = traverseRxStates(ins, transfer_descriptor);

            if(rx_state == NULL)
            {
                return; // No allocator room for this frame
            }

            rx_state->calculated_crc = crcAddSignature(0xFFFFU, data_type_signature);
        }
        else
        {
            return;     // The application doesn't want this transfer
        }
    }
    else
    {
        rx_state = findRxState(ins->rx_states, transfer_descriptor);

        if (rx_state == NULL)
        {
            return;
        }
    }

    CANARD_ASSERT(rx_state != NULL);    // All paths that lead to NULL should be terminated with return above

    // Resolving the state flags:
    const bool not_initialized = rx_state->timestamp_usec == 0;
    const bool tid_timed_out = (timestamp_usec - rx_state->timestamp_usec) > TRANSFER_TIMEOUT_USEC;
    const bool first_frame = IS_START_OF_TRANSFER(tail_byte);
    const bool not_previous_tid =
        computeTransferIDForwardDistance((uint8_t) rx_state->transfer_id, TRANSFER_ID_FROM_TAIL_BYTE(tail_byte)) > 1;

    const bool need_restart =
            (not_initialized) ||
            (tid_timed_out) ||
            (first_frame && not_previous_tid);

    if (need_restart)
    {
        rx_state->transfer_id = TRANSFER_ID_FROM_TAIL_BYTE(tail_byte);
        rx_state->next_toggle = 0;
        releaseStatePayload(ins, rx_state);
        if (!IS_START_OF_TRANSFER(tail_byte)) // missed the first frame
        {
            rx_state->transfer_id++;
            return;
        }
    }

    if (IS_START_OF_TRANSFER(tail_byte) && IS_END_OF_TRANSFER(tail_byte)) // single frame transfer
    {
        rx_state->timestamp_usec = timestamp_usec;
        CanardRxTransfer rx_transfer = {
            .timestamp_usec = timestamp_usec,
            .payload_head = frame->data,
            .payload_len = (uint8_t)(frame->data_len - 1U),
            .data_type_id = data_type_id,
            .transfer_type = transfer_type,
            .transfer_id = TRANSFER_ID_FROM_TAIL_BYTE(tail_byte),
            .priority = priority,
            .source_node_id = source_node_id
        };

        ins->on_reception(ins, &rx_transfer);

        prepareForNextTransfer(rx_state);
        return;
    }

    if (TOGGLE_BIT(tail_byte) != rx_state->next_toggle)
    {
        return; // wrong toggle
    }

    if (TRANSFER_ID_FROM_TAIL_BYTE(tail_byte) != rx_state->transfer_id)
    {
        return; // unexpected tid
    }

    if (IS_START_OF_TRANSFER(tail_byte) && !IS_END_OF_TRANSFER(tail_byte))      // Beginning of multi frame transfer
    {
        if (frame->data_len <= 3)
        {
            return;     // Not enough data
        }

        // take off the crc and store the payload
        rx_state->timestamp_usec = timestamp_usec;
        const int16_t ret = bufferBlockPushBytes(&ins->allocator, rx_state, frame->data + 2,
                                                 (uint8_t) (frame->data_len - 3));
        if (ret < 0)
        {
            releaseStatePayload(ins, rx_state);
            prepareForNextTransfer(rx_state);
            return;
        }
        rx_state->payload_crc = (uint16_t)(((uint16_t) frame->data[0]) | (uint16_t)((uint16_t) frame->data[1] << 8U));
        rx_state->calculated_crc = crcAdd((uint16_t)rx_state->calculated_crc,
                                          frame->data + 2, (uint8_t)(frame->data_len - 3));
    }
    else if (!IS_START_OF_TRANSFER(tail_byte) && !IS_END_OF_TRANSFER(tail_byte))    // Middle of a multi-frame transfer
    {
        const int16_t ret = bufferBlockPushBytes(&ins->allocator, rx_state, frame->data,
                                                 (uint8_t) (frame->data_len - 1));
        if (ret < 0)
        {
            releaseStatePayload(ins, rx_state);
            prepareForNextTransfer(rx_state);
            return;
        }
        rx_state->calculated_crc = crcAdd((uint16_t)rx_state->calculated_crc,
                                          frame->data, (uint8_t)(frame->data_len - 1));
    }
    else                                                                            // End of a multi-frame transfer
    {
        const uint8_t frame_payload_size = (uint8_t)(frame->data_len - 1);

        uint8_t tail_offset = 0;

        if (rx_state->payload_len < CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE)
        {
            // Copy the beginning of the frame into the head, point the tail pointer to the remainder
            for (size_t i = rx_state->payload_len;
                 (i < CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE) && (tail_offset < frame_payload_size);
                 i++, tail_offset++)
            {
                rx_state->buffer_head[i] = frame->data[tail_offset];
            }
        }
        else
        {
            // Like above, except that the beginning goes into the last block of the storage
            CanardBufferBlock* block = rx_state->buffer_blocks;
            if (block != NULL)          // If there's no middle, that's fine, we'll use only head and tail
            {
                size_t offset = CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE;    // Payload offset of the first block
                while (block->next != NULL)
                {
                    block = block->next;
                    offset += CANARD_BUFFER_BLOCK_DATA_SIZE;
                }
                CANARD_ASSERT(block != NULL);

                const size_t offset_within_block = rx_state->payload_len - offset;
                CANARD_ASSERT(offset_within_block < CANARD_BUFFER_BLOCK_DATA_SIZE);

                for (size_t i = offset_within_block;
                     (i < CANARD_BUFFER_BLOCK_DATA_SIZE) && (tail_offset < frame_payload_size);
                     i++, tail_offset++)
                {
                    block->data[i] = frame->data[tail_offset];
                }
            }
        }

        CanardRxTransfer rx_transfer = {
            .timestamp_usec = timestamp_usec,
            .payload_head = rx_state->buffer_head,
            .payload_middle = rx_state->buffer_blocks,
            .payload_tail = (tail_offset >= frame_payload_size) ? NULL : (&frame->data[tail_offset]),
            .payload_len = (uint16_t)(rx_state->payload_len + frame_payload_size),
            .data_type_id = data_type_id,
            .transfer_type = transfer_type,
            .transfer_id = TRANSFER_ID_FROM_TAIL_BYTE(tail_byte),
            .priority = priority,
            .source_node_id = source_node_id
        };

        rx_state->buffer_blocks = NULL;     // Block list ownership has been transferred to rx_transfer!

        // CRC validation
        rx_state->calculated_crc = crcAdd((uint16_t)rx_state->calculated_crc, frame->data, frame->data_len - 1U);
        if (rx_state->calculated_crc == rx_state->payload_crc)
        {
            ins->on_reception(ins, &rx_transfer);
        }

        // Making sure the payload is released even if the application didn't bother with it
        canardReleaseRxTransferPayload(ins, &rx_transfer);
        prepareForNextTransfer(rx_state);
        return;
    }

    rx_state->next_toggle = rx_state->next_toggle ? 0 : 1;
}

void canardCleanupStaleTransfers(CanardInstance* ins, uint64_t current_time_usec)
{
    CanardRxState* prev = ins->rx_states, * state = ins->rx_states;

    while (state != NULL)
    {
        if ((current_time_usec - state->timestamp_usec) > TRANSFER_TIMEOUT_USEC)
        {
            if (state == ins->rx_states)
            {
                releaseStatePayload(ins, state);
                ins->rx_states = ins->rx_states->next;
                freeBlock(&ins->allocator, state);
                state = ins->rx_states;
                prev = state;
            }
            else
            {
                releaseStatePayload(ins, state);
                prev->next = state->next;
                freeBlock(&ins->allocator, state);
                state = prev->next;
            }
        }
        else
        {
            prev = state;
            state = state->next;
        }
    }
}

int16_t canardDecodeScalar(const CanardRxTransfer* transfer,
                           uint32_t bit_offset,
                           uint8_t bit_length,
                           bool value_is_signed,
                           void* out_value)
{
    if (transfer == NULL || out_value == NULL)
    {
        return -CANARD_ERROR_INVALID_ARGUMENT;
    }

    if (bit_length < 1 || bit_length > 64)
    {
        return -CANARD_ERROR_INVALID_ARGUMENT;
    }

    if (bit_length == 1 && value_is_signed)
    {
        return -CANARD_ERROR_INVALID_ARGUMENT;
    }

    /*
     * Reading raw bytes into the temporary storage.
     * Luckily, C guarantees that every element is aligned at the beginning (lower address) of the union.
     */
    union
    {
        bool     boolean;       ///< sizeof(bool) is implementation-defined, so it has to be handled separately
        uint8_t  u8;            ///< Also char
        int8_t   s8;
        uint16_t u16;
        int16_t  s16;
        uint32_t u32;
        int32_t  s32;           ///< Also float, possibly double, possibly long double (depends on implementation)
        uint64_t u64;
        int64_t  s64;           ///< Also double, possibly float, possibly long double (depends on implementation)
        uint8_t bytes[8];
    } storage;

    memset(&storage, 0, sizeof(storage));   // This is important

    const int16_t result = descatterTransferPayload(transfer, bit_offset, bit_length, &storage.bytes[0]);
    if (result <= 0)
    {
        return result;
    }

    CANARD_ASSERT((result > 0) && (result <= 64) && (result <= bit_length));

    /*
     * The bit copy algorithm assumes that more significant bits have lower index, so we need to shift some.
     * Extra most significant bits will be filled with zeroes, which is fine.
     * Coverity Scan mistakenly believes that the array may be overrun if bit_length == 64; however, this branch will
     * not be taken if bit_length == 64, because 64 % 8 == 0.
     */
    if ((bit_length % 8) != 0)
    {
        // coverity[overrun-local]
        storage.bytes[bit_length / 8U] = (uint8_t)(storage.bytes[bit_length / 8U] >> ((8U - (bit_length % 8U)) & 7U));
    }

    /*
     * Determining the closest standard byte length - this will be needed for byte reordering and sign bit extension.
     */
    uint8_t std_byte_length = 0;
    if      (bit_length == 1)   { std_byte_length = sizeof(bool); }
    else if (bit_length <= 8)   { std_byte_length = 1; }
    else if (bit_length <= 16)  { std_byte_length = 2; }
    else if (bit_length <= 32)  { std_byte_length = 4; }
    else if (bit_length <= 64)  { std_byte_length = 8; }
    else
    {
        CANARD_ASSERT(false);
        return -CANARD_ERROR_INTERNAL;
    }

    CANARD_ASSERT((std_byte_length > 0) && (std_byte_length <= 8));

    /*
     * Flipping the byte order if needed.
     */
    if (isBigEndian())
    {
        swapByteOrder(&storage.bytes[0], std_byte_length);
    }

    /*
     * Extending the sign bit if needed. I miss templates.
     * Note that we operate on unsigned values in order to avoid undefined behaviors.
     */
    if (value_is_signed && (std_byte_length * 8 != bit_length))
    {
        if (bit_length <= 8)
        {
            if ((storage.u8 & (1U << (bit_length - 1U))) != 0)                           // If the sign bit is set...
            {
                storage.u8 |= (uint8_t) 0xFFU & (uint8_t) ~((1U << bit_length) - 1U);   // ...set all bits above it.
            }
        }
        else if (bit_length <= 16)
        {
            if ((storage.u16 & (1U << (bit_length - 1U))) != 0)
            {
                storage.u16 |= (uint16_t) 0xFFFFU & (uint16_t) ~((1U << bit_length) - 1U);
            }
        }
        else if (bit_length <= 32)
        {
            if ((storage.u32 & (((uint32_t) 1) << (bit_length - 1U))) != 0)
            {
                storage.u32 |= (uint32_t) 0xFFFFFFFFUL & (uint32_t) ~((((uint32_t) 1) << bit_length) - 1U);
            }
        }
        else if (bit_length < 64)   // Strictly less, this is not a typo
        {
            if ((storage.u64 & (((uint64_t) 1) << (bit_length - 1U))) != 0)
            {
                storage.u64 |= (uint64_t) 0xFFFFFFFFFFFFFFFFULL & (uint64_t) ~((((uint64_t) 1) << bit_length) - 1U);
            }
        }
        else
        {
            CANARD_ASSERT(false);
            return -CANARD_ERROR_INTERNAL;
        }
    }

    /*
     * Copying the result out.
     */
    if (value_is_signed)
    {
        if      (bit_length <= 8)   { *( (int8_t*) out_value) = storage.s8;  }
        else if (bit_length <= 16)  { *((int16_t*) out_value) = storage.s16; }
        else if (bit_length <= 32)  { *((int32_t*) out_value) = storage.s32; }
        else if (bit_length <= 64)  { *((int64_t*) out_value) = storage.s64; }
        else
        {
            CANARD_ASSERT(false);
            return -CANARD_ERROR_INTERNAL;
        }
    }
    else
    {
        if      (bit_length == 1)   { *(    (bool*) out_value) = storage.boolean; }
        else if (bit_length <= 8)   { *( (uint8_t*) out_value) = storage.u8;  }
        else if (bit_length <= 16)  { *((uint16_t*) out_value) = storage.u16; }
        else if (bit_length <= 32)  { *((uint32_t*) out_value) = storage.u32; }
        else if (bit_length <= 64)  { *((uint64_t*) out_value) = storage.u64; }
        else
        {
            CANARD_ASSERT(false);
            return -CANARD_ERROR_INTERNAL;
        }
    }

    CANARD_ASSERT(result <= bit_length);
    CANARD_ASSERT(result > 0);
    return result;
}

void canardEncodeScalar(void* destination,
                        uint32_t bit_offset,
                        uint8_t bit_length,
                        const void* value)
{
    /*
     * This function can only fail due to invalid arguments, so it was decided to make it return void,
     * and in the case of bad arguments try the best effort or just trigger an CANARD_ASSERTion failure.
     * Maybe not the best solution, but it simplifies the API.
     */
    CANARD_ASSERT(destination != NULL);
    CANARD_ASSERT(value != NULL);

    if (bit_length > 64)
    {
        CANARD_ASSERT(false);
        bit_length = 64;
    }

    if (bit_length < 1)
    {
        CANARD_ASSERT(false);
        bit_length = 1;
    }

    /*
     * Preparing the data in the temporary storage.
     */
    union
    {
        bool     boolean;
        uint8_t  u8;
        uint16_t u16;
        uint32_t u32;
        uint64_t u64;
        uint8_t bytes[8];
    } storage;

    memset(&storage, 0, sizeof(storage));

    uint8_t std_byte_length = 0;

    // Extra most significant bits can be safely ignored here.
    if      (bit_length == 1)   { std_byte_length = sizeof(bool);   storage.boolean = (*((bool*) value) != 0); }
    else if (bit_length <= 8)   { std_byte_length = 1;              storage.u8  = *((uint8_t*) value);  }
    else if (bit_length <= 16)  { std_byte_length = 2;              storage.u16 = *((uint16_t*) value); }
    else if (bit_length <= 32)  { std_byte_length = 4;              storage.u32 = *((uint32_t*) value); }
    else if (bit_length <= 64)  { std_byte_length = 8;              storage.u64 = *((uint64_t*) value); }
    else
    {
        CANARD_ASSERT(false);
    }

    CANARD_ASSERT(std_byte_length > 0);

    if (isBigEndian())
    {
        swapByteOrder(&storage.bytes[0], std_byte_length);
    }

    /*
     * The bit copy algorithm assumes that more significant bits have lower index, so we need to shift some.
     * Extra least significant bits will be filled with zeroes, which is fine.
     * Extra most significant bits will be discarded here.
     * Coverity Scan mistakenly believes that the array may be overrun if bit_length == 64; however, this branch will
     * not be taken if bit_length == 64, because 64 % 8 == 0.
     */
    if ((bit_length % 8) != 0)
    {
        // coverity[overrun-local]
        storage.bytes[bit_length / 8U] = (uint8_t)(storage.bytes[bit_length / 8U] << ((8U - (bit_length % 8U)) & 7U));
    }

    /*
     * Now, the storage contains properly serialized scalar. Copying it out.
     */
    copyBitArray(&storage.bytes[0], 0, bit_length, (uint8_t*) destination, bit_offset);
}

void canardReleaseRxTransferPayload(CanardInstance* ins, CanardRxTransfer* transfer)
{
    while (transfer->payload_middle != NULL)
    {
        CanardBufferBlock* const temp = transfer->payload_middle->next;
        freeBlock(&ins->allocator, transfer->payload_middle);
        transfer->payload_middle = temp;
    }

    transfer->payload_middle = NULL;
    transfer->payload_head = NULL;
    transfer->payload_tail = NULL;
    transfer->payload_len = 0;
}

CanardPoolAllocatorStatistics canardGetPoolAllocatorStatistics(CanardInstance* ins)
{
    return ins->allocator.statistics;
}

uint16_t canardConvertNativeFloatToFloat16(float value)
{
    CANARD_ASSERT(sizeof(float) == 4);

    union FP32
    {
        uint32_t u;
        float f;
    };

    const union FP32 f32inf = { 255UL << 23U };
    const union FP32 f16inf = { 31UL << 23U };
    const union FP32 magic = { 15UL << 23U };
    const uint32_t sign_mask = 0x80000000UL;
    const uint32_t round_mask = ~0xFFFUL;

    union FP32 in;
    in.f = value;
    uint32_t sign = in.u & sign_mask;
    in.u ^= sign;

    uint16_t out = 0;

    if (in.u >= f32inf.u)
    {
        out = (in.u > f32inf.u) ? (uint16_t)0x7FFFU : (uint16_t)0x7C00U;
    }
    else
    {
        in.u &= round_mask;
        in.f *= magic.f;
        in.u -= round_mask;
        if (in.u > f16inf.u)
        {
            in.u = f16inf.u;
        }
        out = (uint16_t)(in.u >> 13U);
    }

    out |= (uint16_t)(sign >> 16U);

    return out;
}

float canardConvertFloat16ToNativeFloat(uint16_t value)
{
    CANARD_ASSERT(sizeof(float) == 4);

    union FP32
    {
        uint32_t u;
        float f;
    };

    const union FP32 magic = { (254UL - 15UL) << 23U };
    const union FP32 was_inf_nan = { (127UL + 16UL) << 23U };
    union FP32 out;

    out.u = (value & 0x7FFFU) << 13U;
    out.f *= magic.f;
    if (out.f >= was_inf_nan.f)
    {
        out.u |= 255UL << 23U;
    }
    out.u |= (value & 0x8000UL) << 16U;

    return out.f;
}

/*
 * Internal (static functions)
 */
CANARD_INTERNAL int16_t computeTransferIDForwardDistance(uint8_t a, uint8_t b)
{
    int16_t d = (int16_t)(b - a);
    if (d < 0)
    {
        d = (int16_t)(d + (int16_t)(1U << TRANSFER_ID_BIT_LEN));
    }
    return d;
}

CANARD_INTERNAL void incrementTransferID(uint8_t* transfer_id)
{
    CANARD_ASSERT(transfer_id != NULL);

    (*transfer_id)++;
    if (*transfer_id >= 32)
    {
        *transfer_id = 0;
    }
}

CANARD_INTERNAL int16_t enqueueTxFrames(CanardInstance* ins,
                                        uint32_t can_id,
                                        uint8_t* transfer_id,
                                        uint16_t crc,
                                        const uint8_t* payload,
                                        uint16_t payload_len)
{
    CANARD_ASSERT(ins != NULL);
    CANARD_ASSERT((can_id & CANARD_CAN_EXT_ID_MASK) == can_id);            // Flags must be cleared

    if (transfer_id == NULL)
    {
        return -CANARD_ERROR_INVALID_ARGUMENT;
    }

    if ((payload_len > 0) && (payload == NULL))
    {
        return -CANARD_ERROR_INVALID_ARGUMENT;
    }

    int16_t result = 0;

    if (payload_len < CANARD_CAN_FRAME_MAX_DATA_LEN)                        // Single frame transfer
    {
        CanardTxQueueItem* queue_item = createTxItem(&ins->allocator);
        if (queue_item == NULL)
        {
            return -CANARD_ERROR_OUT_OF_MEMORY;
        }

        memcpy(queue_item->frame.data, payload, payload_len);

        queue_item->frame.data_len = (uint8_t)(payload_len + 1);
        queue_item->frame.data[payload_len] = (uint8_t)(0xC0U | (*transfer_id & 31U));
        queue_item->frame.id = can_id | CANARD_CAN_FRAME_EFF;

        pushTxQueue(ins, queue_item);
        result++;
    }
    else                                                                    // Multi frame transfer
    {
        uint16_t data_index = 0;
        uint8_t toggle = 0;
        uint8_t sot_eot = 0x80;

        CanardTxQueueItem* queue_item = NULL;

        while (payload_len - data_index != 0)
        {
            queue_item = createTxItem(&ins->allocator);
            if (queue_item == NULL)
            {
                return -CANARD_ERROR_OUT_OF_MEMORY;          // TODO: Purge all frames enqueued so far
            }

            uint8_t i = 0;
            if (data_index == 0)
            {
                // add crc
                queue_item->frame.data[0] = (uint8_t) (crc);
                queue_item->frame.data[1] = (uint8_t) (crc >> 8U);
                i = 2;
            }
            else
            {
                i = 0;
            }

            for (; i < (CANARD_CAN_FRAME_MAX_DATA_LEN - 1) && data_index < payload_len; i++, data_index++)
            {
                queue_item->frame.data[i] = payload[data_index];
            }
            // tail byte
            sot_eot = (data_index == payload_len) ? (uint8_t)0x40 : sot_eot;

            queue_item->frame.data[i] = (uint8_t)(sot_eot | ((uint32_t)toggle << 5U) | ((uint32_t)*transfer_id & 31U));
            queue_item->frame.id = can_id | CANARD_CAN_FRAME_EFF;
            queue_item->frame.data_len = (uint8_t)(i + 1);
            pushTxQueue(ins, queue_item);

            result++;
            toggle ^= 1;
            sot_eot = 0;
        }
    }

    return result;
}

/**
 * Puts frame on on the TX queue. Higher priority placed first
 */
CANARD_INTERNAL void pushTxQueue(CanardInstance* ins, CanardTxQueueItem* item)
{
    CANARD_ASSERT(ins != NULL);
    CANARD_ASSERT(item->frame.data_len > 0);       // UAVCAN doesn't allow zero-payload frames

    if (ins->tx_queue == NULL)
    {
        ins->tx_queue = item;
        return;
    }

    CanardTxQueueItem* queue = ins->tx_queue;
    CanardTxQueueItem* previous = ins->tx_queue;

    while (queue != NULL)
    {
        if (isPriorityHigher(queue->frame.id, item->frame.id)) // lower number wins
        {
            if (queue == ins->tx_queue)
            {
                item->next = queue;
                ins->tx_queue = item;
            }
            else
            {
                previous->next = item;
                item->next = queue;
            }
            return;
        }
        else
        {
            if (queue->next == NULL)
            {
                queue->next = item;
                return;
            }
            else
            {
                previous = queue;
                queue = queue->next;
            }
        }
    }
}

/**
 * Creates new tx queue item from allocator
 */
CANARD_INTERNAL CanardTxQueueItem* createTxItem(CanardPoolAllocator* allocator)
{
    CanardTxQueueItem* item = (CanardTxQueueItem*) allocateBlock(allocator);
    if (item == NULL)
    {
        return NULL;
    }
    memset(item, 0, sizeof(*item));
    return item;
}

/**
 * Returns true if priority of rhs is higher than id
 */
CANARD_INTERNAL bool isPriorityHigher(uint32_t rhs, uint32_t id)
{
    const uint32_t clean_id = id & CANARD_CAN_EXT_ID_MASK;
    const uint32_t rhs_clean_id = rhs & CANARD_CAN_EXT_ID_MASK;

    /*
     * STD vs EXT - if 11 most significant bits are the same, EXT loses.
     */
    const bool ext = (id & CANARD_CAN_FRAME_EFF) != 0;
    const bool rhs_ext = (rhs & CANARD_CAN_FRAME_EFF) != 0;
    if (ext != rhs_ext)
    {
        uint32_t arb11 = ext ? (clean_id >> 18U) : clean_id;
        uint32_t rhs_arb11 = rhs_ext ? (rhs_clean_id >> 18U) : rhs_clean_id;
        if (arb11 != rhs_arb11)
        {
            return arb11 < rhs_arb11;
        }
        else
        {
            return rhs_ext;
        }
    }

    /*
     * RTR vs Data frame - if frame identifiers and frame types are the same, RTR loses.
     */
    const bool rtr = (id & CANARD_CAN_FRAME_RTR) != 0;
    const bool rhs_rtr = (rhs & CANARD_CAN_FRAME_RTR) != 0;
    if (clean_id == rhs_clean_id && rtr != rhs_rtr)
    {
        return rhs_rtr;
    }

    /*
     * Plain ID arbitration - greater value loses.
     */
    return clean_id < rhs_clean_id;
}

/**
 * preps the rx state for the next transfer. does not delete the state
 */
CANARD_INTERNAL void prepareForNextTransfer(CanardRxState* state)
{
    CANARD_ASSERT(state->buffer_blocks == NULL);
    state->transfer_id++;
    state->payload_len = 0;
    state->next_toggle = 0;
}

/**
 * returns data type from id
 */
CANARD_INTERNAL uint16_t extractDataType(uint32_t id)
{
    if (extractTransferType(id) == CanardTransferTypeBroadcast)
    {
        uint16_t dtid = MSG_TYPE_FROM_ID(id);
        if (SOURCE_ID_FROM_ID(id) == CANARD_BROADCAST_NODE_ID)
        {
            dtid &= (1U << ANON_MSG_DATA_TYPE_ID_BIT_LEN) - 1U;
        }
        return dtid;
    }
    else
    {
        return (uint16_t) SRV_TYPE_FROM_ID(id);
    }
}

/**
 * returns transfer type from id
 */
CANARD_INTERNAL CanardTransferType extractTransferType(uint32_t id)
{
    const bool is_service = SERVICE_NOT_MSG_FROM_ID(id);
    if (!is_service)
    {
        return CanardTransferTypeBroadcast;
    }
    else if (REQUEST_NOT_RESPONSE_FROM_ID(id) == 1)
    {
        return CanardTransferTypeRequest;
    }
    else
    {
        return CanardTransferTypeResponse;
    }
}

/*
 *  CanardRxState functions
 */

/**
 * Traverses the list of CanardRxState's and returns a pointer to the CanardRxState
 * with either the Id or a new one at the end
 */
CANARD_INTERNAL CanardRxState* traverseRxStates(CanardInstance* ins, uint32_t transfer_descriptor)
{
    CanardRxState* states = ins->rx_states;

    if (states == NULL) // initialize CanardRxStates
    {
        states = createRxState(&ins->allocator, transfer_descriptor);
        
        if(states == NULL)
        {
            return NULL;
        }

        ins->rx_states = states;
        return states;
    }

    states = findRxState(states, transfer_descriptor);
    if (states != NULL)
    {
        return states;
    }
    else
    {
        return prependRxState(ins, transfer_descriptor);
    }
}

/**
 * returns pointer to the rx state of transfer descriptor or null if not found
 */
CANARD_INTERNAL CanardRxState* findRxState(CanardRxState* state, uint32_t transfer_descriptor)
{
    while (state != NULL)
    {
        if (state->dtid_tt_snid_dnid == transfer_descriptor)
        {
            return state;
        }
        state = state->next;
    }
    return NULL;
}

/**
 * prepends rx state to the canard instance rx_states
 */
CANARD_INTERNAL CanardRxState* prependRxState(CanardInstance* ins, uint32_t transfer_descriptor)
{
    CanardRxState* state = createRxState(&ins->allocator, transfer_descriptor);

    if(state == NULL)
    {
        return NULL;
    }

    state->next = ins->rx_states;
    ins->rx_states = state;
    return state;
}

CANARD_INTERNAL CanardRxState* createRxState(CanardPoolAllocator* allocator, uint32_t transfer_descriptor)
{
    CanardRxState init = {
        .next = NULL,
        .buffer_blocks = NULL,
        .dtid_tt_snid_dnid = transfer_descriptor
    };

    CanardRxState* state = (CanardRxState*) allocateBlock(allocator);
    if (state == NULL)
    {
        return NULL;
    }
    memcpy(state, &init, sizeof(*state));

    return state;
}

CANARD_INTERNAL uint64_t releaseStatePayload(CanardInstance* ins, CanardRxState* rxstate)
{
    while (rxstate->buffer_blocks != NULL)
    {
        CanardBufferBlock* const temp = rxstate->buffer_blocks->next;
        freeBlock(&ins->allocator, rxstate->buffer_blocks);
        rxstate->buffer_blocks = temp;
    }
    rxstate->payload_len = 0;
    return CANARD_OK;
}

/*
 *  CanardBufferBlock functions
 */

/**
 * pushes data into the rx state. Fills the buffer head, then appends data to buffer blocks
 */
CANARD_INTERNAL int16_t bufferBlockPushBytes(CanardPoolAllocator* allocator,
                                             CanardRxState* state,
                                             const uint8_t* data,
                                             uint8_t data_len)
{
    uint16_t data_index = 0;

    // if head is not full, add data to head
    if ((CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE - state->payload_len) > 0)
    {
        for (uint16_t i = (uint16_t)state->payload_len;
             i < CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE && data_index < data_len;
             i++, data_index++)
        {
            state->buffer_head[i] = data[data_index];
        }
        if (data_index >= data_len)
        {
            state->payload_len =
                (uint16_t)(state->payload_len + data_len) & ((1U << CANARD_TRANSFER_PAYLOAD_LEN_BITS) - 1U);
            return 1;
        }
    } // head is full.

    uint16_t index_at_nth_block =
        (uint16_t)(((state->payload_len) - CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE) % CANARD_BUFFER_BLOCK_DATA_SIZE);

    // get to current block
    CanardBufferBlock* block = NULL;

    // buffer blocks uninitialized
    if (state->buffer_blocks == NULL)
    {
        state->buffer_blocks = createBufferBlock(allocator);

        if (state->buffer_blocks == NULL)
        {
            return -CANARD_ERROR_OUT_OF_MEMORY;
        }

        block = state->buffer_blocks;
        index_at_nth_block = 0;
    }
    else
    {
        uint16_t nth_block = 1;

        // get to block
        block = state->buffer_blocks;
        while (block->next != NULL)
        {
            nth_block++;
            block = block->next;
        }

        const uint16_t num_buffer_blocks =
            (uint16_t) (((((uint32_t)state->payload_len + data_len) - CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE) /
                         CANARD_BUFFER_BLOCK_DATA_SIZE) + 1U);

        if (num_buffer_blocks > nth_block && index_at_nth_block == 0)
        {
            block->next = createBufferBlock(allocator);
            if (block->next == NULL)
            {
                return -CANARD_ERROR_OUT_OF_MEMORY;
            }
            block = block->next;
        }
    }

    // add data to current block until it becomes full, add new block if necessary
    while (data_index < data_len)
    {
        for (uint16_t i = index_at_nth_block;
             i < CANARD_BUFFER_BLOCK_DATA_SIZE && data_index < data_len;
             i++, data_index++)
        {
            block->data[i] = data[data_index];
        }

        if (data_index < data_len)
        {
            block->next = createBufferBlock(allocator);
            if (block->next == NULL)
            {
                return -CANARD_ERROR_OUT_OF_MEMORY;
            }
            block = block->next;
            index_at_nth_block = 0;
        }
    }

    state->payload_len = (uint16_t)(state->payload_len + data_len) & ((1U << CANARD_TRANSFER_PAYLOAD_LEN_BITS) - 1U);

    return 1;
}

CANARD_INTERNAL CanardBufferBlock* createBufferBlock(CanardPoolAllocator* allocator)
{
    CanardBufferBlock* block = (CanardBufferBlock*) allocateBlock(allocator);
    if (block == NULL)
    {
        return NULL;
    }
    block->next = NULL;
    return block;
}

/**
 * Bit array copy routine, originally developed by Ben Dyer for Libuavcan. Thanks Ben.
 */
void copyBitArray(const uint8_t* src, uint32_t src_offset, uint32_t src_len,
                        uint8_t* dst, uint32_t dst_offset)
{
    CANARD_ASSERT(src_len > 0U);

    // Normalizing inputs
    src += src_offset / 8U;
    dst += dst_offset / 8U;

    src_offset %= 8U;
    dst_offset %= 8U;

    const size_t last_bit = src_offset + src_len;
    while (last_bit - src_offset)
    {
        const uint8_t src_bit_offset = (uint8_t)(src_offset % 8U);
        const uint8_t dst_bit_offset = (uint8_t)(dst_offset % 8U);

        const uint8_t max_offset = MAX(src_bit_offset, dst_bit_offset);
        const uint32_t copy_bits = MIN(last_bit - src_offset, 8U - max_offset);

        const uint8_t write_mask = (uint8_t)((uint8_t)(0xFF00U >> copy_bits) >> dst_bit_offset);
        const uint8_t src_data = (uint8_t)(((uint32_t)src[src_offset / 8U] << src_bit_offset) >> dst_bit_offset);

        dst[dst_offset / 8U] =
            (uint8_t)(((uint32_t)dst[dst_offset / 8U] & (uint32_t)~write_mask) | (uint32_t)(src_data & write_mask));

        src_offset += copy_bits;
        dst_offset += copy_bits;
    }
}

CANARD_INTERNAL int16_t descatterTransferPayload(const CanardRxTransfer* transfer,
                                                 uint32_t bit_offset,
                                                 uint8_t bit_length,
                                                 void* output)
{
    CANARD_ASSERT(transfer != 0);

    if (bit_offset >= transfer->payload_len * 8)
    {
        return 0;       // Out of range, reading zero bits
    }

    if (bit_offset + bit_length > transfer->payload_len * 8)
    {
        bit_length = (uint8_t)(transfer->payload_len * 8U - bit_offset);
    }

    CANARD_ASSERT(bit_length > 0);

    if ((transfer->payload_middle != NULL) || (transfer->payload_tail != NULL)) // Multi frame
    {
        /*
         * This part is hideously complicated and probably should be redesigned.
         * The objective here is to copy the requested number of bits from scattered storage into the temporary
         * local storage. We go through great pains to ensure that all corner cases are handled correctly.
         */
        uint32_t input_bit_offset = bit_offset;
        uint8_t output_bit_offset = 0;
        uint8_t remaining_bit_length = bit_length;

        // Reading head
        if (input_bit_offset < CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE * 8)
        {
            const uint8_t amount = (uint8_t)MIN(remaining_bit_length,
                                                CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE * 8U - input_bit_offset);

            copyBitArray(&transfer->payload_head[0], input_bit_offset, amount, (uint8_t*) output, 0);

            input_bit_offset += amount;
            output_bit_offset = (uint8_t)(output_bit_offset + amount);
            remaining_bit_length = (uint8_t)(remaining_bit_length - amount);
        }

        // Reading middle
        uint32_t remaining_bits = transfer->payload_len * 8U - CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE * 8U;
        uint32_t block_bit_offset = CANARD_MULTIFRAME_RX_PAYLOAD_HEAD_SIZE * 8U;
        const CanardBufferBlock* block = transfer->payload_middle;

        while ((block != NULL) && (remaining_bit_length > 0))
        {
            CANARD_ASSERT(remaining_bits > 0);
            const uint32_t block_end_bit_offset = block_bit_offset + MIN(CANARD_BUFFER_BLOCK_DATA_SIZE * 8,
                                                                         remaining_bits);

            // Perform copy if we've reached the requested offset, otherwise jump over this block and try next
            if (block_end_bit_offset > input_bit_offset)
            {
                const uint8_t amount = (uint8_t) MIN(remaining_bit_length, block_end_bit_offset - input_bit_offset);

                CANARD_ASSERT(input_bit_offset >= block_bit_offset);
                const uint32_t bit_offset_within_block = input_bit_offset - block_bit_offset;

                copyBitArray(&block->data[0], bit_offset_within_block, amount, (uint8_t*) output, output_bit_offset);

                input_bit_offset += amount;
                output_bit_offset = (uint8_t)(output_bit_offset + amount);
                remaining_bit_length = (uint8_t)(remaining_bit_length - amount);
            }

            CANARD_ASSERT(block_end_bit_offset > block_bit_offset);
            remaining_bits -= block_end_bit_offset - block_bit_offset;
            block_bit_offset = block_end_bit_offset;
            block = block->next;
        }

        CANARD_ASSERT(remaining_bit_length <= remaining_bits);

        // Reading tail
        if ((transfer->payload_tail != NULL) && (remaining_bit_length > 0))
        {
            CANARD_ASSERT(input_bit_offset >= block_bit_offset);
            const uint32_t offset = input_bit_offset - block_bit_offset;

            copyBitArray(&transfer->payload_tail[0], offset, remaining_bit_length, (uint8_t*) output,
                         output_bit_offset);

            input_bit_offset += remaining_bit_length;
            output_bit_offset = (uint8_t)(output_bit_offset + remaining_bit_length);
            remaining_bit_length = 0;
        }

        CANARD_ASSERT(input_bit_offset <= transfer->payload_len * 8);
        CANARD_ASSERT(output_bit_offset <= 64);
        CANARD_ASSERT(remaining_bit_length == 0);
    }
    else                                                                    // Single frame
    {
        copyBitArray(&transfer->payload_head[0], bit_offset, bit_length, (uint8_t*) output, 0);
    }

    return bit_length;
}

CANARD_INTERNAL bool isBigEndian(void)
{
#if defined(BYTE_ORDER) && defined(BIG_ENDIAN)
    return BYTE_ORDER == BIG_ENDIAN;                // Some compilers offer this neat shortcut
#else
    union
    {
        uint16_t a;
        uint8_t b[2];
    } u;
    u.a = 1;
    return u.b[1] == 1;                             // Some don't...
#endif
}

CANARD_INTERNAL void swapByteOrder(void* data, size_t size)
{
    CANARD_ASSERT(data != NULL);

    uint8_t* const bytes = (uint8_t*) data;

    size_t fwd = 0;
    size_t rev = size - 1;

    while (fwd < rev)
    {
        const uint8_t x = bytes[fwd];
        bytes[fwd] = bytes[rev];
        bytes[rev] = x;
        fwd++;
        rev--;
    }
}

/*
 * CRC functions
 */
CANARD_INTERNAL uint16_t crcAddByte(uint16_t crc_val, uint8_t byte)
{
    crc_val ^= (uint16_t) ((uint16_t) (byte) << 8U);
    for (uint8_t j = 0; j < 8; j++)
    {
        if (crc_val & 0x8000U)
        {
            crc_val = (uint16_t) ((uint16_t) (crc_val << 1U) ^ 0x1021U);
        }
        else
        {
            crc_val = (uint16_t) (crc_val << 1U);
        }
    }
    return crc_val;
}

CANARD_INTERNAL uint16_t crcAddSignature(uint16_t crc_val, uint64_t data_type_signature)
{
    for (uint16_t shift_val = 0; shift_val < 64; shift_val = (uint16_t)(shift_val + 8U))
    {
        crc_val = crcAddByte(crc_val, (uint8_t) (data_type_signature >> shift_val));
    }
    return crc_val;
}

CANARD_INTERNAL uint16_t crcAdd(uint16_t crc_val, const uint8_t* bytes, size_t len)
{
    while (len--)
    {
        crc_val = crcAddByte(crc_val, *bytes++);
    }
    return crc_val;
}

/*
 *  Pool Allocator functions
 */
CANARD_INTERNAL void initPoolAllocator(CanardPoolAllocator* allocator,
                                       CanardPoolAllocatorBlock* buf,
                                       uint16_t buf_len)
{
    size_t current_index = 0;
    CanardPoolAllocatorBlock** current_block = &(allocator->free_list);
    while (current_index < buf_len)
    {
        *current_block = &buf[current_index];
        current_block = &((*current_block)->next);
        current_index++;
    }
    *current_block = NULL;

    allocator->statistics.capacity_blocks = buf_len;
    allocator->statistics.current_usage_blocks = 0;
    allocator->statistics.peak_usage_blocks = 0;
}

CANARD_INTERNAL void* allocateBlock(CanardPoolAllocator* allocator)
{
    // Check if there are any blocks available in the free list.
    if (allocator->free_list == NULL)
    {
        return NULL;
    }

    // Take first available block and prepares next block for use.
    void* result = allocator->free_list;
    allocator->free_list = allocator->free_list->next;

    // Update statistics
    allocator->statistics.current_usage_blocks++;
    if (allocator->statistics.peak_usage_blocks < allocator->statistics.current_usage_blocks)
    {
        allocator->statistics.peak_usage_blocks = allocator->statistics.current_usage_blocks;
    }

    return result;
}

CANARD_INTERNAL void freeBlock(CanardPoolAllocator* allocator, void* p)
{
    CanardPoolAllocatorBlock* block = (CanardPoolAllocatorBlock*) p;

    block->next = allocator->free_list;
    allocator->free_list = block;

    CANARD_ASSERT(allocator->statistics.current_usage_blocks > 0);
    allocator->statistics.current_usage_blocks--;
}
