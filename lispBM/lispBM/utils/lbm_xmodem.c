/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

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

#include "lbm_xmodem.h"
#include "crc.h"
#include <string.h>

#define SOH   0x01u   /* start of 128-byte block */
#define EOT   0x04u   /* end of transmission */
#define ACK   0x06u   /* acknowledge */
#define NAK   0x15u   /* negative acknowledge / initiate checksum mode */
#define CAN   0x18u   /* cancel */
#define SUB   0x1Au   /* XMODEM padding byte (Ctrl-Z) */
#define CCHAR 'C'     /* initiate CRC mode */

#define BLOCK_SIZE     128u
#define MAX_RETRIES    60
#define INIT_TIMEOUT   500   /* ms to wait for first SOH after sending 'C' */
#define BLOCK_TIMEOUT  1000  /* ms to wait for each byte within a block */
#define EOT_TIMEOUT    3000  /* ms to wait for SOH/EOT between blocks */

static void send_cancel(lbm_xmodem_putc_t putc, void *arg) {
  putc(CAN, arg);
  putc(CAN, arg);
}

/* Read one full block given that SOH has already been consumed.
 * Returns LBM_XMODEM_OK on success, error code otherwise.
 * On CRC/framing errors, *crc_ok is false so the caller can NAK.
 */
static lbm_xmodem_result_t read_block(lbm_xmodem_getc_t getc,
                                      lbm_xmodem_putc_t putc,
                                      void *arg,
                                      uint8_t *blknum_out,
                                      uint8_t *blknum_inv_out,
                                      uint8_t *block,
                                      bool *crc_ok) {
  *crc_ok = false;
  uint8_t blknum, blknum_inv;
  if (!getc(&blknum, BLOCK_TIMEOUT, arg) ||
      !getc(&blknum_inv, BLOCK_TIMEOUT, arg)) {
    send_cancel(putc, arg);
    return LBM_XMODEM_ERR_TIMEOUT;
  }
  *blknum_out = blknum;
  *blknum_inv_out = blknum_inv;

  if ((uint8_t)(blknum + blknum_inv) != 0xFFu) {
    return LBM_XMODEM_OK; /* crc_ok stays false, caller NAKs */
  }

  for (unsigned int i = 0; i < BLOCK_SIZE; i++) {
    if (!getc(&block[i], BLOCK_TIMEOUT, arg)) {
      send_cancel(putc, arg);
      return LBM_XMODEM_ERR_TIMEOUT;
    }
  }

  uint8_t crc_hi, crc_lo;
  if (!getc(&crc_hi, BLOCK_TIMEOUT, arg) ||
      !getc(&crc_lo, BLOCK_TIMEOUT, arg)) {
    send_cancel(putc, arg);
    return LBM_XMODEM_ERR_TIMEOUT;
  }

  uint16_t received_crc = ((uint16_t)crc_hi << 8) | crc_lo;
  uint16_t computed_crc = crc16(block, BLOCK_SIZE);
  *crc_ok = (received_crc == computed_crc);
  return LBM_XMODEM_OK;
}

/* Deliver the final buffered block after stripping trailing SUB padding bytes.
 * Called when EOT is received. Returns false if data callback rejects the data.
 */
static bool deliver_final(lbm_xmodem_data_t data, const uint8_t *block, void *arg) {
  unsigned int len = BLOCK_SIZE;
  while (len > 0 && block[len - 1] == SUB) {
    len--;
  }
  if (len == 0) {
    return true; /* nothing to deliver */
  }
  return data(block, len, arg);
}

lbm_xmodem_result_t lbm_xmodem_receive(lbm_xmodem_getc_t getc,
                                        lbm_xmodem_putc_t putc,
                                        lbm_xmodem_data_t data,
                                        void *arg) {
  uint8_t block[BLOCK_SIZE];
  uint8_t prev[BLOCK_SIZE];
  bool prev_valid = false;
  uint8_t expected_blk = 1;

  /* Initiate CRC mode: send 'C' repeatedly until sender responds with SOH */
  int init_retries = 0;
  bool got_soh = false;
  while (!got_soh) {
    putc(CCHAR, arg);
    uint8_t c;
    if (getc(&c, INIT_TIMEOUT, arg)) {
      if (c == SOH) {
        got_soh = true;
      } else if (c == CAN) {
        return LBM_XMODEM_ERR_CANCEL;
      }
    }
    if (!got_soh) {
      init_retries++;
      if (init_retries >= MAX_RETRIES) {
        return LBM_XMODEM_ERR_TIMEOUT;
      }
    }
  }

  /* Main block receive loop.
   * On entry here, SOH has been consumed (from init loop or previous iteration).
   *
   * Design: ACK each good block immediately so the sender can start transmitting
   * the next one.  The previous block is delivered to the caller *after* the ACK,
   * while the sender is busy with the next transmission – natural pipelining.
   * The very last block (identified by the EOT that follows it) is delivered with
   * trailing SUB (0x1A) padding bytes stripped, so the caller sees only payload.
   */
  for (;;) {
    uint8_t blknum, blknum_inv;
    bool crc_ok;
    lbm_xmodem_result_t r = read_block(getc, putc, arg,
                                        &blknum, &blknum_inv,
                                        block, &crc_ok);
    if (r != LBM_XMODEM_OK) {
      return r;
    }

    if (!crc_ok) {
      putc(NAK, arg);
    } else if (blknum == ((expected_blk - 1u) & 0xFFu)) {
      /* Duplicate of previous block (our ACK was lost) – re-ACK, discard */
      putc(ACK, arg);
    } else if (blknum != (expected_blk & 0xFFu)) {
      /* Block number out of sequence */
      send_cancel(putc, arg);
      return LBM_XMODEM_ERR_SYNC;
    } else {
      /* Good new block: ACK immediately so the sender can start block N+1,
       * then deliver the previous block to the caller while the sender transmits. */
      putc(ACK, arg);
      if (prev_valid) {
        if (!data(prev, BLOCK_SIZE, arg)) {
          send_cancel(putc, arg);
          return LBM_XMODEM_ERR_CANCEL;
        }
      }
      memcpy(prev, block, BLOCK_SIZE);
      prev_valid = true;
      expected_blk++;
    }

    /* Wait for next header byte (SOH or EOT) */
    uint8_t c;
    if (!getc(&c, EOT_TIMEOUT, arg)) {
      send_cancel(putc, arg);
      return LBM_XMODEM_ERR_TIMEOUT;
    }

    if (c == EOT) {
      /* Transfer complete: deliver final block with SUB padding stripped. */
      if (prev_valid) {
        deliver_final(data, prev, arg);
      }
      putc(ACK, arg);
      return LBM_XMODEM_OK;
    }

    if (c == CAN) {
      return LBM_XMODEM_ERR_CANCEL;
    }

    if (c != SOH) {
      /* Garbage between blocks – drain until a recognisable header byte */
      putc(NAK, arg);
      for (;;) {
        if (!getc(&c, BLOCK_TIMEOUT, arg)) {
          send_cancel(putc, arg);
          return LBM_XMODEM_ERR_TIMEOUT;
        }
        if (c == SOH || c == EOT || c == CAN) break;
      }
      if (c == EOT) {
        if (prev_valid) {
          deliver_final(data, prev, arg);
        }
        putc(ACK, arg);
        return LBM_XMODEM_OK;
      }
      if (c == CAN) { return LBM_XMODEM_ERR_CANCEL; }
      /* c == SOH: fall through to next iteration */
    }
    /* SOH consumed, loop back to read_block */
  }
}
