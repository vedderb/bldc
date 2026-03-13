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

/** \file lbm_xmodem.h
 *
 * XMODEM-CRC receiver utility for use in LispBM integrations.
 *
 * Typical use: connect the data callback to lbm_channel_write on a
 * buffered char channel.  CHANNEL_FULL causes the callback to spin, which
 * delays the ACK and throttles the transfer to the rate LispBM consumes input.
 *
 * Protocol: XMODEM-CRC, 128-byte blocks.
 *   SOH | blknum | ~blknum | 128 bytes | CRC_hi | CRC_lo
 *   CRC-16/CCITT, poly 0x1021, init 0x0000, big-endian.
 *   Receiver initiates with 'C'. Transfer ends with EOT from sender.
 */

#ifndef LBM_XMODEM_H_
#define LBM_XMODEM_H_

#include <stdint.h>
#include <stdbool.h>

/** Result codes returned by lbm_xmodem_receive. */
typedef enum {
  LBM_XMODEM_OK          = 0,  /**< Transfer completed successfully. */
  LBM_XMODEM_ERR_TIMEOUT = 1,  /**< No response from sender within retry limit. */
  LBM_XMODEM_ERR_SYNC    = 2,  /**< Block number mismatch or framing error. */
  LBM_XMODEM_ERR_CRC     = 3,  /**< CRC check failed after max retries. */
  LBM_XMODEM_ERR_CANCEL  = 4,  /**< Transfer cancelled (CAN received or data cb returned false). */
} lbm_xmodem_result_t;

/** Callback to read one byte from the transport.
 *  \param c         Byte received.
 *  \param timeout_ms  Maximum time to wait in milliseconds.  0 = non-blocking.
 *  \param arg       Opaque argument passed through from lbm_xmodem_receive.
 *  \return true if a byte was received within the timeout, false on timeout.
 */
typedef bool (*lbm_xmodem_getc_t)(uint8_t *c, int timeout_ms, void *arg);

/** Callback to write one byte to the transport.
 *  \param c    Byte to send (ACK / NAK / CAN / 'C').
 *  \param arg  Opaque argument.
 */
typedef void (*lbm_xmodem_putc_t)(uint8_t c, void *arg);

/** Callback invoked with each successfully received block of data.
 *  The block is 128 bytes for standard XMODEM-CRC.
 *  \param buf  Pointer to received data (valid only for the duration of the call).
 *  \param len  Number of bytes (128).
 *  \param arg  Opaque argument.
 *  \return true to continue transfer, false to abort (sends CAN to sender).
 */
typedef bool (*lbm_xmodem_data_t)(const uint8_t *buf, unsigned int len, void *arg);

/** Run the XMODEM-CRC receiver state machine.
 *
 *  Blocks until the transfer completes, fails, or is cancelled.
 *
 *  \param getc  Byte-receive callback.
 *  \param putc  Byte-send callback.
 *  \param data  Data-block callback.
 *  \param arg   Opaque argument forwarded to all three callbacks.
 *  \return      Result code indicating success or failure reason.
 */
lbm_xmodem_result_t lbm_xmodem_receive(lbm_xmodem_getc_t getc,
                                        lbm_xmodem_putc_t putc,
                                        lbm_xmodem_data_t data,
                                        void *arg);

#endif /* LBM_XMODEM_H_ */
