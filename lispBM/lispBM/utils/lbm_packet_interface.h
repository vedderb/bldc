/*
  Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

  This file is part of LispBM.

  LispBM is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LispBM is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LispBM. If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * lbm_packet_interface - VESC packet protocol support for LispBM examples.
 *
 * Implements a minimal subset of COMM_PACKET_ID commands:
 *
 *   COMM_FW_VERSION       (0)   - Hardware/firmware identification
 *   COMM_LISP_SET_RUNNING (133) - Start or stop the LispBM evaluator
 *   COMM_LISP_GET_STATS   (134) - Report heap and memory usage
 *   COMM_LISP_PRINT       (135) - Send text output from device to host
 *   COMM_LISP_REPL_CMD    (138) - Evaluate an expression and return the result
 *   COMM_LISP_STREAM_CODE (139) - Stream LispBM source to the incremental reader
 *
 * Packet framing uses utils/packet.h (VESC framing with CRC16).
 * Feed received bytes one at a time via lbm_packet_if_process_byte().
 * Send output to the host via lbm_packet_if_printf().
 *
 * Usage:
 *   1. Fill in an lbm_packet_if_cfg_t for your platform.
 *   2. Call lbm_packet_if_init() after lbm_image_boot().
 *   3. In your UART/USB receive loop call lbm_packet_if_process_byte()
 *      for each incoming byte.
 *   4. In your print extension call lbm_packet_if_printf() instead of
 *      writing directly to the serial port.
 */

#ifndef LBM_PACKET_INTERFACE_H_
#define LBM_PACKET_INTERFACE_H_

#include <stdint.h>
#include <stdbool.h>
#include "eval_cps.h"

typedef struct {
  /**
   * Hardware name string returned in COMM_FW_VERSION, e.g. "ESP32C3".
   * Must remain valid for the lifetime of the interface. Required.
   */
  const char *hw_name;

  /**
   * Firmware name string returned in COMM_FW_VERSION, e.g. "LispBM".
   * Must remain valid for the lifetime of the interface. Required.
   */
  const char *fw_name;

  /** Major and minor version numbers returned in COMM_FW_VERSION. */
  uint8_t fw_version_major;
  uint8_t fw_version_minor;

  /**
   * Send len bytes over the serial link. Required.
   */
  void (*send)(uint8_t *data, unsigned int len);

  /**
   * Sleep for ms milliseconds.
   * Used in pause-wait loops and the stream write retry loop.
   * If NULL a busy-wait is used instead (not recommended on RTOS).
   */
  void (*sleep_ms)(uint32_t ms);

  /**
   * Restart LispBM. load_code is currently unused but reserved for
   * platforms that persist code and want to reload it on restart.
   * Returns true on success.
   * If NULL, SET_RUNNING with running=true does nothing.
   */
  bool (*lbm_restart)(bool load_code);

  /**
   * Optional done-callback forwarding. Called for every completed LispBM
   * context that is not the active REPL context. Set to NULL if not needed.
   */
  void (*ctx_done)(eval_context_t *ctx);
} lbm_packet_if_cfg_t;

/**
 * Initialize the packet interface.
 * Must be called after lbm_image_boot() and before processing any bytes.
 */
void lbm_packet_if_init(lbm_packet_if_cfg_t *cfg);

/**
 * Feed one received byte into the packet decoder.
 * Invoke this for every byte that arrives on the serial link.
 */
void lbm_packet_if_process_byte(uint8_t byte);

/**
 * Send a formatted string to the host as a COMM_LISP_PRINT packet.
 * May be called from extensions or done-callbacks.
 * Output longer than PACKET_MAX_PL_LEN - 2 bytes is truncated.
 */
void lbm_packet_if_printf(const char *fmt, ...);

#endif /* LBM_PACKET_INTERFACE_H_ */
