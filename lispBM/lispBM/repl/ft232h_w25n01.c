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

#include "ft232h_w25n01.h"
#include <ftdi.h>
#include <stdlib.h>
#include <string.h>
#include <threads.h>

// ////////////////////////////////////////////////////////////
// FT232H Pin assignment
// Standard assignment for the MPSSE (Multi-Protocol-Syncronous-Serial-Engine)
//
//   AD0 TCK/CLK  output  SCK
//   AD1 TDI/DO   output  SDI (MOSI)
//   AD2 TDO/DI   input   SDO (MISO)
//   AD3          output  CS#  (active-low)
//   AD4          output  WP#  (drive high = inactive)
//   AD5          output  HLD# (drive high = inactive)

#define PIN_CLK   0x01
#define PIN_MOSI  0x02
#define PIN_MISO  0x04
#define PIN_CS    0x08
#define PIN_WP    0x10
#define PIN_HLD   0x20

// Direction: 1 = output.  MISO is input (0).
#define ADBUS_DIR   (PIN_CLK | PIN_MOSI | PIN_CS | PIN_WP | PIN_HLD)

// Idle: CS# high (deasserted), WP# and HLD# high (inactive), CLK and MOSI low.
#define ADBUS_IDLE  (PIN_CS | PIN_WP | PIN_HLD)

// CS asserted (CS# driven low), WP# and HLD# remain high.
#define ADBUS_CS_LO (PIN_WP | PIN_HLD)

// SPI clock: 60 MHz / (2 * (div + 1)).  div = 2 → 10 MHz.
#define MPSSE_CLK_DIV  2

// W25N01GVZEIG opcodes
#define CMD_RESET        0xFF
#define CMD_JEDEC_ID     0x9F
#define CMD_READ_SR      0x0F
#define CMD_WRITE_SR     0x1F
#define CMD_WRITE_EN     0x06
#define CMD_PAGE_READ    0x13
#define CMD_FAST_READ    0x0B
#define CMD_LOAD_DATA    0x02
#define CMD_PROG_EXEC    0x10
#define CMD_BLOCK_ERASE  0xD8

// ////////////////////////////////////////////////////////////
// state

static struct ftdi_context *g_ftdi = NULL;

// ////////////////////////////////////////////////////////////
// sleep_us

void sleep_us(uint32_t us) {
  struct timespec ts = { .tv_sec = 0, .tv_nsec = (long)us * 1000 };
  thrd_sleep(&ts, NULL);
}

// ////////////////////////////////////////////////////////////
// MPSSE helpers

static bool mpsse_write(const uint8_t *buf, int len) {
  return ftdi_write_data(g_ftdi, (unsigned char *)buf, len) == len;
}

static bool mpsse_read(uint8_t *buf, int len) {
  int got = 0;
  while (got < len) {
    int r = ftdi_read_data(g_ftdi, (unsigned char *)(buf + got), len - got);
    if (r < 0) return false;
    got += r;
  }
  return true;
}

// ////////////////////////////////////////////////////////////
// open / close

bool nand_open(void) {
  if (g_ftdi) {
    ftdi_usb_close(g_ftdi);
    ftdi_free(g_ftdi);
    g_ftdi = NULL;
  }

  g_ftdi = ftdi_new();
  if (!g_ftdi) return false;

  if (ftdi_set_interface(g_ftdi, INTERFACE_A) < 0)      goto fail;
  if (ftdi_usb_open(g_ftdi, 0x0403, 0x6014) < 0)        goto fail;

  ftdi_usb_reset(g_ftdi);
  ftdi_set_latency_timer(g_ftdi, 1);
  ftdi_tcioflush(g_ftdi);

  if (ftdi_set_bitmode(g_ftdi, 0xFF, BITMODE_MPSSE) < 0) goto fail;

  // Wait for MPSSE to start up
  sleep_us(50000);

  // Initialise MPSSE for SPI Mode 0 at MPSSE_CLK_DIV
  uint8_t setup[] = {
    0x8A,                                    // disable divide-by-5 → 60 MHz base
    0x97,                                    // disable adaptive clocking
    0x8D,                                    // disable 3-phase data clocking
    0x85,                                    // loopback off
    0x86, MPSSE_CLK_DIV, 0x00,               // set clock divisor
    0x80, ADBUS_IDLE, ADBUS_DIR,             // set ADBUS state + direction
  };
  if (!mpsse_write(setup, (int)sizeof(setup))) goto fail;
  ftdi_tciflush(g_ftdi);
  return true;

fail:
  ftdi_usb_close(g_ftdi);
  ftdi_free(g_ftdi);
  g_ftdi = NULL;
  return false;
}

void nand_close(void) {
  if (g_ftdi) {
    ftdi_usb_close(g_ftdi);
    ftdi_free(g_ftdi);
    g_ftdi = NULL;
  }
}

// ////////////////////////////////////////////////////////////
// raw SPI transfer

// Max bytes per MPSSE sub-transfer. Interleaving write+read per chunk
// prevents the FTDI RX FIFO from filling up on large transfers.
#define MPSSE_CHUNK 512

bool nand_spi_xfer(const uint8_t *tx, uint8_t *rx, int len) {
  if (!g_ftdi || len <= 0) return false;

  // Assert CS#
  uint8_t cs_lo[] = {0x80, ADBUS_CS_LO, ADBUS_DIR};
  if (!mpsse_write(cs_lo, (int)sizeof(cs_lo))) return false;

  bool ok = true;
  int offset = 0;

  while (offset < len && ok) {
    int chunk = len - offset;
    if (chunk > MPSSE_CHUNK) chunk = MPSSE_CHUNK;

    // Build: MPSSE transfer cmd (3) + data (chunk) + flush (1)
    int cmdlen = 3 + chunk + 1;
    uint8_t *cmd = malloc((size_t)cmdlen);
    if (!cmd) { ok = false; break; }

    cmd[0] = 0x31;
    cmd[1] = (uint8_t)((chunk - 1) & 0xFF);
    cmd[2] = (uint8_t)(((chunk - 1) >> 8) & 0xFF);
    if (tx) memcpy(cmd + 3, tx + offset, (size_t)chunk);
    else    memset(cmd + 3, 0x00,         (size_t)chunk);
    cmd[3 + chunk] = 0x87; // flush — triggers immediate RX delivery

    ok = mpsse_write(cmd, cmdlen);
    free(cmd);
    if (!ok) break;

    // Read back this chunk before sending the next — clears the RX FIFO
    uint8_t *rxbuf = rx ? rx + offset : malloc((size_t)chunk);
    if (!rxbuf) { ok = false; break; }
    ok = mpsse_read(rxbuf, chunk);
    if (!rx) free(rxbuf);

    offset += chunk;
  }

  // Deassert CS#
  uint8_t cs_hi[] = {0x80, ADBUS_IDLE, ADBUS_DIR};
  mpsse_write(cs_hi, (int)sizeof(cs_hi));
  return ok;
}

// ////////////////////////////////////////////////////////////
// W25N01GVZEIG commands

bool nand_reset(void) {
  uint8_t tx[] = {CMD_RESET};
  bool ok = nand_spi_xfer(tx, NULL, 1);
  sleep_us(1000); // tRST ≤ 500 µs
  return ok;
}

bool nand_read_id(uint8_t id[3]) {
  // Opcode (1) + dummy (1) + 3 ID bytes.  W25N01 requires one dummy byte.
  uint8_t tx[5] = {CMD_JEDEC_ID, 0x00, 0x00, 0x00, 0x00};
  uint8_t rx[5] = {0};
  if (!nand_spi_xfer(tx, rx, 5)) return false;
  memcpy(id, rx + 2, 3);
  return true;
}

uint8_t nand_read_status(uint8_t reg) {
  uint8_t tx[3] = {CMD_READ_SR, reg, 0x00};
  uint8_t rx[3] = {0};
  nand_spi_xfer(tx, rx, 3);
  return rx[2];
}

bool nand_write_status(uint8_t reg, uint8_t val) {
  uint8_t tx_we[] = {CMD_WRITE_EN};
  if (!nand_spi_xfer(tx_we, NULL, 1)) return false;
  uint8_t tx[3] = {CMD_WRITE_SR, reg, val};
  return nand_spi_xfer(tx, NULL, 3);
}

bool nand_wait_ready(int timeout_us) {
  while (timeout_us > 0) {
    if (!(nand_read_status(NAND_SR3) & NAND_SR3_BUSY)) return true;
    sleep_us(10);
    timeout_us -= 10;
  }
  return false;
}

static bool write_enable(void) {
  uint8_t tx[] = {CMD_WRITE_EN};
  return nand_spi_xfer(tx, NULL, 1);
}

nand_ecc_t nand_read_page(uint16_t page_addr, uint16_t col, uint8_t *buf, int len) {
  if (!buf || col + len > NAND_PAGE_TOTAL_SIZE) return NAND_ECC_ERROR;

  // Page Data Read: load NAND array → internal data buffer
  uint8_t page_read[4] = {
    CMD_PAGE_READ,
    0x00,                          // dummy
    (uint8_t)(page_addr >> 8),
    (uint8_t)(page_addr & 0xFF),
  };
  if (!nand_spi_xfer(page_read, NULL, 4)) return NAND_ECC_ERROR;
  // tRD max 70 µs; allow 200 µs
  if (!nand_wait_ready(200)) return NAND_ECC_ERROR;

  // Fast Read from data buffer: opcode + col(2) + dummy(1) + data
  int txlen = 4 + len;
  uint8_t *tx = calloc(1, (size_t)txlen);
  uint8_t *rx = malloc((size_t)txlen);
  if (!tx || !rx) { free(tx); free(rx); return NAND_ECC_ERROR; }

  tx[0] = CMD_FAST_READ;
  tx[1] = (uint8_t)(col >> 8);
  tx[2] = (uint8_t)(col & 0xFF);
  tx[3] = 0x00; // dummy

  bool ok = nand_spi_xfer(tx, rx, txlen);
  if (ok) memcpy(buf, rx + 4, (size_t)len);
  free(tx); free(rx);
  if (!ok) return NAND_ECC_ERROR;

  uint8_t ecc = (nand_read_status(NAND_SR3) >> 4) & 0x03;
  if (ecc == 0x00) return NAND_ECC_OK;
  if (ecc == 0x01) return NAND_ECC_CORRECTED;
  return NAND_ECC_UNCORRECTABLE;
}

bool nand_write_page(uint16_t page_addr, uint16_t col, const uint8_t *buf, int len) {
  if (!buf || col + len > NAND_PAGE_TOTAL_SIZE) return false;

  if (!write_enable()) return false;

  int txlen = 3 + len;
  uint8_t *tx = malloc((size_t)txlen);
  if (!tx) return false;
  tx[0] = CMD_LOAD_DATA;
  tx[1] = (uint8_t)(col >> 8);
  tx[2] = (uint8_t)(col & 0xFF);
  memcpy(tx + 3, buf, (size_t)len);
  bool ok = nand_spi_xfer(tx, NULL, txlen);
  free(tx);
  if (!ok) return false;

  uint8_t prog[4] = {
    CMD_PROG_EXEC,
    0x00,
    (uint8_t)(page_addr >> 8),
    (uint8_t)(page_addr & 0xFF),
  };
  if (!nand_spi_xfer(prog, NULL, 4)) return false;
  if (!nand_wait_ready(2000)) return false;

  return !(nand_read_status(NAND_SR3) & NAND_SR3_PFAIL);
}

bool nand_is_bad_block(uint16_t block_addr) {
  uint16_t page_addr = (uint16_t)(block_addr * NAND_PAGES_PER_BLOCK);
  uint8_t marker;
  nand_ecc_t r = nand_read_page(page_addr, NAND_PAGE_DATA_SIZE, &marker, 1);
  if (r == NAND_ECC_ERROR) return true;
  return marker != 0xFF;
}

bool nand_erase_block(uint16_t block_addr) {
  if (!write_enable()) return false;

  // Block address maps to the first page of that block
  uint16_t page_addr = (uint16_t)(block_addr * NAND_PAGES_PER_BLOCK);
  uint8_t tx[4] = {
    CMD_BLOCK_ERASE,
    0x00,                          // dummy
    (uint8_t)(page_addr >> 8),
    (uint8_t)(page_addr & 0xFF),
  };
  if (!nand_spi_xfer(tx, NULL, 4)) return false;
  // tERS max 10 ms; allow 15 ms
  if (!nand_wait_ready(15000)) return false;

  return !(nand_read_status(NAND_SR3) & NAND_SR3_EFAIL);
}
