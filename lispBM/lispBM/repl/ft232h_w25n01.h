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

/*
 * Low-level driver for W25N01GVZEIG SPI NAND flash via FT232H MPSSE.
 *
 * Wiring (FT232H, standard MPSSE SPI):
 *   AD0 (TCK/CLK) → Flash SCK
 *   AD1 (TDI/DO)  → Flash SDI   (MOSI)
 *   AD2 (TDO/DI)  ← Flash SDO   (MISO)
 *   AD3           → Flash CS#   (active-low)
 *   AD4           → Flash WP#   (drive high = not write-protected)
 *   AD5           → Flash HLD#  (drive high = not held)
 *
 * Requires: libftdi1-dev  (sudo apt install libftdi1-dev)
 */

#ifndef FT232H_W25N01_H_
#define FT232H_W25N01_H_

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// W25N01GVZEIG geometry
#define NAND_PAGE_DATA_SIZE    2048
#define NAND_PAGE_SPARE_SIZE     64
#define NAND_PAGE_TOTAL_SIZE   (NAND_PAGE_DATA_SIZE + NAND_PAGE_SPARE_SIZE)
#define NAND_PAGES_PER_BLOCK     64
#define NAND_NUM_BLOCKS        1024
#define NAND_NUM_PAGES         (NAND_NUM_BLOCKS * NAND_PAGES_PER_BLOCK)

// Status register addresses passed to nand_read_status / nand_write_status
#define NAND_SR1  0xA0   // Protection Register
#define NAND_SR2  0xB0   // Configuration Register
#define NAND_SR3  0xC0   // Status Register

// SR3 bit flags
#define NAND_SR3_BUSY   0x01
#define NAND_SR3_WEL    0x02
#define NAND_SR3_EFAIL  0x04
#define NAND_SR3_PFAIL  0x08
#define NAND_SR3_ECC0   0x10
#define NAND_SR3_ECC1   0x20

// ECC result returned by nand_read_page
typedef enum {
  NAND_ECC_OK             = 0,  // no errors
  NAND_ECC_CORRECTED      = 1,  // 1-4 bit errors corrected by internal ECC
  NAND_ECC_UNCORRECTABLE  = 2,  // uncorrectable bit errors — data invalid
  NAND_ECC_ERROR          = 3,  // SPI or timeout error
} nand_ecc_t;

// ---- Device open/close -----------------------------------------------

// Open FT232H and initialise MPSSE SPI.
// Returns true on success.
bool nand_open(void);

void nand_close(void);

// ---- Raw SPI transfer ------------------------------------------------

// Full-duplex transfer: clock out len bytes from tx, clock in len bytes
// to rx.  Either tx or rx may be NULL (zeros sent / result discarded).
bool nand_spi_xfer(const uint8_t *tx, uint8_t *rx, int len);

// ---- W25N01GVZEIG commands -------------------------------------------

// Send FFh reset.  Waits 1 ms for tRST.
bool nand_reset(void);

// Read 3-byte JEDEC ID into id[].  Expected: {0xEF, 0xAA, 0x21}.
bool nand_read_id(uint8_t id[3]);

// Read one byte from the named status register (NAND_SR1/SR2/SR3).
uint8_t nand_read_status(uint8_t reg);

// Write one byte to the named status register.
bool nand_write_status(uint8_t reg, uint8_t val);

// Poll SR3 BUSY bit.  Returns true when ready, false on timeout.
// timeout_us is approximate (10 µs polling granularity).
bool nand_wait_ready(int timeout_us);

// Load page page_addr from the NAND array into the device data buffer,
// then read len bytes starting at column col into buf.
// col + len must not exceed NAND_PAGE_TOTAL_SIZE (2112).
// Returns NAND_ECC_OK/CORRECTED/UNCORRECTABLE/ERROR.
nand_ecc_t nand_read_page(uint16_t page_addr, uint16_t col, uint8_t *buf, int len);

// Write len bytes from buf into the device data buffer starting at col,
// then execute a Program Execute to flash page page_addr.
// The target page must already be erased.
// Returns false if PFAIL is set in SR3 after programming.
bool nand_write_page(uint16_t page_addr, uint16_t col, const uint8_t *buf, int len);

// Erase the 128 KB block containing block_addr (0-1023).
// Returns false if EFAIL is set in SR3 after erasing.
bool nand_erase_block(uint16_t block_addr);

// Returns true if block_addr is bad (factory marker or unreadable).
bool nand_is_bad_block(uint16_t block_addr);

#ifdef __cplusplus
}
#endif
#endif
