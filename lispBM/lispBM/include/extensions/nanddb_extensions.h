/*
    Copyright 2026 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef NANDDB_EXTENSIONS_H_
#define NANDDB_EXTENSIONS_H_

#ifdef __cplusplus
extern "C" {
#endif


// Flash Abstraction Layer struct.
typedef struct {
  uint32_t page_size;
  uint32_t pages_per_block;
  uint32_t num_blocks;

  nand_ecc_t (*read_page)(uint16_t page, uint16_t col, uint8_t *buf, int len);
  bool (*write_page)(uint16_t page, uint16_t col, const uint8_t *buf, int len);
  bool (*erase_block)(uint16_t block);
  bool (*is_factory_bad_block)(uint16_t block);
} lbm_nanddb_fal_t;

void lbm_nanddb_extensions_init(lbm_nanddb_fal_t *fal);

#ifdef __cplusplus
}
#endif


#endif
