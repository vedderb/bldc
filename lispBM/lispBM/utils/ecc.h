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

#ifndef ECC_H_
#define ECC_H_

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define RS_MAX_ROOTS 32

/* roots | byte errors | max payload
   2     | 1           | 253
   4     | 2           | 251
   ...
   32    | 16          | 223        */

int  ecc_rs_encoded_size(int payload_size, int nroots);
void ecc_rs_encode(uint8_t *data, int payload_len, int nroots);
int  ecc_rs_decode(uint8_t *data, int payload_len, int nroots);

#ifdef __cplusplus
}
#endif

#endif
