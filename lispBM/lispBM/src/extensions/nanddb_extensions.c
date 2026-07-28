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


#include "nanddb_extensions.h"


/* Database for NAND flash

   NAND flash properties:
   - Large erase size (Blocks).
   - Writes should be done in page-sized chunks (Pages).
   - Blocks and Pages are not the same size.
   - Expected to come with "bad blocks" from factory.
   - Blocks are expected to turn bad over the lifetime of the memory.
   - Depends on ECC in order to reach adverticed number
     of erase cycles.
   - Almost guaranteed to come with its own ECC implementation.

   Desired DB capabilites:
   - Store key-value pairs where key is a string and value is a bytearray.
   - Look up value based on key.
   - Store values that are larger than a page.
   

*/
void lbm_nanddb_extensions_init(lbm_nanddb_fal_t *fal) {

}
