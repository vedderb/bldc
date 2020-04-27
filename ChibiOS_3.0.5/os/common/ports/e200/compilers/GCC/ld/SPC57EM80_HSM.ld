/*
    ChibiOS - Copyright (C) 2006..2015 Giovanni Di Sirio.

    This file is part of ChibiOS.

    ChibiOS is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    ChibiOS is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
 * SPC57EM80-HSM memory setup.
 */
MEMORY
{
    flash     : org = 0x0060C000, len = 144k
    dflash0   : org = 0x00680000, len = 16k
    dflash1   : org = 0x00684000, len = 16k
    ram       : org = 0xA0000000, len = 40k
}

INCLUDE rules_z0.ld
