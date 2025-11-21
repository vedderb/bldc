/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

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

#include "platform_timestamp.h"
#include <ctype.h>
#include <ch.h>
#include <hal.h>
#include <chvt.h>
#include <chtime.h>

uint32_t lbm_timestamp(void) {
  systime_t t = chVTGetSystemTime();
  uint32_t ts = (uint32_t) ((1000000 / CH_CFG_ST_FREQUENCY) * t);
  return ts;
}
