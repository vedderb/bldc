/*
    Copyright 2023 Joel Svensson    svenssonjoel@yahoo.se

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

#ifndef LBM_FLAGS_H_
#define LBM_FLAGS_H_

#include <stdint.h>

#define LBM_FLAG_ATOMIC_MALFUNCTION              (1 << 0)
#define LBM_FLAG_HANDLER_EVENT_DELIVERY_FAILED   (1 << 1)
#define LBM_FLAG_UNFLATTENING_FAILED             (1 << 2)
#define LBM_FLAG_BLOCKED_NOT_FOUND               (1 << 3)

#ifdef __cplusplus
extern "C" {
#endif
  
extern uint32_t lbm_get_flags(void); 
extern void lbm_set_flags(uint32_t flags);  
extern void lbm_clr_flags(uint32_t flags);

#ifdef __cplusplus
}
#endif 

#endif
