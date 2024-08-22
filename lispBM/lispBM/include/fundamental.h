/*
    Copyright 2019 Joel Svensson        svenssonjoel@yahoo.se

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
/** \file fundamental.h
 *
 *  Implementation of the built in functions of the lispbm language (such as +, -, ... ).
 *
 */

#ifndef _FUNDAMENTAL_H_
#define _FUNDAMENTAL_H_

#include <eval_cps.h>

#ifdef __cplusplus
extern "C" {
#endif
  extern const fundamental_fun fundamental_table[];
  bool struct_eq(lbm_value a, lbm_value b);
#ifdef __cplusplus
}
#endif
#endif


