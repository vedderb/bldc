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

/**
 * @file    SPC56ECxx/ppcparams.h
 * @brief   PowerPC parameters for the SPC56ECxx.
 *
 * @defgroup PPC_SPC56ECxx SPC56ECxx Specific Parameters
 * @ingroup PPC_SPECIFIC
 * @details This file contains the PowerPC specific parameters for the
 *          SPC56ECxx platform.
 * @{
 */

#ifndef _PPCPARAMS_H_
#define _PPCPARAMS_H_

/**
 * @brief   Family identification macro.
 */
#define PPC_SPC56ECxx

/**
 * @brief   PPC core model.
 */
#define PPC_VARIANT                 PPC_VARIANT_e200z4

/**
 * @brief   Number of cores.
 */
#define PPC_CORE_NUMBER             1

/**
 * @brief   Number of writable bits in IVPR register.
 */
#define PPC_IVPR_BITS               16

/**
 * @brief   IVORx registers support.
 */
#define PPC_SUPPORTS_IVORS          TRUE

/**
 * @brief   Book E instruction set support.
 */
#define PPC_SUPPORTS_BOOKE          TRUE

/**
 * @brief   VLE instruction set support.
 */
#define PPC_SUPPORTS_VLE            TRUE

/**
 * @brief   Supports VLS Load/Store Multiple Volatile instructions.
 */
#define PPC_SUPPORTS_VLE_MULTI      TRUE

/**
 * @brief   Supports the decrementer timer.
 */
#define PPC_SUPPORTS_DECREMENTER    TRUE

/**
 * @brief   Number of interrupt sources.
 */
#define PPC_NUM_VECTORS             279

#endif /* _PPCPARAMS_H_ */

/** @} */
