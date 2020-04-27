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
 * @file    STM32F1xx/cmparams.h
 * @brief   ARM Cortex-M3 parameters for the STM32F1xx.
 *
 * @defgroup ARMCMx_STM32F1xx STM32F1xx Specific Parameters
 * @ingroup ARMCMx_SPECIFIC
 * @details This file contains the Cortex-M4 specific parameters for the
 *          STM32F1xx platform.
 * @{
 */

#ifndef _CMPARAMS_H_
#define _CMPARAMS_H_

/**
 * @brief   Cortex core model.
 */
#define CORTEX_MODEL            3

/**
 * @brief   Floating Point unit presence.
 */
#define CORTEX_HAS_FPU          0

/**
 * @brief   Number of bits in priority masks.
 */
#define CORTEX_PRIORITY_BITS    4

/* If the device type is not externally defined, for example from the Makefile,
   then a file named board.h is included. This file must contain a device
   definition compatible with the vendor include file.*/
#if !defined(STM32F10X_LD) && !defined(STM32F10X_LD_VL) &&                  \
    !defined(STM32F10X_MD) && !defined(STM32F10X_MD_VL) &&                  \
    !defined(STM32F10X_HD) && !defined(STM32F10X_HD_VL) &&                  \
    !defined(STM32F10X_XL) && !defined(STM32F10X_CL)
#include "board.h"
#endif

/* The following code is not processed when the file is included from an
   asm module.*/
#if !defined(_FROM_ASM_)

/* Including the device CMSIS header. Note, we are not using the definitions
   from this header because we need this file to be usable also from
   assembler source files. We verify that the info matches instead.*/
#include "stm32f10x.h"

#if CORTEX_MODEL != __CORTEX_M
#error "CMSIS __CORTEX_M mismatch"
#endif

#if CORTEX_PRIORITY_BITS != __NVIC_PRIO_BITS
#error "CMSIS __NVIC_PRIO_BITS mismatch"
#endif

#endif /* !defined(_FROM_ASM_) */

#if defined(STM32F10X_CL)
#define __NVECTORS              72
#elif defined(STM32F10X_XL)
#define __NVECTORS              64
#elif defined(STM32F10X_LD_VL) ||                                           \
      defined(STM32F10X_MD_VL) ||                                           \
      defined(STM32F10X_HD_VL)
#define __NVECTORS              64
#elif defined(STM32F10X_LD) ||                                              \
      defined(STM32F10X_MD) ||                                              \
      defined(STM32F10X_HD)
#define __NVECTORS              64
#else
#error "STM32F1xx device not defined or not recognized"
#endif

/**
 * @brief   Number of interrupt vectors.
 * @note    This number does not include the 16 system vectors and must be
 *          rounded to a multiple of 8.
 */
#define CORTEX_NUM_VECTORS      __NVECTORS

#endif /* _CMPARAMS_H_ */

/** @} */
