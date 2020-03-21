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
 * @file    armparams.h
 * @brief   ARM parameters for the LPC214x.
 *
 * @defgroup ARM_LPC214x LPC214x Specific Parameters
 * @ingroup ARM_SPECIFIC
 * @details This file contains the ARM specific parameters for the
 *          LPC214x platform.
 * @{
 */

#ifndef _ARMPARAMS_H_
#define _ARMPARAMS_H_

/**
 * @brief   ARM core model.
 */
#define ARM_CORE                ARM_CORE_ARM7TDMI

/**
 * @brief   Thumb-capable.
 */
#define ARM_SUPPORTS_THUMB      1

/**
 * @brief   Thumb2-capable.
 */
#define ARM_SUPPORTS_THUMB2     0

/**
 * @brief   Implementation of the wait-for-interrupt state enter.
 */
#define ARM_WFI_IMPL            (PCON = 1)

#if !defined(_FROM_ASM_) || defined(__DOXYGEN__)
/**
 * @brief   Address of the IRQ vector register in the interrupt controller.
 */
#define ARM_IRQ_VECTOR_REG      0xFFFFF030U
#else
#define ARM_IRQ_VECTOR_REG      0xFFFFF030
#endif

#endif /* _ARMPARAMS_H_ */

/** @} */
