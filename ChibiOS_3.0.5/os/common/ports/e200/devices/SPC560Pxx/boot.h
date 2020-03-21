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
 * @file    boot.h
 * @brief   Boot parameters for the SPC560Pxx.
 * @{
 */

#ifndef _BOOT_H_
#define _BOOT_H_

/*===========================================================================*/
/* Module constants.                                                         */
/*===========================================================================*/

/**
 * @name    BUCSR registers definitions
 * @{
 */
#define BUCSR_BPEN              0x00000001
#define BUCSR_BALLOC_BFI        0x00000200
/** @} */

/**
 * @name   MSR register definitions
 * @{
 */
#define MSR_WE                  0x00040000
#define MSR_CE                  0x00020000
#define MSR_EE                  0x00008000
#define MSR_PR                  0x00004000
#define MSR_ME                  0x00001000
#define MSR_DE                  0x00000200
#define MSR_IS                  0x00000020
#define MSR_DS                  0x00000010
#define MSR_RI                  0x00000002
/** @} */

/*===========================================================================*/
/* Module pre-compile time settings.                                         */
/*===========================================================================*/

/*
 * BUCSR default settings.
 */
#if !defined(BOOT_BUCSR_DEFAULT) || defined(__DOXYGEN__)
#define BOOT_BUCSR_DEFAULT      (BUCSR_BPEN | BUCSR_BALLOC_BFI)
#endif

/*
 * MSR default settings.
 */
#if !defined(BOOT_MSR_DEFAULT) || defined(__DOXYGEN__)
#define BOOT_MSR_DEFAULT        (MSR_WE | MSR_CE | MSR_ME)
#endif

/*
 * Boot default settings.
 */
#if !defined(BOOT_PERFORM_CORE_INIT) || defined(__DOXYGEN__)
#define BOOT_PERFORM_CORE_INIT  1
#endif

/*
 * VLE mode default settings.
 */
#if !defined(BOOT_USE_VLE) || defined(__DOXYGEN__)
#define BOOT_USE_VLE            1
#endif

/*
 * RAM relocation flag.
 */
#if !defined(BOOT_RELOCATE_IN_RAM) || defined(__DOXYGEN__)
#define BOOT_RELOCATE_IN_RAM    0
#endif

/*===========================================================================*/
/* Derived constants and error checks.                                       */
/*===========================================================================*/

/*===========================================================================*/
/* Module data structures and types.                                         */
/*===========================================================================*/

/*===========================================================================*/
/* Module macros.                                                            */
/*===========================================================================*/

/*===========================================================================*/
/* External declarations.                                                    */
/*===========================================================================*/

/*===========================================================================*/
/* Module inline functions.                                                  */
/*===========================================================================*/

#endif /* _BOOT_H_ */

/** @} */
