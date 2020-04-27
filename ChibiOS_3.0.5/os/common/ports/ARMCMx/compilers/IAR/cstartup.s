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
 * @file    ARMCMx/IAR/cstartup.s
 * @brief   Generic IAR Cortex-Mx startup file.
 *
 * @addtogroup ARMCMx_IAR_STARTUP
 * @{
 */

#if !defined(__DOXYGEN__)

        MODULE  ?cstartup

CONTROL_MODE_PRIVILEGED SET 0
CONTROL_MODE_UNPRIVILEGED SET 1
CONTROL_USE_MSP SET 0
CONTROL_USE_PSP SET 2

        AAPCS INTERWORK, VFP_COMPATIBLE, ROPI
        PRESERVE8

        SECTION .intvec:CODE:NOROOT(3)

        SECTION CSTACK:DATA:NOROOT(3)
        PUBLIC  __main_thread_stack_base__
__main_thread_stack_base__:
        PUBLIC  __heap_end__
__heap_end__:

        SECTION SYSHEAP:DATA:NOROOT(3)
        PUBLIC  __heap_base__
__heap_base__:

        PUBLIC  __iar_program_start
        EXTERN  __vector_table
        EXTWEAK __iar_init_core
        EXTWEAK __iar_init_vfp
        EXTERN  __cmain

        SECTION .text:CODE:REORDER(2)
        REQUIRE __vector_table
        THUMB
__iar_program_start:
        cpsid   i
        ldr     r0, =SFE(CSTACK)
        msr     PSP, r0
        movs    r0, #CONTROL_MODE_PRIVILEGED | CONTROL_USE_PSP
        msr     CONTROL, r0
        isb
        bl      __early_init
        bl      __iar_init_core
        bl      __iar_init_vfp
        b       __cmain

        SECTION .text:CODE:NOROOT:REORDER(2)
        PUBWEAK __early_init
__early_init:
        bx      lr

        END

#endif /* !defined(__DOXYGEN__) */

/**< @} */
