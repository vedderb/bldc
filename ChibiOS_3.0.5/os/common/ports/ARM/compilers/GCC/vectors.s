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
 * @file    ARM/compilers/GCC/vectors.s
 * @brief   Interrupt vectors for ARM devices.
 *
 * @defgroup ARM_VECTORS ARM Exception Vectors
 * @{
 */

#if defined(__DOXYGEN__)
/**
 * @brief   Unhandled exceptions handler.
 * @details Any undefined exception vector points to this function by default.
 *          This function simply stops the system into an infinite loop.
 * @note    The default implementation is a weak symbol, the application
 *          can override the default implementation.
 *
 * @notapi
 */
void _unhandled_exception(void) {}
#endif

#if !defined(__DOXYGEN__)

                .section .vectors, "ax"
                .code   32
                .balign 4

/*
 * System entry points.
 */
                .global _start
_start:
                ldr     pc, _reset
                ldr     pc, _undefined
                ldr     pc, _swi
                ldr     pc, _prefetch
                ldr     pc, _abort
                nop
                ldr     pc, _irq
                ldr     pc, _fiq

_reset:
                .word   Reset_Handler
_undefined:
                .word   Und_Handler
_swi:
                .word   Swi_Handler
_prefetch:
                .word   Prefetch_Handler
_abort:
                .word   Abort_Handler
_fiq:
                .word   Fiq_Handler
_irq:
                .word   Irq_Handler

/*
 * Default exceptions handlers. The handlers are declared weak in order to be
 * replaced by the real handling code. Everything is defaulted to an infinite
 * loop.
 */
                .weak   Reset_Handler
Reset_Handler:
                .weak   Und_Handler
Und_Handler:
                .weak   Swi_Handler
Swi_Handler:
                .weak   Prefetch_Handler
Prefetch_Handler:
                .weak   Abort_Handler
Abort_Handler:
                .weak   Fiq_Handler
Fiq_Handler:
                .weak   Irq_Handler
Irq_Handler:
                .weak   _unhandled_exception
_unhandled_exception:
                b       _unhandled_exception

#endif

/** @} */
