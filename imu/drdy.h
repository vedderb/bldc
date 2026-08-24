/*
	Copyright 2026 Lukas Hrazky

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	The VESC firmware is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
	*/

#ifndef IMU_DRDY_H_
#define IMU_DRDY_H_

#include "ch.h"
#include "hal.h"

#include <stdint.h>
#include <stdbool.h>

// A data-ready (DRDY) EXTI source: an instance binds one GPIO pin whose rising edge signals a
// waiting thread. Instances are independent, so each IMU can have its own DRDY pin, within two
// constraints of the STM32 EXTI block: instances must use distinct pin numbers (the pin number
// selects the EXTI line regardless of port) and only pins 5-15 are supported (lines 0-4 have
// dedicated vectors which are not dispatched, see irq_handlers.c).
typedef struct {
	binary_semaphore_t sem;
	stm32_gpio_t *gpio;
	uint32_t pin;
	uint32_t exti_line;
	volatile uint32_t timestamp;
	volatile uint32_t int_count;
	volatile uint32_t timeout_count;
} drdy_t;

// Bind the instance to a pin and reset its state. Touches no hardware; must not be called
// while the instance is armed.
void drdy_bind(drdy_t *drdy, stm32_gpio_t *gpio, uint32_t pin);

// Arm the pin's EXTI line and register the instance with the ISR dispatch (the shared EXTI
// vectors are enabled centrally at boot).
void drdy_init(drdy_t *drdy);

// Mask the EXTI line again and unregister the instance, leaving the shared vector enabled.
void drdy_deinit(drdy_t *drdy);

// Block until the next data-ready edge or until timeout elapses. Returns true if an edge was
// signalled, false on timeout (counted).
bool drdy_wait(drdy_t *drdy, systime_t timeout);

// Release a waiter from thread context (e.g. to unblock the loop for shutdown).
void drdy_signal(drdy_t *drdy);

// Service pending EXTI lines of all armed instances. Called from the shared EXTI vectors in
// irq_handlers.c.
void drdy_exti_dispatch(void);

// TIM5 timestamp of the latest data-ready edge, captured in the ISR. Only meaningful after
// drdy_wait() returned true.
uint32_t drdy_timestamp(const drdy_t *drdy);

uint32_t drdy_interrupt_count(const drdy_t *drdy);
uint32_t drdy_timeout_count(const drdy_t *drdy);

#endif /* IMU_DRDY_H_ */
