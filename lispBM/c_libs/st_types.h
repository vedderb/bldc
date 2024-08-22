/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

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

#ifndef ST_TYPES_H
#define ST_TYPES_H

#include <stdint.h>
#include <stdbool.h>
#include "system_stm32f4xx.h"

#ifdef USE_STLIB
#include "stm32f4xx_conf.h"
#endif

typedef struct {
	volatile uint32_t MODER;
	volatile uint32_t OTYPER;
	volatile uint32_t OSPEEDR;
	volatile uint32_t PUPDR;
	volatile uint32_t IDR;
	volatile uint32_t ODR;
	volatile union {
		uint32_t W;
		struct {
			uint16_t set;
			uint16_t clear;
		} H;
	} BSRR;
	volatile uint32_t LCKR;
	volatile uint32_t AFRL;
	volatile uint32_t AFRH;
	volatile uint32_t BRR;
} stm32_gpio_t;

#undef GPIOA
#undef GPIOB
#undef GPIOC
#undef GPIOD
#undef GPIOE
#undef GPIOF
#undef GPIOG
#undef GPIOH
#undef GPIOI

#define GPIOA                           ((stm32_gpio_t *)GPIOA_BASE)
#define GPIOB                           ((stm32_gpio_t *)GPIOB_BASE)
#define GPIOC                           ((stm32_gpio_t *)GPIOC_BASE)
#define GPIOD                           ((stm32_gpio_t *)GPIOD_BASE)
#define GPIOE                           ((stm32_gpio_t *)GPIOE_BASE)
#define GPIOF                           ((stm32_gpio_t *)GPIOF_BASE)
#define GPIOG                           ((stm32_gpio_t *)GPIOG_BASE)
#define GPIOH                           ((stm32_gpio_t *)GPIOH_BASE)
#define GPIOI                           ((stm32_gpio_t *)GPIOI_BASE)

#define PAL_STM32_MODE_MASK             (3U << 0U)
#define PAL_STM32_MODE_INPUT            (0U << 0U)
#define PAL_STM32_MODE_OUTPUT           (1U << 0U)
#define PAL_STM32_MODE_ALTERNATE        (2U << 0U)
#define PAL_STM32_MODE_ANALOG           (3U << 0U)

#define PAL_STM32_OTYPE_MASK            (1U << 2U)
#define PAL_STM32_OTYPE_PUSHPULL        (0U << 2U)
#define PAL_STM32_OTYPE_OPENDRAIN       (1U << 2U)

#define PAL_STM32_OSPEED_MASK           (3U << 3U)
#define PAL_STM32_OSPEED_LOWEST         (0U << 3U)
#if defined(STM32F0XX) || defined(STM32F30X) || defined(STM32F37X)
#define PAL_STM32_OSPEED_MID            (1U << 3U)
#else
#define PAL_STM32_OSPEED_MID1           (1U << 3U)
#define PAL_STM32_OSPEED_MID2           (2U << 3U)
#endif
#define PAL_STM32_OSPEED_HIGHEST        (3U << 3U)

#define PAL_STM32_PUDR_MASK             (3U << 5U)
#define PAL_STM32_PUDR_FLOATING         (0U << 5U)
#define PAL_STM32_PUDR_PULLUP           (1U << 5U)
#define PAL_STM32_PUDR_PULLDOWN         (2U << 5U)

#define PAL_STM32_ALTERNATE_MASK        (15U << 7U)
#define PAL_STM32_ALTERNATE(n)          ((n) << 7U)

#define PAL_STM32_MODE_MASK             (3U << 0U)
#define PAL_STM32_MODE_INPUT            (0U << 0U)
#define PAL_STM32_MODE_OUTPUT           (1U << 0U)
#define PAL_STM32_MODE_ALTERNATE        (2U << 0U)
#define PAL_STM32_MODE_ANALOG           (3U << 0U)

#define PAL_STM32_ALTERNATE(n)          ((n) << 7U)

#define PAL_MODE_ALTERNATE(n) (PAL_STM32_MODE_ALTERNATE | PAL_STM32_ALTERNATE(n))

#endif  // ST_TYPES_H

