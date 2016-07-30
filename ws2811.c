/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

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

/*
 * ws2811.c
 *
 *  Created on: 14 jul 2013
 *      Author: benjamin
 */

#include <math.h>
#include "ws2811.h"
#include "stm32f4xx_conf.h"
#include "ch.h"
#include "hal.h"
#include "conf_general.h"

// Settings
#define TIM_PERIOD			(((168000000 / 2 / WS2811_CLK_HZ) - 1))
#define LED_BUFFER_LEN		(WS2811_LED_NUM + 1)
#define BITBUFFER_PAD		50
#define BITBUFFER_LEN		(24 * LED_BUFFER_LEN + BITBUFFER_PAD)
#define WS2811_ZERO			(TIM_PERIOD * 0.2)
#define WS2811_ONE			(TIM_PERIOD * 0.8)

// Gamma table.
static const uint8_t gamma_table[256] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01,
	0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x01, 0x02,
	0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x02, 0x03,
	0x00, 0x00, 0x00, 0x01, 0x01, 0x02, 0x03, 0x05,
	0x00, 0x00, 0x00, 0x01, 0x02, 0x03, 0x05, 0x07,
	0x00, 0x00, 0x01, 0x01, 0x03, 0x04, 0x06, 0x09,
	0x00, 0x00, 0x01, 0x02, 0x03, 0x05, 0x08, 0x0b,
	0x00, 0x00, 0x01, 0x02, 0x04, 0x07, 0x0a, 0x0e,
	0x00, 0x00, 0x01, 0x03, 0x05, 0x08, 0x0d, 0x12,
	0x00, 0x00, 0x01, 0x03, 0x06, 0x0a, 0x0f, 0x16,
	0x00, 0x00, 0x02, 0x04, 0x07, 0x0c, 0x12, 0x1a,
	0x00, 0x00, 0x02, 0x05, 0x09, 0x0e, 0x16, 0x1e,
	0x00, 0x00, 0x02, 0x05, 0x0a, 0x11, 0x19, 0x23,
	0x00, 0x01, 0x03, 0x06, 0x0c, 0x13, 0x1d, 0x29,
	0x00, 0x01, 0x03, 0x07, 0x0e, 0x16, 0x21, 0x2f,
	0x00, 0x01, 0x03, 0x08, 0x0f, 0x19, 0x26, 0x35,
	0x00, 0x01, 0x04, 0x09, 0x11, 0x1c, 0x2b, 0x3c,
	0x00, 0x01, 0x04, 0x0a, 0x13, 0x20, 0x30, 0x43,
	0x00, 0x01, 0x05, 0x0b, 0x16, 0x23, 0x35, 0x4b,
	0x00, 0x01, 0x05, 0x0d, 0x18, 0x27, 0x3b, 0x53,
	0x00, 0x01, 0x06, 0x0e, 0x1a, 0x2b, 0x41, 0x5c,
	0x00, 0x01, 0x06, 0x0f, 0x1d, 0x30, 0x48, 0x65,
	0x00, 0x01, 0x07, 0x11, 0x20, 0x34, 0x4e, 0x6e,
	0x00, 0x02, 0x07, 0x12, 0x23, 0x39, 0x56, 0x79,
	0x00, 0x02, 0x08, 0x14, 0x26, 0x3e, 0x5d, 0x83,
	0x00, 0x02, 0x09, 0x16, 0x29, 0x43, 0x65, 0x8e,
	0x00, 0x02, 0x09, 0x17, 0x2c, 0x49, 0x6d, 0x9a,
	0x00, 0x02, 0x0a, 0x19, 0x30, 0x4e, 0x76, 0xa6,
	0x00, 0x02, 0x0b, 0x1b, 0x33, 0x54, 0x7e, 0xb2,
};

// Private variables
static uint16_t bitbuffer[BITBUFFER_LEN];

// Private function prototypes
static uint32_t rgb_to_local(uint32_t color);

void ws2811_init(void) {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	DMA_InitTypeDef DMA_InitStructure;

	// Default LED values
	for (int i = 0;i < LED_BUFFER_LEN;i++) {
		uint32_t tmp_color = rgb_to_local(COLOR_BLACK);

		for (int bit = 0;bit < 24;bit++) {
			if(tmp_color & (1 << 23)) {
				bitbuffer[bit + i * 24] = WS2811_ONE;
			} else {
				bitbuffer[bit + i * 24] = WS2811_ZERO;
			}
			tmp_color <<= 1;
		}
	}

	// Fill the rest of the buffer with zeros to give the LEDs a chance to update
	// after sending all bits
	for (int i = 0;i < BITBUFFER_PAD;i++) {
		bitbuffer[BITBUFFER_LEN - BITBUFFER_PAD - 1 + i] = 0;
	}

#if WS2811_USE_CH2
	palSetPadMode(GPIOB, 7,
			PAL_MODE_ALTERNATE(GPIO_AF_TIM4) |
			PAL_STM32_OTYPE_OPENDRAIN |
			PAL_STM32_OSPEED_MID1);
#else
	palSetPadMode(GPIOB, 6,
			PAL_MODE_ALTERNATE(GPIO_AF_TIM4) |
			PAL_STM32_OTYPE_OPENDRAIN |
			PAL_STM32_OSPEED_MID1);
#endif

	// DMA clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_DMA1 , ENABLE);

#if WS2811_USE_CH2
	DMA_DeInit(DMA1_Stream3);
	DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&TIM4->CCR2;
#else
	DMA_DeInit(DMA1_Stream0);
	DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&TIM4->CCR1;
#endif
	DMA_InitStructure.DMA_Channel = DMA_Channel_2;
	DMA_InitStructure.DMA_Memory0BaseAddr = (uint32_t)bitbuffer;
	DMA_InitStructure.DMA_DIR = DMA_DIR_MemoryToPeripheral;
	DMA_InitStructure.DMA_BufferSize = BITBUFFER_LEN;
	DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
	DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
	DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;
	DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;
	DMA_InitStructure.DMA_Mode = DMA_Mode_Circular;
	DMA_InitStructure.DMA_Priority = DMA_Priority_High;
	DMA_InitStructure.DMA_FIFOMode = DMA_FIFOMode_Disable;
	DMA_InitStructure.DMA_FIFOThreshold = DMA_FIFOThreshold_Full;
	DMA_InitStructure.DMA_MemoryBurst = DMA_MemoryBurst_Single;
	DMA_InitStructure.DMA_PeripheralBurst = DMA_PeripheralBurst_Single;

#if WS2811_USE_CH2
	DMA_Init(DMA1_Stream3, &DMA_InitStructure);
#else
	DMA_Init(DMA1_Stream0, &DMA_InitStructure);
#endif

	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE);

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = TIM_PERIOD;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;

	TIM_TimeBaseInit(TIM4, &TIM_TimeBaseStructure);

	// Channel 1 Configuration in PWM mode
	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_Pulse = bitbuffer[0];
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;

#if WS2811_USE_CH2
	TIM_OC2Init(TIM4, &TIM_OCInitStructure);
	TIM_OC2PreloadConfig(TIM4, TIM_OCPreload_Enable);
#else
	TIM_OC1Init(TIM4, &TIM_OCInitStructure);
	TIM_OC1PreloadConfig(TIM4, TIM_OCPreload_Enable);
#endif

	// TIM4 counter enable
	TIM_Cmd(TIM4, ENABLE);

	// DMA enable
#if WS2811_USE_CH2
	DMA_Cmd(DMA1_Stream3, ENABLE);
#else
	DMA_Cmd(DMA1_Stream0, ENABLE);
#endif

	// TIM4 Update DMA Request enable
#if WS2811_USE_CH2
	TIM_DMACmd(TIM4, TIM_DMA_CC2, ENABLE);
#else
	TIM_DMACmd(TIM4, TIM_DMA_CC1, ENABLE);
#endif

	// Main Output Enable
	TIM_CtrlPWMOutputs(TIM4, ENABLE);
}

void ws2811_set_led_color(int led, uint32_t color) {
	if (led < WS2811_LED_NUM) {
		color = rgb_to_local(color);

		int bit;
		for (bit = 0;bit < 24;bit++) {
			if(color & (1 << 23)) {
				bitbuffer[bit + led * 24] = WS2811_ONE;
			} else {
				bitbuffer[bit + led * 24] = WS2811_ZERO;
			}
			color <<= 1;
		}
	}
}

void ws2811_all_off(void) {
	for (int i = 0;i < (WS2811_LED_NUM * 24);i++) {
		bitbuffer[i] = WS2811_ZERO;
	}
}

void ws2811_set_all(uint32_t color) {
	int i, bit;

	for (i = 0;i < WS2811_LED_NUM;i++) {
		uint32_t tmp_color = rgb_to_local(color);

		for (bit = 0;bit < 24;bit++) {
			if(tmp_color & (1 << 23)) {
				bitbuffer[bit + i * 24] = WS2811_ONE;
			} else {
				bitbuffer[bit + i * 24] = WS2811_ZERO;
			}
			tmp_color <<= 1;
		}
	}
}

static uint32_t rgb_to_local(uint32_t color) {
	uint32_t r = (color >> 16) & 0xFF;
	uint32_t g = (color >> 8) & 0xFF;
	uint32_t b = color & 0xFF;

	r = gamma_table[r];
	g = gamma_table[g];
	b = gamma_table[b];

	return (g << 16) | (r << 8) | b;
}
