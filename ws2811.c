/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#include <math.h>
#include "ws2811.h"
#include "stm32f4xx_conf.h"
#include "ch.h"
#include "hal.h"

// Settings
#define TIM_PERIOD			(((168000000 / 2 / WS2811_CLK_HZ) - 1))
#define LED_BUFFER_LEN		(WS2811_LED_NUM + 1)
#define BITBUFFER_PAD		50
#define BITBUFFER_LEN		(24 * LED_BUFFER_LEN + BITBUFFER_PAD)
#define WS2811_ZERO			(TIM_PERIOD * 0.2)
#define WS2811_ONE			(TIM_PERIOD * 0.8)

// Private variables
static uint16_t bitbuffer[BITBUFFER_LEN];
static uint32_t RGBdata[LED_BUFFER_LEN];
static uint8_t gamma_table[256];
static uint32_t brightness;

// Private function prototypes
static uint32_t rgb_to_local(uint32_t color);

void ws2811_init(void) {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	DMA_InitTypeDef DMA_InitStructure;

	brightness = 100;

	// Default LED values
	int i, bit;

	for (i = 0;i < LED_BUFFER_LEN;i++) {
		RGBdata[i] = 0;
	}

	for (i = 0;i < LED_BUFFER_LEN;i++) {
		uint32_t tmp_color = rgb_to_local(RGBdata[i]);

		for (bit = 0;bit < 24;bit++) {
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
	for (i = 0;i < BITBUFFER_PAD;i++) {
		bitbuffer[BITBUFFER_LEN - BITBUFFER_PAD - 1 + i] = 0;
	}

	// Generate gamma correction table
	for (i = 0;i < 256;i++) {
		gamma_table[i] = (int)roundf(powf((float)i / 255.0, 1.0 / 0.45) * 255.0);
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
	if (led >= 0 && led < WS2811_LED_NUM) {
		RGBdata[led] = color;

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

uint32_t ws2811_get_led_color(int led) {
	if (led >= 0 && led < WS2811_LED_NUM) {
		return RGBdata[led];
	}

	return 0;
}

void ws2811_all_off(void) {
	int i;

	for (i = 0;i < WS2811_LED_NUM;i++) {
		RGBdata[i] = 0;
	}

	for (i = 0;i < (WS2811_LED_NUM * 24);i++) {
		bitbuffer[i] = WS2811_ZERO;
	}
}

void ws2811_set_all(uint32_t color) {
	int i, bit;

	for (i = 0;i < WS2811_LED_NUM;i++) {
		RGBdata[i] = color;

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

void ws2811_set_brightness(uint32_t br) {
	brightness = br;

	for (int i = 0;i < WS2811_LED_NUM;i++) {
		ws2811_set_led_color(i, ws2811_get_led_color(i));
	}
}

uint32_t ws2811_get_brightness(void) {
	return brightness;
}

static uint32_t rgb_to_local(uint32_t color) {
	uint32_t r = (color >> 16) & 0xFF;
	uint32_t g = (color >> 8) & 0xFF;
	uint32_t b = color & 0xFF;

	r = (r * brightness) / 100;
	g = (g * brightness) / 100;
	b = (b * brightness) / 100;

	r = gamma_table[r];
	g = gamma_table[g];
	b = gamma_table[b];

	return (g << 16) | (r << 8) | b;
}
