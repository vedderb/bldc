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

#include "st_types.h"
#include "vesc_c_if.h"

#include <math.h>

HEADER

// Settings
#define WS2812_CLK_HZ		800000
#define TIM_PERIOD			(((168000000 / 2 / WS2812_CLK_HZ) - 1))
#define WS2812_ZERO			(TIM_PERIOD * 0.2)
#define WS2812_ONE			(TIM_PERIOD * 0.8)
#define BITBUFFER_PAD		50

typedef struct {
	bool is_tim4;
	bool is_ch2;
	int bits;
	int num_leds;
	int ledbuf_len;
	int bitbuf_len;
	uint16_t *bitbuffer;
	uint32_t *RGBdata;
	uint32_t brightness;
} ws_cfg;

static uint32_t rgb_to_local(ws_cfg *cfg, uint32_t color) {
	uint32_t w = (color >> 24) & 0xFF;
	uint32_t r = (color >> 16) & 0xFF;
	uint32_t g = (color >> 8) & 0xFF;
	uint32_t b = color & 0xFF;

	r = (r * cfg->brightness) / 100;
	g = (g * cfg->brightness) / 100;
	b = (b * cfg->brightness) / 100;
	w = (w * cfg->brightness) / 100;

	//r = gamma_table[r];
	//g = gamma_table[g];
	//b = gamma_table[b];
	//w = gamma_table[w];
	
	if (cfg->bits == 32) {
		return (g << 24) | (r << 16) | (b << 8) | w;
	} else {
		return (g << 16) | (r << 8) | b;
	}
}

static void ws2812_init(ws_cfg *cfg) {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	DMA_InitTypeDef DMA_InitStructure;

	// Default LED values
	int i, bit;

	for (i = 0;i < cfg->ledbuf_len;i++) {
		cfg->RGBdata[i] = 0;
	}

	for (i = 0;i < cfg->ledbuf_len;i++) {
		uint32_t tmp_color = rgb_to_local(cfg, cfg->RGBdata[i]);

		for (bit = 0;bit < cfg->bits;bit++) {
			if (tmp_color & (1 << (cfg->bits - 1))) {
				cfg->bitbuffer[bit + i * cfg->bits] = WS2812_ONE;
			} else {
				cfg->bitbuffer[bit + i * cfg->bits] = WS2812_ZERO;
			}
			tmp_color <<= 1;
		}
	}

	// Fill the rest of the buffer with zeros to give the LEDs a chance to update
	// after sending all bits
	for (i = 0;i < BITBUFFER_PAD;i++) {
		cfg->bitbuffer[cfg->bitbuf_len - BITBUFFER_PAD - 1 + i] = 0;
	}

	// Generate gamma correction table
	//for (i = 0;i < 256;i++) {
	//	gamma_table[i] = (int)roundf(powf((float)i / 255.0, 1.0 / 0.45) * 255.0);
	//}
	
	TIM_TypeDef *tim;
	DMA_Stream_TypeDef *dma_stream;
	uint32_t dma_ch;
	
	if (cfg->is_tim4) {
		tim = TIM4;
		if (cfg->is_ch2) {
			dma_stream = DMA1_Stream3;
			dma_ch = DMA_Channel_2;
			VESC_IF->set_pad_mode(GPIOB, 7,
				PAL_MODE_ALTERNATE(2) |
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1);
		} else {
			dma_stream = DMA1_Stream0;
			dma_ch = DMA_Channel_2;
			VESC_IF->set_pad_mode(GPIOB, 6,
				PAL_MODE_ALTERNATE(2) |
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1);
		}
	} else {
		tim = TIM3;
		if (cfg->is_ch2) {
			dma_stream = DMA1_Stream5;
			dma_ch = DMA_Channel_5;
			VESC_IF->set_pad_mode(GPIOC, 7,
				PAL_MODE_ALTERNATE(2) |
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1);
		} else {
			dma_stream = DMA1_Stream4;
			dma_ch = DMA_Channel_5;
			VESC_IF->set_pad_mode(GPIOC, 6,
				PAL_MODE_ALTERNATE(2) |
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1);
		}
	}
	
	TIM_DeInit(tim);

	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_DMA1 , ENABLE);
	DMA_DeInit(dma_stream);
	
	if (cfg->is_ch2) {
		DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&tim->CCR2;
	} else {
		DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&tim->CCR1;
	}
	
	DMA_InitStructure.DMA_Channel = dma_ch;
	DMA_InitStructure.DMA_Memory0BaseAddr = (uint32_t)(cfg->bitbuffer);
	DMA_InitStructure.DMA_DIR = DMA_DIR_MemoryToPeripheral;
	DMA_InitStructure.DMA_BufferSize = cfg->bitbuf_len;
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

	DMA_Init(dma_stream, &DMA_InitStructure);
	
	if (cfg->is_tim4) {
		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE);
	} else {
		RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE);
	}

	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = TIM_PERIOD;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;

	TIM_TimeBaseInit(tim, &TIM_TimeBaseStructure);

	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_Pulse = cfg->bitbuffer[0];
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;

	if (cfg->is_ch2) {
		TIM_OC2Init(tim, &TIM_OCInitStructure);
		TIM_OC2PreloadConfig(tim, TIM_OCPreload_Enable);
	} else {
		TIM_OC1Init(tim, &TIM_OCInitStructure);
		TIM_OC1PreloadConfig(tim, TIM_OCPreload_Enable);
	}
	
	TIM_ARRPreloadConfig(tim, ENABLE);

	TIM_Cmd(tim, ENABLE);

	DMA_Cmd(dma_stream, ENABLE);

	if (cfg->is_ch2) {
		TIM_DMACmd(tim, TIM_DMA_CC2, ENABLE);
	} else {
		TIM_DMACmd(tim, TIM_DMA_CC1, ENABLE);
	}
}

static void ws2812_set_color(ws_cfg *cfg, int led, uint32_t color) {
	if (led >= 0 && led < cfg->num_leds) {
		cfg->RGBdata[led] = color;

		color = rgb_to_local(cfg, color);

		int bit;
		for (bit = 0;bit < cfg->bits;bit++) {
			if (color & (1 << (cfg->bits - 1))) {
				cfg->bitbuffer[bit + led * cfg->bits] = WS2812_ONE;
			} else {
				cfg->bitbuffer[bit + led * cfg->bits] = WS2812_ZERO;
			}
			color <<= 1;
		}
	}
}

static lbm_value ext_init(lbm_value *args, lbm_uint argn) {
	if (argn != 4 || !VESC_IF->lbm_is_number(args[0]) || !VESC_IF->lbm_is_number(args[1]) ||
		!VESC_IF->lbm_is_number(args[2]) || !VESC_IF->lbm_is_number(args[3])) {
		return VESC_IF->lbm_enc_sym_eerror;
	}
	
	if (ARG) {
		VESC_IF->lbm_set_error_reason("Already Initialized");
		return VESC_IF->lbm_enc_sym_eerror;
	}
	
	ws_cfg *cfg = VESC_IF->malloc(sizeof(ws_cfg));
	
	bool ok = false;
	
	if (cfg) {
		cfg->num_leds = VESC_IF->lbm_dec_as_i32(args[0]);
		cfg->is_ch2 = VESC_IF->lbm_dec_as_i32(args[1]);
		cfg->is_tim4 = VESC_IF->lbm_dec_as_i32(args[2]);
		cfg->bits = VESC_IF->lbm_dec_as_i32(args[3]) == 0 ? 24 : 32;
		cfg->ledbuf_len = cfg->num_leds + 1;
		cfg->bitbuf_len = cfg->bits * cfg->ledbuf_len + BITBUFFER_PAD;
		cfg->bitbuffer = VESC_IF->malloc(sizeof(uint16_t) * cfg->bitbuf_len);
		cfg->RGBdata = VESC_IF->malloc(sizeof(uint32_t) * cfg->ledbuf_len);
		cfg->brightness = 100;
		
		ok = cfg->bitbuffer != NULL && cfg->RGBdata != NULL;
	}
	
	if (!ok) {
		if (cfg) {
			if (cfg->bitbuffer) {
				VESC_IF->free(cfg->bitbuffer);
			}
			if (cfg->RGBdata) {
				VESC_IF->free(cfg->RGBdata);
			}
			VESC_IF->free(cfg);
		}
	
		VESC_IF->lbm_set_error_reason("Not enough memory");
		return VESC_IF->lbm_enc_sym_eerror;
	}
	
	ws2812_init(cfg);
	
	ARG = cfg;
	
	return VESC_IF->lbm_enc_sym_true;
}

static lbm_value ext_set_brightness(lbm_value *args, lbm_uint argn) {
	if (argn != 1 || !VESC_IF->lbm_is_number(args[0])) {
		return VESC_IF->lbm_enc_sym_eerror;
	}
	
	ws_cfg *cfg = (ws_cfg*)ARG;
	if (!cfg) {
		VESC_IF->lbm_set_error_reason("Not Initialized");
		return VESC_IF->lbm_enc_sym_eerror;
	}
	
	cfg->brightness = VESC_IF->lbm_dec_as_u32(args[0]);

	for (int i = 0;i < cfg->num_leds;i++) {
		ws2812_set_color(cfg, i, cfg->RGBdata[i]);
	}
	
	return VESC_IF->lbm_enc_sym_true;
}

static lbm_value ext_set_color(lbm_value *args, lbm_uint argn) {
	if (argn != 2 || !VESC_IF->lbm_is_number(args[0]) || !VESC_IF->lbm_is_number(args[1])) {
		return VESC_IF->lbm_enc_sym_eerror;
	}
	
	ws_cfg *cfg = (ws_cfg*)ARG;
	if (!cfg) {
		VESC_IF->lbm_set_error_reason("Not Initialized");
		return VESC_IF->lbm_enc_sym_eerror;
	}
	
	int led = VESC_IF->lbm_dec_as_i32(args[0]);
	uint32_t color = VESC_IF->lbm_dec_as_u32(args[1]);

	ws2812_set_color(cfg, led, color);
	
	return VESC_IF->lbm_enc_sym_true;
}

static void stop(void *arg) {
	if (arg) {
		ws_cfg *cfg = (ws_cfg*)ARG;
		
		TIM_TypeDef *tim;
		DMA_Stream_TypeDef *dma_stream;
		
		if (cfg->is_tim4) {
			tim = TIM4;
			if (cfg->is_ch2) {
				dma_stream = DMA1_Stream3;
			} else {
				dma_stream = DMA1_Stream0;
			}
		} else {
			tim = TIM3;
			if (cfg->is_ch2) {
				dma_stream = DMA1_Stream5;
			} else {
				dma_stream = DMA1_Stream4;
			}
		}
		
		TIM_DeInit(tim);
		DMA_DeInit(dma_stream);
		
		VESC_IF->free(cfg->bitbuffer);
		VESC_IF->free(cfg->RGBdata);
		VESC_IF->free(cfg);
	}
}

INIT_FUN(lib_info *info) {
	INIT_START

	VESC_IF->lbm_add_extension("ext-ws2812-init", ext_init);
	VESC_IF->lbm_add_extension("ext-ws2812-set-brightness", ext_set_brightness);
	VESC_IF->lbm_add_extension("ext-ws2812-set-color", ext_set_color);
	
	info->arg = 0;
	info->stop_fun = stop;
	
	return true;
}

