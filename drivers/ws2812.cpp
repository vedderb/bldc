/*
 * Copyright 2016 Andrew Rossignol andrew.rossignol@gmail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "drivers/ws2812.h"

#include <ch.h>
#include <hal.h>
#include <string.h>

#include "conf_general.h"
#include "stm32f4xx_conf.h"

/*
 * The driver implementation is based heavily upon Benjamin Vedder's work.
 * The primary difference is that the DMA transfer is modified to not use
 * cyclic mode and a new SendFrame method has been introduced. The RESET
 * command has been moved to the beginning of the bit buffer and LED data
 * is only ever present on the data line when a frame is requested to be sent.
 * In theory, this also saves power but with a ~3kW load right next door, it
 * hardly matters.
 *
 * This change prevents race conditions when writing the bit buffer to the LEDs
 * and avoids flickering in the output.
 *
 * TODO(aarossig): Observe the WS2812 waveform used to drive the LEDs with an
 * oscilloscope to verify. There is still occasional flicker, but it seems to
 * be a function of motor current. This may be improved by decreasing the
 * pull-up resistor on the open-drain output and verifying the 0 and 1 bit
 * pattern.
 */

namespace drivers {
namespace ws2812 {

#define LED_BUFFER_LENGTH     (WS2811_LED_NUM + 1)
#define BIT_BUFFER_PAD_LENGTH 50
#define BIT_BUFFER_LENGTH     ((24 * LED_BUFFER_LENGTH) \
                                  + (2 * BIT_BUFFER_PAD_LENGTH))

#define WS2812_CLOCK_HZ       800000
#define TIMER_PERIOD          ((SYSTEM_CORE_CLOCK / 2 / WS2812_CLOCK_HZ))
#define WS2812_ZERO           (TIMER_PERIOD * 0.35)
#define WS2812_ONE            (TIMER_PERIOD * 0.90)

const Color black = { .blue =   0, .red =   0, .green =   0, };
const Color white = { .blue = 255, .red = 255, .green = 255, };

/*
 * Pack a color into a uint32_t so that it can be sent to the LEDs.
 */
union PackedColor {
  Color color;
  uint32_t value;
};

static uint16_t bit_buffer[BIT_BUFFER_LENGTH];

void InterpolateColor(struct Color *dest_color,
    const struct Color& source_color_a,
    const struct Color& source_color_b,
    float ratio) {
  dest_color->red = (source_color_a.red * (1.0f - ratio))
      + (source_color_b.red * ratio);
  dest_color->green = (source_color_a.green * (1.0f - ratio))
      + (source_color_b.green * ratio);
  dest_color->blue = (source_color_a.blue * (1.0f - ratio))
      + (source_color_b.blue * ratio);
}

void Init() {
  // The first BIT_BUFFER_PAD_PENGTH bits are set to zero. This sends the
  // reset command to the WS2812 LEDs before sending the color values.
  memset(&bit_buffer[0], 0, BIT_BUFFER_PAD_LENGTH);
  memset(&bit_buffer[BIT_BUFFER_LENGTH - BIT_BUFFER_PAD_LENGTH], 0,
      BIT_BUFFER_PAD_LENGTH);

  // Configure GPIO pins.
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

  // DMA clock enable.
  RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_DMA1 , ENABLE);

  // Timer clock enable.
  RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE);
}

void SendFrame(const ws2812::Color *color_buffer, size_t length) {
  if (length > WS2811_LED_NUM) {
    return;
  }

  for (size_t i = 0; i < length; i++) {
    PackedColor packed_color = {
      .color = color_buffer[i],
    };

    uint32_t color = packed_color.value;

    for (size_t j = 0; j < 24; j++) {
      if(color & (1 << 23)) {
        bit_buffer[BIT_BUFFER_PAD_LENGTH + (j + i * 24)] = WS2812_ONE;
      } else {
        bit_buffer[BIT_BUFFER_PAD_LENGTH + (j + i * 24)] = WS2812_ZERO;
      }
      color <<= 1;
    }
  }

  TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
  TIM_OCInitTypeDef  TIM_OCInitStructure;
  DMA_InitTypeDef DMA_InitStructure;

#if WS2811_USE_CH2
  DMA_DeInit(DMA1_Stream3);
  DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&TIM4->CCR2;
#else
  DMA_DeInit(DMA1_Stream0);
  DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&TIM4->CCR1;
#endif
  DMA_InitStructure.DMA_Channel = DMA_Channel_2;
  DMA_InitStructure.DMA_Memory0BaseAddr = (uint32_t)bit_buffer;
  DMA_InitStructure.DMA_DIR = DMA_DIR_MemoryToPeripheral;
  DMA_InitStructure.DMA_BufferSize = BIT_BUFFER_LENGTH;
  DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
  DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
  DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;
  DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;
  DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;
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

  // Time Base configuration
  TIM_TimeBaseStructure.TIM_Prescaler = 0;
  TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
  TIM_TimeBaseStructure.TIM_Period = TIMER_PERIOD;
  TIM_TimeBaseStructure.TIM_ClockDivision = 0;
  TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;

  TIM_TimeBaseInit(TIM4, &TIM_TimeBaseStructure);

  // Channel 1 Configuration in PWM mode
  TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
  TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
  TIM_OCInitStructure.TIM_Pulse = bit_buffer[0];
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

}  // namespace ws2812
}  // namespace drivers
