/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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
#include "hw.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "drv8323s.h"

// Variables
static volatile bool i2c_running = false;

// I2C configuration
static const I2CConfig i2cfg = {
                                OPMODE_I2C,
                                100000,
                                STD_DUTY_CYCLE
};

void hw_init_gpio(void) {
  // GPIO clock enable
  RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
  RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
  RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);
  RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);
  RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOE, ENABLE);




  // LEDs
  palSetPadMode(GPIOA, 8,
                PAL_MODE_OUTPUT_PUSHPULL |
                PAL_STM32_OSPEED_HIGHEST);
  palSetPadMode(GPIOC, 9,
                PAL_MODE_OUTPUT_PUSHPULL |
                PAL_STM32_OSPEED_HIGHEST);

  //Temp switches
  palSetPadMode(ADC_SW_EN_PORT, ADC_SW_EN_PIN,
                PAL_MODE_OUTPUT_PUSHPULL |
                PAL_STM32_OSPEED_HIGHEST);
  palSetPadMode(ADC_SW_1_PORT, ADC_SW_1_PIN ,
                PAL_MODE_OUTPUT_PUSHPULL |
                PAL_STM32_OSPEED_HIGHEST);
  palSetPadMode(ADC_SW_2_PORT, ADC_SW_2_PIN,
                PAL_MODE_OUTPUT_PUSHPULL |
                PAL_STM32_OSPEED_HIGHEST);
  palSetPadMode(ADC_SW_3_PORT, ADC_SW_3_PIN ,
                PAL_MODE_OUTPUT_PUSHPULL |
                PAL_STM32_OSPEED_HIGHEST);


  ENABLE_MOS_TEMP1();

  // GPIOC (ENABLE_GATE)
  palSetPadMode(GPIOE, 14,
                PAL_MODE_OUTPUT_PUSHPULL |
                PAL_STM32_OSPEED_HIGHEST);
  palSetPadMode(GPIOD, 4,
                PAL_MODE_OUTPUT_PUSHPULL |
                PAL_STM32_OSPEED_HIGHEST);
  DISABLE_GATE();
  DISABLE_GATE2();
  // GPIOB (DCCAL)


  // GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
  palSetPadMode(GPIOE, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);
  palSetPadMode(GPIOE, 9, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);
  palSetPadMode(GPIOE, 10, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);

  palSetPadMode(GPIOE, 11, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);
  palSetPadMode(GPIOE, 12, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);
  palSetPadMode(GPIOE, 13, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);

  palSetPadMode(GPIOC, 6, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);
  palSetPadMode(GPIOC, 7, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);
  palSetPadMode(GPIOC, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);

  palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);
  palSetPadMode(GPIOB, 15, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);
  palSetPadMode(GPIOA, 7, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
                PAL_STM32_OSPEED_HIGHEST |
                PAL_STM32_PUDR_FLOATING);

  // Hall sensors
  palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_INPUT_PULLUP);
  palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_INPUT_PULLUP);
  palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_INPUT_PULLUP);
  palSetPadMode(HW_HALL_ENC_GPIO4, HW_HALL_ENC_PIN4, PAL_MODE_INPUT_PULLUP);
  palSetPadMode(HW_HALL_ENC_GPIO5, HW_HALL_ENC_PIN5, PAL_MODE_INPUT_PULLUP);
  palSetPadMode(HW_HALL_ENC_GPIO6, HW_HALL_ENC_PIN6, PAL_MODE_INPUT_PULLUP);

  // Fault pin
  palSetPadMode(GPIOE, 3, PAL_MODE_INPUT_PULLUP);
  palSetPadMode(GPIOD, 3, PAL_MODE_INPUT_PULLUP);

  // ADC Pins
  palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOA, 4, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOB, 0, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOB, 1, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);
  palSetPadMode(GPIOC, 5, PAL_MODE_INPUT_ANALOG);
  ENABLE_GATE()
  ENABLE_GATE2()

  drv8323s_init();
}

void hw_setup_adc_channels(void) {

  // ADC1 regular channels
  ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 1, ADC_SampleTime_15Cycles); //0
  ADC_RegularChannelConfig(ADC1, ADC_Channel_9, 2, ADC_SampleTime_15Cycles);//3
  ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 3, ADC_SampleTime_15Cycles);//6
  ADC_RegularChannelConfig(ADC1, ADC_Channel_5 , 4, ADC_SampleTime_15Cycles);//9
  ADC_RegularChannelConfig(ADC1, ADC_Channel_4, 5, ADC_SampleTime_15Cycles);//12

  // ADC2 regular channels
  ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 1, ADC_SampleTime_15Cycles);//1
  ADC_RegularChannelConfig(ADC2, ADC_Channel_8, 2, ADC_SampleTime_15Cycles);//4
  ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 3, ADC_SampleTime_15Cycles);//7
  ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 4, ADC_SampleTime_15Cycles);//10
  ADC_RegularChannelConfig(ADC2, ADC_Channel_12, 5, ADC_SampleTime_15Cycles);//13

  // ADC3 regular channels
  ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 1, ADC_SampleTime_15Cycles);//2
  ADC_RegularChannelConfig(ADC3, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);//5
  ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 3, ADC_SampleTime_15Cycles);//8
  ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);//11
  ADC_RegularChannelConfig(ADC3, ADC_Channel_11, 5, ADC_SampleTime_15Cycles);//14

  // Injected channels
  ADC_InjectedChannelConfig(ADC1, ADC_Channel_9, 1, ADC_SampleTime_15Cycles);
  ADC_InjectedChannelConfig(ADC1, ADC_Channel_8, 2, ADC_SampleTime_15Cycles);


  ADC_InjectedChannelConfig(ADC2, ADC_Channel_5, 1, ADC_SampleTime_15Cycles);
  ADC_InjectedChannelConfig(ADC2, ADC_Channel_4, 2, ADC_SampleTime_15Cycles);

  //  ADC_InjectedChannelConfig(ADC3, ADC_Channel_2, 1, ADC_SampleTime_15Cycles);
  // ADC_InjectedChannelConfig(ADC3, ADC_Channel_0, 2, ADC_SampleTime_15Cycles);
  //ADC_InjectedChannelConfig(ADC3, ADC_Channel_1, 3, ADC_SampleTime_15Cycles);
}

void hw_start_i2c(void) {
  i2cAcquireBus(&HW_I2C_DEV);

  if (!i2c_running) {
    palSetPadMode(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN,
                  PAL_MODE_ALTERNATE(HW_I2C_GPIO_AF) |
                  PAL_STM32_OTYPE_OPENDRAIN |
                  PAL_STM32_OSPEED_MID1 |
                  PAL_STM32_PUDR_PULLUP);
    palSetPadMode(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
                  PAL_MODE_ALTERNATE(HW_I2C_GPIO_AF) |
                  PAL_STM32_OTYPE_OPENDRAIN |
                  PAL_STM32_OSPEED_MID1 |
                  PAL_STM32_PUDR_PULLUP);

    i2cStart(&HW_I2C_DEV, &i2cfg);
    i2c_running = true;
  }

  i2cReleaseBus(&HW_I2C_DEV);
}

void hw_stop_i2c(void) {
  i2cAcquireBus(&HW_I2C_DEV);

  if (i2c_running) {
    palSetPadMode(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN, PAL_MODE_INPUT);
    palSetPadMode(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN, PAL_MODE_INPUT);

    i2cStop(&HW_I2C_DEV);
    i2c_running = false;

  }

  i2cReleaseBus(&HW_I2C_DEV);
}

/**
 * Try to restore the i2c bus
 */
void hw_try_restore_i2c(void) {
  if (i2c_running) {
    i2cAcquireBus(&HW_I2C_DEV);

    palSetPadMode(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN,
                  PAL_STM32_OTYPE_OPENDRAIN |
                  PAL_STM32_OSPEED_MID1 |
                  PAL_STM32_PUDR_PULLUP);

    palSetPadMode(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
                  PAL_STM32_OTYPE_OPENDRAIN |
                  PAL_STM32_OSPEED_MID1 |
                  PAL_STM32_PUDR_PULLUP);

    palSetPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
    palSetPad(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN);

    chThdSleep(1);

    for(int i = 0;i < 16;i++) {
      palClearPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
      chThdSleep(1);
      palSetPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
      chThdSleep(1);
    }

    // Generate start then stop condition
    palClearPad(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN);
    chThdSleep(1);
    palClearPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
    chThdSleep(1);
    palSetPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
    chThdSleep(1);
    palSetPad(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN);

    palSetPadMode(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN,
                  PAL_MODE_ALTERNATE(HW_I2C_GPIO_AF) |
                  PAL_STM32_OTYPE_OPENDRAIN |
                  PAL_STM32_OSPEED_MID1 |
                  PAL_STM32_PUDR_PULLUP);

    palSetPadMode(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
                  PAL_MODE_ALTERNATE(HW_I2C_GPIO_AF) |
                  PAL_STM32_OTYPE_OPENDRAIN |
                  PAL_STM32_OSPEED_MID1 |
                  PAL_STM32_PUDR_PULLUP);

    HW_I2C_DEV.state = I2C_STOP;
    i2cStart(&HW_I2C_DEV, &i2cfg);

    i2cReleaseBus(&HW_I2C_DEV);
  }
}
