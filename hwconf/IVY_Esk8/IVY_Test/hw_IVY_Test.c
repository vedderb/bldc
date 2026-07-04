/*
	Copyright 2018 Benjamin Vedder	benjamin@vedder.se
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

// V2

#include "hw.h"
#include "hw_IVY_Test.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils.h"
#include <math.h>
#include "mc_interface.h"

// Variables
static volatile bool i2c_running = false;
static mutex_t shutdown_mutex;
static float bt_diff = 0.0;

// I2C configuration
static const I2CConfig i2cfg = {
	OPMODE_I2C,
	100000,
	STD_DUTY_CYCLE};

void hw_init_gpio(void)
{
	chMtxObjectInit(&shutdown_mutex);
	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);

	// LEDs
	palSetPadMode(LED_GREEN_GPIO, LED_GREEN_PIN,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(LED_RED_GPIO, LED_RED_PIN,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST);

	// GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
	palSetPadMode(GPIOA, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 9, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 10, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOB, 13, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 15, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);

	// Hall sensors
	palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_INPUT_PULLUP);

#ifdef HW_HAS_PHASE_FILTERS
	// Phase filters
	palSetPadMode(PHASE_FILTER_GPIO, PHASE_FILTER_PIN,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST);
	PHASE_FILTER_OFF();
#endif

#ifdef HW_HAS_CURR_FILTERS
	// Current filter
	palSetPadMode(GPIOD, 2,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST);

	CURRENT_FILTER_OFF();
#endif

	// AUX pin
	// AUX_OFF();
	// palSetPadMode(AUX_GPIO, AUX_PIN,
	// 		PAL_MODE_OUTPUT_PUSHPULL |
	// 		PAL_STM32_OSPEED_HIGHEST);

	// ADC Pins
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOB, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOB, 1, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);
}

void hw_setup_adc_channels(void)
{
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_Vrefint, 5, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_8, 6, ADC_SampleTime_15Cycles);

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_0, 5, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_9, 6, ADC_SampleTime_15Cycles);

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_1, 5, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 6, ADC_SampleTime_15Cycles);

	// Injected channels
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_12, 1, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 2, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_12, 2, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 3, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 3, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_12, 3, ADC_SampleTime_15Cycles);
}

void hw_start_i2c(void)
{
	i2cAcquireBus(&HW_I2C_DEV);

	if (!i2c_running)
	{
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

void hw_stop_i2c(void)
{
	i2cAcquireBus(&HW_I2C_DEV);

	if (i2c_running)
	{
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
void hw_try_restore_i2c(void)
{
	if (i2c_running)
	{
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

		for (int i = 0; i < 16; i++)
		{
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

#ifdef HW_HAS_SHUTDOWN_SWITCH
bool hw_sample_shutdown_button(void)
{
	chMtxLock(&shutdown_mutex);

	bt_diff = 0.0;

	for (int i = 0; i < 3; i++)
	{
		palSetPadMode(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN, PAL_MODE_INPUT_ANALOG);
		chThdSleep(5);
		float val1 = ADC_VOLTS(ADC_IND_SHUTDOWN);
		chThdSleepMilliseconds(1);
		float val2 = ADC_VOLTS(ADC_IND_SHUTDOWN);
		palSetPadMode(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN, PAL_MODE_OUTPUT_PUSHPULL);
		chThdSleepMilliseconds(1);

		bt_diff += (val1 - val2);
	}

	chMtxUnlock(&shutdown_mutex);

	return (bt_diff > 0.12);
}
#elif defined ADC_IND_IN_CURR

#define IN_CURRENT_SHUNT_RES (0.0005)
#define IN_CURRENT_SHUNT_GAIN (20.0)

typedef struct KFP_t
{
	double LastP; // 上次估算协方差 初始化值为0.02
	double Now_P; // 当前估算协方差 初始化值为0
	double out;	  // 卡尔曼滤波器输出 初始化值为0
	double Kg;	  // 卡尔曼增益 初始化值为0
	double Q;	  // 过程噪声协方差 初始化值为0.001，越大越信任输入
	double R;	  // 观测噪声协方差 初始化值为0.543，越大收敛越慢
} KFP;			  // Kalman Filter parameter

static volatile KFP input_current_KFP = {
	.LastP = 0.02,
	.Now_P = 0.0,
	.out = 0.0,
	.Kg = 0.0,
	.Q = 0.0005,
	.R = 0.6};

/**
 *卡尔曼滤波器
 *@param KFP *kfp 卡尔曼结构体参数
 *   float input 需要滤波的参数的测量值（即传感器的采集值）
 *@return 滤波后的参数（最优值）
 */
float kalmanFilter(KFP *kfp, double input)
{
	if (isnan(input))
		return input;
	// 预测协方差方程：k时刻系统估算协方差 = k-1时刻的系统协方差 + 过程噪声协方差
	kfp->Now_P = kfp->LastP + kfp->Q;
	// 卡尔曼增益方程：卡尔曼增益 = k时刻系统估算协方差 / （k时刻系统估算协方差 + 观测噪声协方差）
	kfp->Kg = kfp->Now_P / (kfp->Now_P + kfp->R);
	if (isnan(kfp->Kg))
		kfp->Kg = 0;
	// 更新最优值方程：k时刻状态变量的最优值 = 状态变量的预测值 + 卡尔曼增益 * （测量值 - 状态变量的预测值）
	kfp->out = kfp->out + kfp->Kg * (input - kfp->out); // 因为这一次的预测值就是上一次的输出值
	// 更新协方差方程: 本次的系统协方差付给 kfp->LastP 为下一次运算准备。
	kfp->LastP = (1 - kfp->Kg) * kfp->Now_P;
	return kfp->out;
}

float hw_read_input_current(void)
{
	return (V_REG / 4095.0) * 1000 * ADC_Value[ADC_IND_IN_CURR];
#ifdef CALIB_IN_CURR_OFFSET
	return (V_REG / 4095.0) *
		   kalmanFilter(&input_current_KFP, (ADC_Value[ADC_IND_IN_CURR] - input_current_sensor_offset)) /
		   IN_CURRENT_SHUNT_GAIN / IN_CURRENT_SHUNT_RES;
#else
	return (V_REG / 4095.0) *
		   kalmanFilter(&input_current_KFP, (ADC_Value[ADC_IND_IN_CURR] - 4096 / 2)) /
		   IN_CURRENT_SHUNT_GAIN / IN_CURRENT_SHUNT_RES;
#endif
}

#ifdef CALIB_IN_CURR_OFFSET
static volatile float input_current_sensor_offset = 1.65;
static volatile int16_t input_current_sensor_offset_samples = -1;
static volatile uint32_t input_current_sensor_offset_sum = 0;
void hw_get_input_current_offset(void)
{

	if (input_current_sensor_offset_samples > -1)
	{

		if (input_current_sensor_offset_samples >= 1000)
		{
			input_current_sensor_offset = (((float)input_current_sensor_offset_sum) /
										   input_current_sensor_offset_samples);
			// Reset the sum and samples
			input_current_sensor_offset_sum = 0;
			input_current_sensor_offset_samples = -1;
		}
		else
		{
			input_current_sensor_offset_sum += ADC_Value[ADC_IND_IN_CURR];
			input_current_sensor_offset_samples++;
		}
	}
}

void hw_start_input_current_sensor_offset_measurement(void)
{
	input_current_sensor_offset_samples = 0;
	input_current_sensor_offset_sum = 0;
}
#endif
#endif