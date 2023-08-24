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

#include "hw.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils.h"
#include <math.h>
#include "mc_interface.h"
#include "commands.h"
#include "terminal.h"
#include "mcpwm.h"
#include "mcpwm_foc.h"
#include "gpdrive.h"
#include "app.h"
#include "mempools.h"
#include "timeout.h"
#include "stdio.h"
#include "imu.h"

// Variables
static volatile bool i2c_running = false;
static volatile bool drv_handshake_complete = false;

// Private functions
static void terminal_cmd_doublepulse(int argc, const char** argv);
static void hw_a200s_set_hardware_current_limits(void);

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

	// LEDs	
	palSetPadMode(GPIOB, 5,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOB, 7,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	
	// HW protection pins	
	// Disable	  
	palSetPadMode(GPIOC, 5,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST); 
	ENABLE_GATE();
	 // Lockout
    palSetPadMode(GPIOB, 12, PAL_MODE_INPUT);
	
#ifdef HW_USE_BRK
	// BRK Fault pin
	palSetPadMode(BRK_GPIO, BRK_PIN, PAL_MODE_ALTERNATE(GPIO_AF_TIM1));
#else	
	// Soft Lockout
	palSetPadMode(BRK_GPIO, BRK_PIN, PAL_MODE_INPUT);
#endif
	
	// GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
	palSetPadMode(GPIOA, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 9, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 10, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOB, 13, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 15, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	// Hall sensors	
	palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_INPUT_PULLUP);

	// Phase filters
	palSetPadMode(GPIOC, 9, PAL_MODE_OUTPUT_OPENDRAIN);
	palSetPadMode(GPIOC, 13, PAL_MODE_OUTPUT_OPENDRAIN);
	palSetPadMode(GPIOC, 14, PAL_MODE_OUTPUT_OPENDRAIN);
	PHASE_FILTER_OFF();
	
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
	
	
	//register terminal callbacks	
	terminal_register_command_callback(
		"double_pulse",
		"Start a double pulse test",
		0,
		terminal_cmd_doublepulse);			
		
		
	hw_a200s_reset_faults(); // Handshake with hardware protection
}

void hw_setup_adc_channels(void) {
		
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 		1, ADC_SampleTime_15Cycles);	// 0 - ADC_IND_CURR1
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0,  		2, ADC_SampleTime_15Cycles);	// 3 - ADC_IND_SENS1	
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5,  		3, ADC_SampleTime_15Cycles);	// 6 - ADC_IND_EXT
	ADC_RegularChannelConfig(ADC1, ADC_Channel_Vrefint, 4, ADC_SampleTime_15Cycles);	// 9 - ADC_IND_VREFINT
	ADC_RegularChannelConfig(ADC1, ADC_Channel_8, 		5, ADC_SampleTime_15Cycles);	// 12 - ADC_IND_TEMP_MOS_2

	// ADC2 regular channels																
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 		1, ADC_SampleTime_15Cycles);	// 1 - ADC_IND_CURR2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 		2, ADC_SampleTime_15Cycles);	// 4 - ADC_IND_SENS2	
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 		3, ADC_SampleTime_15Cycles);	// 7 - ADC_IND_EXT2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_14, 		4, ADC_SampleTime_15Cycles);	// 10 - ADC_IND_TEMP_MOTOR
	ADC_RegularChannelConfig(ADC2, ADC_Channel_9, 		5, ADC_SampleTime_15Cycles);	// 13 - ADC_IND_TEMP_MOS_3

	// ADC3 regular channels - only a subset of channels avaliable											
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 		1, ADC_SampleTime_15Cycles);	// 2 - ADC_IND_CURR3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 		2, ADC_SampleTime_15Cycles);	// 5 - ADC_IND_SENS3	
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 		3, ADC_SampleTime_15Cycles);	// 8 - ADC_IND_VIN_SENS
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 		4, ADC_SampleTime_15Cycles);	// 11 - ADC_IND_TEMP_MOS		
	ADC_RegularChannelConfig(ADC3, ADC_Channel_14, 		5, ADC_SampleTime_15Cycles);	// 14 - UNUSED


	// Injected channels																	
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);			// ADC_IND_CURR1
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_15Cycles);			// ADC_IND_CURR2
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_12, 1, ADC_SampleTime_15Cycles);			// ADC_IND_CURR3
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);			// ADC_IND_CURR1
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 2, ADC_SampleTime_15Cycles);			// ADC_IND_CURR2
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_12, 2, ADC_SampleTime_15Cycles);			// ADC_IND_CURR3
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 3, ADC_SampleTime_15Cycles);			// ADC_IND_CURR1
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 3, ADC_SampleTime_15Cycles);			// ADC_IND_CURR2
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_12, 3, ADC_SampleTime_15Cycles);			// ADC_IND_CURR3
	
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

float hw_a200s_get_temp(void) {
	float t1 = (1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15);
	float t2 = (1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS_2]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15);
	float t3 = (1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS_3]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15);
	float res = 0.0;

	if (t1 > t2 && t1 > t3) {
		res = t1;
	} else if (t2 > t1 && t2 > t3) {
		res = t2;
	} else {
		res = t3;
	}

	return res;
}

bool hw_a200s_hardware_handshake(void) {	  	
	uint8_t rxb[1];	
		
	// Request handshake code, needs to wait for the i2c to be setup by the imu driver
	
	if(imu_startup_done()) {			
		if(i2c_bb_tx_rx(imu_get_i2c(), ATTINY3216_ADDR, NULL, 0, rxb, 1)) {		
			if(rxb[0] == ATTINY3216_HANDSHAKE_REPLY) {
				// OK
			} else {
				return false;
			}
		} else {
			return false;				
		}
		
		chThdSleep(10); // Small delay 
		
		
		if(i2c_bb_tx_rx(imu_get_i2c(), ATTINY1616_ADDR, NULL, 0, rxb, 1)) {		
			if(rxb[0] == ATTINY1616_HANDSHAKE_REPLY) {
				// OK
			} else {
				return false;
			}
		} else {
			return false;
		}		
	} else {
		return false;
	}
	
	return true;		
}

bool hw_a200s_drv_fault_check(void) {
	return (palReadPad(GPIOB, 12) || !drv_handshake_complete);	
}

void hw_a200s_reset_faults(void) {		
	// Send reset command to logger, needs to wait for the i2c to be setup by the imu driver	
	if(imu_startup_done()) {
		// Only reset the fault if the hardware protections are working.
		if(hw_a200s_hardware_handshake()) {	
			uint8_t txb[1];	
			
			// Setup current limit digipots at startup they will default to 0A
			hw_a200s_set_hardware_current_limits();			
			
			// Clear latches					
			txb[0] = 0x53;
			i2c_bb_tx_rx(imu_get_i2c(), ATTINY3216_ADDR, txb, 1, NULL, 0);
			
			txb[0] = 0x54;
			i2c_bb_tx_rx(imu_get_i2c(), ATTINY1616_ADDR, txb, 1, NULL, 0);
			
			// ATTiny should now have released the gate drivers for us to use
			drv_handshake_complete = true;
		}
	}
}

void hw_a200s_aux(bool enable){
	static int state = false; // only send changes to attiny1616 to avoid hogging the bus
	
	if(state != enable)
	{
		uint8_t txb[1];		
		
		if(imu_startup_done()) // needs to wait for the i2c to be setup by the imu driver
		{	
			if(enable)
			{
				txb[0] = 0x21;
			} else {
				txb[0] = 0x20;
			}	
			
			if(i2c_bb_tx_rx(imu_get_i2c(), ATTINY1616_ADDR, txb, 1, NULL, 0))
			{		
				state = enable;
			}						
		}	
	}			
}

static void hw_a200s_set_hardware_current_limits(void){	
	
	uint8_t txb[2];	
		
	if(imu_startup_done()) // needs to wait for the i2c to be setup by the imu driver
	{	
		// Digipots have 128 positions, default is midpoint
		// Each position is 3.3 / ((0.0002 / 3) * 20) = 2475 / 128 = 19.34A			
		// So current / 19.34  = trip value position
		// Midpoint is 64 
		int channel_digipot_position = 64.0f + ceilf(HW_PROTECTION_CURR_TRIP_CHANNEL / 19.34f);
		int diode_digipot_position = 64.0f - ceilf(HW_PROTECTION_CURR_TRIP_DIODE / 19.34f);
		
		utils_truncate_number_int(&channel_digipot_position, 64, 127);
		utils_truncate_number_int(&diode_digipot_position, 0, 64);		
		
		txb[0] = 0x00; // Wiper Position register
		txb[1] = (unsigned)channel_digipot_position;
		if(i2c_bb_tx_rx(imu_get_i2c(), TPL0401A_10DCKR_ADDR, txb, 2, NULL, 0))
		{	
			// Success
		}
		
		txb[1] = (unsigned)diode_digipot_position;
		if(i2c_bb_tx_rx(imu_get_i2c(), TPL0401B_10DCKR_ADDR, txb, 2, NULL, 0))
		{	
			// Success
		}			
	}		
}


static void terminal_cmd_doublepulse(int argc, const char** argv)
{
	(void)argc;
	(void)argv;

	int preface, pulse1, breaktime, pulse2;
	int utick;
	int deadtime = -1;

	TIM_TimeBaseInitTypeDef	 TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	TIM_BDTRInitTypeDef TIM_BDTRInitStructure;

	if (argc < 5) {
		commands_printf("Usage: double_pulse <preface> <pulse1> <break> <pulse2> [deadtime]");
		commands_printf("	preface: idle time in us");
		commands_printf("	 pulse1: high time of pulse 1 in us");
		commands_printf("	  break: break between pulses in us");
		commands_printf("	 pulse2: high time of pulse 2 in us");
		commands_printf("  deadtime: overwrite deadtime, in ns");
		return;
	}
	sscanf(argv[1], "%d", &preface);
	sscanf(argv[2], "%d", &pulse1);
	sscanf(argv[3], "%d", &breaktime);
	sscanf(argv[4], "%d", &pulse2);
	if (argc == 6) {
		sscanf(argv[5], "%d", &deadtime);
	}
	timeout_configure_IWDT_slowest();

	utick = (int)(SYSTEM_CORE_CLOCK / 1000000);
	mcpwm_deinit();
	mcpwm_foc_deinit();
	gpdrive_deinit();

	TIM_Cmd(TIM1, DISABLE);
	TIM_Cmd(TIM4, DISABLE);
	//TIM4 als Trigger Timer
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE);

	TIM_TimeBaseStructure.TIM_Period = (SYSTEM_CORE_CLOCK / 20000);
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM4, &TIM_TimeBaseStructure);
	TIM_SelectMasterSlaveMode(TIM4, TIM_MasterSlaveMode_Enable);
	TIM_SelectOutputTrigger(TIM4, TIM_TRGOSource_Enable);
	TIM4->CNT = 0;

	// TIM1
	// TIM1 clock enable
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, ENABLE);

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = (preface + pulse1) * utick;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(TIM1, &TIM_TimeBaseStructure);

	// Channel 1, 2 and 3 Configuration in PWM mode
	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM2;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_OutputNState = TIM_OutputNState_Enable;
	TIM_OCInitStructure.TIM_Pulse = preface * utick;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_High;
	TIM_OCInitStructure.TIM_OCIdleState = TIM_OCIdleState_Set;
	TIM_OCInitStructure.TIM_OCNIdleState = TIM_OCNIdleState_Set;

	TIM_OC1Init(TIM1, &TIM_OCInitStructure);
	TIM_OC1PreloadConfig(TIM1, TIM_OCPreload_Enable);
	TIM_OC2Init(TIM1, &TIM_OCInitStructure);
	TIM_OC2PreloadConfig(TIM1, TIM_OCPreload_Enable);
	TIM_OC3Init(TIM1, &TIM_OCInitStructure);
	TIM_OC3PreloadConfig(TIM1, TIM_OCPreload_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_PWM2);
	TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
	TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
	TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Enable);
	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);


	// Automatic Output enable, Break, dead time and lock configuration
	TIM_BDTRInitStructure.TIM_OSSRState = TIM_OSSRState_Enable;
	TIM_BDTRInitStructure.TIM_OSSIState = TIM_OSSIState_Enable;
	TIM_BDTRInitStructure.TIM_LOCKLevel = TIM_LOCKLevel_OFF;
	if (deadtime < 0) {
		TIM_BDTRInitStructure.TIM_DeadTime = conf_general_calculate_deadtime(HW_DEAD_TIME_NSEC, SYSTEM_CORE_CLOCK);
	} else {
		TIM_BDTRInitStructure.TIM_DeadTime = conf_general_calculate_deadtime(deadtime, SYSTEM_CORE_CLOCK);
	}
	TIM_BDTRInitStructure.TIM_Break = TIM_Break_Disable;
	TIM_BDTRInitStructure.TIM_BreakPolarity = TIM_BreakPolarity_High;
	TIM_BDTRInitStructure.TIM_AutomaticOutput = TIM_AutomaticOutput_Disable;
	TIM_BDTRConfig(TIM1, &TIM_BDTRInitStructure);

	TIM_CCPreloadControl(TIM1, ENABLE);
	TIM_ARRPreloadConfig(TIM1, ENABLE);

	TIM1->CNT = 0;
	TIM1->EGR = TIM_EGR_UG;

	TIM_SelectSlaveMode(TIM1, TIM_SlaveMode_Trigger);
	TIM_SelectInputTrigger(TIM1, TIM_TS_ITR3);
	TIM_SelectOnePulseMode(TIM1, TIM_OPMode_Single);
	TIM_CtrlPWMOutputs(TIM1, ENABLE);

	TIM_Cmd(TIM1, ENABLE);
	//Timer 4 triggert Timer 1
	TIM_Cmd(TIM4, ENABLE);
	TIM_Cmd(TIM4, DISABLE);
	TIM1->ARR = (breaktime + pulse2) * utick;
	TIM1->CCR1 = breaktime * utick;
	while (TIM1->CNT != 0);
	TIM_Cmd(TIM4, ENABLE);

	chThdSleepMilliseconds(1);
	TIM_CtrlPWMOutputs(TIM1, DISABLE);
	mc_configuration* mcconf = mempools_alloc_mcconf();
	*mcconf = *mc_interface_get_configuration();

	switch (mcconf->motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_init(mcconf);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_init(mcconf, mcconf);
		break;

	case MOTOR_TYPE_GPD:
		gpdrive_init(mcconf);
		break;

	default:
		break;
	}
	commands_printf("Done");
	return;
}
