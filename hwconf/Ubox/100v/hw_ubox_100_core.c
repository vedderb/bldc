/*
	BCopyright 2018 Benjamin Vedder	benjamin@vedder.se

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
//Copy from "hw_100_250.h"

#include "hw.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils_math.h"
#include <math.h>
#include "mc_interface.h"

#include "shutdown.h"
#include "app.h"
#include "conf_general.h"
#include "commands.h"
#include "timeout.h"

//UBOX uses a button to press to GND to make internal 12V regulator circuit to start work.
//And uses an IO of VESC MCU to read this button state,
//And another IO to hold internal 12V regulator's enable pin.
#define UBOX_POWER_EN_ON()				do{palSetPad(GPIOA, 15);}while(0)
#define UBOX_POWER_EN_OFF()				do{palClearPad(GPIOA, 15);}while(0)

#define UBOX_POWER_KEY_IO_PULL_LOW()	do{palClearPad(GPIOC, 13);}while(0)
#define UBOX_POWER_KEY_IO_RELEASE()		do{palSetPad(GPIOC, 13);}while(0)
#define UBOX_QUERY_POWER_KEY_IO()		(palReadPad(GPIOC, 13))


void shutdown_ubox_init(void);

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

	// LEDs
	palSetPadMode(LED_GREEN_GPIO, LED_GREEN_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(LED_RED_GPIO, LED_RED_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

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
	palSetPadMode(PHASE_FILTER_GPIO, PHASE_FILTER_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	PHASE_FILTER_OFF();

	// AUX pin
	AUX_OFF();
	palSetPadMode(AUX_GPIO, AUX_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

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
	palSetPadMode(GPIOC, 5, PAL_MODE_INPUT_ANALOG);

	//POWER ON EN, and start the shut_down_ubox thread
	palSetPadMode(GPIOA, 15, PAL_MODE_OUTPUT_PUSHPULL);
	palSetPad(GPIOA, 15);

	palSetPadMode(GPIOC, 13, PAL_MODE_OUTPUT_OPENDRAIN | PAL_MODE_INPUT_PULLUP);
	UBOX_POWER_KEY_IO_RELEASE();

	shutdown_ubox_init();
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_Vrefint, 5, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_8, 6, ADC_SampleTime_15Cycles);

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_0, 5, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_9, 6, ADC_SampleTime_15Cycles);

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 2, ADC_SampleTime_15Cycles);
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

float hw100_250_get_temp(void) {
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

//Copied and modified from shutdown.c
// Private variables
static bool volatile m_button_pressed = false;
static volatile float m_inactivity_time = 0.0;
static THD_WORKING_AREA(shutdown_ubox_thread_wa, 256);
static mutex_t m_sample_mutex;
static volatile bool m_init_done = false;
static volatile bool m_sampling_disabled = false;

// Private functions
static THD_FUNCTION(shutdown_ubox_thread, arg);

void shutdown_ubox_init(void) {
	chMtxObjectInit(&m_sample_mutex);
	chThdCreateStatic(shutdown_ubox_thread_wa, sizeof(shutdown_ubox_thread_wa), NORMALPRIO, shutdown_ubox_thread, NULL);
	m_init_done = true;
}

static bool do_shutdown_ubox(void) {
	conf_general_store_backup_data();

	chThdSleepMilliseconds(100);
	DISABLE_GATE();
	UBOX_POWER_EN_OFF();
	return true;
}


typedef enum {
	power_key_type_undecided = 0,
	power_key_type_momentary,
	power_key_type_latching,
	power_key_type_other_source,
}enPOWER_KEY_TYPE;
static enPOWER_KEY_TYPE power_key_type = power_key_type_undecided;
static uint32_t power_key_pressed_ms = 0;
static bool power_key_pressed_when_power_on = false;

static THD_FUNCTION(shutdown_ubox_thread, arg) {
	(void)arg;
	chRegSetThreadName("Shutdown_ubox");

	static bool power_key_io_released = false;
	systime_t last_iteration_time = chVTGetSystemTimeX();
	uint64_t odometer_old = mc_interface_get_odometer();

	for(;;)	{
		uint8_t power_key_click = 0;
		uint32_t sys_time_ms = chVTGetSystemTime() / (float)CH_CFG_ST_FREQUENCY * 1000.0f;

		float dt = (float)chVTTimeElapsedSinceX(last_iteration_time) / (float)CH_CFG_ST_FREQUENCY;
		last_iteration_time = chVTGetSystemTimeX();

		//Check power button.
		//Because the low level of power key is about 1V, high is 3.3v by its hardware design.
		//This 1.0V low signal voltage maybe can not to make the MCU to recognize it as 0, so it needs a workaround.
		//The principle is: to check if the IO be pulled up from 0V to 3.3V by internal pull-up resistor,
		//if yes, the power key is not pressing. The 0V is implemented by the assistance of IO's open drain pull down.
	    if(power_key_io_released == false) {
	    	power_key_io_released = true;
			UBOX_POWER_KEY_IO_RELEASE();	//Release the open drain configured IO and wait 10ms to let the internal resistor
											//to pulls the line up, next time to check the IO pin register.
	    } else {
	        if(UBOX_QUERY_POWER_KEY_IO() == 0) {
	            if(power_key_pressed_ms < 1000 * 60) {
	            	power_key_pressed_ms += 20;//
	            }
	        } else {
	            if((power_key_pressed_ms > 50) && (power_key_pressed_ms < 500)) {
	                power_key_click = 1;
	            }
	            power_key_pressed_ms = 0;
	        }

	        power_key_io_released = false;
	        UBOX_POWER_KEY_IO_PULL_LOW();//Pre-pull down the open drain configured IO, to make this IO read as 0
	    }

	    if(power_key_type == power_key_type_undecided) {
			if(sys_time_ms < 1000) {
				if(power_key_pressed_ms > 100) {
					power_key_pressed_when_power_on = true;
				}
			} else {
				if(power_key_pressed_when_power_on == false) {
					power_key_type = power_key_type_other_source;
				} else {
					if(power_key_pressed_ms == 0) {
						power_key_type = power_key_type_momentary;
					} else if(power_key_pressed_ms > 2000) {
						power_key_type = power_key_type_latching;
					}
				}
			}
			UBOX_POWER_EN_ON();
	    } else if(power_key_type == power_key_type_momentary) {
			m_button_pressed = (bool)(power_key_pressed_ms > 2000);
			bool clicked = (bool)(power_key_pressed_ms > 1000);

			const app_configuration *conf = app_get_configuration();

			// Note: When the gates are enabled, the push to start function
			// will prevent the regulator from shutting down. Therefore, the
			// gate driver has to be disabled.

			switch (conf->shutdown_mode) {
				case SHUTDOWN_MODE_ALWAYS_OFF:
					//When the power button being pushed accidently, the regulator will working
					//Inactive after 10 seconds, MCU cancels the enable signal, regulator shuts down if button released.
					m_inactivity_time += dt;
					if (m_inactivity_time >= 10.0f) {
						do_shutdown_ubox();
					}
				break;
				case SHUTDOWN_MODE_ALWAYS_ON:
					m_inactivity_time += dt;
					// Without a shutdown switch use inactivity timer to estimate
					// when device is stopped. Check also distance between store
					// to prevent excessive flash write cycles.
					if (m_inactivity_time >= SHUTDOWN_SAVE_BACKUPDATA_TIMEOUT) {
						shutdown_reset_timer();
						// If at least 1km was done then we can store data
						if((mc_interface_get_odometer()-odometer_old) >= 1000) {
							conf_general_store_backup_data();
							odometer_old = mc_interface_get_odometer();
						}
					}
					UBOX_POWER_EN_ON();
				break;
				case SHUTDOWN_MODE_TOGGLE_BUTTON_ONLY:
				if(clicked)	{
					do_shutdown_ubox();
				}
				break;
				default:	break;
			}
			if (conf->shutdown_mode >= SHUTDOWN_MODE_OFF_AFTER_10S) {
				m_inactivity_time += dt;
					float shutdown_timeout = 0.0;

				switch (conf->shutdown_mode) {
				case SHUTDOWN_MODE_OFF_AFTER_10S: shutdown_timeout = 10.0; break;
				case SHUTDOWN_MODE_OFF_AFTER_1M: shutdown_timeout = 60.0; break;
				case SHUTDOWN_MODE_OFF_AFTER_5M: shutdown_timeout = 60.0 * 5.0; break;
				case SHUTDOWN_MODE_OFF_AFTER_10M: shutdown_timeout = 60.0 * 10.0; break;
				case SHUTDOWN_MODE_OFF_AFTER_30M: shutdown_timeout = 60.0 * 30.0; break;
				case SHUTDOWN_MODE_OFF_AFTER_1H: shutdown_timeout = 60.0 * 60.0; break;
				case SHUTDOWN_MODE_OFF_AFTER_5H: shutdown_timeout = 60.0 * 60.0 * 5.0; break;
				default: break;
				}
				if (m_inactivity_time >= shutdown_timeout) {
					do_shutdown_ubox();
				}
			} else {
				//Because SHUTDOWN_MODE_ALWAYS_OFF's implementation will check m_inactivity_time.
				if(conf->shutdown_mode != SHUTDOWN_MODE_ALWAYS_OFF) {
					m_inactivity_time = 0.0;
				}
			}

			if(power_key_pressed_ms > 2000) {
				do_shutdown_ubox();
			}
	    } else {
	    	m_inactivity_time += dt;
	    	// Without a shutdown switch use inactivity timer to estimate
	    	// when device is stopped. Check also distance between store
	    	// to prevent excessive flash write cycles.
	    	if (m_inactivity_time >= SHUTDOWN_SAVE_BACKUPDATA_TIMEOUT) {
	    		shutdown_reset_timer();
	    		// If at least 1km was done then we can store data
	    		if((mc_interface_get_odometer()-odometer_old) >= 1000) {
	    			conf_general_store_backup_data();
	    			odometer_old = mc_interface_get_odometer();
	    		}
	    	}
	    	UBOX_POWER_EN_OFF();//In latching button mode, cancel the MCU's enable signal to regulator,
	    						//there for, when power button released, regulator shuts down.
	    }

		timeout_reset();

		chThdSleepMilliseconds(10);
	}
}



