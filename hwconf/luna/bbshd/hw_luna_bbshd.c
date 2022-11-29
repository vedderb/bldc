/*
	Copyright 2020 Marcos Chaparro	mchaparro@powerdesigns.ca
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
#include "luna_display_serial.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils.h"
#include <math.h>
#include "mc_interface.h"
#include "terminal.h"
#include "commands.h"
#include "stdio.h"
#include "app.h"
#include "mempools.h"

#define EEPROM_ADDR_INITIAL_ASSIST_LEVEL	0
#define EEPROM_ADDR_MOTOR_HAS_PTC_SENSOR	1
#define EEPROM_ADDR_FIXED_THROTTLE_LEVEL	2
#define DEFAULT_INITIAL_ASSIST_LEVEL        1

// Variables
static volatile bool i2c_running = false;
static volatile bool motor_has_PTC_sensor;

void hw_luna_bbshd_setup_dac(void);
static void terminal_cmd_set_initial_assist_level(int argc, const char **argv);
static void terminal_cmd_read_initial_assist_level(int argc, const char **argv);
static void terminal_cmd_set_bbshd_has_PTC_sensor(int argc, const char **argv);
static void terminal_cmd_set_bbshd_use_fixed_throttle_level(int argc, const char **argv);
static void hw_override_pairing_done(void);

int8_t hw_read_initial_assist_level(void);
bool hw_bbshd_has_PTC_sensor(void);
bool hw_bbshd_has_fixed_throttle_level(void);

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
	palSetPadMode(LED_GREEN_GPIO, LED_GREEN_PIN, PAL_MODE_OUTPUT_PUSHPULL |	PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(LED_RED_GPIO, LED_RED_PIN, PAL_MODE_OUTPUT_PUSHPULL |	PAL_STM32_OSPEED_HIGHEST);

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

#ifdef HW_USE_BRK
	// BRK Fault pin
	palSetPadMode(BRK_GPIO, BRK_PIN, PAL_MODE_ALTERNATE(GPIO_AF_TIM1));
#endif

	palSetPadMode(HW_SPEED_SENSOR_PORT, HW_SPEED_SENSOR_PIN, PAL_MODE_INPUT_PULLUP);

	// Current filter
	palSetPadMode(GPIOC, 13, PAL_MODE_OUTPUT_PUSHPULL |	PAL_STM32_OSPEED_HIGHEST);
	CURRENT_FILTER_OFF();

	// AUX pin
	AUX_OFF();
	palSetPadMode(AUX_GPIO, AUX_PIN, PAL_MODE_OUTPUT_PUSHPULL |	PAL_STM32_OSPEED_HIGHEST);

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
    
	
	terminal_register_command_callback(
			"set_initial_assist_level",
			"Set initial assist level [0 - 9].",
			0,
			terminal_cmd_set_initial_assist_level);
    
    terminal_register_command_callback(
			"read_initial_assist_level",
			"Read initial assist level.",
			0,
			terminal_cmd_read_initial_assist_level);
        
	terminal_register_command_callback(
			"set_motor_temp_sensor",
			"Usage: set_motor_temp_sensor [NTC or PTC]",
			0,
			terminal_cmd_set_bbshd_has_PTC_sensor);

	terminal_register_command_callback(
			"fix_throttle",
			"Usage: fix_throttle [1 or 0]",
			0,
			terminal_cmd_set_bbshd_use_fixed_throttle_level);

	int8_t initial_assist_level = hw_read_initial_assist_level();
	motor_has_PTC_sensor = true;// hw_bbshd_has_PTC_sensor();
	hw_override_pairing_done();

	luna_display_serial_start(initial_assist_level);
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0,  1, ADC_SampleTime_15Cycles);	// 0	SENS1
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);	// 3	CURR1
	ADC_RegularChannelConfig(ADC1, ADC_Channel_8,  3, ADC_SampleTime_15Cycles);	// 6	ADC_IND_EXT2
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 4, ADC_SampleTime_15Cycles); // 9	TEMP_MOTOR
	ADC_RegularChannelConfig(ADC1, ADC_Channel_9,  5, ADC_SampleTime_15Cycles);	// 12	V_GATE_DRIVER
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5,  6, ADC_SampleTime_15Cycles);	// 15	TEMP_FET

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1,  1, ADC_SampleTime_15Cycles);	// 1	SENS2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 2, ADC_SampleTime_15Cycles);	// 4	CURR2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6,  3, ADC_SampleTime_15Cycles);	// 7	UNUSED
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 4, ADC_SampleTime_15Cycles);	// 10	ADC_IND_EXT
	ADC_RegularChannelConfig(ADC2, ADC_Channel_7,  5, ADC_SampleTime_15Cycles);	// 13	ADC_IND_EXT3
	ADC_RegularChannelConfig(ADC2, ADC_Channel_Vrefint, 6, ADC_SampleTime_15Cycles);// 16	ADC_IND_VREFINT

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2,  1, ADC_SampleTime_15Cycles);	// 2	SENS3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 2, ADC_SampleTime_15Cycles);	// 5	CURR3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3,  3, ADC_SampleTime_15Cycles);	// 8	PCB_TEMP
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);	// 11	VBUS
	ADC_RegularChannelConfig(ADC3, ADC_Channel_1,  5, ADC_SampleTime_15Cycles);	// 14	UNUSED
	ADC_RegularChannelConfig(ADC3, ADC_Channel_Vrefint, 6, ADC_SampleTime_15Cycles);// 18	UNUSED

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

static float wheel_rpm_filtered = 0;
static float trip_odometer = 0;

void hw_update_speed_sensor(void) {
	static float wheel_rpm = 0;
	static uint8_t sensor_state = 0;
	static uint8_t sensor_state_old = 0;
	static float last_sensor_event_time = 0;
	float current_time = (float)chVTGetSystemTimeX() / (float)CH_CFG_ST_FREQUENCY;

	sensor_state = palReadPad(HW_SPEED_SENSOR_PORT, HW_SPEED_SENSOR_PIN);

	if(sensor_state == 0 && sensor_state_old == 1 ) {
		float revolution_duration = current_time - last_sensor_event_time;

		if (revolution_duration > 0.05) {	//ignore periods <50ms
			last_sensor_event_time = current_time;
			wheel_rpm = 60.0 / revolution_duration;
			UTILS_LP_FAST(wheel_rpm_filtered, (float)wheel_rpm, 0.5);

			
			const volatile mc_configuration *conf = mc_interface_get_configuration();
			trip_odometer += conf->si_wheel_diameter * M_PI;
			//trip_odometer += mc_interface_get_configuration()->si_wheel_diameter * M_PI; test this
		}
	} else {
		// After 3 seconds without sensor signal, set RPM as zero
		if ( (current_time - last_sensor_event_time) > 3.0) {
			wheel_rpm_filtered = 0.0;
		}
	}
	sensor_state_old = sensor_state;
}

/* Get speed in m/s */
float hw_get_speed(void) {
	const volatile mc_configuration *conf = mc_interface_get_configuration();
	float speed = wheel_rpm_filtered * conf->si_wheel_diameter * M_PI / 60.0;
	return speed;
}

/* Get trip distance in meters */
float hw_get_distance(void) {
	return trip_odometer;
}

float hw_get_distance_abs(void) {
	return trip_odometer;
}
/* Gear Shift sensor support
 * Read the gear sensor and use it to override the brake adc signal to reduce motor 
 * power during shifting to extend gearing life.
 */
void hw_brake_override(float *brake_ptr) {
	float brake = *brake_ptr;

	// Track an independent gearshift sensor ramping to be multiplied by the brake signal
	static float gear = 0.0;
	gear = (float)palReadPad(HW_GEAR_SENSOR_PORT, HW_GEAR_SENSOR_PIN);

	// hardcoded ramps for now
	const float ramp_time_neg = 0.1;
	const float ramp_time_pos = 0.3;

	// Apply ramping
	static systime_t last_time = 0;
	static float gear_ramp = 0.0;
	float ramp_time = fabsf(gear) > fabsf(gear_ramp) ? ramp_time_pos : ramp_time_neg;

	if (ramp_time > 0.01) {
		const float ramp_step = (float)ST2MS(chVTTimeElapsedSinceX(last_time)) / (ramp_time * 1000.0);
		utils_step_towards(&gear_ramp, gear, ramp_step);
		last_time = chVTGetSystemTimeX();
		*brake_ptr = brake * gear_ramp;
	}

	static uint16_t delay_to_print = 0;
	if(delay_to_print ++ > 250){
		delay_to_print = 0;
		commands_printf("gear_ramp:%.2f",(double)gear_ramp);
		commands_printf("brake_output:%.2f",(double)brake);
	}
}

// Lookup table linearly interpolated to support the new, undocumented PTC sensor used
// in these drives

#define PTC_LUT_SIZE	20
typedef struct { float x; float y; } coord_t;

coord_t ptc_lut[PTC_LUT_SIZE] = 
{
    {933.0, -15.0},
	{940.0, -11.0},
    {1090.0, 25.0},
    {1107.0, 30.0},
    {1124.0, 35.0}, 
    {1140.0, 40.0}, 
	{1155.0, 45.0},
	{1170.0, 50.0},
	{1181.0, 55.0},
	{1191.0, 60.0},
	{1198.0, 65.0},
	{1202.0, 70.0},
	{1211.0, 75.0},
	{1217.0, 80.0},
	{1224.0, 85.0},
	{1231.0, 90.0},
	{1274.0, 95.0},
	{1385.0, 100.0},
	{1390.0, 110.0},//made up
	{1400.0, 200.0}//made up
};

float interp( coord_t* c, float x)
{
    int i;
	const int n = PTC_LUT_SIZE;

    for( i = 0; i < n-1; i++ )
    {
        if ( c[i].x <= x && c[i+1].x >= x )
        {
			//utils_map()
            float diffx = x - c[i].x;
            float diffn = c[i+1].x - c[i].x;

            return c[i].y + ( c[i+1].y - c[i].y ) * diffx / diffn; 
        }
    }

    return 200.0; // Not in range, trip a fault
}



static void terminal_cmd_set_initial_assist_level(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	eeprom_var initial_assist_level;
	if( argc == 2 ) {
		sscanf(argv[1], "%i", (int*) &(initial_assist_level.as_i32));

		// Store data in eeprom
		conf_general_store_eeprom_var_hw(&initial_assist_level, EEPROM_ADDR_INITIAL_ASSIST_LEVEL);

		//read back written data
		int32_t assist_level = hw_read_initial_assist_level();

		if(assist_level == initial_assist_level.as_i32) {
			commands_printf("BBSHD initial assist level set to %d", assist_level);
		}
		else {
			commands_printf("Error storing EEPROM data.");
		}
	}
	else {
		commands_printf("1 argument required, integer from 0 to 9. Here are some examples:");
		commands_printf("set_initial_assist_level 0");
		commands_printf("set_initial_assist_level 1");
		commands_printf("set_initial_assist_level 4");
		commands_printf("set_initial_assist_level 9");
		commands_printf(" ");
	}
	commands_printf(" ");
	return;
}

static void terminal_cmd_read_initial_assist_level(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	commands_printf("BBSHD initial assist level is set at %i", hw_read_initial_assist_level());
	commands_printf(" ");
	return;
}

int8_t hw_read_initial_assist_level(void) {
	eeprom_var assist_level;
    bool var_not_found = !conf_general_read_eeprom_var_hw(&assist_level, EEPROM_ADDR_INITIAL_ASSIST_LEVEL);

	if( (assist_level.as_i32 < 0) || (assist_level.as_i32 >= 10) || var_not_found)
		assist_level.as_i32 = DEFAULT_INITIAL_ASSIST_LEVEL;
	return (int8_t)assist_level.as_i32;
}

float hw_read_motor_temp(float beta) {
	static float sensor_resistance = 1000;
	UTILS_LP_FAST(sensor_resistance, NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]), 0.1);

	// the ptc sensor has 1.3kOhm at 100°C and 930 Ohm at -15°C. If resistance is outside this
	// range there is no way this is a PTC sensor
	if( (sensor_resistance > 2000 ) || (sensor_resistance < 900) ) {
		motor_has_PTC_sensor = false;
	}
	if( motor_has_PTC_sensor ) {
		// PTC
		return interp(ptc_lut, sensor_resistance);
	} else {
		// NTC
		return (1.0 / ((logf(NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]) / 10000.0) / beta) + (1.0 / 298.15)) - 273.15);
	}
}

static void terminal_cmd_set_bbshd_has_PTC_sensor(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	eeprom_var has_ptc;
	if( argc == 2 ) {
		char sensor_type[32];

		sscanf(argv[1], "%s", sensor_type);

		if( sensor_type[0] == 'N' ) {	// NTC
			has_ptc.as_i32 = 0;
		}
		if( sensor_type[0] == 'P' ) {	// PTC
			has_ptc.as_i32 = 1;
		}

		// Store data in eeprom
		conf_general_store_eeprom_var_hw(&has_ptc, EEPROM_ADDR_MOTOR_HAS_PTC_SENSOR);

		//read back written data
		motor_has_PTC_sensor = hw_bbshd_has_PTC_sensor();
		
		if( motor_has_PTC_sensor ) {
			commands_printf("Set as PTC\n");
		} else {
			commands_printf("Set as NTC\n");
		}
	}
	else {
		commands_printf("1 argument required, NTC or PTC. Here are some examples:");
		commands_printf("set_motor_temp_sensor NTC");
		commands_printf("set_motor_temp_sensor PTC\n");
	}
	return;
}

bool hw_bbshd_has_PTC_sensor(void) {
	eeprom_var has_ptc;
	//return 1;
	bool var_not_found = !conf_general_read_eeprom_var_hw(&has_ptc, EEPROM_ADDR_MOTOR_HAS_PTC_SENSOR);

	if( (has_ptc.as_i32 != 1) || var_not_found) {
		return false;
	} else {
		return true;
	}
}

static void terminal_cmd_set_bbshd_use_fixed_throttle_level(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	eeprom_var use_fixed_throttle;
	if( argc == 2 ) {
		char throttle_type[32];

		sscanf(argv[1], "%s", throttle_type);

		use_fixed_throttle.as_i32 = (throttle_type[0] == '1') ? 1 : 0;

		// Store data in eeprom
		conf_general_store_eeprom_var_hw(&use_fixed_throttle, EEPROM_ADDR_FIXED_THROTTLE_LEVEL);
	}
	else {
		commands_printf("1 argument required: 1 (fixed) or 0 (follow display level)");
	}
	return;
}

bool hw_bbshd_has_fixed_throttle_level(void) {
	eeprom_var use_fixed_throttle;
	bool var_not_found = !conf_general_read_eeprom_var_hw(&use_fixed_throttle, EEPROM_ADDR_FIXED_THROTTLE_LEVEL);

	if( (use_fixed_throttle.as_i32 != 1) || var_not_found) {
		return false;
	} else {
		return true;
	}
}

// Users are getting locked out because they don't know what pairing means. Lets disable the pairing
static void hw_override_pairing_done(void) {
	if( app_get_configuration()->pairing_done == true) {
		app_configuration *appconf = mempools_alloc_appconf();
		*appconf = *app_get_configuration();
		appconf->pairing_done = false;

		conf_general_store_app_configuration(appconf);
		app_set_configuration(appconf);

		mempools_free_appconf(appconf);
	}
}
