/*
	Copyright 2021 Marcos Chaparro	mchaparro@powerdesigns.ca
	Copyright 2021 Maximiliano Cordoba	mcordoba@powerdesigns.ca

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

#include "conf_general.h"
#ifdef HW_HAS_LUNA_SERIAL_DISPLAY
#include "hw.h"
#include "luna_display_serial.h"
#include "app.h"
#include "ch.h"
#include "hal.h"
#include "packet.h"
#include "commands.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>
#include <string.h>
#include "comm_can.h"
#include "datatypes.h"

#define CMD_READ			0x11
#define CMD_WRITE			0x16

#define CMD_CONNECT			0x51
#define CMD_READ_BASIC		0x52
#define CMD_WRITE_BASIC		0x52
#define CMD_VERSION			0x90
#define CMD_AMPS			0x0A
#define CMD_WHEEL_RPM		0x20
#define CMD_MOVING			0x31
#define CMD_LIGHT_ON		0xF1
#define CMD_LIGHT_OFF		0xF0
#define CMD_PEDAL_MOV		0x08
#define CMD_BATT_SOC		0x11
#define CMD_BATT_VOLTAGE	0x12
#define CMD_PAS_LEVEL		0x0B

#define CMD_READ_BASIC_LENGTH	26
#define CMD_WRITE_BASIC_LENGTH	26

#define CMD_WRITE_BASIC_SUCCESS 0x18

#define LUNA_DISPLAY_BAUD	1200
#define LUNA_SERIAL_BUFFER_SIZE	32
#define LUNA_TX_SERIAL_BUFFER_SIZE	32

typedef enum {
	PAS_LEVEL_0 = 0x00,
	PAS_LEVEL_1 = 0x01,
	PAS_LEVEL_2 = 0x0B,
	PAS_LEVEL_3 = 0x0C,
	PAS_LEVEL_4 = 0x0D,
	PAS_LEVEL_5 = 0x02,
	PAS_LEVEL_6 = 0x15,
	PAS_LEVEL_7 = 0x16,
	PAS_LEVEL_8 = 0x17,
	PAS_LEVEL_9 = 0x03,
	PAS_LEVEL_WALK = 0x06,
} LUNA_PAS_LEVEL;

typedef enum {
	WRITE_LOW_BATTERY_ERROR = 0x00,
	WRITE_MAX_CURRENT_ERROR = 0x01,
	WRITE_ASSIST_LEVEL_ERROR = 0x02,
	WRITE_ASSIST_SPEED_ERROR = 0x0C,
	WRITE_SPEEDOMETER_ERROR = 0x16,
	WRITE_SPEEDOMETER_SIGNAL_ERROR = 0x17,
	WRITE_BASIC_SUCCESS = 0x18
} CMD_WRITE_BASIC_RESPONSE;

typedef enum {
	LUNA_ERROR_NONE = 0x00,
	LUNA_ERROR_BRAKES = 0x03,
	LUNA_ERROR_HIGH_VOLTAGE = 0x07,
	LUNA_ERROR_MOTOR_HALL_SENSOR = 0x08,
	LUNA_ERROR_OVER_TEMPERATURE = 0x10,
	LUNA_ERROR_CURRENT_SENSOR = 0x12,
	LUNA_ERROR_BATTERY_TEMPERATURE = 0x13,
	LUNA_ERROR_WHEEL_SPEED_DETECTION = 0x21,
	LUNA_ERROR_BMS_COMMUNICATION = 0x22,
	LUNA_ERROR_TORQUE_SENSOR = 0x25,
	LUNA_ERROR_SPEED_SENSOR = 0x26,
	LUNA_ERROR_COMMUNICATION = 0x30,
} LUNA_ERROR_CODES;

typedef struct {
	uint8_t header;
	uint8_t length;
	uint8_t manufacturer[4];
	uint8_t model[4];
	uint8_t hardware_version[2];
	uint8_t firmware_version[2];
	uint8_t voltage;
	uint8_t max_current;
	uint8_t checksum;
} controller_info_t;

typedef struct {
	unsigned int rd_ptr;
	unsigned int wr_ptr;
	unsigned char data[LUNA_SERIAL_BUFFER_SIZE];
	unsigned char tx[LUNA_TX_SERIAL_BUFFER_SIZE];
} luna_serial_buffer_t;

static volatile LUNA_PAS_LEVEL pas_level = PAS_LEVEL_1;
static volatile bool display_thread_is_running = false;
static volatile bool display_uart_is_running = false;

/* UART driver configuration structure */
static SerialConfig uart_cfg = {
		LUNA_DISPLAY_BAUD,
		0,
		USART_CR2_LINEN,
		0
};

static luna_serial_buffer_t serial_buffer;
static controller_info_t controller_info = {0x51,0x10,"LUNA","BBSH","12","55",0x05,250,00};

// Threads
static THD_WORKING_AREA(display_process_thread_wa, 1024);
static THD_FUNCTION(display_process_thread, arg);

static bool check_assist_level(uint8_t assist_code);
static LUNA_PAS_LEVEL translate_assist_level(int8_t level);
static void set_assist_level(uint8_t assist_code);
static uint8_t checksum(uint8_t *buf, uint8_t len);
static void serial_send_packet(unsigned char *data, unsigned int len);
static void serial_display_byte_process(unsigned char byte);
static void serial_display_check_rx(void);

void luna_display_serial_start(int8_t initial_level) {
    pas_level = translate_assist_level(initial_level);

	if (!display_thread_is_running) {
		chThdCreateStatic(display_process_thread_wa, sizeof(display_process_thread_wa),
				NORMALPRIO, display_process_thread, NULL);
		display_thread_is_running = true;
	}
	serial_buffer.rd_ptr = 0;
	serial_buffer.wr_ptr = 0;

	sdStart(&HW_UART_DEV, &uart_cfg);
	palSetPadMode(HW_UART_TX_PORT, HW_UART_TX_PIN, PAL_MODE_ALTERNATE(HW_UART_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);
	palSetPadMode(HW_UART_RX_PORT, HW_UART_RX_PIN, PAL_MODE_ALTERNATE(HW_UART_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);
    
	display_uart_is_running = true;
}

static LUNA_PAS_LEVEL translate_assist_level(int8_t level) {
    switch (level) {
        case 0: return PAS_LEVEL_0;
        case 1: return PAS_LEVEL_1;
        case 2: return PAS_LEVEL_2;
        case 3: return PAS_LEVEL_3;
        case 4: return PAS_LEVEL_4;
        case 5: return PAS_LEVEL_5;
        case 6: return PAS_LEVEL_6;
        case 7: return PAS_LEVEL_7;
        case 8: return PAS_LEVEL_8;
        case 9: return PAS_LEVEL_9;
        default: return -1;
    }    
}

/*
 * checks if the assist code to a current level
 *
 * returns true if valid, false if not.
 */
static bool check_assist_level(uint8_t assist_code) {
	bool ret = false;

	switch (assist_code) {
		case PAS_LEVEL_0:
		case PAS_LEVEL_1:
		case PAS_LEVEL_2:
		case PAS_LEVEL_3:
		case PAS_LEVEL_4:
		case PAS_LEVEL_5:
		case PAS_LEVEL_6:
		case PAS_LEVEL_7:
		case PAS_LEVEL_8:
		case PAS_LEVEL_9:
			ret = true;
			break;
		default:
			break;
	}
	return ret;
}

/*
 * Sets the PAS current according to the assist code
 *
 */
static void set_assist_level(uint8_t assist_code) {
	float current_scale;
    volatile mc_configuration *mcconf = (volatile mc_configuration*) mc_interface_get_configuration();

	switch (assist_code) {
		case PAS_LEVEL_0: current_scale = 0.0; break;
		case PAS_LEVEL_1: current_scale = 1.0 / 9.0; break;
		case PAS_LEVEL_2: current_scale = 2.0 / 9.0; break;
		case PAS_LEVEL_3: current_scale = 3.0 / 9.0; break;
		case PAS_LEVEL_4: current_scale = 4.0 / 9.0; break;
		case PAS_LEVEL_5: current_scale = 5.0 / 9.0; break;
		case PAS_LEVEL_6: current_scale = 6.0 / 9.0; break;
		case PAS_LEVEL_7: current_scale = 7.0 / 9.0; break;
		case PAS_LEVEL_8: current_scale = 8.0 / 9.0; break;
		case PAS_LEVEL_9: current_scale = 1.0; break;
		default: return;
	}

	if( hw_bbshd_has_fixed_throttle_level() ) {
		mcconf->l_current_max_scale = 1.0;
		app_pas_set_current_sub_scaling(current_scale);
	} else {
		mcconf->l_current_max_scale = current_scale;
	}

	// In level 0, both PAS and throttle should be disabled
	if(current_scale == 0.0) {
		mcconf->l_current_max_scale = current_scale;
	}
}

static uint8_t checksum(uint8_t *buf, uint8_t len) {
	uint8_t sum = 0;
	for(int i = 0; i<len; i++) {
		sum += buf[i];
	}
	return sum;
}

static void serial_send_packet(unsigned char *data, unsigned int len) {
	if (display_uart_is_running) {
		sdWrite(&HW_UART_DEV, data, len);
	}
}

static void serial_display_byte_process(unsigned char byte) {
	//append new byte to the buffer.
	serial_buffer.data[serial_buffer.wr_ptr] = byte;
	serial_buffer.wr_ptr++;

	uint8_t rd_ptr = serial_buffer.rd_ptr;	//read pointer to try at different start addresses
	
	// process with at least 2 bytes available to read
	while( (serial_buffer.wr_ptr - rd_ptr ) > 1) {

		if(serial_buffer.data[rd_ptr] == CMD_WRITE) {
			// start byte found
			uint8_t command = serial_buffer.data[rd_ptr + 1];
			uint8_t checksum_addr;

			switch (command) {
				case CMD_PAS_LEVEL:
					if ( (serial_buffer.wr_ptr - rd_ptr) < 4 ) {	//this packet needs 4 bytes
						break;
					}
					checksum_addr = rd_ptr + 3;
					if(checksum_addr <= serial_buffer.wr_ptr) {	//check the checksum has been received
						// check sum
						if( serial_buffer.data[checksum_addr] ==
							checksum(serial_buffer.data + rd_ptr, 3) ) {

							if(check_assist_level(serial_buffer.data[rd_ptr + 2])){
								pas_level = serial_buffer.data[rd_ptr + 2];
								set_assist_level(pas_level);
							}
							serial_buffer.rd_ptr = rd_ptr + 4;	//mark bytes as read
						}
					}
					break;
				case CMD_WRITE_BASIC:
					commands_printf("CMD_WRITE_BASIC");

					if ( (serial_buffer.wr_ptr - rd_ptr) < CMD_WRITE_BASIC_LENGTH ) {	//this packet needs 26 bytes
						break;
					}

					checksum_addr = rd_ptr + CMD_WRITE_BASIC_LENGTH - 1;

					if(checksum_addr <= serial_buffer.wr_ptr) {	//check the checksum has been received
						commands_printf("checksum received,calculated: %d    %d",serial_buffer.data[checksum_addr],
                        		checksum(serial_buffer.data + rd_ptr, CMD_WRITE_BASIC_LENGTH - 1));
						// check sum
						if( serial_buffer.data[checksum_addr] ==
							checksum(serial_buffer.data + rd_ptr, CMD_WRITE_BASIC_LENGTH - 1) ) {

							volatile mc_configuration *mcconf = (volatile mc_configuration*) mc_interface_get_configuration();

							mcconf->l_min_vin = (float) serial_buffer.data[rd_ptr + 2];
							mcconf->l_current_max = (float) serial_buffer.data[rd_ptr + 3];
							//skipping assist table for now
							mcconf->si_wheel_diameter = (float)serial_buffer.data[rd_ptr + 24] * 25.4 / 2.0;

							//write settings to flash memory as motor_0
							conf_general_store_mc_configuration((mc_configuration*)mcconf, false);

							for(int i = 0; i<=CMD_WRITE_BASIC_LENGTH;i++) {
								commands_printf("%02x ", serial_buffer.data[rd_ptr + i]);
							}
                            
							serial_buffer.rd_ptr = rd_ptr + CMD_WRITE_BASIC_LENGTH;	//mark bytes as read

							serial_buffer.tx[0] = CMD_WRITE_BASIC;
							serial_buffer.tx[1] = CMD_WRITE_BASIC_SUCCESS;
							serial_send_packet(serial_buffer.tx, 2);
						}
					}
					break;
/*				case CMD_LIGHT_ON:
					// TODO: turn ON the light
					buffer.tx[0] = 0x01;
					serial_send_packet(buffer.tx, 1);
					rd_ptr +=2;
					buffer.rd_ptr = rd_ptr;
					continue;
				case CMD_LIGHT_OFF:
					// TODO: turn OFF the light
					buffer.tx[0] = 0x01;
					serial_send_packet(buffer.tx, 1);
					rd_ptr +=2;
					buffer.rd_ptr = rd_ptr;
					continue; */
				default:
					++rd_ptr;
					serial_buffer.rd_ptr = rd_ptr;	//discard the CMD_WRITE byte as it wasn't followed by a supported command
					continue;
			}
		}

		if(serial_buffer.data[rd_ptr] == CMD_READ) {
			// start byte found
			uint8_t command = serial_buffer.data[rd_ptr + 1];

			switch (command) {
				case CMD_VERSION:
					serial_send_packet((uint8_t*)&controller_info, sizeof(controller_info_t));
					rd_ptr +=2;
					serial_buffer.rd_ptr = rd_ptr;	//mark bytes as read
					continue;
				case CMD_READ_BASIC:
					{
					commands_printf("CMD_READ_BASIC");

					volatile mc_configuration *mcconf = (volatile mc_configuration*) mc_interface_get_configuration();

					serial_buffer.tx[0] = 0x52;
					serial_buffer.tx[1] = CMD_READ_BASIC_LENGTH - 2;
					serial_buffer.tx[2] = (uint8_t) mcconf->l_min_vin;        //low battery protection [V]
					serial_buffer.tx[3] = (uint8_t) mcconf->l_current_max;    //current limit [A]

					for(int i=0; i<10; i++) {
						serial_buffer.tx[i+4] = (uint8_t) 20;    //assist level [%]
					}

					serial_buffer.tx[4] = 0;
					serial_buffer.tx[5] = 28;
					serial_buffer.tx[6] = 37;
					serial_buffer.tx[7] = 46;
					serial_buffer.tx[8] = 55;
					serial_buffer.tx[9] = 64;
					serial_buffer.tx[10] = 73;
					serial_buffer.tx[11] = 82;
					serial_buffer.tx[12] = 91;
					serial_buffer.tx[13] = 100;

					for(int i=0; i<10; i++) {
						serial_buffer.tx[i+14] = (uint8_t) 100;    //speed level [%]
					}

					serial_buffer.tx[24] = (uint8_t) (mcconf->si_wheel_diameter * 39.4 * 2.0); //wheel diameter [inch*2]
					serial_buffer.tx[25] = 0x01;	//speedometer [01 = internal]
					serial_buffer.tx[26] = checksum(serial_buffer.tx + 2, CMD_READ_BASIC_LENGTH - 3);

					for(int i = 0; i<=CMD_READ_BASIC_LENGTH;i++) {
						commands_printf("%02x ", serial_buffer.tx[i]);
					}
					serial_send_packet(serial_buffer.tx, CMD_READ_BASIC_LENGTH);
					rd_ptr +=2;
					serial_buffer.rd_ptr = rd_ptr;
					continue;
					}
				case CMD_AMPS:
					{
					float current = mc_interface_get_tot_current_in_filtered() * 2.0;
					utils_truncate_number(&current, 0, 256);
					serial_buffer.tx[0] = (uint8_t) (current);
					serial_buffer.tx[1] = serial_buffer.tx[0];
					serial_send_packet(serial_buffer.tx, 2);
					rd_ptr +=2;
					serial_buffer.rd_ptr = rd_ptr;	//mark bytes as read
					continue;
					}
				case CMD_WHEEL_RPM:
					{
#ifdef HW_HAS_WHEEL_SPEED_SENSOR
					// rpm = spd [m/s] / perim [m] * 60[s/min]
					const volatile mc_configuration *conf = mc_interface_get_configuration();
					uint16_t wheel_rpm = (uint16_t)(mc_interface_get_speed() / (M_PI * conf->si_wheel_diameter) * 60.0);

					serial_buffer.tx[0] = (uint8_t)(wheel_rpm / 256);
					serial_buffer.tx[1] = (uint8_t)(wheel_rpm & 0xFFFF);
					serial_buffer.tx[2] = serial_buffer.tx[0] + serial_buffer.tx[1] + 32;
					serial_send_packet(serial_buffer.tx, 3);
					rd_ptr +=2;
					serial_buffer.rd_ptr = rd_ptr;              
#endif
					continue;
					}
				case CMD_MOVING:
					if( mc_interface_get_speed() > 0.0) {
						serial_buffer.tx[0] = 0x31;	//0x31 0x31 indicates bike is in movement.
						serial_buffer.tx[1] = 0x31;
					}
					else {
						serial_buffer.tx[0] = 0x30;
						serial_buffer.tx[1] = 0x30;
					}
					serial_send_packet(serial_buffer.tx, 2);
					rd_ptr +=2;
					serial_buffer.rd_ptr = rd_ptr;
					continue;
				case CMD_PEDAL_MOV:
					// TODO: return pedal activity. 0: no activity
					serial_buffer.tx[0] = 0x01;
					serial_send_packet(serial_buffer.tx, 1);
					rd_ptr +=2;
					serial_buffer.rd_ptr = rd_ptr;
					continue;
				case CMD_BATT_SOC:
					// Battery state of charge 0-100%.
					{
					float wh_left = 0.0;
					float battery_level = mc_interface_get_battery_level(&wh_left) * 100.0;
					utils_truncate_number((float*)&battery_level, 0.0, 100.0);

					serial_buffer.tx[0] = (uint8_t) battery_level;
					serial_buffer.tx[1] = (uint8_t) battery_level;
					serial_send_packet(serial_buffer.tx, 2);
					rd_ptr +=2;
					serial_buffer.rd_ptr = rd_ptr;
					continue;
					}
				case CMD_BATT_VOLTAGE:
					{
					// This command was custom made for the eggrider display running at 72V
					uint16_t batt_voltage = (uint16_t)(GET_INPUT_VOLTAGE() ) * 100.0;
					serial_buffer.tx[0] = (uint8_t)(batt_voltage / 256);
					serial_buffer.tx[1] = (uint8_t)(batt_voltage & 0xFFFF);
					serial_buffer.tx[2] = serial_buffer.tx[0] + serial_buffer.tx[1] + 0x12;
					serial_send_packet(serial_buffer.tx, 3);
					rd_ptr +=2;
					serial_buffer.rd_ptr = rd_ptr;              
					continue;
					}
				default:
					++rd_ptr;
					serial_buffer.rd_ptr = rd_ptr;	//discard the CMD_READ byte as it wasn't followed by a supported command
					continue;
			}
		}
		rd_ptr++;
	}

	if(serial_buffer.rd_ptr > 0) {
		memmove(serial_buffer.data, serial_buffer.data + serial_buffer.rd_ptr, LUNA_SERIAL_BUFFER_SIZE - serial_buffer.rd_ptr);
		serial_buffer.wr_ptr -= serial_buffer.rd_ptr;
		serial_buffer.rd_ptr = 0;
	}
	if(serial_buffer.wr_ptr == (LUNA_SERIAL_BUFFER_SIZE - 1) ) {
		//shift buffer to the left discarding the oldest byte
		memmove(serial_buffer.data,serial_buffer.data + 1, LUNA_SERIAL_BUFFER_SIZE - 1);
		serial_buffer.wr_ptr -= 1;
	}
}

static void serial_display_check_rx(void){
	bool rx = true;
	while (rx) {
		rx = false;

		if (display_uart_is_running) {
			msg_t res = sdGetTimeout(&HW_UART_DEV, TIME_IMMEDIATE);
			if (res != MSG_TIMEOUT) {
				serial_display_byte_process(res);
				rx = true;
			}
		}
	}
}

static THD_FUNCTION(display_process_thread, arg) {
	(void)arg;
	chRegSetThreadName("Luna serial display");

	event_listener_t el;
	chEvtRegisterMaskWithFlags(&HW_UART_DEV.event, &el, EVENT_MASK(0), CHN_INPUT_AVAILABLE);

    // Wait for motor config initialization
	chThdSleepMilliseconds(500);

    // Set default power level
	set_assist_level(pas_level);

	for(;;) {
		chEvtWaitAnyTimeout(ALL_EVENTS, ST2MS(100));
		serial_display_check_rx();
		set_assist_level(pas_level); //assert periodically to make sure changes are commited when a new motor config is written
	}
}
#endif
