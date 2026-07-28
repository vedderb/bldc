/*
	Copyright 2022 Benjamin Vedder      benjamin@vedder.se
	Copyright 2023 Rasmus Söderhielm    rasmus.soderhielm@gmail.com

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

#ifndef MAIN_DATATYPES_H_
#define MAIN_DATATYPES_H_

#include <stdint.h>
#include <stdbool.h>

#define BMS_MAX_CELLS	50
#define BMS_MAX_TEMPS	50
#define BMS_STATUS_LEN	41

typedef struct {
	float v_tot;
	float v_charge;
	float i_in;
	float i_in_ic;
	float ah_cnt;
	float wh_cnt;
	int cell_num;
	float v_cell[BMS_MAX_CELLS];
	bool bal_state[BMS_MAX_CELLS];
	int temp_adc_num;
	float temps_adc[BMS_MAX_TEMPS];
	float temp_ic;
	float temp_hum;
	float hum;
	float pressure;
	float temp_max_cell;
	float v_cell_min;
	float v_cell_max;
	float soc;
	float soh;
	int can_id;
	float ah_cnt_chg_total;
	float wh_cnt_chg_total;
	float ah_cnt_dis_total;
	float wh_cnt_dis_total;
	int is_charging;
	int is_balancing;
	int is_charge_allowed;
	int data_version;
	char status[BMS_STATUS_LEN];
	uint32_t update_time;
} bms_values;

typedef struct {
	int id;
	uint32_t rx_time;
	float v_cell_min;
	float v_cell_max;
	float t_cell_max;
	float soc;
	float soh;
	bool is_charging;
	bool is_balancing;
	bool is_charge_allowed;
	int data_version;
} bms_soc_soh_temp_stat;

typedef enum {
	CAN_BAUD_125K = 0,
	CAN_BAUD_250K,
	CAN_BAUD_500K,
	CAN_BAUD_1M,
	CAN_BAUD_10K,
	CAN_BAUD_20K,
	CAN_BAUD_50K,
	CAN_BAUD_75K,
	CAN_BAUD_100K,
	CAN_BAUD_INVALID,
} CAN_BAUD;

typedef enum {
	WIFI_MODE_DISABLED = 0,
	WIFI_MODE_STATION,
	WIFI_MODE_ACCESS_POINT
} WIFI_MODE;

typedef enum {
	BLE_MODE_DISABLED = 0,
	BLE_MODE_OPEN,
	BLE_MODE_ENCRYPTED,
	BLE_MODE_SCRIPTING
} BLE_MODE;

typedef enum {
	HW_TYPE_VESC = 0,
	HW_TYPE_VESC_BMS,
	HW_TYPE_CUSTOM_MODULE
} HW_TYPE;

// Communication commands
typedef enum {
	COMM_FW_VERSION							= 0,
	COMM_JUMP_TO_BOOTLOADER					= 1,
	COMM_ERASE_NEW_APP						= 2,
	COMM_WRITE_NEW_APP_DATA					= 3,
	COMM_GET_VALUES							= 4,
	COMM_SET_DUTY							= 5,
	COMM_SET_CURRENT						= 6,
	COMM_SET_CURRENT_BRAKE					= 7,
	COMM_SET_RPM							= 8,
	COMM_SET_POS							= 9,
	COMM_SET_HANDBRAKE						= 10,
	COMM_SET_DETECT							= 11,
	COMM_SET_SERVO_POS						= 12,
	COMM_SET_MCCONF							= 13,
	COMM_GET_MCCONF							= 14,
	COMM_GET_MCCONF_DEFAULT					= 15,
	COMM_SET_APPCONF						= 16,
	COMM_GET_APPCONF						= 17,
	COMM_GET_APPCONF_DEFAULT				= 18,
	COMM_SAMPLE_PRINT						= 19,
	COMM_TERMINAL_CMD						= 20,
	COMM_PRINT								= 21,
	COMM_ROTOR_POSITION						= 22,
	COMM_EXPERIMENT_SAMPLE					= 23,
	COMM_DETECT_MOTOR_PARAM					= 24,
	COMM_DETECT_MOTOR_R_L					= 25,
	COMM_DETECT_MOTOR_FLUX_LINKAGE			= 26,
	COMM_DETECT_ENCODER						= 27,
	COMM_DETECT_HALL_FOC					= 28,
	COMM_REBOOT								= 29,
	COMM_ALIVE								= 30,
	COMM_GET_DECODED_PPM					= 31,
	COMM_GET_DECODED_ADC					= 32,
	COMM_GET_DECODED_CHUK					= 33,
	COMM_FORWARD_CAN						= 34,
	COMM_SET_CHUCK_DATA						= 35,
	COMM_CUSTOM_APP_DATA					= 36,
	COMM_NRF_START_PAIRING					= 37,
	COMM_GPD_SET_FSW						= 38,
	COMM_GPD_BUFFER_NOTIFY					= 39,
	COMM_GPD_BUFFER_SIZE_LEFT				= 40,
	COMM_GPD_FILL_BUFFER					= 41,
	COMM_GPD_OUTPUT_SAMPLE					= 42,
	COMM_GPD_SET_MODE						= 43,
	COMM_GPD_FILL_BUFFER_INT8				= 44,
	COMM_GPD_FILL_BUFFER_INT16				= 45,
	COMM_GPD_SET_BUFFER_INT_SCALE			= 46,
	COMM_GET_VALUES_SETUP					= 47,
	COMM_SET_MCCONF_TEMP					= 48,
	COMM_SET_MCCONF_TEMP_SETUP				= 49,
	COMM_GET_VALUES_SELECTIVE				= 50,
	COMM_GET_VALUES_SETUP_SELECTIVE			= 51,
	COMM_EXT_NRF_PRESENT					= 52,
	COMM_EXT_NRF_ESB_SET_CH_ADDR			= 53,
	COMM_EXT_NRF_ESB_SEND_DATA				= 54,
	COMM_EXT_NRF_ESB_RX_DATA				= 55,
	COMM_EXT_NRF_SET_ENABLED				= 56,
	COMM_DETECT_MOTOR_FLUX_LINKAGE_OPENLOOP	= 57,
	COMM_DETECT_APPLY_ALL_FOC				= 58,
	COMM_JUMP_TO_BOOTLOADER_ALL_CAN			= 59,
	COMM_ERASE_NEW_APP_ALL_CAN				= 60,
	COMM_WRITE_NEW_APP_DATA_ALL_CAN			= 61,
	COMM_PING_CAN							= 62,
	COMM_APP_DISABLE_OUTPUT					= 63,
	COMM_TERMINAL_CMD_SYNC					= 64,
	COMM_GET_IMU_DATA						= 65,
	COMM_BM_CONNECT							= 66,
	COMM_BM_ERASE_FLASH_ALL					= 67,
	COMM_BM_WRITE_FLASH						= 68,
	COMM_BM_REBOOT							= 69,
	COMM_BM_DISCONNECT						= 70,
	COMM_BM_MAP_PINS_DEFAULT				= 71,
	COMM_BM_MAP_PINS_NRF5X					= 72,
	COMM_ERASE_BOOTLOADER					= 73,
	COMM_ERASE_BOOTLOADER_ALL_CAN			= 74,
	COMM_PLOT_INIT							= 75,
	COMM_PLOT_DATA							= 76,
	COMM_PLOT_ADD_GRAPH						= 77,
	COMM_PLOT_SET_GRAPH						= 78,
	COMM_GET_DECODED_BALANCE				= 79,
	COMM_BM_MEM_READ						= 80,
	COMM_WRITE_NEW_APP_DATA_LZO				= 81,
	COMM_WRITE_NEW_APP_DATA_ALL_CAN_LZO		= 82,
	COMM_BM_WRITE_FLASH_LZO					= 83,
	COMM_SET_CURRENT_REL					= 84,
	COMM_CAN_FWD_FRAME						= 85,
	COMM_SET_BATTERY_CUT					= 86,
	COMM_SET_BLE_NAME						= 87,
	COMM_SET_BLE_PIN						= 88,
	COMM_SET_CAN_MODE						= 89,
	COMM_GET_IMU_CALIBRATION				= 90,
	COMM_GET_MCCONF_TEMP					= 91,

	// Custom configuration for hardware
	COMM_GET_CUSTOM_CONFIG_XML				= 92,
	COMM_GET_CUSTOM_CONFIG					= 93,
	COMM_GET_CUSTOM_CONFIG_DEFAULT			= 94,
	COMM_SET_CUSTOM_CONFIG					= 95,

	// BMS commands
	COMM_BMS_GET_VALUES						= 96,
	COMM_BMS_SET_CHARGE_ALLOWED				= 97,
	COMM_BMS_SET_BALANCE_OVERRIDE			= 98,
	COMM_BMS_RESET_COUNTERS					= 99,
	COMM_BMS_FORCE_BALANCE					= 100,
	COMM_BMS_ZERO_CURRENT_OFFSET			= 101,

	// FW updates commands for different HW types
	COMM_JUMP_TO_BOOTLOADER_HW				= 102,
	COMM_ERASE_NEW_APP_HW					= 103,
	COMM_WRITE_NEW_APP_DATA_HW				= 104,
	COMM_ERASE_BOOTLOADER_HW				= 105,
	COMM_JUMP_TO_BOOTLOADER_ALL_CAN_HW		= 106,
	COMM_ERASE_NEW_APP_ALL_CAN_HW			= 107,
	COMM_WRITE_NEW_APP_DATA_ALL_CAN_HW		= 108,
	COMM_ERASE_BOOTLOADER_ALL_CAN_HW		= 109,

	COMM_SET_ODOMETER						= 110,

	// Power switch commands
	COMM_PSW_GET_STATUS						= 111,
	COMM_PSW_SWITCH							= 112,

	COMM_BMS_FWD_CAN_RX						= 113,
	COMM_BMS_HW_DATA						= 114,
	COMM_GET_BATTERY_CUT					= 115,
	COMM_BM_HALT_REQ						= 116,
	COMM_GET_QML_UI_HW						= 117,
	COMM_GET_QML_UI_APP						= 118,
	COMM_CUSTOM_HW_DATA						= 119,
	COMM_QMLUI_ERASE						= 120,
	COMM_QMLUI_WRITE						= 121,

	// IO Board
	COMM_IO_BOARD_GET_ALL					= 122,
	COMM_IO_BOARD_SET_PWM					= 123,
	COMM_IO_BOARD_SET_DIGITAL				= 124,

	COMM_BM_MEM_WRITE						= 125,
	COMM_BMS_BLNC_SELFTEST					= 126,
	COMM_GET_EXT_HUM_TMP					= 127,
	COMM_GET_STATS							= 128,
	COMM_RESET_STATS						= 129,

	// Lisp
	COMM_LISP_READ_CODE						= 130,
	COMM_LISP_WRITE_CODE					= 131,
	COMM_LISP_ERASE_CODE					= 132,
	COMM_LISP_SET_RUNNING					= 133,
	COMM_LISP_GET_STATS						= 134,
	COMM_LISP_PRINT							= 135,

	COMM_BMS_SET_BATT_TYPE					= 136,
	COMM_BMS_GET_BATT_TYPE					= 137,

	COMM_LISP_REPL_CMD						= 138,
	COMM_LISP_STREAM_CODE					= 139,

	COMM_FILE_LIST							= 140,
	COMM_FILE_READ							= 141,
	COMM_FILE_WRITE							= 142,
	COMM_FILE_MKDIR							= 143,
	COMM_FILE_REMOVE						= 144,

	COMM_LOG_START							= 145,
	COMM_LOG_STOP							= 146,
	COMM_LOG_CONFIG_FIELD					= 147,
	COMM_LOG_DATA_F32						= 148,

	COMM_SET_APPCONF_NO_STORE				= 149,
	COMM_GET_GNSS							= 150,

	COMM_LOG_DATA_F64						= 151,

	COMM_LISP_RMSG							= 152,

	//Placeholders for pinlock commands
	//COMM_PINLOCK1							= 153,
	//COMM_PINLOCK2							= 154,
	//COMM_PINLOCK3							= 155,

	COMM_SHUTDOWN							= 156,

	COMM_FW_INFO							= 157,
	
	COMM_CAN_UPDATE_BAUD_ALL				= 158,
} COMM_PACKET_ID;

// CAN commands
typedef enum {
	CAN_PACKET_SET_DUTY						= 0,
	CAN_PACKET_SET_CURRENT					= 1,
	CAN_PACKET_SET_CURRENT_BRAKE			= 2,
	CAN_PACKET_SET_RPM						= 3,
	CAN_PACKET_SET_POS						= 4,
	CAN_PACKET_FILL_RX_BUFFER				= 5,
	CAN_PACKET_FILL_RX_BUFFER_LONG			= 6,
	CAN_PACKET_PROCESS_RX_BUFFER			= 7,
	CAN_PACKET_PROCESS_SHORT_BUFFER			= 8,
	CAN_PACKET_STATUS						= 9,
	CAN_PACKET_SET_CURRENT_REL				= 10,
	CAN_PACKET_SET_CURRENT_BRAKE_REL		= 11,
	CAN_PACKET_SET_CURRENT_HANDBRAKE		= 12,
	CAN_PACKET_SET_CURRENT_HANDBRAKE_REL	= 13,
	CAN_PACKET_STATUS_2						= 14,
	CAN_PACKET_STATUS_3						= 15,
	CAN_PACKET_STATUS_4						= 16,
	CAN_PACKET_PING							= 17,
	CAN_PACKET_PONG							= 18,
	CAN_PACKET_DETECT_APPLY_ALL_FOC			= 19,
	CAN_PACKET_DETECT_APPLY_ALL_FOC_RES		= 20,
	CAN_PACKET_CONF_CURRENT_LIMITS			= 21,
	CAN_PACKET_CONF_STORE_CURRENT_LIMITS	= 22,
	CAN_PACKET_CONF_CURRENT_LIMITS_IN		= 23,
	CAN_PACKET_CONF_STORE_CURRENT_LIMITS_IN	= 24,
	CAN_PACKET_CONF_FOC_ERPMS				= 25,
	CAN_PACKET_CONF_STORE_FOC_ERPMS			= 26,
	CAN_PACKET_STATUS_5						= 27,
	CAN_PACKET_POLL_TS5700N8501_STATUS		= 28,
	CAN_PACKET_CONF_BATTERY_CUT				= 29,
	CAN_PACKET_CONF_STORE_BATTERY_CUT		= 30,
	CAN_PACKET_SHUTDOWN						= 31,
	CAN_PACKET_IO_BOARD_ADC_1_TO_4			= 32,
	CAN_PACKET_IO_BOARD_ADC_5_TO_8			= 33,
	CAN_PACKET_IO_BOARD_ADC_9_TO_12			= 34,
	CAN_PACKET_IO_BOARD_DIGITAL_IN			= 35,
	CAN_PACKET_IO_BOARD_SET_OUTPUT_DIGITAL	= 36,
	CAN_PACKET_IO_BOARD_SET_OUTPUT_PWM		= 37,
	CAN_PACKET_BMS_V_TOT					= 38,
	CAN_PACKET_BMS_I						= 39,
	CAN_PACKET_BMS_AH_WH					= 40,
	CAN_PACKET_BMS_V_CELL					= 41,
	CAN_PACKET_BMS_BAL						= 42,
	CAN_PACKET_BMS_TEMPS					= 43,
	CAN_PACKET_BMS_HUM						= 44,
	CAN_PACKET_BMS_SOC_SOH_TEMP_STAT		= 45,
	CAN_PACKET_PSW_STAT						= 46,
	CAN_PACKET_PSW_SWITCH					= 47,
	CAN_PACKET_BMS_HW_DATA_1				= 48,
	CAN_PACKET_BMS_HW_DATA_2				= 49,
	CAN_PACKET_BMS_HW_DATA_3				= 50,
	CAN_PACKET_BMS_HW_DATA_4				= 51,
	CAN_PACKET_BMS_HW_DATA_5				= 52,
	CAN_PACKET_BMS_AH_WH_CHG_TOTAL			= 53,
	CAN_PACKET_BMS_AH_WH_DIS_TOTAL			= 54,
	CAN_PACKET_UPDATE_PID_POS_OFFSET		= 55,
	CAN_PACKET_POLL_ROTOR_POS				= 56,
	CAN_PACKET_NOTIFY_BOOT					= 57,
	CAN_PACKET_STATUS_6						= 58,
	CAN_PACKET_GNSS_TIME					= 59,
	CAN_PACKET_GNSS_LAT						= 60,
	CAN_PACKET_GNSS_LON						= 61,
	CAN_PACKET_GNSS_ALT_SPEED_HDOP			= 62,
	CAN_PACKET_UPDATE_BAUD					= 63,
	CAN_PACKET_BMS_STATUS_1					= 64,
	CAN_PACKET_BMS_STATUS_2					= 65,
	CAN_PACKET_BMS_STATUS_3					= 66,
	CAN_PACKET_BMS_STATUS_4					= 67,
	CAN_PACKET_BMS_STATUS_5					= 68,
	CAN_PACKET_MAKE_ENUM_32_BITS = 0x7FFFFFFF,
} CAN_PACKET_ID;

typedef struct {
	int id;
	uint32_t rx_time;
	float rpm;
	float current;
	float duty;
} can_status_msg;

typedef struct {
	int id;
	uint32_t rx_time;
	float amp_hours;
	float amp_hours_charged;
} can_status_msg_2;

typedef struct {
	int id;
	uint32_t rx_time;
	float watt_hours;
	float watt_hours_charged;
} can_status_msg_3;

typedef struct {
	int id;
	uint32_t rx_time;
	float temp_fet;
	float temp_motor;
	float current_in;
	float pid_pos_now;
} can_status_msg_4;

typedef struct {
	int id;
	uint32_t rx_time;
	float v_in;
	int32_t tacho_value;
} can_status_msg_5;

typedef struct {
	int id;
	uint32_t rx_time;
	float adc_1;
	float adc_2;
	float adc_3;
	float ppm;
} can_status_msg_6;

typedef struct {
	int id;
	uint32_t rx_time;
	float adc_voltages[4];
} io_board_adc_values;

typedef struct {
	int id;
	uint32_t rx_time;
	uint64_t inputs;
} io_board_digial_inputs;

typedef struct {
	int id;
	uint32_t rx_time;
	float v_in;
	float v_out;
	float temp;
	bool is_out_on;
	bool is_pch_on;
	bool is_dsc_on;
} psw_status;

#endif /* MAIN_DATATYPES_H_ */
