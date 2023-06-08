/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se

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

#include <string.h>
#include <math.h>
#include <stdio.h>

#include "canard_driver.h"
#include "canard.h"
#include "uavcan/equipment/esc/Status.h"
#include "uavcan/equipment/esc/RawCommand.h"
#include "uavcan/equipment/esc/RPMCommand.h"
#include "uavcan/protocol/param/GetSet.h"
#include "uavcan/protocol/GetNodeInfo.h"
#include "uavcan/protocol/RestartNode.h"
#include "uavcan/protocol/file/BeginFirmwareUpdate.h"
#include "uavcan/protocol/file/Read.h"
#include "vesc/RTData.h"

#include "conf_general.h"
#include "app.h"
#include "comm_can.h"
#include "commands.h"
#include "mc_interface.h"
#include "hw.h"
#include "timeout.h"
#include "terminal.h"
#include "mempools.h"
#include "flash_helper.h"
#include "crc.h"
#include "nrf_driver.h"
#include "buffer.h"
#include "utils.h"
#include "mcpwm_foc.h"
#include "imu.h"

// Constants
#define CAN_APP_NODE_NAME								"org.vesc." HW_NAME
#define UNIQUE_ID_LENGTH_BYTES							12
#define STATUS_MSGS_TO_STORE							10
#define AP_MAX_NAME_SIZE								20
#define ARRAY_SIZE(_arr) (sizeof(_arr) / sizeof(_arr[0]))

#define UAVCAN_PROTOCOL_DYNAMIC_NODE_ID_ALLOCATION_UNIQUE_ID_MAX_LENGTH 16

#define CURRENT_CALC_FREQ_HZ							10
#define PARAM_REFRESH_RATE_HZ                           10
#define ESC_STATUS_TIMEOUT								500 //ms?
#define RESERVED_FLASH_SPACE_SIZE                       393216

/*
 * Node status variables
 */
static uavcan_protocol_NodeStatus node_status;

// Private datatypes
typedef struct {
	int id;
	systime_t rx_time;
	bool timeout_flag;
	uavcan_equipment_esc_Status msg;
} status_msg_wrapper_t;

typedef struct {
	float rawval;
	float rpmval;
	systime_t rawtime;
	systime_t rpmtime;
} cmd_info_data;

// Private variables
static CanardInstance canard_ins;
static uint8_t canard_memory_pool[1024];
static cmd_info_data can1_cmd = {0};
#ifdef HW_CAN2_DEV
static cmd_info_data can2_cmd = {0};
static CanardInstance canard_ins_if2;
static uint8_t canard_memory_pool_if2[1024];
#endif
static uint8_t msg_buffer[512];
static uint8_t node_health = UAVCAN_PROTOCOL_NODESTATUS_HEALTH_OK;
static uint8_t node_mode = UAVCAN_PROTOCOL_NODESTATUS_MODE_OPERATIONAL;
static int debug_level;
static status_msg_wrapper_t stat_msgs[STATUS_MSGS_TO_STORE];
static bool refresh_parameters_enabled = true;

// Threads
static THD_WORKING_AREA(canard_thread_wa, 1024);
static THD_FUNCTION(canard_thread, arg);

// Private functions
static void calculateTotalCurrent(void);
static void sendEscStatus(CanardInstance *ins);
static void sendRtData(CanardInstance *ins);
static void readUniqueID(uint8_t* out_uid);
static void onTransferReceived(CanardInstance* ins, CanardRxTransfer* transfer);
static bool shouldAcceptTransfer(const CanardInstance* ins,
		uint64_t* out_data_type_signature,
		uint16_t data_type_id,
		CanardTransferType transfer_type,
		uint8_t source_node_id);
static void terminal_debug_on(int argc, const char **argv);

/*
* Firmware Update Stuff
*/
static struct {
	uint64_t ofs;
	uint32_t last_ms;
	uint8_t node_id;
	uint8_t transfer_id;
	uint8_t path[UAVCAN_PROTOCOL_FILE_PATH_PATH_MAX_LENGTH+1];
	uint8_t sector;
	uint32_t sector_ofs;
	CanardInstance *ins;
} fw_update;

systime_t last_read_file_req = 0;
systime_t jump_delay_start = 0;
bool jump_to_bootloader = false;

#define FLASH_SECTORS			12
#define BOOTLOADER_BASE			11
#define APP_BASE				0
#define APP_SECTORS				7
#define NEW_APP_BASE			8
#define NEW_APP_SECTORS			3
#define NEW_APP_MAX_SIZE		(3 * (1 << 17))

/*
 * Base address of the Flash sectors
 * May be able to remove these and only use the address for NEW_APP_BASE and APP_BASE instead
 */
#define ADDR_FLASH_SECTOR_0     ((uint32_t)0x08000000) // Base @ of Sector 0, 16 Kbytes
#define ADDR_FLASH_SECTOR_1     ((uint32_t)0x08004000) // Base @ of Sector 1, 16 Kbytes
#define ADDR_FLASH_SECTOR_2     ((uint32_t)0x08008000) // Base @ of Sector 2, 16 Kbytes
#define ADDR_FLASH_SECTOR_3     ((uint32_t)0x0800C000) // Base @ of Sector 3, 16 Kbytes
#define ADDR_FLASH_SECTOR_4     ((uint32_t)0x08010000) // Base @ of Sector 4, 64 Kbytes
#define ADDR_FLASH_SECTOR_5     ((uint32_t)0x08020000) // Base @ of Sector 5, 128 Kbytes
#define ADDR_FLASH_SECTOR_6     ((uint32_t)0x08040000) // Base @ of Sector 6, 128 Kbytes
#define ADDR_FLASH_SECTOR_7     ((uint32_t)0x08060000) // Base @ of Sector 7, 128 Kbytes
#define ADDR_FLASH_SECTOR_8     ((uint32_t)0x08080000) // Base @ of Sector 8, 128 Kbytes
#define ADDR_FLASH_SECTOR_9     ((uint32_t)0x080A0000) // Base @ of Sector 9, 128 Kbytes
#define ADDR_FLASH_SECTOR_10    ((uint32_t)0x080C0000) // Base @ of Sector 10, 128 Kbytes
#define ADDR_FLASH_SECTOR_11    ((uint32_t)0x080E0000) // Base @ of Sector 11, 128 Kbytes

static const uint32_t flash_addr[FLASH_SECTORS] = {
	ADDR_FLASH_SECTOR_0,
	ADDR_FLASH_SECTOR_1,
	ADDR_FLASH_SECTOR_2,
	ADDR_FLASH_SECTOR_3,
	ADDR_FLASH_SECTOR_4,
	ADDR_FLASH_SECTOR_5,
	ADDR_FLASH_SECTOR_6,
	ADDR_FLASH_SECTOR_7,
	ADDR_FLASH_SECTOR_8,
	ADDR_FLASH_SECTOR_9,
	ADDR_FLASH_SECTOR_10,
	ADDR_FLASH_SECTOR_11
};

/* 
 * Parameter types and enums 
 */
typedef struct
{
	char* name;
	uint8_t type;
	int64_t val;
	int64_t min;
	int64_t max;
	int64_t defval;
} param_t;

enum ap_var_type {
	AP_PARAM_NONE    = 0,
	AP_PARAM_INT8,
	AP_PARAM_INT16,
	AP_PARAM_INT32,
	AP_PARAM_FLOAT,
	AP_PARAM_VECTOR3F,
	AP_PARAM_GROUP
};

/*
 * Private Parameter function declarations
 */
static void updateParamByName(uint8_t * name, float value);
static void refresh_parameters(void);
static param_t* getParamByIndex(uint16_t index);
static param_t* getParamByName(char * name);
static void write_app_config(void);

/*
 * Local parameter table
 * This table contains the parameters we want to be able to change via UAVCAN. It is a copy of the 
 * appconf parameters with more information, the format of each parameter is as follows:
 * { parameter name, parameter type, current value, min value, max value, default value}
 */
static param_t parameters[] =
{
	{"can_baud_rate", 		AP_PARAM_INT8,   0,   0,   8,   CAN_BAUD_500K},
	{"can_status_rate_1",	AP_PARAM_INT32,  0,   0, 1000,  50},
	{"can_status_rate_2",	AP_PARAM_INT32,  0,   0, 1000,  5},
	{"can_status_msgs_r1",	AP_PARAM_INT16,  0,   0,   255,   0},
	{"can_status_msgs_r2",	AP_PARAM_INT16,  0,   0,   255,   0},
	{"can_esc_index",   	AP_PARAM_INT16,  0,   0, 255,   0},
	{"controller_id",   	AP_PARAM_INT16,  0,   0, 253,   0},
	{"ctl_dir",         	AP_PARAM_INT8,   0,   0, 1,     0}
};

/*
 * This function updates the local parameter value. It is called after reading the current appconf
 * data to update the local copy of the parameter.
 */
static void updateParamByName(uint8_t * name, float value) 
{
	param_t* p = NULL;
	p = getParamByName((char *)name);
	if (p != NULL) {
		if (p->val != value) {
			if (debug_level > 0) {
				commands_printf("%s, %s p->val %0.02f, value %0.02f", p->name, name, (double)p->val, (double)value);
			}
			p->val = value;
		}
	} else {
		if (debug_level > 0) {
			commands_printf("UAVCAN updateParamByName(): Parameter name not found");
		}
	}
}

/*
 * This function updates the app config from the local app config. It is used to 
 * write new parameters after a set parameter request.
 */
static void write_app_config(void) {
	app_configuration *appconf = mempools_alloc_appconf();
	*appconf = *app_get_configuration();

	mc_configuration *mcconf = mempools_alloc_mcconf();
	*mcconf = *mc_interface_get_configuration();

	appconf->can_baud_rate = (uint8_t)getParamByName("can_baud_rate")->val;
	appconf->can_status_rate_1 = (uint32_t)getParamByName("can_status_rate_1")->val;
	appconf->can_status_rate_2 = (uint32_t)getParamByName("can_status_rate_2")->val;
	appconf->can_status_msgs_r1 = (uint16_t)getParamByName("can_status_msgs_r1")->val;
	appconf->can_status_msgs_r2 = (uint16_t)getParamByName("can_status_msgs_r2")->val;
	appconf->uavcan_esc_index = (uint16_t)getParamByName("can_esc_index")->val;
	appconf->controller_id = (uint16_t)getParamByName("controller_id")->val;
	mcconf->m_invert_direction = (uint8_t)getParamByName("ctl_dir")->val;;

   	conf_general_store_app_configuration(appconf);
   	app_set_configuration(appconf);

   	conf_general_store_mc_configuration(mcconf, mc_interface_get_motor_thread() == 2);
   	mc_interface_set_configuration(mcconf);

	mempools_free_appconf(appconf);
	mempools_free_mcconf(mcconf);

	refresh_parameters_enabled = true;
}

/*
 * This function updates the local copy of the parameters from the appconf data
 * This is called periodically so that the paremters are kept in sync. Care has
 * to be taken when a set parameter request is recieved so that the parameter is 
 * not overwritten before it is actually writen to the emulated EEPROM.
 */
static void refresh_parameters(void){
	const app_configuration *appconf = app_get_configuration();
	const volatile mc_configuration *mcconf = mc_interface_get_configuration();

	updateParamByName((uint8_t *)"can_baud_rate", 		appconf->can_baud_rate);
	updateParamByName((uint8_t *)"can_status_rate_1",	appconf->can_status_rate_1);
	updateParamByName((uint8_t *)"can_status_rate_2",	appconf->can_status_rate_2);
	updateParamByName((uint8_t *)"can_status_msgs_r1",	appconf->can_status_msgs_r1);
	updateParamByName((uint8_t *)"can_status_msgs_r2",	appconf->can_status_msgs_r2);
	updateParamByName((uint8_t *)"can_esc_index",   	appconf->uavcan_esc_index);
	updateParamByName((uint8_t *)"controller_id",   	appconf->controller_id);
	updateParamByName((uint8_t *)"ctl_dir",			   	mcconf->m_invert_direction);
}

/*
 * Get parameter by index
 * Retrieves the parameter with the given index if found
 * if not found it returns null
 */
static param_t* getParamByIndex(uint16_t index)
{
	if (index >= (sizeof(parameters) / sizeof(parameters[0])))
	{
		if (debug_level == 2) {
			commands_printf("Index is out of range");
		}
		return NULL;
	}
	if (debug_level == 2) {
		commands_printf("Size of parameter array is: %d", (sizeof(parameters) / sizeof(parameters[0])));
	}
	return &parameters[index];
}

/*
 * Get parameter by name
 * Searches the parameter table for the given name and returns the parameter information 
 * if no parameter is found it returns null.
 */
static param_t* getParamByName(char * name)
{
	for (uint16_t i = 0; i < (sizeof(parameters) / sizeof(parameters[0])); i++)
	{
		if (debug_level == 2) {
			commands_printf("name: %s paramname: %s", name, parameters[i].name);
		}
		
		if (strcmp(name, parameters[i].name) == 0)
		{
			if (debug_level == 2) {
				commands_printf("found match!");
			}
			return &parameters[i];
		}
	} 
	return NULL;
}

/*
 * UAVCAN Driver Init
 */
void canard_driver_init(void) {
	debug_level = 0;

	memset(&can1_cmd, 0, sizeof(can1_cmd));

#ifdef HW_CAN2_DEV
	memset(&can2_cmd, 0, sizeof(can2_cmd));
#endif

	for (int i = 0;i < STATUS_MSGS_TO_STORE;i++) {
		stat_msgs[i].id = -1;
	}

	chThdCreateStatic(canard_thread_wa, sizeof(canard_thread_wa), NORMALPRIO, canard_thread, NULL);

	terminal_register_command_callback(
		"uavcan_debug",
		"Enable UAVCAN debug prints 0: off 1: errors 2: param getset 3: current calc 4: comms stuff)",
		"[level]",
		terminal_debug_on);
}

uavcan_cmd_info canard_driver_last_rawcmd(int can_if) {
	uavcan_cmd_info res = {0};
	res.age = UTILS_AGE_S(0);

	if (can_if == 1) {
		res.value = can1_cmd.rawval;
		res.age = UTILS_AGE_S(can1_cmd.rawtime);
	}

#ifdef HW_CAN2_DEV
	if (can_if == 2) {
		res.value = can2_cmd.rawval;
		res.age = UTILS_AGE_S(can2_cmd.rawtime);
	}
#endif

	return res;
}

uavcan_cmd_info canard_driver_last_rpmcmd(int can_if) {
	uavcan_cmd_info res = {0};
	res.age = UTILS_AGE_S(0);

	if (can_if == 1) {
		res.value = can1_cmd.rpmval;
		res.age = UTILS_AGE_S(can1_cmd.rpmtime);
	}

#ifdef HW_CAN2_DEV
	if (can_if == 2) {
		res.value = can2_cmd.rpmval;
		res.age = UTILS_AGE_S(can2_cmd.rpmtime);
	}
#endif

	return res;
}

/*
 * Calculate the total system current consumption based on the reported consumption by other
 * ESCs transmitting data on the bus.
 */
static void calculateTotalCurrent(void) {
	// Calculate total current being consumed by the ESCs on the system
	float totalCurrent = mc_interface_get_tot_current();
	float avgVoltage = mc_interface_get_input_voltage_filtered();
	float totalSysCurrent = 0;

	uint8_t escTotal = 1;
	
	for (int i = 0;i < STATUS_MSGS_TO_STORE;i++) {
		status_msg_wrapper_t *msgw = &stat_msgs[i];
		if (msgw->id != -1) {
			systime_t elapsedTime = chVTGetSystemTimeX() - msgw->rx_time;
			if(ST2MS(elapsedTime) > ESC_STATUS_TIMEOUT) {
				if (debug_level > 0) {
					commands_printf("ESC timeout for NodeID: %d",msgw->id);
				}
				msgw->id = -1;
			} else {
				totalCurrent += msgw->msg.current;
				totalSysCurrent += msgw->msg.current;
				escTotal++;
				avgVoltage += msgw->msg.voltage;
			}
		}
	}

	avgVoltage = avgVoltage / escTotal;
	if(debug_level == 3) {
		commands_printf("Total number of nodes: %d", escTotal);
		commands_printf("Total Current: %0.02f", (double)totalCurrent);
		commands_printf("Total Sys Curr: %0.02f", (double)totalSysCurrent);
		commands_printf("Average Voltage: %0.02f", (double)avgVoltage);
	}

	// ToDo: Add a way to limit the ESC current based on the total system current consumed
}

/*
* Send Node Status Message
*/
static void sendNodeStatus(CanardInstance *ins) {
	node_mode = fw_update.node_id?UAVCAN_PROTOCOL_NODESTATUS_MODE_SOFTWARE_UPDATE:UAVCAN_PROTOCOL_NODESTATUS_MODE_OPERATIONAL;
	
	node_status.health = node_health;
	node_status.mode = node_mode;
	node_status.uptime_sec = (uint32_t)ST2S(chVTGetSystemTimeX());
	// status.sub_mode =
	// status.vendor_specific_status_code is filled in the firmware update loop
	uavcan_protocol_NodeStatus_encode(&node_status, msg_buffer);
	static uint8_t transfer_id;
	canardBroadcast(ins,
		UAVCAN_PROTOCOL_NODESTATUS_SIGNATURE,
		UAVCAN_PROTOCOL_NODESTATUS_ID,
		&transfer_id,
		CANARD_TRANSFER_PRIORITY_LOW,
		msg_buffer,
		UAVCAN_PROTOCOL_NODESTATUS_MAX_SIZE);
}

/*
 * Send ESC Status Message
 */
static void sendEscStatus(CanardInstance *ins) {
	uavcan_equipment_esc_Status status;
	memset(&status, 0, sizeof(status));

	const volatile mc_configuration *conf = mc_interface_get_configuration();
	const app_configuration *appconf = app_get_configuration();

	if (appconf->uavcan_status_current_mode == UAVCAN_STATUS_CURRENT_MODE_MOTOR) {
		status.current = mc_interface_get_tot_current_filtered();
	} else {
		status.current = mc_interface_get_tot_current_in_filtered();
	}

	status.error_count = mc_interface_get_fault();
	status.esc_index = app_get_configuration()->uavcan_esc_index;
	status.power_rating_pct = (fabsf(mc_interface_get_tot_current()) /
			conf->l_current_max * conf->l_current_max_scale) * 100.0;
	status.rpm = mc_interface_get_rpm() / ((float)conf->si_motor_poles / 2.0);
	status.temperature = fmaxf(mc_interface_temp_fet_filtered(), mc_interface_temp_motor_filtered()) + 273.15;
	status.voltage = mc_interface_get_input_voltage_filtered();

	uavcan_equipment_esc_Status_encode(&status, msg_buffer);

	static uint8_t transfer_id;

	if (debug_level > 11) {
		commands_printf("UAVCAN sendESCStatus");
	}

	canardBroadcast(ins,
		UAVCAN_EQUIPMENT_ESC_STATUS_SIGNATURE,
		UAVCAN_EQUIPMENT_ESC_STATUS_ID,
		&transfer_id,
		CANARD_TRANSFER_PRIORITY_LOW,
		msg_buffer,
		UAVCAN_EQUIPMENT_ESC_STATUS_MAX_SIZE);
}

static void sendRtData(CanardInstance *ins) {
	vesc_RTData data;
	memset(&data, 0, sizeof(data));

	const volatile mc_configuration *conf = mc_interface_get_configuration();
	const app_configuration *appconf = app_get_configuration();

	data.volt_in = mc_interface_get_input_voltage_filtered();
	data.volt_d = mcpwm_foc_get_vd();
	data.volt_q = mcpwm_foc_get_vq();

	data.temp_mos_max = mc_interface_temp_fet_filtered();
	data.temp_mos_1 = NTC_TEMP_MOS1();
	data.temp_mos_2 = NTC_TEMP_MOS2();
	data.temp_mos_3 = NTC_TEMP_MOS3();
	data.temp_motor_max = mc_interface_temp_motor_filtered();
	data.temp_motor_1 = TEMP_MOTOR_1(conf->m_ntc_motor_beta);
	data.temp_motor_2 = TEMP_MOTOR_2(conf->m_ntc_motor_beta);

	data.curr_motor = mc_interface_get_tot_current_filtered();
	data.curr_in = mc_interface_get_tot_current_in_filtered();
	data.curr_d = mcpwm_foc_get_id();
	data.curr_q = mcpwm_foc_get_iq();

	float rpy[3], acc[3], gyro[3];
	imu_get_rpy(rpy);
	imu_get_accel(acc);
	imu_get_gyro(gyro);

	data.roll = rpy[0];
	data.pitch = rpy[1];
	data.yaw = rpy[2];
	data.acc_x = acc[0];
	data.acc_y = acc[1];
	data.acc_z = acc[2];
	data.gyro_x = gyro[0];
	data.gyro_y = gyro[1];
	data.gyro_z = gyro[2];

	data.erpm = mc_interface_get_rpm();
	data.rpm = mc_interface_get_rpm() / ((float)conf->si_motor_poles / 2.0);
	data.duty = mc_interface_get_duty_cycle_now();
	data.ah_used = mc_interface_get_amp_hours(false);
	data.ah_charged = mc_interface_get_amp_hours_charged(false);
	data.wh_used = mc_interface_get_watt_hours(false);
	data.wh_charged = mc_interface_get_watt_hours_charged(false);
	data.encoder_pos = mc_interface_get_pid_pos_now();
	float wh_left = 0.0;
	data.battery_level = mc_interface_get_battery_level(&wh_left);
	data.battery_wh_tot = wh_left / data.battery_level;
	data.fault_code = mc_interface_get_fault();
	data.vesc_id = appconf->controller_id;

	vesc_RTData_encode(&data, msg_buffer, false);

	static uint8_t transfer_id;

	if (debug_level > 11) {
		commands_printf("UAVCAN sendRtData");
	}

	canardBroadcast(ins,
			VESC_RTDATA_SIGNATURE,
			VESC_RTDATA_ID,
			&transfer_id,
			CANARD_TRANSFER_PRIORITY_LOW,
			msg_buffer,
			VESC_RTDATA_MAX_SIZE);
}

/*
 * Reads the STM32 Unique ID
 */
static void readUniqueID(uint8_t* out_uid) {
	uint8_t len = UAVCAN_PROTOCOL_DYNAMIC_NODE_ID_ALLOCATION_UNIQUE_ID_MAX_LENGTH;
	memset(out_uid, 0, len);
	memcpy(out_uid, (const void *)STM32_UUID_8, UNIQUE_ID_LENGTH_BYTES);
}

/*
 * Handle a GET_NODE_INFO request
 * FW_VERSION is from conf_general.h
 * HW_VERSION is from HW_HEADER if HW_MAJOR/HW_MINOR are defined, else it sends 0
 *     This needs a change to all HW_HEADER files if desired.
 * Unique ID is now being read from the STM32 UUID field
 * Node status is syncronize with the data from the node status message. 
 */
static void handle_get_node_info(CanardInstance* ins, CanardRxTransfer* transfer) {
	uavcan_protocol_GetNodeInfoResponse pkt;
	memset(&pkt, 0, sizeof(pkt));

	node_status.uptime_sec = ST2S(chVTGetSystemTimeX());

	pkt.status = node_status;
	pkt.software_version.major = FW_VERSION_MAJOR;
	pkt.software_version.minor = FW_VERSION_MINOR;
	pkt.software_version.optional_field_flags = UAVCAN_PROTOCOL_SOFTWAREVERSION_OPTIONAL_FIELD_FLAG_VCS_COMMIT | UAVCAN_PROTOCOL_SOFTWAREVERSION_OPTIONAL_FIELD_FLAG_IMAGE_CRC;
	pkt.software_version.vcs_commit = FW_TEST_VERSION_NUMBER;
	uint32_t *crc = (uint32_t *)&pkt.software_version.image_crc;
	crc[0] = 0;
	crc[1] = 0;

	readUniqueID(pkt.hardware_version.unique_id);

	// use hw major/minor for APJ_BOARD_ID so we know what fw is
	// compatible with this hardware
#ifdef HW_MAJOR
	pkt.hardware_version.major = HW_MAJOR;
	pkt.hardware_version.minor = HW_MINOR;
#else
	pkt.hardware_version.major = 0;
	pkt.hardware_version.minor = 0;
#endif

	char name[strlen(CAN_APP_NODE_NAME)+1];
	strcpy(name, CAN_APP_NODE_NAME);
	pkt.name.len = strlen(CAN_APP_NODE_NAME);
	pkt.name.data = (uint8_t *)name;

	uint16_t total_size = uavcan_protocol_GetNodeInfoResponse_encode(&pkt, msg_buffer);

	const int16_t resp_res = canardRequestOrRespond(ins,
													transfer->source_node_id,
													UAVCAN_PROTOCOL_GETNODEINFO_SIGNATURE,
													UAVCAN_PROTOCOL_GETNODEINFO_ID,
													&transfer->transfer_id,
													transfer->priority,
													CanardResponse,
													msg_buffer,
													total_size);
	if (resp_res <= 0) {
		if (debug_level > 1) {
			commands_printf("Could not respond to GetNodeInfo: %d\n", resp_res);
		}
	}
}

/*
 * Handle ESC Raw command
 */
static void handle_esc_raw_command(CanardInstance* ins, CanardRxTransfer* transfer) {
	(void)ins;

	uavcan_equipment_esc_RawCommand cmd;
	memset(&cmd, 0, sizeof(cmd));

	uint8_t *tmp = msg_buffer;

	if (uavcan_equipment_esc_RawCommand_decode_internal(transfer, transfer->payload_len, &cmd, &tmp, 0) >= 0) {
		if (cmd.cmd.len > app_get_configuration()->uavcan_esc_index) {
			float raw_val = ((float)cmd.cmd.data[app_get_configuration()->uavcan_esc_index]) / 8192.0;

			if (ins == &canard_ins) {
				can1_cmd.rawtime = chVTGetSystemTimeX();
				can1_cmd.rawval = raw_val;
			}

#ifdef HW_CAN2_DEV
			if (ins == &canard_ins_if2) {
				can2_cmd.rawtime = chVTGetSystemTimeX();
				can2_cmd.rawval = raw_val;
			}
#endif

			volatile const app_configuration *conf = app_get_configuration();

			app_disable_output(100);

			switch (conf->uavcan_raw_mode) {
				case UAVCAN_RAW_MODE_CURRENT:
					mc_interface_set_current_rel(raw_val);
					break;

				case UAVCAN_RAW_MODE_CURRENT_NO_REV_BRAKE:
					if (raw_val >= 0.0) {
						mc_interface_set_current_rel(raw_val);
					} else {
						mc_interface_set_brake_current_rel(-raw_val);
					}
					break;

				case UAVCAN_RAW_MODE_DUTY:
					mc_interface_set_duty(raw_val);
					break;

				case UAVCAN_RAW_MODE_RPM:
					mc_interface_set_pid_speed(raw_val * conf->uavcan_raw_rpm_max);
					break;

				default:
					break;
			}
			timeout_reset();
		}
	}
}

/*
 * Handle ESC RPM command
 */
static void handle_esc_rpm_command(CanardInstance* ins, CanardRxTransfer* transfer) {
	(void)ins;

	uavcan_equipment_esc_RPMCommand cmd;
	memset(&cmd, 0, sizeof(cmd));

	uint8_t *tmp = msg_buffer;

	if (uavcan_equipment_esc_RPMCommand_decode_internal(transfer, transfer->payload_len, &cmd, &tmp, 0) >= 0) {
		if (cmd.rpm.len > app_get_configuration()->uavcan_esc_index) {
			float rpm_val = cmd.rpm.data[app_get_configuration()->uavcan_esc_index];

			if (ins == &canard_ins) {
				can1_cmd.rpmtime = chVTGetSystemTimeX();
				can1_cmd.rpmval = rpm_val;
			}

#ifdef HW_CAN2_DEV
			if (ins == &canard_ins_if2) {
				can2_cmd.rpmtime = chVTGetSystemTimeX();
				can2_cmd.rpmval = rpm_val;
			}
#endif

			mc_interface_set_pid_speed(rpm_val);
			timeout_reset();
		}
	}
}

/*
 * Handle Equipment ESC Status Request
 * Reads status messages from other ESCs on the system and stores the information in memory
 * this data can be used to respond to total system current consumption.
 */
static void handle_esc_status(CanardInstance* ins, CanardRxTransfer* transfer) {
	(void)ins;

	uavcan_equipment_esc_Status msg;
	memset(&msg, 0, sizeof(msg));

	if (uavcan_equipment_esc_Status_decode_internal(transfer, transfer->payload_len, &msg, 0, 0) >= 0) {
		for (int i = 0;i < STATUS_MSGS_TO_STORE;i++) {
			status_msg_wrapper_t *msgw = &stat_msgs[i];
			if (msgw->id == -1 || msgw->id == transfer->source_node_id) {
				msgw->id = transfer->source_node_id;
				msgw->rx_time = chVTGetSystemTimeX();
				msgw->msg = msg;
				break;
			}
		}
	}
}

/*
 * Handle parameter GetSet request
 * In UAVCAN parameters are requested individually either by index or by name
 * VESC currently does not have a way to write parameters individually or a way to 
 * access the parameters by name. So I needed to create a bridge between the parameters
 * and the UAVCAN driver which was not very clean and creates too much overhead if we 
 * want to access a large number of parameters. For that reason only a few paremeters 
 * were added here to allow changing of the most basic features. 
 * For now any tunning that needs to be done could be done using the USB interface
 * parameters can be changed and if it needs to be deployed on the field to other
 * ESCs that are using a UAVCAN interface then a special firmware with the defaults
 * modified can be create and deployed by a UAVCAN firmware update
 */
static void handle_param_getset(CanardInstance* ins, CanardRxTransfer* transfer)
{
	uavcan_protocol_param_GetSetRequest req;
	memset(&req, 0, sizeof(req));

	uint8_t *arraybuf_ptr = msg_buffer;
	if (uavcan_protocol_param_GetSetRequest_decode(transfer, transfer->payload_len, &req, &arraybuf_ptr) < 0) {
		return;
	}

	uavcan_protocol_param_GetSetResponse pkt;

	memset(&pkt, 0, sizeof(uavcan_protocol_param_GetSetResponse));

	char name[AP_MAX_NAME_SIZE+1] = "";

	param_t* p = NULL;

	// Verify the request is valid and try to get the parameter if it is.
	if (req.name.len != 0 && req.name.len > AP_MAX_NAME_SIZE) {
		if (debug_level > 1) {
			commands_printf("UAVCAN param_getset: Parameter Name is too long!");
		}
		p = NULL;
	} else if (req.name.len != 0 && req.name.len <= AP_MAX_NAME_SIZE) {
		strncpy((char *)name, (char *)req.name.data, req.name.len);
		if (debug_level > 1) {
			commands_printf("UAVCAN param_getset param name: %s", name);
		}
		p = getParamByName(name);
	} else {
		if (debug_level > 1) {
			commands_printf("UAVCAN param_getset param index: %d", req.index);
		}
		p = getParamByIndex(req.index);
	}

	// If a valid parameter was retrived...
	if ((p != NULL) && (debug_level > 1)) {
		uint8_t arrSize = strlen(p->name);
		commands_printf("UAVCAN param_getset got\nname: %s\nsize: %d\ntype: %d\nvalue: %d", (char *)p->name, arrSize, p->type, p->val);
	}

	// If a valid parameter was retrived and this is a set parameter request...
	if (p != NULL && req.name.len != 0 && req.value.union_tag != UAVCAN_PROTOCOL_PARAM_VALUE_EMPTY) {
		// Set request and valid parameter found
		// Prevent overwrite of local parameter copy before its written to eeprom
		refresh_parameters_enabled = false;

		switch (req.value.union_tag) {
			case UAVCAN_PROTOCOL_PARAM_VALUE_EMPTY:
				return;
			break;

			case UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE:
				switch(p->type) {
					case AP_PARAM_INT8:
						p->val = (uint8_t)req.value.integer_value;
					break;

					case AP_PARAM_INT16:
						p->val = (uint16_t)req.value.integer_value;
					break;

					case AP_PARAM_INT32:
						p->val = (uint32_t)req.value.integer_value;
					break;

					case AP_PARAM_FLOAT:
						p->val = (float)req.value.integer_value;
					break;

					default:
					break;
				}
			break;

			default:
			break;
		}
		write_app_config();
	}

	// If a valid parameter was retrived send back the value
	if(p != NULL) {
		switch(p->type) {
			case AP_PARAM_INT8:
				pkt.value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.default_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.min_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.max_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.value.integer_value = (int8_t)p->val;
				pkt.default_value.integer_value = (int8_t)p->defval;
				pkt.min_value.integer_value = (int8_t)p->min;
				pkt.max_value.integer_value = (int8_t)p->max;
			break;

			case AP_PARAM_INT16:
				pkt.value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.default_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.min_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.max_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.value.integer_value = (int16_t)p->val;
				pkt.default_value.integer_value = (int16_t)p->defval;
				pkt.min_value.integer_value = (int16_t)p->min;
				pkt.max_value.integer_value = (int16_t)p->max;
			break;

			case AP_PARAM_INT32:
				pkt.value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.default_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.min_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.max_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_INTEGER_VALUE;
				pkt.value.integer_value = (int32_t)p->val;
				pkt.default_value.integer_value = (int32_t)p->defval;
				pkt.min_value.integer_value = (int32_t)p->min;
				pkt.max_value.integer_value = (int32_t)p->max;
			break;

			case AP_PARAM_FLOAT:
				pkt.value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_REAL_VALUE;
				pkt.default_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_REAL_VALUE;
				pkt.min_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_REAL_VALUE;
				pkt.max_value.union_tag = UAVCAN_PROTOCOL_PARAM_VALUE_REAL_VALUE;
				pkt.value.real_value = (float)p->val;
				pkt.default_value.real_value = (float)p->defval;
				pkt.min_value.real_value = (float)p->min;
				pkt.max_value.real_value = (float)p->max;
			break;
		}

		pkt.name.len = strlen((char *)p->name);
		pkt.name.data = (uint8_t *)p->name;
	}

	if (debug_level == 2) {
		commands_printf("UAVCAN param_getset: Sending %s", pkt.name.data);
	}
	uint16_t total_size = uavcan_protocol_param_GetSetResponse_encode(&pkt, msg_buffer);

	const int16_t resp_res = canardRequestOrRespond(ins,
													transfer->source_node_id,
													UAVCAN_PROTOCOL_PARAM_GETSET_SIGNATURE,
													UAVCAN_PROTOCOL_PARAM_GETSET_ID,
													&transfer->transfer_id,
													transfer->priority,
													CanardResponse,
													&msg_buffer[0],
													total_size);

	if ((resp_res <= 0) && (debug_level > 1)) {
		commands_printf("Could not respond to param_getset_req: %d\n", resp_res);
	}												
}

/*
 * Create a Watchdog reset in order to restart the node if a restart command is recieved
 */
static void handle_restart_node(void) {
	// Lock the system and enter an infinite loop. The watchdog will reboot.
	__disable_irq();
	for(;;){};
}

/*
 * Send a read for a fw update file
 * This request is sent after we recieve a begin firmware udpate request, and then
 * everytime we finish processing one chunk of the firmware file. It uses the 
 * fw_update.ofs value to keep track of what chunk of data needs to be requested.
 * We use fw_update.last_ms to give the server enough time to respond to the previous
 * request before we send a new one. The value is cleared after procesing a response
 * so that we dont have to wait 250ms before sending the next request.
 */
static void send_fw_read(CanardInstance *ins)
{
	fw_update.ins = ins;

	uint32_t now = ST2MS(chVTGetSystemTimeX());
	if (now - fw_update.last_ms < 250) {
		// the server may still be responding
		return;
	}
	fw_update.last_ms = now;

	canardEncodeScalar(msg_buffer, 0, 40, &fw_update.ofs);
	uint32_t offset = 40;
	uint8_t len = strlen((const char *)fw_update.path);
	for (uint8_t i=0; i<len; i++) {
		canardEncodeScalar(msg_buffer, offset, 8, &fw_update.path[i]);
		offset += 8;
	}
	uint32_t total_size = (offset+7)/8;

	canardRequestOrRespond(ins,
						   fw_update.node_id,
						   UAVCAN_PROTOCOL_FILE_READ_SIGNATURE,
						   UAVCAN_PROTOCOL_FILE_READ_ID,
						   &fw_update.transfer_id,
						   CANARD_TRANSFER_PRIORITY_HIGH,
						   CanardRequest,
						   &msg_buffer[0],
						   total_size);
}

/*
 * Handle response to file read request. This is called when we recieve a response to 
 * send_fw_read() above. the packet contains a 16 bit value at the begining called 
 * error that needs to be removed before reading the file chunk
 */
static void handle_file_read_response(CanardInstance* ins, CanardRxTransfer* transfer) {
	(void)ins;

	if ((transfer->transfer_id+1)%256 != fw_update.transfer_id ||
		transfer->source_node_id != fw_update.node_id) {
		return;
	}
	int16_t error = 0;
	canardDecodeScalar(transfer, 0, 16, true, (void*)&error);
	uint16_t len = transfer->payload_len - 2;

	uint32_t offset = 16;
	uint32_t buf32[(len+3)/4];
	uint8_t *buf = (uint8_t *)&buf32[0];
	for (uint16_t i=0; i<len; i++) {
		canardDecodeScalar(transfer, offset, 8, false, (void*)&buf[i]);
		offset += 8;
	}

	if (debug_level == 9) {
		commands_printf("UAVCAN read_response\nlen: %d\noffset: %d",len, fw_update.ofs);
	}

	if (nrf_driver_ext_nrf_running()) {
		nrf_driver_pause(2000);
	}

	// Write to flash, skip the first 6 bytes for Size and CRC so need to add 6 always
	uint16_t flash_res = flash_helper_write_new_app_data(fw_update.ofs+6, buf, len);
	fw_update.ofs += len;
	
	// TODO: Check result and abort on failure.
	(void)flash_res;

	// If the packet is incomplete that means that was the last packet (might be an issue if the last packet is exactly full)
	// however Ardupilot seems to be handling this same way, and no issues have been reported so far.
	if (len < UAVCAN_PROTOCOL_FILE_READ_RESPONSE_DATA_MAX_LENGTH) {
		fw_update.node_id = 0;
		const uint32_t app_size = (uint32_t)fw_update.ofs-6;
		uint16_t app_crc = crc16((uint8_t *)ADDR_FLASH_SECTOR_8+6,app_size);
		uint8_t sizecrc[6];
		int32_t ind = 0;

		uint32_t sizefromflash = 0;
		uint16_t crc_app = 0;
		uint32_t nextData = 0;

		// This is debug stuff used to valiate the file transfer.
		if (debug_level == 8) {
			commands_printf("UAVCAN read_response transfer finished %d kB", fw_update.ofs / 1024U);
			commands_printf("new app address: 0x%lx", flash_addr[NEW_APP_BASE]);
			
			// Print reserved space contents for size and crc
			sizefromflash = buffer_get_uint32((uint8_t *)flash_addr[NEW_APP_BASE], &ind);
			crc_app = buffer_get_uint16((uint8_t *)flash_addr[NEW_APP_BASE], &ind);
			nextData = buffer_get_uint32((uint8_t *)flash_addr[NEW_APP_BASE], &ind);
			commands_printf("orig size from flash: 0x%lx", (long)sizefromflash);
			commands_printf("orig crc from flash: 0x%02hhX", crc_app);
			commands_printf("orig nextData: 0x%08lx", (long)nextData);
		}

		// Calculate and write size and crc to start of reserved space
		ind = 0;
		buffer_append_uint32(sizecrc, app_size, &ind);
		buffer_append_uint16(sizecrc, app_crc, &ind);

		flash_res = flash_helper_write_new_app_data(0, sizecrc, sizeof(sizecrc));

		if (debug_level == 8) {
			// Print data for debuging
			commands_printf("ofs: %ld", (long)fw_update.ofs);
			commands_printf("len: %d", len);
			commands_printf("Size: 0x%lx", (long)app_size);
			commands_printf("crc16: 0x%02hhX", app_crc);
			uint16_t app_crc1 = crc16((uint8_t *)flash_addr[APP_BASE],app_size);
			commands_printf("app crc16: 0x%02hhX", app_crc1);
			
			// Print size and crc data read from flash after calculation and write
			ind = 0;
			sizefromflash = buffer_get_uint32((uint8_t *)flash_addr[NEW_APP_BASE], &ind);
			crc_app = buffer_get_uint16((uint8_t *)flash_addr[NEW_APP_BASE], &ind);
			commands_printf("size from flash: 0x%lx", (long)sizefromflash);
			commands_printf("crc from flash: 0x%02hhX", crc_app);
			nextData = buffer_get_uint32((uint8_t *)flash_addr[NEW_APP_BASE], &ind);
			commands_printf("nextData: 0x%lx", (long)nextData);
			ind = 0;
			uint32_t appstartdata = buffer_get_uint32((uint8_t *)flash_addr[APP_BASE], &ind);
			commands_printf("appStartData: 0x%08lx", appstartdata);
			commands_printf("Jumping to Bootloader in 500ms!");
			jump_delay_start = chVTGetSystemTimeX();
		}

		// Do not jump directly to the bootloader after finising the transfer in case we need time
		// to allow other things to finish. Currently it only delays if it needs to print the debug
		// data.
		jump_to_bootloader = true;
	}

	// show offset number we are flashing in kbyte as crude progress indicator
	node_status.vendor_specific_status_code = 1 + (fw_update.ofs / 1024U);

	// Clear the counter so we dont delay the next request for data unecesarily 
	fw_update.last_ms = 0;
}

/**
 * Handle a begin firmware update request. 
 * UAVCAN uses the file system requests to pull the firmware file from the host.
 * A begin firmware update call is made to tell the client node to ask the host
 * for the firmware file. From this point on the client basically becomes the host
 * for the file transfer until the file is received by sending requests for the 
 * next chunk of data every so often.
 */
static void handle_begin_firmware_update(CanardInstance* ins, CanardRxTransfer* transfer)
{
	// manual decoding due to TAO bug in libcanard generated code
	if (transfer->payload_len < 1 || transfer->payload_len > sizeof(fw_update.path)+1) {
		return;
	}

	if (fw_update.node_id == 0) {
		uint32_t offset = 0;
		canardDecodeScalar(transfer, 0, 8, false, (void*)&fw_update.node_id);
		offset += 8;
		for (uint8_t i=0; i<transfer->payload_len-1; i++) {
			canardDecodeScalar(transfer, offset, 8, false, (void*)&fw_update.path[i]);
			offset += 8;
		}

		fw_update.ofs = 0;
		fw_update.last_ms = 0;
		fw_update.sector = 0;
		fw_update.sector_ofs = 0;
		if (fw_update.node_id == 0) {
			last_read_file_req = chVTGetSystemTimeX();
			fw_update.node_id = transfer->source_node_id;
		}
	}

	uavcan_protocol_file_BeginFirmwareUpdateResponse reply;
	memset(&reply, 0, sizeof(reply));

	reply.error = UAVCAN_PROTOCOL_FILE_BEGINFIRMWAREUPDATE_RESPONSE_ERROR_OK;

	uint32_t total_size = uavcan_protocol_file_BeginFirmwareUpdateResponse_encode(&reply, msg_buffer);
	canardRequestOrRespond(ins,
						   transfer->source_node_id,
						   UAVCAN_PROTOCOL_FILE_BEGINFIRMWAREUPDATE_SIGNATURE,
						   UAVCAN_PROTOCOL_FILE_BEGINFIRMWAREUPDATE_ID,
						   &transfer->transfer_id,
						   transfer->priority,
						   CanardResponse,
						   &msg_buffer[0],
						   total_size);

	// Erase the reserved flash for new app
	flash_helper_erase_new_app(RESERVED_FLASH_SPACE_SIZE);

	last_read_file_req = chVTGetSystemTimeX();

	if (debug_level > 0) {
		commands_printf("UAVCAN Begin firmware update from node_id: %d",fw_update.node_id);
	}
	
	send_fw_read(ins);
}

/**
* This callback is invoked by the library when a new message or request or response is received.
*/
static void onTransferReceived(CanardInstance* ins, CanardRxTransfer* transfer) {
	if (debug_level == 4) {
		commands_printf("UAVCAN transfer RX: NODE: %d Type: %d ID: %d",
				transfer->source_node_id, transfer->transfer_type, transfer->data_type_id);
	}

	/*
	* Dynamic node ID allocation protocol.
	* Taking this branch only if we don't have a node ID, ignoring otherwise.
	*/
	// if (canardGetLocalNodeID(ins) == CANARD_BROADCAST_NODE_ID) {
	//     if (transfer->transfer_type == CanardTransferTypeBroadcast &&
	//         transfer->data_type_id == UAVCAN_PROTOCOL_DYNAMIC_NODE_ID_ALLOCATION_ID) {
	//         handle_allocation_response(ins, transfer);
	//     }
	//     return;
	// }

	switch (transfer->data_type_id) {
		case UAVCAN_PROTOCOL_GETNODEINFO_ID:
			handle_get_node_info(ins, transfer);
			break;

		case UAVCAN_EQUIPMENT_ESC_RAWCOMMAND_ID:
			handle_esc_raw_command(ins, transfer);
			break;

		case UAVCAN_EQUIPMENT_ESC_RPMCOMMAND_ID:
			handle_esc_rpm_command(ins, transfer);
			break;

		case UAVCAN_EQUIPMENT_ESC_STATUS_ID:
			handle_esc_status(ins, transfer);
			break;

		case UAVCAN_PROTOCOL_RESTARTNODE_ID:
			if (debug_level > 0) {
				commands_printf("RestartNode\n");
			}
			handle_restart_node();
			break;

		case UAVCAN_PROTOCOL_PARAM_GETSET_ID:
			handle_param_getset(ins, transfer);
			break;

		case UAVCAN_PROTOCOL_FILE_BEGINFIRMWAREUPDATE_ID:
			handle_begin_firmware_update(ins, transfer);
			break;

		case UAVCAN_PROTOCOL_FILE_READ_ID:
			handle_file_read_response(ins, transfer);
			break;
	   }
}

/**
 * This callback is invoked by the library when it detects beginning of a new transfer on the bus that can be received
 * by the local node.
 * If the callback returns true, the library will receive the transfer.
 * If the callback returns false, the library will ignore the transfer.
 * All transfers that are addressed to other nodes are always ignored.
 */
static bool shouldAcceptTransfer(const CanardInstance* ins,
								 uint64_t* out_data_type_signature,
								 uint16_t data_type_id,
								 CanardTransferType transfer_type,
								 uint8_t source_node_id) {
	(void)ins;
	(void)source_node_id;

	if (debug_level == 4) {
		commands_printf("UAVCAN shouldAccept: NODE: %d Type: %d ID: %d",
				source_node_id, transfer_type, data_type_id);
	}

	// This is for future use if Dynamic node ID allocation is used.
	// if (canardGetLocalNodeID(ins) == CANARD_BROADCAST_NODE_ID)
	// {
	//     /*
	//      * If we're in the process of allocation of dynamic node ID, accept only relevant transfers.
	//      */
	//     if ((transfer_type == CanardTransferTypeBroadcast) &&
	//         (data_type_id == UAVCAN_PROTOCOL_DYNAMIC_NODE_ID_ALLOCATION_ID))
	//     {
	//         *out_data_type_signature = UAVCAN_PROTOCOL_DYNAMIC_NODE_ID_ALLOCATION_SIGNATURE;
	//         return true;
	//     }
	//     return false;
	// }

	switch (data_type_id) {
		case UAVCAN_PROTOCOL_GETNODEINFO_ID:
			*out_data_type_signature = UAVCAN_PROTOCOL_GETNODEINFO_SIGNATURE;
			return true;

		case UAVCAN_EQUIPMENT_ESC_RAWCOMMAND_ID:
			*out_data_type_signature = UAVCAN_EQUIPMENT_ESC_RAWCOMMAND_SIGNATURE;
			return true;

		case UAVCAN_EQUIPMENT_ESC_RPMCOMMAND_ID:
			*out_data_type_signature = UAVCAN_EQUIPMENT_ESC_RPMCOMMAND_SIGNATURE;
			return true;

		case UAVCAN_EQUIPMENT_ESC_STATUS_ID:
			*out_data_type_signature = UAVCAN_EQUIPMENT_ESC_STATUS_SIGNATURE;
			return true;

		case UAVCAN_PROTOCOL_RESTARTNODE_ID:
			*out_data_type_signature = UAVCAN_PROTOCOL_RESTARTNODE_SIGNATURE;
			return true;

		case UAVCAN_PROTOCOL_PARAM_GETSET_ID:
			*out_data_type_signature = UAVCAN_PROTOCOL_PARAM_GETSET_SIGNATURE;
			return true;

		case UAVCAN_PROTOCOL_FILE_READ_ID:
			*out_data_type_signature = UAVCAN_PROTOCOL_FILE_READ_SIGNATURE;
			return true;
		
		case UAVCAN_PROTOCOL_FILE_BEGINFIRMWAREUPDATE_ID:
			*out_data_type_signature = UAVCAN_PROTOCOL_FILE_BEGINFIRMWAREUPDATE_SIGNATURE;
			return true;

		default:
			break;
	}

	return false;
}

static void terminal_debug_on(int argc, const char **argv) {
	if (argc == 2) {
		int level = -1;
		sscanf(argv[1], "%d", &level);

		if (level >= 0) {
			debug_level = level;
			commands_printf("UAVCAN debug level is now %d", debug_level);
		} else {
			commands_printf("Invalid argument(s).\n");
		}
	} else {
		commands_printf("This command requires one argument.\n");
	}
}

static THD_FUNCTION(canard_thread, arg) {
	(void)arg;
	chRegSetThreadName("UAVCAN");

	getParamByName("controller_id")->defval = HW_DEFAULT_ID;

	systime_t last_status_time = 0;
	systime_t last_esc_status_time = 0;
	systime_t last_esc_status_time_r2 = 0;
	systime_t last_tot_current_calc_time = 0;
	systime_t last_param_refresh = 0;
	bool was_running = false;

	for (;;) {
		const app_configuration *conf = app_get_configuration();

		if (conf->can_mode != CAN_MODE_UAVCAN) {
			chThdSleepMilliseconds(100);
			was_running = false;
			continue;
		}

		if (!was_running) {
			memset(&canard_ins, 0, sizeof(canard_ins));
			memset(canard_memory_pool, 0, sizeof(canard_memory_pool));

			canardInit(&canard_ins, canard_memory_pool, sizeof(canard_memory_pool),
					onTransferReceived, shouldAcceptTransfer, NULL);

#ifdef HW_CAN2_DEV
			memset(&canard_ins_if2, 0, sizeof(canard_ins_if2));
			memset(canard_memory_pool_if2, 0, sizeof(canard_memory_pool_if2));

			canardInit(&canard_ins_if2, canard_memory_pool_if2, sizeof(canard_memory_pool_if2),
					onTransferReceived, shouldAcceptTransfer, NULL);
#endif

			last_status_time = chVTGetSystemTimeX();
			last_esc_status_time = chVTGetSystemTimeX();
			last_esc_status_time_r2 = chVTGetSystemTimeX();
			last_tot_current_calc_time = chVTGetSystemTimeX();
			last_param_refresh = chVTGetSystemTimeX();

			was_running = true;
		}

		canardSetLocalNodeID(&canard_ins, conf->controller_id);

#ifdef HW_CAN2_DEV
		canardSetLocalNodeID(&canard_ins_if2, conf->controller_id);
#endif

		CANRxFrame *rxmsg;
		while ((rxmsg = comm_can_get_rx_frame(1)) != 0) {
			CanardCANFrame rx_frame;

			if (rxmsg->IDE == CAN_IDE_EXT) {
				rx_frame.id = rxmsg->EID | CANARD_CAN_FRAME_EFF;
			} else {
				rx_frame.id = rxmsg->SID;
			}

			rx_frame.data_len = rxmsg->DLC;
			memcpy(rx_frame.data, rxmsg->data8, rxmsg->DLC);

			canardHandleRxFrame(&canard_ins, &rx_frame, ST2US(chVTGetSystemTimeX()));
		}

		for (const CanardCANFrame* txf = NULL; (txf = canardPeekTxQueue(&canard_ins)) != NULL;) {
			comm_can_transmit_eid_if(txf->id, txf->data, txf->data_len, 1);
			canardPopTxQueue(&canard_ins);
		}

#ifdef HW_CAN2_DEV
		while ((rxmsg = comm_can_get_rx_frame(2)) != 0) {
			CanardCANFrame rx_frame;

			if (rxmsg->IDE == CAN_IDE_EXT) {
				rx_frame.id = rxmsg->EID | CANARD_CAN_FRAME_EFF;
			} else {
				rx_frame.id = rxmsg->SID;
			}

			rx_frame.data_len = rxmsg->DLC;
			memcpy(rx_frame.data, rxmsg->data8, rxmsg->DLC);

			canardHandleRxFrame(&canard_ins_if2, &rx_frame, ST2US(chVTGetSystemTimeX()));
		}

		for (const CanardCANFrame* txf = NULL; (txf = canardPeekTxQueue(&canard_ins_if2)) != NULL;) {
			comm_can_transmit_eid_if(txf->id, txf->data, txf->data_len, 2);
			canardPopTxQueue(&canard_ins_if2);
		}
#endif

		if (ST2MS(chVTTimeElapsedSinceX(last_status_time)) >= 1000) {
			last_status_time = chVTGetSystemTimeX();
			canardCleanupStaleTransfers(&canard_ins, ST2US(chVTGetSystemTimeX()));
			sendNodeStatus(&canard_ins);
#ifdef HW_CAN2_DEV
			canardCleanupStaleTransfers(&canard_ins_if2, ST2US(chVTGetSystemTimeX()));
			sendNodeStatus(&canard_ins_if2);
#endif
		}

		if (conf->can_status_rate_1 > 0 && UTILS_AGE_S(last_esc_status_time) >= (1.0 / (float)conf->can_status_rate_1)) {
			last_esc_status_time = chVTGetSystemTimeX();
			sendEscStatus(&canard_ins);
#ifdef HW_CAN2_DEV
			sendEscStatus(&canard_ins_if2);
#endif

			if ((conf->can_status_msgs_r1 >> 0) & 1) {
				sendRtData(&canard_ins);
#ifdef HW_CAN2_DEV
				sendRtData(&canard_ins_if2);
#endif
			}
		}

		if (conf->can_status_rate_2 > 0 && UTILS_AGE_S(last_esc_status_time_r2) >= (1.0 / (float)conf->can_status_rate_2)) {
			last_esc_status_time_r2 = chVTGetSystemTimeX();

			if ((conf->can_status_msgs_r2 >> 0) & 1) {
				sendRtData(&canard_ins);
#ifdef HW_CAN2_DEV
				sendRtData(&canard_ins_if2);
#endif
			}
		}

		if (ST2MS(chVTTimeElapsedSinceX(last_tot_current_calc_time)) >= 1000 / CURRENT_CALC_FREQ_HZ) {
			last_tot_current_calc_time = chVTGetSystemTimeX();
			calculateTotalCurrent();
			if (debug_level == 3) {
				const volatile mc_configuration *mcconf = mc_interface_get_configuration();
				commands_printf("Max Current: %0.02f", (double)mcconf->lo_current_max);
			}
		}

		if (ST2MS(chVTTimeElapsedSinceX(last_param_refresh)) >= 1000 / PARAM_REFRESH_RATE_HZ) {
			last_param_refresh = chVTGetSystemTimeX();
			if(refresh_parameters_enabled) {
				refresh_parameters();
			}

			if(debug_level == 7) {
				commands_printf("Refreshing Parameters Time Delta: %d", ST2MS(chVTGetSystemTimeX()-last_param_refresh));
			}
		}

		if ((ST2MS(chVTTimeElapsedSinceX(last_read_file_req)) >= 10) && (fw_update.node_id != 0)) {
			last_read_file_req = chVTGetSystemTimeX();
			send_fw_read(fw_update.ins);
		}

		// delay jump to bootloader after receiving data for 0.5 sec
		if ((ST2MS(chVTTimeElapsedSinceX(jump_delay_start)) >= 500) && (jump_to_bootloader == true)) {
			flash_helper_jump_to_bootloader();
		}

		chThdSleepMilliseconds(1);
	}
}
