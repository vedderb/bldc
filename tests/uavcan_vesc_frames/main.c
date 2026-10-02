// Host test for the UAVCAN receive path used by CAN_MODE_VESC_UAVCAN.
//
// In that mode comm_can passes every frame to canard_process_frame() first and only
// hands it to the VESC decoder when the return value is non-zero. This checks that VESC
// frames whose command id matches a UAVCAN service id are not taken as UAVCAN
// transfers, and that real UAVCAN requests still are.

#include <stdio.h>
#include <string.h>

#include "hal.h"
#include "datatypes.h"

// Skip the firmware headers included by canard_driver.c, the parts it needs are stubbed below.
#define CONF_GENERAL_H_
#define APP_H_
#define COMM_CAN_H_
#define COMMANDS_H_
#define MC_INTERFACE_H_
#define HW_H_
#define TIMEOUT_H_
#define TERMINAL_H_
#define MEMPOOLS_H_
#define FLASH_HELPER_H_
#define CRC_H_
#define NRF_NRF_DRIVER_H_
#define BUFFER_H_
#define UTILS_H_
#define MCPWM_FOC_H_
#define IMU_IMU_H_

#define HW_NAME						"test"
#define HW_DEFAULT_ID				0
#define FW_VERSION_MAJOR			0
#define FW_VERSION_MINOR			0
#define FW_TEST_VERSION_NUMBER		0
#define NTC_TEMP_MOS1()				0.0
#define NTC_TEMP_MOS2()				0.0
#define NTC_TEMP_MOS3()				0.0
#define TEMP_MOTOR_1(beta)			0.0
#define TEMP_MOTOR_2(beta)			0.0
#define UTILS_AGE_S(t)				((float)(chVTGetSystemTimeX() - (t)) / 1000.0f)

systime_t test_time_ms = 1000;
int test_reset_count = 0;

static uint8_t STM32_UUID_8[12];
static app_configuration appconf;
static mc_configuration mcconf;
static int flash_erase_count = 0;
static int set_current_rel_count = 0;
static float set_current_rel_last = 0.0;

const app_configuration *app_get_configuration(void) { return &appconf; }
void app_set_configuration(app_configuration *conf) { (void)conf; }
void app_disable_output(int time_ms) { (void)time_ms; }
const volatile mc_configuration *mc_interface_get_configuration(void) { return &mcconf; }
void mc_interface_set_configuration(mc_configuration *conf) { (void)conf; }
int mc_interface_get_motor_thread(void) { return 1; }
void mc_interface_set_current_rel(float val) { set_current_rel_count++; set_current_rel_last = val; }
void mc_interface_set_brake_current_rel(float val) { (void)val; }
void mc_interface_set_duty(float val) { (void)val; }
void mc_interface_set_pid_speed(float val) { (void)val; }
float mc_interface_get_tot_current(void) { return 0.0; }
float mc_interface_get_tot_current_filtered(void) { return 0.0; }
float mc_interface_get_tot_current_in_filtered(void) { return 0.0; }
float mc_interface_get_input_voltage_filtered(void) { return 0.0; }
float mc_interface_get_rpm(void) { return 0.0; }
float mc_interface_get_duty_cycle_now(void) { return 0.0; }
float mc_interface_temp_fet_filtered(void) { return 0.0; }
float mc_interface_temp_motor_filtered(void) { return 0.0; }
float mc_interface_get_amp_hours(bool reset) { (void)reset; return 0.0; }
float mc_interface_get_amp_hours_charged(bool reset) { (void)reset; return 0.0; }
float mc_interface_get_watt_hours(bool reset) { (void)reset; return 0.0; }
float mc_interface_get_watt_hours_charged(bool reset) { (void)reset; return 0.0; }
float mc_interface_get_pid_pos_now(void) { return 0.0; }
float mc_interface_get_battery_level(float *wh_left) { *wh_left = 0.0; return 1.0; }
mc_fault_code mc_interface_get_fault(void) { return FAULT_CODE_NONE; }
float mcpwm_foc_get_vd(void) { return 0.0; }
float mcpwm_foc_get_vq(void) { return 0.0; }
float mcpwm_foc_get_id(void) { return 0.0; }
float mcpwm_foc_get_iq(void) { return 0.0; }
void imu_get_rpy(float *rpy) { memset(rpy, 0, 3 * sizeof(float)); }
void imu_get_accel(float *accel) { memset(accel, 0, 3 * sizeof(float)); }
void imu_get_gyro(float *gyro) { memset(gyro, 0, 3 * sizeof(float)); }
app_configuration *mempools_alloc_appconf(void) { static app_configuration c; return &c; }
mc_configuration *mempools_alloc_mcconf(void) { static mc_configuration c; return &c; }
void mempools_free_appconf(app_configuration *conf) { (void)conf; }
void mempools_free_mcconf(mc_configuration *conf) { (void)conf; }
bool conf_general_store_app_configuration(app_configuration *conf) { (void)conf; return true; }
bool conf_general_store_mc_configuration(mc_configuration *conf, bool is_motor_2) { (void)conf; (void)is_motor_2; return true; }
bool conf_general_store_backup_data(void) { return true; }
void commands_printf(const char* format, ...) { (void)format; }
void terminal_register_command_callback(const char* command, const char *help, const char *arg_names,
		void(*cbf)(int argc, const char **argv)) { (void)command; (void)help; (void)arg_names; (void)cbf; }
void timeout_reset(void) {}
uint16_t flash_helper_erase_new_app(uint32_t new_app_size) { (void)new_app_size; flash_erase_count++; return 0; }
uint16_t flash_helper_write_new_app_data(uint32_t offset, uint8_t *data, uint32_t len) { (void)offset; (void)data; (void)len; return 0; }
void flash_helper_jump_to_bootloader(void) {}
unsigned short crc16(unsigned char *buf, unsigned int len) { (void)buf; (void)len; return 0; }
bool nrf_driver_ext_nrf_running(void) { return false; }
void nrf_driver_pause(int time_ms) { (void)time_ms; }
void buffer_append_uint32(uint8_t* buffer, uint32_t number, int32_t *index) { (void)buffer; (void)number; (void)index; }
void buffer_append_uint16(uint8_t* buffer, uint16_t number, int32_t *index) { (void)buffer; (void)number; (void)index; }
uint32_t buffer_get_uint32(const uint8_t *buffer, int32_t *index) { (void)buffer; (void)index; return 0; }
uint16_t buffer_get_uint16(const uint8_t *buffer, int32_t *index) { (void)buffer; (void)index; return 0; }
CANRxFrame *comm_can_get_rx_frame(int interface) { (void)interface; return 0; }
void comm_can_transmit_eid_if(uint32_t id, const uint8_t *data, uint8_t len, int interface) {
	(void)id; (void)data; (void)len; (void)interface;
}

#include "canard_driver.c"

#define LOCAL_NODE_ID	10
#define REMOTE_NODE_ID	20

static int failures = 0;

#define CHECK(cond) do { \
	if (!(cond)) { \
		printf("FAIL %s:%d: %s\n", __FILE__, __LINE__, #cond); \
		failures++; \
	} \
} while (0)

static void reset_state(void) {
	memset(&canard_ins, 0, sizeof(canard_ins));
	memset(canard_memory_pool, 0, sizeof(canard_memory_pool));
	canardInit(&canard_ins, canard_memory_pool, sizeof(canard_memory_pool),
			onTransferReceived, shouldAcceptTransfer, NULL);
	canardSetLocalNodeID(&canard_ins, LOCAL_NODE_ID);
	canard_ready = true;

	memset(&fw_update, 0, sizeof(fw_update));
	restart_pending = false;
	test_reset_count = 0;
	flash_erase_count = 0;
	set_current_rel_count = 0;
	test_time_ms += 1000;
}

static bool tx_queue_empty(void) {
	return canardPeekTxQueue(&canard_ins) == NULL;
}

// VESC frame as sent by comm_can_transmit_eid: cmd << 8 | receiver id, last byte chosen by the test
static int16_t send_vesc_frame(CAN_PACKET_ID cmd, uint8_t receiver, uint8_t last_byte) {
	CANRxFrame f;
	memset(&f, 0, sizeof(f));
	f.IDE = CAN_IDE_EXT;
	f.EID = ((uint32_t)cmd << 8) | receiver;
	f.DLC = 8;
	for (int i = 0;i < 7;i++) {
		f.data8[i] = 0x11 * (i + 1);
	}
	f.data8[7] = last_byte;
	return canard_process_frame(&f, 1);
}

// Single frame UAVCAN service request from REMOTE_NODE_ID to LOCAL_NODE_ID
static int16_t send_service_request(uint8_t service_id, const uint8_t *payload, uint8_t len) {
	CANRxFrame f;
	memset(&f, 0, sizeof(f));
	f.IDE = CAN_IDE_EXT;
	// Priority, service type, request, destination, service flag, source
	f.EID = (30U << 24) | ((uint32_t)service_id << 16) | (1U << 15) |
			((uint32_t)LOCAL_NODE_ID << 8) | (1U << 7) | REMOTE_NODE_ID;
	if (len > 0) {
		memcpy(f.data8, payload, len);
	}
	f.data8[len] = 0xC0; // Single frame, transfer id 0
	f.DLC = len + 1;
	return canard_process_frame(&f, 1);
}

static int16_t send_restart_request(uint64_t magic) {
	uint8_t payload[5];
	canardEncodeScalar(payload, 0, 40, &magic);
	return send_service_request(UAVCAN_PROTOCOL_RESTARTNODE_ID, payload, sizeof(payload));
}

static void test_not_ready(void) {
	reset_state();
	memset(&canard_ins, 0, sizeof(canard_ins));
	canard_ready = false;

	CHECK(send_vesc_frame(CAN_PACKET_FILL_RX_BUFFER, 3, 0xC5) != 0);
}

static void test_vesc_frames_pass_through(void) {
	// Last byte 0xC0-0xFF: single frame transfer, 0x80-0x9F: start of multi frame transfer
	const uint8_t tails[] = {0xC0, 0xC5, 0xFF, 0x80, 0x9F};
	const CAN_PACKET_ID cmds[] = {
			CAN_PACKET_SET_CURRENT,				// GetNodeInfo
			CAN_PACKET_FILL_RX_BUFFER,			// RestartNode
			CAN_PACKET_SET_CURRENT_REL,			// Destination LOCAL_NODE_ID when receiver >= 128
			CAN_PACKET_SET_CURRENT_BRAKE_REL,	// param GetSet
			CAN_PACKET_BMS_AH_WH,				// BeginFirmwareUpdate
			CAN_PACKET_BMS_HW_DATA_1,			// file Read
	};
	// 0: anonymous broadcast, >= 128: service frame
	const uint8_t receivers[] = {3, 0, 200};

	for (unsigned int c = 0;c < sizeof(cmds) / sizeof(cmds[0]);c++) {
		for (unsigned int r = 0;r < sizeof(receivers);r++) {
			for (unsigned int t = 0;t < sizeof(tails);t++) {
				reset_state();
				int16_t res = send_vesc_frame(cmds[c], receivers[r], tails[t]);
				if (res == 0) {
					printf("  cmd %d receiver %d tail 0x%02X consumed by canard\n",
							cmds[c], receivers[r], tails[t]);
				}
				CHECK(res != 0);
				CHECK(!restart_pending);
				CHECK(flash_erase_count == 0);
				CHECK(fw_update.node_id == 0);
				CHECK(tx_queue_empty());
			}
		}
	}
}

static void test_restart_node(void) {
	reset_state();
	CHECK(send_restart_request(UAVCAN_PROTOCOL_RESTARTNODE_REQUEST_MAGIC_NUMBER) == 0);
	CHECK(restart_pending);
	CHECK(!tx_queue_empty());
	CHECK(canardPeekTxQueue(&canard_ins)->data[0] & 0x80); // ok = true

	reset_state();
	CHECK(send_restart_request(0x1234) == 0);
	CHECK(!restart_pending);
	CHECK(!tx_queue_empty());
	CHECK((canardPeekTxQueue(&canard_ins)->data[0] & 0x80) == 0); // ok = false
}

static void test_get_node_info(void) {
	reset_state();
	CHECK(send_service_request(UAVCAN_PROTOCOL_GETNODEINFO_ID, NULL, 0) == 0);
	CHECK(!tx_queue_empty());
}

static void test_begin_firmware_update(void) {
	reset_state();
	// source_node_id 0 (use the requesting node) and a one character path
	const uint8_t payload[] = {0, 'a'};
	CHECK(send_service_request(UAVCAN_PROTOCOL_FILE_BEGINFIRMWAREUPDATE_ID, payload, sizeof(payload)) == 0);
	CHECK(flash_erase_count == 1);
	CHECK(fw_update.node_id == REMOTE_NODE_ID);
	CHECK(!tx_queue_empty());
}

static void test_esc_raw_command(void) {
	reset_state();
	appconf.uavcan_esc_index = 0;
	appconf.uavcan_raw_mode = UAVCAN_RAW_MODE_CURRENT;

	CANRxFrame f;
	memset(&f, 0, sizeof(f));
	f.IDE = CAN_IDE_EXT;
	f.EID = (16U << 24) | ((uint32_t)UAVCAN_EQUIPMENT_ESC_RAWCOMMAND_ID << 8) | REMOTE_NODE_ID;
	int16_t cmd = 4096; // 0.5
	canardEncodeScalar(f.data8, 0, 14, &cmd);
	f.data8[2] = 0xC0;
	f.DLC = 3;

	CHECK(canard_process_frame(&f, 1) == 0);
	CHECK(set_current_rel_count == 1);
	CHECK(set_current_rel_last == 0.5f);
}

int main(void) {
	test_not_ready();
	test_vesc_frames_pass_through();
	test_restart_node();
	test_get_node_info();
	test_begin_firmware_update();
	test_esc_raw_command();

	CHECK(canard_mtx.locked == 0);

	if (failures) {
		printf("%d check(s) failed\n", failures);
		return 1;
	}

	printf("All tests passed\n");
	return 0;
}
